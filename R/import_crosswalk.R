# R/import_crosswalk.R — 1.4：讀兩份跨波次變項對照表，產生題目位置、題組路徑、
# 題組沿革、題目關係和題組成員的候選
#
# 對照表每一列是一個變項：主構面、次構面、變項標籤、各施測欄 O／X、備註、反向題。
# 對照表沒有問卷題號：positions 改從 .sav 變項標籤開頭（「一.1.」）抓，資料使用說明
# 不合理值表的「問卷題號」欄補缺和印證（給 sav_dir／usage_dir 才做）。
# 所有輸出都是候選，要人工確認。

#' Import the cross-wave crosswalks (step 1.4)
#'
#' Reads the crosswalk workbooks (one sheet per questionnaire; variables x waves with
#' O/X, construct path, notes, reverse flag) and proposes kit additions. Nothing is
#' written into the kit: every output is a candidate with `confidence` and `basis`.
#'
#' @param files Paths of the crosswalk `.xlsx` files (e.g. `cw_M03W48.xlsx`,
#'   `cw_M36W84.xlsx`).
#' @param kit The built kit (needs `datasets`, `admins`, `cohorts`, `aliases` of kind
#'   `crosswalk_col`, `items`, `dataset_vars`, `groups`, `group_tree`, `group_members`).
#' @param usage_dir Optional folder with the usage docs as text (`us_<M03W24>.txt`);
#'   positions in their error tables fill gaps and confirm the .sav ones.
#' @param sav_dir Optional folder of .sav headers (`<m03w24>/<data1>_meta.csv`, as for
#'   `import_sav_dir()`). The crosswalks carry no questionnaire positions; the .sav
#'   variable labels start with them ("一.1.2.").
#' @return A list: `cw` (one row per crosswalk row), `cw_long` (row x wave), `positions`,
#'   `presence`, `coverage`, `group_map`, `group_unmatched`, `group_paths`,
#'   `group_lineage`, `notes`, `relation_hints`, `member_hints`, `issues`.
#' @export
import_crosswalk <- function(files, kit, usage_dir = NULL, sav_dir = NULL) {
  sheets <- list()
  for (f in files) {
    for (sh in readxl::excel_sheets(f)) {
      raw <- suppressMessages(readxl::read_excel(f, sh, col_names = FALSE,
                                                 col_types = "text"))
      sheets[[length(sheets) + 1L]] <- list(file = basename(f), sheet = sh,
                                            raw = as.data.frame(raw, stringsAsFactors = FALSE))
    }
  }
  usage <- NULL
  if (!is.null(usage_dir)) {
    fs <- list.files(usage_dir, pattern = "^us_.*\\.txt$", full.names = TRUE)
    usage <- stats::setNames(lapply(fs, function(f) readLines(f, encoding = "UTF-8", warn = FALSE)),
                             basename(fs))
  }
  sav_meta <- NULL
  if (!is.null(sav_dir)) {
    ds <- kit$datasets[kit$datasets$file != "v02", , drop = FALSE]
    age <- as.integer(kit$admins$ageMonth[match(ds$adminID, kit$admins$adminID)])
    f <- file.path(sav_dir, paste0(tolower(substr(ds$adminID, 1, 3)), "w", sprintf("%02d", age)),
                   paste0(ds$file, "_meta.csv"))
    ok <- file.exists(f)
    sav_meta <- stats::setNames(lapply(f[ok], read_sav_meta), ds$datasetID[ok])
  }
  crosswalk_from_sheets(sheets, kit, usage, sav_meta)
}

#' 核心：sheets 是 list(file, sheet, raw)；usage 是 檔名 → 文字行；sav_meta 是
#' datasetID → .sav 表頭（read_sav_meta()）。後兩者可為 NULL
#' @noRd
crosswalk_from_sheets <- function(sheets, kit, usage = NULL, sav_meta = NULL) {
  iss <- list()
  rows <- list()
  long <- list()
  for (s in sheets) {
    p <- cw_parse_sheet(s$raw, s$file, s$sheet, kit)
    rows[[length(rows) + 1L]] <- p$rows
    long[[length(long) + 1L]] <- p$long
    iss[[length(iss) + 1L]] <- p$issues
  }
  rows <- do.call(rbind, rows)
  long <- do.call(rbind, long)
  ids <- cw_item_ids(rows$instrument, rows$varname, kit)
  rows$item_uid <- ids$item_uid
  rows$item_exists <- ids$item_exists
  rows$kind <- cw_var_kind(rows, long, kit)

  pos <- cw_positions(usage, sav_meta, kit)
  iss[[length(iss) + 1L]] <- pos$issues
  pres <- cw_presence(long, kit)

  grp <- cw_groups(rows, long, kit)
  notes <- cw_notes(rows, long)
  rel <- cw_relation_hints(rows, long, notes, kit)
  mem <- cw_member_hints(rows, grp, rel, kit)

  list(cw = rows, cw_long = long, positions = pos$positions, presence = pres,
       coverage = cw_position_coverage(pos$positions, kit),
       group_map = grp$group_map,
       group_unmatched = grp$group_map[grp$group_map$method == "none", , drop = FALSE],
       group_paths = grp$group_paths, group_lineage = grp$group_lineage,
       notes = notes, relation_hints = rel, member_hints = mem,
       issues = do.call(rbind, iss))
}

# 讀表 ----

#' 工作表名稱 → 問卷、填答者（datasets$respondent）
#' @noRd
cw_sheet_info <- function() {
  data.frame(
    # 原文："家長問卷" "教保問卷" "教師問卷" "兒童問卷"
    sheet = c("\u5bb6\u9577\u554f\u5377", "\u6559\u4fdd\u554f\u5377", "\u6559\u5e2b\u554f\u5377", "\u5152\u7ae5\u554f\u5377"),
    instrument = c("P", "T", "T", "C"),
    # 原文："家長" "教保人員" "教師" "兒童"
    respondent = c("\u5bb6\u9577", "\u6559\u4fdd\u4eba\u54e1", "\u6559\u5e2b", "\u5152\u7ae5"),
    stringsAsFactors = FALSE)
}

#' 一列問題
#' @noRd
cw_issue <- function(where, what, value = "") {
  data.frame(where = where, what = what, value = value, stringsAsFactors = FALSE)
}

#' 把一張工作表轉成 rows（一列一個變項）和 long（變項 × 施測欄）
#' @noRd
cw_parse_sheet <- function(raw, file, sheet, kit) {
  where <- paste(file, sheet, sep = "|")
  hdr <- gsub("[[:space:]]", "", as.character(unlist(raw[1, ], use.names = FALSE)))
  hdr[is.na(hdr)] <- ""
  body <- raw[-1, , drop = FALSE]
  n <- nrow(body)
  empty <- list(rows = NULL, long = NULL)
  info <- cw_sheet_info()
  si <- match(sheet, info$sheet)
  # 原文："變項名稱"
  i_var <- match("\u8b8a\u9805\u540d\u7a31", hdr)
  if (is.na(si) || is.na(i_var)) {
    return(c(empty, list(issues = cw_issue(where, "sheet_skipped", "unknown sheet or no varname column"))))
  }
  inst <- info$instrument[si]
  resp <- info$respondent[si]
  # 原文："主構面"
  i_main <- match("\u4e3b\u69cb\u9762", hdr)
  # 原文："次構面"
  i_sub <- match("\u6b21\u69cb\u9762", hdr)
  # 原文："變項標籤"
  i_lab <- match("\u8b8a\u9805\u6a19\u7c64", hdr)
  # 原文："備註"
  i_note <- which(startsWith(hdr, "\u5099\u8a3b"))[1]
  # 原文："反向題"
  i_rev <- match("\u53cd\u5411\u984c", hdr)

  al <- kit$aliases
  al <- al[al$kind == "crosswalk_col" & al$target_type == "admin", , drop = FALSE]
  wcols <- which(hdr %in% al$alias)
  iss <- list()
  # 原文："月齡"
  odd <- setdiff(grep("\u6708\u9f61", hdr), wcols)
  if (length(odd)) iss[[1]] <- cw_issue(where, "wave_col_without_alias", hdr[odd])
  admin <- al$target_id[match(hdr[wcols], al$alias)]
  ds <- kit$datasets
  dsid <- vapply(admin, function(a) {
    x <- ds$datasetID[ds$adminID == a & ds$instrument == inst & ds$respondent == resp]
    if (length(x)) x[1] else NA_character_
  }, character(1), USE.NAMES = FALSE)
  if (anyNA(dsid)) {
    iss[[length(iss) + 1L]] <- cw_issue(where, "wave_col_without_dataset", hdr[wcols][is.na(dsid)])
  }
  form <- ds$formID[match(dsid, ds$datasetID)]

  col <- function(i) {
    if (is.na(i)) return(rep("", n))
    v <- trimws(as.character(body[[i]]))
    v[is.na(v)] <- ""
    v
  }
  var <- col(i_var)
  keep <- nzchar(var)
  pres <- toupper(as.matrix(body[, wcols, drop = FALSE]))
  pres[is.na(pres)] <- ""
  pres <- trimws(pres)
  rows <- data.frame(
    file = file, sheet = sheet, row = as.character(seq_len(n) + 1L), varname = var,
    main = col(i_main), sub = col(i_sub), label = col(i_lab), note = col(i_note),
    reverse = ifelse(nzchar(col(i_rev)), "1", ""), instrument = inst,
    pattern = apply(pres, 1L, paste, collapse = ""), stringsAsFactors = FALSE)[keep, , drop = FALSE]
  pres <- pres[keep, , drop = FALSE]
  bad <- !(pres %in% c("O", "X"))
  if (any(bad)) iss[[length(iss) + 1L]] <- cw_issue(where, "cell_not_OX", unique(pres[bad]))
  nr <- nrow(rows)
  nw <- length(wcols)
  long <- data.frame(
    file = rep(rows$file, nw), sheet = rep(rows$sheet, nw), row = rep(rows$row, nw),
    varname = rep(rows$varname, nw), instrument = inst,
    wave_idx = rep(seq_len(nw), each = nr), col = rep(hdr[wcols], each = nr),
    adminID = rep(admin, each = nr), datasetID = rep(dsid, each = nr),
    formID = rep(form, each = nr), present = as.vector(pres), stringsAsFactors = FALSE)
  long <- long[!is.na(long$datasetID), , drop = FALSE]
  list(rows = rows, long = long,
       issues = if (length(iss)) do.call(rbind, iss) else cw_issue(where, "", "")[0, ])
}

#' 對照表變項名 → item_uid（同問卷、不分大小寫；找不到就提一個新的）
#' @noRd
cw_item_ids <- function(instrument, varname, kit) {
  it <- kit$items
  hit <- match(paste(instrument, tolower(varname)), paste(it$instrument, tolower(it$varname)))
  data.frame(item_uid = ifelse(is.na(hit), paste0(instrument, ".", varname), it$item_uid[hit]),
             item_exists = ifelse(is.na(hit), "0", "1"), stringsAsFactors = FALSE)
}

#' 變項種類：取 dataset_vars 裡同名變項的 kind（出現過的第一個）；沒有就空白
#' @noRd
cw_var_kind <- function(rows, long, kit) {
  dv <- kit$dataset_vars
  k <- paste(long$file, long$sheet, long$row)
  dk <- dv$kind[match(paste(long$datasetID, tolower(long$varname)),
                      paste(dv$datasetID, tolower(dv$varname)))]
  ok <- !is.na(dk)
  first <- dk[ok][match(paste(rows$file, rows$sheet, rows$row), k[ok])]
  ifelse(is.na(first), "", first)
}

# 題目位置 ----

#' 一個空的位置表
#' @noRd
cw_empty_positions <- function() {
  cols <- c("datasetID", "varname", "position", "part", "source", "sourceID", "confidence",
            "basis")
  as.data.frame(stats::setNames(rep(list(character(0)), length(cols)), cols),
                stringsAsFactors = FALSE)
}

#' 從 .sav 變項標籤開頭抓題號：「一.1.2.這孩子…」→「一.1.2」
#'
#' 形式：國字大題＋數字（一.1.2.）→ high；「[教師基本資料]7.」→ high；只有數字
#' （5.1.）→ medium；單一數字（1.）→ low。sav_meta 是 datasetID → read_sav_meta() 的表。
#' @noRd
cw_sav_positions <- function(sav_meta, kit) {
  if (!length(sav_meta)) return(cw_empty_positions())
  dv <- kit$dataset_vars
  # 原文："[一二三四五六七八九十]+"
  zh <- "[\u4e00\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+"
  pats <- list(
    c(paste0("^(", zh, "(\\.[0-9]+)*)\\.[^0-9]"), "high", "chinese section number"),
    c("^(\\[[^]]+\\][0-9]+(\\.[0-9]+)*)\\.", "high", "bracketed section + number"),
    c("^([0-9]+(\\.[0-9]+)+)\\.[^0-9]", "medium", "numbers only (no section)"),
    c("^([0-9]+)\\.[^0-9]", "low", "single number (no section)"))
  out <- lapply(names(sav_meta), function(id) {
    m <- sav_meta[[id]]
    lab <- trimws(m$label)
    pos <- rep("", length(lab))
    conf <- rep("", length(lab))
    bas <- rep("", length(lab))
    for (p in pats) {
      hit <- !nzchar(pos) & grepl(p[1], lab, perl = TRUE)
      pos[hit] <- sub(paste0(p[1], ".*$"), "\\1", lab[hit], perl = TRUE)
      conf[hit] <- p[2]
      bas[hit] <- paste0(".sav label prefix (", p[3], ")")
    }
    vn <- dv$varname[match(paste(id, tolower(m$var)), paste(dv$datasetID, tolower(dv$varname)))]
    ok <- nzchar(pos) & !is.na(vn)
    k <- sum(ok)
    data.frame(datasetID = rep(id, k), varname = vn[ok], position = pos[ok], part = rep("", k),
               source = rep("sav_label", k), sourceID = rep("", k), confidence = conf[ok],
               basis = bas[ok], stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(cw_empty_positions()), out))
}

#' 從資料使用說明的不合理值表抓問卷題號
#'
#' 表的一列像「| 家長 第一部份 一、4.3 | 家長 | pfa0403 | 105 | 1 |」。只收題號欄有字、
#' 題號不含逗號（一格多題就無法分）、變項欄只有一個變項名的列。
#' @noRd
cw_usage_positions <- function(usage, kit) {
  iss <- list(cw_issue("", "", "")[0, ])
  if (!length(usage)) return(list(positions = cw_empty_positions(), issues = iss[[1]]))
  # 原文："家長" "家長" "親友" "親友" "教保" "教保人員" "教師" "教師"
  resp_map <- c("\u5bb6\u9577" = "\u5bb6\u9577", "\u89aa\u53cb" = "\u89aa\u53cb", "\u6559\u4fdd" = "\u6559\u4fdd\u4eba\u54e1", "\u6559\u5e2b" = "\u6559\u5e2b")
  # 原文："^(家長|親友|教保|教師)[[:space:]]*(第[一二三四五六七八九十]+部[份分])[[:space:]]*(.*)$"
  re <- "^(\u5bb6\u9577|\u89aa\u53cb|\u6559\u4fdd|\u6559\u5e2b)[[:space:]]*(\u7b2c[\u4e00\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u90e8[\u4efd\u5206])[[:space:]]*(.*)$"
  co <- kit$cohorts
  ad <- kit$admins
  ds <- kit$datasets
  dv <- kit$dataset_vars
  res <- list(cw_empty_positions())
  for (fn in names(usage)) {
    code <- sub("^us_([A-Z][0-9]+)W([0-9]+)\\.txt$", "\\1", fn)
    age <- suppressWarnings(as.integer(sub("^us_([A-Z][0-9]+)W([0-9]+)\\.txt$", "\\2", fn)))
    coh <- co$cohortID[match(code, co$code)]
    adm <- ad$adminID[ad$cohortID %in% coh & as.integer(ad$ageMonth) %in% age &
                        ad$status == "released"]
    if (length(adm) != 1L) {
      iss[[length(iss) + 1L]] <- cw_issue(fn, "usage_doc_without_admin")
      next
    }
    tx <- usage[[fn]]
    tx <- tx[startsWith(tx, "|")]
    cells <- strsplit(tx, "|", fixed = TRUE)
    c1 <- trimws(vapply(cells, function(x) if (length(x) >= 2L) x[2] else "", ""))
    c3 <- trimws(vapply(cells, function(x) if (length(x) >= 4L) x[4] else "", ""))
    hit <- grepl(re, c1) & grepl("^[A-Za-z][A-Za-z0-9_]*$", c3)
    if (!any(hit)) next
    who <- sub(re, "\\1", c1[hit])
    # 原文："部份" "部分"
    part <- sub("\u90e8\u4efd", "\u90e8\u5206", sub(re, "\\2", c1[hit]))
    pos <- trimws(sub(re, "\\3", c1[hit]))
    # 原文："[,，]"
    one <- nzchar(pos) & !grepl("[,\uff0c]", pos)
    dsid <- vapply(resp_map[who], function(r) {
      x <- ds$datasetID[ds$adminID == adm & ds$respondent == r]
      if (length(x)) x[1] else NA_character_
    }, character(1), USE.NAMES = FALSE)
    vn <- dv$varname[match(paste(dsid, tolower(c3[hit])), paste(dv$datasetID, tolower(dv$varname)))]
    ok <- one & !is.na(vn)
    if (any(!is.na(dsid) & is.na(vn))) {
      iss[[length(iss) + 1L]] <- cw_issue(fn, "position_var_not_in_dataset",
                                          unique(c3[hit][!is.na(dsid) & is.na(vn)]))
    }
    if (!any(ok)) next
    res[[length(res) + 1L]] <- data.frame(
      datasetID = dsid[ok], varname = vn[ok],
      # 「一、4.3」寫成和 .sav 標籤一樣的「一.4.3」
      # 原文："、"
      position = sub("\u3001", ".", pos[ok], fixed = TRUE), part = part[ok],
      source = "usage_doc", sourceID = paste0("US-", sub("^us_(.*)\\.txt$", "\\1", fn)),
      # 原文："usage doc error table (問卷題號 column)"
      confidence = "medium", basis = "usage doc error table (\u554f\u5377\u984c\u865f column)",
      stringsAsFactors = FALSE)
  }
  list(positions = do.call(rbind, res), issues = do.call(rbind, iss))
}

#' 題目位置：.sav 標籤為主，使用說明補缺、補「第幾部分」並互相印證
#' @noRd
cw_positions <- function(usage, sav_meta, kit) {
  sp <- cw_sav_positions(sav_meta, kit)
  up <- cw_usage_positions(usage, kit)
  u <- up$positions
  uk <- paste(u$datasetID, u$varname)
  n_pos <- tapply(u$position, uk, function(x) length(unique(x)))
  u <- u[!duplicated(uk), , drop = FALSE]
  uk <- paste(u$datasetID, u$varname)
  multi <- as.vector(n_pos[uk]) > 1L
  u$confidence[multi] <- "low"
  u$basis[multi] <- paste0(u$basis[multi], "; several positions listed, first kept")
  sk <- paste(sp$datasetID, sp$varname)
  i <- match(sk, uk)
  both <- !is.na(i)
  # 使用說明常只寫到上一層（一.6 對 一.6.1），也算一致
  up_i <- ifelse(both, u$position[i], "")
  agree <- both & (sp$position == up_i | startsWith(sp$position, paste0(up_i, ".")))
  sp$part[both] <- u$part[i[both]]
  sp$sourceID[both] <- u$sourceID[i[both]]
  sp$confidence[agree] <- "high"
  sp$basis[agree] <- paste0(sp$basis[agree], "; usage doc agrees")
  sp$basis[both & !agree] <- paste0(sp$basis[both & !agree], "; usage doc says ",
                                    u$position[i[both & !agree]])
  sp$confidence[both & !agree] <- "low"
  p <- rbind(sp, u[!(uk %in% sk), , drop = FALSE])
  rownames(p) <- NULL
  list(positions = p, issues = up$issues)
}

#' 已釋出資料檔中 kind = item 的列，有 position 的比例（每個資料檔一列＋合計）
#' @noRd
cw_position_coverage <- function(positions, kit) {
  ds <- kit$datasets
  rel <- ds$datasetID[ds$file != "v02" &
                        ds$adminID %in% kit$admins$adminID[kit$admins$status == "released"]]
  dv <- kit$dataset_vars
  dv <- dv[dv$datasetID %in% rel & dv$kind == "item", , drop = FALSE]
  has <- paste(dv$datasetID, dv$varname) %in% paste(positions$datasetID, positions$varname)
  n <- tapply(rep(1L, nrow(dv)), dv$datasetID, sum)
  k <- tapply(has, dv$datasetID, sum)
  out <- data.frame(datasetID = c(names(n), "ALL"), n_item = c(as.integer(n), nrow(dv)),
                    n_position = c(as.integer(k[names(n)]), sum(has)), stringsAsFactors = FALSE)
  out$pct <- round(100 * out$n_position / pmax(out$n_item, 1L), 1)
  out
}

#' 對照表 O／X 和 dataset_vars（.sav 表頭）是否一致，每個資料檔一列
#' @noRd
cw_presence <- function(long, kit) {
  dv <- kit$dataset_vars
  out <- lapply(unique(long$datasetID), function(id) {
    l <- long[long$datasetID == id, , drop = FALSE]
    d <- dv[dv$datasetID == id, , drop = FALSE]
    o <- unique(tolower(l$varname[l$present == "O"]))
    x <- setdiff(unique(tolower(l$varname[l$present == "X"])), o)
    di <- tolower(d$varname[d$kind == "item"])
    data.frame(datasetID = id, cw_O = length(o), cw_O_in_sav = sum(o %in% tolower(d$varname)),
               sav_item = length(di), sav_item_cw_O = sum(di %in% o),
               sav_item_cw_X = sum(di %in% x), sav_item_not_in_cw = sum(!(di %in% c(o, x))),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# 題組 ----

#' 題組名稱正規化：去空白、全形括號轉半形、去「(僅…)」、小寫
#' @noRd
cw_norm_label <- function(x) {
  # 原文："（）"
  x <- chartr("\uff08\uff09", "()", x)
  # 原文："\\(僅[^)]*\\)"
  x <- gsub("\\(\u50c5[^)]*\\)", "", x)
  x <- gsub("[[:space:]]", "", x)
  tolower(x)
}

#' 主構面＋次構面 → 一或多條路徑（每條是題組名稱的向量）
#'
#' 「_」切層級。「/」：主構面也有「/」→ 兩條路徑都成立（dual）；次構面「/」後面以
#' G1-G3 之類的題本代號開頭 → 那個題本改用後面那條（form_alt）；其他的「/」是名稱的一部分。
#' @noRd
cw_split_path <- function(main, sub) {
  clean <- function(x) {
    # 原文："（）"
    x <- trimws(chartr("\uff08\uff09", "()", x))
    # 原文："^[一二三四五六七八九十]+、"
    x <- sub("^[\u4e00\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u3001", "", x)
    x
  }
  age_of <- function(x) {
    # 原文："\\(僅[^)]*\\)"
    m <- regmatches(x, regexpr("\\(\u50c5[^)]*\\)", x))
    # 原文："^\\(僅|\\)$"
    if (length(m)) gsub("^\\(\u50c5|\\)$", "", m) else ""
  }
  segs_of <- function(m, s) {
    m <- clean(m)
    s <- clean(s)
    # 原文："反向題"
    if (s %in% c("", "NA", "\u53cd\u5411\u984c")) s <- ""
    age <- age_of(s)
    # 原文："\\(僅[^)]*\\)"
    s <- trimws(gsub("\\(\u50c5[^)]*\\)", "", s))
    ms <- strsplit(m, "_", fixed = TRUE)[[1]]
    ss <- if (nzchar(s)) strsplit(s, "_", fixed = TRUE)[[1]] else character(0)
    # 次構面切開後和主構面重複（如「親友女_親職參與」）就不切
    if (length(ss) > 1L && any(ss %in% ms)) ss <- s
    segs <- c(ms, ss)
    segs <- segs[nzchar(segs)]
    segs <- segs[c(TRUE, segs[-1] != segs[-length(segs)])]
    list(segs = segs, age = age)
  }
  tag_re <- "^(G[0-9]+-G[0-9]+)"
  ms <- strsplit(main, "/", fixed = TRUE)[[1]]
  ss <- if (nzchar(sub)) strsplit(sub, "/", fixed = TRUE)[[1]] else ""
  if (length(ms) > 1L && length(ss) == length(ms)) {
    return(lapply(seq_along(ms), function(i) c(segs_of(ms[i], ss[i]), kind = "dual", tag = "")))
  }
  if (length(ms) == 1L && length(ss) > 1L && any(grepl(tag_re, ss[-1]))) {
    return(lapply(seq_along(ss), function(i) {
      tg <- if (grepl(tag_re, ss[i])) sub(paste0(tag_re, ".*$"), "\\1", ss[i]) else ""
      c(segs_of(main, sub(tag_re, "", ss[i])), kind = "form_alt", tag = tg)
    }))
  }
  list(c(segs_of(main, sub), kind = "single", tag = ""))
}

#' 題組候選：路徑樹、和 kit 題組的對應、沿革
#' @noRd
cw_groups <- function(rows, long, kit) {
  sep <- " > "
  # 每列 × 題本（有 O 的）× 路徑
  lo <- long[long$present == "O" & !is.na(long$formID) & nzchar(long$formID), , drop = FALSE]
  rk <- paste(rows$file, rows$sheet, rows$row)
  lk <- paste(lo$file, lo$sheet, lo$row)
  rp <- list()
  for (i in seq_len(nrow(rows))) {
    forms <- unique(lo$formID[lk == rk[i]])
    if (!length(forms)) next
    alts <- cw_split_path(rows$main[i], rows$sub[i])
    tags <- vapply(alts, function(a) a$tag, "")
    for (a in alts) {
      if (!length(a$segs)) next
      f <- if (a$kind == "form_alt") {
        if (nzchar(a$tag)) forms[grepl(a$tag, forms, fixed = TRUE)]
        else forms[!vapply(forms, function(x) any(vapply(tags[nzchar(tags)], grepl, logical(1), x = x, fixed = TRUE)), logical(1))]
      } else forms
      if (!length(f)) next
      rp[[length(rp) + 1L]] <- data.frame(
        key = rk[i], instrument = rows$instrument[i], varname = rows$varname[i],
        item_uid = rows$item_uid[i], item_exists = rows$item_exists[i], formID = f,
        path = paste(a$segs, collapse = sep), alt_kind = a$kind, tag = a$tag, age = a$age,
        stringsAsFactors = FALSE)
    }
  }
  rp <- do.call(rbind, rp)

  # 節點：每個路徑前綴
  up <- unique(rp[, c("instrument", "path")])
  nodes <- list()
  for (i in seq_len(nrow(up))) {
    s <- strsplit(up$path[i], sep, fixed = TRUE)[[1]]
    for (k in seq_along(s)) {
      nodes[[length(nodes) + 1L]] <- data.frame(
        instrument = up$instrument[i], path = paste(s[1:k], collapse = sep),
        parent_path = if (k > 1L) paste(s[1:(k - 1L)], collapse = sep) else "",
        label = s[k], depth = k, stringsAsFactors = FALSE)
    }
  }
  nodes <- do.call(rbind, nodes)
  nodes <- nodes[!duplicated(paste(nodes$instrument, nodes$path)), , drop = FALSE]
  nodes$leaf <- paste(nodes$instrument, nodes$path) %in% paste(rp$instrument, rp$path)

  # 對應方法一：名稱（本層，再往上接「_」）
  g <- kit$groups
  g_inst <- sub("\\..*$", "", g$groupID)
  g_lab <- cw_norm_label(g$label)
  g_id <- cw_norm_label(sub("^[A-Z]\\.", "", g$groupID))
  nodes$groupID <- ""
  nodes$method <- "none"
  nodes$share <- ""
  nodes$n_items <- ""
  for (i in seq_len(nrow(nodes))) {
    s <- strsplit(nodes$path[i], sep, fixed = TRUE)[[1]]
    k <- length(s)
    for (j in k:1) {
      cand <- paste(s[j:k], collapse = "_")
      exact <- which(g_inst == nodes$instrument[i] & g$label == cand)
      nrm <- which(g_inst == nodes$instrument[i] &
                     (g_lab == cw_norm_label(cand) | g_id == cw_norm_label(cand)))
      if (length(exact)) {
        nodes$groupID[i] <- g$groupID[exact[1]]
        nodes$method[i] <- if (j == k) "label_exact" else "label_joined"
        break
      }
      if (length(nrm)) {
        nodes$groupID[i] <- g$groupID[nrm[1]]
        nodes$method[i] <- "label_normalized"
        break
      }
    }
  }
  # 對應方法二：葉節點的題目在 kit 裡同屬哪個題組（≥ 80%，取最小的題組）
  gm <- kit$group_members
  gsize <- tapply(gm$item_uid, gm$groupID, function(x) length(unique(x)))
  for (i in which(nodes$leaf)) {
    u <- unique(rp$item_uid[rp$instrument == nodes$instrument[i] & rp$path == nodes$path[i] &
                              rp$item_exists == "1"])
    m <- gm[gm$item_uid %in% u, , drop = FALSE]
    nu <- length(unique(m$item_uid))
    if (!nu) next
    cnt <- tapply(m$item_uid, m$groupID, function(x) length(unique(x)))
    # 只收大小相近的題組；太大的（例如整個區塊）不算對到
    ok <- names(cnt)[cnt / nu >= 0.8 & gsize[names(cnt)] <= 2 * nu + 5]
    if (!length(ok)) next
    best <- ok[order(gsize[ok], ok)][1]
    nodes$n_items[i] <- as.character(nu)
    nodes$share[i] <- sprintf("%d/%d", cnt[[best]], nu)
    if (nodes$method[i] == "none") {
      nodes$groupID[i] <- best
      nodes$method[i] <- "member_overlap"
    } else if (nodes$groupID[i] != best) {
      nodes$method[i] <- paste0(nodes$method[i], ";overlap_prefers_", best)
    }
  }
  # 沒對到的：提一個新 ID（同名不同路徑時接上一層名稱）
  dup_lab <- paste(nodes$instrument, nodes$label)
  amb <- dup_lab %in% dup_lab[duplicated(dup_lab)]
  par_lab <- sub(paste0("^.*", sep), "", nodes$parent_path)
  prop <- paste0(nodes$instrument, ".",
                 ifelse(amb & nzchar(nodes$parent_path), paste0(par_lab, "_", nodes$label),
                        nodes$label))
  nodes$proposed <- ifelse(nodes$method == "none", prop, "")
  nodes$node_id <- ifelse(nzchar(nodes$groupID), nodes$groupID, nodes$proposed)
  nodes$confidence <- ifelse(grepl("^label_exact$|^label_joined$", nodes$method), "high",
                      ifelse(nodes$method == "none", "low", "medium"))
  nodes$basis <- ifelse(nodes$method == "none", "no kit group with this label",
                        paste0("crosswalk label matched by ", nodes$method,
                               ifelse(nzchar(nodes$share), paste0(" (members ", nodes$share, ")"), "")))
  group_map <- data.frame(
    instrument = nodes$instrument, cw_path = nodes$path, label = nodes$label,
    depth = as.character(nodes$depth), leaf = ifelse(nodes$leaf, "1", "0"),
    groupID = nodes$groupID, method = nodes$method, members = nodes$share,
    proposed_groupID = nodes$proposed, confidence = nodes$confidence, basis = nodes$basis,
    stringsAsFactors = FALSE)

  id_of <- function(inst, path) nodes$node_id[match(paste(inst, path), paste(nodes$instrument, nodes$path))]
  rp$groupID <- id_of(rp$instrument, rp$path)

  # 題組樹：每個題本裡出現的路徑都展開
  fp <- unique(rp[, c("formID", "instrument", "path")])
  ed <- list()
  for (i in seq_len(nrow(fp))) {
    s <- strsplit(fp$path[i], sep, fixed = TRUE)[[1]]
    for (k in seq_along(s)) {
      pth <- paste(s[1:k], collapse = sep)
      ppth <- if (k > 1L) paste(s[1:(k - 1L)], collapse = sep) else ""
      ed[[length(ed) + 1L]] <- data.frame(
        formID = fp$formID[i], groupID = id_of(fp$instrument[i], pth),
        parentID = if (nzchar(ppth)) id_of(fp$instrument[i], ppth) else "",
        cw_path = pth, stringsAsFactors = FALSE)
    }
  }
  ed <- do.call(rbind, ed)
  ed <- ed[ed$groupID != ed$parentID, , drop = FALSE]
  ed <- ed[!duplicated(paste(ed$formID, ed$groupID, ed$parentID)), , drop = FALSE]
  n_it <- tapply(rp$item_uid, paste(rp$formID, rp$groupID), function(x) length(unique(x)))
  gt <- kit$group_tree
  ed$label <- sub(paste0("^.*", sep), "", ed$cw_path)
  ed$in_kit_groups <- ifelse(ed$groupID %in% g$groupID, "1", "0")
  ed$in_kit_tree <- ifelse(paste(ed$formID, ed$groupID, ed$parentID) %in%
                             paste(gt$formID, gt$groupID, gt$parentID), "1", "0")
  ni <- as.vector(n_it[paste(ed$formID, ed$groupID)])
  ed$n_items <- as.character(ifelse(is.na(ni), 0L, ni))
  par_known <- !nzchar(ed$parentID) | ed$parentID %in% g$groupID
  ed$confidence <- ifelse(ed$in_kit_tree == "1", "high",
                   ifelse(ed$in_kit_groups == "1" & par_known, "medium", "low"))
  ed$basis <- ifelse(ed$in_kit_tree == "1", "crosswalk path agrees with kit group_tree",
              ifelse(ed$in_kit_groups == "1" & par_known,
                     "both groups exist in kit; edge only in crosswalk",
                     "edge needs a new group (not in kit$groups)"))
  ed$source <- "crosswalk"
  group_paths <- ed[, c("formID", "groupID", "parentID", "label", "cw_path", "n_items",
                        "in_kit_groups", "in_kit_tree", "source", "confidence", "basis")]
  rownames(group_paths) <- NULL

  list(group_map = group_map, group_paths = group_paths, row_paths = rp,
       group_lineage = cw_lineage(rp, long, kit))
}

#' 題組沿革候選：「/」後面的題本改用另一條路徑；或兩份對照表給同一變項不同的題組
#' @noRd
cw_lineage <- function(rp, long, kit) {
  cols <- c("from_group", "to_group", "from_formID", "formID", "year", "n_items",
            "varnames", "confidence", "basis")
  out <- list()
  ad <- kit$admins
  first_year <- function(f) {
    l <- long[long$present == "O" & long$formID %in% f, , drop = FALSE]
    y <- substr(ad$surveyFrom[match(l$adminID, ad$adminID)], 1, 4)
    y <- y[!is.na(y) & nzchar(y)]
    if (length(y)) min(y) else ""
  }
  # 1) form_alt
  fa <- rp[rp$alt_kind == "form_alt", , drop = FALSE]
  if (nrow(fa)) {
    for (k in unique(fa$key)) {
      x <- fa[fa$key == k, , drop = FALSE]
      old <- x[!nzchar(x$tag), , drop = FALSE]
      new <- x[nzchar(x$tag), , drop = FALSE]
      if (!nrow(old) || !nrow(new)) next
      out[[length(out) + 1L]] <- data.frame(
        from_group = old$groupID[1], to_group = new$groupID[1],
        from_formID = paste(unique(old$formID), collapse = ";"), formID = new$formID[1],
        year = first_year(new$formID[1]), varname = x$varname[1], kind = "form_alt",
        stringsAsFactors = FALSE)
    }
  }
  # 2) 兩份對照表、同一題本、同一變項，題組不同
  rp$file <- sub(" .*$", "", rp$key)
  rp$lv <- paste(rp$instrument, tolower(rp$varname), rp$formID)
  multi <- tapply(rp$file, rp$lv, function(x) length(unique(x)) > 1L)
  for (lv in names(multi)[multi]) {
    x <- rp[rp$lv == lv & rp$alt_kind != "dual", , drop = FALSE]
    gs <- tapply(x$groupID, x$file, function(v) paste(sort(unique(v)), collapse = ";"))
    if (length(unique(gs)) < 2L) next
    fl <- names(gs)
    yr <- vapply(fl, function(f) {
      l <- long[long$file == f & long$present == "O" & long$formID == x$formID[1] &
                  tolower(long$varname) == tolower(x$varname[1]), , drop = FALSE]
      y <- substr(ad$surveyFrom[match(l$adminID, ad$adminID)], 1, 4)
      y <- y[!is.na(y) & nzchar(y)]
      if (length(y)) min(y) else "9999"
    }, "")
    o <- order(yr)
    out[[length(out) + 1L]] <- data.frame(
      from_group = gs[[o[1]]], to_group = gs[[o[2]]], from_formID = x$formID[1],
      formID = x$formID[1], year = yr[o[2]], varname = x$varname[1], kind = "cross_file",
      stringsAsFactors = FALSE)
  }
  if (!length(out)) {
    return(as.data.frame(stats::setNames(rep(list(character(0)), length(cols)), cols),
                         stringsAsFactors = FALSE))
  }
  r <- do.call(rbind, out)
  gk <- paste(r$from_group, r$to_group, r$from_formID, r$formID, r$kind)
  agg <- r[!duplicated(gk), , drop = FALSE]
  agg$n_items <- as.character(as.integer(table(gk)[gk[!duplicated(gk)]]))
  agg$varnames <- vapply(gk[!duplicated(gk)], function(k) paste(r$varname[gk == k], collapse = ";"), "")
  gid <- kit$groups$groupID
  both <- agg$from_group %in% gid & agg$to_group %in% gid
  agg$confidence <- ifelse(agg$kind == "form_alt" & both, "high",
                    ifelse(agg$kind == "form_alt" | both, "medium", "low"))
  agg$basis <- ifelse(agg$kind == "form_alt",
                      "crosswalk sub-construct 'A/G1-G3 B': form-tagged alternative path",
                      "the two crosswalks put this variable in different groups for the same form")
  rownames(agg) <- NULL
  agg[, cols]
}

# 備註 ----

#' 解析備註：類型、從哪一欄開始、題意是否不變
#' @noRd
cw_notes <- function(rows, long) {
  x <- rows[nzchar(rows$note), , drop = FALSE]
  cols <- c("file", "sheet", "row", "varname", "item_uid", "note", "type", "same_meaning",
            "from_col", "effective_admin", "confidence", "basis")
  if (!nrow(x)) {
    return(as.data.frame(stats::setNames(rep(list(character(0)), length(cols)), cols),
                         stringsAsFactors = FALSE))
  }
  nt <- x$note
  # 原文："反向計分" "文字"
  type <- ifelse(grepl("\u53cd\u5411\u8a08\u5206", nt) & !grepl("\u6587\u5b57", nt), "reverse",
          # 原文："構面總分"
          ifelse(grepl("\u69cb\u9762\u7e3d\u5206", nt), "scoring",
          # 原文："調查區間"
          ifelse(grepl("\u8abf\u67e5\u5340\u9593", nt), "period",
          # 原文："選項"
          ifelse(grepl("\u9078\u9805", nt), "options",
          # 原文："新增此題|回溯題組|新增「天數」"
          ifelse(grepl("\u65b0\u589e\u6b64\u984c|\u56de\u6eaf\u984c\u7d44|\u65b0\u589e\u300c\u5929\u6578\u300d", nt), "added",
          # 原文："文字|微調|更改為"
          ifelse(grepl("\u6587\u5b57|\u5fae\u8abf|\u66f4\u6539\u70ba", nt), "wording", "other"))))))
  x$type <- type
  # 原文："題意不變|意義不變|意義相同"
  x$same_meaning <- ifelse(grepl("\u984c\u610f\u4e0d\u8b8a|\u610f\u7fa9\u4e0d\u8b8a|\u610f\u7fa9\u76f8\u540c", nt), "1", "")
  # 原文："([0-9]+)M開始" "^.*?([0-9]+)M開始.*$"
  age <- ifelse(grepl("([0-9]+)M\u958b\u59cb", nt), sub("^.*?([0-9]+)M\u958b\u59cb.*$", "\\1", nt, perl = TRUE), "")
  x$from_col <- ""
  x$effective_admin <- ""
  x$confidence <- ""
  x$basis <- ""
  for (i in seq_len(nrow(x))) {
    if (!nzchar(age[i])) next
    l <- long[long$file == x$file[i] & long$sheet == x$sheet[i] & long$row == x$row[i], , drop = FALSE]
    w <- l[grepl(paste0("_", age[i], "M$"), l$col), , drop = FALSE]
    if (!nrow(w)) next
    x$from_col[i] <- w$col[1]
    x$effective_admin[i] <- w$adminID[1]
    x$confidence[i] <- if (w$wave_idx[1] > 1L) "high" else "low"
    x$basis[i] <- if (w$wave_idx[1] > 1L) "note names the starting wave"
                  else "note's starting wave is this sheet's first column (note copied from the other cohort?)"
  }
  x[, cols]
}

# 題目關係 ----

#' 同一資料檔裡某變項的 itemVer（dataset_vars；不分大小寫）
#' @noRd
cw_dv_ver <- function(datasetID, varname, kit) {
  dv <- kit$dataset_vars
  v <- dv$itemVer[match(paste(datasetID, tolower(varname)), paste(dv$datasetID, tolower(dv$varname)))]
  ifelse(is.na(v), "", v)
}

#' 兩個標籤的相似度：字元二元組 Jaccard
#' @noRd
cw_label_sim <- function(a, b) {
  bg <- function(s) {
    # 原文："[[:space:][:punct:]、，。？：─（）「」『』]"
    s <- gsub("[[:space:][:punct:]\u3001\uff0c\u3002\uff1f\uff1a\u2500\uff08\uff09\u300c\u300d\u300e\u300f]", "", s)
    ch <- strsplit(s, "")[[1]]
    if (length(ch) < 2L) return(ch)
    unique(paste0(ch[-length(ch)], ch[-1]))
  }
  x <- bg(a)
  y <- bg(b)
  if (!length(x) && !length(y)) return(1)
  length(intersect(x, y)) / length(union(x, y))
}

#' item_relations 候選：備註（改字、改區間、改選項）、X／Xn 換題號、同名出現兩次
#' @noRd
cw_relation_hints <- function(rows, long, notes, kit) {
  cols <- c("from_uid", "from_ver", "to_uid", "to_ver", "kind", "similarity", "comparable",
            "meaning_changed", "effective_admin", "reason", "sourceID", "from_varname",
            "to_varname", "from_admins", "to_admins", "from_exists", "to_exists",
            "label_sim", "confidence", "basis")
  rk <- paste(rows$file, rows$sheet, rows$row)
  lk <- paste(long$file, long$sheet, long$row)
  src <- function(f) paste0("CW-", sub("^cw_(.*)\\.xlsx$", "\\1", f))
  o_of <- function(k) {
    l <- long[lk == k & long$present == "O", , drop = FALSE]
    l[order(l$wave_idx), , drop = FALSE]
  }
  sim_lab <- function(s) ifelse(s >= 0.999, "identical", ifelse(s >= 0.8, "high",
                         ifelse(s >= 0.5, "medium", "low")))
  out <- list()
  add <- function(...) out[[length(out) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE)

  # 1) 備註
  nr <- notes[notes$type %in% c("wording", "period", "options") & nzchar(notes$effective_admin), ,
              drop = FALSE]
  for (i in seq_len(nrow(nr))) {
    k <- paste(nr$file[i], nr$sheet[i], nr$row[i])
    l <- o_of(k)
    eff <- long[lk == k & long$adminID == nr$effective_admin[i], , drop = FALSE]
    before <- l[l$wave_idx < eff$wave_idx[1], , drop = FALSE]
    after <- l[l$wave_idx >= eff$wave_idx[1], , drop = FALSE]
    if (!nrow(before) || !nrow(after)) next
    r <- rows[match(k, rk), ]
    fv <- cw_dv_ver(before$datasetID[nrow(before)], r$varname, kit)
    tv <- cw_dv_ver(after$datasetID[1], r$varname, kit)
    ty <- nr$type[i]
    same <- nr$same_meaning[i] == "1"
    simv <- if (ty == "wording") "high" else "medium"
    comp <- if (same) "yes" else if (ty == "wording") "" else "partial"
    mc <- if (same) "0" else if (ty == "period") "1" else ""
    conf <- if (fv == tv || !nzchar(fv) || !nzchar(tv)) "low" else if (same) "high" else "medium"
    add(from_uid = r$item_uid, from_ver = fv, to_uid = r$item_uid, to_ver = tv, kind = "revised",
        similarity = simv, comparable = comp, meaning_changed = mc,
        effective_admin = nr$effective_admin[i], reason = nr$note[i], sourceID = src(r$file),
        from_varname = r$varname, to_varname = r$varname,
        from_admins = paste(before$adminID, collapse = ";"),
        to_admins = paste(after$adminID, collapse = ";"),
        from_exists = r$item_exists, to_exists = r$item_exists, label_sim = "",
        confidence = conf,
        basis = paste0("crosswalk note (", ty, ")",
                       if (fv == tv && nzchar(fv)) "; kit has the same itemVer on both sides" else "",
                       if (!nzchar(fv) || !nzchar(tv)) "; itemVer missing in dataset_vars" else ""))
  }

  # 2) X → Xn（同一張表），以及同一變項名出現兩列
  for (sh in unique(paste(rows$file, rows$sheet))) {
    r <- rows[paste(rows$file, rows$sheet) == sh, , drop = FALSE]
    lv <- tolower(r$varname)
    pairs <- list()
    isn <- grepl("n$", lv) & sub("n$", "", lv) %in% lv
    for (j in which(isn)) {
      pairs[[length(pairs) + 1L]] <- c(which(lv == sub("n$", "", lv[j]))[1], j, "renamed")
    }
    for (v in unique(lv[duplicated(lv)])) {
      w <- which(lv == v)
      pairs[[length(pairs) + 1L]] <- c(w[1], w[2], "same_name")
    }
    for (p in pairs) {
      a <- r[as.integer(p[1]), ]
      b <- r[as.integer(p[2]), ]
      la <- o_of(paste(a$file, a$sheet, a$row))
      lb <- o_of(paste(b$file, b$sheet, b$row))
      if (!nrow(la) || !nrow(lb)) next
      overlap <- intersect(la$adminID, lb$adminID)
      fv <- cw_dv_ver(la$datasetID[nrow(la)], a$varname, kit)
      tv <- cw_dv_ver(lb$datasetID[1], b$varname, kit)
      if (!nzchar(tv) && b$item_exists == "0") tv <- "1"
      s <- cw_label_sim(a$label, b$label)
      seq_ok <- max(la$wave_idx) < min(lb$wave_idx)
      conf <- if (seq_ok && s >= 0.5 && a$item_exists == "1" && b$item_exists == "1") "high"
              else if (seq_ok && s >= 0.3) "medium" else "low"
      kind <- if (p[3] == "renamed") "renamed" else "revised"
      add(from_uid = a$item_uid, from_ver = fv, to_uid = b$item_uid, to_ver = tv, kind = kind,
          similarity = sim_lab(s), comparable = "", meaning_changed = "",
          effective_admin = lb$adminID[1], reason = paste(c(a$note, b$note)[nzchar(c(a$note, b$note))],
                                                         collapse = " | "),
          sourceID = src(a$file), from_varname = a$varname, to_varname = b$varname,
          from_admins = paste(la$adminID, collapse = ";"), to_admins = paste(lb$adminID, collapse = ";"),
          from_exists = a$item_exists, to_exists = b$item_exists, label_sim = sprintf("%.2f", s),
          confidence = conf,
          basis = paste0(if (kind == "renamed") "crosswalk X/Xn pair" else "same varname listed twice",
                         if (seq_ok) "; O/X waves do not overlap" else
                           paste0("; waves overlap (", paste(overlap, collapse = ";"), ")"),
                         if (b$item_exists == "0") "; new varname not in kit$items" else ""))
    }
  }
  if (!length(out)) {
    return(as.data.frame(stats::setNames(rep(list(character(0)), length(cols)), cols),
                         stringsAsFactors = FALSE))
  }
  res <- do.call(rbind, out)
  res <- res[!duplicated(paste(res$from_uid, res$from_ver, res$to_uid, res$to_ver, res$kind)), ,
             drop = FALSE]
  rownames(res) <- NULL
  res[, cols]
}

# 題組成員 ----

#' group_members 缺的成員：對照表的葉節點題組；換題號的新題沿用舊題的題組
#' @noRd
cw_member_hints <- function(rows, grp, rel, kit) {
  cols <- c("formID", "groupID", "item_uid", "varname", "item_exists", "group_exists",
            "reverse", "cw_path", "current_groups", "source", "confidence", "basis")
  gm <- kit$group_members
  gid <- kit$groups$groupID
  gt <- kit$group_tree
  rp <- grp$row_paths
  kind <- rows$kind[match(rp$key, paste(rows$file, rows$sheet, rows$row))]
  rp <- rp[kind %in% c("item", ""), , drop = FALSE]
  rev <- rows$reverse[match(rp$key, paste(rows$file, rows$sheet, rows$row))]
  cur <- function(u, f) {
    vapply(seq_along(u), function(i) {
      paste(sort(unique(gm$groupID[gm$item_uid == u[i] & gm$formID == f[i]])), collapse = ";")
    }, "")
  }
  a <- data.frame(formID = rp$formID, groupID = rp$groupID, item_uid = rp$item_uid,
                  varname = rp$varname, item_exists = rp$item_exists,
                  group_exists = ifelse(rp$groupID %in% gid, "1", "0"),
                  reverse = ifelse(is.na(rev), "", rev), cw_path = rp$path, source = "cw_path",
                  stringsAsFactors = FALSE)
  # 換題號：新題接舊題在同一題本的題組
  rn <- rel[rel$kind == "renamed", , drop = FALSE]
  b <- list()
  for (i in seq_len(nrow(rn))) {
    forms <- unique(rp$formID[rp$item_uid == rn$to_uid[i]])
    old <- gm[gm$item_uid == rn$from_uid[i], , drop = FALSE]
    if (!nrow(old) || !length(forms)) next
    for (f in forms) {
      og <- unique(old$groupID[old$formID == f])
      if (!length(og)) og <- unique(old$groupID)
      # 只接最底層：拿掉同一組裡是別人上層的題組
      og <- setdiff(og, gt$parentID[gt$formID == f & gt$groupID %in% og])
      b[[length(b) + 1L]] <- data.frame(
        formID = f, groupID = og, item_uid = rn$to_uid[i], varname = rn$to_varname[i],
        item_exists = rn$to_exists[i], group_exists = "1", reverse = "",
        cw_path = "", source = paste0("renamed_from:", rn$from_uid[i]), stringsAsFactors = FALSE)
    }
  }
  h <- rbind(a, do.call(rbind, b))
  h <- h[!(paste(h$formID, h$groupID, h$item_uid) %in% paste(gm$formID, gm$groupID, gm$item_uid)), ,
         drop = FALSE]
  # 同一個（題本、題組、題目）兩個來源都有：保留，來源接起來
  k <- paste(h$formID, h$groupID, h$item_uid)
  srcs <- tapply(h$source, k, function(x) paste(unique(x), collapse = "+"))
  h <- h[!duplicated(k), , drop = FALSE]
  h$source <- as.vector(srcs[paste(h$formID, h$groupID, h$item_uid)])
  h$current_groups <- cur(h$item_uid, h$formID)
  # 對照表的題組 kit 裡沒有、題目在這個題本又已經有題組：只是名稱不同，不列
  h <- h[!(h$group_exists == "0" & nzchar(h$current_groups)), , drop = FALSE]
  agree <- grepl("cw_path", h$source) & grepl("renamed_from", h$source)
  h$confidence <- ifelse(h$group_exists == "0", "low",
                  ifelse(agree | (!nzchar(h$current_groups)), "high", "medium"))
  h$basis <- paste0(
    ifelse(grepl("renamed_from", h$source), "new varname inherits the renamed item's group",
           "crosswalk construct path leaf"),
    ifelse(agree, "; crosswalk path agrees", ""),
    ifelse(h$group_exists == "0", "; group not in kit (proposed ID)", ""),
    ifelse(nzchar(h$current_groups), "; item already in other group(s) in this form",
           "; item has no group in this form"),
    ifelse(h$item_exists == "0", "; item not in kit$items yet", ""))
  rownames(h) <- NULL
  h <- h[order(h$confidence != "high", h$formID, h$groupID, h$item_uid), cols]
  rownames(h) <- NULL
  h
}

#' 把 import_crosswalk() 的候選寫成 CSV（UTF-8）
#' @param cw Output of [import_crosswalk()].
#' @param dir Target folder.
#' @return The written paths, invisibly.
#' @noRd
write_crosswalk_candidates <- function(cw, dir = "data-raw/candidates") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  out <- c(cw_positions = "positions", cw_group_map = "group_map",
           cw_group_paths = "group_paths", cw_group_lineage = "group_lineage",
           cw_relation_hints = "relation_hints", cw_member_hints = "member_hints",
           cw_notes = "notes")
  paths <- file.path(dir, paste0(names(out), ".csv"))
  for (i in seq_along(out)) readr::write_csv(cw[[out[[i]]]], paths[i], na = "")
  invisible(paths)
}
