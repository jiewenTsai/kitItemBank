# R/admin_merge.R — 0.3.3：併入新施測（一次新施測的 .sav 表頭 → 題庫的新列）
#
# 兩步，中間由維護者看過：
#   propose_admin_merge()  把新施測每個資料檔的每個變項分類成提案（一列一個），
#                          每列有信心程度（high／medium／low）和白話理由
#   apply_admin_merge()    照接受的提案產生題庫的新列（admins、datasets、dataset_vars、
#                          items、item_versions、item_relations、value_codes、
#                          block_status、group_members），結果要過 check_kit()
# 只看 .sav 表頭（變項名、標籤、值標籤），不碰個人層級的資料。
#
# 提案的類別（class）：
#   same                  題目和版本題庫裡已經有（標籤、選項都對得上）
#   new_version           同一個變項名，但標籤或選項變了 → 新的 itemVer ＋ revised 關係
#   renamed               變項名不一樣，但對得上一題舊題（X → Xn，或標籤、選項一樣）
#                         → 新題號 ＋ renamed 關係
#   new_item              題庫裡沒有的新題
#   not_item              系統、加權、衍生變項（不是題目），照樣列進資料檔
#   dropped_or_block      上一次施測同一個資料檔有、這次沒有的題目：整個區塊沒問 →
#                         block_status none；只少幾題 → 之後沒再用就停用（deprecated），
#                         之後還有用就只記下（not_asked）；上一次的區塊狀態沿用
#   missing_scheme_change 值標籤裡的遺漏代碼和上一次用的方案不一樣 → 建議換方案
#
# 規則（和 R/propose_relations.R 一樣的相似度與選項比對）：
# - 「上一次」= 同一個世代、同一種資料檔（-P、-PK、-T、-S、-C）、時間在這次之前最近的一次；
#   同世代沒有，就找別的世代時間在這次之前最近的同一種資料檔。
# - 對題目：變項名（不分大小寫）對到題庫的 item_uid（match_item_uid()）。
# - 比版本：先比上一次用的版本，再比其他版本。標籤相似度 identical／high（或有一邊沒有
#   標籤）、而且選項沒有不同 → same；都不像 → new_version。
# - 換題號：對照表指定、X → Xn（去掉結尾的 n 對到舊題），或上一次有、這次不見的題目裡
#   標籤和選項都一樣的。

AM_CLASSES <- c("same", "new_version", "renamed", "new_item", "not_item",
                "dropped_or_block", "missing_scheme_change")

# 提案寫成 Excel 時的中文欄名（admin_merge_workbook()）
# 原文：提案編號 | 類別代碼 | 類別
AM_COLUMN_ZH <- c(proposal_id = "\u63d0\u6848\u7de8\u865f", class = "\u985e\u5225\u4ee3\u78bc", class_label = "\u985e\u5225",
                  # 原文：動作 | 資料檔 | 變項 | 順序
                  action = "\u52d5\u4f5c", datasetID = "\u8cc7\u6599\u6a94", varname = "\u8b8a\u9805", order = "\u9806\u5e8f",
                  # 原文：題號位置 | 種類 | 題目代碼 | 版本
                  position = "\u984c\u865f\u4f4d\u7f6e", kind = "\u7a2e\u985e", item_uid = "\u984c\u76ee\u4ee3\u78bc", itemVer = "\u7248\u672c",
                  # 原文：原題目代碼 | 原版本 | 區塊
                  from_uid = "\u539f\u984c\u76ee\u4ee3\u78bc", from_ver = "\u539f\u7248\u672c", block = "\u5340\u584a",
                  # 原文：標籤相似度 | 選項相同
                  similarity = "\u6a19\u7c64\u76f8\u4f3c\u5ea6", values_match = "\u9078\u9805\u76f8\u540c",
                  # 原文：遺漏代碼方案 | 上一次的資料檔 | 標籤
                  scheme = "\u907a\u6f0f\u4ee3\u78bc\u65b9\u6848", prev_dataset = "\u4e0a\u4e00\u6b21\u7684\u8cc7\u6599\u6a94", label = "\u6a19\u7c64",
                  # 原文：選項 | 信心 | 理由
                  values = "\u9078\u9805", confidence = "\u4fe1\u5fc3", reason = "\u7406\u7531",
                  # 原文：決定（接受／不接受／再討論）
                  decision = "\u6c7a\u5b9a\uff08\u63a5\u53d7\uff0f\u4e0d\u63a5\u53d7\uff0f\u518d\u8a0e\u8ad6\uff09")

# 小工具 ----

#' 資料檔代號的後綴：M03M048-PK → "PK"
#' @noRd
am_suffix <- function(datasetID) sub("^.*-", "", datasetID)

#' 後綴 → 問卷別（PK 是親友版，也是家長問卷）
#' @noRd
am_instrument <- function(suffix) {
  ins <- substr(suffix, 1L, 1L)
  ifelse(ins %in% KIT_ENUMS$instrument, ins, "")
}

#' .sav 標籤去掉題號（「四.33」「1.」「6.1.」）和方括號裡的跳題指示，存成題目文字
#' @noRd
am_clean_label <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  # 原文：[一二三四五六七八九十]+
  zh <- "[\u4e00\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+"
  # 原文：(\\.[0-9]+)*[.、]\\s*
  x <- gsub(paste0("^\\s*", zh, "(\\.[0-9]+)*[.\u3001]\\s*"), "", x, perl = TRUE)
  # 原文：(^|(?<=[：:]))\\s*[0-9]+(\\.[0-9]+)*\\.(?![0-9])\\s*
  x <- gsub("(^|(?<=[\uff1a:]))\\s*[0-9]+(\\.[0-9]+)*\\.(?![0-9])\\s*", "", x, perl = TRUE)
  x <- gsub("^\\s*\\[[^]]*\\]\\s*[0-9]+(\\.[0-9]+)*\\.(?![0-9])\\s*", "", x, perl = TRUE)
  trimws(x)
}

#' 全形英數、標點轉半形（Ａ→A、（→(），全形空白轉半形空白
#' @noRd
am_halfwidth <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  vapply(x, function(s) {
    if (!nzchar(s)) return(s)
    cp <- utf8ToInt(s)
    fw <- cp >= 0xFF01 & cp <= 0xFF5E
    cp[fw] <- cp[fw] - 0xFEE0L
    cp[cp == 0x3000] <- 0x20L
    intToUtf8(cp)
  }, character(1), USE.NAMES = FALSE)
}

#' 比較用的標籤：半形、去題號和跳題指示、去空白標點符號
#' @noRd
am_norm_label <- function(x) rel_norm_label(am_clean_label(am_halfwidth(gsub("[\r\n]+", " ", x))))

#' 兩個標籤的相似度（identical／high／medium／low；有一邊空白為 ""）
#'
#' 先用 am_norm_label() 整理；一邊被截斷或只多了共用題幹（短的那個整段在長的裡面，
#' 而且至少 6 個字）算 high。其餘交給 rel_similarity()。
#' @noRd
am_label_sim <- function(a, b, stem = "") {
  na <- am_norm_label(a)
  nb <- am_norm_label(b)
  if (!nzchar(na) || !nzchar(nb)) return("")
  if (na == nb) return("identical")
  short <- if (nchar(na) <= nchar(nb)) na else nb
  long <- if (nchar(na) <= nchar(nb)) nb else na
  if (nchar(short) >= 6L && grepl(short, long, fixed = TRUE)) return("high")
  # 有題幹時，拿掉和不拿掉都比，取比較像的
  lv <- c("low", "medium", "high", "identical")
  s <- c(rel_similarity(na, nb), if (nzchar(stem)) rel_similarity(na, nb, am_norm_label(stem), ""))
  lv[max(match(s, lv), na.rm = TRUE)]
}

#' 比較兩組選項（遺漏代碼不算）：回傳 NA（缺一邊）、TRUE、FALSE；attr "codes_same"
#'
#' 和 rel_values_match() 一樣先去掉遺漏代碼；但選項文字用 am_label_sim() 比，
#' 「午夜12:00」和「午夜12點」這種小差別算一樣（identical／high／medium）。
#' @noRd
am_values_cmp <- function(a, b, miss_codes) {
  if (is.null(a) || is.null(b) || !nrow(a) || !nrow(b)) return(NA)
  a <- a[!rel_is_missing_code(a$code, miss_codes), , drop = FALSE]
  b <- b[!rel_is_missing_code(b$code, miss_codes), , drop = FALSE]
  if (!nrow(a) && !nrow(b)) return(NA)
  if (!setequal(a$code, b$code)) return(structure(FALSE, codes_same = FALSE))
  la <- a$label[match(b$code, a$code)]
  sims <- vapply(seq_along(la), function(k) am_label_sim(la[k], b$label[k]), character(1))
  structure(all(sims %in% c("identical", "high", "medium", "")), codes_same = TRUE)
}

#' .sav 有、題庫的選項組沒有的非遺漏代碼（題庫選項組空白時不算）
#' @noRd
am_codes_added <- function(kit_vals, hv, miss_codes) {
  if (is.null(kit_vals) || !nrow(kit_vals) || is.null(hv) || !nrow(hv)) return(FALSE)
  h <- hv$code[!rel_is_missing_code(hv$code, miss_codes)]
  length(setdiff(h, kit_vals$code)) > 0L
}

#' 一個變項的非遺漏選項（code, label），標籤去掉跳題指示
#'
#' 只有宣告為遺漏（na）或公版遺漏代碼（7777、9999…）的才拿掉；只靠標籤判斷的
#' （例如「3=不適用(如非監護人)」「2=是【請跳答…】」）留著，和題庫的選項組一樣。
#' @noRd
am_header_values <- function(vs, varname) {
  drop <- vs$is_missing == "1" & vs$miss_source %in% c("na", "code")
  s <- vs[vs$varname == varname & !drop, c("code", "label"), drop = FALSE]
  s$label <- trimws(gsub("\\[[^]]*\\]", "", s$label, perl = TRUE))
  rownames(s) <- NULL
  s
}

#' 選項寫成一串 "1=很少; 2=有時"（和 read_sav_meta() 的 vlabs 一樣，parse_vlabs() 讀得回來）
#' @noRd
am_values_text <- function(v) {
  if (is.null(v) || !nrow(v)) return("")
  paste0(v$code, "=", v$label, collapse = "; ")
}

#' 選項比對的白話
#' @noRd
am_values_word <- function(vm) {
  if (is.na(vm)) return("unknown")
  if (isTRUE(vm)) return("same")
  if (identical(attr(vm, "codes_same"), FALSE)) "codes_differ" else "labels_differ"
}

#' sav_meta 整理成 datasetID → 表頭的 list
#' @noRd
am_meta_list <- function(sav_meta) {
  if (is.data.frame(sav_meta)) {
    if (!"datasetID" %in% names(sav_meta)) {
      stop("sav_meta must be a named list (datasetID -> header) or have a datasetID column",
           call. = FALSE)
    }
    sav_meta <- split(sav_meta[, setdiff(names(sav_meta), "datasetID"), drop = FALSE],
                      sav_meta$datasetID)
  }
  if (!length(sav_meta) || is.null(names(sav_meta)) || any(!nzchar(names(sav_meta)))) {
    stop("sav_meta must be a named list: datasetID -> read_sav_meta() output", call. = FALSE)
  }
  need <- c("var", "class", "label", "n_vlab", "vlabs", "na")
  lapply(sav_meta, function(m) {
    m <- as.data.frame(m, stringsAsFactors = FALSE)
    for (cc in setdiff(need, names(m))) m[[cc]] <- rep("", nrow(m))
    m <- m[, need, drop = FALSE]
    m[] <- lapply(m, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })
    m
  })
}

#' 新施測的 admins 列：給 adminID（M03M048、M36M084G01）就從代號推世代、月齡、年級
#' @noRd
am_admin_row <- function(kit, admin) {
  if (is.data.frame(admin) || is.list(admin)) {
    row <- conform_kit_table(as.data.frame(admin, stringsAsFactors = FALSE), "admins")
    if (nrow(row) != 1L) stop("admin must be one row", call. = FALSE)
  } else {
    id <- as.character(admin)[1]
    m <- regmatches(id, regexec("^(M[0-9]{2})M([0-9]{3})(G([0-9]{2}))?$", id))[[1]]
    if (!length(m)) {
      stop("cannot read cohort and age from adminID '", id,
           "'; give admin as a one-row data frame instead", call. = FALSE)
    }
    coh <- kit$cohorts$cohortID[kit$cohorts$code == m[2]]
    if (length(coh) != 1L) stop("no cohort with code ", m[2], " in the kit", call. = FALSE)
    same_coh <- kit$admins[kit$admins$cohortID == coh, , drop = FALSE]
    wn <- suppressWarnings(as.integer(same_coh$waveNo))
    age <- as.integer(m[3])
    before <- suppressWarnings(as.integer(same_coh$ageMonth)) < age
    wave <- if (any(before & !is.na(wn))) max(wn[before & !is.na(wn)]) + 1L else ""
    row <- conform_kit_table(data.frame(
      adminID = id, cohortID = coh, ageMonth = as.character(age),
      grade = if (nzchar(m[5])) as.character(as.integer(m[5])) else "",
      waveNo = as.character(wave), status = "internal", stringsAsFactors = FALSE), "admins")
  }
  if (row$adminID %in% kit$admins$adminID) {
    # 原文：題庫裡已經有 | ，不能再併入一次。新施測要用題庫裡還沒有的代號。
    stop("\u984c\u5eab\u88e1\u5df2\u7d93\u6709 ", row$adminID,
         "\uff0c\u4e0d\u80fd\u518d\u4f75\u5165\u4e00\u6b21\u3002\u65b0\u65bd\u6e2c\u8981\u7528\u984c\u5eab\u88e1\u9084\u6c92\u6709\u7684\u4ee3\u865f\u3002",
         call. = FALSE)
  }
  row
}

#' 每個新資料檔的「上一次」資料檔
#' @noRd
am_prev_dataset <- function(kit, datasetID, admin_row) {
  k2 <- kit
  k2$admins <- rbind(kit$admins, admin_row)
  ord <- rel_admin_order(k2)
  me <- ord[[admin_row$adminID]]
  ds <- kit$datasets
  ds <- ds[am_suffix(ds$datasetID) == am_suffix(datasetID), , drop = FALSE]
  ds$ord <- unname(ord[ds$adminID])
  ds$coh <- kit$admins$cohortID[match(ds$adminID, kit$admins$adminID)]
  ds <- ds[!is.na(ds$ord) & ds$ord < me, , drop = FALSE]
  if (!nrow(ds)) return(NULL)
  same <- ds[ds$coh == admin_row$cohortID, , drop = FALSE]
  if (nrow(same)) ds <- same
  ds[which.max(ds$ord), names(kit$datasets), drop = FALSE]
}

#' 新施測的 datasets 列：沿用上一次的檔名、題本、填答者、說明；可用 datasets 參數改
#' @noRd
am_dataset_rows <- function(kit, ids, admin_row, datasets = NULL) {
  rows <- lapply(ids, function(id) {
    p <- am_prev_dataset(kit, id, admin_row)
    data.frame(datasetID = id, adminID = admin_row$adminID,
               file = if (is.null(p)) "" else p$file,
               instrument = am_instrument(am_suffix(id)),
               formID = if (is.null(p)) "" else p$formID,
               respondent = if (is.null(p)) "" else p$respondent,
               missingScheme = if (is.null(p)) "" else p$missingScheme,
               note = if (is.null(p)) "" else p$note,
               prev = if (is.null(p)) "" else p$datasetID,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  # 題本：上一次的題本不適用這個月齡（例如 72 月齡 → 小一）就換成月齡範圍包含它的題本
  fm <- kit$forms
  age <- suppressWarnings(as.integer(admin_row$ageMonth))
  if (!is.null(fm) && nrow(fm) && !is.na(age)) {
    lo <- suppressWarnings(as.integer(fm$ageFrom))
    hi <- suppressWarnings(as.integer(fm$ageTo))
    fits <- !is.na(lo) & !is.na(hi) & lo <= age & age <= hi
    for (i in seq_len(nrow(out))) {
      k <- match(out$formID[i], fm$formID)
      if (!is.na(k) && (is.na(lo[k]) || is.na(hi[k]) || fits[k])) next
      cand <- fm$formID[fits & fm$instrument == out$instrument[i]]
      if (length(cand) == 1L) out$formID[i] <- cand
    }
  }
  if (!is.null(datasets) && nrow(datasets)) {
    datasets <- as.data.frame(datasets, stringsAsFactors = FALSE)
    for (cc in intersect(c(names(KIT_SCHEMA$datasets$cols), "prev"), names(datasets))) {
      if (cc %in% c("datasetID", "adminID")) next
      v <- as.character(datasets[[cc]][match(out$datasetID, datasets$datasetID)])
      use <- !is.na(v) & nzchar(v)
      out[[cc]][use] <- v[use]
    }
  }
  if (any(!nzchar(out$file))) out$file[!nzchar(out$file)] <- out$datasetID[!nzchar(out$file)]
  bad <- out$datasetID[!nzchar(out$instrument)]
  if (length(bad)) stop("cannot tell the instrument of ", paste(bad, collapse = ", "),
                        "; give it in `datasets`", call. = FALSE)
  out
}

#' 一個資料檔的遺漏代碼：建議用哪一套方案
#' @return list(codes, scheme, candidates)
#' @noRd
am_scheme_guess <- function(kit, vs, prev_scheme) {
  ms <- kit$missing_schemes
  known <- unique(c(ms$code, names(SAV_MISS_CODES)))
  codes <- sort(unique(vs$code[vs$is_missing == "1" & vs$code %in% known]))
  sets <- split(ms$code, ms$scheme)
  if (!length(codes)) return(list(codes = codes, scheme = prev_scheme, candidates = character(0)))
  if (nzchar(prev_scheme) && prev_scheme %in% names(sets) && all(codes %in% sets[[prev_scheme]])) {
    return(list(codes = codes, scheme = prev_scheme, candidates = prev_scheme))
  }
  ok <- names(sets)[vapply(sets, function(s) all(codes %in% s), logical(1))]
  if (!length(ok)) return(list(codes = codes, scheme = "", candidates = character(0)))
  extra <- vapply(sets[ok], function(s) length(setdiff(s, codes)), integer(1))
  best <- ok[extra == min(extra)]
  # 一樣好時取最新的方案（名稱是年份）
  list(codes = codes, scheme = sort(best, decreasing = TRUE)[1], candidates = best)
}

#' 一列提案
#' @noRd
am_row <- function(class, datasetID = "", varname = "", order = "", position = "", kind = "",
                   item_uid = "", itemVer = "", from_uid = "", from_ver = "", block = "",
                   similarity = "", values_match = "", action = "", scheme = "",
                   prev_dataset = "", label = "", values = "", confidence = "", reason = "") {
  data.frame(class = class, action = action, datasetID = datasetID, varname = varname,
             order = as.character(order), position = position, kind = kind,
             item_uid = item_uid, itemVer = as.character(itemVer), from_uid = from_uid,
             from_ver = as.character(from_ver), block = block, similarity = similarity,
             values_match = values_match, scheme = scheme, prev_dataset = prev_dataset,
             label = label, values = values, confidence = confidence, reason = reason,
             stringsAsFactors = FALSE)
}

# 提案 ----

#' Propose how a new administration's data files enter the kit
#'
#' Reads only the header metadata of the new wave's data files (variable names, labels,
#' value labels; the shape [read_sav_meta()] returns) and classifies every variable.
#' Nothing is written: every row is a proposal with a `confidence` (high / medium / low)
#' and a plain-Chinese `reason`. Accept or reject rows, then call [apply_admin_merge()].
#'
#' Classes: `same`, `new_version`, `renamed`, `new_item`, `not_item` (system, weight and
#' derived variables), `dropped_or_block` (items of the previous administration's same
#' data file that are gone; `action` is `block_none`, `block_subset`, `deprecate` or
#' `not_asked`) and `missing_scheme_change` (one row per data file whose missing codes do
#' not fit the previous scheme; `scheme` is the proposed scheme).
#'
#' @param kit A kit (17 tables), e.g. from [read_kit()].
#' @param sav_meta Named list `datasetID -> read_sav_meta()` output (e.g.
#'   `list("M03M048-P" = meta1, "M03M048-T" = meta3)`), or one data frame with a
#'   `datasetID` column.
#' @param admin The new administration: an adminID such as `"M03M048"` or
#'   `"M36M084G01"` (cohort, age and grade are read from it), or a one-row data frame
#'   in the `admins` schema.
#' @param datasets Optional data frame overriding the new `datasets` rows by `datasetID`
#'   (`file`, `instrument`, `formID`, `respondent`, `missingScheme`, `note`, and `prev`,
#'   the datasetID of the data file to compare with). By default they copy the previous
#'   administration's same data file (`-P`, `-PK`, `-T`, `-S`, `-C`).
#' @param crosswalk Optional data frame with `varname` and any of `datasetID`,
#'   `position`, `item_uid`. `position` fills `dataset_vars$position` (else it is read
#'   from the label prefix, e.g. "四.33."); `item_uid` says which existing item the
#'   variable is.
#' @param prev_meta Optional headers of the previous administration's data files, same
#'   shape as `sav_meta`, named by the previous `datasetID` (e.g. `"M03M036-P"`) or by
#'   the new one. Strongly recommended: versions are then decided by comparing .sav
#'   header with .sav header (same wording style). Without it the header is compared
#'   with the kit's item text (written differently, e.g. from the 0.2 bank), so only
#'   clear changes become `new_version` and confidence is lower.
#' @return A data frame of proposals: `proposal_id`, `class`, `action`, `datasetID`,
#'   `varname`, `order`, `position`, `kind`, `item_uid`, `itemVer`, `from_uid`,
#'   `from_ver`, `block`, `similarity`, `values_match`, `scheme`, `prev_dataset`, `label`
#'   (header label), `values` (non-missing value labels, `"1=a; 2=b"`), `confidence`,
#'   `reason`. Attributes `admin` and `datasets` hold the new rows for those tables.
#' @seealso [apply_admin_merge()]
#' @export
propose_admin_merge <- function(kit, sav_meta, admin, datasets = NULL, crosswalk = NULL,
                                prev_meta = NULL) {
  metas <- am_meta_list(sav_meta)
  prevs <- if (length(prev_meta)) am_meta_list(prev_meta) else list()
  admin_row <- am_admin_row(kit, admin)
  ds_new <- am_dataset_rows(kit, names(metas), admin_row, datasets)
  it <- kit$items
  iv <- kit$item_versions
  dv <- kit$dataset_vars
  miss_codes <- unique(c(kit$missing_schemes$code, names(SAV_MISS_CODES)))
  k2 <- kit
  k2$admins <- rbind(kit$admins, admin_row)
  aord <- rel_admin_order(k2)
  me <- aord[[admin_row$adminID]]
  dv_ord <- unname(aord[kit$datasets$adminID[match(dv$datasetID, kit$datasets$datasetID)]])

  ver_key <- paste(iv$item_uid, iv$itemVer, sep = "|")
  vs_rows <- split(kit$value_codes[, c("code", "label")], kit$value_codes$valueSetID)
  vs_of <- function(uid, ver) {
    id <- iv$valueSetID[match(paste(uid, ver, sep = "|"), ver_key)]
    if (is.na(id) || !nzchar(id)) NULL else vs_rows[[id]]
  }
  label_of <- function(uid, ver) {
    x <- iv$label[match(paste(uid, ver, sep = "|"), ver_key)]
    if (is.na(x)) "" else x
  }
  stem_of <- function(uid, ver) {
    x <- iv$stem[match(paste(uid, ver, sep = "|"), ver_key)]
    if (is.na(x)) "" else x
  }
  versions_of <- function(uid) sort(as.integer(iv$itemVer[iv$item_uid == uid]))
  # 參考版本，依序找：
  #   1. 上一次的資料檔用的版本（題本沒換時）
  #   2. 同世代、同題本、時間最近（前後都算）的資料檔
  #   3. 別的世代、同題本、同月齡的資料檔
  #   4. 同題本、時間最近的資料檔
  #   5. 時間在這次之前最近的資料檔；6. 最新的版本
  dv_ds <- match(dv$datasetID, kit$datasets$datasetID)
  dv_form <- kit$datasets$formID[dv_ds]
  dv_adm <- match(kit$datasets$adminID[dv_ds], kit$admins$adminID)
  dv_coh <- kit$admins$cohortID[dv_adm]
  dv_age <- kit$admins$ageMonth[dv_adm]
  ref_ver <- function(uid, prev, form = "") {
    w <- which(dv$item_uid == uid & nzchar(dv$itemVer))
    if (!length(w)) {
      v <- versions_of(uid)
      return(if (length(v)) max(v) else NA_integer_)
    }
    ver <- as.integer(dv$itemVer[w])
    o <- dv_ord[w]
    near <- function(k) ver[k][which.min(abs(o[k] - me))]
    # 題本換了（例如 P-36-72 → P-G1-G3）就不直接沿用上一次的版本
    prev_form <- kit$datasets$formID[match(prev, kit$datasets$datasetID)]
    same_form <- !nzchar(form) || is.na(prev_form) || identical(prev_form, form)
    hit <- dv$datasetID[w] == prev
    if (same_form && any(hit)) return(ver[hit][1])
    if (nzchar(form)) {
      f <- dv_form[w] %in% form & !is.na(o)
      k <- f & dv_coh[w] %in% admin_row$cohortID
      if (any(k)) return(near(k))
      k <- f & dv_age[w] %in% admin_row$ageMonth
      if (any(k)) return(ver[k][1])
      if (any(f)) return(near(f))
    }
    k <- !is.na(o) & o < me
    if (any(k)) return(ver[k][which.max(o[k])])
    max(versions_of(uid))
  }
  # 和題庫的一個版本比（題庫文字和 .sav 寫法不同：放寬，只看清楚的改變）
  compare_ver <- function(uid, ver, lab, hv) {
    sim <- am_label_sim(label_of(uid, ver), lab, stem_of(uid, ver))
    vm <- rel_values_match(vs_of(uid, ver), hv, miss_codes)
    ok <- (sim %in% c("identical", "high", "medium", "")) &&
      !am_codes_added(vs_of(uid, ver), hv, miss_codes)
    list(sim = sim, vm = vm, ok = ok)
  }
  # 和上一次的 .sav 表頭比（同樣的寫法：照嚴格的規則）
  compare_head <- function(plab, pvals, lab, hv) {
    sim <- am_label_sim(plab, lab)
    vm <- am_values_cmp(pvals, hv, miss_codes)
    ok <- (sim %in% c("identical", "high", "")) && !isFALSE(as.logical(vm))
    list(sim = sim, vm = vm, ok = ok)
  }

  cw <- NULL
  if (!is.null(crosswalk) && nrow(crosswalk)) {
    cw <- as.data.frame(crosswalk, stringsAsFactors = FALSE)
    for (cc in setdiff(c("datasetID", "varname", "position", "item_uid"), names(cw))) {
      cw[[cc]] <- rep("", nrow(cw))
    }
    cw[] <- lapply(cw, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })
  }
  cw_get <- function(id, var, col) {
    if (is.null(cw)) return("")
    hit <- which(tolower(cw$varname) == tolower(var) & (cw$datasetID == id | !nzchar(cw$datasetID)))
    if (!length(hit)) return("")
    hit <- hit[order(!nzchar(cw$datasetID[hit]))]
    cw[[col]][hit[1]]
  }

  out <- list()
  add <- function(x) out[[length(out) + 1L]] <<- x
  # 這次新增的版本／題目：別的資料檔（例如 -P 和 -PK）有同一題時用同一個
  new_vers <- list()   # uid -> data.frame(ver, label, values)
  new_uids <- character(0)

  for (id in names(metas)) {
    meta <- metas[[id]]
    d <- ds_new[ds_new$datasetID == id, , drop = FALSE]
    ins <- d$instrument
    prev <- d$prev
    pf <- kit$datasets$formID[match(prev, kit$datasets$datasetID)]
    form_changed <- nzchar(d$formID) && !is.na(pf) && nzchar(pf) && pf != d$formID
    vs <- sav_value_sets(meta)
    uid <- match_item_uid(meta$var, ins, it)
    # 對照表指定的題目
    for (i in seq_len(nrow(meta))) {
      u <- cw_get(id, meta$var[i], "item_uid")
      if (nzchar(u) && u %in% it$item_uid &&
          tolower(sub("^[^.]*\\.", "", u)) == tolower(meta$var[i])) uid[i] <- u
    }
    kind <- sav_var_kind(meta, ins, uid, it)
    # 上一次同名變項的種類比規則可靠
    pdv <- dv[dv$datasetID == prev, , drop = FALSE]
    pk <- pdv$kind[match(tolower(meta$var), tolower(pdv$varname))]
    kind[!is.na(pk)] <- pk[!is.na(pk)]
    # 位置：對照表；沒有就從標籤開頭抓
    tmp <- data.frame(datasetID = id, varname = meta$var, stringsAsFactors = FALSE)
    one_line <- meta
    one_line$label <- gsub("[\r\n]+", " ", one_line$label)
    pos_sav <- cw_sav_positions(stats::setNames(list(one_line), id), list(dataset_vars = tmp))
    pos <- pos_sav$position[match(meta$var, pos_sav$varname)]
    pos[is.na(pos)] <- ""
    for (i in seq_len(nrow(meta))) {
      p <- cw_get(id, meta$var[i], "position")
      if (nzchar(p)) pos[i] <- p
    }
    lab_raw <- meta$label
    lab <- am_clean_label(lab_raw)
    hv <- lapply(meta$var, function(v) am_header_values(vs, v))
    vtxt <- vapply(hv, am_values_text, character(1))
    blk_of <- function(u) {
      b <- it$block[match(u, it$item_uid)]
      if (is.na(b)) "" else b
    }
    # 上一次的 .sav 表頭（有給才有）
    ph <- prevs[[prev]] %||% prevs[[id]]
    pvs <- if (!is.null(ph)) sav_value_sets(ph) else NULL
    # 上一次這個變項的標籤和選項：有表頭用表頭（src = "header"），沒有用題庫第 ver 版
    prev_of <- function(varname, u, ver) {
      k <- if (is.null(ph)) NA else match(tolower(varname), tolower(ph$var))
      if (!is.na(k)) {
        return(list(src = "header", label = ph$label[k], values = am_header_values(pvs, ph$var[k])))
      }
      list(src = "kit", label = label_of(u, ver), values = vs_of(u, ver), stem = stem_of(u, ver))
    }
    cmp_prev <- function(pi, lab_i, hv_i) {
      if (pi$src == "header") compare_head(pi$label, pi$values, lab_i, hv_i) else {
        sim <- am_label_sim(pi$label, lab_i, pi$stem)
        vm <- rel_values_match(pi$values, hv_i, miss_codes)
        # 題庫的選項組常比 .sav 多幾個代碼（0.2 題庫的寫法）：只有 .sav 多出題庫沒有的
        # 選項代碼（加了選項）才算選項改了
        added <- am_codes_added(pi$values, hv_i, miss_codes)
        list(sim = sim, vm = if (added) structure(FALSE, codes_same = FALSE) else vm,
             ok = (sim %in% c("identical", "high", "medium", "")) && !added)
      }
    }
    base <- function(i, ...) am_row(datasetID = id, varname = meta$var[i], order = i,
                                    position = pos[i], kind = kind[i], prev_dataset = prev,
                                    label = lab_raw[i], values = vtxt[i], ...)
    matched <- character(0)
    pending <- integer(0)   # 對不到題目的題目變項

    # 1. 對到題目的：比版本 ----
    for (i in seq_len(nrow(meta))) {
      u <- uid[i]
      if (!nzchar(u)) {
        if (kind[i] == "item") pending <- c(pending, i) else
          add(base(i, class = "not_item", confidence = "high",
                   # 原文：「 | 」是
                   reason = paste0("\u300c", meta$var[i], "\u300d\u662f", CB_KIND_LABEL[[kind[i]]],
                                   # 原文：變項，不是題目；照樣列進資料檔。
                                   "\u8b8a\u9805\uff0c\u4e0d\u662f\u984c\u76ee\uff1b\u7167\u6a23\u5217\u9032\u8cc7\u6599\u6a94\u3002")))
        next
      }
      matched <- c(matched, u)
      r <- ref_ver(u, prev, d$formID)
      if (is.na(r)) {
        add(base(i, class = "new_version", item_uid = u, itemVer = "1", block = blk_of(u),
                 confidence = "low",
                 # 原文：題庫有這一題，但還沒有任何版本；建一個第 1 版。
                 reason = "\u984c\u5eab\u6709\u9019\u4e00\u984c\uff0c\u4f46\u9084\u6c92\u6709\u4efb\u4f55\u7248\u672c\uff1b\u5efa\u4e00\u500b\u7b2c 1 \u7248\u3002"))
        next
      }
      pi <- prev_of(meta$var[i], u, r)
      by_head <- pi$src == "header"
      # 原文：和上一次（ | ）的 .sav 表頭比
      basis <- if (by_head) paste0("\u548c\u4e0a\u4e00\u6b21\uff08", prev, "\uff09\u7684 .sav \u8868\u982d\u6bd4") else
        # 原文：和題庫第  |  版的文字比（沒有上一次的表頭，比較不準）
        paste0("\u548c\u984c\u5eab\u7b2c ", r, " \u7248\u7684\u6587\u5b57\u6bd4\uff08\u6c92\u6709\u4e0a\u4e00\u6b21\u7684\u8868\u982d\uff0c\u6bd4\u8f03\u4e0d\u6e96\uff09")
      # 原文：（題本換成  | ，版本參考同題本的其他施測）
      if (form_changed) basis <- paste0(basis, "\uff08\u984c\u672c\u63db\u6210 ", d$formID, "\uff0c\u7248\u672c\u53c3\u8003\u540c\u984c\u672c\u7684\u5176\u4ed6\u65bd\u6e2c\uff09")
      cr <- cmp_prev(pi, lab_raw[i], hv[[i]])
      # 系統、加權、衍生變項：標籤寫法常換，不另建版本
      if (!cr$ok && kind[i] != "item") {
        add(base(i, class = "same", item_uid = u, itemVer = r, block = blk_of(u),
                 similarity = cr$sim, values_match = am_values_word(cr$vm), confidence = "medium",
                 # 原文：「 | 」是
                 reason = paste0("\u300c", meta$var[i], "\u300d\u662f", CB_KIND_LABEL[[kind[i]]],
                                 # 原文：變項，標籤寫法不同也用第  |  版。
                                 "\u8b8a\u9805\uff0c\u6a19\u7c64\u5beb\u6cd5\u4e0d\u540c\u4e5f\u7528\u7b2c ", r, " \u7248\u3002")))
        next
      }
      if (cr$ok) {
        strong <- cr$sim == "identical" && !isFALSE(as.logical(cr$vm))
        conf <- if (by_head) {
          if (strong || (cr$sim == "high" && isTRUE(cr$vm))) "high" else "medium"
        } else {
          if (strong) "medium" else "low"
        }
        why <- if (identical(attr(cr$vm, "codes_same"), FALSE)) {
          # 原文：標籤幾乎一樣，但選項代碼和題庫不同（題庫的選項組可能寫法不同，請看一下）
          "\u6a19\u7c64\u5e7e\u4e4e\u4e00\u6a23\uff0c\u4f46\u9078\u9805\u4ee3\u78bc\u548c\u984c\u5eab\u4e0d\u540c\uff08\u984c\u5eab\u7684\u9078\u9805\u7d44\u53ef\u80fd\u5beb\u6cd5\u4e0d\u540c\uff0c\u8acb\u770b\u4e00\u4e0b\uff09"
        # 原文：標籤和選項都一樣
        } else if (strong) "\u6a19\u7c64\u548c\u9078\u9805\u90fd\u4e00\u6a23" else
          # 原文：沒有標籤可比，選項沒有不同
          if (cr$sim == "") "\u6c92\u6709\u6a19\u7c64\u53ef\u6bd4\uff0c\u9078\u9805\u6c92\u6709\u4e0d\u540c" else
            # 原文：標籤幾乎一樣，沒有選項可比 | 標籤幾乎一樣，選項代碼一樣
            if (is.na(cr$vm)) "\u6a19\u7c64\u5e7e\u4e4e\u4e00\u6a23\uff0c\u6c92\u6709\u9078\u9805\u53ef\u6bd4" else "\u6a19\u7c64\u5e7e\u4e4e\u4e00\u6a23\uff0c\u9078\u9805\u4ee3\u78bc\u4e00\u6a23"
        add(base(i, class = "same", item_uid = u, itemVer = r, block = blk_of(u),
                 similarity = cr$sim, values_match = am_values_word(cr$vm), confidence = conf,
                 # 原文：： | ；用第  |  版（遺漏代碼不算）。
                 reason = paste0(basis, "\uff1a", why, "\uff1b\u7528\u7b2c ", r, " \u7248\uff08\u907a\u6f0f\u4ee3\u78bc\u4e0d\u7b97\uff09\u3002")))
        next
      }
      # 題庫裡已經有的別的版本（例如之後的施測先建好的）：題目文字幾乎一樣、選項代碼一樣就用它
      others <- setdiff(rev(versions_of(u)), r)
      hit <- NULL
      for (v in others) {
        c2 <- compare_ver(u, v, lab_raw[i], hv[[i]])
        if (c2$ok && (!by_head || c2$sim %in% c("identical", "high"))) { hit <- c(v = v, c2); break }
      }
      if (!is.null(hit)) {
        add(base(i, class = "same", item_uid = u, itemVer = hit$v, block = blk_of(u),
                 similarity = hit$sim, values_match = am_values_word(hit$vm),
                 confidence = "medium",
                 # 原文：有改；題庫已經有的第 
                 reason = paste0(basis, "\u6709\u6539\uff1b\u984c\u5eab\u5df2\u7d93\u6709\u7684\u7b2c ", hit$v,
                                 # 原文： 版文字幾乎一樣、選項代碼一樣，建議用第 
                                 " \u7248\u6587\u5b57\u5e7e\u4e4e\u4e00\u6a23\u3001\u9078\u9805\u4ee3\u78bc\u4e00\u6a23\uff0c\u5efa\u8b70\u7528\u7b2c ", hit$v,
                                 # 原文： 版（上一次用第  |  版）。
                                 " \u7248\uff08\u4e0a\u4e00\u6b21\u7528\u7b2c ", r, " \u7248\uff09\u3002")))
        next
      }
      # 新版本：這次別的資料檔（例如 -P 和 -PK）已經建了一樣的新版本就用同一個
      nv <- new_vers[[u]]
      same_new <- integer(0)
      if (!is.null(nv)) {
        same_new <- which(vapply(seq_len(nrow(nv)), function(k) {
          am_label_sim(nv$label[k], lab_raw[i]) %in% c("identical", "high") &&
            !isFALSE(as.logical(am_values_cmp(parse_vlabs(nv$values[k]), hv[[i]], miss_codes)))
        }, logical(1)))
      }
      if (length(same_new)) {
        ver <- nv$ver[same_new[1]]
      } else {
        ver <- max(c(versions_of(u), nv$ver)) + 1L
        new_vers[[u]] <- rbind(nv, data.frame(ver = ver, label = lab_raw[i], values = vtxt[i],
                                              stringsAsFactors = FALSE))
      }
      vw <- am_values_word(cr$vm)
      conf <- if (by_head) {
        if (vw == "codes_differ" || cr$sim == "low") "high" else
          if (cr$sim == "medium") "medium" else "low"
      } else {
        if (vw == "codes_differ" && cr$sim %in% c("medium", "low")) "medium" else "low"
      }
      # 原文：題目文字改了
      what <- c(if (cr$sim %in% c("medium", "low")) "\u984c\u76ee\u6587\u5b57\u6539\u4e86",
                # 原文：選項代碼改了（遺漏代碼不算）
                if (vw == "codes_differ") "\u9078\u9805\u4ee3\u78bc\u6539\u4e86\uff08\u907a\u6f0f\u4ee3\u78bc\u4e0d\u7b97\uff09",
                # 原文：選項文字改了
                if (vw == "labels_differ") "\u9078\u9805\u6587\u5b57\u6539\u4e86")
      add(base(i, class = "new_version", item_uid = u, itemVer = ver, from_uid = u,
               from_ver = r, block = blk_of(u), similarity = cr$sim, values_match = vw,
               confidence = conf,
               # 原文：： | 、
               reason = paste0(basis, "\uff1a", paste(what, collapse = "\u3001"),
                               # 原文：；上一次用第  |  版，建第  |  版，記一筆改版關係。
                               "\uff1b\u4e0a\u4e00\u6b21\u7528\u7b2c ", r, " \u7248\uff0c\u5efa\u7b2c ", ver, " \u7248\uff0c\u8a18\u4e00\u7b46\u6539\u7248\u95dc\u4fc2\u3002")))
    }

    # 2. 上一次有、這次不見的題目 ----
    gone <- pdv[pdv$kind == "item" & nzchar(pdv$item_uid) & !pdv$item_uid %in% matched, ,
                drop = FALSE]
    gone <- gone[!duplicated(gone$item_uid), , drop = FALSE]

    # 3. 對不到題目的：換題號或新題 ----
    claimed <- character(0)
    for (i in pending) {
      v <- meta$var[i]
      cand <- NULL
      # a. 對照表指定
      u_cw <- cw_get(id, v, "item_uid")
      if (nzchar(u_cw) && u_cw %in% it$item_uid) {
        cand <- list(uid = u_cw, how = "crosswalk")
      }
      # b. X → Xn
      if (is.null(cand) && grepl("[nN]$", v)) {
        b <- match_item_uid(sub("[nN]$", "", v), ins, it)
        if (nzchar(b) && !b %in% matched) cand <- list(uid = b, how = "xn")
      }
      # c. 上一次有、這次不見的題目裡，標籤和選項一樣的
      if (is.null(cand) && nrow(gone)) {
        left <- gone[!gone$item_uid %in% claimed, , drop = FALSE]
        if (nrow(left)) {
          pis <- lapply(seq_len(nrow(left)), function(j) {
            prev_of(left$varname[j], left$item_uid[j], left$itemVer[j])
          })
          sims <- vapply(pis, function(pi) am_label_sim(pi$label, lab_raw[i], pi$stem %||% ""),
                         character(1))
          vms <- lapply(pis, function(pi) rel_values_match(pi$values, hv[[i]], miss_codes))
          okv <- !vapply(vms, function(x) isFALSE(as.logical(x)), logical(1))
          for (lvl in c("identical", "high")) {
            j <- which(sims == lvl & okv)
            if (length(j)) {
              cand <- list(uid = left$item_uid[j[1]], how = "label", n = length(j))
              break
            }
          }
        }
      }
      new_uid <- paste0(ins, ".", v)
      if (!is.null(cand)) {
        fu <- cand$uid
        fv <- ref_ver(fu, prev, d$formID)
        pi <- prev_of(it$varname[match(fu, it$item_uid)], fu, fv)
        sim <- am_label_sim(pi$label, lab_raw[i], pi$stem %||% "")
        vm <- rel_values_match(pi$values, hv[[i]], miss_codes)
        good <- sim %in% c("identical", "high") && !isFALSE(as.logical(vm))
        conf <- switch(cand$how,
                       crosswalk = "high",
                       xn = if (good) "high" else if (sim %in% c("low", "")) "low" else "medium",
                       label = if (good && sim == "identical" && cand$n == 1L) "medium" else "low")
        how <- switch(cand$how,
                      # 原文：對照表說它是
                      crosswalk = "\u5c0d\u7167\u8868\u8aaa\u5b83\u662f",
                      # 原文：變項名是舊題號加 n：
                      xn = "\u8b8a\u9805\u540d\u662f\u820a\u984c\u865f\u52a0 n\uff1a",
                      # 原文：標籤和選項和上一次有、這次不見的
                      label = "\u6a19\u7c64\u548c\u9078\u9805\u548c\u4e0a\u4e00\u6b21\u6709\u3001\u9019\u6b21\u4e0d\u898b\u7684")
        claimed <- c(claimed, fu)
        matched <- c(matched, fu)
        new_uids <- c(new_uids, new_uid)
        add(base(i, class = "renamed", item_uid = new_uid, itemVer = "1", from_uid = fu,
                 from_ver = fv, block = blk_of(fu), similarity = sim,
                 values_match = am_values_word(vm), confidence = conf,
                 # 原文：「 | 」（第 
                 reason = paste0(how, "\u300c", sub("^[^.]*\\.", "", fu), "\u300d\uff08\u7b2c ", fv,
                                 # 原文： 版） | 一樣
                                 " \u7248\uff09", if (cand$how == "label") "\u4e00\u6a23" else "",
                                 # 原文：；建新題號  | ，記一筆換題號關係。
                                 "\uff1b\u5efa\u65b0\u984c\u865f ", new_uid, "\uff0c\u8a18\u4e00\u7b46\u63db\u984c\u865f\u95dc\u4fc2\u3002")))
        next
      }
      # d. 新題：區塊取檔案裡前面最近一個有題目的變項
      blk <- ""
      prior <- which(nzchar(uid[seq_len(i - 1L)]))
      if (length(prior)) blk <- blk_of(uid[max(prior)])
      new_uids <- c(new_uids, new_uid)
      add(base(i, class = "new_item", item_uid = new_uid, itemVer = "1", block = blk,
               confidence = if (nzchar(lab[i])) "medium" else "low",
               # 原文：題庫裡沒有「 | 」，也對不到舊題；建新題 
               reason = paste0("\u984c\u5eab\u88e1\u6c92\u6709\u300c", v, "\u300d\uff0c\u4e5f\u5c0d\u4e0d\u5230\u820a\u984c\uff1b\u5efa\u65b0\u984c ", new_uid,
                               # 原文：，先放在前一題的區塊「 | 」
                               if (nzchar(blk)) paste0("\uff0c\u5148\u653e\u5728\u524d\u4e00\u984c\u7684\u5340\u584a\u300c", blk, "\u300d") else "",
                               # 原文：。
                               "\u3002")))
    }

    # 4. 不見的題目：整個區塊沒問，或只少幾題 ----
    gone <- gone[!gone$item_uid %in% claimed, , drop = FALSE]
    now_blocks <- unique(vapply(matched, blk_of, character(1)))
    if (nrow(gone)) {
      gone$block <- vapply(gone$item_uid, blk_of, character(1))
      for (b in unique(gone$block[nzchar(gone$block) & !gone$block %in% now_blocks])) {
        g <- gone[gone$block == b, , drop = FALSE]
        add(am_row("dropped_or_block", datasetID = id, block = b, action = "block_none",
                   prev_dataset = prev, confidence = "medium",
                   # 原文：上一次（ | ）區塊「 | 」的 
                   reason = paste0("\u4e0a\u4e00\u6b21\uff08", prev, "\uff09\u5340\u584a\u300c", b, "\u300d\u7684 ", nrow(g),
                                   # 原文： 題這次全都沒有；記成這個區塊沒問（block_status none）。
                                   " \u984c\u9019\u6b21\u5168\u90fd\u6c92\u6709\uff1b\u8a18\u6210\u9019\u500b\u5340\u584a\u6c92\u554f\uff08block_status none\uff09\u3002")))
      }
      part <- gone[!nzchar(gone$block) | gone$block %in% now_blocks, , drop = FALSE]
      for (j in seq_len(nrow(part))) {
        u <- part$item_uid[j]
        later <- any(dv$item_uid == u & !is.na(dv_ord) & dv_ord > me)
        add(am_row("dropped_or_block", datasetID = id, varname = part$varname[j],
                   item_uid = u, itemVer = part$itemVer[j], block = part$block[j],
                   action = if (later) "not_asked" else "deprecate", prev_dataset = prev,
                   confidence = if (later) "medium" else "low",
                   reason = if (later) {
                     # 原文：上一次（ | ）有、這次沒有，但之後的施測還有用；只記下，不改題庫。
                     paste0("\u4e0a\u4e00\u6b21\uff08", prev, "\uff09\u6709\u3001\u9019\u6b21\u6c92\u6709\uff0c\u4f46\u4e4b\u5f8c\u7684\u65bd\u6e2c\u9084\u6709\u7528\uff1b\u53ea\u8a18\u4e0b\uff0c\u4e0d\u6539\u984c\u5eab\u3002")
                   } else {
                     # 原文：上一次（ | ）有、這次沒有，之後也沒有再用；建議標成停用（deprecated）。
                     paste0("\u4e0a\u4e00\u6b21\uff08", prev, "\uff09\u6709\u3001\u9019\u6b21\u6c92\u6709\uff0c\u4e4b\u5f8c\u4e5f\u6c92\u6709\u518d\u7528\uff1b\u5efa\u8b70\u6a19\u6210\u505c\u7528\uff08deprecated\uff09\u3002")
                   }))
      }
    }
    # 上一次的區塊狀態：還是沒問或只問一部分就沿用
    pbs <- kit$block_status[kit$block_status$datasetID == prev &
                              kit$block_status$asked %in% c("none", "subset"), , drop = FALSE]
    gone_blocks <- if (nrow(gone)) unique(gone$block[!gone$block %in% now_blocks]) else character(0)
    for (j in seq_len(nrow(pbs))) {
      b <- pbs$block[j]
      present <- b %in% now_blocks
      if (b %in% gone_blocks) next
      keep <- (pbs$asked[j] == "none" && !present) || (pbs$asked[j] == "subset" && present)
      if (!keep) next
      add(am_row("dropped_or_block", datasetID = id, block = b,
                 action = paste0("block_", pbs$asked[j]), prev_dataset = prev,
                 confidence = "high",
                 # 原文：沿用上一次（ | ）的區塊狀態：
                 reason = paste0("\u6cbf\u7528\u4e0a\u4e00\u6b21\uff08", prev, "\uff09\u7684\u5340\u584a\u72c0\u614b\uff1a",
                                 # 原文：沒問 | 只問一部分
                                 if (pbs$asked[j] == "none") "\u6c92\u554f" else "\u53ea\u554f\u4e00\u90e8\u5206",
                                 # 原文：（ | ）
                                 if (nzchar(pbs$who[j])) paste0("\uff08", pbs$who[j], "\uff09") else "",
                                 # 原文：。
                                 "\u3002")))
    }

    # 5. 遺漏代碼方案 ----
    prev_scheme <- d$missingScheme
    g <- am_scheme_guess(kit, vs, prev_scheme)
    if (!identical(g$scheme, prev_scheme) && length(g$codes)) {
      extra <- setdiff(g$codes, kit$missing_schemes$code[kit$missing_schemes$scheme == prev_scheme])
      add(am_row("missing_scheme_change", datasetID = id, scheme = g$scheme,
                 prev_dataset = prev,
                 values = paste(g$codes, collapse = ","),
                 confidence = if (!nzchar(g$scheme)) "low" else
                   if (length(g$candidates) == 1L) "high" else "medium",
                 reason = paste0(
                   # 原文：這個檔的遺漏代碼有  | 、
                   "\u9019\u500b\u6a94\u7684\u907a\u6f0f\u4ee3\u78bc\u6709 ", paste(extra, collapse = "\u3001"),
                   # 原文：，上一次用的  |  方案沒有
                   if (nzchar(prev_scheme)) paste0("\uff0c\u4e0a\u4e00\u6b21\u7528\u7684 ", prev_scheme, " \u65b9\u6848\u6c92\u6709") else "",
                   # 原文：；建議改用  |  方案（
                   if (nzchar(g$scheme)) paste0("\uff1b\u5efa\u8b70\u6539\u7528 ", g$scheme, " \u65b9\u6848\uff08",
                                                 # 原文：、
                                                 cb_scheme_text(kit, g$scheme, sep = "\u3001", eq = " "),
                                                 # 原文：）。
                                                 "\uff09\u3002")
                   # 原文：；題庫裡沒有一套方案包含全部代碼，請先在 missing_schemes 加一套。
                   else "\uff1b\u984c\u5eab\u88e1\u6c92\u6709\u4e00\u5957\u65b9\u6848\u5305\u542b\u5168\u90e8\u4ee3\u78bc\uff0c\u8acb\u5148\u5728 missing_schemes \u52a0\u4e00\u5957\u3002")))
    }
  }

  res <- do.call(rbind, out)
  if (is.null(res)) res <- am_row("same")[0, , drop = FALSE]
  res <- cbind(proposal_id = sprintf("p%04d", seq_len(nrow(res))), res, stringsAsFactors = FALSE)
  rownames(res) <- NULL
  attr(res, "admin") <- admin_row
  # 每個資料檔是和上一次的 .sav 表頭比（header），還是和題庫文字比（kit）
  ds_new$compared_with <- ifelse(ds_new$prev %in% names(prevs) | ds_new$datasetID %in% names(prevs),
                                 "header", "kit")
  attr(res, "datasets") <- ds_new
  res
}

# 套用 ----

#' 選項組：找題庫裡一樣的（先試 prefer），沒有就建新的
#' @noRd
am_value_set <- function(st, values, prefer = "") {
  v <- parse_vlabs(values)
  if (!nrow(v)) return(prefer %||% "")
  rows <- split(st$value_codes[, c("code", "label")], st$value_codes$valueSetID)
  same <- function(r) !is.null(r) && nrow(r) == nrow(v) &&
    isTRUE(rel_values_match(r, v, character(0)))
  if (nzchar(prefer) && same(rows[[prefer]])) return(prefer)
  hit <- names(rows)[vapply(rows, same, logical(1))]
  if (length(hit)) return(hit[1])
  n <- suppressWarnings(max(as.integer(sub("^vs", "", names(rows))), na.rm = TRUE))
  if (!is.finite(n)) n <- 0L
  id <- sprintf("vs%04d", n + 1L)
  st$value_codes <- rbind(st$value_codes, conform_kit_table(data.frame(
    valueSetID = id, code = v$code, label = v$label, order = as.character(seq_len(nrow(v))),
    stringsAsFactors = FALSE), "value_codes"))
  id
}

#' Apply accepted admin-merge proposals to a kit
#'
#' Adds the new administration and its data files, then for each proposal:
#' accepted `same` links the variable to that item version; `new_version` adds an
#' `item_versions` row and a `revised` relation; `renamed` adds the new item (copied from
#' its predecessor, with its group memberships in this form), version 1 and a `renamed`
#' relation; `new_item` adds the item and version 1; `dropped_or_block` adds
#' `block_status` rows (`block_none`, `block_subset`) or marks items deprecated
#' (`deprecate`); `missing_scheme_change` sets the data file's `missingScheme`.
#' A rejected variable proposal still lists the variable in `dataset_vars`, but without
#' an item (to be sorted out later); other rejected proposals change nothing.
#' Value sets are reused when the same codes and labels already exist.
#'
#' @param kit A kit list.
#' @param proposals Output of [propose_admin_merge()] (rows may be edited, e.g. the
#'   `block` of a new item, or filtered).
#' @param accept Which proposals to accept: `TRUE` (all), a logical vector, or a
#'   character vector of `proposal_id`s, classes and/or confidence levels (a row is
#'   accepted when any of its id, class or confidence is listed).
#' @param admin,datasets The new `admins` row and `datasets` rows; default to the
#'   attributes [propose_admin_merge()] put on `proposals`.
#' @return The kit with the new rows added (run [check_kit()] on it).
#' @export
apply_admin_merge <- function(kit, proposals, accept = TRUE,
                              admin = attr(proposals, "admin"),
                              datasets = attr(proposals, "datasets")) {
  if (is.null(admin) || is.null(datasets)) {
    stop("admin and datasets are missing: pass them, or use proposals straight from ",
         "propose_admin_merge()", call. = FALSE)
  }
  p <- as.data.frame(proposals, stringsAsFactors = FALSE)
  p[] <- lapply(p, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })
  acc <- if (is.logical(accept)) rep_len(accept, nrow(p)) else
    p$proposal_id %in% accept | p$class %in% accept | p$confidence %in% accept
  acc[is.na(acc)] <- FALSE
  admin <- conform_kit_table(admin, "admins")
  if (admin$adminID %in% kit$admins$adminID) {
    # 原文：題庫裡已經有 | ，不能再併入一次。新施測要用題庫裡還沒有的代號。
    stop("\u984c\u5eab\u88e1\u5df2\u7d93\u6709 ", admin$adminID,
         "\uff0c\u4e0d\u80fd\u518d\u4f75\u5165\u4e00\u6b21\u3002\u65b0\u65bd\u6e2c\u8981\u7528\u984c\u5eab\u88e1\u9084\u6c92\u6709\u7684\u4ee3\u865f\u3002",
         call. = FALSE)
  }
  ds <- as.data.frame(datasets, stringsAsFactors = FALSE)
  sch <- p[acc & p$class == "missing_scheme_change" & nzchar(p$scheme), , drop = FALSE]
  k <- match(sch$datasetID, ds$datasetID)
  ds$missingScheme[k[!is.na(k)]] <- sch$scheme[!is.na(k)]

  st <- new.env()
  st$value_codes <- kit$value_codes
  kit$admins <- rbind(kit$admins, admin)
  kit$datasets <- rbind(kit$datasets, conform_kit_table(ds, "datasets"))
  form_of <- stats::setNames(ds$formID, ds$datasetID)
  iv0 <- kit$item_versions
  it0 <- kit$items
  ver_row <- function(uid, ver) {
    iv0[iv0$item_uid == uid & iv0$itemVer == as.character(ver), , drop = FALSE]
  }
  new_it <- list(); new_iv <- list(); new_rel <- list(); new_gm <- list()
  new_bs <- list(); dvr <- list()
  done_iv <- character(0)
  today <- format(kit_today(), "%Y-%m-%d")

  add_version <- function(uid, ver, r, from) {
    key <- paste(uid, ver, sep = "|")
    if (key %in% done_iv) return(invisible())
    done_iv <<- c(done_iv, key)
    fr <- if (nzchar(from$uid)) ver_row(from$uid, from$ver) else iv0[0, , drop = FALSE]
    vsid <- am_value_set(st, r$values, if (nrow(fr)) fr$valueSetID[1] else "")
    lab <- am_clean_label(r$label)
    new_iv[[key]] <<- data.frame(
      item_uid = uid, itemVer = as.character(ver),
      label = if (nzchar(lab)) lab else if (nrow(fr)) fr$label[1] else "",
      stem = if (nrow(fr)) fr$stem[1] else "", valueSetID = vsid,
      itemType = if (nrow(fr)) fr$itemType[1] else if (nzchar(r$values)) "MC" else "",
      period = "", note = paste0(admin$adminID, " .sav label: ", r$label),
      stringsAsFactors = FALSE)
  }
  add_relation <- function(r, kind) {
    key <- paste(r$from_uid, r$from_ver, r$item_uid, r$itemVer, sep = "|")
    if (key %in% names(new_rel)) return(invisible())
    sim <- r$similarity
    vm <- switch(r$values_match, same = TRUE, codes_differ = FALSE, labels_differ = FALSE, NA)
    new_rel[[key]] <<- data.frame(
      from_uid = r$from_uid, from_ver = r$from_ver, to_uid = r$item_uid, to_ver = r$itemVer,
      kind = kind, similarity = sim,
      comparable = rel_comparable(sim, vm),
      meaning_changed = if (r$values_match == "codes_differ" || sim == "low") "1" else "0",
      effective_admin = admin$adminID, reason = r$reason, decided_by = "admin_merge",
      decided_on = today, sourceID = "", stringsAsFactors = FALSE)
  }

  for (i in seq_len(nrow(p))) {
    r <- p[i, , drop = FALSE]
    cl <- r$class
    if (cl %in% c("same", "new_version", "renamed", "new_item", "not_item")) {
      link <- acc[i] && cl != "not_item"
      dvr[[length(dvr) + 1L]] <- data.frame(
        datasetID = r$datasetID, varname = r$varname,
        item_uid = if (link) r$item_uid else "",
        itemVer = if (link) r$itemVer else "", kind = r$kind, position = r$position,
        order = r$order, public = "", stringsAsFactors = FALSE)
      if (!link) next
      if (cl == "new_version") {
        add_version(r$item_uid, r$itemVer, r, list(uid = r$from_uid, ver = r$from_ver))
        if (nzchar(r$from_uid)) add_relation(r, "revised")
      } else if (cl %in% c("renamed", "new_item") && !r$item_uid %in% it0$item_uid) {
        if (!r$item_uid %in% names(new_it)) {
          if (cl == "renamed") {
            row <- it0[it0$item_uid == r$from_uid, , drop = FALSE][1, , drop = FALSE]
            row$note <- paste0("new varname in ", admin$adminID, "; predecessor ", r$from_uid)
          } else {
            row <- empty_kit_table("items")[1, , drop = FALSE]
            row[] <- ""
            row$block <- r$block
            # 同一個區塊的題目最常見的角色、模組、來源
            sib <- it0[it0$instrument == am_instrument(am_suffix(r$datasetID)) &
                         it0$block == r$block & nzchar(r$block), , drop = FALSE]
            for (cc in c("role", "module", "source_type")) {
              v <- sib[[cc]][nzchar(sib[[cc]])]
              if (length(v)) row[[cc]] <- names(sort(table(v), decreasing = TRUE))[1]
            }
            row$note <- paste0("new in ", admin$adminID, " (.sav header)")
          }
          row$item_uid <- r$item_uid
          row$instrument <- sub("\\..*$", "", r$item_uid)
          row$varname <- sub("^[^.]*\\.", "", r$item_uid)
          if (nzchar(r$block)) row$block <- r$block
          row$deprecated <- "0"
          new_it[[r$item_uid]] <- row
        }
        add_version(r$item_uid, r$itemVer, r,
                    list(uid = if (cl == "renamed") r$from_uid else "", ver = r$from_ver))
        if (cl == "renamed") {
          add_relation(r, "renamed")
          f <- form_of[[r$datasetID]]
          gm <- kit$group_members[kit$group_members$item_uid == r$from_uid &
                                    kit$group_members$formID == f, , drop = FALSE]
          if (nrow(gm)) {
            gm$item_uid <- r$item_uid
            new_gm[[length(new_gm) + 1L]] <- gm
          }
        }
      }
    } else if (cl == "dropped_or_block" && acc[i]) {
      if (r$action %in% c("block_none", "block_subset")) {
        asked <- sub("^block_", "", r$action)
        pb <- kit$block_status[kit$block_status$datasetID == r$prev_dataset &
                                 kit$block_status$block == r$block &
                                 kit$block_status$asked == asked, , drop = FALSE]
        key <- paste(r$datasetID, r$block)
        if (key %in% names(new_bs)) next
        new_bs[[key]] <- if (nrow(pb)) {
          pb <- pb[1, , drop = FALSE]
          pb$datasetID <- r$datasetID
          pb
        } else {
          conform_kit_table(data.frame(datasetID = r$datasetID, block = r$block, asked = asked,
                                       reason = r$reason, decided_by = "admin_merge",
                                       stringsAsFactors = FALSE), "block_status")
        }
      } else if (r$action == "deprecate") {
        kit$items$deprecated[kit$items$item_uid == r$item_uid] <- "1"
      }
    }
  }

  bind <- function(x, tb) {
    if (!length(x)) return(empty_kit_table(tb))
    conform_kit_table(do.call(rbind, unname(x)), tb)
  }
  kit$items <- rbind(kit$items, bind(new_it, "items"))
  kit$item_versions <- rbind(kit$item_versions, bind(new_iv, "item_versions"))
  kit$value_codes <- conform_kit_table(st$value_codes, "value_codes")
  kit$item_relations <- rbind(kit$item_relations, bind(new_rel, "item_relations"))
  gm <- bind(new_gm, "group_members")
  gm <- gm[!duplicated(kit_key_str(gm, KIT_SCHEMA$group_members$key)), , drop = FALSE]
  kit$group_members <- rbind(kit$group_members, gm)
  kit$block_status <- rbind(kit$block_status, bind(new_bs, "block_status"))
  kit$dataset_vars <- rbind(kit$dataset_vars, bind(dvr, "dataset_vars"))
  for (tb in kit_tables()) rownames(kit[[tb]]) <- NULL
  kit
}

#' Summary of admin-merge proposals: count by class and confidence
#'
#' @param proposals Output of [propose_admin_merge()].
#' @return A data frame: `class`, `high`, `medium`, `low`, `total`.
#' @export
admin_merge_summary <- function(proposals) {
  cls <- AM_CLASSES
  conf <- c("high", "medium", "low")
  m <- table(factor(proposals$class, levels = cls), factor(proposals$confidence, levels = conf))
  out <- data.frame(class = cls, high = as.integer(m[, "high"]), medium = as.integer(m[, "medium"]),
                    low = as.integer(m[, "low"]), stringsAsFactors = FALSE)
  out$total <- out$high + out$medium + out$low
  out
}

# 練習：把一次已經在題庫的施測拿掉 ----

#' A copy of the kit without one administration (for practice)
#'
#' Removes the administration as if it had never been merged: its `admins` row, its
#' `datasets`, their `dataset_vars` and `block_status` rows (and rows of any other table
#' with a `datasetID` or `adminID` column), its aliases, and the `item_relations` that
#' take effect with it. Item versions used only by this administration are removed too,
#' with items left without any version (and their group memberships, relations and
#' unused value sets), so [propose_admin_merge()] sees them as new again. Nothing is
#' written to disk: use the result only to try [propose_admin_merge()] on an
#' administration that is already in the kit.
#'
#' @param kit A kit, from [read_kit()].
#' @param adminID The administration to remove, e.g. `"M03M048"`.
#' @return The kit without that administration.
#' @examples
#' \dontrun{
#' kit <- read_kit()
#' practice <- kit_without_admin(kit, "M03M048")
#' p <- propose_admin_merge(practice, list("M03M048-P" = sav_header_meta("data1.sav")), "M03M048")
#' }
#' @export
kit_without_admin <- function(kit, adminID) {
  if (length(adminID) != 1L || !adminID %in% kit$admins$adminID) {
    stop("no administration '", adminID, "' in the kit", call. = FALSE)
  }
  ids <- kit$datasets$datasetID[kit$datasets$adminID == adminID]
  here <- kit$dataset_vars[kit$dataset_vars$datasetID %in% ids &
                             nzchar(kit$dataset_vars$item_uid), , drop = FALSE]
  kit$admins <- kit$admins[kit$admins$adminID != adminID, , drop = FALSE]
  kit$datasets <- kit$datasets[!kit$datasets$datasetID %in% ids, , drop = FALSE]
  for (tb in setdiff(names(kit), c("admins", "datasets"))) {
    x <- kit[[tb]]
    if (!is.data.frame(x)) next
    if ("datasetID" %in% names(x)) x <- x[!x$datasetID %in% ids, , drop = FALSE]
    if ("adminID" %in% names(x)) x <- x[!x$adminID %in% adminID, , drop = FALSE]
    kit[[tb]] <- x
  }
  al <- kit$aliases
  kit$aliases <- al[!(al$target_type == "admin" & al$target_id == adminID), , drop = FALSE]
  rel <- kit$item_relations
  kit$item_relations <- rel[rel$effective_admin != adminID, , drop = FALSE]
  # 只在這次施測用到的版本、題目
  used <- paste(kit$dataset_vars$item_uid, kit$dataset_vars$itemVer)
  only <- setdiff(paste(here$item_uid, here$itemVer), used)
  vkey <- paste(kit$item_versions$item_uid, kit$item_versions$itemVer)
  kit$item_versions <- kit$item_versions[!vkey %in% only, , drop = FALSE]
  gone <- setdiff(kit$items$item_uid, kit$item_versions$item_uid)
  gone <- intersect(gone, here$item_uid)
  if (length(only)) {
    vkey <- paste(kit$item_versions$item_uid, kit$item_versions$itemVer)
    rel <- kit$item_relations
    kit$item_relations <- rel[paste(rel$from_uid, rel$from_ver) %in% vkey &
                                paste(rel$to_uid, rel$to_ver) %in% vkey, , drop = FALSE]
  }
  if (length(gone)) {
    kit$items <- kit$items[!kit$items$item_uid %in% gone, , drop = FALSE]
    for (tb in setdiff(names(kit), "items")) {
      x <- kit[[tb]]
      if (is.data.frame(x) && "item_uid" %in% names(x)) {
        kit[[tb]] <- x[!x$item_uid %in% gone, , drop = FALSE]
      }
    }
    al <- kit$aliases
    kit$aliases <- al[!(al$target_type == "item" & al$target_id %in% gone), , drop = FALSE]
  }
  vs <- unique(kit$item_versions$valueSetID)
  kit$value_codes <- kit$value_codes[kit$value_codes$valueSetID %in% vs, , drop = FALSE]
  for (tb in names(kit)) if (is.data.frame(kit[[tb]])) rownames(kit[[tb]]) <- NULL
  kit
}

# 讀 .sav 表頭 ----

#' Read only the header of a .sav file (names, labels, value labels)
#'
#' Uses `haven` (suggested, not required) to read zero data rows, so no person-level data
#' is loaded. Returns the same columns as [read_sav_meta()].
#'
#' @param path Path to a `.sav` file.
#' @return A data frame: var, class, label, n_vlab, vlabs, na (all character).
#' @export
sav_header_meta <- function(path) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("reading .sav files needs the haven package", call. = FALSE)
  }
  d <- haven::read_sav(path, user_na = TRUE, n_max = 0L)
  rows <- lapply(names(d), function(v) {
    x <- d[[v]]
    lab <- attr(x, "label", exact = TRUE) %||% ""
    vl <- attr(x, "labels", exact = TRUE)
    vtxt <- if (length(vl)) paste0(norm_code(unname(vl)), "=", names(vl), collapse = "; ") else ""
    na <- c(if (length(attr(x, "na_values"))) norm_code(attr(x, "na_values")),
            if (length(attr(x, "na_range"))) paste0("range ", paste(attr(x, "na_range"), collapse = "-")))
    data.frame(var = v, class = class(x)[length(class(x))], label = as.character(lab),
               n_vlab = as.character(length(vl)), vlabs = vtxt,
               na = paste(na, collapse = ","), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) out <- read_sav_meta_empty()
  rownames(out) <- NULL
  out
}

#' 空的表頭
#' @noRd
read_sav_meta_empty <- function() {
  cols <- c("var", "class", "label", "n_vlab", "vlabs", "na")
  as.data.frame(stats::setNames(rep(list(character(0)), length(cols)), cols),
                stringsAsFactors = FALSE)
}

#' 提案寫成 Excel 的各張表（第一張是說明）
#'
#' @param proposals Output of [propose_admin_merge()].
#' @return A named list of data frames for `writexl::write_xlsx()`.
#' @export
admin_merge_workbook <- function(proposals) {
  admin <- attr(proposals, "admin")
  ds <- attr(proposals, "datasets")
  s <- admin_merge_summary(proposals)
  info <- data.frame(
    # 原文：施測 | 資料檔 | 產生日期 | 怎麼用
    a = c("\u65bd\u6e2c", "\u8cc7\u6599\u6a94", "\u7522\u751f\u65e5\u671f", "\u600e\u9ebc\u7528",
           "same", "new_version", "renamed", "new_item", "not_item", "dropped_or_block",
           # 原文：信心 | 注意
           "missing_scheme_change", "\u4fe1\u5fc3", "\u6ce8\u610f"),
    b = c(
      if (is.null(admin)) "" else admin$adminID,
      # 原文：（上一次：
      if (is.null(ds)) "" else paste(paste0(ds$datasetID, "\uff08\u4e0a\u4e00\u6b21\uff1a",
                                            # 原文：沒有 | ）
                                            ifelse(nzchar(ds$prev), ds$prev, "\u6c92\u6709"), "\uff09"),
                                     # 原文：；
                                     collapse = "\uff1b"),
      format(kit_today(), "%Y-%m-%d"),
      # 原文：每列是一個提案。看過後把要接受的 proposal_id 交給 apply_admin_merge()（或全部接受），產生題庫的新列，再用 check_kit() 檢查。
      "\u6bcf\u5217\u662f\u4e00\u500b\u63d0\u6848\u3002\u770b\u904e\u5f8c\u628a\u8981\u63a5\u53d7\u7684 proposal_id \u4ea4\u7d66 apply_admin_merge()\uff08\u6216\u5168\u90e8\u63a5\u53d7\uff09\uff0c\u7522\u751f\u984c\u5eab\u7684\u65b0\u5217\uff0c\u518d\u7528 check_kit() \u6aa2\u67e5\u3002",
      # 原文：題目和版本題庫裡已經有。
      "\u984c\u76ee\u548c\u7248\u672c\u984c\u5eab\u88e1\u5df2\u7d93\u6709\u3002",
      # 原文：同一個變項名，標籤或選項變了：建新版本，記一筆改版關係。
      "\u540c\u4e00\u500b\u8b8a\u9805\u540d\uff0c\u6a19\u7c64\u6216\u9078\u9805\u8b8a\u4e86\uff1a\u5efa\u65b0\u7248\u672c\uff0c\u8a18\u4e00\u7b46\u6539\u7248\u95dc\u4fc2\u3002",
      # 原文：變項名不一樣，但是同一題：建新題號，記一筆換題號關係。
      "\u8b8a\u9805\u540d\u4e0d\u4e00\u6a23\uff0c\u4f46\u662f\u540c\u4e00\u984c\uff1a\u5efa\u65b0\u984c\u865f\uff0c\u8a18\u4e00\u7b46\u63db\u984c\u865f\u95dc\u4fc2\u3002",
      # 原文：題庫裡沒有的新題。
      "\u984c\u5eab\u88e1\u6c92\u6709\u7684\u65b0\u984c\u3002",
      # 原文：系統、加權、衍生變項，不是題目。
      "\u7cfb\u7d71\u3001\u52a0\u6b0a\u3001\u884d\u751f\u8b8a\u9805\uff0c\u4e0d\u662f\u984c\u76ee\u3002",
      # 原文：上一次有、這次沒有的題目或區塊（action：block_none 沒問、block_subset 只問一部分、deprecate 停用、not_asked 只記下）。
      "\u4e0a\u4e00\u6b21\u6709\u3001\u9019\u6b21\u6c92\u6709\u7684\u984c\u76ee\u6216\u5340\u584a\uff08action\uff1ablock_none \u6c92\u554f\u3001block_subset \u53ea\u554f\u4e00\u90e8\u5206\u3001deprecate \u505c\u7528\u3001not_asked \u53ea\u8a18\u4e0b\uff09\u3002",
      # 原文：遺漏代碼和上一次的方案不一樣，建議換方案（scheme 欄）。
      "\u907a\u6f0f\u4ee3\u78bc\u548c\u4e0a\u4e00\u6b21\u7684\u65b9\u6848\u4e0d\u4e00\u6a23\uff0c\u5efa\u8b70\u63db\u65b9\u6848\uff08scheme \u6b04\uff09\u3002",
      # 原文：高：可以直接接受；中：建議看一下；低：一定要人看。
      "\u9ad8\uff1a\u53ef\u4ee5\u76f4\u63a5\u63a5\u53d7\uff1b\u4e2d\uff1a\u5efa\u8b70\u770b\u4e00\u4e0b\uff1b\u4f4e\uff1a\u4e00\u5b9a\u8981\u4eba\u770b\u3002",
      # 原文：只讀了 .sav 的表頭（變項名、標籤、值標籤），沒有讀任何個人資料。
      "\u53ea\u8b80\u4e86 .sav \u7684\u8868\u982d\uff08\u8b8a\u9805\u540d\u3001\u6a19\u7c64\u3001\u503c\u6a19\u7c64\uff09\uff0c\u6c92\u6709\u8b80\u4efb\u4f55\u500b\u4eba\u8cc7\u6599\u3002"),
    stringsAsFactors = FALSE)
  is_cls <- info$a %in% AM_CLASSES
  # 原文：（ | ）
  info$a[is_cls] <- paste0(info$a[is_cls], "\uff08", AM_CLASS_LABEL[info$a[is_cls]], "\uff09")
  info <- rbind(info, data.frame(
    # 原文：決定
    a = "\u6c7a\u5b9a",
    # 原文：「提案」表最後一欄。每列填「接受」「不接受」或「再討論」，填好交給維護者；維護者把填「接受」的提案編號（proposal_id）交給 apply_admin_merge()。
    b = "\u300c\u63d0\u6848\u300d\u8868\u6700\u5f8c\u4e00\u6b04\u3002\u6bcf\u5217\u586b\u300c\u63a5\u53d7\u300d\u300c\u4e0d\u63a5\u53d7\u300d\u6216\u300c\u518d\u8a0e\u8ad6\u300d\uff0c\u586b\u597d\u4ea4\u7d66\u7dad\u8b77\u8005\uff1b\u7dad\u8b77\u8005\u628a\u586b\u300c\u63a5\u53d7\u300d\u7684\u63d0\u6848\u7de8\u865f\uff08proposal_id\uff09\u4ea4\u7d66 apply_admin_merge()\u3002",
    stringsAsFactors = FALSE), data.frame(
    # 原文：欄名
    a = "\u6b04\u540d",
    # 原文：「提案」表的欄名是中文；類別代碼、提案編號和 R 裡一樣（class、proposal_id），旁邊的「類別」是中文名稱。
    b = "\u300c\u63d0\u6848\u300d\u8868\u7684\u6b04\u540d\u662f\u4e2d\u6587\uff1b\u985e\u5225\u4ee3\u78bc\u3001\u63d0\u6848\u7de8\u865f\u548c R \u88e1\u4e00\u6a23\uff08class\u3001proposal_id\uff09\uff0c\u65c1\u908a\u7684\u300c\u985e\u5225\u300d\u662f\u4e2d\u6587\u540d\u7a31\u3002",
    stringsAsFactors = FALSE))
  # 原文：項目 | 說明
  names(info) <- c("\u9805\u76ee", "\u8aaa\u660e")
  prop <- as.data.frame(proposals, stringsAsFactors = FALSE)
  attr(prop, "admin") <- NULL
  attr(prop, "datasets") <- NULL
  cls_lab <- unname(AM_CLASS_LABEL[prop$class])
  cls_lab[is.na(cls_lab)] <- ""
  k <- match("class", names(prop))
  prop <- cbind(prop[, seq_len(k), drop = FALSE], class_label = cls_lab,
                prop[, -seq_len(k), drop = FALSE], stringsAsFactors = FALSE)
  # 信心寫成和畫面一樣的 高／中／低
  conf <- unname(AM_CONF_LABEL[prop$confidence])
  prop$confidence <- ifelse(is.na(conf), prop$confidence, conf)
  prop$decision <- rep("", nrow(prop))
  zh <- AM_COLUMN_ZH[names(prop)]
  names(prop) <- ifelse(is.na(zh), names(prop), zh)
  s_lab <- unname(AM_CLASS_LABEL[s$class])
  s <- data.frame(s$class, ifelse(is.na(s_lab), "", s_lab), s$high, s$medium, s$low, s$total,
                  stringsAsFactors = FALSE)
  # 原文：類別代碼 | 類別 | 信心高 | 信心中 | 信心低 | 合計
  names(s) <- c("\u985e\u5225\u4ee3\u78bc", "\u985e\u5225", "\u4fe1\u5fc3\u9ad8", "\u4fe1\u5fc3\u4e2d", "\u4fe1\u5fc3\u4f4e", "\u5408\u8a08")
  # 原文：說明 | 統計 | 提案
  stats::setNames(list(info, s, prop), c("\u8aaa\u660e", "\u7d71\u8a08", "\u63d0\u6848"))
}
