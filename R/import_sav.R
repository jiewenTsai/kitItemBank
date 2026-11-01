# R/import_sav.R — 步驟 1.3：把 .sav 表頭（*_meta.csv）轉成 dataset_vars 和值標籤證據
#
# 不直接讀 .sav（套件不依賴 haven）。先用 haven 把每個 .sav 的表頭存成
# <file>_meta.csv（欄位 var, class, label, n_vlab, vlabs, na），再交給這裡。
# vlabs 格式 "1=男; 2=女"；na 是 SPSS 宣告的使用者遺漏值（"7777,9999" 或 "range a-b"）。

# 讀表頭 ----

#' Read a .sav header metadata CSV
#'
#' @param file Path to a `<file>_meta.csv` produced from a .sav header.
#' @return A data frame with columns var, class, label, n_vlab, vlabs, na (all character).
#' @export
read_sav_meta <- function(file) {
  df <- as.data.frame(readr::read_csv(
    file, locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE,
    na = character(0), col_types = readr::cols(.default = readr::col_character())
  ))
  need <- c("var", "class", "label", "n_vlab", "vlabs", "na")
  for (cc in setdiff(need, names(df))) df[[cc]] <- rep("", nrow(df))
  df <- df[, need, drop = FALSE]
  df[] <- lapply(df, function(v) { v[is.na(v)] <- ""; v })
  df
}

# 值標籤 ----

#' 解析 "1=男; 2=女" 成 code / label；標籤內容本身可能含 "; "，只在下一個 "數字=" 前切
#' @noRd
parse_vlabs <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) {
    return(data.frame(code = character(0), label = character(0), stringsAsFactors = FALSE))
  }
  parts <- strsplit(x, ";\\s*(?=-?[0-9.]+=)", perl = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  code <- sub("=.*$", "", parts)
  label <- sub("^[^=]*=", "", parts)
  data.frame(code = norm_code(code), label = trimws(label), stringsAsFactors = FALSE)
}

#' 代碼正規化：去空白、"1.0" → "1"
#' @noRd
norm_code <- function(x) {
  x <- trimws(as.character(x))
  num <- suppressWarnings(as.numeric(x))
  ok <- !is.na(num) & grepl("^-?[0-9.]+$", x)
  x[ok] <- format(num[ok], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  x
}

#' 解析 na 欄：回傳個別代碼和範圍（lo, hi）
#' @noRd
parse_sav_na <- function(x) {
  out <- list(codes = character(0), lo = numeric(0), hi = numeric(0))
  if (is.na(x) || !nzchar(trimws(x))) return(out)
  for (p in trimws(strsplit(x, ",")[[1]])) {
    if (grepl("^range", p)) {
      r <- suppressWarnings(as.numeric(strsplit(sub("^range\\s*", "", p),
                                                "(?<=[0-9])-", perl = TRUE)[[1]]))
      if (length(r) == 2L && !anyNA(r)) { out$lo <- c(out$lo, r[1]); out$hi <- c(out$hi, r[2]) }
    } else if (nzchar(p)) {
      out$codes <- c(out$codes, norm_code(p))
    }
  }
  out
}

#' 公版遺漏代碼（使用說明：7777 拒答、8888 不知道、9996 跳答或不適用、9999 遺漏值）
#' @noRd
SAV_MISS_CODES <- c("7777" = "refused", "8888" = "dont_know", "9995" = "missing",
                    "9996" = "not_applicable", "9997" = "missing", "9998" = "missing",
                    "9999" = "missing")

#' 從標籤判斷遺漏類別；判斷不出回傳 ""
#' @noRd
miss_kind_from_label <- function(label) {
  k <- rep("", length(label))
  # 去掉方括號裡的跳題指示（例如 "本國[跳答第4題]"）
  label <- gsub("\\[[^]]*\\]", "", label)
  # 原文：遺漏
  k[grepl("\u907a\u6f0f", label)] <- "missing"
  # 原文：跳答
  k[grepl("\u8df3\u7b54", label)] <- "skipped"
  # 原文：不適用
  k[grepl("\u4e0d\u9069\u7528", label)] <- "not_applicable"
  # 原文：不知道
  k[grepl("\u4e0d\u77e5\u9053", label)] <- "dont_know"
  # 原文：拒答
  k[grepl("\u62d2\u7b54", label)] <- "refused"
  k
}

#' Value labels from a .sav header, one row per (variable, code)
#'
#' A code counts as missing when SPSS declares it user-missing (`na` column), or,
#' because most released files declare none, when it is one of the project-wide
#' special codes (7777, 8888, 9995-9999) or its label says so (refused, don't know,
#' skipped, not applicable, missing). `miss_source` says which rule fired.
#'
#' @param meta Output of [read_sav_meta()].
#' @return A data frame: varname, code, label, is_missing ("0"/"1"), miss_kind, miss_source.
#' @export
sav_value_sets <- function(meta) {
  rows <- lapply(seq_len(nrow(meta)), function(i) {
    vl <- parse_vlabs(meta$vlabs[i])
    na <- parse_sav_na(meta$na[i])
    # 宣告為遺漏、但沒有值標籤的代碼也要列出來
    extra <- setdiff(na$codes, vl$code)
    if (length(extra)) {
      vl <- rbind(vl, data.frame(code = extra, label = "", stringsAsFactors = FALSE))
    }
    if (!nrow(vl)) return(NULL)
    num <- suppressWarnings(as.numeric(vl$code))
    by_na <- vl$code %in% na$codes
    if (length(na$lo)) {
      for (j in seq_along(na$lo)) by_na <- by_na | (!is.na(num) & num >= na$lo[j] & num <= na$hi[j])
    }
    by_code <- vl$code %in% names(SAV_MISS_CODES)
    lab_kind <- miss_kind_from_label(vl$label)
    by_label <- nzchar(lab_kind)
    kind <- ifelse(by_label, lab_kind,
                   ifelse(by_code, unname(SAV_MISS_CODES[vl$code]),
                          ifelse(by_na, "missing", "")))
    src <- ifelse(by_na, "na", ifelse(by_code, "code", ifelse(by_label, "label", "")))
    data.frame(varname = meta$var[i], code = vl$code, label = vl$label,
               is_missing = ifelse(by_na | by_code | by_label, "1", "0"),
               miss_kind = kind, miss_source = src, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(varname = character(0), code = character(0), label = character(0),
                      is_missing = character(0), miss_kind = character(0),
                      miss_source = character(0), stringsAsFactors = FALSE)
  }
  rownames(out) <- NULL
  out
}

# 變項對題目 ----

#' varname 對到 items 的 item_uid（不分大小寫；C 題庫的 itemID 多了 "C" 開頭，也試一次）
#' @return 與 varname 等長的 item_uid（對不到為 ""）
#' @noRd
match_item_uid <- function(varname, instrument, items) {
  if (is.null(items) || !nrow(items)) return(rep("", length(varname)))
  uid <- items$item_uid
  pre <- paste0(instrument, ".")
  uid <- uid[startsWith(uid, pre)]
  key <- tolower(substring(uid, nchar(pre) + 1L))
  hit <- match(tolower(varname), key)
  if (instrument == "C") {
    miss <- is.na(hit)
    hit[miss] <- match(paste0("c", tolower(varname[miss])), key)
  }
  out <- uid[hit]
  out[is.na(out)] <- ""
  out
}

#' 變項種類規則（依序判斷，先中先贏）
#'
#' 1. weight：標籤含「加權」，或名稱 w_*／wsel*／wt_*／wgt*（不用 weight*，那是體重）
#' 2. derived：IRT 檔（instrument S）裡的非編號變項；名稱 *_eap／_theta／_se／_score／
#'    _sum／_mean／_tot／_irt；標籤含「估計值」「能力值」「總分」且不是題號開頭
#' 3. system：編號、訪員、樣本來源、視訊訪、幼兒性別／出生年月、完訪年月／月齡、
#'    填答者關係；或對到的題目在 items 標為 role = admin
#' 4. item：其他
#' @noRd
sav_var_kind <- function(meta, instrument, item_uid = rep("", nrow(meta)), items = NULL) {
  v <- tolower(meta$var)
  lab <- meta$label
  # 原文：加權
  is_weight <- grepl("\u52a0\u6b0a", lab) | grepl("^(w_|wsel|wt_|wgt)", v)
  is_sys_name <- grepl(paste0(
    "^(release_id|id|.*_id|interviewer.*|sample|video|baby_sex|baby_dob.*|",
    "int_(y|m|d|months|date)|relationship(_o)?|cohort|wave)$"), v)
  # 原文：估計值|能力值|總分；排除「題號.」開頭（[0-9A-Za-z一-鿿]{1,3}\.）
  der_lab <- grepl("\u4f30\u8a08\u503c|\u80fd\u529b\u503c|\u7e3d\u5206", lab) &
    !grepl("^[0-9A-Za-z\u4e00-\u9fff]{1,3}\\.", lab)
  is_derived <- grepl("_(eap|theta|se|score|sum|mean|tot|irt)$", v) | der_lab |
    (instrument == "S" & !is_sys_name)
  is_admin_item <- rep(FALSE, length(v))
  if (!is.null(items) && "role" %in% names(items)) {
    adm <- items$item_uid[items$role %in% "admin"]
    is_admin_item <- nzchar(item_uid) & item_uid %in% adm
  }
  ifelse(is_weight, "weight",
         ifelse(is_derived, "derived",
                ifelse(is_sys_name | is_admin_item, "system", "item")))
}

#' Turn a .sav header into `dataset_vars` rows
#'
#' `item_uid` is filled when `<instrument>.<varname>` exists in `items$item_uid`
#' (case-insensitive; for C also `C.C<varname>`). `itemVer` and `position` stay blank:
#' the version needs the admin's survey period, the position comes from the crosswalk (1.4).
#'
#' @param meta Output of [read_sav_meta()].
#' @param datasetID Dataset key, e.g. `"M36M084G01-P"`.
#' @param instrument One of P, T, C, S.
#' @param items A data frame with `item_uid` (and optionally `role`), e.g. the kit's `items`.
#' @return A data frame in the `dataset_vars` schema (all character).
#' @export
import_sav_header <- function(meta, datasetID, instrument, items = NULL) {
  uid <- match_item_uid(meta$var, instrument, items)
  df <- data.frame(
    datasetID = rep(datasetID, nrow(meta)), varname = meta$var, item_uid = uid,
    itemVer = "", kind = sav_var_kind(meta, instrument, uid, items),
    position = "", order = as.character(seq_len(nrow(meta))),
    stringsAsFactors = FALSE)
  conform_kit_table(df, "dataset_vars")
}

# 和 0.2 題庫比對 ----

#' 0.2 value 文字 "1 很少;\n2 有時;" → 代碼
#' @noRd
v02_value_codes <- function(value) {
  if (is.na(value) || !nzchar(value)) return(character(0))
  segs <- trimws(strsplit(value, "[;\n]")[[1]])
  segs <- segs[grepl("^-?[0-9]+(\\.[0-9]+)?(\\s|$)", segs)]
  unique(norm_code(sub("^(-?[0-9]+(\\.[0-9]+)?).*$", "\\1", segs)))
}

#' 0.2 noResponse "跳答:9996; 遺漏值:9999" → 代碼
#' @noRd
v02_noresponse_codes <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  # 原文：[:：]
  m <- regmatches(x, gregexpr("[:\uff1a]\\s*-?[0-9]+", x))[[1]]
  unique(norm_code(gsub("[^0-9-]", "", m)))
}

#' 一列比對問題
#' @noRd
sav_issue <- function(issue, varname = "", item_uid = "", detail = "") {
  n <- max(length(varname), length(item_uid), length(detail), 1L)
  data.frame(issue = rep(issue, n), varname = rep_len(varname, n),
             item_uid = rep_len(item_uid, n), detail = rep_len(detail, n),
             stringsAsFactors = FALSE)
}

#' Compare a .sav header with the 0.2 bank
#'
#' Issues returned:
#' * `sav_no_v02` — a .sav variable (kind item) with no 0.2 item;
#' * `v02_no_sav` — a 0.2 item marked present in `wave_col` but absent from the .sav
#'   (only when `wave_col` is given; several columns may be joined with `|`);
#' * `value_codes_differ` — substantive (non-missing) code sets differ;
#' * `noresponse_not_in_sav` — a 0.2 noResponse code that the .sav does not use as a
#'   missing code for that variable. `detail` starts with `v02_nonstandard` when the
#'   0.2 code is not a project special code (e.g. 0.2 says refused = 5, the .sav uses
#'   7777), else `sav_lacks_code` (e.g. 0.2 lists 7777 but the .sav has no 7777 label).
#'
#' The 0.2 version compared is the one marked in `wave_col`, else the latest
#' non-deprecated one.
#'
#' @param meta Output of [read_sav_meta()].
#' @param instrument P, T or C (S has no 0.2 bank; returns no rows).
#' @param items_bank A 0.2 bank from [load_bank()].
#' @param wave_col Optional 0.2 `itemWave` column(s) for this admin.
#' @return A data frame: issue, varname, item_uid, detail.
#' @export
compare_sav_v02 <- function(meta, instrument, items_bank, wave_col = NULL) {
  empty <- sav_issue("x")[0, ]
  if (is.null(items_bank) || instrument == "S") return(empty)
  ib <- items_bank$itemBank
  iw <- items_bank$itemWave
  items <- data.frame(item_uid = paste0(instrument, ".", unique(ib$itemID)),
                      stringsAsFactors = FALSE)
  uid <- match_item_uid(meta$var, instrument, items)
  item_id <- sub("^[^.]+\\.", "", uid)
  kind <- sav_var_kind(meta, instrument, uid)
  out <- list(empty)

  # 涵蓋率：.sav 有、0.2 沒有（只算 item 類）
  nm <- !nzchar(uid) & kind == "item"
  if (any(nm)) out[[length(out) + 1L]] <- sav_issue("sav_no_v02", meta$var[nm], "",
                                                    substr(meta$label[nm], 1, 80))

  # 這一波該有、.sav 沒有
  cols <- if (is.null(wave_col) || !nzchar(wave_col)) character(0) else
    intersect(strsplit(wave_col, "|", fixed = TRUE)[[1]], names(iw))
  in_wave <- NULL
  if (length(cols)) {
    on <- Reduce(`|`, lapply(cols, function(cc) iw[[cc]] %in% c("1", 1)))
    in_wave <- iw[on, c("itemID", "itemVer"), drop = FALSE]
    exp_ids <- unique(in_wave$itemID)
    gone <- exp_ids[!tolower(exp_ids) %in% tolower(item_id[nzchar(item_id)])]
    if (length(gone)) out[[length(out) + 1L]] <- sav_issue(
      "v02_no_sav", "", paste0(instrument, ".", gone), paste("in", paste(cols, collapse = "|")))
  }

  # 值標籤比對
  vs <- sav_value_sets(meta)
  for (i in which(nzchar(uid))) {
    v <- meta$var[i]
    s <- vs[vs$varname == v, , drop = FALSE]
    if (!nrow(s)) next
    cand <- ib[ib$itemID == item_id[i], , drop = FALSE]
    if (!is.null(in_wave)) {
      w <- cand[paste(cand$itemID, cand$itemVer) %in% paste(in_wave$itemID, in_wave$itemVer), ,
                drop = FALSE]
      if (nrow(w)) cand <- w
    }
    nd <- cand[!cand$deprecated %in% c("1", 1), , drop = FALSE]
    if (nrow(nd)) cand <- nd
    row <- cand[which.max(as.integer(cand$itemVer)), , drop = FALSE]
    sav_sub <- s$code[s$is_missing == "0"]
    sav_mis <- s$code[s$is_missing == "1"]
    v02_sub <- v02_value_codes(row$value)
    v02_mis <- v02_noresponse_codes(row$noResponse)
    # 0.2 value 裡偶爾也寫了遺漏代碼，不算實質代碼
    v02_sub <- setdiff(v02_sub, c(v02_mis, names(SAV_MISS_CODES)))
    if (length(sav_sub) && length(v02_sub) && !setequal(sav_sub, v02_sub)) {
      rel <- if (all(sav_sub %in% v02_sub)) "sav_subset" else
        if (all(v02_sub %in% sav_sub)) "v02_subset" else
          if (!length(intersect(sav_sub, v02_sub))) "disjoint" else "overlap"
      out[[length(out) + 1L]] <- sav_issue(
        "value_codes_differ", v, uid[i],
        sprintf("%s; sav={%s} v02(v%s)={%s}", rel, paste(sav_sub, collapse = ","),
                row$itemVer, paste(v02_sub, collapse = ",")))
    }
    bad <- setdiff(v02_mis, sav_mis)
    if (length(bad)) out[[length(out) + 1L]] <- sav_issue(
      "noresponse_not_in_sav", v, uid[i],
      sprintf("%s; v02(v%s) noResponse={%s} sav_missing={%s}",
              if (any(!bad %in% names(SAV_MISS_CODES))) "v02_nonstandard" else "sav_lacks_code",
              row$itemVer,
              paste(v02_mis, collapse = ","), paste(sav_mis, collapse = ",")))
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

# 整批 ----

#' 0.2 三份題庫轉成 items 的最小版（item_uid、role），在 1.1 的 items 還沒好之前用
#' @noRd
v02_items_min <- function(banks) {
  rows <- lapply(names(banks), function(pre) {
    ib <- banks[[pre]]$itemBank
    ib <- ib[order(ib$itemID, -as.integer(ib$itemVer)), , drop = FALSE]
    ib <- ib[!duplicated(ib$itemID), , drop = FALSE]
    data.frame(item_uid = paste0(pre, ".", ib$itemID),
               role = ifelse(ib$itemType %in% "AD", "admin", ""),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' The data-file map for the waves whose .sav headers are available
#'
#' Columns: wave_dir, file, datasetID, adminID, instrument, respondent, wave_col
#' (0.2 `itemWave` column(s) used for the expected-items check; blank = skip).
#' @noRd
sav_map_known <- function() {
  # 原文：家長、親友、教保、教師
  par <- "\u5bb6\u9577"; rel <- "\u89aa\u53cb"; ece <- "\u6559\u4fdd"; tea <- "\u6559\u5e2b"
  # 原文：M036小、M036保母|M036機構、M084大
  w03p <- "M036\u5c0f"; w03t <- "M036\u4fdd\u6bcd|M036\u6a5f\u69cb"; w36p <- "M084\u5927"
  data.frame(
    wave_dir = c(rep("m03w36", 3), rep("m36w84", 2)),
    file = c("data1", "data2", "data3", "data1", "data2"),
    adminID = c(rep("M03M036", 3), rep("M36M084G01", 2)),
    datasetID = c("M03M036-P", "M03M036-P2", "M03M036-T",
                  "M36M084G01-P", "M36M084G01-T"),
    instrument = c("P", "P", "T", "P", "T"),
    respondent = c(par, rel, ece, par, tea),
    wave_col = c(w03p, "", w03t, w36p, "M084"),
    stringsAsFactors = FALSE)
}

#' Run the .sav header import for a folder of `*_meta.csv` files
#'
#' @param dbx_dir Folder holding `<wave_dir>/<file>_meta.csv`.
#' @param map A data frame with wave_dir, file, datasetID, instrument; optional
#'   adminID, respondent, wave_col.
#' @param banks Named list of 0.2 banks by instrument prefix (P, T, C).
#' @param items Items table (item_uid, role); defaults to one built from `banks`.
#' @return A list: `datasets`, `dataset_vars`, `value_sets` (with datasetID),
#'   `issues` (with datasetID), `summary` (one row per file), `map`.
#' @export
import_sav_dir <- function(dbx_dir, map = sav_map_known(),
                           banks = list(P = load_bank("parent"),
                                        T = load_bank("childcare_teacher"),
                                        C = load_bank("child_adolescent")),
                           items = v02_items_min(banks)) {
  if (!"adminID" %in% names(map)) map$adminID <- sub("-[^-]+$", "", map$datasetID)
  if (!"respondent" %in% names(map)) map$respondent <- ""
  if (!"wave_col" %in% names(map)) map$wave_col <- ""
  dv <- vs <- iss <- sm <- list()
  for (i in seq_len(nrow(map))) {
    m <- map[i, ]
    f <- file.path(dbx_dir, m$wave_dir, paste0(m$file, "_meta.csv"))
    if (!file.exists(f)) { message("skip (no file): ", f); next }
    meta <- read_sav_meta(f)
    d <- import_sav_header(meta, m$datasetID, m$instrument, items)
    s <- sav_value_sets(meta)
    s <- cbind(datasetID = rep(m$datasetID, nrow(s)), s, stringsAsFactors = FALSE)
    is <- compare_sav_v02(meta, m$instrument, banks[[m$instrument]], m$wave_col)
    is <- cbind(datasetID = rep(m$datasetID, nrow(is)), is, stringsAsFactors = FALSE)
    k <- table(factor(d$kind, levels = KIT_ENUMS$var_kind))
    it <- d$kind == "item"
    sm[[length(sm) + 1L]] <- data.frame(
      datasetID = m$datasetID, n_vars = nrow(d), matched = sum(nzchar(d$item_uid)),
      pct_matched = round(100 * mean(nzchar(d$item_uid)), 1),
      items = k[["item"]], items_matched = sum(nzchar(d$item_uid[it])),
      system = k[["system"]], weight = k[["weight"]], derived = k[["derived"]],
      n_declared_na = sum(nzchar(meta$na)),
      stringsAsFactors = FALSE)
    dv[[length(dv) + 1L]] <- d; vs[[length(vs) + 1L]] <- s; iss[[length(iss) + 1L]] <- is
  }
  datasets <- conform_kit_table(data.frame(
    datasetID = map$datasetID, adminID = map$adminID,
    file = paste0(map$wave_dir, "/", map$file, ".sav"), instrument = map$instrument,
    respondent = map$respondent, stringsAsFactors = FALSE), "datasets")
  list(datasets = datasets,
       dataset_vars = conform_kit_table(do.call(rbind, dv) %||% empty_kit_table("dataset_vars"),
                                        "dataset_vars"),
       value_sets = do.call(rbind, vs), issues = do.call(rbind, iss),
       summary = do.call(rbind, sm), map = map)
}
