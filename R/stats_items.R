# R/stats_items.R — 證據層：一波資料的題項統計（每個變項一列）
#
# 資料人員上傳一個資料檔（寬格式：一列一個孩子，欄名是 SPSS 變項名稱），
# 這裡算每個變項的人數、各遺漏代碼的人數、平均數、標準差、百分位數和選項次數。
#
# 壓縮規則（一定要做）：
#   - 次數 1 到 4 寫成 "<5"（選項次數、各遺漏代碼的人數、系統遺漏人數）；0 照寫
#   - 有效人數少於 10：不算平均數、標準差、百分位數
#   - 不做交叉表
# 遺漏代碼先拿掉才算統計。結果只有彙總，不會留下任何一個人的值。
#
# 隱私：上傳的檔案讀進記憶體後馬上刪掉（stats_read_upload()），不寫到別的地方、不印出值。

# 有效人數少於這個數，不算平均數、標準差、百分位數
STATS_MIN_N <- 10L
# 次數少於這個數（且大於 0），寫成 "<5"
STATS_MIN_CELL <- 5L

#' Is the statistics upload allowed in this R session?
#'
#' Statistics need the raw data of a wave, so the page only works when the option
#' `kititembank.allow_data_upload` is `TRUE` (set it only on a local computer).
#'
#' @return `TRUE` or `FALSE`.
#' @examples
#' stats_upload_allowed()
#' @export
stats_upload_allowed <- function() {
  isTRUE(getOption("kititembank.allow_data_upload", FALSE))
}

#' 次數 → 文字，1 到 4 寫成 "<5"
#' @noRd
stats_count_str <- function(n) {
  n <- as.integer(n)
  ifelse(is.na(n), "", ifelse(n > 0L & n < STATS_MIN_CELL, "<5", as.character(n)))
}

#' 數字 → 文字（三位小數）；NA 變空字串
#' @noRd
stats_num_str <- function(x, digits = 3L) {
  x <- as.numeric(x)
  ifelse(is.na(x) | !is.finite(x), "", formatC(round(x, digits), format = "f", digits = digits))
}

#' Read one uploaded data file into memory and delete the file
#'
#' Reads a wide data file (one row per child) from `.sav` (needs the haven package)
#' or `.csv`. The file at `path` is deleted as soon as it has been read, also when
#' reading fails. Nothing is written anywhere.
#'
#' @param path Path of the uploaded file (for example Shiny's temporary `datapath`).
#' @param name Original file name; its extension decides the reader.
#' @return A data frame: numeric columns stay numeric (SPSS value labels dropped,
#'   user-missing codes kept as numbers), everything else is character.
#' @export
stats_read_upload <- function(path, name = basename(path)) {
  on.exit(unlink(path), add = TRUE)
  ext <- tolower(sub("^.*\\.([^.]*)$", "\\1", name))
  if (identical(ext, "sav")) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      # 原文：要讀 .sav 檔需要 haven 套件；沒有的話請先存成 .csv。
      stop("\u8981\u8b80 .sav \u6a94\u9700\u8981 haven \u5957\u4ef6\uff1b\u6c92\u6709\u7684\u8a71\u8acb\u5148\u5b58\u6210 .csv\u3002", call. = FALSE)
    }
    # user_na = TRUE：SPSS 裡設成「使用者遺漏」的代碼（7777 等）照原數字讀進來，才數得到
    d <- haven::read_sav(path, user_na = TRUE)
    d <- lapply(d, function(v) {
      v <- unclass(v)
      attributes(v) <- NULL
      if (is.numeric(v)) v else as.character(v)
    })
  } else if (identical(ext, "csv")) {
    d <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                         na = c("", "NA"), locale = readr::locale(encoding = "UTF-8"),
                         progress = FALSE, show_col_types = FALSE)
    d <- as.list(d)
  } else {
    # 原文：只能讀 .sav 或 .csv 檔。
    stop("\u53ea\u80fd\u8b80 .sav \u6216 .csv \u6a94\u3002", call. = FALSE)
  }
  as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)
}

#' 資料檔的欄名對到題庫的變項（大小寫不分）
#'
#' @return data.frame：varname（題庫寫法）、column（資料檔裡的欄名）、item_uid、itemVer、kind
#' @noRd
stats_match_vars <- function(data, kit, datasetID, kinds = NULL) {
  dv <- kit$dataset_vars[kit$dataset_vars$datasetID == datasetID, , drop = FALSE]
  if (!is.null(kinds)) dv <- dv[dv$kind %in% kinds, , drop = FALSE]
  hit <- match(tolower(dv$varname), tolower(names(data)))
  dv <- dv[!is.na(hit), , drop = FALSE]
  if (!is.null(dv$order)) {
    o <- suppressWarnings(as.integer(dv$order))
    dv <- dv[order(is.na(o), o, dv$varname), , drop = FALSE]
  }
  data.frame(varname = dv$varname, column = names(data)[match(tolower(dv$varname), tolower(names(data)))],
             item_uid = dv$item_uid, itemVer = dv$itemVer, kind = dv$kind,
             stringsAsFactors = FALSE)
}

#' 一個資料檔用的遺漏代碼（數字）和標籤
#' @noRd
stats_missing_codes <- function(kit, datasetID) {
  sch <- kit$datasets$missingScheme[kit$datasets$datasetID == datasetID]
  sch <- if (length(sch)) sch[1] else ""
  ms <- kit$missing_schemes[kit$missing_schemes$scheme == sch, , drop = FALSE]
  code <- suppressWarnings(as.numeric(ms$code))
  ms <- ms[!is.na(code), , drop = FALSE]
  code <- code[!is.na(code)]
  ord <- order(code)
  list(scheme = sch, code = code[ord], label = ms$label[ord])
}

#' 一題的選項代碼（數字，不含遺漏代碼）；沒有選項或不是數字時 numeric(0)
#' @noRd
stats_value_codes <- function(kit, item_uid, itemVer, miss_codes = numeric(0)) {
  if (!nzchar(item_uid %||% "")) return(numeric(0))
  iv <- kit$item_versions
  vs <- iv$valueSetID[iv$item_uid == item_uid & iv$itemVer == itemVer]
  if (!length(vs) || !nzchar(vs[1])) {
    vs <- iv$valueSetID[iv$item_uid == item_uid]
    vs <- vs[nzchar(vs)]
  }
  if (!length(vs)) return(numeric(0))
  code <- kit$value_codes$code[kit$value_codes$valueSetID == vs[1]]
  code <- suppressWarnings(as.numeric(code))
  if (!length(code) || anyNA(code)) return(numeric(0))
  sort(setdiff(unique(code), miss_codes))
}

#' 一欄變成數字；有非數字的內容就回 NULL（文字變項）
#' @noRd
stats_as_numeric <- function(v) {
  if (is.numeric(v)) return(as.numeric(v))
  v <- trimws(as.character(v))
  v[v %in% ""] <- NA
  x <- suppressWarnings(as.numeric(v))
  if (any(is.na(x) & !is.na(v))) return(NULL)
  x
}

#' Item-level statistics of one data file
#'
#' For each variable of `datasetID` found in `data` (names match without regard to case):
#' number of rows, valid n, count of each missing code of the dataset's scheme, missing
#' rate, mean, SD, 5th/50th/95th percentiles, and category counts for items with value
#' codes. Missing codes are removed before any statistic is computed.
#'
#' Suppression: counts from 1 to 4 are written as `"<5"`; with fewer than 10 valid
#' values no mean, SD or percentiles are given. Only one variable at a time is
#' summarised (no cross-tabs).
#'
#' @param data A data frame, one row per child (e.g. from [stats_read_upload()]).
#' @param kit A kit list, e.g. from [read_kit()].
#' @param datasetID The data file, e.g. `"M36M084G01-P"`.
#' @param kinds Which `dataset_vars$kind` to summarise (default items only).
#' @return A data frame (all columns character) with the columns of the `item_stats`
#'   table except `analysisID`.
#' @export
item_stats <- function(data, kit, datasetID, kinds = "item") {
  vars <- stats_match_vars(data, kit, datasetID, kinds)
  mc <- stats_missing_codes(kit, datasetID)
  n_total <- nrow(data)
  rows <- lapply(seq_len(nrow(vars)), function(i) {
    x <- stats_as_numeric(data[[vars$column[i]]])
    note <- character(0)
    if (is.null(x)) {
      # 文字變項：只數人數，不列內容
      v <- trimws(as.character(data[[vars$column[i]]]))
      sysmis <- is.na(v) | !nzchar(v)
      is_mc <- !sysmis & suppressWarnings(as.numeric(v)) %in% mc$code
      miss_n <- vapply(mc$code, function(cd) sum(!sysmis & suppressWarnings(as.numeric(v)) %in% cd), integer(1))
      valid_n <- sum(!sysmis & !is_mc)
      return(stats_item_row(vars[i, ], "text", n_total, valid_n, sum(sysmis), mc, miss_n,
                            # 原文：文字變項：只算人數
                            NULL, numeric(0), "\u6587\u5b57\u8b8a\u9805\uff1a\u53ea\u7b97\u4eba\u6578"))
    }
    sysmis <- is.na(x)
    miss_n <- vapply(mc$code, function(cd) sum(!sysmis & x == cd), integer(1))
    valid <- x[!sysmis & !(x %in% mc$code)]
    codes <- stats_value_codes(kit, vars$item_uid[i], vars$itemVer[i], mc$code)
    stats_item_row(vars[i, ], if (length(codes)) "coded" else "numeric", n_total, length(valid),
                   sum(sysmis), mc, miss_n, valid, codes, note)
  })
  out <- if (length(rows)) do.call(rbind, rows) else conform_kit_table(NULL, "item_stats")
  out <- out[, setdiff(names(KIT_SCHEMA$item_stats$cols), "analysisID"), drop = FALSE]
  rownames(out) <- NULL
  out
}

#' 一個變項的一列（已壓縮）
#' @noRd
stats_item_row <- function(var, type, n_total, n_valid, n_sysmis, mc, miss_n, valid, codes,
                           note) {
  n_miss <- sum(miss_n) + n_sysmis
  has_stats <- !is.null(valid) && n_valid >= STATS_MIN_N
  q <- if (has_stats) stats::quantile(valid, c(.05, .5, .95), names = FALSE, type = 7) else
    rep(NA_real_, 3)
  freq <- ""
  if (!is.null(valid) && length(codes)) {
    cnt <- vapply(codes, function(cd) sum(valid == cd), integer(1))
    other <- sum(!(valid %in% codes))
    freq <- paste0(stats_format_code(codes), "=", stats_count_str(cnt), collapse = "; ")
    if (other > 0L) {
      freq <- paste0(freq, "; other=", stats_count_str(other))
      # 原文：有不在選項裡的值（other）
      note <- c(note, "\u6709\u4e0d\u5728\u9078\u9805\u88e1\u7684\u503c\uff08other\uff09")
    }
  }
  # 原文：有效人數少於 10，不列平均數等
  if (!is.null(valid) && n_valid < STATS_MIN_N) note <- c(note, "\u6709\u6548\u4eba\u6578\u5c11\u65bc 10\uff0c\u4e0d\u5217\u5e73\u5747\u6578\u7b49")
  data.frame(
    varname = var$varname, item_uid = var$item_uid, itemVer = var$itemVer, value_type = type,
    n_total = as.character(n_total), n_valid = stats_count_str(n_valid),
    n_sysmis = stats_count_str(n_sysmis),
    missing = if (length(mc$code)) paste0(stats_format_code(mc$code), "=", stats_count_str(miss_n),
                                          collapse = "; ") else "",
    miss_rate = if (n_total > 0L) stats_num_str(n_miss / n_total) else "",
    mean = if (has_stats) stats_num_str(mean(valid)) else "",
    sd = if (has_stats) stats_num_str(stats::sd(valid)) else "",
    p5 = stats_num_str(q[1]), p50 = stats_num_str(q[2]), p95 = stats_num_str(q[3]),
    freq = freq,
    # 原文：；
    note = paste(note, collapse = "\uff1b"),
    stringsAsFactors = FALSE
  )
}

# 代碼印成整數（1 而不是 1.000）
stats_format_code <- function(x) format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
