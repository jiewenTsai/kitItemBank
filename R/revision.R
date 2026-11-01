# R/revision.R — 老師改版往返：改版底稿 Excel → 健康檢查 → 規劃中的資料檔（修改包）
#
#   revision_base()          一個題本最近一次施測的資料檔：底稿的根據
#   revision_template()      寫出改版底稿 Excel（說明、題目、新增題目三張表）
#   read_revision_xlsx()     讀回填好的底稿（給 check 和 patch 用）
#   check_revision_xlsx()    健康檢查：每個問題一列，嚴重度「擋下來」或「要注意」
#   revision_patch()         填好的底稿 → 修改包（規劃中的施測、資料檔、新版題目、新題目）
#   revision_patch_workbook() 修改包 Excel 的工作表（read_patch_xlsx() 讀得回來）
#
# 改版的規則：題目文字或選項改了 = 新的題目版本 + revised 關係（舊版留著）；新題目 = 新題號；
# 題號不刪不重用。刪掉的題目只是不放進新的資料檔。底稿要 openxlsx（工作表保護、下拉選單）。
# 頁面在 R/page_revision.R。

# 原文：說明
RV_SHEET_INFO <- "\u8aaa\u660e"
# 原文：題目
RV_SHEET_Q <- "\u984c\u76ee"
# 原文：新增題目
RV_SHEET_NEW <- "\u65b0\u589e\u984c\u76ee"

# 原文：不變 | 改文字 | 改選項 | 刪除
RV_ACTIONS <- c(keep = "\u4e0d\u8b8a", text = "\u6539\u6587\u5b57", options = "\u6539\u9078\u9805", delete = "\u522a\u9664")

# 題目表的欄：前五欄是現況（鎖住），中間是給老師填的，最右邊是系統用（鎖住）
# 原文：列號 | 位置 | 變項 | 目前題目
RV_Q_COLS <- c(rowid = "\u5217\u865f", position = "\u4f4d\u7f6e", varname = "\u8b8a\u9805", label = "\u76ee\u524d\u984c\u76ee",
               # 原文：目前選項 | 動作 | 新題目文字
               values = "\u76ee\u524d\u9078\u9805", action = "\u52d5\u4f5c", new_text = "\u65b0\u984c\u76ee\u6587\u5b57",
               # 原文：新選項 | 備註 | 系統用
               new_values = "\u65b0\u9078\u9805", note = "\u5099\u8a3b", sys = "\u7cfb\u7d71\u7528")
RV_Q_INPUT <- c("action", "new_text", "new_values", "note")

# 原文：變項名稱 | 題目文字 | 選項
RV_NEW_COLS <- c(varname = "\u8b8a\u9805\u540d\u7a31", label = "\u984c\u76ee\u6587\u5b57", values = "\u9078\u9805",
                 # 原文：放在哪一列之後 | 區塊 | 備註
                 after = "\u653e\u5728\u54ea\u4e00\u5217\u4e4b\u5f8c", block = "\u5340\u584a", note = "\u5099\u8a3b")
RV_NEW_REQUIRED <- c("varname", "label", "after")
RV_NEW_ROWS <- 100L

# 原文：開頭
RV_TOP <- "\u958b\u982d"
# 原文：擋下來
RV_BLOCK <- "\u64cb\u4e0b\u4f86"
# 原文：要注意
RV_NOTE <- "\u8981\u6ce8\u610f"

# 說明表的項目（第一欄）
# 原文：題庫版本 | 題本 | 根據的施測
RV_INFO_KEYS <- c(version = "\u984c\u5eab\u7248\u672c", form = "\u984c\u672c", admin = "\u6839\u64da\u7684\u65bd\u6e2c",
                  # 原文：資料檔 | 產生日期
                  dataset = "\u8cc7\u6599\u6a94", date = "\u7522\u751f\u65e5\u671f")

# 底稿用到的底色：表頭、給老師填的欄（黃）、新增題目（綠）
RV_FILL_HEAD <- "#DDE3EA"
RV_FILL_INPUT <- "#FFF4C2"
RV_FILL_NEW <- "#E8F3E8"

# 一個題本最近一次施測 ----

# 施測大概在什麼時候：世代出生起日 + 月齡（以月為單位）；沒有出生日就只用月齡
rv_admin_time <- function(kit, adminID) {
  ad <- kit$admins[match(adminID, kit$admins$adminID), , drop = FALSE]
  born <- kit$cohorts$birth_from[match(ad$cohortID, kit$cohorts$cohortID)]
  y <- cb_int(substr(cb_blank(born), 1L, 4L))
  m <- cb_int(substr(cb_blank(born), 6L, 7L))
  base <- ifelse(is.na(y), 0L, y * 12L + ifelse(is.na(m), 1L, m))
  base + ifelse(is.na(cb_int(ad$ageMonth)), 0L, cb_int(ad$ageMonth))
}

#' The data file a revision template is based on
#'
#' The latest administration that used the form (by cohort birth date plus age in
#' months), and within it the data file with the most item variables.
#'
#' @param kit A kit list, e.g. from [read_kit()].
#' @param formID A form, e.g. `"P-G1-G3"`.
#' @param adminID Optional: use this administration instead of the latest one.
#' @param datasetID Optional: use this data file.
#' @return A list: `form` (the `forms` row), `admin` (the `admins` row), `dataset` (the
#'   `datasets` row), `vars` (all variables of the data file in file order, from
#'   [admin_codebook()]) and `questions` (the item variables with a row id `rowid`,
#'   Q001, Q002, ...).
#' @examples
#' b <- revision_base(read_kit(), "P-G1-G3")
#' b$admin$adminID
#' head(b$questions[c("rowid", "varname", "label")])
#' @export
revision_base <- function(kit, formID, adminID = NULL, datasetID = NULL) {
  formID <- cb_blank(formID)
  if (length(formID) != 1L || !formID %in% kit$forms$formID) {
    # 原文：題庫裡沒有這個題本：
    kitDesign::kit_abort("\u984c\u5eab\u88e1\u6c92\u6709\u9019\u500b\u984c\u672c\uff1a", paste(formID, collapse = ", "))
  }
  ds <- kit$datasets[kit$datasets$formID == formID, , drop = FALSE]
  n_item <- vapply(ds$datasetID, function(d) {
    sum(kit$dataset_vars$datasetID == d & kit$dataset_vars$kind == "item")
  }, integer(1))
  ds <- ds[n_item > 0L, , drop = FALSE]
  n_item <- n_item[n_item > 0L]
  if (!is.null(datasetID)) {
    keep <- ds$datasetID %in% datasetID
    ds <- ds[keep, , drop = FALSE]
    n_item <- n_item[keep]
  }
  if (!is.null(adminID)) {
    keep <- ds$adminID %in% adminID
    ds <- ds[keep, , drop = FALSE]
    n_item <- n_item[keep]
  }
  if (nrow(ds) == 0L) {
    # 原文：題本  |  沒有找到有題目的資料檔。
    kitDesign::kit_abort("\u984c\u672c ", formID, " \u6c92\u6709\u627e\u5230\u6709\u984c\u76ee\u7684\u8cc7\u6599\u6a94\u3002")
  }
  t <- rv_admin_time(kit, ds$adminID)
  ds <- ds[t == max(t), , drop = FALSE]
  n_item <- n_item[t == max(t)]
  ds <- ds[which.max(n_item), , drop = FALSE]

  cb <- admin_codebook(kit, ds$adminID, ds$datasetID)
  vars <- cb$vars
  q <- vars[vars$kind == "item" & nzchar(vars$item_uid), , drop = FALSE]
  width <- max(3L, nchar(nrow(q)))
  q$rowid <- sprintf(paste0("Q%0", width, "d"), seq_len(nrow(q)))
  rownames(q) <- NULL
  list(form = kit$forms[kit$forms$formID == formID, , drop = FALSE],
       admin = cb$admin, dataset = kit$datasets[kit$datasets$datasetID == ds$datasetID, , drop = FALSE],
       vars = vars, questions = q)
}

# 改版底稿 ----

#' 底稿三張表的內容（還沒有格式）
#' @noRd
revision_template_sheets <- function(kit, formID, adminID = NULL, datasetID = NULL,
                                     version = codebook_bank_version(), date = kit_today()) {
  b <- revision_base(kit, formID, adminID, datasetID)
  q <- b$questions
  info_keys <- unname(RV_INFO_KEYS[c("version", "form", "admin", "dataset", "date")])
  info <- data.frame(
    # 原文：這個檔案 | 怎麼填
    k = c("\u9019\u500b\u6a94\u6848", info_keys, "", "\u600e\u9ebc\u586b",
          "1.", "2.", "3.", "4.", "5.", "6.", "7."),
    # 原文：KIT 題本改版底稿： | （ | ）。每一題一列，照問卷順序。
    x = c(paste0("KIT \u984c\u672c\u6539\u7248\u5e95\u7a3f\uff1a", b$form$label, "\uff08", formID, "\uff09\u3002\u6bcf\u4e00\u984c\u4e00\u5217\uff0c\u7167\u554f\u5377\u9806\u5e8f\u3002"),
          version, formID,
          # 原文：，代號 
          paste0(admin_label(kit, b$admin$adminID), "\uff0c\u4ee3\u865f ", b$admin$adminID),
          b$dataset$datasetID, format(as.Date(date), "%Y-%m-%d"),
          "", "",
          # 原文：「題目」表：灰色欄是現在的題目，不要改。黃色欄給你填。
          "\u300c\u984c\u76ee\u300d\u8868\uff1a\u7070\u8272\u6b04\u662f\u73fe\u5728\u7684\u984c\u76ee\uff0c\u4e0d\u8981\u6539\u3002\u9ec3\u8272\u6b04\u7d66\u4f60\u586b\u3002",
          # 原文：「動作」用下拉選單選： | 、
          paste0("\u300c\u52d5\u4f5c\u300d\u7528\u4e0b\u62c9\u9078\u55ae\u9078\uff1a", paste(RV_ACTIONS, collapse = "\u3001"),
                 # 原文：。沒選就當作不變。
                 "\u3002\u6c92\u9078\u5c31\u7576\u4f5c\u4e0d\u8b8a\u3002"),
          # 原文：改文字：在「新題目文字」寫改好的整句。改選項：在「新選項」照「1=有; 2=沒有」的寫法寫全部選項。
          "\u6539\u6587\u5b57\uff1a\u5728\u300c\u65b0\u984c\u76ee\u6587\u5b57\u300d\u5beb\u6539\u597d\u7684\u6574\u53e5\u3002\u6539\u9078\u9805\uff1a\u5728\u300c\u65b0\u9078\u9805\u300d\u7167\u300c1=\u6709; 2=\u6c92\u6709\u300d\u7684\u5beb\u6cd5\u5beb\u5168\u90e8\u9078\u9805\u3002",
          # 原文：刪除：這一題不再問。題庫會留著舊題目，只是新的問卷不放它。
          "\u522a\u9664\uff1a\u9019\u4e00\u984c\u4e0d\u518d\u554f\u3002\u984c\u5eab\u6703\u7559\u8457\u820a\u984c\u76ee\uff0c\u53ea\u662f\u65b0\u7684\u554f\u5377\u4e0d\u653e\u5b83\u3002",
          # 原文：要加新題目，寫在「新增題目」表：變項名稱、題目文字、放在哪一列之後一定要填
          paste0("\u8981\u52a0\u65b0\u984c\u76ee\uff0c\u5beb\u5728\u300c\u65b0\u589e\u984c\u76ee\u300d\u8868\uff1a\u8b8a\u9805\u540d\u7a31\u3001\u984c\u76ee\u6587\u5b57\u3001\u653e\u5728\u54ea\u4e00\u5217\u4e4b\u5f8c\u4e00\u5b9a\u8981\u586b",
                 # 原文：（寫「題目」表的列號，例如 Q012；放最前面寫「 | 」）。
                 "\uff08\u5beb\u300c\u984c\u76ee\u300d\u8868\u7684\u5217\u865f\uff0c\u4f8b\u5982 Q012\uff1b\u653e\u6700\u524d\u9762\u5beb\u300c", RV_TOP, "\u300d\uff09\u3002"),
          # 原文：請不要刪列、加列或改列號；要改哪一題，請選動作。
          "\u8acb\u4e0d\u8981\u522a\u5217\u3001\u52a0\u5217\u6216\u6539\u5217\u865f\uff1b\u8981\u6539\u54ea\u4e00\u984c\uff0c\u8acb\u9078\u52d5\u4f5c\u3002",
          # 原文：改完存檔，交回給題庫管理員。
          "\u6539\u5b8c\u5b58\u6a94\uff0c\u4ea4\u56de\u7d66\u984c\u5eab\u7ba1\u7406\u54e1\u3002"),
    stringsAsFactors = FALSE)
  # 原文：項目 | 內容
  names(info) <- c("\u9805\u76ee", "\u5167\u5bb9")

  qs <- data.frame(
    rowid = q$rowid, position = cb_blank(q$position), varname = q$varname,
    label = cb_blank(q$label), values = cb_blank(q$values),
    action = "", new_text = "", new_values = "", note = "",
    sys = paste(q$item_uid, q$itemVer, sep = "|"),
    stringsAsFactors = FALSE)
  names(qs) <- unname(RV_Q_COLS[names(qs)])

  nw <- as.data.frame(stats::setNames(rep(list(character(0)), length(RV_NEW_COLS)),
                                      unname(RV_NEW_COLS)),
                      check.names = FALSE, stringsAsFactors = FALSE)
  sheets <- list(info, qs, nw)
  names(sheets) <- c(RV_SHEET_INFO, RV_SHEET_Q, RV_SHEET_NEW)
  sheets
}

#' Write a revision template (改版底稿) for a form
#'
#' An Excel file for the questionnaire's authors: sheet 說明 (bank version, form, the
#' administration it is based on, date, how to fill it in), sheet 題目 (one row per
#' question in questionnaire order, with a locked row id, the current text and options,
#' and editable yellow columns: action from a drop-down, new text, new options, note) and
#' sheet 新增題目 (new questions). The sheets are protected (no password) so that only
#' the yellow and green cells can be typed in. Needs the 'openxlsx' package.
#'
#' @inheritParams revision_base
#' @param file Path of the `.xlsx` to write.
#' @param version Bank version written on the 說明 sheet; checked again on import.
#' @param date Date written on the 說明 sheet.
#' @return `file`, invisibly.
#' @examples
#' f <- tempfile(fileext = ".xlsx")
#' if (requireNamespace("openxlsx", quietly = TRUE)) {
#'   revision_template(read_kit(), "P-G1-G3", f)
#' }
#' @export
revision_template <- function(kit, formID, file, adminID = NULL, datasetID = NULL,
                              version = codebook_bank_version(), date = kit_today()) {
  rv_need_openxlsx()
  sh <- revision_template_sheets(kit, formID, adminID, datasetID, version, date)
  wb <- openxlsx::createWorkbook()
  hd <- openxlsx::createStyle(textDecoration = "bold", fgFill = RV_FILL_HEAD, wrapText = TRUE,
                              border = "bottom", valign = "center")
  grey <- openxlsx::createStyle(fontColour = "#555555", wrapText = TRUE, valign = "top")
  yellow <- openxlsx::createStyle(fgFill = RV_FILL_INPUT, wrapText = TRUE, valign = "top",
                                  border = "TopBottomLeftRight", borderColour = "#E0C860",
                                  locked = FALSE)
  green <- openxlsx::createStyle(fgFill = RV_FILL_NEW, wrapText = TRUE, valign = "top",
                                 border = "TopBottomLeftRight", borderColour = "#9CC49C",
                                 locked = FALSE)
  sys <- openxlsx::createStyle(fontColour = "#AAAAAA", fontSize = 8)
  # 保護工作表（沒有密碼）：只有黃色、綠色格子能打字；欄寬列高可以調
  protect <- function(name) {
    openxlsx::protectWorksheet(wb, name, protect = TRUE, lockFormattingColumns = FALSE,
                               lockFormattingRows = FALSE)
  }

  # 說明
  info <- sh[[RV_SHEET_INFO]]
  openxlsx::addWorksheet(wb, RV_SHEET_INFO)
  openxlsx::writeData(wb, RV_SHEET_INFO, info, headerStyle = hd)
  openxlsx::setColWidths(wb, RV_SHEET_INFO, 1:2, c(14, 100))
  openxlsx::addStyle(wb, RV_SHEET_INFO, openxlsx::createStyle(wrapText = TRUE, valign = "top"),
                     rows = seq_len(nrow(info)) + 1L, cols = 2)
  protect(RV_SHEET_INFO)

  # 題目
  qs <- sh[[RV_SHEET_Q]]
  n <- nrow(qs)
  rows <- seq_len(n) + 1L
  openxlsx::addWorksheet(wb, RV_SHEET_Q)
  openxlsx::writeData(wb, RV_SHEET_Q, qs, headerStyle = hd)
  ic <- match(RV_Q_COLS[RV_Q_INPUT], names(qs))
  if (n) {
    openxlsx::addStyle(wb, RV_SHEET_Q, grey, rows = rows, cols = setdiff(seq_along(qs), ic),
                       gridExpand = TRUE)
    openxlsx::addStyle(wb, RV_SHEET_Q, yellow, rows = rows, cols = ic, gridExpand = TRUE)
    openxlsx::addStyle(wb, RV_SHEET_Q, sys, rows = rows, cols = ncol(qs), gridExpand = TRUE)
    openxlsx::dataValidation(wb, RV_SHEET_Q, cols = ic[1], rows = rows, type = "list",
                             value = paste0('"', paste(RV_ACTIONS, collapse = ","), '"'))
  }
  openxlsx::freezePane(wb, RV_SHEET_Q, firstActiveRow = 2L, firstActiveCol = 2L)
  openxlsx::setColWidths(wb, RV_SHEET_Q, seq_along(qs), c(7, 8, 14, 40, 30, 10, 40, 30, 24, 10))
  protect(RV_SHEET_Q)

  # 新增題目
  nw <- sh[[RV_SHEET_NEW]]
  openxlsx::addWorksheet(wb, RV_SHEET_NEW)
  openxlsx::writeData(wb, RV_SHEET_NEW, nw, headerStyle = hd)
  req <- match(RV_NEW_COLS[RV_NEW_REQUIRED], names(nw))
  openxlsx::addStyle(wb, RV_SHEET_NEW, openxlsx::createStyle(fontColour = "#B00020"), rows = 1L,
                     cols = req, stack = TRUE)
  openxlsx::addStyle(wb, RV_SHEET_NEW, green, rows = seq_len(RV_NEW_ROWS) + 1L,
                     cols = seq_along(nw), gridExpand = TRUE)
  openxlsx::writeComment(wb, RV_SHEET_NEW, col = 1, row = 1, comment = openxlsx::createComment(
    # 原文：紅字的欄一定要填。放在哪一列之後：寫「題目」表的列號或變項名稱，或「開頭」。
    "\u7d05\u5b57\u7684\u6b04\u4e00\u5b9a\u8981\u586b\u3002\u653e\u5728\u54ea\u4e00\u5217\u4e4b\u5f8c\uff1a\u5beb\u300c\u984c\u76ee\u300d\u8868\u7684\u5217\u865f\u6216\u8b8a\u9805\u540d\u7a31\uff0c\u6216\u300c\u958b\u982d\u300d\u3002", visible = FALSE))
  openxlsx::freezePane(wb, RV_SHEET_NEW, firstRow = TRUE)
  openxlsx::setColWidths(wb, RV_SHEET_NEW, seq_along(nw), c(16, 44, 30, 14, 20, 24))
  protect(RV_SHEET_NEW)

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

rv_need_openxlsx <- function() {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    # 原文：改版底稿要用 openxlsx 套件：install.packages(\"openxlsx\")
    kitDesign::kit_abort("\u6539\u7248\u5e95\u7a3f\u8981\u7528 openxlsx \u5957\u4ef6\uff1ainstall.packages(\"openxlsx\")")
  }
}

# 讀回底稿 ----

rv_read_sheet <- function(path, sheet, col_names = TRUE) {
  df <- suppressMessages(as.data.frame(readxl::read_excel(
    path, sheet = sheet, col_types = "text", col_names = col_names, .name_repair = "minimal"
  )))
  df[] <- lapply(df, function(v) { v <- as.character(v); v[is.na(v)] <- ""; trimws(v) })
  df
}

#' Read a filled revision template
#'
#' @param path A `.xlsx` made with [revision_template()] and filled in.
#' @return A list: `info` (named character: version, form, admin, dataset, date),
#'   `questions` and `new` (data frames with English column names, plus `excel_row`).
#'   Sheets that are missing come back as `NULL`. Uses 'readxl' only.
#' @export
read_revision_xlsx <- function(path) {
  sheets <- readxl::excel_sheets(path)
  out <- list(sheets = sheets, info = NULL, questions = NULL, new = NULL)
  if (RV_SHEET_INFO %in% sheets) {
    info <- rv_read_sheet(path, RV_SHEET_INFO, col_names = FALSE)
    if (ncol(info) >= 2L) {
      v <- info[[2]][match(RV_INFO_KEYS, info[[1]])]
      v[is.na(v)] <- ""
      out$info <- stats::setNames(v, names(RV_INFO_KEYS))
    }
  }
  rename <- function(df, map) {
    hit <- match(names(df), map)
    names(df)[!is.na(hit)] <- names(map)[hit[!is.na(hit)]]
    df$excel_row <- seq_len(nrow(df)) + 1L
    df
  }
  if (RV_SHEET_Q %in% sheets) out$questions <- rename(rv_read_sheet(path, RV_SHEET_Q), RV_Q_COLS)
  if (RV_SHEET_NEW %in% sheets) out$new <- rename(rv_read_sheet(path, RV_SHEET_NEW), RV_NEW_COLS)
  # 「放在哪一列之後」老師常寫變項名稱（weighty）或小寫列號（q012）：換成題目表的列號
  q <- out$questions
  nw <- out$new
  if (!is.null(q) && !is.null(nw) && all(c("rowid", "varname") %in% names(q)) &&
      "after" %in% names(nw) && nrow(nw)) {
    a <- trimws(cb_blank(nw$after))
    rid <- cb_blank(q$rowid)
    by_id <- match(toupper(a), toupper(rid))
    by_var <- match(tolower(a), tolower(cb_blank(q$varname)))
    swap <- nzchar(a) & !a %in% rid & (!is.na(by_id) | !is.na(by_var))
    a[swap] <- ifelse(!is.na(by_id[swap]), rid[by_id[swap]], rid[by_var[swap]])
    out$new$after <- a
  }
  out
}

# 選項 ----

#' 「1=有; 2=沒有」→ data.frame(code, label, order)；寫錯時 attr(, "error") 是說明
#' @noRd
rv_parse_values <- function(x) {
  x <- cb_blank(x)
  # 原文：[;；\r\n]+
  parts <- trimws(unlist(strsplit(x, "[;\uff1b\r\n]+")))
  parts <- parts[nzchar(parts)]
  res <- data.frame(code = character(0), label = character(0), order = character(0),
                    stringsAsFactors = FALSE)
  if (!length(parts)) return(res)
  # 原文：^([^=＝]+?)\\s*[=＝]\\s*(.+)$
  m <- regmatches(parts, regexec("^([^=\uff1d]+?)\\s*[=\uff1d]\\s*(.+)$", parts))
  bad <- lengths(m) != 3L
  if (any(bad)) {
    # 原文：看不懂「 | 」，選項要寫成「1=有; 2=沒有」。
    attr(res, "error") <- paste0("\u770b\u4e0d\u61c2\u300c", parts[bad][1], "\u300d\uff0c\u9078\u9805\u8981\u5beb\u6210\u300c1=\u6709; 2=\u6c92\u6709\u300d\u3002")
    return(res)
  }
  code <- trimws(vapply(m, `[`, "", 2L))
  label <- trimws(vapply(m, `[`, "", 3L))
  if (any(grepl("|", code, fixed = TRUE))) {
    # 原文：選項代碼不能有「|」。
    attr(res, "error") <- "\u9078\u9805\u4ee3\u78bc\u4e0d\u80fd\u6709\u300c|\u300d\u3002"
    return(res)
  }
  if (anyDuplicated(code)) {
    # 原文：選項代碼「 | 」重複了。
    attr(res, "error") <- paste0("\u9078\u9805\u4ee3\u78bc\u300c", code[duplicated(code)][1], "\u300d\u91cd\u8907\u4e86\u3002")
    return(res)
  }
  data.frame(code = code, label = label, order = as.character(seq_along(code)),
             stringsAsFactors = FALSE)
}

# 健康檢查 ----

rv_problem <- function(severity, sheet, row, column, message) {
  data.frame(severity = severity, sheet = sheet, row = as.integer(row), column = column,
             message = message, stringsAsFactors = FALSE)
}

rv_empty_problems <- function() rv_problem(character(0), character(0), integer(0),
                                           character(0), character(0))

# 一列題目表：要做什麼（沒選 = 不變）
rv_action_key <- function(a) {
  a <- cb_blank(a)
  key <- names(RV_ACTIONS)[match(a, RV_ACTIONS)]
  key[!nzchar(a)] <- "keep"
  key
}

#' Health check of a filled revision template
#'
#' Finds what would make the import wrong or surprising: row ids that were changed,
#' deleted, duplicated or are not in the template, a template made from another bank
#' version, new questions missing required fields, options that cannot be read, variable
#' names or item IDs already used, actions that do not match what was written, and a
#' template with no changes at all.
#'
#' @param path A filled `.xlsx` made with [revision_template()].
#' @param kit The kit the template was made from, e.g. [read_kit()].
#' @param version The current bank version, compared with the one on the 說明 sheet.
#' @return A data frame, one row per problem: `severity` ("擋下來" = must be fixed before
#'   import; "要注意" = will be imported, but check it), `sheet`, `row` (Excel row; `NA`
#'   for the whole sheet), `column`, `message` (plain Chinese). Zero rows = nothing found.
#' @export
check_revision_xlsx <- function(path, kit, version = codebook_bank_version()) {
  rv_check(read_revision_xlsx(path), kit, version)$problems
}

#' 健康檢查的本體：回傳 problems 和整理好的內容（給 revision_patch() 用）
#' @noRd
rv_check <- function(x, kit, version = codebook_bank_version()) {
  out <- list()
  add <- function(sev, sheet, row, column, msg) {
    # paste0() 遇到長度 0 的列號還是會回傳一句，所以用列號判斷有沒有問題
    if (length(msg) && length(row)) out[[length(out) + 1L]] <<- rv_problem(sev, sheet, row, column, msg)
  }
  done <- function(base = NULL, q = NULL, nw = NULL) {
    p <- if (length(out)) do.call(rbind, out) else rv_empty_problems()
    p <- p[order(match(p$severity, c(RV_BLOCK, RV_NOTE)),
                 match(p$sheet, c(RV_SHEET_INFO, RV_SHEET_Q, RV_SHEET_NEW)), p$row), , drop = FALSE]
    rownames(p) <- NULL
    list(problems = p, base = base, questions = q, new = nw)
  }

  # 工作表和說明 ----
  for (s in c(RV_SHEET_INFO, RV_SHEET_Q)) {
    # 原文：找不到「 | 」表；請用原本的底稿填。
    if (!s %in% x$sheets) add(RV_BLOCK, s, NA, "", paste0("\u627e\u4e0d\u5230\u300c", s, "\u300d\u8868\uff1b\u8acb\u7528\u539f\u672c\u7684\u5e95\u7a3f\u586b\u3002"))
  }
  if (length(out)) return(done())
  info <- x$info
  if (is.null(info) || !nzchar(info[["dataset"]]) || !nzchar(info[["form"]])) {
    # 原文：「說明」表被改過，讀不到題本和資料檔；請用原本的底稿填。
    add(RV_BLOCK, RV_SHEET_INFO, NA, "", "\u300c\u8aaa\u660e\u300d\u8868\u88ab\u6539\u904e\uff0c\u8b80\u4e0d\u5230\u984c\u672c\u548c\u8cc7\u6599\u6a94\uff1b\u8acb\u7528\u539f\u672c\u7684\u5e95\u7a3f\u586b\u3002")
    return(done())
  }
  if (!info[["dataset"]] %in% kit$datasets$datasetID) {
    add(RV_BLOCK, RV_SHEET_INFO, NA, RV_INFO_KEYS[["dataset"]],
        # 原文：題庫裡已經沒有資料檔  | ；底稿太舊了，請重新下載。
        paste0("\u984c\u5eab\u88e1\u5df2\u7d93\u6c92\u6709\u8cc7\u6599\u6a94 ", info[["dataset"]], "\uff1b\u5e95\u7a3f\u592a\u820a\u4e86\uff0c\u8acb\u91cd\u65b0\u4e0b\u8f09\u3002"))
    return(done())
  }
  if (!identical(info[["version"]], version)) {
    add(RV_NOTE, RV_SHEET_INFO, NA, RV_INFO_KEYS[["version"]],
        # 原文：底稿是題庫  |  做的，現在是 
        paste0("\u5e95\u7a3f\u662f\u984c\u5eab ", info[["version"]], " \u505a\u7684\uff0c\u73fe\u5728\u662f ", version,
               # 原文：。下面對不起來的列會擋下來；都對得起來就可以匯入。
               "\u3002\u4e0b\u9762\u5c0d\u4e0d\u8d77\u4f86\u7684\u5217\u6703\u64cb\u4e0b\u4f86\uff1b\u90fd\u5c0d\u5f97\u8d77\u4f86\u5c31\u53ef\u4ee5\u532f\u5165\u3002"))
  }
  base <- revision_base(kit, info[["form"]], datasetID = info[["dataset"]])

  # 題目表 ----
  q <- x$questions
  need <- names(RV_Q_COLS)
  miss <- setdiff(need, names(q))
  if (length(miss)) {
    # 原文：、
    add(RV_BLOCK, RV_SHEET_Q, 1L, paste(RV_Q_COLS[miss], collapse = "\u3001"),
        # 原文：「題目」表少了欄： | 、 | 。請不要刪欄或改欄名。
        paste0("\u300c\u984c\u76ee\u300d\u8868\u5c11\u4e86\u6b04\uff1a", paste(RV_Q_COLS[miss], collapse = "\u3001"), "\u3002\u8acb\u4e0d\u8981\u522a\u6b04\u6216\u6539\u6b04\u540d\u3002"))
    return(done(base))
  }
  filled <- Reduce(`|`, lapply(q[setdiff(need, c("sys"))], nzchar))
  q <- q[filled, , drop = FALSE]
  bq <- base$questions
  expect_sys <- paste(bq$item_uid, bq$itemVer, sep = "|")
  rid <- q$rowid
  # 原文：第  |  列
  r_lab <- function(i) paste0("\u7b2c ", q$excel_row[i], " \u5217")

  blank <- !nzchar(rid)
  add(RV_BLOCK, RV_SHEET_Q, q$excel_row[blank], RV_Q_COLS[["rowid"]],
      # 原文：沒有列號。新題目請寫在「新增題目」表，不要在這裡加列。
      paste0(r_lab(which(blank)), "\u6c92\u6709\u5217\u865f\u3002\u65b0\u984c\u76ee\u8acb\u5beb\u5728\u300c\u65b0\u589e\u984c\u76ee\u300d\u8868\uff0c\u4e0d\u8981\u5728\u9019\u88e1\u52a0\u5217\u3002"))
  dup <- nzchar(rid) & rid %in% rid[duplicated(rid)]
  add(RV_BLOCK, RV_SHEET_Q, q$excel_row[dup], RV_Q_COLS[["rowid"]],
      # 原文：：列號  |  重複了。請不要複製列。
      paste0(r_lab(which(dup)), "\uff1a\u5217\u865f ", rid[dup], " \u91cd\u8907\u4e86\u3002\u8acb\u4e0d\u8981\u8907\u88fd\u5217\u3002"))
  unknown <- nzchar(rid) & !rid %in% bq$rowid
  add(RV_BLOCK, RV_SHEET_Q, q$excel_row[unknown], RV_Q_COLS[["rowid"]],
      # 原文：：列號  |  不是底稿原有的，可能被改過。
      paste0(r_lab(which(unknown)), "\uff1a\u5217\u865f ", rid[unknown], " \u4e0d\u662f\u5e95\u7a3f\u539f\u6709\u7684\uff0c\u53ef\u80fd\u88ab\u6539\u904e\u3002"))
  known <- nzchar(rid) & rid %in% bq$rowid & !dup
  changed_id <- known & q$sys != expect_sys[match(rid, bq$rowid)]
  add(RV_BLOCK, RV_SHEET_Q, q$excel_row[changed_id], RV_Q_COLS[["rowid"]],
      # 原文：：列號 
      paste0(r_lab(which(changed_id)), "\uff1a\u5217\u865f ", rid[changed_id],
             # 原文： 和題目對不起來（列號或系統用欄被改過，或底稿太舊）。
             " \u548c\u984c\u76ee\u5c0d\u4e0d\u8d77\u4f86\uff08\u5217\u865f\u6216\u7cfb\u7d71\u7528\u6b04\u88ab\u6539\u904e\uff0c\u6216\u5e95\u7a3f\u592a\u820a\uff09\u3002"))
  gone <- setdiff(bq$rowid, rid)
  add(RV_BLOCK, RV_SHEET_Q, NA, RV_Q_COLS[["rowid"]],
      # 原文：少了  |  列（ | 、
      if (length(gone)) paste0("\u5c11\u4e86 ", length(gone), " \u5217\uff08", paste(utils::head(gone, 5L), collapse = "\u3001"),
                               # 原文：… | ）。不要刪列；要刪題請在「動作」選「刪除」。
                               if (length(gone) > 5L) "\u2026", "\uff09\u3002\u4e0d\u8981\u522a\u5217\uff1b\u8981\u522a\u984c\u8acb\u5728\u300c\u52d5\u4f5c\u300d\u9078\u300c\u522a\u9664\u300d\u3002"))

  act <- rv_action_key(q$action)
  bad_act <- is.na(act)
  add(RV_BLOCK, RV_SHEET_Q, q$excel_row[bad_act], RV_Q_COLS[["action"]],
      # 原文：：動作「 | 」不是選項之一（
      paste0(r_lab(which(bad_act)), "\uff1a\u52d5\u4f5c\u300c", q$action[bad_act], "\u300d\u4e0d\u662f\u9078\u9805\u4e4b\u4e00\uff08",
             # 原文：、 | ）。
             paste(RV_ACTIONS, collapse = "\u3001"), "\uff09\u3002"))
  act[bad_act] <- "keep"
  has_text <- nzchar(q$new_text)
  has_val <- nzchar(q$new_values)

  i <- which(act == "text" & !has_text)
  add(RV_BLOCK, RV_SHEET_Q, q$excel_row[i], RV_Q_COLS[["new_text"]],
      # 原文：：選了「改文字」，但「新題目文字」是空的。
      paste0(r_lab(i), "\uff1a\u9078\u4e86\u300c\u6539\u6587\u5b57\u300d\uff0c\u4f46\u300c\u65b0\u984c\u76ee\u6587\u5b57\u300d\u662f\u7a7a\u7684\u3002"))
  i <- which(act == "options" & !has_val)
  add(RV_BLOCK, RV_SHEET_Q, q$excel_row[i], RV_Q_COLS[["new_values"]],
      # 原文：：選了「改選項」，但「新選項」是空的。
      paste0(r_lab(i), "\uff1a\u9078\u4e86\u300c\u6539\u9078\u9805\u300d\uff0c\u4f46\u300c\u65b0\u9078\u9805\u300d\u662f\u7a7a\u7684\u3002"))
  i <- which(act == "keep" & (has_text | has_val))
  add(RV_BLOCK, RV_SHEET_Q, q$excel_row[i], RV_Q_COLS[["action"]],
      # 原文：：寫了新的文字或選項，但「動作」是不變。請選「改文字」或「改選項」。
      paste0(r_lab(i), "\uff1a\u5beb\u4e86\u65b0\u7684\u6587\u5b57\u6216\u9078\u9805\uff0c\u4f46\u300c\u52d5\u4f5c\u300d\u662f\u4e0d\u8b8a\u3002\u8acb\u9078\u300c\u6539\u6587\u5b57\u300d\u6216\u300c\u6539\u9078\u9805\u300d\u3002"))
  i <- which(act == "delete" & (has_text | has_val))
  add(RV_NOTE, RV_SHEET_Q, q$excel_row[i], RV_Q_COLS[["action"]],
      # 原文：：動作是「 | 」，寫的新文字或新選項不會用到。
      paste0(r_lab(i), "\uff1a\u52d5\u4f5c\u662f\u300c", RV_ACTIONS[act[i]], "\u300d\uff0c\u5beb\u7684\u65b0\u6587\u5b57\u6216\u65b0\u9078\u9805\u4e0d\u6703\u7528\u5230\u3002"))
  i <- which(act == "text" & has_val)
  add(RV_NOTE, RV_SHEET_Q, q$excel_row[i], RV_Q_COLS[["new_values"]],
      # 原文：：選了「改文字」，也寫了新選項；兩個都會改。
      paste0(r_lab(i), "\uff1a\u9078\u4e86\u300c\u6539\u6587\u5b57\u300d\uff0c\u4e5f\u5beb\u4e86\u65b0\u9078\u9805\uff1b\u5169\u500b\u90fd\u6703\u6539\u3002"))
  i <- which(act == "options" & has_text)
  add(RV_NOTE, RV_SHEET_Q, q$excel_row[i], RV_Q_COLS[["new_text"]],
      # 原文：：選了「改選項」，也寫了新題目文字；兩個都會改。
      paste0(r_lab(i), "\uff1a\u9078\u4e86\u300c\u6539\u9078\u9805\u300d\uff0c\u4e5f\u5beb\u4e86\u65b0\u984c\u76ee\u6587\u5b57\uff1b\u5169\u500b\u90fd\u6703\u6539\u3002"))
  i <- which(act == "text" & has_text & q$new_text == q$label)
  add(RV_NOTE, RV_SHEET_Q, q$excel_row[i], RV_Q_COLS[["new_text"]],
      # 原文：：新題目文字和原本一樣，不會產生新版本。
      paste0(r_lab(i), "\uff1a\u65b0\u984c\u76ee\u6587\u5b57\u548c\u539f\u672c\u4e00\u6a23\uff0c\u4e0d\u6703\u7522\u751f\u65b0\u7248\u672c\u3002"))

  # 選項寫法
  q$values_parsed <- vector("list", nrow(q))
  for (i in which(act %in% c("text", "options") & has_val)) {
    pv <- rv_parse_values(q$new_values[i])
    err <- attr(pv, "error")
    if (!is.null(err)) add(RV_BLOCK, RV_SHEET_Q, q$excel_row[i], RV_Q_COLS[["new_values"]],
                           # 原文：：
                           paste0(r_lab(i), "\uff1a", err))
    q$values_parsed[[i]] <- pv
  }
  # 和原本完全一樣的選項：不算改
  same_val <- vapply(seq_len(nrow(q)), function(i) {
    pv <- q$values_parsed[[i]]
    !is.null(pv) && nrow(pv) > 0L &&
      identical(paste0(pv$code, "=", pv$label, collapse = "; "), q$values[i])
  }, logical(1))
  i <- which(same_val & act == "options")
  add(RV_NOTE, RV_SHEET_Q, q$excel_row[i], RV_Q_COLS[["new_values"]],
      # 原文：：新選項和原本一樣，不會產生新版本。
      paste0(r_lab(i), "\uff1a\u65b0\u9078\u9805\u548c\u539f\u672c\u4e00\u6a23\uff0c\u4e0d\u6703\u7522\u751f\u65b0\u7248\u672c\u3002"))

  # 新增題目 ----
  anchor_ok <- function(a) a == RV_TOP | a %in% bq$rowid
  nw <- x$new
  if (is.null(nw)) {
    nw <- as.data.frame(stats::setNames(rep(list(character(0)), length(RV_NEW_COLS) + 1L),
                                        c(names(RV_NEW_COLS), "excel_row")),
                        stringsAsFactors = FALSE)
  } else {
    miss <- setdiff(names(RV_NEW_COLS), names(nw))
    if (length(miss)) {
      # 原文：、
      add(RV_BLOCK, RV_SHEET_NEW, 1L, paste(RV_NEW_COLS[miss], collapse = "\u3001"),
          # 原文：「新增題目」表少了欄： | 、 | 。
          paste0("\u300c\u65b0\u589e\u984c\u76ee\u300d\u8868\u5c11\u4e86\u6b04\uff1a", paste(RV_NEW_COLS[miss], collapse = "\u3001"), "\u3002"))
      return(done(base, q))
    }
    nw <- nw[Reduce(`|`, lapply(nw[names(RV_NEW_COLS)], nzchar)), , drop = FALSE]
  }
  # 原文：新增題目第  |  列
  n_lab <- function(i) paste0("\u65b0\u589e\u984c\u76ee\u7b2c ", nw$excel_row[i], " \u5217")
  for (cc in RV_NEW_REQUIRED) {
    i <- which(!nzchar(nw[[cc]]))
    add(RV_BLOCK, RV_SHEET_NEW, nw$excel_row[i], RV_NEW_COLS[[cc]],
        # 原文：：「 | 」一定要填。
        paste0(n_lab(i), "\uff1a\u300c", RV_NEW_COLS[[cc]], "\u300d\u4e00\u5b9a\u8981\u586b\u3002"))
  }
  i <- which(nzchar(nw$varname) & !grepl("^[A-Za-z][A-Za-z0-9_]*$", nw$varname))
  add(RV_BLOCK, RV_SHEET_NEW, nw$excel_row[i], RV_NEW_COLS[["varname"]],
      # 原文：：變項名稱「 | 」只能用英文字母、數字和底線，而且英文字母開頭。
      paste0(n_lab(i), "\uff1a\u8b8a\u9805\u540d\u7a31\u300c", nw$varname[i], "\u300d\u53ea\u80fd\u7528\u82f1\u6587\u5b57\u6bcd\u3001\u6578\u5b57\u548c\u5e95\u7dda\uff0c\u800c\u4e14\u82f1\u6587\u5b57\u6bcd\u958b\u982d\u3002"))
  i <- which(nzchar(nw$varname) & nw$varname %in% nw$varname[duplicated(nw$varname)])
  add(RV_BLOCK, RV_SHEET_NEW, nw$excel_row[i], RV_NEW_COLS[["varname"]],
      # 原文：：變項名稱「 | 」重複了。
      paste0(n_lab(i), "\uff1a\u8b8a\u9805\u540d\u7a31\u300c", nw$varname[i], "\u300d\u91cd\u8907\u4e86\u3002"))
  ins <- base$dataset$instrument
  used_uid <- tolower(paste0(ins, ".", nw$varname)) %in% tolower(kit$items$item_uid)
  i <- which(nzchar(nw$varname) & used_uid)
  add(RV_BLOCK, RV_SHEET_NEW, nw$excel_row[i], RV_NEW_COLS[["varname"]],
      # 原文：：題號  |  題庫已經用過，不能重用；新題目請換一個變項名稱。
      paste0(n_lab(i), "\uff1a\u984c\u865f ", ins, ".", nw$varname[i], " \u984c\u5eab\u5df2\u7d93\u7528\u904e\uff0c\u4e0d\u80fd\u91cd\u7528\uff1b\u65b0\u984c\u76ee\u8acb\u63db\u4e00\u500b\u8b8a\u9805\u540d\u7a31\u3002"))
  i <- which(nzchar(nw$varname) & !used_uid & tolower(nw$varname) %in% tolower(base$vars$varname))
  add(RV_BLOCK, RV_SHEET_NEW, nw$excel_row[i], RV_NEW_COLS[["varname"]],
      # 原文：：資料檔裡已經有變項  | 。
      paste0(n_lab(i), "\uff1a\u8cc7\u6599\u6a94\u88e1\u5df2\u7d93\u6709\u8b8a\u9805 ", nw$varname[i], "\u3002"))
  i <- which(nzchar(nw$after) & !anchor_ok(nw$after))
  add(RV_BLOCK, RV_SHEET_NEW, nw$excel_row[i], RV_NEW_COLS[["after"]],
      # 原文：：「 | 」不是「題目」表的列號或變項名稱；請寫像 Q012 的列號、表裡的變項名稱，或「 | 」。
      paste0(n_lab(i), "\uff1a\u300c", nw$after[i], "\u300d\u4e0d\u662f\u300c\u984c\u76ee\u300d\u8868\u7684\u5217\u865f\u6216\u8b8a\u9805\u540d\u7a31\uff1b\u8acb\u5beb\u50cf Q012 \u7684\u5217\u865f\u3001\u8868\u88e1\u7684\u8b8a\u9805\u540d\u7a31\uff0c\u6216\u300c", RV_TOP, "\u300d\u3002"))
  i <- which(nzchar(nw$label) & !nzchar(nw$values))
  add(RV_NOTE, RV_SHEET_NEW, nw$excel_row[i], RV_NEW_COLS[["values"]],
      # 原文：：沒有選項，會當成開放題。
      paste0(n_lab(i), "\uff1a\u6c92\u6709\u9078\u9805\uff0c\u6703\u7576\u6210\u958b\u653e\u984c\u3002"))
  nw$values_parsed <- vector("list", nrow(nw))
  for (i in which(nzchar(nw$values))) {
    pv <- rv_parse_values(nw$values[i])
    err <- attr(pv, "error")
    if (!is.null(err)) add(RV_BLOCK, RV_SHEET_NEW, nw$excel_row[i], RV_NEW_COLS[["values"]],
                           # 原文：：
                           paste0(n_lab(i), "\uff1a", err))
    nw$values_parsed[[i]] <- pv
  }

  # 什麼都沒改 ----
  q$act <- act
  changed <- (act == "text" & !(q$new_text == q$label & !has_val)) |
    (act == "options" & !same_val) | act == "delete"
  if (!any(changed) && nrow(nw) == 0L) {
    # 原文：底稿裡沒有任何改動。
    add(RV_NOTE, RV_SHEET_Q, NA, "", "\u5e95\u7a3f\u88e1\u6c92\u6709\u4efb\u4f55\u6539\u52d5\u3002")
  }
  done(base, q, nw)
}

# 匯入：填好的底稿 → 修改包 ----

# 新增一列：add（主鍵）+ 每個有值的欄一列 set
rv_add_row <- function(table, values) {
  sch <- KIT_SCHEMA[[table]]
  values <- lapply(values, function(v) {
    v <- cb_blank(v)
    if (length(v)) v[1] else ""
  })
  key <- paste(vapply(sch$key, function(k) values[[k]] %||% "", ""), collapse = "|")
  rest <- setdiff(names(values), sch$key)
  rest <- rest[vapply(rest, function(k) nzchar(values[[k]]), logical(1))]
  rbind(patch_rows("add", table, key),
        if (length(rest)) patch_rows("set", table, rep(key, length(rest)), rest, "",
                                     unlist(values[rest], use.names = FALSE)))
}

# 施測代號 → 世代、月齡、年級。M03M120G04：世代 M03、120 月齡、四年級
rv_parse_admin <- function(kit, adminID) {
  adminID <- cb_blank(adminID)
  if (length(adminID) != 1L || !nzchar(adminID)) {
    # 原文：要給新施測的代號（adminID），例如 M03M132G05。
    kitDesign::kit_abort("\u8981\u7d66\u65b0\u65bd\u6e2c\u7684\u4ee3\u865f\uff08adminID\uff09\uff0c\u4f8b\u5982 M03M132G05\u3002")
  }
  if (adminID %in% kit$admins$adminID) {
    # 原文：施測代號  |  已經有了；規劃中的施測要用新的代號。
    kitDesign::kit_abort("\u65bd\u6e2c\u4ee3\u865f ", adminID, " \u5df2\u7d93\u6709\u4e86\uff1b\u898f\u5283\u4e2d\u7684\u65bd\u6e2c\u8981\u7528\u65b0\u7684\u4ee3\u865f\u3002")
  }
  m <- regmatches(adminID, regexec("^(M[0-9]{2})M([0-9]{3})(G([0-9]{2}))?$", adminID))[[1]]
  if (length(m) == 0L) {
    # 原文：施測代號  |  的寫法不對：要像 M03M060 或 M03M132G05
    kitDesign::kit_abort("\u65bd\u6e2c\u4ee3\u865f ", adminID, " \u7684\u5beb\u6cd5\u4e0d\u5c0d\uff1a\u8981\u50cf M03M060 \u6216 M03M132G05",
                         # 原文：（世代、M、三位數月齡；國小以後加 G 和兩位數年級）。
                         "\uff08\u4e16\u4ee3\u3001M\u3001\u4e09\u4f4d\u6578\u6708\u9f61\uff1b\u570b\u5c0f\u4ee5\u5f8c\u52a0 G \u548c\u5169\u4f4d\u6578\u5e74\u7d1a\uff09\u3002")
  }
  co <- kit$cohorts$cohortID[match(m[2], kit$cohorts$code)]
  # 原文：沒有世代  | 。
  if (is.na(co)) kitDesign::kit_abort("\u6c92\u6709\u4e16\u4ee3 ", m[2], "\u3002")
  age <- as.integer(m[3])
  grade <- if (nzchar(m[5])) as.integer(m[5]) else NA_integer_
  if (age >= 84L && is.na(grade)) {
    # 原文：施測代號  | ：84 月齡以後要加年級（例如 G01）。
    kitDesign::kit_abort("\u65bd\u6e2c\u4ee3\u865f ", adminID, "\uff1a84 \u6708\u9f61\u4ee5\u5f8c\u8981\u52a0\u5e74\u7d1a\uff08\u4f8b\u5982 G01\uff09\u3002")
  }
  if (age < 84L && !is.na(grade)) {
    # 原文：施測代號  | ：84 月齡以前不加年級。
    kitDesign::kit_abort("\u65bd\u6e2c\u4ee3\u865f ", adminID, "\uff1a84 \u6708\u9f61\u4ee5\u524d\u4e0d\u52a0\u5e74\u7d1a\u3002")
  }
  same <- kit$admins[kit$admins$cohortID == co, , drop = FALSE]
  wave <- ""
  if (nrow(same) && all(cb_int(same$ageMonth) < age, na.rm = TRUE)) {
    w <- cb_int(same$waveNo)
    if (any(!is.na(w))) wave <- as.character(max(w, na.rm = TRUE) + 1L)
  }
  list(adminID = adminID, cohortID = co, ageMonth = as.character(age),
       grade = if (is.na(grade)) "" else as.character(grade), waveNo = wave)
}

#' Turn a filled revision template into a change package
#'
#' Runs [check_revision_xlsx()] first and stops when anything blocks the import. Then
#' builds, as patch rows (see [apply_patch()]): a planned administration (`status =
#' "planned"`), its data file (same form, instrument, respondent and missing-code scheme
#' as the template's data file) and the data file's variables in the new order; for every
#' question whose text or options changed, a new item version and a `revised` relation from
#' the old version (the old version stays); new value sets for new options (an existing
#' value set is reused when the codes and labels are identical); new items (with version 1)
#' for the 新增題目 sheet. Deleted questions are only left out of the new data file: item IDs
#' are never deleted or reused.
#'
#' @param kit The kit the template was made from.
#' @param path The filled `.xlsx`.
#' @param adminID ID of the new planned administration, following the existing pattern
#'   (cohort code, `M`, three-digit age in months, and from 84 months `G` plus a two-digit
#'   grade), e.g. `"M03M132G05"`. It must not exist yet; it is never guessed.
#' @param version Current bank version (see [check_revision_xlsx()]).
#' @param date Date written as `decided_on` of the new relations.
#' @return A list: `patch` (data frame `op`, `table`, `key`, `column`, `from`, `to`),
#'   `problems` (the health check; only "要注意" rows are left), `summary` (named integer
#'   counts: kept, text, options, deleted, new), `adminID` and `datasetID` of the
#'   planned data file, and `base` (the template's data file).
#' @export
revision_patch <- function(kit, path, adminID, version = codebook_bank_version(),
                           date = kit_today()) {
  rv_patch_from_check(kit, rv_check(read_revision_xlsx(path), kit, version), adminID, date)
}

#' 健康檢查過的底稿 → 修改包（有「擋下來」就停）
#' @noRd
rv_patch_from_check <- function(kit, chk, adminID, date = kit_today()) {
  blocking <- chk$problems$severity == RV_BLOCK
  if (any(blocking)) {
    # 原文：底稿有  |  個問題要先改好才能匯入：
    kitDesign::kit_abort("\u5e95\u7a3f\u6709 ", sum(blocking), " \u500b\u554f\u984c\u8981\u5148\u6539\u597d\u624d\u80fd\u532f\u5165\uff1a",
                         paste(utils::head(chk$problems$message[blocking], 3L), collapse = " "))
  }
  ad <- rv_parse_admin(kit, adminID)
  res <- rv_build_patch(kit, chk, ad, date)
  res$problems <- rbind(res$problems, rv_admin_notes(kit, chk$base, ad))
  res
}

# 新施測的月齡不在題本適用的範圍：提醒（不擋）
rv_admin_notes <- function(kit, base, ad) {
  fm <- base$form
  lo <- cb_int(fm$ageFrom)
  hi <- cb_int(fm$ageTo)
  age <- cb_int(ad$ageMonth)
  if ((!is.na(lo) && age < lo) || (!is.na(hi) && age > hi)) {
    return(rv_problem(RV_NOTE, RV_SHEET_INFO, NA, RV_INFO_KEYS[["form"]], paste0(
      # 原文：新施測  |  是  |  月齡，不在題本  |  適用的 
      "\u65b0\u65bd\u6e2c ", ad$adminID, " \u662f ", age, " \u6708\u9f61\uff0c\u4e0d\u5728\u984c\u672c ", fm$formID, " \u9069\u7528\u7684 ",
      # 原文：– |  月齡內；請確認代號沒有打錯。
      cb_blank(fm$ageFrom), "\u2013", cb_blank(fm$ageTo), " \u6708\u9f61\u5167\uff1b\u8acb\u78ba\u8a8d\u4ee3\u865f\u6c92\u6709\u6253\u932f\u3002")))
  }
  rv_empty_problems()
}

rv_build_patch <- function(kit, chk, ad, date = kit_today()) {
  base <- chk$base
  q <- chk$questions
  nw <- chk$new
  bds <- base$dataset
  bq <- base$questions
  out <- list()
  add <- function(x) out[[length(out) + 1L]] <<- x

  # 施測、資料檔 ----
  add(rv_add_row("admins", list(
    adminID = ad$adminID, cohortID = ad$cohortID, ageMonth = ad$ageMonth, grade = ad$grade,
    waveNo = ad$waveNo, status = "planned",
    # 原文：改版底稿匯入（根據  | ）
    note = paste0("\u6539\u7248\u5e95\u7a3f\u532f\u5165\uff08\u6839\u64da ", bds$datasetID, "\uff09"))))
  suffix <- substring(bds$datasetID, nchar(bds$adminID) + 1L)
  if (!nzchar(suffix)) suffix <- paste0("-", bds$instrument)
  dsid <- paste0(ad$adminID, suffix)
  # 原文：資料檔  |  已經有了。
  if (dsid %in% kit$datasets$datasetID) kitDesign::kit_abort("\u8cc7\u6599\u6a94 ", dsid, " \u5df2\u7d93\u6709\u4e86\u3002")
  add(rv_add_row("datasets", list(
    datasetID = dsid, adminID = ad$adminID, file = "planned", instrument = bds$instrument,
    formID = bds$formID, respondent = bds$respondent, missingScheme = bds$missingScheme,
    # 原文：規劃中；由  |  的改版底稿產生
    note = paste0("\u898f\u5283\u4e2d\uff1b\u7531 ", bds$datasetID, " \u7684\u6539\u7248\u5e95\u7a3f\u7522\u751f"))))

  # 選項組：一樣的就沿用，不一樣的開新的 ----
  vc <- kit$value_codes
  vc <- vc[order(vc$valueSetID, cb_int(vc$order), vc$code), , drop = FALSE]
  sig_of <- function(code, label) paste0(code, "=", label, collapse = "\r")
  known_sig <- if (nrow(vc)) vapply(split(seq_len(nrow(vc)), vc$valueSetID), function(i) {
    sig_of(vc$code[i], vc$label[i])
  }, "") else character(0)
  num <- suppressWarnings(as.integer(sub("^vs", "", vc$valueSetID[grepl("^vs[0-9]+$", vc$valueSetID)])))
  next_vs <- if (length(num)) max(num) + 1L else 1L
  value_set <- function(pv) {
    s <- sig_of(pv$code, pv$label)
    hit <- names(known_sig)[known_sig == s]
    if (length(hit)) return(hit[1])
    id <- sprintf("vs%04d", next_vs)
    next_vs <<- next_vs + 1L
    known_sig[[id]] <<- s
    for (j in seq_len(nrow(pv))) {
      add(rv_add_row("value_codes", list(valueSetID = id, code = pv$code[j],
                                         label = pv$label[j], order = pv$order[j])))
    }
    id
  }

  # 改了文字或選項的題目：新版本 + revised ----
  iv <- kit$item_versions
  new_ver <- list()   # rowid → 新版本號
  tgt <- q[q$act %in% c("text", "options"), , drop = FALSE]
  n_text <- 0L
  n_opt <- 0L
  for (i in seq_len(nrow(tgt))) {
    r <- tgt[i, , drop = FALSE]
    b <- bq[bq$rowid == r$rowid, , drop = FALSE]
    old <- iv[iv$item_uid == b$item_uid & iv$itemVer == b$itemVer, , drop = FALSE]
    pv <- r$values_parsed[[1]]
    text_new <- nzchar(r$new_text) && r$new_text != r$label
    val_new <- !is.null(pv) && nrow(pv) > 0L &&
      !identical(paste0(pv$code, "=", pv$label, collapse = "; "), r$values)
    if (!text_new && !val_new) next
    n_text <- n_text + text_new
    n_opt <- n_opt + val_new
    vers <- cb_int(iv$itemVer[iv$item_uid == b$item_uid])
    nv <- as.character(max(c(vers, 0L), na.rm = TRUE) + 1L)
    vs <- if (val_new) value_set(pv) else cb_blank(old$valueSetID)
    # 原文：改文字 | 改選項
    what <- c(if (text_new) "\u6539\u6587\u5b57", if (val_new) "\u6539\u9078\u9805")
    add(rv_add_row("item_versions", list(
      item_uid = b$item_uid, itemVer = nv,
      label = if (text_new) r$new_text else cb_blank(old$label),
      stem = old$stem, valueSetID = vs, itemType = old$itemType, period = old$period,
      # 原文：改版底稿（ | 、 | ）
      note = paste0("\u6539\u7248\u5e95\u7a3f\uff08", paste(what, collapse = "\u3001"), "\uff09",
                    # 原文：：
                    if (nzchar(r$note)) paste0("\uff1a", r$note) else ""))))
    add(rv_add_row("item_relations", list(
      from_uid = b$item_uid, from_ver = b$itemVer, to_uid = b$item_uid, to_ver = nv,
      kind = "revised", effective_admin = ad$adminID,
      # 原文：改版底稿： | 、
      reason = paste0("\u6539\u7248\u5e95\u7a3f\uff1a", paste(what, collapse = "\u3001"),
                      # 原文：；
                      if (nzchar(r$note)) paste0("\uff1b", r$note) else ""),
      decided_by = "revision_xlsx", decided_on = format(as.Date(date), "%Y-%m-%d"))))
    iv <- rbind(iv, conform_kit_table(data.frame(item_uid = b$item_uid, itemVer = nv,
                                                 stringsAsFactors = FALSE), "item_versions"))
    new_ver[[r$rowid]] <- nv
  }

  # 新題目 ----
  ins <- bds$instrument
  it <- kit$items
  for (i in seq_len(nrow(nw))) {
    r <- nw[i, , drop = FALSE]
    uid <- paste0(ins, ".", r$varname)
    anchor <- if (r$after %in% bq$rowid) bq$item_uid[bq$rowid == r$after] else ""
    a_item <- it[it$item_uid == anchor, , drop = FALSE]
    pv <- r$values_parsed[[1]]
    has_vals <- !is.null(pv) && nrow(pv) > 0L
    add(rv_add_row("items", list(
      item_uid = uid, instrument = ins, varname = r$varname,
      block = if (nzchar(r$block)) r$block else cb_blank(a_item$block),
      module = cb_blank(a_item$module), source_type = "questionnaire", deprecated = "0",
      # 原文：改版底稿新增 | ：
      note = paste0("\u6539\u7248\u5e95\u7a3f\u65b0\u589e", if (nzchar(r$note)) paste0("\uff1a", r$note) else ""))))
    add(rv_add_row("item_versions", list(
      item_uid = uid, itemVer = "1", label = r$label,
      valueSetID = if (has_vals) value_set(pv) else "",
      itemType = if (has_vals) "MC" else "CR")))
  }

  # 新資料檔的變項：照新的順序 ----
  vars <- base$vars
  rid_of <- stats::setNames(bq$rowid, bq$varname)
  vars$rowid <- unname(rid_of[vars$varname])
  vars$rowid[is.na(vars$rowid)] <- ""
  act_of <- stats::setNames(q$act, q$rowid)
  act_v <- unname(act_of[vars$rowid])
  act_v[is.na(act_v)] <- "keep"

  tok <- paste0("v", seq_len(nrow(vars)))          # 原有變項的代號；新題目是 n1、n2…
  rid_tok <- stats::setNames(tok, vars$rowid)
  rid_tok <- rid_tok[nzchar(names(rid_tok))]
  ord <- tok
  # 新題目放在指定那一列之後；同一列之後有好幾題就照新增題目表的順序。
  # 「開頭」= 第一題的前面（系統變項的位置不動）
  last_after <- list()
  for (i in seq_len(nrow(nw))) {
    a <- nw$after[i]
    k <- last_after[[a]]
    pos <- if (!is.null(k)) match(k, ord) else if (a == RV_TOP) {
      first_item <- which(ord %in% rid_tok)
      if (length(first_item)) first_item[1] - 1L else length(ord)
    } else match(rid_tok[[a]], ord)
    ord <- append(ord, paste0("n", i), after = pos)
    last_after[[a]] <- paste0("n", i)
  }
  ord <- ord[!ord %in% tok[act_v == "delete"]]

  dv <- lapply(seq_along(ord), function(k) {
    t <- ord[k]
    if (startsWith(t, "n")) {
      r <- nw[as.integer(substring(t, 2L)), , drop = FALSE]
      return(list(datasetID = dsid, varname = tolower(r$varname),
                  item_uid = paste0(ins, ".", r$varname), itemVer = "1", kind = "item",
                  order = as.character(k)))
    }
    v <- vars[as.integer(substring(t, 2L)), , drop = FALSE]
    nv <- if (nzchar(v$rowid)) new_ver[[v$rowid]] else NULL
    list(datasetID = dsid, varname = v$varname, item_uid = v$item_uid,
         itemVer = nv %||% v$itemVer, kind = v$kind,
         position = cb_blank(v$position),
         order = as.character(k),
         public = if (isTRUE(v$public)) "" else "0")
  })
  for (x in dv) add(rv_add_row("dataset_vars", x))

  patch <- do.call(rbind, out)
  rownames(patch) <- NULL
  list(
    patch = patch,
    problems = chk$problems,
    summary = c(kept = sum(q$act == "keep"), text = n_text, options = n_opt,
                deleted = sum(q$act == "delete"), new = nrow(nw)),
    adminID = ad$adminID, datasetID = dsid, base = bds$datasetID
  )
}

#' The sheets of a revision change package
#'
#' Same layout as the edit page's change package, so [read_patch_xlsx()] reads it back:
#' 說明, 改動 (the patch rows) and 健康檢查 (the remaining notes).
#'
#' @param res The result of [revision_patch()].
#' @param base_version Bank version the template was made from.
#' @return A named list of data frames, ready for `writexl::write_xlsx()`.
#' @export
revision_patch_workbook <- function(res, base_version = codebook_bank_version()) {
  s <- res$summary
  info <- data.frame(
    # 原文：項目
    "\u9805\u76ee" = c("kitItemBank", "kind", "base_version", "date", "adminID", "datasetID",
             "based_on", "n_patch", "kept", "text", "options", "deleted", "new"),
    # 原文：內容 | 0.3 修改包（改版底稿）
    "\u5167\u5bb9" = c("0.3 \u4fee\u6539\u5305\uff08\u6539\u7248\u5e95\u7a3f\uff09", "revision", base_version %||% "", format(kit_today()),
             res$adminID, res$datasetID, res$base, nrow(res$patch),
             s[["kept"]], s[["text"]], s[["options"]], s[["deleted"]], s[["new"]]),
    check.names = FALSE, stringsAsFactors = FALSE)
  pr <- res$problems
  # 原文：嚴重度 | 工作表 | 列 | 欄 | 說明
  names(pr) <- c("\u56b4\u91cd\u5ea6", "\u5de5\u4f5c\u8868", "\u5217", "\u6b04", "\u8aaa\u660e")
  sheets <- list(info, res$patch, pr)
  # 原文：說明 | 改動 | 健康檢查
  names(sheets) <- c("\u8aaa\u660e", "\u6539\u52d5", "\u5065\u5eb7\u6aa2\u67e5")
  sheets
}
