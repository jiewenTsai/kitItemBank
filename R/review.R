# R/review.R — 讀回團隊填好的候選確認表（data-raw/make_review_xlsx.R 產生的 Excel）
#
# 只讀有填的列：判斷／決定／處理 或 意見 不是空白。對應用最右邊的「系統用」欄（或「編號」）。

# 每張表裡給人填的欄（第一個是下拉選單）
# 原文：判斷 | 決定 | 處理 | 意見 | 系統用 | 編號
REVIEW_INPUT_COLS <- c("判斷", "決定", "處理")
REVIEW_COMMENT_COL <- "意見"
REVIEW_KEY_COL <- "系統用"
REVIEW_ID_COL <- "編號"

#' Read back a filled-in candidate review workbook
#'
#' @param file Path to the `.xlsx` made by `data-raw/make_review_xlsx.R`.
#' @return A data frame with one row per filled-in row: `sheet`, `id` (編號), `key`
#'   (系統用, the primary key of the candidate; blank for the decisions sheet),
#'   `decision` and `comment`. Zero rows when nothing was filled in.
#' @export
read_review <- function(file) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    kitDesign::kit_abort("read_review() needs the openxlsx package.")
  }
  out <- lapply(openxlsx::getSheetNames(file), function(s) {
    d <- openxlsx::read.xlsx(file, s, colNames = TRUE, check.names = FALSE)
    jc <- intersect(REVIEW_INPUT_COLS, names(d))
    if (!length(jc) || !nrow(d)) return(NULL)
    txt <- function(v) { v <- as.character(v); v[is.na(v)] <- ""; trimws(v) }
    dec <- txt(d[[jc[1]]])
    com <- if (REVIEW_COMMENT_COL %in% names(d)) txt(d[[REVIEW_COMMENT_COL]]) else rep("", nrow(d))
    keep <- nzchar(dec) | nzchar(com)
    if (!any(keep)) return(NULL)
    data.frame(sheet = s, id = txt(d[[REVIEW_ID_COL]])[keep],
               key = if (REVIEW_KEY_COL %in% names(d)) txt(d[[REVIEW_KEY_COL]])[keep] else "",
               decision = dec[keep], comment = com[keep], stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- data.frame(sheet = character(0), id = character(0), key = character(0),
                      decision = character(0), comment = character(0), stringsAsFactors = FALSE)
  }
  rownames(out) <- NULL
  out
}
