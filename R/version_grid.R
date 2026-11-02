# R/version_grid.R — 題目 × 波次，格子寫那一波問的版本（v1、v2…）
#
#   version_grid()           一列一題（不分版本），M03、M36 可以只取一個世代；有世代時另算錨題
#   version_grid_workbook()  跨波次對照總表（Excel）：說明＋一個世代一張工作表
# 構面查詢的「題目 × 波次」、題目查詢的「各版本出現的波次」、總表下載都用這一套。

#' Items × waves, each cell the version asked in that wave
#'
#' One row per item (all versions together). A cell holds the version asked
#' in that wave (`"v1"`, `"v2"`; `"v1/v2"` if two versions were asked), or
#' `""` if the item was not asked. With `cohort`, only that cohort's waves
#' are kept, and `anchor` marks items asked in every one of those waves with
#' the same version.
#'
#' @param bank An item bank from [load_bank()].
#' @param item_ids Item IDs (`itemID`); default all items.
#' @param cohort `""` (all waves) or a cohort (the wave-code prefix), such as
#'   `"M03"`.
#' @param drop_empty Drop items not asked in any of the waves kept.
#' @return A data.frame: `itemID`, `block`, `label` (of the highest
#'   version), `anchor` (`"★"` or `""`; only with `cohort`), then one column
#'   per wave in age order.
#' @examples
#' bank <- load_bank("parent")
#' head(version_grid(bank, cohort = "M03"))
#' @export
version_grid <- function(bank, item_ids = NULL, cohort = "", drop_empty = TRUE) {
  ib <- bank$itemBank
  ids <- unique(as.character(item_ids %||% ib$itemID))
  iw <- bank$itemWave[bank$itemWave$itemID %in% ids, , drop = FALSE]
  waves <- get_wave_cols(bank$itemWave)
  if (nzchar(cohort)) waves <- waves[wave_cohort(waves) == cohort]
  m <- vapply(waves, function(w) {
    vapply(ids, function(id) {
      v <- sort(unique(as.integer(iw$itemVer[iw$itemID == id & iw[[w]] %in% 1L])))
      if (length(v)) paste0("v", v, collapse = "/") else ""
    }, character(1))
  }, character(length(ids)))
  m <- matrix(m, nrow = length(ids), dimnames = list(NULL, waves))
  top <- ib[order(ib$itemID, -as.integer(ib$itemVer)), , drop = FALSE]
  top <- top[match(ids, top$itemID), , drop = FALSE]
  chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
  out <- data.frame(itemID = ids, block = chr(top$block), label = collapse_newlines(chr(top$label)),
                    stringsAsFactors = FALSE)
  if (nzchar(cohort)) {
    out$anchor <- ifelse(apply(m, 1, function(r) length(r) > 0L && all(nzchar(r)) && length(unique(r)) == 1L),
                         # 原文：★
                         "\u2605", "")
  }
  out <- cbind(out, as.data.frame(m, stringsAsFactors = FALSE, optional = TRUE))
  if (drop_empty && length(waves)) out <- out[rowSums(array(nzchar(m), dim(m))) > 0L, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Cross-wave workbook: one sheet per cohort, cells hold the version asked
#'
#' @param bank An item bank from [load_bank()].
#' @param domain The bank's domain, written on the read-me sheet.
#' @return A named list of data.frames for [writexl::write_xlsx()]: `說明`,
#'   then one sheet per cohort (`M03`, `M36`, …), or `全部波次` if the bank
#'   has no cohorts.
#' @export
version_grid_workbook <- function(bank, domain = "") {
  co <- bank_cohorts(bank)
  sheets <- if (length(co)) {
    stats::setNames(lapply(co, function(cn) version_grid(bank, cohort = cn)), co)
  } else {
    # 原文：全部波次
    stats::setNames(list(version_grid(bank)), "\u5168\u90e8\u6ce2\u6b21")
  }
  sheets <- lapply(sheets, version_grid_display)
  readme <- data.frame(
    # 原文：項目 | 題庫 | 題庫版本 | 匯出時間 | 工作表 | 格子 | 錨題
    "\u9805\u76ee" = c("\u984c\u5eab", "\u984c\u5eab\u7248\u672c", "\u532f\u51fa\u6642\u9593", "\u5de5\u4f5c\u8868", "\u683c\u5b50", "\u9328\u984c"),
    # 原文：說明
    "\u8aaa\u660e" = c(domain, kit_version(), format(kit_now(), "%Y-%m-%d %H:%M %Z"),
             # 原文：、
             paste(names(sheets), collapse = "\u3001"),
             # 原文：那一波問的版本（v1、v2…；兩版都問寫 v1/v2）；空白是那一波沒問。一列一題，含已停用的版本。
             "\u90a3\u4e00\u6ce2\u554f\u7684\u7248\u672c\uff08v1\u3001v2\u2026\uff1b\u5169\u7248\u90fd\u554f\u5beb v1/v2\uff09\uff1b\u7a7a\u767d\u662f\u90a3\u4e00\u6ce2\u6c92\u554f\u3002\u4e00\u5217\u4e00\u984c\uff0c\u542b\u5df2\u505c\u7528\u7684\u7248\u672c\u3002",
             # 原文：★：這個世代每一波都問、而且都是同一版，可以當縱貫比較的共同題。
             "\u2605\uff1a\u9019\u500b\u4e16\u4ee3\u6bcf\u4e00\u6ce2\u90fd\u554f\u3001\u800c\u4e14\u90fd\u662f\u540c\u4e00\u7248\uff0c\u53ef\u4ee5\u7576\u7e31\u8cab\u6bd4\u8f03\u7684\u5171\u540c\u984c\u3002"),
    check.names = FALSE, stringsAsFactors = FALSE)
  # 原文：說明
  c(list("\u8aaa\u660e" = readme), sheets)
}

# 欄名換成中文（題號、區塊、題目、錨題；波次欄清掉特殊字元）
#' @noRd
version_grid_display <- function(x) {
  # 原文：題號 | 區塊 | 題目 | 錨題
  zh <- c(itemID = "\u984c\u865f", block = "\u5340\u584a", label = "\u984c\u76ee", anchor = "\u9328\u984c")
  n <- names(x)
  names(x) <- ifelse(n %in% names(zh), zh[n], col_display_labels(n))
  x
}
