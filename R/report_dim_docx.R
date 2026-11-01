# R/report_dim_docx.R — 構面報告 Word 輸出
#
# report_payload 是 dim_report_data() 的結果：list(domain, wave, dims, n_items, problems, …)
# 段落樣式見 report_docx.R 的 kit_docx_styles()。

#' 產生構面報告 Word
#' @param report_payload dim_report_data() 的結果
#' @param out_path 輸出 .docx 路徑
#' @noRd
generate_dim_report_docx <- function(report_payload, out_path) {
  domain <- report_payload$domain
  wave   <- report_payload$wave
  dims   <- report_payload$dims
  domain_txt <- bank_domain_label(domain)
  if (length(domain_txt) == 0L || is.na(domain_txt)) domain_txt <- domain

  doc <- kit_docx_styles(officer::read_docx())
  add <- function(text, style = "Normal") {
    doc <<- officer::body_add_par(doc, as.character(text), style = style)
  }
  # 原文：KIT 構面題項對應報告
  add("KIT \u69cb\u9762\u984c\u9805\u5c0d\u61c9\u5831\u544a", KIT_DOCX_TITLE)
  # 原文：題庫： |   \u00b7  波次：
  add(paste0("\u984c\u5eab\uff1a", domain_txt, "  \u00b7  \u6ce2\u6b21\uff1a", wave,
             # 原文： 個構面
             "  \u00b7  ", length(dims), " \u500b\u69cb\u9762",
             # 原文：、 |  題
             if (!is.null(report_payload$n_items)) paste0("\u3001", report_payload$n_items, " \u984c") else ""))
  # 原文：產生時間：
  add(paste0("\u7522\u751f\u6642\u9593\uff1a", format(Sys.time(), "%Y-%m-%d %H:%M")))

  warn <- dim_report_warnings(report_payload)
  if (length(warn)) {
    # 原文：要注意
    add("\u8981\u6ce8\u610f", KIT_DOCX_HEADING)
    for (w in warn) add(paste0("\u2022 ", w))
  }

  for (dim_info in dims) {
    add(format_dim_report_heading(dim_info$dimNameRaw %||% dim_info$dimID,
                                  dim_info$alias %||% ""), KIT_DOCX_HEADING)
    add(paste0(report_rel_text(dim_info$reliability), "  \u00b7  ",
               # 原文： 題
               nrow(dim_info$items), " \u984c"))
    note <- clean_meta_str(dim_info$note)
    # 原文：備註：
    if (nzchar(note)) add(paste0("\u5099\u8a3b\uff1a", note))
    items <- dim_info$items
    if (!is.null(items) && nrow(items) > 0L) {
      # officer 原生表格：比 flextable 快很多（flextable 每張表約 0.06 秒，80 張就 5 秒）
      tbl <- data.frame(itemID = as.character(items$itemID),
                        # 原文：題目
                        "\u984c\u76ee" = as.character(items$label),
                        check.names = FALSE, stringsAsFactors = FALSE)
      doc <- officer::body_add_table(doc, tbl, style = "table_template")
    }
  }

  print(doc, target = out_path)
  invisible(out_path)
}
