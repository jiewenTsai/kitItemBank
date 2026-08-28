# R/utils_dim_report_docx.R — 構面報告 Word 輸出

#' 構面題項表格（itemID + label）
make_dim_items_ft <- function(items_df) {
  if (!requireNamespace("flextable", quietly = TRUE))
    stop("需要 flextable 套件")

  df <- data.frame(
    itemID = as.character(items_df$itemID),
    label  = as.character(items_df$label),
    stringsAsFactors = FALSE
  )

  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(ft, itemID = "itemID", label = "\u984c\u76ee")
  ft <- flextable::width(ft, j = 1, width = 1.3, unit = "in")
  ft <- flextable::width(ft, j = 2, width = 5.0, unit = "in")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft <- flextable::align(ft, j = 2, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "body")
  ft <- apply_flex_kit_font(ft, part = "all")
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  ft <- flextable::set_table_properties(ft, layout = "fixed")
  ft
}

#' 產生構面報告 Word
#' @param report_payload list(domain, wave, dims)
#' @param out_path       輸出 .docx 路徑
generate_dim_report_docx <- function(report_payload, out_path) {
  if (!requireNamespace("officer", quietly = TRUE))
    stop("需要 officer 套件")
  if (!requireNamespace("flextable", quietly = TRUE))
    stop("需要 flextable 套件")

  domain <- report_payload$domain
  wave   <- report_payload$wave
  dims   <- report_payload$dims

  doc <- officer::read_docx()

  doc <- doc_add_body_fpar(doc, kit_fpar_plain(
    "KIT \u69cb\u9762\u984c\u9805\u5c0d\u61c9\u5831\u544a",
    fp = kit_fp_heading()
  ))
  doc <- doc_add_body_fpar(doc, kit_fpar_plain(
    paste0("Domain\uff1a", domain, "  \u00b7  \u6ce2\u6b21\uff1a", wave,
           "  \u00b7  \u69cb\u9762\u6578\uff1a", length(dims))
  ))
  doc <- doc_add_body_fpar(doc, kit_fpar_plain(
    paste0("\u7522\u751f\u6642\u9593\uff1a",
           format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))
  doc <- doc_add_body_fpar(doc, kit_fpar_plain(""))

  for (dim_info in dims) {
    heading <- format_dim_report_heading(
      dim_info$dimNameRaw %||% dim_info$dimID,
      dim_info$alias %||% ""
    )
    doc <- doc_add_body_fpar(doc, kit_fpar_plain(heading, fp = kit_fp_heading()))

    rel_val <- suppressWarnings(as.numeric(dim_info$reliability))
    rel_txt <- if (!is.null(dim_info$reliability) && !is.na(rel_val)) {
      paste0("Cronbach's \u03b1 = ", sprintf("%.3f", rel_val))
    } else {
      "\u4fe1\u5ea6\u5f85\u586b"
    }
    doc <- doc_add_body_fpar(doc, kit_fpar_plain(rel_txt))

    items_df <- dim_info$items
    if (!is.null(items_df) && nrow(items_df) > 0L) {
      ft <- make_dim_items_ft(items_df)
      doc <- flextable::body_add_flextable(doc, ft)
    }

    doc <- doc_add_body_fpar(doc, kit_fpar_plain(""))
  }

  print(doc, target = out_path)
  invisible(out_path)
}
