# R/report_dim_html.R — 構面報告 HTML
#
# render_dim_report_file()：有 Quarto（R 套件＋命令列工具）時用 inst/reports/dim_summary_report.qmd，
# 沒有或失敗時用 generate_dim_report_html()（純 R，不需要 Quarto）。
# report_payload 是 dim_report_data() 的結果。

report_esc <- function(x) htmltools::htmlEscape(as.character(x %||% ""))

report_rel_text <- function(rel) {
  v <- suppressWarnings(as.numeric(rel))
  # 原文：Cronbach's α = %.3f | 沒有信度
  if (length(v) == 1L && !is.na(v)) sprintf("Cronbach's \u03b1 = %.3f", v) else "\u6c92\u6709\u4fe1\u5ea6"
}

#' 純 R 產生構面報告 HTML 檔（不需要 Quarto）
#' @noRd
generate_dim_report_html <- function(report_payload, out_path) {
  domain <- report_payload$domain
  wave   <- report_payload$wave
  dims   <- report_payload$dims
  domain_txt <- bank_domain_label(domain)
  if (length(domain_txt) == 0L || is.na(domain_txt)) domain_txt <- domain

  dim_sections <- vapply(dims, function(dim_info) {
    heading <- format_dim_report_heading(
      dim_info$dimNameRaw %||% dim_info$dimID, dim_info$alias %||% "")
    rel_val <- suppressWarnings(as.numeric(dim_info$reliability))
    has_rel <- length(rel_val) == 1L && !is.na(rel_val)
    note <- clean_meta_str(dim_info$note)
    items <- dim_info$items
    rows <- vapply(seq_len(nrow(items)), function(i) {
      paste0("<tr><td>", report_esc(items$itemID[i]), "</td>",
             "<td class=\"num\">", report_esc(items$itemVer[i]), "</td>",
             "<td>", report_esc(items$block[i]), "</td>",
             "<td>", report_esc(items$label[i]), "</td></tr>")
    }, character(1L))
    paste0(
      "<section class=\"dim\">",
      "<h2>", report_esc(heading), "</h2>",
      "<p class=\"rel", if (!has_rel) " none", "\">", report_rel_text(rel_val),
      # 原文： ·  |  題</p>
      " \u00b7 ", nrow(items), " \u984c</p>",
      # 原文：<p>備註：
      if (nzchar(note)) paste0("<p>\u5099\u8a3b\uff1a", report_esc(note), "</p>") else "",
      "<table><thead><tr><th>itemID</th><th>itemVer</th><th>block</th><th>label</th>",
      "</tr></thead><tbody>", paste(rows, collapse = "\n"), "</tbody></table></section>"
    )
  }, character(1L))

  warn <- dim_report_warnings(report_payload)
  warn_html <- if (length(warn)) {
    # 原文：<div class=\"warn\"><b>要注意</b><ul>
    paste0("<div class=\"warn\"><b>\u8981\u6ce8\u610f</b><ul>",
           paste0("<li>", report_esc(warn), "</li>", collapse = ""), "</ul></div>")
  } else ""

  html <- paste0(
    "<!DOCTYPE html><html lang=\"zh-TW\"><head><meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    # 原文：<title>KIT 構面報告 
    "<title>KIT \u69cb\u9762\u5831\u544a ", report_esc(wave), "</title>",
    "<style>",
    "body{font-family:'Chiron GoRound TC','PingFang TC','Microsoft JhengHei',sans-serif;",
    "margin:24px auto;max-width:960px;padding:0 16px;font-size:15px;color:#141414;}",
    "h1{border-bottom:3px solid #141414;padding-bottom:8px;}",
    "h2{margin-top:2rem;border-bottom:2px solid #c6d033;padding-bottom:4px;}",
    ".meta{color:#555;}",
    ".rel{display:inline-block;background:#c6d033;border:2px solid #141414;padding:2px 8px;}",
    ".rel.none{background:#ffde59;}",
    ".warn{background:#ffde59;border:2px solid #141414;padding:8px 12px;margin:12px 0;}",
    "table{border-collapse:collapse;width:100%;margin:8px 0 20px;font-size:14px;}",
    "th{background:#141414;color:#fff;text-align:left;}",
    "th,td{border:1px solid #d9d4c5;padding:4px 6px;vertical-align:top;}",
    "td.num{text-align:right;}",
    "</style></head><body>",
    # 原文：<h1>KIT 構面題項對應報告</h1>
    "<h1>KIT \u69cb\u9762\u984c\u9805\u5c0d\u61c9\u5831\u544a</h1>",
    # 原文：<p class=\"meta\">題庫： |  · 波次：
    "<p class=\"meta\">\u984c\u5eab\uff1a", report_esc(domain_txt), " \u00b7 \u6ce2\u6b21\uff1a", report_esc(wave),
    # 原文： ·  |  個構面 · 
    " \u00b7 ", length(dims), " \u500b\u69cb\u9762 \u00b7 ",
    # 原文： 題 · 產生時間：
    report_payload$n_items %||% "", " \u984c \u00b7 \u7522\u751f\u6642\u9593\uff1a",
    format(Sys.time(), "%Y-%m-%d %H:%M"), "</p>",
    warn_html,
    paste(dim_sections, collapse = "\n"),
    "</body></html>"
  )
  writeLines(enc2utf8(html), out_path, useBytes = TRUE)
  invisible(out_path)
}

# 有沒有 Quarto 可以用（R 套件與命令列工具都要有）
report_has_quarto <- function() {
  requireNamespace("quarto", quietly = TRUE) &&
    !is.null(tryCatch(quarto::quarto_path(), error = function(e) NULL))
}

#' 產生構面報告 HTML：有 Quarto 時用 qmd，否則（或失敗時）用純 R 版本
#' @noRd
render_dim_report_file <- function(report_payload, out_path,
                                   qmd_path = system.file("reports", "dim_summary_report.qmd",
                                                          package = "kitItemBank"),
                                   use_quarto = report_has_quarto()) {
  out_dir <- dirname(normalizePath(out_path, mustWork = FALSE))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  quarto_ok <- FALSE
  if (isTRUE(use_quarto) && nzchar(qmd_path) && file.exists(qmd_path)) {
    quarto_ok <- tryCatch({
      # 在暫存資料夾裡渲染，不動套件裡的檔案
      work <- tempfile("dimreport")
      dir.create(work)
      on.exit(unlink(work, recursive = TRUE), add = TRUE)
      file.copy(qmd_path, file.path(work, "report.qmd"))
      rds <- file.path(work, "data.rds")
      saveRDS(report_payload, rds)
      quarto::quarto_render(file.path(work, "report.qmd"), output_file = "report.html",
                            execute_params = list(data_rds = rds), quiet = TRUE)
      file.copy(file.path(work, "report.html"), out_path, overwrite = TRUE)
    }, error = function(e) {
      # 原文：構面報告 Quarto 停下來，改用內建 HTML：
      message("\u69cb\u9762\u5831\u544a Quarto \u505c\u4e0b\u4f86\uff0c\u6539\u7528\u5167\u5efa HTML\uff1a", conditionMessage(e))
      FALSE
    })
  }
  if (!quarto_ok) generate_dim_report_html(report_payload, out_path)
  invisible(out_path)
}
