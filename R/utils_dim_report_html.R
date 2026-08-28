# R/utils_dim_report_html.R — 構面報告 HTML（免 Quarto 後備）

#' 產生構面報告 HTML 檔（Shiny 內可直接使用，無需 Quarto CLI）
generate_dim_report_html <- function(report_payload, out_path) {
  domain <- report_payload$domain
  wave   <- report_payload$wave
  dims   <- report_payload$dims

  esc <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x
  }

  dim_sections <- vapply(dims, function(dim_info) {
    dim_title <- clean_meta_str(dim_info$dimName,
                                default = clean_meta_str(dim_info$dimID))
    dim_raw   <- clean_meta_str(dim_info$dimNameRaw, default = dim_title)
    alias_txt <- clean_meta_str(dim_info$alias)

    alias_html <- if (nzchar(alias_txt) && dim_title != dim_raw) {
      paste0("<p class=\"dim-alias\"><em>",
             esc(dim_raw), "</em> · alias: ", esc(alias_txt), "</p>")
    } else ""

    rel_val <- suppressWarnings(as.numeric(dim_info$reliability))
    rel_text <- if (!is.null(dim_info$reliability) && !is.na(rel_val)) {
      sprintf("Cronbach's &alpha; = %.3f", rel_val)
    } else {
      "\u4fe1\u5ea6\u5f85\u586b"
    }

    note_html <- if (!is.null(dim_info$note) &&
                     nzchar(trimws(as.character(dim_info$note)))) {
      paste0("<p><strong>\u5099\u8a3b\uff1a</strong> ", esc(dim_info$note), "</p>")
    } else ""

    items_df <- dim_info$items
    rows <- vapply(seq_len(nrow(items_df)), function(i) {
      paste0(
        "<tr><td>", esc(items_df$itemID[i]), "</td>",
        "<td>", esc(items_df$itemVer[i]), "</td>",
        "<td>", esc(items_df$block[i]), "</td>",
        "<td>", esc(items_df$label[i]), "</td></tr>"
      )
    }, character(1L))

    paste0(
      "<section class=\"dim-block\">",
      "<h2>", esc(dim_title), "</h2>",
      alias_html,
      "<p class=\"rel-badge\">", rel_text, "</p>",
      note_html,
      "<table class=\"kit-items-dt\"><thead><tr>",
      "<th style=\"width:10%\">itemID</th>",
      "<th style=\"width:10%\">\u7248\u672c</th>",
      "<th style=\"width:10%\">\u6a21\u7d44</th>",
      "<th style=\"width:60%\">\u984c\u76ee</th>",
      "</tr></thead><tbody>",
      paste(rows, collapse = "\n"),
      "</tbody></table></section>"
    )
  }, character(1L))

  html <- paste0(
    "<!DOCTYPE html><html lang=\"zh-TW\"><head><meta charset=\"UTF-8\">",
    "<title>KIT \u69cb\u9762\u5831\u544a</title>",
    "<style>",
    "body{font-family:'Microsoft JhengHei','PingFang TC',sans-serif;",
    "margin:24px;font-size:0.72rem;color:#222;}",
    "h1{border-bottom:2px solid #2d7535;padding-bottom:8px;}",
    "h2{color:#2d7535;border-bottom:1px solid #a5d6a7;padding-bottom:4px;",
    "margin-top:1.5rem;}",
    ".meta table{border-collapse:collapse;margin:12px 0;}",
    ".meta td,.meta th{border:1px solid #ccc;padding:6px 10px;}",
    ".rel-badge{background:#e8f5e9;color:#2d7535;border:1px solid #a5d6a7;",
    "border-radius:4px;padding:2px 8px;display:inline-block;}",
    "table.kit-items-dt{border-collapse:collapse;width:100%;margin:8px 0 20px;}",
    "table.kit-items-dt th,table.kit-items-dt td{border:1px solid #ccc;",
    "padding:4px 6px;vertical-align:top;}",
    ".dim-block{margin-bottom:24px;}",
    "</style></head><body>",
    "<h1>KIT \u69cb\u9762\u984c\u9805\u5c0d\u61c9\u5831\u544a</h1>",
    "<div class=\"meta\"><table>",
    "<tr><th>Domain</th><td>", esc(domain), "</td></tr>",
    "<tr><th>\u6ce2\u6b21</th><td>", esc(wave), "</td></tr>",
    "<tr><th>\u69cb\u9762\u6578</th><td>", length(dims), "</td></tr>",
    "<tr><th>\u7522\u751f\u6642\u9593</th><td>",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</td></tr>",
    "</table></div>",
    paste(dim_sections, collapse = "\n"),
    "</body></html>"
  )

  writeLines(html, out_path, useBytes = TRUE)
  invisible(out_path)
}

#' 嘗試 Quarto 渲染；失敗則改用純 R HTML
render_dim_report_file <- function(qmd_path, report_payload, out_path) {
  tmp_rds <- tempfile(fileext = ".rds")
  saveRDS(report_payload, tmp_rds)

  html_name <- basename(out_path)
  out_dir   <- dirname(normalizePath(out_path, mustWork = FALSE))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  quarto_ok <- FALSE
  if (requireNamespace("quarto", quietly = TRUE) && file.exists(qmd_path)) {
    quarto_ok <- tryCatch({
      if (requireNamespace("callr", quietly = TRUE)) {
        callr::r(
          function(qmd, fname, rds) {
            quarto::quarto_render(
              input          = qmd,
              output_file    = fname,
              execute_params = list(data_rds = rds),
              quiet          = TRUE
            )
          },
          args = list(
            qmd  = normalizePath(qmd_path),
            fname = html_name,
            rds  = tmp_rds
          )
        )
      } else {
        owd <- setwd(dirname(normalizePath(qmd_path)))
        on.exit(setwd(owd), add = TRUE)
        quarto::quarto_render(
          input          = basename(qmd_path),
          output_file    = html_name,
          execute_params = list(data_rds = tmp_rds),
          quiet          = TRUE
        )
      }
      qmd_out <- file.path(dirname(normalizePath(qmd_path)), html_name)
      out_norm <- normalizePath(out_path, winslash = "/", mustWork = FALSE)
      qmd_norm <- normalizePath(qmd_out, winslash = "/", mustWork = FALSE)
      if (!identical(qmd_norm, out_norm) && file.exists(qmd_out)) {
        file.copy(qmd_out, out_path, overwrite = TRUE)
        unlink(qmd_out)
      }
      TRUE
    }, error = function(e) {
      message("\u69cb\u9762\u5831\u544a Quarto \u5931\u6557\uff0c\u6539\u7528\u5167\u5efa HTML\uff1a", e$message)
      FALSE
    })
  }

  if (!quarto_ok)
    generate_dim_report_html(report_payload, out_path)

  invisible(out_path)
}
