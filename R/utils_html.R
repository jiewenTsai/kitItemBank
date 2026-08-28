# R/utils_html.R — 組卷 HTML 預覽 / 匯出

# ── 結構概覽（文字樹狀）────────────────────────────────────────────
build_structure_outline <- function(block_configs, itemBank_df) {
  if (length(block_configs) == 0L) {
    return(list(empty = TRUE, blocks = list()))
  }

  blocks <- lapply(seq_along(block_configs), function(bi) {
    cfg <- block_configs[[bi]]
    scale <- normalize_scale_config(
      cfg$scale_labels,
      cfg$scale_codes,
      cfg$scale_n %||% length(cfg$scale_labels %||% character(0))
    )
    scale_txt <- paste(
      mapply(function(c, l) paste0(c, "=", l), scale$codes, scale$labels,
             USE.NAMES = FALSE),
      collapse = " \u00b7 "
    )

    item_keys <- cfg$items %||% list()
    rows_list <- lapply(item_keys, function(k) {
      get_item_bank_row(itemBank_df, k$itemID, k$itemVer)
    })
    rows_list <- Filter(Negate(is.null), rows_list)
    items_df  <- if (length(rows_list) > 0L) do.call(rbind, rows_list) else NULL

    groups_outline <- if (!is.null(items_df))
      batch_consecutive_likert_groups(
        group_items_for_render(items_df, item_keys),
        block_labels = scale$labels,
        block_codes  = scale$codes
      ) else list()

    group_rows <- lapply(seq_along(groups_outline), function(gi) {
      grp <- groups_outline[[gi]]
      itype <- detect_item_type(
        grp$items$note[1L],
        if ("itemType" %in% names(grp$items)) grp$items$itemType[1L] else NULL,
        if ("value" %in% names(grp$items)) grp$items$value[1L] else NULL
      )
      if (itype %in% c("checkbox", "openended", "numeric", "single_choice",
                       "single_choice_unparsed")) {
        type_zh <- switch(itype,
          checkbox = "\u8907\u9078",
          openended = "\u958b\u653e\u984c",
          numeric = "\u6578\u5024\u984c",
          single_choice = "\u55ae\u9078",
          single_choice_unparsed = "\u55ae\u9078\uff08value\u5f85\u78ba\u8a8d\uff09",
          "\u672a\u77e5"
        )
        lapply(seq_len(nrow(grp$items)), function(j) {
          row <- grp$items[j, , drop = FALSE]
          list(
            itemID = row$itemID[1L], itemVer = row$itemVer[1L],
            label = collapse_newlines(row$label[1L]),
            type_zh = type_zh, layout_zh = type_zh
          )
        })
      } else {
        layout_zh <- layout_label_zh(grp$layout, should_use_matrix_layout(grp))
        lapply(seq_len(nrow(grp$items)), function(j) {
          row <- grp$items[j, , drop = FALSE]
          list(
            itemID = row$itemID[1L], itemVer = row$itemVer[1L],
            label = collapse_newlines(row$label[1L]),
            type_zh = "Likert", layout_zh = layout_zh,
            group_index = gi, is_group_header = (j == 1L)
          )
        })
      }
    })
    item_rows <- unlist(group_rows, recursive = FALSE)

    list(
      index = bi,
      name = cfg$name %||% paste0("\u5340\u584a ", bi),
      prompt = cfg$prompt %||% "",
      instruction = cfg$instruction %||% "",
      scale_text = scale_txt,
      item_count = length(item_rows),
      items = item_rows
    )
  })

  list(empty = FALSE, blocks = blocks)
}

render_structure_outline_ui <- function(outline) {
  if (isTRUE(outline$empty)) {
    return(div(class = "text-muted small", "\u5c1a\u7121\u5340\u584a\u6216\u984c\u76ee\u3002"))
  }

  tagList(lapply(outline$blocks, function(b) {
    tags$div(
      class = "kit-outline-block mb-3",
      tags$div(
        class = "fw-bold",
        paste0(b$index, ". ", b$name),
        tags$span(class = "badge bg-secondary ms-2", paste0(b$item_count, " \u984c"))
      ),
      if (nzchar(trimws(b$prompt)))
        tags$div(class = "small text-muted ms-3", "\u63d0\u793a\u8a9e\uff1a", b$prompt),
      if (nzchar(trimws(b$instruction)))
        tags$div(class = "small text-muted ms-3", "\u7b54\u984c\u8aaa\u660e\uff1a", b$instruction),
      tags$div(class = "small ms-3 mb-1", tags$strong("\u91cf\u5c3a\uff1a"), b$scale_text),
      tags$ol(
        class = "small mb-0",
        lapply(b$items, function(it) {
          lbl <- if (is.null(it$label) || is.na(it$label) || !nzchar(it$label))
            "\uff08\u627e\u4e0d\u5230\u984c\u76ee\u8cc7\u6599\uff09"
          else trunc_label(it$label, 80)
          tags$li(
            tags$code(paste0(it$itemID, " v", it$itemVer)),
            " \u2014 ",
            lbl,
            tags$span(class = "badge bg-light text-dark border ms-1", it$type_zh),
            if (!is.null(it$layout_zh) && it$layout_zh != it$type_zh)
              tags$span(class = "badge bg-info text-dark ms-1", it$layout_zh)
          )
        })
      )
    )
  }))
}

# ── HTML 問卷預覽 / 匯出 ───────────────────────────────────────────
html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

render_likert_scale_html <- function(scale_labels, scale_codes) {
  scale <- normalize_scale_config(scale_labels, scale_codes)
  paste(
    mapply(function(code, lbl) {
      paste0("<span class=\"kit-scale-opt\">",
             html_escape(KIT_CHECKBOX), " ",
             html_escape(code), " ",
             html_escape(lbl), "</span>")
    }, scale$codes, scale$labels, USE.NAMES = FALSE),
    collapse = " "
  )
}

render_choice_options_html <- function(scale_labels, scale_codes, source = "block") {
  if (identical(source, "missing")) {
    return("<span class=\"text-muted\">（本題 value 無法解析，未套用區塊 Likert）</span>")
  }
  cfg    <- normalize_scale_config(scale_labels, scale_codes)
  layout <- choice_options_layout(cfg, source)
  opts <- mapply(function(code, lbl) {
    paste0("<span class=\"kit-scale-opt\">",
           html_escape(KIT_CHECKBOX), " ",
           html_escape(code), " ",
           html_escape(lbl), "</span>")
  }, cfg$codes, cfg$labels, USE.NAMES = FALSE)
  if (identical(layout, "vertical")) {
    paste0("<div class=\"kit-choice-list\">",
           paste(opts, collapse = "<br/>"), "</div>")
  } else {
    paste(opts, collapse = " ")
  }
}

render_likert_matrix_html <- function(items_df, scale_labels, scale_codes,
                                     row_labels = NULL) {
  scale <- normalize_scale_config(scale_labels, scale_codes)

  hdr_codes <- paste0("<th>", html_escape(scale$codes), "</th>", collapse = "")
  hdr_lbls  <- paste0("<th>", html_escape(scale$labels), "</th>", collapse = "")

  body_rows <- vapply(seq_len(nrow(items_df)), function(i) {
    row <- items_df[i, , drop = FALSE]
    if (is.null(row_labels)) {
      lbl <- paste0(
        html_escape(collapse_newlines(row$label[1L])),
        " \u3010", html_escape(row$itemID[1L]), "\u3011")
    } else {
      lbl <- html_escape(row_labels[i])
    }
    cells <- paste0("<td class=\"kit-tick\">", html_escape(KIT_CHECKBOX), "</td>",
                    collapse = "")
    paste0("<tr><td>", lbl, "</td>", cells, "</tr>")
  }, character(1L))

  paste0(
    "<table class=\"kit-q-table kit-q-table-noborder\">",
    "<thead><tr><th></th>", hdr_codes, "</tr>",
    "<tr><th></th>", hdr_lbls, "</tr></thead>",
    "<tbody>", paste(body_rows, collapse = ""), "</tbody></table>"
  )
}

render_block_html <- function(block_cfg, itemBank_df, block_idx = 1L,
                              q_state = list(n = 0L)) {
  prompt   <- trimws(block_cfg$prompt %||% "")
  instr    <- trimws(block_cfg$instruction %||% "")
  scale    <- normalize_scale_config(
    block_cfg$scale_labels,
    block_cfg$scale_codes,
    block_cfg$scale_n
  )

  parts <- c(
    paste0("<section class=\"kit-block\">"),
    paste0("<h2>", html_escape(format_block_title(block_idx, block_cfg$name %||% "\uff08\u672a\u547d\u540d\u5340\u584a\uff09")), "</h2>")
  )
  if (nzchar(prompt)) parts <- c(parts, paste0("<p class=\"kit-prompt\">", html_escape(prompt), "</p>"))
  if (nzchar(instr))  parts <- c(parts, paste0("<p class=\"kit-instr\">", html_escape(instr), "</p>"))

  item_keys <- block_cfg$items %||% list()
  if (length(item_keys) == 0L) {
    parts <- c(parts, "<p class=\"text-muted\">\uff08\u672c\u5340\u584a\u5c1a\u672a\u9078\u984c\uff09</p></section>")
    return(paste0(parts, collapse = "\n"))
  }

  rows_list <- lapply(item_keys, function(k) {
    r <- itemBank_df[itemBank_df$itemID == k$itemID &
                     itemBank_df$itemVer == as.integer(k$itemVer), , drop = FALSE]
    if (nrow(r) == 0L) return(NULL)
    r[1L, , drop = FALSE]
  })
  rows_list <- Filter(Negate(is.null), rows_list)
  if (length(rows_list) == 0L) {
    parts <- c(parts, "<p class=\"text-muted\">\uff08\u627e\u4e0d\u5230\u984c\u76ee\u8cc7\u6599\uff09</p></section>")
    return(paste0(parts, collapse = "\n"))
  }

  items_df <- do.call(rbind, rows_list)
  groups   <- batch_consecutive_likert_groups(
    group_items_for_render(items_df, item_keys),
    block_labels = scale$labels,
    block_codes  = scale$codes
  )

  for (grp in groups) {
    itype <- detect_item_type(
      grp$items$note[1L],
      if ("itemType" %in% names(grp$items)) grp$items$itemType[1L] else NULL,
      if ("value" %in% names(grp$items)) grp$items$value[1L] else NULL
    )

    if (itype == "checkbox") {
      q_state$n <- q_state$n + 1L
      stem <- get_group_stem_text(grp$items, grp$stem_text)
      if (!is.null(stem) && nzchar(stem))
        parts <- c(parts, paste0("<p class=\"kit-stem\">",
                                 html_escape(paste0(q_state$n, ") ", stem)), "</p>"))
      items_html <- apply(grp$items, 1L, function(row) {
        opt <- extract_option_label(row[["label"]], stem)
        paste0("<div class=\"kit-check-item\">",
               html_escape(KIT_CHECKBOX), " ",
               html_escape(opt),
               " \u3010", html_escape(row[["itemID"]]), "\u3011</div>")
      })
      parts <- c(parts, paste(items_html, collapse = "\n"))

    } else if (itype %in% c("openended", "numeric")) {
      q_state$n <- q_state$n + 1L
      q_txt <- format_openended_numbered(
        q_state$n, grp$items$label[1L], grp$items$itemID[1L])
      parts <- c(parts, paste0("<p>", html_escape(q_txt), "</p>"))

    } else if (should_use_matrix_layout(grp)) {
      if (!is.null(grp$stem_text) && nzchar(grp$stem_text))
        parts <- c(parts, paste0("<p>", html_escape(grp$stem_text), "</p>"))
      row_labels <- vapply(seq_len(nrow(grp$items)), function(i) {
        q_state$n <<- q_state$n + 1L
        format_numbered_question(
          q_state$n, grp$items$label[i], grp$items$itemID[i])
      }, character(1L))
      mat_scale <- resolve_matrix_group_scale(
        grp$items, scale$labels, scale$codes
      )
      parts <- c(parts, render_likert_matrix_html(
        grp$items, mat_scale$labels, mat_scale$codes, row_labels = row_labels))

    } else {
      for (i in seq_len(nrow(grp$items))) {
        row <- grp$items[i, , drop = FALSE]
        q_state$n <- q_state$n + 1L
        q_txt <- format_numbered_question(
          q_state$n, row$label[1L], row$itemID[1L])
        resolved <- resolve_item_scale(row, scale$labels, scale$codes)
        opt_html <- if (identical(resolved$source, "missing")) {
          render_choice_options_html(NULL, NULL, source = "missing")
        } else {
          render_choice_options_html(
            resolved$cfg$labels, resolved$cfg$codes, source = resolved$source
          )
        }
        parts <- c(parts,
          paste0("<p><strong>", html_escape(q_txt), "</strong></p>"),
          paste0("<p>", opt_html, "</p>")
        )
      }
    }
    parts <- c(parts, "<div class=\"kit-gap\"></div>")
  }

  paste0(c(parts, "</section>"), collapse = "\n")
}

generate_questionnaire_html <- function(block_configs, itemBank_df, title = "KIT \u554f\u5377\u9810\u89bd") {
  q_state <- list(n = 0L)
  block_html <- vapply(seq_along(block_configs), function(i) {
    render_block_html(block_configs[[i]], itemBank_df,
                      block_idx = i, q_state = q_state)
  }, character(1L), USE.NAMES = FALSE)

  css <- "
    body { font-family: 'Microsoft JhengHei','PingFang TC',sans-serif; margin: 24px; color: #222; }
    h1 { font-size: 1.4rem; border-bottom: 2px solid #333; padding-bottom: 8px; }
    h2 { font-size: 1.15rem; margin-top: 1.5rem; color: #1a5276; }
    .kit-prompt { background: #f8f9fa; padding: 8px 12px; border-left: 4px solid #6c757d; }
    .kit-instr { font-size: 0.95rem; color: #555; }
    .kit-q-table { border-collapse: collapse; width: 100%; margin: 8px 0 16px; font-size: 0.72rem; }
    .kit-q-table th, .kit-q-table td { border: 1px solid #333; padding: 6px 8px; vertical-align: middle; }
    .kit-q-table th { background: #eef3f7; text-align: center; }
    .kit-tick { text-align: center; width: 56px; }
    .kit-scale-opt { margin-right: 12px; white-space: nowrap; }
    .kit-check-item { margin: 6px 0; }
    .kit-open-box { border: 1px solid #333; min-height: 48px; padding: 10px; color: #888; margin: 8px 0 16px; }
    .kit-gap { height: 8px; }
    .kit-outline-block { border: 1px solid #dee2e6; border-radius: 6px; padding: 10px 12px; background: #fafafa; }
  "

  paste0(
    "<!DOCTYPE html><html lang=\"zh-Hant\"><head><meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>", html_escape(title), "</title><style>", css, "</style></head><body>",
    "<h1>", html_escape(title), "</h1>",
    paste(block_html, collapse = "\n"),
    "</body></html>"
  )
}
