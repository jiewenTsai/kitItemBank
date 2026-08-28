# R/utils_docx.R — 組卷 Word 輸出工具函式
# 依賴：officer (已安裝), flextable (已安裝), stringr

# 匯出版本（下載成功時會顯示，方便確認是否為最新程式）
KIT_EXPORT_VERSION <- "20250828-v12"

# ── 常數 ──────────────────────────────────────────────────────────
KIT_CHECKBOX <- "\u25a1"     # □  勾選框字元
KIT_FONT_CJK <- "\u6a19\u6977\u9ad4"  # 標楷體
KIT_FONT_SIZE_BODY    <- 12L
KIT_FONT_SIZE_HEADING <- 14L
KIT_FONT_SIZE_ITEMID  <- 10L

# ── 空值合併 ──────────────────────────────────────────────────────
`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

# ── 題型偵測 ──────────────────────────────────────────────────────

#' 從 itemType 或 note 欄位偵測題目類型
#' @param value_str 可選，itemBank$value；有解析結果時 MC 視為單選題
#' @return "checkbox" | "openended" | "numeric" | "single_choice" |
#'         "single_choice_unparsed" | "likert"
detect_item_type <- function(note_str, item_type = NULL, value_str = NULL) {
  it <- ""
  if (!is.null(item_type) && length(item_type) > 0L) {
    it <- trimws(toupper(as.character(item_type[[1L]])))
    if (identical(it, "NA")) it <- ""
  }
  if (nzchar(it)) {
    if (identical(it, "MC")) {
      parsed <- parse_scale_value(value_str)
      if (!is.null(parsed) && length(parsed$labels) >= 2L) {
        if (is_likert_scale_value(parsed)) return("likert")
        return("single_choice")
      }
      if (item_value_nonempty(value_str)) {
        if (is_likert_placeholder_value(value_str)) return("likert")
        return("single_choice_unparsed")
      }
      return("likert")
    }
    mapped <- switch(it,
      DC = "checkbox",
      CR = "openended",
      NR = "numeric",
      NULL
    )
    if (!is.null(mapped)) return(mapped)
  }

  if (is.na(note_str) || nchar(as.character(note_str)) == 0) return("likert")
  n <- as.character(note_str)
  if (grepl("\u984c\u578b:\u8907\u9078",   n)) return("checkbox")    # 題型:複選
  if (grepl("\u984c\u578b:\u958b\u653e\u984c", n)) return("openended") # 題型:開放題
  if (grepl("\u984c\u578b:\u6578\u5024\u984c", n)) return("numeric")   # 題型:數值題
  if (grepl("\u984c\u578b:\u77e9\u9663",   n)) return("likert")     # 題型:矩陣 → 當 Likert 處理
  if (grepl("\u984c\u578b:\u4e0b\u62c9\u5f0f", n)) return("openended") # 題型:下拉式 → 填空
  "likert"
}

# ── 量尺標籤解析 ──────────────────────────────────────────────────

#' 從 value 欄位嘗試解析量尺選項（編碼 + 標籤）
#' 支援 "1: 選項; 2: …" 格式；若含多組定義，只取第一組。
#' @return list(codes, labels) 或 NULL（無法解析）
parse_scale_value <- function(value_str) {
  if (length(value_str) != 1L) value_str <- value_str[[1L]]
  if (is.null(value_str) || length(value_str) == 0L) return(NULL)
  if (is.na(value_str)) return(NULL)
  v <- trimws(as.character(value_str))
  if (!nzchar(v)) return(NULL)

  trim_option_tail <- function(lbl) {
    trimws(sub("\\s+\\d+[: ].*$", "", lbl, perl = TRUE))
  }

  parse_option_part <- function(part) {
    p <- trimws(part)
    if (!nzchar(p)) return(NULL)
    if (grepl("^([0-9]+):\\s*(.+)$", p, perl = TRUE)) {
      return(list(
        code  = sub("^([0-9]+):\\s*(.+)$", "\\1", p, perl = TRUE),
        label = trim_option_tail(sub("^([0-9]+):\\s*(.+)$", "\\2", p, perl = TRUE))
      ))
    }
    if (grepl("^([0-9]+)\\s+(.+)$", p, perl = TRUE)) {
      return(list(
        code  = sub("^([0-9]+)\\s+(.+)$", "\\1", p, perl = TRUE),
        label = trim_option_tail(sub("^([0-9]+)\\s+(.+)$", "\\2", p, perl = TRUE))
      ))
    }
    NULL
  }

  collect_option_parts <- function(parts) {
    codes  <- character()
    labels <- character()
    for (p in parts) {
      opt <- parse_option_part(p)
      if (is.null(opt)) {
        if (length(codes) > 0L) break
        next
      }
      if (opt$code == "1" && length(codes) > 0L) break
      codes  <- c(codes, opt$code)
      labels <- c(labels, opt$label)
    }
    if (length(codes) >= 2L) list(codes = codes, labels = labels) else NULL
  }

  # 分號區隔（1: 選項 或 1 選項）
  parts <- trimws(unlist(strsplit(v, ";", fixed = TRUE)))
  parts <- parts[nzchar(parts)]
  parsed <- collect_option_parts(parts)
  if (!is.null(parsed)) return(parsed)

  # 整段掃描 code: label（舊資料／多組定義）
  m <- regmatches(v, gregexpr("[0-9]+: [^\n;]+", v, perl = TRUE))[[1]]
  if (length(m) >= 2L) {
    codes <- trimws(sub(": .*", "", m))
    lbls  <- vapply(sub("^[0-9]+: ", "", m), trim_option_tail, character(1L))
    idx1  <- which(codes == "1")
    if (length(idx1) >= 2L) {
      end <- idx1[2L] - 1L
      codes <- codes[seq_len(end)]
      lbls  <- lbls[seq_len(end)]
    }
    if (length(codes) >= 2L) return(list(codes = codes, labels = lbls))
  }
  NULL
}

#' 是否為分類單選（教育程度、國籍、工作狀態等），非 Likert 量尺
is_categorical_scale_value <- function(parsed) {
  if (is.null(parsed) || length(parsed$labels) < 2L) return(FALSE)
  n <- length(parsed$labels)
  if (n > 7L) return(TRUE)
  if (max(nchar(parsed$labels), 0L) > 14L) return(TRUE)
  cat_pat <- paste(c(
    "\u570b\u5c0f", "\u570b\u4e2d", "\u9ad8\u4e2d", "\u5c08\u79d1",
    "\u5927\u5b78", "\u785d\u58eb", "\u535a\u58eb", "\u8d8a\u5357",
    "\u5370\u5c3c", "\u5de5\u4f5c", "\u9000\u4f11", "\u5f85\u696d",
    "\u570b\u7c4d", "\u586b\u5beb", "\u6c11\u570b", "\u5bb6\u7ba1",
    "\u5168\u8077", "\u517c\u8077"
  ), collapse = "|")
  any(grepl(cat_pat, parsed$labels, perl = TRUE))
}

#' 是否為 Likert／頻率量尺 value（可併成矩陣題組）
is_likert_scale_value <- function(parsed) {
  if (is.null(parsed) || length(parsed$labels) < 2L) return(FALSE)
  if (is_categorical_scale_value(parsed)) return(FALSE)
  n <- length(parsed$labels)
  if (n > 7L) return(FALSE)
  likert_pat <- paste(c(
    "\u5f9e\u672a", "\u5f88\u5c11", "\u6709\u6642", "\u7d93\u5e38",
    "\u7e3d\u662f", "\u5b8c\u5168", "\u5c1a\u5f37", "\u9084\u7b97",
    "\u975e\u5e38", "\u540c\u610f", "\u4e0d\u540c\u610f", "\u719f\u7df4",
    "\u62d2\u7b54", "\u5982\u6b64"
  ), collapse = "|")
  hits <- sum(grepl(likert_pat, parsed$labels, perl = TRUE))
  if (hits >= 2L) return(TRUE)
  if (n <= 7L && max(nchar(parsed$labels), 0L) <= 8L) {
    codes_num <- suppressWarnings(as.integer(parsed$codes))
    if (all(!is.na(codes_num)) && all(codes_num <= 10L)) return(TRUE)
  }
  FALSE
}

#' value 欄是否有非空內容
item_value_nonempty <- function(value_str) {
  if (length(value_str) != 1L) value_str <- value_str[[1L]]
  if (is.null(value_str) || is.na(value_str)) return(FALSE)
  nzchar(trimws(as.character(value_str)))
}

#' value 是否為「1~6點量表」等未展開的量尺占位，或 1→4 反計分對照（非分類選項）
is_likert_placeholder_value <- function(value_str) {
  if (!item_value_nonempty(value_str)) return(FALSE)
  v <- trimws(as.character(value_str))
  if (grepl("\u9ede\u91cf\u8868|\u91cf\u8868", v)) return(TRUE)
  if (grepl("^[0-9]+~[0-9]+", v)) return(TRUE)
  if (grepl("\u2192", v, fixed = TRUE)) return(TRUE)
  FALSE
}

#' 是否為應逐題使用 value、不可套用區塊 Likert 的題型
is_value_driven_item <- function(itype) {
  itype %in% c("single_choice", "single_choice_unparsed")
}

#' 群組內各題題型
item_types_in_group <- function(grp) {
  vapply(seq_len(nrow(grp$items)), function(i) {
    detect_item_type(
      grp$items$note[i],
      if ("itemType" %in% names(grp$items)) grp$items$itemType[i] else NULL,
      if ("value" %in% names(grp$items)) grp$items$value[i] else NULL
    )
  }, character(1L))
}

#' 優先使用題目 value 的選項；單選題不 silent fallback 至區塊 Likert
resolve_item_scale <- function(row, block_labels, block_codes) {
  note      <- if ("note" %in% names(row)) row$note[1L] else ""
  item_type <- if ("itemType" %in% names(row)) row$itemType[1L] else NULL
  value     <- if ("value" %in% names(row)) row$value[1L] else NULL

  parsed <- parse_scale_value(value)
  if (!is.null(parsed) && length(parsed$labels) >= 2L) {
    return(list(
      cfg    = normalize_scale_config(parsed$labels, parsed$codes),
      source = "item"
    ))
  }

  itype <- detect_item_type(note, item_type, value)
  if (is_value_driven_item(itype)) {
    return(list(cfg = NULL, source = "missing"))
  }

  list(
    cfg    = normalize_scale_config(block_labels, block_codes),
    source = "block"
  )
}

#' 單選題選項排版：選項多或文字長時改直排
choice_options_layout <- function(scale_cfg, source = "block") {
  n <- scale_cfg$n
  max_len <- if (length(scale_cfg$labels)) max(nchar(scale_cfg$labels), 0L) else 0L
  if (identical(source, "item") && (n >= 6L || max_len > 14L)) return("vertical")
  if (n > 7L || max_len > 18L) return("vertical")
  "inline"
}

#' 向後相容：只回傳標籤向量
parse_scale_labels <- function(value_str) {
  parsed <- parse_scale_value(value_str)
  if (is.null(parsed)) return(NULL)
  parsed$labels
}

# ── stemID / 矩陣題組分組 ───────────────────────────────────────────

item_key_str <- function(itemID, itemVer) {
  paste(as.character(itemID), as.integer(itemVer), sep = "~~")
}

#' 從 label 推導共同題幹（stemID 缺漏時，複選子題常用）
infer_stem_from_label <- function(label) {
  lbl <- trimws(stringr::str_replace_all(as.character(label), "\\r?\\n", " "))
  if (grepl("\u2500[0-9]+", lbl, perl = TRUE)) {
    return(trimws(sub("\u2500[0-9]+.*", "", lbl, perl = TRUE)))
  }
  if (grepl("-[0-9]+", lbl, perl = TRUE)) {
    return(trimws(sub("-[0-9]+.*", "", lbl, perl = TRUE)))
  }
  NULL
}

#' 取得題目的 matrix_group 鍵（含 stemID 缺漏時從 label 推導）
get_item_matrix_group <- function(item_key, items_df) {
  mg <- item_key$matrix_group %||% ""
  if (nzchar(mg)) return(mg)

  row <- get_item_bank_row(items_df, item_key$itemID, item_key$itemVer)
  if (is.null(row)) return("")

  if (detect_item_type(row$note[1L], row$itemType[1L], row$value[1L]) != "checkbox") return("")

  stem <- if ("stemID" %in% names(row) && !is.na(row$stemID) &&
               nzchar(trimws(as.character(row$stemID[1L])))) {
    trimws(as.character(row$stemID[1L]))
  } else {
    infer_stem_from_label(row$label[1L]) %||% ""
  }
  if (nzchar(stem)) paste0("stem:", stem) else ""
}

#' 建立 block 內的題目設定（含預設矩陣群組）
make_item_entry <- function(row) {
  note <- if ("note" %in% names(row)) as.character(row$note) else ""
  item_type <- if ("itemType" %in% names(row)) row$itemType else NULL
  value <- if ("value" %in% names(row)) row$value else NULL
  itype <- detect_item_type(note, item_type, value)
  is_checkbox <- itype == "checkbox"
  is_matrix_note <- grepl("\u984c\u578b:\u77e9\u9663", note)

  stem <- if ("stemID" %in% names(row) && !is.na(row$stemID))
    trimws(as.character(row$stemID)) else ""
  if (!nzchar(stem) && is_checkbox) {
    stem <- infer_stem_from_label(row$label) %||% ""
  }

  layout <- if (is_matrix_note) "matrix" else "auto"
  matrix_group <- if (nzchar(stem)) paste0("stem:", stem) else NULL

  # 分類單選（非 Likert 量尺）逐題輸出，避免被併成矩陣
  if (is_value_driven_item(itype)) {
    layout <- "single"
    matrix_group <- NULL
  }

  list(
    itemID       = as.character(row$itemID),
    itemVer      = as.integer(row$itemVer),
    layout       = layout,
    matrix_group = matrix_group
  )
}

#' 從 itemBank 取單列
get_item_bank_row <- function(itemBank_df, itemID, itemVer) {
  r <- itemBank_df[itemBank_df$itemID == itemID &
                   itemBank_df$itemVer == as.integer(itemVer), , drop = FALSE]
  if (nrow(r) == 0L) return(NULL)
  r[1L, , drop = FALSE]
}

#' 依 block 設定將題目分組（保留順序）
#' @return list of list(stem_text, items, layout, matrix_group)
group_items_for_render <- function(items_df, item_keys) {
  if (length(item_keys) == 0L) return(list())

  groups    <- list()
  processed <- character(0)

  for (k in item_keys) {
    key <- item_key_str(k$itemID, k$itemVer)
    if (key %in% processed) next

    layout <- k$layout %||% "auto"

    if (identical(layout, "single")) {
      row <- get_item_bank_row(items_df, k$itemID, k$itemVer)
      if (is.null(row)) next
      groups[[length(groups) + 1L]] <- list(
        stem_text    = NULL,
        items        = row,
        layout       = "single",
        matrix_group = NULL
      )
      processed <- c(processed, key)
      next
    }

    mg <- get_item_matrix_group(k, items_df)
    if ((is.null(mg) || !nzchar(mg)) && identical(layout, "auto")) {
      row  <- get_item_bank_row(items_df, k$itemID, k$itemVer)
      stem <- if (!is.null(row) && "stemID" %in% names(row) && !is.na(row$stemID))
        trimws(as.character(row$stemID[1L])) else ""
      if (!nzchar(stem) && !is.null(row))
        stem <- infer_stem_from_label(row$label[1L]) %||% ""
      if (nzchar(stem)) mg <- paste0("stem:", stem)
    }

    if (!is.null(mg) && nzchar(mg)) {
      matching <- Filter(function(x) {
        kk <- item_key_str(x$itemID, x$itemVer)
        if (kk %in% processed) return(FALSE)
        xl <- x$layout %||% "auto"
        if (identical(xl, "single")) return(FALSE)
        identical(get_item_matrix_group(x, items_df), mg)
      }, item_keys)

      if (length(matching) == 0L) matching <- list(k)

      rows_list <- lapply(matching, function(x) {
        get_item_bank_row(items_df, x$itemID, x$itemVer)
      })
      rows_list <- Filter(Negate(is.null), rows_list)
      if (length(rows_list) == 0L) next

      items_grp <- do.call(rbind, rows_list)
      stem_text <- {
        stems <- unique(trimws(as.character(items_grp$stemID)))
        stems <- stems[!is.na(stems) & nzchar(stems)]
        if (length(stems) >= 1L) {
          stems[1L]
        } else {
          infer_stem_from_label(items_grp$label[1L])
        }
      }

      grp_layout <- if (identical(layout, "matrix")) "matrix" else "auto"
      groups[[length(groups) + 1L]] <- list(
        stem_text    = stem_text,
        items        = items_grp,
        layout       = grp_layout,
        matrix_group = mg
      )
      processed <- c(processed, vapply(matching, function(x) {
        item_key_str(x$itemID, x$itemVer)
      }, character(1L)))
      next
    }

    if (identical(layout, "matrix")) {
      row <- get_item_bank_row(items_df, k$itemID, k$itemVer)
      if (is.null(row)) next
      groups[[length(groups) + 1L]] <- list(
        stem_text    = NULL,
        items        = row,
        layout       = "matrix",
        matrix_group = NULL
      )
      processed <- c(processed, key)
      next
    }

    row <- get_item_bank_row(items_df, k$itemID, k$itemVer)
    if (is.null(row)) next
    groups[[length(groups) + 1L]] <- list(
      stem_text    = NULL,
      items        = row,
      layout       = "auto",
      matrix_group = NULL
    )
    processed <- c(processed, key)
  }

  groups
}

#' 量尺指紋（相同 value 選項的 Likert 題可併成矩陣）
scale_fingerprint <- function(row) {
  if (is.null(row) || !"value" %in% names(row)) return("")
  parsed <- parse_scale_value(row$value[1L])
  if (is.null(parsed)) return("")
  paste(paste0(parsed$codes, ":", parsed$labels), collapse = "|")
}

#' Likert 自動合併鍵：有 value 時用指紋；無則用區塊量尺（與單題輸出一致）
likert_batch_key <- function(row, block_labels, block_codes) {
  fp <- scale_fingerprint(row)
  if (nzchar(fp)) return(fp)
  resolved <- resolve_item_scale(row, block_labels, block_codes)
  if (!is.null(resolved$cfg)) {
    cfg <- resolved$cfg
    return(paste(paste0(cfg$codes, ":", cfg$labels), collapse = "|"))
  }
  "likert:unparsed"
}

#' 矩陣題組使用的量尺（優先題目 value，其次區塊預設）
resolve_matrix_group_scale <- function(items_df, block_labels, block_codes) {
  if (nrow(items_df) < 1L) {
    return(normalize_scale_config(block_labels, block_codes))
  }
  resolved <- resolve_item_scale(
    items_df[1L, , drop = FALSE], block_labels, block_codes
  )
  if (!is.null(resolved$cfg)) return(resolved$cfg)
  normalize_scale_config(block_labels, block_codes)
}

#' 將連續、同量尺的 Likert 單題自動併為一組（恢復矩陣題組）
batch_consecutive_likert_groups <- function(groups,
                                            block_labels = NULL,
                                            block_codes = NULL) {
  if (length(groups) <= 1L) return(groups)

  out <- list()
  buf <- list()
  buf_fp <- ""

  flush_buf <- function() {
    if (length(buf) == 0L) return(invisible(NULL))
    if (length(buf) == 1L) {
      out <<- c(out, buf)
    } else {
      items <- do.call(rbind, lapply(buf, function(g) g$items))
      out[[length(out) + 1L]] <<- list(
        stem_text    = NULL,
        items        = items,
        layout       = "auto",
        matrix_group = NULL
      )
    }
    buf <<- list()
    buf_fp <<- ""
    invisible(NULL)
  }

  for (g in groups) {
    is_single_auto <- identical(g$layout, "auto") && nrow(g$items) == 1L
    if (!is_single_auto) {
      flush_buf()
      out <- c(out, list(g))
      next
    }
    row <- g$items[1L, , drop = FALSE]
    itype <- detect_item_type(
      row$note[1L],
      if ("itemType" %in% names(row)) row$itemType[1L] else NULL,
      if ("value" %in% names(row)) row$value[1L] else NULL
    )
    if (itype != "likert") {
      flush_buf()
      out <- c(out, list(g))
      next
    }
    fp <- likert_batch_key(row, block_labels, block_codes)
    if (length(buf) == 0L) {
      buf <- list(g)
      buf_fp <- fp
    } else if (identical(fp, buf_fp)) {
      buf <- c(buf, list(g))
    } else {
      flush_buf()
      buf <- list(g)
      buf_fp <- fp
    }
  }
  flush_buf()
  out
}

#' 向後相容：僅依 stemID 分組
group_items_by_stem <- function(items_df) {
  keys <- lapply(seq_len(nrow(items_df)), function(i) {
    row <- items_df[i, , drop = FALSE]
    make_item_entry(row)
  })
  group_items_for_render(items_df, keys)
}

#' 判斷 Likert 群組是否用矩陣表格（格式 B）
should_use_matrix_layout <- function(grp) {
  layout <- grp$layout %||% "auto"
  types  <- item_types_in_group(grp)

  # 使用者按「合併為矩陣題組」：Likert 可出表；分類單選仍禁止
  if (identical(layout, "matrix")) {
    return(!any(types %in% c("single_choice", "single_choice_unparsed")))
  }

  # 分類單選（教育程度、國籍等）不可自動矩陣
  if (any(types %in% c("single_choice", "single_choice_unparsed"))) return(FALSE)

  if (identical(layout, "single")) return(FALSE)
  n <- nrow(grp$items)
  if (n > 1L) return(all(types == "likert"))
  if (any(grepl("\u984c\u578b:\u77e9\u9663", as.character(grp$items$note)))) return(TRUE)
  FALSE
}

layout_label_zh <- function(layout, use_matrix) {
  if (identical(layout, "single")) return("\u55ae\u984c")
  if (identical(layout, "matrix") || isTRUE(use_matrix)) return("\u77e9\u9663\u8868\u683c")
  "\u55ae\u984c"
}

# ── 題目文字處理 ──────────────────────────────────────────────────

#' 從複選子題 label 抽出選項名稱（去掉共同題幹與「─N. 」前綴）
extract_option_label <- function(label, stem_text = NULL) {
  lbl <- trimws(stringr::str_replace_all(as.character(label), "\\r?\\n", " "))

  if (grepl("\u2500\\s*[0-9]+\\.\\s*", lbl, perl = TRUE)) {
    return(trimws(sub(".*\u2500\\s*[0-9]+\\.\\s*", "", lbl, perl = TRUE)))
  }
  if (grepl("-\\s*[0-9]+\\.\\s*", lbl, perl = TRUE)) {
    return(trimws(sub(".*-\\s*[0-9]+\\.\\s*", "", lbl, perl = TRUE)))
  }
  # ─8【拒答...】 等無小數點格式
  if (grepl("\u2500[0-9]+", lbl, perl = TRUE)) {
    return(trimws(sub(".*\u2500[0-9]+", "", lbl, perl = TRUE)))
  }
  if (grepl("-[0-9]+", lbl, perl = TRUE)) {
    return(trimws(sub(".*-[0-9]+", "", lbl, perl = TRUE)))
  }

  if (!is.null(stem_text) && nzchar(trimws(stem_text))) {
    st <- trimws(stem_text)
    if (startsWith(lbl, st)) {
      rest <- trimws(substring(lbl, nchar(st) + 1L))
      rest <- sub("^[\\u2500\\-–—]+\\s*", "", rest, perl = TRUE)
      rest <- sub("^[0-9]+\\.\\s*", "", rest, perl = TRUE)
      if (nzchar(rest)) return(rest)
    }
  }

  sub("^[a-zA-Z]\\)\\s*", "", lbl)
}

#' 取得題組的共同題幹文字
get_group_stem_text <- function(items_df, stem_text = NULL) {
  st <- stem_text %||% NULL
  if (!is.null(st) && nzchar(trimws(st))) return(trimws(st))
  if ("stemID" %in% names(items_df)) {
    stems <- unique(trimws(as.character(items_df$stemID)))
    stems <- stems[!is.na(stems) & nzchar(stems)]
    if (length(stems) >= 1L) return(stems[1L])
  }
  if (nrow(items_df) >= 1L) {
    inferred <- infer_stem_from_label(items_df$label[1L])
    if (!is.null(inferred) && nzchar(inferred)) return(inferred)
  }
  NULL
}

#' 開放題：保留 label 內建底線，否則以克漏字空格結尾
format_openended_text <- function(label, itemID) {
  lbl <- trimws(stringr::str_replace_all(as.character(label), "\\r?\\n", " "))
  if (!grepl("_{2,}|＿{2,}|___+", lbl)) {
    lbl <- paste0(lbl, " __________")
  }
  paste0(lbl, "  \u3010", itemID, "\u3011")
}

# ── Word 字型 / 段落 ──────────────────────────────────────────────

kit_fp_body <- function() {
  officer::fp_text(font.size = KIT_FONT_SIZE_BODY, font.family = KIT_FONT_CJK)
}

kit_fp_heading <- function() {
  officer::fp_text(font.size = KIT_FONT_SIZE_HEADING,
                   font.family = KIT_FONT_CJK, bold = TRUE)
}

kit_fp_itemid <- function() {
  officer::fp_text(font.size = KIT_FONT_SIZE_ITEMID, font.family = KIT_FONT_CJK)
}

kit_fp_muted <- function() {
  officer::fp_text(font.size = KIT_FONT_SIZE_BODY,
                   font.family = KIT_FONT_CJK, color = "#888888")
}

kit_fpar_plain <- function(text, fp = kit_fp_body()) {
  officer::fpar(officer::ftext(as.character(text), prop = fp))
}

kit_fpar_with_itemid <- function(main_text, itemID) {
  officer::fpar(
    officer::ftext(as.character(main_text), prop = kit_fp_body()),
    officer::ftext(paste0("\u3010", itemID, "\u3011"), prop = kit_fp_itemid())
  )
}

doc_add_body_fpar <- function(doc, fp) {
  officer::body_add_fpar(doc, fp)
}

#' 從列尾 【itemID】 拆成主文 + 代碼
parse_itemid_tag <- function(text) {
  text <- as.character(text)
  if (grepl("\u3010[^\u3011]+\u3011\\s*$", text, perl = TRUE)) {
    id <- sub(".*\u3010([^\u3011]+)\u3011\\s*$", "\\1", text, perl = TRUE)
    main <- sub("\\s*\u3010[^\u3011]+\u3011\\s*$", "", text, perl = TRUE)
    list(main = main, id = id)
  } else {
    list(main = text, id = NULL)
  }
}

apply_flex_itemid_col <- function(ft, col_j, texts) {
  for (i in seq_along(texts)) {
    parts <- parse_itemid_tag(texts[i])
    if (!is.null(parts$id)) {
      ft <- flextable::compose(
        ft, j = col_j, i = i,
        value = flextable::as_paragraph(
          flextable::as_chunk(parts$main, props = kit_fp_body()),
          flextable::as_chunk(
            paste0(" \u3010", parts$id, "\u3011"),
            props = kit_fp_itemid()
          )
        )
      )
    }
  }
  ft
}

apply_flex_kit_font <- function(ft, part = "all") {
  ft <- flextable::fontsize(ft, size = KIT_FONT_SIZE_BODY, part = part)
  flextable::font(ft, fontname = KIT_FONT_CJK, part = part)
}

# ── flextable builder helpers ──────────────────────────────────────

#' Word 表格：移除所有可見格線
apply_hidden_borders <- function(ft) {
  flextable::border_remove(ft)
}

#' 清除 flextable 預設表頭（改為僅保留自訂雙列表頭）
prepare_flextable_headers <- function(ft) {
  flextable::delete_part(ft, part = "header")
}

#' 格式 B：矩陣式 Likert 量尺表格
#' @param row_labels  optional character vector，列文字（已含編號時傳入）
make_likert_matrix_ft <- function(items_df, scale_labels, scale_codes = NULL,
                                  row_labels = NULL) {
  scale    <- normalize_scale_config(scale_labels, scale_codes)
  n_scale  <- scale$n
  scale_labels <- scale$labels
  codes    <- scale$codes
  n_items  <- nrow(items_df)

  labels_clean <- stringr::str_replace_all(items_df$label, "\\r?\\n", " ")
  if (is.null(row_labels)) {
    item_texts <- paste0(labels_clean, "  \u3010", items_df$itemID, "\u3011")
  } else {
    item_texts <- row_labels
  }

  col_keys <- c("item", paste0("scale_", seq_len(n_scale)))
  df <- data.frame(item = item_texts, stringsAsFactors = FALSE)
  for (i in seq_len(n_scale)) {
    df[[col_keys[i + 1L]]] <- KIT_CHECKBOX
  }

  total_w <- 6.3
  scale_w <- min(1.15, round((total_w * 0.46) / n_scale, 2))
  item_w  <- round(total_w - scale_w * n_scale, 2)

  ft <- flextable::flextable(df, col_keys = col_keys)
  ft <- prepare_flextable_headers(ft)
  ft <- flextable::add_header_row(
    ft,
    values = c("", codes),
    top    = TRUE
  )
  ft <- flextable::add_header_row(
    ft,
    values = c("", scale_labels),
    top    = FALSE
  )
  ft <- flextable::width(ft, j = 1,             width = item_w,  unit = "in")
  ft <- flextable::width(ft, j = 2:(n_scale+1), width = scale_w, unit = "in")
  ft <- flextable::align(ft, j = 2:(n_scale+1), align = "center", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "center", part = "all")
  ft <- apply_flex_kit_font(ft, part = "all")
  ft <- flextable::bold(ft, bold = TRUE, part = "header")
  ft <- apply_hidden_borders(ft)
  ft <- flextable::set_table_properties(ft, layout = "fixed")
  apply_flex_itemid_col(ft, col_j = 1L, texts = item_texts)
}

#' 格式 C：複選 checkbox — 題幹 + 各選項（□ 選項名 【itemID】）
make_checkbox_ft <- function(items_df, stem_text = NULL) {
  n_items <- nrow(items_df)
  stem    <- get_group_stem_text(items_df, stem_text)

  option_labels <- vapply(seq_len(n_items), function(i) {
    extract_option_label(items_df$label[i], stem)
  }, character(1L))

  item_texts <- paste0(option_labels, "  \u3010", items_df$itemID, "\u3011")

  df <- data.frame(chk = rep(KIT_CHECKBOX, n_items), item = item_texts,
                   stringsAsFactors = FALSE)

  ft <- flextable::flextable(df)
  ft <- flextable::delete_part(ft, part = "header")
  ft <- flextable::width(ft, j = 1, width = 0.35, unit = "in")
  ft <- flextable::width(ft, j = 2, width = 5.95, unit = "in")
  ft <- flextable::align(ft, j = 1, align = "center", part = "body")
  ft <- flextable::align(ft, j = 2, align = "left",   part = "body")
  ft <- flextable::valign(ft, valign = "center", part = "body")
  ft <- apply_flex_kit_font(ft, part = "body")
  ft <- apply_hidden_borders(ft)
  ft <- flextable::set_table_properties(ft, layout = "fixed")
  apply_flex_itemid_col(ft, col_j = 2L, texts = item_texts)
}

#' 格式 D：開放題填答框（單一示意文字框，非實際填答列）
make_openended_ft <- function(n_lines = 1L) {
  df <- data.frame(
    ans = "\uff08\u586b\u7b54\u5340\uff09",  # （填答區）
    stringsAsFactors = FALSE
  )

  ft <- flextable::flextable(df)
  ft <- flextable::delete_part(ft, part = "header")
  ft <- flextable::width(ft, j = 1, width = 6.3, unit = "in")
  ft <- flextable::height(ft, height = 0.55, unit = "in", part = "body")
  ft <- flextable::align(ft, j = 1, align = "left", part = "body")
  ft <- flextable::valign(ft, valign = "center", part = "body")
  ft <- flextable::color(ft, color = "#888888", part = "body")
  ft <- apply_flex_kit_font(ft, part = "body")
  ft <- apply_hidden_borders(ft)
  flextable::set_table_properties(ft, layout = "fixed")
}

#' 格式 A：單題 inline 選項文字
#' @return character string（含 □ 符號）
make_likert_single_text <- function(scale_labels, scale_codes = NULL) {
  scale <- normalize_scale_config(scale_labels, scale_codes)
  paste(
    mapply(function(code, lbl) {
      paste0(KIT_CHECKBOX, " ", code, " ", lbl)
    }, scale$codes, scale$labels, USE.NAMES = FALSE),
    collapse = "    "
  )
}

#' 將選項加入 Word（自動 inline / 直排）
add_choice_options_to_doc <- function(doc, scale_labels, scale_codes = NULL,
                                      source = "block") {
  if (identical(source, "missing")) {
    return(doc_add_body_fpar(
      doc,
      kit_fpar_plain(
        "（本題 value 無法解析，未套用區塊 Likert；請檢查題庫或拆至獨立區塊）",
        fp = kit_fp_muted()
      )
    ))
  }
  cfg    <- normalize_scale_config(scale_labels, scale_codes)
  layout <- choice_options_layout(cfg, source)
  lines  <- mapply(function(code, lbl) {
    paste0(KIT_CHECKBOX, " ", code, " ", lbl)
  }, cfg$codes, cfg$labels, USE.NAMES = FALSE)
  if (identical(layout, "inline")) {
    doc_add_body_fpar(doc, kit_fpar_plain(paste(lines, collapse = "    ")))
  } else {
    for (ln in lines) doc <- doc_add_body_fpar(doc, kit_fpar_plain(ln))
    doc
  }
}

# ── Block 組裝 ────────────────────────────────────────────────────

#' 區塊標題：剝除名稱既有編號，只保留一組 block_idx 前綴
strip_leading_block_number <- function(text) {
  trimws(sub("^\\d+\\.\\s*", "", trimws(as.character(text))))
}

format_block_title <- function(block_idx, blk_name) {
  clean <- strip_leading_block_number(blk_name)
  if (!nzchar(clean)) clean <- "\uff08\u672a\u547d\u540d\u5340\u584a\uff09"
  paste0(block_idx, ". ", clean)
}

#' 格式化單題文字（含編號）
format_numbered_question <- function(q_num, label, itemID) {
  lbl <- stringr::str_replace_all(as.character(label), "\\r?\\n", " ")
  paste0(q_num, ") ", lbl, "  \u3010", itemID, "\u3011")
}

#' 將一個 Block 的題目加入 officer doc
#' @return list(doc, q_num)
add_block_to_doc <- function(doc, block_cfg, itemBank_df,
                             block_idx = 1L, q_num = 0L) {

  blk_name <- trimws(block_cfg$name %||% "\uff08\u672a\u547d\u540d\u5340\u584a\uff09")
  if (nchar(blk_name) == 0) blk_name <- "\uff08\u672a\u547d\u540d\u5340\u584a\uff09"
  title <- format_block_title(block_idx, blk_name)
  doc <- doc_add_body_fpar(doc, kit_fpar_plain(title, fp = kit_fp_heading()))

  # 2. 提示語
  prompt <- trimws(block_cfg$prompt %||% "")
  if (nchar(prompt) > 0)
    doc <- doc_add_body_fpar(doc, kit_fpar_plain(prompt))

  # 3. 答題說明
  instr <- trimws(block_cfg$instruction %||% "")
  if (nchar(instr) > 0)
    doc <- doc_add_body_fpar(doc, kit_fpar_plain(instr))

  # 4. 取得此 Block 選取的題目（按使用者順序）
  item_keys <- block_cfg$items
  if (length(item_keys) == 0) return(list(doc = doc, q_num = q_num))

  rows_list <- lapply(item_keys, function(k) {
    r <- itemBank_df[itemBank_df$itemID == k$itemID &
                     itemBank_df$itemVer == as.integer(k$itemVer), , drop = FALSE]
    if (nrow(r) == 0L) return(NULL)
    r[1L, , drop = FALSE]
  })
  rows_list <- Filter(Negate(is.null), rows_list)
  if (length(rows_list) == 0L) return(list(doc = doc, q_num = q_num))

  items_df <- do.call(rbind, rows_list)

  # 5. 依 block 設定分組（矩陣 / 單題）
  groups <- group_items_for_render(items_df, item_keys)
  scale_cfg <- normalize_scale_config(
    block_cfg$scale_labels,
    block_cfg$scale_codes,
    block_cfg$scale_n
  )
  scale_labels <- scale_cfg$labels
  scale_codes  <- scale_cfg$codes
  open_box_lines <- as.integer(block_cfg$open_box_lines %||% 1L)

  groups <- batch_consecutive_likert_groups(
    groups, block_labels = scale_labels, block_codes = scale_codes
  )

  for (grp in groups) {
    n_in_grp <- nrow(grp$items)
    itype    <- detect_item_type(
      grp$items$note[1L],
      if ("itemType" %in% names(grp$items)) grp$items$itemType[1L] else NULL,
      if ("value" %in% names(grp$items)) grp$items$value[1L] else NULL
    )

    if (itype == "checkbox") {
      q_num <- q_num + 1L
      stem <- get_group_stem_text(grp$items, grp$stem_text)
      if (!is.null(stem) && nzchar(stem))
        doc <- doc_add_body_fpar(doc, kit_fpar_plain(paste0(q_num, ") ", stem)))
      ft  <- make_checkbox_ft(grp$items, stem_text = stem)
      doc <- flextable::body_add_flextable(doc, ft)

    } else if (itype %in% c("openended", "numeric")) {
      q_num <- q_num + 1L
      lbl <- openended_label_text(grp$items$label[1L])
      doc <- doc_add_body_fpar(doc,
        kit_fpar_with_itemid(paste0(q_num, ") ", lbl), grp$items$itemID[1L]))

    } else if (should_use_matrix_layout(grp)) {
      if (!is.null(grp$stem_text) && nzchar(grp$stem_text))
        doc <- doc_add_body_fpar(doc, kit_fpar_plain(grp$stem_text))
      row_labels <- vapply(seq_len(nrow(grp$items)), function(i) {
        q_num <<- q_num + 1L
        format_numbered_question(
          q_num, grp$items$label[i], grp$items$itemID[i])
      }, character(1L))
      mat_scale <- resolve_matrix_group_scale(
        grp$items, scale_labels, scale_codes
      )
      ft  <- make_likert_matrix_ft(
        grp$items, mat_scale$labels, mat_scale$codes, row_labels = row_labels)
      doc <- flextable::body_add_flextable(doc, ft)

    } else {
      for (i in seq_len(nrow(grp$items))) {
        row <- grp$items[i, , drop = FALSE]
        q_num <- q_num + 1L
        lbl <- stringr::str_replace_all(as.character(row$label[1L]), "\\r?\\n", " ")
        doc <- doc_add_body_fpar(doc,
          kit_fpar_with_itemid(paste0(q_num, ") ", lbl), row$itemID[1L]))
        resolved <- resolve_item_scale(row, scale_labels, scale_codes)
        if (identical(resolved$source, "missing")) {
          doc <- add_choice_options_to_doc(doc, NULL, NULL, source = "missing")
        } else {
          doc <- add_choice_options_to_doc(
            doc, resolved$cfg$labels, resolved$cfg$codes, source = resolved$source
          )
        }
      }
    }

    doc <- doc_add_body_fpar(doc, kit_fpar_plain(""))
  }

  list(doc = doc, q_num = q_num)
}

#' 開放題 label（保留底線，不含編號與 itemID）
openended_label_text <- function(label) {
  lbl <- trimws(stringr::str_replace_all(as.character(label), "\\r?\\n", " "))
  if (!grepl("_{2,}|＿{2,}|___+", lbl)) {
    lbl <- paste0(lbl, " __________")
  }
  lbl
}

#' 開放題：克漏字 + 編號（字串版，HTML 等沿用）
format_openended_numbered <- function(q_num, label, itemID) {
  paste0(q_num, ") ", openended_label_text(label), "  \u3010", itemID, "\u3011")
}

# ── 頂層生成函式 ──────────────────────────────────────────────────

#' 生成完整問卷 Word 文件
#' @param block_configs list of block_cfg
#' @param itemBank_df   full itemBank data.frame
#' @return rdocx object（用 print(doc, target = file) 儲存）
generate_questionnaire <- function(block_configs, itemBank_df) {
  if (!requireNamespace("flextable", quietly = TRUE))
    stop("需要安裝 flextable 套件：install.packages('flextable')")
  if (!requireNamespace("officer",   quietly = TRUE))
    stop("需要安裝 officer 套件：install.packages('officer')")

  doc <- officer::read_docx()
  q_num <- 0L

  for (i in seq_along(block_configs)) {
    res <- add_block_to_doc(doc, block_configs[[i]], itemBank_df,
                            block_idx = i, q_num = q_num)
    doc   <- res$doc
    q_num <- res$q_num
  }

  doc
}
