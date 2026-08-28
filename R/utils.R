# R/utils.R — 共用小工具

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0L) x else y

#' 正規化 Likert 量尺的編碼與標籤
normalize_scale_config <- function(scale_labels, scale_codes = NULL, n = NULL) {
  labels <- as.character(scale_labels %||% character(0))
  if (is.null(n)) n <- max(length(labels), 1L)
  if (length(labels) < n) {
    labels <- c(labels, rep("", n - length(labels)))
  } else if (length(labels) > n) {
    labels <- labels[seq_len(n)]
  }

  codes <- as.character(scale_codes %||% character(0))
  if (length(codes) < n) {
    defaults <- as.character(seq_len(n))
    if (length(codes) == 0L) {
      codes <- defaults
    } else {
      codes <- c(codes, defaults[(length(codes) + 1L):n])
    }
  } else if (length(codes) > n) {
    codes <- codes[seq_len(n)]
  }

  for (i in seq_len(n)) {
    if (!nzchar(trimws(labels[i]))) labels[i] <- paste0("\u683c ", i)
    if (!nzchar(trimws(codes[i]))) codes[i] <- as.character(i)
  }

  list(codes = trimws(codes), labels = trimws(labels), n = n)
}

#' 重新載入匯出相關 R 檔（避免 Shiny  session 沿用舊函式）
reload_kit_export_sources <- function() {
  root <- if (file.exists("app.R")) "." else getwd()
  src <- function(f) {
    p <- file.path(root, f)
    if (file.exists(p)) source(p, local = FALSE)
  }
  src("R/utils.R")
  src("R/utils_docx.R")
  src("R/utils_html.R")
  src("R/utils_assemble_io.R")
  invisible(get0("KIT_EXPORT_VERSION", ifnotfound = "unknown"))
}

#' 取得矩陣 df 的非鍵欄位名稱（不寫死清單，動態推導）
get_matrix_cols <- function(df) {
  setdiff(names(df), c("itemID", "itemVer"))
}

#' 從波次欄名取出排序用主鍵（月齡／年級數字）
wave_col_sort_primary <- function(col_name) {
  if (grepl("^M(\\d+)", col_name, perl = TRUE)) {
    return(as.integer(sub("^M(\\d+).*", "\\1", col_name, perl = TRUE)))
  }
  if (grepl("^(\\d+)M", col_name, perl = TRUE)) {
    return(as.integer(sub("^(\\d+)M.*", "\\1", col_name, perl = TRUE)))
  }
  if (grepl("^小(\\d+)", col_name, perl = TRUE)) {
    return(5000L + as.integer(sub("^小(\\d+).*", "\\1", col_name, perl = TRUE)))
  }
  if (grepl("^國(\\d+)", col_name, perl = TRUE)) {
    return(10000L + as.integer(sub("^國(\\d+).*", "\\1", col_name, perl = TRUE)))
  }
  Inf
}

#' 波次欄名次要排序：小月齡 < 無後綴 < 大月齡；保母 < 保母b < 機構 < 機構b
wave_col_sort_secondary <- function(col_name) {
  if (grepl("小月齡", col_name, fixed = TRUE)) return(1L)
  if (grepl("大月齡", col_name, fixed = TRUE)) return(3L)
  if (grepl("保母b", col_name, fixed = TRUE)) return(2L)
  if (grepl("保母", col_name, fixed = TRUE)) return(1L)
  if (grepl("機構b", col_name, fixed = TRUE)) return(4L)
  if (grepl("機構", col_name, fixed = TRUE)) return(3L)
  2L
}

#' 波次欄由左至右：依 M／數字M／小／國 後數字升冪
sort_wave_cols <- function(col_names) {
  if (length(col_names) <= 1L) return(col_names)
  ord <- order(
    vapply(col_names, wave_col_sort_primary, numeric(1)),
    vapply(col_names, wave_col_sort_secondary, integer(1)),
    col_names
  )
  col_names[ord]
}

#' itemWave 的波次欄（已依月齡排序）
get_wave_cols <- function(df) {
  sort_wave_cols(get_matrix_cols(df))
}

#' 清除欄位名稱中的內嵌換行，供顯示用
col_display_labels <- function(col_names) {
  gsub("[\r\n]+", " ", col_names, perl = TRUE)
}

#' 建立 selectize choices：value = 原始欄名（含特殊字元），label = 清理後文字
make_col_choices <- function(col_names) {
  stats::setNames(col_names, col_display_labels(col_names))
}

#' 截斷長字串供 DT 顯示，避免表格列高暴漲
trunc_label <- function(x, width = 80) {
  stringr::str_trunc(as.character(x), width = width, side = "right", ellipsis = "\u2026")
}

#' 壓平多行文字（換行 → " / "）
collapse_newlines <- function(x) {
  stringr::str_replace_all(as.character(x), "\\r?\\n", " / ")
}

#' 為題項建立 selectize 顯示標籤
item_display_label <- function(itemID, itemVer, label) {
  clean <- stringr::str_trunc(
    stringr::str_replace_all(as.character(label), "\\s+", " "),
    60, ellipsis = "\u2026"
  )
  paste0("[", itemID, " v", itemVer, "] ", clean)
}

#' 從 df 中取出 (itemID, itemVer) 指定的列
get_item_row <- function(df, item_id, item_ver) {
  df[df$itemID == item_id & df$itemVer == as.integer(item_ver), , drop = FALSE]
}

#' DT 語系（繁中）
kit_dt_lang <- function() {
  list(
    search     = "\u641c\u5c0b\uff1a",
    lengthMenu = "\u6bcf\u9801 _MENU_ \u7b46",
    info       = "\u7b2c _START_ \u81f3 _END_ \u7b46\uff0c\u5171 _TOTAL_ \u7b46"
  )
}

#' itemID / itemVer / block / label 欄寬比 1:1:1:6（固定像素 + scrollX）
#' @param label_col_idx label 欄的 **0-based** 欄位索引（DT columnDefs targets）
kit_item_core_col_defs <- function(label_col_idx = 3L, unit_px = 80L) {
  list(
    list(targets = label_col_idx - 3L, width = paste0(unit_px, "px"),
         className = "dt-id-col"),
    list(targets = label_col_idx - 2L, width = paste0(unit_px, "px"),
         className = "dt-ver-col"),
    list(targets = label_col_idx - 1L, width = paste0(unit_px, "px"),
         className = "dt-block-col"),
    list(targets = label_col_idx, width = paste0(unit_px * 6L, "px"),
         className = "dt-label-col")
  )
}

#' 建立含核心四欄的 DT（1:1:1:6）
kit_render_item_dt <- function(df, extra_col_defs = list(), page_length = 20L,
                               selection = "none",
                               table_class = "table table-sm table-bordered kit-items-dt",
                               ...) {
  disp <- df
  if ("label" %in% names(disp))
    disp$label <- collapse_newlines(disp$label)

  label_idx <- which(names(disp) == "label")[1L] - 1L
  if (is.na(label_idx) || label_idx < 0L)
    label_idx <- min(3L, ncol(disp) - 1L)

  opts <- list(
    scrollX        = TRUE,
    autoWidth      = FALSE,
    pageLength     = page_length,
    dom            = "lftp",
    language       = kit_dt_lang(),
    columnDefs     = c(kit_item_core_col_defs(label_idx), extra_col_defs),
    scrollCollapse = TRUE
  )

  args <- list(
    data      = disp,
    rownames  = FALSE,
    class     = table_class,
    options   = opts,
    ...
  )
  if (!identical(selection, "none")) {
    args$selection <- selection
  }
  do.call(DT::datatable, args)
}

#' 跨波次對照表 DT：前四欄 1:1:1:6，波次欄僅容納打勾
#'
#' 不使用 scrollX（避免表頭／內容拆成兩表造成欄線錯位）；
#' 橫向捲動由外層 .kit-table-scroll 負責。
kit_render_crosswalk_dt <- function(df, page_length = 25L) {
  disp <- df
  if ("label" %in% names(disp))
    disp$label <- collapse_newlines(disp$label)

  label_idx <- which(names(disp) == "label")[1L] - 1L
  if (is.na(label_idx) || label_idx < 0L)
    label_idx <- min(3L, ncol(disp) - 1L)

  n_wave <- max(0L, ncol(disp) - 4L)
  extra_defs <- kit_wave_col_defs(
    n_wave, first_wave_idx = 4L, width_px = 24L, orderable = FALSE
  )

  DT::datatable(
    disp,
    rownames  = FALSE,
    class     = paste(
      "table table-sm table-bordered kit-items-dt kit-crosswalk-dt"
    ),
    options = list(
      scrollX        = FALSE,
      autoWidth      = FALSE,
      pageLength     = page_length,
      dom            = "lftp",
      language       = kit_dt_lang(),
      columnDefs     = c(kit_item_core_col_defs(label_idx), extra_defs),
      ordering       = TRUE,
      order          = list(list(0L, "asc"))
    )
  )
}

#' 波次欄（打勾）窄欄定義，接在核心四欄之後
kit_wave_col_defs <- function(n_wave_cols, first_wave_idx = 4L, width_px = 24L,
                              orderable = TRUE) {
  if (n_wave_cols <= 0L) return(list())
  lapply(seq_len(n_wave_cols), function(i) {
    list(
      targets   = first_wave_idx + i - 1L,
      width     = paste0(width_px, "px"),
      className = "dt-center dt-wave-col",
      orderable = orderable
    )
  })
}

#' 清理 dimMeta 文字欄（NA / 空白 → default）
clean_meta_str <- function(x, default = "") {
  if (length(x) == 0L || is.null(x)) return(default)
  if (length(x) != 1L) x <- x[[1L]]
  if (is.na(x)) return(default)
  val <- trimws(as.character(x))
  if (!nzchar(val)) default else val
}

#' 安全判斷非空字串（NA 視為 FALSE）
nzchar_safe <- function(x) {
  if (length(x) == 0L || is.null(x)) return(FALSE)
  if (any(is.na(x))) return(FALSE)
  nzchar(trimws(as.character(x[[1L]])))
}

#' 構面顯示名稱：alias 優先，其次 dimName，再 fallback
dim_display_name <- function(meta_row, fallback = "") {
  if (is.null(meta_row) || nrow(meta_row) == 0L) return(fallback)
  alias <- if ("alias" %in% names(meta_row))
    clean_meta_str(meta_row$alias[1L]) else ""
  name  <- if ("dimName" %in% names(meta_row))
    clean_meta_str(meta_row$dimName[1L], default = fallback) else fallback
  if (nzchar(alias)) alias else name
}

#' 自編修表依 dimID 取列（避免 DT 分頁列索引錯位）
lookup_meta_edit_row <- function(dim_id, meta_edit_df) {
  if (is.null(meta_edit_df) || !is.data.frame(meta_edit_df) ||
      nrow(meta_edit_df) == 0L || !"dimID" %in% names(meta_edit_df)) {
    return(NULL)
  }
  hit <- meta_edit_df$dimID == dim_id
  if (!any(hit)) return(NULL)
  meta_edit_df[which(hit)[1L], , drop = FALSE]
}

#' 自編修表解析構面顯示名稱、別名、信度、備註
resolve_dim_meta <- function(dim_id, wave, dm, dr, meta_edit_df = NULL) {
  meta <- dm[dm$dimID == dim_id, , drop = FALSE]
  edit_row <- lookup_meta_edit_row(dim_id, meta_edit_df)

  alias_val <- if (!is.null(edit_row)) {
    clean_meta_str(edit_row$alias[1L])
  } else if (nrow(meta) > 0L) {
    clean_meta_str(meta$alias[1L])
  } else ""

  name_val <- if (!is.null(edit_row)) {
    clean_meta_str(edit_row$dimName[1L], default = dim_id)
  } else {
    dim_display_name(meta, fallback = dim_id)
  }

  note_val <- if (!is.null(edit_row)) {
    clean_meta_str(edit_row$note[1L])
  } else if (nrow(meta) > 0L) {
    clean_meta_str(meta$note[1L])
  } else ""

  rel_val <- NA_real_
  if (!is.null(edit_row) && "reliability" %in% names(edit_row)) {
    rel_txt <- clean_meta_str(edit_row$reliability[1L])
    if (nzchar(rel_txt)) {
      rel_val <- suppressWarnings(as.numeric(rel_txt))
    }
  }
  if (is.na(rel_val)) {
    rel_val <- get_dim_reliability(dim_id, wave, dm, dr)
  }

  display_name <- if (nzchar(alias_val)) alias_val else name_val

  list(
    dimName     = display_name,
    dimNameRaw  = name_val,
    alias       = alias_val,
    reliability = rel_val,
    note        = note_val
  )
}

#' 構面報告標題：原名（別名）；無別名時只顯示原名
format_dim_report_heading <- function(dim_name_raw, alias = "") {
  raw <- clean_meta_str(dim_name_raw)
  ali <- clean_meta_str(alias)
  if (nzchar(ali) && !identical(ali, raw)) {
    paste0(raw, "\uff08", ali, "\uff09")
  } else {
    raw
  }
}

#' 同一 itemID 多版本時：優先當波次版本，否則取最新 itemVer
pick_unique_report_items <- function(item_details, wave_items) {
  if (is.null(item_details) || nrow(item_details) == 0L) {
    return(data.frame(itemID = character(0), label = character(0),
                      stringsAsFactors = FALSE))
  }
  wave_keys <- paste(wave_items$itemID, wave_items$itemVer, sep = "\x01")
  item_details$.in_wave <- paste(item_details$itemID, item_details$itemVer,
                                 sep = "\x01") %in% wave_keys
  item_details <- item_details[order(
    item_details$itemID,
    -item_details$.in_wave,
    -as.integer(item_details$itemVer)
  ), , drop = FALSE]
  item_details <- item_details[!duplicated(item_details$itemID), , drop = FALSE]
  out <- item_details[, c("itemID", "label"), drop = FALSE]
  rownames(out) <- NULL
  out
}

#' 將 dimMeta 編修表（含波次信度）套用到 domain 資料（匯出 Excel 用）
apply_meta_edit_overlay <- function(d, meta_df, wave) {
  if (is.null(d) || is.null(meta_df) || nrow(meta_df) == 0L) return(d)

  dm <- ensure_dim_meta_cols(d$dimMeta)
  for (i in seq_len(nrow(meta_df))) {
    did <- meta_df$dimID[i]
    idx <- which(dm$dimID == did)
    if (length(idx) == 0L) next
    dm$dimName[idx] <- clean_meta_str(meta_df$dimName[i],
                                      default = dm$dimName[idx])
    dm$alias[idx]   <- clean_meta_str(meta_df$alias[i])
    dm$note[idx]    <- clean_meta_str(meta_df$note[i])
  }

  dr <- d$dimReliability
  if (is.null(dr) || !is.data.frame(dr) || nrow(dr) == 0L) {
    dr <- data.frame(
      dimID = character(0), wave = character(0),
      reliability = numeric(0), stringsAsFactors = FALSE
    )
  }

  for (i in seq_len(nrow(meta_df))) {
    did     <- meta_df$dimID[i]
    rel_txt <- clean_meta_str(meta_df$reliability[i])
    dr <- dr[!(dr$dimID == did & dr$wave == wave), , drop = FALSE]
    if (nzchar(rel_txt)) {
      rel_num <- suppressWarnings(as.numeric(rel_txt))
      if (!is.na(rel_num)) {
        dr <- rbind(dr, data.frame(
          dimID = did, wave = wave, reliability = rel_num,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  d$dimMeta <- dm
  d$dimReliability <- dr
  d
}

#' 取得構面在特定波次的信度（波次專屬優先，否則 dimMeta.reliability）
get_dim_reliability <- function(dim_id, wave, dim_meta, dim_rel = NULL) {
  if (!is.null(dim_rel) && nrow(dim_rel) > 0L &&
      all(c("dimID", "wave", "reliability") %in% names(dim_rel))) {
    hit <- dim_rel$dimID == dim_id & dim_rel$wave == wave
    if (any(hit)) {
      val <- dim_rel$reliability[which(hit)[1L]]
      if (!is.na(val) && nzchar(as.character(val))) return(as.numeric(val))
    }
  }
  if (!is.null(dim_meta) && nrow(dim_meta) > 0L) {
    hit <- dim_meta$dimID == dim_id
    if (any(hit) && "reliability" %in% names(dim_meta)) {
      val <- dim_meta$reliability[which(hit)[1L]]
      if (!is.na(val) && nzchar(as.character(val))) return(as.numeric(val))
    }
  }
  NA_real_
}
#' @param df       矩陣 data.frame
#' @param item_id  itemID 字串
#' @param item_ver itemVer 整數
#' @param col_names 所有待更新欄位名稱
#' @param checked_cols 勾選（=1）的欄位名稱
update_item_row <- function(df, item_id, item_ver, col_names, checked_cols) {
  idx <- which(df$itemID == item_id & df$itemVer == as.integer(item_ver))
  if (length(idx) != 1L) return(df)
  for (col in col_names) {
    df[[col]][idx] <- as.integer(col %in% checked_cols)
  }
  df
}
