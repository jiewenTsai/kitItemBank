# R/utils.R — 共用小工具

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0L) x else y

#' 取得矩陣 df 的非鍵欄位名稱（不寫死清單，動態推導）
#' @noRd
get_matrix_cols <- function(df) {
  setdiff(names(df), c("itemID", "itemVer"))
}

#' 從波次欄名取出排序用主鍵（月齡／年級數字）
#' @noRd
wave_col_sort_primary <- function(col_name) {
  # 0.3 的施測代碼：M03M036、M36M084G01 → 月齡
  if (grepl("^M\\d{2}M\\d{3}", col_name, perl = TRUE)) {
    return(as.integer(sub("^M\\d{2}M(\\d{3}).*", "\\1", col_name, perl = TRUE)))
  }
  if (grepl("^M(\\d+)", col_name, perl = TRUE)) {
    return(as.integer(sub("^M(\\d+).*", "\\1", col_name, perl = TRUE)))
  }
  if (grepl("^(\\d+)M", col_name, perl = TRUE)) {
    return(as.integer(sub("^(\\d+)M.*", "\\1", col_name, perl = TRUE)))
  }
  # 原文：^小(\\d+)
  if (grepl("^\u5c0f(\\d+)", col_name, perl = TRUE)) {
    # 原文：^小(\\d+).*
    return(5000L + as.integer(sub("^\u5c0f(\\d+).*", "\\1", col_name, perl = TRUE)))
  }
  # 原文：^國(\\d+)
  if (grepl("^\u570b(\\d+)", col_name, perl = TRUE)) {
    # 原文：^國(\\d+).*
    return(10000L + as.integer(sub("^\u570b(\\d+).*", "\\1", col_name, perl = TRUE)))
  }
  Inf
}

#' 波次欄名次要排序：小月齡 < 無後綴 < 大月齡；保母 < 保母b < 機構 < 機構b
#' @noRd
wave_col_sort_secondary <- function(col_name) {
  # 0.3 的施測代碼：同月齡先排 M03 世代，再排 M36 世代
  if (grepl("^M\\d{2}M\\d{3}", col_name, perl = TRUE)) {
    return(as.integer(substr(col_name, 2L, 3L)))
  }
  # 原文：小月齡
  if (grepl("\u5c0f\u6708\u9f61", col_name, fixed = TRUE)) return(1L)
  # 原文：大月齡
  if (grepl("\u5927\u6708\u9f61", col_name, fixed = TRUE)) return(3L)
  # 原文：保母b
  if (grepl("\u4fdd\u6bcdb", col_name, fixed = TRUE)) return(2L)
  # 原文：保母
  if (grepl("\u4fdd\u6bcd", col_name, fixed = TRUE)) return(1L)
  # 原文：機構b
  if (grepl("\u6a5f\u69cbb", col_name, fixed = TRUE)) return(4L)
  # 原文：機構
  if (grepl("\u6a5f\u69cb", col_name, fixed = TRUE)) return(3L)
  2L
}

#' 波次欄由左至右：依 M／數字M／小／國 後數字升冪
#' @noRd
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
#' @noRd
get_wave_cols <- function(df) {
  sort_wave_cols(get_matrix_cols(df))
}

#' 清除欄位名稱中的內嵌換行，供顯示用
#'
#' `plain = TRUE` 時，認得的波次代碼加上白話（見 wave_label()）；其他欄名照舊。
#' @noRd
col_display_labels <- function(col_names, plain = FALSE) {
  out <- gsub("[\r\n]+", " ", col_names, perl = TRUE)
  if (isTRUE(plain)) out <- wave_label(out)
  out
}

#' 建立 selectize choices：value = 原始欄名（含特殊字元），label = 清理後文字
#'
#' 波次代碼的 label 加白話（「M036大 · 36 個月（大月齡世代）」）；value 不變。
#' @noRd
make_col_choices <- function(col_names) {
  lab <- col_display_labels(col_names, plain = TRUE)
  # 施測代碼再加上舊名稱，選單打 M036小、M03W36 也找得到
  al <- admin_alias_text(col_names)
  # 原文：（舊稱 | ）
  lab[nzchar(al)] <- paste0(lab[nzchar(al)], "\uff08\u820a\u7a31 ", al[nzchar(al)], "\uff09")
  # 年級別名：打 G01、國小一年級也找得到
  gr <- admin_alias_text(col_names, grade = TRUE)
  # 原文：・
  lab[nzchar(gr)] <- paste0(lab[nzchar(gr)], " \u30fb ", gr[nzchar(gr)])
  stats::setNames(col_names, lab)
}

#' 波次代碼的白話說明
#'
#' 「M036大」→「36 個月（大月齡世代）」；「M012保母」→「12 個月（保母）」；
#' 「M084」→「84 個月」；「國1_大月齡」→「國一（大月齡世代）」。
#' 認不得的代碼回傳 NA。
#' @noRd
wave_meaning <- function(x) {
  x <- trimws(gsub("[\r\n]+", " ", as.character(x), perl = TRUE))
  out <- rep(NA_character_, length(x))
  # 原文：大 | 大月齡世代 | 小 | 小月齡世代
  cohort <- c("\u5927" = "\u5927\u6708\u9f61\u4e16\u4ee3", "\u5c0f" = "\u5c0f\u6708\u9f61\u4e16\u4ee3")
  # 原文：一 | 二 | 三 | 四 | 五 | 六 | 七 | 八 | 九
  num_zh <- c("\u4e00", "\u4e8c", "\u4e09", "\u56db", "\u4e94", "\u516d", "\u4e03", "\u516b", "\u4e5d")
  # 0.3 施測代碼：M03M036 → 36 個月（M03 世代）；M36M084G01 → 84 個月・小一（M36 世代）
  a <- regmatches(x, regexec("^(M[0-9]{2})M([0-9]{3})(?:G([0-9]{2}))?[AB]?$", x, perl = TRUE))
  for (i in which(lengths(a) > 0L)) {
    g <- a[[i]]
    # 原文： 個月
    months <- paste0(as.integer(g[3]), " \u500b\u6708")
    gr <- if (nzchar(g[4])) as.integer(g[4]) else NA_integer_
    # 原文：・小 | ・國
    grade <- if (is.na(gr)) "" else if (gr <= 6L) paste0("\u30fb\u5c0f", num_zh[gr]) else
      paste0("\u30fb\u570b", num_zh[gr - 6L])
    # 原文：（ |  世代）
    out[i] <- paste0(months, grade, "\uff08", g[2], " \u4e16\u4ee3\uff09")
  }
  # M + 月齡 + 後綴
  # 原文：^M0*([0-9]+)(大|小|保母|機構)?(b)?$
  m <- regmatches(x, regexec("^M0*([0-9]+)(\u5927|\u5c0f|\u4fdd\u6bcd|\u6a5f\u69cb)?(b)?$", x, perl = TRUE))
  for (i in which(lengths(m) > 0L)) {
    g <- m[[i]]
    # 原文： 個月
    months <- paste0(as.integer(g[2]), " \u500b\u6708")
    suffix <- g[3]
    extra <- if (!nzchar(suffix)) "" else if (suffix %in% names(cohort)) cohort[[suffix]] else
      paste0(suffix, if (nzchar(g[4])) " b")
    # 原文：（ | ）
    out[i] <- if (nzchar(extra)) paste0(months, "\uff08", extra, "\uff09") else months
  }
  # 小4_小月齡、國1_大月齡
  # 原文：^(小|國)([1-9])(?:_(大|小)月齡)?$
  g2 <- regmatches(x, regexec("^(\u5c0f|\u570b)([1-9])(?:_(\u5927|\u5c0f)\u6708\u9f61)?$", x, perl = TRUE))
  for (i in which(lengths(g2) > 0L)) {
    g <- g2[[i]]
    grade <- paste0(g[2], num_zh[as.integer(g[3])])
    # 原文：（ | ）
    out[i] <- if (nzchar(g[4])) paste0(grade, "\uff08", cohort[[g[4]]], "\uff09") else grade
  }
  out
}

#' 波次代碼 + 白話：「M036大 · 36 個月（大月齡世代）」；認不得的代碼照原樣
#' @noRd
wave_label <- function(x) {
  x <- as.character(x)
  m <- wave_meaning(x)
  ok <- !is.na(m)
  # 原文： · 
  x[ok] <- paste0(x[ok], " \u00b7 ", m[ok])
  x
}

#' 截斷長字串供 DT 顯示，避免表格列高暴漲
#' @noRd
trunc_label <- function(x, width = 80) {
  stringr::str_trunc(as.character(x), width = width, side = "right", ellipsis = "\u2026")
}

#' 壓平多行文字（換行 → " / "）
#' @noRd
collapse_newlines <- function(x) {
  stringr::str_replace_all(as.character(x), "\\r?\\n", " / ")
}

#' 清理 dimMeta 文字欄（NA / 空白 → default）
#' @noRd
clean_meta_str <- function(x, default = "") {
  if (length(x) == 0L || is.null(x)) return(default)
  if (length(x) != 1L) x <- x[[1L]]
  if (is.na(x)) return(default)
  val <- trimws(as.character(x))
  if (!nzchar(val)) default else val
}

#' 構面顯示名稱：alias 優先，其次 dimName，再 fallback
#' @noRd
dim_display_name <- function(meta_row, fallback = "") {
  if (is.null(meta_row) || nrow(meta_row) == 0L) return(fallback)
  alias <- if ("alias" %in% names(meta_row))
    clean_meta_str(meta_row$alias[1L]) else ""
  name  <- if ("dimName" %in% names(meta_row))
    clean_meta_str(meta_row$dimName[1L], default = fallback) else fallback
  if (nzchar(alias)) alias else name
}

#' 自編修表依 dimID 取列（避免 DT 分頁列索引錯位）
#' @noRd
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
#' @noRd
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
#' @noRd
format_dim_report_heading <- function(dim_name_raw, alias = "") {
  raw <- clean_meta_str(dim_name_raw)
  ali <- clean_meta_str(alias)
  if (nzchar(ali) && !identical(ali, raw)) {
    paste0(raw, "\uff08", ali, "\uff09")
  } else {
    raw
  }
}

#' 取得構面在特定波次的信度（波次專屬優先，否則 dimMeta.reliability）
#' @noRd
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
