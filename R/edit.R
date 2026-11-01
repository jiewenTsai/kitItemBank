# R/edit.R — 題庫編修的計算（不碰 Shiny、不寫檔）
#
# 每個函式都是「收一份題庫、回一份新的題庫」。改了什麼不用自己記：
# 頁面用 bank_diff(store$base(), 新題庫) 算出改動紀錄。
#
#   set_assignment()        勾選／取消題目的構面或波次（itemDim / itemWave 的 0/1）
#   add_column()            新增構面或波次（構面同時在 dimMeta 加一列）
#   remove_column()         刪除構面或波次（構面同時刪掉 dimMeta 與 dimReliability 的列）
#   set_dim_meta()          改構面資訊（名稱、別名、備註、信度）
#   set_wave_reliability()  設定或拿掉某個構面在某個波次的信度（dimReliability）
#   set_item_fields()       改一題一版的內容（label、value、noResponse、reverseScored…）
#
# 頁面用的小工具：edit_column_counts()、edit_item_rows()、edit_changes_summary()。

EDIT_TABLES <- c("itemDim", "itemWave")

# 構面或波次：畫面上的字
edit_noun <- function(table) {
  # 原文：波次 | 構面
  if (identical(table, "itemWave")) "\u6ce2\u6b21" else "\u69cb\u9762"
}

edit_check_table <- function(table) {
  if (!(is.character(table) && length(table) == 1L && table %in% EDIT_TABLES)) {
    # 原文：table 要是 \"itemDim\" 或 \"itemWave\"。
    kitDesign::kit_abort("table \u8981\u662f \"itemDim\" \u6216 \"itemWave\"\u3002")
  }
  table
}

edit_check_column <- function(bank, table, column) {
  if (!(is.character(column) && length(column) == 1L && !is.na(column) &&
        column %in% get_matrix_cols(bank[[table]]))) {
    # 原文：題庫裡沒有這個 | ： | 。
    kitDesign::kit_abort("\u984c\u5eab\u88e1\u6c92\u6709\u9019\u500b", edit_noun(table), "\uff1a", column, "\u3002")
  }
  column
}

# 構面、波次的欄（波次照月齡排）
edit_columns <- function(bank, table) {
  table <- edit_check_table(table)
  if (table == "itemWave") get_wave_cols(bank$itemWave) else get_matrix_cols(bank$itemDim)
}

#' 勾選或取消題目的構面／波次
#'
#' @param bank 題庫
#' @param table "itemDim"（構面）或 "itemWave"（波次）
#' @param column 構面或波次的欄名
#' @param keys data.frame(itemID, itemVer)：要改的題目
#' @param value 1（勾選）或 0（取消）
#' @return 新的題庫
#' @noRd
set_assignment <- function(bank, table = c("itemDim", "itemWave"), column, keys, value) {
  table <- match.arg(table)
  edit_check_column(bank, table, column)
  value <- suppressWarnings(as.integer(value))
  if (length(value) != 1L || is.na(value) || !value %in% c(0L, 1L)) {
    # 原文：value 要是 0 或 1。
    kitDesign::kit_abort("value \u8981\u662f 0 \u6216 1\u3002")
  }
  if (is.null(keys) || nrow(keys) == 0L) return(bank)
  df <- bank[[table]]
  idx <- match(paste(keys$itemID, as.integer(keys$itemVer), sep = "\r"),
               paste(df$itemID, df$itemVer, sep = "\r"))
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0L) return(bank)
  df[[column]][idx] <- value
  bank[[table]] <- df
  bank
}

#' 新增一個構面或波次（全部題目先是 0）
#'
#' 構面同時在 dimMeta 加一列（dimName = 名稱）。
#' @noRd
add_column <- function(bank, table = c("itemDim", "itemWave"), name) {
  table <- match.arg(table)
  noun <- edit_noun(table)
  name <- trimws(as.character(name %||% ""))
  if (length(name) != 1L || is.na(name) || !nzchar(name)) {
    # 原文：先寫 | 的名稱。
    kitDesign::kit_abort("\u5148\u5beb", noun, "\u7684\u540d\u7a31\u3002")
  }
  if (name %in% names(bank[[table]])) {
    # 原文：已經有「 | 」這個 | ，換一個名稱。
    kitDesign::kit_abort("\u5df2\u7d93\u6709\u300c", name, "\u300d\u9019\u500b", noun, "\uff0c\u63db\u4e00\u500b\u540d\u7a31\u3002")
  }
  df <- bank[[table]]
  df[[name]] <- rep(0L, nrow(df))
  bank[[table]] <- df
  if (table == "itemDim") {
    dm <- ensure_dim_meta_cols(bank$dimMeta)
    if (!name %in% dm$dimID) {
      row <- dm[0, , drop = FALSE]
      row[1, ] <- ""
      row$dimID <- name
      row$dimName <- name
      bank$dimMeta <- rbind(dm, row)
    }
  }
  bank
}

#' 刪除一個構面或波次
#'
#' 構面：同時刪掉 dimMeta 那一列與 dimReliability 裡這個構面的列。
#' 波次：同時刪掉 dimReliability 裡這個波次的列。
#' @noRd
remove_column <- function(bank, table = c("itemDim", "itemWave"), name) {
  table <- match.arg(table)
  edit_check_column(bank, table, name)
  df <- bank[[table]]
  df[[name]] <- NULL
  bank[[table]] <- df
  dr <- bank$dimReliability %||% empty_dim_reliability()
  if (table == "itemDim") {
    dm <- bank$dimMeta
    bank$dimMeta <- dm[dm$dimID != name, , drop = FALSE]
    dr <- dr[dr$dimID != name, , drop = FALSE]
  } else {
    dr <- dr[dr$wave != name, , drop = FALSE]
  }
  rownames(bank$dimMeta) <- NULL
  rownames(dr) <- NULL
  bank$dimReliability <- dr
  bank
}

# 信度：空字串或 0–1 之間的數字
edit_parse_reliability <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return("")
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v) || v < 0 || v > 1) {
    # 原文：信度要是 0 到 1 之間的數字（例如 0.85），或留空。現在是「 | 」。
    kitDesign::kit_abort("\u4fe1\u5ea6\u8981\u662f 0 \u5230 1 \u4e4b\u9593\u7684\u6578\u5b57\uff08\u4f8b\u5982 0.85\uff09\uff0c\u6216\u7559\u7a7a\u3002\u73fe\u5728\u662f\u300c", x, "\u300d\u3002")
  }
  x
}

#' 改構面資訊
#'
#' 沒給（NULL）的欄維持原樣；dimName 留空時用 dimID。dimMeta 沒有這個構面時加一列。
#' @param reliability 全部波次的信度：0–1 的數字或空字串
#' @noRd
set_dim_meta <- function(bank, dim_id, dimName = NULL, alias = NULL, note = NULL,
                         reliability = NULL) {
  dm <- ensure_dim_meta_cols(bank$dimMeta)
  if (!(is.character(dim_id) && length(dim_id) == 1L && nzchar(dim_id))) {
    # 原文：先選一個構面。
    kitDesign::kit_abort("\u5148\u9078\u4e00\u500b\u69cb\u9762\u3002")
  }
  if (!dim_id %in% c(dm$dimID, get_matrix_cols(bank$itemDim))) {
    # 原文：題庫裡沒有這個構面： | 。
    kitDesign::kit_abort("\u984c\u5eab\u88e1\u6c92\u6709\u9019\u500b\u69cb\u9762\uff1a", dim_id, "\u3002")
  }
  i <- match(dim_id, dm$dimID)
  if (is.na(i)) {
    row <- dm[0, , drop = FALSE]
    row[1, ] <- ""
    row$dimID <- dim_id
    row$dimName <- dim_id
    dm <- rbind(dm, row)
    i <- nrow(dm)
  }
  clean <- function(x) {
    x <- as.character(x)
    if (length(x) != 1L || is.na(x)) "" else trimws(x)
  }
  if (!is.null(dimName)) {
    v <- clean(dimName)
    dm$dimName[i] <- if (nzchar(v)) v else dim_id
  }
  if (!is.null(alias)) dm$alias[i] <- clean(alias)
  if (!is.null(note)) dm$note[i] <- clean(note)
  if (!is.null(reliability)) dm$reliability[i] <- edit_parse_reliability(reliability)
  bank$dimMeta <- dm
  bank
}

#' 設定某個構面在某個波次的信度（dimReliability）；value 空的時候拿掉那一列
#' @noRd
set_wave_reliability <- function(bank, dim_id, wave, value) {
  if (!dim_id %in% c(bank$dimMeta$dimID, get_matrix_cols(bank$itemDim))) {
    # 原文：題庫裡沒有這個構面： | 。
    kitDesign::kit_abort("\u984c\u5eab\u88e1\u6c92\u6709\u9019\u500b\u69cb\u9762\uff1a", dim_id, "\u3002")
  }
  if (!wave %in% get_matrix_cols(bank$itemWave)) {
    # 原文：題庫裡沒有這個波次： | 。
    kitDesign::kit_abort("\u984c\u5eab\u88e1\u6c92\u6709\u9019\u500b\u6ce2\u6b21\uff1a", wave, "\u3002")
  }
  v <- edit_parse_reliability(if (is.numeric(value)) format(value) else value)
  dr <- bank$dimReliability %||% empty_dim_reliability()
  hit <- dr$dimID == dim_id & dr$wave == wave
  if (!nzchar(v)) {
    dr <- dr[!hit, , drop = FALSE]
  } else if (any(hit)) {
    dr$reliability[which(hit)[1L]] <- as.numeric(v)
    if (sum(hit) > 1L) dr <- dr[-which(hit)[-1L], , drop = FALSE]
  } else {
    dr <- rbind(dr, data.frame(dimID = dim_id, wave = wave, reliability = as.numeric(v),
                               stringsAsFactors = FALSE))
  }
  rownames(dr) <- NULL
  bank$dimReliability <- dr
  bank
}

# ---- 題目內容（itemBank）---------------------------------------------------------

# 可以改的欄位；0/1 的欄位另外檢查
ITEM_FIELDS <- c("label", "value", "noResponse", "reverseScored", "itemType", "respondent",
                 "deprecated", "note", "block", "stemID")
ITEM_FIELDS_01 <- c("reverseScored", "deprecated")

# 欄位畫面上的名稱
# 原文：題目文字 | 選項 | 遺漏值
ITEM_FIELD_LABELS <- c(label = "\u984c\u76ee\u6587\u5b57", value = "\u9078\u9805", noResponse = "\u907a\u6f0f\u503c",
                       # 原文：反向計分 | 題型 | 填答者
                       reverseScored = "\u53cd\u5411\u8a08\u5206", itemType = "\u984c\u578b", respondent = "\u586b\u7b54\u8005",
                       # 原文：停用 | 備註 | 區塊 | 題幹（stemID）
                       deprecated = "\u505c\u7528", note = "\u5099\u8a3b", block = "\u5340\u584a", stemID = "\u984c\u5e79\uff08stemID\uff09")

#' 改一題一版的內容
#'
#' 只改給了的欄位。文字欄去掉前後空白、\r\n 換成 \n；reverseScored、deprecated 要是 0 或 1。
#' @param bank 題庫
#' @param item_id,item_ver 哪一題哪一版
#' @param ... 欄位 = 新的值，例如 label = "…", reverseScored = 1
#' @param fields 也可以用 named list 給（和 ... 合在一起）
#' @return 新的題庫
#' @noRd
set_item_fields <- function(bank, item_id, item_ver, ..., fields = list()) {
  fields <- c(list(...), as.list(fields))
  ib <- bank$itemBank
  item_ver <- suppressWarnings(as.integer(item_ver))
  if (!(is.character(item_id) && length(item_id) == 1L && !is.na(item_id) &&
        length(item_ver) == 1L && !is.na(item_ver))) {
    # 原文：先選一題和一個版本。
    kitDesign::kit_abort("\u5148\u9078\u4e00\u984c\u548c\u4e00\u500b\u7248\u672c\u3002")
  }
  i <- which(ib$itemID == item_id & ib$itemVer == item_ver)
  if (length(i) == 0L) {
    # 原文：題庫裡沒有  |  第  |  版。
    kitDesign::kit_abort("\u984c\u5eab\u88e1\u6c92\u6709 ", item_id, " \u7b2c ", item_ver, " \u7248\u3002")
  }
  if (length(fields) == 0L) return(bank)
  nm <- names(fields)
  if (is.null(nm) || any(!nzchar(nm))) {
    # 原文：每個要改的欄位都要有名稱，例如 label = \"…\"。
    kitDesign::kit_abort("\u6bcf\u500b\u8981\u6539\u7684\u6b04\u4f4d\u90fd\u8981\u6709\u540d\u7a31\uff0c\u4f8b\u5982 label = \"\u2026\"\u3002")
  }
  bad <- setdiff(nm, ITEM_FIELDS)
  if (length(bad) > 0L) {
    # 原文：不能改這些欄位： | 、 | 。可以改的是 
    kitDesign::kit_abort("\u4e0d\u80fd\u6539\u9019\u4e9b\u6b04\u4f4d\uff1a", paste(bad, collapse = "\u3001"), "\u3002\u53ef\u4ee5\u6539\u7684\u662f ",
                         # 原文：、 | 。
                         paste(ITEM_FIELDS, collapse = "\u3001"), "\u3002")
  }
  for (f in nm) {
    v <- fields[[f]]
    if (f %in% ITEM_FIELDS_01) {
      v <- suppressWarnings(as.integer(as.character(v)))
      if (length(v) != 1L || is.na(v) || !v %in% c(0L, 1L)) {
        # 原文：（ | ）要是 0 或 1。
        kitDesign::kit_abort(ITEM_FIELD_LABELS[[f]], "\uff08", f, "\uff09\u8981\u662f 0 \u6216 1\u3002")
      }
      if (!f %in% names(ib)) ib[[f]] <- 0L
    } else {
      if (is.null(v)) v <- ""
      if (length(v) != 1L) {
        # 原文：（ | ）只能是一段文字。
        kitDesign::kit_abort(ITEM_FIELD_LABELS[[f]], "\uff08", f, "\uff09\u53ea\u80fd\u662f\u4e00\u6bb5\u6587\u5b57\u3002")
      }
      v <- as.character(v)
      if (is.na(v)) v <- ""
      v <- gsub("\r\n?", "\n", v)
      v <- trimws(v, whitespace = "[\\h\\v]")
      if (!f %in% names(ib)) ib[[f]] <- ""
    }
    ib[[f]][i] <- v
  }
  bank$itemBank <- ib
  bank
}

# 一題一版的目前內容（list，欄位照 ITEM_FIELDS；沒有的欄位是 "" 或 0）
edit_item_values <- function(bank, item_id, item_ver) {
  ib <- bank$itemBank
  i <- which(ib$itemID == item_id & ib$itemVer == as.integer(item_ver))[1L]
  if (is.na(i)) return(NULL)
  out <- lapply(ITEM_FIELDS, function(f) {
    if (!f %in% names(ib)) return(if (f %in% ITEM_FIELDS_01) 0L else "")
    v <- ib[[f]][i]
    if (f %in% ITEM_FIELDS_01) {
      v <- suppressWarnings(as.integer(v))
      if (is.na(v)) 0L else v
    } else if (is.na(v)) "" else as.character(v)
  })
  stats::setNames(out, ITEM_FIELDS)
}

# 一題有哪些版本（由小到大）
edit_item_versions <- function(bank, item_id) {
  sort(bank$itemBank$itemVer[bank$itemBank$itemID == item_id])
}

# ---- 頁面用的小工具 ------------------------------------------------------------

# 每個構面／波次勾了幾題：data.frame(column, n)，照 edit_columns() 的順序
edit_column_counts <- function(bank, table) {
  cols <- edit_columns(bank, table)
  df <- bank[[table]]
  n <- vapply(cols, function(cl) sum(df[[cl]] == 1L, na.rm = TRUE), integer(1))
  data.frame(column = cols, n = unname(n), stringsAsFactors = FALSE)
}

# 選單的選項：值 = 欄名，名稱 = 「睡眠習慣 · 12 題」
edit_column_choices <- function(bank, table) {
  cnt <- edit_column_counts(bank, table)
  if (nrow(cnt) == 0L) return(character(0))
  # 原文： · 
  stats::setNames(cnt$column, paste0(col_display_labels(cnt$column), " \u00b7 ",
                                     # 原文： 題
                                     kitDesign::kit_num(cnt$n), " \u984c"))
}

# 右邊的題目表：itemID、itemVer、block、label，加上 assigned（0/1）
# keyword：itemID、區塊、題目裡有這幾個字（不分大小寫）；only_assigned：只留勾了的
edit_item_rows <- function(bank, table, column, keyword = "", only_assigned = FALSE) {
  df <- bank[[table]]
  ib <- bank$itemBank
  j <- match(paste(df$itemID, df$itemVer, sep = "\r"),
             paste(ib$itemID, ib$itemVer, sep = "\r"))
  pick <- function(col) {
    if (!col %in% names(ib)) return(rep("", nrow(df)))
    v <- ib[[col]][j]
    v[is.na(v)] <- ""
    v
  }
  out <- data.frame(
    itemID = df$itemID, itemVer = df$itemVer,
    block = pick("block"), label = collapse_newlines(pick("label")),
    assigned = if (!is.null(column) && column %in% names(df)) df[[column]] else rep(0L, nrow(df)),
    stringsAsFactors = FALSE
  )
  kw <- trimws(keyword %||% "")
  if (length(kw) == 1L && !is.na(kw) && nzchar(kw)) {
    hay <- tolower(paste(out$itemID, out$block, out$label, sep = "\r"))
    out <- out[grepl(tolower(kw), hay, fixed = TRUE), , drop = FALSE]
  }
  if (isTRUE(only_assigned)) out <- out[out$assigned == 1L, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# 「本次改動」一句話：睡眠習慣 +3 題、−1 題；作息 +2 題；新增構面「X」；構面資訊 2 處
edit_changes_summary <- function(changes) {
  if (is.null(changes) || nrow(changes) == 0L) return("")
  parts <- character(0)
  for (tbl in EDIT_TABLES) {
    noun <- edit_noun(tbl)
    # 原文：表
    ch <- changes[changes[["\u8868"]] == tbl, , drop = FALSE]
    # 原文：題號或構面
    whole <- ch[!nzchar(ch[["\u984c\u865f\u6216\u69cb\u9762"]]), , drop = FALSE]
    # 原文：欄 | 改成 | （新增）
    added <- whole[["\u6b04"]][whole[["\u6539\u6210"]] == "\uff08\u65b0\u589e\uff09"]
    # 原文：欄 | 改成 | （刪除）
    gone <- whole[["\u6b04"]][whole[["\u6539\u6210"]] == "\uff08\u522a\u9664\uff09"]
    # 原文：題號或構面
    cells <- ch[nzchar(ch[["\u984c\u865f\u6216\u69cb\u9762"]]), , drop = FALSE]
    # 原文：欄
    for (cl in unique(cells[["\u6b04"]])) {
      # 原文：欄
      x <- cells[cells[["\u6b04"]] == cl, , drop = FALSE]
      # 原文：改成
      plus <- sum(x[["\u6539\u6210"]] == "1")
      # 原文：改成
      minus <- sum(x[["\u6539\u6210"]] == "0")
      # 原文： 題
      bits <- c(if (plus > 0L) paste0("+", kitDesign::kit_num(plus), " \u984c"),
                # 原文：− |  題
                if (minus > 0L) paste0("\u2212", kitDesign::kit_num(minus), " \u984c"))
      lab <- col_display_labels(cl)
      # 原文：（新增的 | ）
      if (cl %in% added) lab <- paste0(lab, "\uff08\u65b0\u589e\u7684", noun, "\uff09")
      # 原文：、
      parts <- c(parts, paste0(lab, " ", paste(bits, collapse = "\u3001")))
    }
    # 原文：欄
    for (cl in setdiff(added, cells[["\u6b04"]])) {
      # 原文：新增 | 「 | 」
      parts <- c(parts, paste0("\u65b0\u589e", noun, "\u300c", col_display_labels(cl), "\u300d"))
    }
    for (cl in gone) {
      # 原文：刪除 | 「 | 」
      parts <- c(parts, paste0("\u522a\u9664", noun, "\u300c", col_display_labels(cl), "\u300d"))
    }
  }
  # 構面資訊：新增、刪除構面時 dimMeta 的 dimID 列與跟著刪的信度不另外算
  # 原文：題號或構面 | 表
  gone_dims <- changes[["\u984c\u865f\u6216\u69cb\u9762"]][changes[["\u8868"]] == "dimMeta" &
                                    # 原文：欄
                                    changes[["\u6b04"]] == "dimID" &
                                    # 原文：改成 | （刪除）
                                    changes[["\u6539\u6210"]] == "\uff08\u522a\u9664\uff09"]
  # 原文：欄 | 表
  gone_waves <- changes[["\u6b04"]][changes[["\u8868"]] == "itemWave" &
                                 # 原文：改成 | （刪除）
                                 changes[["\u6539\u6210"]] == "\uff08\u522a\u9664\uff09"]
  # 原文：表 | 欄
  meta <- changes[(changes[["\u8868"]] == "dimMeta" & changes[["\u6b04"]] != "dimID") |
                    # 原文：表
                    (changes[["\u8868"]] == "dimReliability" &
                       # 原文：題號或構面
                       !changes[["\u984c\u865f\u6216\u69cb\u9762"]] %in% gone_dims &
                       # 原文：欄 | reliability（ | ）
                       !changes[["\u6b04"]] %in% paste0("reliability\uff08", gone_waves, "\uff09")), ,
                  drop = FALSE]
  if (nrow(meta) > 0L) {
    # 原文：構面資訊  |  處
    parts <- c(parts, paste0("\u69cb\u9762\u8cc7\u8a0a ", kitDesign::kit_num(nrow(meta)), " \u8655"))
  }
  # 原文：表
  items <- changes[changes[["\u8868"]] == "itemBank", , drop = FALSE]
  if (nrow(items) > 0L) {
    # 原文：欄
    whole <- items[["\u6b04"]] == "itemVer"
    # 原文：改成 | （新增）
    n_add <- sum(whole & items[["\u6539\u6210"]] == "\uff08\u65b0\u589e\uff09")
    # 原文：改成 | （刪除）
    n_del <- sum(whole & items[["\u6539\u6210"]] == "\uff08\u522a\u9664\uff09")
    cells <- items[!whole, , drop = FALSE]
    if (nrow(cells) > 0L) {
      # 原文：題號或構面 | 版本
      n_items <- length(unique(paste(cells[["\u984c\u865f\u6216\u69cb\u9762"]], cells[["\u7248\u672c"]])))
      # 原文：題目內容  |  題 
      parts <- c(parts, paste0("\u984c\u76ee\u5167\u5bb9 ", kitDesign::kit_num(n_items), " \u984c ",
                               # 原文： 處
                               kitDesign::kit_num(nrow(cells)), " \u8655"))
    }
    # 原文：新增  |  個題目版本
    if (n_add > 0L) parts <- c(parts, paste0("\u65b0\u589e ", kitDesign::kit_num(n_add), " \u500b\u984c\u76ee\u7248\u672c"))
    # 原文：刪除  |  個題目版本
    if (n_del > 0L) parts <- c(parts, paste0("\u522a\u9664 ", kitDesign::kit_num(n_del), " \u500b\u984c\u76ee\u7248\u672c"))
  }
  # 原文：；
  paste(parts, collapse = "\uff1b")
}

# 下載檔名：KIT_parent_2026-11-01_改.xlsx
edit_download_filename <- function(domain, date = Sys.Date()) {
  # 原文：_修改包.xlsx
  paste0("KIT_", domain, "_", format(as.Date(date), "%Y-%m-%d"), "_\u4fee\u6539\u5305.xlsx")
}
