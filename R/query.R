# R/query.R — 題項查詢的計算（純函式，沒有 Shiny）
#
# 題項查詢頁與「用 R 怎麼做」都呼叫這裡的函式：
#   query_items()  依關鍵字、波次、構面、區塊、題型、填答者找題目
#   item_detail()  一題的細節：出現的波次、構面、其他版本
#   label_diff()   兩個版本的文字差在哪
#   crosswalk()    題目 × 波次的 ✓ 表
#   wave_counts()  區塊 × 波次的題數（熱圖）
#   query_code()   把目前的條件寫成 R 程式碼

# 題目的鍵（itemID + itemVer）
item_key <- function(id, ver) paste(id, as.integer(ver), sep = "\r")

# 把 itemWave／itemDim 對齊到 items 的列：回傳 0/1 整數矩陣（列 = items，欄 = cols）
# 題庫裡找不到的題目整列是 0。
bank_matrix <- function(df, items, cols) {
  m <- matrix(0L, nrow = nrow(items), ncol = length(cols),
              dimnames = list(NULL, cols))
  if (nrow(items) == 0L || length(cols) == 0L || is.null(df) || nrow(df) == 0L) return(m)
  idx <- match(item_key(items$itemID, items$itemVer), item_key(df$itemID, df$itemVer))
  ok <- !is.na(idx)
  if (!any(ok)) return(m)
  vals <- as.matrix(df[idx[ok], cols, drop = FALSE])
  vals <- suppressWarnings(matrix(as.integer(vals), nrow = sum(ok)))
  vals[is.na(vals)] <- 0L
  m[ok, ] <- vals
  m
}

# 填答者欄（"家長,親友"）拆成一個個值
split_respondent <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  # 原文：[,，、]
  lapply(strsplit(x, "[,\uff0c\u3001]"), function(v) trimws(v[nzchar(trimws(v))]))
}

# 關鍵字拆成詞（空白分隔，全形空白也算）
keyword_terms <- function(keyword) {
  keyword <- paste(as.character(keyword %||% ""), collapse = " ")
  # 原文：[[:space:]　]+
  terms <- strsplit(trimws(keyword), "[[:space:]\u3000]+", perl = TRUE)[[1]]
  terms[nzchar(terms) & terms != "*"]
}

# ---- 關鍵字正規化 -------------------------------------------------------------
# 關鍵字和被找的文字都先過 normalize_text()，所以兩邊怎麼寫都對得上：
#   全形英數與標點 → 半形（３Ｃ → 3c），全形空白 → 半形空白，英文一律小寫；
#   常用簡體字 → 繁體（亲子 → 親子）；異體字當成同一個字（念／唸、台／臺…）。
# 簡轉繁只收繁體裡不會單獨使用的簡體字（不收 后、里、干、系、范、游…，免得把不同的字併在一起）。
# 原文：亲视电脑机语读书时间动觉发头…（簡體）
NORM_S2T_FROM <- paste0(
  # 原文：亲视电脑机语读书时间动觉发头饭饮学习戏绘图画话说听写认识题问关体医疗药气爱妈爷儿
  "\u4eb2\u89c6\u7535\u8111\u673a\u8bed\u8bfb\u4e66\u65f6\u95f4\u52a8\u89c9\u53d1\u5934\u996d\u996e\u5b66\u4e60\u620f\u7ed8\u56fe\u753b\u8bdd\u8bf4\u542c\u5199\u8ba4\u8bc6\u9898\u95ee\u5173\u4f53\u533b\u7597\u836f\u6c14\u7231\u5988\u7237\u513f",
  # 原文：园师长张开门车网络买卖钱经济职业户国应对会来个们这还没过为吗样东运两岁级龄数设计
  "\u56ed\u5e08\u957f\u5f20\u5f00\u95e8\u8f66\u7f51\u7edc\u4e70\u5356\u94b1\u7ecf\u6d4e\u804c\u4e1a\u6237\u56fd\u5e94\u5bf9\u4f1a\u6765\u4e2a\u4eec\u8fd9\u8fd8\u6ca1\u8fc7\u4e3a\u5417\u6837\u4e1c\u8fd0\u4e24\u5c81\u7ea7\u9f84\u6570\u8bbe\u8ba1",
  # 原文：环压担忧虑绪满婴宝养帮声词汉规则频节广讯许诉护调查伤脸觉
  "\u73af\u538b\u62c5\u5fe7\u8651\u7eea\u6ee1\u5a74\u5b9d\u517b\u5e2e\u58f0\u8bcd\u6c49\u89c4\u5219\u9891\u8282\u5e7f\u8baf\u8bb8\u8bc9\u62a4\u8c03\u67e5\u4f24\u8138\u89c9"
)
NORM_S2T_TO <- paste0(
  # 原文：親視電腦機語讀書時間動覺發頭飯飲學習戲繪圖畫話說聽寫認識題問關體醫療藥氣愛媽爺兒
  "\u89aa\u8996\u96fb\u8166\u6a5f\u8a9e\u8b80\u66f8\u6642\u9593\u52d5\u89ba\u767c\u982d\u98ef\u98f2\u5b78\u7fd2\u6232\u7e6a\u5716\u756b\u8a71\u8aaa\u807d\u5beb\u8a8d\u8b58\u984c\u554f\u95dc\u9ad4\u91ab\u7642\u85e5\u6c23\u611b\u5abd\u723a\u5152",
  # 原文：園師長張開門車網絡買賣錢經濟職業戶國應對會來個們這還沒過為嗎樣東運兩歲級齡數設計
  "\u5712\u5e2b\u9577\u5f35\u958b\u9580\u8eca\u7db2\u7d61\u8cb7\u8ce3\u9322\u7d93\u6fdf\u8077\u696d\u6236\u570b\u61c9\u5c0d\u6703\u4f86\u500b\u5011\u9019\u9084\u6c92\u904e\u70ba\u55ce\u6a23\u6771\u904b\u5169\u6b72\u7d1a\u9f61\u6578\u8a2d\u8a08",
  # 原文：環壓擔憂慮緒滿嬰寶養幫聲詞漢規則頻節廣訊許訴護調查傷臉覺
  "\u74b0\u58d3\u64d4\u6182\u616e\u7dd2\u6eff\u5b30\u5bf6\u990a\u5e6b\u8072\u8a5e\u6f22\u898f\u5247\u983b\u7bc0\u5ee3\u8a0a\u8a31\u8a34\u8b77\u8abf\u67e5\u50b7\u81c9\u89ba"
)
# 異體字 → 同一個寫法（左邊換成右邊）
# 原文：唸臺裏着綫么爲衆羣牀却册啓週峯
# 原文：唸臺裏着綫么爲衆羣牀却册啓週峯
NORM_VAR_FROM <- "\u5538\u81fa\u88cf\u7740\u7dab\u4e48\u7232\u8846\u7fa3\u7240\u5374\u518c\u5553\u9031\u5cef"
# 原文：念台裡著線麼為眾群床卻冊啟周峰
NORM_VAR_TO   <- "\u5ff5\u53f0\u88e1\u8457\u7dda\u9ebc\u70ba\u773e\u7fa4\u5e8a\u537b\u518a\u555f\u5468\u5cf0"

norm_tables <- local({
  tab <- NULL
  function() {
    if (is.null(tab)) {
      # 去掉重複的字（chartr 一個字只能對到一個字）
      dedup <- function(from, to) {
        f <- strsplit(from, "")[[1]]; t <- strsplit(to, "")[[1]]
        stopifnot(length(f) == length(t))
        keep <- !duplicated(f) & f != t
        c(paste(f[keep], collapse = ""), paste(t[keep], collapse = ""))
      }
      tab <<- list(
        # 全形 ！～ (FF01–FF5E) → 半形 !~ (21–7E)；全形空白 → 空白
        wide = c(paste0(intToUtf8(0xFF01:0xFF5E), "\u3000"),
                 paste0(intToUtf8(0x21:0x7E), " ")),
        s2t = dedup(NORM_S2T_FROM, NORM_S2T_TO),
        var = dedup(NORM_VAR_FROM, NORM_VAR_TO)
      )
    }
    tab
  }
})

#' 正規化文字，供關鍵字比對（全形 → 半形、小寫、簡 → 繁、異體字 → 同一個字）
#' @noRd
normalize_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  tb <- norm_tables()
  x <- chartr(tb$wide[1], tb$wide[2], x)
  x <- tolower(x)
  x <- chartr(tb$s2t[1], tb$s2t[2], x)
  chartr(tb$var[1], tb$var[2], x)
}

# 構面的顯示名稱：dimMeta 的 dimName，沒有就用 dimID
dim_names <- function(bank, dim_ids) {
  dm <- bank$dimMeta
  if (is.null(dm) || !all(c("dimID", "dimName") %in% names(dm))) return(dim_ids)
  dim_ids <- as.character(dim_ids)
  nm <- as.character(dm$dimName)[match(dim_ids, dm$dimID)]
  bad <- is.na(nm) | !nzchar(trimws(nm))
  nm[bad] <- dim_ids[bad]
  nm
}

#' Search items in an item bank
#'
#' Finds items by keyword, wave, dimension, block, item type, and respondent.
#' All conditions are combined with AND; an empty condition means "no
#' restriction".
#'
#' @param bank An item bank from [load_bank()] or [read_bank_xlsx()].
#' @param keyword Search text. Whitespace separates terms (see
#'   `keyword_match`). Matching is case-insensitive and ignores full-width vs
#'   half-width forms (`"３Ｃ"` = `"3C"`), common simplified vs traditional
#'   characters (`"亲子"` = `"親子"`), and variant characters (`"念"` = `"唸"`,
#'   `"台"` = `"臺"`). A term ending in `*` matches the beginning of `itemID`
#'   (for example `"sle*"`).
#' @param fields `"text"` searches `label` and `stemID`, and matches `itemID`
#'   only from its beginning (`"sle"` finds `sleep01`; `"3C"` does not find
#'   `pfB0203c`); `"all"` also searches anywhere in `itemID`, `value`, `note`,
#'   and `block`.
#' @param keyword_match `"all"`: every term must match; `"any"`: at least one.
#' @param waves Wave column names of `bank$itemWave` (for example `"M036小"`).
#' @param wave_match `"any"`: the item appears in at least one of `waves`;
#'   `"all"`: it appears in every one of them.
#' @param dims Dimension IDs (column names of `bank$itemDim`).
#' @param dim_match `"any"` or `"all"`, as for `wave_match`.
#' @param blocks,item_types Values of `block` and `itemType` to keep; `NULL`
#'   or empty keeps all.
#' @param respondents Respondents to keep. The `respondent` column can hold
#'   several comma-separated values (`"家長,親友"`); an item is kept when any
#'   of them is in `respondents`.
#' @param include_deprecated Keep items with `deprecated = 1`. Default `FALSE`.
#' @return The matching rows of `bank$itemBank` (all its columns, in bank
#'   order) plus `n_waves`: the number of waves the item appears in.
#' @examples
#' bank <- load_bank("parent")
#' query_items(bank, keyword = "睡眠")
#' query_items(bank, waves = c("M036小", "M048小"), wave_match = "all")
#' @export
query_items <- function(bank, keyword = "", fields = c("text", "all"),
                        keyword_match = c("all", "any"),
                        waves = character(), wave_match = c("any", "all"),
                        dims = character(), dim_match = c("any", "all"),
                        blocks = NULL, item_types = NULL, respondents = NULL,
                        include_deprecated = FALSE) {
  fields <- match.arg(fields)
  keyword_match <- match.arg(keyword_match)
  wave_match <- match.arg(wave_match)
  dim_match <- match.arg(dim_match)
  ib <- bank$itemBank
  keep <- rep(TRUE, nrow(ib))
  chr <- function(col) {
    x <- if (col %in% names(ib)) as.character(ib[[col]]) else rep("", nrow(ib))
    x[is.na(x)] <- ""
    x
  }

  if (!isTRUE(include_deprecated) && "deprecated" %in% names(ib)) {
    dep <- suppressWarnings(as.integer(ib$deprecated))
    keep <- keep & (is.na(dep) | dep != 1L)
  }

  terms <- keyword_terms(keyword)
  if (length(terms)) {
    cols <- c("label", "stemID",
              if (fields == "all") c("itemID", "value", "note", "block"))
    hay <- normalize_text(do.call(paste, c(lapply(cols, chr), sep = "\u001f")))
    ids <- normalize_text(chr("itemID"))
    hits <- lapply(normalize_text(terms), function(t) {
      if (endsWith(t, "*")) return(startsWith(ids, sub("\\*+$", "", t)))
      # 題目文字：itemID 只從開頭比（"3C" 不會對到 pfB0203c）
      grepl(t, hay, fixed = TRUE) | (fields == "text" & startsWith(ids, t))
    })
    keep <- keep & Reduce(if (keyword_match == "all") `&` else `|`, hits)
  }

  if (length(blocks)) keep <- keep & chr("block") %in% blocks
  if (length(item_types)) keep <- keep & chr("itemType") %in% item_types
  if (length(respondents)) {
    rs <- split_respondent(chr("respondent"))
    keep <- keep & vapply(rs, function(v) any(v %in% respondents), logical(1))
  }

  match_matrix <- function(df, sel, how, what) {
    unknown <- setdiff(sel, get_matrix_cols(df))
    if (length(unknown)) {
      warning("Not in the bank (ignored) - ", what, ": ",
              paste(unknown, collapse = ", "), call. = FALSE)
      sel <- setdiff(sel, unknown)
      if (length(sel) == 0L) return(rep(TRUE, nrow(ib)))
    }
    m <- bank_matrix(df, ib, sel)
    if (how == "all") rowSums(m) == length(sel) else rowSums(m) > 0L
  }
  if (length(waves)) keep <- keep & match_matrix(bank$itemWave, waves, wave_match, "waves")
  if (length(dims)) keep <- keep & match_matrix(bank$itemDim, dims, dim_match, "dims")

  out <- ib[keep, , drop = FALSE]
  wave_cols <- get_wave_cols(bank$itemWave)
  out$n_waves <- as.integer(rowSums(bank_matrix(bank$itemWave, out, wave_cols)))
  rownames(out) <- NULL
  out
}

#' Details of one item
#'
#' @param bank An item bank from [load_bank()].
#' @param item_id,item_ver The item (`itemID`, `itemVer`).
#' @return `NULL` when the item is not in the bank; otherwise a list:
#'   * `item`: the `itemBank` row (one-row data.frame);
#'   * `waves`: the waves it appears in (character, in age order);
#'   * `dims`: a data.frame with `dimID` and `dimName`;
#'   * `other_versions`: `itemBank` rows with the same `itemID` and another
#'     `itemVer` (sorted by `itemVer`), plus a list column `waves` and a list
#'     column `differs`: which of `label`, `value`, `noResponse`,
#'     `reverseScored`, `itemType`, `respondent` differ from this version;
#'   * `timeline`: version × wave table, see [version_timeline()];
#'   * `same_stem`: other items (another `itemID`) with the same `stemID`:
#'     `itemID`, `itemVer`, `label`, `deprecated`, and a list column `waves`.
#' @examples
#' bank <- load_bank("parent")
#' item_detail(bank, "area1_1", 2)
#' @export
item_detail <- function(bank, item_id, item_ver) {
  ib <- bank$itemBank
  row <- ib[ib$itemID == item_id & ib$itemVer == as.integer(item_ver), , drop = FALSE]
  if (nrow(row) == 0L) return(NULL)
  row <- row[1L, , drop = FALSE]
  rownames(row) <- NULL
  wave_cols <- get_wave_cols(bank$itemWave)
  dim_cols <- get_matrix_cols(bank$itemDim)
  waves_of <- function(items) {
    m <- bank_matrix(bank$itemWave, items, wave_cols)
    lapply(seq_len(nrow(items)), function(i) wave_cols[m[i, ] == 1L])
  }
  dm <- bank_matrix(bank$itemDim, row, dim_cols)
  dim_ids <- dim_cols[dm[1L, ] == 1L]
  others <- ib[ib$itemID == item_id & ib$itemVer != as.integer(item_ver), , drop = FALSE]
  others <- others[order(others$itemVer), , drop = FALSE]
  rownames(others) <- NULL
  others$waves <- waves_of(others)
  others$differs <- lapply(seq_len(nrow(others)), function(i) {
    version_differs(others[i, , drop = FALSE], row)
  })
  list(
    item = row,
    waves = waves_of(row)[[1L]],
    dims = data.frame(dimID = dim_ids, dimName = dim_names(bank, dim_ids),
                      stringsAsFactors = FALSE),
    other_versions = others,
    timeline = version_timeline(bank, item_id),
    same_stem = same_stem_items(bank, row, waves_of)
  )
}

# 版本之間要比的欄位
VERSION_FIELDS <- c("label", "value", "noResponse", "reverseScored", "itemType", "respondent")

# 兩個版本（各一列）哪些欄位不一樣（空白、NA 當成一樣的空）
version_differs <- function(a, b) {
  val <- function(df, col) {
    v <- if (col %in% names(df)) as.character(df[[col]][1L]) else ""
    v <- if (is.na(v)) "" else trimws(gsub("\r?\n", "\n", v))
    v
  }
  VERSION_FIELDS[vapply(VERSION_FIELDS, function(f) !identical(val(a, f), val(b, f)), logical(1))]
}

# 和 row 同題幹（stemID）的其他 itemID；每個 itemID 一列（優先沒停用、版本最新的）
same_stem_items <- function(bank, row, waves_of) {
  ib <- bank$itemBank
  empty <- data.frame(itemID = character(), itemVer = integer(), label = character(),
                      deprecated = integer(), stringsAsFactors = FALSE)
  empty$waves <- list()
  stem <- trimws(as.character(row$stemID %||% ""))
  if (length(stem) == 0L || is.na(stem) || !nzchar(stem)) return(empty)
  st <- trimws(as.character(ib$stemID))
  hit <- ib[!is.na(st) & st == stem & ib$itemID != row$itemID, , drop = FALSE]
  if (nrow(hit) == 0L) return(empty)
  dep <- suppressWarnings(as.integer(hit$deprecated))
  dep[is.na(dep)] <- 0L
  hit <- hit[order(hit$itemID, dep == 1L, -hit$itemVer), , drop = FALSE]
  hit <- hit[!duplicated(hit$itemID), , drop = FALSE]
  # 依 bank 原來的順序
  hit <- hit[order(match(hit$itemID, ib$itemID)), , drop = FALSE]
  out <- data.frame(itemID = hit$itemID, itemVer = as.integer(hit$itemVer),
                    label = as.character(hit$label),
                    deprecated = suppressWarnings(as.integer(hit$deprecated)),
                    stringsAsFactors = FALSE)
  out$waves <- waves_of(hit)
  out
}

#' Versions × waves of one item
#'
#' @param bank An item bank from [load_bank()].
#' @param item_id An `itemID`.
#' @return A data.frame with one row per version (`itemVer`, `deprecated`),
#'   then one column per wave in which any version appears (age order),
#'   holding `"✓"` or `""`.
#' @examples
#' version_timeline(load_bank("parent"), "area1_1")
#' @export
version_timeline <- function(bank, item_id) {
  ib <- bank$itemBank
  v <- ib[ib$itemID == item_id, , drop = FALSE]
  v <- v[order(v$itemVer), , drop = FALSE]
  wave_cols <- get_wave_cols(bank$itemWave)
  m <- bank_matrix(bank$itemWave, v, wave_cols)
  used <- colSums(m) > 0L
  out <- data.frame(itemVer = as.integer(v$itemVer),
                    deprecated = suppressWarnings(as.integer(v$deprecated)),
                    stringsAsFactors = FALSE)
  ticks <- matrix(ifelse(m[, used, drop = FALSE] == 1L, "\u2713", ""), nrow = nrow(v),
                  dimnames = list(NULL, wave_cols[used]))
  cbind(out, as.data.frame(ticks, stringsAsFactors = FALSE, optional = TRUE))
}

# 題型代碼 → 中文（認得的才翻，括號留代碼）
ITEM_TYPE_LABELS <- c(
  # 原文：單選 | 複選的一個選項 | 填數字 | 填文字 | 行政資料
  MC = "\u55ae\u9078", DC = "\u8907\u9078\u7684\u4e00\u500b\u9078\u9805", NR = "\u586b\u6578\u5b57", CR = "\u586b\u6587\u5b57", AD = "\u884c\u653f\u8cc7\u6599"
)
item_type_label <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  zh <- unname(ITEM_TYPE_LABELS[toupper(x)])
  ok <- nzchar(x) & !is.na(zh)
  # 原文：（ | ）
  x[ok] <- paste0(zh[ok], "\uff08", x[ok], "\uff09")
  x
}

# 「不適用:8888; 拒答:7777; 遺漏值:9999」→「不適用 8888、拒答 7777、遺漏值 9999」
format_no_response <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L || is.na(x[1L]) || !nzchar(trimws(x[1L]))) return("")
  # 原文：[;；\r\n]+
  parts <- trimws(strsplit(x[1L], "[;\uff1b\r\n]+")[[1L]])
  parts <- parts[nzchar(parts)]
  # 原文：^([^:：]+)[:：]\\s*
  parts <- sub("^([^:\uff1a]+)[:\uff1a]\\s*", "\\1 ", parts)
  # 原文：、
  paste(parts, collapse = "\u3001")
}

# 備註拆成一行一段：以「;」分段，但「」裡的「;」不拆
split_note <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L || is.na(x[1L]) || !nzchar(trimws(x[1L]))) return(character())
  # 原文：[;；]|\r?\n
  pieces <- strsplit(x[1L], "[;\uff1b]|\r?\n")[[1L]]
  out <- character(); cur <- NULL
  count <- function(s, ch) lengths(regmatches(s, gregexpr(ch, s, fixed = TRUE)))
  for (p in pieces) {
    cur <- if (is.null(cur)) p else paste0(cur, ";", p)
    # 原文：「 | 」
    if (count(cur, "\u300c") <= count(cur, "\u300d")) {
      out <- c(out, trimws(cur)); cur <- NULL
    }
  }
  if (!is.null(cur)) out <- c(out, trimws(cur))
  out[nzchar(out)]
}

#' How two wordings differ
#'
#' A character-level difference between two strings (for example the `label`
#' of two versions of an item), based on [utils::adist()]; no extra packages.
#'
#' @param a The old text.
#' @param b The new text.
#' @return A data.frame of segments in reading order with columns `text` and
#'   `type`: `"same"`, `"removed"` (only in `a`), or `"added"` (only in `b`).
#'   Pasting the `same` and `removed` segments gives `a`; `same` and `added`
#'   gives `b`.
#' @examples
#' label_diff("孩子每天睡幾小時", "孩子平日每天睡幾個小時")
#' @export
label_diff <- function(a, b) {
  a <- if (length(a) == 0L || is.na(a[1L])) "" else as.character(a[1L])
  b <- if (length(b) == 0L || is.na(b[1L])) "" else as.character(b[1L])
  empty <- data.frame(text = character(), type = character(), stringsAsFactors = FALSE)
  if (identical(a, b)) {
    return(if (nzchar(a)) data.frame(text = a, type = "same", stringsAsFactors = FALSE) else empty)
  }
  ca <- strsplit(a, "")[[1L]]
  cb <- strsplit(b, "")[[1L]]
  if (length(ca) * length(cb) > 4e6) {
    # 太長：整段換掉
    return(rbind(
      if (length(ca)) data.frame(text = a, type = "removed", stringsAsFactors = FALSE),
      if (length(cb)) data.frame(text = b, type = "added", stringsAsFactors = FALSE),
      empty
    ))
  }
  trafo <- attr(utils::adist(a, b, counts = TRUE), "trafos")[1L]
  ops <- strsplit(trafo, "")[[1L]]
  txt <- character(); typ <- character()
  i <- 1L; j <- 1L
  for (op in ops) {
    if (op == "M") {
      txt <- c(txt, ca[i]); typ <- c(typ, "same"); i <- i + 1L; j <- j + 1L
    } else if (op == "D") {
      txt <- c(txt, ca[i]); typ <- c(typ, "removed"); i <- i + 1L
    } else if (op == "I") {
      txt <- c(txt, cb[j]); typ <- c(typ, "added"); j <- j + 1L
    } else if (op == "S") {
      txt <- c(txt, ca[i], cb[j]); typ <- c(typ, "removed", "added")
      i <- i + 1L; j <- j + 1L
    }
  }
  # 連續一段沒變之間的刪改：先列刪掉的、再列加上的，讀起來比較順
  run <- cumsum(c(TRUE, (typ[-1L] == "same") != (typ[-length(typ)] == "same")))
  out_txt <- character(); out_typ <- character()
  for (r in unique(run)) {
    k <- run == r
    if (typ[k][1L] == "same") {
      out_txt <- c(out_txt, paste(txt[k], collapse = "")); out_typ <- c(out_typ, "same")
    } else {
      for (tp in c("removed", "added")) {
        s <- paste(txt[k][typ[k] == tp], collapse = "")
        if (nzchar(s)) { out_txt <- c(out_txt, s); out_typ <- c(out_typ, tp) }
      }
    }
  }
  data.frame(text = out_txt, type = out_typ, stringsAsFactors = FALSE)
}

#' Item × wave table
#'
#' @param bank An item bank from [load_bank()].
#' @param items A data.frame with `itemID` and `itemVer` (for example from
#'   [query_items()]).
#' @return A data.frame with `itemID`, `itemVer`, `block`, `label`, then one
#'   column per wave in age order holding `"✓"` when the item appears in that
#'   wave and `""` otherwise.
#' @examples
#' bank <- load_bank("parent")
#' crosswalk(bank, query_items(bank, keyword = "睡眠"))
#' @export
crosswalk <- function(bank, items) {
  ib <- bank$itemBank
  idx <- match(item_key(items$itemID, items$itemVer), item_key(ib$itemID, ib$itemVer))
  get <- function(col) {
    x <- as.character(ib[[col]])[idx]
    x[is.na(x)] <- ""
    x
  }
  out <- data.frame(itemID = as.character(items$itemID),
                    itemVer = as.integer(items$itemVer),
                    block = get("block"), label = get("label"),
                    stringsAsFactors = FALSE)
  wave_cols <- get_wave_cols(bank$itemWave)
  m <- bank_matrix(bank$itemWave, items, wave_cols)
  # 原文：✓
  ticks <- ifelse(m == 1L, "\u2713", "")
  ticks <- matrix(ticks, nrow = nrow(m), ncol = ncol(m), dimnames = list(NULL, wave_cols))
  cbind(out, as.data.frame(ticks, stringsAsFactors = FALSE, optional = TRUE))
}

#' Number of items per block (or dimension) and wave
#'
#' @param bank An item bank from [load_bank()].
#' @param items A data.frame with `itemID` and `itemVer` (for example from
#'   [query_items()]).
#' @param by `"block"`: one row per block (items without a block are counted
#'   under `"（未分區塊）"`); `"dim"`: one row per dimension that has at least
#'   one of `items` (named by `dimName`; an item in several dimensions counts
#'   in each).
#' @return An integer matrix, one column per wave in age order. Rows are
#'   sorted by the first wave with any item (rows with no wave last), ties in
#'   order of first appearance.
#' @examples
#' bank <- load_bank("parent")
#' wave_counts(bank, query_items(bank, keyword = "睡眠"))
#' wave_counts(bank, query_items(bank, keyword = "睡眠"), by = "dim")
#' @export
wave_counts <- function(bank, items, by = c("block", "dim")) {
  by <- match.arg(by)
  wave_cols <- get_wave_cols(bank$itemWave)
  empty <- matrix(0L, nrow = 0L, ncol = length(wave_cols), dimnames = list(NULL, wave_cols))
  if (nrow(items) == 0L) return(empty)
  m <- bank_matrix(bank$itemWave, items, wave_cols)
  if (by == "block") {
    ib <- bank$itemBank
    idx <- match(item_key(items$itemID, items$itemVer), item_key(ib$itemID, ib$itemVer))
    block <- as.character(ib$block)[idx]
    # 原文：（未分區塊）
    block[is.na(block) | !nzchar(trimws(block))] <- "\uff08\u672a\u5206\u5340\u584a\uff09"
    out <- rowsum(m, factor(block, levels = unique(block)), reorder = TRUE)
  } else {
    dim_cols <- get_matrix_cols(bank$itemDim)
    d <- bank_matrix(bank$itemDim, items, dim_cols)
    used <- colSums(d) > 0L
    if (!any(used)) return(empty)
    out <- crossprod(d[, used, drop = FALSE], m)
    nm <- dim_names(bank, dim_cols[used])
    dup <- nm %in% nm[duplicated(nm)]
    # 原文：（ | ）
    nm[dup] <- paste0(nm[dup], "\uff08", dim_cols[used][dup], "\uff09")
    rownames(out) <- nm
  }
  storage.mode(out) <- "integer"
  first <- apply(out > 0L, 1L, function(r) if (any(r)) which(r)[1L] else NA_integer_)
  out[order(first, seq_len(nrow(out)), na.last = TRUE), , drop = FALSE]
}

# 字串 → R 程式碼裡的字串（中文照原樣）
r_str <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  x <- gsub("\n", "\\\\n", x)
  paste0("\"", x, "\"")
}
r_vec <- function(x) {
  if (length(x) == 1L) r_str(x) else paste0("c(", paste(r_str(x), collapse = ", "), ")")
}

#' R code that reproduces a query
#'
#' Writes the call to [query_items()] for the given conditions (only the
#' arguments that differ from the defaults), followed by [crosswalk()] and
#' saving both to Excel. Used by the app's "用 R 怎麼做" panel.
#'
#' @param domain The item bank: `"parent"`, `"child_adolescent"`, or
#'   `"childcare_teacher"`.
#' @param keyword,fields,keyword_match,waves,wave_match,dims,dim_match,blocks,item_types,respondents,include_deprecated
#'   As in [query_items()].
#' @param file When the bank was opened from an Excel file, its file name: the
#'   code then uses [read_bank_xlsx()] instead of [load_bank()].
#' @return One string of R code (lines separated by `"\n"`).
#' @examples
#' cat(query_code("parent", keyword = "睡眠", waves = c("M036小", "M048小")))
#' @export
query_code <- function(domain = "parent", keyword = "", fields = "text",
                       keyword_match = "all", waves = character(), wave_match = "any",
                       dims = character(), dim_match = "any",
                       blocks = NULL, item_types = NULL, respondents = NULL,
                       include_deprecated = FALSE, file = NULL) {
  args <- character()
  add <- function(name, value) args <<- c(args, paste0(name, " = ", value))
  kw <- paste(keyword_terms(keyword), collapse = " ")
  if (nzchar(kw)) add("keyword", r_str(kw))
  if (identical(fields, "all")) add("fields", r_str("all"))
  if (identical(keyword_match, "any") && length(keyword_terms(keyword)) > 1L) {
    add("keyword_match", r_str("any"))
  }
  if (length(waves)) {
    add("waves", r_vec(waves))
    if (identical(wave_match, "all")) add("wave_match", r_str("all"))
  }
  if (length(dims)) {
    add("dims", r_vec(dims))
    if (identical(dim_match, "all")) add("dim_match", r_str("all"))
  }
  if (length(blocks)) add("blocks", r_vec(blocks))
  if (length(item_types)) add("item_types", r_vec(item_types))
  if (length(respondents)) add("respondents", r_vec(respondents))
  if (isTRUE(include_deprecated)) add("include_deprecated", "TRUE")

  load_line <- if (is.null(file)) {
    paste0("bank <- bank_to_v02(read_kit(), ", r_str(domain), ")")
  } else {
    paste0("bank <- read_bank_xlsx(", r_str(file), ")")
  }
  call <- if (length(args)) {
    paste0("items <- query_items(\n  bank,\n  ",
           paste(args, collapse = ",\n  "), "\n)")
  } else {
    "items <- query_items(bank)"
  }
  paste(
    "library(kitItemBank)",
    load_line,
    call,
    "nrow(items)",
    "",
    # 原文：# 跨波次對照表（題目 × 波次）
    "# \u8de8\u6ce2\u6b21\u5c0d\u7167\u8868\uff08\u984c\u76ee \u00d7 \u6ce2\u6b21\uff09",
    "xw <- crosswalk(bank, items)",
    # 原文：# 各區塊在各波次的題數
    "# \u5404\u5340\u584a\u5728\u5404\u6ce2\u6b21\u7684\u984c\u6578",
    "wave_counts(bank, items)",
    # 原文：# 各構面在各波次的題數
    "# \u5404\u69cb\u9762\u5728\u5404\u6ce2\u6b21\u7684\u984c\u6578",
    "wave_counts(bank, items, by = \"dim\")",
    "",
    # 原文：# 存成 Excel
    "# \u5b58\u6210 Excel",
    # 原文：# （說明、題目（含構面欄）、跨波次對照三張工作表）
    "# \uff08\u8aaa\u660e\u3001\u984c\u76ee\uff08\u542b\u69cb\u9762\u6b04\uff09\u3001\u8de8\u6ce2\u6b21\u5c0d\u7167\u4e09\u5f35\u5de5\u4f5c\u8868\uff09",
    paste0("writexl::write_xlsx(query_workbook(bank, items, domain = ", r_str(domain), "),"),
    # 原文：題項查詢.xlsx
    paste0("                    ", r_str("\u984c\u9805\u67e5\u8a62.xlsx"), ")"),
    sep = "\n"
  )
}

# 查不到時：為什麼、可以怎麼放寬（每個建議附上放寬後的題數）
# args 是 query_items() 的參數（list）；回傳
#   list(reason = 一句話, tries = 字串向量,
#        dims = 名稱含任一個關鍵字的構面（data.frame dimID, dimName，最多 5 個）)
zero_result_hint <- function(bank, args) {
  run <- function(a) nrow(do.call(query_items, c(list(bank), a)))
  # 原文：、
  wave_lab <- paste(col_display_labels(args$waves), collapse = "\u3001")
  kw <- paste(keyword_terms(args$keyword), collapse = " ")
  base_dep <- list(include_deprecated = isTRUE(args$include_deprecated))

  reason <- if (nzchar(kw) &&
                run(c(list(keyword = kw, fields = args$fields %||% "text",
                           keyword_match = args$keyword_match %||% "all"), base_dep)) == 0L) {
    # 原文：題庫裡沒有含「 | 」的題目
    paste0("\u984c\u5eab\u88e1\u6c92\u6709\u542b\u300c", kw, "\u300d\u7684\u984c\u76ee")
  } else if (length(args$waves) &&
             run(c(list(waves = args$waves, wave_match = args$wave_match %||% "any"), base_dep)) == 0L) {
    if (identical(args$wave_match, "all") && length(args$waves) > 1L) {
      # 原文：沒有在  |  每個波次都出現的題目
      paste0("\u6c92\u6709\u5728 ", wave_lab, " \u6bcf\u500b\u6ce2\u6b21\u90fd\u51fa\u73fe\u7684\u984c\u76ee")
    } else {
      # 原文： 沒有題目
      paste0(wave_lab, " \u6c92\u6709\u984c\u76ee")
    }
  } else if (length(args$waves) && nzchar(kw)) {
    # 原文： 沒有含「 | 」的題目
    paste0(wave_lab, " \u6c92\u6709\u542b\u300c", kw, "\u300d\u7684\u984c\u76ee")
  } else {
    # 原文：這幾個條件同時成立的題目是 0 題
    "\u9019\u5e7e\u500b\u689d\u4ef6\u540c\u6642\u6210\u7acb\u7684\u984c\u76ee\u662f 0 \u984c"
  }

  n_terms <- length(keyword_terms(args$keyword))
  tries <- list(
    if (n_terms > 1L && !identical(args$keyword_match, "any"))
      # 原文：關鍵字改成「任一個詞」
      list("\u95dc\u9375\u5b57\u6539\u6210\u300c\u4efb\u4e00\u500b\u8a5e\u300d", utils::modifyList(args, list(keyword_match = "any"))),
    if (identical(args$fields, "text") && nzchar(kw))
      # 原文：把「找」改成「全部欄位」
      list("\u628a\u300c\u627e\u300d\u6539\u6210\u300c\u5168\u90e8\u6b04\u4f4d\u300d", utils::modifyList(args, list(fields = "all"))),
    if (identical(args$wave_match, "all") && length(args$waves) > 1L)
      # 原文：改成「任一波次」
      list("\u6539\u6210\u300c\u4efb\u4e00\u6ce2\u6b21\u300d", utils::modifyList(args, list(wave_match = "any"))),
    if (identical(args$dim_match, "all") && length(args$dims) > 1L)
      # 原文：改成「任一構面」
      list("\u6539\u6210\u300c\u4efb\u4e00\u69cb\u9762\u300d", utils::modifyList(args, list(dim_match = "any"))),
    if (length(args$dims))
      # 原文：拿掉構面條件
      list("\u62ff\u6389\u69cb\u9762\u689d\u4ef6", utils::modifyList(args, list(dims = character()))),
    if (length(args$waves))
      # 原文：拿掉波次條件
      list("\u62ff\u6389\u6ce2\u6b21\u689d\u4ef6", utils::modifyList(args, list(waves = character()))),
    if (length(args$blocks) || length(args$item_types) || length(args$respondents))
      # 原文：拿掉進階設定裡的區塊、題型、填答者
      list("\u62ff\u6389\u9032\u968e\u8a2d\u5b9a\u88e1\u7684\u5340\u584a\u3001\u984c\u578b\u3001\u586b\u7b54\u8005",
           utils::modifyList(args, list(blocks = NULL, item_types = NULL, respondents = NULL),
                             keep.null = TRUE)),
    if (!isTRUE(args$include_deprecated))
      # 原文：勾「包含已停用的題目」
      list("\u52fe\u300c\u5305\u542b\u5df2\u505c\u7528\u7684\u984c\u76ee\u300d", utils::modifyList(args, list(include_deprecated = TRUE))),
    if (nzchar(kw))
      # 原文：拿掉關鍵字或換個說法
      list("\u62ff\u6389\u95dc\u9375\u5b57\u6216\u63db\u500b\u8aaa\u6cd5", utils::modifyList(args, list(keyword = "")))
  )
  tries <- Filter(Negate(is.null), tries)
  n <- vapply(tries, function(t) run(t[[2L]]), integer(1))
  ok <- n > 0L
  list(
    dims = dims_matching(bank, args$keyword),
    reason = reason,
    # 原文：（會有 
    tries = paste0(vapply(tries[ok], `[[`, "", 1L), "\uff08\u6703\u6709 ",
                   # 原文： 題）
                   kitDesign::kit_num(n[ok]), " \u984c\uff09")
  )
}

# 名稱（dimName 或 dimID）含任一個關鍵字的構面，最多 n 個
dims_matching <- function(bank, keyword, n = 5L) {
  empty <- data.frame(dimID = character(), dimName = character(), stringsAsFactors = FALSE)
  terms <- keyword_terms(keyword)
  terms <- normalize_text(terms[!endsWith(terms, "*")])
  dim_ids <- get_matrix_cols(bank$itemDim)
  if (length(terms) == 0L || length(dim_ids) == 0L) return(empty)
  nm <- dim_names(bank, dim_ids)
  hay <- normalize_text(paste(nm, dim_ids, sep = "\u001f"))
  hit <- Reduce(`|`, lapply(terms, function(t) grepl(t, hay, fixed = TRUE)))
  if (!any(hit)) return(empty)
  out <- data.frame(dimID = dim_ids[hit], dimName = nm[hit], stringsAsFactors = FALSE)
  utils::head(out, n)
}

# 「本次：…」的各段，也是下載的「說明」裡的查詢條件
search_settings_parts <- function(a, bank) {
  some <- function(x, n = 4L) {
    x <- col_display_labels(x)
    # 原文：、 |  等  |  個
    if (length(x) > n) paste0(paste(x[seq_len(n)], collapse = "\u3001"), " \u7b49 ", length(x), " \u500b")
    # 原文：、
    else paste(x, collapse = "\u3001")
  }
  terms <- keyword_terms(a$keyword)
  kw <- paste(terms, collapse = " ")
  c(
    # 原文：關鍵字「 | 」（
    if (nzchar(kw)) paste0("\u95dc\u9375\u5b57\u300c", kw, "\u300d\uff08",
                           # 原文：全部欄位 | 題目文字
                           if (identical(a$fields, "all")) "\u5168\u90e8\u6b04\u4f4d" else "\u984c\u76ee\u6587\u5b57",
                           if (length(terms) > 1L) {
                             # 原文：，任一個詞 | ，每個詞都要有
                             if (identical(a$keyword_match, "any")) "\uff0c\u4efb\u4e00\u500b\u8a5e" else "\uff0c\u6bcf\u500b\u8a5e\u90fd\u8981\u6709"
                           # 原文：）
                           }, "\uff09")
    # 原文：關鍵字 不限
    else "\u95dc\u9375\u5b57 \u4e0d\u9650",
    # 原文：波次 
    if (length(a$waves)) paste0("\u6ce2\u6b21 ", some(a$waves),
                                if (length(a$waves) > 1L) {
                                  # 原文：（每個都有） | （任一）
                                  if (identical(a$wave_match, "all")) "\uff08\u6bcf\u500b\u90fd\u6709\uff09" else "\uff08\u4efb\u4e00\uff09"
                                })
    # 原文：波次 不限
    else "\u6ce2\u6b21 \u4e0d\u9650",
    # 原文：構面 
    if (length(a$dims)) paste0("\u69cb\u9762 ", some(dim_names(bank, a$dims)),
                               if (length(a$dims) > 1L) {
                                 # 原文：（每個都有） | （任一）
                                 if (identical(a$dim_match, "all")) "\uff08\u6bcf\u500b\u90fd\u6709\uff09" else "\uff08\u4efb\u4e00\uff09"
                               })
    # 原文：構面 不限
    else "\u69cb\u9762 \u4e0d\u9650",
    # 原文：區塊 
    if (length(a$blocks)) paste0("\u5340\u584a ", some(a$blocks)),
    # 原文：題型 
    if (length(a$item_types)) paste0("\u984c\u578b ", some(a$item_types)),
    # 原文：填答者 
    if (length(a$respondents)) paste0("\u586b\u7b54\u8005 ", some(a$respondents)),
    # 原文：包含已停用的題目
    if (isTRUE(a$include_deprecated)) "\u5305\u542b\u5df2\u505c\u7528\u7684\u984c\u76ee"
  )
}

# 下載的欄名與中文說明
QUERY_COLUMN_NOTES <- c(
  # 原文：題目代碼
  itemID = "\u984c\u76ee\u4ee3\u78bc",
  # 原文：版本：同一題改過措辭、選項或計分就加一版
  itemVer = "\u7248\u672c\uff1a\u540c\u4e00\u984c\u6539\u904e\u63aa\u8fad\u3001\u9078\u9805\u6216\u8a08\u5206\u5c31\u52a0\u4e00\u7248",
  # 原文：已停用：1 = 停用，0 = 還在用
  deprecated = "\u5df2\u505c\u7528\uff1a1 = \u505c\u7528\uff0c0 = \u9084\u5728\u7528",
  # 原文：題幹：幾個小題共用的問句；題幹相同的題目是同一組問題
  stemID = "\u984c\u5e79\uff1a\u5e7e\u500b\u5c0f\u984c\u5171\u7528\u7684\u554f\u53e5\uff1b\u984c\u5e79\u76f8\u540c\u7684\u984c\u76ee\u662f\u540c\u4e00\u7d44\u554f\u984c",
  # 原文：區塊
  block = "\u5340\u584a",
  # 原文：題型：MC 單選、DC 複選的一個選項、NR 填數字、CR 填文字、AD 行政資料
  itemType = "\u984c\u578b\uff1aMC \u55ae\u9078\u3001DC \u8907\u9078\u7684\u4e00\u500b\u9078\u9805\u3001NR \u586b\u6578\u5b57\u3001CR \u586b\u6587\u5b57\u3001AD \u884c\u653f\u8cc7\u6599",
  # 原文：填答者
  respondent = "\u586b\u7b54\u8005",
  # 原文：反向計分：1 = 是
  reverseScored = "\u53cd\u5411\u8a08\u5206\uff1a1 = \u662f",
  # 原文：題目文字
  label = "\u984c\u76ee\u6587\u5b57",
  # 原文：選項與代碼
  value = "\u9078\u9805\u8207\u4ee3\u78bc",
  # 原文：不作答的代碼（拒答、不知道、遺漏值等）
  noResponse = "\u4e0d\u4f5c\u7b54\u7684\u4ee3\u78bc\uff08\u62d2\u7b54\u3001\u4e0d\u77e5\u9053\u3001\u907a\u6f0f\u503c\u7b49\uff09",
  # 原文：備註（適用版本、版本異動）
  note = "\u5099\u8a3b\uff08\u9069\u7528\u7248\u672c\u3001\u7248\u672c\u7570\u52d5\uff09",
  # 原文：這一題出現在幾個波次
  n_waves = "\u9019\u4e00\u984c\u51fa\u73fe\u5728\u5e7e\u500b\u6ce2\u6b21",
  # 原文：這一題歸到的構面（多個用「、」分開）
  "\u9019\u4e00\u984c\u6b78\u5230\u7684\u69cb\u9762\uff08\u591a\u500b\u7528\u300c\u3001\u300d\u5206\u958b\uff09"
)
# 原文：構面
names(QUERY_COLUMN_NOTES)[length(QUERY_COLUMN_NOTES)] <- "\u69cb\u9762"

# items 每一題的構面名稱（「、」分開）
item_dim_names <- function(bank, items) {
  dim_ids <- get_matrix_cols(bank$itemDim)
  if (nrow(items) == 0L) return(character())
  if (length(dim_ids) == 0L) return(rep("", nrow(items)))
  d <- bank_matrix(bank$itemDim, items, dim_ids)
  nm <- dim_names(bank, dim_ids)
  # 原文：、
  apply(d, 1L, function(r) paste(nm[r == 1L], collapse = "\u3001"))
}

#' The query result as Excel sheets
#'
#' The sheets the app's "下載查詢結果" button saves: `說明` (query conditions,
#' bank version, column meanings, suggested citation), `題目` (the items, with
#' a `構面` column), and `跨波次對照` ([crosswalk()]).
#'
#' @param bank An item bank from [load_bank()].
#' @param items The result of [query_items()].
#' @param args The arguments given to [query_items()] (a list), to describe the
#'   conditions in words; `NULL` leaves that line out.
#' @param domain The item bank (`"parent"`, …), for its Chinese name.
#' @param version The bank's version date; by default the built-in bank's.
#' @return A named list of data.frames, for [writexl::write_xlsx()].
#' @examples
#' bank <- load_bank("parent")
#' items <- query_items(bank, keyword = "睡眠")
#' sheets <- query_workbook(bank, items, list(keyword = "睡眠"), domain = "parent")
#' names(sheets)
#' @export
query_workbook <- function(bank, items, args = NULL, domain = NULL,
                           version = kit_version()) {
  cols <- intersect(c(names(bank$itemBank), "n_waves"), names(items))
  it <- items[cols]
  # 原文：構面
  it[["\u69cb\u9762"]] <- item_dim_names(bank, items)
  # 原文：（版本日期不明）
  ver <- if (length(version) && !is.na(version[1L]) && nzchar(version[1L])) version[1L] else "\uff08\u7248\u672c\u65e5\u671f\u4e0d\u660e\uff09"
  dom <- if (is.null(domain)) "" else (bank_domain_label(domain) %||% domain)
  if (is.na(dom)) dom <- domain
  info <- stats::setNames(c(
    # 原文：題庫
    if (nzchar(dom)) paste0(dom, "\u984c\u5eab") else "",
    ver,
    format(Sys.Date()),
    # 原文：；
    if (!is.null(args)) paste(search_settings_parts(args, bank), collapse = "\uff1b") else "",
    # 原文： 題
    paste0(kitDesign::kit_num(nrow(items)), " \u984c"),
    # 原文：臺灣幼兒發展調查資料庫（KIT）題庫，
    paste0("\u81fa\u7063\u5e7c\u5152\u767c\u5c55\u8abf\u67e5\u8cc7\u6599\u5eab\uff08KIT\uff09\u984c\u5eab\uff0c", ver)
  # 原文：題庫 | 題庫版本日期 | 下載日期 | 查詢條件 | 題數 | 建議引用
  ), c("\u984c\u5eab", "\u984c\u5eab\u7248\u672c\u65e5\u671f", "\u4e0b\u8f09\u65e5\u671f", "\u67e5\u8a62\u689d\u4ef6", "\u984c\u6578", "\u5efa\u8b70\u5f15\u7528"))
  info <- info[nzchar(info)]
  notes <- QUERY_COLUMN_NOTES[intersect(names(QUERY_COLUMN_NOTES), names(it))]
  readme <- stats::setNames(data.frame(
    # 原文：工作表「題目」的欄位
    c(names(info), "", "\u5de5\u4f5c\u8868\u300c\u984c\u76ee\u300d\u7684\u6b04\u4f4d", paste0("  ", names(notes)),
      # 原文：工作表「跨波次對照」
      "", "\u5de5\u4f5c\u8868\u300c\u8de8\u6ce2\u6b21\u5c0d\u7167\u300d"),
    c(unname(info), "", "", unname(notes), "",
      # 原文：一列一題、一欄一個波次；✓ 表示這一題在那個波次有問
      "\u4e00\u5217\u4e00\u984c\u3001\u4e00\u6b04\u4e00\u500b\u6ce2\u6b21\uff1b\u2713 \u8868\u793a\u9019\u4e00\u984c\u5728\u90a3\u500b\u6ce2\u6b21\u6709\u554f"),
    stringsAsFactors = FALSE
  # 原文：項目 | 說明
  ), c("\u9805\u76ee", "\u8aaa\u660e"))
  # 原文：說明 | 題目 | 跨波次對照
  stats::setNames(list(readme, it, crosswalk(bank, items)), c("\u8aaa\u660e", "\u984c\u76ee", "\u8de8\u6ce2\u6b21\u5c0d\u7167"))
}
