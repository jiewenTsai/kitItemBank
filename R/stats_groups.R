# R/stats_groups.R — 證據層：一波資料的題組統計（信度、單因素、反向題檢查）
#
# 題組 = 這個資料檔的題本（formID）在 group_members 裡 scheme 為 official 的題組；
# 資料檔裡至少有 2 題才算。只用 psych 套件：
#   psych::alpha(check.keys = TRUE) → α、題目總分相關（r.drop）、刪題後 α、psych 自動反向的題
#   psych::fa(nfactors = 1, cor = "poly") → 負荷量（polychoric 失敗就改用 Pearson，並註明）
#   ω_total 用負荷量自己算：(Σλ)² / ((Σλ)² + Σ(1 − λ²))
#   特徵值比 = 第 1 個 / 第 2 個特徵值；配適度（CFI、TLI、RMSEA、rms）只供參考，4 題以上才列
# 算之前先照題庫的反向標記反向計分：最大選項 + 最小選項 − 原值（選項從 value_codes 來）。
# 遺漏代碼先設成遺漏值；只用每一題都有效的人（完整個案）。

#' ω_total from one-factor standardised loadings
#'
#' \eqn{\omega_t = (\sum\lambda)^2 / ((\sum\lambda)^2 + \sum(1-\lambda^2))}.
#' @param loadings Numeric vector of standardised loadings.
#' @return A number.
#' @examples
#' stats_omega_total(c(.7, .7, .7, .7))
#' @export
stats_omega_total <- function(loadings) {
  s <- sum(loadings)
  s^2 / (s^2 + sum(1 - loadings^2))
}

#' 反向計分：max + min − x
#' @noRd
stats_reverse <- function(x, codes) {
  if (length(codes)) {
    lo <- min(codes)
    hi <- max(codes)
  } else {
    lo <- min(x, na.rm = TRUE)
    hi <- max(x, na.rm = TRUE)
  }
  hi + lo - x
}

#' 一個資料檔的題組和題組裡的題目（資料檔裡有的）
#' @return data.frame：groupID、item_uid、varname、column、itemVer、reverse
#' @noRd
stats_group_members <- function(data, kit, datasetID) {
  form <- kit$datasets$formID[kit$datasets$datasetID == datasetID]
  form <- if (length(form)) form[1] else ""
  gm <- kit$group_members
  gm <- gm[gm$formID == form & gm$scheme == "official", , drop = FALSE]
  gm <- gm[!duplicated(gm[c("groupID", "item_uid")]), , drop = FALSE]
  vars <- stats_match_vars(data, kit, datasetID)
  vars <- vars[nzchar(vars$item_uid) & !duplicated(vars$item_uid), , drop = FALSE]
  hit <- match(gm$item_uid, vars$item_uid)
  gm <- gm[!is.na(hit), , drop = FALSE]
  hit <- hit[!is.na(hit)]
  data.frame(groupID = gm$groupID, item_uid = gm$item_uid, varname = vars$varname[hit],
             column = vars$column[hit], itemVer = vars$itemVer[hit],
             reverse = ifelse(gm$reverse %in% "1", "1", "0"), stringsAsFactors = FALSE)
}

#' Group-level statistics of one data file
#'
#' For each official group of the dataset's form with at least two items in `data`:
#' items are reverse-scored by the bank's `reverse` flags (max + min − x, from the item's
#' value codes), missing codes set to missing and complete cases kept. Then, with the
#' psych package only: `psych::alpha(check.keys = TRUE)` gives raw alpha, corrected
#' item-total correlation (`r_drop`), alpha if item deleted and the items psych reversed
#' on its own (a hint that the bank's reverse flag may be wrong);
#' `psych::fa(nfactors = 1, cor = "poly")` (Pearson if polychoric fails, with a note)
#' gives loadings, ω_total (by hand from the loadings, see [stats_omega_total()]), the
#' ratio of the first to the second eigenvalue, and, for four or more items only, CFI,
#' TLI, RMSEA and rms (for reference only).
#'
#' @inheritParams item_stats
#' @return A data frame (all columns character) with the columns of the `group_stats`
#'   table except `analysisID`: one row per group (`varname` blank) followed by one row
#'   per item of that group.
#' @export
group_stats <- function(data, kit, datasetID) {
  if (!requireNamespace("psych", quietly = TRUE)) {
    # 原文：題組統計需要 psych 套件。
    stop("\u984c\u7d44\u7d71\u8a08\u9700\u8981 psych \u5957\u4ef6\u3002", call. = FALSE)
  }
  mem <- stats_group_members(data, kit, datasetID)
  mc <- stats_missing_codes(kit, datasetID)
  cols <- setdiff(names(KIT_SCHEMA$group_stats$cols), "analysisID")
  out <- list()
  for (g in unique(mem$groupID)) {
    m <- mem[mem$groupID == g, , drop = FALSE]
    if (nrow(m) < 2L) next
    out[[length(out) + 1L]] <- stats_one_group(g, m, data, kit, mc)
  }
  if (!length(out)) return(conform_kit_table(NULL, "group_stats")[, cols, drop = FALSE])
  res <- do.call(rbind, out)
  res <- conform_kit_table(res, "group_stats")[, cols, drop = FALSE]
  rownames(res) <- NULL
  res
}

#' 一個題組：題組一列 + 每題一列
#' @noRd
stats_one_group <- function(g, m, data, kit, mc) {
  notes <- character(0)
  item_note <- rep("", nrow(m))
  x <- list()
  for (i in seq_len(nrow(m))) {
    v <- stats_as_numeric(data[[m$column[i]]])
    if (is.null(v)) {
      # 原文：文字變項，不列入
      item_note[i] <- "\u6587\u5b57\u8b8a\u9805\uff0c\u4e0d\u5217\u5165"
      next
    }
    v[v %in% mc$code] <- NA
    if (m$reverse[i] == "1") {
      codes <- stats_value_codes(kit, m$item_uid[i], m$itemVer[i], mc$code)
      # 原文：題庫沒有選項，用資料的最大最小值反向
      if (!length(codes)) item_note[i] <- "\u984c\u5eab\u6c92\u6709\u9078\u9805\uff0c\u7528\u8cc7\u6599\u7684\u6700\u5927\u6700\u5c0f\u503c\u53cd\u5411"
      if (any(!is.na(v))) v <- stats_reverse(v, codes)
    }
    x[[m$varname[i]]] <- v
  }
  x <- as.data.frame(x, check.names = FALSE)
  if (ncol(x)) x <- x[stats::complete.cases(x), , drop = FALSE]
  # 沒有變異的題目算不了相關
  if (ncol(x)) {
    flat <- vapply(x, function(v) length(unique(v)) < 2L, logical(1))
    # 原文：沒有變異，不列入
    item_note[m$varname %in% names(x)[flat]] <- "\u6c92\u6709\u8b8a\u7570\uff0c\u4e0d\u5217\u5165"
    x <- x[, !flat, drop = FALSE]
  }
  n <- nrow(x)
  grp <- list(groupID = g, varname = "", item_uid = "", n_items = as.character(ncol(x)),
              n = stats_count_str(n))
  items <- data.frame(groupID = g, varname = m$varname, item_uid = m$item_uid,
                      n_items = "", n = "", bank_reverse = m$reverse, stringsAsFactors = FALSE)
  finish <- function(grp, items, notes) {
    # 原文：；
    grp$note <- paste(notes, collapse = "\uff1b")
    items$note <- item_note
    rbind(conform_kit_table(as.data.frame(grp, stringsAsFactors = FALSE), "group_stats"),
          conform_kit_table(items, "group_stats"))
  }
  # 原文：可用的題目少於 2 題，不計算
  if (ncol(x) < 2L) return(finish(grp, items, c(notes, "\u53ef\u7528\u7684\u984c\u76ee\u5c11\u65bc 2 \u984c\uff0c\u4e0d\u8a08\u7b97")))
  # 原文：完整作答人數少於 10，不計算
  if (n < STATS_MIN_N) return(finish(grp, items, c(notes, "\u5b8c\u6574\u4f5c\u7b54\u4eba\u6578\u5c11\u65bc 10\uff0c\u4e0d\u8a08\u7b97")))

  # α（psych 會自己把負相關的題反向，記下來和題庫的反向標記比）
  a <- tryCatch(suppressMessages(suppressWarnings(
    psych::alpha(x, check.keys = TRUE, warnings = FALSE))), error = function(e) NULL)
  if (is.null(a)) {
    # 原文：psych::alpha 算不出來
    notes <- c(notes, "psych::alpha \u7b97\u4e0d\u51fa\u4f86")
  } else {
    grp$alpha <- stats_num_str(a$total$raw_alpha)
    rn <- rownames(a$item.stats)
    base <- sub("-$", "", rn)
    rev_psych <- grepl("-$", rn)
    k <- match(m$varname, base)
    items$r_drop <- ifelse(is.na(k), "", stats_num_str(a$item.stats$r.drop[k]))
    items$alpha_if_deleted <- if (ncol(x) > 2L) {
      ifelse(is.na(k), "", stats_num_str(a$alpha.drop$raw_alpha[k]))
    } else ""
    items$psych_reversed <- ifelse(is.na(k), "", ifelse(rev_psych[k], "1", "0"))
    flag <- !is.na(k) & rev_psych[k]
    # 原文：；
    item_note[flag] <- paste0(item_note[flag], ifelse(nzchar(item_note[flag]), "\uff1b", ""),
                              # 原文：psych 自動反向：題庫的反向標記可能要再看
                              "psych \u81ea\u52d5\u53cd\u5411\uff1a\u984c\u5eab\u7684\u53cd\u5411\u6a19\u8a18\u53ef\u80fd\u8981\u518d\u770b")
    # 原文：psych 自動反向了  |  題
    if (any(flag)) notes <- c(notes, paste0("psych \u81ea\u52d5\u53cd\u5411\u4e86 ", sum(flag), " \u984c"))
  }

  # 單因素：polychoric，失敗改用 Pearson
  if (ncol(x) < 3L) {
    # 原文：只有 2 題，不做單因素分析
    notes <- c(notes, "\u53ea\u6709 2 \u984c\uff0c\u4e0d\u505a\u55ae\u56e0\u7d20\u5206\u6790")
  } else {
    cor_type <- "poly"
    f <- tryCatch(suppressMessages(suppressWarnings(
      psych::fa(x, nfactors = 1, cor = "poly"))), error = function(e) NULL)
    if (is.null(f) || anyNA(f$loadings[, 1])) {
      cor_type <- "pearson"
      # 原文：polychoric 相關算不出來，改用 Pearson 相關
      notes <- c(notes, "polychoric \u76f8\u95dc\u7b97\u4e0d\u51fa\u4f86\uff0c\u6539\u7528 Pearson \u76f8\u95dc")
      f <- tryCatch(suppressMessages(suppressWarnings(
        psych::fa(x, nfactors = 1, cor = "cor"))), error = function(e) NULL)
    }
    if (is.null(f)) {
      # 原文：psych::fa 算不出來
      notes <- c(notes, "psych::fa \u7b97\u4e0d\u51fa\u4f86")
    } else {
      grp$cor_type <- cor_type
      l <- as.numeric(f$loadings[, 1])
      names(l) <- rownames(f$loadings)
      if (sum(l) < 0) l <- -l
      grp$omega_total <- stats_num_str(stats_omega_total(l))
      ev <- f$e.values
      grp$eigen_ratio <- if (length(ev) >= 2L && ev[2] > 0) stats_num_str(ev[1] / ev[2]) else ""
      items$loading <- ifelse(m$varname %in% names(l), stats_num_str(l[m$varname]), "")
      if (ncol(x) >= 4L) {
        grp$cfi <- stats_num_str(f$CFI %||% NA)
        grp$tli <- stats_num_str(f$TLI %||% NA)
        grp$rmsea <- stats_num_str(f$RMSEA[1] %||% NA)
        grp$rms <- stats_num_str(f$rms %||% NA)
        # 原文：配適度只供參考
        notes <- c(notes, "\u914d\u9069\u5ea6\u53ea\u4f9b\u53c3\u8003")
      }
    }
  }
  finish(grp, items, notes)
}
