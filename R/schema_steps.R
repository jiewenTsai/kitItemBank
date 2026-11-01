# R/schema_steps.R — 0.3.11 題目參數：一次分析估出來的步驟參數（beta）
#
# 這個檔照字母順序排在 schema.R、schema_evidence.R 後面讀，所以 KIT_SCHEMA 已經有
# analyses、item_versions；這裡只往後加 item_steps 一張表。read_kit() 找不到
# item_steps.csv 時回傳空表。
#
# 題庫只存 kitsUI 估出來的步驟參數（beta，一題 k 個選項就有 k − 1 步）和標準誤；
# 瑟斯頓門檻（Thurstonian thresholds：能力多高時答到第 k 步以上的機率是 50%）
# 不存，要看的時候用 kits 套件的 kits::algo_thresholds() 當場算（item_step_thresholds()）。
#   import_item_steps()     kitsUI 的參數表 → 題庫列 + 修改包（照一般的審核流程加入）
#   item_step_thresholds()  題庫裡的步驟參數 → 加上門檻
#   item_steps_ui()         題項查詢的細節裡用的小表格（沒有參數時是 NULL）

KIT_SCHEMA[["item_steps"]] <- list(
  cols = list(analysisID = kcol(req = TRUE), item_uid = kcol(req = TRUE),
              itemVer = kcol("int", req = TRUE), step = kcol("int", req = TRUE),
              beta = kcol("num", req = TRUE), se = kcol("num"), note = kcol()),
  key = c("analysisID", "item_uid", "itemVer", "step"),
  fk = list(list(cols = "analysisID", table = "analyses"),
            list(cols = "item_uid", table = "items"),
            list(cols = c("item_uid", "itemVer"), table = "item_versions")))

# 匯入 ----

#' 參數表的欄名：kits／kitsUI 的寫法 → 題庫的寫法
#' @noRd
steps_rename <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  nm <- names(df)
  alias <- c(item_score = "step", beta_se = "se", item_id = "varname", ver = "itemVer")
  for (a in names(alias)) {
    if (a %in% nm && !alias[[a]] %in% nm) nm[nm == a] <- alias[[a]]
  }
  names(df) <- nm
  df
}

#' 數字 → 存進題庫的文字（最多 6 位小數）
#' @noRd
steps_num_chr <- function(x) {
  x <- round(as.numeric(x), 6L)
  out <- vapply(x, function(v) {
    if (is.na(v)) return("")
    format(v, scientific = FALSE, trim = TRUE, drop0trailing = TRUE, digits = 15L)
  }, character(1))
  unname(out)
}

#' 分析：已經在題庫裡的 analysisID，或一列新的分析紀錄
#' @noRd
steps_analysis <- function(kit, analysis) {
  an <- kit$analyses %||% empty_kit_table("analyses")
  if (is.character(analysis) && length(analysis) == 1L) {
    hit <- an[an$analysisID == analysis, , drop = FALSE]
    if (!nrow(hit)) {
      # 原文：題庫裡沒有分析「 | 」；新的分析請給一列分析紀錄
      stop("\u984c\u5eab\u88e1\u6c92\u6709\u5206\u6790\u300c", analysis, "\u300d\uff1b\u65b0\u7684\u5206\u6790\u8acb\u7d66\u4e00\u5217\u5206\u6790\u7d00\u9304",
           # 原文：（analysisID、datasetID、method……）。
           "\uff08analysisID\u3001datasetID\u3001method\u2026\u2026\uff09\u3002", call. = FALSE)
    }
    return(list(row = hit[1, , drop = FALSE], new = FALSE))
  }
  row <- conform_kit_table(as.data.frame(analysis, stringsAsFactors = FALSE), "analyses")
  # 原文：analysis 要是一個 analysisID 或一列分析紀錄。
  if (nrow(row) != 1L) stop("analysis \u8981\u662f\u4e00\u500b analysisID \u6216\u4e00\u5217\u5206\u6790\u7d00\u9304\u3002", call. = FALSE)
  # 原文：分析紀錄要有 analysisID。
  if (!nzchar(row$analysisID)) stop("\u5206\u6790\u7d00\u9304\u8981\u6709 analysisID\u3002", call. = FALSE)
  if (row$analysisID %in% an$analysisID) {
    old <- an[an$analysisID == row$analysisID, , drop = FALSE]
    return(list(row = old[1, , drop = FALSE], new = FALSE))
  }
  if (!nzchar(row$datasetID) || !row$datasetID %in% kit$datasets$datasetID) {
    # 原文：新的分析紀錄要有題庫裡的資料檔（datasetID）：「 | 」。
    stop("\u65b0\u7684\u5206\u6790\u7d00\u9304\u8981\u6709\u984c\u5eab\u88e1\u7684\u8cc7\u6599\u6a94\uff08datasetID\uff09\uff1a\u300c", row$datasetID, "\u300d\u3002", call. = FALSE)
  }
  list(row = row, new = TRUE)
}

#' Import step parameters (kitsUI) into the bank
#'
#' Validates a table of step parameters estimated by kitsUI (one row per item step)
#' and turns it into rows of the `item_steps` table plus a change package (patch rows,
#' see [apply_patch()]) that goes through the usual review. Only the step parameters
#' (beta) and their standard errors are stored; Thurstonian thresholds are computed
#' when shown, see [item_step_thresholds()].
#'
#' @param df A data frame, one row per step. Items are given by `item_uid` (and
#'   `itemVer`), or by `varname` (or kits' `item_id`), looked up in the analysis's data
#'   file. Steps by `step` (or kits' `item_score`): 1, 2, ..., k for each item.
#'   Parameters by `beta`, optional `se` (or `beta_se`) and `note`.
#' @param kit A kit, from [read_kit()].
#' @param analysis An `analysisID` already in `kit$analyses`, or a one-row data frame
#'   (or list) for a new `analyses` row (needs `analysisID` and a `datasetID` of the kit).
#' @param replace `TRUE` to replace the steps the analysis already has in the bank.
#' @return A list: `analyses` (the new analysis row, or zero rows), `item_steps` (the
#'   rows to add), `patch` (a patch data frame: delete, add and set rows). Stops with a
#'   message listing the problems when the table does not fit the bank.
#' @export
import_item_steps <- function(df, kit, analysis, replace = FALSE) {
  an <- steps_analysis(kit, analysis)
  aid <- an$row$analysisID
  d <- an$row$datasetID
  df <- steps_rename(df)
  # 原文：參數表是空的。
  if (!nrow(df)) stop("\u53c3\u6578\u8868\u662f\u7a7a\u7684\u3002", call. = FALSE)
  for (cc in c("step", "beta")) {
    # 原文：參數表少了欄位「 | 」。
    if (!cc %in% names(df)) stop("\u53c3\u6578\u8868\u5c11\u4e86\u6b04\u4f4d\u300c", cc, "\u300d\u3002", call. = FALSE)
  }
  chr <- function(cc) if (cc %in% names(df)) trimws(cb_blank(df[[cc]])) else rep("", nrow(df))
  uid <- chr("item_uid")
  ver <- chr("itemVer")
  vn <- chr("varname")
  probs <- character(0)
  dv <- kit$dataset_vars[kit$dataset_vars$datasetID == d, , drop = FALSE]
  # 只有變項名稱：用分析的資料檔對到題號和版本
  need <- !nzchar(uid)
  if (any(need)) {
    # 原文：參數表要有 item_uid 或 varname。
    if (!any(nzchar(vn[need]))) stop("\u53c3\u6578\u8868\u8981\u6709 item_uid \u6216 varname\u3002", call. = FALSE)
    hit <- match(vn[need], dv$varname)
    uid[need] <- cb_blank(dv$item_uid[hit])
    ver[need & !nzchar(ver)] <- cb_blank(dv$itemVer[hit])[!nzchar(ver[need])]
    bad <- need & !nzchar(uid)
    # 原文：資料檔  |  裡找不到變項：
    if (any(bad)) probs <- c(probs, paste0("\u8cc7\u6599\u6a94 ", d, " \u88e1\u627e\u4e0d\u5230\u8b8a\u9805\uff1a",
                                           # 原文：、
                                           paste(unique(vn[bad]), collapse = "\u3001")))
  }
  # 有題號沒版本：用資料檔裡的版本；資料檔沒有這題時，只有一版就用那一版
  nov <- nzchar(uid) & !nzchar(ver)
  if (any(nov)) {
    from_dv <- cb_blank(dv$itemVer[match(uid[nov], dv$item_uid)])
    iv <- kit$item_versions
    only <- vapply(uid[nov], function(u) {
      v <- unique(iv$itemVer[iv$item_uid == u])
      if (length(v) == 1L) v else ""
    }, character(1), USE.NAMES = FALSE)
    ver[nov] <- ifelse(nzchar(from_dv), from_dv, only)
    bad <- nov & !nzchar(ver)
    # 原文：不知道是第幾版（請填 itemVer）：
    if (any(bad)) probs <- c(probs, paste0("\u4e0d\u77e5\u9053\u662f\u7b2c\u5e7e\u7248\uff08\u8acb\u586b itemVer\uff09\uff1a",
                                           # 原文：、
                                           paste(unique(uid[bad]), collapse = "\u3001")))
  }
  ok <- nzchar(uid) & nzchar(ver)
  known <- paste(uid, ver) %in% paste(kit$item_versions$item_uid, kit$item_versions$itemVer)
  bad <- ok & !known
  # 原文：題庫裡沒有這些題目版本：
  if (any(bad)) probs <- c(probs, paste0("\u984c\u5eab\u88e1\u6c92\u6709\u9019\u4e9b\u984c\u76ee\u7248\u672c\uff1a",
                                         # 原文： 第  |  版
                                         paste(unique(paste0(uid[bad], " \u7b2c ", ver[bad], " \u7248")),
                                               # 原文：、
                                               collapse = "\u3001")))
  step <- suppressWarnings(as.numeric(chr("step")))
  bad <- is.na(step) | step < 1 | step != round(step)
  # 原文：步驟（step）要是 1、2、3……：第 
  if (any(bad)) probs <- c(probs, paste0("\u6b65\u9a5f\uff08step\uff09\u8981\u662f 1\u30012\u30013\u2026\u2026\uff1a\u7b2c ",
                                         # 原文：、 |  列
                                         paste(which(bad), collapse = "\u3001"), " \u5217"))
  beta <- suppressWarnings(as.numeric(chr("beta")))
  bad <- is.na(beta) | !is.finite(beta)
  # 原文：beta 不是數字：第  | 、 |  列
  if (any(bad)) probs <- c(probs, paste0("beta \u4e0d\u662f\u6578\u5b57\uff1a\u7b2c ", paste(which(bad), collapse = "\u3001"), " \u5217"))
  se_txt <- chr("se")
  se <- suppressWarnings(as.numeric(se_txt))
  bad <- nzchar(se_txt) & (is.na(se) | !is.finite(se) | se < 0)
  # 原文：se 要是 0 以上的數字或空白：第 
  if (any(bad)) probs <- c(probs, paste0("se \u8981\u662f 0 \u4ee5\u4e0a\u7684\u6578\u5b57\u6216\u7a7a\u767d\uff1a\u7b2c ",
                                         # 原文：、 |  列
                                         paste(which(bad), collapse = "\u3001"), " \u5217"))
  # 每一題的步驟要剛好是 1..k
  item_key <- paste(uid, ver, sep = "|")
  for (k in unique(item_key[ok & !is.na(step)])) {
    s <- sort(step[item_key == k])
    if (anyDuplicated(s) || !identical(as.numeric(s), as.numeric(seq_along(s)))) {
      # 原文： 第  |  版的步驟要是 1 到 k、
      probs <- c(probs, paste0(sub("|", " \u7b2c ", k, fixed = TRUE), " \u7248\u7684\u6b65\u9a5f\u8981\u662f 1 \u5230 k\u3001",
                               # 原文：不重複不跳號（現在是  | ）
                               "\u4e0d\u91cd\u8907\u4e0d\u8df3\u865f\uff08\u73fe\u5728\u662f ", paste(s, collapse = ", "), "\uff09"))
    }
  }
  if (length(probs)) {
    # 原文：參數表有問題，沒有匯入：\n
    stop("\u53c3\u6578\u8868\u6709\u554f\u984c\uff0c\u6c92\u6709\u532f\u5165\uff1a\n", paste0("- ", utils::head(probs, 10L), collapse = "\n"),
         # 原文：\n……還有  |  個問題
         if (length(probs) > 10L) paste0("\n\u2026\u2026\u9084\u6709 ", length(probs) - 10L, " \u500b\u554f\u984c"),
         call. = FALSE)
  }
  rows <- conform_kit_table(data.frame(
    analysisID = aid, item_uid = uid, itemVer = as.character(as.integer(ver)),
    step = as.character(as.integer(step)), beta = steps_num_chr(beta),
    se = steps_num_chr(se), note = chr("note"), stringsAsFactors = FALSE
  ), "item_steps")
  rows <- rows[order(rows$item_uid, as.integer(rows$itemVer), as.integer(rows$step)), , drop = FALSE]
  rownames(rows) <- NULL

  old <- kit$item_steps %||% empty_kit_table("item_steps")
  old <- old[old$analysisID == aid, , drop = FALSE]
  if (nrow(old) && !isTRUE(replace)) {
    # 原文：分析「 | 」在題庫裡已經有  |  列步驟參數；
    stop("\u5206\u6790\u300c", aid, "\u300d\u5728\u984c\u5eab\u88e1\u5df2\u7d93\u6709 ", nrow(old), " \u5217\u6b65\u9a5f\u53c3\u6578\uff1b",
         # 原文：要換掉請用 replace = TRUE。
         "\u8981\u63db\u6389\u8acb\u7528 replace = TRUE\u3002", call. = FALSE)
  }
  patch <- rbind(
    empty_patch(),
    if (nrow(old)) patch_rows("delete", "item_steps",
                              kit_key_str(old, KIT_SCHEMA$item_steps$key)),
    if (an$new) rv_add_row("analyses", as.list(an$row)),
    do.call(rbind, lapply(seq_len(nrow(rows)), function(i) rv_add_row("item_steps",
                                                                       as.list(rows[i, ]))))
  )
  rownames(patch) <- NULL
  list(analyses = if (an$new) an$row else empty_kit_table("analyses"),
       item_steps = rows, patch = patch)
}

# 門檻 ----

#' kits 套件裝了沒（測試時可以換掉）
#' @noRd
steps_have_kits <- function() requireNamespace("kits", quietly = TRUE)

#' Step parameters of an item, with Thurstonian thresholds
#'
#' Reads the stored step parameters (`item_steps`) and computes the Thurstonian
#' thresholds at call time with `kits::algo_thresholds()`: for step k, the ability at
#' which the probability of answering in category k or higher is `rp`. The bank only
#' keeps standard errors, not the covariances between the steps of an item, so `thr_se`
#' treats the steps as uncorrelated (an approximation). When the kits package is not
#' installed the betas come back with `thr` and `thr_se` empty and a note.
#'
#' @param kit A kit, from [read_kit()].
#' @param item_uid One or more item_uids, for example `"P.cogc01"`.
#' @param itemVer Optional item versions.
#' @param analysisID Optional analyses (default: all analyses with steps of the item).
#' @param rp Response probability (0.5 = RP50).
#' @return A data frame, one row per step: `analysisID`, `item_uid`, `itemVer`, `step`,
#'   `beta`, `se`, `thr`, `thr_se` (numbers), `note`. The attribute `"note"` says how
#'   the thresholds were computed, or why they were not.
#' @export
item_step_thresholds <- function(kit, item_uid, itemVer = NULL, analysisID = NULL, rp = 0.5) {
  st <- kit$item_steps %||% empty_kit_table("item_steps")
  st <- st[st$item_uid %in% item_uid, , drop = FALSE]
  if (!is.null(itemVer)) st <- st[st$itemVer %in% as.character(itemVer), , drop = FALSE]
  if (!is.null(analysisID)) st <- st[st$analysisID %in% analysisID, , drop = FALSE]
  st <- st[order(st$analysisID, st$item_uid, as.integer(st$itemVer), as.integer(st$step)), ,
           drop = FALSE]
  se <- suppressWarnings(as.numeric(st$se))
  out <- data.frame(analysisID = st$analysisID, item_uid = st$item_uid,
                    itemVer = as.integer(st$itemVer), step = as.integer(st$step),
                    beta = as.numeric(st$beta), se = se,
                    thr = rep(NA_real_, nrow(st)), thr_se = rep(NA_real_, nrow(st)),
                    note = st$note, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  if (!nrow(out)) {
    # 原文：題庫裡沒有這一題的步驟參數。
    attr(out, "note") <- "\u984c\u5eab\u88e1\u6c92\u6709\u9019\u4e00\u984c\u7684\u6b65\u9a5f\u53c3\u6578\u3002"
    return(out)
  }
  if (!steps_have_kits()) {
    # 原文：沒有安裝 kits 套件，只列步驟參數（beta）；
    attr(out, "note") <- paste0("\u6c92\u6709\u5b89\u88dd kits \u5957\u4ef6\uff0c\u53ea\u5217\u6b65\u9a5f\u53c3\u6578\uff08beta\uff09\uff1b",
                                # 原文：門檻要裝了 kits 才算得出來。
                                "\u9580\u6abb\u8981\u88dd\u4e86 kits \u624d\u7b97\u5f97\u51fa\u4f86\u3002")
    return(out)
  }
  # kits::algo_thresholds() 吃 algo_cml() 的結果：coef（item_id、item_score、beta、se）
  # 和 vcov。題庫沒有步驟之間的共變，vcov 用對角線 se²；se 空白時是 NA（門檻標準誤也是 NA）
  id <- paste(out$analysisID, out$item_uid, out$itemVer, sep = "|")
  cml <- list(
    coef = data.frame(item_id = id, item_score = out$step, beta = out$beta, se = out$se,
                      stringsAsFactors = FALSE),
    vcov = diag(out$se^2, nrow = nrow(out))
  )
  thr <- kits::algo_thresholds(cml, rp = rp)
  hit <- match(paste(id, out$step), paste(thr$item_id, thr$item_score))
  out$thr <- as.numeric(thr$thr[hit])
  out$thr_se <- as.numeric(thr$thr_se[hit])
  attr(out, "note") <- paste0(
    # 原文：門檻是瑟斯頓門檻（RP | ：答到這一步以上的機率是 
    "\u9580\u6abb\u662f\u745f\u65af\u9813\u9580\u6abb\uff08RP", round(rp * 100), "\uff1a\u7b54\u5230\u9019\u4e00\u6b65\u4ee5\u4e0a\u7684\u6a5f\u7387\u662f ", rp,
    # 原文： 的能力），用 kits 
    " \u7684\u80fd\u529b\uff09\uff0c\u7528 kits ", as.character(utils::packageVersion("kits")),
    # 原文： 的 algo_thresholds() 當場算；題庫沒有步驟之間的共變，門檻標準誤當作步驟互不相關，只供參考。
    " \u7684 algo_thresholds() \u7576\u5834\u7b97\uff1b\u984c\u5eab\u6c92\u6709\u6b65\u9a5f\u4e4b\u9593\u7684\u5171\u8b8a\uff0c\u9580\u6abb\u6a19\u6e96\u8aa4\u7576\u4f5c\u6b65\u9a5f\u4e92\u4e0d\u76f8\u95dc\uff0c\u53ea\u4f9b\u53c3\u8003\u3002")
  out
}

#' Step parameters of an item, for the item detail panel
#'
#' A small table (one block per analysis) of step parameters and Thurstonian thresholds
#' for the item detail of the 題項查詢 page.
#'
#' @param kit A kit, from [read_kit()].
#' @param item_uid One item_uid.
#' @param itemVer Optional item version.
#' @return A `shiny.tag`, or `NULL` when the bank has no step parameters for the item.
#' @export
item_steps_ui <- function(kit, item_uid, itemVer = NULL) {
  st <- kit$item_steps %||% empty_kit_table("item_steps")
  if (!any(st$item_uid %in% item_uid)) return(NULL)
  tb <- item_step_thresholds(kit, item_uid, itemVer)
  if (!nrow(tb)) return(NULL)
  an <- kit$analyses %||% empty_kit_table("analyses")
  num <- function(x) stats_num_str(x, 2L)
  blocks <- lapply(unique(tb$analysisID), function(a) {
    x <- tb[tb$analysisID == a, , drop = FALSE]
    r <- an[an$analysisID == a, , drop = FALSE]
    # 原文：分析 
    head <- paste0("\u5206\u6790 ", a,
                   # 原文：（資料檔 
                   if (nrow(r) && nzchar(r$datasetID[1])) paste0("\uff08\u8cc7\u6599\u6a94 ", r$datasetID[1],
                     # 原文：，
                     if (nzchar(r$date[1])) paste0("\uff0c", r$date[1]),
                     # 原文：，正式結果 | ）
                     if (identical(r$official[1], "1")) "\uff0c\u6b63\u5f0f\u7d50\u679c", "\uff09"))
    df <- data.frame(
      # 原文：第  |  版
      v = paste0("\u7b2c ", x$itemVer, " \u7248"), s = x$step, b = num(x$beta),
      bse = num(x$se), t = num(x$thr), tse = num(x$thr_se), n = cb_blank(x$note),
      stringsAsFactors = FALSE)
    # 原文：版本 | 步驟 | 步驟參數 β | β 標準誤 | 門檻 | 門檻標準誤 | 備註
    names(df) <- c("\u7248\u672c", "\u6b65\u9a5f", "\u6b65\u9a5f\u53c3\u6578 \u03b2", "\u03b2 \u6a19\u6e96\u8aa4", "\u9580\u6abb", "\u9580\u6abb\u6a19\u6e96\u8aa4", "\u5099\u8a3b")
    # 原文：備註 | 備註
    if (!any(nzchar(df[["\u5099\u8a3b"]]))) df[["\u5099\u8a3b"]] <- NULL
    shiny::tagList(shiny::p(shiny::tags$strong(head)),
                   kitDesign::kit_table_scroll(kitDesign::kit_static_table(df)))
  })
  shiny::div(
    class = "kit-item-steps",
    # 原文：題目參數（步驟難度）
    shiny::p(shiny::tags$strong("\u984c\u76ee\u53c3\u6578\uff08\u6b65\u9a5f\u96e3\u5ea6\uff09")),
    blocks,
    shiny::p(class = "kit-hint", attr(tb, "note"))
  )
}
