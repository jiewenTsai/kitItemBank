# R/compare.R — 0.3.3：比較兩個資料檔，只列不一樣的地方
#
# 只用 kit（17 張表），沒有 Shiny：
#   compare_datasets()   兩個資料檔的差異：只在 A、只在 B、換題號、版本不同、位置不同、
#                        選項不同，以及遺漏代碼方案不同（一列，整個檔）
#   compare_workbook()   下載的 Excel（第一張是說明）
# 頁面在 R/page_compare.R。
#
# 怎麼對：有題號（item_uid）就用題號對，沒有（系統變項）就用變項名稱（不分大小寫）對。
# 換題號（X → Xn）：題庫的 item_relations 有 renamed 關係的兩題，一邊一個時配成一對。

# 原文：只在 A | 只在 B | 換題號 | 版本不同 | 位置不同 | 選項不同 | 遺漏代碼不同
COMPARE_TYPES <- c(only_a = "\u53ea\u5728 A", only_b = "\u53ea\u5728 B", renamed = "\u63db\u984c\u865f",
                   # 原文：版本不同 | 位置不同 | 選項不同
                   version = "\u7248\u672c\u4e0d\u540c", position = "\u4f4d\u7f6e\u4e0d\u540c", values = "\u9078\u9805\u4e0d\u540c",
                   # 原文：遺漏代碼不同
                   missing_scheme = "\u907a\u6f0f\u4ee3\u78bc\u4e0d\u540c")

#' 一個資料檔的名稱：「M03 世代 36 月齡（2019.03–2020.08） 家長（data1）」
#' @noRd
compare_dataset_label <- function(kit, datasetID) {
  ds <- kit$datasets[match(datasetID, kit$datasets$datasetID), , drop = FALSE]
  out <- paste(admin_label(kit, ds$adminID), cb_dataset_label(ds))
  out[is.na(ds$datasetID)] <- datasetID[is.na(ds$datasetID)]
  out
}

#' Compare two data files: list only what differs
#'
#' Variables are matched by `item_uid` (system variables without an item by variable
#' name, ignoring case). Items linked by a `renamed` relation (X -> Xn) are paired.
#' One row per difference:
#' * `only_a`, `only_b` — the variable is in one data file only;
#' * `renamed` — the item was renamed between the two files;
#' * `version` — same item, different `itemVer`;
#' * `position` — same item, different questionnaire position (both recorded);
#' * `values` — same item, different value labels (codes or wording);
#' * `missing_scheme` — one row when the two files use different missing-code schemes.
#'
#' @param kit A kit (17 tables), e.g. from [read_kit()].
#' @param a,b Two `datasetID`s, e.g. `"M03M036-P"` and `"M03M048-P"`.
#' @return A data frame: `type`, `type_label` (Chinese), `varname_a`, `varname_b`,
#'   `item_uid_a`, `item_uid_b`, `itemVer_a`, `itemVer_b`, `position_a`, `position_b`,
#'   `order_a`, `order_b`, `label` (item wording, A's when both), `values_a`, `values_b`.
#'   Zero rows means the two files ask the same items the same way.
#' @examples
#' kit <- read_kit()
#' d <- compare_datasets(kit, "M03M036-P", "M03M048-P")
#' table(d$type_label)
#' @export
compare_datasets <- function(kit, a, b) {
  ds <- kit$datasets
  for (x in c(a, b)) {
    if (!x %in% ds$datasetID) stop("no data file '", x, "' in the kit", call. = FALSE)
  }
  da <- ds[ds$datasetID == a, , drop = FALSE]
  db <- ds[ds$datasetID == b, , drop = FALSE]
  va <- cb_dataset_vars(kit, da)
  vb <- cb_dataset_vars(kit, db)
  keyf <- function(v) ifelse(nzchar(v$item_uid), v$item_uid, paste0("var:", tolower(v$varname)))
  va$key <- keyf(va)
  vb$key <- keyf(vb)
  va <- va[!duplicated(va$key), , drop = FALSE]
  vb <- vb[!duplicated(vb$key), , drop = FALSE]

  # 換題號：A 只有舊題、B 只有新題（或反過來）
  ren <- kit$item_relations[kit$item_relations$kind == "renamed", c("from_uid", "to_uid"),
                            drop = FALSE]
  ren <- rbind(ren, stats::setNames(ren[, c("to_uid", "from_uid")], c("from_uid", "to_uid")))
  ren <- ren[ren$from_uid %in% setdiff(va$key, vb$key) & ren$to_uid %in% setdiff(vb$key, va$key), ,
             drop = FALSE]
  ren <- ren[!duplicated(ren$from_uid) & !duplicated(ren$to_uid), , drop = FALSE]
  vb$key[match(ren$to_uid, vb$key)] <- ren$from_uid

  num <- function(x) ifelse(is.na(x), "", as.character(x))
  row <- function(type, ia = NA, ib = NA) {
    ga <- function(col) if (is.na(ia)) "" else as.character(va[[col]][ia])
    gb <- function(col) if (is.na(ib)) "" else as.character(vb[[col]][ib])
    data.frame(type = type, type_label = unname(COMPARE_TYPES[type]),
               varname_a = ga("varname"), varname_b = gb("varname"),
               item_uid_a = ga("item_uid"), item_uid_b = gb("item_uid"),
               itemVer_a = ga("itemVer"), itemVer_b = gb("itemVer"),
               position_a = ga("position"), position_b = gb("position"),
               order_a = num(if (is.na(ia)) NA else va$order[ia]),
               order_b = num(if (is.na(ib)) NA else vb$order[ib]),
               label = cb_one_line(if (!is.na(ia) && nzchar(va$label[ia])) va$label[ia] else gb("label")),
               values_a = ga("values"), values_b = gb("values"),
               stringsAsFactors = FALSE)
  }
  out <- list()
  add <- function(x) out[[length(out) + 1L]] <<- x

  sa <- cb_blank(da$missingScheme)
  sb <- cb_blank(db$missingScheme)
  if (!identical(sa, sb)) {
    r <- row("missing_scheme")
    # 原文：：
    r$values_a <- paste0(sa, if (nzchar(sa)) "\uff1a", cb_scheme_text(kit, sa))
    # 原文：：
    r$values_b <- paste0(sb, if (nzchar(sb)) "\uff1a", cb_scheme_text(kit, sb))
    add(r)
  }
  for (i in which(!va$key %in% vb$key)) add(row("only_a", i))
  for (j in which(!vb$key %in% va$key)) add(row("only_b", NA, j))
  both <- which(va$key %in% vb$key)
  for (i in both) {
    j <- match(va$key[i], vb$key)
    if (!identical(va$item_uid[i], vb$item_uid[j])) add(row("renamed", i, j))
    if (nzchar(va$item_uid[i]) && identical(va$item_uid[i], vb$item_uid[j]) &&
        !identical(va$itemVer[i], vb$itemVer[j])) add(row("version", i, j))
    pa <- cb_blank(va$position[i]); pb <- cb_blank(vb$position[j])
    if (nzchar(pa) && nzchar(pb) && pa != pb) add(row("position", i, j))
    if (!identical(va$values[i], vb$values[j])) add(row("values", i, j))
  }
  res <- do.call(rbind, out)
  if (is.null(res)) res <- row("only_a")[0, , drop = FALSE]
  # 整個檔的差異（遺漏代碼）放最前面，其他照種類、再照檔案順序
  ord <- ifelse(res$type == "missing_scheme", 0L, match(res$type, names(COMPARE_TYPES)))
  o_a <- suppressWarnings(as.integer(res$order_a))
  o_b <- suppressWarnings(as.integer(res$order_b))
  res <- res[order(ord, ifelse(is.na(o_a), o_b, o_a)), , drop = FALSE]
  rownames(res) <- NULL
  res
}

#' 一列差異寫成白話的 A／B 兩格
#' @noRd
compare_cells <- function(d) {
  # 原文：第  |  版
  ver <- function(v) ifelse(nzchar(v), paste0("\u7b2c ", v, " \u7248"), "")
  item_word <- function(var, uid, v) {
    # 原文：（ | ）
    x <- ifelse(nzchar(uid), paste0(var, "\uff08", ver(v), "\uff09"), var)
    # 原文：（）$
    sub("\uff08\uff09$", "", x)
  }
  a <- switch_vec(d$type,
                  only_a = item_word(d$varname_a, d$item_uid_a, d$itemVer_a),
                  # 原文：沒有
                  only_b = rep("\u6c92\u6709", nrow(d)),
                  renamed = item_word(d$varname_a, d$item_uid_a, d$itemVer_a),
                  version = ver(d$itemVer_a),
                  position = d$position_a,
                  values = d$values_a,
                  missing_scheme = d$values_a)
  b <- switch_vec(d$type,
                  # 原文：沒有
                  only_a = rep("\u6c92\u6709", nrow(d)),
                  only_b = item_word(d$varname_b, d$item_uid_b, d$itemVer_b),
                  renamed = item_word(d$varname_b, d$item_uid_b, d$itemVer_b),
                  version = ver(d$itemVer_b),
                  position = d$position_b,
                  values = d$values_b,
                  missing_scheme = d$values_b)
  list(a = a, b = b)
}

#' 依 type 從幾個等長向量裡挑
#' @noRd
switch_vec <- function(type, ...) {
  alts <- list(...)
  out <- rep("", length(type))
  for (nm in names(alts)) {
    hit <- type == nm
    out[hit] <- alts[[nm]][hit]
  }
  out
}

#' 差異的變項欄：一樣就寫一個，換題號寫「X → Xn」
#' @noRd
compare_var_cell <- function(d) {
  ifelse(!nzchar(d$varname_a), d$varname_b,
         ifelse(!nzchar(d$varname_b) | tolower(d$varname_a) == tolower(d$varname_b), d$varname_a,
                # 原文： → 
                paste0(d$varname_a, " \u2192 ", d$varname_b)))
}

#' Excel of the differences between two data files (first sheet explains)
#'
#' @param kit A kit list.
#' @param a,b Two `datasetID`s.
#' @param diff Output of [compare_datasets()] (computed when `NULL`).
#' @return A named list of data frames for `writexl::write_xlsx()`: 說明, 差異.
#' @export
compare_workbook <- function(kit, a, b, diff = NULL) {
  d <- diff %||% compare_datasets(kit, a, b)
  n <- table(factor(d$type, levels = names(COMPARE_TYPES)))
  # 原文： 處 | ；
  cnt <- paste0(COMPARE_TYPES, " ", as.integer(n), " \u8655", collapse = "\uff1b")
  info <- data.frame(
    # 原文：這個檔案 | 產生日期 | 差異 | 怎麼對
    k = c("\u9019\u500b\u6a94\u6848", "A", "B", "\u7522\u751f\u65e5\u671f", "\u5dee\u7570", "\u600e\u9ebc\u5c0d",
          # 原文：只在 A／只在 B | 換題號 | 版本不同 | 位置不同 | 選項不同 | 遺漏代碼不同
          "\u53ea\u5728 A\uff0f\u53ea\u5728 B", "\u63db\u984c\u865f", "\u7248\u672c\u4e0d\u540c", "\u4f4d\u7f6e\u4e0d\u540c", "\u9078\u9805\u4e0d\u540c", "\u907a\u6f0f\u4ee3\u78bc\u4e0d\u540c"),
    # 原文：KIT 題庫兩個資料檔的比較：只列不一樣的地方，一樣的不列。
    x = c("KIT \u984c\u5eab\u5169\u500b\u8cc7\u6599\u6a94\u7684\u6bd4\u8f03\uff1a\u53ea\u5217\u4e0d\u4e00\u6a23\u7684\u5730\u65b9\uff0c\u4e00\u6a23\u7684\u4e0d\u5217\u3002",
          # 原文：：
          paste0(a, "\uff1a", compare_dataset_label(kit, a)),
          # 原文：：
          paste0(b, "\uff1a", compare_dataset_label(kit, b)),
          format(kit_today(), "%Y-%m-%d"),
          # 原文：兩個資料檔問的題目和問法都一樣。
          if (nrow(d)) cnt else "\u5169\u500b\u8cc7\u6599\u6a94\u554f\u7684\u984c\u76ee\u548c\u554f\u6cd5\u90fd\u4e00\u6a23\u3002",
          # 原文：有題號的用題號對；系統變項用變項名稱對（不分大小寫）。
          "\u6709\u984c\u865f\u7684\u7528\u984c\u865f\u5c0d\uff1b\u7cfb\u7d71\u8b8a\u9805\u7528\u8b8a\u9805\u540d\u7a31\u5c0d\uff08\u4e0d\u5206\u5927\u5c0f\u5beb\uff09\u3002",
          # 原文：這個變項只有一個資料檔有。
          "\u9019\u500b\u8b8a\u9805\u53ea\u6709\u4e00\u500b\u8cc7\u6599\u6a94\u6709\u3002",
          # 原文：題庫記了換題號（例如 X 改成 Xn），兩邊各有一個。
          "\u984c\u5eab\u8a18\u4e86\u63db\u984c\u865f\uff08\u4f8b\u5982 X \u6539\u6210 Xn\uff09\uff0c\u5169\u908a\u5404\u6709\u4e00\u500b\u3002",
          # 原文：同一題，但用的版本不一樣（題目文字或選項改過）；版本的差別在「題項查詢」看。
          "\u540c\u4e00\u984c\uff0c\u4f46\u7528\u7684\u7248\u672c\u4e0d\u4e00\u6a23\uff08\u984c\u76ee\u6587\u5b57\u6216\u9078\u9805\u6539\u904e\uff09\uff1b\u7248\u672c\u7684\u5dee\u5225\u5728\u300c\u984c\u9805\u67e5\u8a62\u300d\u770b\u3002",
          # 原文：同一題，在紙本問卷上的位置不一樣（例如「四.33」是第四部分第 33 題）。
          "\u540c\u4e00\u984c\uff0c\u5728\u7d19\u672c\u554f\u5377\u4e0a\u7684\u4f4d\u7f6e\u4e0d\u4e00\u6a23\uff08\u4f8b\u5982\u300c\u56db.33\u300d\u662f\u7b2c\u56db\u90e8\u5206\u7b2c 33 \u984c\uff09\u3002",
          # 原文：同一題，選項的代碼或文字不一樣。
          "\u540c\u4e00\u984c\uff0c\u9078\u9805\u7684\u4ee3\u78bc\u6216\u6587\u5b57\u4e0d\u4e00\u6a23\u3002",
          # 原文：兩個資料檔沒有效答案時填的代碼（拒答、不知道、跳答）不是同一套。
          "\u5169\u500b\u8cc7\u6599\u6a94\u6c92\u6709\u6548\u7b54\u6848\u6642\u586b\u7684\u4ee3\u78bc\uff08\u62d2\u7b54\u3001\u4e0d\u77e5\u9053\u3001\u8df3\u7b54\uff09\u4e0d\u662f\u540c\u4e00\u5957\u3002"),
    stringsAsFactors = FALSE)
  # 原文：項目 | 內容
  names(info) <- c("\u9805\u76ee", "\u5167\u5bb9")
  cells <- compare_cells(d)
  tab <- data.frame(d$type_label, compare_var_cell(d), d$label, cells$a, cells$b,
                    d$item_uid_a, d$item_uid_b, d$order_a, d$order_b,
                    stringsAsFactors = FALSE)
  # 原文：差異 | 變項 | 題目 | 題號（A） | 題號（B）
  names(tab) <- c("\u5dee\u7570", "\u8b8a\u9805", "\u984c\u76ee", "A", "B", "\u984c\u865f\uff08A\uff09", "\u984c\u865f\uff08B\uff09",
                  # 原文：順序（A） | 順序（B）
                  "\u9806\u5e8f\uff08A\uff09", "\u9806\u5e8f\uff08B\uff09")
  # 原文：說明 | 差異
  stats::setNames(list(info, tab), c("\u8aaa\u660e", "\u5dee\u7570"))
}
