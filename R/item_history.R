# R/item_history.R — 一題的沿革單：各版本、出現在哪些施測、題目之間的關係、屬於哪些題組
#
# item_history() 整理資料；item_history_docx() 寫成 Word（樣式見 report_docx.R）。
# 「家族」沿著 item_relations 的換題號（renamed）、拆開、合併往前往後找，一起列出。

#' 一題沿著換題號、拆開、合併的關係，找出整個家族的題號（含自己）
#' @noRd
item_family <- function(kit, item_uid) {
  rel <- kit$item_relations[kit$item_relations$kind %in% c("renamed", "split", "merged"), ,
                            drop = FALSE]
  fam <- item_uid
  repeat {
    more <- unique(c(rel$to_uid[rel$from_uid %in% fam], rel$from_uid[rel$to_uid %in% fam]))
    more <- setdiff(more, fam)
    if (!length(more)) break
    fam <- c(fam, more)
  }
  fam
}

#' The history of one item
#'
#' @param kit A kit, from [read_kit()].
#' @param item_uid For example `"P.fatherinvo01"`.
#' @return A list: `item_uid`, `family` (item_uids linked by renaming, splitting or
#'   merging), `versions` (one row per version of every family member: item_uid, itemVer,
#'   label, options, admins), `relations` (between family members) and `groups` (form,
#'   group label, item_uid).
#' @export
item_history <- function(kit, item_uid) {
  fam <- item_family(kit, item_uid)
  iv <- kit$item_versions[kit$item_versions$item_uid %in% fam, , drop = FALSE]
  iv <- iv[order(match(iv$item_uid, fam), as.integer(iv$itemVer)), , drop = FALSE]
  vtxt <- kit_value_text(kit$value_codes)
  opts <- unname(vtxt[iv$valueSetID])
  opts[is.na(opts)] <- ""
  dv <- kit$dataset_vars[kit$dataset_vars$item_uid %in% fam, , drop = FALSE]
  dv$adminID <- kit$datasets$adminID[match(dv$datasetID, kit$datasets$datasetID)]
  adm_order <- kit$admins$adminID[order(as.integer(kit$admins$ageMonth), kit$admins$cohortID)]
  admins <- vapply(seq_len(nrow(iv)), function(i) {
    a <- unique(dv$adminID[dv$item_uid == iv$item_uid[i] & dv$itemVer == iv$itemVer[i]])
    # 原文：、
    paste(a[order(match(a, adm_order))], collapse = "\u3001")
  }, character(1))
  versions <- data.frame(item_uid = iv$item_uid, itemVer = iv$itemVer, label = iv$label,
                         options = gsub("\n", " ", opts, fixed = TRUE), admins = admins,
                         stringsAsFactors = FALSE)
  rel <- kit$item_relations[kit$item_relations$from_uid %in% fam |
                              kit$item_relations$to_uid %in% fam, , drop = FALSE]
  gm <- kit$group_members[kit$group_members$item_uid %in% fam, , drop = FALSE]
  groups <- data.frame(formID = gm$formID,
                       group = kit$groups$label[match(gm$groupID, kit$groups$groupID)],
                       item_uid = gm$item_uid, stringsAsFactors = FALSE)
  groups <- unique(groups[order(groups$formID, groups$group), , drop = FALSE])
  list(item_uid = item_uid, family = fam, versions = versions, relations = rel, groups = groups)
}

# 關係的白話
ITEM_REL_ZH <- c(
  # 原文：改版 | 換題號 | 拆開 | 合併 | 借用
  revised = "\u6539\u7248", renamed = "\u63db\u984c\u865f", split = "\u62c6\u958b",
  # 原文：合併 | 借用
  merged = "\u5408\u4f75", borrowed = "\u501f\u7528"
)

#' Write an item history to Word
#'
#' @param hist The result of [item_history()].
#' @param path Output `.docx` path.
#' @return `path`, invisibly.
#' @export
item_history_docx <- function(hist, path) {
  doc <- kit_docx_styles(officer::read_docx())
  add <- function(text, style = "Normal") {
    doc <<- officer::body_add_par(doc, as.character(text), style = style)
  }
  table <- function(df) doc <<- officer::body_add_table(doc, df, style = "table_template")
  # 原文：題目沿革單：
  add(paste0("\u984c\u76ee\u6c9b\u9769\u55ae\uff1a", hist$item_uid), KIT_DOCX_TITLE)
  # 原文：題庫版本： |   ·  產生時間：
  add(paste0("\u984c\u5eab\u7248\u672c\uff1a", kit_version() %||% "", "  \u00b7  \u7522\u751f\u6642\u9593\uff1a",
             format(Sys.time(), "%Y-%m-%d %H:%M")))
  if (length(hist$family) > 1L) {
    # 原文：同一個題目家族（換過題號）：
    add(paste0("\u540c\u4e00\u500b\u984c\u76ee\u5bb6\u65cf\uff08\u63db\u904e\u984c\u865f\uff09\uff1a",
               # 原文：、
               paste(hist$family, collapse = "\u3001")))
  }

  # 原文：各版本
  add("\u5404\u7248\u672c", KIT_DOCX_HEADING)
  v <- hist$versions
  # 原文：題號 | 版本 | 題目 | 選項 | 出現在哪些施測
  table(stats::setNames(v[c("item_uid", "itemVer", "label", "options", "admins")],
                        # 原文：題號 | 版本 | 題目 | 選項
                        c("\u984c\u865f", "\u7248\u672c", "\u984c\u76ee", "\u9078\u9805",
                          # 原文：出現在哪些施測
                          "\u51fa\u73fe\u5728\u54ea\u4e9b\u65bd\u6e2c")))

  r <- hist$relations
  if (nrow(r)) {
    # 原文：版本之間的關係
    add("\u7248\u672c\u4e4b\u9593\u7684\u95dc\u4fc2", KIT_DOCX_HEADING)
    kind <- unname(ITEM_REL_ZH[r$kind])
    kind[is.na(kind)] <- r$kind[is.na(kind)]
    # 原文：從 | 到 | 關係 | 從哪次施測起 | 理由 | 決定者
    table(data.frame("\u5f9e" = paste0(r$from_uid, " v", r$from_ver),
                     # 原文：到
                     "\u5230" = paste0(r$to_uid, " v", r$to_ver),
                     # 原文：關係 | 從哪次施測起
                     "\u95dc\u4fc2" = kind, "\u5f9e\u54ea\u6b21\u65bd\u6e2c\u8d77" = r$effective_admin,
                     # 原文：理由
                     "\u7406\u7531" = gsub("\n", " ", r$reason, fixed = TRUE),
                     # 原文：決定者
                     "\u6c7a\u5b9a\u8005" = r$decided_by,
                     check.names = FALSE, stringsAsFactors = FALSE))
  }

  g <- hist$groups
  # 原文：屬於哪些題組
  add("\u5c6c\u65bc\u54ea\u4e9b\u984c\u7d44", KIT_DOCX_HEADING)
  if (nrow(g)) {
    # 原文：題本 | 題組 | 題號
    table(stats::setNames(g, c("\u984c\u672c", "\u984c\u7d44", "\u984c\u865f")))
  } else {
    # 原文：（還沒有歸到任何題組）
    add("\uff08\u9084\u6c92\u6709\u6b78\u5230\u4efb\u4f55\u984c\u7d44\uff09")
  }
  print(doc, target = path)
  invisible(path)
}
