# R/consult.R — 請教單：把要老師判斷的事做成 Excel，填回來後變成修改包
#
# consult_rows()      哪些事要問老師（一列一個問題）
# consult_workbook()  做成 Excel：編號、系統用欄鎖住；「決定」是下拉選單；理由、誰決定自由填
# read_consult()      讀回填好的請教單（只讀有填的列）
# consult_patch()     變成修改包（和 R/patch.R 的改動列同一種格式），不直接改題庫：
#                     決定寫進 item_relations、block_status 的 decided_by／decided_on／reason，
#                     要刪的列刪掉，要接上的題組成員新增；寫不進題庫的回覆放在 notes。
# 做 Excel 要 openxlsx（下拉選單、鎖欄）；讀回只用 readxl。

# 每一種問題：類別名稱、哪張表、問題、下拉選單的選項和對應的處理
#   comparable_* = 改 item_relations$comparable 並記下決定；delete = 刪掉這一列；
#   join_groups = 新題接上舊題的題組；record = 只記下決定；
#   drop_members = 刪掉這個題組在這個題本的成員；keep／note／discuss = 不改題庫，列在 notes
CONSULT_TYPES <- list(
  relation = list(
    # 原文：題目前後版本
    label = "\u984c\u76ee\u524d\u5f8c\u7248\u672c", table = "item_relations",
    # 原文：這兩個版本可以當成同一題、跨波次比較嗎？
    question = "\u9019\u5169\u500b\u7248\u672c\u53ef\u4ee5\u7576\u6210\u540c\u4e00\u984c\u3001\u8de8\u6ce2\u6b21\u6bd4\u8f03\u55ce\uff1f",
    # 原文：可以比較 | 部分可以比較 | 不能比較 | 不是同一題，刪掉這個關係 | 再討論
    choices = c("\u53ef\u4ee5\u6bd4\u8f03", "\u90e8\u5206\u53ef\u4ee5\u6bd4\u8f03", "\u4e0d\u80fd\u6bd4\u8f03", "\u4e0d\u662f\u540c\u4e00\u984c\uff0c\u522a\u6389\u9019\u500b\u95dc\u4fc2", "\u518d\u8a0e\u8ad6"),
    actions = c("comparable_yes", "comparable_partial", "comparable_no", "delete", "discuss")),
  renamed = list(
    # 原文：換題號的新題
    label = "\u63db\u984c\u865f\u7684\u65b0\u984c", table = "item_relations",
    # 原文：換了題號的新題，要接上舊題的題組嗎？
    question = "\u63db\u4e86\u984c\u865f\u7684\u65b0\u984c\uff0c\u8981\u63a5\u4e0a\u820a\u984c\u7684\u984c\u7d44\u55ce\uff1f",
    # 原文：接上舊題的題組 | 不接，新題不屬於這些題組 | 再討論
    choices = c("\u63a5\u4e0a\u820a\u984c\u7684\u984c\u7d44", "\u4e0d\u63a5\uff0c\u65b0\u984c\u4e0d\u5c6c\u65bc\u9019\u4e9b\u984c\u7d44", "\u518d\u8a0e\u8ad6"),
    actions = c("join_groups", "record", "discuss")),
  block = list(
    # 原文：區塊狀態
    label = "\u5340\u584a\u72c0\u614b", table = "block_status",
    # 原文：這個資料檔的這一段，真的是這樣問的嗎？
    question = "\u9019\u500b\u8cc7\u6599\u6a94\u7684\u9019\u4e00\u6bb5\uff0c\u771f\u7684\u662f\u9019\u6a23\u554f\u7684\u55ce\uff1f",
    # 原文：對 | 不對，刪掉這筆 | 再討論
    choices = c("\u5c0d", "\u4e0d\u5c0d\uff0c\u522a\u6389\u9019\u7b46", "\u518d\u8a0e\u8ad6"),
    actions = c("record", "delete", "discuss")),
  single = list(
    # 原文：只有 1 題的題組
    label = "\u53ea\u6709 1 \u984c\u7684\u984c\u7d44", table = "group_members",
    # 原文：這個題組只有 1 題，要保留嗎？
    question = "\u9019\u500b\u984c\u7d44\u53ea\u6709 1 \u984c\uff0c\u8981\u4fdd\u7559\u55ce\uff1f",
    # 原文：保留 | 把這題移出題組 | 再討論
    choices = c("\u4fdd\u7559", "\u628a\u9019\u984c\u79fb\u51fa\u984c\u7d44", "\u518d\u8a0e\u8ad6"),
    actions = c("keep", "drop_members", "discuss")),
  deprecated = list(
    # 原文：題目都停用的題組
    label = "\u984c\u76ee\u90fd\u505c\u7528\u7684\u984c\u7d44", table = "group_members",
    # 原文：這個題組的題目都停用了，這個題本還要留這個題組嗎？
    question = "\u9019\u500b\u984c\u7d44\u7684\u984c\u76ee\u90fd\u505c\u7528\u4e86\uff0c\u9019\u500b\u984c\u672c\u9084\u8981\u7559\u9019\u500b\u984c\u7d44\u55ce\uff1f",
    # 原文：保留 | 從這個題本拿掉這個題組 | 再討論
    choices = c("\u4fdd\u7559", "\u5f9e\u9019\u500b\u984c\u672c\u62ff\u6389\u9019\u500b\u984c\u7d44", "\u518d\u8a0e\u8ad6"),
    actions = c("keep", "drop_members", "discuss")),
  nonleaf = list(
    # 原文：掛在中間層的題目
    label = "\u639b\u5728\u4e2d\u9593\u5c64\u7684\u984c\u76ee", table = "group_members",
    # 原文：題目直接掛在中間層題組，要改掛到下層題組嗎？
    question = "\u984c\u76ee\u76f4\u63a5\u639b\u5728\u4e2d\u9593\u5c64\u984c\u7d44\uff0c\u8981\u6539\u639b\u5230\u4e0b\u5c64\u984c\u7d44\u55ce\uff1f",
    # 原文：保留 | 要改掛（下層題組寫在理由） | 再討論
    choices = c("\u4fdd\u7559", "\u8981\u6539\u639b\uff08\u4e0b\u5c64\u984c\u7d44\u5beb\u5728\u7406\u7531\uff09", "\u518d\u8a0e\u8ad6"),
    actions = c("keep", "note", "discuss")),
  other = list(
    # 原文：其他
    label = "\u5176\u4ed6", table = "",
    # 原文：這裡要改嗎？
    question = "\u9019\u88e1\u8981\u6539\u55ce\uff1f",
    # 原文：要改（怎麼改寫在理由） | 不用改 | 再討論
    choices = c("\u8981\u6539\uff08\u600e\u9ebc\u6539\u5beb\u5728\u7406\u7531\uff09", "\u4e0d\u7528\u6539", "\u518d\u8a0e\u8ad6"),
    actions = c("note", "keep", "discuss"))
)

# audit_kit() 的檢查 → 請教單的問題種類（HOME_AUDIT_OWNER 裡 teacher 的檢查）
CONSULT_AUDIT_TYPE <- c(renamed_no_group = "renamed", group_single_item = "single",
                        group_only_deprecated = "deprecated", group_nonleaf_member = "nonleaf")

# 請教單的欄（Excel 的表頭）
# 原文：編號 | 類別 | 對象 | 內容
CONSULT_COLS <- c(id = "\u7de8\u865f", type = "\u985e\u5225", target = "\u5c0d\u8c61", context = "\u5167\u5bb9",
                  # 原文：問題 | 決定 | 理由
                  question = "\u554f\u984c", decision = "\u6c7a\u5b9a", reason = "\u7406\u7531",
                  # 原文：誰決定 | 系統用
                  decided_by = "\u8ab0\u6c7a\u5b9a", key = "\u7cfb\u7d71\u7528")
# 原文：請教單
CONSULT_SHEET <- "\u8acb\u6559\u55ae"

consult_empty_rows <- function() {
  data.frame(qtype = character(0), table = character(0), key = character(0),
             target = character(0), context = character(0), question = character(0),
             stringsAsFactors = FALSE)
}

consult_tb <- function(kit, name) audit_kit_tb(kit, name)

#' 題目版本的文字（一行）
#' @noRd
consult_iv_label <- function(kit, uid, ver) {
  iv <- consult_tb(kit, "item_versions")
  l <- iv$label[match(paste(uid, ver), paste(iv$item_uid, iv$itemVer))]
  l[is.na(l)] <- ""
  gsub("[\r\n]+", " ", l)
}

#' 題組名稱「標籤（代號）」
#' @noRd
consult_group_label <- function(kit, g) {
  grp <- consult_tb(kit, "groups")
  l <- grp$label[match(g, grp$groupID)]
  # 原文：（ | ）
  ifelse(is.na(l) | l == "" | l == g, g, paste0(l, "\uff08", g, "\uff09"))
}

#' Questions that need a teacher's judgement
#'
#' Three sources: the [audit_kit()] findings that `HOME_AUDIT_OWNER` gives to teachers (renamed
#' items without groups, groups with one item or only deprecated items, items on a non-leaf
#' group); item relations decided automatically whose comparability is in doubt (`comparable`
#' is not `"yes"`, the meaning changed, or the texts are only moderately similar); and block
#' statuses decided automatically.
#'
#' @param kit A kit list.
#' @param audit Optional result of [audit_kit()] (computed when `NULL`).
#' @param include Which sources: any of `"audit"`, `"relations"`, `"blocks"`.
#' @return A data frame, one row per question: `qtype` (see `CONSULT_TYPES`), `table`, `key`
#'   (primary key of the row the answer is about), `target`, `context` and `question`.
#' @export
consult_rows <- function(kit, audit = NULL, include = c("audit", "relations", "blocks")) {
  include <- match.arg(include, several.ok = TRUE)
  out <- list(consult_empty_rows())
  add <- function(qtype, key, target, context) {
    if (!length(key)) return(invisible())
    ty <- CONSULT_TYPES[[qtype]]
    out[[length(out) + 1L]] <<- data.frame(
      qtype = qtype, table = ty$table, key = key, target = target, context = context,
      question = ty$question, stringsAsFactors = FALSE)
  }
  rel <- consult_tb(kit, "item_relations")
  rel_target <- function(r) {
    # 原文： 第  |  版 →  |  第  |  版
    paste0(r$from_uid, " \u7b2c ", r$from_ver, " \u7248 \u2192 ", r$to_uid, " \u7b2c ", r$to_ver, " \u7248")
  }
  rel_texts <- function(r) {
    # 原文：舊：
    paste0("\u820a\uff1a", consult_iv_label(kit, r$from_uid, r$from_ver),
           # 原文：／新：
           "\uff0f\u65b0\uff1a", consult_iv_label(kit, r$to_uid, r$to_ver))
  }

  if ("audit" %in% include) {
    a <- audit %||% audit_kit(kit)
    own <- HOME_AUDIT_OWNER$owner[match(a$check, HOME_AUDIT_OWNER$check)]
    a <- a[own %in% "teacher", , drop = FALSE]
    qt <- unname(CONSULT_AUDIT_TYPE[a$check])
    qt[is.na(qt)] <- "other"
    for (q in unique(qt)) {
      r <- a[qt == q, , drop = FALSE]
      if (q == "renamed") {
        p <- do.call(rbind, strsplit(r$key, "|", fixed = TRUE))
        rr <- data.frame(from_uid = p[, 1], from_ver = p[, 2], to_uid = p[, 3], to_ver = p[, 4],
                         stringsAsFactors = FALSE)
        # 原文：。
        add(q, r$key, rel_target(rr), paste0(r$detail, "\u3002", rel_texts(rr)))
      } else if (q %in% c("single", "deprecated", "nonleaf")) {
        f <- sub("\\|.*$", "", r$key)
        g <- sub("^[^|]*\\|", "", r$key)
        # 原文：題本  | ：
        add(q, r$key, paste0("\u984c\u672c ", f, "\uff1a", consult_group_label(kit, g)), r$detail)
      } else {
        add(q, paste(r$check, r$table, r$key, sep = "|"),
            # 原文：：
            paste0(AUDIT_KIT_CHECKS$label[match(r$check, AUDIT_KIT_CHECKS$check)], "\uff1a", r$key),
            r$detail)
      }
    }
  }

  if ("relations" %in% include && nrow(rel)) {
    doubt <- rel$decided_by %in% c("", "auto") &
      (rel$comparable %in% c("partial", "no") | rel$meaning_changed == "1" |
         rel$similarity %in% c("medium", "low", "none"))
    r <- rel[doubt, , drop = FALSE]
    # 原文：系統判斷：文字相似 
    sys <- paste0("\u7cfb\u7d71\u5224\u65b7\uff1a\u6587\u5b57\u76f8\u4f3c ", ifelse(r$similarity == "", "?", r$similarity),
                  # 原文：、可比較 
                  "\u3001\u53ef\u6bd4\u8f03 ", ifelse(r$comparable == "", "?", r$comparable),
                  # 原文：、意思改了
                  ifelse(r$meaning_changed == "1", "\u3001\u610f\u601d\u6539\u4e86", ""))
    # 原文：。依據：
    why <- ifelse(r$reason == "", "", paste0("\u3002\u4f9d\u64da\uff1a", r$reason))
    add("relation", kit_key_str(r, KIT_SCHEMA$item_relations$key), rel_target(r),
        # 原文：。
        paste0(rel_texts(r), "\u3002", sys, why))
  }

  if ("blocks" %in% include) {
    bs <- consult_tb(kit, "block_status")
    r <- bs[bs$decided_by %in% c("", "auto"), , drop = FALSE]
    # 原文：全部都問 | 只問部分 | 帶入（沒再問） | 沒問
    asked <- c(all = "\u5168\u90e8\u90fd\u554f", subset = "\u53ea\u554f\u90e8\u5206", carried = "\u5e36\u5165\uff08\u6c92\u518d\u554f\uff09", none = "\u6c92\u554f")
    st <- unname(asked[r$asked])
    st[is.na(st)] <- r$asked[is.na(st)]
    add("block", kit_key_str(r, KIT_SCHEMA$block_status$key),
        # 原文：：
        paste0(r$datasetID, "\uff1a", r$block),
        # 原文：狀態： | ；問誰：
        paste0("\u72c0\u614b\uff1a", st, ifelse(r$who == "", "", paste0("\uff1b\u554f\u8ab0\uff1a", r$who)),
               # 原文：；依據：
               ifelse(r$reason == "", "", paste0("\uff1b\u4f9d\u64da\uff1a", r$reason))))
  }

  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

#' Build the consultation workbook (請教單)
#'
#' One row per question. Grey columns are written by the app and locked (the sheet is
#' protected without a password, so it can still be unprotected on purpose); the yellow columns
#' are for the teacher: 決定 (a dropdown whose options depend on the question), 理由 and 誰決定.
#' The right-most 系統用 column is what [read_consult()] matches on.
#'
#' @param kit A kit list.
#' @param rows Questions, from [consult_rows()] (computed when `NULL`).
#' @param file Optional path; when given the workbook is saved there.
#' @return An openxlsx workbook (invisibly when `file` is given). Needs the openxlsx package.
#' @export
consult_workbook <- function(kit, rows = NULL, file = NULL) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    kitDesign::kit_abort("consult_workbook() needs the openxlsx package.")
  }
  rows <- rows %||% consult_rows(kit)
  ord <- order(match(rows$qtype, names(CONSULT_TYPES)), method = "radix")
  rows <- rows[ord, , drop = FALSE]
  n <- nrow(rows)
  lab <- vapply(rows$qtype, function(q) CONSULT_TYPES[[q]]$label, "")
  df <- data.frame(sprintf("Q%04d", seq_len(n)), unname(lab), rows$target, rows$context,
                   rows$question, rep("", n), rep("", n), rep("", n),
                   paste(rows$qtype, rows$key, sep = "|"), stringsAsFactors = FALSE)
  names(df) <- unname(CONSULT_COLS)

  wb <- openxlsx::createWorkbook()
  hd <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DDE3EA", wrapText = TRUE,
                              border = "bottom", valign = "center")
  grey <- openxlsx::createStyle(fontColour = "#555555", wrapText = TRUE, valign = "top",
                                locked = TRUE)
  yellow <- openxlsx::createStyle(fgFill = "#FFF4C2", wrapText = TRUE, valign = "top",
                                  border = "TopBottomLeftRight", borderColour = "#E0C860",
                                  locked = FALSE)
  sys <- openxlsx::createStyle(fontColour = "#AAAAAA", fontSize = 8, locked = TRUE)

  howto <- c(
    # 原文：kitItemBank 請教單
    "kitItemBank \u8acb\u6559\u55ae",
    # 原文：產生日期： | ；題庫版本：
    paste0("\u7522\u751f\u65e5\u671f\uff1a", format(Sys.Date()), "\uff1b\u984c\u5eab\u7248\u672c\uff1a", kit_version() %||% ""),
    "",
    # 原文：這份表列的是要請老師判斷的事，一列一個問題。
    "\u9019\u4efd\u8868\u5217\u7684\u662f\u8981\u8acb\u8001\u5e2b\u5224\u65b7\u7684\u4e8b\uff0c\u4e00\u5217\u4e00\u500b\u554f\u984c\u3002",
    # 原文：1. 灰色欄是電腦整理的，鎖住了不能改。黃色欄給您填：
    "1. \u7070\u8272\u6b04\u662f\u96fb\u8166\u6574\u7406\u7684\uff0c\u9396\u4f4f\u4e86\u4e0d\u80fd\u6539\u3002\u9ec3\u8272\u6b04\u7d66\u60a8\u586b\uff1a",
    # 原文：   「決定」用下拉選單選；「理由」寫您的依據（例如題本、會議）；「誰決定」寫您的名字。
    "   \u300c\u6c7a\u5b9a\u300d\u7528\u4e0b\u62c9\u9078\u55ae\u9078\uff1b\u300c\u7406\u7531\u300d\u5beb\u60a8\u7684\u4f9d\u64da\uff08\u4f8b\u5982\u984c\u672c\u3001\u6703\u8b70\uff09\uff1b\u300c\u8ab0\u6c7a\u5b9a\u300d\u5beb\u60a8\u7684\u540d\u5b57\u3002",
    # 原文：2. 不確定的可以選「再討論」，或留白不填。
    "2. \u4e0d\u78ba\u5b9a\u7684\u53ef\u4ee5\u9078\u300c\u518d\u8a0e\u8ad6\u300d\uff0c\u6216\u7559\u767d\u4e0d\u586b\u3002",
    # 原文：3. 最右邊「系統用」欄是匯回時對應用的，請不要改、不要刪。
    "3. \u6700\u53f3\u908a\u300c\u7cfb\u7d71\u7528\u300d\u6b04\u662f\u532f\u56de\u6642\u5c0d\u61c9\u7528\u7684\uff0c\u8acb\u4e0d\u8981\u6539\u3001\u4e0d\u8981\u522a\u3002",
    # 原文：4. 填好後交回，由維護者在「我的工作」頁上傳；app 會做成修改包，經過審核才會進題庫。
    "4. \u586b\u597d\u5f8c\u4ea4\u56de\uff0c\u7531\u7dad\u8b77\u8005\u5728\u300c\u6211\u7684\u5de5\u4f5c\u300d\u9801\u4e0a\u50b3\uff1bapp \u6703\u505a\u6210\u4fee\u6539\u5305\uff0c\u7d93\u904e\u5be9\u6838\u624d\u6703\u9032\u984c\u5eab\u3002"
  )
  # 原文：說明
  openxlsx::addWorksheet(wb, "\u8aaa\u660e")
  # 原文：說明
  openxlsx::writeData(wb, "\u8aaa\u660e", howto, colNames = FALSE)
  # 原文：說明
  openxlsx::addStyle(wb, "\u8aaa\u660e", openxlsx::createStyle(fontSize = 14, textDecoration = "bold"),
                     rows = 1, cols = 1)
  cnt <- table(factor(lab, levels = vapply(CONSULT_TYPES, `[[`, "", "label")))
  cnt <- cnt[cnt > 0]
  # 原文：類別 | 題數
  intro <- data.frame("\u985e\u5225" = names(cnt), "\u984c\u6578" = as.integer(cnt), check.names = FALSE,
                      stringsAsFactors = FALSE)
  # 原文：說明
  openxlsx::writeData(wb, "\u8aaa\u660e", intro, startRow = length(howto) + 2L, headerStyle = hd)
  # 原文：說明
  openxlsx::setColWidths(wb, "\u8aaa\u660e", 1:2, c(80, 8))

  s <- CONSULT_SHEET
  openxlsx::addWorksheet(wb, s)
  openxlsx::writeData(wb, s, df, headerStyle = hd, withFilter = TRUE)
  ic <- match(CONSULT_COLS[c("decision", "reason", "decided_by")], names(df))
  if (n) {
    r <- seq_len(n) + 1L
    openxlsx::addStyle(wb, s, grey, rows = r, cols = setdiff(seq_along(df), ic), gridExpand = TRUE)
    openxlsx::addStyle(wb, s, yellow, rows = r, cols = ic, gridExpand = TRUE)
    openxlsx::addStyle(wb, s, sys, rows = r, cols = ncol(df), gridExpand = TRUE)
    # 每種問題自己的下拉選單（列已照種類排好，同種類的列連在一起）
    for (q in unique(rows$qtype)) {
      rr <- which(rows$qtype == q) + 1L
      # 原文：，
      ch <- gsub(",", "\uff0c", CONSULT_TYPES[[q]]$choices, fixed = TRUE)
      openxlsx::dataValidation(wb, s, cols = ic[1], rows = rr, type = "list",
                               value = paste0('"', paste(ch, collapse = ","), '"'))
    }
  }
  openxlsx::freezePane(wb, s, firstRow = TRUE, firstCol = TRUE)
  openxlsx::setColWidths(wb, s, seq_along(df), c(8, 14, 28, 60, 30, 22, 30, 12, 14))
  openxlsx::protectWorksheet(wb, s, protect = TRUE, lockSelectingLockedCells = FALSE,
                             lockSelectingUnlockedCells = FALSE, lockFormattingColumns = FALSE,
                             lockFormattingRows = FALSE, lockAutoFilter = FALSE,
                             lockSorting = FALSE)
  if (!is.null(file)) {
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    return(invisible(wb))
  }
  wb
}

#' Read back a filled-in consultation workbook
#'
#' @param path The `.xlsx` made by [consult_workbook()] and filled in.
#' @return A data frame with one row per answered question (決定 or 理由 not blank): `id`,
#'   `qtype`, `key`, `decision` (the text picked), `action` (what it means, see
#'   `CONSULT_TYPES`; `""` when the text is not one of the options), `reason`, `decided_by`.
#' @export
read_consult <- function(path) {
  d <- as.data.frame(readxl::read_excel(path, sheet = CONSULT_SHEET, col_types = "text"),
                     stringsAsFactors = FALSE)
  miss <- setdiff(CONSULT_COLS[c("id", "decision", "reason", "decided_by", "key")], names(d))
  if (length(miss)) {
    # 原文：這不是請教單：少了欄位  | 、
    kitDesign::kit_abort("\u9019\u4e0d\u662f\u8acb\u6559\u55ae\uff1a\u5c11\u4e86\u6b04\u4f4d ", paste(miss, collapse = "\u3001"))
  }
  txt <- function(v) { v <- as.character(v); v[is.na(v)] <- ""; trimws(v) }
  dec <- txt(d[[CONSULT_COLS[["decision"]]]])
  why <- txt(d[[CONSULT_COLS[["reason"]]]])
  sys <- txt(d[[CONSULT_COLS[["key"]]]])
  keep <- (nzchar(dec) | nzchar(why)) & nzchar(sys)
  qtype <- sub("\\|.*$", "", sys[keep])
  act <- vapply(seq_along(qtype), function(i) {
    ty <- CONSULT_TYPES[[qtype[i]]]
    if (is.null(ty)) return("")
    # 原文：，
    ch <- gsub(",", "\uff0c", ty$choices, fixed = TRUE)
    a <- ty$actions[match(dec[keep][i], ch)]
    if (is.na(a)) "" else a
  }, "")
  out <- data.frame(id = txt(d[[CONSULT_COLS[["id"]]]])[keep], qtype = qtype,
                    key = sub("^[^|]*\\|", "", sys[keep]), decision = dec[keep], action = act,
                    reason = why[keep], decided_by = txt(d[[CONSULT_COLS[["decided_by"]]]])[keep],
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

#' Turn consultation answers into a change package
#'
#' Nothing in the kit is changed: the result is a patch (same rows as [kit_patch()]) that goes
#' through the normal review and is applied with [apply_patch()]. Decisions are written into the
#' rows they are about: `item_relations` gets `comparable`, `decided_by`, `decided_on`,
#' `reason` and `sourceID`; `block_status` gets `decided_by`, `reason` and `sourceID` (it has
#' no date column); rows judged wrong are deleted; a renamed item that should keep the old
#' item's groups gets the same `group_members` rows. `group_members` has no columns for who
#' decided, so "keep" answers about groups only appear in `notes`.
#'
#' @param kit The kit the workbook was made from.
#' @param answers From [read_consult()].
#' @param decided_by Used when a row's 誰決定 is blank.
#' @param decided_on Date of the decision.
#' @param source A `sourceID` to record as the basis (added to `sources` as a meeting when it is
#'   not there yet); `NULL` to leave `sourceID` alone.
#' @return A list: `patch` (data frame `op`, `table`, `key`, `column`, `from`, `to`) and `notes`
#'   (answers that change nothing in the kit, with `why`).
#' @export
consult_patch <- function(kit, answers, decided_by = "", decided_on = Sys.Date(),
                          source = paste0("CONSULT-", format(as.Date(decided_on), "%Y%m%d"))) {
  day <- format(as.Date(decided_on), "%Y-%m-%d")
  out <- list(empty_patch())
  addp <- function(x) out[[length(out) + 1L]] <<- x
  notes <- list()
  note <- function(i, why) {
    notes[[length(notes) + 1L]] <<- data.frame(
      id = answers$id[i], qtype = answers$qtype[i], key = answers$key[i],
      decision = answers$decision[i], reason = answers$reason[i],
      decided_by = answers$decided_by[i], why = why, stringsAsFactors = FALSE)
  }
  tb <- list(item_relations = consult_tb(kit, "item_relations"),
             block_status = consult_tb(kit, "block_status"),
             group_members = consult_tb(kit, "group_members"))
  keys <- lapply(names(tb), function(t) kit_key_str(tb[[t]], KIT_SCHEMA[[t]]$key))
  names(keys) <- names(tb)
  gm_keys <- keys$group_members
  deleted <- character(0)
  used_source <- FALSE

  # 一格改成 to（和原本一樣就不列）
  set_cell <- function(t, k, col, to) {
    from <- tb[[t]][[col]][match(k, keys[[t]])]
    if (!identical(from, to)) addp(patch_rows("set", t, k, col, from, to))
  }
  record <- function(t, i, k) {
    who <- answers$decided_by[i]
    if (!nzchar(who)) who <- decided_by
    if (nzchar(who)) set_cell(t, k, "decided_by", who)
    if (t == "item_relations") set_cell(t, k, "decided_on", day)
    if (nzchar(answers$reason[i])) set_cell(t, k, "reason", answers$reason[i])
    if (!is.null(source) && nzchar(source)) {
      set_cell(t, k, "sourceID", source)
      used_source <<- TRUE
    }
  }
  forms_of <- function(u) {
    dv <- consult_tb(kit, "dataset_vars")
    ds <- consult_tb(kit, "datasets")
    f <- unique(ds$formID[ds$datasetID %in% dv$datasetID[dv$item_uid == u]])
    f[nzchar(f)]
  }

  for (i in seq_len(nrow(answers))) {
    q <- answers$qtype[i]; a <- answers$action[i]; k <- answers$key[i]
    ty <- CONSULT_TYPES[[q]]
    # 原文：看不懂這一列的系統用欄
    if (is.null(ty)) { note(i, "\u770b\u4e0d\u61c2\u9019\u4e00\u5217\u7684\u7cfb\u7d71\u7528\u6b04"); next }
    if (!nzchar(a)) {
      # 原文：決定不是選單裡的選項 | 只寫了理由，沒有選決定
      note(i, if (nzchar(answers$decision[i])) "\u6c7a\u5b9a\u4e0d\u662f\u9078\u55ae\u88e1\u7684\u9078\u9805" else "\u53ea\u5beb\u4e86\u7406\u7531\uff0c\u6c92\u6709\u9078\u6c7a\u5b9a")
      next
    }
    if (a %in% c("keep", "note", "discuss")) {
      # 原文：維持原狀，題庫不用改
      note(i, switch(a, keep = "\u7dad\u6301\u539f\u72c0\uff0c\u984c\u5eab\u4e0d\u7528\u6539",
                     # 原文：要人工處理（看理由） | 再討論
                     note = "\u8981\u4eba\u5de5\u8655\u7406\uff08\u770b\u7406\u7531\uff09", discuss = "\u518d\u8a0e\u8ad6"))
      next
    }
    t <- ty$table
    if (q %in% c("single", "deprecated")) {
      f <- sub("\\|.*$", "", k); g <- sub("^[^|]*\\|", "", k)
      x <- tb$group_members$formID == f & tb$group_members$groupID == g
      dk <- setdiff(gm_keys[x], deleted)
      # 原文：題庫裡找不到這個題組成員
      if (!length(dk)) { note(i, "\u984c\u5eab\u88e1\u627e\u4e0d\u5230\u9019\u500b\u984c\u7d44\u6210\u54e1"); next }
      addp(patch_rows("delete", "group_members", dk))
      deleted <- c(deleted, dk)
      next
    }
    # 原文：題庫裡找不到這一列（可能已經改過）
    if (!k %in% keys[[t]] || k %in% deleted) { note(i, "\u984c\u5eab\u88e1\u627e\u4e0d\u5230\u9019\u4e00\u5217\uff08\u53ef\u80fd\u5df2\u7d93\u6539\u904e\uff09"); next }
    if (a == "delete") {
      addp(patch_rows("delete", t, k))
      deleted <- c(deleted, k)
      next
    }
    if (startsWith(a, "comparable_")) set_cell(t, k, "comparable", sub("^comparable_", "", a))
    if (a == "join_groups") {
      p <- strsplit(k, "|", fixed = TRUE)[[1L]]
      old <- tb$group_members[tb$group_members$item_uid == p[1L], , drop = FALSE]
      fn <- forms_of(p[3L])
      for (j in seq_len(nrow(old))) {
        for (f in if (length(fn)) fn else old$formID[j]) {
          nk <- paste(f, old$groupID[j], p[3L], old$scheme[j], sep = "|")
          if (nk %in% gm_keys) next
          addp(patch_rows("add", "group_members", nk))
          if (nzchar(old$reverse[j])) addp(patch_rows("set", "group_members", nk, "reverse", "", old$reverse[j]))
          if (nzchar(old$weight[j])) addp(patch_rows("set", "group_members", nk, "weight", "", old$weight[j]))
          gm_keys <- c(gm_keys, nk)
        }
      }
    }
    record(t, i, k)
  }

  patch <- do.call(rbind, out)
  if (used_source && !source %in% consult_tb(kit, "sources")$sourceID) {
    patch <- rbind(patch_rows("add", "sources", source),
                   patch_rows("set", "sources", rep(source, 3L), c("kind", "title", "date"), "",
                              # 原文：請教單
                              c("meeting", "\u8acb\u6559\u55ae", day)),
                   patch)
  }
  patch <- patch[!duplicated(patch), , drop = FALSE]
  rownames(patch) <- NULL
  nt <- if (length(notes)) do.call(rbind, notes) else data.frame(
    id = character(0), qtype = character(0), key = character(0), decision = character(0),
    reason = character(0), decided_by = character(0), why = character(0),
    stringsAsFactors = FALSE)
  list(patch = patch, notes = nt)
}

#' 修改包 Excel 的工作表（read_patch_xlsx() 讀得回來）：說明、改動、沒有改到題庫的回覆
#' @noRd
consult_patch_sheets <- function(res, base_version = kit_version()) {
  info <- data.frame(
    # 原文：項目
    "\u9805\u76ee" = c("kitItemBank", "source", "base_version", "date", "n_patch", "n_notes"),
    # 原文：內容 | 0.3 修改包 | 請教單
    "\u5167\u5bb9" = c("0.3 \u4fee\u6539\u5305", "\u8acb\u6559\u55ae", base_version %||% "", format(Sys.Date()),
              nrow(res$patch), nrow(res$notes)),
    check.names = FALSE, stringsAsFactors = FALSE)
  sheets <- list(info, res$patch)
  # 原文：說明 | 改動
  names(sheets) <- c("\u8aaa\u660e", "\u6539\u52d5")
  # 原文：沒有改到題庫的回覆
  if (nrow(res$notes)) sheets[["\u6c92\u6709\u6539\u5230\u984c\u5eab\u7684\u56de\u8986"]] <- res$notes
  sheets
}
