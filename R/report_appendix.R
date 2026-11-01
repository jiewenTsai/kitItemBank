# R/report_appendix.R — 0.3.11 期中／期末報告附表
#
# 寫期中、期末報告時要附的三張表，全部從題庫整理，不用另外統計：
#   (a) 今年題本改了什麼：每個題本這一年的改版說明（form_notes）和題目之間的關係
#       （item_relations：從選定的施測開始用的改版、換題號、拆開、合併、借用）
#   (b) 各波次信效度：正式分析（analyses.official = 1）的題組統計（group_stats）：α、ω、人數
#   (c) 施測一覽：這段期間的施測、資料檔和題數
# report_appendix() 整理成資料表；report_appendix_docx() 寫 Word（樣式見 report_docx.R），
# report_appendix_workbook() 給 writexl 寫 Excel；report_appendix_ui() 是施測頁用的選單和下載鈕。
#
# 「哪一年的施測」看調查開始的日期（admins.surveyFrom 的年份）：新題本從那次施測開始用，
# form_notes 的 year（2017、2017.09）也是照這個記。

# 原文：期中／期末報告附表
RA_TITLE <- "\u671f\u4e2d\uff0f\u671f\u672b\u5831\u544a\u9644\u8868"

# 關係的白話（和沿革單一樣）見 ITEM_REL_ZH（R/item_history.R）
RA_REL_KINDS <- c("revised", "renamed", "split", "merged", "borrowed")

#' 每次施測開始的年份
#' @noRd
ra_admin_year <- function(kit, ids = kit$admins$adminID) {
  substr(cb_blank(kit$admins$surveyFrom[match(ids, kit$admins$adminID)]), 1L, 4L)
}

#' 題目版本的寫法：P.cogc01 第 2 版
#' @noRd
# 原文： 第  |  版
ra_item_ref <- function(uid, ver) paste0(uid, " \u7b2c ", ver, " \u7248")

#' 一列關係 → 一句白話
#' @noRd
ra_relation_text <- function(r) {
  from <- ra_item_ref(r$from_uid, r$from_ver)
  to <- ra_item_ref(r$to_uid, r$to_ver)
  same <- r$from_uid == r$to_uid
  out <- ifelse(
    r$kind == "revised",
    # 原文： 從第  |  版改成第  |  版
    ifelse(same, paste0(r$to_uid, " \u5f9e\u7b2c ", r$from_ver, " \u7248\u6539\u6210\u7b2c ", r$to_ver, " \u7248"),
           # 原文： 改版成 
           paste0(from, " \u6539\u7248\u6210 ", to)),
    # 原文： 換題號，變成 
    ifelse(r$kind == "renamed", paste0(from, " \u63db\u984c\u865f\uff0c\u8b8a\u6210 ", to),
    # 原文： 拆開，其中一題是 
    ifelse(r$kind == "split", paste0(from, " \u62c6\u958b\uff0c\u5176\u4e2d\u4e00\u984c\u662f ", to),
    # 原文： 和其他題合併成 
    ifelse(r$kind == "merged", paste0(from, " \u548c\u5176\u4ed6\u984c\u5408\u4f75\u6210 ", to),
    # 原文： 借用 
    ifelse(r$kind == "borrowed", paste0(to, " \u501f\u7528 ", from),
           # 原文： →  | （ | ）
           paste0(from, " \u2192 ", to, "\uff08", r$kind, "\uff09")))))
  )
  # 原文：（從  |  起）
  ifelse(nzchar(r$effective_admin), paste0(out, "\uff08\u5f9e ", r$effective_admin, " \u8d77\uff09"), out)
}

#' 一列關係在哪個題本：施測裡出現這個新版本的資料檔的題本；找不到時用同一問卷的題本
#' @noRd
ra_relation_forms <- function(kit, r) {
  ds <- kit$datasets
  dv <- kit$dataset_vars
  inst <- kit$items$instrument[match(r$to_uid, kit$items$item_uid)]
  inst[is.na(inst)] <- sub("\\..*$", "", r$to_uid[is.na(inst)])
  lapply(seq_len(nrow(r)), function(i) {
    d <- ds[ds$adminID == r$effective_admin[i] & nzchar(ds$formID), , drop = FALSE]
    hit <- dv$datasetID[dv$item_uid == r$to_uid[i] & dv$itemVer == r$to_ver[i] &
                          dv$datasetID %in% d$datasetID]
    f <- unique(d$formID[d$datasetID %in% hit])
    if (!length(f)) f <- unique(d$formID[d$instrument == inst[i]])
    if (!length(f)) "" else f
  })
}

#' 範圍：選定的施測、題本說明的年份、要不要限題本
#' @noRd
ra_scope <- function(kit, year, admins) {
  all_ids <- kit$admins$adminID
  if (!is.null(year)) year <- unique(cb_blank(year))
  year <- year[nzchar(year %||% character(0))]
  if (!length(year)) year <- NULL
  if (!is.null(admins) && length(admins) && any(nzchar(cb_blank(admins)))) {
    admins <- cb_blank(admins)
    admins <- admins[nzchar(admins)]
    ids <- unique(unlist(lapply(admins, function(a) {
      hit <- resolve_admin(kit, a)
      # 原文：題庫裡找不到施測「 | 」。
      if (!length(hit)) stop("\u984c\u5eab\u88e1\u627e\u4e0d\u5230\u65bd\u6e2c\u300c", a, "\u300d\u3002", call. = FALSE)
      hit
    })))
    ids <- all_ids[all_ids %in% ids]
    years <- year %||% unique(ra_admin_year(kit, ids))
    forms <- unique(kit$datasets$formID[kit$datasets$adminID %in% ids])
    # 原文：選定的  |  次施測： | 、
    text <- paste0("\u9078\u5b9a\u7684 ", length(ids), " \u6b21\u65bd\u6e2c\uff1a", paste(ids, collapse = "\u3001"))
    return(list(admins = ids, years = years[nzchar(years)], forms = forms[nzchar(forms)],
                year = year, text = text))
  }
  if (!is.null(year)) {
    ids <- all_ids[ra_admin_year(kit, all_ids) %in% year]
    # 原文：、 |  年開始調查的施測（ |  次）
    text <- paste0(paste(year, collapse = "\u3001"), " \u5e74\u958b\u59cb\u8abf\u67e5\u7684\u65bd\u6e2c\uff08", length(ids), " \u6b21\uff09")
    return(list(admins = ids, years = year, forms = NULL, year = year, text = text))
  }
  list(admins = all_ids, years = NULL, forms = NULL, year = NULL,
       # 原文：題庫裡全部的施測（ |  次）
       text = paste0("\u984c\u5eab\u88e1\u5168\u90e8\u7684\u65bd\u6e2c\uff08", length(all_ids), " \u6b21\uff09"))
}

#' Appendix tables for interim and final reports
#'
#' Collects, from the bank alone, the three tables a progress report appends:
#' what changed in the forms, reliability by wave, and the administrations of the
#' period. An administration belongs to the year in which its survey started
#' (`admins.surveyFrom`).
#'
#' @param kit A kit, from [read_kit()].
#' @param year Optional year(s), e.g. `2020`: the administrations that started that year
#'   and the `form_notes` of that year (`2017.09` counts as 2017).
#' @param admins Optional adminIDs (or aliases); when given they decide the
#'   administrations, and the form notes are those of their forms (and of `year`, or of
#'   the years the administrations started). Without `year` and `admins`: the whole bank.
#' @param version Bank version written to the 說明 part.
#' @param date Date written to the 說明 part.
#' @return A list:
#'   - `scope`: `text`, `year`, `admins`, `version`, `date`;
#'   - `form_changes`: one row per form with notes or relations: `formID`, `form_label`,
#'     `notes` (the form notes' summaries), `n_revised`, `n_renamed`, `n_split`,
#'     `n_merged`, `n_borrowed`, `n_relations`;
#'   - `change_details`: one row per form note or relation: `formID`, `form_label`,
#'     `what` ("題本說明" or "題目"), `year`, `adminID`, `kind`, `kind_label`, `text`
#'     (a plain sentence), `reason`, `decided_by`;
#'   - `reliability`: one row per group and data file of an official analysis: `adminID`,
#'     `admin_label`, `datasetID`, `formID`, `groupID`, `group_label`, `n_items`, `n`,
#'     `alpha`, `omega_total`, `analysisID`, `date`;
#'   - `admins`: one row per administration: `adminID`, `label`, `period`, `status`,
#'     `release`, `n_datasets`, `files`, `n_item_vars`, `n_items`;
#'   - `notes`: plain notes (`reliability` when there is nothing to show).
#' @examples
#' app <- report_appendix(read_kit(), year = 2020)
#' app$form_changes
#' @export
report_appendix <- function(kit, year = NULL, admins = NULL,
                            version = codebook_bank_version(), date = Sys.Date()) {
  sc <- ra_scope(kit, year, admins)
  form_label <- function(f) {
    l <- cb_blank(kit$forms$label[match(f, kit$forms$formID)])
    ifelse(nzchar(l), l, f)
  }

  # (a) 題本改了什麼 ----
  fn <- kit$form_notes
  if (!is.null(sc$years)) fn <- fn[substr(fn$year, 1L, 4L) %in% sc$years, , drop = FALSE]
  if (!is.null(sc$forms)) fn <- fn[fn$formID %in% sc$forms, , drop = FALSE]
  rel <- kit$item_relations[kit$item_relations$effective_admin %in% sc$admins, , drop = FALSE]
  rel_forms <- ra_relation_forms(kit, rel)
  n_each <- lengths(rel_forms)
  rel <- rel[rep(seq_len(nrow(rel)), n_each), , drop = FALSE]
  rel$formID <- unlist(rel_forms) %||% character(0)
  kind_label <- unname(ITEM_REL_ZH[rel$kind])
  kind_label[is.na(kind_label)] <- rel$kind[is.na(kind_label)]
  det <- rbind(
    # 原文：題本說明
    data.frame(formID = fn$formID, what = rep("\u984c\u672c\u8aaa\u660e", nrow(fn)), year = fn$year,
               adminID = rep("", nrow(fn)), kind = rep("", nrow(fn)),
               kind_label = rep("", nrow(fn)), text = cb_one_line(fn$summary),
               reason = cb_one_line(fn$reason), decided_by = rep("", nrow(fn)),
               stringsAsFactors = FALSE),
    # 原文：題目
    data.frame(formID = rel$formID, what = rep("\u984c\u76ee", nrow(rel)),
               year = ra_admin_year(kit, rel$effective_admin), adminID = rel$effective_admin,
               kind = rel$kind, kind_label = kind_label, text = ra_relation_text(rel),
               reason = cb_one_line(rel$reason), decided_by = cb_blank(rel$decided_by),
               stringsAsFactors = FALSE)
  )
  form_order <- function(f) {
    o <- match(f, kit$forms$formID)
    ifelse(is.na(o), length(kit$forms$formID) + 1L, o)
  }
  # 原文：題本說明
  det <- det[order(form_order(det$formID), det$formID, det$what != "\u984c\u672c\u8aaa\u660e", det$year,
                   match(det$kind, RA_REL_KINDS), det$text), , drop = FALSE]
  det <- cbind(det[1L], form_label = form_label(det$formID), det[-1L], stringsAsFactors = FALSE)
  rownames(det) <- NULL
  forms <- unique(det$formID)
  fc <- data.frame(
    formID = forms, form_label = form_label(forms),
    notes = vapply(forms, function(f) {
      # 原文：題本說明
      x <- det[det$formID == f & det$what == "\u984c\u672c\u8aaa\u660e", , drop = FALSE]
      # 原文：： | ；
      paste(paste0(x$year, "\uff1a", x$text), collapse = "\uff1b")
    }, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
  for (k in RA_REL_KINDS) {
    fc[[paste0("n_", k)]] <- vapply(forms, function(f) sum(det$formID == f & det$kind == k),
                                    integer(1), USE.NAMES = FALSE)
  }
  # 原文：題目
  fc$n_relations <- vapply(forms, function(f) sum(det$formID == f & det$what == "\u984c\u76ee"),
                           integer(1), USE.NAMES = FALSE)

  # (b) 各波次信效度 ----
  an <- kit$analyses %||% empty_kit_table("analyses")
  gs <- kit$group_stats %||% empty_kit_table("group_stats")
  off <- an[an$official == "1", , drop = FALSE]
  off$adminID <- cb_blank(kit$datasets$adminID[match(off$datasetID, kit$datasets$datasetID)])
  off_in <- off[off$adminID %in% sc$admins, , drop = FALSE]
  g <- gs[gs$analysisID %in% off_in$analysisID & !nzchar(gs$varname), , drop = FALSE]
  hit <- match(g$analysisID, off_in$analysisID)
  ds_hit <- match(off_in$datasetID[hit], kit$datasets$datasetID)
  rl <- data.frame(
    adminID = off_in$adminID[hit], admin_label = admin_label(kit, off_in$adminID[hit]),
    datasetID = off_in$datasetID[hit], formID = cb_blank(kit$datasets$formID[ds_hit]),
    groupID = g$groupID, group_label = cb_blank(kit$groups$label[match(g$groupID, kit$groups$groupID)]),
    n_items = g$n_items, n = g$n, alpha = g$alpha, omega_total = g$omega_total,
    analysisID = g$analysisID, date = off_in$date[hit], stringsAsFactors = FALSE)
  rl <- rl[order(match(rl$adminID, sc$admins), rl$datasetID, rl$groupID), , drop = FALSE]
  rownames(rl) <- NULL
  notes <- list()
  if (!nrow(rl)) {
    notes$reliability <- if (!nrow(off)) {
      # 原文：題庫裡還沒有正式的信效度分析（analyses 裡沒有 official = 1 的分析），這一節先空著。
      "\u984c\u5eab\u88e1\u9084\u6c92\u6709\u6b63\u5f0f\u7684\u4fe1\u6548\u5ea6\u5206\u6790\uff08analyses \u88e1\u6c92\u6709 official = 1 \u7684\u5206\u6790\uff09\uff0c\u9019\u4e00\u7bc0\u5148\u7a7a\u8457\u3002"
    } else if (!nrow(off_in)) {
      # 原文：選定的施測還沒有正式的信效度分析，這一節先空著。
      "\u9078\u5b9a\u7684\u65bd\u6e2c\u9084\u6c92\u6709\u6b63\u5f0f\u7684\u4fe1\u6548\u5ea6\u5206\u6790\uff0c\u9019\u4e00\u7bc0\u5148\u7a7a\u8457\u3002"
    } else {
      # 原文：選定施測的正式分析裡沒有題組層次的統計（α、ω），這一節先空著。
      "\u9078\u5b9a\u65bd\u6e2c\u7684\u6b63\u5f0f\u5206\u6790\u88e1\u6c92\u6709\u984c\u7d44\u5c64\u6b21\u7684\u7d71\u8a08\uff08\u03b1\u3001\u03c9\uff09\uff0c\u9019\u4e00\u7bc0\u5148\u7a7a\u8457\u3002"
    }
  }

  # (c) 施測一覽 ----
  ov <- admin_summary(kit)
  ov <- ov[ov$adminID %in% sc$admins, , drop = FALSE]
  ds <- kit$datasets
  files <- vapply(ov$adminID, function(a) {
    d <- ds[ds$adminID == a, , drop = FALSE]
    if (!nrow(d)) return("")
    # 原文：（ | ， | ） | 、
    paste0(d$datasetID, "\uff08", cb_respondent(d), "\uff0c", d$file, "\uff09", collapse = "\u3001")
  }, character(1), USE.NAMES = FALSE)
  ad <- data.frame(adminID = ov$adminID, label = ov$label, period = ov$period,
                   status = ov$status_label, release = ov$release,
                   n_datasets = ov$n_datasets, files = files,
                   n_item_vars = ov$n_item_vars, n_items = ov$n_items,
                   stringsAsFactors = FALSE)
  rownames(ad) <- NULL

  list(scope = list(text = sc$text, year = sc$year, admins = sc$admins,
                    version = version %||% "", date = format(as.Date(date), "%Y-%m-%d")),
       form_changes = fc, change_details = det, reliability = rl, admins = ad, notes = notes)
}

# 給人看的表 ----

#' 數字欄 → 兩位小數
#' @noRd
ra_num <- function(x) stats_num_str(x, 2L)

#' 說明：項目、內容
#' @noRd
ra_info <- function(app) {
  row <- function(k, x) data.frame(k = rep(k, length(x)), x = as.character(x),
                                   stringsAsFactors = FALSE)
  info <- rbind(
    # 原文：這個檔案 | KIT 幼兒發展調查資料庫 | ：今年題本改了什麼、各波次信效度、施測一覽。全部從題庫整理。
    row("\u9019\u500b\u6a94\u6848", paste0("KIT \u5e7c\u5152\u767c\u5c55\u8abf\u67e5\u8cc7\u6599\u5eab", RA_TITLE, "\uff1a\u4eca\u5e74\u984c\u672c\u6539\u4e86\u4ec0\u9ebc\u3001\u5404\u6ce2\u6b21\u4fe1\u6548\u5ea6\u3001\u65bd\u6e2c\u4e00\u89bd\u3002\u5168\u90e8\u5f9e\u984c\u5eab\u6574\u7406\u3002")),
    # 原文：題庫版本
    row("\u984c\u5eab\u7248\u672c", app$scope$version),
    # 原文：產生日期
    row("\u7522\u751f\u65e5\u671f", app$scope$date),
    # 原文：範圍
    row("\u7bc4\u570d", app$scope$text),
    # 原文：怎麼算年份 | 一次施測算在它開始調查的那一年（施測的調查期間起日）；題本說明照題庫記的年份（2017.09 算 2017 年）。
    row("\u600e\u9ebc\u7b97\u5e74\u4efd", "\u4e00\u6b21\u65bd\u6e2c\u7b97\u5728\u5b83\u958b\u59cb\u8abf\u67e5\u7684\u90a3\u4e00\u5e74\uff08\u65bd\u6e2c\u7684\u8abf\u67e5\u671f\u9593\u8d77\u65e5\uff09\uff1b\u984c\u672c\u8aaa\u660e\u7167\u984c\u5eab\u8a18\u7684\u5e74\u4efd\uff082017.09 \u7b97 2017 \u5e74\uff09\u3002"),
    # 原文：題本改了什麼 | 題本說明來自 form_notes；題目的改版、換題號、拆開、合併、借用來自 item_relations，只列從範圍內的施測開始用的。
    row("\u984c\u672c\u6539\u4e86\u4ec0\u9ebc", "\u984c\u672c\u8aaa\u660e\u4f86\u81ea form_notes\uff1b\u984c\u76ee\u7684\u6539\u7248\u3001\u63db\u984c\u865f\u3001\u62c6\u958b\u3001\u5408\u4f75\u3001\u501f\u7528\u4f86\u81ea item_relations\uff0c\u53ea\u5217\u5f9e\u7bc4\u570d\u5167\u7684\u65bd\u6e2c\u958b\u59cb\u7528\u7684\u3002"),
    # 原文：信效度 | 只用正式分析（analyses.official = 1）的題組統計（group_stats）：α 來自 psych::alpha，ω_total 由單因素負荷量算出。
    row("\u4fe1\u6548\u5ea6", "\u53ea\u7528\u6b63\u5f0f\u5206\u6790\uff08analyses.official = 1\uff09\u7684\u984c\u7d44\u7d71\u8a08\uff08group_stats\uff09\uff1a\u03b1 \u4f86\u81ea psych::alpha\uff0c\u03c9_total \u7531\u55ae\u56e0\u7d20\u8ca0\u8377\u91cf\u7b97\u51fa\u3002"),
    # 原文：信效度說明
    if (length(app$notes$reliability)) row("\u4fe1\u6548\u5ea6\u8aaa\u660e", app$notes$reliability)
  )
  # 原文：項目 | 內容
  names(info) <- c("\u9805\u76ee", "\u5167\u5bb9")
  rownames(info) <- NULL
  info
}

#' 題本改動摘要（中文欄名）
#' @noRd
ra_form_changes_table <- function(app) {
  fc <- app$form_changes
  # 原文：題本 | 題本名稱 | 題本說明
  data.frame("\u984c\u672c" = fc$formID, "\u984c\u672c\u540d\u7a31" = fc$form_label, "\u984c\u672c\u8aaa\u660e" = fc$notes,
             # 原文：改版 | 換題號 | 拆開
             "\u6539\u7248" = fc$n_revised, "\u63db\u984c\u865f" = fc$n_renamed, "\u62c6\u958b" = fc$n_split,
             # 原文：合併 | 借用 | 題目改動合計
             "\u5408\u4f75" = fc$n_merged, "\u501f\u7528" = fc$n_borrowed, "\u984c\u76ee\u6539\u52d5\u5408\u8a08" = fc$n_relations,
             check.names = FALSE, stringsAsFactors = FALSE)
}

#' 題本改動明細（中文欄名）
#' @noRd
ra_details_table <- function(app) {
  d <- app$change_details
  # 原文：題本 | 類別 | 年份 | 施測
  data.frame("\u984c\u672c" = d$formID, "\u985e\u5225" = d$what, "\u5e74\u4efd" = d$year, "\u65bd\u6e2c" = d$adminID,
             # 原文：關係 | 內容 | 理由 | 決定者
             "\u95dc\u4fc2" = d$kind_label, "\u5167\u5bb9" = d$text, "\u7406\u7531" = d$reason, "\u6c7a\u5b9a\u8005" = d$decided_by,
             check.names = FALSE, stringsAsFactors = FALSE)
}

#' 信效度（中文欄名）
#' @noRd
ra_reliability_table <- function(app) {
  r <- app$reliability
  # 原文：施測 | 資料檔 | 題本
  data.frame("\u65bd\u6e2c" = r$adminID, "\u8cc7\u6599\u6a94" = r$datasetID, "\u984c\u672c" = r$formID,
             # 原文：題組 | 題組名稱 | 題數
             "\u984c\u7d44" = r$groupID, "\u984c\u7d44\u540d\u7a31" = r$group_label, "\u984c\u6578" = r$n_items,
             # 原文：人數 | α | ω
             "\u4eba\u6578" = r$n, "\u03b1" = ra_num(r$alpha), "\u03c9" = ra_num(r$omega_total),
             # 原文：分析代號
             "\u5206\u6790\u4ee3\u865f" = r$analysisID, check.names = FALSE, stringsAsFactors = FALSE)
}

#' 施測一覽（中文欄名）
#' @noRd
ra_admins_table <- function(app) {
  a <- app$admins
  # 原文：施測 | 名稱 | 調查期間 | 狀態
  data.frame("\u65bd\u6e2c" = a$adminID, "\u540d\u7a31" = a$label, "\u8abf\u67e5\u671f\u9593" = a$period, "\u72c0\u614b" = a$status,
             # 原文：釋出編號 | 資料檔數 | 資料檔
             "\u91cb\u51fa\u7de8\u865f" = a$release, "\u8cc7\u6599\u6a94\u6578" = a$n_datasets, "\u8cc7\u6599\u6a94" = a$files,
             # 原文：題目變項數 | 題數
             "\u984c\u76ee\u8b8a\u9805\u6578" = a$n_item_vars, "\u984c\u6578" = a$n_items,
             check.names = FALSE, stringsAsFactors = FALSE)
}

#' Excel workbook of the report appendix
#'
#' @param app The result of [report_appendix()].
#' @return A named list of data frames for `writexl::write_xlsx()`; the first sheet is
#'   說明 (bank version, date, scope).
#' @export
report_appendix_workbook <- function(app) {
  # 原文：說明 | 題本改動摘要
  list("\u8aaa\u660e" = ra_info(app), "\u984c\u672c\u6539\u52d5\u6458\u8981" = ra_form_changes_table(app),
       # 原文：題本改動明細 | 各波次信效度
       "\u984c\u672c\u6539\u52d5\u660e\u7d30" = ra_details_table(app), "\u5404\u6ce2\u6b21\u4fe1\u6548\u5ea6" = ra_reliability_table(app),
       # 原文：施測一覽
       "\u65bd\u6e2c\u4e00\u89bd" = ra_admins_table(app))
}

#' Write the report appendix to Word
#'
#' @param app The result of [report_appendix()].
#' @param path Output `.docx` path.
#' @return `path`, invisibly.
#' @export
report_appendix_docx <- function(app, path) {
  doc <- kit_docx_styles(officer::read_docx())
  add <- function(text, style = "Normal") {
    doc <<- officer::body_add_par(doc, as.character(text), style = style)
  }
  table <- function(df) doc <<- officer::body_add_table(doc, df, style = "table_template")
  add(RA_TITLE, KIT_DOCX_TITLE)

  # 原文：說明
  add("\u8aaa\u660e", KIT_DOCX_HEADING)
  info <- ra_info(app)
  # 原文：：
  for (i in seq_len(nrow(info))[-1L]) add(paste0(info[i, 1], "\uff1a", info[i, 2]))

  # 原文：一、今年題本改了什麼
  add("\u4e00\u3001\u4eca\u5e74\u984c\u672c\u6539\u4e86\u4ec0\u9ebc", KIT_DOCX_HEADING)
  if (!nrow(app$form_changes)) {
    # 原文：這段期間題庫沒有記下題本的改動。
    add("\u9019\u6bb5\u671f\u9593\u984c\u5eab\u6c92\u6709\u8a18\u4e0b\u984c\u672c\u7684\u6539\u52d5\u3002")
  } else {
    fc <- ra_form_changes_table(app)
    # 原文：題本說明
    table(fc[setdiff(names(fc), "\u984c\u672c\u8aaa\u660e")])
    for (f in unique(app$change_details$formID)) {
      d <- app$change_details[app$change_details$formID == f, , drop = FALSE]
      # 原文：　
      add(paste0(f, "\u3000", d$form_label[1]), KIT_DOCX_HEADING)
      for (i in seq_len(nrow(d))) {
        # 原文：題本說明 |  題本說明：
        lead <- if (d$what[i] == "\u984c\u672c\u8aaa\u660e") paste0(d$year[i], " \u984c\u672c\u8aaa\u660e\uff1a") else
          # 原文：：
          paste0(d$kind_label[i], "\uff1a")
        # 原文：・ | 。理由：
        add(paste0("\u30fb", lead, d$text[i], if (nzchar(d$reason[i])) paste0("\u3002\u7406\u7531\uff1a", d$reason[i])))
      }
    }
  }

  # 原文：二、各波次信效度
  add("\u4e8c\u3001\u5404\u6ce2\u6b21\u4fe1\u6548\u5ea6", KIT_DOCX_HEADING)
  if (nrow(app$reliability)) {
    rt <- ra_reliability_table(app)
    # 原文：分析代號
    table(rt[setdiff(names(rt), "\u5206\u6790\u4ee3\u865f")])
  } else {
    # 原文：沒有資料。
    add(app$notes$reliability %||% "\u6c92\u6709\u8cc7\u6599\u3002")
  }

  # 原文：三、施測一覽
  add("\u4e09\u3001\u65bd\u6e2c\u4e00\u89bd", KIT_DOCX_HEADING)
  if (nrow(app$admins)) {
    at <- ra_admins_table(app)
    # 原文：施測 | 名稱 | 狀態 | 資料檔 | 題數
    table(at[c("\u65bd\u6e2c", "\u540d\u7a31", "\u72c0\u614b", "\u8cc7\u6599\u6a94", "\u984c\u6578")])
  } else {
    # 原文：這段期間沒有施測。
    add("\u9019\u6bb5\u671f\u9593\u6c92\u6709\u65bd\u6e2c\u3002")
  }
  print(doc, target = path)
  invisible(path)
}

# 施測頁用的選單和下載鈕 ----

#' Year/administration selector and download buttons for the report appendix
#'
#' Call inside a `renderUI()` of a page server (for example the 施測 page): it builds
#' the inputs `ra_year` and `ra_admins` in `session`'s namespace and registers the two
#' downloads (Word, Excel) with `kitDesign::kit_download_button()`. The downloads read
#' the current selection when clicked.
#'
#' @param session The (module) session.
#' @param kit A kit, or a reactive returning one.
#' @return A `shiny.tag`.
#' @export
report_appendix_ui <- function(session, kit) {
  ns <- session$ns
  kit_now <- function() if (shiny::is.reactive(kit)) shiny::isolate(kit()) else kit
  k <- kit_now()
  years <- sort(unique(ra_admin_year(k)), decreasing = TRUE)
  years <- years[nzchar(years)]
  this_year <- format(Sys.Date(), "%Y")
  ov <- admin_summary(k)
  current <- function() {
    yr <- shiny::isolate(session$input[["ra_year"]]) %||% ""
    ad <- shiny::isolate(session$input[["ra_admins"]]) %||% character(0)
    report_appendix(kit_now(), year = if (nzchar(yr)) yr, admins = if (length(ad)) ad)
  }
  day <- format(Sys.Date(), "%Y%m%d")
  cls <- kitDesign::kit_btn_class("secondary")
  shiny::div(
    class = "kit-report-appendix",
    shiny::p(shiny::tags$strong(RA_TITLE)),
    # 原文：選一個年度（看那一年開始調查的施測），或直接選幾次施測；下載今年題本改了什麼、各波次信效度、施測一覽。
    shiny::p(class = "kit-hint", "\u9078\u4e00\u500b\u5e74\u5ea6\uff08\u770b\u90a3\u4e00\u5e74\u958b\u59cb\u8abf\u67e5\u7684\u65bd\u6e2c\uff09\uff0c\u6216\u76f4\u63a5\u9078\u5e7e\u6b21\u65bd\u6e2c\uff1b\u4e0b\u8f09\u4eca\u5e74\u984c\u672c\u6539\u4e86\u4ec0\u9ebc\u3001\u5404\u6ce2\u6b21\u4fe1\u6548\u5ea6\u3001\u65bd\u6e2c\u4e00\u89bd\u3002"),
    shiny::fluidRow(
      shiny::column(
        # 原文：年度
        4, shiny::selectInput(ns("ra_year"), "\u5e74\u5ea6",
                              # 原文：全部
                              choices = c(stats::setNames("", "\u5168\u90e8"), stats::setNames(years, years)),
                              selected = if (this_year %in% years) this_year else if (length(years)) years[1] else "",
                              width = "100%")
      ),
      shiny::column(
        # 原文：或指定施測（選了就不看年度）
        8, shiny::selectizeInput(ns("ra_admins"), "\u6216\u6307\u5b9a\u65bd\u6e2c\uff08\u9078\u4e86\u5c31\u4e0d\u770b\u5e74\u5ea6\uff09",
                                 choices = stats::setNames(ov$adminID, ov$label), multiple = TRUE,
                                 width = "100%",
                                 # 原文：搜尋施測…
                                 options = list(placeholder = "\u641c\u5c0b\u65bd\u6e2c\u2026"))
      )
    ),
    shiny::div(
      class = "kit-buttons",
      kitDesign::kit_download_button(
        # 原文：下載報告附表（Word）
        session, "dl_ra_docx", "\u4e0b\u8f09\u5831\u544a\u9644\u8868\uff08Word\uff09",
        # 原文：報告附表_
        paste0("\u5831\u544a\u9644\u8868_", day, ".docx"),
        function(file) report_appendix_docx(current(), file),
        class = cls
      ),
      kitDesign::kit_download_button(
        # 原文：下載報告附表（Excel）
        session, "dl_ra_xlsx", "\u4e0b\u8f09\u5831\u544a\u9644\u8868\uff08Excel\uff09",
        # 原文：報告附表_
        paste0("\u5831\u544a\u9644\u8868_", day, ".xlsx"),
        function(file) writexl::write_xlsx(report_appendix_workbook(current()), file),
        class = cls
      )
    )
  )
}
