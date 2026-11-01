# R/group_lineage.R — 題組沿革（group_lineage 表，定義在 R/schema_ext.R）
#
#   import_group_lineage()  對照表找到的候選（data-raw/candidates/cw_group_lineage.csv）→ 表的列
#   group_lineage_chain()   一個題組往前往後的整串沿革
#   group_lineage_ui()      構面頁可以放的一小塊：「題組沿革」清單（沒有沿革就是 NULL）

# 沿革種類的白話
# 原文：延續 | 改名 | 拆開 | 合併
GROUP_LINEAGE_ZH <- c(continued = "\u5ef6\u7e8c", renamed = "\u6539\u540d", split = "\u62c6\u958b", merged = "\u5408\u4f75")

#' Turn group-lineage candidates into `group_lineage` rows
#'
#' The kind of each link is worked out from the candidates (per pair of forms): one group
#' going to several groups is `split`; several groups going to one group is `merged`;
#' otherwise the same group on both sides is `continued` and different groups `renamed`. `effective_admin` is the first
#' administration (by survey start) whose data files use the new form in the candidate's
#' year; `sourceID` is the crosswalk of that administration's cohort when the kit has one.
#'
#' @param candidates_df Candidates with columns `from_group`, `to_group`, `from_formID`,
#'   `formID` (the later form), and optionally `year`, `n_items`, `varnames`,
#'   `confidence`, `basis` — e.g. `data-raw/candidates/cw_group_lineage.csv`.
#' @param kit A kit, from [read_kit()].
#' @param decided_by,decided_on Who accepted the candidates and when.
#' @return A data frame with the columns of the `group_lineage` table, one row per link.
#' @export
import_group_lineage <- function(candidates_df, kit, decided_by = "PI",
                                 decided_on = format(kit_today())) {
  cd <- as.data.frame(candidates_df, stringsAsFactors = FALSE)
  if (is.null(cd$to_formID)) cd$to_formID <- cd$formID
  for (cc in c("from_group", "to_group", "from_formID", "to_formID", "year", "varnames",
               "basis", "confidence")) {
    v <- if (is.null(cd[[cc]])) rep("", nrow(cd)) else as.character(cd[[cc]])
    v[is.na(v)] <- ""
    cd[[cc]] <- trimws(v)
  }
  cd <- cd[nzchar(cd$from_group) & nzchar(cd$to_group), , drop = FALSE]
  cd <- cd[!duplicated(cd[c("from_group", "from_formID", "to_group", "to_formID")]), ,
           drop = FALSE]
  if (nrow(cd) == 0L) return(empty_kit_table("group_lineage"))

  # 同一個轉換（從哪個題本到哪個題本）裡數：一個變多個 = 拆開；多個變一個 = 合併
  step <- paste(cd$from_formID, cd$to_formID)
  n_to <- stats::ave(seq_len(nrow(cd)), step, cd$from_group, FUN = length)
  n_from <- stats::ave(seq_len(nrow(cd)), step, cd$to_group, FUN = length)
  kind <- ifelse(n_to > 1L, "split", ifelse(n_from > 1L, "merged",
                 ifelse(cd$from_group == cd$to_group, "continued", "renamed")))

  eff <- vapply(seq_len(nrow(cd)), function(i) {
    lineage_first_admin(kit, cd$to_formID[i], cd$year[i])
  }, character(1))

  src <- kit$sources[kit$sources$kind == "crosswalk", , drop = FALSE]
  sid <- vapply(eff, function(a) {
    if (!nzchar(a)) return("")
    hit <- src$sourceID[startsWith(src$version, substr(a, 1L, 3L))]
    if (length(hit)) hit[1] else ""
  }, character(1), USE.NAMES = FALSE)

  reason <- cd$basis
  extra <- ifelse(nzchar(cd$varnames), paste0("varnames: ", cd$varnames), "")
  reason <- ifelse(nzchar(reason) & nzchar(extra), paste0(reason, "; ", extra),
                   paste0(reason, extra))

  out <- data.frame(from_group = cd$from_group, from_formID = cd$from_formID,
                    to_group = cd$to_group, to_formID = cd$to_formID, kind = kind,
                    effective_admin = eff, reason = reason, decided_by = decided_by,
                    decided_on = decided_on, sourceID = sid, stringsAsFactors = FALSE)
  conform_kit_table(out, "group_lineage")
}

#' 第一個用這個題本的施測（照調查開始日）；year 有填就只看那一年開始的
#' @noRd
lineage_first_admin <- function(kit, formID, year = "") {
  a <- unique(kit$datasets$adminID[kit$datasets$formID %in% formID])
  ad <- kit$admins[kit$admins$adminID %in% a, , drop = FALSE]
  if (nzchar(year %||% "")) {
    y <- ad[substr(ad$surveyFrom, 1L, 4L) == year, , drop = FALSE]
    if (nrow(y)) ad <- y
  }
  ad <- ad[nzchar(ad$surveyFrom), , drop = FALSE]
  if (nrow(ad) == 0L) return("")
  ad$adminID[order(ad$surveyFrom, ad$adminID)][1]
}

#' 題組 ID：groupID、題組名稱或別名都可以
#' @noRd
lineage_group_id <- function(kit, x) {
  g <- kit$groups
  if (x %in% g$groupID) return(x)
  hit <- g$groupID[g$label == x | g$alias == x]
  if (length(hit)) return(hit)
  al <- kit$aliases[kit$aliases$target_type == "group" & kit$aliases$alias == x, , drop = FALSE]
  if (nrow(al)) return(unique(al$target_id))
  x
}

#' The lineage of one group
#'
#' Follows `group_lineage` forwards and backwards from the group (any kind of link), so
#' the result is the whole chain the group belongs to.
#'
#' @param kit A kit, from [read_kit()].
#' @param groupID A groupID; a group label or alias also works.
#' @return A data frame, one row per link, sorted by when the link took effect:
#'   `from_group`, `from_label`, `from_formID`, `to_group`, `to_label`, `to_formID`,
#'   `kind`, `kind_label` (in Chinese), `effective_admin`, `reason`, `decided_by`,
#'   `decided_on`, `sourceID`. Zero rows when the group has no lineage.
#' @export
group_lineage_chain <- function(kit, groupID) {
  gl <- kit$group_lineage %||% empty_kit_table("group_lineage")
  ids <- lineage_group_id(kit, groupID)
  repeat {
    more <- setdiff(unique(c(gl$to_group[gl$from_group %in% ids],
                             gl$from_group[gl$to_group %in% ids])), ids)
    if (!length(more)) break
    ids <- c(ids, more)
  }
  ch <- gl[gl$from_group %in% ids | gl$to_group %in% ids, , drop = FALSE]
  lab <- function(g) {
    l <- kit$groups$label[match(g, kit$groups$groupID)]
    ifelse(is.na(l) | !nzchar(l), g, l)
  }
  ad <- kit$admins
  when <- ad$surveyFrom[match(ch$effective_admin, ad$adminID)]
  when[is.na(when)] <- ""
  ch <- ch[order(!nzchar(when), when, ch$from_formID, ch$to_formID), , drop = FALSE]
  kl <- unname(GROUP_LINEAGE_ZH[ch$kind])
  out <- data.frame(from_group = ch$from_group, from_label = lab(ch$from_group),
                    from_formID = ch$from_formID, to_group = ch$to_group,
                    to_label = lab(ch$to_group), to_formID = ch$to_formID, kind = ch$kind,
                    kind_label = ifelse(is.na(kl), ch$kind, kl),
                    effective_admin = ch$effective_admin, reason = ch$reason,
                    decided_by = ch$decided_by, decided_on = ch$decided_on,
                    sourceID = ch$sourceID, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

#' A small "group lineage" block for a group view
#'
#' Meant for the dimensions page under a selected dimension, e.g.
#' `group_lineage_ui(read_kit(), dims_label(bank, d))`. Returns `NULL` when the group has
#' no lineage, so it can be dropped into a `tagList()` unconditionally.
#'
#' @param kit A kit, from [read_kit()].
#' @param groupID A groupID; a group label or alias also works.
#' @param open Whether the block starts open.
#' @return A `kitDesign::kit_detail()` block, or `NULL`.
#' @export
group_lineage_ui <- function(kit, groupID, open = FALSE) {
  ch <- group_lineage_chain(kit, groupID)
  if (nrow(ch) == 0L) return(NULL)
  items <- lapply(seq_len(nrow(ch)), function(i) {
    r <- ch[i, , drop = FALSE]
    shiny::tags$li(
      # 原文：「 | 」（ | ）→
      paste0("\u300c", r$from_label, "\u300d\uff08", r$from_formID, "\uff09\u2192",
             # 原文：→「 | 」（ | ）
             r$kind_label, "\u2192\u300c", r$to_label, "\u300d\uff08", r$to_formID, "\uff09",
             # 原文：，從  |  開始
             if (nzchar(r$effective_admin)) paste0("\uff0c\u5f9e ", r$effective_admin, " \u958b\u59cb") else ""),
      if (nzchar(r$reason)) shiny::span(class = "kit-hint", paste0(" ", r$reason))
    )
  })
  kitDesign::kit_detail(
    # 原文：題組沿革（ |  筆）
    paste0("\u984c\u7d44\u6cbf\u9769\uff08", nrow(ch), " \u7b46\uff09"),
    shiny::tags$ul(items),
    # 原文：題組在不同題本之間的延續、改名、拆開或合併；資料在 group_lineage 表。
    shiny::p(class = "kit-hint", "\u984c\u7d44\u5728\u4e0d\u540c\u984c\u672c\u4e4b\u9593\u7684\u5ef6\u7e8c\u3001\u6539\u540d\u3001\u62c6\u958b\u6216\u5408\u4f75\uff1b\u8cc7\u6599\u5728 group_lineage \u8868\u3002"),
    open = open
  )
}
