# R/apply_candidates.R — 把對照表的候選放進 kit：題組（名冊、樹、成員）和題目關係提示
#
# 主持人 2026-11-02 同意全部候選（高、中、低信心）。已決定事項優先：
# D4 拿掉的段落名稱題組（drop）不會被候選加回來；那些題組底下的題組變成根。

#' Apply crosswalk group candidates to a kit
#'
#' Adds the groups, tree edges and members proposed by [import_crosswalk()]
#' (`group_paths` and `member_hints`). Groups listed in `drop` are never added back;
#' an edge whose parent is dropped becomes a root. Members whose item or form is not in
#' the kit are skipped.
#'
#' @param kit A kit.
#' @param paths The `group_paths` candidates (formID, groupID, parentID, label, …).
#' @param hints The `member_hints` candidates (formID, groupID, item_uid, reverse, …).
#' @param drop groupIDs that must not be added.
#' @return The kit with new rows in `groups`, `group_tree` and `group_members`.
#' @export
apply_group_candidates <- function(kit, paths, hints, drop = character(0)) {
  chr <- function(v) { v <- as.character(v); v[is.na(v)] <- ""; v }
  paths[] <- lapply(paths, chr)
  hints[] <- lapply(hints, chr)
  forms <- kit$forms$formID
  paths <- paths[!paths$groupID %in% drop & paths$formID %in% forms, , drop = FALSE]
  paths$parentID[paths$parentID %in% drop] <- ""
  hints <- hints[!hints$groupID %in% drop & hints$formID %in% forms &
                   hints$item_uid %in% kit$items$item_uid, , drop = FALSE]

  # 題組名冊：候選提到、kit 還沒有的題組
  lab <- c(stats::setNames(paths$label, paths$groupID))
  new_g <- setdiff(unique(c(paths$groupID, paths$parentID[nzchar(paths$parentID)],
                            hints$groupID)), kit$groups$groupID)
  if (length(new_g)) {
    label <- unname(lab[new_g])
    miss <- is.na(label) | !nzchar(label)
    label[miss] <- sub("^[A-Z]\\.", "", new_g[miss])
    kit$groups <- rbind(kit$groups, conform_kit_table(data.frame(
      groupID = new_g, label = label, stringsAsFactors = FALSE), "groups"))
  }

  # 題組樹：同一個題本、同一個題組，有上層就不再留一列空白上層
  gt <- rbind(kit$group_tree, conform_kit_table(
    paths[, c("formID", "groupID", "parentID")], "group_tree"))
  gt <- gt[gt$groupID != gt$parentID, , drop = FALSE]
  gt <- gt[!duplicated(kit_key_str(gt, KIT_SCHEMA$group_tree$key)), , drop = FALSE]
  has_parent <- paste(gt$formID, gt$groupID)[nzchar(gt$parentID)]
  gt <- gt[nzchar(gt$parentID) | !paste(gt$formID, gt$groupID) %in% has_parent, , drop = FALSE]
  # 成員的題組也要在這個題本的樹上
  need <- unique(hints[, c("formID", "groupID")])
  need <- need[!paste(need$formID, need$groupID) %in% paste(gt$formID, gt$groupID), , drop = FALSE]
  if (nrow(need)) gt <- rbind(gt, conform_kit_table(need, "group_tree"))
  kit$group_tree <- gt

  # 成員
  gm <- rbind(kit$group_members, conform_kit_table(data.frame(
    formID = hints$formID, groupID = hints$groupID, item_uid = hints$item_uid,
    scheme = "official", reverse = hints$reverse, stringsAsFactors = FALSE), "group_members"))
  kit$group_members <- gm[!duplicated(kit_key_str(gm, KIT_SCHEMA$group_members$key)), ,
                          drop = FALSE]
  kit
}

#' Apply crosswalk relation hints to a kit
#'
#' Adds the item relations found in crosswalk notes ([import_crosswalk()] `relation_hints`)
#' when both item versions exist, the two ends differ, and the pair has no relation yet.
#'
#' @param kit A kit.
#' @param hints The `relation_hints` candidates.
#' @param decided_by,decided_on Who accepted them, and when.
#' @return The kit with new rows in `item_relations`.
#' @export
apply_relation_hints <- function(kit, hints, decided_by = "", decided_on = "") {
  hints[] <- lapply(hints, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })
  ver <- paste(kit$item_versions$item_uid, kit$item_versions$itemVer)
  ok <- paste(hints$from_uid, hints$from_ver) %in% ver &
    paste(hints$to_uid, hints$to_ver) %in% ver &
    paste(hints$from_uid, hints$from_ver) != paste(hints$to_uid, hints$to_ver)
  r <- conform_kit_table(hints[ok, , drop = FALSE], "item_relations")
  r$decided_by <- decided_by
  r$decided_on <- decided_on
  ir <- rbind(kit$item_relations, r)
  kit$item_relations <- ir[!duplicated(kit_key_str(ir, KIT_SCHEMA$item_relations$key)), ,
                           drop = FALSE]
  kit
}
