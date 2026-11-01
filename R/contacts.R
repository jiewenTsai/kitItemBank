# R/contacts.R — 聯絡人名冊（contacts 表，定義在 R/schema_ext.R）：請教單要問誰
#
# contacts_for() 照請教的範圍找負責人：題組 → 題本 → 施測 → 問卷 → 全部，越具體的越前面。

#' Who to ask about an instrument, form, group or administration
#'
#' Looks up the `contacts` table for people responsible for any of the given scopes.
#' The most specific matches come first (group, then form, administration, instrument,
#' and finally people responsible for everything). Inactive rows (`active = "0"`) are
#' left out.
#'
#' @param kit A kit, from [read_kit()].
#' @param instrument,formID,groupID,adminID Scopes of the question; any may be `NULL`.
#'   When `formID` is given and `instrument` is not, the form's instrument is used too.
#' @return The matching `contacts` rows (one per person and scope), with `match` added
#'   (the scope type that matched).
#' @examples
#' contacts_for(read_kit(), instrument = "P")
#' @export
contacts_for <- function(kit, instrument = NULL, formID = NULL, groupID = NULL,
                         adminID = NULL) {
  ct <- kit$contacts %||% empty_kit_table("contacts")
  ct <- ct[ct$active != "0", , drop = FALSE]
  if (is.null(instrument) && !is.null(formID)) {
    instrument <- kit$forms$instrument[kit$forms$formID %in% formID]
  }
  want <- list(group = groupID, form = formID, admin = adminID, instrument = instrument)
  rank <- rep(NA_integer_, nrow(ct))
  for (i in seq_along(want)) {
    hit <- is.na(rank) & ct$scope_type == names(want)[i] & ct$scope_id %in% want[[i]]
    rank[hit] <- i
  }
  rank[is.na(rank) & ct$scope_type == "all"] <- length(want) + 1L
  out <- ct[!is.na(rank), , drop = FALSE]
  out$match <- out$scope_type
  out <- out[order(rank[!is.na(rank)], out$contactID), , drop = FALSE]
  rownames(out) <- NULL
  out
}
