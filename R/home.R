# R/home.R — 「待辦事項」首頁的計算：要注意的事分給誰處理、連到哪一頁
#
# 畫面在 R/page_home.R。這裡只有純函式，方便測試。
# 要注意的清單來自 audit_kit()；首頁只用題庫本身（不給 .sav 和 0.2 遺漏代碼），
# 所以 HOME_NEEDS_MAINTAINER 列的兩種檢查要維護者在自己電腦上跑。

# 誰能處理：self = 自己能改；teacher = 要問老師；stats = 要問統計人員；
# tech = 要請技術人員改（app 裡沒有頁面能改的欄位）
# 原文：自己能改 | 要問老師 | 要問統計人員
HOME_OWNERS <- c(self = "\u81ea\u5df1\u80fd\u6539", teacher = "\u8981\u554f\u8001\u5e2b", stats = "\u8981\u554f\u7d71\u8a08\u4eba\u54e1",
                 # 原文：要請技術人員改
                 tech = "\u8981\u8acb\u6280\u8853\u4eba\u54e1\u6539")

# 每一種「要注意」由誰處理（AUDIT_KIT_CHECKS 的每一種都要有，測試會檢查）
#   self：文字、分類欄位、空題組，看題庫就能改
#   teacher：要懂題目內容才能判斷（題目是不是同一題、題組該怎麼分）
#   stats：要看資料檔（.sav、遺漏代碼、哪些資料檔問了哪些題）
#   tech：app 沒有頁面能改（題目的 role、module），要技術人員直接改 kit 的表
HOME_AUDIT_OWNER <- data.frame(
  check = c("coverage_uid_blank", "coverage_sav", "form_year_diff", "renamed_no_group",
            "missing_code_scheme", "group_only_deprecated", "group_single_item", "group_empty",
            "group_nonleaf_member", "text_simplified", "text_brackets", "text_space",
            "text_newline", "internal_admins", "role_module_blank"),
  owner = c("stats", "stats", "stats", "teacher",
            "stats", "teacher", "teacher", "self",
            "teacher", "self", "self", "self",
            "self", "stats", "tech"),
  stringsAsFactors = FALSE
)

# 首頁不跑的檢查：要維護者手上的資料
HOME_NEEDS_MAINTAINER <- data.frame(
  check = c("coverage_sav", "missing_code_scheme"),
  # 原文：.sav 表頭（import_sav_dir()） | 0.2 題庫的遺漏代碼（import_v02()$noresponse_codes）
  needs = c(".sav \u8868\u982d\uff08import_sav_dir()\uff09", "0.2 \u984c\u5eab\u7684\u907a\u6f0f\u4ee3\u78bc\uff08import_v02()$noresponse_codes\uff09"),
  stringsAsFactors = FALSE
)

#' The "to look at" list for the home page, with who can resolve each finding
#'
#' Runs [audit_kit()] on the kit alone (no `.sav` headers, no 0.2 non-response codes, so the
#' checks in `HOME_NEEDS_MAINTAINER` are skipped unless you pass them) and adds who can
#' resolve each finding (`HOME_AUDIT_OWNER`) and which app page to open for it.
#'
#' @param kit A kit list, e.g. from [read_kit()].
#' @param sav,noresponse Passed to [audit_kit()]; leave `NULL` on the server.
#' @return The rows of [audit_kit()] plus `owner` (`"self"`, `"teacher"` or `"stats"`),
#'   `owner_label`, `check_label`, and the link: `tab` (`"search"`, `"dims"`, `"admin"` or
#'   `""` for none), `target` (itemID, dimension or adminID), `extra` (item_uid or datasetID).
#' @examples
#' home_audit(empty_kit())
#' @export
home_audit <- function(kit, sav = NULL, noresponse = NULL) {
  a <- audit_kit(kit, sav = sav, noresponse = noresponse)
  a$owner <- HOME_AUDIT_OWNER$owner[match(a$check, HOME_AUDIT_OWNER$check)]
  a$owner[is.na(a$owner)] <- "self"
  a$owner_label <- unname(HOME_OWNERS[a$owner])
  a$check_label <- AUDIT_KIT_CHECKS$label[match(a$check, AUDIT_KIT_CHECKS$check)]
  a$check_label[is.na(a$check_label)] <- a$check[is.na(a$check_label)]
  ln <- home_links(kit, a$table, a$key)
  cbind(a, ln)
}

#' 一筆「要注意」連到哪一頁：tab、target、extra
#' @noRd
home_links <- function(kit, table, key) {
  n <- length(key)
  tab <- rep("", n); target <- rep("", n); extra <- rep("", n)
  first <- sub("\\|.*$", "", key)
  seg <- function(i) vapply(strsplit(key, "|", fixed = TRUE), function(p) {
    if (length(p) >= i) p[[i]] else ""
  }, "")
  item <- function(x, uid) { tab[x] <<- "search"; target[x] <<- kit_strip_prefix(uid); extra[x] <<- uid }
  ds_admin <- function(d) {
    a <- kit$datasets$adminID[match(d, kit$datasets$datasetID)]
    a[is.na(a)] <- ""
    a
  }

  x <- table %in% c("items", "item_versions")
  item(x, first[x])
  # 題目關係：看新的那一題（新題的歷史裡會列出舊題）
  x <- table == "item_relations"
  item(x, seg(3)[x])
  x <- table == "value_codes"
  if (any(x)) {
    iv <- kit$item_versions
    uid <- iv$item_uid[match(first[x], iv$valueSetID)]
    ok <- !is.na(uid)
    xx <- which(x)[ok]
    item(xx, uid[ok])
  }
  x <- table %in% c("group_members", "groups")
  g <- ifelse(table == "group_members", seg(2), first)
  tab[x] <- "dims"; target[x] <- kit_strip_prefix(g[x])
  x <- table %in% c("datasets", "dataset_vars")
  a <- ds_admin(first)
  x <- x & nzchar(a)
  tab[x] <- "admin"; target[x] <- a[x]; extra[x] <- first[x]
  data.frame(tab = tab, target = target, extra = extra, stringsAsFactors = FALSE)
}

#' Count home-page findings per owner and check
#'
#' @param ha The result of [home_audit()].
#' @return A data frame with `owner`, `owner_label`, `check`, `check_label`, `n`, ordered by
#'   owner (`HOME_OWNERS`) and then by `AUDIT_KIT_CHECKS`.
#' @export
home_audit_summary <- function(ha) {
  if (!nrow(ha)) {
    return(data.frame(owner = character(0), owner_label = character(0), check = character(0),
                      check_label = character(0), n = integer(0), stringsAsFactors = FALSE))
  }
  k <- paste(ha$owner, ha$check, sep = "\r")
  u <- !duplicated(k)
  out <- data.frame(owner = ha$owner[u], owner_label = ha$owner_label[u], check = ha$check[u],
                    check_label = ha$check_label[u], n = as.integer(table(k)[k[u]]),
                    stringsAsFactors = FALSE)
  out <- out[order(match(out$owner, names(HOME_OWNERS)),
                   match(out$check, AUDIT_KIT_CHECKS$check)), , drop = FALSE]
  rownames(out) <- NULL
  out
}
