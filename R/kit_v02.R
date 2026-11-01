# R/kit_v02.R — 轉換層：把 0.3 題庫（kit，17 張長表）轉成 0.2 的寬表題庫
#
# 0.3 的前台先沿用 0.2 的查詢與編修函式（query.R、dims.R、edit.R…），
# 它們吃的是 load_bank() 的五張表；bank_to_v02() 從 kit 算出同樣形狀的表：
#   itemBank  一列一個題目版本；value 由 value_codes 組回文字，noResponse 由資料檔的遺漏代碼方案組成
#   itemWave  欄是施測代碼（M03M036…），1 = 這個版本出現在那次施測的資料檔
#   itemDim   欄是題組（去掉問卷前綴的 groupID），1 = 這一題在任何題本屬於這個題組
#   dimMeta   題組名冊
# 題庫（domain）↔ 問卷：parent = P、child_adolescent = C、childcare_teacher = T。

KIT_DOMAIN_INSTRUMENT <- c(parent = "P", child_adolescent = "C", childcare_teacher = "T")

#' 去掉 "P." 這種問卷前綴
#' @noRd
kit_strip_prefix <- function(x) sub("^[A-Z]\\.", "", x)

#' 一個 valueSetID 組回 0.2 的選項文字："1 很少;\n2 有時;"
#' @noRd
kit_value_text <- function(vc) {
  vc <- vc[order(vc$valueSetID, as.integer(vc$order)), , drop = FALSE]
  txt <- tapply(paste0(vc$code, " ", vc$label, ";"), vc$valueSetID,
                function(x) paste(x, collapse = "\n"))
  txt
}

#' Convert the 0.3 kit to a 0.2-style bank for one questionnaire
#'
#' The 0.3 app keeps the 0.2 query and edit functions; this builds their input
#' from the long-format kit.
#'
#' @param kit A kit, from [read_kit()].
#' @param domain `"parent"`, `"child_adolescent"` or `"childcare_teacher"`.
#' @return A list like [load_bank()]: `itemBank`, `itemDim`, `itemWave`, `dimMeta`,
#'   `dimReliability`. Wave columns are administration IDs (e.g. `"M03M036"`).
#' @export
bank_to_v02 <- function(kit, domain = "parent") {
  domain <- match.arg(domain, names(KIT_DOMAIN_INSTRUMENT))
  ins <- KIT_DOMAIN_INSTRUMENT[[domain]]
  items <- kit$items[kit$items$instrument == ins, , drop = FALSE]
  iv <- kit$item_versions[kit$item_versions$item_uid %in% items$item_uid, , drop = FALSE]
  iv <- iv[order(iv$item_uid, as.integer(iv$itemVer)), , drop = FALSE]
  vkey <- paste(iv$item_uid, iv$itemVer)

  # 這個版本出現在哪些資料檔
  dv <- kit$dataset_vars[kit$dataset_vars$item_uid %in% items$item_uid &
                           nzchar(kit$dataset_vars$itemVer), , drop = FALSE]
  ds <- kit$datasets[match(dv$datasetID, kit$datasets$datasetID), , drop = FALSE]
  dv$adminID <- ds$adminID
  dv$respondent <- ds$respondent
  dv$scheme <- ds$missingScheme
  dkey <- paste(dv$item_uid, dv$itemVer)

  # 停用：整題停用，或後面還有新版本
  it <- items[match(iv$item_uid, items$item_uid), , drop = FALSE]
  last_ver <- tapply(as.integer(iv$itemVer), iv$item_uid, max)
  deprecated <- it$deprecated == "1" | as.integer(iv$itemVer) < last_ver[iv$item_uid]

  # 反向計分：任何題本裡的成員標了 reverse
  gm <- kit$group_members[kit$group_members$item_uid %in% items$item_uid, , drop = FALSE]
  rev_uid <- unique(gm$item_uid[gm$reverse == "1"])

  vtxt <- kit_value_text(kit$value_codes)
  value <- unname(vtxt[iv$valueSetID])
  value[is.na(value)] <- ""

  join_by_ver <- function(x) {
    out <- tapply(x, dkey, function(v) paste(sort(unique(v[nzchar(v)])), collapse = ","))
    r <- unname(out[vkey])
    r[is.na(r)] <- ""
    r
  }
  ms <- kit$missing_schemes
  scheme_txt <- vapply(split(ms, ms$scheme), function(m) {
    m <- m[order(as.integer(m$code)), , drop = FALSE]
    paste(paste0(m$label, ":", m$code), collapse = "; ")
  }, character(1))
  sch <- join_by_ver(dv$scheme)
  # 兩套方案都出現時，用新的那套
  sch_last <- vapply(strsplit(sch, ","), function(s) if (length(s)) s[length(s)] else "", character(1))
  noresp <- unname(scheme_txt[sch_last])
  noresp[is.na(noresp)] <- ""

  item_bank <- data.frame(
    itemID = kit_strip_prefix(iv$item_uid), itemVer = iv$itemVer,
    deprecated = ifelse(deprecated, "1", "0"),
    stemID = iv$stem, block = it$block, itemType = iv$itemType,
    respondent = join_by_ver(dv$respondent),
    reverseScored = ifelse(iv$item_uid %in% rev_uid, "1", "0"),
    label = iv$label, value = value, noResponse = noresp, note = iv$note,
    stringsAsFactors = FALSE)

  # itemWave：版本 × 施測
  adm <- kit$admins[kit$admins$adminID %in% dv$adminID, , drop = FALSE]
  adm <- adm[order(as.integer(adm$ageMonth), adm$cohortID), , drop = FALSE]
  waves <- adm$adminID
  wave_m <- matrix(0L, nrow(iv), length(waves), dimnames = list(NULL, waves))
  hit <- unique(data.frame(r = match(dkey, vkey), c = match(dv$adminID, waves)))
  hit <- hit[!is.na(hit$r) & !is.na(hit$c), , drop = FALSE]
  wave_m[cbind(hit$r, hit$c)] <- 1L
  item_wave <- data.frame(itemID = item_bank$itemID, itemVer = iv$itemVer,
                          as.data.frame(wave_m, stringsAsFactors = FALSE),
                          check.names = FALSE, stringsAsFactors = FALSE)

  # itemDim：題目 × 題組（題組成員以題號為單位，所有版本都算）
  grp <- kit$groups[startsWith(kit$groups$groupID, paste0(ins, ".")), , drop = FALSE]
  dims <- kit_strip_prefix(grp$groupID)
  dim_m <- matrix(0L, nrow(iv), length(dims), dimnames = list(NULL, dims))
  gm <- gm[gm$groupID %in% grp$groupID, , drop = FALSE]
  r <- which(iv$item_uid %in% gm$item_uid)
  pairs <- merge(data.frame(r = r, item_uid = iv$item_uid[r]),
                 unique(gm[, c("item_uid", "groupID")]), by = "item_uid")
  dim_m[cbind(pairs$r, match(kit_strip_prefix(pairs$groupID), dims))] <- 1L
  item_dim <- data.frame(itemID = item_bank$itemID, itemVer = iv$itemVer,
                         as.data.frame(dim_m, stringsAsFactors = FALSE),
                         check.names = FALSE, stringsAsFactors = FALSE)

  dim_meta <- data.frame(dimID = dims, dimName = grp$label, alias = grp$alias,
                         reliability = "", note = grp$scoring_note, stringsAsFactors = FALSE)

  list(itemBank = item_bank, itemDim = item_dim, itemWave = item_wave, dimMeta = dim_meta,
       dimReliability = empty_dim_reliability())
}

# 同一個 R 程序只讀一次 kit、每個題庫只轉一次
.kit_cache <- new.env()

#' 內建 0.3 題庫（kit）轉成 0.2 形狀，給 app 用
#' @noRd
kit_bank <- function(domain = "parent") {
  if (is.null(.kit_cache$kit)) .kit_cache$kit <- read_kit()
  if (is.null(.kit_cache[[domain]])) .kit_cache[[domain]] <- bank_to_v02(.kit_cache$kit, domain)
  .kit_cache[[domain]]
}

#' 內建 kit（同一個 R 程序只讀一次）
#' @noRd
kit_builtin <- function() {
  if (is.null(.kit_cache$kit)) .kit_cache$kit <- read_kit()
  .kit_cache$kit
}

#' 內建 kit 的題庫版本（inst/extdata/kit/BANK_VERSION 的第一行）
#' @noRd
kit_version <- function() {
  f <- file.path(kit_dir(), "BANK_VERSION")
  if (!nzchar(kit_dir()) || !file.exists(f)) return(NA_character_)
  trimws(readLines(f, n = 1L, warn = FALSE, encoding = "UTF-8"))
}

#' 施測代碼的舊名稱（0.2 波次欄、釋出包代碼、釋出編號），給選單搜尋用
#'
#' 回傳和 ids 一樣長的字串，例如 "M036小、M03W36、D00229"；不是施測代碼或讀不到 kit 時是 ""。
#' @noRd
admin_alias_text <- function(ids) {
  a <- tryCatch(kit_builtin()$aliases, error = function(e) NULL)
  out <- rep("", length(ids))
  if (is.null(a) || !nrow(a)) return(out)
  a <- a[a$target_type == "admin" &
           a$kind %in% c("v02_wave", "release_package", "release_id", "spoken"), , drop = FALSE]
  txt <- tapply(a$alias, a$target_id, function(x) paste(unique(x), collapse = "、"))
  hit <- ids %in% names(txt)
  out[hit] <- unname(txt[ids[hit]])
  out
}
