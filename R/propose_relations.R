# R/propose_relations.R — 1.5：題目關係候選（X／Xn 換題號、0.2 版本異動），以及 .sav 有、題庫沒有的新題號
#
# 產出都是候選，要人工確認：每列帶 confidence（high／medium／low）和 basis（根據）。
# 規則：
# - 換題號（renamed）：同一工具裡，變項 Xn 存在、X 也存在（不分大小寫）。
#   from = X 在 Xn 首次出現前最後一次施測用的版本；to = Xn 首次出現的版本。
#   effective_admin = Xn 最早出現的施測（依出生起日 + 月齡推算的時間排序）。
# - 改版（revised）：同一 item_uid 相鄰的兩個現存版本 (a, b)。理由取 0.2 note 裡
#   「va→vb版本異動: ...」中 a <= 起 < 迄 <= b 的段落（被合併掉的中間版本也算進來）。
# - similarity：標籤去掉空白、標點、符號和 .sav 的題號前綴後比較；
#   完全相同 identical；編輯距離比 <= 0.2 high；<= 0.5 medium；其餘 low；任一邊空白則留空。
# - 值組相同：非遺漏代碼的集合一樣，且兩邊都有標籤時標籤（正規化後）一樣。
# - comparable：identical／high 且值組相同 → yes；low → no；其餘 partial。
# - meaning_changed（只給 revised、renamed）：note 提到「題意」、非遺漏代碼改變、
#   或 similarity 為 low → 1；否則 0。
# - confidence：
#   renamed：similarity identical／high、值組相同、X 和 Xn 沒有出現在同一個資料檔 → high；
#            similarity low 或空白 → low；其餘 medium。
#   revised：有 note 且 similarity identical／high → high；
#            有 note 但 medium，或沒有 note 但 identical／high → medium；其餘 low。

#' 字串正規化：去空白、標點、符號、.sav 題號前綴（「四.」「1.」「6.1.」）
#' @noRd
rel_norm_label <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  # 原文：^[一二三四五六七八九十]+[.、]
  x <- gsub("^\\s*[\u4e00\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+[.\u3001]",
            "", x, perl = TRUE)
  x <- gsub("(^|(?<=[\\p{P}\\p{S}\\s]))[0-9]+(\\.[0-9]+)*\\.(?![0-9])", "", x, perl = TRUE)
  x <- gsub("^\\s*[0-9]+(\\.[0-9]+)*\\.(?![0-9])", "", x, perl = TRUE)
  x <- gsub("\\[[^]]*\\]", "", x, perl = TRUE)
  gsub("[\\s\\p{P}\\p{S}]+", "", x, perl = TRUE)
}

#' 兩組標籤的相似度等級
#' @noRd
rel_similarity <- function(a, b, stem_a = "", stem_b = "") {
  a <- rel_norm_label(a)
  b <- rel_norm_label(b)
  # 共用題幹只在一邊有時，先拿掉（兩邊的題幹都試）
  for (st in unique(rel_norm_label(c(stem_a, stem_b)))) {
    if (!nzchar(st)) next
    a <- ifelse(startsWith(a, st) & !startsWith(b, st), substring(a, nchar(st) + 1L), a)
    b <- ifelse(startsWith(b, st) & !startsWith(a, st), substring(b, nchar(st) + 1L), b)
  }
  out <- rep("", length(a))
  ok <- nzchar(a) & nzchar(b)
  if (!any(ok)) return(out)
  d <- mapply(function(p, q) utils::adist(p, q)[1, 1], a[ok], b[ok])
  r <- d / pmax(nchar(a[ok]), nchar(b[ok]))
  out[ok] <- ifelse(d == 0, "identical", ifelse(r <= 0.2, "high", ifelse(r <= 0.5, "medium", "low")))
  out
}

#' 是否遺漏代碼（missing_schemes 的代碼，或 777／888／999 這類三位以上同數字）
#' @noRd
rel_is_missing_code <- function(code, miss_codes) {
  code %in% miss_codes | grepl("^(7{3,}|8{3,}|9{3,})$", code)
}

#' 比較兩組 (code, label)：回傳 NA（缺一邊）、TRUE、FALSE；attr "codes_same"
#' @noRd
rel_values_match <- function(a, b, miss_codes) {
  if (is.null(a) || is.null(b) || !nrow(a) || !nrow(b)) return(NA)
  a <- a[!rel_is_missing_code(a$code, miss_codes), , drop = FALSE]
  b <- b[!rel_is_missing_code(b$code, miss_codes), , drop = FALSE]
  codes_same <- setequal(a$code, b$code)
  if (!codes_same) return(structure(FALSE, codes_same = FALSE))
  la <- rel_norm_label(a$label[match(b$code, a$code)])
  lb <- rel_norm_label(b$label)
  both <- nzchar(la) & nzchar(lb)
  structure(all(la[both] == lb[both]), codes_same = TRUE)
}

#' 施測的時間順序：出生起日（年*12+月）+ 月齡
#' @noRd
rel_admin_order <- function(kit) {
  ad <- kit$admins
  bf <- kit$cohorts$birth_from[match(ad$cohortID, kit$cohorts$cohortID)]
  ym <- suppressWarnings(as.integer(substr(bf, 1, 4)) * 12L + as.integer(substr(bf, 6, 7)))
  ym[is.na(ym)] <- 0L
  stats::setNames(ym + as.integer(ad$ageMonth), ad$adminID)
}

#' 讀 .sav 表頭標籤：回傳 datasetID、varname、label
#' @noRd
rel_read_meta_labels <- function(meta_dir, map, ids) {
  empty <- data.frame(datasetID = character(0), varname = character(0), label = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(meta_dir) || is.null(map)) return(empty)
  map <- map[map$datasetID %in% ids, , drop = FALSE]
  out <- lapply(seq_len(nrow(map)), function(i) {
    f <- file.path(meta_dir, map$wave_dir[i], paste0(map$file[i], "_meta.csv"))
    if (!file.exists(f)) return(NULL)
    m <- utils::read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8",
                         colClasses = "character", na.strings = character(0))
    data.frame(datasetID = map$datasetID[i], varname = m$var, label = m$label,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(empty), out))
  out
}

#' Propose item relations and the items missing for renamed variables
#'
#' Step 1.5 of the 0.3 rebuild. Builds candidate `item_relations` rows from
#' (a) X / Xn variable renames within one instrument and (b) the 0.2
#' "v1->v2" version-change notes inside one item_uid, and creates the items
#' that appear in .sav headers but not in the bank (Xn whose X is known).
#' All rows are candidates for human review: `confidence` and `basis` say why.
#'
#' @param kit A kit (17 tables), e.g. from [read_kit()].
#' @param v02 Output of `import_v02()`; its `item_versions$note` is used when the kit's
#'   note is blank.
#' @param sav Output of `import_sav_dir()` (`value_sets`, `map`).
#' @param meta_dir Folder holding `<wave_dir>/<file>_meta.csv` (.sav headers), used for
#'   the labels of new items. `NULL` falls back to the predecessor's label.
#' @return A list: `relations` (item_relations columns + `confidence`, `basis`),
#'   `new_items` (items columns), `new_item_versions` (item_versions columns),
#'   `new_value_codes` (value_codes columns), `new_members` (group_members columns),
#'   `dataset_vars_fix` (datasetID, varname, item_uid, itemVer).
#' @export
propose_relations <- function(kit, v02 = NULL, sav = NULL, meta_dir = NULL) {
  it <- kit$items
  iv <- kit$item_versions
  vc <- kit$value_codes
  dv <- kit$dataset_vars
  ds <- kit$datasets
  miss_codes <- unique(kit$missing_schemes$code)
  aord <- rel_admin_order(kit)

  # 0.2 note：kit 的空白時用 v02 的
  note <- iv$note
  if (!is.null(v02$item_versions$note)) {
    alt <- v02$item_versions$note[match(paste(iv$item_uid, iv$itemVer),
                                        paste(v02$item_versions$item_uid,
                                              v02$item_versions$itemVer))]
    use <- !nzchar(note) & !is.na(alt)
    note[use] <- alt[use]
  }

  ver_key <- paste(iv$item_uid, iv$itemVer, sep = "|")
  vs_rows <- split(vc[, c("code", "label")], vc$valueSetID)
  vs_of <- function(uid, ver) {
    id <- iv$valueSetID[match(paste(uid, ver, sep = "|"), ver_key)]
    if (is.na(id) || !nzchar(id)) NULL else vs_rows[[id]]
  }
  label_of <- function(uid, ver) {
    iv$label[match(paste(uid, ver, sep = "|"), ver_key)]
  }
  stem_of <- function(uid, ver) {
    s <- iv$stem[match(paste(uid, ver, sep = "|"), ver_key)]
    if (is.na(s)) "" else s
  }

  dv$adminID <- ds$adminID[match(dv$datasetID, ds$datasetID)]
  dv$ord <- unname(aord[dv$adminID])
  dv$formID <- ds$formID[match(dv$datasetID, ds$datasetID)]
  dv$instrument <- ds$instrument[match(dv$datasetID, ds$datasetID)]

  # c. .sav 有、題庫沒有的 Xn ----
  lk <- paste(it$instrument, tolower(it$varname))
  orphan <- dv[dv$kind == "item" & !nzchar(dv$item_uid) & grepl("[nN]$", dv$varname), ]
  orphan$pred <- it$item_uid[match(paste(orphan$instrument, tolower(sub("[nN]$", "", orphan$varname))),
                                   lk)]
  orphan <- orphan[!is.na(orphan$pred), , drop = FALSE]
  orphan$item_uid <- paste0(orphan$instrument, ".", orphan$varname)
  orphan <- orphan[!(tolower(orphan$item_uid) %in% tolower(it$item_uid)), , drop = FALSE]

  meta <- rel_read_meta_labels(meta_dir, sav$map, unique(orphan$datasetID))
  sav_vs <- sav$value_sets
  next_vs <- suppressWarnings(max(as.integer(sub("^vs", "", unique(vc$valueSetID))), na.rm = TRUE))
  if (!is.finite(next_vs)) next_vs <- 0L

  new_items <- list(); new_iv <- list(); new_vc <- list(); fix <- list(); new_mem <- list()
  for (u in unique(orphan$item_uid)) {
    o <- orphan[orphan$item_uid == u, , drop = FALSE]
    o <- o[order(o$ord), , drop = FALSE]
    pred <- o$pred[1]
    pr <- it[it$item_uid == pred, , drop = FALSE]
    pvers <- iv[iv$item_uid == pred, , drop = FALSE]
    plast <- pvers[which.max(as.integer(pvers$itemVer)), , drop = FALSE]

    row <- pr
    row$item_uid <- u
    row$varname <- o$varname[1]
    row$deprecated <- "0"
    row$note <- paste0("new varname from .sav header (", o$datasetID[1], "); predecessor ", pred)
    new_items[[u]] <- row

    lab <- meta$label[meta$datasetID == o$datasetID[1] & meta$varname == o$varname[1]]
    lab <- if (length(lab) && nzchar(lab[1])) lab[1] else ""
    # 原文：^[一二三四五六七八九十]+[.、]
    lab_clean <- trimws(gsub("^\\s*[\u4e00\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+[.\u3001]\\s*",
                             "", lab, perl = TRUE))
    # 原文：去掉「1.」「6.1.」這種題號（前面可以接在全形冒號或開頭）
    lab_clean <- gsub("(^|(?<=[\uff1a:]))\\s*[0-9]+(\\.[0-9]+)*\\.(?![0-9])\\s*", "", lab_clean,
                      perl = TRUE)

    sv <- sav_vs[sav_vs$datasetID == o$datasetID[1] & sav_vs$varname == o$varname[1] &
                   sav_vs$is_missing != "1", c("code", "label"), drop = FALSE]
    sv$label <- trimws(gsub("\\[[^]]*\\]", "", sv$label, perl = TRUE))
    vs_id <- plast$valueSetID
    if (nrow(sv)) {
      same <- isTRUE(rel_values_match(vs_rows[[vs_id]] %||% sv[0, ], sv, miss_codes))
      if (!same) {
        hit <- names(vs_rows)[vapply(vs_rows, function(r) {
          isTRUE(nrow(r) == nrow(sv) && rel_values_match(r, sv, miss_codes))
        }, logical(1))]
        if (length(hit)) {
          vs_id <- hit[1]
        } else {
          next_vs <- next_vs + 1L
          vs_id <- sprintf("vs%04d", next_vs)
          new_vc[[u]] <- data.frame(valueSetID = vs_id, code = sv$code, label = sv$label,
                                    order = as.character(seq_len(nrow(sv))),
                                    stringsAsFactors = FALSE)
          vs_rows[[vs_id]] <- sv
        }
      }
    }
    new_iv[[u]] <- data.frame(
      item_uid = u, itemVer = "1", label = if (nzchar(lab_clean)) lab_clean else plast$label,
      stem = plast$stem, valueSetID = vs_id, itemType = plast$itemType, period = "",
      note = paste0(".sav label: ", lab), stringsAsFactors = FALSE)

    fix[[u]] <- data.frame(datasetID = o$datasetID, varname = o$varname, item_uid = u,
                           itemVer = "1", stringsAsFactors = FALSE)

    # 題組：前身在 Xn 出現的題本裡的成員資格
    forms <- unique(o$formID[!is.na(o$formID) & nzchar(o$formID)])
    gm <- kit$group_members[kit$group_members$item_uid == pred &
                              kit$group_members$formID %in% forms, , drop = FALSE]
    if (nrow(gm)) {
      gm$item_uid <- u
      gm$scheme <- "official"
      new_mem[[u]] <- gm
    }
  }
  bind <- function(x, tb) {
    if (!length(x)) return(empty_kit_table(tb))
    conform_kit_table(do.call(rbind, unname(x)), tb)
  }
  new_items <- bind(new_items, "items")
  new_iv <- bind(new_iv, "item_versions")
  new_vc <- bind(new_vc, "value_codes")
  new_mem <- bind(new_mem, "group_members")
  fix <- if (length(fix)) do.call(rbind, unname(fix)) else
    data.frame(datasetID = character(0), varname = character(0), item_uid = character(0),
               itemVer = character(0), stringsAsFactors = FALSE)
  rownames(fix) <- NULL

  # 之後的配對用補上新題號的版本
  it2 <- rbind(it, new_items)
  iv2 <- rbind(iv, new_iv)
  ver_key <- paste(iv2$item_uid, iv2$itemVer, sep = "|")
  iv <- iv2
  if (nrow(fix)) {
    k <- match(paste(fix$datasetID, fix$varname), paste(dv$datasetID, dv$varname))
    dv$item_uid[k] <- fix$item_uid
    dv$itemVer[k] <- fix$itemVer
  }

  # a. 換題號 ----
  lk2 <- paste(it2$instrument, tolower(it2$varname))
  xn <- it2[grepl("[nN]$", it2$varname), , drop = FALSE]
  base <- paste(xn$instrument, tolower(sub("[nN]$", "", xn$varname)))
  hit <- base %in% lk2
  pairs <- data.frame(x = it2$item_uid[match(base[hit], lk2)], xn = xn$item_uid[hit],
                      stringsAsFactors = FALSE)
  ren <- lapply(seq_len(nrow(pairs)), function(i) {
    x <- pairs$x[i]; y <- pairs$xn[i]
    dy <- dv[dv$item_uid == y, , drop = FALSE]
    dx <- dv[dv$item_uid == x, , drop = FALSE]
    xv <- sort(as.integer(iv$itemVer[iv$item_uid == x]))
    yv <- sort(as.integer(iv$itemVer[iv$item_uid == y]))
    if (!length(xv) || !length(yv)) return(NULL)
    eff <- ""; to_ver <- yv[1]; from_ver <- xv[length(xv)]
    if (nrow(dy)) {
      dy <- dy[order(dy$ord), , drop = FALSE]
      eff <- dy$adminID[1]
      if (nzchar(dy$itemVer[1])) to_ver <- as.integer(dy$itemVer[1])
      before <- dx[dx$ord <= dy$ord[1] & nzchar(dx$itemVer), , drop = FALSE]
      if (nrow(before)) from_ver <- max(as.integer(before$itemVer[before$ord == max(before$ord)]))
    }
    co <- intersect(dx$datasetID, dy$datasetID)
    sim <- rel_similarity(label_of(x, from_ver), label_of(y, to_ver),
                          stem_of(x, from_ver), stem_of(y, to_ver))
    vm <- rel_values_match(vs_of(x, from_ver), vs_of(y, to_ver), miss_codes)
    codes_changed <- !is.na(vm) && identical(attr(vm, "codes_same"), FALSE)
    comp <- rel_comparable(sim, vm)
    conf <- if (sim %in% c("identical", "high") && isTRUE(vm) && !length(co)) "high" else
      if (sim %in% c("low", "")) "low" else "medium"
    basis <- c(sprintf("varname %s -> %s", sub("^[A-Z]\\.", "", x), sub("^[A-Z]\\.", "", y)),
               sprintf("label %s", if (nzchar(sim)) sim else "missing"),
               sprintf("values %s", if (is.na(vm)) "unknown" else if (vm) "same" else "differ"),
               if (length(co)) sprintf("both in %d dataset(s): %s", length(co),
                                       paste(utils::head(co, 3), collapse = ","))
               else "no overlap",
               if (y %in% new_items$item_uid) "Xn new from .sav header" else NULL)
    data.frame(from_uid = x, from_ver = as.character(from_ver), to_uid = y,
               to_ver = as.character(to_ver), kind = "renamed", similarity = sim,
               comparable = comp,
               meaning_changed = if (codes_changed || sim == "low") "1" else "0",
               effective_admin = eff,
               reason = sprintf("varname %s renamed to %s%s", sub("^[A-Z]\\.", "", x),
                                sub("^[A-Z]\\.", "", y),
                                if (nzchar(eff)) paste0(" from ", eff) else ""),
               sourceID = if (y %in% new_items$item_uid) rel_usage_source(kit, eff) else "V02-BANK",
               confidence = conf, basis = paste(basis, collapse = "; "),
               stringsAsFactors = FALSE)
  })

  # b. 改版 ----
  # 原文：v(\d+)→v(\d+)版本異動:\s*(...)，到下一段、[itemVer、問卷別、適用 為止
  seg_re <- paste0("(?s)v([0-9]+)\u2192v([0-9]+)\u7248\u672c\u7570\u52d5:\\s*(.*?)",
                   "(?=;\\s*v[0-9]+\u2192v[0-9]+\u7248\u672c\u7570\u52d5|\\s*\\[itemVer|",
                   ";\\s*\u554f\u5377\u5225|;\\s*\u9069\u7528|$)")
  segs <- regmatches(note, gregexpr(seg_re, note, perl = TRUE))
  seg_df <- data.frame(item_uid = rep(iv$item_uid[seq_along(note)], lengths(segs)),
                       seg = unlist(segs), stringsAsFactors = FALSE)
  if (nrow(seg_df)) {
    seg_df$from <- as.integer(sub("(?s)^v([0-9]+).*", "\\1", seg_df$seg, perl = TRUE))
    seg_df$to <- as.integer(sub("(?s)^v[0-9]+\u2192v([0-9]+).*", "\\1", seg_df$seg, perl = TRUE))
    seg_df$seg <- trimws(sub(";\\s*$", "", gsub("\\s*[\r\n]+\\s*", " ", seg_df$seg, perl = TRUE)))
    seg_df <- seg_df[!duplicated(paste(seg_df$item_uid, seg_df$seg)), , drop = FALSE]
  }
  vlist <- split(as.integer(iv$itemVer), iv$item_uid)
  vlist <- vlist[lengths(vlist) > 1L]
  rev <- lapply(names(vlist), function(u) {
    v <- sort(unique(vlist[[u]]))
    lapply(seq_len(length(v) - 1L), function(j) {
      a <- v[j]; b <- v[j + 1L]
      s <- seg_df[seg_df$item_uid == u & seg_df$from >= a & seg_df$to <= b, , drop = FALSE]
      s <- s[order(s$from), , drop = FALSE]
      reason <- paste(s$seg, collapse = "; ")
      sim <- rel_similarity(label_of(u, a), label_of(u, b), stem_of(u, a), stem_of(u, b))
      vm <- rel_values_match(vs_of(u, a), vs_of(u, b), miss_codes)
      codes_changed <- !is.na(vm) && identical(attr(vm, "codes_same"), FALSE)
      # 原文：題意
      says_meaning <- grepl("\u984c\u610f", reason)
      has_note <- nrow(s) > 0L
      conf <- if (has_note && sim %in% c("identical", "high")) "high" else
        if ((has_note && sim == "medium") || (!has_note && sim %in% c("identical", "high")))
          "medium" else "low"
      du <- dv[dv$item_uid == u & dv$itemVer == as.character(b), , drop = FALSE]
      eff <- if (nrow(du)) du$adminID[which.min(du$ord)] else ""
      basis <- c(if (has_note) sprintf("v02 note (%d segment(s))", nrow(s)) else "no v02 note",
                 if (b - a > 1L) sprintf("version gap %d->%d (merged versions)", a, b) else NULL,
                 sprintf("label %s", if (nzchar(sim)) sim else "missing"),
                 sprintf("values %s", if (is.na(vm)) "unknown" else if (vm) "same" else
                   if (codes_changed) "codes differ" else "labels differ"),
                 if (says_meaning) "note mentions meaning change" else NULL)
      data.frame(from_uid = u, from_ver = as.character(a), to_uid = u, to_ver = as.character(b),
                 kind = "revised", similarity = sim, comparable = rel_comparable(sim, vm),
                 meaning_changed = if (says_meaning || codes_changed || sim == "low") "1" else "0",
                 effective_admin = eff, reason = reason, sourceID = "V02-BANK",
                 confidence = conf, basis = paste(basis, collapse = "; "),
                 stringsAsFactors = FALSE)
    })
  })
  rev <- unlist(rev, recursive = FALSE)

  rel <- do.call(rbind, c(ren, rev))
  if (is.null(rel)) rel <- cbind(empty_kit_table("item_relations"),
                                 confidence = character(0), basis = character(0))
  extra <- rel[, c("confidence", "basis"), drop = FALSE]
  rel$decided_by <- "auto"
  rel$decided_on <- ""
  rel <- cbind(conform_kit_table(rel, "item_relations"), extra)
  rel <- rel[!duplicated(kit_key_str(rel, KIT_SCHEMA$item_relations$key)), , drop = FALSE]
  rownames(rel) <- NULL

  list(relations = rel, new_items = new_items, new_item_versions = new_iv,
       new_value_codes = new_vc, new_members = new_mem, dataset_vars_fix = fix)
}

#' comparable 等級
#' @noRd
rel_comparable <- function(sim, vm) {
  if (sim %in% c("identical", "high") && !isFALSE(as.logical(vm))) return("yes")
  if (sim %in% c("low", "none")) return("no")
  "partial"
}

#' 施測的資料使用說明 sourceID（US-M36W84），不在 sources 就留空
#' @noRd
rel_usage_source <- function(kit, adminID) {
  if (!nzchar(adminID)) return("")
  ad <- kit$admins[kit$admins$adminID == adminID, , drop = FALSE]
  code <- kit$cohorts$code[match(ad$cohortID, kit$cohorts$cohortID)]
  id <- sprintf("US-%sW%02d", code, as.integer(ad$ageMonth))
  if (length(id) && id %in% kit$sources$sourceID) id else ""
}

#' Add confirmed relation candidates and new items to a kit
#'
#' Helper for the integrator: appends `propose_relations()` output, keeping only
#' relations at the given confidence levels, and fills `dataset_vars` rows from
#' `dataset_vars_fix`.
#'
#' @param kit A kit list.
#' @param rel Output of [propose_relations()].
#' @param levels Confidence levels to keep.
#' @return The kit with the rows added.
#' @noRd
apply_relations <- function(kit, rel, levels = "high") {
  r <- rel$relations[rel$relations$confidence %in% levels, , drop = FALSE]
  kit$item_relations <- conform_kit_table(rbind(kit$item_relations,
                                                conform_kit_table(r, "item_relations")),
                                          "item_relations")
  kit$items <- rbind(kit$items, rel$new_items)
  kit$item_versions <- rbind(kit$item_versions, rel$new_item_versions)
  kit$value_codes <- rbind(kit$value_codes, rel$new_value_codes)
  kit$group_members <- rbind(kit$group_members, rel$new_members)
  f <- rel$dataset_vars_fix
  if (nrow(f)) {
    k <- match(paste(f$datasetID, f$varname),
               paste(kit$dataset_vars$datasetID, kit$dataset_vars$varname))
    kit$dataset_vars$item_uid[k[!is.na(k)]] <- f$item_uid[!is.na(k)]
    kit$dataset_vars$itemVer[k[!is.na(k)]] <- f$itemVer[!is.na(k)]
  }
  kit
}
