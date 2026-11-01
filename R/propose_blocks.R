# R/propose_blocks.R — 1.6：區塊狀態（block_status）候選：比對同題本的施測，找出輪替、只問部分樣本、帶入的區塊
#
# 產出都是候選，要人工確認：每列帶 confidence（high／medium／low）和 basis（根據）。
# 只比已釋出的資料檔（admins.status = released、有 .sav），區塊用 items.block。
# 「題本＠年度」：資料檔落在哪一筆 form_notes（起始 <= 施測開始的最後一筆；
# 年度寫成 2017.09 的從該月起算）。沒有 form_notes 的題本用調查年度（3 月起算）。
# 規則：
# - 同題本同年度（兩個以上施測）：
#   某檔沒有這個區塊、同組別檔有 → none（high）；
#   某檔只有區塊的一部分題目 → subset（medium）。
# - 相鄰年度（同題本，依年度排序）：本檔沒有、但更早和更晚的年度都有的題目（中間空一段）
#   → 整個區塊空了是 none、只空一部分是 subset；
#   前後緊鄰的年度都有 → medium，否則 low。只有一邊有的視為題本改版，不出列。
# - 親友檔（-PK）：和同一施測的家長檔比。家長檔有、親友檔整個沒有的區塊 → none，
#   who = 親友版不問；只有部分題目 → subset。0.2 note「適用:…簡親友版」和實際收錄一致 → high，
#   否則 medium。
# - 說明文件（us_*.txt）：句子提到「串回、帶入、串連其他波次」→ 列出的變項 carried；
#   「未再詢問、未於本波再詢問」→ 補強上面的 none／subset（升為 high）；
#   「再度蒐集、並非所有樣本」→ subset（low，可能是補問也可能是帶入）。
#   .sav 標籤（*_meta.csv）提到「帶入、前次」→ carried（low）。
# - 同一檔同一區塊有多條證據時，asked 取 carried > none > subset，basis 全部接起來。
# asked = all 是預設值，不出列。

# 原文：串回|帶入|串連其他波次|沿用前
PB_PAT_CARRIED <- "\u4e32\u56de|\u5e36\u5165|\u4e32\u9023\u5176\u4ed6\u6ce2\u6b21|\u6cbf\u7528\u524d"
# 原文：未再詢問|未於.{0,12}再詢問|不再詢問
PB_PAT_NONE <- "\u672a\u518d\u8a62\u554f|\u672a\u65bc.{0,12}\u518d\u8a62\u554f|\u4e0d\u518d\u8a62\u554f"
# 原文：再度蒐集|並非所有樣本
PB_PAT_SUBSET <- "\u518d\u5ea6\u8490\u96c6|\u4e26\u975e\u6240\u6709\u6a23\u672c"
# 原文：跳答
PB_PAT_SKIP <- "\u8df3\u7b54"
# 原文：帶入|前次
PB_PAT_META <- "\u5e36\u5165|\u524d\u6b21"

#' form_notes 的年度字串 → 起始日（"2017" → 2017-01-01；"2017.09" → 2017-09-01）
#' @noRd
pb_year_start <- function(y) {
  y <- as.character(y)
  m <- ifelse(grepl("^[0-9]{4}\\.[0-9]{1,2}$", y), sub("^[0-9]{4}\\.", "", y), "1")
  as.Date(sprintf("%s-%02d-01", substr(y, 1, 4), as.integer(m)))
}

#' 已釋出、有題本的資料檔，加上施測開始日和「題本＠年度」
#' @noRd
pb_datasets <- function(kit) {
  ds <- kit$datasets
  adm <- kit$admins[match(ds$adminID, kit$admins$adminID), ]
  keep <- adm$status %in% "released" & ds$file != "v02" & nzchar(ds$formID) &
    !(ds$instrument %in% "S")
  ds <- ds[keep, , drop = FALSE]
  adm <- adm[keep, , drop = FALSE]
  ds$from <- as.Date(ifelse(nzchar(adm$surveyFrom), adm$surveyFrom, NA_character_))
  ds$is_pk <- grepl("-PK$", ds$datasetID)
  fn <- kit$form_notes
  ds$revision <- vapply(seq_len(nrow(ds)), function(i) {
    yrs <- fn$year[fn$formID == ds$formID[i]]
    d <- ds$from[i]
    if (is.na(d)) return("")
    if (!length(yrs)) {
      yy <- as.integer(format(d, "%Y")) - (as.integer(format(d, "%m")) < 3L)
      return(as.character(yy))
    }
    st <- pb_year_start(yrs)
    ok <- which(st <= d)
    if (length(ok)) yrs[ok[which.max(st[ok])]] else yrs[which.min(st)]
  }, character(1))
  ds$rev_start <- pb_year_start(ifelse(nzchar(ds$revision), ds$revision, "9999"))
  ds[order(ds$formID, ds$rev_start, ds$from, ds$datasetID), , drop = FALSE]
}

#' 變項清單的簡短寫法：前 6 個 + 共幾題
#' @noRd
pb_list_vars <- function(v, n = 6L) {
  v <- sort(unique(v))
  # 原文：…共 %d 題
  if (length(v) > n) paste0(paste(v[seq_len(n)], collapse = ", "), sprintf("\u2026\u5171 %d \u984c", length(v)))
  else paste(v, collapse = ", ")
}

#' 一列候選
#' @noRd
pb_row <- function(datasetID, block, asked, who = "", who_var = "", reason = "",
                   sourceID = "", confidence, basis, n_items = NA_integer_) {
  n <- length(datasetID)
  data.frame(datasetID = datasetID, block = block, asked = asked, who = rep_len(who, n),
             who_var = rep_len(who_var, n), reason = rep_len(reason, n),
             decided_by = rep_len("auto", n), sourceID = rep_len(sourceID, n),
             confidence = confidence, basis = basis, n_items = rep_len(as.integer(n_items), n),
             stringsAsFactors = FALSE)
}

#' 說明文件裡的提示：一列一個（資料檔, 變項, 種類）
#' @noRd
pb_usage_hints <- function(kit, usage_dir, dv_all) {
  empty <- data.frame(datasetID = character(0), varname = character(0), kind = character(0),
                      who_var = character(0), sourceID = character(0), stringsAsFactors = FALSE)
  if (is.null(usage_dir) || !dir.exists(usage_dir)) return(empty)
  files <- list.files(usage_dir, pattern = "^us_M[0-9]{2}W[0-9]{2}\\.txt$", full.names = TRUE)
  out <- list()
  for (f in files) {
    tag <- sub("^us_(M[0-9]{2})W([0-9]{2})\\.txt$", "\\1 \\2", basename(f))
    coh <- strsplit(tag, " ")[[1]][1]
    age <- as.integer(strsplit(tag, " ")[[1]][2])
    cid <- kit$cohorts$cohortID[kit$cohorts$code == coh]
    aid <- kit$admins$adminID[kit$admins$cohortID %in% cid &
                                as.integer(kit$admins$ageMonth) == age]
    if (!length(aid)) next
    ids <- kit$datasets$datasetID[kit$datasets$adminID %in% aid]
    dva <- dv_all[dv_all$datasetID %in% ids, , drop = FALSE]
    known <- unique(tolower(dva$varname))
    # 「未再詢問」的變項本來就不在這次的檔裡：用全題庫的變項名
    known_all <- unique(c(known, tolower(kit$items$varname)))
    src <- paste0("US-", coh, "W", sprintf("%02d", age))
    if (!src %in% kit$sources$sourceID) src <- ""
    lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
    section <- ""
    for (ln in lines) {
      # 原文：關於本波次(家長|親友|教保|教師)
      sec <- regmatches(ln, regexpr("\u95dc\u65bc\u672c\u6ce2\u6b21(\u5bb6\u9577|\u89aa\u53cb|\u6559\u4fdd|\u6559\u5e2b)", ln))
      if (length(sec)) {
        section <- substring(sec, 6)
      } else if (grepl("^\\s*(\\([0-9]+\\)|[0-9]+\\.\\s)", ln)) {
        section <- ""
      }
      if (!grepl(paste(PB_PAT_CARRIED, PB_PAT_NONE, PB_PAT_SUBSET, sep = "|"), ln)) next
      # 這一段指向哪個資料檔
      # 原文：家長、親友、教保、教師
      target <- switch(section,
                       "\u5bb6\u9577" = ids[grepl("-P$", ids)],
                       "\u89aa\u53cb" = ids[grepl("-PK$", ids)],
                       "\u6559\u4fdd" = ids[grepl("-T$", ids)],
                       "\u6559\u5e2b" = ids[grepl("-T$", ids)],
                       ids)
      toks_of <- function(s, set = known) {
        tk <- tolower(regmatches(s, gregexpr("[A-Za-z][A-Za-z0-9_]*[0-9][A-Za-z0-9_]*", s))[[1]])
        unique(tk[tk %in% set])
      }
      para_toks <- toks_of(ln, known_all)
      if (!length(para_toks)) next
      # 原文：。
      sents <- strsplit(ln, "\u3002", fixed = TRUE)[[1]]
      last <- character(0)
      for (s in sents) {
        tk <- toks_of(s, if (grepl(PB_PAT_NONE, s) && !grepl(PB_PAT_CARRIED, s)) known_all else known)
        kind <- if (grepl(PB_PAT_CARRIED, s)) "carried" else if (grepl(PB_PAT_NONE, s)) "none" else
          if (grepl(PB_PAT_SUBSET, s)) "subset" else ""
        if (!nzchar(kind)) { if (length(tk)) last <- tk; next }
        # 「串回了前兩個波次中的資料」沒列變項：指前一句的清單
        if (!length(tk) && kind == "carried") tk <- last
        if (!length(tk)) next
        wv <- ""
        if (kind == "carried" && grepl(PB_PAT_SKIP, ln) && !(para_toks[1] %in% tk)) wv <- para_toks[1]
        for (d in target) {
          have <- unique(tolower(dva$varname[dva$datasetID == d]))
          hit <- intersect(tk, have)
          if (kind == "none") hit <- tk  # 沒問的題本來就不在檔裡
          if (!length(hit)) next
          out[[length(out) + 1L]] <- data.frame(datasetID = d, varname = hit, kind = kind,
                                                who_var = wv, sourceID = src,
                                                stringsAsFactors = FALSE)
        }
        last <- tk
      }
    }
  }
  if (!length(out)) return(empty)
  res <- do.call(rbind, out)
  res[!duplicated(res[, c("datasetID", "varname", "kind")]), , drop = FALSE]
}

#' .sav 標籤裡的「帶入、前次」：一列一個（資料檔, 變項）
#' @noRd
pb_meta_hints <- function(kit, meta_dir, ds) {
  empty <- data.frame(datasetID = character(0), varname = character(0), label = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(meta_dir) || !dir.exists(meta_dir)) return(empty)
  adm <- kit$admins[match(ds$adminID, kit$admins$adminID), ]
  coh <- kit$cohorts$code[match(adm$cohortID, kit$cohorts$cohortID)]
  wave_dir <- paste0(tolower(coh), "w", sprintf("%02d", as.integer(adm$ageMonth)))
  out <- list()
  for (i in seq_len(nrow(ds))) {
    f <- file.path(meta_dir, wave_dir[i], paste0(ds$file[i], "_meta.csv"))
    if (!file.exists(f)) next
    m <- utils::read.csv(f, encoding = "UTF-8", stringsAsFactors = FALSE)
    if (!all(c("var", "label") %in% names(m))) next
    hit <- grepl(PB_PAT_META, m$label)
    if (any(hit)) out[[length(out) + 1L]] <- data.frame(
      datasetID = ds$datasetID[i], varname = tolower(m$var[hit]), label = m$label[hit],
      stringsAsFactors = FALSE)
  }
  if (!length(out)) return(empty)
  do.call(rbind, out)
}

#' 合併同一（資料檔, 區塊）的多條證據
#' @noRd
pb_merge_rows <- function(rows) {
  if (!nrow(rows)) return(rows)
  rank_asked <- c(carried = 1L, none = 2L, subset = 3L)
  rank_conf <- c(high = 1L, medium = 2L, low = 3L)
  key <- paste(rows$datasetID, rows$block, sep = "|")
  out <- lapply(split(seq_len(nrow(rows)), factor(key, levels = unique(key))), function(ix) {
    r <- rows[ix, , drop = FALSE]
    best <- r$asked[which.min(rank_asked[r$asked])]
    main <- r[r$asked == best, , drop = FALSE]
    main <- main[order(rank_conf[main$confidence]), , drop = FALSE]
    o <- main[1, , drop = FALSE]
    # 說明文件「未再詢問」涵蓋資料比對缺題的一半以上 → 升為 high
    doc_n <- sum(r$n_items[r$reason == "doc_not_asked"], na.rm = TRUE)
    dat_n <- suppressWarnings(max(r$n_items[r$reason != "doc_not_asked"], na.rm = TRUE))
    if (best %in% c("none", "subset") && doc_n > 0L && is.finite(dat_n) && doc_n >= dat_n / 2)
      o$confidence <- "high"
    if (o$reason == "doc_not_asked") o$reason <- r$reason[r$reason != "doc_not_asked"][1] %||% ""
    if (is.na(o$reason)) o$reason <- ""
    nz <- function(v) unique(v[nzchar(v)])
    o$sourceID <- c(nz(main$sourceID), nz(r$sourceID), "")[1]
    o$who <- c(nz(main$who), "")[1]
    o$who_var <- c(nz(main$who_var), "")[1]
    o$basis <- paste(nz(r$basis), collapse = "\uff1b")
    o
  })
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

#' Propose block_status candidates by comparing datasets of the same form
#'
#' Compares released datasets that use the same form (`datasets$formID`) at the block level
#' (`items$block`): datasets in the same form-year (revision in `form_notes`), consecutive
#' years of the same form, and kin (`-PK`) files against their parent file. Usage documents
#' and .sav labels add hints for carried (帶入) blocks.
#'
#' @param kit A kit list (see [read_kit()]).
#' @param usage_dir Folder with `us_<cohort>W<age>.txt` usage documents (optional).
#' @param meta_dir Folder with `<wave>/<file>_meta.csv` .sav headers (optional).
#' @param crosswalk Optional data frame with columns `cohort`, `q`, `age`, `var` (one row per
#'   variable present in a wave of the official crosswalk); differences within a form-year
#'   that the crosswalk shows but the .sav files do not are listed in `unexplained`.
#' @return A list: `block_status` (schema columns plus `confidence`, `basis`; only rows that
#'   are not simply `all`), `unexplained` (item-level differences within a form-year not
#'   covered by a block row) and `pairs` (the comparisons made).
#' @export
propose_blocks <- function(kit, usage_dir = NULL, meta_dir = NULL, crosswalk = NULL) {
  ds <- pb_datasets(kit)
  dv_all <- kit$dataset_vars[kit$dataset_vars$datasetID %in% ds$datasetID, , drop = FALSE]
  dv <- dv_all[dv_all$kind == "item", , drop = FALSE]
  dv$key <- ifelse(nzchar(dv$item_uid), dv$item_uid, paste0("?", tolower(dv$varname)))
  dv$block <- kit$items$block[match(dv$item_uid, kit$items$item_uid)]
  dv$block[is.na(dv$block)] <- ""
  blk_of <- stats::setNames(dv$block, dv$key)
  var_of <- stats::setNames(dv$varname, dv$key)
  have <- split(dv$key, dv$datasetID)
  have_of <- function(d) have[[d]] %||% character(0)

  rows <- list()
  add <- function(r) rows[[length(rows) + 1L]] <<- r
  pairs <- list()
  unexpl <- list()

  main <- ds[!ds$is_pk, , drop = FALSE]
  grp <- paste(main$formID, main$revision, sep = "@")

  # 同題本同年度 ----
  for (g in unique(grp)) {
    ids <- main$datasetID[grp == g]
    if (length(ids) < 2L) next
    pairs[[length(pairs) + 1L]] <- data.frame(
      kind = "same_year", formID = main$formID[grp == g][1], revision = main$revision[grp == g][1],
      datasets = paste(ids, collapse = " | "), stringsAsFactors = FALSE)
    uni <- unique(unlist(lapply(ids, have_of)))
    for (d in ids) {
      others <- setdiff(ids, d)
      miss <- setdiff(uni, have_of(d))
      for (b in setdiff(unique(blk_of[uni]), "")) {
        inb <- uni[blk_of[uni] == b]
        mb <- intersect(miss, inb)
        if (!length(mb)) next
        who_has <- others[vapply(others, function(o) any(mb %in% have_of(o)), logical(1))]
        if (length(mb) == length(inb)) {
          add(pb_row(d, b, "none",
                     # 原文：輪替（同年度另一施測有此區塊）或題本例外
                     reason = "\u8f2a\u66ff\uff08\u540c\u5e74\u5ea6\u53e6\u4e00\u65bd\u6e2c\u6709\u6b64\u5340\u584a\uff09\u6216\u984c\u672c\u4f8b\u5916",
                     confidence = "high", n_items = length(mb),
                     # 原文：同題本同年度（%s）的 %s 有此區塊 %d 題，本檔 0 題
                     basis = sprintf("\u540c\u984c\u672c\u540c\u5e74\u5ea6\uff08%s\uff09\u7684 %s \u6709\u6b64\u5340\u584a %d \u984c\uff0c\u672c\u6a94 0 \u984c",
                                     g, paste(who_has, collapse = ","), length(inb))))
        } else {
          add(pb_row(d, b, "subset",
                     # 原文：只問部分題目（同年度另一施測問得較多）
                     reason = "\u53ea\u554f\u90e8\u5206\u984c\u76ee\uff08\u540c\u5e74\u5ea6\u53e6\u4e00\u65bd\u6e2c\u554f\u5f97\u8f03\u591a\uff09",
                     confidence = "medium", n_items = length(mb),
                     # 原文：同題本同年度（%s）：本檔有此區塊 %d/%d 題；缺（%s 有）：%s
                     basis = sprintf("\u540c\u984c\u672c\u540c\u5e74\u5ea6\uff08%s\uff09\uff1a\u672c\u6a94\u6709\u6b64\u5340\u584a %d/%d \u984c\uff1b\u7f3a\uff08%s \u6709\uff09\uff1a%s",
                                     g, length(inb) - length(mb), length(inb),
                                     paste(who_has, collapse = ","), pb_list_vars(var_of[mb]))))
        }
      }
      # 沒有區塊的題目：先記下，最後再看有沒有區塊列可以解釋
      mz <- miss[blk_of[miss] == ""]
      if (length(mz)) unexpl[[length(unexpl) + 1L]] <- data.frame(
        formID = main$formID[grp == g][1], revision = main$revision[grp == g][1],
        datasetID = d, varname = unname(var_of[mz]),
        item_uid = ifelse(startsWith(mz, "?"), "", mz), block = "",
        present_in = vapply(mz, function(k) paste(others[vapply(others, function(o) k %in% have_of(o), logical(1))], collapse = ","), character(1)),
        source = "sav",
        # 原文：題目沒有區塊，無法以區塊說明
        note = "\u984c\u76ee\u6c92\u6709\u5340\u584a\uff0c\u7121\u6cd5\u4ee5\u5340\u584a\u8aaa\u660e",
        stringsAsFactors = FALSE)
    }
  }

  # 相鄰年度（中間空一段）----
  for (f in unique(main$formID)) {
    fm <- main[main$formID == f, , drop = FALSE]
    revs <- unique(fm$revision)
    if (length(revs) < 3L) next
    rev_have <- lapply(revs, function(r) unique(unlist(lapply(fm$datasetID[fm$revision == r], have_of))))
    pairs[[length(pairs) + 1L]] <- data.frame(
      kind = "consecutive", formID = f, revision = paste(revs, collapse = " > "),
      datasets = paste(sprintf("%s:%s", fm$revision, fm$datasetID), collapse = " | "),
      stringsAsFactors = FALSE)
    for (i in seq_along(revs)[-c(1L, length(revs))]) {
      before <- unique(unlist(rev_have[seq_len(i - 1L)]))
      after <- unique(unlist(rev_have[(i + 1L):length(revs)]))
      gap_all <- intersect(before, after)
      tight <- intersect(rev_have[[i - 1L]], rev_have[[i + 1L]])
      for (d in fm$datasetID[fm$revision == revs[i]]) {
        gap <- setdiff(gap_all, have_of(d))
        for (b in setdiff(unique(blk_of[gap]), "")) {
          gb <- gap[blk_of[gap] == b]
          n_here <- sum(blk_of[have_of(d)] == b)
          conf <- if (all(gb %in% tight)) "medium" else "low"
          if (n_here == 0L) {
            add(pb_row(d, b, "none",
                       # 原文：輪替（前後年度都有此區塊）
                       reason = "\u8f2a\u66ff\uff08\u524d\u5f8c\u5e74\u5ea6\u90fd\u6709\u6b64\u5340\u584a\uff09",
                       confidence = conf, n_items = length(gb),
                       # 原文：同題本前後年度都有此區塊（%s 之前、之後），本檔 0 題
                       basis = sprintf("\u540c\u984c\u672c\u524d\u5f8c\u5e74\u5ea6\u90fd\u6709\u6b64\u5340\u584a\uff08%s \u4e4b\u524d\u3001\u4e4b\u5f8c\uff09\uff0c\u672c\u6a94 0 \u984c",
                                       revs[i])))
          } else {
            add(pb_row(d, b, "subset",
                       # 原文：區塊內部分題目輪替（前後年度有、本年度沒有）
                       reason = "\u5340\u584a\u5167\u90e8\u5206\u984c\u76ee\u8f2a\u66ff\uff08\u524d\u5f8c\u5e74\u5ea6\u6709\u3001\u672c\u5e74\u5ea6\u6c92\u6709\uff09",
                       confidence = conf, n_items = length(gb),
                       # 原文：%s 之前和之後的年度都有、本檔沒有的題目 %d 題：%s
                       basis = sprintf("%s \u4e4b\u524d\u548c\u4e4b\u5f8c\u7684\u5e74\u5ea6\u90fd\u6709\u3001\u672c\u6a94\u6c92\u6709\u7684\u984c\u76ee %d \u984c\uff1a%s",
                                       revs[i], length(gb), pb_list_vars(var_of[gb]))))
          }
        }
      }
    }
  }

  # 親友檔 ----
  iv <- kit$item_versions
  # 原文：簡親友版
  brief <- unique(iv$item_uid[grepl("\u7c21\u89aa\u53cb\u7248", iv$note)])
  src_v02 <- if ("V02-BANK" %in% kit$sources$sourceID) "V02-BANK" else ""
  for (pk in ds$datasetID[ds$is_pk]) {
    p <- sub("K$", "", pk)
    if (!p %in% ds$datasetID) next
    pairs[[length(pairs) + 1L]] <- data.frame(kind = "kin_vs_parent",
                                              formID = ds$formID[ds$datasetID == pk],
                                              revision = ds$revision[ds$datasetID == pk],
                                              datasets = paste(p, pk, sep = " | "),
                                              stringsAsFactors = FALSE)
    hp <- have_of(p)
    hk <- have_of(pk)
    for (b in setdiff(unique(blk_of[hp]), "")) {
      inb <- hp[blk_of[hp] == b]
      got <- intersect(inb, hk)
      if (length(got) == length(inb)) next
      miss <- setdiff(inb, got)
      if (!length(got)) {
        consistent <- !any(inb %in% brief)
        add(pb_row(pk, b, "none",
                   # 原文：親友版不問
                   who = "\u89aa\u53cb\u7248\u4e0d\u554f",
                   # 原文：親友版（簡親友版）不收此區塊
                   reason = "\u89aa\u53cb\u7248\uff08\u7c21\u89aa\u53cb\u7248\uff09\u4e0d\u6536\u6b64\u5340\u584a",
                   sourceID = src_v02,
                   confidence = if (consistent) "high" else "medium",
                   # 原文：家長檔 %s 有此區塊 %d 題，親友檔 0 題；0.2 note 標「簡親友版」的有 %d 題
                   basis = sprintf("\u5bb6\u9577\u6a94 %s \u6709\u6b64\u5340\u584a %d \u984c\uff0c\u89aa\u53cb\u6a94 0 \u984c\uff1b0.2 note \u6a19\u300c\u7c21\u89aa\u53cb\u7248\u300d\u7684\u6709 %d \u984c",
                                   p, length(inb), sum(inb %in% brief))))
      } else {
        consistent <- all(got %in% brief) && !any(miss %in% brief)
        add(pb_row(pk, b, "subset",
                   # 原文：簡親友版只問部分題目
                   who = "\u7c21\u89aa\u53cb\u7248\u53ea\u554f\u90e8\u5206\u984c\u76ee",
                   # 原文：親友版（簡親友版）只收部分題目
                   reason = "\u89aa\u53cb\u7248\uff08\u7c21\u89aa\u53cb\u7248\uff09\u53ea\u6536\u90e8\u5206\u984c\u76ee",
                   sourceID = src_v02,
                   confidence = if (consistent) "high" else "medium",
                   # 原文：親友檔有家長檔此區塊 %d/%d 題；收錄的題中標「簡親友版」%d 題，沒收的題中標「簡親友版」%d 題；缺：%s
                   basis = sprintf("\u89aa\u53cb\u6a94\u6709\u5bb6\u9577\u6a94\u6b64\u5340\u584a %d/%d \u984c\uff1b\u6536\u9304\u7684\u984c\u4e2d\u6a19\u300c\u7c21\u89aa\u53cb\u7248\u300d%d \u984c\uff0c\u6c92\u6536\u7684\u984c\u4e2d\u6a19\u300c\u7c21\u89aa\u53cb\u7248\u300d%d \u984c\uff1b\u7f3a\uff1a%s",
                                   length(got), length(inb), sum(got %in% brief),
                                   sum(miss %in% brief), pb_list_vars(var_of[miss]))))
      }
    }
  }

  # 說明文件和 .sav 標籤 ----
  hints <- pb_usage_hints(kit, usage_dir, dv_all)
  if (nrow(hints)) {
    lk <- stats::setNames(dv$key, paste(dv$datasetID, tolower(dv$varname)))
    hints$key <- lk[paste(hints$datasetID, hints$varname)]
    inst <- kit$datasets$instrument[match(hints$datasetID, kit$datasets$datasetID)]
    alt <- kit$items$item_uid[match(tolower(paste0(inst, ".", hints$varname)),
                                       tolower(paste0(kit$items$instrument, ".", kit$items$varname)))]
    hints$key[is.na(hints$key)] <- alt[is.na(hints$key)]
    hints$block <- kit$items$block[match(hints$key, kit$items$item_uid)]
    hints <- hints[!is.na(hints$block) & nzchar(hints$block), , drop = FALSE]
    for (k in unique(paste(hints$datasetID, hints$block, hints$kind, sep = "|"))) {
      h <- hints[paste(hints$datasetID, hints$block, hints$kind, sep = "|") == k, , drop = FALSE]
      d <- h$datasetID[1]; b <- h$block[1]; kind <- h$kind[1]
      n_blk <- sum(blk_of[have_of(d)] == b)
      wv <- c(h$who_var[nzchar(h$who_var)], "")[1]
      if (kind == "carried") {
        add(pb_row(d, b, "carried",
                   who = if (nzchar(wv))
                     # 原文：前波次曾受訪者跳答（%s），值由前波次串回
                     sprintf("\u524d\u6ce2\u6b21\u66fe\u53d7\u8a2a\u8005\u8df3\u7b54\uff08%s\uff09\uff0c\u503c\u7531\u524d\u6ce2\u6b21\u4e32\u56de", wv)
                   # 原文：本波未再詢問，值由前波次串回
                   else "\u672c\u6ce2\u672a\u518d\u8a62\u554f\uff0c\u503c\u7531\u524d\u6ce2\u6b21\u4e32\u56de",
                   who_var = wv,
                   # 原文：帶入（說明文件）
                   reason = "\u5e36\u5165\uff08\u8aaa\u660e\u6587\u4ef6\uff09",
                   sourceID = h$sourceID[1],
                   confidence = if (n_blk > 0L && nrow(h) / n_blk >= 0.5) "high" else "medium",
                   # 原文：說明文件 %s：串回／帶入的變項 %d 題（本檔此區塊 %d 題）：%s
                   basis = sprintf("\u8aaa\u660e\u6587\u4ef6 %s\uff1a\u4e32\u56de\uff0f\u5e36\u5165\u7684\u8b8a\u9805 %d \u984c\uff08\u672c\u6a94\u6b64\u5340\u584a %d \u984c\uff09\uff1a%s",
                                   h$sourceID[1], nrow(h), n_blk, pb_list_vars(h$varname))))
      } else if (kind == "none") {
        absent <- h[!(h$key %in% have_of(d)), , drop = FALSE]
        if (!nrow(absent)) next
        # 只用來佐證資料比對出的 none／subset
        add(pb_row(d, b, if (n_blk == 0L) "none" else "subset", reason = "doc_not_asked",
                   sourceID = h$sourceID[1], confidence = "low", n_items = nrow(absent),
                   # 原文：說明文件 %s：本波未再詢問 %d 題：%s
                   basis = sprintf("\u8aaa\u660e\u6587\u4ef6 %s\uff1a\u672c\u6ce2\u672a\u518d\u8a62\u554f %d \u984c\uff1a%s",
                                   h$sourceID[1], nrow(absent), pb_list_vars(absent$varname))))
      } else {
        add(pb_row(d, b, "subset",
                   # 原文：第一波未受訪的樣本補問（待確認：也可能全部重問或帶入）
                   who = "\u7b2c\u4e00\u6ce2\u672a\u53d7\u8a2a\u7684\u6a23\u672c\u88dc\u554f\uff08\u5f85\u78ba\u8a8d\uff1a\u4e5f\u53ef\u80fd\u5168\u90e8\u91cd\u554f\u6216\u5e36\u5165\uff09",
                   # 原文：首次受訪補問（說明文件）
                   reason = "\u9996\u6b21\u53d7\u8a2a\u88dc\u554f\uff08\u8aaa\u660e\u6587\u4ef6\uff09",
                   sourceID = h$sourceID[1], confidence = "low",
                   # 原文：說明文件 %s：因並非所有樣本都在第一波受訪，本波再度蒐集 %d 題：%s
                   basis = sprintf("\u8aaa\u660e\u6587\u4ef6 %s\uff1a\u56e0\u4e26\u975e\u6240\u6709\u6a23\u672c\u90fd\u5728\u7b2c\u4e00\u6ce2\u53d7\u8a2a\uff0c\u672c\u6ce2\u518d\u5ea6\u8490\u96c6 %d \u984c\uff1a%s",
                                   h$sourceID[1], nrow(h), pb_list_vars(h$varname))))
      }
    }
  }
  mh <- pb_meta_hints(kit, meta_dir, ds)
  if (nrow(mh)) {
    mh$key <- dv$key[match(paste(mh$datasetID, mh$varname), paste(dv$datasetID, tolower(dv$varname)))]
    mh$block <- blk_of[mh$key]
    mh <- mh[!is.na(mh$block) & nzchar(mh$block), , drop = FALSE]
    for (k in unique(paste(mh$datasetID, mh$block))) {
      h <- mh[paste(mh$datasetID, mh$block) == k, , drop = FALSE]
      add(pb_row(h$datasetID[1], h$block[1], "carried",
                 # 原文：帶入（.sav 標籤）
                 reason = "\u5e36\u5165\uff08.sav \u6a19\u7c64\uff09", confidence = "low",
                 # 原文：.sav 標籤提到帶入／前次：%s
                 basis = sprintf(".sav \u6a19\u7c64\u63d0\u5230\u5e36\u5165\uff0f\u524d\u6b21\uff1a%s",
                                 pb_list_vars(h$varname))))
    }
  }

  allrows <- if (length(rows)) do.call(rbind, rows) else pb_row(character(0), character(0),
                                                               character(0), confidence = character(0),
                                                               basis = character(0))
  # 只有說明文件「未再詢問」、沒有資料佐證的不出列
  keep_key <- unique(paste(allrows$datasetID, allrows$block)[allrows$reason != "doc_not_asked"])
  allrows <- allrows[paste(allrows$datasetID, allrows$block) %in% keep_key, , drop = FALSE]
  bs <- pb_merge_rows(allrows)
  if (nrow(bs)) bs <- bs[order(bs$datasetID, bs$block), , drop = FALSE]
  rownames(bs) <- NULL

  # 對照表：同年度的差異，.sav 表頭看不到的 ----
  if (!is.null(crosswalk) && nrow(crosswalk)) {
    cw <- crosswalk
    cw$age <- as.integer(cw$age)
    adm <- kit$admins
    adm$code <- kit$cohorts$code[match(adm$cohortID, kit$cohorts$cohortID)]
    cw$adminID <- adm$adminID[match(paste(cw$cohort, cw$age), paste(adm$code, as.integer(adm$ageMonth)))]
    # 原文：家長
    cw$instrument <- ifelse(grepl("^\u5bb6\u9577", cw$q), "P", "T")
    cw$datasetID <- main$datasetID[match(paste(cw$adminID, cw$instrument),
                                         paste(main$adminID, main$instrument))]
    cw <- cw[!is.na(cw$datasetID), , drop = FALSE]
    cw$var <- tolower(cw$var)
    sav_vars <- split(tolower(dv_all$varname), dv_all$datasetID)
    for (g in unique(grp)) {
      ids <- intersect(main$datasetID[grp == g], unique(cw$datasetID))
      if (length(ids) < 2L) next
      cv <- split(cw$var, factor(cw$datasetID, levels = ids))
      uni <- unique(unlist(cv))
      for (d in ids) {
        for (v in setdiff(uni, cv[[d]])) {
          yes <- ids[vapply(ids, function(o) v %in% cv[[o]], logical(1))]
          sav_yes <- ids[vapply(ids, function(o) v %in% (sav_vars[[o]] %||% character(0)), logical(1))]
          # .sav 也是「本檔沒有、別檔有」：上面已比過
          if (!(v %in% (sav_vars[[d]] %||% character(0))) && length(sav_yes)) next
          uid <- dv$item_uid[match(v, tolower(dv$varname))]
          unexpl[[length(unexpl) + 1L]] <- data.frame(
            formID = main$formID[grp == g][1], revision = main$revision[grp == g][1],
            datasetID = d, varname = v, item_uid = ifelse(is.na(uid), "", uid),
            block = ifelse(is.na(uid), "", kit$items$block[match(uid, kit$items$item_uid)]),
            present_in = paste(yes, collapse = ","), source = "crosswalk",
            # 原文：對照表：%s 有、本檔沒有；.sav 表頭：有此變項的檔 = %s
            note = sprintf("\u5c0d\u7167\u8868\uff1a%s \u6709\u3001\u672c\u6a94\u6c92\u6709\uff1b.sav \u8868\u982d\uff1a\u6709\u6b64\u8b8a\u9805\u7684\u6a94 = %s",
                           paste(yes, collapse = ","),
                           # 原文：（都沒有）
                           if (length(sav_yes)) paste(sav_yes, collapse = ",") else "\uff08\u90fd\u6c92\u6709\uff09"),
            stringsAsFactors = FALSE)
        }
      }
    }
  }

  ux <- if (length(unexpl)) do.call(rbind, unexpl) else data.frame(
    formID = character(0), revision = character(0), datasetID = character(0),
    varname = character(0), item_uid = character(0), block = character(0),
    present_in = character(0), source = character(0), note = character(0),
    stringsAsFactors = FALSE)
  # 有區塊列可以解釋的拿掉
  if (nrow(ux)) {
    ux <- ux[!(nzchar(ux$block) & paste(ux$datasetID, ux$block) %in% paste(bs$datasetID, bs$block)), , drop = FALSE]
    rownames(ux) <- NULL
  }
  pr <- if (length(pairs)) do.call(rbind, pairs) else data.frame(
    kind = character(0), formID = character(0), revision = character(0), datasets = character(0),
    stringsAsFactors = FALSE)

  bs_out <- cbind(conform_kit_table(bs, "block_status"),
                  confidence = as.character(bs$confidence %||% character(0)),
                  basis = as.character(bs$basis %||% character(0)), stringsAsFactors = FALSE)
  list(block_status = bs_out, unexplained = ux, pairs = pr)
}
