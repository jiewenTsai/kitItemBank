# R/codebook.R — 施測（codebook）的資料：一次施測有哪些資料檔、每個檔的變項照問卷順序
#
# 只用新的 kit（17 張表，見 R/schema.R），沒有 Shiny：
#   resolve_admin()      代號或別名（M036大、M36W84、D00250、84M）→ adminID
#   admin_summary()      施測清單：每次施測一列
#   admin_codebook()     一次施測：每個資料檔變項一列（照檔案順序），加上區塊狀態
#   codebook_workbook()  「施測題目清單」Excel 的各張表（第一張是說明）
#   codebook_sps()       一個資料檔的 SPSS 標籤語法
# 頁面在 R/page_admin.R。

# 原文：已釋出 | 內部 | 規劃中
CB_STATUS_LABEL <- c(released = "\u5df2\u91cb\u51fa", internal = "\u5167\u90e8", planned = "\u898f\u5283\u4e2d")
# 原文：題目 | 系統 | 權重 | 衍生
CB_KIND_LABEL <- c(item = "\u984c\u76ee", system = "\u7cfb\u7d71", weight = "\u6b0a\u91cd", derived = "\u884d\u751f")
# 原文：家長 | 教師 | 兒童 | 系統
CB_INSTRUMENT_LABEL <- c(P = "\u5bb6\u9577", T = "\u6559\u5e2b", C = "\u5152\u7ae5", S = "\u7cfb\u7d71")
# 原文：小一 | 小二 | 小三 | 小四 | 小五 | 小六 | 國一 | 國二 | 國三
CB_GRADE_LABEL <- c("\u5c0f\u4e00", "\u5c0f\u4e8c", "\u5c0f\u4e09", "\u5c0f\u56db", "\u5c0f\u4e94", "\u5c0f\u516d", "\u570b\u4e00", "\u570b\u4e8c", "\u570b\u4e09")
# 題庫資料夾裡沒有寫版本時用這個
# 原文：2026.1 草稿
CB_DRAFT_VERSION <- "2026.1 \u8349\u7a3f"

# SPSS 的上限：一行 255 bytes 以內；變項標籤 255 bytes、選項標籤 120 bytes
CB_SPS_LINE_BYTES <- 255L
CB_SPS_CHUNK_BYTES <- 200L
CB_SPS_VAR_LABEL_BYTES <- 255L
CB_SPS_VALUE_LABEL_BYTES <- 120L

# 小工具 ----

cb_blank <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

cb_int <- function(x) suppressWarnings(as.integer(x))

# 換行、tab 變成一個空白
cb_one_line <- function(x) trimws(gsub("[\r\n\t]+", " ", cb_blank(x)))

#' 題庫版本：kit 資料夾的 VERSION（第一行）或 datapackage.json 的 version；都沒有就是草稿
#' @noRd
codebook_bank_version <- function(dir = kit_dir()) {
  if (length(dir) == 1L && nzchar(dir)) {
    f <- file.path(dir, "BANK_VERSION")
    if (file.exists(f)) {
      v <- trimws(readLines(f, n = 1L, warn = FALSE, encoding = "UTF-8"))
      if (length(v) && nzchar(v)) return(v)
    }
    f <- file.path(dir, "datapackage.json")
    if (file.exists(f)) {
      txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = " ")
      m <- regmatches(txt, regexpr("\"version\"\\s*:\\s*\"[^\"]+\"", txt))
      if (length(m)) return(sub("^.*:\\s*\"([^\"]+)\"$", "\\1", m))
    }
  }
  CB_DRAFT_VERSION
}

# "2019-03-01" → "2019.03"
cb_ym <- function(d) {
  d <- cb_blank(d)
  ifelse(nzchar(d), gsub("-", ".", substr(d, 1L, 7L), fixed = TRUE), "")
}

# 調查期間：2019.03–2020.08（沒有日期就空白）
cb_period <- function(from, to) {
  a <- cb_ym(from)
  b <- cb_ym(to)
  # 原文：–
  ifelse(!nzchar(a) & !nzchar(b), "", paste0(a, "\u2013", b))
}

# 調查期間，給人看的長寫法：2019 年 3 月到 2020 年 8 月
cb_period_words <- function(from, to) {
  w <- function(d) {
    d <- cb_blank(d)
    if (!nzchar(d)) return("")
    y <- substr(d, 1L, 4L)
    m <- cb_int(substr(d, 6L, 7L))
    # 原文： 年 |  年  |  月
    if (is.na(m)) paste0(y, " \u5e74") else paste0(y, " \u5e74 ", m, " \u6708")
  }
  a <- w(from)
  b <- w(to)
  if (!nzchar(a) && !nzchar(b)) return("")
  # 原文：？ | 到  | ？
  paste0(if (nzchar(a)) a else "\uff1f", "\u5230 ", if (nzchar(b)) b else "\uff1f")
}

cb_grade_label <- function(g) {
  g <- cb_int(g)
  out <- ifelse(!is.na(g) & g >= 1L & g <= length(CB_GRADE_LABEL),
                CB_GRADE_LABEL[pmax(1L, pmin(g, length(CB_GRADE_LABEL)))], "")
  out[is.na(out)] <- ""
  out
}

cb_status_label <- function(s) {
  s <- cb_blank(s)
  out <- unname(CB_STATUS_LABEL[s])
  ifelse(is.na(out), s, out)
}

# 施測的名稱：「M03 世代 36 月齡（2019.03–2020.08）」；國小以後加年級；內部、規劃中寫在括號裡
admin_label <- function(kit, adminID) {
  if (length(adminID) == 0L) return(character(0))
  ad <- kit$admins[match(adminID, kit$admins$adminID), , drop = FALSE]
  co <- cb_blank(kit$cohorts$code[match(ad$cohortID, kit$cohorts$cohortID)])
  co <- ifelse(nzchar(co), co, cb_blank(ad$cohortID))
  grade <- cb_grade_label(ad$grade)
  period <- cb_period(ad$surveyFrom, ad$surveyTo)
  st <- cb_blank(ad$status)
  extra <- ifelse(st %in% c("internal", "planned"), cb_status_label(st), "")
  # 原文：，
  paren <- ifelse(nzchar(period) & nzchar(extra), paste0(period, "\uff0c", extra),
                  paste0(period, extra))
  # 原文： 世代  |  月齡
  out <- paste0(co, " \u4e16\u4ee3 ", cb_blank(ad$ageMonth), " \u6708\u9f61",
                # 原文：・
                ifelse(nzchar(grade), paste0("\u30fb", grade), ""),
                # 原文：（ | ）
                ifelse(nzchar(paren), paste0("\uff08", paren, "\uff09"), ""))
  out[is.na(ad$adminID)] <- adminID[is.na(ad$adminID)]
  out
}

# 資料檔是誰填的：填答者；沒寫就用問卷大類（家長、教師、兒童、系統）
cb_respondent <- function(ds) {
  r <- cb_blank(ds$respondent)
  ins <- unname(CB_INSTRUMENT_LABEL[cb_blank(ds$instrument)])
  ifelse(nzchar(r), r, ifelse(is.na(ins), cb_blank(ds$instrument), ins))
}

# 資料檔的名稱：「家長（data1）」
cb_dataset_label <- function(ds) {
  if (nrow(ds) == 0L) return(character(0))
  # 原文：（ | ）
  paste0(cb_respondent(ds), "\uff08", cb_blank(ds$file), "\uff09")
}

# 一套遺漏代碼方案：照代碼排；"7777=拒答; 8888=不知道"
cb_scheme_rows <- function(kit, scheme) {
  ms <- kit$missing_schemes[kit$missing_schemes$scheme %in% scheme, , drop = FALSE]
  ms[order(ms$scheme, suppressWarnings(as.numeric(ms$code)), ms$code), , drop = FALSE]
}
cb_scheme_text <- function(kit, scheme, sep = "; ", eq = "=") {
  ms <- cb_scheme_rows(kit, scheme)
  if (nrow(ms) == 0L) return("")
  paste0(ms$code, eq, ms$label, collapse = sep)
}

# 每個選項組一串："1=有; 2=沒有"（照 order，再照代碼）
cb_value_text <- function(kit, ids) {
  ids <- cb_blank(ids)
  vc <- kit$value_codes[kit$value_codes$valueSetID %in% unique(ids[nzchar(ids)]), , drop = FALSE]
  if (nrow(vc) == 0L) return(rep("", length(ids)))
  vc <- vc[order(vc$valueSetID, cb_int(vc$order), suppressWarnings(as.numeric(vc$code)), vc$code), ,
           drop = FALSE]
  txt <- vapply(split(paste0(vc$code, "=", cb_one_line(vc$label)), vc$valueSetID),
                paste, character(1), collapse = "; ")
  out <- unname(txt[ids])
  out[is.na(out)] <- ""
  out
}

# 代號或別名 ----

#' Find administrations by ID or alias
#'
#' Matches `x` against `adminID` and against the `aliases` table (`target_type = "admin"`),
#' ignoring case and surrounding spaces. Spoken aliases can point to more than one
#' administration (for example `36M` is both M03M036 and M36M036).
#'
#' @param kit A kit list, e.g. from [read_kit()].
#' @param x One or more strings: an adminID or an alias such as `M036大`, `M36W84`, `D00250`,
#'   `84M`.
#' @return The matching adminIDs (in the order of the `admins` table); `character(0)` when none.
#' @examples
#' kit <- read_kit()
#' resolve_admin(kit, "M36W84")
#' resolve_admin(kit, "36m")
#' @export
resolve_admin <- function(kit, x) {
  ids <- cb_blank(kit$admins$adminID)
  x <- trimws(cb_blank(x))
  x <- x[nzchar(x)]
  if (length(x) == 0L || length(ids) == 0L) return(character(0))
  key <- tolower(x)
  al <- kit$aliases[kit$aliases$target_type == "admin", , drop = FALSE]
  hit <- c(ids[tolower(ids) %in% key], al$target_id[tolower(trimws(al$alias)) %in% key])
  ids[ids %in% hit]
}

# 施測清單 ----

#' One row per administration
#'
#' @param kit A kit list, e.g. from [read_kit()].
#' @return A data frame: `adminID`, `cohort` (cohort code, e.g. M03), `ageMonth`, `grade`,
#'   `period` (e.g. "2019.03–2020.08"), `status`, `status_label` (in Chinese), `release`
#'   (SRDA release ID), `n_datasets`, `n_item_vars` (item variables over all data files),
#'   `n_items` (distinct items), `label` (a readable name for the administration).
#'   Sorted by cohort code and age.
#' @examples
#' admin_summary(read_kit())
#' @export
admin_summary <- function(kit) {
  ad <- kit$admins
  ds <- kit$datasets
  dv <- kit$dataset_vars
  dv_admin <- ds$adminID[match(dv$datasetID, ds$datasetID)]
  is_item <- dv$kind == "item"
  ids <- ad$adminID
  out <- data.frame(
    adminID = ids,
    cohort = cb_blank(kit$cohorts$code[match(ad$cohortID, kit$cohorts$cohortID)]),
    ageMonth = cb_int(ad$ageMonth),
    grade = cb_blank(ad$grade),
    period = cb_period(ad$surveyFrom, ad$surveyTo),
    status = cb_blank(ad$status),
    status_label = cb_status_label(ad$status),
    release = cb_blank(ad$datasetRelease),
    n_datasets = vapply(ids, function(a) sum(ds$adminID == a), integer(1), USE.NAMES = FALSE),
    n_item_vars = vapply(ids, function(a) sum(is_item & dv_admin %in% a), integer(1),
                         USE.NAMES = FALSE),
    n_items = vapply(ids, function(a) {
      length(unique(dv$item_uid[is_item & dv_admin %in% a & nzchar(dv$item_uid)]))
    }, integer(1), USE.NAMES = FALSE),
    label = admin_label(kit, ids),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$cohort, out$ageMonth, out$adminID), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# 一次施測的 codebook ----

#' The codebook of one administration
#'
#' One row per data-file variable, in file order (`order`; variables without an order keep
#' the kit's order at the end).
#'
#' @param kit A kit list, e.g. from [read_kit()].
#' @param adminID One adminID (use [resolve_admin()] for aliases).
#' @param datasetID Optional: only these data files of the administration.
#' @return A list:
#'   - `admin`: the `admins` row;
#'   - `datasets`: the administration's data files, with `respondent_label` and
#'     `dataset_label` added;
#'   - `vars`: one row per variable: `datasetID`, `respondent`, `order`, `position`,
#'     `varname`, `label` (item version label; blank for system variables), `item_uid`,
#'     `itemID` (item_uid without the questionnaire prefix), `itemVer`, `kind`, `block`,
#'     `groups` (group labels of the data file's form, joined with "、"), `valueSetID`,
#'     `values` ("1=…; 2=…"), `missing` (the data file's missing-code scheme, "7777=…; …"),
#'     `public` (`FALSE` = asked but not in the public file);
#'   - `block_status`: the `block_status` rows of these data files, with `respondent` and
#'     `asked_label` added.
#' @examples
#' cb <- admin_codebook(read_kit(), "M03M036")
#' head(cb$vars)
#' @export
admin_codebook <- function(kit, adminID, datasetID = NULL) {
  adminID <- cb_blank(adminID)
  if (length(adminID) != 1L || !adminID %in% kit$admins$adminID) {
    stop("Unknown adminID: ", paste(adminID, collapse = ", "),
         ". Use resolve_admin() for aliases.", call. = FALSE)
  }
  ds <- kit$datasets[kit$datasets$adminID == adminID, , drop = FALSE]
  if (!is.null(datasetID)) ds <- ds[ds$datasetID %in% datasetID, , drop = FALSE]
  ds$respondent_label <- cb_respondent(ds)
  ds$dataset_label <- cb_dataset_label(ds)
  rownames(ds) <- NULL

  vars <- lapply(seq_len(nrow(ds)), function(i) cb_dataset_vars(kit, ds[i, , drop = FALSE]))
  vars <- if (length(vars)) do.call(rbind, vars) else cb_dataset_vars(kit, ds[0, , drop = FALSE])
  rownames(vars) <- NULL

  bs <- kit$block_status[kit$block_status$datasetID %in% ds$datasetID, , drop = FALSE]
  bs <- bs[order(match(bs$datasetID, ds$datasetID), bs$block), , drop = FALSE]
  bs$respondent <- ds$respondent_label[match(bs$datasetID, ds$datasetID)]
  bs$asked_label <- cb_asked_label(bs$asked)
  rownames(bs) <- NULL

  list(admin = kit$admins[kit$admins$adminID == adminID, , drop = FALSE],
       datasets = ds, vars = vars, block_status = bs)
}

# 一個資料檔的變項（ds 是 datasets 的一列；0 列時回傳空表）
cb_dataset_vars <- function(kit, ds) {
  dv <- kit$dataset_vars[kit$dataset_vars$datasetID %in% ds$datasetID, , drop = FALSE]
  ord <- cb_int(dv$order)
  dv <- dv[order(ord, seq_len(nrow(dv)), na.last = TRUE), , drop = FALSE]
  ord <- cb_int(dv$order)

  key <- paste(dv$item_uid, dv$itemVer, sep = "|")
  iv <- kit$item_versions
  ver <- iv[match(key, paste(iv$item_uid, iv$itemVer, sep = "|")), , drop = FALSE]
  has_item <- nzchar(dv$item_uid)
  it <- kit$items[match(dv$item_uid, kit$items$item_uid), , drop = FALSE]

  # 題組：這個資料檔的題本裡，這一題屬於哪些題組
  form <- cb_blank(ds$formID)[1]
  gm <- kit$group_members[kit$group_members$formID %in% form, , drop = FALSE]
  glab <- kit$groups$label[match(gm$groupID, kit$groups$groupID)]
  glab <- ifelse(is.na(glab) | !nzchar(glab), gm$groupID, glab)
  gtxt <- if (nrow(gm)) {
    # 原文：、
    vapply(split(glab, gm$item_uid), function(v) paste(unique(v), collapse = "\u3001"), character(1))
  } else character(0)
  groups <- unname(gtxt[dv$item_uid])
  groups[is.na(groups) | !has_item] <- ""

  vs <- cb_blank(ver$valueSetID)
  scheme <- ds$missingScheme[match(dv$datasetID, ds$datasetID)]
  miss <- vapply(unique(scheme), function(s) cb_scheme_text(kit, s), character(1))

  data.frame(
    datasetID = dv$datasetID,
    respondent = cb_respondent(ds)[match(dv$datasetID, ds$datasetID)],
    order = ord,
    position = dv$position,
    varname = dv$varname,
    label = ifelse(has_item, cb_blank(ver$label), ""),
    item_uid = dv$item_uid,
    itemID = sub("^[A-Za-z]+\\.", "", dv$item_uid),
    itemVer = dv$itemVer,
    kind = dv$kind,
    block = ifelse(has_item, cb_blank(it$block), ""),
    groups = groups,
    valueSetID = vs,
    values = cb_value_text(kit, vs),
    missing = unname(miss[cb_blank(scheme)]),
    public = dv$public != "0",
    stringsAsFactors = FALSE
  )
}

cb_asked_label <- function(asked) {
  # 原文：全部有問 | 只問部分題目 | 由前一波帶入 | 沒問
  lab <- c(all = "\u5168\u90e8\u6709\u554f", subset = "\u53ea\u554f\u90e8\u5206\u984c\u76ee", carried = "\u7531\u524d\u4e00\u6ce2\u5e36\u5165", none = "\u6c92\u554f")
  out <- unname(lab[cb_blank(asked)])
  ifelse(is.na(out), cb_blank(asked), out)
}

#' 區塊狀態寫成白話：「親友版沒問：孩子與家庭基本資料、托育狀況調查問卷資料」
#' @param cb admin_codebook() 的回傳
#' @noRd
admin_block_sentences <- function(cb) {
  bs <- cb$block_status
  if (nrow(bs) == 0L) return(character(0))
  ds <- cb$datasets
  out <- character(0)
  for (d in unique(bs$datasetID)) {
    b <- bs[bs$datasetID == d, , drop = FALSE]
    r <- ds[ds$datasetID == d, , drop = FALSE]
    # 原文：版 |  檔
    who <- paste0(r$respondent_label, if (nzchar(cb_blank(r$respondent))) "\u7248" else " \u6a94")
    for (a in c("none", "subset", "carried", "all")) {
      x <- b[b$asked == a, , drop = FALSE]
      if (nrow(x) == 0L) next
      blocks <- if (a == "carried") {
        # 原文：（ | ）
        ifelse(nzchar(cb_blank(x$who)), paste0(x$block, "\uff08", x$who, "\uff09"), x$block)
      } else x$block
      # 原文：沒問 | 只問部分題目 | 由前一波帶入
      verb <- switch(a, none = "\u6c92\u554f", subset = "\u53ea\u554f\u90e8\u5206\u984c\u76ee", carried = "\u7531\u524d\u4e00\u6ce2\u5e36\u5165",
                     # 原文：全部有問
                     all = "\u5168\u90e8\u6709\u554f")
      # 原文：： | 、
      out <- c(out, paste0(who, verb, "\uff1a", paste(blocks, collapse = "\u3001")))
    }
  }
  out
}

# 下載：施測題目清單 Excel ----

#' The sheets of the 施測題目清單 Excel file
#'
#' The first sheet, 說明, says what the file is, the bank version, the date, the
#' administration, the missing-code scheme(s), the block statuses and how to cite; then one
#' sheet per data file, variables in file order.
#'
#' @param kit A kit list, e.g. from [read_kit()].
#' @param adminID One adminID.
#' @param version Bank version written on the 說明 sheet.
#' @param date Date written on the 說明 sheet.
#' @return A named list of data frames, ready for `writexl::write_xlsx()`.
#' @examples
#' sheets <- codebook_workbook(read_kit(), "M03M036")
#' names(sheets)
#' @export
codebook_workbook <- function(kit, adminID, version = codebook_bank_version(),
                              date = kit_today()) {
  cb <- admin_codebook(kit, adminID)
  ad <- cb$admin
  ds <- cb$datasets
  v <- cb$vars
  info_row <- function(k, x) data.frame(k = rep(k, length(x)), x = as.character(x),
                                   stringsAsFactors = FALSE)
  schemes <- unique(cb_blank(ds$missingScheme))
  schemes <- schemes[nzchar(schemes)]
  n_vars <- vapply(ds$datasetID, function(d) sum(v$datasetID == d), integer(1))
  n_hidden <- vapply(ds$datasetID, function(d) sum(v$datasetID == d & !v$public), integer(1))
  release <- cb_blank(ad$datasetRelease)
  info <- rbind(
    # 原文：這個檔案 | KIT 幼兒發展調查資料庫一次施測的題目清單（codebook）：每個資料檔一張表，變項照問卷順序排列。
    info_row("\u9019\u500b\u6a94\u6848", "KIT \u5e7c\u5152\u767c\u5c55\u8abf\u67e5\u8cc7\u6599\u5eab\u4e00\u6b21\u65bd\u6e2c\u7684\u984c\u76ee\u6e05\u55ae\uff08codebook\uff09\uff1a\u6bcf\u500b\u8cc7\u6599\u6a94\u4e00\u5f35\u8868\uff0c\u8b8a\u9805\u7167\u554f\u5377\u9806\u5e8f\u6392\u5217\u3002"),
    # 原文：題庫版本
    info_row("\u984c\u5eab\u7248\u672c", version),
    # 原文：產生日期
    info_row("\u7522\u751f\u65e5\u671f", format(as.Date(date), "%Y-%m-%d")),
    # 原文：施測 | ，代號 
    info_row("\u65bd\u6e2c", paste0(admin_label(kit, ad$adminID), "\uff0c\u4ee3\u865f ", ad$adminID)),
    # 原文：調查期間
    info_row("\u8abf\u67e5\u671f\u9593", cb_period_words(ad$surveyFrom, ad$surveyTo)),
    # 原文：狀態
    info_row("\u72c0\u614b", paste0(cb_status_label(ad$status),
                       # 原文：，釋出編號 
                       if (nzchar(release)) paste0("\uff0c\u91cb\u51fa\u7de8\u865f ", release) else "")),
    # 原文：資料檔 | ： | ， |  個變項
    info_row("\u8cc7\u6599\u6a94", paste0(ds$datasetID, "\uff1a", ds$dataset_label, "\uff0c", n_vars, " \u500b\u8b8a\u9805",
                        # 原文：（其中  |  個公開檔沒有）
                        ifelse(n_hidden > 0L, paste0("\uff08\u5176\u4e2d ", n_hidden, " \u500b\u516c\u958b\u6a94\u6c92\u6709\uff09"), ""),
                        ifelse(nzchar(cb_blank(ds$missingScheme)),
                               # 原文：，遺漏代碼用方案 
                               paste0("\uff0c\u907a\u6f0f\u4ee3\u78bc\u7528\u65b9\u6848 ", ds$missingScheme), ""))),
    # 原文：遺漏代碼（方案  | ）
    if (length(schemes)) info_row(paste0("\u907a\u6f0f\u4ee3\u78bc\uff08\u65b9\u6848 ", schemes, "\uff09"),
                             vapply(schemes, function(s) cb_scheme_text(kit, s), character(1))),
    # 原文：遺漏代碼是什麼 | 沒有有效答案時填的代碼，例如拒答、不知道、跳答。分析前要先設成遺漏值（SPSS 標籤語法 .sps 會幫你設）。
    info_row("\u907a\u6f0f\u4ee3\u78bc\u662f\u4ec0\u9ebc", "\u6c92\u6709\u6709\u6548\u7b54\u6848\u6642\u586b\u7684\u4ee3\u78bc\uff0c\u4f8b\u5982\u62d2\u7b54\u3001\u4e0d\u77e5\u9053\u3001\u8df3\u7b54\u3002\u5206\u6790\u524d\u8981\u5148\u8a2d\u6210\u907a\u6f0f\u503c\uff08SPSS \u6a19\u7c64\u8a9e\u6cd5 .sps \u6703\u5e6b\u4f60\u8a2d\uff09\u3002"),
    # 原文：區塊狀態
    if (nrow(cb$block_status)) info_row("\u5340\u584a\u72c0\u614b", admin_block_sentences(cb)),
    # 原文：欄位：順序 | 這個變項在資料檔裡的順序。
    info_row("\u6b04\u4f4d\uff1a\u9806\u5e8f", "\u9019\u500b\u8b8a\u9805\u5728\u8cc7\u6599\u6a94\u88e1\u7684\u9806\u5e8f\u3002"),
    # 原文：欄位：題本位置 | 這一題在紙本問卷上的位置，例如「四.33」是第四部分第 33 題。
    info_row("\u6b04\u4f4d\uff1a\u984c\u672c\u4f4d\u7f6e", "\u9019\u4e00\u984c\u5728\u7d19\u672c\u554f\u5377\u4e0a\u7684\u4f4d\u7f6e\uff0c\u4f8b\u5982\u300c\u56db.33\u300d\u662f\u7b2c\u56db\u90e8\u5206\u7b2c 33 \u984c\u3002"),
    # 原文：欄位：題號、版本 | 題庫裡這一題的代號和版本，可以在「題項查詢」找到。
    info_row("\u6b04\u4f4d\uff1a\u984c\u865f\u3001\u7248\u672c", "\u984c\u5eab\u88e1\u9019\u4e00\u984c\u7684\u4ee3\u865f\u548c\u7248\u672c\uff0c\u53ef\u4ee5\u5728\u300c\u984c\u9805\u67e5\u8a62\u300d\u627e\u5230\u3002"),
    # 原文：欄位：公開檔 | 「沒有」表示調查有問，但公開釋出的資料檔裡沒有這個變項。
    info_row("\u6b04\u4f4d\uff1a\u516c\u958b\u6a94", "\u300c\u6c92\u6709\u300d\u8868\u793a\u8abf\u67e5\u6709\u554f\uff0c\u4f46\u516c\u958b\u91cb\u51fa\u7684\u8cc7\u6599\u6a94\u88e1\u6c92\u6709\u9019\u500b\u8b8a\u9805\u3002"),
    # 原文：建議引用
    if (nzchar(cb_blank(ad$citation_zh))) info_row("\u5efa\u8b70\u5f15\u7528", ad$citation_zh),
    if (nzchar(cb_blank(ad$citation_en))) info_row("Citation", ad$citation_en)
  )
  # 原文：項目 | 內容
  names(info) <- c("\u9805\u76ee", "\u5167\u5bb9")
  rownames(info) <- NULL

  sheets <- list(info)
  # 原文：說明
  sheet_names <- "\u8aaa\u660e"
  for (i in seq_len(nrow(ds))) {
    x <- v[v$datasetID == ds$datasetID[i], , drop = FALSE]
    kind <- unname(CB_KIND_LABEL[x$kind])
    sheets[[length(sheets) + 1L]] <- stats::setNames(data.frame(
      ifelse(is.na(x$order), "", as.character(x$order)), x$position, x$varname,
      cb_one_line(x$label), x$block, x$groups, x$values,
      # 原文：有 | 沒有
      ifelse(x$public, "\u6709", "\u6c92\u6709"), ifelse(is.na(kind), x$kind, kind),
      x$item_uid, x$itemVer,
      stringsAsFactors = FALSE
    # 原文：順序 | 題本位置 | 變項名稱 | 題目 | 區塊 | 題組 | 選項 | 公開檔 | 種類
    ), c("\u9806\u5e8f", "\u984c\u672c\u4f4d\u7f6e", "\u8b8a\u9805\u540d\u7a31", "\u984c\u76ee", "\u5340\u584a", "\u984c\u7d44", "\u9078\u9805", "\u516c\u958b\u6a94", "\u7a2e\u985e",
         # 原文：題號 | 版本
         "\u984c\u865f", "\u7248\u672c"))
    sheet_names <- c(sheet_names, cb_sheet_name(paste(ds$datasetID[i], ds$respondent_label[i]),
                                                sheet_names))
  }
  stats::setNames(sheets, sheet_names)
}

# Excel 工作表名稱：不能有 []:*?/\，最多 31 個字，不能重複
cb_sheet_name <- function(x, taken) {
  x <- gsub("[\\[\\]:*?/\\\\]", "_", x, perl = TRUE)
  x <- substr(x, 1L, 31L)
  base <- x
  k <- 1L
  while (x %in% taken) {
    k <- k + 1L
    suf <- paste0(" (", k, ")")
    x <- paste0(substr(base, 1L, 31L - nchar(suf)), suf)
  }
  x
}

# 下載：SPSS 標籤語法 ----

#' SPSS label syntax for one data file
#'
#' `VARIABLE LABELS` for every variable with a label, `VALUE LABELS` (the item's value codes
#' plus the missing-code scheme) and `MISSING VALUES` (the scheme's codes) for items with value
#' codes. Variables not in the public file are left out, so the syntax runs on the public file.
#' Strings use SPSS quoting (`'` doubled), long labels are cut to SPSS's limits (255 bytes for
#' variable labels, 120 for value labels) and split with `+` so no line is longer than 255 bytes.
#'
#' @param kit A kit list, e.g. from [read_kit()].
#' @param datasetID One data file.
#' @param version Bank version written in the header comment.
#' @param date Date written in the header comment.
#' @return One string (UTF-8, lines separated by `\n`), ending with `EXECUTE.`. Write it with
#'   `writeLines(x, file, useBytes = TRUE)`.
#' @examples
#' cat(substr(codebook_sps(read_kit(), "M03M036-P"), 1, 600))
#' @export
codebook_sps <- function(kit, datasetID, version = codebook_bank_version(), date = kit_today()) {
  ds <- kit$datasets[kit$datasets$datasetID %in% datasetID, , drop = FALSE]
  if (length(datasetID) != 1L || nrow(ds) != 1L) {
    stop("Unknown datasetID: ", paste(datasetID, collapse = ", "), call. = FALSE)
  }
  v <- admin_codebook(kit, ds$adminID, ds$datasetID)$vars
  v <- v[v$public, , drop = FALSE]
  scheme <- cb_blank(ds$missingScheme)
  ms <- cb_scheme_rows(kit, scheme)

  lab_v <- v[nzchar(cb_one_line(v$label)), , drop = FALSE]
  val_v <- v[v$kind == "item" & nzchar(v$valueSetID), , drop = FALSE]

  # 開頭的說明（SPSS 的註解：* 開頭、句點結尾）
  head <- c(
    "* Encoding: UTF-8.",
    # 原文：* KIT 題庫  | ： | ， |  的 SPSS 標籤語法.
    paste0("* KIT \u984c\u5eab ", version, "\uff1a", ds$datasetID, "\uff0c", cb_dataset_label(ds), " \u7684 SPSS \u6a19\u7c64\u8a9e\u6cd5."),
    # 原文：* 產生日期 
    paste0("* \u7522\u751f\u65e5\u671f ", format(as.Date(date), "%Y-%m-%d"),
           # 原文：。只放公開檔有的變項；選項標籤和遺漏值只設在有選項的題目.
           "\u3002\u53ea\u653e\u516c\u958b\u6a94\u6709\u7684\u8b8a\u9805\uff1b\u9078\u9805\u6a19\u7c64\u548c\u907a\u6f0f\u503c\u53ea\u8a2d\u5728\u6709\u9078\u9805\u7684\u984c\u76ee."),
    # 原文：* 遺漏代碼方案  | ：
    if (nrow(ms)) paste0("* \u907a\u6f0f\u4ee3\u78bc\u65b9\u6848 ", scheme, "\uff1a",
                        cb_scheme_text(kit, scheme, sep = ", "), ".")
  )
  head <- vapply(head, function(h) {
    if (nchar(h, "bytes") <= CB_SPS_LINE_BYTES) return(h)
    paste0(kitDesign::kit_truncate_bytes(h, CB_SPS_LINE_BYTES - 1L), ".")
  }, character(1), USE.NAMES = FALSE)

  out <- c(head, "")

  if (nrow(lab_v)) {
    out <- c(out, "VARIABLE LABELS")
    for (i in seq_len(nrow(lab_v))) {
      chunks <- cb_sps_string(cb_one_line(lab_v$label[i]), CB_SPS_VAR_LABEL_BYTES)
      pre <- paste0("  ", if (i > 1L) "/" else " ", lab_v$varname[i], " ")
      out <- c(out, cb_sps_join(pre, chunks))
    }
    out[length(out)] <- paste0(out[length(out)], ".")
    out <- c(out, "")
  }

  if (nrow(val_v)) {
    vc <- kit$value_codes[kit$value_codes$valueSetID %in% val_v$valueSetID, , drop = FALSE]
    vc <- vc[order(vc$valueSetID, cb_int(vc$order), suppressWarnings(as.numeric(vc$code)),
                   vc$code), , drop = FALSE]
    out <- c(out, "VALUE LABELS")
    for (i in seq_len(nrow(val_v))) {
      x <- vc[vc$valueSetID == val_v$valueSetID[i], , drop = FALSE]
      m <- ms[!ms$code %in% x$code, , drop = FALSE]
      codes <- c(x$code, m$code)
      labs <- c(cb_one_line(x$label), cb_one_line(m$label))
      out <- c(out, paste0("  ", if (i > 1L) "/" else " ", val_v$varname[i]))
      for (j in seq_along(codes)) {
        chunks <- cb_sps_string(labs[j], CB_SPS_VALUE_LABEL_BYTES)
        out <- c(out, cb_sps_join(paste0("    ", cb_sps_code(codes[j]), " "), chunks))
      }
    }
    out[length(out)] <- paste0(out[length(out)], ".")
    out <- c(out, "")
  }

  miss_num <- suppressWarnings(as.numeric(ms$code))
  miss_num <- sort(unique(miss_num[!is.na(miss_num)]))
  if (nrow(val_v) && length(miss_num)) {
    spec <- cb_sps_missing_spec(miss_num)
    # 有效的選項代碼剛好落在遺漏值的範圍裡：提醒一下
    vc_num <- suppressWarnings(as.numeric(vc$code))
    bad_sets <- unique(vc$valueSetID[!is.na(vc_num) & vc_num >= spec$from & vc_num <= spec$to &
                                       !vc_num %in% miss_num])
    bad <- val_v$varname[val_v$valueSetID %in% bad_sets]
    if (length(bad)) {
      # 原文：* 注意：這些變項有選項代碼落在遺漏值範圍裡，請自己檢查：
      note <- cb_sps_wrap(paste0("* \u6ce8\u610f\uff1a\u9019\u4e9b\u8b8a\u9805\u6709\u9078\u9805\u4ee3\u78bc\u843d\u5728\u907a\u6f0f\u503c\u7bc4\u570d\u88e1\uff0c\u8acb\u81ea\u5df1\u6aa2\u67e5\uff1a",
                                 paste(bad, collapse = " "), "."), prefix = "  ")
      # 註解從第一欄開始，接下來的行縮排
      note[1] <- sub("^  ", "", note[1])
      out <- c(out, note)
    }
    out <- c(out, "MISSING VALUES", cb_sps_wrap(val_v$varname, prefix = "  "),
             paste0("  ", spec$text, "."), "")
  }

  out <- c(out, "EXECUTE.")
  paste(out, collapse = "\n")
}

# 一段標籤文字 → 一串 '…' 片段（每段跳脫後最多 CB_SPS_CHUNK_BYTES bytes）
cb_sps_string <- function(x, max_bytes) {
  x <- kitDesign::kit_truncate_bytes(cb_one_line(x), max_bytes)
  if (!nzchar(x)) return("''")
  ch <- strsplit(x, "")[[1]]
  w <- nchar(ch, "bytes") + (ch == "'")
  grp <- integer(length(ch))
  g <- 1L
  used <- 0L
  for (k in seq_along(ch)) {
    if (used + w[k] > CB_SPS_CHUNK_BYTES) {
      g <- g + 1L
      used <- 0L
    }
    grp[k] <- g
    used <- used + w[k]
  }
  vapply(split(ch, grp), function(p) {
    paste0("'", gsub("'", "''", paste(p, collapse = ""), fixed = TRUE), "'")
  }, character(1), USE.NAMES = FALSE)
}

# 第一段接在 pre 後面，其他段另起一行用 + 接上
cb_sps_join <- function(pre, chunks) {
  c(paste0(pre, chunks[1]), if (length(chunks) > 1L) paste0("     + ", chunks[-1]))
}

# 代碼：數字照寫，其他加引號
cb_sps_code <- function(code) {
  if (grepl("^-?[0-9]+(\\.[0-9]+)?$", code)) code else paste0("'", gsub("'", "''", code), "'")
}

# SPSS 的遺漏值最多 3 個，或一個範圍加一個值：代碼多於 3 個時，用最窄的範圍加一個值
cb_sps_missing_spec <- function(codes) {
  fmt <- function(x) format(x, scientific = FALSE, trim = TRUE)
  n <- length(codes)
  if (n <= 3L) {
    return(list(text = paste0("(", paste(fmt(codes), collapse = ", "), ")"),
                from = Inf, to = -Inf))
  }
  # 範圍包住 n - 1 個連續的代碼，剩下一個（最小或最大）另外列
  low <- list(from = codes[2], to = codes[n], one = codes[1])
  high <- list(from = codes[1], to = codes[n - 1L], one = codes[n])
  best <- if ((low$to - low$from) <= (high$to - high$from)) low else high
  list(text = paste0("(", fmt(best$from), " THRU ", fmt(best$to), ", ", fmt(best$one), ")"),
       from = best$from, to = best$to)
}

# 一串字（或一個長句）折成每行不超過 200 bytes
cb_sps_wrap <- function(words, prefix = "") {
  if (length(words) == 1L && grepl(" ", words)) words <- strsplit(words, " ", fixed = TRUE)[[1]]
  lines <- character(0)
  cur <- ""
  for (w in words) {
    cand <- if (nzchar(cur)) paste(cur, w) else w
    if (nchar(paste0(prefix, cand), "bytes") > CB_SPS_CHUNK_BYTES && nzchar(cur)) {
      lines <- c(lines, paste0(prefix, cur))
      cur <- w
    } else {
      cur <- cand
    }
  }
  c(lines, paste0(prefix, cur))
}

#' 寫 .sps：UTF-8，開頭加 BOM（SPSS 靠它認出 UTF-8）
#' @noRd
codebook_write_sps <- function(text, file) {
  con <- file(file, "wb")
  on.exit(close(con))
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(charToRaw(enc2utf8(paste0(text, "\n"))), con)
  invisible(file)
}
