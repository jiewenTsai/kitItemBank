# R/bank.R — 題庫的讀寫與檢查
#
# 題庫 = 一個 domain 的四張表（itemBank、itemDim、itemWave、dimMeta），
# 加上選用的 dimReliability（波次專屬信度）。
# 內建題庫放在 inst/extdata/bank/<domain>/；app 不寫回任何檔案，
# 改好的題庫用 write_bank_xlsx() 下載，維護者用 update_bank() 換掉內建題庫。

#' 三個題庫（domain）：值 = 資料夾名稱，名稱 = 畫面上的字
#' @noRd
BANK_DOMAINS <- c(
  # 原文：家長
  "\u5bb6\u9577"       = "parent",
  # 原文：兒童青少年
  "\u5152\u7ae5\u9752\u5c11\u5e74" = "child_adolescent",
  # 原文：教保人員
  "\u6559\u4fdd\u4eba\u54e1"   = "childcare_teacher"
)

BANK_SHEETS <- c("itemBank", "itemDim", "itemWave", "dimMeta")
BANK_KEYS <- c("itemID", "itemVer")

#' 題庫的中文名稱
#' @noRd
bank_domain_label <- function(domain) {
  names(BANK_DOMAINS)[match(domain, BANK_DOMAINS)]
}

#' 內建題庫的資料夾
#' @noRd
bank_dir <- function() {
  system.file("extdata", "bank", package = "kitItemBank")
}

#' 內建題庫的版本（inst/extdata/bank/BANK_VERSION 的第一行）
#' @noRd
bank_version <- function() {
  f <- file.path(bank_dir(), "BANK_VERSION")
  if (!nzchar(bank_dir()) || !file.exists(f)) return(NA_character_)
  trimws(readLines(f, n = 1L, warn = FALSE, encoding = "UTF-8"))
}

empty_dim_reliability <- function() {
  data.frame(dimID = character(0), wave = character(0),
             reliability = numeric(0), stringsAsFactors = FALSE)
}

#' Read the built-in item bank
#'
#' @param domain `"parent"`, `"child_adolescent"` or `"childcare_teacher"`.
#' @param dir Root folder of the banks (default: the one shipped with the package).
#' @return A list with data frames `itemBank`, `itemDim`, `itemWave`, `dimMeta` and
#'   `dimReliability` (0 rows when there are no per-wave reliabilities).
#' @examples
#' bank <- load_bank("parent")
#' nrow(bank$itemBank)
#' @export
load_bank <- function(domain = "parent", dir = bank_dir()) {
  domain <- match.arg(domain, unname(BANK_DOMAINS))
  path <- file.path(dir, domain)
  read_one <- function(fname, optional = FALSE) {
    f <- file.path(path, fname)
    if (!file.exists(f)) {
      if (optional) return(NULL)
      # 原文：找不到題庫檔案  | （ | ）。
      kitDesign::kit_abort("\u627e\u4e0d\u5230\u984c\u5eab\u6a94\u6848 ", fname, "\uff08", path, "\uff09\u3002")
    }
    as.data.frame(readr::read_csv(
      f, locale = readr::locale(encoding = "UTF-8"),
      show_col_types = FALSE, name_repair = "minimal",
      col_types = readr::cols(.default = readr::col_character())
    ))
  }
  bank <- list(
    itemBank = read_one("itemBank.csv"),
    itemDim  = read_one("itemDim.csv"),
    itemWave = read_one("itemWave.csv"),
    dimMeta  = read_one("dimMeta.csv"),
    dimReliability = read_one("dimReliability.csv", optional = TRUE) %||%
      empty_dim_reliability()
  )
  normalize_bank(bank)
}

#' 整理型別：itemVer、deprecated、0/1 矩陣是整數，其他是字串（NA → ""）
#' @noRd
normalize_bank <- function(bank) {
  ib <- bank$itemBank
  for (col in names(ib)) {
    if (col %in% c("itemVer", "deprecated", "reverseScored")) {
      v <- suppressWarnings(as.integer(ib[[col]]))
      if (col != "itemVer") v[is.na(v)] <- 0L
      ib[[col]] <- v
    } else {
      v <- as.character(ib[[col]])
      v[is.na(v)] <- ""
      ib[[col]] <- v
    }
  }
  bank$itemBank <- ib
  bank$itemDim <- normalize_matrix_df(bank$itemDim)
  bank$itemWave <- normalize_matrix_df(bank$itemWave)
  bank$dimMeta <- ensure_dim_meta_cols(bank$dimMeta)
  dr <- bank$dimReliability %||% empty_dim_reliability()
  if (nrow(dr) > 0L) dr$reliability <- suppressWarnings(as.numeric(dr$reliability))
  bank$dimReliability <- dr
  bank
}

#' 0/1 矩陣（itemDim、itemWave）：itemID 字串、itemVer 整數、其他欄 0/1 整數
#' @noRd
normalize_matrix_df <- function(df) {
  df <- as.data.frame(df)
  df$itemID <- as.character(df$itemID)
  df$itemVer <- suppressWarnings(as.integer(df$itemVer))
  for (col in get_matrix_cols(df)) {
    v <- suppressWarnings(as.integer(df[[col]]))
    v[is.na(v)] <- 0L
    df[[col]] <- v
  }
  df
}

#' dimMeta：一定有 dimID、dimName、alias、reliability、note；空的 dimName 用 dimID
#' @noRd
ensure_dim_meta_cols <- function(dm) {
  dm <- as.data.frame(dm)
  for (col in c("dimName", "alias", "reliability", "note")) {
    if (!col %in% names(dm)) dm[[col]] <- ""
  }
  for (col in names(dm)) {
    v <- as.character(dm[[col]])
    v[is.na(v)] <- ""
    dm[[col]] <- trimws(v)
  }
  empty <- !nzchar(dm$dimName)
  dm$dimName[empty] <- dm$dimID[empty]
  dm[, c("dimID", "dimName", "alias", "reliability", "note",
         setdiff(names(dm), c("dimID", "dimName", "alias", "reliability", "note")))]
}

#' Read an item bank Excel file
#'
#' The file needs the sheets `itemBank`, `itemDim`, `itemWave` and `dimMeta`; `dimReliability`
#' is optional. Stops when a sheet or a key column is missing; the content itself is checked by
#' [check_bank()].
#' @param path Path to the `.xlsx` file.
#' @return A bank, like [load_bank()].
#' @export
read_bank_xlsx <- function(path) {
  sheets <- readxl::excel_sheets(path)
  missing <- setdiff(BANK_SHEETS, sheets)
  if (length(missing) > 0L) {
    kitDesign::kit_abort(
      # 原文：這個 Excel 少了工作表： | 、
      "\u9019\u500b Excel \u5c11\u4e86\u5de5\u4f5c\u8868\uff1a", paste(missing, collapse = "\u3001"),
      # 原文：。題庫 Excel 要有  | 、 |  四個工作表。
      "\u3002\u984c\u5eab Excel \u8981\u6709 ", paste(BANK_SHEETS, collapse = "\u3001"), " \u56db\u500b\u5de5\u4f5c\u8868\u3002"
    )
  }
  read_one <- function(nm) {
    as.data.frame(readxl::read_excel(path, sheet = nm, col_types = "text",
                                     .name_repair = "minimal"))
  }
  bank <- lapply(stats::setNames(BANK_SHEETS, BANK_SHEETS), read_one)
  bank$dimReliability <- if ("dimReliability" %in% sheets) {
    read_one("dimReliability")
  } else {
    empty_dim_reliability()
  }
  for (tbl in c("itemBank", "itemDim", "itemWave")) {
    if (!all(BANK_KEYS %in% names(bank[[tbl]]))) {
      # 原文：工作表  |  少了 itemID 或 itemVer 欄。
      kitDesign::kit_abort("\u5de5\u4f5c\u8868 ", tbl, " \u5c11\u4e86 itemID \u6216 itemVer \u6b04\u3002")
    }
  }
  if (!"dimID" %in% names(bank$dimMeta)) {
    # 原文：工作表 dimMeta 少了 dimID 欄。
    kitDesign::kit_abort("\u5de5\u4f5c\u8868 dimMeta \u5c11\u4e86 dimID \u6b04\u3002")
  }
  normalize_bank(bank)
}

#' Write an item bank to Excel
#'
#' Writes the four tables (and `dimReliability` when it has rows) as sheets, plus an optional
#' change log sheet 「改動紀錄」.
#' @param bank A bank from [load_bank()] or [read_bank_xlsx()].
#' @param path Output `.xlsx` path.
#' @param changes Change log from [bank_diff()]; no sheet is written when `NULL` or empty.
#' @return `path`, invisibly.
#' @export
write_bank_xlsx <- function(bank, path, changes = NULL) {
  sheets <- list(
    itemBank = bank$itemBank,
    itemDim  = bank$itemDim,
    itemWave = bank$itemWave,
    dimMeta  = ensure_dim_meta_cols(bank$dimMeta)
  )
  if (!is.null(bank$dimReliability) && nrow(bank$dimReliability) > 0L) {
    sheets$dimReliability <- bank$dimReliability
  }
  if (!is.null(changes) && nrow(changes) > 0L) {
    # 原文：改動紀錄
    sheets[["\u6539\u52d5\u7d00\u9304"]] <- changes
  }
  writexl::write_xlsx(sheets, path)
  invisible(path)
}

#' Check an item bank
#'
#' Checks whether a bank can be used: duplicated `itemID` + `itemVer`, missing `itemVer`,
#' `itemDim`/`itemWave` rows without an item, values other than 0/1 in `itemDim`/`itemWave`,
#' dimensions without dimension information, gaps in `itemVer` (a note), and (with
#' `reference`) items added or removed. Content problems found by [bank_audit()] are summed up
#' in one note; they do not stop [update_bank()].
#' @param bank The bank to check.
#' @param reference A bank to compare with (usually the built-in one); `NULL` to skip.
#' @return A list: `ok` (`FALSE` when the bank cannot be used) and `items`, a list of
#'   [kitDesign::kit_check()] items for [kitDesign::kit_checklist()].
#' @export
check_bank <- function(bank, reference = NULL) {
  items <- list()
  ok <- TRUE
  key <- function(df) paste(df$itemID, df$itemVer, sep = "\r")
  ib_keys <- key(bank$itemBank)

  dup <- unique(bank$itemBank$itemID[duplicated(ib_keys)])
  if (length(dup) > 0L) {
    ok <- FALSE
    items[[length(items) + 1L]] <- kitDesign::kit_check(
      # 原文：itemBank 有  |  題的 itemID＋itemVer 重複
      "todo", paste0("itemBank \u6709 ", length(dup), " \u984c\u7684 itemID\uff0bitemVer \u91cd\u8907"),
      # 原文：例如： | 、
      why = paste0("\u4f8b\u5982\uff1a", paste(utils::head(dup, 5L), collapse = "\u3001"),
                   # 原文：。每一題的 itemID＋itemVer 要是唯一的，請在 Excel 改好再打開。
                   "\u3002\u6bcf\u4e00\u984c\u7684 itemID\uff0bitemVer \u8981\u662f\u552f\u4e00\u7684\uff0c\u8acb\u5728 Excel \u6539\u597d\u518d\u6253\u958b\u3002")
    )
  }
  if (anyNA(bank$itemBank$itemVer)) {
    ok <- FALSE
    items[[length(items) + 1L]] <- kitDesign::kit_check(
      # 原文：itemBank 有題目沒有 itemVer
      "todo", "itemBank \u6709\u984c\u76ee\u6c92\u6709 itemVer",
      # 原文：itemVer 要是整數（1、2…）。
      why = "itemVer \u8981\u662f\u6574\u6578\uff081\u30012\u2026\uff09\u3002"
    )
  }
  for (tbl in c("itemDim", "itemWave")) {
    extra <- setdiff(key(bank[[tbl]]), ib_keys)
    if (length(extra) > 0L) {
      items[[length(items) + 1L]] <- kitDesign::kit_check(
        # 原文： 有  |  題在 itemBank 找不到
        "todo", paste0(tbl, " \u6709 ", length(extra), " \u984c\u5728 itemBank \u627e\u4e0d\u5230"),
        # 原文：這些題目查詢時不會出現。
        why = "\u9019\u4e9b\u984c\u76ee\u67e5\u8a62\u6642\u4e0d\u6703\u51fa\u73fe\u3002"
      )
    }
  }
  for (tbl in c("itemDim", "itemWave")) {
    df <- bank[[tbl]]
    bad <- vapply(get_matrix_cols(df), function(cl) sum(!df[[cl]] %in% c(0L, 1L)), integer(1))
    if (sum(bad) > 0L) {
      ok <- FALSE
      items[[length(items) + 1L]] <- kitDesign::kit_check(
        # 原文： 有  |  格不是 0 或 1
        "todo", paste0(tbl, " \u6709 ", kitDesign::kit_num(sum(bad)), " \u683c\u4e0d\u662f 0 \u6216 1"),
        # 原文：在這些欄：
        why = paste0("\u5728\u9019\u4e9b\u6b04\uff1a", paste(col_display_labels(utils::head(names(bad)[bad > 0L], 5L)),
                                       # 原文：、
                                       collapse = "\u3001"),
                     # 原文：。每一格要是 0（沒有）或 1（有），請在 Excel 改好再打開。
                     "\u3002\u6bcf\u4e00\u683c\u8981\u662f 0\uff08\u6c92\u6709\uff09\u6216 1\uff08\u6709\uff09\uff0c\u8acb\u5728 Excel \u6539\u597d\u518d\u6253\u958b\u3002")
      )
    }
  }
  vers <- split(bank$itemBank$itemVer, bank$itemBank$itemID)
  gap <- names(vers)[vapply(vers, function(v) {
    v <- sort(v)
    !anyNA(v) && !identical(as.integer(v), seq_along(v))
  }, logical(1))]
  if (length(gap) > 0L) {
    items[[length(items) + 1L]] <- kitDesign::kit_check(
      # 原文： 題的 itemVer 不連號
      "note", paste0(kitDesign::kit_num(length(gap)), " \u984c\u7684 itemVer \u4e0d\u9023\u865f"),
      # 原文：例如： | 、
      why = paste0("\u4f8b\u5982\uff1a", paste(utils::head(gap, 5L), collapse = "\u3001"),
                   # 原文：。可以照用；如果是合併版本留下的空號，可以在 note 說明。
                   "\u3002\u53ef\u4ee5\u7167\u7528\uff1b\u5982\u679c\u662f\u5408\u4f75\u7248\u672c\u7559\u4e0b\u7684\u7a7a\u865f\uff0c\u53ef\u4ee5\u5728 note \u8aaa\u660e\u3002")
    )
  }
  dims_no_meta <- setdiff(get_matrix_cols(bank$itemDim), bank$dimMeta$dimID)
  if (length(dims_no_meta) > 0L) {
    items[[length(items) + 1L]] <- kitDesign::kit_check(
      # 原文：構面歸屬有  |  個構面，構面資訊裡沒有
      "todo", paste0("\u69cb\u9762\u6b78\u5c6c\u6709 ", length(dims_no_meta), " \u500b\u69cb\u9762\uff0c\u69cb\u9762\u8cc7\u8a0a\u88e1\u6c92\u6709"),
      # 原文：、
      why = paste0(paste(utils::head(dims_no_meta, 5L), collapse = "\u3001"),
                   # 原文：：會用構面代碼當名稱。
                   "\uff1a\u6703\u7528\u69cb\u9762\u4ee3\u78bc\u7576\u540d\u7a31\u3002")
    )
  }
  if (!is.null(reference)) {
    ref_keys <- key(reference$itemBank)
    gone <- setdiff(ref_keys, ib_keys)
    added <- setdiff(ib_keys, ref_keys)
    if (length(gone) > 0L) {
      items[[length(items) + 1L]] <- kitDesign::kit_check(
        # 原文：和內建題庫比，少了  |  題
        "todo", paste0("\u548c\u5167\u5efa\u984c\u5eab\u6bd4\uff0c\u5c11\u4e86 ", kitDesign::kit_num(length(gone)), " \u984c"),
        # 原文：例如： |  第  |  版、 |  版。
        why = paste0("\u4f8b\u5982\uff1a", paste(utils::head(sub("\r", " \u7b2c ", gone), 5L), collapse = " \u7248\u3001"), " \u7248\u3002")
      )
    }
    if (length(added) > 0L) {
      items[[length(items) + 1L]] <- kitDesign::kit_check(
        # 原文：和內建題庫比，多了  |  題
        "note", paste0("\u548c\u5167\u5efa\u984c\u5eab\u6bd4\uff0c\u591a\u4e86 ", kitDesign::kit_num(length(added)), " \u984c")
      )
    }
  }
  audit <- tryCatch(bank_audit(bank), error = function(e) NULL)
  if (!is.null(audit) && nrow(audit) > 0L) {
    sm <- bank_audit_summary(audit)
    items[[length(items) + 1L]] <- kitDesign::kit_check(
      # 原文：內容有  |  項要注意（在「題庫編修」看）
      "note", paste0("\u5167\u5bb9\u6709 ", kitDesign::kit_num(nrow(audit)), " \u9805\u8981\u6ce8\u610f\uff08\u5728\u300c\u984c\u5eab\u7de8\u4fee\u300d\u770b\uff09"),
      # 原文： 項 | 、 | 。
      why = paste0(paste0(sm$label, " ", kitDesign::kit_num(sm$n), " \u9805", collapse = "\u3001"), "\u3002")
    )
  }
  if (ok) {
    items[[length(items) + 1L]] <- kitDesign::kit_check(
      # 原文：四個工作表都在，共  |  題、
      "ok", paste0("\u56db\u500b\u5de5\u4f5c\u8868\u90fd\u5728\uff0c\u5171 ", kitDesign::kit_num(nrow(bank$itemBank)), " \u984c\u3001",
                   # 原文： 個波次、
                   length(get_wave_cols(bank$itemWave)), " \u500b\u6ce2\u6b21\u3001",
                   # 原文： 個構面
                   length(get_matrix_cols(bank$itemDim)), " \u500b\u69cb\u9762")
    )
  }
  list(ok = ok, items = items)
}

#' 一行題庫摘要：「2,052 題 · 24 個波次 · 130 個構面」
#' @noRd
bank_summary_text <- function(bank) {
  # 原文： 題 · 
  paste0(kitDesign::kit_num(nrow(bank$itemBank)), " \u984c \u00b7 ",
         # 原文： 個波次 · 
         length(get_wave_cols(bank$itemWave)), " \u500b\u6ce2\u6b21 \u00b7 ",
         # 原文： 個構面
         length(get_matrix_cols(bank$itemDim)), " \u500b\u69cb\u9762")
}

#' Replace the built-in item bank with an edited Excel file (for maintainers)
#'
#' Run in the kitItemBank source folder. Reads and checks the Excel file downloaded from the
#' app, writes the CSV files in `inst/extdata/bank/<domain>/` and updates `BANK_VERSION`.
#' Review the result with `git diff`, then commit and release.
#'
#' @param path The edited bank (`.xlsx`), as downloaded from the app.
#' @param domain Which bank the file is: `"parent"`, `"child_adolescent"` or
#'   `"childcare_teacher"`.
#' @param pkg_dir Root of the kitItemBank source.
#' @param version Bank version written to `BANK_VERSION` (default: today).
#' @return The output folder, invisibly.
#' @export
update_bank <- function(path, domain, pkg_dir = ".", version = format(Sys.Date())) {
  domain <- match.arg(domain, unname(BANK_DOMAINS))
  out <- file.path(pkg_dir, "inst", "extdata", "bank", domain)
  if (!dir.exists(file.path(pkg_dir, "inst"))) {
    # 原文：在  |  找不到 inst/：請在 kitItemBank 的原始碼資料夾執行。
    kitDesign::kit_abort("\u5728 ", normalizePath(pkg_dir), " \u627e\u4e0d\u5230 inst/\uff1a\u8acb\u5728 kitItemBank \u7684\u539f\u59cb\u78bc\u8cc7\u6599\u593e\u57f7\u884c\u3002")
  }
  bank <- read_bank_xlsx(path)
  chk <- check_bank(bank)
  if (!chk$ok) {
    print(kitDesign::kit_checklist(chk$items))
    # 原文：這份題庫有要先修好的問題（見上面），沒有寫入。
    kitDesign::kit_abort("\u9019\u4efd\u984c\u5eab\u6709\u8981\u5148\u4fee\u597d\u7684\u554f\u984c\uff08\u898b\u4e0a\u9762\uff09\uff0c\u6c92\u6709\u5beb\u5165\u3002")
  }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  write_csv <- function(df, name) {
    readr::write_csv(df, file.path(out, name), na = "")
  }
  write_csv(bank$itemBank, "itemBank.csv")
  write_csv(bank$itemDim, "itemDim.csv")
  write_csv(bank$itemWave, "itemWave.csv")
  write_csv(ensure_dim_meta_cols(bank$dimMeta), "dimMeta.csv")
  rel <- file.path(out, "dimReliability.csv")
  if (nrow(bank$dimReliability) > 0L) {
    write_csv(bank$dimReliability, "dimReliability.csv")
  } else if (file.exists(rel)) {
    file.remove(rel)
  }
  writeLines(version, file.path(pkg_dir, "inst", "extdata", "bank", "BANK_VERSION"))
  sheets <- readxl::excel_sheets(path)
  # 原文：改動紀錄
  if ("\u6539\u52d5\u7d00\u9304" %in% sheets) {
    # 原文：改動紀錄
    log <- readxl::read_excel(path, sheet = "\u6539\u52d5\u7d00\u9304")
    # 原文：改動紀錄： |  筆
    message("\u6539\u52d5\u7d00\u9304\uff1a", nrow(log), " \u7b46")
  }
  # 原文：已寫入  | （ | ）
  message("\u5df2\u5beb\u5165 ", normalizePath(out), "\uff08", bank_summary_text(bank), "\uff09")
  invisible(out)
}

#' Change log between two item banks
#'
#' Compares every field of the items (`itemBank`, including added or removed item versions),
#' every cell of the dimension and wave assignments (`itemDim`, `itemWave`), every field of the
#' dimension information (`dimMeta`, `dimReliability`), and added or removed dimensions and
#' waves.
#' @param old,new Two banks.
#' @return A data frame with Chinese column names 表, 題號或構面, 版本, 欄, 原本, 改成 (the
#'   change log sheet of the downloaded Excel); 0 rows when nothing changed.
#' @export
bank_diff <- function(old, new) {
  out <- list()
  add <- function(tbl, row, ver, col, from, to) {
    n <- max(length(row), length(col))
    if (n == 0L) return(invisible())
    out[[length(out) + 1L]] <<- data.frame(
      # 原文：表 | 題號或構面
      "\u8868" = rep(tbl, n), "\u984c\u865f\u6216\u69cb\u9762" = rep_len(as.character(row), n),
      # 原文：版本 | 欄
      "\u7248\u672c" = rep_len(as.character(ver), n), "\u6b04" = rep_len(as.character(col), n),
      # 原文：原本 | 改成
      "\u539f\u672c" = rep_len(as.character(from), n), "\u6539\u6210" = rep_len(as.character(to), n),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  # itemBank：每一題每一版的每個欄位；新增、刪除的版本各記一列
  ia <- old$itemBank
  ibn <- new$itemBank
  if (!is.null(ia) && !is.null(ibn)) {
    ka <- paste(ia$itemID, ia$itemVer, sep = "\r")
    kb <- paste(ibn$itemID, ibn$itemVer, sep = "\r")
    as_txt <- function(v) {
      v <- as.character(v)
      v[is.na(v)] <- ""
      v
    }
    gone <- which(!ka %in% kb)
    if (length(gone)) add("itemBank", ia$itemID[gone], ia$itemVer[gone], "itemVer",
                          # 原文：（整列） | （刪除）
                          "\uff08\u6574\u5217\uff09", "\uff08\u522a\u9664\uff09")
    added <- which(!kb %in% ka)
    if (length(added)) add("itemBank", ibn$itemID[added], ibn$itemVer[added], "itemVer",
                           # 原文：（沒有這一版） | （新增）
                           "\uff08\u6c92\u6709\u9019\u4e00\u7248\uff09", "\uff08\u65b0\u589e\uff09")
    idx <- match(kb, ka)
    keep <- which(!is.na(idx))
    for (col in setdiff(union(names(ia), names(ibn)), BANK_KEYS)) {
      va <- if (col %in% names(ia)) as_txt(ia[[col]][idx[keep]]) else rep("", length(keep))
      vb <- if (col %in% names(ibn)) as_txt(ibn[[col]][keep]) else rep("", length(keep))
      hit <- which(va != vb)
      if (length(hit)) {
        add("itemBank", ibn$itemID[keep][hit], ibn$itemVer[keep][hit], col, va[hit], vb[hit])
      }
    }
  }
  for (tbl in c("itemDim", "itemWave")) {
    a <- old[[tbl]]
    b <- new[[tbl]]
    ca <- get_matrix_cols(a)
    cb <- get_matrix_cols(b)
    gone <- setdiff(ca, cb)
    added <- setdiff(cb, ca)
    # 原文：（整欄） | （刪除）
    if (length(gone)) add(tbl, "", "", gone, "\uff08\u6574\u6b04\uff09", "\uff08\u522a\u9664\uff09")
    # 原文：（沒有這一欄） | （新增）
    if (length(added)) add(tbl, "", "", added, "\uff08\u6c92\u6709\u9019\u4e00\u6b04\uff09", "\uff08\u65b0\u589e\uff09")
    ka <- paste(a$itemID, a$itemVer, sep = "\r")
    kb <- paste(b$itemID, b$itemVer, sep = "\r")
    idx <- match(kb, ka)
    keep <- !is.na(idx)
    for (col in intersect(ca, cb)) {
      va <- a[[col]][idx[keep]]
      vb <- b[[col]][keep]
      hit <- which(va != vb)
      if (length(hit)) {
        add(tbl, b$itemID[keep][hit], b$itemVer[keep][hit], col, va[hit], vb[hit])
      }
    }
    # 新增的構面欄：記下被勾成 1 的題目
    for (col in added) {
      hit <- which(b[[col]] == 1L)
      if (length(hit)) add(tbl, b$itemID[hit], b$itemVer[hit], col, "0", "1")
    }
  }
  da <- ensure_dim_meta_cols(old$dimMeta)
  db <- ensure_dim_meta_cols(new$dimMeta)
  gone <- setdiff(da$dimID, db$dimID)
  added <- setdiff(db$dimID, da$dimID)
  # 原文：（刪除）
  if (length(gone)) add("dimMeta", gone, "", "dimID", gone, "\uff08\u522a\u9664\uff09")
  # 原文：（沒有這個構面）
  if (length(added)) add("dimMeta", added, "", "dimID", "\uff08\u6c92\u6709\u9019\u500b\u69cb\u9762\uff09", added)
  both <- intersect(da$dimID, db$dimID)
  ia <- match(both, da$dimID)
  ib <- match(both, db$dimID)
  for (col in intersect(setdiff(names(da), "dimID"), names(db))) {
    va <- da[[col]][ia]
    vb <- db[[col]][ib]
    hit <- which(va != vb)
    if (length(hit)) add("dimMeta", both[hit], "", col, va[hit], vb[hit])
  }
  ra <- old$dimReliability %||% empty_dim_reliability()
  rb <- new$dimReliability %||% empty_dim_reliability()
  ka <- paste(ra$dimID, ra$wave, sep = "\r")
  kb <- paste(rb$dimID, rb$wave, sep = "\r")
  va <- as.character(ra$reliability)[match(union(ka, kb), ka)]
  vb <- as.character(rb$reliability)[match(union(ka, kb), kb)]
  va[is.na(va)] <- ""
  vb[is.na(vb)] <- ""
  hit <- which(va != vb)
  if (length(hit)) {
    k <- strsplit(union(ka, kb)[hit], "\r", fixed = TRUE)
    add("dimReliability", vapply(k, `[`, "", 1L), "",
        # 原文：reliability（ | ）
        paste0("reliability\uff08", vapply(k, `[`, "", 2L), "\uff09"), va[hit], vb[hit])
  }
  if (length(out) == 0L) {
    # 原文：表 | 題號或構面
    return(data.frame("\u8868" = character(0), "\u984c\u865f\u6216\u69cb\u9762" = character(0),
                      # 原文：版本 | 欄
                      "\u7248\u672c" = character(0), "\u6b04" = character(0),
                      # 原文：原本 | 改成
                      "\u539f\u672c" = character(0), "\u6539\u6210" = character(0),
                      check.names = FALSE))
  }
  do.call(rbind, out)
}
