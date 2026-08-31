# R/data_io.R — 資料載入 / 寫回

#' 各 domain 對應的題庫 Excel（files/ 預載）
DOMAIN_BUNDLE_XLSX <- c(
  parent            = "\u5bb6\u9577.xlsx",
  child_adolescent  = "\u5152\u7ae5\u9752\u5c11\u5e74.xlsx",
  childcare_teacher = "\u6559\u4fdd\u6559\u5e2b.xlsx"
)

#' 取得 domain 的 Excel 路徑（不存在回傳 NULL）
domain_bundle_xlsx_path <- function(domain, files_root = "files") {
  fname <- DOMAIN_BUNDLE_XLSX[[domain]]
  if (is.null(fname) || !nzchar(fname)) return(NULL)
  path <- file.path(files_root, fname)
  if (file.exists(path)) path else NULL
}

#' 載入指定 domain 的四張核心表
#'
#' 優先讀取 files/ 內對應 Excel；若無則讀 data/<domain>/ CSV。
#'
#' @param domain     domain 名稱（"parent" / "child_adolescent" / "childcare_teacher"）
#' @param data_root  CSV 資料根目錄，預設 "data"
#' @param files_root Excel 預載目錄，預設 "files"
#' @return 含 itemBank, itemDim, itemWave, dimMeta 的具名 list
load_domain <- function(domain = "parent", data_root = "data", files_root = "files") {
  xlsx_path <- domain_bundle_xlsx_path(domain, files_root)
  if (!is.null(xlsx_path)) {
    # 信度以 dimMeta 工作表的 reliability 欄為主；
    # 若 xlsx 另有 dimReliability 工作表（波次專屬覆寫）由 import_domain_xlsx 讀入。
    return(import_domain_xlsx(xlsx_path))
  }

  domain_path <- file.path(data_root, domain)

  if (!dir.exists(domain_path)) {
    warning(sprintf(
      "找不到 %s，且 files/ 無對應 Excel，改用目前工作目錄讀取資料（暫行方案）。\n請將題庫 Excel 放入 files/ 或 CSV 放入 %s/ 。",
      domain_path, domain_path
    ))
    domain_path <- "."
  }

  read_utf8 <- function(fname) {
    fpath <- file.path(domain_path, fname)
    if (!file.exists(fpath)) {
      stop(sprintf("找不到 %s（路徑：%s）", fname, fpath))
    }
    readr::read_csv(
      fpath,
      locale       = readr::locale(encoding = "UTF-8"),
      show_col_types = FALSE,
      name_repair  = "minimal"   # 保留原始欄名，不做任何修改
    )
  }

  result <- list(
    itemBank = read_utf8("itemBank.csv"),
    itemDim  = read_utf8("itemDim.csv"),
    itemWave = read_utf8("itemWave.csv"),
    dimMeta  = read_utf8("dimMeta.csv")
  )
  result <- normalize_domain_bundle(result)
  result$dimReliability <- load_dim_reliability(domain, data_root, domain_path)
  result
}

#' 正規化 itemBank（Excel 讀入後）
normalize_item_bank <- function(df) {
  if ("itemVer" %in% names(df))
    df$itemVer <- as.integer(df$itemVer)
  if ("deprecated" %in% names(df))
    df$deprecated <- as.integer(df$deprecated)
  if ("stemID" %in% names(df)) {
    df$stemID <- vapply(df$stemID, function(x) {
      if (length(x) == 0L || is.null(x) || is.na(x)) "" else as.character(x)
    }, character(1L), USE.NAMES = FALSE)
  }
  if ("itemType" %in% names(df)) {
    df$itemType <- vapply(df$itemType, function(x) {
      if (length(x) == 0L || is.null(x) || is.na(x)) "" else as.character(x)
    }, character(1L), USE.NAMES = FALSE)
  }
  df
}

#' 正規化 itemDim / itemWave 矩陣（0/1 整數化）
normalize_matrix_df <- function(df) {
  if ("itemVer" %in% names(df))
    df$itemVer <- as.integer(df$itemVer)
  for (col in get_matrix_cols(df)) {
    vals <- df[[col]]
    vals[is.na(vals)] <- 0L
    df[[col]] <- as.integer(vals)
  }
  df
}

#' 正規化四表 bundle
normalize_domain_bundle <- function(bundle) {
  bundle$itemBank <- normalize_item_bank(bundle$itemBank)
  bundle$itemDim  <- normalize_matrix_df(bundle$itemDim)
  bundle$itemWave <- normalize_matrix_df(bundle$itemWave)
  bundle$dimMeta  <- ensure_dim_meta_cols(bundle$dimMeta)
  bundle
}

#' 確保 dimMeta 含 alias 欄，並將 NA 轉為空字串
ensure_dim_meta_cols <- function(dm) {
  if (!"alias" %in% names(dm)) dm$alias <- ""
  dm$alias <- vapply(dm$alias, clean_meta_str, character(1L), USE.NAMES = FALSE)
  if ("note" %in% names(dm)) {
    dm$note <- vapply(dm$note, function(x) clean_meta_str(x), character(1L),
                      USE.NAMES = FALSE)
  }
  if ("dimName" %in% names(dm)) {
    dm$dimName <- vapply(dm$dimName, function(x) clean_meta_str(x, default = NA_character_),
                         character(1L), USE.NAMES = FALSE)
    empty <- is.na(dm$dimName) | !nzchar(dm$dimName)
    dm$dimName[empty] <- dm$dimID[empty]
  }
  dm
}

#' 讀取波次專屬信度表（可選檔案）
load_dim_reliability <- function(domain, data_root = "data", domain_path = NULL) {
  if (is.null(domain_path)) {
    domain_path <- file.path(data_root, domain)
    if (!dir.exists(domain_path)) domain_path <- "."
  }
  fpath <- file.path(domain_path, "dimReliability.csv")
  if (!file.exists(fpath)) {
    return(data.frame(
      dimID = character(0), wave = character(0),
      reliability = numeric(0), stringsAsFactors = FALSE
    ))
  }
  readr::read_csv(
    fpath,
    locale       = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE,
    name_repair  = "minimal"
  )
}

#' 寫回 dimMeta.csv
save_dim_meta <- function(domain, df, data_root = "data") {
  domain_path <- file.path(data_root, domain)
  if (!dir.exists(domain_path)) domain_path <- "."
  fpath <- file.path(domain_path, "dimMeta.csv")
  readr::write_csv(ensure_dim_meta_cols(df), fpath, na = "")
  invisible(fpath)
}

#' 寫回 dimReliability.csv（波次專屬信度）
save_dim_reliability <- function(domain, df, data_root = "data") {
  domain_path <- file.path(data_root, domain)
  if (!dir.exists(domain_path)) domain_path <- "."
  fpath <- file.path(domain_path, "dimReliability.csv")
  readr::write_csv(df, fpath, na = "")
  invisible(fpath)
}

#' 將更新後的矩陣（itemDim 或 itemWave）寫回 CSV
#'
#' @param domain     domain 名稱
#' @param table_name "itemDim" 或 "itemWave"
#' @param df         更新後的 data.frame
#' @param data_root  資料根目錄
#' @return 寫入路徑（invisible）
save_matrix <- function(domain, table_name, df, data_root = "data") {
  domain_path <- file.path(data_root, domain)
  if (!dir.exists(domain_path)) domain_path <- "."

  fpath <- file.path(domain_path, paste0(table_name, ".csv"))
  readr::write_csv(df, fpath, na = "")
  invisible(fpath)
}

# ── 四表 Excel 總匯 ───────────────────────────────────────────────

DOMAIN_BUNDLE_SHEETS <- c("itemBank", "itemDim", "itemWave", "dimMeta")

#' 匯出四表至單一 Excel（每表一個工作表；含 dimReliability 時一併匯出）
export_domain_xlsx <- function(domain_list, path) {
  if (!requireNamespace("writexl", quietly = TRUE))
    stop("需要 writexl 套件")
  sheets <- list(
    itemBank = domain_list$itemBank,
    itemDim  = domain_list$itemDim,
    itemWave = domain_list$itemWave,
    dimMeta  = ensure_dim_meta_cols(domain_list$dimMeta)
  )
  dr <- domain_list$dimReliability
  if (!is.null(dr) && nrow(dr) > 0L)
    sheets$dimReliability <- dr
  writexl::write_xlsx(sheets, path)
  invisible(path)
}

#' 將記憶體中的 domain 四表寫回 CSV，並同步寫入四表 Excel
save_domain_from_memory <- function(domain, domain_list, data_root = "data") {
  bundle <- list(
    itemBank = domain_list$itemBank,
    itemDim  = domain_list$itemDim,
    itemWave = domain_list$itemWave,
    dimMeta  = ensure_dim_meta_cols(domain_list$dimMeta),
    dimReliability = domain_list$dimReliability %||% data.frame(
      dimID = character(0), wave = character(0),
      reliability = numeric(0), stringsAsFactors = FALSE
    )
  )
  paths <- save_domain_bundle(domain, bundle, data_root)

  domain_path <- file.path(data_root, domain)
  if (!dir.exists(domain_path)) domain_path <- "."

  xlsx_path <- file.path(
    domain_path,
    paste0("KIT_", domain, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
  )
  export_domain_xlsx(bundle, xlsx_path)
  paths$xlsx <- xlsx_path
  invisible(paths)
}

#' 從 Excel 讀取四表
import_domain_xlsx <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("需要 readxl 套件，請執行 install.packages('readxl')")

  sheets <- readxl::excel_sheets(path)
  missing <- setdiff(DOMAIN_BUNDLE_SHEETS, sheets)
  if (length(missing) > 0L) {
    stop(paste0(
      "Excel 缺少工作表：", paste(missing, collapse = ", "),
      "\n需要：", paste(DOMAIN_BUNDLE_SHEETS, collapse = ", ")
    ))
  }

  read_one <- function(nm) {
    as.data.frame(readxl::read_excel(path, sheet = nm, .name_repair = "minimal"))
  }

  result <- list(
    itemBank = read_one("itemBank"),
    itemDim  = read_one("itemDim"),
    itemWave = read_one("itemWave"),
    dimMeta  = read_one("dimMeta")
  )
  result <- normalize_domain_bundle(result)

  for (tbl in c("itemBank", "itemDim", "itemWave")) {
    if (!all(c("itemID", "itemVer") %in% names(result[[tbl]]))) {
      stop(paste0("工作表 ", tbl, " 必須含 itemID、itemVer 欄"))
    }
  }
  if (!"dimID" %in% names(result$dimMeta))
    stop("工作表 dimMeta 必須含 dimID 欄")

  result$dimReliability <- data.frame(
    dimID = character(0), wave = character(0),
    reliability = numeric(0), stringsAsFactors = FALSE
  )
  if ("dimReliability" %in% sheets)
    result$dimReliability <- read_one("dimReliability")

  result
}

#' 將四表 bundle 寫回 CSV
save_domain_bundle <- function(domain, bundle, data_root = "data") {
  domain_path <- file.path(data_root, domain)
  if (!dir.exists(domain_path)) dir.create(domain_path, recursive = TRUE)

  paths <- list(
    itemBank = file.path(domain_path, "itemBank.csv"),
    itemDim  = file.path(domain_path, "itemDim.csv"),
    itemWave = file.path(domain_path, "itemWave.csv"),
    dimMeta  = file.path(domain_path, "dimMeta.csv")
  )
  readr::write_csv(bundle$itemBank, paths$itemBank, na = "")
  readr::write_csv(bundle$itemDim,  paths$itemDim,  na = "")
  readr::write_csv(bundle$itemWave, paths$itemWave, na = "")
  readr::write_csv(ensure_dim_meta_cols(bundle$dimMeta), paths$dimMeta, na = "")

  if (!is.null(bundle$dimReliability) && nrow(bundle$dimReliability) > 0L) {
    p <- file.path(domain_path, "dimReliability.csv")
    readr::write_csv(bundle$dimReliability, p, na = "")
    paths$dimReliability <- p
  }
  invisible(paths)
}

#' 匯出單一矩陣表至 Excel（itemDim / itemWave 編修用）
export_matrix_xlsx <- function(df, path, sheet_name = "sheet1") {
  if (!requireNamespace("writexl", quietly = TRUE))
    stop("需要 writexl 套件")
  writexl::write_xlsx(stats::setNames(list(df), sheet_name), path)
  invisible(path)
}

#' 從 Excel 讀取單一矩陣表
import_matrix_xlsx <- function(path, table_name = NULL) {
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("需要 readxl 套件")
  sheets <- readxl::excel_sheets(path)
  sheet <- if (!is.null(table_name) && table_name %in% sheets) {
    table_name
  } else if (length(sheets) == 1L) {
    sheets[1L]
  } else if ("itemDim" %in% sheets) {
    "itemDim"
  } else if ("itemWave" %in% sheets) {
    "itemWave"
  } else {
    sheets[1L]
  }
  df <- as.data.frame(
    readxl::read_excel(path, sheet = sheet, .name_repair = "minimal")
  )
  if (!all(c("itemID", "itemVer") %in% names(df))) {
    stop("Excel 必須含 itemID、itemVer 欄")
  }
  normalize_matrix_df(df)
}
