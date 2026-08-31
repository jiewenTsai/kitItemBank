# tests/testthat/helper.R
# 此檔案由 testthat 在每個測試檔前自動 source。
# 負責載入套件與 app 的所有 R 原始碼，並提供輔助函式。

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(officer)
  library(flextable)
})

# ── 定位專案根目錄 ────────────────────────────────────────────────
# test_dir() 執行時 cwd 可能是 tests/testthat/ 或專案根；
# 兩種情況都能正確找到根目錄。
.proj_root <- if (file.exists("app.R")) {
  normalizePath(".")
} else if (file.exists(file.path("..", "..", "app.R"))) {
  normalizePath(file.path("..", ".."))
} else {
  stop(
    "找不到專案根目錄（app.R 不存在）。",
    "請從專案根目錄執行：Rscript tests/testthat.R"
  )
}

# ── Source 所有 app R 原始碼 ──────────────────────────────────────
for (.f in c(
  "R/utils.R",
  "R/data_io.R",
  "R/utils_docx.R",
  "R/utils_dim_report_docx.R"
)) {
  source(file.path(.proj_root, .f))
}
rm(.f)

# ── 輔助函式 ─────────────────────────────────────────────────────

#' 以絕對路徑載入 domain 資料，不受 cwd 影響
.load_domain <- function(domain) {
  load_domain(
    domain,
    data_root  = file.path(.proj_root, "data"),
    files_root = file.path(.proj_root, "files")
  )
}

#' 以絕對路徑讀取 itemBank（優先 files/*.xlsx）
.item_bank <- function(domain = "parent") {
  .load_domain(domain)$itemBank
}

#' 找出某 dim 在 itemWave 中有題項的第一個波次欄名
#' 回傳 NULL 表示找不到有交集的波次（測試應 skip_if）
.find_wave_with_dim <- function(d, dim_id) {
  iw        <- d$itemWave
  id        <- d$itemDim
  wave_cols <- get_wave_cols(iw)

  if (!dim_id %in% names(id)) return(NULL)
  dim_items <- id[id[[dim_id]] == 1L, c("itemID", "itemVer"), drop = FALSE]
  if (nrow(dim_items) == 0L) return(NULL)

  for (w in wave_cols) {
    wave_items <- iw[iw[[w]] == 1L, c("itemID", "itemVer"), drop = FALSE]
    if (nrow(dplyr::inner_join(wave_items, dim_items,
                               by = c("itemID", "itemVer"))) > 0L) {
      return(w)
    }
  }
  NULL
}
