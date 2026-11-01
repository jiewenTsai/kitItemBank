# tests/testthat/helper.R：測試共用的小工具（testthat 會在每個測試檔前讀進來）

#' 內建題庫（同一個 R 程序裡只讀一次）
.bank_cache <- new.env()
.bank <- function(domain = "parent") {
  if (is.null(.bank_cache[[domain]])) .bank_cache[[domain]] <- load_bank(domain)
  .bank_cache[[domain]]
}
