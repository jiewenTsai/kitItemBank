# tests/testthat.R — 測試執行器
# 使用方式（從專案根目錄）：
#   Rscript tests/testthat.R
#
# 或在 R console 中：
#   testthat::test_dir("tests/testthat", reporter = "progress")

library(testthat)
test_dir("tests/testthat", reporter = "progress")
