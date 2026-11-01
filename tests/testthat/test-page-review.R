# page_review.R：審核頁（testServer）。這一頁不寫檔、不連網路，只給下載。

review_html <- function(x) if (is.list(x)) x$html else as.character(x)

review_page_file <- function(meta = list(), base_version = kit_version(), env = parent.frame()) {
  kit <- kit_builtin()
  old <- bank_to_v02(kit, "parent")
  new <- old
  new$itemBank$label[new$itemBank$itemID == "fatherinvo01n"] <- "y"
  res <- kit_patch(kit, old, new, "parent")
  f <- withr::local_tempfile(fileext = ".xlsx", .local_envir = env)
  writexl::write_xlsx(patch_workbook(res, bank_diff(old, new), "parent", base_version,
                                     meta = meta), f)
  data.frame(name = "修改包_test.xlsx", size = file.size(f), type = "", datapath = f,
             stringsAsFactors = FALSE)
}

test_that("the page reviews a package and downloads the applied bank as a zip", {
  went <- NULL
  shiny::testServer(page_review_server, args = list(
    kit = kit_builtin(), version = "2026-11-01-v1",
    goto = function(tab, ...) went <<- c(list(tab = tab), list(...))
  ), {
    expect_match(review_html(output$result), "先在左邊選一個修改包")
    session$setInputs(file = review_page_file(list(reason = "會議決議", submitted_by = "小明"),
                                              base_version = "2026-11-01-v1"))
    html <- review_html(output$result)
    expect_match(html, "修改包_test.xlsx")
    expect_match(html, "共 1 列改動")
    expect_match(html, "從現在的題庫（2026-11-01-v1 版）做的")
    expect_match(html, "檢查通過")
    expect_match(html, "影響到：題目 1 個")
    expect_match(html, "改前改後（1 列）")
    expect_length(review()$blocking, 0L)
    # 修改包裡有記的原因會先填進欄位（testServer 不會真的改 input，這裡看讀到的值）
    expect_identical(pkg()$meta$reason, "會議決議")

    # 還沒填原因和審核人：不能下載
    dl <- review_html(output$download_ui)
    expect_match(dl, "修改原因、審核人")
    expect_false(grepl("kit-download", dl))
    expect_match(dl, "懂技術的人")
    expect_match(dl, "重新部署")
    expect_false(grepl("password|GitHub", dl, ignore.case = TRUE))

    session$setInputs(reason = "會議決議", submitted_by = "小明", approved_by = "乙")
    dl <- review_html(output$download_ui)
    expect_match(dl, "kit-download")
    expect_match(dl, paste0("從 2026-11-01-v1 版變成 ", format(Sys.Date()), "-v1 版"))
    res <- kitDesign::kit_test_download(session, "dl_zip")
    expect_identical(res$filename, paste0("kit_", format(Sys.Date()), "-v1.zip"))
    ls <- zip::zip_list(res$content$file)$filename
    expect_true(all(c("kit/BANK_VERSION", "kit/item_versions.csv", paste0("kit/changes/", format(Sys.Date()), "-v1.csv")) %in% ls))
    d <- withr::local_tempdir()
    zip::unzip(res$content$file, exdir = d)
    rec <- readr::read_csv(file.path(d, "kit", "changes", paste0(format(Sys.Date()), "-v1.csv")),
                           col_types = readr::cols(.default = "c"), na = character(0))
    expect_identical(unique(rec$approved_by), "乙")
    expect_identical(unique(rec$reason), "會議決議")
    expect_identical(unique(rec$submitted_by), "小明")

    # 點一題 → 題項查詢
    session$setInputs(items_dt_rows_selected = 1L)
    expect_identical(went, list(tab = "search", item = "fatherinvo01n", uid = "P.fatherinvo01n"))
  })
})

test_that("an old base version warns; a broken file shows an error", {
  shiny::testServer(page_review_server, args = list(
    kit = kit_builtin(), version = "2026.3", goto = function(...) NULL
  ), {
    session$setInputs(file = review_page_file(base_version = "2026.1"))
    expect_match(review_html(output$result), "從 2026.1 版題庫做的，現在的題庫是 2026.3 版")

    bad <- withr::local_tempfile(fileext = ".xlsx")
    writeLines("not excel", bad)
    session$setInputs(file = data.frame(name = "壞的.xlsx", size = 1, type = "", datapath = bad,
                                        stringsAsFactors = FALSE))
    expect_match(review_html(output$result), "讀修改包「壞的.xlsx」")
    expect_null(review_html(output$download_ui) %||% NULL)
  })
})

test_that("a package that breaks the bank cannot be downloaded; tables render", {
  f <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(
    "說明" = data.frame("項目" = "base_version", "內容" = kit_version(), check.names = FALSE),
    "改動" = data.frame(op = c("add", "delete"), table = "groups", key = c("P.新題組", "P.沒有"),
                      column = "", from = "", to = "")
  ), f)
  shiny::testServer(page_review_server, args = list(
    kit = kit_builtin(), goto = function(...) NULL
  ), {
    session$setInputs(file = data.frame(name = "壞.xlsx", size = 1, type = "", datapath = f,
                                        stringsAsFactors = FALSE),
                      reason = "r", approved_by = "a")
    html <- review_html(output$result)
    expect_match(html, "套不上的改動")
    expect_match(html, "新的結構問題，不能套用")
    dl <- review_html(output$download_ui)
    expect_match(dl, "1 列改動套不上")
    expect_false(grepl("kit-download", dl))
    expect_match(output$issues_dt, "什麼時候有的")
    expect_match(output$changes_dt, "改後")
    expect_true("必填的欄是空白" %in% review_issue_table(review()$issues)$問題)
  })
})

test_that("page_review_ui builds without any password field", {
  ui <- as.character(page_review_ui("review"))
  expect_match(ui, "review-file")
  expect_match(ui, "改動紀錄的格式")
  expect_false(grepl("password", ui, ignore.case = TRUE))
})
