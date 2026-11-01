# page_revision.R：改版頁（testServer，用 test-revision.R 的小題庫）

rev_html <- function(x) if (is.list(x)) x$html else as.character(x)

test_that("the revision page exports a template, checks an upload and gives a change package", {
  skip_if_not_installed("openxlsx")
  k <- rv_kit()
  ui <- as.character(page_revision_ui("rev"))
  expect_match(ui, "下載改版底稿")
  expect_match(ui, "新施測的代號")

  shiny::testServer(page_revision_server, args = list(kit = shiny::reactive(k)), {
    session$setInputs(form = "P-G1-G3")
    expect_identical(base()$dataset$datasetID, "M03M096G02-P")
    expect_match(rev_html(output$base_ui), "5 題")
    expect_match(rev_html(output$result), "傳上填好的改版底稿")

    tpl <- kitDesign::kit_test_download(session, "dl_template")
    expect_match(tpl$filename, "^改版底稿_P-G1-G3_[0-9]{8}\\.xlsx$")
    f <- tpl$content$file
    expect_identical(readxl::excel_sheets(f), c("說明", "題目", "新增題目"))

    # 有擋下來的問題：不給修改包
    bad <- withr::local_tempfile(fileext = ".xlsx")
    rv_fill(f, bad, list(cell(1, 6, "改文字")))
    session$setInputs(upload = data.frame(name = "x.xlsx", size = 1, type = "", datapath = bad))
    html <- rev_html(output$result)
    expect_match(html, "要先改好")
    expect_false(grepl("下載修改包", html))
    expect_match(rev_html(output$problems_dt), "嚴重度")
    expect_true(any(grepl("新題目文字", check()$problems$message)))

    good <- withr::local_tempfile(fileext = ".xlsx")
    rv_fill(f, good, list(cell(1, 6, "改文字"), cell(1, 7, "新的文字"), cell(2, 6, "刪除")))
    session$setInputs(upload = data.frame(name = "y.xlsx", size = 1, type = "", datapath = good))
    expect_match(rev_html(output$result), "寫新施測的代號")
    session$setInputs(admin_id = "M03M096G02")
    expect_match(rev_html(output$result), "已經有了")
    session$setInputs(admin_id = "M03M108G03")
    html <- rev_html(output$result)
    expect_match(html, "規劃中的施測 M03M108G03")
    expect_match(html, "改文字 1 題")
    res <- kitDesign::kit_test_download(session, "dl_patch")
    expect_match(res$filename, "^修改包_改版_M03M108G03_[0-9]{8}\\.xlsx$")
    p <- read_patch_xlsx(res$content$file)$patch
    expect_equal(nrow(check_kit(apply_patch(k, p))), 0L)
    expect_match(rev_html(output$`code-code`), "revision_patch")
  })
})
