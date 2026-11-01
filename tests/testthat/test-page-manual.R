# app.R：「說明 → 操作手冊」分頁

test_that("the manual tab shows the manual with a table of contents and the glossary", {
  html <- as.character(page_manual_ui())
  expect_match(html, "km-toc")
  expect_match(html, "術語表（中英對照）")
  expect_match(html, "kit-manual/manual.html")
  expect_no_match(html, 'id="目錄"')
  expect_true(file.exists(system.file("manual", "manual.html", package = "kitItemBank")))
  expect_true("manual" %in% APP_TABS)
  expect_match(as.character(app_ui(NULL)), "說明")
})
