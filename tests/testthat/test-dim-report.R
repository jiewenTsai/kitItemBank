# 構面報告：標題格式、Word 與 HTML 檔（不需要 Quarto）

test_that("format_dim_report_heading adds the alias in brackets only when it differs", {
  expect_identical(format_dim_report_heading("學習輔助", "學習輔助AB"), "學習輔助（學習輔助AB）")
  expect_identical(format_dim_report_heading("學習輔助", ""), "學習輔助")
  expect_identical(format_dim_report_heading("學習輔助"), "學習輔助")
  expect_identical(format_dim_report_heading("學習輔助", "學習輔助"), "學習輔助")
})

test_that("the Word report is written", {
  skip_if_not_installed("officer")
  rd <- dim_report_data(.bank("parent"), "M036小", domain = "parent")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  generate_dim_report_docx(rd, out)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 5000L)
  txt <- officer::docx_summary(officer::read_docx(out))$text
  expect_true("KIT 構面題項對應報告" %in% txt)
  expect_true("要注意" %in% txt)
  expect_true(any(grepl("注意力（注意力/執行功能）", txt, fixed = TRUE)))
})

test_that("the HTML report works without Quarto", {
  rd <- dim_report_data(.bank("parent"), "M036小", domain = "parent")
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  render_dim_report_file(rd, out, use_quarto = FALSE)
  html <- paste(readLines(out, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_match(html, "KIT 構面題項對應報告", fixed = TRUE)
  expect_match(html, "題庫：家長", fixed = TRUE)
  expect_match(html, "沒有信度", fixed = TRUE)
  expect_match(html, "<th>itemVer</th>", fixed = TRUE)
})

test_that("a one-dimension report has only that dimension", {
  rd <- dim_report_data(.bank("parent"), "M036大", domain = "parent", dims = "3C產品環境")
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  render_dim_report_file(rd, out, use_quarto = FALSE)
  html <- paste(readLines(out, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_match(html, "1 個構面", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("<h2>", html, fixed = TRUE))), 1L)
  expect_match(html, "<h2>3C產品環境", fixed = TRUE)
  skip_if_not_installed("officer")
  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(docx), add = TRUE)
  generate_dim_report_docx(rd, docx)
  txt <- officer::docx_summary(officer::read_docx(docx))$text
  expect_true(any(grepl("1 個構面、11 題", txt, fixed = TRUE)))
})
