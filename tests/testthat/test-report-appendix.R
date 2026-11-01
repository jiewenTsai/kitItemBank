# report_appendix.R：期中／期末報告附表（資料表、Word、Excel、施測頁的選單和下載鈕）

# 假題庫：A19（2019 開始，題本 F-A）、A20（2020 開始，題本 F-B）
#   x1 第 1 版 → 第 2 版（改版，從 A20 起）；y1 → y2（換題號，從 A20 起）
#   form_notes：F-A 2019、F-B 2020、F-B 2020.09
ra_fake_kit <- function() {
  t <- function(tb, ...) conform_kit_table(data.frame(..., stringsAsFactors = FALSE), tb)
  k <- empty_kit()
  k$cohorts <- t("cohorts", cohortID = "K1", code = "M03")
  k$admins <- t("admins", adminID = c("A19", "A20"), cohortID = "K1", ageMonth = c("36", "48"),
                surveyFrom = c("2019-03-01", "2020-03-01"), surveyTo = c("2020-02-28", "2021-02-28"),
                status = "released")
  k$forms <- t("forms", formID = c("F-A", "F-B"), instrument = "P", label = c("家長 A", "家長 B"))
  k$datasets <- t("datasets", datasetID = c("A19-P", "A20-P"), adminID = c("A19", "A20"),
                  file = "data1", instrument = "P", formID = c("F-A", "F-B"), respondent = "家長")
  k$items <- t("items", item_uid = c("P.x1", "P.y1", "P.y2"), instrument = "P",
               varname = c("x1", "y1", "y2"))
  k$item_versions <- t("item_versions", item_uid = c("P.x1", "P.x1", "P.y1", "P.y2"),
                       itemVer = c("1", "2", "1", "1"), label = c("x 舊", "x 新", "y 舊", "y 新"))
  k$dataset_vars <- t("dataset_vars", datasetID = c("A19-P", "A19-P", "A20-P", "A20-P"),
                      varname = c("x1", "y1", "x1", "y2"), item_uid = c("P.x1", "P.y1", "P.x1", "P.y2"),
                      itemVer = c("1", "1", "2", "1"), kind = "item")
  k$item_relations <- t("item_relations", from_uid = c("P.x1", "P.y1"), from_ver = "1",
                        to_uid = c("P.x1", "P.y2"), to_ver = c("2", "1"),
                        kind = c("revised", "renamed"), effective_admin = "A20",
                        reason = c("選項改寫", "換題號"), decided_by = "PI")
  k$form_notes <- t("form_notes", formID = c("F-A", "F-B", "F-B"), year = c("2019", "2020", "2020.09"),
                    summary = c("原版", "新增 y2", "年度中途加題"), reason = c("", "調查年度改版", ""))
  k$groups <- t("groups", groupID = "G1", label = "親職")
  k
}

ra_with_stats <- function(k) {
  t <- function(tb, ...) conform_kit_table(data.frame(..., stringsAsFactors = FALSE), tb)
  k$analyses <- t("analyses", analysisID = c("AN20", "AN20x"), datasetID = "A20-P",
                  official = c("1", "0"), date = "2026-10-01")
  k$group_stats <- t("group_stats", analysisID = c("AN20", "AN20", "AN20x"), groupID = "G1",
                     varname = c("", "x1", ""), n_items = "2", n = "120",
                     alpha = c("0.812", "", "0.5"), omega_total = c("0.83", "", "0.5"))
  k
}

test_that("report_appendix() collects one year's form changes", {
  k <- ra_fake_kit()
  expect_equal(nrow(check_kit(k)), 0L)
  app <- report_appendix(k, year = 2020, version = "2026.9", date = as.Date("2026-11-02"))
  expect_identical(app$scope$admins, "A20")
  expect_identical(app$scope$version, "2026.9")
  expect_match(app$scope$text, "2020")
  fc <- app$form_changes
  expect_identical(fc$formID, "F-B")
  expect_identical(fc$n_revised, 1L)
  expect_identical(fc$n_renamed, 1L)
  expect_identical(fc$n_relations, 2L)
  expect_match(fc$notes, "2020：新增 y2")
  expect_match(fc$notes, "2020.09：年度中途加題")
  d <- app$change_details
  expect_equal(nrow(d), 4L)
  expect_identical(d$what, c("題本說明", "題本說明", "題目", "題目"))
  expect_true(any(grepl("P.x1 從第 1 版改成第 2 版（從 A20 起）", d$text, fixed = TRUE)))
  expect_true(any(grepl("P.y1 第 1 版 換題號，變成 P.y2 第 1 版", d$text, fixed = TRUE)))
  expect_identical(app$admins$adminID, "A20")
  expect_match(app$admins$files, "A20-P")
  expect_identical(app$admins$n_items, 2L)

  y19 <- report_appendix(k, year = "2019")
  expect_identical(y19$form_changes$formID, "F-A")
  expect_identical(y19$form_changes$n_relations, 0L)
  all <- report_appendix(k)
  expect_setequal(all$scope$admins, c("A19", "A20"))
  expect_equal(nrow(all$change_details), 5L)
})

test_that("admins override the year and unknown admins stop", {
  k <- ra_fake_kit()
  app <- report_appendix(k, year = 2020, admins = "A19")
  expect_identical(app$scope$admins, "A19")
  # 題本說明：A19 的題本、年份照 year
  expect_equal(nrow(app$form_changes), 0L)
  app <- report_appendix(k, admins = "A19")
  expect_identical(app$form_changes$formID, "F-A")
  expect_error(report_appendix(k, admins = "ZZZ"), "ZZZ")
})

test_that("reliability uses official group-level stats, with a note when there are none", {
  k <- ra_fake_kit()
  app <- report_appendix(k, year = 2020)
  expect_equal(nrow(app$reliability), 0L)
  expect_match(app$notes$reliability, "還沒有正式")
  k <- ra_with_stats(k)
  app <- report_appendix(k, year = 2020)
  r <- app$reliability
  expect_equal(nrow(r), 1L)
  expect_identical(r$analysisID, "AN20")
  expect_identical(r$alpha, "0.812")
  expect_identical(r$group_label, "親職")
  expect_null(app$notes$reliability)
  app19 <- report_appendix(k, year = 2019)
  expect_match(app19$notes$reliability, "選定的施測")
})

test_that("report_appendix() works on the bundled bank", {
  app <- report_appendix(read_kit(), year = 2020)
  expect_true(nrow(app$admins) >= 1L)
  expect_true(nrow(app$form_changes) >= 1L)
  expect_true(all(app$change_details$formID %in% c(read_kit()$forms$formID, "")))
})

test_that("Word and Excel versions start with 說明", {
  k <- ra_with_stats(ra_fake_kit())
  app <- report_appendix(k, year = 2020, version = "2026.9")
  wb <- report_appendix_workbook(app)
  expect_identical(names(wb)[1], "說明")
  expect_identical(names(wb), c("說明", "題本改動摘要", "題本改動明細", "各波次信效度", "施測一覽"))
  expect_true("2026.9" %in% wb[["說明"]][["內容"]])
  expect_identical(wb[["各波次信效度"]][["α"]], "0.81")
  f <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(wb, f)
  expect_identical(readxl::excel_sheets(f)[1], "說明")

  path <- withr::local_tempfile(fileext = ".docx")
  report_appendix_docx(app, path)
  txt <- officer::docx_summary(officer::read_docx(path))$text
  expect_true("期中／期末報告附表" %in% txt)
  expect_true("說明" %in% txt)
  expect_true(any(grepl("題庫版本：2026.9", txt, fixed = TRUE)))
  expect_true(any(grepl("P.x1 從第 1 版改成第 2 版", txt, fixed = TRUE)))

  # 沒有信效度：寫說明，不放表
  app0 <- report_appendix(ra_fake_kit(), year = 2020)
  path0 <- withr::local_tempfile(fileext = ".docx")
  report_appendix_docx(app0, path0)
  txt0 <- officer::docx_summary(officer::read_docx(path0))$text
  expect_true(any(grepl("還沒有正式的信效度分析", txt0, fixed = TRUE)))
})

test_that("report_appendix_ui() builds the selector and both downloads", {
  k <- ra_with_stats(ra_fake_kit())
  srv <- function(id, kit) shiny::moduleServer(id, function(input, output, session) {
    output$ra <- shiny::renderUI(report_appendix_ui(session, kit))
  })
  suppressWarnings(shiny::testServer(srv, args = list(kit = k), {
    session$setInputs(ra_year = "2020", ra_admins = NULL)
    html <- output$ra$html
    expect_match(html, "下載報告附表（Word）")
    expect_match(html, "下載報告附表（Excel）")
    expect_match(html, "2019")
    res <- kitDesign::kit_test_download(session, "dl_ra_xlsx")
    expect_match(res$filename, "^報告附表_[0-9]{8}\\.xlsx$")
    expect_identical(readxl::excel_sheets(res$content$file)[1], "說明")
    rel <- readxl::read_excel(res$content$file, sheet = "各波次信效度")
    expect_equal(nrow(rel), 1L)
    res <- kitDesign::kit_test_download(session, "dl_ra_docx")
    expect_match(res$filename, "\\.docx$")
    expect_gt(file.size(res$content$file), 0)
  }))
})
