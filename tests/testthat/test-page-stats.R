# page_stats.R：統計頁（預設關閉；打開時只用小的模擬資料測）

stats_page_kit <- function() {
  kit <- empty_kit()
  kit$forms <- conform_kit_table(data.frame(formID = "F1", instrument = "P", label = "F1"), "forms")
  kit$datasets <- conform_kit_table(data.frame(datasetID = "A1-P", adminID = "A1", file = "data1",
                                               instrument = "P", formID = "F1", respondent = "P",
                                               missingScheme = "2016"), "datasets")
  kit$missing_schemes <- conform_kit_table(data.frame(scheme = "2016", code = c("7777", "9999")),
                                           "missing_schemes")
  uid <- paste0("P.x", 1:3)
  kit$item_versions <- conform_kit_table(data.frame(item_uid = uid, itemVer = "1", valueSetID = "vs"),
                                         "item_versions")
  kit$value_codes <- conform_kit_table(data.frame(valueSetID = "vs", code = 1:4), "value_codes")
  kit$dataset_vars <- conform_kit_table(data.frame(datasetID = "A1-P", varname = paste0("x", 1:3),
                                                   item_uid = uid, itemVer = "1", kind = "item"),
                                        "dataset_vars")
  kit$group_members <- conform_kit_table(data.frame(formID = "F1", groupID = "G", item_uid = uid,
                                                    scheme = "official", reverse = "0"),
                                         "group_members")
  kit
}

test_that("the statistics page is disabled by default", {
  withr::local_options(kititembank.allow_data_upload = NULL)
  expect_false(stats_upload_allowed())
  html <- as.character(page_stats_ui("stats"))
  # 原文：本機版本
  expect_match(html, "\u672c\u6a5f\u7248\u672c")            # 本機版本
  expect_match(html, "kititembank.allow_data_upload", fixed = TRUE)
  expect_false(grepl("type=\"file\"", html, fixed = TRUE))
  shiny::testServer(page_stats_server, args = list(kit = stats_page_kit()), {
    expect_null(session$returned)
  })
  withr::local_options(kititembank.allow_data_upload = "yes")
  expect_false(stats_upload_allowed())
})

test_that("when enabled, an upload is read into memory, deleted and summarised", {
  skip_if_not_installed("psych")
  withr::local_options(kititembank.allow_data_upload = TRUE)
  html <- as.character(page_stats_ui("stats"))
  expect_match(html, "type=\"file\"", fixed = TRUE)

  set.seed(1)
  n <- 60
  f <- stats::rnorm(n)
  d <- as.data.frame(lapply(c(.8, .7, .9), function(l) pmin(pmax(round(2.5 + l * f + stats::rnorm(n)), 1), 4)))
  names(d) <- c("X1", "X2", "X3")
  d$X1[1:2] <- 7777
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(d, path)

  suppressWarnings(shiny::testServer(page_stats_server, args = list(kit = stats_page_kit()), {
    session$setInputs(dataset = "A1-P", run_by = "tester", official = FALSE, note = "")
    session$setInputs(file = data.frame(name = "wave.csv", size = 1, type = "text/csv",
                                        datapath = path, stringsAsFactors = FALSE))
    expect_false(file.exists(path))
    info <- data_info()
    expect_identical(info$n_match, 3L)
    r <- result()
    expect_identical(nrow(r$item_stats), 3L)
    expect_identical(r$item_stats$missing[r$item_stats$varname == "x1"], "7777=<5; 9999=0")
    expect_identical(r$analyses$run_by, "tester")
    expect_true(nzchar(r$group_stats$alpha[1]))
    # 改執行者不換分析代號
    id <- r$analyses$analysisID
    session$setInputs(run_by = "other")
    expect_identical(result()$analyses$analysisID, id)
    expect_identical(result()$analyses$run_by, "other")
    # 原文：下載統計
    expect_match(as.character(output$result$html), "\u4e0b\u8f09\u7d71\u8a08")  # 下載統計
    expect_match(output$items_dt, "miss_rate", fixed = TRUE)
    expect_match(output$groups_dt, "alpha", fixed = TRUE)
    # 清除資料：記憶體裡的資料丟掉
    session$setInputs(clear = 1)
    expect_null(uploaded())
  }))
})
