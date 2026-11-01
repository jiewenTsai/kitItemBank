# tests/testthat/test-page-merge.R — 併入新施測頁（testServer），合成的題庫和表頭

mg_kit <- function() {
  df <- function(...) data.frame(..., stringsAsFactors = FALSE)
  kit <- empty_kit()
  kit$cohorts <- conform_kit_table(df(cohortID = "K1", code = "M03", birth_from = "2016-04-01"),
                                   "cohorts")
  kit$admins <- conform_kit_table(df(adminID = "M03M036", cohortID = "K1", ageMonth = "36",
                                     waveNo = "6", status = "internal"), "admins")
  kit$datasets <- conform_kit_table(df(datasetID = c("M03M036-P", "M03M036-T"), adminID = "M03M036",
                                       file = c("data1", "data3"), instrument = c("P", "T"),
                                       missingScheme = "2016"), "datasets")
  kit$missing_schemes <- conform_kit_table(df(scheme = "2016", code = "9999", label = "遺漏值"),
                                           "missing_schemes")
  kit$items <- conform_kit_table(df(item_uid = "P.a01", instrument = "P", varname = "a01",
                                    block = "b1"), "items")
  kit$item_versions <- conform_kit_table(df(item_uid = "P.a01", itemVer = "1", label = "穿衣服",
                                            valueSetID = "vs0001"), "item_versions")
  kit$value_codes <- conform_kit_table(df(valueSetID = "vs0001", code = c("0", "1"),
                                          label = c("不會", "會")), "value_codes")
  kit$dataset_vars <- conform_kit_table(df(datasetID = "M03M036-P", varname = "a01",
                                           item_uid = "P.a01", itemVer = "1", kind = "item",
                                           order = "1"), "dataset_vars")
  kit
}

mg_header_csv <- function(dir, name) {
  f <- file.path(dir, name)
  utils::write.csv(data.frame(var = c("a01", "z09"), class = "numeric",
                              label = c("一.1.穿衣服", "一.2.新的題目"), n_vlab = "2",
                              vlabs = c("0=不會; 1=會; 9999=遺漏值", "1=是; 2=否"), na = ""),
                   f, row.names = FALSE, fileEncoding = "UTF-8")
  f
}

test_that("merge_guess_dataset_ids maps file names to dataset IDs", {
  kit <- mg_kit()
  row <- am_admin_row(kit, "M03M048")
  expect_equal(merge_guess_dataset_ids(kit, row, c("data1_meta.csv", "DATA3.sav", "M03M048-PK.csv",
                                                   "other.sav")),
               c("M03M048-P", "M03M048-T", "M03M048-PK", NA))
})

mg_html <- function(x) if (is.list(x)) x$html else as.character(x)

test_that("upload a header, get proposals, download the Excel; the upload is deleted", {
  dir <- withr::local_tempdir()
  f <- mg_header_csv(dir, "upload1.csv")
  suppressWarnings(shiny::testServer(page_merge_server, args = list(kit = mg_kit(), goto = function(...) NULL), {
    session$flushReact()
    expect_match(mg_html(output$result), "還沒有提案")
    session$setInputs(files = data.frame(name = "data1_meta.csv", size = 1, type = "text/csv",
                                         datapath = f, stringsAsFactors = FALSE))
    expect_false(file.exists(f))
    expect_equal(names(headers()), "data1_meta.csv")
    expect_match(mg_html(output$files_ui), "2 個變項")

    session$setInputs(run = 1)
    expect_null(proposals())
    session$setInputs(admin = "M03M048", run = 2)
    p <- proposals()
    expect_equal(p$class[p$varname == "a01"], "same")
    expect_equal(p$class[p$varname == "z09"], "new_item")
    html <- mg_html(output$result)
    expect_match(html, "M03M048 的併入提案")
    expect_match(html, "和上一次的 M03M036-P 比")
    res <- kitDesign::kit_test_download(session, "dl_xlsx")
    expect_match(res$filename, "^併入提案_M03M048_[0-9]{8}\\.xlsx$")
    expect_identical(readxl::excel_sheets(res$content$file)[1], "說明")
    expect_match(mg_html(output$`code-code`), "propose_admin_merge")
  }))
})

test_that("unknown file names give a plain message instead of proposals", {
  dir <- withr::local_tempdir()
  f <- mg_header_csv(dir, "x.csv")
  suppressWarnings(shiny::testServer(page_merge_server, args = list(kit = mg_kit(), goto = function(...) NULL), {
    session$setInputs(files = data.frame(name = "weird.csv", size = 1, type = "text/csv",
                                         datapath = f, stringsAsFactors = FALSE),
                      admin = "M03M048", run = 1)
    expect_null(proposals())
  }))
  ui <- as.character(page_merge_ui("merge"))
  expect_match(ui, "新施測的代號")
})
