# tests/testthat/test-compare.R — compare_datasets()、compare_workbook()、比較頁（testServer）

cmp_fixture <- function() {
  df <- function(...) data.frame(..., stringsAsFactors = FALSE)
  kit <- empty_kit()
  kit$cohorts <- conform_kit_table(df(cohortID = "K1", code = "M03", birth_from = "2016-04-01"),
                                   "cohorts")
  kit$admins <- conform_kit_table(df(adminID = c("M03M036", "M03M048"), cohortID = "K1",
                                     ageMonth = c("36", "48"), status = "internal"), "admins")
  kit$datasets <- conform_kit_table(df(
    datasetID = c("M03M036-P", "M03M048-P"), adminID = c("M03M036", "M03M048"),
    file = "data1", instrument = "P", respondent = "家長", missingScheme = c("2016", "2020")),
    "datasets")
  kit$missing_schemes <- conform_kit_table(df(scheme = c("2016", "2020", "2020"),
                                              code = c("9999", "9995", "9999"),
                                              label = c("遺漏值", "不適用", "遺漏值")),
                                           "missing_schemes")
  kit$items <- conform_kit_table(df(
    item_uid = c("P.a01", "P.b01", "P.c01", "P.c01n", "P.d01", "P.e01", "P.f01"), instrument = "P",
    varname = c("a01", "b01", "c01", "c01n", "d01", "e01", "f01")), "items")
  kit$item_versions <- conform_kit_table(df(
    item_uid = c("P.a01", "P.b01", "P.b01", "P.c01", "P.c01n", "P.d01", "P.e01", "P.f01"),
    itemVer = c("1", "1", "2", "1", "1", "1", "1", "1"),
    label = c("穿衣服", "看書時間", "看書次數", "童書", "童書", "騎車", "游泳", "畫畫"),
    valueSetID = c("vs1", "vs1", "vs2", "vs1", "vs1", "vs1", "vs1", "vs1")), "item_versions")
  kit$value_codes <- conform_kit_table(df(valueSetID = c("vs1", "vs1", "vs2"),
                                          code = c("0", "1", "1"), label = c("不會", "會", "很少")),
                                       "value_codes")
  kit$item_relations <- conform_kit_table(df(from_uid = "P.c01", from_ver = "1", to_uid = "P.c01n",
                                             to_ver = "1", kind = "renamed"), "item_relations")
  kit$dataset_vars <- conform_kit_table(df(
    datasetID = c(rep("M03M036-P", 6), rep("M03M048-P", 6)),
    varname = c("baby_sex", "a01", "b01", "c01", "e01", "f01",
                "BABY_SEX", "a01", "b01", "c01n", "d01", "f01"),
    item_uid = c("", "P.a01", "P.b01", "P.c01", "P.e01", "P.f01",
                 "", "P.a01", "P.b01", "P.c01n", "P.d01", "P.f01"),
    itemVer = c("", "1", "1", "1", "1", "1", "", "1", "2", "1", "1", "1"),
    kind = c("system", rep("item", 5), "system", rep("item", 5)),
    position = c("", "一.1", "一.2", "一.3", "一.4", "一.5", "", "一.1", "一.2", "一.3", "一.4", "一.9"),
    order = as.character(c(1:6, 1:6))), "dataset_vars")
  kit
}

test_that("compare_datasets lists only the differences", {
  kit <- cmp_fixture()
  expect_equal(nrow(check_kit(kit)), 0L)
  d <- compare_datasets(kit, "M03M036-P", "M03M048-P")
  key <- paste(d$type, d$varname_a, d$varname_b)
  expect_setequal(key, c("missing_scheme  ", "only_a e01 ", "only_b  d01", "renamed c01 c01n",
                         "version b01 b01", "values b01 b01", "position f01 f01"))
  # 系統變項用名稱對（不分大小寫）；a01 一樣 → 不列
  expect_false(any(d$varname_a %in% c("baby_sex", "a01")))
  expect_equal(d$type[1], "missing_scheme")
  expect_match(d$values_b[1], "9995")
  v <- d[d$type == "values", ]
  expect_equal(c(v$values_a, v$values_b), c("0=不會; 1=會", "1=很少"))
  expect_equal(compare_var_cell(d[d$type == "renamed", ]), "c01 → c01n")
  cells <- compare_cells(d)
  expect_equal(cells$a[d$type == "version"], "第 1 版")
  expect_equal(cells$b[d$type == "only_a"], "沒有")
  # 同一個檔 → 沒有差異
  expect_equal(nrow(compare_datasets(kit, "M03M036-P", "M03M036-P")), 0L)
  expect_error(compare_datasets(kit, "M03M036-P", "nope"), "no data file")
})

test_that("compare_workbook starts with 說明", {
  kit <- cmp_fixture()
  wb <- compare_workbook(kit, "M03M036-P", "M03M048-P")
  expect_equal(names(wb), c("說明", "差異"))
  expect_equal(nrow(wb$差異), 7L)
  expect_true(all(c("差異", "變項", "題目", "A", "B") %in% names(wb$差異)))
})

cmp_html <- function(x) if (is.list(x)) x$html else as.character(x)
cmp_test <- function(args, expr) {
  q <- rlang::enquo(expr)
  suppressWarnings(rlang::inject(shiny::testServer(page_compare_server, args = args, !!q)))
}

test_that("the compare page shows a hint, then the differences; download works", {
  went <- NULL
  cmp_test(list(kit = cmp_fixture(), goto = function(...) went <<- list(...)), {
    session$flushReact()
    expect_match(cmp_html(output$result), "還沒選好資料檔")
    session$setInputs(a = "M03M036-P", b = "M03M036-P", types = names(COMPARE_TYPES))
    expect_match(cmp_html(output$result), "同一個資料檔")
    session$setInputs(b = "M03M048-P")
    html <- cmp_html(output$result)
    expect_match(html, "差異（7 處）")
    expect_match(html, "只在 A：1 處")
    expect_match(html, "M03 世代 36 月齡")
    expect_equal(nrow(shown()), 7L)
    session$setInputs(types = c("version", "position"))
    expect_equal(nrow(shown()), 2L)
    # 點一列 → 題項查詢
    session$setInputs(diff_dt_rows_selected = which(shown()$type == "version"))
    expect_equal(went[[1]], "search")
    expect_equal(went$uid, "P.b01")
    expect_equal(went$ver, 1L)

    expect_match(cmp_html(output$downloads), "下載差異清單（Excel）")
    res <- kitDesign::kit_test_download(session, "dl_xlsx")
    expect_match(res$filename, "^差異清單_M03M036-P_M03M048-P_[0-9]{8}\\.xlsx$")
    expect_identical(readxl::excel_sheets(res$content$file)[1], "說明")
    expect_match(cmp_html(output$`code-code`), "compare_datasets")
  })
})

test_that("the compare page accepts a request from another tab", {
  req <- shiny::reactiveVal(NULL)
  cmp_test(list(kit = shiny::reactive(cmp_fixture()), goto = function(...) NULL, request = req), {
    session$flushReact()
    req(list(tab = "compare", args = list(a = "M03M036-P", b = "M03M048-P")))
    session$flushReact()
    # testServer 不會把 updateSelectizeInput 送回 input；這裡只確認不會出錯
    expect_null(input$a %||% NULL)
  })
})

test_that("the compare page UI renders", {
  ui <- as.character(page_compare_ui("compare"))
  expect_match(ui, "資料檔 A")
  expect_match(ui, "要看的差異")
})
