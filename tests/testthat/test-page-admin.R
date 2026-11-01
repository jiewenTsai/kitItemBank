# page_admin.R：施測頁（testServer）

.kit_cache <- new.env()
admin_kit <- function() {
  if (is.null(.kit_cache$kit)) .kit_cache$kit <- read_kit()
  .kit_cache$kit
}
admin_html <- function(x) if (is.list(x)) x$html else as.character(x)
# server 模式的下拉選單在 testServer 裡會說 registerDataObj 沒作用：不是這裡要測的
admin_test <- function(args, expr) {
  q <- rlang::enquo(expr)
  suppressWarnings(rlang::inject(shiny::testServer(page_admin_server, args = args, !!q)))
}

test_that("the admin list shows until an admin is picked; then the codebook", {
  admin_test(list(
    kit = shiny::reactive(admin_kit()), goto = function(...) NULL, request = shiny::reactiveVal(NULL)
  ), {
    session$flushReact()
    expect_null(current())
    expect_match(admin_html(output$browse), "施測清單（24 次）")
    expect_match(admin_html(output$dataset_ui), "先選一次施測")
    expect_identical(nrow(overview()), 24L)

    session$setInputs(admin = "M03M036")
    expect_identical(current(), "M03M036")
    html <- admin_html(output$browse)
    expect_match(html, "M03 世代 36 月齡（2019.03–2020.08）")
    expect_match(html, "回到施測清單")
    expect_lt(regexpr("kit-buttons", html), regexpr("<h4>", html))
    expect_match(html, "2019 年 3 月到 2020 年 8 月")
    expect_match(html, "釋出編號是 D00229")
    expect_match(html, "親友版沒問：")
    expect_match(html, "7777 拒答")
    expect_match(html, "建議引用")
    expect_match(admin_html(output$dataset_ui), "家長（data1）")

    # 沒選資料檔 → 第一個
    expect_identical(current_ds(), "M03M036-P")
    expect_match(admin_html(output$vars_head), "家長（data1）的變項（721 個）")
    session$setInputs(dataset = "M03M036-PK")
    expect_identical(current_ds(), "M03M036-PK")
    expect_match(admin_html(output$vars_head), "親友（data2）的變項")
    expect_match(admin_html(output$downloads), "下載 SPSS 標籤（親友）")
    expect_true(all(vars()$datasetID == "M03M036-PK"))

    # 換施測：資料檔回到新施測的第一個
    session$setInputs(admin = "M36M084G01")
    expect_identical(current_ds(), "M36M084G01-P")
    expect_match(admin_html(output$browse), "M36 世代 84 月齡・小一")

    session$setInputs(back = 1)
    expect_null(current())
    # 點施測清單的一列 = 選這次施測
    session$setInputs(summary_dt_rows_selected = 8L)
    expect_identical(current(), overview()$adminID[8])
    expect_match(admin_html(output$browse), "內部：還沒有公開釋出")
  })
})

test_that("kit can be a plain list; downloads give Excel and SPSS files", {
  admin_test(list(
    kit = admin_kit(), goto = function(...) NULL, request = shiny::reactiveVal(NULL)
  ), {
    session$setInputs(admin = "M03M036", dataset = "M03M036-P")
    dl <- admin_html(output$downloads)
    expect_match(dl, "下載施測題目清單（Excel）")
    expect_match(dl, "下載 SPSS 標籤（家長）")

    res <- kitDesign::kit_test_download(session, "dl_xlsx")
    expect_match(res$filename, "^施測題目清單_M03M036_[0-9]{8}\\.xlsx$")
    expect_identical(readxl::excel_sheets(res$content$file)[1], "說明")

    res <- kitDesign::kit_test_download(session, "dl_sps")
    expect_match(res$filename, "^SPSS標籤_M03M036-P_[0-9]{8}\\.sps$")
    txt <- readLines(res$content$file, encoding = "UTF-8", warn = FALSE)
    expect_match(txt[length(txt)], "EXECUTE.", fixed = TRUE)
    expect_match(paste(txt, collapse = "\n"), "VARIABLE LABELS")

    code <- admin_html(output$`code-code`)
    expect_match(code, "admin_codebook")
  })
})

test_that("clicking an item goes to 題項查詢; a system variable does not", {
  went <- NULL
  admin_test(list(
    kit = admin_kit(), request = shiny::reactiveVal(NULL),
    goto = function(tab, ...) went <<- c(list(tab = tab), list(...))
  ), {
    session$setInputs(admin = "M03M036", dataset = "M03M036-P")
    v <- vars()
    i <- which(v$varname == "cogc01")
    session$setInputs(vars_dt_rows_selected = i)
    expect_identical(went, list(tab = "search", item = "cogC01", ver = 1L, uid = "P.cogC01"))

    went <<- NULL
    session$setInputs(vars_dt_rows_selected = which(!nzchar(v$item_uid))[1])
    expect_null(went)
  })
})

test_that("request() opens an admin by ID or alias, and a data file", {
  req <- shiny::reactiveVal(NULL)
  admin_test(list(kit = admin_kit(), goto = function(...) NULL, request = req), {
    session$flushReact()
    req(list(tab = "search", args = list(admin = "M036大"), at = Sys.time()))
    session$flushReact()
    expect_null(current())
    req(list(tab = "admin", args = list(admin = "M036大"), at = Sys.time()))
    session$flushReact()
    expect_identical(current(), "M36M036")
    req(list(tab = "admin", args = list(admin = "M36W84", dataset = "M36M084G01-T"),
             at = Sys.time()))
    session$flushReact()
    expect_identical(current(), "M36M084G01")
    expect_identical(current_ds(), "M36M084G01-T")
    req(list(tab = "admin", args = list(admin = "沒有這個"), at = Sys.time()))
    session$flushReact()
    expect_identical(current(), "M36M084G01")
  })
})

test_that("用 R 怎麼做 parses and uses the exported functions", {
  code <- admin_code("M03M036", "M03M036-P")
  expect_match(code, "kitItemBank::admin_codebook(kit, \"M03M036\")", fixed = TRUE)
  expect_match(code, "kitItemBank::codebook_sps(kit, \"M03M036-P\")", fixed = TRUE)
  expect_silent(parse(text = code))
})
