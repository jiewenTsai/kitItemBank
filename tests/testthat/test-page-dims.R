# page_dims.R：構面查詢頁（testServer）

dims_fake_store <- function(domain = "parent") {
  base <- shiny::reactiveVal(.bank(domain))
  bank <- shiny::reactiveVal(.bank(domain))
  list(
    domain = shiny::reactive(domain), bank = bank, base = base,
    source = shiny::reactive(list(kind = "builtin", label = "內建題庫")),
    changes = shiny::reactive(bank_diff(base(), bank())),
    set_bank = function(b) bank(b), revert = function() bank(base())
  )
}
dims_html <- function(x) if (is.list(x)) x$html else as.character(x)

test_that("the overview shows until a dimension is selected", {
  shiny::testServer(page_dims_server, args = list(
    store = dims_fake_store(), goto = function(...) NULL, request = shiny::reactiveVal(NULL)
  ), {
    session$flushReact()
    expect_null(current_dim())
    expect_match(dims_html(output$browse), "構面清單（130 個）")
    expect_match(dims_html(output$ready), "選一個波次")

    session$setInputs(dim = "注意力")
    expect_identical(current_dim(), "注意力")
    html <- dims_html(output$browse)
    expect_match(html, "「注意力（注意力/執行功能）」：9 題 · 出現在 23 個波次 · 最近一波 M144大")
    # 按鈕列在標題前面，提示小字接在標題後面（不會壓到按鈕）
    expect_lt(regexpr("kit-buttons", html), regexpr("<h4>", html))
    expect_lt(regexpr("<h4>", html), regexpr("kit-hint", html))
    expect_match(html, "只列已歸到這個構面的題")
    expect_match(html, "改這個構面 →")
    expect_match(dims_html(output$rel_ui), "這個構面還沒有信度資料")
    expect_no_match(dims_html(output$rel_ui), "題庫編修")
    expect_match(dims_html(output$overlap_ui), "和其他構面共用的題目（0 個構面）")
    expect_identical(names(items())[3:5], c("deprecated", "respondent", "label"))
    expect_match(dims_html(output$network_hint), "怎麼讀")
    expect_type(net(), "list")

    # 題數與版本數分開
    session$setInputs(dim = "親友親職參與")
    expect_match(dims_html(output$browse), "：10 題（30 個版本）")
    expect_match(dims_html(output$overlap_ui), "共用題數")

    session$setInputs(back = 1)
    expect_null(current_dim())
    expect_true(all(c("n_items", "n_versions", "last_wave") %in% names(overview())))

    # 點構面清單的一列 = 選這個構面
    session$setInputs(overview_dt_rows_selected = 2L)
    expect_identical(current_dim(), overview()$dimID[2])
  })
})

test_that("clicking an item goes to 題項查詢", {
  went <- NULL
  shiny::testServer(page_dims_server, args = list(
    store = dims_fake_store(), request = shiny::reactiveVal(NULL),
    goto = function(tab, ...) went <<- c(list(tab = tab), list(...))
  ), {
    session$setInputs(dim = "注意力")
    session$setInputs(items_dt_rows_selected = 1L)
    expect_identical(went$tab, "search")
    expect_identical(went$item, items()$itemID[1])
    expect_identical(went$ver, items()$itemVer[1])

    went <<- NULL
    session$setInputs(net_click = "I::cogA11::1")
    expect_identical(went, list(tab = "search", item = "cogA11", ver = 1L))
    session$setInputs(net_click = "D::父母參與")
    expect_identical(current_dim(), "父母參與")

    session$setInputs(edit_dim = 1)
    expect_identical(went, list(tab = "edit", dim = "父母參與"))
    session$setInputs(to_search = 1)
    expect_identical(went, list(tab = "search", keyword = "父母參與"))
  })
})

test_that("with a dimension selected, the report covers only that dimension", {
  shiny::testServer(page_dims_server, args = list(
    store = dims_fake_store(), goto = function(...) NULL, request = shiny::reactiveVal(NULL)
  ), {
    session$setInputs(dim = "3C產品環境", wave = "M036大")
    expect_match(dims_html(output$settings_line), "範圍：只有「3C產品環境」")
    session$setInputs(run = 1)
    expect_identical(tab$result()$scope, "3C產品環境")
    expect_length(tab$result()$dims, 1L)
    expect_match(tab$status()$message, "「3C產品環境」在 M036大 的構面報告：11 題。")
    expect_no_match(tab$status()$message, "個構面、")
    expect_match(dims_html(output$downloads), "下載 Word")
    # 換構面 → 要重新執行
    session$setInputs(dim = "注意力")
    expect_identical(tab$status()$state, "stale")
    # 這個波次沒有這個構面的題目
    session$setInputs(wave = "M156大", run = 2)
    expect_identical(tab$status()$state, "error")
    expect_match(tab$status()$message, "在 M156大 沒有題目")
    # 回到構面清單 → 整個波次
    session$setInputs(back = 1)
    expect_match(dims_html(output$settings_line), "範圍：這個波次的全部構面")
  })
})

test_that("running the report gives a warning status, downloads, and goes stale", {
  store <- dims_fake_store()
  shiny::testServer(page_dims_server, args = list(
    store = store, goto = function(...) NULL, request = shiny::reactiveVal(NULL)
  ), {
    session$setInputs(wave = "M036小")
    expect_match(dims_html(output$settings_line), "家長題庫")
    session$setInputs(run = 1)
    st <- tab$status()
    expect_identical(st$state, "warning")
    expect_match(st$message, "M036小 構面報告：80 個構面")
    expect_match(dims_html(output$status), "完成，但請注意")
    expect_match(dims_html(output$status), "沒有信度")
    expect_match(dims_html(output$downloads), "下載 Word")
    expect_match(dims_html(output$downloads), "下載 HTML")
    expect_match(dims_html(output$result_details), "沒有信度的構面")

    f <- kitDesign::kit_test_download(session, "dl_docx")$content$file
    expect_true(file.exists(f))
    expect_gt(file.info(f)$size, 5000L)
    f <- kitDesign::kit_test_download(session, "dl_html")$content$file
    expect_match(paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = ""), "M036小")

    # 換波次 → 要重新執行
    session$setInputs(wave = "M048小")
    expect_identical(tab$status()$state, "stale")
    expect_null(output$downloads)
  })
})

test_that("a report with reliability for every dimension is plain 【完成】", {
  store <- dims_fake_store()
  b <- .bank("parent")
  # 只留一個構面、一個波次、有信度
  keep <- "注意力"
  b$itemDim <- b$itemDim[, c("itemID", "itemVer", keep)]
  b$dimMeta <- b$dimMeta[b$dimMeta$dimID == keep, ]
  b$dimMeta$reliability <- "0.85"
  store$bank(b)
  shiny::testServer(page_dims_server, args = list(
    store = store, goto = function(...) NULL, request = shiny::reactiveVal(NULL)
  ), {
    session$setInputs(wave = "M036小")
    session$setInputs(run = 1)
    expect_identical(tab$status()$state, "success")
    expect_match(tab$status()$message, "1 個構面、6 題")
  })
})

test_that("request() with a dimension selects it", {
  req <- shiny::reactiveVal(NULL)
  shiny::testServer(page_dims_server, args = list(
    store = dims_fake_store(), goto = function(...) NULL, request = req
  ), {
    session$flushReact()
    req(list(tab = "search", args = list(dim = "注意力"), at = Sys.time()))
    session$flushReact()
    expect_null(current_dim())
    req(list(tab = "dims", args = list(dim = "注意力"), at = Sys.time()))
    session$flushReact()
    expect_identical(current_dim(), "注意力")
    req(list(tab = "dims", args = list(dim = "沒有這個"), at = Sys.time()))
    session$flushReact()
    expect_identical(current_dim(), "注意力")
  })
})

test_that("用 R 怎麼做 uses the exported functions", {
  code <- shiny::isolate(dims_code(dims_fake_store(), "注意力", "M036小"))
  expect_match(code, "kitItemBank::load_bank(\"parent\")", fixed = TRUE)
  expect_match(code, "dim_items(bank, \"注意力\")", fixed = TRUE)
  expect_match(code, "dim_report_data(bank, \"M036小\", domain = \"parent\", dims = \"注意力\")", fixed = TRUE)
  expect_match(code, "dim_overlap(bank, \"注意力\")", fixed = TRUE)
  code2 <- shiny::isolate(dims_code(dims_fake_store(), NULL, "M036小"))
  expect_match(code2, "dim_report_data(bank, \"M036小\", domain = \"parent\")", fixed = TRUE)
  expect_true(all(vapply(parse(text = code), function(e) TRUE, logical(1))))
})
