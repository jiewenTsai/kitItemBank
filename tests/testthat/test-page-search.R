# page_search.R：題項查詢頁（testServer）

fake_store <- function(domain = "parent") {
  base <- shiny::reactiveVal(.bank(domain))
  bank <- shiny::reactiveVal(.bank(domain))
  list(
    domain = shiny::reactive(domain), bank = bank, base = base,
    source = shiny::reactive(list(kind = "builtin", label = "內建題庫")),
    changes = shiny::reactive(bank_diff(base(), bank())),
    set_bank = function(b) bank(b), revert = function() bank(base())
  )
}

# 瀏覽器一開始送來的條件
start_inputs <- function(session, ...) {
  session$setInputs(keyword = "", fields = "text", wave_match = "any", dim_match = "any",
                    include_deprecated = FALSE, ...)
  session$elapse(400)  # 關鍵字有 debounce
}

html_of <- function(x) if (is.list(x)) x$html else as.character(x)

test_that("the domain bank loads and conditions narrow the results", {
  b <- .bank("parent")
  shiny::testServer(page_search_server, args = list(
    store = fake_store(), goto = function(...) NULL, request = shiny::reactiveVal(NULL)
  ), {
    start_inputs(session)
    n_all <- nrow(results())
    expect_equal(n_all, sum(b$itemBank$deprecated != 1L))
    head0 <- html_of(output$result_head)
    expect_match(head0, "共 1,524 題（不含 528 題已停用）")
    expect_match(head0, "下載查詢結果")
    expect_no_match(head0, "btn-primary")  # 還沒有條件：下載不是主要按鈕
    expect_match(html_of(output$settings), "關鍵字 不限")

    session$setInputs(keyword = "睡眠")
    session$elapse(400)
    n_kw <- nrow(results())
    expect_gt(n_kw, 0L)
    expect_lt(n_kw, n_all)

    w <- get_wave_cols(b$itemWave)[1:2]
    session$setInputs(waves = w)
    expect_lte(nrow(results()), n_kw)
    expect_match(html_of(output$settings), "睡眠")
    expect_match(html_of(output$settings), w[1], fixed = TRUE)

    # 有條件了，下載才是主要按鈕
    expect_match(html_of(output$result_head), "btn-primary")
    # 下載：說明、題目（含構面欄）、跨波次對照，檔名帶題庫與日期
    dl <- kitDesign::kit_test_download(session, "download")
    expect_equal(dl$filename, paste0("題項查詢_家長_", format(Sys.Date()), ".xlsx"))
    expect_equal(readxl::excel_sheets(dl$content$file), c("說明", "題目", "跨波次對照"))
    x <- readxl::read_excel(dl$content$file, sheet = "題目")
    expect_equal(names(x), c(names(b$itemBank), "n_waves", "構面"))
    expect_equal(nrow(x), nrow(results()))
    info <- readxl::read_excel(dl$content$file, sheet = "說明")
    expect_true(any(grepl("睡眠", info[[2]])))
    expect_true(any(grepl("臺灣幼兒發展調查資料庫（KIT）題庫", info[[2]])))

    # 各波次題數：收著時頁面上沒有 plotly 圖（在收起來的區塊裡畫會在瀏覽器出錯，
    # 同一批的其他輸出就沒更新）；打開才放
    expect_null(output$heat_ui)
    session$setInputs(heat_open = TRUE)
    expect_match(html_of(output$heat_ui), "search-heat|proxy1-heat")
    session$setInputs(heat_open = FALSE)
    expect_null(output$heat_ui)

    # 用 R 怎麼做
    expect_match(html_of(output[["code-code"]]), "query_items")

    # 清除條件（testServer 不會真的改 input，只確定不出錯）
    session$setInputs(clear = 1)
    expect_equal(args()$keyword, "")
  })
})

test_that("zero results show why and what to try instead of an empty table", {
  shiny::testServer(page_search_server, args = list(
    store = fake_store(), goto = function(...) NULL, request = shiny::reactiveVal(NULL)
  ), {
    start_inputs(session)
    w <- get_wave_cols(.bank("parent")$itemWave)
    session$setInputs(waves = w[c(1, length(w))], wave_match = "all", keyword = "睡眠")
    session$elapse(400)
    expect_equal(nrow(results()), 0L)
    head <- html_of(output$result_head)
    expect_match(head, "kit-callout-warn")
    expect_match(head, "沒有符合的題目")
    expect_match(head, "任一波次")
    expect_no_match(head, "下載查詢結果")
    expect_error(output$table)  # req() 停下來：不畫空表
    expect_null(output$heat_ui)  # 「各波次題數」收著：不放 plotly 圖
    session$setInputs(heat_open = TRUE)
    expect_match(html_of(output$heat_ui), "沒有符合的題目")
    expect_match(html_of(output$xw_dl), "下載跨波次對照總表")
  })
})

test_that("clicking a row shows the item detail; dimension links go to 構面查詢", {
  went <- NULL
  b <- .bank("parent")
  # 一題有構面、也有其他版本
  dm <- as.matrix(b$itemDim[get_matrix_cols(b$itemDim)])
  multi <- names(which(table(b$itemBank$itemID) > 1))
  ok <- b$itemBank$itemID %in% multi & rowSums(dm) > 0 & b$itemBank$deprecated != 1L
  target <- b$itemBank[which(ok)[1], ]
  shiny::testServer(page_search_server, args = list(
    store = fake_store(), goto = function(...) went <<- list(...),
    request = shiny::reactiveVal(NULL)
  ), {
    start_inputs(session)
    session$setInputs(keyword = target$itemID)
    session$elapse(400)
    r <- results()
    i <- which(r$itemID == target$itemID & r$itemVer == target$itemVer)
    expect_length(i, 1L)
    expect_match(html_of(output$detail), "點表格的一列")
    session$setInputs(table_rows_selected = i)
    expect_equal(sel(), list(id = target$itemID, ver = target$itemVer))
    d <- html_of(output$detail)
    expect_match(d, paste0("這一題：", target$itemID), fixed = TRUE)
    expect_match(d, "出現的波次")
    expect_match(d, "其他版本")
    expect_match(d, "dim_go_1")
    session$setInputs(dim_go_1 = 1)
    expect_equal(went[[1]], "dims")
    expect_equal(went$dim, item_detail(b, target$itemID, target$itemVer)$dims$dimID[1])
    # 看其他版本
    session$setInputs(ver_go_1 = 1)
    expect_false(identical(sel()$ver, target$itemVer))
    # 條件改到不含這一題 → 細節收起來
    session$setInputs(keyword = "沒有這個字")
    session$elapse(400)
    expect_null(sel())
  })
})

test_that("request() with an item clears the conditions and opens that item", {
  rq <- shiny::reactiveVal(NULL)
  shiny::testServer(page_search_server, args = list(
    store = fake_store(), goto = function(...) NULL, request = rq
  ), {
    start_inputs(session)
    session$setInputs(keyword = "睡眠")
    session$elapse(400)
    rq(list(tab = "dims", args = list(dim = "x"), at = Sys.time()))  # 別頁的請求：不理
    session$flushReact()
    expect_null(sel())

    rq(list(tab = "search", args = list(item = "area1_1"), at = Sys.time()))
    session$flushReact()
    expect_equal(sel(), list(id = "area1_1", ver = 2L))  # 沒給版本：用沒停用的最新版
    # 回歸測試：結果馬上換成這個 itemID，不等瀏覽器送回關鍵字、也不等 debounce
    # （瀏覽器裡從構面查詢跳過來時，debounce 的計時器沒有再觸發，結果停在全部題目）
    expect_equal(args()$keyword, "area1_1")
    expect_true(all(startsWith(results()$itemID, "area1_1")))
    expect_match(html_of(output$result_head), "共 1 題")
    expect_match(html_of(output$settings), "關鍵字「area1_1」")
    # 結果還沒換成新的關鍵字時，細節不收起來
    expect_match(html_of(output$detail), "這一題：area1_1")
    # 瀏覽器把關鍵字換成 itemID 之後
    session$setInputs(keyword = "area1_1")
    session$elapse(400)
    expect_equal(sel(), list(id = "area1_1", ver = 2L))
    d <- html_of(output$detail)
    expect_match(d, "<del>", fixed = TRUE)  # 第 1 版的文字差異

    rq(list(tab = "search", args = list(item = "area1_1", ver = 1L), at = Sys.time()))
    session$flushReact()
    expect_equal(sel()$ver, 1L)
  })
})

test_that("the search page works for every domain", {
  for (d in unname(BANK_DOMAINS)) {
    shiny::testServer(page_search_server, args = list(
      store = fake_store(d), goto = function(...) NULL, request = shiny::reactiveVal(NULL)
    ), {
      start_inputs(session)
      expect_gt(nrow(results()), 0L)
      expect_match(html_of(output$result_head), "共 ")
    })
  }
})

test_that("the search page UI renders and bookmarks leave out DT inputs", {
  ui <- htmltools::renderTags(page_search_ui("search"))$html
  expect_match(ui, "search-keyword")
  expect_match(ui, "每個波次都有")
  expect_match(ui, "任一個詞")
  expect_match(ui, "search-keyword_match")
  expect_match(ui, "M03 世代")  # 波次代碼的說明
  expect_true(all(c("edit_go", "back_go") %in% SEARCH_BOOKMARK_EXCLUDE))
  expect_match(ui, "包含已停用的題目")
  expect_true("table_rows_selected" %in% SEARCH_BOOKMARK_EXCLUDE)
})

test_that("a jump from another tab keeps the old query and offers to go back", {
  rq <- shiny::reactiveVal(NULL)
  went <- NULL
  shiny::testServer(page_search_server, args = list(
    store = fake_store(), goto = function(...) went <<- list(...), request = rq
  ), {
    start_inputs(session)
    session$setInputs(keyword = "睡眠 睡覺", keyword_match = "any")
    session$elapse(400)
    n_before <- nrow(results())
    expect_match(html_of(output$settings), "任一個詞")
    expect_no_match(html_of(output$result_head), "回到剛才的查詢")

    rq(list(tab = "search", args = list(item = "area1_1"), at = Sys.time()))
    session$flushReact()
    expect_equal(args()$keyword, "area1_1")
    expect_match(html_of(output$result_head), "回到剛才的查詢")
    expect_match(html_of(output$result_head), "睡眠 睡覺")
    # 第二次跳過來：還是回到最早的查詢
    rq(list(tab = "search", args = list(item = "area1_2"), at = Sys.time()))
    session$flushReact()
    expect_match(html_of(output$result_head), "睡眠 睡覺")

    # 這一題：版本 × 波次、差異、同題幹、改這一題
    d <- html_of(output$detail)
    expect_match(html_of(output$timeline), "各版本出現的波次")
    expect_match(d, "看其他版本")
    expect_match(d, "各版本差在哪")
    expect_match(d, "同一個題幹的其他題")
    expect_match(d, "改這一題")
    expect_match(d, "36 個月|96 個月|個月（")
    session$setInputs(edit_go = 1)
    expect_equal(went, list("edit", item = "area1_2", ver = 2L))
    session$setInputs(stem_go_1 = 1)
    expect_false(identical(sel()$id, "area1_2"))

    session$setInputs(back_go = 1)
    expect_equal(args()$keyword, "睡眠 睡覺")
    expect_no_match(html_of(output$result_head), "回到剛才的查詢")
  })
})

test_that("goto('search', keyword = ) sets the keyword and runs", {
  rq <- shiny::reactiveVal(NULL)
  shiny::testServer(page_search_server, args = list(
    store = fake_store(), goto = function(...) NULL, request = rq
  ), {
    start_inputs(session)
    rq(list(tab = "search", args = list(keyword = "親子"), at = Sys.time()))
    session$flushReact()
    expect_equal(args()$keyword, "親子")
    expect_gt(nrow(results()), 0L)
    expect_null(sel())
    expect_no_match(html_of(output$result_head), "回到剛才的查詢")  # 之前沒有條件
  })
})

test_that("zero results list dimensions whose name has the word, as links", {
  went <- NULL
  shiny::testServer(page_search_server, args = list(
    store = fake_store(), goto = function(...) went <<- list(...),
    request = shiny::reactiveVal(NULL)
  ), {
    start_inputs(session)
    session$setInputs(keyword = "親子 互動")
    session$elapse(400)
    expect_equal(nrow(results()), 0L)
    head <- html_of(output$result_head)
    expect_match(head, "任一個詞")
    expect_match(head, "名稱含這個詞的構面")
    expect_match(head, "zdim_go_1")
    session$setInputs(zdim_go_1 = 1)
    expect_equal(went[[1]], "dims")
    expect_match(went$dim, "親子|互動")
  })
})

test_that("the heatmap can be grouped by dimension", {
  shiny::testServer(page_search_server, args = list(
    store = fake_store(), goto = function(...) NULL, request = shiny::reactiveVal(NULL)
  ), {
    start_inputs(session)
    session$setInputs(keyword = "睡")
    session$elapse(400)
    session$setInputs(heat_by = "dim", heat_open = TRUE)
    m <- counts()
    expect_equal(m, wave_counts(.bank("parent"), results(), by = "dim"))
    expect_match(html_of(output$heat_note), "構面")
    p <- wave_heatmap(m)
    expect_s3_class(p, "plotly")
  })
})
