# tests/testthat/test-page-edit.R：題庫編修頁（testServer）

.fake_store <- function(domain = "parent") {
  base <- shiny::reactiveVal(.bank(domain))
  bank <- shiny::reactiveVal(.bank(domain))
  list(
    domain = shiny::reactive(domain),
    bank = bank, base = base,
    source = shiny::reactive(list(kind = "builtin", label = "內建題庫")),
    changes = shiny::reactive(bank_diff(base(), bank())),
    set_bank = function(b) bank(b),
    revert = function() bank(shiny::isolate(base()))
  )
}

.edit_args <- function(store = .fake_store(), request = shiny::reactiveVal(NULL)) {
  list(store = store, goto = function(...) NULL, request = request)
}

test_that("點一列就勾選／取消，store 跟著變", {
  shiny::testServer(page_edit_server, args = .edit_args(), {
    b <- store$bank()
    dim <- get_matrix_cols(b$itemDim)[1]
    session$setInputs(what = "itemDim", `dim-column` = dim, `dim-keyword` = "",
                      `dim-only` = FALSE)
    assigned <- which(b$itemDim[[dim]] == 1L)
    target <- which(b$itemDim[[dim]] == 0L)[1]
    # 表一開始的選取 = 已勾的題
    session$setInputs(`dim-tbl_rows_selected` = assigned)
    expect_equal(nrow(store$changes()), 0L)
    # 點一列 → 勾選
    session$setInputs(`dim-tbl_rows_selected` = sort(c(assigned, target)))
    expect_identical(store$bank()$itemDim[[dim]][target], 1L)
    expect_equal(nrow(store$changes()), 1L)
    # 再點一下 → 取消，回到沒有改動
    session$setInputs(`dim-tbl_rows_selected` = assigned)
    expect_identical(store$bank()$itemDim[[dim]][target], 0L)
    expect_equal(nrow(store$changes()), 0L)
    # 差很多列（表和 store 沒對上）→ 不寫
    session$setInputs(`dim-tbl_rows_selected` = integer(0))
    expect_equal(nrow(store$changes()), 0L)
    # 勾選目前篩出的全部（用關鍵字篩出幾題）
    kw <- b$itemDim$itemID[target]
    session$setInputs(`dim-keyword` = kw)
    session$setInputs(`dim-check_all` = 1)
    hit <- b$itemDim$itemID == kw
    expect_true(all(store$bank()$itemDim[[dim]][hit] == 1L))
    session$setInputs(`dim-uncheck_all` = 1)
    expect_true(all(store$bank()$itemDim[[dim]][hit] == 0L))
    # 本次改動一行字
    session$setInputs(`dim-keyword` = "", `dim-tbl_rows_selected` = assigned)
    session$setInputs(`dim-tbl_rows_selected` = sort(c(assigned, target)))
    expect_match(htmltools::renderTags(output$summary)$html, "本次改動：", fixed = TRUE)
  })
})

test_that("波次也一樣", {
  shiny::testServer(page_edit_server, args = .edit_args(), {
    b <- store$bank()
    w <- get_wave_cols(b$itemWave)[1]
    session$setInputs(what = "itemWave", `wave-column` = w, `wave-keyword` = "",
                      `wave-only` = TRUE)
    assigned <- which(b$itemWave[[w]] == 1L)
    session$setInputs(`wave-tbl_rows_selected` = seq_along(assigned))
    # 只看已勾的：取消第一題
    session$setInputs(`wave-tbl_rows_selected` = seq_along(assigned)[-1])
    expect_identical(store$bank()$itemWave[[w]][assigned[1]], 0L)
    expect_identical(store$changes()[["表"]], "itemWave")
  })
})

test_that("新增、刪除構面（先問一次）", {
  shiny::testServer(page_edit_server, args = .edit_args(), {
    session$setInputs(what = "itemDim", `dim-keyword` = "", `dim-only` = FALSE)
    session$setInputs(`dim-new_name` = "測試構面")
    session$setInputs(`dim-add` = 1)
    expect_true("測試構面" %in% names(store$bank()$itemDim))
    expect_true("測試構面" %in% store$bank()$dimMeta$dimID)
    # 同名再加一次 → 不變
    n <- ncol(store$bank()$itemDim)
    session$setInputs(`dim-new_name` = "測試構面")
    session$setInputs(`dim-add` = 2)
    expect_equal(ncol(store$bank()$itemDim), n)

    dim <- get_matrix_cols(store$base()$itemDim)[1]
    session$setInputs(`dim-column` = dim)
    session$setInputs(`dim-delete` = 1)
    html <- htmltools::renderTags(output$`dim-delete_ui`)$html
    expect_match(html, paste0("刪除「", dim, "」："), fixed = TRUE)
    expect_match(html, "題會失去這個構面", fixed = TRUE)
    expect_true(dim %in% names(store$bank()$itemDim))  # 還沒刪
    session$setInputs(`dim-delete_cancel` = 1)
    expect_true(dim %in% names(store$bank()$itemDim))
    session$setInputs(`dim-delete` = 2)
    session$setInputs(`dim-delete_ok` = 1)
    expect_false(dim %in% names(store$bank()$itemDim))
    expect_false(dim %in% store$bank()$dimMeta$dimID)
    s <- edit_changes_summary(store$changes())
    expect_match(s, paste0("刪除構面「", dim, "」"), fixed = TRUE)
    expect_match(s, "新增構面「測試構面」", fixed = TRUE)
  })
})

test_that("構面資訊按「套用」才改；波次信度設定與拿掉", {
  shiny::testServer(page_edit_server, args = .edit_args(), {
    dim <- store$bank()$dimMeta$dimID[1]
    w <- get_wave_cols(store$bank()$itemWave)[1]
    session$setInputs(what = "meta", `meta-dim` = dim, `meta-dimName` = "新的名稱",
                      `meta-alias` = "別名", `meta-note` = "", `meta-reliability` = "0.9")
    expect_equal(nrow(store$changes()), 0L)  # 還沒套用
    session$setInputs(`meta-apply` = 1)
    r <- store$bank()$dimMeta[store$bank()$dimMeta$dimID == dim, ]
    expect_identical(r$dimName, "新的名稱")
    expect_identical(r$alias, "別名")
    expect_identical(r$reliability, "0.9")
    # 信度打錯 → 不改
    session$setInputs(`meta-reliability` = "很高")
    session$setInputs(`meta-apply` = 2)
    expect_identical(store$bank()$dimMeta$reliability[store$bank()$dimMeta$dimID == dim], "0.9")

    session$setInputs(`meta-wave` = w, `meta-wave_value` = "0.66")
    session$setInputs(`meta-set_wave` = 1)
    dr <- store$bank()$dimReliability
    expect_equal(dr$reliability[dr$dimID == dim & dr$wave == w], 0.66)
    expect_match(htmltools::renderTags(output$`meta-wave_table`)$html, "0.66", fixed = TRUE)
    session$setInputs(`meta-wave_value` = "")
    session$setInputs(`meta-set_wave` = 2)
    dr <- store$bank()$dimReliability
    expect_false(any(dr$dimID == dim & dr$wave == w))
    expect_match(edit_changes_summary(store$changes()), "構面資訊 3 處", fixed = TRUE)
  })
})

test_that("下載的修改包有改動和原始改動；沒有改動時主要按鈕停用；復原清掉改動", {
  shiny::testServer(page_edit_server, args = .edit_args(), {
    session$setInputs(what = "itemDim")
    html <- htmltools::renderTags(output$download_ui)$html
    expect_match(html, "還差：還沒有改動", fixed = TRUE)
    expect_match(html, "disabled", fixed = TRUE)

    b <- store$bank()
    dim <- get_matrix_cols(b$itemDim)[1]
    i <- which(b$itemDim[[dim]] == 0L)[1:2]
    store$set_bank(set_assignment(b, "itemDim", dim, b$itemDim[i, c("itemID", "itemVer")], 1L))
    session$flushReact()
    html <- htmltools::renderTags(output$download_ui)$html
    expect_match(html, "kit-download", fixed = TRUE)
    expect_match(html, "btn-primary", fixed = TRUE)

    dl <- kitDesign::kit_test_download(session, "download")
    expect_equal(dl$status, 200)
    expect_identical(dl$filename, edit_download_filename("parent"))
    f <- dl$content$file
    on.exit(unlink(f), add = TRUE)
    expect_true(all(c("說明", "改動", "原始改動") %in% readxl::excel_sheets(f)))
    log <- readxl::read_excel(f, sheet = "原始改動")
    expect_equal(nrow(log), 2L)
    expect_true(all(log[["欄"]] == dim))
    p <- read_patch_xlsx(f)$patch
    expect_true(nrow(p) > 0L)
    expect_true(all(p$table == "group_members" & p$op == "add"))

    session$setInputs(revert = 1)
    expect_match(htmltools::renderTags(output$revert_ui)$html, "復原，丟掉這些改動", fixed = TRUE)
    expect_equal(nrow(store$changes()), 2L)
    session$setInputs(revert_ok = 1)
    expect_equal(nrow(store$changes()), 0L)
  })
})

test_that("goto(\"edit\", dim = ) 切到題目的構面", {
  req <- shiny::reactiveVal(NULL)
  shiny::testServer(page_edit_server, args = .edit_args(request = req), {
    session$setInputs(what = "meta")
    req(list(tab = "edit", args = list(dim = "睡眠習慣"), at = Sys.time()))
    expect_no_error(session$flushReact())
    req(list(tab = "dims", args = list(dim = "睡眠習慣"), at = Sys.time()))
    expect_no_error(session$flushReact())
  })
})

test_that("題目內容：goto(\"edit\", item =, ver =) 帶入那一題，套用後記成 itemBank 改動", {
  req <- shiny::reactiveVal(NULL)
  shiny::testServer(page_edit_server, args = .edit_args(request = req), {
    session$setInputs(what = "itemDim")
    req(list(tab = "edit", args = list(item = "famC21", ver = 1L), at = Sys.time()))
    session$flushReact()
    html <- htmltools::renderTags(output$`item-item_head`)$html
    expect_match(html, "famC21 第 1 版", fixed = TRUE)
    expect_match(html, "和選項 5 同一個代碼", fixed = TRUE)

    # 表單的值是目前的內容（testServer 不會把 update* 寫回 input，這裡自己設）
    v <- edit_item_values(store$bank(), "famC21", 1L)
    session$setInputs(`item-label` = v$label, `item-value` = v$value,
                      `item-noResponse` = v$noResponse, `item-note` = v$note,
                      `item-itemType` = v$itemType, `item-respondent` = v$respondent,
                      `item-block` = v$block, `item-stemID` = v$stemID,
                      `item-reverseScored` = "0", `item-deprecated` = "0")
    session$setInputs(`item-apply` = 1)
    expect_equal(nrow(store$changes()), 0L)  # 沒改東西

    session$setInputs(`item-noResponse` = "不適用:9995; 拒答:7777; 遺漏值:9999",
                      `item-reverseScored` = "1")
    session$setInputs(`item-apply` = 2)
    ch <- store$changes()
    expect_true(all(ch[["表"]] == "itemBank"))
    expect_setequal(ch[["欄"]], c("noResponse", "reverseScored"))
    expect_match(htmltools::renderTags(output$summary)$html, "題目內容 1 題 2 處", fixed = TRUE)
    # 「要注意」不再列 famC21 的遺漏值問題
    html <- htmltools::renderTags(output$`item-item_head`)$html
    expect_false(grepl("同一個代碼", html, fixed = TRUE))

    # 打錯不改
    session$setInputs(`item-deprecated` = "2")
    session$setInputs(`item-apply` = 3)
    expect_equal(nrow(store$changes()), 2L)

    # ver 沒給 → 帶入啟用的版本
    req(list(tab = "edit", args = list(item = "caregiverf01"), at = Sys.time()))
    session$flushReact()
    expect_match(htmltools::renderTags(output$`item-item_head`)$html, "caregiverf01 第 4 版",
                 fixed = TRUE)
    # 換版本
    session$setInputs(`item-ver` = "2")
    expect_match(htmltools::renderTags(output$`item-item_head`)$html, "caregiverf01 第 2 版",
                 fixed = TRUE)
    # 直接選一題
    session$setInputs(`item-item` = "med0303")
    expect_match(htmltools::renderTags(output$`item-item_head`)$html, "med0303 第 1 版",
                 fixed = TRUE)
  })
})

test_that("題目內容：「要注意」表篩選、點一列帶入那一題", {
  shiny::testServer(page_edit_server, args = .edit_args(), {
    session$setInputs(what = "item", `item-check` = "simplified_char")
    expect_match(htmltools::renderTags(output$`item-audit_headline`)$html, "要注意：", fixed = TRUE)
    expect_no_error(output$`item-audit_tbl`)
    session$setInputs(`item-audit_tbl_rows_selected` = 1L)
    expect_match(htmltools::renderTags(output$`item-item_head`)$html, "med0303 第 1 版",
                 fixed = TRUE)
    expect_match(htmltools::renderTags(output$`item-item_head`)$html, "髋", fixed = TRUE)
    # 構面的問題（沒有 itemID）→ 不會壞
    session$setInputs(`item-check` = "dim_only_deprecated")
    session$setInputs(`item-audit_tbl_rows_selected` = 1L)
    expect_match(htmltools::renderTags(output$`item-item_head`)$html, "med0303", fixed = TRUE)
  })
})
