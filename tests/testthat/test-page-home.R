# page_home.R：我的工作（testServer）
# 不寫死題庫內容：數量、標題、連結都從 home_audit() 算出來比對

home_html <- function(x) if (is.list(x)) x$html else as.character(x)
home_test <- function(args, expr) {
  q <- rlang::enquo(expr)
  suppressWarnings(rlang::inject(shiny::testServer(page_home_server, args = args, !!q)))
}

test_that("the UI has the audit section and no fixed widths", {
  html <- as.character(page_home_ui("home"))
  expect_match(html, "要注意")
  expect_false(grepl("width: *[0-9]{3,}px", html))
  expect_false(grepl("請教單|交辦事項|等你審", html))
})

test_that("audit findings are grouped by owner and link to the right page", {
  kit <- kit_builtin()
  went <- list()
  home_test(list(kit = kit, goto = function(tab, ...) went[[length(went) + 1L]] <<- list(tab = tab, ...)), {
    session$flushReact()
    a <- ha()
    head <- home_html(output$audit_head)
    expect_match(head, paste0("共有 ", kitDesign::kit_num(nrow(a)), " 筆"))
    expect_match(head, "資料檔和 .sav 表頭對不上")
    expect_match(head, "下載要注意的完整清單")
    outs <- list(self = output$audit_self, teacher = output$audit_teacher, stats = output$audit_stats)
    for (o in names(outs)) {
      html <- home_html(outs[[o]])
      n <- sum(a$owner == o)
      expect_match(html, paste0(HOME_OWNERS[[o]], "（", kitDesign::kit_num(n), "）"), fixed = TRUE)
      for (l in unique(a$check_label[a$owner == o])) expect_match(html, l, fixed = TRUE)
      if (any(table(a$check[a$owner == o]) > HOME_SHOW)) expect_match(html, "還有 [0-9,]+ 筆")
    }

    picks <- vapply(c("search", "dims", "admin"), function(t) which(a$tab == t)[1], 1L)
    picks <- picks[!is.na(picks)]
    for (i in picks) session$setInputs(go = i)
    session$setInputs(go = nrow(a) + 1L)  # 不存在：什麼都不做
    blank <- which(a$tab == "")[1]
    if (!is.na(blank)) session$setInputs(go = blank)  # 沒有連結：什麼都不做

    expect_length(went, length(picks))
    for (j in seq_along(picks)) {
      r <- a[picks[j], ]
      g <- went[[j]]
      expect_identical(g$tab, r$tab)
      if (r$tab == "search") expect_identical(c(g$item, g$uid), c(r$target, r$extra))
      if (r$tab == "dims") expect_identical(g$dim, r$target)
      if (r$tab == "admin") expect_identical(c(g$admin, g$dataset), c(r$target, r$extra))
    }

    res <- kitDesign::kit_test_download(session, "dl_audit")
    expect_identical(readxl::excel_sheets(res$content$file), unname(HOME_OWNERS))
  })
})

test_that("an empty kit shows nothing to do", {
  home_test(list(kit = shiny::reactive(empty_kit()), goto = function(...) NULL), {
    expect_match(home_html(output$audit_self), "沒有。")
    expect_match(home_html(output$audit_head), "共有 0 筆")
  })
})
