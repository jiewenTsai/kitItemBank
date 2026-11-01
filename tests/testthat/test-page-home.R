# page_home.R：我的工作（testServer）

home_kit <- function() kit_builtin()
home_html <- function(x) if (is.list(x)) x$html else as.character(x)
home_test <- function(args, expr) {
  q <- rlang::enquo(expr)
  suppressWarnings(rlang::inject(shiny::testServer(page_home_server, args = args, !!q)))
}

test_that("the UI has the four sections and no fixed widths", {
  html <- as.character(page_home_ui("home"))
  for (s in c("等你審的修改包", "要注意", "請教單", "交辦事項")) expect_match(html, s)
  expect_false(grepl("width: *[0-9]{3,}px", html))
})

test_that("audit findings are grouped by owner and link to the right page", {
  went <- list()
  home_test(list(kit = home_kit(), goto = function(tab, ...) went[[length(went) + 1L]] <<- list(tab = tab, ...)), {
    session$flushReact()
    expect_match(home_html(output$pending), "還沒接上")
    head <- home_html(output$audit_head)
    expect_match(head, "資料檔和 .sav 表頭對不上")
    expect_match(head, "下載要注意的完整清單")
    self <- home_html(output$audit_self)
    expect_match(self, "自己能改（")
    expect_match(self, "題目的角色或模組空白")
    expect_match(self, "還有 [0-9]+ 筆")
    expect_match(home_html(output$audit_teacher), "換題號後的新題沒有接上題組")
    expect_match(home_html(output$audit_stats), "同一題本、同一年度")

    a <- ha()
    i <- which(a$check == "renamed_no_group")[1]
    session$setInputs(go = i)
    i <- which(a$check == "form_year_diff")[1]
    session$setInputs(go = i)
    i <- which(a$tab == "dims")[1]
    session$setInputs(go = i)
    session$setInputs(go = 999999L)  # 不存在：什麼都不做

    res <- kitDesign::kit_test_download(session, "dl_audit")
    expect_identical(readxl::excel_sheets(res$content$file), unname(HOME_OWNERS))
  })
  expect_length(went, 3L)
  expect_identical(went[[1]]$tab, "search")
  expect_identical(went[[1]]$uid, "P.caregiverf02n")
  expect_identical(went[[2]][c("tab", "admin", "dataset")],
                   list(tab = "admin", admin = "M03M003", dataset = "M03M003-P"))
  expect_identical(went[[3]]$tab, "dims")
})

test_that("pending reviews come from the hook", {
  home_test(list(kit = home_kit(), goto = function(...) NULL,
                 pending = shiny::reactive(data.frame(label = c("修改包 A", "修改包 B")))), {
    expect_match(home_html(output$pending), "2 份修改包等你審")
    expect_match(home_html(output$pending), "修改包 B")
  })
  home_test(list(kit = home_kit(), goto = function(...) NULL, pending = function() 0L), {
    expect_match(home_html(output$pending), "沒有等你審")
  })
})

test_that("consultation form downloads, and an upload becomes a change package", {
  skip_if_not_installed("openxlsx")
  home_test(list(kit = shiny::reactive(home_kit()), goto = function(...) NULL), {
    expect_match(home_html(output$consult_head), "要請老師判斷的有 [0-9,]+ 個問題")
    res <- kitDesign::kit_test_download(session, "dl_consult")
    expect_match(res$filename, "^請教單_[0-9]{8}\\.xlsx$")
    f <- res$content$file
    wb <- openxlsx::loadWorkbook(f)
    d <- openxlsx::read.xlsx(wb, "請教單", check.names = FALSE)
    i <- which(startsWith(d[["系統用"]], "relation|"))[1]
    openxlsx::writeData(wb, "請教單", t(c("不能比較", "選項改了", "")), startRow = i + 1L,
                        startCol = match("決定", names(d)), colNames = FALSE)
    j <- which(startsWith(d[["系統用"]], "block|"))[1]
    openxlsx::writeData(wb, "請教單", t(c("再討論", "", "")), startRow = j + 1L,
                        startCol = match("決定", names(d)), colNames = FALSE)
    openxlsx::saveWorkbook(wb, f, overwrite = TRUE)

    session$setInputs(consult_who = "王老師", consult_file = list(datapath = f, name = "x.xlsx"))
    out <- home_html(output$consult_result)
    expect_match(out, "讀到 2 個回覆")
    expect_match(out, "下載修改包")
    expect_match(out, "不用改題庫的回覆（1）")
    p <- kitDesign::kit_test_download(session, "dl_consult_patch")
    back <- read_patch_xlsx(p$content$file)$patch
    expect_true(any(back$column == "decided_by" & back$to == "王老師"))

    bad <- withr::local_tempfile(fileext = ".xlsx")
    writexl::write_xlsx(data.frame(a = 1), bad)
    session$setInputs(consult_file = list(datapath = bad, name = "bad.xlsx"))
    expect_match(home_html(output$consult_result), "讀不了這個檔案")
  })
})

test_that("to-dos live in the session and round-trip through CSV", {
  home_test(list(kit = home_kit(), goto = function(...) NULL), {
    expect_match(home_html(output$todo_note), "重新整理或關掉就不見了")
    expect_match(home_html(output$todo_list), "沒有還沒做的交辦事項")
    session$setInputs(todo_task = "問陳老師題組", todo_who = "小美", todo_due = "11/30", todo_add = 1)
    session$setInputs(todo_task = "核對教保問卷", todo_who = "", todo_due = "", todo_add = 2)
    expect_identical(nrow(todo()), 2L)
    expect_match(home_html(output$todo_list), "問陳老師題組（誰做：小美；期限：11/30）")
    session$setInputs(todo_toggle = "1")
    expect_identical(todo()$done, c("1", ""))
    expect_match(home_html(output$todo_list), "已完成（1）")
    res <- kitDesign::kit_test_download(session, "dl_todo")
    session$setInputs(todo_toggle = "1")
    expect_identical(todo()$done, c("", ""))
    session$setInputs(todo_file = list(datapath = res$content$file, name = "t.csv"))
    expect_identical(todo()$done, c("1", ""))
  })
})

test_that("a todo_store loads and saves to-dos", {
  saved <- NULL
  store <- list(load = function() todo_add(empty_todo(), "舊的一件"),
                save = function(x) saved <<- x)
  home_test(list(kit = home_kit(), goto = function(...) NULL, todo_store = store), {
    expect_identical(todo()$task, "舊的一件")
    expect_match(home_html(output$todo_note), "會存起來")
    session$setInputs(todo_task = "新的一件", todo_add = 1)
  })
  expect_identical(saved$task, c("舊的一件", "新的一件"))
})
