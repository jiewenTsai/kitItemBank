# page_review.R：審核頁（testServer）。GitHub 一律用假的 github_api()。

review_html <- function(x) if (is.list(x)) x$html else as.character(x)

review_page_file <- function(meta = list(), base_version = "2026.1", env = parent.frame()) {
  kit <- kit_builtin()
  old <- bank_to_v02(kit, "parent")
  new <- old
  new$itemBank$label[new$itemBank$itemID == "fatherinvo01n"] <- "y"
  res <- kit_patch(kit, old, new, "parent")
  f <- withr::local_tempfile(fileext = ".xlsx", .local_envir = env)
  writexl::write_xlsx(patch_workbook(res, bank_diff(old, new), "parent", base_version,
                                     meta = meta), f)
  data.frame(name = "修改包_test.xlsx", size = file.size(f), type = "", datapath = f,
             stringsAsFactors = FALSE)
}

review_cfg <- function(ok = TRUE) {
  list(token = "x", repo = "someone/repo", branch = "main",
       password_hash = kit_password_hash("pw", salt = "00"), ok = ok,
       problems = if (ok) character(0) else c("沒有設定 GitHub 的寫入權杖（環境變數 KIT_GITHUB_TOKEN）"))
}

test_that("without GitHub settings the page reviews and offers the zip", {
  went <- NULL
  shiny::testServer(page_review_server, args = list(
    kit = kit_builtin(), version = "2026.1", config = review_cfg(FALSE),
    goto = function(tab, ...) went <<- c(list(tab = tab), list(...))
  ), {
    expect_match(review_html(output$result), "先在左邊選一個修改包")
    session$setInputs(file = review_page_file(list(reason = "會議決議", submitted_by = "小明")))
    html <- review_html(output$result)
    expect_match(html, "修改包_test.xlsx")
    expect_match(html, "共 1 列改動")
    expect_match(html, "從現在的題庫（2026.1 版）做的")
    expect_match(html, "檢查通過")
    expect_match(html, "影響到：題目 1 個")
    expect_match(html, "改前改後（1 列）")
    expect_length(review()$blocking, 0L)

    ap <- review_html(output$approve_ui)
    expect_match(ap, "還不能直接核准寫進 GitHub")
    expect_match(ap, "KIT_GITHUB_TOKEN")
    expect_match(ap, "下載套用後的題庫（zip）")
    expect_false(grepl("管理密碼", ap))

    dl <- kitDesign::kit_test_download(session, "dl_zip")
    expect_match(dl$filename, "^kit_2026\\.2_[0-9]{8}\\.zip$")
    expect_true("kit/changes/2026.2.csv" %in% zip::zip_list(dl$content$file)$filename)

    # 點一題 → 題項查詢
    session$setInputs(items_dt_rows_selected = 1L)
    expect_identical(went, list(tab = "search", item = "fatherinvo01n", uid = "P.fatherinvo01n"))
  })
})

test_that("an old base version warns; a broken file shows an error", {
  shiny::testServer(page_review_server, args = list(
    kit = kit_builtin(), version = "2026.3", config = review_cfg(FALSE), goto = function(...) NULL
  ), {
    session$setInputs(file = review_page_file())
    expect_match(review_html(output$result), "從 2026.1 版題庫做的，現在的題庫是 2026.3 版")

    bad <- withr::local_tempfile(fileext = ".xlsx")
    writeLines("not excel", bad)
    session$setInputs(file = data.frame(name = "壞的.xlsx", size = 1, type = "", datapath = bad,
                                        stringsAsFactors = FALSE))
    expect_match(review_html(output$result), "讀修改包「壞的.xlsx」")
    expect_null(review_html(output$approve_ui) %||% NULL)
  })
})

test_that("approval needs reason, approver and the right password, then writes once", {
  n_commits <- 0L
  local_mocked_bindings(github_api = function(cfg, method, path, body = NULL, query = NULL) {
    if (grepl("/contents/", path)) {
      return(list(content = openssl::base64_encode(charToRaw("2026.1\n"))))
    }
    if (method == "POST" && grepl("git/commits$", path)) {
      n_commits <<- n_commits + 1L
      return(list(sha = "C1", html_url = "https://github.com/someone/repo/commit/C1"))
    }
    list(sha = "S", object = list(sha = "H"), tree = list(sha = "T"))
  })
  shiny::testServer(page_review_server, args = list(
    kit = kit_builtin(), version = "2026.1", config = review_cfg(TRUE), goto = function(...) NULL
  ), {
    session$setInputs(file = review_page_file())
    ap <- review_html(output$approve_ui)
    expect_match(ap, "管理密碼")
    expect_match(ap, "成為 2026.2 版")
    expect_match(ap, "從 2026.1 版變成 2026.2 版")

    session$setInputs(password = "pw", approve = 1)
    expect_null(approved())          # 還沒填原因和審核人
    session$setInputs(reason = "改題目名稱", approved_by = "乙", password = "wrong", approve = 2)
    expect_null(approved())
    expect_identical(attempts(), 1L)
    session$setInputs(password = "pw", approve = 3)
    expect_identical(approved()$version, "2026.2")
    expect_identical(n_commits, 1L)
    expect_match(review_html(output$approve_ui), "已核准，寫進 GitHub：題庫現在是 2026.2 版")
    session$setInputs(password = "pw", approve = 4)
    expect_identical(n_commits, 1L)  # 不會寫第二次
  })
})

test_that("too many wrong passwords lock approval for the session", {
  local_mocked_bindings(github_api = function(...) stop("should not be called"))
  shiny::testServer(page_review_server, args = list(
    kit = kit_builtin(), version = "2026.1", config = review_cfg(TRUE), goto = function(...) NULL
  ), {
    session$setInputs(file = review_page_file(), reason = "r", approved_by = "a")
    for (i in seq_len(REVIEW_MAX_ATTEMPTS)) session$setInputs(password = "no", approve = i)
    expect_identical(attempts(), REVIEW_MAX_ATTEMPTS)
    session$setInputs(password = "pw", approve = 99)
    expect_null(approved())
  })
})

test_that("a package that breaks the bank cannot be approved; tables render", {
  f <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(
    "說明" = data.frame("項目" = "base_version", "內容" = "2026.1", check.names = FALSE),
    "改動" = data.frame(op = c("add", "delete"), table = "groups", key = c("P.新題組", "P.沒有"),
                      column = "", from = "", to = "")
  ), f)
  shiny::testServer(page_review_server, args = list(
    kit = kit_builtin(), version = "2026.1", config = review_cfg(TRUE), goto = function(...) NULL
  ), {
    session$setInputs(file = data.frame(name = "壞.xlsx", size = 1, type = "", datapath = f,
                                        stringsAsFactors = FALSE))
    html <- review_html(output$result)
    expect_match(html, "套不上的改動")
    expect_match(html, "新的結構問題，不能核准")
    ap <- review_html(output$approve_ui)
    expect_match(ap, "1 列改動套不上")
    expect_false(grepl("管理密碼", ap))
    expect_match(output$issues_dt, "什麼時候有的")
    expect_match(output$changes_dt, "改後")
    expect_true("必填的欄是空白" %in% review_issue_table(review()$issues)$問題)
  })
})

test_that("page_review_ui builds", {
  ui <- as.character(page_review_ui("review"))
  expect_match(ui, "review-file")
  expect_match(ui, "改動紀錄的格式")
})
