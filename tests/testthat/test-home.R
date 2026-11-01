# home.R：首頁的要注意清單（分給誰、連到哪）、交辦事項

test_that("every audit check has exactly one owner from HOME_OWNERS", {
  expect_setequal(HOME_AUDIT_OWNER$check, AUDIT_KIT_CHECKS$check)
  expect_false(any(duplicated(HOME_AUDIT_OWNER$check)))
  expect_true(all(HOME_AUDIT_OWNER$owner %in% names(HOME_OWNERS)))
  expect_identical(unname(HOME_OWNERS), c("自己能改", "要問老師", "要問統計人員"))
  own <- function(x) HOME_AUDIT_OWNER$owner[match(x, HOME_AUDIT_OWNER$check)]
  expect_true(all(own(c("text_simplified", "text_brackets", "text_space", "text_newline",
                        "role_module_blank", "group_empty")) == "self"))
  expect_true(all(own(c("renamed_no_group", "group_single_item", "group_only_deprecated",
                        "group_nonleaf_member")) == "teacher"))
  expect_true(all(own(c("coverage_uid_blank", "coverage_sav", "form_year_diff",
                        "missing_code_scheme", "internal_admins")) == "stats"))
  # 首頁不跑的檢查都要說得出要什麼資料
  expect_true(all(HOME_NEEDS_MAINTAINER$check %in% AUDIT_KIT_CHECKS$check))
  # 要問老師的檢查都有請教單的問題種類
  teach <- HOME_AUDIT_OWNER$check[HOME_AUDIT_OWNER$owner == "teacher"]
  expect_true(all(teach %in% names(CONSULT_AUDIT_TYPE)))
})

test_that("home_audit adds owner and a link to the right page", {
  ha <- home_audit(read_kit())
  expect_true(all(c("owner", "owner_label", "check_label", "tab", "target", "extra") %in% names(ha)))
  expect_false(any(ha$check %in% HOME_NEEDS_MAINTAINER$check))
  expect_true(all(ha$tab %in% c("", "search", "dims", "admin")))
  r <- ha[ha$check == "renamed_no_group", ][1, ]
  expect_identical(r$tab, "search")
  expect_identical(r$target, "caregiverf02n")
  expect_identical(r$extra, "P.caregiverf02n")
  r <- ha[ha$check == "group_single_item", ][1, ]
  expect_identical(r$tab, "dims")
  expect_false(grepl("^[A-Z]\\.", r$target))
  r <- ha[ha$check == "form_year_diff", ][1, ]
  expect_identical(r$tab, "admin")
  expect_identical(r$target, "M03M003")
  expect_identical(r$extra, "M03M003-P")
  s <- home_audit_summary(ha)
  expect_identical(sum(s$n), nrow(ha))
  expect_identical(unique(s$owner), c("self", "teacher", "stats"))
  expect_equal(nrow(home_audit(empty_kit())), 0L)
  expect_equal(nrow(home_audit_summary(home_audit(empty_kit()))), 0L)
})

test_that("home_links handles every table audit_kit uses", {
  kit <- read_kit()
  ln <- home_links(kit, c("value_codes", "groups", "dataset_vars", "datasets"),
                   c(paste0(kit$value_codes$valueSetID[1], "|1"), "P.x", "M03M003-P|v1", "v02"))
  expect_identical(ln$tab, c("search", "dims", "admin", ""))
  expect_identical(ln$target[2:3], c("x", "M03M003"))
})

test_that("to-dos can be added, completed and saved as CSV", {
  td <- empty_todo()
  td <- todo_add(td, "問陳老師 fatherinvo 題組", who = "小美", due = "11/30")
  td <- todo_add(td, "  ")  # 空白不加
  td <- todo_add(td, "核對 M36 教保問卷")
  expect_identical(td$id, c("1", "2"))
  td <- todo_done(td, "1")
  expect_identical(td$done, c("1", ""))
  f <- withr::local_tempfile(fileext = ".csv")
  write_todo_csv(td, f)
  expect_identical(readBin(f, "raw", 3L), as.raw(c(0xEF, 0xBB, 0xBF)))
  back <- read_todo_csv(f)
  expect_identical(back, td)
  expect_identical(todo_done(back, 1, FALSE)$done, c("", ""))

  # 手寫的 CSV：欄位不齊、沒有 id
  writeLines(c("task,who", "整理遺漏代碼,阿明", ",沒事"), f)
  x <- read_todo_csv(f)
  expect_identical(x$task, "整理遺漏代碼")
  expect_identical(x$id, "1")
  expect_identical(names(x), TODO_COLS)
})
