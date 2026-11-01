# home.R：首頁的要注意清單（分給誰、連到哪）
# 不寫死題庫內容：預期的連結從每一筆的 key 推出來，題庫改版也成立

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
})

test_that("home_audit adds owner and a link to the right page", {
  kit <- read_kit()
  ha <- home_audit(kit)
  expect_true(all(c("owner", "owner_label", "check_label", "tab", "target", "extra") %in% names(ha)))
  expect_false(any(ha$check %in% HOME_NEEDS_MAINTAINER$check))
  expect_true(all(ha$tab %in% c("", "search", "dims", "admin")))
  expect_identical(ha$owner, HOME_AUDIT_OWNER$owner[match(ha$check, HOME_AUDIT_OWNER$check)])
  seg <- function(k, i) vapply(strsplit(k, "|", fixed = TRUE), `[`, "", i)

  r <- ha[ha$table == "item_relations", , drop = FALSE]
  if (nrow(r)) {
    expect_true(all(r$tab == "search"))
    expect_identical(r$extra, seg(r$key, 3))
    expect_identical(r$target, kit_strip_prefix(seg(r$key, 3)))
  }
  r <- ha[ha$table %in% c("items", "item_versions"), , drop = FALSE]
  if (nrow(r)) expect_identical(r$extra, seg(r$key, 1))
  r <- ha[ha$table == "group_members", , drop = FALSE]
  if (nrow(r)) {
    expect_true(all(r$tab == "dims"))
    expect_identical(r$target, kit_strip_prefix(seg(r$key, 2)))
  }
  r <- ha[ha$table == "datasets" & ha$tab == "admin", , drop = FALSE]
  if (nrow(r)) {
    expect_identical(r$extra, seg(r$key, 1))
    expect_identical(r$target, kit$datasets$adminID[match(r$extra, kit$datasets$datasetID)])
  }

  s <- home_audit_summary(ha)
  expect_identical(sum(s$n), nrow(ha))
  expect_identical(s$owner, s$owner[order(match(s$owner, names(HOME_OWNERS)))])
  expect_equal(nrow(home_audit(empty_kit())), 0L)
  expect_equal(nrow(home_audit_summary(home_audit(empty_kit()))), 0L)
})

test_that("home_links handles every table audit_kit uses", {
  kit <- read_kit()
  ds <- kit$datasets$datasetID[1]
  rel <- kit$item_relations[1, ]
  ln <- home_links(kit,
                   c("value_codes", "groups", "dataset_vars", "datasets", "items", "item_relations"),
                   c(paste0(kit$item_versions$valueSetID[nzchar(kit$item_versions$valueSetID)][1], "|1"),
                     "P.x", paste0(ds, "|v1"), "v02", "P.abc",
                     paste(rel$from_uid, rel$from_ver, rel$to_uid, rel$to_ver, sep = "|")))
  expect_identical(ln$tab, c("search", "dims", "admin", "", "search", "search"))
  expect_identical(ln$target[2:3], c("x", kit$datasets$adminID[1]))
  expect_identical(ln$extra[5:6], c("P.abc", rel$to_uid))
})
