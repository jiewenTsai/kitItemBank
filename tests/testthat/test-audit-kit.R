# tests/testthat/test-audit-kit.R — audit_kit() 的小例子

# 一個小 kit：兩個同題本同年度的家長檔、一個內部施測檔
mini_kit <- function() {
  k <- empty_kit()
  add <- function(tb, ...) conform_kit_table(data.frame(..., stringsAsFactors = FALSE), tb)
  k$admins <- add("admins", adminID = c("A1", "A2", "A3"), cohortID = "K",
                  surveyFrom = c("2019-03-01", "2019-03-01", ""),
                  status = c("released", "released", "internal"))
  k$datasets <- add("datasets", datasetID = c("A1-P", "A2-P", "A3-P"),
                    adminID = c("A1", "A2", "A3"), file = c("data1", "data1", "v02"),
                    instrument = "P", formID = "F", respondent = "parent",
                    missingScheme = c("2016", "2016", "2020"))
  k$missing_schemes <- add("missing_schemes", scheme = c("2016", "2020"), code = "7777",
                           label = "r", kind = "refused")
  k$items <- add("items", item_uid = c("P.a", "P.b", "P.c", "P.old", "P.new", "P.x"),
                 instrument = "P", varname = c("a", "b", "c", "old", "new", "x"),
                 block = c("B1", "B1", "B2", "B1", "B1", "B1"),
                 role = c("core", "core", "core", "core", "core", ""),
                 module = c("m", "m", "m", "m", "m", "m"),
                 deprecated = c("0", "0", "0", "1", "0", "0"))
  k$item_versions <- add("item_versions", item_uid = c("P.a", "P.b", "P.c"), itemVer = "1",
                         # 原文：x这
                         label = c("ok (fine)", "bad (open", "x\u8fd9"),
                         stem = c(" lead", "", "a\nb"), valueSetID = c("vs1", "", ""))
  k$value_codes <- add("value_codes", valueSetID = "vs1", code = c("1", "5"),
                       label = c("yes", "no "))
  k$dataset_vars <- add("dataset_vars",
                        datasetID = c("A1-P", "A1-P", "A1-P", "A2-P", "A2-P", "A3-P"),
                        varname = c("a", "b", "zz", "a", "c", "a"),
                        item_uid = c("P.a", "P.b", "", "P.a", "P.c", "P.a"),
                        itemVer = c("1", "1", "", "1", "1", "1"), kind = "item")
  k$groups <- add("groups", groupID = c("G.top", "G.leaf", "G.one", "G.dep", "G.none"),
                  label = "g")
  k$group_tree <- add("group_tree", formID = "F", groupID = "G.leaf", parentID = "G.top")
  k$group_members <- add("group_members", formID = "F",
                         groupID = c("G.top", "G.leaf", "G.leaf", "G.one", "G.dep"),
                         item_uid = c("P.a", "P.a", "P.b", "P.c", "P.old"))
  k$item_relations <- add("item_relations", from_uid = "P.old", from_ver = "1",
                          to_uid = "P.new", to_ver = "1", kind = "renamed")
  k
}

test_that("audit_kit returns the documented columns and never changes the kit", {
  k <- mini_kit()
  res <- audit_kit(k)
  expect_identical(names(res), c("level", "check", "table", "key", "detail"))
  expect_true(all(res$level == "note"))
  expect_true(all(res$check %in% AUDIT_KIT_CHECKS$check))
  expect_identical(k, mini_kit())
  empty <- audit_kit(empty_kit())
  expect_identical(nrow(empty), 0L)
  expect_identical(names(empty), names(res))
})

test_that("coverage: blank item_uid and .sav mismatches", {
  k <- mini_kit()
  sav <- list(datasets = data.frame(datasetID = c("A1-P", "A2-P")),
              dataset_vars = data.frame(datasetID = c("A1-P", "A1-P", "A1-P", "A2-P"),
                                        varname = c("a", "b", "extra", "a")))
  res <- audit_kit(k, sav = sav)
  cv <- res[res$check == "coverage_uid_blank", ]
  expect_identical(cv$key, "A1-P|zz")
  sv <- res[res$check == "coverage_sav", ]
  expect_setequal(sv$key, c("A1-P|extra", "A1-P|zz", "A2-P|c"))
  # 沒給 sav 就不比
  expect_false(any(audit_kit(k)$check == "coverage_sav"))
})

test_that("same form and year datasets with different items, unless block_status explains", {
  k <- mini_kit()
  res <- audit_kit(k)
  fy <- res[res$check == "form_year_diff", ]
  expect_identical(fy$key, "A1-P|A2-P")
  expect_match(fy$detail, "B2 1")
  k$block_status <- conform_kit_table(
    data.frame(datasetID = c("A1-P", "A2-P"), block = c("B1", "B2"), asked = "subset",
               stringsAsFactors = FALSE), "block_status")
  expect_false(any(audit_kit(k)$check == "form_year_diff"))
})

test_that("renamed item without the old item's groups", {
  res <- audit_kit(mini_kit())
  expect_identical(res$key[res$check == "renamed_no_group"], "P.old|1|P.new|1")
})

test_that("non-response codes outside the dataset scheme are aggregated per item", {
  nr <- data.frame(item_uid = c("P.a", "P.a", "P.b"), itemVer = "1",
                   code = c("5", "7777", "5"),
                   # 原文：拒答
                   label = "\u62d2\u7b54", stringsAsFactors = FALSE)
  res <- audit_kit(mini_kit(), noresponse = nr)
  mc <- res[res$check == "missing_code_scheme", ]
  expect_setequal(mc$key, c("P.a|1", "P.b|1"))
  a <- mc$detail[mc$key == "P.a|1"]
  expect_match(a, "A1-P")
  expect_match(a, "A3-P")
  expect_match(a, "2016")
  # 5 也是 vs1 的選項代碼
  # 原文：也是選項代碼
  expect_match(a, "\u4e5f\u662f\u9078\u9805\u4ee3\u78bc")
  expect_false(grepl("7777", a))
})

test_that("group checks: non-leaf members, single item, only deprecated, empty", {
  res <- audit_kit(mini_kit())
  expect_identical(res$key[res$check == "group_nonleaf_member"], "F|G.top")
  expect_identical(res$key[res$check == "group_single_item"], "F|G.one")
  expect_identical(res$key[res$check == "group_only_deprecated"], "F|G.dep")
  expect_identical(res$key[res$check == "group_empty"], "G.none")
})

test_that("text checks reuse the 0.2 rules", {
  res <- audit_kit(mini_kit())
  expect_identical(res$key[res$check == "text_brackets"], "P.b|1")
  expect_identical(res$key[res$check == "text_simplified"], "P.c|1")
  expect_setequal(res$key[res$check == "text_space"], c("P.a|1", "vs1|5"))
  expect_identical(res$key[res$check == "text_newline"], "P.c|1")
})

test_that("internal admins and blank role/module", {
  res <- audit_kit(mini_kit())
  ia <- res[res$check == "internal_admins", ]
  expect_identical(nrow(ia), 1L)
  expect_match(ia$detail, "^1 ")
  expect_identical(res$key[res$check == "role_module_blank"], "P.x")
})

test_that("audit_kit_summary counts per check", {
  res <- audit_kit(mini_kit())
  s <- audit_kit_summary(res)
  expect_identical(sum(s$n), nrow(res))
  expect_true(all(s$n > 0L))
  expect_identical(nrow(audit_kit_summary(res, all = TRUE)), nrow(AUDIT_KIT_CHECKS))
})
