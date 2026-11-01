test_that("an empty kit passes and round-trips", {
  kit <- empty_kit()
  expect_setequal(names(kit), kit_tables())
  expect_equal(nrow(check_kit(kit)), 0L)
  d <- withr::local_tempdir()
  write_kit(kit, d)
  expect_equal(read_kit(d), kit)
})

test_that("check_kit catches keys, foreign keys, values and cycles", {
  kit <- empty_kit()
  kit$cohorts <- conform_kit_table(data.frame(cohortID = c("K2016", "K2016"), code = "M03"), "cohorts")
  kit$admins <- conform_kit_table(data.frame(adminID = "M03M036", cohortID = "K9999",
                                             ageMonth = "36x", status = "public"), "admins")
  kit$forms <- conform_kit_table(data.frame(formID = "P-36-72", instrument = "P", label = "x"), "forms")
  kit$groups <- conform_kit_table(data.frame(groupID = c("a", "b", "c"), label = "x"), "groups")
  kit$group_tree <- conform_kit_table(data.frame(formID = "P-36-72", groupID = c("a", "b", "c"),
                                                 parentID = c("b", "a", "")), "group_tree")
  res <- check_kit(kit)
  expect_true(all(c("duplicate_key", "foreign_key", "bad_value", "bad_type", "tree_cycle")
                  %in% res$check))
  expect_setequal(res$key[res$check == "tree_cycle"], c("P-36-72|a", "P-36-72|b"))
})

test_that("multi-column foreign keys are checked", {
  kit <- empty_kit()
  kit$items <- conform_kit_table(data.frame(item_uid = "P.x", instrument = "P", varname = "x"), "items")
  kit$item_versions <- conform_kit_table(data.frame(item_uid = "P.x", itemVer = "1"), "item_versions")
  kit$item_relations <- conform_kit_table(
    data.frame(from_uid = "P.x", from_ver = "1", to_uid = "P.x", to_ver = "2", kind = "revised"),
    "item_relations")
  res <- check_kit(kit)
  expect_equal(res$check, "foreign_key")
  expect_equal(res$value, "P.x|2")
})
