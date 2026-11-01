# tests/testthat/test-propose-relations.R — propose_relations()：換題號、改版、新題號

rel_fixture <- function() {
  df <- function(...) data.frame(..., stringsAsFactors = FALSE)
  kit <- empty_kit()
  kit$cohorts <- conform_kit_table(df(cohortID = "K1", code = "M03", birth_from = "2016-04-01"),
                                   "cohorts")
  kit$admins <- conform_kit_table(df(adminID = c("A03", "A06"), cohortID = "K1",
                                     ageMonth = c("3", "6"), status = "released"), "admins")
  kit$forms <- conform_kit_table(df(formID = c("F1", "F2"), instrument = "P", label = c("f1", "f2")),
                                 "forms")
  kit$datasets <- conform_kit_table(df(datasetID = c("A03-P", "A06-P"), adminID = c("A03", "A06"),
                                       file = "data1", instrument = "P", formID = c("F1", "F2")),
                                    "datasets")
  kit$items <- conform_kit_table(df(
    item_uid = c("P.fat01", "P.famC01", "P.famC01n", "P.abc"), instrument = "P",
    varname = c("fat01", "famC01", "famC01n", "abc"), block = "b1", role = "measure",
    module = "family"), "items")
  # 原文：v1→v2版本異動: 題意變動
  note_abc <- "v1→v2版本異動: 題意變動"
  kit$item_versions <- conform_kit_table(df(
    item_uid = c("P.fat01", "P.famC01", "P.famC01n", "P.abc", "P.abc"),
    itemVer = c("1", "1", "1", "1", "2"),
    label = c("Father helps child.", "We go out.", "We go out!", "Old wording here",
              "Old wording here."),
    valueSetID = c("vs0001", "vs0001", "vs0002", "vs0001", "vs0001"),
    itemType = "MC", note = c("", "", "", "", note_abc)), "item_versions")
  kit$value_codes <- conform_kit_table(df(valueSetID = c("vs0001", "vs0001", "vs0002"),
                                          code = c("1", "2", "1"), label = c("low", "high", "")),
                                       "value_codes")
  kit$missing_schemes <- conform_kit_table(df(scheme = "2016", code = "9999", label = "m",
                                              kind = "missing"), "missing_schemes")
  kit$sources <- conform_kit_table(df(sourceID = c("V02-BANK", "US-M03W06"),
                                      kind = c("v02_bank", "usage_doc")), "sources")
  kit$dataset_vars <- conform_kit_table(df(
    datasetID = c("A03-P", "A06-P", "A03-P", "A06-P", "A03-P", "A06-P"),
    varname = c("fat01", "fat01n", "famC01", "famC01n", "abc", "abc"),
    item_uid = c("P.fat01", "", "P.famC01", "P.famC01n", "P.abc", "P.abc"),
    itemVer = c("1", "", "1", "1", "1", "2"), kind = "item"), "dataset_vars")
  kit$groups <- conform_kit_table(df(groupID = "P.G", label = "G"), "groups")
  kit$group_tree <- conform_kit_table(df(formID = c("F1", "F2"), groupID = "P.G"), "group_tree")
  kit$group_members <- conform_kit_table(df(formID = c("F1", "F2"), groupID = "P.G",
                                            item_uid = "P.fat01", scheme = "official",
                                            reverse = "0"), "group_members")
  sav <- list(
    value_sets = df(datasetID = "A06-P", varname = "fat01n", code = c("1", "2", "9999"),
                    label = c("low", "high", "m"), is_missing = c("0", "0", "1")),
    map = df(wave_dir = "m03w06", file = "data1", datasetID = "A06-P"))
  list(kit = kit, sav = sav)
}

test_that("rel_similarity grades labels after normalising punctuation", {
  expect_equal(rel_similarity("a, b c.", "abc"), "identical")
  expect_equal(rel_similarity("abcdefghij", "abcdefghik"), "high")
  expect_equal(rel_similarity("abcdefghij", "xyzdefghij"), "medium")
  expect_equal(rel_similarity("abcdefghij", "zzzzzzzzzz"), "low")
  expect_equal(rel_similarity("", "abc"), "")
  # 只有一邊有共用題幹時拿掉再比
  expect_equal(rel_similarity("stem: item", "item", stem_b = "", stem_a = "stem:"), "identical")
})

test_that("X / Xn renames, new items and revisions are proposed", {
  fx <- rel_fixture()
  r <- propose_relations(fx$kit, NULL, fx$sav)
  rel <- r$relations
  expect_true(all(names(KIT_SCHEMA$item_relations$cols) %in% names(rel)))
  expect_true(all(c("confidence", "basis") %in% names(rel)))
  expect_true(all(rel$decided_by == "auto"))

  # .sav 才有的 fat01n → 新題號，前身 P.fat01
  expect_equal(r$new_items$item_uid, "P.fat01n")
  expect_equal(r$new_items$block, "b1")
  expect_equal(r$new_item_versions$valueSetID, "vs0001")
  expect_equal(r$dataset_vars_fix$item_uid, "P.fat01n")
  expect_equal(r$new_members$formID, "F2")
  expect_equal(r$new_members$groupID, "P.G")

  fat <- rel[rel$from_uid == "P.fat01", ]
  expect_equal(nrow(fat), 1L)
  expect_equal(fat$kind, "renamed")
  expect_equal(fat$to_uid, "P.fat01n")
  expect_equal(fat$effective_admin, "A06")
  expect_equal(fat$similarity, "identical")
  expect_equal(fat$confidence, "high")
  expect_equal(fat$sourceID, "US-M03W06")

  # 兩邊都在題庫、值組不同 → medium、partial、意義改變
  fam <- rel[rel$from_uid == "P.famC01", ]
  expect_equal(fam$kind, "renamed")
  expect_equal(fam$confidence, "medium")
  expect_equal(fam$comparable, "partial")
  expect_equal(fam$meaning_changed, "1")

  abc <- rel[rel$from_uid == "P.abc", ]
  expect_equal(abc$kind, "revised")
  expect_equal(c(abc$from_ver, abc$to_ver), c("1", "2"))
  expect_equal(abc$meaning_changed, "1")
  expect_equal(abc$effective_admin, "A06")
  expect_match(abc$reason, "^v1")
  expect_equal(abc$confidence, "high")

  kit2 <- apply_relations(fx$kit, r, c("high", "medium", "low"))
  expect_equal(nrow(check_kit(kit2)), 0L)
  expect_equal(kit2$dataset_vars$item_uid[kit2$dataset_vars$varname == "fat01n"], "P.fat01n")
})

test_that("case-insensitive pairs and new value sets", {
  fx <- rel_fixture()
  kit <- fx$kit
  kit$dataset_vars$varname[kit$dataset_vars$varname == "fat01n"] <- "FAT01n"
  sav <- fx$sav
  sav$value_sets$varname <- "FAT01n"
  sav$value_sets$code[2] <- "3"
  r <- propose_relations(kit, NULL, sav)
  expect_equal(r$new_items$item_uid, "P.FAT01n")
  expect_equal(r$new_value_codes$valueSetID, c("vs0003", "vs0003"))
  expect_equal(r$new_item_versions$valueSetID, "vs0003")
  kit2 <- apply_relations(kit, r, c("high", "medium", "low"))
  expect_equal(nrow(check_kit(kit2)), 0L)
})
