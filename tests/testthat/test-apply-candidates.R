test_that("apply_group_candidates adds groups, edges and members, keeping drops out", {
  kit <- empty_kit()
  kit$forms <- conform_kit_table(data.frame(formID = "P-X"), "forms")
  kit$items <- conform_kit_table(data.frame(item_uid = c("P.a", "P.b"), instrument = "P",
                                            varname = c("a", "b")), "items")
  paths <- data.frame(formID = "P-X", groupID = c("P.sec", "P.top", "P.leaf"),
                      parentID = c("", "P.sec", "P.top"), label = c("sec", "top", "leaf"))
  hints <- data.frame(formID = "P-X", groupID = c("P.leaf", "P.leaf", "P.sec"),
                      item_uid = c("P.a", "P.zz", "P.b"), reverse = c("1", "", ""))
  k <- apply_group_candidates(kit, paths, hints, drop = "P.sec")
  expect_setequal(k$groups$groupID, c("P.top", "P.leaf"))
  expect_equal(k$groups$label[k$groups$groupID == "P.leaf"], "leaf")
  expect_equal(k$group_tree$parentID[k$group_tree$groupID == "P.top"], "")
  expect_equal(k$group_tree$parentID[k$group_tree$groupID == "P.leaf"], "P.top")
  # P.zz 不在 items、P.sec 拿掉了
  expect_equal(k$group_members$item_uid, "P.a")
  expect_equal(k$group_members$reverse, "1")
})

test_that("apply_relation_hints keeps only new relations between existing versions", {
  kit <- empty_kit()
  kit$item_versions <- conform_kit_table(data.frame(item_uid = c("P.a", "P.a", "P.b"),
                                                    itemVer = c("1", "2", "1")), "item_versions")
  kit$item_relations <- conform_kit_table(data.frame(from_uid = "P.a", from_ver = "1",
                                                     to_uid = "P.a", to_ver = "2",
                                                     kind = "revised"), "item_relations")
  hints <- data.frame(from_uid = c("P.a", "P.a", "P.a", "P.b"), from_ver = c("1", "2", "1", "1"),
                      to_uid = c("P.a", "P.a", "P.b", "P.c"), to_ver = c("2", "2", "1", "1"),
                      kind = c("revised", "revised", "renamed", "renamed"))
  k <- apply_relation_hints(kit, hints, "PI", "2026-11-02")
  expect_equal(nrow(k$item_relations), 2L)
  r <- k$item_relations[k$item_relations$to_uid == "P.b", ]
  expect_equal(r$kind, "renamed")
  expect_equal(r$decided_by, "PI")
})
