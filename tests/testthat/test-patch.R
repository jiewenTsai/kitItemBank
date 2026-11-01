test_that("edits on the 0.2 view become a kit patch that applies cleanly", {
  kit <- read_kit()
  old <- bank_to_v02(kit, "parent")
  new <- old
  # 挑一題還不在這個題組的題目（候選套用後 caregiverf02n 已經在裡面）
  id <- setdiff(new$itemDim$itemID, new$itemDim$itemID[new$itemDim[["女方"]] == 1L])[1]
  i <- which(new$itemDim$itemID == id)
  new$itemDim[i, "女方"] <- 1L
  d <- new$dimMeta$dimID == "父親親職參與"
  new$dimMeta$dimName[d] <- "x"
  new$itemBank$label[new$itemBank$itemID == "fatherinvo01n"] <- "y"
  j <- which(new$itemWave$M03M003 == 1L)[1]
  new$itemWave[j, "M03M003"] <- 0L
  res <- kit_patch(kit, old, new, "parent")
  expect_setequal(res$patch$op, c("set", "add"))
  expect_equal(nrow(res$unsupported), 1L)  # itemWave 不從這裡改
  k2 <- apply_patch(kit, res$patch)
  expect_equal(nrow(check_kit(k2)), 0L)
  expect_true(any(k2$group_members$item_uid == paste0("P.", id) &
                    k2$group_members$groupID == "P.女方"))
  expect_equal(k2$groups$label[k2$groups$groupID == "P.父親親職參與"], "x")

  f <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(patch_workbook(res, bank_diff(old, new), "parent"), f)
  expect_equal(read_patch_xlsx(f)$patch, res$patch)
})

test_that("apply_patch refuses rows it cannot apply", {
  kit <- empty_kit()
  p <- data.frame(op = "delete", table = "groups", key = "P.x", column = "", from = "", to = "")
  expect_error(apply_patch(kit, p))
  p$op <- "add"
  k2 <- apply_patch(kit, p)
  expect_equal(k2$groups$groupID, "P.x")
  expect_error(apply_patch(k2, p))
})

test_that("apply_patch adds a row whose last key column is empty", {
  kit <- empty_kit()
  p <- data.frame(op = "add", table = "group_tree", key = "P-G1-G3|P.x|", column = "", from = "", to = "")
  k2 <- apply_patch(kit, p)
  expect_identical(k2$group_tree$parentID, "")
  expect_identical(k2$group_tree$groupID, "P.x")
})
