# tests/testthat/test-propose-blocks.R — propose_blocks()：區塊狀態候選

# 小題庫：題本 F1 三個年度；2016 有兩次施測（A1、A2），A1 另有親友檔
pb_fixture <- function() {
  kit <- empty_kit()
  kit$cohorts <- conform_kit_table(data.frame(cohortID = "K1", code = "M03"), "cohorts")
  kit$admins <- conform_kit_table(data.frame(
    adminID = c("A1", "A2", "A3", "A4"), cohortID = "K1", ageMonth = c("3", "6", "12", "24"),
    surveyFrom = c("2016-06-01", "2016-09-01", "2017-03-01", "2018-03-01"),
    status = "released"), "admins")
  kit$forms <- conform_kit_table(data.frame(formID = "F1", instrument = "P", label = "F1"), "forms")
  kit$form_notes <- conform_kit_table(data.frame(formID = "F1", year = c("2016", "2017", "2018")),
                                      "form_notes")
  kit$datasets <- conform_kit_table(data.frame(
    datasetID = c("A1-P", "A2-P", "A3-P", "A4-P", "A1-PK"),
    adminID = c("A1", "A2", "A3", "A4", "A1"), file = c("data1", "data1", "data1", "data1", "data2"),
    instrument = "P", formID = "F1"), "datasets")
  kit$items <- conform_kit_table(data.frame(
    item_uid = c("P.x1", "P.x2", "P.y1", "P.y2", "P.z1", "P.q1", "P.w1"), instrument = "P",
    varname = c("x1", "x2", "y1", "y2", "z1", "q1", "w1"),
    block = c("X", "X", "Y", "Y", "Z", "", "W")), "items")
  # 原文：適用:父母版,簡親友版／適用:父母版
  kit$item_versions <- conform_kit_table(data.frame(
    item_uid = kit$items$item_uid, itemVer = "1",
    note = ifelse(kit$items$item_uid == "P.x1", "\u9069\u7528:\u7236\u6bcd\u7248,\u7c21\u89aa\u53cb\u7248",
                  "\u9069\u7528:\u7236\u6bcd\u7248")), "item_versions")
  vars <- list(`A1-P` = c("x1", "x2", "y1", "y2", "z1", "q1", "w1"),
               `A2-P` = c("x1", "x2", "y1", "w1"),
               `A3-P` = c("x1", "x2", "y1", "y2", "z1"),
               `A4-P` = c("x1", "x2", "y1", "y2", "z1", "w1"),
               `A1-PK` = c("x1"))
  kit$dataset_vars <- conform_kit_table(do.call(rbind, lapply(names(vars), function(d) data.frame(
    datasetID = d, varname = vars[[d]], item_uid = paste0("P.", vars[[d]]), itemVer = "1",
    kind = "item"))), "dataset_vars")
  kit$sources <- conform_kit_table(data.frame(sourceID = c("US-M03W06", "V02-BANK"),
                                              kind = c("usage_doc", "v02_bank")), "sources")
  kit
}

pb_get <- function(bs, d, b) bs[bs$datasetID == d & bs$block == b, , drop = FALSE]

test_that("same form-year: missing block -> none, partial block -> subset", {
  res <- propose_blocks(pb_fixture())
  bs <- res$block_status
  expect_identical(names(bs), c(names(KIT_SCHEMA$block_status$cols), "confidence", "basis"))
  expect_true(all(bs$decided_by == "auto"))
  z <- pb_get(bs, "A2-P", "Z")
  expect_identical(z$asked, "none")
  expect_identical(z$confidence, "high")
  y <- pb_get(bs, "A2-P", "Y")
  expect_identical(y$asked, "subset")
  expect_identical(y$confidence, "medium")
  # A1 有 A2 的全部題目：沒有列
  expect_equal(nrow(bs[bs$datasetID == "A1-P", ]), 0L)
  expect_false(any(bs$asked == "all"))
  expect_true(any(res$pairs$kind == "same_year" & res$pairs$revision == "2016"))
})

test_that("unexplained lists item differences without a block", {
  ux <- propose_blocks(pb_fixture())$unexplained
  expect_identical(ux$datasetID, "A2-P")
  expect_identical(ux$varname, "q1")
  expect_identical(ux$present_in, "A1-P")
})

test_that("a block skipped between two years is a rotation candidate", {
  bs <- propose_blocks(pb_fixture())$block_status
  w <- pb_get(bs, "A3-P", "W")
  expect_identical(w$asked, "none")
  expect_identical(w$confidence, "medium")
})

test_that("kin files: absent parent blocks are none, partial ones subset", {
  bs <- propose_blocks(pb_fixture())$block_status
  y <- pb_get(bs, "A1-PK", "Y")
  expect_identical(y$asked, "none")
  # 原文：親友版不問
  expect_identical(y$who, "\u89aa\u53cb\u7248\u4e0d\u554f")
  expect_identical(y$confidence, "high")
  x <- pb_get(bs, "A1-PK", "X")
  expect_identical(x$asked, "subset")
  expect_identical(x$confidence, "high")
  expect_identical(x$sourceID, "V02-BANK")
})

test_that("usage documents give carried rows and back up none rows", {
  dir <- tempfile("pb_us"); dir.create(dir); on.exit(unlink(dir, recursive = TRUE))
  # 原文：(8) 關於本波次家長問卷：／部分資料未於本波再詢問。這些未再詢問的題目包含：z1。其中，本計畫串回了：x1, x2。
  writeLines(c("  (8) \u95dc\u65bc\u672c\u6ce2\u6b21\u5bb6\u9577\u554f\u5377\uff1a",
               paste0("i.  \u90e8\u5206\u8cc7\u6599\u672a\u65bc\u672c\u6ce2\u518d\u8a62\u554f\u3002",
                      "\u9019\u4e9b\u672a\u518d\u8a62\u554f\u7684\u984c\u76ee\u5305\u542b\uff1az1\u3002",
                      "\u5176\u4e2d\uff0c\u672c\u8a08\u756b\u4e32\u56de\u4e86\uff1ax1, x2\u3002")),
             file.path(dir, "us_M03W06.txt"), useBytes = TRUE)
  bs <- propose_blocks(pb_fixture(), usage_dir = dir)$block_status
  x <- pb_get(bs, "A2-P", "X")
  expect_identical(x$asked, "carried")
  expect_identical(x$confidence, "high")
  expect_identical(x$sourceID, "US-M03W06")
  z <- pb_get(bs, "A2-P", "Z")
  expect_identical(z$asked, "none")
  expect_identical(z$sourceID, "US-M03W06")
})

test_that("crosswalk-only differences show up as unexplained", {
  cw <- data.frame(cohort = "M03", q = "\u5bb6\u9577\u554f\u5377", age = c(3, 3, 6),
                   var = c("x1", "tinfo", "x1"))
  ux <- propose_blocks(pb_fixture(), crosswalk = cw)$unexplained
  expect_true(any(ux$varname == "tinfo" & ux$datasetID == "A2-P" & ux$source == "crosswalk"))
})

test_that("candidates pass check_kit", {
  kit <- pb_fixture()
  bs <- propose_blocks(kit)$block_status
  kit$block_status <- conform_kit_table(bs, "block_status")
  expect_equal(nrow(check_kit(kit)), 0L)
})
