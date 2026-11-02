test_that("bank_to_v02 turns the built-in kit into valid 0.2 banks", {
  kit <- read_kit()
  for (d in names(KIT_DOMAIN_INSTRUMENT)) {
    b <- bank_to_v02(kit, d)
    expect_true(check_bank(b)$ok, label = d)
    expect_identical(names(b$itemBank), c("itemID", "itemVer", "deprecated", "stemID", "block",
                                          "itemType", "respondent", "reverseScored", "label",
                                          "value", "noResponse", "note"))
  }
  b <- bank_to_v02(kit, "parent")
  x <- b$itemBank[b$itemBank$itemID %in% c("fatherinvo01", "fatherinvo01n"), ]
  expect_equal(x$deprecated, c(1L, 1L, 0L, 0L))
  w <- b$itemWave[b$itemWave$itemID == "fatherinvo01n", ]
  expect_equal(w[["M36M084G01"]], 1L)
  # 原文：父親
  expect_true(nrow(query_items(b, keyword = "\u7236\u89aa")) > 0L)
})

test_that("bank_to_v02 gives the same column types as load_bank", {
  # 0.3.0 雲端版點有同題幹的題目會斷線：itemVer 是字串，same_stem_items() 的 -itemVer 出錯
  b <- bank_to_v02(read_kit(), "parent")
  expect_type(b$itemBank$itemVer, "integer")
  expect_type(b$itemBank$deprecated, "integer")
  expect_type(b$itemDim$itemVer, "integer")
  ib <- b$itemBank
  st <- ib$stemID[nzchar(ib$stemID)]
  r <- ib[ib$stemID %in% st[duplicated(st)], , drop = FALSE][1L, ]
  d <- item_detail(b, r$itemID, r$itemVer)
  expect_false(is.null(d))
})

test_that("admin codes sort by age and read in plain words", {
  expect_equal(sort_wave_cols(c("M36M084G01", "M36M036", "M03M036", "M03M003")),
               c("M03M003", "M03M036", "M36M036", "M36M084G01"))
  expect_match(wave_label("M36M084G01"), "84")
})

test_that("grade aliases find the grade's administrations", {
  kit <- read_kit()
  both <- c("M03M084G01", "M36M084G01")
  # 原文：國小一年級 | 小一
  for (x in c("G01", "M084G01", "\u570b\u5c0f\u4e00\u5e74\u7d1a", "\u5c0f\u4e00")) {
    expect_setequal(resolve_admin(kit, x), both)
  }
  # 原文：國中一年級
  expect_equal(resolve_admin(kit, "\u570b\u4e2d\u4e00\u5e74\u7d1a"), "M36M156G07")
  ch <- make_col_choices(c("M36M084G01", "M36M036"))
  expect_match(names(ch)[1], "G01")
  # 原文：國小一年級
  expect_match(names(ch)[1], "\u570b\u5c0f\u4e00\u5e74\u7d1a")
  expect_false(grepl("G01", names(ch)[2]))
})

test_that("bank_cohort keeps one cohort's waves and items; the timeline splits M03 / M36", {
  b <- kit_bank("parent")
  expect_equal(bank_cohorts(b), c("M03", "M36"))
  m03 <- bank_cohort(b, "M03")
  expect_true(all(startsWith(get_wave_cols(m03$itemWave), "M03")))
  expect_true(nrow(m03$itemBank) < nrow(b$itemBank))
  expect_identical(bank_cohort(b, ""), b)
  d <- item_detail(b, "fatherinvo01", 2L)
  h <- htmltools::renderTags(timeline_full_ui(d))$html
  expect_match(h, "M03 世代")
  expect_match(h, "M36 世代")
  # 有問的波次寫版本號（version_timeline 給的是 ✓，不是 1）
  expect_match(h, "<td>v[0-9]</td>")
})
