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
  expect_true(nrow(query_items(b, keyword = "父親")) > 0L)
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
