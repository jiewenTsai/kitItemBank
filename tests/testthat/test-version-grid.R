test_that("version_grid writes the version asked in each wave, per cohort, with anchor items", {
  b <- kit_bank("parent")
  g <- version_grid(b, "fatherinvo01")
  expect_identical(g$itemID, "fatherinvo01")
  w <- get_wave_cols(b$itemWave)
  expect_true(all(unlist(g[intersect(names(g), w)]) %in% c("", "v1", "v2", "v1/v2", "v3", "v2/v3")))
  m03 <- version_grid(b, cohort = "M03")
  expect_true(all(startsWith(setdiff(names(m03), c("itemID", "block", "label", "anchor")), "M03")))
  expect_true(all(m03$anchor %in% c("", "★")))
  expect_true(any(m03$anchor == "★"))
  expect_false("anchor" %in% names(version_grid(b)))
})

test_that("version_grid_workbook has a read-me sheet and one sheet per cohort", {
  b <- kit_bank("parent")
  wb <- version_grid_workbook(b, "parent")
  expect_identical(names(wb), c("說明", "M03", "M36"))
  expect_identical(names(wb$M03)[1:4], c("題號", "區塊", "題目", "錨題"))
  f <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(wb, f)
  expect_true(file.size(f) > 0)
})

test_that("the item timeline shows version numbers, one row per cohort table", {
  b <- kit_bank("parent")
  d <- item_detail(b, "bodyBg12", 1L)
  h <- htmltools::renderTags(timeline_full_ui(d))$html
  expect_match(h, "<td>v1</td>")
  expect_match(h, "M03 世代")
})
