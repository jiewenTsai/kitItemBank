test_that("item_history follows renames and writes Word", {
  kit <- read_kit()
  h <- item_history(kit, "P.fatherinvo01")
  expect_setequal(h$family, c("P.fatherinvo01", "P.fatherinvo01n"))
  expect_true(any(h$relations$kind == "renamed"))
  expect_true("M36M084G01" %in% unlist(strsplit(h$versions$admins, "、")))
  f <- withr::local_tempfile(fileext = ".docx")
  item_history_docx(h, f)
  expect_true(file.size(f) > 5000)
})

test_that("dim_items_workbook has an intro sheet and one row per version", {
  b <- bank_to_v02(read_kit(), "parent")
  d <- "父親親職參與"
  wb <- dim_items_workbook(b, d, "parent")
  expect_length(wb, 2L)
  expect_equal(nrow(wb[[2]]), nrow(dim_items(b, d)))
  expect_true(any(grepl("M36M084G01", names(wb[[2]]))))
})
