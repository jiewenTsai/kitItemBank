# utils.R：波次代碼的白話

test_that("wave_label explains wave codes and leaves others alone", {
  expect_equal(
    wave_label(c("M036大", "M003小", "M012保母", "M012機構", "M084",
                 "國1_大月齡", "小4_小月齡", "睡眠作息")),
    c("M036大 · 36 個月（大月齡世代）", "M003小 · 3 個月（小月齡世代）",
      "M012保母 · 12 個月（保母）", "M012機構 · 12 個月（機構）", "M084 · 84 個月",
      "國1_大月齡 · 國一（大月齡世代）", "小4_小月齡 · 小四（小月齡世代）", "睡眠作息")
  )
  expect_equal(wave_label(character()), character())
})

test_that("make_col_choices keeps the codes as values; col_display_labels stays short", {
  ch <- make_col_choices(c("M036大", "a\nb"))
  expect_equal(unname(ch), c("M036大", "a\nb"))
  expect_equal(names(ch), c("M036大 · 36 個月（大月齡世代）", "a b"))
  expect_equal(col_display_labels(c("M036大", "a\nb")), c("M036大", "a b"))
  expect_equal(col_display_labels("M036大", plain = TRUE), "M036大 · 36 個月（大月齡世代）")
})

test_that("every wave in the built-in banks gets a plain label", {
  for (d in unname(BANK_DOMAINS)) {
    w <- get_wave_cols(.bank(d)$itemWave)
    expect_false(anyNA(wave_meaning(w)), info = d)
  }
})

test_that("dates and times use the user's time zone, not the server's", {
  withr::local_options(kititembank.tz = "Asia/Taipei")
  expect_identical(kit_tz(), "Asia/Taipei")
  expect_identical(kit_today(), as.Date(format(Sys.time(), "%Y-%m-%d", tz = "Asia/Taipei")))
  expect_identical(attr(kit_now(), "tzone"), "Asia/Taipei")
  withr::local_options(kititembank.tz = "Not/AZone")
  expect_true(kit_tz() %in% OlsonNames())
  s <- shiny::MockShinySession$new()
  s$userData$tz <- "America/New_York"
  shiny::withReactiveDomain(s, expect_identical(kit_tz(), "America/New_York"))
})
