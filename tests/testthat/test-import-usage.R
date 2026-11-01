# tests/testthat/test-import-usage.R — 步驟 1.2：研究設計表、0.2 波次對照、資料使用說明解析

manual_dir <- function() {
  d <- testthat::test_path("..", "..", "data-raw", "kit_manual")
  if (!dir.exists(d)) testthat::skip("data-raw/kit_manual not available")
  d
}

test_that("v02_wave_map covers every 0.2 wave column", {
  wm <- v02_wave_map()
  for (dm in names(V02_DOMAIN_INSTRUMENT)) {
    hdr <- names(utils::read.csv(file.path(bank_dir(), dm, "itemWave.csv"), nrows = 1,
                                 check.names = FALSE, encoding = "UTF-8"))
    expect_setequal(wm$wave_col[wm$domain == dm], setdiff(hdr, BANK_KEYS))
  }
  expect_false(anyNA(wm$formID))
  expect_true(all(wm$status %in% c("released", "internal")))
  # 原文：M036大 | M036小 | 小2_小月齡 | 國1_大月齡 | M012保母
  pick <- function(col) wm[wm$wave_col == col, , drop = FALSE]
  expect_equal(pick("M036大")$adminID, "M36M036")
  expect_equal(pick("M036小")$adminID, "M03M036")
  expect_equal(pick("小2_小月齡")$adminID, "M03M096G02")
  expect_equal(pick("國1_大月齡")$adminID, "M36M156G07")
  expect_equal(pick("M012保母")$datasetID, "M03M012-T")
  expect_equal(pick("M084")$datasetID, "M36M084G01-T")
  expect_true(all(wm$adminID[wm$status == "released"] %in% KIT_RELEASED_ADMINS))
  expect_true(all(!(wm$adminID[wm$status == "internal"] %in% KIT_RELEASED_ADMINS)))
})

test_that("make_admin_id adds grade from 84 months", {
  expect_equal(make_admin_id(c("M03", "M36", "M36"), c(3, 84, 156)),
               c("M03M003", "M36M084G01", "M36M156G07"))
})

test_that("import_design builds tables that pass check_kit", {
  des <- import_design(manual_dir())
  kit <- empty_kit()
  for (tb in intersect(names(des), names(kit))) kit[[tb]] <- des[[tb]]
  expect_equal(nrow(check_kit(kit)), 0L)
  expect_equal(sum(des$admins$status == "released"), 12L)
  expect_setequal(des$admins$adminID[des$admins$status == "released"], KIT_RELEASED_ADMINS)
  expect_true(all(des$wave_map$datasetID %in% des$datasets$datasetID))
  expect_true(all(des$wave_map$wave_col %in% des$aliases$alias))
  expect_true(all(c("M36W84", "D00250", "M36M084") %in% des$aliases$alias))
  v02 <- des$datasets[des$datasets$file == "v02", ]
  expect_true(all(des$admins$status[match(v02$adminID, des$admins$adminID)] == "internal"))
  # 遺漏代碼方案：M03M048 起 2020
  ms <- des$datasets$missingScheme[des$datasets$adminID == "M03M048"]
  expect_true(all(ms == "2020"))
  expect_true(all(des$datasets$missingScheme[des$datasets$adminID == "M36M084G01"] == "2016"))
})

test_that("import_usage_doc parses a usage document", {
  # 原文：幼兒發展調查資料庫建置計畫：3月齡組第七波48月齡 …（節錄）
  txt <- c(
    "幼兒發展調查資料庫建置計畫：3月齡組第七波48月齡",
    "|   調查執行期間 | 2020-03-01~2021-07-31   |",
    "|   data1.csv    | 家長CSV資料檔   |",
    "|   data3.csv    | 教保CSV資料檔   |",
    "|   IRT.csv      | 家長發展題目CSV資料檔   |",
    "  i. 特殊編碼為7777（拒答）、9995（不適用）、9996（跳答）。請注意：9996（跳答/不適用）",
    "  張鑑如（2025）。X（D00266）。https://doi.org/10.6141/TW-SRDA-D00266-1",
    "  Chien-Ju Chang. (2025). KIT (D00266)."
  )
  f <- tempfile(fileext = ".txt")
  writeLines(enc2utf8(txt), f, useBytes = TRUE)
  r <- import_usage_doc(f)
  expect_equal(r$adminID, "M03M048")
  expect_equal(r$waveNo, 7L)
  expect_equal(c(r$surveyFrom, r$surveyTo), c("2020-03-01", "2021-07-31"))
  expect_equal(r$release, "D00266")
  expect_equal(r$doi, "https://doi.org/10.6141/TW-SRDA-D00266-1")
  expect_equal(r$data_files$file, c("data1", "data3", "IRT"))
  expect_equal(r$missing_codes$code, c("7777", "9995", "9996"))
  expect_match(r$citation_en, "^Chien-Ju Chang")
})
