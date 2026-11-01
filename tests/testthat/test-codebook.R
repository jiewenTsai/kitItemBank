# codebook.R：施測的 codebook（小的 kit 寫在這裡；最後用內建 kit 跑一次）

cb_tiny_kit <- function() {
  k <- empty_kit()
  t <- function(tb, ...) conform_kit_table(data.frame(..., stringsAsFactors = FALSE), tb)
  k$cohorts <- t("cohorts", cohortID = c("K2016", "K2013"), code = c("M03", "M36"))
  k$admins <- t("admins",
    adminID = c("M03M036", "M36M084G01", "M36M096G02"),
    cohortID = c("K2016", "K2013", "K2013"), ageMonth = c("36", "84", "96"),
    grade = c("", "1", "2"),
    surveyFrom = c("2019-03-01", "2020-02-01", ""), surveyTo = c("2020-08-31", "2021-08-30", ""),
    status = c("released", "released", "internal"), datasetRelease = c("D00229", "D00250", ""),
    citation_zh = c("張鑑如（2024）。36月齡。", "", ""))
  k$forms <- t("forms", formID = "P-36-72", instrument = "P", label = "家長 36–72 月齡")
  k$datasets <- t("datasets",
    datasetID = c("M03M036-P", "M03M036-PK", "M36M084G01-P"),
    adminID = c("M03M036", "M03M036", "M36M084G01"), file = c("data1", "data2", "data1"),
    instrument = "P", formID = c("P-36-72", "P-36-72", ""),
    respondent = c("家長", "親友", "家長"), missingScheme = c("2016", "2016", "2020"))
  k$items <- t("items", item_uid = c("P.cogc01", "P.sleep01", "P.quote01"), instrument = "P",
               varname = c("cogc01", "sleep01", "quote01"),
               block = c("認知問卷資料", "健康", "健康"))
  k$item_versions <- t("item_versions",
    item_uid = c("P.cogc01", "P.sleep01", "P.quote01"), itemVer = "1",
    label = c("孩子能玩扮演遊戲。", "孩子幾點睡？\n（平日）", "孩子說 'no' 的次數"),
    valueSetID = c("vs1", "", "vs2"))
  k$value_codes <- t("value_codes", valueSetID = c("vs1", "vs1", "vs2"), code = c("2", "1", "1"),
                     label = c("會", "不會", "媽媽說 'yes'"), order = c("2", "1", "1"))
  k$missing_schemes <- t("missing_schemes",
    scheme = c("2016", "2016", "2016", "2016", "2020"),
    code = c("9999", "7777", "8888", "9996", "9999"),
    label = c("遺漏值", "拒答", "不知道", "跳答或不適用", "遺漏值"))
  k$dataset_vars <- t("dataset_vars",
    datasetID = c("M03M036-P", "M03M036-P", "M03M036-P", "M03M036-P", "M03M036-PK",
                  "M36M084G01-P"),
    varname = c("sleep01", "baby_sex", "cogc01", "quote01", "cogc01", "cogc01"),
    item_uid = c("P.sleep01", "", "P.cogc01", "P.quote01", "P.cogc01", "P.cogc01"),
    itemVer = c("1", "", "1", "1", "1", "1"),
    kind = c("item", "system", "item", "item", "item", "item"),
    position = c("二.1", "", "一.1", "二.2", "一.1", ""),
    order = c("3", "1", "2", "4", "1", ""),
    public = c("", "", "", "0", "", ""))
  k$groups <- t("groups", groupID = c("P.mem", "P.cog"), label = c("記憶能力", "認知"))
  k$group_members <- t("group_members", formID = "P-36-72", groupID = c("P.mem", "P.cog"),
                       item_uid = "P.cogc01", scheme = "official")
  k$block_status <- t("block_status", datasetID = c("M03M036-PK", "M03M036-PK"),
                      block = c("健康", "認知問卷資料"), asked = c("none", "subset"))
  k$aliases <- t("aliases",
    alias = c("M036小", "36M", "36M", "M36W84", "D00250", "84M"),
    target_type = "admin",
    target_id = c("M03M036", "M03M036", "M36M036", "M36M084G01", "M36M084G01", "M36M084G01"))
  k
}

test_that("resolve_admin() finds admins by ID and alias, ignoring case", {
  k <- cb_tiny_kit()
  expect_identical(resolve_admin(k, "M03M036"), "M03M036")
  expect_identical(resolve_admin(k, "m03m036"), "M03M036")
  expect_identical(resolve_admin(k, " M036小 "), "M03M036")
  expect_identical(resolve_admin(k, "m36w84"), "M36M084G01")
  expect_identical(resolve_admin(k, "D00250"), "M36M084G01")
  expect_identical(resolve_admin(k, "84m"), "M36M084G01")
  # 別名指向題庫裡沒有的施測：不算
  expect_identical(resolve_admin(k, "36M"), "M03M036")
  expect_identical(resolve_admin(k, c("84M", "M036小")), c("M03M036", "M36M084G01"))
  expect_identical(resolve_admin(k, "nope"), character(0))
  expect_identical(resolve_admin(k, ""), character(0))
  expect_identical(resolve_admin(k, NULL), character(0))
})

test_that("admin_summary() has one row per admin with Chinese status and counts", {
  s <- admin_summary(cb_tiny_kit())
  expect_identical(s$adminID, c("M03M036", "M36M084G01", "M36M096G02"))
  expect_identical(s$cohort, c("M03", "M36", "M36"))
  expect_identical(s$status_label, c("已釋出", "已釋出", "內部"))
  expect_identical(s$period, c("2019.03–2020.08", "2020.02–2021.08", ""))
  expect_identical(s$n_datasets, c(2L, 1L, 0L))
  expect_identical(s$n_item_vars, c(4L, 1L, 0L))
  expect_identical(s$n_items, c(3L, 1L, 0L))
  expect_identical(s$label, c("M03 世代 36 月齡（2019.03–2020.08）",
                              "M36 世代 84 月齡・小一（2020.02–2021.08）",
                              "M36 世代 96 月齡・小二（內部）"))
})

test_that("admin_codebook() lists variables in file order with labels, groups, codes", {
  k <- cb_tiny_kit()
  cb <- admin_codebook(k, "M03M036")
  v <- cb$vars
  expect_identical(v$datasetID, c(rep("M03M036-P", 4), "M03M036-PK"))
  expect_identical(v$varname, c("baby_sex", "cogc01", "sleep01", "quote01", "cogc01"))
  expect_identical(v$order, c(1L, 2L, 3L, 4L, 1L))
  expect_identical(v$label[1:3], c("", "孩子能玩扮演遊戲。", "孩子幾點睡？\n（平日）"))
  expect_identical(v$block[1:3], c("", "認知問卷資料", "健康"))
  expect_identical(v$groups[1:3], c("", "記憶能力、認知", ""))
  expect_identical(v$values[2], "1=不會; 2=會")
  expect_identical(v$values[3], "")
  expect_identical(v$itemID[2], "cogc01")
  expect_identical(v$respondent, c(rep("家長", 4), "親友"))
  expect_identical(v$missing[1], "7777=拒答; 8888=不知道; 9996=跳答或不適用; 9999=遺漏值")
  expect_identical(v$public, c(TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_identical(cb$datasets$dataset_label, c("家長（data1）", "親友（data2）"))
  expect_identical(cb$block_status$asked_label, c("沒問", "只問部分題目"))
  expect_identical(admin_block_sentences(cb),
                   c("親友版沒問：健康", "親友版只問部分題目：認知問卷資料"))

  # 只要一個資料檔；沒有順序的資料檔照 kit 的順序
  expect_identical(admin_codebook(k, "M03M036", "M03M036-PK")$vars$varname, "cogc01")
  v2 <- admin_codebook(k, "M36M084G01")$vars
  expect_true(is.na(v2$order))
  expect_identical(v2$groups, "")  # 沒有題本 → 沒有題組
  expect_identical(nrow(admin_codebook(k, "M36M096G02")$vars), 0L)
  expect_error(admin_codebook(k, "M036小"), "resolve_admin")
})

test_that("codebook_workbook() starts with 說明 and has one sheet per data file", {
  k <- cb_tiny_kit()
  wb <- codebook_workbook(k, "M03M036", version = "2026.1 草稿", date = as.Date("2026-11-01"))
  expect_identical(names(wb), c("說明", "M03M036-P 家長", "M03M036-PK 親友"))
  info <- wb[[1]]
  expect_identical(names(info), c("項目", "內容"))
  get <- function(k) info$內容[info$項目 == k]
  expect_identical(get("題庫版本"), "2026.1 草稿")
  expect_identical(get("產生日期"), "2026-11-01")
  expect_match(get("施測"), "M03 世代 36 月齡（2019.03–2020.08），代號 M03M036")
  expect_identical(get("調查期間"), "2019 年 3 月到 2020 年 8 月")
  expect_match(get("狀態"), "已釋出，釋出編號 D00229")
  expect_match(get("資料檔")[1], "4 個變項（其中 1 個公開檔沒有）")
  expect_identical(get("遺漏代碼（方案 2016）"), "7777=拒答; 8888=不知道; 9996=跳答或不適用; 9999=遺漏值")
  expect_identical(get("區塊狀態"), c("親友版沒問：健康", "親友版只問部分題目：認知問卷資料"))
  expect_identical(get("建議引用"), "張鑑如（2024）。36月齡。")
  s <- wb[[2]]
  expect_identical(names(s)[1:4], c("順序", "題本位置", "變項名稱", "題目"))
  expect_identical(s$變項名稱, c("baby_sex", "cogc01", "sleep01", "quote01"))
  expect_identical(s$題目[3], "孩子幾點睡？ （平日）")
  expect_identical(s$公開檔, c("有", "有", "有", "沒有"))
  expect_identical(s$種類[1:2], c("系統", "題目"))

  f <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(wb, f)
  expect_identical(readxl::excel_sheets(f), names(wb))

  # 工作表名稱：不合法的字換掉，最多 31 字，重複的加編號
  expect_identical(cb_sheet_name("a/b:c", "x"), "a_b_c")
  expect_identical(nchar(cb_sheet_name(strrep("x", 40), "")), 31L)
  expect_identical(cb_sheet_name("說明", "說明"), "說明 (2)")
})

test_that("codebook_sps() writes valid SPSS label syntax", {
  k <- cb_tiny_kit()
  sps <- codebook_sps(k, "M03M036-P", version = "2026.1", date = as.Date("2026-11-01"))
  l <- strsplit(sps, "\n", fixed = TRUE)[[1]]
  expect_identical(l[1], "* Encoding: UTF-8.")
  expect_identical(utils::tail(l, 1), "EXECUTE.")
  expect_true(all(nchar(l, "bytes") <= 255L))
  expect_true(validUTF8(sps))
  # 變項標籤：有標籤的才有；公開檔沒有的（quote01）不放
  expect_true("VARIABLE LABELS" %in% l)
  expect_true("   cogc01 '孩子能玩扮演遊戲。'" %in% l)
  expect_true("  /sleep01 '孩子幾點睡？ （平日）'." %in% l)
  expect_false(any(grepl("quote01|baby_sex", l)))
  # 選項標籤：選項加上遺漏代碼，照順序
  i <- match("VALUE LABELS", l)
  expect_identical(l[i + 1:7], c("   cogc01", "    1 '不會'", "    2 '會'", "    7777 '拒答'",
                                 "    8888 '不知道'", "    9996 '跳答或不適用'",
                                 "    9999 '遺漏值'."))
  # 遺漏值：4 個代碼 → 一個範圍加一個值
  j <- match("MISSING VALUES", l)
  expect_identical(l[j + 1:2], c("  cogc01", "  (8888 THRU 9999, 7777)."))

  # 引號：' 變成 ''
  k$dataset_vars$public <- ""
  sps2 <- codebook_sps(k, "M03M036-P")
  expect_match(sps2, "/quote01 '孩子說 ''no'' 的次數'", fixed = TRUE)
  expect_match(sps2, "1 '媽媽說 ''yes'''", fixed = TRUE)

  # 很長的標籤：切成 255 bytes 內，用 + 接起來
  k$item_versions$label[1] <- strrep("很長的題目'", 40)
  l3 <- strsplit(codebook_sps(k, "M03M036-P"), "\n", fixed = TRUE)[[1]]
  expect_true(all(nchar(l3, "bytes") <= 255L))
  expect_true(any(grepl("^     \\+ '", l3)))

  expect_error(codebook_sps(k, "nope"), "Unknown datasetID")

  f <- withr::local_tempfile(fileext = ".sps")
  codebook_write_sps(sps, f)
  raw <- readBin(f, "raw", file.size(f))
  expect_identical(raw[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))
})

test_that("missing-value specs fit SPSS's limits", {
  expect_identical(cb_sps_missing_spec(c(7777, 8888, 9999))$text, "(7777, 8888, 9999)")
  expect_identical(cb_sps_missing_spec(c(7777, 8888, 9995, 9996, 9999))$text,
                   "(8888 THRU 9999, 7777)")
  expect_identical(cb_sps_missing_spec(c(-9, -8, -7, 99))$text, "(-9 THRU -7, 99)")
})

test_that("the built-in kit gives a codebook for every admin", {
  k <- read_kit()
  s <- admin_summary(k)
  expect_identical(nrow(s), nrow(k$admins))
  expect_true(all(s$status_label %in% c("已釋出", "內部", "規劃中")))
  expect_identical(resolve_admin(k, "M036大"), "M36M036")
  expect_identical(resolve_admin(k, "M36W84"), "M36M084G01")
  expect_identical(resolve_admin(k, "D00250"), "M36M084G01")
  expect_setequal(resolve_admin(k, "84M"), c("M03M084G01", "M36M084G01"))
  cb <- admin_codebook(k, "M03M036")
  expect_identical(nrow(cb$vars), sum(k$dataset_vars$datasetID %in% cb$datasets$datasetID))
  p <- cb$vars[cb$vars$datasetID == "M03M036-P" & !is.na(cb$vars$order), ]
  expect_false(is.unsorted(p$order))
  expect_true(any(grepl("^親友版沒問：", admin_block_sentences(cb))))
  wb <- codebook_workbook(k, "M03M036")
  expect_identical(names(wb)[1], "說明")
  expect_length(wb, nrow(cb$datasets) + 1L)
  for (d in k$datasets$datasetID[k$datasets$adminID %in% c("M03M036", "M36M084G01")]) {
    l <- strsplit(codebook_sps(k, d), "\n", fixed = TRUE)[[1]]
    expect_true(all(nchar(l, "bytes") <= 255L), info = d)
    expect_identical(utils::tail(l, 1), "EXECUTE.")
  }
})
