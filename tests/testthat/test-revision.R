# revision.R：改版底稿 → 健康檢查 → 修改包（小題庫在 helper-revision.R）

test_that("the template has a locked row id, drop-downs and the three sheets", {
  skip_if_not_installed("openxlsx")
  k <- rv_kit()
  expect_equal(nrow(check_kit(k)), 0L)
  b <- revision_base(k, "P-G1-G3")
  expect_identical(b$dataset$datasetID, "M03M096G02-P")   # 最近一次施測
  expect_identical(b$questions$rowid, sprintf("Q%03d", 1:5))
  expect_identical(revision_base(k, "P-G1-G3", adminID = "M03M084G01")$dataset$datasetID,
                   "M03M084G01-P")
  expect_error(revision_base(k, "P-xx"))

  f <- withr::local_tempfile(fileext = ".xlsx")
  revision_template(k, "P-G1-G3", f, version = "2026.1")
  expect_identical(readxl::excel_sheets(f), c("說明", "題目", "新增題目"))
  wb <- openxlsx::loadWorkbook(f)
  expect_true(all(lengths(lapply(wb$worksheets, function(w) w$sheetProtection)) > 0L))
  xml <- paste(readLines(unz(f, "xl/worksheets/sheet2.xml"), warn = FALSE, encoding = "UTF-8"),
               collapse = "")
  expect_match(xml, "\"不變,改文字,改選項,刪除\"", fixed = TRUE)
  # 黃色欄沒鎖：老師可以打字
  yellow <- Filter(function(s) identical(s$sheet, "題目") && 2L %in% s$rows && 6L %in% s$cols,
                   wb$styleObjects)
  expect_length(yellow, 1L)
  expect_false(yellow[[1]]$style$locked)
  expect_setequal(unique(yellow[[1]]$cols), 6:9)
  q <- readxl::read_excel(f, "題目", col_types = "text")
  expect_identical(names(q), c("列號", "位置", "變項", "目前題目", "目前選項", "動作",
                               "新題目文字", "新選項", "備註", "系統用"))
  expect_identical(q[["目前選項"]][1], "1=有; 2=沒有")
  x <- read_revision_xlsx(f)
  expect_identical(x$info[["version"]], "2026.1")
  expect_identical(x$info[["dataset"]], "M03M096G02-P")
  p <- check_revision_xlsx(f, k, version = "2026.1")
  expect_identical(p$severity, "要注意")   # 只有「沒有任何改動」
  expect_match(p$message, "沒有任何改動")
})

test_that("a filled template becomes a change package that passes check_kit", {
  skip_if_not_installed("openxlsx")
  k <- rv_kit()
  f <- withr::local_tempfile(fileext = ".xlsx")
  g <- withr::local_tempfile(fileext = ".xlsx")
  revision_template(k, "P-G1-G3", f, version = "2026.1")
  new <- data.frame(varname = c("q9", "q10"), label = c("新的題目", "開放題"),
                    values = c("0=否; 1=是", ""), after = c("Q002", "開頭"), block = "",
                    note = "", stringsAsFactors = FALSE)
  rv_fill(f, g, list(
    cell(1, 6, "改文字"), cell(1, 7, "第1題改好的文字"), cell(1, 9, "老師說太長"),
    cell(2, 6, "改選項"), cell(2, 8, "1=有；2=沒有；3=不知道"),
    cell(3, 6, "刪除")
  ), new = new)
  expect_false(any(check_revision_xlsx(g, k, "2026.1")$severity == "擋下來"))

  expect_error(revision_patch(k, g, "M03M096G02", version = "2026.1"), "已經有了")
  res <- revision_patch(k, g, "M03M108G03", version = "2026.1", date = as.Date("2026-11-02"))
  expect_identical(res$datasetID, "M03M108G03-P")
  expect_equal(unname(res$summary[c("kept", "text", "options", "deleted", "new")]),
               c(2, 1, 1, 1, 2))
  k2 <- apply_patch(k, res$patch)
  expect_equal(nrow(check_kit(k2)), 0L)

  ad <- k2$admins[k2$admins$adminID == "M03M108G03", ]
  expect_identical(c(ad$status, ad$ageMonth, ad$grade, ad$waveNo), c("planned", "108", "3", "12"))
  dv <- k2$dataset_vars[k2$dataset_vars$datasetID == "M03M108G03-P", ]
  dv <- dv[order(as.integer(dv$order)), ]
  # 開頭：系統變項後面、第一題前面；q9 在 Q002 後面；q3 刪掉
  expect_identical(dv$varname, c("id", "q10", "q1", "q2", "q9", "q4", "q5"))
  expect_identical(dv$itemVer[dv$varname %in% c("q1", "q2", "q4")], c("2", "2", "1"))
  expect_identical(dv$position[dv$varname %in% c("q4", "q9")], c("", "一.4"))
  # 舊版留著；revised 關係
  expect_true(all(c("P.q1|1", "P.q1|2", "P.q3|1") %in%
                    paste(k2$item_versions$item_uid, k2$item_versions$itemVer, sep = "|")))
  expect_identical(k2$item_versions$label[k2$item_versions$item_uid == "P.q1" &
                                            k2$item_versions$itemVer == "2"], "第1題改好的文字")
  rel <- k2$item_relations
  expect_setequal(paste(rel$from_uid, rel$from_ver, rel$to_ver, rel$kind, rel$effective_admin),
                  c("P.q1 1 2 revised M03M108G03", "P.q2 1 2 revised M03M108G03"))
  expect_match(rel$reason[rel$from_uid == "P.q1"], "老師說太長")
  # 選項：新的開新組；和既有一樣的沿用
  vs_q2 <- k2$item_versions$valueSetID[k2$item_versions$item_uid == "P.q2" &
                                         k2$item_versions$itemVer == "2"]
  expect_identical(vs_q2, "vs0003")
  expect_identical(k2$value_codes$label[k2$value_codes$valueSetID == "vs0003"],
                   c("有", "沒有", "不知道"))
  expect_identical(k2$item_versions$valueSetID[k2$item_versions$item_uid == "P.q9"], "vs0002")
  expect_identical(k2$item_versions$itemType[k2$item_versions$item_uid == "P.q10"], "CR")
  expect_identical(k2$items$deprecated[k2$items$item_uid == "P.q3"], "0")  # 題號不刪

  # 修改包 Excel 讀得回來
  h <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(revision_patch_workbook(res, "2026.1"), h)
  back <- read_patch_xlsx(h)
  expect_equal(back$patch, res$patch)
  expect_identical(back$base_version, "2026.1")
})

test_that("the health check finds what teachers tend to do", {
  skip_if_not_installed("openxlsx")
  k <- rv_kit()
  f <- withr::local_tempfile(fileext = ".xlsx")
  g <- withr::local_tempfile(fileext = ".xlsx")
  revision_template(k, "P-G1-G3", f, version = "2025.9")
  new <- data.frame(varname = c("q1", "q8", ""), label = c("x", "", "y"), values = c("", "", "1有"),
                    after = c("Q001", "Q099", "Q002"), block = "", note = "",
                    stringsAsFactors = FALSE)
  rv_fill(f, g, list(
    cell(1, 7, "只寫了新文字"),                 # 沒選動作
    cell(2, 6, "改文字"),                        # 沒寫新文字
    cell(3, 1, "Q002"),                          # 列號重複
    cell(4, 1, "Q404"),                          # 列號被改
    cell(5, 6, "改選項"), cell(5, 8, "1=是;1=否"), # 選項代碼重複
    cell(6, 1, "Q006"), cell(6, 6, "移位置")      # 加了一列；動作不在選單裡
  ), new = new)
  p <- check_revision_xlsx(g, k, version = "2026.1")
  blk <- p$message[p$severity == "擋下來"]
  note <- p$message[p$severity == "要注意"]
  expect_true(any(grepl("動作」是不變", blk)))
  expect_true(any(grepl("Q006 不是底稿原有的", blk)))
  expect_true(any(grepl("動作「移位置」不是選項之一", blk)))
  expect_true(any(grepl("選項代碼「1」重複了", blk)))
  expect_true(any(grepl("新題目文字」是空的", blk)))
  expect_true(any(grepl("Q002 重複了", blk)))
  expect_true(any(grepl("Q404 不是底稿原有的", blk)))
  expect_true(any(grepl("少了 2 列", blk)))
  expect_true(any(grepl("已經用過，不能重用", blk)))
  expect_true(any(grepl("「題目文字」一定要填", blk)))
  expect_true(any(grepl("Q099」不是", blk)))
  expect_true(any(grepl("看不懂「1有」", blk)))
  expect_true(any(grepl("2025.9", note)))
  expect_true(all(c("severity", "sheet", "row", "column", "message") %in% names(p)))
  expect_error(revision_patch(k, g, "M03M108G03", version = "2026.1"), "要先改好")

  # 刪除又寫了新文字：要注意；說明表不見：擋下來
  revision_template(k, "P-G1-G3", f, version = "2026.1")
  rv_fill(f, g, list(cell(3, 6, "刪除"), cell(3, 7, "x")))
  p <- check_revision_xlsx(g, k, version = "2026.1")
  expect_identical(p$severity, "要注意")
  expect_match(p$message, "不會用到")
  rv_fill(f, g, f = function(wb) openxlsx::removeWorksheet(wb, "說明"))
  expect_match(check_revision_xlsx(g, k)$message, "找不到「說明」表")
})

test_that("the planned adminID follows the existing pattern and is never guessed", {
  k <- rv_kit()
  expect_error(rv_parse_admin(k, ""), "代號")
  expect_error(rv_parse_admin(k, "M03M084G01"), "已經有了")
  expect_error(rv_parse_admin(k, "M03-108"), "寫法不對")
  expect_error(rv_parse_admin(k, "M03M108"), "要加年級")
  expect_error(rv_parse_admin(k, "M03M060G01"), "不加年級")
  expect_error(rv_parse_admin(k, "M99M108G03"), "沒有世代")
  a <- rv_parse_admin(k, "M03M072")
  expect_identical(a$waveNo, "")   # 不是最後一波：不猜波次
})

test_that("a template for a real form passes its own health check", {
  skip_if_not_installed("openxlsx")
  k <- read_kit()
  f <- withr::local_tempfile(fileext = ".xlsx")
  revision_template(k, "P-G1-G3", f)
  p <- check_revision_xlsx(f, k)
  expect_false(any(p$severity == "擋下來"))
})

test_that("a planned admin outside the form's ages gets a note, not a block", {
  k <- rv_kit()
  b <- revision_base(k, "P-G1-G3")
  expect_equal(nrow(rv_admin_notes(k, b, rv_parse_admin(k, "M03M108G03"))), 0L)
  p <- rv_admin_notes(k, b, rv_parse_admin(k, "M03M132G05"))
  expect_identical(p$severity, "要注意")
  expect_match(p$message, "不在題本 P-G1-G3 適用的 84–108 月齡內")
})
