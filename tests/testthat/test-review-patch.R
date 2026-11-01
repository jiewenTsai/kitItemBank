# review_patch.R：審核修改包（改前改後、檢查、影響範圍、改動紀錄、版本）

# 一題還不在「女方」題組的題目（照題庫現在的內容挑，不寫死）
review_test_item <- function() {
  dm <- bank_to_v02(kit_builtin(), "parent")$itemDim
  setdiff(dm$itemID, dm$itemID[dm[["女方"]] == 1L])[1]
}

review_test_package <- function(meta = list()) {
  kit <- kit_builtin()
  old <- bank_to_v02(kit, "parent")
  new <- old
  i <- which(new$itemDim$itemID == review_test_item())
  new$itemDim[i, "女方"] <- 1L
  new$itemBank$label[new$itemBank$itemID == "fatherinvo01n"] <- "y"
  res <- kit_patch(kit, old, new, "parent")
  f <- withr::local_tempfile(fileext = ".xlsx", .local_envir = parent.frame())
  writexl::write_xlsx(patch_workbook(res, bank_diff(old, new), "parent", "2026.1", meta = meta), f)
  f
}

test_that("bump_bank_version adds one in the same year, restarts in a new year", {
  expect_identical(bump_bank_version("2026.1", as.Date("2026-11-02")), "2026.2")
  expect_identical(bump_bank_version("2026.9", as.Date("2026-12-31")), "2026.10")
  expect_identical(bump_bank_version("2026.10", as.Date("2026-12-31")), "2026.11")
  expect_identical(bump_bank_version("2026.3", as.Date("2027-01-01")), "2027.1")
  expect_identical(bump_bank_version(" 2026.1 ", as.Date("2026-05-01")), "2026.2")
  # 時鐘比題庫版本還早：不倒退
  expect_identical(bump_bank_version("2027.2", as.Date("2026-05-01")), "2027.3")
  expect_identical(bump_bank_version("", as.Date("2026-05-01")), "2026.1")
  expect_identical(bump_bank_version(NA, as.Date("2026-05-01")), "2026.1")
  expect_identical(bump_bank_version(NULL, as.Date("2026-05-01")), "2026.1")
  expect_identical(bump_bank_version("v2", as.Date("2026-05-01")), "2026.1")
})

test_that("read_review_package reads old packages and the optional fields", {
  f <- review_test_package()
  pk <- read_review_package(f)
  expect_identical(pk$base_version, "2026.1")
  expect_identical(pk$meta, list(reason = "", decided_by = "", submitted_by = ""))
  expect_identical(nrow(pk$row_meta), nrow(pk$patch))

  f2 <- review_test_package(list(reason = "會議決議", submitted_by = "小明", decided_by = ""))
  pk2 <- read_review_package(f2)
  expect_identical(pk2$meta$reason, "會議決議")
  expect_identical(pk2$meta$submitted_by, "小明")
  expect_identical(pk2$meta$decided_by, "")
  # 舊的讀法照樣讀得到
  expect_equal(read_patch_xlsx(f2)$patch, pk2$patch)

  bad <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(a = data.frame(x = 1)), bad)
  expect_error(read_review_package(bad), "不是修改包")
})

test_that("review_patch shows before/after, checks, impact; nothing blocks a clean package", {
  pk <- read_review_package(review_test_package())
  rv <- review_patch(kit_builtin(), pk$patch, pk$base_version, "2026.1")
  expect_true(rv$version_ok)
  expect_length(rv$blocking, 0L)
  expect_true(all(rv$patch$ok))
  expect_identical(sum(rv$issues$new), 0L)
  ch <- rv$changes
  expect_setequal(ch$kind, c("新增", "修改"))
  m <- ch[ch$kind == "修改", ]
  expect_match(m$before, "^名稱：孩子父親")
  expect_identical(m$after, "名稱：y")
  expect_match(ch$after[ch$kind == "新增"][1], "題組：P.女方")
  expect_true(all(c(paste0("P.", review_test_item()), "P.fatherinvo01n") %in% rv$impact$items))
  expect_identical(rv$impact$groups, "P.女方")
  expect_gt(length(rv$impact$datasets), 0L)
  expect_gt(length(rv$impact$admins), 0L)
  expect_true(all(rv$impact$admins %in% kit_builtin()$admins$adminID))
  tb <- review_change_table(ch)
  expect_identical(names(tb), c("動作", "表", "哪一列", "改前", "改後"))
  expect_true("題目的版本" %in% tb$表)
  expect_identical(review_impact_table(rv$impact)$影響到, c("題目", "題組", "題本", "資料檔", "施測"))
})

test_that("review_patch warns about version and conflicts, blocks rows that do not apply", {
  kit <- kit_builtin()
  g <- kit$groups$groupID[1]
  p <- data.frame(
    op = c("set", "delete", "set", "add"),
    table = c("groups", "groups", "nope", "groups"),
    key = c(g, "P.沒有這個", "x", "P.新題組"),
    column = c("label", "", "label", ""),
    from = c("舊的名稱", "", "", ""), to = c("新的名稱", "", "", ""),
    stringsAsFactors = FALSE
  )
  rv <- review_patch(kit, p, "2025.9", "2026.1")
  expect_false(rv$version_ok)
  expect_identical(rv$patch$ok, c(TRUE, FALSE, FALSE, TRUE))
  expect_match(rv$patch$problem[2], "找不到這一列")
  expect_match(rv$patch$problem[3], "沒有「nope」")
  expect_match(rv$patch$conflict[1], "修改包記的原本是「舊的名稱」")
  expect_identical(rv$patch$now[1], kit$groups$label[1])
  # 新增的題組沒有名稱（必填）→ 新的結構問題
  expect_true(any(rv$issues$new & rv$issues$check == "required_blank"))
  expect_match(rv$blocking, "2 列改動套不上", all = FALSE)
  expect_match(rv$blocking, "新的結構問題", all = FALSE)
  it <- review_issue_table(rv$issues)
  expect_true("必填的欄是空白" %in% it$問題)
  expect_true("這次改動造成的" %in% it$什麼時候有的)

  expect_false(review_patch(kit, p[0, ], "", "2026.1")$version_ok)
  expect_match(review_patch(kit, p[0, ], "", "2026.1")$blocking, "沒有改動")
})

test_that("change_record has one row per applied change, with people and versions", {
  kit <- kit_builtin()
  g <- kit$groups$groupID[1]
  p <- data.frame(op = c("set", "delete"), table = "groups", key = c(g, "P.沒有這個"),
                  column = c("label", ""), from = c("舊", ""), to = c("新", ""),
                  stringsAsFactors = FALSE)
  rv <- review_patch(kit, p, "2026.1", "2026.1")
  rec <- change_record(rv, list(reason = "原因", decided_by = "會議", submitted_by = "甲",
                                approved_by = "乙"),
                       "2026.2", as.Date("2026-11-02"),
                       row_meta = data.frame(reason = c("這列自己的原因", ""), decided_by = "",
                                             stringsAsFactors = FALSE))
  expect_identical(names(rec), CHANGE_RECORD_COLS)
  expect_identical(nrow(rec), 1L)
  expect_identical(rec$from, kit$groups$label[1])  # 題庫當時真正的值，不是修改包記的
  expect_identical(rec$to, "新")
  expect_identical(rec$reason, "這列自己的原因")
  expect_identical(rec$decided_by, "會議")
  expect_identical(rec$approved_by, "乙")
  expect_identical(rec$bank_version, "2026.2")
  expect_identical(rec$date, "2026-11-02")
})

test_that("review_files and review_zip hold the changed tables, BANK_VERSION and the record", {
  pk <- read_review_package(review_test_package())
  rv <- review_patch(kit_builtin(), pk$patch, pk$base_version, "2026.1")
  rec <- change_record(rv, list(reason = "r", approved_by = "a"), "2026.2")
  fl <- review_files(rv, rec, "2026.2")
  expect_setequal(names(fl), c("inst/extdata/kit/group_members.csv",
                               "inst/extdata/kit/item_versions.csv",
                               "inst/extdata/kit/BANK_VERSION",
                               "inst/extdata/kit/changes/2026.2.csv"))
  expect_identical(rawToChar(fl[["inst/extdata/kit/BANK_VERSION"]]), "2026.2\n")
  back <- readr::read_csv(I(rawToChar(fl[["inst/extdata/kit/changes/2026.2.csv"]])),
                          col_types = readr::cols(.default = "c"), na = character(0))
  expect_identical(nrow(back), nrow(rec))
  # 沒改到的表和原本的檔一模一樣：只寫改到的那幾張
  gm <- rawToChar(fl[["inst/extdata/kit/group_members.csv"]])
  Encoding(gm) <- "UTF-8"
  expect_match(gm, paste0("P.", review_test_item()), fixed = TRUE)

  z <- withr::local_tempfile(fileext = ".zip")
  review_zip(rv, rec, "2026.2", z)
  ls <- zip::zip_list(z)$filename
  ls <- ls[!grepl("/$", ls)]
  # zip 裡只有改到的表、BANK_VERSION、改動紀錄
  expect_setequal(ls, sub("^inst/extdata/", "", names(fl)))
  # 解壓縮到一份題庫的 inst/extdata/ 上（蓋掉同名檔）= 套用後的題庫
  d <- withr::local_tempdir()
  write_kit(kit_builtin(), file.path(d, "kit"))
  zip::unzip(z, exdir = d)
  expect_identical(readLines(file.path(d, "kit", "BANK_VERSION")), "2026.2")
  expect_true(file.exists(file.path(d, "kit", "changes", "2026.2.csv")))
  k2 <- read_kit(file.path(d, "kit"))
  expect_true(any(k2$group_members$item_uid == paste0("P.", review_test_item()) &
                    k2$group_members$groupID == "P.女方"))
  expect_identical(nrow(check_kit(k2)), 0L)
  expect_identical(k2$item_versions$label[k2$item_versions$item_uid == "P.fatherinvo01n" &
                                            k2$item_versions$itemVer == "1"], "y")
})
