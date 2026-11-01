# consult.R：請教單（做 Excel → 讀回 → 修改包）

consult_fill <- function(f, fill) {
  wb <- openxlsx::loadWorkbook(f)
  d <- openxlsx::read.xlsx(wb, "請教單", check.names = FALSE)
  for (k in names(fill)) {
    i <- which(startsWith(d[["系統用"]], k))[1]
    v <- fill[[k]]
    openxlsx::writeData(wb, "請教單", t(v), startRow = i + 1L,
                        startCol = match("決定", names(d)), colNames = FALSE)
  }
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
}

test_that("consult_rows finds the questions for teachers", {
  kit <- read_kit()
  r <- consult_rows(kit)
  expect_true(all(r$qtype %in% names(CONSULT_TYPES)))
  expect_true(all(c("renamed", "single", "relation", "block") %in% r$qtype))
  expect_false(any(duplicated(paste(r$qtype, r$key))))
  rel <- kit$item_relations
  k <- r$key[r$qtype == "relation"]
  expect_true(all(k %in% kit_key_str(rel, KIT_SCHEMA$item_relations$key)))
  expect_equal(nrow(consult_rows(kit, include = "blocks")), sum(kit$block_status$decided_by == "auto"))
  expect_equal(nrow(consult_rows(empty_kit())), 0L)
  # 選項不能有半形逗號（Excel 下拉選單用逗號分開），選項和處理一樣多
  for (ty in CONSULT_TYPES) {
    expect_length(ty$actions, length(ty$choices))
    expect_false(any(grepl(",", ty$choices, fixed = TRUE)))
    expect_lt(nchar(paste(ty$choices, collapse = ",")), 250)
  }
})

test_that("a filled-in consultation workbook becomes a patch that applies cleanly", {
  skip_if_not_installed("openxlsx")
  kit <- read_kit()
  rows <- consult_rows(kit)
  f <- withr::local_tempfile(fileext = ".xlsx")
  consult_workbook(kit, rows, f)
  expect_identical(readxl::excel_sheets(f), c("說明", "請教單"))
  d <- readxl::read_excel(f, "請教單")
  expect_identical(names(d), unname(CONSULT_COLS))
  expect_equal(nrow(d), nrow(rows))
  expect_match(d$編號[1], "^Q0001$")
  expect_equal(nrow(read_consult(f)), 0L)  # 還沒填

  rn <- rows$key[rows$qtype == "renamed"][1]          # P.caregiverf02|2|P.caregiverf02n|1
  rl <- rows$key[rows$qtype == "relation"][1:3]
  bl <- rows$key[rows$qtype == "block"][1:2]
  sg <- rows$key[rows$qtype == "single"][1:2]
  fill <- list()
  fill[[paste0("renamed|", rn)]] <- c("接上舊題的題組", "題本上是同一題", "陳老師")
  fill[[paste0("relation|", rl[1])]] <- c("不能比較", "選項改了", "")
  fill[[paste0("relation|", rl[2])]] <- c("不是同一題，刪掉這個關係", "", "林老師")
  fill[[paste0("relation|", rl[3])]] <- c("再討論", "", "")
  fill[[paste0("block|", bl[1])]] <- c("對", "", "")
  fill[[paste0("block|", bl[2])]] <- c("隨便寫的", "", "")
  fill[[paste0("single|", sg[1])]] <- c("把這題移出題組", "", "")
  fill[[paste0("single|", sg[2])]] <- c("保留", "本來就是單題量表", "")
  consult_fill(f, fill)

  ans <- read_consult(f)
  expect_equal(nrow(ans), 8L)
  expect_identical(ans$action[ans$key == rn], "join_groups")
  expect_identical(ans$action[ans$key == bl[2]], "")

  res <- consult_patch(kit, ans, decided_by = "王老師", decided_on = as.Date("2026-11-02"))
  p <- res$patch
  expect_identical(names(p), PATCH_COLS)
  # 新題接上舊題的題組
  expect_true(any(p$op == "add" & p$table == "group_members" & grepl("\\|P\\.caregiverf02n\\|", p$key)))
  # 決定寫進題目關係
  expect_true(any(p$table == "item_relations" & p$key == rn & p$column == "decided_by" & p$to == "陳老師"))
  expect_true(any(p$table == "item_relations" & p$key == rn & p$column == "decided_on" & p$to == "2026-11-02"))
  expect_true(any(p$key == rl[1] & p$column == "comparable" & p$to == "no"))
  expect_true(any(p$key == rl[1] & p$column == "decided_by" & p$to == "王老師"))
  expect_true(any(p$key == rl[1] & p$column == "reason" & p$to == "選項改了"))
  expect_true(any(p$op == "delete" & p$key == rl[2]))
  expect_false(any(p$key == rl[3]))
  expect_true(any(p$table == "block_status" & p$key == bl[1] & p$column == "sourceID" &
                    p$to == "CONSULT-20261102"))
  expect_true(any(p$op == "add" & p$table == "sources" & p$key == "CONSULT-20261102"))
  expect_true(any(p$op == "delete" & p$table == "group_members" & startsWith(p$key, sg[1])))
  # 寫不進題庫的回覆
  expect_setequal(res$notes$key, c(rl[3], bl[2], sg[2]))

  k2 <- apply_patch(kit, p)
  expect_equal(nrow(check_kit(k2)), 0L)
  expect_true(any(k2$group_members$item_uid == "P.caregiverf02n"))
  # 接上之後，這一筆要注意就不見了
  expect_false(rn %in% audit_kit(k2)$key)

  # 下載的修改包讀得回來
  x <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(consult_patch_sheets(res), x)
  expect_equal(read_patch_xlsx(x)$patch, p)

  # 不給 source：不動 sourceID，也不新增 sources
  res2 <- consult_patch(kit, ans, source = NULL)
  expect_false(any(res2$patch$column == "sourceID" | res2$patch$table == "sources"))
})

test_that("read_consult refuses a workbook that is not a consultation form", {
  f <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list("請教單" = data.frame(a = 1)), f)
  expect_error(read_consult(f), "請教單")
})
