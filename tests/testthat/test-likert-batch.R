# tests/testthat/test-likert-batch.R
# Likert 量尺批次合併與矩陣判斷測試：對應原 tests/test_likert_batch.R

# ── 共用常數 ─────────────────────────────────────────────────────
.SOC_IDS    <- c("socB19", "socC14", "socC19")
.SOC_LABELS <- c("從不如此", "很少如此", "有時如此", "經常如此", "總是如此")
.SOC_CODES  <- c("1", "2", "3", "4", "5")

.FAM_IDS    <- c("famC07", "famC08", "famC09")
.FAM_LABELS <- c("很少", "有時", "經常", "很經常", "拒答")
.FAM_CODES  <- c("1", "2", "3", "4", "5")

# ── 輔助：從 itemBank 取指定 ID 的 keys 清單 ─────────────────────
.make_keys <- function(item_ids) {
  lapply(item_ids, function(id) {
    list(itemID = id, itemVer = 1L, layout = "auto", matrix_group = NULL)
  })
}

.make_rows <- function(ib, item_ids) {
  do.call(rbind, lapply(item_ids, function(id) {
    ib[ib$itemID == id & ib$itemVer == 1L, , drop = FALSE]
  }))
}

# ── group_items_for_render ────────────────────────────────────────
test_that("group_items_for_render 為每個無 stemID 共享的題項建立獨立群組", {
  ib   <- .item_bank("parent")
  keys <- .make_keys(.SOC_IDS)
  rows <- .make_rows(ib, .SOC_IDS)

  skip_if(nrow(rows) < length(.SOC_IDS),
          paste("parent itemBank 缺少以下題項：", paste(.SOC_IDS, collapse = ", ")))

  groups <- group_items_for_render(rows, keys)

  # 3 個無共同 stemID 的題項 → 3 個群組
  expect_length(groups, 3L)

  # 每個群組恰好有 1 個題項，layout 為 "auto"
  for (g in groups) {
    expect_equal(nrow(g$items), 1L)
    expect_identical(g$layout, "auto")
  }
})

# ── batch_consecutive_likert_groups（soc 題） ────────────────────
test_that("batch_consecutive_likert_groups 合併相同量尺的 soc Likert 題", {
  ib      <- .item_bank("parent")
  keys    <- .make_keys(.SOC_IDS)
  rows    <- .make_rows(ib, .SOC_IDS)

  skip_if(nrow(rows) < length(.SOC_IDS),
          paste("parent itemBank 缺少以下題項：", paste(.SOC_IDS, collapse = ", ")))

  groups  <- group_items_for_render(rows, keys)
  batched <- batch_consecutive_likert_groups(groups, .SOC_LABELS, .SOC_CODES)

  # 合併後群組數 ≤ 合併前（只減不增）
  expect_lte(length(batched), length(groups))
  expect_gte(length(batched), 1L)

  # 任何含多題的群組必須符合矩陣排版條件（全為 Likert）
  for (grp in batched) {
    if (nrow(grp$items) > 1L) {
      expect_true(
        should_use_matrix_layout(grp),
        label = paste("multi-item group should be matrix layout")
      )
    }
  }
})

# ── likert_batch_key ─────────────────────────────────────────────
test_that("likert_batch_key 對每個 soc 題回傳非空字串", {
  ib <- .item_bank("parent")

  for (id in .SOC_IDS) {
    row <- ib[ib$itemID == id & ib$itemVer == 1L, , drop = FALSE]
    skip_if(nrow(row) == 0L, paste(id, "不在 parent itemBank 中"))
    row <- row[1L, , drop = FALSE]

    key <- likert_batch_key(row, .SOC_LABELS, .SOC_CODES)
    expect_true(nzchar(key), label = paste(id, "batch key should be non-empty"))
    expect_type(key, "character")
  }
})

test_that("socB19（value 僅有說明文字）的 batch key 來自區塊量尺設定", {
  ib  <- .item_bank("parent")
  row <- ib[ib$itemID == "socB19" & ib$itemVer == 1L, , drop = FALSE]
  skip_if(nrow(row) == 0L, "socB19 不在 parent itemBank 中")
  row <- row[1L, , drop = FALSE]

  # value 無法解析成選項清單 → fingerprint 為空 → 改用區塊量尺
  fp <- scale_fingerprint(row)
  if (!nzchar(fp)) {
    # 此情況下 batch key 應反映區塊量尺
    key <- likert_batch_key(row, .SOC_LABELS, .SOC_CODES)
    expected_key <- paste(
      paste0(.SOC_CODES, ":", .SOC_LABELS),
      collapse = "|"
    )
    expect_identical(key, expected_key)
  } else {
    # 若 value 已可解析（未來資料更新），batch key 使用 value 指紋
    key <- likert_batch_key(row, .SOC_LABELS, .SOC_CODES)
    expect_true(nzchar(key))
  }
})

# ── fam 題批次合併 ────────────────────────────────────────────────
test_that("famC07/08/09 以相同量尺批次後不超過原群組數且結構正確", {
  ib   <- .item_bank("parent")
  keys <- .make_keys(.FAM_IDS)
  rows <- .make_rows(ib, .FAM_IDS)

  skip_if(nrow(rows) == 0L,
          paste("parent itemBank 缺少以下題項：", paste(.FAM_IDS, collapse = ", ")))

  fam_groups  <- group_items_for_render(rows, keys)
  fam_batched <- batch_consecutive_likert_groups(fam_groups, .FAM_LABELS, .FAM_CODES)

  expect_lte(length(fam_batched), length(fam_groups))
  expect_gte(length(fam_batched), 1L)
  expect_gte(nrow(fam_batched[[1L]]$items), 1L)

  for (grp in fam_batched) {
    if (nrow(grp$items) > 1L) {
      expect_true(
        should_use_matrix_layout(grp),
        label = "fam multi-item group should be matrix layout"
      )
    }
  }
})
