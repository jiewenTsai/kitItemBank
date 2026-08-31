# tests/testthat/test-dim-report.R
# 構面報告整合測試：對應原 tests/test_dim_meta_preview.R

# ── 輔助：組裝報告資料 ──────────────────────────────────────────
.build_report_data <- function(d, wave, meta_edit_df = NULL) {
  iw <- d$itemWave
  id <- d$itemDim
  ib <- d$itemBank
  dm <- ensure_dim_meta_cols(d$dimMeta)
  dr <- d$dimReliability

  wave_items <- iw[iw[[wave]] == 1L, c("itemID", "itemVer"), drop = FALSE]
  dim_cols   <- get_matrix_cols(id)

  Filter(Negate(is.null), lapply(dim_cols, function(dim_col) {
    dim_items     <- id[id[[dim_col]] == 1L, c("itemID", "itemVer"), drop = FALSE]
    items_in_both <- dplyr::inner_join(wave_items, dim_items, by = c("itemID", "itemVer"))
    if (nrow(items_in_both) == 0L) return(NULL)
    item_details  <- dplyr::inner_join(items_in_both, ib, by = c("itemID", "itemVer"))
    item_details$label <- collapse_newlines(item_details$label)
    items_out <- pick_unique_report_items(item_details, wave_items)
    resolved  <- resolve_dim_meta(dim_col, wave, dm, dr, meta_edit_df)
    list(
      dimID       = dim_col,
      dimName     = resolved$dimName,
      dimNameRaw  = resolved$dimNameRaw,
      alias       = resolved$alias,
      reliability = resolved$reliability,
      items       = items_out
    )
  }))
}

# ── 單元測試：format_dim_report_heading ─────────────────────────
test_that("format_dim_report_heading 有別名時加括號、無別名時只顯示原名", {
  expect_identical(
    format_dim_report_heading("學習輔助", "學習輔助AB"),
    "學習輔助（學習輔助AB）"
  )
  expect_identical(format_dim_report_heading("學習輔助", ""), "學習輔助")
  expect_identical(format_dim_report_heading("學習輔助"),     "學習輔助")
  # 別名與原名相同時不重複顯示
  expect_identical(
    format_dim_report_heading("學習輔助", "學習輔助"),
    "學習輔助"
  )
})

# ── 單元測試：resolve_dim_meta 的 meta_edit_df 覆寫 ─────────────
test_that("resolve_dim_meta 正確套用 meta_edit_df 的別名與信度", {
  d      <- .load_domain("parent")
  dim_id <- "學習輔助"
  wave   <- .find_wave_with_dim(d, dim_id)
  skip_if(is.null(wave), paste("找不到含", dim_id, "題項的波次"))

  me <- data.frame(
    dimID       = d$dimMeta$dimID,
    dimName     = as.character(d$dimMeta$dimName),
    alias       = vapply(d$dimMeta$alias,  clean_meta_str, character(1L)),
    reliability = vapply(d$dimMeta$dimID, function(did) {
      v <- get_dim_reliability(did, wave, d$dimMeta, d$dimReliability)
      if (is.na(v)) "" else as.character(v)
    }, character(1L)),
    note        = vapply(d$dimMeta$note,   clean_meta_str, character(1L)),
    stringsAsFactors = FALSE
  )
  idx              <- match(dim_id, me$dimID)
  me$alias[idx]        <- "學習輔助AB"
  me$reliability[idx]  <- "0.88"

  resolved <- resolve_dim_meta(dim_id, wave, d$dimMeta, d$dimReliability, me)

  expect_identical(
    format_dim_report_heading(resolved$dimNameRaw, resolved$alias),
    "學習輔助（學習輔助AB）"
  )
  expect_equal(resolved$reliability, 0.88, tolerance = 1e-6)
})

# ── 單元測試：pick_unique_report_items 欄位結構 ─────────────────
test_that("pick_unique_report_items 只回傳 itemID 和 label 兩欄，且無重複 itemID", {
  d      <- .load_domain("parent")
  dim_id <- "學習輔助"
  wave   <- .find_wave_with_dim(d, dim_id)
  skip_if(is.null(wave), paste("找不到含", dim_id, "題項的波次"))

  iw <- d$itemWave
  id <- d$itemDim
  ib <- d$itemBank

  wave_items    <- iw[iw[[wave]]   == 1L, c("itemID", "itemVer"), drop = FALSE]
  dim_items     <- id[id[[dim_id]] == 1L, c("itemID", "itemVer"), drop = FALSE]
  items_in_both <- dplyr::inner_join(wave_items, dim_items, by = c("itemID", "itemVer"))

  skip_if(nrow(items_in_both) == 0L,
          paste("No items in wave ×", dim_id, "intersection for wave:", wave))

  item_details        <- dplyr::inner_join(items_in_both, ib, by = c("itemID", "itemVer"))
  item_details$label  <- collapse_newlines(item_details$label)
  result              <- pick_unique_report_items(item_details, wave_items)

  expect_true(all(c("itemID", "label") == names(result)))
  expect_false(any(c("itemVer", "block") %in% names(result)))
  expect_gte(nrow(result), 1L)
  expect_false(anyDuplicated(result$itemID) > 0L)
})

# ── 整合測試：meta 覆寫 → 組裝報告資料 → Word 匯出 ─────────────
test_that("構面報告整合：覆寫 meta → 組裝資料 → 產出 Word 檔", {
  d      <- .load_domain("parent")
  dim_id <- "學習輔助"
  wave   <- .find_wave_with_dim(d, dim_id)
  skip_if(is.null(wave), paste("找不到含", dim_id, "題項的波次"))

  me <- data.frame(
    dimID       = d$dimMeta$dimID,
    dimName     = as.character(d$dimMeta$dimName),
    alias       = vapply(d$dimMeta$alias,  clean_meta_str, character(1L)),
    reliability = vapply(d$dimMeta$dimID, function(did) {
      v <- get_dim_reliability(did, wave, d$dimMeta, d$dimReliability)
      if (is.na(v)) "" else as.character(v)
    }, character(1L)),
    note        = vapply(d$dimMeta$note,   clean_meta_str, character(1L)),
    stringsAsFactors = FALSE
  )
  idx             <- match(dim_id, me$dimID)
  me$alias[idx]       <- "學習輔助AB"
  me$reliability[idx] <- "0.88"

  rd  <- .build_report_data(d, wave, me)
  hit <- Filter(function(x) x$dimID == dim_id, rd)
  expect_length(hit, 1L)
  hit <- hit[[1L]]

  # 標題格式
  expect_identical(
    format_dim_report_heading(hit$dimNameRaw, hit$alias),
    "學習輔助（學習輔助AB）"
  )
  # 報告項目欄位
  expect_true(all(c("itemID", "label") == names(hit$items)))
  expect_false(any(c("itemVer", "block") %in% names(hit$items)))
  # 信度覆寫
  expect_equal(hit$reliability, 0.88, tolerance = 1e-6)

  # Word 輸出
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  generate_dim_report_docx(list(domain = "parent", wave = wave, dims = rd), out)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 5000L)
})
