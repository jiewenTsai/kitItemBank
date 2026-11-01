# tests/testthat/test-edit.R：題庫編修的計算（R/edit.R）

.keys <- function(df, i) df[i, c("itemID", "itemVer")]

test_that("set_assignment 勾選、取消，bank_diff 記下來", {
  b <- .bank()
  dim <- get_matrix_cols(b$itemDim)[1]
  off <- which(b$itemDim[[dim]] == 0L)[1:2]
  on <- which(b$itemDim[[dim]] == 1L)[1]
  nb <- set_assignment(b, "itemDim", dim, .keys(b$itemDim, off), 1L)
  expect_true(all(nb$itemDim[[dim]][off] == 1L))
  nb <- set_assignment(nb, "itemDim", dim, .keys(b$itemDim, on), 0L)
  expect_identical(nb$itemDim[[dim]][on], 0L)
  d <- bank_diff(b, nb)
  expect_equal(nrow(d), 3L)
  expect_setequal(d[["改成"]], c("1", "1", "0"))
  expect_true(all(d[["欄"]] == dim))
  # 改回去 → 沒有改動
  back <- set_assignment(nb, "itemDim", dim, .keys(b$itemDim, off), 0L)
  back <- set_assignment(back, "itemDim", dim, .keys(b$itemDim, on), 1L)
  expect_equal(nrow(bank_diff(b, back)), 0L)
  # 波次
  w <- get_wave_cols(b$itemWave)[1]
  i <- which(b$itemWave[[w]] == 0L)[1]
  nw <- set_assignment(b, "itemWave", w, .keys(b$itemWave, i), 1L)
  expect_identical(bank_diff(b, nw)[["表"]], "itemWave")
  expect_error(set_assignment(b, "itemDim", "沒有這個構面", .keys(b$itemDim, 1), 1L),
               class = "kit_user_error")
  expect_error(set_assignment(b, "itemDim", dim, .keys(b$itemDim, 1), 2L),
               class = "kit_user_error")
})

test_that("add_column / remove_column：構面同步 dimMeta 與 dimReliability", {
  b <- .bank()
  nb <- add_column(b, "itemDim", "  新構面  ")
  expect_true("新構面" %in% names(nb$itemDim))
  expect_true(all(nb$itemDim[["新構面"]] == 0L))
  expect_identical(nb$dimMeta$dimName[nb$dimMeta$dimID == "新構面"], "新構面")
  d <- bank_diff(b, nb)
  expect_true(any(d[["表"]] == "itemDim" & d[["改成"]] == "（新增）"))
  expect_true(any(d[["表"]] == "dimMeta" & d[["欄"]] == "dimID"))
  expect_error(add_column(nb, "itemDim", "新構面"), "已經有", class = "kit_user_error")
  expect_error(add_column(b, "itemDim", "  "), class = "kit_user_error")
  expect_error(add_column(b, "itemDim", "itemID"), class = "kit_user_error")

  # 刪掉一個有信度的構面：dimMeta 與 dimReliability 都拿掉
  dim <- get_matrix_cols(b$itemDim)[1]
  w <- get_wave_cols(b$itemWave)[1]
  b2 <- set_wave_reliability(b, dim, w, "0.8")
  rb <- remove_column(b2, "itemDim", dim)
  expect_false(dim %in% names(rb$itemDim))
  expect_false(dim %in% rb$dimMeta$dimID)
  expect_false(dim %in% rb$dimReliability$dimID)
  d <- bank_diff(b, rb)
  expect_true(any(d[["表"]] == "itemDim" & d[["欄"]] == dim & d[["改成"]] == "（刪除）"))
  expect_true(any(d[["表"]] == "dimMeta" & d[["改成"]] == "（刪除）"))
  expect_error(remove_column(b, "itemDim", "沒有這個構面"), class = "kit_user_error")

  # 波次
  wb <- add_column(b, "itemWave", "M999")
  expect_true("M999" %in% names(wb$itemWave))
  expect_equal(nrow(wb$dimMeta), nrow(b$dimMeta))
  rw <- remove_column(b2, "itemWave", w)
  expect_false(w %in% names(rw$itemWave))
  expect_false(w %in% rw$dimReliability$wave)
})

test_that("set_dim_meta 改名稱、別名、備註、信度", {
  b <- .bank()
  dim <- b$dimMeta$dimID[1]
  nb <- set_dim_meta(b, dim, dimName = "新名稱", alias = "別名A", note = "備註",
                     reliability = "0.81")
  r <- nb$dimMeta[nb$dimMeta$dimID == dim, ]
  expect_identical(r$dimName, "新名稱")
  expect_identical(r$alias, "別名A")
  expect_identical(r$note, "備註")
  expect_identical(r$reliability, "0.81")
  d <- bank_diff(b, nb)
  expect_setequal(d[["欄"]], c("dimName", "alias", "note", "reliability"))
  # 名稱留空 → 用 dimID；NULL 不改
  nb2 <- set_dim_meta(nb, dim, dimName = "")
  expect_identical(nb2$dimMeta$dimName[nb2$dimMeta$dimID == dim], dim)
  expect_identical(nb2$dimMeta$alias[nb2$dimMeta$dimID == dim], "別名A")
  expect_error(set_dim_meta(b, dim, reliability = "高"), "信度", class = "kit_user_error")
  expect_error(set_dim_meta(b, dim, reliability = "1.5"), class = "kit_user_error")
  expect_error(set_dim_meta(b, "沒有這個構面", alias = "x"), class = "kit_user_error")
})

test_that("set_wave_reliability 設定、改、拿掉", {
  b <- .bank()
  dim <- b$dimMeta$dimID[1]
  w <- get_wave_cols(b$itemWave)[2]
  nb <- set_wave_reliability(b, dim, w, "0.7")
  expect_equal(nrow(nb$dimReliability), nrow(b$dimReliability) + 1L)
  d <- bank_diff(b, nb)
  expect_identical(d[["表"]], "dimReliability")
  expect_identical(d[["改成"]], "0.7")
  nb <- set_wave_reliability(nb, dim, w, 0.75)
  expect_equal(nb$dimReliability$reliability[nb$dimReliability$dimID == dim], 0.75)
  nb <- set_wave_reliability(nb, dim, w, "")
  expect_equal(nrow(bank_diff(b, nb)), 0L)
  expect_error(set_wave_reliability(b, dim, "沒有這個波次", "0.7"), class = "kit_user_error")
  expect_error(set_wave_reliability(b, dim, w, "abc"), class = "kit_user_error")
})

test_that("edit_item_rows 篩選、edit_column_counts 照順序", {
  b <- .bank()
  dim <- get_matrix_cols(b$itemDim)[1]
  all <- edit_item_rows(b, "itemDim", dim)
  expect_equal(nrow(all), nrow(b$itemDim))
  expect_named(all, c("itemID", "itemVer", "block", "label", "assigned"))
  only <- edit_item_rows(b, "itemDim", dim, only_assigned = TRUE)
  expect_equal(nrow(only), sum(b$itemDim[[dim]] == 1L))
  kw <- b$itemDim$itemID[5]
  hit <- edit_item_rows(b, "itemDim", dim, keyword = toupper(kw))
  expect_true(kw %in% hit$itemID)
  expect_lt(nrow(hit), nrow(all))
  cnt <- edit_column_counts(b, "itemWave")
  expect_identical(cnt$column, get_wave_cols(b$itemWave))
  expect_equal(cnt$n[1], sum(b$itemWave[[cnt$column[1]]] == 1L))
})

test_that("edit_changes_summary 一句話", {
  b <- .bank()
  dim <- get_matrix_cols(b$itemDim)[1]
  off <- which(b$itemDim[[dim]] == 0L)[1:3]
  on <- which(b$itemDim[[dim]] == 1L)[1]
  nb <- set_assignment(b, "itemDim", dim, .keys(b$itemDim, off), 1L)
  nb <- set_assignment(nb, "itemDim", dim, .keys(b$itemDim, on), 0L)
  expect_identical(edit_changes_summary(bank_diff(b, nb)),
                   paste0(dim, " +3 題、−1 題"))
  nb <- add_column(nb, "itemDim", "新構面")
  nb <- set_dim_meta(nb, b$dimMeta$dimID[2], alias = "別名")
  s <- edit_changes_summary(bank_diff(b, nb))
  expect_match(s, "新增構面「新構面」", fixed = TRUE)
  expect_match(s, "構面資訊 1 處", fixed = TRUE)
  rb <- remove_column(b, "itemDim", dim)
  expect_identical(edit_changes_summary(bank_diff(b, rb)), paste0("刪除構面「", dim, "」"))
  expect_identical(edit_changes_summary(bank_diff(b, b)), "")
  expect_identical(edit_download_filename("parent", as.Date("2026-11-01")),
                   "KIT_parent_2026-11-01_改.xlsx")
})

test_that("set_item_fields 改一題一版的內容，bank_diff 記成 itemBank", {
  b <- .bank()
  ib <- b$itemBank
  i <- which(ib$itemID == "famC21" & ib$itemVer == 1L)
  nb <- set_item_fields(b, "famC21", 1L, noResponse = "不適用:9995; 拒答:7777; 遺漏值:9999",
                        reverseScored = "1", itemType = "  MC ")
  expect_identical(nb$itemBank$noResponse[i], "不適用:9995; 拒答:7777; 遺漏值:9999")
  expect_identical(nb$itemBank$reverseScored[i], 1L)
  expect_identical(nb$itemBank$itemType[i], "MC")  # 去掉前後空白，和原本一樣
  # 別的列不動
  expect_identical(nb$itemBank[-i, ], b$itemBank[-i, ])
  d <- bank_diff(b, nb)
  expect_setequal(d[["欄"]], c("noResponse", "reverseScored"))
  expect_true(all(d[["表"]] == "itemBank" & d[["題號或構面"]] == "famC21" & d[["版本"]] == "1"))
  expect_match(edit_changes_summary(d), "題目內容 1 題 2 處", fixed = TRUE)
  # 改好後 bank_audit 不再說撞號
  expect_false(any(bank_audit(nb)$itemID == "famC21" &
                     bank_audit(nb)$check %in% c("noresponse_clash", "noresponse_shared")))

  # named list 也可以；換行統一成 \n、全形空白也去掉
  nb2 <- set_item_fields(b, "famC21", 1L, fields = list(label = "　第一行\r\n第二行 ",
                                                        deprecated = 1))
  expect_identical(nb2$itemBank$label[i], "第一行\n第二行")
  expect_identical(nb2$itemBank$deprecated[i], 1L)

  expect_error(set_item_fields(b, "famC21", 1L, reverseScored = 2), "要是 0 或 1",
               class = "kit_user_error")
  expect_error(set_item_fields(b, "famC21", 1L, deprecated = "是"), class = "kit_user_error")
  expect_error(set_item_fields(b, "famC21", 1L, itemID = "x"), "不能改", class = "kit_user_error")
  expect_error(set_item_fields(b, "famC21", 99L, label = "x"), "沒有 famC21 第 99 版",
               class = "kit_user_error")
  expect_error(set_item_fields(b, "famC21", 1L, label = c("a", "b")), class = "kit_user_error")
  expect_identical(set_item_fields(b, "famC21", 1L), b)
})

test_that("edit_item_values 與 edit_item_versions", {
  b <- .bank()
  v <- edit_item_values(b, "caregiverf01", 4L)
  expect_identical(names(v), ITEM_FIELDS)
  expect_identical(v$deprecated, 0L)
  expect_null(edit_item_values(b, "caregiverf01", 99L))
  expect_identical(edit_item_versions(b, "caregiverf01"), 1:4)
})
