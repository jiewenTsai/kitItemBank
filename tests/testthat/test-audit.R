# tests/testthat/test-audit.R：題庫內容檢查（R/audit.R）與 check_bank、bank_diff 的新項目

.audit_cols <- c("check", "level", "itemID", "itemVer", "dimID", "message")

# 小題庫：4 題（a1 有三版），itemDim 有「反向題」與兩個構面，itemWave 兩個波次
.mini_bank <- function() {
  ib <- data.frame(
    itemID = c("a1", "a1", "a1", "b1", "c1", "d1"),
    itemVer = c(1L, 2L, 4L, 1L, 1L, 1L),
    deprecated = c(1L, 1L, 0L, 0L, 0L, 1L),
    stemID = c("", "", "", "", "（請問 (例如:", ""),
    block = "", itemType = c("MC", "MC", "MC", "", "MC", "DC"),
    respondent = "母親", reverseScored = c(0L, 0L, 0L, 1L, 0L, 0L),
    label = c("睡得好嗎", "睡得好嗎", " 睡得好嗎", "髋部\n疼痛", "C 題", "D 題"),
    value = c("1 很少;\n2 有時;\n3 經常;", "1:很少;\r\n2:有時;", "1~5點量表", "", "0 沒有;\n1 有;", "0 沒有;\n1 有;"),
    noResponse = c("拒答:3; 遺漏值:9999", "不適用:5; 不適答:5", "", "", "不知道:8888; 不適用:8888", "遺漏值:9999"),
    note = "", stringsAsFactors = FALSE
  )
  keys <- ib[, c("itemID", "itemVer")]
  dim <- data.frame(keys, "反向題" = c(0L, 0L, 0L, 0L, 1L, 0L), "睡眠" = c(1L, 1L, 0L, 0L, 0L, 0L),
               "舊構面" = c(1L, 1L, 0L, 0L, 0L, 1L), "單題" = c(0L, 0L, 0L, 1L, 0L, 0L),
               check.names = FALSE)
  wave <- data.frame(keys, M003 = c(1L, 1L, 0L, 1L, 1L, 1L), M006 = c(0L, 0L, 1L, 1L, 1L, 0L))
  list(itemBank = ib, itemDim = dim, itemWave = wave,
       dimMeta = data.frame(dimID = c("反向題", "睡眠", "舊構面", "單題"), stringsAsFactors = FALSE),
       dimReliability = empty_dim_reliability())
}

test_that("bank_audit 的欄位與型別", {
  a <- bank_audit(.mini_bank())
  expect_identical(names(a), .audit_cols)
  expect_type(a$itemVer, "integer")
  expect_true(all(a$level %in% c("todo", "note")))
  expect_true(all(a$check %in% AUDIT_CHECKS$check))
  e <- bank_audit(list(itemBank = .mini_bank()$itemBank[0, ], itemDim = NULL, itemWave = NULL))
  expect_identical(names(e), .audit_cols)
  expect_equal(nrow(e), 0L)
})

test_that("bank_audit 在小題庫找到每一種問題", {
  a <- bank_audit(.mini_bank())
  has <- function(ck, id = NULL, ver = NULL, dim = NULL, msg = NULL) {
    x <- a[a$check == ck, , drop = FALSE]
    if (!is.null(id)) x <- x[x$itemID %in% id, , drop = FALSE]
    if (!is.null(ver)) x <- x[x$itemVer %in% ver, , drop = FALSE]
    if (!is.null(dim)) x <- x[x$dimID %in% dim, , drop = FALSE]
    if (!is.null(msg)) x <- x[grepl(msg, x$message, fixed = TRUE), , drop = FALSE]
    nrow(x) > 0L
  }
  expect_true(has("noresponse_clash", "a1", 1L, msg = "遺漏值「拒答:3」和選項 3 同一個代碼"))
  expect_false(has("noresponse_clash", "a1", 2L))  # 選項只有 1、2，代碼 5 沒有撞
  expect_true(has("noresponse_shared", "a1", 2L, msg = "「不適用」和「不適答」都用代碼 5"))
  expect_true(has("noresponse_shared", "c1", 1L, msg = "代碼 8888"))
  expect_true(has("reverse_mismatch", "c1", 1L, msg = "在「反向題」構面，但 reverseScored 是 0"))
  expect_true(has("reverse_mismatch", "b1", 1L, msg = "reverseScored 是 1，但不在「反向題」構面"))
  expect_true(has("dim_not_inherited", "a1", 4L, msg = "第 2 版有：睡眠、舊構面"))
  expect_true(has("versions_same_wave", "a1", msg = "第 1、2 版都在 M003"))
  expect_true(is.na(a$itemVer[a$check == "versions_same_wave"]))
  expect_true(has("itemver_gap", "a1", msg = "少了第 3 版"))
  expect_true(has("value_empty", "b1", 1L))
  expect_true(has("value_scale_only", "a1", 4L, msg = "1~5點量表"))
  expect_true(has("itemtype_empty", "b1", 1L))
  expect_true(has("dim_single_item", "b1", 1L, dim = "單題"))
  expect_true(has("dim_single_item", "c1", dim = "反向題"))
  expect_true(has("dim_single_item", "a1", NA_integer_, dim = "睡眠"))
  expect_true(has("dim_only_deprecated", dim = "舊構面", msg = "2 題都是停用的版本"))
  expect_true(is.na(a$itemID[a$check == "dim_only_deprecated"][1]))
  expect_true(has("text_newline", "b1", 1L))
  expect_true(has("text_space", "a1", 4L))
  expect_true(has("text_brackets", "c1", 1L, msg = "題幹（stemID）"))
  expect_true(has("simplified_char", "b1", 1L, msg = "「髋」（應為「髖」）"))
  # todo 排在 note 前面
  expect_false(is.unsorted(match(a$level, c("todo", "note"))))
})

test_that("bank_audit 在內建題庫找到已知的問題，而且夠快", {
  b <- .bank("parent")
  t <- system.time(a <- bank_audit(b))[["elapsed"]]
  expect_lt(t, 1)
  row <- function(ck, id) a[a$check == ck & a$itemID %in% id, , drop = FALSE]
  x <- row("noresponse_clash", "famC21")
  expect_gt(nrow(x), 0L)
  expect_match(x$message[1], "拒答:5", fixed = TRUE)
  expect_match(x$message[1], "選項 5", fixed = TRUE)
  expect_gt(nrow(row("noresponse_shared", "caregiverf01")), 0L)
  expect_true(4L %in% row("dim_not_inherited", "caregiverf01")$itemVer)
  expect_gt(nrow(row("reverse_mismatch", "famC17n")), 0L)
  expect_match(row("simplified_char", "med0303")$message, "髋", fixed = TRUE)
  expect_gt(nrow(row("versions_same_wave", "caregiverf01")), 0L)
  s <- bank_audit_summary(a)
  expect_identical(names(s), c("check", "level", "label", "n"))
  expect_equal(sum(s$n), nrow(a))
  expect_equal(s$n[s$check == "versions_same_wave"], 185L)
  expect_gte(s$n[s$check == "itemver_gap"], 40L)
  # 另外兩個題庫也跑得動
  t2 <- bank_audit(.bank("childcare_teacher"))
  expect_match(t2$message[t2$check == "simplified_char" & t2$itemID == "T51010n"], "内", fixed = TRUE)
  expect_identical(names(bank_audit(.bank("child_adolescent"))), .audit_cols)
})

test_that("check_bank：矩陣不是 0/1 擋住；版本不連號與內容問題只提醒", {
  b <- .bank("parent")
  chk <- check_bank(b)
  expect_true(chk$ok)
  titles <- vapply(chk$items, `[[`, "", "title")
  states <- vapply(chk$items, `[[`, "", "state")
  expect_true(any(grepl("itemVer 不連號", titles) & states == "note"))
  expect_true(any(grepl("項要注意（在「題庫編修」看）", titles, fixed = TRUE) & states == "note"))

  bad <- b
  bad$itemDim[[get_matrix_cols(bad$itemDim)[1]]][1:2] <- 2L
  chk <- check_bank(bad)
  expect_false(chk$ok)
  titles <- vapply(chk$items, `[[`, "", "title")
  expect_true(any(grepl("itemDim 有 2 格不是 0 或 1", titles, fixed = TRUE)))
})

test_that("bank_diff 記下 itemBank 欄位的改動與新增、刪除的版本", {
  b <- .mini_bank()
  nb <- set_item_fields(b, "a1", 4L, label = "睡得好嗎", reverseScored = 1L)
  d <- bank_diff(b, nb)
  expect_equal(nrow(d), 2L)
  expect_true(all(d[["表"]] == "itemBank"))
  expect_setequal(d[["欄"]], c("label", "reverseScored"))
  expect_identical(d[["改成"]][d[["欄"]] == "reverseScored"], "1")
  expect_identical(d[["版本"]], c("4", "4"))

  nb2 <- b
  nb2$itemBank <- rbind(b$itemBank[-5, ], transform(b$itemBank[5, ], itemVer = 2L))
  d <- bank_diff(b, nb2)
  x <- d[d[["表"]] == "itemBank", ]
  expect_true(any(x[["題號或構面"]] == "c1" & x[["版本"]] == "1" & x[["改成"]] == "（刪除）"))
  expect_true(any(x[["題號或構面"]] == "c1" & x[["版本"]] == "2" & x[["改成"]] == "（新增）"))
  s <- edit_changes_summary(d)
  expect_match(s, "新增 1 個題目版本", fixed = TRUE)
  expect_match(s, "刪除 1 個題目版本", fixed = TRUE)
  expect_equal(nrow(bank_diff(b, b)), 0L)
})
