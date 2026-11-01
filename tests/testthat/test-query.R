# query.R：題項查詢的純函式

# 一個小題庫：4 題、3 個波次、2 個構面
tiny_bank <- function() {
  ib <- data.frame(
    itemID = c("sle01", "sle01", "sle02", "eat01"),
    itemVer = c(1L, 2L, 1L, 1L),
    deprecated = c(1L, 0L, 0L, 0L),
    stemID = c("睡眠", "睡眠", "上床", ""),
    block = c("作息", "作息", "作息", "飲食"),
    itemType = c("MC", "MC", "NR", "MC"),
    respondent = c("家長", "家長", "家長,親友", "親友"),
    reverseScored = 0L,
    label = c("孩子每天睡幾小時", "孩子平日每天睡幾個小時", "孩子幾點上床 Sleep", "孩子吃早餐嗎"),
    value = c("", "", "", "1 有;2 沒有"),
    noResponse = "", note = c("", "", "", "早餐 note"),
    stringsAsFactors = FALSE
  )
  iw <- data.frame(itemID = ib$itemID, itemVer = ib$itemVer,
                   check.names = FALSE, stringsAsFactors = FALSE)
  iw[["M048小"]] <- c(0L, 1L, 1L, 1L)
  iw[["M003小"]] <- c(1L, 0L, 1L, 0L)
  iw[["M012小"]] <- c(1L, 0L, 0L, 1L)
  id <- data.frame(itemID = ib$itemID, itemVer = ib$itemVer,
                   check.names = FALSE, stringsAsFactors = FALSE)
  id[["作息 (sleep)"]] <- c(1L, 1L, 1L, 0L)
  id[["飲食"]] <- c(0L, 0L, 1L, 1L)
  list(itemBank = ib, itemWave = iw, itemDim = id,
       dimMeta = data.frame(dimID = c("作息 (sleep)", "飲食"), dimName = c("睡眠作息", ""),
                            alias = "", reliability = "", note = "", stringsAsFactors = FALSE))
}

ids <- function(r) paste0(r$itemID, "v", r$itemVer)

test_that("keyword terms are ANDed, case-insensitive, and * matches an itemID prefix", {
  b <- tiny_bank()
  expect_equal(ids(query_items(b, keyword = "孩子 睡")), "sle01v2")  # v1 已停用
  expect_equal(ids(query_items(b, keyword = "孩子 睡", include_deprecated = TRUE)),
               c("sle01v1", "sle01v2"))
  expect_equal(ids(query_items(b, keyword = "sleep")), "sle02v1")
  expect_equal(ids(query_items(b, keyword = "sle*")), c("sle01v2", "sle02v1"))
  expect_equal(ids(query_items(b, keyword = "SLE* 上床")), "sle02v1")
  expect_equal(nrow(query_items(b, keyword = "孩子 不存在")), 0L)
})

test_that("fields = 'all' also searches value, note, and block", {
  b <- tiny_bank()
  expect_equal(nrow(query_items(b, keyword = "note")), 0L)
  expect_equal(ids(query_items(b, keyword = "note", fields = "all")), "eat01v1")
  expect_equal(ids(query_items(b, keyword = "作息", fields = "all")), c("sle01v2", "sle02v1"))
})

test_that("waves and dims each have any / all", {
  b <- tiny_bank()
  w <- c("M003小", "M012小")
  expect_equal(ids(query_items(b, waves = w)), c("sle02v1", "eat01v1"))
  expect_equal(ids(query_items(b, waves = w, include_deprecated = TRUE, wave_match = "all")),
               "sle01v1")
  expect_equal(nrow(query_items(b, waves = w, wave_match = "all")), 0L)
  d <- c("作息 (sleep)", "飲食")
  expect_equal(ids(query_items(b, dims = d)), c("sle01v2", "sle02v1", "eat01v1"))
  expect_equal(ids(query_items(b, dims = d, dim_match = "all")), "sle02v1")
  expect_warning(r <- query_items(b, waves = "M999"), "M999")
  expect_equal(nrow(r), 3L)
})

test_that("advanced filters and respondent lists", {
  b <- tiny_bank()
  expect_equal(ids(query_items(b, respondents = "親友")), c("sle02v1", "eat01v1"))
  expect_equal(ids(query_items(b, respondents = c("家長"))), c("sle01v2", "sle02v1"))
  expect_equal(ids(query_items(b, blocks = "飲食")), "eat01v1")
  expect_equal(ids(query_items(b, item_types = "NR")), "sle02v1")
})

test_that("n_waves counts the waves an item appears in", {
  r <- query_items(tiny_bank(), include_deprecated = TRUE)
  expect_equal(r$n_waves, c(2L, 1L, 2L, 2L))
  expect_true(all(names(tiny_bank()$itemBank) %in% names(r)))
})

test_that("the parent bank: deprecated items are left out by default and queries are fast", {
  b <- .bank("parent")
  all <- query_items(b, include_deprecated = TRUE)
  expect_equal(nrow(all), nrow(b$itemBank))
  expect_equal(nrow(query_items(b)), sum(b$itemBank$deprecated != 1L))
  t <- system.time(query_items(b, keyword = "孩子 時間", fields = "all",
                               waves = get_wave_cols(b$itemWave)[1:3], wave_match = "all"))
  expect_lt(t[["elapsed"]], 0.5)
})

test_that("item_detail gives waves in age order, dims, and other versions", {
  b <- tiny_bank()
  d <- item_detail(b, "sle01", 2)
  expect_equal(d$item$label, "孩子平日每天睡幾個小時")
  expect_equal(d$waves, "M048小")
  expect_equal(d$dims$dimID, "作息 (sleep)")
  expect_equal(d$dims$dimName, "睡眠作息")
  expect_equal(d$other_versions$itemVer, 1L)
  expect_equal(d$other_versions$waves[[1]], c("M003小", "M012小"))
  expect_equal(item_detail(b, "eat01", 1)$dims$dimName, "飲食")  # 沒有 dimName 用 dimID
  expect_null(item_detail(b, "nope", 1))
  expect_equal(nrow(item_detail(b, "eat01", 1)$other_versions), 0L)
})

test_that("label_diff marks added and removed text", {
  d <- label_diff("孩子每天睡幾小時", "孩子平日每天睡幾個小時")
  expect_equal(paste(d$text[d$type != "added"], collapse = ""), "孩子每天睡幾小時")
  expect_equal(paste(d$text[d$type != "removed"], collapse = ""), "孩子平日每天睡幾個小時")
  expect_equal(d$text[d$type == "added"], c("平日", "個"))
  expect_equal(label_diff("一樣", "一樣")$type, "same")
  expect_equal(nrow(label_diff("", "")), 0L)
  d2 <- label_diff("：有沒有", "-有沒有")
  expect_equal(d2$type, c("removed", "added", "same"))
  expect_equal(label_diff(NA, "新")$type, "added")
})

test_that("crosswalk is item × wave with ✓ in age order", {
  b <- tiny_bank()
  r <- query_items(b, include_deprecated = TRUE)
  xw <- crosswalk(b, r)
  expect_equal(names(xw), c("itemID", "itemVer", "block", "label", "M003小", "M012小", "M048小"))
  expect_equal(nrow(xw), 4L)
  expect_equal(xw[["M048小"]], c("", "✓", "✓", "✓"))
  expect_equal(nrow(crosswalk(b, r[0, ])), 0L)
  pb <- .bank("parent")
  xw2 <- crosswalk(pb, query_items(pb, keyword = "睡眠"))
  expect_equal(ncol(xw2), 4L + length(get_wave_cols(pb$itemWave)))
})

test_that("wave_counts is block × wave", {
  b <- tiny_bank()
  m <- wave_counts(b, query_items(b, include_deprecated = TRUE))
  expect_equal(rownames(m), c("作息", "飲食"))
  expect_equal(colnames(m), c("M003小", "M012小", "M048小"))
  expect_equal(unname(m["作息", ]), c(2L, 1L, 2L))
  expect_equal(unname(m["飲食", ]), c(0L, 1L, 1L))
  expect_equal(nrow(wave_counts(b, query_items(b, keyword = "沒有這個"))), 0L)
})

test_that("query_code parses and reproduces the query", {
  code <- query_code("parent", keyword = " 睡眠  時間 ", fields = "all",
                     waves = c("M036小", "M048小"), wave_match = "all",
                     dims = "作息 (sleep)", respondents = "家長", include_deprecated = TRUE)
  expect_silent(ex <- parse(text = code))
  expect_match(code, 'keyword = "睡眠 時間"', fixed = TRUE)
  expect_match(code, 'waves = c("M036小", "M048小")', fixed = TRUE)
  expect_match(code, 'wave_match = "all"', fixed = TRUE)
  expect_match(code, "include_deprecated = TRUE", fixed = TRUE)
  expect_no_match(code, "dim_match")
  plain <- query_code("child_adolescent")
  expect_match(plain, "items <- query_items(bank)", fixed = TRUE)
  expect_match(plain, 'load_bank("child_adolescent")', fixed = TRUE)
  expect_silent(parse(text = plain))
  f <- query_code(keyword = 'a"b', file = "我的 題庫.xlsx")
  expect_silent(parse(text = f))
  expect_match(f, 'read_bank_xlsx("我的 題庫.xlsx")', fixed = TRUE)
  # 程式碼的 query_items() 呼叫和 app 得到一樣的結果
  bank <- tiny_bank()
  env <- new.env()
  env$bank <- bank
  call <- parse(text = query_code(keyword = "孩子", waves = c("M003小", "M012小")))[[3]]
  eval(call, env)
  expect_equal(env$items, query_items(bank, keyword = "孩子", waves = c("M003小", "M012小")))
})

test_that("zero_result_hint says why and what to try", {
  b <- tiny_bank()
  a <- list(keyword = "", fields = "text", waves = c("M003小", "M012小"), wave_match = "all",
            dims = character(), dim_match = "any", include_deprecated = FALSE)
  h <- zero_result_hint(b, a)
  expect_match(h$reason, "每個波次都出現")
  expect_true(any(grepl("任一波次", h$tries)))
  h2 <- zero_result_hint(b, utils::modifyList(a, list(keyword = "早餐 睡", waves = character())))
  expect_match(h2$reason, "含「早餐 睡」")
})

test_that("keywords are normalized: full-width, simplified, variants", {
  expect_equal(normalize_text(c("３Ｃ產品", "亲子　互动", "唸書", "臺灣", "ＴＶ", NA)),
               c("3c產品", "親子 互動", "念書", "台灣", "tv", ""))
  b <- .bank("parent")
  n <- function(k, ...) nrow(query_items(b, keyword = k, ...))
  expect_equal(n("３Ｃ"), n("3C"))
  expect_equal(n("ＴＶ", fields = "all"), n("tv", fields = "all"))
  expect_gt(n("亲子"), 0L)
  expect_equal(n("亲子"), n("親子"))
  expect_gt(n("念書"), 0L)
  expect_equal(n("念書"), n("唸書"))
  # 題目文字：itemID 只從開頭比，"3C" 不會對到 pfB0203c
  expect_false("pfB0203c" %in% query_items(b, keyword = "3C")$itemID)
  expect_true("pfB0203c" %in% query_items(b, keyword = "pfB02")$itemID)
  expect_true("pfB0203c" %in% query_items(b, keyword = "3C", fields = "all")$itemID)
})

test_that("keyword_match = 'any' keeps items matching any term", {
  b <- tiny_bank()
  expect_equal(nrow(query_items(b, keyword = "早餐 上床")), 0L)
  expect_equal(ids(query_items(b, keyword = "早餐 上床", keyword_match = "any")),
               c("sle02v1", "eat01v1"))
  pb <- .bank("parent")
  all <- nrow(query_items(pb, keyword = "睡眠 睡覺"))
  any <- nrow(query_items(pb, keyword = "睡眠 睡覺", keyword_match = "any"))
  expect_gt(any, all)
  code <- query_code(keyword = "睡眠 睡覺", keyword_match = "any")
  expect_match(code, 'keyword_match = "any"', fixed = TRUE)
  expect_no_match(query_code(keyword = "睡眠", keyword_match = "any"), "keyword_match")
})

test_that("zero_result_hint suggests any-word and dimensions with the word in their name", {
  pb <- .bank("parent")
  a <- list(keyword = "親子 互動", fields = "text", keyword_match = "all",
            waves = character(), dims = character(), include_deprecated = FALSE)
  h <- zero_result_hint(pb, a)
  expect_true(any(grepl("任一個詞", h$tries)))
  expect_lte(nrow(h$dims), 5L)
  expect_gt(nrow(h$dims), 0L)
  expect_true(all(grepl("親子|互動", h$dims$dimName)))
  expect_equal(nrow(zero_result_hint(tiny_bank(), utils::modifyList(a, list(keyword = "沒有")))$dims), 0L)
})

test_that("item_detail has a version timeline, field differences, and same-stem items", {
  b <- tiny_bank()
  d <- item_detail(b, "sle01", 2)
  expect_equal(d$timeline$itemVer, c(1L, 2L))
  expect_equal(names(d$timeline), c("itemVer", "deprecated", "M003小", "M012小", "M048小"))
  expect_equal(d$timeline[["M048小"]], c("", "✓"))
  expect_equal(d$other_versions$differs[[1]], "label")
  expect_equal(nrow(d$same_stem), 0L)
  b$itemBank$stemID[3] <- "睡眠"
  b$itemBank$respondent[1] <- "親友"
  d <- item_detail(b, "sle01", 2)
  expect_equal(d$same_stem$itemID, "sle02")
  expect_equal(d$same_stem$waves[[1]], c("M003小", "M048小"))
  expect_equal(d$other_versions$differs[[1]], c("label", "respondent"))
  pd <- item_detail(.bank("parent"), "area1_1", 2)
  expect_true(all(c("label", "value") %in% pd$other_versions$differs[[1]]))
  expect_gt(nrow(pd$same_stem), 0L)
  expect_false("area1_1" %in% pd$same_stem$itemID)
})

test_that("item type, missing-value codes, and notes read plainly", {
  expect_equal(item_type_label(c("MC", "DC", "XX", "", NA)),
               c("單選（MC）", "複選的一個選項（DC）", "XX", "", ""))
  expect_equal(format_no_response("拒答:5; 遺漏值:9999"), "拒答 5、遺漏值 9999")
  expect_equal(format_no_response(NA), "")
  expect_equal(split_note("適用:父母版; v1→v2版本異動: 計分:「0 沒有; 1 有」→「nan」"),
               c("適用:父母版", "v1→v2版本異動: 計分:「0 沒有; 1 有」→「nan」"))
  expect_equal(split_note(""), character())
})

test_that("wave_counts can group by dimension and sorts rows by first wave", {
  b <- tiny_bank()
  r <- query_items(b, include_deprecated = TRUE)
  m <- wave_counts(b, r, by = "dim")
  expect_equal(rownames(m), c("睡眠作息", "飲食"))
  expect_equal(unname(m["睡眠作息", ]), c(2L, 1L, 2L))
  expect_equal(unname(m["飲食", ]), c(1L, 1L, 2L))
  # 依第一次出現的波次排：把飲食只留在 M048小 之後
  b$itemWave[["M003小"]][3] <- 0L
  b$itemBank$block[3] <- "其他"
  m2 <- wave_counts(b, r)
  expect_equal(rownames(m2), c("作息", "飲食", "其他"))
  expect_equal(nrow(wave_counts(b, r[0, ], by = "dim")), 0L)
  pb <- .bank("parent")
  md <- wave_counts(pb, query_items(pb, keyword = "睡"), by = "dim")
  first <- apply(md > 0, 1, function(x) which(x)[1])
  expect_false(is.unsorted(first))
})

test_that("query_workbook has 說明, 題目 with 構面, and 跨波次對照", {
  b <- tiny_bank()
  r <- query_items(b, keyword = "孩子")
  w <- query_workbook(b, r, list(keyword = "孩子"), domain = "parent", version = "2026-09-12")
  expect_equal(names(w), c("說明", "題目", "跨波次對照"))
  expect_equal(w$題目$構面, c("睡眠作息", "睡眠作息、飲食", "飲食"))
  info <- paste(w$說明$說明, collapse = "\n")
  expect_match(info, "關鍵字「孩子」")
  expect_match(info, "臺灣幼兒發展調查資料庫（KIT）題庫，2026-09-12", fixed = TRUE)
  expect_match(info, "家長題庫")
  expect_true("  itemType" %in% w$說明$項目)
  expect_match(query_workbook(b, r, version = NA)$說明$說明[1], "版本日期不明")
})
