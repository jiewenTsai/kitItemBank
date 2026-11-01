# dims.R、network.R：構面查詢的計算（內建家長題庫）

test_that("dim_overview has one row per dimension column", {
  b <- .bank("parent")
  ov <- dim_overview(b)
  expect_equal(nrow(ov), length(get_matrix_cols(b$itemDim)))
  expect_named(ov, c("dimID", "dimName", "alias", "n_items", "n_versions", "n_waves", "n_rel",
                     "first_wave", "last_wave"))
  expect_identical(ov$dimID, get_matrix_cols(b$itemDim))
  att <- ov[ov$dimID == "注意力", ]
  expect_equal(att$alias, "注意力/執行功能")
  expect_equal(att$n_items, length(unique(b$itemDim$itemID[b$itemDim[["注意力"]] == 1L])))
  expect_equal(att$n_versions, sum(b$itemDim[["注意力"]] == 1L))
  expect_equal(att$n_waves, 23L)
  # 題數是 itemID，不是版本：親友親職參與 10 題 × 3 版
  cg <- ov[ov$dimID == "親友親職參與", ]
  expect_equal(cg$n_items, 10L)
  expect_equal(cg$n_versions, 30L)
  expect_true(all(ov$n_items <= ov$n_versions))
  # 最近一波、最早一波照 get_wave_cols() 的順序
  waves <- get_wave_cols(b$itemWave)
  in_any <- !is.na(ov$last_wave)
  expect_true(all(match(ov$first_wave[in_any], waves) <= match(ov$last_wave[in_any], waves)))
  expect_identical(is.na(ov$last_wave), ov$n_waves == 0L)
  expect_true(all(ov$n_rel <= ov$n_waves))
  expect_true(all(ov$n_waves <= length(get_wave_cols(b$itemWave))))
})

test_that("dim_items lists the items of a dimension with a check per wave", {
  b <- .bank("parent")
  x <- dim_items(b, "注意力")
  expect_equal(nrow(x), sum(b$itemDim[["注意力"]] == 1L))
  expect_identical(names(x)[1:5], c("itemID", "itemVer", "deprecated", "respondent", "label"))
  waves <- names(x)[-(1:5)]
  expect_length(waves, 23L)
  expect_true(all(unlist(x[waves]) %in% c("✓", "")))
  expect_true(all(nzchar(x$label)))
  expect_true(all(x$deprecated %in% c(0L, 1L)))
  expect_true(all(nzchar(x$respondent)))
  cg <- dim_items(b, "親友親職參與")
  expect_true(any(cg$deprecated == 1L))
  expect_true(any(grepl("親友", cg$respondent)))
  expect_error(dim_items(b, "沒有這個構面"), "沒有構面")
})

test_that("dim_reliability_by_wave uses dimReliability first, then dimMeta", {
  b <- .bank("parent")
  r <- dim_reliability_by_wave(b, "注意力")
  expect_equal(nrow(r), 23L)
  expect_true(all(is.na(r$reliability)))
  # 一個波次的題數是不同的 itemID：同一題兩版同一波只算一次
  r2 <- dim_reliability_by_wave(b, "親友親職參與")
  expect_true(all(r2$n_items <= 10L))
  b$dimMeta$reliability[b$dimMeta$dimID == "注意力"] <- "0.8"
  b$dimReliability <- data.frame(dimID = "注意力", wave = "M036小", reliability = 0.91)
  r <- dim_reliability_by_wave(b, "注意力")
  expect_equal(r$reliability[r$wave == "M036小"], 0.91)
  expect_equal(r$reliability[r$wave == "M003小"], 0.8)
  expect_equal(dim_overview(b)$n_rel[dim_overview(b)$dimID == "注意力"], 23L)
})

test_that("dim_report_data assembles the report and lists the problems", {
  b <- .bank("parent")
  rd <- dim_report_data(b, "M036小", domain = "parent")
  expect_identical(rd$wave, "M036小")
  expect_gt(length(rd$dims), 0L)
  d1 <- rd$dims[[1]]
  expect_named(d1, c("dimID", "dimName", "dimNameRaw", "alias", "reliability", "note", "items"))
  expect_named(d1$items, c("itemID", "itemVer", "block", "label"))
  expect_false(any(vapply(rd$dims, function(d) anyDuplicated(d$items$itemID) > 0L, logical(1))))
  # 構面在這個波次沒有題目：不放進報告
  in_rep <- vapply(rd$dims, `[[`, "", "dimID")
  expect_length(intersect(in_rep, rd$not_in_wave), 0L)
  # 沒有信度的構面一定列出來
  no_rel <- in_rep[vapply(rd$dims, function(d) is.na(d$reliability), logical(1))]
  expect_gt(length(no_rel), 0L)
  expect_setequal(rd$problems$id[rd$problems$type == "no_reliability"], no_rel)
  # dimMeta 有、itemDim 沒有的構面 = 沒有任何題目
  expect_true("整體" %in% rd$problems$id[rd$problems$type == "empty_dim"])
  w <- dim_report_warnings(rd)
  expect_lte(length(w), 3L)
  expect_match(w[1], "沒有信度")

  # 有信度的構面不再列；題目表裡沒有的題目被抓到
  b$dimReliability <- data.frame(dimID = "注意力", wave = "M036小", reliability = 0.9)
  b$itemDim <- rbind(b$itemDim, b$itemDim[b$itemDim[["注意力"]] == 1L, ][1, ])
  b$itemDim$itemID[nrow(b$itemDim)] <- "ghost01"
  rd2 <- dim_report_data(b, "M036小")
  expect_false("注意力" %in% rd2$problems$id[rd2$problems$type == "no_reliability"])
  expect_true("ghost01 v1" %in% rd2$problems$id[rd2$problems$type == "missing_item"])
  att <- Filter(function(d) d$dimID == "注意力", rd2$dims)[[1]]
  expect_equal(att$reliability, 0.9)
  expect_false("ghost01" %in% att$items$itemID)

  expect_error(dim_report_data(b, "M999"), "沒有波次")
  expect_null(rd$scope)
})

test_that("dim_report_data can report one dimension only", {
  b <- .bank("parent")
  rd <- dim_report_data(b, "M036大", domain = "parent", dims = "3C產品環境")
  expect_identical(rd$scope, "3C產品環境")
  expect_length(rd$dims, 1L)
  expect_identical(rd$dims[[1]]$dimID, "3C產品環境")
  expect_equal(rd$n_items, nrow(rd$dims[[1]]$items))
  # 要注意的地方只談這個構面
  expect_true(all(rd$problems$id == "3C產品環境"))
  expect_false("整體" %in% rd$problems$id)
  # 這個波次沒有這個構面的題目
  rd0 <- dim_report_data(b, "M156大", dims = "注意力")
  expect_length(rd0$dims, 0L)
  expect_identical(rd0$not_in_wave, "注意力")
  expect_error(dim_report_data(b, "M036大", dims = "沒有這個構面"), "沒有構面")
})

test_that("dim_overlap lists the other dimensions sharing items", {
  b <- .bank("parent")
  ol <- dim_overlap(b, "親友親職參與")
  expect_named(ol, c("dimID", "dimName", "n_shared", "share", "n_items"))
  expect_gt(nrow(ol), 0L)
  expect_false("親友親職參與" %in% ol$dimID)
  expect_true(all(ol$n_shared >= 1L & ol$n_shared <= 10L))
  expect_equal(ol$share, round(100 * ol$n_shared / 10, 1))
  expect_true(all(diff(ol$n_shared) <= 0))
  # 自己算一次第一列
  id <- b$itemDim
  mine <- unique(id$itemID[id[["親友親職參與"]] == 1L])
  other <- unique(id$itemID[id[[ol$dimID[1]]] == 1L])
  expect_equal(ol$n_shared[1], length(intersect(mine, other)))
  expect_equal(ol$n_items[1], length(other))
  expect_equal(nrow(dim_overlap(b, "注意力")), 0L)
  expect_error(dim_overlap(b, "沒有這個構面"), "沒有構面")
})

test_that("dim_network draws one dimension, its items, and optionally the other dimensions", {
  b <- .bank("parent")
  n <- dim_network(b, "注意力")
  expect_equal(n$n_items, length(unique(b$itemDim$itemID[b$itemDim[["注意力"]] == 1L])))
  expect_equal(sum(startsWith(n$nodes$id, "D::")), 1L)
  expect_equal(nrow(n$edges), n$n_shown)
  expect_true(all(n$nodes$color.border == "#141414"))
  expect_equal(n$nodes$color.background[1], kitDesign::kit_colors()$accent)

  big <- dim_network(b, "孩子與家庭的基本資料", other_dims = TRUE)
  expect_equal(big$n_shown, 150L)
  expect_equal(big$n_omitted, big$n_items - 150L)
  expect_gt(big$n_multi, 0L)
  items <- big$nodes[startsWith(big$nodes$id, "I::"), ]
  expect_equal(sum(items$color.background == "#ffde59"), big$n_multi)
  expect_equal(sum(startsWith(big$nodes$id, "D::")), 1L + big$n_other)
  expect_match(dim_network_hint(big), "只畫前 150 題")
  expect_match(dim_network_hint(big), "黃色圓點")

  expect_identical(net_parse_id(items$id[1])$kind, "item")
  expect_identical(net_parse_id("D::注意力"), list(kind = "dim", dimID = "注意力"))
  # 同一 itemID 的版本合成一個節點，點下去是最新的版本；版本列在 tooltip
  cg <- dim_network(b, "親友親職參與")
  it <- cg$nodes[startsWith(cg$nodes$id, "I::"), ]
  expect_equal(nrow(it), 10L)
  expect_false(anyDuplicated(it$label) > 0L)
  expect_identical(net_parse_id(it$id[it$label == "caregiverf01"]),
                   list(kind = "item", itemID = "caregiverf01", itemVer = 3L))
  expect_match(it$title[it$label == "caregiverf01"], "3 個版本")
  expect_match(it$title[it$label == "caregiverf01"], "v1：.*v2：.*v3：")
  small <- dim_network(b, "親友親職參與", max_items = 4L)
  expect_equal(small$n_shown, 4L)
  expect_equal(small$n_omitted, 6L)
  w <- dim_network_widget(n, click_input = "dims-net_click")
  expect_s3_class(w, "visNetwork")
})
