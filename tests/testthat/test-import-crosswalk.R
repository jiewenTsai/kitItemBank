# tests/testthat/test-import-crosswalk.R — 步驟 1.4：跨波次對照表 → 位置、題組、關係候選

cw_fixture_kit <- function() {
  kit <- empty_kit()
  df <- function(...) data.frame(..., stringsAsFactors = FALSE)
  kit$cohorts <- df(cohortID = "K1", code = "M03")
  kit$admins <- df(adminID = c("A1", "A2"), cohortID = "K1", ageMonth = c("1", "2"),
                   status = "released", surveyFrom = c("2019-03-01", "2020-03-01"))
  kit$forms <- df(formID = c("P-03-24", "P-G1-G3"), instrument = "P", label = c("f1", "f2"))
  kit$datasets <- df(datasetID = c("A1-P", "A2-P"), adminID = c("A1", "A2"), file = "data1",
                     # 原文："家長"
                     instrument = "P", formID = c("P-03-24", "P-G1-G3"), respondent = "\u5bb6\u9577")
  kit$aliases <- df(alias = c("X_1M", "X_2M"), target_type = "admin", target_id = c("A1", "A2"),
                    kind = "crosswalk_col")
  kit$items <- df(item_uid = c("P.v1", "P.v2", "P.v3"), instrument = "P",
                  varname = c("v1", "v2", "v3"))
  kit$item_versions <- df(item_uid = c("P.v1", "P.v2", "P.v3", "P.v3"), itemVer = c("1", "1", "1", "2"))
  kit$dataset_vars <- df(
    datasetID = c("A1-P", "A1-P", "A1-P", "A2-P", "A2-P", "A2-P"),
    varname = c("v1", "v2", "v3", "v1n", "v2", "v3"),
    item_uid = c("P.v1", "P.v2", "P.v3", "", "P.v2", "P.v3"),
    itemVer = c("1", "1", "1", "", "1", "2"), kind = "item")
  # 原文："P.甲" "P.乙" "甲" "乙"
  kit$groups <- df(groupID = c("P.\u7532", "P.\u4e59"), label = c("\u7532", "\u4e59"))
  # 原文："P.甲"
  kit$group_tree <- df(formID = "P-03-24", groupID = "P.\u7532", parentID = "")
  kit$group_members <- df(formID = c("P-03-24", "P-03-24", "P-G1-G3"),
                          # 原文："P.甲" "P.甲" "P.甲"
                          groupID = c("P.\u7532", "P.\u7532", "P.\u7532"),
                          item_uid = c("P.v1", "P.v2", "P.v2"), scheme = "official")
  kit
}

cw_fixture_sheet <- function() {
  rows <- list(
    # 原文："變項名稱" "主構面" "次構面" "變項標籤" "備註：" "反向題"
    c("\u8b8a\u9805\u540d\u7a31", "\u4e3b\u69cb\u9762", "\u6b21\u69cb\u9762", "\u8b8a\u9805\u6a19\u7c64", "X_\r\n1M", "X_\r\n2M", "\u5099\u8a3b\uff1a", "\u53cd\u5411\u984c"),
    # 原文："區塊" "甲" "父親-陪這孩子玩。"
    c("v1", "\u5340\u584a", "\u7532", "\u7236\u89aa-\u966a\u9019\u5b69\u5b50\u73a9\u3002", "O", "X", NA, NA),
    # 原文："區塊" "甲" "父親-陪這孩子玩耍。"
    c("v1n", "\u5340\u584a", "\u7532", "\u7236\u89aa-\u966a\u9019\u5b69\u5b50\u73a9\u800d\u3002", "X", "O", NA, NA),
    # 原文："區塊" "甲/G1-G3乙" "題二"
    c("v2", "\u5340\u584a", "\u7532/G1-G3\u4e59", "\u984c\u4e8c", "O", "O", NA, NA),
    # 原文："區塊" "丙(僅1-2月齡)" "題三"
    c("v3", "\u5340\u584a", "\u4e19(\u50c51-2\u6708\u9f61)", "\u984c\u4e09", "O", "O",
      # 原文："2019.04 由第2波2M開始，題目文字新增「最常」" "反向題"
      "2019.04 \u7531\u7b2c2\u6ce22M\u958b\u59cb\uff0c\u984c\u76ee\u6587\u5b57\u65b0\u589e\u300c\u6700\u5e38\u300d", "\u53cd\u5411\u984c"))
  raw <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  # 原文："家長問卷"
  list(list(file = "cw_M03W02.xlsx", sheet = "\u5bb6\u9577\u554f\u5377", raw = raw))
}

cw_run_fixture <- function(usage = NULL, sav_meta = NULL) {
  crosswalk_from_sheets(cw_fixture_sheet(), cw_fixture_kit(), usage, sav_meta)
}

test_that("crosswalk sheet becomes rows and row x wave cells", {
  res <- cw_run_fixture()
  expect_identical(res$cw$varname, c("v1", "v1n", "v2", "v3"))
  expect_identical(res$cw$pattern, c("OX", "XO", "OO", "OO"))
  expect_identical(res$cw$item_exists, c("1", "0", "1", "1"))
  expect_identical(res$cw$reverse[4], "1")
  expect_identical(unique(res$cw_long$datasetID), c("A1-P", "A2-P"))
  expect_identical(nrow(res$cw_long), 8L)
})

test_that("cw_split_path splits on _ and handles / alternatives", {
  # 原文："發展問卷資料_社會情緒" "氣質_外放性"
  p <- cw_split_path("\u767c\u5c55\u554f\u5377\u8cc7\u6599_\u793e\u6703\u60c5\u7dd2", "\u6c23\u8cea_\u5916\u653e\u6027")
  # 原文："發展問卷資料" "社會情緒" "氣質" "外放性"
  expect_identical(p[[1]]$segs, c("\u767c\u5c55\u554f\u5377\u8cc7\u6599", "\u793e\u6703\u60c5\u7dd2", "\u6c23\u8cea", "\u5916\u653e\u6027"))
  # 和主構面重複的不切
  # 原文："家庭問卷資料_親職參與" "親友女_親職參與"
  p <- cw_split_path("\u5bb6\u5ead\u554f\u5377\u8cc7\u6599_\u89aa\u8077\u53c3\u8207", "\u89aa\u53cb\u5973_\u89aa\u8077\u53c3\u8207")
  # 原文："家庭問卷資料" "親職參與" "親友女_親職參與"
  expect_identical(p[[1]]$segs, c("\u5bb6\u5ead\u554f\u5377\u8cc7\u6599", "\u89aa\u8077\u53c3\u8207", "\u89aa\u53cb\u5973_\u89aa\u8077\u53c3\u8207"))
  # 原文："甲" "乙/G1-G3丙"
  p <- cw_split_path("\u7532", "\u4e59/G1-G3\u4e19")
  expect_identical(vapply(p, function(a) a$tag, ""), c("", "G1-G3"))
  # 原文："甲" "丙"
  expect_identical(p[[2]]$segs, c("\u7532", "\u4e19"))
  # 原文："甲/乙" "丙/丁"
  p <- cw_split_path("\u7532/\u4e59", "\u4e19/\u4e01")
  expect_identical(vapply(p, function(a) a$kind, ""), c("dual", "dual"))
  # 原文："甲" "注意力/執行功能"
  p <- cw_split_path("\u7532", "\u6ce8\u610f\u529b/\u57f7\u884c\u529f\u80fd")
  expect_identical(length(p), 1L)
  # 原文："甲" "丙(僅36-72月齡)" "36-72月齡"
  expect_identical(cw_split_path("\u7532", "\u4e19(\u50c536-72\u6708\u9f61)")[[1]]$age, "36-72\u6708\u9f61")
})

test_that("group map, tree and lineage candidates", {
  res <- cw_run_fixture()
  gm <- res$group_map
  # 原文："區塊 > 甲" "P.甲"
  expect_identical(gm$groupID[gm$cw_path == "\u5340\u584a > \u7532"], "P.\u7532")
  # 原文："區塊 > 甲"
  expect_identical(gm$method[gm$cw_path == "\u5340\u584a > \u7532"], "label_exact")
  # 原文："區塊 > 丙" "P.丙"
  expect_identical(gm$proposed_groupID[gm$cw_path == "\u5340\u584a > \u4e19"], "P.\u4e19")
  # 原文："區塊 > 丙"
  expect_true("\u5340\u584a > \u4e19" %in% res$group_unmatched$cw_path)
  gp <- res$group_paths
  expect_true(all(c("formID", "groupID", "parentID", "source", "confidence", "basis") %in% names(gp)))
  # 原文："P.甲" "P.區塊"
  expect_identical(gp$parentID[gp$formID == "P-03-24" & gp$groupID == "P.\u7532"], "P.\u5340\u584a")
  expect_length(group_tree_cycles(gp), 0L)
  gl <- res$group_lineage
  expect_identical(nrow(gl), 1L)
  # 原文："P.甲" "P.乙"
  expect_identical(c(gl$from_group, gl$to_group, gl$formID), c("P.\u7532", "P.\u4e59", "P-G1-G3"))
  expect_identical(gl$confidence, "high")
})

test_that("notes and relation hints", {
  res <- cw_run_fixture()
  n <- res$notes
  expect_identical(n$type, "wording")
  expect_identical(n$effective_admin, "A2")
  rh <- res$relation_hints
  ren <- rh[rh$kind == "renamed", ]
  expect_identical(c(ren$from_uid, ren$from_ver, ren$to_uid, ren$to_ver),
                   c("P.v1", "1", "P.v1n", "1"))
  expect_identical(ren$to_exists, "0")
  expect_identical(ren$effective_admin, "A2")
  rev <- rh[rh$kind == "revised", ]
  expect_identical(c(rev$from_uid, rev$from_ver, rev$to_ver), c("P.v3", "1", "2"))
  expect_true(all(c("confidence", "basis") %in% names(rh)))
})

test_that("member hints: new varname inherits the renamed item's group", {
  res <- cw_run_fixture()
  mh <- res$member_hints
  h <- mh[mh$item_uid == "P.v1n", ]
  # 原文："P.甲"
  expect_identical(h$groupID, "P.\u7532")
  expect_identical(h$formID, "P-G1-G3")
  expect_identical(h$confidence, "high")
  expect_match(h$source, "renamed_from:P.v1")
  # v3 沒有題組：提新題組，low
  expect_identical(mh$confidence[mh$item_uid == "P.v3"][1], "low")
  # 已在 group_members 的不列
  # 原文："P.甲"
  expect_false(any(mh$item_uid == "P.v1" & mh$groupID == "P.\u7532" & mh$formID == "P-03-24"))
})

test_that("positions come from .sav labels and usage docs", {
  usage <- list("us_M03W02.txt" = c(
    # 原文："| 家長 第一部份 一、4.3 | 家長 | v3 | 105 | 1 |"
    "| \u5bb6\u9577 \u7b2c\u4e00\u90e8\u4efd \u4e00\u30014.3 | \u5bb6\u9577 | v3 | 105 | 1 |",
    # 原文："| 家長 第三部分 二、5,7,8 | 家長 | v2 | 0 | 1 |"
    "| \u5bb6\u9577 \u7b2c\u4e09\u90e8\u5206 \u4e8c\u30015,7,8 | \u5bb6\u9577 | v2 | 0 | 1 |",
    # 原文："| 家長 第二部分 一、9 | 家長 | v1n | 0 | 1 |"
    "| \u5bb6\u9577 \u7b2c\u4e8c\u90e8\u5206 \u4e00\u30019 | \u5bb6\u9577 | v1n | 0 | 1 |"))
  sav <- list("A2-P" = data.frame(var = c("v2", "v3", "v1n"),
                                  # 原文："二.1.題二" "一.4.3.題三" "沒有題號"
                                  label = c("\u4e8c.1.\u984c\u4e8c", "\u4e00.4.3.\u984c\u4e09", "\u6c92\u6709\u984c\u865f"),
                                  stringsAsFactors = FALSE))
  res <- cw_run_fixture(usage, sav)
  p <- res$positions
  # 原文："二.1"
  expect_identical(p$position[p$varname == "v2"], "\u4e8c.1")
  expect_identical(p$confidence[p$varname == "v3"], "high")
  # 原文："第一部分"
  expect_identical(p$part[p$varname == "v3"], "\u7b2c\u4e00\u90e8\u5206")
  # 原文："一.9"
  expect_identical(p$position[p$varname == "v1n"], "\u4e00.9")
  expect_identical(p$source[p$varname == "v1n"], "usage_doc")
  cov <- res$coverage
  expect_identical(cov$n_position[cov$datasetID == "A2-P"], 3L)
  kit <- cw_fixture_kit()
  i <- match(paste(p$datasetID, p$varname), paste(kit$dataset_vars$datasetID, kit$dataset_vars$varname))
  kit$dataset_vars$position[i] <- p$position
  expect_false(any(check_kit(kit)$column == "position"))
})
