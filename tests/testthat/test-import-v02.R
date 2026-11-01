# tests/testthat/test-import-v02.R：0.2 題庫 → 0.3 kit 表（import_v02）

# 一個很小的 0.2 題庫（三個 domain），寫到暫存資料夾
v02_mini_bank <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  wr <- function(df, d, f) {
    dir.create(file.path(root, d), showWarnings = FALSE)
    readr::write_csv(df, file.path(root, d, f), na = "")
  }
  ib <- data.frame(
    itemID = c("ID", "a1", "a1", "a2", "a3", "a4"),
    itemVer = c(1, 1, 2, 1, 1, 1),
    deprecated = c(0, 1, 0, 1, 0, 0),
    # 原文：共用題幹 | 共用題幹
    stemID = c("", "\u5171\u7528\u984c\u5e79", "\u5171\u7528\u984c\u5e79", "", "", ""),
    # 原文：家庭問卷資料_教養 | 家庭問卷資料_教養 | 健康問卷資料 | 家庭問卷資料_教養
    block = c("", "\u5bb6\u5ead\u554f\u5377\u8cc7\u6599_\u6559\u990a", "\u5bb6\u5ead\u554f\u5377\u8cc7\u6599_\u6559\u990a", "\u5065\u5eb7\u554f\u5377\u8cc7\u6599", "", "\u5bb6\u5ead\u554f\u5377\u8cc7\u6599_\u6559\u990a"),
    itemType = c("AD", "MC", "MC", "", "", "MC"),
    # 原文：家長
    respondent = "\u5bb6\u9577", reverseScored = c(0, 0, 0, 0, 0, 1),
    # 原文：編號 | 題一 | 題一改 | 題二 | 題三 | 題四
    label = c("\u7de8\u865f", "\u984c\u4e00", "\u984c\u4e00\u6539", "\u984c\u4e8c", "\u984c\u4e09", "\u984c\u56db"),
    # 原文：照錄文字 | 1 不同意;\n2 同意; | 1:不同意;\n2:同意; | 1~3點量表
    value = c("\u7167\u9304\u6587\u5b57", "1 \u4e0d\u540c\u610f;\n2 \u540c\u610f;", "1:\u4e0d\u540c\u610f;\n2:\u540c\u610f;", "1~3\u9ede\u91cf\u8868",
              # 原文：照錄數字 | 1 不同意;\n2 同意；有時;
              "\u7167\u9304\u6578\u5b57", "1 \u4e0d\u540c\u610f;\n2 \u540c\u610f\uff1b\u6709\u6642;"),
    # 原文：拒答:7777; 遺漏值:9999 | 怪怪的
    noResponse = c("", "\u62d2\u7b54:7777; \u907a\u6f0f\u503c:9999", "", "", "", "\u602a\u602a\u7684"),
    # 原文：v1→v2
    note = c("", "", "v1\u2192v2", "", "", ""),
    stringsAsFactors = FALSE)
  iw <- data.frame(itemID = ib$itemID, itemVer = ib$itemVer,
                   stats::setNames(data.frame(c(1, 1, 0, 1, 0, 1), c(0, 0, 1, 0, 1, 0)),
                                   # 原文：M036大 | 小2_小月齡
                                   c("M036\u5927", "\u5c0f2_\u5c0f\u6708\u9f61")), check.names = FALSE)
  idim <- data.frame(itemID = ib$itemID, itemVer = ib$itemVer,
                     stats::setNames(data.frame(c(0, 1, 0, 0, 0, 1), c(0, 0, 1, 0, 0, 0),
                                                c(0, 0, 0, 0, 1, 0), c(0, 0, 0, 0, 1, 0),
                                                c(0, 1, 0, 0, 0, 0), c(0, 0, 0, 0, 1, 0)),
                                     # 原文：教養 | 教養_恥感 | X_一 | X_二 | 反向題 | 專注題
                                     c("\u6559\u990a", "\u6559\u990a_\u6065\u611f", "X_\u4e00", "X_\u4e8c", "\u53cd\u5411\u984c", "\u5c08\u6ce8\u984c")),
                     check.names = FALSE)
  dm <- data.frame(dimID = names(idim)[-(1:2)], dimName = names(idim)[-(1:2)], alias = "",
                   reliability = "", note = "")
  for (d in c("parent", "child_adolescent", "childcare_teacher")) {
    wr(ib, d, "itemBank.csv"); wr(iw, d, "itemWave.csv")
    wr(idim, d, "itemDim.csv"); wr(dm, d, "dimMeta.csv")
  }
  writeLines("2026-01-02", file.path(root, "BANK_VERSION"))
  root
}

test_that("v02_parse_value handles codes, ranges, text entry and bad text", {
  # 原文：1 完全不會;\n2 不太熟練；有時會;\n3:普通;
  p <- v02_parse_value("1 \u5b8c\u5168\u4e0d\u6703;\n2 \u4e0d\u592a\u719f\u7df4\uff1b\u6709\u6642\u6703;\n3:\u666e\u901a;")
  expect_equal(p$kind, "codes")
  expect_equal(p$codes$code, c("1", "2", "3"))
  # 原文：完全不會 | 不太熟練；有時會 | 普通
  expect_equal(p$codes$label, c("\u5b8c\u5168\u4e0d\u6703", "\u4e0d\u592a\u719f\u7df4\uff1b\u6709\u6642\u6703", "\u666e\u901a"))
  # 原文：0 ： 0個月; | 0個月
  expect_equal(v02_parse_value("0 \uff1a 0\u500b\u6708;")$codes$label, "0\u500b\u6708")
  # 原文：1~5點量表
  r <- v02_parse_value("1~5\u9ede\u91cf\u8868")
  expect_equal(r$kind, "range")
  expect_equal(r$codes$code, as.character(1:5))
  # 原文：照錄文字
  expect_equal(v02_parse_value("\u7167\u9304\u6587\u5b57")$kind, "text")
  expect_equal(v02_parse_value("")$kind, "blank")
  # 原文：1 是;\n否
  expect_equal(v02_parse_value("1 \u662f;\n\u5426")$kind, "bad")
  # 原文：1 是;\n1 否
  expect_equal(v02_parse_value("1 \u662f;\n1 \u5426")$kind, "bad")
})

test_that("v02_parse_noresponse splits label:code pairs", {
  # 原文：不知道:8888; 拒答:7777; 怪
  p <- v02_parse_noresponse("\u4e0d\u77e5\u9053:8888; \u62d2\u7b54:7777; \u602a")
  expect_equal(p$code[p$ok], c("8888", "7777"))
  # 原文：不知道 | 拒答
  expect_equal(p$label[p$ok], c("\u4e0d\u77e5\u9053", "\u62d2\u7b54"))
  expect_false(p$ok[3])
})

test_that("v02_age_module separates preschool and school waves", {
  # 原文：M036大 | M012保母
  expect_equal(v02_age_module(c("M036\u5927", "M012\u4fdd\u6bcd")), "childcare")
  # 原文：小2_小月齡
  expect_equal(v02_age_module(c("M084", "\u5c0f2_\u5c0f\u6708\u9f61")), "school")
  # 原文：M060機構
  expect_equal(v02_age_module(c("M060\u6a5f\u69cb", "M084")), "")
})

test_that("import_v02 builds kit tables from a small bank", {
  res <- import_v02(v02_mini_bank())
  it <- res$items
  expect_setequal(it$item_uid, paste0(rep(c("P.", "C.", "T."), each = 5),
                                      c("ID", "a1", "a2", "a3", "a4")))
  p <- it[it$instrument == "P", ]
  # 只有每一版都停用才算停用
  expect_equal(p$deprecated[p$varname == "a1"], "0")
  expect_equal(p$deprecated[p$varname == "a2"], "1")
  expect_equal(p$role[p$varname == "ID"], "admin")
  expect_equal(p$module[p$varname == "a1"], "family")
  expect_equal(p$module[p$varname == "a2"], "health")
  # 從「專注題」構面判斷
  expect_equal(p$role[p$varname == "a3"], "admin")

  iv <- res$item_versions[startsWith(res$item_versions$item_uid, "P."), ]
  # 原文：共用題幹 | 共用題幹
  expect_equal(iv$stem[iv$item_uid == "P.a1"], c("\u5171\u7528\u984c\u5e79", "\u5171\u7528\u984c\u5e79"))
  # 「1 不同意」和「1:不同意」是同一組選項
  expect_equal(length(unique(iv$valueSetID[iv$item_uid == "P.a1"])), 1L)
  expect_equal(iv$itemType[iv$item_uid == "P.a2"], "MC")
  expect_equal(iv$itemType[iv$item_uid == "P.a3"], "NR")
  expect_equal(iv$valueSetID[iv$item_uid == "P.ID"], "")
  expect_true(all(grepl("^vs[0-9]{4}$", unique(res$value_codes$valueSetID))))

  # 構面：旗標不是題組；反向題變成成員的 reverse
  # 原文：P.反向題 | P.專注題
  expect_false(any(c("P.\u53cd\u5411\u984c", "P.\u5c08\u6ce8\u984c") %in% res$groups$groupID))
  mem <- res$group_members_v02
  # 原文：P.教養
  expect_equal(mem$reverse[mem$groupID == "P.\u6559\u990a" & mem$item_uid == "P.a1"], "1")
  # 原文：P.教養
  expect_equal(mem$reverse[mem$groupID == "P.\u6559\u990a" & mem$item_uid == "P.a4"], "1")
  # 原文：P.教養
  expect_true("P.\u6559\u990a" %in% mem$groupID[mem$item_uid == "P.a1"])
  # 上下層：教養_恥感 → 教養（已存在）；X_一、X_二 → 新增 X
  e <- res$group_edges
  # 原文：P.教養_恥感 | P.教養
  expect_equal(e$parentID[e$groupID == "P.\u6559\u990a_\u6065\u611f"], "P.\u6559\u990a")
  # 原文：P.X_一
  expect_equal(e$parentID[e$groupID == "P.X_\u4e00"], "P.X")
  expect_true("P.X" %in% res$groups$groupID)

  w <- res$item_waves
  # 原文：M036大 | 小2_小月齡
  expect_setequal(w$wave_col[w$item_uid == "P.a1"], c("M036\u5927", "\u5c0f2_\u5c0f\u6708\u9f61"))
  expect_true("noresponse_unparsed" %in% res$issues$check)
  expect_true("value_no_labels" %in% res$issues$check)
  expect_equal(res$sources$version, "2026-01-02")

  kit <- replace(empty_kit(), V02_KIT_TABLES, res[V02_KIT_TABLES])
  expect_equal(nrow(check_kit(kit)), 0L)
})

test_that("import_v02 on the built-in bank passes check_kit", {
  res <- import_v02()
  kit <- replace(empty_kit(), V02_KIT_TABLES, res[V02_KIT_TABLES])
  expect_equal(nrow(check_kit(kit)), 0L)
  expect_equal(nrow(res$items), length(unique(res$item_versions$item_uid)))
  expect_true(all(res$group_members_v02$item_uid %in% res$items$item_uid))
  expect_true(all(res$group_members_v02$groupID %in% res$groups$groupID))
  expect_true(all(c(res$group_edges$groupID, res$group_edges$parentID) %in% res$groups$groupID))
})
