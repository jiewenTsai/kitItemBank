# tests/testthat/test-import-sav.R — 步驟 1.3：.sav 表頭轉 dataset_vars

sav_fixture <- function() {
  data.frame(
    var = c("release_id", "baby_sex", "cogc01", "fatherinvo01n", "w_post_sel", "weight",
            "cog_eap", "pfx01"),
    class = c("character", rep("haven_labelled", 7)),
    # 原文：幼兒編號、幼兒性別、一.1.玩扮演遊戲、四.1.照顧、事後分層法加權值、體重、認知能力估計值、二.1.收入
    label = c("\u5e7c\u5152\u7de8\u865f", "\u5e7c\u5152\u6027\u5225",
              "\u4e00.1.\u73a9\u626e\u6f14\u904a\u6232", "\u56db.1.\u7167\u9867",
              "\u4e8b\u5f8c\u5206\u5c64\u6cd5\u52a0\u6b0a\u503c", "\u9ad4\u91cd",
              "\u8a8d\u77e5\u80fd\u529b\u4f30\u8a08\u503c", "\u4e8c.1.\u6536\u5165"),
    n_vlab = c("0", "2", "4", "0", "0", "0", "1", "3"),
    # 原文：1=男; 2=女 / 1=完全不能; 2=勉強能做; 7777=拒答; 9999=遺漏值 / 9999=遺漏值 / 1=低; 2=高[跳答第4題]; 3=中
    vlabs = c("", "1=\u7537; 2=\u5973",
              "1=\u5b8c\u5168\u4e0d\u80fd; 2=\u52c9\u5f37\u80fd\u505a; 7777=\u62d2\u7b54; 9999=\u907a\u6f0f\u503c",
              "", "", "", "9999=\u907a\u6f0f\u503c",
              "1=\u4f4e; 2=\u9ad8[\u8df3\u7b54\u7b2c4\u984c]; 3=\u4e2d"),
    na = c("", "", "", "", "", "", "", "3,range 8-9"),
    stringsAsFactors = FALSE)
}

test_that("read_sav_meta reads all columns as character", {
  f <- tempfile(fileext = ".csv")
  readr::write_csv(sav_fixture(), f)
  m <- read_sav_meta(f)
  expect_identical(names(m), c("var", "class", "label", "n_vlab", "vlabs", "na"))
  expect_true(all(vapply(m, is.character, logical(1))))
  expect_identical(m$na[1], "")
})

test_that("import_sav_header fills dataset_vars with item_uid and kind", {
  items <- data.frame(item_uid = c("P.cogC01", "P.relationship", "P.pfx01"),
                      role = c("", "admin", ""), stringsAsFactors = FALSE)
  d <- import_sav_header(sav_fixture(), "M36M084G01-P", "P", items)
  expect_identical(names(d), names(KIT_SCHEMA$dataset_vars$cols))
  expect_identical(d$order, as.character(1:8))
  # 不分大小寫
  expect_identical(d$item_uid[d$varname == "cogc01"], "P.cogC01")
  expect_identical(d$item_uid[d$varname == "fatherinvo01n"], "")
  expect_identical(d$kind, c("system", "system", "item", "item", "weight", "item",
                             "derived", "item"))
  expect_true(all(d$itemVer == "" & d$position == ""))
  kit <- empty_kit()
  kit$dataset_vars <- d
  # 只檢查 enum 和必填：外鍵（datasets／items）這裡沒有，不看
  iss <- check_kit(kit)
  expect_false(any(iss$check %in% c("bad_value", "required_blank", "bad_type") & iss$table == "dataset_vars"))
})

test_that("instrument S makes everything but ids derived; C tries the C prefix", {
  d <- import_sav_header(sav_fixture()[c(1, 7), ], "X-S", "S")
  expect_identical(d$kind, c("system", "derived"))
  items <- data.frame(item_uid = "C.CcogC01", stringsAsFactors = FALSE)
  d <- import_sav_header(sav_fixture()[3, ], "X-C", "C", items)
  expect_identical(d$item_uid, "C.CcogC01")
})

test_that("sav_value_sets flags missing codes by na, special code and label", {
  vs <- sav_value_sets(sav_fixture())
  expect_identical(names(vs), c("varname", "code", "label", "is_missing", "miss_kind",
                                "miss_source"))
  c1 <- vs[vs$varname == "cogc01", ]
  expect_identical(c1$code, c("1", "2", "7777", "9999"))
  expect_identical(c1$is_missing, c("0", "0", "1", "1"))
  expect_identical(c1$miss_kind[3:4], c("refused", "missing"))
  p <- vs[vs$varname == "pfx01", ]
  # 3 由 na 宣告；"[跳答第4題]" 是跳題指示，不算遺漏；range 8-9 沒有標籤，不會多出列
  expect_identical(p$code, c("1", "2", "3"))
  expect_identical(p$is_missing, c("0", "0", "1"))
  expect_identical(p$miss_source[3], "na")
})

test_that("compare_sav_v02 reports coverage, expected items, code and missing mismatches", {
  bank <- list(
    itemBank = data.frame(
      itemID = c("cogC01", "fatherinvo01", "pfx01"), itemVer = c("1", "1", "1"),
      deprecated = "0",
      # 原文：1 完全不能;\n2 勉強能做; / 1 有;\n2 沒有; / 1 低;\n2 高;\n4 很高;
      value = c("1 \u5b8c\u5168\u4e0d\u80fd;\n2 \u52c9\u5f37\u80fd\u505a;", "1 \u6709;\n2 \u6c92\u6709;",
                "1 \u4f4e;\n2 \u9ad8;\n4 \u5f88\u9ad8;"),
      # 原文：拒答:5; 遺漏值:9999
      noResponse = c("\u62d2\u7b54:5; \u907a\u6f0f\u503c:9999", "", ""),
      itemType = "MC", stringsAsFactors = FALSE),
    itemWave = data.frame(itemID = c("cogC01", "fatherinvo01", "pfx01"),
                          itemVer = c("1", "1", "1"), W1 = c(1, 1, 0),
                          stringsAsFactors = FALSE))
  iss <- compare_sav_v02(sav_fixture(), "P", bank, wave_col = "W1")
  expect_identical(iss$varname[iss$issue == "sav_no_v02"], c("fatherinvo01n", "weight"))
  expect_identical(iss$item_uid[iss$issue == "v02_no_sav"], "P.fatherinvo01")
  expect_identical(iss$varname[iss$issue == "noresponse_not_in_sav"], "cogc01")
  expect_match(iss$detail[iss$issue == "noresponse_not_in_sav"], "^v02_nonstandard")
  expect_identical(iss$varname[iss$issue == "value_codes_differ"], "pfx01")
  expect_match(iss$detail[iss$issue == "value_codes_differ"], "^sav_subset")
  expect_identical(nrow(compare_sav_v02(sav_fixture(), "S", bank)), 0L)
})

test_that("import_sav_dir runs a map over a folder", {
  dir <- tempfile()
  dir.create(file.path(dir, "w1"), recursive = TRUE)
  readr::write_csv(sav_fixture(), file.path(dir, "w1", "data1_meta.csv"))
  map <- data.frame(wave_dir = "w1", file = c("data1", "nofile"),
                    datasetID = c("A1-P", "A1-T"), instrument = c("P", "T"),
                    stringsAsFactors = FALSE)
  items <- data.frame(item_uid = "P.cogC01", stringsAsFactors = FALSE)
  res <- suppressMessages(import_sav_dir(dir, map, banks = list(), items = items))
  expect_identical(res$summary$n_vars, 8L)
  expect_identical(res$summary$matched, 1L)
  expect_identical(unique(res$dataset_vars$datasetID), "A1-P")
  expect_identical(res$datasets$adminID, c("A1", "A1"))
})
