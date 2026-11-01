# stats_items.R、stats_groups.R、stats_evidence.R：證據層統計（只用小的模擬資料）

# 假題庫：一個資料檔（方案 2016），題本 F1 有兩個正式題組
#   G.a：a1..a4（a3 反向），選項 1–5
#   G.b：b1、b2，選項 1–4
# 另外 c1 是沒有選項的連續變項、t1 是文字變項
stats_fake_kit <- function() {
  kit <- empty_kit()
  kit$cohorts <- conform_kit_table(data.frame(cohortID = "K1", code = "M01"), "cohorts")
  kit$admins <- conform_kit_table(data.frame(adminID = "A1", cohortID = "K1", ageMonth = "36",
                                             status = "internal"), "admins")
  kit$forms <- conform_kit_table(data.frame(formID = "F1", instrument = "P", label = "F1"), "forms")
  kit$datasets <- conform_kit_table(data.frame(datasetID = "A1-P", adminID = "A1", file = "data1",
                                               instrument = "P", formID = "F1", respondent = "P",
                                               missingScheme = "2016"), "datasets")
  vars <- c("a1", "a2", "a3", "a4", "b1", "b2", "c1", "t1")
  uid <- paste0("P.", vars)
  kit$items <- conform_kit_table(data.frame(item_uid = uid, instrument = "P", varname = vars), "items")
  kit$item_versions <- conform_kit_table(data.frame(
    item_uid = uid, itemVer = "1",
    valueSetID = c("vs5", "vs5", "vs5", "vs5", "vs4", "vs4", "", "")), "item_versions")
  kit$value_codes <- conform_kit_table(data.frame(
    valueSetID = c(rep("vs5", 5), rep("vs4", 4)), code = c(1:5, 1:4)), "value_codes")
  kit$missing_schemes <- conform_kit_table(data.frame(
    scheme = "2016", code = c("7777", "8888", "9996", "9999"),
    kind = c("refused", "dont_know", "not_applicable", "missing")), "missing_schemes")
  kit$dataset_vars <- conform_kit_table(data.frame(
    datasetID = "A1-P", varname = c("ID", toupper(vars[1]), vars[-1]),
    item_uid = c("", uid), itemVer = c("", rep("1", 8)),
    kind = c("system", rep("item", 8)), order = as.character(1:9)), "dataset_vars")
  kit$groups <- conform_kit_table(data.frame(groupID = c("G.a", "G.b"), label = c("A", "B")), "groups")
  kit$group_members <- conform_kit_table(data.frame(
    formID = "F1", groupID = c(rep("G.a", 4), rep("G.b", 2)), item_uid = uid[1:6],
    scheme = "official", reverse = c("0", "0", "1", "0", "0", "0")), "group_members")
  kit
}

# 模擬資料：200 人；a3 是反向題（資料裡負相關）；加一些遺漏代碼
stats_fake_data <- function(n = 200) {
  set.seed(20261102)
  f <- stats::rnorm(n)
  cat5 <- function(l) pmin(pmax(round(3 + l * f + stats::rnorm(n, sd = .8)), 1), 5)
  cat4 <- function(l) pmin(pmax(round(2.5 + l * f + stats::rnorm(n, sd = .8)), 1), 4)
  d <- data.frame(id = seq_len(n), a1 = cat5(.9), a2 = cat5(.8), a3 = cat5(-.9), a4 = cat5(.7),
                  b1 = cat4(.8), b2 = cat4(.7), c1 = round(stats::rnorm(n, 50, 10), 1),
                  t1 = sample(c("x", "y"), n, TRUE), extra = 1)
  if (n < 30) return(d)
  d$a1[1:3] <- 7777      # 3 個拒答 → "<5"
  d$a1[4:15] <- 8888     # 12 個不知道
  d$a2[1] <- NA          # 1 個系統遺漏
  d$a4[c(20, 21)] <- 6   # 不在選項裡的值
  d
}

test_that("suppression helpers write 1-4 as <5", {
  expect_identical(stats_count_str(c(0, 1, 4, 5, 12, NA)), c("0", "<5", "<5", "5", "12", ""))
  expect_identical(stats_num_str(c(1, 2 / 3, NA)), c("1.000", "0.667", ""))
})

test_that("item_stats removes missing codes, counts them and suppresses small cells", {
  kit <- stats_fake_kit()
  d <- stats_fake_data()
  it <- item_stats(d, kit, "A1-P")
  # 大小寫不分：題庫寫 A1、資料寫 a1；系統變項和題庫沒有的欄位不算
  expect_setequal(it$varname, c("A1", "a2", "a3", "a4", "b1", "b2", "c1", "t1"))
  a1 <- it[it$varname == "A1", ]
  expect_identical(a1$n_total, "200")
  expect_identical(a1$n_valid, "185")
  expect_identical(a1$missing, "7777=<5; 8888=12; 9996=0; 9999=0")
  expect_identical(a1$miss_rate, stats_num_str(15 / 200))
  valid <- d$a1[!d$a1 %in% c(7777, 8888)]
  expect_identical(a1$mean, stats_num_str(mean(valid)))
  expect_identical(a1$sd, stats_num_str(stats::sd(valid)))
  expect_identical(a1$p50, stats_num_str(stats::median(valid)))
  expect_identical(a1$value_type, "coded")
  cnt <- vapply(1:5, function(k) sum(valid == k), integer(1))
  expect_identical(a1$freq, paste0(1:5, "=", stats_count_str(cnt), collapse = "; "))
  expect_identical(it$n_sysmis[it$varname == "a2"], "<5")
  expect_match(it$freq[it$varname == "a4"], "other=<5")
  expect_identical(it$value_type[it$varname == "c1"], "numeric")
  expect_identical(it$freq[it$varname == "c1"], "")
  t1 <- it[it$varname == "t1", ]
  expect_identical(t1$value_type, "text")
  expect_identical(c(t1$mean, t1$freq), c("", ""))
  # 每一個次數都不是 1 到 4
  counts <- unlist(strsplit(c(it$missing, it$freq), "; "))
  counts <- sub("^.*=", "", counts[nzchar(counts)])
  expect_false(any(counts %in% as.character(1:4)))
})

test_that("item_stats gives no mean/sd/percentiles with fewer than 10 valid values", {
  kit <- stats_fake_kit()
  d <- stats_fake_data(12)
  d$a2[1:4] <- 9999
  it <- item_stats(d, kit, "A1-P")
  a2 <- it[it$varname == "a2", ]
  expect_identical(a2$n_valid, "8")
  expect_identical(c(a2$mean, a2$sd, a2$p5, a2$p50, a2$p95), rep("", 5))
  expect_match(a2$note, "10")
  expect_true(nzchar(it$mean[it$varname == "b1"]))
})

test_that("2020 scheme codes are used for a 2020 data file", {
  kit <- stats_fake_kit()
  kit$missing_schemes <- conform_kit_table(data.frame(
    scheme = c(rep("2016", 4), rep("2020", 5)),
    code = c("7777", "8888", "9996", "9999", "7777", "8888", "9995", "9996", "9999")),
    "missing_schemes")
  kit$datasets$missingScheme <- "2020"
  d <- stats_fake_data()
  d$b1[1:6] <- 9995
  it <- item_stats(d, kit, "A1-P")
  expect_identical(it$missing[it$varname == "b1"], "7777=0; 8888=0; 9995=6; 9996=0; 9999=0")
  expect_identical(it$n_valid[it$varname == "b1"], "194")
})

test_that("omega_total matches a hand calculation", {
  # (0.7 * 4)^2 / ((0.7 * 4)^2 + 4 * (1 - 0.49)) = 7.84 / 9.88
  expect_equal(stats_omega_total(c(.7, .7, .7, .7)), 7.84 / 9.88)
  l <- c(.8, .6, .5)
  expect_equal(stats_omega_total(l), 1.9^2 / (1.9^2 + (1 - .64) + (1 - .36) + (1 - .25)))
})

test_that("reverse scoring uses max + min of the value codes", {
  expect_identical(stats_reverse(c(1, 2, 5, NA), 1:5), c(5, 4, 1, NA))
  expect_identical(stats_reverse(c(2, 3), numeric(0)), c(3, 2))
})

test_that("group_stats reverse-scores bank items and reports alpha, omega, loadings", {
  skip_if_not_installed("psych")
  kit <- stats_fake_kit()
  d <- stats_fake_data()
  gs <- group_stats(d, kit, "A1-P")
  expect_setequal(unique(gs$groupID), c("G.a", "G.b"))
  ga <- gs[gs$groupID == "G.a" & gs$varname == "", ]
  expect_identical(ga$n_items, "4")
  # 完整個案：a1 有 15 個遺漏代碼、a2 一個 NA（第 1 列，已經算在 a1 裡）
  expect_identical(ga$n, as.character(200 - 15))
  expect_true(nzchar(ga$alpha) && nzchar(ga$omega_total) && nzchar(ga$cfi))
  # 原文：只供參考
  expect_match(ga$note, "\u53ea\u4f9b\u53c3\u8003")
  items <- gs[gs$groupID == "G.a" & nzchar(gs$varname), ]
  expect_identical(items$bank_reverse, c("0", "0", "1", "0"))
  # 題庫已經把 a3 反向，psych 不必再反向；負荷量都是正的
  expect_identical(items$psych_reversed, rep("0", 4))
  expect_true(all(as.numeric(items$loading) > 0))

  # 照算：題庫反向後的 α 和 ω（ω 用 fa 的負荷量手算）
  x <- d[c("a1", "a2", "a3", "a4")]
  x[x == 7777 | x == 8888] <- NA
  x$a3 <- 6 - x$a3
  x <- x[stats::complete.cases(x), ]
  a <- suppressWarnings(psych::alpha(x, check.keys = TRUE, warnings = FALSE))
  expect_identical(ga$alpha, stats_num_str(a$total$raw_alpha))
  f <- suppressWarnings(psych::fa(x, nfactors = 1, cor = "poly"))
  l <- abs(as.numeric(f$loadings[, 1]))
  expect_identical(ga$omega_total, stats_num_str(sum(l)^2 / (sum(l)^2 + sum(1 - l^2))))
  expect_identical(ga$eigen_ratio, stats_num_str(f$e.values[1] / f$e.values[2]))

  # 兩題的題組：有 α，沒有因素分析和配適度
  gb <- gs[gs$groupID == "G.b" & gs$varname == "", ]
  expect_true(nzchar(gb$alpha))
  expect_identical(c(gb$omega_total, gb$cfi), c("", ""))
})

test_that("a wrong bank reverse flag shows up as psych_reversed", {
  skip_if_not_installed("psych")
  kit <- stats_fake_kit()
  kit$group_members$reverse <- "0"   # 題庫忘了標 a3 反向
  gs <- group_stats(stats_fake_data(), kit, "A1-P")
  it <- gs[gs$groupID == "G.a" & gs$varname == "a3", ]
  expect_identical(it$psych_reversed, "1")
  expect_match(it$note, "psych")
})

test_that("groups with too few complete cases are not computed", {
  skip_if_not_installed("psych")
  kit <- stats_fake_kit()
  d <- stats_fake_data(15)
  d$b1[1:8] <- 9999
  gs <- group_stats(d, kit, "A1-P")
  gb <- gs[gs$groupID == "G.b" & gs$varname == "", ]
  expect_identical(gb$alpha, "")
  expect_match(gb$note, "10")
})

test_that("stats_analysis returns three kit tables that pass check_kit", {
  skip_if_not_installed("psych")
  kit <- stats_fake_kit()
  res <- stats_analysis(stats_fake_data(), kit, "A1-P", run_by = "tester", official = TRUE,
                        bank_version = "2026.1", date = as.Date("2026-11-02"), analysisID = "AN1")
  expect_named(res, c("analyses", "item_stats", "group_stats"))
  expect_identical(res$analyses$official, "1")
  expect_match(res$analyses$software, "psych")
  expect_true(all(res$item_stats$analysisID == "AN1"))
  kit[names(res)] <- res
  expect_equal(nrow(check_kit(kit)), 0L)
  wb <- stats_workbook(res, kit)
  # 原文：說明
  expect_identical(names(wb)[1], "\u8aaa\u660e")
  expect_true(any(grepl("<5", wb[[1]][[2]], fixed = TRUE)))
  expect_true(any(wb[[1]][[2]] == "2026.1"))
  # 只有彙總：沒有一欄和資料一樣長
  expect_true(all(vapply(res, nrow, integer(1)) < 200L))
})

test_that("the schema has the three evidence tables and the kit round-trips", {
  expect_true(all(c("analyses", "item_stats", "group_stats") %in% kit_tables()))
  expect_identical(length(empty_kit()), length(KIT_SCHEMA))
  kit <- read_kit()
  expect_identical(nrow(kit$analyses), 0L)
})

test_that("stats_read_upload reads a csv into memory and deletes the file", {
  f <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(data.frame(A1 = c(1, 2, 7777), t1 = c("x", "", "y")), f)
  d <- stats_read_upload(f, "wave.csv")
  expect_false(file.exists(f))
  expect_identical(nrow(d), 3L)
  f2 <- withr::local_tempfile(fileext = ".txt")
  writeLines("x", f2)
  expect_error(stats_read_upload(f2, "wave.txt"))
  expect_false(file.exists(f2))
})

test_that("stats_read_upload keeps SPSS user-missing codes as numbers", {
  skip_if_not_installed("haven")
  f <- withr::local_tempfile(fileext = ".sav")
  v <- haven::labelled_spss(c(1, 2, 7777), labels = c(yes = 1, no = 2), na_values = 7777)
  df <- data.frame(id = 1:3)
  df$a1 <- v
  haven::write_sav(df, f)
  d <- stats_read_upload(f, "wave.sav")
  expect_false(file.exists(f))
  expect_identical(d$a1, c(1, 2, 7777))
})
