# schema_steps.R：item_steps 表、import_item_steps()、item_step_thresholds()、item_steps_ui()

# 假題庫：一個資料檔 A1-P；a1（一版）、a2（兩版，資料檔用第 2 版）、a3（一版，不在資料檔）
steps_fake_kit <- function() {
  kit <- empty_kit()
  kit$cohorts <- conform_kit_table(data.frame(cohortID = "K1", code = "M01"), "cohorts")
  kit$admins <- conform_kit_table(data.frame(adminID = "A1", cohortID = "K1", ageMonth = "36",
                                             status = "internal"), "admins")
  kit$forms <- conform_kit_table(data.frame(formID = "F1", instrument = "P", label = "F1"), "forms")
  kit$datasets <- conform_kit_table(data.frame(datasetID = "A1-P", adminID = "A1", file = "data1",
                                               instrument = "P", formID = "F1"), "datasets")
  kit$items <- conform_kit_table(data.frame(item_uid = c("P.a1", "P.a2", "P.a3"), instrument = "P",
                                            varname = c("a1", "a2", "a3")), "items")
  kit$item_versions <- conform_kit_table(data.frame(
    item_uid = c("P.a1", "P.a2", "P.a2", "P.a3"), itemVer = c("1", "1", "2", "1")), "item_versions")
  kit$dataset_vars <- conform_kit_table(data.frame(
    datasetID = "A1-P", varname = c("a1", "a2"), item_uid = c("P.a1", "P.a2"),
    itemVer = c("1", "2"), kind = "item"), "dataset_vars")
  kit$analyses <- conform_kit_table(data.frame(analysisID = "AN1", datasetID = "A1-P",
                                               method = "kitsUI PCM", official = "1",
                                               date = "2026-10-01"), "analyses")
  kit
}

# kits 的寫法：item_id、item_score、beta、beta_se
steps_fake_params <- function() {
  data.frame(item_id = c("a1", "a1", "a2", "a2", "a2"), item_score = c(1, 2, 1, 2, 3),
             beta = c(-1, 0.25, -0.5, 0.3, 1.2), beta_se = c(0.1, 0.12, 0.2, 0.15, 0.3),
             stringsAsFactors = FALSE)
}

test_that("item_steps is part of the schema and the bank has an empty table", {
  expect_true("item_steps" %in% kit_tables())
  expect_identical(KIT_SCHEMA$item_steps$key, c("analysisID", "item_uid", "itemVer", "step"))
  kit <- read_kit()
  expect_identical(names(kit$item_steps),
                   c("analysisID", "item_uid", "itemVer", "step", "beta", "se", "note"))
  expect_equal(nrow(check_kit(empty_kit())), 0L)
})

test_that("import_item_steps() resolves varnames and builds a patch that applies cleanly", {
  kit <- steps_fake_kit()
  res <- import_item_steps(steps_fake_params(), kit, "AN1")
  expect_equal(nrow(res$analyses), 0L)
  expect_equal(nrow(res$item_steps), 5L)
  expect_identical(unique(res$item_steps$analysisID), "AN1")
  expect_identical(res$item_steps$item_uid, c("P.a1", "P.a1", "P.a2", "P.a2", "P.a2"))
  # a2 在資料檔裡是第 2 版
  expect_identical(res$item_steps$itemVer, c("1", "1", "2", "2", "2"))
  expect_identical(res$item_steps$beta, c("-1", "0.25", "-0.5", "0.3", "1.2"))
  expect_identical(res$item_steps$se[1], "0.1")
  expect_true(all(res$patch$table == "item_steps"))
  expect_equal(sum(res$patch$op == "add"), 5L)

  new <- apply_patch(kit, res$patch)
  expect_equal(nrow(check_kit(new)), 0L)
  expect_equal(nrow(new$item_steps), 5L)
  expect_equal(new$item_steps[order(new$item_steps$item_uid, new$item_steps$step), ],
               res$item_steps, ignore_attr = TRUE)
})

test_that("import_item_steps() accepts item_uid and a new analysis row", {
  kit <- steps_fake_kit()
  df <- data.frame(item_uid = "P.a3", step = c(2, 1), beta = c(0.5, -0.5), note = "pilot")
  res <- import_item_steps(df, kit, data.frame(analysisID = "AN2", datasetID = "A1-P",
                                               method = "kitsUI PCM", official = "0"))
  expect_equal(nrow(res$analyses), 1L)
  # 只有一版：自動用第 1 版；照步驟排序；沒有 se
  expect_identical(res$item_steps$step, c("1", "2"))
  expect_identical(res$item_steps$itemVer, c("1", "1"))
  expect_identical(res$item_steps$se, c("", ""))
  expect_identical(res$patch$table[1], "analyses")
  new <- apply_patch(kit, res$patch)
  expect_equal(nrow(check_kit(new)), 0L)
  expect_true("AN2" %in% new$analyses$analysisID)
  expect_identical(new$item_steps$note, c("pilot", "pilot"))
})

test_that("import_item_steps() reports problems and refuses to double-import", {
  kit <- steps_fake_kit()
  gap <- data.frame(item_uid = "P.a1", itemVer = 1, step = c(1, 3), beta = c(0, 1))
  expect_error(import_item_steps(gap, kit, "AN1"), "不跳號")
  unknown <- data.frame(varname = "zz", step = 1, beta = 0)
  expect_error(import_item_steps(unknown, kit, "AN1"), "找不到變項")
  bad_beta <- data.frame(item_uid = "P.a1", itemVer = 1, step = 1, beta = "x")
  expect_error(import_item_steps(bad_beta, kit, "AN1"), "beta")
  ambiguous <- data.frame(item_uid = "P.a3", itemVer = 9, step = 1, beta = 0)
  expect_error(import_item_steps(ambiguous, kit, "AN1"), "沒有這些題目版本")
  expect_error(import_item_steps(steps_fake_params(), kit, "NOPE"), "沒有分析")

  kit <- apply_patch(kit, import_item_steps(steps_fake_params(), kit, "AN1")$patch)
  expect_error(import_item_steps(steps_fake_params(), kit, "AN1"), "replace = TRUE")
  res <- import_item_steps(steps_fake_params()[1:2, ], kit, "AN1", replace = TRUE)
  expect_equal(sum(res$patch$op == "delete"), 5L)
  new <- apply_patch(kit, res$patch)
  expect_equal(nrow(new$item_steps), 2L)
  expect_equal(nrow(check_kit(new)), 0L)
})

test_that("item_step_thresholds() matches a direct kits::algo_thresholds() call", {
  skip_if_not_installed("kits")
  kit <- steps_fake_kit()
  kit <- apply_patch(kit, import_item_steps(steps_fake_params(), kit, "AN1")$patch)
  out <- item_step_thresholds(kit, c("P.a1", "P.a2"))
  expect_equal(nrow(out), 5L)
  p <- steps_fake_params()
  cml <- list(coef = data.frame(item_id = p$item_id, item_score = p$item_score, beta = p$beta,
                                se = p$beta_se),
              vcov = diag(p$beta_se^2))
  ref <- kits::algo_thresholds(cml)
  expect_equal(out$thr, ref$thr)
  expect_equal(out$thr_se, ref$thr_se)
  expect_match(attr(out, "note"), "algo_thresholds")
  # 一步的題目：門檻就是 beta
  one <- item_step_thresholds(kit, "P.a1", analysisID = "AN1")
  expect_equal(nrow(one), 2L)
  expect_true(all(diff(one$thr) > 0))
})

test_that("thresholds from stored steps equal those of a fitted kits model", {
  skip_if_not_installed("kits")
  betas <- list(a1 = c(-1, 0), a2 = c(0, 1))
  X <- kits::kits_sim(seq(-3, 3, length.out = 300), betas, seed = 1)
  fit <- kits::algo_cml(kits::kits_stats(X))
  ref <- kits::algo_thresholds(fit)
  kit <- steps_fake_kit()
  co <- fit$coef
  res <- import_item_steps(data.frame(item_id = co$item_id, item_score = co$item_score,
                                      beta = co$beta, beta_se = co$se), kit, "AN1")
  out <- item_step_thresholds(apply_patch(kit, res$patch), c("P.a1", "P.a2"))
  # beta 存到小數 6 位；門檻一樣（標準誤不同：題庫沒有步驟之間的共變）
  expect_equal(out$thr, ref$thr, tolerance = 1e-5)
})

test_that("without kits the betas come back with a note", {
  kit <- steps_fake_kit()
  kit <- apply_patch(kit, import_item_steps(steps_fake_params(), kit, "AN1")$patch)
  local_mocked_bindings(steps_have_kits = function() FALSE)
  out <- item_step_thresholds(kit, "P.a2")
  expect_equal(out$beta, c(-0.5, 0.3, 1.2))
  expect_true(all(is.na(out$thr)))
  expect_match(attr(out, "note"), "kits")
  expect_equal(nrow(item_step_thresholds(kit, "P.a3")), 0L)
})

test_that("item_steps_ui() is NULL without steps and a table with them", {
  kit <- steps_fake_kit()
  expect_null(item_steps_ui(kit, "P.a1"))
  expect_null(item_steps_ui(read_kit(), "P.cogc01"))
  kit <- apply_patch(kit, import_item_steps(steps_fake_params(), kit, "AN1")$patch)
  ui <- item_steps_ui(kit, "P.a2", 2)
  html <- as.character(ui)
  expect_match(html, "題目參數")
  expect_match(html, "AN1")
  expect_match(html, "正式結果")
  expect_match(html, "-0.50")
  expect_null(item_steps_ui(kit, "P.a2", 1))
})
