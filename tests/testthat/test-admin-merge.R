# tests/testthat/test-admin-merge.R — 併入新施測：propose_admin_merge()／apply_admin_merge()
#
# 全部用小的合成題庫（不用真的資料）：先做一個「完整」的題庫（M03M036、M03M048 兩次
# 施測），把 M03M048 拿掉，再用它的表頭（合成的）跑提案、全部接受，和完整的比。

am_fixture <- function() {
  df <- function(...) data.frame(..., stringsAsFactors = FALSE)
  kit <- empty_kit()
  kit$cohorts <- conform_kit_table(df(cohortID = "K2016", code = "M03", birth_from = "2016-04-01"),
                                   "cohorts")
  kit$admins <- conform_kit_table(df(adminID = c("M03M036", "M03M048"), cohortID = "K2016",
                                     ageMonth = c("36", "48"), waveNo = c("6", "7"),
                                     status = "internal"), "admins")
  kit$forms <- conform_kit_table(df(formID = "P-36-72", instrument = "P", label = "parent"),
                                 "forms")
  kit$datasets <- conform_kit_table(df(
    datasetID = c("M03M036-P", "M03M048-P"), adminID = c("M03M036", "M03M048"),
    file = "data1", instrument = "P", formID = "P-36-72", respondent = "家長",
    missingScheme = c("2016", "2020"), note = "parents"), "datasets")
  kit$missing_schemes <- conform_kit_table(df(
    scheme = c(rep("2016", 4), rep("2020", 5)),
    code = c("7777", "8888", "9996", "9999", "7777", "8888", "9995", "9996", "9999"),
    label = c("拒答", "不知道", "跳答或不適用", "遺漏值", "拒答", "不知道", "不適用", "跳答", "遺漏值"),
    kind = c("refused", "dont_know", "not_applicable", "missing", "refused", "dont_know",
             "not_applicable", "skipped", "missing")), "missing_schemes")
  kit$items <- conform_kit_table(df(
    item_uid = c("P.a01", "P.b01", "P.c01", "P.c01n", "P.d01", "P.e01"), instrument = "P",
    varname = c("a01", "b01", "c01", "c01n", "d01", "e01"),
    block = c("blk1", "blk1", "blk1", "blk1", "blk1", "blk2"), role = "measure",
    module = "family", source_type = "questionnaire", deprecated = "0"), "items")
  kit$item_versions <- conform_kit_table(df(
    item_uid = c("P.a01", "P.b01", "P.b01", "P.c01", "P.c01n", "P.d01", "P.e01"),
    itemVer = c("1", "1", "2", "1", "1", "1", "1"),
    label = c("孩子會自己穿衣服", "孩子每天看書的時間", "孩子每週和家人一起看書的次數",
              "家裡有幾本童書", "家裡有幾本童書", "孩子會騎腳踏車", "孩子會游泳"),
    valueSetID = c("vs0001", "vs0001", "vs0002", "vs0001", "vs0001", "vs0003", "vs0001"),
    itemType = "MC"), "item_versions")
  kit$value_codes <- conform_kit_table(df(
    valueSetID = c("vs0001", "vs0001", "vs0002", "vs0002", "vs0002", "vs0003", "vs0003"),
    code = c("0", "1", "1", "2", "3", "1", "2"),
    label = c("不會", "會", "很少", "有時", "常常", "是", "否"),
    order = c("1", "2", "1", "2", "3", "1", "2")), "value_codes")
  kit$item_relations <- conform_kit_table(df(
    from_uid = c("P.b01", "P.c01"), from_ver = "1", to_uid = c("P.b01", "P.c01n"), to_ver = c("2", "1"),
    kind = c("revised", "renamed"), effective_admin = "M03M048"), "item_relations")
  kit$groups <- conform_kit_table(df(groupID = "P.G", label = "G"), "groups")
  kit$group_tree <- conform_kit_table(df(formID = "P-36-72", groupID = "P.G"), "group_tree")
  kit$group_members <- conform_kit_table(df(formID = "P-36-72", groupID = "P.G",
                                            item_uid = c("P.c01", "P.c01n"), scheme = "official",
                                            reverse = "0"), "group_members")
  kit$dataset_vars <- conform_kit_table(df(
    datasetID = c(rep("M03M036-P", 5), rep("M03M048-P", 5)),
    varname = c("baby_sex", "a01", "b01", "c01", "e01", "baby_sex", "a01", "b01", "c01n", "d01"),
    item_uid = c("", "P.a01", "P.b01", "P.c01", "P.e01", "", "P.a01", "P.b01", "P.c01n", "P.d01"),
    itemVer = c("", "1", "1", "1", "1", "", "1", "2", "1", "1"),
    kind = c("system", rep("item", 4), "system", rep("item", 4)),
    position = c("", "一.1", "一.2", "一.3", "二.1", "", "一.1", "一.2", "一.3", "一.4"),
    order = c("1", "2", "3", "4", "5", "1", "2", "3", "4", "5")), "dataset_vars")
  kit$block_status <- conform_kit_table(df(datasetID = "M03M048-P", block = "blk2", asked = "none"),
                                        "block_status")
  kit
}

# 新施測拿掉：施測、資料檔、變項、區塊狀態，和只在這次出現的題目、版本、關係
am_remove_admin <- function(kit, admin) {
  ids <- kit$datasets$datasetID[kit$datasets$adminID == admin]
  kit$admins <- kit$admins[kit$admins$adminID != admin, , drop = FALSE]
  kit$datasets <- kit$datasets[!kit$datasets$datasetID %in% ids, , drop = FALSE]
  kit$block_status <- kit$block_status[!kit$block_status$datasetID %in% ids, , drop = FALSE]
  here <- kit$dataset_vars[kit$dataset_vars$datasetID %in% ids & nzchar(kit$dataset_vars$item_uid), ]
  kit$dataset_vars <- kit$dataset_vars[!kit$dataset_vars$datasetID %in% ids, , drop = FALSE]
  used <- paste(kit$dataset_vars$item_uid, kit$dataset_vars$itemVer)
  only <- setdiff(paste(here$item_uid, here$itemVer), used)
  vkey <- paste(kit$item_versions$item_uid, kit$item_versions$itemVer)
  kit$item_versions <- kit$item_versions[!vkey %in% only, , drop = FALSE]
  kit$items <- kit$items[kit$items$item_uid %in% kit$item_versions$item_uid, , drop = FALSE]
  kit$group_members <- kit$group_members[kit$group_members$item_uid %in% kit$items$item_uid, ]
  kit$item_relations <- kit$item_relations[kit$item_relations$effective_admin != admin, ]
  vs <- unique(kit$item_versions$valueSetID)
  kit$value_codes <- kit$value_codes[kit$value_codes$valueSetID %in% vs, , drop = FALSE]
  for (tb in names(kit)) rownames(kit[[tb]]) <- NULL
  kit
}

# 一個資料檔的合成表頭：標籤 = 題號 + 題目文字；值標籤 = 選項 + 遺漏代碼
am_header <- function(kit, datasetID, miss = "9995=不適用; 9996=跳答; 9999=遺漏值") {
  dv <- kit$dataset_vars[kit$dataset_vars$datasetID == datasetID, , drop = FALSE]
  dv <- dv[order(as.integer(dv$order)), , drop = FALSE]
  iv <- kit$item_versions
  k <- match(paste(dv$item_uid, dv$itemVer), paste(iv$item_uid, iv$itemVer))
  lab <- ifelse(is.na(k), "幼兒性別", paste0(dv$position, ".", iv$label[k]))
  vl <- vapply(seq_len(nrow(dv)), function(i) {
    if (is.na(k[i])) return("1=男; 2=女")
    vc <- kit$value_codes[kit$value_codes$valueSetID == iv$valueSetID[k[i]], ]
    paste(c(paste0(vc$code, "=", vc$label), miss), collapse = "; ")
  }, character(1))
  data.frame(var = dv$varname, class = "numeric", label = lab, n_vlab = "", vlabs = vl, na = "",
             stringsAsFactors = FALSE)
}

test_that("pretend M03M048 is new: proposals, accept all, get the full kit back", {
  full <- am_fixture()
  expect_equal(nrow(check_kit(full)), 0L)
  base <- am_remove_admin(full, "M03M048")
  expect_equal(nrow(check_kit(base)), 0L)
  expect_false("P.c01n" %in% base$items$item_uid)
  meta <- am_header(full, "M03M048-P")

  p <- propose_admin_merge(base, list("M03M048-P" = meta), "M03M048")
  cls <- stats::setNames(p$class, ifelse(nzchar(p$varname), p$varname, p$block))
  expect_equal(unname(cls[c("baby_sex", "a01", "b01", "c01n", "d01")]),
               c("not_item", "same", "new_version", "renamed", "new_item"))
  expect_equal(unname(cls["blk2"]), "dropped_or_block")
  expect_true("missing_scheme_change" %in% p$class)
  expect_true(all(nzchar(p$reason)))
  expect_true(all(p$confidence %in% c("high", "medium", "low")))
  b <- p[p$varname == "b01", ]
  expect_equal(c(b$itemVer, b$from_ver, b$values_match), c("2", "1", "codes_differ"))
  r <- p[p$varname == "c01n", ]
  expect_equal(c(r$item_uid, r$from_uid, r$confidence), c("P.c01n", "P.c01", "high"))
  expect_equal(p$scheme[p$class == "missing_scheme_change"], "2020")
  expect_equal(p$action[p$class == "dropped_or_block"], "block_none")
  expect_equal(p$position[p$varname == "d01"], "一.4")
  s <- admin_merge_summary(p)
  expect_equal(sum(s$total), nrow(p))

  out <- apply_admin_merge(base, p, accept = TRUE)
  expect_equal(nrow(check_kit(out)), 0L)

  # 和完整的題庫比
  key <- function(kit, tb, cols = KIT_SCHEMA[[tb]]$key) sort(kit_key_str(kit[[tb]], cols))
  for (tb in c("admins", "datasets", "items", "item_versions", "item_relations",
               "dataset_vars", "block_status", "group_members")) {
    expect_equal(key(out, tb), key(full, tb), info = tb)
  }
  dv_cols <- c("datasetID", "varname", "item_uid", "itemVer", "kind", "position", "order")
  sort_dv <- function(x) {
    x <- x[x$datasetID == "M03M048-P", dv_cols]
    x <- x[order(x$varname), ]
    rownames(x) <- NULL
    x
  }
  expect_equal(sort_dv(out$dataset_vars), sort_dv(full$dataset_vars))
  expect_equal(out$datasets$missingScheme[out$datasets$datasetID == "M03M048-P"], "2020")
  expect_equal(out$datasets$file[out$datasets$datasetID == "M03M048-P"], "data1")
  ad <- out$admins[out$admins$adminID == "M03M048", ]
  expect_equal(c(ad$cohortID, ad$ageMonth, ad$waveNo), c("K2016", "48", "7"))
  # 選項組：b01 第 2 版重用 vs0002；新題 d01 重用 vs0003
  nv <- out$item_versions
  expect_equal(nv$valueSetID[nv$item_uid == "P.b01" & nv$itemVer == "2"], "vs0002")
  expect_equal(nv$valueSetID[nv$item_uid == "P.d01"], "vs0003")
  expect_equal(nv$label[nv$item_uid == "P.d01"], "孩子會騎腳踏車")
  expect_equal(out$item_relations$kind[out$item_relations$to_uid == "P.c01n"], "renamed")
  expect_equal(out$items$block[out$items$item_uid == "P.d01"], "blk1")
})

test_that("rejected proposals keep the variable but do not link it", {
  full <- am_fixture()
  base <- am_remove_admin(full, "M03M048")
  meta <- am_header(full, "M03M048-P")
  p <- propose_admin_merge(base, list("M03M048-P" = meta), "M03M048")
  keep <- p$proposal_id[!p$class %in% c("new_version", "missing_scheme_change", "dropped_or_block")]
  out <- apply_admin_merge(base, p, accept = keep)
  expect_equal(nrow(check_kit(out)), 0L)
  dv <- out$dataset_vars[out$dataset_vars$datasetID == "M03M048-P", ]
  expect_equal(nrow(dv), 5L)
  expect_equal(dv$item_uid[dv$varname == "b01"], "")
  expect_equal(out$datasets$missingScheme[out$datasets$datasetID == "M03M048-P"], "2016")
  expect_equal(nrow(out$block_status), 0L)
  expect_false(any(out$item_versions$item_uid == "P.b01" & out$item_versions$itemVer == "2"))
  # 用信心程度接受
  out2 <- apply_admin_merge(base, p, accept = "high")
  expect_equal(nrow(check_kit(out2)), 0L)
})

test_that("same item in two data files gets one new version; a new value set is created", {
  full <- am_fixture()
  base <- am_remove_admin(full, "M03M048")
  base$datasets <- rbind(base$datasets, conform_kit_table(data.frame(
    datasetID = "M03M036-PK", adminID = "M03M036", file = "data2", instrument = "P",
    formID = "P-36-72", missingScheme = "2016", stringsAsFactors = FALSE), "datasets"))
  base$dataset_vars <- rbind(base$dataset_vars, conform_kit_table(data.frame(
    datasetID = "M03M036-PK", varname = c("a01", "e01"), item_uid = c("P.a01", "P.e01"),
    itemVer = "1", kind = "item", order = c("1", "2"), stringsAsFactors = FALSE), "dataset_vars"))
  base$block_status <- conform_kit_table(data.frame(
    datasetID = "M03M036-PK", block = "blk3", asked = "none", who = "親友版不問",
    stringsAsFactors = FALSE), "block_status")
  m <- data.frame(var = c("a01", "e01"), class = "numeric",
                  label = c("一.1.孩子會自己穿衣服", "二.1.孩子會游泳嗎"),
                  n_vlab = "", vlabs = c("0=不會; 1=會", "0=不會; 1=會; 2=會一點"), na = "",
                  stringsAsFactors = FALSE)
  p <- propose_admin_merge(base, list("M03M048-P" = m, "M03M048-PK" = m), "M03M048")
  e <- p[p$varname == "e01", ]
  expect_equal(e$class, c("new_version", "new_version"))
  expect_equal(e$itemVer, c("2", "2"))
  expect_equal(e$prev_dataset, c("M03M036-P", "M03M036-PK"))
  # 沒有遺漏代碼 → 不提換方案；上一次的區塊狀態沿用
  expect_false("missing_scheme_change" %in% p$class)
  bs <- p[p$class == "dropped_or_block", ]
  expect_true(any(bs$datasetID == "M03M048-PK" & bs$action == "block_none" & bs$block == "blk3"))
  out <- apply_admin_merge(base, p)
  expect_equal(nrow(check_kit(out)), 0L)
  iv <- out$item_versions[out$item_versions$item_uid == "P.e01" & out$item_versions$itemVer == "2", ]
  expect_equal(nrow(iv), 1L)
  expect_false(iv$valueSetID %in% base$value_codes$valueSetID)
  expect_equal(nrow(out$value_codes[out$value_codes$valueSetID == iv$valueSetID, ]), 3L)
  expect_equal(sum(out$item_relations$to_uid == "P.e01"), 1L)
  expect_equal(out$block_status$who[out$block_status$datasetID == "M03M048-PK"], "親友版不問")
  # -P 少了 b01、c01（區塊 blk1 還有問）：之後沒有再用 → 建議停用
  expect_setequal(p$item_uid[p$action == "deprecate"], c("P.b01", "P.c01"))
})

test_that("a partly dropped item is proposed as deprecated; workbook starts with 說明", {
  full <- am_fixture()
  base <- am_remove_admin(full, "M03M048")
  meta <- am_header(full, "M03M048-P")
  meta <- meta[meta$var != "a01", ]
  p <- propose_admin_merge(base, list("M03M048-P" = meta), "M03M048")
  d <- p[p$class == "dropped_or_block" & p$item_uid == "P.a01", ]
  expect_equal(d$action, "deprecate")
  out <- apply_admin_merge(base, p)
  expect_equal(out$items$deprecated[out$items$item_uid == "P.a01"], "1")
  wb <- admin_merge_workbook(p)
  expect_equal(names(wb)[1], "說明")
  f <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(wb, f)
  expect_equal(readxl::excel_sheets(f)[1], "說明")
  unlink(f)
})

test_that("errors are plain: existing admin, unknown adminID", {
  full <- am_fixture()
  meta <- am_header(full, "M03M048-P")
  expect_error(propose_admin_merge(full, list("M03M048-P" = meta), "M03M048"), "already")
  expect_error(propose_admin_merge(full, list("X-P" = meta), "weird"), "cannot read")
  expect_error(propose_admin_merge(full, meta, "M03M060"), "named list")
})

test_that("sav_header_meta reads names and labels from a synthetic .sav", {
  skip_if_not_installed("haven")
  f <- tempfile(fileext = ".sav")
  d <- data.frame(id = c(1, 2), q1 = haven::labelled(c(1, 9999), c(Yes = 1, Missing = 9999),
                                                         label = "1.Question one"))
  haven::write_sav(d, f)
  m <- sav_header_meta(f)
  unlink(f)
  expect_equal(m$var, c("id", "q1"))
  expect_equal(m$label[2], "1.Question one")
  expect_equal(m$vlabs[2], "1=Yes; 9999=Missing")
  expect_equal(names(m), c("var", "class", "label", "n_vlab", "vlabs", "na"))
})
