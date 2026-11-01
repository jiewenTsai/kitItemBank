# concept_info.R：概念、三份問卷的版本、官方文件、文獻、題目 × 波次

test_that("the built-in kit has concepts, official document records and references", {
  k <- read_kit()
  expect_gt(nrow(k$concepts), 100)
  expect_true(all(c("認知發展", "語言發展", "社會情緒發展", "身體動作發展", "家庭") %in% k$concepts$conceptID))
  expect_true(all(nzchar(k$concepts$domain)))
  expect_true(all(k$groups$construct %in% c("", k$concepts$conceptID)))
  expect_gt(nrow(k$group_docs), 150)
  expect_gt(nrow(k$refs), 20)
  expect_true(all(k$group_refs$refID %in% k$refs$refID))
  # 家長、教師、兒童的社會能力是同一個概念；家長和教師的「學習環境」不是
  g <- k$groups
  expect_identical(length(unique(g$construct[g$groupID %in% c("P.社會能力", "T.社會能力", "C.社會能力")])), 1L)
  expect_false(identical(g$construct[g$groupID == "P.學習環境"], g$construct[g$groupID == "T.學習環境"]))
  # 名稱對齊官方文件，舊名在別名
  expect_identical(g$label[g$groupID == "P.外放性"], "外放性氣質")
  expect_match(g$alias[g$groupID == "P.Home"], "家庭環境")
  expect_identical(g$label[g$groupID == "T.師生倫理信念"], "師生關係信念")
})

test_that("concept_info_ui shows informant versions, the official document and references", {
  k <- read_kit()
  html <- as.character(concept_info_ui(k, "P.社會能力"))
  expect_match(html, "同一概念的家長、教師、兒童版本")
  expect_match(html, "教師評")
  expect_match(html, "兒童自評")
  html <- as.character(concept_info_ui(k, "P.工作記憶"))
  expect_match(html, "G1-G3問卷構面及信效度")
  expect_match(html, "Thorell")
  expect_match(html, "https://doi.org/10.1080/87565640802101516")
  expect_null(concept_info_ui(k, "P.no-such-group"))
})

test_that("dim_item_wave_ui lists versions per wave by cohort and marks anchor items", {
  b <- kit_bank("parent")
  html <- as.character(dim_item_wave_ui(b, "父親親職參與"))
  expect_match(html, "M03 世代")
  expect_match(html, "M36 世代")
  expect_match(html, "錨題")
  expect_match(html, "v1")
  expect_null(dim_item_wave_ui(b, "no-such-dim"))
})
