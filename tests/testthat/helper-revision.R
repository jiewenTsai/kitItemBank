# helper-revision.R：改版底稿測試用的小題庫和填表工具（test-revision.R、test-page-revision.R 共用）

rv_kit <- function() {
  k <- empty_kit()
  row <- function(tb, ...) conform_kit_table(data.frame(..., stringsAsFactors = FALSE), tb)
  k$cohorts <- row("cohorts", cohortID = "K2016", code = "M03", birth_from = "2016-04-01")
  k$admins <- row("admins", adminID = c("M03M084G01", "M03M096G02"), cohortID = "K2016",
                  ageMonth = c("84", "96"), grade = c("1", "2"), waveNo = c("10", "11"),
                  status = "internal")
  k$forms <- row("forms", formID = "P-G1-G3", instrument = "P", label = "家長 國小 1–3 年級",
                 ageFrom = "84", ageTo = "108")
  k$missing_schemes <- row("missing_schemes", scheme = "2020", code = "7777", label = "拒答")
  k$datasets <- row("datasets", datasetID = c("M03M084G01-P", "M03M096G02-P"),
                    adminID = c("M03M084G01", "M03M096G02"), file = "data1", instrument = "P",
                    formID = "P-G1-G3", respondent = "家長", missingScheme = "2020")
  uids <- paste0("P.q", 1:5)
  k$items <- row("items", item_uid = uids, instrument = "P", varname = paste0("q", 1:5),
                 block = "生活", module = "family", deprecated = "0")
  k$value_codes <- row("value_codes", valueSetID = c("vs0001", "vs0001", "vs0002", "vs0002"),
                       code = c("1", "2", "0", "1"), label = c("有", "沒有", "否", "是"),
                       order = c("1", "2", "1", "2"))
  k$item_versions <- row("item_versions", item_uid = uids, itemVer = "1",
                         label = paste0("第", 1:5, "題的文字"), valueSetID = "vs0001",
                         itemType = "MC")
  dv <- function(ds) row("dataset_vars", datasetID = ds, varname = c("id", paste0("q", 1:5)),
                         item_uid = c("", uids), itemVer = c("", rep("1", 5)),
                         kind = c("system", rep("item", 5)), position = c("", paste0("一.", 1:5)),
                         order = as.character(1:6))
  k$dataset_vars <- rbind(dv("M03M084G01-P"), dv("M03M096G02-P"))
  k
}

# 模擬老師填底稿：cells 是 cell() 的清單；new 是新增題目表；f 可以再對 workbook 動手腳
rv_fill <- function(path, out, cells = list(), new = NULL, f = NULL) {
  wb <- openxlsx::loadWorkbook(path)
  for (x in cells) openxlsx::writeData(wb, "題目", x$value, startCol = x$col, startRow = x$row,
                                       colNames = FALSE)
  if (!is.null(new)) openxlsx::writeData(wb, "新增題目", new, startRow = 2, colNames = FALSE)
  if (!is.null(f)) f(wb)
  openxlsx::saveWorkbook(wb, out, overwrite = TRUE)
  out
}
# 題目表的欄：列號 1、動作 6、新題目文字 7、新選項 8、備註 9；row 是第幾題
cell <- function(row, col, value) list(row = row + 1L, col = col, value = value)
