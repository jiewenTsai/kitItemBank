# data-raw/make_review_xlsx.R — 把 data-raw/candidates/ 的候選做成給團隊確認的 Excel
#
# 用法（在套件根目錄，先跑過 build_kit.R）：
#   LC_ALL=en_US.UTF-8 Rscript --no-init-file data-raw/make_review_xlsx.R <輸出檔.xlsx>
# 每張表：灰色欄是系統產生的（不要改），黃色欄給人填（下拉選單 + 意見）。
# 最右邊「系統用」欄位是匯回時對應用的主鍵。

pkgload::load_all(quiet = TRUE)
out <- commandArgs(TRUE)[1]
if (is.na(out)) stop("need output path")

kit <- read_kit("inst/extdata/kit")
cand <- function(f) read.csv(file.path("data-raw/candidates", f), encoding = "UTF-8",
                             stringsAsFactors = FALSE, colClasses = "character")

zh <- function(x, map) { y <- unname(map[x]); y[is.na(y)] <- x[is.na(y)]; y }
CONF <- c(high = "高", medium = "中", low = "低")
conf_order <- function(x) match(x, c("low", "medium", "high"))
iv_label <- function(uid, ver) {
  kit$item_versions$label[match(paste(uid, ver), paste(kit$item_versions$item_uid,
                                                       kit$item_versions$itemVer))]
}
item_label <- function(uid) {  # 最新版的題目文字
  iv <- kit$item_versions[order(kit$item_versions$item_uid, -as.integer(kit$item_versions$itemVer)), ]
  iv$label[match(uid, iv$item_uid)]
}
grp_label <- function(g) {
  l <- kit$groups$label[match(g, kit$groups$groupID)]
  ifelse(is.na(l), g, l)
}
num <- function(prefix, n) sprintf("%s%04d", prefix, seq_len(n))

AGREE <- c("同意", "不同意", "不確定")

# 1 題目關係 ----
r <- cand("item_relations.csv")
r <- r[order(conf_order(r$confidence), r$kind, r$from_uid), ]
rel <- data.frame(
  編號 = num("R", nrow(r)),
  已進題庫 = ifelse(r$confidence == "high", "是", "否"),
  信心 = zh(r$confidence, CONF),
  關係 = zh(r$kind, c(renamed = "換題號", revised = "改版", split = "拆開", merged = "合併",
                     borrowed = "借用")),
  舊題號 = r$from_uid, 舊版 = r$from_ver, 舊題目 = iv_label(r$from_uid, r$from_ver),
  新題號 = r$to_uid, 新版 = r$to_ver, 新題目 = iv_label(r$to_uid, r$to_ver),
  文字相似 = zh(r$similarity, c(identical = "相同", high = "高", medium = "中", low = "低",
                              none = "無")),
  可比較 = zh(r$comparable, c(yes = "可以", partial = "部分", no = "不行")),
  意思改了 = zh(r$meaning_changed, c("1" = "是", "0" = "否")),
  從哪次施測起 = r$effective_admin,
  依據 = trimws(paste(r$reason, r$basis, sep = "；")),
  判斷 = "", 意見 = "",
  系統用 = paste(r$from_uid, r$from_ver, r$to_uid, r$to_ver, sep = "|"),
  check.names = FALSE, stringsAsFactors = FALSE)

# 2 區塊狀態 ----
b <- cand("block_status.csv")
b <- b[order(conf_order(b$confidence), b$datasetID), ]
ds <- kit$datasets[match(b$datasetID, kit$datasets$datasetID), ]
blk <- data.frame(
  編號 = num("B", nrow(b)),
  已進題庫 = ifelse(b$confidence == "high", "是", "否"),
  信心 = zh(b$confidence, CONF),
  施測 = ds$adminID, 填答者 = ds$respondent, 資料檔 = b$datasetID, 區塊 = b$block,
  狀態 = zh(b$asked, c(all = "全部都問", subset = "只問部分", carried = "帶入（沒再問）",
                     none = "沒問")),
  問誰 = b$who, 條件變項 = b$who_var,
  依據 = trimws(paste(b$reason, b$basis, sep = "；")),
  判斷 = "", 意見 = "",
  系統用 = paste(b$datasetID, b$block, sep = "|"),
  check.names = FALSE, stringsAsFactors = FALSE)

# 3 題組成員 ----
m <- cand("cw_member_hints.csv")
m <- m[order(-conf_order(m$confidence), m$formID, m$groupID), ]
mem <- data.frame(
  編號 = num("M", nrow(m)),
  信心 = zh(m$confidence, CONF),
  題本 = m$formID, 題號 = m$item_uid, 題目 = item_label(m$item_uid),
  建議題組 = grp_label(m$groupID),
  題組已存在 = ifelse(m$group_exists == "1", "是", "否（要新建）"),
  反向題 = ifelse(m$reverse == "1", "是", ""),
  目前所屬題組 = m$current_groups, 對照表路徑 = m$cw_path, 依據 = m$basis,
  判斷 = "", 意見 = "",
  系統用 = paste(m$formID, m$groupID, m$item_uid, sep = "|"),
  check.names = FALSE, stringsAsFactors = FALSE)

# 4 題組對應：對照表的構面 ↔ 題庫的題組 ----
g <- cand("cw_group_map.csv")
g <- g[order(conf_order(g$confidence), g$instrument, g$cw_path), ]
gmap <- data.frame(
  編號 = num("G", nrow(g)),
  信心 = zh(g$confidence, CONF),
  問卷 = zh(g$instrument, c(P = "家長", T = "教保／教師", C = "兒童")),
  對照表路徑 = g$cw_path, 名稱 = g$label, 最底層 = ifelse(g$leaf %in% c("TRUE", "1"), "是", ""),
  對到題庫題組 = ifelse(nzchar(g$groupID), grp_label(g$groupID), ""),
  建議新題組 = g$proposed_groupID, 題數 = g$members, 依據 = g$basis,
  判斷 = "", 意見 = "",
  系統用 = paste(g$instrument, g$cw_path, sep = "|"),
  check.names = FALSE, stringsAsFactors = FALSE)

# 5 題組上下層 ----
p <- cand("cw_group_paths.csv")
p <- p[order(p$formID, p$parentID, p$groupID), ]
tree <- data.frame(
  編號 = num("T", nrow(p)),
  信心 = zh(p$confidence, CONF),
  題本 = p$formID, 上層題組 = grp_label(p$parentID), 題組 = grp_label(p$groupID),
  題數 = p$n_items, 題庫已有這層 = ifelse(p$in_kit_tree %in% c("TRUE", "1"), "是", "否"),
  對照表路徑 = p$cw_path,
  判斷 = "", 意見 = "",
  系統用 = paste(p$formID, p$groupID, p$parentID, sep = "|"),
  check.names = FALSE, stringsAsFactors = FALSE)

# 6 遺漏代碼 ----
a <- cand("audit.csv")
mc <- a[a$check == "missing_code_scheme", ]
uid <- sub("\\|.*$", "", mc$key); ver <- sub("^[^|]*\\|", "", mc$key)
miss <- data.frame(
  編號 = num("N", nrow(mc)),
  題號 = uid, 版 = ver, 題目 = iv_label(uid, ver),
  也是選項代碼 = ifelse(grepl("也是選項代碼", mc$detail), "是", ""),
  說明 = mc$detail,
  判斷 = "", 意見 = "",
  系統用 = mc$key,
  check.names = FALSE, stringsAsFactors = FALSE)
miss <- miss[order(miss$也是選項代碼 != "是", miss$題號), ]
miss$編號 <- num("N", nrow(miss))
MISS_CHOICE <- c("登錄錯了，改成標準遺漏代碼", "這個代碼是真的選項", "不確定")

# 7 其他要注意 ----
o <- a[a$check != "missing_code_scheme", ]
lab <- AUDIT_KIT_CHECKS$label[match(o$check, AUDIT_KIT_CHECKS$check)]
oth <- data.frame(
  編號 = num("A", nrow(o)), 類別 = lab, 對象 = o$key, 說明 = o$detail,
  處理 = "", 意見 = "",
  系統用 = paste(o$check, o$key, sep = "|"),
  check.names = FALSE, stringsAsFactors = FALSE)
OTHER_CHOICE <- c("要改", "不用改", "不確定")

# 8 待決定 ----
dec <- data.frame(
  編號 = c("D1", "D2", "D3", "D4"),
  問題 = c(
    "「父親親職參與」和「父職參與」要合併成一個題組嗎？（fatherinvo01n、03n 在前者；02n、04n、05n 在後者）",
    "teacherinfo：兩份 .sav（M03M036 家長、M36M072 家長）都沒有這個變項，只有對照表說 M03M036 有。哪個對？",
    "M36 世代 36–72 月齡也有教保問卷檔（data3）。草稿寫「教保只有 M03」要改嗎？",
    "0.2 有很多「題組」其實是問卷的段落名稱（例如 健康問卷資料、孩子與家庭基本資料）。要從題組名冊拿掉嗎？"),
  建議 = c("合併，保留「父親親職參與」", "以 .sav 為準：兩個都沒有", "改：M36 也有", "拿掉，段落改記在題目的 block"),
  決定 = "", 意見 = "",
  check.names = FALSE, stringsAsFactors = FALSE)

# 已經回覆過的決定直接帶入
for (f in list.files("data-raw/kit_decisions", "^review_.*\\.csv$", full.names = TRUE)) {
  done <- read.csv(f, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = "character")
  k <- match(dec$編號, done$id)
  dec$決定[!is.na(k)] <- done$decision[k[!is.na(k)]]
  dec$意見[!is.na(k)] <- done$comment[k[!is.na(k)]]
}

# 說明 ----
counts <- c(nrow(dec), nrow(rel), nrow(blk), nrow(mem), nrow(gmap), nrow(tree), nrow(miss), nrow(oth))
sheets <- c("待決定", "題目關係", "區塊狀態", "題組成員", "題組對應", "題組上下層", "遺漏代碼",
            "其他要注意")
what <- c(
  "幾個要 PI 或團隊拍板的問題。",
  "兩個題目版本之間的關係：換題號（例如 fatherinvo01 → fatherinvo01n）或改版。「已進題庫 = 是」的是信心高、先放進去的，請抽看；「否」的請逐筆判斷。",
  "某個資料檔裡，一整段題目沒問、只問部分人，或從前一波帶入。大多是親友版不問的段落。",
  "對照表顯示某題應該屬於某個題組，但題庫還沒有。信心「低」的多半是題組還不存在、要新建。",
  "對照表的構面名稱對到題庫的哪個題組；對不到的有建議的新代號。",
  "對照表裡題組的上下層（例如 社會情緒 > 氣質 > 外放性），比 0.2 的分層更細。",
  "0.2 題庫登錄的遺漏代碼（例如「拒答:5」）不在資料檔的遺漏代碼方案裡（7777 拒答、8888 不知道、9996、9999）。「也是選項代碼 = 是」的最要緊：5 同時也是答案選項。",
  "其他自動檢查找到的地方：只有一題的題組、空的題組、文字有換行或簡體字、沒有角色的題目等。")
intro <- data.frame(工作表 = sheets, 筆數 = counts, 內容 = what, stringsAsFactors = FALSE)
howto <- c(
  "kitItemBank 0.3 候選確認表",
  paste0("產生日期：", format(Sys.Date()), "；題庫版本：2026.1（草稿）"),
  "",
  "怎麼填：",
  "1. 灰色欄是電腦整理的，請不要改。黃色欄「判斷／決定／處理」用下拉選單選，「意見」自由寫。",
  "2. 不同意或要修改時，請在「意見」寫正確的內容（例如正確的題組名稱）。",
  "3. 不用一次看完：可以先看「待決定」，再看各表「信心 = 低／中」的列。",
  "4. 最右邊「系統用」欄是匯回時對應用的，請不要改、不要刪。",
  "5. 可以排序、篩選；但請不要新增或刪除列。",
  "")

# 寫檔 ----
wb <- openxlsx::createWorkbook()
hd <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DDE3EA", wrapText = TRUE,
                            border = "bottom", valign = "center")
grey <- openxlsx::createStyle(fontColour = "#555555", wrapText = TRUE, valign = "top")
yellow <- openxlsx::createStyle(fgFill = "#FFF4C2", wrapText = TRUE, valign = "top",
                                border = "TopBottomLeftRight", borderColour = "#E0C860")
sys <- openxlsx::createStyle(fontColour = "#AAAAAA", fontSize = 8)

openxlsx::addWorksheet(wb, "說明")
openxlsx::writeData(wb, "說明", howto, startRow = 1, colNames = FALSE)
openxlsx::addStyle(wb, "說明", openxlsx::createStyle(fontSize = 14, textDecoration = "bold"),
                   rows = 1, cols = 1)
openxlsx::writeData(wb, "說明", intro, startRow = length(howto) + 2, headerStyle = hd)
openxlsx::setColWidths(wb, "說明", 1:3, c(16, 8, 90))
openxlsx::addStyle(wb, "說明", openxlsx::createStyle(wrapText = TRUE, valign = "top"),
                   rows = length(howto) + 2 + seq_len(nrow(intro)), cols = 3)

add_sheet <- function(name, df, input_cols, choices, widths = NULL) {
  openxlsx::addWorksheet(wb, name)
  openxlsx::writeData(wb, name, df, headerStyle = hd, withFilter = TRUE)
  n <- nrow(df); rows <- seq_len(n) + 1L
  ic <- match(input_cols, names(df))
  openxlsx::addStyle(wb, name, grey, rows = rows, cols = setdiff(seq_along(df), ic),
                     gridExpand = TRUE)
  openxlsx::addStyle(wb, name, yellow, rows = rows, cols = ic, gridExpand = TRUE)
  openxlsx::addStyle(wb, name, sys, rows = rows, cols = ncol(df), gridExpand = TRUE)
  if (n) openxlsx::dataValidation(wb, name, cols = ic[1], rows = rows, type = "list",
                                  value = paste0('"', paste(choices, collapse = ","), '"'))
  openxlsx::freezePane(wb, name, firstRow = TRUE, firstCol = TRUE)
  w <- widths %||% rep(14, ncol(df))
  openxlsx::setColWidths(wb, name, seq_along(df), w)
}
add_sheet("待決定", dec, c("決定", "意見"), c("照建議", "不照建議（寫在意見）", "再討論"),
          c(6, 70, 30, 18, 40))
add_sheet("題目關係", rel, c("判斷", "意見"), AGREE,
          c(8, 7, 5, 8, 18, 5, 36, 18, 5, 36, 8, 7, 7, 13, 40, 10, 28, 10))
add_sheet("區塊狀態", blk, c("判斷", "意見"), AGREE,
          c(8, 7, 5, 12, 8, 14, 22, 12, 22, 10, 50, 10, 28, 10))
add_sheet("題組成員", mem, c("判斷", "意見"), AGREE,
          c(8, 5, 10, 20, 36, 20, 10, 7, 24, 30, 30, 10, 28, 10))
add_sheet("題組對應", gmap, c("判斷", "意見"), AGREE,
          c(8, 5, 10, 36, 18, 7, 20, 22, 6, 30, 10, 28, 10))
add_sheet("題組上下層", tree, c("判斷", "意見"), AGREE,
          c(8, 5, 10, 22, 22, 6, 10, 40, 10, 28, 10))
add_sheet("遺漏代碼", miss, c("判斷", "意見"), MISS_CHOICE,
          c(8, 18, 5, 36, 10, 60, 24, 28, 10))
add_sheet("其他要注意", oth, c("處理", "意見"), OTHER_CHOICE,
          c(8, 22, 30, 60, 10, 28, 10))

openxlsx::saveWorkbook(wb, out, overwrite = TRUE)
cat("wrote", out, "\n")
print(intro[, 1:2], row.names = FALSE)
