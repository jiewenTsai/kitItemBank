# KIT 題庫管理系統 — R Shiny 開發規格書

版本：v0.1（草案，供 Cursor 開發參考）

---

## 1. 系統目的與範圍

維護並查詢 KIT 題庫的四張核心表（`itemBank`、`itemDim`、`itemWave`、`dimMeta`），涵蓋三個 domain（`parent`／`child_adolescent`／`childcare_teacher`）。本規格書只涵蓋以下四項功能，不包含組卷／PDF 輸出（該功能已在先前討論過，但不在本次開發範圍內，之後可作為獨立模組加掛）：

1. `itemDim` / `itemWave` 的視覺化編修
2. 即時題項 filter（跨 `itemBank`／`itemDim`／`itemWave`）
3. 跨波次變項對照表（第 2 項結果的固定文件化）
4. 構面題項對應報告（跨四張表，含信度）

---

## 2. 資料層規格

### 2.1 檔案佈局

```
data/
├── parent/
│   ├── itemBank.csv
│   ├── itemDim.csv      # 原 qMatrix.csv，寬表 0/1 矩陣
│   ├── itemWave.csv     # 寬表 0/1 矩陣
│   └── dimMeta.csv      # 原 metadata.csv
├── child_adolescent/
│   └── （同上四張）
└── childcare_teacher/
    └── （同上四張）
```

> 目前拿到的檔案還叫 `qMatrix.csv`／`metadata.csv`，進 Cursor 後第一件事就是重新命名成 `itemDim.csv`／`dimMeta.csv`，程式碼一律用新名字，避免兩套命名混用。

### 2.2 表格 Schema

**`itemBank`**（長表，一列一個 itemID×itemVer）

| 欄位 | 型別 | 說明 |
|---|---|---|
| `itemID` | chr | 穩定題項身分（＝SPSS 變項名） |
| `itemVer` | int | 內容版本號，`itemID`+`itemVer` 為複合主鍵 |
| `stemID` | chr, 可空 | 多拆單題組的共同題幹（自動偵測，可能需人工校正） |
| `block` | chr | 所屬問卷模組（如「家長」「親友」「兒童」「教保環境」…） |
| `label` | chr | 題目逐字稿 |
| `value` | chr | 數值說明／計分編碼 |
| `note` | chr | 題型、遺漏值代碼、適用版本等自由文字 |

**`itemDim`**（寬表，items × dimensions 二元矩陣）

| 欄位 | 說明 |
|---|---|
| `itemID`, `itemVer` | 複合鍵，對應 `itemBank` |
| `<dimID_1>` … `<dimID_k>` | 每個構面一欄，值為 0/1，欄名即 `dimMeta.dimID` 的實際文字 |

**`itemWave`**（寬表，items × waves 二元矩陣）

| 欄位 | 說明 |
|---|---|
| `itemID`, `itemVer` | 複合鍵 |
| `<wave_1>` … `<wave_n>` | 每個波次一欄（如 `M3`,`M36_小月齡`,`國1_大月齡`），值為 0/1 |

**`dimMeta`**（長表，一列一個構面）

| 欄位 | 說明 |
|---|---|
| `dimID` | 構面識別碼（文字，即 `itemDim` 的欄名） |
| `dimName` | 構面顯示名稱 |
| `reliability` | 信度數值，目前留空待填 |
| `note` | 備註 |

### 2.3 重要實作陷阱

- **欄名不是合法 R 變數名**：`itemDim`／`itemWave` 的欄名含中文、空格、括號（如 `Home(反向題)`、`M36_小月齡`）。程式裡一律用**字串**存取欄位（`.data[[col]]`、`dplyr::all_of(col)`、`rlang::sym(col)`），不要用裸露的 tidyverse NSE 寫法直接打欄名。
- **欄位數量會變動**：家長 domain 的 `itemDim` 目前有 122 個構面欄，`itemWave` 有 24 個波次欄。任何 UI／邏輯都不能寫死欄位清單，一律從 `setdiff(names(df), c("itemID","itemVer"))` 動態取得。
- **讀檔用 `readr::read_csv()`**（不要用 base `read.csv`），並指定 `locale = locale(encoding = "UTF-8")`；檔案是用 `utf-8-sig`（含 BOM）存的，`readr` 預設能正確處理 BOM，base R 不一定。
- **`itemBank.label`／`value` 可能含內嵌換行**，顯示於 DT 表格時建議先 `stringr::str_trunc()` 或做省略號截斷，避免表格列高爆炸。

---

## 3. 應用程式架構

建議用「domain 切換 + 功能分頁」的兩層結構，不用 golem（規模還不到需要那套 production 骨架的程度，一般 modular Shiny app 結構就夠）：

```
app.R
R/
├── data_io.R          # load_domain(domain) / save_matrix(domain, table, df)
├── mod_edit_matrix.R  # 功能1
├── mod_filter.R       # 功能2
├── mod_wave_lookup.R  # 功能3（可 reuse mod_filter 的 reactive）
├── mod_dim_report.R   # 功能4
└── utils.R            # 共用小工具（e.g. 安全欄位存取）
reports/
└── dim_summary_report.qmd   # 功能4用的參數化範本
data/
└── （2.1 的三個 domain 資料夾）
```

**頂層 UI**：一個 `selectInput("domain", ...)` 切換三個 domain，底下用 `tabsetPanel` 放四個功能分頁。每個模組吃同一個 `reactive` 的「目前 domain 四張表」清單（`list(itemBank=, itemDim=, itemWave=, dimMeta=)`），domain 切換時整包重新載入。

**資料載入**（`data_io.R`）：

```r
load_domain <- function(domain, data_root = "data") {
  p <- file.path(data_root, domain)
  list(
    itemBank = readr::read_csv(file.path(p, "itemBank.csv"), locale = readr::locale(encoding = "UTF-8")),
    itemDim  = readr::read_csv(file.path(p, "itemDim.csv"),  locale = readr::locale(encoding = "UTF-8")),
    itemWave = readr::read_csv(file.path(p, "itemWave.csv"), locale = readr::locale(encoding = "UTF-8")),
    dimMeta  = readr::read_csv(file.path(p, "dimMeta.csv"),  locale = readr::locale(encoding = "UTF-8"))
  )
}
```

在 server 端用 `reactiveVal`（不是 `reactive`）持有目前 domain 的四張表，因為功能 1 需要「編輯後更新記憶體內容、按存檔才寫回 CSV」，`reactive` 沒辦法被模組寫入。

---

## 4. 功能規格

### 4.1 `itemDim` / `itemWave` 編修模組（`mod_edit_matrix.R`）

**不要直接把 122 欄的矩陣塞進一個可編輯表格**——欄太多時 `rhandsontable`／`DT` 的可編輯格線在瀏覽器裡幾乎不能用（要一直橫向捲動找欄位，容易點錯格子）。改成「選題目→打勾清單」的 pattern：

- 左側：`itemBank` 的搜尋/選擇清單（可用 `itemID`／`label` 關鍵字篩選，`selectizeInput` 或 `DT` 單選列）。
- 右側：選定的 `(itemID, itemVer)` 對應的所有構面（或波次），用 `shinyWidgets::checkboxGroupButtons` 或一組 `checkboxInput` 呈現目前狀態（勾選 = 該欄值為1），使用者直接勾/取消勾。
- 這個 pattern 對 `itemDim`（選構面）跟 `itemWave`（選波次）完全一樣，寫成同一個模組、用參數區分 `matrix_type = c("dim","wave")` 即可重複使用。
- 下方另外提供一個**唯讀**的總覽表（`DT::datatable`，`scrollX = TRUE`），給需要一次看全貌、抓漏勾的人掃視用——這個不能編輯，純檢視。
- **存檔**：按「儲存」才把 `reactiveVal` 裡更新後的整張表用 `readr::write_csv()`（`na = ""`, UTF-8）覆寫回對應的 CSV。因為主檔用 git 版本控管（前面已經談過，不用 DuckDB），存檔動作本身不用做交易鎖，git 會留下每次修改的完整歷程。

### 4.2 即時題項 Filter 模組（`mod_filter.R`）

**UI**：
- 波次多選（`selectizeInput`，選項來自 `itemWave` 動態欄名）
- 構面多選（`selectizeInput`，選項來自 `itemDim` 動態欄名）
- AND／OR 邏輯切換（`radioButtons`，決定多選條件之間是交集還是聯集）
- 即時結果表（`DT::datatable`），欄位至少含 `itemID`,`itemVer`,`label`,`block`

**邏輯**：
```r
filtered_items <- reactive({
  waves <- input$wave_sel; dims <- input$dim_sel
  iw <- itemWave_df
  id_wave <- if (length(waves)==0) iw[,c("itemID","itemVer")] else
    iw[rowSums(iw[, waves, drop=FALSE] == 1) > 0, c("itemID","itemVer")]  # OR；AND 用 == length(waves)
  id_dim <- if (length(dims)==0) itemDim_df[,c("itemID","itemVer")] else
    itemDim_df[rowSums(itemDim_df[, dims, drop=FALSE] == 1) > 0, c("itemID","itemVer")]
  ids <- dplyr::inner_join(id_wave, id_dim, by=c("itemID","itemVer"))
  dplyr::inner_join(ids, itemBank_df, by=c("itemID","itemVer"))
})
```
這是全案的核心 reactive，功能 3、功能 4 都直接重用它（或它的變形），不要各自重寫一份 join 邏輯。

### 4.3 跨波次變項對照表模組（`mod_wave_lookup.R`）

這是功能 2 的「文件化」版本：把目前篩選出的題目集合，轉成 item × wave 的十字對照表（不是 filter 結果的清單，而是矩陣式呈現，方便列印/交接給非技術同仁）。

- 沿用 4.2 的 `filtered_items()`。
- Pivot：`itemWave_df` 篩到這些 `(itemID,itemVer)` 之後，`tidyr::pivot_longer()` 再依需要轉成好讀的 ✓/空白矩陣（用 `dplyr::case_when(value==1 ~ "✓", TRUE ~ "")`），列上加 `label` 方便閱讀。
- `downloadButton`：輸出成 `.xlsx`（用 `openxlsx` 或 `writexl`，比 csv 更適合當「文件」發出去）。
- 這一頁的定位是「目前的一份快照」，不是即時互動查詢介面——所以下載的檔名建議帶時間戳（`glue::glue("跨波次對照表_{domain}_{Sys.Date()}.xlsx")`），方便日後追溯是哪一天匯出的版本。

### 4.4 構面題項對應報告模組（`mod_dim_report.R`）

**UI**：選 domain（已在頂層）+ 選波次（單選，代表「某一年度」）+「產生報告」按鈕。

**資料組裝邏輯**：
1. 篩出該波次有施測的 `(itemID,itemVer)`（`itemWave` 該欄=1）。
2. 對每個構面（`itemDim` 欄名），找出「該構面=1」且「屬於上一步結果」的題目，join `itemBank` 拿到 `label`。
3. 對每個構面 join `dimMeta` 拿到 `reliability`／`note`。
4. 组成一個 nested 結構（每個構面一組：構面名、信度、底下題目清單），餵給 Quarto 範本。

**輸出**：走參數化 Quarto（不是即時畫面，是要產出「固定文件」）：

```
reports/dim_summary_report.qmd
---
title: "構面題項對應報告"
params:
  domain: "parent"
  wave: "M36"
---
```

Server 端用 `quarto::quarto_render()` 背景執行（建議搭配 `{callr}` 跑在子行程，避免長報告卡住 Shiny 主執行緒），完成後用 `downloadHandler` 把產出的 PDF/Word 給使用者下載。這個 render 邏輯直接沿用先前討論過的「PDF 組卷」機制（同一套 Quarto 參數化 + `quarto_render` 呼叫方式），現在先用在這份報告上，之後要加組卷功能時可以共用同一個 render 輔助函式。

---

## 5. 資料寫回與版本控制策略

- 不用 DuckDB／SQLite（前面已定案，資料量撐不到需要它的規模）。
- 唯一會被 app 寫入的檔案是 `itemDim.csv`／`itemWave.csv`（功能 1 的存檔動作）；`itemBank.csv`／`dimMeta.csv` 這次規格書沒有編修介面，先當唯讀主檔（要改就直接改 CSV 或之後再開一個 `mod_edit_itembank`）。
- 建議整個 `data/` 資料夾進 git，每次「儲存」動作就是一次檔案覆寫，git diff 自然變成稽核記錄，不用另外做 audit log 表。
- Shiny app 啟動時 `load_domain()` 讀進記憶體，中途不會自動重讀檔案；如果多人協作，記得存檔前提醒使用者「先 git pull 確認拿到的是最新版」，這是純檔案式架構下唯一要人工注意的協作風險。

---

## 6. 技術棧與套件清單

| 用途 | 套件 |
|---|---|
| App 框架 | `shiny`, `bslib`（主題／版面） |
| 表格顯示 | `DT` |
| 篩選/選擇 UI | `shinyWidgets` |
| 資料處理 | `dplyr`, `tidyr`, `readr`, `stringr` |
| 報告輸出 | `quarto`（呼叫 `quarto_render()`）, `callr`（背景執行避免卡 UI） |
| Excel 匯出 | `writexl` 或 `openxlsx` |

---

## 7. 本次規格未涵蓋（留待之後）

- PDF 組卷（建立題目區塊、提示語、勾選歸屬）——先前已規劃過表結構相容，但本次四項功能不含這塊 UI。
- `itemBank`／`dimMeta` 本身的編修介面（目前假設用文字編輯器或另開 CSV 直接改）。
- 多人同時線上編輯的鎖定機制（目前假設單人維護 + git 版控就夠）。
