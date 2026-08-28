# kitItemBank：KIT 題庫管理工具

維護並查詢 KIT 題庫（`itemBank`、`itemDim`、`itemWave`、`dimMeta`），支援三個 domain（家長／兒童青少年／教保教師），並提供進階篩選、跨波次對照、構面報告與問卷組卷 Word 輸出。為了資料安全性考量，本工具目前以**本機安裝與執行**為主。

同系列工具：[kitJoin](https://github.com/jiewenTsai/kitJoin)（多波次資料串接）

---

## 功能

- **Tab 1 編修**：`itemDim`／`itemWave` 矩陣視覺化編修；Excel 匯入／匯出；domain 資料讀寫
- **Tab 2 進階預覽**：跨 `itemBank`／`itemDim`／`itemWave` 即時篩選；跨波次變項對照表
- **Tab 3 構面報告**：編修 `dimMeta`；產出構面題項對應 Word 報告
- **Tab 4 組卷**：區塊式選題、Likert 量尺設定、預覽與 **Word 問卷下載**

右上角可切換 domain：**家長**／**兒童青少年**／**教保教師**。

---

## 使用：快速開始

### 方法 1. Windows（建議）

1. 安裝 [R 4.2 以上](https://cran.csie.ntu.edu.tw/)（含 RStudio 或 Positron 亦可）。
2. 將本專案資料夾解壓到本機（例如 `Documents/kitItmBank`）。
3. 在 R 中設定工作目錄至專案根目錄，執行：

```r
install.packages(c(
  "shiny", "bslib", "DT", "dplyr", "readr", "stringr", "tidyr",
  "writexl", "readxl", "officer", "flextable", "jsonlite"
))
shiny::runApp("app.R")
```

> 瀏覽器會自動開啟本機介面。若下載 Word 無反應，請改用 Chrome 或檢查瀏覽器是否封鎖下載。

### 方法 2. 預載 Excel 題庫（選用）

將三份題庫 Excel 放入 `files/` 目錄，啟動時會**優先**載入：

| 檔名 | domain |
|------|--------|
| `家長.xlsx` | parent |
| `兒童青少年.xlsx` | child_adolescent |
| `教保教師.xlsx` | childcare_teacher |

若 `files/` 無對應檔案，則改讀 `data/<domain>/` 下的 CSV。

---

## 使用流程摘要

| 步驟 | 分頁 | 動作 |
|------|------|------|
| 1 | 編修 | 選 domain → 檢視／編修 `itemDim`、`itemWave` → 必要時 Excel 匯入／匯出 |
| 2 | 進階預覽 | 設定篩選條件 → 檢視題項清單 → 產出跨波次對照表 |
| 3 | 構面報告 | 編修 `dimMeta` → 產出構面報告 Word |
| 4 | 組卷 | 新增區塊 → 選題 → 設定量尺／說明 → 預覽 → **下載 Word 問卷** |

---

## 資料檔案佈局

```
data/
├── parent/
│   ├── itemBank.csv
│   ├── itemDim.csv
│   ├── itemWave.csv
│   └── dimMeta.csv
├── child_adolescent/
│   └── （同上四張）
└── childcare_teacher/
    └── （同上四張）

files/          # 選用：三 domain 的 Excel 題庫 bundle
```

四張核心表說明：

| 表 | 說明 |
|----|------|
| `itemBank` | 長表，一列一個 `itemID`×`itemVer`（題文、value、note 等） |
| `itemDim` | 寬表，題項 × 構面 0/1 矩陣 |
| `itemWave` | 寬表，題項 × 波次 0/1 矩陣 |
| `dimMeta` | 構面 metadata（名稱、信度、備註） |

---

## 組卷：矩陣題組為何有時合併、有時不會？

組卷 Word 輸出時，連續的 Likert 題可能自動併成**矩陣表格**（表頭共用選項、各題為列），也可能維持**逐題單列**格式。規則如下：

| 條件 | 結果 |
|------|------|
| 連續多題 Likert，且**量尺相同**（`value` 解析結果相同，或皆無 `value` 而共用同一區塊量尺） | 自動合併為矩陣 |
| 中間夾了不同題型（開放題、複選、分類單選等） | 合併中斷，前後分開 |
| `value` 量尺文字不同（例如「從不如此」vs「從未如此」） | 不合併 |
| 教育程度、國籍、工作狀態等**分類單選** | 永不自動矩陣 |
| 手動設為「單題」 | 不合併 |
| 手動「合併為矩陣題組」 | 強制矩陣（分類單選除外） |

常見狀況：

- **`value` 僅寫「1~6點量表」而未展開選項文字**（如 `socB19`）：系統無法從題庫讀取選項，改採**該區塊設定的 Likert 量尺**；若與相鄰題共用同一區塊量尺，仍會自動合併。
- **同一區塊內不同量尺的 Likert 題**：請拆成不同區塊，或在中間插入說明／不同題型以切斷自動合併。
- **手動控制**：組卷分頁選取多題後，可使用「合併為矩陣題組／改為單題／恢復自動」按鈕。

矩陣表頭的量尺優先順序：**題目 `value` 解析結果** → **區塊 Likert 設定**。

---

## 開發／測試

```r
shiny::runApp("app.R")
```

```bash
Rscript tests/test_dim_meta_preview.R
Rscript tests/import_domain_xlsx.R   # 從 Excel 更新 data/ CSV
```

詳細規格見 `KIT_itembank_shiny_spec.md`。

---

## 授權

MIT License（與 [kitJoin](https://github.com/jiewenTsai/kitJoin) 相同）
