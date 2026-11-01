# kitItemBank：KIT 題庫管理工具

[English](README.md) | 繁體中文

查詢與維護 KIT 題庫（臺灣幼兒發展調查資料庫）。三個題庫：家長、兒童青少年、教保人員。

- **題項查詢**：用關鍵字、波次、構面找題目；看一題在各波次怎麼問、有哪些版本。
- **構面查詢**：看一個構面有哪些題、各波次的信度、構面與題目的網路圖；產出構面報告（Word、HTML）。
- **題庫編修**：改題目的構面、波次歸屬與構面資訊，下載改好的題庫 Excel（含改動紀錄）。

app 不寫回任何檔案：查詢只看；改好的題庫用下載的，交給維護者更新內建題庫。

## 安裝與開啟

```r
# install.packages("pak")
pak::pkg_install("jiewenTsai/kitItmBank")
kitItemBank::run_kititembank()
```

Windows 也可以雙擊 `run.bat`。需要 R 4.2 以上。

## 題庫資料

內建題庫在 `inst/extdata/bank/<題庫>/`：`itemBank.csv`、`itemDim.csv`、`itemWave.csv`、`dimMeta.csv`
（選用 `dimReliability.csv`），版本寫在 `inst/extdata/bank/BANK_VERSION`。

| 表 | 內容 |
|---|---|
| `itemBank` | 一列一題（`itemID` × `itemVer`）：題目、選項、遺漏值、備註等 |
| `itemDim` | 題目 × 構面的 0/1 表 |
| `itemWave` | 題目 × 波次的 0/1 表 |
| `dimMeta` | 一列一個構面：名稱、別名、信度、備註 |

在 R 裡直接用：

```r
library(kitItemBank)
bank <- load_bank("parent")
query_items(bank, keyword = "睡眠", waves = c("M036大", "M048大"))
```

### 更新內建題庫（維護者）

1. 在 app 的「題庫編修」改好，按「下載改好的題庫」。
2. 在 kitItemBank 的原始碼資料夾：

   ```r
   pkgload::load_all()
   update_bank("KIT_parent_2026-11-01_改.xlsx", "parent")
   ```

   它會檢查檔案、寫進 `inst/extdata/bank/parent/`、更新 `BANK_VERSION`。
3. `git diff` 看改了什麼（Excel 的「改動紀錄」工作表也列出每一處），commit、發一版；雲端版重新部署。

## 雲端版

根目錄的 `app.R` 給 Posit Connect Cloud。題目公開、沒有個資，雲端放完整題庫。
雲端寫進去的檔案在 app 重開後就消失，所以 app 本來就不寫檔：改好的題庫一律下載。

## 開發

```bash
LC_ALL=en_US.UTF-8 Rscript --no-init-file -e 'pkgload::load_all(); run_kititembank()'
LC_ALL=en_US.UTF-8 Rscript --no-init-file -e 'devtools::test()'
```

- 介面一律用 [kitDesign](https://github.com/jiewenTsai/kitDesign)；規則見它的 `brand/KIT設計語言.md`。
- R 程式碼的字串只用 ASCII：寫中文，改完跑 kitDesign 的 `tools/escape_non_ascii.R`。
- 每一版的改變見 [NEWS.md](NEWS.md)。

## 授權

MIT © KIT
