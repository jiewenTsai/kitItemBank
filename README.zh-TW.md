# kitItemBank：KIT 題庫管理工具

[English](README.md) | 繁體中文

查詢與維護 KIT 題庫（臺灣幼兒發展調查資料庫）。三個題庫：家長、兒童青少年、教保人員。

## 兩個版本

目前同時有兩個版本在線上，同一個 repo，用分支區分。

| | 0.2（寬表版） | 0.3（長格式版，現行開發） |
|---|---|---|
| 雲端版 | <https://01a1981a-700b-e470-a554-3b8cc7de9331.share.connect.posit.cloud/> | <https://jiewentsai-kititembank.share.connect.posit.cloud/> |
| 程式碼 | 分支 [`v0.2`](https://github.com/jiewenTsai/kitItmBank/tree/v0.2)，tag `0.2.0`；之後只修錯 | 分支 `main`，tag `0.3.0` 起 |
| 安裝 | `pak::pkg_install("jiewenTsai/kitItmBank@v0.2")` | `pak::pkg_install("jiewenTsai/kitItmBank")` |
| 題庫存法 | 4 張寬表（`inst/extdata/bank/`）：一列一題，波次、構面各一欄 0/1，手工維護 | 長格式 kit：表一張一個 CSV（`inst/extdata/kit/`），從 .sav 表頭、跨波次對照表、資料使用說明建出來 |
| 波次代碼 | `M036大`、`M048小` 等 | 施測代號 `M36M036`、`M36M084G01`，畫面附白話說明；舊代碼、`M36W84`、`84M`、`G01`、「國小一年級」都搜得到 |
| 「這題在哪一波」 | 看 itemWave 表人工填的 0/1 | 看每次施測每個資料檔實際有哪些變項（`dataset_vars`） |
| 題目改版、換題號 | 記在題目的備註 | 記在題目關係表（改版、換題號、拆開、合併），附誰決定、根據哪份文件 |
| 題組 | 構面 0/1 表；有些「構面」其實是問卷段落名稱 | 題組名冊＋每個題本一棵題組樹；段落名稱改記在題目的區塊 |
| 改題庫 | 下載整份改好的題庫 Excel，維護者用 `update_bank()` 換進去 | 下載修改包（只有改了什麼），在「審核」頁檢查後下載套用後的題庫，由技術人員放進 repo |

**怎麼選：** 平常查題目、做構面報告兩版都可以；要知道「哪一次施測、哪個資料檔真的有這題」、
要 codebook 或 SPSS 標籤語法、要維護題庫，用 0.3。

0.3 的設計見 `kitItemBank_重構設計_v2.md`，每一版的改變見 [NEWS.md](NEWS.md)。

## 0.3 的頁面

**查詢**

- **題項查詢**：用關鍵字、波次、構面找題目；看一題在各施測怎麼問、有哪些版本；下載查詢結果、一題的沿革單（Word）。
- **構面查詢**：題組的題目、各波次信度、網路圖；構面報告（Word、HTML）、題組各波次題目表（Excel）。
- **施測**：選一次施測，看它的資料檔、哪些區塊沒問或只問一部分、變項依問卷順序排列；
  下載施測題目清單（codebook，Excel）、SPSS 標籤語法（`.sps`）。
- **比較**：選兩個資料檔，只列出差異。

**維護**

- **待辦事項**：「要注意」清單，依「自己能改／要問老師／要問統計人員」分組，點了就到對應的頁面。
- **題庫編修**：改題組成員、題組資訊、題目文字，下載修改包。
- **改版**：老師要出下一版題本時，匯出現在題本的題目給老師；老師改好的 Excel 匯回，檢查後轉成修改包。
- **併入新施測**：讀新施測的 .sav 表頭（只讀變項名稱和標籤），自動分成相同、新版本、換題號、新題、沒問了等提案。
- **審核**：上傳修改包 → 看改前改後、檢查、影響範圍 → 下載套用後的題庫。
- **統計**（只在本機版開啟）：上傳一個資料檔，算題目統計和題組信度；原始資料不存檔、不上雲端。

app 不寫回任何檔案：改題庫一律走「修改包 → 審核 → 下載 → 技術人員放進 repo」。

## 安裝與開啟

```r
# install.packages("pak")
pak::pkg_install("jiewenTsai/kitItmBank")
kitItemBank::run_kititembank()
```

Windows 也可以雙擊 `run.bat`。需要 R 4.2 以上。

## 題庫資料（0.3）

內建題庫在 `inst/extdata/kit/`，一張表一個 CSV，版本寫在 `BANK_VERSION`（例如 `2026-11-02-v1`：做出這版題庫的日期，加上當天第幾版）。

| 層 | 表 |
|---|---|
| 研究設計 | `cohorts` 世代、`admins` 施測、`datasets` 資料檔、`dataset_vars` 資料檔變項、`block_status` 區塊狀態 |
| 工具 | `forms` 題本、`form_notes` 題本各年度說明 |
| 題目 | `items` 題號、`item_versions` 題目版本、`item_relations` 題目關係、`aliases` 別名 |
| 題組 | `groups` 題組名冊、`group_tree` 題組樹、`group_members` 題組成員、`group_lineage` 題組沿革 |
| 編碼 | `value_codes` 選項、`missing_schemes` 遺漏代碼方案（2016、2020） |
| 其他 | `sources` 來源文件、`contacts` 聯絡人；統計結果 `analyses`、`item_stats`、`group_stats` |

在 R 裡直接用：

```r
library(kitItemBank)
kit <- read_kit()
check_kit(kit)                               # 結構檢查
admin_codebook(kit, "M36W84")                # 一次施測的 codebook
bank <- bank_to_v02(kit, "parent")           # 轉成 0.2 形狀，查詢函式照用
query_items(bank, keyword = "睡眠")
```

### 更新內建題庫（技術人員）

1. 在「審核」頁上傳修改包，確認後下載套用後的題庫（zip）。
2. 把 zip 裡的檔案放進 `inst/extdata/kit/`（含 `changes/` 修改紀錄和新的 `BANK_VERSION`）。
3. 跑 `check_kit(read_kit())` 和測試，`git diff` 看改了什麼，commit、推上 GitHub、打 tag；雲端版重新部署。

## 雲端版

根目錄的 `app.R` 給 Posit Connect Cloud；部署用的套件清單在 `manifest.json`。題目公開、沒有個資，雲端放完整題庫。
雲端寫進去的檔案在 app 重開後就消失，所以 app 本來就不寫檔。

## 開發

```bash
LC_ALL=en_US.UTF-8 Rscript --no-init-file -e 'pkgload::load_all(); run_kititembank()'
LC_ALL=en_US.UTF-8 Rscript --no-init-file -e 'devtools::test()'
```

- 介面一律用 [kitDesign](https://github.com/jiewenTsai/kitDesign)；規則見它的 `brand/KIT設計語言.md`。
- R 程式碼的字串只用 ASCII：寫中文，改完跑 kitDesign 的 `tools/escape_non_ascii.R`。
- 題庫從原始資料重建：`Rscript data-raw/build_kit.R <資料夾>`（只讀 .sav 表頭）。

## 授權

MIT © KIT
