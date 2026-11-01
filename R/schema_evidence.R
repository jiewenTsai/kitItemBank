# R/schema_evidence.R — 證據層：分析紀錄和統計結果的三張表（加在 KIT_SCHEMA 後面）
#
# 這個檔照字母順序排在 schema.R 之後讀，所以 KIT_SCHEMA 已經有原本的 17 張表；
# 這裡只往後加，不改原本的定義。read_kit() 找不到這三個 CSV 時回傳空表。
#
# 只存「壓縮過的彙總」：人數 1 到 4 寫成 "<5"，有效人數少於 10 不算平均、標準差、百分位數。
# 原始資料（一列一個孩子）永遠不進題庫。統計怎麼算見 R/stats_items.R、R/stats_groups.R。

# 一次分析：用哪一個資料檔、哪一版題庫、哪個軟體、誰跑的、算不算正式結果
KIT_SCHEMA[["analyses"]] <- list(
  cols = list(analysisID = kcol(req = TRUE), datasetID = kcol(req = TRUE),
              bank_version = kcol(), method = kcol(), software = kcol(),
              date = kcol("date"), run_by = kcol(), official = kcol(enum = "flag01"),
              note = kcol()),
  key = "analysisID",
  fk = list(list(cols = "datasetID", table = "datasets")))

# 題項統計：一個變項一列（人數、各遺漏代碼的人數、平均數、百分位數、選項次數）
KIT_SCHEMA[["item_stats"]] <- list(
  cols = list(analysisID = kcol(req = TRUE), varname = kcol(req = TRUE),
              item_uid = kcol(), itemVer = kcol(), value_type = kcol(),
              n_total = kcol(), n_valid = kcol(), n_sysmis = kcol(),
              missing = kcol(), miss_rate = kcol(),
              mean = kcol(), sd = kcol(), p5 = kcol(), p50 = kcol(), p95 = kcol(),
              freq = kcol(), note = kcol()),
  key = c("analysisID", "varname"),
  fk = list(list(cols = "analysisID", table = "analyses"),
            list(cols = "item_uid", table = "items")))

# 題組統計：每個題組一列（varname 空白：α、ω、特徵值比、配適度），
# 加上題組裡每一題一列（varname：題目總分相關、刪題後 α、負荷量、反向標記）
KIT_SCHEMA[["group_stats"]] <- list(
  cols = list(analysisID = kcol(req = TRUE), groupID = kcol(req = TRUE), varname = kcol(),
              item_uid = kcol(), n_items = kcol(), n = kcol(), cor_type = kcol(),
              alpha = kcol(), omega_total = kcol(), eigen_ratio = kcol(),
              cfi = kcol(), tli = kcol(), rmsea = kcol(), rms = kcol(),
              r_drop = kcol(), alpha_if_deleted = kcol(), loading = kcol(),
              bank_reverse = kcol(enum = "flag01"), psych_reversed = kcol(enum = "flag01"),
              note = kcol()),
  key = c("analysisID", "groupID", "varname"),
  fk = list(list(cols = "analysisID", table = "analyses"),
            list(cols = "groupID", table = "groups"),
            list(cols = "item_uid", table = "items")))
