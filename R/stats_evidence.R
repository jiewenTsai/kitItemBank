# R/stats_evidence.R — 證據層：一次分析的紀錄（analyses）和下載的 Excel
#
# stats_analysis() 把題項統計、題組統計和一列分析紀錄包成題庫的三張表
# （analyses、item_stats、group_stats），維護者照一般的審核流程加進題庫。
# stats_workbook() 做給人看的 Excel：第一張是說明（題庫版本、資料檔、日期、壓縮規則）。

#' 分析方法的文字
#' @noRd
STATS_METHOD <- paste(
  "item_stats: n, missing codes, mean, sd, P5/P50/P95, category counts;",
  "group_stats: psych::alpha(check.keys=TRUE), psych::fa(nfactors=1, cor='poly'),",
  "omega_total from loadings; suppression: counts 1-4 as '<5', no mean/sd/percentiles",
  "when valid n < 10, no cross-tabs"
)

#' 軟體版本：R 和 psych
#' @noRd
stats_software <- function() {
  psy <- if (requireNamespace("psych", quietly = TRUE)) {
    as.character(utils::packageVersion("psych"))
  } else "?"
  paste0("R ", getRversion(), "; psych ", psy)
}

#' Run the evidence statistics for one data file
#'
#' Runs [item_stats()] and [group_stats()] and wraps the results as kit tables:
#' one row in `analyses` and the suppressed aggregates in `item_stats` and
#' `group_stats`, all sharing one `analysisID`. Only aggregates are returned; the
#' data are not kept.
#'
#' @inheritParams item_stats
#' @param run_by Who ran the analysis.
#' @param official `TRUE` if these are the official statistics of this data file.
#' @param note A note for the `analyses` row.
#' @param bank_version Bank version written to the record.
#' @param date Date of the analysis.
#' @param analysisID ID of the analysis (default: datasetID plus a time stamp).
#' @return A named list of three data frames: `analyses`, `item_stats`, `group_stats`.
#' @export
stats_analysis <- function(data, kit, datasetID, run_by = "", official = FALSE, note = "",
                           bank_version = kit_version(), date = kit_today(),
                           analysisID = NULL) {
  if (!datasetID %in% kit$datasets$datasetID) {
    # 原文：題庫裡沒有資料檔「 | 」。
    stop("\u984c\u5eab\u88e1\u6c92\u6709\u8cc7\u6599\u6a94\u300c", datasetID, "\u300d\u3002", call. = FALSE)
  }
  it <- item_stats(data, kit, datasetID)
  gs <- group_stats(data, kit, datasetID)
  stats_bundle(it, gs, datasetID, analysisID = analysisID, run_by = run_by,
               official = official, note = note, bank_version = bank_version, date = date)
}

#' 題項統計 + 題組統計 + 一列分析紀錄 → 題庫的三張表
#' @noRd
stats_bundle <- function(items, groups, datasetID, analysisID = NULL, run_by = "",
                         official = FALSE, note = "", bank_version = kit_version(),
                         date = kit_today()) {
  analysisID <- analysisID %||% stats_new_id(datasetID)
  an <- conform_kit_table(data.frame(
    analysisID = analysisID, datasetID = datasetID,
    bank_version = if (is.na(bank_version %||% NA)) "" else bank_version,
    method = STATS_METHOD, software = stats_software(),
    date = format(as.Date(date), "%Y-%m-%d"), run_by = trimws(run_by %||% ""),
    official = if (isTRUE(official)) "1" else "0", note = trimws(note %||% ""),
    stringsAsFactors = FALSE
  ), "analyses")
  list(
    analyses = an,
    item_stats = conform_kit_table(cbind(analysisID = rep(analysisID, nrow(items)), items,
                                         stringsAsFactors = FALSE), "item_stats"),
    group_stats = conform_kit_table(cbind(analysisID = rep(analysisID, nrow(groups)), groups,
                                          stringsAsFactors = FALSE), "group_stats")
  )
}

#' 分析代號：AN_<datasetID>_<年月日時分秒>
#' @noRd
stats_new_id <- function(datasetID) {
  paste0("AN_", datasetID, "_", format(kit_now(), "%Y%m%d%H%M%S"))
}

#' Excel workbook of one analysis
#'
#' @param res The result of [stats_analysis()].
#' @param kit The kit used (for the data file's label and missing codes).
#' @return A named list of data frames for `writexl::write_xlsx()`; the first sheet is
#'   說明 (bank version, data file, date, suppression rules).
#' @export
stats_workbook <- function(res, kit) {
  an <- res$analyses
  d <- an$datasetID[1]
  ds <- kit$datasets[kit$datasets$datasetID == d, , drop = FALSE]
  mc <- stats_missing_codes(kit, d)
  row <- function(k, x) data.frame(k = rep(k, length(x)), x = as.character(x),
                                   stringsAsFactors = FALSE)
  info <- rbind(
    # 原文：這個檔案 | KIT 幼兒發展調查資料庫一個資料檔的題項統計和題組統計（證據層）。只有彙總數字，沒有任何一個人的資料。
    row("\u9019\u500b\u6a94\u6848", "KIT \u5e7c\u5152\u767c\u5c55\u8abf\u67e5\u8cc7\u6599\u5eab\u4e00\u500b\u8cc7\u6599\u6a94\u7684\u984c\u9805\u7d71\u8a08\u548c\u984c\u7d44\u7d71\u8a08\uff08\u8b49\u64da\u5c64\uff09\u3002\u53ea\u6709\u5f59\u7e3d\u6578\u5b57\uff0c\u6c92\u6709\u4efb\u4f55\u4e00\u500b\u4eba\u7684\u8cc7\u6599\u3002"),
    # 原文：題庫版本
    row("\u984c\u5eab\u7248\u672c", an$bank_version),
    # 原文：資料檔 | （施測  | ，題本  | ）
    row("\u8cc7\u6599\u6a94", paste0(d, if (nrow(ds)) paste0("\uff08\u65bd\u6e2c ", ds$adminID[1], "\uff0c\u984c\u672c ", ds$formID[1], "\uff09") else "")),
    # 原文：分析代號
    row("\u5206\u6790\u4ee3\u865f", an$analysisID),
    # 原文：日期
    row("\u65e5\u671f", an$date),
    # 原文：執行者
    row("\u57f7\u884c\u8005", an$run_by),
    # 原文：正式結果 | 是 | 否
    row("\u6b63\u5f0f\u7d50\u679c", if (an$official == "1") "\u662f" else "\u5426"),
    # 原文：軟體
    row("\u8edf\u9ad4", an$software),
    # 原文：遺漏代碼 | 方案  | ：
    row("\u907a\u6f0f\u4ee3\u78bc", if (length(mc$code)) paste0("\u65b9\u6848 ", mc$scheme, "\uff1a",
                                            # 原文：、
                                            paste0(stats_format_code(mc$code), " ", mc$label, collapse = "\u3001"))
        # 原文：這個資料檔沒有記下遺漏代碼方案
        else "\u9019\u500b\u8cc7\u6599\u6a94\u6c92\u6709\u8a18\u4e0b\u907a\u6f0f\u4ee3\u78bc\u65b9\u6848"),
    # 原文：壓縮規則
    row("\u58d3\u7e2e\u898f\u5247", c(
      # 原文：次數 1 到 4 一律寫成「<5」（選項次數、各遺漏代碼人數、系統遺漏人數）；0 照寫。
      "\u6b21\u6578 1 \u5230 4 \u4e00\u5f8b\u5beb\u6210\u300c<5\u300d\uff08\u9078\u9805\u6b21\u6578\u3001\u5404\u907a\u6f0f\u4ee3\u78bc\u4eba\u6578\u3001\u7cfb\u7d71\u907a\u6f0f\u4eba\u6578\uff09\uff1b0 \u7167\u5beb\u3002",
      # 原文：有效人數少於 10 時，不列平均數、標準差和百分位數；題組完整作答人數少於 10 時不計算。
      "\u6709\u6548\u4eba\u6578\u5c11\u65bc 10 \u6642\uff0c\u4e0d\u5217\u5e73\u5747\u6578\u3001\u6a19\u6e96\u5dee\u548c\u767e\u5206\u4f4d\u6578\uff1b\u984c\u7d44\u5b8c\u6574\u4f5c\u7b54\u4eba\u6578\u5c11\u65bc 10 \u6642\u4e0d\u8a08\u7b97\u3002",
      # 原文：不做交叉表：每個數字只描述一個變項或一個題組。
      "\u4e0d\u505a\u4ea4\u53c9\u8868\uff1a\u6bcf\u500b\u6578\u5b57\u53ea\u63cf\u8ff0\u4e00\u500b\u8b8a\u9805\u6216\u4e00\u500b\u984c\u7d44\u3002",
      # 原文：遺漏代碼先設成遺漏值，才計算平均數、標準差、百分位數和信度。
      "\u907a\u6f0f\u4ee3\u78bc\u5148\u8a2d\u6210\u907a\u6f0f\u503c\uff0c\u624d\u8a08\u7b97\u5e73\u5747\u6578\u3001\u6a19\u6e96\u5dee\u3001\u767e\u5206\u4f4d\u6578\u548c\u4fe1\u5ea6\u3002"
    )),
    # 原文：題組統計
    row("\u984c\u7d44\u7d71\u8a08", c(
      # 原文：題組用這個題本在題庫裡的正式題組（scheme = official），資料檔裡至少有 2 題才算。
      "\u984c\u7d44\u7528\u9019\u500b\u984c\u672c\u5728\u984c\u5eab\u88e1\u7684\u6b63\u5f0f\u984c\u7d44\uff08scheme = official\uff09\uff0c\u8cc7\u6599\u6a94\u88e1\u81f3\u5c11\u6709 2 \u984c\u624d\u7b97\u3002",
      # 原文：先照題庫的反向標記反向計分（最大選項 + 最小選項 − 原值），只用每一題都有效的人。
      "\u5148\u7167\u984c\u5eab\u7684\u53cd\u5411\u6a19\u8a18\u53cd\u5411\u8a08\u5206\uff08\u6700\u5927\u9078\u9805 + \u6700\u5c0f\u9078\u9805 \u2212 \u539f\u503c\uff09\uff0c\u53ea\u7528\u6bcf\u4e00\u984c\u90fd\u6709\u6548\u7684\u4eba\u3002",
      # 原文：α、r_drop（題目和其他題總分的相關）、刪題後 α 來自 psych::alpha(check.keys = TRUE)；psych_reversed = 1 表示 psych 自己把這一題反向，題庫的反向標記可能要再看。
      "\u03b1\u3001r_drop\uff08\u984c\u76ee\u548c\u5176\u4ed6\u984c\u7e3d\u5206\u7684\u76f8\u95dc\uff09\u3001\u522a\u984c\u5f8c \u03b1 \u4f86\u81ea psych::alpha(check.keys = TRUE)\uff1bpsych_reversed = 1 \u8868\u793a psych \u81ea\u5df1\u628a\u9019\u4e00\u984c\u53cd\u5411\uff0c\u984c\u5eab\u7684\u53cd\u5411\u6a19\u8a18\u53ef\u80fd\u8981\u518d\u770b\u3002",
      # 原文：負荷量來自 psych::fa(nfactors = 1, cor = \"poly\")（cor_type 寫 pearson 表示 polychoric 算不出來，改用 Pearson）。ω_total = (Σλ)² / ((Σλ)² + Σ(1 − λ²))。eigen_ratio = 第 1 個特徵值 / 第 2 個特徵值。
      "\u8ca0\u8377\u91cf\u4f86\u81ea psych::fa(nfactors = 1, cor = \"poly\")\uff08cor_type \u5beb pearson \u8868\u793a polychoric \u7b97\u4e0d\u51fa\u4f86\uff0c\u6539\u7528 Pearson\uff09\u3002\u03c9_total = (\u03a3\u03bb)\u00b2 / ((\u03a3\u03bb)\u00b2 + \u03a3(1 \u2212 \u03bb\u00b2))\u3002eigen_ratio = \u7b2c 1 \u500b\u7279\u5fb5\u503c / \u7b2c 2 \u500b\u7279\u5fb5\u503c\u3002",
      # 原文：CFI、TLI、RMSEA、rms 只供參考，4 題以上才列。
      "CFI\u3001TLI\u3001RMSEA\u3001rms \u53ea\u4f9b\u53c3\u8003\uff0c4 \u984c\u4ee5\u4e0a\u624d\u5217\u3002"
    )),
    # 原文：給維護者 | 要把結果加進題庫，用「下載題庫列」的三個 CSV（analyses、item_stats、group_stats），照一般的審核流程加入。
    row("\u7d66\u7dad\u8b77\u8005", "\u8981\u628a\u7d50\u679c\u52a0\u9032\u984c\u5eab\uff0c\u7528\u300c\u4e0b\u8f09\u984c\u5eab\u5217\u300d\u7684\u4e09\u500b CSV\uff08analyses\u3001item_stats\u3001group_stats\uff09\uff0c\u7167\u4e00\u822c\u7684\u5be9\u6838\u6d41\u7a0b\u52a0\u5165\u3002")
  )
  # 原文：項目 | 內容
  names(info) <- c("\u9805\u76ee", "\u5167\u5bb9")
  rownames(info) <- NULL
  stats::setNames(list(info, res$analyses, res$item_stats, res$group_stats),
                  # 原文：說明 | 分析紀錄 | 題項統計 | 題組統計
                  c("\u8aaa\u660e", "\u5206\u6790\u7d00\u9304", "\u984c\u9805\u7d71\u8a08", "\u984c\u7d44\u7d71\u8a08"))
}
