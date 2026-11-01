# R/review_patch.R — 審核修改包：套用前先看清楚改了什麼、會不會壞、影響哪些東西
#
# 審核頁（R/page_review.R）收到一個修改包（R/patch.R 的 Excel）以後：
#   read_review_package()  讀修改包：改動列、base_version，和說明頁裡的原因／決定者／送出者（有的話）
#   review_patch()         一列一列套到目前的題庫：哪些列套不上、改前改後、check_kit() 的結果、
#                          影響到哪些題目／題組／資料檔／施測、版本對不對得上
#   bump_bank_version()    套用後的新題庫版本：2026-11-02-v1 → 同一天 2026-11-02-v2、隔天 2026-11-03-v1
#   change_record()        改動紀錄（一個修改包一個 CSV，放在 inst/extdata/kit/changes/）
#   review_files()         套用後要放進 repo 的檔案：改到的表、BANK_VERSION、改動紀錄
#   review_zip()           把這些檔案包成 zip（審核頁下載用）
# 這裡都不碰 Shiny、不連網路；app 也不寫檔：zip 交給懂技術的人放進 repo、push。

# 改動紀錄的欄位（CSV，一列一個改動）
CHANGE_RECORD_COLS <- c("bank_version", "base_version", "applied_to", "date", "op", "table",
                        "key", "column", "from", "to", "reason", "decided_by", "submitted_by",
                        "approved_by")

# 修改包說明頁裡可以有的人工欄位（舊的修改包沒有，空白就好）
PATCH_META_FIELDS <- c("reason", "decided_by", "submitted_by")

# 畫面上的表名
KIT_TABLE_LABELS <- c(
  # 原文：世代 | 施測 | 資料檔 | 資料檔的變項
  cohorts = "\u4e16\u4ee3", admins = "\u65bd\u6e2c", datasets = "\u8cc7\u6599\u6a94", dataset_vars = "\u8cc7\u6599\u6a94\u7684\u8b8a\u9805",
  # 原文：區塊狀態 | 題本 | 題本沿革 | 題目
  block_status = "\u5340\u584a\u72c0\u614b", forms = "\u984c\u672c", form_notes = "\u984c\u672c\u6cbf\u9769", items = "\u984c\u76ee",
  # 原文：題目的版本 | 題目之間的關係 | 別名
  item_versions = "\u984c\u76ee\u7684\u7248\u672c", item_relations = "\u984c\u76ee\u4e4b\u9593\u7684\u95dc\u4fc2", aliases = "\u5225\u540d",
  # 原文：題組 | 題組的上下層 | 題組成員
  groups = "\u984c\u7d44", group_tree = "\u984c\u7d44\u7684\u4e0a\u4e0b\u5c64", group_members = "\u984c\u7d44\u6210\u54e1",
  # 原文：選項代碼 | 遺漏代碼 | 資料來源
  value_codes = "\u9078\u9805\u4ee3\u78bc", missing_schemes = "\u907a\u6f0f\u4ee3\u78bc", sources = "\u8cc7\u6599\u4f86\u6e90"
)

# 畫面上的欄名（沒列到的用原本的欄名）
KIT_COLUMN_LABELS <- c(
  # 原文：題目代號 | 版本 | 名稱 | 題幹 | 題型
  item_uid = "\u984c\u76ee\u4ee3\u865f", itemVer = "\u7248\u672c", label = "\u540d\u7a31", stem = "\u984c\u5e79", itemType = "\u984c\u578b",
  # 原文：備註 | 區塊 | 停用 | 題組 | 題本
  note = "\u5099\u8a3b", block = "\u5340\u584a", deprecated = "\u505c\u7528", groupID = "\u984c\u7d44", formID = "\u984c\u672c",
  # 原文：計分方案 | 反向題 | 權重 | 別名
  scheme = "\u8a08\u5206\u65b9\u6848", reverse = "\u53cd\u5411\u984c", weight = "\u6b0a\u91cd", alias = "\u5225\u540d",
  # 原文：計分說明 | 類型 | 構念 | 上層題組
  scoring_note = "\u8a08\u5206\u8aaa\u660e", type = "\u985e\u578b", construct = "\u69cb\u5ff5", parentID = "\u4e0a\u5c64\u984c\u7d44",
  # 原文：順序 | 資料檔 | 變項名稱 | 施測
  order = "\u9806\u5e8f", datasetID = "\u8cc7\u6599\u6a94", varname = "\u8b8a\u9805\u540d\u7a31", adminID = "\u65bd\u6e2c",
  # 原文：世代 | 種類 | 問卷 | 選項組
  cohortID = "\u4e16\u4ee3", kind = "\u7a2e\u985e", instrument = "\u554f\u5377", valueSetID = "\u9078\u9805\u7d44",
  # 原文：代碼 | 原因 | 決定者 | 狀態
  code = "\u4ee3\u78bc", reason = "\u539f\u56e0", decided_by = "\u6c7a\u5b9a\u8005", status = "\u72c0\u614b",
  # 原文：用途 | 主題 | 期間 | 公開檔有沒有
  role = "\u7528\u9014", module = "\u4e3b\u984c", period = "\u671f\u9593", public = "\u516c\u958b\u6a94\u6709\u6c92\u6709"
)

# check_kit() 的檢查名稱 → 白話
KIT_CHECK_LABELS <- c(
  # 原文：少了一張表 | 表裡少了欄位
  missing_table = "\u5c11\u4e86\u4e00\u5f35\u8868", missing_column = "\u8868\u88e1\u5c11\u4e86\u6b04\u4f4d",
  # 原文：同一列出現兩次 | 必填的欄是空白
  duplicate_key = "\u540c\u4e00\u5217\u51fa\u73fe\u5169\u6b21", required_blank = "\u5fc5\u586b\u7684\u6b04\u662f\u7a7a\u767d",
  # 原文：填了不能填的值 | 格式不對（數字或日期）
  bad_value = "\u586b\u4e86\u4e0d\u80fd\u586b\u7684\u503c", bad_type = "\u683c\u5f0f\u4e0d\u5c0d\uff08\u6578\u5b57\u6216\u65e5\u671f\uff09",
  # 原文：對不到別的表 | 題組的上下層繞成一圈
  foreign_key = "\u5c0d\u4e0d\u5230\u5225\u7684\u8868", tree_cycle = "\u984c\u7d44\u7684\u4e0a\u4e0b\u5c64\u7e5e\u6210\u4e00\u5708"
)

kit_table_label <- function(x) {
  out <- unname(KIT_TABLE_LABELS[x])
  ifelse(is.na(out), x, out)
}

# 同一個欄名在某張表要叫別的名字：題目的版本的 label 是題目文字
# 原文：題目
KIT_COLUMN_LABELS_BY_TABLE <- c("item_versions.label" = "\u984c\u76ee")

kit_column_label <- function(x, table = NULL) {
  out <- unname(KIT_COLUMN_LABELS[x])
  if (length(table)) {
    by <- unname(KIT_COLUMN_LABELS_BY_TABLE[paste0(table, ".", x)])
    out <- ifelse(is.na(by), out, by)
  }
  ifelse(is.na(out) | !nzchar(x), x, out)
}

#' Next bank version
#'
#' Bank versions are `<date>-v<n>`, e.g. `"2026-11-02-v1"`: the day the new bank was made,
#' and which version of that day. A second version on the same day adds one to `n`.
#'
#' @param version The current version, e.g. `"2026-11-02-v1"`. Empty, unreadable or an
#'   older `<year>.<n>` version → `<date>-v1`.
#' @param date The day of the new version.
#' @return One string, e.g. `"2026-11-02-v2"` or `"2026-11-03-v1"`.
#' @examples
#' bump_bank_version("2026-11-02-v1", as.Date("2026-11-02"))
#' bump_bank_version("2026-11-02-v1", as.Date("2026-11-05"))
#' @export
bump_bank_version <- function(version, date = kit_today()) {
  day <- format(as.Date(date), "%Y-%m-%d")
  v <- trimws(as.character(version %||% ""))
  if (length(v) != 1L || is.na(v) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}-v[0-9]+$", v)) {
    return(paste0(day, "-v1"))
  }
  v_day <- substr(v, 1L, 10L)
  n <- as.integer(sub("^.*-v", "", v))
  # 題庫版本的日期比今天還晚（時鐘不對）時，照原本的日期往上加，不讓版本倒退
  if (day > v_day) paste0(day, "-v1") else paste0(v_day, "-v", n + 1L)
}

#' Read a change package for review
#'
#' Like [read_patch_xlsx()], and also reads the optional fields `reason`, `decided_by` and
#' `submitted_by` from the 說明 sheet, and optional per-row `reason` / `decided_by` columns on
#' the 改動 sheet. Packages made before these fields existed read fine (the fields are blank).
#'
#' @param path A `.xlsx` change package.
#' @return A list: `patch`, `base_version`, `meta` (named list of the optional fields) and
#'   `row_meta` (data frame with `reason` and `decided_by`, one row per patch row).
#' @export
read_review_package <- function(path) {
  sheets <- readxl::excel_sheets(path)
  # 原文：說明 | 改動
  if (!all(c("\u8aaa\u660e", "\u6539\u52d5") %in% sheets)) {
    # 原文：這不是修改包：Excel 裡要有「說明」和「改動」兩張工作表。
    kitDesign::kit_abort("\u9019\u4e0d\u662f\u4fee\u6539\u5305\uff1aExcel \u88e1\u8981\u6709\u300c\u8aaa\u660e\u300d\u548c\u300c\u6539\u52d5\u300d\u5169\u5f35\u5de5\u4f5c\u8868\u3002")
  }
  # 原文：改動
  raw <- as.data.frame(readxl::read_excel(path, sheet = "\u6539\u52d5", col_types = "text"))
  miss <- setdiff(PATCH_COLS, names(raw))
  if (length(miss)) {
    # 原文：修改包的「改動」少了這些欄： | 、 | 。
    kitDesign::kit_abort("\u4fee\u6539\u5305\u7684\u300c\u6539\u52d5\u300d\u5c11\u4e86\u9019\u4e9b\u6b04\uff1a", paste(miss, collapse = "\u3001"), "\u3002")
  }
  p <- read_patch_xlsx(path)
  # 原文：說明
  info <- as.data.frame(readxl::read_excel(path, sheet = "\u8aaa\u660e", col_types = "text"))
  get_info <- function(field) {
    v <- if (ncol(info) >= 2L) info[[2]][info[[1]] %in% field] else character(0)
    v <- v[!is.na(v)]
    if (length(v)) trimws(v[1L]) else ""
  }
  meta <- stats::setNames(lapply(PATCH_META_FIELDS, get_info), PATCH_META_FIELDS)
  row_meta <- data.frame(reason = rep("", nrow(p$patch)), decided_by = rep("", nrow(p$patch)),
                         stringsAsFactors = FALSE)
  for (cc in names(row_meta)) {
    if (cc %in% names(raw)) {
      v <- raw[[cc]]
      v[is.na(v)] <- ""
      row_meta[[cc]] <- v
    }
  }
  bv <- p$base_version
  bv <- if (length(bv) && !is.na(bv[1L])) trimws(bv[1L]) else ""
  list(patch = p$patch, base_version = bv, meta = meta, row_meta = row_meta)
}

#' 一列改動為什麼套不上（白話）；套得上回傳 ""
#' @noRd
review_row_problem <- function(kit, p) {
  sch <- KIT_SCHEMA[[p$table]]
  # 原文：題庫裡沒有「 | 」這張表
  if (is.null(sch)) return(paste0("\u984c\u5eab\u88e1\u6c92\u6709\u300c", p$table, "\u300d\u9019\u5f35\u8868"))
  # 原文：不認得的動作「 | 」
  if (!p$op %in% c("add", "delete", "set")) return(paste0("\u4e0d\u8a8d\u5f97\u7684\u52d5\u4f5c\u300c", p$op, "\u300d"))
  hit <- any(kit_key_str(kit[[p$table]], sch$key) == p$key)
  if (p$op == "add") {
    # 原文：要新增的列已經在題庫裡
    if (hit) return("\u8981\u65b0\u589e\u7684\u5217\u5df2\u7d93\u5728\u984c\u5eab\u88e1")
    # 多加一個 "|"：鍵的最後一欄空白（題組樹的最上層）也算對
    if (length(strsplit(paste0(p$key, "|"), "|", fixed = TRUE)[[1L]]) != length(sch$key)) {
      # 原文：這一列的代號格式不對
      return("\u9019\u4e00\u5217\u7684\u4ee3\u865f\u683c\u5f0f\u4e0d\u5c0d")
    }
    return("")
  }
  if (p$op == "set" && !p$column %in% names(sch$cols)) {
    # 原文：「 | 」沒有「 | 」這一欄
    return(paste0("\u300c", kit_table_label(p$table), "\u300d\u6c92\u6709\u300c", p$column, "\u300d\u9019\u4e00\u6b04"))
  }
  # 原文：題庫裡找不到這一列（可能已經被改過或刪掉）
  if (!hit) return("\u984c\u5eab\u88e1\u627e\u4e0d\u5230\u9019\u4e00\u5217\uff08\u53ef\u80fd\u5df2\u7d93\u88ab\u6539\u904e\u6216\u522a\u6389\uff09")
  ""
}

#' 一列的內容寫成一句：「欄：值；欄：值」（只放有值的欄，或只放 cols）
#' @noRd
review_row_text <- function(row, cols = NULL, table = NULL) {
  if (is.null(row) || !nrow(row)) return("")
  cols <- cols %||% names(row)[nzchar(as.character(unlist(row[1L, ])))]
  if (!length(cols)) return("")
  vals <- vapply(cols, function(cc) {
    v <- as.character(row[[cc]][1L])
    # 原文：（空白）
    if (nzchar(v)) v else "\uff08\u7a7a\u767d\uff09"
  }, character(1))
  # 原文：： | ；
  paste(paste0(kit_column_label(cols, table), "\uff1a", vals), collapse = "\uff1b")
}

#' Review a change package against the current bank
#'
#' Applies the patch row by row (rows that cannot be applied are skipped and listed), then
#' compares before and after, runs [check_kit()], and lists what is affected.
#'
#' @param kit The current kit.
#' @param patch A patch data frame (see [kit_patch()]).
#' @param base_version The bank version the package was made from (`""` when unknown).
#' @param current_version The bank version of `kit`.
#' @return A list:
#'   * `patch` the patch, with columns `ok` (row applied), `problem`, `now` (the bank's value
#'     before a `set` row) and `conflict` (the package's "from" differs from `now`);
#'   * `kit` the patched kit; `changes` one row per changed table row: `kind` (新增／刪除／修改),
#'     `table`, `key`, `before`, `after`;
#'   * `issues` [check_kit()] on the patched kit, with `new` (not in the current kit);
#'   * `impact` from [review_impact()]; `base_version`, `current_version`, `version_ok`;
#'   * `blocking` reasons approval is not possible (character, empty when it is).
#' @export
review_patch <- function(kit, patch, base_version = "", current_version = kit_version()) {
  patch <- as.data.frame(patch, stringsAsFactors = FALSE)
  for (cc in PATCH_COLS) {
    v <- as.character(patch[[cc]] %||% rep("", nrow(patch)))
    v[is.na(v)] <- ""
    patch[[cc]] <- v
  }
  patch <- patch[PATCH_COLS]
  n <- nrow(patch)
  ok <- logical(n)
  problem <- character(n)
  conflict <- character(n)
  now_val <- character(n)
  new_kit <- kit
  for (i in seq_len(n)) {
    p <- patch[i, , drop = FALSE]
    problem[i] <- review_row_problem(new_kit, p)
    if (nzchar(problem[i])) next
    if (p$op == "set") {
      sch <- KIT_SCHEMA[[p$table]]
      df <- new_kit[[p$table]]
      now_val[i] <- df[[p$column]][kit_key_str(df, sch$key) == p$key][1L]
      if (!identical(now_val[i], p$from)) {
        # 原文：修改包記的原本是「 | 」，題庫現在是「 | 」
        conflict[i] <- paste0("\u4fee\u6539\u5305\u8a18\u7684\u539f\u672c\u662f\u300c", p$from, "\u300d\uff0c\u984c\u5eab\u73fe\u5728\u662f\u300c", now_val[i], "\u300d")
      }
    }
    new_kit <- apply_patch(new_kit, p)
    ok[i] <- TRUE
  }
  patch$ok <- ok
  patch$problem <- problem
  patch$conflict <- conflict
  patch$now <- now_val

  changes <- review_changes(kit, new_kit, patch[ok, , drop = FALSE])
  old_issues <- check_kit(kit)
  issues <- check_kit(new_kit)
  sig <- function(x) paste(x$check, x$table, x$key, x$column, x$value, sep = "\r")
  issues$new <- !sig(issues) %in% sig(old_issues)

  base_version <- trimws(as.character(base_version %||% ""))
  if (length(base_version) != 1L || is.na(base_version)) base_version <- ""
  current_version <- as.character(current_version %||% "")
  if (length(current_version) != 1L || is.na(current_version)) current_version <- ""
  version_ok <- nzchar(base_version) && identical(base_version, current_version)

  blocking <- c(
    # 原文：修改包裡沒有改動
    if (n == 0L) "\u4fee\u6539\u5305\u88e1\u6c92\u6709\u6539\u52d5",
    # 原文：有  |  列改動套不上
    if (any(!ok)) paste0("\u6709 ", sum(!ok), " \u5217\u6539\u52d5\u5957\u4e0d\u4e0a"),
    # 原文：套用後的題庫有  |  個新的結構問題
    if (any(issues$new)) paste0("\u5957\u7528\u5f8c\u7684\u984c\u5eab\u6709 ", sum(issues$new), " \u500b\u65b0\u7684\u7d50\u69cb\u554f\u984c")
  )
  list(patch = patch, kit = new_kit, changes = changes, issues = issues,
       impact = review_impact(kit, new_kit, patch[ok, , drop = FALSE]),
       base_version = base_version, current_version = current_version,
       version_ok = version_ok, blocking = blocking)
}

#' 改前改後：每個改到的（表, 列）一列
#' @noRd
review_changes <- function(old, new, patch) {
  empty <- data.frame(kind = character(0), table = character(0), key = character(0),
                      before = character(0), after = character(0), stringsAsFactors = FALSE)
  if (!nrow(patch)) return(empty)
  tk <- unique(patch[c("table", "key")])
  rows <- lapply(seq_len(nrow(tk)), function(i) {
    tb <- tk$table[i]
    key <- tk$key[i]
    sch <- KIT_SCHEMA[[tb]]
    a <- old[[tb]][kit_key_str(old[[tb]], sch$key) == key, , drop = FALSE]
    b <- new[[tb]][kit_key_str(new[[tb]], sch$key) == key, , drop = FALSE]
    if (!nrow(a) && !nrow(b)) return(NULL)
    if (!nrow(a)) {
      # 原文：新增
      return(data.frame(kind = "\u65b0\u589e", table = tb, key = key, before = "",
                        after = review_row_text(b, table = tb), stringsAsFactors = FALSE))
    }
    if (!nrow(b)) {
      # 原文：刪除
      return(data.frame(kind = "\u522a\u9664", table = tb, key = key, before = review_row_text(a, table = tb),
                        after = "", stringsAsFactors = FALSE))
    }
    diff <- names(a)[as.character(unlist(a[1L, ])) != as.character(unlist(b[1L, ]))]
    if (!length(diff)) return(NULL)
    # 原文：修改
    data.frame(kind = "\u4fee\u6539", table = tb, key = key, before = review_row_text(a, diff, tb),
               after = review_row_text(b, diff, tb), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) return(empty)
  rownames(out) <- NULL
  out
}

#' What a change package touches
#'
#' Items, groups, data files and administrations affected by the applied rows: those changed
#' directly, plus groups that contain a changed item (via `group_members`, before or after),
#' data files that hold a changed item and the administrations of those files.
#'
#' @param old,new The kit before and after the patch.
#' @param patch The applied patch rows.
#' @return A list of character vectors: `items`, `groups`, `forms`, `datasets`, `admins`.
#' @export
review_impact <- function(old, new, patch) {
  first_part <- function(k, i = 1L) {
    vapply(strsplit(k, "|", fixed = TRUE), function(x) if (length(x) >= i) x[i] else "",
           character(1))
  }
  # 一個改到的（表, 列）在改前或改後的整列，用來找出它連到的題目、題組、資料檔
  touched_rows <- function(tb) {
    keys <- unique(patch$key[patch$table == tb])
    if (!length(keys)) return(empty_kit_table(tb))
    sch <- KIT_SCHEMA[[tb]]
    rbind(old[[tb]][kit_key_str(old[[tb]], sch$key) %in% keys, , drop = FALSE],
          new[[tb]][kit_key_str(new[[tb]], sch$key) %in% keys, , drop = FALSE])
  }
  keys_of <- function(tb) unique(patch$key[patch$table == tb])

  items <- c(first_part(keys_of("items")), first_part(keys_of("item_versions")),
             touched_rows("group_members")$item_uid, touched_rows("dataset_vars")$item_uid,
             first_part(keys_of("item_relations")), first_part(keys_of("item_relations"), 3L))
  groups <- c(keys_of("groups"), touched_rows("group_tree")$groupID,
              touched_rows("group_tree")$parentID, touched_rows("group_members")$groupID)
  forms <- c(keys_of("forms"), first_part(keys_of("form_notes")),
             touched_rows("group_tree")$formID, touched_rows("group_members")$formID)
  datasets <- c(keys_of("datasets"), touched_rows("dataset_vars")$datasetID,
                touched_rows("block_status")$datasetID)
  clean <- function(x) sort(unique(x[!is.na(x) & nzchar(x)]))
  items <- clean(items)
  gm <- rbind(old$group_members, new$group_members)
  groups <- c(groups, gm$groupID[gm$item_uid %in% items])
  dv <- rbind(old$dataset_vars, new$dataset_vars)
  datasets <- clean(c(datasets, dv$datasetID[dv$item_uid %in% items]))
  ds <- rbind(old$datasets, new$datasets)
  admins <- clean(c(keys_of("admins"), ds$adminID[ds$datasetID %in% datasets]))
  list(items = items, groups = clean(groups), forms = clean(forms), datasets = datasets,
       admins = admins)
}

#' 影響範圍的白話摘要表
#' @noRd
review_impact_table <- function(impact) {
  data.frame(
    # 原文：影響到 | 題目 | 題組 | 題本 | 資料檔 | 施測
    "\u5f71\u97ff\u5230" = c("\u984c\u76ee", "\u984c\u7d44", "\u984c\u672c", "\u8cc7\u6599\u6a94", "\u65bd\u6e2c"),
    # 原文：數量
    "\u6578\u91cf" = vapply(impact[c("items", "groups", "forms", "datasets", "admins")], length,
                  integer(1)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

#' check_kit() 的結果寫成白話表
#' @noRd
review_issue_table <- function(issues) {
  data.frame(
    # 原文：問題
    "\u554f\u984c" = ifelse(is.na(KIT_CHECK_LABELS[issues$check]), issues$check,
                  KIT_CHECK_LABELS[issues$check]),
    # 原文：表
    "\u8868" = kit_table_label(issues$table),
    # 原文：哪一列
    "\u54ea\u4e00\u5217" = review_key_text(issues$key),
    # 原文：欄
    "\u6b04" = kit_column_label(issues$column, issues$table),
    # 原文：值
    "\u503c" = issues$value,
    # 原文：什麼時候有的 | 這次改動造成的 | 原本就有
    "\u4ec0\u9ebc\u6642\u5019\u6709\u7684" = ifelse(issues$new, "\u9019\u6b21\u6539\u52d5\u9020\u6210\u7684", "\u539f\u672c\u5c31\u6709"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

#' 主鍵在畫面上：| 前後加空白，窄螢幕可以斷行
#' @noRd
review_key_text <- function(key) gsub("|", " | ", key, fixed = TRUE)

#' 改前改後寫成白話表
#' @noRd
review_change_table <- function(changes) {
  data.frame(
    # 原文：動作
    "\u52d5\u4f5c" = changes$kind,
    # 原文：表
    "\u8868" = kit_table_label(changes$table),
    # 原文：哪一列
    "\u54ea\u4e00\u5217" = review_key_text(changes$key),
    # 原文：改前
    "\u6539\u524d" = changes$before,
    # 原文：改後
    "\u6539\u5f8c" = changes$after,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

#' A change record for a reviewed package
#'
#' One row per applied patch row. Saved as
#' `inst/extdata/kit/changes/<bank_version>.csv` (see [review_files()]).
#'
#' @param review The result of [review_patch()].
#' @param meta A list with `reason`, `decided_by`, `submitted_by`, `approved_by`.
#' @param new_version The new bank version.
#' @param date The approval date.
#' @param row_meta Optional per-row `reason` / `decided_by` (from [read_review_package()]);
#'   blanks fall back to `meta`.
#' @return A data frame with columns `bank_version`, `base_version`, `applied_to`, `date`, `op`,
#'   `table`, `key`, `column`, `from`, `to`, `reason`, `decided_by`, `submitted_by`,
#'   `approved_by`. `from` is the bank's value before the change (for `set` rows).
#' @export
change_record <- function(review, meta, new_version, date = kit_today(), row_meta = NULL) {
  p <- review$patch
  keep <- p$ok %||% rep(TRUE, nrow(p))
  m <- function(f) {
    v <- as.character(meta[[f]] %||% "")
    if (length(v) != 1L || is.na(v)) "" else trimws(v)
  }
  n_all <- nrow(p)
  per_row <- function(f) {
    v <- rep(m(f), n_all)
    if (!is.null(row_meta) && f %in% names(row_meta) && nrow(row_meta) == n_all) {
      r <- as.character(row_meta[[f]])
      use <- !is.na(r) & nzchar(trimws(r))
      v[use] <- trimws(r[use])
    }
    v[keep]
  }
  # 「原本」用題庫當時真正的值；修改包記的值和題庫不同時，審核頁已經先提醒過
  from <- p$from
  if (!is.null(p$now)) from[p$op == "set"] <- p$now[p$op == "set"]
  p <- p[keep, , drop = FALSE]
  from <- from[keep]
  out <- data.frame(
    bank_version = rep(new_version, nrow(p)),
    base_version = rep(review$base_version %||% "", nrow(p)),
    applied_to = rep(review$current_version %||% "", nrow(p)),
    date = rep(format(as.Date(date)), nrow(p)),
    op = p$op, table = p$table, key = p$key, column = p$column, from = from, to = p$to,
    reason = per_row("reason"), decided_by = per_row("decided_by"),
    submitted_by = rep(m("submitted_by"), nrow(p)),
    approved_by = rep(m("approved_by"), nrow(p)),
    stringsAsFactors = FALSE
  )
  out[CHANGE_RECORD_COLS]
}

#' 改動紀錄的檔名（相對於 kit 資料夾）
#' @noRd
change_record_path <- function(new_version) {
  file.path("changes", paste0(new_version, ".csv"))
}

#' Files to put in the repository after review
#'
#' @param review The result of [review_patch()].
#' @param record The result of [change_record()].
#' @param new_version The new bank version.
#' @param prefix Where the kit lives in the repository.
#' @return A named list: path in the repository → file content (raw bytes, UTF-8). Holds the
#'   changed tables (written with [write_kit()], so rows stay sorted), `BANK_VERSION`, and the
#'   change record.
#' @export
review_files <- function(review, record, new_version, prefix = "inst/extdata/kit") {
  dir <- tempfile("kit_review_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  write_kit(review$kit, dir)
  tables <- unique(review$patch$table[review$patch$ok])
  tables <- intersect(kit_tables(), tables)
  read_raw <- function(f) readBin(f, "raw", file.size(f))
  out <- lapply(tables, function(tb) read_raw(file.path(dir, paste0(tb, ".csv"))))
  names(out) <- file.path(prefix, paste0(tables, ".csv"))
  out[[file.path(prefix, "BANK_VERSION")]] <- charToRaw(enc2utf8(paste0(new_version, "\n")))
  rec <- file.path(dir, "record.csv")
  readr::write_csv(record, rec, na = "")
  out[[file.path(prefix, change_record_path(new_version))]] <- read_raw(rec)
  out
}

#' Download the patched bank as a zip
#'
#' The files from [review_files()] under a `kit/` folder: the changed tables, `BANK_VERSION`,
#' and the change record under `kit/changes/`. Unzip it into `inst/extdata/` (overwriting),
#' commit and push.
#'
#' @param review,record,new_version As in [review_files()].
#' @param file The `.zip` to write.
#' @return `file`, invisibly.
#' @export
review_zip <- function(review, record, new_version, file) {
  root <- tempfile("kit_zip_")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  files <- review_files(review, record, new_version, prefix = "kit")
  for (path in names(files)) {
    dir.create(dirname(file.path(root, path)), recursive = TRUE, showWarnings = FALSE)
    writeBin(files[[path]], file.path(root, path))
  }
  if (file.exists(file)) unlink(file)
  zip::zipr(file, file.path(root, "kit"))
  invisible(file)
}
