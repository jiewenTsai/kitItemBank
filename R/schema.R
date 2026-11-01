# R/schema.R — 0.3 題庫（kit）的結構定義：17 張表的欄位、主鍵、外鍵、可填的值
#
# 這裡是唯一的來源：check_kit()、read_kit()／write_kit()、之後的 Excel 範本和
# datapackage.json 都從 KIT_SCHEMA 產生。所有欄位都以文字存，型別只用來檢查。
# 設計見 kitItemBank_重構設計_v2.md 第 2、3 節。

# 共用的可填值
KIT_ENUMS <- list(
  instrument  = c("P", "T", "C", "S"),
  status      = c("released", "internal", "planned"),
  var_kind    = c("item", "system", "weight", "derived"),
  asked       = c("all", "subset", "carried", "none"),
  role        = c("measure", "background", "admin"),
  module      = c("development", "family", "health", "childcare", "school",
                  "demographic", "system"),
  source_type = c("questionnaire", "individual_test", "observation", "administrative"),
  rel_kind    = c("revised", "renamed", "split", "merged", "borrowed"),
  similarity  = c("identical", "high", "medium", "low", "none"),
  comparable  = c("yes", "partial", "no"),
  alias_type  = c("item", "admin", "form", "group"),
  group_type  = c("domain", "scale", "subscale", "composite", "testlet"),
  miss_kind   = c("refused", "dont_know", "not_applicable", "skipped", "missing"),
  source_kind = c("crosswalk", "usage_doc", "sav_header", "pdf", "meeting", "v02_bank"),
  flag01      = c("0", "1")
)

# 一欄的定義：type 是 "chr" "int" "num" "date"；enum 指向 KIT_ENUMS；req = 不可空白
kcol <- function(type = "chr", enum = NULL, req = FALSE) {
  list(type = type, enum = enum, req = req)
}

#' 17 張表的定義
#'
#' 每張表：`cols`（欄名 → kcol）、`key`（主鍵欄）、`fk`（外鍵：本表欄 → "表.欄"）。
#' 多欄外鍵寫成 `c("item_uid", "itemVer")` → `"item_versions"`。
#' @noRd
KIT_SCHEMA <- list(
  # ① 研究設計 ----
  cohorts = list(
    cols = list(cohortID = kcol(req = TRUE), code = kcol(req = TRUE),
                birth_from = kcol("date"), birth_to = kcol("date"), note = kcol()),
    key = "cohortID"),
  admins = list(
    cols = list(adminID = kcol(req = TRUE), cohortID = kcol(req = TRUE),
                ageMonth = kcol("int", req = TRUE), grade = kcol(), waveNo = kcol("int"),
                surveyFrom = kcol("date"), surveyTo = kcol("date"),
                status = kcol(enum = "status", req = TRUE), datasetRelease = kcol(),
                citation_zh = kcol(), citation_en = kcol(), note = kcol()),
    key = "adminID",
    fk = list(list(cols = "cohortID", table = "cohorts"))),
  datasets = list(
    cols = list(datasetID = kcol(req = TRUE), adminID = kcol(req = TRUE),
                file = kcol(req = TRUE), instrument = kcol(enum = "instrument", req = TRUE),
                formID = kcol(), respondent = kcol(), missingScheme = kcol(), note = kcol()),
    key = "datasetID",
    fk = list(list(cols = "adminID", table = "admins"),
              list(cols = "formID", table = "forms"))),
  dataset_vars = list(
    cols = list(datasetID = kcol(req = TRUE), varname = kcol(req = TRUE),
                item_uid = kcol(), itemVer = kcol(),
                kind = kcol(enum = "var_kind", req = TRUE),
                position = kcol(), order = kcol("int")),
    key = c("datasetID", "varname"),
    fk = list(list(cols = "datasetID", table = "datasets"),
              list(cols = "item_uid", table = "items"),
              list(cols = c("item_uid", "itemVer"), table = "item_versions"))),
  block_status = list(
    cols = list(datasetID = kcol(req = TRUE), block = kcol(req = TRUE),
                asked = kcol(enum = "asked", req = TRUE), who = kcol(), who_var = kcol(),
                reason = kcol(), decided_by = kcol(), sourceID = kcol()),
    key = c("datasetID", "block"),
    fk = list(list(cols = "datasetID", table = "datasets"),
              list(cols = "sourceID", table = "sources"))),
  # ② 工具 ----
  forms = list(
    cols = list(formID = kcol(req = TRUE), instrument = kcol(enum = "instrument", req = TRUE),
                label = kcol(req = TRUE), stage = kcol(),
                ageFrom = kcol("int"), ageTo = kcol("int")),
    key = "formID"),
  form_notes = list(
    cols = list(formID = kcol(req = TRUE), year = kcol(req = TRUE), summary = kcol(),
                reason = kcol(), position_source = kcol(), sourceID = kcol()),
    key = c("formID", "year"),
    fk = list(list(cols = "formID", table = "forms"),
              list(cols = "sourceID", table = "sources"))),
  # ③ 題目 ----
  items = list(
    cols = list(item_uid = kcol(req = TRUE), instrument = kcol(enum = "instrument", req = TRUE),
                varname = kcol(req = TRUE), block = kcol(),
                role = kcol(enum = "role"), also_background = kcol(enum = "flag01"),
                module = kcol(enum = "module"), source_type = kcol(enum = "source_type"),
                deprecated = kcol(enum = "flag01"), text_public = kcol(enum = "flag01"),
                note = kcol()),
    key = "item_uid"),
  item_versions = list(
    cols = list(item_uid = kcol(req = TRUE), itemVer = kcol("int", req = TRUE),
                label = kcol(), stem = kcol(), valueSetID = kcol(), itemType = kcol(),
                period = kcol(), note = kcol()),
    key = c("item_uid", "itemVer"),
    fk = list(list(cols = "item_uid", table = "items"))),
  item_relations = list(
    cols = list(from_uid = kcol(req = TRUE), from_ver = kcol("int", req = TRUE),
                to_uid = kcol(req = TRUE), to_ver = kcol("int", req = TRUE),
                kind = kcol(enum = "rel_kind", req = TRUE),
                similarity = kcol(enum = "similarity"), comparable = kcol(enum = "comparable"),
                meaning_changed = kcol(enum = "flag01"), effective_admin = kcol(),
                reason = kcol(), decided_by = kcol(), decided_on = kcol("date"),
                sourceID = kcol()),
    key = c("from_uid", "from_ver", "to_uid", "to_ver"),
    fk = list(list(cols = c("from_uid", "from_ver"), table = "item_versions"),
              list(cols = c("to_uid", "to_ver"), table = "item_versions"),
              list(cols = "effective_admin", table = "admins"),
              list(cols = "sourceID", table = "sources"))),
  aliases = list(
    cols = list(alias = kcol(req = TRUE), target_type = kcol(enum = "alias_type", req = TRUE),
                target_id = kcol(req = TRUE), kind = kcol()),
    key = c("alias", "target_type", "target_id")),
  # ④ 題組 ----
  groups = list(
    cols = list(groupID = kcol(req = TRUE), label = kcol(req = TRUE),
                type = kcol(enum = "group_type"), construct = kcol(),
                instrument_source = kcol(), scoring_note = kcol(), alias = kcol()),
    key = "groupID"),
  group_tree = list(
    cols = list(formID = kcol(req = TRUE), groupID = kcol(req = TRUE), parentID = kcol(),
                order = kcol("int")),
    key = c("formID", "groupID", "parentID"),
    fk = list(list(cols = "formID", table = "forms"),
              list(cols = "groupID", table = "groups"),
              list(cols = "parentID", table = "groups"))),
  group_members = list(
    cols = list(formID = kcol(req = TRUE), groupID = kcol(req = TRUE),
                item_uid = kcol(req = TRUE), scheme = kcol(req = TRUE),
                reverse = kcol(enum = "flag01"), weight = kcol("num")),
    key = c("formID", "groupID", "item_uid", "scheme"),
    fk = list(list(cols = "formID", table = "forms"),
              list(cols = "groupID", table = "groups"),
              list(cols = "item_uid", table = "items"))),
  # ⑤ 編碼 ----
  value_codes = list(
    cols = list(valueSetID = kcol(req = TRUE), code = kcol(req = TRUE), label = kcol(),
                order = kcol("int")),
    key = c("valueSetID", "code")),
  missing_schemes = list(
    cols = list(scheme = kcol(req = TRUE), code = kcol(req = TRUE), label = kcol(),
                kind = kcol(enum = "miss_kind")),
    key = c("scheme", "code")),
  # 其他 ----
  sources = list(
    cols = list(sourceID = kcol(req = TRUE), kind = kcol(enum = "source_kind", req = TRUE),
                title = kcol(), version = kcol(), date = kcol("date"), location = kcol()),
    key = "sourceID")
)

#' 表名，照存檔和顯示的順序
#' @noRd
kit_tables <- function() names(KIT_SCHEMA)

#' 一張空的表（全部文字欄）
#' @noRd
empty_kit_table <- function(table) {
  cols <- names(KIT_SCHEMA[[table]]$cols)
  df <- as.data.frame(stats::setNames(rep(list(character(0)), length(cols)), cols),
                      stringsAsFactors = FALSE)
  df
}

#' An empty kit (all 17 tables with 0 rows)
#'
#' @return A named list of data frames, one per table in the 0.3 schema.
#' @examples
#' kit <- empty_kit()
#' names(kit)
#' @export
empty_kit <- function() {
  stats::setNames(lapply(kit_tables(), empty_kit_table), kit_tables())
}

#' 把一張表整理成 schema 的欄位：補缺欄、去多餘欄、全部轉文字、NA 變空字串
#' @noRd
conform_kit_table <- function(df, table) {
  cols <- names(KIT_SCHEMA[[table]]$cols)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (cc in setdiff(cols, names(df))) df[[cc]] <- rep("", nrow(df))
  df <- df[, cols, drop = FALSE]
  for (cc in cols) {
    v <- as.character(df[[cc]])
    v[is.na(v)] <- ""
    df[[cc]] <- v
  }
  rownames(df) <- NULL
  df
}

#' 預設的 kit 資料夾
#' @noRd
kit_dir <- function() {
  system.file("extdata", "kit", package = "kitItemBank")
}

#' Read a kit (one CSV per table)
#'
#' Missing tables come back empty, so a partially built kit can still be checked.
#'
#' @param dir Folder holding `<table>.csv` files.
#' @return A named list of data frames (all columns character).
#' @export
read_kit <- function(dir = kit_dir()) {
  kit <- lapply(kit_tables(), function(tb) {
    f <- file.path(dir, paste0(tb, ".csv"))
    if (!nzchar(dir) || !file.exists(f)) return(empty_kit_table(tb))
    df <- as.data.frame(readr::read_csv(
      f, locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE,
      na = character(0), col_types = readr::cols(.default = readr::col_character())
    ))
    conform_kit_table(df, tb)
  })
  stats::setNames(kit, kit_tables())
}

#' Write a kit (one CSV per table)
#'
#' Rows are sorted by primary key so diffs between bank versions stay small.
#'
#' @param kit A kit list.
#' @param dir Target folder (created if needed).
#' @return `dir`, invisibly.
#' @export
write_kit <- function(kit, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (tb in kit_tables()) {
    df <- conform_kit_table(kit[[tb]] %||% empty_kit_table(tb), tb)
    key <- KIT_SCHEMA[[tb]]$key
    if (nrow(df) > 0L) df <- df[do.call(order, unname(as.list(df[key]))), , drop = FALSE]
    readr::write_csv(df, file.path(dir, paste0(tb, ".csv")), na = "")
  }
  invisible(dir)
}

# 檢查 ----

#' 一列檢查結果
#' @noRd
kit_issue <- function(level, check, table, key = character(0), column = "", value = "",
                      message = "") {
  n <- max(length(key), length(value), 1L)
  if (length(key) == 0L) key <- ""
  data.frame(level = rep(level, n), check = rep(check, n), table = rep(table, n),
             key = rep_len(key, n), column = rep(column, n), value = rep_len(value, n),
             message = rep(message, n), stringsAsFactors = FALSE)
}

empty_kit_issues <- function() {
  kit_issue("block", "", "")[0, , drop = FALSE]
}

#' 主鍵字串（多欄用 | 接起來）
#' @noRd
kit_key_str <- function(df, cols) {
  if (length(cols) == 1L) return(df[[cols]])
  do.call(paste, c(unname(as.list(df[cols])), sep = "|"))
}

#' Check a kit against the schema
#'
#' Structural checks only: required columns, primary keys, foreign keys, allowed values,
#' types and cycles in group trees. All findings here are blocking (`level = "block"`);
#' content checks that need a human (`level = "note"`) live in `audit_kit()`.
#'
#' @param kit A kit list, e.g. from [read_kit()].
#' @return A data frame with one row per problem: `level`, `check`, `table`, `key`,
#'   `column`, `value`, `message`. Zero rows means the kit passes.
#' @examples
#' check_kit(empty_kit())
#' @export
check_kit <- function(kit) {
  out <- list()
  add <- function(x) out[[length(out) + 1L]] <<- x

  missing_tb <- setdiff(kit_tables(), names(kit))
  if (length(missing_tb)) {
    add(kit_issue("block", "missing_table", missing_tb, message = "table missing"))
  }

  for (tb in intersect(kit_tables(), names(kit))) {
    sch <- KIT_SCHEMA[[tb]]
    df <- kit[[tb]]
    miss_cols <- setdiff(names(sch$cols), names(df))
    if (length(miss_cols)) {
      add(kit_issue("block", "missing_column", tb, column = paste(miss_cols, collapse = ", "),
                    message = "column missing"))
      next
    }
    if (nrow(df) == 0L) next
    df[] <- lapply(df, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })
    keys <- kit_key_str(df, sch$key)

    dup <- unique(keys[duplicated(keys)])
    if (length(dup)) add(kit_issue("block", "duplicate_key", tb, key = dup,
                                   column = paste(sch$key, collapse = "|"),
                                   message = "primary key repeated"))

    for (cc in names(sch$cols)) {
      spec <- sch$cols[[cc]]
      v <- df[[cc]]
      blank <- !nzchar(v)
      if (spec$req && any(blank)) {
        add(kit_issue("block", "required_blank", tb, key = keys[blank], column = cc,
                      message = "required value is blank"))
      }
      if (!is.null(spec$enum)) {
        bad <- !blank & !(v %in% KIT_ENUMS[[spec$enum]])
        if (any(bad)) add(kit_issue("block", "bad_value", tb, key = keys[bad], column = cc,
                                    value = v[bad], message = "value not allowed"))
      }
      pat <- switch(spec$type,
                    int = "^-?[0-9]+$",
                    num = "^-?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$",
                    date = "^[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?$",
                    NULL)
      if (!is.null(pat)) {
        bad <- !blank & !grepl(pat, v)
        if (any(bad)) add(kit_issue("block", "bad_type", tb, key = keys[bad], column = cc,
                                    value = v[bad], message = paste("not", spec$type)))
      }
    }

    for (fk in sch$fk %||% list()) {
      ref <- kit[[fk$table]]
      if (is.null(ref)) next
      ref_key <- KIT_SCHEMA[[fk$table]]$key
      # 外鍵欄對到參照表的主鍵：單欄對單欄主鍵；多欄照順序對
      ref_cols <- if (length(fk$cols) == 1L) ref_key[1L] else ref_key[seq_along(fk$cols)]
      here <- kit_key_str(df, fk$cols)
      blank <- Reduce(`|`, lapply(fk$cols, function(cc) !nzchar(df[[cc]])))
      there <- if (nrow(ref)) kit_key_str(ref, ref_cols) else character(0)
      bad <- !blank & !(here %in% there)
      if (any(bad)) {
        add(kit_issue("block", "foreign_key", tb, key = keys[bad],
                      column = paste(fk$cols, collapse = "|"), value = here[bad],
                      message = paste0("not found in ", fk$table)))
      }
    }
  }

  gt <- kit$group_tree
  if (!is.null(gt) && nrow(gt)) {
    cyc <- group_tree_cycles(gt)
    if (length(cyc)) add(kit_issue("block", "tree_cycle", "group_tree", key = cyc,
                                   message = "group tree has a cycle"))
  }

  if (!length(out)) return(empty_kit_issues())
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

#' 找出題組樹的循環：回傳 "formID|groupID"（在循環上的節點）
#' @noRd
group_tree_cycles <- function(gt) {
  gt <- gt[nzchar(gt$parentID), , drop = FALSE]
  out <- character(0)
  for (f in unique(gt$formID)) {
    e <- gt[gt$formID == f, , drop = FALSE]
    # 反覆拿掉沒有子節點或沒有上層的節點；剩下的就在循環上
    nodes <- unique(c(e$groupID, e$parentID))
    repeat {
      ends <- union(setdiff(nodes, e$parentID), setdiff(nodes, e$groupID))
      if (!length(ends)) break
      e <- e[!(e$groupID %in% ends) & !(e$parentID %in% ends), , drop = FALSE]
      nodes <- setdiff(nodes, ends)
    }
    if (length(nodes)) out <- c(out, paste(f, nodes, sep = "|"))
  }
  out
}
