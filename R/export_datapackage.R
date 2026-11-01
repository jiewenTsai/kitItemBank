# R/export_datapackage.R — 從 KIT_SCHEMA 產生 datapackage.json（Frictionless Data）
#
# 每張表一個 resource（<表>.csv），欄位的型別、必填、可填值、主鍵、外鍵都照 KIT_SCHEMA。
# 日期欄可能只有年或年月（2016、2016-06、2016-06-01），所以寫成 string 加 pattern。
# 空字串當成缺值（missingValues = [""]），和 read_kit() 一樣。

# kcol 的型別 → Table Schema 的型別
DP_TYPES <- c(chr = "string", int = "integer", num = "number", date = "string")
DP_DATE_PATTERN <- "[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?"

#' 一張表的 Table Schema
#' @noRd
dp_table_schema <- function(tb) {
  sch <- KIT_SCHEMA[[tb]]
  fields <- lapply(names(sch$cols), function(cc) {
    spec <- sch$cols[[cc]]
    f <- list(name = cc, type = unname(DP_TYPES[spec$type]))
    con <- list()
    if (isTRUE(spec$req)) con$required <- TRUE
    if (!is.null(spec$enum)) con$enum <- I(KIT_ENUMS[[spec$enum]])
    if (identical(spec$type, "date")) con$pattern <- DP_DATE_PATTERN
    if (length(con)) f$constraints <- con
    f
  })
  out <- list(fields = fields, missingValues = I(""), primaryKey = I(sch$key))
  if (length(sch$fk)) {
    out$foreignKeys <- lapply(sch$fk, function(fk) {
      ref_key <- KIT_SCHEMA[[fk$table]]$key
      # 和 check_kit() 一樣：單欄對單欄主鍵；多欄照順序對
      ref_cols <- if (length(fk$cols) == 1L) ref_key[1L] else ref_key[seq_along(fk$cols)]
      list(fields = I(fk$cols),
           reference = list(resource = if (identical(fk$table, tb)) "" else fk$table,
                            fields = I(ref_cols)))
    })
  }
  out
}

#' The kit's data package descriptor
#'
#' A [Frictionless Data](https://specs.frictionlessdata.io/) tabular data package built
#' from the schema: one resource per table (`<table>.csv`) with field types, required
#' fields, allowed values, primary keys and foreign keys.
#'
#' @param version Bank version, e.g. `"2026-11-02-v1"`.
#' @return A list ready for `jsonlite::toJSON(auto_unbox = TRUE)`.
#' @export
datapackage_descriptor <- function(version = codebook_bank_version()) {
  resources <- lapply(kit_tables(), function(tb) {
    list(name = tb, path = paste0(tb, ".csv"), profile = "tabular-data-resource",
         format = "csv", mediatype = "text/csv", encoding = "utf-8",
         schema = dp_table_schema(tb))
  })
  list(profile = "tabular-data-package", name = "kit-item-bank",
       title = "Kids in Taiwan (KIT) item bank", version = version,
       created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
       resources = resources)
}

#' Write `datapackage.json` for a kit folder
#'
#' @param dir The kit folder (holding the `<table>.csv` files).
#' @param version Bank version; by default the folder's `BANK_VERSION`.
#' @return The path of the written file, invisibly.
#' @examples
#' d <- tempfile()
#' write_kit(empty_kit(), d)
#' write_datapackage(d)
#' @export
write_datapackage <- function(dir = kit_dir(), version = codebook_bank_version(dir)) {
  path <- file.path(dir, "datapackage.json")
  writeLines(enc2utf8(datapackage_json(version)), path, useBytes = TRUE)
  invisible(path)
}

#' The descriptor as a JSON string
#' @noRd
datapackage_json <- function(version = codebook_bank_version()) {
  as.character(jsonlite::toJSON(datapackage_descriptor(version), auto_unbox = TRUE,
                                pretty = TRUE))
}
