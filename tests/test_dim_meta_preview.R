# 構面 meta 編修 → Word 報告 整合測試
suppressPackageStartupMessages({
  library(dplyr)
})

source("R/utils.R")
source("R/data_io.R")
source("R/utils_docx.R")
source("R/utils_dim_report_docx.R")

wave <- "M108_大月齡"
dim_id <- "學習輔助"

d <- load_domain("parent")

me <- data.frame(
  dimID       = d$dimMeta$dimID,
  dimName     = as.character(d$dimMeta$dimName),
  alias       = vapply(d$dimMeta$alias, clean_meta_str, character(1L)),
  reliability = vapply(d$dimMeta$dimID, function(did) {
    v <- get_dim_reliability(did, wave, d$dimMeta, d$dimReliability)
    if (is.na(v)) "" else as.character(v)
  }, character(1L)),
  note        = vapply(d$dimMeta$note, clean_meta_str, character(1L)),
  stringsAsFactors = FALSE
)
idx <- match(dim_id, me$dimID)
me$alias[idx] <- "學習輔助AB"
me$reliability[idx] <- "0.88"

assemble_data <- function(wave, d, meta_edit_df = NULL) {
  iw <- d$itemWave
  id <- d$itemDim
  ib <- d$itemBank
  dm <- ensure_dim_meta_cols(d$dimMeta)
  dr <- d$dimReliability
  wave_items <- iw[iw[[wave]] == 1L, c("itemID", "itemVer"), drop = FALSE]
  dim_cols <- get_matrix_cols(id)
  result <- lapply(dim_cols, function(dim_col) {
    dim_items <- id[id[[dim_col]] == 1L, c("itemID", "itemVer"), drop = FALSE]
    items_in_both <- inner_join(wave_items, dim_items, by = c("itemID", "itemVer"))
    if (nrow(items_in_both) == 0L) return(NULL)
    item_details <- inner_join(items_in_both, ib, by = c("itemID", "itemVer"))
    item_details$label <- collapse_newlines(item_details$label)
    items_out <- pick_unique_report_items(item_details, wave_items)
    resolved <- resolve_dim_meta(dim_col, wave, dm, dr, meta_edit_df)
    list(
      dimID = dim_col, dimName = resolved$dimName,
      dimNameRaw = resolved$dimNameRaw, alias = resolved$alias,
      reliability = resolved$reliability, items = items_out
    )
  })
  Filter(Negate(is.null), result)
}

rd <- assemble_data(wave, d, me)
hit <- Filter(function(x) x$dimID == dim_id, rd)[[1]]

stopifnot(identical(format_dim_report_heading(hit$dimNameRaw, hit$alias),
                    "學習輔助（學習輔助AB）"))
stopifnot(all(c("itemID", "label") == names(hit$items)))
stopifnot(!any(c("itemVer", "block") %in% names(hit$items)))
stopifnot(abs(hit$reliability - 0.88) < 1e-6)

out <- tempfile(fileext = ".docx")
generate_dim_report_docx(list(domain = "parent", wave = wave, dims = rd), out)
stopifnot(file.exists(out))
stopifnot(file.info(out)$size > 5000L)

cat("PASS: dim report Word export\n")
