source("R/utils.R")
source("R/utils_docx.R")
ib <- readr::read_csv(
  "data/parent/itemBank.csv",
  show_col_types = FALSE,
  locale = readr::locale(encoding = "UTF-8")
)
keys <- lapply(c("socB19", "socC14", "socC19"), function(id) {
  list(itemID = id, itemVer = 1L, layout = "auto", matrix_group = NULL)
})
rows <- do.call(rbind, lapply(keys, function(k) {
  ib[ib$itemID == k$itemID & ib$itemVer == k$itemVer, , drop = FALSE]
}))
groups <- group_items_for_render(rows, keys)
block_labels <- c("\u5f9e\u4e0d\u5982\u6b64", "\u5f88\u5c11\u5982\u6b64",
                  "\u6709\u6642\u5982\u6b64", "\u7d93\u5e38\u5982\u6b64",
                  "\u7e3d\u662f\u5982\u6b64")
block_codes <- c("1", "2", "3", "4", "5")
batched <- batch_consecutive_likert_groups(
  groups, block_labels = block_labels, block_codes = block_codes
)
cat("soc groups:", length(groups),
    " batched:", length(batched),
    " rows:", nrow(batched[[1]]$items), "\n")
cat("matrix?", should_use_matrix_layout(batched[[1]]), "\n")

for (id in c("socB19", "socC14", "socC19")) {
  row <- ib[ib$itemID == id & ib$itemVer == 1L, , drop = FALSE][1L, , drop = FALSE]
  cat(id, "key=", likert_batch_key(row, block_labels, block_codes), "\n")
}

fam_keys <- lapply(c("famC07", "famC08", "famC09"), function(id) {
  list(itemID = id, itemVer = 1L, layout = "auto", matrix_group = NULL)
})
fam_rows <- do.call(rbind, lapply(fam_keys, function(k) {
  ib[ib$itemID == k$itemID & ib$itemVer == k$itemVer, , drop = FALSE]
}))
fam_groups <- group_items_for_render(fam_rows, fam_keys)
fam_labels <- c("\u5f88\u5c11", "\u6709\u6642", "\u7d93\u5e38", "\u5f88\u7d93\u5e38", "\u62d2\u7b54")
fam_codes <- c("1", "2", "3", "4", "5")
fam_batched <- batch_consecutive_likert_groups(
  fam_groups, block_labels = fam_labels, block_codes = fam_codes
)
cat("fam batched:", length(fam_batched), " rows:", nrow(fam_batched[[1]]$items),
    " matrix?", should_use_matrix_layout(fam_batched[[1]]), "\n")
