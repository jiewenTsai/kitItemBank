# 將新版四表 Excel 匯入 data/<domain>/ CSV
source("R/utils.R")
source("R/data_io.R")

imports <- list(
  parent            = "C:/Users/User/Downloads/files/家長.xlsx",
  child_adolescent  = "C:/Users/User/Downloads/files/兒童青少年.xlsx",
  childcare_teacher = "C:/Users/User/Downloads/files/教保教師.xlsx"
)

for (domain in names(imports)) {
  path <- imports[[domain]]
  if (!file.exists(path)) stop("找不到：", path)
  cat("Importing", domain, "from", basename(path), "...\n")
  bundle <- import_domain_xlsx(path)
  save_domain_bundle(domain, bundle, data_root = "data")
  cat("  itemBank:", nrow(bundle$itemBank), "rows\n")
  cat("  itemDim cols:", length(get_matrix_cols(bundle$itemDim)), "\n")
  cat("  itemWave cols:", paste(get_wave_cols(bundle$itemWave), collapse = ", "), "\n")
  cat("  dimMeta:", nrow(bundle$dimMeta), "rows\n\n")
}

cat("Done.\n")
