# data-raw/build_bank.R — 第一次建立內建題庫（2026-11-01）
#
# 來源：0912 版的 files/家長.xlsx、兒童青少年.xlsx、教保教師.xlsx
# （舊 app 啟動時優先讀這三個檔；data/*.csv 是 2026-08-28 的舊版，不用）。
# 之後更新題庫用 update_bank("改好的題庫.xlsx", "parent")。
#
# 執行：在套件根目錄 LC_ALL=en_US.UTF-8 Rscript data-raw/build_bank.R <files 資料夾>

args <- commandArgs(trailingOnly = TRUE)
files_dir <- if (length(args)) args[[1]] else "files"
pkgload::load_all(".", helpers = FALSE, quiet = TRUE)
map <- c("家長" = "parent", "兒童青少年" = "child_adolescent", "教保教師" = "childcare_teacher")
for (f in list.files(files_dir, pattern = "\\.xlsx$", full.names = TRUE)) {
  # macOS 的檔名是 NFD：比對前先轉成 NFC
  nm <- utf8::utf8_normalize(tools::file_path_sans_ext(basename(f)))
  update_bank(f, map[[nm]], version = "2026-09-12")
}
