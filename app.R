# 雲端部署（Posit Connect Cloud）用：載入這個資料夾的套件，開 app。
# 本機請用 kitItemBank::run_kititembank()。
pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
kit_app()
