# data-raw/build_kit.R — 從各來源組出 0.3 題庫（kit），寫到 inst/extdata/kit/
#
# 用法（在套件根目錄）：
#   LC_ALL=en_US.UTF-8 Rscript --no-init-file data-raw/build_kit.R <sav_meta 資料夾>
# sav_meta 資料夾放各次施測 .sav 的表頭（<wave>/<data1|data2|data3|IRT>_meta.csv），
# 只有變項名稱和標籤，沒有個人資料；不進 repo。
#
# 步驟：1.1 import_v02() → 1.2 import_design() → 1.3 import_sav_dir() → 組合 → 檢查 → 寫檔

pkgload::load_all(quiet = TRUE)

args <- commandArgs(TRUE)
sav_dir <- if (length(args)) args[[1]] else stop("need the sav_meta folder")
out_dir <- "inst/extdata/kit"

v02 <- import_v02()
des <- import_design("data-raw/kit_manual")

# 1.3：已釋出資料檔的 .sav 表頭 ----
rel <- des$datasets[des$datasets$file != "v02", ]
adm <- des$admins[match(rel$adminID, des$admins$adminID), ]
wm <- des$wave_map
wave_cols <- vapply(rel$datasetID, function(id) {
  paste(wm$wave_col[wm$datasetID == id], collapse = "|")
}, character(1))
sav_map <- data.frame(
  wave_dir = paste0(tolower(substr(rel$adminID, 1, 3)), "w",
                    sprintf("%02d", as.integer(adm$ageMonth))),
  file = rel$file, datasetID = rel$datasetID, instrument = rel$instrument,
  adminID = rel$adminID, respondent = rel$respondent, wave_col = unname(wave_cols),
  stringsAsFactors = FALSE)
sav <- import_sav_dir(sav_dir, sav_map, items = v02$items)

# dataset_vars ----
# 已釋出：用 .sav 表頭；itemVer 用 0.2 對應波次欄的版本補上
iw <- v02$item_waves
iw$datasetID <- wm$datasetID[match(paste(iw$domain, iw$wave_col),
                                   paste(wm$domain, wm$wave_col))]
iw <- iw[!is.na(iw$datasetID), ]
# 同一題同一資料檔有兩版時取較新的
iw <- iw[order(iw$datasetID, iw$item_uid, -as.integer(iw$itemVer)), ]
iw <- iw[!duplicated(paste(iw$datasetID, iw$item_uid)), ]

dv_rel <- sav$dataset_vars
ver <- iw$itemVer[match(paste(dv_rel$datasetID, dv_rel$item_uid),
                        paste(iw$datasetID, iw$item_uid))]
# 親友檔沒有自己的波次欄：用同一施測家長檔的版本
need <- is.na(ver) & nzchar(dv_rel$item_uid)
p_id <- sub("-PK$", "-P", dv_rel$datasetID[need])
ver[need] <- iw$itemVer[match(paste(p_id, dv_rel$item_uid[need]),
                              paste(iw$datasetID, iw$item_uid))]
# 還是沒有：用該題最新版
need <- is.na(ver) & nzchar(dv_rel$item_uid)
latest <- tapply(as.integer(v02$item_versions$itemVer), v02$item_versions$item_uid, max)
ver[need] <- as.character(latest[dv_rel$item_uid[need]])
dv_rel$itemVer <- ifelse(is.na(ver), "", ver)

# 內部（status internal）：沒有 .sav，用 0.2 的波次欄
int_ids <- des$datasets$datasetID[des$datasets$file == "v02"]
iw_int <- iw[iw$datasetID %in% int_ids, ]
dv_int <- conform_kit_table(data.frame(
  datasetID = iw_int$datasetID, varname = sub("^[A-Z]\\.", "", iw_int$item_uid),
  item_uid = iw_int$item_uid, itemVer = iw_int$itemVer,
  kind = ifelse(v02$items$role[match(iw_int$item_uid, v02$items$item_uid)] %in% "admin",
                "system", "item"),
  stringsAsFactors = FALSE), "dataset_vars")
dataset_vars <- rbind(dv_rel, dv_int)

# 題組：成員和樹都掛在題本底下 ----
ds_form <- des$datasets[, c("datasetID", "formID")]
uf <- unique(data.frame(item_uid = dataset_vars$item_uid,
                        formID = ds_form$formID[match(dataset_vars$datasetID, ds_form$datasetID)],
                        stringsAsFactors = FALSE))
uf <- uf[nzchar(uf$item_uid) & !is.na(uf$formID) & nzchar(uf$formID), ]
gm <- merge(v02$group_members_v02, uf, by = "item_uid")
group_members <- conform_kit_table(data.frame(
  formID = gm$formID, groupID = gm$groupID, item_uid = gm$item_uid, scheme = "official",
  reverse = gm$reverse, stringsAsFactors = FALSE), "group_members")
group_members <- group_members[!duplicated(kit_key_str(group_members, KIT_SCHEMA$group_members$key)), ]

edges <- v02$group_edges
tree <- list()
for (f in unique(group_members$formID)) {
  g <- unique(group_members$groupID[group_members$formID == f])
  repeat {  # 往上補祖先
    up <- setdiff(edges$parentID[edges$groupID %in% g], g)
    if (!length(up)) break
    g <- c(g, up)
  }
  e <- edges[edges$groupID %in% g, ]
  roots <- setdiff(g, e$groupID)
  tree[[f]] <- data.frame(formID = f, groupID = c(e$groupID, roots),
                          parentID = c(e$parentID, rep("", length(roots))),
                          stringsAsFactors = FALSE)
}
group_tree <- conform_kit_table(do.call(rbind, tree), "group_tree")

# 組合 ----
kit <- empty_kit()
for (tb in c("cohorts", "admins", "datasets", "forms", "form_notes", "missing_schemes",
             "sources", "aliases")) kit[[tb]] <- conform_kit_table(des[[tb]], tb)
for (tb in c("items", "item_versions", "value_codes", "groups")) {
  kit[[tb]] <- conform_kit_table(v02[[tb]], tb)
}
kit$dataset_vars <- dataset_vars
kit$group_members <- group_members
kit$group_tree <- group_tree

res <- check_kit(kit)
cat("\n== check_kit:", nrow(res), "issues\n")
if (nrow(res)) print(table(res$table, res$check))

write_kit(kit, out_dir)
cat("\n== rows\n")
print(vapply(kit, nrow, integer(1)))
cat("\n== sav coverage\n")
print(sav$summary[, c("datasetID", "n_vars", "matched", "pct_matched", "items",
                      "items_matched", "system", "weight", "derived")], row.names = FALSE)
saveRDS(list(kit = kit, v02 = v02, des = des, sav = sav, check = res),
        file.path(sav_dir, "..", "kit_build.rds"))
