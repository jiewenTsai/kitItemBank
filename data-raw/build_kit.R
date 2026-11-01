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

# 已決定事項（data-raw/kit_decisions/）----
dec_dir <- "data-raw/kit_decisions"
rd <- function(f) utils::read.csv(file.path(dec_dir, f), encoding = "UTF-8",
                                  stringsAsFactors = FALSE, colClasses = "character")

# D4 段落名稱不是題組：拿掉；題目的 block 空白時用段落名稱補上
drop <- rd("drop_groups.csv")$groupID
gm <- kit$group_members[kit$group_members$groupID %in% drop, ]
sec <- tapply(kit$groups$label[match(gm$groupID, kit$groups$groupID)], gm$item_uid,
              function(x) if (length(unique(x)) == 1L) x[1] else NA_character_)
fill <- !nzchar(kit$items$block) & kit$items$item_uid %in% names(sec)
kit$items$block[fill] <- unname(sec[kit$items$item_uid[fill]])
kit$items$block[is.na(kit$items$block)] <- ""
kit$groups <- kit$groups[!kit$groups$groupID %in% drop, ]
kit$group_members <- kit$group_members[!kit$group_members$groupID %in% drop, ]
kit$group_tree <- kit$group_tree[!kit$group_tree$groupID %in% drop, ]
kit$group_tree$parentID[kit$group_tree$parentID %in% drop] <- ""

# D1 合併題組：成員、樹改指向新題組，舊名稱留作別名
mg <- rd("merge_groups.csv")
for (i in seq_len(nrow(mg))) {
  from <- mg$from[i]; to <- mg$to[i]
  kit$aliases <- rbind(kit$aliases, conform_kit_table(data.frame(
    alias = kit$groups$label[kit$groups$groupID == from], target_type = "group",
    target_id = to, kind = "merged"), "aliases"))
  kit$group_members$groupID[kit$group_members$groupID == from] <- to
  kit$group_tree$groupID[kit$group_tree$groupID == from] <- to
  kit$group_tree$parentID[kit$group_tree$parentID == from] <- to
  kit$groups <- kit$groups[kit$groups$groupID != from, ]
}
kit$group_members <- kit$group_members[!duplicated(kit_key_str(kit$group_members,
                                                               KIT_SCHEMA$group_members$key)), ]
gt <- kit$group_tree[kit$group_tree$groupID != kit$group_tree$parentID, ]
gt <- gt[!duplicated(kit_key_str(gt, KIT_SCHEMA$group_tree$key)), ]
# 拿掉上層之後變成根的題組，如果還有別的上層就不要再留一列空白上層
has_parent <- paste(gt$formID, gt$groupID)[nzchar(gt$parentID)]
kit$group_tree <- gt[nzchar(gt$parentID) | !paste(gt$formID, gt$groupID) %in% has_parent, ]

# 1.5–1.7：候選。D5（主持人 2026-11-02）：高、中、低信心全部放進 kit；
# 已決定事項（D1 合併、D4 拿掉段落題組）優先。全部候選仍寫到 data-raw/candidates/ 留底 ----
accept <- c("high", "medium", "low")
accepted_by <- "PI"
accepted_on <- "2026-11-02"
auto_to_pi <- function(df) {
  a <- df$decided_by %in% c("", "auto")
  df$decided_by[a] <- accepted_by
  if ("decided_on" %in% names(df)) df$decided_on[a] <- accepted_on
  df
}
cand_dir <- "data-raw/candidates"
dir.create(cand_dir, showWarnings = FALSE)
all_dir <- file.path(sav_dir, "all")

# 1.5 題目關係（含 .sav 有、題庫沒有的新題號）
rel <- propose_relations(kit, v02, sav, meta_dir = sav_dir)
readr::write_csv(rel$relations, file.path(cand_dir, "item_relations.csv"), na = "")
rel$relations <- auto_to_pi(rel$relations)
kit <- apply_relations(kit, rel, levels = accept)

# 1.4 對照表：題目位置直接寫入；題組樹、成員、關係提示只出候選
cw <- import_crosswalk(file.path(all_dir, c("cw_M03W48.xlsx", "cw_M36W84.xlsx")), kit,
                       usage_dir = all_dir, sav_dir = sav_dir)
k <- match(paste(kit$dataset_vars$datasetID, kit$dataset_vars$varname),
           paste(cw$positions$datasetID, cw$positions$varname))
kit$dataset_vars$position[!is.na(k)] <- cw$positions$position[k[!is.na(k)]]
write_crosswalk_candidates(cw, cand_dir)
# 對照表的題組樹、成員、關係提示（D5）；D1 合併掉的題組改指向新題組，D4 拿掉的不加回來
for (x in c("group_paths", "member_hints")) {
  for (i in seq_len(nrow(mg))) {
    for (cc in intersect(c("groupID", "parentID"), names(cw[[x]]))) {
      cw[[x]][[cc]][cw[[x]][[cc]] %in% mg$from[i]] <- mg$to[i]
    }
  }
}
kit <- apply_group_candidates(kit, cw$group_paths, cw$member_hints, drop = drop)
kit <- apply_relation_hints(kit, cw$relation_hints, accepted_by, accepted_on)

# D2 對照表為準：調查有問、公開釋出檔沒有的變項也記下來，public = 0
cl <- cw$cw_long[cw$cw_long$present == "O" & nzchar(cw$cw_long$datasetID), ]
# 對照表和 .sav 的大小寫不一定一樣
cl <- cl[!paste(cl$datasetID, tolower(cl$varname)) %in%
           paste(kit$dataset_vars$datasetID, tolower(kit$dataset_vars$varname)) &
           cl$datasetID %in% sav$datasets$datasetID, ]
cl <- cl[!duplicated(paste(cl$datasetID, cl$varname)), ]
uid <- kit$items$item_uid[match(tolower(paste0(cl$instrument, ".", cl$varname)),
                                 tolower(kit$items$item_uid))]
uid[is.na(uid)] <- ""
latest <- tapply(as.integer(kit$item_versions$itemVer), kit$item_versions$item_uid, max)
kit$dataset_vars <- rbind(kit$dataset_vars, conform_kit_table(data.frame(
  datasetID = cl$datasetID, varname = cl$varname, item_uid = uid,
  itemVer = ifelse(nzchar(uid), as.character(latest[uid]), ""),
  kind = ifelse(nzchar(uid) & !kit$items$role[match(uid, kit$items$item_uid)] %in% "admin",
                "item", "system"),
  public = "0", stringsAsFactors = FALSE), "dataset_vars"))
cat("\n== D2 crosswalk-only vars:", nrow(cl), "\n")
print(cl[, c("datasetID", "varname")], row.names = FALSE)

# 1.6 區塊狀態
blocks <- propose_blocks(kit, usage_dir = all_dir, meta_dir = sav_dir,
                         crosswalk = utils::read.csv(file.path(sav_dir, "item_admin_from_crosswalk.csv"),
                                                     encoding = "UTF-8", stringsAsFactors = FALSE))
kit$block_status <- conform_kit_table(
  auto_to_pi(blocks$block_status[blocks$block_status$confidence %in% accept, ]), "block_status")
readr::write_csv(blocks$block_status, file.path(cand_dir, "block_status.csv"), na = "")

res <- check_kit(kit)

# 1.7 要注意清單
aud <- audit_kit(kit, sav = sav, noresponse = v02$noresponse_codes)
readr::write_csv(aud, file.path(cand_dir, "audit.csv"), na = "")
cat("\n== audit_kit\n")
print(audit_kit_summary(aud), row.names = FALSE)
cat("\n== check_kit:", nrow(res), "issues\n")
if (nrow(res)) print(table(res$table, res$check))

write_kit(kit, out_dir)
cat("\n== rows\n")
print(vapply(kit, nrow, integer(1)))
cat("\n== sav coverage\n")
print(sav$summary[, c("datasetID", "n_vars", "matched", "pct_matched", "items",
                      "items_matched", "system", "weight", "derived")], row.names = FALSE)
saveRDS(list(kit = kit, v02 = v02, des = des, sav = sav, rel = rel, cw = cw, blocks = blocks,
             audit = aud, check = res),
        file.path(sav_dir, "..", "kit_build.rds"))
