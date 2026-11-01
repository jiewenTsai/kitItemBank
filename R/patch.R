# R/patch.R — 修改包：編修頁的改動 → 0.3 題庫（kit）的改動列 → 套用
#
# 0.3 的編修頁還是改 0.2 形狀的題庫（bank_to_v02() 轉出來的）；下載時用 kit_patch()
# 把「改前／改後」換成 kit 的改動列，存成修改包 Excel。懂技術的人用 apply_patch() 套用，
# 再跑 build／check，送進 git。
#
# 一列改動：op（add 新增一列／delete 刪除一列／set 改一格）、table、key（主鍵值用 | 接起來，
# 照 KIT_SCHEMA 的主鍵順序）、column、from、to。新增的列先 add（只有主鍵），其他欄再用 set 補。
# 能轉的：題組成員（itemDim）、題組名冊（dimMeta）、題目文字等欄位（itemBank）。
# 不能轉的（例如改 itemWave：0.3 的施測內容以 .sav 和對照表為準）列在 unsupported，不會套用。

PATCH_COLS <- c("op", "table", "key", "column", "from", "to")

empty_patch <- function() {
  as.data.frame(stats::setNames(rep(list(character(0)), length(PATCH_COLS)), PATCH_COLS),
                stringsAsFactors = FALSE)
}

patch_rows <- function(op, table, key, column = "", from = "", to = "") {
  n <- max(length(key), 1L)
  if (!length(key)) return(empty_patch())
  data.frame(op = rep(op, n), table = rep(table, n), key = key, column = rep_len(column, n),
             from = rep_len(from, n), to = rep_len(to, n), stringsAsFactors = FALSE)
}

#' 0.2 題庫的「題目 × 題組」：itemID × dimID，任何一版是 1 就算
#' @noRd
v02_membership <- function(bank) {
  m <- bank$itemDim
  dims <- get_matrix_cols(m)
  if (!length(dims) || !nrow(m)) return(character(0))
  x <- as.matrix(m[dims]) == 1L | as.matrix(m[dims]) == "1"
  hit <- which(x, arr.ind = TRUE)
  unique(paste(m$itemID[hit[, 1L]], dims[hit[, 2L]], sep = "\r"))
}

#' Turn edits of a 0.2-style bank into a 0.3 kit patch
#'
#' @param kit The kit the edited bank was made from (with [bank_to_v02()]).
#' @param old,new The bank before and after editing.
#' @param domain `"parent"`, `"child_adolescent"` or `"childcare_teacher"`.
#' @return A list: `patch` (data frame `op`, `table`, `key`, `column`, `from`, `to`) and
#'   `unsupported` (rows of [bank_diff()] that cannot be expressed in the kit).
#' @export
kit_patch <- function(kit, old, new, domain = "parent") {
  domain <- match.arg(domain, names(KIT_DOMAIN_INSTRUMENT))
  ins <- KIT_DOMAIN_INSTRUMENT[[domain]]
  # paste0() 遇到長度 0 會回傳 "P."，要先擋掉
  uid <- function(id) if (length(id)) paste0(ins, ".", id) else character(0)
  gid <- uid
  out <- list()
  add <- function(x) out[[length(out) + 1L]] <<- x
  diff <- bank_diff(old, new)
  # 原文：表 | 欄
  tb_col <- "\u8868"
  supported <- rep(FALSE, nrow(diff))

  # 題組名冊 ----
  dm_old <- ensure_dim_meta_cols(old$dimMeta)
  dm_new <- ensure_dim_meta_cols(new$dimMeta)
  added <- setdiff(dm_new$dimID, dm_old$dimID)
  gone <- setdiff(dm_old$dimID, dm_new$dimID)
  add(patch_rows("add", "groups", gid(added)))
  for (d in added) {
    r <- dm_new[dm_new$dimID == d, ]
    add(patch_rows("set", "groups", gid(d), "label", "", r$dimName))
    if (nzchar(r$alias)) add(patch_rows("set", "groups", gid(d), "alias", "", r$alias))
    if (nzchar(r$note)) add(patch_rows("set", "groups", gid(d), "scoring_note", "", r$note))
  }
  both <- intersect(dm_old$dimID, dm_new$dimID)
  map <- c(dimName = "label", alias = "alias", note = "scoring_note")
  for (cc in names(map)) {
    a <- dm_old[[cc]][match(both, dm_old$dimID)]
    b <- dm_new[[cc]][match(both, dm_new$dimID)]
    ch <- a != b
    add(patch_rows("set", "groups", gid(both[ch]), map[[cc]], a[ch], b[ch]))
  }

  # 題組成員：以題號為單位，套到這一題出現過的所有題本 ----
  mo <- v02_membership(old)
  mn <- v02_membership(new)
  forms_of <- function(u) {
    ds <- kit$dataset_vars$datasetID[kit$dataset_vars$item_uid == u]
    f <- unique(kit$datasets$formID[kit$datasets$datasetID %in% ds])
    f[nzchar(f)]
  }
  for (k in setdiff(mn, mo)) {
    p <- strsplit(k, "\r", fixed = TRUE)[[1L]]
    for (f in forms_of(uid(p[1L]))) {
      add(patch_rows("add", "group_members", paste(f, gid(p[2L]), uid(p[1L]), "official", sep = "|")))
    }
  }
  for (k in setdiff(mo, mn)) {
    p <- strsplit(k, "\r", fixed = TRUE)[[1L]]
    gm <- kit$group_members[kit$group_members$item_uid == uid(p[1L]) &
                              kit$group_members$groupID == gid(p[2L]), , drop = FALSE]
    add(patch_rows("delete", "group_members",
                   kit_key_str(gm, KIT_SCHEMA$group_members$key)))
  }
  add(patch_rows("delete", "groups", gid(gone)))
  supported <- supported | diff[[tb_col]] %in% c("itemDim", "dimMeta")

  # 題目欄位 ----
  ib_map <- c(label = "item_versions.label", stemID = "item_versions.stem",
              itemType = "item_versions.itemType", note = "item_versions.note",
              block = "items.block", deprecated = "items.deprecated")
  # 原文：題號或構面 | 版本 | 欄 | 原本 | 改成
  is_ib <- diff[[tb_col]] == "itemBank" & diff[["\u6b04"]] %in% names(ib_map)
  for (i in which(is_ib)) {
    # 原文：題號或構面
    id <- diff[["\u984c\u865f\u6216\u69cb\u9762"]][i]
    # 原文：欄
    target <- strsplit(ib_map[[diff[["\u6b04"]][i]]], ".", fixed = TRUE)[[1L]]
    # 原文：版本
    key <- if (target[1L] == "items") uid(id) else paste(uid(id), diff[["\u7248\u672c"]][i], sep = "|")
    add(patch_rows("set", target[1L], key, target[2L],
                   # 原文：原本 | 改成
                   diff[["\u539f\u672c"]][i], diff[["\u6539\u6210"]][i]))
  }
  supported <- supported | is_ib

  patch <- do.call(rbind, out)
  if (is.null(patch)) patch <- empty_patch()
  patch <- patch[!duplicated(patch), , drop = FALSE]
  rownames(patch) <- NULL
  list(patch = patch, unsupported = diff[!supported, , drop = FALSE])
}

#' Apply a kit patch
#'
#' @param kit A kit.
#' @param patch A patch data frame (see [kit_patch()]).
#' @return The patched kit. Stops when a row cannot be applied (unknown table or column,
#'   a key to delete or set that does not exist, a key to add that already exists).
#'   Run [check_kit()] afterwards.
#' @export
apply_patch <- function(kit, patch) {
  for (i in seq_len(nrow(patch))) {
    p <- patch[i, ]
    sch <- KIT_SCHEMA[[p$table]]
    if (is.null(sch)) kitDesign::kit_abort("unknown table in patch: ", p$table)
    df <- kit[[p$table]]
    keys <- kit_key_str(df, sch$key)
    hit <- which(keys == p$key)
    if (p$op == "add") {
      if (length(hit)) kitDesign::kit_abort("patch adds an existing row: ", p$table, " ", p$key)
      vals <- strsplit(p$key, "|", fixed = TRUE)[[1L]]
      if (length(vals) != length(sch$key)) kitDesign::kit_abort("bad key: ", p$key)
      new <- empty_kit_table(p$table)[NA_integer_, , drop = FALSE]
      new[] <- ""
      new[sch$key] <- as.list(vals)
      kit[[p$table]] <- rbind(df, new)
    } else if (p$op == "delete") {
      if (!length(hit)) kitDesign::kit_abort("patch deletes a missing row: ", p$table, " ", p$key)
      kit[[p$table]] <- df[-hit, , drop = FALSE]
    } else if (p$op == "set") {
      if (!p$column %in% names(sch$cols)) kitDesign::kit_abort("unknown column: ", p$column)
      if (!length(hit)) kitDesign::kit_abort("patch sets a missing row: ", p$table, " ", p$key)
      kit[[p$table]][[p$column]][hit] <- p$to
    } else {
      kitDesign::kit_abort("unknown op: ", p$op)
    }
  }
  kit[] <- lapply(names(kit), function(tb) {
    df <- kit[[tb]]
    rownames(df) <- NULL
    df
  })
  kit
}

#' 修改包 Excel 的工作表：說明、改動（kit 改動列）、不能轉的改動、原始改動（0.2）
#' @noRd
patch_workbook <- function(res, changes, domain, base_version = kit_version(), meta = list()) {
  # meta：審核頁會讀的 reason、decided_by、submitted_by（有填才放進說明頁）
  meta <- unlist(meta[intersect(c("reason", "decided_by", "submitted_by"), names(meta))])
  meta <- meta[!is.na(meta) & nzchar(meta)]
  info <- data.frame(
    # 原文：項目 | 內容
    "\u9805\u76ee" = c("kitItemBank", "domain", "base_version", "date", "n_patch", "n_unsupported",
                       names(meta)),
    # 原文：內容 | 0.3 修改包
    "\u5167\u5bb9" = c("0.3 \u4fee\u6539\u5305", domain, base_version %||% "", format(Sys.Date()),
                       nrow(res$patch), nrow(res$unsupported), unname(meta)),
    check.names = FALSE, stringsAsFactors = FALSE)
  sheets <- list(info, res$patch)
  # 原文：說明 | 改動 | 不能轉的改動 | 原始改動
  names(sheets) <- c("\u8aaa\u660e", "\u6539\u52d5")
  # 原文：不能轉的改動
  if (nrow(res$unsupported)) sheets[["\u4e0d\u80fd\u8f49\u7684\u6539\u52d5"]] <- res$unsupported
  # 原文：原始改動
  if (nrow(changes)) sheets[["\u539f\u59cb\u6539\u52d5"]] <- changes
  sheets
}

#' Read a patch workbook back
#'
#' @param path A `.xlsx` downloaded from the edit page.
#' @return A list: `patch` (data frame) and `base_version`.
#' @export
read_patch_xlsx <- function(path) {
  # 原文：說明 | 改動
  info <- readxl::read_excel(path, sheet = "\u8aaa\u660e", col_types = "text")
  # 原文：改動
  patch <- as.data.frame(readxl::read_excel(path, sheet = "\u6539\u52d5", col_types = "text"))
  patch[] <- lapply(patch, function(v) { v[is.na(v)] <- ""; v })
  list(patch = patch[PATCH_COLS], base_version = info[[2]][info[[1]] == "base_version"])
}
