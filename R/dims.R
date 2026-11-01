# R/dims.R — 構面查詢的計算（沒有 Shiny）
#
# 構面查詢頁只呼叫這裡的函式；「用 R 怎麼做」給的也是同樣的函式：
#   dim_overview(bank)                 每個構面一列：題數、版本數、出現的波次數、有信度的波次數、最近一波
#   dim_items(bank, dim_id)            一個構面的題目 × 波次 ✓ 表
#   dim_overlap(bank, dim_id)          和這個構面共用題目的其他構面
#   dim_reliability_by_wave(bank, id)  一個構面各波次的信度
#   dim_report_data(bank, wave, dims = NULL)  構面報告的資料（Word／HTML 用）＋ 要注意的地方
#
# 「題」= 不同的 itemID；「版本」= itemID + itemVer。

# ── 內部小工具 ────────────────────────────────────────────────

# 題目的鍵：itemID + itemVer
dims_key <- function(df) paste(df$itemID, as.integer(df$itemVer), sep = "\r")

# 構面欄（itemDim 的非鍵欄）
dims_cols <- function(bank) get_matrix_cols(bank$itemDim)

# 構面 × 波次的題數矩陣：題數是不同的 itemID（同一題兩個版本在同一波只算一題）；
# 一題在 itemWave 找不到時不算任何波次
dims_wave_counts <- function(bank) {
  id <- bank$itemDim
  iw <- bank$itemWave
  dims <- get_matrix_cols(id)
  waves <- get_wave_cols(iw)
  k <- match(dims_key(id), dims_key(iw))
  W <- matrix(0L, nrow(id), length(waves), dimnames = list(NULL, waves))
  ok <- !is.na(k)
  if (any(ok) && length(waves) > 0L) {
    W[ok, ] <- as.matrix(iw[k[ok], waves, drop = FALSE])
  }
  out <- matrix(0L, length(dims), length(waves), dimnames = list(dims, waves))
  for (j in seq_along(dims)) {
    r <- which(id[[dims[j]]] == 1L)
    if (length(r) == 0L || length(waves) == 0L) next
    per_item <- rowsum(W[r, , drop = FALSE], id$itemID[r], reorder = FALSE) > 0L
    out[j, ] <- as.integer(colSums(per_item))
  }
  out
}

# 構面的名稱與別名（dimMeta 沒有這個構面時，名稱就是 dimID）
dims_meta <- function(bank, dim_ids) {
  dm <- ensure_dim_meta_cols(bank$dimMeta)
  i <- match(dim_ids, dm$dimID)
  name <- ifelse(is.na(i), dim_ids, dm$dimName[i])
  name[!nzchar(name)] <- dim_ids[!nzchar(name)]
  alias <- ifelse(is.na(i), "", dm$alias[i])
  data.frame(dimID = dim_ids, dimName = name, alias = alias,
             stringsAsFactors = FALSE)
}

# 畫面上的構面名稱：「dimName（alias）」，沒有別名時只有 dimName
dims_label <- function(bank, dim_ids) {
  m <- dims_meta(bank, dim_ids)
  vapply(seq_len(nrow(m)), function(i) {
    format_dim_report_heading(m$dimName[i], m$alias[i])
  }, character(1L))
}

# 找不到構面時停下來（白話）
dims_check_dim <- function(bank, dim_id) {
  if (length(dim_id) != 1L || is.na(dim_id) || !dim_id %in% dims_cols(bank)) {
    # 原文：這個題庫沒有構面「 | 」。
    kitDesign::kit_abort("\u9019\u500b\u984c\u5eab\u6c92\u6709\u69cb\u9762\u300c", dim_id, "\u300d\u3002")
  }
  invisible(dim_id)
}

# 一個構面在各波次的信度（wave 是向量）
dims_rel <- function(bank, dim_id, waves) {
  dm <- ensure_dim_meta_cols(bank$dimMeta)
  dr <- bank$dimReliability
  vapply(waves, function(w) get_dim_reliability(dim_id, w, dm, dr), numeric(1L),
         USE.NAMES = FALSE)
}

# ── 匯出的函式 ───────────────────────────────────────────────

#' Overview of all dimensions in an item bank
#'
#' One row per dimension (each non-key column of `itemDim`), with its name,
#' alias, how many items (distinct `itemID`) and item versions it has, in how
#' many waves at least one of its items appears, in how many of those waves it
#' has a reliability value, and its first and latest wave.
#'
#' @param bank An item bank from [load_bank()] (a list with `itemBank`,
#'   `itemDim`, `itemWave`, `dimMeta`, `dimReliability`).
#' @return A data.frame with columns `dimID`, `dimName`, `alias`, `n_items`
#'   (distinct `itemID`s assigned to the dimension), `n_versions` (item
#'   versions assigned), `n_waves`, `n_rel`, `first_wave`, `last_wave` (in wave
#'   order, youngest age first; `NA` when the dimension is in no wave).
#' @examples
#' bank <- load_bank("parent")
#' head(dim_overview(bank))
#' @export
dim_overview <- function(bank) {
  dims <- dims_cols(bank)
  counts <- dims_wave_counts(bank)
  meta <- dims_meta(bank, dims)
  id <- bank$itemDim
  n_items <- vapply(dims, function(d) length(unique(id$itemID[id[[d]] == 1L])),
                    integer(1L), USE.NAMES = FALSE)
  n_versions <- vapply(dims, function(d) sum(id[[d]] == 1L, na.rm = TRUE),
                       integer(1L), USE.NAMES = FALSE)
  n_waves <- as.integer(rowSums(counts > 0L))
  waves_of <- lapply(seq_along(dims), function(i) colnames(counts)[counts[i, ] > 0L])
  n_rel <- vapply(seq_along(dims), function(i) {
    if (length(waves_of[[i]]) == 0L) return(0L)
    sum(!is.na(dims_rel(bank, dims[i], waves_of[[i]])))
  }, integer(1L))
  pick <- function(f) vapply(waves_of, function(w) if (length(w)) f(w) else NA_character_,
                             character(1L))
  data.frame(meta, n_items = n_items, n_versions = n_versions, n_waves = n_waves,
             n_rel = as.integer(n_rel),
             first_wave = pick(function(w) w[1L]),
             last_wave = pick(function(w) w[length(w)]),
             stringsAsFactors = FALSE)
}

#' Items of one dimension, with the waves they appear in
#'
#' A crosswalk-like table: one row per item version assigned to the
#' dimension, and one column per wave in which at least one of the
#' dimension's items appears ("✓" when the item is in that wave).
#'
#' @param bank An item bank from [load_bank()].
#' @param dim_id The dimension (a column name of `itemDim`).
#' @return A data.frame with `itemID`, `itemVer`, `deprecated` (1 = no longer
#'   used), `respondent`, `label`, then one column per wave (in age order).
#'   Items missing from `itemBank` have an empty `label` and `respondent` and
#'   `NA` `deprecated`.
#' @examples
#' bank <- load_bank("parent")
#' dim_items(bank, "注意力")
#' @export
dim_items <- function(bank, dim_id) {
  dims_check_dim(bank, dim_id)
  id <- bank$itemDim
  iw <- bank$itemWave
  ib <- bank$itemBank
  rows <- id[id[[dim_id]] == 1L, c("itemID", "itemVer"), drop = FALSE]
  rows <- rows[order(rows$itemID, rows$itemVer), , drop = FALSE]
  i <- match(dims_key(rows), dims_key(ib))
  txt <- function(col) {
    v <- if (col %in% names(ib)) as.character(ib[[col]][i]) else rep(NA_character_, length(i))
    ifelse(is.na(v), "", v)
  }
  out <- data.frame(
    itemID = rows$itemID,
    itemVer = as.integer(rows$itemVer),
    deprecated = if ("deprecated" %in% names(ib)) as.integer(ib$deprecated[i]) else NA_integer_,
    respondent = txt("respondent"),
    label = collapse_newlines(txt("label")),
    stringsAsFactors = FALSE
  )
  waves <- get_wave_cols(iw)
  k <- match(dims_key(rows), dims_key(iw))
  for (w in waves) {
    v <- ifelse(is.na(k), 0L, iw[[w]][k])
    # 原文：✓
    if (any(v == 1L)) out[[w]] <- ifelse(v == 1L, "\u2713", "")
  }
  rownames(out) <- NULL
  out
}

#' 一個題組在 kit 的 group_tree 裡的子題組（只回傳這個題庫有的構面）
#'
#' 例如家長問卷的「社會能力」本身沒有題目，題目在「主動性」「合群性」…這些子題組。
#' @param kit kit（有 group_tree）；NULL 時回傳 character(0)
#' @return 子題組的 dimID（去掉問卷前綴），依 group_tree 出現的順序
#' @noRd
dim_children <- function(bank, dim_id, domain = NULL, kit = kit_builtin()) {
  if (is.null(kit) || is.null(kit$group_tree) || length(dim_id) != 1L) return(character(0))
  ins <- if (!is.null(domain) && domain %in% names(KIT_DOMAIN_INSTRUMENT)) {
    KIT_DOMAIN_INSTRUMENT[[domain]]
  } else {
    unique(sub("\\..*$", "", kit$group_tree$groupID))
  }
  gt <- kit$group_tree
  kids <- gt$groupID[gt$parentID %in% paste0(ins, ".", dim_id)]
  kids <- unique(kit_strip_prefix(kids))
  kids[kids %in% setdiff(dims_cols(bank), dim_id)]
}

#' 題組各波次題目表的 Excel 工作表：說明＋題目 × 波次（✓）
#'
#' 題組本身沒有題目、但有子題組時（例如「社會能力」），放子題組的題目，第一欄寫是哪個子題組。
#' @noRd
dim_items_workbook <- function(bank, dim_id, domain = NULL, kit = kit_builtin()) {
  items <- dim_items(bank, dim_id)
  kids <- if (nrow(items) == 0L) dim_children(bank, dim_id, domain, kit) else character(0)
  if (length(kids)) {
    parts <- lapply(kids, function(k) {
      it <- dim_items(bank, k)
      cbind(data.frame(x = rep(dims_label(bank, k), nrow(it)), stringsAsFactors = FALSE), it)
    })
    all_waves <- get_wave_cols(bank$itemWave)
    cols <- unique(unlist(lapply(parts, names)))
    cols <- c(setdiff(cols, all_waves), intersect(all_waves, cols))
    parts <- lapply(parts, function(p) {
      for (cc in setdiff(cols, names(p))) p[[cc]] <- rep("", nrow(p))
      p[, cols, drop = FALSE]
    })
    items <- do.call(rbind, parts)
    # 原文：子題組
    names(items)[1] <- "\u5b50\u984c\u7d44"
    rownames(items) <- NULL
  }
  waves <- intersect(get_wave_cols(bank$itemWave), names(items))
  names(items)[match(waves, names(items))] <- col_display_labels(waves, plain = TRUE)
  # 原文：一列是一個題目版本；✓ 表示這個版本在那次施測有問。deprecated = 1 是已停用的舊版本。
  note <- "\u4e00\u5217\u662f\u4e00\u500b\u984c\u76ee\u7248\u672c\uff1b\u2713 \u8868\u793a\u9019\u500b\u7248\u672c\u5728\u90a3\u6b21\u65bd\u6e2c\u6709\u554f\u3002deprecated = 1 \u662f\u5df2\u505c\u7528\u7684\u820a\u7248\u672c\u3002"
  if (length(kids)) {
    # 原文：這個題組本身沒有題目，表裡是它的子題組（
    note <- paste0(note, "\u9019\u500b\u984c\u7d44\u672c\u8eab\u6c92\u6709\u984c\u76ee\uff0c\u8868\u88e1\u662f\u5b83\u7684\u5b50\u984c\u7d44\uff08",
                   # 原文：、 | ）的題目，第一欄寫是哪個子題組。
                   paste(dims_label(bank, kids), collapse = "\u3001"), "\uff09\u7684\u984c\u76ee\uff0c\u7b2c\u4e00\u6b04\u5beb\u662f\u54ea\u500b\u5b50\u984c\u7d44\u3002")
  }
  info <- data.frame(
    # 原文：項目 | 內容
    "\u9805\u76ee" = c(
      # 原文：題組 | 題庫 | 題庫版本 | 產生日期 | 說明
      "\u984c\u7d44", "\u984c\u5eab", "\u984c\u5eab\u7248\u672c", "\u7522\u751f\u65e5\u671f", "\u8aaa\u660e"),
    "\u5167\u5bb9" = c(
      dims_label(bank, dim_id), bank_domain_label(domain) %||% "", kit_version() %||% "",
      format(Sys.Date()), note),
    check.names = FALSE, stringsAsFactors = FALSE)
  # 原文：說明 | 題目
  stats::setNames(list(info, items), c("\u8aaa\u660e", "\u984c\u76ee"))
}

#' Other dimensions that share items with one dimension
#'
#' An item counts as shared when the same `itemID` (any version) is assigned
#' to both dimensions.
#'
#' @param bank An item bank from [load_bank()].
#' @param dim_id The dimension (a column name of `itemDim`).
#' @return A data.frame, one row per other dimension sharing at least one
#'   item, most shared first: `dimID`, `dimName` (with alias, as on screen),
#'   `n_shared` (shared `itemID`s), `share` (`n_shared` as a percentage of
#'   this dimension's items, 0–100), `n_items` (the other dimension's items).
#' @examples
#' bank <- load_bank("parent")
#' dim_overlap(bank, "注意力")
#' @export
dim_overlap <- function(bank, dim_id) {
  dims_check_dim(bank, dim_id)
  id <- bank$itemDim
  dims <- setdiff(dims_cols(bank), dim_id)
  mine <- unique(id$itemID[id[[dim_id]] == 1L])
  empty <- data.frame(dimID = character(0), dimName = character(0), n_shared = integer(0),
                      share = numeric(0), n_items = integer(0), stringsAsFactors = FALSE)
  if (length(mine) == 0L || length(dims) == 0L) return(empty)
  rows <- id$itemID %in% mine
  n_shared <- vapply(dims, function(d) length(unique(id$itemID[rows & id[[d]] == 1L])),
                     integer(1L), USE.NAMES = FALSE)
  keep <- n_shared > 0L
  if (!any(keep)) return(empty)
  d <- dims[keep]
  out <- data.frame(
    dimID = d,
    dimName = dims_label(bank, d),
    n_shared = n_shared[keep],
    share = round(100 * n_shared[keep] / length(mine), 1),
    n_items = vapply(d, function(x) length(unique(id$itemID[id[[x]] == 1L])), integer(1L),
                     USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$n_shared, out$dimID), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Reliability of one dimension in each wave
#'
#' For each wave in which the dimension has at least one item: the number of
#' its items in that wave and its reliability. A wave-specific value in
#' `dimReliability` comes first; otherwise the general `dimMeta$reliability`.
#'
#' @param bank An item bank from [load_bank()].
#' @param dim_id The dimension (a column name of `itemDim`).
#' @return A data.frame with `wave`, `n_items` (distinct `itemID`s),
#'   `reliability` (`NA` when
#'   there is none).
#' @noRd
dim_reliability_by_wave <- function(bank, dim_id) {
  dims_check_dim(bank, dim_id)
  counts <- dims_wave_counts(bank)[dim_id, ]
  waves <- names(counts)[counts > 0L]
  data.frame(
    wave = waves,
    n_items = as.integer(counts[waves]),
    reliability = dims_rel(bank, dim_id, waves),
    stringsAsFactors = FALSE
  )
}

#' Data for the dimension report of one wave
#'
#' Collects, for every dimension that has items in `wave` (or only for the
#' dimensions in `dims`), its name, alias,
#' reliability, note and items (one row per itemID; when an item has several
#' versions in the wave, the latest version is used). Also lists what needs
#' attention, so it can be shown before the report is shared.
#'
#' @param bank An item bank from [load_bank()].
#' @param wave A wave (a column name of `itemWave`).
#' @param domain The item bank name written in the report (for example
#'   `"parent"`).
#' @param dims `NULL` (all dimensions) or the dimensions to report (column
#'   names of `itemDim`). The problems are then limited to these dimensions
#'   and their items.
#' @return A list:
#'   - `domain`, `wave`, `scope` (`dims`, or `NULL` for all dimensions);
#'   - `dims`: a list with one element per dimension (`dimID`, `dimName`,
#'     `dimNameRaw`, `alias`, `reliability`, `note`, `items` — a data.frame
#'     with `itemID`, `itemVer`, `block`, `label`); this is what
#'     the Word and HTML reports take;
#'   - `n_items`: number of distinct items in the report;
#'   - `not_in_wave`: dimensions that have no items in this wave (left out);
#'   - `problems`: a data.frame with `type`, `id`, `message`. Types:
#'     `"no_reliability"` (a dimension in the report without reliability),
#'     `"empty_dim"` (a dimension with no items at all, including dimensions
#'     only listed in `dimMeta`), `"missing_item"` (an item in `itemDim` that
#'     is not in `itemBank`).
#' @examples
#' bank <- load_bank("parent")
#' rd <- dim_report_data(bank, "M036小", domain = "parent")
#' length(rd$dims)
#' table(rd$problems$type)
#' # 只做一個構面
#' dim_report_data(bank, "M036小", domain = "parent", dims = "注意力")$dims[[1]]$items
#' @export
dim_report_data <- function(bank, wave, domain = NULL, dims = NULL) {
  iw <- bank$itemWave
  waves <- get_wave_cols(iw)
  if (length(wave) != 1L || is.na(wave) || !wave %in% waves) {
    # 原文：這個題庫沒有波次「 | 」。
    kitDesign::kit_abort("\u9019\u500b\u984c\u5eab\u6c92\u6709\u6ce2\u6b21\u300c", wave, "\u300d\u3002")
  }
  id <- bank$itemDim
  ib <- bank$itemBank
  dm <- ensure_dim_meta_cols(bank$dimMeta)
  dr <- bank$dimReliability
  all_dims <- get_matrix_cols(id)
  scope <- NULL
  if (!is.null(dims)) {
    for (d in dims) dims_check_dim(bank, d)
    scope <- unique(as.character(dims))
  }
  dims <- scope %||% all_dims

  id_key <- dims_key(id)
  in_wave <- id_key %in% dims_key(iw[iw[[wave]] == 1L, , drop = FALSE])
  ib_i <- match(id_key, dims_key(ib))

  out <- list()
  not_in_wave <- character(0)
  for (d in dims) {
    hit <- which(id[[d]] == 1L & in_wave & !is.na(ib_i))
    if (length(hit) == 0L) {
      if (any(id[[d]] == 1L)) not_in_wave <- c(not_in_wave, d)
      next
    }
    items <- ib[ib_i[hit], c("itemID", "itemVer", "block", "label"), drop = FALSE]
    items$label <- collapse_newlines(items$label)
    # 同一題有好幾個版本都在這個波次時，用最新的版本
    items <- items[order(items$itemID, -items$itemVer), , drop = FALSE]
    items <- items[!duplicated(items$itemID), , drop = FALSE]
    rownames(items) <- NULL
    meta <- resolve_dim_meta(d, wave, dm, dr)
    out[[length(out) + 1L]] <- list(
      dimID = d,
      dimName = meta$dimName,
      dimNameRaw = dims_meta(bank, d)$dimName,
      alias = meta$alias,
      reliability = meta$reliability,
      note = meta$note,
      items = items
    )
  }

  # 要注意的地方
  no_rel <- vapply(out, function(x) x$dimID, "")[
    vapply(out, function(x) is.na(x$reliability), logical(1L))]
  empty <- dims[vapply(dims, function(d) !any(id[[d]] == 1L), logical(1L))]
  if (is.null(scope)) empty <- c(empty, setdiff(dm$dimID, all_dims))
  assigned <- rowSums(as.matrix(id[, dims, drop = FALSE]) == 1L) > 0L
  missing_rows <- id[assigned & is.na(ib_i), c("itemID", "itemVer"), drop = FALSE]
  missing_ids <- if (nrow(missing_rows) == 0L) character(0) else
    unique(paste0(missing_rows$itemID, " v", missing_rows$itemVer))
  problems <- rbind(
    dims_problem("no_reliability", no_rel,
                 # 原文：「 | 」在  |  沒有信度
                 paste0("\u300c", no_rel, "\u300d\u5728 ", wave, " \u6c92\u6709\u4fe1\u5ea6")),
    # 原文：「 | 」沒有任何題目
    dims_problem("empty_dim", empty, paste0("\u300c", empty, "\u300d\u6c92\u6709\u4efb\u4f55\u984c\u76ee")),
    dims_problem("missing_item", missing_ids,
                 # 原文： 在構面歸屬裡有，題目表裡沒有
                 paste0(missing_ids, " \u5728\u69cb\u9762\u6b78\u5c6c\u88e1\u6709\uff0c\u984c\u76ee\u8868\u88e1\u6c92\u6709"))
  )

  list(
    domain = domain %||% "",
    wave = wave,
    scope = scope,
    dims = out,
    n_items = length(unique(unlist(lapply(out, function(x) x$items$itemID)))),
    not_in_wave = not_in_wave,
    problems = problems
  )
}

dims_problem <- function(type, id, message) {
  if (length(id) == 0L) message <- character(0)
  data.frame(type = rep(type, length(id)), id = as.character(id),
             message = as.character(message), stringsAsFactors = FALSE)
}

# 狀態框用：每一種要注意的事寫成一句（最多 3 句），名字最多列 3 個
dim_report_warnings <- function(rd) {
  p <- rd$problems
  if (is.null(p) || nrow(p) == 0L) return(character(0))
  names_of <- function(x) {
    x <- unique(x)
    # 原文：「 | 」 | 、
    paste0(paste0("\u300c", utils::head(x, 3L), "\u300d", collapse = "\u3001"),
           # 原文：等  |  個
           if (length(x) > 3L) paste0("\u7b49 ", kitDesign::kit_num(length(x)), " \u500b") else "")
  }
  out <- character(0)
  ids <- p$id[p$type == "no_reliability"]
  if (length(ids)) out <- c(out, paste0(
    # 原文： 個構面在  |  沒有信度：
    kitDesign::kit_num(length(ids)), " \u500b\u69cb\u9762\u5728 ", rd$wave, " \u6c92\u6709\u4fe1\u5ea6\uff1a", names_of(ids),
    # 原文：。可以在「題庫編修」的構面資訊填。
    "\u3002\u53ef\u4ee5\u5728\u300c\u984c\u5eab\u7de8\u4fee\u300d\u7684\u69cb\u9762\u8cc7\u8a0a\u586b\u3002"))
  ids <- p$id[p$type == "empty_dim"]
  if (length(ids)) out <- c(out, paste0(
    # 原文： 個構面沒有任何題目： | 。
    kitDesign::kit_num(length(ids)), " \u500b\u69cb\u9762\u6c92\u6709\u4efb\u4f55\u984c\u76ee\uff1a", names_of(ids), "\u3002"))
  ids <- p$id[p$type == "missing_item"]
  if (length(ids)) out <- c(out, paste0(
    # 原文： 題在構面歸屬裡有、題目表裡沒有，沒有放進報告：
    kitDesign::kit_num(length(ids)), " \u984c\u5728\u69cb\u9762\u6b78\u5c6c\u88e1\u6709\u3001\u984c\u76ee\u8868\u88e1\u6c92\u6709\uff0c\u6c92\u6709\u653e\u9032\u5831\u544a\uff1a",
    # 原文：、
    paste(utils::head(ids, 3L), collapse = "\u3001"),
    # 原文： 等 | 。
    if (length(ids) > 3L) " \u7b49" else "", "\u3002"))
  utils::head(out, 3L)
}
