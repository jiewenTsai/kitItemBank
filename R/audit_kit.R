# R/audit_kit.R — 套件資料（kit）的「要注意」清單：只列出來等人判斷，不改任何東西
#
# check_kit() 管會擋下來的結構錯誤；這裡管設計 v2 第 3 節「列為要注意」的項目。
# 文字檢查沿用 R/audit.R 的 AUDIT_SIMPLIFIED、audit_count()、audit_short()。

# 每一種檢查：代碼、主要的表、畫面上的名稱（照這個順序排）
AUDIT_KIT_CHECKS <- data.frame(
  check = c("coverage_uid_blank", "coverage_sav", "form_year_diff", "renamed_no_group",
            "missing_code_scheme", "group_only_deprecated", "group_single_item", "group_empty",
            "group_nonleaf_member", "text_simplified", "text_brackets", "text_space",
            "text_newline", "internal_admins", "role_module_blank"),
  table = c("dataset_vars", "dataset_vars", "datasets", "item_relations",
            "item_versions", "group_members", "group_members", "groups",
            "group_members", "item_versions", "item_versions", "item_versions",
            "item_versions", "datasets", "items"),
  # 原文：題目變項沒有題號 | 資料檔和 .sav 表頭對不上
  label = c("\u984c\u76ee\u8b8a\u9805\u6c92\u6709\u984c\u865f", "\u8cc7\u6599\u6a94\u548c .sav \u8868\u982d\u5c0d\u4e0d\u4e0a",
            # 原文：同一題本、同一年度的資料檔題目不同 | 換題號後的新題沒有接上題組
            "\u540c\u4e00\u984c\u672c\u3001\u540c\u4e00\u5e74\u5ea6\u7684\u8cc7\u6599\u6a94\u984c\u76ee\u4e0d\u540c", "\u63db\u984c\u865f\u5f8c\u7684\u65b0\u984c\u6c92\u6709\u63a5\u4e0a\u984c\u7d44",
            # 原文：遺漏代碼不在資料檔的方案裡
            "\u907a\u6f0f\u4ee3\u78bc\u4e0d\u5728\u8cc7\u6599\u6a94\u7684\u65b9\u6848\u88e1",
            # 原文：題組只剩停用的題 | 題組只有 1 題 | 題組沒有任何題目
            "\u984c\u7d44\u53ea\u5269\u505c\u7528\u7684\u984c", "\u984c\u7d44\u53ea\u6709 1 \u984c", "\u984c\u7d44\u6c92\u6709\u4efb\u4f55\u984c\u76ee",
            # 原文：題目掛在中間層題組
            "\u984c\u76ee\u639b\u5728\u4e2d\u9593\u5c64\u984c\u7d44",
            # 原文：簡體字 | 括號不成對 | 文字前後有空白
            "\u7c21\u9ad4\u5b57", "\u62ec\u865f\u4e0d\u6210\u5c0d", "\u6587\u5b57\u524d\u5f8c\u6709\u7a7a\u767d",
            # 原文：文字有換行 | 內部施測的資料檔（v02） | 題目的角色或模組空白
            "\u6587\u5b57\u6709\u63db\u884c", "\u5167\u90e8\u65bd\u6e2c\u7684\u8cc7\u6599\u6a94\uff08v02\uff09", "\u984c\u76ee\u7684\u89d2\u8272\u6216\u6a21\u7d44\u7a7a\u767d"),
  level = "note",
  stringsAsFactors = FALSE
)

# 把 kit 的一張表取出來，全部轉成字串、NA 換成 ""
audit_kit_tb <- function(kit, name) {
  df <- kit[[name]]
  if (is.null(df)) df <- empty_kit_table(name)
  df[] <- lapply(df, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })
  df
}

# 向量接成「a、b、c…（共 n）」
audit_kit_list <- function(x, max = 5L) {
  x <- unique(x)
  # 原文：、
  s <- paste(utils::head(x, max), collapse = "\u3001")
  # 原文：…（共  | ）
  if (length(x) > max) s <- paste0(s, "\u2026\uff08\u5171 ", length(x), "\uff09")
  s
}

# 一欄文字的四種檢查（簡體字、括號、前後空白、換行）；回傳 list of data.frame
audit_kit_text <- function(v, key, table, what, brackets = TRUE) {
  v[is.na(v)] <- ""
  out <- list()
  one <- function(check, x, detail) {
    if (length(x)) out[[length(out) + 1L]] <<- data.frame(
      check = check, table = table, key = key[x], detail = detail, stringsAsFactors = FALSE)
  }
  simp_re <- paste0("[", paste(names(AUDIT_SIMPLIFIED), collapse = ""), "]")
  x <- which(grepl(simp_re, v, perl = TRUE))
  one("text_simplified", x, vapply(x, function(i) {
    ch <- unique(regmatches(v[i], gregexpr(simp_re, v[i], perl = TRUE))[[1]])
    # 原文：有簡體字
    paste0(what, "\u6709\u7c21\u9ad4\u5b57",
           # 原文：「 | 」（應為「 | 」） | 、
           paste0("\u300c", ch, "\u300d\uff08\u61c9\u70ba\u300c", AUDIT_SIMPLIFIED[ch], "\u300d\uff09", collapse = "\u3001"))
  }, ""))
  # 原文：[(（] | [)）]
  x <- if (brackets) which(audit_count(v, "[(\uff08]") != audit_count(v, "[)\uff09]")) else integer(0)
  # 原文：的括號不成對：「 | 」
  one("text_brackets", x, paste0(what, "\u7684\u62ec\u865f\u4e0d\u6210\u5c0d\uff1a\u300c", audit_short(v[x]), "\u300d"))
  # 原文：^[ \t　]|[ \t　]$
  x <- which(grepl("^[ \t\u3000]|[ \t\u3000]$", v, perl = TRUE))
  # 原文：前後有空白
  one("text_space", x, rep(paste0(what, "\u524d\u5f8c\u6709\u7a7a\u767d"), length(x)))
  x <- which(grepl("[\r\n]", v))
  # 原文：裡有換行
  one("text_newline", x, rep(paste0(what, "\u88e1\u6709\u63db\u884c"), length(x)))
  out
}

#' List the parts of a kit that need a person to decide
#'
#' Content checks from section 3 of the 0.3 design ("要注意"): variables marked as items but
#' without an `item_uid`; `.sav` variables missing from `dataset_vars` (and the reverse);
#' datasets of the same form, respondent and survey year whose item sets differ without a
#' `block_status` row explaining it; renamed items whose new item lost its groups; 0.2
#' non-response codes that are not in the dataset's missing-value scheme (e.g. 「拒答:5」 where
#' the data use 7777); groups with one item, only deprecated items or no items; items attached
#' to a non-leaf group; simplified characters, unbalanced brackets, stray spaces and newlines in
#' item and option text; internal (`file = "v02"`) datasets; items with no role or module.
#' Nothing is changed; blocking structural problems are in [check_kit()].
#'
#' @param kit A kit list, e.g. from [read_kit()].
#' @param sav Optional output of `import_sav_dir()` (or a data frame with `datasetID` and
#'   `varname`) to compare against `dataset_vars`.
#' @param noresponse Optional data frame with `item_uid`, `itemVer`, `code`, `label`: the 0.2
#'   non-response codes (`import_v02()$noresponse_codes`).
#' @return A data frame, one row per finding: `level` (always `"note"`), `check` (short code,
#'   see `AUDIT_KIT_CHECKS`), `table`, `key` (primary key; several columns joined with `|`) and
#'   `detail` (one line in Traditional Chinese).
#' @seealso [audit_kit_summary()], [check_kit()], [bank_audit()].
#' @examples
#' audit_kit(empty_kit())
#' @export
audit_kit <- function(kit, sav = NULL, noresponse = NULL) {
  ds <- audit_kit_tb(kit, "datasets")
  dv <- audit_kit_tb(kit, "dataset_vars")
  adm <- audit_kit_tb(kit, "admins")
  items <- audit_kit_tb(kit, "items")
  iv <- audit_kit_tb(kit, "item_versions")
  vc <- audit_kit_tb(kit, "value_codes")
  gm <- audit_kit_tb(kit, "group_members")
  gt <- audit_kit_tb(kit, "group_tree")
  grp <- audit_kit_tb(kit, "groups")
  rel <- audit_kit_tb(kit, "item_relations")
  bs <- audit_kit_tb(kit, "block_status")
  ms <- audit_kit_tb(kit, "missing_schemes")

  out <- list()
  add <- function(check, table, key, detail) {
    if (length(key) == 0L) return(invisible())
    out[[length(out) + 1L]] <<- data.frame(check = check, table = table, key = key,
                                           detail = detail, stringsAsFactors = FALSE)
  }

  # ---- 1. 涵蓋率 ----------------------------------------------------------------
  x <- which(dv$kind == "item" & dv$item_uid == "")
  add("coverage_uid_blank", "dataset_vars", paste(dv$datasetID[x], dv$varname[x], sep = "|"),
      # 原文：kind 是 item，但 item_uid 空白
      rep("kind \u662f item\uff0c\u4f46 item_uid \u7a7a\u767d", length(x)))
  if (!is.null(sav)) {
    sv <- if (is.data.frame(sav)) sav else sav$dataset_vars
    sv_ids <- if (!is.data.frame(sav) && !is.null(sav$datasets)) sav$datasets$datasetID else character(0)
    sv_ids <- unique(c(as.character(sv_ids), as.character(sv$datasetID)))
    k_sav <- paste(sv$datasetID, sv$varname, sep = "|")
    in_ids <- dv$datasetID %in% sv_ids
    k_kit <- paste(dv$datasetID, dv$varname, sep = "|")
    a <- setdiff(unique(k_sav), k_kit)
    # 原文：.sav 表頭有，dataset_vars 沒有
    add("coverage_sav", "dataset_vars", a, rep(".sav \u8868\u982d\u6709\uff0cdataset_vars \u6c92\u6709", length(a)))
    b <- setdiff(unique(k_kit[in_ids]), k_sav)
    # 原文：dataset_vars 有，.sav 表頭沒有
    add("coverage_sav", "dataset_vars", b, rep("dataset_vars \u6709\uff0c.sav \u8868\u982d\u6c92\u6709", length(b)))
    nos <- setdiff(ds$datasetID[ds$file != "v02"], sv_ids)
    add("coverage_sav", "datasets", nos,
        # 原文：釋出的資料檔沒有 .sav 表頭可以對照
        rep("\u91cb\u51fa\u7684\u8cc7\u6599\u6a94\u6c92\u6709 .sav \u8868\u982d\u53ef\u4ee5\u5c0d\u7167", length(nos)))
  }

  # ---- 2. 同一題本、同一年度的資料檔，題目不同 ----------------------------------------
  year <- substr(adm$surveyFrom[match(ds$adminID, adm$adminID)], 1L, 4L)
  year[is.na(year)] <- ""
  ok <- ds$formID != "" & year != ""
  gkey <- paste(ds$formID, ds$respondent, year, sep = "|")
  dvi <- dv[dv$kind == "item" & dv$item_uid != "", c("datasetID", "item_uid")]
  iset <- split(dvi$item_uid, dvi$datasetID)
  blk_of <- function(u) {
    b <- items$block[match(u, items$item_uid)]
    # 原文：（無區塊）
    b[is.na(b) | b == ""] <- "\uff08\u7121\u5340\u584a\uff09"
    b
  }
  # 區塊有 block_status 的列就算有解釋
  explained <- function(u, ids) {
    b <- items$block[match(u, items$item_uid)]
    b[is.na(b)] <- ""
    b %in% bs$block[bs$datasetID %in% ids]
  }
  diff_txt <- function(id, u) {
    # 原文： 沒有多的題
    if (length(u) == 0L) return(paste0(id, " \u6c92\u6709\u591a\u7684\u984c"))
    tb <- sort(table(blk_of(u)), decreasing = TRUE)
    # 原文： 多  |  題（
    paste0(id, " \u591a ", length(u), " \u984c\uff08",
           # 原文：）
           audit_kit_list(paste0(names(tb), " ", as.integer(tb)), 4L), "\uff09")
  }
  for (g in unique(gkey[ok])) {
    ids <- ds$datasetID[ok & gkey == g]
    if (length(ids) < 2L) next
    pr <- utils::combn(ids, 2L)
    for (j in seq_len(ncol(pr))) {
      a <- pr[1L, j]; b <- pr[2L, j]
      ua <- setdiff(iset[[a]], iset[[b]]); ub <- setdiff(iset[[b]], iset[[a]])
      ua <- ua[!explained(ua, c(a, b))]; ub <- ub[!explained(ub, c(a, b))]
      if (length(ua) + length(ub) == 0L) next
      add("form_year_diff", "datasets", paste(a, b, sep = "|"),
          # 原文：同為  | 、 | ，沒有區塊狀態可以解釋：
          paste0("\u540c\u70ba ", sub("\\|", "\u3001", g), "\uff0c\u6c92\u6709\u5340\u584a\u72c0\u614b\u53ef\u4ee5\u89e3\u91cb\uff1a",
                 # 原文：；
                 diff_txt(a, ua), "\uff1b", diff_txt(b, ub)))
    }
  }

  # ---- 3. 換題號後的新題沒有接上舊題的題組 -----------------------------------------
  rn <- rel[rel$kind == "renamed", , drop = FALSE]
  x <- which(rn$from_uid %in% gm$item_uid & !(rn$to_uid %in% gm$item_uid))
  add("renamed_no_group", "item_relations",
      paste(rn$from_uid[x], rn$from_ver[x], rn$to_uid[x], rn$to_ver[x], sep = "|"),
      # 原文：舊題  |  在題組 
      vapply(x, function(i) paste0("\u820a\u984c ", rn$from_uid[i], " \u5728\u984c\u7d44 ",
                                   audit_kit_list(gm$groupID[gm$item_uid == rn$from_uid[i]], 3L),
                                   # 原文：；新題  |  沒有任何題組
                                   "\uff1b\u65b0\u984c ", rn$to_uid[i], " \u6c92\u6709\u4efb\u4f55\u984c\u7d44"), ""))

  # ---- 4. 遺漏代碼不在資料檔的方案裡 ----------------------------------------------
  if (!is.null(noresponse) && nrow(noresponse) > 0L) {
    nr <- as.data.frame(lapply(noresponse[c("item_uid", "itemVer", "code", "label")],
                               as.character), stringsAsFactors = FALSE)
    use <- dv[dv$item_uid != "", c("datasetID", "item_uid", "itemVer")]
    use$scheme <- ds$missingScheme[match(use$datasetID, ds$datasetID)]
    use <- use[!is.na(use$scheme) & use$scheme != "", , drop = FALSE]
    # dataset_vars 沒寫版本時，該題所有版本都算
    m1 <- merge(use[use$itemVer != "", ], nr, by = c("item_uid", "itemVer"))
    m2 <- merge(use[use$itemVer == "", setdiff(names(use), "itemVer")], nr, by = "item_uid")
    m <- rbind(m1, m2[names(m1)])
    m <- m[!(paste(m$scheme, m$code) %in% paste(ms$scheme, ms$code)), , drop = FALSE]
    if (nrow(m)) {
      vs <- iv$valueSetID[match(paste(m$item_uid, m$itemVer), paste(iv$item_uid, iv$itemVer))]
      also <- paste(vs, m$code) %in% paste(vc$valueSetID, vc$code)
      # 原文：（也是選項代碼）
      m$txt <- paste0(m$label, ":", m$code, ifelse(also, "\uff08\u4e5f\u662f\u9078\u9805\u4ee3\u78bc\uff09", ""))
      k1 <- paste(m$item_uid, m$itemVer, m$datasetID, sep = "\r")
      # 原文：、
      pat <- tapply(m$txt, k1, function(z) paste(sort(unique(z)), collapse = "\u3001"))
      sch <- tapply(m$scheme, k1, `[`, 1L)
      p <- do.call(rbind, strsplit(names(pat), "\r", fixed = TRUE))
      d <- data.frame(item_uid = p[, 1], itemVer = p[, 2], datasetID = p[, 3],
                      pat = as.character(pat), scheme = as.character(sch),
                      stringsAsFactors = FALSE)
      k2 <- paste(d$item_uid, d$itemVer, d$pat, sep = "\r")
      for (kk in unique(k2)) {
        r <- d[k2 == kk, , drop = FALSE]
        add("missing_code_scheme", "item_versions", paste(r$item_uid[1], r$itemVer[1], sep = "|"),
            # 原文：0.2 題庫寫「 | 」，不在遺漏方案 
            paste0("0.2 \u984c\u5eab\u5beb\u300c", r$pat[1], "\u300d\uff0c\u4e0d\u5728\u907a\u6f0f\u65b9\u6848 ",
                   # 原文：、 | ：
                   paste(sort(unique(r$scheme)), collapse = "\u3001"), "\uff1a",
                   audit_kit_list(sort(r$datasetID), 6L)))
      }
    }
  }

  # ---- 5. 題組 -------------------------------------------------------------------
  parents <- unique(paste(gt$formID, gt$parentID, sep = "|")[gt$parentID != ""])
  fg <- paste(gm$formID, gm$groupID, sep = "|")
  dep <- items$deprecated[match(gm$item_uid, items$item_uid)] %in% "1"
  nonleaf <- fg %in% parents
  for (k in unique(fg[nonleaf])) {
    u <- gm$item_uid[fg == k]
    add("group_nonleaf_member", "group_members", k,
        # 原文：中間層題組直接掛了  |  題：
        paste0("\u4e2d\u9593\u5c64\u984c\u7d44\u76f4\u63a5\u639b\u4e86 ", length(u), " \u984c\uff1a", audit_kit_list(u)))
  }
  for (k in unique(fg[!nonleaf])) {
    r <- which(fg == k)
    u <- unique(gm$item_uid[r])
    if (all(dep[r])) {
      add("group_only_deprecated", "group_members", k,
          # 原文： 題都是停用的題：
          paste0(length(u), " \u984c\u90fd\u662f\u505c\u7528\u7684\u984c\uff1a", audit_kit_list(u)))
    } else if (length(u) == 1L) {
      # 原文：只有 1 題：
      add("group_single_item", "group_members", k, paste0("\u53ea\u6709 1 \u984c\uff1a", u))
    }
  }
  x <- setdiff(grp$groupID, c(gm$groupID, gt$parentID))
  # 原文：沒有任何題目，也沒有下層題組
  add("group_empty", "groups", x, rep("\u6c92\u6709\u4efb\u4f55\u984c\u76ee\uff0c\u4e5f\u6c92\u6709\u4e0b\u5c64\u984c\u7d44", length(x)))

  # ---- 6. 文字 -------------------------------------------------------------------
  ivk <- paste(iv$item_uid, iv$itemVer, sep = "|")
  out <- c(out,
           # 原文：題目文字（label）
           audit_kit_text(iv$label, ivk, "item_versions", "\u984c\u76ee\u6587\u5b57\uff08label\uff09"),
           # 題幹是從 label 切出來的開頭，常停在括號中間，不查括號
           # 原文：題幹（stem）
           audit_kit_text(iv$stem, ivk, "item_versions", "\u984c\u5e79\uff08stem\uff09", brackets = FALSE),
           audit_kit_text(vc$label, paste(vc$valueSetID, vc$code, sep = "|"), "value_codes",
                          # 原文：選項文字
                          "\u9078\u9805\u6587\u5b57"))

  # ---- 7. 內部施測 -----------------------------------------------------------------
  x <- ds$datasetID[ds$file == "v02"]
  if (length(x)) {
    add("internal_admins", "datasets", "v02",
        # 原文： 個資料檔（
        paste0(length(x), " \u500b\u8cc7\u6599\u6a94\uff08", length(unique(ds$adminID[ds$file == "v02"])),
               # 原文： 次施測）還沒有 .sav，題目只來自 0.2 題庫：
               " \u6b21\u65bd\u6e2c\uff09\u9084\u6c92\u6709 .sav\uff0c\u984c\u76ee\u53ea\u4f86\u81ea 0.2 \u984c\u5eab\uff1a", audit_kit_list(x, 4L)))
  }

  # ---- 8. 角色、模組空白 -----------------------------------------------------------
  x <- which(items$role == "" | items$module == "")
  # 原文：role 和 module 都空白
  what <- ifelse(items$role[x] == "" & items$module[x] == "", "role \u548c module \u90fd\u7a7a\u767d",
                 # 原文：role 空白 | module 空白
                 ifelse(items$role[x] == "", "role \u7a7a\u767d", "module \u7a7a\u767d"))
  add("role_module_blank", "items", items$item_uid[x], what)

  if (length(out) == 0L) {
    return(data.frame(level = character(0), check = character(0), table = character(0),
                      key = character(0), detail = character(0), stringsAsFactors = FALSE))
  }
  res <- do.call(rbind, out)
  res$level <- "note"
  res <- res[order(match(res$check, AUDIT_KIT_CHECKS$check), res$table, res$key,
                   method = "radix"), c("level", "check", "table", "key", "detail")]
  rownames(res) <- NULL
  res
}

#' Count kit audit findings per check
#'
#' @param res The result of [audit_kit()].
#' @param all Keep checks that found nothing (`n = 0`)?
#' @return A data frame with `check`, `label` (Traditional Chinese name) and `n`, in the order
#'   of `AUDIT_KIT_CHECKS`.
#' @examples
#' audit_kit_summary(audit_kit(empty_kit()), all = TRUE)
#' @export
audit_kit_summary <- function(res, all = FALSE) {
  n <- table(factor(res$check, levels = AUDIT_KIT_CHECKS$check))
  out <- data.frame(check = AUDIT_KIT_CHECKS$check, label = AUDIT_KIT_CHECKS$label,
                    n = as.integer(n), stringsAsFactors = FALSE)
  if (!all) out <- out[out$n > 0L, , drop = FALSE]
  rownames(out) <- NULL
  out
}
