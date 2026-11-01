# R/import_v02.R — 把 0.2 的寬表題庫（三個 domain）轉成 0.3 kit 的長表（實作計畫步驟 1.1）
#
# import_v02() 只讀不寫，回傳一個 list：
#   schema 表：items、item_versions、value_codes、groups、sources
#   其他（給後面步驟用）：group_edges、group_members_v02、item_waves、noresponse、
#     noresponse_codes、versions_v02、group_meta_v02、pseudo_dims、item_roles、issues
#
# 規則（都可在下面的表裡查到）：
# - item_uid = 問卷前綴 + "." + itemID；前綴 parent → P、childcare_teacher → T、child_adolescent → C
# - stemID 在 0.2 存的是共用題幹「文字」（不是代碼），直接放進 item_versions$stem
# - value 文字一行一個選項（「1 完全不會;」「1:很少;」「0 ： 0個月;」）；照錄文字／照錄數字沒有代碼；
#   「1~5點量表」展開成 1..5、標籤空白（列為 issue，等 .sav 補標籤）
# - 相同的選項組（代碼 + 標籤 + 順序完全一樣）共用一個 valueSetID：vs0001、vs0002…，
#   照 parent → child_adolescent → childcare_teacher、itemBank 列序第一次出現的順序編
# - 反向題、專注題、社會期許（題）、sorting、選擇家長 是旗標不是構念：不做成題組
# - role／module 依 V02_ROLE_RULES 由 block（空白時用構面名稱，再不行用同字首的題）判斷

#' import_v02() 產生的 schema 表
#' @noRd
V02_KIT_TABLES <- c("items", "item_versions", "value_codes", "groups", "sources")

#' 0.2 的 domain → 問卷前綴
#' @noRd
V02_PREFIX <- c(parent = "P", child_adolescent = "C", childcare_teacher = "T")

#' 依 itemID 判斷 role／module（先於 block 規則）；domain 空白 = 三個題庫都適用
#' @noRd
V02_ID_RULES <- data.frame(
  rbind(
    c("^(ID|baby_id|int_date|interviewer|interviewer_id)$", "admin", "system", ""),
    # 保母、教保人員的聯絡資料（追蹤用）；問卷版本、下波訪問方式
    c("^(m|tp)(add|address|mobile|name|tel|class|kindergarten)", "admin", "system", "parent"),
    c("^(version|onlineorface)$", "admin", "system", "parent"),
    c("^sex$", "background", "demographic", "parent"),
    # 教師自己的基本資料（T4001–T4019）
    c("^T40", "background", "demographic", "childcare_teacher")
  ),
  stringsAsFactors = FALSE
)
names(V02_ID_RULES) <- c("pattern", "role", "module", "domain")

#' 照錄（沒有選項代碼）的 value 文字 → 預設題型
#' @noRd
# 原文：照錄文字 | 文字照錄 | 照錄 | 照錄數字
V02_TEXT_ENTRY <- c("\u7167\u9304\u6587\u5b57" = "CR", "\u6587\u5b57\u7167\u9304" = "CR", "\u7167\u9304" = "CR", "\u7167\u9304\u6578\u5b57" = "NR")

#' 不是構念的 0.2 構面：kind = reverse／attention／social_desirability／routing
#' @noRd
V02_PSEUDO_DIMS <- data.frame(
  # 原文：反向題 | 專注題 | 社會期許 | 社會期許題 | 選擇家長
  dimID = c("\u53cd\u5411\u984c", "\u5c08\u6ce8\u984c", "\u793e\u6703\u671f\u8a31", "\u793e\u6703\u671f\u8a31\u984c", "sorting", "\u9078\u64c7\u5bb6\u9577"),
  kind = c("reverse", "attention", "social_desirability", "social_desirability",
           "routing", "routing"),
  stringsAsFactors = FALSE
)

#' role／module 規則：依序比對，第一條符合的勝出。module 為 "@age" 時依波次決定
#' （都在 72 月齡以下 → childcare；都在 84 月齡以上或小／國 → school；混合 → 空白）。
#' domain 空白 = 三個題庫都適用。
#' @noRd
V02_ROLE_RULES <- data.frame(
  rbind(
    # 原文：專注題
    c("\u5c08\u6ce8\u984c", "admin", "system", ""),
    # 原文：目標幼兒身分確認|選擇家長|^sorting$
    c("\u76ee\u6a19\u5e7c\u5152\u8eab\u5206\u78ba\u8a8d|\u9078\u64c7\u5bb6\u9577|^sorting$", "admin", "system", ""),
    # 原文：孩子教保狀況|托育
    c("\u5b69\u5b50\u6559\u4fdd\u72c0\u6cc1|\u6258\u80b2", "background", "childcare", ""),
    # 原文：基本資料|受訪者資料|樣本
    c("\u57fa\u672c\u8cc7\u6599|\u53d7\u8a2a\u8005\u8cc7\u6599|\u6a23\u672c", "background", "demographic", ""),
    # 原文：健康|睡眠|運動習慣|飲食習慣|生長發育|物質使用|自殺|身心症
    c("\u5065\u5eb7|\u7761\u7720|\u904b\u52d5\u7fd2\u6163|\u98f2\u98df\u7fd2\u6163|\u751f\u9577\u767c\u80b2|\u7269\u8cea\u4f7f\u7528|\u81ea\u6bba|\u8eab\u5fc3\u75c7", "measure", "health", ""),
    # 原文：師生|家園|親師|教學|管教|活動、課程|學習環境|教養
    c("\u5e2b\u751f|\u5bb6\u5712|\u89aa\u5e2b|\u6559\u5b78|\u7ba1\u6559|\u6d3b\u52d5\u3001\u8ab2\u7a0b|\u5b78\u7fd2\u74b0\u5883|\u6559\u990a", "measure", "@age", "childcare_teacher"),
    # 原文：親師
    c("\u89aa\u5e2b", "measure", "@age", ""),
    # 原文：家庭|教養|親職|親子|手足|共同教養|共親職|母職|父職|[Hh][Oo][Mm][Ee]|兼容並蓄|對己有利|規避逃離|順從退讓|鼓勵成熟|嗜好培養|學習材料|學習輔助|學習刺激|語言刺激|語文環境|家長網路|網路成癮|網路設備|成就期待|婚姻|偏心|3C|電子產品|才藝|父母參與|父母回應|條件式|接受幼兒|環境多樣|心理控制|縱容|華人特質|恥感|^管$|^教$|女方|男方|親友|依附|圖書|物理環境|生活常規|生活訓練|行為監督|經濟條件|家庭支持
    c("\u5bb6\u5ead|\u6559\u990a|\u89aa\u8077|\u89aa\u5b50|\u624b\u8db3|\u5171\u540c\u6559\u990a|\u5171\u89aa\u8077|\u6bcd\u8077|\u7236\u8077|[Hh][Oo][Mm][Ee]|\u517c\u5bb9\u4e26\u84c4|\u5c0d\u5df1\u6709\u5229|\u898f\u907f\u9003\u96e2|\u9806\u5f9e\u9000\u8b93|\u9f13\u52f5\u6210\u719f|\u55dc\u597d\u57f9\u990a|\u5b78\u7fd2\u6750\u6599|\u5b78\u7fd2\u8f14\u52a9|\u5b78\u7fd2\u523a\u6fc0|\u8a9e\u8a00\u523a\u6fc0|\u8a9e\u6587\u74b0\u5883|\u5bb6\u9577\u7db2\u8def|\u7db2\u8def\u6210\u766e|\u7db2\u8def\u8a2d\u5099|\u6210\u5c31\u671f\u5f85|\u5a5a\u59fb|\u504f\u5fc3|3C|\u96fb\u5b50\u7522\u54c1|\u624d\u85dd|\u7236\u6bcd\u53c3\u8207|\u7236\u6bcd\u56de\u61c9|\u689d\u4ef6\u5f0f|\u63a5\u53d7\u5e7c\u5152|\u74b0\u5883\u591a\u6a23|\u5fc3\u7406\u63a7\u5236|\u7e31\u5bb9|\u83ef\u4eba\u7279\u8cea|\u6065\u611f|^\u7ba1$|^\u6559$|\u5973\u65b9|\u7537\u65b9|\u89aa\u53cb|\u4f9d\u9644|\u5716\u66f8|\u7269\u7406\u74b0\u5883|\u751f\u6d3b\u5e38\u898f|\u751f\u6d3b\u8a13\u7df4|\u884c\u70ba\u76e3\u7763|\u7d93\u6fdf\u689d\u4ef6|\u5bb6\u5ead\u652f\u6301", "measure", "family", ""),
    # 原文：發展|身體|動作|認知|語言|社會情緒|抑制力|工作記憶|計畫力|調節力|衝動|情緒|利社會|問題行為|心理適應|幸福感|同儕|合群|主動性|獨立性|順從性|外放性|奮力控制|注意力|記憶|視動|抓握|手部|穩定度|協調|讀寫|^聽$|^說$|^讀$|^寫$|退縮|攻擊|焦慮|憂鬱|違規|同理|自我控制|遵守規範|害羞|社會適應|社會能力|自尊|生活滿意|身體意象
    c("\u767c\u5c55|\u8eab\u9ad4|\u52d5\u4f5c|\u8a8d\u77e5|\u8a9e\u8a00|\u793e\u6703\u60c5\u7dd2|\u6291\u5236\u529b|\u5de5\u4f5c\u8a18\u61b6|\u8a08\u756b\u529b|\u8abf\u7bc0\u529b|\u885d\u52d5|\u60c5\u7dd2|\u5229\u793e\u6703|\u554f\u984c\u884c\u70ba|\u5fc3\u7406\u9069\u61c9|\u5e78\u798f\u611f|\u540c\u5115|\u5408\u7fa4|\u4e3b\u52d5\u6027|\u7368\u7acb\u6027|\u9806\u5f9e\u6027|\u5916\u653e\u6027|\u596e\u529b\u63a7\u5236|\u6ce8\u610f\u529b|\u8a18\u61b6|\u8996\u52d5|\u6293\u63e1|\u624b\u90e8|\u7a69\u5b9a\u5ea6|\u5354\u8abf|\u8b80\u5beb|^\u807d$|^\u8aaa$|^\u8b80$|^\u5beb$|\u9000\u7e2e|\u653b\u64ca|\u7126\u616e|\u6182\u9b31|\u9055\u898f|\u540c\u7406|\u81ea\u6211\u63a7\u5236|\u9075\u5b88\u898f\u7bc4|\u5bb3\u7f9e|\u793e\u6703\u9069\u61c9|\u793e\u6703\u80fd\u529b|\u81ea\u5c0a|\u751f\u6d3b\u6eff\u610f|\u8eab\u9ad4\u610f\u8c61", "measure", "development", ""),
    # 原文：學校|學習情形|學習態度|上下學|英語文|語文
    c("\u5b78\u6821|\u5b78\u7fd2\u60c5\u5f62|\u5b78\u7fd2\u614b\u5ea6|\u4e0a\u4e0b\u5b78|\u82f1\u8a9e\u6587|\u8a9e\u6587", "measure", "school", "")
  ),
  stringsAsFactors = FALSE
)
names(V02_ROLE_RULES) <- c("pattern", "role", "module", "domain")

#' 構面名稱裡「_」前面的上層題組名稱對照（題庫沒有這個名稱、但很清楚是哪一個）
#' @noRd
# 原文：精細 | 精細動作總分 | 粗大 | 粗大動作總分
V02_PARENT_ALIAS <- c("\u7cbe\u7d30" = "\u7cbe\u7d30\u52d5\u4f5c\u7e3d\u5206", "\u7c97\u5927" = "\u7c97\u5927\u52d5\u4f5c\u7e3d\u5206")

# 解析 ----

#' 解析 0.2 的 value 文字
#'
#' @return list(kind, codes, bad)：kind 是 "codes"、"range"、"text"、"blank"、"bad"；
#'   codes 是 data.frame(code, label)；bad 是解析不了的行。
#' @noRd
v02_parse_value <- function(txt) {
  empty <- data.frame(code = character(0), label = character(0), stringsAsFactors = FALSE)
  # 原文：[;；]\\s*$
  t0 <- trimws(gsub("[;\uff1b]\\s*$", "", trimws(txt %||% "")))
  if (is.na(t0) || !nzchar(t0)) return(list(kind = "blank", codes = empty, bad = character(0)))
  if (t0 %in% names(V02_TEXT_ENTRY)) return(list(kind = "text", codes = empty, bad = character(0)))
  # 「1~5點量表」「0~9」：展開成代碼，標籤空白
  # 原文：^(-?[0-9]+)\\s*[~～]\\s*(-?[0-9]+)\\s*(點量表)?$
  rg <- regmatches(t0, regexec("^(-?[0-9]+)\\s*[~\uff5e]\\s*(-?[0-9]+)\\s*(\u9ede\u91cf\u8868)?$", t0))[[1]]
  if (length(rg)) {
    a <- as.integer(rg[2]); b <- as.integer(rg[3])
    if (b >= a && b - a <= 20L) {
      cc <- as.character(seq(a, b))
      return(list(kind = "range", codes = data.frame(code = cc, label = rep("", length(cc)),
                                                     stringsAsFactors = FALSE),
                  bad = character(0)))
    }
  }
  # 只用半形分號和換行分行：全形分號會出現在標籤裡（「…熟練；有時會」）
  # 原文：[；]\\s*$
  parts <- trimws(gsub("[\uff1b]\\s*$", "", trimws(unlist(strsplit(txt, "[;\r\n]+")))))
  parts <- parts[nzchar(parts)]
  # 原文：^(-?[0-9]+(\\.[0-9]+)?)(\\s*[:：]\\s*|\\s+)(\\S.*)$
  pat <- "^(-?[0-9]+(\\.[0-9]+)?)(\\s*[:\uff1a]\\s*|\\s+)(\\S.*)$"
  ok <- grepl(pat, parts, perl = TRUE)
  if (!all(ok)) return(list(kind = "bad", codes = empty, bad = parts[!ok]))
  codes <- data.frame(code = sub(pat, "\\1", parts, perl = TRUE),
                      label = trimws(sub(pat, "\\4", parts, perl = TRUE)),
                      stringsAsFactors = FALSE)
  if (anyDuplicated(codes$code)) {
    return(list(kind = "bad", codes = empty,
                bad = paste("duplicate code", unique(codes$code[duplicated(codes$code)]))))
  }
  list(kind = "codes", codes = codes, bad = character(0))
}

#' 解析 noResponse 文字（「不知道:8888; 拒答:7777」）→ data.frame(label, code, ok)
#' @noRd
v02_parse_noresponse <- function(txt) {
  # 原文：[;；\n]+
  parts <- trimws(unlist(strsplit(txt, "[;\uff1b\n]+")))
  parts <- parts[nzchar(parts)]
  # 原文：^(.*?)\\s*[:：]\\s*(-?[0-9]+)$
  pat <- "^(.*?)\\s*[:\uff1a]\\s*(-?[0-9]+)$"
  ok <- grepl(pat, parts, perl = TRUE)
  data.frame(label = ifelse(ok, trimws(sub(pat, "\\1", parts, perl = TRUE)), ""),
             code = ifelse(ok, sub(pat, "\\2", parts, perl = TRUE), ""),
             token = parts, ok = ok, stringsAsFactors = FALSE)
}

# role／module ----

#' 一組文字各自套規則 → data.frame(role, module)（沒有符合的是 NA）
#' @noRd
v02_match_rule <- function(x, domain) {
  rules <- V02_ROLE_RULES[!nzchar(V02_ROLE_RULES$domain) | V02_ROLE_RULES$domain == domain, ,
                          drop = FALSE]
  idx <- vapply(x, function(s) {
    if (!nzchar(s)) return(NA_integer_)
    hit <- which(vapply(rules$pattern, grepl, logical(1), x = s, perl = TRUE))
    if (length(hit)) hit[1] else NA_integer_
  }, integer(1), USE.NAMES = FALSE)
  data.frame(role = rules$role[idx], module = rules$module[idx], stringsAsFactors = FALSE)
}

#' 從波次欄名判斷學前（childcare）或學齡（school）
#' @noRd
v02_age_module <- function(wave_cols) {
  if (!length(wave_cols)) return("")
  month <- suppressWarnings(as.integer(sub("^M([0-9]+).*$", "\\1", wave_cols)))
  # 原文：^(小|國)
  school <- grepl("^(\u5c0f|\u570b)", wave_cols) | (!is.na(month) & month >= 84L)
  pre <- !school & !is.na(month) & month <= 72L
  if (all(school)) return("school")
  if (all(pre)) return("childcare")
  ""
}

#' 題號字首（同字首的題通常在同一區塊），由長到短：字母部分（只有一個字母時取前 3 字，
#' T11025 → T11）、前 4 字
#' @noRd
v02_id_prefixes <- function(id) {
  p <- sub("^([A-Za-z_]*).*$", "\\1", id)
  if (nchar(p) <= 1L) p <- substr(id, 1L, 3L)
  unique(c(p, substr(id, 1L, 4L)))
}

# 主函式 ----

#' Import the 0.2 item bank into 0.3 kit tables
#'
#' Reads the three 0.2 banks (`parent`, `child_adolescent`, `childcare_teacher`) and converts
#' the wide format into the long 0.3 tables. Nothing is written to disk.
#'
#' @param dir Root folder of the 0.2 banks (default: the one shipped with the package).
#' @return A named list. Schema tables: `items`, `item_versions`, `value_codes`, `groups`,
#'   `sources`. Extra tables for later import steps: `group_edges` (groupID, parentID),
#'   `group_members_v02` (groupID, item_uid, reverse), `item_waves` (item_uid, itemVer,
#'   domain, wave_col), `noresponse` (raw text per version), `noresponse_codes` (parsed),
#'   `versions_v02` (per-version 0.2 fields with no 0.3 column), `group_meta_v02` (dimMeta
#'   reliability and note), `pseudo_dims`, `item_roles` (how role/module were decided) and
#'   `issues` (item_uid, itemVer, check, detail).
#' @examples
#' \dontrun{
#' res <- import_v02()
#' tbs <- c("items", "item_versions", "value_codes", "groups", "sources")
#' check_kit(replace(empty_kit(), tbs, res[tbs]))
#' }
#' @export
import_v02 <- function(dir = bank_dir()) {
  doms <- names(V02_PREFIX)
  banks <- stats::setNames(lapply(doms, load_bank, dir = dir), doms)

  issues <- list()
  add_issue <- function(uid, ver, check, detail) {
    if (length(uid)) issues[[length(issues) + 1L]] <<- data.frame(
      item_uid = uid, itemVer = as.character(ver), check = check, detail = detail,
      stringsAsFactors = FALSE)
  }

  # 每個版本一列（三個題庫接起來）
  vers <- do.call(rbind, lapply(doms, function(d) {
    ib <- banks[[d]]$itemBank
    data.frame(domain = d, prefix = V02_PREFIX[[d]], itemID = ib$itemID,
               item_uid = paste0(V02_PREFIX[[d]], ".", ib$itemID),
               itemVer = as.integer(ib$itemVer), deprecated = ib$deprecated,
               stemID = ib$stemID, block = ib$block, itemType = ib$itemType,
               respondent = ib$respondent, reverseScored = ib$reverseScored,
               label = ib$label, value = ib$value, noResponse = ib$noResponse,
               note = ib$note, stringsAsFactors = FALSE)
  }))
  rownames(vers) <- NULL
  vkey <- paste(vers$item_uid, vers$itemVer, sep = "|")

  # 波次 ----
  item_waves <- do.call(rbind, lapply(doms, function(d) {
    iw <- banks[[d]]$itemWave
    cols <- get_matrix_cols(iw)
    do.call(rbind, lapply(cols, function(cl) {
      k <- iw[[cl]] == 1L
      data.frame(item_uid = paste0(V02_PREFIX[[d]], ".", iw$itemID[k]),
                 itemVer = as.character(iw$itemVer[k]), domain = rep(d, sum(k)),
                 wave_col = rep(cl, sum(k)), stringsAsFactors = FALSE)
    }))
  }))
  item_waves <- item_waves[order(item_waves$domain, item_waves$item_uid,
                                 as.integer(item_waves$itemVer)), , drop = FALSE]
  rownames(item_waves) <- NULL

  # 選項 ----
  parsed <- lapply(vers$value, v02_parse_value)
  kind <- vapply(parsed, `[[`, character(1), "kind")
  set_key <- vapply(parsed, function(p) {
    if (!nrow(p$codes)) return("")
    paste(p$codes$code, p$codes$label, sep = "\t", collapse = "\n")
  }, character(1))
  uniq <- unique(set_key[nzchar(set_key)])
  set_id <- sprintf("vs%04d", seq_along(uniq))
  vers$valueSetID <- ifelse(nzchar(set_key), set_id[match(set_key, uniq)], "")
  first <- match(uniq, set_key)
  value_codes <- do.call(rbind, lapply(seq_along(uniq), function(i) {
    cc <- parsed[[first[i]]]$codes
    data.frame(valueSetID = set_id[i], code = cc$code, label = cc$label,
               order = as.character(seq_len(nrow(cc))), stringsAsFactors = FALSE)
  }))
  # 題型：0.2 有填就照用；空白時依 value 補（照錄 → CR／NR，有代碼 → MC）
  fill_type <- !nzchar(vers$itemType)
  # 原文：[;；]\\s*$
  txt_type <- unname(V02_TEXT_ENTRY[trimws(gsub("[;\uff1b]\\s*$", "", vers$value))])
  vers$itemType_filled <- vers$itemType
  vers$itemType_filled[fill_type & kind == "text"] <- txt_type[fill_type & kind == "text"]
  vers$itemType_filled[fill_type & kind %in% c("codes", "range")] <- "MC"
  bad <- which(kind == "bad")
  add_issue(vers$item_uid[bad], vers$itemVer[bad], "value_unparsed",
            vapply(parsed[bad], function(p) paste(p$bad, collapse = " / "), character(1)))
  rng <- which(kind == "range")
  add_issue(vers$item_uid[rng], vers$itemVer[rng], "value_no_labels",
            paste("expanded from", trimws(vers$value[rng])))
  blk <- which(kind == "blank")
  add_issue(vers$item_uid[blk], vers$itemVer[blk], "value_blank", "value text is empty")
  add_issue(vers$item_uid[fill_type], vers$itemVer[fill_type], "itemtype_filled",
            paste0("0.2 itemType blank; set to '", vers$itemType_filled[fill_type], "'"))

  item_versions <- conform_kit_table(data.frame(
    item_uid = vers$item_uid, itemVer = vers$itemVer, label = vers$label, stem = vers$stemID,
    valueSetID = vers$valueSetID, itemType = vers$itemType_filled, note = vers$note,
    stringsAsFactors = FALSE), "item_versions")

  # 構面（0.2 itemDim）→ 長表 ----
  dims_long <- do.call(rbind, lapply(doms, function(d) {
    idm <- banks[[d]]$itemDim
    cols <- get_matrix_cols(idm)
    do.call(rbind, lapply(cols, function(cl) {
      k <- idm[[cl]] == 1L
      data.frame(domain = rep(d, sum(k)), dimID = rep(cl, sum(k)),
                 item_uid = paste0(V02_PREFIX[[d]], ".", idm$itemID[k]),
                 itemVer = idm$itemVer[k], stringsAsFactors = FALSE)
    }))
  }))
  pseudo_kind <- V02_PSEUDO_DIMS$kind[match(dims_long$dimID, V02_PSEUDO_DIMS$dimID)]

  # 反向：reverseScored = 1、在「反向題」構面、或 block 寫著反向
  rev_uid <- unique(c(vers$item_uid[vers$reverseScored == 1L],
                      dims_long$item_uid[pseudo_kind %in% "reverse"],
                      # 原文：反向
                      vers$item_uid[grepl("\u53cd\u5411", vers$block)]))

  # 題號 ----
  uids <- unique(vers$item_uid)
  # block：取最新、有填的版本；各版本不同時列為 issue
  blocks_by_uid <- split(vers$block[order(vers$itemVer)], vers$item_uid[order(vers$itemVer)])
  item_block <- vapply(blocks_by_uid[uids], function(b) {
    b <- b[nzchar(b)]
    if (length(b)) b[length(b)] else ""
  }, character(1))
  multi <- vapply(blocks_by_uid[uids], function(b) length(unique(b[nzchar(b)])) > 1L, logical(1))
  add_issue(uids[multi], rep("", sum(multi)), "block_differs",
            vapply(blocks_by_uid[uids][multi], function(b) paste(unique(b[nzchar(b)]),
                                                                  collapse = " / "), character(1)))
  all_dep <- vapply(split(vers$deprecated, vers$item_uid)[uids], function(x) all(x == 1L),
                    logical(1))
  dom_of <- vers$domain[match(uids, vers$item_uid)]
  id_of <- vers$itemID[match(uids, vers$item_uid)]

  # role／module：itemID → block → 構面名稱 → 同字首的題
  rr <- data.frame(item_uid = uids, role = NA_character_, module = NA_character_,
                   basis = "", stringsAsFactors = FALSE)
  for (j in seq_len(nrow(V02_ID_RULES))) {
    ru <- V02_ID_RULES[j, ]
    k <- is.na(rr$role) & grepl(ru$pattern, id_of, perl = TRUE) &
      (!nzchar(ru$domain) | dom_of == ru$domain)
    rr$role[k] <- ru$role; rr$module[k] <- ru$module; rr$basis[k] <- "id"
  }
  sys <- rr$basis == "id"
  for (d in doms) {
    k <- which(dom_of == d & !sys & nzchar(item_block))
    m <- v02_match_rule(gsub("\n", " ", item_block[k]), d)
    rr$role[k] <- m$role; rr$module[k] <- m$module
    rr$basis[k[!is.na(m$role)]] <- "block"
  }
  dims_by_uid <- split(dims_long$dimID, dims_long$item_uid)
  for (i in which(is.na(rr$role))) {
    dd <- unique(dims_by_uid[[uids[i]]])
    if (!length(dd)) next
    m <- unique(stats::na.omit(v02_match_rule(dd, dom_of[i])))
    if (nrow(m) == 1L) {
      rr$role[i] <- m$role; rr$module[i] <- m$module; rr$basis[i] <- "dims"
    }
  }
  # 同字首的題：至少 3 題已判定、而且 80% 以上一致
  known <- !is.na(rr$role) & rr$basis %in% c("block", "dims")
  for (i in which(is.na(rr$role))) {
    for (p in v02_id_prefixes(id_of[i])) {
      same <- which(known & dom_of == dom_of[i] & startsWith(id_of, p))
      if (length(same) < 3L) next
      tb <- sort(table(paste(rr$role[same], rr$module[same], sep = "|")), decreasing = TRUE)
      if (tb[[1]] / length(same) >= 0.8) {
        rm <- strsplit(names(tb)[1], "|", fixed = TRUE)[[1]]
        rr$role[i] <- rm[1]; rr$module[i] <- rm[2]; rr$basis[i] <- "prefix"
      }
      break
    }
  }
  waves_by_uid <- split(item_waves$wave_col, item_waves$item_uid)
  age <- which(rr$module %in% "@age")
  rr$module[age] <- vapply(uids[age], function(u) v02_age_module(unique(waves_by_uid[[u]])),
                           character(1))
  rr$role[is.na(rr$role)] <- ""
  rr$module[is.na(rr$module)] <- ""

  items <- conform_kit_table(data.frame(
    item_uid = uids, instrument = V02_PREFIX[dom_of], varname = id_of, block = item_block,
    role = rr$role, module = rr$module, source_type = "questionnaire",
    deprecated = ifelse(all_dep, "1", "0"), stringsAsFactors = FALSE), "items")

  # 版本號不連號
  gap <- vapply(split(vers$itemVer, vers$item_uid)[uids], function(v) {
    !identical(sort(v), seq_along(v))
  }, logical(1))
  add_issue(uids[gap], rep("", sum(gap)), "itemver_gap",
            vapply(split(vers$itemVer, vers$item_uid)[uids][gap],
                   function(v) paste(sort(v), collapse = ","), character(1)))

  # 題組 ----
  gm <- do.call(rbind, lapply(doms, function(d) {
    dm <- banks[[d]]$dimMeta
    ids <- unique(c(dm$dimID, get_matrix_cols(banks[[d]]$itemDim)))
    j <- match(ids, dm$dimID)
    data.frame(domain = d, dimID = ids, label = ifelse(is.na(j), ids, dm$dimName[j]),
               alias = ifelse(is.na(j), "", dm$alias[j]),
               reliability = ifelse(is.na(j), "", dm$reliability[j]),
               note = ifelse(is.na(j), "", dm$note[j]), in_meta = !is.na(j),
               stringsAsFactors = FALSE)
  }))
  gm$groupID <- paste0(V02_PREFIX[gm$domain], ".", gm$dimID)
  pseudo <- gm$dimID %in% V02_PSEUDO_DIMS$dimID
  pseudo_dims <- data.frame(
    domain = gm$domain[pseudo], dimID = gm$dimID[pseudo],
    kind = V02_PSEUDO_DIMS$kind[match(gm$dimID[pseudo], V02_PSEUDO_DIMS$dimID)],
    n_items = vapply(which(pseudo), function(i) {
      length(unique(dims_long$item_uid[dims_long$domain == gm$domain[i] &
                                         dims_long$dimID == gm$dimID[i]]))
    }, integer(1)), stringsAsFactors = FALSE)
  gm <- gm[!pseudo, , drop = FALSE]
  # 原文：複合|上位量表\\(非
  gm$composite <- grepl("\u8907\u5408|\u4e0a\u4f4d\u91cf\u8868\\(\u975e", gm$note)

  # 上下層：「A_B」→ A；A 不在題庫時，兩個以上的子題組共用才新增（問卷資料這類區塊名稱除外）
  edges <- list(); created <- character(0)
  for (d in doms) {
    g <- gm[gm$domain == d, , drop = FALSE]
    has_us <- grepl("_", g$dimID)
    par <- sub("_[^_]*$", "", g$dimID[has_us])
    par <- ifelse(par %in% names(V02_PARENT_ALIAS), V02_PARENT_ALIAS[par], par)
    kids <- g$dimID[has_us]
    n_kids <- table(par)
    keep <- par %in% g$dimID |
      # 原文：問卷資料
      (n_kids[par] >= 2L & !grepl("\u554f\u5377\u8cc7\u6599", par))
    new_par <- setdiff(unique(par[keep]), g$dimID)
    created <- c(created, paste0(V02_PREFIX[[d]], ".", new_par))
    if (length(new_par)) {
      gm <- rbind(gm, data.frame(domain = d, dimID = new_par, label = new_par, alias = "",
                                 reliability = "", note = "", in_meta = FALSE,
                                 groupID = paste0(V02_PREFIX[[d]], ".", new_par),
                                 composite = FALSE, stringsAsFactors = FALSE))
    }
    if (any(keep)) edges[[length(edges) + 1L]] <- data.frame(
      groupID = paste0(V02_PREFIX[[d]], ".", kids[keep]),
      parentID = paste0(V02_PREFIX[[d]], ".", par[keep]), basis = "name",
      stringsAsFactors = FALSE)
    # dimMeta 備註「採上位量表'X'整體值」
    # 原文：上位量表'([^']+)'
    up <- regmatches(g$note, regexec("\u4e0a\u4f4d\u91cf\u8868'([^']+)'", g$note))
    up_par <- vapply(up, function(m) if (length(m)) m[2] else "", character(1))
    k <- nzchar(up_par) & up_par %in% g$dimID
    if (any(k)) edges[[length(edges) + 1L]] <- data.frame(
      groupID = paste0(V02_PREFIX[[d]], ".", g$dimID[k]),
      parentID = paste0(V02_PREFIX[[d]], ".", up_par[k]), basis = "dimMeta_note",
      stringsAsFactors = FALSE)
  }
  group_edges <- if (length(edges)) unique(do.call(rbind, edges)) else
    data.frame(groupID = character(0), parentID = character(0), basis = character(0))
  rownames(group_edges) <- NULL

  groups <- conform_kit_table(data.frame(
    groupID = gm$groupID, label = gm$label,
    type = ifelse(gm$composite | gm$groupID %in% created, "composite", ""),
    alias = gm$alias, stringsAsFactors = FALSE), "groups")
  group_meta_v02 <- data.frame(groupID = gm$groupID, domain = gm$domain, dimID = gm$dimID,
                               reliability = gm$reliability, note = gm$note,
                               in_dimMeta = gm$in_meta,
                               created = gm$groupID %in% created, stringsAsFactors = FALSE)

  # 成員：任一版本標 1 就是成員（以題號為單位）
  real <- is.na(pseudo_kind)
  gmem <- unique(data.frame(groupID = paste0(V02_PREFIX[dims_long$domain[real]], ".",
                                             dims_long$dimID[real]),
                            item_uid = dims_long$item_uid[real], stringsAsFactors = FALSE))
  gmem$reverse <- ifelse(gmem$item_uid %in% rev_uid, "1", "0")
  gmem <- gmem[order(gmem$groupID, gmem$item_uid), , drop = FALSE]
  rownames(gmem) <- NULL
  rev_alone <- setdiff(rev_uid, gmem$item_uid)
  add_issue(rev_alone, rep("", length(rev_alone)), "reverse_no_group",
            "reverse-scored item belongs to no group")

  # 遺漏代碼（只抽出，不判斷）----
  hasnr <- nzchar(trimws(vers$noResponse))
  noresponse <- data.frame(item_uid = vers$item_uid[hasnr],
                           itemVer = as.character(vers$itemVer[hasnr]),
                           noResponse = vers$noResponse[hasnr], stringsAsFactors = FALSE)
  nrc <- lapply(which(hasnr), function(i) {
    p <- v02_parse_noresponse(vers$noResponse[i])
    if (!nrow(p)) return(NULL)
    cbind(data.frame(item_uid = vers$item_uid[i], itemVer = as.character(vers$itemVer[i]),
                     stringsAsFactors = FALSE), p)
  })
  noresponse_codes <- do.call(rbind, nrc)
  nbad <- noresponse_codes[!noresponse_codes$ok, , drop = FALSE]
  add_issue(nbad$item_uid, nbad$itemVer, "noresponse_unparsed", nbad$token)
  noresponse_codes <- noresponse_codes[noresponse_codes$ok, c("item_uid", "itemVer", "code",
                                                              "label")]
  rownames(noresponse_codes) <- NULL

  versions_v02 <- data.frame(item_uid = vers$item_uid, itemVer = as.character(vers$itemVer),
                             domain = vers$domain, deprecated = as.character(vers$deprecated),
                             respondent = vers$respondent,
                             reverseScored = as.character(vers$reverseScored),
                             block = vers$block, itemType_v02 = vers$itemType,
                             stringsAsFactors = FALSE)

  ver_file <- file.path(dir, "BANK_VERSION")
  version <- if (file.exists(ver_file)) {
    trimws(readLines(ver_file, n = 1L, warn = FALSE, encoding = "UTF-8"))
  } else ""
  sources <- conform_kit_table(data.frame(
    sourceID = "v02_bank", kind = "v02_bank", title = "kitItemBank 0.2 item bank",
    version = version,
    date = if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", version)) version else "",
    location = "inst/extdata/bank", stringsAsFactors = FALSE), "sources")

  iss <- if (length(issues)) do.call(rbind, issues) else
    data.frame(item_uid = character(0), itemVer = character(0), check = character(0),
               detail = character(0))
  rownames(iss) <- NULL

  list(
    items = items, item_versions = item_versions,
    value_codes = conform_kit_table(value_codes, "value_codes"),
    groups = groups, sources = sources,
    group_edges = group_edges, group_members_v02 = gmem, item_waves = item_waves,
    noresponse = noresponse, noresponse_codes = noresponse_codes,
    versions_v02 = versions_v02, group_meta_v02 = group_meta_v02,
    pseudo_dims = pseudo_dims, item_roles = rr,
    role_rules = rbind(cbind(on = "itemID", V02_ID_RULES),
                       cbind(on = "block_or_dim", V02_ROLE_RULES)),
    issues = iss
  )
}
