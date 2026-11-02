# R/concept_info.R — 構面查詢的題組資訊：概念、三份問卷的版本、官方文件、文獻、題目 × 波次
#
#   dims_group_id()       構面查詢的 dimID（不含問卷代碼）→ kit 的 groupID
#   concept_info_ui()     概念、英文名稱、領域；家長評／教師評／兒童自評各有哪些題組；官方文件的寫法和信效度；文獻
#   dim_item_wave_ui()    這個題組的題目 × 波次（version_grid()）：每一格是那一波問的版本，M03、M36 分開；整頁寬

# 原文：家長評 | 教師評 | 兒童自評
INFORMANT_LABEL <- c(P = "\u5bb6\u9577\u8a55", T = "\u6559\u5e2b\u8a55", C = "\u5152\u7ae5\u81ea\u8a55")
# 原文：量表出處 | 中文版／修訂 | KIT 編製與驗證
REF_ROLE_LABEL <- c(source = "\u91cf\u8868\u51fa\u8655", adaptation = "\u4e2d\u6587\u7248\uff0f\u4fee\u8a02", kit_validation = "KIT \u7de8\u88fd\u8207\u9a57\u8b49",
                    # 原文：效標工具
                    criterion = "\u6548\u6a19\u5de5\u5177")
# 原文：已在 PubMed 核對 | 取自 KIT 文件（不在 PubMed）
REF_STATUS_LABEL <- c(verified = "\u5df2\u5728 PubMed \u6838\u5c0d", not_in_pubmed = "\u53d6\u81ea KIT \u6587\u4ef6\uff08\u4e0d\u5728 PubMed\uff09",
                      # 原文：可能的出處（未確認）
                      likely = "\u53ef\u80fd\u7684\u51fa\u8655\uff08\u672a\u78ba\u8a8d\uff09")

#' @noRd
dims_group_id <- function(domain, dim_id) {
  paste0(KIT_DOMAIN_INSTRUMENT[[domain]], ".", dim_id)
}

#' @noRd
concept_info_ui <- function(kit, group_id) {
  g <- kit$groups
  row <- g[g$groupID == group_id, , drop = FALSE]
  if (!nrow(row)) return(NULL)
  cc <- kit$concepts %||% data.frame()
  con <- if (nzchar(row$construct)) cc[cc$conceptID == row$construct, , drop = FALSE] else cc[0, ]
  gm <- kit$group_members
  forms_of <- function(id) {
    f <- sort(unique(gm$formID[gm$groupID == id]))
    if (!length(f)) f <- sort(unique(kit$group_tree$formID[kit$group_tree$groupID == id]))
    # 原文：、
    paste(f, collapse = "\u3001")
  }
  head_line <- NULL
  if (nrow(con)) {
    head_line <- shiny::p(
      # 原文：概念：
      shiny::strong("\u6982\u5ff5\uff1a"), con$label,
      # 原文：（ | ）
      if (nzchar(con$label_en)) paste0("\uff08", con$label_en, "\uff09"),
      # 原文：　領域：
      if (nzchar(con$domain)) paste0("\u3000\u9818\u57df\uff1a", con$domain)
    )
  }
  # 同一概念的其他問卷
  sib <- if (nrow(con)) g[g$construct == row$construct, , drop = FALSE] else row
  sib_ui <- NULL
  if (length(unique(substr(sib$groupID, 1, 1))) > 1L) {
    tab <- data.frame(
      # 原文：填答者
      "\u586b\u7b54\u8005" = INFORMANT_LABEL[substr(sib$groupID, 1, 1)],
      # 原文：題組 | （這一個）
      "\u984c\u7d44" = ifelse(sib$groupID == group_id, paste0(sib$label, "\uff08\u9019\u4e00\u500b\uff09"), sib$label),
      # 原文：題本
      "\u984c\u672c" = vapply(sib$groupID, forms_of, character(1)),
      # 原文：題數
      "\u984c\u6578" = vapply(sib$groupID, function(id) length(unique(gm$item_uid[gm$groupID == id])), integer(1)),
      check.names = FALSE, stringsAsFactors = FALSE)
    tab <- tab[order(match(substr(sib$groupID, 1, 1), c("P", "T", "C"))), , drop = FALSE]
    sib_ui <- shiny::tagList(
      # 原文：同一概念的家長、教師、兒童版本
      shiny::h5("\u540c\u4e00\u6982\u5ff5\u7684\u5bb6\u9577\u3001\u6559\u5e2b\u3001\u5152\u7ae5\u7248\u672c"),
      # 原文：不同填答者的分數分開計分，不能直接相加或比較；切換上方題庫可以看其他填答者的題目。
      shiny::p(class = "kit-hint", "\u4e0d\u540c\u586b\u7b54\u8005\u7684\u5206\u6578\u5206\u958b\u8a08\u5206\uff0c\u4e0d\u80fd\u76f4\u63a5\u76f8\u52a0\u6216\u6bd4\u8f03\uff1b\u5207\u63db\u4e0a\u65b9\u984c\u5eab\u53ef\u4ee5\u770b\u5176\u4ed6\u586b\u7b54\u8005\u7684\u984c\u76ee\u3002"),
      kitDesign::kit_table_scroll(kitDesign::kit_static_table(tab)))
  }
  # 官方《問卷構面及信效度》文件
  gd <- kit$group_docs %||% data.frame()
  gd <- if (nrow(gd)) gd[gd$groupID == group_id, , drop = FALSE] else gd
  doc_ui <- NULL
  if (nrow(gd)) {
    # 原文：3-24月齡 | 36-72月齡
    doc_name <- c("3-24" = "3-24\u6708\u9f61", "36-72" = "36-72\u6708\u9f61", "G1-G3" = "G1-G3")
    tab <- data.frame(
      # 原文：文件 | 問卷構面及信效度
      "\u6587\u4ef6" = paste0(unname(ifelse(gd$doc %in% names(doc_name), doc_name[gd$doc], gd$doc)), "\u554f\u5377\u69cb\u9762\u53ca\u4fe1\u6548\u5ea6"),
      # 原文：題本
      "\u984c\u672c" = gd$formID,
      # 原文：官方名稱 | （ | ）
      "\u5b98\u65b9\u540d\u7a31" = ifelse(nzchar(gd$official_en), paste0(gd$official_label, "\uff08", gd$official_en, "\uff09"), gd$official_label),
      # 原文：信效度
      "\u4fe1\u6548\u5ea6" = gd$reliability,
      # 原文：備註
      "\u5099\u8a3b" = gd$note,
      check.names = FALSE, stringsAsFactors = FALSE)
    # 原文：官方文件
    doc_ui <- shiny::tagList(shiny::h5("\u5b98\u65b9\u6587\u4ef6"), kitDesign::kit_table_scroll(kitDesign::kit_static_table(tab)))
  }
  # 文獻
  gr <- kit$group_refs %||% data.frame()
  gr <- if (nrow(gr)) gr[gr$groupID == group_id, , drop = FALSE] else gr
  ref_ui <- NULL
  if (nrow(gr)) {
    rf <- kit$refs[match(gr$refID, kit$refs$refID), , drop = FALSE]
    ord <- order(match(gr$role, names(REF_ROLE_LABEL)), rf$year)
    items <- lapply(ord, function(i) {
      link <- if (nzchar(rf$doi[i])) shiny::tags$a(href = paste0("https://doi.org/", rf$doi[i]), target = "_blank", rel = "noopener", "DOI")
              else if (nzchar(rf$pmid[i])) shiny::tags$a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/", rf$pmid[i], "/"), target = "_blank", rel = "noopener", "PubMed")
      shiny::tags$li(
        # 原文：：
        shiny::strong(paste0(REF_ROLE_LABEL[[gr$role[i]]] %||% gr$role[i], "\uff1a")), rf$citation[i], " ", link,
        # 原文：　
        shiny::span(class = "kit-hint", paste0("\u3000", REF_STATUS_LABEL[[rf$status[i]]] %||% "",
                                                # 原文：；
                                                if (nzchar(gr$evidence[i])) paste0("\uff1b", gr$evidence[i]) else "")))
    })
    # 原文：文獻
    ref_ui <- shiny::tagList(shiny::h5("\u6587\u737b"), shiny::tags$ul(items))
  }
  if (is.null(head_line) && is.null(sib_ui) && is.null(doc_ui) && is.null(ref_ui)) return(NULL)
  # 原文：題組資訊：概念、官方文件、文獻
  kitDesign::kit_detail("\u984c\u7d44\u8cc7\u8a0a\uff1a\u6982\u5ff5\u3001\u5b98\u65b9\u6587\u4ef6\u3001\u6587\u737b",
                        shiny::tagList(head_line, sib_ui, doc_ui, ref_ui), open = TRUE)
}

#' @noRd
dim_item_wave_ui <- function(bank, dim_id) {
  idim <- bank$itemDim
  if (!dim_id %in% names(idim)) return(NULL)
  keys <- unique(idim$itemID[idim[[dim_id]] == 1L])
  if (!length(keys)) return(NULL)
  co <- bank_cohorts(bank)
  tables <- lapply(if (length(co)) co else "", function(cn) {
    g <- version_grid(bank, keys, cohort = cn)
    if (!nrow(g)) return(NULL)
    g$block <- NULL
    g$label <- NULL
    # 原文： 世代（錨題  |  題）
    head <- if (nzchar(cn)) shiny::h5(paste0(cn, " \u4e16\u4ee3\uff08\u9328\u984c ", sum(g$anchor == "\u2605"), " \u984c\uff09"))
    shiny::tagList(head, kitDesign::kit_table_scroll(kitDesign::kit_static_table(version_grid_display(g))))
  })
  shiny::tagList(
    # 原文：題目 × 波次（縱貫比較用）
    shiny::h4("\u984c\u76ee \u00d7 \u6ce2\u6b21\uff08\u7e31\u8cab\u6bd4\u8f03\u7528\uff09"),
    # 原文：每一格是那一波問的版本（v1、v2…），空白是那一波沒問；版本換了就不能直接比。★ 錨題：這個世代每一波都問、而且版本相同，可以當縱貫比較的共同題。
    shiny::p(class = "kit-hint", "\u6bcf\u4e00\u683c\u662f\u90a3\u4e00\u6ce2\u554f\u7684\u7248\u672c\uff08v1\u3001v2\u2026\uff09\uff0c\u7a7a\u767d\u662f\u90a3\u4e00\u6ce2\u6c92\u554f\uff1b\u7248\u672c\u63db\u4e86\u5c31\u4e0d\u80fd\u76f4\u63a5\u6bd4\u3002\u2605 \u9328\u984c\uff1a\u9019\u500b\u4e16\u4ee3\u6bcf\u4e00\u6ce2\u90fd\u554f\u3001\u800c\u4e14\u7248\u672c\u76f8\u540c\uff0c\u53ef\u4ee5\u7576\u7e31\u8cab\u6bd4\u8f03\u7684\u5171\u540c\u984c\u3002"),
    tables)
}
