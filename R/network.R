# R/network.R — 構面查詢的網路圖：一個構面 → 它的題目（→ 這些題目也屬於的其他構面）
#
# dim_network()        算節點與連線（沒有 Shiny，可以測）；同一 itemID 的版本是同一個節點
# dim_network_widget() 畫成 visNetwork，KIT 的樣子全用 visNetwork 的選項（不寫 CSS）
#
# 顏色：構面節點 nb-lime 底、ink 2px 框（方框）；題目白底 ink 框（圓點）；
# 屬於不只一個構面的題目 nb-yellow（圖下面的文字圖例也會寫，不只靠顏色）；線 line。

NET_INK <- "#141414"
NET_YELLOW <- "#ffde59"
NET_LINE <- "#d9d4c5"
NET_FONT <- "Chiron GoRound TC"
NET_MAX_ITEMS <- 150L

net_lime <- function() kitDesign::kit_colors()$accent

net_item_id <- function(item_id, item_ver) paste0("I::", item_id, "::", as.integer(item_ver))
net_dim_id <- function(dim_id) paste0("D::", dim_id)

#' 網路圖節點 id 拆回來：list(kind = "item"/"dim", itemID, itemVer, dimID)
#' @noRd
net_parse_id <- function(node_id) {
  node_id <- as.character(node_id)
  if (startsWith(node_id, "I::")) {
    parts <- strsplit(sub("^I::", "", node_id), "::", fixed = TRUE)[[1L]]
    n <- length(parts)
    return(list(kind = "item", itemID = paste(parts[-n], collapse = "::"),
                itemVer = as.integer(parts[n])))
  }
  if (startsWith(node_id, "D::")) return(list(kind = "dim", dimID = sub("^D::", "", node_id)))
  NULL
}

net_esc <- function(x) htmltools::htmlEscape(as.character(x))

#' 一個構面的網路圖資料
#'
#' 同一個 itemID 的版本合成一個節點（標籤 = itemID，滑過去列出每個版本和題目）；
#' 節點 id 用這個構面裡最新的版本，點一下就到那一版。
#'
#' @param bank 題庫（load_bank()）
#' @param dim_id 構面
#' @param other_dims TRUE：也畫這些題目（任一版本）所屬的其他構面
#' @param max_items 最多畫幾題（itemID；其餘不畫，n_omitted 告訴你幾題）
#' @return list(nodes, edges, n_items, n_shown, n_omitted, n_multi, n_other)
#' @noRd
dim_network <- function(bank, dim_id, other_dims = FALSE, max_items = NET_MAX_ITEMS) {
  dims_check_dim(bank, dim_id)
  id <- bank$itemDim
  ib <- bank$itemBank
  dims <- get_matrix_cols(id)
  rows <- which(id[[dim_id]] == 1L)
  rows <- rows[order(id$itemID[rows], id$itemVer[rows])]
  all_ids <- unique(id$itemID[rows])
  n_items <- length(all_ids)
  item_ids <- all_ids[seq_len(min(n_items, max_items))]
  n_shown <- length(item_ids)
  rows <- rows[id$itemID[rows] %in% item_ids]

  # 每一題（itemID）：這個構面裡的版本、題目、屬於哪些構面（任一版本）
  k <- match(dims_key(id[rows, , drop = FALSE]), dims_key(ib))
  # 原文：（題目表裡沒有這一題）
  label <- ifelse(is.na(k), "\uff08\u984c\u76ee\u8868\u88e1\u6c92\u6709\u9019\u4e00\u984c\uff09", collapse_newlines(ib$label[k]))
  D <- as.matrix(id[rows, dims, drop = FALSE]) == 1L
  D_item <- matrix(FALSE, 0L, length(dims), dimnames = list(NULL, dims))
  if (length(rows)) {
    D_item <- rowsum(D * 1L, id$itemID[rows], reorder = FALSE)[item_ids, , drop = FALSE] > 0L
  }
  n_dims <- rowSums(D_item)
  multi <- n_dims > 1L
  by_item <- split(seq_along(rows), factor(id$itemID[rows], levels = item_ids))
  item_vers <- vapply(by_item, function(j) max(as.integer(id$itemVer[rows[j]])), integer(1L),
                      USE.NAMES = FALSE)
  tips <- vapply(seq_along(item_ids), function(i) {
    j <- by_item[[i]]
    # 原文：：
    vers <- paste0("v", as.integer(id$itemVer[rows[j]]), "\uff1a", net_esc(trunc_label(label[j], 60)),
                   collapse = "<br>")
    paste0("<b>", net_esc(item_ids[i]), "</b>",
           # 原文：（ |  個版本）
           if (length(j) > 1L) paste0("\uff08", length(j), " \u500b\u7248\u672c\uff09") else "",
           "<br>", vers,
           # 原文：<br>屬於  |  個構面
           if (multi[i]) paste0("<br>\u5c6c\u65bc ", n_dims[i], " \u500b\u69cb\u9762") else "",
           # 原文：<br>點一下看 v
           if (length(j) > 1L) paste0("<br>\u9ede\u4e00\u4e0b\u770b v", item_vers[i]) else "")
  }, character(1L))
  ink <- NET_INK
  lime <- net_lime()
  n <- length(item_ids)

  item_nodes <- data.frame(
    id = net_item_id(item_ids, item_vers),
    label = item_ids,
    title = tips,
    shape = rep("dot", n),
    size = rep(10, n),
    color.background = ifelse(multi, NET_YELLOW, "#ffffff"),
    color.border = rep(ink, n),
    color.highlight.background = ifelse(multi, NET_YELLOW, "#ffffff"),
    color.highlight.border = rep(ink, n),
    color.hover.background = ifelse(multi, NET_YELLOW, "#ffffff"),
    color.hover.border = rep(ink, n),
    font.size = rep(12, n),
    stringsAsFactors = FALSE
  )

  dim_node <- function(d, main = FALSE) {
    lab <- dims_label(bank, d)
    n <- vapply(d, function(x) length(unique(id$itemID[id[[x]] == 1L])), integer(1L))
    data.frame(
      id = net_dim_id(d),
      label = lab,
      # 原文： 題
      title = paste0("<b>", net_esc(lab), "</b><br>", n, " \u984c",
                     # 原文：<br>點一下看這個構面
                     if (!main) "<br>\u9ede\u4e00\u4e0b\u770b\u9019\u500b\u69cb\u9762" else ""),
      shape = rep("box", length(d)),
      size = rep(if (main) 30 else 20, length(d)),
      color.background = rep(lime, length(d)),
      color.border = rep(ink, length(d)),
      color.highlight.background = rep(lime, length(d)),
      color.highlight.border = rep(ink, length(d)),
      color.hover.background = rep(lime, length(d)),
      color.hover.border = rep(ink, length(d)),
      font.size = rep(if (main) 18 else 14, length(d)),
      stringsAsFactors = FALSE
    )
  }

  nodes <- rbind(dim_node(dim_id, main = TRUE), item_nodes)
  edges <- data.frame(from = rep(net_dim_id(dim_id), n),
                      to = item_nodes$id, stringsAsFactors = FALSE)
  others <- character(0)
  if (isTRUE(other_dims) && n > 0L) {
    hit <- which(D_item, arr.ind = TRUE)
    hit <- hit[dims[hit[, "col"]] != dim_id, , drop = FALSE]
    if (nrow(hit) > 0L) {
      others <- unique(dims[hit[, "col"]])
      nodes <- rbind(nodes, dim_node(others))
      edges <- rbind(edges, data.frame(
        from = net_dim_id(dims[hit[, "col"]]),
        to = item_nodes$id[hit[, "row"]],
        stringsAsFactors = FALSE
      ))
    }
  }
  rownames(nodes) <- NULL
  rownames(edges) <- NULL
  list(
    nodes = nodes, edges = edges,
    n_items = n_items, n_shown = n_shown,
    n_omitted = n_items - n_shown,
    n_multi = sum(multi), n_other = length(others)
  )
}

#' 網路圖下面的一行：怎麼讀（圖例用文字，不只靠顏色）、沒畫的題數
#' @noRd
dim_network_hint <- function(net) {
  paste0(
    # 原文：怎麼讀：黃綠色方框是構面，圓點是題目；
    "\u600e\u9ebc\u8b80\uff1a\u9ec3\u7da0\u8272\u65b9\u6846\u662f\u69cb\u9762\uff0c\u5713\u9ede\u662f\u984c\u76ee\uff1b",
    # 原文：黃色圓點是也屬於其他構面的題目（
    if (net$n_multi > 0L) paste0("\u9ec3\u8272\u5713\u9ede\u662f\u4e5f\u5c6c\u65bc\u5176\u4ed6\u69cb\u9762\u7684\u984c\u76ee\uff08",
                                 # 原文： 題）；
                                 kitDesign::kit_num(net$n_multi), " \u984c\uff09\uff1b")
    # 原文：這些題目都只屬於這個構面；
    else "\u9019\u4e9b\u984c\u76ee\u90fd\u53ea\u5c6c\u65bc\u9019\u500b\u69cb\u9762\uff1b",
    # 原文：滑過去看題目，點一題到「題項查詢」看細節
    "\u6ed1\u904e\u53bb\u770b\u984c\u76ee\uff0c\u9ede\u4e00\u984c\u5230\u300c\u984c\u9805\u67e5\u8a62\u300d\u770b\u7d30\u7bc0",
    # 原文：，點其他構面換看那個構面
    if (net$n_other > 0L) "\uff0c\u9ede\u5176\u4ed6\u69cb\u9762\u63db\u770b\u90a3\u500b\u69cb\u9762" else "",
    # 原文：。
    "\u3002",
    if (net$n_omitted > 0L) paste0(
      # 原文：題目太多，只畫前  |  題，另外 
      "\u984c\u76ee\u592a\u591a\uff0c\u53ea\u756b\u524d ", kitDesign::kit_num(net$n_shown), " \u984c\uff0c\u53e6\u5916 ",
      # 原文： 題沒有畫（完整的看上面的表）。
      kitDesign::kit_num(net$n_omitted), " \u984c\u6c92\u6709\u756b\uff08\u5b8c\u6574\u7684\u770b\u4e0a\u9762\u7684\u8868\uff09\u3002"
    ) else ""
  )
}

#' 畫成 visNetwork
#' @param net dim_network() 的結果
#' @param click_input 點節點時送到哪一個 Shiny input（含 namespace）；NULL 不送
#' @noRd
dim_network_widget <- function(net, click_input = NULL, height = "520px") {
  w <- visNetwork::visNetwork(net$nodes, net$edges, height = height, width = "100%")
  w <- visNetwork::visNodes(
    w, borderWidth = 2, borderWidthSelected = 3,
    font = list(face = NET_FONT, color = NET_INK)
  )
  w <- visNetwork::visEdges(
    w, width = 1.5, smooth = FALSE,
    color = list(color = NET_LINE, highlight = NET_INK, hover = NET_INK)
  )
  w <- visNetwork::visLayout(w, randomSeed = 20261101)
  w <- visNetwork::visPhysics(
    w, solver = "forceAtlas2Based",
    forceAtlas2Based = list(gravitationalConstant = -60, springLength = 80),
    stabilization = list(enabled = TRUE, iterations = 400)
  )
  w <- visNetwork::visInteraction(w, hover = TRUE, tooltipDelay = 100,
                                  zoomView = TRUE, dragNodes = TRUE)
  events <- list(
    # 穩定以後關掉物理模擬：圖不再晃
    stabilizationIterationsDone = "function() { this.setOptions({physics: false}); }"
  )
  if (!is.null(click_input)) {
    events$selectNode <- sprintf(
      "function(p) { if (p.nodes.length) Shiny.setInputValue('%s', p.nodes[0], {priority: 'event'}); }",
      click_input
    )
  }
  do.call(visNetwork::visEvents, c(list(w), events))
}
