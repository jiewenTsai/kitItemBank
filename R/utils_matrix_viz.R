# R/utils_matrix_viz.R — 矩陣預覽：構面/波次 ↔ 題目（含交叉連結）

item_node_id <- function(itemID, itemVer) {
  paste0("I::", itemID, "::", as.integer(itemVer))
}

#' 由 itemDim / itemWave 建立二部圖
#' @param cluster_by_col TRUE=每構面獨立題目節點（階層）；FALSE=共用題目節點（交叉）
build_matrix_network <- function(matrix_df, item_bank, column_label = "\u69cb\u9762",
                                 max_items_per_col = 40L, show_items = TRUE,
                                 cluster_by_col = FALSE) {
  empty <- list(
    nodes = data.frame(
      id = character(), label = character(), group = character(),
      cluster = character(), n_dims = integer(), value = numeric(),
      title = character(), stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = character(), to = character(),
      value = numeric(), title = character(), stringsAsFactors = FALSE
    )
  )

  mat_cols <- get_matrix_cols(matrix_df)
  if (length(mat_cols) == 0L) return(empty)

  joined <- dplyr::left_join(
    matrix_df,
    item_bank[, c("itemID", "itemVer", "block", "label"), drop = FALSE],
    by = c("itemID", "itemVer")
  )

  col_assigned <- vapply(mat_cols, function(col) {
    sum(joined[[col]] == 1L, na.rm = TRUE)
  }, integer(1L))

  active_cols <- mat_cols[col_assigned > 0L]
  if (length(active_cols) == 0L) return(empty)

  nodes_list <- list()
  edges_list <- list()
  edge_keys  <- character(0)
  item_meta  <- list()

  add_edge <- function(from, to, value = 1L, title = "") {
    key <- paste(from, to, sep = "->")
    if (key %in% edge_keys) return(invisible(NULL))
    edge_keys <<- c(edge_keys, key)
    edges_list[[length(edges_list) + 1L]] <<- data.frame(
      from = from, to = to, value = as.numeric(value), title = title,
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  register_item <- function(row, col) {
    ikey <- paste(as.character(row$itemID), as.integer(row$itemVer), sep = "~~")
    if (is.null(item_meta[[ikey]])) {
      blk <- trimws(as.character(row$block))
      if (!nzchar(blk) || is.na(blk)) blk <- "(\u672a\u5206\u985e)"
      item_meta[[ikey]] <<- list(
        itemID = as.character(row$itemID),
        itemVer = as.integer(row$itemVer),
        block = blk,
        label = collapse_newlines(row$label),
        dims  = character(0)
      )
    }
    cur <- item_meta[[ikey]]
    cur$dims <- unique(c(cur$dims, col))
    item_meta[[ikey]] <<- cur
    invisible(ikey)
  }

  for (col in active_cols) {
    col_id <- paste0("C::", col)
    n_assigned <- col_assigned[[col]]
    nodes_list[[col_id]] <- data.frame(
      id      = col_id,
      label   = trunc_label(col_display_labels(col), 30),
      group   = "column",
      cluster = NA_character_,
      n_dims  = NA_integer_,
      value   = max(8L, min(36L, round(sqrt(n_assigned) * 2) + 4L)),
      title   = paste0(column_label, "\uff1a", col, "\n", n_assigned, " \u984c"),
      stringsAsFactors = FALSE
    )

    if (!isTRUE(show_items)) next

    rows <- joined[!is.na(joined[[col]]) & joined[[col]] == 1L, , drop = FALSE]
    if (nrow(rows) == 0L) next

    omitted <- 0L
    if (nrow(rows) > max_items_per_col) {
      omitted <- nrow(rows) - max_items_per_col
      rows <- rows[seq_len(max_items_per_col), , drop = FALSE]
    }

    for (i in seq_len(nrow(rows))) {
      row  <- rows[i, , drop = FALSE]
      ikey <- register_item(row, col)

      if (isTRUE(cluster_by_col)) {
        node_id <- paste0("I::", row$itemID, "::", row$itemVer, "::", col)
        blk     <- item_meta[[ikey]]$block
        nodes_list[[node_id]] <- data.frame(
          id      = node_id,
          label   = trunc_label(as.character(row$itemID), 18),
          group   = "item",
          cluster = col,
          n_dims  = 1L,
          value   = 6L,
          title   = paste0(row$itemID, " v", row$itemVer,
                           "\n\u6a21\u7d44\uff1a", blk,
                           "\n", item_meta[[ikey]]$label),
          stringsAsFactors = FALSE
        )
        add_edge(col_id, node_id, 1L, "")
      } else {
        node_id <- item_node_id(row$itemID, row$itemVer)
        add_edge(col_id, node_id, 1L, col)
      }
    }

    if (omitted > 0L) {
      more_id <- paste0("M::", col)
      nodes_list[[more_id]] <- data.frame(
        id      = more_id,
        label   = paste0("+\u2026", omitted),
        group   = "item",
        cluster = if (isTRUE(cluster_by_col)) col else NA_character_,
        n_dims  = NA_integer_,
        value   = 5L,
        title   = paste0("\u9084\u6709 ", omitted,
                         " \u984c\uff0c\u8acb\u7528\u300c\u8868\u683c\u300d\u67e5\u770b"),
        stringsAsFactors = FALSE
      )
      add_edge(col_id, more_id, 1L, "")
    }
  }

  if (isTRUE(show_items) && !isTRUE(cluster_by_col) && length(item_meta) > 0L) {
    for (ikey in names(item_meta)) {
      meta    <- item_meta[[ikey]]
      node_id <- item_node_id(meta$itemID, meta$itemVer)
      n_dims  <- length(meta$dims)
      cross   <- n_dims > 1L
      nodes_list[[node_id]] <- data.frame(
        id      = node_id,
        label   = trunc_label(meta$itemID, 18),
        group   = if (cross) "item_cross" else "item",
        cluster = NA_character_,
        n_dims  = n_dims,
        value   = if (cross) 8L else 6L,
        title   = paste0(
          meta$itemID, " v", meta$itemVer,
          if (cross) paste0("\n\u2190 ", n_dims, " \u500b", column_label, "\uff08\u4ea4\u53c9\u8f09\u5165\uff09") else "",
          "\n\u6a21\u7d44\uff1a", meta$block,
          "\n", meta$label,
          "\n", column_label, "\uff1a", paste(meta$dims, collapse = ", ")
        ),
        stringsAsFactors = FALSE
      )
    }
  }

  nodes <- if (length(nodes_list)) do.call(rbind, nodes_list) else empty$nodes
  edges <- if (length(edges_list)) do.call(rbind, edges_list) else empty$edges
  list(nodes = nodes, edges = edges, cluster_by_col = isTRUE(cluster_by_col))
}

#' 渲染 visNetwork
render_matrix_visnetwork <- function(nodes, edges, column_label = "\u69cb\u9762",
                                     cluster_by_col = FALSE) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) stop("visNetwork")
  if (is.null(nodes) || nrow(nodes) == 0L) return(NULL)

  if (is.null(edges) || nrow(edges) == 0L) {
    edges <- data.frame(from = character(), to = character(),
                        value = numeric(), title = character())
  }
  edges$width <- pmax(0.35, log1p(edges$value))

  has_items <- any(nodes$group %in% c("item", "item_cross"))

  net <- visNetwork::visNetwork(nodes, edges, height = "100%", width = "100%") %>%
    visNetwork::visGroups(
      groupname        = "column",
      color            = list(background = "#2e86c1", border = "#1a5276", highlight = "#5dade2"),
      shape            = "box",
      shapeProperties  = list(borderRadius = 14),
      font             = list(size = 13, color = "#ffffff"),
      margin           = 10
    ) %>%
    visNetwork::visGroups(
      groupname        = "item",
      color            = list(background = "#fef5e7", border = "#d68910", highlight = "#fdebd0"),
      shape            = "box",
      shapeProperties  = list(borderRadius = 0),
      font             = list(size = 9, color = "#784212"),
      margin           = 2
    ) %>%
    visNetwork::visGroups(
      groupname        = "item_cross",
      color            = list(background = "#fadbd8", border = "#c0392b", highlight = "#f5b7b1"),
      shape            = "box",
      shapeProperties  = list(borderRadius = 0),
      font             = list(size = 9, color = "#78281f"),
      margin           = 3
    ) %>%
    visNetwork::visEdges(
      arrows = "to",
      color  = list(color = "#bdc3c7", highlight = "#566573"),
      smooth = if (isTRUE(cluster_by_col)) {
        list(type = "cubicBezier", roundness = 0.15)
      } else {
        list(type = "dynamic", roundness = 0.2)
      }
    ) %>%
    visNetwork::visOptions(
      collapse = list(
        enabled = isTRUE(cluster_by_col),
        fit     = TRUE,
        clusterOptions = list(
          shape             = "box",
          shapeProperties   = list(borderRadius = 0),
          font              = list(size = 11),
          color             = list(background = "#fdebd0", border = "#d68910")
        )
      ),
      nodesIdSelection = TRUE,
      highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 2L)
    ) %>%
    visNetwork::visInteraction(
      navigationButtons = TRUE,
      dragView          = TRUE,
      zoomView          = TRUE,
      tooltipDelay      = 150
    ) %>%
    visNetwork::visPhysics(
      solver        = "forceAtlas2Based",
      stabilization = list(enabled = TRUE, iterations = 180L),
      forceAtlas2Based = list(
        gravitationalConstant = if (isTRUE(cluster_by_col)) -50 else -80,
        centralGravity        = 0.008,
        springLength          = if (isTRUE(cluster_by_col)) 100 else 140,
        avoidOverlap          = 0.85
      )
    ) %>%
    visNetwork::visLayout(randomSeed = 42)

  if (isTRUE(cluster_by_col) && has_items && "cluster" %in% names(nodes)) {
    net <- net %>%
      visNetwork::visClusteringByGroup(
        groups = "cluster",
        shape  = "box",
        color  = "#fdebd0",
        label  = paste0(column_label, "\uff1a")
      )
  }

  net
}

has_visnetwork <- function() {
  requireNamespace("visNetwork", quietly = TRUE)
}
