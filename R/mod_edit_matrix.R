# R/mod_edit_matrix.R — 功能 1：itemDim / itemWave 視覺化編修
#
# 雙模式：
#   - 預覽模式：矩陣總覽（唯讀）
#   - 編修模式：構面/波次指派 + Excel 匯入匯出

editMatrixUI <- function(id, matrix_label = "\u69cb\u9762") {
  ns <- NS(id)

  tagList(
    navset_tab(
      id = ns("mode_tabs"),
      nav_panel(
        title = tagList(icon("eye"), " \u9810\u89bd\u6a21\u5f0f"),
        value = "preview",
        navset_tab(
          nav_panel(
            title = tagList(icon("project-diagram"), " \u7db2\u8def\u5716"),
            tags$p(
              class = "text-muted kit-preview-hint mb-2",
              tags$strong("\u4ea4\u53c9模式（預設）\uff1a"),
              "同一題目一個節點，連到多個", matrix_label,
              "時以紅框標示。",
              tags$strong("\u968e層模式\uff1a"),
              "依構面收合，僅顯示單一構面內題組。"
            ),
            uiOutput(ns("network_hint_ui")),
            checkboxInput(
              ns("network_show_items"), "\u986f\u793a\u984c\u76ee\u7bc0\u9ede",
              value = TRUE
            ),
            checkboxInput(
              ns("network_cluster_by_col"),
              paste0("\u968e\u5c64模式\uff08\u4f9d", matrix_label, "\u6536\u5408\uff09"),
              value = FALSE
            ),
            div(
              class = "kit-network-pane",
              visNetwork::visNetworkOutput(ns("network_viz"), height = "620px")
            )
          ),
          nav_panel(
            title = tagList(icon("table"), " \u8868\u683c"),
            tags$p(
              class = "text-muted kit-preview-hint mb-2",
              paste0("\u77e9\u9663\u7e3d\u89bd\uff1b\u503c\u70ba 1 \u7684\u5132\u5b9a\u683c\u6703\u4ee5\u85cd\u8272\u6a19\u793a\u3002")
            ),
            div(class = "kit-table-scroll",
              DT::dataTableOutput(ns("overview_dt")))
          )
        )
      ),
      nav_panel(
        title = tagList(icon("pen-to-square"), " \u7de8\u4fee\u6a21\u5f0f"),
        value = "edit",
        bslib::layout_columns(
          col_widths = c(3, 9),

          bslib::card(
            bslib::card_header(paste0(matrix_label, "\u6e05\u55ae")),
            selectInput(
              ns("col_sel"), label = NULL,
              choices = character(0), size = 10L,
              selectize = FALSE, width = "100%"
            ),
            div(
              class = "d-flex gap-1 mt-1",
              actionButton(ns("add_col_btn"), "\uff0b\u65b0\u589e",
                icon = icon("plus"), class = "btn-sm btn-primary flex-fill"),
              actionButton(ns("del_col_btn"), "\u2716\u522a\u9664",
                icon = icon("trash"), class = "btn-sm btn-outline-danger flex-fill")
            ),
            uiOutput(ns("add_col_form_ui")),
            hr(style = "margin: 0.6rem 0;"),
            tags$small(class = "text-muted d-block mb-1", "Excel \u532f\u5165/\u532f\u51fa"),
            downloadButton(ns("export_xlsx_btn"), "\u532f\u51fa .xlsx",
              class = "btn-sm btn-outline-secondary w-100 mb-1"),
            fileInput(ns("import_xlsx_file"), NULL,
              buttonLabel = "\u9078\u64c7 Excel\u2026",
              accept = c(".xlsx",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
              width = "100%"),
            actionButton(ns("import_xlsx_btn"), "\u532f\u5165\u4e26\u8986\u84cb",
              class = "btn-sm btn-outline-warning w-100")
          ),

          bslib::card(
            bslib::card_header(
              div(
                class = "d-flex align-items-center flex-wrap gap-2",
                span(textOutput(ns("panel_title"), inline = TRUE), class = "fw-bold me-1"),
                tags$small(class = "text-muted me-auto",
                  textOutput(ns("sel_count_txt"), inline = TRUE)),
                actionButton(ns("sel_all_btn"), "\u5168\u9078",
                  icon = icon("check-square"), class = "btn-sm btn-outline-secondary"),
                actionButton(ns("desel_all_btn"), "\u5168\u6d88",
                  icon = icon("square"), class = "btn-sm btn-outline-secondary"),
                actionButton(ns("save_btn"),
                  paste0("\u5132\u5b58", matrix_label, "\u8a2d\u5b9a"),
                  icon = icon("floppy-disk"), class = "btn-sm btn-success")
              )
            ),
            tags$p(
              class = "text-muted small px-2 mb-0 pt-1",
              "\u9ede\u64ca\u5217 = \u52a0\u5165/\u79fb\u9664\uff08\u85cd\u8272 = \u5df2\u6307\u6d3e\uff09\u3001\u53ef\u641c\u5c0b\u6216\u6309 Shift/Ctrl \u591a\u9078"
            ),
            DT::dataTableOutput(ns("items_dt"))
          )
        )
      )
    )
  )
}


editMatrixServer <- function(id, data_rv, domain_rv,
                             matrix_type = c("dim", "wave")) {
  matrix_type <- match.arg(matrix_type)
  label_zh <- if (matrix_type == "dim") "\u69cb\u9762" else "\u6ce2\u6b21"
  tbl_name  <- if (matrix_type == "dim") "itemDim" else "itemWave"

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    matrix_df <- reactive({
      req(data_rv())
      data_rv()[[tbl_name]]
    })

    mat_cols <- reactive({
      cols <- get_matrix_cols(matrix_df())
      if (matrix_type == "wave") sort_wave_cols(cols) else cols
    })

    observe({
      cols    <- mat_cols()
      display <- col_display_labels(cols)
      choices <- stats::setNames(cols, display)
      cur     <- isolate(input$col_sel)
      sel     <- if (!is.null(cur) && cur %in% cols) cur else cols[1L]
      updateSelectInput(session, "col_sel", choices = choices, selected = sel)
    })

    sel_col <- reactive({ req(input$col_sel); input$col_sel })

    output$panel_title <- renderText({
      col <- tryCatch(sel_col(), error = function(e) NULL)
      if (is.null(col)) return(paste0("\u9078\u53d6", label_zh))
      paste0("\u300c", col_display_labels(col), "\u300d\u7684\u984c\u76ee")
    })

    output$sel_count_txt <- renderText({
      req(data_rv())
      sel <- input$items_dt_rows_selected %||% integer(0)
      paste0("\u5df2\u6307\u6d3e ", length(sel),
             " / ", nrow(data_rv()$itemBank), " \u984c")
    })

    rows_for_col <- function(dim_col) {
      req(data_rv())
      mdf <- matrix_df()
      ib  <- data_rv()$itemBank
      if (!(dim_col %in% names(mdf))) return(integer(0))
      joined <- dplyr::left_join(
        ib[, c("itemID","itemVer")],
        mdf[, c("itemID","itemVer", dim_col)],
        by = c("itemID","itemVer")
      )
      which(!is.na(joined[[dim_col]]) & joined[[dim_col]] == 1L)
    }

    output$items_dt <- DT::renderDataTable({
      req(data_rv())
      ib <- data_rv()$itemBank

      dim_col <- tryCatch(isolate(sel_col()), error = function(e) NULL)
      if (is.null(dim_col)) dim_col <- isolate(mat_cols())[1L]

      init_sel <- rows_for_col(dim_col)
      disp <- ib[, c("itemID","itemVer","block","label"), drop = FALSE]

      kit_render_item_dt(
        disp,
        page_length = 20L,
        selection   = list(mode = "multiple", selected = init_sel, style = "info")
      )
    })

    items_dt_proxy <- DT::dataTableProxy("items_dt", session = session)

    observeEvent(input$mode_tabs, {
      if (!identical(input$mode_tabs, "edit")) return()
      session$onFlushed(function() {
        DT::reloadData(items_dt_proxy, resetPaging = FALSE, clearSelection = "none")
      }, once = TRUE)
    }, ignoreInit = TRUE)

    observeEvent(sel_col(), {
      rows <- rows_for_col(sel_col())
      DT::selectRows(items_dt_proxy, rows)
    }, ignoreInit = TRUE)

    observeEvent(input$sel_all_btn, {
      req(data_rv())
      DT::selectRows(items_dt_proxy, seq_len(nrow(data_rv()$itemBank)))
    })

    observeEvent(input$desel_all_btn, {
      DT::selectRows(items_dt_proxy, integer(0))
    })

    observeEvent(input$save_btn, {
      req(sel_col(), data_rv())
      dim_col    <- sel_col()
      sel_idx    <- input$items_dt_rows_selected %||% integer(0)

      d   <- data_rv()
      mdf <- d[[tbl_name]]
      ib  <- d$itemBank

      if (!(dim_col %in% names(mdf))) {
        showNotification("\u6b04\u4f4d\u4e0d\u5b58\u5728", type = "error"); return()
      }

      mdf_key <- paste(mdf$itemID, mdf$itemVer, sep = "~~")
      sel_key  <- if (length(sel_idx) > 0L)
        paste(ib$itemID[sel_idx], ib$itemVer[sel_idx], sep = "~~")
      else character(0)

      mdf[[dim_col]] <- as.integer(mdf_key %in% sel_key)

      d[[tbl_name]] <- mdf
      data_rv(d)

      tryCatch({
        saved_path <- save_matrix(domain_rv(), tbl_name, mdf)
        showNotification(
          paste0("\u2705 \u5df2\u5132\u5b58\u300c", col_display_labels(dim_col),
                 "\u300d\uff0c\u5171 ", length(sel_idx),
                 " \u984c \u2192 ", saved_path),
          type = "message", duration = 4
        )
      }, error = function(e) {
        showNotification(paste0("\u274c \u5132\u5b58\u5931\u6557\uff1a", e$message),
          type = "error", duration = 8)
      })
    })

    add_form_visible <- reactiveVal(FALSE)
    observeEvent(input$add_col_btn,    { add_form_visible(!add_form_visible()) })
    observeEvent(input$cancel_add_btn, { add_form_visible(FALSE) })

    output$add_col_form_ui <- renderUI({
      if (!add_form_visible()) return(NULL)
      div(
        class = "mt-2 p-2 bg-light rounded",
        textInput(ns("new_col_name"),
          label = paste0("\u65b0", label_zh, "\u540d\u7a31"),
          placeholder = if (label_zh == "\u69cb\u9762") "\u4f8b\uff1a\u8a18\u61b6\u529b"
                        else "\u4f8b\uff1a2024\u5e74",
          width = "100%"),
        div(class = "d-flex gap-1 mt-1",
          actionButton(ns("confirm_add_btn"), "\u78ba\u8a8d\u65b0\u589e",
            class = "btn-sm btn-primary"),
          actionButton(ns("cancel_add_btn"), "\u53d6\u6d88",
            class = "btn-sm btn-outline-secondary")
        )
      )
    })

    observeEvent(input$confirm_add_btn, {
      new_name <- trimws(input$new_col_name)
      if (nchar(new_name) == 0L) {
        showNotification(paste0(label_zh, "\u540d\u7a31\u4e0d\u80fd\u70ba\u7a7a"),
          type = "warning"); return()
      }
      d   <- data_rv()
      mdf <- d[[tbl_name]]
      if (new_name %in% names(mdf)) {
        showNotification(paste0("\u300c", new_name, "\u300d\u5df2\u5b58\u5728"),
          type = "warning"); return()
      }
      mdf[[new_name]] <- 0L
      d[[tbl_name]]   <- mdf
      data_rv(d)
      tryCatch({
        save_matrix(domain_rv(), tbl_name, mdf)
        cols    <- get_matrix_cols(mdf)
        display <- col_display_labels(cols)
        updateSelectInput(session, "col_sel",
          choices  = stats::setNames(cols, display),
          selected = new_name)
        add_form_visible(FALSE)
        updateTextInput(session, "new_col_name", value = "")
        showNotification(paste0("\u2705 \u5df2\u65b0\u589e\u300c", new_name, "\u300d"),
          type = "message")
      }, error = function(e) {
        showNotification(paste0("\u274c \u5132\u5b58\u5931\u6557\uff1a", e$message),
          type = "error")
      })
    })

    observeEvent(input$del_col_btn, {
      req(sel_col())
      dim_col <- sel_col()
      showModal(modalDialog(
        title = paste0("\u78ba\u8a8d\u522a\u9664\u300c",
          col_display_labels(dim_col), "\u300d\uff1f"),
        tags$p(paste0("\u5c07\u79fb\u9664\u3010", label_zh,
          "\u3011\u6b04\u4f4d\u4e26\u7acb\u5373\u5132\u5b58\u3002")),
        tags$p(class = "text-danger fw-bold",
          "\u26a0 \u6b64\u64cd\u4f5c\u7121\u6cd5\u5fa9\u539f\u3002"),
        footer = tagList(
          actionButton(ns("confirm_del_btn"), "\u78ba\u8a8d\u522a\u9664",
            class = "btn-danger"),
          modalButton("\u53d6\u6d88")
        )
      ))
    })

    observeEvent(input$confirm_del_btn, {
      req(sel_col())
      dim_col <- sel_col()
      d   <- data_rv()
      mdf <- d[[tbl_name]]
      if (length(get_matrix_cols(mdf)) <= 1L) {
        showNotification("\u81f3\u5c11\u4fdd\u7559\u4e00\u500b\u6b04\u4f4d",
          type = "warning"); removeModal(); return()
      }
      mdf[[dim_col]] <- NULL
      d[[tbl_name]]  <- mdf
      data_rv(d)
      tryCatch({
        save_matrix(domain_rv(), tbl_name, mdf)
        removeModal()
        showNotification(paste0("\u2705 \u5df2\u522a\u9664\u300c",
          col_display_labels(dim_col), "\u300d"), type = "message")
      }, error = function(e) {
        removeModal()
        showNotification(paste0("\u274c \u5132\u5b58\u5931\u6557\uff1a", e$message),
          type = "error")
      })
    })

    output$export_xlsx_btn <- downloadHandler(
      filename = function() {
        paste0(tbl_name, "_", domain_rv(), "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        export_matrix_xlsx(matrix_df(), file, sheet_name = tbl_name)
      }
    )

    observeEvent(input$import_xlsx_btn, {
      req(input$import_xlsx_file)
      tryCatch({
        imported <- import_matrix_xlsx(
          input$import_xlsx_file$datapath,
          table_name = tbl_name
        )
        d <- data_rv()
        d[[tbl_name]] <- imported
        data_rv(d)
        path <- save_matrix(domain_rv(), tbl_name, imported)
        showNotification(
          paste0("\u2705 \u5df2\u532f\u5165\u4e26\u5132\u5b58 ", tbl_name,
                 " \u2192 ", path),
          type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste0("\u274c \u532f\u5165\u5931\u6557\uff1a", e$message),
          type = "error", duration = 8)
      })
    })

    output$network_hint_ui <- renderUI({
      if (has_visnetwork()) return(NULL)
      div(
        class = "alert alert-warning py-2",
        icon("triangle-exclamation"), " ",
        "請安裝 ", tags$code("visNetwork"), " 套件以顯示網路圖：",
        tags$code("install.packages('visNetwork')")
      )
    })

    output$network_viz <- visNetwork::renderVisNetwork({
      req(data_rv())
      if (!has_visnetwork()) return(NULL)

      net <- build_matrix_network(
        matrix_df(), data_rv()$itemBank,
        column_label   = label_zh,
        show_items     = isTRUE(input$network_show_items),
        cluster_by_col = isTRUE(input$network_cluster_by_col)
      )
      if (is.null(net$nodes) || nrow(net$nodes) == 0L) return(NULL)

      tryCatch(
        render_matrix_visnetwork(
          net$nodes, net$edges,
          column_label   = label_zh,
          cluster_by_col = isTRUE(input$network_cluster_by_col)
        ),
        error = function(e) NULL
      )
    })

    output$overview_dt <- DT::renderDataTable({
      df <- matrix_df()
      mat_cols    <- mat_cols()
      display_df  <- df[, c("itemID", "itemVer", mat_cols), drop = FALSE]
      display_cols <- col_display_labels(names(display_df))
      names(display_df) <- display_cols

      dt <- DT::datatable(
        display_df,
        rownames = FALSE,
        class    = "table table-sm table-bordered kit-items-dt",
        options  = list(
          scrollX    = TRUE,
          pageLength = 15L,
          dom        = "lftp",
          language   = kit_dt_lang()
        )
      )

      mat_display <- col_display_labels(mat_cols)
      for (col in mat_display) {
        dt <- DT::formatStyle(
          dt,
          columns = col,
          target  = "cell",
          backgroundColor = DT::styleEqual(
            c(0L, 1L, "0", "1"),
            c("transparent", "#cfe2ff", "transparent", "#cfe2ff")
          ),
          color = DT::styleEqual(
            c(0L, 1L, "0", "1"),
            c("#999999", "#003366", "#999999", "#003366")
          ),
          fontWeight = DT::styleEqual(
            c(0L, 1L, "0", "1"),
            c("normal", "bold", "normal", "bold")
          )
        )
      }
      dt
    })
  })
}
