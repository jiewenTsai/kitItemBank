# R/mod_filter.R — 功能 2：即時題項篩選
# filtered_items() reactive 是整個 app 的核心，供功能 3/4 共用。

filterUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # ── 左側：篩選條件面板 ──────────────────────────────────────
      column(
        width = 3,
        wellPanel(
          tags$h6(tags$strong("篩選條件"), class = "mb-3"),
          selectizeInput(
            ns("wave_sel"),
            label   = "波次（可多選）",
            choices = NULL,
            multiple = TRUE,
            options  = list(
              placeholder = "不限波次（全部）",
              plugins     = list("remove_button")
            )
          ),
          selectizeInput(
            ns("dim_sel"),
            label   = "構面（可多選）",
            choices = NULL,
            multiple = TRUE,
            options  = list(
              placeholder = "不限構面（全部）",
              plugins     = list("remove_button")
            )
          ),
          radioButtons(
            ns("filter_logic"),
            label    = "多選邏輯",
            choices  = c("聯集（OR）" = "OR", "交集（AND）" = "AND"),
            selected = "OR",
            inline   = TRUE
          ),
          hr(style = "margin: 0.5rem 0;"),
          actionButton(
            ns("reset_btn"),
            label = "重置條件",
            icon  = icon("rotate-left"),
            class = "btn-sm btn-outline-secondary w-100"
          )
        )
      ),

      # ── 右側：篩選結果 ─────────────────────────────────────────
      column(
        width = 9,
        div(
          class = "d-flex align-items-center mb-2 gap-3",
          h6(textOutput(ns("result_count"), inline = TRUE), class = "mb-0 me-auto"),
          tags$small(class = "text-muted",
            "（進階預覽：結果同步提供給「跨波次對照表」與「構面報告」使用）"
          )
        ),
        div(
          class = "kit-table-scroll",
          DT::dataTableOutput(ns("result_dt"))
        )
      )
    )
  )
}

#' Filter 模組 Server
#' @return filtered_items reactive（data.frame，含 itemBank 所有欄位）
filterServer <- function(id, data_rv) {
  moduleServer(id, function(input, output, session) {

    # ── 初始化選項 ────────────────────────────────────────────────
    observe({
      req(data_rv())
      wave_cols <- get_wave_cols(data_rv()$itemWave)
      dim_cols  <- get_matrix_cols(data_rv()$itemDim)
      updateSelectizeInput(session, "wave_sel",
        choices  = make_col_choices(wave_cols),
        selected = character(0),
        server   = FALSE
      )
      updateSelectizeInput(session, "dim_sel",
        choices  = make_col_choices(dim_cols),
        selected = character(0),
        server   = FALSE
      )
    })

    # ── 重置 ───────────────────────────────────────────────────────
    observeEvent(input$reset_btn, {
      updateSelectizeInput(session, "wave_sel", selected = character(0))
      updateSelectizeInput(session, "dim_sel",  selected = character(0))
      updateRadioButtons(session, "filter_logic", selected = "OR")
    })

    # ── 核心 reactive：filtered_items ────────────────────────────
    filtered_items <- reactive({
      req(data_rv())

      waves <- input$wave_sel   # character vector of selected wave col names
      dims  <- input$dim_sel    # character vector of selected dim  col names
      logic <- input$filter_logic %||% "OR"

      iw <- data_rv()$itemWave
      id <- data_rv()$itemDim
      ib <- data_rv()$itemBank

      # --- 波次篩選 ---
      id_wave <- if (length(waves) == 0L) {
        iw[, c("itemID", "itemVer"), drop = FALSE]
      } else {
        wave_mat   <- iw[, waves, drop = FALSE]
        n_matched  <- rowSums(wave_mat == 1, na.rm = TRUE)
        threshold  <- if (logic == "AND") length(waves) else 1L
        iw[n_matched >= threshold, c("itemID", "itemVer"), drop = FALSE]
      }

      # --- 構面篩選 ---
      id_dim <- if (length(dims) == 0L) {
        id[, c("itemID", "itemVer"), drop = FALSE]
      } else {
        dim_mat    <- id[, dims, drop = FALSE]
        n_matched  <- rowSums(dim_mat == 1, na.rm = TRUE)
        threshold  <- if (logic == "AND") length(dims) else 1L
        id[n_matched >= threshold, c("itemID", "itemVer"), drop = FALSE]
      }

      # --- 交集 + join 回 itemBank ---
      ids <- dplyr::inner_join(id_wave, id_dim, by = c("itemID", "itemVer"))
      dplyr::inner_join(ids, ib, by = c("itemID", "itemVer"))
    })

    # ── 結果筆數 ──────────────────────────────────────────────────
    output$result_count <- renderText({
      n <- nrow(filtered_items())
      paste0("共 ", n, " 筆題項")
    })

    # ── 結果表格 ──────────────────────────────────────────────────
    output$result_dt <- DT::renderDataTable({
      df <- filtered_items()

      show_cols <- intersect(
        c("itemID", "itemVer", "block", "label"),
        names(df)
      )
      disp <- df[, show_cols, drop = FALSE]

      kit_render_item_dt(
        disp,
        page_length = 20L,
        table_class = "table table-sm table-bordered table-hover kit-items-dt"
      )
    })

    # 回傳給 mod_wave_lookup / mod_dim_report 使用
    filtered_items
  })
}

# 空值合併運算子（避免引用 rlang）
`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y
