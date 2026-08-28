# R/mod_assemble.R — 功能 5：組卷輸出 Word
#
# 三欄版面：
#   左欄：Block 管理（新增/刪除/上移/下移 + 設定名稱、提示語、說明）
#   中欄：選題（從篩選結果 or 全庫）+ 已選題目排序
#   右欄：量尺設定、開放題格數、產生下載

# ── UI ──────────────────────────────────────────────────────────────
assembleUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "kit-header mb-3",
      tags$h1("組卷"),
      tags$p(
        tags$span(class = "kit-subtitle", "臺灣幼兒發展調查資料庫"),
        " \u00b7 功能 5：設定 Block、選題、預覽與輸出問卷"
      )
    ),

    bslib::navset_tab(
      id = ns("assemble_tabs"),

      bslib::nav_panel(
        title = tagList(icon("pen-to-square"), " 1｜編輯組卷"),
        value = "tab_edit",
        bslib::layout_columns(
          col_widths = c(3, 5, 4),

          bslib::card(
            bslib::card_header(
              class = "d-flex align-items-center gap-2",
              tags$strong("區塊管理"),
              actionButton(ns("add_block_btn"), "\uff0b\u65b0\u589e",
                class = "btn-sm btn-primary ms-auto",
                icon  = icon("plus"))
            ),
            uiOutput(ns("block_list_ui")),
            div(
              class = "d-flex gap-1 mt-1",
              actionButton(ns("up_block_btn"),  "\u2191\u4e0a\u79fb",
                class = "btn-sm btn-outline-secondary flex-fill"),
              actionButton(ns("dn_block_btn"),  "\u2193\u4e0b\u79fb",
                class = "btn-sm btn-outline-secondary flex-fill"),
              actionButton(ns("del_block_btn"), "\u2716\u522a\u9664",
                class = "btn-sm btn-outline-danger flex-fill")
            ),
            hr(style = "margin: 0.6rem 0;"),
            h6("\u5340\u584a\u8a2d\u5b9a", class = "mb-1"),
            uiOutput(ns("block_config_ui")),
            hr(style = "margin: 0.6rem 0;"),
            h6("\u8349\u7a3f\u5b58\u6a94", class = "mb-1"),
            tags$p(class = "text-muted small mb-1",
              "JSON \u683c\u5f0f\u53ef\u66ab\u5b58\u7d44\u5377\u9032\u5ea6\uff0c\u4e4b\u5f8c\u8f09\u5165\u7e7c\u7e8c\u7de8\u8f2f\u3002"),
            downloadButton(ns("save_draft_btn"), "\u4e0b\u8f09\u8349\u7a3f JSON",
              class = "btn-sm btn-outline-primary w-100 mb-1"),
            fileInput(ns("load_draft_file"), NULL,
              buttonLabel = "\u9078\u64c7\u8349\u7a3f JSON\u2026",
              accept = ".json", width = "100%"),
            actionButton(ns("load_draft_btn"), "\u9084\u539f\u8349\u7a3f",
              class = "btn-sm btn-outline-warning w-100")
          ),

          bslib::card(
            bslib::card_header(textOutput(ns("item_panel_title"))),
            uiOutput(ns("item_selector_ui"))
          ),

          bslib::card(
            bslib::card_header("\u91cf\u5c3a\u8207\u532f\u51fa"),
            uiOutput(ns("likert_help_ui")),
            h6("Likert \u91cf\u5c3a", class = "mb-1"),
            fluidRow(
              column(5,
                numericInput(ns("scale_n"), "\u683c\u6578\uff082\u20137\uff09",
                  value = 5L, min = 2L, max = 7L, step = 1L)
              ),
              column(7,
                br(),
                actionButton(ns("parse_scale_btn"),
                  "\u81ea\u52d5\u89e3\u6790",
                  icon  = icon("wand-magic-sparkles"),
                  class = "btn-sm btn-outline-secondary w-100")
              )
            ),
            uiOutput(ns("scale_label_inputs_ui")),
            actionButton(ns("save_scale_btn"), "\u5957\u7528\u91cf\u5c3a\u81f3\u9078\u5b9a\u5340\u584a",
              class = "btn-sm btn-primary w-100 mb-2"),
            hr(style = "margin: 0.5rem 0;"),
            actionButton(ns("dl_word_btn"), "\u4e0b\u8f09 Word \u554f\u5377",
              icon = icon("file-word"), class = "btn-success w-100 mb-1"),
            tags$p(class = "text-muted small mb-0",
              "\u532f\u51fa\u524d\u53ef\u5207\u5230\u300c2\uff5c\u7d50\u69cb\u8207\u9810\u89bd\u300d\u78ba\u8a8d\u5167\u5bb9\u3002"),
            uiOutput(ns("dl_status_ui"))
          )
        )
      ),

      bslib::nav_panel(
        title = tagList(icon("eye"), " 2｜結構與預覽"),
        value = "tab_preview",
        div(
          class = "d-flex align-items-center flex-wrap gap-2 mb-2 mt-1",
          actionButton(ns("preview_btn"), "\u66f4\u65b0\u9810\u89bd",
            icon = icon("rotate"), class = "btn-outline-primary btn-sm"),
          tags$span(class = "text-muted small",
            "\u6b64\u9801\u70ba\u5168\u87fb\u9810\u89bd\u5340\uff1b\u9078\u984c\u6216\u91cf\u5c3a\u8b8a\u66f4\u5f8c\u8acb\u6309\u300c\u66f4\u65b0\u9810\u89bd\u300d\u3002")
        ),
        bslib::navset_tab(
          bslib::nav_panel(
            "\u7d50\u69cb\u6982\u89bd",
            div(class = "kit-preview-pane p-2", uiOutput(ns("structure_preview_ui")))
          )
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────────────
#' @param id               namespace id
#' @param data_rv          reactiveVal（四張表 list）
#' @param filtered_items_r reactive，回傳 filtered_items（功能 2 的結果）
assembleServer <- function(id, data_rv, filtered_items_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Block 狀態 ────────────────────────────────────────────────
    blk <- reactiveValues(
      order   = character(0),  # block IDs in display order
      data    = list(),        # named list: id -> block_cfg
      next_id = 1L,
      next_mg = 1L             # matrix group id counter
    )

    current_id <- reactiveVal(NULL)

    # 取得目前選取的 block config
    cur_blk <- reactive({
      cid <- current_id()
      if (is.null(cid) || !(cid %in% names(blk$data))) return(NULL)
      blk$data[[cid]]
    })

    # 預設 block config
    make_new_block <- function(id, name) {
      list(
        id             = id,
        name           = name,
        prompt         = "",
        instruction    = "\u6bcf\u984c\u53ea\u80fd\u9078\u4e00\u500b\u7b54\u6848\u3002",  # 每題只能選一個答案。
        items          = list(),
        scale_n        = 5L,
        scale_codes    = c("1", "2", "3", "4", "5"),
        scale_labels   = c("\u5b8c\u5168\u4e0d\u80fd", "\u5c1a\u5f37\u52c9\u5f37",
                           "\u9084\u7b97\u719f\u7df4", "\u975e\u5e38\u719f\u7df4", "\u62d2\u7b54"),
        open_box_lines = 1L
      )
    }

    # ── Block 操作 ────────────────────────────────────────────────

    # 新增
    observeEvent(input$add_block_btn, {
      new_id   <- paste0("blk_", blk$next_id)
      new_name <- paste0("\u65b0\u5340\u584a ", blk$next_id)  # 新區塊 N
      blk$next_id <- blk$next_id + 1L
      blk$data[[new_id]] <- make_new_block(new_id, new_name)
      blk$order <- c(blk$order, new_id)
      current_id(new_id)
    })

    # 刪除
    observeEvent(input$del_block_btn, {
      cid <- current_id()
      req(cid %in% blk$order)
      blk$order <- blk$order[blk$order != cid]
      blk$data[[cid]] <- NULL
      current_id(if (length(blk$order) > 0L) blk$order[1L] else NULL)
    })

    # 上移
    observeEvent(input$up_block_btn, {
      cid <- current_id(); req(cid)
      ord <- blk$order
      idx <- which(ord == cid)
      if (length(idx) == 1L && idx > 1L) {
        ord[c(idx-1L, idx)] <- ord[c(idx, idx-1L)]
        blk$order <- ord
      }
    })

    # 下移
    observeEvent(input$dn_block_btn, {
      cid <- current_id(); req(cid)
      ord <- blk$order
      idx <- which(ord == cid)
      if (length(idx) == 1L && idx < length(ord)) {
        ord[c(idx, idx+1L)] <- ord[c(idx+1L, idx)]
        blk$order <- ord
      }
    })

    # 從列表點選
    observeEvent(input$block_selector, {
      req(input$block_selector %in% blk$order)
      current_id(input$block_selector)
    })

    # ── Block 清單 UI ─────────────────────────────────────────────
    output$block_list_ui <- renderUI({
      ord <- blk$order
      if (length(ord) == 0L) {
        return(div(class = "text-muted small py-2",
          "\u5c1a\u7121\u5340\u584a\uff0c\u8acb\u6309\u300c\uff0b\u65b0\u589e\u300d"))  # 尚無區塊
      }
      names_vec <- sapply(ord, function(id) blk$data[[id]]$name)
      choices <- setNames(ord, paste0(seq_along(ord), ". ", names_vec))

      selectInput(
        ns("block_selector"),
        label     = NULL,
        choices   = choices,
        selected  = current_id(),
        size      = min(8L, length(ord)),
        selectize = FALSE,
        width     = "100%"
      )
    })

    # ── Block 設定 UI（名稱、提示語、說明）───────────────────────
    output$block_config_ui <- renderUI({
      cb <- cur_blk()
      if (is.null(cb)) {
        return(div(class = "text-muted small",
          "\u8acb\u9078\u53d6\u6216\u65b0\u589e\u5340\u584a"))
      }
      tagList(
        textInput(ns("blk_name"), "\u5340\u584a\u540d\u7a31", value = cb$name),
        textAreaInput(ns("blk_prompt"),
          "\u63d0\u793a\u8a9e\uff08\u7531\u7d44\u5377\u8005\u5361\u5165\uff09",
          value = cb$prompt, rows = 3L, resize = "vertical"),
        textInput(ns("blk_instruction"),
          "\u7b54\u984c\u8aaa\u660e", value = cb$instruction),
        actionButton(ns("save_blk_cfg_btn"),
          "\u5132\u5b58\u5340\u584a\u8a2d\u5b9a",
          icon  = icon("floppy-disk"),
          class = "btn-sm btn-outline-primary w-100 mt-1")
      )
    })

    observeEvent(input$save_blk_cfg_btn, {
      cid <- current_id(); req(cid)
      blk$data[[cid]]$name        <- input$blk_name
      blk$data[[cid]]$prompt      <- input$blk_prompt
      blk$data[[cid]]$instruction <- input$blk_instruction
      showNotification("\u5340\u584a\u8a2d\u5b9a\u5df2\u5132\u5b58", type = "message", duration = 2)
    })

    # ── 選題面板標題 ──────────────────────────────────────────────
    output$item_panel_title <- renderText({
      cb <- cur_blk()
      if (is.null(cb)) return("\u984c\u76ee\u9078\u64c7")
      paste0("\u984c\u76ee\u9078\u64c7 \u2014 ", cb$name)
    })

    # ── 選題 UI ───────────────────────────────────────────────────
    output$item_selector_ui <- renderUI({
      if (is.null(current_id())) {
        return(div(class = "text-muted p-2",
          "\u8acb\u5148\u9078\u53d6\u4e00\u500b\u5340\u584a"))
      }
      tagList(
        tags$p(class = "text-muted small mb-2",
          tags$strong("\u5f9e\u7b4b\u9078\u7d50\u679c\u532f\u5165\uff1a"),
          "\u53ea\u986f\u793a\u300c2\uff5c\u7be9\u9078\u300d\u9801\u76ee\u524d\u7be9\u51fa\u7684\u984c\u76ee\uff0c\u9069\u5408\u6309\u69cb\u9762/\u6ce2\u6b21\u689d\u4ef6\u6279\u6b21\u9078\u984c\u3002",
          tags$br(),
          tags$strong("\u5168\u5eab\u624b\u52d5\u9078\u984c\uff1a"),
          "\u986f\u793a\u6574\u500b\u984c\u5eab\uff0c\u53ef\u641c\u5c0b\u4efb\u610f\u984c\u76ee\uff0c\u4e0d\u53d7\u7be9\u9078\u689d\u4ef6\u9650\u5236\u3002"
        ),
        bslib::navset_tab(
          bslib::nav_panel(
            "\u5f9e\u7b4b\u9078\u7d50\u679c\u532f\u5165",
            div(class = "mt-2",
              DT::dataTableOutput(ns("filter_items_dt")),
              actionButton(ns("add_from_filter_btn"),
                "\u52a0\u5165\u9078\u53d6\u984c\u76ee",
                icon  = icon("plus"),
                class = "btn-sm btn-primary mt-1")
            )
          ),
          bslib::nav_panel(
            "\u5168\u5eab\u624b\u52d5\u9078\u984c",
            div(class = "mt-2",
              DT::dataTableOutput(ns("all_items_dt")),
              actionButton(ns("add_from_all_btn"),
                "\u52a0\u5165\u9078\u53d6\u984c\u76ee",
                icon  = icon("plus"),
                class = "btn-sm btn-primary mt-1")
            )
          )
        ),
        hr(style = "margin: 0.5rem 0;"),
        tags$h6("\u5df2\u9078\u984c\u76ee\uff08\u53ef\u591a\u9078\u9032\u884c\u77e9\u9663\u8a2d\u5b9a\uff09"),
        selectInput(ns("sel_item_list"), NULL,
          choices   = character(0),
          selected  = NULL,
          multiple  = TRUE,
          size      = 6L,
          selectize = FALSE,
          width     = "100%"
        ),
        div(class = "d-flex gap-1 flex-wrap mb-1",
          actionButton(ns("matrix_group_btn"), "\u5408\u4f75\u70ba\u77e9\u9663\u984c\u7d44",
            icon = icon("table"), class = "btn-sm btn-outline-primary"),
          actionButton(ns("matrix_single_btn"), "\u6539\u70ba\u55ae\u984c",
            icon = icon("align-left"), class = "btn-sm btn-outline-secondary"),
          actionButton(ns("matrix_auto_btn"), "\u6062\u5fa9\u81ea\u52d5",
            icon = icon("wand-magic-sparkles"), class = "btn-sm btn-outline-secondary")
        ),
        tags$p(class = "text-muted small mb-1",
          "\u300c\u77e9\u9663\u8868\u683c\u300d=\u7bc4\u4f8b docx \u683c\u5f0f B\uff08\u5217\u984c+\u91cf\u5c3a\u6b04\u683c\uff09\uff1b",
          "\u300c\u55ae\u984c\u300d=\u683c\u5f0f A\uff08\u6bcf\u984c\u4e00\u884c \u25a1 1 xxx\uff09\u3002",
          " \u6709 stemID \u7684\u984c\u76ee\u9810\u8a2d\u81ea\u52d5\u5408\u4f75\u70ba\u77e9\u9663\u984c\u7d44\u3002"),
        div(class = "d-flex gap-1",
          actionButton(ns("item_up_btn"),  "\u2191\u4e0a\u79fb",
            class = "btn-sm btn-outline-secondary"),
          actionButton(ns("item_dn_btn"),  "\u2193\u4e0b\u79fb",
            class = "btn-sm btn-outline-secondary"),
          actionButton(ns("item_del_btn"), "\u2715\u79fb\u9664",
            class = "btn-sm btn-outline-danger"),
          tags$span(class = "ms-auto text-muted small",
            textOutput(ns("item_count_txt"), inline = TRUE))
        )
      )
    })

    # ── 篩選結果 DT ───────────────────────────────────────────────
    output$filter_items_dt <- DT::renderDataTable({
      req(data_rv())
      fi <- filtered_items_r()
      if (nrow(fi) == 0L)
        return(DT::datatable(data.frame(msg = "\u7b4b\u9078\u7d50\u679c\u70ba\u7a7a")))
      disp <- fi[, c("itemID", "itemVer", "block", "label"), drop = FALSE]
      disp$label <- trunc_label(collapse_newlines(disp$label), 60)
      DT::datatable(disp,
        selection = "multiple", rownames = FALSE,
        class = "table table-sm table-bordered",
        options = list(scrollX = TRUE, pageLength = 8L, dom = "ftp",
          language = list(search = "\u641c\u5c0b\uff1a"))
      )
    })

    # ── 全庫 DT ───────────────────────────────────────────────────
    output$all_items_dt <- DT::renderDataTable({
      req(data_rv())
      ib   <- data_rv()$itemBank
      disp <- ib[, c("itemID", "itemVer", "block", "label"), drop = FALSE]
      disp$label <- trunc_label(collapse_newlines(disp$label), 60)
      DT::datatable(disp,
        selection = "multiple", rownames = FALSE,
        class = "table table-sm table-bordered",
        options = list(scrollX = TRUE, pageLength = 8L, dom = "lftp",
          language = list(search = "\u641c\u5c0b\uff1a",
            lengthMenu = "\u6bcf\u9801 _MENU_ \u7b46"))
      )
    })

    # ── Helper：把新選題目加入目前 block ─────────────────────────
    add_items_to_cur_block <- function(new_rows_df) {
      cid <- current_id(); req(cid)
      cur_items <- blk$data[[cid]]$items
      exist_keys <- sapply(cur_items, function(x)
        paste(x$itemID, x$itemVer, sep = "~~"))

      new_items <- lapply(seq_len(nrow(new_rows_df)), function(i) {
        r <- new_rows_df[i, , drop = FALSE]
        k <- paste(r$itemID, r$itemVer, sep = "~~")
        if (k %in% exist_keys) return(NULL)
        make_item_entry(r)
      })
      new_items <- Filter(Negate(is.null), new_items)
      blk$data[[cid]]$items <- c(cur_items, new_items)
    }

    observeEvent(input$add_from_filter_btn, {
      sel <- input$filter_items_dt_rows_selected
      req(length(sel) > 0L)
      add_items_to_cur_block(filtered_items_r()[sel, , drop = FALSE])
      showNotification(paste0("\u5df2\u52a0\u5165 ", length(sel), " \u984c"),  # 已加入 N 題
        type = "message", duration = 2)
    })

    observeEvent(input$add_from_all_btn, {
      sel <- input$all_items_dt_rows_selected
      req(length(sel) > 0L)
      add_items_to_cur_block(data_rv()$itemBank[sel, , drop = FALSE])
      showNotification(paste0("\u5df2\u52a0\u5165 ", length(sel), " \u984c"),
        type = "message", duration = 2)
    })

    item_layout_badge <- function(x) {
      layout <- x$layout %||% "auto"
      if (identical(layout, "single")) return("[\u55ae\u984c]")
      if (identical(layout, "matrix")) return("[\u77e9\u9663]")
      if (!is.null(x$matrix_group) && nzchar(x$matrix_group)) return("[\u77e9\u9663\u00b7\u81ea\u52d5]")
      "[\u81ea\u52d5]"
    }

    # ── 更新已選題目列表 ──────────────────────────────────────────
    observe({
      cb <- cur_blk()
      if (is.null(cb) || length(cb$items) == 0L) {
        updateSelectInput(session, "sel_item_list", choices = character(0), selected = character(0))
        return()
      }
      req(data_rv())
      ib     <- data_rv()$itemBank
      items  <- cb$items
      keys   <- sapply(items, function(x) paste(x$itemID, x$itemVer, sep = "~~"))
      labels <- sapply(items, function(x) {
        r <- ib[ib$itemID == x$itemID & ib$itemVer == x$itemVer, ]
        badge <- item_layout_badge(x)
        base  <- if (nrow(r) == 0L) paste0("[", x$itemID, " v", x$itemVer, "]")
                 else paste0("[", x$itemID, " v", x$itemVer, "] ",
                              trunc_label(collapse_newlines(r$label[1L]), 40))
        paste(badge, base)
      })
      sel_keep <- intersect(input$sel_item_list %||% character(0), keys)
      updateSelectInput(session, "sel_item_list",
        choices  = setNames(keys, labels),
        selected = sel_keep
      )
    })

    output$item_count_txt <- renderText({
      cb <- cur_blk()
      if (is.null(cb)) return("")
      paste0("\u5df2\u9078 ", length(cb$items), " \u984c")  # 已選 N 題
    })

    selected_item_keys <- reactive({
      sel <- input$sel_item_list
      if (is.null(sel) || length(sel) == 0L) return(character(0))
      sel
    })

    # ── 上移選取題目 ──────────────────────────────────────────────
    observeEvent(input$item_up_btn, {
      cid <- current_id(); req(cid)
      sel <- selected_item_keys()
      req(length(sel) >= 1L)
      key <- sel[1L]
      items <- blk$data[[cid]]$items
      keys  <- sapply(items, function(x) paste(x$itemID, x$itemVer, sep = "~~"))
      idx   <- which(keys == key)
      if (length(idx) == 1L && idx > 1L) {
        items[c(idx-1L, idx)] <- items[c(idx, idx-1L)]
        blk$data[[cid]]$items <- items
      }
    })

    # ── 下移選取題目 ──────────────────────────────────────────────
    observeEvent(input$item_dn_btn, {
      cid <- current_id(); req(cid)
      sel <- selected_item_keys()
      req(length(sel) >= 1L)
      key <- sel[1L]
      items <- blk$data[[cid]]$items
      keys  <- sapply(items, function(x) paste(x$itemID, x$itemVer, sep = "~~"))
      idx   <- which(keys == key)
      if (length(idx) == 1L && idx < length(items)) {
        items[c(idx, idx+1L)] <- items[c(idx+1L, idx)]
        blk$data[[cid]]$items <- items
      }
    })

    # ── 移除選取題目 ──────────────────────────────────────────────
    observeEvent(input$item_del_btn, {
      cid <- current_id(); req(cid)
      sel <- selected_item_keys()
      req(length(sel) >= 1L)
      items <- blk$data[[cid]]$items
      keys  <- sapply(items, function(x) paste(x$itemID, x$itemVer, sep = "~~"))
      blk$data[[cid]]$items <- items[!(keys %in% sel)]
    })

    apply_layout_to_selected <- function(fn) {
      cid <- current_id(); req(cid)
      sel <- selected_item_keys()
      req(length(sel) >= 1L)
      items <- blk$data[[cid]]$items
      ib    <- data_rv()$itemBank
      changed <- 0L
      for (i in seq_along(items)) {
        key <- paste(items[[i]]$itemID, items[[i]]$itemVer, sep = "~~")
        if (!(key %in% sel)) next
        row <- ib[ib$itemID == items[[i]]$itemID &
                  ib$itemVer == items[[i]]$itemVer, , drop = FALSE]
        items[[i]] <- fn(items[[i]], if (nrow(row) > 0L) row[1L, , drop = FALSE] else NULL)
        changed <- changed + 1L
      }
      blk$data[[cid]]$items <- items
      changed
    }

    observeEvent(input$matrix_group_btn, {
      mg_id <- paste0("mg_", blk$next_mg)
      blk$next_mg <- blk$next_mg + 1L
      n <- apply_layout_to_selected(function(it, row) {
        it$layout <- "matrix"
        it$matrix_group <- mg_id
        it
      })
      showNotification(paste0("\u5df2\u5c07 ", n, " \u984c\u8a2d\u70ba\u77e9\u9663\u984c\u7d44"),
        type = "message", duration = 2)
    })

    observeEvent(input$matrix_single_btn, {
      n <- apply_layout_to_selected(function(it, row) {
        it$layout <- "single"
        it$matrix_group <- NULL
        it
      })
      showNotification(paste0("\u5df2\u5c07 ", n, " \u984c\u8a2d\u70ba\u55ae\u984c\u986f\u793a"),
        type = "message", duration = 2)
    })

    observeEvent(input$matrix_auto_btn, {
      n <- apply_layout_to_selected(function(it, row) {
        if (is.null(row)) {
          it$layout <- "auto"
          it$matrix_group <- NULL
          return(it)
        }
        entry <- make_item_entry(row)
        it$layout <- entry$layout
        it$matrix_group <- entry$matrix_group
        it
      })
      showNotification(paste0("\u5df2\u5c07 ", n, " \u984c\u6062\u5fa9\u70ba\u81ea\u52d5\u5224\u5b9a"),
        type = "message", duration = 2)
    })

    # 切換 block 時同步量尺 UI
    observeEvent(current_id(), {
      cb <- cur_blk()
      if (is.null(cb)) return()
      updateNumericInput(session, "scale_n", value = cb$scale_n %||% 5L)
    }, ignoreNULL = FALSE)

    output$likert_help_ui <- renderUI({
      tags$details(
        class = "kit-likert-help mb-2",
        tags$summary(tags$strong("Likert \u5982\u4f55\u8a2d\u5b9a\uff1f")),
        tags$ol(
          tags$li("\u5de6\u5074\u9078\u5b9a\u8981\u8a2d\u5b9a\u7684\u5340\u584a\u3002"),
          tags$li("\u4e2d\u9593\u52a0\u5165 Likert \u984c\u76ee\uff08\u975e\u8907\u9078/\u958b\u653e\u984c\uff09\u3002"),
          tags$li("\u8a2d\u5b9a\u300c\u683c\u6578\u300d\uff082\u20137\uff09\u3002"),
          tags$li(tags$strong("右側 Likert 僅用於量尺題"), "：同一區塊若混有教育程度、工作狀態等單選題，會逐題讀 value，", tags$strong("不會"), "自動套用 Likert。"),
          tags$li(tags$strong("Likert 量尺題"), "（如 從未如此～總是如此）會", tags$strong("自動"), "併成矩陣表格；分類單選（教育程度等）仍逐題顯示。"),
          tags$li("也可多選後按「合併為矩陣題組」手動合併。"),
          tags$li("\u6309\u300c\u5957\u7528\u91cf\u5c3a\u81f3\u9078\u5b9a\u5340\u584a\u300d\u5132\u5b58\uff08\u6bcf\u5340\u584a\u91cf\u5c3a\u7368\u7acb\uff09\u3002"),
          tags$li("\u8981\u77e9\u9663\u8868\u683c\uff1f\u5728\u4e2d\u9593\u300c\u5df2\u9078\u984c\u76ee\u300d\u591a\u9078\u5f8c\u6309\u300c\u5408\u4f75\u70ba\u77e9\u9663\u984c\u7d44\u300d\u3002")
        ),
        tags$p(class = "text-muted small mb-0",
          "\u7de8\u78bc\u53ef\u70ba 0\u30016666\uff08\u62d2\u7b54\uff09\u7b49\uff1b\u55ae\u984c=\u683c\u5f0f A\uff08\u25a1 1 xxx\uff09\uff1b\u77e9\u9663=\u683c\u5f0f B\uff08\u8868\u683c\uff09\u3002")
      )
    })

    # ── 量尺設定 UI ───────────────────────────────────────────────
    output$scale_label_inputs_ui <- renderUI({
      n <- as.integer(input$scale_n %||% 5L)
      cb <- cur_blk()
      cur_lbls <- if (!is.null(cb)) cb$scale_labels else character(0)
      cur_cods <- if (!is.null(cb)) cb$scale_codes else character(0)

      tagList(
        tags$div(
          class = "row g-1 mb-1 text-muted small fw-bold",
          tags$div(class = "col-3", "\u7de8\u78bc"),
          tags$div(class = "col-9", "\u6a19\u7c64\u6587\u5b57")
        ),
        lapply(seq_len(n), function(i) {
          tags$div(
            class = "row g-1 align-items-center mb-1",
            tags$div(
              class = "col-3",
              textInput(
                ns(paste0("cod_", i)),
                label       = NULL,
                value       = if (i <= length(cur_cods)) cur_cods[i] else as.character(i),
                placeholder = as.character(i),
                width       = "100%"
              )
            ),
            tags$div(
              class = "col-9",
              textInput(
                ns(paste0("lbl_", i)),
                label       = NULL,
                value       = if (i <= length(cur_lbls)) cur_lbls[i] else "",
                placeholder = paste0("\u683c ", i, " \u6a19\u7c64"),
                width       = "100%"
              )
            )
          )
        }),
        tags$p(class = "text-muted small mb-0",
          "\u7de8\u78bc\u53ef\u81ea\u8a02\uff08\u5982 0\u30016666 \u8868\u793a\u62d2\u7b54\uff09\uff0c\u6703\u540c\u6642\u986f\u793a\u5728\u9810\u89bd\u8207 Word \u4e2d\u3002")
      )
    })

    # 自動從選定 block 第一題的 value 欄解析量尺標籤
    observeEvent(input$parse_scale_btn, {
      cid <- current_id(); req(cid, data_rv())
      cb <- blk$data[[cid]]
      if (length(cb$items) == 0L) {
        showNotification("\u8acb\u5148\u9078\u984c\u518d\u81ea\u52d5\u89e3\u6790",
          type = "warning"); return()
      }
      first_key <- cb$items[[1L]]
      ib <- data_rv()$itemBank
      r  <- ib[ib$itemID == first_key$itemID & ib$itemVer == first_key$itemVer, ]
      if (nrow(r) == 0L) return()

      parsed <- parse_scale_value(r$value[1L])
      if (is.null(parsed)) {
        showNotification("value \u6b04\u7121\u6cd5\u81ea\u52d5\u89e3\u6790\uff0c\u8acb\u624b\u52d5\u586b\u5beb",
          type = "warning"); return()
      }

      n <- length(parsed$labels)
      updateNumericInput(session, "scale_n", value = n)
      for (i in seq_len(n)) {
        updateTextInput(session, paste0("cod_", i), value = parsed$codes[i])
        updateTextInput(session, paste0("lbl_", i), value = parsed$labels[i])
      }
      showNotification(paste0("\u81ea\u52d5\u89e3\u6790\u5b8c\u6210\uff0c\u5171 ", n, " \u683c\uff0c\u8acb\u78ba\u8a8d\u5167\u5bb9\u5f8c\u6309\u300c\u5957\u7528\u300d"),
        type = "message", duration = 4)
    })

    # 套用量尺至目前 block
    observeEvent(input$save_scale_btn, {
      cid <- current_id(); req(cid)
      n    <- as.integer(input$scale_n %||% 5L)
      cods <- sapply(seq_len(n), function(i) {
        v <- input[[paste0("cod_", i)]]
        if (is.null(v) || !nzchar(trimws(v))) as.character(i) else trimws(v)
      })
      lbls <- sapply(seq_len(n), function(i) {
        v <- input[[paste0("lbl_", i)]]
        if (is.null(v) || nchar(trimws(v)) == 0) paste0("\u683c ", i) else trimws(v)
      })
      blk$data[[cid]]$scale_n      <- n
      blk$data[[cid]]$scale_codes  <- cods
      blk$data[[cid]]$scale_labels <- lbls
      blk$data[[cid]]$open_box_lines <- 1L
      showNotification(paste0("\u91cf\u5c3a\u8a2d\u5b9a\u5df2\u5957\u7528\u81f3\u300c",
        blk$data[[cid]]$name, "\u300d"),  # 量尺設定已套用至「X」
        type = "message", duration = 2)
    })

    # ── 問卷組裝 helper ───────────────────────────────────────────
    collect_block_configs <- function() {
      d <- data_rv()
      if (is.null(d)) stop("\u8cc7\u6599\u5c1a\u672a\u8f09\u5165\uff0c\u8acb\u5148\u5207\u63db Domain")

      ord <- isolate(blk$order)
      if (length(ord) == 0L) stop("\u8acb\u5148\u65b0\u589e\u5340\u584a")

      blk_data <- isolate(reactiveValuesToList(blk))$data
      if (is.null(blk_data)) blk_data <- list()

      cfgs <- lapply(ord, function(id) {
        cfg <- blk_data[[id]]
        if (is.null(cfg)) stop("\u5340\u584a\u8a2d\u5b9a\u7f3a\u5931\uff1a", id)
        cfg$open_box_lines <- 1L
        cfg
      })

      has_items <- any(vapply(cfgs, function(cfg) length(cfg$items) > 0L, logical(1L)))
      if (!has_items) stop("\u8acb\u81f3\u5c11\u5728\u4e00\u500b\u5340\u584a\u52a0\u5165\u984c\u76ee")

      list(data = d, configs = cfgs)
    }

    get_block_configs <- function() {
      ord <- blk$order
      lapply(ord, function(id) blk$data[[id]])
    }

    observeEvent(input$preview_btn, {
      reload_kit_export_sources()
      bslib::nav_select(ns("assemble_tabs"), "tab_preview", session = session)
      showNotification(
        paste0("\u9810\u89bd\u5df2\u66f4\u65b0\uff08", KIT_EXPORT_VERSION, "\uff09\uff0c\u5df2\u5207\u63db\u81f3\u300c2\uff5c\u7d50\u69cb\u8207\u9810\u89bd\u300d"),
        type = "message", duration = 3)
    })

    output$structure_preview_ui <- renderUI({
      input$preview_btn
      d <- data_rv()
      if (is.null(d)) {
        return(div(class = "text-muted small", "\u8cc7\u6599\u5c1a\u672a\u8f09\u5165\u3002"))
      }
      ord <- blk$order
      if (length(ord) == 0L) {
        return(div(class = "text-muted small", "\u5c1a\u712a\u5340\u584a\u3002"))
      }
      cfgs <- get_block_configs()
      outline <- build_structure_outline(cfgs, d$itemBank)
      render_structure_outline_ui(outline)
    })

    outputOptions(output, "structure_preview_ui", suspendWhenHidden = FALSE)

    send_base64_download <- function(path, filename, mime) {
      if (!file.exists(path)) stop("\u6a94\u6848\u751f\u6210\u5931\u6557")
      raw <- readBin(path, "raw", n = file.info(path)$size)
      b64 <- jsonlite::base64_enc(raw)
      session$sendCustomMessage("kit_download_base64", list(
        b64      = b64,
        filename = filename,
        mime     = mime
      ))
    }

    dl_error <- reactiveVal(NULL)

    output$dl_status_ui <- renderUI({
      err <- dl_error()
      if (is.null(err) || !nzchar(err)) return(NULL)
      tags$p(class = "text-danger small mt-2 mb-0", icon("triangle-exclamation"), " ", err)
    })

    observeEvent(input$dl_word_btn, {
      dl_error(NULL)
      tryCatch({
        ver <- reload_kit_export_sources()
        if (!requireNamespace("officer", quietly = TRUE) ||
            !requireNamespace("flextable", quietly = TRUE)) {
          stop("\u672a\u5b89\u88dd officer / flextable \u5957\u4ef6\uff0c\u7121\u6cd5\u532f\u51fa Word")
        }
        pkg <- collect_block_configs()
        tmp <- tempfile(fileext = ".docx")
        doc <- generate_questionnaire(pkg$configs, pkg$data$itemBank)
        print(doc, target = tmp)
        send_base64_download(
          tmp,
          paste0("KIT_questionnaire_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx"),
          "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
        showNotification(
          paste0("\u5df2\u4e0b\u8f09 Word\uff08", ver, "\uff09"),
          type = "message", duration = 3)
      }, error = function(e) {
        dl_error(conditionMessage(e))
        showNotification(paste0("\u532f\u51fa\u5931\u6557\uff1a", conditionMessage(e)),
          type = "error", duration = 6)
      })
    })

    # ── 組卷草稿 JSON ─────────────────────────────────────────────
    output$save_draft_btn <- downloadHandler(
      filename = function() {
        paste0("KIT_draft_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
      },
      content = function(file) {
        save_assemble_draft(blk, file)
      }
    )

    observeEvent(input$load_draft_btn, {
      req(input$load_draft_file)
      tryCatch({
        res <- load_assemble_draft(blk, input$load_draft_file$datapath)
        current_id(res$current)
        showNotification(
          paste0("\u8349\u7a3f\u5df2\u9084\u539f\uff08", length(res$order), " \u500b\u5340\u584a\uff09"),
          type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste0("\u9084\u539f\u5931\u6557\uff1a", e$message),
          type = "error", duration = 8)
      })
    })

  })
}
