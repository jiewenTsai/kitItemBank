# R/mod_dim_report.R — 功能 4：構面題項對應報告
#
# 雙模式：
#   - 預覽模式：依波次產生報告 + 快速預覽
#   - 編修模式：編修 dimMeta（alias、note）與波次專屬信度

dimReportUI <- function(id) {
  ns <- NS(id)

  tagList(
    wellPanel(
      fluidRow(
        column(3,
          selectInput(
            ns("wave_sel"),
            label   = "\u9078\u64c7\u6ce2\u6b21\uff08\u55ae\u9078\uff09",
            choices = NULL
          )
        ),
        column(3,
          br(),
          actionButton(
            ns("gen_btn"),
            label = "\u7522\u751f\u5831\u544a",
            icon  = icon("file-lines"),
            class = "btn-primary"
          )
        ),
        column(3,
          br(),
          uiOutput(ns("dl_ui"))
        ),
        column(3,
          br(),
          uiOutput(ns("status_badge_ui"))
        )
      )
    ),

    navset_tab(
      id = ns("mode_tabs"),
      nav_panel(
        title = tagList(icon("eye"), " \u9810\u89bd\u6a21\u5f0f"),
        value = "preview",
        uiOutput(ns("preview_ui"))
      ),
      nav_panel(
        title = tagList(icon("pen-to-square"), " \u7de8\u4fee dimMeta"),
        value = "edit",
        tags$p(
          class = "text-muted kit-preview-hint",
          "\u7de8\u4fee\u5f8c\u6309\u300c\u5132\u5b58\u4e26\u532f\u51fa\u984c\u5eab\u300d\uff1b",
          "\u6703\u540c\u6b65\u5beb\u56de CSV \u4e26\u7522\u751f\u56db\u8868 Excel",
          "\uff08itemBank / itemDim / itemWave / dimMeta\uff0c\u542b dimReliability\uff09\u3002",
          "\u9810\u89bd\u8207\u5831\u544a\u6703\u5373\u6642\u53cd\u6620\u7de8\u4fee\u5167\u5bb9\u3002"
        ),
        div(
          class = "d-flex align-items-center gap-2 mb-2 flex-wrap",
          actionButton(ns("save_meta_btn"), "\u5132\u5b58\u4e26\u532f\u51fa\u984c\u5eab",
            icon = icon("file-excel"), class = "btn-sm btn-success"),
          downloadButton(ns("dl_bundle_xlsx"), "\u4e0b\u8f09\u984c\u5eab .xlsx",
            class = "btn-sm btn-outline-success"),
          tags$small(class = "text-muted",
            textOutput(ns("meta_edit_hint"), inline = TRUE))
        ),
        div(
          class = "row g-2 mb-3",
          div(class = "col-md-4",
            selectizeInput(
              ns("edit_dim_sel"),
              label   = "\u9078\u64c7\u69cb\u9762\u7de8\u8f2f",
              choices = NULL,
              options = list(placeholder = "\u641c\u5c0b dimID / dimName\u2026")
            )
          ),
          div(class = "col-md-3",
            textInput(ns("edit_alias"), "alias", value = "")
          ),
          div(class = "col-md-2",
            textInput(ns("edit_rel"), "\u4fe1\u5ea6\uff08\u672c\u6ce2\u6b21\uff09", value = "")
          ),
          div(class = "col-md-3",
            textInput(ns("edit_dimName"), "dimName", value = "")
          ),
          div(class = "col-12",
            textAreaInput(ns("edit_note"), "note", value = "", rows = 2L,
              resize = "vertical")
          )
        ),
        tags$p(class = "text-muted kit-preview-hint small mb-2",
          "\u4fee\u6539\u5b57\u6bb5\u5f8c\u6703\u7acb\u5373\u53cd\u6620\u65bc\u300c\u9810\u89bd\u6a21\u5f0f\u300d\u8207\u5831\u544a\u3002"),
        div(class = "kit-table-scroll",
          DT::dataTableOutput(ns("meta_edit_dt")))
      )
    )
  )
}

dimReportServer <- function(id, data_rv, domain_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(data_rv())
      wave_cols <- get_wave_cols(data_rv()$itemWave)
      updateSelectInput(session, "wave_sel", choices = wave_cols)
    })

    report_status <- reactiveVal("idle")
    report_path   <- reactiveVal(NULL)
    report_data   <- reactiveVal(NULL)

    meta_edit_rv <- reactiveVal(NULL)
    meta_edit_seq <- reactiveVal(0L)
    form_loading  <- reactiveVal(FALSE)

    sync_dim_choices <- function(df) {
      if (is.null(df) || nrow(df) == 0L) {
        updateSelectizeInput(session, "edit_dim_sel", choices = character(0),
                             selected = character(0))
        return(invisible(NULL))
      }
      labels <- paste0(df$dimID, " \u2014 ", df$dimName)
      ch <- stats::setNames(df$dimID, labels)
      sel <- input$edit_dim_sel %||% character(0)
      if (!length(sel) || !(sel %in% df$dimID)) sel <- df$dimID[1L]
      updateSelectizeInput(session, "edit_dim_sel", choices = ch, selected = sel)
    }

    load_form_from_dim <- function(did) {
      df <- meta_edit_rv()
      req(df, nzchar(did))
      row <- df[df$dimID == did, , drop = FALSE]
      if (nrow(row) == 0L) return(invisible(NULL))
      form_loading(TRUE)
      updateTextInput(session, "edit_dimName", value = row$dimName[1L])
      updateTextInput(session, "edit_alias",   value = row$alias[1L])
      updateTextInput(session, "edit_rel",     value = row$reliability[1L])
      updateTextAreaInput(session, "edit_note", value = row$note[1L])
      form_loading(FALSE)
    }

    apply_form_to_meta <- function() {
      if (isTRUE(form_loading())) return(invisible(NULL))
      req(meta_edit_rv(), input$edit_dim_sel)
      df <- meta_edit_rv()
      idx <- match(input$edit_dim_sel, df$dimID)
      if (is.na(idx)) return(invisible(NULL))

      df$dimName[idx]     <- clean_meta_str(input$edit_dimName %||% "",
                                            default = df$dimName[idx])
      df$alias[idx]       <- clean_meta_str(input$edit_alias %||% "")
      df$reliability[idx] <- clean_meta_str(input$edit_rel %||% "")
      df$note[idx]        <- clean_meta_str(input$edit_note %||% "")

      meta_edit_rv(df)
      meta_edit_seq(meta_edit_seq() + 1L)
      invisible(df)
    }

    build_meta_edit_df <- function(wave) {
      req(data_rv())
      dm <- ensure_dim_meta_cols(data_rv()$dimMeta)
      dr <- data_rv()$dimReliability %||% data.frame()

      rel_wave <- vapply(dm$dimID, function(did) {
        val <- get_dim_reliability(did, wave, dm, dr)
        if (is.na(val)) "" else as.character(val)
      }, character(1L))

      data.frame(
        dimID       = dm$dimID,
        dimName     = as.character(dm$dimName),
        alias       = vapply(dm$alias, clean_meta_str, character(1L),
                             USE.NAMES = FALSE),
        reliability = rel_wave,
        note        = vapply(dm$note, clean_meta_str, character(1L),
                             USE.NAMES = FALSE),
        stringsAsFactors = FALSE
      )
    }

    observeEvent(input$wave_sel, {
      req(input$wave_sel, data_rv())
      df <- build_meta_edit_df(input$wave_sel)
      meta_edit_rv(df)
      sync_dim_choices(df)
    }, ignoreNULL = FALSE)

    observeEvent(data_rv(), {
      req(input$wave_sel, data_rv())
      df <- build_meta_edit_df(input$wave_sel)
      meta_edit_rv(df)
      sync_dim_choices(df)
    }, ignoreInit = TRUE)

    observeEvent(input$edit_dim_sel, {
      req(input$edit_dim_sel)
      load_form_from_dim(input$edit_dim_sel)
    }, ignoreNULL = TRUE)

    observeEvent(input$edit_dimName, apply_form_to_meta(), ignoreInit = TRUE)
    observeEvent(input$edit_alias,   apply_form_to_meta(), ignoreInit = TRUE)
    observeEvent(input$edit_rel,     apply_form_to_meta(), ignoreInit = TRUE)
    observeEvent(input$edit_note,    apply_form_to_meta(), ignoreInit = TRUE)

    output$meta_edit_hint <- renderText({
      w <- input$wave_sel %||% "\uff08\u672a\u9078\u6ce2\u6b21\uff09"
      paste0("\u76ee\u524d\u7de8\u8f2f\u6ce2\u6b21\uff1a\u300c", w,
             "\u300d\u7684\u4fe1\u5ea6\u503c")
    })

    output$meta_edit_dt <- DT::renderDataTable({
      df <- meta_edit_rv()
      req(df)

      DT::datatable(
        df,
        rownames  = FALSE,
        selection = "none",
        class     = "table table-sm table-bordered kit-items-dt",
        options   = list(
          scrollX      = TRUE,
          pageLength   = 25L,
          lengthChange = TRUE,
          dom          = "lftip",
          language     = kit_dt_lang(),
          columnDefs = list(
            list(targets = 0L, width = "14%"),
            list(targets = 1L, width = "22%"),
            list(targets = 2L, width = "18%"),
            list(targets = 3L, width = "12%"),
            list(targets = 4L, width = "34%")
          )
        )
      )
    })

    assemble_data <- function(wave, d, meta_edit_df = NULL) {
      iw <- d$itemWave
      id <- d$itemDim
      ib <- d$itemBank
      dm <- ensure_dim_meta_cols(d$dimMeta)
      dr <- d$dimReliability

      wave_items <- iw[iw[[wave]] == 1L, c("itemID", "itemVer"), drop = FALSE]
      dim_cols <- get_matrix_cols(id)

      result <- lapply(dim_cols, function(dim_col) {
        dim_items <- id[id[[dim_col]] == 1L, c("itemID", "itemVer"), drop = FALSE]

        items_in_both <- dplyr::inner_join(
          wave_items, dim_items, by = c("itemID", "itemVer"))

        if (nrow(items_in_both) == 0L) return(NULL)

        item_details <- dplyr::inner_join(
          items_in_both, ib, by = c("itemID", "itemVer"))

        item_details$label <- collapse_newlines(item_details$label)
        items_out <- pick_unique_report_items(item_details, wave_items)

        resolved <- resolve_dim_meta(dim_col, wave, dm, dr, meta_edit_df)

        list(
          dimID       = dim_col,
          dimName     = resolved$dimName,
          dimNameRaw  = resolved$dimNameRaw,
          alias       = resolved$alias,
          reliability = resolved$reliability,
          items       = items_out
        )
      })

      Filter(Negate(is.null), result)
    }

    effective_domain <- reactive({
      req(data_rv())
      d <- data_rv()
      me <- meta_edit_rv()
      w  <- input$wave_sel
      if (!is.null(me) && nzchar(w %||% ""))
        apply_meta_edit_overlay(d, me, w)
      else
        d
    })

    live_preview_data <- reactive({
      meta_edit_seq()
      req(input$wave_sel, data_rv())
      assemble_data(input$wave_sel, data_rv(), meta_edit_rv())
    })

    observeEvent(list(meta_edit_seq(), input$wave_sel, data_rv()), {
      req(input$wave_sel, data_rv())
      report_data(assemble_data(
        input$wave_sel, data_rv(), meta_edit_rv()))
    }, ignoreInit = TRUE)

    observeEvent(input$save_meta_btn, {
      apply_form_to_meta()
      req(data_rv(), input$wave_sel, meta_edit_rv())
      df <- meta_edit_rv()
      wave <- input$wave_sel

      d <- data_rv()
      dm <- ensure_dim_meta_cols(d$dimMeta)

      for (i in seq_len(nrow(df))) {
        did <- df$dimID[i]
        idx <- match(did, dm$dimID)
        if (is.na(idx)) next
        dm$dimName[idx] <- clean_meta_str(df$dimName[i], default = dm$dimName[idx])
        dm$alias[idx]   <- clean_meta_str(df$alias[i])
        dm$note[idx]    <- clean_meta_str(df$note[i])
      }

      dr <- d$dimReliability
      if (is.null(dr) || nrow(dr) == 0L) {
        dr <- data.frame(
          dimID = character(0), wave = character(0),
          reliability = numeric(0), stringsAsFactors = FALSE
        )
      }

      for (i in seq_len(nrow(df))) {
        did <- df$dimID[i]
        rel_txt <- clean_meta_str(df$reliability[i])
        dr <- dr[!(dr$dimID == did & dr$wave == wave), , drop = FALSE]
        if (nzchar(rel_txt)) {
          rel_num <- suppressWarnings(as.numeric(rel_txt))
          if (is.na(rel_num)) {
            showNotification(
              paste0("\u300c", did, "\u300d\u4fe1\u5ea6\u683c\u5f0f\u932f\u8aa4\uff1a", rel_txt),
              type = "warning", duration = 5)
            next
          }
          dr <- rbind(dr, data.frame(
            dimID = did, wave = wave, reliability = rel_num,
            stringsAsFactors = FALSE
          ))
        }
      }

      d$dimMeta <- dm
      d$dimReliability <- dr
      data_rv(d)
      meta_edit_rv(build_meta_edit_df(wave))

      report_status("idle")
      report_path(NULL)

      tryCatch({
        paths <- save_domain_from_memory(domain_rv(), d)
        showNotification(
          paste0("\u2705 \u5df2\u5132\u5b58\u984c\u5eab\uff08CSV + Excel\uff09\u2192 ",
                 basename(paths$xlsx)),
          type = "message", duration = 6)
      }, error = function(e) {
        showNotification(paste0("\u274c \u5132\u5b58\u5931\u6557\uff1a", e$message),
          type = "error", duration = 8)
      })
    })

    output$dl_bundle_xlsx <- downloadHandler(
      filename = function() {
        paste0("KIT_", domain_rv(), "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        req(effective_domain())
        export_domain_xlsx(effective_domain(), file)
      }
    )

    observeEvent(input$gen_btn, {
      apply_form_to_meta()
      req(input$wave_sel, data_rv())

      report_status("running")
      report_path(NULL)
      report_data(NULL)

      wave <- input$wave_sel
      dom  <- domain_rv()
      d    <- data_rv()

      rd <- tryCatch(
        assemble_data(wave, d, meta_edit_rv()),
        error = function(e) {
          report_status("error")
          showNotification(paste0("\u8cc7\u6599\u7d44\u88dd\u5931\u6557\uff1a", e$message),
                           type = "error", duration = 8)
          NULL
        }
      )
      if (is.null(rd)) return()
      report_data(rd)

      if (length(rd) == 0L) {
        report_status("idle")
        showNotification(
          paste0("\u6ce2\u6b21\u300c", wave, "\u300d\u4e0b\u627e\u4e0d\u5230\u4efb\u4f55\u69cb\u9762\u984c\u76ee\uff0c\u8acb\u78ba\u8a8d\u8cc7\u6599\u3002"),
          type = "warning", duration = 6)
        return()
      }

      payload <- list(domain = dom, wave = wave, dims = rd)
      tmp_docx <- tempfile(fileext = ".docx")

      tryCatch({
        generate_dim_report_docx(payload, tmp_docx)
        report_path(tmp_docx)
        report_status("done")
        showNotification(
          paste0("\u2705 Word \u5831\u544a\u7522\u751f\u5b8c\u6210\uff01\u5171 ", length(rd), " \u500b\u69cb\u9762\u3002"),
          type = "message", duration = 4)
      }, error = function(e) {
        report_status("error")
        showNotification(
          paste0("\u274c \u5831\u544a\u7522\u751f\u5931\u6557\uff1a", e$message),
          type = "error", duration = 10)
      })
    })

    output$status_badge_ui <- renderUI({
      switch(report_status(),
        idle    = tags$span(class = "badge bg-secondary", "\u5c1a\u672a\u7522\u751f"),
        running = tags$span(class = "badge bg-warning text-dark",
                    icon("spinner", class = "fa-spin"), " \u7522\u751f\u4e2d\u2026"),
        done    = tags$span(class = "badge bg-success",
                    icon("check"), " \u5b8c\u6210"),
        error   = tags$span(class = "badge bg-danger",
                    icon("exclamation-triangle"), " \u932f\u8aa4")
      )
    })

    output$dl_ui <- renderUI({
      req(report_status() == "done", report_path())
      downloadButton(
        ns("dl_report"),
        label = "\u4e0b\u8f09 Word \u5831\u544a",
        icon  = icon("file-word"),
        class = "btn-outline-primary btn-sm"
      )
    })

    output$dl_report <- downloadHandler(
      filename = function() {
        paste0(
          "\u69cb\u9762\u5831\u544a",
          "_", domain_rv(),
          "_", input$wave_sel,
          "_", Sys.Date(), ".docx"
        )
      },
      content = function(file) {
        req(report_path())
        file.copy(report_path(), file, overwrite = TRUE)
      }
    )

    output$preview_ui <- renderUI({
      meta_edit_seq()
      req(input$wave_sel, data_rv())
      rd <- assemble_data(input$wave_sel, data_rv(), meta_edit_rv())
      if (length(rd) == 0L) {
        return(tags$p(class = "text-muted kit-preview-hint",
          paste0("\u6ce2\u6b21\u300c", input$wave_sel,
                 "\u300d\u76ee\u524d\u7121\u69cb\u9762\u984c\u76ee\uff0c\u6216\u5c1a\u672a\u8f09\u5165\u8cc7\u6599\u3002")))
      }

      tagList(
        div(class = "kit-dim-preview",
          tags$p(class = "kit-preview-hint mb-2",
            paste0("\u5feb\u901f\u9810\u89bd\uff1a\u300c", input$wave_sel,
                   "\u300d\u6ce2\u6b21 \u00d7 \u5171 ", length(rd),
                   " \u500b\u69cb\u9762\uff08\u542b\u7de8\u4fee\u4e2d\u7684 alias / \u4fe1\u5ea6\uff09")),
          lapply(rd, function(dim_info) {
            items_df <- dim_info$items

            rel_text <- if (!is.na(dim_info$reliability) &&
                            !is.null(dim_info$reliability)) {
              sprintf("\u03b1 = %.3f", as.numeric(dim_info$reliability))
            } else {
              "\u4fe1\u5ea6\u5f85\u586b"
            }

            alias_txt <- clean_meta_str(dim_info$alias)
            dim_raw   <- clean_meta_str(dim_info$dimNameRaw,
                                        default = dim_info$dimID %||% "")
            dim_title <- format_dim_report_heading(dim_raw, alias_txt)

            tbl_html <- tags$table(
              class = "table table-sm table-bordered kit-items-dt",
              tags$thead(
                tags$tr(
                  tags$th(style = "width:18%", "itemID"),
                  tags$th(style = "width:82%", "\u984c\u76ee")
                )
              ),
              tags$tbody(
                lapply(seq_len(nrow(items_df)), function(i) {
                  tags$tr(
                    tags$td(items_df$itemID[i]),
                    tags$td(items_df$label[i])
                  )
                })
              )
            )

            tags$details(
              class = "mb-2 kit-dim-block",
              tags$summary(
                tags$span(class = "kit-dim-title", dim_title),
                tags$span(class = "kit-badge kit-badge-rel", rel_text),
                tags$span(class = "kit-badge kit-badge-count",
                          paste0(nrow(items_df), " \u984c"))
              ),
              div(class = "mt-1", tbl_html)
            )
          })
        )
      )
    })

    outputOptions(output, "preview_ui", suspendWhenHidden = FALSE)
  })
}
