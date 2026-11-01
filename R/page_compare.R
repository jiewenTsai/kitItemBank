# R/page_compare.R — 比較：兩個資料檔放在一起，只列不一樣的地方
#
# 由上往下：這一頁做什麼 → 條件（左 1/3：資料檔 A、B、要看哪些差異）｜右 2/3：
#   沒選好：提示
#   選好了：「差異（n 處）」→ A、B 是哪兩個檔 → 各種差異幾處 → 下載 Excel
#   → 差異表（點一列 = 到題項查詢）
# → ▸ 用 R 怎麼做
# 只讀 kit；比較是即時的，沒有「執行」按鈕。計算都在 R/compare.R。

#' Compare page UI
#'
#' @param id Module id.
#' @return The page UI.
#' @export
page_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kitDesign::kit_tab_guide(
      # 原文：把兩個資料檔放在一起比，只列不一樣的地方：哪些變項只有一邊有、哪一題換了版本、位置或選項。
      goal = "\u628a\u5169\u500b\u8cc7\u6599\u6a94\u653e\u5728\u4e00\u8d77\u6bd4\uff0c\u53ea\u5217\u4e0d\u4e00\u6a23\u7684\u5730\u65b9\uff1a\u54ea\u4e9b\u8b8a\u9805\u53ea\u6709\u4e00\u908a\u6709\u3001\u54ea\u4e00\u984c\u63db\u4e86\u7248\u672c\u3001\u4f4d\u7f6e\u6216\u9078\u9805\u3002",
      steps = c(
        # 原文：在左邊選資料檔 A 和 B（例如同一個世代相鄰兩次施測的家長問卷）。
        "\u5728\u5de6\u908a\u9078\u8cc7\u6599\u6a94 A \u548c B\uff08\u4f8b\u5982\u540c\u4e00\u500b\u4e16\u4ee3\u76f8\u9130\u5169\u6b21\u65bd\u6e2c\u7684\u5bb6\u9577\u554f\u5377\uff09\u3002",
        # 原文：右邊列出兩個檔不一樣的地方；左邊可以只勾想看的差異。
        "\u53f3\u908a\u5217\u51fa\u5169\u500b\u6a94\u4e0d\u4e00\u6a23\u7684\u5730\u65b9\uff1b\u5de6\u908a\u53ef\u4ee5\u53ea\u52fe\u60f3\u770b\u7684\u5dee\u7570\u3002",
        # 原文：點一題，到「題項查詢」看這一題在各波次怎麼問。
        "\u9ede\u4e00\u984c\uff0c\u5230\u300c\u984c\u9805\u67e5\u8a62\u300d\u770b\u9019\u4e00\u984c\u5728\u5404\u6ce2\u6b21\u600e\u9ebc\u554f\u3002",
        # 原文：按「下載差異清單」存成 Excel。
        "\u6309\u300c\u4e0b\u8f09\u5dee\u7570\u6e05\u55ae\u300d\u5b58\u6210 Excel\u3002"
      ),
      # 原文：兩個資料檔的差異清單；差異清單（Excel）。
      output = "\u5169\u500b\u8cc7\u6599\u6a94\u7684\u5dee\u7570\u6e05\u55ae\uff1b\u5dee\u7570\u6e05\u55ae\uff08Excel\uff09\u3002",
      # 原文：這一頁只看不改。一樣的題目不會列出來。
      note = "\u9019\u4e00\u9801\u53ea\u770b\u4e0d\u6539\u3002\u4e00\u6a23\u7684\u984c\u76ee\u4e0d\u6703\u5217\u51fa\u4f86\u3002"
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectizeInput(
          # 原文：資料檔 A
          ns("a"), "\u8cc7\u6599\u6a94 A", choices = NULL, width = "100%",
          # 原文：搜尋資料檔（代號、施測、填答者）…
          options = list(placeholder = "\u641c\u5c0b\u8cc7\u6599\u6a94\uff08\u4ee3\u865f\u3001\u65bd\u6e2c\u3001\u586b\u7b54\u8005\uff09\u2026",
                         searchField = c("label", "value"))
        ),
        shiny::selectizeInput(
          # 原文：資料檔 B
          ns("b"), "\u8cc7\u6599\u6a94 B", choices = NULL, width = "100%",
          # 原文：搜尋資料檔（代號、施測、填答者）…
          options = list(placeholder = "\u641c\u5c0b\u8cc7\u6599\u6a94\uff08\u4ee3\u865f\u3001\u65bd\u6e2c\u3001\u586b\u7b54\u8005\uff09\u2026",
                         searchField = c("label", "value"))
        ),
        # 原文：A、B 對調
        shiny::div(class = "kit-buttons", kitDesign::kit_button(ns("swap"), "A\u3001B \u5c0d\u8abf")),
        shiny::checkboxGroupInput(
          # 原文：要看的差異
          ns("types"), "\u8981\u770b\u7684\u5dee\u7570",
          choices = stats::setNames(names(COMPARE_TYPES), unname(COMPARE_TYPES)),
          selected = names(COMPARE_TYPES)
        )
      ),
      shiny::column(8, shiny::uiOutput(ns("result")))
    ),
    kitDesign::kit_details_row(kitDesign::kit_code_panel_ui(ns("code")))
  )
}

#' Compare page server
#'
#' @param id Module id.
#' @param kit The kit (17 tables): a reactive, or a plain list.
#' @param goto Switch to another tab: `goto("search", item = "cogc01", ver = 1, uid = "P.cogc01")`.
#' @param request Optional reactive with requests from other tabs:
#'   `list(tab = "compare", args = list(a = "M03M036-P", b = "M03M048-P"))`.
#' @return Invisibly, a list of reactives (`a`, `b`, `diff`).
#' @export
page_compare_server <- function(id, kit, goto, request = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    kit_now <- if (shiny::is.reactive(kit)) kit else function() kit

    choices <- shiny::reactive({
      k <- kit_now()
      ds <- k$datasets
      ord <- rel_admin_order(k)[ds$adminID]
      ds <- ds[order(ord, ds$datasetID), , drop = FALSE]
      data.frame(value = ds$datasetID,
                 label = paste0(compare_dataset_label(k, ds$datasetID), " ", ds$datasetID),
                 stringsAsFactors = FALSE)
    })
    push <- function(id_in, sel) {
      ch <- shiny::isolate(choices())
      shiny::updateSelectizeInput(session, id_in, choices = stats::setNames(ch$value, ch$label),
                                  selected = sel %||% "", server = FALSE)
    }
    shiny::observe({
      choices()
      push("a", shiny::isolate(input$a))
      push("b", shiny::isolate(input$b))
    })
    shiny::observeEvent(input$swap, {
      a <- input$a; b <- input$b
      push("a", b)
      push("b", a)
    })
    if (!is.null(request)) {
      shiny::observeEvent(request(), {
        r <- request()
        if (!identical(r$tab, "compare")) return()
        if (!is.null(r$args$a)) push("a", r$args$a)
        if (!is.null(r$args$b)) push("b", r$args$b)
      })
    }

    pick <- shiny::reactive({
      ids <- kit_now()$datasets$datasetID
      a <- input$a %||% ""
      b <- input$b %||% ""
      list(a = if (a %in% ids) a else "", b = if (b %in% ids) b else "")
    })
    diff <- shiny::reactive({
      p <- pick()
      shiny::req(nzchar(p$a), nzchar(p$b), p$a != p$b)
      compare_datasets(kit_now(), p$a, p$b)
    })
    shown <- shiny::reactive({
      d <- diff()
      d[d$type %in% (input$types %||% character(0)), , drop = FALSE]
    })

    output$result <- shiny::renderUI({
      p <- pick()
      if (!nzchar(p$a) || !nzchar(p$b)) {
        return(shiny::tagList(
          # 原文：還沒選好資料檔
          shiny::h4("\u9084\u6c92\u9078\u597d\u8cc7\u6599\u6a94"),
          # 原文：在左邊選兩個資料檔（例如同一個世代相鄰兩次施測的家長問卷），這裡會列出它們不一樣的地方。
          shiny::p(class = "kit-hint", "\u5728\u5de6\u908a\u9078\u5169\u500b\u8cc7\u6599\u6a94\uff08\u4f8b\u5982\u540c\u4e00\u500b\u4e16\u4ee3\u76f8\u9130\u5169\u6b21\u65bd\u6e2c\u7684\u5bb6\u9577\u554f\u5377\uff09\uff0c\u9019\u88e1\u6703\u5217\u51fa\u5b83\u5011\u4e0d\u4e00\u6a23\u7684\u5730\u65b9\u3002")
        ))
      }
      if (p$a == p$b) {
        # 原文：A 和 B 是同一個資料檔，請選兩個不一樣的。
        return(shiny::p(class = "kit-hint", "A \u548c B \u662f\u540c\u4e00\u500b\u8cc7\u6599\u6a94\uff0c\u8acb\u9078\u5169\u500b\u4e0d\u4e00\u6a23\u7684\u3002"))
      }
      k <- kit_now()
      d <- diff()
      n <- table(factor(d$type, levels = names(COMPARE_TYPES)))
      n <- n[n > 0L]
      shiny::tagList(
        # 原文：差異（ |  處）
        shiny::h4(paste0("\u5dee\u7570\uff08", kitDesign::kit_num(nrow(d)), " \u8655\uff09")),
        # 原文：A： | （ | ）
        shiny::p(shiny::tags$strong("A\uff1a"), paste0(compare_dataset_label(k, p$a), "\uff08", p$a, "\uff09")),
        # 原文：B： | （ | ）
        shiny::p(shiny::tags$strong("B\uff1a"), paste0(compare_dataset_label(k, p$b), "\uff08", p$b, "\uff09")),
        if (nrow(d) == 0L) {
          # 原文：兩個資料檔問的題目和問法都一樣。
          shiny::p("\u5169\u500b\u8cc7\u6599\u6a94\u554f\u7684\u984c\u76ee\u548c\u554f\u6cd5\u90fd\u4e00\u6a23\u3002")
        } else {
          shiny::tags$ul(lapply(names(n), function(t) shiny::tags$li(paste0(
            # 原文：： |  處
            COMPARE_TYPES[[t]], "\uff1a", kitDesign::kit_num(n[[t]]), " \u8655"))))
        },
        shiny::uiOutput(ns("downloads")),
        if (nrow(d) > 0L) shiny::tagList(
          # 原文：差異表
          shiny::h4("\u5dee\u7570\u8868"),
          # 原文：點一列，到「題項查詢」看這一題。表裡只列左邊勾的差異。
          shiny::p(class = "kit-hint", "\u9ede\u4e00\u5217\uff0c\u5230\u300c\u984c\u9805\u67e5\u8a62\u300d\u770b\u9019\u4e00\u984c\u3002\u8868\u88e1\u53ea\u5217\u5de6\u908a\u52fe\u7684\u5dee\u7570\u3002"),
          kitDesign::kit_table_scroll(DT::DTOutput(ns("diff_dt")))
        )
      )
    })

    output$diff_dt <- DT::renderDT({
      d <- shown()
      cells <- compare_cells(d)
      df <- data.frame(d$type_label, compare_var_cell(d), d$label, cells$a, cells$b,
                       stringsAsFactors = FALSE)
      # 原文：差異 | 變項 | 題目
      names(df) <- c("\u5dee\u7570", "\u8b8a\u9805", "\u984c\u76ee", "A", "B")
      kitDesign::kit_dt(df, selection = "single", page_length = 25L, search = TRUE,
                        # 原文：題目 | 題目
                        wrap_cols = c("\u984c\u76ee", "A", "B"), min_widths = c("\u984c\u76ee" = "12rem"))
    })
    shiny::observeEvent(input$diff_dt_rows_selected, {
      i <- input$diff_dt_rows_selected
      d <- shown()
      if (length(i) != 1L || i > nrow(d)) return()
      DT::selectRows(DT::dataTableProxy("diff_dt"), NULL)
      uid <- if (nzchar(d$item_uid_a[i])) d$item_uid_a[i] else d$item_uid_b[i]
      ver <- if (nzchar(d$item_uid_a[i])) d$itemVer_a[i] else d$itemVer_b[i]
      if (!nzchar(uid)) {
        # 原文：「
        kitDesign::kit_notify(paste0("\u300c", compare_var_cell(d[i, , drop = FALSE]),
                                     # 原文：」沒有對到題目（系統變項或整個檔的差異）。
                                     "\u300d\u6c92\u6709\u5c0d\u5230\u984c\u76ee\uff08\u7cfb\u7d71\u8b8a\u9805\u6216\u6574\u500b\u6a94\u7684\u5dee\u7570\uff09\u3002"), session)
        return()
      }
      goto("search", item = sub("^[A-Za-z]+\\.", "", uid), ver = cb_int(ver), uid = uid)
    })

    output$downloads <- shiny::renderUI({
      p <- pick()
      shiny::req(nzchar(p$a), nzchar(p$b), p$a != p$b)
      k <- kit_now()
      day <- format(kit_today(), "%Y%m%d")
      shiny::div(
        class = "kit-buttons",
        kitDesign::kit_download_button(
          # 原文：下載差異清單（Excel）
          session, "dl_xlsx", "\u4e0b\u8f09\u5dee\u7570\u6e05\u55ae\uff08Excel\uff09",
          # 原文：差異清單_
          paste0("\u5dee\u7570\u6e05\u55ae_", p$a, "_", p$b, "_", day, ".xlsx"),
          function(file) writexl::write_xlsx(compare_workbook(k, p$a, p$b, diff()), file),
          class = kitDesign::kit_btn_class("secondary")
        )
      )
    })

    kitDesign::kit_code_panel_server("code", shiny::reactive({
      p <- pick()
      ids <- kit_now()$datasets$datasetID
      compare_code(if (nzchar(p$a)) p$a else ids[1], if (nzchar(p$b)) p$b else ids[2])
    }))

    invisible(list(a = shiny::reactive(pick()$a), b = shiny::reactive(pick()$b), diff = diff))
  })
}

# 用 R 怎麼做
compare_code <- function(a, b) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", as.character(x), fixed = TRUE), "\"")
  paste(c(
    "kit <- kitItemBank::read_kit()",
    "",
    # 原文：# 兩個資料檔不一樣的地方：一列一處
    "# \u5169\u500b\u8cc7\u6599\u6a94\u4e0d\u4e00\u6a23\u7684\u5730\u65b9\uff1a\u4e00\u5217\u4e00\u8655",
    paste0("d <- kitItemBank::compare_datasets(kit, ", q(a), ", ", q(b), ")"),
    "table(d$type_label)",
    "",
    # 原文：# 下載的差異清單（Excel，第一張是說明）
    "# \u4e0b\u8f09\u7684\u5dee\u7570\u6e05\u55ae\uff08Excel\uff0c\u7b2c\u4e00\u5f35\u662f\u8aaa\u660e\uff09",
    paste0("writexl::write_xlsx(kitItemBank::compare_workbook(kit, ", q(a), ", ", q(b), "), ",
           # 原文：差異清單_
           q(paste0("\u5dee\u7570\u6e05\u55ae_", a, "_", b, ".xlsx")), ")")
  ), collapse = "\n")
}
