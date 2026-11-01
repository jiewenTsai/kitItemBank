# R/page_dims.R — 構面查詢（只看，不改；構面資訊在「題庫編修」改）
#
# 由上往下：這一頁做什麼 → 條件（左 1/3：構面、波次）｜右 2/3：
#   沒選構面：構面清單（點一列 = 選這個構面）
#   選了構面：[回到構面清單] 改這個構面 → ；「睡眠習慣」：12 題（18 個版本）· 出現在 18 個波次
#   → 一行提示（只列已歸類的題，用關鍵字可能找到更多 →）→ 題目 × 波次 ✓ 表
#   → ▸ 和其他構面共用的題目 → 各波次信度 → 網路圖
# → 還差 → [產生構面報告] → 狀態框 → 下載 Word／HTML → ▸ 沒有信度的構面 ▸ 用 R 怎麼做
# 構面報告：選了構面就只做這個構面；沒選構面就做這個波次的全部構面（設定行寫明範圍）。
# 計算都在 R/dims.R、R/network.R；報告檔在 R/report_dim_docx.R、R/report_dim_html.R。

# 原文：產生構面報告
DIMS_RUN_LABEL <- "\u7522\u751f\u69cb\u9762\u5831\u544a"

page_dims_ui <- function(id) {
  ns <- shiny::NS(id)
  restored <- function(name) shiny::restoreInput(ns(name), NULL)
  pick <- function(name, label, placeholder) {
    sel <- restored(name)
    shiny::selectizeInput(
      ns(name), label, choices = c("", sel), selected = sel %||% "", width = "100%",
      options = list(placeholder = placeholder, searchField = list("label", "value"))
    )
  }
  kitDesign::kit_run_tab_ui(
    id, DIMS_RUN_LABEL,
    guide = kitDesign::kit_tab_guide(
      # 原文：看一個構面有哪些題、各波次的信度，產出構面報告。
      goal = "\u770b\u4e00\u500b\u69cb\u9762\u6709\u54ea\u4e9b\u984c\u3001\u5404\u6ce2\u6b21\u7684\u4fe1\u5ea6\uff0c\u7522\u51fa\u69cb\u9762\u5831\u544a\u3002",
      steps = c(
        # 原文：在左邊搜尋一個構面，或在右邊的構面清單點一列。
        "\u5728\u5de6\u908a\u641c\u5c0b\u4e00\u500b\u69cb\u9762\uff0c\u6216\u5728\u53f3\u908a\u7684\u69cb\u9762\u6e05\u55ae\u9ede\u4e00\u5217\u3002",
        # 原文：看它的題目 × 波次表、各波次信度和網路圖。
        "\u770b\u5b83\u7684\u984c\u76ee \u00d7 \u6ce2\u6b21\u8868\u3001\u5404\u6ce2\u6b21\u4fe1\u5ea6\u548c\u7db2\u8def\u5716\u3002",
        # 原文：點一題（表或網路圖都可以），到「題項查詢」看這一題的細節。
        "\u9ede\u4e00\u984c\uff08\u8868\u6216\u7db2\u8def\u5716\u90fd\u53ef\u4ee5\uff09\uff0c\u5230\u300c\u984c\u9805\u67e5\u8a62\u300d\u770b\u9019\u4e00\u984c\u7684\u7d30\u7bc0\u3002",
        # 原文：要構面報告：在左邊選一個波次，按「產生構面報告」。選了構面就只做這個構面；沒選構面就做這個波次的全部構面。
        "\u8981\u69cb\u9762\u5831\u544a\uff1a\u5728\u5de6\u908a\u9078\u4e00\u500b\u6ce2\u6b21\uff0c\u6309\u300c\u7522\u751f\u69cb\u9762\u5831\u544a\u300d\u3002\u9078\u4e86\u69cb\u9762\u5c31\u53ea\u505a\u9019\u500b\u69cb\u9762\uff1b\u6c92\u9078\u69cb\u9762\u5c31\u505a\u9019\u500b\u6ce2\u6b21\u7684\u5168\u90e8\u69cb\u9762\u3002",
        # 原文：看狀態框有沒有要注意的事，再下載 Word 或 HTML。
        "\u770b\u72c0\u614b\u6846\u6709\u6c92\u6709\u8981\u6ce8\u610f\u7684\u4e8b\uff0c\u518d\u4e0b\u8f09 Word \u6216 HTML\u3002"
      ),
      # 原文：一個構面的題目與信度；構面報告（Word、HTML）：一個構面，或一個波次的全部構面。
      output = "\u4e00\u500b\u69cb\u9762\u7684\u984c\u76ee\u8207\u4fe1\u5ea6\uff1b\u69cb\u9762\u5831\u544a\uff08Word\u3001HTML\uff09\uff1a\u4e00\u500b\u69cb\u9762\uff0c\u6216\u4e00\u500b\u6ce2\u6b21\u7684\u5168\u90e8\u69cb\u9762\u3002",
      # 原文：這一頁只看不改。構面名稱、別名、信度在「題庫編修」改。
      note = "\u9019\u4e00\u9801\u53ea\u770b\u4e0d\u6539\u3002\u69cb\u9762\u540d\u7a31\u3001\u5225\u540d\u3001\u4fe1\u5ea6\u5728\u300c\u984c\u5eab\u7de8\u4fee\u300d\u6539\u3002"
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        # 原文：構面 | 搜尋構面（名稱、別名）…
        pick("dim", "\u69cb\u9762", "\u641c\u5c0b\u69cb\u9762\uff08\u540d\u7a31\u3001\u5225\u540d\uff09\u2026"),
        # 原文：波次（構面報告用） | 選一個波次
        pick("wave", "\u6ce2\u6b21\uff08\u69cb\u9762\u5831\u544a\u7528\uff09", "\u9078\u4e00\u500b\u6ce2\u6b21")
      ),
      shiny::column(8, shiny::uiOutput(ns("browse")))
    ),
    downloads = TRUE, details = TRUE, code = TRUE
  )
}

#' @param store bank_store_server() 的回傳
#' @param goto 切到別的分頁：goto("search", item = "…", ver = 1)
#' @param request 別的分頁送來的請求（reactive，list(tab, args, at)）
#' @noRd
page_dims_server <- function(id, store, goto, request) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    bank <- shiny::reactive(store$bank())
    overview <- shiny::reactive(dim_overview(bank()))
    current_dim <- shiny::reactiveVal(NULL)

    select_dim <- function(d) {
      current_dim(d)
      shiny::updateSelectizeInput(session, "dim", selected = d %||% "")
    }

    # ── 條件：構面、波次的選項跟著題庫 ──────────────────────
    shiny::observe({
      ov <- overview()
      cur <- shiny::isolate(current_dim() %||% input$dim)
      if (length(cur) != 1L || !cur %in% ov$dimID) cur <- ""
      if (!nzchar(cur) && !is.null(shiny::isolate(current_dim()))) current_dim(NULL)
      shiny::updateSelectizeInput(
        session, "dim",
        choices = stats::setNames(c("", ov$dimID), c("", dims_label(bank(), ov$dimID))),
        selected = cur
      )
    })
    shiny::observe({
      waves <- get_wave_cols(bank()$itemWave)
      cur <- shiny::isolate(input$wave) %||% ""
      if (!cur %in% waves) cur <- ""
      shiny::updateSelectizeInput(session, "wave", choices = c("", make_col_choices(waves)),
                                  selected = cur)
    })
    shiny::observeEvent(input$dim, {
      d <- input$dim
      if (!nzchar(d %||% "")) d <- NULL
      if (!identical(d, current_dim())) current_dim(d)
    }, ignoreNULL = FALSE)

    # 別的分頁送來的「看這個構面」
    shiny::observeEvent(request(), {
      r <- request()
      if (!identical(r$tab, "dims")) return()
      d <- r$args$dim
      if (is.null(d)) return()
      if (d %in% dims_cols(bank())) {
        select_dim(d)
      } else {
        # 原文：這個題庫沒有構面「 | 」。
        kitDesign::kit_notify(paste0("\u9019\u500b\u984c\u5eab\u6c92\u6709\u69cb\u9762\u300c", d, "\u300d\u3002"), session)
      }
    })

    # ── 右邊：構面清單，或一個構面 ────────────────────────
    output$browse <- shiny::renderUI({
      d <- current_dim()
      ov <- overview()
      if (is.null(d) || !d %in% ov$dimID) {
        return(shiny::tagList(
          # 原文：構面清單（ |  個）
          shiny::h4(paste0("\u69cb\u9762\u6e05\u55ae\uff08", kitDesign::kit_num(nrow(ov)), " \u500b\uff09")),
          # 原文：點一列，看這個構面的題目、各波次信度和網路圖。
          shiny::p(class = "kit-hint", "\u9ede\u4e00\u5217\uff0c\u770b\u9019\u500b\u69cb\u9762\u7684\u984c\u76ee\u3001\u5404\u6ce2\u6b21\u4fe1\u5ea6\u548c\u7db2\u8def\u5716\u3002"),
          kitDesign::kit_table_scroll(DT::DTOutput(ns("overview_dt")))
        ))
      }
      row <- ov[ov$dimID == d, , drop = FALSE]
      kids <- if (row$n_items == 0L) dim_children(bank(), d, store$domain()) else character(0)
      shiny::tagList(
        # 先放按鈕列再放標題：提示小字的負上邊距只會貼近標題，不會壓到按鈕
        shiny::div(
          class = "kit-buttons",
          # 原文：回到構面清單
          kitDesign::kit_button(ns("back"), "\u56de\u5230\u69cb\u9762\u6e05\u55ae"),
          # 原文：下載各波次題目表（Excel） | 題組各波次題目_
          kitDesign::kit_download_button(
            session, "dl_dim_xlsx", "\u4e0b\u8f09\u5404\u6ce2\u6b21\u984c\u76ee\u8868\uff08Excel\uff09",
            filename = paste0("\u984c\u7d44\u5404\u6ce2\u6b21\u984c\u76ee_",
                              gsub("[\\/:*?\"<>|[:space:]]+", "_", dims_label(bank(), d)), ".xlsx"),
            content = function(file) {
              b <- shiny::isolate(bank())
              writexl::write_xlsx(dim_items_workbook(b, d, store$domain()), file)
            },
            class = kitDesign::kit_btn_class("secondary")
          ),
          # 原文：改這個構面 →
          shiny::actionLink(ns("edit_dim"), "\u6539\u9019\u500b\u69cb\u9762 \u2192")
        ),
        # 原文：「 | 」：
        shiny::h4(paste0("\u300c", dims_label(bank(), d), "\u300d\uff1a", dims_count_text(row))),
        if (length(kids)) dims_children_line(bank(), kids, ns("child_dim")) else shiny::tagList(
          shiny::p(
            class = "kit-hint",
            # 原文：點一題，到「題項查詢」看細節。這裡只列已歸到這個構面的題，
            "\u9ede\u4e00\u984c\uff0c\u5230\u300c\u984c\u9805\u67e5\u8a62\u300d\u770b\u7d30\u7bc0\u3002\u9019\u88e1\u53ea\u5217\u5df2\u6b78\u5230\u9019\u500b\u69cb\u9762\u7684\u984c\uff0c",
            # 原文：用關鍵字可能找到更多 →
            shiny::actionLink(ns("to_search"), "\u7528\u95dc\u9375\u5b57\u53ef\u80fd\u627e\u5230\u66f4\u591a \u2192")
          ),
          kitDesign::kit_table_scroll(DT::DTOutput(ns("items_dt")))
        ),
        kitDesign::kit_details_row(shiny::uiOutput(ns("overlap_ui"))),
        # 題組沿革（kit 的 group_lineage；這一頁其他部分讀 0.2 形狀的題庫）
        group_lineage_ui(kit_builtin(), dims_label(bank(), d)),
        # 原文：各波次信度
        shiny::h4("\u5404\u6ce2\u6b21\u4fe1\u5ea6"),
        shiny::uiOutput(ns("rel_ui")),
        # 原文：網路圖
        shiny::h4("\u7db2\u8def\u5716"),
        # 原文：也畫這些題目的其他構面
        shiny::checkboxInput(ns("other_dims"), "\u4e5f\u756b\u9019\u4e9b\u984c\u76ee\u7684\u5176\u4ed6\u69cb\u9762",
                             value = isTRUE(shiny::isolate(input$other_dims))),
        visNetwork::visNetworkOutput(ns("network"), height = "520px"),
        shiny::uiOutput(ns("network_hint"))
      )
    })

    output$overview_dt <- DT::renderDT({
      ov <- overview()
      df <- data.frame(
        # 原文：構面 | 別名 | 題數 | 版本數
        "\u69cb\u9762" = ov$dimName, "\u5225\u540d" = ov$alias, "\u984c\u6578" = ov$n_items, "\u7248\u672c\u6578" = ov$n_versions,
        # 原文：出現的波次 | 最近一波
        "\u51fa\u73fe\u7684\u6ce2\u6b21" = ov$n_waves, "\u6700\u8fd1\u4e00\u6ce2" = ifelse(is.na(ov$last_wave), "", ov$last_wave),
        # 原文：有信度的波次
        "\u6709\u4fe1\u5ea6\u7684\u6ce2\u6b21" = ov$n_rel,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      # 原文：構面
      kitDesign::kit_dt(df, selection = "single", page_length = 20L, wrap_cols = "\u69cb\u9762")
    })
    shiny::observeEvent(input$overview_dt_rows_selected, {
      i <- input$overview_dt_rows_selected
      ov <- overview()
      if (length(i) == 1L && i <= nrow(ov)) select_dim(ov$dimID[i])
    })
    shiny::observeEvent(input$back, select_dim(NULL))
    # 點子題組的連結 = 選那個構面
    shiny::observeEvent(input$child_dim, {
      d <- input$child_dim
      if (length(d) == 1L && d %in% dims_cols(bank())) select_dim(d)
    })
    shiny::observeEvent(input$edit_dim, {
      shiny::req(current_dim())
      goto("edit", dim = current_dim())
    })
    shiny::observeEvent(input$to_search, {
      shiny::req(current_dim())
      # keyword：題項查詢支援時會帶入構面名稱當關鍵字；不支援時只是切到題項查詢
      goto("search", keyword = dims_meta(bank(), current_dim())$dimName)
    })

    items <- shiny::reactive({
      shiny::req(current_dim() %in% dims_cols(bank()))
      dim_items(bank(), current_dim())
    })
    output$items_dt <- DT::renderDT({
      it <- items()
      # 原文：已停用
      it$deprecated <- ifelse(is.na(it$deprecated), "", ifelse(it$deprecated == 1L, "\u5df2\u505c\u7528", ""))
      kitDesign::kit_dt(it, selection = "single", page_length = 20L,
                        wrap_cols = "label", min_widths = c(label = "18rem"))
    })
    shiny::observeEvent(input$items_dt_rows_selected, {
      i <- input$items_dt_rows_selected
      it <- items()
      if (length(i) != 1L || i > nrow(it)) return()
      DT::selectRows(DT::dataTableProxy("items_dt"), NULL)
      goto("search", item = it$itemID[i], ver = it$itemVer[i])
    })

    output$rel_ui <- shiny::renderUI({
      shiny::req(current_dim() %in% dims_cols(bank()))
      rel <- dim_reliability_by_wave(bank(), current_dim())
      n_rel <- sum(!is.na(rel$reliability))
      if (n_rel == 0L) {
        # 原文：這個構面還沒有信度資料。
        return(shiny::p(class = "kit-hint", "\u9019\u500b\u69cb\u9762\u9084\u6c92\u6709\u4fe1\u5ea6\u8cc7\u6599\u3002"))
      }
      shiny::tagList(
        shiny::p(class = "kit-hint", paste0(
          # 原文： 個波次裡， |  個有信度。
          kitDesign::kit_num(nrow(rel)), " \u500b\u6ce2\u6b21\u88e1\uff0c", kitDesign::kit_num(n_rel), " \u500b\u6709\u4fe1\u5ea6\u3002")),
        kitDesign::kit_table_scroll(kitDesign::kit_static_table(data.frame(
          # 原文：波次 | 題數
          "\u6ce2\u6b21" = rel$wave, "\u984c\u6578" = rel$n_items,
          # 原文：信度 | 沒有
          "\u4fe1\u5ea6" = ifelse(is.na(rel$reliability), "\u6c92\u6709", sprintf("%.3f", rel$reliability)),
          check.names = FALSE, stringsAsFactors = FALSE
        )))
      )
    })

    output$overlap_ui <- shiny::renderUI({
      shiny::req(current_dim() %in% dims_cols(bank()))
      ol <- dim_overlap(bank(), current_dim())
      kitDesign::kit_detail(
        # 原文：和其他構面共用的題目（ |  個構面）
        paste0("\u548c\u5176\u4ed6\u69cb\u9762\u5171\u7528\u7684\u984c\u76ee\uff08", kitDesign::kit_num(nrow(ol)), " \u500b\u69cb\u9762\uff09"),
        if (nrow(ol) == 0L) {
          # 原文：這個構面的題目都沒有歸到其他構面。
          shiny::p(class = "kit-hint", "\u9019\u500b\u69cb\u9762\u7684\u984c\u76ee\u90fd\u6c92\u6709\u6b78\u5230\u5176\u4ed6\u69cb\u9762\u3002")
        } else {
          shiny::tagList(
            # 原文：同一個 itemID（任一版本）也歸到其他構面就算共用。比例 = 共用題數 ÷ 這個構面的題數。
            shiny::p(class = "kit-hint", "\u540c\u4e00\u500b itemID\uff08\u4efb\u4e00\u7248\u672c\uff09\u4e5f\u6b78\u5230\u5176\u4ed6\u69cb\u9762\u5c31\u7b97\u5171\u7528\u3002\u6bd4\u4f8b = \u5171\u7528\u984c\u6578 \u00f7 \u9019\u500b\u69cb\u9762\u7684\u984c\u6578\u3002"),
            kitDesign::kit_table_scroll(kitDesign::kit_static_table(data.frame(
              # 原文：構面 | 共用題數
              "\u69cb\u9762" = ol$dimName, "\u5171\u7528\u984c\u6578" = ol$n_shared,
              # 原文：占這個構面 | 那個構面的題數
              "\u5360\u9019\u500b\u69cb\u9762" = paste0(ol$share, "%"), "\u90a3\u500b\u69cb\u9762\u7684\u984c\u6578" = ol$n_items,
              check.names = FALSE, stringsAsFactors = FALSE
            )))
          )
        }
      )
    })

    net <- shiny::reactive({
      shiny::req(current_dim() %in% dims_cols(bank()))
      dim_network(bank(), current_dim(), other_dims = isTRUE(input$other_dims))
    })
    output$network <- visNetwork::renderVisNetwork({
      dim_network_widget(net(), click_input = ns("net_click"))
    })
    output$network_hint <- shiny::renderUI({
      shiny::p(class = "kit-hint", dim_network_hint(net()))
    })
    shiny::observeEvent(input$net_click, {
      hit <- net_parse_id(input$net_click)
      if (is.null(hit)) return()
      if (hit$kind == "item") {
        goto("search", item = hit$itemID, ver = hit$itemVer)
      } else if (!identical(hit$dimID, current_dim())) {
        select_dim(hit$dimID)
      }
    })

    # ── 構面報告：選了構面就只做這個構面；沒選構面就做這個波次的全部構面 ──
    wave <- shiny::reactive(input$wave %||% "")
    scope <- shiny::reactive({
      d <- current_dim()
      if (!is.null(d) && d %in% dims_cols(bank())) d else NULL
    })
    tab <- kitDesign::kit_run_tab_server(
      input, output, session, DIMS_RUN_LABEL,
      # 原文：選一個波次
      ready = shiny::reactive(if (!nzchar(wave())) "\u9078\u4e00\u500b\u6ce2\u6b21" else character(0)),
      settings = function() c(
        # 原文：題庫
        paste0(dims_domain_label(store$domain()), "\u984c\u5eab"),
        # 原文：波次 
        if (nzchar(wave())) paste0("\u6ce2\u6b21 ", wave()),
        # 原文：範圍：這個波次的全部構面
        if (is.null(scope())) "\u7bc4\u570d\uff1a\u9019\u500b\u6ce2\u6b21\u7684\u5168\u90e8\u69cb\u9762"
        # 原文：範圍：只有「 | 」
        else paste0("\u7bc4\u570d\uff1a\u53ea\u6709\u300c", dims_label(bank(), scope()), "\u300d")
      ),
      stale_on = function() list(wave(), scope(), store$bank()),
      # 原文：正在整理構面報告…
      progress = "\u6b63\u5728\u6574\u7406\u69cb\u9762\u5831\u544a\u2026",
      run = function() {
        rd <- dim_report_data(store$bank(), wave(), domain = store$domain(), dims = scope())
        if (length(rd$dims) == 0L) {
          if (!is.null(rd$scope)) {
            # 原文：「 | 」在 
            kitDesign::kit_abort("\u300c", dims_label(store$bank(), rd$scope), "\u300d\u5728 ", wave(),
                                 # 原文： 沒有題目：換一個波次試試，或回到構面清單做整個波次的報告。
                                 " \u6c92\u6709\u984c\u76ee\uff1a\u63db\u4e00\u500b\u6ce2\u6b21\u8a66\u8a66\uff0c\u6216\u56de\u5230\u69cb\u9762\u6e05\u55ae\u505a\u6574\u500b\u6ce2\u6b21\u7684\u5831\u544a\u3002")
          }
          # 原文：波次  |  沒有任何構面的題目：換一個波次試試。
          kitDesign::kit_abort("\u6ce2\u6b21 ", wave(), " \u6c92\u6709\u4efb\u4f55\u69cb\u9762\u7684\u984c\u76ee\uff1a\u63db\u4e00\u500b\u6ce2\u6b21\u8a66\u8a66\u3002")
        }
        rd
      },
      done = function(rd) {
        n_no_rel <- sum(rd$problems$type == "no_reliability")
        msg <- if (!is.null(rd$scope)) {
          # 原文：「 | 」在  |  的構面報告：
          paste0("\u300c", dims_label(bank(), rd$dims[[1]]$dimID), "\u300d\u5728 ", rd$wave, " \u7684\u69cb\u9762\u5831\u544a\uff1a",
                 # 原文： 題。
                 kitDesign::kit_num(rd$n_items), " \u984c\u3002",
                 # 原文：這個構面在這個波次沒有信度。
                 if (n_no_rel > 0L) "\u9019\u500b\u69cb\u9762\u5728\u9019\u500b\u6ce2\u6b21\u6c92\u6709\u4fe1\u5ea6\u3002" else "")
        } else {
          # 原文： 構面報告： |  個構面、
          paste0(rd$wave, " \u69cb\u9762\u5831\u544a\uff1a", kitDesign::kit_num(length(rd$dims)), " \u500b\u69cb\u9762\u3001",
                 # 原文： 題。
                 kitDesign::kit_num(rd$n_items), " \u984c\u3002",
                 # 原文：其中 
                 if (n_no_rel > 0L) paste0("\u5176\u4e2d ", kitDesign::kit_num(n_no_rel),
                                            # 原文： 個構面沒有信度。
                                            " \u500b\u69cb\u9762\u6c92\u6709\u4fe1\u5ea6\u3002") else "")
        }
        kitDesign::kit_done_status(
          msg,
          warnings = dim_report_warnings(rd),
          notes = if (length(rd$not_in_wave)) paste0(
            # 原文：另有  |  個構面在 
            "\u53e6\u6709 ", kitDesign::kit_num(length(rd$not_in_wave)), " \u500b\u69cb\u9762\u5728 ", rd$wave,
            # 原文： 沒有題目，沒有放進報告。
            " \u6c92\u6709\u984c\u76ee\uff0c\u6c92\u6709\u653e\u9032\u5831\u544a\u3002")
        )
      },
      details_ui = function(rd) dims_report_details(bank(), rd),
      code = function() dims_code(store, current_dim(), wave())
    )

    output$downloads <- shiny::renderUI({
      rd <- tab$result()
      if (is.null(rd)) return(NULL)
      # 原文：構面報告_
      who <- if (is.null(rd$scope)) "" else paste0("_", gsub("[\\/:*?\"<>|[:space:]]+", "_", rd$dims[[1]]$dimNameRaw))
      # 原文：構面報告_
      base <- paste0("\u69cb\u9762\u5831\u544a_", dims_domain_label(rd$domain), "_", rd$wave, who,
                     "_", format(Sys.Date(), "%Y%m%d"))
      cls <- kitDesign::kit_btn_class("secondary")
      shiny::div(
        class = "kit-buttons",
        # 原文：下載 Word
        kitDesign::kit_download_button(session, "dl_docx", "\u4e0b\u8f09 Word", paste0(base, ".docx"),
                                       function(file) generate_dim_report_docx(rd, file),
                                       class = cls),
        # 原文：下載 HTML
        kitDesign::kit_download_button(session, "dl_html", "\u4e0b\u8f09 HTML", paste0(base, ".html"),
                                       function(file) render_dim_report_file(rd, file),
                                       class = cls)
      )
    })

    invisible(list(current_dim = current_dim, report = tab))
  })
}

# 構面標題後面的數字：「10 題（30 個版本）· 出現在 5 個波次 · 最近一波 M072小」
# 版本數和題數一樣時不寫版本
dims_count_text <- function(row) {
  paste0(
    # 原文： 題
    kitDesign::kit_num(row$n_items), " \u984c",
    # 原文：（ |  個版本）
    if (row$n_versions != row$n_items) paste0("\uff08", kitDesign::kit_num(row$n_versions), " \u500b\u7248\u672c\uff09") else "",
    # 原文： · 出現在  |  個波次
    " \u00b7 \u51fa\u73fe\u5728 ", kitDesign::kit_num(row$n_waves), " \u500b\u6ce2\u6b21",
    # 原文： · 最近一波 
    if (!is.na(row$last_wave)) paste0(" \u00b7 \u6700\u8fd1\u4e00\u6ce2 ", row$last_wave) else ""
  )
}

# 題組本身沒有題目、但有子題組時的一行：「這個題組本身沒有題目，題目在它的子題組：主動性、合群性…」
# 每個子題組是一個連結，點了送 input_id = 子題組的 dimID
dims_children_line <- function(bank, kids, input_id) {
  links <- lapply(seq_along(kids), function(i) {
    a <- shiny::tags$a(
      href = "#", class = "kit-child-dim",
      onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;",
                        input_id, jsonlite::toJSON(kids[i], auto_unbox = TRUE)),
      dims_label(bank, kids[i]))
    # 原文：、
    if (i > 1L) shiny::tagList("\u3001", a) else a
  })
  shiny::p(
    # 原文：這個題組本身沒有題目，題目都在它的子題組裡。點一個子題組看它的題目：
    "\u9019\u500b\u984c\u7d44\u672c\u8eab\u6c92\u6709\u984c\u76ee\uff0c\u984c\u76ee\u90fd\u5728\u5b83\u7684\u5b50\u984c\u7d44\u88e1\u3002\u9ede\u4e00\u500b\u5b50\u984c\u7d44\u770b\u5b83\u7684\u984c\u76ee\uff1a",
    links,
    # 原文：。下載的各波次題目表會放全部子題組的題目。
    "\u3002\u4e0b\u8f09\u7684\u5404\u6ce2\u6b21\u984c\u76ee\u8868\u6703\u653e\u5168\u90e8\u5b50\u984c\u7d44\u7684\u984c\u76ee\u3002"
  )
}

dims_domain_label <- function(domain) {
  lab <- bank_domain_label(domain)
  if (length(lab) != 1L || is.na(lab)) domain else lab
}

# 細節列：沒有信度的構面（全部）、其他要注意的地方
dims_report_details <- function(bank, rd) {
  p <- rd$problems
  no_rel <- p$id[p$type == "no_reliability"]
  other <- p[p$type != "no_reliability", , drop = FALSE]
  shiny::tagList(
    kitDesign::kit_detail(
      # 原文：沒有信度的構面（ |  個）
      paste0("\u6c92\u6709\u4fe1\u5ea6\u7684\u69cb\u9762\uff08", kitDesign::kit_num(length(no_rel)), " \u500b\uff09"),
      if (length(no_rel) == 0L) {
        # 原文：報告裡的構面都有信度。
        shiny::p(class = "kit-hint", "\u5831\u544a\u88e1\u7684\u69cb\u9762\u90fd\u6709\u4fe1\u5ea6\u3002")
      } else {
        shiny::tagList(
          # 原文：、
          shiny::p(paste(dims_label(bank, no_rel), collapse = "\u3001")),
          # 原文：信度在「題庫編修」的構面資訊填，填好再產生一次報告。
          shiny::p(class = "kit-hint", "\u4fe1\u5ea6\u5728\u300c\u984c\u5eab\u7de8\u4fee\u300d\u7684\u69cb\u9762\u8cc7\u8a0a\u586b\uff0c\u586b\u597d\u518d\u7522\u751f\u4e00\u6b21\u5831\u544a\u3002")
        )
      }
    ),
    if (nrow(other) > 0L) {
      kitDesign::kit_detail(
        # 原文：其他要注意的地方（ |  項）
        paste0("\u5176\u4ed6\u8981\u6ce8\u610f\u7684\u5730\u65b9\uff08", kitDesign::kit_num(nrow(other)), " \u9805\uff09"),
        shiny::tags$ul(lapply(other$message, shiny::tags$li))
      )
    }
  )
}

# 用 R 怎麼做：同樣的函式，用在 kitItemBank::bank_to_v02() 轉好的題庫
dims_code <- function(store, dim_id, wave) {
  domain <- store$domain()
  src <- store$source()
  scoped <- !is.null(dim_id)
  dim_id <- dim_id %||% dims_cols(store$bank())[1L]
  wave <- if (nzchar(wave %||% "")) wave else get_wave_cols(store$bank()$itemWave)[1L]
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", as.character(x), fixed = TRUE), "\"")
  load_line <- if (identical(src$kind, "file")) {
    # 原文：bank <- kitItemBank::read_bank_xlsx(\"你打開的題庫.xlsx\")
    "bank <- kitItemBank::read_bank_xlsx(\"\u4f60\u6253\u958b\u7684\u984c\u5eab.xlsx\")"
  } else {
    paste0("bank <- kitItemBank::bank_to_v02(kitItemBank::read_kit(), ", q(domain), ")")
  }
  paste(c(
    load_line,
    if (nrow(store$changes()) > 0L)
      # 原文：# 注意：畫面上的題庫有還沒下載的改動；這裡讀到的是改動前的題庫
      "# \u6ce8\u610f\uff1a\u756b\u9762\u4e0a\u7684\u984c\u5eab\u6709\u9084\u6c92\u4e0b\u8f09\u7684\u6539\u52d5\uff1b\u9019\u88e1\u8b80\u5230\u7684\u662f\u6539\u52d5\u524d\u7684\u984c\u5eab",
    "",
    # 原文：# 每個構面一列：題數（itemID）、版本數、出現的波次數、有信度的波次數、最近一波
    "# \u6bcf\u500b\u69cb\u9762\u4e00\u5217\uff1a\u984c\u6578\uff08itemID\uff09\u3001\u7248\u672c\u6578\u3001\u51fa\u73fe\u7684\u6ce2\u6b21\u6578\u3001\u6709\u4fe1\u5ea6\u7684\u6ce2\u6b21\u6578\u3001\u6700\u8fd1\u4e00\u6ce2",
    "dims <- kitItemBank::dim_overview(bank)",
    "",
    # 原文：# 一個構面的題目 × 波次（✓ = 這一題在這個波次）；和它共用題目的其他構面
    "# \u4e00\u500b\u69cb\u9762\u7684\u984c\u76ee \u00d7 \u6ce2\u6b21\uff08\u2713 = \u9019\u4e00\u984c\u5728\u9019\u500b\u6ce2\u6b21\uff09\uff1b\u548c\u5b83\u5171\u7528\u984c\u76ee\u7684\u5176\u4ed6\u69cb\u9762",
    paste0("items <- kitItemBank::dim_items(bank, ", q(dim_id), ")"),
    paste0("kitItemBank::dim_overlap(bank, ", q(dim_id), ")"),
    "",
    # 原文：# 構面報告的資料（只有這個構面），和要注意的地方
    if (scoped) "# \u69cb\u9762\u5831\u544a\u7684\u8cc7\u6599\uff08\u53ea\u6709\u9019\u500b\u69cb\u9762\uff09\uff0c\u548c\u8981\u6ce8\u610f\u7684\u5730\u65b9"
    # 原文：# 一個波次的構面報告資料（全部構面），和要注意的地方
    else "# \u4e00\u500b\u6ce2\u6b21\u7684\u69cb\u9762\u5831\u544a\u8cc7\u6599\uff08\u5168\u90e8\u69cb\u9762\uff09\uff0c\u548c\u8981\u6ce8\u610f\u7684\u5730\u65b9",
    paste0("rd <- kitItemBank::dim_report_data(bank, ", q(wave), ", domain = ", q(domain),
           if (scoped) paste0(", dims = ", q(dim_id)), ")"),
    # 原文：length(rd$dims)   # 報告裡的構面數
    "length(rd$dims)   # \u5831\u544a\u88e1\u7684\u69cb\u9762\u6578",
    # 原文：rd$problems       # 沒有信度、沒有題目的構面、題目表裡沒有的題目
    "rd$problems       # \u6c92\u6709\u4fe1\u5ea6\u3001\u6c92\u6709\u984c\u76ee\u7684\u69cb\u9762\u3001\u984c\u76ee\u8868\u88e1\u6c92\u6709\u7684\u984c\u76ee"
  ), collapse = "\n")
}
