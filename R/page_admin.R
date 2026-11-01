# R/page_admin.R — 施測（一次施測的 codebook：各資料檔的變項照問卷順序）
#
# 由上往下：這一頁做什麼 → 條件（左 1/3：施測、資料檔）｜右 2/3：
#   沒選施測：施測清單（點一列 = 選這次施測）
#   選了施測：[回到施測清單]；「M03 世代 36 月齡（2019.03–2020.08）」
#   → 概要（調查期間、狀態、資料檔、遺漏代碼、區塊狀態）→ 下載 Excel／SPSS 標籤
#   → 這個資料檔的變項表（點一題 = 到題項查詢）→ ▸ 建議引用
# → ▸ 用 R 怎麼做
# 只讀新的 kit（17 張表）；查詢是即時的，沒有「執行」按鈕。計算都在 R/codebook.R。

page_admin_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kitDesign::kit_tab_guide(
      # 原文：看一次施測問了哪些題：每個資料檔的變項照問卷順序排好，等於一本 codebook。
      goal = "\u770b\u4e00\u6b21\u65bd\u6e2c\u554f\u4e86\u54ea\u4e9b\u984c\uff1a\u6bcf\u500b\u8cc7\u6599\u6a94\u7684\u8b8a\u9805\u7167\u554f\u5377\u9806\u5e8f\u6392\u597d\uff0c\u7b49\u65bc\u4e00\u672c codebook\u3002",
      steps = c(
        # 原文：在左邊搜尋一次施測（代號或別名都可以，例如 M036大、M36W84、D00250、84M），或在右邊的施測清單點一列。
        "\u5728\u5de6\u908a\u641c\u5c0b\u4e00\u6b21\u65bd\u6e2c\uff08\u4ee3\u865f\u6216\u5225\u540d\u90fd\u53ef\u4ee5\uff0c\u4f8b\u5982 M036\u5927\u3001M36W84\u3001D00250\u300184M\uff09\uff0c\u6216\u5728\u53f3\u908a\u7684\u65bd\u6e2c\u6e05\u55ae\u9ede\u4e00\u5217\u3002",
        # 原文：選一個資料檔（家長、親友、教保人員……），右邊的表列出它的變項。
        "\u9078\u4e00\u500b\u8cc7\u6599\u6a94\uff08\u5bb6\u9577\u3001\u89aa\u53cb\u3001\u6559\u4fdd\u4eba\u54e1\u2026\u2026\uff09\uff0c\u53f3\u908a\u7684\u8868\u5217\u51fa\u5b83\u7684\u8b8a\u9805\u3002",
        # 原文：點一題，到「題項查詢」看這一題在各波次怎麼問。
        "\u9ede\u4e00\u984c\uff0c\u5230\u300c\u984c\u9805\u67e5\u8a62\u300d\u770b\u9019\u4e00\u984c\u5728\u5404\u6ce2\u6b21\u600e\u9ebc\u554f\u3002",
        # 原文：按「下載施測題目清單」存成 Excel；要在 SPSS 加標籤，按「下載 SPSS 標籤」。
        "\u6309\u300c\u4e0b\u8f09\u65bd\u6e2c\u984c\u76ee\u6e05\u55ae\u300d\u5b58\u6210 Excel\uff1b\u8981\u5728 SPSS \u52a0\u6a19\u7c64\uff0c\u6309\u300c\u4e0b\u8f09 SPSS \u6a19\u7c64\u300d\u3002"
      ),
      # 原文：一次施測的資料檔、變項清單和區塊狀態；施測題目清單（Excel）；SPSS 標籤語法（.sps）。
      output = "\u4e00\u6b21\u65bd\u6e2c\u7684\u8cc7\u6599\u6a94\u3001\u8b8a\u9805\u6e05\u55ae\u548c\u5340\u584a\u72c0\u614b\uff1b\u65bd\u6e2c\u984c\u76ee\u6e05\u55ae\uff08Excel\uff09\uff1bSPSS \u6a19\u7c64\u8a9e\u6cd5\uff08.sps\uff09\u3002",
      # 原文：這一頁只看不改。標「內部」的施測還沒有公開釋出。
      note = "\u9019\u4e00\u9801\u53ea\u770b\u4e0d\u6539\u3002\u6a19\u300c\u5167\u90e8\u300d\u7684\u65bd\u6e2c\u9084\u6c92\u6709\u516c\u958b\u91cb\u51fa\u3002"
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectizeInput(
          # 原文：施測
          ns("admin"), "\u65bd\u6e2c", choices = NULL, width = "100%",
          # 原文：搜尋施測（代號、別名）…
          options = list(placeholder = "\u641c\u5c0b\u65bd\u6e2c\uff08\u4ee3\u865f\u3001\u5225\u540d\uff09\u2026",
                         searchField = c("label", "value", "aliases"))
        ),
        shiny::uiOutput(ns("dataset_ui"))
      ),
      shiny::column(8, shiny::uiOutput(ns("browse")))
    ),
    kitDesign::kit_details_row(kitDesign::kit_code_panel_ui(ns("code")))
  )
}

#' @param kit 題庫（kit，17 張表）：reactive，或直接給 list
#' @param goto 切到別的分頁：goto("search", item = "cogc01", ver = 1, uid = "P.cogc01")
#' @param request 別的分頁送來的請求（reactive，list(tab, args, at)）：
#'   list(tab = "admin", args = list(admin = "M036大", dataset = "M36M036-P")) 打開那次施測
#' @noRd
page_admin_server <- function(id, kit, goto, request) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    kit_now <- if (shiny::is.reactive(kit)) kit else function() kit
    overview <- shiny::reactive(admin_summary(kit_now()))
    current <- shiny::reactiveVal(NULL)   # 選的 adminID
    want_ds <- shiny::reactiveVal(NULL)   # 指定要看的資料檔（別的分頁送來的）

    # ── 條件：施測的選項（搜尋代號、名稱、別名）──────────────
    admin_choices <- shiny::reactive({
      k <- kit_now()
      ov <- overview()
      al <- k$aliases[k$aliases$target_type == "admin", , drop = FALSE]
      data.frame(
        value = ov$adminID, label = ov$label,
        aliases = vapply(seq_len(nrow(ov)), function(i) {
          paste(unique(c(ov$adminID[i], al$alias[al$target_id == ov$adminID[i]], ov$release[i])),
                collapse = " ")
        }, character(1)),
        stringsAsFactors = FALSE
      )
    })
    push_choices <- function(sel) {
      shiny::updateSelectizeInput(session, "admin", choices = shiny::isolate(admin_choices()),
                                  selected = sel %||% "", server = TRUE)
    }
    shiny::observe({
      admin_choices()
      cur <- shiny::isolate(current())
      if (!is.null(cur) && !cur %in% overview()$adminID) {
        cur <- NULL
        current(NULL)
      }
      push_choices(cur)
    })
    select_admin <- function(a, ds = NULL) {
      want_ds(ds)
      current(a)
      push_choices(a)
      if (!is.null(ds)) shiny::updateRadioButtons(session, "dataset", selected = ds)
    }
    shiny::observeEvent(input$admin, {
      a <- input$admin
      if (!nzchar(a %||% "")) a <- NULL
      if (!identical(a, current())) {
        want_ds(NULL)
        current(a)
      }
    }, ignoreNULL = FALSE)
    shiny::observeEvent(input$back, select_admin(NULL))

    # 別的分頁送來的「看這次施測」：代號或別名都可以
    shiny::observeEvent(request(), {
      r <- request()
      if (!identical(r$tab, "admin")) return()
      x <- r$args$admin %||% r$args$adminID
      if (is.null(x)) return()
      hit <- resolve_admin(kit_now(), x)
      if (length(hit) == 0L) {
        # 原文：題庫裡找不到施測「 | 」。
        kitDesign::kit_notify(paste0("\u984c\u5eab\u88e1\u627e\u4e0d\u5230\u65bd\u6e2c\u300c", paste(x, collapse = " "), "\u300d\u3002"), session)
        return()
      }
      if (length(hit) > 1L) {
        # 原文：「 | 」對到 
        kitDesign::kit_notify(paste0("\u300c", paste(x, collapse = " "), "\u300d\u5c0d\u5230 ", length(hit),
                                     # 原文： 次施測，先打開第一個；其他的在左邊選。
                                     " \u6b21\u65bd\u6e2c\uff0c\u5148\u6253\u958b\u7b2c\u4e00\u500b\uff1b\u5176\u4ed6\u7684\u5728\u5de6\u908a\u9078\u3002"), session)
      }
      select_admin(hit[1], r$args$dataset)
    })

    cb <- shiny::reactive({
      a <- current()
      shiny::req(a, a %in% overview()$adminID)
      admin_codebook(kit_now(), a)
    })

    # ── 條件：資料檔 ───────────────────────────────────
    output$dataset_ui <- shiny::renderUI({
      if (is.null(current())) {
        # 原文：先選一次施測，這裡會列出它的資料檔。
        return(shiny::p(class = "kit-hint", "\u5148\u9078\u4e00\u6b21\u65bd\u6e2c\uff0c\u9019\u88e1\u6703\u5217\u51fa\u5b83\u7684\u8cc7\u6599\u6a94\u3002"))
      }
      x <- cb()
      ds <- x$datasets
      n <- vapply(ds$datasetID, function(d) sum(x$vars$datasetID == d), integer(1))
      sel <- shiny::isolate(want_ds())
      if (is.null(sel) || !sel %in% ds$datasetID) sel <- ds$datasetID[1]
      shiny::radioButtons(
        # 原文：資料檔
        ns("dataset"), "\u8cc7\u6599\u6a94",
        # 原文：：
        choices = stats::setNames(ds$datasetID, paste0(ds$dataset_label, "\uff1a",
                                                       # 原文： 個變項
                                                       kitDesign::kit_num(n), " \u500b\u8b8a\u9805")),
        selected = sel
      )
    })
    shiny::observeEvent(input$dataset, {
      if (!identical(input$dataset, want_ds())) want_ds(NULL)
    })
    current_ds <- shiny::reactive({
      ids <- cb()$datasets$datasetID
      w <- want_ds()
      d <- input$dataset
      if (!is.null(w) && w %in% ids) w else if (!is.null(d) && d %in% ids) d else ids[1]
    })

    # ── 右邊：施測清單，或一次施測 ────────────────────────
    output$browse <- shiny::renderUI({
      a <- current()
      ov <- overview()
      if (is.null(a) || !a %in% ov$adminID) {
        return(shiny::tagList(
          # 原文：施測清單（ |  次）
          shiny::h4(paste0("\u65bd\u6e2c\u6e05\u55ae\uff08", kitDesign::kit_num(nrow(ov)), " \u6b21\uff09")),
          # 原文：點一列，看這次施測的資料檔和變項。標「內部」的還沒有公開釋出。
          shiny::p(class = "kit-hint", "\u9ede\u4e00\u5217\uff0c\u770b\u9019\u6b21\u65bd\u6e2c\u7684\u8cc7\u6599\u6a94\u548c\u8b8a\u9805\u3002\u6a19\u300c\u5167\u90e8\u300d\u7684\u9084\u6c92\u6709\u516c\u958b\u91cb\u51fa\u3002"),
          kitDesign::kit_table_scroll(DT::DTOutput(ns("summary_dt")))
        ))
      }
      s <- ov[ov$adminID == a, , drop = FALSE]
      x <- cb()
      cite <- c(cb_blank(x$admin$citation_zh), cb_blank(x$admin$citation_en))
      cite <- cite[nzchar(cite)]
      shiny::tagList(
        # 先放按鈕列再放標題：提示小字的負上邊距只會貼近標題，不會壓到按鈕
        # 原文：回到施測清單
        shiny::div(class = "kit-buttons", kitDesign::kit_button(ns("back"), "\u56de\u5230\u65bd\u6e2c\u6e05\u55ae")),
        shiny::h4(s$label),
        shiny::p(class = "kit-hint", paste0(
          # 原文：代號  |  ·  |  ·  |  個資料檔 · 
          "\u4ee3\u865f ", a, " \u00b7 ", s$status_label, " \u00b7 ", kitDesign::kit_num(s$n_datasets), " \u500b\u8cc7\u6599\u6a94 \u00b7 ",
          # 原文： 題
          kitDesign::kit_num(s$n_items), " \u984c"
        )),
        admin_overview_ui(kit_now(), x),
        shiny::uiOutput(ns("downloads")),
        shiny::uiOutput(ns("vars_head")),
        kitDesign::kit_table_scroll(DT::DTOutput(ns("vars_dt"))),
        if (length(cite)) kitDesign::kit_details_row(kitDesign::kit_detail(
          # 原文：建議引用
          "\u5efa\u8b70\u5f15\u7528",
          lapply(cite, function(t) shiny::p(style = "overflow-wrap: anywhere;", t))
        ))
      )
    })

    output$summary_dt <- DT::renderDT({
      ov <- overview()
      df <- data.frame(
        # 原文：施測 | 代號 | 狀態
        "\u65bd\u6e2c" = ov$label, "\u4ee3\u865f" = ov$adminID, "\u72c0\u614b" = ov$status_label,
        # 原文：資料檔 | 題數
        "\u8cc7\u6599\u6a94" = ov$n_datasets, "\u984c\u6578" = ov$n_items,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      # 原文：施測
      kitDesign::kit_dt(df, selection = "single", page_length = 0L, wrap_cols = "\u65bd\u6e2c")
    })
    shiny::observeEvent(input$summary_dt_rows_selected, {
      i <- input$summary_dt_rows_selected
      ov <- overview()
      if (length(i) == 1L && i <= nrow(ov)) select_admin(ov$adminID[i])
    })

    vars <- shiny::reactive({
      v <- cb()$vars
      v[v$datasetID == current_ds(), , drop = FALSE]
    })
    output$vars_head <- shiny::renderUI({
      v <- vars()
      ds <- cb()$datasets
      d <- ds[ds$datasetID == current_ds(), , drop = FALSE]
      n_hidden <- sum(!v$public)
      shiny::tagList(
        # 原文：的變項（ |  個）
        shiny::h4(paste0(d$dataset_label, "\u7684\u8b8a\u9805\uff08", kitDesign::kit_num(nrow(v)), " \u500b\uff09")),
        shiny::p(
          class = "kit-hint",
          # 原文：照資料檔裡的順序排。點一題，到「題項查詢」看這一題；表格上方的搜尋框可以再找。
          "\u7167\u8cc7\u6599\u6a94\u88e1\u7684\u9806\u5e8f\u6392\u3002\u9ede\u4e00\u984c\uff0c\u5230\u300c\u984c\u9805\u67e5\u8a62\u300d\u770b\u9019\u4e00\u984c\uff1b\u8868\u683c\u4e0a\u65b9\u7684\u641c\u5c0b\u6846\u53ef\u4ee5\u518d\u627e\u3002",
          # 原文：這個資料檔還沒有記下順序，暫時照變項名稱排。
          if (nrow(v) && all(is.na(v$order))) "\u9019\u500b\u8cc7\u6599\u6a94\u9084\u6c92\u6709\u8a18\u4e0b\u9806\u5e8f\uff0c\u66ab\u6642\u7167\u8b8a\u9805\u540d\u7a31\u6392\u3002",
          # 原文：有 
          if (n_hidden > 0L) paste0("\u6709 ", kitDesign::kit_num(n_hidden),
                                    # 原文： 個變項調查有問、但公開檔沒有，題目前面標「公開檔沒有」。
                                    " \u500b\u8b8a\u9805\u8abf\u67e5\u6709\u554f\u3001\u4f46\u516c\u958b\u6a94\u6c92\u6709\uff0c\u984c\u76ee\u524d\u9762\u6a19\u300c\u516c\u958b\u6a94\u6c92\u6709\u300d\u3002")
        )
      )
    })
    output$vars_dt <- DT::renderDT({
      v <- vars()
      lab <- cb_one_line(v$label)
      # 原文：［公開檔沒有］
      lab <- ifelse(v$public, lab, paste0("\uff3b\u516c\u958b\u6a94\u6c92\u6709\uff3d", lab))
      df <- data.frame(
        # 原文：位置 | 變項 | 題目 | 區塊 | 題組
        "\u4f4d\u7f6e" = v$position, "\u8b8a\u9805" = v$varname, "\u984c\u76ee" = lab, "\u5340\u584a" = v$block, "\u984c\u7d44" = v$groups,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      kitDesign::kit_dt(df, selection = "single", page_length = 25L, search = TRUE,
                        # 原文：題目 | 題組 | 題目
                        wrap_cols = c("\u984c\u76ee", "\u984c\u7d44"), min_widths = c("\u984c\u76ee" = "16rem"))
    })
    shiny::observeEvent(input$vars_dt_rows_selected, {
      i <- input$vars_dt_rows_selected
      v <- vars()
      if (length(i) != 1L || i > nrow(v)) return()
      DT::selectRows(DT::dataTableProxy("vars_dt"), NULL)
      if (!nzchar(v$item_uid[i])) {
        # 原文：「 | 」是系統變項，題庫裡沒有它的題目。
        kitDesign::kit_notify(paste0("\u300c", v$varname[i], "\u300d\u662f\u7cfb\u7d71\u8b8a\u9805\uff0c\u984c\u5eab\u88e1\u6c92\u6709\u5b83\u7684\u984c\u76ee\u3002"), session)
        return()
      }
      goto("search", item = v$itemID[i], ver = cb_int(v$itemVer[i]), uid = v$item_uid[i])
    })

    # ── 下載：整次施測的 Excel；目前資料檔的 SPSS 標籤 ──────────
    output$downloads <- shiny::renderUI({
      a <- current()
      shiny::req(a)
      k <- kit_now()
      d <- current_ds()
      ds <- cb()$datasets
      resp <- ds$respondent_label[ds$datasetID == d]
      day <- format(Sys.Date(), "%Y%m%d")
      cls <- kitDesign::kit_btn_class("secondary")
      shiny::tagList(
        shiny::div(
          class = "kit-buttons",
          kitDesign::kit_download_button(
            # 原文：下載施測題目清單（Excel）
            session, "dl_xlsx", "\u4e0b\u8f09\u65bd\u6e2c\u984c\u76ee\u6e05\u55ae\uff08Excel\uff09",
            # 原文：施測題目清單_
            paste0("\u65bd\u6e2c\u984c\u76ee\u6e05\u55ae_", a, "_", day, ".xlsx"),
            function(file) writexl::write_xlsx(codebook_workbook(k, a), file),
            class = cls
          ),
          kitDesign::kit_download_button(
            # 原文：下載 SPSS 標籤（ | ）
            session, "dl_sps", paste0("\u4e0b\u8f09 SPSS \u6a19\u7c64\uff08", resp, "\uff09"),
            # 原文：SPSS標籤_
            paste0("SPSS\u6a19\u7c64_", d, "_", day, ".sps"),
            function(file) codebook_write_sps(codebook_sps(k, d), file),
            class = cls
          )
        ),
        shiny::p(class = "kit-hint",
                 # 原文：Excel 裡每個資料檔一張表，第一張是說明。SPSS 標籤只做目前選的資料檔，只放公開檔有的變項。
                 "Excel \u88e1\u6bcf\u500b\u8cc7\u6599\u6a94\u4e00\u5f35\u8868\uff0c\u7b2c\u4e00\u5f35\u662f\u8aaa\u660e\u3002SPSS \u6a19\u7c64\u53ea\u505a\u76ee\u524d\u9078\u7684\u8cc7\u6599\u6a94\uff0c\u53ea\u653e\u516c\u958b\u6a94\u6709\u7684\u8b8a\u9805\u3002")
      )
    })

    kitDesign::kit_code_panel_server("code", shiny::reactive({
      ov <- overview()
      a <- current() %||% ov$adminID[1]
      d <- if (is.null(current())) {
        kit_now()$datasets$datasetID[kit_now()$datasets$adminID == a][1]
      } else current_ds()
      admin_code(a, d)
    }))

    invisible(list(current_admin = current, dataset = current_ds, codebook = cb))
  })
}

# 概要：調查期間、狀態、資料檔、遺漏代碼、區塊狀態、備註
admin_overview_ui <- function(kit, cb) {
  ad <- cb$admin
  ds <- cb$datasets
  n <- vapply(ds$datasetID, function(d) sum(cb$vars$datasetID == d), integer(1))
  n_item <- vapply(ds$datasetID, function(d) sum(cb$vars$datasetID == d & cb$vars$kind == "item"),
                   integer(1))
  period <- cb_period_words(ad$surveyFrom, ad$surveyTo)
  release <- cb_blank(ad$datasetRelease)
  status <- switch(
    cb_blank(ad$status),
    # 原文：已釋出 | ，公開資料的釋出編號是  | 。
    released = paste0("\u5df2\u91cb\u51fa", if (nzchar(release)) paste0("\uff0c\u516c\u958b\u8cc7\u6599\u7684\u91cb\u51fa\u7de8\u865f\u662f ", release), "\u3002"),
    # 原文：內部：還沒有公開釋出，只有計畫團隊能用。
    internal = "\u5167\u90e8\uff1a\u9084\u6c92\u6709\u516c\u958b\u91cb\u51fa\uff0c\u53ea\u6709\u8a08\u756b\u5718\u968a\u80fd\u7528\u3002",
    # 原文：規劃中：還沒有施測。
    planned = "\u898f\u5283\u4e2d\uff1a\u9084\u6c92\u6709\u65bd\u6e2c\u3002",
    cb_status_label(ad$status)
  )
  schemes <- unique(cb_blank(ds$missingScheme))
  schemes <- schemes[nzchar(schemes)]
  blocks <- admin_block_sentences(cb)
  note <- cb_blank(ad$note)
  shiny::div(
    # 原文：調查期間： | 還沒有記下日期。
    shiny::p(shiny::tags$strong("\u8abf\u67e5\u671f\u9593\uff1a"), if (nzchar(period)) period else "\u9084\u6c92\u6709\u8a18\u4e0b\u65e5\u671f\u3002"),
    # 原文：狀態：
    shiny::p(shiny::tags$strong("\u72c0\u614b\uff1a"), status),
    # 原文：資料檔（ |  個）：
    shiny::p(shiny::tags$strong(paste0("\u8cc7\u6599\u6a94\uff08", length(n), " \u500b\uff09\uff1a"))),
    shiny::tags$ul(lapply(seq_len(nrow(ds)), function(i) shiny::tags$li(paste0(
      # 原文：： |  個變項，其中 
      ds$dataset_label[i], "\uff1a", kitDesign::kit_num(n[i]), " \u500b\u8b8a\u9805\uff0c\u5176\u4e2d ",
      # 原文： 題
      kitDesign::kit_num(n_item[i]), " \u984c"
    )))),
    shiny::p(
      # 原文：遺漏代碼：
      shiny::tags$strong("\u907a\u6f0f\u4ee3\u78bc\uff1a"),
      # 原文：沒有記下用哪一套。
      if (length(schemes) == 0L) "\u6c92\u6709\u8a18\u4e0b\u7528\u54ea\u4e00\u5957\u3002" else
        lapply(seq_along(schemes), function(i) shiny::tagList(
          if (i > 1L) shiny::tags$br(),
          # 原文：方案  | ：
          paste0(if (length(schemes) > 1L) paste0("\u65b9\u6848 ", schemes[i], "\uff1a") else "",
                 # 原文：、
                 cb_scheme_text(kit, schemes[i], sep = "\u3001", eq = " "))
        ))
    ),
    # 原文：遺漏代碼是沒有有效答案時填的數字（拒答、不知道、跳答），分析前要先設成遺漏值；SPSS 標籤會幫你設。
    shiny::p(class = "kit-hint", "\u907a\u6f0f\u4ee3\u78bc\u662f\u6c92\u6709\u6709\u6548\u7b54\u6848\u6642\u586b\u7684\u6578\u5b57\uff08\u62d2\u7b54\u3001\u4e0d\u77e5\u9053\u3001\u8df3\u7b54\uff09\uff0c\u5206\u6790\u524d\u8981\u5148\u8a2d\u6210\u907a\u6f0f\u503c\uff1bSPSS \u6a19\u7c64\u6703\u5e6b\u4f60\u8a2d\u3002"),
    # 原文：區塊狀態：
    shiny::p(shiny::tags$strong("\u5340\u584a\u72c0\u614b\uff1a")),
    if (length(blocks)) shiny::tags$ul(lapply(blocks, shiny::tags$li))
    # 原文：題庫沒有記到少問的區塊：每個資料檔的區塊都照題本問。
    else shiny::p("\u984c\u5eab\u6c92\u6709\u8a18\u5230\u5c11\u554f\u7684\u5340\u584a\uff1a\u6bcf\u500b\u8cc7\u6599\u6a94\u7684\u5340\u584a\u90fd\u7167\u984c\u672c\u554f\u3002"),
    # 原文：備註：
    if (nzchar(note)) shiny::p(shiny::tags$strong("\u5099\u8a3b\uff1a"), note)
  )
}

# 用 R 怎麼做：同樣的函式，用在 kitItemBank::read_kit() 讀到的題庫
admin_code <- function(admin_id, dataset_id) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", as.character(x), fixed = TRUE), "\"")
  paste(c(
    "kit <- kitItemBank::read_kit()",
    "",
    # 原文：# 施測清單：每次施測一列
    "# \u65bd\u6e2c\u6e05\u55ae\uff1a\u6bcf\u6b21\u65bd\u6e2c\u4e00\u5217",
    "kitItemBank::admin_summary(kit)",
    # 原文：# 用代號或別名找施測（例如 M036大、M36W84、84M）
    "# \u7528\u4ee3\u865f\u6216\u5225\u540d\u627e\u65bd\u6e2c\uff08\u4f8b\u5982 M036\u5927\u3001M36W84\u300184M\uff09",
    "kitItemBank::resolve_admin(kit, \"M36W84\")",
    "",
    # 原文：# 一次施測：每個變項一列（照檔案順序），和區塊狀態
    "# \u4e00\u6b21\u65bd\u6e2c\uff1a\u6bcf\u500b\u8b8a\u9805\u4e00\u5217\uff08\u7167\u6a94\u6848\u9806\u5e8f\uff09\uff0c\u548c\u5340\u584a\u72c0\u614b",
    paste0("cb <- kitItemBank::admin_codebook(kit, ", q(admin_id), ")"),
    "cb$vars",
    "cb$block_status",
    "",
    # 原文：# 下載的施測題目清單（Excel）和 SPSS 標籤
    "# \u4e0b\u8f09\u7684\u65bd\u6e2c\u984c\u76ee\u6e05\u55ae\uff08Excel\uff09\u548c SPSS \u6a19\u7c64",
    paste0("writexl::write_xlsx(kitItemBank::codebook_workbook(kit, ", q(admin_id), "), ",
           # 原文：施測題目清單_
           q(paste0("\u65bd\u6e2c\u984c\u76ee\u6e05\u55ae_", admin_id, ".xlsx")), ")"),
    paste0("writeLines(kitItemBank::codebook_sps(kit, ", q(dataset_id), "), ",
           q(paste0(dataset_id, ".sps")), ", useBytes = TRUE)")
  ), collapse = "\n")
}
