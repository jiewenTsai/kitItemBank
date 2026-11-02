# R/page_search.R — 題項查詢
#
# 由上往下：這一頁做什麼 → 條件（左 1/3）｜本次條件 → 結果一句話＋下載 → 表 → 這一題
# → ▸ 跨波次對照表 ▸ 各波次題數 ▸ 用 R 怎麼做
# 查詢是即時的，沒有「執行」按鈕；主角是「下載查詢結果」。計算都在 R/query.R。

SEARCH_PAGE_LENGTH <- 20L

page_search_ui <- function(id) {
  ns <- shiny::NS(id)
  # 書籤還原：下拉選單的選項是伺服器端放的，先把網址裡的值放進選項，才不會被丟掉
  restored <- function(name) shiny::restoreInput(ns(name), NULL)
  # 原文：不限
  multi <- function(name, label, placeholder = "\u4e0d\u9650") {
    sel <- restored(name)
    shiny::selectizeInput(
      ns(name), label, choices = sel, selected = sel, multiple = TRUE,
      width = "100%", options = list(placeholder = placeholder, plugins = list("remove_button"))
    )
  }
  advanced_set <- length(restored("blocks")) || length(restored("item_types")) ||
    length(restored("respondents")) || isTRUE(restored("include_deprecated"))

  shiny::tagList(
    kitDesign::kit_tab_guide(
      # 原文：用關鍵字、波次、構面找題目，看一題在各波次怎麼問。
      goal = "\u7528\u95dc\u9375\u5b57\u3001\u6ce2\u6b21\u3001\u69cb\u9762\u627e\u984c\u76ee\uff0c\u770b\u4e00\u984c\u5728\u5404\u6ce2\u6b21\u600e\u9ebc\u554f\u3002",
      steps = c(
        # 原文：在左邊打關鍵字，或選波次、構面（不選就是不限）。
        "\u5728\u5de6\u908a\u6253\u95dc\u9375\u5b57\uff0c\u6216\u9078\u6ce2\u6b21\u3001\u69cb\u9762\uff08\u4e0d\u9078\u5c31\u662f\u4e0d\u9650\uff09\u3002",
        # 原文：右邊的表馬上更新；點一列，下面出現這一題的細節。
        "\u53f3\u908a\u7684\u8868\u99ac\u4e0a\u66f4\u65b0\uff1b\u9ede\u4e00\u5217\uff0c\u4e0b\u9762\u51fa\u73fe\u9019\u4e00\u984c\u7684\u7d30\u7bc0\u3002",
        # 原文：按「下載查詢結果」，存成 Excel（含跨波次對照表）。
        "\u6309\u300c\u4e0b\u8f09\u67e5\u8a62\u7d50\u679c\u300d\uff0c\u5b58\u6210 Excel\uff08\u542b\u8de8\u6ce2\u6b21\u5c0d\u7167\u8868\uff09\u3002"
      ),
      # 原文：符合條件的題目清單、一題的細節（波次、構面、其他版本），以及跨波次對照表。
      output = "\u7b26\u5408\u689d\u4ef6\u7684\u984c\u76ee\u6e05\u55ae\u3001\u4e00\u984c\u7684\u7d30\u7bc0\uff08\u6ce2\u6b21\u3001\u69cb\u9762\u3001\u5176\u4ed6\u7248\u672c\uff09\uff0c\u4ee5\u53ca\u8de8\u6ce2\u6b21\u5c0d\u7167\u8868\u3002",
      note = shiny::tagList(
        # 原文：條件會寫進網址：把網址貼給同事，就看到同樣的查詢。
        "\u689d\u4ef6\u6703\u5beb\u9032\u7db2\u5740\uff1a\u628a\u7db2\u5740\u8cbc\u7d66\u540c\u4e8b\uff0c\u5c31\u770b\u5230\u540c\u6a23\u7684\u67e5\u8a62\u3002",
        shiny::tags$br(),
        WAVE_CODE_NOTE
      )
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        # 原文：關鍵字
        shiny::textInput(ns("keyword"), "\u95dc\u9375\u5b57", width = "100%",
                         # 原文：例如：睡眠、sle*
                         placeholder = "\u4f8b\u5982\uff1a\u7761\u7720\u3001sle*"),
        shiny::radioButtons(ns("keyword_match"), NULL,
                            # 原文：每個詞都要有 | 任一個詞
                            c("\u6bcf\u500b\u8a5e\u90fd\u8981\u6709" = "all", "\u4efb\u4e00\u500b\u8a5e" = "any"), inline = TRUE),
        shiny::div(class = "kit-hint",
                   # 原文：多個詞用空白分開；不分全形半形、簡體繁體。打 itemID 的開頭加 *，例如 sle*。
                   "\u591a\u500b\u8a5e\u7528\u7a7a\u767d\u5206\u958b\uff1b\u4e0d\u5206\u5168\u5f62\u534a\u5f62\u3001\u7c21\u9ad4\u7e41\u9ad4\u3002\u6253 itemID \u7684\u958b\u982d\u52a0 *\uff0c\u4f8b\u5982 sle*\u3002"),
        # 原文：找
        shiny::radioButtons(ns("fields"), "\u627e",
                            # 原文：題目文字 | 全部欄位
                            c("\u984c\u76ee\u6587\u5b57" = "text", "\u5168\u90e8\u6b04\u4f4d" = "all"), inline = TRUE),
        # 原文：波次
        multi("waves", "\u6ce2\u6b21"),
        shiny::radioButtons(ns("wave_match"), NULL,
                            # 原文：任一波次 | 每個波次都有
                            c("\u4efb\u4e00\u6ce2\u6b21" = "any", "\u6bcf\u500b\u6ce2\u6b21\u90fd\u6709" = "all"), inline = TRUE),
        # 原文：構面
        multi("dims", "\u69cb\u9762"),
        shiny::radioButtons(ns("dim_match"), NULL,
                            # 原文：任一構面 | 每個構面都有
                            c("\u4efb\u4e00\u69cb\u9762" = "any", "\u6bcf\u500b\u69cb\u9762\u90fd\u6709" = "all"), inline = TRUE),
        kitDesign::kit_advanced(
          # 原文：進階設定（區塊、題型、填答者）
          title = "\u9032\u968e\u8a2d\u5b9a\uff08\u5340\u584a\u3001\u984c\u578b\u3001\u586b\u7b54\u8005\uff09", open = advanced_set,
          # 原文：區塊
          multi("blocks", "\u5340\u584a"),
          # 原文：題型
          multi("item_types", "\u984c\u578b"),
          # 原文：填答者
          multi("respondents", "\u586b\u7b54\u8005"),
          # 原文：包含已停用的題目
          shiny::checkboxInput(ns("include_deprecated"), "\u5305\u542b\u5df2\u505c\u7528\u7684\u984c\u76ee", FALSE)
        ),
        # 原文：清除條件
        kitDesign::kit_button(ns("clear"), "\u6e05\u9664\u689d\u4ef6")
      ),
      shiny::column(
        8,
        shiny::uiOutput(ns("settings")),
        shiny::uiOutput(ns("result_head")),
        DT::DTOutput(ns("table")),
        shiny::uiOutput(ns("detail"))
      )
    ),
    # 這一題的「各版本出現的波次」：整頁寬，M03、M36 分開
    shiny::uiOutput(ns("timeline")),
    # 跨波次對照總表：全部題目 × 全部波次，只給下載
    shiny::uiOutput(ns("xw_dl")),
    kitDesign::kit_details_row(
      # 打開、收起時告訴伺服器（heat_open）：plotly 圖只在打開時放進頁面，見 output$heat_ui
      htmltools::tagAppendAttributes(
        kitDesign::kit_detail(
          # 原文：各波次題數
          "\u5404\u6ce2\u6b21\u984c\u6578",
          # 原文：每一列是
          shiny::radioButtons(ns("heat_by"), "\u6bcf\u4e00\u5217\u662f",
                              # 原文：區塊 | 構面
                              c("\u5340\u584a" = "block", "\u69cb\u9762" = "dim"), inline = TRUE),
          # 原文：世代 | 全部
          shiny::radioButtons(ns("heat_cohort"), "\u4e16\u4ee3", c("\u5168\u90e8" = "", M03 = "M03", M36 = "M36"),
                              inline = TRUE),
          shiny::uiOutput(ns("heat_ui")),
          shiny::uiOutput(ns("heat_note"))
        ),
        ontoggle = sprintf("Shiny.setInputValue('%s', this.open)", ns("heat_open"))
      ),
      kitDesign::kit_code_panel_ui(ns("code"))
    )
  )
}

#' @param store bank_store_server() 的回傳
#' @param goto 切到別的分頁：goto("dims", dim = "…")、goto("edit", item = "…", ver = 1)
#' @param request 別的分頁送來的請求（reactive，list(tab, args, at)）：
#'   list(item = "…", ver = 1) 打開那一題；list(keyword = "…") 用這個關鍵字查
#' @noRd
page_search_server <- function(id, store, goto, request) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    bank <- shiny::reactive(store$bank())

    # ---- 選項跟著題庫走（換題庫、編修以後） ---------------------------------
    choices <- shiny::reactive({
      b <- bank()
      ib <- b$itemBank
      dim_ids <- get_matrix_cols(b$itemDim)
      dn <- dim_names(b, dim_ids)
      uniq <- function(x) sort(unique(x[!is.na(x) & nzchar(trimws(x))]))
      types <- uniq(as.character(ib$itemType))
      list(
        waves = make_col_choices(get_wave_cols(b$itemWave)),
        dims = stats::setNames(dim_ids, ifelse(dn == dim_ids, dim_ids,
                                               # 原文：（ | ）
                                               paste0(dn, "\uff08", dim_ids, "\uff09"))),
        blocks = uniq(as.character(ib$block)),
        item_types = stats::setNames(types, item_type_label(types)),
        respondents = uniq(unlist(split_respondent(ib$respondent)))
      )
    })
    shiny::observe({
      ch <- choices()
      for (nm in names(ch)) {
        keep <- intersect(shiny::isolate(input[[nm]]), unname(ch[[nm]]))
        shiny::updateSelectizeInput(session, nm, choices = ch[[nm]],
                                    selected = keep %||% character(0))
      }
    })

    # ---- 條件與結果 ----------------------------------------------------------
    # 打字時等 300 毫秒再查（debounce）；「清除條件」和別的分頁送來的請求直接設定，
    # 不等瀏覽器把新的關鍵字送回來、也不靠 debounce 的計時器：從構面查詢跳過來時，
    # 計時器有時候不會再觸發，結果就停在舊的關鍵字（瀏覽器裡已經換成新的了）。
    typed <- shiny::debounce(shiny::reactive(input$keyword %||% ""), 300)
    keyword <- shiny::reactiveVal("")
    shiny::observeEvent(typed(), keyword(typed()), ignoreNULL = FALSE)
    args <- shiny::reactive({
      ch <- choices()
      list(
        keyword = keyword(),
        fields = input$fields %||% "text",
        keyword_match = input$keyword_match %||% "all",
        waves = intersect(input$waves, unname(ch$waves)),
        wave_match = input$wave_match %||% "any",
        dims = intersect(input$dims, unname(ch$dims)),
        dim_match = input$dim_match %||% "any",
        blocks = intersect(input$blocks, ch$blocks),
        item_types = intersect(input$item_types, unname(ch$item_types)),
        respondents = intersect(input$respondents, ch$respondents),
        include_deprecated = isTRUE(input$include_deprecated)
      )
    })
    results <- shiny::reactive(do.call(query_items, c(list(bank()), args())))
    # 同樣的條件、連已停用的也算，多出幾題（結果句寫「不含 N 題已停用」）
    n_hidden <- shiny::reactive({
      a <- args()
      if (isTRUE(a$include_deprecated)) return(0L)
      a$include_deprecated <- TRUE
      nrow(do.call(query_items, c(list(bank()), a))) - nrow(results())
    })

    output$settings <- shiny::renderUI({
      kitDesign::kit_settings_line(search_settings_parts(args(), bank()))
    })

    # ---- 回到剛才的查詢（從別的分頁跳過來以前的條件） ------------------------
    prev_query <- shiny::reactiveVal(NULL)
    jump_keyword <- NULL  # 上一次跳過來時設的關鍵字
    snapshot <- function() {
      a <- shiny::isolate(args())
      list(args = a, sel = shiny::isolate(sel()))
    }
    remember_query <- function() {
      a <- shiny::isolate(args())
      # 還停在上一次跳過來的結果：保留更早的查詢
      if (!is.null(shiny::isolate(prev_query())) && identical(a$keyword, jump_keyword)) return()
      prev_query(if (has_query(a)) snapshot() else NULL)
    }
    back_link <- function() {
      p <- prev_query()
      if (is.null(p)) return(NULL)
      shiny::p(
        # 原文：← 回到剛才的查詢
        shiny::actionLink(ns("back_go"), "\u2190 \u56de\u5230\u525b\u624d\u7684\u67e5\u8a62"),
        shiny::span(class = "kit-hint",
                    # 原文：（ | ）
                    paste0("\uff08", query_brief(p$args, bank()), "\uff09"))
      )
    }

    output$result_head <- shiny::renderUI({
      r <- results()
      back <- back_link()
      if (nrow(r) == 0L) {
        h <- zero_result_hint(bank(), args())
        return(shiny::tagList(back, kitDesign::kit_callout(
          warn = TRUE,
          # 原文：沒有符合的題目： | 。
          shiny::tags$strong(paste0("\u6c92\u6709\u7b26\u5408\u7684\u984c\u76ee\uff1a", h$reason, "\u3002")),
          if (length(h$tries)) shiny::tagList(
            # 原文：可以試試：
            shiny::tags$br(), "\u53ef\u4ee5\u8a66\u8a66\uff1a",
            shiny::tags$ul(lapply(h$tries, shiny::tags$li))
          # 原文：按左邊的「清除條件」重新開始。
          ) else shiny::tagList(shiny::tags$br(), "\u6309\u5de6\u908a\u7684\u300c\u6e05\u9664\u689d\u4ef6\u300d\u91cd\u65b0\u958b\u59cb\u3002"),
          if (nrow(h$dims)) shiny::tagList(
            # 原文：名稱含這個詞的構面：
            "\u540d\u7a31\u542b\u9019\u500b\u8a5e\u7684\u69cb\u9762\uff1a",
            lapply(seq_len(nrow(h$dims)), function(i) shiny::tagList(
              # 原文：、
              if (i > 1L) "\u3001",
              # 原文： →
              shiny::actionLink(ns(paste0("zdim_go_", i)), paste0(h$dims$dimName[i], " \u2192"))
            ))
          )
        )))
      }
      domain <- store$domain()
      shiny::tagList(
        back,
        shiny::p(result_sentence(r, n_hidden())),
        shiny::div(
          class = "kit-buttons",
          kitDesign::kit_download_button(
            # 原文：下載查詢結果
            session, "download", "\u4e0b\u8f09\u67e5\u8a62\u7d50\u679c",
            # 原文：題項查詢_
            filename = paste0("\u984c\u9805\u67e5\u8a62_", bank_domain_label(domain) %||% domain, "_",
                              format(kit_today()), ".xlsx"),
            content = function(file) {
              b <- shiny::isolate(bank())
              rr <- shiny::isolate(results())
              src <- shiny::isolate(store$source())
              ver <- if (identical(src$kind, "builtin")) kit_version() else src$label
              if (nrow(shiny::isolate(store$changes())) > 0L) {
                # 原文：（含還沒下載的改動）
                ver <- paste0(ver %||% "", "\uff08\u542b\u9084\u6c92\u4e0b\u8f09\u7684\u6539\u52d5\uff09")
              }
              writexl::write_xlsx(
                query_workbook(b, rr, shiny::isolate(args()), domain = domain, version = ver),
                file
              )
            },
            # 有條件、有結果了，下載才是主要按鈕
            class = if (has_query(args())) "btn-primary"
          )
        )
      )
    })

    # ---- 結果表；點一列 → 這一題 ---------------------------------------------
    sel <- shiny::reactiveVal(NULL)  # list(id, ver)
    # 別的分頁或網址送來的那一題：條件還在更新的途中，結果暫時沒有它也先不要收起來
    pinned <- FALSE

    output$table <- DT::renderDT({
      r <- results()
      shiny::req(nrow(r) > 0L)
      s <- shiny::isolate(sel())
      hit <- if (is.null(s)) integer() else
        which(r$itemID == s$id & r$itemVer == s$ver)
      df <- data.frame(itemID = r$itemID, itemVer = r$itemVer, block = r$block,
                       label = collapse_newlines(r$label), n_waves = r$n_waves,
                       stringsAsFactors = FALSE)
      # 原文：波次數
      names(df)[5] <- "\u6ce2\u6b21\u6578"
      kitDesign::kit_dt(
        df, selection = list(mode = "single", target = "row", selected = hit),
        page_length = SEARCH_PAGE_LENGTH, wrap_cols = "label",
        options = if (length(hit)) list(
          displayStart = (hit[1] - 1L) %/% SEARCH_PAGE_LENGTH * SEARCH_PAGE_LENGTH
        ) else list()
      )
    })

    shiny::observeEvent(input$table_rows_selected, {
      i <- input$table_rows_selected[1]
      r <- results()
      if (!is.na(i) && i <= nrow(r)) {
        pinned <<- FALSE
        sel(list(id = r$itemID[i], ver = r$itemVer[i]))
      }
    })
    # 條件改了、選的那題不在結果裡 → 收起細節
    shiny::observeEvent(results(), {
      s <- sel()
      r <- results()
      if (is.null(s)) return()
      if (any(r$itemID == s$id & r$itemVer == s$ver)) pinned <<- FALSE
      else if (!pinned) sel(NULL)
    }, ignoreInit = TRUE)

    detail <- shiny::reactive({
      s <- sel()
      if (is.null(s)) return(NULL)
      item_detail(bank(), s$id, s$ver)
    })
    # 題庫的「要注意」清單（bank_audit()）：題庫改了才重算
    audit <- shiny::reactive(bank_audit(bank()))

    # 連結（構面、其他版本、同題幹的題、0 題提示裡的構面）：id 用序號，觀察器用到幾個開幾個
    links <- shiny::reactiveValues(dim = character(), ver = integer(), stem = list(),
                                   zdim = character())
    n_obs <- c(dim = 0L, ver = 0L, stem = 0L, zdim = 0L)
    link_ids <- character()
    follow_link <- function(kind, i) {
      if (kind %in% c("dim", "zdim")) {
        d <- links[[kind]][i]
        if (!is.na(d)) goto("dims", dim = d)
      } else if (kind == "ver") {
        v <- links$ver[i]
        s <- sel()
        # 結果沒變，不用 pinned：條件一改、這題不在結果裡就收起來
        if (!is.na(v) && !is.null(s)) sel(list(id = s$id, ver = v))
      } else if (kind == "stem") {
        it <- links$stem[[i]]
        if (!is.null(it)) sel(it)
      }
    }
    ensure_link_observers <- function(kind, n) {
      while (n_obs[[kind]] < n) {
        i <- n_obs[[kind]] + 1L
        local({
          ii <- i
          kk <- kind
          shiny::observeEvent(input[[paste0(kk, "_go_", ii)]], follow_link(kk, ii),
                              ignoreInit = TRUE)
        })
        n_obs[[kind]] <<- i
        link_ids <<- c(link_ids, paste0(kind, "_go_", i))
        session$setBookmarkExclude(c(SEARCH_BOOKMARK_EXCLUDE, link_ids))
      }
    }
    shiny::observe({
      d <- detail()
      links$dim <- if (is.null(d)) character() else d$dims$dimID
      links$ver <- if (is.null(d)) integer() else as.integer(d$other_versions$itemVer)
      ss <- if (is.null(d)) NULL else utils::head(d$same_stem, SAME_STEM_SHOW)
      links$stem <- if (is.null(ss)) list() else
        lapply(seq_len(nrow(ss)), function(i) list(id = ss$itemID[i], ver = ss$itemVer[i]))
      shiny::isolate({
        ensure_link_observers("dim", length(links$dim))
        ensure_link_observers("ver", length(links$ver))
        ensure_link_observers("stem", length(links$stem))
      })
    })
    shiny::observe({
      r <- results()
      links$zdim <- if (nrow(r) > 0L) character() else dims_matching(bank(), args()$keyword)$dimID
      shiny::isolate(ensure_link_observers("zdim", length(links$zdim)))
    })

    shiny::observeEvent(input$edit_go, {
      s <- sel()
      if (!is.null(s)) goto("edit", item = s$id, ver = s$ver)
    })

    output$detail <- shiny::renderUI({
      d <- detail()
      if (is.null(d)) {
        if (nrow(results()) == 0L) return(NULL)
        # 原文：點表格的一列，這裡會出現那一題的細節。
        return(shiny::p(class = "kit-hint", "\u9ede\u8868\u683c\u7684\u4e00\u5217\uff0c\u9019\u88e1\u6703\u51fa\u73fe\u90a3\u4e00\u984c\u7684\u7d30\u7bc0\u3002"))
      }
      a <- audit()
      it <- d$item
      notes <- a$message[a$itemID %in% it$itemID &
                           (is.na(a$itemVer) | a$itemVer == as.integer(it$itemVer))]
      uid <- paste0(KIT_DOMAIN_INSTRUMENT[[store$domain()]], ".", it$itemID)
      kit <- kit_builtin()
      history <- if (uid %in% kit$items$item_uid) {
        shiny::div(
          class = "kit-buttons",
          kitDesign::kit_download_button(
            # 原文：下載沿革單（Word） | 沿革單_
            session, "dl_history", "\u4e0b\u8f09\u6cbf\u9769\u55ae\uff08Word\uff09",
            filename = paste0("\u6cbf\u9769\u55ae_", it$itemID, ".docx"),
            content = function(file) item_history_docx(item_history(kit, uid), file)
          )
        )
      }
      # kitsUI 步驟參數（沒有匯入過就是 NULL）
      steps <- if (uid %in% kit$items$item_uid) item_steps_ui(kit, uid, as.integer(it$itemVer))
      shiny::tagList(item_detail_ui(d, ns, notes), history, steps)
    })

    # ---- 細節列 ---------------------------------------------------------------
    # 原文：沒有符合的題目，先放寬左邊的條件。
    no_items <- "\u6c92\u6709\u7b26\u5408\u7684\u984c\u76ee\uff0c\u5148\u653e\u5bec\u5de6\u908a\u7684\u689d\u4ef6\u3002"
    output$timeline <- shiny::renderUI({
      d <- detail()
      shiny::req(d)
      timeline_full_ui(d)
    })
    output$xw_dl <- shiny::renderUI({
      shiny::div(
        class = "kit-buttons",
        kitDesign::kit_download_button(
          # 原文：下載跨波次對照總表（Excel） | 跨波次對照總表_
          session, "dl_xw", "\u4e0b\u8f09\u8de8\u6ce2\u6b21\u5c0d\u7167\u7e3d\u8868\uff08Excel\uff09",
          filename = paste0("\u8de8\u6ce2\u6b21\u5c0d\u7167\u7e3d\u8868_", store$domain(), "_", format(kit_today(), "%Y%m%d"), ".xlsx"),
          content = function(file) {
            writexl::write_xlsx(version_grid_workbook(bank(), store$domain()), file)
          },
          class = kitDesign::kit_btn_class("secondary")
        ),
        # 原文：全部題目（含已停用）× 全部波次；M03、M36 各一張工作表，格子寫那一波問的版本（v1、v2…）。
        shiny::span(class = "kit-hint", "\u5168\u90e8\u984c\u76ee\uff08\u542b\u5df2\u505c\u7528\uff09\u00d7 \u5168\u90e8\u6ce2\u6b21\uff1bM03\u3001M36 \u5404\u4e00\u5f35\u5de5\u4f5c\u8868\uff0c\u683c\u5b50\u5beb\u90a3\u4e00\u6ce2\u554f\u7684\u7248\u672c\uff08v1\u3001v2\u2026\uff09\u3002")
      )
    })

    heat_by <- shiny::reactive(if (identical(input$heat_by, "dim")) "dim" else "block")
    heat_cohort <- shiny::reactive(input$heat_cohort %||% "")
    counts <- shiny::reactive({
      b <- bank_cohort(bank(), heat_cohort())
      r <- results()
      r <- r[item_key(r$itemID, r$itemVer) %in% item_key(b$itemBank$itemID, b$itemBank$itemVer), , drop = FALSE]
      wave_counts(b, r, by = heat_by())
    })
    # plotly 圖只在「各波次題數」打開時才放進頁面。在收起來（或整個分頁藏起來）的
    # <details> 裡畫 plotly 會量到 0 寬、在瀏覽器丟出 "Something went wrong with axis
    # scaling"；Shiny 一批更新裡有一個輸出出錯，後面的輸出（結果的一句話、表、這一題）
    # 就都沒套上，畫面停在舊的結果。從構面查詢跳過來時就是這樣。
    # 靠 clientData 的 hidden 判斷不夠：分頁切換的那一批裡，hidden 還是舊的值。
    output$heat_ui <- shiny::renderUI({
      if (!isTRUE(input$heat_open)) return(NULL)
      m <- counts()
      if (nrow(m) == 0L) {
        return(shiny::p(class = "kit-hint", if (heat_by() == "dim" && nrow(results()) > 0L)
          # 原文：這些題目都還沒有歸到構面。
          "\u9019\u4e9b\u984c\u76ee\u90fd\u9084\u6c92\u6709\u6b78\u5230\u69cb\u9762\u3002" else no_items))
      }
      plotly::plotlyOutput(ns("heat"), height = paste0(180L + 22L * nrow(m), "px"))
    })
    output$heat_note <- shiny::renderUI({
      # 原文：構面 | 區塊
      what <- if (heat_by() == "dim") "\u69cb\u9762" else "\u5340\u584a"
      co <- heat_cohort()
      shiny::p(class = "kit-hint", paste0(
        # 原文：題庫： | 世代： | 全部 | 。
        "\u984c\u5eab\uff1a", bank_domain_label(store$domain()), "\uff1b\u4e16\u4ee3\uff1a", if (nzchar(co)) co else "\u5168\u90e8", "\u3002",
        # 原文：每一格是一個 | 在一個波次有幾題，顏色越深題數越多；空白表示那一波次沒有。
        "\u6bcf\u4e00\u683c\u662f\u4e00\u500b", what, "\u5728\u4e00\u500b\u6ce2\u6b21\u6709\u5e7e\u984c\uff0c\u984f\u8272\u8d8a\u6df1\u984c\u6578\u8d8a\u591a\uff1b\u7a7a\u767d\u8868\u793a\u90a3\u4e00\u6ce2\u6b21\u6c92\u6709\u3002",
        # 原文：列依第一次出現的波次排。
        "\u5217\u4f9d\u7b2c\u4e00\u6b21\u51fa\u73fe\u7684\u6ce2\u6b21\u6392\u3002",
        # 原文：一題可以在幾個構面裡，每個都算。
        if (heat_by() == "dim") "\u4e00\u984c\u53ef\u4ee5\u5728\u5e7e\u500b\u69cb\u9762\u88e1\uff0c\u6bcf\u500b\u90fd\u7b97\u3002"
      ))
    })
    output$heat <- plotly::renderPlotly({
      # 收合時不畫：瀏覽器說「看得到」（打開「各波次題數」）以後才畫。
      # 在收起來的區塊裡畫 plotly 會量到 0 寬、丟出錯誤，同一批的其他更新（結果表）也跟著不見。
      shiny::req(isTRUE(input$heat_open))
      hidden <- session$clientData[[paste0("output_", ns("heat"), "_hidden")]]
      shiny::req(identical(hidden, FALSE))
      m <- counts()
      shiny::req(nrow(m) > 0L)
      wave_heatmap(m)
    })

    kitDesign::kit_code_panel_server("code", shiny::reactive({
      a <- args()
      src <- store$source()
      # 原文：^[^：]*：
      file <- if (identical(src$kind, "file")) sub("^[^\uff1a]*\uff1a", "", src$label) else NULL
      code <- do.call(query_code, c(list(domain = store$domain()), a, list(file = file)))
      if (nrow(store$changes()) > 0L) {
        # 原文：# 注意：畫面上的題庫有還沒下載的改動，這段程式讀的是改動前的題庫。\n
        code <- paste0("# \u6ce8\u610f\uff1a\u756b\u9762\u4e0a\u7684\u984c\u5eab\u6709\u9084\u6c92\u4e0b\u8f09\u7684\u6539\u52d5\uff0c\u9019\u6bb5\u7a0b\u5f0f\u8b80\u7684\u662f\u6539\u52d5\u524d\u7684\u984c\u5eab\u3002\n",
                       code)
      }
      code
    }))

    # ---- 清除條件、別的分頁送來的請求 ----------------------------------------
    set_conditions <- function(a = list()) {
      kw <- a$keyword %||% ""
      shiny::updateTextInput(session, "keyword", value = kw)
      keyword(kw)
      shiny::updateRadioButtons(session, "fields", selected = a$fields %||% "text")
      shiny::updateRadioButtons(session, "keyword_match", selected = a$keyword_match %||% "all")
      shiny::updateRadioButtons(session, "wave_match", selected = a$wave_match %||% "any")
      shiny::updateRadioButtons(session, "dim_match", selected = a$dim_match %||% "any")
      for (nm in c("waves", "dims", "blocks", "item_types", "respondents")) {
        shiny::updateSelectizeInput(session, nm, selected = a[[nm]] %||% character(0))
      }
      shiny::updateCheckboxInput(session, "include_deprecated",
                                 value = isTRUE(a$include_deprecated))
    }
    clear_conditions <- function(kw = "", include_deprecated = FALSE) {
      set_conditions(list(keyword = kw, include_deprecated = include_deprecated))
    }
    shiny::observeEvent(input$clear, {
      clear_conditions()
      prev_query(NULL)
      pinned <<- FALSE
      sel(NULL)
    })
    shiny::observeEvent(input$back_go, {
      p <- prev_query()
      if (is.null(p)) return()
      prev_query(NULL)
      jump_keyword <<- NULL
      set_conditions(p$args)
      pinned <<- !is.null(p$sel)
      sel(p$sel)
    })

    shiny::observeEvent(request(), {
      r <- request()
      if (!identical(r$tab, "search")) return()
      if (!is.null(r$args$keyword) && is.null(r$args$item)) {
        kw <- paste(as.character(r$args$keyword), collapse = " ")
        remember_query()
        jump_keyword <<- kw
        clear_conditions(kw = kw)
        pinned <<- FALSE
        sel(NULL)
        return()
      }
      if (is.null(r$args$item)) return()
      id <- as.character(r$args$item)
      remember_query()
      jump_keyword <<- id
      ib <- bank()$itemBank
      vers <- ib[ib$itemID == id, c("itemVer", "deprecated"), drop = FALSE]
      if (nrow(vers) == 0L) {
        clear_conditions(kw = id)
        sel(NULL)
        return()
      }
      ver <- suppressWarnings(as.integer(r$args$ver))
      if (length(ver) != 1L || is.na(ver) || !ver %in% vers$itemVer) {
        active <- vers$itemVer[is.na(vers$deprecated) | vers$deprecated != 1L]
        ver <- max(if (length(active)) active else vers$itemVer)
      }
      dep <- isTRUE(vers$deprecated[vers$itemVer == ver][1] == 1L)
      clear_conditions(kw = id, include_deprecated = dep)
      pinned <<- TRUE
      sel(list(id = id, ver = ver))
    })

    # ---- 書籤：條件是 input，自動進網址；另外記住打開的那一題 ----------------
    session$setBookmarkExclude(SEARCH_BOOKMARK_EXCLUDE)
    session$onBookmark(function(state) {
      s <- sel()
      if (!is.null(s)) state$values$item <- list(id = s$id, ver = s$ver)
    })
    session$onRestore(function(state) {
      s <- state$values$item
      if (!is.null(s$id)) {
        pinned <<- TRUE
        sel(list(id = as.character(s$id), ver = as.integer(s$ver)))
      }
    })

    invisible(list(results = results, selected = sel))
  })
}

# DT 自己的 input 與按鈕不進網址
SEARCH_BOOKMARK_EXCLUDE <- c(
  "clear", "heat_open", "edit_go", "back_go",
  as.vector(outer(c("table", "xw"), c(
    "_rows_selected", "_rows_current", "_rows_all", "_state", "_search",
    "_search_columns", "_cell_clicked", "_cells_selected", "_columns_selected",
    "_row_last_clicked", "_cell_info"
  ), paste0))
)

# 「這一題」最多列幾個同題幹的題
SAME_STEM_SHOW <- 10L

# 波次代碼的白話（放在「這一頁做什麼」裡）
WAVE_CODE_NOTE <- paste0(
  # 原文：波次代碼：M03M036 是 M03 世代（3 個月起追蹤）的孩子 36 個月大時的調查；M36 是 36 個月起追蹤的世代；
  "\u6ce2\u6b21\u4ee3\u78bc\uff1aM03M036 \u662f M03 \u4e16\u4ee3\uff083 \u500b\u6708\u8d77\u8ffd\u8e64\uff09\u7684\u5b69\u5b50 36 \u500b\u6708\u5927\u6642\u7684\u8abf\u67e5\uff1bM36 \u662f 36 \u500b\u6708\u8d77\u8ffd\u8e64\u7684\u4e16\u4ee3\uff1b",
  # 原文：G01 是國小一年級。舊代碼（M036大、M036小）也可以在「施測」頁搜尋。波次選單裡每個代碼後面都有白話。
  "G01 \u662f\u570b\u5c0f\u4e00\u5e74\u7d1a\u3002\u820a\u4ee3\u78bc\uff08M036\u5927\u3001M036\u5c0f\uff09\u4e5f\u53ef\u4ee5\u5728\u300c\u65bd\u6e2c\u300d\u9801\u641c\u5c0b\u3002\u6ce2\u6b21\u9078\u55ae\u88e1\u6bcf\u500b\u4ee3\u78bc\u5f8c\u9762\u90fd\u6709\u767d\u8a71\u3002"
)

# 一個查詢的簡短說法（「回到剛才的查詢」旁邊）：不寫「不限」的條件，最多兩段
query_brief <- function(a, bank) {
  parts <- search_settings_parts(a, bank)
  # 原文：不限$
  parts <- parts[!grepl("\u4e0d\u9650$", parts)]
  paste(utils::head(parts, 2L), collapse = " \u00b7 ")
}

# 有沒有設任何條件（沒有就是「全部題目」）
has_query <- function(a) {
  length(keyword_terms(a$keyword)) > 0L || length(a$waves) > 0L || length(a$dims) > 0L ||
    length(a$blocks) > 0L || length(a$item_types) > 0L || length(a$respondents) > 0L ||
    isTRUE(a$include_deprecated)
}

# 「共 37 題（不含 5 題已停用），來自 12 個題幹（…）。以下每頁 20 題。」
result_sentence <- function(r, n_hidden = 0L) {
  n <- nrow(r)
  stems <- unique(r$stemID[!is.na(r$stemID) & nzchar(trimws(r$stemID))])
  paste0(
    # 原文：共  |  題
    "\u5171 ", kitDesign::kit_num(n), " \u984c",
    # 原文：（不含  |  題已停用）
    if (isTRUE(n_hidden > 0L)) paste0("\uff08\u4e0d\u542b ", kitDesign::kit_num(n_hidden), " \u984c\u5df2\u505c\u7528\uff09"),
    # 原文：，來自 
    if (length(stems)) paste0("\uff0c\u4f86\u81ea ", kitDesign::kit_num(length(stems)),
                              # 原文： 個題幹（共用同一句問句的小題算一個題幹）
                              " \u500b\u984c\u5e79\uff08\u5171\u7528\u540c\u4e00\u53e5\u554f\u53e5\u7684\u5c0f\u984c\u7b97\u4e00\u500b\u984c\u5e79\uff09"),
    # 原文：，以下每頁  |  題
    if (n > SEARCH_PAGE_LENGTH) paste0("\uff0c\u4ee5\u4e0b\u6bcf\u9801 ", SEARCH_PAGE_LENGTH, " \u984c") else "",
    # 原文：。點一列看這一題的細節。
    "\u3002\u9ede\u4e00\u5217\u770b\u9019\u4e00\u984c\u7684\u7d30\u7bc0\u3002"
  )
}

# 多行文字 → 中間夾 <br>
text_lines <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L || is.na(x[1L]) || !nzchar(trimws(x[1L]))) return(NULL)
  lines <- strsplit(x[1L], "\r?\n")[[1]]
  shiny::tagList(lapply(seq_along(lines), function(i) {
    shiny::tagList(if (i > 1L) shiny::tags$br(), lines[i])
  }))
}

# label_diff() 的結果 → <del>／<ins>
diff_tags <- function(d) {
  # 黏在一起寫成一段 HTML：tagList 會在標籤之間加空白，中文裡會多出空格
  txt <- htmltools::htmlEscape(d$text)
  open <- c(same = "", removed = "<del>", added = "<ins>")[d$type]
  close <- c(same = "", removed = "</del>", added = "</ins>")[d$type]
  shiny::HTML(paste0(open, txt, close, collapse = ""))
}

# 一題出現的波次：「M003小–M072小（13 個波次）」
wave_span <- function(w) {
  # 原文：沒有出現在任何波次
  if (length(w) == 0L) return("\u6c92\u6709\u51fa\u73fe\u5728\u4efb\u4f55\u6ce2\u6b21")
  # 原文：（1 個波次）
  if (length(w) == 1L) return(paste0(col_display_labels(w), "\uff081 \u500b\u6ce2\u6b21\uff09"))
  # 原文：–
  paste0(col_display_labels(w[1L]), "\u2013", col_display_labels(w[length(w)]),
         # 原文：（ |  個波次）
         "\uff08", length(w), " \u500b\u6ce2\u6b21\uff09")
}

# 「這一題」：不加框；版本差異收合
#' 一題的「各版本出現的波次」：整頁寬；有世代（M03、M36）就一個世代一張表
#' @noRd
timeline_full_ui <- function(d) {
  it <- d$item
  tl <- d$timeline
  if (is.null(tl) || !nrow(tl)) return(NULL)
  waves <- setdiff(names(tl), c("itemVer", "deprecated"))
  # 一格寫那一波問的版本（v1、v2…），跟構面查詢、總表同一套（version_grid()）
  # version_timeline() 給 ✓；舊格式（0/1）也認得
  asked <- function(x) as.character(x) %in% c("\u2713", "1")
  cell <- vapply(waves, function(w) {
    v <- tl$itemVer[asked(tl[[w]])]
    if (length(v)) paste0("v", sort(v), collapse = "/") else ""
  }, character(1))
  co <- wave_cohort(waves)
  groups <- if (all(nzchar(co))) split(waves, factor(co, levels = unique(co))) else list(waves)
  tables <- lapply(names(groups) %||% "", function(g) {
    w <- groups[[if (nzchar(g)) g else 1L]]
    # 原文：題號
    tab <- cbind(stats::setNames(data.frame(it$itemID, stringsAsFactors = FALSE), "\u984c\u865f"),
                 as.data.frame(as.list(cell[w]), stringsAsFactors = FALSE, optional = TRUE))
    names(tab) <- col_display_labels(names(tab))
    shiny::tagList(
      # 原文： 世代
      if (nzchar(g)) shiny::h5(paste0(g, " \u4e16\u4ee3")),
      kitDesign::kit_table_scroll(kitDesign::kit_static_table(tab))
    )
  })
  dep <- tl$itemVer[!is.na(tl$deprecated) & tl$deprecated == 1L]
  hint <- paste0(
    # 原文：每一格是那一波問的版本（v1、v2…），空白是那一波沒問。這一題是第  |  版
    "\u6bcf\u4e00\u683c\u662f\u90a3\u4e00\u6ce2\u554f\u7684\u7248\u672c\uff08v1\u3001v2\u2026\uff09\uff0c\u7a7a\u767d\u662f\u90a3\u4e00\u6ce2\u6c92\u554f\u3002\u9019\u4e00\u984c\u662f\u7b2c ",
    it$itemVer, " \u7248",
    # 原文：；已停用：
    if (length(dep)) paste0("\uff1b\u5df2\u505c\u7528\uff1a", paste0("v", dep, collapse = "\u3001")),
    "\u3002")
  shiny::tagList(
    # 原文：「 | 」各版本出現的波次
    shiny::h4(paste0("\u300c", it$itemID, "\u300d\u5404\u7248\u672c\u51fa\u73fe\u7684\u6ce2\u6b21")),
    shiny::p(class = "kit-hint", hint),
    tables
  )
}

item_detail_ui <- function(d, ns, notes = character()) {
  it <- d$item
  field <- function(label, value) {
    if (is.null(value) || !length(value)) return(NULL)
    if (is.character(value) && length(value) == 1L) {
      value <- text_lines(value)
      if (is.null(value)) return(NULL)
    }
    shiny::p(shiny::tags$strong(label), value)
  }
  is_dep <- function(x) isTRUE(suppressWarnings(as.integer(x)) == 1L)
  # 原文：是 | 否
  yes_no <- function(x) if (isTRUE(suppressWarnings(as.integer(x)) == 1L)) "\u662f" else "\u5426"
  chr1 <- function(x) {
    x <- as.character(x %||% "")[1L]
    if (is.na(x)) "" else x
  }
  meta <- c(
    # 原文：區塊 
    if (nzchar(chr1(it$block))) paste0("\u5340\u584a ", it$block),
    # 原文：題型 
    if (nzchar(chr1(it$itemType))) paste0("\u984c\u578b ", item_type_label(it$itemType)),
    # 原文：填答者 
    if (nzchar(chr1(it$respondent))) paste0("\u586b\u7b54\u8005 ", it$respondent),
    # 原文：反向計分
    if (isTRUE(suppressWarnings(as.integer(it$reverseScored)) == 1L)) "\u53cd\u5411\u8a08\u5206",
    # 原文：已停用
    if (is_dep(it$deprecated)) "\u5df2\u505c\u7528"
  )
  dims <- d$dims
  ov <- d$other_versions
  ss <- d$same_stem
  note <- split_note(it$note)
  nr <- format_no_response(it$noResponse)

  # 版本 × 波次
  timeline_ui <- if (nrow(ov)) {
    shiny::tagList(
      shiny::p(
        # 原文：看其他版本：
        "\u770b\u5176\u4ed6\u7248\u672c\uff1a",
        lapply(seq_len(nrow(ov)), function(i) shiny::tagList(
          # 原文：、
          if (i > 1L) "\u3001",
          # 原文：第  |  版 →
          shiny::actionLink(ns(paste0("ver_go_", i)), paste0("\u7b2c ", ov$itemVer[i], " \u7248 \u2192"))
        ))
      )
    )
  }

  # 各版本差在哪（收合）
  diff_ui <- if (nrow(ov)) {
    # 原文：不作答代碼 | 反向計分
    field_names <- c(noResponse = "\u4e0d\u4f5c\u7b54\u4ee3\u78bc", reverseScored = "\u53cd\u5411\u8a08\u5206",
                     # 原文：題型 | 填答者
                     itemType = "\u984c\u578b", respondent = "\u586b\u7b54\u8005")
    show_val <- function(f, v) {
      v <- chr1(v)
      if (f == "reverseScored") return(yes_no(v))
      if (f == "itemType") v <- item_type_label(v)
      if (f == "noResponse") v <- format_no_response(v)
      # 原文：（空白）
      if (nzchar(v)) v else "\uff08\u7a7a\u767d\uff09"
    }
    kitDesign::kit_detail(
      # 原文：各版本差在哪
      "\u5404\u7248\u672c\u5dee\u5728\u54ea",
      shiny::tags$ul(lapply(seq_len(nrow(ov)), function(i) {
        o <- ov[i, , drop = FALSE]
        df <- o$differs[[1]]
        shiny::tags$li(
          # 原文：第  |  版 | （已停用） | ：
          paste0("\u7b2c ", o$itemVer, " \u7248", if (is_dep(o$deprecated)) "\uff08\u5df2\u505c\u7528\uff09", "\uff1a",
                 wave_span(o$waves[[1]])),
          # 原文：內容和這一版相同。
          if (length(df) == 0L) shiny::tagList(shiny::tags$br(), "\u5167\u5bb9\u548c\u9019\u4e00\u7248\u76f8\u540c\u3002"),
          if ("label" %in% df) shiny::tagList(
            # 原文：題目差在哪：
            shiny::tags$br(), "\u984c\u76ee\u5dee\u5728\u54ea\uff1a", diff_tags(label_diff(o$label, it$label))),
          if ("value" %in% df) shiny::tagList(
            # 原文：選項差在哪：
            shiny::tags$br(), "\u9078\u9805\u5dee\u5728\u54ea\uff1a",
            diff_tags(label_diff(gsub("\r?\n", " ", chr1(o$value)), gsub("\r?\n", " ", chr1(it$value))))
          ),
          lapply(intersect(names(field_names), df), function(f) shiny::tagList(
            shiny::tags$br(),
            # 原文：： |  → 
            paste0(field_names[[f]], "\uff1a", show_val(f, o[[f]]), " \u2192 ", show_val(f, it[[f]]),
                   # 原文：（這一版）
                   "\uff08\u9019\u4e00\u7248\uff09")
          ))
        )
      })),
      # 原文：刪除線是另一版才有的字，底線是這一版才有的字。
      shiny::p(class = "kit-hint", "\u522a\u9664\u7dda\u662f\u53e6\u4e00\u7248\u624d\u6709\u7684\u5b57\uff0c\u5e95\u7dda\u662f\u9019\u4e00\u7248\u624d\u6709\u7684\u5b57\u3002")
    )
  }

  # 同一個題幹的其他題
  stem_ui <- if (nrow(ss)) {
    show <- utils::head(ss, SAME_STEM_SHOW)
    shiny::tagList(
      # 原文：同一個題幹的其他題（ |  題）：
      shiny::p(shiny::tags$strong(paste0("\u540c\u4e00\u500b\u984c\u5e79\u7684\u5176\u4ed6\u984c\uff08", kitDesign::kit_num(nrow(ss)), " \u984c\uff09\uff1a")),
               # 原文：題幹是幾個小題共用的問句：「 | 」
               shiny::span(class = "kit-hint", paste0("\u984c\u5e79\u662f\u5e7e\u500b\u5c0f\u984c\u5171\u7528\u7684\u554f\u53e5\uff1a\u300c", chr1(it$stemID), "\u300d"))),
      shiny::tags$ul(lapply(seq_len(nrow(show)), function(i) shiny::tags$li(
        shiny::actionLink(ns(paste0("stem_go_", i)), show$itemID[i]),
        # 原文： · 
        paste0(" ", trunc_label(collapse_newlines(show$label[i]), 40), " \u00b7 ",
               wave_span(show$waves[[i]]),
               # 原文： · 已停用
               if (isTRUE(show$deprecated[i] == 1L)) " \u00b7 \u5df2\u505c\u7528" else "")
      ))),
      if (nrow(ss) > SAME_STEM_SHOW)
        # 原文：還有 
        shiny::p(class = "kit-hint", paste0("\u9084\u6709 ", kitDesign::kit_num(nrow(ss) - SAME_STEM_SHOW),
                                            # 原文： 題；用關鍵字找這段題幹可以看到全部。
                                            " \u984c\uff1b\u7528\u95dc\u9375\u5b57\u627e\u9019\u6bb5\u984c\u5e79\u53ef\u4ee5\u770b\u5230\u5168\u90e8\u3002"))
    )
  }

  shiny::div(
    # 原文：這一題： | （第  |  版）
    shiny::tags$h4(paste0("\u9019\u4e00\u984c\uff1a", it$itemID, "\uff08\u7b2c ", it$itemVer, " \u7248\uff09")),
    shiny::p(class = "kit-hint",
             # 原文： ·  |  · 
             if (length(meta)) paste0(paste(meta, collapse = " \u00b7 "), " \u00b7 "),
             # 原文：改這一題 →
             shiny::actionLink(ns("edit_go"), "\u6539\u9019\u4e00\u984c \u2192")),
    # 這一題在 bank_audit() 裡的問題：有才顯示，一行一個
    if (length(notes)) shiny::p(
      # 原文：! 要注意（ |  項）：
      shiny::tags$strong(paste0("! \u8981\u6ce8\u610f\uff08", length(notes), " \u9805\uff09\uff1a")),
      lapply(seq_along(notes), function(i) shiny::tagList(shiny::tags$br(), notes[i]))
    ),
    # 原文：題目：
    field("\u984c\u76ee\uff1a", it$label),
    # 原文：選項：
    field("\u9078\u9805\uff1a", it$value),
    # 原文：不作答代碼：
    if (nzchar(nr)) shiny::p(shiny::tags$strong("\u4e0d\u4f5c\u7b54\u4ee3\u78bc\uff1a"), nr),
    if (length(note)) shiny::p(
      # 原文：備註：
      shiny::tags$strong("\u5099\u8a3b\uff1a"),
      lapply(seq_along(note), function(i) shiny::tagList(if (i > 1L) shiny::tags$br(), note[i]))
    ),
    # 原文：出現的波次（ |  個波次）：
    shiny::p(shiny::tags$strong(paste0("\u51fa\u73fe\u7684\u6ce2\u6b21\uff08", length(d$waves), " \u500b\u6ce2\u6b21\uff09\uff1a")),
             # 原文：、
             if (length(d$waves)) paste(col_display_labels(d$waves, plain = TRUE), collapse = "\u3001")
             # 原文：沒有出現在任何波次。
             else "\u6c92\u6709\u51fa\u73fe\u5728\u4efb\u4f55\u6ce2\u6b21\u3002"),
    shiny::p(
      # 原文：構面：
      shiny::tags$strong("\u69cb\u9762\uff1a"),
      # 原文：還沒有歸到任何構面。
      if (nrow(dims) == 0L) "\u9084\u6c92\u6709\u6b78\u5230\u4efb\u4f55\u69cb\u9762\u3002" else
        shiny::tagList(lapply(seq_len(nrow(dims)), function(i) shiny::tagList(
          # 原文：、
          if (i > 1L) "\u3001",
          # 原文： →
          shiny::actionLink(ns(paste0("dim_go_", i)), paste0(dims$dimName[i], " \u2192"))
        )))
    ),
    # 原文：點構面名稱，到「構面查詢」看那個構面。
    if (nrow(dims)) shiny::p(class = "kit-hint", "\u9ede\u69cb\u9762\u540d\u7a31\uff0c\u5230\u300c\u69cb\u9762\u67e5\u8a62\u300d\u770b\u90a3\u500b\u69cb\u9762\u3002"),
    timeline_ui,
    diff_ui,
    stem_ui
  )
}

# 區塊（或構面）× 波次的熱圖：格子裡寫題數，波次放上面
wave_heatmap <- function(m) {
  z <- m
  z[z == 0L] <- NA
  txt <- ifelse(m == 0L, "", as.character(m))
  x <- col_display_labels(colnames(m))
  y <- rownames(m)
  p <- plotly::plot_ly(
    x = x, y = y, z = z, type = "heatmap",
    colorscale = kitDesign::kit_chart_sequential(),
    xgap = 1, ygap = 1,
    text = txt, texttemplate = "%{text}",
    # 原文：%{y}<br>%{x}：%{z} 題<extra></extra>
    hovertemplate = "%{y}<br>%{x}\uff1a%{z} \u984c<extra></extra>",
    # 原文：題數
    colorbar = list(title = list(text = "\u984c\u6578"))
  )
  p <- plotly::layout(
    p,
    xaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = x,
                 side = "top"),
    yaxis = list(title = "", type = "category", autorange = "reversed",
                 categoryorder = "array", categoryarray = y)
  )
  kitDesign::kit_plotly_style(p)
}
