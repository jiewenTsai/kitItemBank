# R/bank_store.R — 題庫選擇列與全 app 共用的題庫狀態
#
# 分頁列正下方的一列：選題庫（家長／兒童青少年／教保人員）、現在用的是哪一份
# （內建題庫或打開的 Excel）、打開題庫 Excel、回到內建題庫。
#
# bank_store_server() 回傳「store」，三個分頁都用它：
#   store$domain()      目前的題庫代碼（"parent"…）
#   store$bank()        目前的題庫（含編修頁還沒下載的改動）
#   store$base()        改動前的題庫（內建或打開的 Excel）
#   store$source()      list(kind = "builtin" / "file", label = 畫面上的字)
#   store$changes()     改動紀錄（bank_diff(base, bank)）
#   store$set_bank(b)   編修頁把改好的題庫放回來（只在這個瀏覽器分頁裡）
#   store$revert()      復原目前題庫的所有改動
#
# 每個題庫各自保留自己的改動，切換題庫不會弄丟；有改動時離開頁面前先問一次。

bank_bar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "kit-scopebar",
    shiny::radioButtons(
      # 原文：題庫
      ns("domain"), "\u984c\u5eab",
      choices = BANK_DOMAINS, selected = "parent", inline = TRUE
    ),
    shiny::div(
      class = "kit-scopebar-line",
      shiny::uiOutput(ns("source_line"), inline = TRUE),
      shiny::div(
        class = "kit-scopebar-actions",
        shiny::fileInput(
          ns("open_xlsx"), NULL,
          # 原文：打開題庫 Excel…
          buttonLabel = "\u6253\u958b\u984c\u5eab Excel\u2026", placeholder = NULL,
          accept = ".xlsx", width = "auto"
        ),
        shiny::uiOutput(ns("back_ui"), inline = TRUE)
      )
    ),
    shiny::uiOutput(ns("open_check"))
  )
}

bank_store_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # 每個題庫一格：base、current、source；第一次選到時才讀內建題庫
    slots <- shiny::reactiveValues()
    pending <- shiny::reactiveVal(NULL)  # 打開了、還沒確認的 Excel

    builtin_source <- function() {
      v <- bank_version()
      list(kind = "builtin",
           # 原文：內建題庫
           label = paste0("\u5167\u5efa\u984c\u5eab", if (!is.na(v)) paste0(" ", v)))
    }

    domain <- shiny::reactive(input$domain %||% "parent")
    # 第一次選到一個題庫時才讀；在 observer 裡寫 slots，不在 reactive 裡寫
    # （reactive 一邊算一邊改自己讀的值，輸出會在算到一半時又被作廢）
    shiny::observeEvent(domain(), {
      d <- domain()
      if (is.null(slots[[d]])) {
        b <- load_bank(d)
        slots[[d]] <- list(base = b, current = b, source = builtin_source())
      }
    }, priority = 100)
    slot <- shiny::reactive({
      s <- slots[[domain()]]
      shiny::req(s)
      s
    })
    bank <- shiny::reactive(slot()$current)
    base <- shiny::reactive(slot()$base)
    source <- shiny::reactive(slot()$source)
    changes <- shiny::reactive(bank_diff(base(), bank()))

    set_bank <- function(new_bank) {
      d <- shiny::isolate(domain())
      s <- shiny::isolate(slots[[d]])
      s$current <- new_bank
      slots[[d]] <- s
      invisible(new_bank)
    }
    revert <- function() {
      d <- shiny::isolate(domain())
      s <- shiny::isolate(slots[[d]])
      s$current <- s$base
      slots[[d]] <- s
    }

    # 任何一個題庫有改動時，離開頁面前先問一次
    any_changes <- shiny::reactive({
      any(vapply(unname(BANK_DOMAINS), function(d) {
        s <- slots[[d]]
        !is.null(s) && !identical(s$current, s$base)
      }, logical(1)))
    })
    shiny::observe(kitDesign::kit_unload_guard(any_changes()))

    output$source_line <- shiny::renderUI({
      n <- nrow(changes())
      shiny::tags$span(
        # 原文： · 
        shiny::tags$strong(source()$label), " \u00b7 ", bank_summary_text(bank()),
        if (n > 0L) shiny::tags$span(
          class = "kit-scopebar-todo",
          # 原文： · 有  |  處改動還沒下載（在「題庫編修」下載）
          paste0(" \u00b7 \u6709 ", kitDesign::kit_num(n), " \u8655\u6539\u52d5\u9084\u6c92\u4e0b\u8f09\uff08\u5728\u300c\u984c\u5eab\u7de8\u4fee\u300d\u4e0b\u8f09\uff09")
        )
      )
    })
    output$back_ui <- shiny::renderUI({
      if (identical(source()$kind, "file")) {
        # 原文：回到內建題庫
        kitDesign::kit_button(session$ns("back_builtin"), "\u56de\u5230\u5167\u5efa\u984c\u5eab")
      }
    })

    # 打開 Excel：先檢查，列出檢查清單，按「用這份題庫」才換
    shiny::observeEvent(input$open_xlsx, {
      f <- input$open_xlsx
      res <- tryCatch({
        b <- read_bank_xlsx(f$datapath)
        list(bank = b, name = f$name,
             check = check_bank(b, reference = load_bank(domain())))
      }, error = function(e) list(error = e, name = f$name))
      pending(res)
    })
    output$open_check <- shiny::renderUI({
      p <- pending()
      if (is.null(p)) return(NULL)
      ns <- session$ns
      if (!is.null(p$error)) {
        return(shiny::tagList(
          # 原文：打開「 | 」
          kitDesign::kit_error_status(p$error, action = paste0("\u6253\u958b\u300c", p$name, "\u300d")),
          # 原文：關掉這個訊息
          kitDesign::kit_button(ns("open_cancel"), "\u95dc\u6389\u9019\u500b\u8a0a\u606f")
        ))
      }
      shiny::div(
        class = "kit-scopebar-panel",
        # 原文：打開「 | 」當作 | 題庫？
        shiny::h5(paste0("\u6253\u958b\u300c", p$name, "\u300d\u7576\u4f5c", bank_domain_label(domain()), "\u984c\u5eab\uff1f")),
        kitDesign::kit_checklist(p$check$items),
        shiny::div(
          class = "kit-buttons",
          # 原文：用這份題庫
          if (p$check$ok) shiny::actionButton(ns("open_ok"), "\u7528\u9019\u4efd\u984c\u5eab"),
          # 原文：不用，維持現在的題庫
          kitDesign::kit_button(ns("open_cancel"), "\u4e0d\u7528\uff0c\u7dad\u6301\u73fe\u5728\u7684\u984c\u5eab")
        ),
        if (!identical(shiny::isolate(slot()$current), shiny::isolate(slot()$base))) {
          shiny::div(class = "kit-hint",
                     # 原文：現在的題庫有還沒下載的改動；換成這份以後，那些改動會不見。
                     "\u73fe\u5728\u7684\u984c\u5eab\u6709\u9084\u6c92\u4e0b\u8f09\u7684\u6539\u52d5\uff1b\u63db\u6210\u9019\u4efd\u4ee5\u5f8c\uff0c\u90a3\u4e9b\u6539\u52d5\u6703\u4e0d\u898b\u3002")
        }
      )
    })
    shiny::observeEvent(input$open_ok, {
      p <- pending()
      shiny::req(p, is.null(p$error))
      slots[[domain()]] <- list(
        base = p$bank, current = p$bank,
        # 原文：你打開的檔案：
        source = list(kind = "file", label = paste0("\u4f60\u6253\u958b\u7684\u6a94\u6848\uff1a", p$name))
      )
      pending(NULL)
      kitDesign::kit_notify(kitDesign::kit_done_status(
        # 原文：已換成「 | 」： | 。
        paste0("\u5df2\u63db\u6210\u300c", p$name, "\u300d\uff1a", bank_summary_text(p$bank), "\u3002")
      ))
    })
    shiny::observeEvent(input$open_cancel, pending(NULL))
    shiny::observeEvent(domain(), pending(NULL), ignoreInit = TRUE)
    shiny::observeEvent(input$back_builtin, {
      b <- load_bank(domain())
      slots[[domain()]] <- list(base = b, current = b, source = builtin_source())
    })

    list(
      domain = domain, bank = bank, base = base, source = source,
      changes = changes, set_bank = set_bank, revert = revert
    )
  })
}
