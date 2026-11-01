# R/page_home.R — 待辦事項（首頁）：題庫裡有哪些地方要注意、該找誰
#
# 由上往下：這一頁做什麼 → 要注意（自己能改｜要問老師｜要問統計人員｜要請技術人員改，
# 每一筆連到對的分頁）。看的是三個題庫合在一起（這一頁只拿到 kit，不跟著題庫列）。
# 計算都在 R/home.R；這一頁不改題庫、不在伺服器寫檔。

# 每種要注意在首頁最多列幾筆（其他的下載完整清單看）
HOME_SHOW <- 20L

#' The home page (待辦事項): UI
#'
#' @param id Module id.
#' @return UI.
#' @export
page_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kitDesign::kit_tab_guide(
      # 原文：看看題庫裡有哪些地方要注意，該由誰處理。
      goal = "\u770b\u770b\u984c\u5eab\u88e1\u6709\u54ea\u4e9b\u5730\u65b9\u8981\u6ce8\u610f\uff0c\u8a72\u7531\u8ab0\u8655\u7406\u3002",
      steps = c(
        # 原文：要注意的地方分成四堆：自己能改、要問老師、要問統計人員、要請技術人員改。
        "\u8981\u6ce8\u610f\u7684\u5730\u65b9\u5206\u6210\u56db\u5806\uff1a\u81ea\u5df1\u80fd\u6539\u3001\u8981\u554f\u8001\u5e2b\u3001\u8981\u554f\u7d71\u8a08\u4eba\u54e1\u3001\u8981\u8acb\u6280\u8853\u4eba\u54e1\u6539\u3002",
        # 原文：點開一種，看每一筆在說什麼；點一筆，會打開能看它的分頁。
        "\u9ede\u958b\u4e00\u7a2e\uff0c\u770b\u6bcf\u4e00\u7b46\u5728\u8aaa\u4ec0\u9ebc\uff1b\u9ede\u4e00\u7b46\uff0c\u6703\u6253\u958b\u80fd\u770b\u5b83\u7684\u5206\u9801\u3002",
        # 原文：要全部的清單，按「下載要注意的完整清單」。
        "\u8981\u5168\u90e8\u7684\u6e05\u55ae\uff0c\u6309\u300c\u4e0b\u8f09\u8981\u6ce8\u610f\u7684\u5b8c\u6574\u6e05\u55ae\u300d\u3002"
      ),
      # 原文：要注意的完整清單（Excel，一個處理人一張表）。
      output = "\u8981\u6ce8\u610f\u7684\u5b8c\u6574\u6e05\u55ae\uff08Excel\uff0c\u4e00\u500b\u8655\u7406\u4eba\u4e00\u5f35\u8868\uff09\u3002",
      # 原文：這一頁只看不改。
      note = "\u9019\u4e00\u9801\u53ea\u770b\u4e0d\u6539\u3002"
    ),
    # 原文：要注意
    shiny::h4("\u8981\u6ce8\u610f"),
    shiny::uiOutput(ns("audit_head")),
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(ns("audit_self"))),
      shiny::column(3, shiny::uiOutput(ns("audit_teacher"))),
      shiny::column(3, shiny::uiOutput(ns("audit_stats"))),
      shiny::column(3, shiny::uiOutput(ns("audit_tech")))
    )
  )
}

#' The home page (待辦事項): server
#'
#' @param id Module id.
#' @param kit The kit (17 tables): a reactive or a plain list.
#' @param goto Switch tabs: `goto("search", item = , uid = )`, `goto("dims", dim = )`,
#'   `goto("admin", admin = , dataset = )`.
#' @return Invisibly, a list with the reactive `audit`.
#' @export
page_home_server <- function(id, kit, goto) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    kit_now <- if (shiny::is.reactive(kit)) kit else function() kit
    ha <- shiny::reactive(home_audit(kit_now()))
    li_style <- "overflow-wrap: anywhere; margin-bottom: .25rem;"
    link <- function(value, label) {
      shiny::tags$a(href = "#", onclick = sprintf(
        "Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;",
        ns("go"), value), label)
    }

    output$audit_head <- shiny::renderUI({
      a <- ha()
      need <- AUDIT_KIT_CHECKS$label[match(HOME_NEEDS_MAINTAINER$check, AUDIT_KIT_CHECKS$check)]
      shiny::tagList(
        shiny::p(class = "kit-hint", paste0(
          # 原文：這裡是三個題庫（家長、兒童青少年、教保人員）合在一起看，不管上面的題庫列選哪一個。
          "\u9019\u88e1\u662f\u4e09\u500b\u984c\u5eab\uff08\u5bb6\u9577\u3001\u5152\u7ae5\u9752\u5c11\u5e74\u3001\u6559\u4fdd\u4eba\u54e1\uff09\u5408\u5728\u4e00\u8d77\u770b\uff0c\u4e0d\u7ba1\u4e0a\u9762\u7684\u984c\u5eab\u5217\u9078\u54ea\u4e00\u500b\u3002",
          # 原文：題庫裡共有  |  筆要注意的地方，依誰能處理分成四堆。
          "\u984c\u5eab\u88e1\u5171\u6709 ", kitDesign::kit_num(nrow(a)), " \u7b46\u8981\u6ce8\u610f\u7684\u5730\u65b9\uff0c\u4f9d\u8ab0\u80fd\u8655\u7406\u5206\u6210\u56db\u5806\u3002",
          # 原文：點一筆，會打開能看它的分頁；每種最多列  |  筆，全部的請下載清單。
          "\u9ede\u4e00\u7b46\uff0c\u6703\u6253\u958b\u80fd\u770b\u5b83\u7684\u5206\u9801\uff1b\u6bcf\u7a2e\u6700\u591a\u5217 ", HOME_SHOW, " \u7b46\uff0c\u5168\u90e8\u7684\u8acb\u4e0b\u8f09\u6e05\u55ae\u3002")),
        shiny::p(class = "kit-hint", paste0(
          # 原文：這裡只用題庫本身檢查。「 | 」「
          "\u9019\u88e1\u53ea\u7528\u984c\u5eab\u672c\u8eab\u6aa2\u67e5\u3002\u300c", paste(need, collapse = "\u300d\u300c"),
          # 原文：」要對照 .sav 表頭和 0.2 題庫，請維護者在自己電腦上跑 audit_kit()。
          "\u300d\u8981\u5c0d\u7167 .sav \u8868\u982d\u548c 0.2 \u984c\u5eab\uff0c\u8acb\u7dad\u8b77\u8005\u5728\u81ea\u5df1\u96fb\u8166\u4e0a\u8dd1 audit_kit()\u3002")),
        shiny::div(class = "kit-buttons", kitDesign::kit_download_button(
          # 原文：下載要注意的完整清單（Excel）
          session, "dl_audit", "\u4e0b\u8f09\u8981\u6ce8\u610f\u7684\u5b8c\u6574\u6e05\u55ae\uff08Excel\uff09",
          # 原文：要注意清單_
          paste0("\u8981\u6ce8\u610f\u6e05\u55ae_", format(kit_today(), "%Y%m%d"), ".xlsx"),
          function(file) writexl::write_xlsx(home_audit_sheets(ha()), file),
          class = kitDesign::kit_btn_class("secondary")))
      )
    })
    owner_ui <- function(owner) {
      a <- ha()
      idx <- which(a$owner == owner)
      head <- shiny::p(style = "margin-top: 1rem;", shiny::tags$strong(
        # 原文：（ | ）
        paste0(HOME_OWNERS[[owner]], "\uff08", kitDesign::kit_num(length(idx)), "\uff09")))
      # 原文：沒有。
      if (!length(idx)) return(shiny::tagList(head, shiny::p(class = "kit-hint", "\u6c92\u6709\u3002")))
      checks <- unique(a$check[idx])
      shiny::tagList(head, kitDesign::kit_details_row(lapply(checks, function(cc) {
        ii <- idx[a$check[idx] == cc]
        more <- length(ii) - HOME_SHOW
        ii <- utils::head(ii, HOME_SHOW)
        kitDesign::kit_detail(
          # 原文：（ | ）
          paste0(a$check_label[ii[1]], "\uff08", kitDesign::kit_num(length(ii) + max(more, 0L)), "\uff09"),
          shiny::tags$ul(lapply(ii, function(i) {
            # 原文：：
            txt <- paste0(a$key[i], "\uff1a", a$detail[i])
            shiny::tags$li(style = li_style, if (nzchar(a$tab[i])) link(i, txt) else txt)
          })),
          # 原文：還有  |  筆，請下載完整清單。
          if (more > 0L) shiny::p(class = "kit-hint", paste0("\u9084\u6709 ", kitDesign::kit_num(more), " \u7b46\uff0c\u8acb\u4e0b\u8f09\u5b8c\u6574\u6e05\u55ae\u3002"))
        )
      })))
    }
    output$audit_self <- shiny::renderUI(owner_ui("self"))
    output$audit_teacher <- shiny::renderUI(owner_ui("teacher"))
    output$audit_stats <- shiny::renderUI(owner_ui("stats"))
    output$audit_tech <- shiny::renderUI(owner_ui("tech"))
    shiny::observeEvent(input$go, {
      a <- ha()
      i <- suppressWarnings(as.integer(input$go))
      if (length(i) != 1L || is.na(i) || i < 1L || i > nrow(a)) return()
      r <- a[i, ]
      switch(r$tab,
             search = goto("search", item = r$target, uid = r$extra),
             dims = goto("dims", dim = r$target),
             admin = goto("admin", admin = r$target, dataset = r$extra),
             NULL)
    })

    invisible(list(audit = ha))
  })
}

#' 要注意的完整清單：一個處理人一張表
#' @noRd
home_audit_sheets <- function(ha) {
  out <- lapply(names(HOME_OWNERS), function(o) {
    a <- ha[ha$owner == o, , drop = FALSE]
    # 原文：類別 | 表 | 對象 | 說明
    data.frame("\u985e\u5225" = a$check_label, "\u8868" = a$table, "\u5c0d\u8c61" = a$key, "\u8aaa\u660e" = a$detail,
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  names(out) <- unname(HOME_OWNERS)
  out
}
