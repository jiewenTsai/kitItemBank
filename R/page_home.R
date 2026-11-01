# R/page_home.R — 我的工作（首頁）：現在有哪些事要做
#
# 由上往下：這一頁做什麼 → 等你審的修改包 → 要注意（自己能改｜要問老師｜要問統計人員，
# 每一筆連到對的分頁）→ 請教單（下載給老師填、上傳變成修改包）→ 交辦事項（只存在這次連線，
# 可以下載／上傳 CSV）。計算都在 R/home.R、R/consult.R；這一頁不改題庫、不在伺服器寫檔。

# 每種要注意在首頁最多列幾筆（其他的下載完整清單看）
HOME_SHOW <- 20L

#' The home page (我的工作): UI
#'
#' @param id Module id.
#' @return UI.
#' @export
page_home_ui <- function(id) {
  ns <- shiny::NS(id)
  wrap <- "overflow-wrap: anywhere;"
  shiny::tagList(
    kitDesign::kit_tab_guide(
      # 原文：看看現在有哪些事要做：等你審的修改包、題庫裡要注意的地方、交辦事項。
      goal = "\u770b\u770b\u73fe\u5728\u6709\u54ea\u4e9b\u4e8b\u8981\u505a\uff1a\u7b49\u4f60\u5be9\u7684\u4fee\u6539\u5305\u3001\u984c\u5eab\u88e1\u8981\u6ce8\u610f\u7684\u5730\u65b9\u3001\u4ea4\u8fa6\u4e8b\u9805\u3002",
      steps = c(
        # 原文：先看「等你審的修改包」：有人改了題庫，要你看過才會進題庫。
        "\u5148\u770b\u300c\u7b49\u4f60\u5be9\u7684\u4fee\u6539\u5305\u300d\uff1a\u6709\u4eba\u6539\u4e86\u984c\u5eab\uff0c\u8981\u4f60\u770b\u904e\u624d\u6703\u9032\u984c\u5eab\u3002",
        # 原文：再看「要注意」：分成自己能改、要問老師、要問統計人員三堆；點一筆，會打開對的分頁。
        "\u518d\u770b\u300c\u8981\u6ce8\u610f\u300d\uff1a\u5206\u6210\u81ea\u5df1\u80fd\u6539\u3001\u8981\u554f\u8001\u5e2b\u3001\u8981\u554f\u7d71\u8a08\u4eba\u54e1\u4e09\u5806\uff1b\u9ede\u4e00\u7b46\uff0c\u6703\u6253\u958b\u5c0d\u7684\u5206\u9801\u3002",
        # 原文：要問老師的事，下載「請教單」給老師填；填好後上傳，會做成修改包，一樣經過審核。
        "\u8981\u554f\u8001\u5e2b\u7684\u4e8b\uff0c\u4e0b\u8f09\u300c\u8acb\u6559\u55ae\u300d\u7d66\u8001\u5e2b\u586b\uff1b\u586b\u597d\u5f8c\u4e0a\u50b3\uff0c\u6703\u505a\u6210\u4fee\u6539\u5305\uff0c\u4e00\u6a23\u7d93\u904e\u5be9\u6838\u3002",
        # 原文：自己記的待辦寫在「交辦事項」，做完打勾；離開前記得下載存起來。
        "\u81ea\u5df1\u8a18\u7684\u5f85\u8fa6\u5beb\u5728\u300c\u4ea4\u8fa6\u4e8b\u9805\u300d\uff0c\u505a\u5b8c\u6253\u52fe\uff1b\u96e2\u958b\u524d\u8a18\u5f97\u4e0b\u8f09\u5b58\u8d77\u4f86\u3002"
      ),
      # 原文：要注意的完整清單（Excel）；請教單（Excel）；請教單做成的修改包（Excel）；交辦事項（CSV）。
      output = "\u8981\u6ce8\u610f\u7684\u5b8c\u6574\u6e05\u55ae\uff08Excel\uff09\uff1b\u8acb\u6559\u55ae\uff08Excel\uff09\uff1b\u8acb\u6559\u55ae\u505a\u6210\u7684\u4fee\u6539\u5305\uff08Excel\uff09\uff1b\u4ea4\u8fa6\u4e8b\u9805\uff08CSV\uff09\u3002",
      # 原文：這一頁只看不改：請教單和交辦事項都是下載給你，題庫要經過修改包的審核才會改。
      note = "\u9019\u4e00\u9801\u53ea\u770b\u4e0d\u6539\uff1a\u8acb\u6559\u55ae\u548c\u4ea4\u8fa6\u4e8b\u9805\u90fd\u662f\u4e0b\u8f09\u7d66\u4f60\uff0c\u984c\u5eab\u8981\u7d93\u904e\u4fee\u6539\u5305\u7684\u5be9\u6838\u624d\u6703\u6539\u3002"
    ),
    # 原文：等你審的修改包
    shiny::h4("\u7b49\u4f60\u5be9\u7684\u4fee\u6539\u5305"),
    shiny::uiOutput(ns("pending")),
    # 原文：要注意
    shiny::h4("\u8981\u6ce8\u610f"),
    shiny::uiOutput(ns("audit_head")),
    shiny::fluidRow(
      shiny::column(4, shiny::uiOutput(ns("audit_self"))),
      shiny::column(4, shiny::uiOutput(ns("audit_teacher"))),
      shiny::column(4, shiny::uiOutput(ns("audit_stats")))
    ),
    # 原文：請教單
    shiny::h4("\u8acb\u6559\u55ae"),
    shiny::uiOutput(ns("consult_head")),
    # 原文：誰決定（老師沒填時用這個名字）
    shiny::textInput(ns("consult_who"), "\u8ab0\u6c7a\u5b9a\uff08\u8001\u5e2b\u6c92\u586b\u6642\u7528\u9019\u500b\u540d\u5b57\uff09", width = "100%"),
    shiny::uiOutput(ns("consult_download")),
    # 原文：上傳填好的請教單
    shiny::fileInput(ns("consult_file"), "\u4e0a\u50b3\u586b\u597d\u7684\u8acb\u6559\u55ae",
                     # 原文：選擇檔案… | 還沒選
                     buttonLabel = "\u9078\u64c7\u6a94\u6848\u2026", placeholder = "\u9084\u6c92\u9078", accept = ".xlsx",
                     width = "100%"),
    shiny::uiOutput(ns("consult_result")),
    # 原文：交辦事項
    shiny::h4("\u4ea4\u8fa6\u4e8b\u9805"),
    shiny::uiOutput(ns("todo_note")),
    shiny::fluidRow(
      # 原文：要做什麼
      shiny::column(6, shiny::textInput(ns("todo_task"), "\u8981\u505a\u4ec0\u9ebc", width = "100%")),
      # 原文：誰做
      shiny::column(3, shiny::textInput(ns("todo_who"), "\u8ab0\u505a", width = "100%")),
      # 原文：期限
      shiny::column(3, shiny::textInput(ns("todo_due"), "\u671f\u9650", width = "100%",
                                        # 原文：例如 11/30
                                        placeholder = "\u4f8b\u5982 11/30"))
    ),
    # 原文：加進交辦事項
    shiny::div(class = "kit-buttons", kitDesign::kit_button(ns("todo_add"), "\u52a0\u9032\u4ea4\u8fa6\u4e8b\u9805", "primary")),
    shiny::div(style = wrap, shiny::uiOutput(ns("todo_list"))),
    shiny::uiOutput(ns("todo_download")),
    # 原文：上傳之前下載的交辦事項（會換掉目前的清單）
    shiny::fileInput(ns("todo_file"), "\u4e0a\u50b3\u4e4b\u524d\u4e0b\u8f09\u7684\u4ea4\u8fa6\u4e8b\u9805\uff08\u6703\u63db\u6389\u76ee\u524d\u7684\u6e05\u55ae\uff09",
                     # 原文：選擇檔案… | 還沒選
                     buttonLabel = "\u9078\u64c7\u6a94\u6848\u2026", placeholder = "\u9084\u6c92\u9078", accept = ".csv",
                     width = "100%")
  )
}

#' The home page (我的工作): server
#'
#' @param id Module id.
#' @param kit The kit (17 tables): a reactive or a plain list.
#' @param goto Switch tabs: `goto("search", item = , uid = )`, `goto("dims", dim = )`,
#'   `goto("admin", admin = , dataset = )`.
#' @param pending Change packages waiting for review: `NULL` (not connected yet), or a reactive
#'   / function returning a count or a data frame (one row per package; a `label` column is
#'   shown when present).
#' @param todo_store Where to keep to-dos: `NULL` keeps them in this session only; or a list
#'   with `load()` (returns a to-do data frame) and `save(todo)` (e.g. backed by GitHub).
#' @return Invisibly, a list with the reactives `audit` and `todo`.
#' @export
page_home_server <- function(id, kit, goto, pending = NULL, todo_store = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    kit_now <- if (shiny::is.reactive(kit)) kit else function() kit
    ha <- shiny::reactive(home_audit(kit_now()))
    day <- function() format(Sys.Date(), "%Y%m%d")
    li_style <- "overflow-wrap: anywhere; margin-bottom: .25rem;"
    link <- function(input_id, value, label) {
      shiny::tags$a(href = "#", onclick = sprintf(
        "Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;",
        ns(input_id), value), label)
    }

    # ── 等你審的修改包 ─────────────────────────────────
    output$pending <- shiny::renderUI({
      if (is.null(pending)) {
        # 原文：修改包的審核還沒接上；接上後，這裡會列出等你審的修改包。
        return(shiny::p(class = "kit-hint", "\u4fee\u6539\u5305\u7684\u5be9\u6838\u9084\u6c92\u63a5\u4e0a\uff1b\u63a5\u4e0a\u5f8c\uff0c\u9019\u88e1\u6703\u5217\u51fa\u7b49\u4f60\u5be9\u7684\u4fee\u6539\u5305\u3002"))
      }
      x <- pending()
      n <- if (is.data.frame(x)) nrow(x) else as.integer(x %||% 0L)
      # 原文：沒有等你審的修改包。
      if (!length(n) || is.na(n) || n == 0L) return(shiny::p("\u6c92\u6709\u7b49\u4f60\u5be9\u7684\u4fee\u6539\u5305\u3002"))
      shiny::tagList(
        # 原文： 份修改包等你審。
        shiny::p(shiny::tags$strong(paste0(kitDesign::kit_num(n), " \u4efd\u4fee\u6539\u5305\u7b49\u4f60\u5be9\u3002"))),
        if (is.data.frame(x) && "label" %in% names(x)) {
          shiny::tags$ul(lapply(x$label, function(l) shiny::tags$li(style = li_style, l)))
        }
      )
    })

    # ── 要注意 ─────────────────────────────────────────
    output$audit_head <- shiny::renderUI({
      a <- ha()
      need <- AUDIT_KIT_CHECKS$label[match(HOME_NEEDS_MAINTAINER$check, AUDIT_KIT_CHECKS$check)]
      shiny::tagList(
        shiny::p(class = "kit-hint", paste0(
          # 原文：題庫裡共有  |  筆要注意的地方，依誰能處理分成三堆。
          "\u984c\u5eab\u88e1\u5171\u6709 ", kitDesign::kit_num(nrow(a)), " \u7b46\u8981\u6ce8\u610f\u7684\u5730\u65b9\uff0c\u4f9d\u8ab0\u80fd\u8655\u7406\u5206\u6210\u4e09\u5806\u3002",
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
          paste0("\u8981\u6ce8\u610f\u6e05\u55ae_", day(), ".xlsx"),
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
            shiny::tags$li(style = li_style, if (nzchar(a$tab[i])) link("go", i, txt) else txt)
          })),
          # 原文：還有  |  筆，請下載完整清單。
          if (more > 0L) shiny::p(class = "kit-hint", paste0("\u9084\u6709 ", kitDesign::kit_num(more), " \u7b46\uff0c\u8acb\u4e0b\u8f09\u5b8c\u6574\u6e05\u55ae\u3002"))
        )
      })))
    }
    output$audit_self <- shiny::renderUI(owner_ui("self"))
    output$audit_teacher <- shiny::renderUI(owner_ui("teacher"))
    output$audit_stats <- shiny::renderUI(owner_ui("stats"))
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

    # ── 請教單 ─────────────────────────────────────────
    rows <- shiny::reactive(consult_rows(kit_now(), audit = audit_kit(kit_now())))
    output$consult_head <- shiny::renderUI({
      r <- rows()
      lab <- vapply(CONSULT_TYPES, `[[`, "", "label")
      n <- table(factor(r$qtype, levels = names(CONSULT_TYPES)))
      n <- n[n > 0]
      shiny::tagList(
        shiny::p(paste0(
          # 原文：要請老師判斷的有  |  個問題
          "\u8981\u8acb\u8001\u5e2b\u5224\u65b7\u7684\u6709 ", kitDesign::kit_num(nrow(r)), " \u500b\u554f\u984c",
          # 原文：： | 、 | 。
          if (length(n)) paste0("\uff1a", paste0(lab[names(n)], " ", as.integer(n), collapse = "\u3001")), "\u3002")),
        shiny::p(class = "kit-hint", paste0(
          # 原文：請教單是 Excel，一列一個問題：「決定」用下拉選單選，「理由」「誰決定」自由寫。
          "\u8acb\u6559\u55ae\u662f Excel\uff0c\u4e00\u5217\u4e00\u500b\u554f\u984c\uff1a\u300c\u6c7a\u5b9a\u300d\u7528\u4e0b\u62c9\u9078\u55ae\u9078\uff0c\u300c\u7406\u7531\u300d\u300c\u8ab0\u6c7a\u5b9a\u300d\u81ea\u7531\u5beb\u3002",
          # 原文：填好後在下面上傳，會做成修改包（只是一份改動清單，不會直接改題庫），再走審核。
          "\u586b\u597d\u5f8c\u5728\u4e0b\u9762\u4e0a\u50b3\uff0c\u6703\u505a\u6210\u4fee\u6539\u5305\uff08\u53ea\u662f\u4e00\u4efd\u6539\u52d5\u6e05\u55ae\uff0c\u4e0d\u6703\u76f4\u63a5\u6539\u984c\u5eab\uff09\uff0c\u518d\u8d70\u5be9\u6838\u3002"))
      )
    })
    output$consult_download <- shiny::renderUI({
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        # 原文：這台伺服器沒有裝 openxlsx，做不出有下拉選單的請教單。
        return(kitDesign::kit_callout(warn = TRUE, "\u9019\u53f0\u4f3a\u670d\u5668\u6c92\u6709\u88dd openxlsx\uff0c\u505a\u4e0d\u51fa\u6709\u4e0b\u62c9\u9078\u55ae\u7684\u8acb\u6559\u55ae\u3002"))
      }
      shiny::div(class = "kit-buttons", kitDesign::kit_download_button(
        # 原文：下載請教單（Excel） | 請教單_
        session, "dl_consult", "\u4e0b\u8f09\u8acb\u6559\u55ae\uff08Excel\uff09", paste0("\u8acb\u6559\u55ae_", day(), ".xlsx"),
        function(file) consult_workbook(kit_now(), rows(), file),
        class = kitDesign::kit_btn_class("secondary")))
    })
    consult_res <- shiny::reactive({
      f <- input$consult_file
      shiny::req(f)
      tryCatch({
        ans <- read_consult(f$datapath)
        res <- consult_patch(kit_now(), ans, decided_by = trimws(input$consult_who %||% ""))
        res$n_answers <- nrow(ans)
        res
      }, error = function(e) list(error = conditionMessage(e)))
    })
    output$consult_result <- shiny::renderUI({
      res <- consult_res()
      if (!is.null(res$error)) {
        # 原文：讀不了這個檔案：
        return(kitDesign::kit_callout(warn = TRUE, paste0("\u8b80\u4e0d\u4e86\u9019\u500b\u6a94\u6848\uff1a", res$error)))
      }
      if (res$n_answers == 0L) {
        # 原文：這份請教單還沒有填任何決定或理由。
        return(shiny::p("\u9019\u4efd\u8acb\u6559\u55ae\u9084\u6c92\u6709\u586b\u4efb\u4f55\u6c7a\u5b9a\u6216\u7406\u7531\u3002"))
      }
      nt <- res$notes
      shiny::tagList(
        shiny::p(paste0(
          # 原文：讀到  |  個回覆：做成 
          "\u8b80\u5230 ", kitDesign::kit_num(res$n_answers), " \u500b\u56de\u8986\uff1a\u505a\u6210 ",
          # 原文： 處改動；
          kitDesign::kit_num(nrow(res$patch)), " \u8655\u6539\u52d5\uff1b",
          # 原文： 個回覆不用改題庫（維持原狀、再討論或要人工處理）。
          kitDesign::kit_num(nrow(nt)), " \u500b\u56de\u8986\u4e0d\u7528\u6539\u984c\u5eab\uff08\u7dad\u6301\u539f\u72c0\u3001\u518d\u8a0e\u8ad6\u6216\u8981\u4eba\u5de5\u8655\u7406\uff09\u3002")),
        if (nrow(res$patch)) shiny::div(class = "kit-buttons", kitDesign::kit_download_button(
          # 原文：下載修改包（Excel） | 修改包_請教單_
          session, "dl_consult_patch", "\u4e0b\u8f09\u4fee\u6539\u5305\uff08Excel\uff09", paste0("\u4fee\u6539\u5305_\u8acb\u6559\u55ae_", day(), ".xlsx"),
          function(file) writexl::write_xlsx(consult_patch_sheets(consult_res()), file),
          class = kitDesign::kit_btn_class("primary"))),
        if (nrow(nt)) kitDesign::kit_details_row(kitDesign::kit_detail(
          # 原文：不用改題庫的回覆（ | ）
          paste0("\u4e0d\u7528\u6539\u984c\u5eab\u7684\u56de\u8986\uff08", kitDesign::kit_num(nrow(nt)), "\uff09"),
          shiny::tags$ul(lapply(seq_len(nrow(nt)), function(i) shiny::tags$li(
            style = li_style,
            # 原文：：
            paste0(nt$id[i], " ", nt$key[i], "\uff1a", nt$why[i],
                   # 原文：（ | ）
                   if (nzchar(nt$reason[i])) paste0("\uff08", nt$reason[i], "\uff09") else "")
          )))
        ))
      )
    })

    # ── 交辦事項 ───────────────────────────────────────
    todo <- shiny::reactiveVal(empty_todo())
    if (!is.null(todo_store)) {
      t0 <- tryCatch(todo_store$load(), error = function(e) NULL)
      if (is.data.frame(t0)) todo(t0)
    }
    set_todo <- function(x) {
      todo(x)
      if (!is.null(todo_store)) {
        ok <- tryCatch({ todo_store$save(x); TRUE }, error = function(e) FALSE)
        # 原文：交辦事項沒有存成功；請先下載 CSV 留著。
        if (!ok) kitDesign::kit_notify("\u4ea4\u8fa6\u4e8b\u9805\u6c92\u6709\u5b58\u6210\u529f\uff1b\u8acb\u5148\u4e0b\u8f09 CSV \u7559\u8457\u3002", session)
      }
    }
    output$todo_note <- shiny::renderUI({
      if (is.null(todo_store)) {
        kitDesign::kit_callout(warn = TRUE, paste0(
          # 原文：交辦事項只存在這個瀏覽器分頁：重新整理或關掉就不見了。
          "\u4ea4\u8fa6\u4e8b\u9805\u53ea\u5b58\u5728\u9019\u500b\u700f\u89bd\u5668\u5206\u9801\uff1a\u91cd\u65b0\u6574\u7406\u6216\u95dc\u6389\u5c31\u4e0d\u898b\u4e86\u3002",
          # 原文：app 不在伺服器上存檔，請按「下載交辦事項」存成 CSV，下次再上傳。
          "app \u4e0d\u5728\u4f3a\u670d\u5668\u4e0a\u5b58\u6a94\uff0c\u8acb\u6309\u300c\u4e0b\u8f09\u4ea4\u8fa6\u4e8b\u9805\u300d\u5b58\u6210 CSV\uff0c\u4e0b\u6b21\u518d\u4e0a\u50b3\u3002"))
      } else {
        # 原文：交辦事項改了就會存起來；也可以下載 CSV 留一份。
        shiny::p(class = "kit-hint", "\u4ea4\u8fa6\u4e8b\u9805\u6539\u4e86\u5c31\u6703\u5b58\u8d77\u4f86\uff1b\u4e5f\u53ef\u4ee5\u4e0b\u8f09 CSV \u7559\u4e00\u4efd\u3002")
      }
    })
    shiny::observeEvent(input$todo_add, {
      task <- trimws(input$todo_task %||% "")
      if (!nzchar(task)) {
        # 原文：先寫「要做什麼」。
        kitDesign::kit_notify("\u5148\u5beb\u300c\u8981\u505a\u4ec0\u9ebc\u300d\u3002", session)
        return()
      }
      set_todo(todo_add(todo(), task, input$todo_who, input$todo_due))
      shiny::updateTextInput(session, "todo_task", value = "")
    })
    shiny::observeEvent(input$todo_toggle, {
      id <- as.character(input$todo_toggle)
      td <- todo()
      if (!id %in% td$id) return()
      set_todo(todo_done(td, id, td$done[td$id == id] != "1"))
    })
    shiny::observeEvent(input$todo_file, {
      x <- tryCatch(read_todo_csv(input$todo_file$datapath), error = function(e) NULL)
      if (is.null(x)) {
        # 原文：讀不了這個檔案；請上傳從這裡下載的交辦事項 CSV。
        kitDesign::kit_notify("\u8b80\u4e0d\u4e86\u9019\u500b\u6a94\u6848\uff1b\u8acb\u4e0a\u50b3\u5f9e\u9019\u88e1\u4e0b\u8f09\u7684\u4ea4\u8fa6\u4e8b\u9805 CSV\u3002", session)
        return()
      }
      set_todo(x)
      # 原文：讀進  |  件交辦事項。
      kitDesign::kit_notify(paste0("\u8b80\u9032 ", nrow(x), " \u4ef6\u4ea4\u8fa6\u4e8b\u9805\u3002"), session)
    })
    todo_line <- function(td, i) {
      # 原文：誰做：
      extra <- c(if (nzchar(td$who[i])) paste0("\u8ab0\u505a\uff1a", td$who[i]),
                 # 原文：期限：
                 if (nzchar(td$due[i])) paste0("\u671f\u9650\uff1a", td$due[i]))
      # 原文：（ | ； | ）
      paste0(td$task[i], if (length(extra)) paste0("\uff08", paste(extra, collapse = "\uff1b"), "\uff09"))
    }
    output$todo_list <- shiny::renderUI({
      td <- todo()
      open <- which(td$done != "1")
      done <- which(td$done == "1")
      shiny::tagList(
        # 原文：沒有還沒做的交辦事項。
        if (!length(open)) shiny::p(class = "kit-hint", "\u6c92\u6709\u9084\u6c92\u505a\u7684\u4ea4\u8fa6\u4e8b\u9805\u3002")
        else shiny::tags$ul(lapply(open, function(i) shiny::tags$li(
          # 原文：［做完了］
          style = li_style, todo_line(td, i), " ", link("todo_toggle", td$id[i], "\uff3b\u505a\u5b8c\u4e86\uff3d")))),
        if (length(done)) kitDesign::kit_details_row(kitDesign::kit_detail(
          # 原文：已完成（ | ）
          paste0("\u5df2\u5b8c\u6210\uff08", length(done), "\uff09"),
          shiny::tags$ul(lapply(done, function(i) shiny::tags$li(
            style = li_style, shiny::tags$s(todo_line(td, i)), " ",
            # 原文：［還沒做完］
            link("todo_toggle", td$id[i], "\uff3b\u9084\u6c92\u505a\u5b8c\uff3d"))))
        ))
      )
    })
    output$todo_download <- shiny::renderUI({
      shiny::div(class = "kit-buttons", kitDesign::kit_download_button(
        # 原文：下載交辦事項（CSV） | 交辦事項_
        session, "dl_todo", "\u4e0b\u8f09\u4ea4\u8fa6\u4e8b\u9805\uff08CSV\uff09", paste0("\u4ea4\u8fa6\u4e8b\u9805_", day(), ".csv"),
        function(file) write_todo_csv(todo(), file),
        class = kitDesign::kit_btn_class("secondary")))
    })

    invisible(list(audit = ha, todo = todo))
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
