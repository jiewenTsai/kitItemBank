# R/page_edit.R — 題庫編修
#
# 全 app 唯一會改題庫的地方。由上往下：
#   要改什麼（題目內容／題目的構面／題目的波次／構面資訊）→ 一次只顯示一種
#   → 本次改動 → 還差 → [下載改好的題庫]（全頁唯一的主要按鈕）
#   → ▸ 改了哪些 ▸ 用 R 怎麼做 → 復原所有改動
# 改動只放在 store 裡（這個瀏覽器分頁）；app 不寫檔，改好的題庫用下載的。
# 計算都在 R/edit.R（set_assignment()、add_column()…），這裡只接畫面。

EDIT_WHAT <- c(
  # 原文：題目內容
  "\u984c\u76ee\u5167\u5bb9" = "item",
  # 原文：題目的構面
  "\u984c\u76ee\u7684\u69cb\u9762" = "itemDim",
  # 原文：構面資訊（名稱、別名、信度）
  "\u69cb\u9762\u8cc7\u8a0a\uff08\u540d\u7a31\u3001\u5225\u540d\u3001\u4fe1\u5ea6\uff09" = "meta"
)

page_edit_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kitDesign::kit_tab_guide(
      # 原文：改題目的構面、波次歸屬與構面資訊，下載改好的題庫。
      # 原文：改題目內容、題目的構面、構面資訊，下載修改包。
      goal = "\u6539\u984c\u76ee\u5167\u5bb9\u3001\u984c\u76ee\u7684\u69cb\u9762\u3001\u69cb\u9762\u8cc7\u8a0a\uff0c\u4e0b\u8f09\u4fee\u6539\u5305\u3002",
      steps = c(
        # 原文：選要改什麼：題目內容、題目的構面，或構面資訊。（題目出現在哪些施測以 .sav 和對照表為準，不在這裡改）
        "\u9078\u8981\u6539\u4ec0\u9ebc\uff1a\u984c\u76ee\u5167\u5bb9\u3001\u984c\u76ee\u7684\u69cb\u9762\uff0c\u6216\u69cb\u9762\u8cc7\u8a0a\u3002\uff08\u984c\u76ee\u51fa\u73fe\u5728\u54ea\u4e9b\u65bd\u6e2c\u4ee5 .sav \u548c\u5c0d\u7167\u8868\u70ba\u6e96\uff0c\u4e0d\u5728\u9019\u88e1\u6539\uff09",
        # 原文：題目內容：在「要注意」表點一列，或直接選一題一版；改好按「套用這一題的改動」。
        "\u984c\u76ee\u5167\u5bb9\uff1a\u5728\u300c\u8981\u6ce8\u610f\u300d\u8868\u9ede\u4e00\u5217\uff0c\u6216\u76f4\u63a5\u9078\u4e00\u984c\u4e00\u7248\uff1b\u6539\u597d\u6309\u300c\u5957\u7528\u9019\u4e00\u984c\u7684\u6539\u52d5\u300d\u3002",
        # 原文：選一個構面，在右邊的表點題目：點一下勾選，再點一下取消。
        "\u9078\u4e00\u500b\u69cb\u9762\uff0c\u5728\u53f3\u908a\u7684\u8868\u9ede\u984c\u76ee\uff1a\u9ede\u4e00\u4e0b\u52fe\u9078\uff0c\u518d\u9ede\u4e00\u4e0b\u53d6\u6d88\u3002",
        # 原文：看下面的「本次改動」，沒問題就下載修改包。
        "\u770b\u4e0b\u9762\u7684\u300c\u672c\u6b21\u6539\u52d5\u300d\uff0c\u6c92\u554f\u984c\u5c31\u4e0b\u8f09\u4fee\u6539\u5305\u3002"
      ),
      # 原文：修改包 Excel（說明、改動，和原始改動紀錄），交給題庫維護者套用。
      output = "\u4fee\u6539\u5305 Excel\uff08\u8aaa\u660e\u3001\u6539\u52d5\uff0c\u548c\u539f\u59cb\u6539\u52d5\u7d00\u9304\uff09\uff0c\u4ea4\u7d66\u984c\u5eab\u7dad\u8b77\u8005\u5957\u7528\u3002",
      # 原文：改動只存在這個瀏覽器分頁裡，app 不會寫回任何檔案；離開前記得下載。
      note = "\u6539\u52d5\u53ea\u5b58\u5728\u9019\u500b\u700f\u89bd\u5668\u5206\u9801\u88e1\uff0capp \u4e0d\u6703\u5beb\u56de\u4efb\u4f55\u6a94\u6848\uff1b\u96e2\u958b\u524d\u8a18\u5f97\u4e0b\u8f09\u3002"
    ),
    # 原文：要改什麼
    shiny::radioButtons(ns("what"), "\u8981\u6539\u4ec0\u9ebc", choices = EDIT_WHAT,
                        selected = "itemDim", inline = TRUE),
    shiny::tabsetPanel(
      id = ns("panel"), type = "hidden", selected = "itemDim",
      shiny::tabPanelBody("item", edit_item_ui(ns("item"))),
      shiny::tabPanelBody("itemDim", edit_assign_ui(ns("dim"), "itemDim")),
      shiny::tabPanelBody("meta", edit_meta_ui(ns("meta")))
    ),
    shiny::uiOutput(ns("summary")),
    shiny::uiOutput(ns("download_ui")),
    shiny::tags$p(
      class = "kit-hint",
      # 原文：下載後，把修改包交給審核的人，在「審核」頁上傳：看改前改後、檢查結果，沒問題就下載套用後的題庫。
      "\u4e0b\u8f09\u5f8c\uff0c\u628a\u4fee\u6539\u5305\u4ea4\u7d66\u5be9\u6838\u7684\u4eba\uff0c\u5728\u300c\u5be9\u6838\u300d\u9801\u4e0a\u50b3\uff1a\u770b\u6539\u524d\u6539\u5f8c\u3001\u6aa2\u67e5\u7d50\u679c\uff0c\u6c92\u554f\u984c\u5c31\u4e0b\u8f09\u5957\u7528\u5f8c\u7684\u984c\u5eab\u3002"
    ),
    kitDesign::kit_details_row(
      # 原文：改了哪些
      kitDesign::kit_detail("\u6539\u4e86\u54ea\u4e9b", DT::DTOutput(ns("changes_tbl"))),
      kitDesign::kit_code_panel(ns("code"))
    ),
    shiny::uiOutput(ns("revert_ui"))
  )
}

#' @param store bank_store_server() 的回傳
#' @param goto 切到別的分頁：goto("dims", dim = "…")
#' @param request 別的分頁送來的請求（reactive，list(tab, args, at)）
#' @noRd
page_edit_server <- function(id, store, goto, request) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dim_panel <- edit_assign_server("dim", store, "itemDim")
    edit_meta_server("meta", store)
    show_panel <- function(what) {
      shiny::updateRadioButtons(session, "what", selected = what)
      shiny::updateTabsetPanel(session, "panel", selected = what)
    }
    # 「要注意」表點到構面的問題 → 切到題目的構面、選那個構面
    item_panel <- edit_item_server("item", store, on_dim = function(d) {
      show_panel("itemDim")
      dim_panel$select(d)
    })

    shiny::observeEvent(input$what, {
      shiny::updateTabsetPanel(session, "panel", selected = input$what)
    })

    # 別的分頁送來：
    #   goto("edit", dim = "睡眠習慣")           → 題目的構面、選那個構面
    #   goto("edit", item = "famC21", ver = 1L)  → 題目內容、帶入那一題那一版
    shiny::observeEvent(request(), {
      r <- request()
      if (!identical(r$tab, "edit")) return()
      it <- r$args$item
      d <- r$args$dim
      if (is.character(it) && length(it) == 1L && nzchar(it)) {
        show_panel("item")
        item_panel$load(it, r$args$ver)
      } else if (is.character(d) && length(d) == 1L && nzchar(d)) {
        show_panel("itemDim")
        dim_panel$select(d)
      }
    })

    n_changes <- shiny::reactive(nrow(store$changes()))

    output$summary <- shiny::renderUI({
      if (n_changes() == 0L) return(NULL)
      shiny::tags$p(class = "kit-settings-line",
                    # 原文：本次改動：
                    paste0("\u672c\u6b21\u6539\u52d5\uff1a", edit_changes_summary(store$changes())))
    })

    output$download_ui <- shiny::renderUI({
      if (n_changes() == 0L) {
        return(shiny::tagList(
          # 原文：還沒有改動
          kitDesign::kit_ready_line("\u9084\u6c92\u6709\u6539\u52d5"),
          # 原文：下載修改包
          kitDesign::kit_button(ns("download_off"), "\u4e0b\u8f09\u4fee\u6539\u5305", "primary",
                                disabled = TRUE)
        ))
      }
      kitDesign::kit_download_button(
        # 原文：下載修改包
        session, "download", "\u4e0b\u8f09\u4fee\u6539\u5305",
        filename = edit_download_filename(store$domain()),
        content = function(file) {
          res <- kit_patch(kit_builtin(), store$base(), store$bank(), store$domain())
          writexl::write_xlsx(patch_workbook(res, store$changes(), store$domain()), file)
        },
        class = "btn-primary"
      )
    })

    output$changes_tbl <- DT::renderDT({
      # 原文：原本 | 改成
      kitDesign::kit_dt(store$changes(), page_length = 25L, wrap_cols = c("\u539f\u672c", "\u6539\u6210"))
    })

    output$code <- kitDesign::kit_render_code_panel(
      function() {
        d <- store$domain()
        kitDesign::kit_code(c(
          # 原文：# 「要注意」表：題庫內容要人看一下的地方，一列一個
          "# \u300c\u8981\u6ce8\u610f\u300d\u8868\uff1a\u984c\u5eab\u5167\u5bb9\u8981\u4eba\u770b\u4e00\u4e0b\u7684\u5730\u65b9\uff0c\u4e00\u5217\u4e00\u500b",
          paste0("audit <- kitItemBank::bank_audit(kitItemBank::bank_to_v02(kitItemBank::read_kit(), \"", d, "\"))"),
          "kitItemBank::bank_audit_summary(audit)",
          "",
          # 原文：# 給題庫維護者：在 kitItemBank 的原始碼資料夾執行。
          "# \u7d66\u984c\u5eab\u7dad\u8b77\u8005\uff1a\u5728 kitItemBank \u7684\u539f\u59cb\u78bc\u8cc7\u6599\u593e\u57f7\u884c\u3002",
          # 原文：# 讀這一頁下載的修改包，套到內建題庫（kit），檢查後寫回 inst/extdata/kit/。
          "# \u8b80\u9019\u4e00\u9801\u4e0b\u8f09\u7684\u4fee\u6539\u5305\uff0c\u5957\u5230\u5167\u5efa\u984c\u5eab\uff08kit\uff09\uff0c\u6aa2\u67e5\u5f8c\u5beb\u56de inst/extdata/kit/\u3002",
          paste0("p <- kitItemBank::read_patch_xlsx(\"", edit_download_filename(d), "\")"),
          "kit <- kitItemBank::apply_patch(kitItemBank::read_kit(\"inst/extdata/kit\"), p$patch)",
          "kitItemBank::check_kit(kit)   # 0 \u5217 = \u6c92\u554f\u984c",
          "kitItemBank::write_kit(kit, \"inst/extdata/kit\")",
          # 原文：# 寫完用 git diff 看改了什麼，再 commit、發一版。
          "# \u5beb\u5b8c\u7528 git diff \u770b\u6539\u4e86\u4ec0\u9ebc\uff0c\u518d commit\u3001\u767c\u4e00\u7248\u3002"
        ))
      },
      # 原文：這一段給題庫維護者用：把改好的題庫換進 app 的內建題庫。
      note = "\u9019\u4e00\u6bb5\u7d66\u984c\u5eab\u7dad\u8b77\u8005\u7528\uff1a\u628a\u6539\u597d\u7684\u984c\u5eab\u63db\u9032 app \u7684\u5167\u5efa\u984c\u5eab\u3002"
    )

    # 復原所有改動：先問一次
    reverting <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(n_changes(), if (n_changes() == 0L) reverting(FALSE))
    output$revert_ui <- shiny::renderUI({
      n <- n_changes()
      if (n == 0L) return(NULL)
      if (!reverting()) {
        return(shiny::div(
          class = "kit-buttons",
          # 原文：復原所有改動
          kitDesign::kit_button(ns("revert"), "\u5fa9\u539f\u6240\u6709\u6539\u52d5", "danger")
        ))
      }
      shiny::tagList(
        kitDesign::kit_callout(
          warn = TRUE,
          # 原文：復原所有改動：這個題庫的 
          paste0("\u5fa9\u539f\u6240\u6709\u6539\u52d5\uff1a\u9019\u500b\u984c\u5eab\u7684 ", kitDesign::kit_num(n),
                 # 原文： 處改動都會不見，回到「 | 」。
                 " \u8655\u6539\u52d5\u90fd\u6703\u4e0d\u898b\uff0c\u56de\u5230\u300c", store$source()$label, "\u300d\u3002")
        ),
        shiny::div(
          class = "kit-buttons",
          # 原文：復原，丟掉這些改動
          kitDesign::kit_button(ns("revert_ok"), "\u5fa9\u539f\uff0c\u4e1f\u6389\u9019\u4e9b\u6539\u52d5", "danger"),
          # 原文：不復原
          kitDesign::kit_button(ns("revert_cancel"), "\u4e0d\u5fa9\u539f")
        )
      )
    })
    shiny::observeEvent(input$revert, reverting(TRUE))
    shiny::observeEvent(input$revert_cancel, reverting(FALSE))
    shiny::observeEvent(input$revert_ok, {
      store$revert()
      reverting(FALSE)
      # 原文：已復原所有改動。
      kitDesign::kit_notify("\u5df2\u5fa9\u539f\u6240\u6709\u6539\u52d5\u3002")
    })

    invisible(NULL)
  })
}

# ---- 題目的構面／題目的波次（同一套）-------------------------------------------

edit_assign_ui <- function(id, table) {
  ns <- shiny::NS(id)
  noun <- edit_noun(table)
  shiny::fluidRow(
    shiny::column(
      4,
      shiny::selectizeInput(
        ns("column"), noun, choices = NULL,
        # 原文：搜尋 | …
        options = list(placeholder = paste0("\u641c\u5c0b", noun, "\u2026"))
      ),
      shiny::uiOutput(ns("delete_ui")),
      # 原文：新增
      shiny::textInput(ns("new_name"), paste0("\u65b0\u589e", noun),
                       # 原文：新 | 的名稱
                       placeholder = paste0("\u65b0", noun, "\u7684\u540d\u7a31")),
      # 原文：加入
      shiny::div(class = "kit-buttons", kitDesign::kit_button(ns("add"), "\u52a0\u5165"))
    ),
    shiny::column(
      8,
      shiny::uiOutput(ns("headline")),
      # 原文：關鍵字
      shiny::textInput(ns("keyword"), "\u95dc\u9375\u5b57",
                       # 原文：itemID、區塊或題目裡的字
                       placeholder = "itemID\u3001\u5340\u584a\u6216\u984c\u76ee\u88e1\u7684\u5b57"),
      # 原文：只看已勾的
      shiny::checkboxInput(ns("only"), "\u53ea\u770b\u5df2\u52fe\u7684", value = FALSE),
      shiny::div(
        class = "kit-buttons",
        # 原文：勾選目前篩出的全部
        kitDesign::kit_button(ns("check_all"), "\u52fe\u9078\u76ee\u524d\u7be9\u51fa\u7684\u5168\u90e8"),
        # 原文：取消目前篩出的全部
        kitDesign::kit_button(ns("uncheck_all"), "\u53d6\u6d88\u76ee\u524d\u7be9\u51fa\u7684\u5168\u90e8")
      ),
      shiny::tags$p(class = "kit-hint",
                    # 原文：點一列就勾選，再點一下取消；淡綠色的列是有這個 | 的題目。
                    paste0("\u9ede\u4e00\u5217\u5c31\u52fe\u9078\uff0c\u518d\u9ede\u4e00\u4e0b\u53d6\u6d88\uff1b\u6de1\u7da0\u8272\u7684\u5217\u662f\u6709\u9019\u500b", noun, "\u7684\u984c\u76ee\u3002")),
      DT::DTOutput(ns("tbl"))
    )
  )
}

edit_assign_server <- function(id, store, table) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    noun <- edit_noun(table)
    key_of <- function(df) paste(df$itemID, df$itemVer, sep = "\r")

    # 左邊：構面／波次清單（每個帶勾了幾題）
    choices <- shiny::reactive(edit_column_choices(store$bank(), table))
    want <- shiny::reactiveVal(NULL)  # 新增後、或別的分頁要選的欄
    shiny::observe({
      ch <- choices()
      w <- shiny::isolate(want())
      cur <- shiny::isolate(input$column)
      sel <- if (!is.null(w) && w %in% ch) w else if (length(cur) && cur %in% ch) cur else
        unname(ch[1L])
      shiny::updateSelectizeInput(session, "column", choices = ch, selected = sel)
    })
    column <- shiny::reactive({
      cl <- input$column
      shiny::req(cl, cl %in% get_matrix_cols(store$bank()[[table]]))
      cl
    })
    shiny::observeEvent(input$column, if (identical(want(), input$column)) want(NULL))

    # 右邊：題目表。表本身（itemID、itemVer、區塊、題目）只在欄、篩選或題目變了才重畫；
    # 勾了哪些用 DT 的選取表示，點一下就改 store，再把選取對回去。
    rows <- shiny::reactive({
      edit_item_rows(store$bank(), table, column(), input$keyword, input$only)
    })
    shown <- shiny::reactiveVal(NULL)  # 一樣的值不會讓表重畫
    shiny::observe({
      shown(rows()[, c("itemID", "itemVer", "block", "label")])
    })
    assigned_idx <- shiny::reactive({
      s <- shown()
      shiny::req(s)
      df <- store$bank()[[table]]
      v <- df[[column()]][match(key_of(s), key_of(df))]
      which(v == 1L)
    })

    output$headline <- shiny::renderUI({
      df <- store$bank()[[table]]
      cl <- column()
      s <- shown()
      filtered <- nzchar(trimws(input$keyword %||% "")) || isTRUE(input$only)
      shiny::tagList(
        # 原文：「 | 」的題目：已勾 
        shiny::h4(paste0("\u300c", col_display_labels(cl), "\u300d\u7684\u984c\u76ee\uff1a\u5df2\u52fe ",
                         kitDesign::kit_num(sum(df[[cl]] == 1L, na.rm = TRUE)), " / ",
                         # 原文： 題
                         kitDesign::kit_num(nrow(df)), " \u984c")),
        if (filtered && !is.null(s)) {
          shiny::tags$p(class = "kit-hint",
                        # 原文：目前篩出  |  題。
                        paste0("\u76ee\u524d\u7be9\u51fa ", kitDesign::kit_num(nrow(s)), " \u984c\u3002"))
        }
      )
    })

    output$tbl <- DT::renderDT({
      s <- shown()
      shiny::req(s)
      kitDesign::kit_dt(
        s,
        selection = list(mode = "multiple", target = "row",
                         selected = shiny::isolate(assigned_idx())),
        page_length = 20L, wrap_cols = "label"
      )
    })
    proxy <- DT::dataTableProxy("tbl")

    # store 變了（勾選全部、復原、換欄）→ 把表的選取對回去
    shiny::observe({
      idx <- assigned_idx()
      cur <- shiny::isolate(input$tbl_rows_selected) %||% integer(0)
      if (!setequal(idx, cur)) DT::selectRows(proxy, idx)
    })

    # 使用者點了一列 → 只差一列時才當作「點一下」；差很多表示表和 store 沒對上，對回去
    shiny::observeEvent(input$tbl_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
      s <- shown()
      shiny::req(s)
      sel <- input$tbl_rows_selected %||% integer(0)
      idx <- assigned_idx()
      on <- setdiff(sel, idx)
      off <- setdiff(idx, sel)
      if (length(on) + length(off) == 0L) return()
      if (length(on) + length(off) > 1L) {
        DT::selectRows(proxy, idx)
        return()
      }
      i <- c(on, off)
      if (i > nrow(s)) return()
      store$set_bank(set_assignment(store$bank(), table, column(),
                                    s[i, c("itemID", "itemVer")],
                                    if (length(on)) 1L else 0L))
    })

    set_shown <- function(value) {
      s <- shown()
      shiny::req(s, nrow(s) > 0L)
      store$set_bank(set_assignment(store$bank(), table, column(),
                                    s[, c("itemID", "itemVer")], value))
    }
    shiny::observeEvent(input$check_all, set_shown(1L))
    shiny::observeEvent(input$uncheck_all, set_shown(0L))

    # 新增
    shiny::observeEvent(input$add, {
      name <- trimws(input$new_name %||% "")
      res <- tryCatch(add_column(store$bank(), table, name), error = function(e) e)
      if (inherits(res, "error")) {
        # 原文：新增
        kitDesign::kit_notify(kitDesign::kit_error_status(res, paste0("\u65b0\u589e", noun)))
        return()
      }
      want(name)
      store$set_bank(res)
      shiny::updateTextInput(session, "new_name", value = "")
      # 原文：已新增 | 「 | 」：在右邊的表點題目來勾選。
      kitDesign::kit_notify(paste0("\u5df2\u65b0\u589e", noun, "\u300c", name, "\u300d\uff1a\u5728\u53f3\u908a\u7684\u8868\u9ede\u984c\u76ee\u4f86\u52fe\u9078\u3002"))
    })

    # 刪除：先說後果，再按一次才刪
    deleting <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$column, deleting(FALSE))
    output$delete_ui <- shiny::renderUI({
      cl <- column()
      if (!deleting()) {
        return(shiny::div(
          class = "kit-buttons",
          # 原文：刪除這個
          kitDesign::kit_button(ns("delete"), paste0("\u522a\u9664\u9019\u500b", noun), "danger")
        ))
      }
      n <- sum(store$bank()[[table]][[cl]] == 1L, na.rm = TRUE)
      shiny::tagList(
        kitDesign::kit_callout(
          warn = TRUE,
          # 原文：刪除「 | 」：
          paste0("\u522a\u9664\u300c", col_display_labels(cl), "\u300d\uff1a", kitDesign::kit_num(n),
                 # 原文： 題會失去這個 | 。
                 " \u984c\u6703\u5931\u53bb\u9019\u500b", noun, "\u3002",
                 # 原文：這個構面的名稱、別名、信度也會一起刪掉。
                 if (table == "itemDim") "\u9019\u500b\u69cb\u9762\u7684\u540d\u7a31\u3001\u5225\u540d\u3001\u4fe1\u5ea6\u4e5f\u6703\u4e00\u8d77\u522a\u6389\u3002"
                 # 原文：各構面在這個波次的信度也會一起刪掉。
                 else "\u5404\u69cb\u9762\u5728\u9019\u500b\u6ce2\u6b21\u7684\u4fe1\u5ea6\u4e5f\u6703\u4e00\u8d77\u522a\u6389\u3002")
        ),
        shiny::div(
          class = "kit-buttons",
          # 原文：刪除這個
          kitDesign::kit_button(ns("delete_ok"), paste0("\u522a\u9664\u9019\u500b", noun), "danger"),
          # 原文：不刪
          kitDesign::kit_button(ns("delete_cancel"), "\u4e0d\u522a")
        )
      )
    })
    shiny::observeEvent(input$delete, deleting(TRUE))
    shiny::observeEvent(input$delete_cancel, deleting(FALSE))
    shiny::observeEvent(input$delete_ok, {
      cl <- column()
      store$set_bank(remove_column(store$bank(), table, cl))
      deleting(FALSE)
      # 原文：已刪除 | 「 | 」。
      kitDesign::kit_notify(paste0("\u5df2\u522a\u9664", noun, "\u300c", col_display_labels(cl), "\u300d\u3002"))
    })

    list(select = function(col) {
      want(col)
      shiny::updateSelectizeInput(session, "column", selected = col)
    })
  })
}

# ---- 構面資訊 -------------------------------------------------------------------

edit_meta_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      4,
      # 原文：構面
      shiny::selectizeInput(ns("dim"), "\u69cb\u9762", choices = NULL,
                            # 原文：搜尋構面…
                            options = list(placeholder = "\u641c\u5c0b\u69cb\u9762\u2026")),
      # 原文：名稱
      shiny::textInput(ns("dimName"), "\u540d\u7a31"),
      # 原文：別名
      shiny::textInput(ns("alias"), "\u5225\u540d"),
      # 原文：備註
      shiny::textInput(ns("note"), "\u5099\u8a3b"),
      # 原文：信度（全部波次） | 例如 0.85；可以留空
      shiny::textInput(ns("reliability"), "\u4fe1\u5ea6\uff08\u5168\u90e8\u6ce2\u6b21\uff09", placeholder = "\u4f8b\u5982 0.85\uff1b\u53ef\u4ee5\u7559\u7a7a"),
      # 原文：套用
      shiny::div(class = "kit-buttons", kitDesign::kit_button(ns("apply"), "\u5957\u7528")),
      # 原文：改完按「套用」才會記下來。
      shiny::tags$p(class = "kit-hint", "\u6539\u5b8c\u6309\u300c\u5957\u7528\u300d\u624d\u6703\u8a18\u4e0b\u4f86\u3002")
    ),
    shiny::column(
      8,
      shiny::uiOutput(ns("wave_title")),
      shiny::uiOutput(ns("wave_table")),
      # 原文：波次
      shiny::selectInput(ns("wave"), "\u6ce2\u6b21", choices = NULL),
      # 原文：這個波次的信度
      shiny::textInput(ns("wave_value"), "\u9019\u500b\u6ce2\u6b21\u7684\u4fe1\u5ea6",
                       # 原文：例如 0.82；留空表示拿掉
                       placeholder = "\u4f8b\u5982 0.82\uff1b\u7559\u7a7a\u8868\u793a\u62ff\u6389"),
      shiny::div(class = "kit-buttons",
                 # 原文：設定這個波次的信度
                 kitDesign::kit_button(ns("set_wave"), "\u8a2d\u5b9a\u9019\u500b\u6ce2\u6b21\u7684\u4fe1\u5ea6"))
    )
  )
}

edit_meta_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    meta <- shiny::reactive(ensure_dim_meta_cols(store$bank()$dimMeta))

    dim_choices <- shiny::reactive({
      dm <- meta()
      ids <- union(dm$dimID, get_matrix_cols(store$bank()$itemDim))
      nm <- dm$dimName[match(ids, dm$dimID)]
      # 原文：（ | ）
      lab <- ifelse(!is.na(nm) & nzchar(nm) & nm != ids, paste0(nm, "\uff08", ids, "\uff09"), ids)
      stats::setNames(ids, col_display_labels(lab))
    })
    want <- shiny::reactiveVal(NULL)
    shiny::observe({
      ch <- dim_choices()
      cur <- shiny::isolate(input$dim)
      w <- shiny::isolate(want())
      sel <- if (!is.null(w) && w %in% ch) w else if (length(cur) && cur %in% ch) cur else
        unname(ch[1L])
      shiny::updateSelectizeInput(session, "dim", choices = ch, selected = sel)
    })
    shiny::observe({
      w <- get_wave_cols(store$bank()$itemWave)
      cur <- shiny::isolate(input$wave)
      shiny::updateSelectInput(session, "wave", choices = make_col_choices(w),
                               selected = if (length(cur) && cur %in% w) cur else w[1L])
    })
    dim_id <- shiny::reactive({
      d <- input$dim
      shiny::req(d, d %in% dim_choices())
      d
    })

    # 選到的構面的資訊；值一樣時不會再把輸入框蓋掉（打到一半不會被清掉）
    row <- shiny::reactiveVal(NULL)
    shiny::observe({
      dm <- meta()
      d <- dim_id()
      i <- match(d, dm$dimID)
      row(list(dimID = d,
               dimName = if (is.na(i)) d else dm$dimName[i],
               alias = if (is.na(i)) "" else dm$alias[i],
               note = if (is.na(i)) "" else dm$note[i],
               reliability = if (is.na(i)) "" else dm$reliability[i]))
    })
    shiny::observe({
      r <- row()
      shiny::req(r)
      shiny::updateTextInput(session, "dimName", value = r$dimName)
      shiny::updateTextInput(session, "alias", value = r$alias)
      shiny::updateTextInput(session, "note", value = r$note)
      shiny::updateTextInput(session, "reliability", value = r$reliability)
    })

    shiny::observeEvent(input$apply, {
      d <- dim_id()
      old <- store$bank()
      res <- tryCatch(
        set_dim_meta(old, d, dimName = input$dimName %||% "", alias = input$alias %||% "",
                     note = input$note %||% "", reliability = input$reliability %||% ""),
        error = function(e) e
      )
      if (inherits(res, "error")) {
        # 原文：套用構面資訊
        kitDesign::kit_notify(kitDesign::kit_error_status(res, "\u5957\u7528\u69cb\u9762\u8cc7\u8a0a"))
        return()
      }
      if (identical(ensure_dim_meta_cols(res$dimMeta), ensure_dim_meta_cols(old$dimMeta))) {
        # 原文：沒有改到東西：和原本的構面資訊一樣。
        kitDesign::kit_notify(kitDesign::kit_status("warning", "\u6c92\u6709\u6539\u5230\u6771\u897f\uff1a\u548c\u539f\u672c\u7684\u69cb\u9762\u8cc7\u8a0a\u4e00\u6a23\u3002"))
        return()
      }
      store$set_bank(res)
      # 原文：已套用「 | 」的構面資訊。
      kitDesign::kit_notify(paste0("\u5df2\u5957\u7528\u300c", d, "\u300d\u7684\u69cb\u9762\u8cc7\u8a0a\u3002"))
    })

    wave_rows <- shiny::reactive({
      dr <- store$bank()$dimReliability %||% empty_dim_reliability()
      dr <- dr[dr$dimID == dim_id(), , drop = FALSE]
      dr <- dr[order(match(dr$wave, get_wave_cols(store$bank()$itemWave))), , drop = FALSE]
      # 原文：波次
      data.frame("\u6ce2\u6b21" = col_display_labels(dr$wave),
                 # 原文：信度
                 "\u4fe1\u5ea6" = format(dr$reliability, trim = TRUE),
                 check.names = FALSE, stringsAsFactors = FALSE)
    })
    output$wave_title <- shiny::renderUI({
      # 原文：「 | 」各波次的信度
      shiny::h4(paste0("\u300c", dim_id(), "\u300d\u5404\u6ce2\u6b21\u7684\u4fe1\u5ea6"))
    })
    output$wave_table <- shiny::renderUI({
      w <- wave_rows()
      if (nrow(w) == 0L) {
        return(shiny::tags$p(class = "kit-hint",
                             # 原文：這個構面沒有各波次的信度：每個波次都用「信度（全部波次）」。
                             "\u9019\u500b\u69cb\u9762\u6c92\u6709\u5404\u6ce2\u6b21\u7684\u4fe1\u5ea6\uff1a\u6bcf\u500b\u6ce2\u6b21\u90fd\u7528\u300c\u4fe1\u5ea6\uff08\u5168\u90e8\u6ce2\u6b21\uff09\u300d\u3002"))
      }
      shiny::tagList(
        shiny::tags$p(class = "kit-hint",
                      # 原文：下面這些波次用自己的信度；其他波次用「信度（全部波次）」。
                      "\u4e0b\u9762\u9019\u4e9b\u6ce2\u6b21\u7528\u81ea\u5df1\u7684\u4fe1\u5ea6\uff1b\u5176\u4ed6\u6ce2\u6b21\u7528\u300c\u4fe1\u5ea6\uff08\u5168\u90e8\u6ce2\u6b21\uff09\u300d\u3002"),
        kitDesign::kit_static_table(w)
      )
    })

    shiny::observeEvent(input$set_wave, {
      d <- dim_id()
      w <- input$wave
      res <- tryCatch(set_wave_reliability(store$bank(), d, w, input$wave_value %||% ""),
                      error = function(e) e)
      if (inherits(res, "error")) {
        # 原文：設定波次的信度
        kitDesign::kit_notify(kitDesign::kit_error_status(res, "\u8a2d\u5b9a\u6ce2\u6b21\u7684\u4fe1\u5ea6"))
        return()
      }
      store$set_bank(res)
      shiny::updateTextInput(session, "wave_value", value = "")
      v <- trimws(input$wave_value %||% "")
      kitDesign::kit_notify(if (nzchar(v)) {
        # 原文：已設定「 | 」在  |  的信度： | 。
        paste0("\u5df2\u8a2d\u5b9a\u300c", d, "\u300d\u5728 ", col_display_labels(w), " \u7684\u4fe1\u5ea6\uff1a", v, "\u3002")
      } else {
        # 原文：已拿掉「 | 」在  |  的信度。
        paste0("\u5df2\u62ff\u6389\u300c", d, "\u300d\u5728 ", col_display_labels(w), " \u7684\u4fe1\u5ea6\u3002")
      })
    })

    list(select = function(d) {
      want(d)
      shiny::updateSelectizeInput(session, "dim", selected = d)
    })
  })
}

# ---- 題目內容 -------------------------------------------------------------------
# 上面「要注意」表（bank_audit() 的結果），點一列就把那一題帶到下面；
# 也可以直接選一題一版。下面的表單改好按「套用這一題的改動」才記下來。

edit_item_ui <- function(id) {
  ns <- shiny::NS(id)
  # 原文：全部
  check_choices <- c("\u5168\u90e8" = "all",
                     stats::setNames(AUDIT_CHECKS$check, AUDIT_CHECKS$label))
  shiny::tagList(
    shiny::uiOutput(ns("audit_headline")),
    # 原文：看哪一種
    shiny::selectInput(ns("check"), "\u770b\u54ea\u4e00\u7a2e", choices = check_choices, selected = "all"),
    # 原文：點一列就把那一題帶到下面的表單。
    shiny::tags$p(class = "kit-hint", "\u9ede\u4e00\u5217\u5c31\u628a\u90a3\u4e00\u984c\u5e36\u5230\u4e0b\u9762\u7684\u8868\u55ae\u3002"),
    DT::DTOutput(ns("audit_tbl")),
    shiny::fluidRow(
      shiny::column(
        4,
        # 原文：題目（itemID）
        shiny::selectizeInput(ns("item"), "\u984c\u76ee\uff08itemID\uff09", choices = NULL,
                              # 原文：搜尋 itemID…
                              options = list(placeholder = "\u641c\u5c0b itemID\u2026")),
        # 原文：版本
        shiny::radioButtons(ns("ver"), "\u7248\u672c", choices = character(0), selected = character(0),
                            inline = TRUE)
      ),
      shiny::column(
        8,
        shiny::uiOutput(ns("item_head")),
        # 原文：題目文字（label）
        shiny::textAreaInput(ns("label"), "\u984c\u76ee\u6587\u5b57\uff08label\uff09", rows = 3, width = "100%"),
        # 原文：選項（value）
        shiny::textAreaInput(ns("value"), "\u9078\u9805\uff08value\uff09", rows = 5, width = "100%"),
        # 原文：遺漏值（noResponse）
        shiny::textAreaInput(ns("noResponse"), "\u907a\u6f0f\u503c\uff08noResponse\uff09", rows = 2, width = "100%"),
        # 原文：反向計分（reverseScored）
        shiny::radioButtons(ns("reverseScored"), "\u53cd\u5411\u8a08\u5206\uff08reverseScored\uff09",
                            # 原文：0 不是 | 1 是
                            choices = c("0 \u4e0d\u662f" = "0", "1 \u662f" = "1"), inline = TRUE),
        # 原文：題型（itemType）
        shiny::textInput(ns("itemType"), "\u984c\u578b\uff08itemType\uff09"),
        # 原文：填答者（respondent）
        shiny::textInput(ns("respondent"), "\u586b\u7b54\u8005\uff08respondent\uff09"),
        # 原文：停用（deprecated）
        shiny::radioButtons(ns("deprecated"), "\u505c\u7528\uff08deprecated\uff09",
                            # 原文：0 使用中 | 1 停用
                            choices = c("0 \u4f7f\u7528\u4e2d" = "0", "1 \u505c\u7528" = "1"), inline = TRUE),
        # 原文：備註（note）
        shiny::textAreaInput(ns("note"), "\u5099\u8a3b\uff08note\uff09", rows = 2, width = "100%"),
        kitDesign::kit_advanced(
          # 原文：區塊、題幹
          title = "\u5340\u584a\u3001\u984c\u5e79",
          # 原文：區塊（block）
          shiny::textInput(ns("block"), "\u5340\u584a\uff08block\uff09", width = "100%"),
          # 原文：題幹（stemID）
          shiny::textInput(ns("stemID"), "\u984c\u5e79\uff08stemID\uff09", width = "100%")
        ),
        shiny::div(class = "kit-buttons",
                   # 原文：套用這一題的改動
                   kitDesign::kit_button(ns("apply"), "\u5957\u7528\u9019\u4e00\u984c\u7684\u6539\u52d5")),
        # 原文：改完按「套用這一題的改動」才會記下來。
        shiny::tags$p(class = "kit-hint", "\u6539\u5b8c\u6309\u300c\u5957\u7528\u9019\u4e00\u984c\u7684\u6539\u52d5\u300d\u624d\u6703\u8a18\u4e0b\u4f86\u3002")
      )
    )
  )
}

#' @param on_dim 「要注意」表點到構面的問題時呼叫：function(dimID)
#' @return list(load = function(item_id, ver))
#' @noRd
edit_item_server <- function(id, store, on_dim = function(d) NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # 要注意：只有表在畫面上時才算（換到別的面板不算）
    audit <- shiny::reactive(bank_audit(store$bank()))
    audit_view <- shiny::reactive({
      a <- audit()
      ck <- input$check %||% "all"
      if (!identical(ck, "all")) a <- a[a$check == ck, , drop = FALSE]
      rownames(a) <- NULL
      a
    })

    output$audit_headline <- shiny::renderUI({
      a <- audit()
      if (nrow(a) == 0L) {
        # 原文：要注意：沒有找到要注意的地方。
        return(kitDesign::kit_status("success", "\u8981\u6ce8\u610f\uff1a\u6c92\u6709\u627e\u5230\u8981\u6ce8\u610f\u7684\u5730\u65b9\u3002"))
      }
      n_todo <- sum(a$level == "todo")
      # 原文：要注意： |  項
      shiny::h4(paste0("\u8981\u6ce8\u610f\uff1a", kitDesign::kit_num(nrow(a)), " \u9805",
                       # 原文：（其中  |  項建議修改）
                       if (n_todo > 0L) paste0("\uff08\u5176\u4e2d ", kitDesign::kit_num(n_todo), " \u9805\u5efa\u8b70\u4fee\u6539\uff09")))
    })

    output$audit_tbl <- DT::renderDT({
      a <- audit_view()
      df <- data.frame(
        # 原文：等級 | 建議修改 | 看一下
        "\u7b49\u7d1a" = ifelse(a$level == "todo", "\u5efa\u8b70\u4fee\u6539", "\u770b\u4e00\u4e0b"),
        itemID = a$itemID, itemVer = a$itemVer, dimID = a$dimID,
        # 原文：說明
        "\u8aaa\u660e" = a$message,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      if (all(is.na(df$dimID))) df$dimID <- NULL
      # 原文：說明
      kitDesign::kit_dt(df, selection = "single", page_length = 10L, wrap_cols = "\u8aaa\u660e")
    })

    # 目前的題目：list(id, ver)；輸入框和別的分頁都改這裡
    current <- shiny::reactiveVal(NULL)
    default_ver <- function(b, it) {
      ib <- b$itemBank
      s <- ib[ib$itemID == it, , drop = FALSE]
      if (nrow(s) == 0L) return(NA_integer_)
      act <- s$itemVer[s$deprecated == 0L]
      if (length(act)) max(act) else max(s$itemVer)
    }
    load <- function(it, ver = NULL) {
      b <- shiny::isolate(store$bank())
      vs <- edit_item_versions(b, it)
      if (length(vs) == 0L) {
        # 原文：題庫裡沒有  |  這一題。
        kitDesign::kit_notify(kitDesign::kit_status("warning", paste0("\u984c\u5eab\u88e1\u6c92\u6709 ", it, " \u9019\u4e00\u984c\u3002")))
        return(invisible(FALSE))
      }
      v <- suppressWarnings(as.integer(ver %||% NA))
      if (length(v) != 1L || is.na(v) || !v %in% vs) v <- default_ver(b, it)
      current(list(id = it, ver = v))
      invisible(TRUE)
    }

    # 題目選單（server 端；題目清單沒變就不重送）
    ids <- shiny::reactive(sort(unique(store$bank()$itemBank$itemID)))
    ids_sent <- shiny::reactiveVal(NULL)
    shiny::observe(ids_sent(ids()))
    shiny::observe({
      ch <- ids_sent()
      cur <- shiny::isolate(current())
      shiny::updateSelectizeInput(session, "item", choices = ch, server = TRUE,
                                  selected = if (!is.null(cur)) cur$id else character(0))
    })
    shiny::observeEvent(current(), {
      cur <- current()
      if (!identical(input$item, cur$id)) {
        shiny::updateSelectizeInput(session, "item", choices = shiny::isolate(ids_sent()),
                                    server = TRUE, selected = cur$id)
      }
    })
    shiny::observeEvent(input$item, {
      it <- input$item
      cur <- current()
      if (!nzchar(it %||% "") || (!is.null(cur) && identical(cur$id, it))) return()
      load(it)
    })

    # 版本
    shiny::observe({
      cur <- current()
      shiny::req(cur)
      ib <- store$bank()$itemBank
      s <- ib[ib$itemID == cur$id, , drop = FALSE]
      s <- s[order(s$itemVer), , drop = FALSE]
      # 原文：第  |  版 | （停用）
      lab <- paste0("\u7b2c ", s$itemVer, " \u7248", ifelse(s$deprecated == 1L, "\uff08\u505c\u7528\uff09", ""))
      shiny::updateRadioButtons(session, "ver", choices = stats::setNames(as.character(s$itemVer), lab),
                                selected = as.character(cur$ver), inline = TRUE)
    })
    shiny::observeEvent(input$ver, {
      cur <- current()
      v <- suppressWarnings(as.integer(input$ver))
      if (is.null(cur) || is.na(v) || identical(v, cur$ver)) return()
      if (v %in% edit_item_versions(store$bank(), cur$id)) current(list(id = cur$id, ver = v))
    })

    # 「要注意」表點一列
    shiny::observeEvent(input$audit_tbl_rows_selected, {
      a <- audit_view()
      i <- input$audit_tbl_rows_selected
      if (length(i) != 1L || i > nrow(a)) return()
      r <- a[i, ]
      if (is.na(r$itemID)) {
        if (!is.na(r$dimID)) on_dim(r$dimID)
        return()
      }
      load(r$itemID, r$itemVer)
    })

    # 目前題目的內容；值一樣時不會再把輸入框蓋掉（打到一半不會被清掉）
    loaded <- shiny::reactiveVal(NULL)
    shiny::observe({
      cur <- current()
      shiny::req(cur)
      vals <- edit_item_values(store$bank(), cur$id, cur$ver)
      shiny::req(vals)
      loaded(c(cur, list(vals = vals)))
    })
    shiny::observe({
      l <- loaded()
      shiny::req(l)
      v <- l$vals
      for (f in c("label", "value", "noResponse", "note")) {
        shiny::updateTextAreaInput(session, f, value = v[[f]])
      }
      for (f in c("itemType", "respondent", "block", "stemID")) {
        shiny::updateTextInput(session, f, value = v[[f]])
      }
      for (f in ITEM_FIELDS_01) {
        shiny::updateRadioButtons(session, f, selected = as.character(v[[f]]))
      }
    })

    output$item_head <- shiny::renderUI({
      l <- loaded()
      if (is.null(l)) {
        # 原文：在「要注意」表點一列，或在左邊選一題。
        return(shiny::tags$p(class = "kit-hint", "\u5728\u300c\u8981\u6ce8\u610f\u300d\u8868\u9ede\u4e00\u5217\uff0c\u6216\u5728\u5de6\u908a\u9078\u4e00\u984c\u3002"))
      }
      a <- audit()
      mine <- a[!is.na(a$itemID) & a$itemID == l$id & (is.na(a$itemVer) | a$itemVer == l$ver), ,
                drop = FALSE]
      shiny::tagList(
        # 原文： 第  |  版
        shiny::h4(paste0(l$id, " \u7b2c ", l$ver, " \u7248")),
        if (nrow(mine) > 0L) {
          kitDesign::kit_callout(
            warn = any(mine$level == "todo"),
            # 原文：要注意：
            shiny::tags$strong("\u8981\u6ce8\u610f\uff1a"),
            shiny::tags$ul(lapply(mine$message, shiny::tags$li))
          )
        } else {
          # 原文：這一版沒有要注意的地方。
          shiny::tags$p(class = "kit-hint", "\u9019\u4e00\u7248\u6c92\u6709\u8981\u6ce8\u610f\u7684\u5730\u65b9\u3002")
        }
      )
    })

    # 套用：只送有改的欄位
    shiny::observeEvent(input$apply, {
      l <- loaded()
      if (is.null(l)) {
        # 原文：先選一題。
        kitDesign::kit_notify(kitDesign::kit_status("warning", "\u5148\u9078\u4e00\u984c\u3002"))
        return()
      }
      eol <- function(x) gsub("\r\n?", "\n", x %||% "")
      fields <- list()
      for (f in setdiff(ITEM_FIELDS, ITEM_FIELDS_01)) {
        new <- input[[f]]
        if (is.null(new)) next
        if (eol(new) != eol(l$vals[[f]])) fields[[f]] <- new
      }
      for (f in ITEM_FIELDS_01) {
        new <- suppressWarnings(as.integer(input[[f]] %||% NA))
        if (!is.na(new) && new != l$vals[[f]]) fields[[f]] <- new
      }
      if (length(fields) == 0L) {
        # 原文：沒有改到東西：和原本的內容一樣。
        kitDesign::kit_notify(kitDesign::kit_status("warning", "\u6c92\u6709\u6539\u5230\u6771\u897f\uff1a\u548c\u539f\u672c\u7684\u5167\u5bb9\u4e00\u6a23\u3002"))
        return()
      }
      res <- tryCatch(set_item_fields(store$bank(), l$id, l$ver, fields = fields),
                      error = function(e) e)
      if (inherits(res, "error")) {
        # 原文：套用這一題的改動
        kitDesign::kit_notify(kitDesign::kit_error_status(res, "\u5957\u7528\u9019\u4e00\u984c\u7684\u6539\u52d5"))
        return()
      }
      if (identical(res$itemBank, store$bank()$itemBank)) {
        # 原文：沒有改到東西：去掉前後空白後和原本的內容一樣。
        kitDesign::kit_notify(kitDesign::kit_status("warning", "\u6c92\u6709\u6539\u5230\u6771\u897f\uff1a\u53bb\u6389\u524d\u5f8c\u7a7a\u767d\u5f8c\u548c\u539f\u672c\u7684\u5167\u5bb9\u4e00\u6a23\u3002"))
        return()
      }
      store$set_bank(res)
      # 原文：已套用  |  第  |  版的改動：
      kitDesign::kit_notify(paste0("\u5df2\u5957\u7528 ", l$id, " \u7b2c ", l$ver, " \u7248\u7684\u6539\u52d5\uff1a",
                                   # 原文：、 | 。
                                   paste(ITEM_FIELD_LABELS[names(fields)], collapse = "\u3001"), "\u3002"))
    })

    list(load = function(item_id, ver = NULL) load(item_id, ver))
  })
}
