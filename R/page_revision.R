# R/page_revision.R — 改版：給老師的改版底稿 → 交回來的底稿做健康檢查 → 下載修改包
#
# 由上往下：這一頁做什麼 → 左 1/3：① 選題本、下載改版底稿；② 上傳填好的底稿、
# 寫新施測的代號｜右 2/3：健康檢查（擋下來／要注意）→ 改了什麼 → [下載修改包]
# → ▸ 用 R 怎麼做。app 不寫題庫；修改包交給管理員用 apply_patch() 套用。
# 計算都在 R/revision.R。

#' 有題目的題本：選單用（「家長 國小 1–3 年級（P-G1-G3）」= formID）
#' @noRd
rv_form_choices <- function(kit) {
  dv <- kit$dataset_vars
  ds_item <- unique(dv$datasetID[dv$kind == "item"])
  used <- unique(kit$datasets$formID[kit$datasets$datasetID %in% ds_item])
  fm <- kit$forms[kit$forms$formID %in% used, , drop = FALSE]
  # 原文：（ | ）
  stats::setNames(fm$formID, paste0(fm$label, "\uff08", fm$formID, "\uff09"))
}

#' Revision page: UI
#'
#' Export a revision template (改版底稿) for a form, upload a filled one, see its health
#' check and download the change package.
#'
#' @param id Module id.
#' @return A Shiny tag list.
#' @export
page_revision_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kitDesign::kit_tab_guide(
      # 原文：題本要改版時：給出題老師一份改版底稿，交回來後先做健康檢查，再變成修改包。
      goal = "\u984c\u672c\u8981\u6539\u7248\u6642\uff1a\u7d66\u51fa\u984c\u8001\u5e2b\u4e00\u4efd\u6539\u7248\u5e95\u7a3f\uff0c\u4ea4\u56de\u4f86\u5f8c\u5148\u505a\u5065\u5eb7\u6aa2\u67e5\uff0c\u518d\u8b8a\u6210\u4fee\u6539\u5305\u3002",
      steps = c(
        # 原文：選一個題本，按「下載改版底稿」交給老師。底稿是這個題本最近一次施測的題目，照問卷順序。
        "\u9078\u4e00\u500b\u984c\u672c\uff0c\u6309\u300c\u4e0b\u8f09\u6539\u7248\u5e95\u7a3f\u300d\u4ea4\u7d66\u8001\u5e2b\u3002\u5e95\u7a3f\u662f\u9019\u500b\u984c\u672c\u6700\u8fd1\u4e00\u6b21\u65bd\u6e2c\u7684\u984c\u76ee\uff0c\u7167\u554f\u5377\u9806\u5e8f\u3002",
        # 原文：老師在黃色欄選動作（不變、改文字、改選項、刪除）、寫新文字或新選項；新題目寫在「新增題目」表。
        "\u8001\u5e2b\u5728\u9ec3\u8272\u6b04\u9078\u52d5\u4f5c\uff08\u4e0d\u8b8a\u3001\u6539\u6587\u5b57\u3001\u6539\u9078\u9805\u3001\u522a\u9664\uff09\u3001\u5beb\u65b0\u6587\u5b57\u6216\u65b0\u9078\u9805\uff1b\u65b0\u984c\u76ee\u5beb\u5728\u300c\u65b0\u589e\u984c\u76ee\u300d\u8868\u3002",
        # 原文：把填好的底稿傳上來，右邊會列出要先改好的（擋下來）和要看一下的（要注意）。
        "\u628a\u586b\u597d\u7684\u5e95\u7a3f\u50b3\u4e0a\u4f86\uff0c\u53f3\u908a\u6703\u5217\u51fa\u8981\u5148\u6539\u597d\u7684\uff08\u64cb\u4e0b\u4f86\uff09\u548c\u8981\u770b\u4e00\u4e0b\u7684\uff08\u8981\u6ce8\u610f\uff09\u3002",
        # 原文：寫新施測的代號（例如 M03M132G05），沒有擋下來的問題就能下載修改包。
        "\u5beb\u65b0\u65bd\u6e2c\u7684\u4ee3\u865f\uff08\u4f8b\u5982 M03M132G05\uff09\uff0c\u6c92\u6709\u64cb\u4e0b\u4f86\u7684\u554f\u984c\u5c31\u80fd\u4e0b\u8f09\u4fee\u6539\u5305\u3002"
      ),
      # 原文：改版底稿（Excel）；健康檢查；修改包（Excel，管理員用 apply_patch() 套用）。
      output = "\u6539\u7248\u5e95\u7a3f\uff08Excel\uff09\uff1b\u5065\u5eb7\u6aa2\u67e5\uff1b\u4fee\u6539\u5305\uff08Excel\uff0c\u7ba1\u7406\u54e1\u7528 apply_patch() \u5957\u7528\uff09\u3002",
      # 原文：改了文字或選項會變成新的題目版本，舊版留著；刪除只是新的問卷不放，題號不會刪。
      note = "\u6539\u4e86\u6587\u5b57\u6216\u9078\u9805\u6703\u8b8a\u6210\u65b0\u7684\u984c\u76ee\u7248\u672c\uff0c\u820a\u7248\u7559\u8457\uff1b\u522a\u9664\u53ea\u662f\u65b0\u7684\u554f\u5377\u4e0d\u653e\uff0c\u984c\u865f\u4e0d\u6703\u522a\u3002"
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        # 原文：① 下載改版底稿
        shiny::h4("\u2460 \u4e0b\u8f09\u6539\u7248\u5e95\u7a3f"),
        # 原文：題本
        shiny::selectInput(ns("form"), "\u984c\u672c", choices = NULL, width = "100%"),
        shiny::uiOutput(ns("base_ui")),
        # 原文：② 上傳填好的底稿
        shiny::h4("\u2461 \u4e0a\u50b3\u586b\u597d\u7684\u5e95\u7a3f"),
        shiny::fileInput(ns("upload"), NULL, accept = ".xlsx", width = "100%",
                         # 原文：選檔案… | 還沒有選檔案
                         buttonLabel = "\u9078\u6a94\u6848\u2026", placeholder = "\u9084\u6c92\u6709\u9078\u6a94\u6848"),
        # 原文：新施測的代號
        shiny::textInput(ns("admin_id"), "\u65b0\u65bd\u6e2c\u7684\u4ee3\u865f", width = "100%",
                         # 原文：例如 M03M132G05
                         placeholder = "\u4f8b\u5982 M03M132G05"),
        shiny::p(class = "kit-hint",
                 # 原文：世代代號 + M + 三位數月齡；84 月齡以後再加 G 和兩位數年級。要用還沒有的代號。
                 "\u4e16\u4ee3\u4ee3\u865f + M + \u4e09\u4f4d\u6578\u6708\u9f61\uff1b84 \u6708\u9f61\u4ee5\u5f8c\u518d\u52a0 G \u548c\u5169\u4f4d\u6578\u5e74\u7d1a\u3002\u8981\u7528\u9084\u6c92\u6709\u7684\u4ee3\u865f\u3002")
      ),
      shiny::column(8, shiny::uiOutput(ns("result")))
    ),
    kitDesign::kit_details_row(kitDesign::kit_code_panel_ui(ns("code")))
  )
}

#' Revision page: server
#'
#' @param id Module id.
#' @param kit The kit (17 tables): a reactive or a plain list.
#' @return Invisibly, a list of reactives: `base` (see [revision_base()]), `check` (the
#'   health check) and `patch` (the change package, or an error message).
#' @export
page_revision_server <- function(id, kit) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    kit_now <- if (shiny::is.reactive(kit)) kit else function() kit
    version <- codebook_bank_version()

    shiny::observe({
      ch <- rv_form_choices(kit_now())
      sel <- shiny::isolate(input$form)
      if (is.null(sel) || !sel %in% ch) sel <- ch[1]
      shiny::updateSelectInput(session, "form", choices = ch, selected = sel)
    })

    base <- shiny::reactive({
      f <- input$form
      shiny::req(f, nzchar(f))
      tryCatch(revision_base(kit_now(), f), error = function(e) NULL)
    })

    output$base_ui <- shiny::renderUI({
      b <- base()
      # 原文：這個題本沒有找到有題目的資料檔。
      if (is.null(b)) return(shiny::p(class = "kit-hint", "\u9019\u500b\u984c\u672c\u6c92\u6709\u627e\u5230\u6709\u984c\u76ee\u7684\u8cc7\u6599\u6a94\u3002"))
      k <- kit_now()
      f <- b$form$formID
      shiny::tagList(
        shiny::p(class = "kit-hint", paste0(
          # 原文：根據  |  的 | 問卷，
          "\u6839\u64da ", admin_label(k, b$admin$adminID), " \u7684", cb_respondent(b$dataset), "\u554f\u5377\uff0c",
          # 原文： 題。
          kitDesign::kit_num(nrow(b$questions)), " \u984c\u3002")),
        shiny::div(
          class = "kit-buttons",
          kitDesign::kit_download_button(
            # 原文：下載改版底稿（Excel）
            session, "dl_template", "\u4e0b\u8f09\u6539\u7248\u5e95\u7a3f\uff08Excel\uff09",
            # 原文：改版底稿_
            paste0("\u6539\u7248\u5e95\u7a3f_", f, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
            function(file) revision_template(k, f, file, datasetID = b$dataset$datasetID,
                                             version = version),
            class = kitDesign::kit_btn_class("secondary")
          )
        )
      )
    })

    upload <- shiny::reactive({
      u <- input$upload
      shiny::req(u, nrow(u) >= 1L)
      tryCatch(read_revision_xlsx(u$datapath[1]), error = function(e) conditionMessage(e))
    })
    check <- shiny::reactive({
      x <- upload()
      if (is.character(x)) return(NULL)
      rv_check(x, kit_now(), version)
    })
    patch <- shiny::reactive({
      chk <- check()
      if (is.null(chk) || any(chk$problems$severity == RV_BLOCK)) return(NULL)
      a <- trimws(input$admin_id %||% "")
      if (!nzchar(a)) return(NULL)
      tryCatch(rv_patch_from_check(kit_now(), chk, a),
               error = function(e) conditionMessage(e))
    })

    output$result <- shiny::renderUI({
      if (is.null(input$upload)) {
        return(shiny::tagList(
          # 原文：健康檢查
          shiny::h4("\u5065\u5eb7\u6aa2\u67e5"),
          # 原文：左邊傳上填好的改版底稿，這裡會列出檢查的結果。
          shiny::p(class = "kit-hint", "\u5de6\u908a\u50b3\u4e0a\u586b\u597d\u7684\u6539\u7248\u5e95\u7a3f\uff0c\u9019\u88e1\u6703\u5217\u51fa\u6aa2\u67e5\u7684\u7d50\u679c\u3002")
        ))
      }
      x <- upload()
      if (is.character(x)) {
        # 原文：讀不了這個檔案：
        return(kitDesign::kit_callout(warn = TRUE, paste0("\u8b80\u4e0d\u4e86\u9019\u500b\u6a94\u6848\uff1a", x)))
      }
      chk <- check()
      p <- chk$problems
      n_block <- sum(p$severity == RV_BLOCK)
      n_note <- sum(p$severity == RV_NOTE)
      res <- patch()
      shiny::tagList(
        # 原文：健康檢查
        shiny::h4("\u5065\u5eb7\u6aa2\u67e5"),
        if (n_block) {
          kitDesign::kit_callout(warn = TRUE, paste0(
            # 原文：有  |  個問題要先改好（擋下來），才能變成修改包。
            "\u6709 ", kitDesign::kit_num(n_block), " \u500b\u554f\u984c\u8981\u5148\u6539\u597d\uff08\u64cb\u4e0b\u4f86\uff09\uff0c\u624d\u80fd\u8b8a\u6210\u4fee\u6539\u5305\u3002",
            # 原文：請老師改好底稿再傳一次。
            "\u8acb\u8001\u5e2b\u6539\u597d\u5e95\u7a3f\u518d\u50b3\u4e00\u6b21\u3002"))
        } else {
          kitDesign::kit_callout(paste0(
            # 原文：沒有擋下來的問題。 | 有 
            "\u6c92\u6709\u64cb\u4e0b\u4f86\u7684\u554f\u984c\u3002", if (n_note) paste0("\u6709 ", kitDesign::kit_num(n_note),
                                                      # 原文： 個地方要注意，請看一下。
                                                      " \u500b\u5730\u65b9\u8981\u6ce8\u610f\uff0c\u8acb\u770b\u4e00\u4e0b\u3002")))
        },
        if (nrow(p)) kitDesign::kit_table_scroll(DT::DTOutput(ns("problems_dt"))),
        if (!n_block) rv_patch_ui(session, res, input$admin_id, version)
      )
    })

    output$problems_dt <- DT::renderDT({
      chk <- check()
      shiny::req(chk)
      p <- chk$problems
      res <- patch()
      if (is.list(res)) p <- res$problems
      # 原文： 第  |  列
      where <- ifelse(is.na(p$row), p$sheet, paste0(p$sheet, " \u7b2c ", p$row, " \u5217"))
      # 原文：嚴重度 | 位置 | 說明
      df <- data.frame("\u56b4\u91cd\u5ea6" = p$severity, "\u4f4d\u7f6e" = where, "\u8aaa\u660e" = p$message,
                       check.names = FALSE, stringsAsFactors = FALSE)
      # 原文：說明
      kitDesign::kit_dt(df, page_length = 25L, wrap_cols = "\u8aaa\u660e",
                        # 原文：說明
                        min_widths = c("\u8aaa\u660e" = "12rem"))
    })

    kitDesign::kit_code_panel_server("code", shiny::reactive({
      revision_code(input$form %||% "P-G1-G3", trimws(input$admin_id %||% ""))
    }))

    invisible(list(base = base, check = check, patch = patch))
  })
}

# 修改包：改了什麼 + 下載按鈕；還沒寫代號或代號不對時說明
rv_patch_ui <- function(session, res, admin_id, version) {
  # 原文：修改包
  head <- shiny::h4("\u4fee\u6539\u5305")
  if (is.null(res)) {
    return(shiny::tagList(head, shiny::p(class = "kit-hint",
                                         # 原文：在左邊寫新施測的代號，就能下載修改包。
                                         "\u5728\u5de6\u908a\u5beb\u65b0\u65bd\u6e2c\u7684\u4ee3\u865f\uff0c\u5c31\u80fd\u4e0b\u8f09\u4fee\u6539\u5305\u3002")))
  }
  if (is.character(res)) {
    return(shiny::tagList(head, kitDesign::kit_callout(warn = TRUE, res)))
  }
  s <- res$summary
  shiny::tagList(
    head,
    # 原文：規劃中的施測  | ，資料檔 
    shiny::p(paste0("\u898f\u5283\u4e2d\u7684\u65bd\u6e2c ", res$adminID, "\uff0c\u8cc7\u6599\u6a94 ", res$datasetID,
                    # 原文：（根據  | ）：
                    "\uff08\u6839\u64da ", res$base, "\uff09\uff1a")),
    shiny::tags$ul(
      # 原文：不變  |  題
      shiny::tags$li(paste0("\u4e0d\u8b8a ", kitDesign::kit_num(s[["kept"]]), " \u984c")),
      # 原文：改文字  |  題、改選項 
      shiny::tags$li(paste0("\u6539\u6587\u5b57 ", s[["text"]], " \u984c\u3001\u6539\u9078\u9805 ", s[["options"]],
                            # 原文： 題（各成為新的題目版本）
                            " \u984c\uff08\u5404\u6210\u70ba\u65b0\u7684\u984c\u76ee\u7248\u672c\uff09")),
      # 原文：刪除  |  題
      shiny::tags$li(paste0("\u522a\u9664 ", s[["deleted"]], " \u984c")),
      # 原文：新增  |  題
      shiny::tags$li(paste0("\u65b0\u589e ", s[["new"]], " \u984c"))
    ),
    shiny::div(
      class = "kit-buttons",
      kitDesign::kit_download_button(
        # 原文：下載修改包（Excel）
        session, "dl_patch", "\u4e0b\u8f09\u4fee\u6539\u5305\uff08Excel\uff09",
        # 原文：修改包_改版_
        paste0("\u4fee\u6539\u5305_\u6539\u7248_", res$adminID, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
        function(file) writexl::write_xlsx(revision_patch_workbook(res, version), file),
        class = kitDesign::kit_btn_class("primary")
      )
    ),
    shiny::p(class = "kit-hint", paste0(
      # 原文：共  |  列改動。交給題庫管理員：
      "\u5171 ", kitDesign::kit_num(nrow(res$patch)), " \u5217\u6539\u52d5\u3002\u4ea4\u7d66\u984c\u5eab\u7ba1\u7406\u54e1\uff1a",
      # 原文：read_patch_xlsx() 讀進來、apply_patch() 套用，再跑 check_kit()。
      "read_patch_xlsx() \u8b80\u9032\u4f86\u3001apply_patch() \u5957\u7528\uff0c\u518d\u8dd1 check_kit()\u3002"))
  )
}

# 用 R 怎麼做
revision_code <- function(form_id, admin_id) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", as.character(x), fixed = TRUE), "\"")
  if (!nzchar(admin_id)) admin_id <- "M03M132G05"
  paste(c(
    "kit <- kitItemBank::read_kit()",
    "",
    # 原文：# 改版底稿：這個題本最近一次施測的題目，照問卷順序
    "# \u6539\u7248\u5e95\u7a3f\uff1a\u9019\u500b\u984c\u672c\u6700\u8fd1\u4e00\u6b21\u65bd\u6e2c\u7684\u984c\u76ee\uff0c\u7167\u554f\u5377\u9806\u5e8f",
    # 原文：, \"改版底稿.xlsx\")
    paste0("kitItemBank::revision_template(kit, ", q(form_id), ", \"\u6539\u7248\u5e95\u7a3f.xlsx\")"),
    "",
    # 原文：# 老師填好交回：健康檢查（擋下來／要注意）
    "# \u8001\u5e2b\u586b\u597d\u4ea4\u56de\uff1a\u5065\u5eb7\u6aa2\u67e5\uff08\u64cb\u4e0b\u4f86\uff0f\u8981\u6ce8\u610f\uff09",
    # 原文：kitItemBank::check_revision_xlsx(\"改版底稿_填好.xlsx\", kit)
    "kitItemBank::check_revision_xlsx(\"\u6539\u7248\u5e95\u7a3f_\u586b\u597d.xlsx\", kit)",
    "",
    # 原文：# 變成修改包：規劃中的施測、資料檔、新版題目、新題目
    "# \u8b8a\u6210\u4fee\u6539\u5305\uff1a\u898f\u5283\u4e2d\u7684\u65bd\u6e2c\u3001\u8cc7\u6599\u6a94\u3001\u65b0\u7248\u984c\u76ee\u3001\u65b0\u984c\u76ee",
    # 原文：res <- kitItemBank::revision_patch(kit, \"改版底稿_填好.xlsx\", adminID = 
    paste0("res <- kitItemBank::revision_patch(kit, \"\u6539\u7248\u5e95\u7a3f_\u586b\u597d.xlsx\", adminID = ",
           q(admin_id), ")"),
    "kit2 <- kitItemBank::apply_patch(kit, res$patch)",
    "kitItemBank::check_kit(kit2)"
  ), collapse = "\n")
}
