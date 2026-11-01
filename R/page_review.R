# R/page_review.R — 審核（收修改包 → 看改前改後、檢查、影響範圍 → 下載套用後的題庫）
#
# 由上往下：這一頁做什麼 → 左 1/3：選修改包、原因／決定者／送出者／審核人
#   ｜右 2/3：修改包的摘要 → 版本對不對 → 套不上的改動 → 檢查套用後的題庫
#   → 影響範圍 → 改前改後 → 下載套用後的題庫（zip：改到的表、新的 BANK_VERSION、改動紀錄）
# → ▸ 改動紀錄的格式 ▸ 用 R 怎麼做
# app 不寫檔、不連網路：zip 交給懂技術的人放進 repo、push，雲端 app 重新部署後就用新版題庫。
# 計算都在 R/review_patch.R。

#' Review page UI
#'
#' @param id Module id.
#' @return The page UI.
#' @export
page_review_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kitDesign::kit_tab_guide(
      # 原文：審核別人送來的修改包：看清楚改了什麼、會不會弄壞題庫、影響哪些題目和施測，沒問題就下載套用後的題庫。
      goal = "\u5be9\u6838\u5225\u4eba\u9001\u4f86\u7684\u4fee\u6539\u5305\uff1a\u770b\u6e05\u695a\u6539\u4e86\u4ec0\u9ebc\u3001\u6703\u4e0d\u6703\u5f04\u58de\u984c\u5eab\u3001\u5f71\u97ff\u54ea\u4e9b\u984c\u76ee\u548c\u65bd\u6e2c\uff0c\u6c92\u554f\u984c\u5c31\u4e0b\u8f09\u5957\u7528\u5f8c\u7684\u984c\u5eab\u3002",
      steps = c(
        # 原文：選一個修改包（在「題庫編修」下載的 Excel）。
        "\u9078\u4e00\u500b\u4fee\u6539\u5305\uff08\u5728\u300c\u984c\u5eab\u7de8\u4fee\u300d\u4e0b\u8f09\u7684 Excel\uff09\u3002",
        # 原文：看右邊：改前改後、檢查結果、影響範圍；修改包是從舊版題庫做的時候會提醒你。
        "\u770b\u53f3\u908a\uff1a\u6539\u524d\u6539\u5f8c\u3001\u6aa2\u67e5\u7d50\u679c\u3001\u5f71\u97ff\u7bc4\u570d\uff1b\u4fee\u6539\u5305\u662f\u5f9e\u820a\u7248\u984c\u5eab\u505a\u7684\u6642\u5019\u6703\u63d0\u9192\u4f60\u3002",
        # 原文：填修改原因、誰決定的、誰送來的、你的名字；修改包裡有記的會先填好。
        "\u586b\u4fee\u6539\u539f\u56e0\u3001\u8ab0\u6c7a\u5b9a\u7684\u3001\u8ab0\u9001\u4f86\u7684\u3001\u4f60\u7684\u540d\u5b57\uff1b\u4fee\u6539\u5305\u88e1\u6709\u8a18\u7684\u6703\u5148\u586b\u597d\u3002",
        # 原文：按「下載套用後的題庫（zip）」，把 zip 交給懂技術的人放進 repo、push。
        "\u6309\u300c\u4e0b\u8f09\u5957\u7528\u5f8c\u7684\u984c\u5eab\uff08zip\uff09\u300d\uff0c\u628a zip \u4ea4\u7d66\u61c2\u6280\u8853\u7684\u4eba\u653e\u9032 repo\u3001push\u3002"
      ),
      # 原文：套用後的題庫（zip）：改到的表、新的題庫版本（BANK_VERSION）和這次的改動紀錄（changes/）。
      output = "\u5957\u7528\u5f8c\u7684\u984c\u5eab\uff08zip\uff09\uff1a\u6539\u5230\u7684\u8868\u3001\u65b0\u7684\u984c\u5eab\u7248\u672c\uff08BANK_VERSION\uff09\u548c\u9019\u6b21\u7684\u6539\u52d5\u7d00\u9304\uff08changes/\uff09\u3002",
      # 原文：這一頁不會改任何檔案。zip 的內容放進 repo、push 以後，雲端的 app 重新部署才會用新版題庫。
      note = "\u9019\u4e00\u9801\u4e0d\u6703\u6539\u4efb\u4f55\u6a94\u6848\u3002zip \u7684\u5167\u5bb9\u653e\u9032 repo\u3001push \u4ee5\u5f8c\uff0c\u96f2\u7aef\u7684 app \u91cd\u65b0\u90e8\u7f72\u624d\u6703\u7528\u65b0\u7248\u984c\u5eab\u3002"
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::fileInput(
          # 原文：修改包（Excel） | 選擇修改包…
          ns("file"), "\u4fee\u6539\u5305\uff08Excel\uff09", buttonLabel = "\u9078\u64c7\u4fee\u6539\u5305\u2026",
          # 原文：還沒有選
          placeholder = "\u9084\u6c92\u6709\u9078", accept = ".xlsx", width = "100%"
        ),
        # 原文：修改原因
        shiny::textAreaInput(ns("reason"), "\u4fee\u6539\u539f\u56e0", rows = 3, width = "100%"),
        # 原文：誰決定的（例如：題庫小組會議）
        shiny::textInput(ns("decided_by"), "\u8ab0\u6c7a\u5b9a\u7684\uff08\u4f8b\u5982\uff1a\u984c\u5eab\u5c0f\u7d44\u6703\u8b70\uff09", width = "100%"),
        # 原文：誰送來的
        shiny::textInput(ns("submitted_by"), "\u8ab0\u9001\u4f86\u7684", width = "100%"),
        # 原文：審核人（你的名字）
        shiny::textInput(ns("approved_by"), "\u5be9\u6838\u4eba\uff08\u4f60\u7684\u540d\u5b57\uff09", width = "100%"),
        # 原文：這四欄會寫進改動紀錄。下載前，修改原因和審核人一定要填。
        shiny::p(class = "kit-hint", "\u9019\u56db\u6b04\u6703\u5beb\u9032\u6539\u52d5\u7d00\u9304\u3002\u4e0b\u8f09\u524d\uff0c\u4fee\u6539\u539f\u56e0\u548c\u5be9\u6838\u4eba\u4e00\u5b9a\u8981\u586b\u3002")
      ),
      shiny::column(8, shiny::uiOutput(ns("result")))
    ),
    shiny::uiOutput(ns("download_ui")),
    kitDesign::kit_details_row(
      # 原文：改動紀錄的格式
      kitDesign::kit_detail("\u6539\u52d5\u7d00\u9304\u7684\u683c\u5f0f", review_record_help()),
      kitDesign::kit_code_panel(ns("code"))
    )
  )
}

#' Review page server
#'
#' @param id Module id.
#' @param kit The current bank (kit): a reactive, or a list.
#' @param goto Go to another tab: `goto("search", item = "cogC01", uid = "P.cogC01")`,
#'   `goto("admin", admin = "M03M036")`.
#' @param version The current bank version (a string or a reactive).
#' @return A list of reactives: `review` (the [review_patch()] result) and `meta` (the people
#'   fields for the change record).
#' @export
page_review_server <- function(id, kit, goto, version = kit_version()) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    kit_now <- if (shiny::is.reactive(kit)) kit else function() kit
    version_now <- if (shiny::is.reactive(version)) version else function() version

    # ── 讀修改包 ─────────────────────────────────────
    pkg <- shiny::reactive({
      f <- input$file
      shiny::req(f)
      tryCatch(c(read_review_package(f$datapath), list(name = f$name)),
                error = function(e) list(error = e, name = f$name))
    })
    # 修改包裡有記的原因、決定者、送出者，先填好
    shiny::observeEvent(pkg(), {
      p <- pkg()
      if (!is.null(p$error)) return()
      for (f in PATCH_META_FIELDS) {
        v <- p$meta[[f]] %||% ""
        if (nzchar(v)) {
          if (f == "reason") shiny::updateTextAreaInput(session, f, value = v)
          else shiny::updateTextInput(session, f, value = v)
        }
      }
    })
    review <- shiny::reactive({
      p <- pkg()
      shiny::req(is.null(p$error))
      review_patch(kit_now(), p$patch, p$base_version, version_now())
    })
    meta <- shiny::reactive({
      v <- function(x) trimws(input[[x]] %||% "")
      list(reason = v("reason"), decided_by = v("decided_by"), submitted_by = v("submitted_by"),
           approved_by = v("approved_by"))
    })
    new_version <- shiny::reactive(bump_bank_version(version_now()))

    # ── 右邊：摘要、版本、檢查、影響、改前改後 ─────────────────
    output$result <- shiny::renderUI({
      if (is.null(input$file)) {
        return(shiny::p(class = "kit-hint",
                        # 原文：先在左邊選一個修改包（在「題庫編修」下載的 Excel），這裡會列出它改了什麼。
                        "\u5148\u5728\u5de6\u908a\u9078\u4e00\u500b\u4fee\u6539\u5305\uff08\u5728\u300c\u984c\u5eab\u7de8\u4fee\u300d\u4e0b\u8f09\u7684 Excel\uff09\uff0c\u9019\u88e1\u6703\u5217\u51fa\u5b83\u6539\u4e86\u4ec0\u9ebc\u3002"))
      }
      p <- pkg()
      if (!is.null(p$error)) {
        # 原文：讀修改包「 | 」
        return(kitDesign::kit_error_status(p$error, paste0("\u8b80\u4fee\u6539\u5305\u300c", p$name, "\u300d")))
      }
      rv <- review()
      pt <- rv$patch
      shiny::tagList(
        # 原文：「 | 」
        shiny::h4(paste0("\u300c", p$name, "\u300d")),
        shiny::p(class = "kit-settings-line", paste0(
          # 原文：共  |  列改動，改到題庫裡 
          "\u5171 ", kitDesign::kit_num(nrow(pt)), " \u5217\u6539\u52d5\uff0c\u6539\u5230\u984c\u5eab\u88e1 ",
          # 原文： 列
          kitDesign::kit_num(nrow(rv$changes)), " \u5217",
          # 原文：； |  列套不上 | 。
          if (any(!pt$ok)) paste0("\uff1b", kitDesign::kit_num(sum(!pt$ok)), " \u5217\u5957\u4e0d\u4e0a"), "\u3002"
        )),
        review_version_ui(rv),
        if (any(nzchar(pt$conflict))) kitDesign::kit_problems(
          # 原文：這些格子在修改包匯出以後被改過（套用會用修改包的值蓋掉）：
          head = "\u9019\u4e9b\u683c\u5b50\u5728\u4fee\u6539\u5305\u532f\u51fa\u4ee5\u5f8c\u88ab\u6539\u904e\uff08\u5957\u7528\u6703\u7528\u4fee\u6539\u5305\u7684\u503c\u84cb\u6389\uff09\uff1a",
          # 原文： 的 | ：
          paste0(kit_table_label(pt$table), " ", pt$key, " \u7684", kit_column_label(pt$column), "\uff1a",
                 pt$conflict)[nzchar(pt$conflict)]
        ),
        if (any(!pt$ok)) kitDesign::kit_problems(
          # 原文：套不上的改動（要先解決才能下載）：
          head = "\u5957\u4e0d\u4e0a\u7684\u6539\u52d5\uff08\u8981\u5148\u89e3\u6c7a\u624d\u80fd\u4e0b\u8f09\uff09\uff1a",
          # 原文：：
          paste0(kit_table_label(pt$table), " ", pt$key, "\uff1a", pt$problem)[!pt$ok]
        ),
        # 原文：檢查套用後的題庫
        shiny::h4("\u6aa2\u67e5\u5957\u7528\u5f8c\u7684\u984c\u5eab"),
        review_check_ui(rv$issues),
        if (nrow(rv$issues)) kitDesign::kit_table_scroll(DT::DTOutput(ns("issues_dt"))),
        # 原文：影響範圍
        shiny::h4("\u5f71\u97ff\u7bc4\u570d"),
        review_impact_ui(rv$impact),
        if (length(rv$impact$items)) shiny::p(class = "kit-hint",
                                              # 原文：點一題，到「題項查詢」看這一題。
                                              "\u9ede\u4e00\u984c\uff0c\u5230\u300c\u984c\u9805\u67e5\u8a62\u300d\u770b\u9019\u4e00\u984c\u3002"),
        if (length(rv$impact$items)) kitDesign::kit_table_scroll(DT::DTOutput(ns("items_dt"))),
        # 原文：改前改後（ |  列）
        shiny::h4(paste0("\u6539\u524d\u6539\u5f8c\uff08", kitDesign::kit_num(nrow(rv$changes)), " \u5217\uff09")),
        # 原文：只列出改到的欄；新增或刪除的列列出它全部有值的欄。
        shiny::p(class = "kit-hint", "\u53ea\u5217\u51fa\u6539\u5230\u7684\u6b04\uff1b\u65b0\u589e\u6216\u522a\u9664\u7684\u5217\u5217\u51fa\u5b83\u5168\u90e8\u6709\u503c\u7684\u6b04\u3002"),
        kitDesign::kit_table_scroll(DT::DTOutput(ns("changes_dt")))
      )
    })

    output$issues_dt <- DT::renderDT({
      df <- review_issue_table(review()$issues)
      # 原文：問題 | 哪一列
      kitDesign::kit_dt(df, page_length = 10L, wrap_cols = c("\u554f\u984c", "\u54ea\u4e00\u5217"))
    })
    output$changes_dt <- DT::renderDT({
      df <- review_change_table(review()$changes)
      kitDesign::kit_dt(df, page_length = 25L, search = TRUE,
                        # 原文：哪一列 | 改前 | 改後
                        wrap_cols = c("\u54ea\u4e00\u5217", "\u6539\u524d", "\u6539\u5f8c"),
                        # 原文：改前 | 改後
                        min_widths = c("\u6539\u524d" = "12rem", "\u6539\u5f8c" = "12rem"))
    })
    impact_items <- shiny::reactive({
      rv <- review()
      ids <- rv$impact$items
      iv <- rbind(kit_now()$item_versions, rv$kit$item_versions)
      label <- vapply(ids, function(u) {
        l <- iv$label[iv$item_uid == u & nzchar(iv$label)]
        if (length(l)) l[length(l)] else ""
      }, character(1))
      data.frame(item_uid = ids, varname = sub("^[A-Za-z]+\\.", "", ids), label = unname(label),
                 stringsAsFactors = FALSE)
    })
    output$items_dt <- DT::renderDT({
      x <- impact_items()
      # 原文：題目 | 代號 | 名稱
      df <- data.frame("\u984c\u76ee" = x$varname, "\u4ee3\u865f" = x$item_uid, "\u540d\u7a31" = cb_one_line(x$label),
                       check.names = FALSE, stringsAsFactors = FALSE)
      # 原文：名稱
      kitDesign::kit_dt(df, selection = "single", page_length = 10L, wrap_cols = "\u540d\u7a31")
    })
    shiny::observeEvent(input$items_dt_rows_selected, {
      i <- input$items_dt_rows_selected
      x <- impact_items()
      if (length(i) != 1L || i > nrow(x)) return()
      DT::selectRows(DT::dataTableProxy("items_dt"), NULL)
      if (!x$item_uid[i] %in% kit_now()$items$item_uid) {
        # 原文：「 | 」是這個修改包新增的題目，題庫裡還沒有。
        kitDesign::kit_notify(paste0("\u300c", x$item_uid[i], "\u300d\u662f\u9019\u500b\u4fee\u6539\u5305\u65b0\u589e\u7684\u984c\u76ee\uff0c\u984c\u5eab\u88e1\u9084\u6c92\u6709\u3002"),
                              session)
        return()
      }
      goto("search", item = x$varname[i], uid = x$item_uid[i])
    })

    # ── 下載套用後的題庫 ─────────────────────────────────
    output$download_ui <- shiny::renderUI({
      p <- pkg()
      if (!is.null(p$error)) return(NULL)
      rv <- review()
      m <- meta()
      nv <- new_version()
      # 原文：修改原因 | 審核人
      miss <- c(if (!nzchar(m$reason)) "\u4fee\u6539\u539f\u56e0", if (!nzchar(m$approved_by)) "\u5be9\u6838\u4eba")
      # 原文：填 | 、
      todo <- c(rv$blocking, if (length(miss)) paste0("\u586b", paste(miss, collapse = "\u3001")))
      shiny::tagList(
        # 原文：下載套用後的題庫
        shiny::h4("\u4e0b\u8f09\u5957\u7528\u5f8c\u7684\u984c\u5eab"),
        if (length(todo)) {
          shiny::tagList(
            kitDesign::kit_ready_line(todo),
            # 原文：下載套用後的題庫（zip）
            kitDesign::kit_button(ns("dl_off"), "\u4e0b\u8f09\u5957\u7528\u5f8c\u7684\u984c\u5eab\uff08zip\uff09", "primary",
                                  disabled = TRUE)
          )
        } else {
          shiny::tagList(
            # 原文：套用後題庫會從  |  版變成 
            shiny::p(paste0("\u5957\u7528\u5f8c\u984c\u5eab\u6703\u5f9e ", rv$current_version, " \u7248\u8b8a\u6210 ", nv,
                            # 原文： 版。zip 裡有改到的表、新的 BANK_VERSION 和這次的改動紀錄（changes/
                            " \u7248\u3002zip \u88e1\u6709\u6539\u5230\u7684\u8868\u3001\u65b0\u7684 BANK_VERSION \u548c\u9019\u6b21\u7684\u6539\u52d5\u7d00\u9304\uff08changes/",
                            # 原文：.csv）。
                            nv, ".csv\uff09\u3002")),
            shiny::div(
              class = "kit-buttons",
              kitDesign::kit_download_button(
                # 原文：下載套用後的題庫（zip）
                session, "dl_zip", "\u4e0b\u8f09\u5957\u7528\u5f8c\u7684\u984c\u5eab\uff08zip\uff09",
                paste0("kit_", nv, "_", format(Sys.Date(), "%Y%m%d"), ".zip"),
                function(file) {
                  r <- review()
                  rec <- change_record(r, meta(), new_version(), row_meta = pkg()$row_meta)
                  review_zip(r, rec, new_version(), file)
                },
                class = kitDesign::kit_btn_class("primary")
              )
            )
          )
        },
        kitDesign::kit_callout(
          # 原文：下載以後：
          shiny::tags$strong("\u4e0b\u8f09\u4ee5\u5f8c\uff1a"),
          # 原文：把 zip 交給懂技術的人：解壓縮後，把 kit 資料夾裡的檔案放進 repo 的 
          paste0("\u628a zip \u4ea4\u7d66\u61c2\u6280\u8853\u7684\u4eba\uff1a\u89e3\u58d3\u7e2e\u5f8c\uff0c\u628a kit \u8cc7\u6599\u593e\u88e1\u7684\u6a94\u6848\u653e\u9032 repo \u7684 ",
                 # 原文：inst/extdata/kit/（蓋掉同名的檔案），commit、push。雲端的 app 重新部署以後，
                 "inst/extdata/kit/\uff08\u84cb\u6389\u540c\u540d\u7684\u6a94\u6848\uff09\uff0ccommit\u3001push\u3002\u96f2\u7aef\u7684 app \u91cd\u65b0\u90e8\u7f72\u4ee5\u5f8c\uff0c",
                 # 原文：就會用新版題庫。在那之前，這個 app 還是舊版，不要再用它審核下一個修改包。
                 "\u5c31\u6703\u7528\u65b0\u7248\u984c\u5eab\u3002\u5728\u90a3\u4e4b\u524d\uff0c\u9019\u500b app \u9084\u662f\u820a\u7248\uff0c\u4e0d\u8981\u518d\u7528\u5b83\u5be9\u6838\u4e0b\u4e00\u500b\u4fee\u6539\u5305\u3002")
        )
      )
    })

    output$code <- kitDesign::kit_render_code_panel(
      function() {
        # 原文：修改包.xlsx
        name <- input$file$name %||% "\u4fee\u6539\u5305.xlsx"
        kitDesign::kit_code(c(
          # 原文：# 讀修改包，看它套到內建題庫的結果
          "# \u8b80\u4fee\u6539\u5305\uff0c\u770b\u5b83\u5957\u5230\u5167\u5efa\u984c\u5eab\u7684\u7d50\u679c",
          paste0("pkg <- kitItemBank::read_review_package(\"", name, "\")"),
          "rv <- kitItemBank::review_patch(kitItemBank::read_kit(), pkg$patch, pkg$base_version)",
          # 原文：rv$changes   # 改前改後
          "rv$changes   # \u6539\u524d\u6539\u5f8c",
          # 原文：rv$issues    # check_kit() 的結果
          "rv$issues    # check_kit() \u7684\u7d50\u679c",
          # 原文：rv$impact    # 影響到的題目、題組、資料檔、施測
          "rv$impact    # \u5f71\u97ff\u5230\u7684\u984c\u76ee\u3001\u984c\u7d44\u3001\u8cc7\u6599\u6a94\u3001\u65bd\u6e2c",
          # 原文：rv$blocking  # 不能下載套用後題庫的原因（空的 = 可以）
          "rv$blocking  # \u4e0d\u80fd\u4e0b\u8f09\u5957\u7528\u5f8c\u984c\u5eab\u7684\u539f\u56e0\uff08\u7a7a\u7684 = \u53ef\u4ee5\uff09",
          "",
          # 原文：# 和這一頁下載的 zip 一樣（在原始碼資料夾執行，等於把 zip 解壓縮到 inst/extdata/）
          "# \u548c\u9019\u4e00\u9801\u4e0b\u8f09\u7684 zip \u4e00\u6a23\uff08\u5728\u539f\u59cb\u78bc\u8cc7\u6599\u593e\u57f7\u884c\uff0c\u7b49\u65bc\u628a zip \u89e3\u58d3\u7e2e\u5230 inst/extdata/\uff09",
          "v <- kitItemBank::bump_bank_version(rv$current_version)",
          # 原文：rec <- kitItemBank::change_record(rv, list(reason = \"…\", approved_by = \"…\"), v)
          "rec <- kitItemBank::change_record(rv, list(reason = \"\u2026\", approved_by = \"\u2026\"), v)",
          "kitItemBank::review_zip(rv, rec, v, \"kit.zip\")",
          "utils::unzip(\"kit.zip\", exdir = \"inst/extdata\")"
        ))
      },
      # 原文：這一段給題庫維護者用。
      note = "\u9019\u4e00\u6bb5\u7d66\u984c\u5eab\u7dad\u8b77\u8005\u7528\u3002"
    )

    invisible(list(review = review, meta = meta))
  })
}

# 版本對不對得上
review_version_ui <- function(rv) {
  if (rv$version_ok) {
    return(shiny::p(class = "kit-hint",
                    # 原文：這個修改包是從現在的題庫（ |  版）做的。
                    paste0("\u9019\u500b\u4fee\u6539\u5305\u662f\u5f9e\u73fe\u5728\u7684\u984c\u5eab\uff08", rv$current_version, " \u7248\uff09\u505a\u7684\u3002")))
  }
  kitDesign::kit_callout(
    warn = TRUE,
    if (!nzchar(rv$base_version)) {
      # 原文：這個修改包沒有記下是從哪一版題庫做的；現在的題庫是 
      paste0("\u9019\u500b\u4fee\u6539\u5305\u6c92\u6709\u8a18\u4e0b\u662f\u5f9e\u54ea\u4e00\u7248\u984c\u5eab\u505a\u7684\uff1b\u73fe\u5728\u7684\u984c\u5eab\u662f ", rv$current_version,
             # 原文： 版。請特別看下面的「改前」對不對。
             " \u7248\u3002\u8acb\u7279\u5225\u770b\u4e0b\u9762\u7684\u300c\u6539\u524d\u300d\u5c0d\u4e0d\u5c0d\u3002")
    } else {
      # 原文：這個修改包是從  |  版題庫做的，現在的題庫是 
      paste0("\u9019\u500b\u4fee\u6539\u5305\u662f\u5f9e ", rv$base_version, " \u7248\u984c\u5eab\u505a\u7684\uff0c\u73fe\u5728\u7684\u984c\u5eab\u662f ",
             # 原文： 版：匯出以後題庫改過了。請特別看下面的「改前」對不對。
             rv$current_version, " \u7248\uff1a\u532f\u51fa\u4ee5\u5f8c\u984c\u5eab\u6539\u904e\u4e86\u3002\u8acb\u7279\u5225\u770b\u4e0b\u9762\u7684\u300c\u6539\u524d\u300d\u5c0d\u4e0d\u5c0d\u3002")
    }
  )
}

# 檢查結果的一句話
review_check_ui <- function(issues) {
  n_new <- sum(issues$new)
  n_old <- sum(!issues$new)
  if (n_new == 0L) {
    return(kitDesign::kit_status(
      # 原文：檢查通過：套用後的題庫沒有新的結構問題。
      "success", "\u6aa2\u67e5\u901a\u904e\uff1a\u5957\u7528\u5f8c\u7684\u984c\u5eab\u6c92\u6709\u65b0\u7684\u7d50\u69cb\u554f\u984c\u3002",
      # 原文：題庫原本就有 
      notes = if (n_old > 0L) paste0("\u984c\u5eab\u539f\u672c\u5c31\u6709 ", kitDesign::kit_num(n_old),
                                     # 原文： 個問題，和這個修改包無關，列在下面。
                                     " \u500b\u554f\u984c\uff0c\u548c\u9019\u500b\u4fee\u6539\u5305\u7121\u95dc\uff0c\u5217\u5728\u4e0b\u9762\u3002") else character(0)
    ))
  }
  kitDesign::kit_status(
    # 原文：套用後的題庫有  |  個新的結構問題，不能套用。
    "error", paste0("\u5957\u7528\u5f8c\u7684\u984c\u5eab\u6709 ", kitDesign::kit_num(n_new), " \u500b\u65b0\u7684\u7d50\u69cb\u554f\u984c\uff0c\u4e0d\u80fd\u5957\u7528\u3002"),
    # 原文：下面的表列出每個問題；要請送修改包的人改好再送一次。
    notes = "\u4e0b\u9762\u7684\u8868\u5217\u51fa\u6bcf\u500b\u554f\u984c\uff1b\u8981\u8acb\u9001\u4fee\u6539\u5305\u7684\u4eba\u6539\u597d\u518d\u9001\u4e00\u6b21\u3002"
  )
}

# 影響範圍：一行數字 + 施測、資料檔、題組的清單
review_impact_ui <- function(impact) {
  tb <- review_impact_table(impact)
  # 原文： 個 | 、
  line <- paste0(tb[[1]], " ", kitDesign::kit_num(tb[[2]]), " \u500b", collapse = "\u3001")
  lst <- function(title, x) {
    if (!length(x)) return(NULL)
    # 原文：： | 、
    shiny::p(shiny::tags$strong(paste0(title, "\uff1a")), paste(x, collapse = "\u3001"),
             style = "overflow-wrap: anywhere;")
  }
  shiny::tagList(
    # 原文：影響到： | 。
    shiny::p(class = "kit-settings-line", paste0("\u5f71\u97ff\u5230\uff1a", line, "\u3002")),
    # 原文：施測
    lst("\u65bd\u6e2c", impact$admins),
    # 原文：資料檔
    lst("\u8cc7\u6599\u6a94", impact$datasets),
    # 原文：題本
    lst("\u984c\u672c", impact$forms),
    # 原文：題組
    lst("\u984c\u7d44", impact$groups)
  )
}

# ▸ 改動紀錄的格式
review_record_help <- function() {
  shiny::tagList(
    # 原文：每個修改包留一個改動紀錄：inst/extdata/kit/changes/<新版本>.csv（UTF-8），一列一個改動。欄位：
    shiny::p("\u6bcf\u500b\u4fee\u6539\u5305\u7559\u4e00\u500b\u6539\u52d5\u7d00\u9304\uff1ainst/extdata/kit/changes/<\u65b0\u7248\u672c>.csv\uff08UTF-8\uff09\uff0c\u4e00\u5217\u4e00\u500b\u6539\u52d5\u3002\u6b04\u4f4d\uff1a"),
    shiny::tags$ul(
      # 原文：bank_version：套用後的新題庫版本；base_version：修改包是從哪一版做的；applied_to：實際套到哪一版
      shiny::tags$li("bank_version\uff1a\u5957\u7528\u5f8c\u7684\u65b0\u984c\u5eab\u7248\u672c\uff1bbase_version\uff1a\u4fee\u6539\u5305\u662f\u5f9e\u54ea\u4e00\u7248\u505a\u7684\uff1bapplied_to\uff1a\u5be6\u969b\u5957\u5230\u54ea\u4e00\u7248"),
      # 原文：date：審核（下載）的日期；op：add 新增一列、delete 刪除一列、set 改一格
      shiny::tags$li("date\uff1a\u5be9\u6838\uff08\u4e0b\u8f09\uff09\u7684\u65e5\u671f\uff1bop\uff1aadd \u65b0\u589e\u4e00\u5217\u3001delete \u522a\u9664\u4e00\u5217\u3001set \u6539\u4e00\u683c"),
      # 原文：table、key、column：哪張表、哪一列（主鍵用 | 接起來）、哪一欄
      shiny::tags$li("table\u3001key\u3001column\uff1a\u54ea\u5f35\u8868\u3001\u54ea\u4e00\u5217\uff08\u4e3b\u9375\u7528 | \u63a5\u8d77\u4f86\uff09\u3001\u54ea\u4e00\u6b04"),
      # 原文：from、to：改前、改後的值（改前用題庫當時真正的值）
      shiny::tags$li("from\u3001to\uff1a\u6539\u524d\u3001\u6539\u5f8c\u7684\u503c\uff08\u6539\u524d\u7528\u984c\u5eab\u7576\u6642\u771f\u6b63\u7684\u503c\uff09"),
      # 原文：reason、decided_by、submitted_by、approved_by：原因、誰決定的、誰送來的、誰審核的
      shiny::tags$li("reason\u3001decided_by\u3001submitted_by\u3001approved_by\uff1a\u539f\u56e0\u3001\u8ab0\u6c7a\u5b9a\u7684\u3001\u8ab0\u9001\u4f86\u7684\u3001\u8ab0\u5be9\u6838\u7684")
    )
  )
}
