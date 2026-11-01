# R/page_stats.R — 統計（證據層）：上傳一個資料檔，看題項統計和題組統計
#
# 由上往下：這一頁做什麼 → 條件（左 1/3：資料檔、上傳、執行者）｜右 2/3：
#   沒上傳：提示
#   上傳了：對到幾個變項 → 下載（Excel、題庫列 CSV）→ 題項統計表 → 題組統計表
#
# 隱私（一定要做）：
#   - 只有 options(kititembank.allow_data_upload = TRUE) 時才能用（run_kititembank() 開本機版時會設好）；
#     沒開時整頁只有一段說明，沒有上傳的地方，伺服器也什麼都不做
#   - 上傳的檔案讀進記憶體後馬上刪掉（stats_read_upload()），不寫到任何地方、不印出值
#   - 只留下彙總；「清除資料」把記憶體裡的資料丟掉
# 計算都在 R/stats_items.R、R/stats_groups.R、R/stats_evidence.R。

#' Statistics page UI
#'
#' @param id Module id.
#' @return UI. When `getOption("kititembank.allow_data_upload")` is not `TRUE`, only an
#'   explanation that statistics run on the local version only.
#' @export
page_stats_ui <- function(id) {
  ns <- shiny::NS(id)
  guide <- kitDesign::kit_tab_guide(
    # 原文：上傳一個資料檔（一波、一列一個孩子），看每一題和每個題組的統計，給題庫當證據。
    goal = "\u4e0a\u50b3\u4e00\u500b\u8cc7\u6599\u6a94\uff08\u4e00\u6ce2\u3001\u4e00\u5217\u4e00\u500b\u5b69\u5b50\uff09\uff0c\u770b\u6bcf\u4e00\u984c\u548c\u6bcf\u500b\u984c\u7d44\u7684\u7d71\u8a08\uff0c\u7d66\u984c\u5eab\u7576\u8b49\u64da\u3002",
    steps = c(
      # 原文：在左邊選資料檔（例如 M36M084G01-P），再上傳這個資料檔的 .sav 或 .csv。
      "\u5728\u5de6\u908a\u9078\u8cc7\u6599\u6a94\uff08\u4f8b\u5982 M36M084G01-P\uff09\uff0c\u518d\u4e0a\u50b3\u9019\u500b\u8cc7\u6599\u6a94\u7684 .sav \u6216 .csv\u3002",
      # 原文：變項名稱照 SPSS 的寫法，大小寫不分；題庫沒有的欄位不會用到。
      "\u8b8a\u9805\u540d\u7a31\u7167 SPSS \u7684\u5beb\u6cd5\uff0c\u5927\u5c0f\u5beb\u4e0d\u5206\uff1b\u984c\u5eab\u6c92\u6709\u7684\u6b04\u4f4d\u4e0d\u6703\u7528\u5230\u3002",
      # 原文：看右邊的題項統計和題組統計；要給同事看或交給維護者，按下載。
      "\u770b\u53f3\u908a\u7684\u984c\u9805\u7d71\u8a08\u548c\u984c\u7d44\u7d71\u8a08\uff1b\u8981\u7d66\u540c\u4e8b\u770b\u6216\u4ea4\u7d66\u7dad\u8b77\u8005\uff0c\u6309\u4e0b\u8f09\u3002"
    ),
    # 原文：題項統計（人數、遺漏代碼、平均數、百分位數、選項次數）；題組統計（α、ω、負荷量、反向題檢查）；Excel 和題庫列 CSV。
    output = "\u984c\u9805\u7d71\u8a08\uff08\u4eba\u6578\u3001\u907a\u6f0f\u4ee3\u78bc\u3001\u5e73\u5747\u6578\u3001\u767e\u5206\u4f4d\u6578\u3001\u9078\u9805\u6b21\u6578\uff09\uff1b\u984c\u7d44\u7d71\u8a08\uff08\u03b1\u3001\u03c9\u3001\u8ca0\u8377\u91cf\u3001\u53cd\u5411\u984c\u6aa2\u67e5\uff09\uff1bExcel \u548c\u984c\u5eab\u5217 CSV\u3002",
    # 原文：資料只放在這台電腦的記憶體裡，讀完就刪掉上傳的暫存檔，不會存到題庫或雲端。次數 1 到 4 寫成「<5」，有效人數少於 10 不列平均數。
    note = "\u8cc7\u6599\u53ea\u653e\u5728\u9019\u53f0\u96fb\u8166\u7684\u8a18\u61b6\u9ad4\u88e1\uff0c\u8b80\u5b8c\u5c31\u522a\u6389\u4e0a\u50b3\u7684\u66ab\u5b58\u6a94\uff0c\u4e0d\u6703\u5b58\u5230\u984c\u5eab\u6216\u96f2\u7aef\u3002\u6b21\u6578 1 \u5230 4 \u5beb\u6210\u300c<5\u300d\uff0c\u6709\u6548\u4eba\u6578\u5c11\u65bc 10 \u4e0d\u5217\u5e73\u5747\u6578\u3002"
  )
  if (!stats_upload_allowed()) {
    return(shiny::tagList(
      guide,
      shiny::div(
        class = "kit-stats-off",
        # 原文：這一頁在這裡不能用
        shiny::h4("\u9019\u4e00\u9801\u5728\u9019\u88e1\u4e0d\u80fd\u7528"),
        # 原文：統計要用到每個孩子的原始資料。為了保護資料，原始資料不能上傳到雲端，所以統計只能在本機版本（自己電腦上執行的 kitItemBank）做。
        shiny::p("\u7d71\u8a08\u8981\u7528\u5230\u6bcf\u500b\u5b69\u5b50\u7684\u539f\u59cb\u8cc7\u6599\u3002\u70ba\u4e86\u4fdd\u8b77\u8cc7\u6599\uff0c\u539f\u59cb\u8cc7\u6599\u4e0d\u80fd\u4e0a\u50b3\u5230\u96f2\u7aef\uff0c\u6240\u4ee5\u7d71\u8a08\u53ea\u80fd\u5728\u672c\u6a5f\u7248\u672c\uff08\u81ea\u5df1\u96fb\u8166\u4e0a\u57f7\u884c\u7684 kitItemBank\uff09\u505a\u3002"),
        # 原文：要做統計，請在自己的電腦上打開本機版：在 R 裡執行 
        shiny::p("\u8981\u505a\u7d71\u8a08\uff0c\u8acb\u5728\u81ea\u5df1\u7684\u96fb\u8166\u4e0a\u6253\u958b\u672c\u6a5f\u7248\uff1a\u5728 R \u88e1\u57f7\u884c ",
                 shiny::tags$code("kitItemBank::run_kititembank()"),
                 # 原文：。本機版會打開統計頁，資料只留在你的電腦。
                 "\u3002\u672c\u6a5f\u7248\u6703\u6253\u958b\u7d71\u8a08\u9801\uff0c\u8cc7\u6599\u53ea\u7559\u5728\u4f60\u7684\u96fb\u8166\u3002"),
        # 原文：已經算好的統計，維護者加進題庫以後，大家都看得到。
        shiny::p(class = "kit-hint", "\u5df2\u7d93\u7b97\u597d\u7684\u7d71\u8a08\uff0c\u7dad\u8b77\u8005\u52a0\u9032\u984c\u5eab\u4ee5\u5f8c\uff0c\u5927\u5bb6\u90fd\u770b\u5f97\u5230\u3002")
      )
    ))
  }
  shiny::tagList(
    guide,
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectizeInput(
          # 原文：資料檔
          ns("dataset"), "\u8cc7\u6599\u6a94", choices = NULL, width = "100%",
          # 原文：搜尋資料檔（代號）…
          options = list(placeholder = "\u641c\u5c0b\u8cc7\u6599\u6a94\uff08\u4ee3\u865f\uff09\u2026")
        ),
        # 原文：上傳資料檔（.sav 或 .csv）
        shiny::fileInput(ns("file"), "\u4e0a\u50b3\u8cc7\u6599\u6a94\uff08.sav \u6216 .csv\uff09",
                         # 原文：選檔案… | 還沒有上傳
                         buttonLabel = "\u9078\u6a94\u6848\u2026", placeholder = "\u9084\u6c92\u6709\u4e0a\u50b3",
                         accept = c(".sav", ".csv"), width = "100%"),
        # 原文：執行者
        shiny::textInput(ns("run_by"), "\u57f7\u884c\u8005", width = "100%"),
        # 原文：這是這個資料檔的正式統計
        shiny::checkboxInput(ns("official"), "\u9019\u662f\u9019\u500b\u8cc7\u6599\u6a94\u7684\u6b63\u5f0f\u7d71\u8a08", value = FALSE),
        # 原文：備註
        shiny::textInput(ns("note"), "\u5099\u8a3b", width = "100%"),
        # 原文：清除資料
        shiny::div(class = "kit-buttons", kitDesign::kit_button(ns("clear"), "\u6e05\u9664\u8cc7\u6599"))
      ),
      shiny::column(8, shiny::uiOutput(ns("result")))
    )
  )
}

#' Statistics page server
#'
#' Does nothing unless `getOption("kititembank.allow_data_upload")` is `TRUE`.
#'
#' @param id Module id.
#' @param kit A kit list (or a reactive returning one).
#' @return Invisibly, a list of reactives (`data_info`, `result`), or `NULL` when disabled.
#' @export
page_stats_server <- function(id, kit) {
  shiny::moduleServer(id, function(input, output, session) {
    if (!stats_upload_allowed()) return(invisible(NULL))
    ns <- session$ns
    kit_now <- if (shiny::is.reactive(kit)) kit else function() kit
    # 記憶體裡的資料：list(data, name)；只在這個 session 裡
    uploaded <- shiny::reactiveVal(NULL)

    shiny::observe({
      ds <- kit_now()$datasets
      # 原文：（ | ）
      lab <- paste0(ds$datasetID, ifelse(nzchar(ds$respondent), paste0("\uff08", ds$respondent, "\uff09"), ""))
      shiny::updateSelectizeInput(session, "dataset", choices = stats::setNames(ds$datasetID, lab),
                                  selected = shiny::isolate(input$dataset) %||% "", server = TRUE)
    })

    shiny::observeEvent(input$file, {
      f <- input$file
      shiny::req(f)
      # 讀完（不管成不成功）就刪掉暫存檔
      d <- tryCatch(stats_read_upload(f$datapath[1], f$name[1]), error = function(e) e)
      unlink(f$datapath)
      if (inherits(d, "error")) {
        uploaded(NULL)
        # 原文：讀不到這個檔案：
        kitDesign::kit_notify(paste0("\u8b80\u4e0d\u5230\u9019\u500b\u6a94\u6848\uff1a", conditionMessage(d)), session)
        return()
      }
      uploaded(list(data = d, name = f$name[1]))
    })
    shiny::observeEvent(input$clear, {
      uploaded(NULL)
      shiny::updateTextInput(session, "note", value = "")
      session$sendInputMessage("file", list(value = NULL))
    })

    data_info <- shiny::reactive({
      u <- uploaded()
      d <- input$dataset
      shiny::req(u, nzchar(d %||% ""))
      k <- kit_now()
      dv <- k$dataset_vars[k$dataset_vars$datasetID == d, , drop = FALSE]
      hit <- tolower(names(u$data)) %in% tolower(dv$varname)
      list(name = u$name, n_rows = nrow(u$data), n_cols = ncol(u$data), n_match = sum(hit),
           n_items = sum(tolower(dv$varname[dv$kind == "item"]) %in% tolower(names(u$data))),
           unmatched = names(u$data)[!hit])
    })

    stats <- shiny::reactive({
      u <- uploaded()
      d <- input$dataset
      shiny::req(u, nzchar(d %||% ""))
      k <- kit_now()
      # 原文：計算中…
      shiny::withProgress(message = "\u8a08\u7b97\u4e2d\u2026", value = 0.5, {
        list(datasetID = d, analysisID = stats_new_id(d),
             items = item_stats(u$data, k, d), groups = group_stats(u$data, k, d))
      })
    })

    # 分析代號跟著這次計算走：改執行者、備註不會換代號
    result <- shiny::reactive({
      s <- stats()
      stats_bundle(s$items, s$groups, s$datasetID, analysisID = s$analysisID,
                   run_by = input$run_by, official = input$official, note = input$note)
    })

    output$result <- shiny::renderUI({
      u <- uploaded()
      if (is.null(u)) {
        # 原文：先在左邊選資料檔、上傳資料，這裡會列出統計。資料只留在這台電腦的記憶體裡。
        return(shiny::p(class = "kit-hint", "\u5148\u5728\u5de6\u908a\u9078\u8cc7\u6599\u6a94\u3001\u4e0a\u50b3\u8cc7\u6599\uff0c\u9019\u88e1\u6703\u5217\u51fa\u7d71\u8a08\u3002\u8cc7\u6599\u53ea\u7559\u5728\u9019\u53f0\u96fb\u8166\u7684\u8a18\u61b6\u9ad4\u88e1\u3002"))
      }
      if (!nzchar(input$dataset %||% "")) {
        # 原文：已經讀進資料，請在左邊選這是哪一個資料檔。
        return(shiny::p(class = "kit-hint", "\u5df2\u7d93\u8b80\u9032\u8cc7\u6599\uff0c\u8acb\u5728\u5de6\u908a\u9078\u9019\u662f\u54ea\u4e00\u500b\u8cc7\u6599\u6a94\u3002"))
      }
      info <- data_info()
      r <- result()
      n_grp <- sum(!nzchar(r$group_stats$varname))
      day <- format(Sys.Date(), "%Y%m%d")
      d <- input$dataset
      cls <- kitDesign::kit_btn_class("secondary")
      shiny::tagList(
        # 原文： 的統計
        shiny::h4(paste0(d, " \u7684\u7d71\u8a08")),
        shiny::p(class = "kit-hint", paste0(
          # 原文：檔案  | ： |  列、
          "\u6a94\u6848 ", info$name, "\uff1a", kitDesign::kit_num(info$n_rows), " \u5217\u3001",
          # 原文： 欄；對到題庫 
          kitDesign::kit_num(info$n_cols), " \u6b04\uff1b\u5c0d\u5230\u984c\u5eab ", kitDesign::kit_num(info$n_match),
          # 原文： 個變項，其中  |  題。
          " \u500b\u8b8a\u9805\uff0c\u5176\u4e2d ", kitDesign::kit_num(info$n_items), " \u984c\u3002",
          # 原文：題庫沒有的欄位（ |  個）不會用到。
          if (length(info$unmatched)) paste0("\u984c\u5eab\u6c92\u6709\u7684\u6b04\u4f4d\uff08", length(info$unmatched), " \u500b\uff09\u4e0d\u6703\u7528\u5230\u3002") else ""
        )),
        shiny::div(
          class = "kit-buttons",
          kitDesign::kit_download_button(
            # 原文：下載統計（Excel）
            session, "dl_xlsx", "\u4e0b\u8f09\u7d71\u8a08\uff08Excel\uff09",
            # 原文：統計_
            paste0("\u7d71\u8a08_", d, "_", day, ".xlsx"),
            function(file) writexl::write_xlsx(stats_workbook(result(), kit_now()), file),
            class = cls
          ),
          lapply(c("analyses", "item_stats", "group_stats"), function(tb) {
            kitDesign::kit_download_button(
              # 原文：下載題庫列：
              session, paste0("dl_", tb), paste0("\u4e0b\u8f09\u984c\u5eab\u5217\uff1a", tb, ".csv"),
              paste0(tb, "_", d, "_", day, ".csv"),
              function(file) readr::write_csv(result()[[tb]], file, na = ""),
              class = cls
            )
          })
        ),
        # 原文：Excel 第一張是說明（題庫版本、資料檔、日期、壓縮規則）。三個 CSV 是題庫的列，交給維護者照一般的審核流程加入。
        shiny::p(class = "kit-hint", "Excel \u7b2c\u4e00\u5f35\u662f\u8aaa\u660e\uff08\u984c\u5eab\u7248\u672c\u3001\u8cc7\u6599\u6a94\u3001\u65e5\u671f\u3001\u58d3\u7e2e\u898f\u5247\uff09\u3002\u4e09\u500b CSV \u662f\u984c\u5eab\u7684\u5217\uff0c\u4ea4\u7d66\u7dad\u8b77\u8005\u7167\u4e00\u822c\u7684\u5be9\u6838\u6d41\u7a0b\u52a0\u5165\u3002"),
        # 原文：題項統計（ |  個變項）
        shiny::h4(paste0("\u984c\u9805\u7d71\u8a08\uff08", kitDesign::kit_num(nrow(r$item_stats)), " \u500b\u8b8a\u9805\uff09")),
        # 原文：missing 是各遺漏代碼的人數；freq 是各選項的人數。次數 1 到 4 寫成「<5」；有效人數少於 10 不列平均數、標準差和百分位數。
        shiny::p(class = "kit-hint", "missing \u662f\u5404\u907a\u6f0f\u4ee3\u78bc\u7684\u4eba\u6578\uff1bfreq \u662f\u5404\u9078\u9805\u7684\u4eba\u6578\u3002\u6b21\u6578 1 \u5230 4 \u5beb\u6210\u300c<5\u300d\uff1b\u6709\u6548\u4eba\u6578\u5c11\u65bc 10 \u4e0d\u5217\u5e73\u5747\u6578\u3001\u6a19\u6e96\u5dee\u548c\u767e\u5206\u4f4d\u6578\u3002"),
        kitDesign::kit_table_scroll(DT::DTOutput(ns("items_dt"))),
        # 原文：題組統計（ |  個題組）
        shiny::h4(paste0("\u984c\u7d44\u7d71\u8a08\uff08", kitDesign::kit_num(n_grp), " \u500b\u984c\u7d44\uff09")),
        # 原文：每個題組先一列（α、ω、特徵值比；4 題以上加配適度，只供參考），接著是題組裡每一題（題目總分相關、刪題後 α、負荷量）。psych_reversed = 1 表示 psych 自己把這題反向，題庫的反向標記可能要再看。
        shiny::p(class = "kit-hint", "\u6bcf\u500b\u984c\u7d44\u5148\u4e00\u5217\uff08\u03b1\u3001\u03c9\u3001\u7279\u5fb5\u503c\u6bd4\uff1b4 \u984c\u4ee5\u4e0a\u52a0\u914d\u9069\u5ea6\uff0c\u53ea\u4f9b\u53c3\u8003\uff09\uff0c\u63a5\u8457\u662f\u984c\u7d44\u88e1\u6bcf\u4e00\u984c\uff08\u984c\u76ee\u7e3d\u5206\u76f8\u95dc\u3001\u522a\u984c\u5f8c \u03b1\u3001\u8ca0\u8377\u91cf\uff09\u3002psych_reversed = 1 \u8868\u793a psych \u81ea\u5df1\u628a\u9019\u984c\u53cd\u5411\uff0c\u984c\u5eab\u7684\u53cd\u5411\u6a19\u8a18\u53ef\u80fd\u8981\u518d\u770b\u3002"),
        kitDesign::kit_table_scroll(DT::DTOutput(ns("groups_dt")))
      )
    })

    output$items_dt <- DT::renderDT({
      x <- result()$item_stats
      x$analysisID <- NULL
      kitDesign::kit_dt(x, page_length = 25L, search = TRUE, wrap_cols = c("missing", "freq", "note"))
    })
    output$groups_dt <- DT::renderDT({
      x <- result()$group_stats
      x$analysisID <- NULL
      kitDesign::kit_dt(x, page_length = 25L, search = TRUE, wrap_cols = "note")
    })

    invisible(list(data_info = data_info, result = result))
  })
}
