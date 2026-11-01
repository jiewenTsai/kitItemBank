# R/page_merge.R — 併入新施測：上傳新施測的 .sav（只讀表頭），看提案，下載 Excel
#
# 由上往下：這一頁做什麼 → 條件（左 1/3：新施測代號、上傳資料檔、產生提案）｜右 2/3：
#   還沒產生：提示
#   產生了：「M03M060 的併入提案（n 列）」→ 各類幾列 → 下載 Excel → 提案表
# → ▸ 用 R 怎麼做
# 只讀 .sav 表頭（sav_header_meta()），讀完就刪掉上傳的暫存檔；app 不存任何檔案。
# 真正寫進題庫由維護者用 apply_admin_merge()。計算都在 R/admin_merge.R。

# 原文：已有 | 新版本 | 換題號 | 新題 | 不是題目 | 少問或停用 | 遺漏代碼方案
AM_CLASS_LABEL <- c(same = "\u5df2\u6709", new_version = "\u65b0\u7248\u672c", renamed = "\u63db\u984c\u865f", new_item = "\u65b0\u984c",
                    # 原文：不是題目 | 少問或停用
                    not_item = "\u4e0d\u662f\u984c\u76ee", dropped_or_block = "\u5c11\u554f\u6216\u505c\u7528",
                    # 原文：遺漏代碼方案
                    missing_scheme_change = "\u907a\u6f0f\u4ee3\u78bc\u65b9\u6848")
# 原文：高 | 中 | 低
AM_CONF_LABEL <- c(high = "\u9ad8", medium = "\u4e2d", low = "\u4f4e")

#' 上傳的檔名 → 新施測的 datasetID
#'
#' 檔名（去掉副檔名和 _meta）是 "M03M060-P" 這種代號就用它的後綴；不然對同世代以前用過的
#' 檔名（data1 → -P、data2 → -PK、data3 → -T、IRT → -S），取最近一次。
#' @return 與 stems 等長；對不到為 NA
#' @noRd
merge_guess_dataset_ids <- function(kit, admin_row, stems) {
  stems <- sub("_meta$", "", sub("\\.(sav|csv)$", "", basename(stems), ignore.case = TRUE),
               ignore.case = TRUE)
  ds <- kit$datasets
  ord <- rel_admin_order(kit)
  ds$ord <- unname(ord[ds$adminID])
  ds$coh <- kit$admins$cohortID[match(ds$adminID, kit$admins$adminID)]
  ds <- ds[order(ds$coh != admin_row$cohortID, -ds$ord), , drop = FALSE]
  vapply(stems, function(s) {
    if (grepl("-(P|PK|T|S|C)$", s)) return(paste0(admin_row$adminID, "-", am_suffix(s)))
    hit <- which(tolower(ds$file) == tolower(s))
    if (!length(hit)) return(NA_character_)
    paste0(admin_row$adminID, "-", am_suffix(ds$datasetID[hit[1]]))
  }, character(1), USE.NAMES = FALSE)
}

#' 新施測每個資料檔要和哪一個資料檔比（上一次）
#' @return data.frame(datasetID, prev, prev_file)；認不出施測或檔名時回傳 NULL
#' @noRd
merge_prev_plan <- function(kit, admin_id, new_names) {
  if (!nzchar(admin_id) || !length(new_names)) return(NULL)
  row <- tryCatch(am_admin_row(kit, admin_id), error = function(e) NULL)
  if (is.null(row)) return(NULL)
  ids <- merge_guess_dataset_ids(kit, row, new_names)
  ids <- unique(ids[!is.na(ids)])
  if (!length(ids)) return(NULL)
  ds <- am_dataset_rows(kit, ids, row)
  data.frame(datasetID = ds$datasetID, prev = ds$prev,
             prev_file = kit$datasets$file[match(ds$prev, kit$datasets$datasetID)],
             stringsAsFactors = FALSE)
}

#' 上一次的檔名 → 上一次的 datasetID（檔名是代號，或是上一次的檔名 data1、data3…）
#' @return 與 names 等長；對不到為 NA
#' @noRd
merge_match_prev <- function(plan, names) {
  if (!length(names)) return(character(0))
  stems <- sub("_meta$", "", sub("\\.(sav|csv)$", "", basename(names), ignore.case = TRUE),
               ignore.case = TRUE)
  p <- plan[nzchar(plan$prev), , drop = FALSE]
  vapply(stems, function(s) {
    k <- which(tolower(p$prev) == tolower(s))
    if (!length(k)) k <- which(tolower(p$prev_file) == tolower(s))
    if (length(k)) p$prev[k[1]] else NA_character_
  }, character(1), USE.NAMES = FALSE)
}

#' 讀一個上傳的檔的表頭，讀完就刪掉暫存檔
#' @noRd
merge_read_upload <- function(path, name) {
  on.exit(unlink(path), add = TRUE)
  if (grepl("\\.sav$", name, ignore.case = TRUE)) sav_header_meta(path) else read_sav_meta(path)
}

#' Merge page UI (new administration)
#'
#' @param id Module id.
#' @return The page UI.
#' @export
page_merge_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kitDesign::kit_tab_guide(
      # 原文：把新一次施測併進題庫之前，先看每個變項該怎麼進：已有的題、新版本、換題號、新題，和這次沒問的題。
      goal = "\u628a\u65b0\u4e00\u6b21\u65bd\u6e2c\u4f75\u9032\u984c\u5eab\u4e4b\u524d\uff0c\u5148\u770b\u6bcf\u500b\u8b8a\u9805\u8a72\u600e\u9ebc\u9032\uff1a\u5df2\u6709\u7684\u984c\u3001\u65b0\u7248\u672c\u3001\u63db\u984c\u865f\u3001\u65b0\u984c\uff0c\u548c\u9019\u6b21\u6c92\u554f\u7684\u984c\u3002",
      steps = c(
        # 原文：在左邊寫新施測的代號（例如 M03M060、M36M096G02）。
        "\u5728\u5de6\u908a\u5beb\u65b0\u65bd\u6e2c\u7684\u4ee3\u865f\uff08\u4f8b\u5982 M03M060\u3001M36M096G02\uff09\u3002",
        # 原文：上傳這次施測的資料檔（.sav，或先存好的 _meta.csv）。檔名用 data1、data2、data3、IRT，或直接用 M03M060-P 這種代號。
        "\u4e0a\u50b3\u9019\u6b21\u65bd\u6e2c\u7684\u8cc7\u6599\u6a94\uff08.sav\uff0c\u6216\u5148\u5b58\u597d\u7684 _meta.csv\uff09\u3002\u6a94\u540d\u7528 data1\u3001data2\u3001data3\u3001IRT\uff0c\u6216\u76f4\u63a5\u7528 M03M060-P \u9019\u7a2e\u4ee3\u865f\u3002",
        # 原文：再上傳上一次施測的同一些資料檔（左邊會列出要哪幾個）：版本用 .sav 和 .sav 比才準。
        "\u518d\u4e0a\u50b3\u4e0a\u4e00\u6b21\u65bd\u6e2c\u7684\u540c\u4e00\u4e9b\u8cc7\u6599\u6a94\uff08\u5de6\u908a\u6703\u5217\u51fa\u8981\u54ea\u5e7e\u500b\uff09\uff1a\u7248\u672c\u7528 .sav \u548c .sav \u6bd4\u624d\u6e96\u3002",
        # 原文：按「產生提案」：右邊列出每個變項的提案、信心和理由。
        "\u6309\u300c\u7522\u751f\u63d0\u6848\u300d\uff1a\u53f3\u908a\u5217\u51fa\u6bcf\u500b\u8b8a\u9805\u7684\u63d0\u6848\u3001\u4fe1\u5fc3\u548c\u7406\u7531\u3002",
        # 原文：按「下載併入提案」存成 Excel，看過以後交給題庫維護者。
        "\u6309\u300c\u4e0b\u8f09\u4f75\u5165\u63d0\u6848\u300d\u5b58\u6210 Excel\uff0c\u770b\u904e\u4ee5\u5f8c\u4ea4\u7d66\u984c\u5eab\u7dad\u8b77\u8005\u3002"
      ),
      # 原文：併入提案清單（Excel，第一張是說明）。
      output = "\u4f75\u5165\u63d0\u6848\u6e05\u55ae\uff08Excel\uff0c\u7b2c\u4e00\u5f35\u662f\u8aaa\u660e\uff09\u3002",
      # 原文：只讀 .sav 的表頭（變項名、標籤、值標籤），不讀任何人的回答；上傳的檔讀完馬上刪掉，app 不存任何檔案。真正寫進題庫要由維護者用 apply_admin_merge()。
      note = "\u53ea\u8b80 .sav \u7684\u8868\u982d\uff08\u8b8a\u9805\u540d\u3001\u6a19\u7c64\u3001\u503c\u6a19\u7c64\uff09\uff0c\u4e0d\u8b80\u4efb\u4f55\u4eba\u7684\u56de\u7b54\uff1b\u4e0a\u50b3\u7684\u6a94\u8b80\u5b8c\u99ac\u4e0a\u522a\u6389\uff0capp \u4e0d\u5b58\u4efb\u4f55\u6a94\u6848\u3002\u771f\u6b63\u5beb\u9032\u984c\u5eab\u8981\u7531\u7dad\u8b77\u8005\u7528 apply_admin_merge()\u3002"
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        # 原文：新施測的代號 | 例如 M03M060
        shiny::textInput(ns("admin"), "\u65b0\u65bd\u6e2c\u7684\u4ee3\u865f", placeholder = "\u4f8b\u5982 M03M060", width = "100%"),
        # 原文：資料檔（.sav 或 _meta.csv，可以多個）
        shiny::fileInput(ns("files"), "\u8cc7\u6599\u6a94\uff08.sav \u6216 _meta.csv\uff0c\u53ef\u4ee5\u591a\u500b\uff09", multiple = TRUE,
                         accept = c(".sav", ".csv"), width = "100%",
                         # 原文：選檔案… | 還沒選
                         buttonLabel = "\u9078\u6a94\u6848\u2026", placeholder = "\u9084\u6c92\u9078"),
        shiny::uiOutput(ns("files_ui")),
        # 原文：上一次施測的資料檔（建議）
        shiny::fileInput(ns("prev_files"), "\u4e0a\u4e00\u6b21\u65bd\u6e2c\u7684\u8cc7\u6599\u6a94\uff08\u5efa\u8b70\uff09", multiple = TRUE,
                         accept = c(".sav", ".csv"), width = "100%",
                         # 原文：選檔案… | 還沒選
                         buttonLabel = "\u9078\u6a94\u6848\u2026", placeholder = "\u9084\u6c92\u9078"),
        shiny::uiOutput(ns("prev_ui")),
        # 原文：產生提案
        shiny::div(class = "kit-buttons", kitDesign::kit_button(ns("run"), "\u7522\u751f\u63d0\u6848", kind = "primary"))
      ),
      shiny::column(8, shiny::uiOutput(ns("result")))
    ),
    kitDesign::kit_details_row(kitDesign::kit_code_panel_ui(ns("code")))
  )
}

#' Merge page server (new administration)
#'
#' @param id Module id.
#' @param kit The kit (17 tables): a reactive, or a plain list.
#' @param goto Switch to another tab (`goto("search", item = ..., uid = ...)`).
#' @param request Optional reactive with requests from other tabs (unused for now).
#' @return Invisibly, a list of reactives (`headers`, `proposals`).
#' @export
page_merge_server <- function(id, kit, goto, request = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    kit_now <- if (shiny::is.reactive(kit)) kit else function() kit
    headers <- shiny::reactiveVal(list())    # 檔名 → 表頭（只有表頭）
    proposals <- shiny::reactiveVal(NULL)

    prev_headers <- shiny::reactiveVal(list())
    read_all <- function(f) {
      out <- list()
      for (i in seq_len(nrow(f))) {
        m <- tryCatch(merge_read_upload(f$datapath[i], f$name[i]), error = function(e) e)
        if (inherits(m, "error")) {
          # 原文：「 | 」讀不到表頭：
          kitDesign::kit_notify(paste0("\u300c", f$name[i], "\u300d\u8b80\u4e0d\u5230\u8868\u982d\uff1a", conditionMessage(m)), session)
          next
        }
        out[[f$name[i]]] <- m
      }
      out
    }
    shiny::observeEvent(input$files, {
      headers(read_all(input$files))
      proposals(NULL)
    })
    shiny::observeEvent(input$prev_files, {
      prev_headers(read_all(input$prev_files))
      proposals(NULL)
    })
    plan <- shiny::reactive(merge_prev_plan(kit_now(), trimws(input$admin %||% ""), names(headers())))

    output$prev_ui <- shiny::renderUI({
      pl <- plan()
      if (is.null(pl)) {
        # 原文：寫好代號、上傳這次的資料檔後，這裡會列出要上傳上一次的哪幾個檔。
        return(shiny::p(class = "kit-hint", "\u5beb\u597d\u4ee3\u865f\u3001\u4e0a\u50b3\u9019\u6b21\u7684\u8cc7\u6599\u6a94\u5f8c\uff0c\u9019\u88e1\u6703\u5217\u51fa\u8981\u4e0a\u50b3\u4e0a\u4e00\u6b21\u7684\u54ea\u5e7e\u500b\u6a94\u3002"))
      }
      got <- merge_match_prev(pl, names(prev_headers()))
      pl <- pl[nzchar(pl$prev), , drop = FALSE]
      # 原文：題庫裡沒有上一次的同一種資料檔，只能和題庫的文字比。
      if (!nrow(pl)) return(shiny::p(class = "kit-hint", "\u984c\u5eab\u88e1\u6c92\u6709\u4e0a\u4e00\u6b21\u7684\u540c\u4e00\u7a2e\u8cc7\u6599\u6a94\uff0c\u53ea\u80fd\u548c\u984c\u5eab\u7684\u6587\u5b57\u6bd4\u3002"))
      shiny::tagList(
        # 原文：要和上一次的這些資料檔比：
        shiny::p(class = "kit-hint", "\u8981\u548c\u4e0a\u4e00\u6b21\u7684\u9019\u4e9b\u8cc7\u6599\u6a94\u6bd4\uff1a"),
        shiny::tags$ul(lapply(seq_len(nrow(pl)), function(i) shiny::tags$li(paste0(
          # 原文：（ | ）：
          pl$prev[i], "\uff08", pl$prev_file[i], "\uff09\uff1a",
          # 原文：已上傳 | 還沒上傳，會改和題庫的文字比（比較不準）
          if (pl$prev[i] %in% got) "\u5df2\u4e0a\u50b3" else "\u9084\u6c92\u4e0a\u50b3\uff0c\u6703\u6539\u548c\u984c\u5eab\u7684\u6587\u5b57\u6bd4\uff08\u6bd4\u8f03\u4e0d\u6e96\uff09"))))
      )
    })

    output$files_ui <- shiny::renderUI({
      h <- headers()
      # 原文：檔案太大傳不上去時，先在自己電腦用 kitItemBank::sav_header_meta() 存成 _meta.csv 再上傳。
      if (!length(h)) return(shiny::p(class = "kit-hint", "\u6a94\u6848\u592a\u5927\u50b3\u4e0d\u4e0a\u53bb\u6642\uff0c\u5148\u5728\u81ea\u5df1\u96fb\u8166\u7528 kitItemBank::sav_header_meta() \u5b58\u6210 _meta.csv \u518d\u4e0a\u50b3\u3002"))
      shiny::tags$ul(lapply(names(h), function(n) shiny::tags$li(paste0(
        # 原文：： |  個變項
        n, "\uff1a", kitDesign::kit_num(nrow(h[[n]])), " \u500b\u8b8a\u9805"))))
    })

    shiny::observeEvent(input$run, {
      h <- headers()
      a <- trimws(input$admin %||% "")
      if (!nzchar(a)) {
        # 原文：先寫新施測的代號（例如 M03M060）。
        kitDesign::kit_notify("\u5148\u5beb\u65b0\u65bd\u6e2c\u7684\u4ee3\u865f\uff08\u4f8b\u5982 M03M060\uff09\u3002", session)
        return()
      }
      if (!length(h)) {
        # 原文：先上傳這次施測的資料檔。
        kitDesign::kit_notify("\u5148\u4e0a\u50b3\u9019\u6b21\u65bd\u6e2c\u7684\u8cc7\u6599\u6a94\u3002", session)
        return()
      }
      k <- kit_now()
      res <- tryCatch({
        row <- am_admin_row(k, a)
        ids <- merge_guess_dataset_ids(k, row, names(h))
        if (anyNA(ids)) {
          # 原文：認不出這些檔是哪一個資料檔： | 、
          stop(paste0("\u8a8d\u4e0d\u51fa\u9019\u4e9b\u6a94\u662f\u54ea\u4e00\u500b\u8cc7\u6599\u6a94\uff1a", paste(names(h)[is.na(ids)], collapse = "\u3001"),
                      # 原文：。請把檔名改成 data1、data2、data3、IRT，或  | -P 這種代號。
                      "\u3002\u8acb\u628a\u6a94\u540d\u6539\u6210 data1\u3001data2\u3001data3\u3001IRT\uff0c\u6216 ", a, "-P \u9019\u7a2e\u4ee3\u865f\u3002"),
               call. = FALSE)
        }
        # 原文：兩個檔對到同一個資料檔： | 、
        if (anyDuplicated(ids)) stop("\u5169\u500b\u6a94\u5c0d\u5230\u540c\u4e00\u500b\u8cc7\u6599\u6a94\uff1a", paste(unique(ids[duplicated(ids)]), collapse = "\u3001"),
                                     call. = FALSE)
        ph <- prev_headers()
        pm <- NULL
        if (length(ph)) {
          pl <- merge_prev_plan(k, a, names(h))
          pid <- if (is.null(pl)) rep(NA_character_, length(ph)) else merge_match_prev(pl, names(ph))
          if (anyNA(pid)) {
            # 原文：上一次的這些檔用不到（認不出是哪一個資料檔）：
            kitDesign::kit_notify(paste0("\u4e0a\u4e00\u6b21\u7684\u9019\u4e9b\u6a94\u7528\u4e0d\u5230\uff08\u8a8d\u4e0d\u51fa\u662f\u54ea\u4e00\u500b\u8cc7\u6599\u6a94\uff09\uff1a",
                                         # 原文：、
                                         paste(names(ph)[is.na(pid)], collapse = "\u3001")), session)
          }
          pm <- stats::setNames(ph[!is.na(pid)], pid[!is.na(pid)])
          pm <- pm[!duplicated(names(pm))]
        }
        propose_admin_merge(k, stats::setNames(h, ids), row, prev_meta = pm)
      }, error = function(e) e)
      if (inherits(res, "error")) {
        kitDesign::kit_notify(conditionMessage(res), session)
        proposals(NULL)
        return()
      }
      proposals(res)
    })

    output$result <- shiny::renderUI({
      p <- proposals()
      if (is.null(p)) {
        return(shiny::tagList(
          # 原文：還沒有提案
          shiny::h4("\u9084\u6c92\u6709\u63d0\u6848"),
          # 原文：在左邊寫新施測的代號、上傳資料檔，按「產生提案」。
          shiny::p(class = "kit-hint", "\u5728\u5de6\u908a\u5beb\u65b0\u65bd\u6e2c\u7684\u4ee3\u865f\u3001\u4e0a\u50b3\u8cc7\u6599\u6a94\uff0c\u6309\u300c\u7522\u751f\u63d0\u6848\u300d\u3002")
        ))
      }
      ad <- attr(p, "admin")
      ds <- attr(p, "datasets")
      s <- admin_merge_summary(p)
      s <- s[s$total > 0L, , drop = FALSE]
      shiny::tagList(
        # 原文： 的併入提案（ |  列）
        shiny::h4(paste0(ad$adminID, " \u7684\u4f75\u5165\u63d0\u6848\uff08", kitDesign::kit_num(nrow(p)), " \u5217\uff09")),
        shiny::tags$ul(lapply(seq_len(nrow(ds)), function(i) shiny::tags$li(paste0(
          # 原文：：和上一次的  | （沒有） |  比
          ds$datasetID[i], "\uff1a\u548c\u4e0a\u4e00\u6b21\u7684 ", if (nzchar(ds$prev[i])) ds$prev[i] else "\uff08\u6c92\u6709\uff09", " \u6bd4",
          # 原文：（用 .sav 表頭） | （沒有表頭，和題庫文字比，比較不準）
          if (identical(ds$compared_with[i], "header")) "\uff08\u7528 .sav \u8868\u982d\uff09" else "\uff08\u6c92\u6709\u8868\u982d\uff0c\u548c\u984c\u5eab\u6587\u5b57\u6bd4\uff0c\u6bd4\u8f03\u4e0d\u6e96\uff09")))),
        shiny::tags$ul(lapply(seq_len(nrow(s)), function(i) shiny::tags$li(paste0(
          # 原文：： |  列
          AM_CLASS_LABEL[[s$class[i]]], "\uff1a", kitDesign::kit_num(s$total[i]), " \u5217",
          # 原文：（其中  |  列信心低，一定要看）
          if (s$low[i] > 0L) paste0("\uff08\u5176\u4e2d ", kitDesign::kit_num(s$low[i]), " \u5217\u4fe1\u5fc3\u4f4e\uff0c\u4e00\u5b9a\u8981\u770b\uff09") else "")))),
        shiny::div(
          class = "kit-buttons",
          kitDesign::kit_download_button(
            # 原文：下載併入提案（Excel）
            session, "dl_xlsx", "\u4e0b\u8f09\u4f75\u5165\u63d0\u6848\uff08Excel\uff09",
            # 原文：併入提案_
            paste0("\u4f75\u5165\u63d0\u6848_", ad$adminID, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
            function(file) writexl::write_xlsx(admin_merge_workbook(p), file),
            class = kitDesign::kit_btn_class("secondary")
          )
        ),
        # 原文：提案表
        shiny::h4("\u63d0\u6848\u8868"),
        # 原文：「已有」的題目不用動；先看信心低的列。
        shiny::p(class = "kit-hint", "\u300c\u5df2\u6709\u300d\u7684\u984c\u76ee\u4e0d\u7528\u52d5\uff1b\u5148\u770b\u4fe1\u5fc3\u4f4e\u7684\u5217\u3002"),
        kitDesign::kit_table_scroll(DT::DTOutput(ns("prop_dt")))
      )
    })

    output$prop_dt <- DT::renderDT({
      p <- proposals()
      shiny::req(p)
      # 信心低的排前面，「已有」排最後
      o <- order(p$class == "same", match(p$confidence, c("low", "medium", "high")),
                 p$datasetID, suppressWarnings(as.integer(p$order)))
      p <- p[o, , drop = FALSE]
      df <- data.frame(unname(AM_CLASS_LABEL[p$class]), unname(AM_CONF_LABEL[p$confidence]),
                       p$datasetID, ifelse(nzchar(p$varname), p$varname, p$block),
                       p$item_uid, p$reason, stringsAsFactors = FALSE)
      # 原文：類別 | 信心 | 資料檔 | 變項 | 題號 | 理由
      names(df) <- c("\u985e\u5225", "\u4fe1\u5fc3", "\u8cc7\u6599\u6a94", "\u8b8a\u9805", "\u984c\u865f", "\u7406\u7531")
      # 原文：理由
      kitDesign::kit_dt(df, page_length = 25L, search = TRUE, wrap_cols = "\u7406\u7531",
                        # 原文：理由
                        min_widths = c("\u7406\u7531" = "14rem"))
    })

    kitDesign::kit_code_panel_server("code", shiny::reactive({
      a <- trimws(input$admin %||% "")
      merge_code(if (nzchar(a)) a else "M03M060")
    }))

    invisible(list(headers = headers, proposals = proposals))
  })
}

# 用 R 怎麼做
merge_code <- function(admin_id) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", as.character(x), fixed = TRUE), "\"")
  paste(c(
    "kit <- kitItemBank::read_kit()",
    "",
    # 原文：# 只讀 .sav 的表頭（變項名、標籤、值標籤）
    "# \u53ea\u8b80 .sav \u7684\u8868\u982d\uff08\u8b8a\u9805\u540d\u3001\u6a19\u7c64\u3001\u503c\u6a19\u7c64\uff09",
    "meta <- list(",
    paste0("  ", q(paste0(admin_id, "-P")), " = kitItemBank::sav_header_meta(\"data1.sav\"),"),
    paste0("  ", q(paste0(admin_id, "-T")), " = kitItemBank::sav_header_meta(\"data3.sav\")"),
    ")",
    # 原文：# 上一次施測的同一些資料檔（名稱用上一次的 datasetID）：版本用 .sav 和 .sav 比才準
    "# \u4e0a\u4e00\u6b21\u65bd\u6e2c\u7684\u540c\u4e00\u4e9b\u8cc7\u6599\u6a94\uff08\u540d\u7a31\u7528\u4e0a\u4e00\u6b21\u7684 datasetID\uff09\uff1a\u7248\u672c\u7528 .sav \u548c .sav \u6bd4\u624d\u6e96",
    # 原文：prev <- list(\"<上一次>-P\" = kitItemBank::sav_header_meta(\"prev/data1.sav\"))
    "prev <- list(\"<\u4e0a\u4e00\u6b21>-P\" = kitItemBank::sav_header_meta(\"prev/data1.sav\"))",
    paste0("p <- kitItemBank::propose_admin_merge(kit, meta, ", q(admin_id), ", prev_meta = prev)"),
    "kitItemBank::admin_merge_summary(p)",
    "",
    # 原文：# 看過以後：接受全部（或只給要接受的 proposal_id），產生新的題庫，再檢查
    "# \u770b\u904e\u4ee5\u5f8c\uff1a\u63a5\u53d7\u5168\u90e8\uff08\u6216\u53ea\u7d66\u8981\u63a5\u53d7\u7684 proposal_id\uff09\uff0c\u7522\u751f\u65b0\u7684\u984c\u5eab\uff0c\u518d\u6aa2\u67e5",
    "kit2 <- kitItemBank::apply_admin_merge(kit, p, accept = TRUE)",
    "kitItemBank::check_kit(kit2)",
    "# kitItemBank::write_kit(kit2, \"inst/extdata/kit\")"
  ), collapse = "\n")
}
