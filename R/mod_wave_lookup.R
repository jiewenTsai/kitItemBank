# R/mod_wave_lookup.R — 功能 3：跨波次變項對照表
#
# 以功能 2（mod_filter）篩出的題目集合為輸入，
# 呈現 item × wave 的打勾矩陣，並提供 .xlsx 下載（帶時間戳）。

waveLookupUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "alert alert-info py-2 mb-3",
      icon("circle-info"), " ",
      "本頁為", tags$strong("進階預覽"), "：以「",
      tags$strong("2｜進階預覽 \u2192 題項篩選"), "」的結果為範圍。",
      "請先到篩選頁設定條件，再回到本頁查看對照表。"
    ),

    fluidRow(
      column(
        width = 12,
        div(
          class = "d-flex align-items-center mb-2 gap-2",
          h6(textOutput(ns("crosswalk_count"), inline = TRUE), class = "mb-0 me-auto"),
          downloadButton(
            ns("dl_xlsx"),
            label = "下載 .xlsx",
            icon  = icon("file-excel"),
            class = "btn-sm btn-outline-success"
          )
        ),
        div(
          class = "kit-table-scroll",
          DT::dataTableOutput(ns("crosswalk_dt"))
        )
      )
    )
  )
}

#' @param id               namespace id
#' @param data_rv          reactiveVal（四張表 list）
#' @param filtered_items_r reactive，回傳 filtered_items data.frame
#' @param domain_rv        reactive，目前 domain 名稱
waveLookupServer <- function(id, data_rv, filtered_items_r, domain_rv) {
  moduleServer(id, function(input, output, session) {

    # ── 建立對照表 ───────────────────────────────────────────────
    crosswalk_df <- reactive({
      req(data_rv())
      fi <- filtered_items_r()                  # 篩選後的 itemBank 列
      iw <- data_rv()$itemWave
      ib <- data_rv()$itemBank

      if (nrow(fi) == 0L) {
        return(data.frame(message = "（篩選結果為空，請調整條件）"))
      }

      # 只保留篩選後的題目
      wave_sub <- dplyr::semi_join(
        iw, fi[, c("itemID", "itemVer"), drop = FALSE],
        by = c("itemID", "itemVer")
      )

      wave_cols <- get_wave_cols(iw)

      # 0/1 → ✓ / 空白
      for (col in wave_cols) {
        wave_sub[[col]] <- dplyr::case_when(
          wave_sub[[col]] == 1L ~ "\u2713",
          TRUE                  ~ ""
        )
      }

      # 加 label + block（截斷處理）
      label_df <- ib[, c("itemID", "itemVer", "block", "label")]
      label_df$label <- collapse_newlines(label_df$label)

      result <- dplyr::left_join(
        label_df, wave_sub,
        by = c("itemID", "itemVer")
      )

      # 波次欄依月齡由小到大排列
      result <- result[, c("itemID", "itemVer", "block", "label", wave_cols), drop = FALSE]

      # 欄名去除內嵌換行（只修改波次欄，itemID/itemVer/block/label 不動）
      old_names <- names(result)
      new_names <- old_names
      wave_idx  <- which(old_names %in% wave_cols)
      new_names[wave_idx] <- col_display_labels(old_names[wave_idx])
      names(result) <- new_names

      result
    })

    # ── 顯示筆數 ────────────────────────────────────────────────
    output$crosswalk_count <- renderText({
      df <- crosswalk_df()
      if ("message" %in% names(df)) return(df$message[1])
      paste0("共 ", nrow(df), " 題（跨 ",
             length(get_wave_cols(data_rv()$itemWave)), " 個波次）")
    })

    # ── DT ──────────────────────────────────────────────────────
    output$crosswalk_dt <- DT::renderDataTable({
      df <- crosswalk_df()
      if ("message" %in% names(df)) {
        return(DT::datatable(df, rownames = FALSE,
          options = list(dom = "t")))
      }

      kit_render_crosswalk_dt(df, page_length = 25L)
    })

    # ── 下載 .xlsx ───────────────────────────────────────────────
    output$dl_xlsx <- downloadHandler(
      filename = function() {
        paste0(
          "\u8de8\u6ce2\u6b21\u5c0d\u7167\u8868",  # 跨波次對照表
          "_", domain_rv(),
          "_", Sys.Date(),
          ".xlsx"
        )
      },
      content = function(file) {
        df <- crosswalk_df()
        if ("message" %in% names(df)) {
          writexl::write_xlsx(data.frame(note = "篩選結果為空"), file)
        } else {
          writexl::write_xlsx(df, file)
        }
      }
    )
  })
}
