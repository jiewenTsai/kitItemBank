# R/mod_domain_io.R — 題庫四表 Excel 總匯入／匯出

DOMAIN_XLSX_SHEETS <- c("itemBank", "itemDim", "itemWave", "dimMeta")

domainBundleUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    class = "mb-3",
    bslib::card_header(icon("file-excel"), " 題庫總匯（四表 Excel）"),
    tags$p(
      class = "text-muted kit-preview-hint mb-2",
      "單一 .xlsx 含 ",
      tags$code("itemBank"), "、",
      tags$code("itemDim"), "、",
      tags$code("itemWave"), "、",
      tags$code("dimMeta"),
      " 四個工作表（itemBank / itemDim / itemWave / dimMeta）",
      "；匯入／匯出同進同出，dimMeta 編修後匯出即含最新內容。",
      "（構面信度見 dimMeta.reliability；波次專屬覆寫可選 dimReliability 工作表）"
    ),
    div(
      class = "d-flex flex-wrap align-items-end gap-2",
      downloadButton(ns("export_xlsx"), "\u532f\u51fa .xlsx",
        class = "btn-sm btn-outline-success"),
      fileInput(ns("import_xlsx"), NULL,
        buttonLabel = "\u9078\u64c7 Excel\u2026",
        accept = c(".xlsx",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
        width = "220px"),
      actionButton(ns("import_xlsx_btn"), "\u532f\u5165\u4e26\u8986\u84cb",
        class = "btn-sm btn-outline-warning")
    )
  )
}

domainBundleServer <- function(id, domain_data, domain_rv) {
  moduleServer(id, function(input, output, session) {

    output$export_xlsx <- downloadHandler(
      filename = function() {
        paste0("KIT_", domain_rv(), "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(domain_data())
        export_domain_xlsx(domain_data(), file)
      }
    )

    observeEvent(input$import_xlsx_btn, {
      req(input$import_xlsx)
      if (!requireNamespace("readxl", quietly = TRUE)) {
        showNotification(
          "請安裝 readxl：install.packages('readxl')",
          type = "error", duration = 8)
        return()
      }
      tryCatch({
        imported <- import_domain_xlsx(input$import_xlsx$datapath)
        domain  <- domain_rv()
        paths   <- save_domain_bundle(domain, imported)
        domain_data(imported)
        showNotification(
          paste0("\u2705 \u5df2\u532f\u5165\u984c\u5eab\u7e3d\u532f\uff1a",
                 paste(basename(unlist(paths)), collapse = ", ")),
          type = "message", duration = 6)
      }, error = function(e) {
        showNotification(
          paste0("\u274c \u532f\u5165\u5931\u6557\uff1a", e$message),
          type = "error", duration = 10)
      })
    })
  })
}
