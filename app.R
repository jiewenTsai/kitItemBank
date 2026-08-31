# app.R — KIT 題庫管理系統
#
# 兩層結構：
#   頂層：Navbar，右上角 domain 切換
#   功能頁：四個 nav_panel，分別對應規格書 4.1–4.4

# ── 套件 ──────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(writexl)
  library(officer)
  library(flextable)
  library(jsonlite)
  if (requireNamespace("visNetwork", quietly = TRUE)) library(visNetwork)
  if (requireNamespace("readxl", quietly = TRUE)) library(readxl)
})

# ── 本地模組 ──────────────────────────────────────────────────────
source("kit_theme.R")
source("R/utils.R")
source("R/data_io.R")
source("R/utils_matrix_viz.R")
source("R/mod_edit_matrix.R")
source("R/mod_filter.R")
source("R/mod_wave_lookup.R")
source("R/mod_dim_report.R")
source("R/utils_docx.R")
source("R/utils_html.R")
source("R/utils_assemble_io.R")
source("R/utils_dim_report_docx.R")
source("R/mod_assemble.R")
source("R/mod_domain_io.R")

# ── Domain 設定 ───────────────────────────────────────────────────
DOMAIN_CHOICES <- c(
  "家長（Parent）"                      = "parent",
  "兒童青少年（Child / Adolescent）"    = "child_adolescent",
  "教保人員（Childcare Teacher）"       = "childcare_teacher"
)

# ── UI ────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = div(
    class = "kit-navbar-brand",
    div(
      class = "kit-navbar-brand-titles",
      tags$span(
        class = "kit-brand-kitid",
        "kitItemBank"
      ),
      tags$span(
        class = "kit-brand-title",
        "KIT 題庫管理工具"
      )
    ),
    kit_navbar_meta()
  ),
  theme  = kit_theme(),
  window_title = "kitItemBank KIT 題庫管理工具",

  # 共用 CSS
  header = tagList(
    tags$head(
      tags$style(kit_css()),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('kit_download_base64', function(msg) {
          var raw = window.atob(msg.b64);
          var arr = new Uint8Array(raw.length);
          for (var i = 0; i < raw.length; i++) arr[i] = raw.charCodeAt(i);
          var blob = new Blob([arr], {type: msg.mime});
          var url = URL.createObjectURL(blob);
          var a = document.createElement('a');
          a.href = url;
          a.download = msg.filename;
          document.body.appendChild(a);
          a.click();
          document.body.removeChild(a);
          URL.revokeObjectURL(url);
        });
      ")),
      tags$style(HTML("
        .navbar .form-select { font-size:0.85rem; padding:0.2rem 0.6rem; }
        .nav-item.domain-selector { margin-top: auto; margin-bottom: auto; }
        /* 橫列 navbar：title 與分頁選單拉開間距 */
        .navbar .kit-navbar-brand {
          display: flex;
          flex-direction: column;
          align-items: flex-start;
          gap: 0.1rem;
          margin-right: 2.75rem !important;
          padding-right: 0.5rem;
          white-space: nowrap;
        }
        .navbar .kit-navbar-brand-titles {
          display: flex;
          align-items: baseline;
          gap: 0.65rem;
        }
        .navbar .kit-brand-kitid {
          font-weight: 600;
          font-size: 0.92rem;
          letter-spacing: 0.02em;
          opacity: 0.92;
        }
        .navbar .kit-brand-title {
          font-weight: 500;
          letter-spacing: 0.03em;
        }
        .navbar .navbar-collapse .navbar-nav {
          margin-left: 0.25rem;
        }
        @media (min-width: 992px) {
          .navbar .kit-navbar-brand { margin-right: 3.25rem !important; }
        }
        details > summary { list-style: none; }
        details > summary::-webkit-details-marker { display: none; }
        details > summary::before { content: '\\25b6\\2002'; font-size:0.75em; }
        details[open] > summary::before { content: '\\25bc\\2002'; }
        .kit-outline-block { border: 1px solid #dee2e6; border-radius: 6px; padding: 10px 12px; background: #fafafa; }
        .kit-html-preview { border: 1px solid #dee2e6; border-radius: 6px; padding: 12px; max-height: 480px; overflow: auto; background: #fff; }
        .kit-html-preview h2 { font-size: 1.05rem; color: #1a5276; margin-top: 1rem; }
        .kit-html-preview .kit-q-table { border-collapse: collapse; width: 100%; font-size: var(--kit-table-font, 0.72rem); margin: 8px 0; }
        .kit-html-preview .kit-q-table th, .kit-html-preview .kit-q-table td { border: 1px solid #333; padding: 4px 6px; }
        .kit-html-preview .kit-open-box { border: 1px solid #333; min-height: 40px; padding: 8px; color: #888; margin: 6px 0; }
        .kit-html-preview .kit-scale-opt { margin-right: 10px; }
        .kit-preview-pane { min-height: calc(100vh - 220px); max-height: calc(100vh - 220px); overflow: auto; }
        .kit-preview-pane .kit-html-preview { max-height: none; min-height: 480px; border: 1px solid #dee2e6; border-radius: 6px; padding: 12px; background: #fff; }
        .kit-likert-help { font-size: 0.85rem; }
        .kit-likert-help ol { margin-bottom: 0.5rem; padding-left: 1.2rem; }
        /* 預覽區統一字級 */
        :root { --kit-table-font: 0.72rem; }
        .kit-preview-hint,
        .kit-dim-preview,
        .kit-dim-preview details,
        .kit-dim-preview summary,
        .kit-dim-preview .table,
        .kit-dim-preview .kit-badge,
        table.kit-items-dt,
        table.dataTable.kit-items-dt,
        table.dataTable.kit-items-dt td,
        table.dataTable.kit-items-dt th,
        .kit-items-dt td,
        .kit-items-dt th {
          font-size: var(--kit-table-font) !important;
          line-height: 1.35;
        }
        .kit-dim-block {
          border: 1px solid #e0e0e0;
          border-radius: 4px;
          padding: 0.4rem 0.7rem;
        }
        .kit-dim-block summary { cursor: pointer; }
        .kit-dim-title { font-weight: 600; }
        .kit-dim-sub { color: #666; margin-left: 0.35rem; }
        .kit-badge {
          display: inline-block;
          border-radius: 4px;
          padding: 0.1rem 0.4rem;
          margin-left: 0.35rem;
          font-size: var(--kit-table-font) !important;
        }
        .kit-badge-rel { background: #e9ecef; color: #333; }
        .kit-badge-alias { background: #cff4fc; color: #055160; }
        .kit-badge-count { background: #f8f9fa; color: #333; border: 1px solid #dee2e6; }
        .kit-preview-note { color: #555; }
        .kit-network-pane {
          border: 1px solid #dee2e6;
          border-radius: 6px;
          background: #fafafa;
          min-height: 620px;
        }
        table.kit-items-dt .dt-id-col,
        table.kit-items-dt .dt-ver-col,
        table.kit-items-dt .dt-block-col,
        table.kit-items-dt .dt-label-col {
          font-size: var(--kit-table-font) !important;
          line-height: 1.35;
        }
        table.kit-items-dt .dt-id-col,
        table.kit-items-dt .dt-ver-col,
        table.kit-items-dt .dt-block-col { color: #666; white-space: nowrap; }
        table.kit-items-dt .dt-ver-col { text-align: center; }
        table.kit-items-dt .dt-label-col { white-space: normal; word-break: break-word; color: #333; }
        /* 跨波次對照表：固定欄寬 1:1:1:6 + 窄波次欄（單表，不用 scrollX） */
        .kit-table-scroll .kit-crosswalk-dt {
          table-layout: fixed !important;
          width: max-content !important;
        }
        .kit-table-scroll .kit-crosswalk-dt th,
        .kit-table-scroll .kit-crosswalk-dt td {
          box-sizing: border-box;
        }
        .kit-table-scroll .kit-crosswalk-dt .dt-id-col {
          width: 80px !important; min-width: 80px !important; max-width: 80px !important;
        }
        .kit-table-scroll .kit-crosswalk-dt .dt-ver-col {
          width: 80px !important; min-width: 80px !important; max-width: 80px !important;
        }
        .kit-table-scroll .kit-crosswalk-dt .dt-block-col {
          width: 80px !important; min-width: 80px !important; max-width: 80px !important;
          overflow: hidden; text-overflow: ellipsis;
        }
        .kit-table-scroll .kit-crosswalk-dt .dt-label-col {
          width: 480px !important; min-width: 480px !important; max-width: 480px !important;
          white-space: normal !important; word-break: break-word !important;
        }
        .kit-table-scroll .kit-crosswalk-dt .dt-wave-col {
          width: 24px !important; min-width: 24px !important; max-width: 24px !important;
          padding: 2px 1px !important; text-align: center !important;
          overflow: hidden; white-space: nowrap;
        }
        .kit-table-scroll .kit-crosswalk-dt th.dt-wave-col {
          writing-mode: vertical-rl;
          text-orientation: mixed;
          height: 88px;
          vertical-align: bottom;
          font-size: 0.62rem !important;
          line-height: 1.1;
          overflow: hidden;
        }
        table.kit-items-dt .dt-wave-col { width: 24px !important; padding: 2px 1px !important; text-align: center; }
        .kit-q-table-noborder th, .kit-q-table-noborder td { border: none !important; }
      "))
    )
  ),

  # ── 頁面 1：構面 / 波次編修 ──────────────────────────────────
  nav_panel(
    title = tagList(icon("table-cells"), " 1｜編修"),
    value = "tab_edit",
    div(
      class = "kit-header mb-4",
      tags$h1("構面 / 波次編修"),
      tags$p(
        tags$span(class = "kit-subtitle", "臺灣幼兒發展調查資料庫"),
        " · 每個分頁含「預覽模式」與「編修模式」；下方可匯出／匯入四表 Excel 總匯"
      )
    ),
    domainBundleUI("domain_io"),
    navset_tab(
      nav_panel(
        title = tagList(icon("layer-group"), " 構面（itemDim）"),
        editMatrixUI("edit_dim", matrix_label = "構面")
      ),
      nav_panel(
        title = tagList(icon("wave-square"), " 波次（itemWave）"),
        editMatrixUI("edit_wave", matrix_label = "波次")
      )
    )
  ),

  # ── 頁面 2：進階預覽（篩選 + 對照） ───────────────────────────
  nav_panel(
    title = tagList(icon("magnifying-glass-chart"), " 2｜進階預覽"),
    value = "tab_adv_preview",
    div(
      class = "kit-header mb-4",
      tags$h1("進階預覽"),
      tags$p(
        tags$span(class = "kit-subtitle", "臺灣幼兒發展調查資料庫"),
        " · 依波次/構面篩選題項，或查看跨波次對照矩陣（唯讀）"
      )
    ),
    navset_tab(
      nav_panel(
        title = tagList(icon("filter"), " 題項篩選"),
        filterUI("filter")
      ),
      nav_panel(
        title = tagList(icon("table"), " 跨波次對照表"),
        waveLookupUI("wave_lookup")
      )
    )
  ),

  # ── 頁面 3：構面報告 ─────────────────────────────────────────
  nav_panel(
    title = tagList(icon("file-lines"), " 3｜構面報告"),
    value = "tab_report",
    div(
      class = "kit-header mb-4",
      tags$h1("構面題項對應報告"),
      tags$p(
        tags$span(class = "kit-subtitle", "臺灣幼兒發展調查資料庫"),
        " · 預覽模式：依波次產生報告；編修模式：dimMeta 別名與波次信度"
      )
    ),
    domainBundleUI("domain_io_report"),
    dimReportUI("dim_report")
  ),

  # ── 頁面 4：組卷輸出 ─────────────────────────────────────────
  nav_panel(
    title = tagList(icon("file-word"), " 4｜組卷"),
    value = "tab_assemble",
    assembleUI("assemble")
  ),

  # ── 右上角：Domain 選擇器 ────────────────────────────────────
  nav_spacer(),
  nav_item(
    class = "domain-selector",
    div(
      style = "padding: 0.3rem 0;",
      selectInput(
        "domain",
        label    = NULL,
        choices  = DOMAIN_CHOICES,
        selected = "parent",
        width    = "240px"
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── 資料層：reactiveVal 持有四張表 ───────────────────────────
  domain_data <- reactiveVal(NULL)

  # domain 切換時重新載入
  observeEvent(input$domain, {
    showNotification(
      paste0("\u8f09\u5165\u8cc7\u6599：", input$domain, "\u2026"),  # 載入資料：
      id = "loading_notif", duration = NULL, type = "default"
    )
    tryCatch({
      domain_data(load_domain(input$domain))
      removeNotification("loading_notif")
      showNotification(
        paste0("\u2705 ", input$domain, " 資料載入完成"),
        type = "message", duration = 3
      )
    }, error = function(e) {
      removeNotification("loading_notif")
      showNotification(
        paste0("\u274c 載入失敗：", e$message),
        type = "error", duration = 10
      )
    })
  }, ignoreNULL = FALSE)

  # 包成 reactive 供各模組使用
  domain_rv <- reactive(input$domain)

  # ── 模組連線 ─────────────────────────────────────────────────
  domainBundleServer("domain_io", domain_data, domain_rv)
  domainBundleServer("domain_io_report", domain_data, domain_rv)
  editMatrixServer("edit_dim",  domain_data, domain_rv, matrix_type = "dim")
  editMatrixServer("edit_wave", domain_data, domain_rv, matrix_type = "wave")

  # filterServer 回傳核心 reactive，供功能 3/4 共用
  filtered_items <- filterServer("filter", domain_data)

  waveLookupServer("wave_lookup", domain_data, filtered_items, domain_rv)
  dimReportServer("dim_report",  domain_data, domain_rv)
  assembleServer("assemble", domain_data, filtered_items)
}

# ── 啟動 ──────────────────────────────────────────────────────────
shinyApp(ui, server)
