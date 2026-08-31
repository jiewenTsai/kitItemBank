#' KIT Shiny 主題與版面元件
#'
#' 配色參考 KIT 資料跨波次串連平臺（https://kitwaves.hdfs.ntnu.edu.tw/）。
#' Bootswatch Brite 尚未納入 bslib，改以 Lumen 為底並覆寫 KIT 品牌色。

#' @return A [bslib::bs_theme()] object.
#' @export
kit_theme <- function() {
  bslib::bs_theme(
    bootswatch   = "lumen",
    primary      = "#2d7535",
    secondary    = "#616161",
    success      = "#2d7535",
    info         = "#2778c4",
    warning      = "#c6d033",
    danger       = "#d9534f",
    base_font    = bslib::font_google("Roboto"),
    heading_font = bslib::font_google("Roboto"),
    "navbar-bg"  = "#2d7535",
    "body-bg"    = "#f5f5f5"
  )
}

#' App 版本（每次功能更新請手動遞增，例如 0.1.0 → 0.1.1 或 0.2.0）
KIT_APP_VERSION <- "0.1.0"

#' 讀取 app.R 最後修改日期（yyyy-mm-dd）
kit_app_last_modified <- function() {
  app_path <- if (file.exists("app.R")) "app.R" else file.path(getwd(), "app.R")
  if (!file.exists(app_path)) return("unknown")
  format(file.info(app_path)$mtime, "%Y-%m-%d")
}

#' KIT 品牌色常數
#' @export
kit_colors <- function() {
  list(
    primary       = "#2d7535",
    primary_light = "#3a8f45",
    accent        = "#c6d033",
    subtitle      = "#e8f0a0",
    body_bg       = "#f5f5f5",
    text_muted    = "#424242",
    border        = "#e0e0e0",
    info          = "#2778c4",
    danger        = "#d9534f"
  )
}

#' KIT App 共用 CSS
#' @export
kit_css <- function() {
  cols <- kit_colors()
  shiny::HTML(sprintf("
    .kit-header {
      background: linear-gradient(135deg, %1$s 0%%, %2$s 100%%);
      color: #fff;
      padding: 1.25rem 1.5rem;
      border-radius: 0.5rem;
      border-bottom: 4px solid %3$s;
      box-shadow: 0 2px 8px rgba(45, 117, 53, 0.2);
    }
    .kit-header h1 {
      margin: 0;
      font-size: 1.75rem;
      font-weight: 500;
    }
    .kit-header p {
      margin: 0.35rem 0 0;
      opacity: 0.92;
      font-size: 0.95rem;
    }
    .kit-subtitle {
      color: %4$s;
      font-weight: 400;
    }
    .nav-tabs .nav-link.active {
      border-bottom: 3px solid %1$s;
      font-weight: 500;
    }
    .nav-tabs .nav-link {
      color: %5$s;
    }
    .table {
      background: #fff;
    }
    .well, pre {
      background-color: #fff;
      border: 1px solid %6$s;
    }
    .kit-table-scroll {
      overflow-x: auto;
      max-width: 100%%;
      margin-bottom: 0.5rem;
    }
    .kit-table-scroll table {
      font-size: 0.85rem;
      white-space: nowrap;
    }
    .kit-spinner {
      width: 3rem;
      height: 3rem;
      border: 0.35rem solid %6$s;
      border-top-color: %1$s;
      border-radius: 50%%;
      animation: kit-spin 0.8s linear infinite;
    }
    @keyframes kit-spin {
      to { transform: rotate(360deg); }
    }
    td.na-cell {
      background-color: #e0e0e0 !important;
      color: #888 !important;
      font-style: italic;
    }
    tr.flag-duplicate {
      background-color: #fde8e8 !important;
    }
    .rule-cond-panel {
      background: #f9f9f9;
      border: 1px solid %6$s;
      border-radius: 0.4rem;
      padding: 0.75rem 1rem;
      margin-bottom: 0.75rem;
    }
    .cond-row {
      background: #fafafa;
      border: 1px solid %6$s !important;
    }
    .cond-builder-wrap .form-group,
    .cond-builder-wrap .shiny-input-container {
      margin-bottom: 0 !important;
    }
    .well { background: #f5f5f5; border: 1px solid #e3e3e3; border-radius: 4px;
            padding: 8px 12px; margin-bottom: 8px; }
    .kit-navbar-meta {
      font-size: 0.68rem;
      color: rgba(255, 255, 255, 0.55);
      line-height: 1.2;
      font-weight: 400;
      letter-spacing: 0.02em;
    }
    ", cols$primary, cols$primary_light, cols$accent,
     cols$subtitle, cols$text_muted, cols$border))
}

#' Navbar 版本與最後修改日期
#' @export
kit_navbar_meta <- function() {
  shiny::div(
    class = "kit-navbar-meta",
    sprintf("v%s · %s", KIT_APP_VERSION, kit_app_last_modified())
  )
}

#' KIT 頁首區塊
#'
#' @param title 主標題
#' @param subtitle 副標題（顯示為淺黃綠色）
#' @param tagline 標語（接在副標題後）
#' @export
kit_header <- function(
    title    = "KIT 不合理值標記工具",
    subtitle = "臺灣幼兒發展調查資料庫",
    tagline  = "不合理值標記") {
  shiny::div(
    class = "kit-header mb-4",
    shiny::h1(title),
    shiny::p(
      shiny::tags$span(class = "kit-subtitle", subtitle),
      " · ", tagline
    )
  )
}
