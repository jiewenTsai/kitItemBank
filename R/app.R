# R/app.R — KIT 題庫管理工具：頁面、伺服器、啟動
#
# 頁首（kit_page）→ 分組分頁（查詢：題項查詢、構面查詢、施測、比較｜
# 維護：待辦事項、題庫編修、改版、併入新施測、審核、統計｜說明：操作手冊）
# → 題庫選擇列（全 app 共用）→ 分頁內容。
# 查詢的兩頁只看；改題庫只在「題庫編修」，改好的題庫用下載的，app 不寫檔。

#' The app's page
#'
#' @param request Shiny's request (used for URL bookmarks).
#' @return The page UI.
#' @export
app_ui <- function(request) {
  ver <- kitDesign::kit_app_description("kitItemBank")
  # 從網址打開時回到那個分頁（分頁列的萊姆色也要跟著）
  tab <- shiny::restoreInput("nav-tabs", "search")
  if (!tab %in% APP_TABS) tab <- "search"
  kitDesign::kit_page(
    # 原文：KIT 題庫管理工具 | 臺灣幼兒發展調查資料庫
    "KIT \u984c\u5eab\u7ba1\u7406\u5de5\u5177", "\u81fa\u7063\u5e7c\u5152\u767c\u5c55\u8abf\u67e5\u8cc7\u6599\u5eab",
    # 原文：查詢與維護題庫
    tagline = "\u67e5\u8a62\u8207\u7dad\u8b77\u984c\u5eab",
    version = ver$version, date = ver$date, package = "kitItemBank",
    # kit_step_tabs_ui("nav", groups = ) 的兩半拆開，中間放題庫選擇列：
    # 分頁列 → 題庫選擇列 → 分頁內容（tabset id 照模組的 "nav-tabs"）
    kitDesign::kit_navgroups(APP_TAB_GROUPS, "nav-tabs", selected = tab),
    # 瀏覽器回報時區：匯出檔名和產生時間用使用者的時區，不用主機的 UTC（kit_tz()）
    shiny::tags$script(shiny::HTML(paste0(
      "$(document).on('shiny:connected', function () {",
      " try { Shiny.setInputValue('client_tz', Intl.DateTimeFormat().resolvedOptions().timeZone); } catch (e) {}",
      " });"))),
    bank_bar_ui("bank"),
    shiny::tabsetPanel(
      id = "nav-tabs", type = "hidden", selected = tab,
      # 原文：題項查詢
      shiny::tabPanel("\u984c\u9805\u67e5\u8a62", value = "search", page_search_ui("search")),
      # 原文：構面查詢
      shiny::tabPanel("\u69cb\u9762\u67e5\u8a62", value = "dims", page_dims_ui("dims")),
      # 原文：施測
      shiny::tabPanel("\u65bd\u6e2c", value = "admin", page_admin_ui("admin")),
      # 原文：比較
      shiny::tabPanel("\u6bd4\u8f03", value = "compare", page_compare_ui("compare")),
      # 原文：待辦事項
      shiny::tabPanel("\u5f85\u8fa6\u4e8b\u9805", value = "home", page_home_ui("home")),
      # 原文：題庫編修
      shiny::tabPanel("\u984c\u5eab\u7de8\u4fee", value = "edit", page_edit_ui("edit")),
      # 原文：改版
      shiny::tabPanel("\u6539\u7248", value = "revision", page_revision_ui("revision")),
      # 原文：併入新施測
      shiny::tabPanel("\u4f75\u5165\u65b0\u65bd\u6e2c", value = "merge", page_merge_ui("merge")),
      # 原文：審核
      shiny::tabPanel("\u5be9\u6838", value = "review", page_review_ui("review")),
      # 原文：統計
      shiny::tabPanel("\u7d71\u8a08", value = "stats", page_stats_ui("stats")),
      # 原文：操作手冊
      shiny::tabPanel("\u64cd\u4f5c\u624b\u518a", value = "manual", page_manual_ui())
    )
  )
}

#' 「說明 → 操作手冊」分頁：左邊目錄、右邊內文
#'
#' 內容是 inst/manual/manual_fragment.html，由 data-raw/build_manual.R 從 docs/操作手冊.qmd 產生；
#' 同一份手冊的單獨網頁（manual.html）掛在 kit-manual/ 底下，給「在新分頁開」用。
#' @noRd
page_manual_ui <- function() {
  dir <- system.file("manual", package = "kitItemBank")
  shiny::addResourcePath("kit-manual", dir)
  shiny::includeHTML(file.path(dir, "manual_fragment.html"))
}

APP_TAB_GROUPS <- list(
  # 原文：查詢 | 題項查詢 | 構面查詢 | 施測 | 比較
  kitDesign::kit_navgroup("\u67e5\u8a62", c(search = "\u984c\u9805\u67e5\u8a62", dims = "\u69cb\u9762\u67e5\u8a62",
                                     admin = "\u65bd\u6e2c", compare = "\u6bd4\u8f03")),
  # 原文：維護 | 待辦事項 | 題庫編修 | 改版 | 併入新施測 | 審核 | 統計
  kitDesign::kit_navgroup("\u7dad\u8b77", c(home = "\u5f85\u8fa6\u4e8b\u9805", edit = "\u984c\u5eab\u7de8\u4fee",
                                     revision = "\u6539\u7248", merge = "\u4f75\u5165\u65b0\u65bd\u6e2c",
                                     review = "\u5be9\u6838", stats = "\u7d71\u8a08")),
  # 原文：說明 | 操作手冊（不是工作的一步：虛線框）
  kitDesign::kit_navgroup("\u8aaa\u660e", c(manual = "\u64cd\u4f5c\u624b\u518a"), tools = TRUE)
)

# 所有分頁的 value（從網址打開時要認得）
APP_TABS <- c("search", "dims", "admin", "compare", "home", "edit", "revision", "merge",
              "review", "stats", "manual")

#' The app's server
#'
#' @param input,output,session Shiny's arguments.
#' @export
app_server <- function(input, output, session) {
  shiny::observeEvent(input$client_tz, session$userData$tz <- input$client_tz)
  store <- bank_store_server("bank")
  nav <- kitDesign::kit_step_tabs_server("nav")
  # 頁和頁之間用連結接起來，不靠共用狀態：
  #   goto("dims", dim = "睡眠習慣")  → 切到構面查詢，request() 收到 list(tab = "dims", dim = …)
  #   goto("search", item = "sleC1203") → 切到題項查詢、打開那一題
  request <- shiny::reactiveVal(NULL)
  goto <- function(tab, ...) {
    request(list(tab = tab, args = list(...), at = kit_now()))
    nav$select(tab)
  }
  # 查詢條件寫進網址（kit_app() 用 enableBookmarking = "url"），網址貼給同事就看到同樣的結果
  # 只有題項查詢的條件、題庫、目前的分頁進網址；編修與構面頁的設定不進網址
  shiny::observe({
    ids <- names(shiny::reactiveValuesToList(input, all.names = TRUE))
    keep <- grepl("^search-", ids) | ids %in% c("bank-domain", "nav-tabs")
    session$setBookmarkExclude(ids[!keep])
    session$doBookmark()
  })
  shiny::onBookmarked(shiny::updateQueryString)
  page_search_server("search", store, goto, request)
  page_dims_server("dims", store, goto, request)
  page_admin_server("admin", kit_builtin(), goto, request)
  page_compare_server("compare", kit_builtin(), goto, request)
  page_home_server("home", kit_builtin(), goto)
  page_edit_server("edit", store, goto, request)
  page_revision_server("revision", kit_builtin())
  page_merge_server("merge", kit_builtin(), goto, request)
  page_review_server("review", kit_builtin(), goto)
  page_stats_server("stats", kit_builtin())
}

#' The KIT item bank tool as a shinyApp object
#'
#' For deployment (Posit Connect Cloud uses the root `app.R`, which calls this).
#' @param ... Passed to [shiny::shinyApp()].
#' @return A `shiny.appobj`.
#' @export
kit_app <- function(...) {
  shiny::shinyApp(app_ui, app_server, enableBookmarking = "url", ...)
}

#' Open the KIT item bank tool
#'
#' The local app enables the statistics page (統計), which reads uploaded data files;
#' the cloud app (`app.R`, [kit_app()]) does not.
#'
#' @param ... Passed to [shiny::runApp()], for example `port` or `launch.browser`.
#' @return Called for its side effect (runs the app).
#' @examples
#' if (interactive()) run_kititembank()
#' @export
run_kititembank <- function(...) {
  # 本機版：開放「統計」頁上傳資料（雲端版的 app.R 不經過這裡，所以不開放）；
  # .sav 檔常超過 shiny 預設的 5 MB 上傳上限
  old <- options(kititembank.allow_data_upload = TRUE, shiny.maxRequestSize = 500 * 1024^2)
  on.exit(options(old), add = TRUE)
  shiny::runApp(kit_app(), ...)
}
