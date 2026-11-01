# R/app.R — KIT 題庫管理工具：頁面、伺服器、啟動
#
# 頁首（kit_page）→ 分組分頁（查詢：題項查詢、構面查詢、施測、比較｜
# 維護：待辦事項、題庫編修、改版、併入新施測、審核、統計）
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
    show_kitdesign = TRUE,
    # kit_step_tabs_ui("nav", groups = ) 的兩半拆開，中間放題庫選擇列：
    # 分頁列 → 題庫選擇列 → 分頁內容（tabset id 照模組的 "nav-tabs"）
    kitDesign::kit_navgroups(APP_TAB_GROUPS, "nav-tabs", selected = tab),
    shiny::div(class = "kit-hint", style = "text-align:right;margin:-4px 0 6px;", manual_link()),
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
      shiny::tabPanel("\u7d71\u8a08", value = "stats", page_stats_ui("stats"))
    )
  )
}

#' 頁首的「操作手冊」連結（inst/manual/manual.html，由 data-raw/build_manual.R 產生）
#' @noRd
manual_link <- function() {
  shiny::addResourcePath("kit-manual", system.file("manual", package = "kitItemBank"))
  # 原文：操作手冊（含術語表）
  shiny::tags$a(href = "kit-manual/manual.html", target = "_blank", rel = "noopener",
                "\u64cd\u4f5c\u624b\u518a\uff08\u542b\u8853\u8a9e\u8868\uff09")
}

APP_TAB_GROUPS <- list(
  # 原文：查詢 | 題項查詢 | 構面查詢 | 施測 | 比較
  kitDesign::kit_navgroup("\u67e5\u8a62", c(search = "\u984c\u9805\u67e5\u8a62", dims = "\u69cb\u9762\u67e5\u8a62",
                                     admin = "\u65bd\u6e2c", compare = "\u6bd4\u8f03")),
  # 原文：維護 | 待辦事項 | 題庫編修 | 改版 | 併入新施測 | 審核 | 統計
  kitDesign::kit_navgroup("\u7dad\u8b77", c(home = "\u5f85\u8fa6\u4e8b\u9805", edit = "\u984c\u5eab\u7de8\u4fee",
                                     revision = "\u6539\u7248", merge = "\u4f75\u5165\u65b0\u65bd\u6e2c",
                                     review = "\u5be9\u6838", stats = "\u7d71\u8a08"))
)

# 所有分頁的 value（從網址打開時要認得）
APP_TABS <- c("search", "dims", "admin", "compare", "home", "edit", "revision", "merge",
              "review", "stats")

#' The app's server
#'
#' @param input,output,session Shiny's arguments.
#' @export
app_server <- function(input, output, session) {
  store <- bank_store_server("bank")
  nav <- kitDesign::kit_step_tabs_server("nav")
  # 頁和頁之間用連結接起來，不靠共用狀態：
  #   goto("dims", dim = "睡眠習慣")  → 切到構面查詢，request() 收到 list(tab = "dims", dim = …)
  #   goto("search", item = "sleC1203") → 切到題項查詢、打開那一題
  request <- shiny::reactiveVal(NULL)
  goto <- function(tab, ...) {
    request(list(tab = tab, args = list(...), at = Sys.time()))
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
