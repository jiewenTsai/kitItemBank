# R/utils_assemble_io.R — 組卷草稿存檔 / 讀檔（JSON）

KIT_DRAFT_VERSION <- "1"

#' 將組卷 reactiveValues 狀態序列化為 list
serialize_assemble_state <- function(blk_rv) {
  blk_list <- isolate(reactiveValuesToList(blk_rv))
  list(
    kit_draft_version = KIT_DRAFT_VERSION,
    saved_at          = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    next_id           = blk_list$next_id %||% 1L,
    next_mg           = blk_list$next_mg %||% 1L,
    order             = blk_list$order %||% character(0),
    blocks            = blk_list$data %||% list()
  )
}

#' 寫入 JSON 草稿檔
save_assemble_draft <- function(blk_rv, path) {
  state <- serialize_assemble_state(blk_rv)
  jsonlite::write_json(state, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}

#' 從 JSON 草稿還原至 reactiveValues
load_assemble_draft <- function(blk_rv, path) {
  if (!file.exists(path)) stop("\u627e\u4e0d\u5230\u6a94\u6848\uff1a", path)
  state <- jsonlite::read_json(path, simplifyVector = FALSE)

  ver <- state$kit_draft_version %||% "0"
  if (ver != KIT_DRAFT_VERSION) {
    warning("\u8349\u7a3f\u7248\u672c\u70ba ", ver, "\uff0c\u76ee\u524d\u652f\u63f4 ", KIT_DRAFT_VERSION)
  }

  order  <- unlist(state$order %||% list(), use.names = FALSE)
  blocks <- state$blocks %||% list()

  if (is.null(blocks) || length(blocks) == 0L) {
    stop("\u8349\u7a3f\u5167\u5bb9\u70ba\u7a7a")
  }

  blk_rv$order   <- order
  blk_rv$data    <- blocks
  blk_rv$next_id <- as.integer(state$next_id %||% 1L)
  blk_rv$next_mg <- as.integer(state$next_mg %||% 1L)

  list(
    order   = order,
    current = if (length(order) > 0L) order[1L] else NULL
  )
}
