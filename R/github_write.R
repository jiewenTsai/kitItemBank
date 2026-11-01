# R/github_write.R — 核准的修改包寫進 GitHub：一次 commit、一個題庫版本標籤
#
# 設定只從環境變數來（不寫在程式裡、不存檔）：
#   KIT_GITHUB_TOKEN         有 contents:write 權限的 token（fine-grained，只給這個 repo）
#   KIT_GITHUB_REPO          預設 jiewenTsai/kitItmBank
#   KIT_GITHUB_BRANCH        預設 main
#   KIT_ADMIN_PASSWORD_HASH  管理密碼的雜湊，用 kit_password_hash("密碼") 產生；不放密碼本身
#
# 連網路的只有 github_api() 一個函式；測試時換掉它（testthat::local_mocked_bindings），
# 不會真的連到 GitHub。其他函式（github_publish() 等）只把 GitHub Git Data API 的步驟串起來：
#   讀 branch 最新的 commit → 確認 GitHub 上的 BANK_VERSION 和 app 的一樣 → 每個檔案一個 blob
#   → 一棵 tree → 一個 commit → 移動 branch → 建 annotated tag（bank-2026.2）

GITHUB_DEFAULT_REPO <- "jiewenTsai/kitItmBank"
GITHUB_DEFAULT_BRANCH <- "main"

#' GitHub settings for approving change packages
#'
#' Read from environment variables only: `KIT_GITHUB_TOKEN`, `KIT_GITHUB_REPO` (default
#' `jiewenTsai/kitItmBank`), `KIT_GITHUB_BRANCH` (default `main`) and
#' `KIT_ADMIN_PASSWORD_HASH` (made with [kit_password_hash()]).
#'
#' @return A list: `token`, `repo`, `branch`, `password_hash`, `ok` (everything needed is
#'   there) and `problems` (plain-language reasons approval is off; empty when `ok`).
#' @export
github_config <- function() {
  env <- function(x, default = "") {
    v <- trimws(Sys.getenv(x, ""))
    if (nzchar(v)) v else default
  }
  cfg <- list(
    token = env("KIT_GITHUB_TOKEN"),
    repo = env("KIT_GITHUB_REPO", GITHUB_DEFAULT_REPO),
    branch = env("KIT_GITHUB_BRANCH", GITHUB_DEFAULT_BRANCH),
    password_hash = env("KIT_ADMIN_PASSWORD_HASH")
  )
  problems <- c(
    # 原文：沒有設定 GitHub 的寫入權杖（環境變數 KIT_GITHUB_TOKEN）
    if (!nzchar(cfg$token)) "\u6c92\u6709\u8a2d\u5b9a GitHub \u7684\u5beb\u5165\u6b0a\u6756\uff08\u74b0\u5883\u8b8a\u6578 KIT_GITHUB_TOKEN\uff09",
    if (!nzchar(cfg$password_hash)) {
      # 原文：沒有設定管理密碼（環境變數 KIT_ADMIN_PASSWORD_HASH）
      "\u6c92\u6709\u8a2d\u5b9a\u7ba1\u7406\u5bc6\u78bc\uff08\u74b0\u5883\u8b8a\u6578 KIT_ADMIN_PASSWORD_HASH\uff09"
    } else if (is.null(parse_password_hash(cfg$password_hash))) {
      # 原文：管理密碼的設定格式不對（KIT_ADMIN_PASSWORD_HASH 要用 kit_password_hash() 產生）
      "\u7ba1\u7406\u5bc6\u78bc\u7684\u8a2d\u5b9a\u683c\u5f0f\u4e0d\u5c0d\uff08KIT_ADMIN_PASSWORD_HASH \u8981\u7528 kit_password_hash() \u7522\u751f\uff09"
    },
    if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", cfg$repo)) {
      # 原文：GitHub 專案的名稱格式不對（KIT_GITHUB_REPO： | ）
      paste0("GitHub \u5c08\u6848\u7684\u540d\u7a31\u683c\u5f0f\u4e0d\u5c0d\uff08KIT_GITHUB_REPO\uff1a", cfg$repo, "\uff09")
    },
    if (!requireNamespace("httr2", quietly = TRUE) || !requireNamespace("openssl", quietly = TRUE)) {
      # 原文：伺服器上沒有裝 httr2 或 openssl 套件
      "\u4f3a\u670d\u5668\u4e0a\u6c92\u6709\u88dd httr2 \u6216 openssl \u5957\u4ef6"
    }
  )
  cfg$ok <- length(problems) == 0L
  cfg$problems <- problems
  structure(cfg, class = "kit_github_config")
}

#' @export
format.kit_github_config <- function(x, ...) {
  paste0("<kit_github_config> ", x$repo, "@", x$branch,
         if (isTRUE(x$ok)) " (ready)" else " (not configured)")
}

#' @export
print.kit_github_config <- function(x, ...) {
  # 不印出 token 和密碼雜湊
  cat(format(x), "\n")
  invisible(x)
}

#' Hash an admin password
#'
#' Put the result in the environment variable `KIT_ADMIN_PASSWORD_HASH`; never store the
#' password itself. The hash is salted SHA-256: `sha256$<salt>$<hex>`.
#'
#' @param password The password.
#' @param salt A random salt (hex); a new one is made when not given.
#' @return One string.
#' @examples
#' if (requireNamespace("openssl", quietly = TRUE)) kit_password_hash("example")
#' @export
kit_password_hash <- function(password, salt = NULL) {
  if (!(is.character(password) && length(password) == 1L && !is.na(password) &&
        nzchar(password))) {
    # 原文：密碼不能是空的。
    kitDesign::kit_abort("\u5bc6\u78bc\u4e0d\u80fd\u662f\u7a7a\u7684\u3002")
  }
  salt <- salt %||% paste(as.character(openssl::rand_bytes(16L)), collapse = "")
  paste0("sha256$", salt, "$", password_digest(password, salt))
}

password_digest <- function(password, salt) {
  as.character(openssl::sha256(charToRaw(enc2utf8(paste0(salt, password)))))
}

#' 拆開密碼雜湊：sha256$salt$hex，或只有 64 位 hex（沒有 salt）；格式不對回傳 NULL
#' @noRd
parse_password_hash <- function(stored) {
  stored <- tolower(trimws(stored))
  if (grepl("^[0-9a-f]{64}$", stored)) return(list(salt = "", hex = stored))
  m <- regmatches(stored, regexec("^sha256\\$([0-9a-f]*)\\$([0-9a-f]{64})$", stored))[[1L]]
  if (length(m) != 3L) return(NULL)
  list(salt = m[2L], hex = m[3L])
}

#' Check a typed admin password against the stored hash
#'
#' @param password What was typed.
#' @param stored The stored hash (from [kit_password_hash()]).
#' @return `TRUE` or `FALSE`.
#' @export
kit_password_ok <- function(password, stored) {
  h <- parse_password_hash(stored %||% "")
  if (is.null(h) || !is.character(password) || length(password) != 1L || is.na(password) ||
      !nzchar(password)) {
    return(FALSE)
  }
  a <- charToRaw(password_digest(password, h$salt))
  b <- charToRaw(h$hex)
  # 每一位都比，不在第一個不同的地方提早結束
  length(a) == length(b) && sum(as.integer(xor(a, b))) == 0L
}

#' The one function that talks to GitHub
#'
#' @param cfg From [github_config()].
#' @param method `"GET"`, `"POST"` or `"PATCH"`.
#' @param path API path after `https://api.github.com/`, e.g. `"repos/o/r/git/blobs"`.
#' @param body A list sent as JSON, or `NULL`.
#' @param query A named list of query parameters, or `NULL`.
#' @return The parsed JSON response (a list). Stops with a plain-language message when GitHub
#'   answers with an error.
#' @noRd
github_api <- function(cfg, method, path, body = NULL, query = NULL) {
  req <- httr2::request("https://api.github.com")
  req <- httr2::req_url_path_append(req, path)
  if (length(query)) req <- do.call(httr2::req_url_query, c(list(req), query))
  req <- httr2::req_method(req, method)
  req <- httr2::req_auth_bearer_token(req, cfg$token)
  req <- httr2::req_headers(req, Accept = "application/vnd.github+json",
                            `X-GitHub-Api-Version` = "2022-11-28")
  req <- httr2::req_user_agent(req, "kitItemBank")
  req <- httr2::req_timeout(req, 60)
  if (!is.null(body)) req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  out <- if (length(httr2::resp_body_raw(resp))) {
    tryCatch(httr2::resp_body_json(resp), error = function(e) list())
  } else list()
  if (status >= 300L) {
    msg <- out$message %||% httr2::resp_status_desc(resp)
    # 原文：GitHub 回應錯誤（ | ）：
    kitDesign::kit_abort("GitHub \u56de\u61c9\u932f\u8aa4\uff08", status, " ", method, " ", path, "\uff09\uff1a", msg,
                         class = "kit_github_error", data = list(status = status))
  }
  out
}

github_repo_path <- function(cfg, ...) paste("repos", cfg$repo, ..., sep = "/")

#' GitHub 上 branch 的 BANK_VERSION（第一行）
#' @noRd
github_bank_version <- function(cfg, path = "inst/extdata/kit/BANK_VERSION") {
  res <- github_api(cfg, "GET", github_repo_path(cfg, "contents", path),
                    query = list(ref = cfg$branch))
  txt <- rawToChar(openssl::base64_decode(gsub("\\s", "", res$content %||% "")))
  Encoding(txt) <- "UTF-8"
  trimws(strsplit(txt, "\n", fixed = TRUE)[[1L]][1L])
}

#' Write files to GitHub as one commit and tag it
#'
#' Uses the Git Data API: blobs → one tree → one commit → move the branch (no force) →
#' annotated tag. Before writing, checks that `BANK_VERSION` on GitHub is still
#' `expected_version`, so a bank changed after this app was deployed is never overwritten.
#'
#' @param files Named list: repository path → content (raw or character).
#' @param message The commit message.
#' @param tag The tag name, e.g. `"bank-2026.2"`; `NULL` for no tag.
#' @param tag_message The tag message.
#' @param expected_version The bank version GitHub must have now; `NULL` to skip the check.
#' @param cfg From [github_config()].
#' @return A list: `commit` (sha), `url` (web page of the commit), `tag`, `tag_error`
#'   (`""` when the tag was made; the commit stays even when only the tag fails).
#' @export
github_publish <- function(files, message, tag = NULL, tag_message = message,
                           expected_version = NULL, cfg = github_config()) {
  if (!length(files) || is.null(names(files)) || any(!nzchar(names(files)))) {
    # 原文：沒有要寫進 GitHub 的檔案。
    kitDesign::kit_abort("\u6c92\u6709\u8981\u5beb\u9032 GitHub \u7684\u6a94\u6848\u3002")
  }
  ref <- github_api(cfg, "GET", github_repo_path(cfg, "git", "ref", "heads", cfg$branch))
  head <- ref$object$sha
  if (!is.null(expected_version)) {
    gh_ver <- github_bank_version(cfg)
    if (!identical(gh_ver, expected_version)) {
      kitDesign::kit_abort(
        # 原文：GitHub 上的題庫已經是  |  版，這個 app 用的還是 
        "GitHub \u4e0a\u7684\u984c\u5eab\u5df2\u7d93\u662f ", gh_ver, " \u7248\uff0c\u9019\u500b app \u7528\u7684\u9084\u662f ", expected_version,
        # 原文： 版。為了不蓋掉別人的改動，這次沒有寫入：等 app 重新部署成新版題庫以後，再審核一次。
        " \u7248\u3002\u70ba\u4e86\u4e0d\u84cb\u6389\u5225\u4eba\u7684\u6539\u52d5\uff0c\u9019\u6b21\u6c92\u6709\u5beb\u5165\uff1a\u7b49 app \u91cd\u65b0\u90e8\u7f72\u6210\u65b0\u7248\u984c\u5eab\u4ee5\u5f8c\uff0c\u518d\u5be9\u6838\u4e00\u6b21\u3002",
        class = "kit_github_stale"
      )
    }
  }
  base_tree <- github_api(cfg, "GET", github_repo_path(cfg, "git", "commits", head))$tree$sha
  entries <- lapply(names(files), function(path) {
    x <- files[[path]]
    if (is.character(x)) x <- charToRaw(enc2utf8(paste(x, collapse = "\n")))
    blob <- github_api(cfg, "POST", github_repo_path(cfg, "git", "blobs"),
                       body = list(content = openssl::base64_encode(x), encoding = "base64"))
    list(path = path, mode = "100644", type = "blob", sha = blob$sha)
  })
  tree <- github_api(cfg, "POST", github_repo_path(cfg, "git", "trees"),
                     body = list(base_tree = base_tree, tree = entries))
  commit <- github_api(cfg, "POST", github_repo_path(cfg, "git", "commits"),
                       body = list(message = message, tree = tree$sha, parents = list(head)))
  github_api(cfg, "PATCH", github_repo_path(cfg, "git", "refs", "heads", cfg$branch),
             body = list(sha = commit$sha, force = FALSE))
  tag_error <- ""
  if (!is.null(tag)) {
    tag_error <- tryCatch({
      tg <- github_api(cfg, "POST", github_repo_path(cfg, "git", "tags"),
                       body = list(tag = tag, message = tag_message, object = commit$sha,
                                   type = "commit"))
      github_api(cfg, "POST", github_repo_path(cfg, "git", "refs"),
                 body = list(ref = paste0("refs/tags/", tag), sha = tg$sha))
      ""
    }, error = function(e) conditionMessage(e))
  }
  list(commit = commit$sha,
       url = commit$html_url %||% paste0("https://github.com/", cfg$repo, "/commit/", commit$sha),
       tag = tag %||% "", tag_error = tag_error)
}

#' Approve a reviewed change package: write it to GitHub
#'
#' Bumps the bank version, makes the change record, and writes the changed tables,
#' `BANK_VERSION` and the record as one commit tagged `bank-<new version>`.
#'
#' @param review The result of [review_patch()] (must have no `blocking` reasons).
#' @param meta A list with `reason`, `decided_by`, `submitted_by`, `approved_by`.
#' @param cfg From [github_config()].
#' @param date The approval date.
#' @param row_meta Optional per-row reasons (see [change_record()]).
#' @return The result of [github_publish()], plus `version` (the new bank version).
#' @export
review_approve <- function(review, meta, cfg = github_config(), date = Sys.Date(),
                           row_meta = NULL) {
  if (length(review$blocking)) {
    # 原文：這個修改包還不能核准： | ； | 。
    kitDesign::kit_abort("\u9019\u500b\u4fee\u6539\u5305\u9084\u4e0d\u80fd\u6838\u51c6\uff1a", paste(review$blocking, collapse = "\uff1b"), "\u3002")
  }
  if (!isTRUE(cfg$ok)) {
    # 原文：還不能寫進 GitHub： | ； | 。
    kitDesign::kit_abort("\u9084\u4e0d\u80fd\u5beb\u9032 GitHub\uff1a", paste(cfg$problems, collapse = "\uff1b"), "\u3002")
  }
  new_version <- bump_bank_version(review$current_version, date)
  record <- change_record(review, meta, new_version, date, row_meta = row_meta)
  files <- review_files(review, record, new_version)
  msg <- review_commit_message(review, meta, new_version)
  res <- github_publish(files, msg, tag = paste0("bank-", new_version),
                        tag_message = paste0("KIT bank ", new_version),
                        expected_version = review$current_version, cfg = cfg)
  res$version <- new_version
  res$files <- names(files)
  res
}

#' commit 訊息：第一行說新版本和改動數，下面是原因和人
#' @noRd
review_commit_message <- function(review, meta, new_version) {
  m <- function(f) {
    v <- as.character(meta[[f]] %||% "")
    if (length(v) != 1L || is.na(v)) "" else trimws(v)
  }
  n <- sum(review$patch$ok)
  tables <- unique(review$patch$table[review$patch$ok])
  paste(c(
    paste0("Bank ", new_version, ": ", n, " change", if (n != 1L) "s", " (",
           paste(tables, collapse = ", "), ")"),
    "",
    if (nzchar(m("reason"))) paste0("Reason: ", m("reason")),
    paste0("Base version: ", if (nzchar(review$base_version)) review$base_version else "?",
           "; applied to: ", review$current_version),
    if (nzchar(m("decided_by"))) paste0("Decided by: ", m("decided_by")),
    if (nzchar(m("submitted_by"))) paste0("Submitted by: ", m("submitted_by")),
    if (nzchar(m("approved_by"))) paste0("Approved by: ", m("approved_by")),
    paste0("Change record: inst/extdata/kit/", change_record_path(new_version))
  ), collapse = "\n")
}
