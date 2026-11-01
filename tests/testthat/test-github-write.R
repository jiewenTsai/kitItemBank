# github_write.R：設定、密碼、寫進 GitHub 的步驟。github_api() 一律換成假的，不會連網路。

fake_cfg <- function(...) {
  cfg <- list(token = "test-token", repo = "someone/repo", branch = "main",
              password_hash = kit_password_hash("pw", salt = "ab"), ok = TRUE,
              problems = character(0))
  utils::modifyList(cfg, list(...))
}

# 假的 GitHub：記下每次呼叫，照路徑回答
fake_github <- function(bank_version = "2026.1", fail_tag = FALSE) {
  calls <- list()
  fn <- function(cfg, method, path, body = NULL, query = NULL) {
    calls[[length(calls) + 1L]] <<- list(method = method, path = path, body = body, query = query)
    if (method == "GET" && grepl("/git/ref/heads/", path)) return(list(object = list(sha = "HEAD0")))
    if (method == "GET" && grepl("/contents/", path)) {
      return(list(content = openssl::base64_encode(charToRaw(paste0(bank_version, "\n")))))
    }
    if (method == "GET" && grepl("/git/commits/", path)) return(list(tree = list(sha = "TREE0")))
    if (method == "POST" && grepl("/git/blobs$", path)) {
      return(list(sha = paste0("BLOB", length(calls))))
    }
    if (method == "POST" && grepl("/git/trees$", path)) return(list(sha = "TREE1"))
    if (method == "POST" && grepl("/git/commits$", path)) {
      return(list(sha = "COMMIT1", html_url = "https://github.com/someone/repo/commit/COMMIT1"))
    }
    if (method == "PATCH") return(list(object = list(sha = "COMMIT1")))
    if (method == "POST" && grepl("/git/tags$", path)) {
      if (fail_tag) kitDesign::kit_abort("tag exists")
      return(list(sha = "TAG1"))
    }
    if (method == "POST" && grepl("/git/refs$", path)) return(list(ref = body$ref))
    stop("unexpected call: ", method, " ", path)
  }
  list(fn = fn, calls = function() calls)
}

test_that("github_config reads only environment variables and says what is missing", {
  withr::local_envvar(KIT_GITHUB_TOKEN = NA, KIT_GITHUB_REPO = NA, KIT_GITHUB_BRANCH = NA,
                      KIT_ADMIN_PASSWORD_HASH = NA)
  cfg <- github_config()
  expect_false(cfg$ok)
  expect_identical(cfg$repo, "jiewenTsai/kitItmBank")
  expect_identical(cfg$branch, "main")
  expect_match(cfg$problems, "KIT_GITHUB_TOKEN", all = FALSE)
  expect_match(cfg$problems, "KIT_ADMIN_PASSWORD_HASH", all = FALSE)

  withr::local_envvar(KIT_GITHUB_TOKEN = "tok-SECRET", KIT_ADMIN_PASSWORD_HASH = "not-a-hash",
                      KIT_GITHUB_REPO = "a/b", KIT_GITHUB_BRANCH = "dev")
  cfg <- github_config()
  expect_false(cfg$ok)
  expect_match(cfg$problems, "格式不對", all = FALSE)

  withr::local_envvar(KIT_ADMIN_PASSWORD_HASH = kit_password_hash("secret"))
  cfg <- github_config()
  expect_true(cfg$ok)
  expect_identical(cfg$repo, "a/b")
  expect_identical(cfg$branch, "dev")
  expect_false(grepl("tok-SECRET", format.kit_github_config(cfg), fixed = TRUE))
  expect_false(grepl(cfg$password_hash, format.kit_github_config(cfg), fixed = TRUE))
})

test_that("passwords are compared by salted hash", {
  h <- kit_password_hash("正確的密碼")
  expect_match(h, "^sha256\\$[0-9a-f]{32}\\$[0-9a-f]{64}$")
  expect_false(grepl("正確的密碼", h))
  expect_true(kit_password_ok("正確的密碼", h))
  expect_false(kit_password_ok("錯的", h))
  expect_false(kit_password_ok("", h))
  expect_false(kit_password_ok(NA_character_, h))
  expect_false(kit_password_ok("x", ""))
  expect_false(kit_password_ok("x", "garbage"))
  # 兩次雜湊用不同的 salt
  expect_false(identical(kit_password_hash("a"), kit_password_hash("a")))
  # 沒有 salt 的 64 位 hex 也收（例如用別的工具算的 sha256）
  plain <- as.character(openssl::sha256(charToRaw("abc")))
  expect_true(kit_password_ok("abc", toupper(plain)))
  expect_error(kit_password_hash(""), "不能是空的")
})

test_that("github_publish makes one commit with every file, then a tag", {
  gh <- fake_github()
  local_mocked_bindings(github_api = gh$fn)
  files <- list("inst/extdata/kit/BANK_VERSION" = charToRaw("2026.2\n"),
                "inst/extdata/kit/groups.csv" = "a,b")
  res <- github_publish(files, "msg", tag = "bank-2026.2", expected_version = "2026.1",
                        cfg = fake_cfg())
  expect_identical(res$commit, "COMMIT1")
  expect_identical(res$tag_error, "")
  expect_match(res$url, "COMMIT1$")
  calls <- gh$calls()
  what <- vapply(calls, function(x) paste(x$method, sub("^repos/someone/repo/", "", x$path)),
                 character(1))
  expect_identical(what, c(
    "GET git/ref/heads/main", "GET contents/inst/extdata/kit/BANK_VERSION",
    "GET git/commits/HEAD0", "POST git/blobs", "POST git/blobs", "POST git/trees",
    "POST git/commits", "PATCH git/refs/heads/main", "POST git/tags", "POST git/refs"
  ))
  expect_identical(calls[[2]]$query, list(ref = "main"))
  blob <- calls[[4]]$body
  expect_identical(blob$encoding, "base64")
  expect_identical(rawToChar(openssl::base64_decode(blob$content)), "2026.2\n")
  tree <- calls[[6]]$body
  expect_identical(tree$base_tree, "TREE0")
  expect_identical(vapply(tree$tree, `[[`, "", "path"), names(files))
  expect_identical(calls[[7]]$body$parents, list("HEAD0"))
  expect_false(calls[[8]]$body$force)
  expect_identical(calls[[10]]$body$ref, "refs/tags/bank-2026.2")
  expect_identical(calls[[10]]$body$sha, "TAG1")
})

test_that("github_publish refuses when GitHub's bank moved on; a failed tag keeps the commit", {
  gh <- fake_github(bank_version = "2026.2")
  local_mocked_bindings(github_api = gh$fn)
  expect_error(github_publish(list(a = "x"), "m", expected_version = "2026.1", cfg = fake_cfg()),
               class = "kit_github_stale")
  expect_false(any(vapply(gh$calls(), function(x) x$method != "GET", logical(1))))

  gh2 <- fake_github(fail_tag = TRUE)
  local_mocked_bindings(github_api = gh2$fn)
  res <- github_publish(list(a = "x"), "m", tag = "bank-2026.2", cfg = fake_cfg())
  expect_identical(res$commit, "COMMIT1")
  expect_match(res$tag_error, "tag exists")
  expect_error(github_publish(list(), "m", cfg = fake_cfg()), "沒有要寫進")
})

test_that("review_approve bumps the version and writes tables, BANK_VERSION and the record", {
  gh <- fake_github()
  local_mocked_bindings(github_api = gh$fn)
  kit <- kit_builtin()
  g <- kit$groups$groupID[1]
  p <- data.frame(op = "set", table = "groups", key = g, column = "label",
                  from = kit$groups$label[1], to = "新名稱", stringsAsFactors = FALSE)
  rv <- review_patch(kit, p, "2026.1", "2026.1")
  meta <- list(reason = "改名", decided_by = "會議", submitted_by = "甲", approved_by = "乙")
  res <- review_approve(rv, meta, fake_cfg(), date = as.Date("2026-11-02"))
  expect_identical(res$version, "2026.2")
  expect_setequal(res$files, c("inst/extdata/kit/groups.csv", "inst/extdata/kit/BANK_VERSION",
                               "inst/extdata/kit/changes/2026.2.csv"))
  calls <- gh$calls()
  commit <- Filter(function(x) x$method == "POST" && grepl("git/commits$", x$path), calls)[[1]]
  expect_match(commit$body$message, "^Bank 2026.2: 1 change \\(groups\\)")
  expect_match(commit$body$message, "Approved by: 乙")
  tag <- Filter(function(x) grepl("git/tags$", x$path), calls)[[1]]
  expect_identical(tag$body$tag, "bank-2026.2")

  bad <- review_patch(kit, p[0, ], "2026.1", "2026.1")
  expect_error(review_approve(bad, meta, fake_cfg()), "還不能核准")
  expect_error(review_approve(rv, meta, fake_cfg(ok = FALSE, problems = "x")), "還不能寫進")
})
