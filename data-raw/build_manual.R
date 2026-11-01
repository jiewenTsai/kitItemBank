# data-raw/build_manual.R — 把 docs/操作手冊.md 轉成 app 裡的網頁版（inst/manual/manual.html）
#
# 用法（在套件根目錄）：
#   LC_ALL=en_US.UTF-8 Rscript --no-init-file data-raw/build_manual.R
# 改了 docs/操作手冊.md 就重跑一次；app 頁首的「操作手冊」連到這個檔。

md <- readLines("docs/操作手冊.md", encoding = "UTF-8")
body <- commonmark::markdown_html(paste(md, collapse = "\n"), extensions = TRUE)

# 標題加上和 GitHub 一樣的 id，目錄的連結才跳得過去
slug <- function(x) {
  x <- tolower(gsub("<[^>]+>", "", x))
  x <- gsub("[^\\p{L}\\p{N}\\- ]", "", x, perl = TRUE)
  gsub(" ", "-", x, fixed = TRUE)
}
m <- gregexpr("<h([1-6])>(.*?)</h\\1>", body, perl = TRUE)
regmatches(body, m) <- lapply(regmatches(body, m), function(h) {
  lv <- sub("^<h([1-6])>.*$", "\\1", h)
  txt <- sub("^<h[1-6]>(.*)</h[1-6]>$", "\\1", h)
  sprintf('<h%s id="%s">%s</h%s>', lv, slug(txt), txt, lv)
})

# 目錄連結 commonmark 會轉成 %E9…，改回原字和 id 對上
m <- gregexpr('href="#[^"]*"', body)
regmatches(body, m) <- lapply(regmatches(body, m), function(h) {
  enc2utf8(vapply(h, utils::URLdecode, character(1), USE.NAMES = FALSE))
})

css <- "
:root { --fg:#1f2328; --muted:#59636e; --bg:#ffffff; --line:#d1d9e0; --code:#f6f8fa; --accent:#2f6f4e; }
@media (prefers-color-scheme: dark) {
  :root { --fg:#e6e6e6; --muted:#a0a8b0; --bg:#16191d; --line:#3a4048; --code:#22262b; --accent:#7cc39a; }
}
body { margin:0; background:var(--bg); color:var(--fg);
  font: 16px/1.75 'Noto Sans TC','PingFang TC','Microsoft JhengHei',sans-serif; }
main { max-width: 1000px; margin: 0 auto; padding: 24px 16px 80px; }
h1 { font-size: 1.9em; border-bottom: 2px solid var(--accent); padding-bottom: .3em; }
h2 { margin-top: 2.2em; border-bottom: 1px solid var(--line); padding-bottom: .2em; }
h3 { margin-top: 1.8em; }
a { color: var(--accent); }
code { background: var(--code); padding: .1em .35em; border-radius: 4px; font-size: .92em; }
pre { background: var(--code); padding: 12px 14px; border-radius: 6px; overflow-x: auto; }
pre code { padding: 0; background: none; }
table { border-collapse: collapse; margin: 1em 0; display: block; overflow-x: auto; }
th, td { border: 1px solid var(--line); padding: 6px 10px; vertical-align: top; text-align: left; }
th { background: var(--code); white-space: nowrap; }
blockquote { margin: 1em 0; padding: 0 1em; color: var(--muted); border-left: 4px solid var(--line); }
hr { border: 0; border-top: 1px solid var(--line); margin: 2em 0; }
"
html <- c("<!doctype html>", '<html lang="zh-Hant">', "<head>", '<meta charset="utf-8">',
          '<meta name="viewport" content="width=device-width, initial-scale=1">',
          "<title>KIT 題庫管理工具 操作手冊</title>",
          "<style>", css, "</style>", "</head>", "<body>", "<main>", body, "</main>", "</body>", "</html>")
dir.create("inst/manual", showWarnings = FALSE)
writeLines(enc2utf8(html), "inst/manual/manual.html", useBytes = TRUE)
message("wrote inst/manual/manual.html")
