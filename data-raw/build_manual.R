# data-raw/build_manual.R — 把 docs/操作手冊.md 轉成 app 裡的網頁版（inst/manual/manual.html）
#
# 用法（在套件根目錄）：
#   LC_ALL=en_US.UTF-8 Rscript --no-init-file data-raw/build_manual.R
# 改了 docs/操作手冊.md 就重跑一次。產生兩個檔：
#   inst/manual/manual.html          單獨的網頁（「在新分頁開」）
#   inst/manual/manual_fragment.html app 裡「說明 → 操作手冊」分頁的內容（左邊目錄、右邊內文）

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

# app 分頁用：左邊目錄（h2、h3），右邊內文（拿掉手冊自己的「目錄」那一節）
hs <- regmatches(body, gregexpr('<h([23]) id="([^"]*)">(.*?)</h\\1>', body, perl = TRUE))[[1]]
toc <- vapply(hs, function(h) {
  lv <- sub('^<h([23]).*$', "\\1", h)
  id <- sub('^<h[23] id="([^"]*)".*$', "\\1", h)
  txt <- gsub("<[^>]+>", "", sub('^<h[23][^>]*>(.*)</h[23]>$', "\\1", h))
  sprintf('<a class="km-l%s" href="#%s">%s</a>', lv, id, txt)
}, character(1), USE.NAMES = FALSE)
toc <- toc[!grepl('href="#\u76ee\u9304"', toc)]
frag_body <- sub('(?s)<h2 id="\u76ee\u9304">.*?<hr />', "", body, perl = TRUE)
frag_css <- "
.kit-manual .km-wrap { display: grid; grid-template-columns: 14rem minmax(0, 1fr); gap: 0 2.5rem; }
.kit-manual .km-toc { position: sticky; top: 1rem; align-self: start; font-size: .9rem;
  max-height: calc(100vh - 2rem); overflow-y: auto; padding-top: .5rem; }
.kit-manual .km-toc a { display: block; padding: .15rem 0 .15rem .6rem; color: inherit; opacity: .8;
  text-decoration: none; border-left: 2px solid transparent; line-height: 1.5; }
.kit-manual .km-toc a:hover { opacity: 1; border-left-color: currentColor; }
.kit-manual .km-toc a.km-l3 { padding-left: 1.4rem; font-size: .85rem; }
.kit-manual main { min-width: 0; line-height: 1.75; max-width: 52rem; }
.kit-manual h1 { font-size: 1.6rem; margin-top: .25rem; }
.kit-manual h2 { margin-top: 2em; border-bottom: 1px solid rgba(127,127,127,.35); padding-bottom: .2em; scroll-margin-top: 1rem; }
.kit-manual h3 { margin-top: 1.6em; scroll-margin-top: 1rem; }
.kit-manual table { border-collapse: collapse; margin: 1em 0; display: block; overflow-x: auto; }
.kit-manual th, .kit-manual td { border: 1px solid rgba(127,127,127,.35); padding: 6px 10px; vertical-align: top; }
.kit-manual th { white-space: nowrap; }
.kit-manual pre { padding: 12px 14px; border-radius: 6px; overflow-x: auto; background: rgba(127,127,127,.1); }
.kit-manual blockquote { margin: 1em 0; padding: 0 1em; border-left: 4px solid rgba(127,127,127,.35); opacity: .85; }
@media (max-width: 768px) {
  .kit-manual .km-wrap { grid-template-columns: 1fr; }
  .kit-manual .km-toc { position: static; max-height: none; border-bottom: 1px solid rgba(127,127,127,.35);
    padding-bottom: .75rem; margin-bottom: .5rem; }
}
"
frag <- c('<div class="kit-manual">', "<style>", frag_css, "</style>",
          # 原文：在新分頁開（方便列印或另存）
          '<p class="kit-hint"><a href="kit-manual/manual.html" target="_blank" rel="noopener">\u5728\u65b0\u5206\u9801\u958b\uff08\u65b9\u4fbf\u5217\u5370\u6216\u53e6\u5b58\uff09</a></p>',
          '<div class="km-wrap">', '<nav class="km-toc">', toc, "</nav>",
          "<main>", frag_body, "</main>", "</div>", "</div>",
          # 頁內連結：在 app 裡直接捲到那一節，不改網址（網址留給書籤）
          "<script>",
          "document.addEventListener('click', function (e) {",
          "  var a = e.target.closest('.kit-manual a[href^=\"#\"]');",
          "  if (!a) return;",
          "  var el = document.getElementById(decodeURIComponent(a.getAttribute('href').slice(1)));",
          "  if (!el) return;",
          "  e.preventDefault();",
          "  el.scrollIntoView({ block: 'start' });",
          "});",
          "</script>")
writeLines(enc2utf8(frag), "inst/manual/manual_fragment.html", useBytes = TRUE)
message("wrote inst/manual/manual_fragment.html")
