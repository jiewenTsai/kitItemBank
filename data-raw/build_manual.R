# data-raw/build_manual.R — 把 docs/操作手冊.qmd 做成 app 裡的網頁版（inst/manual/）
#
# 用法（在套件根目錄）：
#   LC_ALL=en_US.UTF-8 Rscript --no-init-file data-raw/build_manual.R
# 改了 docs/操作手冊.qmd 就重跑一次。要有 Quarto（RStudio、Positron 都附一份；
# 都沒有時設環境變數 QUARTO_PATH）。產生兩個檔：
#   inst/manual/manual.html          單獨的網頁（「在新分頁開」）：Quarto 加 kitDesign 的主題
#                                    （docs/kit.scss、docs/fonts.html，照 kitDesign「在 Quarto 文件裡用」）
#   inst/manual/manual_fragment.html app 裡「說明 → 操作手冊」分頁的內容（左邊目錄、右邊內文）；
#                                    app 本身已經是 kitDesign 的樣式，這裡只取內文

# 找 Quarto
find_quarto <- function() {
  cand <- c(Sys.getenv("QUARTO_PATH"), Sys.which("quarto"),
            "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto",
            "/Applications/Positron.app/Contents/Resources/app/quarto/bin/quarto",
            "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
            "C:/Program Files/Quarto/bin/quarto.exe")
  cand <- cand[nzchar(cand) & file.exists(cand)]
  if (!length(cand)) stop("Quarto not found: install Quarto or set QUARTO_PATH")
  cand[[1]]
}
quarto <- find_quarto()

# knitr 會在 PATH 上找 pandoc，壞掉的（例如裝錯 CPU 版本的 anaconda pandoc）會讓整個 render 停掉：
# 先用 Quarto 附的 pandoc，PATH 上跑不起來的 pandoc 所在資料夾拿掉
qtools <- list.files(file.path(dirname(quarto), "tools"), pattern = "^pandoc(\\.exe)?$",
                     recursive = TRUE, full.names = TRUE)
env <- character()
if (length(qtools)) env <- c(env, paste0("RSTUDIO_PANDOC=", dirname(qtools[[1]])))
path <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
bad <- vapply(path, function(d) {
  p <- file.path(d, "pandoc")
  file.exists(p) && !identical(suppressWarnings(system2(p, "--version", stdout = FALSE, stderr = FALSE)), 0L)
}, logical(1))
env <- c(env, paste0("PATH=", paste(path[!bad], collapse = .Platform$path.sep)))

# 在暫存資料夾 render（不在 docs/ 留下輸出檔）
# 原文：操作手冊.qmd
qmd <- "\u64cd\u4f5c\u624b\u518a.qmd"
tmp <- tempfile("manual")
dir.create(tmp)
invisible(file.copy(file.path("docs", c(qmd, "kit.scss", "fonts.html")), tmp))
old <- setwd(tmp)
st <- system2(quarto, c("render", shQuote(qmd), "--to", "html"), env = env)
setwd(old)
out <- file.path(tmp, sub("\\.qmd$", ".html", qmd))
if (!identical(st, 0L) || !file.exists(out)) stop("quarto render failed")

dir.create("inst/manual", showWarnings = FALSE)
invisible(file.copy(out, "inst/manual/manual.html", overwrite = TRUE))
message("wrote inst/manual/manual.html")

# app 分頁用：Quarto 輸出的 <main> 內文，左邊目錄（h2、h3）
html <- paste(readLines(out, encoding = "UTF-8", warn = FALSE), collapse = "\n")
body <- regmatches(html, regexpr('(?s)<main class="content" id="quarto-document-content">.*?</main>',
                                 html, perl = TRUE))
stopifnot(length(body) == 1L)
body <- sub('(?s)^<main[^>]*>(.*)</main>$', "\\1", body, perl = TRUE)
hs <- regmatches(body, gregexpr('<h([23]) class="anchored" data-anchor-id="([^"]*)">(.*?)</h\\1>',
                                body, perl = TRUE))[[1]]
toc <- vapply(hs, function(h) {
  lv <- sub('^<h([23]).*$', "\\1", h)
  id <- sub('^.*data-anchor-id="([^"]*)".*$', "\\1", h)
  txt <- gsub("<[^>]+>", "", sub('^<h[23][^>]*>(.*)</h[23]>$', "\\1", h))
  sprintf('<a class="km-l%s" href="#%s">%s</a>', lv, id, txt)
}, character(1), USE.NAMES = FALSE)
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
.kit-manual h2 { margin-top: 2em; scroll-margin-top: 1rem; }
.kit-manual h3 { margin-top: 1.6em; scroll-margin-top: 1rem; }
.kit-manual table { display: block; overflow-x: auto; }
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
          "<main>", body, "</main>", "</div>", "</div>",
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
