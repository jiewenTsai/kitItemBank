# 把 R/ 裡字串的中文換成 \uXXXX（R CMD check 要求 R 程式碼只用 ASCII，註解除外）
#
# 用法（在套件根目錄）：
#   LC_ALL=en_US.UTF-8 Rscript tools/escape_non_ascii.R
#
# 寫程式時字串直接寫中文；改完跑一次這個檔：
# - 字串裡的非 ASCII 字換成 \uXXXX（超過 FFFF 的換成 \U{XXXXX}）；
# - 在那一行的上面加一行註解，寫出原文（「# 原文：…」），讀程式時看得懂。
# 已經換過的字串沒有非 ASCII 字，再跑一次不會改變任何東西。
# 符號（變數名稱、欄名）不能有中文；有的話這裡會停下來。

escape_string <- function(text) {
  cps <- utf8ToInt(text)
  out <- vapply(cps, function(cp) {
    if (cp < 128) intToUtf8(cp)
    else if (cp <= 0xFFFF) sprintf("\\u%04x", cp)
    else sprintf("\\U{%x}", cp)
  }, character(1))
  paste(out, collapse = "")
}

escape_file <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  exprs <- parse(text = lines, keep.source = TRUE, srcfile = srcfilecopy(path, lines))
  pd <- utils::getParseData(exprs, includeText = TRUE)
  pd <- pd[pd$terminal, ]
  bad_sym <- pd[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB") &
                  grepl("[^\\x01-\\x7F]", pd$text, perl = TRUE), ]
  if (nrow(bad_sym) > 0) {
    stop(path, ": non-ASCII symbols at lines ", paste(bad_sym$line1, collapse = ", "))
  }
  strs <- pd[pd$token == "STR_CONST" & grepl("[^\\x01-\\x7F]", pd$text, perl = TRUE), ]
  if (nrow(strs) == 0) return(invisible(FALSE))
  if (any(grepl("^[rR]", strs$text))) stop(path, ": raw strings with non-ASCII text")
  # 從後面往前換，前面的欄位位置才不會跑掉
  strs <- strs[order(-strs$line1, -strs$col1), ]
  notes <- list()
  for (i in seq_len(nrow(strs))) {
    s <- strs[i, ]
    if (s$line1 != s$line2) stop(path, ": multi-line string with non-ASCII text at line ", s$line1)
    ln <- lines[s$line1]
    chars <- strsplit(ln, "")[[1]]
    # getParseData 的欄位是字元位置（UTF-8 下一個中文字算一欄）
    before <- paste(chars[seq_len(s$col1 - 1)], collapse = "")
    after <- paste(chars[-seq_len(s$col2)], collapse = "")
    tok <- paste(chars[s$col1:s$col2], collapse = "")
    lines[s$line1] <- paste0(before, escape_string(tok), after)
    key <- as.character(s$line1)
    notes[[key]] <- c(substr(tok, 2, nchar(tok) - 1), notes[[key]])
  }
  # 原文的註解放在那一行上面（從下面往上插，行號才不會跑掉）
  for (key in sort(as.integer(names(notes)), decreasing = TRUE)) {
    indent <- sub("^(\\s*).*$", "\\1", lines[key])
    note <- paste0(indent, "# 原文：", paste(notes[[as.character(key)]], collapse = " | "))
    lines <- append(lines, note, after = key - 1)
  }
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  parse(path, encoding = "UTF-8")
  invisible(TRUE)
}

files <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
changed <- vapply(files, escape_file, logical(1))
cat("escaped:", if (any(changed)) paste(basename(files[changed]), collapse = ", ") else "none", "\n")
