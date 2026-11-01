# R/import_usage.R — 步驟 1.2：研究設計和題本表（世代、施測、資料檔、題本、來源、施測別名）
#
# 手工整理的 CSV 放在 data-raw/kit_manual/；這裡的函式負責：
#   import_usage_doc()：從資料使用說明（純文字）抓得準的欄位，用來填寫或核對 CSV；
#   v02_wave_map()：0.2 題庫每個波次欄 → 施測、資料檔、題本；
#   import_design()：讀 CSV，補上 0.2 才有的 internal 資料檔和別名，回傳 kit 的各表。

# 中文數字（第一波…第十三波）
# 原文：一 二 三 四 五 六 七 八 九 十
ZH_NUM <- c("\u4e00", "\u4e8c", "\u4e09", "\u56db", "\u4e94", "\u516d", "\u4e03",
            # 原文：八 | 九 | 十
            "\u516b", "\u4e5d", "\u5341")

#' 「七」→ 7、「十一」→ 11、「十」→ 10
#' @noRd
zh_num_to_int <- function(x) {
  vapply(x, function(s) {
    ch <- strsplit(s, "")[[1]]
    if (!length(ch) || !all(ch %in% ZH_NUM)) return(NA_integer_)
    v <- match(ch, ZH_NUM)
    if (length(v) == 1L) return(as.integer(v))
    # 十X、X十、X十Y
    if (v[1] == 10L) return(as.integer(10L + if (length(v) > 1L) v[2] else 0L))
    as.integer(v[1] * 10L + if (length(v) > 2L) v[3] else 0L)
  }, integer(1), USE.NAMES = FALSE)
}

#' 施測代碼：世代 + M + 三位數月齡（84 月齡起再加 G + 兩位數年級）
#' @noRd
make_admin_id <- function(cohort, age, grade = NA_integer_) {
  age <- as.integer(age)
  grade <- ifelse(is.na(grade) & age >= 84L, (age - 84L) %/% 12L + 1L, grade)
  paste0(cohort, "M", sprintf("%03d", age),
         ifelse(is.na(grade), "", sprintf("G%02d", as.integer(grade))))
}

#' Extract the reliably parseable facts from a KIT usage document
#'
#' Reads a 資料使用說明 converted to plain text (pandoc) and returns what can be
#' pulled out without judgement: title, cohort, age, wave number, survey period,
#' release code, DOI, citations, the data files and the special (missing) codes.
#' Use it to fill or verify the hand-curated CSVs in `data-raw/kit_manual/`.
#'
#' @param txt_file Path to the text version of one usage document.
#' @return A list: `file`, `title`, `cohort` (`"M03"`/`"M36"`), `ageMonth`, `waveNo`,
#'   `adminID`, `surveyFrom`, `surveyTo`, `survey_raw`, `release`, `doi`, `citation_zh`,
#'   `citation_en`, `data_files` (data frame `file`, `desc`), `missing_codes`
#'   (data frame `code`, `label`). Fields that cannot be found are `NA`.
#' @export
import_usage_doc <- function(txt_file) {
  x <- readLines(txt_file, encoding = "UTF-8", warn = FALSE)
  x <- enc2utf8(x)
  first <- function(pat, v = x) {
    hit <- grep(pat, v, perl = TRUE, value = TRUE)
    if (length(hit)) hit[1] else NA_character_
  }
  title <- trimws(x[nzchar(trimws(x))][1])

  # 標題：…：3月齡組第七波48月齡
  # 原文：([0-9]+)月齡組第([一二…十]+)波([0-9]+)月齡
  m <- regmatches(title, regexec(paste0("([0-9]+)\u6708\u9f61\u7d44\u7b2c([",
                                        paste(ZH_NUM, collapse = ""),
                                        # 原文：]+)波([0-9]+)月齡
                                        "]+)\u6ce2([0-9]+)\u6708\u9f61"), title, perl = TRUE))[[1]]
  cohort <- if (length(m)) sprintf("M%02d", as.integer(m[2])) else NA_character_
  wave_no <- if (length(m)) zh_num_to_int(m[3]) else NA_integer_
  age <- if (length(m)) as.integer(m[4]) else NA_integer_

  # 原文：調查執行期間
  sv <- first("\u8abf\u67e5\u57f7\u884c\u671f\u9593")
  dates <- if (is.na(sv)) character(0) else
    regmatches(sv, gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", sv))[[1]]
  survey_raw <- if (is.na(sv)) NA_character_ else
    # 原文：調查執行期間
    trimws(gsub("\\|", "", sub(".*\u8abf\u67e5\u57f7\u884c\u671f\u9593", "", sv)))

  # 資料檔：表格裡的 data1.csv、IRT.csv 那幾列（同一檔的 .sav/.dta 不重複列）
  rows <- grep("^\\|\\s+(data[0-9]+|IRT)\\.csv\\s+\\|", x, perl = TRUE, value = TRUE)
  files <- data.frame(
    file = sub("^\\|\\s+(data[0-9]+|IRT)\\.csv.*$", "\\1", rows, perl = TRUE),
    desc = trimws(sub("^\\|[^|]*\\|([^|]*)\\|.*$", "\\1", rows, perl = TRUE)),
    stringsAsFactors = FALSE)
  # 原文：CSV資料檔 → 資料檔
  files$desc <- sub("CSV\u8cc7\u6599\u6a94$", "\u8cc7\u6599\u6a94", files$desc)

  # 原文：（D00250）
  rel_line <- first("\uff08D[0-9]{5}\uff09")
  release <- if (is.na(rel_line)) NA_character_ else
    # 原文：.*（(D[0-9]{5})）.*
    sub(".*\uff08(D[0-9]{5})\uff09.*", "\\1", rel_line, perl = TRUE)
  doi_line <- first("https://doi\\.org/")
  doi <- if (is.na(doi_line)) NA_character_ else
    sub(".*(https://doi\\.org/\\S+).*", "\\1", doi_line, perl = TRUE)
  # 原文：張鑑如（
  cit_zh <- trimws(first("^\\s*\u5f35\u9451\u5982\uff08"))
  cit_en <- trimws(first("^\\s*Chien-Ju Chang\\."))

  # 原文：特殊編碼 ……7777（拒答）、8888（不知道）……
  mc <- first("\u7279\u6b8a\u7de8\u78bc")
  codes <- data.frame(code = character(0), label = character(0), stringsAsFactors = FALSE)
  if (!is.na(mc)) {
    # 只看「請注意」之前的部分，免得把說明裡的舊代碼也抓進來
    # 原文：請注意
    mc <- sub("\u8acb\u6ce8\u610f.*$", "", mc)
    # 原文：([0-9]{4})（([^）]+)）
    hit <- regmatches(mc, gregexpr("([0-9]{4})\uff08([^\uff09]+)\uff09", mc, perl = TRUE))[[1]]
    codes <- data.frame(code = substr(hit, 1L, 4L),
                        # 原文：^[0-9]{4}（(.*)）$
                        label = sub("^[0-9]{4}\uff08(.*)\uff09$", "\\1", hit, perl = TRUE),
                        stringsAsFactors = FALSE)
  }

  list(file = basename(txt_file), title = title, cohort = cohort, ageMonth = age,
       waveNo = wave_no,
       adminID = if (is.na(cohort)) NA_character_ else make_admin_id(cohort, age),
       surveyFrom = if (length(dates)) min(dates) else NA_character_,
       surveyTo = if (length(dates)) max(dates) else NA_character_,
       survey_raw = survey_raw, release = release, doi = doi,
       citation_zh = cit_zh, citation_en = cit_en,
       data_files = files, missing_codes = codes)
}

# 0.2 題庫 ----

#' 0.2 題庫的三個領域 → 問卷大類、填答者
#' @noRd
V02_DOMAIN_INSTRUMENT <- c(parent = "P", childcare_teacher = "T", child_adolescent = "C")

#' 題本：問卷大類 × 月齡（或年級）
#' @noRd
form_for <- function(instrument, age, grade) {
  instrument <- rep_len(instrument, length(age))
  stage <- ifelse(is.na(grade), "", ifelse(grade <= 3L, "G1-G3",
                                    ifelse(grade <= 6L, "G4-G6", "G7-G9")))
  pre <- ifelse(instrument == "P", ifelse(age <= 24L, "P-00-24", "P-36-72"),
                ifelse(instrument == "T", "T-presch", NA_character_))
  ifelse(nzchar(stage), paste0(instrument, "-", stage), pre)
}

#' 解析一個 0.2 波次欄名
#'
#' 規則（沒有世代字尾的欄位要靠假設，也寫在 admins.csv 的 note）：
#'   M036大 → M36、M036小 → M03；
#'   沒有大／小的（教保、教師欄）：36 月齡以前 → M03，36 月齡起 → M36；
#'   小2_小月齡 → M03 國小二年級；國1_大月齡 → M36 七年級；
#'   年級 g 的月齡 = 84 + 12 (g - 1)。
#' @noRd
parse_v02_wave <- function(col) {
  # 原文：^M([0-9]{3})(大|小)?(保母|機構)?(b)?$
  re_m <- "^M([0-9]{3})(\u5927|\u5c0f)?(\u4fdd\u6bcd|\u6a5f\u69cb)?(b)?$"
  # 原文：^(小|國)([1-9])_(大|小)月齡$
  re_g <- "^(\u5c0f|\u570b)([1-9])_(\u5927|\u5c0f)\u6708\u9f61$"
  out <- data.frame(wave_col = col, cohort = NA_character_, ageMonth = NA_integer_,
                    grade = NA_integer_, setting = "", stringsAsFactors = FALSE)
  # 原文：大 | 小
  coh <- function(s) ifelse(s == "\u5927", "M36", ifelse(s == "\u5c0f", "M03", NA_character_))
  for (i in seq_along(col)) {
    m <- regmatches(col[i], regexec(re_m, col[i], perl = TRUE))[[1]]
    if (length(m)) {
      age <- as.integer(m[2])
      out$ageMonth[i] <- age
      out$cohort[i] <- if (nzchar(m[3])) coh(m[3]) else if (age < 36L) "M03" else "M36"
      out$grade[i] <- if (age >= 84L) (age - 84L) %/% 12L + 1L else NA_integer_
      out$setting[i] <- paste0(m[4], m[5])
      next
    }
    g <- regmatches(col[i], regexec(re_g, col[i], perl = TRUE))[[1]]
    if (length(g)) {
      # 原文：國
      grade <- as.integer(g[3]) + if (g[2] == "\u570b") 6L else 0L
      out$grade[i] <- grade
      out$ageMonth[i] <- 84L + 12L * (grade - 1L)
      out$cohort[i] <- coh(g[4])
    }
  }
  out
}

#' 已有官方資料使用說明的 12 次施測
#' @noRd
KIT_RELEASED_ADMINS <- c("M03M003", "M03M006", "M03M012", "M03M018", "M03M024",
                         "M03M036", "M03M048", "M36M036", "M36M048", "M36M060",
                         "M36M072", "M36M084G01")

#' Map every 0.2 wave column to an administration, dataset and form
#'
#' @param dir The 0.2 bank folder (holding `parent/`, `childcare_teacher/`,
#'   `child_adolescent/`, each with `itemWave.csv`).
#' @return A data frame with `domain`, `wave_col`, `adminID`, `datasetID`,
#'   `instrument`, `formID`, `status` (`released` for the 12 documented
#'   administrations, `internal` otherwise). Several wave columns can share one
#'   dataset (e.g. `M012保母` and `M012機構` are one childcare form).
#' @export
v02_wave_map <- function(dir = bank_dir()) {
  out <- lapply(names(V02_DOMAIN_INSTRUMENT), function(dm) {
    f <- file.path(dir, dm, "itemWave.csv")
    if (!file.exists(f)) return(NULL)
    hdr <- names(readr::read_csv(f, n_max = 0L, show_col_types = FALSE,
                                 locale = readr::locale(encoding = "UTF-8"),
                                 col_types = readr::cols(.default = "c")))
    cols <- setdiff(hdr, BANK_KEYS)
    p <- parse_v02_wave(cols)
    if (anyNA(p$cohort)) {
      stop("Cannot parse wave column(s): ", paste(p$wave_col[is.na(p$cohort)], collapse = ", "))
    }
    ins <- V02_DOMAIN_INSTRUMENT[[dm]]
    admin <- make_admin_id(p$cohort, p$ageMonth, p$grade)
    data.frame(domain = dm, wave_col = cols, adminID = admin,
               datasetID = paste0(admin, "-", ins), instrument = ins,
               formID = form_for(ins, p$ageMonth, p$grade),
               status = ifelse(admin %in% KIT_RELEASED_ADMINS, "released", "internal"),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# 組裝 ----

#' 讀一張手工 CSV（全部文字），整理成 schema 的欄位
#' @noRd
read_manual_csv <- function(dir, table, file = paste0(table, ".csv")) {
  f <- file.path(dir, file)
  if (!file.exists(f)) return(empty_kit_table(table))
  df <- as.data.frame(readr::read_csv(
    f, locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE,
    na = character(0), col_types = readr::cols(.default = readr::col_character())))
  extra <- setdiff(names(df), names(KIT_SCHEMA[[table]]$cols))
  if (length(extra)) stop(file, ": unknown column(s) ", paste(extra, collapse = ", "))
  conform_kit_table(df, table)
}

#' Build the research-design and instrument tables of the kit
#'
#' Reads the hand-curated CSVs, adds a placeholder dataset (`file = "v02"`) for every
#' wave column of the 0.2 bank whose administration has no official usage document
#' yet, and generates admin aliases: each 0.2 wave column, release-package codes
#' (`M36W84`), release IDs (`D00250`), plus the spoken forms in `aliases.csv`.
#'
#' @param manual_dir Folder with `cohorts.csv`, `admins.csv`, `datasets.csv`,
#'   `forms.csv`, `form_notes.csv`, `missing_schemes.csv`, `sources.csv`, `aliases.csv`.
#' @param bank The 0.2 bank folder used by [v02_wave_map()]; `NULL` skips it.
#' @return A list of data frames: `cohorts`, `admins`, `datasets`, `forms`,
#'   `form_notes`, `missing_schemes`, `sources`, `aliases`, and `wave_map`
#'   (not a kit table).
#' @export
import_design <- function(manual_dir, bank = bank_dir()) {
  tabs <- c("cohorts", "admins", "datasets", "forms", "form_notes",
            "missing_schemes", "sources", "aliases")
  out <- stats::setNames(lapply(tabs, function(tb) read_manual_csv(manual_dir, tb)), tabs)

  wm <- if (is.null(bank)) NULL else v02_wave_map(bank)
  if (!is.null(wm)) {
    miss <- setdiff(wm$adminID, out$admins$adminID)
    if (length(miss)) stop("admins.csv lacks 0.2 administration(s): ",
                           paste(unique(miss), collapse = ", "))
    # 0.2 才有的資料檔：先放 file = "v02"，等官方文件出來再換成 data1 等
    new_ds <- unique(wm[!(wm$datasetID %in% out$datasets$datasetID),
                        c("datasetID", "adminID", "instrument", "formID", "domain")])
    if (nrow(new_ds)) {
      rel <- new_ds$adminID %in% out$admins$adminID[out$admins$status == "released"]
      if (any(rel)) stop("released dataset(s) missing from datasets.csv: ",
                         paste(new_ds$datasetID[rel], collapse = ", "))
      # 原文：家長 | 教師 | 兒童
      resp <- c(P = "\u5bb6\u9577", T = "\u6559\u5e2b", C = "\u5152\u7ae5")
      cols <- tapply(wm$wave_col, wm$datasetID, paste, collapse = ", ")
      adm <- out$admins[match(new_ds$adminID, out$admins$adminID), ]
      out$datasets <- rbind(out$datasets, conform_kit_table(data.frame(
        datasetID = new_ds$datasetID, adminID = new_ds$adminID, file = "v02",
        instrument = new_ds$instrument, formID = new_ds$formID,
        respondent = unname(resp[new_ds$instrument]),
        missingScheme = ifelse(adm$surveyFrom != "" & adm$surveyFrom < "2020-03",
                               "2016", "2020"),
        note = paste0("v02 placeholder: from 0.2 bank ", new_ds$domain, " column(s) ",
                      cols[new_ds$datasetID], "; scheme assumed"),
        stringsAsFactors = FALSE), "datasets"))
    }
  }

  # 自動產生的別名：0.2 波次欄、釋出包代碼（M36W84）、釋出編號（D00250）
  adm <- out$admins
  gen <- list()
  if (!is.null(wm)) {
    gen$v02 <- data.frame(alias = wm$wave_col, target_type = "admin",
                          target_id = wm$adminID, kind = "v02_wave")
  }
  gen$pkg <- data.frame(alias = paste0(substr(adm$adminID, 1L, 3L), "W",
                                       sprintf("%02d", as.integer(substr(adm$adminID, 5L, 7L)))),
                        target_type = "admin", target_id = adm$adminID, kind = "release_package")
  has_g <- nchar(adm$adminID) > 7L
  gen$nog <- data.frame(alias = substr(adm$adminID[has_g], 1L, 7L), target_type = "admin",
                        target_id = adm$adminID[has_g], kind = "admin_without_grade")
  rel <- nzchar(adm$datasetRelease)
  gen$rel <- data.frame(alias = adm$datasetRelease[rel], target_type = "admin",
                        target_id = adm$adminID[rel], kind = "release_id")
  gen <- conform_kit_table(do.call(rbind, gen), "aliases")
  al <- rbind(out$aliases, gen)
  al <- al[!duplicated(kit_key_str(al, KIT_SCHEMA$aliases$key)), , drop = FALSE]
  rownames(al) <- NULL
  out$aliases <- al

  out$wave_map <- wm
  out
}
