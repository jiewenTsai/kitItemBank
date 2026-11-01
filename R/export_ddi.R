# R/export_ddi.R — 一次施測（或其中幾個資料檔）的 DDI-Codebook 2.5 XML
#
# codeBook
#   stdyDscr  標題、題庫版本、引用、調查期間
#   fileDscr  每個資料檔一個（檔名、填答者、變項數）
#   dataDscr  每個變項一個 var：name、labl、qstn/qstnLit、catgry（選項；遺漏代碼 missing="Y"）
# 變項照 admin_codebook() 的順序；預設只放公開檔有的變項。
# 另有 admin_export_buttons()：施測頁可以放的兩個下載鈕（datapackage.json、DDI XML）。

DDI_NS <- "ddi:codebook:2_5"

# XML 的 ID 只能有英數、底線、點、減號，不能數字開頭
ddi_id <- function(prefix, x) {
  paste0(prefix, gsub("[^A-Za-z0-9_.-]", "_", x))
}

#' DDI-Codebook 2.5 for one administration
#'
#' @param kit A kit, from [read_kit()].
#' @param admin An adminID or alias (see [resolve_admin()]).
#' @param datasetID Only these data files of the administration; `NULL` = all.
#' @param public_only Only variables in the public release files (default), as in the
#'   SPSS labels.
#' @param version Bank version written to the study description.
#' @return An `xml2` document; save it with `xml2::write_xml(doc, "file.xml")`.
#' @examples
#' doc <- ddi_codebook(read_kit(), "M36W84")
#' @export
ddi_codebook <- function(kit, admin, datasetID = NULL, public_only = TRUE,
                         version = codebook_bank_version()) {
  a <- resolve_admin(kit, admin)
  if (length(a) != 1L) stop("Unknown or ambiguous admin: ", admin, call. = FALSE)
  cb <- admin_codebook(kit, a, datasetID)
  ad <- cb$admin
  ds <- cb$datasets
  v <- cb$vars
  if (public_only) v <- v[v$public, , drop = FALSE]

  doc <- xml2::xml_new_root("codeBook", xmlns = DDI_NS, version = "2.5",
                            ID = ddi_id("KIT_", a))
  # 引數名加點：var 的屬性 name = … 才不會被當成元素名稱
  add <- function(.parent, .el, .text = NULL, ...) {
    node <- xml2::xml_add_child(.parent, .el, ...)
    if (!is.null(.text)) xml2::xml_text(node) <- enc2utf8(as.character(.text))
    node
  }

  # 研究描述
  sd <- add(doc, "stdyDscr")
  cit <- add(sd, "citation")
  ts <- add(cit, "titlStmt")
  add(ts, "titl", admin_label(kit, a))
  add(ts, "IDNo", a, agency = "KIT")
  if (nzchar(cb_blank(ad$datasetRelease))) add(ts, "IDNo", ad$datasetRelease, agency = "SRDA")
  vs <- add(cit, "verStmt")
  add(vs, "version", version, type = "item bank")
  for (lang in c("zh", "en")) {
    txt <- cb_blank(ad[[paste0("citation_", lang)]])
    if (nzchar(txt)) add(cit, "biblCit", txt, `xml:lang` = lang)
  }
  si <- add(sd, "stdyInfo")
  sum <- add(si, "sumDscr")
  if (nzchar(cb_blank(ad$surveyFrom))) add(sum, "collDate", date = ad$surveyFrom, event = "start")
  if (nzchar(cb_blank(ad$surveyTo))) add(sum, "collDate", date = ad$surveyTo, event = "end")

  # 資料檔
  fid <- stats::setNames(ddi_id("F_", ds$datasetID), ds$datasetID)
  for (i in seq_len(nrow(ds))) {
    fd <- add(doc, "fileDscr", ID = fid[[i]])
    ft <- add(fd, "fileTxt")
    add(ft, "fileName", ds$file[i])
    add(ft, "fileCont", ds$dataset_label[i])
    dm <- add(ft, "dimensns")
    add(dm, "varQnty", sum(v$datasetID == ds$datasetID[i]))
  }

  # 變項
  dd <- add(doc, "dataDscr")
  iv <- kit$item_versions
  stem <- iv$stem[match(paste(v$item_uid, v$itemVer), paste(iv$item_uid, iv$itemVer))]
  stem <- cb_blank(stem)
  vc <- kit$value_codes
  vc <- vc[order(vc$valueSetID, cb_int(vc$order), suppressWarnings(as.numeric(vc$code)),
                 vc$code), , drop = FALSE]
  scheme <- ds$missingScheme[match(v$datasetID, ds$datasetID)]
  for (i in seq_len(nrow(v))) {
    var <- add(dd, "var", ID = ddi_id("V_", paste(v$datasetID[i], v$varname[i], sep = "_")),
               name = v$varname[i], files = fid[[v$datasetID[i]]])
    lab <- cb_one_line(v$label[i])
    if (nzchar(lab)) add(var, "labl", lab)
    if (nzchar(lab) || nzchar(stem[i])) {
      q <- add(var, "qstn")
      add(q, "qstnLit", trimws(paste(stem[i], v$label[i])))
    }
    codes <- vc[vc$valueSetID == v$valueSetID[i] & nzchar(v$valueSetID[i]), , drop = FALSE]
    for (k in seq_len(nrow(codes))) {
      cg <- add(var, "catgry")
      add(cg, "catValu", codes$code[k])
      if (nzchar(codes$label[k])) add(cg, "labl", codes$label[k])
    }
    ms <- cb_scheme_rows(kit, cb_blank(scheme[i]))
    for (k in seq_len(nrow(ms))) {
      cg <- add(var, "catgry", missing = "Y")
      add(cg, "catValu", ms$code[k])
      if (nzchar(ms$label[k])) add(cg, "labl", ms$label[k])
    }
  }
  doc
}

#' Download buttons for the administration page: datapackage.json and DDI XML
#'
#' For `page_admin.R`'s `output$downloads`: put the result inside the existing
#' `kit-buttons` div next to the Excel and SPSS buttons, e.g.
#' `admin_export_buttons(session, k, a, d)`.
#'
#' @param session The Shiny session (of the page module).
#' @param kit The kit shown on the page.
#' @param adminID The selected administration.
#' @param datasetID The selected data file; `NULL` = the whole administration in the DDI.
#' @return A `tagList` of two download buttons.
#' @export
admin_export_buttons <- function(session, kit, adminID, datasetID = NULL) {
  day <- format(kit_today(), "%Y%m%d")
  cls <- kitDesign::kit_btn_class("secondary")
  shiny::tagList(
    kitDesign::kit_download_button(
      # 原文：下載 DDI 編碼簿（XML）
      session, "dl_ddi", "\u4e0b\u8f09 DDI \u7de8\u78bc\u7c3f\uff08XML\uff09",
      paste0("DDI_", adminID, if (length(datasetID)) paste0("_", datasetID[1]) else "", "_", day, ".xml"),
      function(file) xml2::write_xml(ddi_codebook(kit, adminID, datasetID), file),
      class = cls
    ),
    kitDesign::kit_download_button(
      # 原文：下載題庫結構（datapackage.json）
      session, "dl_datapackage", "\u4e0b\u8f09\u984c\u5eab\u7d50\u69cb\uff08datapackage.json\uff09",
      "datapackage.json",
      function(file) writeLines(enc2utf8(datapackage_json()), file, useBytes = TRUE),
      class = cls
    )
  )
}
