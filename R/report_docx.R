# R/report_docx.R — Word 字型與段落（構面報告用）
# 依賴：officer（flextable 只有 apply_flex_kit_font() 用到）
#
# 段落樣式只設一次（kit_docx_styles()），之後用 body_add_par(style = …)：
# 一段一段用 body_add_fpar() 加，文件愈長愈慢（80 個構面要 10 秒），用樣式不到 1 秒。

# 原文：標楷體
KIT_FONT_CJK <- "\u6a19\u6977\u9ad4"
KIT_FONT_LATIN <- "Times New Roman"
KIT_FONT_SIZE_BODY    <- 12L
KIT_FONT_SIZE_HEADING <- 14L

kit_fp_body <- function() {
  officer::fp_text(font.size = KIT_FONT_SIZE_BODY, font.family = KIT_FONT_LATIN,
                   eastasia.family = KIT_FONT_CJK)
}

kit_fp_heading <- function() {
  officer::fp_text(font.size = KIT_FONT_SIZE_HEADING, font.family = KIT_FONT_LATIN,
                   eastasia.family = KIT_FONT_CJK, bold = TRUE)
}

kit_fpar_plain <- function(text, fp = kit_fp_body()) {
  officer::fpar(officer::ftext(as.character(text), prop = fp))
}

doc_add_body_fpar <- function(doc, fp) {
  officer::body_add_fpar(doc, fp)
}

apply_flex_kit_font <- function(ft, part = "all") {
  ft <- flextable::fontsize(ft, size = KIT_FONT_SIZE_BODY, part = part)
  flextable::font(ft, fontname = KIT_FONT_CJK, part = part)
}

# 報告用的段落樣式：Normal（內文）、kit title（報告標題）、kit heading（構面標題）
KIT_DOCX_TITLE <- "kit title"
KIT_DOCX_HEADING <- "kit heading"

kit_docx_styles <- function(doc) {
  fpt <- function(size, bold = FALSE) {
    officer::fp_text_lite(font.size = size, bold = bold, font.family = KIT_FONT_LATIN,
                          eastasia.family = KIT_FONT_CJK, hansi.family = KIT_FONT_LATIN)
  }
  doc <- officer::docx_set_paragraph_style(doc, "Normal", "Normal",
                                           fp_t = fpt(KIT_FONT_SIZE_BODY))
  doc <- officer::docx_set_paragraph_style(
    doc, "kittitle", KIT_DOCX_TITLE, base_on = "Normal",
    fp_t = fpt(18L, TRUE), fp_p = officer::fp_par(padding.bottom = 6)
  )
  officer::docx_set_paragraph_style(
    doc, "kitheading", KIT_DOCX_HEADING, base_on = "Normal",
    fp_t = fpt(KIT_FONT_SIZE_HEADING, TRUE),
    fp_p = officer::fp_par(padding.top = 14, padding.bottom = 4)
  )
}
