# R/schema_concepts.R — 0.3.7 題組整併第二步：概念層、官方文件紀錄、文獻
#
# 只往 KIT_SCHEMA、KIT_ENUMS 加表（R/schema.R 原來的表不動）：
#   concepts     概念：一個測量概念一列（例如「社會能力」），家長、教師、兒童三份問卷的題組都接到同一個概念；
#                領域（domain）照官方《問卷構面及信效度》：認知發展、語言發展、社會情緒發展、身體動作發展、家庭……
#                題組用 groups.construct 指到 conceptID。
#   group_docs   題組在官方《問卷構面及信效度》文件裡的寫法：文件、題本、官方名稱（中英）、信效度、題目
#   refs         文獻：量表出處、中文版修訂、KIT 自己的編製驗證、效標工具
#   group_refs   題組用到哪篇文獻（角色：出處、修訂、KIT 驗證、效標）

KIT_ENUMS$ref_role <- c("source", "adaptation", "kit_validation", "criterion")
KIT_ENUMS$ref_status <- c("verified", "not_in_pubmed", "likely")

KIT_SCHEMA[["concepts"]] <- list(
  cols = list(conceptID = kcol(req = TRUE), label = kcol(req = TRUE), label_en = kcol(),
              domain = kcol(), parentID = kcol(), definition = kcol(), alias = kcol(),
              note = kcol()),
  key = "conceptID",
  fk = list(list(cols = "parentID", table = "concepts")))

KIT_SCHEMA$groups$fk <- c(KIT_SCHEMA$groups$fk %||% list(),
                          list(list(cols = "construct", table = "concepts")))

# doc 是文件名稱的簡稱（3-24、36-72、G1-G3）；items 是文件列的題目（變項名，空白分隔）
KIT_SCHEMA[["group_docs"]] <- list(
  cols = list(groupID = kcol(req = TRUE), formID = kcol(req = TRUE), doc = kcol(req = TRUE),
              official_label = kcol(), official_en = kcol(), reliability = kcol(),
              items = kcol(), note = kcol()),
  key = c("groupID", "formID", "doc"),
  fk = list(list(cols = "groupID", table = "groups"),
            list(cols = "formID", table = "forms")))

KIT_SCHEMA[["refs"]] <- list(
  cols = list(refID = kcol(req = TRUE), citation = kcol(req = TRUE), year = kcol("int"),
              doi = kcol(), pmid = kcol(), status = kcol(enum = "ref_status"), note = kcol()),
  key = "refID")

KIT_SCHEMA[["group_refs"]] <- list(
  cols = list(groupID = kcol(req = TRUE), refID = kcol(req = TRUE),
              role = kcol(enum = "ref_role", req = TRUE), evidence = kcol()),
  key = c("groupID", "refID", "role"),
  fk = list(list(cols = "groupID", table = "groups"),
            list(cols = "refID", table = "refs")))
