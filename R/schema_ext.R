# R/schema_ext.R — 0.3.7 加到 KIT_SCHEMA 的表（不改 R/schema.R 裡原來的 17 張表）
#
# R 檔照字母順序讀進來，schema_ext.R 排在 schema.R 後面，所以這裡可以直接往
# KIT_SCHEMA、KIT_ENUMS 加東西；read_kit()、write_kit()、check_kit()、empty_kit()、
# write_datapackage() 都照 KIT_SCHEMA 跑，自動包含新表。
#   group_lineage  題組沿革：一個題本的題組在後來的題本延續、改名、拆開或合併成哪個題組
#   contacts       聯絡人名冊：誰負責哪些問卷、題本、題組、施測，請教單要問誰

KIT_ENUMS$lineage_kind <- c("continued", "renamed", "split", "merged")
KIT_ENUMS$scope_type <- c("all", "instrument", "form", "group", "admin")

KIT_SCHEMA[["group_lineage"]] <- list(
  cols = list(from_group = kcol(req = TRUE), from_formID = kcol(req = TRUE),
              to_group = kcol(req = TRUE), to_formID = kcol(req = TRUE),
              kind = kcol(enum = "lineage_kind", req = TRUE),
              # 從哪一次施測開始用新的題組
              effective_admin = kcol(),
              reason = kcol(), decided_by = kcol(), decided_on = kcol("date"),
              sourceID = kcol()),
  key = c("from_group", "from_formID", "to_group", "to_formID"),
  fk = list(list(cols = "from_group", table = "groups"),
            list(cols = "from_formID", table = "forms"),
            list(cols = "to_group", table = "groups"),
            list(cols = "to_formID", table = "forms"),
            list(cols = "effective_admin", table = "admins"),
            list(cols = "sourceID", table = "sources")))

# 一人一個負責範圍一列：同一個人負責好幾個範圍就寫好幾列（contactID 相同）。
# scope_type = "all" 時 scope_id 留空；其他時候 scope_id 是問卷代碼（P T C S）、formID、
# groupID 或 adminID。active = 0 是已經不負責（留著當紀錄）。
KIT_SCHEMA[["contacts"]] <- list(
  cols = list(contactID = kcol(req = TRUE), name = kcol(req = TRUE), role = kcol(),
              unit = kcol(), email = kcol(),
              scope_type = kcol(enum = "scope_type", req = TRUE), scope_id = kcol(),
              active = kcol(enum = "flag01"), note = kcol()),
  key = c("contactID", "scope_type", "scope_id"))
