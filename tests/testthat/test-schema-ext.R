# schema-ext.R：0.3.7 新增的表（group_lineage、contacts）和匯出（datapackage.json、DDI）

ext_tiny_kit <- function() {
  k <- empty_kit()
  t <- function(tb, ...) conform_kit_table(data.frame(..., stringsAsFactors = FALSE), tb)
  k$cohorts <- t("cohorts", cohortID = "K2013", code = "M36")
  k$admins <- t("admins", adminID = c("M36M072", "M36M084G01"), cohortID = "K2013",
                ageMonth = c("72", "84"), surveyFrom = c("2019-02-01", "2020-02-01"),
                surveyTo = c("2019-08-31", "2021-08-30"), status = "released",
                datasetRelease = c("D00240", "D00250"), citation_en = c("", "Chang (2025)."))
  k$forms <- t("forms", formID = c("F-A", "F-B"), instrument = "P", label = c("a", "b"))
  k$datasets <- t("datasets", datasetID = c("M36M072-P", "M36M084G01-P", "M36M084G01-S"),
                  adminID = c("M36M072", "M36M084G01", "M36M084G01"),
                  file = c("data1", "data1", "IRT"), instrument = c("P", "P", "S"),
                  formID = c("F-A", "F-B", ""), missingScheme = c("2020", "2020", ""))
  k$sources <- t("sources", sourceID = "CW-M36W84", kind = "crosswalk", version = "M36W84")
  k$groups <- t("groups", groupID = c("g.old", "g.new", "g.a", "g.b"),
                label = c("Old", "New", "A", "B"))
  k$items <- t("items", item_uid = c("P.x1", "P.x2"), instrument = "P", varname = c("x1", "x2"))
  k$item_versions <- t("item_versions", item_uid = c("P.x1", "P.x2"), itemVer = "1",
                       label = c("Item <one> & more", "Item two"), stem = c("Stem:", ""),
                       valueSetID = c("vs1", ""))
  k$value_codes <- t("value_codes", valueSetID = "vs1", code = c("1", "2"),
                     label = c("no", "yes"), order = c("1", "2"))
  k$missing_schemes <- t("missing_schemes", scheme = "2020", code = c("7777", "9999"),
                         label = c("refused", "missing"), kind = c("refused", "missing"))
  k$dataset_vars <- t("dataset_vars",
    datasetID = c("M36M084G01-P", "M36M084G01-P", "M36M084G01-P", "M36M084G01-S", "M36M072-P"),
    varname = c("x1", "x2", "hidden", "theta", "x1"),
    item_uid = c("P.x1", "P.x2", "", "", "P.x1"), itemVer = c("1", "1", "", "", "1"),
    kind = c("item", "item", "system", "derived", "item"), order = c("1", "2", "3", "1", "1"),
    public = c("", "", "0", "", ""))
  k
}

test_that("the new tables are part of the schema and round-trip", {
  expect_true(all(c("group_lineage", "contacts") %in% kit_tables()))
  kit <- empty_kit()
  expect_length(kit, length(KIT_SCHEMA))
  expect_equal(nrow(check_kit(kit)), 0L)
  d <- withr::local_tempdir()
  write_kit(kit, d)
  expect_true(all(file.exists(file.path(d, c("group_lineage.csv", "contacts.csv")))))
  expect_equal(read_kit(d), kit)
})

test_that("import_group_lineage works out kinds, admins and sources", {
  k <- ext_tiny_kit()
  cd <- data.frame(from_group = c("g.old", "g.a", "g.a", "g.old"),
                   to_group = c("g.new", "g.a", "g.b", "g.new"),
                   from_formID = "F-A", formID = "F-B", year = "2020",
                   varnames = c("x1", "", "x2", "x1"), basis = "crosswalk",
                   stringsAsFactors = FALSE)
  gl <- import_group_lineage(cd, k, decided_on = "2026-11-02")
  expect_identical(names(gl), names(KIT_SCHEMA$group_lineage$cols))
  expect_equal(nrow(gl), 3L)
  expect_equal(gl$kind, c("renamed", "split", "split"))
  expect_true(all(gl$effective_admin == "M36M084G01"))
  expect_true(all(gl$sourceID == "CW-M36W84"))
  k$group_lineage <- gl
  expect_equal(nrow(check_kit(k)), 0L)

  merged <- import_group_lineage(data.frame(from_group = c("g.a", "g.b"), to_group = "g.new",
                                            from_formID = "F-A", formID = "F-B"), k)
  expect_equal(merged$kind, c("merged", "merged"))

  k$group_lineage$kind[1] <- "moved"
  expect_true("bad_value" %in% check_kit(k)$check)
})

test_that("group_lineage_chain follows links both ways; the UI block is NULL without lineage", {
  k <- ext_tiny_kit()
  k$group_lineage <- conform_kit_table(data.frame(
    from_group = c("g.a", "g.old"), from_formID = c("F-A", "F-A"),
    to_group = c("g.old", "g.new"), to_formID = c("F-A", "F-B"),
    kind = c("renamed", "renamed"), effective_admin = c("M36M072", "M36M084G01")),
    "group_lineage")
  ch <- group_lineage_chain(k, "g.new")
  expect_equal(nrow(ch), 2L)
  expect_equal(ch$to_group, c("g.old", "g.new"))
  expect_equal(ch$to_label, c("Old", "New"))
  expect_equal(nrow(group_lineage_chain(k, "New")), 2L)
  expect_equal(nrow(group_lineage_chain(k, "g.b")), 0L)
  expect_null(group_lineage_ui(k, "g.b"))
  expect_match(as.character(group_lineage_ui(k, "g.old")), "M36M084G01")
})

test_that("contacts_for puts the most specific scope first", {
  k <- ext_tiny_kit()
  k$contacts <- conform_kit_table(data.frame(
    contactID = c("c1", "c2", "c3", "c4"), name = c("A", "B", "C", "D"),
    scope_type = c("all", "instrument", "group", "form"),
    scope_id = c("", "P", "g.new", "F-B"), active = c("", "", "", "0")), "contacts")
  expect_equal(nrow(check_kit(k)), 0L)
  hit <- contacts_for(k, formID = "F-B", groupID = "g.new")
  expect_equal(hit$contactID, c("c3", "c2", "c1"))
  expect_equal(contacts_for(k, instrument = "T")$contactID, "c1")
})

test_that("write_datapackage writes valid JSON listing every table", {
  d <- withr::local_tempdir()
  write_kit(empty_kit(), d)
  writeLines("2099.1", file.path(d, "BANK_VERSION"))
  path <- write_datapackage(d)
  expect_true(jsonlite::validate(paste(readLines(path, encoding = "UTF-8"), collapse = "\n")))
  dp <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(dp$version, "2099.1")
  res <- vapply(dp$resources, `[[`, "", "name")
  expect_setequal(res, kit_tables())
  expect_true(all(file.exists(file.path(d, vapply(dp$resources, `[[`, "", "path")))))
  iv <- dp$resources[[match("item_relations", res)]]$schema
  expect_equal(unlist(iv$primaryKey), KIT_SCHEMA$item_relations$key)
  fk <- iv$foreignKeys[[1]]
  expect_equal(unlist(fk$fields), c("from_uid", "from_ver"))
  expect_equal(unlist(fk$reference$fields), c("item_uid", "itemVer"))
  ad <- dp$resources[[match("admins", res)]]$schema$fields
  st <- ad[[match("status", vapply(ad, `[[`, "", "name"))]]
  expect_equal(unlist(st$constraints$enum), KIT_ENUMS$status)
  expect_equal(ad[[match("ageMonth", vapply(ad, `[[`, "", "name"))]]$type, "integer")
  # codebook_bank_version() reads it back when there is no BANK_VERSION
  file.remove(file.path(d, "BANK_VERSION"))
  expect_equal(codebook_bank_version(d), "2099.1")
})

test_that("ddi_codebook gives parseable DDI with one var per public variable", {
  k <- ext_tiny_kit()
  doc <- ddi_codebook(k, "M36M084G01", version = "2099.1")
  x <- xml2::read_xml(as.character(doc))
  expect_equal(xml2::xml_attr(x, "version"), "2.5")
  expect_equal(unname(xml2::xml_ns(x)[1]), "ddi:codebook:2_5")
  x <- xml2::xml_ns_strip(x)
  vars <- xml2::xml_find_all(x, "//dataDscr/var")
  expect_equal(length(vars), 3L)   # x1, x2, theta; "hidden" is not in the public file
  expect_equal(xml2::xml_attr(vars, "name"), c("x1", "x2", "theta"))
  expect_equal(length(xml2::xml_find_all(x, "//fileDscr")), 2L)
  expect_equal(xml2::xml_text(xml2::xml_find_first(x, "//verStmt/version")), "2099.1")
  expect_equal(xml2::xml_text(xml2::xml_find_first(x, "//biblCit")), "Chang (2025).")
  x1 <- vars[[1]]
  expect_equal(xml2::xml_text(xml2::xml_find_first(x1, "labl")), "Item <one> & more")
  expect_equal(xml2::xml_text(xml2::xml_find_first(x1, "qstn/qstnLit")), "Stem: Item <one> & more")
  cat_x1 <- xml2::xml_find_all(x1, "catgry")
  expect_equal(xml2::xml_text(xml2::xml_find_all(x1, "catgry/catValu")),
               c("1", "2", "7777", "9999"))
  expect_equal(xml2::xml_attr(cat_x1, "missing"), c(NA, NA, "Y", "Y"))
  # theta: no value set, no missing scheme
  expect_equal(length(xml2::xml_find_all(vars[[3]], "catgry")), 0L)

  all_vars <- ddi_codebook(k, "M36M084G01", public_only = FALSE)
  expect_equal(length(xml2::xml_find_all(xml2::xml_ns_strip(all_vars), "//var")), 4L)
  one <- ddi_codebook(k, "M36M084G01", datasetID = "M36M084G01-S")
  expect_equal(length(xml2::xml_find_all(xml2::xml_ns_strip(one), "//var")), 1L)
  expect_error(ddi_codebook(k, "nope"), "Unknown")
})

test_that("the built-in kit has the new tables and exports", {
  kit <- read_kit()
  expect_true(nrow(kit$group_lineage) >= 1L)
  expect_equal(nrow(check_kit(kit)), 0L)
  a <- kit$admins$adminID[kit$admins$status == "released"][1]
  doc <- ddi_codebook(kit, a)
  n <- sum(admin_codebook(kit, a)$vars$public)
  expect_equal(length(xml2::xml_find_all(xml2::xml_ns_strip(doc), "//var")), n)
})
