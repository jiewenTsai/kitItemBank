# kitItemBank: KIT item bank tool

English | [繁體中文](README.zh-TW.md)

Search and maintain the KIT item bank (Taiwan Early Childhood Development Survey Database).
Three banks: parents, children and adolescents, childcare teachers. The interface is in
Traditional Chinese.

- **Item search (題項查詢)**: find items by keyword, wave and dimension; see how an item was asked
  in each wave and which versions exist.
- **Dimensions (構面查詢)**: the items and per-wave reliability of a dimension, a network graph of
  dimensions and items, and a dimension report (Word, HTML).
- **Editing (題庫編修)**: change dimension and wave assignments and dimension information, then
  download the edited bank as Excel (with a change log).

The app never writes files. Edited banks are downloaded and handed to the maintainer, who updates
the built-in bank with `update_bank()`.

## Install and run

```r
pak::pkg_install("jiewenTsai/kitItmBank")
kitItemBank::run_kititembank()
```

## Data

The built-in bank lives in `inst/extdata/bank/<bank>/` (`itemBank.csv`, `itemDim.csv`,
`itemWave.csv`, `dimMeta.csv`, optional `dimReliability.csv`). From R:

```r
bank <- kitItemBank::load_bank("parent")
kitItemBank::query_items(bank, keyword = "睡眠")
```

To update it: download the edited bank from the app, then in the package source folder run
`update_bank("<file>.xlsx", "parent")`, review `git diff`, commit and release.

See [NEWS.md](NEWS.md) for changes. MIT © KIT
