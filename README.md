# kitItemBank: KIT item bank tool

English | [繁體中文](README.zh-TW.md)

Search and maintain the KIT item bank (Taiwan Early Childhood Development Survey Database).
Three banks: parents, children and adolescents, childcare teachers. The interface is in
Traditional Chinese.

## Two versions

Two versions are online at the same time, from the same repository.

| | 0.2 (wide tables) | 0.3 (long format, current) |
|---|---|---|
| Cloud app | <https://01a1981a-700b-e470-a554-3b8cc7de9331.share.connect.posit.cloud/> | <https://jiewentsai-kititembank.share.connect.posit.cloud/> |
| Code | branch [`v0.2`](https://github.com/jiewenTsai/kitItmBank/tree/v0.2), tag `0.2.0`; bug fixes only | branch `main`, tags from `0.3.0` |
| Install | `pak::pkg_install("jiewenTsai/kitItmBank@v0.2")` | `pak::pkg_install("jiewenTsai/kitItmBank")` |
| Bank storage | 4 wide tables in `inst/extdata/bank/`, maintained by hand | long-format kit, one CSV per table in `inst/extdata/kit/`, built from .sav headers, crosswalks and usage documents |
| Wave codes | `M036大`, `M048小`, … | administration IDs `M36M036`, `M36M084G01` with plain-language labels; old codes, `M36W84`, `84M`, `G01`, 國小一年級 still work |
| Which wave asked an item | hand-filled 0/1 table | the variables actually in each data file of each administration (`dataset_vars`) |
| Revisions and renamed items | item notes | an item relations table (revised, renamed, split, merged) with who decided and the source document |
| Editing | download the whole edited bank; the maintainer runs `update_bank()` | download a change package; check it on the review page (審核), download the patched bank, and a technical person commits it |

Use 0.3 when you need to know which administration and data file really contain an item, want a
codebook or SPSS label syntax, or maintain the bank. See [NEWS.md](NEWS.md) for changes.

## Pages in 0.3

- **Query:** item search (題項查詢), dimensions (構面查詢), administrations (施測: codebook Excel and
  SPSS `.sps`), compare two data files (比較).
- **Maintenance:** my work (我的工作: the audit list grouped by who can resolve it), editing
  (題庫編修: change packages), revision round-trip with teachers (改版), merging a new administration
  from .sav headers (併入新施測), review (審核), statistics (統計, local runs only; raw data is never
  stored).

The app never writes files.

## Install and run

```r
pak::pkg_install("jiewenTsai/kitItmBank")
kitItemBank::run_kititembank()
```

## Data

The built-in bank lives in `inst/extdata/kit/` (one CSV per table, version in `BANK_VERSION`).

```r
kit <- kitItemBank::read_kit()
kitItemBank::check_kit(kit)
kitItemBank::admin_codebook(kit, "M36W84")
```

MIT © KIT
