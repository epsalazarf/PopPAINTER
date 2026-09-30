# Changelog

All notable changes to the PopPAINTER suite are documented here. Entry numbering follows the apps' own version numbers (PopCanvas, PopMosaic) rather than a separate repo-wide scheme — some intermediate app versions between the initial `v1.0-beta` release and `v1.4` were iterated on without a dedicated changelog entry, so this file starts formally at **v1.4**.

The format loosely follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [PopCanvas 1.5] - 2026-09-30

### Added

- Support for reading `.evec`/`.eval` output from **smartpca** (EIGENSOFT), in addition to PLINK — auto-detected, no conversion needed.
- The eigenvalue file and POPINFO file are now both optional: PopCanvas can render a basic PCA plot from an eigenvec file alone.
- PLINK's `FID` column is now preserved (previously discarded) and offered as a "Group Coloring" option — including Group Centroids — whenever it carries real grouping information (not the default `0`/`-9` placeholder, and between 2 and 20 unique values).
- Auto-detection of PLINK 1.9 (headerless), PLINK 2.0 (headered), and smartpca eigenvec formats, so any of the three can be dropped in directly.

### Fixed

- Headerless PLINK 1.9 `.eigenvec` files (no header row at all) were misread: the first sample was silently swallowed as a fake header row, shifting every value.
- A numeric-looking `FID` column (e.g. PLINK's `0` placeholder) could be mistaken for a PC column, shifting all PC values over by one.
- A missing `library(readr)` dependency that would have broken a fresh deployment (e.g. on shinyapps.io) despite working locally.
- Restored a bold 0/0 axis crosshair as a visual sanity check: real PCA output is centered near the origin, so a plot rendering visibly off-center or squished against it is a sign something is wrong upstream (e.g. running PCA on data that needed PCoA instead).

### Changed

- Updated `PopCanvas/README.md` to document all three supported eigenvec formats and clarify which inputs are required versus optional.

## [1.4] - 2026-08-21

### Highlights

- **Both apps are now live online** — no installation required:
  - **PopCanvas (v1.4):** https://epsalazarf.shinyapps.io/PopCanvas/
  - **PopMosaic (v1.43):** https://epsalazarf.shinyapps.io/PopMosaic/

### Added

- **Load Demo Data** button in both apps — loads a built-in 1000 Genomes Project (Phase 3) example dataset with one click, no file uploads required to try the apps.
- Bundled demo datasets (`PopCanvas/demo/`, `PopMosaic/demo/`) for local and online use.
- Marp-based Quick Start Guides for each app: [PopCanvas-guide.md](PopCanvas/PopCanvas-guide.md), [PopMosaic-guide.md](PopMosaic/PopMosaic-guide.md).
- Zenodo archival DOI and citation entry, and a `CITATION.cff` file for GitHub's built-in citation support.
- Standardized `popinfo` metadata convention: `POPULATION` (human-readable population name), and the optional `META` / `SUPER` hierarchy for broader groupings (`META` = linguistic/ethnic/subcontinental, `SUPER` = continental).
- Generative AI usage disclaimer, documenting where Claude Code (Sonnet 5) was used in development and documentation.
- Preview apps (v2.0) on the `dev` branch: a reworked interactive interface built on `shinydashboard` and `plotly`, with a redesigned sidebar and additional grouping/coloring options. Still under active development — not yet deployed online, and not recommended for production use.

### Changed

- Rewrote and reorganized the repository, PopCanvas, and PopMosaic README files for clarity and consistency, including a "Which Version Should I Use?" guide recommending the stable v1.x apps (`main` branch) for most users.
- Corrected R package requirement lists for both apps to match what is actually loaded by each app.
- Fixed minor bugs in both v1.x apps.

### Fixed

- Removed stale/inconsistent references in documentation and in the v2.0 apps' in-app instructions (e.g. legacy `POP_SIMPLE`/`SUPERPOP` column names) so all docs consistently describe the `POPULATION` / `META` / `SUPER` convention.

## [1.0-beta] - 2025-08-20

- Initial public release of PopCanvas and PopMosaic as R Shiny apps, with core PCA and ADMIXTURE visualization functionality and bundled 1000 Genomes demo data.
