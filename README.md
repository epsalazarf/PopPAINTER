# PopPAINTER: Interactive Shiny Apps for Population Genomics

[![DOI](https://zenodo.org/badge/1038821545.svg)](https://doi.org/10.5281/zenodo.20724143)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

**PopPAINTER** is a suite of R Shiny applications for interactive visualization and exploration of population genomics data. Currently included:

- **[PopCanvas](PopCanvas/)** — PCA visualizer (inspired by Pollock's vibrant chaos).
- **[PopMosaic](PopMosaic/)** — ADMIXTURE plotter (inspired by Mondrian's orderly grids).

Additional visualization tools (e.g., a Circos-style genomic relationship plot) are planned for future versions.

## About

PopPAINTER apps allow users to:

- Render and explore PCA and ADMIXTURE results interactively.
- Modify colors, labels, and subset selections without editing source code.
- Export publication-quality figures (PDF/PNG).

They are intended for research workflows where quick, high-quality data visualization is needed — particularly for human population genomics.

## Which Version Should I Use?

Each app is developed in two parallel tracks:

|                 | **v1.x** (recommended)                                       | **v2.0** (preview)                                         |
| --------------- | ------------------------------------------------------------ | ---------------------------------------------------------- |
| **Branch**      | [`main`](https://github.com/epsalazarf/PopPAINTER/tree/main) | [`dev`](https://github.com/epsalazarf/PopPAINTER/tree/dev) |
| **Engine**      | Base R `shiny` + `ggplot2`                                   | `plotly` + sidebar dashboard layout                        |
| **Status**      | Stable, tested                                               | Actively in development                                    |
| **Footprint**   | Lightweight, fast to launch                                  | Heavier, more dependencies                                 |
| **Online demo** | ✅ Live on shinyapps.io                                       | ⏳ Not yet deployed                                         |

**For most users, the v1.x apps on `main` are recommended** — they are the most stable and lightweight versions, and are the only ones currently available as hosted online demos (see links below).

The **v2.0** apps (`PopCanvas-app-v2.0.R`, `PopMosaic-app-v2.0.R`, on the `dev` branch) introduce a richer, interactive Plotly-based interface with additional features, but are still under active development and may be incomplete or unstable. Try them if you want a preview of upcoming functionality, or want to contribute feedback.

### Requirements

- **R** (≥ 4.0)
- **RStudio** (recommended)
- Packages: `shiny`, `shinyjs`, `tidyverse`, `scales`, `RColorBrewer`, `pheatmap`, `colourpicker` (full per-app lists in each app's `DESCRIPTION` and `README`).
- Input files depend on the app (see below).

## PopCanvas (PCA Viewer)

**Purpose:** Explore principal component analysis results with dynamic subsetting and coloring.

**Required files:**

- `.eval` and `.evec` files (from `smartpca` or similar), one of each per dataset.
- `popinfo` file (see **About the popinfo file** below).

**Features:**

- Point/tag view modes.
- Subset and highlight populations.
- Color by region, population, or custom categories.
- Interactive zoom and panning.

**Output:**

- Export plots to high-quality PNG or PDF.

**Online version (v1.4):** https://epsalazarf.shinyapps.io/PopCanvas/

**Docs:** [App README](PopCanvas/README.md) · [Quick Start Guide](PopCanvas/PopCanvas-guide.md)

## PopMosaic (ADMIXTURE Plotter)

**Purpose:** Display and customize ADMIXTURE results across multiple K values.

**Required files:**

- `.Q` files from ADMIXTURE (all K values for the same dataset).
- `popinfo` file.

**Features:**

- Switch between K values dynamically.
- Subset and sort samples by population or ancestry proportion.
- Recolor components interactively.
- Add population labels or collapse to group bars.

**Output:**

- Export plots to PNG or PDF.

**Online version (v1.43):** https://epsalazarf.shinyapps.io/PopMosaic/

**Docs:** [App README](PopMosaic/README.md) · [Quick Start Guide](PopMosaic/PopMosaic-guide.md)

## Usage

### Running an App

#### From RStudio:

1. Open the app file in the relevant directory — `PopCanvas/PopCanvas-app.R` or `PopMosaic/PopMosaic-app.R`.
2. Click **Run App** on the top right corner of the code window.
3. Upload your data, or click **Load Demo Data** to try the app with a built-in 1000 Genomes example.

#### From R console:

```r
shiny::runApp("PopCanvas/PopCanvas-app.R")   # or PopMosaic/PopMosaic-app.R
```

#### Online versions:

Open the URL indicated above for each app — no installation required.

### Quick Start Guides

Each app includes a short slide-style guide (built with [Marp](https://marp.app/)) covering setup, inputs, and key controls:

- [PopCanvas-guide.md](PopCanvas/PopCanvas-guide.md)
- [PopMosaic-guide.md](PopMosaic/PopMosaic-guide.md)

These render as readable Markdown on GitHub, or as a slide deck via the Marp CLI/VS Code extension, or an online converter such as [marp.vercel.app](https://marp.vercel.app/).

### Repository Structure

```
PopPAINTER/
├── PopCanvas/      # PCA visualizer app (v1.x on main, v2.0 preview on dev)
├── PopMosaic/      # ADMIXTURE visualizer app (v1.x on main, v2.0 preview on dev)
├── README.md
└── .gitignore
```

- `main` branch — stable releases for public use.
- `dev` branch — internal experimental development with new features (v2.0 apps).

### About the popinfo file

A `popinfo` file links sample IDs to population and metadata categories.

- **Required columns:**
  - `ID` — unique sample names.
  - `POP` — population code.
- **Recommended columns:**
  - `POPULATION` — human-readable population name.
  - `META` — grouping of populations for broader categories, typically linguistic, ethnic, or subcontinental groupings.
  - `SUPER` — grouping into very broad categories, typically continental-level.

Recommended columns are optional, but when present PopPAINTER apps expect them to follow a loose hierarchy: `ID` → `POP` → `META` → `SUPER`, from most to least granular. PopPAINTER apps rely on `popinfo` for added functions like grouping, coloring, and filtering. Ensure the file matches your dataset and contains only relevant samples.

**Format:**

- Tab- or space-delimited TXT/TSV file.
- First row contains column headers.
- One sample per line.

## License

This software is free to use and modify under the [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/) license.

## Citation

If you use PopPAINTER in your work, please cite:

> _Salazar-Fernandez, E. P. (2026). PopPAINTER: Population genomics visualization suite [Computer software]. DOI: https://doi.org/10.5281/zenodo.20724143_

This DOI always resolves to the latest version. See the [Zenodo record](https://doi.org/10.5281/zenodo.20724143) for version-specific citations.

## Disclaimers

### 1000 Genomes Project

Population metadata, PCA and ADMIXTURE results for the demo were derived from the 1000 Genomes Project dataset (Phase 3):

> _A global reference for human genetic variation, The 1000 Genomes Project Consortium, Nature 526, 68-74 (01 October 2015) [doi:10.1038/nature15393](http://www.nature.com/nature/journal/v526/n7571/full/nature15393.html)_

### Generative AI Disclaimer

- Release apps version 1.3+ used Claude Code (Sonnet 5) for minor bug fixes and modifications for online upload to `shinyapps.io`.
- Development apps version 2.0+ used Claude Code (Sonnet 5) for the major rework and re-implementation in `R::Dashboard` and `R::Plotly` engines.
- Quickguides and extra documentation were written by Claude Sonnet 5.

## Contact

For issues, suggestions, or contributions, feel free to reach out at [epsalazarf@gmail.com](mailto:epsalazarf@gmail.com).
