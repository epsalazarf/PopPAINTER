# PopPAINTER: Interactive Shiny Apps for Population Genomics

*By Pavel Salazar-Fernandez ([epsalazarf@gmail.com](mailto:epsalazarf@gmail.com))*

**PopPAINTER** is a suite of R Shiny applications for interactive visualization and exploration of population genomics data.  
Currently included:

- **PopCanvas** — PCA visualizer (inspired by Pollock’s vibrant chaos).

- **PopMosaic** — ADMIXTURE plotter (inspired by Mondrian’s orderly grids).

Additional visualization tools (e.g., a Circos-style genomic relationship plot) are planned for future versions.

## About

PopPAINTER apps allow users to:

- Render and explore PCA and ADMIXTURE results interactively.

- Modify colors, labels, and subset selections without editing source code.

- Export publication-quality figures (PDF/PNG).

They are intended for research workflows where quick, high-quality data visualization is needed — particularly for human population genomics.

## Requirements

- **R** (≥ 4.0)

- **RStudio** (recommended)

- Packages: `shiny`, `shinyjs`, `tidyverse`, `scales`, `RColorBrewer`, `pheatmap`, `colourpicker` (full per-app lists in each app’s `DESCRIPTION` and `README`).

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

## About the popinfo file

A `popinfo` file links sample IDs to population and metadata categories.

- **Required columns:**
  
  - `ID` — unique sample names.
  
  - `POP` — population code.

- **Recommended columns:**
  
  - `POP_SIMPLE` — human-readable population name.
  
  - `REGION` — grouping of populations for broader categories.

PopPAINTER apps rely on `popinfo` for added functions like grouping, coloring, and filtering. Ensure the file matches your dataset and contains only relevant samples.

**Format:**

- Tab- or space-delimited TXT file.

- First row contains column headers.

- One sample per line.

## Running an App

From RStudio:

1. Open the app file in the relevant directory — `PopCanvas/PopCanvas-app.R` or `PopMosaic/PopMosaic-app.R`.

2. Click **Run App**.

From R console:

```r
shiny::runApp("PopCanvas/PopCanvas-app.R")   # or PopMosaic/PopMosaic-app.R
```

## Repository Structure

```
PopPAINTER/
├── PopCanvas/      # PCA visualizer app
├── PopMosaic/      # ADMIXTURE visualizer app
├── README.md
└── .gitignore
```

- `main` branch — beta releases for internal use.

- `dev` branch — active development and new features.

## License

This software is completely free to use, but credits to this repo is very well appreciated.


