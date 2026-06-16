# PopMosaic

_ADMIXTURE Plotter (R Shiny)_

Part of the [PopPAINTER](../README.md) suite. Developed by **Pavel Salazar-Fernandez**.

## Description

PopMosaic is an interactive Shiny application for displaying and customizing
ADMIXTURE results across multiple K values. It reads `.Q` ancestry-proportion
files and a formatted `popinfo` metadata file to render color-coded mosaic
(stacked-bar) plots, with interactive subsetting, sorting, and recoloring.

## Requirements

- **R** (≥ 4.0); **RStudio** recommended.

- R packages:

  ```r
  install.packages(c("shiny", "shinyjs", "tidyverse",
                     "RColorBrewer", "pheatmap", "colourpicker"))
  ```

  (`grid` ships with base R and needs no installation.)

## Required Files

- One or more `.Q` files from ADMIXTURE (all K values for the same dataset).

- A `popinfo` file linking sample IDs to population and metadata categories
  (see **About the popinfo file** in the [root README](../README.md)).

## Features

- Switch between K values dynamically.

- Subset and sort samples by population or ancestry proportion.

- Recolor ancestry components interactively.

- Add population labels or collapse samples into group bars.

## Output

- Export plots to high-quality PNG or PDF.

## Running the App

From RStudio, open `PopMosaic-app.R` and click **Run App**, or from the R console:

```r
shiny::runApp("PopMosaic/PopMosaic-app.R")
```

## Example Data

A demo dataset is bundled in `demo/`:

- `demo.1kgp.k8.Q`
- `demo.1kgp.popinfo.tsv`

## Contact

For issues or suggestions: [epsalazarf@gmail.com](mailto:epsalazarf@gmail.com).
