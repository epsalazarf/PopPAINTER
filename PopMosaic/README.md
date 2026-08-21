# PopMosaic: ADMIXTURE Plotter (R Shiny)

_Current Version: v1.43 [August 2026]_

_Part of the [PopPAINTER](../README.md) suite._

This is the **stable, recommended** version of PopMosaic (`main` branch). A features preview (v2.0, Plotly-based) is under development on the `dev` branch — see [Which Version Should I Use?](../README.md#which-version-should-i-use) for details.

**Online version:** https://epsalazarf.shinyapps.io/PopMosaic/ · **Quick Start Guide:** [PopMosaic-guide.md](PopMosaic-guide.md)

## Description

**PopMosaic** is an interactive Shiny application for displaying and customizing ADMIXTURE results across multiple K values. It reads `.Q` ancestry-proportion files and a formatted `popinfo` metadata file to render color-coded mosaic (stacked-bar) plots, with interactive tools for subsetting, sorting, and recoloring the plot.

### Requirements

- **R** (≥ 4.0); **RStudio** recommended.

- **R** packages:
  
  ```r
  install.packages(c("shiny", "shinyjs", "dplyr", "readr", "tidyr", "tibble", "ggplot2", "RColorBrewer", "pheatmap", "colourpicker", "markdown"), dependencies = TRUE)
  ```

### Running the App

If you cloned the repo to your local machine, from RStudio open `PopMosaic-app.R` and click **Run App**, or from the R console:

```r
shiny::runApp("PopPAINTER/PopMosaic/PopMosaic-app.R")
```

After the app starts, upload your `.Q` file(s) and `popinfo` file in the side panel, or click **Load Demo Data** to try a built-in example.

## Features

- **Multi-K visualization**: Switch between K values dynamically.

- **Population Selection**: Subset and sort samples by population or ancestry proportion.

- **Color Customization**: Recolor ancestry components interactively.

- **Dynamic Plot Options**: Add population labels or collapse samples into group bars.

- **Save Options**: Download the current plot as PNG or PDF.

## Input Files

- **ADMIXTURE Results**: One or more `.Q` files from ADMIXTURE or similar population genomics software.

- A fam or `popinfo` file (see below) linking sample IDs to population tags .

#### About the popinfo file

A `popinfo` is a metadata file links sample IDs to population and metadata categories along with other fields. PopMosaic relies on the `popinfo` for added functions like grouping, reordering and filtering. Make sure the `popinfo` file matches your data set and contains only relevant samples. 

- **Required columns:**
  
  - `ID` — unique sample names.
  
  - `POP` — population code.

- **Recommended columns:**
  
  - `POPULATION` — human-readable population name.
  
  - `META` — grouping of populations for broader categories, typically linguistic, ethnic, or subcontinental groupings.
  
  - `SUPER` — grouping into very broad categories, typically continental-level.

  Recommended columns are optional, but when present PopMosaic expects them to follow a loose hierarchy: `ID` → `POP` → `META` → `SUPER`, from most to least granular.

- Example:
  
  ```
  ID       POP      POPULATION    META               SUPER
  HG00119  GBR      British       Northern European   EUR
  HG00120  GBR      British       Northern European   EUR
  HG00275  CHB      Han Chinese   East Asian          EAS
  ```

> **NOTE:** Since Q files have no IDs to match, you are required to upload a popinfo file with _exactly the same row numbers as the samples used in the analysis_ (excluding header) and make sure samples are in the same order as the input file to prevent mislabeling.

#### File Formatting Guidelines

- Tab- or space-delimited TXT/TSV file.

- PLINK 1 `.fam` file: FAMID will be used as POP tag.

- First row contains column headers.

- One sample per line.

#### Example Data

_New feature_: Click the "Load Demo Data" button for a 1000 Genomes example data.

To test the app, you can use the demo dataset included in `PopMosaic/demo/`:

- `demo.1kgp.k8.Q`
- `demo.1kgp.popinfo.tsv`

Load these files in the sidepanel inside the app to view a sample ADMIXTURE plot.

## Application Guide

### UI Controls

- **Upload Q files** (required): Select one or more Q files from the same data set to be rendered.

- **Upload POPINFO** (optional): Upload a sample metadata file to enable further controls.

- **Select Q file**: Choose which K=X results to plot.

- **Plot Title**: Set a custom title for the main plot (default: input file name).

- **Add borders**: Enables a white outline to the sample bars.

- **K Sort**: Rearranges sample bars according to their most predominant K component.

- **Show Names**: Displays samples names across the X-axis (numbered if POPINFO not loaded).

- **Grouping**:  Sorts samples according to a criteria.
  
  - **Default**: Follows the original Q file order.
  - **K Groups**: Groups samples according to ther predominant K component.
  - **Factor**: (Requires POPINFO) Group samples according to the selected **Factor** value, in alphabetical order.

- **Color X** Palette: Change the color associated to their corresponding K.

##### Controls after loading POPINFO

- **Factor**: Filter the displayed populations.
- **Population Emphasis**: Highlight a specific population.
- **Factor**: Categorical value column to group samples by a chosen category (e.g., `POPULATION`, `REGION`). App filters out columns with unique (IDs) or numerical values.
- **Select Group(s)**: If empty, displays all samples. Selecting  factor values from the menu subsets sample groups in the order added.
- **POP as second factor**: If enabled, further divides Factor groups to their population tag. Requires a `POP` column in the POPINFO to work.

### Saving the Plot

- Right click the plot image in app to download it as a PNG. What you see is what you get.

### Troubleshooting

- Ensure that the `popinfo` file's rows matches the exact number of samples in the `.Q` file.
- Check for missing or incorrectly formatted data in the input files.
- If the app fails to run, make sure all necessary R libraries are installed.
- If plots fail to render without an error message, restart your R/R Studio session.

## License

This software is free to use and modify under the [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/) license.

## Citation

If you use PopMosaic in your work, please cite:

> _Salazar-Fernandez, E. P. (2026). PopPAINTER: Population genomics visualization suite [Computer software]. DOI: https://doi.org/10.5281/zenodo.20724143_

## Disclaimers

- Population metadata and ADMIXTURE results for the demo dataset were derived from the [1000 Genomes Project](http://www.nature.com/nature/journal/v526/n7571/full/nature15393.html) (Phase 3).
- This app (v1.3+) used Claude Code (Sonnet 5) for minor bug fixes and modifications for online deployment to `shinyapps.io`; the quick guide was also written by Claude Sonnet 5.
- See the [PopPAINTER README](../README.md#disclaimers) for full disclaimer text.

## Contact

For issues, suggestions, or contributions, feel free to reach me at [epsalazarf@gmail.com](mailto:epsalazarf@gmail.com).
