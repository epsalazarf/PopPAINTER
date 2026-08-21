---
marp: true
theme: default
paginate: true
footer: "PopMosaic Quick Guide · PopPAINTER suite"
style: |
  section {
    font-size: 26px;
  }
  section.lead h1 {
    font-size: 2.4em;
    color: #0057B7;
  }
  section.lead h2 {
    color: #444;
    font-weight: 400;
  }
  h1, h2 {
    color: #0057B7;
  }
  code {
    background: #f4f4f4;
  }
---

<!-- _class: lead -->

# PopMosaic ❧ ADMX

## ADMIXTURE Plotter — Quick Start Guide

*Inspired by Mondrian's orderly grids*

Part of the **PopPAINTER** suite

---

## What is PopMosaic?

PopMosaic is an interactive Shiny app for displaying and customizing **ADMIXTURE results**.

- Reads `.Q` ancestry-proportion files + an optional `popinfo` metadata file
- Renders color-coded mosaic (stacked-bar) plots
- Supports multiple K values from the same run

**Goal:** turn a folder of `.Q` files into an explorable, relabelable ancestry plot.

---

## Key Features

- **Multi-K Visualization** — switch between K values dynamically
- **Population Selection** — subset and sort samples by population
- **Color Customization** — recolor each ancestry component interactively
- **Dynamic Plot Options** — group bars, add borders, show sample names
- **Confusion Matrix & K Donut views** — alternate summaries of the same run
- **Save Options** — export the current view as PNG

---

<!-- _class: lead -->

# Try It Yourself

No files needed — just click and explore

---

## Load the Demo Data

1. Open the app
2. Click **Load Demo Data** in the sidebar
3. A K=8 1000 Genomes ADMIXTURE run (2,504 samples) appears instantly,
   with its matching `popinfo` already loaded

The button disappears once the plot renders — data's loaded, you're ready to explore.

*(This loads the same files as if you'd uploaded `demo.1kgp.k8.Q` and `demo.1kgp.popinfo.tsv` from the `demo/` folder yourself.)*

---

## Sidebar Controls — Cheat Sheet

| Control | What it does |
|---|---|
| **Select Q file** | Choose which K result to plot |
| **Plot Title** | Custom title (defaults to the file name) |
| **Add Borders** | White outline around each sample bar |
| **K Sort** | Order bars by their dominant K component |
| **Show Names** | Display sample IDs on the X-axis |
| **Grouping** | Default / K Groups / Factor (see next slide) |
| **Color per K** | Recolor each ancestry component individually |

---

## Controls After Loading POPINFO

Once a `popinfo` file is in (the demo loads one automatically):

- **Factor** — pick a category to group samples by (e.g. `POPULATION`, `REGION`)
- **Select Group(s)** — subset & order populations; leave empty to show all
- **POP as second factor** — nests groups further by `POP`
- Choosing **Grouping → Factor** applies the selected factor to the plot

---

## Explore the Other Tabs

- **ADMIXTURE Plot** — the main stacked-bar view
- **Confusion Matrix** — mean ancestry proportion per group, as a heatmap
- **K Donut Plot** — proportional overview of total ancestry across the run
- **Data Table** — the full sample-by-K table, searchable/sortable/exportable

All four update live as you change the sidebar controls.

---

## Save & Export

- Right-click the plot image and **"Save image as…"**
- What you see is what you get — no separate export step

Tip: the **Data Table** tab has its own export buttons (CSV / Excel / PDF)
for the underlying numbers.

---

## Bring Your Own Data

When you're ready to move past the demo:

- **One or more `.Q` files** from the same ADMIXTURE run (same prefix)
- **`popinfo.tsv`** *(optional but recommended)* — must have the
  **exact same number of rows, in the same order**, as your `.Q` file
  - Required: `ID`, `POP`
  - Recommended: `POPULATION`, `META`

Full format details are in the app's **Instructions** tab and the project `README.md`.

---

<!-- _class: lead -->

# Questions?

epsalazarf@gmail.com

*PopPAINTER: Population genomics visualization suite*
