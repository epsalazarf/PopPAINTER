---
marp: true
theme: default
paginate: true
footer: "PopCanvas Quick Guide · PopPAINTER suite"
style: |
  section {
    font-size: 26px;
  }
  section.lead h1 {
    font-size: 2.4em;
    color: #C8102E;
  }
  section.lead h2 {
    color: #444;
    font-weight: 400;
  }
  h1, h2 {
    color: #C8102E;
  }
  code {
    background: #f4f4f4;
  }
---

<!-- _class: lead -->

# PopCanvas ❧ PCA

## Popgen PCA Plotter — Quick Start Guide

*Inspired by Pollock's vibrant chaos*

Part of the **PopPAINTER** suite

---

## What is PopCanvas?

PopCanvas is an interactive Shiny app for exploring **PCA results** from population genomics software.

- Reads `.eigenvec` / `.eigenval` files + a `popinfo` metadata file
- Produces a customizable, interactive PCA plot in the browser
- No coding required — everything is point-and-click

**Goal:** go from PCA output to a publication-ready, explorable figure in seconds.

---

## Key Features

- **Interactive Zoom** — brush + double-click to zoom into any region
- **Population Selection** — show or hide populations on demand
- **Population Emphasis** — highlight one population against the rest
- **Color Customization** — color by any category in your `popinfo` file
- **Plot Types** — points, sample-ID text, or population labels
- **Group Centroids** — overlay average position per group
- **Save Options** — export the current view as PNG or PDF

---

<!-- _class: lead -->

# Try It Yourself

No files needed — just click and explore

---

## Load the Demo Data

1. Open the app
2. Click **Load Demo Data** in the sidebar
3. A 1000 Genomes PCA (2,504 samples) appears instantly

The button disappears once the plot renders — data's loaded, you're ready to explore.

*(This loads the same files as if you'd uploaded `demo.1kgp.eigenvec` / `.eigenval` / `.popinfo.tsv` from the `demo/` folder yourself.)*

---

## Sidebar Controls — Cheat Sheet

| Control | What it does |
|---|---|
| **Title** | Custom plot title |
| **First/Second Component** | Choose which PCs go on X / Y |
| **Flip x-axis / y-axis** | Mirror an axis |
| **Type** | Points / Text (IDs) / Labels (populations) |
| **Populations displayed** | Filter which populations show |
| **Populations emphasis** | Highlight one population |
| **Group Coloring** | Color by any `popinfo` column |
| **Group Centroids** | Show average position per group |
| **Legend** | Toggle the color legend |

---

## Interactive Plotting

- **Zoom in:** drag to select an area (brush), then double-click inside it
- **Zoom out:** double-click outside the selected area
- **Inspect points:** brushed samples list their metadata in the
  **Points Info** panel at the bottom of the sidebar

Great for zooming into a crowded cluster to see who's actually in it.

---

## Save & Export

- **Save as PNG** / **Save as PDF** buttons in the sidebar
- Or right-click the plot image directly and "Save image as…"

Also check the **Data Table** tab — the full merged PCA + metadata table,
searchable and sortable (filters there don't affect the plot).

---

## Bring Your Own Data

When you're ready to move past the demo:

- **`.eigenvec` + `.eigenval`** — from PLINK (or similar) PCA output
- **`popinfo.tsv`** — tab-separated, first column matches sample IDs
  - Required: `ID`, `POP`
  - Recommended: `POPULATION`, `META`

Full format details are in the app's **Instructions** tab and the project `README.md`.

---

<!-- _class: lead -->

# Questions?

epsalazarf@gmail.com

*PopPAINTER: Population genomics visualization suite*
