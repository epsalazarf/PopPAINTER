# <ABOUT> =====================================================================
# Title       : PopMosaic v2.0 — Interactive ADMIXTURE Plotter (R Shiny)
# Description : An interactive R Shiny dashboard for exploring ADMIXTURE .Q
#               ancestry-proportion files across multiple K values, with an
#               optional popinfo metadata file for category-based sample
#               filtering, grouping, and labeling — uploaded live from within
#               the app. Preserves the v1.2 feature set (K switching, borders,
#               K-sort, sample labels, custom recoloring) and adds a
#               redesigned sidebar dashboard, up to two crossed popinfo
#               categories (replacing the old factor/POP toggle), per-group
#               K donut summaries, an averaged stacked-barplot tab, and a
#               built-in instructions panel.
#
# Dependencies: shiny, shinydashboard, plotly, dplyr, readr, tidyr, forcats,
#               stringr, ggplot2, scales, DT, colourpicker, RColorBrewer,
#               pheatmap
#
# Engine note : v1.2 rendered everything with ggplot2 (static). v2.0 renders
#               the Admixture Plot as native Plotly (proportional-width
#               facets via manual subplot composition, so group panels keep
#               scaling with sample count under interactive zoom/hover), the
#               K Donut plots as native Plotly pies, and the Averaged Stacked
#               Barplot as ggplot2 + ggplotly(). The Confusion Matrix stays a
#               static pheatmap (unaffected by the grouping redesign). Static
#               PNG/PDF export is provided via a ggplot2 fallback (no
#               kaleido/orca system dependency required).
#
# Author      : Pavel Salazar-Fernandez (epsalazarf@gmail.com)
# Version     : 2.0
# Usage       : shiny::runApp('PopMosaic-app-v2.0.R')
# =============================================================================

# <START> ---------------------------------------------------------------------
message("> Starting: PopMosaic v2.0 — ADMIXTURE Plotter dashboard...")

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(plotly)
  library(dplyr)
  library(readr)
  library(tibble)
  library(tidyr)
  library(forcats)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(DT)
  library(colourpicker)
  library(RColorBrewer)
  library(pheatmap)
})

options(shiny.maxRequestSize = 50 * 1024^2)  # allow multiple Q file uploads (50 MB)

# <PREPARATIONS> --------------------------------------------------------------

# Robust popinfo reader: tries tab, then comma, then space delimiting.
read_popinfo_file <- function(path) {
  df <- tryCatch(readr::read_tsv(path, show_col_types = FALSE, trim_ws = TRUE),
                 error = function(e) NULL)
  if (is.null(df) || ncol(df) <= 1)
    df <- tryCatch(readr::read_csv(path, show_col_types = FALSE, trim_ws = TRUE),
                   error = function(e) NULL)
  if (is.null(df) || ncol(df) <= 1)
    df <- tryCatch(readr::read_delim(path, delim = " ", show_col_types = FALSE, trim_ws = TRUE),
                   error = function(e) NULL)
  df
}

# Classify every popinfo column as usable-categorical or not, with a reason.
# (Adapted from POPsampler: skips ID-like, numeric/coordinate-like, constant,
# and mostly-empty columns, so only genuine grouping fields are offered.)
classify_columns <- function(df) {
  n <- nrow(df)
  rows <- lapply(names(df), function(cn) {
    chr <- trimws(as.character(df[[cn]]))
    nonempty <- chr[!is.na(chr) & chr != ""]
    nonempty_frac <- length(nonempty) / n
    u <- unique(nonempty)
    n_unique <- length(u)

    ok <- TRUE
    reason <- NA_character_
    if (nonempty_frac < 0.5) {
      ok <- FALSE; reason <- "mostly empty"
    } else if (n_unique <= 1) {
      ok <- FALSE; reason <- "constant value"
    } else if (n_unique == length(nonempty)) {
      ok <- FALSE; reason <- "all values unique (ID-like)"
    } else {
      numeric_like <- suppressWarnings(!any(is.na(as.numeric(u))))
      if (numeric_like) {
        has_decimal <- any(grepl("\\.", u, fixed = TRUE))
        high_cardinality <- (n_unique / length(nonempty)) > 0.3
        if (has_decimal || high_cardinality) {
          ok <- FALSE; reason <- "numeric / continuous (e.g. coordinates)"
        }
      }
    }
    data.frame(column = cn, is_categorical = ok, n_levels = n_unique,
               reason = reason, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

DEFAULT_PALETTE <- c("#e60049", "#0bb4ff", "#50e991", "#e6d800",
                     "#9b19f5", "#ffa300", "#dc0ab4", "#b3d4ff",
                     "#00bfa0", "#7c1158", "#fd7f6f", "#b2e061",
                     "#bd7ebe", "#ffee65", "#fdcce5", "#beb9db")

# Build a set of native Plotly stacked-bar traces, one panel per facet
# combination, with panel widths proportional to sample count (mirrors
# ggplot2's facet_grid(scales="free", space="free"), which ggplotly() does
# not honor). Returns the combined plot plus facet strip labels/panel domains
# for annotation placement by the caller.
#
# Note: subplot()'s own inter-panel margin is applied on top of the supplied
# `widths`, so the resulting panel domains do NOT sit at the naive
# cumsum(widths) positions -- that drift compounds with panel count (it was
# large enough with >20 panels to make strip labels land over the wrong
# panel). We build the widget once via plotly_build() to read back the real
# domains, then use those for both the gap size and the annotation midpoints.
plotly_stacked_admix <- function(df, facet_vars, colors, show_xlabels, margin = 0.0015) {
  klevels <- levels(df$K)

  mk_panel <- function(sub, show_legend, show_yticks) {
    p <- plot_ly()
    for (k in klevels) {
      kd <- sub[sub$K == k, , drop = FALSE]
      p <- add_trace(p, data = kd, x = ~ID, y = ~Percent, type = "bar", name = k,
                      marker = list(color = colors[[k]]), showlegend = show_legend,
                      text = ~.hover, hoverinfo = "text", textposition = "none")
    }
    layout(p, barmode = "stack", bargap = 0,
           xaxis = list(title = "", showticklabels = show_xlabels, tickangle = -90),
           yaxis = list(title = if (show_yticks) "Proportion" else "", range = c(0, 1),
                        tickformat = ".2f", showticklabels = show_yticks))
  }

  if (length(facet_vars) == 0) {
    df$ID <- droplevels(df$ID)
    return(list(plot = mk_panel(df, TRUE, TRUE), strips = NULL, domains = NULL))
  }

  fk <- df %>% distinct(across(all_of(facet_vars)))
  fk$.facet_id <- seq_len(nrow(fk))
  df <- df %>% left_join(fk, by = facet_vars)
  weights <- unname(vapply(fk$.facet_id, function(i)
    n_distinct(df$ID[df$.facet_id == i]), numeric(1)))
  strips <- apply(as.data.frame(fk[facet_vars]), 1, paste, collapse = " / ")

  plots <- vector("list", nrow(fk))
  for (i in seq_len(nrow(fk))) {
    sub <- df[df$.facet_id == i, , drop = FALSE]
    sub$ID <- droplevels(factor(sub$ID, levels = unique(sub$ID)))
    plots[[i]] <- mk_panel(sub, i == 1, i == 1)
  }
  sp <- subplot(plots, nrows = 1, shareY = TRUE, titleX = FALSE,
                widths = weights / sum(weights), margin = margin)

  built <- plotly_build(sp)
  xax <- built$x$layout[grepl("^xaxis", names(built$x$layout))]
  nm <- names(xax); nm[nm == "xaxis"] <- "xaxis1"
  xax <- xax[order(as.integer(sub("xaxis", "", nm)))]
  domains <- t(vapply(xax, function(a) a$domain, numeric(2)))

  list(plot = sp, strips = strips, domains = domains)
}

# <\PREPARATIONS> ---------------------------------------------------------


# <UI> ------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "purple",

  dashboardHeader(title = "PopMosaic ❧ ADMX", titleWidth = 280),

  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "main_menu",
      menuItem("Admixture Plot",   tabName = "admix",        icon = icon("chart-bar")),
      menuItem("Confusion Matrix", tabName = "confusion",    icon = icon("table-cells")),
      menuItem("K Donut Plot",     tabName = "donut",        icon = icon("chart-pie")),
      menuItem("Averaged Stacked", tabName = "stacked",      icon = icon("layer-group")),
      menuItem("Data Table",       tabName = "table",        icon = icon("table")),
      menuItem("Instructions",     tabName = "instructions", icon = icon("circle-info"))
    ),

    tags$hr(style = "border-color:#5a4a78; margin:4px 8px;"),

    # ----- Data input (always visible) -----------------------------------
    tags$div(
      class = "side-section",
      tags$p("Data Input", class = "side-title"),
      fileInput("qfiles", "Q files (.Q) — required",
                multiple = TRUE, accept = c(".Q", "text/plain")),
      fileInput("pifile", "Popinfo (.tsv/.txt) — optional",
                multiple = FALSE, accept = c(".tsv", ".txt", ".csv", ".fam",  ".psam")),
      uiOutput("popinfo_warning")
    ),

    tags$hr(style = "border-color:#5a4a78; margin:4px 8px;"),

    # ----- Plot controls (rendered once data is loaded) -------------------
    tags$div(class = "side-section",
             tags$p("Settings", class = "side-title"),
             uiOutput("controls_ui"),
             uiOutput("cat1_menu"),
             uiOutput("cat1_vals_menu"),
             uiOutput("cat2_toggle"),
             uiOutput("cat2_menu"),
             uiOutput("cat2_vals_menu"),
             uiOutput("caption_ui")),

    tags$hr(style = "border-color:#5a4a78; margin:4px 8px;"),

    # ----- K threshold filter (rendered once Q data is loaded) -------------
    tags$div(class = "side-section",
             tags$p("Filtering", class = "side-title"),
             uiOutput("threshold_menu")),

    # ----- Sample counter ---------------------------------------------------
    uiOutput("sidebar_counts")
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background:#f5f4f8; }
      .skin-purple .main-header .navbar { background:#5b3f8e; }
      .skin-purple .main-header .logo   { background:#4a3275; }
      .box { border-top-color:#7e57c2 !important; }
      .side-section { padding:0 12px; }
      .side-title {
        color:#b9a7d6; font-size:10px; font-weight:700; text-transform:uppercase;
        letter-spacing:.08em; margin:6px 0 4px;
      }
      .sidebar-stat {
        display:flex; align-items:center; justify-content:space-between;
        padding:5px 12px; border-left:3px solid; margin:2px 10px;
        border-radius:0 3px 3px 0;
      }
      .stat-label { color:#c9bce0; font-size:11px; }
      .stat-value { font-weight:700; font-size:15px; }
      .stat-total    { border-color:#7e57c2; background:rgba(126,87,194,.12); }
      .stat-total    .stat-value { color:#b39ddb; }
      .stat-shown    { border-color:#26a69a; background:rgba(38,166,154,.12); }
      .stat-shown    .stat-value { color:#4db6ac; }
      /* Cap the Subcategories picker so a long selection scrolls instead of
         pushing the rest of the sidebar down. Selection order still drives
         facet order -- remove + reselect a tag to move it to the end. */
      .selectize-control.multi .selectize-input {
        max-height:120px; overflow-y:auto;
      }
    "))),

    tabItems(

      # --- Tab 1: Admixture Plot ---------------------------------------------
      tabItem(tabName = "admix",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = FALSE,
              title = "Admixture Plot",
              plotlyOutput("AdmixPlot", height = "700px"))
        ),
        fluidRow(
          box(width = 12, status = "info", title = "Palette",
              helpText("Recolor ancestry components. Applies to all plots."),
              tags$div(style = "display:flex; gap:28px; align-items:flex-start; flex-wrap:wrap;",
                tags$div(style = "flex:1 1 460px;", uiOutput("colorInputs")),
                tags$div(style = "display:flex; flex-direction:column; gap:8px; min-width:140px; padding-top:4px;",
                         downloadButton("dlAdmixPng", "Save PNG", class = "btn-sm"),
                         downloadButton("dlAdmixPdf", "Save PDF", class = "btn-sm"))
              ))
        )
      ),

      # --- Tab 2: Confusion Matrix --------------------------------------------
      tabItem(tabName = "confusion",
        fluidRow(
          box(width = 12, status = "primary", title = "Confusion Matrix",
              helpText("Mean ancestry proportion per dominant-K group (clustered heatmap)."),
              plotOutput("ConfusionPlot", height = "650px"))
        )
      ),

      # --- Tab 3: K Donut Plot -------------------------------------------------
      tabItem(tabName = "donut",
        fluidRow(
          box(width = 12, status = "primary", title = "Overall K Composition",
              plotlyOutput("KDonutBig", height = "420px"),
              tags$hr(),
              downloadButton("dlDonutBigPng", "Save PNG", class = "btn-sm"),
              downloadButton("dlDonutBigPdf", "Save PDF", class = "btn-sm"))
        ),
        fluidRow(
          box(width = 12, status = "primary", title = "K Composition by Category 1",
              helpText("Requires a popinfo file with Category 1 selected in the sidebar."),
              plotlyOutput("KDonutSmall", height = "520px"),
              tags$hr(),
              downloadButton("dlDonutSmallPng", "Save PNG", class = "btn-sm"),
              downloadButton("dlDonutSmallPdf", "Save PDF", class = "btn-sm"))
        )
      ),

      # --- Tab 4: Averaged Stacked Barplot -------------------------------------
      tabItem(tabName = "stacked",
        fluidRow(
          box(width = 12, status = "primary", title = "Averaged Stacked Barplot",
              helpText("Mean K composition per Category 1 group, split by Category 2 when active."),
              plotlyOutput("StackedPlot", height = "600px"),
              tags$hr(),
              downloadButton("dlStackPng", "Save PNG", class = "btn-sm"),
              downloadButton("dlStackPdf", "Save PDF", class = "btn-sm"))
        )
      ),

      # --- Tab 5: Data Table ----------------------------------------------------
      tabItem(tabName = "table",
        fluidRow(
          box(width = 12, status = "primary", title = "Admixture Data Table",
              DT::DTOutput("Ktable"))
        )
      ),

      # --- Tab 6: Instructions -----------------------------------------------
      tabItem(tabName = "instructions",
        fluidRow(
          box(width = 8, status = "primary", solidHeader = TRUE,
              title = tags$span(icon("circle-info"), " PopMosaic v2.0 — Instructions"),

              tags$h4("Overview"),
              tags$p(
                tags$strong("PopMosaic"), " is an interactive viewer for ",
                tags$code("ADMIXTURE"), " ancestry-proportion (", tags$code(".Q"),
                ") results across multiple K values. Upload the ", tags$code(".Q"),
                " files for a dataset and an optional ", tags$code("popinfo"),
                " metadata table to generate a color-coded, fully interactive ",
                "stacked-bar mosaic plot, K summary donuts, and averaged group ",
                "barplots. All files are uploaded from within the app and can ",
                "be swapped at any time without restarting."),

              tags$hr(),
              tags$h4("Getting Started"),
              tags$ol(
                tags$li("In the sidebar, upload all ", tags$strong("Q files"),
                        " for one dataset (required). Files must follow the ",
                        tags$code("<title><K>.Q"), " naming pattern (e.g. ",
                        tags$code("demo.1kgp.k8.Q"), ") — only files matching ",
                        "the first uploaded file's title are used."),
                tags$li("Pick which ", tags$strong("K"), " to display from the ",
                        "dropdown that appears once files load."),
                tags$li("Optionally upload a ", tags$strong("Popinfo"),
                        " file (same row order/count as the Q files) to unlock ",
                        "category-based filtering, grouping, and the donut / ",
                        "averaged-barplot tabs."),
                tags$li("Swap any file at any time to explore a different dataset.")
              ),

              tags$hr(),
              tags$h4("Controls"),
              tags$table(
                class = "table table-condensed table-hover", style = "font-size:13px;",
                tags$thead(tags$tr(tags$th("Control"), tags$th("What it does"))),
                tags$tbody(
                  tags$tr(tags$td(tags$strong("Select K / Title")),
                          tags$td("Choose which uploaded K value to plot and set a custom title.")),
                  tags$tr(tags$td(tags$strong("De-noise (K < 0.01)")),
                          tags$td("Zero out any component below 0.01 for each sample (checked before ",
                                  "rounding) and rescale the rest back up to sum to 1, to strip out ",
                                  "noise-level components and give a cleaner-looking plot. Applies ",
                                  "everywhere -- plots, tables, and downloads.")),
                  tags$tr(tags$td(tags$strong("Add Borders (PDF/PNG only)")),
                          tags$td("Narrow each bar slightly to show a gap between samples in the ",
                                  "static PNG/PDF export. Plotly already spaces bars on screen, so ",
                                  "this has no visible effect in the interactive plot.")),
                  tags$tr(tags$td(tags$strong("K Sort")),
                          tags$td("Within each dominant-K group, order samples by descending membership.")),
                  tags$tr(tags$td(tags$strong("Show Sample Names")),
                          tags$td("Display sample IDs on the x-axis (hidden by default for readability).")),
                  tags$tr(tags$td(tags$strong("Grouping")),
                          tags$td("Default (no panels), K Groups (facet by dominant K), or Factor ",
                                  "(facet by Category 1, and Category 2 if active).")),
                  tags$tr(tags$td(tags$strong("Category 1 / 2 + Subcategories")),
                          tags$td("Pick up to two popinfo columns to filter and group samples by. ",
                                  "Only categorical, non-ID popinfo columns are offered. Deselect ",
                                  "subcategories to exclude them; Category 1 takes priority in facet order.")),
                  tags$tr(tags$td(tags$strong("Ancestral Component / Threshold Range")),
                          tags$td("Pick a single K component and a min/max range; samples whose ",
                                  "proportion for that component falls outside the range are hidden ",
                                  "from every plot and the sample table. Default 0.00-1.00 keeps everyone.")),
                  tags$tr(tags$td(tags$strong("Palette")),
                          tags$td("Recolor each K component (Admixture Plot tab). Applies to every plot.")),
                  tags$tr(tags$td(tags$strong("Caption")),
                          tags$td("Custom footnote text shown under the Admixture Plot."))
                )
              ),

              tags$hr(),
              tags$h4("Tabs"),
              tags$ul(
                tags$li(tags$strong("Admixture Plot:"), " the main interactive mosaic — hover a segment ",
                        "for the sample name, K proportion, and current category membership."),
                tags$li(tags$strong("Confusion Matrix:"), " mean ancestry proportion per dominant-K ",
                        "group, as a clustered heatmap."),
                tags$li(tags$strong("K Donut Plot:"), " one overall donut summarizing K composition ",
                        "across displayed samples, plus a small-multiple donut per Category 1 group."),
                tags$li(tags$strong("Averaged Stacked:"), " the same averaged K composition as the ",
                        "donuts, as stacked bars — one bar per Category 1 group, split into ",
                        "Category 2 sub-bars when a second category is active."),
                tags$li(tags$strong("Data Table:"), " the underlying per-sample K proportions, ",
                        "exportable to CSV/Excel/PDF.")
              ),

              tags$hr(),
              tags$h4("Interacting with the Plots"),
              tags$ul(
                tags$li(tags$strong("Zoom / pan:"), " drag to zoom, double-click to reset (native Plotly)."),
                tags$li(tags$strong("Hover:"), " point over a bar segment or donut slice for its details."),
                tags$li(tags$strong("Legend:"), " click a K entry to toggle its visibility.")
              ),

              tags$hr(),
              tags$h4("Saving"),
              tags$p(
                "Use the Plotly camera icon for a quick interactive-view PNG, or the ",
                tags$strong("Save PNG / Save PDF"), " buttons on each tab for a ",
                "publication-quality static render (300 dpi PNG or A4-landscape PDF)."),

              tags$hr(),
              tags$h4("Input File Formats"),
              tags$p(tags$strong("Q files (.Q):"),
                     " headerless, whitespace-delimited ADMIXTURE output, one row per sample and ",
                     "one column per ancestral component. All K files for a dataset must share the ",
                     "same row order and follow ", tags$code("<title><K>.Q"), "."),
              tags$p(tags$strong("Popinfo (.tsv / .txt / .csv):"),
                     " same row order and count as the Q files. First column is typically the ",
                     "sample ID (checked against ", tags$code("Sample"), "/", tags$code("SampleID"),
                     "/", tags$code("SID"), "/", tags$code("IID"), "); remaining columns are ",
                     "metadata such as ", tags$code("POP"), ", ", tags$code("COUNTRY"), ", ",
                     tags$code("SUPERPOP"), "."),
              tags$pre(
                "K1        K2        K3\n",
                "0.981245  0.011932  0.006823\n",
                "0.023981  0.941220  0.034799\n",
                "0.015442  0.032109  0.952449"
              )
          ),

          box(width = 4, status = "info", title = tags$span(icon("user"), " Credits"),
              tags$p(tags$strong("Author")),
              tags$p(icon("envelope"), " Pavel Salazar-Fernandez", tags$br(),
                     tags$a(href = "mailto:epsalazarf@gmail.com", "epsalazarf@gmail.com")),
              tags$hr(),
              tags$p(tags$strong("Version")),
              tags$p("2.0 — Plotly engine (2026)"),
              tags$p(tags$strong("Previous")),
              tags$p("1.2 — ggplot2 engine (Sep 2025)"),
              tags$hr(),
              tags$p(tags$strong("Built with")),
              tags$ul(style = "padding-left:18px; font-size:13px;",
                tags$li(tags$a(href = "https://shiny.posit.co/", target = "_blank", "R Shiny")),
                tags$li(tags$a(href = "https://rstudio.github.io/shinydashboard/", target = "_blank", "shinydashboard")),
                tags$li(tags$a(href = "https://plotly.com/r/", target = "_blank", "Plotly")),
                tags$li(tags$a(href = "https://dt.tidyverse.org/", target = "_blank", "DT")),
                tags$li(tags$a(href = "https://www.tidyverse.org/", target = "_blank", "tidyverse")))
          )
        )
      )
    )
  )
)
# <\UI> -----------------------------------------------------------------------


# <SERVER> --------------------------------------------------------------------
server <- function(input, output, session) {

  # ----- Uploaded Q files: filter to the pattern, order by K --------------
  qfiles_list <- reactive({
    req(input$qfiles)
    df <- input$qfiles
    defaultTitle <- gsub("[0-9]+\\.Q$", "", df$name[1])
    defaultTitleEsc <- stringr::str_replace_all(
      defaultTitle, "([\\^\\$\\.\\|\\(\\)\\[\\]\\*\\+\\?\\\\])", "\\\\\\1")
    pattern <- paste0("^", defaultTitleEsc, "([0-9]+)\\.Q$")
    valid <- grepl(pattern, df$name)
    validate(need(any(valid), "No valid Q files found matching the expected <title><K>.Q pattern."))
    df <- df[valid, ]
    df$K_value <- as.numeric(sub(pattern, "\\1", df$name))
    df <- df[order(df$K_value), ]
    setNames(df$datapath, paste0("K=", df$K_value))
  })

  deftitle <- reactive({
    if (is.null(input$qfiles)) return("ADMIXTURE Plot")
    raw <- gsub("[0-9]+\\.Q$", "", input$qfiles$name[1])
    gsub("[._-]+$", "", raw)  # drop the separator left dangling before the stripped <K>.Q
  })
  title_text <- reactive({
    if (!is.null(input$plottitle) && nzchar(input$plottitle)) input$plottitle else deftitle()
  })

  # ----- Read the selected Q file ------------------------------------------
  qData <- reactive({
    req(input$Ks)
    data <- tryCatch(
      read.table(input$Ks, header = FALSE, sep = "", stringsAsFactors = FALSE),
      error = function(e) validate(need(FALSE, "Error reading the selected Q file.")))
    validate(need(all(sapply(data, is.numeric)),
                  "The Q file must contain only numeric values (headerless, space-delimited)."))
    data %>% rename_with(~ gsub("V", "K", .x))
  })

  nK <- reactive({ req(qData()); ncol(qData()) })

  # ----- Read the optional popinfo file ------------------------------------
  popinfo <- reactive({
    if (is.null(input$pifile)) return(NULL)
    df <- read_popinfo_file(input$pifile$datapath)
    validate(need(!is.null(df) && ncol(df) > 1,
                  "Could not parse popinfo as a tab/comma/space-delimited table with a header row."))
    df
  })

  popinfo_ok <- reactive({
    pi <- popinfo()
    if (is.null(pi)) return(FALSE)
    q <- tryCatch(qData(), error = function(e) NULL)
    !is.null(q) && nrow(pi) == nrow(q)
  })

  output$popinfo_warning <- renderUI({
    req(input$pifile)
    q <- tryCatch(qData(), error = function(e) NULL)
    pi <- popinfo()
    req(q, pi)
    if (nrow(q) != nrow(pi))
      tags$div(style = "color:#ffb3b3; font-size:11px; margin-top:4px;",
               "Row count mismatch: popinfo not merged.")
  })

  # Categorical popinfo columns usable for Category 1 / 2.
  catcols <- reactive({
    req(popinfo_ok())
    ci <- classify_columns(popinfo())
    ci$column[ci$is_categorical]
  })

  # ----- Sidebar controls: base block (K select, title, toggles, grouping) --
  output$controls_ui <- renderUI({
    req(qfiles_list())
    base_choices <- c("Default" = "0", "K Groups" = "1")
    choices <- if (isolate(popinfo_ok())) c(base_choices, "Factor" = "2") else base_choices
    tagList(
      selectInput("Ks", "Select K:", choices = qfiles_list()),
      textInput("plottitle", "Plot Title", value = "", placeholder = deftitle()),
      tags$hr(style = "margin:6px 0;"),
      checkboxInput("denoise", "De-noise (K < 0.01)", FALSE),
      checkboxInput("brdr", "Add Borders (PDF/PNG only)", FALSE),
      checkboxInput("ksort", "K Sort", FALSE),
      checkboxInput("xlabs", "Show Sample Names", FALSE),
      tags$hr(style = "margin:6px 0;"),
      radioButtons("autogrp", "Grouping:", choices = choices, selected = "0")
    )
  })

  # Keep the Grouping radio's "Factor" choice in sync with popinfo validity
  # without re-rendering (and resetting) the rest of controls_ui.
  observeEvent(popinfo_ok(), {
    req(!is.null(input$autogrp))
    base_choices <- c("Default" = "0", "K Groups" = "1")
    choices <- if (popinfo_ok()) c(base_choices, "Factor" = "2") else base_choices
    sel <- if (input$autogrp %in% choices) input$autogrp else "0"
    updateRadioButtons(session, "autogrp", choices = choices, selected = sel)
  })

  # ----- Category 1 / Category 2 selectors (hidden until popinfo is valid) --
  output$cat1_menu <- renderUI({
    req(popinfo_ok())
    cc <- catcols()
    req(length(cc) > 0)
    tagList(tags$hr(style = "margin:6px 0;"),
            selectInput("cat1_col", "Category 1:", choices = cc))
  })

  output$cat1_vals_menu <- renderUI({
    req(popinfo_ok(), input$cat1_col)
    df <- popinfo()
    req(input$cat1_col %in% names(df))
    vals <- sort(unique(trimws(as.character(df[[input$cat1_col]]))))
    vals <- vals[vals != "" & !is.na(vals)]
    selectizeInput("cat1_vals", "Subcategories:", choices = vals, selected = vals, multiple = TRUE)
  })

  output$cat2_toggle <- renderUI({
    req(popinfo_ok())
    cc <- catcols()
    req(length(cc) > 1)
    checkboxInput("use_cat2", "Cross with Category 2", FALSE)
  })

  output$cat2_menu <- renderUI({
    req(popinfo_ok(), isTRUE(input$use_cat2))
    cc <- setdiff(catcols(), input$cat1_col)
    req(length(cc) > 0)
    selectInput("cat2_col", "Category 2:", choices = cc)
  })

  output$cat2_vals_menu <- renderUI({
    req(popinfo_ok(), isTRUE(input$use_cat2), input$cat2_col)
    df <- popinfo()
    req(input$cat2_col %in% names(df))
    vals <- sort(unique(trimws(as.character(df[[input$cat2_col]]))))
    vals <- vals[vals != "" & !is.na(vals)]
    selectizeInput("cat2_vals", "Subcategories:", choices = vals, selected = vals, multiple = TRUE)
  })

  output$caption_ui <- renderUI({
    req(qfiles_list())
    tagList(tags$hr(style = "margin:6px 0;"), textInput("plotcaption", "Caption", value = ""))
  })

  # ----- K threshold filter: keep only samples whose value for the chosen
  # component falls inside [min, max] (range slider; default 0-1 = no-op) ---
  output$threshold_menu <- renderUI({
    req(nK())
    klevels <- paste0("K", seq_len(nK()))
    tagList(
      selectInput("k_select", "Ancestral Component:", choices = klevels),
      sliderInput("k_thrs", "Threshold Range:", min = 0, max = 1, value = c(0, 1), step = 0.01)
    )
  })

  # ----- Palette (moved under the Admixture Plot's main area; laid out as a
  # 5-column grid so it doesn't scroll into a long list as K grows) ---------
  output$colorInputs <- renderUI({
    req(nK())
    n <- nK()
    inputs <- lapply(seq_len(n), function(i) {
      colourpicker::colourInput(inputId = paste0("col", i), label = paste0("K", i),
                  value = if (i <= length(DEFAULT_PALETTE)) DEFAULT_PALETTE[i] else "#000000",
                  palette = "square")
    })
    # The grid must be built here, inside the renderUI output: CSS Grid only
    # lays out its DIRECT children, and uiOutput() wraps this in its own div,
    # so a grid declared in the static UI around uiOutput() never reaches
    # these inputs -- it was one level too high, which is why the palette
    # fell back to a plain stacked list.
    tags$div(style = "display:grid; grid-template-columns:repeat(5, minmax(90px, 1fr)); gap:2px 14px;",
             do.call(tagList, inputs))
  })
  outputOptions(output, "colorInputs", suspendWhenHidden = FALSE)

  barcolors <- reactive({
    req(nK())
    cols <- lapply(seq_len(nK()), function(i) input[[paste0("col", i)]])
    req(!any(vapply(cols, is.null, logical(1))))
    unlist(cols)
  })
  barcolors_named <- reactive({ setNames(barcolors(), paste0("K", seq_len(nK()))) })

  # ----- Sidebar sample counter ---------------------------------------------
  output$sidebar_counts <- renderUI({
    req(qData())
    df <- plotdata()
    n_total <- n_distinct(df$ID)
    n_shown <- n_distinct(df$ID[df$Flag == 1])
    stat_box <- function(cls, lbl, val)
      tags$div(class = paste("sidebar-stat", cls),
               tags$span(lbl, class = "stat-label"),
               tags$span(format(val, big.mark = ","), class = "stat-value"))
    tags$div(style = "padding:6px 0 12px;",
             stat_box("stat-total", "Samples", n_total),
             stat_box("stat-shown", "Displayed", n_shown))
  })

  # ----- Core data prep: merge, filter (Flag), order ------------------------
  plotdata <- reactive({
    req(qData())
    data <- qData()
    ok <- popinfo_ok()

    if (ok) {
      pi <- popinfo()
      data <- dplyr::bind_cols(data, pi)
      candidates <- c("Sample", "SampleID", "SID", "IID")
      present <- intersect(candidates, names(pi))
      if (length(present) > 0) {
        data$ID <- as.character(data[[present[1]]])
      } else if (nrow(pi) == length(unique(pi[[1]]))) {
        data$ID <- as.character(data[[names(pi)[1]]])
      } else {
        data$ID <- as.character(seq_len(nrow(data)))
      }
    } else if (!("ID" %in% names(data))) {
      data$ID <- as.character(seq_len(nrow(data)))
    }

    data_long <- data %>%
      pivot_longer(cols = starts_with("K"), names_to = "K", values_to = "Percent")

    if (isTRUE(input$denoise)) {
      # Zero out components below the noise floor (checked on the raw,
      # unrounded value), then rescale the rest back up to sum to 1.
      # NB: the rescale guard uses pmax() rather than ifelse(sum(Percent) > 0, ...)
      # -- with a per-group scalar condition, base ifelse() returns output the
      # length of the condition (1), not of the yes/no vectors, collapsing
      # every row in the group to a single recycled value.
      data_long <- data_long %>%
        group_by(ID) %>%
        mutate(
          Percent = ifelse(Percent < 0.01, 0, Percent),
          .gsum = sum(Percent),
          Percent = Percent / pmax(.gsum, 1e-12)
        ) %>%
        select(-.gsum) %>%
        ungroup()
    }

    data_long <- data_long %>%
      group_by(ID) %>%
      mutate(KGroup = K[which.max(Percent)], KProbability = max(Percent), Flag = 1L) %>%
      ungroup()

    cat1 <- input$cat1_col
    if (ok && !is.null(cat1) && cat1 %in% names(data_long)) {
      v1 <- as.character(data_long[[cat1]])
      sel1 <- input$cat1_vals
      if (is.null(sel1) || length(sel1) == 0) sel1 <- sort(unique(v1))
      keep <- v1 %in% sel1
      use2 <- isTRUE(input$use_cat2) && !is.null(input$cat2_col) &&
        input$cat2_col %in% names(data_long) && input$cat2_col != cat1
      if (use2) {
        v2 <- as.character(data_long[[input$cat2_col]])
        sel2 <- input$cat2_vals
        if (is.null(sel2) || length(sel2) == 0) sel2 <- sort(unique(v2))
        keep <- keep & (v2 %in% sel2)
      }
      data_long$Flag <- as.integer(keep)
      data_long[[cat1]] <- factor(v1, levels = sel1)
      if (use2) data_long[[input$cat2_col]] <- factor(as.character(data_long[[input$cat2_col]]), levels = sel2)
    }

    thr <- input$k_thrs
    if (!is.null(input$k_select) && !is.null(thr) && input$k_select %in% unique(data_long$K)) {
      kval <- data_long %>% filter(K == input$k_select) %>% select(ID, .kval = Percent)
      data_long <- data_long %>%
        left_join(kval, by = "ID") %>%
        mutate(Flag = as.integer(as.logical(Flag) & .kval >= thr[1] & .kval <= thr[2])) %>%
        select(-.kval)
    }

    if (isTRUE(input$ksort)) {
      data_long <- data_long %>% group_by(KGroup) %>% arrange(desc(Percent), .by_group = TRUE) %>% ungroup()
    }
    if (isTRUE(input$autogrp == "1")) {
      data_long <- data_long %>% arrange(KGroup, desc(KProbability))
    }
    if (isTRUE(input$autogrp == "2") && ok && !is.null(cat1) && cat1 %in% names(data_long)) {
      ord_vars <- cat1
      if (isTRUE(input$use_cat2) && !is.null(input$cat2_col) && input$cat2_col %in% names(data_long))
        ord_vars <- c(ord_vars, input$cat2_col)
      # Only impose the K-dominance sort within groups when K Sort is on;
      # otherwise keep each group's samples in their original file order
      # (arrange() is a stable sort, so ties preserve incoming row order).
      if (isTRUE(input$ksort)) {
        data_long <- data_long %>% arrange(across(all_of(ord_vars)), KGroup, desc(KProbability))
      } else {
        data_long <- data_long %>% arrange(across(all_of(ord_vars)))
      }
    }

    data_long %>% mutate(ID = forcats::fct_inorder(factor(ID)))
  })

  # Facet variables driven by the Grouping radio.
  current_facet_vars <- reactive({
    if (isTRUE(input$autogrp == "1")) return("KGroup")
    if (isTRUE(input$autogrp == "2") && popinfo_ok() && !is.null(input$cat1_col)) {
      fv <- input$cat1_col
      if (isTRUE(input$use_cat2) && !is.null(input$cat2_col) && input$cat2_col != input$cat1_col)
        fv <- c(fv, input$cat2_col)
      return(fv)
    }
    character(0)
  })

  # Displayed (Flag==1) rows, with K releveled and a hover-text column.
  admix_render_df <- reactive({
    df <- plotdata() %>% filter(Flag == 1)
    validate(need(nrow(df) > 0, "No samples to display with the current filters."))
    klevels <- paste0("K", seq_len(nK()))
    df$K <- factor(df$K, levels = klevels)
    cat1 <- input$cat1_col
    has1 <- popinfo_ok() && !is.null(cat1) && cat1 %in% names(df)
    use2 <- has1 && isTRUE(input$use_cat2) && !is.null(input$cat2_col) && input$cat2_col %in% names(df)
    df$.hover <- paste0("Sample: ", df$ID, "<br>", df$K, ": ", sprintf("%.2f", df$Percent))
    if (has1) df$.hover <- paste0(df$.hover, "<br>", cat1, ": ", df[[cat1]])
    if (use2) df$.hover <- paste0(df$.hover, "<br>", input$cat2_col, ": ", df[[input$cat2_col]])
    df
  })

  # =========================================================================
  # ADMIXTURE PLOT (native Plotly, proportional-width facets)
  # =========================================================================
  output$AdmixPlot <- renderPlotly({
    df <- admix_render_df()
    fv <- current_facet_vars()
    res <- plotly_stacked_admix(df, fv, barcolors_named(), isTRUE(input$xlabs))
    p <- res$plot

    anns <- list()
    if (!is.null(res$strips)) {
      mid <- rowMeans(res$domains)
      nfac <- length(res$strips)
      fsize <- if (nfac > 16) 8 else if (nfac > 8) 9.5 else 11
      angle <- if (nfac > 8) -35 else 0
      anns <- lapply(seq_along(res$strips), function(i)
        list(text = res$strips[i], x = mid[i], y = 1.03, xref = "paper", yref = "paper",
             showarrow = FALSE, xanchor = if (angle != 0) "left" else "center", yanchor = "bottom",
             textangle = angle, font = list(size = fsize, color = "#444")))
    }
    cap <- input$plotcaption
    has_cap <- !is.null(cap) && nzchar(cap)
    tick_space <- if (isTRUE(input$xlabs)) 90 else 0
    if (has_cap) {
      # Anchored to the bottom of the plotting area (y=0) with a fixed pixel
      # yshift, so it always lands inside the reserved bottom margin instead
      # of drifting outside it (which clipped it entirely when xlabs was off).
      anns <- c(anns, list(list(text = cap, x = 0, y = 0, xref = "paper", yref = "paper",
                                 showarrow = FALSE, xanchor = "left", yanchor = "top",
                                 yshift = -(tick_space + 22),
                                 font = list(size = 11, color = "#555"))))
    }
    bmargin <- 40 + tick_space + (if (has_cap) 25 else 0)

    p %>%
      layout(title = list(text = title_text(), x = 0.5, y = 0.99, yanchor = "top"),
             margin = list(t = 90, b = bmargin),
             bargap = 0,
             annotations = anns) %>%
      config(displaylogo = FALSE,
             toImageButtonOptions = list(format = "png", filename = title_text()))
  })

  # =========================================================================
  # CONFUSION MATRIX (unchanged: static pheatmap)
  # =========================================================================
  meanmatrix <- reactive({
    req(plotdata())
    plotdata() %>%
      filter(Flag == 1) %>%
      group_by(KGroup, K) %>%
      summarise(MeanPercent = mean(Percent), .groups = "drop") %>%
      pivot_wider(names_from = K, values_from = MeanPercent) %>%
      column_to_rownames(var = "KGroup") %>%
      as.matrix()
  })

  output$ConfusionPlot <- renderPlot({
    req(meanmatrix(), nK())
    n <- nK()
    cols <- barcolors()
    annocol <- list(
      Group  = setNames(cols, paste0("K", 1:n)),
      `Comp.` = setNames(cols, paste0("K", 1:n))
    )
    pheatmap(meanmatrix(), display_numbers = TRUE, number_color = "black",
             fontsize_number = 8, scale = "none",
             annotation_colors = annocol,
             annotation_row = data.frame(Group = paste0("K", 1:n), row.names = paste0("K", 1:n)),
             annotation_col = data.frame(`Comp.` = paste0("K", 1:n), row.names = paste0("K", 1:n)),
             main = title_text(),
             angle_col = 0, border_color = "white",
             legend = FALSE, annotation_legend = FALSE,
             color = c("gray90", colorRampPalette(rev(brewer.pal(11, "Spectral")))(100)))
  })

  # =========================================================================
  # K DONUT PLOTS (native Plotly pies)
  # =========================================================================
  KAggrBig <- reactive({
    df <- plotdata() %>% filter(Flag == 1)
    validate(need(nrow(df) > 0, "No samples to display."))
    klevels <- paste0("K", seq_len(nK()))
    agg <- df %>% mutate(K = factor(K, levels = klevels)) %>%
      group_by(K) %>% summarise(Percent = sum(Percent), .groups = "drop")
    agg$Fraction <- agg$Percent / sum(agg$Percent)
    agg
  })

  output$KDonutBig <- renderPlotly({
    agg <- KAggrBig()
    colors <- barcolors_named()
    plot_ly(agg, labels = ~K, values = ~Fraction, type = "pie", hole = 0.55,
            marker = list(colors = colors[as.character(agg$K)], line = list(color = "white", width = 1)),
            textinfo = "label+text", text = ~sprintf("%.2f", Fraction),
            hovertext = ~paste0(K, ": ", sprintf("%.2f", Fraction)), hoverinfo = "text") %>%
      layout(title = list(text = title_text(), x = 0.5), showlegend = TRUE, margin = list(t = 60)) %>%
      config(displaylogo = FALSE)
  })

  KAggrByCat1 <- reactive({
    req(popinfo_ok(), input$cat1_col)
    df <- plotdata() %>% filter(Flag == 1)
    cat1 <- input$cat1_col
    validate(need(cat1 %in% names(df), "Category 1 not available."))
    klevels <- paste0("K", seq_len(nK()))
    df %>% mutate(K = factor(K, levels = klevels)) %>%
      group_by(.data[[cat1]], K) %>% summarise(Percent = sum(Percent), .groups = "drop") %>%
      group_by(.data[[cat1]]) %>% mutate(Fraction = Percent / sum(Percent)) %>% ungroup()
  })

  output$KDonutSmall <- renderPlotly({
    validate(need(popinfo_ok() && !is.null(input$cat1_col),
                  "Requires a popinfo file with Category 1 selected."))
    agg <- KAggrByCat1()
    cat1 <- input$cat1_col
    groups <- unique(as.character(agg[[cat1]]))
    if (!is.null(input$cat1_vals)) groups <- intersect(input$cat1_vals, groups)
    validate(need(length(groups) > 0, "No groups to display."))
    n <- length(groups)
    ncolg <- ceiling(sqrt(n)); nrowg <- ceiling(n / ncolg)
    colors <- barcolors_named()
    pad <- 0.025
    p <- plot_ly()
    anns <- list()
    for (i in seq_along(groups)) {
      g <- groups[i]
      sub <- agg[as.character(agg[[cat1]]) == g, , drop = FALSE]
      row <- (i - 1) %/% ncolg; col <- (i - 1) %% ncolg
      x0 <- col / ncolg + pad; x1 <- (col + 1) / ncolg - pad
      y1 <- 1 - row / nrowg - pad; y0 <- 1 - (row + 1) / nrowg + pad
      p <- add_pie(p, data = sub, labels = ~K, values = ~Fraction, hole = 0.55,
                   domain = list(x = c(x0, x1), y = c(y0, y1)),
                   marker = list(colors = colors[as.character(sub$K)], line = list(color = "white", width = 1)),
                   textinfo = "none", hoverinfo = "text",
                   text = ~paste0(g, "<br>", K, ": ", sprintf("%.2f", Fraction)),
                   showlegend = (i == 1), name = g)
      anns[[i]] <- list(text = g, x = (x0 + x1) / 2, y = y1 + 0.015, xref = "paper", yref = "paper",
                         showarrow = FALSE, xanchor = "center", yanchor = "bottom", font = list(size = 11))
    }
    p %>% layout(annotations = anns, margin = list(t = 20, b = 10)) %>% config(displaylogo = FALSE)
  })

  # =========================================================================
  # AVERAGED STACKED BARPLOT (ggplot2 + ggplotly)
  # =========================================================================
  StackedAggr <- reactive({
    req(popinfo_ok(), input$cat1_col)
    df <- plotdata() %>% filter(Flag == 1)
    cat1 <- input$cat1_col
    validate(need(cat1 %in% names(df), "Category 1 not available."))
    use2 <- isTRUE(input$use_cat2) && !is.null(input$cat2_col) &&
      input$cat2_col %in% names(df) && input$cat2_col != cat1
    klevels <- paste0("K", seq_len(nK()))
    grp_vars <- if (use2) c(cat1, input$cat2_col) else cat1
    df %>% mutate(K = factor(K, levels = klevels)) %>%
      group_by(across(all_of(c(grp_vars, "K")))) %>%
      summarise(MeanPercent = mean(Percent), .groups = "drop")
  })

  build_gg_stacked <- function() {
    agg <- StackedAggr()
    cat1 <- input$cat1_col
    use2 <- isTRUE(input$use_cat2) && !is.null(input$cat2_col) &&
      input$cat2_col %in% names(agg) && input$cat2_col != cat1
    xcol <- if (use2) input$cat2_col else cat1
    agg$.hover <- paste0(cat1, ": ", agg[[cat1]],
                          if (use2) paste0("<br>", input$cat2_col, ": ", agg[[input$cat2_col]]) else "",
                          "<br>", agg$K, ": ", sprintf("%.2f", agg$MeanPercent))
    p <- ggplot(agg, aes(x = .data[[xcol]], y = MeanPercent, fill = K, text = .hover)) +
      geom_col(width = 0.8, color = "white", linewidth = 0.2) +
      scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
      scale_fill_manual(values = barcolors_named()) +
      theme_minimal() +
      labs(title = title_text(), x = NULL, y = "Mean Ancestry Proportion", caption = input$plotcaption) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", hjust = 0.5))
    if (use2) p <- p + facet_grid(stats::as.formula(paste("~", cat1)), scales = "free", space = "free")
    p
  }

  output$StackedPlot <- renderPlotly({
    validate(need(popinfo_ok() && !is.null(input$cat1_col),
                  "Requires a popinfo file with Category 1 selected."))
    ggplotly(build_gg_stacked(), tooltip = "text") %>% config(displaylogo = FALSE)
  })

  # =========================================================================
  # DATA TABLE
  # =========================================================================
  table_data <- reactive({
    df <- plotdata() %>% filter(Flag == 1)
    pivot_wider(df, names_from = K, values_from = Percent) %>%
      select(-Flag, -KProbability) %>%
      relocate(KGroup, .after = last_col())
  })

  output$Ktable <- DT::renderDT({
    table_data()
  }, rownames = FALSE, filter = "top",
     extensions = "Buttons",
     options = list(pageLength = 25, lengthMenu = c(25, 50, 100), paging = TRUE, scrollX = TRUE,
                    dom = 'l<"sep">Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf')),
     server = FALSE)

  # =========================================================================
  # STATIC EXPORT (ggplot2 — no kaleido/orca needed)
  # =========================================================================
  build_gg_admix <- function() {
    df <- plotdata() %>% filter(Flag == 1)
    klevels <- paste0("K", seq_len(nK()))
    df$K <- factor(df$K, levels = klevels)
    p <- ggplot(df, aes(x = ID, y = Percent, fill = K)) +
      geom_col(width = if (isTRUE(input$brdr)) 0.85 else 1) +
      scale_y_continuous(labels = function(x) sprintf("%.2f", x), expand = c(0, 0)) +
      scale_fill_manual(values = barcolors_named()) +
      theme_minimal() +
      labs(title = title_text(), subtitle = paste("K =", nK(), "; Samples =", n_distinct(df$ID)),
           x = NULL, y = NULL, caption = input$plotcaption) +
      theme(panel.grid = element_blank(), legend.position = "none",
            panel.spacing = unit(2, "pt"),
            plot.title = element_text(face = "bold", hjust = 0.5))
    if (isTRUE(input$autogrp == "1")) {
      p <- p + facet_grid(~ KGroup, scales = "free", space = "free")
    } else if (isTRUE(input$autogrp == "2") && popinfo_ok() && !is.null(input$cat1_col)) {
      cat1 <- input$cat1_col
      frm <- if (isTRUE(input$use_cat2) && !is.null(input$cat2_col) && input$cat2_col != cat1)
        paste("~", cat1, "+", input$cat2_col) else paste("~", cat1)
      p <- p + facet_grid(stats::as.formula(frm), scales = "free", space = "free")
    }
    if (isTRUE(input$xlabs)) {
      p <- p + theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.5))
    } else {
      p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }
    p
  }

  build_gg_donut <- function(agg, subtitle = NULL) {
    agg <- agg %>% mutate(ymax = cumsum(Fraction), ymin = c(0, utils::head(ymax, -1)),
                          labelPosition = (ymax + ymin) / 2,
                          labelPct = paste0(K, "\n", sprintf("%.2f", Fraction)))
    ggplot(agg, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = K)) +
      geom_rect() + coord_polar(theta = "y") + xlim(c(1.5, 4)) +
      geom_label(aes(x = 3.5, y = labelPosition, label = labelPct), size = 3.2, show.legend = FALSE) +
      scale_fill_manual(values = barcolors_named()) +
      theme_void() +
      labs(title = title_text(), subtitle = subtitle) +
      theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))
  }
  build_gg_donut_big   <- function() build_gg_donut(KAggrBig())
  build_gg_donut_small <- function() {
    cat1 <- input$cat1_col
    agg <- KAggrByCat1() %>%
      group_by(.data[[cat1]]) %>%
      mutate(ymax = cumsum(Fraction), ymin = c(0, utils::head(ymax, -1)),
             labelPosition = (ymax + ymin) / 2,
             labelPct = paste0(K, "\n", sprintf("%.2f", Fraction))) %>%
      ungroup()
    ggplot(agg, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = K)) +
      geom_rect() + coord_polar(theta = "y") + xlim(c(1.5, 4)) +
      facet_wrap(stats::as.formula(paste("~", cat1))) +
      scale_fill_manual(values = barcolors_named()) +
      theme_void() +
      labs(title = paste0(title_text(), " by ", cat1)) +
      theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  }

  save_plot <- function(file, plot, device) {
    ggplot2::ggsave(file, plot = plot, device = device, width = 297, height = 105,
                    units = "mm", dpi = 300, scale = 1.1,
                    bg = if (device == "png") "white" else NULL)
  }

  output$dlAdmixPng <- downloadHandler(
    filename = function() paste0(title_text(), ".admixture.png"),
    content  = function(file) save_plot(file, build_gg_admix(), "png"))
  output$dlAdmixPdf <- downloadHandler(
    filename = function() paste0(title_text(), ".admixture.pdf"),
    content  = function(file) save_plot(file, build_gg_admix(), "pdf"))

  output$dlDonutBigPng <- downloadHandler(
    filename = function() paste0(title_text(), ".donut.png"),
    content  = function(file) save_plot(file, build_gg_donut_big(), "png"))
  output$dlDonutBigPdf <- downloadHandler(
    filename = function() paste0(title_text(), ".donut.pdf"),
    content  = function(file) save_plot(file, build_gg_donut_big(), "pdf"))

  output$dlDonutSmallPng <- downloadHandler(
    filename = function() paste0(title_text(), ".donut_by_cat1.png"),
    content  = function(file) save_plot(file, build_gg_donut_small(), "png"))
  output$dlDonutSmallPdf <- downloadHandler(
    filename = function() paste0(title_text(), ".donut_by_cat1.pdf"),
    content  = function(file) save_plot(file, build_gg_donut_small(), "pdf"))

  output$dlStackPng <- downloadHandler(
    filename = function() paste0(title_text(), ".stacked_avg.png"),
    content  = function(file) save_plot(file, build_gg_stacked(), "png"))
  output$dlStackPdf <- downloadHandler(
    filename = function() paste0(title_text(), ".stacked_avg.pdf"),
    content  = function(file) save_plot(file, build_gg_stacked(), "pdf"))
}
# <\SERVER> ---------------------------------------------------------------


# <APP> ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# <END> -----------------------------------------------------------------------
