# <ABOUT> =====================================================================
# Title       : PopCanvas v2.0 — Interactive PCA Plotter (R Shiny)
# Description : An interactive R Shiny dashboard for exploring PLINK PCA results.
#               Reads .eigenvec / .eigenval files plus an optional POPINFO file,
#               uploaded live from within the app, and renders them on the Plotly
#               engine (2D and 3D). Preserves the v1.0 feature set — population
#               masking, population emphasis, group coloring, centroids, axis
#               flipping, per-axis component selection — and adds in-app file
#               switching, a 3D PCA explorer, an interactive data table and a
#               built-in instructions panel.
#
# Dependencies: shiny, shinydashboard, plotly, tidyverse (dplyr/readr/tibble),
#               scales, DT, viridisLite
#
# Engine note : v1.0 rendered with ggplot2 (static, brush-zoom). v2.0 renders
#               with Plotly for native pan/zoom/hover/box-select. Static PNG/PDF
#               export is still provided via a ggplot2 fallback (no kaleido/orca
#               system dependency required).
#
# Author      : Pavel Salazar-Fernandez (epsalazarf@gmail.com)
# Version     : 2.0
# Usage       : shiny::runApp('PopCanvas-app-v2.0.R')
# =============================================================================

# <START> ---------------------------------------------------------------------
message("> Starting: PopCanvas v2.0 — PCA Visualizer dashboard...")

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(plotly)
  library(dplyr)
  library(readr)
  library(tibble)
  library(scales)
  library(DT)
  library(viridisLite)
})

options(shiny.maxRequestSize = 200 * 1024^2)  # allow large eigenvec uploads (200 MB)

# <PREPARATIONS> --------------------------------------------------------------

# Checks if input is a factor and, if not, retags it (NA kept as a level).
refact <- function(x) {
  if (!is.factor(x)) x <- factor(x)
  ll <- as.character(na.omit(unique(x)))
  if (anyNA(x)) ll <- c(ll, NA)
  factor(x, levels = ll, exclude = NULL)
}

# Robust column existence check.
col_exists <- function(df, col) !is.null(df) && col %in% names(df)

# Build a named color vector (level -> hex) for a categorical field, mirroring
# v1.0's rainbow(s = 0.5, v = 0.9) palette. NA-derived rows get a neutral grey.
# If coloring by POP and a COLOR column is supplied, those hex codes are honored.
assign_colors <- function(df, fld) {
  vals <- as.character(df[[fld]])
  vals[is.na(vals)] <- "(NA)"

  if (identical(fld, "POP") && col_exists(df, "COLOR")) {
    lut <- df %>%
      mutate(.POP = as.character(.data[["POP"]]),
             .COL = as.character(.data[["COLOR"]])) %>%
      distinct(.POP, .COL) %>%
      filter(!is.na(.POP), !is.na(.COL))
    pal <- setNames(
      ifelse(grepl("^#", lut$.COL), lut$.COL, paste0("#", lut$.COL)),
      lut$.POP
    )
    miss <- setdiff(unique(vals), names(pal))
    if (length(miss)) pal <- c(pal, setNames(rep("#999999", length(miss)), miss))
  } else {
    lv_noNA <- setdiff(unique(vals), "(NA)")
    pal <- setNames(rainbow(length(lv_noNA), s = 0.5, v = 0.9), lv_noNA)
    if ("(NA)" %in% vals) pal <- c(pal, "(NA)" = "#999999")
  }

  df$.grp <- factor(vals, levels = names(pal))
  list(df = df, pal = pal)
}

# Columns from popinfo that are usable as grouping/coloring fields.
group_fields <- function(df) {
  if (is.null(df)) return(character(0))
  nuniq <- vapply(df, function(x) length(unique(x)), integer(1))
  nm    <- names(nuniq[nuniq > 1 & nuniq < nrow(df)])
  setdiff(nm, c("ID", grep("^PC[0-9]+$", names(df), value = TRUE)))
}

# Metadata columns worth showing in hover text / selection info.
META_PREF <- c("ID", "POP", "POPULATION", "POP_SIMPLE", "SEX",
               "MLABEL", "SLABEL", "COUNTRY", "SUPERPOP", "CONTINENT")

# <\PREPARATIONS> -------------------------------------------------------------


# <UI> ------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "purple",

  dashboardHeader(title = "PopCanvas ❧ PCA", titleWidth = 280),

  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "main_menu",
      menuItem("2D PCA Plot",   tabName = "plot2d",       icon = icon("braille")),
      menuItem("3D PCA Plot",   tabName = "plot3d",       icon = icon("cube")),
      menuItem("Data Table",    tabName = "table",        icon = icon("table")),
      menuItem("Instructions",  tabName = "instructions", icon = icon("circle-info"))
    ),

    tags$hr(style = "border-color:#5a4a78; margin:4px 8px;"),

    # ----- Data input (always visible) -----------------------------------
    tags$div(
      class = "side-section",
      tags$p("Data Input", class = "side-title"),
      fileInput("eigfile", "Eigenvec (.eigenvec) — required",
                multiple = FALSE,
                accept = c(".eigenvec", ".evec", ".txt", ".tsv", ".csv")),
      fileInput("evalfile", "Eigenval (.eigenval) — optional",
                multiple = FALSE,
                accept = c(".eigenval", ".eval", ".txt")),
      fileInput("pifile", "Popinfo (.tsv/.txt) — optional",
                multiple = FALSE,
                accept = c(".tsv", ".txt", ".csv"))
    ),

    tags$hr(style = "border-color:#5a4a78; margin:4px 8px;"),

    # ----- Plot controls (rendered once data is loaded) ------------------
    tags$div(class = "side-section",
             tags$p("Settings", class = "side-title"),
             uiOutput("controls_ui")),

    # ----- Sample counter ------------------------------------------------
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
    "))),

    tabItems(

      # --- Tab 1: 2D PCA -----------------------------------------------------
      tabItem(tabName = "plot2d",
        fluidRow(
          box(width = 9, status = "primary", solidHeader = FALSE,
              title = "2D PCA",
              plotlyOutput("pca2d", height = "1080px")),
          box(width = 3, status = "info", title = "Selection Info",
              helpText("Use the Plotly box / lasso select tool to inspect points."),
              verbatimTextOutput("selinfo"),
              tags$hr(),
              downloadButton("dlPlotpng", "Save PNG", class = "btn-sm"),
              downloadButton("dlPlotpdf", "Save PDF", class = "btn-sm"))
        )
      ),

      # --- Tab 2: 3D PCA -----------------------------------------------------
      tabItem(tabName = "plot3d",
        fluidRow(
          box(width = 12, status = "primary", title = "3D PCA Explorer",
              plotlyOutput("pca3d", height = "1080px"))
        )
      ),

      # --- Tab 3: Data Table -------------------------------------------------
      tabItem(tabName = "table",
        fluidRow(
          box(width = 12, status = "primary", title = "PCA Data Table",
              downloadButton("dlTable", "Export TSV", class = "btn-sm"),
              tags$br(), tags$br(),
              DT::dataTableOutput("pca_table"))
        )
      ),

      # --- Tab 4: Instructions ----------------------------------------------
      tabItem(tabName = "instructions",
        fluidRow(
          box(width = 8, status = "primary", solidHeader = TRUE,
              title = tags$span(icon("circle-info"), " PopCanvas v2.0 — Instructions"),

              tags$h4("Overview"),
              tags$p(
                tags$strong("PopCanvas"), " is an interactive viewer for ",
                tags$abbr(title = "Principal Component Analysis", "PCA"),
                " results produced by ", tags$code("PLINK"), ". Upload the ",
                tags$code(".eigenvec"), " / ", tags$code(".eigenval"),
                " files and an optional ", tags$code("popinfo"),
                " metadata table to generate a color-coded, fully interactive ",
                "2D and 3D PCA plot. All files are uploaded from within the app ",
                "and can be swapped at any time without restarting."),

              tags$hr(),
              tags$h4("Getting Started"),
              tags$ol(
                tags$li("In the sidebar, upload an ", tags$strong("Eigenvec"),
                        " file (required). The plot appears as soon as it loads."),
                tags$li("Optionally upload an ", tags$strong("Eigenval"),
                        " file to label each axis with its % of variance explained."),
                tags$li("Optionally upload a ", tags$strong("Popinfo"),
                        " file to unlock coloring, masking, emphasis and centroids."),
                tags$li("Swap any file at any time to explore a different dataset.")
              ),

              tags$hr(),
              tags$h4("Controls"),
              tags$table(
                class = "table table-condensed table-hover", style = "font-size:13px;",
                tags$thead(tags$tr(tags$th("Control"), tags$th("What it does"))),
                tags$tbody(
                  tags$tr(tags$td(tags$strong("Title / Caption")),
                          tags$td("Custom plot title and caption text.")),
                  tags$tr(tags$td(tags$strong("X / Y / Z component")),
                          tags$td("Pick which principal component maps to each axis (Z used in 3D).")),
                  tags$tr(tags$td(tags$strong("Flip X / Y axis")),
                          tags$td("Reverse an axis direction to match a reference orientation.")),
                  tags$tr(tags$td(tags$strong("Type")),
                          tags$td("Render samples as points, ID text, or population labels.")),
                  tags$tr(tags$td(tags$strong("Populations displayed")),
                          tags$td("Mask the plot to only the chosen population(s). Empty = show all.")),
                  tags$tr(tags$td(tags$strong("Population emphasis")),
                          tags$td("Highlight one population with distinct markers on top of the rest.")),
                  tags$tr(tags$td(tags$strong("Group coloring")),
                          tags$td("Color points by any categorical column in the popinfo file.")),
                  tags$tr(tags$td(tags$strong("Group centroids")),
                          tags$td("Overlay a diamond marker + label at each group's mean position.")),
                  tags$tr(tags$td(tags$strong("Legend")),
                          tags$td("Toggle the color legend."))
                )
              ),

              tags$hr(),
              tags$h4("Interacting with the Plot"),
              tags$ul(
                tags$li(tags$strong("Zoom / pan:"), " drag to zoom, double-click to reset (native Plotly)."),
                tags$li(tags$strong("Hover:"), " point over a sample to see its ID and metadata."),
                tags$li(tags$strong("Select:"), " use the box / lasso tool to populate the ",
                        tags$em("Selection Info"), " panel with the chosen samples' metadata."),
                tags$li(tags$strong("3D:"), " click-drag to rotate, scroll to zoom in the 3D tab.")
              ),

              tags$hr(),
              tags$h4("Saving"),
              tags$p(
                "Use the Plotly camera icon for a quick interactive-view PNG, or the ",
                tags$strong("Save PNG / Save PDF"), " buttons for a publication-quality ",
                "static render of the current 2D plot."),

              tags$hr(),
              tags$h4("Input File Formats"),
              tags$p(tags$strong("Eigenvec (.eigenvec):"),
                     " whitespace/tab-delimited with a header. First columns ",
                     tags$code("FID"), " / ", tags$code("IID"),
                     " (FID dropped automatically), followed by PC columns."),
              tags$p(tags$strong("Eigenval (.eigenval):"),
                     " one eigenvalue per line, in component order."),
              tags$p(tags$strong("Popinfo (.tsv / .txt):"),
                     " first column = sample ID (must match ", tags$code("IID"),
                     "); remaining columns = metadata such as ", tags$code("POP"),
                     ", ", tags$code("POPULATION"), ", ", tags$code("CONTINENT"),
                     ". An optional ", tags$code("COLOR"),
                     " column (hex codes) overrides the auto palette when coloring by POP."),
              tags$pre(
                "ID       POP   POP_SIMPLE   COUNTRY          SUPERPOP   CONTINENT\n",
                "HG00096  GBR   British      United_Kingdom   EUR        Europe\n",
                "HG00097  GBR   British      United_Kingdom   EUR        Europe\n",
                "NA18525  CHB   Han          China            EAS        East_Asia"
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
              tags$p("1.0 — ggplot2 engine (Aug 2025)"),
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

  # ----- Default title from the uploaded eigenvec name -----------------------
  deftitle <- reactive({
    if (is.null(input$eigfile)) "PCA Plot"
    else tools::file_path_sans_ext(basename(input$eigfile$name))
  })

  # ----- Read & parse the eigenvec file --------------------------------------
  pca_raw <- reactive({
    req(input$eigfile)
    df <- read_delim(input$eigfile$datapath, show_col_types = FALSE,
                     trim_ws = TRUE)
    names(df) <- gsub("^#", "", names(df))
    names(df) <- gsub("IID", "ID", names(df))
    df <- df[, !grepl("FID", names(df)), drop = FALSE]

    pccols <- which(vapply(df, is.numeric, logical(1)))
    validate(need(length(pccols) >= 2,
                  "Eigenvec must contain at least two numeric PC columns."))
    idcol <- min(pccols) - 1L
    if (idcol < 1L) {                       # no ID column -> synthesize one
      df <- tibble(ID = paste0("S", seq_len(nrow(df)))) %>% bind_cols(df)
      pccols <- pccols + 1L; idcol <- 1L
    }
    names(df)[idcol]  <- "ID"
    names(df)[pccols] <- paste0("PC", seq_along(pccols))
    df$ID <- as.character(df$ID)
    df[, c("ID", paste0("PC", seq_along(pccols))), drop = FALSE]
  })

  # ----- Read the optional eigenval file -------------------------------------
  pca_eval <- reactive({
    if (is.null(input$evalfile)) return(NULL)
    v <- scan(input$evalfile$datapath, quiet = TRUE, blank.lines.skip = TRUE)
    v[is.finite(v)]
  })

  # ----- Read the optional popinfo file --------------------------------------
  popinfo <- reactive({
    if (is.null(input$pifile)) return(NULL)
    pi <- read_delim(input$pifile$datapath, show_col_types = FALSE, trim_ws = TRUE)
    names(pi)[1] <- "ID"
    pi$ID <- as.character(pi$ID)
    pi
  })

  # ----- Merge PCA + popinfo (left join keeps every PCA sample) --------------
  pca_data <- reactive({
    df <- pca_raw()
    pi <- popinfo()
    if (!is.null(pi)) {
      df <- left_join(df, pi, by = "ID")
      # Re-factor low-cardinality metadata columns so NA is a real level.
      meta <- setdiff(names(df), c("ID", grep("^PC[0-9]+$", names(df), value = TRUE)))
      for (cc in meta) {
        u <- length(unique(df[[cc]]))
        if (u > 1 && u < nrow(df)) df[[cc]] <- refact(df[[cc]])
      }
    }
    df
  })

  ncomps     <- reactive(sum(grepl("^PC[0-9]+$", names(pca_data()))))
  has_pop    <- reactive(col_exists(pca_data(), "POP"))
  fields     <- reactive(group_fields(pca_data()))
  pop_levels <- reactive(if (has_pop()) sort(unique(as.character(pca_data()$POP))) else character(0))

  # Axis labels with optional % variance explained.
  pc_label <- function(i) {
    ev <- pca_eval()
    base <- paste0("PC", i)
    if (is.null(ev) || i > length(ev)) return(base)
    paste0(base, " (", percent(ev[i] / sum(ev), accuracy = 0.1), ")")
  }

  # ----- Dynamic sidebar controls (appear once an eigenvec is loaded) --------
  output$controls_ui <- renderUI({
    req(input$eigfile)
    nc  <- ncomps()
    fld <- fields()
    pops <- pop_levels()

    tagList(
      textInput("plottitle", "Title", value = ""),
      div(style = "display:flex; gap:6px;",
          numericInput("PCx", "X", value = 1, min = 1, max = nc, step = 1),
          numericInput("PCy", "Y", value = 2, min = 1, max = nc, step = 1),
          numericInput("PCz", "Z", value = min(3, nc), min = 1, max = nc, step = 1)),
      div(style = "display:flex; gap:12px;",
          checkboxInput("flipx", "Flip X", FALSE),
          checkboxInput("flipy", "Flip Y", FALSE)),
      radioButtons("type", "Type:",
                   choices = c("Points" = 1, "Text (ID)" = 2, "Labels (POP)" = 3),
                   selected = 1, inline = TRUE),

      if (length(pops) > 0) tagList(
        tags$hr(style = "margin:6px 0;"),
        selectizeInput("pops", "Populations displayed:",
                       choices = pops, selected = NULL, multiple = TRUE,
                       options = list(placeholder = "All populations")),
        selectizeInput("pope", "Population emphasis:",
                       choices = c("None" = "", pops), selected = "",
                       multiple = FALSE)
      ),

      if (length(fld) > 0) tagList(
        tags$hr(style = "margin:6px 0;"),
        selectizeInput("flds", "Group coloring:",
                       choices = fld,
                       selected = if ("POP" %in% fld) "POP" else fld[1],
                       options = list(maxItems = 1)),
        checkboxInput("cntrd", "Group centroids", value = FALSE),
        checkboxInput("legon", "Legend", value = TRUE)
      ),

      tags$hr(style = "margin:6px 0;"),
      textInput("plotcaption", "Caption", value = "")
    )
  })

  # ----- Sample counter ------------------------------------------------------
  output$sidebar_counts <- renderUI({
    req(input$eigfile)
    n_total <- nrow(pca_data())
    n_shown <- nrow(plot_base())
    stat_box <- function(cls, lbl, val)
      tags$div(class = paste("sidebar-stat", cls),
               tags$span(lbl, class = "stat-label"),
               tags$span(format(val, big.mark = ","), class = "stat-value"))
    tags$div(style = "padding:6px 0 12px;",
             stat_box("stat-total", "Samples", n_total),
             stat_box("stat-shown", "Displayed", n_shown))
  })

  # ----- Core data prep: masking + color assignment --------------------------
  # Returns the displayed (masked) data; emphasis split is done per-render.
  plot_base <- reactive({
    df <- pca_data(); req(nrow(df) > 0)
    if (has_pop() && !is.null(input$pops) && length(input$pops) > 0)
      df <- df[as.character(df$POP) %in% input$pops, , drop = FALSE]
    df
  })

  active_field <- reactive({
    fld <- input$flds
    if (!is.null(fld) && nzchar(fld) && col_exists(plot_base(), fld)) fld else NULL
  })

  # Build hover text from available metadata columns.
  hover_text <- function(df) {
    cols <- intersect(META_PREF, names(df))
    if (length(cols) == 0) cols <- "ID"
    apply(df[, cols, drop = FALSE], 1, function(r)
      paste(cols, r, sep = ": ", collapse = "<br>"))
  }

  # =========================================================================
  # 2D PLOTLY
  # =========================================================================
  output$pca2d <- renderPlotly({
    df <- plot_base(); req(nrow(df) > 0)
    pcx <- paste0("PC", input$PCx); pcy <- paste0("PC", input$PCy)
    validate(need(all(c(pcx, pcy) %in% names(df)), "Selected components not in data."))

    fld <- active_field()

    # Emphasis split
    emph <- NULL
    if (has_pop() && !is.null(input$pope) && nzchar(input$pope)) {
      em <- as.character(df$POP) == input$pope
      emph <- df[em, , drop = FALSE]
      df   <- df[!em, , drop = FALSE]
    }

    # Coloring
    if (!is.null(fld)) {
      ac  <- assign_colors(df, fld); df <- ac$df; pal <- ac$pal
    } else {
      df$.grp <- factor("samples"); pal <- c(samples = "#5b3f8e")
    }

    disp_mode <- if (input$type == 1) "markers" else "text"
    disp_text <- if (input$type == 2) df$ID
                 else if (input$type == 3 && has_pop()) as.character(df$POP)
                 else NULL

    mk <- if (disp_mode == "markers")
      list(size = 9, opacity = 0.7, line = list(color = "white", width = 0.4))
    else NULL
    tf <- if (disp_mode == "text") list(size = 10) else NULL

    p <- plot_ly(source = "pca2d") %>%
      add_trace(
        data = df, x = df[[pcx]], y = df[[pcy]],
        type = "scatter", mode = disp_mode,
        color = df$.grp, colors = pal,
        text = disp_text, textposition = "middle center",
        textfont = tf,
        hovertext = hover_text(df), hoverinfo = "text",
        customdata = df$ID,
        marker = mk
      )

    # Centroids
    if (isTRUE(input$cntrd) && !is.null(fld)) {
      cen <- df %>%
        group_by(.grp) %>%
        summarise(x = mean(.data[[pcx]], na.rm = TRUE),
                  y = mean(.data[[pcy]], na.rm = TRUE), .groups = "drop")
      cen$col <- pal[as.character(cen$.grp)]
      p <- p %>%
        add_markers(data = cen, x = ~x, y = ~y, inherit = FALSE,
                    marker = list(symbol = "diamond", size = 15,
                                  color = cen$col,
                                  line = list(color = "black", width = 1.5)),
                    text = cen$.grp, hoverinfo = "text", showlegend = FALSE) %>%
        add_text(data = cen, x = ~x, y = ~y, text = cen$.grp, inherit = FALSE,
                 textposition = "top center",
                 textfont = list(color = "black", size = 11), showlegend = FALSE)
    }

    # Emphasis overlay
    if (!is.null(emph) && nrow(emph) > 0) {
      p <- p %>%
        add_markers(data = emph, x = emph[[pcx]], y = emph[[pcy]], inherit = FALSE,
                    marker = list(symbol = "square", size = 11, color = "#22001A",
                                  line = list(color = "white", width = 1)),
                    hovertext = hover_text(emph), hoverinfo = "text",
                    name = input$pope, showlegend = FALSE)
    }

    ttl <- if (!is.null(input$plottitle) && nzchar(input$plottitle))
      input$plottitle else deftitle()

    p %>%
      layout(
        title = list(text = ttl, x = 0.5),
        xaxis = list(title = pc_label(input$PCx), zeroline = TRUE,
                     zerolinecolor = "#cccccc",
                     autorange = if (isTRUE(input$flipx)) "reversed" else TRUE),
        yaxis = list(title = pc_label(input$PCy), zeroline = TRUE,
                     zerolinecolor = "#cccccc",
                     autorange = if (isTRUE(input$flipy)) "reversed" else TRUE),
        showlegend = isTRUE(input$legon) && !is.null(fld),
        legend = list(title = list(text = if (!is.null(fld)) fld else "")),
        annotations = list(text = input$plotcaption, showarrow = FALSE,
                           xref = "paper", yref = "paper", x = 0, y = -0.08,
                           xanchor = "left", font = list(size = 11, color = "#555"))
      ) %>%
      config(displaylogo = FALSE,
             toImageButtonOptions = list(format = "png", filename = deftitle()))
  })

  # ----- Selection info (box / lasso) ----------------------------------------
  output$selinfo <- renderPrint({
    sel <- event_data("plotly_selected", source = "pca2d")
    if (is.null(sel) || length(sel$customdata) == 0)
      return(cat("No points selected.\nUse the box/lasso select tool."))
    df   <- pca_data()
    cols <- intersect(META_PREF, names(df))
    out  <- df[match(unlist(sel$customdata), df$ID), cols, drop = FALSE]
    print(as.data.frame(out), row.names = FALSE)
  })

  # =========================================================================
  # 3D PLOTLY
  # =========================================================================
  output$pca3d <- renderPlotly({
    df <- plot_base(); req(nrow(df) > 0)
    pcx <- paste0("PC", input$PCx); pcy <- paste0("PC", input$PCy)
    pcz <- paste0("PC", input$PCz)
    validate(need(all(c(pcx, pcy, pcz) %in% names(df)),
                  "Selected components not in data."))

    fld <- active_field()
    if (!is.null(fld)) { ac <- assign_colors(df, fld); df <- ac$df; pal <- ac$pal }
    else { df$.grp <- factor("samples"); pal <- c(samples = "#5b3f8e") }

    disp_mode <- if (input$type == 1) "markers" else "text"
    disp_text <- if (input$type == 2) df$ID
                 else if (input$type == 3 && has_pop()) as.character(df$POP)
                 else NULL

    ttl <- if (!is.null(input$plottitle) && nzchar(input$plottitle))
      input$plottitle else paste(deftitle(), "(3D)")

    mk <- if (disp_mode == "markers") list(size = 4, opacity = 0.75) else NULL
    tf <- if (disp_mode == "text") list(size = 9) else NULL

    plot_ly(
      data = df, x = df[[pcx]], y = df[[pcy]], z = df[[pcz]],
      type = "scatter3d", mode = disp_mode,
      color = df$.grp, colors = pal,
      text = disp_text, hovertext = hover_text(df), hoverinfo = "text",
      marker = mk,
      textfont = tf
    ) %>%
      layout(
        title = list(text = ttl, x = 0.5),
        showlegend = isTRUE(input$legon) && !is.null(fld),
        legend = list(title = list(text = if (!is.null(fld)) fld else "")),
        scene = list(
          xaxis = list(title = pc_label(input$PCx)),
          yaxis = list(title = pc_label(input$PCy)),
          zaxis = list(title = pc_label(input$PCz))
        )
      ) %>%
      config(displaylogo = FALSE)
  })

  # =========================================================================
  # DATA TABLE
  # =========================================================================
  table_data <- reactive({
    df <- pca_data(); req(df)
    pccol <- grep("^PC[0-9]+$", names(df), value = TRUE)
    df[pccol] <- lapply(df[pccol], function(x) round(x, 4))
    df
  })

  output$pca_table <- DT::renderDataTable({
    DT::datatable(table_data(), rownames = FALSE, filter = "top",
                  options = list(pageLength = 25, scrollX = TRUE,
                                 searchHighlight = TRUE))
  })

  output$dlTable <- downloadHandler(
    filename = function() paste0(deftitle(), "_pca_table.tsv"),
    content  = function(file) readr::write_tsv(table_data(), file)
  )

  # =========================================================================
  # STATIC EXPORT (ggplot2 fallback — no kaleido/orca needed)
  # =========================================================================
  build_ggplot_2d <- function() {
    df <- plot_base(); req(nrow(df) > 0)
    pcx <- paste0("PC", input$PCx); pcy <- paste0("PC", input$PCy)
    fld <- active_field()

    emph <- NULL
    if (has_pop() && !is.null(input$pope) && nzchar(input$pope)) {
      em=as.character(df$POP) == input$pope
      emph <- df[em, , drop = FALSE]; df <- df[!em, , drop = FALSE]
    }
    if (!is.null(fld)) { ac <- assign_colors(df, fld); df <- ac$df; pal <- ac$pal }
    else { df$.grp <- factor("samples"); pal <- c(samples = "#5b3f8e") }

    ttl <- if (!is.null(input$plottitle) && nzchar(input$plottitle))
      input$plottitle else deftitle()

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[pcx]], y = .data[[pcy]],
                                          color = .grp)) +
      ggplot2::theme_light() +
      ggplot2::geom_hline(yintercept = 0, color = "#CCCCCC") +
      ggplot2::geom_vline(xintercept = 0, color = "#CCCCCC") +
      ggplot2::scale_color_manual(values = pal, name = if (!is.null(fld)) fld else NULL)

    if (input$type == 1)
      p <- p + ggplot2::geom_point(size = 3, alpha = 0.6)
    else if (input$type == 2)
      p <- p + ggplot2::geom_text(ggplot2::aes(label = ID), size = 3, fontface = "bold")
    else if (input$type == 3 && has_pop())
      p <- p + ggplot2::geom_text(ggplot2::aes(label = POP), size = 3, fontface = "bold")

    if (isTRUE(input$cntrd) && !is.null(fld)) {
      cen <- df %>% group_by(.grp) %>%
        summarise(x = mean(.data[[pcx]], na.rm = TRUE),
                  y = mean(.data[[pcy]], na.rm = TRUE), .groups = "drop")
      cen$col <- pal[as.character(cen$.grp)]
      p <- p + ggplot2::geom_label(
        data = cen, ggplot2::aes(x = x, y = y, label = .grp),
        fill = cen$col, color = "white", fontface = "bold",
        size = 4, inherit.aes = FALSE)
    }
    if (!is.null(emph) && nrow(emph) > 0)
      p <- p + ggplot2::geom_point(
        data = emph, ggplot2::aes(x = .data[[pcx]], y = .data[[pcy]]),
        color = "#22001A", shape = 15, size = 3, inherit.aes = FALSE)

    p <- p +
      ggplot2::ggtitle(ttl) +
      ggplot2::labs(x = pc_label(input$PCx), y = pc_label(input$PCy),
                    caption = input$plotcaption) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                     legend.position = if (isTRUE(input$legon) && !is.null(fld)) "right" else "none")
    if (isTRUE(input$flipx)) p <- p + ggplot2::scale_x_reverse()
    if (isTRUE(input$flipy)) p <- p + ggplot2::scale_y_reverse()
    p
  }

  output$dlPlotpng <- downloadHandler(
    filename = function() paste0(deftitle(), ".pc", input$PCx, "x", input$PCy, ".png"),
    content  = function(file) ggplot2::ggsave(file, plot = build_ggplot_2d(),
                                              device = "png", width = 297, height = 210,
                                              units = "mm", dpi = 300, scale = 1.1)
  )
  output$dlPlotpdf <- downloadHandler(
    filename = function() paste0(deftitle(), ".pc", input$PCx, "x", input$PCy, ".pdf"),
    content  = function(file) ggplot2::ggsave(file, plot = build_ggplot_2d(),
                                              device = "pdf", width = 297, height = 210,
                                              units = "mm", dpi = 300, scale = 1.1)
  )
}
# <\SERVER> -------------------------------------------------------------------


# <APP> -----------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# <END> -----------------------------------------------------------------------
