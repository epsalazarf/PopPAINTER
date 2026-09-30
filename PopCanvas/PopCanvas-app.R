# AUTO PCA PLOTTER (Shiny) v1.5
# Shiny: app.R
# Author: Pavel Salazar-Fernandez (epsalazarf@gmail.com)
# Version Upgrade (R 4.0+): September 12 2022
# Latest Update: September 29 2026

# Requirements:
# - EVAL and EVEC files from the PLINK PCA.
# - popinfo file

# Pipeline:
# 1. Reads .eigenvec and .eigenval files from a chosen directory.
# 2. Identifies names, regions and populations from a given popinfo.
# 3. Generates an interactive color-coded PCA plot.

# Features:
# - Plot types: Can select between points or tags for the plot.
# - Select Population: Displays only selected population(s).
# - Emphasize Population: Highlights points or tags for a chosen population.
# - Color by Category: User can choose the criteria for coloring using the
#   popinfo.
# - Auto-Legend: Shows color coding for the selected category.
# - Interactive Zoom: select an area and double click to zoom in, double click again to zoom out.

#<START> ####
message("> Starting: PCA Visualizer dashboard...")

# Load required libraries
suppressPackageStartupMessages({
  require(shiny)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(markdown)
})

#<INPUT> ####
app_dir <- getwd()
#</INPUT>

#<PREPARATIONS> ####

# FUNCTIONS
refact <- function(x){
  # Checks if input is tagged as factor and if not retags it.
  if (!is.factor(x))
    x <- factor(x)
  ll <- as.character(na.omit(unique(x)))
  if (anyNA(x))
    ll <- c(ll, NA)
  factor(x, levels = ll, exclude = NULL)
}

#</PREPARATIONS>

#<UI> ####
ui <- fluidPage(
  # Page Title
  #img(src = "logo480x.jpg", height = "100px", style = "float:right"),
  titlePanel("PopCanvas ❧ PCA"),
  helpText("PCA Plotter - v1.5 [Sep 2026]"),
  hr(),
  # Sidebar
  sidebarLayout(
    # Input Panels
    sidebarPanel(width = 3,
                 h4("Upload Data"),
                 fileInput("eigfiles", "Upload eigenvec file (+ optional eigenval):",
                           multiple = TRUE,
                           accept = c(".eigenvec", ".eigenval", ".evec", ".eval", "text/plain")),
                 fileInput("pifile", "Upload POPINFO file (optional):",
                           multiple = FALSE, accept = c(".csv", ".tsv", ".txt")),
                 conditionalPanel(
                   condition = "!output.PCAPlot",
                   actionButton("loadDemo", "Load Demo Data")
                 ),
                 hr(),
                 h4("Settings"),
                 textInput("plottitle", label = "Title", value = ""),
                 checkboxInput("flx", label = "Flip x-axis", value = FALSE),
                 checkboxInput("fly", label = "Flip y-axis", value = FALSE),
                 radioButtons("type", label = "Type:",
                              choices = list("Points" = 1, "Text" = 2, "Labels" = 3),
                              selected = 1),
                 hr(),
                 # Dynamically generated once eigenvec/popinfo are uploaded
                 uiOutput("settingsUI"),
                 checkboxInput("cntrd", label = "Group Centroids", value = TRUE),
                 checkboxInput("legon", label = "Legend", value = FALSE),
                 hr(),
                 textInput("plotcaption", label = "Caption", value = ""),
                 downloadButton("dlPlotpng", "Save as PNG"),
                 downloadButton("dlPlotpdf", "Save as PDF"),
                 hr(),
                 h5("Points Info"),
                 verbatimTextOutput("brshinfo")),
    
    # Plotting Area
    mainPanel(width = 9,
              tabsetPanel(type = "tabs",
                          tabPanel("Plot", plotOutput("PCAPlot", 
                            width = "1080px", height = "940px", dblclick = "dclk",
                            brush = brushOpts(id = "brsh", resetOnNew = TRUE))),
                          tabPanel("Data Table", 
                            DT::DTOutput("PCA_table")),
                          tabPanel("Instructions", 
                            includeMarkdown(paste0(app_dir,"/README.md")))
                          )
              )
  ),
  helpText("PopPAINTER ❦ Population genomics visualization suite")
) 
#</UI>

#<SERVER> ####
server <- function(input, output, session) {
  #<REACTIVES>

  # Single source of truth for file paths — fed either by real uploads or
  # by the demo button, so the rest of the reactive chain doesn't care where
  # the data came from.
  dataSrc <- reactiveValues(vecpath = NULL, valpath = NULL, deftitle = NULL, pipath = NULL)

  observeEvent(input$eigfiles, {
    df <- input$eigfiles
    vec_idx <- grep("\\.e(.*)vec$", df$name, ignore.case = TRUE)
    val_idx <- grep("\\.e(.*)val$", df$name, ignore.case = TRUE)
    if (length(vec_idx) == 0) {
      showNotification("Upload must include a .eigenvec file.", type = "error")
      return()
    }
    dataSrc$vecpath  <- df$datapath[vec_idx[1]]
    # The eigenvalue file is optional — it only adds the %-variance-explained
    # suffix to the axis labels, the plot itself only needs PC coordinates.
    dataSrc$valpath  <- if (length(val_idx) > 0) df$datapath[val_idx[1]] else NULL
    dataSrc$deftitle <- tools::file_path_sans_ext(basename(df$name[vec_idx[1]]))
  })

  observeEvent(input$pifile, {
    dataSrc$pipath <- input$pifile$datapath
  })

  observeEvent(input$loadDemo, {
    dataSrc$vecpath  <- "demo/demo.1kgp.eigenvec"
    dataSrc$valpath  <- "demo/demo.1kgp.eigenval"
    dataSrc$pipath   <- "demo/demo.1kgp.popinfo.tsv"
    dataSrc$deftitle <- "demo.1kgp"
  })

  # Read the eigenvec/eigenval pair once identified.
  pca_raw <- reactive({
    req(dataSrc$vecpath)

    # smartpca's .evec has no real header row — just a leading "#eigvals:"
    # line — and pads columns with a variable number of spaces for visual
    # alignment rather than a single consistent delimiter, which read_delim()
    # cannot parse (each extra space becomes a spurious empty column). Detect
    # it from that signature line and read it with read_table() instead,
    # which treats any run of whitespace as one delimiter.
    first_line <- readLines(dataSrc$vecpath, n = 1, warn = FALSE)
    is_smartpca <- grepl("^\\s*#\\s*eigvals", first_line, ignore.case = TRUE)

    if (is_smartpca) {
      # The eigvals line also reliably tells us how many PCs are present,
      # since the data rows carry no column names at all.
      eigvals_hdr <- as.numeric(strsplit(
        trimws(sub("^\\s*#\\s*eigvals:?\\s*", "", first_line, ignore.case = TRUE)),
        "\\s+")[[1]])
      ncomps <- length(eigvals_hdr)
      raw <- read_table(dataSrc$vecpath, skip = 1, col_names = FALSE,
                         show_col_types = FALSE, guess_max = 1e5)
      # Keep ID + the PC columns; smartpca appends a trailing case/control
      # or population label after them that we don't need (grouping comes
      # from the user's own popinfo file instead).
      data <- as.data.frame(raw[, 1:(ncomps + 1)])
      colnames(data) <- c("ID", paste0("PC", seq_len(ncomps)))
      PCcols <- 2:(ncomps + 1)
      IDcol <- 1
    } else {
      # PLINK 1.9's `--pca` output has no header row at all — the file
      # starts straight in with data (FID IID PC1 PC2 ...). Reading that
      # with col_names=TRUE (the header-based path below) would silently
      # swallow the first sample as fake column names and misalign
      # everything after it. Detect this by checking whether the row
      # actually looks like numeric PC data rather than column labels.
      first_tokens <- strsplit(trimws(first_line), "\\s+")[[1]]
      pc_like_tokens <- if (length(first_tokens) > 2) first_tokens[-(1:2)] else character(0)
      has_header <- length(pc_like_tokens) == 0 ||
        any(is.na(suppressWarnings(as.numeric(pc_like_tokens))))

      if (!has_header) {
        # No header at all: PLINK 1.9's column order is fixed and always
        # FID, IID, PC1..PCn — there's nothing left to introspect.
        data <- read_table(dataSrc$vecpath, col_names = FALSE,
                            show_col_types = FALSE, guess_max = 1e5)
        ncomps <- ncol(data) - 2
        colnames(data) <- c("FID", "ID", paste0("PC", seq_len(ncomps)))
        data <- data[, c("ID", "FID", paste0("PC", seq_len(ncomps)))]
        PCcols <- 3:(2 + ncomps)
        IDcol <- 1
      } else {
        data <- read_delim(dataSrc$vecpath, show_col_types = FALSE)
        raw_names <- colnames(data)

        # Identify sample ID ("IID") and family/group ID ("FID") columns by
        # name before doing anything position-based — "FID" and "IID" never
        # overlap as substrings of each other, so this is safe. FID is kept
        # (not dropped) and moved to the second column, right after ID, since
        # it can carry a meaningful grouping label (e.g. taxon/strain) rather
        # than PLINK's usual placeholder value.
        classes <- sapply(data, class)
        id_idx  <- grep("IID", raw_names, ignore.case = TRUE)
        fid_idx <- grep("FID", raw_names, ignore.case = TRUE)
        if (length(id_idx) == 0) {
          # Fallback: assume ID sits immediately before the first PC-looking
          # column that isn't FID (FID can itself be all-numeric, e.g. "0").
          id_idx <- min(setdiff(grep("numeric", classes), fid_idx)) - 1
        }
        # A numeric-looking FID (PLINK's usual "0" placeholder, or a numeric
        # group code) would otherwise get counted as a PC column here too —
        # exclude ID/FID explicitly rather than relying on class alone.
        pc_idx <- setdiff(grep("numeric", classes), c(id_idx, fid_idx))
        ncomps <- length(pc_idx)
        other_idx <- setdiff(seq_len(ncol(data)), c(id_idx, fid_idx, pc_idx))

        data <- data[, c(id_idx, fid_idx, pc_idx, other_idx)]
        pc_start <- if (length(fid_idx) > 0) 3 else 2
        new_names <- colnames(data)
        new_names[1] <- "ID"
        if (length(fid_idx) > 0) new_names[2] <- "FID"
        new_names[pc_start:(pc_start + ncomps - 1)] <- paste0("PC", seq_len(ncomps))
        colnames(data) <- new_names

        PCcols <- pc_start:(pc_start + ncomps - 1)
        IDcol <- 1
      }
    }

    # smartpca's .eval lists eigenvalues for every possible eigenvector
    # (as many as there are samples), not just the retained PCs — scan()
    # handles that length difference fine, and summing all of them for the
    # %-variance-explained denominator is actually more correct than PLINK's
    # own (retained-PCs-only) approximation. The eigenval file is optional;
    # without it we simply skip the %-variance suffix on the axis labels.
    eval <- if (!is.null(dataSrc$valpath)) scan(dataSrc$valpath) else NULL

    list(data = data, eval = eval, PCcols = PCcols, ncomps = ncomps,
         IDcol = IDcol, deftitle = dataSrc$deftitle)
  })

  # Popinfo (optional — the plot can render from the eigenvec alone)
  popinfo_rx <- reactive({
    if (is.null(dataSrc$pipath)) return(NULL)
    pi <- read_delim(dataSrc$pipath, show_col_types = FALSE, trim_ws = TRUE)
    id_candidates <- c("ID", "IID", "SID", "Sample", "SampleID")
    id_present <- id_candidates[id_candidates %in% colnames(pi)]
    if (length(id_present) > 0) {
      pi <- pi %>% rename(ID = any_of(id_present[1]))
    }
    pi
  })

  # Whether a POPINFO file has been loaded — gates grouping/coloring/
  # filtering controls, which are meaningless without it.
  has_popinfo <- reactive(!is.null(popinfo_rx()))

  # Merge PCA data with POPINFO when available; otherwise pass the raw
  # eigenvec data through as-is for a simple, ungrouped plot.
  pca.merged <- reactive({
    req(pca_raw())
    raw <- pca_raw()

    # FID only qualifies as a coloring field when it carries real grouping
    # information: not PLINK's usual "no family ID" placeholder ("0"/"-9"),
    # and not so many categories that coloring by it is meaningless.
    fid_qualifies <- FALSE
    if ("FID" %in% colnames(raw$data)) {
      fid_vals <- unique(as.character(raw$data$FID))
      fid_qualifies <- length(fid_vals) >= 2 && length(fid_vals) <= 20 &&
        !all(fid_vals %in% c("0", "-9"))
    }

    if (!has_popinfo()) {
      fields <- if (fid_qualifies) "FID" else character(0)
      return(list(data = raw$data, eval = raw$eval, PCcols = raw$PCcols,
                  ncomps = raw$ncomps, IDcol = raw$IDcol, deftitle = raw$deftitle,
                  pops = character(0), fields = fields, allIDs = raw$data$ID))
    }

    data <- merge(raw$data, popinfo_rx(), by.x = "ID", sort = FALSE)
    data[,-raw$PCcols] <- as.data.frame(lapply(data[,-raw$PCcols],
                                               function(x) if (length(unique(x)) != 1 &&
                                                               length(unique(x)) != nrow(data))
                                               {refact(x)} else{x}))
    pops <- as.character(unique(data$POP))
    uniquecols <- sapply(data, function(x) length(unique(x)))
    # Exclude ID/PC columns by their actual indices rather than assuming a
    # contiguous ID..lastPC block — FID (when present) sits between them.
    fields <- setdiff(names(uniquecols[uniquecols > 1]),
                       colnames(data)[c(raw$IDcol, raw$PCcols)])
    if ("FID" %in% fields && !fid_qualifies) {
      fields <- setdiff(fields, "FID")
    }
    if ("POPULATION" %in% colnames(data)) {
      names(pops) <- unique(paste0(data$POP, " (", data$POPULATION, ")"))
    }

    list(data = data, eval = raw$eval, PCcols = raw$PCcols, ncomps = raw$ncomps,
         IDcol = raw$IDcol, deftitle = raw$deftitle,
         pops = pops, fields = fields, allIDs = data$ID)
  })

  # Accessors mirroring the former globals, now reactive.
  pca.data <- reactive(pca.merged()$data)
  eval_rx <- reactive(pca.merged()$eval)
  ncomps <- reactive(pca.merged()$ncomps)
  IDcol <- reactive(pca.merged()$IDcol)
  deftitle <- reactive(pca.merged()$deftitle)
  pops <- reactive(pca.merged()$pops)
  fields <- reactive(pca.merged()$fields)
  allIDs <- reactive(pca.merged()$allIDs)

  # Inputs
  sub.pops <- reactive(pca.data()[pca.data()$POP %in% input$pops,IDcol()])
  pope.idn <- reactive(pca.data()[pca.data()$POP %in% input$pope,IDcol()])
  groups <- reactive(as.character(levels(refact(
    pca.data()[(pca.data()$POP != input$pope),input$flds]))))
  PCaCol <- reactive(paste0("PC",input$PCa))
  PCbCol <- reactive(paste0("PC",input$PCb))
  # Without an eigenval file there's no variance data to report — just show
  # the plain PC label instead of the "(X%)" suffix.
  pct.PCa <- reactive({
    if (is.null(eval_rx())) PCaCol()
    else paste0(PCaCol(), " (",percent(eval_rx()[input$PCa]/sum(eval_rx())),")")
  })
  pct.PCb <- reactive({
    if (is.null(eval_rx())) PCbCol()
    else paste0(PCbCol(), " (",percent(eval_rx()[input$PCb]/sum(eval_rx())),")")
  })
  ranges <- reactiveValues(x = NULL, y = NULL)

  #</REACTIVES>

  #<DYNAMIC UI>
  # Settings that depend on the uploaded data (PC choices, population/grouping choices).
  output$settingsUI <- renderUI({
    req(pca.data())
    pc_inputs <- tagList(
      numericInput("PCa", label = "First Component (X)", value = 1,
                   min = 1, max = ncomps(), step = 1),
      numericInput("PCb", label = "Second Component (Y)", value = 2,
                   min = 1, max = ncomps(), step = 1)
    )

    # Population filter/emphasis only make sense with a real POPINFO file
    # (they key off its POP column).
    pop_inputs <- if (has_popinfo()) {
      tagList(
        selectizeInput("pops", label = "Populations displayed:",
                       choices = pops(), selected = NULL,
                       options = list(maxItems = length(pops()) - 1,
                                      placeholder = 'Select population(s)',
                                      onInitialize = I('function() { this.setValue(""); }'))),
        selectizeInput("pope", label = "Populations emphasis:",
                       choices = sort(pops()), selected = NULL, multiple = FALSE,
                       options = list(placeholder = 'None',
                                      onInitialize = I('function() { this.setValue(""); }')))
      )
    }

    # Group Coloring, though, is available whenever there's *any* qualifying
    # field to color by — a real POPINFO, or (without one) a usable FID
    # column carried over from the eigenvec file itself.
    color_input <- if (length(fields()) > 0) {
      selectizeInput("flds", label = "Group Coloring:",
                     choices = fields(),
                     selected = if ("POP" %in% fields()) "POP" else fields()[1],
                     options = list(maxItems = 1,
                                    placeholder = 'Choose color grouping'))
    }

    if (is.null(pop_inputs) && is.null(color_input)) return(pc_inputs)
    tagList(pc_inputs, hr(), pop_inputs, color_input)
  })
  #</DYNAMIC UI>

  #<OBSERVERS>
  observeEvent(input$dclk, {
    brush <- input$brsh
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  #</OBSERVERS>

  #<OUTPUT> ####
  pca.plot <- reactive({
    req(pca.data(), input$PCa, input$PCb)

    # No POPINFO loaded: there's no POP field to filter/emphasize by, but a
    # qualifying FID column can still be used to color the plot. Draw a
    # plain (or FID-colored) PCA scatter straight from the eigenvec data,
    # instead of running any of the popinfo-dependent logic below.
    if (!has_popinfo()) {
      pca.keep <- pca.data()
      can_color <- length(fields()) > 0 && !is.null(input$flds) && input$flds %in% colnames(pca.keep)

      if (can_color) {
        grp.colors <- setNames(rainbow(length(unique(pca.keep[[input$flds]])), s = 0.5, v = 0.9),
                                unique(as.character(pca.keep[[input$flds]])))
        pca.keep$plotColors <- grp.colors[as.character(pca.keep[[input$flds]])]

        if (input$cntrd) {
          centroids <- list()
          for (g in names(grp.colors)) {
            centroids[[g]] <- apply(pca.keep[as.character(pca.keep[[input$flds]]) %in% g,
                                             c(PCaCol(), PCbCol())], 2, mean)
          }
          pca.cntrd <- as.data.frame(do.call("rbind", centroids))
          pca.cntrd$GRP <- rownames(pca.cntrd)
          pca.cntrd$GCOL <- grp.colors[pca.cntrd$GRP]
        }
      }

      p <- ggplot(data = pca.keep,
                   aes_string(x = PCaCol(), y = PCbCol(),
                              color = if (can_color) "plotColors" else NULL)) +
        theme_light() +
        # A bold 0/0 crosshair is a sanity check, not decoration: real PCA
        # output is centered near the origin, so a plot that renders
        # visibly off-center or squished against it is a sign something
        # is wrong upstream (e.g. running PCA where PCoA was needed).
        geom_hline(yintercept = 0, color = "gray75", linewidth = 0.5) +
        geom_vline(xintercept = 0, color = "gray75", linewidth = 0.5)

      if (can_color) {
        p <- p +
          {if (input$type == 1) geom_point(size = 3, alpha = 0.6) } +
          {if (input$type != 1) geom_text(aes(label = ID), size = 3, alpha = 0.9, fontface = "bold") } +
          {if (input$cntrd) {
            geom_label(data = pca.cntrd,
                       aes_string(x = PCaCol(), y = PCbCol(), label = "GRP"),
                       size = 4, alpha = 0.9, color = "white",
                       fill = pca.cntrd$GCOL, fontface = "bold") }} +
          scale_color_identity(name = input$flds, labels = names(grp.colors),
                               breaks = unname(grp.colors), guide = "legend") +
          {if (!input$legon) theme(legend.position = "none") }
      } else {
        p <- p +
          {if (input$type == 1) geom_point(size = 3, alpha = 0.6, color = "#2C3E82") } +
          {if (input$type != 1) geom_text(aes(label = ID), size = 3, alpha = 0.9,
                                           fontface = "bold", color = "#2C3E82") } +
          theme(legend.position = "none")
      }

      return(
        p +
          ggtitle(ifelse(input$plottitle == "", deftitle(), input$plottitle)) +
          theme(plot.title = element_text(lineheight = 0.8, face = "bold", hjust = 0.5),
                plot.caption = element_text(hjust = 0),
                panel.grid.minor = element_blank()) +
          scale_x_continuous(breaks = breaks_width(width = 0.01)) +
          scale_y_continuous(breaks = breaks_width(width = 0.01)) +
          labs(x = pct.PCa(), y = pct.PCb(), caption = input$plotcaption) +
          {if (input$flx) scale_x_reverse()} +
          {if (input$fly) scale_y_reverse()} +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = T)
      )
    }

    # Data subset
    pca.keep <- pca.data()
    if (!is.null(input$pops)) {
      pca.keep <- pca.data()[ allIDs() %in% sub.pops(), , drop = F]
    }
    if (input$pope %in% pops()) {
      #print(pope.idn())
      head(allIDs())
      pca.emph <- pca.data()[ allIDs() %in% pope.idn(), , drop = F]
      pca.emph$plotColors <- "#22001A"
      pca.keep <- subset(pca.keep, !(ID %in% pca.emph$ID))
    }

    #Coloring
    if (input$flds == "POP") {
      if ("COLORX" %in% colnames(pca.keep)) {
        pca.keep$plotColors <- paste0(pca.keep$COLOR,"FF")
      } else {
        grp.colors <- setNames(rainbow(length(pops()), s = 0.5, v = 0.9), pops())
        pca.keep$plotColors <- grp.colors[pca.keep$POP]
      }
    } else {
      grp.colors <- setNames(rainbow(length(na.omit(groups())),
                                     s = 0.5, v = 0.9), na.omit(groups()))
      if (any(is.na(groups())))
        grp.colors <- c(grp.colors,setNames("#999999", NA))
      pca.keep$plotColors <- grp.colors[as.character(pca.keep[,input$flds])]
    }
    
    pca.keep$plotColors <- factor(pca.keep$plotColors,
                                  unique(pca.keep$plotColors))
    if (any(is.na(groups()))) {
      levels(pca.keep$plotColors) <- c(levels(pca.keep$plotColors),"#999999")
      pca.keep$plotColors[is.na(pca.keep$plotColors)] <- "#999999"
    }
    
    # Centroid creation
    if (input$cntrd) {
      centroids <- list()
      for (g in groups()) {
        centroids[[g]] <- apply(pca.keep[pca.keep[,input$flds] %in% g,
                                         c(PCaCol(),PCbCol())], 2, mean)
      }
      pca.cntrd <- as.data.frame(do.call("rbind",centroids))
      pca.cntrd$GRP <- rownames(pca.cntrd)
      pca.cntrd$GCOL <- grp.colors[pca.cntrd$GRP]
    }
    
    #Plotting
    ggplot(data = pca.keep,
           aes_string(x = PCaCol(), y = PCbCol(),
                      color = "plotColors")) +
      theme_light() +
      # A bold 0/0 crosshair is a sanity check, not decoration: real PCA
      # output is centered near the origin, so a plot that renders visibly
      # off-center or squished against it is a sign something is wrong
      # upstream (e.g. running PCA where PCoA was needed).
      geom_hline(yintercept = 0, color = "gray75", linewidth = 0.5) +
      geom_vline(xintercept = 0, color = "gray75", linewidth = 0.5) +
      # POINTS
      {if (input$type == 1) geom_point(size = 3, alpha = 0.6) } +
      # ID
      {if (input$type == 2) geom_text(aes(label = ID), size = 3, alpha = 0.9, fontface = "bold") } +
      # LABEL
      {if (input$type == 3) geom_text(aes(label = POP), size = 3, alpha = 0.9, fontface = "bold") } +
      #Centroids
      {if (input$cntrd & input$type == 4) {
        geom_point(data = pca.cntrd,
                   aes_string(x = PCaCol(), y = PCbCol(), color = "plotColors"),
                   size = 4, alpha = 0.9, shape = 23, stroke = 2,
                   fill =  pca.cntrd$GCOL, color = "white") }} +
      {if (input$cntrd & input$type < 4) {
        geom_label(data = pca.cntrd,
                   aes_string(x = PCaCol(), y = PCbCol(), label = "GRP"),
                   size = 4, alpha = 0.9, color = "white",
                   fill = pca.cntrd$GCOL, fontface = "bold") }} +
      #Emphasis
      {if (input$pope %in% pops() & input$type == 3) {
        geom_point(data = pca.emph,
                   aes_string(x = PCaCol(), y = PCbCol(), color = "plotColors"),
                   size = 3, alpha = 0.8, shape = 22,
                   fill = "#22001A", color = "white") }} +
      {if (input$pope %in% pops() & input$type == 2) {
        geom_label(data = pca.emph,
                   aes_string(x = PCaCol(), y = PCbCol(), label = "ID"),
                   size = 3, alpha = 0.8, color = "white",
                   fill = pca.emph$plotColors, fontface = "bold") }} +
      {if (input$pope %in% pops() & input$type == 1) {
        geom_label(data = pca.emph,
                   aes_string(x = PCaCol(), y = PCbCol(), label = "POP"),
                   size = 3, alpha = 0.8, color = "white",
                   fill = pca.emph$plotColors, fontface = "bold") }} +

      # TODO: Emphasis removes centroid, to be fixed.

      # Aesthetics
      scale_color_identity(name = input$flds,
                           labels = groups(), guide = "legend") +
      {if (!input$legon) theme(legend.position = "none") } +
      ggtitle(ifelse(input$plottitle == "", deftitle(), input$plottitle)) +
      theme(plot.title = element_text(lineheight = 0.8, face = "bold", hjust = 0.5),
            plot.caption = element_text(hjust = 0),
            panel.grid.minor = element_blank()) +
      scale_x_continuous(breaks = breaks_width(width = 0.01)) +
      scale_y_continuous(breaks = breaks_width(width = 0.01)) +
      labs(x = pct.PCa(), y = pct.PCb(), caption = input$plotcaption) +
      {if (input$flx) scale_x_reverse()} +
      {if (input$fly) scale_y_reverse()} +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = T)
  })
  
  output$PCAPlot <- renderPlot({ pca.plot() })
  
  
  output$PCA_table <- DT::renderDT(pca.data(),
    filter = "top",
    extensions = 'Buttons', 
    options = list(pageLength = 25,
                   lengthMenu = c(25,50,100),
                   paging = TRUE,
                   scrollX=TRUE,
                   dom = 'l<"sep">Bfrtip',
                   buttons = c('copy', 'csv', 'excel', 'pdf')),
    server = FALSE
  )
  

  output$dlPlotpdf <- downloadHandler(
    filename = function() { paste0(deftitle(),".pc", input$PCa,"x", input$PCb,".pdf")},
    content = function(file) {
      ggsave(file, plot = pca.plot(), device = "pdf",
             width = 297, height = 210, units = "mm", dpi = 300, scale = 1.2)
    }
  )

  output$dlPlotpng <- downloadHandler(
    filename = function() { paste0(deftitle(),".pc", input$PCa,"x", input$PCb,".png")},
    content = function(file) {
      ggsave(file, plot = pca.plot(), device = "png",
             width = 297, height = 210, units = "mm", dpi = 300, scale = 1.2)
    }
  )

  output$brshinfo <- renderPrint({
    req(pca.data())
    # Get the brushed points based on the specified PC columns
    brushed_points <- brushedPoints(pca.data(), input$brsh, xvar = PCaCol(), yvar = PCbCol())

    # Select only available columns among those we want
    available_columns <- intersect(c("ID", "POPULATION", "POP", "META", "SUPER"), colnames(pca.data()))

    # Display brushed points if any are selected and columns are available
    if (nrow(brushed_points) > 0 && length(available_columns) > 0) {
      brushed_points[, available_columns, drop = FALSE]
    } else {
      "No points selected."
    }
  })
}

#</SERVER>

#<APP> ####
shinyApp(ui = ui, server = server)
#</APP>


#<END> ####
