# AUTO PCA PLOTTER (Shiny) v1.3
# Shiny: app.R
# Author: Pavel Salazar-Fernandez (epsalazarf@gmail.com)
# Version Upgrade (R 4.0+): September 12 2022
# Lastest Update: August 18 2026

# Requirements:
# - EVAL and EVEC files from the PLINK PCA.
# - popinfo file

# Pipeline:
# 1. Reads .eigenvec and .eigenval files from a chosen directory.
# 2. Identifies names, regions and populations from a given popinfo.tsv/txt
# 3. Generates a color-coded PCA plot.

# Features:
# - Plot types: Can select between points or tags for the plot.
# - Select Population: Displays only selected population(s).
# - Emphasize Population: Highlights points or tags for a chosen population.
# - Color by Category: User can choose the criteria for coloring using the
#   popinfo.
# - Auto-Legend: Shows color coding for the selected category.
# - Interactive Zoom: select an area and double click to zoom in, double click
#   again to zoom out.

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
  helpText("PCA Plotter - v1.3 [Aug 2026]"),
  hr(),
  # Sidebar
  sidebarLayout(
    # Input Panels
    sidebarPanel(width = 3,
                 h4("Upload Data"),
                 fileInput("eigfiles", "Upload eigenvec/eigenval files:",
                           multiple = TRUE,
                           accept = c(".eigenvec", ".eigenval", ".evec", ".eval", "text/plain")),
                 fileInput("pifile", "Upload POPINFO file:",
                           multiple = FALSE, accept = c(".csv", ".tsv", ".txt")),
                 hr(),
                 h4("Settings"),
                 textInput("plottitle", label = "Title", value = ""),
                 checkboxInput("flx", label = "Flip x-axis", value = FALSE),
                 checkboxInput("fly", label = "Flip y-axis", value = FALSE),
                 #checkboxInput("ash", label = "ASINH zoom [TBD]", value = TRUE),
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

  # Identify the .eigenvec and .eigenval files among the uploaded pair.
  eigfiles_list <- reactive({
    req(input$eigfiles)
    df <- input$eigfiles
    vec_idx <- grep("\\.e(.*)vec$", df$name, ignore.case = TRUE)
    val_idx <- grep("\\.e(.*)val$", df$name, ignore.case = TRUE)
    validate(need(length(vec_idx) > 0, "No .eigenvec file found among uploads."))
    validate(need(length(val_idx) > 0, "No .eigenval file found among uploads."))
    list(vecpath = df$datapath[vec_idx[1]],
         valpath = df$datapath[val_idx[1]],
         deftitle = tools::file_path_sans_ext(basename(df$name[vec_idx[1]])))
  })

  # Read the eigenvec/eigenval pair once identified.
  pca_raw <- reactive({
    req(eigfiles_list())
    ef <- eigfiles_list()
    data <- read_delim(ef$vecpath, show_col_types = FALSE)
    names(data) <- gsub(names(data), pattern = "IID", replacement = "ID")
    data <- data[,!grepl(".*FID.*", colnames(data))]
    eval <- scan(ef$valpath)

    PCcols <- grep("numeric", sapply(data, class))
    ncomps <- table(sapply(data, class))["numeric"]
    PC1Col <- match("numeric", sapply(data, class))
    IDcol <- PC1Col - 1
    colnames(data)[IDcol:(IDcol + ncomps)] <- c("ID", paste0("PC", 1:ncomps))
    pcteval <- unlist(lapply(eval, function(x) {round((x/sum(eval))*100, 2)}))
    names(pcteval) <- colnames(data[PCcols])

    list(data = data, eval = eval, PCcols = PCcols, ncomps = ncomps,
         IDcol = IDcol, pcteval = pcteval, deftitle = ef$deftitle)
  })

  # Popinfo
  popinfo_rx <- reactive({
    req(input$pifile)
    pi <- read_delim(input$pifile$datapath, show_col_types = FALSE, trim_ws = TRUE)
    id_candidates <- c("ID", "IID", "SID", "Sample", "SampleID")
    id_present <- id_candidates[id_candidates %in% colnames(pi)]
    if (length(id_present) > 0) {
      pi <- pi %>% rename(ID = any_of(id_present[1]))
    }
    pi
  })

  # Merge PCA data with POPINFO once both are uploaded.
  pca.merged <- reactive({
    req(pca_raw(), popinfo_rx())
    raw <- pca_raw()
    data <- merge(raw$data, popinfo_rx(), by.x = "ID", sort = FALSE)
    data[,-raw$PCcols] <- as.data.frame(lapply(data[,-raw$PCcols],
                                               function(x) if (length(unique(x)) != 1 &&
                                                               length(unique(x)) != nrow(data))
                                               {refact(x)} else{x}))
    pops <- as.character(unique(data$POP))
    uniquecols <- sapply(data, function(x) length(unique(x)))
    fields <- names(uniquecols[uniquecols > 1])[-(raw$IDcol:(raw$IDcol + raw$ncomps))]
    if ("POPULATION" %in% colnames(data)) {
      names(pops) <- unique(paste0(data$POP, " (", data$POPULATION, ")"))
    }

    list(data = data, eval = raw$eval, PCcols = raw$PCcols, ncomps = raw$ncomps,
         IDcol = raw$IDcol, pcteval = raw$pcteval, deftitle = raw$deftitle,
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
  pct.PCa <- reactive(paste0(PCaCol(),
                             " (",percent(eval_rx()[input$PCa]/sum(eval_rx())),")"))
  pct.PCb <- reactive(paste0(PCbCol(),
                             " (",percent(eval_rx()[input$PCb]/sum(eval_rx())),")"))
  ranges <- reactiveValues(x = NULL, y = NULL)

  #</REACTIVES>

  #<DYNAMIC UI>
  # Settings that depend on the uploaded data (PC choices, population/grouping choices).
  output$settingsUI <- renderUI({
    req(pca.data())
    tagList(
      numericInput("PCa", label = "First Component (X)", value = 1,
                   min = 1, max = ncomps(), step = 1),
      numericInput("PCb", label = "Second Component (Y)", value = 2,
                   min = 1, max = ncomps(), step = 1),
      hr(),
      selectizeInput("pops", label = "Populations displayed:",
                     choices = pops(), selected = NULL,
                     options = list(maxItems = length(pops()) - 1,
                                    placeholder = 'Select population(s)',
                                    onInitialize = I('function() { this.setValue(""); }'))),
      selectizeInput("pope", label = "Populations emphasis:",
                     choices = sort(pops()), selected = NULL, multiple = FALSE,
                     options = list(placeholder = 'None',
                                    onInitialize = I('function() { this.setValue(""); }'))),
      selectizeInput("flds", label = "Group Coloring:",
                     choices = fields(), selected = "POP",
                     options = list(maxItems = 1,
                                    placeholder = 'Choose color grouping'))
    )
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
      geom_hline(yintercept = 0, color = "#CCCCCC") +
      geom_vline(xintercept = 0, color = "#CCCCCC") +
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
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = T) #+
    #{if (input$ash) scale_x_continuous(transform = "asinh", guide = "axis_logticks")} +
    #{if (input$ash) scale_y_continuous(transform = "asinh", guide = "axis_logticks")}
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

#<SANDBOX> ####

#<END> ####
