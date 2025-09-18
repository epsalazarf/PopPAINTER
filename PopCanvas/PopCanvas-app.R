# AUTO PCA PLOTTER (Shiny) v2.0
# Shiny: app.R
# Author: Pavel Salazar-Fernandez (epsalazarf@gmail.com)
# Version Upgrade (R 4.0+): September 12 2022
# Lastest Update: August 18 2025

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
# - [TBD] ASH transformation for contextual zoom.

#<START> ####
message("> Starting: PCA Visualizer dashboard...")

# Load required libraries
suppressPackageStartupMessages({
  require(shiny)
  library(tidyverse)
  library(scales)
})

#<INPUT> ####
# Choose Eig* file
eigfile <- file.choose()
setwd(dirname(eigfile))

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

# asinh_trans <- function() {
#   scales::trans_new(
#     name = 'asinh',
#     transform = function(x) asinh(x),
#     inverse = function(x) sinh(x),
#     domain = c(-Inf, Inf)
#   )
# }

# Load PCA files
deftitle <- tools::file_path_sans_ext(basename(eigfile))
pca.data <- read_delim(dir(pattern = paste0(deftitle,".e(.*)vec$"))[1], show_col_types = F)
names(pca.data) <- gsub(names(pca.data), pattern = "IID", replacement = "ID")
pca.data <- pca.data[,!grepl(".*FID.*", colnames(pca.data))]
eval <- scan(dir(pattern = paste0(deftitle,".e(.*)val$"))[1])

PCcols <- grep("numeric",sapply(pca.data, class))
ncomps <- table(sapply(pca.data, class))["numeric"]
PC1Col <- match("numeric",sapply(pca.data, class))
IDcol <- PC1Col - 1
colnames(pca.data)[IDcol:(IDcol + ncomps)] <- c("ID",paste0("PC",1:ncomps))
pcteval <- unlist(lapply(eval, function(x) {round((x/sum(eval))*100, 2)}))
names(pcteval) <- colnames(pca.data[PCcols])

# Read popinfo file (.tsv):
pifile <- file.choose()

popinfo <- read_delim(pifile) %>% rename(ID = 1)

# Data merging
pca.data <- merge(pca.data, popinfo, by.x = "ID", sort = F)
pca.data[,-PCcols] <- as.data.frame(lapply(pca.data[,-PCcols],
                                           function(x) if (length(unique(x)) != 1 &&
                                                           length(unique(x)) != nrow(pca.data))
                                           {refact(x)} else{x}))
allIDs <- pca.data$ID

# Data field scan
pops <- as.character(unique(pca.data$POP))
slabels <- as.character(unique(pca.data$SLABEL))
uniquecols <- sapply(pca.data,function(x) length(unique(x)))
fields <- names(uniquecols[uniquecols > 1])[-(IDcol:(IDcol + ncomps))]
if ("POP_simplex" %in% colnames(pca.data)) {
  names(pops) <- unique(paste0(pca.data$POP," (",pca.data$POPULATION,")"))
}

#</PREPARATIONS>

#<UI> ####
ui <- fluidPage(
  # Page Title
  #img(src = "logo480x.jpg", height = "100px", style = "float:right"),
  titlePanel("PopCanvas ❧ PCA"),
  helpText("PCA Plotter - v1.0"),
  hr(),
  # Sidebar
  sidebarLayout(
    # Input Panels
    sidebarPanel(width = 3,
                 h4("Settings"),
                 textInput("plottitle", label = "Title", value = ""),
                 numericInput("PCa", label = "First Component (X)", value = 1,
                              min = 1, max = ncomps, step = 1),
                 numericInput("PCb", label = "Second Component (Y)", value = 2,
                              min = 1, max = ncomps, step = 1),
                 checkboxInput("flx", label = "Flip x-axis", value = FALSE),
                 checkboxInput("fly", label = "Flip y-axis", value = FALSE),
                 #checkboxInput("ash", label = "ASINH zoom [TBD]", value = TRUE),
                 radioButtons("type", label = "Type:",
                              choices = list("Points" = 1, "Text" = 2, "Labels" = 3),
                              selected = 1),
                 hr(),
                 selectizeInput("pops", label = "Populations displayed:",
                                choices = pops, selected = NULL,
                                options = list(maxItems = length(pops) - 1,
                                               placeholder = 'Select population(s)',
                                               onInitialize = I('function() { this.setValue(""); }'))),
                 selectizeInput("pope", label = "Populations emphasis:",
                                choices = sort(pops), selected = NULL, multiple = F,
                                options = list(placeholder = 'None',
                                               onInitialize = I('function() { this.setValue(""); }'))),
                 selectizeInput("flds", label = "Group Coloring:",
                                choices = fields, selected = "POP",
                                options = list(maxItems = 1,
                                               placeholder = 'Choose color grouping')),
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
              plotOutput("PCAPlot", width = "1280px", height = "1024px",
                         dblclick = "dclk",
                         brush = brushOpts(id = "brsh", resetOnNew = TRUE)))
  ),
  helpText("Developed by: Pavel Salazar-Fernandez")
)
#</UI>

#<SERVER> ####
server <- function(input, output) {
  #<REACTIVES>
  # Inputs
  sub.pops <- reactive(pca.data[pca.data$POP %in% input$pops,IDcol])
  pope.idn <- reactive(pca.data[pca.data$POP %in% input$pope,IDcol])
  groups <- reactive(as.character(levels(refact(
    pca.data[(pca.data$POP != input$pope),input$flds]))))
  PCaCol <- reactive(paste0("PC",input$PCa))
  PCbCol <- reactive(paste0("PC",input$PCb))
  pct.PCa <- reactive(paste0(PCaCol(),
                             " (",percent(eval[input$PCa]/sum(eval)),")"))
  pct.PCb <- reactive(paste0(PCbCol(),
                             " (",percent(eval[input$PCb]/sum(eval)),")"))
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  #</REACTIVES>
  
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
    # Data subset
    pca.keep <- pca.data
    if (!is.null(input$pops)) {
      pca.keep <- pca.data[ allIDs %in% sub.pops(), , drop = F]
    }
    if (input$pope %in% pops) {
      #print(pope.idn())
      head(allIDs)
      pca.emph <- pca.data[ allIDs %in% pope.idn(), , drop = F]
      pca.emph$plotColors <- "#22001A"
      pca.keep <- subset(pca.keep, !(ID %in% pca.emph$ID))
    }
    
    #Coloring
    if (input$flds == "POP") {
      if ("COLORX" %in% colnames(pca.keep)) {
        pca.keep$plotColors <- paste0(pca.keep$COLOR,"FF")
      } else {
        grp.colors <- setNames(rainbow(length(pops), s = 0.5, v = 0.9), pops)
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
      {if (input$pope %in% pops & input$type == 3) {
        geom_point(data = pca.emph,
                   aes_string(x = PCaCol(), y = PCbCol(), color = "plotColors"),
                   size = 3, alpha = 0.8, shape = 22,
                   fill = "#22001A", color = "white") }} +
      {if (input$pope %in% pops & input$type == 2) {
        geom_label(data = pca.emph,
                   aes_string(x = PCaCol(), y = PCbCol(), label = "ID"),
                   size = 3, alpha = 0.8, color = "white",
                   fill = pca.emph$plotColors, fontface = "bold") }} +
      {if (input$pope %in% pops & input$type == 1) {
        geom_label(data = pca.emph,
                   aes_string(x = PCaCol(), y = PCbCol(), label = "POP"),
                   size = 3, alpha = 0.8, color = "white",
                   fill = pca.emph$plotColors, fontface = "bold") }} +
      
      # TODO: Emphasis removes centroid, to be fixed.
      
      # Aesthetics
      scale_color_identity(name = input$flds,
                           labels = groups(), guide = "legend") +
      {if (!input$legon) theme(legend.position = "none") } +
      ggtitle(ifelse(input$plottitle == "", deftitle, input$plottitle)) +
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
  
  output$dlPlotpdf <- downloadHandler(
    filename = function() { paste0(deftitle,".pc", input$PCa,"x", input$PCb,".pdf")},
    content = function(file) {
      ggsave(file, plot = pca.plot(), device = "pdf", 
             width = 297, height = 210, units = "mm", dpi = 300, scale = 1.2)
    }
  )
  
  output$dlPlotpng <- downloadHandler(
    filename = function() { paste0(deftitle,".pc", input$PCa,"x", input$PCb,".png")},
    content = function(file) {
      ggsave(file, plot = pca.plot(), device = "png",
             width = 297, height = 210, units = "mm", dpi = 300, scale = 1.2)
    }
  )
  
  output$brshinfo <- renderPrint({
    # Get the brushed points based on the specified PC columns
    brushed_points <- brushedPoints(pca.data, input$brsh, xvar = PCaCol(), yvar = PCbCol())
    
    # Select only available columns among those we want
    available_columns <- intersect(c("ID", "POPULATION", "POP", "MLABEL", "SLABEL","COUNTRY","SUPERPOP", "CONTINENT"), colnames(pca.data))
    
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
