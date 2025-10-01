# AUTO ADMIXTURE PLOTTER (Shiny) v4.1 beta
# Shiny: app.R
# Author: Pavel Salazar-Fernandez (epsalazarf@gmail.com)
# Version Upgrade (R 4.0+): March 20 2025
# Latest Update: September 29 2025

# Requirements:
# - R library: shinyjs, ggplot2, tidyverse
# - Q files from the admixture program (all Q files with same suffix are read).
# - Optional: popinfo file

# Pipeline:
# 1. Reads Q table file.
# 2. Generates plot.
# 3. Optional: load POPINFO file for labeling and resorting.

# Features:
# - Changing the 'Ks' parameter changes the plot showed.
# - Select two or more populations for display.
# - Add borders to individual bars.
# - Group samples by selected factor.
# - Arrange samples by ancestral component.
# - Subset by a value from a category.
# - Custom re-coloring of components with visual color picker.

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(tidyverse)
  library(RColorBrewer)
  library(pheatmap)
  library(grid)
  library(colourpicker)
})

# UI ------------------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),
  titlePanel("PopMosaic ❧ ADMX"),
  helpText("ADMIXTURE Plotter - v1.2"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      # Upload Q files (multiple) and optional POPINFO file
      fileInput("qfiles", "Upload Q files:", 
                multiple = TRUE, accept = c(".Q", "text/plain")),
      fileInput("pifile", "Upload POPINFO file (optional):",
                multiple = FALSE, accept = c(".csv", ".tsv", ".txt")),
      # Warning message for POPINFO file row mismatch
      uiOutput("popinfo_warning"),
      hr(),
      # Dynamically generate Q file selection based on upload
      uiOutput("qFileSelectUI"),
      textInput("plottitle", label = "Plot Title", value = ""),
      hr(),
      # POPINFO Options: factor and groups selection (rendered only if POPINFO is valid)
      uiOutput("factormenu"),
      uiOutput("groupsmenu"),
      uiOutput("popfactmenu"),
      hr(),
      # Plot Options
      checkboxInput("brdr", label = "Add Borders", value = FALSE),
      checkboxInput("ksort", label = "K Sort", value = FALSE),
      checkboxInput("xlabs", label = "Show Names", value = FALSE),
      radioButtons("autogrp", label = "Grouping", selected = 0,
                   choices = list("Default" = 0, "K Groups" = 1, "Factor" = 2)),
      hr(),
      # Dynamic color inputs based on number of clusters (K)
      uiOutput("colorInputs"),
    ),
    mainPanel(
      width = 10,
      tabsetPanel(
        tabPanel("ADMIXTURE Plot", 
                 plotOutput("AdmixPlot", height = "480px", width = "100%")),
        tabPanel("Confusion Matrix", 
                 plotOutput("ConfusionPlot", height = "480px", width = "60%")),
        tabPanel("K Donut Plot", 
                 plotOutput("KDonutPlot", height = "480px", width = "100%")),
        tabPanel("Data Table", 
                 DT::DTOutput("Ktable"))
      )
    )
  ),
  helpText("Developed by: Pavel Salazar-Fernandez")
)

# SERVER --------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive: Process uploaded Q files and generate a list for selection.
  qfiles_list <- reactive({
    req(input$qfiles)
    df <- input$qfiles
    # Derive default title from the first file assuming pattern: <title>.<K#>.Q
    defaultTitle <- gsub("[0-9]+\\.Q$", "", df$name[1])
    if (input$plottitle == "") {
      updateTextInput(session, "plottitle", value = defaultTitle)
    }
    # Escape regex metacharacters in the default title.
    defaultTitleEsc <- stringr::str_replace_all(defaultTitle, "([\\^\\$\\.\\|\\(\\)\\[\\]\\*\\+\\?\\\\])", "\\\\\\1")
    # Build regex pattern using the escaped default title.
    pattern <- paste0("^", defaultTitleEsc, "([0-9]+)\\.Q$")
    valid <- grepl(pattern, df$name)
    if (sum(valid) == 0) {
      validate(need(FALSE, "No valid Q files found matching the expected pattern."))
    }
    df <- df[valid, ]
    # Extract the K value from the filename.
    df$K_value <- as.numeric(sub(pattern, "\\1", df$name))
    df <- df[order(df$K_value), ]
    # Create a named vector: names as "K=<value>" and values as datapath.
    setNames(df$datapath, paste0("K=", df$K_value))
  })
  
  # UI for selecting which Q file (i.e., which K) to display.
  output$qFileSelectUI <- renderUI({
    req(qfiles_list())
    selectInput("Ks", "Select Q file:", choices = qfiles_list())
  })
  
  # Read the selected Q file.
  qData <- reactive({
    req(input$Ks)
    data <- tryCatch({
      read.table(input$Ks, header = FALSE, sep = "", stringsAsFactors = FALSE)
    }, error = function(e) {
      validate(need(FALSE, "Error reading the selected Q file."))
    })
    
    # Validate that all columns are numeric, as expected for admixture proportions.
    if (!all(sapply(data, is.numeric))) {
      validate(need(FALSE, "The Q file must contain only numeric values. Please check that it is a headerless, space-delimited table."))
    }
    
    # Rename columns from V1, V2, ... to K1, K2, ...
    data <- data %>% rename_with(~ gsub("V", "K", .x))
    data
  })
  
  # Number of clusters (K) determined by number of columns in Q data.
  nK <- reactive({
    req(qData())
    ncol(qData())
  })
  
  # Read POPINFO file if provided.
  popinfo <- reactive({
    if (is.null(input$pifile)) return(NULL)
    tryCatch({
      read_tsv(input$pifile$datapath)
    }, error = function(e) {
      validate(need(FALSE, "Error reading POPINFO file."))
    })
  })
  
  # Display a warning if POPINFO row count does not match Q data.
  output$popinfo_warning <- renderUI({
    req(input$pifile, qData(), popinfo())
    if(nrow(qData()) != nrow(popinfo())) {
      span(style = "color:red; font-size:small;", "Error: sample row numbers differ")
    }
  })
  
  # Dynamic UI for factor selection based on valid POPINFO file.
  output$factormenu <- renderUI({
    req(popinfo())
    # Only render factor menu if row counts match.
    if(nrow(qData()) != nrow(popinfo())) return(NULL)
    df <- popinfo()
    uniquecols <- sapply(df, function(x) length(unique(x)))
    fields <- names(uniquecols[uniquecols > 1 & uniquecols < nrow(df) / 2])
    if(length(fields) == 0) return(NULL)
    selectizeInput("fctr", label = "Factor", choices = fields,
                   options = list(maxItems = 1, placeholder = 'Choose...'))
  })
  
  # Dynamic UI for groups selection based on chosen factor.
  output$groupsmenu <- renderUI({
    req(popinfo(), input$fctr)
    # Only render group selection if POPINFO is valid.
    if(nrow(qData()) != nrow(popinfo())) return(NULL)
    groups <- unique(popinfo()[[input$fctr]])
    selectizeInput("grps", label = "Select Group(s)",
                   choices = groups, multiple = TRUE,
                   options = list(placeholder = 'Choose factor value(s)'))
  })
  
  output$popfactmenu <- renderUI({
    req(popinfo(), input$fctr)
    checkboxInput("popfactor", label = "POP as second factor", value = FALSE)
  })
  
  # Dynamic color inputs generated based on the number of clusters.
  output$colorInputs <- renderUI({
    req(nK())
    n <- nK()
    default_palette <- c("#e60049", "#0bb4ff", "#50e991", "#e6d800",
                         "#9b19f5", "#ffa300", "#dc0ab4", "#b3d4ff",
                         "#00bfa0", "#7c1158", "#fd7f6f", "#b2e061",
                         "#bd7ebe", "#ffee65", "#fdcce5", "#beb9db")
    inputs <- lapply(seq_len(n), function(i) {
      colourInput(inputId = paste0("col", i),
                  label = paste("Color", i, ":"),
                  value = if(i <= length(default_palette)) default_palette[i] else "#000000",
                  palette = "square")
    })
    do.call(tagList, inputs)
  })
  
  # Assemble bar colors from dynamic inputs.
  barcolors <- reactive({
    req(nK())
    sapply(seq_len(nK()), function(i) input[[paste0("col", i)]])
  })
  
  # Process Q data: merge with POPINFO only if valid; otherwise, skip merging.
  plotdata <- reactive({
    req(qData())
    data <- qData()
    # Merge with POPINFO only if provided and row counts match.
    if (!is.null(popinfo()) && nrow(data) == nrow(popinfo())) {
      data <- cbind(data, popinfo())
      
      # Define candidate column names for sample IDs.
      candidates <- c("Sample", "SampleID", "SID", "IID")
      # Check which candidates are present in the popinfo data.
      present <- candidates[candidates %in% colnames(popinfo())]
      
      if (length(present) > 0) {
        # Use the first candidate found.
        data <- data %>% mutate(ID = .data[[present[1]]])
      } else if (nrow(popinfo()) == length(unique(popinfo()[[1]]))) {
        # If no candidate is found, check if the first column in popinfo is unique.
        first_col <- colnames(popinfo())[1]
        data <- data %>% mutate(ID = .data[[first_col]])
      } else {
        # Fallback: create a numeric ID.
        data <- data %>% mutate(ID = row_number())
      }
    } else {
      # If POPINFO is not loaded or row counts don't match, use default numeric IDs.
      if (!("ID" %in% colnames(data))) {
        data <- data %>% mutate(ID = row_number())
      }
    }
    if (!("ID" %in% colnames(data))) {
      data <- data %>% mutate(ID = row_number())
    }
    
    # Reshape data: pivot from wide to long format.
    data_long <- data %>%
      pivot_longer(cols = starts_with("K"), names_to = "K", values_to = "Percent") %>%
      group_by(ID) %>%
      mutate(KGroup = K[which.max(Percent)],
             KProbability = max(Percent),
             Flag = 1) %>%
      ungroup()
    
    # Additional processing (e.g., ordering, filtering) follows...
    if (input$autogrp == 1) {
      data_long <- data_long %>% arrange(KGroup, desc(KProbability))
    }
    if (input$autogrp == 2 && !is.null(popinfo()) && !is.null(input$fctr)) {
      if (!is.null(input$grps) && length(input$grps) > 0) {
        data_long <- data_long %>% 
          mutate(Flag = if_else(.data[[input$fctr]] %in% input$grps, 1, 0),
                 !!input$fctr := factor(.data[[input$fctr]], levels = input$grps))
      }
    }
    if (input$ksort) {
      data_long <- data_long %>% group_by(KGroup) %>% arrange(desc(Percent), .by_group = TRUE) %>% ungroup()
    }
    
    data_long <- data_long %>% mutate(ID = forcats::fct_inorder(factor(ID)))
    
    if (input$autogrp == 2 && input$ksort) {
      # Group by the selected factor, then arrange by KGroup and descending KProbability
      data_long <- data_long %>%
        group_by(.data[[input$fctr]]) %>%
        arrange(KGroup, desc(KProbability), .by_group = TRUE) %>%
        ungroup() %>%
        # Finally, update the ordering of ID to reflect this new sorted order
        mutate(ID = forcats::fct_inorder(factor(ID)))
    }
    
    data_long
  })
  
  # Aggregate data for the K donut plot.
  KAggr <- reactive({
    req(plotdata())
    agg <- aggregate(Percent ~ K, subset(plotdata(), Flag == 1), FUN = sum)
    total <- sum(agg$Percent)
    agg <- agg %>%
      mutate(KFraction = Percent / total,
             ymax = cumsum(KFraction),
             ymin = c(0, head(ymax, n = -1)),
             labelPosition = (ymax + ymin) / 2,
             labelPct = paste0(K, "\n", sprintf("%1.2f%%", 100 * KFraction)))
    agg
  })
  
  # Compute the confusion matrix for the heatmap.
  meanmatrix <- reactive({
    req(plotdata())
    mat <- plotdata() %>%
      filter(Flag == 1) %>%
      group_by(KGroup, K) %>%
      summarise(MeanPercent = mean(Percent), .groups = 'drop') %>%
      pivot_wider(names_from = K, values_from = MeanPercent) %>%
      column_to_rownames(var = "KGroup") %>%
      as.matrix()
    mat
  })
  
  # Plot Outputs -------------------------------------------------------------
  
  output$AdmixPlot <- renderPlot({
    req(plotdata())
    n_samples <- dplyr::n_distinct(subset(plotdata(), Flag == 1)$ID)
    p <- ggplot(subset(plotdata(), Flag == 1), aes(x = ID, y = Percent, fill = K)) +
      geom_col(width = if (input$brdr) 0.85 else 1) +
      scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
      scale_fill_manual(values = barcolors()) +
      theme_minimal() +
      labs(title = input$plottitle, subtitle = paste("K =", nK(), "; Samples =", n_samples)) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            panel.grid = element_blank(), legend.position = "none")
    
    if (input$autogrp == 1) {
      p <- p + facet_grid(~ KGroup, scales = 'free', space = 'free')
    } else if (input$autogrp == 2 && !is.null(popinfo()) && !is.null(input$fctr)) {
      p <- p + facet_grid(
        as.formula(paste("~", input$fctr, ifelse(input$popfactor, "+ POP", ""))), 
        scales = 'free', 
        space = 'free')
    }
    
    if (input$xlabs) {
      p <- p + theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1, vjust = 0.5))
    } else {
      p <- p + theme(axis.text.x = element_blank(),
                     axis.ticks.x = element_blank())
    }
    p
  })
  
  output$ConfusionPlot <- renderPlot({
    req(meanmatrix())
    annocol <- list(
      Group = setNames(barcolors(), paste0("K", 1:nK())),
      `Comp.` = setNames(barcolors(), paste0("K", 1:nK()))
    )
    pheatmap(meanmatrix(), display_numbers = TRUE, number_color = "black",
             fontsize_number = 8, scale = "none",
             annotation_colors = annocol,
             annotation_row = data.frame(Group = paste0("K", 1:nK()), 
                                         row.names = paste0("K", 1:nK())),
             annotation_col = data.frame(`Comp.` = paste0("K", 1:nK()), 
                                         row.names = paste0("K", 1:nK())),
             main = input$plottitle,
             angle_col = 0, border_color = "white",
             legend = FALSE, annotation_legend = FALSE,
             color = c("gray90", colorRampPalette(rev(brewer.pal(11, "Spectral")))(100)))
  })
  
  output$KDonutPlot <- renderPlot({
    req(KAggr())
    ggplot(KAggr(), aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = K)) +
      geom_rect() +
      coord_polar(theta = "y") +
      xlim(c(2, 4)) +
      geom_label(aes(x = 3.5, y = labelPosition, label = labelPct), size = 4) +
      scale_fill_manual(values = barcolors()) +
      theme_minimal() +
      labs(y = input$plottitle, x = NULL) +
      theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank())
  })
  
  output$Ktable <- DT::renderDT({
    pivot_wider(plotdata(), names_from = K, values_from = Percent) %>%
      filter( Flag == 1) %>%
      select(-ID, -Flag, -KProbability) %>%
      relocate(KGroup, .after = last_col())},
    rownames = ifelse(is.null(popinfo()),TRUE,FALSE),
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
}

shinyApp(ui = ui, server = server)