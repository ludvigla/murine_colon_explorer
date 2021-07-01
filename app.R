library(shinydashboard)
library(htmlwidgets)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(zeallot)
library(shinyBS)
library(shinyWidgets)
library(RColorBrewer)
library(d3heatmap)

coords.list <- list(readRDS(file = "data/coords1"), readRDS(file = "data/coords2"))
expr.data.list <- list(readRDS(file = "data/expr.data1"), readRDS(file = "data/expr.data2"))
dim.data.list <- list(readRDS(file = "data/dim.data1"), readRDS(file = "data/dim.data2"))
contrib.data <- readRDS(file = "data/contrib.data")
cell.data.list <- list(readRDS(file = "data/cell.data1"), readRDS(file = "data/cell.data2"))
gene.choices <- readRDS("data/gene.choices")
htmp <- readRDS("data/htmp")
htmp <- lapply(cell.data.list, function(x) {
  corMat <- cor(t(x))
  diag(corMat) <- NA
  corMat <- as.data.frame(corMat)
  return(corMat)
})


palette.select <- list("Spectral" = rev(brewer.pal(n = 9, name = "Spectral")),
                    "viridis" = "viridis", "magma" = "magma", "Blues" = "Blues", "Greens" = "Greens",
                    "Oranges" = "Oranges", "OrRd" = "OrRd", "Purples" = "Purples", "Reds" = "Reds",
                    "Jet" = c("darkblue", "cyan", "yellow", "red", "darkred"), 
                    "GrRdBlk" = c("lightgray", "mistyrose", "red", "darkred", "black"),
                    "GrRd" = c("lightgray", "mistyrose", "red", "darkred"))



ui <- dashboardPage(
  
  header = dashboardHeader(title = "Murine colon \ndata explorer"),
  
  sidebar = dashboardSidebar(
    width = 250,
    column(width = 12,
           selectInput(
             inputId = "var",
             label = "factor",
             choices = colnames(dim.data.list[[1]]),
             selected = "factor_1"),
           selectizeInput(
             inputId = "gene",
             label = "gene",
             choices = NULL),
           selectInput(
             inputId = "cell",
             label = "cell",
             choices = rownames(cell.data.list[[1]]),
             selected = "Distal absorptive"),
           sliderInput(
             inputId = "alpha",
             label = "Opacity", value = 1,
             min = 0, max = 1, step = 0.01
           ),
           sliderInput(
             inputId = "maxcutoff",
             label = "Max cutoff", value = 1,
             min = 0.5, max = 1, step = 0.01
           ),
           fluidRow(
             column(width = 4, checkboxInput(
               inputId = "revpal", 
               label = "reverse palette", 
               value = FALSE)),
             column(width = 4, checkboxInput(
               inputId = "scalealpha", 
               label = "opacity gradient", 
               value = FALSE))
           ),
           actionBttn(inputId = "go", style = "bordered", size = "s", label = "Factor gene weights"),
           actionBttn(inputId = "go2", style = "bordered", size = "s", label = "Value histogram"),
           actionBttn(inputId = "go3", style = "bordered", size = "s", label = "Cell correlations"),
           selectInput(
             inputId = "pal",
             label = "palette",
             choices = names(palette.select),
             selected = "Jet")
    )
    
  ),
  
  body = dashboardBody(

    fluidRow(
      tabBox(
        title = "",
        id = "tabset1", width = "100%",
        tabPanel(
          value = 1, 
          title = "d0 : naive", 
          column(width = 12, leafletOutput("map1", width = "100%", height = "800px"))),
        tabPanel(
          value = 2, 
          title = "Td14 : DSS-induced colitis", 
          column(width = 12, leafletOutput("map2", width = "100%", height = "800px")))
      ),
      bsModal(
        id = "modalExample", 
        title = "Gene weights", "go", 
        size = "large", 
        uiOutput("myUIOutput")),
      bsModal(
        id = "modalExample2", 
        title = "histogram", "go2", 
        size = "large", 
        plotOutput("histogram")),
      bsModal(
        id = "modalExample3", 
        title = "Cell-cell correlation plot", "go3", 
        size = "large", 
        d3heatmapOutput("corrplot", height = "700px", width = "750px"))
    )
  )
)

server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'gene', selected = "Muc2", choices = gene.choices, server = TRUE)
  
  # Create reactive value (responds on input)
  rv <- reactiveValues(lastBtn = "factor")
  rv2 <- reactiveValues(curgene = "Muc2")
  # Set last button to "gene" when a factor is selected
  observeEvent(input$var, {
    if (input$var > 0 ) {
      rv$lastBtn = "factor"
    }
  })
  
  
  # Set last button to "cell" when a cell type is selected
  observeEvent(input$cell, {
    if (input$cell > 0 ) {
      rv$lastBtn = "cell"
    }
  })
  
  
  # Set last button to "gene" when a gene is selected
  observeEvent(input$gene, {
    if (input$gene > 0 ) {
      rv$lastBtn = "gene"
    }
  })
  
  
  # Generate data
  get_data <- reactive({
    # Selected tab
    i <- as.integer(input$tabset1)
    # Get coordinates for selected tab
    data <- coords.list[[i]]
    # Collect data based on input type; gene, factor or cell type
    if (rv$lastBtn == "gene") {
      variable <- input$gene
      if (input$gene %in% gene.choices) {
        rv2$curgene <- variable
      } else {
        variable <- rv2$curgene
      }
      data[, paste0(variable, "_raw")] <- expr.data.list[[i]][variable, ]
      data[, variable] <- expr.data.list[[i]][variable, ]
    }  else if (rv$lastBtn == "factor") {
      variable <- input$var
      data[, paste0(variable, "_raw")] <- dim.data.list[[i]][, variable]
      data[, variable] <- dim.data.list[[i]][, variable]
    } else if (rv$lastBtn == "cell") {
      variable <- input$cell
      data[, paste0(variable, "_raw")] <- cell.data.list[[i]][variable, ]
      data[, variable] <- cell.data.list[[i]][variable, ]
    }
    
    # Clip data if max cut-off if specified, i.e. if max cut-off is lower than 1
    if (input$maxcutoff < 1) {
      data[, variable][data[, variable] > quantile(data[, variable], probs = input$maxcutoff)] <- quantile(data[, variable], probs = input$maxcutoff)
    }
    # Create a column with opacity values for each spot scaled by the value to be visualized
    # The maximum opcaity value is set by alpha 
    # If no scaling is applied, all spots will have the same opacity specified by alpha
    if (input$scalealpha) {
      data[, "alpha"] <- scales::rescale(data[, variable], to = c(0, input$alpha))
    } else {
      data[, "alpha"] <- input$alpha
    }
    # reverse color palette if specified
    if (input$revpal %in% c(T, F)) {
      pal <- colorNumeric(
        palette = palette.select[[input$pal]],
        alpha = TRUE, 
        reverse = input$revpal,
        domain = data[, variable])
      pal_rev <- colorNumeric(
        palette = palette.select[[input$pal]],
        alpha = TRUE, reverse = !input$revpal,
        domain = data[, variable])
    }
    return(list(data, variable, pal, pal_rev))
  })
  
  
  # Create a pop-up window to display gene contributions for factors
  output$myUIOutput <- renderUI({
    if (rv$lastBtn == "factor") {
      plotOutput("contrib")
    } else {
      p("Select a factor to visualize to get gene weights!")
    }
  })
  
  
  # generate gene contribution data for UIOutput
  get_contrib <- reactive({
    if (rv$lastBtn == "factor") {
      vals <- sort(contrib.data[, input$var], decreasing = T)
      df <- data.frame(gene = factor(names(vals)[1:40], levels = rev(names(vals)[1:40])), val = vals[1:40])
      return(df)
    }
  })
  
  
  # render gene contributions plot to be displayed in UIOutput
  output$contrib <- renderPlot({
    contrib <- get_contrib()
    if (is.null(contrib)) {
      ggplot()
    } else {
      ggplot(contrib, aes(gene, val)) + 
        geom_bar(stat = "identity", fill = "lightgray", color = "black") +
        coord_flip() +
        labs( x = "", y = "", fill = "gene\nweight") +
        theme(panel.background = element_blank())
    }
  })
  
  
  # render a histogram pop-up for the selected values to be visualzied
  output$histogram <- renderPlot({
    c(data, variable, pal, pal_rev) %<-% get_data()
    ggplot(data, aes_string(paste0("`", variable, "_raw`"))) + 
      geom_histogram(fill = "lightgray", color = "black", bins = 30) +
      theme(panel.background = element_blank()) +
      labs(x = variable) +
      geom_vline(aes(xintercept = input$maxcutoff*max(data[, paste0(variable, "_raw")]), color = "cutoff_threshold"), linetype = "longdash") +
      scale_color_manual(values = c("cutoff_threshold" = "black"))
  })

  
  output$corrplot <- renderD3heatmap({
    vals <- htmp[[as.integer(input$tabset1)]]
    d3heatmap(vals, 
              opacity = 1,
              col = rev(RColorBrewer::brewer.pal(n = 11, name = "RdBu")), 
              show_grid = FALSE, 
              na.rm = TRUE, 
              key = TRUE, 
              density.info = "none", 
              key.location = "tr", 
              keysize = 1, 
              key.title = "correlation")
  })

  
  # Create leaflets for two tabs and add tiles
  # Output id is map1 and map2
  lapply(1:2, function(i) {
    output[[paste0("map", i)]] <- renderLeaflet({
      m <- leaflet(width = 500, height = 500, 
                    options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"), preferCanvas = TRUE)) %>% 
        addTiles(urlTemplate = paste0("map", i, "/tiles/{z}/{x}/{y}.png"), 
                 options = tileOptions(continuousWorld = FALSE, 
                               tms = TRUE, 
                               tileSize = "256", 
                               minZoom = "1", 
                               maxZoom = "5")) %>%
        setView(lat = 0, lng = 0, zoom = 1) %>%
        #setMaxBounds(lng1 = 0, lat1 = 0, lng2 = 245.4, lat2 = -250.3) %>% # TODO: find a way to automatically set bounds around tiled image
        addStyleEditor() %>%
        addDrawToolbar(targetGroup = "draw",
                       polylineOptions = FALSE,
                       circleOptions = FALSE,
                       markerOptions = FALSE,
                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
        fitBounds(lng1 = 0, lat1 = 0, lng2 = 256, lat2 = -256) %>%
        addMiniMap(tiles = paste0("map", i, "/tiles/{z}/{x}/{y}.png"))
      return(m)
    })
  })
  

  # Listen to changes in zoom levels and updated of get_data()
  toListen <- reactive({
    list(input$map1_zoom, input$map2_zoom, get_data())
  })

  
  # Draw new circle markers when zoom levels are changed or when get_data() is updated
  lapply(1:2, function(i) {
    observeEvent(toListen(), {
      c(data, variable, pal, pal_rev) %<-% get_data()
      proxy <- leafletProxy(paste0("map", i))
      radi = 1.7*2^(input[[paste0("map", i, "_zoom")]] - 1)
      proxy %>% 
        clearMarkers() %>% 
        clearControls() %>%
        addCircleMarkers(radius = radi, 
                         lat = -coords.list[[i]]$pixel_y, 
                         lng = coords.list[[i]]$pixel_x, 
                         fillColor = pal(data[, variable]), 
                         group = "Spots", 
                         fillOpacity = data[, "alpha"], 
                         stroke = FALSE) %>%
        addLegend(pal = pal_rev, 
                  className = "legendbox",
                  position = "topright", 
                  title = variable, 
                  values = data[, variable], 
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    })
  })

  
}

# Run the application
shinyApp(ui = ui, server = server)
