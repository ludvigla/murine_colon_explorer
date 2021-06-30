
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

coords.list <- list(readRDS(file = "~/Eduardo/app_test/data/coords1"), readRDS(file = "~/Eduardo/app_test/data/coords2"))
expr.data.list <- list(readRDS(file = "~/Eduardo/app_test/data/expr.data1"), readRDS(file = "~/Eduardo/app_test/data/expr.data2"))
dim.data.list <- list(readRDS(file = "~/Eduardo/app_test/data/dim.data1"), readRDS(file = "~/Eduardo/app_test/data/dim.data2"))
contrib.data <- readRDS(file = "~/Eduardo/app_test/data/contrib.data")
cell.data.list <- list(readRDS(file = "~/Eduardo/app_test/data/cell.data1"), readRDS(file = "~/Eduardo/app_test/data/cell.data2"))
gene.choices <- readRDS("~/Eduardo/app_test/data/gene.choices")


palette.select <- list("Spectral" = rev(brewer.pal(n = 9, name = "Spectral")),
                    "viridis" = "viridis", "magma" = "magma", "Blues" = "Blues", "Greens" = "Greens",
                    "Oranges" = "Oranges", "OrRd" = "OrRd", "Purples" = "Purples", "Reds" = "Reds",
                    "Jet" = c("darkblue", "cyan", "yellow", "red", "darkred"), 
                    "GrRdBlk" = c("lightgray", "mistyrose", "red", "darkred", "black"),
                    "GrRd" = c("lightgray", "mistyrose", "red", "darkred"))



ui <- dashboardPage(
  
  header = dashboardHeader(title = "Murine colon \ndata explorer"),
  
  sidebar = dashboardSidebar(
    chooseSliderSkin("Nice"),
    width = 250,
    column(width = 12,
           selectInput(
             inputId = "var",
             label = "factor",
             choices = colnames(dim.data.list[[1]]),
             selected = "factor_1"),
           selectInput(
             inputId = "gene",
             label = "gene",
             choices = rownames(expr.data.list[[1]]),
             selected = "Muc2"),
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
           actionBttn(inputId = "go3", style = "bordered", size = "s", label = "Celltype correlation"),
           selectInput(
             inputId = "pal",
             label = "palette",
             choices = names(palette.select),
             selected = "Spectral")
    )
    
  ),
  
  body = dashboardBody(
    tags$head(tags$style(HTML("/* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }"))),
    fluidRow(
      tabBox(
        title = "",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", width = "100%",
        tabPanel(value = 1, title = "d0 : naive", column(width = 12, leafletOutput("map1", width = "100%", height = "600px"))),
        tabPanel(value = 2, title = "Td14 : DSS-induced colitis", column(width = 12, leafletOutput("map2", width = "100%", height = "600px")))
      ),
      bsModal(id = "modalExample", title = "Gene weights", "go", size = "large", uiOutput("myUIOutput")),
      bsModal(id = "modalExample2", title = "histogram", "go2", size = "large", plotOutput("histogram")),
      tags$head(tags$style(HTML('.modal-lg {width: 850px;}'))),
      bsModal(id = "modalExample3", title = "celltype correlation", "go3", size = "large", d3heatmapOutput("heatmap", height = "700px", width = "800px"))
    )
  )
)

server <- function(input, output, session) {
  
  output$myUIOutput <- renderUI({
    if (rv$lastBtn == "factor") {
      plotOutput("contrib")
    } else {
      p("Select a factor to visualize to get gene weights!")
    }
  })
  
  lapply(1:2, function(i) {
    output[[paste0("map", i)]] <- renderLeaflet({
      m <<- leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"), preferCanvas = TRUE)) %>% 
        addTiles(urlTemplate = paste0("map", i, "/tiles/{z}/{x}/{y}.png"),
                 option = list(continuousWorld = TRUE, tms = TRUE, 
                               tileSize = "256", minZoom = "1", maxZoom = "5")) %>%
        setView(lat = 0, lng = 0, zoom = 1) %>%
        setMaxBounds(lng1 = 0, lat1 = 0, lng2 = 245.4, lat2 = -250.3) %>%
        addStyleEditor() %>%
        addDrawToolbar(targetGroup = "draw",
                       polylineOptions = FALSE,
                       circleOptions = FALSE,
                       markerOptions = FALSE,
                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
    })
  })
  
  
  output$contrib <- renderPlot({
    contrib <- get_contrib()
    ggplot(contrib, aes(gene, val)) + 
      geom_bar(stat = "identity", fill = "lightgray", color = "black") +
      coord_flip() +
      labs( x = "", y = "", fill = "gene\nweight") +
      theme(panel.background = element_blank())
  })
  
  
  output$histogram <- renderPlot({
    c(data, variable, pal, pal_rev) %<-% get_data()
    ggplot(data, aes_string(paste0("`", variable, "_raw`"))) + 
      geom_histogram(fill = "lightgray", color = "black", bins = 30) +
      theme(panel.background = element_blank()) +
      labs(x = variable) +
      geom_vline(aes(xintercept = input$maxcutoff*max(data[, paste0(variable, "_raw")]), color = "cutoff_threshold"), linetype = "longdash") +
      scale_color_manual(values = c("cutoff_threshold" = "black"))
  })
  
  output$heatmap <- renderD3heatmap({
    i <- input$tabset1
    corMat <- cor(t(cell.data.list[[1]]))
    diag(corMat) <- NA
    d3heatmap(corMat, col = rev(RColorBrewer::brewer.pal(n = 11, name = "RdBu")), cexRow = 0.8, cexCol = 0.8, key = TRUE, key.location = "tr", keysize = 2, key.title = "correlation")
  })
  
  toListen <- reactive({
    list(input$map1_zoom, input$map2_zoom, get_data())
  })
  
  
  lapply(1:2, function(i) {
    observeEvent(toListen(), {
      c(data, variable, pal, pal_rev) %<-% get_data()
      proxy <- leafletProxy(paste0("map", i))
      radi = 1.5*2^(input[[paste0("map", i, "_zoom")]] - 1)
      proxy %>% clearMarkers() %>% clearControls() %>%
        addCircleMarkers(radius = radi, lat = -coords.list[[i]]$pixel_y, lng = coords.list[[i]]$pixel_x, 
                         fillColor = pal(data[, variable]), group = "Spots", fillOpacity = data[, "alpha"], stroke = FALSE) %>%
        addLegend(pal = pal_rev, position = "topright", title = variable, values = data[, variable], labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    })
  })
  
  rv <- reactiveValues(lastBtn = "factor")
  observeEvent(input$var, {
    if (input$var > 0 ) {
      rv$lastBtn = "factor"
    }
  })
  
  
  observeEvent(input$cell, {
    if (input$cell > 0 ) {
      rv$lastBtn = "cell"
    }
  })
  
  
  observeEvent(input$gene, {
    if (input$gene > 0 ) {
      rv$lastBtn = "gene"
    }
  })
  
  
  get_data <- reactive({
    i <- as.integer(input$tabset1)
    data <- coords.list[[i]]
    if (rv$lastBtn == "gene") {
      variable <- input$gene
      data[, paste0(variable, "_raw")] <- expr.data.list[[i]][input$gene, ]
      data[, variable] <- expr.data.list[[i]][input$gene, ]
    }  else if (rv$lastBtn == "factor") {
      variable <- input$var
      data[, paste0(variable, "_raw")] <- dim.data.list[[i]][, variable]
      data[, variable] <- dim.data.list[[i]][, variable]
    } else if (rv$lastBtn == "cell") {
      variable <- input$cell
      data[, paste0(variable, "_raw")] <- cell.data.list[[i]][variable, ]
      data[, variable] <- cell.data.list[[i]][variable, ]
    }
    
    if (input$maxcutoff < 1) {
      data[, variable][data[, variable] > quantile(data[, variable], probs = input$maxcutoff)] <- quantile(data[, variable], probs = input$maxcutoff)
    }
    if (input$scalealpha) {
      data[, "alpha"] <- scales::rescale(data[, variable], to = c(0, input$alpha))
    } else {
      data[, "alpha"] <- input$alpha
    }
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
  
  get_contrib <- reactive({
    if (rv$lastBtn == "factor") {
      vals <- sort(contrib.data[, input$var], decreasing = T)
      df <- data.frame(gene = factor(names(vals)[1:40], levels = rev(names(vals)[1:40])), val = vals[1:40])
      return(df)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
