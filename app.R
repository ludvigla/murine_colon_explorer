
library(shinydashboard)
library(htmlwidgets)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(STutility)
library(zeallot)
library(shinyBS)
library(shinyWidgets)
library(RColorBrewer)
#se.Colon <- readRDS("~/Eduardo/app_test/se.Colon.stereoscope")


#width <- 8008#9901
#height <- 7851#8929
#sf <- 250.3/8008#154.6226/9901
coords.list <- list(readRDS(file = "~/Eduardo/app_test/data/coords1"), readRDS(file = "~/Eduardo/app_test/data/coords2"))
expr.data.list <- list(readRDS(file = "~/Eduardo/app_test/data/expr.data1"), readRDS(file = "~/Eduardo/app_test/data/expr.data2"))
dim.data.list <- list(readRDS(file = "~/Eduardo/app_test/data/dim.data1"), readRDS(file = "~/Eduardo/app_test/data/dim.data2"))
contrib.data <- readRDS(file = "~/Eduardo/app_test/data/contrib.data")
cell.data.list <- list(readRDS(file = "~/Eduardo/app_test/data/cell.data1"), readRDS(file = "~/Eduardo/app_test/data/cell.data2"))
gene.choices <- readRDS("~/Eduardo/app_test/data/gene.choices")

#coords <- subset(GetStaffli(se.Colon)@meta.data[, c("pixel_x", "pixel_y", "sample")], sample == "1")
#coords <- coords[, 1:2]*sf
#saveRDS(coords, file = "~/Eduardo/app_test/data/coords1")
#expr.data <- se.Colon@assays$SCT@data[VariableFeatures(se.Colon), se.Colon$day %in% "d14"]
#saveRDS(expr.data, file = "~/Eduardo/app_test/data/expr.data2")
#dim.data <- se.Colon@reductions$NMF@cell.embeddings[rownames(coords.list[[2]]), ]
#saveRDS(dim.data, file = "~/Eduardo/app_test/data/dim.data2")
#contrib.data <- se.Colon@reductions$NMF@feature.loadings
#saveRDS(contrib.data, file = "~/Eduardo/app_test/data/contrib.data1")
#cell.data <- se.Colon@assays$stereoscope.low.level@data
#rownames(cell.data) <- gsub(pattern = "\\.1\\.", replacement = "(1)", x = rownames(cell.data))
#rownames(cell.data) <- gsub(pattern = "\\.2\\.", replacement = "(2)", x = rownames(cell.data))
#rownames(cell.data) <- gsub(pattern = "\\.", replacement = " ", x = rownames(cell.data))
#rownames(cell.data) <- gsub(pattern = "     ", replacement = " & γδ", x = rownames(cell.data))
#rownames(cell.data) <- gsub(pattern = "CCL21 ", replacement = "CCL21+", x = rownames(cell.data))
#rownames(cell.data) <- gsub(pattern = "CXCL14 ", replacement = "CXCL14+", x = rownames(cell.data))
#rownames(cell.data) <- gsub(pattern = "HAND1 ", replacement = "HAND1+", x = rownames(cell.data))
#rownames(cell.data) <- gsub(pattern = "COL6A5 ", replacement = "COL6A5+", x = rownames(cell.data))
#rownames(cell.data) <- gsub(pattern = "EBF ", replacement = "EBF+", x = rownames(cell.data))
#rownames(cell.data) <- gsub(pattern = "SPP1 ", replacement = "SPP1+", x = rownames(cell.data))
#rownames(cell.data) <- gsub(pattern = "SOX6 ", replacement = "SOX6+", x = rownames(cell.data))
#rownames(cell.data) <- gsub(pattern = "IFIT3 ", replacement = "IFIT3+", x = rownames(cell.data))
#rownames(cell.data) <- gsub(pattern = "PDGFRA ", replacement = "PDGFRA+", x = rownames(cell.data))
#rownames(cell.data) <- gsub(pattern = "IL18 ", replacement = "IL18+", x = rownames(cell.data))
#cell.data <- cell.data[, rownames(coords)]
#saveRDS(cell.data, file = "~/Eduardo/app_test/data/cell.data2")

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
      bsModal(id = "modalExample2", title = "histogram", "go2", size = "large", plotOutput("histogram"))
    )
  )
)

server <- function(input, output, session) {
  
  output$myUIOutput <- renderUI({
    print(rv$lastBtn)
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

  
  observe({
    click <- input$map1_click
    if(is.null(click))
      return()
    text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
    print(text)
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
