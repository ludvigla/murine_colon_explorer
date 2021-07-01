# Murine colon spatial transcriptomics data explorer
With this app you can explore spatial transcriptomics data of sections from mouse colon tissue presented in "The spatial transcriptomic landscape of the healing intestine following damage" (Parigi. M. et al).

# Installation
Clone the repo from the terminal by running:

`$git clone https://github.com/ludvigla/murine_colon_explorer`

To run the app, you first need to install the following R packages:

CRAN:
- ggplot2
- zeallot
- shiny
- shinydashboard
- RColorBrewer
- viridis
- scales
- shinyWidgets
- leaflet
- leaflet.extras
- htmlwidgets
- shinyBS

GitHub:
- d3heatmap

You can install the packages directly with the install-packages.R script:

`$Rscript install-packages.R`

# Run the app
From RStudio, navigate to the cloned repository:

`setwd("~/murine_colon_explorer")`

You can then activate the app by running:

`library(shiny)`

`runApp()`

Or alternatively you can open the app.R file File->Open file->.../app.R and then click on the Run App icon at the top of the script.

# How to use
When you open the app, you will see an H&E image of the naive mouse colon tissue section (`d0 : naive`) overlaid by spots. The color of the spots correspond either to factor activity values, normalized gene expression or cell type proportions which you can select from in the left panel. You can also switch tabs to visualize the `d14 : DSS-induced colitis` mouse colon tissue section. 

Formatting options:
  * opacity : sets the (maximum) spot opacity to control transparanecy
  * maxcutoff : trim values based on a specifiec quantile, eg. a value of 0.99 will clip the values of the displayed value vector to the 99th quantile
  * reverse palette : should the color palette be reversed?
  * opacity gradient : should the spot opacity be scaled with the displayed value?
  * Factor gene weights : show a bar plot of the top contributing genes for a selected factor
  * Value histogram : show a histogram of the selected value vector
  * palette : select a color palette
  

[![demo]](https://user-images.githubusercontent.com/23736938/124124194-73489780-da78-11eb-80c8-0b34c28de1ff.mp4)

![demo](https://user-images.githubusercontent.com/23736938/124124416-aee36180-da78-11eb-857d-e6edf3ef5bc2.mp4)



