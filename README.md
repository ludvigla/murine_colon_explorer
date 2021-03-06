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
  

[![]](https://user-images.githubusercontent.com/23736938/124126192-b7d53280-da7a-11eb-8aab-4286aeafc10f.mp4)

[![]](https://user-images.githubusercontent.com/23736938/124126317-da674b80-da7a-11eb-8fe0-cf55f36db955.mp4)

[![]](https://user-images.githubusercontent.com/23736938/124126375-e94dfe00-da7a-11eb-800f-e07be67d34de.mp4)

[![]](https://user-images.githubusercontent.com/23736938/124126434-f5d25680-da7a-11eb-83cb-712dde6aa831.mp4)
