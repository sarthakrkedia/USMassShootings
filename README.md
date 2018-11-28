# USMassShootings
This R shiny app visualises Mass shooting in United States, intent of shooter, place of shooting and injuries recorded 

All my Shiny projects will need extra packages for R, a complete list of required packages:

library(ddply) library(shinythemes) library(data.table) # A faster way to handle data frames in R library(ggplot2) # For more control on plots library(ggthemes) # For prettier ggplot2 plot aesthetics and acessibility for color-blind palettes library(knitr) # For pretty tables library(lubridate) # For easy handling dates library(scales) # To add more ticks and facilitate plot interpretation library(tidyverse) library(stringr) library(kableExtra) library(DT) library(cowplot) library(lattice) library(chron) library(shiny) library(scales) require(leaflet) library(shinydashboard)

Shiny apps are contained in a single script called app.R. The script app.R lives in a directory (for example, newdir/) and the app can be run with runApp("newdir"). app.R has three components:

a user interface object
a server function
a call to the shinyApp function
You can run a Shiny app by giving the name of its directory to the function runApp. For example if your Shiny app is in a directory called my_app, run it with the following code:

library(shiny) runApp("my_app")
