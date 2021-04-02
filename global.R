# global.R
# 
# 


# libraries ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(data.table)


# globally available objects ----------------------------------------------

global_test_text <- "global.R is working"

# EXAMPLES

# dummy inputs for 5 uis: also works as the default values on boot!
# A reactivevalues object is populated with this data at boot. it is called
# RV_defaults




