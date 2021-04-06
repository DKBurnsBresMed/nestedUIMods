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





# Functions ---------------------------------------------------------------

# ~ Generate default input set for each type ------------------------------

# Type A: has no logicals, 4 numerics, 3 pickers, 5 text inputs

udf_def_i_A <- function(nam, n) {
  list(
    name  = nam,
    n     = n,
    logic = list(),
    num   = list(1,5,8,20),
    pck   = list(
      list(
        selected = "option #1",
        choices = paste0("option #",1:5)
      ),
      list(
        selected = "option #1",
        choices = paste0("option #",1:3)
      ),
      list(
        selected = "option #2",
        choices = paste0("option #",1:10)
      )
    ),
    txt   = list(paste0("Type A: some text for text input #",1:5))
  )
}

# Type B: 2 switches, 2 numerics, 1 picker, 5 txt

udf_def_i_B <- function(nam, n) {
  list(
    name  = nam,
    n     = n,
    logic = list(TRUE, FALSE),
    num   = list(25, 50),
    pck   = list(
      list(
        selected = "A",
        choices = LETTERS
      )
    ),
    txt   = list(paste0("Type B: some different text to type A, for inputs #",1:5))
  )
}

# Type B: 3 switches, 10 numerics, 1 picker, 1 txt

udf_def_i_C <- function(nam, n) {
  list(
    name  = nam,
    n     = n,
    logic = list(TRUE, FALSE, TRUE),
    num   = list(25, 50, 200, 85, 150, 90, 1, 0.0001, 0.25, 100),
    pck   = list(
      list(
        selected = "aa",
        choices = paste0(letters[1:3],letters[1:3])
      )
    ),
    txt   = list("Type C: text input")
  )
}
