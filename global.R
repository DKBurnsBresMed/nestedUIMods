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

n_inputs <- 5
input_types <- c("A", "B", "C", "B", "A")
isolated_input_sets <- list(
  inputs_1 = list(
    logicals = list(),
    numerics = list(
      A_numb1 = 1,
      A_numb2 = 5,
      A_numb3 = 10,
      A_numb4 = 20,
      A_numb5 = 50
    ),
    pickers = list(
      A_pick1 = list(
        selected = "option #1",
        choices = paste0("option #",1:5)
      ),
      A_pick2 = list(
        selected = "option #2",
        choices = paste0("option #",1:5)
      ),
      A_pick3 = list(
        selected = "option #4",
        choices = paste0("option #",1:5)
      )
    )
  ),
  inputs_2 = list(
    logicals = list(),
    numerics = list(B_numb1 = 0, B_numb2 = 1),
    pickers = list(
      B_pick1 = list(
        selected = "option #3",
        choices = paste0("option #",1:5)
      ),
      B_pick2 = list(
        selected = "option #4",
        choices = paste0("option #",1:5)
      ),
      B_pick3 = list(
        selected = "option #2",
        choices = paste0("option #",1:5)
      )
    )
  ),
  inputs_3 = list(
    logicals = list(C_switch1 = TRUE, C_switch2 = FALSE),
    numerics = list(C_numb1 = 0, C_numb2 = 1),
    pickers = list(
      C_pick1 = list(
        selected = "option #1",
        choices = paste0("option #",1:5)
      ),
      C_pick2 = list(
        selected = "option #2",
        choices = paste0("option #",1:5)
      ),
      C_pick3 = list(
        selected = "option #5",
        choices = paste0("option #",1:5)
      )
    )
  ),
  inputs_4 = list(
    logicals = list(),
    numerics = list(B_numb1 = 0, B_numb2 = 1),
    pickers = list(
      B_pick1 = list(
        selected = "option #1",
        choices = paste0("option #",1:5)
      ),
      B_pick2 = list(
        selected = "option #1",
        choices = paste0("option #",1:5)
      )
    )
  ),
  inputs_5 = list(
    logicals = list(),
    numerics = list(
      A_numb1 = 1,
      A_numb2 = 5,
      A_numb3 = 10,
      A_numb4 = 20,
      A_numb5 = 50
    ),
    pickers = list(
      A_pick1 = list(
        selected = "option #1",
        choices = paste0("option #",1:5)
      ),
      A_pick2 = list(
        selected = "option #2",
        choices = paste0("option #",1:5)
      ),
      A_pick3 = list(
        selected = "option #4",
        choices = paste0("option #",1:5)
      )
    )
  )
)

# To test:
# Func_Make_L2_UI_tab(
#   n = 3,
#   type = "C",
#   input_set = isolated_input_sets$inputs_3
# )




# Functions making UI elements --------------------------------------------

# level 1 UI creates the name and type of each level 2 UI. it is reasonably simple logically

Func_Make_L1_UI <- function(n_uis, names, types) {
  lapply(1:n_uis, function(this_ui) {
    fluidRow(
      width = 12,
      column(
        3,
        textInputIcon(
          inputId = paste0("UI_inputset_name",this_ui),
          label = NULL,
          placeholder = "Insert a name for this input set",
          value = names[this_ui],
          icon = icon("signature"),
          size = "sm",
          width = "100%"
        )
      ),
      column(
        9,
        radioGroupButtons(
          inputId = paste0("UI_inputset_type",this_ui),
          label = NULL,
          choices = c("A", "B", "C"),
          selected = types[this_ui],
          width = "100%",
          individual = FALSE,
          size = "sm",
          justified = TRUE,
          
        )
      )
    )
  })
}



# Makes a set of UIs using a number and a list of typings. This is the level 2 UI.
# The level 1 UI determines the name and type of each level 2 UI

Func_Make_L2_UI <- function(n_inputs, input_types, isolated_input_sets = NULL) {
  
  require(shinydashboard)
  require(shinyWidgets)
  
  # generate some tab names for simplicity and reduction of repitition
  
  tab_names <- paste0("tab #", 1:n_inputs)
  
  # depending on whether there are any isolated inputs or not, generate
  # a list of length 1:n_inputs containing default values
  
  isolated_input_sets <- lapply(1:n_inputs, function(this_input_set) {
    if (length(isolated_input_sets) < n_inputs) {
      Func_blank_input_set(type = input_types[this_input_set])
    } else {
      isolated_input_sets[[this_input_set]]
    }
  })
  
  
  # generate the UI elements. note that they get put into containers
  # when they get put into tabs. All that needs to go below is the
  # UI elements themselves
  
  
  tab_content <- lapply(1:n_inputs, function(This_ui) {
    print(str(This_ui))
    print(str(input_types[This_ui]))
    print(str(isolated_input_sets[[This_ui]]))
    Func_Make_L2_UI_tab(
      n         = This_ui,
      type      = input_types[This_ui],
      input_set = isolated_input_sets[[This_ui]]
    )
  })
  
  # put all of the UIs in a tab to go in the tab box
  
  final_tabs <- lapply(1:n_inputs, function(This_ui) {
    fluidRow(
      width = 12,
      column(
        12,
        tab_content[[This_ui]]
      )
    )
  })
  
  
  final_tabs
  
}




# ~ Sub-functions creating UI components ----------------------------------

Func_blank_input_set <- function(type) {
  if (type == "A") {
    list(
      logicals = list(),
      numerics = list(
        numb1 = 1,
        numb2 = 5,
        numb3 = 10,
        numb4 = 20,
        numb5 = 50
      ),
      pickers = list(
        pick1 = list(
          selected = "option #1",
          choices = paste0("option #",1:5)
        ),
        pick2 = list(
          selected = "option #2",
          choices = paste0("option #",1:5)
        ),
        pick3 = list(
          selected = "option #4",
          choices = paste0("option #",1:5)
        )
      )
    )
  } else if (type == "B") {
    list(
      logicals = list(),
      numerics = list(
        numb1 = 0,
        numb2 = 1
      ),
      pickers = list(
        pick1 = list(
          selected = "option #3",
          choices = paste0("option #",1:5)
        ),
        pick2 = list(
          selected = "option #4",
          choices = paste0("option #",1:5)
        ),
        pick3 = list(
          selected = "option #2",
          choices = paste0("option #",1:5)
        )
      )
    )
  } else {
    list(
      logicals = list(
        switch1 = TRUE,
        switch2 = FALSE
      ),
      numerics = list(
        numb1 = 0,
        numb2 = 1
      ),
      pickers = list(
        pick1 = list(
          selected = "option #3",
          choices = paste0("option #",1:5)
        ),
        pick2 = list(
          selected = "option #4",
          choices = paste0("option #",1:5)
        ),
        pick3 = list(
          selected = "option #2",
          choices = paste0("option #",1:5)
        )
      )
    )
  } 
}


Mod_input_set <- function(id, n, type, input_set) {
  moduleServer(
    id = id,
    function(input, output, session) {
      
      # reactive: either generate default input set or pass along the
      # one entered into the call to the module
      live_inputs <- reactive({
        if (is.null(input_set)) {
          # generate an input set containing the default values for this type
          Func_blank_input_set(type = type)
        } else {
          # There are some inputs in input_set, pass along
          input_set
        }
      })
      
      # count the amount of each type of input needed, now we have the inputs
      n_of_UI_types <- reactive({
        req(!is.null(live_inputs()))
        lapply(live_inputs(),class)
      })
      
      
      # now, generate the names of the UI elements for this input set. all 
      # individual inputs, as well as what they are housed in!
      this_input_UI_element_names <- reactive({
        req(!is.null(live_inputs()))
        req(!is.null(n_of_UI_types()))
        
        # make a list of the individual inputs required for this "type" of UI
        
        n_elem <- n_of_UI_types()
        
        # first thing is input names. if there are these types of inputs to make, make their
        # names
        input_names <-
          list(
            logicals = vector(mode = "list", length = length(n_elem$logicals)),
            numerics = vector(mode = "list", length = length(n_elem$numerics)),
            pickers  = vector(mode = "list", length = length(n_elem$pickers))
          )
        
        if (length(n_of_UI_types()$logicals) > 0) {
          input_names$logicals <- lapply(1:length(input_names$logicals), function(this_logical){
            paste0("UI_set_",n,"_logical_",this_logical)
          })
        }
        if (length(n_of_UI_types()$numerics) > 0) {
          input_names$numerics <- lapply(1:length(input_names$numerics), function(this_numeric){
            paste0("UI_set_",n,"_logical_",this_numeric)
          })
        }
        if (length(n_of_UI_types()$pickers) > 0) {
          input_names$pickers <- lapply(1:length(input_names$pickers), function(this_picker){
            paste0("UI_set_",n,"_logical_",this_picker)
          })
        }
        
        # return the input names
        return(input_names)
      })
      
      
      # Now that we have the names of the inputs we need to make we can proceed to
      # generate the UI elements
      UI_elements <- reactive({
        req(!is.null(live_inputs()))
        req(!is.null(n_of_UI_types()))
        req(!is.null(this_input_UI_element_names()))
        
        # get the names of the UI elements and their values
        n_elem <- n_of_UI_types()
        nams   <- this_input_UI_element_names()
        vals   <- live_inputs()
        
        # produce empty list to populate
        UI_elements <-
          list(
            logicals = vector(mode = "list", length = length(n_elem$logicals)),
            numerics = vector(mode = "list", length = length(n_elem$numerics)),
            pickers  = vector(mode = "list", length = length(n_elem$pickers))
          )
        
        # generate the UI elements
        if (length(n_of_UI_types()$logicals) > 0) {
          UI_elements$logicals <- lapply(1:length(UI_elements$logicals), function(this_logical){
            materialSwitch(
              inputId = nams$logicals[[this_logical]],
              label   = nams$logicals[[this_logical]],
              value   = vals$logicals[[this_logical]]
            )
          })
        }
        if (length(n_of_UI_types()$numerics) > 0) {
          UI_elements$numerics <- lapply(1:length(), function(this_numeric){
            numericInputIcon(
              inputId = nams$numerics[[this_numeric]],
              label   = nams$numerics[[this_numeric]],
              value   = vals$numerics[[this_numeric]],
              size    = "sm",
              icon    = icon("calculator"),
              width   = "100%"
            )
          })
        }
        if (length(n_of_UI_types()$pickers) > 0) {
          UI_elements$pickers <- lapply(1:length(), function(this_picker){
            pickerInput(
              inputId  = nams$pickers[[this_picker]],
              label    = nams$pickers[[this_picker]],
              choices  = vals$pickers[[this_picker]]$choices,
              selected = vals$pickers[[this_picker]]$selected,
              width    = "100%"
            )
          })
        }
        
        # return all of our elements
        return(UI_elements)
      })
      
      
      # Now that we have the UI elements, we need to house them inside of an appropriate
      # container, depending on the type
      
      output$this_UI <- renderUI({
        req(!is.null(UI_elements()))
        
        ui_title <- paste0("Set #",n,", Type ", type)
        
        status <- switch (type,
                          "A" = "primary",
                          "B" = "info",
                          "C" = "warning"
        )
        box(
          title = ui_title,
          status = status,
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          fluidRow(width = 12,
                   column(12, splitLayout(
                     UI_elements$logicals
                   ))),
          verticalLayout(UI_elements$numerics, fluid = TRUE),
          verticalLayout(UI_elements$numerics, fluid = TRUE)
        )
        
      })
    }
  )
}


one_input_set_UI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("this_UI"))
}



# function to make the "level 2" UI: takes the input set for the corresponding type
# of UI. The level 1 UI controls the types of each input set

Func_Make_L2_UI_tab <- function(n, type, input_set) {
  
  # First, pull out all of the required inputs. Create the UI elements
  
  logicals <- input_set$logicals
  numerics <- input_set$numerics
  pickers  <- input_set$pickers
  
  # for each type of input - generate the appropriate inputs, with the appropriate names
  
  if (length(logicals) > 0) {
    ui_logicals <- lapply(1:length(logicals), function(this_logical) {
      materialSwitch(
        inputId = paste0("UI_set_",n,"_type_",type,"_logical_",this_logical),
        label   = paste0("UI_set_",n,"_type_",type,"_logical_",this_logical),
        value   = logicals[[paste0(type,"_switch",this_logical)]]
      )
    })
  }
  if (length(numerics) > 0) {
    ui_numerics <- lapply(1:length(numerics), function(this_numeric) {
      numericInputIcon(
        inputId = paste0("UI_set_",n,"_type_",type,"_numeric_",this_numeric),
        label = paste0("UI_set_",n,"_type_",type,"_numeric_",this_numeric),
        value = numerics[[paste0(type,"_numb",this_numeric)]],
        size = "sm",
        icon = icon("calculator"),
        width = "100%"
      )
    })
  }
  if (length(pickers) > 0) {
    ui_pickers <- lapply(1:length(pickers), function(this_picker) {
      pickerInput(
        inputId = paste0("UI_set_",n,"_type_",type,"_picker_",this_picker),
        label = paste0("UI_set_",n,"_type_",type,"_picker_",this_picker),
        choices = pickers[[paste0(type,"_pick",this_picker)]][["choices"]],
        selected = pickers[[paste0(type,"_pick",this_picker)]][["selected"]],
        width = "100%"
      )
    })
  }
  
  
  # Now organise those UI elements according to the UI type
  
  if (type == "A") {
    
    UI <- box(
      title = paste0("Input set ", n, ", type ", type),
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      status = "primary",
      
      # Type A has 5 numerics, 3 pickers. These should be in a box with primary status.
      fluidRow(
        width = 12,
        h3("Type A"),
        column(
          6,
          tagList(
            ui_numerics[1:3]
          )
        ),
        column(
          6,
          tagList(
            ui_numerics[4:5]
          )
        )
      ),
      fluidRow(
        width = 12,
        column(
          12,
          ui_pickers
        )
      )
    )
    return(UI)
  }
  
  if (type == "B") {
    
    # 2 numerics 3 pickers, not in a box
    UI <- div(
      h3("Type B"),
      fluidRow(
        width = 12,
        column(6,ui_numerics[[1]]),
        column(6,ui_numerics[[2]])
      ),
      fluidRow(
        width = 12,
        column(
          12,
          ui_pickers
        )
      )
    )
    
    return(UI)
  }
  
  if (type == "C") {
    
    # logicals numerics  pickers 
    # 2        2        3 
    
    # in a box
    
    UI <- box(
      title = paste0("Input set ", n, ", type ", type),
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      status = "info",
      h3("Type C"),
      fluidRow(
        width = 12,
        column(6,ui_logicals[[1]]),
        column(6,ui_logicals[[2]]),
      ),
      fluidRow(
        width = 12,
        column(12,splitLayout(ui_numerics))
      ),
      verticalLayout(ui_pickers,fluid = TRUE)
      
    )
    
    return(UI)
  }
  
}

# this_type <- A
# n_logical <- 0
# n_numeric <- 5
# n_picker  <- 3

Mod_get_inputs <- function(id, set, type, RV_ISO) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
      n <- reactive({
        # number of each type of input
        if (type == "A") {
          n <- list(
            logical = 0,
            numeric = 5,
            picker  = 3
          )
        } else if (type == "B") {
          n <- list(
            logical = 0,
            numeric = 2,
            picker  = 3
          )
        } else {
          n <- list(
            logical = 2,
            numeric = 2,
            picker  = 3
          )
        }
      })
      
      print(unlist(n()))
      
      # logicals
      logicals <- reactive({
        req(!is.null(n))
        if (n()$logical == 0) {logicals <- list()} else {
          nams_logicals <- paste0("UI_set_",set,"_type_",type,"_logical_",1:n()$logical)
          logicals <- lapply(1:n()$logical, function(this_logical){
            if (!is.null(input[[nams_logicals[this_logical]]])) {
              input[[nams_logicals[this_logical]]]
            } else {
              RV_ISO$isolated_input_sets[[set]][["logicals"]]
            }
          })
          names(logicals) <- paste0(type,"_switch",1:n()$logical)
          return(logicals)
        }
      }) 
      
      print(str(logicals()))
      
      # numerics
      numerics <- reactive({
        req(!is.null(n))
        if (n()$numeric == 0) {numerics <- list()} else {
          nams_numerics <- paste0("UI_set_",set,"_type_",type,"_numeric_",1:n()$numeric)
          numerics <- lapply(1:n()$numeric, function(this_numeric){
            input[[nams_numerics[this_numeric]]]
          })
          names(numerics) <- paste0(type,"_numb",1:n()$numeric)
          return(numerics)
        }
      })
      
      # pickers
      pickers <- reactive({
        req(!is.null(n))
        if (n()$picker == 0) {pickers <- list()} else {
          nams_pickers <- paste0("UI_set_",set,"_type_",type,"_picker_",1:n()$picker)
          pickers <- lapply(1:n()$picker, function(this_picker){
            input[[nams_pickers[this_picker]]]
          })
          names(pickers) <- paste0(type,"_pick",1:n()$picker)
          return(pickers)
        }
      })
      
      
      OUT <- reactive({
        req(any(
          !is.null(logicals()),
          !is.null(numerics()),
          !is.null(pickers())
        ))
        list(
          logicals = logicals(),
          numerics = numerics(),
          pickers  = pickers()
        )
      })
      
      return(OUT())
    }
  )
}


Func_get_inputs <- function(set, type) {
  
  # number of each type of input
  if (type == "A") {
    n <- list(
      logical = 0,
      numeric = 5,
      picker  = 3
    )
  } else if (type == "B") {
    n <- list(
      logical = 0,
      numeric = 2,
      picker  = 3
    )
  } else {
    n <- list(
      logical = 2,
      numeric = 2,
      picker  = 3
    )
  }
  
  # logicals
  if (n()$logical == 0) {logicals <- list()} else {
    nams_logicals <- paste0("UI_set_",set,"_type_",type,"_logical_",1:n()$logical)
    logicals <- lapply(1:n()$logical, function(this_logical){
      isolate(input[[nams_logicals[this_logical]]])
    })
    names(logicals) <- paste0(type,"_switch",1:n()$logical)
  }
  
  # numerics
  if (n()$numeric == 0) {numerics <- list()} else {
    nams_numerics <- paste0("UI_set_",set,"_type_",type,"_numeric_",1:n()$numeric)
    numerics <- lapply(1:n()$numeric, function(this_numeric){
      isolate(input[[nams_numerics[this_numeric]]])
    })
    names(numerics) <- paste0(type,"_numb",1:n()$numeric)
  }
  
  # pickers
  if (n()$picker == 0) {pickers <- list()} else {
    nams_pickers <- paste0("UI_set_",set,"_type_",type,"_picker_",1:n()$picker)
    pickers <- lapply(1:n()$picker, function(this_picker){
      isolate(input[[nams_pickers[this_picker]]])
    })
    names(pickers) <- paste0(type,"_pick",1:n()$picker)
  }
  
  
  return(list(
    logicals = logicals,
    numerics = numerics,
    pickers = pickers
  ))
  
}
