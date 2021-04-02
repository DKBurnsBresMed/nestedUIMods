server <- function(input, output, session) {
  
  
  # The point of this server is to to handle the following elements:
  #   - slider to determine number of sets of inputs (N UIs)
  #   - radio buttons to select type of each of those UIs
  #   - individual sets of inputs for each of those UIs (switches, numerics, pickers, text)
  #   - system to store changes to the inputs, even across changes to UI type
  
  
  # element 1 - a simple slider -------------------------------------------
  
  output$UI_n_input_sets <- renderUI({
    sliderInput(
      inputId = "n_input_sets",
      label = "Number of input sets:",
      min = 1,
      step = 1,
      max = 50,
      value = 5,
      ticks = F,
      post = " set(s)",
      width = "100%"
    )
  })
  
  
  # element 2: type selector -------------------------------------------
  
  udf_gen_type_ui <- function(n_input_sets, nam_iso = NULL, selec_iso = NULL) {

    nam_elems <- lapply(1:n_input_sets, function(this_ui) {
      textInputIcon(
        inputId = paste0("UI_name_",this_ui),
        label = NULL,
        value = NULL, 
        placeholder = paste0("Insert name for UI #",this_ui),
        icon = icon("signature"),
        size = "sm",
        width = "100%"
      )
    })
    
    # select all A if it's null or has the wrong length. if wrong length, select A initially
    if (is.null(selec_iso)) selec_iso <- rep("A",n_input_sets)
    if (length(selec_iso) < n_input_sets) {
      selec_iso <- c(selec_iso,rep("A",n_input_sets - length(selec_iso)))
    }
    
    type_elems <- lapply(1:n_input_sets, function(this_ui) {
      prettyRadioButtons(
        inputId = paste0("UI_type_",this_ui),
        label = NULL,
        status = "primary",
        choices = LETTERS[1:3],
        selected = "A",
        inline = TRUE,
        width = "100%",
        shape = "square"
      )
    })
    
    # produce the UI
    lapply(1:n_input_sets, function(this_ui){
      fluidRow(
        width = 12,
        column(4,nam_elems[[this_ui]]),
        column(8,type_elems[[this_ui]])
      )
    })
  }
  
  # send the UI to the front end
  output$type_ui <- renderUI({
    
    req(!is.null(input$n_input_sets))
    
    udf_gen_type_ui(
      n_input_sets = input$n_input_sets,
      nam_iso = NULL,
      selec_iso = NULL
    )
    
  })
  
  
  # Data handling -------------------------------------------
  
  # This generates all the possible datasets that could exist for all of the different
  # UIs which could be generated
  
  
  

  # UI generator modules ----------------------------------------------------

  
  
  
}