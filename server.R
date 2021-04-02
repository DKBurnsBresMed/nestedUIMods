server <- function(input, output, session) {
  
  
  # The point of this server is to to handle the following elements:
  #   - slider to determine number of sets of inputs (N UIs)
  #   - radio buttons to select type of each of those UIs
  #   - individual sets of inputs for each of those UIs (switches, numerics, pickers, text)
  #   - system to store changes to the inputs, even across changes to UI type
  

  # default values ----------------------------------------------------------

  n_ui <- 5
  max_ui <- 50
  
  # element 1 - a simple slider -------------------------------------------
  
  output$UI_n_input_sets <- renderUI({
    sliderInput(
      inputId = "n_input_sets",
      label = "Number of input sets:",
      min = 1,
      step = 1,
      max = max_ui,
      value = n_ui,
      ticks = F,
      post = " set(s)",
      width = "100%"
    )
  })
  outputOptions(output,"UI_n_input_sets", suspendWhenHidden = FALSE, priority = 100)
  
  # tracker for the highest that n_input_sets has been during this session (observer to track it working)
  max_uis <- reactiveValues(n = n_ui)  
  observeEvent(input$n_input_sets,{
    if (input$n_input_sets > max_uis$n) max_uis$n <- input$n_input_sets
  })
  # observe(print(reactiveValuesToList(max_uis)))
  
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
  # UIs which could be generated. this is n_input_sets * 3 sets of inputs currently.
  # It is therefore far more efficient to have a function to generate a default
  # input set for A B and C type UIs
  

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
  

  # ~ initiation dataset ----------------------------------------------------

  # Make a set of default inputs covering types A B and C for each of the input sets
  input_sets_default <- eventReactive(input$UI_gen_input_sets,{
    lapply(1:input$n_input_sets, function(this_input_sets) {
      
      # identify the type of input
      
      this_name <- input[[paste0("UI_name_",this_input_sets)]]
      this_type <- input[[paste0("UI_type_",this_input_sets)]]
      
      # respond by generating default dataset according to type. Generate
      # all the types
      
      if(is.null(this_type)) {
        list(
          type = "A",
          A = udf_def_i_A(nam = NULL, n = this_input_sets),
          B = udf_def_i_B(nam = NULL, n = this_input_sets),
          C = udf_def_i_C(nam = NULL, n = this_input_sets)
        )
      } else {
        list(
          type = this_type,
          A = udf_def_i_A(nam = this_name, n = this_input_sets),
          B = udf_def_i_B(nam = this_name, n = this_input_sets),
          C = udf_def_i_C(nam = this_name, n = this_input_sets)
        )
      } 
    })
  })
  
  # So, in the background we want to keep a "LIVE" version of this, such that 
  # if the user adds or takes away sets of inputs or changes the types of individual
  # sets of inputs
  
  
  # debug printer for default values
  output$DBG_input_sets_default <- renderPrint(print(input_sets_default()))
  
  # UI generator modules ----------------------------------------------------

  
  
  
}