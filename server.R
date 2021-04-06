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
  observe(print(reactiveValuesToList(max_uis)))
  
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
  

  # ~ Module dataset inputs -------------------------------------------------------
  
  # The udf_def_i_A B and C functions are used to generate a default dataset
  # in each of the A B and C modules. Inside these modules, the isolated
  # input set is compared to the defaults - if the default set is different
  # to the inputs inside the module, then we know that the user has changed
  # something. This means user changes can override the default settings, which
  # are used in the first instance to populate the inputs. It also means that 
  # settings that have been changed can be passed back into the main server,
  # into the reactivevalues list which then feeds through into further calculations
  # AFTER the nested UI has finished

  # so in short:
  #   - There is a module for type A, B, C
  #   - These modules identify whether any of the inputs in the input list 
  #     are different to those which have been supplied in the call to the module
  #     (i.e. whether the user has changed any of the inputs)
  #   - The modules return only the current input set (i.e. for A, B or C type UI)
  #     and a flag for the type
  #   - The results from all modules from all input sets are consolidated into one
  #     list. This list is then passed along into further model calculations
  
  # Example:
  #   ui1 <- callModule(module = input_module, id = "input set 1")
  #   
  #   This should contain all of the ui elements and a reactive called OUT_LIST
  #   which 
  
  input_module <- function(id, n, nam, type, iso_inputs) {
    moduleServer(
      id = id,
      module = function(input, output, session) {
        
        # get namespace
        ns <- NS(id)
        
        # First, generate a default input set for this 
        if (type == "A") {
          default_inputs <- udf_def_i_A(nam = nam,n = n)
        } else if (type == "B") {
          default_inputs <- udf_def_i_B(nam = nam,n = n)
        } else {
          default_inputs <- udf_def_i_C(nam = nam,n = n)
        }
        
        # Now, if any of the isolated inputs coming from the main server are NOT the same
        # as the default values, then these are the ones we should be passing through
        # the module. Note that iso_inputs will be e.g. RV_iso_inputs[[1]][[RV_iso_inputs[[1]]$type]]
        # i.e. the current type selection from the radio buttons fed into the list of all possible inputs
        # to pull out the subset of inputs that are relevant for this set of inputs
        
        if (any(unlist(iso_inputs, recursive = TRUE) != unlist(default_inputs, recursive = TRUE))) {
          
          # so the user has changed some of the inputs for this type. We don't want to lose
          # this information, so we should be feeding it through the UI from the point of
          # generation
          live_inputs <- iso_inputs
          
        } else {
          live_inputs <- default_inputs
        }
        
        # generate the ui elements for type A
        
        ## logicals (empty in type A, so no UI and empty list is all that's needed)
        
        logic_iso <- reactive({
          if (length(live_inputs$logic) == 0) {
            list()
          } else {
            # get the defaults
            def <- live_inputs$logic
            
            lapply(1:length(def), function(this_logic) {
              
              this_nam <- paste0("logic_",this_logic)
              
              if (is.null(input[[this_nam]])) {
                # the value is null, so output the default value (which will populate it)
                def[[this_logic]]
              } else {
                # The value is not null, so isolate it so that it can be
                # restored for later
                isolate(input[[this_nam]])
              }
            })
          }
        })
        
        
        ## numerics
        
        num_iso <- reactive({
          if (length(live_inputs$num) == 0) {
            list()
          } else {
            # get the defaults
            def <- live_inputs$num
            
            lapply(1:length(def), function(this_num) {
              
              this_nam <- paste0("num_",this_num)
              
              if (is.null(input[[this_nam]])) {
                # the value is null, so output the default value (which will populate it)
                def[[this_num]]
              } else {
                # The value is not null, so isolate it so that it can be
                # restored for later
                isolate(input[[this_nam]])
              }
            })
          }
        })
        
        
        ## pickers (remember this one needs options and selected)
        
        
        # Note that for pickers, you need a nested list of selected and choices
        
        pck_iso <- reactive({
          if (length(live_inputs$pck) == 0) {
            list()
          } else {
            # get the defaults
            def <- live_inputs$pck
            
            lapply(1:length(def), function(this_pck) {
              
              this_nam <- paste0("pck_",this_pck)
              
              if (is.null(input[[this_nam]])) {
                # the value is null, so output the default value (which will populate it)
                def[[this_pck]]
              } else {
                # The value is not null, so isolate it so that it can be
                # restored for later. note that the available choices will always match default!
                list(
                  selected = isolate(input[[this_nam]]),
                  choices = def[[this_pck]]$choices  
                )
              }
            })
          }
        })
        
        txt_iso <- reactive({
          if (length(live_inputs$txt) == 0) {
            list()
          } else {
            # get the defaults
            def <- live_inputs$txt
            
            lapply(1:length(def), function(this_txt) {
              
              this_nam <- paste0("txt_",this_txt)
              
              if (is.null(input[[this_nam]])) {
                # the value is null, so output the default value (which will populate it)
                def[[this_txt]]
              } else {
                # The value is not null, so isolate it so that it can be
                # restored for later
                isolate(input[[this_nam]])
              }
            })
          }
        })
        
        
        # ui outputs - return a NULL if ther shouldn't be elements
        # for this input type
        
        output$ui_logic <- renderUI({
          if(length(live_inputs$logic) == 0) return(NULL)
          lapply(1:length(live_inputs$logic), function(this_logic){
            switchInput(
              inputId = paste0("logic_",this_logic),
              label = paste0("logiceric input #", this_logic, ", type A"),
              value = logic_iso()[[this_logic]],
              min = 0,
              icon = icon("abacus"),
              size = "lg",
              width = "100%"
            )
          })
        })
        output$ui_num <- renderUI({
          if(length(live_inputs$num) == 0) return(NULL)
          lapply(1:length(live_inputs$num), function(this_num){
            numericInputIcon(
              inputId = paste0("num_",this_num),
              label = paste0("Numeric input #", this_num, ", type A"),
              value = num_iso()[[this_num]],
              min = 0,
              icon = icon("abacus"),
              size = "lg",
              width = "100%"
            )
          })
        })
        output$ui_pck <- renderUI({
          if(length(live_inputs$pck) == 0) return(NULL)
          lapply(1:length(live_inputs$pck), function(this_pck){
            pickerInput(
              inputId = paste0("pck_",this_pck),
              label = paste0("pckeric input #", this_pck, ", type A"),
              selected = pck_iso()[[this_pck]]$selected,
              choices = pck_iso()[[this_pck]]$choices,
              width = "100%"
            )
          })
        })
        output$ui_txt <- renderUI({
          if(length(live_inputs$txt) == 0) return(NULL)
          lapply(1:length(live_inputs$txt), function(this_txt){
            textInputIcon(
              inputId = paste0("txt_",this_txt),
              label = paste0("text input #", this_txt, ", type A"),
              value = txt_iso()[[this_txt]],
              min = 0,
              icon = icon("signature"),
              size = "lg",
              width = "100%"
            )
          })
        })
        
        
        # now that we have all the input types, we can consolidate into 
        # a UI
        
        output$UI <- renderUI({
          
          # aesthetics etc for each type
          if (type == "A") {
            status <- "primary"
          } else if (type == "B") {
            status <- "info"
          } else {
            status <- "success"
          }
          
          box(
            title = paste0("input set #", n, ", type: ", type),
            status = status,
            solidHeader = TRUE,
            width = "100%",
            collapsible = TRUE,
            collapsed = TRUE,
            uiOutput(ui_logic),
            uiOutput(ui_num),
            uiOutput(ui_pck),
            uiOutput(ui_txt)
          )
          
          
        })
        
        
        # output the LIVE input set. pass along the iso reactivevalues into one list,
        # only for this type. add in a token for type and n so we can recieve it again
        # within the main server
        OUT_LIST <- reactive({
          list(
            type = type,
            n = n,
            dat = list(
              logic = logic_iso(),
              num = num_iso(),
              pck = pck_iso(),
              txt = txt_iso()
            )
          )
        })
        
        
        # this is the return of the entire module server - a flag for type and n,
        # and then the input set as object "dat", which can then be slotted into
        # the appropriate place in the reactivevalues containing the full input 
        # dataset. This means that even if the user changes the "type", the data
        # they entered for the other type possibilities for that input set
        # won't be lost
        return(OUT_LIST)
        
        
        # OUT_LIST is then injected into a reactivevalues object inside the main server
        # which contains all of the possible input sets for all possible inputs
        
      }
    )
  }
  
  
  # debug printer for default values
  output$DBG_input_sets_default <- renderPrint(print(input_sets_default()))
  
  # UI generator modules ----------------------------------------------------

  getOutputsFromModule <- function(id) {
    ns<-NS(id)
    tagList(
      uiOutput(outputId = ns("UI"))
    )
  }
  
  
}