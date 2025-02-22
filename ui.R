ui <- dashboardPage(
  header = dashboardHeader(title = "Test"),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    chooseSliderSkin("Sharp"),
    fluidPage(
      
      # Application title
      titlePanel("Old Faithful Geyser Data"),
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          width = 3,
          uiOutput("UI_n_input_sets"),
          actionBttn(
            inputId = "UI_gen_input_sets",
            label = "Generate set of default inputs",
            style = "unite",
            color = "success",
            size = "lg",
            block = TRUE
          ),
          actionBttn(
            inputId = "UI_live_input_sets",
            label = "Pass settings into inputs",
            style = "unite",
            color = "primary",
            size = "lg",
            block = TRUE
          ),
          verbatimTextOutput("DBG_input_sets_default")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          width = 9,
          tabBox(
            title = "multi-ui UI",
            width = "100%",
            tabPanel(
              title = "name and type",
              icon = icon("calculator"),
              uiOutput("type_ui")
            )
          )
        )
      )
    )
  ),
  skin = "blue"
)