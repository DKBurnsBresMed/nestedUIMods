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
          uiOutput("UI_n_input_sets")
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