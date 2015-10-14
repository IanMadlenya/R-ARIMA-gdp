library(shiny)
## Create the shiny user interface
shinyUI(pageWithSidebar(
  ## Main title
  headerPanel("Estimate Performance"),
 
  sidebarPanel(
    helpText("This app estimates different measures based on current data"),
    wellPanel(
    p(strong("Measures")),
    checkboxInput(inputId = "gdp", label = "GDP", value = TRUE),
    checkboxInput(inputId = "hpi", label = "HPI", value = FALSE),
    checkboxInput(inputId = "unrate",  label = "UNRATE", value = FALSE),
    
    checkboxInput(inputId = "gdp_base", label = "GDP Base", value = TRUE),
    checkboxInput(inputId = "hpi_base", label = "HPI Base", value = FALSE),
    checkboxInput(inputId = "unrate_base",  label = "UNRATE Base", value = FALSE),
    
    checkboxInput(inputId = "gdp_adverse", label = "GDP Adverse", value = TRUE),
    checkboxInput(inputId = "hpi_adverse", label = "HPI Adverse", value = FALSE),
    checkboxInput(inputId = "unrate_adverse",  label = "UNRATE Adverse", value = FALSE)),

    submitButton("Submit"),
    p("Documentation:", a("About", href = "About.html"))
  ),
  
  mainPanel(
    tabsetPanel(
      ## Tab with the plot of the Forecast object
      tabPanel("Arima Plot",
      conditionalPanel(condition = "input.gdp",
                       br(),
                       div(plotOutput(outputId = "plot_gdp"))),
      
      conditionalPanel(condition = "input.hpi",
                       br(),
                       div(plotOutput(outputId = "plot_hpi"))),
      
      conditionalPanel(condition = "input.unrate",
                       br(),
                       div(plotOutput(outputId = "plot_unrate"))),
      
      ## base plot
      conditionalPanel(condition = "input.gdp_base", 
                       br(),
                       div(plotOutput(outputId = "plot_gdp_base"))),
      
      conditionalPanel(condition = "input.hpi_base",
                       br(),
                       div(plotOutput(outputId = "plot_hpi_base"))),
      
      conditionalPanel(condition = "input.unrate_base",
                       br(),
                       div(plotOutput(outputId = "plot_unrate_base"))),
      
      ## adverse plot
      conditionalPanel(condition = "input.gdp_adverse", 
                       br(),
                       div(plotOutput(outputId = "plot_gdp_adverse"))),
      
      conditionalPanel(condition = "input.hpi_adverse",
                       br(),
                       div(plotOutput(outputId = "plot_hpi_adverse"))),
      
      conditionalPanel(condition = "input.unrate_adverse",
                       br(),
                       div(plotOutput(outputId = "plot_unrate_adverse")))),
      
      ## Tab with the actual Forecast object
      tabPanel("Arima Forecast",
               conditionalPanel(condition = "input.gdp",
                                br(),
                                div(dataTableOutput(outputId = "Fcast_gdp"))),
               
               conditionalPanel(condition = "input.hpi",
                                br(),
                                div(dataTableOutput(outputId = "Fcast_hpi"))),
               
               conditionalPanel(condition = "input.unrate",
                                br(),
                                div(dataTableOutput(outputId = "Fcast_unrate"))))
    
    )
  )
)
)