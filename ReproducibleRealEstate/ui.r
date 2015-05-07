################################################################################
#                                                                              #
#   User Interface for Zesimate MD Shiny App                                   #                                               #
#                                                                              #
################################################################################

# Set library(s)

  library(shiny)

# Define UI for miles per gallon application

shinyUI(fluidPage(
  
  # Application title
  titlePanel("King County Sales Analysis: Value of a View"),
  
  # Sidebar with controls to select the model and the variable to display
  
  sidebarLayout(
    sidebarPanel(
      # Recalculate Button
      actionButton('rerun', "Calculate/Recalculate Model"),
      
      # Select Input Type
      selectInput(
        "inputType", "Select Items to Modify",
        c('Data Filters' = 'dataf',
          'Model Variables' = 'mvar',
          'Spatial Model Options' = 'spect')),
      
      # Condition Data Filters panel
      conditionalPanel(
        condition = "input.inputType == 'dataf'",
        
        h4("Data Filters"),
      
        sliderInput("priceLimits", "Price Range",
                    min = 50000, max = 5000000, 
                    value = c(100000, 5000000),step=10000),
        sliderInput("lotSizeLimits", "Lot Size Range (SqFt)",
                    min = 500, max = 435600, 
                    value = c(1000, 43560*2), step=20),
        sliderInput("homeSizeLimits", "Home Size Range (SqFt)",
                    min = 300, max = 15000, 
                    value = c(500, 3000), step=50),
        sliderInput("bedLimits", "# of Bedrooms Range ",
                    min = 0, max = 14, 
                    value = c(0, 8), step=1)
       ),
      
      # Conditiona Model Variables
      conditionalPanel(
        condition = "input.inputType == 'mvar'",
        h4("Model Variables"),
      
        h6("Variables of Interest"),
        checkboxInput("viewMount", label = "Mountain View", value = TRUE),
        checkboxInput("viewWater", label = "Water View", value = TRUE),
        checkboxInput("viewOther", label = "Other View", value = TRUE),
        checkboxInput("byScore", label = "Use View Scores?", value = FALSE),
      
        h6("Control Variables"),
        checkboxInput("homeSize", label = "Home Size (1,000s SqFt)", value = TRUE),
        checkboxInput("Age", label = "Age of Home", value = TRUE),
        checkboxInput("Baths", label = "Baths", value = TRUE),
        checkboxInput("BldgGrade", label = "Building Quality", value = TRUE),
        checkboxInput("Fireplaces", label = "# of Fireplaces", value = TRUE),
        checkboxInput("Townhome", label = "Is a Townhome?", value = TRUE),
        checkboxInput("lotAcres", label = "Lot Size (acres)", value = TRUE),
        checkboxInput("WFNT", label = "Water frontage", value = TRUE),
        checkboxInput("Month", label = "Month", value = TRUE)
      ),
      
      # Conditiona Model Specifications
      conditionalPanel(
        condition = "input.inputType == 'spect'",
        h4("Spatial Modeling"),
        checkboxInput("spatEcon", label = "Use Spatial Error Model", value = FALSE),
  
        h4("Describe Spatial Weights Matrix"),
        sliderInput("swmKnn", "# of Nearest Neighbors",
                    min = 1, max = 25, 
                    value = 10,step=1),
        checkboxInput("swmType", label = "Distance Weighted?", value = TRUE),
        p(strong(div("WARNING: Calculation of spatial error model may take a few minutes.
          Existing output will become slightly transparent while calculations
          are being made.", style = "color:red")))
        
      )
    ),
    
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
   tabsetPanel(
     tabPanel("Sales Map", plotOutput("mapP")),
    tabPanel("Model Coefficients", div(tableOutput("valTable"), style="font-size:70%")),
    tabPanel("View Premiums", plotOutput("valPlot2")),
    tabPanel("Residuals Map", plotOutput("resPlot")),
    tabPanel("Diagnostics", tableOutput("diagTable"))#,
   )
  )
 )
))

