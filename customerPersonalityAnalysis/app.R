#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Customer Personality Analysis"),
    
    fluidRow(
      column(3,
        wellPanel(
          h4("Filter"),
          sliderInput("year_birth",
                      "Customer's birth year",
                      1893,1996,c(1900,1950)),
          selectInput("education", "Education",
                      c("Graduation", "PhD", "Master", "Basic", "2n cycle")
          ),
          checkboxGroupInput("status", "Marital Status",
                             c("Single", "Together", "Married", "Divorced", "Widow", "Alone")
          ),
          selectInput("spendType", "Amount spent on",
                      c("Wine", "Fruit", "Meat", "Fish", "Sweets", "Gold")
          ),
          sliderInput("income",
                      "Total of yearly household income",
                      1730,666666,c(3000,50000)),
          sliderInput("deals",
                      "Number of purchases made with a discount",
                      0,15,1),
          sliderInput("website",
                      "Number of purchases made through the company’s website",
                      0,27,1),
          sliderInput("catalogue",
                      "Number of purchases made using a catalogue",
                      0,28,1),
          sliderInput("stores",
                      "Number of purchases made directly in stores",
                      0,13,1),
          sliderInput("visits",
                      "Number of visits to company’s website in the last month",
                      0,20,1),
        )
      ),
      column(9,
             sidebarLayout(
               sidebarPanel(
                 sliderInput("bins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot")
               )
             )
      )
    ),
    #wellPanel(
    #  selectInput("xvar", "X-axis variable", axis_vars, selected = ""),
    #  selectInput("yvar", "Y-axis variable", axis_vars, selected = "Amount spent on wine"),
    #)
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
