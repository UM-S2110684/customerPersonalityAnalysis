#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggvis)
library(dplyr)

#Global variable for x and y
axis_vars <- c(
  "Amount spent on Wine" = "MntWines",
  "Amount spent on Fruit" = "MntFruits",
  "Amount spent on Meat" = "MntMeatProducts",
  "Amount spent on Fish" = "MntFishProducts",
  "Amount spent on Sweets" = "MntSweetProducts",
  "Amount spent on Gold" = "MntGoldProds",
  "Number of purchases made with a discount" = "NumDealsPurchases",
  "Number of purchases made through the company’s website" = "NumWebPurchases",
  "Number of purchases made using a catalogue" = "NumCatalogPurchases",
  "Number of purchases made directly in stores" = "NumStorePurchases",
  "Number of visits to company’s website in the last month" = "NumWebVisitsMonth",
  "Total spent on Goods" = "Total_Spent"
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Customer Purchase Behavoir Analysis"),
    
    fluidRow(
      column(3,
        wellPanel(
          h4("Choose the following filter to apply to your graph: "),
          sliderInput("age",
                      "Customer's Age",
                      26,82,c(30,50)),
          selectInput("education", "Education",
                      c("Graduation", "PhD", "Master", "Basic", "2n cycle")
          ),
          radioButtons("status", "Marital Status",
                             c("Single", "Together", "Married", "Divorced", "Widow"),
                             selected = "Single"
          ),
          sliderInput("income",
                      "Total of yearly household income",
                      1730,162397,c(3000,50000)),
          sliderInput("days_engaged",
                      "The number of days customer engaged",
                      0,699,c(100,400)),
        ),
        wellPanel(
          h4("Choose x-axis and y-axis variable for your graph to be plotted:"),
          selectInput("xvar", "X-axis variable", axis_vars, selected = "MntWines"),
          selectInput("yvar", "Y-axis variable", axis_vars, selected = "NumDealsPurchases"),
        )
      ),
      column(9,
          # Show a plot of the generated distribution
          ggvisOutput("plot1"),
          wellPanel(
            span("Number of cusotmer selected:",
                 textOutput("n_customer")
            )
          )
      )
    ),
)

# Define server logic required to plot graph
server <- {
  #Read data set
  df <- read.csv("marketing_campaign.csv",sep = '\t')
  
  #Data Cleaning Process
  #Remove "Z_CostContact" and "Z_Revenue" that non-informative
  df <- subset(df, select = -c(Z_CostContact, Z_Revenue))
  
  #Feature Engeneering
  table(df$Marital_Status)
  table(df$Education)
  df["Marital_Status"][df["Marital_Status"] == 'Alone'] <- 'Single'
  df["Marital_Status"][df["Marital_Status"] == 'YOLO'] <- 'Single'
  df["Marital_Status"][df["Marital_Status"] == 'Absurd'] <- 'Single'
  
  #Exploratory Data Analysis
  #Calculate the age instead of 'Year-Birth'
  df['Year_Birth']= 2022-df['Year_Birth']
  names(df)[names(df)=="Year_Birth"] <- "Age"
  
  #Total spending on all items
  df['Total_Spent'] = df["MntWines"]+ df["MntFruits"]+ df["MntMeatProducts"]
  + df["MntFishProducts"]+ df["MntSweetProducts"]+ df["MntGoldProds"]
  
  #Creating a new feature indicates the number of days customer engaged to the company
  df$Dt_Customer <- as.Date(df$Dt_Customer, format="%d-%m-%Y")
  newest_customer <- max(df$Dt_Customer)
  df['newest_customer'] = newest_customer
  df['days_engaged'] <- (df['newest_customer'] - df['Dt_Customer'])
  df <- subset(df, select = -c(Dt_Customer, newest_customer))
  
  # Dropping the outliers 
  df <- na.omit(df)
  df <- subset(df, Age<100 & Income<600000)
  
  #Encode "Education" and "Marital_Status" into numeric forms
  df['Education_encode'] <- as.numeric(as.factor(df$Education))
  df['Marital_Status_encode'] <- as.numeric(as.factor(df$Marital_Status))
  df_enc <- subset(df, select=c(Education_encode, Education))
  df_enc1 <- subset(df, select=c(Marital_Status_encode, Marital_Status))
  df <- subset(df, select = -c(Education, Marital_Status))
  names(df)[names(df)=="Marital_Status_encode"] <- "Marital_Status"
  names(df)[names(df)=="Education_encode"] <- "Education"
  
  
  function(input, output) {
    # Filter the movies, returning a data frame
    cpbas <- reactive({
      # Due to dplyr issue #318, we need temp variables for input values
      minage <- input$age[1]
      maxage <- input$age[2]
      education <- input$education
      status <- input$status
      minincome <- input$income[1]
      maxincome <- input$income[2]
      mindays_engaged <- input$days_engaged[1]
      maxdays_engaged <- input$days_engaged[2]
      
      #Convert education string to int
      if(education=="2n Cycle"){
        education =1;
      }else if(education=="Basic"){
        education =2;
      }else if(education=="Graduation"){
        education =3;
      }else if(education=="Master"){
        education =4;
      }else{
        education =5;
      }
      #Convert status string to int
      if(status=="Divorced"){
        status =1;
      }else if(status=="Married"){
        status =2;
      }else if(status=="Single"){
        status =3;
      }else if(status=="Together"){
        status =4;
      }else{
        status =5;
      }
        
      # Apply filters
      m <- df %>%
        filter(
          #Range of Age
          Age >= minage,
          Age <= maxage,
          #Only 1 education type
          Education == education,
          #Only 1 status selected
          Marital_Status == status,
          #Range of Income
          Income >= minincome,
          Income <= maxincome,
          #Range of customer days engaged with company
          days_engaged >= mindays_engaged,
          days_engaged <= maxdays_engaged,
        )
      m <- as.data.frame(m)
    })
    
    # A reactive expression with the ggvis plot
    vis <- reactive({
      # Lables for axes
      xvar_name <- names(axis_vars)[axis_vars == input$xvar]
      yvar_name <- names(axis_vars)[axis_vars == input$yvar]
      
      # Since the inputs are strings
      xvar <- prop("x", as.symbol(input$xvar))
      yvar <- prop("y", as.symbol(input$yvar))
      
      #Generate plotting
      cpbas %>%
        ggvis(x = xvar, y = yvar) %>%
        #Style the plot
        layer_points(size := 50, size.hover := 200,
                     stroke:="purple",fill:= "purple",
                     fillOpacity := 0.2, fillOpacity.hover := 0.5,
                     key := ~ID) %>%
        layer_model_predictions(model="lm")%>%
        add_tooltip(movie_tooltip, "hover") %>%
        add_axis("x", title = xvar_name) %>%
        add_axis("y", title = yvar_name) %>%
        set_options(width = 500, height = 500)
    })
    vis %>% bind_shiny("plot1")
    #Show the number of customer took part in plot
    output$n_customer <- renderText({ nrow(cpbas()) })
  }
}
# Run the application 
shinyApp(ui = ui, server = server)
