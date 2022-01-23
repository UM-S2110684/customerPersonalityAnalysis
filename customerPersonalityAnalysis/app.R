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
  "Number of visits to company’s website in the last month" = "NumWebVisitsMonth"
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Customer Personality Analysis"),
    
    fluidRow(
      column(3,
        wellPanel(
          h4("Filter"),
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
                      1730,666666,c(3000,50000)),
        ),
        wellPanel(
          selectInput("xvar", "X-axis variable", axis_vars, selected = "MntWines"),
          selectInput("yvar", "Y-axis variable", axis_vars, selected = "NumDealsPurchases"),
        )
      ),
      column(9,
          # Show a plot of the generated distribution
          ggvisOutput("plot1"),
      )
    ),
)

# Define server logic required to draw a histogram
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
  
  # calculate the age instead of 'Year-Birth'
  df['Year_Birth']= 2022-df['Year_Birth']
  names(df)[names(df)=="Year_Birth"] <- "Age"
  #Total spending on all items
  df['Total_Spent'] = df["MntWines"]+ df["MntFruits"]+ df["MntMeatProducts"]
  + df["MntFishProducts"]+ df["MntSweetProducts"]+ df["MntGoldProds"]
  
  # creating a new feature indicates the number of days customer engaged to the company
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
    movies <- reactive({
      # Due to dplyr issue #318, we need temp variables for input values
      minage <- input$age[1]
      maxage <- input$age[2]
      education <- input$education
      status <- input$status
      minincome <- input$income[1]
      maxincome <- input$income[2]
      
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
          Age >= minage,
          Age <= maxage,
          Education == education,
          Marital_Status == status,
          Income >= minincome,
          Income <= maxincome,
        )
      
      m <- as.data.frame(m)
      
      # Add column which says whether the movie won any Oscars
      # Be a little careful in case we have a zero-row data frame
      #m$has_oscar <- character(nrow(m))
      #m$has_oscar[m$Oscars == 0] <- "No"
      #m$has_oscar[m$Oscars >= 1] <- "Yes"
      m
    })
    
    # Function for generating tooltip text
    movie_tooltip <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.null(x$ID)) return(NULL)
      
      # Pick out the movie with this ID
      df <- isolate(movies())
      movie <- df[df$ID == x$ID, ]
      
      paste0("<b>", movie$Title, "</b><br>",
             movie$Year, "<br>",
             "$", format(movie$BoxOffice, big.mark = ",", scientific = FALSE)
      )
    }
    # A reactive expression with the ggvis plot
    vis <- reactive({
      # Lables for axes
      xvar_name <- names(axis_vars)[axis_vars == input$xvar]
      yvar_name <- names(axis_vars)[axis_vars == input$yvar]
      
      # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
      # but since the inputs are strings, we need to do a little more work.
      xvar <- prop("x", as.symbol(input$xvar))
      yvar <- prop("y", as.symbol(input$yvar))
      
      movies %>%
        ggvis(x = xvar, y = yvar) %>%
        layer_points(size := 50, size.hover := 200,
                     fillOpacity := 0.2, fillOpacity.hover := 0.5,
                     key := ~ID) %>%
        add_tooltip(movie_tooltip, "hover") %>%
        add_axis("x", title = xvar_name) %>%
        add_axis("y", title = yvar_name) %>%
        #add_legend("stroke", title = "Unknown", values = c("Yes", "No")) %>%
        #scale_nominal("stroke", domain = c("Yes", "No"),
                      #range = c("orange", "#aaa")) %>%
        set_options(width = 500, height = 500)
    })
    vis %>% bind_shiny("plot1")
  }
}
# Run the application 
shinyApp(ui = ui, server = server)
