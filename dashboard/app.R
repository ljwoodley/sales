library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)

sales <- read_csv("sales_data.csv")


ui <- navbarPage("Sales Dashboard",theme = shinytheme("flatly"),
   
  tabPanel("Yearly Revenue",
        fluidRow(
        mainPanel(plotlyOutput("salesplot",width = "150%",height="600px"),
                  column(12,
                         h6("*Double click on a Country to isolate or single click to remove it from graph")
                         
         )
      )
   )
),

  tabPanel("Sales Breakdown",
           sidebarLayout(
             sidebarPanel(
               selectInput("productInput","Product Type",choices = c("Accessories","Clothing","Bikes")),
               
               radioButtons("ageInput","Age Group",choices=c("Youth (<25)","Adults (35-64)","Young Adults (25-34)","Seniors (64+)"),
                            selected = "Youth (<25)"),
               
               radioButtons("yearInput","Year",choices=c("2013","2014","2015","2016"),selected="2013")
             ),
             
             mainPanel(plotlyOutput("TotalSales",height = "600"),width = 7)
           )
             ), 
  
  tabPanel("2016 Bike Sales (half-year)",
           fluidPage(
               selectInput("countryInput","Country",choices = c("Canada","Australia","United States","Germany","France","United Kingdom"),
                           selected="New York")
               
             ),
            
               mainPanel(plotlyOutput("bike_sales"),width="130%",height="200%")
                    
           )
   )
 

#--------SERVER--------#
server <- function(input, output) {
    
    #create data
  yearly_sales <- sales %>% 
      group_by(Country,Year=as.factor(Year)) %>% 
      summarise(Revenue=sum(Revenue)) 
  
    #line chart
  output$salesplot <- renderPlotly({ 
    plot_ly(yearly_sales,x=~Year,y=~Revenue, color=~Country,type="scatter",mode='lines+markers') %>% 
      layout(legend=list(x=100,y=0.7))
  })
  
  
  #create data set  
  total_sales <- sales %>% 
      group_by(Year,Country,`Age Group`,`Product Category`) %>% 
      summarise(total=sum(Revenue)) 
    
   #make it reactive 
  total <- reactive({total_sales %>% 
    filter(`Age Group`==input$ageInput,
           `Product Category` == input$productInput,
            Year==input$yearInput)
  })
  
 #pie chart   
  output$TotalSales <- renderPlotly({
   plot_ly(total()[,c(2,5)],labels= ~Country, values=~total, type='pie',textinfo='label+percent')
 })
  
  bike_quantity <- sales %>%
    filter(Year==2016,`Product Category`=="Bikes") %>% 
    group_by(Country,Month,`Sub Category`) %>% 
    summarise(total=sum(`Order Quantity`))
  
  #spread Sub Category for plotly bar chart
  bike_quantity <- spread(bike_quantity,`Sub Category`,total)
  #reorder levels 
  bike_quantity$Month <- factor(bike_quantity$Month,
                                levels=c("January","February","March","April","May","June","July","August",
                                         "September","October","November","December"))
  #make data reactive
  bike_sales <- reactive({bike_quantity %>% 
        filter(Country==input$countryInput)
               
  })
  
  #bar chart
  output$bike_sales <- renderPlotly({
    plot_ly(bike_sales(),x=~Month, y=~`Mountain Bikes`,name='Mountain Bikes', type="bar") %>% 
      add_trace(y=~`Road Bikes`,name="Road Bikes") %>%
      add_trace(y=~`Touring Bikes`,name="Touring Bikes") %>% 
      layout(yaxis=list(title='Count'),barmode='group',
             legend=list(x=100, y=0.7))
  })
  
}

shinyApp(ui = ui, server = server)

