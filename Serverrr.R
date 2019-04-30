
library(shinydashboard)
library(shiny)
library(ggplot2)


# ************************   FRONTEND  ************************************
ui <-	dashboardPage(skin= "purple",
                    dashboardHeader(title = "16BCE2061_R3"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Plots", tabName = "dashboard", icon = icon("edit")),
                        menuItem("Dataset Used", tabName = "dataset", icon = icon("copy")),
                        #menuItem("Structure", tabName = "struct", icon = icon("boxes")),
                        menuItem("Summary", tabName = "datasummary", icon = icon("bars")),
                        menuItem("Source Code", tabName = "scode", icon = icon("laptop-code")),
                        menuItem("Developer", tabName = "devp", icon = icon("user"))
                      )
                    ),
                    dashboardBody(
                      # Boxes need to be put in a row (or column)
                      tabItems(
                        tabItem(tabName= "dashboard",
                                fluidRow(
                                  infoBox("Pie Charts", 2, icon = icon("chart-line")),
                                  infoBox("ScatterPlot", 2, icon = icon("chart-line"), color= "purple"),
                                  infoBox("Boxplot", 1, icon = icon("chart-line"), color= "yellow"),
                                  infoBox("Barplot", 2, icon = icon("chart-line"), fill = TRUE),
                                  infoBox("Linegraphs", 1, icon = icon("chart-line"), color= "purple",fill = TRUE),
                                  infoBox("SubPlots", 2, icon = icon("chart-line"),color= "yellow",fill = TRUE)
                                  
                                ),
                                
                                fluidRow(
                                  
                                  box(  title = "PLOT 1",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot3", height = 400)),
                                  box(  title = "PLOT 2",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot4", height = 400)),
                                  box(  title = "PLOT 3",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot5", height = 400)),
                                  box(  title = "PLOT 4",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot6", height = 400)),
                                  box(  title = "PLOT 5",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot7", height = 400)),
                                  box(  title = "PLOT 6",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot8", height = 400)),
                                  box(  title = "PLOT 7",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot9", height = 400)),
                                  box(  title = "PLOT 8",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot10", height = 400)),
                                  box(  title = "PLOT 9",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot11", height = 400)),
                                  box(  title = "PLOT 10",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot12", height = 400)),
                                  box(  title = "PLOT 11",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot13", height = 400)),
                                  box(  title = "PLOT 12",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot14", height = 400)),
                                  box(  title = "PLOT 13",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot15", height = 400)),
                                  box(  title = "PLOT 14",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot16", height = 400)),
                                  box(  title = "PLOT 15",status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,plotOutput("plot17", height = 400))
                                  
                                )
                        ),
                        tabItem(tabName= "dataset", dataTableOutput("table")),
                        tabItem(tabName= "datasummary",dataTableOutput("summary")),
                        #tabItem(tabName= "struct",textOutput("str")), 
                        tabItem(tabName= "scode",
                                fluidRow(tags$img(src= "s1.png"),width="50px"),
                                fluidRow(tags$img(src= "s2.png"),width="50%"),
                                fluidRow(tags$img(src= "s3.png"),width="50%"),
                                fluidRow(tags$img(src= "s4.png"),width="50%")
                                ),
                          
                        
                        tabItem(tabName= "devp", tags$img(src = "ayush.png"))
                      )
                    )
)


# ************************   BACKEND  **************************************
server<- function(input, output) 
{
  
  d <- read.csv("dv.csv",stringsAsFactors = F,header=T)
  
  output$plot3 <- renderPlot({
    type <- tapply(d$latitude, d$type , FUN= function(x) length(unique(x)) )
    pie(type, main= "Total Distribution Of Types Of Apartments")
  })
  
  output$plot4 <- renderPlot({
    city <- tapply(d$state, d$city , FUN= function(x) length(unique(x)))
    pie(city, main= "Total Distribution of Apartments in various Cities")
  })
  
  output$plot5 <- renderPlot({
    price <- c(623,314,43,5,0)
    plot(price,xlab= "Price Range", ylab ="No Of Flats", main= "Price vs No. Of Flats", col= "red",border= 		"blue", type="o")
  })
  
  output$plot6 <- renderPlot({
    a<-tapply (d$latitude, d$zip, FUN= function(x) length(unique(x)))
    barplot(a, col= "cyan", main= "No Of Flats vs Zip", xlab= "Zip Of the Flat", ylab="Flat Count")
    
  }) 
  
  # Start ggplot and other plots from here
  output$plot7 <- renderPlot({
    ggplot(d, aes(x= beds, y= baths, colour= beds, size= beds))+ geom_point()
    
  }) 
  
  output$plot8 <- renderPlot({
    ggplot(d, aes(x= price, y= city, colour= type, size= price))+ geom_violin()
    
    
  }) 
  
  output$plot9 <- renderPlot({
    theme_set(theme_bw())
    ggplot(d, aes(x= city, y= price)) + geom_point(aes(col = city, size= sq__ft ))
    
  }) 
  
  output$plot10 <- renderPlot({
    theme_set(theme_bw())
    ggplot(d, aes(sq__ft))+ scale_fill_brewer(palette= "Spectral") + geom_histogram(aes(fill= 			 sale_date), bins = 4, col= "black", size= 0.1)
    
    
    
  }) 
  
  output$plot11<- renderPlot({
    ggplot(d, aes(sq__ft))+ scale_fill_brewer(palette= "Spectral") + geom_density(aes(fill= type), 			bins = 4, col= "black", size= 0.1)
    
  }) 
  
  output$plot12<- renderPlot({
    ggplot(d, aes(sq__ft))+ geom_violin(aes(y= beds, col= "type"))+ geom_point(aes(y= beds, col= 			"city"))
    
    
  }) 
  
  output$plot13<- renderPlot({
    ggplot(d, aes(price, city, colour= city, size= sq__ft))+geom_point()
    
  })
  
  output$plot14<- renderPlot({ 
    ggplot(d, aes(sq__ft, city, colour= sale_date))+geom_point()+facet_grid(~beds)
  })
  
  output$plot15<- renderPlot({
    ggplot(d, aes(sq__ft, city, colour= sale_date, size= beds))+geom_point()+facet_grid(~baths)
  })
  output$plot16<- renderPlot({
    ggplot(d, aes(beds, colour= sale_date))+ geom_bar()
  })
  
  output$plot17<- renderPlot({
    ggplot(d, aes(baths, colour= city))+ geom_bar()
  })
  
  output$table <- renderDataTable({
    d
    
  }) 
  
  output$summary <- renderDataTable({
    
    df <- data.frame(summary(d))
    df
    
  })
  output$str <- renderText({
    
    df1 <- data.frame(str(d))
    df1
    
  })
  
}

# ************************  FUNCTION TO CALL  *****************************

shinyApp(ui, server)

