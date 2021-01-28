library(sp)
library(DT)
library(leaflet)
library(raster)
library(rgeos)
library(dplyr)
library(shiny)

ui <- fluidPage(
  
  titlePanel("Calculate Visitor and Resident"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-seperated-values,text/plain",
                           ".csv")),
      textInput("NELAT", "NE Latitude", value = "34.87245"),
      textInput("NELON", "NE Longitude", value = "37.94796"),
      textInput("SWLAT", "SW Latitude", value = "34.82852"),
      textInput("SWLON", "SW Longitude", value = "37.81224"),
      tags$hr(),
      tags$head(
        tags$style(HTML('#go{background-color:orange}'))
      ),
      actionButton("go", "Run!")
    ),
    
    mainPanel(
      leafletOutput("map")
    )
  ),

  # fileInput("file1", "Choose CSV File",
  #           multiple = FALSE,
  #           accept = c("text/csv",
  #                      "text/comma-seperated-values,text/plain",
  #                      ".csv")),
  # textInput("NELAT", "NE Latitude", value = "34.87245"),
  # textInput("NELON", "NE Longitude", value = "37.94796"),
  # textInput("SWLAT", "SW Latitude", value = "34.82852"),
  # textInput("SWLON", "SW Longitude", value = "37.81224"),
  # actionButton("go", "Go"),
  DT::dataTableOutput('data')
  # leafletOutput("map")
  
)

server <- function(input, output){
  
  # req(input$file1)
  # df <- read.csv(input$file1$datapath, row.names = NULL)
  # df$Person <- as.factor(df$Person)
  #
  
  
# this below works
  # dataset <- eventReactive(input$go,{
  #   req(input$file1)
  #   df <- read.csv(input$file1$datapath, row.names = NULL)
  #   df$Person <- as.factor(df$Person)
  #   df
  # })
  
  df <- eventReactive(input$go,{
    req(input$file1)
    df1 <- read.csv(input$file1$datapath, row.names = NULL)
    df1$Person <- as.factor(df1$Person)
    df1
  })

  # code below is going to try to do the function in get that as the dataset 
  
  dataset <- eventReactive(input$go,{
    req(input$file1)
    df <- read.csv(input$file1$datapath, row.names = NULL)

    dfMainPoly <-df
    coordinates(dfMainPoly) <- cbind(df$lon, df$lat)
    proj4string(dfMainPoly) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    # df$Person <- as.factor(df$Person)
    a <- as.numeric(input$SWLON)
    b <- as.numeric(input$NELON)
    c <- as.numeric(input$SWLAT)
    d <- as.numeric(input$NELAT)
    e <- as(raster::extent(a,b,c,d), "SpatialPolygons")
    # e <- as(raster::extent(37.81224,37.94796, 34.82852,34.87245 ), "SpatialPolygons")
    proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    
    inAoi1 <- crop(dfMainPoly, e)
    
    #going to create a DF from this
    inAoi1DF <- data.frame(lon = inAoi1$lon, lat = inAoi1$lat, Person = inAoi1$Person)
    
    #Summary count for AOI 1
    countAoi1 <- inAoi1DF %>% count(Person)
    #change column names
    countAoi1 <- rename(countAoi1, Count_in_AOI1 = n)
    
    #Summary count for main DF
    countMainDf <- df %>% count(Person)
    #Summary count for main DF
    countMainDf <- rename(countMainDf, Total_Count = n)
    
    #Merge dfs
    masterDf <- merge(countAoi1, countMainDf, by = "Person")
    
    # attempting for loop to calculate percentages
    masterDf$Percentage_Count <- ""
    masterDf$V_or_R <- ""
    
    #count of unique rows
    n <- nrow(masterDf)
    
    #do the for loop, seems to have worked.... cool
    for (i in 1:n) {
      masterDf$Percentage_Count[i] <- (masterDf$Count_in_AOI1[i]/masterDf$Total_Count[i]) *100
      if (masterDf$Percentage_Count[i] >= 80) {
        masterDf$V_or_R[i] <- "R"
      } else {
        masterDf$V_or_R[i] <- "V"
      }
    }
    masterDf
  })
  
  # This works
  # clipAOI <- eventReactive(input$go,{
  #   a <- as.numeric(input$SWLON)
  #   b <- as.numeric(input$NELON)
  #   c <- as.numeric(input$SWLAT)
  #   d <- as.numeric(input$NELAT)
  #   # e <- as(raster::extent(a,b,c,d), "SpatialPolygons")
  #   e <- as(raster::extent(37.81224,37.94796, 34.82852,34.87245 ), "SpatialPolygons")
  #   proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  #   e
  # })
  
  clipAOI <- eventReactive(input$go,{
    a <- as.numeric(input$SWLON)
    b <- as.numeric(input$NELON)
    c <- as.numeric(input$SWLAT)
    d <- as.numeric(input$NELAT)
    # e <- as(raster::extent(a,b,c,d), "SpatialPolygons")
    e <- as(raster::extent(a, b, c, d ), "SpatialPolygons")
    proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    e
  })


  # t <- eventReactive(input$go,{
  #   cat(input$NELAT, "Hope This works")
  #   a <- input$SWLON
  #   b <- input$NELON
  #   c <- input$SWLAT
  #   d <- input$SWLAT
  #   e <- as(raster::extent(a,b,c,d), "SpatialPolygons")
  #   proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  # 
  # })
  # 



  # #this is testing out the text output
  # output$value <- renderText({input$NELAT})
  # output$value <- renderText({input$NELON})

  
  # output$data <- DT::renderDataTable({
  #   datatable(dataset(), filter = "top", class = "cell-border stripe", rownames = FALSE)
  # }
  # )
  
  output$data <- DT::renderDataTable(
    datatable(dataset(), filter = "top", class = "cell-border stripe", rownames = FALSE)
  )
  
  
  # pal <- eventReactive(input$go,{
  #   req(input$file1)
  #   df1 <- read.csv(input$file1$datapath, row.names = NULL)
  #   pal <- colorNumeric(palette = "RdYlBu", domain = df1$Person)
  #   pal
  # })
  # 
  output$map <- renderLeaflet({
    leaflet() %>% flyToBounds(input$NELON, input$NELAT, input$SWLON, input$SWLAT)%>% addTiles() %>% addMarkers(data = df(), clusterOptions = markerClusterOptions(), label = df()$Person) %>% addPolygons(data = clipAOI())
  })
  
  # output$raster <- renderText({
  #   t()
  # })
}

#Create Shiny app ----
shinyApp(ui, server)


