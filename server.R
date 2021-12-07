library(shiny)
library(data.table)
library(geosphere)

server <- function(input, output, session) {
  mydata <- fread("ships.csv")
  newData <- mydata
    output$CountryMap <- renderLeaflet({
      leaflet() %>%
      addTiles() %>%
      fitBounds(-124.7666, 49.4000, -67.0583, 25.0666)%>%
      setView(-95.91245, 37.2333, zoom = 3)
    })
    
    shipType <- unique(mydata$ship_type)
    shipType <- sort(shipType, decreasing = FALSE)
    output$shipTypeCB <- renderUI({
      selectInput("shipTypeCB",
                  label = "Ship Type",
                  choices = shipType,
                  multiple = F,
                  selected = "All")
    })
    
    output$shipNameCB <- renderUI({
      selectInput("shipNameCB",
                  label = "Ship Name",
                  choices = '')
    })
    
    observeEvent(input$shipTypeCB,{
      newData <- subset(mydata, ship_type == input$shipTypeCB)
      shipName <- unique(newData$SHIPNAME)
      shipName <- sort(shipName, decreasing = FALSE)
      updateSelectInput(session,
                        "shipNameCB",
                        choices = shipName)
    })
    
    observeEvent(input$shipNameCB,{
      if(input$shipNameCB =="")
        return()
      newData <- subset(newData, SHIPNAME == input$shipNameCB)
      newData <- newData[order(newData[,"DATETIME"], decreasing = FALSE),]
      count <- nrow(newData)
      i <- 1
      max <- 0
      dist <- 0
      whichOne <- 1
      portResultLAT <- array()
      portResultLON <- array()
      while (i < count-1){
        distArray <- distm(c(newData$LON[i], newData$LAT[i]), 
                           c(newData$LON[i+1], newData$LAT[i+1]),
                           fun = distHaversine)
        dist <- distArray[,1]
        if(dist >= max){
          max <- dist
          whichOne <- i
          portResultLAT[1] = newData$LAT[i]
          portResultLON[1] = newData$LON[i]
          portResultLAT[2] = newData$LAT[i+1]
          portResultLON[2] = newData$LON[i+1]
        }
        i <- i + 1
      }
      leafletProxy("CountryMap") %>%
        clearMarkers() %>%
        addCircleMarkers(lat = portResultLAT, lng = portResultLON,
                         radius= 5, fillOpacity = 0.5, stroke = FALSE) %>%
        setView(lat = portResultLAT[1], lng = portResultLON[1], zoom = 10)
      
      output$resultDistance <- renderText(paste("Sailed Distance: ", round(max, digits = 2)," meters"))
    })
}
