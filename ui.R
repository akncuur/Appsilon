# Define UI for dataset viewer app ---
library(shiny)
library(leaflet)

ui <- fluidPage(
  fluidRow(
    column(3),
    column(2,
           uiOutput("shipTypeCB"),
           uiOutput("shipNameCB"),
           uiOutput("resultDistance"),
    ),
    column(4,
           leafletOutput("CountryMap", width = 500, height = 500),
    ),
    column(3)
  )
)