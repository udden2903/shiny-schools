library(XML)
library(tidyverse)
library(ggmap)
library(jsonlite)

#Load and parse XML file
schools_xml <- xmlParse("exp_slutbetyg_skola_2016.xml", encoding="UTF-8")

#Create a progress bar for use in function
pb <- progress_estimated(xmlSize(schools_xml["//skola"]))

#Define function to extract relevant values
get_school <- function(x) {
  print(pb$tick())
  namn <- unlist(xpathApply(schools_xml, "//skola", xmlAttrs)[[x]][[2]])
  betyg <- unlist(xpathApply(schools_xml, "//skola/gnm_meritvarde", xmlValue)[x])
  cbind(namn, betyg) %>% as_tibble()
}

#Apply function to all schools
school_grades <- map_df(1:xmlSize(schools_xml["//skola"]), get_school)

#Clean the data
school_grades$betyg <- as.numeric(gsub(",", ".", school_grades$betyg))

#Get coordinates for each school based on name
geocodes <- lapply(school_grades$namn, geocode)

#Bring the coordinates into a dataframe
geocodes_df <- map_df(geocodes, data.frame)

#Combine previous dataframes
schools_address <- cbind(school_grades, geocodes_df)

#Retain observations without NAs
schools_filtered <- filter(schools_address, complete.cases(schools_address))

#Use SL's travel planner API to find the estimated duration from Stockholm Central to a given school
get_trip_dur <- function(lat, long, name) {
  res <- tryCatch(jsonlite::fromJSON(sprintf("https://api.sl.se/api2/TravelplannerV2/trip.json?key=90e4b7d4883e43298ee24222669724c0&date=2017-06-22&time=08:00&originId=300109002&destCoordLat=%s&destCoordLong=%s&destCoordName=%s&searchForArrival=0&unsharp=0", 
                                    lat, long, URLencode(gsub(" ", "%20", name)))), error=function(err) NA)
  tryCatch(as.numeric(head(strsplit(res$TripList$Trip$dur, " "), n = 1)), error = function(err) NA)
}

#Set up a progress bar for use in function
pb <- progress_estimated(nrow(schools_filtered))

#Define function to get the trip duration for every school
#Suspend execution of function for 2.5 seconds with each iteration due to API restrictions
get_all_dur <- function(x) {
  Sys.sleep(2.5)
  print(pb$tick())
  get_trip_dur(schools_filtered$lat[x], schools_filtered$lon[x], schools_filtered$namn[x])
}

#Apply function to all schools
durations <- lapply(1:nrow(schools_filtered), get_all_dur)

#Create dataframe from list
durations_df <- data.frame(duration = unlist(durations))

#Combine duration data with school data
schools_complete <- cbind(schools_filtered, durations_df)

#Export feather file
write_feather(schools_complete, "/home/andreas/R/Projects/grundskolor/schools_complete.feather")

#Import feather file
schools_complete_b <- read_feather("/home/andreas/R/Projects/grundskolor/schools_complete.feather")

#Load libraries for shiny app
library(leaflet)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sliderInput("obs", "Slutbetyg arskurs 9:",
                               min = min(schools_complete_b$betyg), max = max(schools_complete_b$betyg), value = c(220, 260)),
                   sliderInput("tidSL", "Tid till Centralen",
                               min = 0, max = 200, value = c(30))),
  dashboardBody(leafletOutput('map', width="100%",height="650px"))
)

server <- function(input, output) { 
  
  filteredData <- reactive({
    schools_complete_b %>% filter(schools_complete_b$betyg >= input$obs[1], schools_complete_b$betyg <= input$obs[2],
                                    duration <= input$tidSL)
  })
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = 18.05138, lat = 59.33777, zoom = 10)
    })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      addCircleMarkers(~lon, ~lat, popup = ~as.character(namn), radius = 3, color = "navy", stroke = FALSE, fillOpacity = 0.6)
  })
}

shinyApp(ui, server)
