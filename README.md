# shiny-schools
A simple Shiny app that uses leaflet to display a map of schools in Stockholm County. 

This app allows the user to filter schools based on average student performance and estimated travel duration by public transport from the Stockholm Central Station. It parses an XML file to obtain information about student performance and uses a JSON API from www.trafiklab.se for travel information. It also relies on the ggmap package to obtain the coordinates of the schools. 
