library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Travel Score" = "travelScore",
  "Violent Crime" = "ViolentCrime",
  "Lodging Rate" = "Lodging",
  "Meal Rate" = "Meals",
  "Population" = "Population"
)

months <- c(
  "January" = "Jan",
  "February" = "Feb",
  "March" = "Mar",
  "April" = "Apr",
  "May" = "May",
  "June" = "Jun",
  "July" = "Jul",
  "August" = "Aug",
  "September" = "Sep",
  "October" = "Oct",
  "November" = "Nov",
  "December" = "Dec"
)

days <- c( "1" = "1",
           "2" = "2"
)

#travelProfile <- c(
#  "Urban/Site Seeing" = "Urban",
#  "Small City/Relaxing" = "Relaxing",
#  "Warm Places" = "Warm",
#  "Senior Leisure" = "Senior"
#)

shinyUI(navbarPage("Travel Explorer", id="nav",

  tabPanel("Interactive map",
    div(class="outer",
      
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      leafletMap("map", width="100%", height="100%",
        initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
        options=list(
          center = c(37.45, -93.85),
          zoom = 4,
          maxBounds = list(list(15.961329,-129.92981), list(52.908902,-56.80481)) # Show US only
        )
      ),
      
      absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
        top = 100, left = 300, right = "auto", bottom = "auto",
        width = 330, height = "auto",
        
        h2("Travel Explorer"),
        
        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "TravelScore"),
        selectInput("travelMonth", "Travel Month", months),
        selectInput("travelProfile", "Travel Profile", travelProfile),
        
        
       plotOutput("barplotTravelScore", height = 300)
       #plotOutput("scatterCollegeIncome", height = 250)
      ),
      
      #absolutePanel(id = "controls2", class = "modal", fixed = TRUE, draggable = TRUE,
      #              top = 60, left = "auto", right = 20, bottom = "auto",
      #              width = "330", height = "auto",
      #              
      #              h2("City Explorer"),
      #              
      #              
      #              selectInput("cityInput", "City", cityvars),
      #              dateInput("dateInput", "Travel Date", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en"),
      #              actionButton("updateButton", "Update Information"),
                    
                    
                    
      #              h3("Flight Information"),
      #              dataTableOutput("flightTable"),
                    
      #              h3("Current News"),
      #              plotOutput("cityNews", height = 200)
                    
                    #selectInput("siz1", "Size", vars, selected = "TravelScore"),
                    #selectInput("travelMonth1", "Travel Month", months),
                    #selectInput("travelProfile1", "Travel Profile", travelProfile)
                    
                    
                    
                    #plotOutput("barplotTravelScore", height = 300)
                    #plotOutput("scatterCollegeIncome", height = 250)
      #),
      
      #####
      
      #####
      
      
      tags$div(id="cite",
        'Data compiled for ', tags$em('Programming for Analytics Group Project'), ' by James, Ken, Philip, and Pooja.'
      )
    )
  ),

  ####### CityExplorer ######################
  tabPanel("City Explorer",
           fluidRow(
             column(2,
                    #  selectInput("city", "City", c("All cities"="", structure(city.abb, names=city.name), "Boston"), multiple=FALSE)
                    selectInput("cityInput", "City", cityvars, multiple=FALSE)
                    #dateInput("dateInput", "Travel Date", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en"),
                    #actionButton("updateButton", "Update Information"),
                    
             ),
             column(2,
                    dateInput("dateInput", "Travel Date", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en")
                    
                    #conditionalPanel("input.states",
                    #                   selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                    # )
             )
             
           ),
           
           hr(),
           h4("City Summary Information"),
           fluidRow(
             column(12,
                  htmlOutput("citySummary")
             )
           ),
           hr(),
           fluidRow(
             
             
             column(8,
                    #  selectInput("city", "City", c("All cities"="", structure(city.abb, names=city.name), "Boston"), multiple=FALSE)
                    #selectInput("cityInput", "City", cityvars, multiple=FALSE)
                    actionButton("updateTwitterInfo", "Get Twitter Information")
             )
           ),
           h4("Current Twitter News"),
           fluidRow(
             column(12,
                    htmlOutput("twitterInfo")
             )
             
           ),
           fluidRow(
             hr(),
             
             column(8,
                    #  selectInput("city", "City", c("All cities"="", structure(city.abb, names=city.name), "Boston"), multiple=FALSE)
                    #selectInput("cityInput", "City", cityvars, multiple=FALSE)
                    actionButton("updateCityInfo", "Get Flights Information")
             )
           ),
           
           h4("Airline Flight Information"),
           fluidRow(
              column(12,
                    dataTableOutput("flightTable")
                   )
             
           )
           
           
           #conditionalPanel("false", icon("crosshair"))
  ),
  ####### end of City Explorer #############
  
 
 
 ####### Data Explorer ######################
  tabPanel("Data Explorer",
    fluidRow(
      column(2,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(2,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      )
      
    ),
    fluidRow(
      column(1,
        numericInput("minScore", "Min score", min=0, max=100, value=0)
      ),
      column(1,
        numericInput("maxScore", "Max score", min=0, max=100, value=100)
      )
    ),
    hr(),
    dataTableOutput("citiestable")
  ),
  
  conditionalPanel("false", icon("crosshair"))
))
