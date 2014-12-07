# adopted from shiny superzip example


library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(rPython)
library(data.table)

#python.load('test.py')
python.load('Flight API.py')
python.load('GroupProjTwitr.py')
set.seed(100)



shinyServer(function(input, output, session) {
  
  displayVar = "Lodging"
  ## Interactive Map ###########################################

  # Create the map
  map <- createLeafletMap(session, "map")

  # A reactive expression that returns the set of cities that are
  # in bounds right now
  citiesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(allcities[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(allcities,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  # Precalculate the breaks we'll need for the two histograms
  #centileBreaks <- hist(plot = FALSE, allcities$Population, breaks = 20)$breaks
  output$barplotTravelScore <- renderPlot({
    # If no cities are in view, don't plot
    if (nrow(citiesInBounds()) == 0)
      return(NULL)
    
    print("in output$barTravelScore")
    #print(paste0("citiesInBounds", citiesInBounds()$key))
    
    #topCities <- subset(citiesInBounds()[1:10,])
    topCities <- citiesInBounds()
    topCities <- topCities[order(-topCities$travelScore),][1:10,]
    
    # reorder for plotting
    topCities <- topCities[order(topCities$travelScore),]
    print( topCities)
   
    # xlim = range(topCities$travelScore),
    par(mai=c(1,2,1,1))
    barplot(topCities$travelScore,
         horiz = TRUE,
         main = "Top Cities (visible cities)",
         xlab = "Travel Score",
         xlim = c(0,100),
         ylab = "City",
         names.arg = c(topCities$city),
         axisnames = TRUE,
         las=1,
         col = '#00DD00',
         border = 'white')
  })
  
 
  # session$onFlushed is necessary to work around a bug in the Shiny/Leaflet
  # integration; without it, the addCircle commands arrive in the browser
  # before the map is created.
  session$onFlushed(once=TRUE, function() {
    paintObs <- observe({
      colorBy <- input$color
      sizeBy <- input$size
      selectedMonth <- input$travelMonth
      travelProfile <- input$travelProfile

      print("======================================================")
      
      print("in session$onFlushed")
      print(paste0("dummyVar=", dummyVar))
      dummyVar <- 1
      print(paste0("Selected Month is:  ", selectedMonth))
      print(paste0("allcities lodging BEFORE= ",allcities$Lodging[125] ))
      print(paste0("allcities temperature BEFORE= ",allcities$Temperature[125] ))
      
      #print(allcities$Lodging[125])
      allcities['Lodging'] <<- populateLodgingRate(allcities,selectedMonth)
      allcities['Temperature'] <<- populateTemperature(allcities,selectedMonth)
      
      print(paste0("allcities lodging AFTER=", allcities$Lodging[125]))
      print(paste0("allcities temperature AFTER=", allcities$Temperature[125]))
      
      #print(allcities$Lodging[125])
      #print( colorData)
      
      ## subset the allcities dataframe to only the columns to be used as factors in the travelscore calculation
      citiesFactorsDF <- subset(allcities, select=c("Population", "ViolentCrime", "PropertyCrime", "Lodging", "Meals", "Temperature", "TransportMiles"))
      # convert NAs to 0
      citiesFactorsDF[is.na(citiesFactorsDF)] <- 0
      
      print(paste0("allcities travelScore BEFORE=", allcities$travelScore[125]))
      tempResultDF <- calculateTravelScore(citiesFactorsDF, travelProfile)
      allcities['travelScore'] <<- tempResultDF
      
      print(paste0("tempResultDF=",tempResultDF[125,]))
      #print(tempResultDF)
      print(paste0("allcities travelScore AFTER=", allcities$travelScore[125]))
      #print(allcities$travelScore)
      
  #    colorData <- if (colorBy == "Population") {
  #      as.numeric(allcities$Population > (100 - input$threshold))
  #    } else {
  #      allcities[[colorBy]]
  #    }
      colorData <- allcities[[colorBy]]
      
      colors <- brewer.pal(11, "Spectral")[cut(colorData, 11, labels = FALSE)]
      colors <- colors[match(allcities$Population, allcities$Population)]
      
      # clear popup since information is being updated
      map$clearPopups()
      
      # Clear existing circles before drawing
      map$clearShapes()
      # Draw in batches of 1000; makes the app feel a bit more responsive
      chunksize <- 1000
      for (from in seq.int(1, nrow(allcities), chunksize)) {
        to <- min(nrow(allcities), from + chunksize)
        citieschunk <- allcities[from:to,]
        # Bug in Shiny causes this to error out when user closes browser
        # before we get here
        try(
          map$addCircle(
            citieschunk$latitude, citieschunk$longitude,
            (citieschunk[[sizeBy]] / max(allcities[[sizeBy]], na.rm = TRUE)) * 100000,
            citieschunk$key,
            list(stroke=FALSE, fill=TRUE, fillOpacity=0.4),
            list(color = colors[from:to])
          )
        )
      }
    })
    
    # TIL this is necessary in order to prevent the observer from
    # attempting to write to the websocket after the session is gone.
    session$onSessionEnded(paintObs$suspend)
  })
  
  # Show a popup at the given location
  showCityPopup <- function(cityKey, lat, lng) {
    #selectedCity <- allcities[allcities$key == cityKey,]
    selectedCity <- getCityInfo(cityKey, input$travelMonth)
    
    print("showCityPopup: ")
    print( cityKey)
    print(lat)
    print(lng)
    print(selectedCity)
    print(selectedCity$city)
    print(paste0("selectedMonth=", input$travelMonth))

    
    content <- stringCityInfo(cityKey, input$travelMonth)
    map$showPopup(lat, lng, content, cityKey)
  }

  # When map is clicked, show a popup with city info
  clickObs <- observe({
    map$clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showCityPopup(event$id, event$lat, event$lng)
    })
  })
  
  ####### update City Explorer  #########
  updateCityInfo <- observe({
    
    cityName <- input$cityInput
    inputDate <- input$dateInput
    print(paste0("in updateCityInfo: ", cityName))
    cityKey = allcities$key[allcities$city == cityName]
    print(paste0("cityKey= ", cityKey))
    print(paste0("inputDate=", inputDate))
    selectedMonth <- format(as.Date(inputDate), "%b")
    print(paste0("selectedMonth=", selectedMonth))
    
    content <- stringCityInfo(cityKey, selectedMonth)
    output$citySummary <-renderText( { content} )
    output$flightTable <- renderDataTable( { as.table(blankFlightTable) } )
    output$twitterInfo <- renderText( { "Click update button to retrieve current Twitter Information for your selected city"})
    
    #stateCode <- allcities$state[allcities$city == cityName]
    #twitterResults <- python.call('getTwitterFeeds', paste0(cityName, ",", stateCode))
    
    #print("server.R: twitterResults=")
    #print(twitterResults)
    #output$twitterInfo <- renderText( { twitterResults })
  })
  
 observe ({
    if (is.null(input$updateCityInfo))
      return()
    isolate({
      cityName <- input$cityInput
      inputDate <- input$dateInput
      print(paste0("in updateCityInfo: isolate ", cityName))
      print(paste0("in updateCityInfo: isolate ", inputDate))
      
      print("flight table=")
      print(flightTable)
      
      inputDateChar <- as.character(inputDate) 
     
      destCityAirportCode = allcities$airportCode[allcities$city == cityName]
    
      flightResults <- python.call('google_flight', 'WAS', destCityAirportCode, inputDateChar)
      #flightResults = "There is no flights available."
      Flight <- flightResults
      print("flightResults=")
      
      print(length(flightResults))
      ######################################### format flight info ######
      #if (Flight == "There is no flights available.") {
      if (length(Flight) <= 1) {
          
          print("No flights")
          Result <- "There is no flights available."
      }
      #print("flightResultsDF=")
      #print(flightResultsDF)
      #flightResultsDict = blankFlightTable
      else {
      
        Arrival_Time = c(1:(length(Flight)+1))
        Flight_Number = c(1:(length(Flight)+1))
        Arrival_Airport = c(1:(length(Flight)+1))
        Price = c(1:(length(Flight)+1))
        Departure_Airport = c(1:(length(Flight)+1))
        Number_of_Stops = c(1:(length(Flight)+1))
        Departure_Time = c(1:(length(Flight)+1))
        
        
        for (i in 1:length(Flight))
        {
          Arrival_Time[i] <- Flight[[i]]$`Arrival Time`
          flightstr = ""
          for (n in 1:length(Flight[[i]]$`Flight Number`))
          {
            flightstr <- paste(flightstr, Flight[[i]]$`Flight Number`[n])
          }
          Flight_Number[i] = flightstr
          Arrival_Airport[i] <- Flight[[i]]$`Arrival Airport`
          Price[i] <- Flight[[i]]$`Price`
          Departure_Airport[i] <- Flight[[i]]$`Departure Airport`
          Number_of_Stops[i] <- Flight[[i]]$`Number of Stops`
          Departure_Time[i] <- Flight[[i]]$`Departure Time`
        }
        
        Result <-cbind(Arrival_Time,
                       Flight_Number,
                       Arrival_Airport,
                       Price,
                       Departure_Airport,
                       Number_of_Stops,
                       Departure_Time)
      
      } # of else
      ######## end of flight formating ##########
      
      #flightResultsTable <- formatGoogleFlight(flightResults)
      print("flightResultsTable=")
      flightResultsTable <- Result
      flightResultsTable <- data.table(flightResultsTable)
      output$flightTable <- renderDataTable( {
         flightResultsTable
      })
      
    })
     
    })
    
    observe ({
      if (is.null(input$updateTwitterInfo))
        return()
      isolate({
        cityName <- input$cityInput
        inputDate <- input$dateInput
        print(paste0("in updateTwitterInfo: isolate ", cityName))
        print(paste0("in updateTwiiterInfo: isolate ", inputDate))
        
        inputDateChar <- as.character(inputDate) 
        
        
        stateCode <- allcities$state[allcities$city == cityName]
        twitterResults <- python.call('getTwitterFeeds', paste0(cityName, ",", stateCode))
        
        print("server.R: twitterResults=")
        print(twitterResults)
        output$twitterInfo <- renderText( { twitterResults })
        
      })
      
  })
  ####
  session$onSessionEnded(clickObs$suspend)

  ## Data Explorer ###########################################
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map$clearPopups()
      dist <- 0.5
      key <- input$goto$key
      lat <- input$goto$lat
      lng <- input$goto$lng
      showCityPopup(key, lat, lng)
      map$fitBounds(lat - dist, lng - dist,
                    lat + dist, lng + dist)
    })
  })
  
  output$citiestable <- renderDataTable({
    cleantable %>%
      filter(
        TravelScore >= input$minScore,
        TravelScore <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities
        
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-key="', Key, '"><i class="fa fa-crosshairs"></i></a>', sep="")) 
    })
  


  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })

})
