library(dplyr)
library(RMySQL) # will load DBI as well

###################################################################
# Read information from the database
###################################################################
## open a connection to a MySQL database
con <- dbConnect(dbDriver("MySQL"), user = "root", password = "root", host='192.168.56.101', dbname = "groupV1")

## list the tables in the database
dbListTables(con)

set.seed(100)
allcities <- dbReadTable(con, "v_cities")
allperDiem <- dbReadTable(con, "v_perDiem")
alltemperature <- dbReadTable(con, "v_temperature")
# close database connection
dbDisconnect(con)

dummyVar = 0
###################################################################
# set up the data structures 
###################################################################
allcities$latitude <- jitter(allcities$lat)
allcities$longitude <- jitter(allcities$long)
allcities$Population <- as.numeric(allcities$Population)
allcities$ViolentCrime <- as.numeric(allcities$ViolentCrime)
allcities$PropertyCrime <- as.numeric(allcities$PropertyCrime)
allcities$Lodging <- as.numeric(allcities$Lodging)
allcities$Meals <- as.numeric(allcities$Meals)
allcities$Temperature <- as.numeric(allcities$Temperature)
allcities$TransportMiles <- as.numeric(allcities$PassengerMiles)
allcities$travelScore <- sample(100, size = nrow(allcities), replace = TRUE)
allcities$key <- seq(1,nrow(allcities))
row.names(allcities) <- seq(1,nrow(allcities))

## subset the allcities dataframe to only the columns to be used as factors in the travelscore calculation
citiesFactorsDF <- subset(allcities, select=c("Population", "ViolentCrime", "PropertyCrime", "Lodging", "Meals", "Temperature", "TransportMiles"))
# convert NAs to 0
citiesFactorsDF[is.na(citiesFactorsDF)] <- 0

## create a normalization (max) value for each score factor
colMax <- function(data) sapply(data, max, na.rm = TRUE)
maxValueFactors <- colMax(citiesFactorsDF)

### table for the Data Explorer tab panel
cleantable <- allcities %>%
  select(
    Key = key,
    City = city,
    State = state,
    Lat = latitude,
    Long = longitude,
    Population = Population,
    Lodging = Lodging,
    Meals = Meals,
    Temperature = Temperature,
    TransportMiles = PassengerMiles,
    TravelScore = travelScore
  )

travelProfile <- c(
  "Urban/Site Seeing" = "Urban",
  "Small City/Relaxing" = "Relaxing",
  "Warm Places" = "Warm",
  "Senior Leisure" = "Senior",
  "Budget/Student" = "Budget"
)

#cityvars <- c(paste0(allcities$city, '" = "', allcities$key, ','))
cityvars <- sort(allcities$city)

flightTable <- cbind(c("Airline", "AA", "UA"), c("Flight#", 100,99), c("Price", 500,199))
blankFlightTable  <- cbind(c("Airline","Click update button to retrieve flights for your seleced city"), c("Flight#","-"))

###################################################################
# travelScoreWeights - weighting factor for each type data types
#  Population, Violent Crime, Property Crime,  
###################################################################
#travelScoreDefaultWeightsUrban = c(0,0,0,0,   0.1,0.1,0.1,0.2,0.2,   0,0,0,0)
#travelScoreDefaultWeightsRelaxing = c(0,0,0,0,   0.1,0.1,0.1,0.2,0.2,   0,0,0,0)
#travelScoreDefaultWeightsWarm = c(0,0,0,0,   0.1,0.1,0.1,0.2,0.2,   0,0,0,0)
#travelScoreDefaultWeightsSenior = c(0,0,0,0,   0.1,0.4,0.4,0.1,0.1,   0,0,0,0)
#travelScoreDefaultWeightsBudget = c(0,0,0,0,   0.0,0.1,0.1,0.4,0.4,   0,0,0,0)

travelScoreDefaultWeightsUrban = c( 0.1,0.1,0.1,0.2,0.2,1,1 )
travelScoreDefaultWeightsRelaxing = c(  0.0,0.1,0.5,100,0.2,1,1  )
travelScoreDefaultWeightsWarm = c(  0.1,0.1,0.5,0.2,0.1,1,1  )
travelScoreDefaultWeightsSenior = c(  0.1,0.4,0.4,0.1,0.1,1,1 )
travelScoreDefaultWeightsBudget = c(  0.0,0.1,0.1,0.4,0.4,1,1 )
## create a data frame with all the weights
travelScoreDF <- data.frame(matrix(0, nrow=length(travelProfile), ncol=(length(colnames(citiesFactorsDF)))), stringsAsFactors = FALSE)
colnames(travelScoreDF) <- c(colnames(citiesFactorsDF))
rownames(travelScoreDF)  <- travelProfile
travelScoreDF['Urban',] <- travelScoreDefaultWeightsUrban
travelScoreDF['Relaxing',] <- travelScoreDefaultWeightsRelaxing
travelScoreDF['Warm',] <- travelScoreDefaultWeightsWarm
travelScoreDF['Senior',] <- travelScoreDefaultWeightsSenior
travelScoreDF['Budget',] <- travelScoreDefaultWeightsBudget

print("travelScoreDF=")
travelScoreDF



###################################################################
# functions to calculate values when date is changed
###################################################################
populateLodgingRate <- function( cityDF, month) {
  print(paste0("in populateLodging Rate month=", month))
  #print(month)
  cityDF$Lodging <- allperDiem[month]
  
  return(cityDF$Lodging)
}

populateTemperature <- function( cityDF, month) {
  print(paste0("in populateTemperature month=", month))
  #print(month)
  cityDF$Temperature <- alltemperature[month]
  
  return(cityDF$Temperature)
}

###################################################################
# functions to calculate new travel score when input is changed
###################################################################
calculateTravelScore <- function(cityFactorDF, travelProfile)  {
  print(paste0("in function: calculateTravelScore:  travelProfile=", travelProfile))
  
  #print("cityFactorDF=")
  #print(cityFactorDF)
  #print("travelScoreDF[travelProfile,]=")
  #print(travelScoreDF[travelProfile,])
  tempVec <- as.numeric(travelScoreDF[travelProfile,]*100)
  
  # normalize the factors
  for (i in names(cityFactorDF)) {
    cityFactorDF[[i]] <- cityFactorDF[[i]]/maxValueFactors[[i]]
  } 
  
  #cityFactorDF$Meals <- cityFactorDF$Meals/maxValueFactors['Meals']
  #print(cityFactorDF)
  
  tscore <- as.data.frame(as.matrix(cityFactorDF) %*% cbind(tempVec) )
  
  #tscore <- ddply(cityFactorDF, .(id), function(x)as.data.frame(as.matrix(x[, -1]) %*% tempVec), .inform=TRUE)
  
  print(paste0("tscore= ", tscore[125,]))
  #print(tscore[125,])
  #tscore$travelScore <- tscore$travelScore
  return(tscore)
}

###################################################################
# functions to return city data from allcities as an object for the one city input
###################################################################
#getCityInfo <- function(cityKey)  {
#  print("in function getCityInfo:")
#  print(allcities[allcities$key == cityKey,])
#  return(allcities[allcities$key == cityKey,])
#}

getCityInfo <- function(cityKey, selectedMonth)  {
  print("in function getCityInfo:")
  print(paste0("selectedMonth=", selectedMonth))
  print(allcities[allcities$key == cityKey,])
  #cityDF <- allcities[allcities$key ==cityKey,]
  allcities['Lodging'] <<- populateLodgingRate(allcities,selectedMonth)
  allcities['Temperature'] <<- populateTemperature(allcities,selectedMonth)
  return(allcities[allcities$key == cityKey,])
}


###################################################################
# functions to return city information formated as string for printing
###################################################################
stringCityInfo <- function(cityKey, selectedMonth) {
  
  selectedCity <- getCityInfo(cityKey, selectedMonth)
  
  print("in stringCityInfo: ")
  print( cityKey)
  #print(lat)
  #print(lng)
  #print(selectedCity)
  print(selectedCity$city)
  content <- as.character(tagList(
    tags$h4("Travel Score:", as.integer(selectedCity$travelScore)),
    tags$strong(HTML(sprintf("%s, %s ",
                             selectedCity$city, selectedCity$state
    ))), tags$br(),
    sprintf("Lodging Cost: %s", dollar(selectedCity$Lodging)), tags$br(),
    sprintf("Meal Cost: %s", dollar(selectedCity$Meals)), tags$br(),
    sprintf("Population: %s", format(selectedCity$Population, big.mark=",")), tags$br(),
    sprintf("Property Crime Rate: %s", selectedCity$PropertyCrime), tags$br(),
    sprintf("Violent Crime Rate: %s", selectedCity$ViolentCrime), tags$br(),
    sprintf("Average Temperature: %s", selectedCity$Temperature), tags$br(),
    sprintf("Public Transport Miles: %s", format(selectedCity$TransportMiles, big.mark=","))
  ))
  
  return(content)
}



formatGoogleFlight <- function(Flight) {
  Arrival_Time = c(1:length(Flight))
  Flight_Number = c(1:length(Flight))
  Arrival_Airport = c(1:length(Flight))
  Price = c(1:length(Flight))
  Departure_Airport = c(1:length(Flight))
  Number_of_Stops = c(1:length(Flight))
  Departure_Time = c(1:length(Flight))
  
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
  
  return(Result)
}

