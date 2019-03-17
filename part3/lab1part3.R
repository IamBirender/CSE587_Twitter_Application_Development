library(twitteR)
library(keyring)
library(lubridate)
library(glue)
library(dplyr)
library(tidyr)
library(ggmap)
library(ggplot2)
library(maptools)
library(maps)
library(usmap)

# Twitter API registration
setup_twitter_oauth(
  key_get("twitter_consumer_key", keyring = "mykeyring"),
  key_get("twitter_consumer_secret", keyring = "mykeyring"),
  key_get("twitter_access_token", keyring = "mykeyring"),
  key_get("twitter_access_secret", keyring = "mykeyring")
)

# Google API registration
register_google(key_get("GoogleAPI", keyring = "mykeyring"))

## Pre-Data Collection

# Set keywords
# flu vaccine h1n1 tamiflu influenza headache lymph sinus
keywords <- c("flu","vaccine","h1n1","tamiflu","influenza","headache","lymph","sinus")

# Set date range
# (for tweets on a single day, make startDate and endDate identical)
startDate <- "2019-03-03"
endDate <- "2019-03-03"
dateRange <- seq(as.Date(startDate), as.Date(endDate), by=1)

## Data Collection

# Collect tweets for each keyword throughout date range
for(day in as.list(dateRange)){
  for(k in keywords){
    # Check current API rate limit
    myRateLimit <- getCurRateLimitInfo(c("search"))
    
    # If limit has been reached, sleep until API reset time
    if(myRateLimit$remaining < 1){
      resetTime <- myRateLimit$reset
      nowTime <- with_tz(Sys.time(), tz="UTC")
      sleepingTime <- as.numeric(resetTime-nowTime, units="secs")
      print(glue('Sleeping for ',{sleepingTime}))
      Sys.sleep(as.numeric(resetTime-nowTime, units="secs"))
    }
  
    searchWord <- as.character(k)
    
    # Construct filename based on keyword
    filename <- glue('tw-',{searchWord},'.csv')
    
    print(glue('Attemping search of ',{searchWord}, ' since ',as.character(day),' until ',as.character(day+1)))
    
    # Searches Twitter API for provided dates given searchWord, then strips any retweets, and converts to DF
    dtweets <- twListToDF(strip_retweets(searchTwitter(searchWord, n=900, since=as.character(day), until = as.character(day+1), retryOnRateLimit = 450)))
    
    print(glue('Completed search of ',{searchWord}, ' with ',{nrow(dtweets)},' rows'))
    
    print(glue('Appending to ',{filename}))
    if(!file.exists(filename)){
      write.csv(dtweets, file=filename)
    } else {
      existingCSV <- read.csv(file = filename, stringsAsFactors = F)
      existingCSV <- select(existingCSV, -c("X"))
      newCSV <- rbind(existingCSV, dtweets)
      write.csv(newCSV, file=filename)
    }
  }
}

## Geocoding section

# IMPORTANT
# Choose either selected subset of keywords or all keywords (will take longer due to API calls)
geocodeTheseKeywords <- c('lymph')
# ALL KEYWORDS OPTION: geocodeTheseKeywords <- keywords
# Valid keywords: flu vaccine h1n1 tamiflu influenza headache lymph sinus

for(k in geocodeTheseKeywords){
  geocoded.users <- NULL
  geocoded.users.nogeneric <- NULL
  filteredCSV <- NULL
  
  # Read in raw tweets from 'tw-{k}.csv'
  geoprep.data <- read.csv(glue('tw-',{k},'.csv'))
  
  # Drop all columns except for "id", "text", and "screenName" 
  geoprep.data <- select(geoprep.data, "id","text","screenName")
  
  # Rename "id" to "tweetID" to clarify
  geoprep.data <- rename(geoprep.data, tweetID = id)
  
  # Remove any duplicate tweets based on "tweetID"
  geoprep.data <- unique(geoprep.data, by = "tweetID")
  
  # Geocoding the tweet authors
  
  ## If keyword has already been geocoded, it skips this step and reads in 'geo-users-tw-{k}.csv' 
  if(!file.exists(glue('geo-users-tw-',{k},'.csv'))){
    # Query Twitter API for data frame of user profile information
    print("Attemping twitteR lookupUsers...")
    geoprep.users <- twListToDF(lookupUsers(as.character(geoprep.data$screenName), includeNA = FALSE))
    
    # Select only columns "screenName" and "location"
    geoprep.users <- select(geoprep.users, screenName, location)
    
    # Select only users with a non-empty location listed and only keep columns 'screenName' and 'location'
    geoprep.users <- subset(geoprep.users, location != "")
    
    # Query Google Maps API to geocode user locations
    geoprep.users.locgeocoded <- geocode(geoprep.users$location)
    
    # Add latitude and longitude columns to geocoded.users
    geocoded.users.latlng <- dplyr::mutate(geoprep.users, latitude = geoprep.users.locgeocoded$lat)
    geocoded.users.latlng <- dplyr::mutate(geocoded.users.latlng, longitude = geoprep.users.locgeocoded$lon)
    
    # Save geocoded information for this keyword to 'geo-users-tw-{k}.csv'
    write.csv(geocoded.users.latlng, file = glue('geo-users-tw-',{k},'.csv'))
  } else {
    ## If keyword had already been geocoded, it skips to this and reads in 'geo-users-tw-{k}.csv' 
    print(glue('Loading geo-users-',{k},'.csv'))
    geocoded.users <- read.csv(glue('geo-users-tw-',{k},'.csv'))
    
    # Drops the extra column that gets added at the beginning
    geocoded.users <- select(geocoded.users, -c("X"))
  }
  
  # File naming clarification
  # geo-users-nogeneric-{keyword}.csv means that imprecise rows as defined belowed have been dropped
  
  if(!file.exists(glue('geo-users-nogeneric-',{k},'.csv'))){
    
    # Drop imprecise rows where geocode failed such as locations of "sitting in grass" or "somewhere"
    geocoded.users.nogeneric <- drop_na(geocoded.users.latlng)
    
    # Drop imprecise rows where geocode returned merely the geographical center of the United States
    geocoded.users.nogeneric <- geocoded.users.nogeneric[!(geocoded.users.nogeneric$latitude==37.0902400 & geocoded.users.nogeneric$longitude==-95.7128910),]
    
    write.csv(geocoded.users.nogeneric, file = glue('geo-users-nogeneric-',{k},'.csv'))
    
  } else {
    
    ## If keyword data had already been cleared of imprecise rows, it skips to this and reads in 'geo-users-nogeneric-{k}.csv' 
    print(glue('Loading geo-users-nogeneric-',{k},'.csv'))
    geocoded.users.nogeneric <- read.csv(glue('geo-users-nogeneric-',{k},'.csv'))
    
    # Drops the extra column that gets added at the beginning
    geocoded.users.nogeneric <- select(geocoded.users.nogeneric, -c("X"))
    
  }
  
  # Reverse Geocoding
  
  if(!file.exists(glue('geo-filtered-',{k},'.csv'))){
    
    # Preparation
    geocoded.state.attached <- geocoded.users.nogeneric
    
    # Create data frame for the listing of states as a result of reverse geocoding
    statesToAttach <- data.frame(state=character(), stringsAsFactors=FALSE)
    
    # Makes sure i is initialized to 1
    i <- 1
    
    # 
    for(i in seq(1, nrow(geocoded.users.nogeneric), 1)){
      
      # 
      testrevgeo <- revgeocode(c(geocoded.users.nogeneric[i,4], geocoded.users.nogeneric[i,3]), output="all")
      if(is.na(testrevgeo)){
        statesToAttach <- add_row(statesToAttach, state = NA)
      } else {
        testrevgeo.results.length <- length(testrevgeo$results)
        if(testrevgeo.results.length != 1){
          testrevgeo.results.length = testrevgeo.results.length - 1
        }
        
        #print("Now attaching state to new row")
        
        mystate <- NA
        mystate <- tryCatch(
          {
            #print("here")
            testrevgeo$results[[testrevgeo.results.length]]$address_components[[1]]$long_name
          }, error = function(e) {
            NA
            #print(glue('Encountered error reading JSON:',{e}))
          }
        )
        
        statesToAttach <- add_row(statesToAttach, state = mystate)
      }
      
    }
    
    # Bind state names to geocoded.users.nogeneric
    newCSV <- cbind(geocoded.users.nogeneric, state = statesToAttach$state)
    
    # Filter out non-US states
    filteredCSV <- subset(newCSV, state %in% state.name)
    
    write.csv(filteredCSV, file=glue('geo-filtered-',{k},'.csv'))
  } else {
    print(glue('Loading geo-filtered-',{k},'.csv'))
    filteredCSV <- read.csv(glue('geo-filtered-',{k},'.csv'))
    filteredCSV <- select(filteredCSV, -c("X"))
  } 
  uniqueGeoTweets <- merge(filteredCSV, geoprep.data, by = "screenName", all.x = TRUE)
  uniqueGeoTweets <- unique(uniqueGeoTweets, by = "tweetID")
  write.csv(uniqueGeoTweets, file=glue('geo-unique-tweets-',{k},'.csv'))
}

# geo-unique-tweets-{keyword}.csv now contains:
#   - screenName of user who wrote Tweet with keyword in it
#   - user profile's listed location
#   - latitude based on Google geocoding of user's location
#   - longitude based on Google geocoding of user's location
#   - U.S. state name based on Google reverse geocoding of user's coordinates
#   - unique Tweet ID from Twitter
#   - text of Tweet

stateTweetFullList <- NULL
for(k in keywords){
  if(is.null(stateTweetFullList)){
    stateTweetFullList <- select(read.csv(glue('geo-unique-tweets-',{k},'.csv')), -c("X"))
  } else {
    uniqueCSV <- select(read.csv(glue('geo-unique-tweets-',{k},'.csv')), -c("X"))
    
    stateTweetFullList <- rbind(stateTweetFullList, uniqueCSV)
  }
}
stateTweetUniqueList <- unique(stateTweetFullList, by = "tweetID")
totalTweetsPerState <- count(stateTweetFullList, state)

plot_usmap(regions = "states", data = totalTweetsPerState, values = "n", lines = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Flu Tweets", label = scales::comma)+
  theme(legend.position = "right")

