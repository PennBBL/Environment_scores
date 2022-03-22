# Read in the data and packages
library(tidycensus)
library(tidygeocoder)
library(tidyverse)
library(tigris)
library(parallel)

address <- read_delim("~/Projects/Geocoding/Data/address_dump_oracle_Oct2021.xls", 
                      delim = "\t", escape_double = FALSE, 
                      col_types = cols(...11 = col_skip()), 
                      trim_ws = TRUE) 
demo <- read_csv("~/Projects/Geocoding/Data/address_dump_oracle_demographics.csv")

# call_geolocator_laton comes from the tidygeocoder package, had to make changes to the url so it ran properly
call_geolocator_latlon <- function(lat, lon, benchmark, vintage) {
  if(missing(benchmark)) {
    benchmark<-"Public_AR_Current"
  } else {
    benchmark<-benchmark
  }
  if(missing(vintage)) {
    vintage<-"Current_Current"
  } else {
    vintage<-vintage
  }
  # Build url
  call_start <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates?"
  
  url <- paste0("x=", lon,"&y=", lat)
  
  benchmark0 <- paste0("&benchmark=", benchmark)
  vintage0 <- paste0("&vintage=", vintage, "&format=json")
  
  url_full <- paste0(call_start, url, benchmark0, vintage0)
  #print(url_full)
  # Check response
  r <- httr::GET(url_full)
  httr::stop_for_status(r)
  response <- httr::content(r)
  if (length(response$result$geographies$`Census Blocks`[[1]]$GEOID) == 0) {
    message(paste0("Lat/lon (", lat, ", ", lon,
                   ") returned no geocodes. An NA was returned."))
    return(NA_character_)
  } else {
    
    #regex search for block group geography in response
    response_block<-grep(response[["result"]][["geographies"]], pattern = ".Block.")
    
    #check If a block group result is found or return NA
    #If block group response is found check GEOID length and return either NA for missing data or the value
    if(length(response_block) == 0){
      return(NA_character_)
    } else {
      if (length(response[["result"]][["geographies"]][[response_block]][[1]]$GEOID) == 0) {
        message(paste0("Lat/lon (", lat, ", ", lon,
                       ") returned no geocodes. An NA was returned."))
        return(NA_character_)
      } else {
        if (length(response[["result"]][["geographies"]][[response_block]][[1]]$GEOID) > 1) {
          message(paste0("Lat/lon (", lat, ", ", lon,
                         ") returned more than geocode. The first match was returned."))
        }
        return(response[["result"]][["geographies"]][[response_block]][[1]]$GEOID)
      }
    }
    
  }
}

# Function which takes an address data frame and outputs a data frame with the latitude and longitude coordinates from the given address column

runGeocodeArcGIS <- function(df,address_col){
  newdf <- geo(address = as.character(df[,address_col]),method = "arcgis")
  newdf$year <- str_replace_all(df$DOVISIT,pattern = "-.*",replacement = "")
  return(newdf)
}

# Takes a data frame with latitude/longitude coordinates and attaches information about the Census Block from the 2010 Census

getCensusBlocks <- function(df){
  CensusBlock <- call_geolocator_latlon(lat = df$lat, lon = df$long,vintage = "Census2010_Current")
  newdf <- data.frame("address" = df$address,lat = df$lat,long = df$long,CensusBlock = CensusBlock,year = df$year)
  return(newdf)
}

# Takes the state, country, tract, and (partial) block numbers for an address and links the block to variables from the ACS survey 
getCensusVars <- function(df){
  geoID <- paste0(df$state,df$county,df$tract,str_sub(df$block,start = 1,end = 1))
  Census <- get_acs(geography = "block group",state = df$state,county = df$county,variables = NewVars,year = min(max(as.numeric(df$year) + 2,2013),2019))
  Census.long <- Census %>% 
    filter(GEOID == geoID) %>% 
    select(variable,estimate) %>% 
    pivot_wider(names_from = variable,values_from = estimate)
  newdf <- cbind(df,Census.long) %>% 
    select(-`row_number()`)
  return(newdf)
}

address2 <- address %>% 
  mutate(across(c(PERM_ADDRESS,LOCAL_ADDRESS),~str_remove_all(.x,pattern = ","))) %>% # Remove commas from address
  mutate(across(c(PERM_ADDRESS,LOCAL_ADDRESS), ~ str_trim(.x,side = "both"))) %>% #Trim whitespace in addresses
  unite("PERM_address_full",PERM_ADDRESS:PERM_ZIP,sep = ", ",remove = FALSE) %>% # Create column for one line permanent address
  unite("Local_address_full",LOCAL_ADDRESS:LOCAL_ZIP,sep = ", ",remove = FALSE) # Same thing for local addresses

# Split address2 data frame into list 
address.all.list <- address2 %>% 
  group_split(row_number()) 

numCores <- detectCores()

# Link addresses to lat/lon coordinates
address_arcGIS <- parallel::mclapply(address.all.list,FUN = runGeocodeArcGIS,address_col = "PERM_address_full",mc.cores = numCores)
address_arcGIS <- bind_rows(address_arcGIS)

# Remove rows which don't contain a full address 
address_arcGIS_clean <- address_arcGIS %>% 
  filter(!is.na(lat) & str_detect(address,pattern = "NA|N/A",negate = TRUE) & str_detect(address,pattern = "\\.,",negate = TRUE))

#Split data frame into list
address_arcGIS_clean_l <- address_arcGIS_clean %>% 
  group_split(row_number())

# Attach Census block info for all rows in address_arcGIS_clean by applying getCensusBlocks to the list address_arcGIS_clean_l 
CensusBlock <- parallel::mclapply(address_arcGIS_clean_l,FUN = getCensusBlocks,mc.cores = numCores)
CensusBlock <- bind_rows(CensusBlock)

# Remove rows where a Census Block was not found
CensusBlock.trim <- CensusBlock %>% 
  filter(!is.na(CensusBlock))

# Parse the CensusBlock variable to determine state, county, tract, and block of addresses
CensusBlock.trim <- CensusBlock.trim %>% 
  mutate(state = str_sub(CensusBlock,start = 1,end = 2)) %>% 
  mutate(county = str_sub(CensusBlock,start = 3,end = 5)) %>% 
  mutate(tract = str_sub(CensusBlock,start = 6,end = 11)) %>% 
  mutate(block = str_sub(CensusBlock,start = 12,end = 15)) %>% 
  distinct(.keep_all = TRUE)

# Combine Census Block information with address2 data frame; turn into list   
address.geocoded.l <- address2 %>% 
  mutate(year = str_extract(DOVISIT,pattern = "[[:digit:]][[:digit:]][[:digit:]][[:digit:]]")) %>% 
  left_join(CensusBlock.trim,by = c("PERM_ADDRESS_FULL" = "address","year")) %>% 
  filter(!is.na(CensusBlock)) %>% 
  group_split(row_number())

### Variable names have changed, need to create data set mapping old names to new names 

OldVars <- c("B01001e1","B01001e26","B02001e1","B02001e2","B02001e3","B02001e4","B02001e5","B02001e6","B09002e1","B11001e1","B12001e1","B15002e1","B16004e1","B17017e1","B19113e1","B21001e1","B23025e1","C17002e1","C24010e1","B11001e9","B15002e18","B15002e35","B15002e15","B15002e32","B17017e2","B15002e11","B15002e28","B15002e1","B15002e14","B15002e13","B15002e15","B15002e16","B15002e17","B15002e18","B15002e30","B15002e31","B15002e32","B15002e33","B15002e34","B15002e35","B12001e4","B12001e13","B12001e1","B01002e1")

NewVars <- ifelse(str_detect(OldVars,pattern = "e([[:digit:]])$"),str_replace_all(OldVars,pattern = "e([[:digit:]])$",replacement = "_00\\1"),str_replace_all(OldVars,pattern = "e([[:digit:]][[:digit:]])$","_0\\1"))
 
Var.map <- data.frame("Old.var" = OldVars,"New.Var" = NewVars)

write.csv(Var.map,file = "~/Projects/Geocoding/Data/CensusVariableMap.csv")

# Attach necessary Census data to the original addresses; write data set to file
address_Census <- parallel::mclapply(address.geocoded.l,FUN = getCensusVars)
address_Census <- bind_rows(address_Census)

write.csv(address_Census,file = "~/Projects/Geocoding/Data/PermAddressDumpCensus.csv")

### Same thing as above, except using the current address instead of permanent address

address_arcGIS_current <-  parallel::mclapply(address.all.list,FUN = runGeocodeArcGIS,address_col = "Local_address_full",mc.cores = (numCores-2))
address_arcGIS_current <- bind_rows(address_arcGIS_current)

address_arcGIS_clean_current <- address_arcGIS_current %>% 
  filter(!is.na(lat) & str_detect(address,pattern = "NA|N/A",negate = TRUE) & str_detect(address,pattern = "\\.,",negate = TRUE))

address_arcGIS_clean_l_current <- address_arcGIS_clean_current %>% 
  group_split(row_number())

CensusBlock_current <- parallel::mclapply(address_arcGIS_clean_l_current,FUN = getCensusBlocks,mc.cores = numCores)
CensusBlock_current <- bind_rows(CensusBlock_current)

CensusBlock.trim.current <- CensusBlock_current %>% 
  filter(!is.na(CensusBlock))

CensusBlock.trim.current <- CensusBlock.trim.current %>% 
  mutate(state = str_sub(CensusBlock,start = 1,end = 2)) %>% 
  mutate(county = str_sub(CensusBlock,start = 3,end = 5)) %>% 
  mutate(tract = str_sub(CensusBlock,start = 6,end = 11)) %>% 
  mutate(block = str_sub(CensusBlock,start = 12,end = 15)) %>% 
  distinct(.keep_all = TRUE)

address.geocoded.l.current <- address2 %>% 
  mutate(year = str_replace_all(DOVISIT,pattern = "-.*","")) %>% 
  left_join(CensusBlock.trim.current,by = c("Local_address_full" = "address","year")) %>% 
  filter(!is.na(CensusBlock)) %>% 
  group_split(row_number())

address_Census_current <- parallel::mclapply(address.geocoded.l.current,FUN = getCensusVars)
address_Census_current <- bind_rows(address_Census_current)

write.csv(address_Census_current,file = "~/Projects/Geocoding/Data/LocalAddressDumpCensus.csv")


         