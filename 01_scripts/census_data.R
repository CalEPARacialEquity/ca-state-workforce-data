# resources ----
# tidycensus: 
#   - https://walker-data.com/tidycensus/articles/basic-usage.html


# setup -------------------------------------------------------------------

# load packages
library(tidycensus)
library(dplyr)

# get census API key
# census_key <- Sys.getenv('census_api_key')
census_api_key(census_key, install = TRUE)


# investigate variables ---------------------------------------------------
v19 <- load_variables(2019, "acs1", cache = TRUE)
View(v19)

View(v19 %>% filter(concept == 'RACE'))


# get data ----------------------------------------------------------------
# NOTE this gets data from 5 year ACS - may want to try to change to 3 or 1 year
ca_total <- get_acs(geography = "county", 
                    variables = c(total = "B02001_001"), 
                    state = "CA", 
                    year = 2019)
    # check
    sum(ca_total$estimate) / 10^6 # total CA population (million)
    
ca_white <- get_acs(geography = "county", 
                    variables = c(total = "B02001_002"), 
                    state = "CA", 
                    year = 2019)
    # check
    sum(ca_white$estimate) / 10^6 # total white population (million)
    sum(ca_white$estimate) / sum(ca_total$estimate) # white % of total population

ca_afr_am <- get_acs(geography = "county", 
                     variables = c(total = "B02001_003"), 
                     state = "CA", 
                     year = 2019)
    # check
    sum(ca_afr_am$estimate) / 10^6 # total african american population (million)
    sum(ca_afr_am$estimate) / sum(ca_total$estimate) # african american % of total population

    
    
    