# resources ----
# tidycensus: 
#   - https://walker-data.com/tidycensus/articles/basic-usage.html


# setup -------------------------------------------------------------------
## load packages
library(tidycensus)
library(dplyr)

## get census API key
census_key <- Sys.getenv('census_api_key')
# census_api_key(census_key) # install = TRUE


# investigate variables ---------------------------------------------------
v19 <- load_variables(2019, "acs1", cache = TRUE)
View(v19)

View(v19 %>% filter(concept == 'RACE'))


# get data ----------------------------------------------------------------
# NOTE this gets data from 5 year ACS - may want to try to change to 3 or 1 year
ca_ethnicity_county <- get_acs(geography = "county", # state
                               variables = c(total_pop = 'B02001_001', 
                                             white_pop = 'B02001_002',
                                             african_am_pop = 'B02001_003'), 
                               state = "CA", 
                               year = 2019)
    # check
    ca_ethnicity_county %>% 
        count(variable, 
              name = 'total',
              wt = estimate) %>% 
        mutate(pct = total / ca_ethnicity %>% 
                   filter(variable == 'total_pop') %>% 
                   pull(estimate) %>% 
                   sum())
    