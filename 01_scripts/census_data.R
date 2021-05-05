# resources ----
# tidycensus: 
#   - https://walker-data.com/tidycensus/articles/basic-usage.html
#   - https://jennhuck.github.io/workshops/tidycensus.html


# setup -------------------------------------------------------------------
## load packages
library(tidycensus)
library(dplyr)

## get census API key
census_key <- Sys.getenv('census_api_key')
census_api_key(census_key) # install = TRUE


# investigate variables ---------------------------------------------------
v19 <- load_variables(2019, 'acs1', cache = TRUE)
View(v19)
View(v19 %>% filter(concept == 'RACE'))


# get data ----------------------------------------------------------------
# NOTE: need to check whether to use 1, 3, or 5 year ACS - defaults to 5 year
ca_ethnicity_county <- get_acs(geography = 'county', # can also use 'state'
                               variables = c(#total_pop = 'B02001_001', 
                                             white_pop = 'B02001_002',
                                             african_am_pop = 'B02001_003'), 
                               summary_var = c(total_pop = 'B02001_001'), 
                               survey = 'acs1', # use acs1, acs3, or acs5 to get 1, 3, or 5 year acs
                               state = 'CA', 
                               year = 2019) %>% 
    # rename(total_pop = summary_est) %>% 
    {.}

## check
ca_ethnicity_county %>% 
    count(variable, 
          name = 'total',
          wt = estimate) %>% 
    mutate(pct = total / ca_ethnicity %>% 
               filter(variable == 'total_pop') %>% 
               pull(estimate) %>% 
               sum())
    