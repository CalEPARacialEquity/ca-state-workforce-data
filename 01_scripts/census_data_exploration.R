# resources ----
# tidycensus: 
#   - https://walker-data.com/tidycensus/articles/basic-usage.html
#   - https://jennhuck.github.io/workshops/tidycensus.html
#   - https://censusreporter.org/data/table/?table=B02001&geo_ids=04000US06
#   - https://censusreporter.org/data/table/?table=B03002&geo_ids=04000US06
#   - https://censusreporter.org/topics/race-hispanic/
#   - https://censusreporter.org/topics/
#   - https://www.ppic.org/publication/californias-population/ # validation of the numbers


# setup -------------------------------------------------------------------
## load packages
library(tidycensus)
library(dplyr)
library(sf)
library(stringr)

## get census API key
# census_key <- Sys.getenv('census_api_key')
# census_api_key(census_key) # install = TRUE


# investigate variables ---------------------------------------------------
v19 <- load_variables(2019, 'acs1', cache = TRUE)
#
# NOTE: 
#   for data including hispanic info, filter for: label = hispanic, concept = race
#   for data not including hispanic info, just filter for: concept = race 

View(v19)
View(v19 %>% filter(concept == 'RACE'))

View(v19 %>% filter(str_detect(tolower(concept),'race'), 
                    str_detect(tolower(label), 'hispanic')))


    


# get data ----------------------------------------------------------------
# NOTE: option to use 1, 3, or 5 year ACS
#   - 3 yr ACS only available from 2005-2007 through 2011-2013 from tidycensus
#   - 1 yr ACS only available for geographies with population >65,000
#   Therefore, can use statewide 1 yr ACS (e.g. for 2019), but for data on a 
#   finer (geographic) scale need to use the 5 yr ACS (e.g. 2015-2019)

## statewide ----
### 1 yr ACS ----
ca_state_1yr <- get_acs(geography = 'state', 
                        variables = c(total_pop = 'B02001_001', 
                                      white_pop = 'B02001_002',
                                      african_am_pop = 'B02001_003',
                                      native_am_pop = 'B02001_004',
                                      asian_pop = 'B02001_005',
                                      hawaiian_pacific_pop = 'B02001_006',
                                      other_pop = 'B02001_007',
                                      two_more_pop = 'B02001_008',
                                      ), 
                        summary_var = c(total_pop = 'B02001_001'), 
                        survey = 'acs1', # use acs1, acs3, or acs5 to get 1, 3, or 5 year acs
                        state = 'CA', 
                        geometry = FALSE,
                        year = 2019) %>% 
    # rename(total_pop = summary_est) %>% 
    mutate(pop_pct = estimate / summary_est) %>% 
    {.}

sum(ca_state_1yr$estimate[2:8])
ca_state_1yr$estimate[1]


ca_state_1yr$estimate[8]
sum(ca_state_1yr$estimate[9:10])

ca_state_1yr_hisp <- get_acs(geography = 'state', 
                            variables = c(total_pop = 'B02001_001',
                                          total_hispanic = 'B03002_012',
                                          total_not_hispanic = 'B03002_002',
                                          not_hispanic_white = 'B03002_003',
                                          not_hispanic_afr_am = 'B03002_004',
                                          not_hispanic_nat_am = 'B03002_005',
                                          not_hispanic_asian = 'B03002_006',
                                          not_hispanic_haw_pac = 'B03002_007',
                                          not_hispanic_other = 'B03002_008',
                                          not_hispanic_two_more = 'B03002_009',
                                          hispanic_white = 'B03002_013',
                                          total_white = 'B02001_002',
                                          hispanic_afr_am = 'B03002_014',
                                          total_afr_am = 'B02001_003'),
                            summary_var = c(total_pop = 'B02001_001'), 
                        survey = 'acs1', # use acs1, acs3, or acs5 to get 1, 3, or 5 year acs
                        state = 'CA', 
                        geometry = FALSE,
                        year = 2019)

# check - white (should be zero -- total white == not hispanic white + hispanic white)
ca_state_1yr_hisp %>% filter(variable =='total_white') %>% pull(estimate) -
    ca_state_1yr_hisp %>% filter(variable =='not_hispanic_white') %>% pull(estimate) -
    ca_state_1yr_hisp %>% filter(variable =='hispanic_white') %>% pull(estimate)

# check - african american (should be zero -- total white == not hispanic white + hispanic white)
ca_state_1yr_hisp %>% filter(variable =='total_afr_am') %>% pull(estimate) -
    ca_state_1yr_hisp %>% filter(variable =='not_hispanic_afr_am') %>% pull(estimate) -
    ca_state_1yr_hisp %>% filter(variable =='hispanic_afr_am') %>% pull(estimate)


# these are equal
# all of the individual 'not hispanic' groups
ca_state_1yr_hisp %>% filter(str_detect(variable, '^not_hispanic')) %>% pull(estimate) %>% sum()
# total not hispanic 
ca_state_1yr_hisp %>% filter(variable =='total_not_hispanic') %>% pull(estimate)

# total hispanic + (all individual non-hispanic groups) == total state pop?
## state
ca_state_1yr_hisp %>% filter(variable =='total_pop') %>% pull(estimate)
## groups
ca_state_1yr_hisp %>% filter(variable =='total_hispanic') %>% pull(estimate) + 
    ca_state_1yr_hisp %>% filter(str_detect(variable, '^not_hispanic')) %>% pull(estimate) %>% sum()

#### FINAL TABLE ----
final_ca_state_1yr_hisp <- get_acs(geography = 'state', 
                            variables = c(# total_pop = 'B02001_001',
                                          total_hispanic = 'B03002_012', # Hispanic or Latino
                                          # total_not_hispanic = 'B03002_002',
                                          not_hispanic_white = 'B03002_003', # White
                                          not_hispanic_afr_am = 'B03002_004', # Black or African American
                                          not_hispanic_nat_am = 'B03002_005', # Native American
                                          not_hispanic_asian = 'B03002_006', # Asian
                                          not_hispanic_haw_pac = 'B03002_007', # Pacific Islander
                                          not_hispanic_other = 'B03002_008', # Other or Multiple
                                          not_hispanic_two_more = 'B03002_009'# Other or Multiple
                                          # hispanic_white = 'B03002_013',
                                          # total_white = 'B02001_002',
                                          # hispanic_afr_am = 'B03002_014',
                                          # total_afr_am = 'B02001_003'
                                          ),
                            summary_var = c(total_pop = 'B02001_001'), 
                        survey = 'acs1', # use acs1, acs3, or acs5 to get 1, 3, or 5 year acs
                        state = 'CA', 
                        geometry = FALSE,
                        year = 2019) %>% 
    mutate(pop_pct = estimate / summary_est) %>% 
    rename(total_pop = summary_est, 
           total_pop_moe = summary_moe)
# check
sum(final_ca_state_1yr_hisp$estimate) == final_ca_state_1yr_hisp$total_pop[1]
sum(final_ca_state_1yr_hisp$pop_pct) == 1

# group the 'not_hispanic_other' and 'not_hispanic_two_more' to 'Other or Multiple'
final_ca_state_1yr_hisp_2 <- final_ca_state_1yr_hisp %>% 
    mutate(variable = case_when(variable == 'not_hispanic_other' | 
                  variable == 'not_hispanic_two_more' ~ 
                  'not_hispanic_other_multiple',
              TRUE ~ variable)) %>% 
    # select(-moe, - total_pop_moe, -pop_pct) %>% 
    group_by(GEOID, NAME, variable, total_pop) %>% 
    # mutate(estimate = sum(estimate)) %>% 
    summarize(estimate = sum(estimate)) %>%
    ungroup() %>% 
    # distinct() %>% 
    mutate(pop_pct = estimate / total_pop)
    

# check
sum(final_ca_state_1yr_hisp_2$estimate) == final_ca_state_1yr_hisp_2$total_pop[1]


### 5 yr ACS ----
ca_state_5yr <- get_acs(geography = 'state', #
                                  variables = c(#total_pop = 'B02001_001', 
                                      white_pop = 'B02001_002',
                                      african_am_pop = 'B02001_003'), 
                                  summary_var = c(total_pop = 'B02001_001'), 
                                  survey = 'acs5', # use acs1, acs3, or acs5 to get 1, 3, or 5 year acs
                                  state = 'CA', 
                                  year = 2019) %>% 
    # rename(total_pop = summary_est) %>% 
    mutate(pop_pct = estimate / summary_est) %>% 
    {.}

## by county ----
### 1 yr ACS ----
# NOTE: THIS DOESN'T RETURN DATA FOR ALL COUNTIES
ca_county_1yr <- get_acs(geography = 'county', # can also use 'state'
                                   variables = c(#total_pop = 'B02001_001', 
                                       white_pop = 'B02001_002',
                                       african_am_pop = 'B02001_003'), 
                                   summary_var = c(total_pop = 'B02001_001'), 
                                   survey = 'acs1', # use acs1, acs3, or acs5 to get 1, 3, or 5 year acs
                                   state = 'CA', 
                                   year = 2019) %>% 
    # rename(total_pop = summary_est) %>% 
    {.}

### 5 yr ACS ----
ca_county_5yr <- get_acs(geography = 'county', # can also use 'state'
                                   variables = c(#total_pop = 'B02001_001', 
                                       white_pop = 'B02001_002',
                                       african_am_pop = 'B02001_003'), 
                                   summary_var = c(total_pop = 'B02001_001'), 
                                   survey = 'acs5', # use acs1, acs3, or acs5 to get 1, 3, or 5 year acs
                                   state = 'CA', 
                                   geometry = TRUE,
                                   year = 2019) %>% 
    # rename(total_pop = summary_est) %>% 
    {.}
test <- ca_county_5yr %>% filter(variable == 'african_am_pop')
plot(test['estimate'])

## check
ca_county %>% 
    count(variable, 
          name = 'total',
          wt = estimate) %>% 
    mutate(pct = total / ca %>% 
               filter(variable == 'total_pop') %>% 
               pull(estimate) %>% 
               sum())

## by census tract ----
### 5 yr ACS ----
ca_tract_5yr <- get_acs(geography = 'tract', # can also use 'state'
                                   variables = c(#total_pop = 'B02001_001', 
                                       white_pop = 'B02001_002',
                                       african_am_pop = 'B02001_003'), 
                                   summary_var = c(total_pop = 'B02001_001'), 
                                   survey = 'acs5', # use acs1, acs3, or acs5 to get 1, 3, or 5 year acs
                                   state = 'CA', 
                                   year = 2019) %>% 
    # rename(total_pop = summary_est) %>% 
    {.}

## by census block group ----
### 5 yr ACS w/ geom ----
ca_block_group_5yr <- get_acs(geography = 'block group', # can also use 'state'
                                        variables = c(#total_pop = 'B02001_001', 
                                            white_pop = 'B02001_002',
                                            african_am_pop = 'B02001_003'), 
                                        summary_var = c(total_pop = 'B02001_001'), 
                                        survey = 'acs5', # use acs1, acs3, or acs5 to get 1, 3, or 5 year acs
                                        state = 'CA', 
                                        geometry = TRUE,
                                        year = 2019) %>% 
    # rename(total_pop = summary_est) %>% 
    {.}

### save to geopackage file ----
st_write(ca_block_group_5yr, 'block_group_5yr.gpkg')
    