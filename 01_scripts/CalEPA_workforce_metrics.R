#hello testing git
# resources ----
# census data: 
#   - tidycensus R package: https://walker-data.com/tidycensus/articles/basic-usage.html  
#   - general census workshop w/ R: https://jennhuck.github.io/workshops/tidycensus.html
#   - ACS Table B02001 data (race w/o hispanic/latino data): https://censusreporter.org/data/table/?table=B02001&geo_ids=04000US06
#   - ACS Table B03002 data (race with hispanic/latino data): https://censusreporter.org/data/table/?table=B03002&geo_ids=04000US06
#   - info about race in census data: https://censusreporter.org/topics/race-hispanic/
#   - other general info about concepts and tables in census data: https://censusreporter.org/topics/
#   - check/validation of the numbers for CA: https://www.ppic.org/publication/californias-population/ 
# CA workforce data:
#   - CalHR data page: https://www.calhr.ca.gov/pages/statewide-reports.aspx
#   - workforce data on data portal: https://data.ca.gov/dataset/calhr-civil-rights-data-for-gare-capital-cohort-2019


# load packages ----
library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(here)
library(purrr)
library(glue)
library(lubridate)
library(ggplot2)
library(forcats)
library(scales)
library(tidycensus)
library(sf)
library(stringr)


# 1 - load / transform data ---------------------------------------------------------------

## 5112 ----
# pull data from data.ca.gov, this may take a few minutes
df_5112_2020 <- read_csv('https://data.ca.gov/dataset/e620a64f-6b86-4ce0-ab4b-03d06674287b/resource/d308f328-6b5b-41c1-8bc2-a4afcfcee3d1/download/5112-inline-report_12.31.20.csv') %>% 
    clean_names()

# # Add year column to dataset
# df_5112_2020 <- df_5112_2020 %>% 
#     mutate(as_of_date = as.Date("2020-12-31"))

# add additional levels for ethnicity grouping
#   ethnicity (in original dataset) is most detailed level
#   level 1 groups all BIPOC together (resulting groups are BIPOC and white)
#   level 2 groups some ethnicity sub-groupings together (Asian group and Pacific Islander group)
df_5112_2020 <- df_5112_2020 %>% 
    mutate(
        ethnicity_level1 = case_when(
            ethnicity == 'White' ~ 'White', 
            TRUE ~ 'BIPOC'
        ),
        ethnicity_level2 = case_when(
            str_detect(ethnicity, 'Pacific Islander') ~ 'Pacific Islander', # groups (all start with "Pacific Islander - "): Guamanian, Hawaiian, Other or Multiple, and Samoan
            str_detect(ethnicity, 'Asian') ~ 'Asian', # groups (all start with "Asian - "): Cambodian, Chinese, Filipino, Indian, Japanese, Korean, Laotian, Other or Multiple, Vietnamese
            TRUE ~ ethnicity)
    )

# Filtering for EPA and related BDOs 
df_5112_2020_epa <- df_5112_2020 %>% 
    filter(department == "Air Resources Board"|
               department == "Environmental Health Hazard Assessment, Office of"|
               department == "Environmental Protection Agency"|
               department == "Pesticide Regulation, Department of"|
               department == "Resources Recycling and Recovery, Department of"|
               department == "Toxic Substances Control, Department of"|
               department == "Water Resources Control Board")


## 5102 ----
df_5102_all_yrs <- read_csv('https://data.ca.gov/dataset/e620a64f-6b86-4ce0-ab4b-03d06674287b/resource/aba87ad9-f6b0-4a7e-a45e-d1452417eb7f/download/calhr_5102_statewide_2011-2020.csv') %>%
    clean_names() 

# add additional levels for ethnicity grouping
#   identity_variable (in original dataset) is most detailed level
#   level 1 groups all BIPOC together (resulting groups are BIPOC and white)
#   level 2 groups some identity_variable sub-groupings together (Asian group and Pacific Islander group)
df_5102_all_yrs <- df_5102_all_yrs %>% 
    mutate(
        ethnicity_level1 = case_when(
            identity_variable == 'White' ~ 'White', 
            TRUE ~ 'BIPOC'),
        ethnicity_level2 = case_when(
            str_detect(identity_variable, 'Pacific Islander') ~ 'Pacific Islander', # groups (all start with "Pacific Islander - "): Guamanian, Hawaiian, Other or Multiple, and Samoan
            str_detect(identity_variable, 'Asian') ~ 'Asian', # groups (all start with "Asian - "): Cambodian, Chinese, Filipino, Indian, Japanese, Korean, Laotian, Other or Multiple, Vietnamese
            TRUE ~ identity_variable)
    )

# filter for 2020
df_5102_2020 <- df_5102_all_yrs %>% 
    filter(as_of_date == as.Date('2020-12-31'))

# Filtering for EPA and related BDOs 
df_5102_2020_epa <- df_5102_2020 %>% 
    filter(dept == "Air Resources Board"|
               dept == "Environmental Health Hazard Assessment, Office of"|
               dept == "Environmental Protection Agency"|
               dept == "Pesticide Regulation, Department of"|
               dept == "Resources Recycling and Recovery, Department of"|
               dept == "Toxic Substances Control, Department of"|
               dept == "Water Resources Control Board")


## census data (ACS 1 yr) ----
### view all variables
acs_variables <- load_variables(2019, 'acs1', cache = TRUE)
    # to view label for a given code (name)
    # acs_variables %>% filter(name == 'B03002_009') %>% pull(label)

### get ACS data
acs_data_raw <- get_acs(geography = 'state', 
                        variables = c(# total_state_pop = 'B02001_001',
                            'Hispanic or Latino' = 'B03002_012', # Total Hispanic or Latino
                            'White' = 'B03002_003', # White (Not Hispanic or Latino)
                            'Black or African American' = 'B03002_004', # Black or African American (Not Hispanic or Latino)
                            'Native American or Alaska Native' = 'B03002_005', # American Indian and Alaska Native (Not Hispanic or Latino)
                            'Asian' = 'B03002_006', # Asian (Not Hispanic or Latino)
                            'Pacific Islander' = 'B03002_007', # Native Hawaiian and Other Pacific Islander (Not Hispanic or Latino)
                            'Other' = 'B03002_008', # Some other race (Not Hispanic or Latino)
                            'Multiple' = 'B03002_009'# Two or more races (Not Hispanic or Latino)
                        ),
                        summary_var = c(total_state_pop = 'B02001_001'), 
                        survey = 'acs1', # use 'acs1' or 'acs5' to get 1 or 5 year acs
                        state = 'CA', 
                        geometry = FALSE, # set to TRUE to get as geospatial data
                        year = 2019)

# clean / reformat the acs data (the data will be formatted to be 
# consistent with the "level 2" ethnicity groupings in the workforce 
# data that are created above)
acs_data_level2 <- acs_data_raw %>% 
    clean_names() %>% 
    mutate(pop_pct = estimate / summary_est) %>% 
    rename(total_state_pop = summary_est, 
           total_state_pop_moe = summary_moe,
           location_name = name,
           ethnicity_level2 = variable)

    # check (should be TRUE) - make sure sum of populations by group == total state population
    sum(acs_data_level2$estimate) == acs_data_level2$total_state_pop[1]

### group the 'Other' and 'Multiple' rows into one 'Other or Multiple Race' row
acs_data_level2 <- acs_data_level2 %>% 
    mutate(ethnicity_level2 = 
               case_when(ethnicity_level2 == 'Other' | 
                             ethnicity_level2 == 'Multiple' ~ 
                             'Other or Multiple Race',
                         TRUE ~ ethnicity_level2)) %>% 
    group_by(geoid, location_name, ethnicity_level2, total_state_pop) %>% 
    summarize(estimate = sum(estimate)) %>%
    ungroup() %>% 
    mutate(pop_pct = estimate / total_state_pop) %>% 
    {.}

    # check (should be TRUE) - make sure sum of populations by group == total state population
    sum(acs_data_level2$estimate) == acs_data_level2$total_state_pop[1]
    
### create a dataset grouped at level 1 - all BIPOC together (resulting groups are BIPOC and white)
acs_data_level1 <- acs_data_level2 %>%  
    mutate(
        ethnicity_level1 = case_when(
            ethnicity_level2 == 'White' ~ 'White', 
            TRUE ~ 'BIPOC')
    ) %>% 
    group_by(geoid, location_name, ethnicity_level1, total_state_pop) %>% 
    summarize(estimate = sum(estimate)) %>% 
    ungroup() %>% 
    mutate(pop_pct = estimate / total_state_pop) %>% 
    {.}
    
    # check 
    sum(acs_data_level1$estimate) == acs_data_level1$total_state_pop[1]





# 2 - 5112 exploration ----------------------------------------------------------------
## count of entries by race ----
plot_1 <- ggplot() +
    geom_bar(data = df_5112_2020_epa, 
             aes(ethnicity)) +
    coord_flip()


# 3 - 5112 rates ----------------------------------------------------------

## calculate rates ----

# original ethnicity 
summary_rates <- df_5112_2020_epa %>% 
    select(ethnicity, hire_type) %>% 
    add_count(ethnicity, name = 'ethnicity_total') %>% 
    add_count(ethnicity, hire_type, name = 'ethnicity_type_total') %>% 
    mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
    distinct() %>%
    arrange(ethnicity)


# level 1 ethnicity (white vs BIPOC)
summary_rates_level1 <- df_5112_2020_epa %>% 
    select(ethnicity_level1, hire_type) %>% 
    add_count(ethnicity_level1, name = 'ethnicity_total') %>% 
    add_count(ethnicity_level1, hire_type, name = 'ethnicity_type_total') %>% 
    distinct() %>% 
    mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
    arrange(ethnicity_level1) %>%
    {.}


## plot rates ----
### advancement ----
plot_advancement <- summary_rates %>% 
    filter(hire_type == 'Advancements') %>% 
    # fct_reorder()
    ggplot() +
    geom_bar(aes(x = ethnicity, y = rate), stat = 'identity') +
    coord_flip() + 
    scale_y_continuous(labels = percent) +
    labs(title = 'Rate of Advancement by Ethnicity',
         x = 'Ethnicity',
         y = 'Rate')

### intake (external) ----
plot_intake_outside <- summary_rates %>% 
    filter(hire_type == 'Intake: Outside Hires') %>% 
    # fct_reorder()
    ggplot() +
    geom_bar(aes(x = ethnicity, y = rate), stat = 'identity') +
    coord_flip() + 
    scale_y_continuous(labels = percent) +
    labs(title = 'Rate of External Hires by Ethnicity',
         x = 'Ethnicity',
         y = 'Rate')

### intake (other agency) ----
plot_intake_other_agencies <- summary_rates %>% 
    filter(hire_type == 'Intake: Hires from other State Agencies') %>% 
    # fct_reorder()
    ggplot() +
    geom_bar(aes(x = ethnicity, y = rate), stat = 'identity') +
    coord_flip() + 
    scale_y_continuous(labels = percent) +
    labs(title = 'Rate of Hires from Other State Agencies by Ethnicity',
         x = 'Ethnicity',
         y = 'Rate')

### combined ----
ordering <- summary_rates %>% 
    filter(hire_type == 'Intake: Outside Hires') %>% 
    arrange(desc(rate)) %>% 
    pull(ethnicity) %>% 
    as.character()
missing <- unique(summary_rates$ethnicity)[!unique(summary_rates$ethnicity) %in% ordering]
ordering <- c(ordering, missing)

plot_5112_rates_combined <- summary_rates %>% 
    mutate(ethnicity = as.factor(ethnicity)) %>% 
    mutate(ethnicity = fct_relevel(ethnicity, rev(ordering))) %>%  
    ggplot() +
    geom_bar(mapping = aes(x = ethnicity, 
                           y = rate, 
                           fill = hire_type), 
             stat = 'identity') +
    coord_flip() + 
    scale_y_continuous(labels = percent) +
    labs(title = 'Portion of Hires / Advancements',
         x = 'Ethnicity',
         y = 'Portion') +
    theme(legend.position = 'bottom', 
          legend.title = element_blank()) +
    guides(fill = guide_legend(reverse = TRUE))

### combined L1 ethnicity ----
plot_5112_rates_combined_level1 <- summary_rates_level1 %>% 
    ggplot() +
    geom_bar(mapping = aes(x = ethnicity_level1, 
                           y = rate, 
                           fill = hire_type), 
             stat = 'identity') +
    coord_flip() + 
    scale_y_continuous(labels = percent) +
    labs(title = 'Portion of Hires / Advancements',
         x = 'Ethnicity',
         y = 'Portion') +
    theme(legend.position = 'bottom', 
          legend.title = element_blank()) +
    guides(fill = guide_legend(reverse = TRUE))



# 4 - 5102 summaries (vs CA pop) -----------------------------------------------

# format census data for plot (all of CA) ----
acs_data_level1 <- acs_data_level1 %>%
    mutate(type = 'State Population') %>%
    rename(ethnicity_total = estimate,
           rate = pop_pct) %>%
    
    {.}

acs_data_level2 <- acs_data_level2 %>%
    mutate(type = 'State Population') %>%
    rename(ethnicity_total = estimate,
           rate = pop_pct) %>%
    {.}

## 5102 summary functions
fun_summary_5102_l1 <- function(dataset) {
    dataset %>%
        add_count(ethnicity_level1,
                  wt = record_count,
                  name = 'ethnicity_total') %>%
        select(ethnicity_level1, ethnicity_total) %>%
        distinct() %>%
        mutate(rate = ethnicity_total / sum(ethnicity_total)) %>%
        mutate(type = 'State Government Workforce') %>%
        arrange(ethnicity_level1) %>%
        {.}
}

fun_summary_5102_l2 <- function(dataset) {
    dataset %>%
        add_count(ethnicity_level2,
                  wt = record_count,
                  name = 'ethnicity_total') %>%
        select(ethnicity_level2, ethnicity_total) %>%
        distinct() %>%
        mutate(rate = ethnicity_total / sum(ethnicity_total)) %>%
        mutate(type = 'State Government Workforce') %>%
        arrange(ethnicity_level2) %>%
        {.}
}

## plot function
fun_plot_5102_l1 <- function(dataset, plot_title) {
    dataset %>% 
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity_level1, 
                               y = rate, 
                               fill = type), 
                 stat = 'identity', 
                 position = 'dodge') +
        coord_flip() + 
        scale_y_continuous(labels = percent) +
        labs(title = plot_title,
             x = 'Ethnicity',
             y = 'Portion', 
             caption = 'State population data from 2019 1 yr ACS  |  workforce data from CalHR 5102') +
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}

fun_plot_5102_l2 <- function(dataset, plot_title) {
    dataset %>% 
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity_level2, 
                               y = rate, 
                               fill = type), 
                 stat = 'identity', 
                 position = 'dodge') +
        coord_flip() + 
        scale_y_continuous(labels = percent) +
        labs(title = plot_title,
             x = 'Ethnicity',
             y = 'Portion', 
             caption = 'State population data from 2019 1 yr ACS  |  workforce data from CalHR 5102') +
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}


## TEST - combined dataset !!!!!!!!!!!!!!!!!!!!!!!!
z_test <- df_5102_2020_epa %>% 
    filter(employee_category == 'Legal Occupations') %>%
    fun_summary_5102_l2() %>% 
     bind_rows(acs_data_level2) %>% 
    mutate(category = 'Legal Occupations') %>% 
    bind_rows(df_5102_2020_epa %>% 
                  filter(employee_category == 'Management Occupations') %>%
                  fun_summary_5102_l2() %>% 
                  bind_rows(acs_data_level2) %>% 
                  mutate(category = 'Management Occupations'))

fun_plot_5102_l2_facet <- function(dataset, plot_title) {
    dataset %>% 
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity_level2, 
                               y = rate, 
                               fill = type), 
                 stat = 'identity', 
                 position = 'dodge') + 
        facet_wrap(~ category) +
        coord_flip() + 
        scale_y_continuous(labels = percent) +
        labs(title = plot_title,
             x = 'Ethnicity',
             y = 'Portion', 
             caption = 'State population data from 2019 1 yr ACS  |  workforce data from CalHR 5102') +
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}
(z_test_plot <- z_test %>% 
    fun_plot_5102_l2_facet(plot_title = 'test plot'))

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

## CalEPA summary ----
### L1 ----
(pl_5102_calepa_l1 <- df_5102_2020_epa %>% 
     fun_summary_5102_l1() %>% 
     bind_rows(acs_data_level1) %>% 
     fun_plot_5102_l1(plot_title = 'All CalEPA Employees vs State Population'))


### L2 ----
(pl_5102_calepa_l2 <- df_5102_2020_epa %>% 
     fun_summary_5102_l2() %>% 
     bind_rows(acs_data_level2) %>% 
     fun_plot_5102_l2(plot_title = 'All CalEPA Employees vs State Population'))

## Legal ----
### L2 ----
(pl_5102_legal_l2 <- df_5102_2020_epa %>% 
     filter(employee_category == 'Legal Occupations') %>%
     fun_summary_5102_l2() %>% 
     bind_rows(acs_data_level2) %>% 
     fun_plot_5102_l2(plot_title = 'CalEPA Legal Occupations vs State Population'))
 
## Management ----
### L2 ----
(pl_5102_mgmt_l2 <- df_5102_2020_epa %>% 
     filter(employee_category == 'Management Occupations') %>%
     fun_summary_5102_l2() %>% 
     bind_rows(acs_data_level2) %>% 
     fun_plot_5102_l2(plot_title = 'CalEPA Management Occupations vs State Population'))
 
## Admin ----
### L2 ----
(pl_5102_admin_l2 <- df_5102_2020_epa %>% 
     filter(employee_category == 'Office and Administrative Support Occupations' |
                (employee_category == 'Computer and Mathematical Occupations' & 
                     employee_category == 'Unmapped Classes')) %>% 
     fun_summary_5102_l2() %>% 
     bind_rows(acs_data_level2) %>% 
     fun_plot_5102_l2(plot_title = 'CalEPA Administrative Occupations vs State Population'))
    
## Tech ----
### L2 ----
(pl_5102_tech_l2 <- df_5102_2020_epa %>% 
     filter(employee_category == 'Architecture and Engineering Occupations' |
                employee_category == 'Life, Physical, and Social Science Occupations' | 
                (employee_category == 'Computer and Mathematical Occupations' & 
                     employee_category == 'Operations Research Analysts')) %>% 
     fun_summary_5102_l2() %>% 
     bind_rows(acs_data_level2) %>% 
     fun_plot_5102_l2(plot_title = 'CalEPA Technical Occupations vs State Population'))
 


# 5 - 5112 group summaries ------------------------------------------------------
## summary function 
fun_summary_5112_l1 <- function(dataset) {
    dataset %>% 
        select(ethnicity_level1, hire_type) %>% 
        add_count(ethnicity_level1, name = 'ethnicity_total') %>% 
        add_count(ethnicity_level1, hire_type, name = 'ethnicity_type_total') %>% 
        distinct() %>% 
        mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
        arrange(ethnicity_level1) %>%
        {.} 
}

## plot function
fun_plot_5112_l1 <- function(dataset, plot_title) {
    dataset %>% 
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity_level1, 
                               y = rate, 
                               fill = hire_type), 
                 stat = 'identity') +
        coord_flip() + 
        scale_y_continuous(labels = percent) +
        labs(title = plot_title,
             x = 'Ethnicity',
             y = 'Portion') +
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE)) + 
        # geom_text(data = dataset, 
        #           stat = 'identity', 
        #           aes(x = ethnicity_level1, 
        #               y = rate, 
        #               label = ethnicity_total), 
        #           vjust = 0) + 
        geom_blank()
}

## Legal ----
sum_5112_l1_legal <- df_5112_2020_epa %>% 
    filter(soc_major_group_title == 'Legal Occupations') %>% 
    fun_summary_5112_l1()

### plot
(pl_5112_l1_legal <- sum_5112_l1_legal %>% 
    fun_plot_5112_l1(plot_title = 'Legal - Portion of Hires / Advancements'))


## Management ----
sum_5112_l1_management <- df_5112_2020_epa %>% 
    filter(soc_major_group_title == 'Management Occupations') %>%
    fun_summary_5112_l1()

### plot
(pl_5112_l1_mgmt <- sum_5112_l1_management %>% 
    fun_plot_5112_l1(plot_title = 'Management - Portion of Hires / Advancements'))


## Admin ----
sum_5112_l1_admin <- df_5112_2020_epa %>% 
    filter(soc_major_group_title == 'Office and Administrative Support Occupations' |
               (soc_major_group_title == 'Computer and Mathematical Occupations' & 
                    soc_detailed_group_title == 'Unmapped Classes')) %>% 
    fun_summary_5112_l1()

### plot
(pl_5112_l1_admin<- sum_5112_l1_admin %>% 
    fun_plot_5112_l1(plot_title = 'Administrative - Portion of Hires / Advancements'))


## Tech ----
sum_5112_l1_tech <- df_5112_2020_epa %>% 
    filter(soc_major_group_title == 'Architecture and Engineering Occupations' |
               soc_major_group_title == 'Life, Physical, and Social Science Occupations' | 
               (soc_major_group_title == 'Computer and Mathematical Occupations' & 
                    soc_detailed_group_title == 'Operations Research Analysts')) %>% 
    fun_summary_5112_l1()

### plot
(pl_5112_l1_tech <- sum_5112_l1_tech %>% 
    fun_plot_5112_l1(plot_title = 'Tech - Portion of Hires / Advancements'))
