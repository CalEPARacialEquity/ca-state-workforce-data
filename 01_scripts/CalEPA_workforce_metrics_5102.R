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
library(glue)
library(tidyr)


# 1 - load / transform data ---------------------------------------------------------------

## 5102 ----
df_5102 <- read_csv('https://data.ca.gov/dataset/e620a64f-6b86-4ce0-ab4b-03d06674287b/resource/aba87ad9-f6b0-4a7e-a45e-d1452417eb7f/download/calhr_5102_statewide_2011-2020.csv') %>%
    clean_names() 

# add additional levels for ethnicity grouping
#   identity_variable (in original dataset) is most detailed level
#   level 1 groups all BIPOC together (resulting groups are BIPOC and white)
#   level 2 groups some identity_variable sub-groupings together (Asian group and Pacific Islander group)
df_5102 <- df_5102 %>% 
    mutate(
        ethnicity_level1 = case_when(
            identity_variable == 'White' ~ 'White', 
            TRUE ~ 'BIPOC'),
        ethnicity_level2 = case_when(
            str_detect(identity_variable, 'Pacific Islander') ~ 'Pacific Islander', # groups (all start with "Pacific Islander - "): Guamanian, Hawaiian, Other or Multiple, and Samoan
            str_detect(identity_variable, 'Asian') ~ 'Asian', # groups (all start with "Asian - "): Cambodian, Chinese, Filipino, Indian, Japanese, Korean, Laotian, Other or Multiple, Vietnamese
            TRUE ~ identity_variable)
    )

# Filtering for EPA and related BDOs 
df_5102_epa <- df_5102 %>% 
    filter(dept == "Air Resources Board"|
               dept == "Environmental Health Hazard Assessment, Office of"|
               dept == "Environmental Protection Agency"|
               dept == "Pesticide Regulation, Department of"|
               dept == "Resources Recycling and Recovery, Department of"|
               dept == "Toxic Substances Control, Department of"|
               dept == "Water Resources Control Board")

# filter for 2020 data
df_5102_2020_epa <- df_5102_epa %>% 
    filter(as_of_date == '2020-12-31')


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
    {.}
    
    # check (should be TRUE) - make sure sum of populations by group == total state population
    sum(acs_data_level2$estimate) == acs_data_level2$total_state_pop[1]
    
### add a column with each ethnicity's % of total state population
acs_data_level2 <- acs_data_level2 %>% 
    mutate(pop_pct = estimate / total_state_pop) %>% 
    {.}

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
    mutate(pop_pct = estimate / total_state_pop) %>% # update the pop_pct to reflect level 1 numbers
    {.}
    
    # check 
    sum(acs_data_level1$estimate) == acs_data_level1$total_state_pop[1]


    
    

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 2 - 5102 summaries (vs CA pop) -----------------------------------------------
## define color palette for plots
colors_plots <-c('gold', 'darkgreen') # state pop / CalEPA workforce
    
## format census data for plot (all of CA) ----
acs_data_level1 <- acs_data_level1 %>%
    mutate(type = factor('State Population')) %>%
    rename(ethnicity_total = estimate,
           rate = pop_pct) %>%
    
    {.}

acs_data_level2 <- acs_data_level2 %>%
    mutate(type = factor('State Population')) %>%
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
        mutate(type = 'CalEPA Workforce') %>%
        arrange(ethnicity_level1) %>%
        mutate(type = factor(type)) %>% 
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
        mutate(type = 'CalEPA Workforce') %>%
        arrange(ethnicity_level2) %>%
        mutate(type = factor(type)) %>% 
        {.}
}

## plot function
fun_plot_5102_l1 <- function(dataset, plot_title) {
    dataset %>% 
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity_level1, 
                               y = rate, 
                               fill = factor(type, levels = rev(levels(type)))), 
                 stat = 'identity', 
                 position = 'dodge') +
        scale_fill_manual(values = colors_plots) +
        scale_y_continuous(labels = percent) +
        labs(title = plot_title,
             x = 'Ethnicity',
             y = 'Portion', 
             caption = 'State population data from 2019 1 yr ACS  |  workforce data from CalHR 5102') +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}

fun_plot_5102_l1_stack <- function(dataset, plot_title) {
    dataset %>% 
        mutate(type = fct_rev(type)) %>% 
        ggplot() +
        geom_bar(mapping = aes(x = type, # ethnicity_level1, 
                               y = rate, 
                               fill = ethnicity_level1), # factor(type, levels = rev(levels(type)))), 
                 stat = 'identity') +
        # scale_fill_manual(values = colors_plots) +
        scale_y_continuous(labels = percent) +
        labs(title = plot_title,
             # x = 'Ethnicity',
             y = 'Portion', 
             caption = 'State population data from 2019 1 yr ACS  |  workforce data from CalHR 5102') +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}


fun_plot_5102_l2 <- function(dataset, plot_title) {
    dataset %>% 
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity_level2, 
                               y = rate, 
                               fill = factor(type, levels = rev(levels(type)))), 
                 stat = 'identity', 
                 position = 'dodge') +
        scale_fill_manual(values = colors_plots) +
        scale_y_continuous(labels = percent) +
        labs(title = plot_title,
             x = 'Ethnicity',
             y = 'Portion', 
             caption = 'State population data from 2019 1 yr ACS  |  workforce data from CalHR 5102') +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}

fun_plot_5102_l2_stack <- function(dataset, plot_title) {
    dataset %>% 
        mutate(type = fct_rev(type)) %>%
        ggplot() +
        geom_bar(mapping = aes(x = type, 
                               y = rate, 
                               fill = ethnicity_level2), # factor(type, levels = rev(levels(type)))), 
                 stat = 'identity') +
        # scale_fill_manual(values = colors_plots) +
        scale_y_continuous(labels = percent) +
        labs(title = plot_title,
             x = element_blank(),
             y = 'Portion', 
             caption = 'State population data from 2019 1 yr ACS  |  workforce data from CalHR 5102') +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}

## 2.1 - CalEPA summary ----
### L1 ----
(pl_5102_calepa_l1 <- df_5102_2020_epa %>% 
     fun_summary_5102_l1() %>% 
     bind_rows(acs_data_level1) %>% 
     fun_plot_5102_l1(plot_title = 'All CalEPA Employees vs State Population'))

(pl_5102_calepa_l1_stack <- df_5102_2020_epa %>% 
     fun_summary_5102_l1() %>% 
     bind_rows(acs_data_level1) %>% 
     fun_plot_5102_l1_stack(plot_title = 'All CalEPA Employees vs State Population'))


### L2 ----
(pl_5102_calepa_l2 <- df_5102_2020_epa %>% 
     fun_summary_5102_l2() %>% 
     bind_rows(acs_data_level2) %>% 
     fun_plot_5102_l2(plot_title = 'All CalEPA Employees vs State Population'))

(pl_5102_calepa_l2_stack <- df_5102_2020_epa %>% 
     fun_summary_5102_l2() %>% 
     bind_rows(acs_data_level2) %>% 
     fun_plot_5102_l2_stack(plot_title = 'All CalEPA Employees vs State Population'))


## 2.2. - CalEPA By Group ----

### create groupings ----
df_5102_2020_epa <- df_5102_2020_epa %>% 
    mutate(metrics_group = case_when(
        # legal
        employee_category == 'Legal Occupations' ~ 
            'Legal Occupations', 
        # management
        employee_category == 'Management Occupations' ~ 
            'Management Occupations', 
        # admin
        employee_category == 'Office and Administrative Support Occupations' |
            (employee_category == 'Computer and Mathematical Occupations' & 
                 sub_category == 'Unmapped Classes') ~ 
            'Administrative Occupations',
        # technical
        employee_category == 'Architecture and Engineering Occupations' |
            employee_category == 'Life, Physical, and Social Science Occupations' | 
            (employee_category == 'Computer and Mathematical Occupations' & 
                 sub_category == 'Operations Research Analysts') ~ 
            'Technical Occupations')
    )

### L1 ----
# sum_5102_groups_l1 <- df_5102_2020_epa %>% 
#     add_count(metrics_group, # ethnicity_level1, 
#               wt = record_count,
#               name = 'group_total') %>%
#     add_count(ethnicity_level1, metrics_group,
#               wt = record_count,
#               name = 'ethnicity_group_total') %>% 
#     select(ethnicity_level1, ethnicity_group_total, group_total, metrics_group) %>%
#     distinct() %>%
#     mutate(rate = ethnicity_group_total / group_total) %>% 
#     mutate(type = 'CalEPA Workforce') %>%
#     arrange(metrics_group, ethnicity_level1) %>%
#     {.}
# 
# (pl_5102_groups_l1 <- sum_5102_groups_l1 %>% 
#     filter(!is.na(metrics_group)) %>% 
#         bind_rows(acs_data_level1)
#     ggplot() +
#     geom_bar(mapping = aes(x = ethnicity_level1, 
#                            y = rate, 
#                            fill = type), 
#              stat = 'identity', 
#              position = 'dodge') + 
#     facet_wrap(~ metrics_group) +
#     coord_flip() + 
#     scale_y_continuous(labels = percent) +
#     labs(title = 'Test', 
#          subtitle = 'Test',
#          x = 'Ethnicity',
#          y = 'Portion', 
#          caption = 'State population data from 2019 1 yr ACS  |  workforce data from CalHR 5102') +
#     theme(legend.position = 'bottom', 
#           legend.title = element_blank()) +
#     guides(fill = guide_legend(reverse = TRUE)))

sum_5102_groups_l1 <- df_5102_2020_epa %>% 
    filter(metrics_group == 'Legal Occupations') %>%
    fun_summary_5102_l1() %>% 
    bind_rows(acs_data_level1) %>% 
    mutate(metrics_group = 'Legal Occupations') %>%
    bind_rows(df_5102_2020_epa %>% 
                  filter(metrics_group == 'Management Occupations') %>%
                  fun_summary_5102_l1() %>% 
                  bind_rows(acs_data_level1) %>% 
                  mutate(metrics_group = 'Management Occupations')) %>%
    bind_rows(df_5102_2020_epa %>% 
                  filter(metrics_group == 'Administrative Occupations') %>%
                  fun_summary_5102_l1() %>% 
                  bind_rows(acs_data_level1) %>% 
                  mutate(metrics_group = 'Administrative Occupations')) %>%
    bind_rows(df_5102_2020_epa %>% 
                  filter(metrics_group == 'Technical Occupations') %>%
                  fun_summary_5102_l1() %>% 
                  bind_rows(acs_data_level1) %>% 
                  mutate(metrics_group = 'Technical Occupations')) %>%
    mutate(type = factor(type)) %>% 
    {.}

(pl_5102_groups_l1 <- sum_5102_groups_l1 %>%  
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity_level1, 
                               y = rate, 
                               fill = factor(type, levels = rev(levels(type)))), 
                 stat = 'identity', 
                 position = 'dodge') + 
        facet_wrap(~ metrics_group) +
        scale_fill_manual(values = colors_plots) +
        scale_y_continuous(labels = percent) +
        labs(title = 'Test Plot',
             subtitle = 'Test Plot',
             x = 'Ethnicity',
             y = 'Portion', 
             caption = 'State population data from 2019 1 yr ACS  |  workforce data from CalHR 5102') +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE)))

(pl_5102_groups_l1_stack <- sum_5102_groups_l1 %>%  
        ggplot() +
        geom_bar(mapping = aes(x = fct_rev(type), 
                               y = rate, 
                               fill = ethnicity_level1), #factor(type, levels = rev(levels(type)))), 
                 stat = 'identity') + 
        facet_wrap(~ metrics_group) +
        # scale_fill_manual(values = colors_plots) +
        scale_y_continuous(labels = percent) +
        labs(title = 'Test Plot',
             subtitle = 'Test Plot',
             x = element_blank(),
             y = 'Portion', 
             caption = 'State population data from 2019 1 yr ACS  |  workforce data from CalHR 5102') +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE)))


### L2 ----
fun_summary_5102_groups_l2 <- function(dataset) {
    dataset %>% 
        filter(metrics_group == 'Legal Occupations') %>%
        fun_summary_5102_l2() %>% 
        bind_rows(acs_data_level2) %>% 
        mutate(metrics_group = 'Legal Occupations') %>%
        bind_rows(df_5102_2020_epa %>% 
                      filter(metrics_group == 'Management Occupations') %>%
                      fun_summary_5102_l2() %>% 
                      bind_rows(acs_data_level2) %>% 
                      mutate(metrics_group = 'Management Occupations')) %>%
        bind_rows(df_5102_2020_epa %>% 
                      filter(metrics_group == 'Administrative Occupations') %>%
                      fun_summary_5102_l2() %>% 
                      bind_rows(acs_data_level2) %>% 
                      mutate(metrics_group = 'Administrative Occupations')) %>%
        bind_rows(df_5102_2020_epa %>% 
                      filter(metrics_group == 'Technical Occupations') %>%
                      fun_summary_5102_l2() %>% 
                      bind_rows(acs_data_level2) %>% 
                      mutate(metrics_group = 'Technical Occupations')) %>%
        mutate(type = factor(type)) %>% 
        {.}
}
   
sum_5102_groups_l2 <- df_5102_2020_epa %>% 
    fun_summary_5102_groups_l2() 


fun_plot_5102_groups_l2 <- function(dataset, title = 'Test', subtitle = 'Test') {
    dataset %>% 
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity_level2, 
                               y = rate, 
                               fill = factor(type, levels = rev(levels(type)))), 
                 stat = 'identity', 
                 position = 'dodge') + 
        facet_wrap(~ metrics_group) +
        scale_y_continuous(labels = percent) +
        scale_fill_manual(values = colors_plots) +
        labs(title = title,
             subtitle = subtitle,
             x = 'Ethnicity',
             y = 'Portion', 
             caption = 'State population data from 2019 1 yr ACS  |  workforce data from CalHR 5102') +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}

(pl_5102_groups_l2 <- sum_5102_groups_l2 %>%  
        fun_plot_5102_groups_l2())


        
fun_plot_5102_groups_l2_stack <- function(dataset, title = 'Test', subtitle = 'Test') {
    dataset %>% 
        ggplot() +
        geom_bar(mapping = aes(x = fct_rev(type), 
                               y = rate, 
                               fill = ethnicity_level2), 
                 stat = 'identity') + 
        facet_wrap(~ metrics_group) +
        scale_y_continuous(labels = percent) +
        # scale_fill_manual(values = colors_plots) +
        labs(title = title,
             subtitle = subtitle,
             x = element_blank(),
             y = 'Portion', 
             caption = 'State population data from 2019 1 yr ACS  |  Workforce data from CalHR 5102') +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}

(pl_5102_groups_l2_stack <- sum_5102_groups_l2 %>%  
    fun_plot_5102_groups_l2_stack)
    

## 2.4 By BDO Plots ----
### L2 ----
summary_5102_l2_bdo <- df_5102_2020_epa %>%
    add_count(dept,  wt = record_count,
              name = 'dept_total') %>% 
    add_count(ethnicity_level2, dept,
              wt = record_count,
              name = 'ethnicity_total') %>%
    select(ethnicity_level2, ethnicity_total, dept, dept_total) %>%
    distinct() %>%
    mutate(rate = ethnicity_total / dept_total) %>%
    mutate(type = 'CalEPA Workforce') %>%
    arrange(dept, ethnicity_level2) %>%
    mutate(type = factor(type)) %>% 
    bind_rows(acs_data_level2 %>% 
                  rename(dept = type)) %>% 
    {.}


ordering_bdo <- summary_5102_l2_bdo %>% 
    filter(ethnicity_level2 == 'White') %>% 
    arrange(rate) %>% 
    pull(dept)
(pl_5102_l2_bdo_stack <- summary_5102_l2_bdo %>% 
    # mutate(type = fct_rev(type)) %>%
    ggplot() +
    geom_bar(mapping = aes(x = fct_relevel(dept, ordering_bdo), #fct_rev(type), 
                           y = rate, 
                           fill = ethnicity_level2), # factor(type, levels = rev(levels(type)))), 
             stat = 'identity') +
    # scale_fill_manual(values = colors_plots) +
    scale_y_continuous(labels = percent) +
    labs(title = 'Test Plot',
         x = element_blank(),
         y = 'Portion', 
         caption = 'State population data from 2019 1 yr ACS  |  workforce data from CalHR 5102') +
    coord_flip() + 
    # facet_wrap(~ dept) +
    theme(legend.position = 'bottom', 
          legend.title = element_blank()) +
    guides(fill = guide_legend(reverse = TRUE))) + 
    geom_vline(xintercept = 1.5, size = 0.5, linetype = 'dashed')#, color = 'grey50')


### L2 - BDO & Group ----
pl_5102_groups_l2_arb <- df_5102_2020_epa %>% 
    filter(dept == 'Air Resources Board') %>% 
    fun_summary_5102_groups_l2() %>% 
    mutate(type = case_when(as.character(type) == 'CalEPA Workforce' ~ 
                                'Air Resources Board Workforce',
                            TRUE ~ as.character(type))) %>%
    fun_plot_5102_groups_l2_stack(subtitle = 'Air Resources Board')

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 3 - retention / departures (by job class) ----------------------------------------------------

## get 5112 data ----
### pull data from data.ca.gov
df_5112_2020 <- read_csv('https://data.ca.gov/dataset/e620a64f-6b86-4ce0-ab4b-03d06674287b/resource/d308f328-6b5b-41c1-8bc2-a4afcfcee3d1/download/5112-inline-report_12.31.20.csv') %>% 
    clean_names()

### add additional levels for ethnicity grouping
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

### Filtering for EPA and related BDOs 
df_5112_2020_epa <- df_5112_2020 %>% 
    filter(department == "Air Resources Board"|
               department == "Environmental Health Hazard Assessment, Office of"|
               department == "Environmental Protection Agency"|
               department == "Pesticide Regulation, Department of"|
               department == "Resources Recycling and Recovery, Department of"|
               department == "Toxic Substances Control, Department of"|
               department == "Water Resources Control Board")


## calculate... ----

job_class <- 'ENVIRONMENTAL SCIENTIST'

job_class_flux <- df_5112_2020_epa %>% 
    filter(class_title == job_class) %>% 
    group_by(ethnicity_level2, hire_type) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    mutate(hire_type = case_when(
        hire_type == 'Intake: Outside Hires' ~ 
            'intake_outside',
        hire_type == 'Intake: Hires from other State Agencies' ~ 
            'intake_other_agencies',
        hire_type == 'Advancements' ~ 
            'intake_advancements'
    ))
    # add_count(ethnicity, name = 'ethnicity_total') %>% 
    # add_count(ethnicity, hire_type, name = 'ethnicity_type_total')

job_class_flux <- job_class_flux %>% 
    pivot_wider(names_from = hire_type, 
                values_from = count)

job_class_delta <- df_5102_epa %>% 
    filter(class_title == job_class) %>% 
    filter(as_of_date == '2020-12-31' | as_of_date == '2019-12-31') %>% 
    mutate(year = glue('yr_{year(as_of_date)}')) %>% 
    group_by(year, ethnicity_level2) %>% 
    summarize(total = sum(record_count)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = year, values_from = total) %>% 
    mutate(change = yr_2020 - yr_2019) %>% 
    {.}
    
job_class_calc <- job_class_delta %>% 
    left_join(job_class_flux, by = 'ethnicity_level2')


## timeline ----
job_class_timeline <- df_5102_epa %>% 
    filter(class_title == job_class) 

