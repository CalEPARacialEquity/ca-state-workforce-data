# workforce metrics


# load packages
library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(here)
library(purrr)
library(glue)
library(lubridate)


# 1- read data into R --------------------------------------------------------

## 5112 ----
df_int_adv_2020 <- read_csv(here('02_data_raw', 
                                '5112 Inline Report_12.31.20.csv')) %>% 
    clean_names()

## filter for CalEPA departments
df_int_adv_2020_calepa <- df_int_adv_2020 %>% 
    filter(department %in% c('Air Resources Board',
                             'Environmental Health Hazard Assessment, Office of',
                             'Environmental Protection Agency',
                             'Pesticide Regulation, Department of',
                             'Toxic Substances Control, Department of',
                             'Water Resources Control Board'))

## summarize (get number of employees for each unique type of record)
df_int_adv_2020_calepa_summary <- df_int_adv_2020_calepa %>% 
    group_by_all() %>% 
    summarize(count = n()) %>% 
    ungroup()


## 5102 ----
df_workforce_totals_2020 <- read_excel(here('02_data_raw', 
                                       'calhr-5102-statewide-2020.xlsx')) %>% 
    clean_names() %>% 
    mutate(class_code = as.numeric(class_code))
    
    # as a check, get the total number of employees in 2020
        sum(df_workforce_totals_2020$record_count) # 222837
    # compare to the total number of records in the 5112 dataset
        nrow(df_int_adv_2020) # 48904
        
    df_workforce_totals_2020_calepa <- df_workforce_totals_2020 %>% 
        filter(dept %in% c('Air Resources Board',
                           'Environmental Health Hazard Assessment, Office of',
                           'Environmental Protection Agency',
                           'Pesticide Regulation, Department of',
                           'Toxic Substances Control, Department of',
                           'Water Resources Control Board'))
        

##  join datasets -----------------------------------------------------------
## join the 5102 data (totals) to the 5112 data (intake/advancements) - to get the baseline for comparison
    df_int_adv_2020_calepa_summary <- df_int_adv_2020_calepa_summary %>% 
        left_join(df_workforce_totals_2020_calepa %>% 
                      select(-c(as_of_date, scheme_code, 
                                # employee_category,
                                sub_category, soc_code# ,
                                # class_code
                                )) %>% 
                      rename(workforce_total_count = record_count), 
                  by = c('department' = 'dept',
                         'soc_major_group_title' = 'employee_category',
                         # 'soc_detailed_group_title' = 'sub_category', # NOTE: "soc_detailed_group_title" DOES NOT MATCH "sub_category"
                         # 'soc_detailed_group_code' = 'soc_code',
                         'class_code' = 'class_code',
                         'class_title' = 'class_title',
                         'ethnicity' = 'identity_variable',
                         'gender' = 'gender')
                  )
    # check - should be TRUE (makes sure the number of rows doesn't change as a result of the join - only the number of columns should change)
    nrow(df_int_adv_2020_calepa_summary) == n_distinct(df_int_adv_2020_calepa)
    
## add intake/advancement numbers as percent of total workforce
    df_int_adv_2020_calepa_summary <- df_int_adv_2020_calepa_summary %>% 
        mutate(pct_of_total = count / workforce_total_count)
    
## unmatched records - these are the records that are in the 5112 dataset but not in the 5102 dataset
    z_unmatched <- df_int_adv_2020_calepa_summary %>% filter(is.na(workforce_total_count))
    
    # for example:  department = African-American Museum, California
    #               class_title = STAFF SERVICES MANAGER I
    #               ethnicity = Asian - Filipino
    #               gender = Female
    
    # View(df_5102_2020_totals %>% filter(dept == 'African-American Museum, California' & class_title == 'STAFF SERVICES MANAGER I'))
    # View(df_5102_2020_totals %>% filter(dept == 'Air Resources Board' & class_title == 'SUPERVISING HYDROELECTRIC POWER UTILITY ENGINEER'))


# 2- compare categories ------------------------------------------------------
View(df_workforce_totals_2020_calepa %>% count(employee_category))
View(df_int_adv_2020_calepa %>% count(soc_major_group_title)) 

    # employee category vs soc_major_group_title
    (df_int_adv_2020_calepa %>% 
            distinct(soc_major_group_title) %>% 
            pull(soc_major_group_title)) %in%
    (df_workforce_totals_2020_calepa %>% 
         distinct(employee_category) %>% 
         pull(employee_category))

View(df_workforce_totals_2020_calepa %>% count(sub_category))
View(df_int_adv_2020_calepa %>% count(soc_detailed_group_title))

    # sub_category vs soc_detailed_group_title
    (df_int_adv_2020_calepa %>% 
            distinct(soc_detailed_group_title) %>% 
            pull(soc_detailed_group_title)) %in% 
        (df_workforce_totals_2020_calepa %>% 
             distinct(sub_category) %>% 
             pull(sub_category))
    
    
    


# 3 - ACS Stuff ---------------------------------------------------------------
library(tidycensus)
v19 <- load_variables(2019, "acs5", cache = TRUE)