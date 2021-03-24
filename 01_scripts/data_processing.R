# load packages -----------------------------------
library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(here)
library(purrr)
library(glue)
library(lubridate)


# !!!!!!!!!!!!!!! ENTER THE RANGE OF YEARS WITH 5102 REPORTS (start_year:end_year) !!!!!!!!!!!!!!!
year_range <- 2011:2020


# read data into R -----------------------------------
df_5102_report <- map_df(.x = year_range, 
                         .f = ~ read_excel(here('02_data_raw', 
                                                glue('calhr-5102-statewide-', .x, '.xlsx')),
                                           col_types = 'text')) %>% 
    type_convert() 

# save the original column names - may want to revert back to these when saving the output file
names_df_5102_report <- names(df_5102_report) 

# clean up the column names to make them easier to work with in R
df_5102_report <- df_5102_report %>% 
    clean_names() 

# to check an individual year's file
    # year <- 2019
    # df_year <- read_excel(path = here('02_data_raw', glue('calhr-5102-statewide-', year, '.xlsx')),
    #                       col_types = 'text') #%>% 
    #     #type_convert()
    # head(df_year) # view the first couple of records
    # tail(df_year) # view the last couple of records



# re-format data -----------------------------------
# fix dates
# check the number of NAs in the original dataset (to be sure there's a value for each record)
sum(is.na(df_5102_report$as_of_date))
# convert the dates (it's okay if there are warning messages from this step, as long as the checks below look okay)
df_5102_report <- df_5102_report %>% 
    mutate(as_of_date = case_when(!is.na(mdy(as_of_date)) ~ 
                                      mdy(as_of_date),
                                  !is.na(excel_numeric_to_date(as.numeric(as_of_date))) ~ 
                                      excel_numeric_to_date(as.numeric(as_of_date)),
                                  TRUE ~ NA_Date_))
# check to make sure the conversion worked
sum(is.na(df_5102_report$as_of_date)) # should be the same as the number above, probably zero
range(df_5102_report$as_of_date) # check to make sure the new dates are within the correct range



# write the processed data to a new file -----------------------------------
# revert back to the original names (assuming that we want the output dataset to have the same column names as the source datasets)
names(df_5102_report) <- names_df_5102_report

# write the data to the '03_data_processed' folder
# NOTE: writing the data to a gzip file rather than a regular csv to save space - you can 
# read/write using this format directly with R using the readr package, and you can extract 
# it to a regular csv using 7zip (or some other software)
write_csv(x = df_5102_report, 
          file = here('03_data_processed', 
                      glue('calhr_5102_statewide_', 
                           year_range[1], 
                           '-', 
                           year_range[length(year_range)], 
                           '.csv.gz')))

# also writing  a copy of the data directly to the shiny folder, since all of the code/data for 
# the app needs to be contained within a single folder in order to load to shinyapps.io
write_csv(x = df_5102_report, 
          file = here('05_shiny_app', 
                      'data', 
                      glue('calhr_5102_statewide_', 
                           year_range[1], 
                           '-', 
                           year_range[length(year_range)], 
                           '.csv.gz')))
