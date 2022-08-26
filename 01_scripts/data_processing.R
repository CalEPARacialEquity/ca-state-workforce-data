
# load packages -----------------------------------------------------------
library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(glue)
library(lubridate)
library(archive)
library(ckanr)



# read data ---------------------------------------------------------------
## get list of files in the '02_data_raw/5102' folder
files_list <- list.files(here('02_data_raw', '5102'))

## verify the files to be used
## (should be excel files with name 'calhr-5102-statewide-YYYY.xlsx')
files_valid <- str_detect(string = files_list, pattern = 'calhr-5102-statewide-\\d{4}.xlsx')
files_list <- files_list[files_valid]

## read data
df_5102_report <- map_df(.x = list.files(here('02_data_raw', '5102')), 
                         .f = ~ read_excel(here('02_data_raw', '5102', .x),
                                           col_types = 'text') %>% 
                             # standardize field names (remove spaces)
                             `names<-`(str_remove_all(string = names(.), pattern = ' '))
) %>% 
    type_convert()

# clean up the column names to make them easier to work with in R
df_5102_report <- df_5102_report %>% 
    clean_names()



# re-format data ----------------------------------------------------------
## fix dates ----
### check original dataset (to be sure there's a value for each record)
sum(is.na(df_5102_report$as_of_date))
df_5102_report %>% count(as_of_date)

### convert the dates 
### (it's okay if there are warning messages from this step, as long as the checks below look okay)
df_5102_report <- df_5102_report %>% 
    mutate(as_of_date = case_when(!is.na(mdy(as_of_date)) ~ 
                                      mdy(as_of_date),
                                  !is.na(excel_numeric_to_date(as.numeric(as_of_date))) ~ 
                                      excel_numeric_to_date(as.numeric(as_of_date)),
                                  TRUE ~ NA_Date_))

### check to make sure the conversion worked
sum(is.na(df_5102_report$as_of_date)) # should be the same as the number above, probably zero
df_5102_report %>% count(as_of_date)



# write the processed data to a new file -----------------------------------
## revert back to the original names 
## (assuming that we want the output dataset to have the same column names as the source datasets)
df_5102_report <- df_5102_report %>% 
    rename('As Of Date' = as_of_date, 'Dept' = dept,
           'Employee Category' = employee_category, 'Sub Category' = sub_category, 
           'SOC Code' = soc_code, 'Scheme Code' = scheme_code, 
           'Class Code' = class_code, 'Class Title' = class_title, 
           'Identity Variable' = identity_variable, 'Gender' = gender,
           'Record Count' = record_count)

## write the data to the '03_data_processed' folder
## NOTE: writing the data to a zip file rather than a regular csv to save space - you can 
## read/write using this format directly with R using the readr package, or extract 
# it to a regular csv
out_file <- paste0('calhr_5102_statewide_', 
                   year(min(df_5102_report$`As Of Date`)), 
                   '-', 
                   year(max(df_5102_report$`As Of Date`)))

### zip file (zipped csv)
write_csv(x = df_5102_report, 
          file = archive_write(here('03_data_processed', 
                                    paste0(out_file,'.zip')), 
                               paste0(out_file, '.csv')))

### gz file
# write_csv(x = df_5102_report, 
#           file = here('03_data_processed', 
#                       paste0(out_file, '.csv.gz')))


## also write a copy data directly to the shiny folder, since all of the code/data for 
## the app needs to be contained within a single folder in order to load to shinyapps.io
write_csv(x = df_5102_report,
          file = here('05_shiny_app',
                      paste0(out_file, '.csv.gz')))



# upload to open data portal ----------------------------------------------
## Note: As an alternative to the steps below, you can do this by extracting
## the csv from the zip file created above, then manually load that csv file
## to the data.ca.gov portal, at: 
## https://data.ca.gov/dataset/calhr-civil-rights-data-for-gare-capital-cohort-2019/resource/aba87ad9-f6b0-4a7e-a45e-d1452417eb7f

## write data to temp csv file ----
temp_dir <- tempdir()
write_csv(x = df_5102_report, 
          file = file.path(temp_dir,  paste0(out_file, '.csv')))

## set the ckan defaults ----
### get data portal API key 
### (assuming this is saved in the local environment, in a variable called: data_portal_key)
### (it's available on data.ca.gov by going to your user profile)
portal_key <- Sys.getenv('data_portal_key')
resource_id <- 'aba87ad9-f6b0-4a7e-a45e-d1452417eb7f' # https://data.ca.gov/dataset/calhr-civil-rights-data-for-gare-capital-cohort-2019/resource/aba87ad9-f6b0-4a7e-a45e-d1452417eb7f

ckanr_setup(url = 'https://data.ca.gov/', 
            key = portal_key) 

## upload file to portal ----
file_upload <- resource_update(id = resource_id, 
                               path = file.path(temp_dir,  
                                                paste0(out_file, '.csv')))
