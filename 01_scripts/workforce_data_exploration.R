# load packages -----------------------------------
# (do this each time you start R, and just for the packages you need to use in that session)
    # NOTE: if you don't already have these packages downloaded to your computer, type:
    # install.packages('package_name') and replace package_name with the package you want 
    # to download (but keep the quotes) - you only have to do that once, not each time you start R
library(readxl) # reading data from excel files into R
library(readr)  # reading data from csv files (and other formats) into R
library(dplyr)  # basic data manipulation
library(janitor)    # cleaning column names, and other basic data cleaning tasks
library(here)   # good for defining file paths in a foolproof way (almost)
library(ggplot2)    # plotting
library(stringr)    # working with character strings



# read data into R and re-format -----------------------------------

# if your data is in excel format, use:
df_workforce_data_2020 <- read_excel(path = here('02_data_raw', 
                                                 '5102',
                                                 'calhr-5102-statewide-2020.xlsx'))

    # NOTE: the here() part is a good practice, but not strictly necessary -- this works too (remove the comment symbol to run):
    # df_workforce_data_2020 <- read_excel('02_data_raw/5102 Inline Report_12.31.30.xlsx')

# or, if your data is in csv format, you can use the line below (remove the comment symbol to run):
# df_workforce_data_2020 <- read_csv(file = here('02_data_raw', '5102 Inline Report_12.31.30.csv'))
    ## NOTE: when I manually converted the original excel file into csv format, there was an extra row of 
    ## blank data added to the bottom of the dataset, which I manually deleted prior to loading into R
    ## normally I'd probably just try to work with the data in its original format to avoid that kind of 
    ## issue - otherwise you'd probably want to make sure that step is documented somewhere

# reformat column names to make them easier to work with in R
# (NOTE: if you don't reformat the names, you have to use backticks to refer to 
# the columns that have spaces in their names, e.g.: `Class Title` instead of class_title)
df_workforce_data_2020 <- df_workforce_data_2020 %>% 
    clean_names() # this function is from the janitor package, type: ?clean_names() into the console for details

# view a preview of the dataset in the console - this lets you easily check the field names, types, and a sample of the data
glimpse(df_workforce_data_2020)



# general data cleaning/processing and exploration -----------------------------------

# create a new column with a more generic ethnicity label (called "identity_variable_generic")
df_workforce_data_2020 <- df_workforce_data_2020 %>% 
    mutate(identity_variable_generic = case_when(identity_variable == 'White' ~ 'White',
                                                 str_detect(identity_variable, 'Pacific Islander') ~ 'Pacific Islander',
                                                 str_detect(identity_variable, 'Other or Multiple') ~ 'Other',
                                                 str_detect(identity_variable, 'Native American') ~ 'Native American',
                                                 str_detect(identity_variable, 'Hispanic or Latino') ~ 'Hispanic or Latino',
                                                 str_detect(identity_variable, 'Black or African American') ~ 'Black or African American',
                                                 str_detect(identity_variable, 'Asian') ~ 'Asian', 
                                                 TRUE ~ NA_character_)) # if none of the above conditions are met, assign a NA (ie. NULL) value - shouldn't happen in this case

# if you wanted to save this modified data to a separate csv file, you could do that with:
write_csv(x = df_workforce_data_2020, 
          file = here('03_data_processed', # the output file will go in the folder called '03_data_processed'
                      'workforce_data_2020_processed.csv')) 

# just for reference, get a list/count of all of the classifications in the "Class Title" field
df_class_count <- df_workforce_data_2020 %>% 
    count(class_title) 



# look at the environmental scientist classification -----------------------------------

# create a new data frame with just the records where "Class Title" is "ENVIRONMENTAL SCIENTIST", and 
# arrange by "Record Count" (in descending order)
df_env_sci <- df_workforce_data_2020 %>% 
    filter(class_title == 'ENVIRONMENTAL SCIENTIST') %>% 
    arrange(desc(record_count))

# get the total number of environmental scientists for each combination of the new generic identity 
# variable and gender
df_env_sci_summary <- df_env_sci %>% 
    group_by(identity_variable_generic, gender) %>% 
    summarize(total_n = sum(record_count)) %>% 
    ungroup()



# make a plot -----------------------------------

# (this one is not great, but it illustrates the concept of plotting in R I suppose)
plot_env_sci <- df_workforce_data_2020 %>% 
    filter(class_title == 'ENVIRONMENTAL SCIENTIST') %>% 
    group_by(identity_variable_generic, gender) %>% 
    summarize(total_n = sum(record_count)) %>% 
    ggplot() + # code below this line actually creates the plot
    aes(x = identity_variable_generic, y = total_n, fill = gender) +
    geom_bar(stat = 'identity', position = 'dodge') +
    coord_flip() +
    labs(x = 'Race / Ethnicity', 
         y = 'Number of Employees', 
         title = 'Environmental Scientists In California State Government',
         subtitle = 'Year 2020', 
         caption = "Note: data from 5102 Inline Report", 
         fill = 'Gender') 
# to preview the plot in RStudio, just enter the variable name that holds the plot (should show up in the lower left pane)
plot_env_sci

# save the plot to a separate file (might have to play around with the width/height/dpi settings to get the look right)
ggsave(filename = here('04_plots', # saves the file to the 04_plots folder
                       'plot_environmental_scientist.png'), 
       plot = plot_env_sci, 
       width = 10, 
       height = 4.5, 
       dpi = 125)
