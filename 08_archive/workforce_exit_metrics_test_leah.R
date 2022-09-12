#Exploring "exits" from the system by ethnicity ----
##by Leah Jones


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

# # filter for 2020
# df_5102_2020 <- df_5102_all_yrs %>% 
#     filter(as_of_date == as.Date('2020-12-31'))

# Filtering for EPA and related BDOs 
df_5102_all_yrs_epa <- df_5102_all_yrs %>% 
    filter(dept == "Air Resources Board"|
               dept == "Environmental Health Hazard Assessment, Office of"|
               dept == "Environmental Protection Agency"|
               dept == "Pesticide Regulation, Department of"|
               dept == "Resources Recycling and Recovery, Department of"|
               dept == "Toxic Substances Control, Department of"|
               dept == "Water Resources Control Board")

# #Function for summary rates
# fun_summary_5102_l2 <- function(dataset) {
#     dataset %>%
#         add_count(ethnicity_level2,
#                   wt = record_count,
#                   name = 'ethnicity_total') %>%
#         select(ethnicity_level2, ethnicity_total, as_of_date) %>%
#         distinct() %>%
#         mutate(rate = ethnicity_total / sum(ethnicity_total)) %>%
#         mutate(type = 'State Government Workforce') %>%
#         arrange(ethnicity_level2) %>%
#         {.}
# }

#Apply summary rates
summary <- df_5102_all_yrs_epa %>% 
    group_by(as_of_date) %>% 
    add_count(ethnicity_level2,
              wt = record_count,
              name = 'ethnicity_total') %>% 
     select(ethnicity_level2, ethnicity_total, as_of_date) %>%
     distinct()
    # group_by(as_of_date) %>% 
    # mutate(rate = ethnicity_total / sum(ethnicity_total)) %>%
    # mutate(type = 'State Government Workforce') %>%
    # arrange(ethnicity_level2)
 
#Graph it
plot_test <- ggplot(summary, aes(x=as_of_date,
                            y=ethnicity_total,
                            group=ethnicity_level2)) +
    geom_line(aes(color=ethnicity_level2)) +
    geom_point(aes(color=ethnicity_level2))
print(plot_test2)

   
# #Plot function
# fun_plot_5102_l2_line <- function(dataset, plot_title) {
#     dataset %>% 
#         ggplot() +
#         geom_line(mapping = aes(x = as_of_date, 
#                                y = ethnicity_total, 
#                                group = ethnicity_level2), 
#                  stat = 'identity', 
#                  position = 'dodge') +
#         geom_point()
#         # coord_flip() + 
#         # scale_y_continuous(labels = percent) +
#         # labs(title = plot_title,
#         #      x = 'Ethnicity',
#         #      y = 'Portion', 
#         #      caption = 'Workforce data from CalHR 5102') +
#         # theme(legend.position = 'bottom', 
#         #       legend.title = element_blank()) +
#         # guides(fill = guide_legend(reverse = TRUE))
# }
# 
# #Apply rates function to dataset
# (line_pl_5102_calepa_l2 <- summary %>% 
#         # fun_summary_5102_l2() %>% 
#         # bind_rows(acs_data_level2) %>% 
#         fun_plot_5102_l2_line(plot_title = 'All CalEPA Employees Over Time'))

# 
# #Map as a line graph
# plot_epa <- ggplot() +
#     geom_line(data = df_5102_all_yrs_epa, 
#              aes(x = as_of_date, y = ))



