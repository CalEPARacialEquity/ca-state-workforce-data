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


# 1 - load/transform data ---------------------------------------------------------------

## 5112 ----
# pull data from data.ca.gov, this may take a few minutes
df_5112_2020 <- read_csv('https://data.ca.gov/dataset/e620a64f-6b86-4ce0-ab4b-03d06674287b/resource/d308f328-6b5b-41c1-8bc2-a4afcfcee3d1/download/5112-inline-report_12.31.20.csv') %>% 
    clean_names()

# Add year column to dataset
df_5112_2020 <- df_5112_2020 %>% 
    mutate(as_of_date = as.Date("2020-12-31"))

# add additional levels for ethnicity grouping
df_5112_2020 <- df_5112_2020 %>% 
    mutate(ethnicity_level1 = case_when(ethnicity == 'White' ~ 'White', 
                                        TRUE ~ 'BIPOC'))

# Filtering for EPA and related BDOs 
df_5112_2020_epa <- df_5112_2020 %>% 
    filter(department == "Air Resources Board"|
               department == "Environmental Health Hazard Assessment, Office of"|
               department == "Environmental Protection Agency"|
               department == "Pesticide Regulation, department of"|
               department == "Resources Recycling and Recovery, department of"|
               department == "Toxic Substances Control, department of"|
               department == "Water Resources Control Board")


## 5102 ----
df_5102_all_yrs <- readr::read_csv('https://data.ca.gov/dataset/e620a64f-6b86-4ce0-ab4b-03d06674287b/resource/aba87ad9-f6b0-4a7e-a45e-d1452417eb7f/download/calhr_5102_statewide_2011-2020.csv') %>%
    clean_names() 

# filter for 2020
df_5102_2020 <- df_5102_all_yrs %>% 
    filter(as_of_date == as.Date('2020-12-31'))

# Filtering for EPA and related BDOs 
df_5102_2020_epa <- df_5102_2020 %>% 
    filter(dept == "Air Resources Board"|
               dept == "Environmental Health Hazard Assessment, Office of"|
               dept == "Environmental Protection Agency"|
               dept == "Pesticide Regulation, dept of"|
               dept == "Resources Recycling and Recovery, dept of"|
               dept == "Toxic Substances Control, dept of"|
               dept == "Water Resources Control Board")


# 2 - 5112 exploration ----------------------------------------------------------------
## 5112 - count of entries by race ----
plot_1 <- ggplot() +
    geom_bar(data = df_5112_2020_epa, 
             aes(ethnicity)) +
    coord_flip()


# 3 - 5112 rates ----------------------------------------------------------

## calculate rates ----
    # # Old method
    # counts_ethnicity <- df_5112_2020_epa %>% 
    #     count(ethnicity, name = 'ethnicity_total')
    # 
    # summary_rates <- df_5112_2020_epa %>% 
    #     count(ethnicity, hire_type) %>% 
    #     left_join(counts_ethnicity) %>% 
    #     mutate(rate = n / ethnicity_total)

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



# 4 - 5102 summaries ----------------------------------------------------------

# summary function 
fun_summary_5102 <- function(dataset) {
    dataset %>% 
        select(identity_variable) %>% 
        add_count(identity_variable, name = 'ethnicity_total') %>% 
        distinct() %>% 
        mutate(rate = ethnicity_total / sum(ethnicity_total)) %>%
        mutate(type = 'State Government Workforce') %>% 
        arrange(identity_variable) %>%
        {.}
}

# plot function
fun_plot_5102 <- function(dataset, plot_title) {
    dataset %>% 
        ggplot() +
        geom_bar(mapping = aes(x = identity_variable, 
                               y = rate, 
                               fill = type), 
                 stat = 'identity', 
                 position = 'dodge') +
        coord_flip() + 
        scale_y_continuous(labels = percent) +
        labs(title = plot_title,
             x = 'Ethnicity',
             y = 'Portion') +
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}

## legal ----
summary_rates_legal_5102 <- df_5102_2020_epa %>% 
    filter(employee_category == 'Legal Occupations') %>%  
    fun_summary_5102()

### dummy data (all of CA)
summary_rates_legal_CA <- summary_rates_legal_5102 %>% 
    mutate(type = 'State Population')

### combine workforce / population data
summary_rates_legal_5102 <- summary_rates_legal_5102 %>% 
    bind_rows(summary_rates_legal_CA)

### plot
(plot_5102_legal <- fun_plot_5102(dataset = summary_rates_legal_5102, 
                                 plot_title = 'Portion of Total'))





# 5 - 5112 summaries ------------------------------------------------------
## summary function 
fun_summary_5112 <- function(dataset) {
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
fun_plot_5112 <- function(dataset, plot_title) {
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
        guides(fill = guide_legend(reverse = TRUE))
}

## legal ----
summary_rates_level1_legal <- df_5112_2020_epa %>% 
    filter(soc_major_group_title == 'Legal Occupations') %>% 
    fun_summary_5112()

### plot
(plot_legal_5112_rates_combined_level1 <- summary_rates_level1_legal %>% 
    fun_plot_5112(plot_title = 'Legal - Portion of Hires / Advancements'))


## Management ----
summary_rates_level1_management <- df_5112_2020_epa %>% 
    filter(soc_major_group_title == 'Management Occupations') %>%
    fun_summary_5112()

### plot
(plot_mgmt_5112_rates_combined_level1 <- summary_rates_level1_management %>% 
    fun_plot_5112(plot_title = 'Management - Portion of Hires / Advancements'))


## Admin ----
summary_rates_level1_admin <- df_5112_2020_epa %>% 
    filter(soc_major_group_title == 'Office and Administrative Support Occupations' |
               (soc_major_group_title == 'Computer and Mathematical Occupations' & 
                    soc_detailed_group_title == 'Unmapped Classes')) %>% 
    fun_summary_5112()

### plot
(plot_admin_5112_rates_combined_level1 <- summary_rates_level1_admin %>% 
    fun_plot_5112(plot_title = 'Administrative - Portion of Hires / Advancements'))


## Tech ----
summary_rates_level1_tech <- df_5112_2020_epa %>% 
    filter(soc_major_group_title == 'Architecture and Engineering Occupations' |
               soc_major_group_title == 'Life, Physical, and Social Science Occupations' | 
               (soc_major_group_title == 'Computer and Mathematical Occupations' & 
                    soc_detailed_group_title == 'Operations Research Analysts')) %>% 
    fun_summary_5112()

### plot
(plot_tech_5112_rates_combined_level1 <- summary_rates_level1_tech %>% 
    fun_plot_5112(plot_title = 'Tech - Portion of Hires / Advancements'))
