# resources ----
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


# 1 - load / transform data ---------------------------------------------------------------

## 5112 ----
# pull data from data.ca.gov, this may take a few minutes
df_5112_2020 <- read_csv('https://data.ca.gov/dataset/e620a64f-6b86-4ce0-ab4b-03d06674287b/resource/d308f328-6b5b-41c1-8bc2-a4afcfcee3d1/download/5112-inline-report_12.31.20.csv') %>% 
    clean_names()

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




    


# 2 - 5112 exploration ----------------------------------------------------------------
## count of entries by race ----
ggplot() +
    geom_bar(data = df_5112_2020_epa, 
             aes(ethnicity)) +
    coord_flip()


# 3 - 5112 rates ----------------------------------------------------------

## define color palette for rate plots
# RColorBrewer::brewer.pal(n = 3, name = 'Paired')
colors_rate_plots <- c('gold', 'lightblue', 'darkblue')
    
## calculate rates ----

# original ethnicity 
summary_rates <- df_5112_2020_epa %>% 
    select(ethnicity, hire_type) %>% 
    add_count(ethnicity, name = 'ethnicity_total') %>% 
    add_count(ethnicity, hire_type, name = 'ethnicity_type_total') %>% 
    mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
    distinct() %>%
    arrange(ethnicity) %>% 
    mutate(data_label = glue('{ethnicity} \n(n = {ethnicity_total})')) %>% 
    {.}
    

# level 1 ethnicity (white vs BIPOC)
summary_rates_level1 <- df_5112_2020_epa %>% 
    select(ethnicity_level1, hire_type) %>% 
    add_count(ethnicity_level1, name = 'ethnicity_total') %>% 
    add_count(ethnicity_level1, hire_type, name = 'ethnicity_type_total') %>% 
    distinct() %>% 
    mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
    arrange(ethnicity_level1) %>%
    mutate(data_label = glue('{ethnicity_level1} \n(n = {ethnicity_total})')) %>% 
    {.}


## plot rates ----
# ### advancement ----#
# (plot_advancement <- summary_rates %>% 
#     filter(hire_type == 'Advancements') %>% 
#     # fct_reorder()
#     ggplot() +
#     geom_bar(aes(x = ethnicity, y = rate), stat = 'identity') +
#     coord_flip() + 
#     scale_y_continuous(labels = percent) +
#     labs(title = 'Rate of Advancement by Ethnicity',
#          x = 'Ethnicity',
#          y = 'Rate'))
# 
# ### intake (external) ----#
# (plot_intake_outside <- summary_rates %>% 
#     filter(hire_type == 'Intake: Outside Hires') %>% 
#     # fct_reorder()
#     ggplot() +
#     geom_bar(aes(x = ethnicity, y = rate), stat = 'identity') +
#     coord_flip() + 
#     scale_y_continuous(labels = percent) +
#     labs(title = 'Rate of External Hires by Ethnicity',
#          x = 'Ethnicity',
#          y = 'Rate'))
# 
# ### intake (other agency) ----#
# (plot_intake_other_agencies <- summary_rates %>% 
#     filter(hire_type == 'Intake: Hires from other State Agencies') %>% 
#     # fct_reorder()
#     ggplot() +
#     geom_bar(aes(x = ethnicity, y = rate), stat = 'identity') +
#     coord_flip() + 
#     scale_y_continuous(labels = percent) +
#     labs(title = 'Rate of Hires from Other State Agencies by Ethnicity',
#          x = 'Ethnicity',
#          y = 'Rate'))

### detailed ethnic groups ----
ordering <- summary_rates %>% 
    filter(hire_type == 'Intake: Outside Hires') %>% 
    arrange(desc(rate)) %>% 
    pull(ethnicity) %>% 
    as.character()
ordering_2 <- summary_rates %>% 
    filter(hire_type == 'Intake: Hires from other State Agencies') %>% 
    arrange(desc(rate)) %>% 
    pull(ethnicity) %>% 
    as.character()
missing_1 <- ordering_2[!ordering_2 %in% ordering]
ordering <- c(ordering, missing_1)
missing_2 <- unique(summary_rates$ethnicity)[!unique(summary_rates$ethnicity) %in% ordering]
ordering <- c(ordering, missing_2)
data_labels <- tibble(ethnicity = ordering) %>% 
    left_join(summary_rates %>% distinct(ethnicity, data_label)) %>% 
    pull(data_label)

(plot_5112_rates_combined <- summary_rates %>% 
        mutate(ethnicity = as.factor(ethnicity)) %>% 
        mutate(ethnicity = fct_relevel(ethnicity, rev(ordering))) %>%  
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity, # data_label, 
                               y = rate, 
                               fill = hire_type), 
                 stat = 'identity') +
        scale_fill_manual(values = colors_rate_plots) +
        scale_y_continuous(labels = percent) +
        scale_x_discrete(breaks = ordering,
                         labels = data_labels) +
        labs(title = 'Portion of Hires / Advancements',
             x = 'Ethnicity',
             y = 'Portion') +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE)))

### combined L1 ethnicity ----
(plot_5112_rates_combined_level1 <- summary_rates_level1 %>% 
     ggplot() +
     geom_bar(mapping = aes(x = data_label, # ethnicity_level1, 
                            y = rate, 
                            fill = hire_type), 
              stat = 'identity') +
     scale_fill_manual(values = colors_rate_plots) +
     coord_flip() + 
     scale_y_continuous(labels = percent) +
     labs(title = 'Portion of Hires / Advancements',
          x = 'Ethnicity',
          y = 'Portion') +
     theme(legend.position = 'bottom', 
           legend.title = element_blank()) +
     guides(fill = guide_legend(reverse = TRUE)))



# 5 - 5112 group summaries ------------------------------------------------------
## create groupings ----
df_5112_2020_epa <- df_5112_2020_epa %>% 
    mutate(metrics_group = case_when(
        # legal
        soc_major_group_title == 'Legal Occupations' ~ 
            'Legal Occupations', 
        # management
        soc_major_group_title == 'Management Occupations' ~ 
            'Management Occupations', 
        # admin
        soc_major_group_title == 'Office and Administrative Support Occupations' |
            (soc_major_group_title == 'Computer and Mathematical Occupations' & 
                 soc_detailed_group_title == 'Unmapped Classes') ~ 
            'Administrative Occupations',
        # technical
        soc_major_group_title == 'Architecture and Engineering Occupations' |
            soc_major_group_title == 'Life, Physical, and Social Science Occupations' | 
            (soc_major_group_title == 'Computer and Mathematical Occupations' & 
                 soc_detailed_group_title == 'Operations Research Analysts') ~ 
            'Technical Occupations')
    )

## L1 summary by group ----
sum_5112_groups_l1 <- df_5112_2020_epa %>% 
    filter(!is.na(metrics_group)) %>% 
    select(ethnicity_level1, hire_type, metrics_group) %>% 
    add_count(ethnicity_level1, metrics_group, name = 'ethnicity_total') %>% 
    add_count(ethnicity_level1, hire_type, metrics_group, name = 'ethnicity_type_total') %>% 
    distinct() %>% 
    mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
    # stuff below here is for adding labels to each segment of the bars in the bar charts
    mutate(hire_type = as.factor(hire_type)) %>%  
    mutate(hire_type = fct_relevel(hire_type, 
                                   c('Intake: Outside Hires', 
                                     'Intake: Hires from other State Agencies', 
                                     'Advancements'))) %>% 
    arrange(ethnicity_level1, metrics_group, hire_type) %>% 
    group_by(ethnicity_level1, metrics_group) %>% 
    mutate(label_y = cumsum(rate) - 0.5 * rate) %>% # puts the labels in the middle of each bar
    ungroup() %>%
    {.}

(pl_5112_groups_l1 <- sum_5112_groups_l1 %>%
        mutate(data_label = glue('{ethnicity_level1} \n(n = {ethnicity_total})')) %>% 
        mutate(hire_type = fct_rev(hire_type)) %>% 
        ggplot() +
        geom_bar(mapping = aes(x = data_label, 
                               # x = ethnicity_level1,
                               y = rate, 
                               fill = hire_type), 
                 stat = 'identity') +
        scale_y_continuous(labels = percent) +
        labs(title = 'CalEPA Intake Versus Advancements by Occupation Group and Ethicity (Year 2020)',
             subtitle = 'Includes all CalEPA Boards, Departments, and Offices',
             x = 'Ethnicity',
             y = 'Percent of Total (by Occupation / Ethnicity Group)', 
             caption = 'Workforce data from CalHR 5112 report') +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_fill_manual(values = colors_rate_plots) + 
        # geom_text(aes(x = ethnicity_level1,
        #               y = 1.05,
        #               label = glue('n = {ethnicity_total}'),
        #               hjust = 0.5)) +
        # geom_text(aes(x = ethnicity_level1,
        #               y = label_y,
        #               # label = ethnicity_type_total,
        #               label = glue('n = {ethnicity_type_total}'))) +
        coord_flip() + 
        facet_wrap(~ metrics_group, scales = 'free_y') + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()#,
              # panel.margin=unit(.05, "lines"),
              # panel.border = element_rect(color = "black", fill = NA, size = 1), 
              # strip.background = element_rect(color = "black", size = 1)
              ) +
        geom_blank())

## L1 summary by group & BDO ----
sum_5112_groups_l1_BDO <- df_5112_2020_epa %>% 
    filter(!is.na(metrics_group)) %>% 
    select(ethnicity_level1, hire_type, metrics_group, department) %>% 
    add_count(ethnicity_level1, metrics_group, department, name = 'ethnicity_total') %>% 
    add_count(ethnicity_level1, hire_type, metrics_group, department, name = 'ethnicity_type_total') %>% 
    distinct() %>% 
    mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
    # stuff below here is for adding labels to each segment of the bars in the bar charts
    mutate(hire_type = as.factor(hire_type)) %>%  
    mutate(hire_type = fct_relevel(hire_type, 
                                   c('Intake: Outside Hires', 
                                     'Intake: Hires from other State Agencies', 
                                     'Advancements'))) %>% 
    arrange(department, ethnicity_level1, metrics_group, hire_type) %>% 
    group_by(department, ethnicity_level1, metrics_group) %>%
    mutate(label_y = cumsum(rate) - 0.5 * rate) %>% # puts the labels in the middle of each bar
    ungroup() %>%
    {.}


pl_l1_BDO_dept <- 'Environmental Health Hazard Assessment, Office of'
(pl_l1_BDO <- sum_5112_groups_l1_BDO %>%
        filter(department == pl_l1_BDO_dept) %>%
        mutate(data_label = glue('{ethnicity_level1} \n(n = {ethnicity_total})')) %>% 
        mutate(hire_type = fct_rev(hire_type)) %>% 
        ggplot() +
        geom_bar(mapping = aes(x = data_label, 
                               # x = ethnicity_level1,
                               y = rate, 
                               fill = hire_type), 
                 stat = 'identity') +
        scale_y_continuous(labels = percent) +
        labs(title = 'CalEPA Intake Versus Advancements by Occupation Group and Ethicity (Year 2020)',
             subtitle = pl_l1_BDO_dept,
             x = 'Ethnicity',
             y = 'Percent of Total (by Occupation / Ethnicity Group)', 
             caption = 'Workforce data from CalHR 5102 report') +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_fill_manual(values = c('aquamarine4', 'blue', 'darkblue')) +
        # geom_text(aes(x = ethnicity_level1,
        #               y = 1.05,
        #               label = glue('n = {ethnicity_total}'),
        #               hjust = 0.5)) +
        # geom_text(aes(x = ethnicity_level1,
        #               y = label_y,
        #               # label = ethnicity_type_total,
        #               label = glue('n = {ethnicity_type_total}'))) +
        coord_flip() + 
        facet_wrap(~ metrics_group, scales = 'free_y') + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()#,
              # panel.margin=unit(.05, "lines"),
              # panel.border = element_rect(color = "black", fill = NA, size = 1), 
              # strip.background = element_rect(color = "black", size = 1)
              ) +
        geom_blank())



## L2 summary by group ----
sum_groups_l2 <- df_5112_2020_epa %>% 
    filter(!is.na(metrics_group)) %>% 
    select(ethnicity_level2, hire_type, metrics_group) %>% 
    add_count(ethnicity_level2, metrics_group, name = 'ethnicity_total') %>% 
    add_count(ethnicity_level2, hire_type, metrics_group, name = 'ethnicity_type_total') %>% 
    distinct() %>% 
    mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
    # stuff below here is for adding labels to each segment of the bars in the bar charts
    mutate(hire_type = as.factor(hire_type)) %>%  
    mutate(hire_type = fct_relevel(hire_type, 
                                   c('Intake: Outside Hires', 
                                     'Intake: Hires from other State Agencies', 
                                     'Advancements'))) %>% 
    arrange(metrics_group, ethnicity_level2, hire_type) %>% 
    {.}


(pl_groups_l2 <- sum_groups_l2 %>%
        mutate(data_label = glue('{ethnicity_level2} \n(n = {ethnicity_total})')) %>% 
        mutate(hire_type = fct_rev(hire_type)) %>% 
        ggplot() +
        geom_bar(mapping = aes(x = data_label, #ethnicity_level2,  
                               # x = ethnicity_level1,
                               y = rate, 
                               fill = hire_type), 
                 stat = 'identity') +
        scale_y_continuous(labels = percent) +
        # scale_fill_brewer(palette = "Dark2") + 
        scale_fill_manual(values = colors_rate_plots) +
        labs(title = 'CalEPA Intake Versus Advancements by Occupation Group and Ethicity (Year 2020)',
             subtitle = 'Includes all CalEPA Boards, Departments, and Offices',
             x = 'Ethnicity',
             y = 'Percent of Total (by Occupation / Ethnicity Group)', 
             caption = 'Workforce data from CalHR 5102 report') +
        guides(fill = guide_legend(reverse = TRUE)) +
        # geom_text(aes(x = ethnicity_level2, # data_label,
        #               y = 1.05,
        #               label = glue('n = {ethnicity_total}'),
        #               hjust = 0.5)) +
        # geom_text(aes(x = ethnicity_level1,
        #               y = label_y,
        #               # label = ethnicity_type_total,
        #               label = glue('n = {ethnicity_type_total}'))) +
        coord_flip() + 
        facet_wrap(~ metrics_group, scales = 'free_y') + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        geom_blank())



# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## 5.2 Individual Group Plots ----
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
        mutate(data_label = glue('{ethnicity_level1} \n(n = {ethnicity_total})')) %>% 
        ggplot() +
        geom_bar(mapping = aes(x = data_label, # ethnicity_level1, 
                               y = rate, 
                               fill = hire_type), 
                 stat = 'identity') +
        scale_fill_manual(values = colors_rate_plots) +
        scale_y_continuous(labels = percent) +
        labs(title = plot_title,
             x = 'Ethnicity',
             y = 'Portion') +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE)) + 
        # geom_text(data = dataset,
        #           stat = 'identity',
        #           aes(x = ethnicity_level1,
        #               y = 1 + 0.1,
        #               label = glue('n = {ethnicity_total}'))
        #           ) +
        geom_blank()
}

### Legal ----
### plot
(pl_5112_l1_legal <- df_5112_2020_epa %>% 
        filter(soc_major_group_title == 'Legal Occupations') %>% 
        fun_summary_5112_l1() %>% 
        fun_plot_5112_l1(plot_title = 'Legal - Portion of Hires / Advancements'))


### Management ----
### plot
(pl_5112_l1_mgmt <- df_5112_2020_epa %>% 
        filter(soc_major_group_title == 'Management Occupations') %>%
        fun_summary_5112_l1() %>% 
        fun_plot_5112_l1(plot_title = 'Management - Portion of Hires / Advancements'))


### Admin ----
### plot
(pl_5112_l1_admin <- df_5112_2020_epa %>% 
        filter(soc_major_group_title == 'Office and Administrative Support Occupations' |
                   (soc_major_group_title == 'Computer and Mathematical Occupations' & 
                        soc_detailed_group_title == 'Unmapped Classes')) %>% 
        fun_summary_5112_l1() %>% 
        fun_plot_5112_l1(plot_title = 'Administrative - Portion of Hires / Advancements'))


### Tech ----
### plot
(pl_5112_l1_tech <- df_5112_2020_epa %>% 
        filter(soc_major_group_title == 'Architecture and Engineering Occupations' |
                   soc_major_group_title == 'Life, Physical, and Social Science Occupations' | 
                   (soc_major_group_title == 'Computer and Mathematical Occupations' & 
                        soc_detailed_group_title == 'Operations Research Analysts')) %>% 
        fun_summary_5112_l1() %>% 
        fun_plot_5112_l1(plot_title = 'Tech - Portion of Hires / Advancements'))



# 6 - exits (by job class) ----------------------------------------------------
job_class <- 'ENVIRONMENTAL SCIENTIST'

job_class_flux <- df_5112_2020_epa %>% 
    filter(class_title == all_of(job_class))

job_class_timeline <- df_5102_epa %>% 
    filter(class_title == job_class)
