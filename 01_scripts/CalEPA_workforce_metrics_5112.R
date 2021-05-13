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
library(patchwork)

report_year <- 2020

# 1 - load / transform data ---------------------------------------------------------------

## 5112 ----
# pull data from data.ca.gov, this may take a few minutes
# NOTE: change the URL to get data for a different year
df_5112 <- read_csv('https://data.ca.gov/dataset/e620a64f-6b86-4ce0-ab4b-03d06674287b/resource/d308f328-6b5b-41c1-8bc2-a4afcfcee3d1/download/5112-inline-report_12.31.20.csv') %>% 
    clean_names()

# add additional levels for ethnicity grouping
#   ethnicity (in original dataset) is most detailed level
#   level 1 groups all BIPOC together (resulting groups are BIPOC and white)
#   level 2 groups some ethnicity sub-groupings together (Asian group and Pacific Islander group)
df_5112 <- df_5112 %>% 
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
df_5112_epa <- df_5112 %>% 
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
    geom_bar(data = df_5112_epa, 
             aes(ethnicity)) +
    coord_flip()



# 3 - 5112 All-CalEPA rates ----------------------------------------------------------

## define color palette for rate plots
# RColorBrewer::brewer.pal(n = 3, name = 'Paired')
colors_rate_plots <- c('darkblue', 'lightblue', 'gold') # intake outside, intake other agencies, advancements

### detailed ethnic groups ----
# original ethnicity 
sum_5112_detail <- df_5112_epa %>% 
    select(ethnicity, hire_type) %>% 
    add_count(ethnicity, name = 'ethnicity_total') %>% 
    add_count(ethnicity, hire_type, name = 'ethnicity_type_total') %>% 
    mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
    distinct() %>%
    arrange(ethnicity) %>% 
    mutate(data_label = glue('{ethnicity} \n(n = {ethnicity_total})')) %>% 
    mutate(hire_type = factor(hire_type)) %>% 
    # stuff below here is for adding labels to each segment of the bars in the bar charts
    mutate(hire_type = as.factor(hire_type)) %>%  
    mutate(hire_type = fct_relevel(hire_type, 
                                   c('Advancements', 
                                     'Intake: Hires from other State Agencies', 
                                     'Intake: Outside Hires'))) %>% 
    arrange(ethnicity, hire_type) %>% 
    group_by(ethnicity) %>% 
    mutate(label_y = cumsum(rate) - 0.5 * rate) %>% # puts the labels in the middle of each bar
    ungroup() %>%
    {.}

# create ordering for bars (by number of advancements)
ordering_detail <- sum_5112_detail %>%
    filter(hire_type == 'Advancements') %>%
    arrange(desc(rate)) %>%
    pull(ethnicity) %>%
    as.character()
missing_detail <- unique(sum_5112_detail$ethnicity)[!unique(sum_5112_detail$ethnicity) %in% ordering_detail]
ordering_detail <- c(ordering_detail, missing_detail)
# ordering <- sum_5112_detail %>% 
#     filter(hire_type == 'Intake: Outside Hires') %>% 
#     arrange(desc(rate)) %>% 
#     pull(ethnicity) %>% 
#     as.character()
# ordering_2 <- sum_5112_detail %>% 
#     filter(hire_type == 'Intake: Hires from other State Agencies') %>% 
#     arrange(desc(rate)) %>% 
#     pull(ethnicity) %>% 
#     as.character()
# missing_1 <- ordering_2[!ordering_2 %in% ordering]
# ordering <- c(ordering, missing_1)
# missing_2 <- unique(sum_5112_detail$ethnicity)[!unique(sum_5112_detail$ethnicity) %in% ordering]
# ordering <- c(ordering, missing_2)
data_labels_detail <- tibble(ethnicity = ordering_detail) %>%
    left_join(sum_5112_detail %>% distinct(ethnicity, data_label)) %>%
    pull(data_label)

(pl_5112_detail <- sum_5112_detail %>% 
        mutate(ethnicity = as.factor(ethnicity)) %>% 
        mutate(ethnicity = fct_relevel(ethnicity, rev(ordering_detail))) %>%
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity, # data_label, 
                               y = rate, 
                               fill = fct_rev(hire_type)), 
                 stat = 'identity') +
        scale_fill_manual(values = colors_rate_plots) +
        scale_y_continuous(labels = percent) +
        scale_x_discrete(breaks = ordering_detail,
                         labels = data_labels_detail) +
        labs(title = glue('Intake Versus Advancement by Ethnicity for all CalEPA BDOs (Year {report_year})'),
             subtitle = 'Labels represent total number of employees in each grouping',
             x = 'Ethnicity Group',
             y = 'Percent of Total Intakes and Advancements (by Ethnicity Group)', 
             caption = glue('Data Source: {report_year} CalHR 5112 Report')) +
        geom_text(aes(x = ethnicity, # data_label, # 
                      y = label_y,
                      # label = ethnicity_type_total,
                      label = glue('{ethnicity_type_total}'))) +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE)))

ggsave(filename = here('07_slides', '2021-05-14', 'images', '5112_calepa_detail.png'), 
       plot = pl_5112_detail, 
       width = 10, 
       height = 6, 
       dpi = 125
       )



### L1 ethnicity ----
# level 1 ethnicity (white vs BIPOC)
sum_5112_l1 <- df_5112_epa %>% 
    select(ethnicity_level1, hire_type) %>% 
    add_count(ethnicity_level1, name = 'ethnicity_total') %>% 
    add_count(ethnicity_level1, hire_type, name = 'ethnicity_type_total') %>% 
    distinct() %>% 
    mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
    arrange(ethnicity_level1) %>%
    mutate(data_label = glue('{ethnicity_level1} \n(n = {ethnicity_total})')) %>% 
    # stuff below here is for adding labels to each segment of the bars in the bar charts
    mutate(hire_type = as.factor(hire_type)) %>%  
    mutate(hire_type = fct_relevel(hire_type, 
                                   c('Advancements',
                                     'Intake: Hires from other State Agencies', 
                                     'Intake: Outside Hires'))) %>% 
    arrange(ethnicity_level1, hire_type) %>% 
    group_by(ethnicity_level1) %>% 
    mutate(label_y = cumsum(rate) - 0.5 * rate) %>% # puts the labels in the middle of each bar
    ungroup() %>%
    {.}

(pl_5112_l1 <- sum_5112_l1 %>%
        # mutate(data_label = glue('{ethnicity_level1} \n(n = {ethnicity_total})')) %>% 
        mutate(hire_type = fct_rev(hire_type)) %>% 
        ggplot() +
        geom_bar(mapping = aes(x = data_label, 
                               #x = ethnicity_level1,
                               y = rate, 
                               fill = hire_type), 
                 stat = 'identity') +
        scale_y_continuous(labels = percent) +
        labs(title = glue('Intake Versus Advancement by Ethnicity for all CalEPA BDOs (Year {report_year})'),
             subtitle = 'Labels represent total number of employees in each grouping',
             x = 'Ethnicity Group',
             y = 'Percent of Total Intakes and Advancements (by Ethnicity Group)', 
             caption = glue('Data Source: {report_year} CalHR 5112 Report')) +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_fill_manual(values = colors_rate_plots) + 
        # geom_text(aes(x = ethnicity_level1,
        #               y = 1.05,
        #               label = glue('n = {ethnicity_total}'),
        #               hjust = 0.5)) +
        geom_text(aes(x = data_label, # ethnicity_level1,
                      y = label_y,
                      # label = ethnicity_type_total,
                      label = glue('{ethnicity_type_total}'))) +
        coord_flip() + 
        # facet_wrap(~ metrics_group, scales = 'free_y') + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        geom_blank())

ggsave(filename = here('07_slides', '2021-05-14', 'images', '5112_calepa_level1.png'), 
       plot = pl_5112_l1, 
       width = 10, 
       height = 6, 
       dpi = 125
       )

### L2 ethnicity ----
# level 2 ethnicity (white vs BIPOC)
sum_5112_l2 <- df_5112_epa %>% 
    select(ethnicity_level2, hire_type) %>% 
    add_count(ethnicity_level2, name = 'ethnicity_total') %>% 
    add_count(ethnicity_level2, hire_type, name = 'ethnicity_type_total') %>% 
    distinct() %>% 
    mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
    arrange(ethnicity_level2) %>%
    mutate(data_label = glue('{ethnicity_level2} \n(n = {ethnicity_total})')) %>% 
    # stuff below here is for adding labels to each segment of the bars in the bar charts
    mutate(hire_type = as.factor(hire_type)) %>%  
    mutate(hire_type = fct_relevel(hire_type, 
                                   c('Advancements',
                                     'Intake: Hires from other State Agencies', 
                                     'Intake: Outside Hires'))) %>% 
    arrange(ethnicity_level2, hire_type) %>% 
    group_by(ethnicity_level2) %>% 
    mutate(label_y = cumsum(rate) - 0.5 * rate) %>% # puts the labels in the middle of each bar
    ungroup() %>%
    {.}

ordering_l2 <- sum_5112_l2 %>%
    filter(hire_type == 'Advancements') %>%
    arrange(desc(rate)) %>%
    pull(ethnicity_level2) %>%
    as.character()
missing_l2 <- unique(sum_5112_l2$ethnicity_level2)[!unique(sum_5112_l2$ethnicity_level2) %in% ordering_l2]
ordering_l2 <- c(ordering_l2, missing_l2)
data_labels_l2 <- tibble(ethnicity_level2 = ordering_l2) %>%
    left_join(sum_5112_l2 %>% distinct(ethnicity_level2, data_label)) %>%
    pull(data_label)

(pl_5112_l2 <- sum_5112_l2 %>%
        # mutate(data_label = glue('{ethnicity_level1} \n(n = {ethnicity_total})')) %>% 
        mutate(hire_type = fct_rev(hire_type)) %>% 
        mutate(ethnicity_level2 = as.factor(ethnicity_level2)) %>% 
        mutate(ethnicity_level2 = fct_relevel(ethnicity_level2, rev(ordering_l2))) %>%
        ggplot() +
        geom_bar(mapping = aes(# x = data_label, 
                               x = ethnicity_level2,
                               y = rate, 
                               fill = hire_type), 
                 stat = 'identity') +
        scale_y_continuous(labels = percent) +
        labs(title = glue('Intake Versus Advancement by Ethnicity for all CalEPA BDOs (Year {report_year})'),
             subtitle = 'Labels represent total number of employees in each grouping',
             x = 'Ethnicity Group',
             y = 'Percent of Total Intakes and Advancements (by Ethnicity Group)', 
             caption = glue('Data Source: {report_year} CalHR 5112 Report')) +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_fill_manual(values = colors_rate_plots) + 
        scale_x_discrete(breaks = ordering_l2,
                         labels = data_labels_l2) +
        # geom_text(aes(x = ethnicity_level1,
        #               y = 1.05,
        #               label = glue('n = {ethnicity_total}'),
        #               hjust = 0.5)) +
        geom_text(aes(x = ethnicity_level2,
                      y = label_y,
                      # label = ethnicity_type_total,
                      label = glue('{ethnicity_type_total}'))) +
        coord_flip() + 
        # facet_wrap(~ metrics_group, scales = 'free_y') + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        geom_blank())

ggsave(filename = here('07_slides', '2021-05-14', 'images', '5112_calepa_level2.png'), 
       plot = pl_5112_l2, 
       width = 10, 
       height = 6, 
       dpi = 125
       )

# patchwork (combine plots)
pl_5512_patch <- pl_5112_l1 / pl_5112_l2 
pl_5512_patch[[1]] <- pl_5512_patch[[1]] + 
    labs(title = element_blank(),
         subtitle = element_blank(),
         caption = element_blank()) + 
    theme(legend.position = 'none')
pl_5512_patch[[2]] <- pl_5512_patch[[2]] + 
    labs(title = element_blank(),
         subtitle = element_blank(),
         caption = element_blank())
pl_5512_patch <- pl_5512_patch + 
    plot_layout(heights = c(1, 3)) +
    plot_annotation(
        title = glue('Intake Versus Advancement by Ethnicity for all CalEPA BDOs (Year {report_year})'),
        subtitle = 'Labels represent total number of employees in each grouping',
        caption = glue('Data Source: {report_year} CalHR 5112 Report'))

ggsave(filename = here('07_slides', '2021-05-14', 'images', '5112_calepa_level1_2_combined.png'), 
       plot = pl_5512_patch, 
       width = 10, 
       height = 6, 
       dpi = 125
       )

# 4 - 5112 group summaries ------------------------------------------------------
## create groupings ----
df_5112_epa <- df_5112_epa %>% 
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
sum_5112_groups_l1 <- df_5112_epa %>% 
    filter(!is.na(metrics_group)) %>% 
    select(ethnicity_level1, hire_type, metrics_group) %>% 
    add_count(ethnicity_level1, metrics_group, name = 'ethnicity_total') %>% 
    add_count(ethnicity_level1, hire_type, metrics_group, name = 'ethnicity_type_total') %>% 
    add_count(metrics_group, name = 'metrics_group_total') %>% 
    distinct() %>% 
    mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
    # stuff below here is for adding labels to each segment of the bars in the bar charts
    mutate(hire_type = as.factor(hire_type)) %>%  
    mutate(hire_type = fct_relevel(hire_type, 
                                   c('Advancements',
                                     'Intake: Outside Hires', 
                                     'Intake: Hires from other State Agencies'))) %>% 
    arrange(ethnicity_level1, metrics_group, hire_type) %>% 
    group_by(ethnicity_level1, metrics_group) %>% 
    mutate(label_y = cumsum(rate) - 0.5 * rate) %>% # puts the labels in the middle of each bar
    ungroup() %>%
    {.}

(pl_5112_groups_l1 <- sum_5112_groups_l1 %>%
        mutate(data_label = glue('{ethnicity_level1} \n(n = {ethnicity_total})')) %>% 
        mutate(facet_label = glue('{metrics_group} (n = {metrics_group_total})')) %>% 
        mutate(hire_type = fct_rev(hire_type)) %>% 
        # mutate(ethnicity_level1 = factor(ethnicity_level1)) %>% 
        # mutate(ethnicity_level1 = fct_rev(ethnicity_level1)) %>% 
        # mutate(data_label = factor(data_label)) %>% 
        # mutate(data_label = fct_rev(data_label)) %>%
        ggplot() +
        geom_bar(mapping = aes(x = data_label, 
                               # x = ethnicity_level1,
                               y = rate, 
                               fill = hire_type), 
                 stat = 'identity') +
        scale_y_continuous(labels = percent) +
        labs(title = glue('Intake Versus Advancement by Ethnicity for all CalEPA BDOs (Year {report_year})'),
             subtitle = 'Labels represent total number of employees in each grouping',
             x = 'Ethnicity Group',
             y = 'Percent of Total Intakes and Advancements (by Occupation and Ethnicity Group)', 
             caption = glue('Data Source: {report_year} CalHR 5112 Report')) +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_fill_manual(values = colors_rate_plots) + 
        # geom_text(aes(x = ethnicity_level1,
        #               y = 1.05,
        #               label = glue('n = {ethnicity_total}'),
        #               hjust = 0.5)) +
        geom_text(aes(x = data_label, # ethnicity_level1,
                      y = label_y,
                      # label = ethnicity_type_total,
                      label = glue('{ethnicity_type_total}'))) +
        coord_flip() + 
        facet_wrap(~ facet_label, scales = 'free_y') + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        geom_blank())

ggsave(filename = here('07_slides', '2021-05-14', 'images', 
                       '5112_calepa_groups_level1.png'), 
       plot = pl_5112_groups_l1, 
       width = 10, 
       height = 6, 
       dpi = 125
       )

# NOT USED BELOW HERE !!!!!! ----------------------------------------------

# ## L1 summary by group & BDO ----
# sum_5112_groups_l1_BDO <- df_5112_epa %>% 
#     filter(!is.na(metrics_group)) %>% 
#     select(ethnicity_level1, hire_type, metrics_group, department) %>% 
#     add_count(ethnicity_level1, metrics_group, department, name = 'ethnicity_total') %>% 
#     add_count(ethnicity_level1, hire_type, metrics_group, department, name = 'ethnicity_type_total') %>% 
#     distinct() %>% 
#     mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
#     # stuff below here is for adding labels to each segment of the bars in the bar charts
#     mutate(hire_type = as.factor(hire_type)) %>%  
#     mutate(hire_type = fct_relevel(hire_type, 
#                                    c('Intake: Outside Hires', 
#                                      'Intake: Hires from other State Agencies', 
#                                      'Advancements'))) %>% 
#     arrange(department, ethnicity_level1, metrics_group, hire_type) %>% 
#     group_by(department, ethnicity_level1, metrics_group) %>%
#     mutate(label_y = cumsum(rate) - 0.5 * rate) %>% # puts the labels in the middle of each bar
#     ungroup() %>%
#     {.}
# 
# 
# pl_l1_BDO_dept <- 'Environmental Health Hazard Assessment, Office of'
# (pl_l1_BDO <- sum_5112_groups_l1_BDO %>%
#         filter(department == pl_l1_BDO_dept) %>%
#         mutate(data_label = glue('{ethnicity_level1} \n(n = {ethnicity_total})')) %>% 
#         mutate(hire_type = fct_rev(hire_type)) %>% 
#         ggplot() +
#         geom_bar(mapping = aes(x = data_label, 
#                                # x = ethnicity_level1,
#                                y = rate, 
#                                fill = hire_type), 
#                  stat = 'identity') +
#         scale_y_continuous(labels = percent) +
#         labs(title = 'CalEPA Intake Versus Advancements by Occupation Group and Ethnicity (Year 2020)',
#              subtitle = pl_l1_BDO_dept,
#              x = 'Ethnicity',
#              y = 'Percent of Total (by Occupation / Ethnicity Group)', 
#              caption = glue('Data Source: {report_year} CalHR 5112 Report')) +
#         guides(fill = guide_legend(reverse = TRUE)) +
#         scale_fill_manual(values = c('aquamarine4', 'blue', 'darkblue')) +
#         # geom_text(aes(x = ethnicity_level1,
#         #               y = 1.05,
#         #               label = glue('n = {ethnicity_total}'),
#         #               hjust = 0.5)) +
#         # geom_text(aes(x = ethnicity_level1,
#         #               y = label_y,
#         #               # label = ethnicity_type_total,
#         #               label = glue('n = {ethnicity_type_total}'))) +
#         coord_flip() + 
#         facet_wrap(~ metrics_group, scales = 'free_y') + 
#         theme(legend.position = 'bottom', 
#               legend.title = element_blank()#,
#               # panel.margin=unit(.05, "lines"),
#               # panel.border = element_rect(color = "black", fill = NA, size = 1), 
#               # strip.background = element_rect(color = "black", size = 1)
#               ) +
#         geom_blank())



# ## L2 summary by group ----
# sum_groups_l2 <- df_5112_epa %>% 
#     filter(!is.na(metrics_group)) %>% 
#     select(ethnicity_level2, hire_type, metrics_group) %>% 
#     add_count(ethnicity_level2, metrics_group, name = 'ethnicity_total') %>% 
#     add_count(ethnicity_level2, hire_type, metrics_group, name = 'ethnicity_type_total') %>% 
#     distinct() %>% 
#     mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
#     # stuff below here is for adding labels to each segment of the bars in the bar charts
#     mutate(hire_type = as.factor(hire_type)) %>%  
#     mutate(hire_type = fct_relevel(hire_type, 
#                                    c('Intake: Outside Hires', 
#                                      'Intake: Hires from other State Agencies', 
#                                      'Advancements'))) %>% 
#     arrange(metrics_group, ethnicity_level2, hire_type) %>% 
#     {.}
# 
# 
# (pl_groups_l2 <- sum_groups_l2 %>%
#         mutate(data_label = glue('{ethnicity_level2} \n(n = {ethnicity_total})')) %>% 
#         mutate(hire_type = fct_rev(hire_type)) %>% 
#         ggplot() +
#         geom_bar(mapping = aes(x = data_label, #ethnicity_level2,  
#                                # x = ethnicity_level1,
#                                y = rate, 
#                                fill = hire_type), 
#                  stat = 'identity') +
#         scale_y_continuous(labels = percent) +
#         # scale_fill_brewer(palette = "Dark2") + 
#         scale_fill_manual(values = colors_rate_plots) +
#         labs(title = 'CalEPA Intake Versus Advancements by Occupation Group and Ethnicity (Year 2020)',
#              subtitle = 'Includes all CalEPA BDOs',
#              x = 'Ethnicity',
#              y = 'Percent of Total (by Occupation / Ethnicity Group)', 
#              caption = glue('Data Source: {report_year} CalHR 5112 Report')) +
#         guides(fill = guide_legend(reverse = TRUE)) +
#         # geom_text(aes(x = ethnicity_level2, # data_label,
#         #               y = 1.05,
#         #               label = glue('n = {ethnicity_total}'),
#         #               hjust = 0.5)) +
#         # geom_text(aes(x = ethnicity_level1,
#         #               y = label_y,
#         #               # label = ethnicity_type_total,
#         #               label = glue('n = {ethnicity_type_total}'))) +
#         coord_flip() + 
#         facet_wrap(~ metrics_group, scales = 'free_y') + 
#         theme(legend.position = 'bottom', 
#               legend.title = element_blank()) +
#         geom_blank())



# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ## 4.2 Individual Group Plots ----
# ## summary function 
# fun_summary_5112_l1 <- function(dataset) {
#     dataset %>% 
#         select(ethnicity_level1, hire_type) %>% 
#         add_count(ethnicity_level1, name = 'ethnicity_total') %>% 
#         add_count(ethnicity_level1, hire_type, name = 'ethnicity_type_total') %>% 
#         distinct() %>% 
#         mutate(rate = ethnicity_type_total / ethnicity_total) %>% 
#         arrange(ethnicity_level1) %>%
#         {.} 
# }
# 
# ## plot function
# fun_plot_5112_l1 <- function(dataset, plot_title) {
#     dataset %>% 
#         mutate(data_label = glue('{ethnicity_level1} \n(n = {ethnicity_total})')) %>% 
#         ggplot() +
#         geom_bar(mapping = aes(x = data_label, # ethnicity_level1, 
#                                y = rate, 
#                                fill = hire_type), 
#                  stat = 'identity') +
#         scale_fill_manual(values = colors_rate_plots) +
#         scale_y_continuous(labels = percent) +
#         labs(title = plot_title,
#              x = 'Ethnicity',
#              y = 'Portion') +
#         coord_flip() + 
#         theme(legend.position = 'bottom', 
#               legend.title = element_blank()) +
#         guides(fill = guide_legend(reverse = TRUE)) + 
#         # geom_text(data = dataset,
#         #           stat = 'identity',
#         #           aes(x = ethnicity_level1,
#         #               y = 1 + 0.1,
#         #               label = glue('n = {ethnicity_total}'))
#         #           ) +
#         geom_blank()
# }
# 
# ### Legal ----
# ### plot
# (pl_5112_l1_legal <- df_5112_epa %>% 
#         filter(soc_major_group_title == 'Legal Occupations') %>% 
#         fun_summary_5112_l1() %>% 
#         fun_plot_5112_l1(plot_title = 'Legal - Portion of Hires / Advancements'))
# 
# 
# ### Management ----
# ### plot
# (pl_5112_l1_mgmt <- df_5112_epa %>% 
#         filter(soc_major_group_title == 'Management Occupations') %>%
#         fun_summary_5112_l1() %>% 
#         fun_plot_5112_l1(plot_title = 'Management - Portion of Hires / Advancements'))
# 
# 
# ### Admin ----
# ### plot
# (pl_5112_l1_admin <- df_5112_epa %>% 
#         filter(soc_major_group_title == 'Office and Administrative Support Occupations' |
#                    (soc_major_group_title == 'Computer and Mathematical Occupations' & 
#                         soc_detailed_group_title == 'Unmapped Classes')) %>% 
#         fun_summary_5112_l1() %>% 
#         fun_plot_5112_l1(plot_title = 'Administrative - Portion of Hires / Advancements'))
# 
# 
# ### Tech ----
# ### plot
# (pl_5112_l1_tech <- df_5112_epa %>% 
#         filter(soc_major_group_title == 'Architecture and Engineering Occupations' |
#                    soc_major_group_title == 'Life, Physical, and Social Science Occupations' | 
#                    (soc_major_group_title == 'Computer and Mathematical Occupations' & 
#                         soc_detailed_group_title == 'Operations Research Analysts')) %>% 
#         fun_summary_5112_l1() %>% 
#         fun_plot_5112_l1(plot_title = 'Tech - Portion of Hires / Advancements'))
