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
library(patchwork)



# enter parameters (for current reporting year) -----------------------------
report_year <- 2020
acs_data_year <- 2019

url_data_all_yrs <- 'https://data.ca.gov/dataset/e620a64f-6b86-4ce0-ab4b-03d06674287b/resource/aba87ad9-f6b0-4a7e-a45e-d1452417eb7f/download/calhr_5102_statewide_2011-2020.csv'

default_pl_title <- glue('CalEPA Workforce Compared to State Population (Year {report_year})')
default_pl_title_groups <- glue('CalEPA Workforce by Occupation and Ethnicity Group Compared to State Population (Year {report_year})')
default_pl_title_bdo <- glue('CalEPA Workforce by BDO Compared to State Population (Year {report_year})')

default_pl_subtitle <- element_blank()
default_pl_caption <- glue('Data sources: state population data from {acs_data_year} 1 yr American Community Survey  |  workforce data from {report_year} CalHR 5102 Report')


# 1 - load / transform data ---------------------------------------------------------------

## 5102 ----
df_5102 <- read_csv(url_data_all_yrs) %>%
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

# add a year column
df_5102 <- df_5102 %>% 
    mutate(reporting_year = year(as_of_date))


# Filtering for EPA and related BDOs 
df_5102_epa <- df_5102 %>% 
    filter(dept == "Air Resources Board"|
               dept == "Environmental Health Hazard Assessment, Office of"|
               dept == "Environmental Protection Agency"|
               dept == "Pesticide Regulation, Department of"|
               dept == "Resources Recycling and Recovery, Department of"|
               dept == "Toxic Substances Control, Department of"|
               dept == "Water Resources Control Board")

# filter for data from the report year (selected above)
df_5102_epa_1yr <- df_5102_epa %>% 
    filter(reporting_year == report_year)


## census data (ACS 1 yr) ----
### view all variables
# acs_variables <- load_variables(acs_data_year, 'acs1', cache = TRUE)
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
                        year = acs_data_year)

# clean / reformat the acs data (the data will be formatted to be 
# consistent with the "level 2" ethnicity groupings in the workforce 
# data that are created above)
acs_data_level2 <- acs_data_raw %>% 
    clean_names() %>% 
    rename(total_state_pop = summary_est, 
           total_state_pop_moe = summary_moe,
           location_name = name,
           ethnicity_level2 = variable)

    ## check (should be TRUE) - make sure sum of populations by group == total state population
    # sum(acs_data_level2$estimate) == acs_data_level2$total_state_pop[1]

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
    
    ## check (should be TRUE) - make sure sum of populations by group == total state population
    # sum(acs_data_level2$estimate) == acs_data_level2$total_state_pop[1]
    
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
    
    ## check 
    # sum(acs_data_level1$estimate) == acs_data_level1$total_state_pop[1]


    
    

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 2 - 5102 summaries (vs CA pop) -----------------------------------------------
## define color palette for plots
### state pop vs CalEPA workforce plots
colors_5102_state_epa <-c('gold', 'darkgreen') # state pop / CalEPA workforce
### colors for ethnicities (in stacked bar plots) - trying to keep 'white' the same for both levels of plots for consistency/comparison
brewer_colors <- rev(RColorBrewer::brewer.pal(n = 7, name = 'Set2'))
colors_5102_2 <- brewer_colors[6:7] # BIPOC / White
colors_5102_7 <- brewer_colors # detailed ethnic groups: Asian ... White
    
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
fun_plot_5102_l1 <- function(dataset, plot_title, plot_caption) {
    dataset %>% 
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity_level1, 
                               y = rate, 
                               fill = factor(type, levels = rev(levels(type)))), 
                 stat = 'identity', 
                 position = 'dodge') +
        scale_fill_manual(values = colors_5102_state_epa) +
        scale_y_continuous(labels = label_percent(accuracy = 1L)) +
        labs(title = plot_title,
             x = 'Ethnicity Group',
             y = 'Percent of Total', 
             caption = plot_caption) +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}

fun_plot_5102_l1_stack <- function(dataset, plot_title, plot_caption) {
    dataset %>% 
        mutate(type = fct_rev(type)) %>% 
        ggplot() +
        geom_bar(mapping = aes(x = type, # ethnicity_level1, 
                               y = rate, 
                               fill = ethnicity_level1), # factor(type, levels = rev(levels(type)))), 
                 stat = 'identity') +
        scale_fill_manual(values = colors_5102_2) +
        scale_y_continuous(labels = label_percent(accuracy = 1L)) +
        labs(title = plot_title,
             x = element_blank(),
             y = 'Percent of Total', 
             caption = plot_caption) + 
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}


fun_plot_5102_l2 <- function(dataset, plot_title, plot_caption) {
    dataset %>% 
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity_level2, 
                               y = rate, 
                               fill = factor(type, levels = rev(levels(type)))), 
                 stat = 'identity', 
                 position = 'dodge') +
        scale_fill_manual(values = colors_5102_state_epa) +
        scale_y_continuous(labels = label_percent(accuracy = 1L)) +
        labs(title = plot_title,
             x = 'Ethnicity Group',
             y = 'Percent of Total', 
             caption = plot_caption) +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}

fun_plot_5102_l2_stack <- function(dataset, plot_title, plot_caption) {
    dataset %>% 
        mutate(type = fct_rev(type)) %>%
        ggplot() +
        geom_bar(mapping = aes(x = type, 
                               y = rate, 
                               fill = ethnicity_level2), # factor(type, levels = rev(levels(type)))), 
                 stat = 'identity') +
        scale_fill_manual(values = colors_5102_7) +
        scale_y_continuous(labels = label_percent(accuracy = 1L)) +
        labs(title = plot_title,
             x = element_blank(),
             y = 'Percent of Total', 
             caption = plot_caption) +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}

## 2.1 - CalEPA summary ----
### L1 ----
(pl_5102_calepa_l1 <- df_5102_epa_1yr %>% 
     fun_summary_5102_l1() %>% 
     bind_rows(acs_data_level1) %>% 
     fun_plot_5102_l1(plot_title = default_pl_title, 
                      plot_caption = default_pl_caption))

(pl_5102_calepa_l1_stack <- df_5102_epa_1yr %>% 
        fun_summary_5102_l1() %>% 
        bind_rows(acs_data_level1) %>% 
        fun_plot_5102_l1_stack(plot_title = default_pl_title, 
                               plot_caption = default_pl_caption))

ggsave(filename = here('07_slides', '2021-05-14', 'images', 
                       '5102_calepa_level1_stack.png'), 
       plot = pl_5102_calepa_l1_stack, 
       width = 10, 
       height = 6, 
       dpi = 125
       )

# # patchwork (combine plots) - L1
# pl_5512_patch_calepa_l1 <- pl_5102_calepa_l1_stack / pl_5102_calepa_l1

### L2 ----
(pl_5102_calepa_l2 <- df_5102_epa_1yr %>% 
     fun_summary_5102_l2() %>% 
     bind_rows(acs_data_level2) %>% 
     fun_plot_5102_l2(plot_title = default_pl_title, 
                      plot_caption = default_pl_caption))
 
ggsave(filename = here('07_slides', '2021-05-14', 'images', 
                       '5102_calepa_level2.png'), 
       plot = pl_5102_calepa_l2, 
       width = 10, 
       height = 6, 
       dpi = 125
       )

(pl_5102_calepa_l2_stack <- df_5102_epa_1yr %>% 
        fun_summary_5102_l2() %>% 
        bind_rows(acs_data_level2) %>% 
        fun_plot_5102_l2_stack(plot_title = default_pl_title, 
                               plot_caption = default_pl_caption))

ggsave(filename = here('07_slides', '2021-05-14', 'images', 
                       '5102_calepa_level2_stack.png'), 
       plot = pl_5102_calepa_l2_stack, 
       width = 10, 
       height = 6, 
       dpi = 125
       )

# patchwork (combine plots) - L1 and L2 stacked plots
pl_5102_patch_calepa_stack_combined <- pl_5102_calepa_l1_stack / pl_5102_calepa_l2_stack
pl_5102_patch_calepa_stack_combined[[1]] <- pl_5102_patch_calepa_stack_combined[[1]] + 
    labs(title = element_blank(),
         subtitle = element_blank(),
         caption = element_blank())
pl_5102_patch_calepa_stack_combined[[2]] <- pl_5102_patch_calepa_stack_combined[[2]] + 
    labs(title = element_blank(),
         subtitle = element_blank(),
         caption = element_blank())
pl_5102_patch_calepa_stack_combined <- pl_5102_patch_calepa_stack_combined + 
    plot_layout(heights = c(1.7, 2)) +
    plot_annotation(
        title = default_pl_title,
        caption = default_pl_caption)
ggsave(filename = here('07_slides', '2021-05-14', 'images', 
                       '5102_calepa_level1_2_combined_stack.png'), 
       plot = pl_5102_patch_calepa_stack_combined, 
       width = 10, 
       height = 6, 
       dpi = 125
       )

# patchwork (combine plots) - both L2 plots
pl_5102_patch_calepa_l2 <- pl_5102_calepa_l2_stack / pl_5102_calepa_l2
pl_5102_patch_calepa_l2[[1]] <- pl_5102_patch_calepa_l2[[1]] + 
    labs(title = element_blank(),
         subtitle = element_blank(),
         caption = element_blank())
pl_5102_patch_calepa_l2[[2]] <- pl_5102_patch_calepa_l2[[2]] + 
    labs(title = element_blank(),
         subtitle = element_blank(),
         caption = element_blank())
pl_5102_patch_calepa_l2 <- pl_5102_patch_calepa_l2 + 
    plot_annotation(
        title = default_pl_title,
        caption = default_pl_caption)
ggsave(filename = here('07_slides', '2021-05-14', 'images', 
                       '5102_calepa_level2_combined.png'), 
       plot = pl_5102_patch_calepa_l2, 
       width = 10, 
       height = 6, 
       dpi = 125
       )

## 2.2. - CalEPA By Group ----

### create groupings ----
df_5102_epa_1yr <- df_5102_epa_1yr %>% 
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
# sum_5102_groups_l1 <- df_5102_epa_1yr %>% 
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
#          x = 'Ethnicity Group',
#          y = 'Percent of Total', 
#          caption = 'State population data from 2019 1 yr ACS  |  workforce data from CalHR 5102') +
#     theme(legend.position = 'bottom', 
#           legend.title = element_blank()) +
#     guides(fill = guide_legend(reverse = TRUE)))

sum_5102_groups_l1 <- df_5102_epa_1yr %>% 
    filter(metrics_group == 'Legal Occupations') %>%
    fun_summary_5102_l1() %>% 
    bind_rows(acs_data_level1) %>% 
    mutate(metrics_group = 'Legal Occupations') %>%
    bind_rows(df_5102_epa_1yr %>% 
                  filter(metrics_group == 'Management Occupations') %>%
                  fun_summary_5102_l1() %>% 
                  bind_rows(acs_data_level1) %>% 
                  mutate(metrics_group = 'Management Occupations')) %>%
    bind_rows(df_5102_epa_1yr %>% 
                  filter(metrics_group == 'Administrative Occupations') %>%
                  fun_summary_5102_l1() %>% 
                  bind_rows(acs_data_level1) %>% 
                  mutate(metrics_group = 'Administrative Occupations')) %>%
    bind_rows(df_5102_epa_1yr %>% 
                  filter(metrics_group == 'Technical Occupations') %>%
                  fun_summary_5102_l1() %>% 
                  bind_rows(acs_data_level1) %>% 
                  mutate(metrics_group = 'Technical Occupations')) %>%
    mutate(type = factor(type)) %>% 
    {.}

facet_label_5102_groups_l1 <- sum_5102_groups_l1 %>% 
        filter(type == 'CalEPA Workforce') %>% 
        count(metrics_group, wt = ethnicity_total) %>% 
        mutate(facet_label = glue('{metrics_group} (CalEPA n = {n})'))


(pl_5102_groups_l1 <- sum_5102_groups_l1 %>%  
        left_join(facet_label_5102_groups_l1, 
                  by = c('metrics_group')) %>% 
        select(-metrics_group) %>% 
        rename('metrics_group' = 'facet_label') %>% 
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity_level1, 
                               y = rate, 
                               fill = factor(type, levels = rev(levels(type)))), 
                 stat = 'identity', 
                 position = 'dodge') + 
        facet_wrap(~ metrics_group) +
        scale_fill_manual(values = colors_5102_state_epa) +
        scale_y_continuous(labels = label_percent(accuracy = 1L)) +
        labs(title = default_pl_title_groups,
             subtitle = default_pl_subtitle,
             x = 'Ethnicity Group',
             y = 'Percent of Total', 
             caption = default_pl_caption) +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE)))

ggsave(filename = here('07_slides', '2021-05-14', 'images', 
                       '5102_calepa_groups_level1.png'), 
       plot = pl_5102_groups_l1, 
       width = 10, 
       height = 6, 
       dpi = 125
       )

(pl_5102_groups_l1_stack <- sum_5102_groups_l1 %>%  
        left_join(facet_label_5102_groups_l1, 
                  by = c('metrics_group')) %>% 
        select(-metrics_group) %>% 
        rename('metrics_group' = 'facet_label') %>% 
        ggplot() +
        geom_bar(mapping = aes(x = fct_rev(type), 
                               y = rate, 
                               fill = ethnicity_level1), #factor(type, levels = rev(levels(type)))), 
                 stat = 'identity') + 
        facet_wrap(~ metrics_group) +
        scale_fill_manual(values = colors_5102_2) +
        scale_y_continuous(labels = percent) +
        labs(title = default_pl_title_groups,
             subtitle = default_pl_subtitle,
             x = element_blank(),
             y = 'Percent of Total', 
             caption = default_pl_caption) +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE)))

ggsave(filename = here('07_slides', '2021-05-14', 'images', 
                       '5102_calepa_groups_level1_stack.png'), 
       plot = pl_5102_groups_l1_stack, 
       width = 10, 
       height = 6, 
       dpi = 125
       )


### L2 ----
fun_summary_5102_groups_l2 <- function(dataset) {
    dataset %>% 
        filter(metrics_group == 'Legal Occupations') %>%
        fun_summary_5102_l2() %>% 
        bind_rows(acs_data_level2) %>% 
        mutate(metrics_group = 'Legal Occupations') %>%
        bind_rows(dataset %>% 
                      filter(metrics_group == 'Management Occupations') %>%
                      fun_summary_5102_l2() %>% 
                      bind_rows(acs_data_level2) %>% 
                      mutate(metrics_group = 'Management Occupations')) %>%
        bind_rows(dataset %>% 
                      filter(metrics_group == 'Administrative Occupations') %>%
                      fun_summary_5102_l2() %>% 
                      bind_rows(acs_data_level2) %>% 
                      mutate(metrics_group = 'Administrative Occupations')) %>%
        bind_rows(dataset %>% 
                      filter(metrics_group == 'Technical Occupations') %>%
                      fun_summary_5102_l2() %>% 
                      bind_rows(acs_data_level2) %>% 
                      mutate(metrics_group = 'Technical Occupations')) %>%
        mutate(type = factor(type)) %>% 
        {.}
}


fun_plot_5102_groups_l2 <- function(dataset, 
                                    plot_title = default_pl_title_groups, 
                                    plot_subtitle = default_pl_subtitle, 
                                    plot_caption = default_pl_caption) {
    dataset %>% 
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity_level2, 
                               y = rate, 
                               fill = factor(type, levels = rev(levels(type)))), 
                 stat = 'identity', 
                 position = 'dodge') + 
        facet_wrap(~ metrics_group) +
        scale_y_continuous(labels = percent) +
        scale_fill_manual(values = colors_5102_state_epa) +
        labs(title = plot_title,
             subtitle = plot_subtitle,
             x = 'Ethnicity Group',
             y = 'Percent of Total', 
             caption = plot_caption) +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}


sum_5102_groups_l2 <- df_5102_epa_1yr %>%
    fun_summary_5102_groups_l2()

facet_label_5102_groups_l2 <- sum_5102_groups_l2 %>% 
    filter(type == 'CalEPA Workforce') %>% 
    count(metrics_group, wt = ethnicity_total) %>% 
    mutate(facet_label = glue('{metrics_group} (CalEPA n = {n})'))

(pl_5102_groups_l2 <- df_5102_epa_1yr %>% 
        fun_summary_5102_groups_l2() %>%
        left_join(facet_label_5102_groups_l2, by = c('metrics_group')) %>% 
        select(-metrics_group) %>% 
        rename('metrics_group' = 'facet_label') %>% 
        fun_plot_5102_groups_l2())

ggsave(filename = here('07_slides', '2021-05-14', 'images', 
                       '5102_calepa_groups_level2.png'), 
       plot = pl_5102_groups_l2, 
       width = 10, 
       height = 6, 
       dpi = 125
       )

        
fun_plot_5102_groups_l2_stack <- function(dataset, 
                                          plot_title = default_pl_title_groups, 
                                          plot_subtitle = default_pl_subtitle, 
                                          plot_caption = default_pl_caption) {
    dataset %>% 
        ggplot() +
        geom_bar(mapping = aes(x = fct_rev(type), 
                               y = rate, 
                               fill = ethnicity_level2), 
                 stat = 'identity') + 
        facet_wrap(~ metrics_group) +
        scale_y_continuous(labels = percent) +
        scale_fill_manual(values = colors_5102_7) +
        labs(title = plot_title,
             subtitle = plot_subtitle,
             x = element_blank(),
             y = 'Percent of Total', 
             caption = plot_caption) +
        coord_flip() + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
}


(pl_5102_groups_l2_stack <- df_5102_epa_1yr %>% 
        fun_summary_5102_groups_l2() %>% 
        left_join(facet_label_5102_groups_l2, by = c('metrics_group')) %>% 
        select(-metrics_group) %>% 
        rename('metrics_group' = 'facet_label') %>% 
        fun_plot_5102_groups_l2_stack())

ggsave(filename = here('07_slides', '2021-05-14', 'images', 
                       '5102_calepa_groups_level2_stack.png'), 
       plot = pl_5102_groups_l2_stack, 
       width = 10, 
       height = 6, 
       dpi = 125
       )
    

## 2.4 By BDO Plots ----
### L2 ----
summary_5102_l2_bdo <- df_5102_epa_1yr %>%
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
        scale_fill_manual(values = colors_5102_7) +
        scale_y_continuous(labels = percent) +
        labs(title = default_pl_title_bdo,
             x = element_blank(),
             y = 'Percent of Total', 
             caption = default_pl_caption) +
        coord_flip() + 
        # facet_wrap(~ dept) +
        theme(legend.position = 'bottom', 
              legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE)) + 
        geom_vline(xintercept = 1.5, size = 0.5, linetype = 'dashed'))#, color = 'grey50')

ggsave(filename = here('07_slides', '2021-05-14', 'images', 
                       '5102_calepa_bdo_level2.png'), 
       plot = pl_5102_l2_bdo_stack, 
       width = 10, 
       height = 6, 
       dpi = 125
       )


### L2 - BDO & Group ----
##### enter dept name for plot vs dept name in 5102 dataset
# bdo_titles <- c("Air Resources Board" = 'Air Resources Board', 
#                 "Offc. of Env. Health Hazard Assessment" = 'Environmental Health Hazard Assessment, Office of',
#                 "Cal. Environmental Protection Agency" = 'Environmental Protection Agency',
#                 "Dept. of Pesticide Regulation" = 'Pesticide Regulation, Department of',
#                 "Dept. of Resources Recycling and Recovery" = 'Resources Recycling and Recovery, Department of',
#                 "Dept. of Toxic Substances Control" = 'Toxic Substances Control, Department of',
#                 "Water Resources Control Board" = 'Water Resources Control Board')

bdo_titles <- c("ARB" = 'Air Resources Board', 
                "OEHHA" = 'Environmental Health Hazard Assessment, Office of',
                "CalEPA (Agency)" = 'Environmental Protection Agency',
                "DPR" = 'Pesticide Regulation, Department of',
                "CalRecycle" = 'Resources Recycling and Recovery, Department of',
                "DTSC" = 'Toxic Substances Control, Department of',
                "SWRCB" = 'Water Resources Control Board')
bdo_titles_filenames <- tolower(c('ARB', 'OEHHA', 'CalEPA_Agency', 
                                  'DPR', 'CalRecycle', 'DTSC', 'SWRCB'))

# get facet labels
pl_5102_groups_l2_bdo_all_facets <- map2_df(.x = names(bdo_titles), 
                                            .y = unname(bdo_titles), 
                                            .f = ~  df_5102_epa_1yr %>% 
                                                filter(dept == .y) %>% 
                                                fun_summary_5102_groups_l2() %>% 
                                                filter(type == 'CalEPA Workforce') %>% 
                                                count(metrics_group, wt = ethnicity_total) %>% 
                                                mutate(facet_label = glue('{metrics_group} ({.x} n = {n})')) %>% 
                                                mutate(bdo = glue('{.x}'))
                                            )

#### make plots
pl_5102_groups_l2_bdo_all <- map2(.x = names(bdo_titles), 
                                  .y = unname(bdo_titles), 
                                  .f = ~ df_5102_epa_1yr %>% 
                                      filter(dept == .y) %>% 
                                      fun_summary_5102_groups_l2() %>%
                                      mutate(type = case_when(as.character(type) == 'CalEPA Workforce' ~
                                                                  glue('{.x}'),
                                                              TRUE ~ as.character(type))) %>%
                                      mutate(type = factor(type, 
                                                           levels = c(.x, 'State Population'))) %>%
                                      mutate(bdo = glue('{.x}')) %>%
                                      left_join(pl_5102_groups_l2_bdo_all_facets,
                                                by = c('metrics_group', 'bdo')) %>%
                                      select(-metrics_group) %>%
                                      rename('metrics_group' = 'facet_label') %>%
                                      distinct() %>%
                                      fun_plot_5102_groups_l2_stack(plot_title = glue('{.x} Workforce by Occupation and Ethnicity Group vs. State Population (Year {report_year})')) %>%
                                      {.}
)
## View one plot
pl_5102_groups_l2_bdo_all[[3]]

#### save plots
walk2(.x = pl_5102_groups_l2_bdo_all, 
      # .y = tolower(str_replace_all(string = names(bdo_titles), 
      #                              pattern = ' ', 
      #                              replacement = '_') %>% 
      #                  str_remove_all(pattern = '\\.|(|)')), 
      .y = bdo_titles_filenames, # keep filenames consistent regardless of changes to plot labels/titles
      .f = ~ ggsave(filename = here('07_slides', '2021-05-14', 'images', 
                                    glue('5102_bdo_group_l2_{.y}.png')), 
                    plot = .x,
                    width = 10, 
                    height = 6, 
                    dpi = 125)
)


# # plot one individual bdo (not needed if above map/walk functions are working)
# bdo_plot <- "Air Resources Board"
# bdo_plot <- 'Water Resources Control Board'
# (pl_5102_groups_l2_bdo <- df_5102_epa_1yr %>%
#     filter(dept == bdo_plot) %>%
#     fun_summary_5102_groups_l2() %>%
#     mutate(type = case_when(as.character(type) == 'CalEPA Workforce' ~
#                                 glue('{bdo_plot} Workforce'),
#                             TRUE ~ as.character(type))) %>%
#     mutate(type = factor(type, levels = c(glue('{bdo_plot} Workforce'), 'State Population'))) %>%
#     fun_plot_5102_groups_l2_stack(plot_title = glue('{bdo_plot} Workforce by Occupation and Ethnicity Group Compared to State Population (Year {report_year})'))
# )
# ggsave(filename = here('07_slides', '2021-05-14', 'images',
#                        glue('5102_bdo_group_l2_{bdo_plot}.png')),
#        plot = pl_5102_groups_l2_bdo,
#        width = 10,
#        height = 6,
#        dpi = 125
#        )


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
    left_join(job_class_flux, by = 'ethnicity_level2') %>% 
    mutate_if(is.numeric, ~replace_na(., 0)) %>% 
    mutate(intake_total = intake_outside + 
               intake_other_agencies + 
               intake_advancements) %>% 
    mutate(departure = intake_total - change)


## timeline ----
job_class_timeline <- df_5102_epa %>% 
    filter(class_title == job_class) 

