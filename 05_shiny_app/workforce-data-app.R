# workforce data application


# load packages ---------------
library(shiny)
library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(purrr)
library(glue)
library(lubridate)
library(ggplot2)
library(stringr)
library(forcats)
library(scales)
library(tidycensus)
library(sf)


# import data ---------------
## 5102 report ----
url_data_all_yrs <- 'https://data.ca.gov/dataset/e620a64f-6b86-4ce0-ab4b-03d06674287b/resource/aba87ad9-f6b0-4a7e-a45e-d1452417eb7f/download/calhr_5102_statewide_2011-2020.csv'

df_5102_report <- read_csv(url_data_all_yrs,
                        col_types = cols(.default = col_character())) %>%  
    type_convert() %>% 
    clean_names() %>% 
    # mutate(record_count = as.integer(record_count)) %>% 
    {.}

## census data (ACS 1 yr) ----
years <- c(2011,2012,2013,2014,2015,2016,2017,2018,2019)


### get ACS data
acs_data_raw <- map_dfr(
    years,
    ~ get_acs(geography = 'state', 
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
                        year = .x),
    .id = "year"
    )


# Clean/Transform Data ---------------
## 5102 ----
# create a new column with just the year
df_5102_report <- df_5102_report %>% 
    mutate(report_year = year(as_of_date))

## ACS ----
# clean / reformat the acs data (the data will be formatted to be 
# consistent with the "level 2" ethnicity groupings in the workforce 
# data that are created above)
acs_data_raw <- acs_data_raw %>% mutate(year = (as.numeric(year) + 2010))

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
    group_by(year, geoid, location_name, ethnicity_level2, total_state_pop) %>% 
    summarize(estimate = sum(estimate)) %>%
    ungroup() %>% 
    {.}


# ignore below
## check (should be TRUE) - make sure sum of populations by group == total state population
# sum(acs_data_level2$estimate) == acs_data_level2$total_state_pop[1]
# okay resume

# LEFT OF HERE 6/17 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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




# Define UI for application ###################################################
ui <- fluidPage(

    # Application title
    titlePanel("CA State Workforce Data"),

    # Sidebar with filters 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "rpt_year", 
                        label = "Reporting Year:", 
                        choices = # rev(
                            df_5102_report %>% 
                                          distinct(report_year) %>% 
                                          arrange(desc(report_year)) %>% 
                                          pull(report_year)
                            #)
                        ),
            selectInput(inputId = "department", 
                        label = "Department:", 
                        choices = c('All', 
                                    df_5102_report %>% 
                                        distinct(dept) %>% 
                                        arrange(dept) %>% 
                                        pull(dept))
                        ),
            selectInput(inputId = "class_selected",
                        label = "Class Title:",
                        choices = c('All',
                                    str_to_title(df_5102_report %>% 
                                                     distinct(class_title) %>% 
                                                     arrange(class_title) %>% 
                                                     pull(class_title)))
                        ),
            # uiOutput('cl_title')
        ),
        
        

        # outputs
        mainPanel(
            column(12, align = 'center',
                   plotOutput('plot_class_title', 
                              height = 500),
                   radioButtons(inputId = 'bar_type', 
                                label = 'bar type:',
                                choices = c('stacked', 'grouped'),
                                selected = 'stacked')
            )
        )
    )
)

##### Define server logic ###################################################
server <- function(input, output, session) {
    
    # # --------------- create filter for class titles (updates based on selected year/agency) --------------- 
    # output$cl_title <- renderUI({
    #     classes_filtered <- str_to_title(df_5102_report %>%
    #                                          filter(dept %in% filter_dpt(),
    #                                                 report_year == input$rpt_year) %>%
    #                                          distinct(class_title) %>%
    #                                          arrange(class_title) %>%
    #                                          pull(class_title))
    #     selectInput(inputId = "class_selected",
    #                 label = "Class Title:",
    #                 choices = c('All', classes_filtered))
    # })
    
    # --------------- get filters ---------------
    # department
    filter_dpt <- reactive({
        if(input$department == 'All')
            return(unique(df_5102_report$dept))
        else
            return(input$department)
    })
    
    # class title
    filter_title <- reactive({
        if(input$class_selected == 'All')
            return(unique(df_5102_report$class_title))
        else
            return(input$class_selected)
    })
    
    
    # --------------- update filter options ---------------
    # reactive update to available selections in dropdown for report year
    observer_year <- reactive({
        list(input$department, input$class_selected)
    })
    observeEvent(observer_year(),
                 {updateSelectInput(session = session,
                                    inputId = 'rpt_year',
                                    choices = c(#'All',
                                                df_5102_report %>%
                                                    filter(dept %in% filter_dpt(), 
                                                           class_title %in% toupper(filter_title())) %>%
                                                    distinct(report_year) %>% 
                                                    arrange(desc(report_year)) %>%
                                                    pull(report_year)),
                                    selected = input$rpt_year
                 )})
    
    # reactive update to available selections in dropdown for department
    # input$rpt_year, input$department, input$class_selected
    observer_department <- reactive({
        list(input$rpt_year, input$class_selected)
    })
    observeEvent(observer_department(),
                 {updateSelectInput(session = session,
                                    inputId = 'department',
                                    choices = c('All',
                                                df_5102_report %>%
                                                    filter(report_year == input$rpt_year,
                                                           class_title %in% toupper(filter_title())) %>%
                                                    distinct(dept) %>% 
                                                    arrange(dept) %>%
                                                    pull(dept)),
                                    selected = input$department
                 )})
    
    # reactive update to available selections in dropdown for class title
    observer_class_title <- reactive({
        list(input$rpt_year, input$department)
    })
    observeEvent(observer_class_title(),
                 {updateSelectInput(session = session,
                                    inputId = 'class_selected',
                                    choices = c('All',
                                                str_to_title(df_5102_report %>%
                                                                 filter(report_year == input$rpt_year,
                                                                        dept %in% filter_dpt()) %>%
                                                                 distinct(class_title) %>%
                                                                 arrange(class_title) %>%
                                                                 pull(class_title))),
                                    selected = input$class_selected
                 )})

    # --------------- make plot --------------- 
    
    # create function factory for getting integer y-axis values (see: https://joshuacook.netlify.app/post/integer-values-ggplot-axis/)
    integer_breaks <- function(n = 5, ...) {
        fxn <- function(x) {
            breaks <- floor(pretty(x, n, ...))
            names(breaks) <- attr(breaks, "labels")
            breaks
        }
        return(fxn)
    }
    
    # make the plot
    output$plot_class_title <- renderPlot({
        # make the plot
        pl_class_title <- df_5102_report %>% 
            filter(dept %in% filter_dpt(),
                   class_title %in% toupper(filter_title()), 
                   report_year == input$rpt_year) %>% 
            group_by(identity_variable, gender) %>% 
            summarize(total_n = sum(record_count)) %>% 
            ungroup() %>% 
            mutate(identity_variable = fct_reorder(identity_variable, total_n)) %>% 
            ggplot() + # code below this line actually creates the plot
            aes(x = identity_variable, y = total_n, fill = gender) +
            geom_bar(stat = 'identity', 
                     position = ifelse(input$bar_type == 'stacked', 'stack', 'dodge')) + # position = 'dodge'
            scale_y_continuous(labels = comma_format(accuracy = 1),
                               breaks = integer_breaks()) +
            coord_flip() +
            labs(x = 'Race / Ethnicity', 
                 y = 'Number of Employees', 
                 title = glue('Employees In California State Government'),
                 subtitle = glue('Year: ', input$rpt_year, 
                                 ' | Department: ', input$department, 
                                 ' | Class Title: ', input$class_selected), 
                 caption = "Note: data from 5102 Inline Report", 
                 fill = 'Gender')
        
        # output the plot
        pl_class_title
    })
    
        
        
    # --------------- create plot by agency --------------- 
    # output$plot_agency <- renderPlot({
    #     pl_agency <- ggplot(data = df_5102_report %>% 
    #                             filter(dept == input$department,
    #                                    report_year == input$rpt_year)) +
    #         aes(x = identity_variable,
    #             y = record_count) +
    #         geom_bar()
    # })
}


# Run the application 
shinyApp(ui = ui, server = server)
