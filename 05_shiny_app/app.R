# Workforce Data Application

# Load Packages ---------------
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


# Write Necessary Functions -----------------------------------------------
## Create function factory for getting integer y-axis values (see: https://joshuacook.netlify.app/post/integer-values-ggplot-axis/) ----
integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
        breaks <- floor(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
    }
    return(fxn)
}

colors_5102_state_dept <-c('gold', 'darkgreen')

# Import Data ---------------
## 5102 Report ----
url_data_all_yrs <-
    'https://data.ca.gov/dataset/e620a64f-6b86-4ce0-ab4b-03d06674287b/resource/aba87ad9-f6b0-4a7e-a45e-d1452417eb7f/download/calhr_5102_statewide_2011-2020.csv'

df_5102_report <-
    read_csv(
        'data\\calhr_5102_statewide_2011-2020.csv.gz',
        col_types = cols(.default = col_character())
    ) %>%
    type_convert() %>%
    clean_names() %>%
    # mutate(record_count = as.integer(record_count)) %>%
    {
        .
    }

## Census Data (ACS 1 yr) ----
# Define years
years <- c(2011:2019)

### Pull ACS Data
acs_data_raw <- map_dfr(
    years,
    ~ get_acs(
        geography = 'state',
        variables = c(
            # total_state_pop = 'B02001_001',
            'Hispanic or Latino' = 'B03002_012',
            # Total Hispanic or Latino
            'White' = 'B03002_003',
            # White (Not Hispanic or Latino)
            'Black or African American' = 'B03002_004',
            # Black or African American (Not Hispanic or Latino)
            'Native American or Alaska Native' = 'B03002_005',
            # American Indian and Alaska Native (Not Hispanic or Latino)
            'Asian' = 'B03002_006',
            # Asian (Not Hispanic or Latino)
            'Pacific Islander' = 'B03002_007',
            # Native Hawaiian and Other Pacific Islander (Not Hispanic or Latino)
            'Other' = 'B03002_008',
            # Some other race (Not Hispanic or Latino)
            'Multiple' = 'B03002_009'# Two or more races (Not Hispanic or Latino)
        ),
        summary_var = c(total_state_pop = 'B02001_001'),
        survey = 'acs1',
        # use 'acs1' or 'acs5' to get 1 or 5 year acs
        state = 'CA',
        geometry = FALSE,
        # set to TRUE to get as geospatial data
        year = .x
    ),
    .id = "year"
)


# Clean/Transform Data ---------------
## 5102 Report ----
# create a new column with just the year
df_5102_report <- df_5102_report %>%
    mutate(report_year = year(as_of_date))

df_5102_report <- df_5102_report %>%
    mutate(
        ethnicity_level1 = case_when(
            identity_variable == 'White' ~ 'White',
            TRUE ~ 'BIPOC'),
        ethnicity_level2 = case_when(
            str_detect(identity_variable, 'Pacific Islander') ~ 'Pacific Islander',
            # groups (all start with "Pacific Islander - "): Guamanian, Hawaiian, Other or Multiple, and Samoan
            str_detect(identity_variable, 'Asian') ~ 'Asian',
            # groups (all start with "Asian - "): Cambodian, Chinese, Filipino, Indian, Japanese, Korean, Laotian, Other or Multiple, Vietnamese
            TRUE ~ identity_variable
        )
    )

## Census Data (ACS) ----
# Clean / reformat the ACS data (the data will be formatted to be consistent with the "level 2" ethnicity groupings in the workforce data that are created above)
acs_data_raw <-
    acs_data_raw %>% mutate(year = (as.numeric(year) + 2010))

acs_data_level2 <- acs_data_raw %>%
    clean_names() %>%
    rename(
        total_state_pop = summary_est,
        total_state_pop_moe = summary_moe,
        location_name = name,
        ethnicity_level2 = variable
    )

# Check (should be TRUE) - make sure sum of populations by group == total state population
# sum(acs_data_level2$estimate) == acs_data_level2$total_state_pop[1]

# Group the 'Other' and 'Multiple' rows into one 'Other or Multiple Race' row
acs_data_level2_final <- acs_data_level2 %>%
    mutate(
        ethnicity_level2 =
            case_when(
                ethnicity_level2 == 'Other' |
                    ethnicity_level2 == 'Multiple' ~
                    'Other or Multiple Race',
                TRUE ~ ethnicity_level2
            )
    ) %>%
    group_by(year, geoid, location_name, ethnicity_level2, total_state_pop) %>%
    summarize(estimate = sum(estimate)) %>%
    ungroup() %>%
    # Add a column with each ethnicity's % of total state population
    mutate(pop_pct = estimate / total_state_pop) %>% 
    {.}


## check (should be TRUE) - make sure sum of populations by group == total state population
# sum(acs_data_level2$estimate) == acs_data_level2$total_state_pop[1]

## create a dataset grouped at level 1 - all BIPOC together (resulting groups are BIPOC and white)
acs_data_level1 <- acs_data_level2_final %>%
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

## check (should be TRUE)
# sum(acs_data_level1$estimate) == acs_data_level1$total_state_pop[1]


# Define UI for application -----------------------------------------------
ui <- fluidPage(
    tabsetPanel(
        tabPanel("Introduction",
                 h2("This Report"),
                 tags$body("Understanding the composition of a workforce is useful for anticipating future hiring needs and associated budgets (such as succession planning), program and/or policy implementation, and contextually understanding workforce survey results. The purpose of this workforce demographics report is to provide initial data to assess whether California government workforce resemble the state's population demographics. Annual updates of workforce demographics will provide a baseline for understanding whether future racial equity initiatives foster a diverse workforce within and across agencies. Alternatively, it can serve as a guide to reflect whether we adequately represent the populations in the communities we serve."),
                 br(),
                 tags$body("This code can be modified and applied to any department(s) in the California state system listed in California Department of Human Resource's (CalHR) 5102 data  set. This code was written by California Environmental Protection Agency's (CalEPA)  Racial Equity Team (CRET) Data Subteam in Spring 2021 to provide helpful  racial demographics visualizations for the workforce of the Agency and its Boards,  Departments, and Office (BDOs). The effort came out of a desire to supplement the Agency's Race Forward survey results with workforce demographic data. The Data Subteam intends on maintaining and building off this and other resources and datasets. Future developments include adding RShiny components, analyzing entry-level vs promotional classification demographics, and incorporating data on gender identity, veteran status, and ability."),
                 br(),
                 tags$body("The Data Subteam acknowledges that racial equity work is complex and  cannot be pared down to simple numbers in bins. In addition, diversity is intersectional and includes identities such as gender identity, race/ethnicity, sexual orientation, veteran status, and ability. This code and resulting  visualizations seeks to be the first step in critically examining an agency's  racial demographics, and may be more helpful in developing the right questions  that need to be asked, rather than immediate answers and/or numeric targets. If you have any questions about this code, please email Leah Jones at"),
                 tags$link("leah.jones@waterboards.ca.gov"),
                 tags$body("."),
                 br(),
                 tags$h2("CRET Workforce Equity Subteam"),
                 tags$body("The vision of CRET Workforce Equity Subteam's work is to create a workforce in which  one's work and/or career outcomes are not predicted by one's race. This vision is informed by more detailed goals and objectives related to work/career satisfaction, promotion and retention, compensation, inclusion, mental and other health safety, and more.  CRET's model of change - normalize, organize and operationalize – is  aligned with over 200 government organizations part of a nationwide network to  transform governments to deliver more racial equity. We anticipate that these demographic metrics and resulting performance measures will evolve over time and  will be informed by future data and analyses of surveys, workforce demographics,  and research."),
                 br(),
                 tags$h2("Why Diversity and Inclusion in Representation Matters"),
                 tags$body("The workforce composition of state agencies matters and conversations based on data help drive meaningful change. Inadequate diversity has been proven to lead to less innovation (Hofstra et al., 2020). Workforce diversity enhances our ability to protect public health and the environment by helping the state to understand exposures to pollutants, set priorities, and improve communication."),
                 br(),
                 tags$body("It is well-known that people relate to people like themselves. In order to fully engage in conversations regarding policies that affect all communities in California, we must improve our conversations with the communities we serve. One of the first steps to engaging in meaningful conversations is to build trust within the communities. Looking to our own workforce’s experiences by listening to and empowering those who grew up, live, and work in our communities may help us engage and build that trust. In addition, nearly half of Californians speak a language other than English at home. Communicating our messages in locally spoken languages enhances the effectiveness of our programs and protects people in emergencies."),
                 br(),
                 tags$body("Specific to CalEPA's concerns, CalEnviroscreen data have shown us that communities of color experience the highest levels of environmental burdens and the greatest effects of climate change (Figure 1). COVID-19 has also exacerbated existing social, health, educational, criminal justice, and housing inequities that were already disproportionately affecting these communities. CalEPA’s environmental regulations and policies have real effects on California residents. Finding ways to better engage with communities to effectively address these disparate impacts can help us better serve those who are the most burdened by these inequities."),
                 br()
        ),
        tabPanel("Department Summary Plots",
                 # Sidebar with filters (inputs)
                 # Broken down by department(s) and reporting year, not class
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "sum_rpt_year",
                             label = "Reporting Year:",
                             choices = df_5102_report %>%
                                 distinct(report_year) %>%
                                 arrange(desc(report_year)) %>%
                                 pull(report_year)
                         ),
                         selectInput(inputId = "sum_department",
                                     label = "Department:",
                                     choices = c('All',
                                                 df_5102_report %>% 
                                                     distinct(dept) %>% 
                                                     arrange(dept) %>% 
                                                     pull(dept)
                                     )
                                     #,
                                     #multiple = TRUE
                         )
                         #,
                         # textInput(inputId = "sum_title",
                         #           label = "Add a title:",
                         #           placeholder = "CalEPA Workforce Demographics")
                     ),
                     
                     # Main panel with plot (output)
                     mainPanel(
                         column(
                             12,
                             align = 'center',
                             plotOutput('sum_plot',
                                        height = 500)
                         )
                     )
                 )
        ),
        tabPanel("Exploratory Tool: Department Ethnicity and Gender",
                 # Sidebar with filters (inputs)
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "exp_rpt_year",
                             label = "Reporting Year:",
                             choices = #rev(
                                 df_5102_report %>%
                                 distinct(report_year) %>%
                                 arrange(desc(report_year)) %>%
                                 pull(report_year)
                             #)
                         ),
                         selectInput(
                             inputId = "exp_department",
                             label = "Department:",
                             choices = c('All',
                                         df_5102_report %>% 
                                             distinct(dept) %>% 
                                             arrange(dept) %>% 
                                             pull(dept))
                         ),
                         selectInput(
                             inputId = "exp_class_selected",
                             label = "Class Title:",
                             choices = c(
                                 'All',
                                 str_to_title(
                                     df_5102_report %>%
                                         distinct(class_title) %>%
                                         arrange(class_title) %>%
                                         pull(class_title)
                                 )
                             )
                         ),
                     ),
                     
                     # Main panel with plot (output)
                     mainPanel(
                         column(
                             12,
                             align = 'center',
                             plotOutput('exp_plot',
                                        height = 500),
                             radioButtons(
                                 inputId = 'bar_type',
                                 label = 'bar type:',
                                 choices = c('stacked', 'grouped'),
                                 selected = 'stacked'
                             )
                         )
                     )
                 )
        )
    )
)


# Define server logic -----------------------------------------------------
server <- function(input, output, session) {
    
    ## Department Summary Plot -------------------------------------------------
    # --------------- Reactive values ---------------
    # Filter department
    sum_filter_dpt <- reactive({
        if(input$sum_department == 'All')
            return(unique(df_5102_report$dept))
        else
            return(input$sum_department)
    })
    
    # --------------- Update filter options ---------------
    # Reactive update to available selections in dropdown for report year
    sum_observer_year <- reactive({
        list(input$sum_department)
    })
    observeEvent(sum_observer_year(),
                 {
                     updateSelectInput(
                         session = session,
                         inputId = 'sum_rpt_year',
                         choices = c(
                             #'All',
                             df_5102_report %>%
                                 filter(
                                     dept %in% sum_filter_dpt()
                                 ) %>%
                                 distinct(report_year) %>%
                                 arrange(desc(report_year)) %>%
                                 pull(report_year)
                         ),
                         selected = input$sum_rpt_year
                     )
                 })
    
    # Reactive update to available selections in dropdown for department
    sum_observer_department <- reactive({
        list(input$sum_rpt_year)
    })
    observeEvent(sum_observer_department(),
                 {
                     updateSelectInput(
                         session = session,
                         inputId = 'sum_department',
                         choices = c(
                             'All',
                             df_5102_report %>%
                                 filter(
                                     report_year == input$sum_rpt_year
                                 ) %>%
                                 distinct(dept) %>%
                                 arrange(dept) %>%
                                 pull(dept)
                         ),
                         selected = input$sum_department
                     )
                 })
    # --------------- Render text ---------------
    # output$sum_title <- renderText({input$sum_title})
    # --------------- Render plot ---------------
    # Plot class title
    output$sum_plot <- renderPlot({
        pl_dept_sum <- df_5102_report %>%
            filter(
                dept %in% sum_filter_dpt(),
                report_year == input$sum_rpt_year
            ) %>%
            add_count(ethnicity_level2,
                      wt = record_count,
                      name = 'ethnicity_total') %>%
            select(ethnicity_level2, ethnicity_total) %>%
            distinct() %>%
            mutate(rate = ethnicity_total / sum(ethnicity_total)) %>%
            mutate(type = 'Department Workforce') %>%
            arrange(ethnicity_level2) %>%
            mutate(type = factor(type)) %>%
            bind_rows(acs_data_level2_final) %>% 
            ggplot() + # code below this line actually creates the plot
            geom_bar(mapping = aes(x = ethnicity_level2,
                                   y = rate,
                                   fill = factor(type, levels = rev(levels(type)))),
                     stat = 'identity',
                     position = 'dodge') +
            scale_fill_manual(values = colors_5102_state_dept) +
            scale_y_continuous(labels = label_percent(accuracy = 1L)) +
            labs(title = "plot_title",
                 x = 'Ethnicity Group',
                 y = 'Percent of Total',
                 caption = "plot_caption") +
            coord_flip() +
            theme(legend.position = 'bottom',
                  legend.title = element_blank()) +
            guides(fill = guide_legend(reverse = TRUE))
        
        # output the plot
        pl_dept_sum
    })
    
    
    # Exploratory Tool --------------------------------------------------------
    # --------------- Reactive values ---------------
    # Filter department
    exp_filter_dpt <- reactive({
        if(input$exp_department == 'All')
            return(unique(df_5102_report$dept))
        else
            return(input$exp_department)
    })
    
    # Filter class title
    exp_filter_title <- reactive({
        if(input$exp_class_selected == 'All')
            return(unique(df_5102_report$class_title))
        else
            return(input$exp_class_selected)
    })
    
    
    # --------------- Update filter options ---------------
    # Reactive update to available selections in dropdown for report year
    exp_observer_year <- reactive({
        list(input$exp_department, input$exp_class_selected)
    })
    observeEvent(exp_observer_year(),
                 {
                     updateSelectInput(
                         session = session,
                         inputId = 'exp_rpt_year',
                         choices = c(
                             #'All',
                             df_5102_report %>%
                                 filter(
                                     dept %in% exp_filter_dpt(),
                                     class_title %in% toupper(exp_filter_title())
                                 ) %>%
                                 distinct(report_year) %>%
                                 arrange(desc(report_year)) %>%
                                 pull(report_year)
                         ),
                         selected = input$exp_rpt_year
                     )
                 })
    
    # Reactive update to available selections in dropdown for department
    exp_observer_department <- reactive({
        list(input$exp_rpt_year, input$exp_class_selected)
    })
    observeEvent(exp_observer_department(),
                 {
                     updateSelectInput(
                         session = session,
                         inputId = 'exp_department',
                         choices = c(
                             'All',
                             df_5102_report %>%
                                 filter(
                                     report_year == input$exp_rpt_year,
                                     class_title %in% toupper(exp_filter_title())
                                 ) %>%
                                 distinct(dept) %>%
                                 arrange(dept) %>%
                                 pull(dept)
                         ),
                         selected = input$exp_department
                     )
                 })
    
    # Reactive update to available selections in dropdown for class title
    exp_observer_class_title <- reactive({
        list(input$exp_rpt_year, input$exp_department)
    })
    observeEvent(exp_observer_class_title(),
                 {
                     updateSelectInput(
                         session = session,
                         inputId = 'exp_class_selected',
                         choices = c(
                             'All',
                             str_to_title(
                                 df_5102_report %>%
                                     filter(report_year == input$exp_rpt_year,
                                            dept %in% exp_filter_dpt()) %>%
                                     distinct(class_title) %>%
                                     arrange(class_title) %>%
                                     pull(class_title)
                             )
                         ),
                         selected = input$exp_class_selected
                     )
                 })
    
    # --------------- Render plot ---------------
    # Plot class title
    output$exp_plot <- renderPlot({
        pl_class_title <- df_5102_report %>%
            filter(
                dept %in% exp_filter_dpt(),
                class_title %in% toupper(exp_filter_title()),
                report_year == input$exp_rpt_year
            ) %>%
            group_by(identity_variable, gender) %>%
            summarize(total_n = sum(record_count)) %>%
            ungroup() %>%
            mutate(identity_variable = fct_reorder(identity_variable, total_n)) %>%
            ggplot() + # code below this line actually creates the plot
            aes(x = identity_variable,
                y = total_n,
                fill = gender) +
            geom_bar(
                stat = 'identity',
                position = ifelse(input$bar_type == 'stacked', 'stack', 'dodge')
            ) + # position = 'dodge'
            scale_y_continuous(labels = comma_format(accuracy = 1),
                               breaks = integer_breaks()) +
            coord_flip() +
            labs(
                x = 'Race / Ethnicity',
                y = 'Number of Employees',
                title = glue('Employees In California State Government'),
                subtitle = glue(
                    'Year: ',
                    input$exp_rpt_year,
                    ' | Department: ',
                    input$exp_department,
                    ' | Class Title: ',
                    input$exp_class_selected
                ),
                caption = "Note: data from 5102 Inline Report",
                fill = 'Gender'
            )
        
        # output the plot
        pl_class_title
    })
    
    
    # --------------- Create plot by agency ---------------
    # output$plot_agency <- renderPlot({
    #     pl_agency <- ggplot(data = df_5102_report %>%
    #                             filter(dept == input$exp_department,
    #                                    report_year == input$exp_rpt_year)) +
    #         aes(x = identity_variable,
    #             y = record_count) +
    #         geom_bar()
    # })
}


# Run the application -----------------------------------------------------
shinyApp(ui = ui, server = server)