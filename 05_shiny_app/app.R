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
library(shinyWidgets)
library(plotly)
library(RColorBrewer) 


# Write Necessary Functions -----------------------------------------------
## Create function factory for getting integer y-axis values (see: https://joshuacook.netlify.app/post/integer-values-ggplot-axis/)
integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
        breaks <- floor(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
    }
    return(fxn)
}

colors_5102_state_dept <- c('darkgreen', 'gold')


# Import Data ---------------
census_data <-
    read_csv("data/census_data.csv")
# %>%
# mutate(type = as.character(type))

workforce_data <-
    read_csv("data/workforce_data.csv.gz")
# %>%
# mutate(type = as.character(type))

deptchoices <- c(workforce_data %>%
                     distinct(dept) %>%
                     arrange(dept) %>%
                     pull(dept))

categorychoices <- c(workforce_data %>%
                         distinct(employee_category) %>%
                         arrange(employee_category) %>%
                         pull(employee_category))

subcategorychoices <- c(workforce_data %>%
                            distinct(sub_category) %>%
                            arrange(sub_category) %>%
                            pull(sub_category))

# Define UI for app -----------------------------------------------

## Page 1 ------------------------------------------------------------------
ui <- navbarPage(
    title = "Workforce Demographic Tools",
    tabPanel(
        "Introduction",
        h2("This Report"),
        tags$p(
            "Understanding the composition of a workforce is useful for anticipating future hiring needs and associated budgets (such as succession planning), program and/or policy implementation, and contextually understanding workforce survey results. The purpose of this workforce demographics report is to provide initial data to assess whether California government workforce resemble the state's population demographics. Annual updates of workforce demographics will provide a baseline for understanding whether future racial equity initiatives foster a diverse workforce within and across agencies. Alternatively, it can serve as a guide to reflect whether we adequately represent the populations in the communities we serve."
        ),
        br(),
        tags$p(
            "This code can be modified and applied to any department(s) in the California state system listed in California Department of Human Resource's (CalHR) 5102 data  set. This code was written by California Environmental Protection Agency's (CalEPA)  Racial Equity Team (CRET) Data Subteam in Spring 2021 to provide helpful  racial demographics visualizations for the workforce of the Agency and its Boards,  Departments, and Office (BDOs). The effort came out of a desire to supplement the Agency's Race Forward survey results with workforce demographic data. The Data Subteam intends on maintaining and building off this and other resources and datasets. Future developments include adding RShiny components, analyzing entry-level vs promotional classification demographics, and incorporating data on gender identity, veteran status, and ability."
        ),
        br(),
        tags$p(
            "The Data Subteam acknowledges that racial equity work is complex and  cannot be pared down to simple numbers in bins. In addition, diversity is intersectional and includes identities such as gender identity, race/ethnicity, sexual orientation, veteran status, and ability. This code and resulting  visualizations seeks to be the first step in critically examining an agency's  racial demographics, and may be more helpful in developing the right questions  that need to be asked, rather than immediate answers and /
or numeric targets. If you have any questions about this code, please email Leah Jones at",
            tags$a(href = "mailto:leah.jones@waterboards.ca.gov", "leah.jones@waterboards.ca.gov"),
            "."
        ),
        br(),
        tags$h2("CRET Workforce Equity Subteam"),
        tags$p(
            "The vision of CRET Workforce Equity Subteam's work is to create a workforce in which  one's work and / or career outcomes are not predicted by one's race. This vision is informed by more detailed goals and objectives related to work/career satisfaction, promotion and retention, compensation, inclusion, mental and other health safety, and more.  CRET's model of change - normalize, organize and operationalize – is  aligned with over 200 government organizations part of a nationwide network to  transform governments to deliver more racial equity. We anticipate that these demographic metrics and resulting performance measures will evolve over time and  will be informed by future data and analyses of surveys, workforce demographics,  and research."
        ),
        br(),
        tags$h2("Why Diversity and Inclusion in Representation Matters"),
        tags$p(
            "The workforce composition of state agencies matters and conversations based on data help drive meaningful change. Inadequate diversity has been proven to lead to less innovation (Hofstra et al., 2020). Workforce diversity enhances our ability to protect public health and the environment by helping the state to understand exposures to pollutants, set priorities, and improve communication."
        ),
        br(),
        tags$p(
            "It is well-known that people relate to people like themselves. In order to fully engage in conversations regarding policies that affect all communities in California, we must improve our conversations with the communities we serve. One of the first steps to engaging in meaningful conversations is to build trust within the communities. Looking to our own workforce’s experiences by listening to and empowering those who grew up, live, and work in our communities may help us engage and build that trust. In addition, nearly half of Californians speak a language other than English at home. Communicating our messages in locally spoken languages enhances the effectiveness of our programs and protects people in emergencies."
        ),
        br(),
        tags$p(
            "Specific to CalEPA's concerns, CalEnviroscreen data have shown us that communities of color experience the highest levels of environmental burdens and the greatest effects of climate change (Figure 1). COVID-19 has also exacerbated existing social, health, educational, criminal justice, and housing inequities that were already disproportionately affecting these communities. CalEPA’s environmental regulations and policies have real effects on California residents. Finding ways to better engage with communities to effectively address these disparate impacts can help us better serve those who are the most burdened by these inequities."
        ),
        br()
    ),
    
    ## Page 2 ------------------------------------------------------------------
    tabPanel(
        "Department Summary Plots",
        # Sidebar with filters (inputs)
        # Broken down by department(s) and reporting year, not class
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId = "sum_rpt_year",
                    label = "Reporting Year:",
                    choices = workforce_data %>%
                        distinct(report_year) %>%
                        arrange(desc(report_year)) %>%
                        pull(report_year),
                    selected = 2019 # delete once we get 2020 census data
                ),
                pickerInput(
                    inputId = "sum_department",
                    label = "Department:",
                    choices = deptchoices,
                    # selected = workforce_data$dept,
                    options = pickerOptions(
                        `actions-box` = TRUE,
                        `liveSearch` = TRUE,
                        `virtual-scroll` = 10,
                        `multiple-separator` = "\n",
                        `selected-text-format` = paste0("count > ", length(deptchoices) -
                                                            1),
                        `count-selected-text` = "All Departments",
                        size = 10
                    ),
                    multiple = TRUE
                    ),
                pickerInput(
                    inputId = "sum_category",
                    label = "Employee Category:",
                    choices = categorychoices,
                    # selected = workforce_data$employee_category,
                    options = pickerOptions(
                        `actions-box` = TRUE,
                        `liveSearch` = TRUE,
                        `virtual-scroll` = 10,
                        `multiple-separator` = "\n",
                        `selected-text-format` = paste0("count > ", length(categorychoices) -
                                                            1),
                        `count-selected-text` = "All Categories",
                        size = 10
                    ),
                    multiple = TRUE
                    ),
                pickerInput(
                    inputId = "sum_sub_category",
                    label = "Employee Sub-Category:",
                    choices = subcategorychoices,
                    # selected = workforce_data$sub_category,
                    options = pickerOptions(
                        `actions-box` = TRUE,
                        `liveSearch` = TRUE,
                        `virtual-scroll` = 10,
                        `multiple-separator` = "\n",
                        `selected-text-format` = paste0("count > ", length(subcategorychoices) -
                                                            1),
                        `count-selected-text` = "All Sub-Categories",
                        size = 10
                    ),
                    multiple = TRUE
                ),
                tags$h6(
                    em(
                        "Hover over the above input bar for a list of the selected department(s)."
                    )
                ),
                textInput(
                    inputId = "sum_graph_title",
                    label = "Graph title:",
                    placeholder = "All Workforce Demographics"
                ),
                width = 3
            ),
            
            # Main panel with plot (output)
            mainPanel(column(
                12,
                align = 'center',
                plotlyOutput('sum_plot',
                             height = 500),
                radioButtons(
                    inputId = 'sum_level_type',
                    label = 'Ethnicity level:',
                    choices = c('Level 1', 'Level 2'),
                    selected = 'Level 2'
                )
            ))
        )
    ),
    
    ## Page 3 ------------------------------------------------------------------
    tabPanel(
        "Demographics Over Time",
        # Sidebar with filters (inputs)
        # Broken down by department(s) and reporting year, not class
        sidebarLayout(
            sidebarPanel(
                pickerInput(
                    inputId = "time_department",
                    label = "Department:",
                    choices = deptchoices,
                    selected = workforce_data$dept,
                    options = pickerOptions(
                        `actions-box` = TRUE,
                        `liveSearch` = TRUE,
                        `virtual-scroll` = 10,
                        `multiple-separator` = "\n",
                        `selected-text-format` = paste0("count > ", length(deptchoices) -
                                                            1),
                        `count-selected-text` = "All Departments",
                        size = 10
                    ),
                    multiple = TRUE
                ),
                tags$h6(
                    em(
                        "Hover over the above input bar for a list of the selected department(s)."
                    )
                ),
                textInput(
                    inputId = "time_graph_title",
                    label = "Graph title:",
                    placeholder = "All Workforce Demographics"
                ),
                width = 3
            ),
            
            # Main panel with plot (output)
            mainPanel(column(
                12,
                align = 'center',
                plotlyOutput('time_plot',
                             height = 500),
                radioButtons(
                    inputId = 'time_level_type',
                    label = 'Ethnicity level:',
                    choices = c('Level 1', 'Level 2'),
                    selected = 'Level 2'
                )
            ))
        )
    ),
    
    ## Page 4 ------------------------------------------------------------------
    tabPanel(
        "Exploratory Tool: Department Ethnicity and Gender",
        # Sidebar with filters (inputs)
        sidebarLayout(
            sidebarPanel(
                selectizeInput(
                    inputId = "exp_rpt_year",
                    label = "Reporting Year:",
                    choices = workforce_data %>%
                        distinct(report_year) %>%
                        arrange(desc(report_year)) %>%
                        pull(report_year)
                ),
                selectizeInput(
                    inputId = "exp_department",
                    label = "Department:",
                    choices = c(
                        'All',
                        workforce_data %>%
                            distinct(dept) %>%
                            arrange(dept) %>%
                            pull(dept)
                    )
                ),
                selectizeInput(
                    inputId = "exp_class_selected",
                    label = "Class Title:",
                    choices = c(
                        'All',
                        str_to_title(
                            workforce_data %>%
                                distinct(class_title) %>%
                                arrange(class_title) %>%
                                pull(class_title)
                        )
                    )
                )
            ),
            
            # Main panel with plot (output)
            mainPanel(column(
                12,
                align = 'center',
                plotlyOutput('exp_plot',
                             height = 500),
                radioButtons(
                    inputId = 'bar_type',
                    label = 'bar type:',
                    choices = c('stacked', 'grouped'),
                    selected = 'stacked'
                )
            ))
        )
    )
)

# Define server logic -----------------------------------------------------
server <- function(input, output, session) {
    # Page 2 ------------------------------------------------------------------
    # Reactive update to available selections in dropdown for report year
    
    # # update category options based on sub-category selection
    # sub_category_updated <- reactive({
    #     workforce_data %>%
    #         filter(
    #             sub_category %in% input$sum_sub_category
    #             # ,Level == input$time_level_type
    #         ) %>%
    #         distinct(employee_category, .keep_all = TRUE) %>%
    #         arrange(employee_category) %>%
    #         pull(employee_category)
    # })
    # 
    # # update sub-category options based on category selection
    # category_updated <- reactive({
    #     workforce_data %>%
    #         filter(
    #             employee_category %in% input$sum_category
    #             # ,Level == input$time_level_type
    #         ) %>%
    #         distinct(sub_category, .keep_all = TRUE) %>%
    #         arrange(sub_category) %>%
    #         pull(sub_category)
    # })
    # 
    
    census_data_updated <- reactive({
        census_data %>%
            select(year,
                   Level,
                   Ethnicity,
                   total_pop,
                   ethnicity_total,
                   rate,
                   type) %>%
            filter(Level == input$sum_level_type) %>%
            filter(year == input$sum_rpt_year)
    })
    
    workforce_data_updated <- reactive({
        workforce_data %>%
            filter(
                dept %in% sum_observer_department(),
                report_year == input$sum_rpt_year,
                employee_category %in% sum_observer_category(),
                sub_category %in% sum_observer_sub_category(),
                Level == input$sum_level_type
            ) %>%
            rename(year = report_year,
                   total_pop = record_count) %>%
            add_count(Ethnicity,
                      wt = total_pop,
                      name = 'ethnicity_total') %>%
            distinct(Ethnicity, ethnicity_total, .keep_all = TRUE) %>%
            mutate(rate = ethnicity_total / sum(ethnicity_total)) %>%
            arrange(Ethnicity) %>%
            select(dept,
                   year,
                   Level,
                   Ethnicity,
                   total_pop,
                   ethnicity_total,
                   rate,
                   employee_category,
                   sub_category,
                   type) %>%
            bind_rows(census_data_updated())
    })
    
    # # Reactive update for department
    # observeEvent(workforce_data_updated(),
    #              {
    #                  updatePickerInput(
    #                      session = session,
    #                      inputId = 'sum_department',
    #                      choices = c(workforce_data_updated() %>%
    #                                      distinct(dept) %>%
    #                                      arrange(desc(dept)) %>%
    #                                      pull(dept)
    #                      ),
    #                      selected = input$sum_department
    #                  )
    #              })
    # 
    # # Reactive update for year
    # observeEvent(workforce_data_updated(),
    #              {
    #                  updateSelectInput(
    #                      session = session,
    #                      inputId = 'sum_rpt_year',
    #                      choices = c(workforce_data_updated() %>%
    #                                      distinct(year) %>%
    #                                      arrange(desc(year)) %>%
    #                                      pull(year)
    #                      ),
    #                      selected = input$sum_rpt_year
    #                  )
    #              })
    # 
    # # Reactive update for category
    # observeEvent(workforce_data_updated(),
    #              {
    #                  updatePickerInput(
    #                      session = session,
    #                      inputId = 'sum_category',
    #                      choices = c(workforce_data_updated() %>%
    #                                      distinct(employee_category) %>%
    #                                      arrange(desc(employee_category)) %>%
    #                                      pull(employee_category)
    #                      ),
    #                      selected = input$sum_category
    #                  )
    #              })
    # 
    # # Reactive update for sub-category
    # observeEvent(workforce_data_updated(),
    #              {
    #                  updatePickerInput(
    #                      session = session,
    #                      inputId = 'sum_sub_category',
    #                      choices = c(workforce_data_updated() %>%
    #                                      distinct(sub_category) %>%
    #                                      arrange(desc(sub_category)) %>%
    #                                      pull(sub_category)
    #                      ),
    #                      selected = input$sum_sub_category
    #                  )
    #              })
    
    
    # Reactive update for department
    sum_observer_department <- reactive({
        list(input$sum_rpt_year, input$sum_category, input$sum_sub_category)
    })
    observeEvent(sum_observer_department(),
                 {
                     updatePickerInput(
                         session = session,
                         inputId = 'sum_department',
                         choices = c(
                             workforce_data %>%
                                 filter(report_year == input$sum_rpt_year,
                                        employee_category %in% input$sum_category,
                                        sub_category %in% input$sum_sub_category
                                 ) %>%
                                 distinct(dept) %>%
                                 arrange(desc(dept)) %>%
                                 pull(dept)
                         ),
                         selected = input$sum_department
                     )
                 })

    # Reactive update for year
    sum_observer_year <- reactive({
        list(input$sum_department, input$sum_category, input$sum_sub_category)
    })
    observeEvent(sum_observer_year(),
                 {
                     updateSelectInput(
                         session = session,
                         inputId = 'sum_rpt_year',
                         choices = c(
                             workforce_data %>%
                                 filter(dept %in% input$sum_department,
                                        employee_category %in% input$sum_category,
                                        sub_category %in% input$sum_sub_category
                                 ) %>%
                                 distinct(report_year) %>%
                                 arrange(desc(report_year)) %>%
                                 pull(report_year)
                         ),
                         selected = input$sum_rpt_year
                     )
                 })

    # Reactive update for category
    sum_observer_category <- reactive({
        list(input$sum_rpt_year, input$sum_department, input$sum_sub_category)
    })
    observeEvent(sum_observer_category(),
                 {
                     updatePickerInput(
                         session = session,
                         inputId = 'sum_category',
                         choices = c(
                             workforce_data %>%
                                 filter(report_year == input$sum_rpt_year,
                                        dept %in% input$sum_department,
                                        sub_category %in% input$sum_sub_category
                                 ) %>%
                                 distinct(employee_category) %>%
                                 arrange(desc(employee_category)) %>%
                                 pull(employee_category)
                         ),
                         selected = input$sum_category
                     )
                 })

    # Reactive update for sub-category
    sum_observer_sub_category <- reactive({
        list(input$sum_rpt_year, input$sum_category, input$sum_department)
    })
    observeEvent(sum_observer_sub_category(),
                 {
                     updatePickerInput(
                         session = session,
                         inputId = 'sum_sub_category',
                         choices = c(
                             workforce_data %>%
                                 filter(report_year == input$sum_rpt_year,
                                        employee_category %in% input$sum_category,
                                        dept %in% input$sum_department
                                 ) %>%
                                 distinct(sub_category) %>%
                                 arrange(desc(sub_category)) %>%
                                 pull(sub_category)
                         ),
                         selected = input$sum_sub_category
                     )
                 })

    # Reactive update to available selections in dropdown for department
    # observeEvent(input$sum_rpt_year,
    #              {
    #                  updateSelectInput(
    #                      session = session,
    #                      inputId = 'sum_department',
    #                      choices = c(
    #                          workforce_data %>%
    #                              filter(report_year == input$sum_rpt_year) %>%
    #                              distinct(dept) %>%
    #                              arrange(dept) %>%
    #                              pull(dept)
    #                      ),
    #                      selected = input$sum_department
    #                  )
    #              })
    # Reactive update to filter for ACS year
    

    
    ### Render plot -------------------------------------------------------------
    output$sum_plot <- renderPlotly({
        pl_dept_sum <- ggplotly(
            workforce_data_updated() %>%
                ggplot() + # code below this line actually creates the plot
                geom_bar(
                    mapping = aes(
                        x = Ethnicity,
                        y = rate,
                        fill = type
                    ),
                    stat = 'identity',
                    position = 'dodge'
                ) +
                scale_fill_manual(values = colors_5102_state_dept) +
                scale_y_continuous(labels = label_percent(accuracy = 1L)) +
                labs(
                    x = 'Ethnicity Group',
                    y = 'Percent of Total',
                    caption = "Data sources: state population data from 1 yr American Community Survey  |  workforce data from CalHR 5102 Report"
                ) +
                coord_flip() +
                theme(
                    legend.position = 'bottom',
                    legend.title = element_blank()
                ) +
                guides(fill = guide_legend(reverse = TRUE))
        ) %>%
            layout(
                title = input$sum_graph_title,
                legend = list(
                    title = '',
                    xanchor = 'right',
                    yanchor =  'bottom'
                )
            )
        # output the plot
        pl_dept_sum
    })
    
    # Page 3 ------------------------------------------------------------------
    # Reactive update to filter for ACS year
    time_workforce_data_updated <- reactive({
        workforce_data %>%
            filter(
                dept %in% input$time_department,
                Level == input$time_level_type
            ) %>%
            rename(year = report_year) %>%
            group_by(year) %>%
            add_count(Ethnicity,
                      wt = record_count,
                      name = 'ethnicity_total') %>%
            add_count(year,
                      wt = record_count,
                      name = 'total_pop') %>% 
            ungroup() %>% 
            mutate(rate = ethnicity_total / total_pop) %>%
            distinct(Ethnicity, year, .keep_all = TRUE) %>%
            arrange(Ethnicity) %>%
            select(year,
                   Level,
                   Ethnicity,
                   total_pop,
                   ethnicity_total,
                   rate,
                   type)
    })
    
    group_number <- reactive({
        nrow(time_workforce_data_updated() %>% 
                  distinct(Ethnicity))
    })
    
    ### Render plot -------------------------------------------------------------
    output$time_plot <- renderPlotly({
        pl_dept_time <- ggplotly(
            time_workforce_data_updated() %>%
                ggplot() + # code below this line actually creates the plot
                geom_col(
                    mapping = aes(
                        group = group_number(),
                        x = factor(year),
                        y = rate,
                        fill = Ethnicity
                    )
                ) +
                # scale_fill_manual(values = colors_5102_state_dept) +
                scale_y_continuous(limits = c(0,1), labels = label_percent(accuracy = 1L)) +
                scale_colour_brewer(palette = "RdYlBu",
                                    aesthetics = "fill") +
                labs(
                    x = 'Year',
                    y = 'Percent of Total',
                    caption = "Data sources: state population data from 1 yr American Community Survey  |  workforce data from CalHR 5102 Report"
                ) +
                theme(
                    legend.position = 'bottom',
                    legend.title = element_blank()
                ) 
            # + guides(fill = guide_legend(reverse = TRUE))
        ) %>%
            layout(
                title = input$time_graph_title,
                legend = list(
                    title = '',
                    xanchor = 'right',
                    yanchor =  'bottom'
                )
                # yaxis = list(tickformat = "%",
                #              range = c(0, 1))
            )
        # output the plot
        pl_dept_time
    })

    
    # Page 4 ------------------------------------------------------------------
    
    ## Reactive values ---------------------------------------------------------
    # Filter department
    exp_filter_dpt <- reactive({
        if (input$exp_department == 'All')
            return(unique(workforce_data$dept))
        else
            return(input$exp_department)
    })
    
    # Filter class title
    exp_filter_title <- reactive({
        if (input$exp_class_selected == 'All')
            return(unique(workforce_data$class_title))
        else
            return(input$exp_class_selected)
    })
    
    
    
    ## Update filter options ---------------------------------------------------
    # Reactive update to available selections in dropdown for report year
    exp_observer_year <- reactive({
        list(input$exp_department, input$exp_class_selected)
    })
    observeEvent(exp_observer_year(),
                 {
                     updateSelectizeInput(
                         session = session,
                         inputId = 'exp_rpt_year',
                         choices = c(
                             #'All',
                             workforce_data %>%
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
                     updateSelectizeInput(
                         session = session,
                         inputId = 'exp_department',
                         choices = c(
                             'All',
                             workforce_data %>%
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
                     updateSelectizeInput(
                         session = session,
                         inputId = 'exp_class_selected',
                         choices = c(
                             'All',
                             str_to_title(
                                 workforce_data %>%
                                     filter(
                                         report_year == input$exp_rpt_year,
                                         dept %in% exp_filter_dpt()
                                     ) %>%
                                     distinct(class_title) %>%
                                     arrange(class_title) %>%
                                     pull(class_title)
                             )
                         ),
                         selected = input$exp_class_selected
                     )
                 })
    
    
    ## Render plot -------------------------------------------------------------
    # Plot class title
    output$exp_plot <- renderPlotly({
        pl_class_title <- ggplotly(
            workforce_data %>%
                filter(
                    dept %in% exp_filter_dpt(),
                    class_title %in% toupper(exp_filter_title()),
                    report_year == input$exp_rpt_year
                ) %>%
                rename(total_pop = record_count) %>% 
                group_by(identity_variable, gender) %>%
                summarize(total_n = sum(total_pop)) %>%
                ungroup() %>%
                mutate(identity_variable = fct_reorder(identity_variable, total_n)) %>%
                ggplot() + # code below this line actually creates the plot
                aes(
                    x = identity_variable,
                    y = total_n,
                    fill = gender
                ) +
                geom_bar(
                    stat = 'identity',
                    position = ifelse(input$bar_type == 'stacked', 'stack', 'dodge')
                ) + # position = 'dodge'
                scale_fill_manual(values = colors_5102_state_dept) +
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
        )
        
        # output the plot
        pl_class_title
    })
    
    
    # Create plot by agency?
    # output$plot_agency <- renderPlot({
    #     pl_agency <- ggplot(data = workforce_data %>%
    #                             filter(dept == input$exp_department,
    #                                    report_year == input$exp_rpt_year)) +
    #         aes(x = identity_variable,
    #             y = total_pop) +
    #         geom_bar()
    # })
}


# Run the application -----------------------------------------------------
shinyApp(ui = ui, server = server)