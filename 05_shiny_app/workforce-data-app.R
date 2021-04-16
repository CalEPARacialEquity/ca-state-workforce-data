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


# import data ---------------
df_5102_report <- read_csv('data/calhr_5102_statewide_2011-2020.csv.gz',
                        col_types = cols(.default = col_character())) %>% 
    type_convert() %>% 
    clean_names() %>% 
    # mutate(record_count = as.integer(record_count)) %>% 
    {.}


# clean/transform data ---------------
# create a new column with just the year
df_5102_report <- df_5102_report %>% 
    mutate(report_year = year(as_of_date))



##### Define UI for application ###################################################
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
