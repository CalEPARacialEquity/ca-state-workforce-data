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
    clean_names()


# clean/transform data ---------------
# create a new column with just the year
df_5102_report <- df_5102_report %>% 
    mutate(report_year = year(as_of_date))



##### Define UI for application ###################################################
ui <- fluidPage(

    # Application title
    titlePanel("CA State Workforce Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "rpt_year", 
                        label = "Reporting Year:", 
                        choices = rev(df_5102_report %>% distinct(report_year) %>% pull(report_year))
                        ),
            selectInput(inputId = "department", 
                        label = "Department:", 
                        choices = c('All', 
                                    df_5102_report %>% distinct(dept) %>% pull(dept))
                        ),
            # selectInput(inputId = "cl_title", 
            #             label = "Class Title:", 
            #             choices = c('All',
            #                         str_to_title(df_5102_report %>% distinct(class_title) %>% pull(class_title)))
            #             ),
            uiOutput('cl_title')
        ),
        
        

        # outputs
        mainPanel(
            column(12, align = 'center',
                   plotOutput('plot_class_title', height = 500),
                   radioButtons(inputId = 'bar_type', 
                                label = 'bar type:',
                                choices = c('stacked', 'grouped'),
                                selected = 'stacked')
            )
        )
    )
)

##### Define server logic ###################################################
server <- function(input, output) {
    
    # create filter for class titles (updates based on selected year/agency) ----
    output$cl_title <- renderUI({
        classes_filtered <- str_to_title(df_5102_report %>% 
                                             filter(dept %in% filter_dpt(),
                                                    report_year == input$rpt_year) %>% 
                                             distinct(class_title) %>% 
                                             arrange(class_title) %>% 
                                             pull(class_title))
        selectInput(inputId = "class_selected", 
                    label = "Class Title:", 
                    choices = c('All', classes_filtered))
    })
    
    # get filters ----
    # department
    filter_dpt <- reactive({
        if(input$department == "All")
            return(unique(df_5102_report$dept))
        else
            return(input$department)
    })
    
    # class title
    filter_title <- reactive({
        if(input$class_selected == "All")
            return(unique(df_5102_report$class_title))
        else
            return(input$class_selected)
    })
    
    
        
    # make plot ----
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
            scale_y_continuous(labels = comma) +
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
    
        
        
    # create plot by agency ----
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
