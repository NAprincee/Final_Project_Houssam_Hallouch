# Packages ----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(DT)
library(e1071)
library(plotly)
library(scales)
library(shinydashboard)
library(shinycssloaders)

theme_set(theme_bw())

options(spinner.color="#0275D8", spinner.color.background="#ffffff", 
        spinner.size=1, spinner.type = 2)

# Data --------------------------------------------------------------------

data <- read.csv("data_for_analysis.csv")

states <- sort(unique(data$State_Name))
schools <- c("Small", "Medium", "Large")

# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "College Scorecard Data Analysis (USA)", titleWidth = 400),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("pen")),
      menuItem("Dataset", tabName = "data", icon = icon("database")),
      menuItem("Summary", tabName = "summary", icon = icon("magnifying-glass-chart")),
      menuItem("Visualization", tabName = "viz", icon=icon("chart-line"))
      ),
    
    hr(),
    
    pickerInput(
      inputId = "state_input",
      label = "Select States",
      choices = states,
      multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE,
                              liveSearch = TRUE,
                              size = 10,
                              selectedTextFormat = "count > 3",
                              countSelectedText = "{0} States Selected"),
      selected = c("California","Colorado","Montana")),
    
    pickerInput(
      inputId = "school_size_input",
      label = "Select School Sizes",
      choices = schools, selected = schools,
      multiple = TRUE,
      choicesOpt = list(
        subtext = c("1 - 5,000 students","5,000 - 15,000 students","15,000 + students")
      ),
      options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
    ),
    
    sliderInput(
      inputId = "admission_input",
      label = "Select Admission Rate Range",
      min = min(data$Admissions_rate_percent, na.rm = TRUE),
      max = max(data$Admissions_rate_percent, na.rm = TRUE),
      value = c(min(data$Admissions_rate_percent, na.rm = TRUE),
                max(data$Admissions_rate_percent, na.rm = TRUE)),
      post="%"),
    
    sliderInput(
      inputId = "median_family_income_input",
      label = "Select Median Family Income Range",
      min = min(data$Median_family_income, na.rm = TRUE),
      max = max(data$Median_family_income, na.rm = TRUE),
      value = c(0, max(data$Median_family_income, na.rm = TRUE)),
      pre="$")
  ),
  dashboardBody(
    
    tags$head(tags$style(HTML('
                              /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }
                              '))),
    
    tabItems(
      
      tabItem(tabName = "intro",
              
              tags$h1("College Scorecard Analysis"),
              
              tags$hr(),
              
              tags$p("The aim of this project is to provide valuable insights for 
                     students in their college search by analyzing the college scorecard data 
                     obtained from the DATA.gov website. We want to offer a data-driven 
                     perspective to assist students in making informed decisions when selecting 
                     an institution to attend. In order to achieve this, we conducted a 
                     comprehensive analysis of various factors, including institutional size, 
                     gender demographics, average entrance age, financial aid availability, 
                     and median earnings after graduation."),
                     
              tags$p("The motivation behind our research stems from the lack of reliable tools 
                     available to students seeking colleges. Traditionally, students rely heavily 
                     on subjective recommendations and anecdotal information, often resulting in 
                     suboptimal choices. Hence, we identified several key requirements to address 
                     this issue effectively. Firstly, we focused on visualizing the distributions 
                     of school demographics and statistics to provide a comprehensive overview. 
                     Additionally, we aimed to present a selection of institutions based on their 
                     sizes through well-organized tables. Finally, we recognized the value of 
                     developing a predictive model that could estimate median earnings 
                     post-graduation based on factors such as institution location and size."),
                     
               tags$p("By approaching the analysis in a professional manner, our project endeavors 
                     to empower students with data-backed insights and recommendations, ultimately 
                     assisting them in making more informed decisions regarding their higher 
                     education journey.")
      ),
      
      
      tabItem(tabName = "data",
              tags$br(),
              htmlOutput("count_schools"),
              tags$br(),
              DTOutput("data_output") %>% withSpinner()
      ),
      
      tabItem(tabName = "summary",
              tags$br(),
              DTOutput("summary_output") %>% withSpinner()
      ),
      
      tabItem(tabName = "viz",
              
              tabBox(
                id = "viz_tabs", 
                width = 12, 
                tabPanel(
                  title = "Summary Plots",
                  fluidRow(
                    box(width = 6,plotlyOutput("school_size_bar_output") %>% withSpinner()),
                    box(width = 6,plotlyOutput("state_count_bar_output") %>% withSpinner())
                  ),
                  fluidRow(
                    box(width = 12,
                        column(width = 4, 
                               pickerInput(
                                 inputId = "variable",
                                 label = "Select Variable",
                                 choices = colnames(data)[c(3,4,12,5,6,7,9)], selected = "Mean_entry_age",
                                 multiple = FALSE,
                                 options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
                               )
                               ),
                        column(width = 8, 
                               plotlyOutput("summary_barplot") %>% withSpinner()
                               )
                        )
                  )
                ),
                tabPanel(
                  title = "Density Plots",
                  fluidRow(
                    box(width = 6,plotlyOutput("density_loans") %>% withSpinner()),
                    box(width = 6,plotlyOutput("density_median_fam_income") %>% withSpinner())
                  ),
                  fluidRow(
                    box(width = 6,plotlyOutput("density_percent_female") %>% withSpinner()),
                    box(width = 6,plotlyOutput("density_percent_admission") %>% withSpinner())
                  ),
                  fluidRow(
                    box(width = 6,plotlyOutput("density_median_earnings") %>% withSpinner()),
                    box(width = 6,plotlyOutput("density_ent_age") %>% withSpinner())
                  )
                  )
                )
              
      )
    ) 
  )
)

# SERVER ------------------------------------------------------------------

server <- function(input, output) {
  
  ## Data filter ----
  data_filtered <- reactive({
    data %>%
      filter(Admissions_rate_percent < input$admission_input[2],
             Admissions_rate_percent > input$admission_input[1],
             Median_family_income < input$median_family_income_input[2],
             Median_family_income > input$median_family_income_input[1],
             School_size %in% input$school_size_input,
             State_Name %in% input$state_input)        
  })
  
  ## Count of Schools Filtered ----
  count_schools <- reactive({
    nrow(data_filtered())
  })
  
  output$count_schools <- renderText({
    paste0("Number of Schools Selected = ", count_schools())
  })
  
  output$data_output <- renderDT({
    
    validate(
      need(nrow(data_filtered()) > 0, 
           "There are no institutions returned for the filtering criteria. Please update.")
    )
    
    data_filt <- data_filtered() %>% 
      transmute(
        "Institution" = Institution_name, 
        "State" = State_Name,
        "Median Family Income" = Median_family_income,
        "Median Earnings After 10yrs" = Median_earnings_after_10yrs, 
        "Percent Students with loans" = Percent_students_with_loans/100,
        "Female Students %" = Percent_Female_students/100, 
        "Mean Entry Age" = Mean_entry_age, 
        "Admissions %" = Admissions_rate_percent/100,
        "Schools Size" = School_size
      ) %>%
      arrange(Institution)
    
    datatable(data_filt, selection = "none", caption = "Table: Filtered Data.") %>% 
      formatCurrency(c("Median Family Income","Median Earnings After 10yrs")) %>% 
      formatPercentage(c("Percent Students with loans","Female Students %","Admissions %"), 2)
  })
  
  output$summary_output <- renderDT({
    
    # Function to calculate statistics
    calculate_statistics <- function(data) {
      
      stats_df <- tibble(
        `Complete Cases` = sum(complete.cases(data)),
        `Missing` = sum(!complete.cases(data)),
        Mean = mean(data, na.rm = TRUE),
        Median = median(data, na.rm = TRUE),
        Min = min(data, na.rm = TRUE),
        Max = max(data, na.rm = TRUE),
        SD = sd(data, na.rm = TRUE),
        Skew = skewness(data, na.rm = TRUE),
        Kurt = kurtosis(data, na.rm = TRUE)
      ) %>% round(2)
      
      return(stats_df)
    }
    
    # Apply the function to the desired columns
    cols <- c("Percent_students_with_loans","Percent_Female_students",
              "Percent_Male_students","Admissions_rate_percent",
              "Median_earnings_after_10yrs","Median_family_income",
              "Mean_entry_age")
    statistics <- lapply(data_filtered()[, cols], calculate_statistics)
    
    # Combine the results into a single dataframe
    data_stat <- bind_rows(statistics, .id = "Variable")
    data_stat$Variable <- c("% Students with Loan","% Female","% Male",
                            "Admission Rate %", "Median Earnings After 10yrs",
                            "Median Family Income", "Mean Entry Age")
    
    # Print the resulting dataframe
    datatable(data_stat, caption = "Table: Summary Statistics.", selection = "none", options = list(dom = 't'))
    
  })
  
  ## Summary Plots ----
  
  output$school_size_bar_output <- renderPlotly({
    validate(
      need(nrow(data_filtered()) > 0, 
           "There are no institutions returned for the filtering criteria. Please update.")
    )
    
    (data_filtered() %>% 
       group_by(School_size) %>% 
       summarise(count = n()) %>% 
       arrange(count, desc(count)) %>% 
       ggplot(aes(x = School_size, y = count, fill = School_size,
                  text = paste0("Count: ",count))) +
       geom_bar(colour = "#ffffff", stat = "identity", alpha = 0.7) +
       geom_text(aes(label = count,  y = count + 5),  # https://stackoverflow.com/a/74722512/13323413
                 position = position_dodge(width=1), size = 4) +
       theme(legend.position = "none") +
       labs(x = NULL, y = "Count", title = "Number of Schools by Size")) %>% 
      ggplotly(tooltip = "text") 
  })
  
  output$state_count_bar_output <- renderPlotly({
    validate(
      need(nrow(data_filtered()) > 0, 
           "There are no institutions returned for the filtering criteria. Please update.")
    )
    
    (data_filtered() %>% 
       group_by(State_Name) %>% 
       summarise(count = n()) %>% 
       arrange(count, desc(count)) %>% 
       ggplot(aes(y = State_Name, x = count,
                  text = paste0("Count: ",count))) +
       geom_bar(fill = "#7bb8bd", stat = "identity", alpha = 0.7) +
       # scale_fill_brewer(palette="Spectral") +
       geom_text(aes(label = count,  x = count + 5), 
                 position = position_dodge(width=1), size = 4) +
       theme(legend.position = "none") +
       labs(x = "Count", y = NULL, title = "Number of Schools by State")) %>% 
      ggplotly(tooltip = NULL) 
  })
  
  output$summary_barplot <- renderPlotly({
    
    (data_filtered() %>%
       group_by(School_size) %>%
       summarize(Mean = round(mean(get(input$variable), na.rm = TRUE), 2)) %>%
       ggplot(aes(text = paste0("Average: ",Mean))) +
       geom_col(aes(x = School_size, y = Mean, fill = School_size)) +
       theme(legend.position = "none") +
       labs(x = "School Size", y = "Average",
            title = paste0("Average of ",input$variable," by School Size"))
     ) %>% 
      ggplotly(tooltip = "text")
    
  })
  
  ## Plot row 1 ----
  
  output$density_loans <- renderPlotly({
    validate(
      need(nrow(data_filtered()) > 0, 
           "There are no institutions returned for the filtering criteria. Please update.")
    )
    
    (
      ggplot(data_filtered(), aes(text = paste("School Size:",School_size))) +
        geom_density(aes(x = Percent_students_with_loans, fill = School_size),
                     alpha = 0.45, color = "#ffffff") +
        labs(x = "% Students with Loans", y = "Density", fill = "School Size",
             title = "Distribution of Percentage of Students with Loans") +
        scale_x_continuous(labels = label_percent(scale = 1))
    ) %>% 
      ggplotly(tooltip = "text") %>% 
      layout(legend = list(orientation = "h",x = 0,y = -0.22))
  })
  
  output$density_median_fam_income <- renderPlotly({
    validate(
      need(nrow(data_filtered()) > 0, 
           "There are no institutions returned for the filtering criteria. Please update.")
    )
    
    (
      ggplot(data_filtered(), aes(text = paste("School Size:",School_size))) +
        geom_density(aes(x = Median_family_income, fill = School_size),
                     alpha = 0.45, color = "#ffffff") +
        labs(x = "Median Family Income", y = "Density", fill = "School Size",
             title = "Distribution of Median Family Income") +
        scale_x_continuous(labels = label_dollar(scale = 1))
    ) %>% 
      ggplotly(tooltip = "text") %>% 
      layout(legend = list(orientation = "h",x = 0,y = -0.22))
  })
  
  ## Plot row 2 ----
  
  output$density_percent_female <- renderPlotly({
    validate(
      need(nrow(data_filtered()) > 0, 
           "There are no institutions returned for the filtering criteria. Please update.")
    )
    
    (
      ggplot(data_filtered(), aes(text = paste("School Size:",School_size))) +
        geom_density(aes(x = Percent_Female_students, fill = School_size),
                     alpha = 0.45, color = "#ffffff") +
        labs(x = "% Female Students", y = "Density", fill = "School Size",
             title = "Distribution of Percentage of Female Students") +
        scale_x_continuous(labels = label_percent(scale = 1))
    ) %>% 
      ggplotly(tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0, y = -0.22))
  })
  
  output$density_percent_admission <- renderPlotly({
    validate(
      need(nrow(data_filtered()) > 0, 
           "There are no institutions returned for the filtering criteria. Please update.")
    )
    
    (
      ggplot(data_filtered(), aes(text = paste("School Size:",School_size))) +
        geom_density(aes(x = Admissions_rate_percent, fill = School_size),
                     alpha = 0.45, color = "#ffffff") +
        labs(x = "Admissions Rate %", y = "Density", fill = "School Size",
             title = "Distribution of Admission Rate Percentage") +
        scale_x_continuous(labels = label_percent(scale = 1))
    ) %>% 
      ggplotly(tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0, y = -0.22))
  })
  
  ## Plot row 3 ----
  
  output$density_median_earnings <- renderPlotly({
    validate(
      need(nrow(data_filtered()) > 0, 
           "There are no institutions returned for the filtering criteria. Please update.")
    )
    
    (
      ggplot(data_filtered(), aes(text = paste("School Size:",School_size))) +
        geom_density(aes(x = Median_earnings_after_10yrs, fill = School_size),
                     alpha = 0.45, color = "#ffffff") +
        labs(x = "Median Earnings after 10 Years", 
             y = "Density", 
             fill = "School Size",
             title = "Distribution of Median Earnings after 10yrs") +
        scale_x_continuous(labels = label_dollar(scale = 1))
    ) %>% 
      ggplotly(tooltip = "text") %>% 
      layout(legend = list(orientation = "h",x = 0,y = -0.22))
  })
  
  output$density_ent_age <- renderPlotly({
    validate(
      need(nrow(data_filtered()) > 0, 
           "There are no institutions returned for the filtering criteria. Please update.")
    )
    
    (
      ggplot(data_filtered(), aes(text = paste("School Size:",School_size))) +
        geom_density(aes(x = Mean_entry_age, fill = School_size),
                     alpha = 0.45, color = "#ffffff") +
        labs(x = "Average Entrance Age", 
             y = "Density", 
             fill = "School Size",
             title = "Distribution of Average Entrance Age") +
        scale_x_continuous(labels = label_dollar(scale = 1))
    ) %>% 
      ggplotly(tooltip = "text") %>% 
      layout(legend = list(orientation = "h",x = 0,y = -0.22))
  })
  
}


shinyApp(ui = ui, server = server)
