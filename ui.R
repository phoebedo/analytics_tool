# Load required libraries
source("keys.R")
library(DBI)
library(RMySQL)
library(ggcorrplot)
library(ggplot2)
library(ggpubr)
library(shinythemes)
library(shiny)
library(shinycssloaders)
library(kableExtra)
library(DT)
library(plotly)
library(shinyWidgets)



# ui.R

# Define UI for application
fluidPage(
  theme = shinytheme("flatly"),
  # Application title
  titlePanel("Analytics Ultra Plus Pro Max PA+++ Tool"),
  
  # Sidebar layout with input and output definitions
  navbarPage("Menu", 
             collapsible = TRUE, 
             id = "menu_nav",
             tabPanel("Data View", 
                      value = "data_view",
                      sidebarPanel(
                        # Input: Textarea for SQL query input
                        textAreaInput("sql_query", "Enter your SQL query:", rows = 10),
                        # Button to submit query
                        div(
                          style="display:inline-block",
                          actionButton("submit_query", "Query"),
                          style="margin-left:auto"
                        ))
                        ,
                      
                      # Main panel for displaying the results
                      mainPanel(
                        
                        # Conditional panel to render content only if data from SQL is available
                         
                           uiOutput("view_analysis_btn"),
                           # Data preview table
                           h3(textOutput("data_preview_h3")),
                           DTOutput("data_preview")
                        
                      )
             ) # end data view
             , 
             
             tabPanel("Analysis View", 
                      value = "analysis_view", 
                      tabsetPanel(
                                  id = "analysis_tab", 
                        tabPanel("Stats", 
                         value = "stats_tab", 
                         em(textOutput("auto_detect_note")),
                         uiOutput("summary_stats_continous"),
                         
                         uiOutput("category_select"),
                         uiOutput("category_table")     
                       ), #end Stats tab 
                        
                        
                        
                        tabPanel("Distribution", 
                                 value = "dist_tab", 
                                 #fluid Distribution
                                 fluidRow(
                                 sidebarPanel(
                                   
                                   
                                   h4("Distribution"),
                                   # Select input for choosing the variable (numeric)
                                   selectInput("variable", "Select Variable:", choices = NULL),
                                   
                                   # Select input for choosing the category (categorical)
                                   selectInput("category", "Select Category:", 
                                               choices = c("None"), 
                                               selected = "None"),
                                   
                                   # Checkbox input for selecting the plot type
                                   selectInput("plot_type", "Plot Type:", 
                                                 choices = c("Histogram", "Boxplot"), 
                                                 selected = "Histogram")
                                   # uiOutput("category_select")
                                   
                                 ) #end side panel
                                 ,
                              
                                 # Main panel for displaying the results
                                 mainPanel(
                                   
                                  h4("Distribution graphs go here"),
                                  plotlyOutput("distribution_plot")
                                  
                                 ), 
                                 
                                 #Fluid Bucket Top Down
                                 fluidRow(
                                   sidebarPanel(
                                     h4("Contribution - Top Down"),
                                     selectInput("bucket_size", "Bucket size (%)",
                                                 choices = c(5,10,20),
                                                 selected = "10"),
                                     # Select input for choosing the category (categorical)
                                     selectInput("contributor", "Of",
                                                 choices = c("None"),
                                                 selected = "None"),

                                     # Checkbox input for selecting the plot type
                                     selectInput("contribute_to", "Contribute To",
                                                 choices = c("None"),
                                                 selected = "None"),
                                     
                                     selectInput("measure_bucket", "Measure",
                                                 choices = c("Sum","Median","Average"),
                                                 selected = "Median")

                                   ),
                                   mainPanel(
                                     uiOutput("topdown_table")
                                   )

                                 )  #end Bucket Top Down
                                  
                                 ) # end distribution main panel
                        ), #end Distribution
                        
                       
                       tabPanel("Cohort Analysis", 
                                value = "corr_tab", 
                                h4("Cohort Analysis")
                              
                       ), # end cohort 
                        
                       
                       # Single Regression 
                        tabPanel("Regression", 
                                 value = "corr_tab", 
                                 
                                 #Corr matrix
                                 fluidRow(
                                   h4("Correlation Overview "),
                                   div(plotOutput("correlation_matrix")) 
                                 ), # end corr matrix 
                                 
                                 #Single regression
                              
                                 fluidRow(
                                 
                                 sidebarPanel(
                                   h4("Regression"),
                                   
                                   selectInput("reg_independent", 
                                               "Independent variable (X):", 
                                               choices = c("None"), 
                                               selected = "None"),
                                 
                                   selectInput("reg_dependent", 
                                               "Dependent variable (Y):", 
                                               choices = c("None"), 
                                               selected = "None"),
                                   
                                   
                                   selectInput("reg_model", 
                                               "Regression model:", 
                                               choices = c("Linear", "2nd deg. Polynomial", "3rd deg. Polynomial"), 
                                               selected = "Linear"), 
                                   
                                   
                                   selectInput("regression_cat", "Category Breakdown", 
                                               choices = c("None"), 
                                               selected = "None")
                                 
                                 ), 
                      
                                 mainPanel(
                                   h4("Regression Model"),
                                   plotOutput("regression_model_single")
                                   )
                                 ),# end single regression 
                                
                                 
                                 
                                 
                                 
                                  #Multivariate regression
                                 
                                 fluidRow(
                                   
                                   sidebarPanel(
                                     h4("Multivariate Regression"),
                                     
                                     checkboxGroupInput("multi_reg_predictors", "Predictors", 
                                                        choices = "None", 
                                                        selected = "None"),
                                     
                                     checkboxGroupInput("multi_reg_responses", "Responses", 
                                                        choices = "None", 
                                                        selected = "None"),
                                     
                                     
                                     selectInput("multi_reg_model", 
                                                 "Regression model:", 
                                                 choices = c("Linear"), 
                                                 selected = "Linear")
                                     
                                   ), 
                                   
                                   mainPanel(
                                     h4("Multivariate Regression Model")
                                   )
                                 )# end multivariate regression 
                                 
                                 
                                 
                       ), #END REGRESSION TAB 
                       
                      
                       
                       #Time Series Forecast
                       tabPanel("Time Series Forecast", 
                                value = "time_series", 
                                h1("Pendinggggg ....")) #End time series forecast
                       
                      
                        
            ) #end analysis view
                      

    ) # end nav page 
  
  )


) #END UI 





