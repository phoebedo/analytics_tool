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



# ui.R

# Define UI for application
fluidPage(
  theme = shinytheme("flatly"),
  # Application title
  titlePanel("Analytics Ultra Pro Max Plus PA+++ Tool"),
  
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
                                 
                                 # Fluid Bucket Top Down
                                 fluidRow(
                                   sidebarPanel(
                                     h4("Contribution"),
                                     sliderInput("top_percent", 
                                                 label = "Top ... %",
                                                 min=1,
                                                 max=100,
                                                 value=10),
                                     # Select input for choosing the category (categorical)
                                     selectInput("contributor", "Of", 
                                                 choices = c("None"), 
                                                 selected = "None"),
                                     
                                     # Checkbox input for selecting the plot type
                                     selectInput("contribute_to", "Contribute To", 
                                                 choices = c("None"), 
                                                 selected = "None")
                                     
                                   ),
                                   mainPanel(
                                     h4("Contribution....")
                                   )
                                   
                                 ) #end Bucket Top Down
                                  
                                 ) # end distribution main panel
                        ), #end Distribution
                        
                       
                       tabPanel("Cohort Analysis", 
                                value = "corr_tab", 
                                h3("Cohort Analysis goes here")
                       ), # end cohort 
                        
                        tabPanel("Regression", 
                                 value = "corr_tab", 
                                 
                                 
                                 fluidRow(
                                   h4("Correlation Overview "),
                                   div(plotOutput("correlation_matrix")) 
                                 ),
                                 
                                 fluidRow(
                                 
                                 sidebarPanel(
                                   h4("Correlation Zoom In"),
                                   
                                   selectInput("corr_independent", 
                                               "Independent variable (X):", 
                                               choices = c("None"), 
                                               selected = "None"),
                                 
                                   selectInput("corr_dependent", 
                                               "Dependent variable (Y):", 
                                               choices = c("None"), 
                                               selected = "None"),
                                   
                                   
                                   selectInput("corr_model", 
                                               "Regression model:", 
                                               choices = c("Linear", "2nd deg. Polynomial", "3rd deg. Polynomial"), 
                                               selected = "Linear")
                                 
                                 ), 
                      
                                 mainPanel(
                                   h4("Correlation Zoom In "),
                                   plotOutput("correlation_plot")
                                   )
                                 )
                       ), # end regression 
                       
                       
                       #Time Series Forecast
                       tabPanel("Time Series Forecast", 
                                value = "time_series", 
                                h1("No idea how....")) #End time series forecast
                       
                      
                        
            ) #end analysis view
                      

    ) # end nav page 
  
  )


) #END UI 





