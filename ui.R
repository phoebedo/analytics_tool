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
                        )
                        ,
                        
                    
                        sliderInput("prev_rows", 
                                    label = "Choose number of rows to preview",
                                    min=5,
                                    max=200,
                                    value=10)
                      ),
                      
                      # Main panel for displaying the results
                      mainPanel(
                        
                        # Conditional panel to render content only if data from SQL is available
                         
                           uiOutput("view_analysis_btn"),
                           # Data preview table
                           h3(textOutput("data_preview_h3")),
                           tableOutput("data_preview")
                
                        
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
                                 uiOutput("category_table")),
                        
                        
                        
                        tabPanel("Distribution", 
                                 value = "dist_tab", 
                                 
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
                                  plotOutput("distribution_plot")
                                  
                                 ), 
                                 
                                 # Contribution Top 
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
                                   
                                 )
                                  
                                 ) # end distribution mainpanel
                        ), #end Distribution
                        
                        
                        tabPanel("Correlation", 
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
                       ) # end correlation 
                        
            ) #end analysis view
                      

    ) # end nav page 
  
  )


) #end ui





