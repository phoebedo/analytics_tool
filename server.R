# server.R
source("keys.R")

library(DBI)
library(RMySQL)
library(ggcorrplot)
library(ggplot2)
library(plotly)
library(ggpubr)
library(kableExtra)
library(DT)





server <- function(input, output, session) {
  #Helper functions
  # Function to establish MySQL connection
  db <- dbConnect(
    RMySQL::MySQL(),
    dbname = dbname,   #database name
    host = host,         # host
    username = username, # username
    password = password #password
  )
  
  # Function to fetch data from SsQL query
  query_data <- function(query) {
    tryCatch({
      data <- dbGetQuery(db, query)
      return(data)
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # Reactive expression to store queried data
  queried_data <- reactiveVal(NULL)
  
  
  # Function to update queried_data with SQL query
  observeEvent(input$submit_query, {
    if (nchar(input$sql_query) > 0) {
      data <- query_data(input$sql_query)
      if (!is.null(data)) {
        queried_data(data)
      } else {
        queried_data(NULL)
        showModal(modalDialog(
          title = "Error",
          "An error occurred while executing the SQL query. Please double check your query",
          easyClose = TRUE
        ))
      }
    } else {
      queried_data(NULL)
      showModal(modalDialog(
        title = "Warning",
        "Please enter an SQL query.",
        easyClose = TRUE
      ) ) 
    }
    
    
    output$data_preview <- renderTable({
      if (!is.null(queried_data())) {
        head(queried_data(), input$prev_rows)
      }
    }, striped = T,
    hover = T,
    spacing ="s",
    width = "100%",
    align = "l"
    )
    
    output$data_preview_h3 <- renderText({
      if (!is.null(queried_data())) {"Data Preview"}
    })
    
    
    output$view_analysis_btn <- renderUI({
      if (!is.null(queried_data())) {
        tagList(
          actionButton("view_analysis_btn", "View Analysis")
        )
        
      }
    
    
  }) # Output data preview table
  
    
  })
  
  observeEvent(input$view_analysis_btn, {
    updateNavbarPage(session, "menu_nav", "analysis_view")
  })
  
  output$auto_detect_note <- renderText({
    if (!is.null(queried_data())) {"Note: data types is auto detected"}
  })
  
  
  output$category_select <- renderUI({
    if (!is.null(queried_data())) {
      categorical_data <- Filter(function(x) is.factor(x) | is.character(x), queried_data())
      selectInput("category_select", "Select Category:", choices = names(categorical_data))
    }
  })
  

  # Stats table
  # Continuous var summary statistics table

  output$summary_stats_continous <- renderUI({
    if (!is.null(queried_data())) {
      # cleaning : remove NA
      data <- queried_data()

  numeric_data <- Filter(is.numeric, data)

  stats <- apply(numeric_data, 2, function(x) {
    c(Min = min(x, na.rm = TRUE),
      Q1 = quantile(x, probs = 0.25, na.rm = TRUE),
      Median = median(x, na.rm = TRUE),
      Mean = mean(x, na.rm = TRUE),
      Q3 = quantile(x, probs = 0.75, na.rm = TRUE), 
      Max = max(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE)
    )
  })
  
  
  
  colnames(stats) <- names(numeric_data)
  rownames(stats) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max", "SD")
  stats <- as.data.frame(stats)
  tbl <- stats  %>% round(digits = 2) %>%
    kable(align = "c",
          caption = "Summary - Statistics for Numeric Variables",
          table.attr = "style='width:60%;'") %>%
    kable_styling(bootstrap_options=
                    c("hover","striped","bordered"),
                  position = "center",
                  font_size = 12)

  HTML(tbl)
    }
    
  }) #end summary stats

  # Categorical var summary statistics table
  
  
  
  output$category_table <- renderUI({
    if (!is.null(queried_data())) {
      
      categorical_data <- Filter(function(x) is.factor(x) | is.character(x), queried_data())
      # 
      category <- input$category_select
      
      
      # 
      # categorical_data[[category]]
      # 
      # Calculate frequencies for the selected category
      freq <- table(categorical_data[[category]])

      category_table <- data.frame(
        Frequency = round(as.numeric(freq),0),
        Cumulative_Frequency = cumsum(freq),
        Probability = round(as.numeric(prop.table(freq)),2),
        Cumulative_Probability = round(cumsum(prop.table(freq)),2)

      )
      

      category_table <- category_table %>%
        kable(align = "c",
              caption = "Summary - Categorical Data Frequency",
              table.attr = "style='width:60%;'") %>%
        kable_styling(bootstrap_options=
                        c("hover","striped","bordered"),
                      position = "center",
                      font_size = 12)

      HTML(category_table)

      
    }
  }) # end cat summary
  
  
  # output$correlation_matrix <- renderPlot({
  #   if (!is.null(queried_data())) {
  #     numeric_data <- Filter(is.numeric, queried_data())
  #     cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
  #     ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", outline.col = "white",  ggtheme = ggplot2::theme_gray,
  #                colors = c("#6D9EC1", "white", "#E46726"), lab=T)
  #   }
  # }) # end corr matrix
  # 
  
  # Update select input on dataset 
  # Populate the choices for selectInput for variable (numeric)
  observe({
    if (!is.null(queried_data())) {
      data <- queried_data()
      numeric_vars <- Filter(is.numeric, data)
      updateSelectInput(session, "variable", choices = names(numeric_vars))
      updateSelectInput(session, "contributor", choices = names(numeric_vars))
      updateSelectInput(session, "contribute_to", choices = names(numeric_vars))
      updateSelectInput(session, "corr_independent", choices = names(numeric_vars))
      updateSelectInput(session, "corr_dependent", choices = names(numeric_vars))
    }
  })
  # Populate the choices for selectInput for category (categorical)
  observe({
    if (!is.null(queried_data())) {
      data <- queried_data()
      categorical_vars <- Filter(function(x) is.factor(x) | is.character(x), queried_data())
      updateSelectInput(session, "category", choices = c("None", names(categorical_vars)), selected ="None")
    }
  })
  
  
  
  
  #* Distribution page
  # Render the plot for the selected variable and category
  output$distribution_plot <- renderPlotly({
    if (!is.null(queried_data())) {
      data <- queried_data()
      variable <- input$variable
      category <- input$category
      plot_type <- input$plot_type
      
      
      
      # Create the plot based on the selected plot type
      if (plot_type == "Histogram") {
        if(category == "None"){
          fig_hist <- plot_ly(x =data[[variable]], type = "histogram") %>% layout(bargap=0.1)
          fig_hist
        }else{
          
          fig_hist <- plot_ly(alpha = 0.5)
          for(cat in unique(data[[category]])){
            cat_data <- filter(data, data[[category]]==cat)
            fig_hist <- fig_hist %>% add_histogram(x = cat_data[[variable]], name= cat)
          }
          
          fig_hist <- fig_hist %>% layout(barmode = "stack", bargap = 0.05)
          fig_hist
          
        }
        
      }else{
       if(category=="None"){
         fig_boxplot <- plot_ly(x =data[[variable]], type = "box", name = variable) 
         fig_boxplot
       }else{
         fig_boxplot <- plot_ly(x =data[[variable]], y = data[[category]], type = "box", name = variable) 
         fig_boxplot
       }
        
      }
    

    }
    
    
}) # end distribution plot 
  
  output$correlation_plot <- renderPlot({
    data = queried_data()
    x = input$corr_independent
    y = input$corr_dependent
    
    data %>% ggplot(aes(x=data[[x]], y=data[[y]])) +
      ggtitle(paste(x,"-", y, "Linear Regression"))+
      xlab(paste(x)) +
      ylab(paste(y)) +
      theme(plot.title = element_text(hjust = 0.5))+
      geom_point(alpha=.2) +
      stat_smooth(method="lm", formula = y~x,col= "blue",se=FALSE) +
      stat_regline_equation(label.x=0, label.y=4) +
      stat_cor(aes(label=..rr.label..), label.x=1, label.y=5)
    
  })
  
  
  
  
  
  
  
  
  #* CORRELATION 
  
  output$correlation_matrix <- renderPlot({
    if (!is.null(queried_data())) {
      numeric_data <- Filter(is.numeric, queried_data())
      cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
      cor_matrix %>% round(digits = 1) %>% ggcorrplot(
                 hc.order = TRUE, 
                 type = "lower", 
                 outline.col = "white",  
                 ggtheme = ggplot2::theme_gray, 
                 colors = c("#6D9EC1", "white", "#E46726"),
                 lab=T) 
    }
  }) # end corr matrix
  
  
  # update select input for Independent and Dependent variable 

  
  
  
  
  
  
  #close connection
  
  # onStop(function() {
  #   all_cons <- dbListConnections(MySQL())
  #   
  #   for(con in all_cons){dbDisconnect(con)}
  # })
  
  
  
}

