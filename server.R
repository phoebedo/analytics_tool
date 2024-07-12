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
library(ggpmisc)



server <- function(input, output, session) {
  #Helper functions
  # Function to establish MySQL connection
  db <- dbConnect(
    RMySQL::MySQL(),
    dbname = dbname,   #database name
    host = host, 
    port = port, # host
    username = username, # username
    password = password #passwords
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
  
  datasetsReactive <- reactiveValues(data_total= NULL, data_filtered_descriptive = NULL)
  
  
  
  # Function to update queried_data with SQL query
  
  observeEvent(input$submit_query, {
    if (nchar(input$sql_query) > 0) {
      data <- query_data(input$sql_query)
      if (!is.null(data)) {
        queried_data(data)
        datasetsReactive$data_total <- data
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
    
    
    output$data_preview <- renderDT({
      if (!is.null(queried_data())) {
       queried_data() %>%
        datatable(extensions  = "Responsive")
      }
     
      
    }
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
    
    
    
    updateAwesomeCheckbox(session,inputId = 'mRedesignSliders',value = FALSE)
   
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
    c(Min = min(x, na.rm = TRUE)  %>% round(digits = 2) ,
      Q1 = quantile(x, probs = 0.25, na.rm = TRUE)  %>% round(digits = 2) ,
      Median = median(x, na.rm = TRUE)  %>% round(digits = 2) ,
      Mean = mean(x, na.rm = TRUE) %>% round(digits = 2) ,
      Q3 = quantile(x, probs = 0.75, na.rm = TRUE) %>% round(digits = 2) , 
      Max = max(x, na.rm = TRUE) %>% round(digits = 2) ,
      SD = sd(x, na.rm = TRUE) %>% round(digits = 2) , 
      Sum = sum(x, na.rm = TRUE) %>% round(digits = 2) , 
      Count = sum(!is.na(x))  %>% round(digits = 0) 
    )
  })
  
  
  
  colnames(stats) <- names(numeric_data)
  rownames(stats) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max", "SD", "Sum","Count")
  stats <- as.data.frame(stats)
  tbl <- stats %>%
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
  
  
  
  #Update selectInput on dataset available
  # Populate the choices for selectInput for variable (numeric)
  observe({
    if (!is.null(queried_data())) {
      data <- queried_data()
      numeric_vars <- Filter(is.numeric, data)
      categorical_vars <- Filter(function(x) is.factor(x) | is.character(x), data)
      
      
      updateSelectInput(session, "variable", choices = names(numeric_vars))
      updateSelectInput(session, "contributor", choices = names(numeric_vars))
      updateSelectInput(session, "contribute_to", choices = names(numeric_vars))
      updateSelectInput(session, "reg_independent", choices = names(numeric_vars))
      updateSelectInput(session, "reg_dependent", choices = names(numeric_vars))
      updateCheckboxGroupInput(session, "multi_reg_predictors", choices = names(numeric_vars), selected = NULL)
      updateCheckboxGroupInput(session, "multi_reg_responses", choices = names(numeric_vars), selected = NULL)
      updateMultiInput(session, "select_num_fields_to_filter", choices = c(names(numeric_vars)), selected =NULL)
      
      
      
      
      
      updateSelectInput(session, "category", choices = c("None", names(categorical_vars)), selected ="None")
      updateSelectInput(session, "regression_cat", choices = c("None", names(categorical_vars)), selected ="None")
      updateMultiInput(session, "select_cat_fields_to_filter", choices = c(names(categorical_vars)), selected =NULL)
      
      
      
      
    }
  })
  
  
  #APPLYING FILTERS----
  slidercolrange <- -2
  
  # observeEvent(input$mNumVarFilter,{
  #   disable("mRedesignSliders")
  #   updateAwesomeCheckbox(session,inputId = 'mRedesignSliders',value = FALSE)
  #   enable("mRedesignSliders")
  # }) 
  # 
  # observeEvent(input$mChrVarFilter,{
  #   disable("mRedesignSliders")
  #   updateAwesomeCheckbox(session,inputId = 'mRedesignSliders',value = FALSE)
  #   enable("mRedesignSliders")
  # }) 
  # 
  
  #######- above multi table datatable end
  
  output$muimultisliderplay <- renderUI({
    if (!is.null(queried_data())) {data <- queried_data()}
    
    tryCatch({
      if (length(input$select_num_fields_to_filter) != 0){
        slider_options <- input$select_num_fields_to_filter
      }
      else{
        return()
      }
      
        # First, create a list of sliders each with a different name
        sliders <- lapply(1:length(slider_options), function(i) {
          if (slidercolrange==12){
            slidercolrange <- 1
          }
          else{
            slidercolrange <- slidercolrange ++ 6
          }
          inputName1A <- slider_options[i]
          
          # if (input$mRedesignSliders == TRUE){
          #   column(slidercolrange+6,sliderInput(inputId = inputName1A, 
          #                                       label = inputName1A, 
          #                                       min=min(datasetsReactive$data_filtered_descriptive()[,inputName1A],na.rm = TRUE), 
          #                                       max=max(datasetsReactive$data_filtered_descriptive()[,inputName1A],na.rm = TRUE),
          #                                       value=c(min(datasetsReactive$data_filtered_descriptive()[[inputName1A]],na.rm = TRUE),
          #                                               max(datasetsReactive$data_filtered_descriptive()[[inputName1A]],na.rm = TRUE)),
          #                                       width = "500px"))
          # }
          # else{
            column(slidercolrange+6,sliderInput(inputId = inputName1A, 
                                                label = inputName1A,
                                                min=min(datasetsReactive$data_total[,inputName1A],na.rm = TRUE),
                                                max=max(datasetsReactive$data_total[,inputName1A],na.rm = TRUE),
                                                value=c(min(datasetsReactive$data_total[[inputName1A]],na.rm = TRUE),
                                                        max(datasetsReactive$data_total[[inputName1A]],na.rm = TRUE)),
                                                width = "500px"))
         # }  
          
          
        })
        # Create a tagList of sliders (this is important)
        do.call(tagList, sliders)
        
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      
    })
    
    
    output$multislidertext <- renderUI({
      tryCatch({
        if (length(input$select_cat_fields_to_filter) != 0){
          slider_optionsTXT <- input$select_cat_fields_to_filter
        }
        else{
          return()
        }
        
        # First, create a list of sliders each with a different name
        sliders <- lapply(1:length(slider_optionsTXT), function(i) {
          if (slidercolrange==12){
            slidercolrange <- 1
          }
          else{
            slidercolrange <- slidercolrange ++ 6
          }
          inputName1ATXT <- slider_optionsTXT[i]
          
          # if (input$mRedesignSliders == TRUE){
          #   mchoice <- as.list(unlist(t(distinct(datasetsReactive$data_filtered_descriptive()[inputName1ATXT]))))
          # }
          # else{
            mchoice <- as.list(unlist(t(distinct(datasetsReactive$data_total[inputName1ATXT]))))
         # }
          
          column(slidercolrange+6,
                 selectInput(inputId = inputName1ATXT, 
                             label = inputName1ATXT, 
                             choices = mchoice,
                             selected = mchoice,
                             multiple = TRUE)) 
          
        })
        # Create a tagList of sliders (this is important)
        do.call(tagList, sliders)
        
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      
    })
    
   #UPDATE FILTERED
    
    datasetsReactive$data_filtered_descriptive <- reactive({
      tryCatch({ 
        data_ <- datasetsReactive$data_total
        slider_options <- slider_options <- input$select_num_fields_to_filter
        # this is how you fetch the input variables from ui 
        for(i in slider_options) {
          
          xxtt<-as.double(eval(parse(text=paste0("input$",i))))
          data_ <- data_[data_[[i]] <= xxtt[2] &                       
                           data_[[i]] >= xxtt[1],]
          
        }
        
        slider_optionsTXT <-  input$select_cat_fields_to_filter
        # this is how you fetch the input variables from ui component component character fields
        for(i in slider_optionsTXT) {
          
          xxttTXT<-eval(parse(text=paste0("input$",i)))
          data_ <- data_[data_[[i]] %in%  xxttTXT,]
        }
        data_
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      return(data_)
    }) 
  
  
  

  #* Distribution page
  # Render the plot for the selected variable and category
  output$distribution_plot <- renderPlotly({
    if (!is.null(queried_data())) {
      data <- datasetsReactive$data_filtered_descriptive()
      variable <- input$variable
      category <- input$category
      plot_type <- input$plot_type
      
      # Create the plot based on the selected plot type
      if (plot_type == "Histogram") {
        # If not category not selected -> show as combined 
        if(category == "None"){
          fig_hist <- plot_ly(x =data[[variable]], type = "histogram") %>% layout(bargap=0.1)
          fig_hist
        }else{ #break down
          
          #Option - stack
          # fig_hist <- plot_ly(alpha = 0.5)
          # for(cat in unique(data[[category]])){
          #   cat_data <- filter(data, data[[category]]==cat)
          #   fig_hist <- fig_hist %>% add_histogram(x = cat_data[[variable]], name= cat)
          # }
          # 
          # fig_hist <- fig_hist %>% layout(barmode = "stack", bargap = 0.05)
          # fig_hist

            
            p <- data %>%
              ggplot(aes(x=data[[variable]])) +
              geom_histogram(alpha=0.6, binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
              facet_wrap(~data[[category]], scales = 'free_x')

            fig_hist<- ggplotly(p)
            fig_hist

        }
        
      }else{
       if(category=="None"){
         ## If not category not selected -> show as combined 
         fig_boxplot <- plot_ly(x =data[[variable]], type = "box", name = variable) 
         fig_boxplot
       }else{
         
         fig_boxplot <- plot_ly(x =data[[variable]], y = data[[category]], type = "box", name = variable) 
         fig_boxplot
         
       }
        
      }
    
    }
    
    
}) # end distribution plot 
  
  #Bucket TOP Down analysis 
  
  output$topdown_table <- renderUI({
    
    if (!is.null(queried_data())) {
      
      data <- queried_data()
      bucket_selected <- as.double(input$bucket_size)
      upper <- 1
      bucket <- bucket_selected/100
      lower <- upper-bucket
      contributor_var = input$contributor
      contributee_var = input$contribute_to
      measure = input$measure_bucket
      
      resdf <- data.frame(paste("Top % of", contributor_var),
                          paste(measure, "of", contributor_var),
                          paste(measure,"of", contributee_var))
      
      names(resdf) <- resdf[1,]
      resdf <- resdf[-1,]
      
      while (lower>0){
        current_bucket <- data %>% filter(data[[contributor_var]] <= quantile(data[[contributor_var]],
                                                                              upper, 
                                                                              na.rm = T),
                                          data[[contributor_var]] >= quantile(data[[contributor_var]],
                                                                             lower,
                                                                             na.rm = T))
        if (measure == "Sum"){
          resdf[nrow(resdf) + 1,] = c((1-lower)*100, 
                                      sum(current_bucket[[contributor_var]],na.rm=T) , 
                                      sum(current_bucket[[contributee_var]], na.rm=T) %>% round(digits = 2))
          
        }else if(measure == "Median"){
          resdf[nrow(resdf) + 1,] = c((1-lower)*100, 
                                      median(current_bucket[[contributor_var]], na.rm=T)  %>% round(digits = 2), 
                                      median(current_bucket[[contributee_var]], na.rm=T) %>% round(digits = 2))
          
          
        }else if(measure == "Average"){
          resdf[nrow(resdf) + 1,] = c((1-lower)*100, 
                                      mean(current_bucket[[contributor_var]], na.rm=T)  %>% round(digits = 2) , 
                                      mean(current_bucket[[contributee_var]], na.rm=T) %>% round(digits = 2))
          
        }
        
        
        
        upper <- lower
        lower <- lower - bucket 
        
        
      }
      
      
      resdf <- resdf %>%
        kable(align = "c",
              caption = "Top Down Result",
              table.attr = "style='width:60%;'") %>%
        kable_styling(bootstrap_options=
                        c("hover","striped","bordered"),
                      position = "center",
                      font_size = 12)
      
      
      
      HTML(resdf)
      
      
      
      
      
      
    }
  }) 
  
  
  
  
  
#* CORRELATION PAGE
  
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
                 lab=T
        ) 
    }
  }) # end corr matrix
  

  
  #Single variable regression
  output$regression_model_single <- renderPlot({
    
    # if(queried_data()){
    #   
    #           data = queried_data()
    #           indp_var = input$corr_independent
    #           dp_var = input$corr_dependent
    #           X = data[[indp_var]]
    #           y = data[[dp_var]]
    #           reg_type = input$corr_model
    #           
    #           
    #           #Linear Model
    #           if(reg_type=="Linear"){
    #             lm_model <- linear_reg() %>%
    #               set_engine("lm") %>%
    #               set_mode("regression") %>%
    #               fit(y~X, data= data)
    #             
    #             
    #             x_range <- seq(min(X), max(X), length.out = 100)
    #             x_range <- matrix(x_range, nrow=100, ncol=1)
    #             xdf <- data.frame(x_range)
    #             colnames(xdf) <- c(indp_var)
    #             
    #             ydf <- lm_model %>% predict(xdf) 
    #             
    #             colnames(ydf) <- c(dp_var)
    #             xy <- data.frame(xdf, ydf) 
    #             
    #             fig <- plot_ly(data, 
    #                            x = ~indp_var, 
    #                            y = ~dp_var, 
    #                            type = 'scatter', 
    #                            alpha = 0.5, 
    #                            mode = 'markers', 
    #                            name = dp_var)
    #             fig <- fig %>% 
    #                     add_trace(data = xy, 
    #                               x = ~indp_var, 
    #                               y = ~dp_var, 
    #                               name = 'Regression Fit',
    #                               mode = 'lines', 
    #                               alpha = 1)
    #             
    #             #render
    #             fig
    #             
    #             }#end linear  
    #           
    #           
    #           
    #           
    #           
    #   
    # }
    
    if(!is.null(queried_data())){
      data = queried_data()
      x = input$reg_independent
      y = input$reg_dependent
      facet_cat = input$regression_cat
      reg_model = input$reg_model
      
      if(reg_model=="Linear"){
        if(facet_cat=="None"){
          reg_plot<-data %>% ggplot(aes(x=data[[x]], y=data[[y]])) +
            ggtitle(paste(x,"-", y, "Linear Regression"))+
            xlab(paste(x)) +
            ylab(paste(y)) +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_point(alpha=.2) +
            stat_smooth(method="lm", formula = y~x,col= "blue",se=FALSE) +
            stat_poly_eq(use_label(c("eq", "R2"))) 
          
          # 
          # 
          # reg_plot<- ggplotly(p)
          reg_plot
        }else{
          reg_plot<-data %>% ggplot(aes(x=data[[x]], y=data[[y]]) )+
            ggtitle(paste(x,"-", y, "Linear Regression"))+
            xlab(paste(x)) +
            ylab(paste(y)) +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_point(alpha=.2) +
            stat_smooth(method="lm", formula = y~x,col= "blue",se=FALSE) +
            stat_poly_eq(use_label(c("eq", "R2"))) +
            facet_wrap(~data[[facet_cat]], scales = "free")
          reg_plot
          
        }
      }#end linear
        
      else if (reg_model== "2nd deg. Polynomial"){
          if(facet_cat=="None"){
            reg_plot<-data %>% ggplot(aes(x=data[[x]], y=data[[y]])) +
              ggtitle(paste(x,"-", y, "Linear Regression"))+
              xlab(paste(x)) +
              ylab(paste(y)) +
              theme(plot.title = element_text(hjust = 0.5))+
              geom_point(alpha=.2) +
              stat_smooth(method="lm", formula = y~poly(x, 2, raw=TRUE),col= "blue",se=FALSE) +
              stat_poly_eq(method="lm", formula = y~poly(x, 2, raw=TRUE),use_label(c("eq", "R2"))) 
            
            # 
            # 
            # reg_plot<- ggplotly(p)
            reg_plot
          }else{
            reg_plot<-data %>% ggplot(aes(x=data[[x]], y=data[[y]]) )+
              ggtitle(paste(x,"-", y, "Linear Regression"))+
              xlab(paste(x)) +
              ylab(paste(y)) +
              theme(plot.title = element_text(hjust = 0.5))+
              geom_point(alpha=.2) +
              stat_smooth(method="lm",formula=y ~ poly(x, 2, raw=TRUE),col= "blue",se=FALSE) +
              stat_poly_eq(method="lm", formula = y~poly(x, 2, raw=TRUE),use_label(c("eq", "R2"))) +
              facet_wrap(~data[[facet_cat]], scales = "free")
            reg_plot
          
          }
        }#end 2nd poly
  
      else if(reg_model=="3rd deg. Polynomial"){
        if(facet_cat=="None"){
          reg_plot<-data %>% ggplot(aes(x=data[[x]], y=data[[y]])) +
            ggtitle(paste(x,"-", y, "Linear Regression"))+
            xlab(paste(x)) +
            ylab(paste(y)) +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_point(alpha=.2) +
            stat_smooth(method="lm", formula = y~poly(x, 3, raw=TRUE),col= "blue",se=FALSE) +
            stat_poly_eq(method="lm", formula = y~poly(x, 3, raw=TRUE),use_label(c("eq", "R2"))) 
          
          # 
          # 
          # reg_plot<- ggplotly(p)
          reg_plot
        }else{
          reg_plot<-data %>% ggplot(aes(x=data[[x]], y=data[[y]]) )+
            ggtitle(paste(x,"-", y, "Linear Regression"))+
            xlab(paste(x)) +
            ylab(paste(y)) +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_point(alpha=.2) +
            stat_smooth(method="lm",formula=y ~ poly(x, 3, raw=TRUE),col= "blue",se=FALSE) +
            stat_poly_eq(method="lm", formula = y~poly(x, 3, raw=TRUE),use_label(c("eq", "R2"))) +
            facet_wrap(~data[[facet_cat]], scales = "free")
          reg_plot
          
        }
        
      }#end 3rd poly
      
    }
    

  }) #end single regression
  
  
  
  
  
  # NEW TAB Q4|Next year 

  #Auto-regressive - no feature
  #Time series - features 
  
  
  
  
  
  
  
  #close DB connection
  
  onStop(function() {
      lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
    }
  )
  
  
}























