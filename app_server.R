# Server Logic
server <- function(input, output, session) {
  data <- reactive({
    df <- read.csv("bank-full-2025.csv") %>%
      as_tibble() %>%
      mutate(across(where(is.character), as.factor)) %>%
      mutate(across(where(is.integer), as.numeric)) 
    updateSelectInput(session, "job", choices = levels(df$job))
    updateRadioButtons(session, "marital", choices = levels(df$marital))
    updateSelectInput(session, "education", choices = levels(df$education))
    updateSelectInput(session, "contact", choices = levels(df$contact))
    updateSelectInput(session, "month", choices = levels(df$month))
    updateSelectInput(session, "poutcome", choices = levels(df$poutcome))
    df
  })
  
  # Variables Table Data
  variables_data <- data.frame(
    "Variable Name" = c("age", "job", "marital", "education", "default", 
                        "balance", "housing", "loan", "contact", "day_of_week",
                        "month", "duration", "campaign", "pdays", "previous",
                        "poutcome", "y"),
    "Role" = c(rep("Feature", 16), "Target"),
    "Type" = c("Integer", "Categorical", "Categorical", "Categorical", "Binary",
               "Integer", "Binary", "Binary", "Categorical", "Date",
               "Date", "Integer", "Integer", "Integer", "Integer",
               "Categorical", "Binary"),
    "Demographic" = c("Age", "Occupation", "Marital Status", "Education Level", "",
                      "", "", "", "", "", "", "", "", "", "",
                      "", ""),
    "Description" = c(
      "Age",
      "Type of job (admin., blue-collar, etc.)",
      "Marital status (divorced, married, etc.)",
      "Education level",
      "Has credit in default?",
      "Average yearly balance",
      "Has housing loan?",
      "Has personal loan?",
      "Contact communication type",
      "Last contact day of the week",
      "Last contact month",
      "Last contact duration in seconds",
      "Number of contacts during this campaign",
      "Days since last contacted",
      "Number of previous contacts",
      "Outcome of the previous campaign",
      "Has the client subscribed to a term deposit?"
    )
  )
  
  # Ensure Correct Column Names
  colnames(variables_data) <- c("Variable Name", "Role", "Type", "Demographic", "Description")
  
  # Render Enhanced Variables Table
  # Render Enhanced Variables Table with Fixed Column Alignment
  output$variables_table <- renderDataTable({
    datatable(
      variables_data,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,           # Ensures proper column width
        scrollX = TRUE,             # Enables horizontal scrolling for better fit
        columnDefs = list(
          list(width = '150px', targets = "_all")  # Ensures consistent column width
        ),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      rownames = FALSE,
      class = "cell-border stripe hover"
    ) %>%
      formatStyle(
        names(variables_data),
        backgroundColor = 'white', 
        color = 'black',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Variable Name', backgroundColor = '#f2f2f2'
      ) %>%
      formatStyle(
        columns = names(variables_data), 
        'border-bottom' = '2px solid #ddd',
        'padding' = '10px',
        'text-align' = 'left'
      )
  })
  
  # Render Variables Table
  output$variables_table <- renderDataTable({
    variables_data
  }, options = list(pageLength = 20, autoWidth = TRUE, scrollX = TRUE))
 
  
  # Add this logic to the server
  observe({
    req(data())  # Ensure data is loaded
    updateSelectInput(session, "selected_var", choices = colnames(data()))
  })
 
  
  # Add this logic to the server
  observe({
    req(data())  # Ensure data is loaded
    # Use all column names except the specified ones
    selected_cols <- colnames(data())[!colnames(data()) %in% c("day_of_week", "duration", "campaign", "pdays", "day", "previous")]
    updateSelectInput(session, "selected_var", choices = selected_cols)
  })
  
  
  # Distribution Plot
  # Distribution Plot
  output$var_distribution <- renderPlotly({
    df <- data()
    selected_var <- input$selected_var
    
    req(selected_var)  # Ensure variable selection is not NULL
    
    # Custom Binwidths for Different Variables
    binwidth_values <- list(
      "age" = 5,
      "balance" = 1,
      "day" = 1,
      "duration" = 0.25,
      "campaign" = 0.25,
      "pdays" = 500,
      "previous" = 500
    )
    
    # Histogram with Density Curve and Median Line
    if (is.numeric(df[[selected_var]])) {
      plot <- ggplot(df, aes(x = .data[[selected_var]])) +
        geom_histogram(binwidth = binwidth_values[[selected_var]], 
                       fill = "#1F77B4", color = "black", alpha = 0.7) +
        geom_density(aes(y = ..count.. * binwidth_values[[selected_var]]), 
                     color = "orange", size = 1.2) +
        geom_vline(aes(xintercept = median(df[[selected_var]], na.rm = TRUE)),
                   color = "red", linetype = "dashed", size = 1.5) +
        annotate("text", 
                 x = median(df[[selected_var]], na.rm = TRUE), 
                 y = max(table(df[[selected_var]])) * 0.9, 
                 label = paste("Median:", median(df[[selected_var]])), 
                 color = "red", size = 5, fontface = "bold") +
        ggtitle(paste("Distribution of", selected_var)) +
        labs(x = selected_var, y = "Count") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_blank(),
          legend.position = "none"
        )
      
      # Apply Log Scale for Highly Skewed Variables
      if (selected_var %in% c("balance", "duration")) {
        plot <- plot + scale_x_continuous(trans = "log10")
      }
      
    } else {
      plot <- ggplot(df, aes(x = .data[[selected_var]], fill = .data[[selected_var]])) +
        geom_bar(color = "black", alpha = 0.7) +
        ggtitle(paste("Distribution of", selected_var)) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }
    
    ggplotly(plot, tooltip = c("x", "y"))
  })
  
  
  # Summary Table
  output$var_summary <- renderDataTable({
    df <- data()
    selected_var <- input$selected_var
    
    req(selected_var)  # Ensure variable selection is not NULL
    
    if (is.numeric(df[[selected_var]])) {
      summary_data <- data.frame(
        Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
        Value = as.numeric(summary(df[[selected_var]]))
      )
    } else {
      summary_data <- as.data.frame(table(df[[selected_var]]))
      colnames(summary_data) <- c("Category", "Count")
    }
    
    summary_data
  })

  
  # Data Summary Output
  output$summary <- renderPrint({
    data() %>% summary()
  })
  
  # Model Initialization
  trained_model <- reactiveVal(NULL)
  
  observeEvent(input$train, {
    df <- data() %>% 
      filter(!is.na(y)) %>% 
      mutate(y = as.factor(y))  
    
    # Train the LDA model directly without checking for other model types
    model <- train(
      y ~ ., 
      data = df, 
      method = "lda", 
      trControl = trainControl(method = "cv", number = 10, classProbs = TRUE)
    )
    
    trained_model(model)
  })
  
  
  output$model_output <- renderPrint({
    req(trained_model())
    trained_model()$results
  })
  
  output$roc_curve <- renderPlot({
    req(trained_model())
    df <- data() %>% 
      filter(!is.na(y)) %>%
      mutate(y = as.factor(y))
    
    pred_probs <- predict(trained_model(), df, type = "prob")[, "yes"]
    
    if (any(is.na(pred_probs))) {
      showNotification("Warning: Some predicted probabilities are NA.", type = "error")
      return(NULL)
    }
    
    roc_obj <- roc(df$y, pred_probs, levels = rev(levels(df$y)))
    plot(roc_obj, col = "blue", lwd = 2, main = paste("ROC Curve for", input$model_type))
    abline(a = 0, b = 1, col = "gray", lty = 2)
    text(0.7, 0.2, paste("AUC =", round(auc(roc_obj), 3)), col = "red")
  })
  
  # Prediction Logic for User Input
  prediction <- eventReactive(input$predict, {
    req(trained_model())
    new_data <- tibble(
      age = input$age, balance = input$balance, day = input$day, duration = input$duration, 
      campaign = input$campaign, pdays = input$pdays, previous = input$previous, job = input$job, 
      marital = input$marital, education = input$education, default = input$default, 
      housing = input$housing, loan = input$loan, contact = input$contact, 
      month = input$month, poutcome = input$poutcome
    ) %>% 
      replace_na(list(balance = 0, duration = 0, pdays = -1))
    
    predict(trained_model(), new_data, type = "prob")
  })
  
  # Display Prediction Results
  output$result <- renderPrint({
    prediction()
  })
  
  # Probability Visualization for Prediction
  output$probability_plot <- renderPlot({
    req(prediction())
    pred_probs <- prediction()
    prob_df <- tibble(Class = c("No", "Yes"), Probability = as.numeric(pred_probs))
    
    ggplot(prob_df, aes(x = Class, y = Probability, fill = Class)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_manual(values = c("No" = "#E91E63", "Yes" = "#3F51B5")) +
      labs(title = "Subscription Probability", x = "Subscription Decision", y = "Probability") +
      theme_minimal()
  })
  
  
  # Visualization Options for Data Exploration
  output$visualization_plot <- renderPlotly({
    df <- data()

    plot <- switch(input$viz_type_page,
                   "Distribution of y" = ggplot(df, aes(x = y, fill = y)) + 
                     geom_bar(width = 0.6, color = "black") +                  # Adds outline for better clarity
                     scale_fill_manual(values = c("#E91E63", "#3F51B5")) +     # Improved color contrast
                     geom_text(stat = "count", aes(label = ..count..), 
                               vjust = -0.5, color = "black", size = 5) +       # Display count values on top
                     ggtitle("Distribution of Target Variable (y)") +
                     labs(x = "Subscription Status (y)", y = "Count") +         # Clear axis labels
                     theme_minimal() +
                     theme(
                       plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Title centered
                       axis.text = element_text(size = 12),                               # Larger axis text
                       axis.title = element_text(size = 14),                              # Larger axis labels
                       panel.grid.major = element_line(color = "grey90"),                 # Light gridlines
                       panel.grid.minor = element_blank(),                                # Hide minor gridlines
                       legend.position = "top",                                           # Improved legend placement
                       legend.title = element_blank()                                     # Remove legend title for clarity
                     )
                   , 
                   
                   "Distribution of Age" = ggplot(df, aes(x = age)) +
                     geom_histogram(binwidth = 5, fill = "#E91E63", color = "black", alpha = 0.8) +  # Added outline and slight transparency
                     geom_density(aes(y = ..count.. * 5), color = "#3F51B5", size = 1.2) +           # Density curve overlay for trend visualization
                     geom_vline(aes(xintercept = median(age, na.rm = TRUE)),                         # Vertical line for median
                                color = "red", linetype = "dashed", size = 1) +
                     annotate("text", x = median(df$age), y = max(table(df$age)) * 0.9,             # Median label
                              label = paste("Median Age:", median(df$age)), color = "red", size = 4) +
                     ggtitle("Distribution of Age") +
                     labs(x = "Age", y = "Count") +
                     theme_minimal() +
                     theme(
                       plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Centered title
                       axis.text = element_text(size = 12),                               # Improved axis text size
                       axis.title = element_text(size = 14),                              # Improved axis label size
                       panel.grid.major = element_line(color = "grey90"),                 # Subtle grid lines
                       panel.grid.minor = element_blank(),                                # Hide minor gridlines
                       legend.position = "none"                                           # Hide unnecessary legend
                     )
                   ,
                   
                   "Job vs y" = if("job" %in% colnames(df)) {
                     ggplot(df, aes(x = y, fill = y)) +
                       geom_bar(width = 0.6, color = "black", alpha = 0.9) +
                       geom_text(stat = "count", aes(label = comma(..count..)), 
                                 hjust = -0.2, vjust = 0.5, color = "black", size = 4, fontface = "bold") +
                       facet_wrap(~ job, scales = "free_y", ncol = 2) +                     # 6 rows, 2 columns
                       coord_flip() +                                                       # Horizontal bars
                       scale_fill_manual(values = c( "#E91E63", "#3F51B5")) +
                       ggtitle("Job Distribution by Subscription Status") +
                       labs(x = "Subscription Status (y)", y = "Count") +
                       theme_minimal() +
                       theme(
                         plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                         axis.text = element_text(size = 10),                               
                         axis.title = element_text(size = 12),                               
                         panel.spacing = unit(1.5, "lines"),                                # Increase spacing between panels
                         strip.text = element_text(face = "bold", size = 12, margin = margin(b = 10)),
                         legend.position = "top",
                         legend.title = element_blank()
                       )
                   } else { 
                     ggplot() + 
                       ggtitle("Error: 'job' column not found") + 
                       theme_minimal() 
                   }
                   ,
                   
                   "Education vs y" = if("education" %in% colnames(df)) {
                     ggplot(df, aes(x = y, fill = y)) +
                       geom_bar() +
                       facet_wrap(~ education, ncol = 2) +
                       ggtitle("Education Distribution by Subscription Status") +
                       theme_minimal() +
                       labs(x = "Subscription Status (y)", y = "Count") +
                       theme(
                         axis.title.x = element_text(size = 14, face = "bold", vjust = -0.5),
                         axis.title.y = element_text(size = 14, face = "bold"),
                         axis.text.y = element_blank(),  # This removes the y-axis numbers
                         plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
                       )
                   } else {
                     ggplot() + ggtitle("Error: 'education' column not found")
                   }
                   
                   ,
                   
                   ggplot(df, aes(x = balance, fill = y)) +
                     geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.3, position = "identity") +
                     geom_density(alpha = 0.4, color = "black") +
                     scale_x_continuous(trans = "log10") +
                     labs(
                       title = "Balance Distribution by Subscription Status",
                       x = "Balance (Scaled)",
                       y = "Density"
                     ) +
                     theme_minimal() +
                     theme(
                       plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                       axis.title.x = element_text(size = 14, face = "bold", vjust = -0.5),
                       axis.title.y = element_text(size = 14, face = "bold"),
                       axis.text.x = element_blank(),  # Hides x-axis numbers
                       axis.text.y = element_blank(),  # Hides y-axis numbers
                       legend.title = element_text(size = 12, face = "bold")
                     )
                   
                   ,
                   
                   "Campaign and y" = ggplot(df, aes(x = factor(campaign), fill = y)) +
                     geom_bar(position = "fill", color = "black") +  # Shows relative proportions
                     ggtitle("Proportion of Subscription Status by Number of Contacts") +
                     theme_minimal() +
                     labs(x = "Number of Contacts (Campaign)", y = "Proportion") +
                     theme(
                       plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                       axis.title.x = element_text(size = 14, face = "bold", vjust = -0.5),
                       axis.title.y = element_text(size = 14, face = "bold"),
                       axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
                       legend.title = element_text(size = 12, face = "bold"),
                       legend.position = "top"
                     ) +
                     scale_fill_manual(values = c( "#E91E63", "#3F51B5"))  # Green & Orange

                   ,
                   
                   "Pie chart for contact types" = ggplot(df, aes(x = "", fill = contact)) +
                     geom_bar(width = 1) + coord_polar("y") +
                     ggtitle("Distribution of Contact Types") +
                     theme_minimal()      
                   ,
                 
                   "Marital vs y" = ggplot(df, aes(x = marital, fill = y)) +
                     geom_bar(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
                     ggtitle("Marital Status Distribution by Subscription Status") +
                     theme_minimal() +
                     scale_fill_manual(values = c( "#E91E63", "#3F51B5")) +
                     labs(
                       x = "Marital Status",
                       y = "Count"
                     ) +
                     theme(
                       plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                       axis.title.x = element_text(size = 14, face = "bold", vjust = -0.5),
                       axis.title.y = element_text(size = 14, face = "bold"),
                       legend.title = element_text(size = 12, face = "bold")
                     )
                   
                   ,
                   
                   "Default vs y" = ggplot(df, aes(x = default, fill = y)) +
                     geom_bar(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
                     ggtitle("Default Status Distribution by Subscription Status") +
                     theme_minimal() +
                     scale_fill_manual(values = c("#E91E63", "#3F51B5")) +
                     labs(
                       x = "Default Status",
                       y = "Count"
                     ) +
                     theme(
                       plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                       axis.title.x = element_text(size = 14, face = "bold", vjust = -0.5),
                       axis.title.y = element_text(size = 14, face = "bold"),
                       legend.title = element_text(size = 12, face = "bold")
                     )
                   ,
                   
                   "Housing vs y" = ggplot(df, aes(x = housing, fill = y)) +
                     geom_bar(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
                     ggtitle("Housing Loan Status by Subscription Status") +
                     theme_minimal() +
                     scale_fill_manual(values = c("#E91E63", "#3F51B5")) +
                     labs(
                       x = "Housing Loan Status",
                       y = "Count"
                     ) +
                     theme(
                       plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                       axis.title.x = element_text(size = 14, face = "bold", vjust = -0.5),
                       axis.title.y = element_text(size = 14, face = "bold"),
                       legend.title = element_text(size = 12, face = "bold")
                     )
                   ,
                   
                   "Loan vs y" = ggplot(df, aes(x = loan, fill = y)) +
                     geom_bar(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
                     ggtitle("Personal Loan Status by Subscription Status") +
                     theme_minimal() +
                     scale_fill_manual(values = c("#E91E63", "#3F51B5")) +  # Blue & Orange for contrast
                     labs(
                       x = "Personal Loan Status",
                       y = "Count"
                     ) +
                     theme(
                       plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                       axis.title.x = element_text(size = 14, face = "bold", vjust = -0.5),
                       axis.title.y = element_text(size = 14, face = "bold"),
                       legend.title = element_text(size = 12, face = "bold")
                     )
                   ,
                   
                   "Month vs y" = ggplot(df, aes(x = month, fill = y)) +
                     geom_bar(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
                     ggtitle("Subscription Status by Month") +
                     theme_minimal() +
                     scale_fill_manual(values = c("#E91E63", "#3F51B5")) +  # Bright colors
                     labs(
                       x = "Month",
                       y = "Count"
                     ) +
                     theme(
                       plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                       axis.title.x = element_text(size = 14, face = "bold", vjust = -0.5),
                       axis.title.y = element_text(size = 14, face = "bold"),
                       axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
                       legend.title = element_text(size = 12, face = "bold")
                     )
                   
                   ,
                   
                   "Poutcome vs y" = ggplot(df, aes(x = poutcome, fill = y)) +
                     geom_bar(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
                     ggtitle("Previous Outcome Distribution by Subscription Status") +
                     theme_minimal() +
                     scale_fill_manual(values = c("#E91E63", "#3F51B5")) +  # Bright contrasting colors
                     labs(
                       x = "Previous Outcome",
                       y = "Proportion"
                     ) +
                     theme(
                       plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                       axis.title.x = element_text(size = 14, face = "bold", vjust = -0.5),
                       axis.title.y = element_text(size = 14, face = "bold"),
                       axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
                       legend.title = element_text(size = 12, face = "bold")
                     )
                   ,
                    
                   
                   "Day Distribution" = ggplot(df, aes(x = factor(day), fill = y)) +
                     geom_bar(position = "dodge", color = "black", width = 0.8) +
                     scale_fill_manual(values = c("yes" = "#3F51B5", "no" = "#E91E63")) +  # Bright colors
                     ggtitle("Distribution of Subscription Status by Day") +
                     labs(
                       x = "Day of the Month",
                       y = "Count"
                     ) +
                     theme_minimal() +
                     theme(
                       plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                       axis.title.x = element_text(size = 14, face = "bold", vjust = -0.5),
                       axis.title.y = element_text(size = 14, face = "bold"),
                       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "bold")
                     )
                   ,
                   
                   # Flipped Upper Triangular Correlation Matrix
                   
                   "Correlation" = {
                     corr_matrix <- cor(df %>% select_if(is.numeric), use = "complete.obs")
                     
                     # Mask upper triangle values to flip the triangle
                     lower_tri <- corr_matrix
                     lower_tri[upper.tri(lower_tri, diag = TRUE)] <- NA
                     
                     plot_ly(
                       x = colnames(corr_matrix),
                       y = colnames(corr_matrix),
                       z = lower_tri,
                       type = "heatmap",
                       colors = colorRampPalette(c("#B22222", "white", "#1E90FF"))(200), # Red to white to blue
                       zmin = -1,   # Set color scale minimum
                       zmax = 1,    # Set color scale maximum
                       colorbar = list(
                         title = "Correlation",
                         tickvals = seq(-1, 1, 0.2),
                         ticktext = sprintf("%.1f", seq(-1, 1, 0.2)),
                         len = 1,
                         outlinecolor = "black",
                         outlinewidth = 1.5
                       ),
                       showscale = TRUE
                     ) %>%
                       layout(
                         title = list(
                           text = "Mirrored Correlation Matrix",
                           font = list(size = 22, color = "#333333"),
                           x = 0.5,
                           xanchor = "center"
                         ),
                         xaxis = list(
                           title = "Features",
                           tickangle = -45,
                           tickfont = list(size = 12, color = "black"),
                           showgrid = FALSE,
                           zeroline = FALSE
                         ),
                         yaxis = list(
                           title = "Features",
                           tickfont = list(size = 12, color = "black"),
                           showgrid = FALSE,
                           zeroline = FALSE
                         ),
                         annotations = lapply(1:ncol(corr_matrix), function(i) {
                           lapply(1:i, function(j) {
                             list(
                               x = colnames(corr_matrix)[j],
                               y = colnames(corr_matrix)[i],
                               text = sprintf("%.2f", corr_matrix[i, j]),
                               showarrow = FALSE,
                               xanchor = "center",  
                               yanchor = "middle",
                               font = list(color = ifelse(abs(corr_matrix[i, j]) > 0.5, "white", "black"))
                             )
                           })
                         }) %>% unlist(recursive = FALSE),
                         plot_bgcolor = "white",
                         paper_bgcolor = "#ffffff",
                         margin = list(l = 100, r = 100, b = 100, t = 80)
                       )
                   }
                  , 
                   
                   "Contact Type Distribution" = ggplot(df, aes(x = contact, fill = y)) +
                     geom_bar(position = "dodge") +
                     geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
                     scale_fill_manual(values = c("no" = "#E91E63", "yes" = "#3F51B5")) +  # Green for 'no', Yellow for 'yes'
                     ggtitle("Distribution of Contact Types by Subscription Status") +
                     labs(
                       x = "Contact Type",
                       y = "Count"
                     ) +
                     theme_minimal() +
                     theme(
                       plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                       axis.title.x = element_text(size = 14, face = "bold", vjust = -0.5),
                       axis.title.y = element_text(size = 14, face = "bold"),
                       axis.text.y = element_blank()  # <- This hides the y-axis numbers
                     )
                   
    )
    
    ggplotly(plot, tooltip = c("x", "y", "fill", "text"))
  })
  
 
}

