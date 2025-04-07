# Load Required Libraries
library(shiny)
library(shinythemes)
library(plotly)
library(DT)
library(dplyr)
library(tibble)
library(caret)
library(pROC)
library(tidyr)


# UI Layout
ui <- fluidPage(
  # Custom Header Outside NavbarPage
  fluidRow(
    column(12, 
           div(
             h2("Bank Campaign Success Predictor Using LDA", 
                style = "color: #FFFFFF; text-align: center; margin: 0; padding: 15px; font-weight: bold;"),
             style = "background: linear-gradient(to right, #2E86C1, #21618C); 
                 border-radius: 10px; 
                 margin-bottom: 15px; 
                 box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2);"
           )
    )
  ),
  
  # Navbar Page with Tabs
  navbarPage(
    theme = shinytheme("flatly"),
    
    # CSS Styling for Tabs
    tags$style(HTML("
      .nav-tabs > li > a {
        font-size: 16px;        /* Increase font size */
        font-weight: bold;      /* Bold font */
        margin-right: 8px;      /* Spacing between tabs */
        padding: 10px 15px;     /* Increase padding */
        color: #FFFFFF;         /* Text color */
        background-color: #34495E; /* Tab background */
      }
      .nav-tabs > li.active > a {
        background-color: #2E86C1; /* Active tab color */
        color: white;
      }
      .nav-tabs > li > a:hover {
        background-color: #5A6268; /* Hover color for tabs */
        color: white;
      }
    "))
  
  
  
  
  ,
  tabPanel("Brief Description",
           fluidPage(
             titlePanel("A Brief Description of the Application"),
             fluidRow(
               column(12,
                      h4("This Shiny application, titled 'Bank Campaign Success Predictor Using LDA', analyzes and predicts the success of marketing campaigns using the data from the Bank Marketing Dataset. The primary goal is to determine whether a client will subscribe to a term deposit based on demographic, financial, and campaign-related features."),
                      br(),
                      p("The app includes five main components:"),
                      tags$ul(
                        tags$li(strong("Data Overview:"), " An overview of the dataset, including variable descriptions and summaries."),
                        tags$li(strong("Variable Analysis:"), " Visualizes selected variable distributions and provides summary statistics."),
                        tags$li(strong("Variables vs. Target Variable:"), " Shows the relationship between variables and the target variable, including a correlation matrix."),
                        tags$li(strong("Modeling:"), " Uses Linear Discriminant Analysis (LDA) to predict client subscription success and visualize model performance with an ROC curve."),
                        tags$li(strong("Prediction:"), " Predicts client subscription likelihood based on user input, displaying probabilities visually.")
                      ),
                      br(),
                      p("Overall, the application serves as an interactive tool for data exploration, model training, and predictive analysis, providing valuable insights into campaign success factors.")
               )
             )
           )
  )
  
  ,
  tabPanel("Data Overview",
           HTML('<h2 style="color:#1E90FF; font-weight:bold; text-align:left;">A Brief Description of the Data</h2>'),
           p("The dataset pertains to direct marketing campaigns conducted by a Portuguese banking institution. 
             These campaigns were carried out via phone calls, often requiring multiple contacts with the same 
             client to determine whether they would subscribe ('yes') or not ('no') to the bank's term deposit product."),
           
           HTML('<h2 style="color:#1E90FF; font-weight:bold; text-align:left;">Dataset Link</h2>'),
           tags$a(href = "https://archive.ics.uci.edu/dataset/222/bank+marketing", 
                  "Bank Marketing Dataset", target = "_blank", 
                  style = "color:#FFC107; font-weight:bold;"),
           
           hr(),
           
           HTML('<h2 style="color:#1E90FF; font-weight:bold; text-align:left; margin-top:20px;">Variables Table</h2>'),
           DTOutput("variables_table"),
           
           HTML('<h2 style="color:#1E90FF; font-weight:bold; text-align:left;">Dataset Summary</h2>'),
           mainPanel(
             verbatimTextOutput("summary")
           )
  ),
  
  # Add new tab panel for variable distribution and statistics
  tabPanel("Variable Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput("selected_var", "Select Variable", choices = NULL)
             ),
             mainPanel(
               plotlyOutput("var_distribution"),
               DTOutput("var_summary")
             )
           )
  ),
  
  tabPanel(" Variables vs. Target Variable ",
           sidebarLayout(
             sidebarPanel(
               selectInput("viz_type_page", "Select Visualization Type", 
                           choices = c("Distribution of y", "Correlation", 
                                       "Education vs y", 
                                       "Marital vs y", "Default vs y", "Housing vs y", 
                                       "Loan vs y", "Month vs y", "Poutcome vs y"))
             ),
             mainPanel(
               plotlyOutput("visualization_plot")
             )
           )
  ),
  
  tabPanel("Modeling",
           sidebarLayout(
             sidebarPanel(
               selectInput("model_type", "The model used to predict the success of client subscription to a term deposit is:", 
                           choices = c("LDA - Linear Discriminant Analysis")),
               actionButton("train", "Train Model", class = "btn btn-primary btn-lg")
             ),
             mainPanel(
               verbatimTextOutput("model_output"),
               plotOutput("roc_curve")
             )
           )
  )
  
  ,
  
  tabPanel("Prediction",
           sidebarLayout(
             sidebarPanel(
               sliderInput("age", "Age", min = 18, max = 95, value = 18, step = 1),
               sliderInput("day", "Day", min = 1, max = 31, value = 1, step = 1),
               sliderInput("duration", "Duration", min = 0, max = 1500, value = 0, step = 100),
               sliderInput("campaign", "Campaign", min = 1, max = 63, value = 1, step = 1),
               sliderInput("pdays", "Pdays", min = -1, max = 871, value = 0, step = 10),
               selectInput("education", "Education", choices = c("Loading...")),
               selectInput("contact", "Contact Type", choices = c("Loading...")),
               selectInput("month", "Last Contact Month", choices = c("Loading...")),
               selectInput("poutcome", "Previous Outcome", choices = c("Loading...")),
               selectInput("job", "Job", choices = c("Loading...")),
               numericInput("previous", "Previous", value = 0, min = 0, max = 275),
               numericInput("balance", "Balance", value = 0, min = -8019, max = 102127),
               radioButtons("marital", "Marital Status", choices = c("Loading..."), inline = TRUE),
               radioButtons("default", "Default", choices = c("yes", "no"), inline = TRUE),
               radioButtons("housing", "Housing Loan", choices = c("yes", "no"), inline = TRUE),
               radioButtons("loan", "Personal Loan", choices = c("yes", "no"), inline = TRUE),
               
               actionButton("predict", "Predict Subscription", class = "btn btn-success btn-lg")
             ),
             mainPanel(
               verbatimTextOutput("result"),
               plotOutput("probability_plot")
             )
           )
  )
))



