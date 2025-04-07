# Predictive Analysis of Marketing Campaign Success in the Banking Sector

## Overview
This project aims to predict customer subscription to term deposits based on demographic, financial, and campaign-related features. The primary focus is to identify key factors influencing successful marketing campaigns using machine learning models such as Decision Tree, LDA, QDA, and Naive Bayes. Among these, LDA demonstrated the best performance, and thus the Shiny app primarily utilizes this model for prediction.

## R Markdown File
The analysis and model training process are documented in a dedicated R Markdown file. This file provides a comprehensive walkthrough of data preprocessing, exploratory data analysis, model training, and evaluation steps. It includes code snippets and outputs to enhance reproducibility.

## Shiny App
The project features a Shiny application titled **'Bank Campaign Success Predictor Using LDA'**, which allows users to interactively analyze data, visualize relationships between variables, and predict the likelihood of client subscription.

#### UI and Server Structure
The Shiny application is structured into two main parts:

1. **UI (User Interface):**
   - Uses the **shiny** and **shinythemes** libraries to create a dynamic and responsive interface.
   - Utilizes **Plotly** for interactive plots and **DT** for displaying data tables.
   - The UI consists of tabs for Brief Description, Data Overview, Variable Analysis, Variable vs. Target Variable, Modeling, and Prediction.

2. **Server Logic:**
   - The server processes user inputs and dynamically updates the outputs.
   - Implements LDA for predicting subscription success and generates visualizations for data analysis and model evaluation.

## Shiny App Components
The application is structured into the following key parts:

1. **Brief Description:** Overview of the application and its objectives.
2. **Data Overview:** Displays the dataset used for analysis, including variable descriptions and summaries.
3. **Variable Analysis:** Visualizes selected variable distributions and provides summary statistics.
4. **Variables vs. Target Variable:** Analyzes the relationship between chosen variables and the target variable (subscription), including a correlation matrix.
5. **Modeling:** Utilizes LDA to predict client subscription success, with model performance visualized through an ROC curve.
6. **Prediction:** Allows users to input demographic and financial data to predict the likelihood of subscription success.

## How to Run Locally
1. Clone the repository:
   ```bash
   git clone https://github.com/Mohammedsaif2030/Predictive-Analysis-of-Marketing-Campaign-Success-in-the-Banking-Sector.git
   ```
2. Set the working directory:
   ```r
   setwd('path/to/app')
   ```
3. Install required packages:
   ```r
   install.packages(c('shiny', 'shinythemes', 'plotly', 'DT', 'dplyr', 'tibble', 'caret', 'pROC', 'tidyr'))
   ```
4. Run the application:
   ```r
   shiny::runApp()
   ```
## Live App
You can access the deployed Shiny app using the following link:
[Bank Campaign Success Predictor](https://mohammed-saif-alotaibi.shinyapps.io/fainlproject/)

## Technology Stack
- **R**
- **Shiny**
- **LDA (Linear Discriminant Analysis)**
- **Plotly** for interactive visualizations
- **DT** for interactive tables
- **pROC** for ROC curve visualization
- **dplyr**, **tibble**, **caret**, **tidyr** for data manipulation and modeling

## Dataset
The dataset used in this application is the **Bank Marketing Dataset** (bank-full-2025.csv), which contains information about clients contacted during marketing campaigns conducted by a Portuguese banking institution. The target variable indicates whether a client subscribed to a term deposit (yes/no).

You can find more information about the dataset at the following link: [Bank Marketing Dataset](https://archive.ics.uci.edu/dataset/222/bank+marketing)

## License
This project is licensed under the MIT License.
