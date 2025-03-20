library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinythemes)
library(plotly)

library(readr)
library(DBI)
library(tidyr)
library(purrr)
library(jsonlite)
library(RSQLite)
library(httr) 
library(stringr)


# ✅ Load the Nutrient_Calculator function from the RDS file
Nutrient_Calculator <- read_rds("FOOD nutrition calculator.Rds")
# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Conditional rendering of landing page or app content
  uiOutput("landing_page"),
  
  # Main app UI (hidden initially)
  uiOutput("main_ui")
)

# Server
server <- function(input, output, session) {
  
  # Show Landing Page Initially
  output$landing_page <- renderUI({
    div(
      class = "landing-page",
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100vh; text-align: center;",
      
      # Title above the button
      titlePanel("Welcome to the Daily Nutrition Calculator"),
      
      fluidRow(
        column(12, 
               p("Track your daily calories, macronutrients, fitness progress, and more!"),
               br(), # Adding some space between text and button
               actionButton("get_started", "Get Started", class = "btn-primary", style = "padding: 15px 30px; font-size: 20px;")
        )
      )
    )
  })
  
  # Hide Landing Page and Show Main UI when "Get Started" is clicked
  observeEvent(input$get_started, {
    output$landing_page <- renderUI({})  # Remove landing page
    output$main_ui <- renderUI({
      fluidRow(
        column(3,  # Reduced column width from 4 to 3
               wellPanel(
                 numericInput("weight", "Weight (kg):", value = 70, min = 30, step = 1),
                 numericInput("height", "Height (cm):", value = 170, min = 100, step = 1),
                 numericInput("age", "Age:", value = 25, min = 10, step = 1),
                 selectInput("gender", "Gender:", choices = c("Male", "Female")),
                 selectInput("goal", "Fitness Goal:", 
                             choices = c("Lose Weight", "Maintain Weight", "Build Muscle")),
                 selectInput("activity_level", "Activity Level:", 
                             choices = c("Sedentary", "Lightly Active", "Moderately Active", "Very Active")),
                 
                 # Adjust the actionButton for "Calculate Recommendations"
                 actionButton("calculate", "Calculate Recommendations", 
                              class = "btn-primary", 
                              style = "padding: 10px 15px; font-size: 10px; width: 100%;")
               )
        ),
        
        column(9,  # Adjusted the width of the content area accordingly
               tabsetPanel(
                 tabPanel("Summary",
                          fluidRow(
                            column(6, 
                                   div(style = "padding: 20px; background-color: #f8f9fa; border-radius: 10px; font-size: 18px; line-height: 1.6;",
                                       verbatimTextOutput("nutrition_recommendation")
                                   )
                            ),
                            column(6, plotlyOutput("donut_chart"))
                          )
                 ),
                 
                 tabPanel("Fitness Tips",
                          wellPanel(
                            h4("Fitness Tips:"),
                            uiOutput("fitness_tips")
                          )
                 ),
                 
                 tabPanel("Supplements",
                          wellPanel(
                            h4("Recommended Supplements:"),
                            uiOutput("supplement_recommendation")
                          )
                 ),
                 
                 # Daily Nutrition Calculator Tab
                 tabPanel("Daily Food Calculator",
                          fluidRow(
                            column(12,
                                   wellPanel(
                                     textInput("food_input", "Enter a food name:", placeholder = "e.g., pasta, apple, chicken"),
                                     br(),
                                     # ✅ Dropdown for selecting only one nutrient
                                     selectInput("nutrient_select", "Select a Nutrient:", 
                                                 choices = c("Energy", "Protein", "Calcium.Ca", "Iron.Fe", "Carbohydrate.by.diff", 
                                                             "Caffeine", "Water", "Total.Sugars", "Total.lipid.fat", 
                                                             "Magnesium.Mg", "Potassium.K", "Sodium.Na", "Zinc.Zn"),
                                                 selected = "Energy",  # Default selection
                                                 multiple = FALSE),  # ❌ Only one choice at a time
                                     
                                     br(),
                                     h4("Nutritional Value"),
                                     numericInput("food_servings", "Number of Servings:", value = 1, min = 1, step = 1),
                                    
                                     verbatimTextOutput("nutrition_value"),
                                     actionButton("add_food", "Add Food")
                                   )
                            )
                          ),
                          fluidRow(
                            column(12, DTOutput("nutrition_table"))
                          )
                          
                 )
                 ,
                 
                 # Progress Tracking Tab
                 tabPanel("Progress Tracking",
                          fluidRow(
                            column(6,
                                   wellPanel(
                                     numericInput("progress_weight", "Enter Current Weight (kg):", value = 70, min = 30),
                                     numericInput("progress_calories", "Enter Daily Calories Consumed:", value = 2000, min = 0),
                                     numericInput("progress_protein", "Enter Daily Protein Intake (g):", value = 120, min = 0),
                                     numericInput("progress_carbs", "Enter Daily Carbs Intake (g):", value = 250, min = 0),
                                     numericInput("progress_fat", "Enter Daily Fat Intake (g):", value = 70, min = 0),
                                     actionButton("log_progress", "Log Progress")
                                   )
                            ),
                            column(6, plotlyOutput("progress_chart"))
                          ),
                          fluidRow(
                            column(12, DTOutput("progress_table"))
                          )
                 )
               )
        )
      )
    })
  })
  
  # Nutrition recommendation calculation
  output$nutrition_recommendation <- renderText({
    weight <- input$weight
    height <- input$height
    age <- input$age
    gender <- input$gender
    goal <- input$goal
    activity_level <- input$activity_level
    
    # BMR Calculation
    bmr <- ifelse(gender == "Male", 
                  10 * weight + 6.25 * height - 5 * age + 5,
                  10 * weight + 6.25 * height - 5 * age - 161)
    
    # Activity multiplier
    activity_multiplier <- switch(activity_level,
                                  "Sedentary" = 1.2,
                                  "Lightly Active" = 1.375,
                                  "Moderately Active" = 1.55,
                                  "Very Active" = 1.725)
    
    # Adjust BMR based on goal and activity level
    recommended_calories <- round(bmr * activity_multiplier)
    
    # Macronutrient calculations
    if (goal == "Lose Weight") {
      recommended_calories <- round(recommended_calories - 500)  # Create a calorie deficit
      recommended_protein <- round(weight * 1.2)
    } else if (goal == "Maintain Weight") {
      recommended_calories <- round(recommended_calories)
      recommended_protein <- round(weight * 1.5)
    } else {
      recommended_calories <- round(recommended_calories + 500)  # Calorie surplus for muscle gain
      recommended_protein <- round(weight * 2.0)
    }
    
    recommended_carbs <- round(recommended_calories * 0.5 / 4)
    recommended_fat <- round(recommended_calories * 0.3 / 9)
    
    paste(
      "Recommended Daily Intake (", goal, "):\n",
      "Calories:", recommended_calories, "kcal\n",
      "Protein:", recommended_protein, "g\n",
      "Carbohydrates:", recommended_carbs, "g\n",
      "Fat:", recommended_fat, "g"
    )
  })
  
  # Donut chart of macronutrient breakdown
  output$donut_chart <- renderPlotly({
    values <- c(50, 30, 20)  # Example percentages for Carbs, Protein, Fat
    labels <- c("Carbohydrates", "Protein", "Fat")
    
    plot_ly(labels = labels, values = values, type = "pie", hole = 0.4) %>%
      layout(title = "Macronutrient Breakdown")
  })
  
  # Fitness tips based on goal
  output$fitness_tips <- renderUI({
    goal <- input$goal
    fitness_tips <- switch(goal,
                           "Lose Weight" = list(
                             "Focus on a calorie deficit and incorporate cardio exercises like running or cycling.",
                             "Strength training also helps retain muscle mass.",
                             "Aim for at least 150 minutes of moderate aerobic activity weekly."
                           ),
                           "Build Muscle" = list(
                             "Increase protein intake and prioritize compound movements like squats and bench presses.",
                             "Ensure progressive overload in your training to build strength.",
                             "Focus on lifting heavier weights to stimulate muscle growth."
                           ),
                           "Maintain Weight" = list(
                             "Maintain a balanced diet with steady activity.",
                             "Focus on whole foods and ensure you're getting enough recovery time.",
                             "Include a mix of cardio and strength training in your routine."
                           )
    )
    lapply(fitness_tips, function(tip) tags$li(tip))
  })
  
  # Supplement recommendations based on goal
  output$supplement_recommendation <- renderUI({
    goal <- input$goal
    supplement_recommendations <- switch(goal,
                                         "Lose Weight" = list(
                                           "Green tea extract",
                                           "Whey protein",
                                           "CLA (Conjugated Linoleic Acid)",
                                           "Caffeine for energy and fat burning"
                                         ),
                                         "Build Muscle" = list(
                                           "Whey protein",
                                           "Creatine monohydrate",
                                           "BCAAs (Branched-Chain Amino Acids)",
                                           "Beta-Alanine for improved endurance"
                                         ),
                                         "Maintain Weight" = list(
                                           "Multivitamins",
                                           "Omega-3 fatty acids",
                                           "Probiotics for gut health"
                                         )
    )
    lapply(supplement_recommendations, function(supplement) tags$li(supplement))
  })
  
  # Daily Nutrition Calculator
  # ✅ Reactive Function: Fetches food nutrition using `Nutrient_Calculator()`
  food_nutrition <- reactive({
    req(input$food_input)  # Ensure input is not empty
    Nutrient_Calculator(input$food_input)  # Fetch data (returns a table)
  })
  
  # ✅ Render Selected Nutrient Value (Extract Single Value)
  output$nutrition_value <- renderText({
    nutrition_values <- food_nutrition()
    
    # 🔹 If no data found, return message
    if (is.null(nutrition_values) || nrow(nutrition_values) == 0) {
      return("No data found for the entered food.")
    }
    
    # 🔹 Get the user-selected nutrient
    selected_nutrient <- input$nutrient_select
    
    # 🔹 Ensure the selected nutrient exists in the table
    if (!(selected_nutrient %in% colnames(nutrition_values))) {
      return("Nutrient not available for this food.")
    }
    
    # 🔹 Extract the first numeric value from the selected column
    nutrient_value <- nutrition_values[[selected_nutrient]][1]  # First row of selected nutrient
    
    # 🔹 Ensure it's numeric before displaying
    if (is.na(nutrient_value) || !is.numeric(nutrient_value)) {
      return("Nutrient data is not available.")
    }
    
    # 🔹 Return formatted value
    paste(selected_nutrient, ":", round(nutrient_value, 2))
  })
  

  # ✅ Make `nutrition_values` Reactive
  nutrition_values <- reactive({
    req(input$food_input)  # Ensure food input is provided
    Nutrient_Calculator(input$food_input)  # Fetch nutrition data
  })
  
  # ✅ Reactive Values for Food Log
  food_log <- reactiveVal(data.frame(
    Food = character(), 
    Calories = numeric(), 
    Protein = numeric(), 
    Carbs = numeric(), 
    Fat = numeric(), 
    Servings = numeric(), 
    stringsAsFactors = FALSE
  ))
  
  # ✅ Observe Event for "Add Food" Button
  observeEvent(input$add_food, {
    req(input$food_input, input$food_servings)  # Ensure inputs are not empty
    
    # ✅ Get reactive food nutrient data
    food_data <- nutrition_values()
    
    # ✅ Check if food data was found
    if (is.null(food_data) || nrow(food_data) == 0) {
      showNotification("Food not found!", type = "error")
      return()
    }
    
    # ✅ Ensure required columns exist (Handle missing columns)
    required_columns <- c("Energy", "Protein", "Carbohydrate.by.diff", "Total.lipid.fat")
    for (col in required_columns) {
      if (!(col %in% colnames(food_data))) {
        food_data[[col]] <- 0  # Assign 0 if column is missing
      }
    }
    
    # ✅ Create a new food entry with scaled nutrition based on servings
    new_entry <- data.frame(
      Food = input$food_input,
      Calories = round(food_data$Energy[1] * input$food_servings, 2),
      Protein = round(food_data$Protein[1] * input$food_servings, 2),
      Carbs = round(food_data$Carbohydrate.by.diff[1] * input$food_servings, 2),
      Fat = round(food_data$Total.lipid.fat[1] * input$food_servings, 2),
      Servings = input$food_servings
    )
    
    # ✅ Append the new entry to the existing log
    updated_log <- bind_rows(food_log(), new_entry)  # Uses bind_rows() for better reactivity
    food_log(updated_log)  # ✅ Updates the reactive object correctly
  })
  
  # ✅ Render Food Log Table - Updates Immediately
  output$nutrition_table <- renderDT({
    datatable(food_log(), options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  
  
  # Progress tracking reactiveVal to store logged progress
  progress_log <- reactiveVal(data.frame(
    Date = as.Date(character(0)),
    Weight = numeric(0),
    Calories = numeric(0),
    Protein = numeric(0),
    Carbs = numeric(0),
    Fat = numeric(0)
  ))
  
  observeEvent(input$log_progress, {
    # Log progress
    new_entry <- data.frame(
      Date = Sys.Date(),
      Weight = input$progress_weight,
      Calories = input$progress_calories,
      Protein = input$progress_protein,
      Carbs = input$progress_carbs,
      Fat = input$progress_fat
    )
    
    updated_progress <- rbind(progress_log(), new_entry)
    progress_log(updated_progress)
  })
  
  # Display progress in table
  output$progress_table <- renderDT({
    datatable(progress_log())
  })
  
  # Plot progress over time
  output$progress_chart <- renderPlotly({
    plot_ly(progress_log(), x = ~Date, y = ~Weight, type = "scatter", mode = "lines+markers")
  })
}



# Run the app
shinyApp(ui = ui, server = server)
