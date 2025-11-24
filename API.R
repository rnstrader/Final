library(plumber)
library(tidyverse)
library(tidymodels)

#In this API.R file we will be fitting our best model (random forest) to the entire dataset.
#Then we will create an API using this fitted best model.

#First, we will be fitting the best model to the full dataset.
final_rec <- recipe(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + GenHlth, data = diabetes)
final_model <- final_rf_wf |>
  fit(data = diabetes)
final_model

#Next, we will Build an API with three endpoints starting with a /pred endpoint below:

#Setting up default values based on full dataset
default_vals <- list(
  HighBP = "No",
  HighChol = "No",
  BMI = mean(diabetes$BMI),
  PhysActivity = "Yes",
  GenHlth = "Very Good"
)

#API
function(HighBP = default_vals$HighBP)
