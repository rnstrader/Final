library(plumber)
library(tidyverse)
library(tidymodels)

#In this API.R file we will be fitting our best model (random forest) to the entire dataset. 
final_rec <- recipe(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + GenHlth, data = diabetes)
final_rf_fit_full <- final_rf_wf |>
  fit(data = diabetes)
final_rf_fit_full

