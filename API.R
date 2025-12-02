library(plumber)
library(tidyverse)
library(tidymodels)
library(ranger)
library(GGally)
library(leaflet)
diabetes <- readRDS("diabetes.rds")
final_rf_wf <- readRDS("final_rf_wf.rds")

#In this API.R file we will be fitting our best model (random forest) to the entire dataset.
#Then we will create an API using this fitted best model.

#First, we will be fitting the best model to the full dataset.
final_rec <- recipe(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + GenHlth, data = diabetes)
final_model <- final_rf_wf |>
  fit(data = diabetes)

#Next, we will Build an API with three endpoints starting with a /pred endpoint below:

#Setting up default values based on full dataset
default_vals <- list(
  HighBP = "Normal",
  HighChol = "Normal",
  BMI = mean(diabetes$BMI),
  PhysActivity = "Yes",
  GenHlth = "Very Good"
)

pr <- plumber$new()

#API /pred endpoint
#* @post /pred
#* @param HighBP The high blood pressure status (e.g., 'Normal', 'High BP').
#* @param HighChol The high cholesterol status (e.g., 'Normal', 'High Chol').
#* @param BMI Body Mass Index (numeric).
#* @param PhysActivity Physical activity status (e.g., 'Yes', 'No').
#* @param GenHlth General health status (e.g., 'Excellent', 'Very Good', etc.).
#* @serializer json list(auto_unbox = TRUE)
function(HighBP = default_vals$HighBP,
         HighChol = default_vals$HighChol,
         BMI = default_vals$BMI,
         PhysActivity = default_vals$PhysActivity,
         GenHlth = default_vals$GenHlth) {
  new_obs <- tibble(
    HighBP = HighBP,
    HighChol = HighChol,
    BMI = as.numeric(BMI),
    PhysActivity = PhysActivity,
    GenHlth = GenHlth
  )
  
  predict(final_model, new_obs, type = "prob")
}

#/info endpoint
#* @get /info
info_function <- function() {
    list(
      name = "Ryan Strader",
      github_pages = "http://rnstrader.github.io/Final"
    )
  }

#/confusion endpoint
#* @get /confusion
#* @serializer contentType list(type='image/png')
confusion_function <- function() {
  preds <- final_model |>
    predict(new_data = diabetes) |>
    bind_cols(diabetes |> select(Diabetes_binary))
  
  cm <- conf_mat(preds, truth = Diabetes_binary, estimate = .pred_class)
  cm_df <- as.data.frame(cm$table)
  
  plot <- ggplot(cm_df, aes(x = Prediction, y = Truth, fill = Freq)) + geom_tile() + geom_text(aes(label = Freq), size = 6) + scale_fill_gradient(low = "white", high = "limegreen") + labs(title = "Confusion Matrix for Final Random Forest Model", x = "Predicted Class", y = "Actual Class") + theme_minimal()
  print(plot)
}

#Example API calls
#httr::POST("http://127.0.0.1:8000/pred",
#           body = list(HighBP = "Normal", HighChol = "Normal", BMI = 22, PhysActivity = "Yes", GenHlth = "Excellent"),
#           encode = "json")
#httr::POST("http://127.0.0.1:8000/pred",
#           body = list(HighBP = "High BP", HighChol = "High Chol", BMI = 35, PhysActivity = "No", GenHlth = "Fair"),
#           encode = "json")
#httr::POST("http://127.0.0.1:8000/pred",
#           body = list(),
#           encode = "json")

#Starting the plumber api
pr$run(host = "0.0.0.0", port = 8000)

