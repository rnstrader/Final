library(plumber)
library(tidyverse)
library(tidymodels)
diabetes <- readRDS("diabetes.rds")
final_rf_wf <- readRDS("final_rf_wf.rds")

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

#API /pred endpoint
function(HighBP = default_vals$HighBP,
         HighChol = default_vals$HighChol,
         BMI = default_vals$BMI,
         PhysActivity = default_vals$PhysActivity,
         GenHlth = default_vals$GenHlth) {
  new_obs <- tibble(
    HighBP = HighBP,
    HighChol = HighChol,
    BMI = BMI,
    PhysActivity = PhysActivity,
    GenHlth = GenHlth
  )
  
  pred <- predict(final_model, new_obs, type = "prob")
  return(pred)
}

#/info endpoint
pr() |>
  pr_get("/info", function(req, res) {
    list(
      name = "Ryan Strader",
      github_pages = "http://rnstrader.github.io/Final"
    )
  })

#/confusion endpoint
function() {
  preds <- final_model |>
    predict(new_data = diabetes) |>
    bind_cols(diabetes |> select(Diabetes_binary))
  
  cm <- conf_mat(preds, truth = Diabetes_binary, estimate = .pred_class)
  cm_df <- as.data.frame(cm$table)
  
  p <- ggplot(cm_df, aes(x = Prediction, y = Truth, fill = Freq)) + geom_tile() + geom_text(aes(label = Freq), size = 6) + scale_fill_gradient(low = "white", high = "limegreen") + labs(title = "Confusion Matrix for Final Random Forest Model", x = "Predicted Class", y = "Actual Class") + theme_minimal()
  print(p)
}

#Example API calls
#httr::POST("http://127.0.0.1:8000/pred",
#           body = list(HighBP = "No", HighChol = "No", BMI = 22, PhysActivity = "Yes", GenHlth = 1),
#           encode = "json")
#httr::POST("http://127.0.0.1:8000/pred",
#           body = list(HighBP = "Yes", HighChol = "Yes", BMI = 35, PhysActivity = "No", GenHlth = 4),
#           encode = "json")
#httr::POST("http://127.0.0.1:8000/pred",
#           body = list(),
#           encode = "json")

#Starting the plumber api
pr("API.R") |> pr_run(port = 8000)

