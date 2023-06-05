

#' @importFrom R6 R6Class
#' @export

trainer_classification <- R6::R6Class(
  "Trainer",

  public = list(
    train_data = NULL,
    test_data = NULL,
    label_column = NULL,
    labels = NULL,
    formula = NULL,
    method = NULL,
    num_trees = NULL,
    importance = NULL,
    tune_grid = NULL,
    dependent_vars = NULL,
    status = NULL,


    initialize = function(train_data,
                          test_data,
                          label_column)
    {

      self$train_data <- train_data
      self$test_data <- test_data
      self$label_column <- label_column
      self$labels <- self$test_data[[label_column]]
      self$status <- "classification"
    }
  )
)

trainer_regression <- R6::R6Class(
  "Trainer",

  public = list(
    train_data = NULL,
    test_data = NULL,
    label_column = NULL,
    labels = NULL,
    formula = NULL,
    method = NULL,
    num_trees = NULL,
    importance = NULL,
    tune_grid = NULL,
    dependent_vars = NULL,
    model = NULL,
    predictions = NULL,
    status = NULL,


    initialize = function(train_data,
                          test_data,
                          label_column)
    {

      self$train_data <- train_data
      self$test_data <- test_data
      self$label_column <- label_column
      self$labels <- self$test_data[[label_column]]
      self$status <- "regression"
    }
  )
)

