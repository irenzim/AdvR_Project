library(ranger)
library(caret)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)
library(tidyr)
library(pROC)
library(magrittr)
library(gridExtra)
library(gbm)
library(Metrics)
# #' @importFrom caret train trainControl
# #' @importFrom stats predict
# #' @importFrom dplyr `%>%` select
# #' @importFrom ggplot2 aes geom_tile geom_text scale_fill_gradient theme_bw theme element_blank element_text labs


#' Create an instance of the regression trainer
#'
#' @param train_data Training data that the user wants to use for training
#' @param test_data Testing data that the user wants to use for testing
#' @param label_column String with a name of the dependent variable
#' @return An instance of the regression trainer
#' @export


create_trainer_regression <- function(train_data, test_data, label_column) {
  trainer_regression$new(train_data = train_data,
                         test_data = test_data,
                         label_column = label_column)
}

#' Create an instance of the classification trainer
#' @param train_data Training data that the user wants to use for training
#' @param test_data Testing data that the user wants to use for testing
#' @param label_column String with a name of the dependent variable
#' @return An instance of the regression trainer
#' @export
create_trainer_classification <- function(train_data, test_data, label_column) {
  trainer_classification$new(train_data = train_data,
                         test_data = test_data,
                         label_column = label_column)
}

#' Apply Logarithm Transformation to a Specific Column
#'
#' This function applies the natural logarithm transformation to a specific column in a data frame, if the column is numeric.
#'
#' @param data The input data frame.
#' @param column The name of the column to apply the transformation on.
#'
#' @return The data frame with the logarithm transformation applied to the specified column (if it is numeric).
#'
#' @examples
#' df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c("x", "y", "z"))
#' df_transformed <- apply_log_transform(df, "a")
#'
#' @export
apply_log_transform <- function(data, column) {
  if (!is.numeric(data[[column]])) {
    warning("The specified column is not numeric. Transformation not applied.")
    return(data)
  }

  data[[column]] <- log(data[[column]])
  return(data)
}

#' Calculate Mean for Numeric Columns
#'
#' This function calculates the mean for each numeric column in a data frame.
#'
#' @param data The input data frame.
#'
#' @return A vector containing the mean values for each numeric column.
#'
#' @examples
#' df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
#' means <- calculate_numeric_column_means(df)
#'
#' @export
calculate_numeric_column_means <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  col_means <- sapply(data[numeric_cols], mean)
  return(col_means)
}

#' Create a random forest model
#'
#' This function creates a random forest model using the ranger algorithm for
#' classification or regression tasks.
#'
#' @param trainer An instance of the classification or regression trainer
#' @param formula The formula specifying the model formula
#' @param num_trees The number of trees to grow
#' @param mtry The number of variables randomly sampled as candidates at each split
#' @param min_node_size The minimum number of observations in a terminal node
#'
#' @return A list containing the trained model and additional information
#' @export
create_random_forest = function(trainer,
                                formula,
                                num_trees,
                                mtry,
                                min_node_size)
{
  trainer$formula = formula
  trainer$num_trees = num_trees
  trainer$dependent_vars = all.vars(trainer$formula)[-1]

  train_data <- trainer$train_data
  test_data <- trainer$test_data

  if (trainer$status == "classification"){

  trainer$tune_grid <-expand.grid(mtry = mtry,
                               splitrule="gini",
                               min.node.size = min_node_size)

  model <- train(formula,
                 data = train_data[1:1000,],
                 method = "ranger",
                 num.trees = num_trees,
                 importance = "impurity",
                 tuneGrid = trainer$tune_grid,
                 trControl = trainControl(method = "none", classProbs = TRUE))

  probs <- predict(model,
                   test_data,
                   type="prob")[, "Yes"]

  predicted <-predict(model,
                      test_data %>%
                        select(!!trainer$dependent_vars))

  return(list(model=model, probs=probs, predicted=predicted))
  }
  else {
    trainer$tune_grid <-expand.grid(mtry = mtry,
                                 splitrule="variance",
                                 min.node.size = min_node_size)

    model <- train(formula,
                   data = train_data[1:1000,],
                   method = "ranger",
                   num.trees = num_trees,
                   importance = "impurity",
                   tuneGrid = trainer$tune_grid)

    trainer$model <- model
    metrics <- calc_metrics_regression(trainer, model)
    predictions <- metrics$predicted_values
    trainer$predictions <- predictions
    return(list(model=model, predictions = predictions))
  }
}

#' Plot Confusion Matrix
#'
#' This function generates a plot of the confusion matrix based on the predicted
#' values and true labels.
#'
#' @param test_data The testing data
#' @param predicted The predicted values
#' @param labels The true labels
#'
#' @return A ggplot object representing the confusion matrix plot
#' @export
plot_confusion_matrix = function(test_data,
                                 predicted,
                                 labels)
{

  confusion_matrix <- as.data.frame(table(predicted, labels))
  p <- ggplot(data = confusion_matrix,
              mapping = aes(x = predicted,
                            y = labels)) +
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    scale_fill_gradient(low = "#655DBB",
                        high = "#ECF2FF") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust=0.5)) +
    labs(title = "Confusion Matrix",
         x = "Predicted class",
         y = "True class")

  return(p)
}

#' Calculate Balanced Metrics for Classification
#'
#' This function calculates various balanced metrics for a binary classification problem,
#' including precision, recall, and F1-score for both positive and negative classes,
#' as well as balanced precision, balanced recall, and balanced F1-score.
#'
#' @param labels The true labels
#' @param predicted The predicted labels
#'
#' @return A list containing the calculated metrics:
#'   \item{precision_pos}{Precision for the positive class}
#'   \item{recall_pos}{Recall for the positive class}
#'   \item{f1_pos}{F1-score for the positive class}
#'   \item{precision_neg}{Precision for the negative class}
#'   \item{recall_neg}{Recall for the negative class}
#'   \item{f1_neg}{F1-score for the negative class}
#'   \item{balanced_precision}{Balanced precision}
#'   \item{balanced_recall}{Balanced recall}
#'   \item{balanced_f1}{Balanced F1-score}
#'
#' @export
calc_balanced_metrics_classification = function(labels, predicted) {
  allowed_labels <- c("Yes", "No")

  if (length(labels) != length(predicted)) {
    stop("Invalid input: labels and predicted should be the same length.")
  }

  if (!all(labels %in% allowed_labels) || !all(predicted %in% allowed_labels)) {
    stop("Invalid input: labels and predicted should only contain 'Yes' and 'No' values.")
  }

  tp <- sum(labels == "Yes" & predicted == "Yes")
  tn <- sum(labels == "No" & predicted == "No")
  fp <- sum(labels == "No" & predicted == "Yes")
  fn <- sum(labels == "Yes" & predicted == "No")

  precision_pos <- tp / (tp + fp)
  recall_pos <- tp / (tp + fn)
  f1_pos <- 2 * precision_pos * recall_pos / (precision_pos + recall_pos)

  precision_neg <- tn / (tn + fn)
  recall_neg <- tn / (tn + fp)
  f1_neg <- 2 * precision_neg * recall_neg / (precision_neg + recall_neg)

  balanced_precision <- (precision_pos + precision_neg) / 2
  balanced_recall <- (recall_pos + recall_neg) / 2
  balanced_f1 <- (f1_pos + f1_neg) / 2

  return(list(
    precision_pos = precision_pos,
    recall_pos = recall_pos,
    f1_pos = f1_pos,
    precision_neg = precision_neg,
    recall_neg = recall_neg,
    f1_neg = f1_neg,
    balanced_precision = balanced_precision,
    balanced_recall = balanced_recall,
    balanced_f1 = balanced_f1
  ))
}


#' Calculate Metrics for Classification
#'
#' This function calculates various evaluation metrics for a binary classification problem,
#' including accuracy, recall, precision, and F1-score.
#'
#' @param labels The true labels
#' @param predicted The predicted labels
#'
#' @return A list containing the calculated metrics:
#'   \item{accuracy}{Accuracy of the classification}
#'   \item{recall}{Recall (true positive rate)}
#'   \item{precision}{Precision (positive predictive value)}
#'   \item{f1}{F1-score (harmonic mean of precision and recall)}
#'
#' @export
calc_metrics_classification = function(labels, predicted)
{

  cm <- confusionMatrix(predicted, labels)
  accuracy <- cm$overall[["Accuracy"]]
  recall <- cm$byClass[["Recall"]]
  precision <- cm$byClass[["Precision"]]
  f1 <- cm$byClass[["F1"]]

  return(list(accuracy = accuracy, recall = recall, precision = precision, f1=f1))
}

#' Calculate Metrics for Regression
#'
#' This function calculates various evaluation metrics for a regression problem,
#' including mean squared error (MSE), root mean squared error (RMSE),
#' mean absolute error (MAE), and coefficient of determination (R-squared).
#'
#' @param trainer The regression trainer object
#' @param model The trained regression model
#'
#' @return A list containing the calculated metrics:
#'   \item{predicted_values}{Predicted values for the test data}
#'   \item{train_mse}{Training mean squared error}
#'   \item{train_rmse}{Training root mean squared error}
#'   \item{train_mae}{Training mean absolute error}
#'   \item{train_rsquared}{Training coefficient of determination (R-squared)}
#'   \item{test_mse}{Test mean squared error}
#'   \item{test_rmse}{Test root mean squared error}
#'   \item{test_mae}{Test mean absolute error}
#'   \item{test_rsquared}{Test coefficient of determination (R-squared)}
#'
#' @export
calc_metrics_regression = function(trainer, model) {
  if (!is.null(model$results) && all(c("RMSE", "MAE", "Rsquared") %in% colnames(model$results))) {
    train_mse <- model$results$RMSE^2
    train_rmse <- model$results$RMSE
    train_mae <- model$results$MAE
    train_rsquared <- model$results$Rsquared
  } else {
    stop("Invalid model object: results should contain RMSE, MAE, and Rsquared columns.")
  }

  predicted <- predict(model, trainer$test_data %>% select(!!trainer$dependent_vars))
  test_mse <- mse(trainer$labels, predicted)
  test_rmse <- rmse(trainer$labels, predicted)
  test_mae <- mae(trainer$labels, predicted)
  test_rsquared <- cor(predicted, trainer$labels) ^ 2


  return(list(
    predicted_values = predicted,
    train_mse = train_mse,
    train_rmse = train_rmse,
    train_mae = train_mae,
    train_rsquared = train_rsquared,
    test_mse = test_mse,
    test_rmse = test_rmse,
    test_mae = test_mae,
    test_rsquared = test_rsquared
  ))
}


#' Calculate Learning Curve
#'
#' This function calculates the learning curve for a classification or regression model.
#' It iteratively trains the model on different subsets of the training data with increasing sizes,
#' and evaluates the model's performance on both the training and test data at each iteration.
#' The calculated metrics differ based on the status of the trainer object.
#' For classification models, the metrics include accuracy, precision, recall, and F1-score.
#' For regression models, the metrics include mean squared error (MSE), root mean squared error (RMSE),
#' mean absolute error (MAE), and coefficient of determination (R-squared).
#'
#' @param trainer The trainer object containing the training and test data
#'
#' @return A list of metrics at different training data sizes:
#' If the status is "classification":
#'   \item{train_acc}{Training accuracy}
#'   \item{train_prec}{Training precision}
#'   \item{train_rec}{Training recall}
#'   \item{train_f1}{Training F1-score}
#'   \item{test_acc}{Test accuracy}
#'   \item{test_prec}{Test precision}
#'   \item{test_rec}{Test recall}
#'   \item{test_f1}{Test F1-score}
#'
#' If the status is "regression":
#'   \item{train_mse}{Training mean squared error}
#'   \item{train_rmse}{Training root mean squared error}
#'   \item{train_mae}{Training mean absolute error}
#'   \item{train_rsquared}{Training coefficient of determination (R-squared)}
#'   \item{test_mse}{Test mean squared error}
#'   \item{test_rmse}{Test root mean squared error}
#'   \item{test_mae}{Test mean absolute error}
#'   \item{test_rsquared}{Test coefficient of determination (R-squared)}
#'
#' @export
calculate_learning_curve = function(trainer)
{
  if (trainer$status == "classification"){
    train_data <- trainer$train_data
    test_data <- trainer$test_data
    label_column <- trainer$label_column
    train_sizes <- seq(0.01, 0.9, by = 0.05)
    metrics <- list(train_acc = list(),
                    train_prec = list(),
                    train_rec = list(),
                    train_f1 = list(),
                    test_acc = list(),
                    test_prec = list(),
                    test_rec = list(),
                    test_f1 = list())

    for (i in seq_along(train_sizes)) {
      train_idx <- sample.int(nrow(train_data), size=floor(train_sizes[i] * nrow(train_data)), replace = FALSE)
      training_data <- train_data[train_idx, ]
      print(nrow(training_data))

      model <- train(trainer$formula,
                     data = training_data,
                     method = "ranger",
                     num.trees = trainer$num_trees,
                     importance = "impurity",
                     tuneGrid = trainer$tune_grid,
                     trControl = trainControl(method = "none", classProbs = TRUE))

      train_preds <- predict(model,
                             training_data %>%
                               select(!!trainer$dependent_vars))

      test_preds <- predict(model,
                            test_data %>%
                              select(!!trainer$dependent_vars))


      training_metrics <- calc_metrics_classification(training_data[[label_column]], train_preds)
      testing_metrics <- calc_metrics_classification(test_data[[label_column]], test_preds)

      metrics$train_acc[[i]] <- training_metrics$accuracy
      metrics$train_prec[[i]] <- training_metrics$precision
      metrics$train_rec[[i]] <- training_metrics$recall
      metrics$train_f1[[i]] <- training_metrics$f1

      metrics$test_acc[[i]] <- testing_metrics$accuracy
      metrics$test_prec[[i]] <- testing_metrics$precision
      metrics$test_rec[[i]] <- testing_metrics$recall
      metrics$test_f1[[i]] <- testing_metrics$f1


    }
    return(metrics)
  }
  else {
    metrics_list <- list(train_mse = list(),
                         train_rmse = list(),
                         train_mae = list(),
                         train_rsquared= list(),
                         test_mse= list(),
                         test_rmse = list(),
                         test_mae= list(),
                         test_rsquared = list())

    train_sizes <- seq(0.1, 0.2, by = 0.1)
    for (i in seq_along(train_sizes)) {
      train_idx <- sample.int(nrow(trainer$train_data),
                              size=floor(train_sizes[i] * nrow(trainer$train_data)),
                              replace = FALSE)
      training_data <- trainer$train_data[train_idx, ]
      print(nrow(training_data))

      model <- train(trainer$formula,
                     data=training_data,
                     method = "ranger",
                     tuneGrid = trainer$tune_grid)


      metrics <- calc_metrics_regression(trainer, model)


      metrics_list$train_mse[[i]] <- metrics$train_mse
      metrics_list$train_rmse[[i]] <- metrics$train_rmse
      metrics_list$train_mae[[i]] <- metrics$train_mae
      metrics_list$train_rsquared[[i]] <- metrics$train_rsquared

      metrics_list$test_mse[[i]] <- metrics$test_mae
      metrics_list$test_rmse[[i]] <- metrics$test_rmse
      metrics_list$test_mae[[i]] <- metrics$test_mae
      metrics_list$test_rsquared[[i]] <- metrics$test_rsquared
    }

    return(metrics_list)
  }
}


#' Plot Metrics
#'
#' This function generates plots to visualize the metrics calculated during the learning curve analysis.
#' The plots differ based on the status of the trainer object (classification or regression).
#' For classification models, the plots include accuracy, precision, recall, and F1-score.
#' For regression models, the plots include mean squared error (MSE), root mean squared error (RMSE),
#' mean absolute error (MAE), and coefficient of determination (R-squared).
#'
#' @param trainer The trainer object containing the training and test data
#' @param metrics The metrics calculated during the learning curve analysis
#'
#' @return If the status is "classification":
#'   \item{acc}{Accuracy plot}
#'   \item{prec}{Precision plot}
#'   \item{rec}{Recall plot}
#'   \item{f1}{F1-score plot}
#'
#' If the status is "regression":
#'   \item{metrics_df}{Data frame of all metrics}
#'   \item{p_mse}{MSE plot}
#'   \item{p_rmse}{RMSE plot}
#'   \item{p_mae}{MAE plot}
#'   \item{p_rsquared}{R-squared plot}
#'
#' @export
plot_metrics = function(trainer, metrics)
{

  metrics_df <- as.data.frame(do.call(cbind, metrics))
  metrics_df <- metrics_df %>% mutate_all(as.numeric)

  if (trainer$status == "classification") {
    train_sizes = seq(0.01, 0.9, by = 0.05)
    p_acc <-  ggplot(metrics_df, aes(x=train_sizes)) +
      geom_line(aes(y=train_acc, color="train_acc")) +
      geom_line(aes(y=test_acc, color="test_acc")) +
      scale_color_manual(values = c("train_acc"="blue", "test_acc"="red"),
                         guide = guide_legend(reverse = TRUE)) +
      xlab("Fraction of training data") +
      ylab("Accuracy")

    p_prec <-  ggplot(metrics_df, aes(x=train_sizes)) +
      geom_line(aes(y=train_prec, color="train_prec")) +
      geom_line(aes(y=test_prec, color="test_prec")) +
      scale_color_manual(values = c("train_prec"="blue", "test_prec"="red"),
                         guide = guide_legend(reverse = TRUE)) +
      xlab("Fraction of training data") +
      ylab("Preicision")

    p_rec <-  ggplot(metrics_df, aes(x=train_sizes)) +
      geom_line(aes(y=train_rec, color="train_rec")) +
      geom_line(aes(y=test_rec, color="test_rec")) +
      scale_color_manual(values = c("train_rec"="blue", "test_rec"="red"),
                         guide = guide_legend(reverse = TRUE)) +
      xlab("Fraction of training data") +
      ylab("Recall")

    p_f1 <-  ggplot(metrics_df, aes(x=train_sizes)) +
      geom_line(aes(y=train_f1, color="train_f1")) +
      geom_line(aes(y=test_f1, color="test_f1")) +
      scale_color_manual(values = c("train_f1"="blue", "test_f1"="red"),
                         guide = guide_legend(reverse = TRUE)) +
      xlab("Fraction of training data") +
      ylab("F1")


    return (list(acc =p_acc, prec = p_prec, rec=p_rec, f1=p_f1))
  }
  else {
    train_sizes <- seq(0.1, 0.2, by = 0.1)
    p_mse <-  ggplot(metrics_df, aes(x=train_sizes)) +
      geom_line(aes(y=train_mse, color="train_mse")) +
      geom_line(aes(y=test_mse, color="test_mse")) +
      scale_color_manual(values = c("train_mse"="blue", "test_mse"="red"),
                         guide = guide_legend(reverse = TRUE)) +
      xlab("Fraction of training data") +
      ylab("MSE")

    p_rmse <-  ggplot(metrics_df, aes(x=train_sizes)) +
      geom_line(aes(y=train_rmse, color="train_rmse")) +
      geom_line(aes(y=test_rmse, color="test_rmse")) +
      scale_color_manual(values = c("train_rmse"="blue", "test_rmse"="red"),
                         guide = guide_legend(reverse = TRUE)) +
      xlab("Fraction of training data") +
      ylab("RMSE")

    p_mae <-  ggplot(metrics_df, aes(x=train_sizes)) +
      geom_line(aes(y=train_mae, color="train_mae")) +
      geom_line(aes(y=test_mae, color="test_mae")) +
      scale_color_manual(values = c("train_mae"="blue", "test_mae"="red"),
                         guide = guide_legend(reverse = TRUE)) +
      xlab("Fraction of training data") +
      ylab("MAE")

    p_rsquared <-  ggplot(metrics_df, aes(x=train_sizes)) +
      geom_line(aes(y=train_rsquared, color="train_rsquared")) +
      geom_line(aes(y=test_rsquared, color="test_rsquared")) +
      scale_color_manual(values = c("train_rsquared"="blue", "test_rsquared"="red"),
                         guide = guide_legend(reverse = TRUE)) +
      xlab("Fraction of training data") +
      ylab("Rsquared")

    return (list(metrics_df=metrics_df,
                 p_mse=p_mse,
                 p_rmse=p_rmse,
                 p_mae=p_mae,
                 p_rsquared=p_rsquared))
  }
}
#' Actual vs. Predicted Plot and Residual Plot
#'
#' This function generates two plots to compare the actual values with the predicted values and visualize the residuals.
#' The first plot shows the actual values on the y-axis and the predicted values on the x-axis, with points indicating the data points.
#' The second plot displays the residuals (the differences between the actual and predicted values) on the y-axis and the predicted values on the x-axis.
#' The residual plot helps analyze the distribution of residuals and identify any patterns or trends.
#'
#' @param trainer The trainer object containing the test data and actual labels
#' @param predictions The predicted values obtained from the trained model
#'
#' @return A list containing the following plots:
#'   \item{actual_predicted}{Actual vs. Predicted plot}
#'   \item{residuals_plot}{Residual plot}
#'
#' @export
actual_vs_predicted = function(trainer, predictions){

  actual_vs_predicted <- ggplot(data=trainer$test_data, aes(x=predictions, y=trainer$labels)) +
    geom_point(color = "#E76F51") +
    geom_abline(intercept=0, slope=1) +
    labs(x="Predicted", y="Actual")

  residuals <- trainer$labels - predictions
  residuals_df <- data.frame(predicted = trainer$predictions, residuals = residuals)
  residuals_plot <- ggplot(data=residuals_df, aes(x=predicted, y=residuals)) +
    geom_point(color="#E76F51") +
    geom_hline(yintercept=0, linetype="dashed") +
    labs(title="Residual plot",
         x="Predicted value",
         y="Residuals")

  return(list(actual_predicted = actual_vs_predicted, resiudals_plot = residuals_plot))
}

#' Probability Distribution Plot
#'
#' This function generates a histogram plot to visualize the distribution of predicted probabilities for each class in a classification problem.
#' The plot displays the predicted probabilities on the x-axis and the count of data points on the y-axis.
#' The histogram bins represent the predicted probability ranges, and each class is filled with a different color.
#'
#' @param trainer The trainer object containing the true labels
#' @param probs The predicted probabilities obtained from the trained model
#'
#' @return A histogram plot visualizing the distribution of predicted probabilities
#'
#' @export
plot_probs = function(trainer, probs)
{
  df_with_probs <- data.frame(probs = probs, label = trainer$labels)
  p <- ggplot(df_with_probs, aes(x=probs, fill=label)) +
    geom_histogram(position="stack", bins=30, breaks=seq(from=0.05, to=1, by=0.05)) +
    scale_x_continuous(limits=c(0.0, 1.0),
                       expand=expansion(mult=0,
                                        add=0)) +
    scale_y_continuous(expand=expansion(mult=0,
                                        add=0))


  labs(x="Predicted probability", y="Count")
  return(p)
}

