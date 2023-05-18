#install.packages("gbm")
#install.packages("Metrics")
library(R6)
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

setwd("C:/Users/pearly/Desktop/University Materials/02. summer semester 2023/Adv.R/projects_training metric")
load("data/medical_train_test.RData")

Trainer_classification <- R6Class(
  "Trainer",
  
  public = list(
    
    # initialize method to create a new instance of the Trainer class
    
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
    
    
    initialize = function(train_data, 
                          test_data,
                          label_column) 
    {
      
      self$train_data <- train_data
      self$test_data <- test_data
      self$label_column <- label_column
      self$labels <- self$test_data[[label_column]]
    },
    
    data_head = function(df){
      head(df)
    },
    
    create_random_forest = function(train_data,
                                    test_data,
                                    formula,
                                    method,
                                    num_trees,
                                    importance,
                                    mtry,
                                    min_node_size)
    {
      self$formula = formula
      self$num_trees = num_trees
      self$method = method
      self$importance = importance
      self$dependent_vars = all.vars(self$formula)[-1]
      
      self$tune_grid <-expand.grid(mtry = mtry,
                                   splitrule="gini",
                                   min.node.size = min_node_size)
      
      model <- train(formula,
                     data = train_data[1:1000,], 
                     method = "ranger", 
                     num.trees = num_trees, 
                     importance = importance,
                     tuneGrid = self$tune_grid,
                     trControl = trainControl(method = "none", classProbs = TRUE))
      
      probs <- predict(model,
                       test_data,
                       type="prob")[, "Yes"]
      
      predicted <-predict(model,
                          test_data %>% 
                            select(!!self$dependent_vars))
      
      return(list(model=model, probs=probs, predicted=predicted))
    },
    
    plot_confusion_matrix = function(data, 
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
    },
    
    calc_balanced_metrics = function(labels, predicted)
    {
      tp <- sum(labels == "Yes" & predicted == "Yes")
      tn <- sum(labels == "No" & predicted == "No")
      fp <- sum(labels == "No" & predicted == "Yes")
      fn <- sum(labels == "Yes" & predicted == "No")
      
      precision_pos <- tp / (tp+fp)
      recall_pos <- tp / (tp+fn)
      f1_pos <- 2 * precision_pos * recall_pos / (precision_pos + recall_pos)
      
      precision_neg <- tn / (tn+fn)
      recall_neg <- tn / (tn+fp)
      f1_neg <- 2 * precision_neg * recall_neg / (precision_neg + recall_neg)
      
      balanced_precision <- (precision_pos + precision_neg) / 2
      balanced_recall <- (recall_pos + recall_neg) / 2
      balanced_f1 <- (f1_pos + f1_neg) / 2
      
      return(list(precision_pos = precision_pos,
                  recall_pos = recall_pos,
                  f1_pos = f1_pos,
                  precision_neg = precision_neg,
                  recall_neg = recall_neg,
                  f1_neg = f1_neg,
                  balanced_precision = balanced_precision,
                  balanced_recall = balanced_recall,
                  balanced_f1 = balanced_f1))
    },
    
    calc_metrics = function(labels, predicted) 
    {
      
      cm <- confusionMatrix(predicted, labels)
      accuracy <- cm$overall[["Accuracy"]]
      recall <- cm$byClass[["Recall"]]
      precision <- cm$byClass[["Precision"]]
      f1 <- cm$byClass[["F1"]]
      
      return(list(accuracy = accuracy, recall = recall, precision = precision, f1=f1))
    },
    
    # called as "learning_curve"
    calculate_learning_curve = function(train_data, test_data, label_column) 
    {
      
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
        model <- train(self$formula,
                       data = train_data[1:1000,], 
                       method = self$method, 
                       num.trees = self$num_trees, # default 500
                       # how many cores/threads used
                       num.threads = self$num_threads,
                       # which importance measure
                       importance = self$importance,
                       tuneGrid = self$tune_grid,
                       trControl = trainControl(method = "none", classProbs = TRUE))
        
        train_preds <- predict(model,
                               train_data %>% 
                                 select(!!self$dependent_vars))
        
        test_preds <- predict(model,
                              test_data %>% 
                                select(!!self$dependent_vars))
        
        
        training_metrics <- self$calc_metrics(train_data[[label_column]], train_preds)
        testing_metrics <- self$calc_metrics(test_data[[label_column]], test_preds)
        
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
    },
    # called as "plot_metrics"
    plot_metrics = function(metrics, train_sizes = seq(0.01, 0.9, by = 0.05))
    {
      
      metrics_df <- as.data.frame(do.call(cbind, metrics))
      metrics_df <- metrics_df %>% mutate_all(as.numeric)
      
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
    },
    
    plot_probs = function(probs, labels)
    {
      df_with_probs <- data.frame(probs = probs, label = labels)
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
    
  )
)


Trainer_regression <- R6Class(
  "Trainer",
  
  public = list(
    
    # initialize method to create a new instance of the Trainer class
    
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
    
    
    initialize = function(train_data, 
                          test_data,
                          label_column) 
    {
      
      self$train_data <- train_data
      self$test_data <- test_data
      self$label_column <- label_column
      self$labels <- self$test_data[[label_column]]
    },
    data_head = function(df){
      head(df)
    },
    create_random_forest = function(train_data,
                                    test_data,
                                    formula,
                                    method,
                                    num_trees,
                                    mtry,
                                    min_node_size)
    {
      self$formula = formula
      self$num_trees = num_trees
      self$method = method
      self$dependent_vars = all.vars(self$formula)[-1]
      self$tune_grid <-expand.grid(mtry = mtry,
                                   splitrule="variance",
                                   min.node.size = min_node_size)
      
      model <- train(formula,
                     data = train_data[1:1000,], 
                     method = "ranger", 
                     num.trees = num_trees, 
                     importance = "impurity",
                     tuneGrid = self$tune_grid)
      
      self$model = model
      metrics <- self$calc_metrics(model)
      self$predictions = metrics$predicted_values
    },
    calc_metrics = function(model){
      train_mse <- model$results$RMSE^2
      train_rmse <-model$results$RMSE
      train_mae <- model$results$MAE
      train_rsquared <- model$results$Rsquared
      
      predicted <- predict(model,
                           self$test_data%>% 
                             select(!!self$dependent_vars))
      
      test_mse <- mse(self$labels, predicted)
      test_rmse <- rmse(self$labels, predicted)
      test_mae <- mae(self$labels, predicted)
      test_rsqaured <- cor(predicted, self$labels) ^ 2
      
      return(list(predicted_values=predicted,
                  train_mse = train_mse,
                  train_rmse = train_rmse,
                  train_mae = train_mae,
                  train_rsquared = train_rsquared,
                  test_mse = test_mse,
                  test_rmse = test_rmse,
                  test_mae = test_mae,
                  test_rsquared = test_rsqaured))
    },
    calculate_learning_curve = function(train_data, test_data) 
    {
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
        train_idx <- sample.int(nrow(train_data), 
                                size=floor(train_sizes[i] * nrow(train_data)), 
                                replace = FALSE)
        training_data <- train_data[train_idx, ]
        
        model <- train(self$formula,
                       data=training_data, 
                       method = "ranger",
                       tuneGrid = self$tune_grid)
        
        metrics <- self$calc_metrics(model)
        
        
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
    },
    plot_metrics = function(metrics, train_sizes = seq(0.1, 0.2, by = 0.1))
    {
      metrics_df <- as.data.frame(do.call(cbind, metrics))
      metrics_df <- metrics_df %>% mutate_all(as.numeric)
      print(metrics_df)
      
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
    },
    actual_vs_predicted = function(predictions){
      
      actual_vs_predicted <- ggplot(data=self$test_data, aes(x=predictions, y=self$labels)) +
        geom_point(color = "#E76F51") +
        geom_abline(intercept=0, slope=1) +
        labs(x="Predicted", y="Actual")
      
      residuals <- self$labels - predictions
      residuals_df <- data.frame(predicted = self$predictions, residuals = residuals)
      residuals_plot <- ggplot(data=residuals_df, aes(x=predicted, y=residuals)) +
        geom_point(color="#E76F51") +
        geom_hline(yintercept=0, linetype="dashed") +
        labs(title="Residual plot",
             x="Predicted value",
             y="Residuals")
      return(list(actual_predicted = actual_vs_predicted, resiudals_plot = residuals_plot))
    }
    
  )
)



       