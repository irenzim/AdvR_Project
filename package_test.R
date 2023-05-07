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

setwd("/Volumes/HD/GitHub/masters/second semester/master_seminar/2023-04-17_boosting")

load("medical_train_test.RData")


model1.formula <- UCURNINS ~ UMARSTAT + USATMED + URELATE + REGION + 
  FHOSP + FDENT + FEMER + FDOCT + UIMMSTAT + 
  UAGE + U_FTPT + U_WKSLY + U_USHRS + 
  HOTHVAL + HRETVAL + HSSVAL + 
  UBRACE + UEDUC3 + GENDER


set.seed(123456789)
params_rf <- 
  expand.grid(mtry = 15,
              # split rule
              splitrule = "gini",
              # minimum size of a leaf
              min.node.size = 250)

#TRAINING AND PREDICTING

model1 <- train(model1.formula,
                    data = medical.train[1:1000,], 
                    method = "ranger", 
                    num.trees = 100, # default 500
                    # how many cores/threads used
                    num.threads = 4,
                    # which importance measure
                    importance = "impurity",
                    tuneGrid = params_rf,
                    trControl = trainControl(method = "none", classProbs = TRUE))

model1$
probs <- predict(model1,
                 medical.test,
                 type="prob")[, "Yes"]


medical.test["pred"] <-
  predict(model1,
          medical.test %>% 
            select(UMARSTAT, USATMED, URELATE, REGION, 
                   FHOSP, FDENT, FEMER, FDOCT, UIMMSTAT, 
                   UAGE, U_FTPT, U_WKSLY, U_USHRS, 
                   HOTHVAL, HRETVAL, HSSVAL, 
                   UBRACE, UEDUC3, GENDER))

### CONFUSION MATRIX ###

pred <- factor(medical.test$pred)
true <- factor(medical.test$UCURNINS)
cm <- confusionMatrix(pred, true)
cm$overall[["Accuracy"]]
confusion_matrix <- as.data.frame(table(pred, true))
confusion_matrix
ggplot(data = confusion_matrix,
       mapping = aes(x = pred,
                     y = true)) +
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

### AUROC ###

labels <- (medical.test[, "UCURNINS"]) %>% pull
pred <- probs

thresholds <- seq(0, 1, by = 0.1)
roc_data <- roc(predictor=probs, response=labels, thresholds=thresholds)
roc_data$thresholds

plot(roc_data,
     print.auc=TRUE,
     auc.polygon=TRUE,
     legacy.axes=TRUE,
     main="AUROC Curve",
     xlab="False Positive Rate",
     ylab="True Positive Rate")

### METRICS ###

calc_metrics <- function(actual, predicted) {
  
  cm <- confusionMatrix(predicted, actual)
  accuracy <- cm$overall[["Accuracy"]]
  recall <- cm$byClass[["Recall"]]
  precision <- cm$byClass[["Precision"]]
  f1 <- cm$byClass[["F1"]]
  
  return(list(accuracy = accuracy, recall = recall, precision = precision, f1=f1))
}

calc_balanced_metrics <- function(actual, predicted){
  tp <- sum(actual == "Yes" & predicted == "Yes")
  tn <- sum(actual == "No" & predicted == "No")
  fp <- sum(actual == "No" & predicted == "Yes")
  fn <- sum(actual == "Yes" & predicted == "No")
  
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
}

metrics <- calc_balanced_metrics(true, pred)
metrics

### LEARNING CURVE ###

calculate_learning_curve <- function(train_data, test_data, formula, tune_grid) {
  
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
    train_idx <- sample.int(nrow(train_data), size = floor(train_sizes[i] * nrow(train_data)), replace = FALSE)
    training_data <- train_data[train_idx, ]
    
    print(nrow(training_data))
    model <- train(formula,
                  data = training_data, 
                  method = "ranger", 
                  num.trees = 100, # default 500
                  # how many cores/threads used
                  num.threads = 4,
                  # which importance measure
                  importance = "impurity",
                  tuneGrid = tune_grid,
                  trControl = trainControl(method = "none", classProbs = TRUE))
    
    train_preds <- predict(model,
                          training_data %>% 
                          select(UMARSTAT, USATMED, URELATE, REGION, 
                          FHOSP, FDENT, FEMER, FDOCT, UIMMSTAT, 
                          UAGE, U_FTPT, U_WKSLY, U_USHRS, 
                          HOTHVAL, HRETVAL, HSSVAL, 
                          UBRACE, UEDUC3, GENDER))
    
    test_preds <- predict(model,
                          test_data %>% 
                          select(UMARSTAT, USATMED, URELATE, REGION, 
                                  FHOSP, FDENT, FEMER, FDOCT, UIMMSTAT, 
                                  UAGE, U_FTPT, U_WKSLY, U_USHRS, 
                                  HOTHVAL, HRETVAL, HSSVAL, 
                                  UBRACE, UEDUC3, GENDER))
    
  
    training_metrics <- calc_metrics(training_data$UCURNINS, train_preds)
    testing_metrics <- calc_metrics(test_data$UCURNINS, test_preds)
    
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

result <- calculate_learning_curve(medical.train, medical.test, model1.formula, params_rf)


plot_acc <- function(metrics, train_sizes = seq(0.01, 0.9, by = 0.05)){
  
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
  }
plots <- plot_acc(result)
plots
grid.arrange(plots$acc, plots$prec, plots$rec, plots$f1, ncol = 2)

### CLASS PROBABILITIES ###

plot_probs <- function(probs, labels){
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





