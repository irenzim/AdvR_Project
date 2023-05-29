
# ---------------------- Set Up Environment ---------------------- 
if (!require('pacman')) install.packages('pacman')
pacman::p_load(R6, ranger, caret, dplyr, tibble, readr, ggplot2, tidyr, pROC, magrittr,gridExtra,gbm,Metrics,Rcpp)
setwd("C:/Users/pearly/Desktop/University Materials/02. summer semester 2023/Adv.R/projects_training metric/Final Submission")

# ---------------------- Package, Data, Rcpp ---------------------- 
install.packages("autoforest.tar.gz", repos = NULL, type = "source")
library(MyFirstPackage)
load("medical_train_test.RData")
sourceCpp("DfFunctions.cpp")

# ------------------------- Demo For Rcpp ------------------------- 
summary_df(medical.train)
print(col_names(medical.train))
print(df_dim(medical.train))

# ------------------------- Demo for Package ------------------------- 

### CLASSIFICATION ###

model1.formula <- UCURNINS ~ UMARSTAT + USATMED + URELATE + REGION + 
  FHOSP + FDENT + FEMER + FDOCT + UIMMSTAT + 
  UAGE + U_FTPT + U_WKSLY + U_USHRS + 
  HOTHVAL + HRETVAL + HSSVAL + 
  UBRACE + UEDUC3 + GENDER

trainer_classification <- MyFirstPackage::create_trainer_classification(medical.train, medical.test, "UCURNINS")

results <- create_random_forest(trainer_classification,
                                model1.formula,
                                100,
                                10,
                                5)

?plot_confusion_matrix
trainer_classification$tune_grid
p <- plot_confusion_matrix(trainer_classification$test_data, results$predicted, trainer_classification$labels)
p

?calc_metrics_classification
metrics <- calc_metrics_classification(trainer_classification$labels, results$predicted)
metrics

?calc_balanced_metrics
metrics_balanced <- calc_balanced_metrics_classification(trainer_classification$labels, results$predicted)
metrics_balanced

?calculate_learning_curve
metrics_learning_curve <- calculate_learning_curve(trainer_classification)
metrics_learning_curve

?plot_probs
plot_probs(trainer_classification, results$probs)

### REGRESSION ###

model1.formula <- U_USHRS ~ UMARSTAT + USATMED + URELATE + REGION + 
  FHOSP + FDENT + FEMER + FDOCT + UIMMSTAT + 
  UAGE + U_FTPT + U_WKSLY +  
  HOTHVAL + HRETVAL + HSSVAL + 
  UBRACE + UEDUC3 + GENDER

trainer_regression <- create_trainer_regression(medical.train, medical.test, "U_USHRS")
results <- create_random_forest(trainer_regression,
                                model1.formula,
                                100,
                                10,
                                5)

?calc_metrics_regression
metrics <- calc_metrics_regression(trainer_regression, trainer_regression$model)
metrics
learning_curve_metrics <- calculate_learning_curve(trainer_regression)
learning_curve_metrics
plot_metrics(trainer_regression, learning_curve_metrics)
?actual_vs_predicted
actual_vs_predicted(trainer_regression, trainer_regression$predictions)
