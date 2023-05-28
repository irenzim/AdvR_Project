sourceCpp("DfFunctions.cpp")

setwd("C:/Users/pearly/Desktop/University Materials/02. summer semester 2023/Adv.R/projects_training metric/Final Submission")
load("medical_train_test.RData")

summary_df(medical.train)
print(columnNames(medical.train))
print(df_dim(medical.train))