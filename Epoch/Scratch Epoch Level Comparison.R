################################################################
##        Scratch Epoch level prediction performance          ##
##        Di, Junrui 04/13/2020                               ##
################################################################


rm(list = ls())
dat = readr::read_csv("/Volumes/PfIRe-SQUAD/scratch_dev/model_development/model_training/trained_models/random_forest/20hz/FINAL_MODEL_RESULTS_2_7_20/per_subject_validation_performance.csv")

## Summarized the performance
library(tableone)
vars = names(dat)[2:11]
tab1 = CreateContTable(vars = vars, data = dat)
tab1.1 = print(tab1)

## Total number of minutes
sum(dat$total_data_contributed_minutes)
# [1] 753.2

write.csv(tab1.1, file = "~/Pfizer/DMTI CT-44 Scratch Sleep Team - Scratch and Sleep Methods Paper/QCed Plots/results/scratchepochlevel/SummaryStatistics.csv")


