#########################################################
##            Epoch Level Sleep Compare                ##
##            Di, Junrui 08/14/2020                    ##
#########################################################

rm(list = ls())
library(dplyr)
library(readr)
library(caret)
library(lubridate)

load("~/OneDrive - Pfizer/JDi/SQUAD/SAP/sleep/data/Sleep_PSGStage.rda") ## Epoch Level PSG Data
names(psg_stage) = paste0("10070618",names(psg_stage))

## Take ID# from Quanticate Provided Dataset, n left = 32, n right = 31 ##
IDleft = sas7bdat::read.sas7bdat("~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Output Datasets and Documentation/f_15_2_7_2_3_1.sas7bdat")$subject
IDright = sas7bdat::read.sas7bdat("~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Output Datasets and Documentation/f_15_2_7_2_3_2.sas7bdat")$subject
idlist = union(IDleft, IDright)
##########################################################################
psg_stage = psg_stage[which(names(psg_stage) %in% idlist)]

# 1. Read in epoch level Prediction for Date of PSG Visits ---------------------------------------
setwd("~/Pfizer/Christakis, Yiorgos - sleep_preds/") ## YC provided epoch level prediction

left_files = list.files(pattern = "leftwrist")
left = tibble()
for(i in 1:length(left_files)){
  file.i = left_files[i]
  id.i = strsplit(file.i,"_")[[1]][1]
  if(id.i %in% idlist){
    psg.i = psg_stage[id.i][[1]]
    date.i = as.Date(psg.i$Time[1])
    x = read_csv(file.i) %>% rename(Time = X1, Stage = `0`) %>% mutate(ID = id.i, Date = as.Date(Time[1]))
    if(x$Date == date.i){ ## Keep only the psg night
      left = rbind(left,x)
    }
  }
}

right_files = list.files(pattern = "rightwrist")
right = tibble()
for(i in 1:length(right_files)){
  file.i = right_files[i]
  id.i = strsplit(file.i,"_")[[1]][1]
  if(id.i %in% idlist){
    psg.i = psg_stage[id.i][[1]]
    date.i = as.Date(psg.i$Time[1])
    x = read_csv(file.i) %>% rename(Time = X1, Stage = `0`) %>% mutate(ID = id.i, Date = as.Date(Time[1]))
    if(x$Date == date.i){
      right = rbind(right,x)
    }
  }
}


# 2. Extract Performance Metrics------------------------------------------------
rm(list = setdiff(ls(),c("psg_stage","left","right","IDleft","IDright")))

## Left
ids = as.character(IDleft)
metrics_left = tibble()
for(i in 1:length(ids)){
  id.i = ids[i]
  psg.i = psg_stage[id.i][[1]] %>% mutate(Time = floor_date(Time, unit = "seconds")) %>% 
    select(Time, Stage2) %>% rename(S.psg = Stage2)
  
  dat.i = left %>% filter(ID == id.i) %>% mutate(Time = floor_date(Time, unit = "seconds"))
  tseq = seq(min(dat.i$Time),max(dat.i$Time),by = "sec")
  stages = rep(dat.i$Stage, each = 60)
  pred.i = tibble(Time = tseq, S.pred = stages[1:length(tseq)])
  compare.i = merge(x = psg.i, y = pred.i, all.x = T) 
  cm = confusionMatrix(data = factor(na.omit(compare.i$S.pred),levels = c("0","1")), reference = factor(na.omit(compare.i$S.psg),levels = c("0","1")), positive = "0")
  met = as.data.frame(t(c(cm$overall, cm$byClass))) %>% mutate(ID = id.i)
  metrics_left = rbind(metrics_left,met)
}


## right
ids = as.character(IDright)
metrics_right = tibble()
for(i in 1:length(ids)){
  id.i = ids[i]
  psg.i = psg_stage[id.i][[1]] %>% mutate(Time = floor_date(Time, unit = "seconds")) %>% 
    select(Time, Stage2) %>% rename(S.psg = Stage2)
  
  dat.i = right %>% filter(ID == id.i) %>% mutate(Time = floor_date(Time, unit = "seconds"))
  tseq = seq(min(dat.i$Time),max(dat.i$Time),by = "sec")
  stages = rep(dat.i$Stage, each = 60)
  pred.i = tibble(Time = tseq, S.pred = stages[1:length(tseq)])
  compare.i = merge(x = psg.i, y = pred.i, all.x = T)
  cm = confusionMatrix(data = factor(na.omit(compare.i$S.pred),levels = c("0","1")), reference = factor(na.omit(compare.i$S.psg),levels = c("0","1")), positive = "0")
  met = as.data.frame(t(c(cm$overall, cm$byClass))) %>% mutate(ID = id.i)
  metrics_right = rbind(metrics_right,met)
}



# 3. Evaluate the performance ---------------------------------------------
metrics_left = metrics_left %>% select(ID, Accuracy, Sensitivity,Specificity,`Pos Pred Value`,
                                       `Neg Pred Value`,Precision, Recall, F1) %>% mutate(Wrist = "Left")

metrics_right = metrics_right %>% select(ID, Accuracy, Sensitivity,Specificity,`Pos Pred Value`,
                                       `Neg Pred Value`,Precision, Recall, F1) %>% mutate(Wrist = "Right")

metrics_both = rbind(metrics_left,metrics_right)
library(tableone)
vars = names(metrics_both)[2:9]

tab1 = CreateTableOne(vars = vars, strata = "Wrist", data = metrics_both)
tab1.1 = print(tab1)
write.csv(tab1.1, file = "~/Pfizer/DMTI CT-44 Scratch Sleep Team - Scratch and Sleep Methods Paper/QCed Plots/results/sleepepochlevel/Subject_Summary.csv")


