####################################################
##  Additional Statistical Analyses for Revision  ##
##  Di, Junrui 10/16/2020                         ##
####################################################
library(sas7bdat)
library(VIM)


# 1. Sleep v.s. Scratch ---------------------------------------------------
rm(list = ls())
setwd("~/Pfizer/DMTI CT-44 Scratch Sleep Team - Scratch and Sleep Methods Paper/QCed Plots/") ## Location where created plots are saved
source("scripts/Utils Function.R")

## files directory for Quanticate provided data
files.dir = "~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Output Datasets and Documentation/"

## list of IDs for the 33 subjects from Quanticate provided data
IDs = readr::read_csv("~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Demography Dataset/demo_pro_train.csv")$SUBJECT

## ScratchPy Prediction Data for V02 for the 33 subjects (from data sent to Quanticate, prediction without excluding gapping period
## because it is not for comparing against videography)
scratch = readr::read_csv("~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Data Sharing/ScratchData/ScratchPyScratch_forLongitudinal_indyTSO.csv")


prediction = scratch %>% select(ID, Visit, Number_of_Scratch, Duration_of_Scratch) %>%
  rename(n = Number_of_Scratch, t = Duration_of_Scratch) %>% as_tibble() %>%  group_by(ID, Visit) %>%
  summarise(n_pred = sum(n), log_n_pred = log(n_pred + 1), t_pred = sum(t)/60,log_t_pred = log(t_pred + 1)) %>% ungroup() %>% 
  mutate(ID = as.numeric(ID)) %>%  select(ID, Visit, log_n_pred, log_t_pred) %>%
  rename(Counts = log_n_pred, Duration = log_t_pred) %>% filter(Visit == "V02") %>% filter(ID %in% IDs)

## SleePy data processed by Quanticates
tst = read.sas7bdat(paste0(files.dir, "f_15_2_7_2_3_6",".sas7bdat")) %>% select(subject, sleeppy_var) %>% rename(TST = sleeppy_var, ID = subject) %>% mutate(ID = as.numeric(ID))
waso = read.sas7bdat(paste0(files.dir, "f_15_2_7_2_3_12",".sas7bdat")) %>% select(subject, sleeppy_var) %>% rename(WASO = sleeppy_var, ID = subject) %>% mutate(ID = as.numeric(ID))

data = tst %>% left_join(waso) %>% left_join(prediction)  %>% na.omit()
xvars = c("Total Sleep Time", "WASO (Log Transformed)")
vars = c("TST","WASO")

## The plotting codes has been QCed for the main analyses
for(i in 1:2){
  v = vars[i]
  dat = data %>% select(Counts, Duration, all_of(v))
  names(dat)[3] = "sleep"
  
  png(file = paste0("results/revision/scratch_vs_", v,".png"),
      width = 18, height = 7,units = "in", res = 300)
  par(mfrow = c(1,2))
  par(mar = c(4,6,4,4))
  plot(dat$sleep, dat$Counts, main = "Sleep v.s. Scratch", xlab = xvars[i],ylab = "Scratch Counts (Log Transformed)",
       cex = 4,cex.lab = 2, cex.axis = 1.8, cex.main = 2, col =  alphablend("black",0.4), pch = 20)
  legend("topleft",legend = bquote(r == .(cor_sig(dat$sleep, dat$Counts))),bty = "n", cex = 2)
  abline(lm(Counts~sleep, data = dat), col = "red", lty = 1, lwd = 3)
  legend("bottomright",legend = c("Regression"),lty = c(1), bty = "n",
         col = c("red"), lwd = c(2,3),cex = 1.8)
  
  plot(dat$sleep, dat$Duration, main = "Sleep v.s. Scratch", xlab = xvars[i],ylab = "Scratch Duration (Log Transformed)",
       cex = 4,cex.lab = 2, cex.axis = 1.8, cex.main = 2, col =  alphablend("black",0.4), pch = 20)
  legend("topleft",legend = bquote(r == .(cor_sig(dat$sleep, dat$Duration))),bty = "n", cex = 2)
  abline(lm(Duration~sleep, data = dat), col = "red", lty = 1, lwd = 3)
  legend("bottomright",legend = c("Regression"),lty = c(1), bty = "n",
         col = c("red"), lwd = c(2,3),cex = 1.8)
  dev.off()
}



# 2. Scratch Annotation Duration Distribution -----------------------------

## This is to understand the basic 
rm(list = ls())
data = readr::read_csv("/Volumes/PfIRe-SQUAD/scratch_dev/reference_annotation_management/subject_annotations_psg_sleep_window/scratch_annotations_psg_sleep_window_all_subjects_original.csv")
duration = as.numeric(data$annotation_stop_time - data$annotation_start_time)
cat_dur = cut(duration, breaks = c(0,1,2,3,Inf),right = T)
print(table(cat_dur))
print(table(cat_dur)/3629 * 100)


# 3. Performance by ISGA and Gender ---------------------------------------
rm(list = ls())
library(tableone)
dat = readr::read_csv("/Volumes/PfIRe-SQUAD/scratch_dev/model_development/model_training/trained_models/random_forest/20hz/FINAL_MODEL_RESULTS_2_7_20/per_subject_validation_performance.csv",)
cov_data = read.sas7bdat("~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Demography Dataset/demo_pro_train.sas7bdat") %>%
  select(SUBJECT, GENDER,ISGA_V0) %>%
  rename(subject = SUBJECT) 
dat = merge(x = dat, y = cov_data )

tab1 = CreateTableOne(vars = names(dat)[2:11], data = dat,strata = "GENDER")
tab1.1 = print(tab1,nonnormal = names(dat)[2:11])
tab2 = CreateTableOne(vars = names(dat)[2:11], data = dat,strata = "ISGA_V0")
tab2.1 = print(tab2,nonnormal = names(dat)[2:11])

write.csv(tab1.1, file = "results/revision/Performance_by_Gender.csv")
write.csv(tab2.1, file = "results/revision/Performance_by_ISGA.csv")


# 4. Summary Statistics ---------------------------------------------------
rm(list = ls())
library(tableone)
cov_data = read.sas7bdat("~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Demography Dataset/demo_pro_train.sas7bdat")
dat = cov_data %>% select(SUBJECT,ISGAN_V0, PGISN, PPNRSN,SPSN) %>% mutate(SPS = ifelse(SPSN == 3, "Severe", ifelse(SPSN == 2, "Moderate","Mild")))
tab1 = CreateTableOne(names(dat)[2:6],factorVars = "SPS", data = dat)

write.csv(print(tab1), file = "results/revision/Additional_Baseline_summary_statistics.csv")



# 5. PRO time and SleepPy TSO ---------------------------------------------
rm(list = ls())
library(lubridate)
library(sas7bdat)
setwd("~/Pfizer/DMTI CT-44 Scratch Sleep Team - Scratch and Sleep Methods Paper/QCed Plots/") ## Location where created plots are saved
source("scripts/Utils Function.R") ## 
pro = readr::read_csv("~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Demography Dataset/demo_pro_train.csv") %>%
   select(SUBJECT,sttm,entm,visitdt) %>% rename(ID = SUBJECT) %>% 
  mutate(visitdt = dmy(visitdt),
         Start = ymd_hms(paste(visitdt,sttm)),
         End = ymd_hms(paste(visitdt, entm))) %>%
  mutate(Start_fix = as.POSIXct(ifelse(!am(Start),Start - 3600 * 24, Start), origin = "1970-01-01",tz = "UTC")) %>%
  mutate(TSO_PRO = as.numeric(difftime(time1 = End, time2 = Start_fix, units = "mins"))) %>%
  select(ID, Start_fix,End, TSO_PRO)

tso = read.sas7bdat("~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Output Datasets and Documentation/f_15_2_7_2_3_3.sas7bdat") %>%
  select(subject, sleeppy_var) %>% rename(ID = subject, TSO = sleeppy_var)

dat = merge(pro,tso) %>% na.omit() %>% 
  mutate(diff = TSO_PRO - TSO, mean = (TSO_PRO+TSO)/2)




png(file = "results/revision/PROTSO_VS_SleepPy.png",
    width = 18, height = 7,units = "in", res = 300)
par(mfrow = c(1,2))
par(mar = c(4,6,4,4))

lo = round_any(min(c(dat$TSO_PRO,dat$TSO), na.rm = T),10,f = floor)
up = round_any(max(c(dat$TSO_PRO,dat$TSO), na.rm = T),10, f = ceiling)

plot(dat$TSO_PRO, dat$TSO, main = "TSO", xlab = "Patient Reported TSO",ylab = "SleepPy Prediction",
     xlim = c(lo, up), ylim = c(lo, up), cex = 4, col = alphablend("black",0.4),    ## lo and up define the limits of the axis 
     pch = 20, cex.lab = 2, cex.axis = 1.8, cex.main = 2)
legend("topleft",legend = bquote(r == .(cor_sig(dat$TSO_PRO,dat$TSO))),bty = "n", cex = 2)
legend("bottomright",legend = c("Identity","Regression"),lty = c(1,1), bty = "n",
       col = c("black","red"), lwd = c(2,3),cex = 1.8)
abline(lm(TSO~TSO_PRO, data = dat), col = "red", lty = 1, lwd = 3)
abline(a = 0, b = 1, lty = 1, lwd = 2)


BAplot(ave = dat$mean,dif = dat$diff,var1 = "PRO", var2 = "SleepPy",title = "TSO", bar = 10)
dev.off()


# 6. PRO time and PSG TSO ---------------------------------------------
rm(list = ls())
library(lubridate)
library(sas7bdat)
setwd("~/Pfizer/DMTI CT-44 Scratch Sleep Team - Scratch and Sleep Methods Paper/QCed Plots/") ## Location where created plots are saved
source("scripts/Utils Function.R") ## 
pro = readr::read_csv("~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Demography Dataset/demo_pro_train.csv") %>%
  select(SUBJECT,sttm,entm,visitdt) %>% rename(ID = SUBJECT) %>% 
  mutate(visitdt = dmy(visitdt),
         Start = ymd_hms(paste(visitdt,sttm)),
         End = ymd_hms(paste(visitdt, entm))) %>%
  mutate(Start_fix = as.POSIXct(ifelse(!am(Start),Start - 3600 * 24, Start), origin = "1970-01-01",tz = "UTC")) %>%
  mutate(TSO_PRO = as.numeric(difftime(time1 = End, time2 = Start_fix, units = "mins"))) %>%
  select(ID, Start_fix,End, TSO_PRO)

tso = read.sas7bdat("~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Output Datasets and Documentation/f_15_2_7_2_3_3.sas7bdat") %>%
  select(subject, psg_var) %>% rename(ID = subject, TSO = psg_var)

dat = merge(pro,tso) %>% na.omit() %>% 
  mutate(diff = TSO_PRO - TSO, mean = (TSO_PRO+TSO)/2)




png(file = "results/revision/PROTSO_VS_PSG.png",
    width = 18, height = 7,units = "in", res = 300)
par(mfrow = c(1,2))
par(mar = c(4,6,4,4))

lo = round_any(min(c(dat$TSO_PRO,dat$TSO), na.rm = T),10,f = floor)
up = round_any(max(c(dat$TSO_PRO,dat$TSO), na.rm = T),10, f = ceiling)

plot(dat$TSO_PRO, dat$TSO, main = "TSO", xlab = "Patient Reported TSO",ylab = "PSG",
     xlim = c(lo, up), ylim = c(lo, up), cex = 4, col = alphablend("black",0.4),    ## lo and up define the limits of the axis 
     pch = 20, cex.lab = 2, cex.axis = 1.8, cex.main = 2)
legend("topleft",legend = bquote(r == .(cor_sig(dat$TSO_PRO,dat$TSO))),bty = "n", cex = 2)
legend("bottomright",legend = c("Identity","Regression"),lty = c(1,1), bty = "n",
       col = c("black","red"), lwd = c(2,3),cex = 1.8)
abline(lm(TSO~TSO_PRO, data = dat), col = "red", lty = 1, lwd = 3)
abline(a = 0, b = 1, lty = 1, lwd = 2)


BAplot(ave = dat$mean,dif = dat$diff,var1 = "PRO", var2 = "PSG",title = "TSO", bar = 10)
dev.off()
