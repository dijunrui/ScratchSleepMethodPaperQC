##################################################
##    SleepPy Endpoints Validation against PSG  ##
##    Di, Junrui August 07 2020                 ##
##################################################

## Description 
## This script aims to create scatter plot and bland altman plot
## to compare sleepPy predicted endpoints v.s. PSG. 

rm(list = ls())
library(sas7bdat) ## r package to read in SAS data
setwd("~/Pfizer/DMTI CT-44 Scratch Sleep Team - Scratch and Sleep Methods Paper/QCed Plots/") ## Location where created plots are saved
source("scripts/Utils Function.R") ## utility function to create BA plot and calculate correlation with significance level 

##########################################
## QC-ed Datasets created by Quanticate ##
##########################################


files.dir = "~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Output Datasets and Documentation/"

## We create a dictionary here to loop over file names with corresponding variable names, 
## wrists, and figure main titles, log transformation indicator, and where to round. 
data_dictionary = tibble(
  filenams = paste0("f_15_2_7_2_3_",1:15),
  vars = rep(c("TSO", "TST", "PST", "WASO", "SOL"), each = 3),
  wrist = rep(c("SleepPy (Left)","SleepPy (Right)", "SleepPy (Average of Two Wrists)"), 5),
  titles = rep(c("Total Sleep Opportunity\n (Minutes)", "Total Sleep Time\n (Minutes)", "Percent Time Asleep\n (%)",
                 "Wake after Sleep Onset\n (Minutes) Log Transformed", "Sleep Onset Latency\n (Minutes) Log Transformed"),each = 3),
  logT = rep(c(FALSE,FALSE,FALSE,TRUE,TRUE),each = 3),
  bars = rep(c(10,10,10,1,1), each = 3))  ## 10 --> round (floor or ceiling) to the nearest tenth, 
##########################################
##########################################

BA_out = tibble()
FigNames = NULL
for(i in 1:nrow(data_dictionary)){
  file.i = paste0(files.dir, data_dictionary$filenams[i],".sas7bdat")
  dat = read.sas7bdat(file.i)
  
  if(data_dictionary$logT[i]){
    lo = round_any(min(c(dat$psg_var,dat$sleeppy_var), na.rm = T),data_dictionary$bars[i],f = floor)
    up = round_any(max(c(dat$psg_var,dat$sleeppy_var), na.rm = T),data_dictionary$bars[i], f = ceiling)
  }
  if(!data_dictionary$logT[i]){
    lo = round_any(min(c(dat$psg_var,dat$sleeppy_var), na.rm = T),data_dictionary$bars[i],f = floor)
    up = round_any(max(c(dat$psg_var,dat$sleeppy_var), na.rm = T), data_dictionary$bars[i], f = ceiling)
  }
  
  
  BA_out = rbind(BA_out,BA_calc(ave = dat$mean, dif = dat$diff))

  
  png(file = paste0("results/SleepValidation_", data_dictionary$vars[i],"_",data_dictionary$wrist[i],".png"),
      width = 18, height = 7,units = "in", res = 300)
  par(mfrow = c(1,2))
  par(mar = c(4,6,4,4))
  
  ## Scatter Plot
  plot(dat$psg_var, dat$sleeppy_var, main = data_dictionary$titles[i], xlab = "PSG",ylab = data_dictionary$wrist[i],
       xlim = c(lo, up), ylim = c(lo, up), cex = 4, col = alphablend("black",0.4),    ## lo and up define the limits of the axis 
       pch = 20, cex.lab = 2, cex.axis = 1.8, cex.main = 2)
  legend("topleft",legend = bquote(r == .(cor_sig(dat$psg_var,dat$sleeppy_var))),bty = "n", cex = 2)
  legend("bottomright",legend = c("Identity","Regression"),lty = c(1,1), bty = "n",
         col = c("black","red"), lwd = c(2,3),cex = 1.8)
  abline(lm(sleeppy_var~psg_var, data = dat), col = "red", lty = 1, lwd = 3)
  abline(a = 0, b = 1, lty = 1, lwd = 2)
  
  BAplot(ave = dat$mean,dif = dat$diff,var1 = data_dictionary$wrist[i], var2 = "PSG",title = data_dictionary$titles[i], bar = data_dictionary$bars[i])
  dev.off()
  FigNames = c(FigNames,paste0("results/SleepValidation_", data_dictionary$vars[i],"_",data_dictionary$wrist[i],".png"))
}

BA_out = BA_out %>% mutate(Variable = data_dictionary$vars, Wrists = data_dictionary$wrist) %>% mutate(FigNames = gsub("results/","",FigNames))
names(BA_out)[1:3] = c("LowerLimit","MeanDiff","UpperLimit")

write.csv(BA_out, file = paste0("results/SleepValidation_","Bias.csv"))

#######################################################
## Plots for NWB, which is not created by Quanticate ##
#######################################################

NWB = read.sas7bdat("~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Output Datasets and Documentation/f_nwb_sleeppy_psg.sas7bdat")
v = unique(NWB$wrist)
wrists = c("SleepPy (Left)","SleepPy (Right)", "SleepPy (Average of Two Wrists)")
titles = "Number of Wake Bouts (Log Transformed)"

for(i in 1:3){
  w = v[i]
  dat = NWB %>% filter(wrist == w)
  lo = round_any(min(c(dat$psg_var,dat$sleeppy_var), na.rm = T),1,f = floor)
  up = round_any(max(c(dat$psg_var,dat$sleeppy_var), na.rm = T),1, f = ceiling)
  
  png(file = paste0("results/SleepValidation_NWB_", wrists[i],".png"),
      width = 18, height = 7,units = "in", res = 300)
  par(mfrow = c(1,2))
  par(mar = c(4,6,4,4))
  
  ## Scatter Plot
  plot(dat$psg_var, dat$sleeppy_var, main = titles, xlab = "PSG",ylab = wrists[i],
       xlim = c(lo, up), ylim = c(lo, up), cex = 4, col = alphablend("black",0.4),    ## lo and up define the limits of the axis 
       pch = 20, cex.lab = 2, cex.axis = 1.8, cex.main = 2)
  legend("topleft",legend = bquote(r == .(cor_sig(dat$psg_var,dat$sleeppy_var))),bty = "n", cex = 2)
  legend("bottomright",legend = c("Identity","Regression"),lty = c(1,1), bty = "n",
         col = c("black","red"), lwd = c(2,3),cex = 1.8)
  abline(lm(sleeppy_var~psg_var, data = dat), col = "red", lty = 1, lwd = 3)
  abline(a = 0, b = 1, lty = 1, lwd = 2)
  
  BAplot(ave = dat$mean,dif = dat$diff,var1 = wrists[i], var2 = "PSG",title = titles, bar = 1)
  dev.off()
}
