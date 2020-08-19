#####################################################
##  SleepPy Endpoints Left Right Wrist Aggreement  ##
##    Di, Junrui August 07 2020                    ##
#####################################################

## Description :
## This script aims to create scatter plot and bland altman plot
## to explore the agreement between left and right wrists 
## sleepPy predicted endpoints.

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
  filenams = paste0("f_15_2_7_2_1_",1:5),
  vars = c("TSO", "TST", "PST", "WASO", "SOL"),
  titles = c("Total Sleep Opportunity\n (Minutes)", "Total Sleep Time\n (Minutes)", "Percent Time Asleep\n (%)",
                 "Wake after Sleep Onset\n (Minutes) Log Transformed", "Sleep Onset Latency\n (Minutes) Log Transformed"),
  logT = c(FALSE,FALSE,FALSE,TRUE,TRUE),
  bars = c(10,10,10,1,1))  ## 10 --> round (floor or ceiling) to the nearest tenth, 1 --> round to the nearest 1
##########################################
##########################################

BA_out = tibble()
FigNames = NULL
for(i in 1:nrow(data_dictionary)){
  file.i = paste0(files.dir, data_dictionary$filenams[i],".sas7bdat") 
  dat = read.sas7bdat(file.i) %>% filter(avisitn == 2)
  
  if(data_dictionary$logT[i]){
    lo = round_any(min(c(dat$Left,dat$Right), na.rm = T),data_dictionary$bars[i],f = floor)
    up = round_any(max(c(dat$Left,dat$Right), na.rm = T),data_dictionary$bars[i], f = ceiling)
  }
  if(!data_dictionary$logT[i]){
    lo = round_any(min(c(dat$Left,dat$Right), na.rm = T),data_dictionary$bars[i],f = floor)
    up = round_any(max(c(dat$Left,dat$Right), na.rm = T), data_dictionary$bars[i], f = ceiling)
  }
  
  
  BA_out = rbind(BA_out,BA_calc(ave = dat$mean, dif = dat$diff))
  
  
  png(file = paste0("results/SleepLeftRightAgreement_", data_dictionary$vars[i],".png"),
      width = 18, height = 7,units = "in", res = 300)
  par(mfrow = c(1,2))
  par(mar = c(4,6,4,4))
  
  ## Scatter Plot
  plot(dat$Left, dat$Right, main = data_dictionary$titles[i], xlab = "Left - SleepPy",ylab = "Right - SleepPy",
       xlim = c(lo, up), ylim = c(lo, up), cex = 4, col = alphablend("black",0.4),
       pch = 20, cex.lab = 2, cex.axis = 1.8, cex.main = 2)
  legend("topleft",legend = bquote(r == .(cor_sig(dat$Left,dat$Right))),bty = "n", cex = 2)
  legend("bottomright",legend = c("Identity","Regression"),lty = c(1,1), bty = "n",
         col = c("black","red"), lwd = c(2,3),cex = 1.8)
  abline(lm(Right~Left, data = dat), col = "red", lty = 1, lwd = 3)
  abline(a = 0, b = 1, lty = 1, lwd = 2)
  

  BAplot(ave = dat$mean,dif = dat$diff,var1 = "Left", var2 = "Right",title = data_dictionary$titles[i], bar = data_dictionary$bars[i])
  dev.off()
  FigNames = c(FigNames,paste0("results/SleepLeftRightAgreement_", data_dictionary$vars[i],".png"))
}

BA_out = BA_out %>% mutate(Variable = data_dictionary$vars) %>% mutate(FigNames = gsub("results/","",FigNames))
names(BA_out)[1:3] = c("LowerLimit","MeanDiff","UpperLimit")

write.csv(BA_out, file = paste0("results/SleepLeftRightAgreement_","Bias.csv"))

#######################################################
## Plots for NWB, which is not created by Quanticate ##
#######################################################

dat = read.sas7bdat("~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Output Datasets and Documentation/f_nwb_sleeppy_wrist.sas7bdat") %>%
  filter(avisitn == 2)
titles = "Number of Wake Bouts (Log Transformed)"

lo = round_any(min(c(dat$Left,dat$Right), na.rm = T),1,f = floor)
up = round_any(max(c(dat$Left,dat$Right), na.rm = T), 1, f = ceiling)
png(file = "results/SleepLeftRightAgreement_NWB.png",
    width = 18, height = 7,units = "in", res = 300)
par(mfrow = c(1,2))
par(mar = c(4,6,4,4))

## Scatter Plot
plot(dat$Left, dat$Right, main = titles, xlab = "Left - SleepPy",ylab = "Right - SleepPy",
     xlim = c(lo, up), ylim = c(lo, up), cex = 4, col = alphablend("black",0.4),
     pch = 20, cex.lab = 2, cex.axis = 1.8, cex.main = 2)
legend("topleft",legend = bquote(r == .(cor_sig(dat$Left,dat$Right))),bty = "n", cex = 2)
legend("bottomright",legend = c("Identity","Regression"),lty = c(1,1), bty = "n",
       col = c("black","red"), lwd = c(2,3),cex = 1.8)
abline(lm(Right~Left, data = dat), col = "red", lty = 1, lwd = 3)
abline(a = 0, b = 1, lty = 1, lwd = 2)

## BA Plot
BAplot(ave = dat$mean,dif = dat$diff, var1 = "Left", var2 = "Right",title = titles, bar = 1)
dev.off()

