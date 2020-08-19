##################################################
##  ScratchPy Endpoints Validation against PSG  ##
##  Di, Junrui August 07 2020                   ##
##################################################

## Description 
## This script aims to create scatter plot and bland altman plot
## to compare scratchPy predicted endpoints v.s. video annotation 

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
  scatterfiles = paste0("f_15_2_7_2_8_",1:4),
  bafiles = paste0("f_15_2_7_2_7_",1:4),
  titles = rep(c("Scratch Events (Log Transformed)","Scratch Duration (Log Transformed)"),2),
  types = rep(c("V02","V01V02"), each = 2)
)
##########################################
##########################################

BA_out = tibble()
FigNames = NULL
for(i in 1:4){
  
  dat1 = read.sas7bdat(paste0(files.dir, data_dictionary$scatterfiles[i],".sas7bdat")) %>% 
    na.omit() %>% select(subject, avisitn, acc, vid)
  dat2 = read.sas7bdat(paste0(files.dir, data_dictionary$bafiles[i],".sas7bdat")) %>% 
    na.omit() %>% select(subject, avisitn, diff,mean)
  dat = merge(x = dat1, y = dat2) %>% na.omit()


  
  lo = round_any(min(c(dat$acc,dat$vid), na.rm = T),1,f = floor)
  up = round_any(max(c(dat$acc,dat$vid), na.rm = T),1, f = ceiling)
  
  if(data_dictionary$types[i] == "V02"){
    cl = alphablend("black",0.4)
    pc = 20
  }

  if(data_dictionary$types[i] == "V01V02"){
    cl = c(alphablend("black",0.4), alphablend("brown",0.4))[as.factor(dat$avisitn)]
    pc = c(20,18)[as.factor(dat$avisitn)]
  }
 
  BA_out = rbind(BA_out,BA_calc(ave = dat$mean, dif = dat$diff))
  png(file = paste0("results/ScratchValidation_", data_dictionary$titles[i],"_",data_dictionary$types[i],".png"),
      width = 18, height = 7,units = "in", res = 300)
  par(mfrow = c(1,2))
  par(mar = c(4,6,4,4))
  plot(dat$vid, dat$acc, main = data_dictionary$titles[i], xlab = "Video Annotation",ylab = "ScratchPy Prediction",
       xlim = c(lo, up), ylim = c(lo, up), cex = 4, col = cl,
       pch = pc, cex.lab = 2, cex.axis = 1.8, cex.main = 2)
  legend("topleft",legend = bquote(r == .(cor_sig(dat$vid, dat$acc))),bty = "n", cex = 2)
  legend("bottomright",legend = c("Identity","Regression"),lty = c(1,1), bty = "n",
         col = c("black","red"), lwd = c(2,3),cex = 1.8)
  abline(lm(acc~vid, data = dat), col = "red", lty = 1, lwd = 3)
  abline(a = 0, b = 1, lty = 1, lwd = 2)
  if(data_dictionary$types[i] == "V01V02"){
    legend("bottomleft",legend = c("V01","V02"),pch = c(20,18), bty = "n",
           col = cl,cex = 2.8)
  }
  
  
  if(data_dictionary$types[i] == "V02"){
    BAplot(ave = dat$mean, dif = dat$diff, var1 = "ScratchPy Prediction", 
           var2 = "Video Annotation",title = data_dictionary$titles[i],bar = 1)
  }
  
  if(data_dictionary$types[i] == "V01V02"){
    BAplot(ave = dat$mean, dif = dat$diff, var1 = "ScratchPy Prediction", 
           var2 = "Video Annotation",title = data_dictionary$titles[i],bar = 1,
           group = dat$avisitn)
  }
  
  dev.off()
  FigNames = c(FigNames,paste0("results/ScratchValidation_", data_dictionary$titles[i],"_",data_dictionary$types[i],".png"))
}

BA_out = BA_out  %>% mutate(FigNames = gsub("results/","",FigNames))
names(BA_out)[1:3] = c("LowerLimit","MeanDiff","UpperLimit")
write.csv(BA_out, file = paste0("results/ScratchValidation_","Bias.csv"))