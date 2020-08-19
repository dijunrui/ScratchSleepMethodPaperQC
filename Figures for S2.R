##############################################
##   Figures in Supplemantary Materials S2  ##
##   Di, Junrui 08/13/2020                  ##
##############################################


# 1. S2.1 Distribution of temperature -------------------------------------
## This is same as 
rm(list = ls())
library(dplyr)
library(lubridate)
setwd("/Volumes/PfIRe-SQUAD/geneactiv_data_dev/geneactiv_epoch_data/")
files.left = grep("_02_",list.files(pattern = "leftwrist.csv"), value = T)
files.right = grep("_02_", list.files(pattern = "rightwrist.csv"), value = T)
to_remove_left = "100706184007_02_leftwrist.csv"    ## nonwear, subjects removed 
to_remove_right = "100706184007_02_rightwrist.csv"  ## nonwear, subjects removed
files.left = setdiff(files.left, to_remove_left)
files.right = setdiff(files.right, to_remove_right)

load("~/OneDrive - Pfizer/JDi/SQUAD/SAP/sleep/data/Sleep_PSG.rda")  ## Epoch level PSG data
tso = PSG[,c(1,3,4,5)]

In_side_TSO_left = NULL
Out_side_TSO_left = NULL
All_left = NULL


for(i in 1:length(files.left)){
  file.i = files.left[i]
  id.i = as.numeric(substr(file.i,1,12))
  x = readr::read_csv(file.i, skip = 99)[,c(1,7)]  
  names(x) = c("Time","Temp")
  x$Time = ymd_hms(substr(x$Time,1,19))
  
  
  tso.start = tso$Lights_off[which(tso$ID == id.i)]
  tso.end = tso$Lights_on[which(tso$ID == id.i)]
  dt = tso$Date[which(tso$ID == id.i)]
  seg.start = ymd_hms(paste(dt,"12:00:00", sep = " "))
  seg.end = ymd_hms(paste(dt + 1,"11:59:00", sep = " "))
  
  x = x %>% filter(Time >= seg.start & Time <= seg.end)  ## Segment data from Visit 2 (12:00PM - 11:59AM next day)
  intso = x %>% filter(Time >= tso.start & Time <= tso.end) ## inside sleep window
  outtso = x %>% filter(Time < tso.start | Time > tso.end)  ## outside sleep window
  
  In_side_TSO_left = c(In_side_TSO_left, intso$Temp)
  Out_side_TSO_left = c(Out_side_TSO_left, outtso$Temp)
  All_left = c(All_left, x$Temp)
}

In_side_TSO_right = NULL
Out_side_TSO_right = NULL
All_right = NULL


for(i in 1:length(files.right)){
  file.i = files.right[i]
  id.i = as.numeric(substr(file.i,1,12))
  x = readr::read_csv(file.i, skip = 99)[,c(1,7)]
  names(x) = c("Time","Temp")
  x$Time = ymd_hms(substr(x$Time,1,19))
  
  
  tso.start = tso$Lights_off[which(tso$ID == id.i)]
  tso.end = tso$Lights_on[which(tso$ID == id.i)]
  dt = tso$Date[which(tso$ID == id.i)]
  seg.start = ymd_hms(paste(dt,"12:00:00", sep = " "))
  seg.end = ymd_hms(paste(dt + 1,"11:59:00", sep = " "))
  
  x = x %>% filter(Time >= seg.start & Time <= seg.end) ## Segment data from Visit 2 (12:00PM - 11:59AM next day)
  intso = x %>% filter(Time >= tso.start & Time <= tso.end) ## inside sleep window
  outtso = x %>% filter(Time < tso.start | Time > tso.end) ## outside sleep window
  
  In_side_TSO_right = c(In_side_TSO_right, intso$Temp)
  Out_side_TSO_right = c(Out_side_TSO_right, outtso$Temp)
  All_right = c(All_right, x$Temp)
}


In_side_TSO = c(In_side_TSO_left,In_side_TSO_right)
Out_side_TSO = c(Out_side_TSO_left, Out_side_TSO_right)
All = c(All_left, All_right)


png(file = "~/Pfizer/DMTI CT-44 Scratch Sleep Team - Scratch and Sleep Methods Paper/QCed Plots/results/tem_distribution.png",
    width = 14,height = 7,units = "in",res = 300)
par(mar = c(5,5,4,3))
plot(density(In_side_TSO), xlim = c(10,45), ylim = c(0,0.2), main = "Distributions of Temperature",
     xlab = "Temperature (C)",cex.lab = 1.8, cex.main = 1.8, lwd = 2, col = "red", cex.axis = 1.8)
abline(v = seq(10,45,5), h = seq(0,0.2,0.05), lty = 3, lwd = 0.5)
lines(density(Out_side_TSO), lwd = 2, col = "blue")
lines(density(All), lwd = 2, col = "green")
legend("topleft",c("Full Recording","Outside Sleep Window","Inside Sleep Window"),
       lty = 1, lwd = 2, col = c("green","blue","red"),bty = "n", cex = 1.8)
dev.off()



# 2. S2, movement threshold -----------------------------------------------
rm(list = ls())
setwd("~/Pfizer/DMTI CT-44 Scratch Sleep Team - Scratch and Sleep Methods Paper/QCed Plots/")
x = readr::read_csv("data/thresholds_remove_bad_subs.csv")
library(ggplot2)

png(file = "~/Pfizer/DMTI CT-44 Scratch Sleep Team - Scratch and Sleep Methods Paper/QCed Plots/results/motion_distribution.png",
    width = 14,height = 7,units = "in",res = 300)
ggplot(x, aes(x = threshold)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "steelblue", binwidth = 0.05, alpha = 0.2) +
  geom_density(aes(color = "Density"), size = 1.2) + theme(legend.position = "none",
                                                           axis.title.x = element_text(size = 18),
                                                           axis.text.x = element_text(size = 17),
                                                           axis.title.y = element_text(size = 18),
                                                           axis.text.y = element_text(size = 17),
                                                           plot.title = element_text(size = 20, face = "bold",hjust = 0.5)) + 
  labs(title = "Distribution of Movement Threshold", x= "Threshold", y = "Density")
dev.off()



