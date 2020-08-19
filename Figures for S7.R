#####################################
##    Figure 8 Supp Materials      ##
##    Di, Junrui 08/13/2020        ##
#####################################

rm(list = ls())
library(lubridate)
setwd("~/OneDrive - Pfizer/JDi/SQUAD/SAP/")
load("sleep/data/Sleep_SleepPy_150temperature.rda")
load("sleep/data/Sleep_PSG.rda")
load("sleep/data/Sleep_PSGStage.rda")

tso.left = c(sleep_left_150temp$T.start[sleep_left_150temp$ID == "100706184008" & sleep_left_150temp$Night == "V02"],
             sleep_left_150temp$T.end[sleep_left_150temp$ID == "100706184008" & sleep_left_150temp$Night == "V02"])
tso.right = c(sleep_right_150temp$T.start[sleep_right_150temp$ID == "100706184008" & sleep_right_150temp$Night == "V02"],
              sleep_right_150temp$T.end[sleep_right_150temp$ID == "100706184008"  & sleep_right_150temp$Night == "V02"])

png(file = "~/Pfizer/DMTI CT-44 Scratch Sleep Team - Scratch and Sleep Methods Paper/Statistical Analyses/results/Pt8.png",
    width = 14,height = 7,units = "in",res = 300)
stages = psg_stage$`4008`
plot(stages$Time, stages$Stage2, type = "l", cex.axis = 1.8, xlim = c(min(stages$Time),max(stages$Time)),lwd = 3,
     main = "Epoch Level PSG (Sleep/Wake) Pt 8", cex.main = 1.8,cex.lab = 2, xlab = "Time", ylab = "", yaxt = "n", xaxt = "n")
axis(1, at = seq.POSIXt(min(stages$Time),max(stages$Time),by = 3600),cex.axis = 1.8,
     labels = as.character(strftime(seq.POSIXt(min(stages$Time),max(stages$Time),by = 3600),"%H:%M",tz = "UTC")))
abline(v = seq.POSIXt(min(stages$Time),max(stages$Time),by = 3600), lty = 2)
axis(2, at = c(0,1), labels = c("Sleep","Wake"), cex.axis = 1.8)
segments(x0 = tso.left[1],y0 = 0.75, x1 = tso.left[2],y1 = 0.75,lty = 3,lwd = 3, col = "blue")
points(x = c(tso.left[1], tso.left[2]), y = c(0.75,0.75), pch = "|", col = "blue", cex = 2)
legend(x = ymd_hms("2018-09-21 01:00:24"), y = 0.85, legend = "Left Wrist TSO", cex = 1.8,bty = "n")
segments(x0 = tso.right[1],y0 = 0.25, x1 = tso.right[2],y1 = 0.25,lty = 3,lwd = 3, col = "red")
points(x = c(tso.right[1], tso.right[2]), y = c(0.25,0.25), pch = "|", col = "red",cex = 2)
legend(x = ymd_hms("2018-09-21 01:00:24"), y = 0.35, legend = "Right Wrist TSO", cex = 1.8,bty = "n")
dev.off()
