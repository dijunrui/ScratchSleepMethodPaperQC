#######################################################
##     Amendment to the analyses for understanding   ##
##     correlation between prediction and video      ##
##     for both V01 and V02                          ##
##     Di, Junrui 10/23/2020                         ##
#######################################################

## Originally, when comparing scratchpy prediction v.s. videography ground truth
## for both V01 V02, observations were treated independently. This needs to be 
## fixed to appropriately address the within-subject correlation. 
## 1. For correlation, a MMRM was fitted and r.squaredGLMM() were used
## 2. For SD of difference (limits for BA plot), a MMRM was used


rm(list = ls())
library(nlme)
library(MuMIn)
setwd("~/Pfizer/DMTI CT-44 Scratch Sleep Team - Scratch and Sleep Methods Paper/QCed Plots/") ## Location where created plots are saved
source("scripts/Utils Function.R") 
files.dir = "~/OneDrive - Pfizer/SQUAD Study Programming Analysis Tables Quanticate/Quanticate Derived Datasets and Documentation/Training Set Output Datasets and Documentation/"

## We create a dictionary here to loop over file names with corresponding variable names, 
## wrists, and figure main titles, log transformation indicator, and where to round. 
data_dictionary = tibble(
  scatterfiles = paste0("f_15_2_7_2_8_",3:4),
  bafiles = paste0("f_15_2_7_2_7_",3:4),
  titles = c("Scratch Events (Log Transformed)","Scratch Duration (Log Transformed)")
)


FigNames = NULL
for(i in 1:2){
  
  dat1 = read.sas7bdat(paste0(files.dir, data_dictionary$scatterfiles[i],".sas7bdat")) %>% 
    na.omit() %>% select(subject, avisitn, acc, vid)
  dat2 = read.sas7bdat(paste0(files.dir, data_dictionary$bafiles[i],".sas7bdat")) %>% 
    na.omit() %>% select(subject, avisitn, diff,mean)
  dat = merge(x = dat1, y = dat2) %>% na.omit() %>% mutate(avisitn = as.factor(avisitn))
  
  
  
  lo = round_any(min(c(dat$acc,dat$vid), na.rm = T),1,f = floor)
  up = round_any(max(c(dat$acc,dat$vid), na.rm = T),1, f = ceiling)
  cl = c(alphablend("black",0.4), alphablend("brown",0.4))[as.factor(dat$avisitn)]
  pc = c(20,18)[as.factor(dat$avisitn)]
  
  ## Codes to generate limits for difference while taking into account 
  ## repeated measures
  mixd_model1 = lme(diff ~ 1, random = ~1|subject, data = dat)
  
  var_intercept = as.numeric(getVarCov(mixd_model1))
  var_residual = (mixd_model1$sigma)^2
  sd_adjust = sqrt(var_intercept + var_residual)
  mean_adjust = mixd_model1$coefficients$fixed[1]
  
  
  
  print(data_dictionary$titles[i])
  print(paste0("Variance of Intercept: ", var_intercept, ", Variance of Residual: ", var_residual, ", Adjusted SD: ", sd_adjust))
  
  
  
  ## Codes to generated r while taking into account repeated measures ## unstructured, 
  mixed_model2 = lme(acc ~ vid + avisitn, random = ~avisitn|subject, data = dat)
  r = sqrt(r.squaredGLMM(mixed_model2)[1])  ## marginal r^2 from mixed effects model
  pvalue = anova(mixed_model2)[2,4]         ##
  sig_inf = symnum(pvalue, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
                                                                            "**", "*", ".", " "))
  r_sig = paste0("r = ", round(r,2), " ", sig_inf)
  n_size = paste(paste0(c("V01: n = ", "V02: n = "),table(dat$avisitn)), collapse = ",")
  print(paste0("Model Adjusted GLMM R: ", r, ", P values = ", pvalue))
  print("____________________________________________________")
  
  png(file = paste0("results/revision/ScratchValidation_", data_dictionary$titles[i],".png"),
      width = 18, height = 7,units = "in", res = 300)
  par(mfrow = c(1,2))
  par(mar = c(4,6,4,4))
  plot(dat$vid, dat$acc, main = data_dictionary$titles[i], xlab = "Video Annotation",ylab = "ScratchPy Prediction",
       xlim = c(lo, up), ylim = c(lo, up), cex = 4, col = cl,
       pch = pc, cex.lab = 2, cex.axis = 1.8, cex.main = 2)
  
  legend("topleft", legend = paste0(r_sig, "\n", n_size), cex = 2, bty = "n")
  legend("bottomright",legend = c("Identity","Regression"),lty = c(1,1), bty = "n",
         col = c("black","red"), lwd = c(2,3),cex = 1.8)
  abline(lm(acc~vid, data = dat), col = "red", lty = 1, lwd = 3)
  abline(a = 0, b = 1, lty = 1, lwd = 2)
  legend("bottomleft",legend = c("V01","V02"),pch = c(18,20), bty = "n",
         col = cl,cex = 2.8)
  
  
  BAplot2(ave = dat$mean, dif = dat$diff, mean.diffs = mean_adjust, sd_diff = sd_adjust,var1 = "ScratchPy Prediction", 
          var2 = "Video Annotation",title = data_dictionary$titles[i],bar = 1,
          group = dat$avisitn)
  
  
  dev.off()
  FigNames = c(FigNames,paste0("results/ScratchValidation_", data_dictionary$titles[i],"_",data_dictionary$types[i],".png"))
}
