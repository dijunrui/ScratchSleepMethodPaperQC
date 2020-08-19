library(BlandAltmanLeh)
library(plyr)
library(dplyr)
library(VIM)

cor_sig = function(x, y, digits = 2){
  value = cor(x,y, use = "pairwise.complete.obs")
  test = cor.test(as.numeric(x), as.numeric(y), use = "pairwise.complete.obs")
  Signif = symnum(test$p.value, corr = FALSE, na = FALSE, 
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
                                                                           "**", "*", ".", " "))
  n = nrow(na.omit(cbind(x,y)))  
  paste0(round(value,digits = digits)," ", Signif, "  n = ", n)
}
BA_calc = function(ave, dif){
  mean.diffs = mean(dif)
  critical.diff = 1.96 * sd(dif)
  lower.limit = mean.diffs - critical.diff
  upper.limit = mean.diffs + critical.diff
  
  balines = c(lower.limit,mean.diffs,upper.limit)
  return(balines)
}

BAplot = function(ave, dif, var1, var2, title, bar, group = NULL){
  

  dat = data.frame(Ave = ave, Dif = dif)
  
  mean.diffs = mean(dif)
  critical.diff = 1.96 * sd(dif)
  lower.limit = mean.diffs - critical.diff
  upper.limit = mean.diffs + critical.diff
  balines = c(lower.limit,mean.diffs,upper.limit)
  
  lo = lower.limit - 1.96 * sqrt(sd(dif)^2 * 3/nrow(dat))
  up = upper.limit + 1.96 * sqrt(sd(dif)^2 * 3/nrow(dat))
  
  
  a = c(lo,dif,up)
  lo = round_any(min(a),bar,f = floor)
  up = round_any(max(a),bar, f= ceiling)
  
  if(is.null(group)){
    plot(x = dat$Ave, y = dat$Dif,col =  alphablend('black',0.4), pch = 20, cex = 4,cex.lab = 2, cex.axis = 1.8,ylim = c(lo,up),
         xlab = "Mean", ylab = paste0(var1, " - ", var2), main = title, cex.main = 2)
  }
  if(!is.null(group)){
    plot(x = dat$Ave, y = dat$Dif,col =  c(alphablend('black',0.4),alphablend('brown',0.4))[as.factor(group)], pch = c(20,18)[as.factor(group)], cex = 4,cex.lab = 2, cex.axis = 1.8,ylim = c(lo,up),
         xlab = "Mean", ylab = paste0(var1, " - ", var2), main = title, cex.main = 2)
  }
  
  abline(h = balines[2], lwd = 3, lty = 1, col = "blue")
  abline(h = c(balines[c(1,3)]), lwd = 3, lty = 3, col = "blue")
  legend("topright",legend = c("Mean Difference","Limits"), lty = c(1,3), col = c("blue","blue"), lwd = 3,
         cex = 2, bty = "n")
  
}

