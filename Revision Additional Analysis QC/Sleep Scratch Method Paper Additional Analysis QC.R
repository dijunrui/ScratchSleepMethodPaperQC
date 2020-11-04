######## Additional Analysis a #####;

wkdir.lc="C:\\Users\\zhangy6\\Documents\\yaozhang\\Preclinical\\DigitalMedcine\\AtopicDerm\\SQUAD\\Publication\\Methodology July2020\\Revision\\"

# dat1_scatt from SAS;
dat1.scatt=read.csv(paste(wkdir.lc,"dat1_scatt.csv",sep=""), header=T, as.is=T)

dat1.scatt$avisitn=as.factor(dat1.scatt$avisitn)

library(nlme)
library(MuMIn)
myendpt=unique(dat1.scatt$endpt)
cor.mmrm=NULL
for(i in 1:length(myendpt)) {
    tmpdat0=dat1.scatt[dat1.scatt$endpt==myendpt[i],]
    tmpdat0=tmpdat0[tmpdat0$ptflag==1,]
    if(nrow(tmpdat0)>0) {
      tmpmdl=lme(acc~vid+avisitn, random=~avisitn|subject,data=tmpdat0)
      tmp=data.frame(endpt=myendpt[i],
                     fixed.int=fixed.effects(tmpmdl)[1],fixed.vid=fixed.effects(tmpmdl)[2],fixed.visit2=fixed.effects(tmpmdl)[3],
                     sigma2=tmpmdl$sigma^2,
                     vcov11=getVarCov(tmpmdl)[1,1],vcov21=getVarCov(tmpmdl)[2,1],vcov22=getVarCov(tmpmdl)[2,2],
                     r=sqrt(r.squaredGLMM(tmpmdl)[1]),
                     pvalue0=anova(tmpmdl)[2,4])
      cor.mmrm=rbind(cor.mmrm,tmp)
    }
}

cor.mmrm$SAS.Fixed.int=cor.mmrm$fixed.int+cor.mmrm$fixed.visit2
cor.mmrm$SAS.Fixed.vid=cor.mmrm$fixed.vid
cor.mmrm$SAS.Fixed.visit1=-cor.mmrm$fixed.visit2
# V1=p1+e1, V2=p1+p2+e2;
cor.mmrm$SAS.UN11=cor.mmrm$sigma2+cor.mmrm$vcov11
cor.mmrm$SAS.UN21=cor.mmrm$vcov11+cor.mmrm$vcov21
cor.mmrm$SAS.UN22=cor.mmrm$sigma2+cor.mmrm$vcov11+cor.mmrm$vcov22+2*cor.mmrm$vcov21
cor.mmrm

######## Additional Analysis b #####;

wkdir.lc="C:\\Users\\zhangy6\\Documents\\yaozhang\\Preclinical\\DigitalMedcine\\AtopicDerm\\SQUAD\\Publication\\Methodology July2020\\Revision\\"
Qdir1="C:\\Users\\zhangy6\\Pfizer\\Di, Junrui - SQUAD Study Programming Analysis Tables Quanticate\\Data Sharing\\ScratchData\\"

# remove R NA characters in dataset for SAS import;
dat2.scratch0=read.csv(paste(Qdir1,"ScratchPyScratch_forLongitudinal_indyTSO.csv",sep=""), header=T, as.is=T)
write.table(dat2.scratch0,file=paste(wkdir.lc,"dat2_scratch0.csv",sep=""),sep=",",row.names=F,quote=T,na="")

######## Additional Analysis c #####;

wkdir.lc="C:\\Users\\zhangy6\\Documents\\yaozhang\\Preclinical\\DigitalMedcine\\AtopicDerm\\SQUAD\\Publication\\Methodology July2020\\Revision\\"

# re-compute quantile results in R default method; import same dataset from SAS;
dat3=read.csv(paste(wkdir.lc,"dat3.csv",sep=""), header=T, as.is=T)

dat3.tbl.g=NULL
for (i in names(dat3[,5:14])) {
  tmprow=data.frame(var=i)
  for (j in c("FEMALE","MALE")) {
    tmpdat=dat3[dat3$GENDER==j,i]
    tmpframe=data.frame(med=median(tmpdat),q1=quantile(tmpdat,0.25),q3=quantile(tmpdat,0.75))
    names(tmpframe)=paste(names(tmpframe),j,sep=".")
    tmprow=cbind(tmprow,round(tmpframe,2))
  }
  dat3.tbl.g=rbind(dat3.tbl.g,tmprow)
}
dat3.tbl.g

isga.level=names(table(dat3$ISGA_V0))
dat3.tbl.isga=NULL
for (i in names(dat3[,5:14])) {
  tmprow=data.frame(var=i)
  for (j in isga.level) {
    tmpdat=dat3[dat3$ISGA_V0==j,i]
    tmpframe=data.frame(med=median(tmpdat),q1=quantile(tmpdat,0.25),q3=quantile(tmpdat,0.75))
    names(tmpframe)=paste(names(tmpframe),j,sep=".")
    tmprow=cbind(tmprow,round(tmpframe,2))
  }
  dat3.tbl.isga=rbind(dat3.tbl.isga,tmprow)
}
dat3.tbl.isga

###################### end of code ################;

