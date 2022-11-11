library(nlme) #install the package nlme and PowerTOST first
library(PowerTOST)

setwd("C:/Users/mzzj/Desktop/BE TRIAL  METHOD/BE Tradition") #set the path of the document

#SIMULATION
simfun<-function(seed,N,sample1,uT,uR,q,p,sij,eijk) {
  set.seed(seed)
  list1=list()
  for (i in 1:N){
    my.df1 <- data.frame(Subject=rep(1:sample1), 
                         seq=rep(1:2,each=sample1/2), 
                         period=rep(1:2,sample1/2), form=2)
    my.df1$distance <- uR + (-1)^my.df1$seq*q+(-1)^my.df1$period*p + rnorm(sample1,0,sij)+ rnorm(sample1,0,eijk)
    my.df2 <- data.frame(Subject=rep(1:sample1), 
                         seq=rep(1:2,each=sample1/2), 
                         period=rep(1:2,sample1/2), form=1)
    my.df2$distance <- uT + (-1)^my.df1$seq*q+(-1)^my.df2$period*p + rnorm(sample1,0,sij)+ rnorm(sample1,0,eijk)
    my.df<-rbind(my.df1,my.df2)
    list1[[i]]=my.df}
  list1
}
cmaxs<-simfun(120,1,12,8.349,8.187,0.016,0.025,0.088,0.237)
aucts<-simfun(120,1,12,9.231,9.163,0.048,0.026,0.101,0.122)
write.table (cmaxs, file ="cmaxs.csv",sep =",") 
write.table (aucts, file ="aucts.csv",sep =",") 
pk  <- list( )


pk$subject = as.factor(cmaxs[[1]]$Subject)
pk$seq = as.factor(cmaxs[[1]]$seq)
pk$period = as.factor(cmaxs[[1]]$period)
pk$form= as.factor(cmaxs[[1]]$form)

pk$LNCMAX <-cmaxs[[1]]$distance
pk$LNAUCall <-aucts[[1]]$distance


modeAUCLST1<-lme(LNAUCall~period+ form+seq,data=pk,random=~seq|subject)  #The main mixed linear model of BE
summary(modeAUCLST1)$coefficients

modAUCLST2<-lm(LNAUCall~subject+ period +form,data=pk)
mseAUCLST<-sum(residuals(modAUCLST2)^2)/modAUCLST2$df.residual
mseAUCLST
CVAUCLST<- mse2CV(mseAUCLST)
CVAUCLST
res.by <- by(pk$LNAUCall ,pk$form,  mean)
meanTAUCLST<-res.by[1]
meanRAUCLST<-res.by[2]
PEAUCLST<- exp(meanTAUCLST-meanRAUCLST)    #the mean of uT-UR
PEAUCLST  #lower limit of confidence interval
CIAUCLST<- CI.BE(alpha=0.05,pe=PEAUCLST,CV=CVAUCLST, n=12, design = "2x2", robust=FALSE)
CIAUCLST  #upper limit of confidence interval
anova(modeAUCLST1) #get p value of form



modeCMAX1<-lme(LNCMAX~period+ form+seq,data=pk,random=~seq|subject)  #The main mixed linear model of BE
summary(modeCMAX1)$coefficients

modCMAX2<-lm(LNCMAX~subject+ period +form,data=pk)
mseCMAX<-sum(residuals(modCMAX2)^2)/modCMAX2$df.residual
mseCMAX
CVCMAX<- mse2CV(mseCMAX)
CVCMAX
res.by <- by(pk$LNCMAX ,pk$form,  mean)
meanTCMAX<-res.by[1]
meanRCMAX<-res.by[2]
PECMAX<- exp(meanTCMAX-meanRCMAX)    #the mean of uT-UR
PECMAX  #lower limit of confidence interval
CICMAX<- CI.BE(alpha=0.05,pe=PECMAX,CV=CVCMAX, n=12, design = "2x2", robust=FALSE)
CICMAX  #upper limit of confidence interval
anova(modeCMAX1) #get p value of form

