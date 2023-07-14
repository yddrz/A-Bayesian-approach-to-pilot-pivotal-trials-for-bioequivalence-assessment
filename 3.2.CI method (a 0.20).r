library(nlme) #install the package nlme and PowerTOST first
library(PowerTOST)#Power and Sample Size Based on Two One-Sided t-Tests (TOST) for (Bio)Equivalence Studies

cimethod2<-function(pkdata,N) {
  Y<-rep(99,10000)
  for (i in 1:N){
    
    pkdata[[i]]$Subject=as.factor(pkdata[[i]]$Subject)
    pkdata[[i]]$seq=as.factor(pkdata[[i]]$seq)
    pkdata[[i]]$period=as.factor(pkdata[[i]]$period)
    pkdata[[i]]$form=as.factor(pkdata[[i]]$form)
    
    modAUCLST2<-lm(distance~Subject+ period +form,data= pkdata[[i]])
    mseAUCLST<-sum(residuals(modAUCLST2)^2)/modAUCLST2$df.residual
    mseAUCLST
    CVAUCLST<- mse2CV(mseAUCLST)
    CVAUCLST
    res.by <- by(pkdata[[i]]$distance,pkdata[[i]]$form,  mean)
    meanTAUCLST<-res.by[1]
    meanRAUCLST<-res.by[2]
    PEAUCLST<- exp(meanTAUCLST-meanRAUCLST)    #the mean of uT-UR
    PEAUCLST  
    CIAUCLST<- CI.BE(alpha=0.20,pe=PEAUCLST,CV=CVAUCLST, n=nrow(pkdata[[i]])/2, design = "2x2", robust=FALSE)
    CIAUCLST  #upper limit of confidence interval  lower limit of confidence interval
    if (CIAUCLST[1]>=0.8 && CIAUCLST[2]<=1.25)
      Y[[i]]=1 else 
        Y[[i]]=0
  }
  Y
}

result1_1<-cimethod2(cmax1,10000)
result1_2<-cimethod2(auct1,10000)
sum(result1_1)
sum(result1_2)


result2_1<-cimethod2(cmax2,10000)
result2_2<-cimethod2(auct2,10000)
sum(result2_1)
sum(result2_2)


result3_1<-cimethod2(cmax3,10000)
result3_2<-cimethod2(auct3,10000)
sum(result3_1)
sum(result3_2)


result4_1<-cimethod2(cmax4,10000)
result4_2<-cimethod2(auct4,10000)
sum(result4_1)
sum(result4_2)


result5_1<-cimethod2(cmax5,10000)
result5_2<-cimethod2(auct5,10000)
sum(result5_1)
sum(result5_2)


result6_1<-cimethod2(cmax6,10000)
result6_2<-cimethod2(auct6,10000)
sum(result6_1)
sum(result6_2)


result7_1<-cimethod2(cmax7,10000)
result7_2<-cimethod2(auct7,10000)
sum(result7_1)
sum(result7_2)


result8_1<-cimethod2(cmax8,10000)
result8_2<-cimethod2(auct8,10000)
sum(result8_1)
sum(result8_2)


result9_1<-cimethod2(cmax9,10000)
result9_2<-cimethod2(auct9,10000)
sum(result9_1)
sum(result9_2)
