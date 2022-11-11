library(nlme) #install the package nlme and PowerTOST first
library(PowerTOST)#Power and Sample Size Based on Two One-Sided t-Tests (TOST) for (Bio)Equivalence Studies

PAR<-function(pkdata,N,n1,n2,n1_star,n2_star){
  Y<-rep(99,200)
  
  for (i in 1:N){
    modAUCLST2<-lm(distance~Subject+ period +form,data= pkdata[[i]])
    mseAUCLST<-sum(residuals(modAUCLST2)^2)/modAUCLST2$df.residual
    
    sigma_star<-sqrt(mseAUCLST)
    t1<-sqrt(1/n1+1/n2)
    t2<-sqrt(1/n1_star+1/n2_star)
    lamda<-exp((-qt(0.05,n1+n2-2)*t1+qt(0.05,n1_star+n2_star-2)*t2) *sqrt(mseAUCLST))
    epil<-lamda*0.8
    epiu<-1/(lamda*0.8)
    pemean<-tapply(pkdata[[i]]$distance,pkdata[[i]]$form, mean)
    r_star<-exp(pemean[1]-pemean[2])
    gamma<-exp((qt(0.05,n1+n2-2)*sqrt(1/n1+1/n2)) *sqrt(mseAUCLST))
    a<-gamma*0.8
    b<-1/(gamma*0.8)
    
    CVAUCLST<- mse2CV(mseAUCLST)
    res.by <- by(pkdata[[i]]$distance,pkdata[[i]]$form,  mean)
    meanTAUCLST<-res.by[1]
    meanRAUCLST<-res.by[2]
    PEAUCLST<- exp(meanTAUCLST-meanRAUCLST)    #the mean of uT-UR
    PEAUCLST  
    CIAUCLST<- CI.BE(alpha=0.05,pe=PEAUCLST,CV=CVAUCLST, n=n1_star+n2_star, design = "2x2", robust=FALSE)
    CIAUCLST  #upper limit of confidence interval  lower limit of confidence interval
    
    if( (r_star>= a && r_star<= b) && (CIAUCLST[1]>= epil && CIAUCLST[2]<= epiu))
      Y[[i]]=1 else 
        Y[[i]]=0
  }
  Y
}


PAR1_1<-PAR(cmax1,200,12,12,6,6)
PAR1_2<-PAR(auct1,200,12,12,6,6)
PAR1_3<-PAR(aucall1,200,12,12,6,6)
sum(PAR1_1)
sum(PAR1_2)
sum(PAR1_3)

PAR2_1<-PAR(cmax2,200,12,12,6,6)
PAR2_2<-PAR(auct2,200,12,12,6,6)
PAR2_3<-PAR(aucall2,200,12,12,6,6)
sum(PAR2_1)
sum(PAR2_2)
sum(PAR2_3)

PAR3_1<-PAR(cmax3,200,12,12,6,6)
PAR3_2<-PAR(auct3,200,12,12,6,6)
PAR3_3<-PAR(aucall3,200,12,12,6,6)
sum(PAR3_1)
sum(PAR3_2)
sum(PAR3_3)

PAR4_1<-PAR(cmax4,200,12,12,6,6)
PAR4_2<-PAR(auct4,200,12,12,6,6)
PAR4_3<-PAR(aucall4,200,12,12,6,6)
sum(PAR4_1)
sum(PAR4_2)
sum(PAR4_3)

PAR5_1<-PAR(cmax5,200,12,12,6,6)
PAR5_2<-PAR(auct5,200,12,12,6,6)
PAR5_3<-PAR(aucall5,200,12,12,6,6)
sum(PAR5_1)
sum(PAR5_2)
sum(PAR5_3)

PAR6_1<-PAR(cmax6,200,12,12,6,6)
PAR6_2<-PAR(auct6,200,12,12,6,6)
PAR6_3<-PAR(aucall6,200,12,12,6,6)
sum(PAR6_1)
sum(PAR6_2)
sum(PAR6_3)

PAR7_1<-PAR(cmax7,200,12,12,6,6)
PAR7_2<-PAR(auct7,200,12,12,6,6)
PAR7_3<-PAR(aucall7,200,12,12,6,6)
sum(PAR7_1)
sum(PAR7_2)
sum(PAR7_3)

PAR8_1<-PAR(cmax8,200,12,12,6,6)
PAR8_2<-PAR(auct8,200,12,12,6,6)
PAR8_3<-PAR(aucall8,200,12,12,6,6)
sum(PAR8_1)
sum(PAR8_2)
sum(PAR8_3)

PAR9_1<-PAR(cmax9,200,12,12,6,6)
PAR9_2<-PAR(auct9,200,12,12,6,6)
PAR9_3<-PAR(aucall9,200,12,12,6,6)
sum(PAR9_1)
sum(PAR9_2)
sum(PAR9_3)