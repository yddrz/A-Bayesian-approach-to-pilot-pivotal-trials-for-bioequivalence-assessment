#Proposed model (adjusted)
#Threshold=0.85  wMix=(0.9,0.1)
setwd("/data/Workdir_zhu/Dora")

BLMMA91<-function(pkdata,N,n1,n2,m1,m2){
  Y<-matrix(c(99),nrow=2,ncol=10000)
  for (i in 1:N){
    
    data  <- list(Nobs     = 2*(m1+m2),
                  Npts     = m1+m2,
                  m1       = m1,
                  m2       = m2,  
                  n1       = n1,
                  n2       = n2,
                  pts      = pkdata[[i]]$Subject,
                  trt      = pkdata[[i]]$form,
                  prd      = pkdata[[i]]$period,
                  seq      = pkdata[[i]]$seq,
                  y        = pkdata[[i]]$distance,
                  prior.wt = c(0, 10),
                  prior.mt = c(0, 3),
                  Prior.tau.HN = 0.25,
                  wMix     = c(0.9, 0.1),
                  thres    = c(0.8, 1.25)
    )
    
    MyMod.fit <- bugs(data = data,
                      inits = inits,
                      model.file = "MyMod.txt",
                      parameters = c("theta.pred", "prob.ex","r", "pred.prob.be", "pred.prob.be1"),
                      n.chains = 1,
                      n.iter = 11000,
                      n.burnin = 1000,
                      bugs.seed = 1
    )
    
    if (MyMod.fit$mean$pred.prob.be1>0.85)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.85)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA911_1<-BLMMA91(cmax1,10000,12,12,6,6)
write.csv(BLMMA911_1,"BLMMA911_1.csv",row.names = FALSE)
sum(BLMMA911_1[1,])
sum(BLMMA911_1[2,])


BLMMA911_2<-BLMMA91(auct1,10000,12,12,6,6)
write.csv(BLMMA911_2,"BLMMA911_2.csv",row.names = FALSE)
sum(BLMMA911_2[1,])
sum(BLMMA911_2[2,])


BLMMA912_1<-BLMMA91(cmax2,10000,12,12,6,6)
write.csv(BLMMA912_1,"BLMMA912_1.csv",row.names = FALSE)
sum(BLMMA912_1[1,])
sum(BLMMA912_1[2,])
BLMMA912_2<-BLMMA91(auct2,10000,12,12,6,6)
write.csv(BLMMA912_2,"BLMMA912_2.csv",row.names = FALSE)
sum(BLMMA912_2[1,])
sum(BLMMA912_2[2,])


BLMMA913_1<-BLMMA91(cmax3,10000,12,12,6,6)
write.csv(BLMMA913_1,"BLMMA913_1.csv",row.names = FALSE)
sum(BLMMA913_1[1,])
sum(BLMMA913_1[2,])
BLMMA913_2<-BLMMA91(auct3,10000,12,12,6,6)
write.csv(BLMMA913_2,"BLMMA913_2.csv",row.names = FALSE)
sum(BLMMA913_2[1,])
sum(BLMMA913_2[2,])


BLMMA914_1<-BLMMA91(cmax4,10000,12,12,6,6)
write.csv(BLMMA914_1,"BLMMA914_1.csv",row.names = FALSE)
sum(BLMMA914_1[1,])
sum(BLMMA914_1[2,])
BLMMA914_2<-BLMMA91(auct4,10000,12,12,6,6)
write.csv(BLMMA914_2,"BLMMA914_2.csv",row.names = FALSE)
sum(BLMMA914_2[1,])
sum(BLMMA914_2[2,])


BLMMA915_1<-BLMMA91(cmax5,10000,12,12,6,6)
write.csv(BLMMA915_1,"BLMMA915_1.csv",row.names = FALSE)
sum(BLMMA915_1[1,])
sum(BLMMA915_1[2,])
BLMMA915_2<-BLMMA91(auct5,10000,12,12,6,6)
write.csv(BLMMA915_2,"BLMMA915_2.csv",row.names = FALSE)
sum(BLMMA915_2[1,])
sum(BLMMA915_2[2,])


BLMMA916_1<-BLMMA91(cmax6,10000,12,12,6,6)
write.csv(BLMMA916_1,"BLMMA916_1.csv",row.names = FALSE)
sum(BLMMA916_1[1,])
sum(BLMMA916_1[2,])
BLMMA916_2<-BLMMA91(auct6,10000,12,12,6,6)
write.csv(BLMMA916_2,"BLMMA916_2.csv",row.names = FALSE)
sum(BLMMA916_2[1,])
sum(BLMMA916_2[2,])


BLMMA917_1<-BLMMA91(cmax7,10000,12,12,6,6)
write.csv(BLMMA917_1,"BLMMA917_1.csv",row.names = FALSE)
sum(BLMMA917_1[1,])
sum(BLMMA917_1[2,])
BLMMA917_2<-BLMMA91(auct7,10000,12,12,6,6)
write.csv(BLMMA917_2,"BLMMA917_2.csv",row.names = FALSE)
sum(BLMMA917_2[1,])
sum(BLMMA917_2[2,])

BLMMA918_1<-BLMMA91(cmax8,10000,12,12,6,6)
write.csv(BLMMA918_1,"BLMMA918_1.csv",row.names = FALSE)
sum(BLMMA918_1[1,])
sum(BLMMA918_1[2,])
BLMMA918_2<-BLMMA91(auct8,10000,12,12,6,6)
write.csv(BLMMA918_2,"BLMMA918_2.csv",row.names = FALSE)
sum(BLMMA918_2[1,])
sum(BLMMA918_2[2,])


BLMMA919_1<-BLMMA91(cmax9,10000,12,12,6,6)
write.csv(BLMMA919_1,"BLMMA919_1.csv",row.names = FALSE)
sum(BLMMA919_1[1,])
sum(BLMMA919_1[2,])
BLMMA919_2<-BLMMA91(auct9,10000,12,12,6,6)
write.csv(BLMMA919_2,"BLMMA919_2.csv",row.names = FALSE)
sum(BLMMA919_2[1,])
sum(BLMMA919_2[2,])


#Proposed model (adjusted)
#Threshold=0.85  wMix=(1,0)

BLMMA851<-function(pkdata,N,n1,n2,m1,m2){
  Y<-matrix(c(99),nrow=2,ncol=10000)
  for (i in 1:N){
    
    data  <- list(Nobs     = 2*(m1+m2),
                  Npts     = m1+m2,
                  m1       = m1,
                  m2       = m2,  
                  n1       = n1,
                  n2       = n2,
                  pts      = pkdata[[i]]$Subject,
                  trt      = pkdata[[i]]$form,
                  prd      = pkdata[[i]]$period,
                  seq      = pkdata[[i]]$seq,
                  y        = pkdata[[i]]$distance,
                  prior.wt = c(0, 10),
                  prior.mt = c(0, 3),
                  Prior.tau.HN = 0.25,
                  wMix     = c(1.0, 0),
                  thres    = c(0.8, 1.25)
    )
    
    MyMod.fit <- bugs(data = data,
                      inits = inits,
                      model.file = "MyMod.txt",
                      parameters = c("theta.pred", "prob.ex","r", "pred.prob.be", "pred.prob.be1"),
                      n.chains = 1,
                      n.iter = 11000,
                      n.burnin = 1000,
                      bugs.seed = 1
    )
    
    if (MyMod.fit$mean$pred.prob.be1>0.85)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.85)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA8511_1<-BLMMA851(cmax1,10000,12,12,6,6)
write.csv(BLMMA8511_1,"BLMMA8511_1.csv",row.names = FALSE)
sum(BLMMA8511_1[1,])
sum(BLMMA8511_1[2,])


BLMMA8511_2<-BLMMA851(auct1,10000,12,12,6,6)
write.csv(BLMMA8511_2,"BLMMA8511_2.csv",row.names = FALSE)
sum(BLMMA8511_2[1,])
sum(BLMMA8511_2[2,])


BLMMA8512_1<-BLMMA851(cmax2,10000,12,12,6,6)
write.csv(BLMMA8512_1,"BLMMA8512_1.csv",row.names = FALSE)
sum(BLMMA8512_1[1,])
sum(BLMMA8512_1[2,])
BLMMA8512_2<-BLMMA851(auct2,10000,12,12,6,6)
write.csv(BLMMA8512_2,"BLMMA8512_2.csv",row.names = FALSE)
sum(BLMMA8512_2[1,])
sum(BLMMA8512_2[2,])


BLMMA8513_1<-BLMMA851(cmax3,10000,12,12,6,6)
write.csv(BLMMA8513_1,"BLMMA8513_1.csv",row.names = FALSE)
sum(BLMMA8513_1[1,])
sum(BLMMA8513_1[2,])
BLMMA8513_2<-BLMMA851(auct3,10000,12,12,6,6)
write.csv(BLMMA8513_2,"BLMMA8513_2.csv",row.names = FALSE)
sum(BLMMA8513_2[1,])
sum(BLMMA8513_2[2,])


BLMMA8514_1<-BLMMA851(cmax4,10000,12,12,6,6)
write.csv(BLMMA8514_1,"BLMMA8514_1.csv",row.names = FALSE)
sum(BLMMA8514_1[1,])
sum(BLMMA8514_1[2,])
BLMMA8514_2<-BLMMA851(auct4,10000,12,12,6,6)
write.csv(BLMMA8514_2,"BLMMA8514_2.csv",row.names = FALSE)
sum(BLMMA8514_2[1,])
sum(BLMMA8514_2[2,])


BLMMA8515_1<-BLMMA851(cmax5,10000,12,12,6,6)
write.csv(BLMMA8515_1,"BLMMA8515_1.csv",row.names = FALSE)
sum(BLMMA8515_1[1,])
sum(BLMMA8515_1[2,])
BLMMA8515_2<-BLMMA851(auct5,10000,12,12,6,6)
write.csv(BLMMA8515_2,"BLMMA8515_2.csv",row.names = FALSE)
sum(BLMMA8515_2[1,])
sum(BLMMA8515_2[2,])


BLMMA8516_1<-BLMMA851(cmax6,10000,12,12,6,6)
write.csv(BLMMA8516_1,"BLMMA8516_1.csv",row.names = FALSE)
sum(BLMMA8516_1[1,])
sum(BLMMA8516_1[2,])
BLMMA8516_2<-BLMMA851(auct6,10000,12,12,6,6)
write.csv(BLMMA8516_2,"BLMMA8516_2.csv",row.names = FALSE)
sum(BLMMA8516_2[1,])
sum(BLMMA8516_2[2,])


BLMMA8517_1<-BLMMA851(cmax7,10000,12,12,6,6)
write.csv(BLMMA8517_1,"BLMMA8517_1.csv",row.names = FALSE)
sum(BLMMA8517_1[1,])
sum(BLMMA8517_1[2,])
BLMMA8517_2<-BLMMA851(auct7,10000,12,12,6,6)
write.csv(BLMMA8517_2,"BLMMA8517_2.csv",row.names = FALSE)
sum(BLMMA8517_2[1,])
sum(BLMMA8517_2[2,])

BLMMA8518_1<-BLMMA851(cmax8,10000,12,12,6,6)
write.csv(BLMMA8518_1,"BLMMA8518_1.csv",row.names = FALSE)
sum(BLMMA8518_1[1,])
sum(BLMMA8518_1[2,])
BLMMA8518_2<-BLMMA851(auct8,10000,12,12,6,6)
write.csv(BLMMA8518_2,"BLMMA8518_2.csv",row.names = FALSE)
sum(BLMMA8518_2[1,])
sum(BLMMA8518_2[2,])


BLMMA8519_1<-BLMMA851(cmax9,10000,12,12,6,6)
write.csv(BLMMA8519_1,"BLMMA8519_1.csv",row.names = FALSE)
sum(BLMMA8519_1[1,])
sum(BLMMA8519_1[2,])
BLMMA8519_2<-BLMMA851(auct9,10000,12,12,6,6)
write.csv(BLMMA8519_2,"BLMMA8519_2.csv",row.names = FALSE)
sum(BLMMA8519_2[1,])
sum(BLMMA8519_2[2,])
