#Proposed model (adjusted)
#Threshold=0.85  wMix=(0.9,0.1)
setwd("/data/Workdir_zhu/Dora")

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
BLMMA8511_1<-BLMMA851(cmax1,10000,12,12,6,6)
write.csv(BLMMA8511_1,"BLMMA8511_1.csv",row.names = FALSE)
sum(BLMMA8511_1[1,])
BLMMA8511_2<-BLMMA851(auct1,10000,12,12,6,6)
write.csv(BLMMA8511_2,"BLMMA8511_2.csv",row.names = FALSE)
sum(BLMMA8511_2[1,])

BLMMA8512_1<-BLMMA851(cmax2,10000,12,12,6,6)
write.csv(BLMMA8512_1,"BLMMA8512_1.csv",row.names = FALSE)
sum(BLMMA8512_1[1,])
BLMMA8512_2<-BLMMA851(auct2,10000,12,12,6,6)
write.csv(BLMMA8512_2,"BLMMA8512_2.csv",row.names = FALSE)
sum(BLMMA8512_2[1,])

BLMMA8513_1<-BLMMA851(cmax3,10000,12,12,6,6)
write.csv(BLMMA8513_1,"BLMMA8513_1.csv",row.names = FALSE)
sum(BLMMA8513_1[1,])
BLMMA8513_2<-BLMMA851(auct3,10000,12,12,6,6)
write.csv(BLMMA8513_2,"BLMMA8513_2.csv",row.names = FALSE)
sum(BLMMA8513_2[1,])

BLMMA8514_1<-BLMMA851(cmax4,10000,12,12,6,6)
write.csv(BLMMA8514_1,"BLMMA8514_1.csv",row.names = FALSE)
sum(BLMMA8514_1[1,])
BLMMA8514_2<-BLMMA851(auct4,10000,12,12,6,6)
write.csv(BLMMA8514_2,"BLMMA8514_2.csv",row.names = FALSE)
sum(BLMMA8514_2[1,])

BLMMA8515_1<-BLMMA851(cmax5,10000,12,12,6,6)
write.csv(BLMMA8515_1,"BLMMA8515_1.csv",row.names = FALSE)
sum(BLMMA8515_1[1,])
BLMMA8515_2<-BLMMA851(auct5,10000,12,12,6,6)
write.csv(BLMMA8515_2,"BLMMA8515_2.csv",row.names = FALSE)
sum(BLMMA8515_2[1,])

BLMMA8516_1<-BLMMA851(cmax6,10000,12,12,6,6)
write.csv(BLMMA8516_1,"BLMMA8516_1.csv",row.names = FALSE)
sum(BLMMA8516_1[1,])
BLMMA8516_2<-BLMMA851(auct6,10000,12,12,6,6)
write.csv(BLMMA8516_2,"BLMMA8516_2.csv",row.names = FALSE)
sum(BLMMA8516_2[1,])

BLMMA8517_1<-BLMMA851(cmax7,10000,12,12,6,6)
write.csv(BLMMA8517_1,"BLMMA8517_1.csv",row.names = FALSE)
sum(BLMMA8517_1[1,])
BLMMA8517_2<-BLMMA851(auct7,10000,12,12,6,6)
write.csv(BLMMA8517_2,"BLMMA8517_2.csv",row.names = FALSE)
sum(BLMMA8517_2[1,])

BLMMA8518_1<-BLMMA851(cmax8,10000,12,12,6,6)
write.csv(BLMMA8518_1,"BLMMA8518_1.csv",row.names = FALSE)
sum(BLMMA8518_1[1,])
BLMMA8518_2<-BLMMA851(auct8,10000,12,12,6,6)
write.csv(BLMMA8518_2,"BLMMA8518_2.csv",row.names = FALSE)
sum(BLMMA8518_2[1,])

BLMMA8519_1<-BLMMA851(cmax9,10000,12,12,6,6)
write.csv(BLMMA8519_1,"BLMMA8519_1.csv",row.names = FALSE)
sum(BLMMA8519_1[1,])
BLMMA8519_2<-BLMMA851(auct9,10000,12,12,6,6)
write.csv(BLMMA8519_2,"BLMMA8519_2.csv",row.names = FALSE)
sum(BLMMA8519_2[1,])


#Proposed model (adjusted)
#Threshold=0.86  wMix=(0.9,0.1)
setwd("/data/Workdir_zhu/Dora")

BLMMA861<-function(pkdata,N,n1,n2,m1,m2){
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
                  wMix     = c(0.9,0.1),
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.86)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.86)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA8611_1<-BLMMA861(cmax1,10000,12,12,6,6)
write.csv(BLMMA8611_1,"BLMMA8611_1.csv",row.names = FALSE)
sum(BLMMA8611_1[1,])
BLMMA8611_2<-BLMMA861(auct1,10000,12,12,6,6)
write.csv(BLMMA8611_2,"BLMMA8611_2.csv",row.names = FALSE)
sum(BLMMA8611_2[1,])

BLMMA8612_1<-BLMMA861(cmax2,10000,12,12,6,6)
write.csv(BLMMA8612_1,"BLMMA8612_1.csv",row.names = FALSE)
sum(BLMMA8612_1[1,])
BLMMA8612_2<-BLMMA861(auct2,10000,12,12,6,6)
write.csv(BLMMA8612_2,"BLMMA8612_2.csv",row.names = FALSE)
sum(BLMMA8612_2[1,])

BLMMA8613_1<-BLMMA861(cmax3,10000,12,12,6,6)
write.csv(BLMMA8613_1,"BLMMA8613_1.csv",row.names = FALSE)
sum(BLMMA8613_1[1,])
BLMMA8613_2<-BLMMA861(auct3,10000,12,12,6,6)
write.csv(BLMMA8613_2,"BLMMA8613_2.csv",row.names = FALSE)
sum(BLMMA8613_2[1,])

BLMMA8614_1<-BLMMA861(cmax4,10000,12,12,6,6)
write.csv(BLMMA8614_1,"BLMMA8614_1.csv",row.names = FALSE)
sum(BLMMA8614_1[1,])
BLMMA8614_2<-BLMMA861(auct4,10000,12,12,6,6)
write.csv(BLMMA8614_2,"BLMMA8614_2.csv",row.names = FALSE)
sum(BLMMA8614_2[1,])

BLMMA8615_1<-BLMMA861(cmax5,10000,12,12,6,6)
write.csv(BLMMA8615_1,"BLMMA8615_1.csv",row.names = FALSE)
sum(BLMMA8615_1[1,])
BLMMA8615_2<-BLMMA861(auct5,10000,12,12,6,6)
write.csv(BLMMA8615_2,"BLMMA8615_2.csv",row.names = FALSE)
sum(BLMMA8615_2[1,])

BLMMA8616_1<-BLMMA861(cmax6,10000,12,12,6,6)
write.csv(BLMMA8616_1,"BLMMA8616_1.csv",row.names = FALSE)
sum(BLMMA8616_1[1,])
BLMMA8616_2<-BLMMA861(auct6,10000,12,12,6,6)
write.csv(BLMMA8616_2,"BLMMA8616_2.csv",row.names = FALSE)
sum(BLMMA8616_2[1,])

BLMMA8617_1<-BLMMA861(cmax7,10000,12,12,6,6)
write.csv(BLMMA8617_1,"BLMMA8617_1.csv",row.names = FALSE)
sum(BLMMA8617_1[1,])
BLMMA8617_2<-BLMMA861(auct7,10000,12,12,6,6)
write.csv(BLMMA8617_2,"BLMMA8617_2.csv",row.names = FALSE)
sum(BLMMA8617_2[1,])

BLMMA8618_1<-BLMMA861(cmax8,10000,12,12,6,6)
write.csv(BLMMA8618_1,"BLMMA8618_1.csv",row.names = FALSE)
sum(BLMMA8618_1[1,])
BLMMA8618_2<-BLMMA861(auct8,10000,12,12,6,6)
write.csv(BLMMA8618_2,"BLMMA8618_2.csv",row.names = FALSE)
sum(BLMMA8618_2[1,])

BLMMA8619_1<-BLMMA861(cmax9,10000,12,12,6,6)
write.csv(BLMMA8619_1,"BLMMA8619_1.csv",row.names = FALSE)
sum(BLMMA8619_1[1,])
BLMMA8619_2<-BLMMA861(auct9,10000,12,12,6,6)
write.csv(BLMMA8619_2,"BLMMA8619_2.csv",row.names = FALSE)
sum(BLMMA8619_2[1,])


#Proposed model (adjusted)
#Threshold=0.87  wMix=(0.9,0.1)
setwd("/data/Workdir_zhu/Dora")

BLMMA871<-function(pkdata,N,n1,n2,m1,m2){
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
                  wMix     = c(0.9,0.1),
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.87)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.87)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA8711_1<-BLMMA871(cmax1,10000,12,12,6,6)
write.csv(BLMMA8711_1,"BLMMA8711_1.csv",row.names = FALSE)
sum(BLMMA8711_1[1,])
BLMMA8711_2<-BLMMA871(auct1,10000,12,12,6,6)
write.csv(BLMMA8711_2,"BLMMA8711_2.csv",row.names = FALSE)
sum(BLMMA8711_2[1,])

BLMMA8712_1<-BLMMA871(cmax2,10000,12,12,6,6)
write.csv(BLMMA8712_1,"BLMMA8712_1.csv",row.names = FALSE)
sum(BLMMA8712_1[1,])
BLMMA8712_2<-BLMMA871(auct2,10000,12,12,6,6)
write.csv(BLMMA8712_2,"BLMMA8712_2.csv",row.names = FALSE)
sum(BLMMA8712_2[1,])


BLMMA8713_1<-BLMMA871(cmax3,10000,12,12,6,6)
write.csv(BLMMA8713_1,"BLMMA8713_1.csv",row.names = FALSE)
sum(BLMMA8713_1[1,])
BLMMA8713_2<-BLMMA871(auct3,10000,12,12,6,6)
write.csv(BLMMA8713_2,"BLMMA8713_2.csv",row.names = FALSE)
sum(BLMMA8713_2[1,])

BLMMA8714_1<-BLMMA871(cmax4,10000,12,12,6,6)
write.csv(BLMMA8714_1,"BLMMA8714_1.csv",row.names = FALSE)
sum(BLMMA8714_1[1,])
BLMMA8714_2<-BLMMA871(auct4,10000,12,12,6,6)
write.csv(BLMMA8714_2,"BLMMA8714_2.csv",row.names = FALSE)
sum(BLMMA8714_2[1,])

BLMMA8715_1<-BLMMA871(cmax5,10000,12,12,6,6)
write.csv(BLMMA8715_1,"BLMMA8715_1.csv",row.names = FALSE)
sum(BLMMA8715_1[1,])
BLMMA8715_2<-BLMMA871(auct5,10000,12,12,6,6)
write.csv(BLMMA8715_2,"BLMMA8715_2.csv",row.names = FALSE)
sum(BLMMA8715_2[1,])


BLMMA8716_1<-BLMMA871(cmax6,10000,12,12,6,6)
write.csv(BLMMA8716_1,"BLMMA8716_1.csv",row.names = FALSE)
sum(BLMMA8716_1[1,])
BLMMA8716_2<-BLMMA871(auct6,10000,12,12,6,6)
write.csv(BLMMA8716_2,"BLMMA8716_2.csv",row.names = FALSE)
sum(BLMMA8716_2[1,])

BLMMA8717_1<-BLMMA871(cmax7,10000,12,12,6,6)
write.csv(BLMMA8717_1,"BLMMA8717_1.csv",row.names = FALSE)
sum(BLMMA8717_1[1,])
BLMMA8717_2<-BLMMA871(auct7,10000,12,12,6,6)
write.csv(BLMMA8717_2,"BLMMA8717_2.csv",row.names = FALSE)
sum(BLMMA8717_2[1,])

BLMMA8718_1<-BLMMA871(cmax8,10000,12,12,6,6)
write.csv(BLMMA8718_1,"BLMMA8718_1.csv",row.names = FALSE)
sum(BLMMA8718_1[1,])
BLMMA8718_2<-BLMMA871(auct8,10000,12,12,6,6)
write.csv(BLMMA8718_2,"BLMMA8718_2.csv",row.names = FALSE)
sum(BLMMA8718_2[1,])

BLMMA8719_1<-BLMMA871(cmax9,10000,12,12,6,6)
write.csv(BLMMA8719_1,"BLMMA8719_1.csv",row.names = FALSE)
sum(BLMMA8719_1[1,])
BLMMA8719_2<-BLMMA871(auct9,10000,12,12,6,6)
write.csv(BLMMA8719_2,"BLMMA8719_2.csv",row.names = FALSE)
sum(BLMMA8719_2[1,])
