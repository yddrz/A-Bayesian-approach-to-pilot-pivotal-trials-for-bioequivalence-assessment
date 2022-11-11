#Proposed model (adjusted)
#Threshold=0.75  wMix=(0.8,0.2)
setwd("/data/Workdir_zhu/Dora")

BLMMA751<-function(pkdata,N,n1,n2,m1,m2){
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
                  wMix     = c(0.8,0.2),
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.75)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.75)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA7511_1<-BLMMA751(cmax1,10000,12,12,6,6)
write.csv(BLMMA7511_1,"BLMMA7511_1.csv",row.names = FALSE)
sum(BLMMA7511_1[1,])
BLMMA7511_2<-BLMMA751(auct1,10000,12,12,6,6)
write.csv(BLMMA7511_2,"BLMMA7511_2.csv",row.names = FALSE)
sum(BLMMA7511_2[1,])

BLMMA7512_1<-BLMMA751(cmax2,10000,12,12,6,6)
write.csv(BLMMA7512_1,"BLMMA7512_1.csv",row.names = FALSE)
sum(BLMMA7512_1[1,])
BLMMA7512_2<-BLMMA751(auct2,10000,12,12,6,6)
write.csv(BLMMA7512_2,"BLMMA7512_2.csv",row.names = FALSE)
sum(BLMMA7512_2[1,])

BLMMA7513_1<-BLMMA751(cmax3,10000,12,12,6,6)
write.csv(BLMMA7513_1,"BLMMA7513_1.csv",row.names = FALSE)
sum(BLMMA7513_1[1,])
BLMMA7513_2<-BLMMA751(auct3,10000,12,12,6,6)
write.csv(BLMMA7513_2,"BLMMA7513_2.csv",row.names = FALSE)
sum(BLMMA7513_2[1,])

BLMMA7514_1<-BLMMA751(cmax4,10000,12,12,6,6)
write.csv(BLMMA7514_1,"BLMMA7514_1.csv",row.names = FALSE)
sum(BLMMA7514_1[1,])
BLMMA7514_2<-BLMMA751(auct4,10000,12,12,6,6)
write.csv(BLMMA7514_2,"BLMMA7514_2.csv",row.names = FALSE)
sum(BLMMA7514_2[1,])

BLMMA7515_1<-BLMMA751(cmax5,10000,12,12,6,6)
write.csv(BLMMA7515_1,"BLMMA7515_1.csv",row.names = FALSE)
sum(BLMMA7515_1[1,])
BLMMA7515_2<-BLMMA751(auct5,10000,12,12,6,6)
write.csv(BLMMA7515_2,"BLMMA7515_2.csv",row.names = FALSE)
sum(BLMMA7515_2[1,])

BLMMA7516_1<-BLMMA751(cmax6,10000,12,12,6,6)
write.csv(BLMMA7516_1,"BLMMA7516_1.csv",row.names = FALSE)
sum(BLMMA7516_1[1,])
BLMMA7516_2<-BLMMA751(auct6,10000,12,12,6,6)
write.csv(BLMMA7516_2,"BLMMA7516_2.csv",row.names = FALSE)
sum(BLMMA7516_2[1,])

BLMMA7517_1<-BLMMA751(cmax7,10000,12,12,6,6)
write.csv(BLMMA7517_1,"BLMMA7517_1.csv",row.names = FALSE)
sum(BLMMA7517_1[1,])
BLMMA7517_2<-BLMMA751(auct7,10000,12,12,6,6)
write.csv(BLMMA7517_2,"BLMMA7517_2.csv",row.names = FALSE)
sum(BLMMA7517_2[1,])

BLMMA7518_1<-BLMMA751(cmax8,10000,12,12,6,6)
write.csv(BLMMA7518_1,"BLMMA7518_1.csv",row.names = FALSE)
sum(BLMMA7518_1[1,])
BLMMA7518_2<-BLMMA751(auct8,10000,12,12,6,6)
write.csv(BLMMA7518_2,"BLMMA7518_2.csv",row.names = FALSE)
sum(BLMMA7518_2[1,])

BLMMA7519_1<-BLMMA751(cmax9,10000,12,12,6,6)
write.csv(BLMMA7519_1,"BLMMA7519_1.csv",row.names = FALSE)
sum(BLMMA7519_1[1,])
BLMMA7519_2<-BLMMA751(auct9,10000,12,12,6,6)
write.csv(BLMMA7519_2,"BLMMA7519_2.csv",row.names = FALSE)
sum(BLMMA7519_2[1,])


#Proposed model (adjusted)
#Threshold=0.76  wMix=(0.9,0.1)
setwd("/data/Workdir_zhu/Dora")

BLMMA761<-function(pkdata,N,n1,n2,m1,m2){
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
                  wMix     = c(0.8,0.2),
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.76)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.76)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA7611_1<-BLMMA761(cmax1,10000,12,12,6,6)
write.csv(BLMMA7611_1,"BLMMA7611_1.csv",row.names = FALSE)
sum(BLMMA7611_1[1,])
BLMMA7611_2<-BLMMA761(auct1,10000,12,12,6,6)
write.csv(BLMMA7611_2,"BLMMA7611_2.csv",row.names = FALSE)
sum(BLMMA7611_2[1,])

BLMMA7612_1<-BLMMA761(cmax2,10000,12,12,6,6)
write.csv(BLMMA7612_1,"BLMMA7612_1.csv",row.names = FALSE)
sum(BLMMA7612_1[1,])
BLMMA7612_2<-BLMMA761(auct2,10000,12,12,6,6)
write.csv(BLMMA7612_2,"BLMMA7612_2.csv",row.names = FALSE)
sum(BLMMA7612_2[1,])

BLMMA7613_1<-BLMMA761(cmax3,10000,12,12,6,6)
write.csv(BLMMA7613_1,"BLMMA7613_1.csv",row.names = FALSE)
sum(BLMMA7613_1[1,])
BLMMA7613_2<-BLMMA761(auct3,10000,12,12,6,6)
write.csv(BLMMA7613_2,"BLMMA7613_2.csv",row.names = FALSE)
sum(BLMMA7613_2[1,])

BLMMA7614_1<-BLMMA761(cmax4,10000,12,12,6,6)
write.csv(BLMMA7614_1,"BLMMA7614_1.csv",row.names = FALSE)
sum(BLMMA7614_1[1,])
BLMMA7614_2<-BLMMA761(auct4,10000,12,12,6,6)
write.csv(BLMMA7614_2,"BLMMA7614_2.csv",row.names = FALSE)
sum(BLMMA7614_2[1,])

BLMMA7615_1<-BLMMA761(cmax5,10000,12,12,6,6)
write.csv(BLMMA7615_1,"BLMMA7615_1.csv",row.names = FALSE)
sum(BLMMA7615_1[1,])
BLMMA7615_2<-BLMMA761(auct5,10000,12,12,6,6)
write.csv(BLMMA7615_2,"BLMMA7615_2.csv",row.names = FALSE)
sum(BLMMA7615_2[1,])

BLMMA7616_1<-BLMMA761(cmax6,10000,12,12,6,6)
write.csv(BLMMA7616_1,"BLMMA7616_1.csv",row.names = FALSE)
sum(BLMMA7616_1[1,])
BLMMA7616_2<-BLMMA761(auct6,10000,12,12,6,6)
write.csv(BLMMA7616_2,"BLMMA7616_2.csv",row.names = FALSE)
sum(BLMMA7616_2[1,])

BLMMA7617_1<-BLMMA761(cmax7,10000,12,12,6,6)
write.csv(BLMMA7617_1,"BLMMA7617_1.csv",row.names = FALSE)
sum(BLMMA7617_1[1,])
BLMMA7617_2<-BLMMA761(auct7,10000,12,12,6,6)
write.csv(BLMMA7617_2,"BLMMA7617_2.csv",row.names = FALSE)
sum(BLMMA7617_2[1,])

BLMMA7618_1<-BLMMA761(cmax8,10000,12,12,6,6)
write.csv(BLMMA7618_1,"BLMMA7618_1.csv",row.names = FALSE)
sum(BLMMA7618_1[1,])
BLMMA7618_2<-BLMMA761(auct8,10000,12,12,6,6)
write.csv(BLMMA7618_2,"BLMMA7618_2.csv",row.names = FALSE)
sum(BLMMA7618_2[1,])

BLMMA7619_1<-BLMMA761(cmax9,10000,12,12,6,6)
write.csv(BLMMA7619_1,"BLMMA7619_1.csv",row.names = FALSE)
sum(BLMMA7619_1[1,])
BLMMA7619_2<-BLMMA761(auct9,10000,12,12,6,6)
write.csv(BLMMA7619_2,"BLMMA7619_2.csv",row.names = FALSE)
sum(BLMMA7619_2[1,])


#Proposed model (adjusted)
#Threshold=0.77  wMix=(0.9,0.1)
setwd("/data/Workdir_zhu/Dora")

BLMMA771<-function(pkdata,N,n1,n2,m1,m2){
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
                  wMix     = c(0.8,0.2),
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.77)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.77)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA7711_1<-BLMMA771(cmax1,10000,12,12,6,6)
write.csv(BLMMA7711_1,"BLMMA7711_1.csv",row.names = FALSE)
sum(BLMMA7711_1[1,])
BLMMA7711_2<-BLMMA771(auct1,10000,12,12,6,6)
write.csv(BLMMA7711_2,"BLMMA7711_2.csv",row.names = FALSE)
sum(BLMMA7711_2[1,])

BLMMA7712_1<-BLMMA771(cmax2,10000,12,12,6,6)
write.csv(BLMMA7712_1,"BLMMA7712_1.csv",row.names = FALSE)
sum(BLMMA7712_1[1,])
BLMMA7712_2<-BLMMA771(auct2,10000,12,12,6,6)
write.csv(BLMMA7712_2,"BLMMA7712_2.csv",row.names = FALSE)
sum(BLMMA7712_2[1,])


BLMMA7713_1<-BLMMA771(cmax3,10000,12,12,6,6)
write.csv(BLMMA7713_1,"BLMMA7713_1.csv",row.names = FALSE)
sum(BLMMA7713_1[1,])
BLMMA7713_2<-BLMMA771(auct3,10000,12,12,6,6)
write.csv(BLMMA7713_2,"BLMMA7713_2.csv",row.names = FALSE)
sum(BLMMA7713_2[1,])

BLMMA7714_1<-BLMMA771(cmax4,10000,12,12,6,6)
write.csv(BLMMA7714_1,"BLMMA7714_1.csv",row.names = FALSE)
sum(BLMMA7714_1[1,])
BLMMA7714_2<-BLMMA771(auct4,10000,12,12,6,6)
write.csv(BLMMA7714_2,"BLMMA7714_2.csv",row.names = FALSE)
sum(BLMMA7714_2[1,])

BLMMA7715_1<-BLMMA771(cmax5,10000,12,12,6,6)
write.csv(BLMMA7715_1,"BLMMA7715_1.csv",row.names = FALSE)
sum(BLMMA7715_1[1,])
BLMMA7715_2<-BLMMA771(auct5,10000,12,12,6,6)
write.csv(BLMMA7715_2,"BLMMA7715_2.csv",row.names = FALSE)
sum(BLMMA7715_2[1,])


BLMMA7716_1<-BLMMA771(cmax6,10000,12,12,6,6)
write.csv(BLMMA7716_1,"BLMMA7716_1.csv",row.names = FALSE)
sum(BLMMA7716_1[1,])
BLMMA7716_2<-BLMMA771(auct6,10000,12,12,6,6)
write.csv(BLMMA7716_2,"BLMMA7716_2.csv",row.names = FALSE)
sum(BLMMA7716_2[1,])

BLMMA7717_1<-BLMMA771(cmax7,10000,12,12,6,6)
write.csv(BLMMA7717_1,"BLMMA7717_1.csv",row.names = FALSE)
sum(BLMMA7717_1[1,])
BLMMA7717_2<-BLMMA771(auct7,10000,12,12,6,6)
write.csv(BLMMA7717_2,"BLMMA7717_2.csv",row.names = FALSE)
sum(BLMMA7717_2[1,])

BLMMA7718_1<-BLMMA771(cmax8,10000,12,12,6,6)
write.csv(BLMMA7718_1,"BLMMA7718_1.csv",row.names = FALSE)
sum(BLMMA7718_1[1,])
BLMMA7718_2<-BLMMA771(auct8,10000,12,12,6,6)
write.csv(BLMMA7718_2,"BLMMA7718_2.csv",row.names = FALSE)
sum(BLMMA7718_2[1,])

BLMMA7719_1<-BLMMA771(cmax9,10000,12,12,6,6)
write.csv(BLMMA7719_1,"BLMMA7719_1.csv",row.names = FALSE)
sum(BLMMA7719_1[1,])
BLMMA7719_2<-BLMMA771(auct9,10000,12,12,6,6)
write.csv(BLMMA7719_2,"BLMMA7719_2.csv",row.names = FALSE)
sum(BLMMA7719_2[1,])
