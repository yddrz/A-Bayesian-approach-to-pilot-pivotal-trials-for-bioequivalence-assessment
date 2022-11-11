#Proposed model (adjusted)
#Threshold=0.95  wMix=(1.0,0)
setwd("/data/Workdir_zhu/Dora")

BLMMA951<-function(pkdata,N,n1,n2,m1,m2){
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.95)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.95)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA9511_1<-BLMMA951(cmax1,10000,12,12,6,6)
write.csv(BLMMA9511_1,"BLMMA9511_1.csv",row.names = FALSE)
sum(BLMMA9511_1[1,])
sum(BLMMA9511_1[2,])


BLMMA9511_2<-BLMMA951(auct1,10000,12,12,6,6)
write.csv(BLMMA9511_2,"BLMMA9511_2.csv",row.names = FALSE)
sum(BLMMA9511_2[1,])
sum(BLMMA9511_2[2,])


BLMMA9512_1<-BLMMA951(cmax2,10000,12,12,6,6)
write.csv(BLMMA9512_1,"BLMMA9512_1.csv",row.names = FALSE)
sum(BLMMA9512_1[1,])
sum(BLMMA9512_1[2,])
BLMMA9512_2<-BLMMA951(auct2,10000,12,12,6,6)
write.csv(BLMMA9512_2,"BLMMA9512_2.csv",row.names = FALSE)
sum(BLMMA9512_2[1,])
sum(BLMMA9512_2[2,])


BLMMA9513_1<-BLMMA951(cmax3,10000,12,12,6,6)
write.csv(BLMMA9513_1,"BLMMA9513_1.csv",row.names = FALSE)
sum(BLMMA9513_1[1,])
sum(BLMMA9513_1[2,])
BLMMA9513_2<-BLMMA951(auct3,10000,12,12,6,6)
write.csv(BLMMA9513_2,"BLMMA9513_2.csv",row.names = FALSE)
sum(BLMMA9513_2[1,])
sum(BLMMA9513_2[2,])


BLMMA9514_1<-BLMMA951(cmax4,10000,12,12,6,6)
write.csv(BLMMA9514_1,"BLMMA9514_1.csv",row.names = FALSE)
sum(BLMMA9514_1[1,])
sum(BLMMA9514_1[2,])
BLMMA9514_2<-BLMMA951(auct4,10000,12,12,6,6)
write.csv(BLMMA9514_2,"BLMMA9514_2.csv",row.names = FALSE)
sum(BLMMA9514_2[1,])
sum(BLMMA9514_2[2,])


BLMMA9515_1<-BLMMA951(cmax5,10000,12,12,6,6)
write.csv(BLMMA9515_1,"BLMMA9515_1.csv",row.names = FALSE)
sum(BLMMA9515_1[1,])
sum(BLMMA9515_1[2,])
BLMMA9515_2<-BLMMA951(auct5,10000,12,12,6,6)
write.csv(BLMMA9515_2,"BLMMA9515_2.csv",row.names = FALSE)
sum(BLMMA9515_2[1,])
sum(BLMMA9515_2[2,])


BLMMA9516_1<-BLMMA951(cmax6,10000,12,12,6,6)
write.csv(BLMMA9516_1,"BLMMA9516_1.csv",row.names = FALSE)
sum(BLMMA9516_1[1,])
sum(BLMMA9516_1[2,])
BLMMA9516_2<-BLMMA951(auct6,10000,12,12,6,6)
write.csv(BLMMA9516_2,"BLMMA9516_2.csv",row.names = FALSE)
sum(BLMMA9516_2[1,])
sum(BLMMA9516_2[2,])


BLMMA9517_1<-BLMMA951(cmax7,10000,12,12,6,6)
write.csv(BLMMA9517_1,"BLMMA9517_1.csv",row.names = FALSE)
sum(BLMMA9517_1[1,])
sum(BLMMA9517_1[2,])
BLMMA9517_2<-BLMMA951(auct7,10000,12,12,6,6)
write.csv(BLMMA9517_2,"BLMMA9517_2.csv",row.names = FALSE)
sum(BLMMA9517_2[1,])
sum(BLMMA9517_2[2,])

BLMMA9518_1<-BLMMA951(cmax8,10000,12,12,6,6)
write.csv(BLMMA9518_1,"BLMMA9518_1.csv",row.names = FALSE)
sum(BLMMA9518_1[1,])
sum(BLMMA9518_1[2,])
BLMMA9518_2<-BLMMA951(auct8,10000,12,12,6,6)
write.csv(BLMMA9518_2,"BLMMA9518_2.csv",row.names = FALSE)
sum(BLMMA9518_2[1,])
sum(BLMMA9518_2[2,])


BLMMA9519_1<-BLMMA951(cmax9,10000,12,12,6,6)
write.csv(BLMMA9519_1,"BLMMA9519_1.csv",row.names = FALSE)
sum(BLMMA9519_1[1,])
sum(BLMMA9519_1[2,])
BLMMA9519_2<-BLMMA951(auct9,10000,12,12,6,6)
write.csv(BLMMA9519_2,"BLMMA9519_2.csv",row.names = FALSE)
sum(BLMMA9519_2[1,])
sum(BLMMA9519_2[2,])


#Proposed model (adjusted)
#Threshold=0.96  wMix=(1.0,0)
setwd("/data/Workdir_zhu/Dora")

BLMMA961<-function(pkdata,N,n1,n2,m1,m2){
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.96)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.96)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA9611_1<-BLMMA961(cmax1,10000,12,12,6,6)
write.csv(BLMMA9611_1,"BLMMA9611_1.csv",row.names = FALSE)
sum(BLMMA9611_1[1,])
sum(BLMMA9611_1[2,])


BLMMA9611_2<-BLMMA961(auct1,10000,12,12,6,6)
write.csv(BLMMA9611_2,"BLMMA9611_2.csv",row.names = FALSE)
sum(BLMMA9611_2[1,])
sum(BLMMA9611_2[2,])


BLMMA9612_1<-BLMMA961(cmax2,10000,12,12,6,6)
write.csv(BLMMA9612_1,"BLMMA9612_1.csv",row.names = FALSE)
sum(BLMMA9612_1[1,])
sum(BLMMA9612_1[2,])
BLMMA9612_2<-BLMMA961(auct2,10000,12,12,6,6)
write.csv(BLMMA9612_2,"BLMMA9612_2.csv",row.names = FALSE)
sum(BLMMA9612_2[1,])
sum(BLMMA9612_2[2,])


BLMMA9613_1<-BLMMA961(cmax3,10000,12,12,6,6)
write.csv(BLMMA9613_1,"BLMMA9613_1.csv",row.names = FALSE)
sum(BLMMA9613_1[1,])
sum(BLMMA9613_1[2,])
BLMMA9613_2<-BLMMA961(auct3,10000,12,12,6,6)
write.csv(BLMMA9613_2,"BLMMA9613_2.csv",row.names = FALSE)
sum(BLMMA9613_2[1,])
sum(BLMMA9613_2[2,])


BLMMA9614_1<-BLMMA961(cmax4,10000,12,12,6,6)
write.csv(BLMMA9614_1,"BLMMA9614_1.csv",row.names = FALSE)
sum(BLMMA9614_1[1,])
sum(BLMMA9614_1[2,])
BLMMA9614_2<-BLMMA961(auct4,10000,12,12,6,6)
write.csv(BLMMA9614_2,"BLMMA9614_2.csv",row.names = FALSE)
sum(BLMMA9614_2[1,])
sum(BLMMA9614_2[2,])


BLMMA9615_1<-BLMMA961(cmax5,10000,12,12,6,6)
write.csv(BLMMA9615_1,"BLMMA9615_1.csv",row.names = FALSE)
sum(BLMMA9615_1[1,])
sum(BLMMA9615_1[2,])
BLMMA9615_2<-BLMMA961(auct5,10000,12,12,6,6)
write.csv(BLMMA9615_2,"BLMMA9615_2.csv",row.names = FALSE)
sum(BLMMA9615_2[1,])
sum(BLMMA9615_2[2,])


BLMMA9616_1<-BLMMA961(cmax6,10000,12,12,6,6)
write.csv(BLMMA9616_1,"BLMMA9616_1.csv",row.names = FALSE)
sum(BLMMA9616_1[1,])
sum(BLMMA9616_1[2,])
BLMMA9616_2<-BLMMA961(auct6,10000,12,12,6,6)
write.csv(BLMMA9616_2,"BLMMA9616_2.csv",row.names = FALSE)
sum(BLMMA9616_2[1,])
sum(BLMMA9616_2[2,])


BLMMA9617_1<-BLMMA961(cmax7,10000,12,12,6,6)
write.csv(BLMMA9617_1,"BLMMA9617_1.csv",row.names = FALSE)
sum(BLMMA9617_1[1,])
sum(BLMMA9617_1[2,])
BLMMA9617_2<-BLMMA961(auct7,10000,12,12,6,6)
write.csv(BLMMA9617_2,"BLMMA9617_2.csv",row.names = FALSE)
sum(BLMMA9617_2[1,])
sum(BLMMA9617_2[2,])

BLMMA9618_1<-BLMMA961(cmax8,10000,12,12,6,6)
write.csv(BLMMA9618_1,"BLMMA9618_1.csv",row.names = FALSE)
sum(BLMMA9618_1[1,])
sum(BLMMA9618_1[2,])
BLMMA9618_2<-BLMMA961(auct8,10000,12,12,6,6)
write.csv(BLMMA9618_2,"BLMMA9618_2.csv",row.names = FALSE)
sum(BLMMA9618_2[1,])
sum(BLMMA9618_2[2,])


BLMMA9619_1<-BLMMA961(cmax9,10000,12,12,6,6)
write.csv(BLMMA9619_1,"BLMMA9619_1.csv",row.names = FALSE)
sum(BLMMA9619_1[1,])
sum(BLMMA9619_1[2,])
BLMMA9619_2<-BLMMA961(auct9,10000,12,12,6,6)
write.csv(BLMMA9619_2,"BLMMA9619_2.csv",row.names = FALSE)
sum(BLMMA9619_2[1,])
sum(BLMMA9619_2[2,])


#Proposed model (adjusted)
#Threshold=0.97  wMix=(1.0,0)
setwd("/data/Workdir_zhu/Dora")

BLMMA971<-function(pkdata,N,n1,n2,m1,m2){
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.97)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.97)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA9711_1<-BLMMA971(cmax1,10000,12,12,6,6)
write.csv(BLMMA9711_1,"BLMMA9711_1.csv",row.names = FALSE)
sum(BLMMA9711_1[1,])
sum(BLMMA9711_1[2,])


BLMMA9711_2<-BLMMA971(auct1,10000,12,12,6,6)
write.csv(BLMMA9711_2,"BLMMA9711_2.csv",row.names = FALSE)
sum(BLMMA9711_2[1,])
sum(BLMMA9711_2[2,])


BLMMA9712_1<-BLMMA971(cmax2,10000,12,12,6,6)
write.csv(BLMMA9712_1,"BLMMA9712_1.csv",row.names = FALSE)
sum(BLMMA9712_1[1,])
sum(BLMMA9712_1[2,])
BLMMA9712_2<-BLMMA971(auct2,10000,12,12,6,6)
write.csv(BLMMA9712_2,"BLMMA9712_2.csv",row.names = FALSE)
sum(BLMMA9712_2[1,])
sum(BLMMA9712_2[2,])


BLMMA9713_1<-BLMMA971(cmax3,10000,12,12,6,6)
write.csv(BLMMA9713_1,"BLMMA9713_1.csv",row.names = FALSE)
sum(BLMMA9713_1[1,])
sum(BLMMA9713_1[2,])
BLMMA9713_2<-BLMMA971(auct3,10000,12,12,6,6)
write.csv(BLMMA9713_2,"BLMMA9713_2.csv",row.names = FALSE)
sum(BLMMA9713_2[1,])
sum(BLMMA9713_2[2,])


BLMMA9714_1<-BLMMA971(cmax4,10000,12,12,6,6)
write.csv(BLMMA9714_1,"BLMMA9714_1.csv",row.names = FALSE)
sum(BLMMA9714_1[1,])
sum(BLMMA9714_1[2,])
BLMMA9714_2<-BLMMA971(auct4,10000,12,12,6,6)
write.csv(BLMMA9714_2,"BLMMA9714_2.csv",row.names = FALSE)
sum(BLMMA9714_2[1,])
sum(BLMMA9714_2[2,])


BLMMA9715_1<-BLMMA971(cmax5,10000,12,12,6,6)
write.csv(BLMMA9715_1,"BLMMA9715_1.csv",row.names = FALSE)
sum(BLMMA9715_1[1,])
sum(BLMMA9715_1[2,])
BLMMA9715_2<-BLMMA971(auct5,10000,12,12,6,6)
write.csv(BLMMA9715_2,"BLMMA9715_2.csv",row.names = FALSE)
sum(BLMMA9715_2[1,])
sum(BLMMA9715_2[2,])


BLMMA9716_1<-BLMMA971(cmax6,10000,12,12,6,6)
write.csv(BLMMA9716_1,"BLMMA9716_1.csv",row.names = FALSE)
sum(BLMMA9716_1[1,])
sum(BLMMA9716_1[2,])
BLMMA9716_2<-BLMMA971(auct6,10000,12,12,6,6)
write.csv(BLMMA9716_2,"BLMMA9716_2.csv",row.names = FALSE)
sum(BLMMA9716_2[1,])
sum(BLMMA9716_2[2,])


BLMMA9717_1<-BLMMA971(cmax7,10000,12,12,6,6)
write.csv(BLMMA9717_1,"BLMMA9717_1.csv",row.names = FALSE)
sum(BLMMA9717_1[1,])
sum(BLMMA9717_1[2,])
BLMMA9717_2<-BLMMA971(auct7,10000,12,12,6,6)
write.csv(BLMMA9717_2,"BLMMA9717_2.csv",row.names = FALSE)
sum(BLMMA9717_2[1,])
sum(BLMMA9717_2[2,])

BLMMA9718_1<-BLMMA971(cmax8,10000,12,12,6,6)
write.csv(BLMMA9718_1,"BLMMA9718_1.csv",row.names = FALSE)
sum(BLMMA9718_1[1,])
sum(BLMMA9718_1[2,])
BLMMA9718_2<-BLMMA971(auct8,10000,12,12,6,6)
write.csv(BLMMA9718_2,"BLMMA9718_2.csv",row.names = FALSE)
sum(BLMMA9718_2[1,])
sum(BLMMA9718_2[2,])


BLMMA9719_1<-BLMMA971(cmax9,10000,12,12,6,6)
write.csv(BLMMA9719_1,"BLMMA9719_1.csv",row.names = FALSE)
sum(BLMMA9719_1[1,])
sum(BLMMA9719_1[2,])
BLMMA9719_2<-BLMMA971(auct9,10000,12,12,6,6)
write.csv(BLMMA9719_2,"BLMMA9719_2.csv",row.names = FALSE)
sum(BLMMA9719_2[1,])
sum(BLMMA9719_2[2,])

