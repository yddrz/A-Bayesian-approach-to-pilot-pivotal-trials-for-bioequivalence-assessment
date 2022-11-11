#Proposed model (adjusted)
#Threshold=0.5  wMix=(1.0,0)
setwd("/data/Workdir_zhu/Dora")

BLMMA51<-function(pkdata,N,n1,n2,m1,m2){
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.5)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.5)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA511_1<-BLMMA51(cmax1,10000,12,12,6,6)
write.csv(BLMMA511_1,"BLMMA511_1.csv",row.names = FALSE)
sum(BLMMA511_1[1,])
sum(BLMMA511_1[2,])


BLMMA511_2<-BLMMA51(auct1,10000,12,12,6,6)
write.csv(BLMMA511_2,"BLMMA511_2.csv",row.names = FALSE)
sum(BLMMA511_2[1,])
sum(BLMMA511_2[2,])


BLMMA512_1<-BLMMA51(cmax2,10000,12,12,6,6)
write.csv(BLMMA512_1,"BLMMA512_1.csv",row.names = FALSE)
sum(BLMMA512_1[1,])
sum(BLMMA512_1[2,])
BLMMA512_2<-BLMMA51(auct2,10000,12,12,6,6)
write.csv(BLMMA512_2,"BLMMA512_2.csv",row.names = FALSE)
sum(BLMMA512_2[1,])
sum(BLMMA512_2[2,])


BLMMA513_1<-BLMMA51(cmax3,10000,12,12,6,6)
write.csv(BLMMA513_1,"BLMMA513_1.csv",row.names = FALSE)
sum(BLMMA513_1[1,])
sum(BLMMA513_1[2,])
BLMMA513_2<-BLMMA51(auct3,10000,12,12,6,6)
write.csv(BLMMA513_2,"BLMMA513_2.csv",row.names = FALSE)
sum(BLMMA513_2[1,])
sum(BLMMA513_2[2,])


BLMMA514_1<-BLMMA51(cmax4,10000,12,12,6,6)
write.csv(BLMMA514_1,"BLMMA514_1.csv",row.names = FALSE)
sum(BLMMA514_1[1,])
sum(BLMMA514_1[2,])
BLMMA514_2<-BLMMA51(auct4,10000,12,12,6,6)
write.csv(BLMMA514_2,"BLMMA514_2.csv",row.names = FALSE)
sum(BLMMA514_2[1,])
sum(BLMMA514_2[2,])


BLMMA515_1<-BLMMA51(cmax5,10000,12,12,6,6)
write.csv(BLMMA515_1,"BLMMA515_1.csv",row.names = FALSE)
sum(BLMMA515_1[1,])
sum(BLMMA515_1[2,])
BLMMA515_2<-BLMMA51(auct5,10000,12,12,6,6)
write.csv(BLMMA515_2,"BLMMA515_2.csv",row.names = FALSE)
sum(BLMMA515_2[1,])
sum(BLMMA515_2[2,])


BLMMA516_1<-BLMMA51(cmax6,10000,12,12,6,6)
write.csv(BLMMA516_1,"BLMMA516_1.csv",row.names = FALSE)
sum(BLMMA516_1[1,])
sum(BLMMA516_1[2,])
BLMMA516_2<-BLMMA51(auct6,10000,12,12,6,6)
write.csv(BLMMA516_2,"BLMMA516_2.csv",row.names = FALSE)
sum(BLMMA516_2[1,])
sum(BLMMA516_2[2,])


BLMMA517_1<-BLMMA51(cmax7,10000,12,12,6,6)
write.csv(BLMMA517_1,"BLMMA517_1.csv",row.names = FALSE)
sum(BLMMA517_1[1,])
sum(BLMMA517_1[2,])
BLMMA517_2<-BLMMA51(auct7,10000,12,12,6,6)
write.csv(BLMMA517_2,"BLMMA517_2.csv",row.names = FALSE)
sum(BLMMA517_2[1,])
sum(BLMMA517_2[2,])

BLMMA518_1<-BLMMA51(cmax8,10000,12,12,6,6)
write.csv(BLMMA518_1,"BLMMA518_1.csv",row.names = FALSE)
sum(BLMMA518_1[1,])
sum(BLMMA518_1[2,])
BLMMA518_2<-BLMMA51(auct8,10000,12,12,6,6)
write.csv(BLMMA518_2,"BLMMA518_2.csv",row.names = FALSE)
sum(BLMMA518_2[1,])
sum(BLMMA518_2[2,])


BLMMA519_1<-BLMMA51(cmax9,10000,12,12,6,6)
write.csv(BLMMA519_1,"BLMMA519_1.csv",row.names = FALSE)
sum(BLMMA519_1[1,])
sum(BLMMA519_1[2,])
BLMMA519_2<-BLMMA51(auct9,10000,12,12,6,6)
write.csv(BLMMA519_2,"BLMMA519_2.csv",row.names = FALSE)
sum(BLMMA519_2[1,])
sum(BLMMA519_2[2,])

#Proposed model (adjusted)
#Threshold=0.6  wMix=(1.0,0)
BLMMA61<-function(pkdata,N,n1,n2,m1,m2){
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.6)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.6)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA611_1<-BLMMA61(cmax1,10000,12,12,6,6)
write.csv(BLMMA611_1,"BLMMA611_1.csv",row.names = FALSE)
sum(BLMMA611_1[1,])
sum(BLMMA611_1[2,])


BLMMA611_2<-BLMMA61(auct1,10000,12,12,6,6)
write.csv(BLMMA611_2,"BLMMA611_2.csv",row.names = FALSE)
sum(BLMMA611_2[1,])
sum(BLMMA611_2[2,])


BLMMA612_1<-BLMMA61(cmax2,10000,12,12,6,6)
write.csv(BLMMA612_1,"BLMMA612_1.csv",row.names = FALSE)
sum(BLMMA612_1[1,])
sum(BLMMA612_1[2,])
BLMMA612_2<-BLMMA61(auct2,10000,12,12,6,6)
write.csv(BLMMA612_2,"BLMMA612_2.csv",row.names = FALSE)
sum(BLMMA612_2[1,])
sum(BLMMA612_2[2,])


BLMMA613_1<-BLMMA61(cmax3,10000,12,12,6,6)
write.csv(BLMMA613_1,"BLMMA613_1.csv",row.names = FALSE)
sum(BLMMA613_1[1,])
sum(BLMMA613_1[2,])
BLMMA613_2<-BLMMA61(auct3,10000,12,12,6,6)
write.csv(BLMMA613_2,"BLMMA613_2.csv",row.names = FALSE)
sum(BLMMA613_2[1,])
sum(BLMMA613_2[2,])


BLMMA614_1<-BLMMA61(cmax4,10000,12,12,6,6)
write.csv(BLMMA614_1,"BLMMA614_1.csv",row.names = FALSE)
sum(BLMMA614_1[1,])
sum(BLMMA614_1[2,])
BLMMA614_2<-BLMMA61(auct4,10000,12,12,6,6)
write.csv(BLMMA614_2,"BLMMA614_2.csv",row.names = FALSE)
sum(BLMMA614_2[1,])
sum(BLMMA614_2[2,])


BLMMA615_1<-BLMMA61(cmax5,10000,12,12,6,6)
write.csv(BLMMA615_1,"BLMMA615_1.csv",row.names = FALSE)
sum(BLMMA615_1[1,])
sum(BLMMA615_1[2,])
BLMMA615_2<-BLMMA61(auct5,10000,12,12,6,6)
write.csv(BLMMA615_2,"BLMMA615_2.csv",row.names = FALSE)
sum(BLMMA615_2[1,])
sum(BLMMA615_2[2,])


BLMMA616_1<-BLMMA61(cmax6,10000,12,12,6,6)
write.csv(BLMMA616_1,"BLMMA616_1.csv",row.names = FALSE)
sum(BLMMA616_1[1,])
sum(BLMMA616_1[2,])
BLMMA616_2<-BLMMA61(auct6,10000,12,12,6,6)
write.csv(BLMMA616_2,"BLMMA616_2.csv",row.names = FALSE)
sum(BLMMA616_2[1,])
sum(BLMMA616_2[2,])


BLMMA617_1<-BLMMA61(cmax7,10000,12,12,6,6)
write.csv(BLMMA617_1,"BLMMA617_1.csv",row.names = FALSE)
sum(BLMMA617_1[1,])
sum(BLMMA617_1[2,])
BLMMA617_2<-BLMMA61(auct7,10000,12,12,6,6)
write.csv(BLMMA617_2,"BLMMA617_2.csv",row.names = FALSE)
sum(BLMMA617_2[1,])
sum(BLMMA617_2[2,])

BLMMA618_1<-BLMMA61(cmax8,10000,12,12,6,6)
write.csv(BLMMA618_1,"BLMMA618_1.csv",row.names = FALSE)
sum(BLMMA618_1[1,])
sum(BLMMA618_1[2,])
BLMMA618_2<-BLMMA61(auct8,10000,12,12,6,6)
write.csv(BLMMA618_2,"BLMMA618_2.csv",row.names = FALSE)
sum(BLMMA618_2[1,])
sum(BLMMA618_2[2,])


BLMMA619_1<-BLMMA61(cmax9,10000,12,12,6,6)
write.csv(BLMMA619_1,"BLMMA619_1.csv",row.names = FALSE)
sum(BLMMA619_1[1,])
sum(BLMMA619_1[2,])
BLMMA619_2<-BLMMA61(auct9,10000,12,12,6,6)
write.csv(BLMMA619_2,"BLMMA619_2.csv",row.names = FALSE)
sum(BLMMA619_2[1,])
sum(BLMMA619_2[2,])


#Proposed model (adjusted)
#Threshold=0.7  wMix=(1.0,0)
BLMMA71<-function(pkdata,N,n1,n2,m1,m2){
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.7)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.7)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA711_1<-BLMMA71(cmax1,10000,12,12,6,6)
write.csv(BLMMA711_1,"BLMMA711_1.csv",row.names = FALSE)
sum(BLMMA711_1[1,])
sum(BLMMA711_1[2,])


BLMMA711_2<-BLMMA71(auct1,10000,12,12,6,6)
write.csv(BLMMA711_2,"BLMMA711_2.csv",row.names = FALSE)
sum(BLMMA711_2[1,])
sum(BLMMA711_2[2,])


BLMMA712_1<-BLMMA71(cmax2,10000,12,12,6,6)
write.csv(BLMMA712_1,"BLMMA712_1.csv",row.names = FALSE)
sum(BLMMA712_1[1,])
sum(BLMMA712_1[2,])
BLMMA712_2<-BLMMA71(auct2,10000,12,12,6,6)
write.csv(BLMMA712_2,"BLMMA712_2.csv",row.names = FALSE)
sum(BLMMA712_2[1,])
sum(BLMMA712_2[2,])


BLMMA713_1<-BLMMA71(cmax3,10000,12,12,6,6)
write.csv(BLMMA713_1,"BLMMA713_1.csv",row.names = FALSE)
sum(BLMMA713_1[1,])
sum(BLMMA713_1[2,])
BLMMA713_2<-BLMMA71(auct3,10000,12,12,6,6)
write.csv(BLMMA713_2,"BLMMA713_2.csv",row.names = FALSE)
sum(BLMMA713_2[1,])
sum(BLMMA713_2[2,])


BLMMA714_1<-BLMMA71(cmax4,10000,12,12,6,6)
write.csv(BLMMA714_1,"BLMMA714_1.csv",row.names = FALSE)
sum(BLMMA714_1[1,])
sum(BLMMA714_1[2,])
BLMMA714_2<-BLMMA71(auct4,10000,12,12,6,6)
write.csv(BLMMA714_2,"BLMMA714_2.csv",row.names = FALSE)
sum(BLMMA714_2[1,])
sum(BLMMA714_2[2,])


BLMMA715_1<-BLMMA71(cmax5,10000,12,12,6,6)
write.csv(BLMMA715_1,"BLMMA715_1.csv",row.names = FALSE)
sum(BLMMA715_1[1,])
sum(BLMMA715_1[2,])
BLMMA715_2<-BLMMA71(auct5,10000,12,12,6,6)
write.csv(BLMMA715_2,"BLMMA715_2.csv",row.names = FALSE)
sum(BLMMA715_2[1,])
sum(BLMMA715_2[2,])


BLMMA716_1<-BLMMA71(cmax6,10000,12,12,6,6)
write.csv(BLMMA716_1,"BLMMA716_1.csv",row.names = FALSE)
sum(BLMMA716_1[1,])
sum(BLMMA716_1[2,])
BLMMA716_2<-BLMMA71(auct6,10000,12,12,6,6)
write.csv(BLMMA716_2,"BLMMA716_2.csv",row.names = FALSE)
sum(BLMMA716_2[1,])
sum(BLMMA716_2[2,])


BLMMA717_1<-BLMMA71(cmax7,10000,12,12,6,6)
write.csv(BLMMA717_1,"BLMMA717_1.csv",row.names = FALSE)
sum(BLMMA717_1[1,])
sum(BLMMA717_1[2,])
BLMMA717_2<-BLMMA71(auct7,10000,12,12,6,6)
write.csv(BLMMA717_2,"BLMMA717_2.csv",row.names = FALSE)
sum(BLMMA717_2[1,])
sum(BLMMA717_2[2,])

BLMMA718_1<-BLMMA71(cmax8,10000,12,12,6,6)
write.csv(BLMMA718_1,"BLMMA718_1.csv",row.names = FALSE)
sum(BLMMA718_1[1,])
sum(BLMMA718_1[2,])
BLMMA718_2<-BLMMA71(auct8,10000,12,12,6,6)
write.csv(BLMMA718_2,"BLMMA718_2.csv",row.names = FALSE)
sum(BLMMA718_2[1,])
sum(BLMMA718_2[2,])


BLMMA719_1<-BLMMA71(cmax9,10000,12,12,6,6)
write.csv(BLMMA719_1,"BLMMA719_1.csv",row.names = FALSE)
sum(BLMMA719_1[1,])
sum(BLMMA719_1[2,])
BLMMA719_2<-BLMMA71(auct9,10000,12,12,6,6)
write.csv(BLMMA719_2,"BLMMA719_2.csv",row.names = FALSE)
sum(BLMMA719_2[1,])
sum(BLMMA719_2[2,])



#Proposed model (adjusted)
#Threshold=0.8  wMix=(1.0,0)
BLMMA81<-function(pkdata,N,n1,n2,m1,m2){
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.8)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.8)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA811_1<-BLMMA81(cmax1,10000,12,12,6,6)
write.csv(BLMMA811_1,"BLMMA811_1.csv",row.names = FALSE)
sum(BLMMA811_1[1,])
sum(BLMMA811_1[2,])


BLMMA811_2<-BLMMA81(auct1,10000,12,12,6,6)
write.csv(BLMMA811_2,"BLMMA811_2.csv",row.names = FALSE)
sum(BLMMA811_2[1,])
sum(BLMMA811_2[2,])


BLMMA812_1<-BLMMA81(cmax2,10000,12,12,6,6)
write.csv(BLMMA812_1,"BLMMA812_1.csv",row.names = FALSE)
sum(BLMMA812_1[1,])
sum(BLMMA812_1[2,])
BLMMA812_2<-BLMMA81(auct2,10000,12,12,6,6)
write.csv(BLMMA812_2,"BLMMA812_2.csv",row.names = FALSE)
sum(BLMMA812_2[1,])
sum(BLMMA812_2[2,])


BLMMA813_1<-BLMMA81(cmax3,10000,12,12,6,6)
write.csv(BLMMA813_1,"BLMMA813_1.csv",row.names = FALSE)
sum(BLMMA813_1[1,])
sum(BLMMA813_1[2,])
BLMMA813_2<-BLMMA81(auct3,10000,12,12,6,6)
write.csv(BLMMA813_2,"BLMMA813_2.csv",row.names = FALSE)
sum(BLMMA813_2[1,])
sum(BLMMA813_2[2,])


BLMMA814_1<-BLMMA81(cmax4,10000,12,12,6,6)
write.csv(BLMMA814_1,"BLMMA814_1.csv",row.names = FALSE)
sum(BLMMA814_1[1,])
sum(BLMMA814_1[2,])
BLMMA814_2<-BLMMA81(auct4,10000,12,12,6,6)
write.csv(BLMMA814_2,"BLMMA814_2.csv",row.names = FALSE)
sum(BLMMA814_2[1,])
sum(BLMMA814_2[2,])


BLMMA815_1<-BLMMA81(cmax5,10000,12,12,6,6)
write.csv(BLMMA815_1,"BLMMA815_1.csv",row.names = FALSE)
sum(BLMMA815_1[1,])
sum(BLMMA815_1[2,])
BLMMA815_2<-BLMMA81(auct5,10000,12,12,6,6)
write.csv(BLMMA815_2,"BLMMA815_2.csv",row.names = FALSE)
sum(BLMMA815_2[1,])
sum(BLMMA815_2[2,])


BLMMA816_1<-BLMMA81(cmax6,10000,12,12,6,6)
write.csv(BLMMA816_1,"BLMMA816_1.csv",row.names = FALSE)
sum(BLMMA816_1[1,])
sum(BLMMA816_1[2,])
BLMMA816_2<-BLMMA81(auct6,10000,12,12,6,6)
write.csv(BLMMA816_2,"BLMMA816_2.csv",row.names = FALSE)
sum(BLMMA816_2[1,])
sum(BLMMA816_2[2,])


BLMMA817_1<-BLMMA81(cmax7,10000,12,12,6,6)
write.csv(BLMMA817_1,"BLMMA817_1.csv",row.names = FALSE)
sum(BLMMA817_1[1,])
sum(BLMMA817_1[2,])
BLMMA817_2<-BLMMA81(auct7,10000,12,12,6,6)
write.csv(BLMMA817_2,"BLMMA817_2.csv",row.names = FALSE)
sum(BLMMA817_2[1,])
sum(BLMMA817_2[2,])

BLMMA818_1<-BLMMA81(cmax8,10000,12,12,6,6)
write.csv(BLMMA818_1,"BLMMA818_1.csv",row.names = FALSE)
sum(BLMMA818_1[1,])
sum(BLMMA818_1[2,])
BLMMA818_2<-BLMMA81(auct8,10000,12,12,6,6)
write.csv(BLMMA818_2,"BLMMA818_2.csv",row.names = FALSE)
sum(BLMMA818_2[1,])
sum(BLMMA818_2[2,])


BLMMA819_1<-BLMMA81(cmax9,10000,12,12,6,6)
write.csv(BLMMA819_1,"BLMMA819_1.csv",row.names = FALSE)
sum(BLMMA819_1[1,])
sum(BLMMA819_1[2,])
BLMMA819_2<-BLMMA81(auct9,10000,12,12,6,6)
write.csv(BLMMA819_2,"BLMMA819_2.csv",row.names = FALSE)
sum(BLMMA819_2[1,])
sum(BLMMA819_2[2,])

#Proposed model (adjusted)
#Threshold=0.9  wMix=(1.0,0)
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.9)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.9)
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

