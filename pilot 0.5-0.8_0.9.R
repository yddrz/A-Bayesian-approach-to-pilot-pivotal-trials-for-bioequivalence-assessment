#Proposed model (adjusted)
#Threshold=0.5  wMix=(0.9,0.1)
setwd("/data/Workdir_zhu/Dora")

BLMMA59<-function(pkdata,N,n1,n2,m1,m2){
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.5)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.5)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA591_1<-BLMMA59(cmax1,10000,12,12,6,6)
write.csv(BLMMA591_1,"BLMMA591_1.csv",row.names = FALSE)
sum(BLMMA591_1[1,])
sum(BLMMA591_1[2,])


BLMMA591_2<-BLMMA59(auct1,10000,12,12,6,6)
write.csv(BLMMA591_2,"BLMMA591_2.csv",row.names = FALSE)
sum(BLMMA591_2[1,])
sum(BLMMA591_2[2,])


BLMMA592_1<-BLMMA59(cmax2,10000,12,12,6,6)
write.csv(BLMMA592_1,"BLMMA592_1.csv",row.names = FALSE)
sum(BLMMA592_1[1,])
sum(BLMMA592_1[2,])
BLMMA592_2<-BLMMA59(auct2,10000,12,12,6,6)
write.csv(BLMMA592_2,"BLMMA592_2.csv",row.names = FALSE)
sum(BLMMA592_2[1,])
sum(BLMMA592_2[2,])


BLMMA593_1<-BLMMA59(cmax3,10000,12,12,6,6)
write.csv(BLMMA593_1,"BLMMA593_1.csv",row.names = FALSE)
sum(BLMMA593_1[1,])
sum(BLMMA593_1[2,])
BLMMA593_2<-BLMMA59(auct3,10000,12,12,6,6)
write.csv(BLMMA593_2,"BLMMA593_2.csv",row.names = FALSE)
sum(BLMMA593_2[1,])
sum(BLMMA593_2[2,])


BLMMA594_1<-BLMMA59(cmax4,10000,12,12,6,6)
write.csv(BLMMA594_1,"BLMMA594_1.csv",row.names = FALSE)
sum(BLMMA594_1[1,])
sum(BLMMA594_1[2,])
BLMMA594_2<-BLMMA59(auct4,10000,12,12,6,6)
write.csv(BLMMA594_2,"BLMMA594_2.csv",row.names = FALSE)
sum(BLMMA594_2[1,])
sum(BLMMA594_2[2,])


BLMMA595_1<-BLMMA59(cmax5,10000,12,12,6,6)
write.csv(BLMMA595_1,"BLMMA595_1.csv",row.names = FALSE)
sum(BLMMA595_1[1,])
sum(BLMMA595_1[2,])
BLMMA595_2<-BLMMA59(auct5,10000,12,12,6,6)
write.csv(BLMMA595_2,"BLMMA595_2.csv",row.names = FALSE)
sum(BLMMA595_2[1,])
sum(BLMMA595_2[2,])


BLMMA596_1<-BLMMA59(cmax6,10000,12,12,6,6)
write.csv(BLMMA596_1,"BLMMA596_1.csv",row.names = FALSE)
sum(BLMMA596_1[1,])
sum(BLMMA596_1[2,])
BLMMA596_2<-BLMMA59(auct6,10000,12,12,6,6)
write.csv(BLMMA596_2,"BLMMA596_2.csv",row.names = FALSE)
sum(BLMMA596_2[1,])
sum(BLMMA596_2[2,])


BLMMA597_1<-BLMMA59(cmax7,10000,12,12,6,6)
write.csv(BLMMA597_1,"BLMMA597_1.csv",row.names = FALSE)
sum(BLMMA597_1[1,])
sum(BLMMA597_1[2,])
BLMMA597_2<-BLMMA59(auct7,10000,12,12,6,6)
write.csv(BLMMA597_2,"BLMMA597_2.csv",row.names = FALSE)
sum(BLMMA597_2[1,])
sum(BLMMA597_2[2,])

BLMMA598_1<-BLMMA59(cmax8,10000,12,12,6,6)
write.csv(BLMMA598_1,"BLMMA598_1.csv",row.names = FALSE)
sum(BLMMA598_1[1,])
sum(BLMMA598_1[2,])
BLMMA598_2<-BLMMA59(auct8,10000,12,12,6,6)
write.csv(BLMMA598_2,"BLMMA598_2.csv",row.names = FALSE)
sum(BLMMA598_2[1,])
sum(BLMMA598_2[2,])


BLMMA599_1<-BLMMA59(cmax9,10000,12,12,6,6)
write.csv(BLMMA599_1,"BLMMA599_1.csv",row.names = FALSE)
sum(BLMMA599_1[1,])
sum(BLMMA599_1[2,])
BLMMA599_2<-BLMMA59(auct9,10000,12,12,6,6)
write.csv(BLMMA599_2,"BLMMA599_2.csv",row.names = FALSE)
sum(BLMMA599_2[1,])
sum(BLMMA599_2[2,])

#Proposed model (adjusted)
#Threshold=0.6  wMix=(0.9,0.1)
BLMMA69<-function(pkdata,N,n1,n2,m1,m2){
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.6)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.6)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA691_1<-BLMMA69(cmax1,10000,12,12,6,6)
write.csv(BLMMA691_1,"BLMMA691_1.csv",row.names = FALSE)
sum(BLMMA691_1[1,])
sum(BLMMA691_1[2,])


BLMMA691_2<-BLMMA69(auct1,10000,12,12,6,6)
write.csv(BLMMA691_2,"BLMMA691_2.csv",row.names = FALSE)
sum(BLMMA691_2[1,])
sum(BLMMA691_2[2,])


BLMMA692_1<-BLMMA69(cmax2,10000,12,12,6,6)
write.csv(BLMMA692_1,"BLMMA692_1.csv",row.names = FALSE)
sum(BLMMA692_1[1,])
sum(BLMMA692_1[2,])
BLMMA692_2<-BLMMA69(auct2,10000,12,12,6,6)
write.csv(BLMMA692_2,"BLMMA692_2.csv",row.names = FALSE)
sum(BLMMA692_2[1,])
sum(BLMMA692_2[2,])


BLMMA693_1<-BLMMA69(cmax3,10000,12,12,6,6)
write.csv(BLMMA693_1,"BLMMA693_1.csv",row.names = FALSE)
sum(BLMMA693_1[1,])
sum(BLMMA693_1[2,])
BLMMA693_2<-BLMMA69(auct3,10000,12,12,6,6)
write.csv(BLMMA693_2,"BLMMA693_2.csv",row.names = FALSE)
sum(BLMMA693_2[1,])
sum(BLMMA693_2[2,])


BLMMA694_1<-BLMMA69(cmax4,10000,12,12,6,6)
write.csv(BLMMA694_1,"BLMMA694_1.csv",row.names = FALSE)
sum(BLMMA694_1[1,])
sum(BLMMA694_1[2,])
BLMMA694_2<-BLMMA69(auct4,10000,12,12,6,6)
write.csv(BLMMA694_2,"BLMMA694_2.csv",row.names = FALSE)
sum(BLMMA694_2[1,])
sum(BLMMA694_2[2,])


BLMMA695_1<-BLMMA69(cmax5,10000,12,12,6,6)
write.csv(BLMMA695_1,"BLMMA695_1.csv",row.names = FALSE)
sum(BLMMA695_1[1,])
sum(BLMMA695_1[2,])
BLMMA695_2<-BLMMA69(auct5,10000,12,12,6,6)
write.csv(BLMMA695_2,"BLMMA695_2.csv",row.names = FALSE)
sum(BLMMA695_2[1,])
sum(BLMMA695_2[2,])


BLMMA696_1<-BLMMA69(cmax6,10000,12,12,6,6)
write.csv(BLMMA696_1,"BLMMA696_1.csv",row.names = FALSE)
sum(BLMMA696_1[1,])
sum(BLMMA696_1[2,])
BLMMA696_2<-BLMMA69(auct6,10000,12,12,6,6)
write.csv(BLMMA696_2,"BLMMA696_2.csv",row.names = FALSE)
sum(BLMMA696_2[1,])
sum(BLMMA696_2[2,])


BLMMA697_1<-BLMMA69(cmax7,10000,12,12,6,6)
write.csv(BLMMA697_1,"BLMMA697_1.csv",row.names = FALSE)
sum(BLMMA697_1[1,])
sum(BLMMA697_1[2,])
BLMMA697_2<-BLMMA69(auct7,10000,12,12,6,6)
write.csv(BLMMA697_2,"BLMMA697_2.csv",row.names = FALSE)
sum(BLMMA697_2[1,])
sum(BLMMA697_2[2,])

BLMMA698_1<-BLMMA69(cmax8,10000,12,12,6,6)
write.csv(BLMMA698_1,"BLMMA698_1.csv",row.names = FALSE)
sum(BLMMA698_1[1,])
sum(BLMMA698_1[2,])
BLMMA698_2<-BLMMA69(auct8,10000,12,12,6,6)
write.csv(BLMMA698_2,"BLMMA698_2.csv",row.names = FALSE)
sum(BLMMA698_2[1,])
sum(BLMMA698_2[2,])


BLMMA699_1<-BLMMA69(cmax9,10000,12,12,6,6)
write.csv(BLMMA699_1,"BLMMA699_1.csv",row.names = FALSE)
sum(BLMMA699_1[1,])
sum(BLMMA699_1[2,])
BLMMA699_2<-BLMMA69(auct9,10000,12,12,6,6)
write.csv(BLMMA699_2,"BLMMA699_2.csv",row.names = FALSE)
sum(BLMMA699_2[1,])
sum(BLMMA699_2[2,])


#Proposed model (adjusted)
#Threshold=0.7  wMix=(0.9,0.1)
BLMMA79<-function(pkdata,N,n1,n2,m1,m2){
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.7)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.7)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA791_1<-BLMMA79(cmax1,10000,12,12,6,6)
write.csv(BLMMA791_1,"BLMMA791_1.csv",row.names = FALSE)
sum(BLMMA791_1[1,])
sum(BLMMA791_1[2,])


BLMMA791_2<-BLMMA79(auct1,10000,12,12,6,6)
write.csv(BLMMA791_2,"BLMMA791_2.csv",row.names = FALSE)
sum(BLMMA791_2[1,])
sum(BLMMA791_2[2,])


BLMMA792_1<-BLMMA79(cmax2,10000,12,12,6,6)
write.csv(BLMMA792_1,"BLMMA792_1.csv",row.names = FALSE)
sum(BLMMA792_1[1,])
sum(BLMMA792_1[2,])
BLMMA792_2<-BLMMA79(auct2,10000,12,12,6,6)
write.csv(BLMMA792_2,"BLMMA792_2.csv",row.names = FALSE)
sum(BLMMA792_2[1,])
sum(BLMMA792_2[2,])


BLMMA793_1<-BLMMA79(cmax3,10000,12,12,6,6)
write.csv(BLMMA793_1,"BLMMA793_1.csv",row.names = FALSE)
sum(BLMMA793_1[1,])
sum(BLMMA793_1[2,])
BLMMA793_2<-BLMMA79(auct3,10000,12,12,6,6)
write.csv(BLMMA793_2,"BLMMA793_2.csv",row.names = FALSE)
sum(BLMMA793_2[1,])
sum(BLMMA793_2[2,])


BLMMA794_1<-BLMMA79(cmax4,10000,12,12,6,6)
write.csv(BLMMA794_1,"BLMMA794_1.csv",row.names = FALSE)
sum(BLMMA794_1[1,])
sum(BLMMA794_1[2,])
BLMMA794_2<-BLMMA79(auct4,10000,12,12,6,6)
write.csv(BLMMA794_2,"BLMMA794_2.csv",row.names = FALSE)
sum(BLMMA794_2[1,])
sum(BLMMA794_2[2,])


BLMMA795_1<-BLMMA79(cmax5,10000,12,12,6,6)
write.csv(BLMMA795_1,"BLMMA795_1.csv",row.names = FALSE)
sum(BLMMA795_1[1,])
sum(BLMMA795_1[2,])
BLMMA795_2<-BLMMA79(auct5,10000,12,12,6,6)
write.csv(BLMMA795_2,"BLMMA795_2.csv",row.names = FALSE)
sum(BLMMA795_2[1,])
sum(BLMMA795_2[2,])


BLMMA796_1<-BLMMA79(cmax6,10000,12,12,6,6)
write.csv(BLMMA796_1,"BLMMA796_1.csv",row.names = FALSE)
sum(BLMMA796_1[1,])
sum(BLMMA796_1[2,])
BLMMA796_2<-BLMMA79(auct6,10000,12,12,6,6)
write.csv(BLMMA796_2,"BLMMA796_2.csv",row.names = FALSE)
sum(BLMMA796_2[1,])
sum(BLMMA796_2[2,])


BLMMA797_1<-BLMMA79(cmax7,10000,12,12,6,6)
write.csv(BLMMA797_1,"BLMMA797_1.csv",row.names = FALSE)
sum(BLMMA797_1[1,])
sum(BLMMA797_1[2,])
BLMMA797_2<-BLMMA79(auct7,10000,12,12,6,6)
write.csv(BLMMA797_2,"BLMMA797_2.csv",row.names = FALSE)
sum(BLMMA797_2[1,])
sum(BLMMA797_2[2,])

BLMMA798_1<-BLMMA79(cmax8,10000,12,12,6,6)
write.csv(BLMMA798_1,"BLMMA798_1.csv",row.names = FALSE)
sum(BLMMA798_1[1,])
sum(BLMMA798_1[2,])
BLMMA798_2<-BLMMA79(auct8,10000,12,12,6,6)
write.csv(BLMMA798_2,"BLMMA798_2.csv",row.names = FALSE)
sum(BLMMA798_2[1,])
sum(BLMMA798_2[2,])


BLMMA799_1<-BLMMA79(cmax9,10000,12,12,6,6)
write.csv(BLMMA799_1,"BLMMA799_1.csv",row.names = FALSE)
sum(BLMMA799_1[1,])
sum(BLMMA799_1[2,])
BLMMA799_2<-BLMMA79(auct9,10000,12,12,6,6)
write.csv(BLMMA799_2,"BLMMA799_2.csv",row.names = FALSE)
sum(BLMMA799_2[1,])
sum(BLMMA799_2[2,])



#Proposed model (adjusted)
#Threshold=0.8  wMix=(0.9,0.1)
BLMMA89<-function(pkdata,N,n1,n2,m1,m2){
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
    
    if (MyMod.fit$mean$pred.prob.be1>0.8)
      Y[1,i]=1 else 
        Y[1,i]=0
    
    
    if (MyMod.fit$mean$pred.prob.be>0.8)
      Y[2,i]=1 else 
        Y[2,i]=0
  }
  Y
}
BLMMA891_1<-BLMMA89(cmax1,10000,12,12,6,6)
write.csv(BLMMA891_1,"BLMMA891_1.csv",row.names = FALSE)
sum(BLMMA891_1[1,])
sum(BLMMA891_1[2,])


BLMMA891_2<-BLMMA89(auct1,10000,12,12,6,6)
write.csv(BLMMA891_2,"BLMMA891_2.csv",row.names = FALSE)
sum(BLMMA891_2[1,])
sum(BLMMA891_2[2,])


BLMMA892_1<-BLMMA89(cmax2,10000,12,12,6,6)
write.csv(BLMMA892_1,"BLMMA892_1.csv",row.names = FALSE)
sum(BLMMA892_1[1,])
sum(BLMMA892_1[2,])
BLMMA892_2<-BLMMA89(auct2,10000,12,12,6,6)
write.csv(BLMMA892_2,"BLMMA892_2.csv",row.names = FALSE)
sum(BLMMA892_2[1,])
sum(BLMMA892_2[2,])


BLMMA893_1<-BLMMA89(cmax3,10000,12,12,6,6)
write.csv(BLMMA893_1,"BLMMA893_1.csv",row.names = FALSE)
sum(BLMMA893_1[1,])
sum(BLMMA893_1[2,])
BLMMA893_2<-BLMMA89(auct3,10000,12,12,6,6)
write.csv(BLMMA893_2,"BLMMA893_2.csv",row.names = FALSE)
sum(BLMMA893_2[1,])
sum(BLMMA893_2[2,])


BLMMA894_1<-BLMMA89(cmax4,10000,12,12,6,6)
write.csv(BLMMA894_1,"BLMMA894_1.csv",row.names = FALSE)
sum(BLMMA894_1[1,])
sum(BLMMA894_1[2,])
BLMMA894_2<-BLMMA89(auct4,10000,12,12,6,6)
write.csv(BLMMA894_2,"BLMMA894_2.csv",row.names = FALSE)
sum(BLMMA894_2[1,])
sum(BLMMA894_2[2,])


BLMMA895_1<-BLMMA89(cmax5,10000,12,12,6,6)
write.csv(BLMMA895_1,"BLMMA895_1.csv",row.names = FALSE)
sum(BLMMA895_1[1,])
sum(BLMMA895_1[2,])
BLMMA895_2<-BLMMA89(auct5,10000,12,12,6,6)
write.csv(BLMMA895_2,"BLMMA895_2.csv",row.names = FALSE)
sum(BLMMA895_2[1,])
sum(BLMMA895_2[2,])


BLMMA896_1<-BLMMA89(cmax6,10000,12,12,6,6)
write.csv(BLMMA896_1,"BLMMA896_1.csv",row.names = FALSE)
sum(BLMMA896_1[1,])
sum(BLMMA896_1[2,])
BLMMA896_2<-BLMMA89(auct6,10000,12,12,6,6)
write.csv(BLMMA896_2,"BLMMA896_2.csv",row.names = FALSE)
sum(BLMMA896_2[1,])
sum(BLMMA896_2[2,])


BLMMA897_1<-BLMMA89(cmax7,10000,12,12,6,6)
write.csv(BLMMA897_1,"BLMMA897_1.csv",row.names = FALSE)
sum(BLMMA897_1[1,])
sum(BLMMA897_1[2,])
BLMMA897_2<-BLMMA89(auct7,10000,12,12,6,6)
write.csv(BLMMA897_2,"BLMMA897_2.csv",row.names = FALSE)
sum(BLMMA897_2[1,])
sum(BLMMA897_2[2,])

BLMMA898_1<-BLMMA89(cmax8,10000,12,12,6,6)
write.csv(BLMMA898_1,"BLMMA898_1.csv",row.names = FALSE)
sum(BLMMA898_1[1,])
sum(BLMMA898_1[2,])
BLMMA898_2<-BLMMA89(auct8,10000,12,12,6,6)
write.csv(BLMMA898_2,"BLMMA898_2.csv",row.names = FALSE)
sum(BLMMA898_2[1,])
sum(BLMMA898_2[2,])


BLMMA899_1<-BLMMA89(cmax9,10000,12,12,6,6)
write.csv(BLMMA899_1,"BLMMA899_1.csv",row.names = FALSE)
sum(BLMMA899_1[1,])
sum(BLMMA899_1[2,])
BLMMA899_2<-BLMMA89(auct9,10000,12,12,6,6)
write.csv(BLMMA899_2,"BLMMA899_2.csv",row.names = FALSE)
sum(BLMMA899_2[1,])
sum(BLMMA899_2[2,])
