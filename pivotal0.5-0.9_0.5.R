
#Threshold=0.5 wMix=(0.5, 0.5)
BLMM_M055<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
  Y<-rep(99,10000)
  
  for (i in 1:N){
    data  <- list(Nobs.p     = 2*(n1+n2),
                  Npts.p     = n1+n2,
                  pts.p      = pkdata[[i]]$Subject,
                  trt.p      = pkdata[[i]]$form,
                  prd.p      = pkdata[[i]]$period,
                  yp        = pkdata[[i]]$distance,
                  # ,
                  Nobs.m     = 2*(n1_m+n2_m),
                  Npts.m     = n1_m+n2_m,
                  pts.m      = pkmdata[[i]]$Subject,
                  trt.m      = pkmdata[[i]]$form,
                  prd.m      = pkmdata[[i]]$period,
                  ym        = pkmdata[[i]]$distance,
                  # ym = rep(0, 24),
                  prior.wt = c(0, 10),
                  prior.mt = c(0, 3),
                  Prior.tau.HN = 0.25,
                  wMix     = c(0.5, 0.5),
                  thres    = c(0.8, 1.25)
    )
    
    
    MyMod.fit <- bugs(data = data,
                      inits = inits,
                      model.file = "MyMod1.txt",
                      parameters = c("theta.m", "prob.ex", "pred.prob.be"),
                      n.chains = 1,
                      n.iter = 11000,
                      n.burnin = 1000,
                      bugs.seed = 1
    )
    
    
    if (MyMod.fit$summary[4]>=0.5)
      Y[[i]]=1 else 
        Y[[i]]=0
    
  }
  Y
}
#sc5+sc3
BLMM_M055_31<-BLMM_M055(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M055_31,"BLMM_M055_31.csv",row.names = FALSE)
sum(BLMM_M055_31)
BLMM_M055_32<-BLMM_M055(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M055_32,"BLMM_M055_32.csv",row.names = FALSE)
sum(BLMM_M055_32)

#sc5+sc4
BLMM_M055_41<-BLMM_M055(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M055_41,"BLMM_M055_41.csv",row.names = FALSE)
sum(BLMM_M055_41)
BLMM_M055_42<-BLMM_M055(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M055_42,"BLMM_M055_42.csv",row.names = FALSE)
sum(BLMM_M055_42)

#sc5+sc5
BLMM_M055_51<-BLMM_M055(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M055_51,"BLMM_M055_51.csv",row.names = FALSE)
sum(BLMM_M055_51)
BLMM_M055_52<-BLMM_M055(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M055_52,"BLMM_M055_52.csv",row.names = FALSE)
sum(BLMM_M055_52)

#sc5+sc6
BLMM_M055_61<-BLMM_M055(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M055_61,"BLMM_M055_61.csv",row.names = FALSE)
sum(BLMM_M055_61)
BLMM_M055_62<-BLMM_M055(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M055_62,"BLMM_M055_62.csv",row.names = FALSE)
sum(BLMM_M055_62)

#sc5+sc7
BLMM_M055_71<-BLMM_M055(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M055_71,"BLMM_M055_71.csv",row.names = FALSE)
sum(BLMM_M055_71)
BLMM_M055_72<-BLMM_M055(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M055_72,"BLMM_M055_72.csv",row.names = FALSE)
sum(BLMM_M055_72)

#Threshold=0.6 wMix=(0.5, 0.5)
BLMM_M065<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
  Y<-rep(99,10000)
  
  for (i in 1:N){
    data  <- list(Nobs.p     = 2*(n1+n2),
                  Npts.p     = n1+n2,
                  pts.p      = pkdata[[i]]$Subject,
                  trt.p      = pkdata[[i]]$form,
                  prd.p      = pkdata[[i]]$period,
                  yp        = pkdata[[i]]$distance,
                  # ,
                  Nobs.m     = 2*(n1_m+n2_m),
                  Npts.m     = n1_m+n2_m,
                  pts.m      = pkmdata[[i]]$Subject,
                  trt.m      = pkmdata[[i]]$form,
                  prd.m      = pkmdata[[i]]$period,
                  ym        = pkmdata[[i]]$distance,
                  # ym = rep(0, 24),
                  prior.wt = c(0, 10),
                  prior.mt = c(0, 3),
                  Prior.tau.HN = 0.25,
                  wMix     = c(0.5, 0.5),
                  thres    = c(0.8, 1.25)
    )
    
    
    MyMod.fit <- bugs(data = data,
                      inits = inits,
                      model.file = "MyMod1.txt",
                      parameters = c("theta.m", "prob.ex", "pred.prob.be"),
                      n.chains = 1,
                      n.iter = 11000,
                      n.burnin = 1000,
                      bugs.seed = 1
    )
    
    
    if (MyMod.fit$summary[4]>=0.6)
      Y[[i]]=1 else 
        Y[[i]]=0
    
  }
  Y
}
#sc5+sc3
BLMM_M065_31<-BLMM_M065(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M065_31,"BLMM_M065_31.csv",row.names = FALSE)
sum(BLMM_M065_31)
BLMM_M065_32<-BLMM_M065(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M065_32,"BLMM_M065_32.csv",row.names = FALSE)
sum(BLMM_M065_32)

#sc5+sc4
BLMM_M065_41<-BLMM_M065(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M065_41,"BLMM_M065_41.csv",row.names = FALSE)
sum(BLMM_M065_41)
BLMM_M065_42<-BLMM_M065(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M065_42,"BLMM_M065_42.csv",row.names = FALSE)
sum(BLMM_M065_42)

#sc5+sc5
BLMM_M065_51<-BLMM_M065(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M065_51,"BLMM_M065_51.csv",row.names = FALSE)
sum(BLMM_M065_51)
BLMM_M065_52<-BLMM_M065(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M065_52,"BLMM_M065_52.csv",row.names = FALSE)
sum(BLMM_M065_52)

#sc5+sc6
BLMM_M065_61<-BLMM_M065(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M065_61,"BLMM_M065_61.csv",row.names = FALSE)
sum(BLMM_M065_61)
BLMM_M065_62<-BLMM_M065(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M065_62,"BLMM_M065_62.csv",row.names = FALSE)
sum(BLMM_M065_62)

#sc5+sc7
BLMM_M065_71<-BLMM_M065(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M065_71,"BLMM_M065_71.csv",row.names = FALSE)
sum(BLMM_M065_71)
BLMM_M065_72<-BLMM_M065(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M065_72,"BLMM_M065_72.csv",row.names = FALSE)
sum(BLMM_M065_72)


#Threshold=0.7 wMix=(0.5, 0.5)
BLMM_M075<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
  Y<-rep(99,10000)
  
  for (i in 1:N){
    data  <- list(Nobs.p     = 2*(n1+n2),
                  Npts.p     = n1+n2,
                  pts.p      = pkdata[[i]]$Subject,
                  trt.p      = pkdata[[i]]$form,
                  prd.p      = pkdata[[i]]$period,
                  yp        = pkdata[[i]]$distance,
                  # ,
                  Nobs.m     = 2*(n1_m+n2_m),
                  Npts.m     = n1_m+n2_m,
                  pts.m      = pkmdata[[i]]$Subject,
                  trt.m      = pkmdata[[i]]$form,
                  prd.m      = pkmdata[[i]]$period,
                  ym        = pkmdata[[i]]$distance,
                  # ym = rep(0, 24),
                  prior.wt = c(0, 10),
                  prior.mt = c(0, 3),
                  Prior.tau.HN = 0.25,
                  wMix     = c(0.5, 0.5),
                  thres    = c(0.8, 1.25)
    )
    
    
    MyMod.fit <- bugs(data = data,
                      inits = inits,
                      model.file = "MyMod1.txt",
                      parameters = c("theta.m", "prob.ex", "pred.prob.be"),
                      n.chains = 1,
                      n.iter = 11000,
                      n.burnin = 1000,
                      bugs.seed = 1
    )
    
    
    if (MyMod.fit$summary[4]>=0.7)
      Y[[i]]=1 else 
        Y[[i]]=0
    
  }
  Y
}
#sc5+sc3
BLMM_M075_31<-BLMM_M075(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M075_31,"BLMM_M075_31.csv",row.names = FALSE)
sum(BLMM_M075_31)
BLMM_M075_32<-BLMM_M075(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M075_32,"BLMM_M075_32.csv",row.names = FALSE)
sum(BLMM_M075_32)

#sc5+sc4
BLMM_M075_41<-BLMM_M075(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M075_41,"BLMM_M075_41.csv",row.names = FALSE)
sum(BLMM_M075_41)
BLMM_M075_42<-BLMM_M075(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M075_42,"BLMM_M075_42.csv",row.names = FALSE)
sum(BLMM_M075_42)

#sc5+sc5
BLMM_M075_51<-BLMM_M075(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M075_51,"BLMM_M075_51.csv",row.names = FALSE)
sum(BLMM_M075_51)
BLMM_M075_52<-BLMM_M075(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M075_52,"BLMM_M075_52.csv",row.names = FALSE)
sum(BLMM_M075_52)

#sc5+sc6
BLMM_M075_61<-BLMM_M075(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M075_61,"BLMM_M075_61.csv",row.names = FALSE)
sum(BLMM_M075_61)
BLMM_M075_62<-BLMM_M075(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M075_62,"BLMM_M075_62.csv",row.names = FALSE)
sum(BLMM_M075_62)

#sc5+sc7
BLMM_M075_71<-BLMM_M075(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M075_71,"BLMM_M075_71.csv",row.names = FALSE)
sum(BLMM_M075_71)
BLMM_M075_72<-BLMM_M075(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M075_72,"BLMM_M075_72.csv",row.names = FALSE)
sum(BLMM_M075_72)



#Threshold=0.8 wMix=(0.5, 0.5)
BLMM_M085<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
  Y<-rep(99,10000)
  
  for (i in 1:N){
    data  <- list(Nobs.p     = 2*(n1+n2),
                  Npts.p     = n1+n2,
                  pts.p      = pkdata[[i]]$Subject,
                  trt.p      = pkdata[[i]]$form,
                  prd.p      = pkdata[[i]]$period,
                  yp        = pkdata[[i]]$distance,
                  # ,
                  Nobs.m     = 2*(n1_m+n2_m),
                  Npts.m     = n1_m+n2_m,
                  pts.m      = pkmdata[[i]]$Subject,
                  trt.m      = pkmdata[[i]]$form,
                  prd.m      = pkmdata[[i]]$period,
                  ym        = pkmdata[[i]]$distance,
                  # ym = rep(0, 24),
                  prior.wt = c(0, 10),
                  prior.mt = c(0, 3),
                  Prior.tau.HN = 0.25,
                  wMix     = c(0.5, 0.5),
                  thres    = c(0.8, 1.25)
    )
    
    
    MyMod.fit <- bugs(data = data,
                      inits = inits,
                      model.file = "MyMod1.txt",
                      parameters = c("theta.m", "prob.ex", "pred.prob.be"),
                      n.chains = 1,
                      n.iter = 11000,
                      n.burnin = 1000,
                      bugs.seed = 1
    )
    
    
    if (MyMod.fit$summary[4]>=0.8)
      Y[[i]]=1 else 
        Y[[i]]=0
    
  }
  Y
}
#sc5+sc3
BLMM_M085_31<-BLMM_M085(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M085_31,"BLMM_M085_31.csv",row.names = FALSE)
sum(BLMM_M085_31)
BLMM_M085_32<-BLMM_M085(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M085_32,"BLMM_M085_32.csv",row.names = FALSE)
sum(BLMM_M085_32)

#sc5+sc4
BLMM_M085_41<-BLMM_M085(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M085_41,"BLMM_M085_41.csv",row.names = FALSE)
sum(BLMM_M085_41)
BLMM_M085_42<-BLMM_M085(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M085_42,"BLMM_M085_42.csv",row.names = FALSE)
sum(BLMM_M085_42)

#sc5+sc5
BLMM_M085_51<-BLMM_M085(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M085_51,"BLMM_M085_51.csv",row.names = FALSE)
sum(BLMM_M085_51)
BLMM_M085_52<-BLMM_M085(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M085_52,"BLMM_M085_52.csv",row.names = FALSE)
sum(BLMM_M085_52)

#sc5+sc6
BLMM_M085_61<-BLMM_M085(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M085_61,"BLMM_M085_61.csv",row.names = FALSE)
sum(BLMM_M085_61)
BLMM_M085_62<-BLMM_M085(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M085_62,"BLMM_M085_62.csv",row.names = FALSE)
sum(BLMM_M085_62)

#sc5+sc7
BLMM_M085_71<-BLMM_M085(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M085_71,"BLMM_M085_71.csv",row.names = FALSE)
sum(BLMM_M085_71)
BLMM_M085_72<-BLMM_M085(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M085_72,"BLMM_M085_72.csv",row.names = FALSE)
sum(BLMM_M085_72)


#Threshold=0.9 wMix=(0.5, 0.5)
BLMM_M095<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
  Y<-rep(99,10000)
  
  for (i in 1:N){
    data  <- list(Nobs.p     = 2*(n1+n2),
                  Npts.p     = n1+n2,
                  pts.p      = pkdata[[i]]$Subject,
                  trt.p      = pkdata[[i]]$form,
                  prd.p      = pkdata[[i]]$period,
                  yp        = pkdata[[i]]$distance,
                  # ,
                  Nobs.m     = 2*(n1_m+n2_m),
                  Npts.m     = n1_m+n2_m,
                  pts.m      = pkmdata[[i]]$Subject,
                  trt.m      = pkmdata[[i]]$form,
                  prd.m      = pkmdata[[i]]$period,
                  ym        = pkmdata[[i]]$distance,
                  # ym = rep(0, 24),
                  prior.wt = c(0, 10),
                  prior.mt = c(0, 3),
                  Prior.tau.HN = 0.25,
                  wMix     = c(0.5, 0.5),
                  thres    = c(0.8, 1.25)
    )
    
    
    MyMod.fit <- bugs(data = data,
                      inits = inits,
                      model.file = "MyMod1.txt",
                      parameters = c("theta.m", "prob.ex", "pred.prob.be"),
                      n.chains = 1,
                      n.iter = 11000,
                      n.burnin = 1000,
                      bugs.seed = 1
    )
    
    
    if (MyMod.fit$summary[4]>=0.9)
      Y[[i]]=1 else 
        Y[[i]]=0
    
  }
  Y
}
#sc5+sc3
BLMM_M095_31<-BLMM_M095(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M095_31,"BLMM_M095_31.csv",row.names = FALSE)
sum(BLMM_M095_31)
BLMM_M095_32<-BLMM_M095(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M095_32,"BLMM_M095_32.csv",row.names = FALSE)
sum(BLMM_M095_32)

#sc5+sc4
BLMM_M095_41<-BLMM_M095(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M095_41,"BLMM_M095_41.csv",row.names = FALSE)
sum(BLMM_M095_41)
BLMM_M095_42<-BLMM_M095(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M095_42,"BLMM_M095_42.csv",row.names = FALSE)
sum(BLMM_M095_42)

#sc5+sc5
BLMM_M095_51<-BLMM_M095(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M095_51,"BLMM_M095_51.csv",row.names = FALSE)
sum(BLMM_M095_51)
BLMM_M095_52<-BLMM_M095(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M095_52,"BLMM_M095_52.csv",row.names = FALSE)
sum(BLMM_M095_52)

#sc5+sc6
BLMM_M095_61<-BLMM_M095(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M095_61,"BLMM_M095_61.csv",row.names = FALSE)
sum(BLMM_M095_61)
BLMM_M095_62<-BLMM_M095(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M095_62,"BLMM_M095_62.csv",row.names = FALSE)
sum(BLMM_M095_62)

#sc5+sc7
BLMM_M095_71<-BLMM_M095(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M095_71,"BLMM_M095_71.csv",row.names = FALSE)
sum(BLMM_M095_71)
BLMM_M095_72<-BLMM_M095(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M095_72,"BLMM_M095_72.csv",row.names = FALSE)
sum(BLMM_M095_72)


################################
#sc5+sc1  wmix=0.5 t=0.5
BLMM_M055_11<-BLMM_M055(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M055_11,"BLMM_M055_11.csv",row.names = FALSE)
sum(BLMM_M055_11)
BLMM_M055_12<-BLMM_M055(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M055_12,"BLMM_M055_12.csv",row.names = FALSE)
sum(BLMM_M055_12)

#sc5+sc2
BLMM_M055_21<-BLMM_M055(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M055_21,"BLMM_M055_21.csv",row.names = FALSE)
sum(BLMM_M055_21)
BLMM_M055_22<-BLMM_M055(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M055_22,"BLMM_M055_22.csv",row.names = FALSE)
sum(BLMM_M055_22)

#sc5+sc8
BLMM_M055_81<-BLMM_M055(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M055_81,"BLMM_M055_81.csv",row.names = FALSE)
sum(BLMM_M055_81)
BLMM_M055_82<-BLMM_M055(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M055_82,"BLMM_M055_82.csv",row.names = FALSE)
sum(BLMM_M055_82)

#sc5+sc9
BLMM_M055_91<-BLMM_M055(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M055_91,"BLMM_M055_91.csv",row.names = FALSE)
sum(BLMM_M055_91)
BLMM_M055_92<-BLMM_M055(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M055_92,"BLMM_M055_92.csv",row.names = FALSE)
sum(BLMM_M055_92)


#sc5+sc1  wmix=0.5 t=0.6
BLMM_M065_11<-BLMM_M065(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M065_11,"BLMM_M065_11.csv",row.names = FALSE)
sum(BLMM_M065_11)
BLMM_M065_12<-BLMM_M065(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M065_12,"BLMM_M065_12.csv",row.names = FALSE)
sum(BLMM_M065_12)

#sc5+sc2
BLMM_M065_21<-BLMM_M065(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M065_21,"BLMM_M065_21.csv",row.names = FALSE)
sum(BLMM_M065_21)
BLMM_M065_22<-BLMM_M065(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M065_22,"BLMM_M065_22.csv",row.names = FALSE)
sum(BLMM_M065_22)

#sc5+sc8
BLMM_M065_81<-BLMM_M065(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M065_81,"BLMM_M065_81.csv",row.names = FALSE)
sum(BLMM_M065_81)
BLMM_M065_82<-BLMM_M065(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M065_82,"BLMM_M065_82.csv",row.names = FALSE)
sum(BLMM_M065_82)

#sc5+sc9
BLMM_M065_91<-BLMM_M065(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M065_91,"BLMM_M065_91.csv",row.names = FALSE)
sum(BLMM_M065_91)
BLMM_M065_92<-BLMM_M065(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M065_92,"BLMM_M065_92.csv",row.names = FALSE)
sum(BLMM_M065_92)


#sc5+sc1  wmix=0.5 t=0.7
BLMM_M075_11<-BLMM_M075(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M075_11,"BLMM_M075_11.csv",row.names = FALSE)
sum(BLMM_M075_11)
BLMM_M075_12<-BLMM_M075(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M075_12,"BLMM_M075_12.csv",row.names = FALSE)
sum(BLMM_M075_12)

#sc5+sc2
BLMM_M075_21<-BLMM_M075(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M075_21,"BLMM_M075_21.csv",row.names = FALSE)
sum(BLMM_M075_21)
BLMM_M075_22<-BLMM_M075(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M075_22,"BLMM_M075_22.csv",row.names = FALSE)
sum(BLMM_M075_22)

#sc5+sc8
BLMM_M075_81<-BLMM_M075(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M075_81,"BLMM_M075_81.csv",row.names = FALSE)
sum(BLMM_M075_81)
BLMM_M075_82<-BLMM_M075(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M075_82,"BLMM_M075_82.csv",row.names = FALSE)
sum(BLMM_M075_82)

#sc5+sc9
BLMM_M075_91<-BLMM_M075(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M075_91,"BLMM_M075_91.csv",row.names = FALSE)
sum(BLMM_M075_91)
BLMM_M075_92<-BLMM_M075(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M075_92,"BLMM_M075_92.csv",row.names = FALSE)
sum(BLMM_M075_92)

#sc5+sc1  wmix=0.5 t=0.8
BLMM_M085_11<-BLMM_M085(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M085_11,"BLMM_M085_11.csv",row.names = FALSE)
sum(BLMM_M085_11)
BLMM_M085_12<-BLMM_M085(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M085_12,"BLMM_M085_12.csv",row.names = FALSE)
sum(BLMM_M085_12)

#sc5+sc2
BLMM_M085_21<-BLMM_M085(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M085_21,"BLMM_M085_21.csv",row.names = FALSE)
sum(BLMM_M085_21)
BLMM_M085_22<-BLMM_M085(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M085_22,"BLMM_M085_22.csv",row.names = FALSE)
sum(BLMM_M085_22)

#sc5+sc8
BLMM_M085_81<-BLMM_M085(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M085_81,"BLMM_M085_81.csv",row.names = FALSE)
sum(BLMM_M085_81)
BLMM_M085_82<-BLMM_M085(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M085_82,"BLMM_M085_82.csv",row.names = FALSE)
sum(BLMM_M085_82)

#sc5+sc9
BLMM_M085_91<-BLMM_M085(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M085_91,"BLMM_M085_91.csv",row.names = FALSE)
sum(BLMM_M085_91)
BLMM_M085_92<-BLMM_M085(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M085_92,"BLMM_M085_92.csv",row.names = FALSE)
sum(BLMM_M085_92)

#sc5+sc1  wmix=0.5 t=0.9
BLMM_M095_11<-BLMM_M095(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M095_11,"BLMM_M095_11.csv",row.names = FALSE)
sum(BLMM_M095_11)
BLMM_M095_12<-BLMM_M095(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M095_12,"BLMM_M095_12.csv",row.names = FALSE)
sum(BLMM_M095_12)

#sc5+sc2
BLMM_M095_21<-BLMM_M095(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M095_21,"BLMM_M095_21.csv",row.names = FALSE)
sum(BLMM_M095_21)
BLMM_M095_22<-BLMM_M095(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M095_22,"BLMM_M095_22.csv",row.names = FALSE)
sum(BLMM_M095_22)

#sc5+sc8
BLMM_M095_81<-BLMM_M095(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M095_81,"BLMM_M095_81.csv",row.names = FALSE)
sum(BLMM_M095_81)
BLMM_M095_82<-BLMM_M095(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M095_82,"BLMM_M095_82.csv",row.names = FALSE)
sum(BLMM_M095_82)

#sc5+sc9
BLMM_M095_91<-BLMM_M095(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M095_91,"BLMM_M095_91.csv",row.names = FALSE)
sum(BLMM_M095_91)
BLMM_M095_92<-BLMM_M095(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M095_92,"BLMM_M095_92.csv",row.names = FALSE)
sum(BLMM_M095_92)

