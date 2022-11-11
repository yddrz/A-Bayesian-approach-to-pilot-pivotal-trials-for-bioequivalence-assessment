
#Threshold=0.5 wMix=(0, 1.0)
BLMM_M051<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0, 1.0),
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
BLMM_M051_31<-BLMM_M051(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M051_31,"BLMM_M051_31.csv",row.names = FALSE)
sum(BLMM_M051_31)
BLMM_M051_32<-BLMM_M051(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M051_32,"BLMM_M051_32.csv",row.names = FALSE)
sum(BLMM_M051_32)

#sc5+sc4
BLMM_M051_41<-BLMM_M051(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M051_41,"BLMM_M051_41.csv",row.names = FALSE)
sum(BLMM_M051_41)
BLMM_M051_42<-BLMM_M051(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M051_42,"BLMM_M051_42.csv",row.names = FALSE)
sum(BLMM_M051_42)

#sc5+sc5
BLMM_M051_51<-BLMM_M051(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M051_51,"BLMM_M051_51.csv",row.names = FALSE)
sum(BLMM_M051_51)
BLMM_M051_52<-BLMM_M051(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M051_52,"BLMM_M051_52.csv",row.names = FALSE)
sum(BLMM_M051_52)

#sc5+sc6
BLMM_M051_61<-BLMM_M051(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M051_61,"BLMM_M051_61.csv",row.names = FALSE)
sum(BLMM_M051_61)
BLMM_M051_62<-BLMM_M051(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M051_62,"BLMM_M051_62.csv",row.names = FALSE)
sum(BLMM_M051_62)

#sc5+sc7
BLMM_M051_71<-BLMM_M051(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M051_71,"BLMM_M051_71.csv",row.names = FALSE)
sum(BLMM_M051_71)
BLMM_M051_72<-BLMM_M051(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M051_72,"BLMM_M051_72.csv",row.names = FALSE)
sum(BLMM_M051_72)

#Threshold=0.6 wMix=(0, 1.0)
BLMM_M061<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0, 1.0),
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
BLMM_M061_31<-BLMM_M061(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M061_31,"BLMM_M061_31.csv",row.names = FALSE)
sum(BLMM_M061_31)
BLMM_M061_32<-BLMM_M061(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M061_32,"BLMM_M061_32.csv",row.names = FALSE)
sum(BLMM_M061_32)

#sc5+sc4
BLMM_M061_41<-BLMM_M061(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M061_41,"BLMM_M061_41.csv",row.names = FALSE)
sum(BLMM_M061_41)
BLMM_M061_42<-BLMM_M061(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M061_42,"BLMM_M061_42.csv",row.names = FALSE)
sum(BLMM_M061_42)

#sc5+sc5
BLMM_M061_51<-BLMM_M061(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M061_51,"BLMM_M061_51.csv",row.names = FALSE)
sum(BLMM_M061_51)
BLMM_M061_52<-BLMM_M061(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M061_52,"BLMM_M061_52.csv",row.names = FALSE)
sum(BLMM_M061_52)

#sc5+sc6
BLMM_M061_61<-BLMM_M061(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M061_61,"BLMM_M061_61.csv",row.names = FALSE)
sum(BLMM_M061_61)
BLMM_M061_62<-BLMM_M061(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M061_62,"BLMM_M061_62.csv",row.names = FALSE)
sum(BLMM_M061_62)

#sc5+sc7
BLMM_M061_71<-BLMM_M061(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M061_71,"BLMM_M061_71.csv",row.names = FALSE)
sum(BLMM_M061_71)
BLMM_M061_72<-BLMM_M061(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M061_72,"BLMM_M061_72.csv",row.names = FALSE)
sum(BLMM_M061_72)


#Threshold=0.7 wMix=(0, 1.0)
BLMM_M071<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0, 1.0),
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
BLMM_M071_31<-BLMM_M071(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M071_31,"BLMM_M071_31.csv",row.names = FALSE)
sum(BLMM_M071_31)
BLMM_M071_32<-BLMM_M071(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M071_32,"BLMM_M071_32.csv",row.names = FALSE)
sum(BLMM_M071_32)

#sc5+sc4
BLMM_M071_41<-BLMM_M071(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M071_41,"BLMM_M071_41.csv",row.names = FALSE)
sum(BLMM_M071_41)
BLMM_M071_42<-BLMM_M071(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M071_42,"BLMM_M071_42.csv",row.names = FALSE)
sum(BLMM_M071_42)

#sc5+sc5
BLMM_M071_51<-BLMM_M071(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M071_51,"BLMM_M071_51.csv",row.names = FALSE)
sum(BLMM_M071_51)
BLMM_M071_52<-BLMM_M071(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M071_52,"BLMM_M071_52.csv",row.names = FALSE)
sum(BLMM_M071_52)

#sc5+sc6
BLMM_M071_61<-BLMM_M071(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M071_61,"BLMM_M071_61.csv",row.names = FALSE)
sum(BLMM_M071_61)
BLMM_M071_62<-BLMM_M071(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M071_62,"BLMM_M071_62.csv",row.names = FALSE)
sum(BLMM_M071_62)

#sc5+sc7
BLMM_M071_71<-BLMM_M071(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M071_71,"BLMM_M071_71.csv",row.names = FALSE)
sum(BLMM_M071_71)
BLMM_M071_72<-BLMM_M071(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M071_72,"BLMM_M071_72.csv",row.names = FALSE)
sum(BLMM_M071_72)



#Threshold=0.8 wMix=(0, 1.0)
BLMM_M081<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0, 1.0),
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
BLMM_M081_31<-BLMM_M081(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M081_31,"BLMM_M081_31.csv",row.names = FALSE)
sum(BLMM_M081_31)
BLMM_M081_32<-BLMM_M081(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M081_32,"BLMM_M081_32.csv",row.names = FALSE)
sum(BLMM_M081_32)

#sc5+sc4
BLMM_M081_41<-BLMM_M081(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M081_41,"BLMM_M081_41.csv",row.names = FALSE)
sum(BLMM_M081_41)
BLMM_M081_42<-BLMM_M081(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M081_42,"BLMM_M081_42.csv",row.names = FALSE)
sum(BLMM_M081_42)

#sc5+sc5
BLMM_M081_51<-BLMM_M081(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M081_51,"BLMM_M081_51.csv",row.names = FALSE)
sum(BLMM_M081_51)
BLMM_M081_52<-BLMM_M081(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M081_52,"BLMM_M081_52.csv",row.names = FALSE)
sum(BLMM_M081_52)

#sc5+sc6
BLMM_M081_61<-BLMM_M081(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M081_61,"BLMM_M081_61.csv",row.names = FALSE)
sum(BLMM_M081_61)
BLMM_M081_62<-BLMM_M081(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M081_62,"BLMM_M081_62.csv",row.names = FALSE)
sum(BLMM_M081_62)

#sc5+sc7
BLMM_M081_71<-BLMM_M081(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M081_71,"BLMM_M081_71.csv",row.names = FALSE)
sum(BLMM_M081_71)
BLMM_M081_72<-BLMM_M081(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M081_72,"BLMM_M081_72.csv",row.names = FALSE)
sum(BLMM_M081_72)


#Threshold=0.9 wMix=(0, 1.0)
BLMM_M091<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0, 1.0),
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
BLMM_M091_31<-BLMM_M091(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M091_31,"BLMM_M091_31.csv",row.names = FALSE)
sum(BLMM_M091_31)
BLMM_M091_32<-BLMM_M091(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M091_32,"BLMM_M091_32.csv",row.names = FALSE)
sum(BLMM_M091_32)

#sc5+sc4
BLMM_M091_41<-BLMM_M091(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M091_41,"BLMM_M091_41.csv",row.names = FALSE)
sum(BLMM_M091_41)
BLMM_M091_42<-BLMM_M091(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M091_42,"BLMM_M091_42.csv",row.names = FALSE)
sum(BLMM_M091_42)

#sc5+sc5
BLMM_M091_51<-BLMM_M091(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M091_51,"BLMM_M091_51.csv",row.names = FALSE)
sum(BLMM_M091_51)
BLMM_M091_52<-BLMM_M091(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M091_52,"BLMM_M091_52.csv",row.names = FALSE)
sum(BLMM_M091_52)

#sc5+sc6
BLMM_M091_61<-BLMM_M091(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M091_61,"BLMM_M091_61.csv",row.names = FALSE)
sum(BLMM_M091_61)
BLMM_M091_62<-BLMM_M091(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M091_62,"BLMM_M091_62.csv",row.names = FALSE)
sum(BLMM_M091_62)

#sc5+sc7
BLMM_M091_71<-BLMM_M091(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M091_71,"BLMM_M091_71.csv",row.names = FALSE)
sum(BLMM_M091_71)
BLMM_M091_72<-BLMM_M091(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M091_72,"BLMM_M091_72.csv",row.names = FALSE)
sum(BLMM_M091_72)


#sc5+sc1  wmix=1.0 t=0.5
BLMM_M051_11<-BLMM_M051(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M051_11,"BLMM_M051_11.csv",row.names = FALSE)
sum(BLMM_M051_11)
BLMM_M051_12<-BLMM_M051(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M051_12,"BLMM_M051_12.csv",row.names = FALSE)
sum(BLMM_M051_12)

#sc5+sc2
BLMM_M051_21<-BLMM_M051(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M051_21,"BLMM_M051_21.csv",row.names = FALSE)
sum(BLMM_M051_21)
BLMM_M051_22<-BLMM_M051(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M051_22,"BLMM_M051_22.csv",row.names = FALSE)
sum(BLMM_M051_22)

#sc5+sc8
BLMM_M051_81<-BLMM_M051(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M051_81,"BLMM_M051_81.csv",row.names = FALSE)
sum(BLMM_M051_81)
BLMM_M051_82<-BLMM_M051(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M051_82,"BLMM_M051_82.csv",row.names = FALSE)
sum(BLMM_M051_82)

#sc5+sc9
BLMM_M051_91<-BLMM_M051(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M051_91,"BLMM_M051_91.csv",row.names = FALSE)
sum(BLMM_M051_91)
BLMM_M051_92<-BLMM_M051(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M051_92,"BLMM_M051_92.csv",row.names = FALSE)
sum(BLMM_M051_92)


#sc5+sc1  wmix=1.0 t=0.6
BLMM_M061_11<-BLMM_M061(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M061_11,"BLMM_M061_11.csv",row.names = FALSE)
sum(BLMM_M061_11)
BLMM_M061_12<-BLMM_M061(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M061_12,"BLMM_M061_12.csv",row.names = FALSE)
sum(BLMM_M061_12)

#sc5+sc2
BLMM_M061_21<-BLMM_M061(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M061_21,"BLMM_M061_21.csv",row.names = FALSE)
sum(BLMM_M061_21)
BLMM_M061_22<-BLMM_M061(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M061_22,"BLMM_M061_22.csv",row.names = FALSE)
sum(BLMM_M061_22)

#sc5+sc8
BLMM_M061_81<-BLMM_M061(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M061_81,"BLMM_M061_81.csv",row.names = FALSE)
sum(BLMM_M061_81)
BLMM_M061_82<-BLMM_M061(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M061_82,"BLMM_M061_82.csv",row.names = FALSE)
sum(BLMM_M061_82)

#sc5+sc9
BLMM_M061_91<-BLMM_M061(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M061_91,"BLMM_M061_91.csv",row.names = FALSE)
sum(BLMM_M061_91)
BLMM_M061_92<-BLMM_M061(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M061_92,"BLMM_M061_92.csv",row.names = FALSE)
sum(BLMM_M061_92)


#sc5+sc1  wmix=1.0 t=0.7
BLMM_M071_11<-BLMM_M071(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M071_11,"BLMM_M071_11.csv",row.names = FALSE)
sum(BLMM_M071_11)
BLMM_M071_12<-BLMM_M071(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M071_12,"BLMM_M071_12.csv",row.names = FALSE)
sum(BLMM_M071_12)

#sc5+sc2
BLMM_M071_21<-BLMM_M071(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M071_21,"BLMM_M071_21.csv",row.names = FALSE)
sum(BLMM_M071_21)
BLMM_M071_22<-BLMM_M071(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M071_22,"BLMM_M071_22.csv",row.names = FALSE)
sum(BLMM_M071_22)

#sc5+sc8
BLMM_M071_81<-BLMM_M071(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M071_81,"BLMM_M071_81.csv",row.names = FALSE)
sum(BLMM_M071_81)
BLMM_M071_82<-BLMM_M071(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M071_82,"BLMM_M071_82.csv",row.names = FALSE)
sum(BLMM_M071_82)

#sc5+sc9
BLMM_M071_91<-BLMM_M071(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M071_91,"BLMM_M071_91.csv",row.names = FALSE)
sum(BLMM_M071_91)
BLMM_M071_92<-BLMM_M071(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M071_92,"BLMM_M071_92.csv",row.names = FALSE)
sum(BLMM_M071_92)

#sc5+sc1  wmix=1.0 t=0.8
BLMM_M081_11<-BLMM_M081(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M081_11,"BLMM_M081_11.csv",row.names = FALSE)
sum(BLMM_M081_11)
BLMM_M081_12<-BLMM_M081(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M081_12,"BLMM_M081_12.csv",row.names = FALSE)
sum(BLMM_M081_12)

#sc5+sc2
BLMM_M081_21<-BLMM_M081(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M081_21,"BLMM_M081_21.csv",row.names = FALSE)
sum(BLMM_M081_21)
BLMM_M081_22<-BLMM_M081(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M081_22,"BLMM_M081_22.csv",row.names = FALSE)
sum(BLMM_M081_22)

#sc5+sc8
BLMM_M081_81<-BLMM_M081(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M081_81,"BLMM_M081_81.csv",row.names = FALSE)
sum(BLMM_M081_81)
BLMM_M081_82<-BLMM_M081(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M081_82,"BLMM_M081_82.csv",row.names = FALSE)
sum(BLMM_M081_82)

#sc5+sc9
BLMM_M081_91<-BLMM_M081(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M081_91,"BLMM_M081_91.csv",row.names = FALSE)
sum(BLMM_M081_91)
BLMM_M081_92<-BLMM_M081(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M081_92,"BLMM_M081_92.csv",row.names = FALSE)
sum(BLMM_M081_92)

#sc5+sc1  wmix=1.0 t=0.9
BLMM_M091_11<-BLMM_M091(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M091_11,"BLMM_M091_11.csv",row.names = FALSE)
sum(BLMM_M091_11)
BLMM_M091_12<-BLMM_M091(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M091_12,"BLMM_M091_12.csv",row.names = FALSE)
sum(BLMM_M091_12)

#sc5+sc2
BLMM_M091_21<-BLMM_M091(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M091_21,"BLMM_M091_21.csv",row.names = FALSE)
sum(BLMM_M091_21)
BLMM_M091_22<-BLMM_M091(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M091_22,"BLMM_M091_22.csv",row.names = FALSE)
sum(BLMM_M091_22)

#sc5+sc8
BLMM_M091_81<-BLMM_M091(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M091_81,"BLMM_M091_81.csv",row.names = FALSE)
sum(BLMM_M091_81)
BLMM_M091_82<-BLMM_M091(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M091_82,"BLMM_M091_82.csv",row.names = FALSE)
sum(BLMM_M091_82)

#sc5+sc9
BLMM_M091_91<-BLMM_M091(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M091_91,"BLMM_M091_91.csv",row.names = FALSE)
sum(BLMM_M091_91)
BLMM_M091_92<-BLMM_M091(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M091_92,"BLMM_M091_92.csv",row.names = FALSE)
sum(BLMM_M091_92)

