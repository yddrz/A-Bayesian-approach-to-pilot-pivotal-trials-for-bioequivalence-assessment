
#Threshold=0.5 wMix=(0.2, 0.8)
BLMM_M058<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0.2, 0.8),
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
BLMM_M058_31<-BLMM_M058(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M058_31,"BLMM_M058_31.csv",row.names = FALSE)
sum(BLMM_M058_31)
BLMM_M058_32<-BLMM_M058(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M058_32,"BLMM_M058_32.csv",row.names = FALSE)
sum(BLMM_M058_32)

#sc5+sc4
BLMM_M058_41<-BLMM_M058(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M058_41,"BLMM_M058_41.csv",row.names = FALSE)
sum(BLMM_M058_41)
BLMM_M058_42<-BLMM_M058(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M058_42,"BLMM_M058_42.csv",row.names = FALSE)
sum(BLMM_M058_42)

#sc5+sc5
BLMM_M058_51<-BLMM_M058(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M058_51,"BLMM_M058_51.csv",row.names = FALSE)
sum(BLMM_M058_51)
BLMM_M058_52<-BLMM_M058(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M058_52,"BLMM_M058_52.csv",row.names = FALSE)
sum(BLMM_M058_52)

#sc5+sc6
BLMM_M058_61<-BLMM_M058(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M058_61,"BLMM_M058_61.csv",row.names = FALSE)
sum(BLMM_M058_61)
BLMM_M058_62<-BLMM_M058(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M058_62,"BLMM_M058_62.csv",row.names = FALSE)
sum(BLMM_M058_62)

#sc5+sc7
BLMM_M058_71<-BLMM_M058(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M058_71,"BLMM_M058_71.csv",row.names = FALSE)
sum(BLMM_M058_71)
BLMM_M058_72<-BLMM_M058(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M058_72,"BLMM_M058_72.csv",row.names = FALSE)
sum(BLMM_M058_72)

#Threshold=0.6 wMix=(0.2, 0.8)
BLMM_M068<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0.2, 0.8),
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
BLMM_M068_31<-BLMM_M068(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M068_31,"BLMM_M068_31.csv",row.names = FALSE)
sum(BLMM_M068_31)
BLMM_M068_32<-BLMM_M068(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M068_32,"BLMM_M068_32.csv",row.names = FALSE)
sum(BLMM_M068_32)

#sc5+sc4
BLMM_M068_41<-BLMM_M068(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M068_41,"BLMM_M068_41.csv",row.names = FALSE)
sum(BLMM_M068_41)
BLMM_M068_42<-BLMM_M068(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M068_42,"BLMM_M068_42.csv",row.names = FALSE)
sum(BLMM_M068_42)

#sc5+sc5
BLMM_M068_51<-BLMM_M068(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M068_51,"BLMM_M068_51.csv",row.names = FALSE)
sum(BLMM_M068_51)
BLMM_M068_52<-BLMM_M068(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M068_52,"BLMM_M068_52.csv",row.names = FALSE)
sum(BLMM_M068_52)

#sc5+sc6
BLMM_M068_61<-BLMM_M068(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M068_61,"BLMM_M068_61.csv",row.names = FALSE)
sum(BLMM_M068_61)
BLMM_M068_62<-BLMM_M068(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M068_62,"BLMM_M068_62.csv",row.names = FALSE)
sum(BLMM_M068_62)

#sc5+sc7
BLMM_M068_71<-BLMM_M068(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M068_71,"BLMM_M068_71.csv",row.names = FALSE)
sum(BLMM_M068_71)
BLMM_M068_72<-BLMM_M068(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M068_72,"BLMM_M068_72.csv",row.names = FALSE)
sum(BLMM_M068_72)


#Threshold=0.7 wMix=(0.2, 0.8)
BLMM_M078<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0.2, 0.8),
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
BLMM_M078_31<-BLMM_M078(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M078_31,"BLMM_M078_31.csv",row.names = FALSE)
sum(BLMM_M078_31)
BLMM_M078_32<-BLMM_M078(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M078_32,"BLMM_M078_32.csv",row.names = FALSE)
sum(BLMM_M078_32)

#sc5+sc4
BLMM_M078_41<-BLMM_M078(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M078_41,"BLMM_M078_41.csv",row.names = FALSE)
sum(BLMM_M078_41)
BLMM_M078_42<-BLMM_M078(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M078_42,"BLMM_M078_42.csv",row.names = FALSE)
sum(BLMM_M078_42)

#sc5+sc5
BLMM_M078_51<-BLMM_M078(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M078_51,"BLMM_M078_51.csv",row.names = FALSE)
sum(BLMM_M078_51)
BLMM_M078_52<-BLMM_M078(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M078_52,"BLMM_M078_52.csv",row.names = FALSE)
sum(BLMM_M078_52)

#sc5+sc6
BLMM_M078_61<-BLMM_M078(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M078_61,"BLMM_M078_61.csv",row.names = FALSE)
sum(BLMM_M078_61)
BLMM_M078_62<-BLMM_M078(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M078_62,"BLMM_M078_62.csv",row.names = FALSE)
sum(BLMM_M078_62)

#sc5+sc7
BLMM_M078_71<-BLMM_M078(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M078_71,"BLMM_M078_71.csv",row.names = FALSE)
sum(BLMM_M078_71)
BLMM_M078_72<-BLMM_M078(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M078_72,"BLMM_M078_72.csv",row.names = FALSE)
sum(BLMM_M078_72)



#Threshold=0.8 wMix=(0.2, 0.8)
BLMM_M088<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0.2, 0.8),
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
BLMM_M088_31<-BLMM_M088(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M088_31,"BLMM_M088_31.csv",row.names = FALSE)
sum(BLMM_M088_31)
BLMM_M088_32<-BLMM_M088(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M088_32,"BLMM_M088_32.csv",row.names = FALSE)
sum(BLMM_M088_32)

#sc5+sc4
BLMM_M088_41<-BLMM_M088(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M088_41,"BLMM_M088_41.csv",row.names = FALSE)
sum(BLMM_M088_41)
BLMM_M088_42<-BLMM_M088(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M088_42,"BLMM_M088_42.csv",row.names = FALSE)
sum(BLMM_M088_42)

#sc5+sc5
BLMM_M088_51<-BLMM_M088(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M088_51,"BLMM_M088_51.csv",row.names = FALSE)
sum(BLMM_M088_51)
BLMM_M088_52<-BLMM_M088(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M088_52,"BLMM_M088_52.csv",row.names = FALSE)
sum(BLMM_M088_52)

#sc5+sc6
BLMM_M088_61<-BLMM_M088(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M088_61,"BLMM_M088_61.csv",row.names = FALSE)
sum(BLMM_M088_61)
BLMM_M088_62<-BLMM_M088(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M088_62,"BLMM_M088_62.csv",row.names = FALSE)
sum(BLMM_M088_62)

#sc5+sc7
BLMM_M088_71<-BLMM_M088(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M088_71,"BLMM_M088_71.csv",row.names = FALSE)
sum(BLMM_M088_71)
BLMM_M088_72<-BLMM_M088(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M088_72,"BLMM_M088_72.csv",row.names = FALSE)
sum(BLMM_M088_72)


#Threshold=0.9 wMix=(0.2, 0.8)
BLMM_M098<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0.2, 0.8),
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
BLMM_M098_31<-BLMM_M098(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M098_31,"BLMM_M098_31.csv",row.names = FALSE)
sum(BLMM_M098_31)
BLMM_M098_32<-BLMM_M098(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M098_32,"BLMM_M098_32.csv",row.names = FALSE)
sum(BLMM_M098_32)

#sc5+sc4
BLMM_M098_41<-BLMM_M098(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M098_41,"BLMM_M098_41.csv",row.names = FALSE)
sum(BLMM_M098_41)
BLMM_M098_42<-BLMM_M098(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M098_42,"BLMM_M098_42.csv",row.names = FALSE)
sum(BLMM_M098_42)

#sc5+sc5
BLMM_M098_51<-BLMM_M098(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M098_51,"BLMM_M098_51.csv",row.names = FALSE)
sum(BLMM_M098_51)
BLMM_M098_52<-BLMM_M098(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M098_52,"BLMM_M098_52.csv",row.names = FALSE)
sum(BLMM_M098_52)

#sc5+sc6
BLMM_M098_61<-BLMM_M098(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M098_61,"BLMM_M098_61.csv",row.names = FALSE)
sum(BLMM_M098_61)
BLMM_M098_62<-BLMM_M098(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M098_62,"BLMM_M098_62.csv",row.names = FALSE)
sum(BLMM_M098_62)

#sc5+sc7
BLMM_M098_71<-BLMM_M098(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M098_71,"BLMM_M098_71.csv",row.names = FALSE)
sum(BLMM_M098_71)
BLMM_M098_72<-BLMM_M098(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M098_72,"BLMM_M098_72.csv",row.names = FALSE)
sum(BLMM_M098_72)



################################
#sc5+sc1  wmix=0.8 t=0.5
BLMM_M058_11<-BLMM_M058(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M058_11,"BLMM_M058_11.csv",row.names = FALSE)
sum(BLMM_M058_11)
BLMM_M058_12<-BLMM_M058(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M058_12,"BLMM_M058_12.csv",row.names = FALSE)
sum(BLMM_M058_12)

#sc5+sc2
BLMM_M058_21<-BLMM_M058(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M058_21,"BLMM_M058_21.csv",row.names = FALSE)
sum(BLMM_M058_21)
BLMM_M058_22<-BLMM_M058(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M058_22,"BLMM_M058_22.csv",row.names = FALSE)
sum(BLMM_M058_22)

#sc5+sc8
BLMM_M058_81<-BLMM_M058(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M058_81,"BLMM_M058_81.csv",row.names = FALSE)
sum(BLMM_M058_81)
BLMM_M058_82<-BLMM_M058(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M058_82,"BLMM_M058_82.csv",row.names = FALSE)
sum(BLMM_M058_82)

#sc5+sc9
BLMM_M058_91<-BLMM_M058(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M058_91,"BLMM_M058_91.csv",row.names = FALSE)
sum(BLMM_M058_91)
BLMM_M058_92<-BLMM_M058(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M058_92,"BLMM_M058_92.csv",row.names = FALSE)
sum(BLMM_M058_92)


#sc5+sc1  wmix=0.8 t=0.6
BLMM_M068_11<-BLMM_M068(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M068_11,"BLMM_M068_11.csv",row.names = FALSE)
sum(BLMM_M068_11)
BLMM_M068_12<-BLMM_M068(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M068_12,"BLMM_M068_12.csv",row.names = FALSE)
sum(BLMM_M068_12)

#sc5+sc2
BLMM_M068_21<-BLMM_M068(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M068_21,"BLMM_M068_21.csv",row.names = FALSE)
sum(BLMM_M068_21)
BLMM_M068_22<-BLMM_M068(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M068_22,"BLMM_M068_22.csv",row.names = FALSE)
sum(BLMM_M068_22)

#sc5+sc8
BLMM_M068_81<-BLMM_M068(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M068_81,"BLMM_M068_81.csv",row.names = FALSE)
sum(BLMM_M068_81)
BLMM_M068_82<-BLMM_M068(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M068_82,"BLMM_M068_82.csv",row.names = FALSE)
sum(BLMM_M068_82)

#sc5+sc9
BLMM_M068_91<-BLMM_M068(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M068_91,"BLMM_M068_91.csv",row.names = FALSE)
sum(BLMM_M068_91)
BLMM_M068_92<-BLMM_M068(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M068_92,"BLMM_M068_92.csv",row.names = FALSE)
sum(BLMM_M068_92)


#sc5+sc1  wmix=0.8 t=0.7
BLMM_M078_11<-BLMM_M078(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M078_11,"BLMM_M078_11.csv",row.names = FALSE)
sum(BLMM_M078_11)
BLMM_M078_12<-BLMM_M078(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M078_12,"BLMM_M078_12.csv",row.names = FALSE)
sum(BLMM_M078_12)

#sc5+sc2
BLMM_M078_21<-BLMM_M078(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M078_21,"BLMM_M078_21.csv",row.names = FALSE)
sum(BLMM_M078_21)
BLMM_M078_22<-BLMM_M078(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M078_22,"BLMM_M078_22.csv",row.names = FALSE)
sum(BLMM_M078_22)

#sc5+sc8
BLMM_M078_81<-BLMM_M078(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M078_81,"BLMM_M078_81.csv",row.names = FALSE)
sum(BLMM_M078_81)
BLMM_M078_82<-BLMM_M078(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M078_82,"BLMM_M078_82.csv",row.names = FALSE)
sum(BLMM_M078_82)

#sc5+sc9
BLMM_M078_91<-BLMM_M078(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M078_91,"BLMM_M078_91.csv",row.names = FALSE)
sum(BLMM_M078_91)
BLMM_M078_92<-BLMM_M078(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M078_92,"BLMM_M078_92.csv",row.names = FALSE)
sum(BLMM_M078_92)

#sc5+sc1  wmix=0.8 t=0.8
BLMM_M088_11<-BLMM_M088(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M088_11,"BLMM_M088_11.csv",row.names = FALSE)
sum(BLMM_M088_11)
BLMM_M088_12<-BLMM_M088(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M088_12,"BLMM_M088_12.csv",row.names = FALSE)
sum(BLMM_M088_12)

#sc5+sc2
BLMM_M088_21<-BLMM_M088(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M088_21,"BLMM_M088_21.csv",row.names = FALSE)
sum(BLMM_M088_21)
BLMM_M088_22<-BLMM_M088(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M088_22,"BLMM_M088_22.csv",row.names = FALSE)
sum(BLMM_M088_22)

#sc5+sc8
BLMM_M088_81<-BLMM_M088(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M088_81,"BLMM_M088_81.csv",row.names = FALSE)
sum(BLMM_M088_81)
BLMM_M088_82<-BLMM_M088(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M088_82,"BLMM_M088_82.csv",row.names = FALSE)
sum(BLMM_M088_82)

#sc5+sc9
BLMM_M088_91<-BLMM_M088(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M088_91,"BLMM_M088_91.csv",row.names = FALSE)
sum(BLMM_M088_91)
BLMM_M088_92<-BLMM_M088(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M088_92,"BLMM_M088_92.csv",row.names = FALSE)
sum(BLMM_M088_92)

#sc5+sc1  wmix=0.8 t=0.9
BLMM_M098_11<-BLMM_M098(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M098_11,"BLMM_M098_11.csv",row.names = FALSE)
sum(BLMM_M098_11)
BLMM_M098_12<-BLMM_M098(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M098_12,"BLMM_M098_12.csv",row.names = FALSE)
sum(BLMM_M098_12)

#sc5+sc2
BLMM_M098_21<-BLMM_M098(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M098_21,"BLMM_M098_21.csv",row.names = FALSE)
sum(BLMM_M098_21)
BLMM_M098_22<-BLMM_M098(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M098_22,"BLMM_M098_22.csv",row.names = FALSE)
sum(BLMM_M098_22)

#sc5+sc8
BLMM_M098_81<-BLMM_M098(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M098_81,"BLMM_M098_81.csv",row.names = FALSE)
sum(BLMM_M098_81)
BLMM_M098_82<-BLMM_M098(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M098_82,"BLMM_M098_82.csv",row.names = FALSE)
sum(BLMM_M098_82)

#sc5+sc9
BLMM_M098_91<-BLMM_M098(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M098_91,"BLMM_M098_91.csv",row.names = FALSE)
sum(BLMM_M098_91)
BLMM_M098_92<-BLMM_M098(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M098_92,"BLMM_M098_92.csv",row.names = FALSE)
sum(BLMM_M098_92)

