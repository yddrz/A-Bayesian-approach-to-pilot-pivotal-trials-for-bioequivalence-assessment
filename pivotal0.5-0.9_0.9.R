
#Threshold=0.5 wMix=(0.1, 0.9)
BLMM_M059<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0.1, 0.9),
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
BLMM_M059_31<-BLMM_M059(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M059_31,"BLMM_M059_31.csv",row.names = FALSE)
sum(BLMM_M059_31)
BLMM_M059_32<-BLMM_M059(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M059_32,"BLMM_M059_32.csv",row.names = FALSE)
sum(BLMM_M059_32)

#sc5+sc4
BLMM_M059_41<-BLMM_M059(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M059_41,"BLMM_M059_41.csv",row.names = FALSE)
sum(BLMM_M059_41)
BLMM_M059_42<-BLMM_M059(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M059_42,"BLMM_M059_42.csv",row.names = FALSE)
sum(BLMM_M059_42)

#sc5+sc5
BLMM_M059_51<-BLMM_M059(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M059_51,"BLMM_M059_51.csv",row.names = FALSE)
sum(BLMM_M059_51)
BLMM_M059_52<-BLMM_M059(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M059_52,"BLMM_M059_52.csv",row.names = FALSE)
sum(BLMM_M059_52)

#sc5+sc6
BLMM_M059_61<-BLMM_M059(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M059_61,"BLMM_M059_61.csv",row.names = FALSE)
sum(BLMM_M059_61)
BLMM_M059_62<-BLMM_M059(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M059_62,"BLMM_M059_62.csv",row.names = FALSE)
sum(BLMM_M059_62)

#sc5+sc7
BLMM_M059_71<-BLMM_M059(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M059_71,"BLMM_M059_71.csv",row.names = FALSE)
sum(BLMM_M059_71)
BLMM_M059_72<-BLMM_M059(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M059_72,"BLMM_M059_72.csv",row.names = FALSE)
sum(BLMM_M059_72)

#Threshold=0.6 wMix=(0.1, 0.9)
BLMM_M069<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0.1, 0.9),
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
BLMM_M069_31<-BLMM_M069(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M069_31,"BLMM_M069_31.csv",row.names = FALSE)
sum(BLMM_M069_31)
BLMM_M069_32<-BLMM_M069(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M069_32,"BLMM_M069_32.csv",row.names = FALSE)
sum(BLMM_M069_32)

#sc5+sc4
BLMM_M069_41<-BLMM_M069(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M069_41,"BLMM_M069_41.csv",row.names = FALSE)
sum(BLMM_M069_41)
BLMM_M069_42<-BLMM_M069(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M069_42,"BLMM_M069_42.csv",row.names = FALSE)
sum(BLMM_M069_42)

#sc5+sc5
BLMM_M069_51<-BLMM_M069(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M069_51,"BLMM_M069_51.csv",row.names = FALSE)
sum(BLMM_M069_51)
BLMM_M069_52<-BLMM_M069(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M069_52,"BLMM_M069_52.csv",row.names = FALSE)
sum(BLMM_M069_52)

#sc5+sc6
BLMM_M069_61<-BLMM_M069(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M069_61,"BLMM_M069_61.csv",row.names = FALSE)
sum(BLMM_M069_61)
BLMM_M069_62<-BLMM_M069(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M069_62,"BLMM_M069_62.csv",row.names = FALSE)
sum(BLMM_M069_62)

#sc5+sc7
BLMM_M069_71<-BLMM_M069(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M069_71,"BLMM_M069_71.csv",row.names = FALSE)
sum(BLMM_M069_71)
BLMM_M069_72<-BLMM_M069(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M069_72,"BLMM_M069_72.csv",row.names = FALSE)
sum(BLMM_M069_72)


#Threshold=0.7 wMix=(0.1, 0.9)
BLMM_M079<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0.1, 0.9),
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
BLMM_M079_31<-BLMM_M079(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M079_31,"BLMM_M079_31.csv",row.names = FALSE)
sum(BLMM_M079_31)
BLMM_M079_32<-BLMM_M079(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M079_32,"BLMM_M079_32.csv",row.names = FALSE)
sum(BLMM_M079_32)

#sc5+sc4
BLMM_M079_41<-BLMM_M079(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M079_41,"BLMM_M079_41.csv",row.names = FALSE)
sum(BLMM_M079_41)
BLMM_M079_42<-BLMM_M079(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M079_42,"BLMM_M079_42.csv",row.names = FALSE)
sum(BLMM_M079_42)

#sc5+sc5
BLMM_M079_51<-BLMM_M079(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M079_51,"BLMM_M079_51.csv",row.names = FALSE)
sum(BLMM_M079_51)
BLMM_M079_52<-BLMM_M079(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M079_52,"BLMM_M079_52.csv",row.names = FALSE)
sum(BLMM_M079_52)

#sc5+sc6
BLMM_M079_61<-BLMM_M079(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M079_61,"BLMM_M079_61.csv",row.names = FALSE)
sum(BLMM_M079_61)
BLMM_M079_62<-BLMM_M079(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M079_62,"BLMM_M079_62.csv",row.names = FALSE)
sum(BLMM_M079_62)

#sc5+sc7
BLMM_M079_71<-BLMM_M079(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M079_71,"BLMM_M079_71.csv",row.names = FALSE)
sum(BLMM_M079_71)
BLMM_M079_72<-BLMM_M079(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M079_72,"BLMM_M079_72.csv",row.names = FALSE)
sum(BLMM_M079_72)



#Threshold=0.8 wMix=(0.1, 0.9)
BLMM_M089<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0.1, 0.9),
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
BLMM_M089_31<-BLMM_M089(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M089_31,"BLMM_M089_31.csv",row.names = FALSE)
sum(BLMM_M089_31)
BLMM_M089_32<-BLMM_M089(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M089_32,"BLMM_M089_32.csv",row.names = FALSE)
sum(BLMM_M089_32)

#sc5+sc4
BLMM_M089_41<-BLMM_M089(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M089_41,"BLMM_M089_41.csv",row.names = FALSE)
sum(BLMM_M089_41)
BLMM_M089_42<-BLMM_M089(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M089_42,"BLMM_M089_42.csv",row.names = FALSE)
sum(BLMM_M089_42)

#sc5+sc5
BLMM_M089_51<-BLMM_M089(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M089_51,"BLMM_M089_51.csv",row.names = FALSE)
sum(BLMM_M089_51)
BLMM_M089_52<-BLMM_M089(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M089_52,"BLMM_M089_52.csv",row.names = FALSE)
sum(BLMM_M089_52)

#sc5+sc6
BLMM_M089_61<-BLMM_M089(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M089_61,"BLMM_M089_61.csv",row.names = FALSE)
sum(BLMM_M089_61)
BLMM_M089_62<-BLMM_M089(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M089_62,"BLMM_M089_62.csv",row.names = FALSE)
sum(BLMM_M089_62)

#sc5+sc7
BLMM_M089_71<-BLMM_M089(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M089_71,"BLMM_M089_71.csv",row.names = FALSE)
sum(BLMM_M089_71)
BLMM_M089_72<-BLMM_M089(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M089_72,"BLMM_M089_72.csv",row.names = FALSE)
sum(BLMM_M089_72)


#Threshold=0.9 wMix=(0.1, 0.9)
BLMM_M099<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
                  wMix     = c(0.1, 0.9),
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
BLMM_M099_31<-BLMM_M099(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M099_31,"BLMM_M099_31.csv",row.names = FALSE)
sum(BLMM_M099_31)
BLMM_M099_32<-BLMM_M099(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M099_32,"BLMM_M099_32.csv",row.names = FALSE)
sum(BLMM_M099_32)

#sc5+sc4
BLMM_M099_41<-BLMM_M099(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M099_41,"BLMM_M099_41.csv",row.names = FALSE)
sum(BLMM_M099_41)
BLMM_M099_42<-BLMM_M099(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M099_42,"BLMM_M099_42.csv",row.names = FALSE)
sum(BLMM_M099_42)

#sc5+sc5
BLMM_M099_51<-BLMM_M099(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M099_51,"BLMM_M099_51.csv",row.names = FALSE)
sum(BLMM_M099_51)
BLMM_M099_52<-BLMM_M099(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M099_52,"BLMM_M099_52.csv",row.names = FALSE)
sum(BLMM_M099_52)

#sc5+sc6
BLMM_M099_61<-BLMM_M099(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M099_61,"BLMM_M099_61.csv",row.names = FALSE)
sum(BLMM_M099_61)
BLMM_M099_62<-BLMM_M099(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M099_62,"BLMM_M099_62.csv",row.names = FALSE)
sum(BLMM_M099_62)

#sc5+sc7
BLMM_M099_71<-BLMM_M099(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M099_71,"BLMM_M099_71.csv",row.names = FALSE)
sum(BLMM_M099_71)
BLMM_M099_72<-BLMM_M099(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M099_72,"BLMM_M099_72.csv",row.names = FALSE)
sum(BLMM_M099_72)



#sc5+sc1  wmix=0.9 t=0.5
BLMM_M059_11<-BLMM_M059(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M059_11,"BLMM_M059_11.csv",row.names = FALSE)
sum(BLMM_M059_11)
BLMM_M059_12<-BLMM_M059(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M059_12,"BLMM_M059_12.csv",row.names = FALSE)
sum(BLMM_M059_12)

#sc5+sc2
BLMM_M059_21<-BLMM_M059(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M059_21,"BLMM_M059_21.csv",row.names = FALSE)
sum(BLMM_M059_21)
BLMM_M059_22<-BLMM_M059(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M059_22,"BLMM_M059_22.csv",row.names = FALSE)
sum(BLMM_M059_22)

#sc5+sc8
BLMM_M059_81<-BLMM_M059(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M059_81,"BLMM_M059_81.csv",row.names = FALSE)
sum(BLMM_M059_81)
BLMM_M059_82<-BLMM_M059(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M059_82,"BLMM_M059_82.csv",row.names = FALSE)
sum(BLMM_M059_82)

#sc5+sc9
BLMM_M059_91<-BLMM_M059(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M059_91,"BLMM_M059_91.csv",row.names = FALSE)
sum(BLMM_M059_91)
BLMM_M059_92<-BLMM_M059(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M059_92,"BLMM_M059_92.csv",row.names = FALSE)
sum(BLMM_M059_92)


#sc5+sc1  wmix=0.9 t=0.6
BLMM_M069_11<-BLMM_M069(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M069_11,"BLMM_M069_11.csv",row.names = FALSE)
sum(BLMM_M069_11)
BLMM_M069_12<-BLMM_M069(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M069_12,"BLMM_M069_12.csv",row.names = FALSE)
sum(BLMM_M069_12)

#sc5+sc2
BLMM_M069_21<-BLMM_M069(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M069_21,"BLMM_M069_21.csv",row.names = FALSE)
sum(BLMM_M069_21)
BLMM_M069_22<-BLMM_M069(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M069_22,"BLMM_M069_22.csv",row.names = FALSE)
sum(BLMM_M069_22)

#sc5+sc8
BLMM_M069_81<-BLMM_M069(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M069_81,"BLMM_M069_81.csv",row.names = FALSE)
sum(BLMM_M069_81)
BLMM_M069_82<-BLMM_M069(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M069_82,"BLMM_M069_82.csv",row.names = FALSE)
sum(BLMM_M069_82)

#sc5+sc9
BLMM_M069_91<-BLMM_M069(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M069_91,"BLMM_M069_91.csv",row.names = FALSE)
sum(BLMM_M069_91)
BLMM_M069_92<-BLMM_M069(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M069_92,"BLMM_M069_92.csv",row.names = FALSE)
sum(BLMM_M069_92)


#sc5+sc1  wmix=0.9 t=0.7
BLMM_M079_11<-BLMM_M079(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M079_11,"BLMM_M079_11.csv",row.names = FALSE)
sum(BLMM_M079_11)
BLMM_M079_12<-BLMM_M079(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M079_12,"BLMM_M079_12.csv",row.names = FALSE)
sum(BLMM_M079_12)

#sc5+sc2
BLMM_M079_21<-BLMM_M079(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M079_21,"BLMM_M079_21.csv",row.names = FALSE)
sum(BLMM_M079_21)
BLMM_M079_22<-BLMM_M079(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M079_22,"BLMM_M079_22.csv",row.names = FALSE)
sum(BLMM_M079_22)

#sc5+sc8
BLMM_M079_81<-BLMM_M079(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M079_81,"BLMM_M079_81.csv",row.names = FALSE)
sum(BLMM_M079_81)
BLMM_M079_82<-BLMM_M079(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M079_82,"BLMM_M079_82.csv",row.names = FALSE)
sum(BLMM_M079_82)

#sc5+sc9
BLMM_M079_91<-BLMM_M079(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M079_91,"BLMM_M079_91.csv",row.names = FALSE)
sum(BLMM_M079_91)
BLMM_M079_92<-BLMM_M079(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M079_92,"BLMM_M079_92.csv",row.names = FALSE)
sum(BLMM_M079_92)

#sc5+sc1  wmix=0.9 t=0.8
BLMM_M089_11<-BLMM_M089(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M089_11,"BLMM_M089_11.csv",row.names = FALSE)
sum(BLMM_M089_11)
BLMM_M089_12<-BLMM_M089(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M089_12,"BLMM_M089_12.csv",row.names = FALSE)
sum(BLMM_M089_12)

#sc5+sc2
BLMM_M089_21<-BLMM_M089(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M089_21,"BLMM_M089_21.csv",row.names = FALSE)
sum(BLMM_M089_21)
BLMM_M089_22<-BLMM_M089(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M089_22,"BLMM_M089_22.csv",row.names = FALSE)
sum(BLMM_M089_22)

#sc5+sc8
BLMM_M089_81<-BLMM_M089(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M089_81,"BLMM_M089_81.csv",row.names = FALSE)
sum(BLMM_M089_81)
BLMM_M089_82<-BLMM_M089(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M089_82,"BLMM_M089_82.csv",row.names = FALSE)
sum(BLMM_M089_82)

#sc5+sc9
BLMM_M089_91<-BLMM_M089(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M089_91,"BLMM_M089_91.csv",row.names = FALSE)
sum(BLMM_M089_91)
BLMM_M089_92<-BLMM_M089(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M089_92,"BLMM_M089_92.csv",row.names = FALSE)
sum(BLMM_M089_92)

#sc5+sc1  wmix=0.9 t=0.9
BLMM_M099_11<-BLMM_M099(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M099_11,"BLMM_M099_11.csv",row.names = FALSE)
sum(BLMM_M099_11)
BLMM_M099_12<-BLMM_M099(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M099_12,"BLMM_M099_12.csv",row.names = FALSE)
sum(BLMM_M099_12)

#sc5+sc2
BLMM_M099_21<-BLMM_M099(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M099_21,"BLMM_M099_21.csv",row.names = FALSE)
sum(BLMM_M099_21)
BLMM_M099_22<-BLMM_M099(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M099_22,"BLMM_M099_22.csv",row.names = FALSE)
sum(BLMM_M099_22)

#sc5+sc8
BLMM_M099_81<-BLMM_M099(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M099_81,"BLMM_M099_81.csv",row.names = FALSE)
sum(BLMM_M099_81)
BLMM_M099_82<-BLMM_M099(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M099_82,"BLMM_M099_82.csv",row.names = FALSE)
sum(BLMM_M099_82)

#sc5+sc9
BLMM_M099_91<-BLMM_M099(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M099_91,"BLMM_M099_91.csv",row.names = FALSE)
sum(BLMM_M099_91)
BLMM_M099_92<-BLMM_M099(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M099_92,"BLMM_M099_92.csv",row.names = FALSE)
sum(BLMM_M099_92)


