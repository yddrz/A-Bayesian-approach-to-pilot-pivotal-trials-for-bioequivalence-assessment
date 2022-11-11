#Threshold=0.75 wMix=(0.5, 0.5)
BLMM_M755<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
    
    
    if (MyMod.fit$summary[4]>=0.75)
      Y[[i]]=1 else 
        Y[[i]]=0
    
  }
  Y
}
#sc5+sc3
BLMM_M755_31<-BLMM_M755(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M755_31,"BLMM_M755_31.csv",row.names = FALSE)
sum(BLMM_M755_31)
BLMM_M755_32<-BLMM_M755(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M755_32,"BLMM_M755_32.csv",row.names = FALSE)
sum(BLMM_M755_32)

#sc5+sc4
BLMM_M755_41<-BLMM_M755(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M755_41,"BLMM_M755_41.csv",row.names = FALSE)
sum(BLMM_M755_41)
BLMM_M755_42<-BLMM_M755(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M755_42,"BLMM_M755_42.csv",row.names = FALSE)
sum(BLMM_M755_42)

#sc5+sc5
BLMM_M755_51<-BLMM_M755(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M755_51,"BLMM_M755_51.csv",row.names = FALSE)
sum(BLMM_M755_51)
BLMM_M755_52<-BLMM_M755(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M755_52,"BLMM_M755_52.csv",row.names = FALSE)
sum(BLMM_M755_52)

#sc5+sc6
BLMM_M755_61<-BLMM_M755(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M755_61,"BLMM_M755_61.csv",row.names = FALSE)
sum(BLMM_M755_61)
BLMM_M755_62<-BLMM_M755(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M755_62,"BLMM_M755_62.csv",row.names = FALSE)
sum(BLMM_M755_62)

#sc5+sc7
BLMM_M755_71<-BLMM_M755(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M755_71,"BLMM_M755_71.csv",row.names = FALSE)
sum(BLMM_M755_71)
BLMM_M755_72<-BLMM_M755(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M755_72,"BLMM_M755_72.csv",row.names = FALSE)
sum(BLMM_M755_72)





#Threshold=0.75 wMix=(0.2, 0.8)
BLMM_M758<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
    
    
    if (MyMod.fit$summary[4]>=0.75)
      Y[[i]]=1 else 
        Y[[i]]=0
    
  }
  Y
}
#sc5+sc3
BLMM_M758_31<-BLMM_M758(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M758_31,"BLMM_M758_31.csv",row.names = FALSE)
sum(BLMM_M758_31)
BLMM_M758_32<-BLMM_M758(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M758_32,"BLMM_M758_32.csv",row.names = FALSE)
sum(BLMM_M758_32)

#sc5+sc4
BLMM_M758_41<-BLMM_M758(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M758_41,"BLMM_M758_41.csv",row.names = FALSE)
sum(BLMM_M758_41)
BLMM_M758_42<-BLMM_M758(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M758_42,"BLMM_M758_42.csv",row.names = FALSE)
sum(BLMM_M758_42)

#sc5+sc5
BLMM_M758_51<-BLMM_M758(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M758_51,"BLMM_M758_51.csv",row.names = FALSE)
sum(BLMM_M758_51)
BLMM_M758_52<-BLMM_M758(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M758_52,"BLMM_M758_52.csv",row.names = FALSE)
sum(BLMM_M758_52)

#sc5+sc6
BLMM_M758_61<-BLMM_M758(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M758_61,"BLMM_M758_61.csv",row.names = FALSE)
sum(BLMM_M758_61)
BLMM_M758_62<-BLMM_M758(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M758_62,"BLMM_M758_62.csv",row.names = FALSE)
sum(BLMM_M758_62)

#sc5+sc7
BLMM_M758_71<-BLMM_M758(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M758_71,"BLMM_M758_71.csv",row.names = FALSE)
sum(BLMM_M758_71)
BLMM_M758_72<-BLMM_M758(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M758_72,"BLMM_M758_72.csv",row.names = FALSE)
sum(BLMM_M758_72)



#Threshold=0.75 wMix=(0, 1.0)
BLMM_M751<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
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
    
    
    if (MyMod.fit$summary[4]>=0.75)
      Y[[i]]=1 else 
        Y[[i]]=0
    
  }
  Y
}
#sc5+sc3
BLMM_M751_31<-BLMM_M751(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_M751_31,"BLMM_M751_31.csv",row.names = FALSE)
sum(BLMM_M751_31)
BLMM_M751_32<-BLMM_M751(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_M751_32,"BLMM_M751_32.csv",row.names = FALSE)
sum(BLMM_M751_32)

#sc5+sc4
BLMM_M751_41<-BLMM_M751(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_M751_41,"BLMM_M751_41.csv",row.names = FALSE)
sum(BLMM_M751_41)
BLMM_M751_42<-BLMM_M751(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_M751_42,"BLMM_M751_42.csv",row.names = FALSE)
sum(BLMM_M751_42)

#sc5+sc5
BLMM_M751_51<-BLMM_M751(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_M751_51,"BLMM_M751_51.csv",row.names = FALSE)
sum(BLMM_M751_51)
BLMM_M751_52<-BLMM_M751(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_M751_52,"BLMM_M751_52.csv",row.names = FALSE)
sum(BLMM_M751_52)

#sc5+sc6
BLMM_M751_61<-BLMM_M751(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_M751_61,"BLMM_M751_61.csv",row.names = FALSE)
sum(BLMM_M751_61)
BLMM_M751_62<-BLMM_M751(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_M751_62,"BLMM_M751_62.csv",row.names = FALSE)
sum(BLMM_M751_62)

#sc5+sc7
BLMM_M751_71<-BLMM_M751(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_M751_71,"BLMM_M751_71.csv",row.names = FALSE)
sum(BLMM_M751_71)
BLMM_M751_72<-BLMM_M751(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_M751_72,"BLMM_M751_72.csv",row.names = FALSE)
sum(BLMM_M751_72)


#Threshold=0.75 wMix=(0.1, 0.9)
BLMM_759<-function(pkdata,N,n1,n2,pkmdata,n1_m,n2_m){
  Y<-rep(99,10)
  
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
    
    
    if (MyMod.fit$summary[4]>=0.75)
      Y[[i]]=1 else 
        Y[[i]]=0
    
  }
  Y
}
#sc5+sc3
BLMM_759_31<-BLMM_759(cmax5,10000,6,6,cmaxp3,12,12)
write.csv(BLMM_759_31,"BLMM_759_31.csv",row.names = FALSE)
sum(BLMM_759_31)
BLMM_759_32<-BLMM_759(auct5,10000,6,6,auctp3,12,12)
write.csv(BLMM_759_32,"BLMM_759_32.csv",row.names = FALSE)
sum(BLMM_759_32)

#sc5+sc4
BLMM_759_41<-BLMM_759(cmax5,10000,6,6,cmaxp4,12,12)
write.csv(BLMM_759_41,"BLMM_759_41.csv",row.names = FALSE)
sum(BLMM_759_41)
BLMM_759_42<-BLMM_759(auct5,10000,6,6,auctp4,12,12)
write.csv(BLMM_759_42,"BLMM_759_42.csv",row.names = FALSE)
sum(BLMM_759_42)

#sc5+sc5
BLMM_759_51<-BLMM_759(cmax5,10000,6,6,cmaxp5,12,12)
write.csv(BLMM_759_51,"BLMM_759_51.csv",row.names = FALSE)
sum(BLMM_759_51)
BLMM_759_52<-BLMM_759(auct5,10000,6,6,auctp5,12,12)
write.csv(BLMM_759_52,"BLMM_759_52.csv",row.names = FALSE)
sum(BLMM_759_52)

#sc5+sc6
BLMM_759_61<-BLMM_759(cmax5,10000,6,6,cmaxp6,12,12)
write.csv(BLMM_759_61,"BLMM_759_61.csv",row.names = FALSE)
sum(BLMM_759_61)
BLMM_759_62<-BLMM_759(auct5,10000,6,6,auctp6,12,12)
write.csv(BLMM_759_62,"BLMM_759_62.csv",row.names = FALSE)
sum(BLMM_759_62)

#sc5+sc7
BLMM_759_71<-BLMM_759(cmax5,10000,6,6,cmaxp7,12,12)
write.csv(BLMM_759_71,"BLMM_759_71.csv",row.names = FALSE)
sum(BLMM_759_71)
BLMM_759_72<-BLMM_759(auct5,10000,6,6,auctp7,12,12)
write.csv(BLMM_759_72,"BLMM_759_72.csv",row.names = FALSE)
sum(BLMM_759_72)

############################################

#sc5+sc1 wmix=0.5
BLMM_M755_11<-BLMM_M755(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M755_11,"BLMM_M755_11.csv",row.names = FALSE)
sum(BLMM_M755_11)
BLMM_M755_12<-BLMM_M755(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M755_12,"BLMM_M755_12.csv",row.names = FALSE)
sum(BLMM_M755_12)

#sc5+sc2
BLMM_M755_21<-BLMM_M755(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M755_21,"BLMM_M755_21.csv",row.names = FALSE)
sum(BLMM_M755_21)
BLMM_M755_22<-BLMM_M755(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M755_22,"BLMM_M755_22.csv",row.names = FALSE)
sum(BLMM_M755_22)

#sc5+sc8
BLMM_M755_81<-BLMM_M755(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M755_81,"BLMM_M755_81.csv",row.names = FALSE)
sum(BLMM_M755_81)
BLMM_M755_82<-BLMM_M755(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M755_82,"BLMM_M755_82.csv",row.names = FALSE)
sum(BLMM_M755_82)

#sc5+sc9
BLMM_M755_91<-BLMM_M755(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M755_91,"BLMM_M755_91.csv",row.names = FALSE)
sum(BLMM_M755_91)
BLMM_M755_92<-BLMM_M755(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M755_92,"BLMM_M755_92.csv",row.names = FALSE)
sum(BLMM_M755_92)

################################################
#sc5+sc1   wmix=0.8
BLMM_M758_11<-BLMM_M758(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M758_11,"BLMM_M758_11.csv",row.names = FALSE)
sum(BLMM_M758_11)
BLMM_M758_12<-BLMM_M758(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M758_12,"BLMM_M758_12.csv",row.names = FALSE)
sum(BLMM_M758_12)

#sc5+sc2
BLMM_M758_21<-BLMM_M758(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M758_21,"BLMM_M758_21.csv",row.names = FALSE)
sum(BLMM_M758_21)
BLMM_M758_22<-BLMM_M758(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M758_22,"BLMM_M758_22.csv",row.names = FALSE)
sum(BLMM_M758_22)

#sc5+sc8
BLMM_M758_81<-BLMM_M758(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M758_81,"BLMM_M758_81.csv",row.names = FALSE)
sum(BLMM_M758_81)
BLMM_M758_82<-BLMM_M758(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M758_82,"BLMM_M758_82.csv",row.names = FALSE)
sum(BLMM_M758_82)

#sc5+sc9
BLMM_M758_91<-BLMM_M758(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M758_91,"BLMM_M758_91.csv",row.names = FALSE)
sum(BLMM_M758_91)
BLMM_M758_92<-BLMM_M758(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M758_92,"BLMM_M758_92.csv",row.names = FALSE)
sum(BLMM_M758_92)

####################################################
#sc5+sc1  wmix=1.0
BLMM_M751_11<-BLMM_M751(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_M751_11,"BLMM_M751_11.csv",row.names = FALSE)
sum(BLMM_M751_11)
BLMM_M751_12<-BLMM_M751(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_M751_12,"BLMM_M751_12.csv",row.names = FALSE)
sum(BLMM_M751_12)

#sc5+sc2
BLMM_M751_21<-BLMM_M751(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_M751_21,"BLMM_M751_21.csv",row.names = FALSE)
sum(BLMM_M751_21)
BLMM_M751_22<-BLMM_M751(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_M751_22,"BLMM_M751_22.csv",row.names = FALSE)
sum(BLMM_M751_22)

#sc5+sc8
BLMM_M751_81<-BLMM_M751(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_M751_81,"BLMM_M751_81.csv",row.names = FALSE)
sum(BLMM_M751_81)
BLMM_M751_82<-BLMM_M751(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_M751_82,"BLMM_M751_82.csv",row.names = FALSE)
sum(BLMM_M751_82)

#sc5+sc9
BLMM_M751_91<-BLMM_M751(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_M751_91,"BLMM_M751_91.csv",row.names = FALSE)
sum(BLMM_M751_91)
BLMM_M751_92<-BLMM_M751(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_M751_92,"BLMM_M751_92.csv",row.names = FALSE)
sum(BLMM_M751_92)
##############################################################
#sc5+sc1  wmix=0.9
BLMM_759_11<-BLMM_759(cmax5,10000,6,6,cmaxp1,12,12)
write.csv(BLMM_759_11,"BLMM_759_11.csv",row.names = FALSE)
sum(BLMM_759_11)
BLMM_759_12<-BLMM_759(auct5,10000,6,6,auctp1,12,12)
write.csv(BLMM_759_12,"BLMM_759_12.csv",row.names = FALSE)
sum(BLMM_759_12)

#sc5+sc2
BLMM_759_21<-BLMM_759(cmax5,10000,6,6,cmaxp2,12,12)
write.csv(BLMM_759_21,"BLMM_759_21.csv",row.names = FALSE)
sum(BLMM_759_21)
BLMM_759_22<-BLMM_759(auct5,10000,6,6,auctp2,12,12)
write.csv(BLMM_759_22,"BLMM_759_22.csv",row.names = FALSE)
sum(BLMM_759_22)

#sc5+sc8
BLMM_759_81<-BLMM_759(cmax5,10000,6,6,cmaxp8,12,12)
write.csv(BLMM_759_81,"BLMM_759_81.csv",row.names = FALSE)
sum(BLMM_759_81)
BLMM_759_82<-BLMM_759(auct5,10000,6,6,auctp8,12,12)
write.csv(BLMM_759_82,"BLMM_759_82.csv",row.names = FALSE)
sum(BLMM_759_82)

#sc5+sc9
BLMM_759_91<-BLMM_759(cmax5,10000,6,6,cmaxp9,12,12)
write.csv(BLMM_759_91,"BLMM_759_91.csv",row.names = FALSE)
sum(BLMM_759_91)
BLMM_759_92<-BLMM_759(auct5,10000,6,6,auctp9,12,12)
write.csv(BLMM_759_92,"BLMM_759_92.csv",row.names = FALSE)
sum(BLMM_759_92)