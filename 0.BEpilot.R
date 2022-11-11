
install.packages(c('RColorBrewer','ggrepel'),repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')

library(R2OpenBUGS)

setwd("/data/Workdir_zhu/Dora")

sink("MyMod.txt")
cat("
model{
	# For fitting the pilot trial
	# tau.y[1] describes the within-patient variability
	for(i in 1:Nobs){
	y[i] ~ dnorm(mu.y[i], tau.y[1])
	mu.y[i] <- beta[1] + beta[2]*(prd[i] - 1) + theta[1]*(trt[i] - 1) + sig.y[pts[i]]
	}
	# for patient j
	# tau.y[2] describes the between-patient variability
	for(j in 1:Npts){
		sig.y[j] ~ dnorm(0, tau.y[2])
	}


	beta[1] ~ dnorm(0, 0.001)
	beta[2] ~ dnorm(0, 0.001)
	
	tau.y[1] ~ dgamma(0.001, 0.001)
	tau.y[2] ~ dgamma(0.001, 0.001)
	
		theta[1] <- mu.ex + re.ex
		mu.ex ~ dnorm(prior.mt[1], prec.mt)
    prec.mt <- pow(prior.mt[2], -2)
		re.ex ~ dnorm(0, prec.re.ex)
		prec.re.ex <- pow(Prior.tau.HN, -2)
    
		theta[2] ~ dnorm(prior.wt[1], prec.sw)
    prec.sw <- pow(prior.wt[2], -2)
    
    a<-(1/n1+1/n2)
    b<-(1/m1+1/m2)
    
    r <- exp(-1.64*sqrt(1/tau.y[1])*(sqrt(a)-sqrt(b)))
   
    theta.pred <- theta[which]
    which ~ dcat(wMix[1:2])
    
    	for(k in 1:2){
				prob.ex[k] <- equals(which, k)
			}

		# prediction of bioequivalence
		pred.prob.be <- step(log(thres[2]) - theta.pred) - step(log(thres[1]) - theta.pred)
		pred.prob.be1 <- step(log(thres[2]*r) - theta.pred) - step(log(thres[1]*(1/r)) - theta.pred)
		
}
    ", fill = TRUE)
sink()


inits <- function() {
  list(beta = c(0, 0), tau.y = c(1, 1), 
       mu.ex = 0.015, re.ex = 0
  )
}



