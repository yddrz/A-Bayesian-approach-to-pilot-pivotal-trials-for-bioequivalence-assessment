setwd("/data/Workdir_zhu/Dora")
library(R2OpenBUGS)
sink("MyMod1.txt")
cat("
model{
	# For fitting the pilot trial
	# tau.yp[1] describes the within-patient variability
	for(i in 1:Nobs.p){
	yp[i] ~ dnorm(mu.yp[i], tau.yp[1])
	mu.yp[i] <- beta.p[1] + beta.p[2]*(prd.p[i] - 1) + theta.p*(trt.p[i] - 1) + sig.yp[pts.p[i]]
	}
	# for patient j
	# tau.yp[2] describes the between-patient variability
	for(j in 1:Npts.p){
		sig.yp[j] ~ dnorm(0, tau.yp[2])
	}

# For fitting the pivotal trial
	# tau.ym[1] describes the within-patient variability
	for(i in 1:Nobs.m){
	ym[i] ~ dnorm(mu.ym[i], tau.ym[1])
	mu.ym[i] <- beta.m[1] + beta.m[2]*(prd.m[i] - 1) + theta.m*(trt.m[i] - 1) + sig.ym[pts.m[i]]
	}
	# for patient j
	# tau.ym[2] describes the between-patient variability
	for(j in 1:Npts.m){
		sig.ym[j] ~ dnorm(0, tau.ym[2])
	}

	beta.p[1] ~ dnorm(0, 0.001)
	beta.p[2] ~ dnorm(0, 0.001)
beta.m[1] ~ dnorm(0, 0.001)
beta.m[2] ~ dnorm(0, 0.001)

	tau.yp[1] ~ dgamma(0.001, 0.001)
	tau.yp[2] ~ dgamma(0.001, 0.001)
tau.ym[1] ~ dgamma(0.001, 0.001)
tau.ym[2] ~ dgamma(0.001, 0.001)

		theta.p <- mu.ex + re.ex
		mu.ex ~ dnorm(prior.mt[1], prec.mt)
    		prec.mt <- pow(prior.mt[2], -2)
		re.ex ~ dnorm(0, prec.re.ex)
		prec.re.ex <- pow(Prior.tau.HN, -2)
    
		theta[1] <- mu.ex + re.ex
		theta[2] ~ dnorm(prior.wt[1], prec.sw)
		prec.sw <- pow(prior.wt[2], -2)
		
    
    theta.m <- theta[which]
    which ~ dcat(wMix[1:2])
    
    	for(k in 1:2){
				prob.ex[k] <- equals(which, k)
			}

		# prediction of bioequivalence
		pred.prob.be <- step(log(thres[2]) - theta.m) - step(log(thres[1]) - theta.m)

}
    ", fill = TRUE)
sink()


inits <- function() {
  list(beta.p = c(0, 0), tau.yp = c(1, 1), 
       mu.ex = 0.015, re.ex = 0,
       beta.m = c(0, 0), tau.ym = c(1, 1)
       
  )
}