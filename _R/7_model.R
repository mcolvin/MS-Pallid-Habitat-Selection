
	
# BUGS MODELS
## mod_01: stage model with segment effect
## mod_02: temperature model with segment effect
## mod_03: depth model with segment effect
## mod_04: current velocity model with segment effect


## mod_03a: temperature and stage model with segment effect
## mod_04a: temperature:stage interaction model with segment effect
## mod_05: temperature model without segment effect
## mod_06: stage model without segment effect
## mod_07: temperature and stage model without segment effect
## mod_08: temperature:stage interaction model without segment effect



mod_01<- function()
	{
	## PROCESS MODEL
	for(i in 1:nobs1)
		{
		for(habitat in 1:nhabs)
			{
			# PREDICT PROPORTION OF HABITAT ON LOGIT SCALE
			z[i,habitat]<- beta1[X[i,1],habitat]+beta2[X[i,1],habitat]*X[i,2]
			z_exp[i,habitat]<-  exp(z[i,habitat])
			# CONVERT TO PROBABILITY
			p[i,habitat]<-z_exp[i,habitat]/sum(z_exp[i,1:nhabs])
			}# end h		
		}

	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	## PREDICTING AVAILABILITY GIVEN STAGE
	for(i in 1:nobs1)
		{
		areas[i,1:nhabs]~dmulti(p[i,1:nhabs],total[i])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		beta1[i,1]<-0
		beta1[i,2]~dnorm(0,0.37)
		beta1[i,3]~dnorm(0,0.37)
		beta1[i,4]~dnorm(0,0.37)
		beta1[i,5]~dnorm(0,0.37)
		beta1[i,6]~dnorm(0,0.37)
		beta1[i,7]~dnorm(0,0.37)
	  
		### x1: BETAS
		beta2[i,1]<-0
		beta2[i,2]~dnorm(0,0.37)	
		beta2[i,3]~dnorm(0,0.37)
		beta2[i,4]~dnorm(0,0.37)
		beta2[i,5]~dnorm(0,0.37)
		beta2[i,6]~dnorm(0,0.37)
		beta2[i,7]~dnorm(0,0.37)
		}	
	# END AVAILABILITY MODEL

	## PRECICT AVAILABILITY GIVEN CONDITIONS AT OBSERVATION
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			z_hat[i,habitat]<- beta1[XX[i,1],habitat]+beta2[XX[i,1],habitat]*XX[i,2]
			z_hat_exp[i,habitat]<-  exp(z_hat[i,habitat])
			# CONVERT TO PROBABILITY
			avail_hat[i,habitat]<-z_hat_exp[i,habitat]/sum(z_hat_exp[i,1:nhabs])
			}
		}
	## END
	
	
	# BEGIN HABITAT SELECTION MODEL
		## PROCESS MODEL
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			zz[i,habitat]<-Intercept[XX[i,1],habitat]+
				Beta_1[XX[i,1],habitat]*XX[i,2]+
				log(avail_hat[i,habitat])# PREDICTED PROP. AVAILABLE GIVEN STAGE
			zz_exp[i,habitat]<- exp(zz[i,habitat])
			pp[i,habitat]<-zz_exp[i,habitat]/sum(zz_exp[i,1:nhabs])
			}# end h		
		}# end i
	
	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	for(i in 1:nobs2)
		{
		XX[i,6]~dcat(pp[i,1:nhabs])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		Intercept[i,1]<-0
		Intercept[i,2]~dnorm(0,0.4)
		Intercept[i,3]~dnorm(0,0.4)
		Intercept[i,4]~dnorm(0,0.4)
		Intercept[i,5]~dnorm(0,0.4)
		Intercept[i,6]~dnorm(0,0.4)
		Intercept[i,7]~dnorm(0,0.4)

		Beta_1[i,1]<-0
		Beta_1[i,2]~dnorm(0,0.4)	
		Beta_1[i,3]~dnorm(0,0.4)
		Beta_1[i,4]~dnorm(0,0.4)
		Beta_1[i,5]~dnorm(0,0.4)
		Beta_1[i,6]~dnorm(0,0.4)
		Beta_1[i,7]~dnorm(0,0.4)
		}
	# END HABITAT SELECITON MODEL
}
# end model

mod_02<- function()
	{
	## PROCESS MODEL
	for(i in 1:nobs1)
		{
		for(habitat in 1:nhabs)
			{
			# PREDICT PROPORTION OF HABITAT ON LOGIT SCALE
			z[i,habitat]<- beta1[X[i,1],habitat]+beta2[X[i,1],habitat]*X[i,2]
			z_exp[i,habitat]<-  exp(z[i,habitat])
			# CONVERT TO PROBABILITY
			p[i,habitat]<-z_exp[i,habitat]/sum(z_exp[i,1:nhabs])
			}# end h		
		}

	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	## PREDICTING AVAILABILITY GIVEN STAGE
	for(i in 1:nobs1)
		{
		areas[i,1:nhabs]~dmulti(p[i,1:nhabs],total[i])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		beta1[i,1]<-0
		beta1[i,2]~dnorm(0,0.37)
		beta1[i,3]~dnorm(0,0.37)
		beta1[i,4]~dnorm(0,0.37)
		beta1[i,5]~dnorm(0,0.37)
		beta1[i,6]~dnorm(0,0.37)
		beta1[i,7]~dnorm(0,0.37)
	  
		### x1: BETAS
		beta2[i,1]<-0
		beta2[i,2]~dnorm(0,0.37)	
		beta2[i,3]~dnorm(0,0.37)
		beta2[i,4]~dnorm(0,0.37)
		beta2[i,5]~dnorm(0,0.37)
		beta2[i,6]~dnorm(0,0.37)
		beta2[i,7]~dnorm(0,0.37)
		}	
	# END AVAILABILITY MODEL

	## PRECICT AVAILABILITY
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			z_hat[i,habitat]<- beta1[XX[i,1],habitat]+beta2[XX[i,1],habitat]*XX[i,2]
			z_hat_exp[i,habitat]<-  exp(z_hat[i,habitat])
			# CONVERT TO PROBABILITY
			avail_hat[i,habitat]<-z_hat_exp[i,habitat]/sum(z_hat_exp[i,1:nhabs])
			}
		}
	## END
	
	
	# BEGIN HABITAT SELECTION MODEL
		## PROCESS MODEL
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			zz[i,habitat]<-Intercept[XX[i,1],habitat]+
				Beta_stage[XX[i,1],habitat]*XX[i,3]+
				log(avail_hat[i,habitat])# PREDICTED PROP. AVAILABLE GIVEN STAGE
			zz_exp[i,habitat]<- exp(zz[i,habitat])
			pp[i,habitat]<-zz_exp[i,habitat]/sum(zz_exp[i,1:nhabs])
			}# end h		
		}# end i
	
	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	for(i in 1:nobs2)
		{
		XX[i,6]~dcat(pp[i,1:nhabs])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		Intercept[i,1]<-0
		Intercept[i,2]~dnorm(0,0.4)
		Intercept[i,3]~dnorm(0,0.4)
		Intercept[i,4]~dnorm(0,0.4)
		Intercept[i,5]~dnorm(0,0.4)
		Intercept[i,6]~dnorm(0,0.4)
		Intercept[i,7]~dnorm(0,0.4)

		Beta_stage[i,1]<-0
		Beta_stage[i,2]~dnorm(0,0.4)	
		Beta_stage[i,3]~dnorm(0,0.4)
		Beta_stage[i,4]~dnorm(0,0.4)
		Beta_stage[i,5]~dnorm(0,0.4)
		Beta_stage[i,6]~dnorm(0,0.4)
		Beta_stage[i,7]~dnorm(0,0.4)
		}
	# END HABITAT SELECITON MODEL
}
# end model

mod_03<- function()
	{
	## PROCESS MODEL
	for(i in 1:nobs1)
		{
		for(habitat in 1:nhabs)
			{
			# PREDICT PROPORTION OF HABITAT ON LOGIT SCALE
			z[i,habitat]<- beta1[X[i,1],habitat]+beta2[X[i,1],habitat]*X[i,2]
			z_exp[i,habitat]<-  exp(z[i,habitat])
			# CONVERT TO PROBABILITY
			p[i,habitat]<-z_exp[i,habitat]/sum(z_exp[i,1:nhabs])
			}# end h		
		}

	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	## PREDICTING AVAILABILITY GIVEN STAGE
	for(i in 1:nobs1)
		{
		areas[i,1:nhabs]~dmulti(p[i,1:nhabs],total[i])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		beta1[i,1]<-0
		beta1[i,2]~dnorm(0,0.37)
		beta1[i,3]~dnorm(0,0.37)
		beta1[i,4]~dnorm(0,0.37)
		beta1[i,5]~dnorm(0,0.37)
		beta1[i,6]~dnorm(0,0.37)
		beta1[i,7]~dnorm(0,0.37)
	  
		### x1: BETAS
		beta2[i,1]<-0
		beta2[i,2]~dnorm(0,0.37)	
		beta2[i,3]~dnorm(0,0.37)
		beta2[i,4]~dnorm(0,0.37)
		beta2[i,5]~dnorm(0,0.37)
		beta2[i,6]~dnorm(0,0.37)
		beta2[i,7]~dnorm(0,0.37)
		}	
	# END AVAILABILITY MODEL

	## PRECICT AVAILABILITY
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			z_hat[i,habitat]<- beta1[XX[i,1],habitat]+beta2[XX[i,1],habitat]*XX[i,2]
			z_hat_exp[i,habitat]<-  exp(z_hat[i,habitat])
			# CONVERT TO PROBABILITY
			avail_hat[i,habitat]<-z_hat_exp[i,habitat]/sum(z_hat_exp[i,1:nhabs])
			}
		}
	## END
	
	
	# BEGIN HABITAT SELECTION MODEL
		## PROCESS MODEL
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			zz[i,habitat]<-Intercept[XX[i,1],habitat]+
				Beta_1[XX[i,1],habitat]*XX[i,4]+
				log(avail_hat[i,habitat])# PREDICTED PROP. AVAILABLE GIVEN STAGE
			zz_exp[i,habitat]<- exp(zz[i,habitat])
			pp[i,habitat]<-zz_exp[i,habitat]/sum(zz_exp[i,1:nhabs])
			}# end h		
		}# end i
	
	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	for(i in 1:nobs2)
		{
		XX[i,6]~dcat(pp[i,1:nhabs])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		Intercept[i,1]<-0
		Intercept[i,2]~dnorm(0,0.4)
		Intercept[i,3]~dnorm(0,0.4)
		Intercept[i,4]~dnorm(0,0.4)
		Intercept[i,5]~dnorm(0,0.4)
		Intercept[i,6]~dnorm(0,0.4)
		Intercept[i,7]~dnorm(0,0.4)

		Beta_1[i,1]<-0
		Beta_1[i,2]~dnorm(0,0.4)	
		Beta_1[i,3]~dnorm(0,0.4)
		Beta_1[i,4]~dnorm(0,0.4)
		Beta_1[i,5]~dnorm(0,0.4)
		Beta_1[i,6]~dnorm(0,0.4)
		Beta_1[i,7]~dnorm(0,0.4)

		}
	# END HABITAT SELECITON MODEL
}
# end model
mod_04<- function()
	{
	## PROCESS MODEL
	for(i in 1:nobs1)
		{
		for(habitat in 1:nhabs)
			{
			# PREDICT PROPORTION OF HABITAT ON LOGIT SCALE
			z[i,habitat]<- beta1[X[i,1],habitat]+beta2[X[i,1],habitat]*X[i,2]
			z_exp[i,habitat]<-  exp(z[i,habitat])
			# CONVERT TO PROBABILITY
			p[i,habitat]<-z_exp[i,habitat]/sum(z_exp[i,1:nhabs])
			}# end h		
		}

	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	## PREDICTING AVAILABILITY GIVEN STAGE
	for(i in 1:nobs1)
		{
		areas[i,1:nhabs]~dmulti(p[i,1:nhabs],total[i])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		beta1[i,1]<-0
		beta1[i,2]~dnorm(0,0.37)
		beta1[i,3]~dnorm(0,0.37)
		beta1[i,4]~dnorm(0,0.37)
		beta1[i,5]~dnorm(0,0.37)
		beta1[i,6]~dnorm(0,0.37)
		beta1[i,7]~dnorm(0,0.37)
	  
		### x1: BETAS
		beta2[i,1]<-0
		beta2[i,2]~dnorm(0,0.37)	
		beta2[i,3]~dnorm(0,0.37)
		beta2[i,4]~dnorm(0,0.37)
		beta2[i,5]~dnorm(0,0.37)
		beta2[i,6]~dnorm(0,0.37)
		beta2[i,7]~dnorm(0,0.37)
		}	
	# END AVAILABILITY MODEL

	## PRECICT AVAILABILITY
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			z_hat[i,habitat]<- beta1[XX[i,1],habitat]+beta2[XX[i,1],habitat]*XX[i,2]
			z_hat_exp[i,habitat]<-  exp(z_hat[i,habitat])
			# CONVERT TO PROBABILITY
			avail_hat[i,habitat]<-z_hat_exp[i,habitat]/sum(z_hat_exp[i,1:nhabs])
			}
		}
	## END
	
	
	# BEGIN HABITAT SELECTION MODEL
		## PROCESS MODEL
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			zz[i,habitat]<-Intercept[XX[i,1],habitat]+
				Beta_1[XX[i,1],habitat]*XX[i,5]+
				log(avail_hat[i,habitat])# PREDICTED PROP. AVAILABLE GIVEN STAGE
			zz_exp[i,habitat]<- exp(zz[i,habitat])
			pp[i,habitat]<-zz_exp[i,habitat]/sum(zz_exp[i,1:nhabs])
			}# end h		
		}# end i
	
	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	for(i in 1:nobs2)
		{
		XX[i,6]~dcat(pp[i,1:nhabs])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		Intercept[i,1]<-0
		Intercept[i,2]~dnorm(0,0.4)
		Intercept[i,3]~dnorm(0,0.4)
		Intercept[i,4]~dnorm(0,0.4)
		Intercept[i,5]~dnorm(0,0.4)
		Intercept[i,6]~dnorm(0,0.4)
		Intercept[i,7]~dnorm(0,0.4)

		Beta_1[i,1]<-0
		Beta_1[i,2]~dnorm(0,0.4)	
		Beta_1[i,3]~dnorm(0,0.4)
		Beta_1[i,4]~dnorm(0,0.4)
		Beta_1[i,5]~dnorm(0,0.4)
		Beta_1[i,6]~dnorm(0,0.4)
		Beta_1[i,7]~dnorm(0,0.4)
		
		}
	# END HABITAT SELECITON MODEL
}
# end model

## UPDATED TO HERE

mod_03a<- function()
	{
	## PROCESS MODEL
	for(i in 1:nobs1)
		{
		for(habitat in 1:nhabs)
			{
			# PREDICT PROPORTION OF HABITAT ON LOGIT SCALE
			z[i,habitat]<- beta1[X[i,1],habitat]+beta2[X[i,1],habitat]*X[i,2]
			z_exp[i,habitat]<-  exp(z[i,habitat])
			# CONVERT TO PROBABILITY
			p[i,habitat]<-z_exp[i,habitat]/sum(z_exp[i,1:nhabs])
			}# end h		
		}

	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	## PREDICTING AVAILABILITY GIVEN STAGE
	for(i in 1:nobs1)
		{
		areas[i,1:nhabs]~dmulti(p[i,1:nhabs],total[i])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		beta1[i,1]<-0
		beta1[i,2]~dnorm(0,0.37)
		beta1[i,3]~dnorm(0,0.37)
		beta1[i,4]~dnorm(0,0.37)
		beta1[i,5]~dnorm(0,0.37)
		beta1[i,6]~dnorm(0,0.37)
		beta1[i,7]~dnorm(0,0.37)
	  
		### x1: BETAS
		beta2[i,1]<-0
		beta2[i,2]~dnorm(0,0.37)	
		beta2[i,3]~dnorm(0,0.37)
		beta2[i,4]~dnorm(0,0.37)
		beta2[i,5]~dnorm(0,0.37)
		beta2[i,6]~dnorm(0,0.37)
		beta2[i,7]~dnorm(0,0.37)
		}	
	# END AVAILABILITY MODEL

	## PRECICT AVAILABILITY
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			z_hat[i,habitat]<- beta1[XX[i,3],habitat]+beta2[XX[i,3],habitat]*XX[i,2]
			z_hat_exp[i,habitat]<-  exp(z_hat[i,habitat])
			# CONVERT TO PROBABILITY
			avail_hat[i,habitat]<-z_hat_exp[i,habitat]/sum(z_hat_exp[i,1:nhabs])
			}
		}
	## END
	
	
	# BEGIN HABITAT SELECTION MODEL
		## PROCESS MODEL
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			zz[i,habitat]<-Intercept[XX[i,3],habitat]+
				Beta_stage[XX[i,3],habitat]*XX[i,2]+
				Beta_temp[XX[i,3],habitat]*XX[i,1]+
				log(avail_hat[i,habitat])# PREDICTED PROP. AVAILABLE GIVEN STAGE
			zz_exp[i,habitat]<- exp(zz[i,habitat])
			pp[i,habitat]<-zz_exp[i,habitat]/sum(zz_exp[i,1:nhabs])
			}# end h		
		}# end i
	
	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	for(i in 1:nobs2)
		{
		hab[i]~dcat(pp[i,1:nhabs])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		Intercept[i,1]<-0
		Intercept[i,2]~dnorm(0,0.4)
		Intercept[i,3]~dnorm(0,0.4)
		Intercept[i,4]~dnorm(0,0.4)
		Intercept[i,5]~dnorm(0,0.4)
		Intercept[i,6]~dnorm(0,0.4)
		Intercept[i,7]~dnorm(0,0.4)

		Beta_stage[i,1]<-0
		Beta_stage[i,2]~dnorm(0,0.4)	
		Beta_stage[i,3]~dnorm(0,0.4)
		Beta_stage[i,4]~dnorm(0,0.4)
		Beta_stage[i,5]~dnorm(0,0.4)
		Beta_stage[i,6]~dnorm(0,0.4)
		Beta_stage[i,7]~dnorm(0,0.4)
		
		Beta_temp[i,1]<-0
		Beta_temp[i,2]~dnorm(0,0.4)	
		Beta_temp[i,3]~dnorm(0,0.4)
		Beta_temp[i,4]~dnorm(0,0.4)
		Beta_temp[i,5]~dnorm(0,0.4)
		Beta_temp[i,6]~dnorm(0,0.4)
		Beta_temp[i,7]~dnorm(0,0.4)
		}
	# END HABITAT SELECITON MODEL
}
# end model
mod_04a<- function()
	{
	## PROCESS MODEL
	for(i in 1:nobs1)
		{
		for(habitat in 1:nhabs)
			{
			# PREDICT PROPORTION OF HABITAT ON LOGIT SCALE
			z[i,habitat]<- beta1[X[i,1],habitat]+beta2[X[i,1],habitat]*X[i,2]
			z_exp[i,habitat]<-  exp(z[i,habitat])
			# CONVERT TO PROBABILITY
			p[i,habitat]<-z_exp[i,habitat]/sum(z_exp[i,1:nhabs])
			}# end h		
		}

	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	## PREDICTING AVAILABILITY GIVEN STAGE
	for(i in 1:nobs1)
		{
		areas[i,1:nhabs]~dmulti(p[i,1:nhabs],total[i])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		beta1[i,1]<-0
		beta1[i,2]~dnorm(0,0.37)
		beta1[i,3]~dnorm(0,0.37)
		beta1[i,4]~dnorm(0,0.37)
		beta1[i,5]~dnorm(0,0.37)
		beta1[i,6]~dnorm(0,0.37)
		beta1[i,7]~dnorm(0,0.37)
	  
		### x1: BETAS
		beta2[i,1]<-0
		beta2[i,2]~dnorm(0,0.37)	
		beta2[i,3]~dnorm(0,0.37)
		beta2[i,4]~dnorm(0,0.37)
		beta2[i,5]~dnorm(0,0.37)
		beta2[i,6]~dnorm(0,0.37)
		beta2[i,7]~dnorm(0,0.37)
		}	
	# END AVAILABILITY MODEL

	## PRECICT AVAILABILITY
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			z_hat[i,habitat]<- beta1[XX[i,3],habitat]+beta2[XX[i,3],habitat]*XX[i,2]
			z_hat_exp[i,habitat]<-  exp(z_hat[i,habitat])
			# CONVERT TO PROBABILITY
			avail_hat[i,habitat]<-z_hat_exp[i,habitat]/sum(z_hat_exp[i,1:nhabs])
			}
		}
	## END
	
	
	# BEGIN HABITAT SELECTION MODEL
		## PROCESS MODEL
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			zz[i,habitat]<-Intercept[XX[i,3],habitat]+
				Beta_stage[XX[i,3],habitat]*XX[i,2]+
				Beta_temp[XX[i,3],habitat]*XX[i,1]+
				Beta_int[XX[i,3],habitat]*XX[i,1]*XX[i,2]+
				log(avail_hat[i,habitat])# PREDICTED PROP. AVAILABLE GIVEN STAGE
			zz_exp[i,habitat]<- exp(zz[i,habitat])
			pp[i,habitat]<-zz_exp[i,habitat]/sum(zz_exp[i,1:nhabs])
			}# end h		
		}# end i
	
	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	for(i in 1:nobs2)
		{
		hab[i]~dcat(pp[i,1:nhabs])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		Intercept[i,1]<-0
		Intercept[i,2]~dnorm(0,0.4)
		Intercept[i,3]~dnorm(0,0.4)
		Intercept[i,4]~dnorm(0,0.4)
		Intercept[i,5]~dnorm(0,0.4)
		Intercept[i,6]~dnorm(0,0.4)
		Intercept[i,7]~dnorm(0,0.4)

		Beta_stage[i,1]<-0
		Beta_stage[i,2]~dnorm(0,0.4)	
		Beta_stage[i,3]~dnorm(0,0.4)
		Beta_stage[i,4]~dnorm(0,0.4)
		Beta_stage[i,5]~dnorm(0,0.4)
		Beta_stage[i,6]~dnorm(0,0.4)
		Beta_stage[i,7]~dnorm(0,0.4)
		
		Beta_temp[i,1]<-0
		Beta_temp[i,2]~dnorm(0,0.4)	
		Beta_temp[i,3]~dnorm(0,0.4)
		Beta_temp[i,4]~dnorm(0,0.4)
		Beta_temp[i,5]~dnorm(0,0.4)
		Beta_temp[i,6]~dnorm(0,0.4)
		Beta_temp[i,7]~dnorm(0,0.4)
		
		Beta_int[i,1]<-0
		Beta_int[i,2]~dnorm(0,0.4)	
		Beta_int[i,3]~dnorm(0,0.4)
		Beta_int[i,4]~dnorm(0,0.4)
		Beta_int[i,5]~dnorm(0,0.4)
		Beta_int[i,6]~dnorm(0,0.4)
		Beta_int[i,7]~dnorm(0,0.4)
		}
	# END HABITAT SELECITON MODEL
}
# end model
mod_05<- function()
	{
	## PROCESS MODEL
	for(i in 1:nobs1)
		{
		for(habitat in 1:nhabs)
			{
			# PREDICT PROPORTION OF HABITAT ON LOGIT SCALE
			z[i,habitat]<- beta1[X[i,1],habitat]+beta2[X[i,1],habitat]*X[i,2]
			z_exp[i,habitat]<-  exp(z[i,habitat])
			# CONVERT TO PROBABILITY
			p[i,habitat]<-z_exp[i,habitat]/sum(z_exp[i,1:nhabs])
			}# end h		
		}

	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	## PREDICTING AVAILABILITY GIVEN STAGE
	for(i in 1:nobs1)
		{
		areas[i,1:nhabs]~dmulti(p[i,1:nhabs],total[i])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		beta1[i,1]<-0
		beta1[i,2]~dnorm(0,0.37)
		beta1[i,3]~dnorm(0,0.37)
		beta1[i,4]~dnorm(0,0.37)
		beta1[i,5]~dnorm(0,0.37)
		beta1[i,6]~dnorm(0,0.37)
		beta1[i,7]~dnorm(0,0.37)
	  
		### x1: BETAS
		beta2[i,1]<-0
		beta2[i,2]~dnorm(0,0.37)	
		beta2[i,3]~dnorm(0,0.37)
		beta2[i,4]~dnorm(0,0.37)
		beta2[i,5]~dnorm(0,0.37)
		beta2[i,6]~dnorm(0,0.37)
		beta2[i,7]~dnorm(0,0.37)
		}	
	# END AVAILABILITY MODEL

	## PRECICT AVAILABILITY
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			z_hat[i,habitat]<- beta1[XX[i,3],habitat]+beta2[XX[i,3],habitat]*XX[i,2]
			z_hat_exp[i,habitat]<-  exp(z_hat[i,habitat])
			# CONVERT TO PROBABILITY
			avail_hat[i,habitat]<-z_hat_exp[i,habitat]/sum(z_hat_exp[i,1:nhabs])
			}
		}
	## END
	
	
	# BEGIN HABITAT SELECTION MODEL
		## PROCESS MODEL
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			zz[i,habitat]<-Intercept[habitat]+
				Beta_temp[habitat]*XX[i,1]+
				log(avail_hat[i,habitat])# PREDICTED PROP. AVAILABLE GIVEN STAGE
			zz_exp[i,habitat]<- exp(zz[i,habitat])
			pp[i,habitat]<-zz_exp[i,habitat]/sum(zz_exp[i,1:nhabs])
			}# end h		
		}# end i
	
	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	for(i in 1:nobs2)
		{
		hab[i]~dcat(pp[i,1:nhabs])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT

	Intercept[1]<-0
	Intercept[2]~dnorm(0,0.4)
	Intercept[3]~dnorm(0,0.4)
	Intercept[4]~dnorm(0,0.4)
	Intercept[5]~dnorm(0,0.4)
	Intercept[6]~dnorm(0,0.4)
	Intercept[7]~dnorm(0,0.4)

	Beta_temp[1]<-0
	Beta_temp[2]~dnorm(0,0.4)	
	Beta_temp[3]~dnorm(0,0.4)
	Beta_temp[4]~dnorm(0,0.4)
	Beta_temp[5]~dnorm(0,0.4)
	Beta_temp[6]~dnorm(0,0.4)
	Beta_temp[7]~dnorm(0,0.4)

	# END HABITAT SELECITON MODEL
}
# end model

mod_06<- function()
	{
	## PROCESS MODEL
	for(i in 1:nobs1)
		{
		for(habitat in 1:nhabs)
			{
			# PREDICT PROPORTION OF HABITAT ON LOGIT SCALE
			z[i,habitat]<- beta1[X[i,1],habitat]+beta2[X[i,1],habitat]*X[i,2]
			z_exp[i,habitat]<-  exp(z[i,habitat])
			# CONVERT TO PROBABILITY
			p[i,habitat]<-z_exp[i,habitat]/sum(z_exp[i,1:nhabs])
			}# end h		
		}

	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	## PREDICTING AVAILABILITY GIVEN STAGE
	for(i in 1:nobs1)
		{
		areas[i,1:nhabs]~dmulti(p[i,1:nhabs],total[i])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		beta1[i,1]<-0
		beta1[i,2]~dnorm(0,0.37)
		beta1[i,3]~dnorm(0,0.37)
		beta1[i,4]~dnorm(0,0.37)
		beta1[i,5]~dnorm(0,0.37)
		beta1[i,6]~dnorm(0,0.37)
		beta1[i,7]~dnorm(0,0.37)
	  
		### x1: BETAS
		beta2[i,1]<-0
		beta2[i,2]~dnorm(0,0.37)	
		beta2[i,3]~dnorm(0,0.37)
		beta2[i,4]~dnorm(0,0.37)
		beta2[i,5]~dnorm(0,0.37)
		beta2[i,6]~dnorm(0,0.37)
		beta2[i,7]~dnorm(0,0.37)
		}	
	# END AVAILABILITY MODEL

	## PRECICT AVAILABILITY
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			z_hat[i,habitat]<- beta1[XX[i,3],habitat]+beta2[XX[i,3],habitat]*XX[i,2]
			z_hat_exp[i,habitat]<-  exp(z_hat[i,habitat])
			# CONVERT TO PROBABILITY
			avail_hat[i,habitat]<-z_hat_exp[i,habitat]/sum(z_hat_exp[i,1:nhabs])
			}
		}
	## END
	
	
	# BEGIN HABITAT SELECTION MODEL
		## PROCESS MODEL
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			zz[i,habitat]<-Intercept[habitat]+
				Beta_stage[habitat]*XX[i,2]+
				log(avail_hat[i,habitat])# PREDICTED PROP. AVAILABLE GIVEN STAGE
			zz_exp[i,habitat]<- exp(zz[i,habitat])
			pp[i,habitat]<-zz_exp[i,habitat]/sum(zz_exp[i,1:nhabs])
			}# end h		
		}# end i
	
	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	for(i in 1:nobs2)
		{
		hab[i]~dcat(pp[i,1:nhabs])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	Intercept[1]<-0
	Intercept[2]~dnorm(0,0.4)
	Intercept[3]~dnorm(0,0.4)
	Intercept[4]~dnorm(0,0.4)
	Intercept[5]~dnorm(0,0.4)
	Intercept[6]~dnorm(0,0.4)
	Intercept[7]~dnorm(0,0.4)

	Beta_stage[1]<-0
	Beta_stage[2]~dnorm(0,0.4)	
	Beta_stage[3]~dnorm(0,0.4)
	Beta_stage[4]~dnorm(0,0.4)
	Beta_stage[5]~dnorm(0,0.4)
	Beta_stage[6]~dnorm(0,0.4)
	Beta_stage[7]~dnorm(0,0.4)

	# END HABITAT SELECITON MODEL
}
# end model

mod_07<- function()
	{
	## PROCESS MODEL
	for(i in 1:nobs1)
		{
		for(habitat in 1:nhabs)
			{
			# PREDICT PROPORTION OF HABITAT ON LOGIT SCALE
			z[i,habitat]<- beta1[X[i,1],habitat]+beta2[X[i,1],habitat]*X[i,2]
			z_exp[i,habitat]<-  exp(z[i,habitat])
			# CONVERT TO PROBABILITY
			p[i,habitat]<-z_exp[i,habitat]/sum(z_exp[i,1:nhabs])
			}# end h		
		}

	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	## PREDICTING AVAILABILITY GIVEN STAGE
	for(i in 1:nobs1)
		{
		areas[i,1:nhabs]~dmulti(p[i,1:nhabs],total[i])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		beta1[i,1]<-0
		beta1[i,2]~dnorm(0,0.37)
		beta1[i,3]~dnorm(0,0.37)
		beta1[i,4]~dnorm(0,0.37)
		beta1[i,5]~dnorm(0,0.37)
		beta1[i,6]~dnorm(0,0.37)
		beta1[i,7]~dnorm(0,0.37)
	  
		### x1: BETAS
		beta2[i,1]<-0
		beta2[i,2]~dnorm(0,0.37)	
		beta2[i,3]~dnorm(0,0.37)
		beta2[i,4]~dnorm(0,0.37)
		beta2[i,5]~dnorm(0,0.37)
		beta2[i,6]~dnorm(0,0.37)
		beta2[i,7]~dnorm(0,0.37)
		}	
	# END AVAILABILITY MODEL

	## PRECICT AVAILABILITY
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			z_hat[i,habitat]<- beta1[XX[i,3],habitat]+beta2[XX[i,3],habitat]*XX[i,2]
			z_hat_exp[i,habitat]<-  exp(z_hat[i,habitat])
			# CONVERT TO PROBABILITY
			avail_hat[i,habitat]<-z_hat_exp[i,habitat]/sum(z_hat_exp[i,1:nhabs])
			}
		}
	## END
	
	
	# BEGIN HABITAT SELECTION MODEL
		## PROCESS MODEL
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			zz[i,habitat]<-Intercept[habitat]+
				Beta_stage[habitat]*XX[i,2]+
				Beta_temp[habitat]*XX[i,1]+
				log(avail_hat[i,habitat])# PREDICTED PROP. AVAILABLE GIVEN STAGE
			zz_exp[i,habitat]<- exp(zz[i,habitat])
			pp[i,habitat]<-zz_exp[i,habitat]/sum(zz_exp[i,1:nhabs])
			}# end h		
		}# end i
	
	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	for(i in 1:nobs2)
		{
		hab[i]~dcat(pp[i,1:nhabs])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	Intercept[1]<-0
	Intercept[2]~dnorm(0,0.4)
	Intercept[3]~dnorm(0,0.4)
	Intercept[4]~dnorm(0,0.4)
	Intercept[5]~dnorm(0,0.4)
	Intercept[6]~dnorm(0,0.4)
	Intercept[7]~dnorm(0,0.4)

	Beta_stage[1]<-0
	Beta_stage[2]~dnorm(0,0.4)	
	Beta_stage[3]~dnorm(0,0.4)
	Beta_stage[4]~dnorm(0,0.4)
	Beta_stage[5]~dnorm(0,0.4)
	Beta_stage[6]~dnorm(0,0.4)
	Beta_stage[7]~dnorm(0,0.4)
	
	Beta_temp[1]<-0
	Beta_temp[2]~dnorm(0,0.4)	
	Beta_temp[3]~dnorm(0,0.4)
	Beta_temp[4]~dnorm(0,0.4)
	Beta_temp[5]~dnorm(0,0.4)
	Beta_temp[6]~dnorm(0,0.4)
	Beta_temp[7]~dnorm(0,0.4)
	# END HABITAT SELECITON MODEL
}
# end model

mod_08<- function()
	{
	## PROCESS MODEL
	for(i in 1:nobs1)
		{
		for(habitat in 1:nhabs)
			{
			# PREDICT PROPORTION OF HABITAT ON LOGIT SCALE
			z[i,habitat]<- beta1[X[i,1],habitat]+beta2[X[i,1],habitat]*X[i,2]
			z_exp[i,habitat]<-  exp(z[i,habitat])
			# CONVERT TO PROBABILITY
			p[i,habitat]<-z_exp[i,habitat]/sum(z_exp[i,1:nhabs])
			}# end h		
		}

	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	## PREDICTING AVAILABILITY GIVEN STAGE
	for(i in 1:nobs1)
		{
		areas[i,1:nhabs]~dmulti(p[i,1:nhabs],total[i])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		beta1[i,1]<-0
		beta1[i,2]~dnorm(0,0.37)
		beta1[i,3]~dnorm(0,0.37)
		beta1[i,4]~dnorm(0,0.37)
		beta1[i,5]~dnorm(0,0.37)
		beta1[i,6]~dnorm(0,0.37)
		beta1[i,7]~dnorm(0,0.37)
	  
		### x1: BETAS
		beta2[i,1]<-0
		beta2[i,2]~dnorm(0,0.37)	
		beta2[i,3]~dnorm(0,0.37)
		beta2[i,4]~dnorm(0,0.37)
		beta2[i,5]~dnorm(0,0.37)
		beta2[i,6]~dnorm(0,0.37)
		beta2[i,7]~dnorm(0,0.37)
		}	
	# END AVAILABILITY MODEL

	## PRECICT AVAILABILITY
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			z_hat[i,habitat]<- beta1[XX[i,3],habitat]+beta2[XX[i,3],habitat]*XX[i,2]
			z_hat_exp[i,habitat]<-  exp(z_hat[i,habitat])
			# CONVERT TO PROBABILITY
			avail_hat[i,habitat]<-z_hat_exp[i,habitat]/sum(z_hat_exp[i,1:nhabs])
			}
		}
	## END
	
	
	# BEGIN HABITAT SELECTION MODEL
		## PROCESS MODEL
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			zz[i,habitat]<-Intercept[habitat]+
				Beta_stage[habitat]*XX[i,2]+
				Beta_temp[habitat]*XX[i,1]+
				Beta_int[habitat]*XX[i,1]*XX[i,2]+
				log(avail_hat[i,habitat])# PREDICTED PROP. AVAILABLE GIVEN STAGE
			zz_exp[i,habitat]<- exp(zz[i,habitat])
			pp[i,habitat]<-zz_exp[i,habitat]/sum(zz_exp[i,1:nhabs])
			}# end h		
		}# end i
	
	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	for(i in 1:nobs2)
		{
		hab[i]~dcat(pp[i,1:nhabs])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	Intercept[1]<-0
	Intercept[2]~dnorm(0,0.4)
	Intercept[3]~dnorm(0,0.4)
	Intercept[4]~dnorm(0,0.4)
	Intercept[5]~dnorm(0,0.4)
	Intercept[6]~dnorm(0,0.4)
	Intercept[7]~dnorm(0,0.4)

	Beta_stage[1]<-0
	Beta_stage[2]~dnorm(0,0.4)	
	Beta_stage[3]~dnorm(0,0.4)
	Beta_stage[4]~dnorm(0,0.4)
	Beta_stage[5]~dnorm(0,0.4)
	Beta_stage[6]~dnorm(0,0.4)
	Beta_stage[7]~dnorm(0,0.4)
	
	Beta_temp[1]<-0
	Beta_temp[2]~dnorm(0,0.4)	
	Beta_temp[3]~dnorm(0,0.4)
	Beta_temp[4]~dnorm(0,0.4)
	Beta_temp[5]~dnorm(0,0.4)
	Beta_temp[6]~dnorm(0,0.4)
	Beta_temp[7]~dnorm(0,0.4)
	
	Beta_int[1]<-0
	Beta_int[2]~dnorm(0,0.4)	
	Beta_int[3]~dnorm(0,0.4)
	Beta_int[4]~dnorm(0,0.4)
	Beta_int[5]~dnorm(0,0.4)
	Beta_int[6]~dnorm(0,0.4)
	Beta_int[7]~dnorm(0,0.4)

	# END HABITAT SELECITON MODEL
}
# end model




### MODEL 3 INCLUDING GOF AND 
### HABITAT SELECTION FOR VALUES -2,2 FOR TEMP AND STAGE
mod_03_gof<- function()
	{
	## PROCESS MODEL
	for(i in 1:nobs1)
		{
		for(habitat in 1:nhabs)
			{
			# PREDICT PROPORTION OF HABITAT ON LOGIT SCALE
			z[i,habitat]<- beta1[X[i,1],habitat]+beta2[X[i,1],habitat]*X[i,2]
			z_exp[i,habitat]<-  exp(z[i,habitat])
			# CONVERT TO PROBABILITY
			p[i,habitat]<-z_exp[i,habitat]/sum(z_exp[i,1:nhabs])
			}# end h		
		}

	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	## PREDICTING AVAILABILITY GIVEN STAGE
	for(i in 1:nobs1)
		{
		areas[i,1:nhabs]~dmulti(p[i,1:nhabs],total[i])		
		}
		
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		beta1[i,1]<-0
		beta1[i,2]~dnorm(0,0.37)
		beta1[i,3]~dnorm(0,0.37)
		beta1[i,4]~dnorm(0,0.37)
		beta1[i,5]~dnorm(0,0.37)
		beta1[i,6]~dnorm(0,0.37)
		beta1[i,7]~dnorm(0,0.37)
	  
		### x1: BETAS
		beta2[i,1]<-0
		beta2[i,2]~dnorm(0,0.37)	
		beta2[i,3]~dnorm(0,0.37)
		beta2[i,4]~dnorm(0,0.37)
		beta2[i,5]~dnorm(0,0.37)
		beta2[i,6]~dnorm(0,0.37)
		beta2[i,7]~dnorm(0,0.37)
		}	
	# END AVAILABILITY MODEL

	## PRECICT AVAILABILITY
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			z_hat[i,habitat]<- beta1[XX[i,1],habitat]+beta2[XX[i,1],habitat]*XX[i,2]
			z_hat_exp[i,habitat]<-  exp(z_hat[i,habitat])
			# CONVERT TO PROBABILITY
			avail_hat[i,habitat]<-z_hat_exp[i,habitat]/sum(z_hat_exp[i,1:nhabs])
			}
		}
	## END
	
	
	# BEGIN HABITAT SELECTION MODEL
		## PROCESS MODEL
	for(i in 1:nobs2)
		{
		for(habitat in 1:nhabs)
			{
			zz[i,habitat]<-Intercept[XX[i,1],habitat]+
				Beta_stage[XX[i,1],habitat]*XX[i,2]+
				Beta_temp[XX[i,1],habitat]*XX[i,3]+
				log(avail_hat[i,habitat])# PREDICTED PROP. AVAILABLE GIVEN STAGE
			zz_exp[i,habitat]<- exp(zz[i,habitat])
			pp[i,habitat]<-zz_exp[i,habitat]/sum(zz_exp[i,1:nhabs])
			}# end h		
		}# end i
	
	## OBSERVATON MODEL (LIKLIHOOD MODEL)
	for(i in 1:nobs2)
		{
		hab[i]~dcat(pp[i,1:nhabs])	
		}
		
	## HABITAT SELECITON
	for(i in 1:n_sel)
		{
		for(habitat in 1:nhabs)
			{
			# AVAILABLE
			z_hat2[i,habitat]<- beta1[select[i,3],habitat]+beta2[select[i,3],habitat]*select[i,2]
			z_hat_exp2[i,habitat]<-  exp(z_hat2[i,habitat])
			# CONVERT TO PROBABILITY
			avail2[i,habitat]<-z_hat_exp2[i,habitat]/sum(z_hat_exp2[i,1:nhabs])
			# PREDICTED PROBABILITY OF USE
			zz2[i,habitat]<-Intercept[select[i,3],habitat]+
				Beta_stage[select[i,3],habitat]*select[i,2]+
				Beta_temp[select[i,3],habitat]*select[i,1]+
				log(avail2[i,habitat])# PREDICTED PROP. AVAILABLE GIVEN STAGE
			zz_exp2[i,habitat]<- exp(zz2[i,habitat])
			pp2[i,habitat]<-zz_exp2[i,habitat]/sum(zz_exp2[i,1:nhabs])
			
			# SELECTION
			sel[i,habitat]<- pp2[i,habitat]/avail2[i,habitat]			
			}# end h
		}
	## END HABITAT SELECTION


	
		
	# PRIORS		
	## BASELINE CONSTRAIN TO BE 0	
	### INTERCEPT
	for(i in 1:2)
		{
		Intercept[i,1]<-0
		Intercept[i,2]~dnorm(0,0.4)
		Intercept[i,3]~dnorm(0,0.4)
		Intercept[i,4]~dnorm(0,0.4)
		Intercept[i,5]~dnorm(0,0.4)
		Intercept[i,6]~dnorm(0,0.4)
		Intercept[i,7]~dnorm(0,0.4)

		Beta_stage[i,1]<-0
		Beta_stage[i,2]~dnorm(0,0.4)	
		Beta_stage[i,3]~dnorm(0,0.4)
		Beta_stage[i,4]~dnorm(0,0.4)
		Beta_stage[i,5]~dnorm(0,0.4)
		Beta_stage[i,6]~dnorm(0,0.4)
		Beta_stage[i,7]~dnorm(0,0.4)
		
		Beta_temp[i,1]<-0
		Beta_temp[i,2]~dnorm(0,0.4)	
		Beta_temp[i,3]~dnorm(0,0.4)
		Beta_temp[i,4]~dnorm(0,0.4)
		Beta_temp[i,5]~dnorm(0,0.4)
		Beta_temp[i,6]~dnorm(0,0.4)
		Beta_temp[i,7]~dnorm(0,0.4)
		}
	# END HABITAT SELECITON MODEL
}
# end model
