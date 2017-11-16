# MODEL 3: TEMPERATURE AND STAGE WITH PREDICTIONS AND HABITAT SELECTION



params<- c("beta1",'beta2','Intercept',
	'Beta_stage','Beta_temp','p',"sel") 
selection<- expand.grid(temp=seq(-2,2,by=0.5),stage=0,loc=c(1,2))
selection<- rbind(selection,expand.grid(temp=0,stage=seq(-2,2,by=0.5),loc=c(1,2)))
mod_dat2<- mod_dat
mod_dat2$n_sel<- nrow(selection)
mod_dat2$select<- selection
inits<-function(){
    list('beta1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'beta2'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Intercept'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Beta_stage'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Beta_temp'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE))
   }
out <- jags(data=mod_dat2,
	inits=inits,
	parameters=params,	
	model.file=mod_03_gof,
	n.chains = 3,	
	n.iter = 75000,	
	n.burnin = 30000,   
	n.thin=2,
	working.directory=getwd())
save(out, file="./output/out-model-03-gof.Rdata")

### END 3






# MODEL 1: TEMPERATURE ONLY
params<- c("beta1",'beta2','Intercept',
	'Beta_temp','p') # THINGS TO KEEP TRACK OF
inits<-function(){
    list(beta1=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	beta2=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	Intercept=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	Beta_temp=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE))

   }
out <- jags(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_01,
	n.chains = 3,	
	n.iter = 75000,	
	n.burnin = 30000,   
	n.thin=2,
	working.directory=getwd())
save(out, file="./output/out-model-01.Rdata")


  # if the model does not converge, update it!
 # jagsfit.upd <- update(jagsfit, n.iter=100)
  #print(jagsfit.upd)
 # print(jagsfit.upd, intervals=c(0.025, 0.5, 0.975))
  #plot(jagsfit.upd)



# MODEL 2: STAGE ONLY
params<- c("beta1",'beta2','Intercept',
	'Beta_stage','p') # THINGS TO KEEP TRACK OF
inits<-function(){
    list(beta1=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	beta2=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	Intercept=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	Beta_stage=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE))
   }
out <- jags.parallel(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_02,
	n.chains = 3,	
	n.iter = 75000,	
	n.burnin = 30000,    
	n.thin=2,
	working.directory=getwd())
save(out, file="./output/out-model-02.Rdata")

  
# MODEL 3: TEMPERATURE AND STAGE
params<- c("beta1",'beta2','Intercept',
	'Beta_stage','Beta_temp','p') 
	
inits<-function(){
    list('beta1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'beta2'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Intercept'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Beta_stage'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Beta_temp'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE))
   }
out <- jags.parallel(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_03,
	n.chains = 3,	
	n.iter = 75000,	
	n.burnin = 30000,   
	n.thin=2,
	working.directory=getwd())
save(out, file="./output/out-model-03.Rdata")

# MODEL 4: TEMPERATURE, STAGE, AND INTERACTION
params<- c("beta1",'beta2','Intercept',
	'Beta_stage','Beta_temp','Beta_int','p')
inits<-function()
	{
    list('beta1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'beta2'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Intercept'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Beta_stage'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE), 
	'Beta_temp'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE), 
	'Beta_int'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE))
	}
out <- jags.parallel(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_04,
	n.chains = 3,	
	n.iter = 75000,	
	n.burnin = 30000,   
	n.thin=2,
	working.directory=getwd())
save(out, file="./output/out-model-04.Rdata")


# MODEL 5: TEMPERATURE ONLY
params<- c("beta1",'beta2','Intercept',
	'Beta_temp','p') # THINGS TO KEEP TRACK OF
inits<-function(){
    list('beta1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'beta2'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Intercept'=c(NA,runif(6)),
	'Beta_temp'=c(NA,runif(6)))
 
   }
out <- jags.parallel(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_05,
	n.chains = 3,	
	n.iter = 75000,	
	n.burnin = 30000,   
	n.thin=2,
	working.directory=getwd())
save(out, file="./output/out-model-05.Rdata")

# MODEL 6: STAGE ONLY
params<- c("beta1",'beta2','Intercept',
	'Beta_stage','p') # THINGS TO KEEP TRACK OF
inits<-function(){
    list('beta1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'beta2'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Intercept'=c(NA,runif(6)),
	'Beta_stage'=c(NA,runif(6)))
   }
out <- jags.parallel(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_06,
	n.chains = 3,	
	n.iter = 75000,	
	n.burnin = 30000,    
	n.thin=2,
	working.directory=getwd())
save(out, file="./output/out-model-06.Rdata")

# MODEL 7: TEMPERATURE AND STAGE
params<- c("beta1",'beta2','Intercept',
	'Beta_stage','Beta_temp','p') 
inits<-function(){
    list('beta1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'beta2'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Intercept'=c(NA,runif(6)),
	'Beta_stage'=c(NA,runif(6)), 
	'Beta_temp'=c(NA,runif(6)))
   }
out <- jags.parallel(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_07,
	n.chains = 3,	
	n.iter = 75000,	
	n.burnin = 30000,     
	n.thin=2,
	working.directory=getwd())
save(out, file="./output/out-model-07.Rdata")

# MODEL 8: TEMPERATURE, STAGE, AND INTERACTION
params<- c("beta1",'beta2','Intercept',
	'Beta_stage','Beta_temp','Beta_int','p')
inits<-function(){
    list('beta1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'beta2'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Intercept'=c(NA,runif(6)),
	'Beta_stage'=c(NA,runif(6)), 
	'Beta_temp'=c(NA,runif(6)), 
	'Beta_int'=c(NA,runif(6)))
  }
out <- jags.parallel(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_08,
	n.chains = 3,	
	n.iter = 75000,	
	n.burnin = 30000,    
	n.thin=2,
	working.directory=getwd())
save(out, file="./output/out-model-08.Rdata")