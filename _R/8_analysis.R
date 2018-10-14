ni<-50*500
nb<-20*500


#######################################################################
#
#  Model 01: STAGE ONLY
#
#######################################################################
params<- c("beta1",'beta2','Intercept',
	'Beta_1','p') # THINGS TO KEEP TRACK OF
inits<-function(){
    list('beta1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'beta2'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Intercept'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Beta_1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE))
   }
out <- jags.parallel(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_01,
	n.chains = 3,	
	n.iter = ni,n.burnin = nb,   
	n.thin=2, 
    export_obj_names=c("ni","nb"),
	working.directory=getwd())
save(out, file="_output/out-model-01.Rdata")
print("model 1 completed")



# if the model does not converge, update it!
# jagsfit.upd <- update(jagsfit, n.iter=100)
# print(jagsfit.upd)
# print(jagsfit.upd, intervals=c(0.025, 0.5, 0.975))
# plot(jagsfit.upd)

#######################################################################
#
#  MODEL 2: TEMPERATURE ONLY
#
#######################################################################
params<- c("beta1",'beta2','Intercept',
	'Beta_1','p') # THINGS TO KEEP TRACK OF
inits<-function(){
    list('beta1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'beta2'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Intercept'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Beta_1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE))
   }
out <- jags.parallel(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_02,
	n.chains = 3,	
	n.iter = ni,n.burnin = nb,     
	n.thin=2, 
    export_obj_names=c("ni","nb"),
	working.directory=getwd())
save(out, file="_output/out-model-02.Rdata")
print("model 2 completed")


#######################################################################
#
#  MODEL 3: DEPTH MODEL
#
#######################################################################
params<- c("beta1",'beta2','Intercept',
	'Beta_1','p') # THINGS TO KEEP TRACK OF
inits<-function(){
    list('beta1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'beta2'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Intercept'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Beta_1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE))
   }
out<-NULL
out <- jags.parallel(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_03,
	n.chains = 3,	
	n.iter = ni,n.burnin = nb,  
	n.thin=2, 
    export_obj_names=c("ni","nb"),
	working.directory=getwd())
save(out, file="_output/out-model-03.Rdata")
print("model 3 completed")





#######################################################################
#
#  MODEL 4: KPH
#
#######################################################################
params<- c("beta1",'beta2','Intercept',
	'Beta_stage','Beta_temp','Beta_int',
    'p')
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
	n.iter = ni,n.burnin = nb,     
	n.thin=2, 
    export_obj_names=c("ni","nb"),
	working.directory=getwd())
save(out, file="_output/out-model-04.Rdata")
print("model 4 completed")



#######################################################################
#
# MODEL 5: TEMPERATURE ONLY 
#
#######################################################################
params<- c("beta1",'beta2','Intercept',
	'Beta_temp','p') # THINGS TO KEEP TRACK OF
inits<-function(){
    list('beta1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'beta2'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Intercept'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Beta_temp'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE)) 
   }
out <- jags.parallel(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_05,
	n.chains = 3,	
	n.iter = ni,n.burnin = nb,     
	n.thin=2, 
    export_obj_names=c("ni","nb"),
	working.directory=getwd())
save(out, file="_output/out-model-05.Rdata")
print("model 5 completed")



#######################################################################
#
#  MODEL 6: STAGE ONLY
#
#######################################################################
params<- c("beta1",'beta2','Intercept',
	'Beta_stage','p') # THINGS TO KEEP TRACK OF
inits<-function(){
    list('beta1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'beta2'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Intercept'=c(NA,runif(6)),
	'Beta_stage'=c(NA,runif(6)))
   }
out<-NULL
out <- jags.parallel(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_06,
	n.chains = 3,	
	n.iter = ni,n.burnin = nb,     
	n.thin=2, 
    export_obj_names=c("ni","nb"),
	working.directory=getwd())
save(out, file="_output/out-model-06.Rdata")
print("model 6 completed")



#######################################################################
#
# MODEL 7: TEMPERATURE AND STAGE 
#
#######################################################################
params<- c("beta1",'beta2','Intercept',
	'Beta_stage','Beta_temp','p') 
inits<-function(){
    list('beta1'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'beta2'=matrix(c(NA,NA,runif(2*6)),2,7,byrow=FALSE),
	'Intercept'=c(NA,runif(6)),
	'Beta_stage'=c(NA,runif(6)), 
	'Beta_temp'=c(NA,runif(6)))
   }
out<-NULL
out <- jags.parallel(data=mod_dat,
	inits=inits,
	parameters=params,	
	model.file=mod_07,
	n.chains = 3,	
	n.iter = ni,n.burnin = nb,     
	n.thin=2, 
    export_obj_names=c("ni","nb"),
	working.directory=getwd())
save(out, file="_output/out-model-07.Rdata")
print("model 7 completed")


#######################################################################
#
# MODEL 8: TEMPERATURE, STAGE, AND INTERACTION 
#
#######################################################################
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
	n.iter = ni,n.burnin = nb,     
	n.thin=2, 
    export_obj_names=c("ni","nb"),
	working.directory=getwd())
save(out, file="_output/out-model-08.Rdata")
print("model 8 completed")

