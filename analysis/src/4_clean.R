



# COMBINE CATFISH POINT AND VICKSBURG INTO A SINGLE DATASE
avail<- rbind(cp_avail,vb_avail) # THIS IS SETTING THE STAGE TO BE ABLE TO RUN CATFISH POINT AND VICKSBURG AS 1 ANALYSIS
dat$loc<- ifelse(dat$Study.Site=="CP",1,2)


   
# BUNDLE UP DATA
mod_dat<- list(total=apply(avail[,-c(1,2)],1,sum),
	X=as.matrix(avail[,c(1,2)]),
	areas=as.matrix(avail[,-c(1,2)]),
	nhabs=7,
	nobs1=nrow(avail),
	# HABITAT SELCTION DATA
	nobs2=nrow(dat), 
	hab=dat$habitat,
	XX=as.matrix(cbind(
		(dat$TempC-mean(dat$TempC))/sd(dat$TempC),
		(dat$Stage.m-mean(dat$Stage.m))/sd(dat$Stage.m),
		dat$loc)))
mod_dat$X[,2]<- (mod_dat$X[,2]-mean(dat$Stage.m))/sd(dat$Stage.m) # SCALE STAGE TO LINK TO AVIALBILITY MODEL


obs_p<- mod_dat$areas/ mod_dat$total
obs_p<- cbind(as.data.frame(mod_dat$X),obs_p)
		