



# COMBINE CATFISH POINT AND VICKSBURG INTO A SINGLE DATASE
avail<- rbind(cp_avail,vb_avail) # THIS IS SETTING THE STAGE TO BE ABLE TO RUN CATFISH POINT AND VICKSBURG AS 1 ANALYSIS
dat$loc<- ifelse(dat$Study.Site=="CP",1,2)

dat$date<- as.Date(as.character(dat$date),"%m/%d/%Y")
dat<- dat[order(dat$date),]
   
stage_mn<-mean(dat$Stage.m)
stage_sd<-sd(dat$Stage.m)

temp_mn<- mean(dat$TempC)
temp_sd<- sd(dat$TempC)

depth_mn<- mean(na.omit(dat$depth))
depth_sd<- sd(na.omit(dat$depth))

kph_mn<- mean(na.omit(dat$kph))
kph_sd<- sd(na.omit(dat$kph)   )


## HABITAT OBSERVED DATA AND COVARIATES
XX=as.matrix(cbind(
    dat$loc,
    scale(dat$Stage.m,stage_mn,stage_sd),    
    scale(dat$TempC,temp_mn,temp_sd),
    scale(dat$depth,depth_mn,depth_sd),
    scale(dat$kph,kph_mn,kph_sd),        
    dat$habitat))
XX<-XX[complete.cases(XX),]
# BUNDLE UP DATA
mod_dat<- list(total=apply(avail[,-c(1,2)],1,sum),
	X=as.matrix(avail[,c(1,2)]),
	areas=as.matrix(avail[,-c(1,2)]),
	nhabs=7,
	nobs1=nrow(avail),
	# HABITAT SELCTION DATA
	nobs2=nrow(XX),
    XX=XX)

mod_dat$X[,2]<- scale(mod_dat$X[,2],stage_mn,stage_sd) # SCALE STAGE TO LINK TO AVIALBILITY MODEL


obs_p<- mod_dat$areas/ mod_dat$total
obs_p<- cbind(as.data.frame(mod_dat$X),obs_p)
		