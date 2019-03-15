

#######################################################################
#
#  COMBINE CATFISH POINT AND VICKSBURG AVAILABILITY INTO A SINGLE DATASE
#
#######################################################################

# THIS IS SETTING THE STAGE TO BE ABLE TO RUN 
# CATFISH POINT AND VICKSBURG AS 1 ANALYSIS
avail<- rbind(cp_avail,vb_avail) 
dat$loc<- ifelse(dat$Study.Site=="CP",1,2)

dat$date<- as.Date(as.character(dat$date),"%m/%d/%Y")
dat<- dat[order(dat$date),]
   
   
#######################################################################
#
#  CENTER CONTINUOUS VARIABLES
#
#######################################################################
stage_mn<-mean(dat$Stage.m)
stage_sd<-sd(dat$Stage.m)

temp_mn<- mean(dat$TempC)
temp_sd<- sd(dat$TempC)

depth_mn<- mean(na.omit(dat$depth))
depth_sd<- sd(na.omit(dat$depth))

kph_mn<- mean(na.omit(dat$kph))
kph_sd<- sd(na.omit(dat$kph))



#######################################################################
#
#  MAKE UP DATA FOR JAGS
#
#######################################################################
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
    XX=XX,
    hab=XX[,6])

mod_dat$X[,2]<- scale(mod_dat$X[,2],stage_mn,stage_sd) # SCALE STAGE TO LINK TO AVIALBILITY MODEL


obs_p<- mod_dat$areas/ mod_dat$total
obs_p<- cbind(as.data.frame(mod_dat$X),obs_p)
	

#######################################################################
#
#  PREDICTION DATASET
#
#######################################################################

## CATFISH POINT & VICSBURG TEMPERATURE
temps<-scale(seq(1,33,by=1),temp_mn,temp_sd)
selection<- expand.grid(temp=temps,stage=0,loc=c(1,2))

## CATFISH POINT & VICSBURG STAGE
xx<-seq(-2,18,by=1)
stages<- c(scale(xx,stage_mn,stage_sd),
    scale(xx,stage_mn,stage_sd))
loc<-sort(rep(c(1,2),length(xx)))    
selection<- rbind(selection,data.frame(temp=0,stage=stages,loc=loc))
mod_dat2<- mod_dat
mod_dat2$n_sel<- nrow(selection)
mod_dat2$select<- selection	



#######################################################################
#
#  PREDICTED RESPONSES
#
#######################################################################

load("_output/out-model-03-gof.Rdata")

## CATFISH POINT & VICSBURG TEMPERATURE
temps<-scale(seq(1,33,by=1),temp_mn,temp_sd)
selection<- expand.grid(temp=temps,stage=0,loc=c(1,2))

## CATFISH POINT & VICSBURG STAGE
xx<-seq(-2,18,by=1)
stages<- c(scale(xx,stage_mn,stage_sd),
    scale(xx,stage_mn,stage_sd))
loc<-sort(rep(c(1,2),length(xx)))    
selection<- rbind(selection,data.frame(temp=0,stage=stages,loc=loc))
selection$temp_raw<- selection$temp*as.numeric(attributes(temps)[3])+as.numeric(attributes(temps)[2])
stages<- scale(xx,stage_mn,stage_sd)
selection$stage_raw<- selection$stage*as.numeric(attributes(stages)[3])+as.numeric(attributes(stages)[2])
pdat<- selection
pdat$habId<-1
for(i in 2:7)
    {
    app<- selection
    app$habId<-i
    pdat<-rbind(pdat,app)
    }
indx<-grep("sel",colnames(out$BUGSoutput$sims.matrix))
pdat$lower<-apply(out$BUGSoutput$sims.matrix[,indx],2,quantile,prob=0.025)
pdat$selectivity<-apply(out$BUGSoutput$sims.matrix[,indx],2,mean)
pdat$upper<-apply(out$BUGSoutput$sims.matrix[,indx],2,quantile,prob=0.975)

pdat<- subset(pdat, stage_raw >=-2 & stage_raw <=12)
pdat<- subset(pdat, temp_raw >=3 & temp_raw <=32)