figures<- function(n){
if(n==1)
	{ # PLOT OF TEMPERATURE AT CATFISH POINT
	plotDat2<- subset(dat,loc==1)

	par(mfrow=c(2,1),mar=c(1,4,0,1),oma=c(3,1,1,1),cex.lab=1.3)
	plot(TempC~date,plotDat2,type="b",col="black",xlab="",ylab="Temperature (C)",
		pch=19,main="",xaxt='n',las=1)
	axis(2,at=axTicks(1),labels=FALSE)
	plot(Stage.m~date,plotDat2,type="b",col="black",xlab="",ylab="Stage (M)",
		pch=19,main="",las=1)
	mtext(side=1,"Date",outer=TRUE, line=1.5,cex=1.3)
	}
if(n==2)
	{ # PLOT OF TEMPERATURE AT TARA TO VICKSBURG
	plotDat2<- subset(dat,loc==2)
	par(mfrow=c(2,1),mar=c(1,4,0,1),oma=c(3,1,1,1),cex.lab=1.3)
	plot(TempC~date,plotDat2,type="b",col="black",xlab="",ylab="Temperature (C)",
		pch=19,main="",xaxt='n',las=1)
	axis(2,at=axTicks(1),labels=FALSE)
	plot(Stage.m~date,plotDat2,type="b",col="black",xlab="",ylab="Stage (M)",
		pch=19,main="",las=1)
	mtext(side=1,"Date",outer=TRUE, line=1.5,cex=1.3)
	}
if(n==3)
	{# PLOT OF ESTIMATED AND OBSERVED AVAILABILITY
	obs_p<- mod_dat$areas/ mod_dat$total
	p_hat<-M03$BUGSoutput$mean$p
	plot(p_hat,obs_p)
	abline(0,1)
	}
if(n==4)
	{# PLOT PREDCITED AVAILABILITY WITH STAGE for CATFISH POINT AND TARA-VICKBURG
	b0<- M03$BUGSoutput$mean$beta1
	b1<-M03$BUGSoutput$mean$beta2
	color<- brewer.pal(7,"Set2")
	## CATFISH POINT
	xx<- data.frame(stage=seq(-2,10,0.1))
	## SCALE STAGE 
	xx$stage_scaled<- scale(xx$stage, center = mean(dat$Stage.m), scale = sd(dat$Stage.m))
	
	## ESTIMATE PROPORATION AVAILABLE
	y<-sapply(1:nrow(xx),function(x)
		{
		y<- c(b0[1,]) +c(b1[1,])*xx$stage_scaled[x]
		p<- exp(y)/sum(exp(y))
		return(p)
		})
	y<- t(y) # PROPORTION AVAILABLE

	y_cum<-sapply(1:nrow(xx),function(x)
		{
		y<- c(b0[1,]) +c(b1[1,])*xx$stage_scaled[x]
		p<- exp(y)/sum(exp(y))
		p<-cumsum(p)
		return(p)
		})
	y_cum<- t(y_cum)


	par(mfrow=c(2,1),mar=c(3,3,1,1),oma=c(2,2,1,1))
	matplot(xx$stage,y_cum,type='n',las=1,xlab="",
		ylab="",
		main="Catfish point",xaxt='n',ylim=c(0,1))
		axis(1,at=axTicks(1))
	for(i in 7:1)
		{
		polygon(c(xx$stage,rev(xx$stage)),
			c(y_cum[,i],rep(0,nrow(xx))),
			col=color[i])		
		}
	#matpoints(obs_p[obs_p$loc==1,]$Stage,obs_p[obs_p$loc==1,-c(1,2)],pch=1,type='p')
	mtext(side=2, "Proportion available",outer=TRUE, line=0)
	mtext(side=1, "Stage",outer=TRUE, line=0)
	
	
	## TARA TO VICKSBURG
	xx<- data.frame(stage=seq(0,14,0.1))
	## SCALE STAGE 
	xx$stage_scaled<- scale(xx$stage, center = mean(dat$Stage.m), scale = sd(dat$Stage.m))
	
	## ESTIMATE PROPORATION AVAILABLE
	y<-sapply(1:nrow(xx),function(x)
		{
		y<- c(b0[1,]) +c(b1[1,])*xx$stage_scaled[x]
		p<- exp(y)/sum(exp(y))
		return(p)
		})
	y<- t(y) # PROPORTION AVAILABLE

	y_cum<-sapply(1:nrow(xx),function(x)
		{
		y<- c(b0[1,]) +c(b1[1,])*xx$stage_scaled[x]
		p<- exp(y)/sum(exp(y))
		p<-cumsum(p)
		return(p)
		})
	y_cum<- t(y_cum)
	matplot(xx$stage,y_cum,type='n',las=1,xlab="",
		ylab="",main="Tara to Vicksburg",xaxt='n',ylim=c(0,1))
	axis(1,at=axTicks(1))
	for(i in 7:1)
		{
		polygon(c(xx$stage,rev(xx$stage)),
			c(y_cum[,i],rep(0,nrow(xx))),
			col=color[i])		
		}
	legend(7,0.15,legend=c('MC','SND','WD','NAT','REV','SC','ILT'),
		fill=color, horiz=TRUE,cex=0.8,bg="white",xjust=0.5)	
	}
if(n==5)
	{# HABITAT SELECTION FOR CATFISH POINT: TEMPERATURE
	# temp -1.6,1.6
	# stage -2.2,2
	preddat<- expand.grid(temp=sort(c(seq(4,32,0.25),temp_mn)),
		stage=sort(c(seq(-2,12,0.1),stage_mn)),loc=1)
	preddat$temp_scaled<- scale(preddat$temp, center = temp_mn, scale = temp_sd)
	preddat$stage_scaled<- scale(preddat$stage, center =stage_mn, scale = stage_sd)
	
	b0<- M03$BUGSoutput$mean$beta1
	b1<-M03$BUGSoutput$mean$beta2
	Intercept<- M03$BUGSoutput$mean$Intercept
	Beta_temp<-M03$BUGSoutput$mean$Beta_temp
	Beta_stage<-M03$BUGSoutput$mean$Beta_stage

	##
	newdat<- subset(preddat, stage_scaled==0)
	## EFFECT OF TEMPERATURE AT AVERAGE STAGE
	y<-sapply(1:nrow(newdat),function(x)
		{
		loc<- newdat$loc[x]
		# AVAILABILITY GIVEN STAGE
		y<- c(b0[loc,]) +
			c(b1[loc,])*newdat$stage_scaled[x]
		avail<- exp(y)/sum(exp(y))
		
		# HABITAT SELECTION
		y<- c(Intercept[loc,]) +
			c(Beta_temp[loc,])*newdat$temp_scaled[x]+
			c(Beta_stage[loc,])*newdat$stage_scaled[x]+
			log(avail)
		s<- exp(y)/sum(exp(y))
		return(s)
		})
	y_cum<- t(apply(y,2,cumsum)	)		
	y<- t(y)

	color<- brewer.pal(7,"Set2")
	par(mfrow=c(2,1),mar=c(1,3,1,1),oma=c(4,2,1,1),cex.lab=1.3)
	matplot(newdat$temp,y,type='l',ylim=c(0,0.4),las=1, ylab="",
		main="",col=color,lty=1,lwd=3,xaxt='n')
	axis(1,at=axTicks(1),labels=FALSE)
	legend("topright",legend=c('MC','SND','WD','NAT','REV','SC','ILT'),
		lwd=4,lty=1,col=color,bty='n', horiz=TRUE,cex=0.8,xjust=0.5)	
	
	matplot(newdat$temp,y_cum,type='n',ylim=c(0,1),las=1, ylab="",
		main="",col=color,lty=1,lwd=2)

	for(i in 7:1)
		{
		x<- c(newdat$temp,rev(newdat$temp))
		y<- c(rep(0,nrow(y_cum)),rev(y_cum[,i]))
		polygon(x,y,col=color[i])		
		}
	mtext(side=3, "Catfish Point", line=-0.75,outer=TRUE,cex=1.5)
	mtext(side=1, "Temperature", line=1.5,outer=TRUE,cex=1.3)
	mtext(side=2, "Probability of use", line=-0.75,outer=TRUE,cex=1.3)
	}
if(n==6)
	{# HABITAT SELECTION FOR CATFISH POINT: STAGE
	# temp -1.6,1.6
	# stage -2.2,2
	preddat<- expand.grid(temp=sort(c(seq(4,32,0.25),temp_mn)),
		stage=sort(c(seq(-2,12,0.1),stage_mn)),loc=1)
	preddat$temp_scaled<- scale(preddat$temp, center = temp_mn, scale = temp_sd)
	preddat$stage_scaled<- scale(preddat$stage, center =stage_mn, scale = stage_sd)
	
	b0<- M03$BUGSoutput$mean$beta1
	b1<-M03$BUGSoutput$mean$beta2
	Intercept<- M03$BUGSoutput$mean$Intercept
	Beta_temp<-M03$BUGSoutput$mean$Beta_temp
	Beta_stage<-M03$BUGSoutput$mean$Beta_stage

	##
	newdat<- subset(preddat, temp_scaled==0)
	## EFFECT OF TEMPERATURE AT AVERAGE STAGE
	y<-sapply(1:nrow(newdat),function(x)
		{
		loc<- newdat$loc[x]
		# AVAILABILITY GIVEN STAGE
		y<- c(b0[loc,]) +
			c(b1[loc,])*newdat$stage_scaled[x]
		avail<- exp(y)/sum(exp(y))
		
		# HABITAT SELECTION
		y<- c(Intercept[loc,]) +
			c(Beta_temp[loc,])*newdat$temp_scaled[x]+
			c(Beta_stage[loc,])*newdat$stage_scaled[x]+
			log(avail)
		s<- exp(y)/sum(exp(y))
		return(s)
		})
	y_cum<- t(apply(y,2,cumsum)	)		
	y<- t(y)

	color<- brewer.pal(7,"Set2")
	par(mfrow=c(2,1),mar=c(1,3,1,1),oma=c(4,2,1,1),cex.lab=1.3)
	matplot(newdat$stage,y,type='l',ylim=c(0,0.8),las=1, ylab="",
		main="",col=color,lty=1,lwd=3,xaxt='n')
	axis(1,at=axTicks(1),labels=FALSE)
	legend("topright",legend=c('MC','SND','WD','NAT','REV','SC','ILT'),
		lwd=4,lty=1,col=color,bty='n', horiz=TRUE,cex=0.8,xjust=0.5)	
	
	matplot(newdat$stage,y_cum,type='n',ylim=c(0,1),las=1, ylab="",
		main="",col=color,lty=1,lwd=2)

	for(i in 7:1)
		{
		x<- c(newdat$stage,rev(newdat$stage))
		y<- c(rep(0,nrow(y_cum)),rev(y_cum[,i]))
		polygon(x,y,col=color[i])		
		}
		

	mtext(side=3, "Catfish Point", line=-0.75,outer=TRUE,cex=1.5)
	mtext(side=1, "Stage", line=1.5,outer=TRUE,cex=1.3)
	mtext(side=2, "Probability of use", line=-0.75,outer=TRUE,cex=1.3)
	}
if(n==7)
	{# HABITAT SELECTION FOR TARA TO VICKSBURG: TEMPERATURE
	preddat<- expand.grid(temp=sort(c(seq(4,30,0.25),temp_mn)),
		stage=sort(c(seq(0,14,0.1),stage_mn)),loc=2)
	preddat$temp_scaled<- scale(preddat$temp, center = temp_mn, scale = temp_sd)
	preddat$stage_scaled<- scale(preddat$stage, center =stage_mn, scale = stage_sd)
	
	b0<- M03$BUGSoutput$mean$beta1
	b1<-M03$BUGSoutput$mean$beta2
	Intercept<- M03$BUGSoutput$mean$Intercept
	Beta_temp<-M03$BUGSoutput$mean$Beta_temp
	Beta_stage<-M03$BUGSoutput$mean$Beta_stage

	##
	newdat<- subset(preddat, stage_scaled==0)
	## EFFECT OF TEMPERATURE AT AVERAGE STAGE
	y<-sapply(1:nrow(newdat),function(x)
		{
		loc<- newdat$loc[x]
		# AVAILABILITY GIVEN STAGE
		y<- c(b0[loc,]) +
			c(b1[loc,])*newdat$stage_scaled[x]
		avail<- exp(y)/sum(exp(y))
		
		# HABITAT SELECTION
		y<- c(Intercept[loc,]) +
			c(Beta_temp[loc,])*newdat$temp_scaled[x]+
			c(Beta_stage[loc,])*newdat$stage_scaled[x]+
			log(avail)
		s<- exp(y)/sum(exp(y))
		return(s)
		})
	y_cum<- t(apply(y,2,cumsum)	)		
	y<- t(y)
	color<- brewer.pal(7,"Set2")
	par(mfrow=c(2,1),mar=c(1,3,1,1),oma=c(4,2,1,1),cex.lab=1.3)
	matplot(newdat$temp,y,type='l',ylim=c(0,0.7),las=1, ylab="",
		main="",col=color,lty=1,lwd=3,xaxt='n')
	axis(1,at=axTicks(1),labels=FALSE)
	legend("topright",legend=c('MC','SND','WD','NAT','REV','SC','ILT'),
		lwd=4,lty=1,col=color,bty='n', horiz=TRUE,cex=0.8,xjust=0.5)	

	matplot(newdat$temp,y_cum,type='n',ylim=c(0,1),las=1, ylab="",
		main="",col=color,lty=1,lwd=2)

	for(i in 7:1)
		{
		x<- c(newdat$temp,rev(newdat$temp))
		y<- c(rep(0,nrow(y_cum)),rev(y_cum[,i]))
		polygon(x,y,col=color[i])		
		}
	mtext(side=3, "TARA TO VICKSBURG", line=-0.75,outer=TRUE,cex=1.5)
	mtext(side=1, "Temperature", line=1.5,outer=TRUE,cex=1.3)
	mtext(side=2, "Probability of use", line=-0.5,outer=TRUE,cex=1.3)
	}
if(n==8)
	{# HABITAT SELECTION FOR TARA TO VICKSBURG: STAGE
	preddat<- expand.grid(temp=sort(c(seq(4,32,0.25),temp_mn)),
		stage=sort(c(seq(-2,12,0.1),stage_mn)),loc=2)
	preddat$temp_scaled<- scale(preddat$temp, center = temp_mn, scale = temp_sd)
	preddat$stage_scaled<- scale(preddat$stage, center =stage_mn, scale = stage_sd)
	
	b0<- M03$BUGSoutput$mean$beta1
	b1<-M03$BUGSoutput$mean$beta2
	Intercept<- M03$BUGSoutput$mean$Intercept
	Beta_temp<-M03$BUGSoutput$mean$Beta_temp
	Beta_stage<-M03$BUGSoutput$mean$Beta_stage

	##
	newdat<- subset(preddat, temp_scaled==0)
	## EFFECT OF TEMPERATURE AT AVERAGE STAGE
	y<-sapply(1:nrow(newdat),function(x)
		{
		loc<- newdat$loc[x]
		# AVAILABILITY GIVEN STAGE
		y<- c(b0[loc,]) +
			c(b1[loc,])*newdat$stage_scaled[x]
		avail<- exp(y)/sum(exp(y))
		
		# HABITAT SELECTION
		y<- c(Intercept[loc,]) +
			c(Beta_temp[loc,])*newdat$temp_scaled[x]+
			c(Beta_stage[loc,])*newdat$stage_scaled[x]+
			log(avail)
		s<- exp(y)/sum(exp(y))
		return(s)
		})
	y_cum<- t(apply(y,2,cumsum)	)		
	y<- t(y)

	color<- brewer.pal(7,"Set2")
	par(mfrow=c(2,1),mar=c(1,3,1,1),oma=c(4,2,1,1),cex.lab=1.3)
	matplot(newdat$stage,y,type='l',ylim=c(0,0.9),las=1, ylab="",
		main="",col=color,lty=1,lwd=3,xaxt='n')
	axis(1,at=axTicks(1),labels=FALSE)
	legend("topright",legend=c('MC','SND','WD','NAT','REV','SC','ILT'),
		lwd=4,lty=1,col=color,bty='n', horiz=TRUE,cex=0.8,xjust=0.5)	
	
	matplot(newdat$stage,y_cum,type='n',ylim=c(0,1),las=1, ylab="",
		main="",col=color,lty=1,lwd=2)

	for(i in 7:1)
		{
		x<- c(newdat$stage,rev(newdat$stage))
		y<- c(rep(0,nrow(y_cum)),rev(y_cum[,i]))
		polygon(x,y,col=color[i])		
		}
		
	mtext(side=3, "TARA TO VICKSBURG", line=-0.75,outer=TRUE,cex=1.5)
	mtext(side=1, "Stage", line=1.5,outer=TRUE,cex=1.3)
	mtext(side=2, "Probability of use", line=-0.75,outer=TRUE,cex=1.3)
	}

}
