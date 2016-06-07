figures<- function(n){

if(n==1)
	{# PLOT OF ESTIMATED AND OBSERVED AVAILABILITY
	obs_p<- mod_dat$areas/ mod_dat$total
	p_hat<-M03$BUGSoutput$mean$p
	plot(p_hat,obs_p)
	abline(0,1)
	}
if(n==2)
	{# PLOT AVAILABILITY WITH STAGE
	b0<- out$BUGSoutput$mean$beta1
	b1<-out$BUGSoutput$mean$beta2


	## CATFISH POINT
	cp<- as.data.frame(mod_dat$X)
	cp<- subset(cp,loc==1)
	xx<- seq(min(cp$Stage),max(cp$Stage),0.1)

	y<-sapply(1:length(xx),function(x)
		{
		y<- c(b0[1,]) +c(b1[1,])*xx[x]
		p<- exp(y)/sum(exp(y))
		p<-cumsum(p)	
		return(p)
		})
	y<- t(y)

	y_cum<-sapply(1:length(xx),function(x)
		{
		y<- c(b0[1,]) +c(b1[1,])*xx[x]
		p<- exp(y)/sum(exp(y))
		p<-cumsum(p)
		return(p)
		})
	y_cum<- t(y_cum)


	par(mfrow=c(2,1),mar=c(3,3,1,1),oma=c(2,2,1,1))
	matplot(xx,y_cum,type='n',las=1,xlab="",
		ylab="",
		main="Catfish point",xaxt='n',ylim=c(0,1))
		axis(1,at=axTicks(1))
	for(i in 7:1)
		{
		polygon(c(xx,rev(xx)),
			c(y_cum[,i],rep(0,length(xx))),
			col=i)		
		}
	#matpoints(obs_p[obs_p$loc==1,]$Stage,obs_p[obs_p$loc==1,-c(1,2)],pch=1,type='p')
	mtext(side=2, "Proportion available",outer=TRUE, line=0)
	mtext(side=1, "Stage",outer=TRUE, line=0)
	# TARA TO VICKSBURG
	cp<- as.data.frame(mod_dat$X)
	cp<- subset(cp,loc==2)
	xx<- seq(min(cp$Stage),max(cp$Stage),0.1)

	y_cum<-sapply(1:length(xx),function(x)
		{
		y<- c(b0[2,]) +c(b1[2,])*xx[x]
		p<- exp(y)/sum(exp(y))
		p<-cumsum(p)
		return(p)
		})
	y_cum<- t(y_cum)


	matplot(xx,y_cum,type='n',las=1,xlab="Stage",
		ylab="Proportion available",
		main="Tara to Vicksburg",ylim=c(0,1))
	for(i in 7:1)
		{
		polygon(c(xx,rev(xx)),
			c(y_cum[,i],rep(0,length(xx))),
			col=i)		
		}
		
		
	## PROPORTION AVAILABILE
	# CATFISH POINT
	cp<- as.data.frame(mod_dat$X)
	cp<- subset(cp,loc==1)
	xx<- seq(min(cp$Stage),max(cp$Stage),0.1)

	y<-sapply(1:length(xx),function(x)
		{
		y<- c(b0[1,]) +c(b1[1,])*xx[x]
		p<- exp(y)/sum(exp(y))
		return(p)
		})
	y<- t(y)

	matplot(xx,y,type='l',las=1,xlab="Stage",
		ylab="Proportion available",lwd=2,
		main="Catfish Point",ylim=c(0,1))
	legend("top", habs,col=c(1:7), lty=c(1:7),horiz=TRUE,
		cex=0.75,lwd=2,bty='n')

	matpoints(obs_p[obs_p$loc==1,]$Stage,
		obs_p[obs_p$loc==1,-c(1,2)],pch=19,type='p')
	}
if(n==3)
	{
	# HABITAT SELECTION 
	# temp -1.6,1.6
	# stage -2.2,2
	preddat<- expand.grid(temp=seq(-1.6,1.6,0.1),
		stage=0,loc=c(0,1))
	loc<-1
	dat$tmp<-1
	yyy<-dcast(dat,loc+date+TempC+Stage.m~habitat,
		value.var="tmp",sum)
	yyy$date<- as.Date(yyy$date,"%m/%d/%Y")
	yyy$temp_std<- (yyy$TempC-mean(dat$TempC))/sd(dat$TempC)	
	yyy<-yyy[order(yyy$loc,yyy$date),]
	
	Intercept<- M03$BUGSoutput$mean$Intercept
	Beta_temp<-M03$BUGSoutput$mean$Beta_temp
	Beta_stage<-M03$BUGSoutput$mean$Beta_stage
		
	y<-sapply(1:nrow(yyy),function(x)
		{
		loc<- yyy$loc[x]
		# AVAILABILITY GIVEN STAGE
		y<- c(b0[loc,]) +
			c(b1[loc,])*yyy$Stage.m[x]
		avail<- exp(y)/sum(exp(y))
		
		# HABITAT SELECTION
		y<- c(Intercept[loc,]) +
			c(Beta_temp[loc,])*yyy$temp_std[x]+
			log(avail)
		s<- exp(y)/sum(exp(y))
		return(s)
		})

	y_cum<- t(apply(y,2,cumsum)	)
	plotDat<- y_cum[yyy$loc==loc,]
	plotDat2<- yyy[yyy$loc==loc,]

	plot(temp_std~date,yyy,type='n',ylim=c(0,1),las=1, ylab="Probability of use",
		main="Catfish point")
	color<- brewer.pal(7,"Set2")
	par(lend=2)
	sapply(1:nrow(plotDat),function(x)
		{
		for(i in 7:1)
			{
			segments(plotDat2$date[x],0,plotDat2$date[x],plotDat[x,i],col=color[i],
				lwd=6)
			}
		})
	 par(xpd=TRUE)
	legend(16000,1.15,legend=c('MC','SND','WD','NAT','REV','SC','ILT'),
		lwd=5,col=color,bty='n', horiz=TRUE,cex=1,xjust=0.5)
		}
	


if(n==3)
	{
	############################################################
	# CATFISH POINT
	############################################################
	habs<-c('MC','SND','WD',
		'NAT','REV','SC','ILT')
	# temp -1.6,1.6
	# stage -2.2,2
	preddat<- expand.grid(temp=seq(-1.6,1.6,0.1),
		Stage=0,loc=c(0,1))
	loc<-1
	color<- brewer.pal(7,"Set2")
	dev.new(width=11.25, height=5.5)

	b0<-M03$BUGSoutput$mean$beta1
	b1<- M03$BUGSoutput$mean$beta2


	## PROPORTION AVAILABILE
	cp<- subset(preddat,loc==1)
	y<-sapply(1:nrow(cp),function(x)
		{
		y<- c(b0[loc,]) + c(b1[loc,])*cp$Stage[x]
		p<- exp(y)/sum(exp(y))
		return(p)
		})
	y<- t(y)

	matplot(cp$Stage,y,type='l',las=1,
		xlab="Stage",
		ylab="Proportion available",lwd=4,
		col=color,lty=1,
		main="Catfish Point",ylim=c(0,0.7))
	legend("top", habs,col=color, lty=1,horiz=TRUE,
		cex=0.95,lwd=5,bty='n')

	#matpoints(obs_p[obs_p$loc==loc,]$Stage,
	#	obs_p[obs_p$loc==loc,-c(1,2)],pch=19,type='p',
	#	col=color)

	# AVAILABILITY: DAILY
	yyy<-dcast(dat,loc+date+TempC+Stage.m~habitat,
		value.var="tmp",sum)
	yyy$date<- as.Date(yyy$date,"%m/%d/%Y")
	yyy$temp_std<- (yyy$TempC-mean(dat$TempC))/sd(dat$TempC)	
	yyy<-yyy[order(yyy$loc,yyy$date),]
	Intercept<- out$BUGSoutput$mean$Intercept
	Beta_temp<-out$BUGSoutput$mean$Beta_temp
		

	y<-sapply(1:nrow(yyy),function(x)
		{
		loc<- yyy$loc[x]
		# AVAILABILITY GIVEN STAGE
		y<- c(b0[loc,]) +
			c(b1[loc,])*yyy$Stage.m[x]
		avail<- exp(y)/sum(exp(y))
		return(avail)
		})

	y_cum<- t(apply(y,2,cumsum)	)
	plotDat<- y_cum[yyy$loc==loc,]
	plotDat2<- yyy[yyy$loc==loc,]

	par(mar=c(4,4,4,4))
	plot(temp_std~date,yyy,type='n',ylim=c(0,1),las=1, ylab="",
		main="Catfish Point",xlab="")
	color<- brewer.pal(7,"Set2")
	par(lend=2)
	sapply(1:nrow(plotDat),function(x)
		{
		for(i in 7:1)
			{
			segments(plotDat2$date[x],0,plotDat2$date[x],plotDat[x,i],col=color[i],
				lwd=6)
			}
		})
	par(xpd=TRUE)
	legend(16000,1.15,legend=c('MC','SND','WD','NAT','REV','SC','ILT'),
		lwd=5,col=color,bty='n', horiz=TRUE,cex=1,xjust=0.5)
	par(new=TRUE)
	plot(Stage.m~date,plotDat2,type="p",col="blue",xaxt="n",yaxt="n",xlab="",ylab="",
		pch=19)
	axis(4,las=2)
	mtext("Stage",side=4,line=2)
	mtext("Date",side=1,line=2)
	mtext("Habitat Availability",side=2,line=2.2)
		
		
		
		
	## PROBABILITY OF USE	
	y<-sapply(1:nrow(cp),function(x)
		{
		loc<- cp$loc[x]
		# AVAILABILITY GIVEN STAGE
		y<- c(b0[loc,]) +
			c(b1[loc,])*mean(dat$Stage.m)
		avail<- exp(y)/sum(exp(y))
		
		# HABITAT SELECTION
		y<- c(Intercept[loc,]) +
			c(Beta_temp[loc,])*cp$temp[x]+
			c(Beta_stage[loc,])*cp$Stage[x]+
			log(avail)
		s<- exp(y)/sum(exp(y))
		return(s)
		})

		
	matplot(cp$temp,t(y),type='l',las=1,lwd=2,
		ylab="Probablity of use",
		ylim=c(0,0.5))
	panLab("Catfish Point")
	
	
	
	
	
	y_cum<- t(apply(y,2,cumsum)	)
	plotDat<- y_cum[cp$loc==loc,]
	plotDat2<- cp[cp$loc==loc,]

	plot(temp_std~date,yyy,type='n',ylim=c(0,1),las=1, ylab="",xlab="",
		main="Catfish Point")

	par(lend=2)
	sapply(1:nrow(plotDat),function(x)
		{
		for(i in 7:1)
			{
			segments(plotDat2$date[x],0,plotDat2$date[x],plotDat[x,i],col=color[i],
				lwd=6)
			}
		})
	 par(xpd=TRUE)
	legend(16000,1.15,legend=c('MC','SND','WD','NAT','REV','SC','ILT'),
		lwd=5,col=color,bty='n', horiz=TRUE,cex=1,xjust=0.5)

	par(new=TRUE)
	plot(TempC~date,plotDat2,type="p",col="blue",xaxt="n",yaxt="n",xlab="",ylab="",
		pch=19)
	axis(4,las=2)
	mtext("Temperature",side=4,line=2)
	mtext("Date",side=1,line=2)
	mtext("Probability of use",side=2,line=2.2)


	plotDat<-t(y)	
	plotDat<- plotDat[yyy$loc==loc,]
	plotDat2<- yyy[yyy$loc==loc,]

	plot(temp_std~date,yyy,type='n',
		ylim=c(0,0.8),las=1, ylab="",xlab="",
		main="Catfish Point")

	par(lend=2)
	sapply(1:7,function(x)
		{
		points(plotDat2$date,plotDat[,x],
			col=color[x],pch=19,type='p')
		})
	 par(xpd=TRUE)
	legend(16000,0.92,legend=c('MC','SND','WD','NAT','REV','SC','ILT'),
		col=color,bty='n', horiz=TRUE,cex=1,xjust=0.5,pch=19)

	par(new=TRUE)
	plot(TempC~date,plotDat2,type="b",col="blue",xaxt="n",yaxt="n",xlab="",ylab="",
		pch=19)
	axis(4,las=2)
	mtext("Temperature",side=4,line=2)
	mtext("Date",side=1,line=2)
	mtext("Probability of use",side=2,line=2.2)
	}

if(n==4)
	{
	############################################################
	# TARA TO VICKSBURG
	############################################################
	loc<-2
	color<- brewer.pal(7,"Set2")
	dev.new(width=11.25, height=5.5)
	## PROPORTION AVAILABILE
	cp<- as.data.frame(mod_dat$X)
	cp<- subset(cp,loc==loc)
	xx<- seq(min(cp$Stage),max(cp$Stage),0.1)
	y<-sapply(1:length(xx),function(x)
		{
		y<- c(b0[loc,]) +c(b1[loc,])*xx[x]
		p<- exp(y)/sum(exp(y))
		return(p)
		})
	y<- t(y)

	matplot(xx,y,type='l',las=1,xlab="Stage",
		ylab="Proportion available",lwd=4,
		col=color,lty=1,
		main="Tara to Vicksburg",ylim=c(0,0.7))
	legend("top", habs,col=color, lty=1,horiz=TRUE,
		cex=0.95,lwd=5,bty='n')

	matpoints(obs_p[obs_p$loc==2,]$Stage,
		obs_p[obs_p$loc==2,-c(1,2)],pch=19,type='p',
		col=color)

	# AVAILABILITY: DAILY
	yyy<-dcast(dat,loc+date+TempC+Stage.m~habitat,
		value.var="tmp",sum)
	yyy$date<- as.Date(yyy$date,"%m/%d/%Y")
	yyy$temp_std<- (yyy$TempC-mean(dat$TempC))/sd(dat$TempC)	
	yyy<-yyy[order(yyy$loc,yyy$date),]
	Intercept<- out$BUGSoutput$mean$Intercept
	Beta_temp<-out$BUGSoutput$mean$Beta_temp
		

	y<-sapply(1:nrow(yyy),function(x)
		{
		loc<- yyy$loc[x]
		# AVAILABILITY GIVEN STAGE
		y<- c(b0[loc,]) +
			c(b1[loc,])*yyy$Stage.m[x]
		avail<- exp(y)/sum(exp(y))
		return(avail)
		})

	y_cum<- t(apply(y,2,cumsum)	)
	plotDat<- y_cum[yyy$loc==loc,]
	plotDat2<- yyy[yyy$loc==loc,]

	par(mar=c(4,4,4,4))
	plot(temp_std~date,yyy,type='n',ylim=c(0,1),las=1, ylab="",
		main="Tara-Vicksburg",xlab="")
	color<- brewer.pal(7,"Set2")
	par(lend=2)
	sapply(1:nrow(plotDat),function(x)
		{
		for(i in 7:1)
			{
			segments(plotDat2$date[x],0,plotDat2$date[x],plotDat[x,i],col=color[i],
				lwd=6)
			}
		})
	par(xpd=TRUE)
	legend(16000,1.15,legend=c('MC','SND','WD','NAT','REV','SC','ILT'),
		lwd=5,col=color,bty='n', horiz=TRUE,cex=1,xjust=0.5)
	par(new=TRUE)
	plot(Stage.m~date,plotDat2,type="p",col="blue",xaxt="n",yaxt="n",xlab="",ylab="",
		pch=19)
	axis(4,las=2)
	mtext("Stage",side=4,line=2)
	mtext("Date",side=1,line=2)
	mtext("Habitat Availability",side=2,line=2.2)
		
		
		
		
	## PROBABILITY OF USE	
	y<-sapply(1:nrow(yyy),function(x)
		{
		loc<- yyy$loc[x]
		# AVAILABILITY GIVEN STAGE
		y<- c(b0[loc,]) +
			c(b1[loc,])*yyy$Stage.m[x]
		avail<- exp(y)/sum(exp(y))
		
		# HABITAT SELECTION
		y<- c(Intercept[loc,]) +
			c(Beta_temp[loc,])*yyy$temp_std[x]+
			log(avail)
		s<- exp(y)/sum(exp(y))
		return(s)
		})

	y_cum<- t(apply(y,2,cumsum)	)
	plotDat<- y_cum[yyy$loc==loc,]
	plotDat2<- yyy[yyy$loc==loc,]

	plot(temp_std~date,yyy,type='n',ylim=c(0,1),las=1, ylab="",xlab="",
		main="Tara-Vicksburg")

	par(lend=2)
	sapply(1:nrow(plotDat),function(x)
		{
		for(i in 7:1)
			{
			segments(plotDat2$date[x],0,plotDat2$date[x],plotDat[x,i],col=color[i],
				lwd=6)
			}
		})
	 par(xpd=TRUE)
	legend(16000,1.15,legend=c('MC','SND','WD','NAT','REV','SC','ILT'),
		lwd=5,col=color,bty='n', horiz=TRUE,cex=1,xjust=0.5)

	par(new=TRUE)
	plot(TempC~date,plotDat2,type="p",col="blue",xaxt="n",yaxt="n",xlab="",ylab="",
		pch=19)
	axis(4,las=2)
	mtext("Temperature",side=4,line=2)
	mtext("Date",side=1,line=2)
	mtext("Probability of use",side=2,line=2.2)


	plotDat<-t(y)	
	plotDat<- plotDat[yyy$loc==loc,]
	plotDat2<- yyy[yyy$loc==loc,]

	plot(temp_std~date,yyy,type='n',ylim=c(0,0.6),las=1, ylab="",xlab="",
		main="Tara-Vicksburg")

	par(lend=2)
	sapply(1:7,function(x)
		{
		points(plotDat2$date,plotDat[,x],col=color[x],pch=19,type='pb')
		})
	 par(xpd=TRUE)
	legend(16000,0.68,legend=c('MC','SND','WD','NAT','REV','SC','ILT'),
		col=color,bty='n', horiz=TRUE,cex=1,xjust=0.5,pch=19)

	par(new=TRUE)
	plot(Stage.m~date,plotDat2,type="b",col="blue",xaxt="n",yaxt="n",xlab="",ylab="",
		pch=19)
	#points(Stage.m+5~date,plotDat2,type="b",col="red",xaxt="n",yaxt="n",xlab="",ylab="",
	#	pch=19,lwd=2)	
		
	axis(4,las=2)
	mtext("Stage",side=4,line=2)
	mtext("Date",side=1,line=2)
	mtext("Probability of use",side=2,line=2.2)
	}
}