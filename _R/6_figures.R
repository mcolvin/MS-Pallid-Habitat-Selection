figures<- function(n){
if(n==2)
	{ # PLOT OF TEMPERATURE AT TARA TO VICKSBURG
	plotDat1<- subset(dat,loc==1)
	par(mfrow=c(2,1),mar=c(1,5,0,1),oma=c(3,1,1,1),cex.lab=1.3)
	plot(TempC~date,plotDat1,type="l",
        col="black",xlab="",
        ylab=expression(paste("Temperature (",degree,"C)")),
		pch=19,main="",xaxt='n',las=1,lwd=2)
	axis(2,at=axTicks(1),labels=FALSE)
	plotDat2<- subset(dat,loc==2)	
    points(TempC~date,plotDat2,type='l',col="grey",lwd=2,lty=1)
    ## STAGE
    plot(Stage.m~date,plotDat1,type="l",col="black",xlab="",
        ylab="Stage (m)",
		pch=19,main="",las=1,lwd=2,
        ylim=c(-2,15))
	mtext(side=1,"Date",outer=TRUE, line=1.5,cex=1.3)
    points(Stage.m~date,plotDat2,type='l',col="grey",lwd=2,lty=1)    
    legend("bottomleft",c("Catfish Point","Vicksburg"),
        lty=c(1,1),col=c("black","grey"),
            lwd=2,bty='n')
	}
if(n==3)
	{# PLOT PREDCITED AVAILABILITY WITH STAGE for CATFISH POINT AND TARA-VICKBURG
	b0<- M03$BUGSoutput$mean$beta1
	b1<-M03$BUGSoutput$mean$beta2
	#color<-rev(brewer.pal(7,"Set2"))    
	#color<- (grey(1/c(1:8))[-1])
	## CATFISH POINT
	xx<- data.frame(stage=seq(-2,12,0.1))
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
		main="Catfish Point",xaxt='n',ylim=c(0,1))
		axis(1,at=axTicks(1))
	for(i in 7:1)
		{
		polygon(c(xx$stage,rev(xx$stage)),
			c(y_cum[,i],rep(0,nrow(xx))),
			col=color[i])		
		}
	#matpoints(obs_p[obs_p$loc==1,]$Stage,obs_p[obs_p$loc==1,-c(1,2)],pch=1,type='p')
	mtext(side=2, "Proportion available",outer=TRUE, line=0)
	mtext(side=1, "Stage (m)",outer=TRUE, line=0)
	
	
	## TARA TO VICKSBURG
	xx<- data.frame(stage=seq(0,14,0.1))
	## SCALE STAGE 
	xx$stage_scaled<- scale(xx$stage, center = mean(dat$Stage.m), scale = sd(dat$Stage.m))

	## ESTIMATE PROPORATION AVAILABLE
	y<-sapply(1:nrow(xx),function(x)
		{
		y<- c(b0[2,]) +c(b1[2,])*xx$stage_scaled[x]
		p<- exp(y)/sum(exp(y))
		return(p)
		})
	y<- t(y) # PROPORTION AVAILABLE

	y_cum<-sapply(1:nrow(xx),function(x)
		{
		y<- c(b0[2,]) +c(b1[2,])*xx$stage_scaled[x]
		p<- exp(y)/sum(exp(y))
		p<-cumsum(p)
		return(p)
		})
	y_cum<- t(y_cum)
	matplot(xx$stage,y_cum,type='n',las=1,xlab="",
		ylab="",main="Vicksburg",xaxt='n',ylim=c(0,1))
	axis(1,at=axTicks(1))
	for(i in 7:1)
		{
		polygon(c(xx$stage,rev(xx$stage)),
			c(y_cum[,i],rep(0,nrow(xx))),
			col=color[i])		
		}
	legend(4,0.3,legend=c('Main channel',
        'Sandbar',
        'Wing dike',
        'Natural bank','Revetted bank',
        'Secondary channel',
        'Island tip'),
		fill=color, horiz=FALSE,cex=0.6,
        bg="white",xjust=0.5,ncol=2)	
	
    }

if(n==4)## 4
	{# HABITAT SELECTION FOR CATFISH POINT & VICKSBURG: STAGE
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

	#color<- rev(brewer.pal(7,"Set2"))
	par(mfrow=c(2,1),mar=c(3,3,1,1),oma=c(2,2,1,1))
	#matplot(newdat$stage,y,type='l',ylim=c(0,0.8),las=1, ylab="",
	#	main="",col=color,lty=1,lwd=3,xaxt='n')
	#axis(1,at=axTicks(1),labels=FALSE)
	#legend("topright",legend=c('MC','SND','WD','NAT','REV','SC','ILT'),
	#	lwd=4,lty=1,col=color,bty='n', horiz=TRUE,cex=0.8,xjust=0.5)	
	
	matplot(newdat$stage,y_cum,type='n',ylim=c(0,1),las=1, ylab="",
		col=color,lty=1,lwd=2,main="Catfish Point",xaxt='n')
    axis(1,at=axTicks(1),labels=TRUE)
	for(i in 7:1)
		{
		x<- c(newdat$stage,rev(newdat$stage))
		y<- c(rep(0,nrow(y_cum)),rev(y_cum[,i]))
		polygon(x,y,col=color[i])		
		}

    ## VICKSBURG
    	preddat<- expand.grid(temp=sort(c(seq(4,32,0.25),temp_mn)),
		stage=sort(c(seq(-2,12,0.1),stage_mn)),loc=2)
	preddat$temp_scaled<- scale(preddat$temp, center = temp_mn, scale = temp_sd)
	preddat$stage_scaled<- scale(preddat$stage, center =stage_mn, scale = stage_sd)
	b0<- M03$BUGSoutput$mean$beta1
	b1<-M03$BUGSoutput$mean$beta2
	Intercept<- M03$BUGSoutput$mean$Intercept
	Beta_temp<-M03$BUGSoutput$mean$Beta_temp
	Beta_stage<-M03$BUGSoutput$mean$Beta_stage
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
	matplot(newdat$stage,y_cum,type='n',ylim=c(0,1),las=1, ylab="",
		col=color,lty=1,lwd=2,main="Vicksburg")

	for(i in 7:1)
		{
		x<- c(newdat$stage,rev(newdat$stage))
		y<- c(rep(0,nrow(y_cum)),rev(y_cum[,i]))
		polygon(x,y,col=color[i])		
		}
		
	#mtext(side=3, "TARA TO VICKSBURG", line=-0.75,outer=TRUE,cex=1.5)
	mtext(side=1, "Stage (m)", line=0,outer=TRUE)
	mtext(side=2, "Cumulative probability of use", line=-0.75,outer=TRUE)
    	legend(1.5,0.3,legend=c('Main channel',
        'Sandbar',
        'Wing dike',
        'Natural bank','Revetted bank',
        'Secondary channel',
        'Island tip'),
		fill=color, horiz=FALSE,cex=0.6,
        bg="white",xjust=0.5,ncol=2)	
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
	#color<- rev(brewer.pal(7,"Set2"))
par(mfrow=c(2,1),mar=c(3,3,1,1),oma=c(2,2,1,1))
	matplot(newdat$temp,y_cum,type='n',
        ylim=c(0,1),las=1, ylab="",
		main="Catfish Point",
        col=color,lty=1,lwd=2,xaxt='n')
    axis(1,at=axTicks(1),labels=TRUE)
	for(i in 7:1)
		{
		x<- c(newdat$temp,rev(newdat$temp))
		y<- c(rep(0,nrow(y_cum)),rev(y_cum[,i]))
		polygon(x,y,col=color[i])		
		}
    ## VICKSBURG
    preddat<- expand.grid(temp=sort(c(seq(4,30,0.25),temp_mn)),
		stage=sort(c(seq(0,14,0.1),stage_mn)),loc=2)
	preddat$temp_scaled<- scale(preddat$temp, center = temp_mn, scale = temp_sd)
	preddat$stage_scaled<- scale(preddat$stage, center =stage_mn, scale = stage_sd)
	
	b0<- M03$BUGSoutput$mean$beta1
	b1<-M03$BUGSoutput$mean$beta2
	Intercept<- M03$BUGSoutput$mean$Intercept
	Beta_temp<-M03$BUGSoutput$mean$Beta_temp
	Beta_stage<-M03$BUGSoutput$mean$Beta_stage
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
	matplot(newdat$temp,y_cum,type='n',ylim=c(0,1),las=1, ylab="",
		main="Vicksburg",col=color,lty=1,lwd=2)

	for(i in 7:1)
		{
		x<- c(newdat$temp,rev(newdat$temp))
		y<- c(rep(0,nrow(y_cum)),rev(y_cum[,i]))
		polygon(x,y,col=color[i])		
		}
	#mtext(side=3, "TARA TO VICKSBURG", line=-0.75,outer=TRUE,cex=1.5)
	mtext(side=1, expression(paste("Temperature (",degree,"C)")), line=0,outer=TRUE)
	mtext(side=2, "Cumulative probability of use",
        line=0,outer=TRUE)
    legend(10.5,0.3,legend=c('Main channel',
        'Sandbar',
        'Wing dike',
        'Natural bank','Revetted bank',
        'Secondary channel',
        'Island tip'),
		fill=color, horiz=FALSE,cex=0.6,
        bg="white",xjust=0.5,ncol=2)	
	}    
if(n==6)
	{
	load("./_output/out-model-03-gof.Rdata")
	#png(file = "_output/Rplot3.png", bg = "transparent", type = c("cairo"), width=2000, height=2000, res=300)
   
	selection<- expand.grid(temp=seq(-2,2,by=0.5),stage=0,loc=c(1,2))
	selection<- rbind(selection,
		expand.grid(temp=0,stage=seq(-2,2,by=0.5),loc=c(1,2)))
	selection$index<- c(1:nrow(selection))
	sel_mn<- matrix(out$BUGSoutput$summary[
		grep("sel",rownames(out$BUGSoutput$summary)),1],ncol=7,byrow=FALSE)
	sel_lci<-  matrix(out$BUGSoutput$summary[
		grep("sel",rownames(out$BUGSoutput$summary)),3],ncol=7,byrow=FALSE)
	sel_uci<-  matrix(out$BUGSoutput$summary[
		grep("sel",rownames(out$BUGSoutput$summary)),7],ncol=7,byrow=FALSE)
	selection$temp_raw<- selection$temp*temp_sd+temp_mn
	selection$stage_raw<- selection$stage*stage_sd+stage_mn
	sel_temp<-selection[1:18,]
	sel_mn_temp<- sel_mn[1:18,]
	sel_lci_temp<- sel_lci[1:18,]
	sel_uci_temp<- sel_uci[1:18,]
	sel_stage<-selection[19:36,]
	sel_mn_stage<- sel_mn[19:36,]
	sel_lci_stage<- sel_lci[19:36,]
	sel_uci_stage<- sel_uci[19:36,]
	par(mfrow=c(7,2),mar=c(1,3,0,0),oma=c(3,2,1,1))
	trans_red<- rgb(228,16,16,alpha=40,maxColorValue=255)
	trans_blu<- rgb(0,0,255,alpha=40,maxColorValue=255)
	cols<- c(trans_red,trans_blu)
	colLines<-c("red","blue")
	for(hab in 1:7)
		{
		# TEMPERATURE
		maxy<- (max(unlist(sel_uci_temp[,hab]))*1.1)
		miny<- min(unlist(sel_lci_temp[,hab]))
		xlabs<- c("","Temperature")
		xaxis<- c("n","s")
		plot(stage~temp_raw,sel_temp,
			type='n',
			ylim=c(miny,maxy),
			xlim=c(-1,35),
			las=1,
			ylab="Selection",
			xlab=ifelse(hab==7,xlabs[2],xlabs[1]),
			xaxt='n')	
		axis(1,at=axTicks(1),labels=FALSE)
		if(hab==7)
			{
			axis(1,at=axTicks(1),labels=TRUE)
			mtext(side=1,"Temperature",line=2.5)
			}
		for(loc in 1:2)
			{
			indx<- which(sel_temp$loc==loc)
			xy<-cbind(x=c(sel_temp$temp_raw[indx],
				rev(sel_temp$temp_raw[indx])),
				y=c(sel_lci_temp[indx,hab],
					rev(sel_uci_temp[indx,hab])))
			polygon(xy,col=cols[loc],border=NA)
			points(sel_temp$temp_raw[indx],
				sel_mn_temp[indx,hab],type='l',col=colLines[loc])
			}
		panLab(paste0(letters[hab],") ",habs_full[hab],sep=""))
		# STAGE	
		maxy<- (max(unlist(sel_uci_stage[,hab]))*1.1)
		miny<- min(unlist(sel_lci_stage[,hab]))
		xlabs<- c("","Stage")	
		xaxis<- c("n","s")
		plot(stage~stage_raw,sel_stage,
			type='n',
			ylim=c(miny,maxy),
			las=1,
			ylab="Selection",
			xlab=ifelse(hab==7,xlabs[2],xlabs[1]),
			xaxt=ifelse(hab==7,xaxis[2],xaxis[1]))	
	
		for(loc in 1:2)
			{
			indx<- which(sel_stage$loc==loc)
			xy<-cbind(x=c(sel_stage$stage_raw[indx],
				rev(sel_stage$stage_raw[indx])),
				y=c(sel_lci_stage[indx,hab],
					rev(sel_uci_stage[indx,hab])))
			polygon(xy,col=cols[loc],border=NA)
			points(sel_stage$stage_raw[indx],
				sel_mn_stage[indx,hab],type='l',col=colLines[loc])
			}
		axis(1,at=axTicks(1),labels=FALSE)
		if(hab==7)
			{
			axis(1,at=axTicks(1),labels=TRUE)
			mtext(side=1,"Stage",line=2.5)
			}
		}
	legend("topright",c("Catfish Point","Vicksburg"),bty='n',fill=colLines)
	mtext(side=2, "Habitat selection",outer=TRUE,line=0)
 #dev.off()
	}
    
if(n=="S2")
    {## TEMPERATURE
    load("_output/out-model-03-gof.Rdata")
    stages<- scale(seq(-2,18,by=2),stage_mn,stage_sd)   
    temps<-scale(seq(5,38,by=5),temp_mn,temp_sd)
    
    #temps<-seq(-2,2,by=0.5)#scale(seq(0,35,5),temp_mn,temp_sd) 
    #stages<-seq(-2,2,by=0.5)#scale(seq(-1,12,1),stage_mn,stage_sd)    
	selection<- expand.grid(temp=temps,stage=0,loc=c(1,2))
	selection<- rbind(selection,expand.grid(temp=0,stage=stages,loc=c(1,2)))
	selection$index<- c(1:nrow(selection))
	sel_mn<- matrix(out$BUGSoutput$summary[grep("sel",rownames(out$BUGSoutput$summary)),1],ncol=7,byrow=FALSE)
	sel_lci<-  matrix(out$BUGSoutput$summary[grep("sel",rownames(out$BUGSoutput$summary)),3],ncol=7,byrow=FALSE)
	sel_uci<-  matrix(out$BUGSoutput$summary[grep("sel",rownames(out$BUGSoutput$summary)),7],ncol=7,byrow=FALSE)
	selection$temp_raw<- selection$temp*temp_sd+temp_mn
	selection$stage_raw<- selection$stage*stage_sd+stage_mn	


	
	par(mfrow=c(4,2),mar=c(2,3,0,0),oma=c(2,2,1,1))
	for(kk in 1:7)
		{
		hab=kk
		indx<- which(selection$stage==0)		
		maxy<- (max(unlist(sel_uci[,hab]))*1.1)
		miny<- min(unlist(sel_lci[,hab]))
		pdat<-subset(selection, loc==1 & stage==0)
		xxx<-ifelse(kk %in% c(1,2,3,4,5),'n','s')
		yyy<-ifelse(kk %in% c(2,4,6),'s','s')
		plot(stage~temp_raw,selection,type='n',ylim=c(miny,
			maxy),
			xlim=c(-1,35),las=1,ylab="Selection",
			xlab="Temperature",xaxt=xxx,yaxt=yyy)
		if(kk %in% c(1,2,3,4,5)){axis(1, at=axTicks(1),labels=TRUE)}
		if(kk %in% c(2,4,6)){axis(2, at=axTicks(2),labels=FALSE)}
		for(i in 1:nrow(pdat))
			{
			ii<- pdat$index[i]
			points(pdat$temp_raw[i]-0.1,sel_mn[ii,hab],pch=19)
			segments(pdat$temp_raw[i]-0.1,sel_lci[ii,hab],pdat$temp_raw[i]-0.1,sel_uci[ii,hab])
			}
		pdat<-subset(selection, loc==2 & stage==0)
		for(i in 1:nrow(pdat))
			{
			ii<- pdat$index[i]
			points(pdat$temp_raw[i]+0.1,sel_mn[ii,hab],pch=17,col='red')
			segments(pdat$temp_raw[i]+0.1,
				sel_lci[ii,hab],
				pdat$temp_raw[i]+0.1,
				sel_uci[ii,hab],col='red')
            }		
		abline(h=1,lty=3)
		panLab(habs_full[kk])
        rug(jitter(na.omit(dat[dat$habitat==kk&dat$loc==1,]$TempC)))
        rug(jitter(na.omit(dat[dat$habitat==kk&dat$loc==2,]$TempC)),col='red')
		# add density plot of detections
		#par(new=TRUE)
		#dns1<-density(na.omit(dat[dat$habitat==kk& dat$loc==1,]$TempC))
		#dns2<-density(na.omit(dat[dat$habitat==kk& dat$loc==2,]$TempC))
		#plot(dns1,col="lightgrey",main="",xlim=c(-2,14),xaxt='n',yaxt='n',
		#	ylim=c(0,max(c(dns1$y,dns2$y))*1.4),lty=1,lwd=1)
		#points(dns2,lwd=1,lty=2,col="lightgrey",type='l')
		mtext(side=1, "Temperature (C)",outer=TRUE,line=0.5,cex=1.3)
		mtext(side=2, "Habitat selection",outer=TRUE,line=0.5,cex=1.3)
		}
	
	#plot.new();
	#legend(x=0,y=0.75,
	#	legend=c("Catfish Point","","Vicksburg",""),
	#	pch=c(19,NA,17,NA),lty=c(NA,1,NA,2),col=c('black','lightgrey'),cex=1.3)
	}	

if(n=="S1")
    {## Stage
    load("_output/out-model-03-gof.Rdata")
    stages<- scale(seq(-2,18,by=2),stage_mn,stage_sd)   
    temps<-scale(seq(5,38,by=5),temp_mn,temp_sd)
     
	selection<- expand.grid(temp=temps,stage=0,loc=c(1,2))
	selection<- rbind(selection,expand.grid(temp=0,stage=stages,loc=c(1,2)))
	selection$index<- c(1:nrow(selection))
	sel_mn<- matrix(out$BUGSoutput$summary[grep("sel",rownames(out$BUGSoutput$summary)),1],ncol=7,byrow=FALSE)
	sel_lci<-  matrix(out$BUGSoutput$summary[grep("sel",rownames(out$BUGSoutput$summary)),3],ncol=7,byrow=FALSE)
	sel_uci<-  matrix(out$BUGSoutput$summary[grep("sel",rownames(out$BUGSoutput$summary)),7],ncol=7,byrow=FALSE)
	
	selection$temp_raw<- selection$temp*temp_sd+temp_mn
	selection$stage_raw<- selection$stage*stage_sd+stage_mn

		
	# stage
	
	par(mfrow=c(4,2),mar=c(2,3,0,0),oma=c(2,2,1,1))
	for(kk in 1:7)
		{
		hab=kk	
		indx<- which(selection$temp==0)
		maxy<- (max(unlist(sel_uci[indx,hab]))*1.1)
		miny<- min(unlist(sel_lci[indx,hab]))
		pdat<-subset(selection, loc==1 & temp==0)
		xxx<-ifelse(kk %in% c(1,2,3,4,5),'n','s')
		yyy<-ifelse(kk %in% c(2,4,6),'s','s')
		plot(stage~stage_raw,selection,type='n',ylim=c(miny,
			maxy),
			xlim=c(-2,14),las=1,ylab="Selection",
			xlab="Temperature",xaxt=xxx,yaxt=yyy)
		if(kk %in% c(1,2,3,4,5)){axis(1, at=axTicks(1),labels=TRUE)}
		if(kk %in% c(2,4,6)){axis(2, at=axTicks(2),labels=FALSE)}
		for(i in 1:nrow(pdat))
			{
			ii<- pdat$index[i]
			points(pdat$stage_raw[i]-0.1,sel_mn[ii,hab],pch=19)
			segments(pdat$stage_raw[i]-0.1,sel_lci[ii,hab],pdat$stage_raw[i]-0.1,sel_uci[ii,hab])
			}
		pdat<-subset(selection, loc==2 & temp==0)
		for(i in 1:nrow(pdat))
			{
			ii<- pdat$index[i]
			points(pdat$stage_raw[i]+0.1,sel_mn[ii,hab],pch=17,col='red')
			segments(pdat$stage_raw[i]+0.1,
				sel_lci[ii,hab],
				pdat$stage_raw[i]+0.1,
				sel_uci[ii,hab],col='red')
			}		
		abline(h=1,lty=3)
		panLab(habs_full[kk])
        rug(jitter(na.omit(dat[dat$habitat==kk&dat$loc==1,]$Stage.m)))
        rug(jitter(na.omit(dat[dat$habitat==kk&dat$loc==2,]$Stage.m)),col='red')
  
        
		# add density plot of detections
		#par(new=TRUE)
		#dns1<-density(na.omit(dat[dat$habitat==kk&dat$loc==1,]$Stage.m))
		#dns2<-density(na.omit(dat[dat$habitat==kk&dat$loc==2,]$Stage.m))
		#plot(dns1,col="lightgrey",main="",xlim=c(-2,14),xaxt='n',yaxt='n',
		#	ylim=c(0,max(c(dns1$y,dns2$y))*1.4),lty=1,lwd=1)
		#points(dns2,lwd=1,lty=2,col="lightgrey",type='l')
		mtext(side=1, "Stage (m)",outer=TRUE,line=0.5,cex=1.3)
		mtext(side=2, "Habitat selection",outer=TRUE,line=0.5,cex=1.3)
		}
	
	#plot.new();
	#legend(x=0,y=0.75,
	#	legend=c("Catfish Point","","Vicksburg",""),
	#	pch=c(19,NA,17,NA),lty=c(NA,1,NA,2),col=c('black','lightgrey'),cex=1.3)
	
	#plot(Stage.m~TempC,dat, ylab="Stage",xlab="Temperature")
	}
    
    }
    
    
    
