
#test for differences in depth and surface current velocity among 
#habitats (an ANOVA will work, but there may be something better). If 
#moderate depth and current velocity are important, I would expect no 
#differences for sandbar, dikes, island tip, and natural bank. 

library(multcomp)
library(lsmeans)
#dat<-read.csv("./dat/LMR.tracking.CPTV.avail.csv")
#setwd("C:/Users/dahann/Downloads")
dat<-read.csv("_dat/LMR.tracking.CPTV3.csv",na.strings = ".")


habs<-c('MC','SND','WD','NAT','REV','SC','ILT')
dat$ht<- NA
for(i in 1:7)
    {
    dat[which(dat$habitat==i),]$ht<-habs[i]    
    }

## DEPTH
par(mfrow=c(1,2))
boxplot(depth~ht,dat,subset=Study.Site=="CP",
    at=c(1:7)-0.2,boxwex=0.2,xaxt="n",ylim=c(0,40),
    las=1,ylab="Depth (m)",xlab="Habitat type")
boxplot(depth~ht,dat,subset=Study.Site=="TV",
    at=c(1:7)+0.2,boxwex=0.2,xaxt="n",yaxt='n',add=TRUE,col='grey')
axis(1,c(1:7),labels=habs)  
 #legend("topright",fill=c("white","grey"),legend=c("Catfish Point","Vicksburg")) 
options(contrasts=c("contr.sum","contr.poly"))
fit<-lm(depth~as.factor(ht)*Study.Site,dat)
anova(fit)

fit<-aov(depth~as.factor(ht)*Study.Site,dat)
anova(fit)

fit<-aov(depth~as.factor(ht),dat)
anova(fit)
TukeyHSD(fit)# 21 comparisons (1/21 = p = 0.002381)




## CURRENT VELOCITY

boxplot(kph~ht,dat,subset=Study.Site=="CP",
    at=c(1:7)-0.2,boxwex=0.2,xaxt="n",ylim=c(0,9),
    las=1,ylab="Current velocity (kph)",xlab="Habitat type")
boxplot(kph~ht,dat,subset=Study.Site=="TV",
    at=c(1:7)+0.2,boxwex=0.2,xaxt="n",yaxt='n',add=TRUE,col='grey')
axis(1,c(1:7),labels=habs)  
 #legend("topright",fill=c("white","grey"),legend=c("Catfish Point","Vicksburg")) 
options(contrasts=c("contr.sum","contr.poly"))
fit<-aov(kph~as.factor(ht)*Study.Site,dat)
anova(fit)

#fit<-aov(kph~as.factor(ht),dat)
anova(fit)
TukeyHSD(fit)





## DEPTH

boxplot(depth~ht,dat,subset=Study.Site=="CP",
    at=c(1:7)-0.2,boxwex=0.2,xaxt="n",ylim=c(0,40),
    las=1,ylab="Depth (m)",xlab="Habitat type")
boxplot(depth~ht,dat,subset=Study.Site=="TV",
    at=c(1:7)+0.2,boxwex=0.2,xaxt="n",yaxt='n',add=TRUE,col='grey')
axis(1,c(1:7),labels=habs)  
 legend("topright",fill=c("white","grey"),legend=c("Catfish Point","Vicksburg")) 
options(contrasts=c("contr.sum","contr.poly"))
fit<-lm(depth~as.factor(ht)*Study.Site,dat)
anova(fit)

fit<-aov(depth~as.factor(ht)*Study.Site,dat)
anova(fit)

fit<-aov(depth~as.factor(ht),dat)
anova(fit)
TukeyHSD(fit)




plot(kph~depth,dat,las=1,xlab="Depth (m)",ylab="Current velocity (kph)",
    subset=(Study.Site=="CP"))
points(kph~depth,dat,subset=(habitat==1 & Study.Site=="CP"),col=1,pch=19)
points(kph~depth,dat,subset=(habitat==2 & Study.Site=="CP"),col=2,pch=19)
points(kph~depth,dat,subset=(habitat==3 & Study.Site=="CP"),col=3,pch=19)
points(kph~depth,dat,subset=(habitat==4 & Study.Site=="CP"),col=4,pch=19)
points(kph~depth,dat,subset=(habitat==5 & Study.Site=="CP"),col=5,pch=19)
points(kph~depth,dat,subset=(habitat==6 & Study.Site=="CP"),col=6,pch=19)
points(kph~depth,dat,subset=(habitat==7 & Study.Site=="CP"),col=7,pch=19)

plot(kph~depth,dat,las=1,xlab="Depth (m)",ylab="Current velocity (kph)",
    subset=(Study.Site=="TV"))
points(kph~depth,dat,subset=(habitat==1 & Study.Site=="TV"),col=1,pch=19)
points(kph~depth,dat,subset=(habitat==2 & Study.Site=="TV"),col=2,pch=19)
points(kph~depth,dat,subset=(habitat==3 & Study.Site=="TV"),col=3,pch=19)
points(kph~depth,dat,subset=(habitat==4 & Study.Site=="TV"),col=4,pch=19)
points(kph~depth,dat,subset=(habitat==5 & Study.Site=="TV"),col=5,pch=19)
points(kph~depth,dat,subset=(habitat==6 & Study.Site=="TV"),col=6,pch=19)
points(kph~depth,dat,subset=(habitat==7 & Study.Site=="TV"),col=7,pch=19)


#temp and stage plot
setwd("F:/D drive personal laptop/Seasonal Analysis")
cps=read.csv("CP Stages.csv",header=T,stringsAsFactors=FALSE)
str(cps)
cps$date=as.Date(cps$date,format='%m/%d/%y')
vbs=read.csv("VB Stages.csv",header=T,stringsAsFactors=FALSE)
vbs$date=as.Date(vbs$date,format='%m/%d/%y')
setwd("C:/Users/dahann/Downloads")
lmr=read.csv("LMR.tracking.CPTV3.csv",header=T)
lmr$date=as.Date(lmr$date,format='%m/%d/%y')
lmrcp=lmr[which(lmr$Study.Site=="CP"),]
lmrcp = lmrcp[!duplicated(lmrcp$date),]
lmrtv=lmr[which(lmr$Study.Site=="TV"),]
lmrtv = lmrtv[!duplicated(lmrtv$date),]

par(mfrow=c(2,1))
par(mai=c(0.1,0.9,0.8,0.1))
plot(lmrcp$date,lmrcp$temp.field,type="l",lwd=2,xlab=NA,xaxt='n',ylab=expression(Temperature~(degree*C)))
lines(lmrtv$date,lmrtv$temp.field,col="blue",lty=1,lwd=2)
par(mai=c(0.9,0.9,0,0.1))
plot(cps$date,cps$stage,type='l',lwd=2,ylab=expression(Stage (m)),xlab="Date")
lines(vbs$date,vbs$stage,col="blue",lty=1,lwd=2)
