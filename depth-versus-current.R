
#test for differences in depth and surface current velocity among 
#habitats (an ANOVA will work, but there may be something better). If 
#moderate depth and current velocity are important, I would expect no 
#differences for sandbar, dikes, island tip, and natural bank. 
setwd("C:/Users/pkroboth/Desktop/PLS/PLS Seasonal Data for Release/PSG-Habitat-Selection/PSG-Habitat-Selection/_dat")
library(multcomp)
library(lsmeans)
#dat<-read.csv("./dat/LMR.tracking.CPTV.avail.csv")
dat<-read.csv("LMR.tracking.CPTV3.csv",na.strings = ".")


habs<-c('MC','SND','WD','NAT','REV','SC','ILT')
dat$ht<- NA
for(i in 1:7)
    {
    dat[which(dat$habitat==i),]$ht<-habs[i]    
    }

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





## CURRENT VELOCITY

boxplot(kph~ht,dat,subset=Study.Site=="CP",
    at=c(1:7)-0.2,boxwex=0.2,xaxt="n",ylim=c(0,9),
    las=1,ylab="Current velocit (kph)",xlab="Habitat type")
boxplot(kph~ht,dat,subset=Study.Site=="TV",
    at=c(1:7)+0.2,boxwex=0.2,xaxt="n",yaxt='n',add=TRUE,col='grey')
axis(1,c(1:7),labels=habs)  
 legend("topright",fill=c("white","grey"),legend=c("Catfish Point","Vicksburg")) 
options(contrasts=c("contr.sum","contr.poly"))
fit<-aov(kph~as.factor(ht)*Study.Site,dat)
anova(fit)

fit<-aov(kph~as.factor(ht),dat)
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

#####Detection Data.PK run
track<-read.csv("TrackLMR.csv",header=T)
pls=subset(track,track$species=="Pallid")
pls2=subset(pls,pls$Study.Site=="catfish"|pls$Study.Site=="vicksburg")
length(unique(pls2$tag))

######DH code for the Boxplots
dat<-read.csv("LMR.tracking.CPTV3.csv",na.strings = ".")


habs<-c('MC','SND','WD','NAT','REV','SC','ILT')
dat$ht<- NA
for(i in 1:7)
{
  dat[which(dat$habitat==i),]$ht<-habs[i]    
}

## DEPTH

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
TukeyHSD(fit)




## CURRENT VELOCITY
head(dat)
summary(dat$mps)
boxplot(mps~ht,dat,subset=Study.Site=="CP",
        at=c(1:7)-0.2,boxwex=0.2,xaxt="n",ylim=c(0,2.5),
        las=1,ylab="Current velocity (m/s)",xlab="Habitat type")
boxplot(mps~ht,dat,subset=Study.Site=="TV",
        at=c(1:7)+0.2,boxwex=0.2,xaxt="n",yaxt='n',add=TRUE,col='grey')
axis(1,c(1:7),labels=habs)  
#legend("topright",fill=c("white","grey"),legend=c("Catfish Point","Vicksburg")) 
options(contrasts=c("contr.sum","contr.poly"))


fit<-aov(mps~as.factor(ht)*Study.Site,dat)
anova(fit)

fit<-aov(mps~as.factor(ht),dat)
anova(fit)
TukeyHSD(fit)


SCmps<-subset(dat,subset=Study.Site=="CP"&habitat==6,select=mps)
summary(SCmps)
SCvmps<-subset(dat,subset=Study.Site=="TV"&habitat==6,select=mps)
summary(SCvmps)









library(ggplot2)
depthdat=na.omit(data.frame(dat$depth,dat$ht,dat$Study.Site))

depplot=ggplot(depthdat, aes(x=dat.ht, y=dat.depth, fill=dat.Study.Site)) + 
  geom_boxplot()+
  theme_bw()+
  ylab("Depth (m)")+
  xlab("Habitat")+
  ylim(0,40)+
  scale_x_discrete(limits=c('MC','SND','WD','NAT','REV','SC','ILT'))+
  theme(legend.title=element_blank())+
  scale_fill_manual(values=c("white", "grey50"), 
                    name="",
                    breaks=c("CP", "TV"),
                    labels=c("Catfish Point", "Vicksburg"))

veldat=na.omit(data.frame(dat$mps,dat$ht,dat$Study.Site))

library(ggplot2)
velplot=ggplot(veldat, aes(x=dat.ht, y=dat.mps, fill=dat.Study.Site)) + 
  geom_boxplot()+
  theme_bw()+
  ylab("Current velocity (m/s)")+
  xlab("Habitat")+
  ylim(0,2.5)+
  scale_x_discrete(limits=c('MC','SND','WD','NAT','REV','SC','ILT'))+
  theme(legend.title=element_blank())+
  scale_fill_manual(values=c("white", "grey50"), 
                    name="",
                    breaks=c("CP", "TV"),
                    labels=c("Catfish Point", "Vicksburg"))
library(ggpubr)
ggarrange(depplot,velplot,ncol = 2, nrow = 1,common.legend = T,legend="right")





