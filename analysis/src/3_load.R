

	habs<-c('MC','SND','WD',
	'NAT','REV','SC','ILT')

#dat<-read.csv("./dat/LMR.tracking.CPTV.avail.csv")
dat<-read.csv("./dat/LMR.tracking.CPTV2.csv")
cp_avail<-read.csv("./dat/CPHabAvail.csv")
cp_avail<- data.frame(loc=1, cp_avail)
vb_avail<-read.csv("./dat/VburgHabAvail.csv")
vb_avail<- data.frame(loc=2, vb_avail)

#load("./output/out-model-01.Rdata")
#M01<- out
#
#load("./output/out-model-02.Rdata")
#M02<- out
#
#load("./output/out-model-03.Rdata")
#M03<- out
#
#load("./output/out-model-04.Rdata")
#M04<- out
#
#load("./output/out-model-05.Rdata")
#M05<- out
#
#load("./output/out-model-06.Rdata")
#M06<- out
#
#load("./output/out-model-07.Rdata")
#M07<- out
#
#load("./output/out-model-08.Rdata")
#M08<- out