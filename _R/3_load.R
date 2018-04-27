

habs<-c('MC','SND','WD',
'NAT','REV','SC','ILT')

habs_full<- c('Main channel',
        'Sandbar',
        'Wing dike',
        'Natural bank','Revetted bank',
        'Secondary channel',
        'Island tip')
#dat<-read.csv("./dat/LMR.tracking.CPTV.avail.csv")
dat<-read.csv("_dat/LMR.tracking.CPTV2.csv",na.strings = ".")
cp_avail<-read.csv("_dat/CPHabAvail.csv",na.strings = ".")
cp_avail<- data.frame(loc=1, cp_avail)
vb_avail<-read.csv("_dat/VburgHabAvail.csv",na.strings = ".")
vb_avail<- data.frame(loc=2, vb_avail)

load("_output/out-model-03.Rdata")
M03<- out

if(loadout==TRUE){
load("_output/out-model-01.Rdata")
M01<- out

load("_output/out-model-02.Rdata")
M02<- out

load("_output/out-model-03.Rdata")
M03<- out

load("_output/out-model-04.Rdata")
M04<- out

load("_output/out-model-05.Rdata")
M05<- out

load("_output/out-model-06.Rdata")
M06<- out

load("_output/out-model-07.Rdata")
M07<- out

load("_output/out-model-08.Rdata")
M08<- out
}