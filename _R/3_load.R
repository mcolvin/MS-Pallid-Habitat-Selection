

habs<-c('MC','SND','WD',
'NAT','REV','SC','ILT')

habs_full<- c('Main channel',
        'Sandbar',
        'Wing dike',
        'Natural bank','Revetted bank',
        'Secondary channel',
        'Island tip')
color<-c(
    rgb(45/255,173/255,228/255),     # MAIN CHANNEL 
    rgb(247/255,235/255,18/255), # SAND BAR
    rgb(22/255,173/255,81/255), # wing dike
    rgb(5/255,5/255,5/255),     # NAT
    rgb(238/255,33/255,25/255), # rev        
    rgb(245/255,103/255,33/255), # side channel
    rgb(153/255,81/255,160/255)) # island tip        
        

## LOAD TRACKING LOCATIONS
#dat<-read.csv("_dat/LMR.tracking.CPTV2.csv",na.strings = ".")
dat<-read.csv("_dat/20180931-stage-data/LMR.tracking.CPTV3.csv",na.strings = ".") # 20181006 revised data 

## LOAD CATFISH POINT HABITAT AVAILABILITY
cp_avail<-read.csv("_dat/CPHabAvail.csv",na.strings = ".")
cp_avail<- data.frame(loc=1, cp_avail)
## LOAD VICKSBURG AVAILABILITY
vb_avail<-read.csv("_dat/VburgHabAvail.csv",na.strings = ".")
vb_avail<- data.frame(loc=2, vb_avail)

## LOAD THE BEST APPROXIMATING MODEL
#load("_output/out-model-03.Rdata")
#M03<- out

## LOAD ALL THE FITTED MODELS
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