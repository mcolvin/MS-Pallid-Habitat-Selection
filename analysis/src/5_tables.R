tables<- function(n){

if(n==1)
	{# MODEL SELECTION TABLE
	tmp<- data.frame(model=c(1:8),
		dic=c(M01$BUGSoutput$DIC,
			M02$BUGSoutput$DIC,
			M03$BUGSoutput$DIC,
			M04$BUGSoutput$DIC,
			M05$BUGSoutput$DIC,
			M06$BUGSoutput$DIC,
			M07$BUGSoutput$DIC,
			M08$BUGSoutput$DIC))
	tmp<- tmp[order(tmp$dic),]
	tmp$ddic<- tmp$dic-min(tmp$dic)
	return(tmp)
	}

}

