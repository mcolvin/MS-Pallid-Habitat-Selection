

setwd("C:/Users/mcolvin/Documents/projects/MS-Pallids/analysis")
	source("./src/1_global.R")  	
	source("./src/2_functions.R")         
	source("./src/3_load.R")     
  	source("./src/4_clean.R" )
 	source("./src/5_tables.R") 
	source("./src/6_figures.R") 
	source("./src/7_model.R")
	

## MODEL SELECTION TABLE
tbl1<- tables(1)
write.csv(tbl1,"./tables/model-selection.csv")



