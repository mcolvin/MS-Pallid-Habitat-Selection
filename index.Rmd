

	source("./R/1_global.R")  	
	source("./R/2_functions.R") 
	loadout<- FALSE	
	source("./R/3_load.R")     
  	source("./R/4_clean.R" )
 	source("./R/5_tables.R") 
	source("./R/6_figures.R") 
	source("./R/7_model.R")
	#source("./R/8_analysis.R")



figures(14)

# PLOTS OF TEMPERATURE AND STAGE VERSUS HABIAT SELECTION 
# FOR BOTH RIVER SEGMENTS
figures(15) 

# FIGURES


# Figures

### Figure 1.  Lower Mississippi River temperature and 
### stage over the study period for Catfish Point river segment.

figures(1)

### Figure 2.  Lower Mississippi River temperature and 
### stage over the study period for Tara to Vicksburg 
### river segment.

figures(2)
  
### Figure 3.  Predicted habitat availability at varying river 
### stages for Catfish Point (top panel) and Tara to Vicksburg 
### (bottom panel) segments of the lower Mississippi River. 
 
figures(4)

### Figure 4. Predicted probability of habitat use at 
### varying river temperature with river stage held at average.
### Predicted values for the Mississippi River segment located at Catfish Point.
### Top panel represents actual probabilities of use and bottom panel represents 
### cumlative probabilities.

figures(5)

### Figure 4.  Predicted probability of habitat use at 
### varying river stage with river temperature held at average.
### Predicted values for the Mississippi River segment located at Catfish Point.
### Top panel represents actual probabilities of use and bottom panel represents 
### cumlative probabilities.  

figures(6)


### Figure 5.  Predicted probability of habitat use at 
### varying river temperature with river stage held at average.
### Predicted values for the Mississippi River segment from Tara to Vicksburg.
### Top panel represents actual probabilities of use and bottom panel represents 
### cumlative probabilities. 

figures(8)


### Figure 6. Predicted probability of habitat use at 
### varying river stage with river temperature held at average.
### Predicted values for the Mississippi River segment from Tara to Vicksburg.
### Top panel represents actual probabilities of use and bottom panel represents 
### cumlative probabilities.

figures(7)

### pr USE|STAGE T - V
figures(8)

### TAKES A MINUTE TO RUN
figures(9)

figures(13)


### VARYING METRICS FOR BOTH SEGEMENTS (pr AVAILIBLE, pr USE|TEMP, pr USE|STAGE)
figures(14)

### TAKES A MINUTE TO RUN
### HABITAT SELECTION FOR BOTH SEGMENTS
figures(15)


## MODEL SELECTION TABLE

### Table 1. Deviance information criteria (DIC) values and model 
### selection results for candidate habitat selection models for 
### Pallid Sturgeon captures in the Lower Mississippi River. 

tbl1<- tables(1)
write.csv(tbl1,"./tables/model-selection.csv")



