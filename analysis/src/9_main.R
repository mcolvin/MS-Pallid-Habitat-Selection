

setwd("C:/Users/mcolvin/Documents/projects/MS-Pallid-Habitat-Selection/analysis")
	source("./src/1_global.R")  	
	source("./src/2_functions.R")         
	source("./src/3_load.R")     
  	source("./src/4_clean.R" )
 	source("./src/5_tables.R") 
	source("./src/6_figures.R") 
	source("./src/7_model.R")



setwd("C:/Users/mcolvin/Documents/projects/MS-Pallid-Habitat-Selection/analysis")
topdir<- "C:/Users/mcolvin/Documents/projects/MS-Pallid-Habitat-Selection"
# COMPILE FIGURES TO DOCX
knitr::knit("./src/2_figures.Rmd")	
knitr::pandoc('2_figures.md', format='docx')
file.copy("2_figures.docx", paste0(topdir,"/2_figures.docx"),overwrite=TRUE)
file.remove("2_figures.docx");file.remove("./2_figures.md")

# COMPILE TABLES TO DOCX
knitr::knit("./src/3_tables.Rmd")	
knitr::pandoc('3_tables.md', format='docx')
file.copy("./3_tables.docx",paste0(topdir,"/3_tables.docx"),overwrite=TRUE)
file.remove("./3_tables.docx");file.remove("./3_tables.md")	



# FIGURES	
dev.new(width=11.25, height=11)
figures(1);dev.off()
dev.new(width=11.25, height=11)
figures(2);dev.off()
dev.new(width=11.25, height=11)
figures(3);dev.off()
dev.new(width=11.25, height=11)
figures(4);dev.off()


## MODEL SELECTION TABLE
tbl1<- tables(1)
write.csv(tbl1,"./tables/model-selection.csv")



