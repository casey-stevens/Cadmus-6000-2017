#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))


#read in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-24,-3]





#############################################################################################
#Item 62: AVERAGE DUCT LEAKAGE TOTAL FLOW BY STATE  (SF table 69)
#############################################################################################
#subset to columns needed for analysis
item62.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"System.Type"
                                                                   ,""
                                                                   ,""
                                                                   ,""
                                                                   ,""
                                                                   ,""
                                                                   ,""
                                                                   ,""))]
item62.dat$count <- 1
