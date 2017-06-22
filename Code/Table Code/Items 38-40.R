#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list=ls())

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))








#############################################################################################
#Item 38: AVERAGE BLOWER DOOR AIR FLOW BY STATE
#############################################################################################
item38.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,""
                                                                   ,""))]
item38.dat1 <- left_join(rbsa.dat, item38.dat, by = "CK_Cadmus_ID")
length(unique(item38.dat1$CK_Cadmus_ID)) #565 yay!





###NOTE: ONLY TWO SITES HAVE BLOWER DOORS, SKIPPED THIS ANALYSIS FOR NOW