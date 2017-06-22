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
windows.doors.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, windows.export))










#############################################################################################
#Item 35: 
#############################################################################################
# Need to discuss
# Average window to floor area ratio
# Does this mean average window area / average floor area
# Fist average within homes, then average across homes within building type and floor area? I think so...

