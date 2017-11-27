#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

##  Create "Not In" operator
"%notin%" <- Negate("%in%")

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
#Item 38: AVERAGE BLOWER DOOR AIR FLOW BY STATE
#############################################################################################
item38.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"MECH_Blower_DOOR_BlowerDoorLocation_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_BlowerDoorLocation_SecondTrial"
                                                                   ,"MECH_Blower_DOOR_P25_CFM_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_P25_CFM_SecondTrial"
                                                                   ,"MECH_Blower_DOOR_P25_FanPressure_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_P25_FanPressure_SecondTrial"
                                                                   ,"MECH_Blower_DOOR_P50_CFM_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_P50_CFM_SecondTrial"
                                                                   ,"MECH_Blower_DOOR_P50_FanPressure_FirstTrial"
                                                                   ,"MECH_Blower_DOOR_P50_FanPressure_SecondTrial"))]
item38.dat1 <- left_join(rbsa.dat, item38.dat, by = "CK_Cadmus_ID")
length(unique(item38.dat1$CK_Cadmus_ID))





###NOTE: ONLY TWO SITES HAVE BLOWER DOORS, SKIPPED THIS ANALYSIS FOR NOW