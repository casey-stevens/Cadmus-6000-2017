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
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))


#read in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]





#############################################################################################
#Item 62: AVERAGE DUCT LEAKAGE TOTAL FLOW BY STATE  (SF table 69)
#############################################################################################
#subset to columns needed for analysis
item62.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"System.Type"
                                                                   ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                   ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                   ,"MECH_TrueFLow_NSOP"
                                                                   ,"MECH_TrueFLow_TFSOP"))]
str(item62.dat)
item62.dat1 <- left_join(rbsa.dat, item62.dat)

item62.dat1$Flow <- sqrt(item62.dat1$MECH_TrueFLow_TFSOP / item62.dat1$MECH_TrueFLow_NSOP) * ((115 * sqrt(item62.dat1$MECH_TrueFLow_Plate14_PressureDifference)) + 
                                                                                                (154 * sqrt(item62.dat1$MECH_TrueFLow_Plate20_PressureDifference)))
unique(item62.dat1$Flow)
