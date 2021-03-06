#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################
##  Clear variables
# rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)


##  Create "Not In" operator
"%notin%" <- Negate("%in%")


# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
rbsa.dat <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]

length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
billing.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, billing.export))
#clean cadmus IDs
billing.dat$CK_Cadmus_ID <- trimws(toupper(billing.dat$CK_Cadmus_ID))


#############################################################################################
# 
#############################################################################################
