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

# Read in Billing Data Results
getSheetNames(file.path(filepathBillingData, 
                        paste("Final Compiled MF Building Data.xlsx")))
billing.dat <-
  read.xlsx(xlsxFile = file.path(filepathBillingData, 
                                 paste("Final Compiled MF Building Data.xlsx")),
            sheet = "Building Data Final")

# Read in Building Data
building.summary <-
  read.xlsx(xlsxFile = file.path(filepathRawData, 
                                 one.line.bldg.export), startRow = 2)
building.summary$BuildingFlag = 1

# Merge the two files 
billing.combined <- merge(x = billing.dat, 
                          y = building.summary,
                          by = "PK_BuildingID",
                          all.x = T)

# Did any not merge?
stopifnot(length(which(is.na(billing.combined$BuildingFlag))) == 0)
