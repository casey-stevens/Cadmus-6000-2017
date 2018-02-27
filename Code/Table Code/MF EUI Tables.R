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

# Merge on the RBSA file 
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
rbsa.dat$rbsaFlag <- 1

billing.combined2 <- merge(x = billing.combined, 
                           y = rbsa.dat,
                           by.x = "PK_BuildingID",
                           by.y = "CK_Building_ID",
                           all.x = T)
# Did any not merge?
stopifnot(length(which(is.na(rbsa.dat$rbsaFlag))) == 0)


###################################################################################################################
# ITEM 305: AVERAGE ANNUAL UNIT ELECTRIC CONSUMPTION BY BUILDING SIZE (Table 99)
###################################################################################################################

item305.dat1 <- 
  billing.combined[which(billing.combined2$Average.Unit.kWh.Usage > 0),]

item305.data <- weightedData(item305.dat1[which(colnames(item305.dat1) %notin% c("PK_BuildingID"
                                                                                 ,"Average.Unit.kWh.Usage"))])
