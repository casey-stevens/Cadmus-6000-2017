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
                                                                   ,"System.Type"
                                                                   ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                   ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                   ,"MECH_TrueFLow_NSOP"
                                                                   ,"MECH_TrueFLow_TFSOP"
                                                                   ,"MECH_TrueFLow_SOP_NoFilter"))]
str(item62.dat)
item62.dat1 <- item62.dat[which(!is.na(item62.dat$MECH_TrueFLow_NSOP)),]
item62.dat2 <- item62.dat1[which(!is.na(item62.dat1$MECH_TrueFLow_SOP_NoFilter)),]
ii=145
for(ii in 1:nrow(item62.dat2)){
  if(!is.na(item62.dat2$MECH_TrueFLow_Plate14_PressureDifference[ii])){
    item62.dat2$Flow[ii] <- sqrt(item62.dat2$MECH_TrueFLow_NSOP[ii] / item62.dat2$MECH_TrueFLow_SOP_NoFilter[ii]) * (115 * sqrt(abs(item62.dat2$MECH_TrueFLow_Plate14_PressureDifference[ii])))
  }else{
    item62.dat2$Flow[ii] <- sqrt(item62.dat2$MECH_TrueFLow_NSOP[ii] / item62.dat2$MECH_TrueFLow_SOP_NoFilter[ii]) * (154 * sqrt(abs(item62.dat2$MECH_TrueFLow_Plate20_PressureDifference[ii])))
  }
}

unique(item62.dat2$Flow)
item62.dat3 <- item62.dat2[which(item62.dat2$Flow %notin% c("NaN",NA)),]
item62.dat3 <- item62.dat3[which(item62.dat3$Flow < 2000),]

item62.merge <- left_join(rbsa.dat, item62.dat3)
item62.merge <- item62.merge[which(!is.na(item62.merge$Flow)),]


################################################
# Adding pop and sample sizes for weights
################################################
item62.data <- weightedData(item62.merge[-which(colnames(item62.merge) %in% c("MECH_TrueFLow_NSOP"
                                                                              ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                              ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                              ,"MECH_TrueFLow_SOP_NoFilter"
                                                                              ,"MECH_TrueFLow_TFSOP"
                                                                              ,"System.Type"
                                                                              ,"Flow" ))])
item62.data <- left_join(item62.data, item62.merge[which(colnames(item62.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"MECH_TrueFLow_NSOP"
                                                                                       ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                                       ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                                       ,"MECH_TrueFLow_SOP_NoFilter"
                                                                                       ,"MECH_TrueFLow_TFSOP"
                                                                                       ,"System.Type"
                                                                                       ,"Flow"))])
item62.data$count <- 1
colnames(item62.data)
#######################
# Weighted Analysis
#######################
item62.final <- mean_one_group(CustomerLevelData = item62.data
                               ,valueVariable = 'Flow'
                               ,byVariable = 'State'
                               ,aggregateRow = 'Region')
item62.final.SF <- item62.final[which(item62.final$BuildingType == "Single Family")
                                ,which(colnames(item62.final) %notin% c("BuildingType"))]
exportTable(item62.final.SF, "SF", "Table AA", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item62.final <- mean_one_group_unweighted(CustomerLevelData = item62.data
                               ,valueVariable = 'Flow'
                               ,byVariable = 'State'
                               ,aggregateRow = 'Region')
item62.final.SF <- item62.final[which(item62.final$BuildingType == "Single Family")
                                ,which(colnames(item62.final) %notin% c("BuildingType"))]
exportTable(item62.final.SF, "SF", "Table AA", weighted = FALSE)


#############################################################################################
#Item 63: AVERAGE DUCT LEAKAGE TOTAL FLOW (NORMALIZED BY HOUSE AREA) BY STATE (SF table 70)
#############################################################################################
item63.dat <- item62.merge
item63.dat$Normalized.Flow <- item63.dat$Flow / item63.dat$Conditioned.Area
item63.dat1 <- item63.dat[which(!is.na(item63.dat$Normalized.Flow)),]

################################################
# Adding pop and sample sizes for weights
################################################
item63.data <- weightedData(item63.dat1[-which(colnames(item63.dat1) %in% c("MECH_TrueFLow_NSOP"
                                                                              ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                              ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                              ,"MECH_TrueFLow_SOP_NoFilter"
                                                                              ,"MECH_TrueFLow_TFSOP"
                                                                              ,"System.Type"
                                                                              ,"Flow"
                                                                              ,"Normalized.Flow"))])
item63.data <- left_join(item63.data, item63.dat1[which(colnames(item63.dat1) %in% c("CK_Cadmus_ID"
                                                                                       ,"MECH_TrueFLow_NSOP"
                                                                                       ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                                       ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                                       ,"MECH_TrueFLow_SOP_NoFilter"
                                                                                       ,"MECH_TrueFLow_TFSOP"
                                                                                       ,"System.Type"
                                                                                       ,"Flow"
                                                                                       ,"Normalized.Flow"))])
item63.data$count <- 1
colnames(item63.data)

#######################
# Weighted Analysis
#######################
item63.final <- mean_one_group(CustomerLevelData = item63.data
                               ,valueVariable = 'Normalized.Flow'
                               ,byVariable = 'State'
                               ,aggregateRow = 'Region')
item63.final.SF <- item63.final[which(item63.final$BuildingType == "Single Family")
                                ,which(colnames(item63.final) %notin% c("BuildingType"))]
exportTable(item63.final.SF, "SF", "Table BB", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item63.final <- mean_one_group_unweighted(CustomerLevelData = item63.data
                                          ,valueVariable = 'Normalized.Flow'
                                          ,byVariable = 'State'
                                          ,aggregateRow = 'Region')
item63.final.SF <- item63.final[which(item63.final$BuildingType == "Single Family")
                                ,which(colnames(item63.final) %notin% c("BuildingType"))]
exportTable(item63.final.SF, "SF", "Table BB", weighted = FALSE)

