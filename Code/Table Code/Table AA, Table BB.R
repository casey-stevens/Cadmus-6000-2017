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
#Table AA
#############################################################################################
#subset to columns needed for analysis
tableAA.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"System.Type"
                                                                   ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                   ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                   ,"MECH_TrueFLow_NSOP"
                                                                   ,"MECH_TrueFLow_TFSOP"
                                                                   ,"MECH_TrueFLow_SOP_NoFilter"
                                                                   ,"Primary.Heating.System"))]
str(tableAA.dat)
tableAA.dat1 <- tableAA.dat[which(!is.na(tableAA.dat$MECH_TrueFLow_NSOP)),]
tableAA.dat2 <- tableAA.dat1[which(!is.na(tableAA.dat1$MECH_TrueFLow_SOP_NoFilter)),]

tableAA.dat2$MECH_TrueFLow_NSOP <- as.numeric(as.character(tableAA.dat2$MECH_TrueFLow_NSOP))
tableAA.dat2$MECH_TrueFLow_Plate14_PressureDifference <- as.numeric(as.character(tableAA.dat2$MECH_TrueFLow_Plate14_PressureDifference))
tableAA.dat2$MECH_TrueFLow_Plate20_PressureDifference <- as.numeric(as.character(tableAA.dat2$MECH_TrueFLow_Plate20_PressureDifference))
tableAA.dat2$MECH_TrueFLow_SOP_NoFilter <- as.numeric(as.character(tableAA.dat2$MECH_TrueFLow_SOP_NoFilter))
tableAA.dat2$MECH_TrueFLow_TFSOP <- as.numeric(as.character(tableAA.dat2$MECH_TrueFLow_TFSOP))


ii=145
for(ii in 1:nrow(tableAA.dat2)){
  if(!is.na(tableAA.dat2$MECH_TrueFLow_Plate14_PressureDifference[ii])){
    tableAA.dat2$Flow[ii] <- sqrt(tableAA.dat2$MECH_TrueFLow_NSOP[ii] / tableAA.dat2$MECH_TrueFLow_SOP_NoFilter[ii]) * (115 * sqrt(abs(tableAA.dat2$MECH_TrueFLow_Plate14_PressureDifference[ii])))
  }else{
    tableAA.dat2$Flow[ii] <- sqrt(tableAA.dat2$MECH_TrueFLow_NSOP[ii] / tableAA.dat2$MECH_TrueFLow_SOP_NoFilter[ii]) * (154 * sqrt(abs(tableAA.dat2$MECH_TrueFLow_Plate20_PressureDifference[ii])))
  }
}

unique(tableAA.dat2$Flow)
tableAA.dat3 <- tableAA.dat2[which(tableAA.dat2$Flow %notin% c("NaN",NA)),]

#  Write out
# Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
# write.xlsx(tableAA.dat3, paste(filepathCleaningDocs, "Insulation Exports", paste("true.flow.calcs ", rundate, ".xlsx", sep = ""), sep="/"),
#            append = T, row.names = F, showNA = F)


# tableAA.dat3 <- tableAA.dat3[which(tableAA.dat3$Flow < 2000),]

tableAA.merge <- left_join(rbsa.dat, tableAA.dat3)
tableAA.merge <- tableAA.merge[which(!is.na(tableAA.merge$Flow)),]


################################################
# Adding pop and sample sizes for weights
################################################
tableAA.data <- weightedData(tableAA.merge[-which(colnames(tableAA.merge) %in% c("MECH_TrueFLow_NSOP"
                                                                              ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                              ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                              ,"MECH_TrueFLow_SOP_NoFilter"
                                                                              ,"MECH_TrueFLow_TFSOP"
                                                                              ,"System.Type"
                                                                              ,"Flow"
                                                                              ,"Primary.Heating.System"))])
tableAA.data <- left_join(tableAA.data, tableAA.merge[which(colnames(tableAA.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"MECH_TrueFLow_NSOP"
                                                                                       ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                                       ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                                       ,"MECH_TrueFLow_SOP_NoFilter"
                                                                                       ,"MECH_TrueFLow_TFSOP"
                                                                                       ,"System.Type"
                                                                                       ,"Flow"
                                                                                       ,"Primary.Heating.System"))])
tableAA.data$count <- 1
colnames(tableAA.data)
#######################
# Weighted Analysis
#######################
tableAA.final <- mean_one_group(CustomerLevelData = tableAA.data
                               ,valueVariable = 'Flow'
                               ,byVariable = 'State'
                               ,aggregateRow = 'Region')

tableAA.final.SF <- tableAA.final[which(tableAA.final$BuildingType == "Single Family")
                                ,which(colnames(tableAA.final) %notin% c("BuildingType"))]
tableAA.final.MH <- tableAA.final[which(tableAA.final$BuildingType == "Manufactured")
                                  ,which(colnames(tableAA.final) %notin% c("BuildingType"))]

# exportTable(tableAA.final.SF, "SF", "Table AA", weighted = TRUE)
exportTable(tableAA.final.MH, "MH", "Table AA", weighted = TRUE)

#######################
# unweighted Analysis
#######################
tableAA.final <- mean_one_group_unweighted(CustomerLevelData = tableAA.data
                               ,valueVariable = 'Flow'
                               ,byVariable = 'State'
                               ,aggregateRow = 'Region')
tableAA.final.SF <- tableAA.final[which(tableAA.final$BuildingType == "Single Family")
                                ,which(colnames(tableAA.final) %notin% c("BuildingType"))]
tableAA.final.MH <- tableAA.final[which(tableAA.final$BuildingType == "Manufactured")
                                  ,which(colnames(tableAA.final) %notin% c("BuildingType"))]

# exportTable(tableAA.final.SF, "SF", "Table AA", weighted = FALSE)
exportTable(tableAA.final.MH, "MH", "Table AA", weighted = FALSE)


#############################################################################################
#Item 63: AVERAGE DUCT LEAKAGE TOTAL FLOW (NORMALIZED BY HOUSE AREA) BY STATE (SF table 70)
#############################################################################################
tableBB.dat <- tableAA.merge
tableBB.dat$Normalized.Flow <- as.numeric(as.character(tableBB.dat$Flow)) / as.numeric(as.character(tableBB.dat$Conditioned.Area))
tableBB.dat1 <- tableBB.dat[which(!is.na(tableBB.dat$Normalized.Flow)),]

################################################
# Adding pop and sample sizes for weights
################################################
tableBB.data <- weightedData(tableBB.dat1[-which(colnames(tableBB.dat1) %in% c("MECH_TrueFLow_NSOP"
                                                                              ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                              ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                              ,"MECH_TrueFLow_SOP_NoFilter"
                                                                              ,"MECH_TrueFLow_TFSOP"
                                                                              ,"Primary.Heating.System"
                                                                              ,"System.Type"
                                                                              ,"Flow"
                                                                              ,"Normalized.Flow"))])
tableBB.data <- left_join(tableBB.data, tableBB.dat1[which(colnames(tableBB.dat1) %in% c("CK_Cadmus_ID"
                                                                                       ,"MECH_TrueFLow_NSOP"
                                                                                       ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                                       ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                                       ,"MECH_TrueFLow_SOP_NoFilter"
                                                                                       ,"MECH_TrueFLow_TFSOP"
                                                                                       ,"Primary.Heating.System"
                                                                                       ,"System.Type"
                                                                                       ,"Flow"
                                                                                       ,"Normalized.Flow"))])
tableBB.data$count <- 1
colnames(tableBB.data)

#######################
# Weighted Analysis
#######################
tableBB.final <- mean_one_group(CustomerLevelData = tableBB.data
                               ,valueVariable = 'Normalized.Flow'
                               ,byVariable = 'State'
                               ,aggregateRow = 'Region')

tableBB.final.SF <- tableBB.final[which(tableBB.final$BuildingType == "Single Family")
                                ,which(colnames(tableBB.final) %notin% c("BuildingType"))]
tableBB.final.MH <- tableBB.final[which(tableBB.final$BuildingType == "Manufactured")
                                  ,which(colnames(tableBB.final) %notin% c("BuildingType"))]

# exportTable(tableBB.final.SF, "SF", "Table BB", weighted = TRUE)
exportTable(tableBB.final.MH, "MH", "Table BB", weighted = TRUE)

#######################
# unweighted Analysis
#######################
tableBB.final <- mean_one_group_unweighted(CustomerLevelData = tableBB.data
                                          ,valueVariable = 'Normalized.Flow'
                                          ,byVariable = 'State'
                                          ,aggregateRow = 'Region')
tableBB.final.SF <- tableBB.final[which(tableBB.final$BuildingType == "Single Family")
                                ,which(colnames(tableBB.final) %notin% c("BuildingType"))]
tableBB.final.MH <- tableBB.final[which(tableBB.final$BuildingType == "Manufactured")
                                  ,which(colnames(tableBB.final) %notin% c("BuildingType"))]

# exportTable(tableBB.final.SF, "SF", "Table BB", weighted = FALSE)
exportTable(tableBB.final.MH, "MH", "Table BB", weighted = FALSE)

