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
download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx', mechanical.export, mode = 'wb')
mechanical.dat <- read.xlsx(mechanical.export)
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

































############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
scl.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.scl.data", rundate, ".xlsx", sep = "")))
length(unique(scl.dat$CK_Cadmus_ID))
scl.dat$CK_Building_ID <- scl.dat$Category
scl.dat <- scl.dat[which(names(scl.dat) != "Category")]

#############################################################################################
#Table AA
#############################################################################################
#subset to columns needed for analysis
tableAA.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"System.Type"
                                                                    ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                    ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                    ,"MECH_TrueFLow_NSOP"
                                                                    ,"MECH_TrueFLow_TFSOP"
                                                                    ,"MECH_TrueFLow_SOP_NoFilter"
                                                                    ,"Primary.Heating.System"))]
str(tableAA.os.dat)
tableAA.os.dat1 <- tableAA.os.dat[which(!is.na(tableAA.os.dat$MECH_TrueFLow_NSOP)),]
tableAA.os.dat2 <- tableAA.os.dat1[which(!is.na(tableAA.os.dat1$MECH_TrueFLow_SOP_NoFilter)),]

tableAA.os.dat2$MECH_TrueFLow_NSOP <- as.numeric(as.character(tableAA.os.dat2$MECH_TrueFLow_NSOP))
tableAA.os.dat2$MECH_TrueFLow_Plate14_PressureDifference <- as.numeric(as.character(tableAA.os.dat2$MECH_TrueFLow_Plate14_PressureDifference))
tableAA.os.dat2$MECH_TrueFLow_Plate20_PressureDifference <- as.numeric(as.character(tableAA.os.dat2$MECH_TrueFLow_Plate20_PressureDifference))
tableAA.os.dat2$MECH_TrueFLow_SOP_NoFilter <- as.numeric(as.character(tableAA.os.dat2$MECH_TrueFLow_SOP_NoFilter))
tableAA.os.dat2$MECH_TrueFLow_TFSOP <- as.numeric(as.character(tableAA.os.dat2$MECH_TrueFLow_TFSOP))


ii=145
for(ii in 1:nrow(tableAA.os.dat2)){
  if(!is.na(tableAA.os.dat2$MECH_TrueFLow_Plate14_PressureDifference[ii])){
    tableAA.os.dat2$Flow[ii] <- sqrt(tableAA.os.dat2$MECH_TrueFLow_NSOP[ii] / tableAA.os.dat2$MECH_TrueFLow_SOP_NoFilter[ii]) * (115 * sqrt(abs(tableAA.os.dat2$MECH_TrueFLow_Plate14_PressureDifference[ii])))
  }else{
    tableAA.os.dat2$Flow[ii] <- sqrt(tableAA.os.dat2$MECH_TrueFLow_NSOP[ii] / tableAA.os.dat2$MECH_TrueFLow_SOP_NoFilter[ii]) * (154 * sqrt(abs(tableAA.os.dat2$MECH_TrueFLow_Plate20_PressureDifference[ii])))
  }
}

unique(tableAA.os.dat2$Flow)
tableAA.os.dat3 <- tableAA.os.dat2[which(tableAA.os.dat2$Flow %notin% c("NaN",NA)),]

tableAA.os.merge <- left_join(scl.dat, tableAA.os.dat3)
tableAA.os.merge <- tableAA.os.merge[which(!is.na(tableAA.os.merge$Flow)),]


################################################
# Adding pop and sample sizes for weights
################################################
tableAA.os.data <- weightedData(tableAA.os.merge[-which(colnames(tableAA.os.merge) %in% c("MECH_TrueFLow_NSOP"
                                                                                 ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                                 ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                                 ,"MECH_TrueFLow_SOP_NoFilter"
                                                                                 ,"MECH_TrueFLow_TFSOP"
                                                                                 ,"System.Type"
                                                                                 ,"Flow"
                                                                                 ,"Primary.Heating.System"))])
tableAA.os.data <- left_join(tableAA.os.data, unique(tableAA.os.merge[which(colnames(tableAA.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"MECH_TrueFLow_NSOP"
                                                                                           ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                                           ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                                           ,"MECH_TrueFLow_SOP_NoFilter"
                                                                                           ,"MECH_TrueFLow_TFSOP"
                                                                                           ,"System.Type"
                                                                                           ,"Flow"
                                                                                           ,"Primary.Heating.System"))]))
tableAA.os.data$count <- 1
colnames(tableAA.os.data)
#######################
# Weighted Analysis
#######################
tableAA.os.final <- mean_one_group(CustomerLevelData = tableAA.os.data
                                ,valueVariable = 'Flow'
                                ,byVariable = 'CK_Building_ID'
                                ,aggregateRow = 'Remove')
tableAA.os.final <- tableAA.os.final[which(tableAA.os.final$CK_Building_ID %notin% c("Remove","Total")),]
tableAA.os.final.SF <- tableAA.os.final[which(tableAA.os.final$BuildingType == "Single Family")
                                  ,which(colnames(tableAA.os.final) %notin% c("BuildingType"))]

exportTable(tableAA.os.final.SF, "SF", "Table AA", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# unweighted Analysis
#######################
tableAA.os.final <- mean_one_group_unweighted(CustomerLevelData = tableAA.os.data
                                           ,valueVariable = 'Flow'
                                           ,byVariable = 'CK_Building_ID'
                                           ,aggregateRow = 'Remove')
tableAA.os.final <- tableAA.os.final[which(tableAA.os.final$CK_Building_ID %notin% c("Remove","Total")),]
tableAA.os.final.SF <- tableAA.os.final[which(tableAA.os.final$BuildingType == "Single Family")
                                  ,which(colnames(tableAA.os.final) %notin% c("BuildingType"))]

exportTable(tableAA.os.final.SF, "SF", "Table AA", weighted = FALSE, osIndicator = "SCL", OS = T)


#############################################################################################
#Item 63: AVERAGE DUCT LEAKAGE TOTAL FLOW (NORMALIZED BY HOUSE AREA) BY CK_Building_ID (SF table 70)
#############################################################################################
tableBB.os.dat <- tableAA.os.merge
tableBB.os.dat$Normalized.Flow <- as.numeric(as.character(tableBB.os.dat$Flow)) / as.numeric(as.character(tableBB.os.dat$Conditioned.Area))
tableBB.os.dat1 <- tableBB.os.dat[which(!is.na(tableBB.os.dat$Normalized.Flow)),]

################################################
# Adding pop and sample sizes for weights
################################################
tableBB.os.data <- weightedData(tableBB.os.dat1[-which(colnames(tableBB.os.dat1) %in% c("MECH_TrueFLow_NSOP"
                                                                               ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                               ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                               ,"MECH_TrueFLow_SOP_NoFilter"
                                                                               ,"MECH_TrueFLow_TFSOP"
                                                                               ,"Primary.Heating.System"
                                                                               ,"System.Type"
                                                                               ,"Flow"
                                                                               ,"Normalized.Flow"))])
tableBB.os.data <- left_join(tableBB.os.data, unique(tableBB.os.dat1[which(colnames(tableBB.os.dat1) %in% c("CK_Cadmus_ID"
                                                                                         ,"MECH_TrueFLow_NSOP"
                                                                                         ,"MECH_TrueFLow_Plate14_PressureDifference"
                                                                                         ,"MECH_TrueFLow_Plate20_PressureDifference"
                                                                                         ,"MECH_TrueFLow_SOP_NoFilter"
                                                                                         ,"MECH_TrueFLow_TFSOP"
                                                                                         ,"Primary.Heating.System"
                                                                                         ,"System.Type"
                                                                                         ,"Flow"
                                                                                         ,"Normalized.Flow"))]))
tableBB.os.data$count <- 1
colnames(tableBB.os.data)

#######################
# Weighted Analysis
#######################
tableBB.os.final <- mean_one_group(CustomerLevelData = tableBB.os.data
                                ,valueVariable = 'Normalized.Flow'
                                ,byVariable = 'CK_Building_ID'
                                ,aggregateRow = 'Remove')
tableBB.os.final <- tableBB.os.final[which(tableBB.os.final$CK_Building_ID %notin% c("Remove","Total")),]

tableBB.os.final.SF <- tableBB.os.final[which(tableBB.os.final$BuildingType == "Single Family")
                                  ,which(colnames(tableBB.os.final) %notin% c("BuildingType"))]

exportTable(tableBB.os.final.SF, "SF", "Table BB", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# unweighted Analysis
#######################
tableBB.os.final <- mean_one_group_unweighted(CustomerLevelData = tableBB.os.data
                                           ,valueVariable = 'Normalized.Flow'
                                           ,byVariable = 'CK_Building_ID'
                                           ,aggregateRow = 'Remove')
tableBB.os.final <- tableBB.os.final[which(tableBB.os.final$CK_Building_ID %notin% c("Remove","Total")),]
tableBB.os.final.SF <- tableBB.os.final[which(tableBB.os.final$BuildingType == "Single Family")
                                  ,which(colnames(tableBB.os.final) %notin% c("BuildingType"))]

exportTable(tableBB.os.final.SF, "SF", "Table BB", weighted = FALSE, osIndicator = "SCL", OS = T)