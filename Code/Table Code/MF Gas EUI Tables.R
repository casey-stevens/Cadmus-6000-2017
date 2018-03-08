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

# Read in Billing Data Results and Clean
getSheetNames(file.path(filepathBillingData, 
                        paste("Gas MF Compiled Final.xlsx")))
billing.dat <-
  read.xlsx(xlsxFile = file.path(filepathBillingData, 
                                 paste("Gas MF Compiled Final.xlsx")),
            sheet = "Gas Compiled")

billing.keep <- c("PK_BuildingID", "Average.Common.Area.Therms.Usage",
                  "Average.Unit.Therms.Usage", "Master.Meter.Therms.Usage", "Unit.Only.Flag" )
billing.dat2 <- billing.dat[,billing.keep]
# Combine two units from same building
billing.dat3 <- summarise(group_by(billing.dat2, PK_BuildingID, Unit.Only.Flag),
                          Average.Common.Area.Therms.Usage = mean(Average.Common.Area.Therms.Usage),
                          Average.Unit.Therms.Usage = mean(Average.Unit.Therms.Usage),
                          Master.Meter.Therms.Usage = mean(Master.Meter.Therms.Usage))

# Read in Building Data and Clean
building.summary <-
  read.xlsx(xlsxFile = file.path(filepathRawData, 
                                 one.line.bldg.export), startRow = 2)
building.keep <- c("PK_BuildingID", "Total.Units.in.Building", "Total.Residential.Floor.Area",
                   "Area.of.Conditioned.Common.Space", "Total.Non-Residential.Floor.Area")
building.summary2 <- building.summary[,building.keep]

# Merge the two files 
billing.combined <- merge(x = billing.dat3, 
                          y = building.summary2,
                          by = "PK_BuildingID",
                          all.x = T)

billing.combined$UnitsFinal <- billing.combined$Total.Units.in.Building

# Merge on the RBSA file 
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
#Manually Fix some CK_Building_IDs
rbsa.dat$CK_Building_ID[which(rbsa.dat$CK_Cadmus_ID == "BPM53895 OS BPA")] <- "BLDG_New_3"
rbsa.dat$CK_Building_ID[which(rbsa.dat$CK_Cadmus_ID == "RM1684")] <- "BLDG_958942DC-E293-0A4D-BFE3-9BF9EBA4211B"
rbsa.dat$CK_Building_ID[which(rbsa.dat$CK_Cadmus_ID == "RS2071")] <- "BLDG_68C13530-9E52-4C34-90F3-7D9EEA375EF9"
BillingFinal <-left_join(rbsa.dat, billing.combined, by = c("CK_Building_ID" = "PK_BuildingID"))
BillingFinalClean <- BillingFinal[-which(duplicated(BillingFinal$CK_Building_ID)),]

###################################################################################################################
# ITEM 309: AVERAGE ANNUAL TOTAL RESIDENTIAL GAS THERMS PER RESIDENTIAL UNIT BY BUILDING SIZE FOR BUILDINGS WITH GAS SERVICE  (Table 103)
###################################################################################################################

item309.dat1 <- BillingFinalClean[which(BillingFinalClean$Average.Unit.Therms.Usage > 0 |
                                          BillingFinalClean$Master.Meter.Therms.Usage > 0),]
item309.dat1$TotalUnitUsage <- 
  item309.dat1$Average.Unit.Therms.Usage * item309.dat1$UnitsFinal
item309.dat1$TotalGasUsage <- 
  rowSums(item309.dat1[,c("TotalUnitUsage", "Master.Meter.Therms.Usage")], na.rm = T)
item309.dat1$GasPerUnit <- item309.dat1$TotalGasUsage/item309.dat1$UnitsFinal
colnames(item309.dat1)
keep.cols <- 
  c("Unit.Only.Flag","Average.Common.Area.Therms.Usage", "Average.Unit.Therms.Usage", "Master.Meter.Therms.Usage",
    "Total.Units.in.Building", "Total.Residential.Floor.Area",
    "Area.of.Conditioned.Common.Space", "Total.Non-Residential.Floor.Area", 
    "BuildingFlag", "UnitsFinal", "TotalUnitUsage", "TotalGasUsage", "GasPerUnit")

item309.data <- weightedData(item309.dat1[which(colnames(item309.dat1) %notin% c(keep.cols))])

item309.data <- left_join(item309.data, item309.dat1[which(colnames(item309.dat1) %in% c("CK_Building_ID",keep.cols))])
item309.data$count <- 1

item309.final <- mean_one_group(CustomerLevelData = item309.data
                                ,valueVariable = 'GasPerUnit'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

exportTable(item309.final, "MF", "Table 103", weighted = TRUE)

item309.final <- mean_one_group_unweighted(CustomerLevelData = item309.data
                                           ,valueVariable = 'GasPerUnit'
                                           ,byVariable = 'HomeType'
                                           ,aggregateRow = "All Sizes")

exportTable(item309.final, "MF", "Table 103", weighted = FALSE)

###################################################################################################################
# ITEM 310: AVERAGE ANNUAL TOTAL RESIDENTIAL GAS THERMS PER RESIDENTIAL UNIT BY BUILDING SIZE FOR BUILDINGS WITH GAS SERVICE  (Table 104)
###################################################################################################################

item310.dat1 <- BillingFinalClean[which(BillingFinalClean$Average.Unit.Therms.Usage > 0 |
                                          BillingFinalClean$Master.Meter.Therms.Usage > 0),]

item310.dat1$TotalUnitUsage <- 
  item310.dat1$Average.Unit.Therms.Usage * item310.dat1$UnitsFinal
item310.dat1$TotalGasUsage <- 
  rowSums(item310.dat1[,c("TotalUnitUsage", "Master.Meter.Therms.Usage")], na.rm = T)

item310.dat2 <- item310.dat1[which(item310.dat1$Total.Residential.Floor.Area > 0), ]
item310.dat2$EUI <- item310.dat2$TotalGasUsage/item310.dat2$Total.Residential.Floor.Area

keep.cols <- 
  c("Unit.Only.Flag","Average.Common.Area.Therms.Usage", "Average.Unit.Therms.Usage", "Master.Meter.Therms.Usage",
    "Total.Units.in.Building", "Total.Residential.Floor.Area",
    "Area.of.Conditioned.Common.Space", "Total.Non-Residential.Floor.Area", 
    "BuildingFlag", "UnitsFinal", "TotalUnitUsage", "TotalGasUsage", "EUI")


item310.data <- weightedData(item310.dat2[which(colnames(item310.dat2) %notin% c(keep.cols))])

item310.data <- left_join(item310.data, item310.dat2[which(colnames(item310.dat2) %in% c("CK_Building_ID",keep.cols))])
item310.data$count <- 1

item310.final <- mean_one_group(CustomerLevelData = item310.data
                                ,valueVariable = 'EUI'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

exportTable(item310.final, "MF", "Table 104", weighted = TRUE)

item310.final <- mean_one_group_unweighted(CustomerLevelData = item310.data
                                           ,valueVariable = 'EUI'
                                           ,byVariable = 'HomeType'
                                           ,aggregateRow = "All Sizes")

exportTable(item310.final, "MF", "Table 104", weighted = FALSE)
