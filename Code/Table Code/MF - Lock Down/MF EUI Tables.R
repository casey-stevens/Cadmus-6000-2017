#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################


##  Clear variables
# rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

##  Create "Not In" operator
"%notin%" <- Negate("%in%")

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation - MF-BLDG.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")

# Read in Billing Data Results and Clean
getSheetNames(file.path(filepathBillingData, 
                        paste("Final Compiled MF Building Data.xlsx")))
billing.dat <-
  read.xlsx(xlsxFile = file.path(filepathBillingData, 
                                 paste("Final Compiled MF Building Data.xlsx")),
            sheet = "Building Data Final")
billing.keep <- c("PK_BuildingID", "Average.Common.Area.kWh.Usage", "Average.Unit.kWh.Usage",
                  "Unit.Decision", "Common.Decision", "Average.Common.Area.Therms.Usage",
                  "Average.Unit.Therms.Usage", "Master.Meter.Therms.Usage")
billing.dat2 <- billing.dat[,billing.keep]
length(which(billing.dat2$Average.Unit.kWh.Usage > 0))

# Read in Building Data and Clean
building.summary <-
  read.xlsx(xlsxFile = file.path(filepathRawData, 
                                 one.line.bldg.export), startRow = 2)
building.keep <- c("PK_BuildingID", "Total.Units.in.Building", "Total.Residential.Floor.Area",
                   "Area.of.Conditioned.Common.Space", "Total.Non-Residential.Floor.Area")
building.summary2 <- building.summary[,building.keep]
building.summary2$BuildingFlag = 1

# Merge the two files 
billing.combined <- merge(x = billing.dat2, 
                          y = building.summary2,
                          by = "PK_BuildingID",
                          all.x = T)
length(which(billing.combined$Average.Unit.kWh.Usage > 0))

# Did any not merge?
stopifnot(length(which(is.na(billing.combined$BuildingFlag))) == 0)

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
# ITEM 305: AVERAGE ANNUAL UNIT ELECTRIC CONSUMPTION BY BUILDING SIZE (Table 99)
###################################################################################################################

## Should be 361 rows
item305.dat1 <- BillingFinalClean[which(BillingFinalClean$Average.Unit.kWh.Usage > 0),]
keep.cols <- 
  c( "Average.Common.Area.kWh.Usage", "Average.Unit.kWh.Usage", "Unit.Decision", 
    "Common.Decision","Average.Common.Area.Therms.Usage", "Average.Unit.Therms.Usage", "Master.Meter.Therms.Usage",
    "Total.Units.in.Building", "Total.Residential.Floor.Area",
    "Area.of.Conditioned.Common.Space", "Total.Non-Residential.Floor.Area", 
    "BuildingFlag", "UnitsFinal")

item305.data <- weightedData(item305.dat1[which(colnames(item305.dat1) %notin% c(keep.cols))])

item305.data <- left_join(item305.data, item305.dat1[which(colnames(item305.dat1) %in% c("CK_Building_ID",keep.cols))])
item305.data$count <- 1

item305.final <- mean_one_group(CustomerLevelData = item305.data
                                ,valueVariable = 'Average.Unit.kWh.Usage'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

exportTable(item305.final, "MF", "Table 99", weighted = TRUE)

### Unweighted

item305.final <- mean_one_group_unweighted(CustomerLevelData = item305.data
                                           ,valueVariable = 'Average.Unit.kWh.Usage'
                                           ,byVariable = 'HomeType'
                                           ,aggregateRow = "All Sizes")

exportTable(item305.final, "MF", "Table 99", weighted = FALSE)

###################################################################################################################
# ITEM 306: AVERAGE ANNUAL UNIT ELECTRIC CONSUMPTION BY UNIT SIZE AND BUILDING SIZE  (Table 100)
###################################################################################################################

item306.dat1 <- BillingFinalClean[which(BillingFinalClean$Average.Unit.kWh.Usage > 0),]
item306.dat2 <- item306.dat1[which(item306.dat1$Total.Residential.Floor.Area > 0),]
item306.dat2$TotalResidentialUsage <- 
  item306.dat2$Average.Unit.kWh.Usage * item306.dat2$UnitsFinal

item306.dat2$EUI <- 
  item306.dat2$TotalResidentialUsage/item306.dat2$Total.Residential.Floor.Area

keep.cols <- 
  c("Average.Common.Area.kWh.Usage", "Average.Unit.kWh.Usage", "Unit.Decision", 
    "Common.Decision","Average.Common.Area.Therms.Usage", "Average.Unit.Therms.Usage", "Master.Meter.Therms.Usage",
    "Total.Units.in.Building", "Total.Residential.Floor.Area",
    "Area.of.Conditioned.Common.Space", "Total.Non-Residential.Floor.Area", 
    "BuildingFlag","UnitsFinal", "TotalResidentialUsage", "EUI")

item306.data <- weightedData(item306.dat2[which(colnames(item306.dat2) %notin% c(keep.cols))])

item306.data <- left_join(item306.data, item306.dat2[which(colnames(item306.dat2) %in% c("CK_Building_ID",keep.cols))])
item306.data$count <- 1

item306.final <- mean_one_group(CustomerLevelData = item306.data
                                ,valueVariable = 'EUI'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

exportTable(item306.final, "MF", "Table 100", weighted = TRUE)

item306.final <- mean_one_group_unweighted(CustomerLevelData = item306.data
                                ,valueVariable = 'EUI'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

exportTable(item306.final, "MF", "Table 100", weighted = FALSE)

###################################################################################################################
# ITEM 307: AVERAGE ANNUAL PER UNIT COMMON AREA ELECTRIC CONSUMPTION BY BUILDING SIZE   (Table 101)
###################################################################################################################


## Should be 361 rows
item307.dat1 <- BillingFinalClean[which(BillingFinalClean$Average.Common.Area.kWh.Usage > 0),]
item307.dat1$TotalCommonUsage <- 
  item307.dat1$Average.Common.Area.kWh.Usage * item307.dat1$Common.Decision
item307.dat1$CommonPerUnit <- 
  item307.dat1$TotalCommonUsage/item307.dat1$UnitsFinal

keep.cols <- 
  c("Average.Common.Area.kWh.Usage", "Average.Unit.kWh.Usage", "Unit.Decision", 
    "Common.Decision","Average.Common.Area.Therms.Usage", "Average.Unit.Therms.Usage", "Master.Meter.Therms.Usage",
    "Total.Units.in.Building", "Total.Residential.Floor.Area",
    "Area.of.Conditioned.Common.Space", "Total.Non-Residential.Floor.Area", 
    "BuildingFlag", "UnitsFinal", "TotalCommonUsage", "CommonPerUnit")

item307.data <- weightedData(item307.dat1[which(colnames(item307.dat1) %notin% c(keep.cols))])

item307.data <- left_join(item307.data, item307.dat1[which(colnames(item307.dat1) %in% c("CK_Building_ID",keep.cols))])
item307.data$count <- 1

item307.final <- mean_one_group(CustomerLevelData = item307.data
                                ,valueVariable = 'CommonPerUnit'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

exportTable(item307.final, "MF", "Table 101", weighted = TRUE)

item307.final <- mean_one_group_unweighted(CustomerLevelData = item307.data
                                ,valueVariable = 'CommonPerUnit'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

exportTable(item307.final, "MF", "Table 101", weighted = FALSE)

###################################################################################################################
# ITEM 308: AVERAGE ANNUAL PER SQUARE FOOT COMMON AREA ELECTRIC CONSUMPTION BY BUILDING SIZE (Table 102)
###################################################################################################################

item308.dat1 <- BillingFinalClean[which(BillingFinalClean$Average.Common.Area.kWh.Usage > 0),]
item308.dat1$TotalCommonUsage <- 
  item308.dat1$Average.Common.Area.kWh.Usage * item308.dat1$Common.Decision
item308.dat2 <- item308.dat1[which(item308.dat1$Area.of.Conditioned.Common.Space > 0), ]

item308.dat2$CommonEUI <- 
  item308.dat2$TotalCommonUsage/item308.dat2$Area.of.Conditioned.Common.Space

keep.cols <- 
  c("Average.Common.Area.kWh.Usage", "Average.Unit.kWh.Usage", "Unit.Decision", 
    "Common.Decision","Average.Common.Area.Therms.Usage", "Average.Unit.Therms.Usage", "Master.Meter.Therms.Usage",
    "Total.Units.in.Building", "Total.Residential.Floor.Area",
    "Area.of.Conditioned.Common.Space", "Total.Non-Residential.Floor.Area", 
    "BuildingFlag", "UnitsFinal", "TotalCommonUsage", "CommonEUI")

item308.data <- weightedData(item308.dat2[which(colnames(item308.dat2) %notin% c(keep.cols))])

item308.data <- left_join(item308.data, item308.dat2[which(colnames(item308.dat2) %in% c("CK_Building_ID",keep.cols))])
item308.data$count <- 1

item308.final <- mean_one_group(CustomerLevelData = item308.data
                                ,valueVariable = 'CommonEUI'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

exportTable(item308.final, "MF", "Table 102", weighted = TRUE)

item308.final <- mean_one_group_unweighted(CustomerLevelData = item308.data
                                ,valueVariable = 'CommonEUI'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

exportTable(item308.final, "MF", "Table 102", weighted = FALSE)


###################################################################################################################
# ITEM 311: AVERAGE ANNUAL TOTAL ELECTRIC CONSUMPTION BY BUILDING SIZE (Table 105)
###################################################################################################################
### Careful with this table... should not include buildings that have common area bt did  nto receive
item311.dat1 <- BillingFinalClean[which(BillingFinalClean$Average.Unit.kWh.Usage > 0),]
item311.dat2 <- item311.dat1[-which((item311.dat1$Area.of.Conditioned.Common.Space > 0 &
                                      is.na(item311.dat1$Average.Common.Area.kWh.Usage)) |
                                      (item311.dat1$Area.of.Conditioned.Common.Space == 0 &
                                      item311.dat1$Average.Common.Area.kWh.Usage > 0)),]
item311.dat2$TotalUnitUsage <- 
  item311.dat2$Average.Unit.kWh.Usage * item311.dat2$UnitsFinal
item311.dat2$TotalCommonUsage <-  
  item311.dat2$Average.Common.Area.kWh.Usage * item311.dat2$Common.Decision

item311.dat2$TotalUsage <- 
  rowSums(item311.dat2[,c('TotalUnitUsage', 'TotalCommonUsage')], na.rm = T)

item311.dat2$AvgPerUnit <- item311.dat2$TotalUsage/item311.dat2$UnitsFinal

keep.cols <- 
  c("Average.Common.Area.kWh.Usage", "Average.Unit.kWh.Usage", "Unit.Decision", 
    "Common.Decision","Average.Common.Area.Therms.Usage", "Average.Unit.Therms.Usage", "Master.Meter.Therms.Usage",
    "Total.Units.in.Building", "Total.Residential.Floor.Area",
    "Area.of.Conditioned.Common.Space", "Total.Non-Residential.Floor.Area", 
    "BuildingFlag", "UnitsFinal", "TotalUnitUsage", "TotalCommonUsage",
    "TotalUsage", "AvgPerUnit")

item311.data <- weightedData(item311.dat2[which(colnames(item311.dat2) %notin% c(keep.cols))])

item311.data <- left_join(item311.data, item311.dat2[which(colnames(item311.dat2) %in% c("CK_Building_ID",keep.cols))])
item311.data$count <- 1

item311.final <- mean_one_group(CustomerLevelData = item311.data
                                ,valueVariable = 'AvgPerUnit'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

exportTable(item311.final, "MF", "Table 105", weighted = TRUE)

item311.final <- mean_one_group_unweighted(CustomerLevelData = item311.data
                                ,valueVariable = 'AvgPerUnit'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

exportTable(item311.final, "MF", "Table 105", weighted = FALSE)

###################################################################################################################
# ITEM 312: AVERAGE ANNUAL TOTAL ELECTRIC CONSUMPTION PER UNIT SQUARE FOOT BY BUILDING SIZE (Table 106)
###################################################################################################################

### Careful with this table... should not include buildings that have common area bt did  nto receive
item312.dat1 <- BillingFinalClean[which(BillingFinalClean$Average.Unit.kWh.Usage > 0),]
item312.dat2 <- item312.dat1[-which((item312.dat1$Area.of.Conditioned.Common.Space > 0 &
                                      is.na(item312.dat1$Average.Common.Area.kWh.Usage)) |
                                      (item312.dat1$Area.of.Conditioned.Common.Space == 0 &
                                      is.na(item312.dat1$Average.Common.Area.kWh.Usage > 0))),]

item312.dat2$TotalUnitUsage <- 
  item312.dat2$Average.Unit.kWh.Usage * item312.dat2$UnitsFinal
item312.dat2$TotalCommonUsage <-  
  item312.dat2$Average.Common.Area.kWh.Usage * item312.dat2$Common.Decision

item312.dat2$TotalUsage <- 
  rowSums(item312.dat2[,c('TotalUnitUsage', 'TotalCommonUsage')], na.rm = T)
item312.dat2$TotalSqFt <- 
  item312.dat2$Area.of.Conditioned.Common.Space + item312.dat2$Total.Residential.Floor.Area

item312.dat2$BuildingEUI <- item312.dat2$TotalUsage/item312.dat2$TotalSqFt

keep.cols <- 
  c("Average.Common.Area.kWh.Usage", "Average.Unit.kWh.Usage", "Unit.Decision", 
    "Common.Decision","Average.Common.Area.Therms.Usage", "Average.Unit.Therms.Usage", "Master.Meter.Therms.Usage",
    "Total.Units.in.Building", "Total.Residential.Floor.Area",
    "Area.of.Conditioned.Common.Space", "Total.Non-Residential.Floor.Area", 
    "BuildingFlag", "UnitsFinal", "TotalUnitUsage", "TotalCommonUsage",
    "TotalUsage", "TotalSqFt", "BuildingEUI")

item312.data <- weightedData(item312.dat2[which(colnames(item312.dat2) %notin% c(keep.cols))])

item312.data <- left_join(item312.data, item312.dat2[which(colnames(item312.dat2) %in% c("CK_Building_ID",keep.cols))])
item312.data$count <- 1

item312.final <- mean_one_group(CustomerLevelData = item312.data
                                ,valueVariable = 'BuildingEUI'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

exportTable(item312.final, "MF", "Table 106", weighted = TRUE)

item312.final <- mean_one_group_unweighted(CustomerLevelData = item312.data
                                           ,valueVariable = 'BuildingEUI'
                                           ,byVariable = 'HomeType'
                                           ,aggregateRow = "All Sizes")

exportTable(item312.final, "MF", "Table 106", weighted = FALSE)
