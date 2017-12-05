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
length(unique(rbsa.dat$CK_Cadmus_ID)) 

#Read in data for analysis
buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))
length(unique(buildings.dat$CK_Building_ID))
buildings.dat.clean <- buildings.dat[which(!duplicated(buildings.dat$CK_Building_ID)),]


buildings.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.interview.export))
buildings.interview.dat$CK_Building_ID <- trimws(toupper(buildings.interview.dat$CK_BuildingID))



#############################################################################################
#Item 213: AVERAGE NUMBER OF CFLS INSTALLED PER HOME BY STATE (SF table 78, MH table 57)
#############################################################################################
#subset to columns needed for analysis
#from buildings interview data:
item213.interview.dat <- buildings.interview.dat[which(colnames(buildings.interview.dat) %in% c("CK_Building_ID"
                                                               ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataTotalNumberOfUnitsInAuditedBuilding"))]
names(item213.interview.dat) <- c("Number.of.Units", "CK_Building_ID")

item213.building.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
                                                                           ,"SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround"))]
names(item213.building.dat) <- c("Total.Floors", "CK_Building_ID")


item213.dat <- left_join(item213.building.dat, item213.interview.dat)
item213.dat1 <- item213.dat[which(!is.na(item213.dat$Number.of.Units)),]

item213.merge <- left_join(rbsa.dat, item213.dat1)
item213.merge <- item213.merge[which(!is.na(item213.merge$Number.of.Units)),]
item213.merge <- item213.merge[which(!is.na(item213.merge$HomeYearBuilt)),]


################################################
# Adding pop and sample sizes for weights
################################################
item213.data <- weightedData(item213.merge[-which(colnames(item213.merge) %in% c("Total.Floors"
                                                                                 ,"Number.of.Units"))])
item213.data <- left_join(item213.data, item213.merge[which(colnames(item213.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Total.Floors"
                                                                                           ,"Number.of.Units"))])
item213.data$count <- 1
colnames(item213.data)
#######################
# Weighted Analysis
#######################
item213.summary <- proportionRowsAndColumns1(CustomerLevelData = item213.data
                                          ,valueVariable = 'Number.of.Units'
                                          ,columnVariable = 'HomeYearBuilt_bins_MF'
                                          ,rowVariable = 'HomeType'
                                          ,aggregateColumnName = "Remove")
item213.summary <- item213.summary[which(item213.summary$HomeYearBuilt_bins_MF != "Remove"),]
item213.summary <- item213.summary[which(item213.summary$HomeType != "Total"),]

item213.all.vintages <- proportions_one_group(CustomerLevelData = item213.data
                                             ,valueVariable = 'Number.of.Units'
                                             ,groupingVariable = 'HomeType'
                                             ,total.name = "All Vintages"
                                             ,columnName = 'HomeYearBuilt_bins_MF'
                                             ,weighted = TRUE
                                             ,two.prop.total = TRUE)
item213.all.vintages <- item213.all.vintages[which(item213.all.vintages$HomeType != "Total"),]

item213.all.sizes <- proportions_one_group(CustomerLevelData = item213.data
                                             ,valueVariable = 'Number.of.Units'
                                             ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                             ,total.name = "All Sizes"
                                             ,columnName = 'HomeType'
                                             ,weighted = TRUE
                                             ,two.prop.total = TRUE)

item213.final <- rbind.data.frame(item213.summary, item213.all.vintages, item213.all.sizes, stringsAsFactors = F)
item213.final$HomeYearBuilt_bins_MF[which(item213.final$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"

item213.cast <- dcast(setDT(item213.final)
                      ,formula = BuildingType + HomeYearBuilt_bins_MF ~ HomeType
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N"))

item213.table <- data.frame("BuildingType" = item213.cast$BuildingType
                            ,"Housing.Vintage" = item213.cast$HomeYearBuilt_bins_MF
                            ,"Percent.")



#######################
# Unweighted Analysis
#######################

