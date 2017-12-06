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
#Item 213: DISTRIBUTION OF UNITS BY BUILDING SIZE AND VINTAGE (MF Table 5)
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
item213.data$Count <- 1
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
colnames(item213.cast)
item213.table <- data.frame("BuildingType" = item213.cast$BuildingType
                            ,"Housing.Vintage" = item213.cast$HomeYearBuilt_bins_MF
                            ,"Low.Rise.1.3"    = item213.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.SE"     = item213.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.n"      = item213.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6"    = item213.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.SE"     = item213.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.n"      = item213.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.7.Plus"= NA #item213.cast$
                            ,"High.Rise.SE"    = NA #item213.cast$
                            ,"High.Rise.n"     = NA #item213.cast$
                            ,"n"               = item213.cast$`n_All Sizes`
                            )
# row ordering example code
levels(item213.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item213.table <- item213.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item213.table <- data.frame(item213.table)

item213.table.MF <- item213.table[which(item213.table$BuildingType == "Multifamily")
                                  ,which(colnames(item213.table) %notin% c("BuildingType"))]
exportTable(item213.table.MF, "MF", "Table 5", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item213.summary <- proportions_two_groups_unweighted(CustomerLevelData = item213.data
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
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
item213.all.vintages <- item213.all.vintages[which(item213.all.vintages$HomeType != "Total"),]

item213.all.sizes <- proportions_one_group(CustomerLevelData = item213.data
                                           ,valueVariable = 'Number.of.Units'
                                           ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                           ,total.name = "All Sizes"
                                           ,columnName = 'HomeType'
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)

item213.final <- rbind.data.frame(item213.summary, item213.all.vintages, item213.all.sizes, stringsAsFactors = F)
item213.final$HomeYearBuilt_bins_MF[which(item213.final$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"

item213.cast <- dcast(setDT(item213.final)
                      ,formula = BuildingType + HomeYearBuilt_bins_MF ~ HomeType
                      ,value.var = c("Percent", "SE", "Count", "n"))
colnames(item213.cast)
item213.table <- data.frame("BuildingType" = item213.cast$BuildingType
                            ,"Housing.Vintage" = item213.cast$HomeYearBuilt_bins_MF
                            ,"Low.Rise.1.3"    = item213.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.SE"     = item213.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.n"      = item213.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6"    = item213.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.SE"     = item213.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.n"      = item213.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.7.Plus"= NA #item213.cast$
                            ,"High.Rise.SE"    = NA #item213.cast$
                            ,"High.Rise.n"     = NA #item213.cast$
                            ,"n"               = item213.cast$`n_All Sizes`
)
# row ordering example code
levels(item213.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item213.table <- item213.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item213.table <- data.frame(item213.table)

item213.table.MF <- item213.table[which(item213.table$BuildingType == "Multifamily")
                                  ,which(colnames(item213.table) %notin% c("BuildingType"))]
exportTable(item213.table.MF, "MF", "Table 5", weighted = FALSE)





#############################################################################################
#Item 217: DISTRIBUTION OF UNIT TYPES BY VINTAGE (MF Table 9)
#############################################################################################
#subset to columns needed for analysis
#from buildings interview data:
item217.dat <- buildings.interview.dat[which(colnames(buildings.interview.dat) %in% c("CK_Building_ID"
                                                                                      ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataNumberOfFourPlusBedroomUnitsInAuditedBuilding"
                                                                                      ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataNumberOfOneBedroomUnitsInAuditedBuilding"
                                                                                      ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataNumberOfStudioUnitsInAuditedBuilding"
                                                                                      ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataNumberOfThreeBedroomUnitsInAuditedBuilding"
                                                                                      ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataNumberOfTwoBedroomUnitsInAuditedBuilding"))]
names(item217.dat) <- c("Number.of.4.Plus.Bedroom.Units"
                        ,"Number.of.1.Bedroom.Units"
                        ,"Number.of.Studio.Units"
                        ,"Number.of.3.Bedroom.Units"
                        ,"Number.of.2.Bedroom.Units"
                        ,"CK_Building_ID")

item217.dat1 <- item217.dat[which(!is.na(item217.dat$Number.of.Studio.Units)),]
item217.melt <- melt(item217.dat1, id.vars = "CK_Building_ID")
names(item217.melt) <- c("CK_Building_ID", "Number.of.Units", "Count")

item217.merge <- left_join(rbsa.dat, item217.melt)
item217.merge <- item217.merge[which(!is.na(item217.merge$Number.of.Units)),]
item217.merge <- item217.merge[which(!is.na(item217.merge$Count)),]
item217.merge <- item217.merge[which(!is.na(item217.merge$HomeYearBuilt)),]


################################################
# Adding pop and sample sizes for weights
################################################
item217.data <- weightedData(item217.merge[-which(colnames(item217.merge) %in% c("Number.of.Units"
                                                                                 ,"Count"))])
item217.data <- left_join(item217.data, item217.merge[which(colnames(item217.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Number.of.Units"
                                                                                           ,"Count"))])
item217.data$count <- 1
colnames(item217.data)
#######################
# Weighted Analysis
#######################
item217.summary <- proportionRowsAndColumns1(CustomerLevelData = item217.data
                                             ,valueVariable = 'Count'
                                             ,columnVariable = "HomeYearBuilt_bins_MF"
                                             ,rowVariable = "Number.of.Units"
                                             ,aggregateColumnName = "Remove")
item217.summary <- item217.summary[which(item217.summary$HomeYearBuilt_bins_MF != "Remove"),]

item217.all.vintages <- proportions_one_group(CustomerLevelData = item217.data
                                              ,valueVariable = 'Count'
                                              ,groupingVariable = 'Number.of.Units'
                                              ,total.name = "All Vintages"
                                              ,columnName = "HomeYearBuilt_bins_MF"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
item217.all.vintages <- item217.all.vintages[which(item217.all.vintages$Number.of.Units != "Total"),]

item217.final <- rbind.data.frame(item217.summary, item217.all.vintages, stringsAsFactors = F)

item217.cast <- dcast(setDT(item217.final)
                      ,formula = BuildingType + HomeYearBuilt_bins_MF + n + N~ Number.of.Units
                      ,value.var = c("w.percent","w.SE","count"))
item217.table <- data.frame("BuildingType" = item217.cast$BuildingType
                            ,"Housing.Vintage" = item217.cast$HomeYearBuilt_bins_MF
                            ,"Percent.Studio"  = item217.cast$w.percent_Number.of.Studio.Units
                            ,"SE.Studio"       = item217.cast$w.SE_Number.of.Studio.Units
                            ,"Percent.One.Bedroom" = item217.cast$w.percent_Number.of.1.Bedroom.Units
                            ,"SE.One.Bedroom"      = item217.cast$w.SE_Number.of.1.Bedroom.Units
                            ,"Percent.Two.Bedroom" = item217.cast$w.percent_Number.of.2.Bedroom.Units
                            ,"SE.Two.Bedroom"      = item217.cast$w.SE_Number.of.2.Bedroom.Units
                            ,"Percent.Three.Bedroom" = item217.cast$w.percent_Number.of.3.Bedroom.Units
                            ,"SE.Three.Bedroom"      = item217.cast$w.SE_Number.of.3.Bedroom.Units
                            ,"Percent.Four.Plus.Bedrooms"  = item217.cast$w.percent_Number.of.4.Plus.Bedroom.Units
                            ,"SE.Four.Plus.Bedrooms"       = item217.cast$w.SE_Number.of.4.Plus.Bedroom.Units
                            ,"n"                           = item217.cast$n)
# row ordering example code
levels(item217.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item217.table <- item217.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item217.table <- data.frame(item217.table)

item217.table.MF <- item217.table[which(item217.table$BuildingType == "Multifamily")
                                  ,which(colnames(item217.table) %notin% c("BuildingType"))]
