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
source("Code/Table Code/Weighting Implementation - MF-BLDG.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) 
rbsa.dat.site <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat[grep("bldg",rbsa.dat$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
buildings.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.interview.export))
#clean cadmus IDs
buildings.interview.dat$CK_Building_ID <- trimws(toupper(buildings.interview.dat$CK_BuildingID))


#############################################################################################
#Item 228: REPORTED BUILDING VACANCY RATE BY VINTAGE (MF Table 20)
#############################################################################################
item228.dat <- unique(buildings.interview.dat[which(colnames(buildings.interview.dat) %in% c("CK_Building_ID"
                                                                                     ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataVacancy_PercentOfUnitsNotOccupiedInAuditedBuilding"))])


colnames(item228.dat) <- c("PercentUnoccupied"
                           ,"CK_Building_ID")

which(duplicated(item228.dat$CK_Building_ID))

#merge together analysis data with cleaned RBSA data
item228.dat1 <- left_join(rbsa.dat.bldg, item228.dat)

#Subset to Multifamily
item228.dat2 <- item228.dat1[grep("Multifamily", item228.dat1$BuildingType),]

item228.dat2$PercentUnoccupied <- as.numeric(as.character(item228.dat2$PercentUnoccupied))
item228.dat3 <- item228.dat2[which(item228.dat2$PercentUnoccupied %notin% c("N/A",NA)),]
item228.dat4 <- item228.dat3[which(item228.dat3$HomeYearBuilt %notin% c("N/A",NA)),]



################################################
# Adding pop and sample sizes for weights
################################################
item228.data <- weightedData(item228.dat4[-which(colnames(item228.dat4) %in% c("PercentUnoccupied"))])
item228.data <- left_join(item228.data, item228.dat4[which(colnames(item228.dat4) %in% c("CK_Cadmus_ID"
                                                                                           ,"PercentUnoccupied"))])


item228.data$PercentUnoccupied <- as.numeric(as.character(item228.data$PercentUnoccupied))
item228.data$count <- 1
#######################
# weighted analysis
#######################
item228.table <- mean_one_group(CustomerLevelData = item228.data
                                ,valueVariable    = 'PercentUnoccupied'
                                ,byVariable       = 'HomeYearBuilt_bins_MF'
                                ,aggregateRow     = "All Vintages")
# row ordering example code
unique(item228.table$HomeYearBuilt_bins_MF)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item228.table <- item228.table %>% mutate(HomeYearBuilt_bins_MF = factor(HomeYearBuilt_bins_MF, levels = rowOrder)) %>% arrange(HomeYearBuilt_bins_MF)  
item228.table <- data.frame(item228.table[which(names(item228.table) != "BuildingType")])

exportTable(item228.table, "MF", "Table 20", weighted = TRUE)

#######################
# unweighted analysis
#######################
item228.table <- mean_one_group_unweighted(CustomerLevelData = item228.data
                                           ,valueVariable    = 'PercentUnoccupied'
                                           ,byVariable       = 'HomeYearBuilt_bins_MF'
                                           ,aggregateRow     = "All Vintages")
# row ordering example code
unique(item228.table$HomeYearBuilt_bins_MF)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item228.table <- item228.table %>% mutate(HomeYearBuilt_bins_MF = factor(HomeYearBuilt_bins_MF, levels = rowOrder)) %>% arrange(HomeYearBuilt_bins_MF)  
item228.table <- data.frame(item228.table[which(names(item228.table) != "BuildingType")])

exportTable(item228.table, "MF", "Table 20", weighted = FALSE)
