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


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) 
rbsa.dat.site <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat[grep("bldg",rbsa.dat$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
# buildings.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.interview.export))
#clean cadmus IDs
buildings.interview.dat$CK_Building_ID <- trimws(toupper(buildings.interview.dat$CK_BuildingID))


#############################################################################################
#Item 228A: REPORTED BUILDING VACANCY RATE BY VINTAGE (MF Table 20A)
#############################################################################################
item228A.dat <- unique(buildings.interview.dat[which(colnames(buildings.interview.dat) %in% c("CK_Building_ID"
                                                                                             ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataVacancy_PercentOfUnitsNotOccupiedInAuditedBuilding"))])


colnames(item228A.dat) <- c("PercentUnoccupied"
                           ,"CK_Building_ID")

which(duplicated(item228A.dat$CK_Building_ID))

#merge together analysis data with cleaned RBSA data
item228A.dat1 <- left_join(rbsa.dat.bldg, item228A.dat)

#Subset to Multifamily
item228A.dat2 <- item228A.dat1[grep("Multifamily", item228A.dat1$BuildingType),]

item228A.dat2$PercentUnoccupied <- as.numeric(as.character(item228A.dat2$PercentUnoccupied))
item228A.dat3 <- item228A.dat2[which(item228A.dat2$PercentUnoccupied %notin% c("N/A",NA)),]
item228A.dat4 <- item228A.dat3[which(item228A.dat3$HomeYearBuilt %notin% c("N/A",NA)),]
item228A.dat4 <- item228A.dat4[which(item228A.dat4$Category == "PSE"),]


################################################
# Adding pop and sample sizes for weights
################################################
item228A.data <- weightedData(item228A.dat4[-which(colnames(item228A.dat4) %in% c("PercentUnoccupied"
                                                                                  ,"Category"))])
item228A.data <- left_join(item228A.data, item228A.dat4[which(colnames(item228A.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"PercentUnoccupied"
                                                                                         ,"Category"))])


item228A.data$PercentUnoccupied <- as.numeric(as.character(item228A.data$PercentUnoccupied))
item228A.data$count <- 1
#######################
# weighted analysis
#######################
item228A.table <- mean_one_group(CustomerLevelData = item228A.data
                                ,valueVariable    = 'PercentUnoccupied'
                                ,byVariable       = 'HomeYearBuilt_bins_MF'
                                ,aggregateRow     = "All Vintages")
# row ordering example code
unique(item228A.table$HomeYearBuilt_bins_MF)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item228A.table <- item228A.table %>% mutate(HomeYearBuilt_bins_MF = factor(HomeYearBuilt_bins_MF, levels = rowOrder)) %>% arrange(HomeYearBuilt_bins_MF)  
item228A.table <- data.frame(item228A.table[which(names(item228A.table) != "BuildingType")])

exportTable(item228A.table, "MF", "Table 20A", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# unweighted analysis
#######################
item228A.table <- mean_one_group_unweighted(CustomerLevelData = item228A.data
                                           ,valueVariable    = 'PercentUnoccupied'
                                           ,byVariable       = 'HomeYearBuilt_bins_MF'
                                           ,aggregateRow     = "All Vintages")
# row ordering example code
unique(item228A.table$HomeYearBuilt_bins_MF)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item228A.table <- item228A.table %>% mutate(HomeYearBuilt_bins_MF = factor(HomeYearBuilt_bins_MF, levels = rowOrder)) %>% arrange(HomeYearBuilt_bins_MF)  
item228A.table <- data.frame(item228A.table[which(names(item228A.table) != "BuildingType")])

exportTable(item228A.table, "MF", "Table 20A", weighted = FALSE,OS = T, osIndicator = "PSE")




#############################################################################################
#Item 228B: REPORTED BUILDING VACANCY RATE BY VINTAGE (MF Table 20B)
#############################################################################################
item228B.dat <- unique(buildings.interview.dat[which(colnames(buildings.interview.dat) %in% c("CK_Building_ID"
                                                                                     ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataVacancy_PercentOfUnitsNotOccupiedInAuditedBuilding"))])


colnames(item228B.dat) <- c("PercentUnoccupied"
                           ,"CK_Building_ID")

which(duplicated(item228B.dat$CK_Building_ID))

#merge together analysis data with cleaned RBSA data
item228B.dat1 <- left_join(rbsa.dat.bldg, item228B.dat)

#Subset to Multifamily
item228B.dat2 <- item228B.dat1[grep("Multifamily", item228B.dat1$BuildingType),]

item228B.dat2$PercentUnoccupied <- as.numeric(as.character(item228B.dat2$PercentUnoccupied))
item228B.dat3 <- item228B.dat2[which(item228B.dat2$PercentUnoccupied %notin% c("N/A",NA)),]
item228B.dat4 <- item228B.dat3[which(item228B.dat3$HomeYearBuilt %notin% c("N/A",NA)),]



################################################
# Adding pop and sample sizes for weights
################################################
item228B.data <- weightedData(item228B.dat4[-which(colnames(item228B.dat4) %in% c("PercentUnoccupied"
                                                                                  ,"Category"))])
item228B.data <- left_join(item228B.data, item228B.dat4[which(colnames(item228B.dat4) %in% c("CK_Cadmus_ID"
                                                                                           ,"PercentUnoccupied"
                                                                                           ,"Category"))])


item228B.data$PercentUnoccupied <- as.numeric(as.character(item228B.data$PercentUnoccupied))
item228B.data$count <- 1
#######################
# weighted analysis
#######################
item228B.table <- mean_one_group(CustomerLevelData = item228B.data
                                ,valueVariable    = 'PercentUnoccupied'
                                ,byVariable       = 'Category'
                                ,aggregateRow     = "Remove")
item228B.table <- item228B.table[which(item228B.table$Category != "Remove"),]
# row ordering example code
unique(item228B.table$Category)
rowOrder <- c("PSE"
              ,"PSE KING COUNTY"
              ,"PSE NON-KING COUNTY"
              ,"2017 RBSA PS")
item228B.table <- item228B.table %>% mutate(Category = factor(Category, levels = rowOrder)) %>% arrange(Category)  
item228B.table <- data.frame(item228B.table[which(names(item228B.table) != "BuildingType")])

exportTable(item228B.table, "MF", "Table 20B", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# unweighted analysis
#######################
item228B.table <- mean_one_group_unweighted(CustomerLevelData = item228B.data
                                           ,valueVariable    = 'PercentUnoccupied'
                                           ,byVariable       = 'Category'
                                           ,aggregateRow     = "Remove")
item228B.table <- item228B.table[which(item228B.table$Category != "Remove"),]
# row ordering example code
unique(item228B.table$Category)
rowOrder <- c("PSE"
              ,"PSE KING COUNTY"
              ,"PSE NON-KING COUNTY"
              ,"2017 RBSA PS")
item228B.table <- item228B.table %>% mutate(Category = factor(Category, levels = rowOrder)) %>% arrange(Category)  
item228B.table <- data.frame(item228B.table[which(names(item228B.table) != "BuildingType")])

exportTable(item228B.table, "MF", "Table 20B", weighted = FALSE,OS = T, osIndicator = "PSE")
