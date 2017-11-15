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
item228.dat1 <- left_join(rbsa.dat, item228.dat)

#Subset to Multifamily
item228.dat2 <- item228.dat1[grep("Multifamily", item228.dat1$BuildingType),]
item228.dat3 <- item228.dat2[which(!is.na(item228.dat2$PercentUnoccupied)),]
item228.dat4 <- item228.dat3[which(!is.na(item228.dat3$HomeYearBuilt)),]



################################################
# Adding pop and sample sizes for weights
################################################
item228.data <- weightedData(item228.dat4[-which(colnames(item228.dat4) %in% c("PercentUnoccupied"))])
item228.data <- left_join(item228.data, item228.dat4[which(colnames(item228.dat4) %in% c("CK_Cadmus_ID"
                                                                                           ,"PercentUnoccupied"))])


item228.data$PercentUnoccupied <- item228.data$PercentUnoccupied / 100

#######################
# weighted analysis
#######################
item228.final <- mean_one_group(CustomerLevelData = item228.data
                                ,valueVariable = 'PercentUnoccupied'
                                ,byVariable = 'HomeYearBuilt_bins_MF'
                                ,aggregateRow = "All Vintages")
exportTable(item228.final, "MF", "Table 20", weighted = TRUE)

#######################
# unweighted analysis
#######################
item228.final <- mean_one_group_unweighted(CustomerLevelData = item228.data
                                ,valueVariable = 'PercentUnoccupied'
                                ,byVariable = 'HomeYearBuilt_bins_MF'
                                ,aggregateRow = "All Vintages")
exportTable(item228.final, "MF", "Table 20", weighted = FALSE)


# #Will remove NA's from this analysis since it appears zeros are legitamte averages
# 
# item228.dat3 <- item228.dat2[which(!is.na(item228.dat2$PercentUnoccupied)
#                                      & !is.na(item228.dat2$HomeYearBuilt_MF)),]
# 
# #summarise by vintage
# item228.vintage <- summarise(group_by(item228.dat3, HomeYearBuilt_MF)
#                            ,SampleSize = length(unique(CK_Cadmus_ID))
#                            ,Mean = mean(PercentUnoccupied)
#                            ,SE = sd(PercentUnoccupied) / sqrt(SampleSize))
# 
# item228.vintageAll <- summarise(item228.dat3
#                              ,SampleSize = length(unique(CK_Cadmus_ID))
#                              ,HomeYearBuilt_MF = "All Vintages"
#                              ,Mean = mean(PercentUnoccupied)
#                              ,SE = sd(PercentUnoccupied) / sqrt(SampleSize))
# 
# item228.table <- rbind.data.frame(item228.vintage,item228.vintageAll,stringsAsFactors = F)
# 
# item228.final <- data.frame("Vintage" = item228.table$HomeYearBuilt_MF
#                             ,"Percent" = item228.table$Mean
#                             ,"SE" = item228.table$SE
#                             ,"SampleSize" = item228.table$SampleSize
#                             ,stringsAsFactors = F)


