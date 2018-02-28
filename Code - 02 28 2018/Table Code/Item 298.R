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
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))





#############################################################################################
#Item 298: IN-UNIT LAUNDRY CHARACTERISTICS (MF Table 92)
#############################################################################################
#subset to correct columns for analysis
item298.dat <- sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                              ,"CK_SiteID"
                                                                              ,"INTRVW_CUST_RES_HomeandEnergyUseHome_ClothesWasherLoadsPerWeek"
                                                                              ,"INTRVW_CUST_RES_HomeandEnergyUseHome_PercentOfLoadsThatGoInDryer"
                                                                              ,""))]
#rename columns
colnames(item298.dat) <- c("CK_Cadmus_ID"
                           ,"CK_Site_ID"
                           ,"Clothes.Washer.Loads.per.Week"
                           ,"Dryer.Loads.per.Washer.Load")

#remove NA values for clothes washers
item298.dat1 <- unique(item298.dat[which(!(is.na(item298.dat$Clothes.Washer.Loads.per.Week))),])
#view if any IDs are duplicated (none should be)
which(duplicated(item298.dat1$CK_Cadmus_ID))

#join cleaned RBSA data onto subsetted data for item
item298.dat2 <- left_join(rbsa.dat, item298.dat1, by = "CK_Cadmus_ID")

#subset to only multifamily units
item298.dat3 <- item298.dat2[grep("Multifamily", item298.dat2$BuildingType),]
#view unique values of clothes washers
unique(item298.dat3$Clothes.Washer.Loads.per.Week)
unique(item298.dat3$Dryer.Loads.per.Washer.Load)

#remove any sites that do not have clothes washers
item298.dat4 <- item298.dat3[which(item298.dat3$Clothes.Washer.Loads.per.Week != "No Washing Machine"),]
item298.dat4$Clothes.Washer.Loads.per.Week <- as.numeric(as.character(item298.dat4$Clothes.Washer.Loads.per.Week))
item298.dat4$Dryer.Loads.per.Washer.Load   <- as.numeric(as.character(item298.dat4$Dryer.Loads.per.Washer.Load)) / 100

################################################
# Adding pop and sample sizes for weights
################################################
item298.data <- weightedData(item298.dat4[-which(colnames(item298.dat4) %in% c("CK_Site_ID"
                                                                              ,"Clothes.Washer.Loads.per.Week"
                                                                              ,"Dryer.Loads.per.Washer.Load"))])
item298.data <- left_join(item298.data, item298.dat4[which(colnames(item298.dat4) %in% c("CK_Cadmus_ID"
                                                                                       ,"CK_Site_ID"
                                                                                       ,"Clothes.Washer.Loads.per.Week"
                                                                                       ,"Dryer.Loads.per.Washer.Load"))])
item298.data$count <- 1
colnames(item298.data)

#######################
# Weighted Analysis
#######################
item298.washer.loads <- mean_one_group(CustomerLevelData = item298.data
                                       ,valueVariable = 'Clothes.Washer.Loads.per.Week'
                                       ,byVariable = "BuildingType"
                                       ,aggregateRow = "Remove")
item298.dryer.loads <- mean_one_group(CustomerLevelData = item298.data
                                      ,valueVariable = "Dryer.Loads.per.Washer.Load"
                                      ,byVariable = 'BuildingType'
                                      ,aggregateRow = "Remove")
item298.final <- rbind.data.frame(item298.washer.loads, item298.dryer.loads, stringsAsFactors = F)
item298.final.MF <- item298.final[which(colnames(item298.final) %notin% c("BuildingType"))]

exportTable(item298.final.MF, "MF", "Table 92", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item298.washer.loads <- mean_one_group_unweighted(CustomerLevelData = item298.data
                                       ,valueVariable = 'Clothes.Washer.Loads.per.Week'
                                       ,byVariable = "BuildingType"
                                       ,aggregateRow = "Remove")
item298.dryer.loads <- mean_one_group_unweighted(CustomerLevelData = item298.data
                                      ,valueVariable = "Dryer.Loads.per.Washer.Load"
                                      ,byVariable = 'BuildingType'
                                      ,aggregateRow = "Remove")
item298.final <- rbind.data.frame(item298.washer.loads, item298.dryer.loads, stringsAsFactors = F)
item298.final.MF <- item298.final[which(colnames(item298.final) %notin% c("BuildingType"))]

exportTable(item298.final.MF, "MF", "Table 92", weighted = FALSE)
