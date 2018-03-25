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
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
# mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

#bring in one-line file for heat loss info
billing.dat <- read.xlsx(xlsxFile = file.path(filepathBillingData, billing.data))
billing.dat$CK_Cadmus_ID <- trimws(toupper(billing.dat$CK_Cadmus_ID))



#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item172.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item172.dat.11 <- item172.dat.1[which(item172.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item172.dat.12 <- item172.dat.11[which(item172.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item172.dat.12$CK_Cadmus_ID))

#check uniques
unique(item172.dat.12$Primary.Heating.System)
item172.dat.12$count <- 1

#subset to only electric primary heating systems
item172.dat.13 <- unique(item172.dat.12[which(item172.dat.12$Heating.Fuel == "Electric"),])

#summaryise up to the customer level - keeping heating fuel information
item172.sum <- summarise(group_by(item172.dat.13, CK_Cadmus_ID, Heating.Fuel)
                         ,Count = sum(count))
item172.sum$Count <- 1

#check uniques
which(duplicated(item172.sum$CK_Cadmus_ID)) #none are duplicated!
unique(item172.sum$Heating.Fuel)

#merge with cleaned rbsa data and subset to complete data
item172.merge <- left_join(rbsa.dat, item172.sum)
item172.merge <- item172.merge[which(!is.na(item172.merge$Heating.Fuel)),]

#re-assign mechanical data for later use
item172.mechanical <- item172.merge





#############################################################################################
# 
# EUI Data
# 
#############################################################################################
item172.dat <- billing.dat[which(colnames(billing.dat) %in% c("CK_Cadmus_ID"
                                                              ,"UsageNAC_kWh"))]
item172.dat1 <- left_join(item172.dat, item172.mechanical)
item172.dat2 <- left_join(rbsa.dat, item172.dat1)

item172.dat3 <- item172.dat2[which(item172.dat2$UsageNAC_kWh > 0),]
item172.dat4 <- item172.dat3[grep("site",item172.dat3$CK_Building_ID, ignore.case = T),]

which(duplicated(item172.dat4$CK_Cadmus_ID))


################################################
# Adding pop and sample sizes for weights
################################################
item172.data <- weightedData(item172.dat4[-which(colnames(item172.dat4) %in% c("UsageNAC_kWh"
                                                                               ,"Count"
                                                                               ,"Heating.Fuel"))])
item172.data <- left_join(item172.data, item172.dat4[which(colnames(item172.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"UsageNAC_kWh"
                                                                                         ,"Count"
                                                                                         ,"Heating.Fuel"))])
item172.data$count <- 1
#######################
# Weighted Analysis
#######################
item172.final <- mean_one_group(CustomerLevelData = item172.data
                                ,valueVariable = 'UsageNAC_kWh'
                                ,byVariable = 'State'
                                ,aggregateRow = "Region")
item172.final.SF <- item172.final[which(item172.final$BuildingType == "Single Family"),
                                  -which(names(item172.final) == "BuildingType")]
exportTable(item172.final.SF, "SF", "Table B-17", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item172.final <- mean_one_group_unweighted(CustomerLevelData = item172.data
                                ,valueVariable = 'UsageNAC_kWh'
                                ,byVariable = 'State'
                                ,aggregateRow = "Region")
item172.final.SF <- item172.final[which(item172.final$BuildingType == "Single Family"),
                                  -which(names(item172.final) == "BuildingType")]
exportTable(item172.final.SF, "SF", "Table B-17", weighted = FALSE)