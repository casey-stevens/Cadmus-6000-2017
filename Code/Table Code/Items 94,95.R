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
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, "Appliances_CS.xlsx")
                            , sheet = "Sheet1")
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))


#############################################################################################
#Item 94: DISTRIBUTION OF COOK TOP FUEL BY TYPE (SF table 101, MH table 82)
#############################################################################################
#subset to columns needed for analysis
item94.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Stove.Fuel"
                                                                   ,""))]
item94.dat$count <- 1

item94.dat0 <- item94.dat[which(item94.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item94.dat1 <- left_join(rbsa.dat, item94.dat0, by = "CK_Cadmus_ID")
item94.dat2 <- item94.dat1[which(item94.dat1$Type == "Stove/Oven"),]

item94.dat3 <- item94.dat2[which(item94.dat2$Stove.Fuel %notin% c("No Stove", "No Cooktop", NA)),]
item94.dat3$Stove.Fuel[which(item94.dat3$Stove.Fuel %notin% c("Electric", "Gas", "Propane"))] <- "Other"


################################################
# Adding pop and sample sizes for weights
################################################
item94.data <- weightedData(item94.dat3[-which(colnames(item94.dat3) %in% c("count"
                                                                              ,"Type"
                                                                              ,"Stove.Fuel"))])
item94.data <- left_join(item94.data, item94.dat3[which(colnames(item94.dat3) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Type"
                                                                                       ,"Stove.Fuel"))])
item94.data$count <- 1

#######################
# Weighted Analysis
#######################
item94.final <- proportions_one_group(CustomerLevelData = item94.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'Stove.Fuel'
                                      ,total.name       = 'Total'
                                      ,columnName       = 'Remove')

item94.final.SF <- item94.final[which(item94.final$BuildingType == "Single Family")
                                ,-which(colnames(item94.final) %in% c("BuildingType"
                                                                      ,"Remove"))]
item94.final.MH <- item94.final[which(item94.final$BuildingType == "Manufactured")
                                ,-which(colnames(item94.final) %in% c("BuildingType"
                                                                      ,"Remove"))]

exportTable(item94.final.SF, "SF", "Table 101", weighted = TRUE)
exportTable(item94.final.MH, "MH", "Table 82", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item94.final <- proportions_one_group(CustomerLevelData = item94.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'Stove.Fuel'
                                      ,total.name       = 'Total'
                                      ,columnName       = 'Remove'
                                      ,weighted         = FALSE)

item94.final.SF <- item94.final[which(item94.final$BuildingType == "Single Family")
                                ,-which(colnames(item94.final) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"Total.Count"))]
item94.final.MH <- item94.final[which(item94.final$BuildingType == "Manufactured")
                                ,-which(colnames(item94.final) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"Total.Count"))]

exportTable(item94.final.SF, "SF", "Table 101", weighted = FALSE)
exportTable(item94.final.MH, "MH", "Table 82", weighted = FALSE)





#############################################################################################
#Item 95: DISTRIBUTION OF STOVE FUEL BY TYPE (SF table 102, MH table 83)
#############################################################################################
#subset to columns needed for analysis
item95.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Oven.Fuel"
                                                                   ,""))]
item95.dat$count <- 1

item95.dat0 <- item95.dat[which(item95.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item95.dat1 <- left_join(rbsa.dat, item95.dat0, by = "CK_Cadmus_ID")
item95.dat2 <- item95.dat1[which(item95.dat1$Type == "Stove/Oven"),]

item95.dat3 <- item95.dat2[which(item95.dat2$Oven.Fuel %notin% c("No Stove", "No Cooktop", NA)),]
item95.dat3$Oven.Fuel[which(item95.dat3$Oven.Fuel %notin% c("Electric", "Gas", "Propane"))] <- "Other"


################################################
# Adding pop and sample sizes for weights
################################################
item95.data <- weightedData(item95.dat3[-which(colnames(item95.dat3) %in% c("count"
                                                                            ,"Type"
                                                                            ,"Oven.Fuel"))])
item95.data <- left_join(item95.data, item95.dat3[which(colnames(item95.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"count"
                                                                                     ,"Type"
                                                                                     ,"Oven.Fuel"))])
item95.data$count <- 1

#######################
# Weighted Analysis
#######################
item95.final <- proportions_one_group(CustomerLevelData = item95.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'Oven.Fuel'
                                      ,total.name       = 'Total'
                                      ,columnName       = 'Remove')

item95.final.SF <- item95.final[which(item95.final$BuildingType == "Single Family")
                                ,-which(colnames(item95.final) %in% c("BuildingType"
                                                                      ,"Remove"))]
item95.final.MH <- item95.final[which(item95.final$BuildingType == "Manufactured")
                                ,-which(colnames(item95.final) %in% c("BuildingType"
                                                                      ,"Remove"))]

exportTable(item95.final.SF, "SF", "Table 102", weighted = TRUE)
exportTable(item95.final.MH, "MH", "Table 83", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item95.final <- proportions_one_group(CustomerLevelData = item95.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'Oven.Fuel'
                                      ,total.name       = 'Total'
                                      ,columnName       = 'Remove'
                                      ,weighted         = FALSE)

item95.final.SF <- item95.final[which(item95.final$BuildingType == "Single Family")
                                ,-which(colnames(item95.final) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"Total.Count"))]
item95.final.MH <- item95.final[which(item95.final$BuildingType == "Manufactured")
                                ,-which(colnames(item95.final) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"Total.Count"))]

exportTable(item95.final.SF, "SF", "Table 102", weighted = FALSE)
exportTable(item95.final.MH, "MH", "Table 83", weighted = FALSE)
