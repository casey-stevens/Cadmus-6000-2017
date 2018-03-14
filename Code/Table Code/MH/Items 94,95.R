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
# appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
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
unique(item94.dat2$Stove.Fuel)
item94.dat2$Stove.Fuel[which(item94.dat2$Stove.Fuel %in% c("Gas", "gas", "natural gas", "Natural Gas"))] <- "Gas"
item94.dat3 <- item94.dat2[which(item94.dat2$Stove.Fuel %notin% c("No oven ", "No oven", "No Oven", "No Cooktop", "No Stove", NA)),]
item94.dat3$Stove.Fuel[which(item94.dat3$Stove.Fuel %notin% c("Electric", "Gas", "Propane"))] <- "Other"
unique(item94.dat3$Stove.Fuel)


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
                                      ,total.name       = 'Total')


unique(item94.final$Stove.Fuel)
rowOrder <- c("Electric"
              ,"Gas"
              ,"Propane"
              ,"Other"
              ,"Total")
item94.final <- item94.final %>% mutate(Stove.Fuel = factor(Stove.Fuel, levels = rowOrder)) %>% arrange(Stove.Fuel)  
item94.final <- data.frame(item94.final)


item94.final.SF <- item94.final[which(item94.final$BuildingType == "Single Family")
                                ,-which(colnames(item94.final) %in% c("BuildingType"))]
item94.final.MH <- item94.final[which(item94.final$BuildingType == "Manufactured")
                                ,-which(colnames(item94.final) %in% c("BuildingType"))]

# exportTable(item94.final.SF, "SF", "Table 101", weighted = TRUE)
exportTable(item94.final.MH, "MH", "Table 82", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item94.final <- proportions_one_group(CustomerLevelData = item94.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'Stove.Fuel'
                                      ,total.name       = 'Total'
                                      ,weighted         = FALSE)


unique(item94.final$Stove.Fuel)
rowOrder <- c("Electric"
              ,"Gas"
              ,"Propane"
              ,"Other"
              ,"Total")
item94.final <- item94.final %>% mutate(Stove.Fuel = factor(Stove.Fuel, levels = rowOrder)) %>% arrange(Stove.Fuel)  
item94.final <- data.frame(item94.final)


item94.final.SF <- item94.final[which(item94.final$BuildingType == "Single Family")
                                ,-which(colnames(item94.final) %in% c("BuildingType"))]
item94.final.MH <- item94.final[which(item94.final$BuildingType == "Manufactured")
                                ,-which(colnames(item94.final) %in% c("BuildingType"))]

# exportTable(item94.final.SF, "SF", "Table 101", weighted = FALSE)
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
unique(item95.dat2$Oven.Fuel)
item95.dat2$Oven.Fuel[which(item95.dat2$Oven.Fuel %in% c("Gas", "gas", "natural gas", "Natural Gas"))] <- "Gas"
item95.dat3 <- item95.dat2[which(item95.dat2$Oven.Fuel %notin% c("No oven ", "No oven", "No Oven", "No Cooktop", "No Stove", NA)),]
unique(item95.dat3$Oven.Fuel)

item95.dat3$Oven.Fuel[which(item95.dat3$Oven.Fuel %notin% c("Electric", "Gas", "Propane"))] <- "Other"
unique(item95.dat3$Oven.Fuel)


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
                                      ,total.name       = 'Total')

item95.final.SF <- item95.final[which(item95.final$BuildingType == "Single Family")
                                ,-which(colnames(item95.final) %in% c("BuildingType"))]
item95.final.MH <- item95.final[which(item95.final$BuildingType == "Manufactured")
                                ,-which(colnames(item95.final) %in% c("BuildingType"))]

# exportTable(item95.final.SF, "SF", "Table 102", weighted = TRUE)
exportTable(item95.final.MH, "MH", "Table 83", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item95.final <- proportions_one_group(CustomerLevelData = item95.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'Oven.Fuel'
                                      ,total.name       = 'Total'
                                      ,weighted         = FALSE)

item95.final.SF <- item95.final[which(item95.final$BuildingType == "Single Family")
                                ,-which(colnames(item95.final) %in% c("BuildingType"))]
item95.final.MH <- item95.final[which(item95.final$BuildingType == "Manufactured")
                                ,-which(colnames(item95.final) %in% c("BuildingType"))]

# exportTable(item95.final.SF, "SF", "Table 102", weighted = FALSE)
exportTable(item95.final.MH, "MH", "Table 83", weighted = FALSE)

