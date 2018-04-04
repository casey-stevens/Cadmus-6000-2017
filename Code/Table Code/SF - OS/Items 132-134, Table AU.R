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
# sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))

#Read in data for analysis
# survey.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Labeled and Translated")
#clean cadmus IDs
survey.dat$CK_Cadmus_ID <- trimws(toupper(survey.dat$NEXID))



# #############################################################################################
# #Item 132: AVERAGE COOLING THERMOSTAT SETPOINT BY STATE (SF table 139, MH table 114)
# #############################################################################################
# #subset to columns needed for analysis
# item132.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
#                                                                                      ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouCoolYourHomeWhatTemperatureDoYouTryToMaintain"
#                                                                                      ,""))])
# colnames(item132.dat) <- c("CK_Cadmus_ID"
#                            ,"Thermostat_Setpoint")
# item132.dat$count <- 1
# item132.dat$Thermostat_Setpoint <- as.numeric(as.character(item132.dat$Thermostat_Setpoint))
# #remove any repeat header rows from exporting
# item132.dat0 <- item132.dat[which(item132.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# #merge together analysis data with cleaned RBSA data
# item132.dat1 <- left_join(rbsa.dat, item132.dat0, by = "CK_Cadmus_ID")
# 
# 
# unique(item132.dat1$Thermostat_Setpoint)
# 
# item132.dat2.0 <- item132.dat1[which(!(is.na(item132.dat1$Thermostat_Setpoint))),]
# item132.dat2 <- item132.dat2.0[which(item132.dat2.0$Thermostat_Setpoint != 0),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item132.data <- weightedData(item132.dat2[-which(colnames(item132.dat2) %in% c("Thermostat_Setpoint"
#                                                                                ,"count"))])
# item132.data <- left_join(item132.data, unique(item132.dat2[which(colnames(item132.dat2) %in% c("CK_Cadmus_ID"
#                                                                                          ,"Thermostat_Setpoint"
#                                                                                          ,"count"))]))
# 
# item132.data$count <- 1
# #######################
# # Weighted Analysis
# #######################
# item132.final <- mean_one_group(item132.data
#                                 ,valueVariable = 'Thermostat_Setpoint'
#                                 ,byVariable = 'State'
#                                 ,aggregateRow = 'Region')
# 
# item132.final.SF <- item132.final[which(item132.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item132.final) %in% c("BuildingType"))]
# item132.final.MH <- item132.final[which(item132.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item132.final) %in% c("BuildingType"))]
# 
# exportTable(item132.final.SF, "SF", "Table 139", weighted = TRUE)
# # exportTable(item132.final.MH, "MH", "Table 114", weighted = TRUE)
# 
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# item132.final <- mean_one_group_unweighted(item132.data
#                                            ,valueVariable = 'Thermostat_Setpoint'
#                                            ,byVariable = 'State'
#                                            ,aggregateRow = 'Region')
# 
# item132.final.SF <- item132.final[which(item132.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item132.final) %in% c("BuildingType"))]
# item132.final.MH <- item132.final[which(item132.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item132.final) %in% c("BuildingType"))]
# 
# exportTable(item132.final.SF, "SF", "Table 139", weighted = FALSE)
# # exportTable(item132.final.MH, "MH", "Table 114", weighted = FALSE)
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 133: PERCENTAGE OF HOMES REPORTING A COOLING THERMOSTAT SETUP BY STATE (SF table 140, MH table 115)
# #############################################################################################
# #subset to columns needed for analysis
# item133.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
#                                                                                      ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouCoolYourHomeWhatTemperatureDoYouTryToMaintain"
#                                                                                      ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouGoToBedWhatDoYouSetTheThermostatToForCooling"))])
# colnames(item133.dat) <- c("CK_Cadmus_ID", "Thermostat_Setpoint", "Nighttime_Cooling")
# item133.dat$count <- 1
# 
# #remove any repeat header rows from exporting
# item133.dat0 <- item133.dat[which(item133.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# #merge together analysis data with cleaned RBSA data
# item133.dat1 <- left_join(item133.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# item133.dat1$Thermostat_Setpoint <- as.numeric(as.character(item133.dat1$Thermostat_Setpoint))
# item133.dat1$Nighttime_Cooling <- as.numeric(as.character(item133.dat1$Nighttime_Cooling))
# 
# 
# item133.dat2.0 <- item133.dat1[which(!(is.na(item133.dat1$Thermostat_Setpoint))),]
# item133.dat2 <- item133.dat2.0[which(item133.dat2.0$Thermostat_Setpoint > 0),]
# unique(item133.dat2$Thermostat_Setpoint)
# unique(item133.dat2$Nighttime_Cooling)
# 
# item133.dat3.0 <- item133.dat2[which(!(is.na(item133.dat2$Nighttime_Cooling))),]
# item133.dat3 <- item133.dat3.0[which(item133.dat3.0$Nighttime_Cooling > 0),]
# 
# item133.dat3$Cooling.Setup <- 0
# item133.dat3$Cooling.Setup[which(item133.dat3$Nighttime_Cooling > item133.dat3$Thermostat_Setpoint)] <- 1
# 
# item133.sum <- summarise(group_by(item133.dat3, CK_Cadmus_ID)
#                          ,Ind = sum(unique(Cooling.Setup)))
# unique(item133.sum$Ind)
# 
# item133.merge <- left_join(rbsa.dat, item133.sum)
# item133.merge <- item133.merge[which(!is.na(item133.merge$Ind)),]
# # item133.merge$Ind[which(is.na(item133.merge$Ind))] <- 0
# 
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item133.data <- weightedData(item133.merge[-which(colnames(item133.merge) %in% c("Ind"))])
# item133.data <- left_join(item133.data, unique(item133.merge[which(colnames(item133.merge) %in% c("CK_Cadmus_ID","Ind"))]))
# item133.data$count <- 1
# item133.data$Count <- 1
# #######################
# # Weighted Analysis
# #######################
# item133.final <- proportions_one_group(CustomerLevelData = item133.data
#                                        ,valueVariable = 'Ind'
#                                        ,groupingVariable = 'State'
#                                        ,total.name = "Region"
#                                        ,weighted = TRUE)
# 
# item133.final.SF <- item133.final[which(item133.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item133.final) %in% c("BuildingType"))]
# item133.final.MH <- item133.final[which(item133.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item133.final) %in% c("BuildingType"))]
# 
# exportTable(item133.final.SF, "SF", "Table 140", weighted = TRUE)
# # exportTable(item133.final.MH, "MH", "Table 115", weighted = TRUE)
# 
# #######################
# # Unweighted Analysis
# #######################
# item133.final <- proportions_one_group(CustomerLevelData = item133.data
#                                        ,valueVariable = 'Ind'
#                                        ,groupingVariable = 'State'
#                                        ,total.name = "Region"
#                                        ,weighted = FALSE)
# 
# item133.final.SF <- item133.final[which(item133.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item133.final) %in% c("BuildingType"))]
# item133.final.MH <- item133.final[which(item133.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item133.final) %in% c("BuildingType"))]
# 
# exportTable(item133.final.SF, "SF", "Table 140", weighted = FALSE)
# # exportTable(item133.final.MH, "MH", "Table 115", weighted = FALSE)
# 
# 
# 
# #############################################################################################
# # Table AU: AVERAGE SIZE OF COOLING SETBACK BY STATE (SF table 138, MH table 113)
# #############################################################################################
# #subset to columns needed for analysis
# tableAU.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
#                                                                                      ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouCoolYourHomeWhatTemperatureDoYouTryToMaintain"
#                                                                                      ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouGoToBedWhatDoYouSetTheThermostatToForCooling"))])
# colnames(tableAU.dat) <- c("CK_Cadmus_ID", "Thermostat_Setpoint", "Nighttime_Cooling")
# tableAU.dat$Thermostat_Setpoint <- as.numeric(as.character(tableAU.dat$Thermostat_Setpoint))
# tableAU.dat$Nighttime_Cooling <- as.numeric(as.character(tableAU.dat$Nighttime_Cooling))
# tableAU.dat$count <- 1
# 
# #remove any repeat header rows from exporting
# tableAU.dat0 <- tableAU.dat[which(tableAU.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# #merge together analysis data with cleaned RBSA data
# tableAU.dat1 <- left_join(tableAU.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# 
# tableAU.dat2.0 <- tableAU.dat1[which(!(is.na(tableAU.dat1$Thermostat_Setpoint))),]
# tableAU.dat2 <- tableAU.dat2.0[which(tableAU.dat2.0$Thermostat_Setpoint > 0),]
# unique(tableAU.dat2$Thermostat_Setpoint)
# unique(tableAU.dat2$Nighttime_Cooling)
# 
# tableAU.dat3.0 <- tableAU.dat2[which(!(is.na(tableAU.dat2$Nighttime_Cooling))),]
# tableAU.dat3 <- tableAU.dat3.0[which(tableAU.dat3.0$Nighttime_Cooling > 0),]
# 
# tableAU.dat3$Cooling.Setup <- tableAU.dat3$Nighttime_Cooling - tableAU.dat3$Thermostat_Setpoint
# 
# tableAU.dat4 <- tableAU.dat3[which(colnames(tableAU.dat3) %in% c("CK_Cadmus_ID", "Cooling.Setup"))]
# tableAU.sum <- summarise(group_by(tableAU.dat4, CK_Cadmus_ID)
#                          ,Cooling.Setup = sum(Cooling.Setup))
# 
# tableAU.merge <- left_join(rbsa.dat, tableAU.sum)
# tableAU.merge <- tableAU.merge[which(!is.na(tableAU.merge$Cooling.Setup)),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# tableAU.data <- weightedData(tableAU.merge[-which(colnames(tableAU.merge) %in% c("Cooling.Setup"))])
# tableAU.data <- left_join(tableAU.data, unique(tableAU.merge[which(colnames(tableAU.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Cooling.Setup"))]))
# 
# tableAU.data$count <- 1
# #######################
# # Weighted Analysis
# #######################
# tableAU.final <- mean_one_group(tableAU.data
#                                 ,valueVariable = 'Cooling.Setup'
#                                 ,byVariable = 'State'
#                                 ,aggregateRow = 'Region')
# 
# tableAU.final.SF <- tableAU.final[which(tableAU.final$BuildingType == "Single Family")
#                                   ,-which(colnames(tableAU.final) %in% c("BuildingType"))]
# tableAU.final.MH <- tableAU.final[which(tableAU.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(tableAU.final) %in% c("BuildingType"))]
# 
# exportTable(tableAU.final.SF, "SF", "Table AU", weighted = TRUE)
# # exportTable(tableAU.final.MH, "MH", "Table AU", weighted = TRUE)
# 
# #######################
# # MF
# #######################
# tableAU.final <- mean_one_group(tableAU.data
#                                 ,valueVariable = 'Cooling.Setup'
#                                 ,byVariable = 'HomeType'
#                                 ,aggregateRow = 'All Sizes')
# 
# tableAU.final.MF <- tableAU.final[which(tableAU.final$BuildingType == "Multifamily")
#                                   ,-which(colnames(tableAU.final) %in% c("BuildingType"))]
# # exportTable(tableAU.final.MF, "MF", "Table AU", weighted = TRUE)
# 
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# tableAU.final <- mean_one_group_unweighted(tableAU.data
#                                            ,valueVariable = 'Cooling.Setup'
#                                            ,byVariable = 'State'
#                                            ,aggregateRow = 'Region')
# 
# tableAU.final.SF <- tableAU.final[which(tableAU.final$BuildingType == "Single Family")
#                                   ,-which(colnames(tableAU.final) %in% c("BuildingType"))]
# tableAU.final.MH <- tableAU.final[which(tableAU.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(tableAU.final) %in% c("BuildingType"))]
# 
# exportTable(tableAU.final.SF, "SF", "Table AU", weighted = FALSE)
# # exportTable(tableAU.final.MH, "MH", "Table AU", weighted = FALSE)
# 
# #######################
# # MF
# #######################
# tableAU.final <- mean_one_group_unweighted(tableAU.data
#                                 ,valueVariable = 'Cooling.Setup'
#                                 ,byVariable = 'HomeType'
#                                 ,aggregateRow = 'All Sizes')
# 
# tableAU.final.MF <- tableAU.final[which(tableAU.final$BuildingType == "Multifamily")
#                                   ,-which(colnames(tableAU.final) %in% c("BuildingType"))]
# # exportTable(tableAU.final.MF, "MF", "Table AU", weighted = FALSE)
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 134: PERCENTAGE OF HOUSEHOLDS REPORTING GAS SERVICE BY STATE (SF table 141, MH table 116)
# #############################################################################################
# #subset to columns needed for analysis
# item134.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
#                                                             ,"Does.your.home.use.natural.gas?"
#                                                             ,""))]
# colnames(item134.dat) <- c("Natural.Gas.Use", "CK_Cadmus_ID")
# 
# #merge together analysis data with cleaned RBSA data
# item134.dat1 <- left_join(rbsa.dat, item134.dat, by = "CK_Cadmus_ID")
# unique(item134.dat1$Natural.Gas.Use)
# 
# item134.dat2 <- item134.dat1[which(item134.dat1$Natural.Gas.Use %in% c("Yes", "No")),]
# 
# item134.dat2$Gas.Count <- 0
# item134.dat2$Gas.Count[which(item134.dat2$Natural.Gas.Use == "Yes")] <- 1
# 
# item134.sum <- summarise(group_by(item134.dat2, CK_Cadmus_ID)
#                          ,Ind = sum(unique(Gas.Count)))
# 
# item134.merge <- left_join(rbsa.dat, item134.sum)
# item134.merge <- item134.merge[which(!is.na(item134.merge$Ind)),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item134.data <- weightedData(item134.merge[-which(colnames(item134.merge) %in% c("Ind"))])
# item134.data <- left_join(item134.data, item134.merge[which(colnames(item134.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Ind"))])
# item134.data$Count <- 1
# #######################
# # Weighted Analysis
# #######################
# item134.final <- proportions_one_group(CustomerLevelData = item134.data
#                                        ,valueVariable = 'Ind'
#                                        ,groupingVariable = 'State'
#                                        ,total.name = "Region"
#                                        ,weighted = TRUE)
# 
# item134.final.SF <- item134.final[which(item134.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item134.final) %in% c("BuildingType"))]
# item134.final.MH <- item134.final[which(item134.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item134.final) %in% c("BuildingType"))]
# 
# exportTable(item134.final.SF, "SF", "Table 141", weighted = TRUE)
# # exportTable(item134.final.MH, "MH", "Table 116", weighted = TRUE)
# 
# #######################
# # Unweighted Analysis
# #######################
# item134.final <- proportions_one_group(CustomerLevelData = item134.data
#                                        ,valueVariable = 'Ind'
#                                        ,groupingVariable = 'State'
#                                        ,total.name = "Region"
#                                        ,weighted = FALSE)
# 
# item134.final.SF <- item134.final[which(item134.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item134.final) %in% c("BuildingType"))]
# item134.final.MH <- item134.final[which(item134.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item134.final) %in% c("BuildingType"))]
# 
# exportTable(item134.final.SF, "SF", "Table 141", weighted = FALSE)
# # exportTable(item134.final.MH, "MH", "Table 116", weighted = FALSE)

































############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean os data
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]

#############################################################################################
#Item 132: AVERAGE COOLING THERMOSTAT SETPOINT BY CK_Building_ID (SF table 139, MH table 114)
#############################################################################################
#subset to columns needed for analysis
item132.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouCoolYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,""))])
colnames(item132.os.dat) <- c("CK_Cadmus_ID"
                           ,"Thermostat_Setpoint")
item132.os.dat$count <- 1
item132.os.dat$Thermostat_Setpoint <- as.numeric(as.character(item132.os.dat$Thermostat_Setpoint))
#remove any repeat header rows from exporting
item132.os.dat0 <- item132.os.dat[which(item132.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned scl data
item132.os.dat1 <- left_join(os.dat, item132.os.dat0, by = "CK_Cadmus_ID")


unique(item132.os.dat1$Thermostat_Setpoint)

item132.os.dat2.0 <- item132.os.dat1[which(!(is.na(item132.os.dat1$Thermostat_Setpoint))),]
item132.os.dat2 <- item132.os.dat2.0[which(item132.os.dat2.0$Thermostat_Setpoint != 0),]

################################################
# Adding pop and sample sizes for weights
################################################
item132.os.data <- weightedData(item132.os.dat2[-which(colnames(item132.os.dat2) %in% c("Thermostat_Setpoint"
                                                                               ,"count"))])
item132.os.data <- left_join(item132.os.data, unique(item132.os.dat2[which(colnames(item132.os.dat2) %in% c("CK_Cadmus_ID"
                                                                                                ,"Thermostat_Setpoint"
                                                                                                ,"count"))]))

item132.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item132.os.final <- mean_one_group(item132.os.data
                                ,valueVariable = 'Thermostat_Setpoint'
                                ,byVariable = 'CK_Building_ID'
                                ,aggregateRow = 'Remove')
item132.os.final <- item132.os.final[which(item132.os.final$CK_Building_ID %notin% c("Remove", "Total")),]

item132.os.final.SF <- item132.os.final[which(item132.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item132.os.final) %in% c("BuildingType"))]

exportTable(item132.os.final.SF, "SF", "Table 139", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item132.os.final <- mean_one_group_unweighted(item132.os.data
                                           ,valueVariable = 'Thermostat_Setpoint'
                                           ,byVariable = 'CK_Building_ID'
                                           ,aggregateRow = 'Remove')
item132.os.final <- item132.os.final[which(item132.os.final$CK_Building_ID %notin% c("Remove", "Total")),]

item132.os.final.SF <- item132.os.final[which(item132.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item132.os.final) %in% c("BuildingType"))]

exportTable(item132.os.final.SF, "SF", "Table 139", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
#Item 133: PERCENTAGE OF HOMES REPORTING A COOLING THERMOSTAT SETUP BY CK_Building_ID (SF table 140, MH table 115)
#############################################################################################
#subset to columns needed for analysis
item133.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouCoolYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouGoToBedWhatDoYouSetTheThermostatToForCooling"))])
colnames(item133.os.dat) <- c("CK_Cadmus_ID", "Thermostat_Setpoint", "Nighttime_Cooling")
item133.os.dat$count <- 1

#remove any repeat header rows from exporting
item133.os.dat0 <- item133.os.dat[which(item133.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned scl data
item133.os.dat1 <- left_join(item133.os.dat0, os.dat, by = "CK_Cadmus_ID")

item133.os.dat2.0 <- item133.os.dat1[which(!(is.na(item133.os.dat1$Thermostat_Setpoint))),]
item133.os.dat2 <- item133.os.dat2.0[which(item133.os.dat2.0$Thermostat_Setpoint != 0),]
unique(item133.os.dat2$Thermostat_Setpoint)
unique(item133.os.dat2$Nighttime_Cooling)

item133.os.dat3.0 <- item133.os.dat2[which(!(is.na(item133.os.dat2$Nighttime_Cooling))),]
item133.os.dat3 <- item133.os.dat3.0[which(item133.os.dat3.0$Nighttime_Cooling != 0),]

item133.os.dat3$Cooling.Setup <- 0
item133.os.dat3$Cooling.Setup[which(item133.os.dat3$Nighttime_Cooling > item133.os.dat3$Thermostat_Setpoint)] <- 1

item133.os.sum <- summarise(group_by(item133.os.dat3, CK_Cadmus_ID, CK_Building_ID)
                         ,Ind = sum(Cooling.Setup))

item133.os.merge <- left_join(os.dat, item133.os.sum)
item133.os.merge <- item133.os.merge[which(!is.na(item133.os.merge$Ind)),]

################################################
# Adding pop and sample sizes for weights
################################################
item133.os.data <- weightedData(item133.os.merge[-which(colnames(item133.os.merge) %in% c("Ind"))])
item133.os.data <- left_join(item133.os.data, unique(item133.os.merge[which(colnames(item133.os.merge) %in% c("CK_Cadmus_ID"
                                                                                                  ,"Ind"))]))
item133.os.data$count <- 1
item133.os.data$Count <- 1
#######################
# Weighted Analysis
#######################
item133.os.final <- proportions_one_group(CustomerLevelData = item133.os.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'CK_Building_ID'
                                       ,total.name = "Remove"
                                       ,weighted = TRUE)
item133.os.final <- item133.os.final[which(item133.os.final$CK_Building_ID %notin% c("Remove", "Total")),]

item133.os.final.SF <- item133.os.final[which(item133.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item133.os.final) %in% c("BuildingType"))]

exportTable(item133.os.final.SF, "SF", "Table 140", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item133.os.final <- proportions_one_group(CustomerLevelData = item133.os.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'CK_Building_ID'
                                       ,total.name = "Remove"
                                       ,weighted = FALSE)
item133.os.final <- item133.os.final[which(item133.os.final$CK_Building_ID %notin% c("Remove", "Total")),]

item133.os.final.SF <- item133.os.final[which(item133.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item133.os.final) %in% c("BuildingType"))]

exportTable(item133.os.final.SF, "SF", "Table 140", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
# Table AU: AVERAGE SIZE OF COOLING SETBACK BY CK_Building_ID (SF table 138, MH table 113)
#############################################################################################
#subset to columns needed for analysis
tableAU.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouCoolYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouGoToBedWhatDoYouSetTheThermostatToForCooling"))])
colnames(tableAU.os.dat) <- c("CK_Cadmus_ID", "Thermostat_Setpoint", "Nighttime_Cooling")
tableAU.os.dat$Thermostat_Setpoint <- as.numeric(as.character(tableAU.os.dat$Thermostat_Setpoint))
tableAU.os.dat$Nighttime_Cooling <- as.numeric(as.character(tableAU.os.dat$Nighttime_Cooling))
tableAU.os.dat$count <- 1

#remove any repeat header rows from exporting
tableAU.os.dat0 <- tableAU.os.dat[which(tableAU.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned scl data
tableAU.os.dat1 <- left_join(tableAU.os.dat0, os.dat, by = "CK_Cadmus_ID")

tableAU.os.dat2.0 <- tableAU.os.dat1[which(!(is.na(tableAU.os.dat1$Thermostat_Setpoint))),]
tableAU.os.dat2 <- tableAU.os.dat2.0[which(tableAU.os.dat2.0$Thermostat_Setpoint != 0),]
unique(tableAU.os.dat2$Thermostat_Setpoint)
unique(tableAU.os.dat2$Nighttime_Cooling)

tableAU.os.dat3.0 <- tableAU.os.dat2[which(!(is.na(tableAU.os.dat2$Nighttime_Cooling))),]
tableAU.os.dat3 <- tableAU.os.dat3.0[which(tableAU.os.dat3.0$Nighttime_Cooling != 0),]

tableAU.os.dat3$Cooling.Setup <- tableAU.os.dat3$Nighttime_Cooling - tableAU.os.dat3$Thermostat_Setpoint

tableAU.os.dat4 <- tableAU.os.dat3[which(colnames(tableAU.os.dat3) %in% c("CK_Cadmus_ID", "Cooling.Setup"))]
tableAU.os.sum <- summarise(group_by(tableAU.os.dat4, CK_Cadmus_ID)
                         ,Cooling.Setup = sum(Cooling.Setup))

tableAU.os.merge <- left_join(os.dat, tableAU.os.sum)
tableAU.os.merge <- tableAU.os.merge[which(!is.na(tableAU.os.merge$Cooling.Setup)),]

################################################
# Adding pop and sample sizes for weights
################################################
tableAU.os.data <- weightedData(tableAU.os.merge[-which(colnames(tableAU.os.merge) %in% c("Cooling.Setup"))])
tableAU.os.data <- left_join(tableAU.os.data, unique(tableAU.os.merge[which(colnames(tableAU.os.merge) %in% c("CK_Cadmus_ID"
                                                                                                  ,"Cooling.Setup"))]))

tableAU.os.data$count <- 1
#######################
# Weighted Analysis
#######################
tableAU.os.final <- mean_one_group(tableAU.os.data
                                ,valueVariable = 'Cooling.Setup'
                                ,byVariable = 'CK_Building_ID'
                                ,aggregateRow = 'Remove')
tableAU.os.final <- tableAU.os.final[which(tableAU.os.final$CK_Building_ID %notin% c("Remove", "Total")),]

tableAU.os.final.SF <- tableAU.os.final[which(tableAU.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableAU.os.final) %in% c("BuildingType"))]

exportTable(tableAU.os.final.SF, "SF", "Table AU", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
tableAU.os.final <- mean_one_group_unweighted(tableAU.os.data
                                           ,valueVariable = 'Cooling.Setup'
                                           ,byVariable = 'CK_Building_ID'
                                           ,aggregateRow = 'Remove')
tableAU.os.final <- tableAU.os.final[which(tableAU.os.final$CK_Building_ID %notin% c("Remove", "Total")),]

tableAU.os.final.SF <- tableAU.os.final[which(tableAU.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableAU.os.final) %in% c("BuildingType"))]

exportTable(tableAU.os.final.SF, "SF", "Table AU", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
#Item 134: PERCENTAGE OF HOUSEHOLDS REPORTING GAS SERVICE BY CK_Building_ID (SF table 141, MH table 116)
#############################################################################################
#subset to columns needed for analysis
item134.os.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Does.your.home.use.natural.gas?"
                                                            ,""))]
colnames(item134.os.dat) <- c("Natural.Gas.Use", "CK_Cadmus_ID")

#merge together analysis data with cleaned scl data
item134.os.dat1 <- left_join(os.dat, item134.os.dat, by = "CK_Cadmus_ID")
unique(item134.os.dat1$Natural.Gas.Use)

item134.os.dat2 <- item134.os.dat1[which(item134.os.dat1$Natural.Gas.Use %in% c("Yes", "No")),]

item134.os.dat2$Gas.Count <- 0
item134.os.dat2$Gas.Count[which(item134.os.dat2$Natural.Gas.Use == "Yes")] <- 1

item134.os.sum <- summarise(group_by(item134.os.dat2, CK_Cadmus_ID, CK_Building_ID)
                         ,Ind = sum(unique(Gas.Count)))

item134.os.merge <- left_join(os.dat, item134.os.sum)
item134.os.merge <- item134.os.merge[which(!is.na(item134.os.merge$Ind)),]

################################################
# Adding pop and sample sizes for weights
################################################
item134.os.data <- weightedData(item134.os.merge[-which(colnames(item134.os.merge) %in% c("Ind"))])
item134.os.data <- left_join(item134.os.data, unique(item134.os.merge[which(colnames(item134.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))]))
item134.os.data$Count <- 1
#######################
# Weighted Analysis
#######################
item134.os.final <- proportions_one_group(CustomerLevelData = item134.os.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'CK_Building_ID'
                                       ,total.name = "Remove"
                                       ,weighted = TRUE)
item134.os.final <- item134.os.final[which(item134.os.final$CK_Building_ID %notin% c("Remove", "Total")),]

item134.os.final.SF <- item134.os.final[which(item134.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item134.os.final) %in% c("BuildingType"))]

exportTable(item134.os.final.SF, "SF", "Table 141", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item134.os.final <- proportions_one_group(CustomerLevelData = item134.os.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'CK_Building_ID'
                                       ,total.name = "Remove"
                                       ,weighted = FALSE)
item134.os.final <- item134.os.final[which(item134.os.final$CK_Building_ID %notin% c("Remove", "Total")),]

item134.os.final.SF <- item134.os.final[which(item134.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item134.os.final) %in% c("BuildingType"))]

exportTable(item134.os.final.SF, "SF", "Table 141", weighted = FALSE, osIndicator = export.ind, OS = T)

