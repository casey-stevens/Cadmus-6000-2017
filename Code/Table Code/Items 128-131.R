#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))



#############################################################################################
#Item 128: DISTRIBUTION OF HOMES WITH GAS FUEL ASSISTANCE BY PERCENTAGE OF ASSISTANCE AND STATE (SF table 135, MH table 110)
#############################################################################################
#subset to columns needed for analysis
item128.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_DoesAnotherEntityPayPartOfYourGasBill_WhatShare"
                                                                                     ,""))])
colnames(item128.dat) <- c("CK_Cadmus_ID", "Gas_Fuel_Assistance")
item128.dat$count <- 1

#remove any repeat header rows from exporting
item128.dat0 <- item128.dat[which(item128.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item128.dat1 <- left_join(item128.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item128.dat1$Gas_Fuel_Assistance)
















#############################################################################################
#Item 129: AVERAGE HEATING THERMOSTAT SETPOINT BY STATE (SF table 136, MH table 111)
#############################################################################################
#subset to columns needed for analysis
item129.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouHeatYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,""))])
colnames(item129.dat) <- c("CK_Cadmus_ID", "Thermostat_Setpoint")
item129.dat$count <- 1

#remove any repeat header rows from exporting
item129.dat0 <- item129.dat[which(item129.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item129.dat1 <- left_join(item129.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item129.dat1$Thermostat_Setpoint)

item129.dat2 <- item129.dat1[which(!(is.na(item129.dat1$Thermostat_Setpoint))),]

#summarise by state
item129.state <- summarise(group_by(item129.dat2, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Thermostat_Setpoint)
                           ,SE = sd(Thermostat_Setpoint) / sqrt(SampleSize))

#summarise across states
item129.region <- summarise(group_by(item129.dat2, BuildingType)
                           ,State = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Thermostat_Setpoint)
                           ,SE = sd(Thermostat_Setpoint) / sqrt(SampleSize))

#join together state and region info
item129.final <- rbind.data.frame(item129.state, item129.region, stringsAsFactors = F)

#put columns in correct order
item129.table <- data.frame("BuildingType" = item129.final$BuildingType
                            ,"State" = item129.final$State
                            ,"Mean" = item129.final$Mean
                            ,"SE" = item129.final$SE
                            ,"SampleSize" = item129.final$SampleSize)

#subset to only relevant buildingtypes
item129.table1 <- item129.table[which(item129.table$BuildingType %in% c("Single Family", "Manufactured")),]










#############################################################################################
#Item 130: PERCENTAGE OF HOMES REPORTING A HEATING SETBACK BY STATE (SF table 137, MH table 112)
#############################################################################################
#subset to columns needed for analysis
item130.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouHeatYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouGoToBedWhatDoYouSetTheThermostatToForHeating"))])
colnames(item130.dat) <- c("CK_Cadmus_ID", "Thermostat_Setpoint", "Nighttime_Heating")
item130.dat$count <- 1

#remove any repeat header rows from exporting
item130.dat0 <- item130.dat[which(item130.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item130.dat1 <- left_join(item130.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item130.dat2 <- item130.dat1[which(!(is.na(item130.dat1$Thermostat_Setpoint))),]
unique(item130.dat2$Thermostat_Setpoint)
unique(item130.dat2$Nighttime_Heating)

item130.dat3 <- item130.dat2[which(!(is.na(item130.dat2$Nighttime_Heating))),]

item130.dat3$Heating.Setback <- 0
item130.dat3$Heating.Setback[which(item130.dat3$Nighttime_Heating < item130.dat3$Thermostat_Setpoint)] <- 1


#summarise by states
item130.state <- summarise(group_by(item130.dat3, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(Heating.Setback)
                           ,Total.Count = sum(count)
                           ,Percent = Count / Total.Count
                           ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

#summarise across states
item130.region <- summarise(group_by(item130.dat3, BuildingType)
                           ,State = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(Heating.Setback)
                           ,Total.Count = sum(count)
                           ,Percent = Count / Total.Count
                           ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

#join state and region information
item130.final <- rbind.data.frame(item130.state, item130.region, stringsAsFactors = F)

#put columns in correct order
item130.table <- data.frame("BuildingType" = item130.final$BuildingType
                            ,"State" = item130.final$State
                            ,"Percent" = item130.final$Percent
                            ,"SE" = item130.final$SE
                            ,"SampleSize" = item130.final$SampleSize)

#subset to only relevant buildingtypes
item130.table1 <- item130.table[which(item130.table$BuildingType %in% c("Single Family", "Manufactured")),]













#############################################################################################
#Item 131: AVERAGE SIZE OF HEATING SETBACK BY STATE (SF table 138, MH table 113)
#############################################################################################
#subset to columns needed for analysis
item131.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouHeatYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouGoToBedWhatDoYouSetTheThermostatToForHeating"))])
colnames(item131.dat) <- c("CK_Cadmus_ID", "Thermostat_Setpoint", "Nighttime_Heating")
item131.dat$count <- 1

#remove any repeat header rows from exporting
item131.dat0 <- item131.dat[which(item131.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item131.dat1 <- left_join(item131.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item131.dat2 <- item131.dat1[which(!(is.na(item131.dat1$Thermostat_Setpoint))),]
unique(item131.dat2$Thermostat_Setpoint)
unique(item131.dat2$Nighttime_Heating)

item131.dat3 <- item131.dat2[which(!(is.na(item131.dat2$Nighttime_Heating))),]

item131.dat3$Heating.Setback <- item131.dat3$Thermostat_Setpoint - item131.dat3$Nighttime_Heating

#summarise by states
item131.state <- summarise(group_by(item131.dat3, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Heating.Setback)
                           ,SE = sd(Heating.Setback) / sqrt(SampleSize))
  
#summarise across states
item131.region <- summarise(group_by(item131.dat3, BuildingType)
                           ,State = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Heating.Setback)
                           ,SE = sd(Heating.Setback) / sqrt(SampleSize))

#combine state and region information
item131.final <- rbind.data.frame(item131.state, item131.region, stringsAsFactors = F)

#put columns in correct order
item131.table <- data.frame("BuildingType" = item131.final$BuildingType
                            ,"State" = item131.final$State
                            ,"Mean" = item131.final$Mean
                            ,"SE" = item131.final$SE
                            ,"SampleSize" = item131.final$SampleSize)

#subset to only relevant buildingtypes
item131.table1 <- item131.table[which(item131.table$BuildingType %in% c("Single Family", "Manufactured")),]
