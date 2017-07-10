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
#Item 132: AVERAGE COOLING THERMOSTAT SETPOINT BY STATE (SF table 139, MH table 114)
#############################################################################################
#subset to columns needed for analysis
item132.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouCoolYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,""))])
colnames(item132.dat) <- c("CK_Cadmus_ID"
                           ,"Thermostat_Setpoint")
item132.dat$count <- 1

#remove any repeat header rows from exporting
item132.dat0 <- item132.dat[which(item132.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item132.dat1 <- left_join(item132.dat0, rbsa.dat, by = "CK_Cadmus_ID")


unique(item132.dat1$Thermostat_Setpoint)

item132.dat2 <- item132.dat1[which(!(is.na(item132.dat1$Thermostat_Setpoint))),]

#summarise by state
item132.state <- summarise(group_by(item132.dat2, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Thermostat_Setpoint)
                           ,SE = sd(Thermostat_Setpoint) / sqrt(SampleSize))

#summarise across states
item132.region <- summarise(group_by(item132.dat2, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean = mean(Thermostat_Setpoint)
                            ,SE = sd(Thermostat_Setpoint) / sqrt(SampleSize))

#join together state and region info
item132.final <- rbind.data.frame(item132.state, item132.region, stringsAsFactors = F)

#put columns in correct order
item132.table <- data.frame("BuildingType" = item132.final$BuildingType
                            ,"State" = item132.final$State
                            ,"Mean" = item132.final$Mean
                            ,"SE" = item132.final$SE
                            ,"SampleSize" = item132.final$SampleSize)

#subset to only relevant buildingtypes
item132.table1 <- item132.table[which(item132.table$BuildingType %in% c("Single Family", "Manufactured")),]










#############################################################################################
#Item 133:  (SF table , MH table )
#############################################################################################
#subset to columns needed for analysis
item133.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,""
                                                                                     ,""))])
colnames(item133.dat) <- c("CK_Cadmus_ID", "Thermostat_Setpoint", "Nighttime_Cooling")
item133.dat$count <- 1

#remove any repeat header rows from exporting
item133.dat0 <- item133.dat[which(item133.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item133.dat1 <- left_join(item133.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item133.dat2 <- item133.dat1[which(!(is.na(item133.dat1$Thermostat_Setpoint))),]
unique(item133.dat2$Thermostat_Setpoint)
unique(item133.dat2$Nighttime_Heating)

item133.dat3 <- item133.dat2[which(!(is.na(item133.dat2$Nighttime_Heating))),]

item133.dat3$Heating.Setback <- 0
item133.dat3$Heating.Setback[which(item133.dat3$Nighttime_Heating < item133.dat3$Thermostat_Setpoint)] <- 1


#summarise by states
item133.state <- summarise(group_by(item133.dat3, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(Heating.Setback)
                           ,Total.Count = sum(count)
                           ,Percent = Count / Total.Count
                           ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

#summarise across states
item133.region <- summarise(group_by(item133.dat3, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(Heating.Setback)
                            ,Total.Count = sum(count)
                            ,Percent = Count / Total.Count
                            ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

#join state and region information
item133.final <- rbind.data.frame(item133.state, item133.region, stringsAsFactors = F)

#put columns in correct order
item133.table <- data.frame("BuildingType" = item133.final$BuildingType
                            ,"State" = item133.final$State
                            ,"Percent" = item133.final$Percent
                            ,"SE" = item133.final$SE
                            ,"SampleSize" = item133.final$SampleSize)

#subset to only relevant buildingtypes
item133.table1 <- item133.table[which(item133.table$BuildingType %in% c("Single Family", "Manufactured")),]













#############################################################################################
#Item 134: PERCENTAGE OF HOUSEHOLDS REPORTING GAS SERVICE BY STATE (SF table 141, MH table 116)
#############################################################################################
#subset to columns needed for analysis
item134.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,""
                                                                                     ,""))])
colnames(item134.dat) <- c("CK_Cadmus_ID", "", "")
item134.dat$count <- 1

#remove any repeat header rows from exporting
item134.dat0 <- item134.dat[which(item134.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item134.dat1 <- left_join(item134.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item134.dat2 <- item134.dat1[which(!(is.na(item134.dat1$Thermostat_Setpoint))),]
unique(item134.dat2$Thermostat_Setpoint)
unique(item134.dat2$Nighttime_Heating)

item134.dat3 <- item134.dat2[which(!(is.na(item134.dat2$Nighttime_Heating))),]

item134.dat3$Heating.Setback <- item134.dat3$Thermostat_Setpoint - item134.dat3$Nighttime_Heating

#summarise by states
item134.state <- summarise(group_by(item134.dat3, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Heating.Setback)
                           ,SE = sd(Heating.Setback) / sqrt(SampleSize))

#summarise across states
item134.region <- summarise(group_by(item134.dat3, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean = mean(Heating.Setback)
                            ,SE = sd(Heating.Setback) / sqrt(SampleSize))

#combine state and region information
item134.final <- rbind.data.frame(item134.state, item134.region, stringsAsFactors = F)

#put columns in correct order
item134.table <- data.frame("BuildingType" = item134.final$BuildingType
                            ,"State" = item134.final$State
                            ,"Mean" = item134.final$Mean
                            ,"SE" = item134.final$SE
                            ,"SampleSize" = item134.final$SampleSize)

#subset to only relevant buildingtypes
item134.table1 <- item134.table[which(item134.table$BuildingType %in% c("Single Family", "Manufactured")),]
