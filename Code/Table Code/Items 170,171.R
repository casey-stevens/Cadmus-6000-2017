
# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))


#Read in data for analysis
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))



#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
mechanical.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
mechanical.dat.11 <- mechanical.dat.1[which(mechanical.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
mechanical.dat.12 <- mechanical.dat.11[which(mechanical.dat.11$Primary.Heating.System == "Yes"),]
length(unique(mechanical.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(mechanical.dat.12$Primary.Heating.System)
mechanical.dat.12$count <- 1

mechanical.dat.13 <- unique(mechanical.dat.12[which(mechanical.dat.12$Heating.Fuel == "Electric"),])

mechanical.sum1 <- summarise(group_by(mechanical.dat.13, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
mechanical.sum1$Count <- 1
which(duplicated(mechanical.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(mechanical.sum1$Heating.Fuel)

mechanical.final <- mechanical.sum1






#############################################################################################
# 
#Sites Interview Data
# 
#############################################################################################
#############################################################################################
#Item 170: AVERAGE HEATING THERMOSTAT SETPOINT BY STATE (SF table 136, MH table 111)
#############################################################################################
#subset to columns needed for analysis
item170.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouHeatYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,""))])
colnames(item170.dat) <- c("CK_Cadmus_ID", "Thermostat_Setpoint")
item170.dat$count <- 1

#remove any repeat header rows from exporting
item170.dat0 <- item170.dat[which(item170.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item170.dat1 <- left_join(item170.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item170.dat1.1 <-left_join(item170.dat1, mechanical.final, by = "CK_Cadmus_ID")
item170.dat1.2 <- item170.dat1.1[which(item170.dat1.1$Heating.Fuel == "Electric"),]

item170.dat1.3 <- item170.dat1.2[which(!(is.na(item170.dat1.2$Thermostat_Setpoint))),]
item170.dat2 <- item170.dat1.3[which(item170.dat1.3$Thermostat_Setpoint != 0),]

#summarise by state
item170.state <- summarise(group_by(item170.dat2, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Thermostat_Setpoint)
                           ,SE = sd(Thermostat_Setpoint) / sqrt(SampleSize))

#summarise across states
item170.region <- summarise(group_by(item170.dat2, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean = mean(Thermostat_Setpoint)
                            ,SE = sd(Thermostat_Setpoint) / sqrt(SampleSize))

#join together state and region info
item170.final <- rbind.data.frame(item170.state, item170.region, stringsAsFactors = F)

#put columns in correct order
item170.table <- data.frame("BuildingType" = item170.final$BuildingType
                            ,"State" = item170.final$State
                            ,"Mean" = item170.final$Mean
                            ,"SE" = item170.final$SE
                            ,"SampleSize" = item170.final$SampleSize)

#subset to only relevant buildingtypes
item170.table1 <- item170.table[which(item170.table$BuildingType %in% c("Single Family", "Manufactured")),]










#############################################################################################
#Item 171: PERCENTAGE OF HOMES REPORTING A HEATING SETBACK BY STATE (SF table 137, MH table 112)
#############################################################################################
#subset to columns needed for analysis
item171.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouHeatYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouGoToBedWhatDoYouSetTheThermostatToForHeating"))])
colnames(item171.dat) <- c("CK_Cadmus_ID", "Nighttime_Heating", "Thermostat_Setpoint")
item171.dat$count <- 1

#remove any repeat header rows from exporting
item171.dat0 <- item171.dat[which(item171.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item171.dat1 <- left_join(item171.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item171.dat1.1 <-left_join(item171.dat1, mechanical.final, by = "CK_Cadmus_ID")
item171.dat1.2 <- item171.dat1.1[which(item171.dat1.1$Heating.Fuel == "Electric"),]

item171.dat1.3 <- item171.dat1.2[which(!(is.na(item171.dat1.2$Thermostat_Setpoint))),]
item171.dat2 <- item171.dat1.3[which(item171.dat1.3$Thermostat_Setpoint != 0),]

unique(item171.dat2$Thermostat_Setpoint)
unique(item171.dat2$Nighttime_Heating)

item171.dat3.0 <- item171.dat2[which(!(is.na(item171.dat2$Nighttime_Heating))),]
item171.dat3 <- item171.dat3.0[which(item171.dat3.0$Nighttime_Heating != 0),]

item171.dat3$Heating.Setback <- 0
item171.dat3$Heating.Setback[which(item171.dat3$Nighttime_Heating < item171.dat3$Thermostat_Setpoint)] <- 1


#summarise by states
item171.state <- summarise(group_by(item171.dat3, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(Heating.Setback)
                           ,Total.Count = sum(count)
                           ,Percent = Count / Total.Count
                           ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

#summarise across states
item171.region <- summarise(group_by(item171.dat3, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(Heating.Setback)
                            ,Total.Count = sum(count)
                            ,Percent = Count / Total.Count
                            ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

#join state and region information
item171.final <- rbind.data.frame(item171.state, item171.region, stringsAsFactors = F)

#put columns in correct order
item171.table <- data.frame("BuildingType" = item171.final$BuildingType
                            ,"State" = item171.final$State
                            ,"Percent" = item171.final$Percent
                            ,"SE" = item171.final$SE
                            ,"SampleSize" = item171.final$SampleSize)

#subset to only relevant buildingtypes
item171.table1 <- item171.table[which(item171.table$BuildingType %in% c("Single Family", "Manufactured")),]







