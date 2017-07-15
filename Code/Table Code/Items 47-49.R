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
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

#subset to columns needed for the analysis of items 47,48,49
mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"System.Sub-Type"
                                                                        ,"Heating.Fuel"))]
mechanical.dat1$Heating.Fuel[which(mechanical.dat1$Heating.Fuel == "Natural gas")] <- "Natural Gas"




#############################################################################################
#Item 47: DISTRIBUTION OF FUEL CHOICE, FORCED AIR FURNACES (SF table 54, MH table 36)
#############################################################################################

item47.dat <- mechanical.dat1

item47.dat1 <- item47.dat[which(item47.dat$Generic == "Furnace"),]

item47.dat2 <- left_join(item47.dat1, rbsa.dat, by = "CK_Cadmus_ID")
item47.dat2$count <- 1

item47.tmp1 <- summarise(group_by(item47.dat2, BuildingType, Heating.Fuel)
                          ,SampleSizes = length(unique(CK_Cadmus_ID))
                          ,FuelCount   = sum(count))

item47.tmp2 <- summarise(group_by(item47.dat2, BuildingType)
                         ,Heating.Fuel = "Total"
                         ,SampleSizes = length(unique(CK_Cadmus_ID))
                         ,FuelCount   = sum(count))
item47.merge1 <- rbind.data.frame(item47.tmp1, item47.tmp2, stringsAsFactors = F)

item47.tmp3 <- summarise(group_by(item47.dat2, BuildingType)
                         ,TotalCount   = sum(count))

item47.final <- left_join(item47.merge1, item47.tmp3, by = "BuildingType")
item47.final$Percent <- item47.final$FuelCount / item47.final$TotalCount
item47.final$SE      <- sqrt(item47.final$Percent * (1 - item47.final$Percent) / item47.final$SampleSizes)

item47.table <- data.frame("BuildingType" = item47.final$BuildingType
                           ,"Heating.Fuel" = item47.final$Heating.Fuel
                           ,"Percent" = item47.final$Percent
                           ,"SE" = item47.final$SE
                           ,"SampleSize" = item47.final$SampleSizes)
item47.table1 <- item47.table[which(item47.table$BuildingType %in% c("Single Family", "Manufactured")),]





#############################################################################################
#Item 48: DISTRIBUTION OF FUEL CHOICE, BOILERS (SF table 55)
#############################################################################################

item48.dat <- mechanical.dat1

unique(item48.dat$Generic)
item48.dat1 <- item48.dat[which(item48.dat$Generic == "Boiler"),]

item48.dat2 <- left_join(item48.dat1, rbsa.dat, by = "CK_Cadmus_ID")
item48.dat2$count <- 1

item48.dat3 <- item48.dat2[which(item48.dat2$BuildingType == "Single Family"),]

item48.tmp1 <- summarise(group_by(item48.dat3, BuildingType, Heating.Fuel)
                         ,SampleSizes = length(unique(CK_Cadmus_ID))
                         ,FuelCount   = sum(count))

item48.tmp2 <- summarise(group_by(item48.dat3, BuildingType)
                         ,Heating.Fuel = "Total"
                         ,SampleSizes = length(unique(CK_Cadmus_ID))
                         ,FuelCount   = sum(count))
item48.merge1 <- rbind.data.frame(item48.tmp1, item48.tmp2, stringsAsFactors = F)

item48.tmp3 <- summarise(group_by(item48.dat3, BuildingType)
                         ,TotalCount   = sum(count))

item48.final <- left_join(item48.merge1, item48.tmp3, by = "BuildingType")
item48.final$Percent <- item48.final$FuelCount / item48.final$TotalCount
item48.final$SE      <- sqrt(item48.final$Percent * (1 - item48.final$Percent) / item48.final$SampleSizes)

item48.table <- data.frame("BuildingType" = item48.final$BuildingType
                           ,"Heating.Fuel" = item48.final$Heating.Fuel
                           ,"Percent" = item48.final$Percent
                           ,"SE" = item48.final$SE
                           ,"SampleSize" = item48.final$SampleSizes)
item48.table1 <- item48.table[which(item48.table$BuildingType %in% c("Single Family")),]







#############################################################################################
#Item 49: DISTRIBUTION OF FUEL CHOICE, COMBUSTION HEATING STOVES (SF table 56, MH table 37)
#############################################################################################

item49.dat <- mechanical.dat1

unique(item49.dat$`System.Sub-Type`)
item49.dat1 <- item49.dat[which(item49.dat$`System.Sub-Type` == "Space Heating Stove"),]

item49.dat2 <- left_join(item49.dat1, rbsa.dat, by = "CK_Cadmus_ID")
item49.dat2$count <- 1

item49.dat3 <- item49.dat2[which(item49.dat2$BuildingType %in% c("Single Family", "Manufactured")),]

item49.tmp1 <- summarise(group_by(item49.dat3, BuildingType, Heating.Fuel)
                         ,SampleSizes = length(unique(CK_Cadmus_ID))
                         ,FuelCount   = sum(count))

item49.tmp2 <- summarise(group_by(item49.dat3, BuildingType)
                         ,Heating.Fuel = "Total"
                         ,SampleSizes = length(unique(CK_Cadmus_ID))
                         ,FuelCount   = sum(count))
item49.merge1 <- rbind.data.frame(item49.tmp1, item49.tmp2, stringsAsFactors = F)

item49.tmp3 <- summarise(group_by(item49.dat3, BuildingType)
                         ,TotalCount   = sum(count))

item49.final <- left_join(item49.merge1, item49.tmp3, by = "BuildingType")
item49.final$Percent <- item49.final$FuelCount / item49.final$TotalCount
item49.final$SE      <- sqrt(item49.final$Percent * (1 - item49.final$Percent) / item49.final$SampleSizes)

item49.table <- data.frame("BuildingType" = item49.final$BuildingType
                           ,"Heating.Fuel" = item49.final$Heating.Fuel
                           ,"Percent" = item49.final$Percent
                           ,"SE" = item49.final$SE
                           ,"SampleSize" = item49.final$SampleSizes)
item49.table1 <- item49.table[which(item49.table$BuildingType %in% c("Single Family", "Manufactured")),]
