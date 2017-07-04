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
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
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

item94.dat1 <- left_join(item94.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item94.dat1.1 <- item94.dat1[which(item94.dat1$Stove.Fuel %in% c("Electric", "Gas", "Propane")),]
item94.dat2 <- item94.dat1.1[which(item94.dat1.1$Type == "Stove/Oven"),]


#summarise by fuel type
item94.sum1 <- summarise(group_by(item94.dat2, BuildingType, Stove.Fuel)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

#summarise across fuel types
item94.sum2 <- summarise(group_by(item94.dat2, BuildingType)
                         ,Stove.Fuel = "All Fuel Types"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

#row bind
item94.merge <- rbind.data.frame(item94.sum1, item94.sum2, stringsAsFactors = F)

item94.merge1 <- left_join(item94.merge, item94.sum2, by = c("BuildingType"))
colnames(item94.merge1) <- c("BuildingType"
                             ,"Stove.Fuel"
                             ,"SampleSize"
                             ,"Count"
                             ,"Remove"
                             ,"Remove"
                             ,"Total.Count")

item94.final <- item94.merge1[which(colnames(item94.merge1) %in% c("BuildingType"
                                                                   ,"Stove.Fuel"
                                                                   ,"SampleSize"
                                                                   ,"Count"
                                                                   ,"Total.Count"))]

item94.final$Percent <- item94.final$Count / item94.final$Total.Count
item94.final$SE <- sqrt(item94.final$Percent * (1 - item94.final$Percent) / item94.final$SampleSize)

item94.table <- data.frame("BuildingType" = item94.final$BuildingType
                           ,"Stove.Fuel" = item94.final$Stove.Fuel
                           ,"Percent" = item94.final$Percent
                           ,"SE" = item94.final$SE
                           ,"SampleSize" = item94.final$SampleSize)

item94.table1 <- item94.table[which(item94.table$BuildingType %in% c("Single Family","Manufactured")),]













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

item95.dat1 <- left_join(item95.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item95.dat1.1 <- item95.dat1[which(item95.dat1$Oven.Fuel %in% c("Electric", "Gas", "Propane")),]
item95.dat2 <- item95.dat1.1[which(item95.dat1.1$Type == "Stove/Oven"),]


#summarise by fuel type
item95.sum1 <- summarise(group_by(item95.dat2, BuildingType, Oven.Fuel)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

#summarise across fuel types
item95.sum2 <- summarise(group_by(item95.dat2, BuildingType)
                         ,Oven.Fuel = "All Fuel Types"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

#row bind
item95.merge <- rbind.data.frame(item95.sum1, item95.sum2, stringsAsFactors = F)

item95.merge1 <- left_join(item95.merge, item95.sum2, by = c("BuildingType"))
colnames(item95.merge1) <- c("BuildingType"
                             ,"Oven.Fuel"
                             ,"SampleSize"
                             ,"Count"
                             ,"Remove"
                             ,"Remove"
                             ,"Total.Count")

item95.final <- item95.merge1[which(colnames(item95.merge1) %in% c("BuildingType"
                                                                   ,"Oven.Fuel"
                                                                   ,"SampleSize"
                                                                   ,"Count"
                                                                   ,"Total.Count"))]

item95.final$Percent <- item95.final$Count / item95.final$Total.Count
item95.final$SE <- sqrt(item95.final$Percent * (1 - item95.final$Percent) / item95.final$SampleSize)

item95.table <- data.frame("BuildingType" = item95.final$BuildingType
                           ,"Oven.Fuel" = item95.final$Oven.Fuel
                           ,"Percent" = item95.final$Percent
                           ,"SE" = item95.final$SE
                           ,"SampleSize" = item95.final$SampleSize)

item95.table1 <- item95.table[which(item95.table$BuildingType %in% c("Single Family","Manufactured")),]
