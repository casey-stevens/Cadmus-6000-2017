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
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
#Item 96: DISTRIBUTION OF WATER HEATER FUEL BY STATE (SF table 103, MH table 84)
#############################################################################################
#subset to columns needed for analysis
item96.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,""))]
item96.dat$count <- 1

item96.dat0 <- item96.dat[which(item96.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item96.dat1 <- left_join(item96.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item96.dat1$DHW.Fuel)
item96.dat2 <- item96.dat1[which(item96.dat1$DHW.Fuel %in% c("Electric"
                                                             , "Natural Gas"
                                                             , "Natural gas"
                                                             , "Propane")),]

#summarise by state
#summarise by fuel types
item96.sum1 <- summarise(group_by(item96.dat2, BuildingType, State, DHW.Fuel)
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        ,Count = sum(count))

#summarise across fuel types
item96.sum2 <- summarise(group_by(item96.dat2, BuildingType, State)
                        ,DHW.Fuel = "All Fuel Types"
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        ,Count = sum(count))


#summarise across states
#summarise by fuel types
item96.sum3 <- summarise(group_by(item96.dat2, BuildingType, DHW.Fuel)
                        ,State = "Region"
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        ,Count = sum(count))

#summarise across fuel types
item96.sum4 <- summarise(group_by(item96.dat2, BuildingType)
                        ,State = "Region"
                        ,DHW.Fuel = "All Fuel Types"
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        ,Count = sum(count))


item96.totCount <- rbind.data.frame(item96.sum2, item96.sum4, stringsAsFactors = F)

item96.merge1 <- rbind.data.frame(item96.sum1,item96.sum2,item96.sum3,item96.sum4, stringsAsFactors = F)

item96.merge2 <- left_join(item96.merge1, item96.totCount, by = c("BuildingType", "State"))
colnames(item96.merge2) <- c("BuildingType"
                             ,"State"
                             ,"DHW.Fuel"
                             ,"SampleSize"
                             ,"Count"
                             ,"Remove"
                             ,"Remove"
                             ,"Total.Count")

item96.final <- item96.merge2[which(colnames(item96.merge2)!= "Remove")]

item96.final$Percent <- item96.final$Count / item96.final$Total.Count
item96.final$SE <- sqrt(item96.final$Percent * (1 - item96.final$Percent) / item96.final$SampleSize)

library(data.table)
item96.table <- dcast(setDT(item96.final)
                      ,formula = BuildingType + DHW.Fuel ~ State
                      , value.var = c("Percent", "SE", "SampleSize"))

item96.table <- data.frame(item96.table)
item96.table0 <- item96.table[which(!(colnames(item96.table) %in% c("SampleSize_MT"
                                                                  ,"SampleSize_WA"
                                                                  ,"SampleSize_OR")))]

item96.table1 <- item96.table0[which(item96.table0$BuildingType %in% c("Single Family","Manufactured")),]








#############################################################################################
#Item 97: DISTRIBUTION OF WATER HEATERS BY TYPE (SF table 104)
#############################################################################################
#subset to columns needed for analysis
item97.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,""))]
item97.dat$count <- 1

item97.dat0 <- item97.dat[which(item97.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item97.dat1 <- left_join(item97.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item97.dat1$DHW.Fuel)
item97.dat2 <- item97.dat1[grep("Water Heater",item97.dat1$Generic),]

#summarise by type
item97.sum1 <- summarise(group_by(item97.dat2, BuildingType, Generic)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

#summarise across types
item97.sum2 <- summarise(group_by(item97.dat2, BuildingType)
                         ,Generic = "Total"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))
#total counts
item97.totCount <- item97.sum2[which(colnames(item97.sum2) %in% c("BuildingType", "Count"))]
colnames(item97.totCount) <- c("BuildingType", "Total.Count")

#rbind by and across types
item97.merge1 <- rbind.data.frame(item97.sum1, item97.sum2, stringsAsFactors = F)

#merge on total counts
item97.merge2 <- left_join(item97.merge1, item97.totCount, by = "BuildingType")

#calculate percents and SEs
item97.merge2$Percent <- item97.merge2$Count / item97.merge2$Total.Count
item97.merge2$SE <- sqrt(item97.merge2$Percent * (1 - item97.merge2$Percent) / item97.merge2$SampleSize)

item97.final <- item97.merge2[which(colnames(item97.merge2) %in% c("BuildingType"
                                                                   ,"Generic"
                                                                   ,"Percent"
                                                                   ,"SE"
                                                                   ,"SampleSize"))]
item97.table <- item97.final[which(item97.final$BuildingType %in% c("Single Family")),]










#############################################################################################
#Item 98: DISTRIBUTION OF WATER HEATER LOCATION BY STATE (SF table 105, MH table 85)
#############################################################################################
#subset to columns needed for analysis
item98.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,"DHW.Location"))]
item98.dat$count <- 1

item98.dat0 <- item98.dat[which(item98.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item98.dat1 <- left_join(item98.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item98.dat2 <- item98.dat1[grep("Water Heater",item98.dat1$Generic),]
item98.dat2$DHW.Location[grep("storage|Storage",item98.dat2$DHW.Location)] <- "Storage"
item98.dat2$DHW.Location[grep("outside|Outside|exterior|Exterior",item98.dat2$DHW.Location)] <- "Outside"
item98.dat2$DHW.Location[grep("Other|2&3|Mechanical",item98.dat2$DHW.Location)] <- "Other"

unique(item98.dat2$DHW.Location)

# summarise by state
# summarise by location
item98.state1 <- summarise(group_by(item98.dat2, BuildingType, State, DHW.Location)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(count))

# summarise across location
item98.state2 <- summarise(group_by(item98.dat2, BuildingType, State)
                           ,DHW.Location = "Total"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(count))


# summarise across states
# summarise by location
item98.region1 <- summarise(group_by(item98.dat2, BuildingType, DHW.Location)
                           ,State = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(count))

# summarise across location
item98.region2 <- summarise(group_by(item98.dat2, BuildingType)
                            ,DHW.Location = "Total"
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(count))

# total counts
item98.totCount <- rbind.data.frame(item98.state2, item98.region2, stringsAsFactors = F)
item98.totCount1 <- item98.totCount[which(colnames(item98.totCount) %in% c("BuildingType", "State", "Count"))]
colnames(item98.totCount1) <- c("BuildingType", "State", "Total.Count")

item98.merge <- rbind.data.frame(item98.state1, item98.state2, item98.region1, item98.region2, stringsAsFactors = F)

item98.final <- left_join(item98.merge, item98.totCount1, by = c("BuildingType", "State"))
item98.final$Percent <- item98.final$Count / item98.final$Total.Count
item98.final$SE <- sqrt(item98.final$Percent * (1 - item98.final$Percent) / item98.final$SampleSize)

library(data.table)
item98.table <- dcast(setDT(item98.final)
                      ,formula = BuildingType + DHW.Location ~ State
                      ,value.var = c("Percent", "SE", "SampleSize"))

item98.table <- data.frame(item98.table)
item98.table0 <- item98.table[which(!(colnames(item98.table) %in% c("SampleSize_MT"
                                                                    ,"SampleSize_WA"
                                                                    ,"SampleSize_OR")))]

item98.table1 <- item98.table0[which(item98.table0$BuildingType %in% c("Single Family","Manufactured")),]

