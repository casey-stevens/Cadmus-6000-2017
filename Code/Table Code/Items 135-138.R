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
#Item 135: DISTRIBUTION OF WOOD USE AS HEATING FUEL BY STATE (SF table 142, MH table 117)
#############################################################################################
#subset to columns needed for analysis
item135.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseOtherFuel_QuantityOfWood_Cords"
                                                                                     ,""))])

colnames(item135.dat) <- c("CK_Cadmus_ID", "QTY")
#remove any repeat header rows from exporting
item135.dat0 <- item135.dat[which(item135.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item135.dat1 <- item135.dat0[which(!(is.na(item135.dat0$QTY))),]
  
item135.dat2 <- left_join(rbsa.dat, item135.dat1, by = "CK_Cadmus_ID")
item135.dat2$count <- 1

item135.dat2$QTY_bins <- as.numeric(as.character(item135.dat2$QTY))
item135.dat2$QTY_bins[which(item135.dat2$QTY < 1)] <- "< 1 Cord"
item135.dat2$QTY_bins[which(item135.dat2$QTY >= 1 & item135.dat2$QTY < 4)] <- "1-3 Cords"
item135.dat2$QTY_bins[which(item135.dat2$QTY >= 4 & item135.dat2$QTY < 7)] <- "4-6 Cords"
item135.dat2$QTY_bins[which(item135.dat2$QTY > 6)] <- "> 6 Cords"
item135.dat2$QTY_bins[which(is.na(item135.dat2$QTY))] <- "None"


#summarise by state
item135.state <- summarise(group_by(item135.dat2, BuildingType, State, QTY_bins)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(count))
item135.state.total <- summarise(group_by(item135.dat2, BuildingType, State)
                                 ,QTY_bins = "Total"
                                 ,SampleSize = length(unique(CK_Cadmus_ID))
                                 ,Count = sum(count))

#summarise across states
item135.region <- summarise(group_by(item135.dat2, BuildingType, QTY_bins)
                           ,State = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(count))
item135.region.total <- summarise(group_by(item135.dat2, BuildingType)
                                 ,State = "Region"
                                 ,QTY_bins = "Total"
                                 ,SampleSize = length(unique(CK_Cadmus_ID))
                                 ,Count = sum(count))

#row bind state and region info
item135.merge1 <- rbind.data.frame(item135.state, item135.state.total, item135.region, item135.region.total, stringsAsFactors = F)

#rowbind total information
item135.merge2 <- rbind.data.frame(item135.state.total, item135.region.total, stringsAsFactors = F)
item135.merge2 <- item135.merge2[which(colnames(item135.merge2) %in% c("BuildingType"
                                                                       ,"State"
                                                                       ,"Count"))]

#join on total counts
item135.final <- left_join(item135.merge1, item135.merge2, by = c("BuildingType", "State"))
colnames(item135.final) <- c("BuildingType", "State", "QTY_bins", "SampleSize", "Count", "Total.Count")

item135.final$Percent <- item135.final$Count / item135.final$Total.Count
item135.final$SE <- sqrt(item135.final$Percent * (1 - item135.final$Percent) / item135.final$SampleSize)

library(data.table)
item135.table <- dcast(setDT(item135.final)
                       ,formula = BuildingType + QTY_bins ~ State
                       ,value.var = c("Percent", "SE", "SampleSize"))
item135.table <- data.frame(item135.table, stringsAsFactors = F)

item135.table1 <- data.frame("BuildingType" = item135.table$BuildingType
                             ,"Annual.Wood.Use" = item135.table$QTY_bins
                             ,"Percent_MT" = item135.table$Percent_MT
                             ,"SE_MT" = item135.table$SE_MT
                             ,"Percent_OR" = item135.table$Percent_OR
                             ,"SE_OR" = item135.table$SE_OR
                             ,"Percent_WA" = item135.table$Percent_WA
                             ,"SE_WA" = item135.table$SE_WA
                             ,"Percent_Region" = item135.table$Percent_Region
                             ,"SE_Region" = item135.table$SE_Region
                             ,"SampleSize" = item135.table$SampleSize_Region)

item135.table.final <- item135.table1[which(item135.table1$BuildingType %in% c("Single Family", "Manufactured")),]














#############################################################################################
#Item 136: DISTRIBUTION OF PELLET USE AS HEATING FUEL BY STATE (SF table 143, MH table 118)
#############################################################################################
#subset to columns needed for analysis
item136.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseOtherFuel_QuantityOfPellets_Tons"
                                                                                     ,""))])

colnames(item136.dat) <- c("CK_Cadmus_ID", "QTY")
#remove any repeat header rows from exporting
item136.dat0 <- item136.dat[which(item136.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item136.dat1 <- item136.dat0[which(!(is.na(item136.dat0$QTY))),]

item136.dat2 <- left_join(rbsa.dat, item136.dat1, by = "CK_Cadmus_ID")
item136.dat2$count <- 1

item136.dat2$QTY_bins <- as.numeric(as.character(item136.dat2$QTY))
item136.dat2$QTY_bins[which(item136.dat2$QTY < 1)] <- "< 1 Ton"
item136.dat2$QTY_bins[which(item136.dat2$QTY >= 1 & item136.dat2$QTY < 3)] <- "1-2 Tons"
item136.dat2$QTY_bins[which(item136.dat2$QTY >= 2 & item136.dat2$QTY < 5)] <- "2-4 Tons"
item136.dat2$QTY_bins[which(item136.dat2$QTY > 4)] <- "> 4 Tons"
item136.dat2$QTY_bins[which(is.na(item136.dat2$QTY))] <- "None"


#summarise by state
item136.state <- summarise(group_by(item136.dat2, BuildingType, State, QTY_bins)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(count))
item136.state.total <- summarise(group_by(item136.dat2, BuildingType, State)
                                 ,QTY_bins = "Total"
                                 ,SampleSize = length(unique(CK_Cadmus_ID))
                                 ,Count = sum(count))

#summarise across states
item136.region <- summarise(group_by(item136.dat2, BuildingType, QTY_bins)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(count))
item136.region.total <- summarise(group_by(item136.dat2, BuildingType)
                                  ,State = "Region"
                                  ,QTY_bins = "Total"
                                  ,SampleSize = length(unique(CK_Cadmus_ID))
                                  ,Count = sum(count))

#row bind state and region info
item136.merge1 <- rbind.data.frame(item136.state, item136.state.total, item136.region, item136.region.total, stringsAsFactors = F)

#rowbind total information
item136.merge2 <- rbind.data.frame(item136.state.total, item136.region.total, stringsAsFactors = F)
item136.merge2 <- item136.merge2[which(colnames(item136.merge2) %in% c("BuildingType"
                                                                       ,"State"
                                                                       ,"Count"))]

#join on total counts
item136.final <- left_join(item136.merge1, item136.merge2, by = c("BuildingType", "State"))
colnames(item136.final) <- c("BuildingType", "State", "QTY_bins", "SampleSize", "Count", "Total.Count")

item136.final$Percent <- item136.final$Count / item136.final$Total.Count
item136.final$SE <- sqrt(item136.final$Percent * (1 - item136.final$Percent) / item136.final$SampleSize)

library(data.table)
item136.table <- dcast(setDT(item136.final)
                       ,formula = BuildingType + QTY_bins ~ State
                       ,value.var = c("Percent", "SE", "SampleSize"))
item136.table <- data.frame(item136.table, stringsAsFactors = F)

item136.table1 <- data.frame("BuildingType" = item136.table$BuildingType
                             ,"Annual.Pellet.Fuel.Use" = item136.table$QTY_bins
                             ,"Percent_MT" = item136.table$Percent_MT
                             ,"SE_MT" = item136.table$SE_MT
                             ,"Percent_OR" = item136.table$Percent_OR
                             ,"SE_OR" = item136.table$SE_OR
                             ,"Percent_WA" = item136.table$Percent_WA
                             ,"SE_WA" = item136.table$SE_WA
                             ,"Percent_Region" = item136.table$Percent_Region
                             ,"SE_Region" = item136.table$SE_Region
                             ,"SampleSize" = item136.table$SampleSize_Region)

item136.table.final <- item136.table1[which(item136.table1$BuildingType %in% c("Single Family", "Manufactured")),]
















#############################################################################################
#Item 137: DISTRIBUTION OF OIL USE AS HEATING FUEL BY STATE (SF table 144)
#############################################################################################
#subset to columns needed for analysis
item137.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseOtherFuel_QuantityOfFuelOil_Kerosene_Gallons"
                                                                                     ,""))])

colnames(item137.dat) <- c("CK_Cadmus_ID", "QTY")
#remove any repeat header rows from exporting
item137.dat0 <- item137.dat[which(item137.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item137.dat1 <- item137.dat0[which(!(is.na(item137.dat0$QTY))),]

item137.dat2 <- left_join(rbsa.dat, item137.dat1, by = "CK_Cadmus_ID")
item137.dat2$count <- 1

item137.dat2$QTY_bins <- as.numeric(as.character(item137.dat2$QTY))
item137.dat2$QTY_bins[which(item137.dat2$QTY < 100)] <- "< 100 Gallons"
item137.dat2$QTY_bins[which(item137.dat2$QTY >= 100 & item137.dat2$QTY < 251)] <- "100-250 Gallons"
item137.dat2$QTY_bins[which(item137.dat2$QTY >= 250 & item137.dat2$QTY < 501)] <- "250-500 Gallons"
item137.dat2$QTY_bins[which(item137.dat2$QTY >= 500 & item137.dat2$QTY < 1001)] <- "500-1000 Gallons"
item137.dat2$QTY_bins[which(item137.dat2$QTY > 1000)] <- "> 1000 Gallons"
item137.dat2$QTY_bins[which(is.na(item137.dat2$QTY))] <- "None"


#summarise by state
item137.state <- summarise(group_by(item137.dat2, BuildingType, State, QTY_bins)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(count))
item137.state.total <- summarise(group_by(item137.dat2, BuildingType, State)
                                 ,QTY_bins = "Total"
                                 ,SampleSize = length(unique(CK_Cadmus_ID))
                                 ,Count = sum(count))

#summarise across states
item137.region <- summarise(group_by(item137.dat2, BuildingType, QTY_bins)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(count))
item137.region.total <- summarise(group_by(item137.dat2, BuildingType)
                                  ,State = "Region"
                                  ,QTY_bins = "Total"
                                  ,SampleSize = length(unique(CK_Cadmus_ID))
                                  ,Count = sum(count))

#row bind state and region info
item137.merge1 <- rbind.data.frame(item137.state, item137.state.total, item137.region, item137.region.total, stringsAsFactors = F)

#rowbind total information
item137.merge2 <- rbind.data.frame(item137.state.total, item137.region.total, stringsAsFactors = F)
item137.merge2 <- item137.merge2[which(colnames(item137.merge2) %in% c("BuildingType"
                                                                       ,"State"
                                                                       ,"Count"))]

#join on total counts
item137.final <- left_join(item137.merge1, item137.merge2, by = c("BuildingType", "State"))
colnames(item137.final) <- c("BuildingType", "State", "QTY_bins", "SampleSize", "Count", "Total.Count")

item137.final$Percent <- item137.final$Count / item137.final$Total.Count
item137.final$SE <- sqrt(item137.final$Percent * (1 - item137.final$Percent) / item137.final$SampleSize)

library(data.table)
item137.table <- dcast(setDT(item137.final)
                       ,formula = BuildingType + QTY_bins ~ State
                       ,value.var = c("Percent", "SE", "SampleSize"))
item137.table <- data.frame(item137.table, stringsAsFactors = F)

item137.table1 <- data.frame("BuildingType" = item137.table$BuildingType
                             ,"Annual.Oil.Fuel.Use" = item137.table$QTY_bins
                             ,"Percent_MT" = item137.table$Percent_MT
                             ,"SE_MT" = item137.table$SE_MT
                             ,"Percent_OR" = item137.table$Percent_OR
                             ,"SE_OR" = item137.table$SE_OR
                             ,"Percent_WA" = item137.table$Percent_WA
                             ,"SE_WA" = item137.table$SE_WA
                             ,"Percent_Region" = item137.table$Percent_Region
                             ,"SE_Region" = item137.table$SE_Region
                             ,"SampleSize" = item137.table$SampleSize_Region)

item137.table.final <- item137.table1[which(item137.table1$BuildingType %in% c("Single Family")),]

















#############################################################################################
#Item 138: DISTRIBUTION OF PROPANE USE AS HEATING FUEL BY STATE (SF table 145)
#############################################################################################
#subset to columns needed for analysis
item138.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseOtherFuel_QuantityOfPropane_Gallons"
                                                                                     ,""))])

colnames(item138.dat) <- c("CK_Cadmus_ID", "QTY")
#remove any repeat header rows from exporting
item138.dat0 <- item138.dat[which(item138.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item138.dat1 <- item138.dat0[which(!(is.na(item138.dat0$QTY))),]

item138.dat2 <- left_join(rbsa.dat, item138.dat1, by = "CK_Cadmus_ID")
item138.dat2$count <- 1

item138.dat2$QTY_bins <- as.numeric(as.character(item138.dat2$QTY))
item138.dat2$QTY_bins[which(item138.dat2$QTY < 50)] <- "< 50 Gallons"
item138.dat2$QTY_bins[which(item138.dat2$QTY >= 50 & item138.dat2$QTY < 251)] <- "50-250 Gallons"
item138.dat2$QTY_bins[which(item138.dat2$QTY >= 250 & item138.dat2$QTY < 501)] <- "250-500 Gallons"
item138.dat2$QTY_bins[which(item138.dat2$QTY >= 500 & item138.dat2$QTY < 1001)] <- "500-1000 Gallons"
item138.dat2$QTY_bins[which(item138.dat2$QTY > 1000)] <- "> 1000 Gallons"
item138.dat2$QTY_bins[which(is.na(item138.dat2$QTY))] <- "None"


#summarise by state
item138.state <- summarise(group_by(item138.dat2, BuildingType, State, QTY_bins)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(count))
item138.state.total <- summarise(group_by(item138.dat2, BuildingType, State)
                                 ,QTY_bins = "Total"
                                 ,SampleSize = length(unique(CK_Cadmus_ID))
                                 ,Count = sum(count))

#summarise across states
item138.region <- summarise(group_by(item138.dat2, BuildingType, QTY_bins)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(count))
item138.region.total <- summarise(group_by(item138.dat2, BuildingType)
                                  ,State = "Region"
                                  ,QTY_bins = "Total"
                                  ,SampleSize = length(unique(CK_Cadmus_ID))
                                  ,Count = sum(count))

#row bind state and region info
item138.merge1 <- rbind.data.frame(item138.state, item138.state.total, item138.region, item138.region.total, stringsAsFactors = F)

#rowbind total information
item138.merge2 <- rbind.data.frame(item138.state.total, item138.region.total, stringsAsFactors = F)
item138.merge2 <- item138.merge2[which(colnames(item138.merge2) %in% c("BuildingType"
                                                                       ,"State"
                                                                       ,"Count"))]

#join on total counts
item138.final <- left_join(item138.merge1, item138.merge2, by = c("BuildingType", "State"))
colnames(item138.final) <- c("BuildingType", "State", "QTY_bins", "SampleSize", "Count", "Total.Count")

item138.final$Percent <- item138.final$Count / item138.final$Total.Count
item138.final$SE <- sqrt(item138.final$Percent * (1 - item138.final$Percent) / item138.final$SampleSize)

library(data.table)
item138.table <- dcast(setDT(item138.final)
                       ,formula = BuildingType + QTY_bins ~ State
                       ,value.var = c("Percent", "SE", "SampleSize"))
item138.table <- data.frame(item138.table, stringsAsFactors = F)

item138.table1 <- data.frame("BuildingType" = item138.table$BuildingType
                             ,"Annual.Propane.Fuel.Use" = item138.table$QTY_bins
                             ,"Percent_MT" = item138.table$Percent_MT
                             ,"SE_MT" = item138.table$SE_MT
                             ,"Percent_OR" = item138.table$Percent_OR
                             ,"SE_OR" = item138.table$SE_OR
                             ,"Percent_WA" = item138.table$Percent_WA
                             ,"SE_WA" = item138.table$SE_WA
                             ,"Percent_Region" = item138.table$Percent_Region
                             ,"SE_Region" = item138.table$SE_Region
                             ,"SampleSize" = item138.table$SampleSize_Region)

item138.table.final <- item138.table1[which(item138.table1$BuildingType %in% c("Single Family")),]


