#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))



#############################################################################################
# Item 1
#############################################################################################
item1.dat <- rbsa.dat

item1.dat$count <- 1

#Get state information
#across home types
item1.state.tab0 <- summarise(group_by(item1.dat, BuildingType, State)
                              ,BuildingTypeXX = "Total"
                           ,Count = sum(count)
                           ,SampleSize = length(unique(CK_Cadmus_ID)))
#by home types
item1.state.tab1 <- summarise(group_by(item1.dat, BuildingType, BuildingTypeXX, State)
                           ,Count = sum(count)
                           ,SampleSize = length(unique(CK_Cadmus_ID)))
item1.state.full <- rbind.data.frame(item1.state.tab1, item1.state.tab0, stringsAsFactors = F)

#get region information
#across home types
item1.region.tab0 <- summarise(group_by(item1.dat, BuildingType)
                               ,BuildingTypeXX = "Total"
                            ,State = "Region"
                            ,Count = sum(count)
                            ,SampleSize = length(unique(CK_Cadmus_ID))
)
#by home types
item1.region.tab1 <- summarise(group_by(item1.dat, BuildingType, BuildingTypeXX)
                            ,State = "Region"
                            ,Count = sum(count)
                            ,SampleSize = length(unique(CK_Cadmus_ID))
)
item1.region.full <- rbind.data.frame(item1.region.tab1, item1.region.tab0, stringsAsFactors = F)

#rbind state and region information
item1.tab.full <- rbind.data.frame(item1.state.full, item1.region.full, stringsAsFactors = F)

#calculate total counts
item1.total <- rbind.data.frame(item1.state.tab0, item1.region.tab0, stringsAsFactors = F)
item1.total1 <- item1.total[which(colnames(item1.total) %in% c("BuildingType", "State", "Count"))]

# merge on total counts
item1.tab.full1 <- left_join(item1.tab.full, item1.total1, by = c("BuildingType", "State"))
colnames(item1.tab.full1) <- c("BuildingType", "Home.Type", "State", "Count", "SampleSize", "Total.Count")

item1.tab.full1$Percent <- item1.tab.full1$Count / item1.tab.full1$Total.Count
item1.tab.full1$SE      <- sqrt((item1.tab.full1$Percent * (1 - item1.tab.full1$Percent)) / item1.tab.full1$SampleSize)


#############################################################################################
library(data.table)
item1.table <- dcast(setDT(item1.tab.full1)
                     ,formula = BuildingType + Home.Type ~ State
                     ,value.var = c("Percent", "SE", "SampleSize"))

item1.table1 <- data.frame("BuildingType" = item1.table$BuildingType
                             ,"Home.Type" = item1.table$Home.Type
                             ,"Percent_MT" = item1.table$Percent_MT
                             ,"SE_MT" = item1.table$SE_MT
                             ,"Percent_OR" = item1.table$Percent_OR
                             ,"SE_OR" = item1.table$SE_OR
                             ,"Percent_WA" = item1.table$Percent_WA
                             ,"SE_WA" = item1.table$SE_WA
                             ,"Percent_Region" = item1.table$Percent_Region
                             ,"SE_Region" = item1.table$SE_Region
                             ,"SampleSize" = item1.table$SampleSize_Region)

item1.table.final <- item1.table1[which(item1.table1$BuildingType %in% c("Single Family", "Manufactured")),]




#############################################################################################
# Item 2
#############################################################################################
item2.dat <- rbsa.dat

item2.dat$count <- 1

#Get state information
#across home types
item2.state.tab0 <- summarise(group_by(item2.dat, BuildingType, State)
                              ,HomeYearBuilt_bins = "Total"
                              ,Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID)))

item2.state.tab1 <- summarise(group_by(item2.dat, BuildingType, HomeYearBuilt_bins, State)
                              ,Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID)))
item2.state.full <- rbind.data.frame(item2.state.tab1, item2.state.tab0, stringsAsFactors = F)

#get region information
item2.region.tab0 <- summarise(group_by(item2.dat, BuildingType)
                               ,HomeYearBuilt_bins = "Total"
                               ,State = "Region"
                               ,Count = sum(count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)
item2.region.tab1 <- summarise(group_by(item2.dat, BuildingType, HomeYearBuilt_bins)
                               ,State = "Region"
                               ,Count = sum(count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)
item2.region.full <- rbind.data.frame(item2.region.tab1, item2.region.tab0, stringsAsFactors = F)

item2.total <- rbind.data.frame(item2.state.tab0, item2.region.tab0, stringsAsFactors = F)
item2.total1 <- item2.total[which(colnames(item2.total) %in% c("BuildingType", "State", "Count"))]

#rbind state and region information
item2.tab.full <- rbind.data.frame(item2.state.full, item2.region.full, stringsAsFactors = F)

item2.final <- left_join(item2.tab.full, item2.total1, by = c("BuildingType", "State"))
colnames(item2.final) <- c("BuildingType", "Housing.Vintage", "State", "Count", "SampleSize", "Total.Count")

item2.final$Percent <- item2.final$Count / item2.final$Total.Count
item2.final$SE      <- sqrt((item2.final$Percent * (1 - item2.final$Percent)) / item2.final$SampleSize)

#############################################################################################
library(data.table)
item2.table <- dcast(setDT(item2.tab.full1)
                     ,formula = BuildingType + Housing.Vintage ~ State
                     ,value.var = c("Percent", "SE", "SampleSize"))

item2.table1 <- data.frame("BuildingType" = item2.table$BuildingType
                           ,"Housing.Vintage" = item2.table$Housing.Vintage
                           ,"Percent_MT" = item2.table$Percent_MT
                           ,"SE_MT" = item2.table$SE_MT
                           ,"Percent_OR" = item2.table$Percent_OR
                           ,"SE_OR" = item2.table$SE_OR
                           ,"Percent_WA" = item2.table$Percent_WA
                           ,"SE_WA" = item2.table$SE_WA
                           ,"Percent_Region" = item2.table$Percent_Region
                           ,"SE_Region" = item2.table$SE_Region
                           ,"SampleSize" = item2.table$SampleSize_Region)

item2.table2 <- item2.table1[which(item2.table1$BuildingType %in% c("Single Family", "Manufactured")),]
item2.table.final <- item2.table2[which(!(is.na(item2.table2$Housing.Vintage))),]




#############################################################################################
# Item 6: DISTRIBUTION OF HOMES BY BUILDING HEIGHT AND STATE (SF table 13)
#############################################################################################
item6.dat  <- rbsa.dat[which(!(is.na(rbsa.dat$BuildingHeight))),] ##only 369

item6.dat$count <- 1

#Get state information
#across home types
item6.state.tab0 <- summarise(group_by(item6.dat, BuildingType, State)
                              ,BuildingHeight = "Total"
                              ,Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID)))

item6.state.tab1 <- summarise(group_by(item6.dat, BuildingType, BuildingHeight, State)
                              ,Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID)))
item6.state.full <- rbind.data.frame(item6.state.tab1, item6.state.tab0, stringsAsFactors = F)

#get region information
item6.region.tab0 <- summarise(group_by(item6.dat, BuildingType)
                               ,BuildingHeight = "Total"
                               ,State = "Region"
                               ,Count = sum(count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)
item6.region.tab1 <- summarise(group_by(item6.dat, BuildingType, BuildingHeight)
                               ,State = "Region"
                               ,Count = sum(count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)
item6.region.full <- rbind.data.frame(item6.region.tab1, item6.region.tab0, stringsAsFactors = F)

item6.total <- rbind.data.frame(item6.state.tab0, item6.region.tab0, stringsAsFactors = F)
item6.total1 <- item6.total[which(colnames(item6.total) %in% c("BuildingType", "State", "Count"))]

#rbind state and region information
item6.tab.full <- rbind.data.frame(item6.state.full, item6.region.full, stringsAsFactors = F)

item6.tab.full1 <- left_join(item6.tab.full, item6.total1, by = c("BuildingType", "State"))
colnames(item6.tab.full1) <- c("BuildingType", "BuildingHeight", "State", "Count", "SampleSize", "Total.Count")

item6.tab.full1$Percent <- item6.tab.full1$Count / item6.tab.full1$Total.Count
item6.tab.full1$SE      <- sqrt((item6.tab.full1$Percent * (1 - item6.tab.full1$Percent)) / item6.tab.full1$SampleSize)

#############################################################################################
library(data.table)
item6.table <- dcast(setDT(item6.tab.full1)
                     ,formula = BuildingType + BuildingHeight ~ State
                     ,value.var = c("Percent", "SE", "SampleSize"))

item6.table1 <- data.frame("BuildingType" = item6.table$BuildingType
                           ,"BuildingHeight" = item6.table$BuildingHeight
                           ,"Percent_MT" = item6.table$Percent_MT
                           ,"SE_MT" = item6.table$SE_MT
                           ,"Percent_OR" = item6.table$Percent_OR
                           ,"SE_OR" = item6.table$SE_OR
                           ,"Percent_WA" = item6.table$Percent_WA
                           ,"SE_WA" = item6.table$SE_WA
                           ,"Percent_Region" = item6.table$Percent_Region
                           ,"SE_Region" = item6.table$SE_Region
                           ,"SampleSize" = item6.table$SampleSize_Region)

item6.table.final <- item6.table1[which(item6.table1$BuildingType %in% c("Single Family")),]
