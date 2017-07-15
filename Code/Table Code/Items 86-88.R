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
#Item 86: DISTRIBUTION OF CLOTHES WASHERS BY VINTAGE (SF table 93, MH table 74)
#############################################################################################
#subset to columns needed for analysis
item86.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,""))]
item86.dat$count <- 1

item86.dat0 <- item86.dat[which(item86.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item86.dat1 <- left_join(item86.dat0, rbsa.dat, by = "CK_Cadmus_ID")


item86.dat2 <- item86.dat1[which(item86.dat1$Type == "Washer"),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item86.dat2$EquipVintage_bins <- as.numeric(as.character(item86.dat2$Age))
item86.dat3 <- item86.dat2[which(!(is.na(item86.dat2$EquipVintage_bins))),]

item86.dat3$EquipVintage_bins[which(item86.dat3$Age < 1980)] <- "Pre 1980"
item86.dat3$EquipVintage_bins[which(item86.dat3$Age >= 1980 & item86.dat3$Age < 1990)] <- "1980-1989"
item86.dat3$EquipVintage_bins[which(item86.dat3$Age >= 1990 & item86.dat3$Age < 1995)] <- "1990-1994"
item86.dat3$EquipVintage_bins[which(item86.dat3$Age >= 1995 & item86.dat3$Age < 2000)] <- "1995-1999"
item86.dat3$EquipVintage_bins[which(item86.dat3$Age >= 2000 & item86.dat3$Age < 2005)] <- "2000-2004"
item86.dat3$EquipVintage_bins[which(item86.dat3$Age >= 2005 & item86.dat3$Age < 2009)] <- "2005-2009"
item86.dat3$EquipVintage_bins[which(item86.dat3$Age >= 2009)] <- "Post 2009"
#check uniques
unique(item86.dat3$EquipVintage_bins)

#count by vintage
item86.sum <- summarise(group_by(item86.dat3, BuildingType, Type, EquipVintage_bins)
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        ,Count = sum(count))
#count across vintages (i.e. total counts)
item86.sum1 <- summarise(group_by(item86.dat3, BuildingType, Type)
                         ,EquipVintage_bins = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item86.count <- rbind.data.frame(item86.sum, item86.sum1, stringsAsFactors = F)

item86.totalCount <- left_join(item86.count, item86.sum1, by = c("BuildingType","Type"))
colnames(item86.totalCount) <- c("BuildingType"
                                 ,"Remove"
                                 ,"Equipment Vintage"
                                 ,"SampleSize"
                                 ,"Count"
                                 ,"Remove"
                                 ,"Remove"
                                 ,"TotalCount")
item86.final <- item86.totalCount[which(colnames(item86.totalCount) != "Remove")]
item86.final$Percent <- item86.final$Count / item86.final$TotalCount
item86.final$SE      <- sqrt(item86.final$Percent * (1 - item86.final$Percent) / item86.final$SampleSize)

item86.table <- data.frame("BuildingType" = item86.final$BuildingType
                           ,"Equipment.Vintage" = item86.final$`Equipment Vintage`
                           ,"Percent" = item86.final$Percent
                           ,"SE" = item86.final$SE
                           ,"SampleSize" = item86.final$SampleSize)


item86.table1 <- item86.table[which(item86.table$BuildingType %in% c("Single Family","Manufactured")),]
View(item86.table1)




#############################################################################################
#Item 87: DISTRIBUTION OF CLOTHES WASHERS BY TYPE AND STATE (SF table 94)
#############################################################################################
#subset to columns needed for analysis
item87.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,"Washer.Type"))]
item87.dat$count <- 1

item87.dat0 <- item87.dat[which(item87.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item87.dat1 <- left_join(item87.dat0, rbsa.dat, by = "CK_Cadmus_ID")


item87.dat2 <- item87.dat1[which(item87.dat1$Type == "Washer"),]



##Summarise by state
#summarise by washer types
item87.sum1 <- summarise(group_by(item87.dat2, BuildingType, State, Washer.Type)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))
#summarise across washer types
item87.sum2 <- summarise(group_by(item87.dat2, BuildingType, State)
                         ,Washer.Type = "All Clothes Washer Types"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

#row bind
item87.merge1 <- rbind.data.frame(item87.sum1, item87.sum2, stringsAsFactors = F)

##Summarise across states (for the region)
#summarise by washer types
item87.sum3 <- summarise(group_by(item87.dat2, BuildingType, Washer.Type)
                         ,State = "Region"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))
#summarise across washer types
item87.sum4 <- summarise(group_by(item87.dat2, BuildingType)
                         ,State = "Region"
                         ,Washer.Type = "All Clothes Washer Types"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

#row bind
item87.merge2 <- rbind.data.frame(item87.sum3, item87.sum4, stringsAsFactors = F)

#row bind state and region information
item87.merge3 <- rbind.data.frame(item87.merge1, item87.merge2, stringsAsFactors = F)

#rbind for total counts
item87.totCounts <- rbind.data.frame(item87.sum2, item87.sum4, stringsAsFactors = F)

#left join to get total counts for percents later
item87.join <- left_join(item87.merge3, item87.totCounts, by = c("BuildingType", "State"))
colnames(item87.join) <- c("BuildingType"
                           ,"State"
                           ,"Washer.Type"
                           ,"SampleSize"
                           ,"Count"
                           ,"Remove"
                           ,"Remove"
                           ,"Total.Count")
item87.join1 <- item87.join[which(colnames(item87.join) != "Remove")]
item87.join1$Percent <- item87.join1$Count / item87.join1$Total.Count
item87.join1$SE <- sqrt(item87.join1$Percent * (1 - item87.join1$Percent) / item87.join1$SampleSize)


library(data.table)
item87.cast <- dcast(setDT(item87.join1),
                     formula   = BuildingType + Washer.Type ~ State, sum,
                     value.var = c("Percent","SE", "SampleSize"))

#subset to only the columns needed for the final RBSA table
item87.table <- data.frame("BuildingType" = item87.cast$BuildingType
                           ,"Washer.Type" = item87.cast$Washer.Type
                           ,"Percent_MT" = item87.cast$Percent_MT
                           ,"SE_MT" = item87.cast$SE_MT
                           ,"Percent_WA" = item87.cast$Percent_WA
                           ,"SE_WA" = item87.cast$SE_WA
                           ,"Percent_Region" = item87.cast$Percent_Region
                           ,"SE_Region" = item87.cast$SE_Region
                           ,"SampleSize" = item87.cast$SampleSize_Region)



#subset to only the relevant building types for this item
item87.table1 <- item87.table[which(item87.table$BuildingType %in% c("Single Family")),]









#############################################################################################
#Item 88: DISTRIBUTION OF CLOTHES WASHERS BY TYPE AND VINTAGE (SF table 95, MH table 76)
#############################################################################################
#subset to columns needed for analysis
item88.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,"Washer.Type"))]
item88.dat$count <- 1

item88.dat0 <- item88.dat[which(item88.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item88.dat1 <- left_join(item88.dat0, rbsa.dat, by = "CK_Cadmus_ID")


item88.dat2 <- item88.dat1[which(item88.dat1$Type == "Washer"),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item88.dat2$EquipVintage_bins <- as.numeric(as.character(item88.dat2$Age))
item88.dat3 <- item88.dat2[which(!(is.na(item88.dat2$EquipVintage_bins))),]

item88.dat3$EquipVintage_bins[which(item88.dat3$Age < 1990)] <- "Pre 1990"
item88.dat3$EquipVintage_bins[which(item88.dat3$Age >= 1990 & item88.dat3$Age < 1995)] <- "1990-1994"
item88.dat3$EquipVintage_bins[which(item88.dat3$Age >= 1995 & item88.dat3$Age < 2000)] <- "1995-1999"
item88.dat3$EquipVintage_bins[which(item88.dat3$Age >= 2000 & item88.dat3$Age < 2005)] <- "2000-2004"
item88.dat3$EquipVintage_bins[which(item88.dat3$Age >= 2005 & item88.dat3$Age < 2009)] <- "2005-2009"
item88.dat3$EquipVintage_bins[which(item88.dat3$Age >= 2009)] <- "Post 2009"
#check uniques
unique(item88.dat3$EquipVintage_bins)


##Summarise by EquipVintage_bins
#summarise by washer types
item88.sum1 <- summarise(group_by(item88.dat3, BuildingType, EquipVintage_bins, Washer.Type)
                         ,Count = sum(count))
#summarise across washer types
item88.sum2 <- summarise(group_by(item88.dat3, BuildingType, EquipVintage_bins)
                         ,Washer.Type = "All Clothes Washer Types"
                         ,Count = sum(count))

# total counts
item88.totCounts1 <- summarise(group_by(item88.dat3, BuildingType, Washer.Type)
                               ,EquipVintage_bins = "All Equip Vintages"
                               ,SampleSize = length(unique(CK_Cadmus_ID))
                               ,Total.Count = sum(count))
#summarise across washer types
item88.totCounts2 <- summarise(group_by(item88.dat3, BuildingType)
                         ,Washer.Type = "All Clothes Washer Types"
                         ,EquipVintage_bins = "All Equip Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Total.Count = sum(count))
#rbind
item88.totCount <- rbind.data.frame(item88.totCounts1, item88.totCounts2, stringsAsFactors = F)


#row bind
item88.merge1 <- rbind.data.frame(item88.sum1, item88.sum2, stringsAsFactors = F)

#left join to get total counts for percents later
item88.join <- left_join(item88.merge1, item88.totCount, by = c("BuildingType", "Washer.Type"))
colnames(item88.join) <- c("BuildingType"
                           ,"EquipVintage_bins"
                           ,"Washer.Type"
                           ,"Count"
                           ,"Remove"
                           ,"SampleSize"
                           ,"Total.Count")
item88.join1 <- item88.join[which(colnames(item88.join) != "Remove")]

item88.join1$Percent <- item88.join1$Count / item88.join1$Total.Count
item88.join1$SE      <- sqrt(item88.join1$Percent * (1 - item88.join1$Percent) / item88.join1$SampleSize)


library(data.table)
item88.cast <- dcast(setDT(item88.join1),
                     formula   = BuildingType + Washer.Type + SampleSize ~ EquipVintage_bins, sum,
                     value.var = c("Percent","SE"))

item88.table <- data.frame("BuildingType" = item88.cast$BuildingType
                          ,"Washer.Type" = item88.cast$Washer.Type
                          ,"Percent_Pre1990" = item88.cast$`Percent_Pre 1990`
                          ,"SE_Pre1990" = item88.cast$`SE_Pre 1990`
                          ,"Percent_1990_1994" = item88.cast$`Percent_1990-1994`
                          ,"SE_1990_1994" = item88.cast$`SE_1990-1994`
                          ,"Percent_1995_1999" = item88.cast$`Percent_1995-1999`
                          ,"SE_1995_1999" = item88.cast$`SE_1995-1999`
                          ,"Percent_2000_2004" = item88.cast$`Percent_2000-2004`
                          ,"SE_2000_2004" = item88.cast$`SE_2000-2004`
                          ,"Percent_2005_2009" = item88.cast$`Percent_2005-2009`
                          ,"SE_2005_2009" = item88.cast$`SE_2005-2009`
                          ,"Percent_Post_2009" = item88.cast$`Percent_Post 2009`
                          ,"SE_Post_2009" = item88.cast$`SE_Post 2009`
                          ,"SampleSize" = item88.cast$SampleSize)

#subset to only building types wanted for this item
item88.table1 <- item88.table[which(item88.table$BuildingType %in% c("Single Family", "Manufactured")),]


