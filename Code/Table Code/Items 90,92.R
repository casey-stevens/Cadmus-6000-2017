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
#Item 90: DISTRIBUTION OF CLOTHES DRYERS BY VINTAGE (SF table 97, MH table 78)
#############################################################################################
#subset to columns needed for analysis
item90.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,""))]
item90.dat$count <- 1

item90.dat0 <- item90.dat[which(item90.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item90.dat1 <- left_join(item90.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item90.dat2 <- item90.dat1[which(item90.dat1$Type == "Dryer"),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item90.dat2$EquipVintage_bins <- as.numeric(as.character(item90.dat2$Age))
item90.dat3 <- item90.dat2[which(!(is.na(item90.dat2$EquipVintage_bins))),]

item90.dat3$EquipVintage_bins[which(item90.dat3$Age < 1980)] <- "Pre 1980"
item90.dat3$EquipVintage_bins[which(item90.dat3$Age >= 1980 & item90.dat3$Age < 1990)] <- "1980-1989"
item90.dat3$EquipVintage_bins[which(item90.dat3$Age >= 1990 & item90.dat3$Age < 1995)] <- "1990-1994"
item90.dat3$EquipVintage_bins[which(item90.dat3$Age >= 1995 & item90.dat3$Age < 2000)] <- "1995-1999"
item90.dat3$EquipVintage_bins[which(item90.dat3$Age >= 2000 & item90.dat3$Age < 2005)] <- "2000-2004"
item90.dat3$EquipVintage_bins[which(item90.dat3$Age >= 2005 & item90.dat3$Age < 2009)] <- "2005-2009"
item90.dat3$EquipVintage_bins[which(item90.dat3$Age >= 2009)] <- "Post 2009"
#check uniques
unique(item90.dat3$EquipVintage_bins)

#count by vintage
item90.sum <- summarise(group_by(item90.dat3, BuildingType, Type, EquipVintage_bins)
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        ,Count = sum(count))
#count across vintages (i.e. total counts)
item90.sum1 <- summarise(group_by(item90.dat3, BuildingType, Type)
                         ,EquipVintage_bins = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item90.count <- rbind.data.frame(item90.sum, item90.sum1, stringsAsFactors = F)

item90.totalCount <- left_join(item90.count, item90.sum1, by = c("BuildingType","Type"))
colnames(item90.totalCount) <- c("BuildingType"
                                 ,"Remove"
                                 ,"Equipment Vintage"
                                 ,"SampleSize"
                                 ,"Count"
                                 ,"Remove"
                                 ,"Remove"
                                 ,"TotalCount")
item90.final <- item90.totalCount[which(colnames(item90.totalCount) != "Remove")]
item90.final$Percent <- item90.final$Count / item90.final$TotalCount
item90.final$SE      <- sqrt(item90.final$Percent * (1 - item90.final$Percent) / item90.final$SampleSize)

item90.final1 <- item90.final[which(colnames(item90.final) %in% c("BuildingType"
                                                                  ,"Remove"
                                                                  ,"Equipment Vintage"
                                                                  ,"SampleSize"
                                                                  ,"Percent"
                                                                  ,"SE"))]

item90.table <- item90.final1[which(item90.final1$BuildingType %in% c("Single Family", "Manufactured")),]










#############################################################################################
#Item 92: DISTRIBUTION OF DISHWASHERS BY VINTAGE (SF table 99, MH table 80)
#############################################################################################
#subset to columns needed for analysis
item92.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,""))]
item92.dat$count <- 1

item92.dat0 <- item92.dat[which(item92.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item92.dat1 <- left_join(item92.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item92.dat2 <- item92.dat1[which(item92.dat1$Type == "Dishwasher"),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item92.dat2$EquipVintage_bins <- as.numeric(as.character(item92.dat2$Age))
item92.dat3 <- item92.dat2[which(!(is.na(item92.dat2$EquipVintage_bins))),]

item92.dat3$EquipVintage_bins[which(item92.dat3$Age < 1980)] <- "Pre 1980"
item92.dat3$EquipVintage_bins[which(item92.dat3$Age >= 1980 & item92.dat3$Age < 1990)] <- "1980-1989"
item92.dat3$EquipVintage_bins[which(item92.dat3$Age >= 1990 & item92.dat3$Age < 1995)] <- "1990-1994"
item92.dat3$EquipVintage_bins[which(item92.dat3$Age >= 1995 & item92.dat3$Age < 2000)] <- "1995-1999"
item92.dat3$EquipVintage_bins[which(item92.dat3$Age >= 2000 & item92.dat3$Age < 2005)] <- "2000-2004"
item92.dat3$EquipVintage_bins[which(item92.dat3$Age >= 2005 & item92.dat3$Age < 2009)] <- "2005-2009"
item92.dat3$EquipVintage_bins[which(item92.dat3$Age >= 2009)] <- "Post 2009"
#check uniques
unique(item92.dat3$EquipVintage_bins)

#count by vintage
item92.sum <- summarise(group_by(item92.dat3, BuildingType, Type, EquipVintage_bins)
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        ,Count = sum(count))
#count across vintages (i.e. total counts)
item92.sum1 <- summarise(group_by(item92.dat3, BuildingType, Type)
                         ,EquipVintage_bins = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item92.count <- rbind.data.frame(item92.sum, item92.sum1, stringsAsFactors = F)

item92.totalCount <- left_join(item92.count, item92.sum1, by = c("BuildingType","Type"))
colnames(item92.totalCount) <- c("BuildingType"
                                 ,"Remove"
                                 ,"Equipment Vintage"
                                 ,"SampleSize"
                                 ,"Count"
                                 ,"Remove"
                                 ,"Remove"
                                 ,"TotalCount")
item92.final <- item92.totalCount[which(colnames(item92.totalCount) != "Remove")]
item92.final$Percent <- item92.final$Count / item92.final$TotalCount
item92.final$SE      <- sqrt(item92.final$Percent * (1 - item92.final$Percent) / item92.final$SampleSize)

item92.final1 <- item92.final[which(colnames(item92.final) %in% c("BuildingType"
                                                                  ,"Remove"
                                                                  ,"Equipment Vintage"
                                                                  ,"SampleSize"
                                                                  ,"Percent"
                                                                  ,"SE"))]

item92.table <- item92.final1[which(item92.final1$BuildingType %in% c("Single Family", "Manufactured")),]



