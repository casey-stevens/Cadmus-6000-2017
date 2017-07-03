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
#Item 80: AVERAGE NUMBER OF APPLIANCES PER HOME BY TYPE (SF table 87, MH table 68)
#############################################################################################
#subset to columns needed for analysis
item80.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Type"
                                                               ,"Large.Unusual.Load.Quantity"
                                                               ,""
                                                               ,""
                                                               ,""))]
item80.dat$count <- 1

item80.dat0 <- item80.dat[which(item80.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item80.dat1 <- left_join(item80.dat0, rbsa.dat, by = "CK_Cadmus_ID")
item80.dat1$Large.Unusual.Load.Quantity[which(is.na(item80.dat1$Large.Unusual.Load.Quantity))] <- 1
unique(item80.dat1$Large.Unusual.Load.Quantity)
item80.dat1$Large.Unusual.Load.Quantity <- as.numeric(as.character(item80.dat1$Large.Unusual.Load.Quantity))


item80.dat1$TotalQty <- item80.dat1$Large.Unusual.Load.Quantity * item80.dat1$count

item80.sum <- summarise(group_by(item80.dat1, CK_Cadmus_ID, BuildingType, Type)
                        ,SiteCount = sum(TotalQty))

item80.final <- summarise(group_by(item80.sum, BuildingType, Type)
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        ,Mean = sum(SiteCount) / SampleSize
                        ,SE   = sd(SiteCount) / sqrt(SampleSize))

item80.final1 <- item80.final[which(item80.final$BuildingType %in% c("Single Family", "Manufactured")),]














#############################################################################################
#Item 81: DISTRIBUTION OF REFRIGERATOR/FREEZERS BY VINTAGE (SF table 88, MH table 69)
#############################################################################################
#subset to columns needed for analysis
item81.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,""))]
item81.dat$count <- 1

item81.dat0 <- item81.dat[which(item81.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item81.dat1 <- left_join(item81.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item81.dat2 <- item81.dat1[which(item81.dat1$Type == "Refrigerator"),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item81.dat2$EquipVintage_bins <- as.numeric(as.character(item81.dat2$Age))
item81.dat3 <- item81.dat2[which(!(is.na(item81.dat2$EquipVintage_bins))),]

item81.dat3$EquipVintage_bins[which(item81.dat3$Age < 1980)] <- "Pre 1980"
item81.dat3$EquipVintage_bins[which(item81.dat3$Age >= 1980 & item81.dat3$Age < 1990)] <- "1980-1989"
item81.dat3$EquipVintage_bins[which(item81.dat3$Age >= 1990 & item81.dat3$Age < 1995)] <- "1990-1994"
item81.dat3$EquipVintage_bins[which(item81.dat3$Age >= 1995 & item81.dat3$Age < 2000)] <- "1995-1999"
item81.dat3$EquipVintage_bins[which(item81.dat3$Age >= 2000 & item81.dat3$Age < 2005)] <- "2000-2004"
item81.dat3$EquipVintage_bins[which(item81.dat3$Age >= 2005 & item81.dat3$Age < 2009)] <- "2005-2009"
item81.dat3$EquipVintage_bins[which(item81.dat3$Age >= 2009)] <- "Post 2009"
#check uniques
unique(item81.dat3$EquipVintage_bins)

#count by vintage
item81.sum <- summarise(group_by(item81.dat3, BuildingType, Type, EquipVintage_bins)
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        ,Count = sum(count))
#count across vintages (i.e. total counts)
item81.sum1 <- summarise(group_by(item81.dat3, BuildingType, Type)
                         ,EquipVintage_bins = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item81.count <- rbind.data.frame(item81.sum, item81.sum1, stringsAsFactors = F)

item81.totalCount <- left_join(item81.count, item81.sum1, by = c("BuildingType","Type"))
colnames(item81.totalCount) <- c("BuildingType"
                                 ,"Remove"
                                 ,"Equipment Vintage"
                                 ,"SampleSize"
                                 ,"Count"
                                 ,"Remove"
                                 ,"Remove"
                                 ,"TotalCount")
item81.final <- item81.totalCount[which(colnames(item81.totalCount) != "Remove")]
item81.final$Percent <- item81.final$Count / item81.final$TotalCount
item81.final$SE      <- sqrt(item81.final$Percent * (1 - item81.final$Percent) / item81.final$SampleSize)

item81.final1 <- item81.final[which(colnames(item81.final) %in% c("BuildingType"
                                                                  ,"Remove"
                                                                  ,"Equipment Vintage"
                                                                  ,"SampleSize"
                                                                  ,"Percent"
                                                                  ,"SE"))]

item81.table <- item81.final1
