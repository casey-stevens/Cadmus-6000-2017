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



#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))


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

#get sample sizes within building type categories
item80.sampleSize <- summarise(group_by(item80.sum, BuildingType)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

item80.merge <- left_join(item80.sum, item80.sampleSize, by = "BuildingType")

#summarise by type
item80.final <- summarise(group_by(item80.merge, BuildingType, Type)
                        ,SampleSize = unique(SampleSize)
                        ,Mean = sum(SiteCount) / SampleSize
                        ,SE   = sd(SiteCount) / sqrt(SampleSize))


item80.table <- data.frame("BuildingType" = item80.final$BuildingType
                           ,"Type" = item80.final$Type
                           ,"Mean" = item80.final$Mean
                           ,"SE" = item80.final$SE
                           ,"SampleSize" = item80.final$SampleSize)


item80.table1 <- item80.table[which(item80.table$BuildingType %in% c("Single Family","Manufactured")),]




item80.table.final <- item80.table1[which(item80.table1$Type %in% c("Dishwasher"
                                                                    ,"Dryer"
                                                                    ,"Washer"
                                                                    ,"Freezer"
                                                                    ,"Refrigerator")),]
View(item80.table.final)
########## For Water Heater ##########

item80.mech <- mechanical.dat[grep("Water Heat", mechanical.dat$Generic),]
item80.mech$WaterHeaterCount <- 1
item80.mech1 <- left_join(rbsa.dat, item80.mech, by = "CK_Cadmus_ID")
item80.mech2 <- unique(item80.mech1[-grep("Multifamily", item80.mech1$BuildingType),])
which(duplicated(item80.mech2$CK_Cadmus_ID))

item80.mech2$WaterHeaterCount[which(is.na(item80.mech2$WaterHeaterCount))] <- 0
item80.mech2$count <- 1

#summarise by home
item80.site <- summarise(group_by(item80.mech2, CK_Cadmus_ID, BuildingType)
                         ,WaterHeaterCount = sum(WaterHeaterCount))
unique(item80.site$WaterHeaterCount)

#summarise by Building Type
item80.mech.sum <- summarise(group_by(item80.site, BuildingType)
                             ,Mean = mean(WaterHeaterCount)
                             ,SE = sd(WaterHeaterCount) / sqrt(length(unique(CK_Cadmus_ID)))
                             ,SampleSize = length(unique(CK_Cadmus_ID)))


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

item81.table <- data.frame("BuildingType" = item81.final$BuildingType
                           ,"Equipment.Vintage" = item81.final$`Equipment Vintage`
                           ,"Percent" = item81.final$Percent
                           ,"SE" = item81.final$SE
                           ,"SampleSize" = item81.final$SampleSize)


item81.table1 <- item81.table[which(item81.table$BuildingType %in% c("Single Family","Manufactured")),]
View(item81.table1)
