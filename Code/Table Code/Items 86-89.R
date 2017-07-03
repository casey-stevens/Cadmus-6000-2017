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
#Item 86:  (SF table , MH table )
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

item86.final1 <- item86.final[which(colnames(item86.final) %in% c("BuildingType"
                                                                  ,"Remove"
                                                                  ,"Equipment Vintage"
                                                                  ,"SampleSize"
                                                                  ,"Percent"
                                                                  ,"SE"))]

item86.table <- item86.final1[which(item86.final1$BuildingType %in% c("Single Family", "Manufactured"))]








#############################################################################################
#Item 87:  (SF table , MH table )
#############################################################################################
