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
#Item 191: AVERAGE COOLING EFFICIENCY (SEER) FOR POST-1989 CENTRAL AC SYSTEMS BY VINTAGE (MH TABLE 43)
#############################################################################################
#subset to columns needed for analysis
item191.dat <- unique(mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                           ,"Generic"
                                                                           ,"Component.1.Year.of.Manufacture"
                                                                           , "SEER"))])

#remove any repeat header rows from exporting
item191.dat0 <- item191.dat[which(item191.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item191.dat1 <- left_join(item191.dat0, rbsa.dat, by = "CK_Cadmus_ID")


#############################################################################################
# data normalization / cleaning
#############################################################################################

item191.dat1$Equip.Vintage <- as.numeric(as.character(item191.dat1$Component.1.Year.of.Manufacture)) #Warning OK here
item191.dat1$Equip.Vintage[which(item191.dat1$Component.1.Year.of.Manufacture < 1990)] <- NA
item191.dat1$Equip.Vintage[which(item191.dat1$Component.1.Year.of.Manufacture >= 1990 & item191.dat1$Component.1.Year.of.Manufacture < 2000)] <- "1990-1999"
item191.dat1$Equip.Vintage[which(item191.dat1$Component.1.Year.of.Manufacture >= 2000 & item191.dat1$Component.1.Year.of.Manufacture < 2006)] <- "2000-2005"
item191.dat1$Equip.Vintage[which(item191.dat1$Component.1.Year.of.Manufacture >= 2006)] <- "Post 2005"
unique(item191.dat1$Equip.Vintage)

#remove NAs from equipment vintage
item191.dat2 <- item191.dat1[which(!(is.na(item191.dat1$Equip.Vintage))),]
length(unique(item191.dat2$CK_Cadmus_ID)) #110 unique sites

#remove NAs from SEER
item191.dat3 <- item191.dat2[which(!(is.na(item191.dat2$SEER))),]
item191.dat4 <- item191.dat3[which(item191.dat3$SEER != "Could Not Collect"),]
item191.dat4$SEER <- as.numeric(as.character(item191.dat4$SEER))

str(item191.dat4)

#############################################################################################
# Analysis
#############################################################################################

#summarise by vintage
item191.vint <- summarise(group_by(item191.dat4, BuildingType, Equip.Vintage)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(SEER)
                           ,SE = sd(SEER) / sqrt(SampleSize))
#summarise across vintages
item191.all <- summarise(group_by(item191.dat4, BuildingType)
                           ,Equip.Vintage = "All Vintages"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(SEER)
                           ,SE = sd(SEER) / sqrt(SampleSize))

item191.merge <- rbind.data.frame(item191.vint, item191.all, stringsAsFactors = F)


item191.table <- data.frame("BuildingType" = item191.merge$BuildingType
                            ,"Equipment.Vintage" = item191.merge$Equip.Vintage
                            ,"Mean" = item191.merge$Mean
                            ,"SE" = item191.merge$SE
                            ,"SampleSize" = item191.merge$SampleSize)

#subset to relevant building types
item191.table1 <- item191.table[which(item191.table$BuildingType %in% c("Manufactured")),]
