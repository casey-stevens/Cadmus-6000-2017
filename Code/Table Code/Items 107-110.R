#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

##  Create "Not In" operator
"%notin%" <- Negate("%in%")

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) 

#Read in data for analysis
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))



#############################################################################################
#Item 107: AVERAGE NUMBER OF TELEVISIONS PER HOME BY STATE (SF table 114)
#############################################################################################
#subset to columns needed for analysis
item107.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]
item107.dat$count <- 1

item107.dat0 <- item107.dat[which(item107.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]


item107.dat1 <- item107.dat0[which(item107.dat0$Type == "Television"),]
item107.merge <- left_join(item107.dat1, rbsa.dat)

item107.customer <- summarise(group_by(item107.merge, CK_Cadmus_ID, BuildingType, State)
                              ,Site.Count = sum(count))

item107.customer <- left_join(rbsa.dat, item107.customer)
item107.customer <- item107.customer[which(!is.na(item107.customer$Site.Count)),]

item107.data <- weightedData(item107.customer[-which(colnames(item107.customer) %in% c("Site.Count"))])
item107.data <- left_join(item107.data, item107.customer[which(colnames(item107.customer) %in% c("CK_Cadmus_ID"
                                                                                                 ,"Site.Count"))])


item107.final <- mean_one_group(item107.data
                                ,valueVariable = 'Site.Count'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region'
                                ,weighted = TRUE)
item107.final.SF <- item107.final[which(item107.final$BuildingType == "Single Family")
                                  ,-which(colnames(item107.final) %in% c("BuildingType"))]
exportTable(item107.final.SF, "SF", "Table 114", weighted = TRUE)


item107.final.MH <- item107.final[which(item107.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item107.final) %in% c("BuildingType"))]
exportTable(item107.final.MH, "MH", "Table 89", weighted = TRUE)




item107.final <- mean_one_group(item107.data
                                ,valueVariable = 'Site.Count'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region'
                                ,weighted = FALSE)
item107.final.SF <- item107.final[which(item107.final$BuildingType == "Single Family")
                                  ,-which(colnames(item107.final) %in% c("BuildingType"))]
exportTable(item107.final.SF, "SF", "Table 114", weighted = FALSE)


item107.final.MH <- item107.final[which(item107.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item107.final) %in% c("BuildingType"))]
exportTable(item107.final.MH, "MH", "Table 89", weighted = FALSE)



# ##Summarise by state
# #summarise by site
# item107.state1 <- summarise(group_by(item107.dat2, CK_Cadmus_ID, BuildingType, State)
#                           ,Site.Count = sum(count))
# 
# #summarise across sites
# item107.state <- summarise(group_by(item107.dat3, BuildingType, State)
#                           ,SampleSize = length(unique(CK_Cadmus_ID))
#                           ,Mean = mean(Site.Count)
#                           ,SE = sd(Site.Count) / sqrt(SampleSize))
# 
# 
# ##Summarise across states
# #summarise by site
# item107.region1 <- summarise(group_by(item107.dat2, CK_Cadmus_ID, BuildingType)
#                           ,State = "Region"
#                           ,Site.Count = sum(count))
# 
# #summarise across sites
# item107.region <- summarise(group_by(item107.dat3, BuildingType)
#                           ,State = "Region"
#                           ,SampleSize = length(unique(CK_Cadmus_ID))
#                           ,Mean = mean(Site.Count)
#                           ,SE = sd(Site.Count) / sqrt(SampleSize))
# 
# item107.final <- rbind.data.frame(item107.state, item107.region, stringsAsFactors = F)
# item107.table <- item107.final[which(item107.final$BuildingType %in% c("Single Family", "Manufactured")),]








#############################################################################################
#Item 108: AVERAGE TELEVISION POWER BY VINTAGE (SF table 115, MH table 90)
#############################################################################################
#subset to columns needed for analysis
item108.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,"TV.Wattage"
                                                                    ,"Age"))]
item108.dat$count <- 1

item108.dat0 <- item108.dat[which(item108.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item108.dat1 <- left_join(item108.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item108.dat2 <- item108.dat1[which(item108.dat1$Type == "Television"),]

#clean wattage
unique(item108.dat2$TV.Wattage)
item108.dat2$TV.Wattage <- as.numeric(as.character(item108.dat2$TV.Wattage))
item108.dat3            <- item108.dat2[which(!(is.na(item108.dat2$TV.Wattage))),]

#clean age
unique(item108.dat3$Age)
item108.dat3$Age <- as.numeric(as.character(item108.dat3$Age))
item108.dat4     <- item108.dat3[which(!(is.na(item108.dat3$Age))),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item108.dat4$EquipVintage_bins <- as.numeric(as.character(item108.dat4$Age))

item108.dat4$EquipVintage_bins[which(item108.dat4$Age <  1990)] <- "Pre 1990"
item108.dat4$EquipVintage_bins[which(item108.dat4$Age >= 1990 & item108.dat4$Age < 1995)] <- "1990-1994"
item108.dat4$EquipVintage_bins[which(item108.dat4$Age >= 1995 & item108.dat4$Age < 2000)] <- "1995-1999"
item108.dat4$EquipVintage_bins[which(item108.dat4$Age >= 2000 & item108.dat4$Age < 2005)] <- "2000-2004"
item108.dat4$EquipVintage_bins[which(item108.dat4$Age >= 2005 & item108.dat4$Age <= 2009)] <- "2005-2009"
item108.dat4$EquipVintage_bins[which(item108.dat4$Age >  2009)] <- "Post 2010"
#check uniques
unique(item108.dat4$EquipVintage_bins)


item108.customer <- left_join(item108.dat4, rbsa.dat)


###########################################
# add pop and sample sizes for weighting
###########################################
item108.data <- weightedData(item108.customer[-which(colnames(item108.customer) %in% c("Type"
                                                                                       ,"Age"
                                                                                       ,"TV.Wattage"
                                                                                       ,"count"
                                                                                       ,"EquipVintage_bins"))])
item108.data <- left_join(item108.data, item108.customer[which(colnames(item108.customer) %in% c("CK_Cadmus_ID"
                                                                                                 ,"Type"
                                                                                                 ,"Age"
                                                                                                 ,"TV.Wattage"
                                                                                                 ,"count"
                                                                                                 ,"EquipVintage_bins"))])


#####################
# Weighted analysis
#####################
item108.final    <- mean_one_group(item108.data
                                   ,valueVariable = 'TV.Wattage'
                                   ,byVariable    = 'EquipVintage_bins'
                                   ,aggregateRow  = "All Vintages"
                                   ,weighted = TRUE)
item108.final.SF <- item108.final[which(item108.final$BuildingType == "Single Family")
                                  ,-which(colnames(item108.final) == "BuildingType")]
exportTable(item108.final.SF, "SF", "Table 115", weighted = TRUE)

item108.final.MH <- item108.final[which(item108.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item108.final) == "BuildingType")]
exportTable(item108.final.MH, "MH", "Table 90", weighted = TRUE)


##############
# Unweighted
##############
item108.final    <- mean_one_group(item108.data
                                   ,valueVariable = 'TV.Wattage'
                                   ,byVariable    = 'EquipVintage_bins'
                                   ,aggregateRow  = "All Vintages"
                                   ,weighted = FALSE)
item108.final.SF <- item108.final[which(item108.final$BuildingType == "Single Family")
                                  ,-which(colnames(item108.final) == "BuildingType")]
exportTable(item108.final.SF, "SF", "Table 115", weighted = FALSE)

item108.final.MH <- item108.final[which(item108.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item108.final) == "BuildingType")]
exportTable(item108.final.MH, "MH", "Table 90", weighted = FALSE)


# #summarise at the site level
# item108.dat5 <- summarise(group_by(item108.dat4,CK_Cadmus_ID, BuildingType, EquipVintage_bins)
#                           ,Site.Mean = mean(TV.Wattage))
# 
# #summarise across sites
# #by vintages
# item108.sum1 <- summarise(group_by(item108.dat4, BuildingType, EquipVintage_bins)
#                           ,SampleSize = length(unique(CK_Cadmus_ID))
#                           ,Mean = mean(TV.Wattage)
#                           ,SE = sd(TV.Wattage) / sqrt(SampleSize))
# #across vintages
# item108.sum2 <- summarise(group_by(item108.dat4, BuildingType)
#                           ,EquipVintage_bins = "All Vintages"
#                           ,SampleSize = length(unique(CK_Cadmus_ID))
#                           ,Mean = mean(TV.Wattage)
#                           ,SE = sd(TV.Wattage) / sqrt(SampleSize))
# 
# 
# #row bind
# item108.final <- rbind.data.frame(item108.sum1, item108.sum2, stringsAsFactors = F)
# 
# item108.table <- item108.final[which(item108.final$BuildingType %in% c("Single Family","Manufactured")),]









#############################################################################################
#Item 109: DISTRIBUTION OF TELEVISION SCREENS BY TYPE AND VINTAGE (SF table 116, MH table 91)
#############################################################################################
#subset to columns needed for analysis
item109.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,"TV.Screen.Type"
                                                                    ,"Age"))]
item109.dat$count <- 1

item109.dat0 <- item109.dat[which(item109.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item109.dat1 <- left_join(item109.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item109.dat2 <- item109.dat1[which(item109.dat1$Type == "Television"),]

#clean screen type
unique(item109.dat2$TV.Screen.Type)
item109.dat3 <- item109.dat2[which(!(is.na(item109.dat2$TV.Screen.Type))),]
item109.dat4 <- item109.dat3[which(!(item109.dat3$TV.Screen.Type %in% c("Unknown"))),]

item109.dat4$TV.Screen.Type[grep("Tube",item109.dat4$TV.Screen.Type)] <- "CRT"
item109.dat4$TV.Screen.Type[which(item109.dat4$TV.Screen.Type != "CRT")] <- "Other"
unique(item109.dat4$TV.Screen.Type)


#clean age
unique(item109.dat4$Age)
item109.dat4$Age <- as.numeric(as.character(item109.dat4$Age))
item109.dat5     <- item109.dat4[which(!(is.na(item109.dat4$Age))),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item109.dat5$EquipVintage_bins <- as.numeric(as.character(item109.dat5$Age))

item109.dat5$EquipVintage_bins[which(item109.dat5$Age < 1990)] <- "Pre 1990"
item109.dat5$EquipVintage_bins[which(item109.dat5$Age >= 1990 & item109.dat5$Age < 1995)] <- "1990-1994"
item109.dat5$EquipVintage_bins[which(item109.dat5$Age >= 1995 & item109.dat5$Age < 2000)] <- "1995-1999"
item109.dat5$EquipVintage_bins[which(item109.dat5$Age >= 2000 & item109.dat5$Age < 2005)] <- "2000-2004"
item109.dat5$EquipVintage_bins[which(item109.dat5$Age >= 2005 & item109.dat5$Age <= 2009)] <- "2005-2009"
item109.dat5$EquipVintage_bins[which(item109.dat5$Age > 2009)] <- "Post 2010"
#check uniques
unique(item109.dat5$EquipVintage_bins)

item109.customer <- left_join(item109.dat5, rbsa.dat)


###########################################
# add pop and sample sizes for weighting
###########################################
item109.data <- weightedData(item109.customer[-which(colnames(item109.customer) %in% c("Type"
                                                                                       ,"Age"
                                                                                       ,"TV.Screen.Type"
                                                                                       ,"count"
                                                                                       ,"EquipVintage_bins"))])
item109.data <- left_join(item109.data, item109.customer[which(colnames(item109.customer) %in% c("CK_Cadmus_ID"
                                                                                                 ,"Type"
                                                                                                 ,"Age"
                                                                                                 ,"TV.Screen.Type"
                                                                                                 ,"count"
                                                                                                 ,"EquipVintage_bins"))])


#####################
# Weighted analysis
#####################
item109.final    <- proportionRowsAndColumns1(item109.data
                                              ,valueVariable       = 'count'
                                              ,columnVariable      = "EquipVintage_bins"
                                              ,rowVariable         = "TV.Screen.Type"
                                              ,aggregateColumnName = "All Vintages"
                                              # ,weighted            = TRUE
)
item109.cast <- dcast(setDT(item109.final)
                      ,BuildingType + EquipVintage_bins ~ TV.Screen.Type
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N"))


item109.final.SF <- item109.cast[which(item109.cast$BuildingType == "Single Family"),]
exportTable(item109.final.SF, "SF", "Table 116", weighted = TRUE)

item109.final.MH <- item109.final[which(item109.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item109.final) == "BuildingType")]
exportTable(item109.final.MH, "MH", "Table 91", weighted = TRUE)


##############
# Unweighted
##############
item109.final    <- proportionRowsAndColumns1(item109.data
                                              ,valueVariable       = 'count'
                                              ,columnVariable      = "EquipVintage_bins"
                                              ,rowVariable         = "TV.Screen.Type"
                                              ,aggregateColumnName = "All Vintages"
                                              # ,weighted            = FALSE
                                              )

item109.cast <- dcast(setDT(item109.final)
                      ,BuildingType + EquipVintage_bins ~ TV.Screen.Type
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N"))


item109.final.SF <- item109.cast[which(item109.cast$BuildingType == "Single Family"),]
exportTable(item109.final.SF, "SF", "Table 116", weighted = FALSE)

item109.final.MH <- item109.final[which(item109.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item109.final) == "BuildingType")]
exportTable(item109.final.MH, "MH", "Table 91", weighted = FALSE)






# #summarise by TV Type
# #by vintage
# item109.sum1 <- summarise(group_by(item109.dat5, BuildingType, TV.Screen.Type, EquipVintage_bins)
#                           ,Count = sum(count))
# #across vintages
# item109.sum2 <- summarise(group_by(item109.dat5, BuildingType, TV.Screen.Type)
#                           ,EquipVintage_bins = "All Vintages"
#                           ,Count = sum(count))
# 
# item109.tot.count1 <- summarise(group_by(item109.dat5, BuildingType, EquipVintage_bins)
#                                 ,SampleSize = length(unique(CK_Cadmus_ID))
#                                ,Count = sum(count))
# item109.tot.count2 <- summarise(group_by(item109.dat5, BuildingType)
#                                 ,EquipVintage_bins = "All Vintages"
#                                 ,SampleSize = length(unique(CK_Cadmus_ID))
#                                 ,Count = sum(count))
# item109.tot.count <- rbind.data.frame(item109.tot.count1, item109.tot.count2, stringsAsFactors = F)
# 
# 
# item109.merge <- rbind.data.frame(item109.sum1, item109.sum2, stringsAsFactors = F)
# item109.join <- left_join(item109.merge, item109.tot.count, by = c("BuildingType", "EquipVintage_bins"))
# colnames(item109.join) <- c("BuildingType"
#                             ,"TV.Screen.Type"
#                             ,"Equipment.Vintages"
#                             ,"Count"
#                             ,"SampleSize"
#                             ,"Total.Count")
# item109.join$Percent <- item109.join$Count / item109.join$Total.Count
# item109.join$SE <- sqrt(item109.join$Percent * (1 - item109.join$Percent) / item109.join$SampleSize)
# 
# library(data.table)
# item109.cast <- dcast(setDT(item109.join)
#                       ,formula = BuildingType + Equipment.Vintages + SampleSize ~ TV.Screen.Type
#                       ,value.var = c("Percent", "SE"))
# 
# 
# 
# item109.final <- data.frame("BuildingType" = item109.cast$BuildingType
#                             ,"Equipment.Vintages" = item109.cast$Equipment.Vintages
#                             ,"Percent_CRT" = item109.cast$Percent_CRT
#                             ,"SE_CRT" = item109.cast$SE_CRT
#                             ,"Percent_Other" = item109.cast$Percent_Other
#                             ,"SE_Other" = item109.cast$SE_Other
#                             ,"SampleSize" = item109.cast$SampleSize)
# 
# 
# item109.table <- item109.final[which(item109.final$BuildingType %in% c("Single Family", "Manufactured")),]










#############################################################################################
#Item 110: DISTRIBUTION OF TELEVISIONS BY ROOM TYPE (SF table 117, MH table 92)
#############################################################################################
#subset to columns needed for analysis
item110.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,"Clean.Room"))]
item110.dat$count <- 1

item110.dat0 <- item110.dat[which(item110.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item110.dat1 <- left_join(item110.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#subset to only television
item110.dat2 <- item110.dat1[which(item110.dat1$Type == "Television"),]
#remove any missing room types
item110.dat3 <- item110.dat2[which(!(is.na(item110.dat2$Clean.Room))),]


item110.customer <- left_join(item110.dat3, rbsa.dat)


###########################################
# add pop and sample sizes for weighting
###########################################
item110.data <- weightedData(item110.customer[-which(colnames(item110.customer) %in% c("Type"
                                                                                       ,"Clean.Room"
                                                                                       ,"count"))])
item110.data <- left_join(item110.data, item110.customer[which(colnames(item110.customer) %in% c("CK_Cadmus_ID"
                                                                                                 ,"Type"
                                                                                                 ,"Clean.Room"
                                                                                                 ,"count"))])


#####################
# Weighted analysis
#####################
item110.final    <- proportions_one_group(item110.data
                                   ,valueVariable    = 'count'
                                   ,groupingVariable = 'Clean.Room'
                                   ,total.name       = "Total"
                                   ,columnName       = "Room"
                                   ,weighted         = TRUE)
item110.final.SF <- item110.final[which(item110.final$BuildingType == "Single Family")
                                  ,-which(colnames(item110.final) == "BuildingType")]
exportTable(item110.final.SF, "SF", "Table 117", weighted = TRUE)

item110.final.MH <- item110.final[which(item110.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item110.final) == "BuildingType")]
exportTable(item110.final.MH, "MH", "Table 92", weighted = TRUE)


##############
# Unweighted
##############
item110.final    <- proportions_one_group(item110.data
                                          ,valueVariable    = 'count'
                                          ,groupingVariable = 'Clean.Room'
                                          ,total.name       = "Total"
                                          ,columnName       = "Room"
                                          ,weighted         = FALSE)
item110.final.SF <- item110.final[which(item110.final$BuildingType == "Single Family")
                                  ,-which(colnames(item110.final) == "BuildingType")]
exportTable(item110.final.SF, "SF", "Table 115", weighted = FALSE)

item110.final.MH <- item110.final[which(item110.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item110.final) == "BuildingType")]
exportTable(item110.final.MH, "MH", "Table 90", weighted = FALSE)






# #summarise by clean room
# item110.sum1 <- summarise(group_by(item110.dat3, BuildingType, Clean.Room)
#                           ,SampleSize = length(unique(CK_Cadmus_ID))
#                           ,Count = sum(count))
# #summarise across clean room
# item110.sum2 <- summarise(group_by(item110.dat3, BuildingType)
#                           ,Clean.Room = "All Room Types"
#                           ,SampleSize = length(unique(CK_Cadmus_ID))
#                           ,Count = sum(count))
# 
# item110.tot.count <- item110.sum2[which(colnames(item110.sum2) %in% c("BuildingType","Count"))]
# 
# #rbind
# item110.merge1 <- rbind.data.frame(item110.sum1, item110.sum2, stringsAsFactors = F)
# #leftjoin
# item110.final <- left_join(item110.merge1, item110.tot.count, by = "BuildingType")
# colnames(item110.final) <- c("BuildingType"
#                              ,"Room.Type"
#                              ,"SampleSize"
#                              ,"Count"
#                              ,"Total.Count")
# item110.final$Percent <- item110.final$Count / item110.final$Total.Count
# item110.final$SE <- sqrt(item110.final$Percent * (1 - item110.final$Percent) / item110.final$SampleSize)
# 
# 
# item110.table <- data.frame("BuildingType" = item110.final$BuildingType
#                             ,"Room.Type" = item110.final$Room.Type
#                             ,"Percent" = item110.final$Percent
#                             ,"SE" = item110.final$SE
#                             ,"SampleSize" = item110.final$SampleSize
#                             ,stringsAsFactors = F)
# item110.table1 <- item110.table[which(item110.table$BuildingType %in% c("Single Family", "Manufactured")),]
