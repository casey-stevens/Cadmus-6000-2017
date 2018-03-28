#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)


##  Create "Not In" operator
"%notin%" <- Negate("%in%")


# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))
rbsa.dat <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
appliances.dat <- data.frame(appliances.dat, stringsAsFactors = FALSE)                                                         
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))



#Read in data for analysis
mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

# #############################################################################################
# #Item 80: AVERAGE NUMBER OF APPLIANCES PER HOME BY TYPE (SF table 87, MH table 68)
# #############################################################################################
#   # For water Heaters
#   item80.mech <- mechanical.dat[grep("Water Heat", mechanical.dat$Generic),]
#   item80.mech$Generic[grep("Water Heat", item80.mech$Generic)] <- "Water Heater"
#   item80.mech$WaterHeaterCount <- 1
#   item80.mech1 <- left_join(rbsa.dat, item80.mech, by = "CK_Cadmus_ID")
#   item80.mech2 <- unique(item80.mech1[-grep("Multifamily", item80.mech1$BuildingType),])
#   which(duplicated(item80.mech2$CK_Cadmus_ID))
#   
#   item80.mech2$WaterHeaterCount[which(is.na(item80.mech2$WaterHeaterCount))] <- 0
#   item80.mech2$count <- 1
#   
#   #summarise by home
#   item80.site <- summarise(group_by(item80.mech2, CK_Cadmus_ID, Generic)
#                            ,Count = sum(WaterHeaterCount))
#   unique(item80.site$Count)
#   colnames(item80.site)[which(colnames(item80.site) == "Generic")] <- "Type"
# 
# #For everything else
#   item80.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
#                                                                ,"Type"
#                                                                ,"Large.Unusual.Load.Quantity"
#                                                                ,"Age"
#                                                                ,""
#                                                                ,""))]
#   item80.dat$count <- 1
#   
#   item80.dat0 <- item80.dat[which(item80.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
#   
#   item80.dat1 <- left_join(item80.dat0, rbsa.dat, by = "CK_Cadmus_ID")
#   item80.dat1$Large.Unusual.Load.Quantity[which(item80.dat1$Large.Unusual.Load.Quantity %in% c("N/A",NA))] <- 1
#   unique(item80.dat1$Large.Unusual.Load.Quantity)
#   item80.dat1$Large.Unusual.Load.Quantity <- as.numeric(as.character(item80.dat1$Large.Unusual.Load.Quantity))
#   
#   
#   item80.dat1$TotalQty <- item80.dat1$Large.Unusual.Load.Quantity * item80.dat1$count
#   
#   item80.sum <- summarise(group_by(item80.dat1, CK_Cadmus_ID, Type)
#                           ,Count = sum(TotalQty))
#   
#   
#   
#   
# # Row bind water heater and appliance counts
# item80.merge <- rbind.data.frame(item80.site, item80.sum)
#   
# item80.merge <- left_join(rbsa.dat, item80.merge) #switch RBSA.dat to rbsa.merge to get more info on washers/dryers
# 
# item80.merge <- item80.merge[which(!is.na(item80.merge$Type)),]
# item80.merge$Count[which(is.na(item80.merge$Count))] <- 0
# 
# item80.cast <- dcast(setDT(item80.merge)
#                      ,formula = CK_Cadmus_ID ~ Type
#                      ,value.var = c("Count"))
# 
# 
# 
# # item80.missing.washer <- item80.cast[which(is.na(item80.cast$Washer)),]
# # item80.missing.washer <- left_join(item80.missing.washer, rbsa.dat)
# # item80.washer.sf <- item80.missing.washer[which(item80.missing.washer$BuildingType == "Single Family"),]
# # 
# # item80.washer.sf.merge <- left_join(item80.washer.sf, sites.interview.dat1)
# 
# 
# item80.cast[is.na(item80.cast),] <- 0
# 
# item80.melt <- melt(item80.cast, id.vars = "CK_Cadmus_ID")
# names(item80.melt) <- c("CK_Cadmus_ID", "Type", "Count")
# 
# item80.merge <- left_join(rbsa.dat, item80.melt)
# item80.merge$Type <- as.character(item80.merge$Type)
# 
# unique(item80.merge$Type)
# item80.merge <- item80.merge[which(item80.merge$Type %in% c("Dishwasher"
#                                                             ,"Dryer"
#                                                             ,"Freezer"
#                                                             ,"Refrigerator"
#                                                             ,"Washer"
#                                                             ,"Water Heater")),]
# 
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item80.data <- weightedData(item80.merge[-which(colnames(item80.merge) %in% c("Count"
#                                                                               ,"Type"
#                                                                               ,"Age"))])
# item80.data <- left_join(item80.data, item80.merge[which(colnames(item80.merge) %in% c("CK_Cadmus_ID"
#                                                                                        ,"Count"
#                                                                                        ,"Type"
#                                                                                        ,"Age"))])
# item80.data$count <- 1
# 
# #######################
# # Weighted Analysis
# #######################
# item80.final <- mean_one_group(CustomerLevelData = item80.data
#                                ,valueVariable    = 'Count'
#                                ,byVariable       = 'Type'
#                                ,aggregateRow = "Total")
# item80.final <- item80.final[which(item80.final$Type != "Total"),]
# 
# 
# item80.final.SF <- item80.final[which(item80.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item80.final) %in% c("BuildingType"))]
# item80.final.MH <- item80.final[which(item80.final$BuildingType == "Manufactured")
#                                 ,-which(colnames(item80.final) %in% c("BuildingType"))]
# 
# exportTable(item80.final.SF, "SF", "Table 87", weighted = TRUE)
# # exportTable(item80.final.MH, "MH", "Table 68", weighted = TRUE)
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# item80.final <- mean_one_group_unweighted(CustomerLevelData = item80.data
#                                           ,valueVariable    = 'Count'
#                                           ,byVariable       = 'Type'
#                                           ,aggregateRow = "Total")
# item80.final <- item80.final[which(item80.final$Type != "Total"),]
# 
# item80.final <- item80.final[which(item80.final$Type %in% c("Dishwasher"
#                                                             ,"Dryer"
#                                                             ,"Freezer"
#                                                             ,"Refrigerator"
#                                                             ,"Washer"
#                                                             ,"Water Heater")),]
# 
# 
# item80.final.SF <- item80.final[which(item80.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item80.final) %in% c("BuildingType"))]
# item80.final.MH <- item80.final[which(item80.final$BuildingType == "Manufactured")
#                                 ,-which(colnames(item80.final) %in% c("BuildingType"))]
# 
# exportTable(item80.final.SF, "SF", "Table 87", weighted = FALSE)
# # exportTable(item80.final.MH, "MH", "Table 68", weighted = FALSE)



#############################################################################################
#Table AB: Average Age of Appliance Equipment by Type
#############################################################################################
tableAB.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"))]
tableAB.dat$count <- 1

tableAB.dat$Age <- as.numeric(as.character(tableAB.dat$Age))
tableAB.dat0 <- tableAB.dat[which(tableAB.dat$Age > 0),]

tableAB.merge <- left_join(rbsa.dat, tableAB.dat0, by = "CK_Cadmus_ID")
tableAB.merge <- tableAB.merge[grep("site", tableAB.merge$CK_Building_ID, ignore.case = T),]
tableAB.merge <- tableAB.merge[which(tableAB.merge$Age > 0),]

unique(tableAB.merge$Type)
tableAB.merge <- tableAB.merge[which(tableAB.merge$Type %in% c("Dishwasher"
                                                            ,"Dryer"
                                                            ,"Freezer"
                                                            ,"Refrigerator"
                                                            ,"Washer")),]

tableAB.merge <- tableAB.merge[which(tableAB.merge$Category == "PSE"),]
################################################
# Adding pop and sample sizes for weights
################################################
tableAB.data <- weightedData(tableAB.merge[-which(colnames(tableAB.merge) %in% c("count"
                                                                              ,"Type"
                                                                              ,"Age"
                                                                              ,"Category"))])
tableAB.data <- left_join(tableAB.data, tableAB.merge[which(colnames(tableAB.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Type"
                                                                                       ,"Age"
                                                                                       ,"Category"))])
tableAB.data$count <- 1

#######################
# Weighted Analysis
#######################
tableAB.final <- mean_one_group(CustomerLevelData = tableAB.data
                               ,valueVariable    = 'Age'
                               ,byVariable       = 'Type'
                               ,aggregateRow = "Total")
tableAB.final <- tableAB.final[which(tableAB.final$Type != "Total"),]
tableAB.final$Mean <- round(tableAB.final$Mean,0)

tableAB.final.MF <- tableAB.final[which(tableAB.final$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAB.final) %in% c("BuildingType"))]

exportTable(tableAB.final.MF, "MF", "Table AB", weighted = TRUE,OS = T, osIndicator = "PSE")


#######################
# Unweighted Analysis
#######################
tableAB.final <- mean_one_group_unweighted(CustomerLevelData = tableAB.data
                                          ,valueVariable    = 'Age'
                                          ,byVariable       = 'Type'
                                          ,aggregateRow = "Total")
tableAB.final <- tableAB.final[which(tableAB.final$Type != "Total"),]

tableAB.final.MF <- tableAB.final[which(tableAB.final$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAB.final) %in% c("BuildingType"))]

exportTable(tableAB.final.MF, "MF", "Table AB", weighted = FALSE,OS = T, osIndicator = "PSE")




#############################################################################################
#Table AC: Percent of Appliance Equipment above measure life by Type
#############################################################################################
# For water Heaters
tableAC.mech <- mechanical.dat[grep("Water Heat", mechanical.dat$Generic),]
tableAC.mech$Generic[grep("Water Heat", tableAC.mech$Generic)] <- "Water Heater"
tableAC.mech$WaterHeaterCount <- 1

tableAC.mech1 <- left_join(rbsa.dat, tableAC.mech, by = "CK_Cadmus_ID")
tableAC.mech2 <- unique(tableAC.mech1[-grep("Multifamily", tableAC.mech1$BuildingType),])
which(duplicated(tableAC.mech2$CK_Cadmus_ID))

tableAC.mech2$WaterHeaterCount[which(is.na(tableAC.mech2$WaterHeaterCount))] <- 0
tableAC.mech2$count <- 1

#summarise by home
tableAC.site <- summarise(group_by(tableAC.mech2, CK_Cadmus_ID, Generic, DHW.Year.Manufactured)
                         ,count = sum(WaterHeaterCount))
unique(tableAC.site$count)
colnames(tableAC.site)[which(colnames(tableAC.site) %in% c("Generic", "DHW.Year.Manufactured"))] <- c("Type","Age")
tableAC.site$Age <- as.numeric(as.character(tableAC.site$Age))
tableAC.site1 <- tableAC.site[which(!is.na(tableAC.site$Age)),]
tableAC.site2 <- tableAC.site1[which(tableAC.site1$Age > 0),]


tableAC.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,"Age"
                                                                    ,""
                                                                    ,""))]

tableAC.dat$count <- 1

tableAC.dat$Age <- as.numeric(as.character(tableAC.dat$Age))
tableAC.dat0 <- tableAC.dat[which(tableAC.dat$Age > 0),]

tableAC.merge0 <- rbind.data.frame(tableAC.site2, tableAC.dat0)
tableAC.merge <- left_join(rbsa.dat, tableAC.merge0, by = "CK_Cadmus_ID")
tableAC.merge <- tableAC.merge[grep("site",tableAC.merge$CK_Building_ID, ignore.case = T),]
tableAC.merge <- tableAC.merge[which(tableAC.merge$Age > 0),]

unique(tableAC.merge$Type)
tableAC.merge <- tableAC.merge[which(tableAC.merge$Type %in% c("Dishwasher"
                                                               ,"Dryer"
                                                               ,"Freezer"
                                                               ,"Refrigerator"
                                                               ,"Washer"
                                                               ,"Water Heater")),]

tableAC.merge$MeasureMap <- 0
tableAC.merge$MeasureMap[which(tableAC.merge$Type == "Refrigerator")] <- 15
tableAC.merge$MeasureMap[which(tableAC.merge$Type == "Freezer")] <- 22
tableAC.merge$MeasureMap[which(tableAC.merge$Type == "Washer")] <- 14
tableAC.merge$MeasureMap[which(tableAC.merge$Type == "Dryer")] <- 12
tableAC.merge$MeasureMap[which(tableAC.merge$Type == "Dishwasher")] <- 12
tableAC.merge$MeasureMap[which(tableAC.merge$Type == "Water Heater")] <- 15

tableAC.merge$Age.Diff <- 2017 - tableAC.merge$Age

tableAC.merge$Above.Measure.Life <- "No"
tableAC.merge$Above.Measure.Life[which(tableAC.merge$Age.Diff > tableAC.merge$MeasureMap)] <- "Yes"

tableAC.merge$Ind <- 0
tableAC.merge$Ind[which(tableAC.merge$Age.Diff > tableAC.merge$MeasureMap)] <- 1
tableAC.merge <- tableAC.merge[which(tableAC.merge$Category == "PSE")]
################################################
# Adding pop and sample sizes for weights
################################################
tableAC.data <- weightedData(tableAC.merge[-which(colnames(tableAC.merge) %in% c("Type"
                                                                                 ,"Age"
                                                                                 ,"count"
                                                                                 ,"MeasureMap"
                                                                                 ,"Above.Measure.Life"
                                                                                 ,"Age.Diff"
                                                                                 ,"Ind"
                                                                                 ,"Category"))])
tableAC.data <- left_join(tableAC.data, tableAC.merge[which(colnames(tableAC.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Type"
                                                                                           ,"Age"
                                                                                           ,"count"
                                                                                           ,"MeasureMap"
                                                                                           ,"Above.Measure.Life"
                                                                                           ,"Age.Diff"
                                                                                           ,"Ind"
                                                                                           ,"Category"))])
tableAC.data$count <- 1
tableAC.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableAC.final <- proportions_one_group(CustomerLevelData = tableAC.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "Type"
                                       ,total.name = "Total")
tableAC.final <- tableAC.final[which(tableAC.final$Type != "Total"),]

tableAC.final.MF <- tableAC.final[which(tableAC.final$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAC.final) %in% c("BuildingType"))]

exportTable(tableAC.final.MF, "MF", "Table AC", weighted = TRUE,OS = T, osIndicator = "PSE")


#######################
# Unweighted Analysis
#######################
tableAC.final <- proportions_one_group(CustomerLevelData = tableAC.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "Type"
                                       ,total.name = "Total"
                                       ,weighted = FALSE)
tableAC.final <- tableAC.final[which(tableAC.final$Type != "Total"),]

tableAC.final.MF <- tableAC.final[which(tableAC.final$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAC.final) %in% c("BuildingType"))]

exportTable(tableAC.final.MF, "MF", "Table AC", weighted = FALSE,OS = T, osIndicator = "PSE")






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
item81.dat1 <- item81.dat1[grep("site",item81.dat1$CK_Building_ID, ignore.case = T),]
item81.dat2 <- item81.dat1[which(item81.dat1$Type %in% c("Refrigerator", "Freezer")),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item81.dat2$EquipVintage_bins <- as.numeric(as.character(item81.dat2$Age))
item81.dat3 <- item81.dat2[which(!(is.na(item81.dat2$EquipVintage_bins))),]

item81.dat3$EquipVintage_bins[which(item81.dat3$Age < 1980)] <- "Pre 1980"
item81.dat3$EquipVintage_bins[which(item81.dat3$Age >= 1980 & item81.dat3$Age < 1990)] <- "1980-1989"
item81.dat3$EquipVintage_bins[which(item81.dat3$Age >= 1990 & item81.dat3$Age < 1995)] <- "1990-1994"
item81.dat3$EquipVintage_bins[which(item81.dat3$Age >= 1995 & item81.dat3$Age < 2000)] <- "1995-1999"
item81.dat3$EquipVintage_bins[which(item81.dat3$Age >= 2000 & item81.dat3$Age < 2005)] <- "2000-2004"
item81.dat3$EquipVintage_bins[which(item81.dat3$Age >= 2005 & item81.dat3$Age < 2010)] <- "2005-2009"
item81.dat3$EquipVintage_bins[which(item81.dat3$Age >= 2010 & item81.dat3$Age < 2015)] <- "2010-2014"
item81.dat3$EquipVintage_bins[which(item81.dat3$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item81.dat3$EquipVintage_bins)


item81.merge <- left_join(rbsa.dat, item81.dat3)
item81.merge <- item81.merge[which(!is.na(item81.merge$EquipVintage_bins)),]

################################################
# Adding pop and sample sizes for weights
################################################
item81.data <- weightedData(item81.merge[-which(colnames(item81.merge) %in% c("count"
                                                                          ,"Type"
                                                                          ,"Age"
                                                                          ,"EquipVintage_bins"
                                                                          ,"Category"))])
item81.data <- left_join(item81.data, item81.merge[which(colnames(item81.merge) %in% c("CK_Cadmus_ID"
                                                                                   ,"count"
                                                                                   ,"Type"
                                                                                   ,"Age"
                                                                                   ,"EquipVintage_bins"
                                                                                   ,"Category"))])
item81.data$count <- 1

#######################
# Weighted Analysis
#######################
item81.summary <- proportionRowsAndColumns1(CustomerLevelData = item81.data
                                            ,valueVariable = "count"
                                            ,columnVariable = "Category"
                                            ,rowVariable = "EquipVintage_bins"
                                            ,aggregateColumnName = "Remove")
item81.summary <- item81.summary[which(item81.summary$Category != "Remove"),]

item81.cast <- dcast(setDT(item81.summary)
                     ,formula = BuildingType + EquipVintage_bins ~ Category
                     ,value.var = c("w.percent","w.SE","n","EB"))

item81.final <- data.frame("BuildingType" = item81.cast$BuildingType
                           ,"EquipVintage_bins" = item81.cast$EquipVintage_bins
                           ,"PSE.Percent"                 = item81.cast$w.percent_PSE
                           ,"PSE.SE"                      = item81.cast$w.SE_PSE
                           ,"PSE.n"                       = item81.cast$n_PSE
                           ,"PSE.King.County.Percent"     = item81.cast$`w.percent_PSE KING COUNTY`
                           ,"PSE.King.County.SE"          = item81.cast$`w.SE_PSE KING COUNTY`
                           ,"PSE.King.County.n"           = item81.cast$`n_PSE KING COUNTY`
                           ,"PSE.Non.King.County.Percent" = item81.cast$`w.percent_PSE NON-KING COUNTY`
                           ,"PSE.Non.King.County.SE"      = item81.cast$`w.SE_PSE NON-KING COUNTY`
                           ,"PSE.Non.King.County.n"       = item81.cast$`n_PSE NON-KING COUNTY`
                           ,"2017.RBSA.PS.Percent"        = item81.cast$`w.percent_2017 RBSA PS`
                           ,"2017.RBSA.PS.SE"             = item81.cast$`w.SE_2017 RBSA PS`
                           ,"2017.RBSA.PS.n"              = item81.cast$`n_2017 RBSA PS`
                           ,"PSE_EB"                      = item81.cast$EB_PSE
                           ,"PSE.King.County_EB"          = item81.cast$`EB_PSE KING COUNTY`
                           ,"PSE.Non.King.County_EB"      = item81.cast$`EB_PSE NON-KING COUNTY`
                           ,"2017.RBSA.PS_EB"             = item81.cast$`EB_2017 RBSA PS`)

unique(item81.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item81.final <- item81.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item81.final <- data.frame(item81.final)

item81.final.MF <- item81.final[which(item81.final$BuildingType == "Multifamily")
                                ,-which(colnames(item81.final) %in% c("BuildingType"))]

exportTable(item81.final.MF, "MF", "Table 87", weighted = TRUE,OS = T, osIndicator = "PSE")


#######################
# Unweighted Analysis
#######################
item81.summary <- proportions_two_groups_unweighted(CustomerLevelData = item81.data
                                            ,valueVariable = "count"
                                            ,columnVariable = "Category"
                                            ,rowVariable = "EquipVintage_bins"
                                            ,aggregateColumnName = "Remove")
item81.summary <- item81.summary[which(item81.summary$Category != "Remove"),]

item81.cast <- dcast(setDT(item81.summary)
                     ,formula = BuildingType + EquipVintage_bins ~ Category
                     ,value.var = c("Percent","SE","n"))

item81.final <- data.frame("BuildingType" = item81.cast$BuildingType
                           ,"EquipVintage_bins" = item81.cast$EquipVintage_bins
                           ,"PSE.Percent"                 = item81.cast$Percent_PSE
                           ,"PSE.SE"                      = item81.cast$SE_PSE
                           ,"PSE.n"                       = item81.cast$n_PSE
                           ,"PSE.King.County.Percent"     = item81.cast$`Percent_PSE KING COUNTY`
                           ,"PSE.King.County.SE"          = item81.cast$`SE_PSE KING COUNTY`
                           ,"PSE.King.County.n"           = item81.cast$`n_PSE KING COUNTY`
                           ,"PSE.Non.King.County.Percent" = item81.cast$`Percent_PSE NON-KING COUNTY`
                           ,"PSE.Non.King.County.SE"      = item81.cast$`SE_PSE NON-KING COUNTY`
                           ,"PSE.Non.King.County.n"       = item81.cast$`n_PSE NON-KING COUNTY`
                           ,"2017.RBSA.PS.Percent"        = item81.cast$`Percent_2017 RBSA PS`
                           ,"2017.RBSA.PS.SE"             = item81.cast$`SE_2017 RBSA PS`
                           ,"2017.RBSA.PS.n"              = item81.cast$`n_2017 RBSA PS`)
unique(item81.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item81.final <- item81.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item81.final <- data.frame(item81.final)

item81.final.MF <- item81.final[which(item81.final$BuildingType == "Multifamily")
                                ,-which(colnames(item81.final) %in% c("BuildingType"))]

exportTable(item81.final.MF, "MF", "Table 87", weighted = FALSE,OS = T, osIndicator = "PSE")


