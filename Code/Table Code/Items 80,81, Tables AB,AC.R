#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list=ls())
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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))
rbsa.dat <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
appliances.dat <- data.frame(read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
                             ,stringsAsFactors = FALSE)
                                                         
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))



#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))


# sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
# sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))
# 
# 
# sites.interview.dat1 <- sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID", "INTRVW_CUST_RES_HomeandEnergyUseHome_ClothesWasherLoadsPerWeek"))]
# sites.interview.dat1 <- sites.interview.dat1[which(!is.na(sites.interview.dat1$INTRVW_CUST_RES_HomeandEnergyUseHome_ClothesWasherLoadsPerWeek)),]
# 
# rbsa.dat.sf <- rbsa.dat[which(rbsa.dat$BuildingType == "Single Family"),]
# 
# rbsa.merge <- left_join(rbsa.dat.sf, sites.interview.dat1)
# rbsa.merge <- rbsa.merge[which(!is.na(rbsa.merge$INTRVW_CUST_RES_HomeandEnergyUseHome_ClothesWasherLoadsPerWeek)),]

#############################################################################################
#Item 80: AVERAGE NUMBER OF APPLIANCES PER HOME BY TYPE (SF table 87, MH table 68)
#############################################################################################
  # For water Heaters
  item80.mech <- mechanical.dat[grep("Water Heat", mechanical.dat$Generic),]
  item80.mech$Generic[grep("Water Heat", item80.mech$Generic)] <- "Water Heater"
  item80.mech$WaterHeaterCount <- 1
  item80.mech1 <- left_join(rbsa.dat, item80.mech, by = "CK_Cadmus_ID")
  item80.mech2 <- unique(item80.mech1[-grep("Multifamily", item80.mech1$BuildingType),])
  which(duplicated(item80.mech2$CK_Cadmus_ID))
  
  item80.mech2$WaterHeaterCount[which(is.na(item80.mech2$WaterHeaterCount))] <- 0
  item80.mech2$count <- 1
  
  #summarise by home
  item80.site <- summarise(group_by(item80.mech2, CK_Cadmus_ID, Generic)
                           ,Count = sum(WaterHeaterCount))
  unique(item80.site$Count)
  colnames(item80.site)[which(colnames(item80.site) == "Generic")] <- "Type"

#For everything else
  item80.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Type"
                                                               ,"Large.Unusual.Load.Quantity"
                                                               ,"Age"
                                                               ,""
                                                               ,""))]
  item80.dat$count <- 1
  
  item80.dat0 <- item80.dat[which(item80.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
  
  item80.dat1 <- left_join(item80.dat0, rbsa.dat, by = "CK_Cadmus_ID")
  item80.dat1$Large.Unusual.Load.Quantity[which(item80.dat1$Large.Unusual.Load.Quantity %in% c("N/A",NA))] <- 1
  unique(item80.dat1$Large.Unusual.Load.Quantity)
  item80.dat1$Large.Unusual.Load.Quantity <- as.numeric(as.character(item80.dat1$Large.Unusual.Load.Quantity))
  
  
  item80.dat1$TotalQty <- item80.dat1$Large.Unusual.Load.Quantity * item80.dat1$count
  
  item80.sum <- summarise(group_by(item80.dat1, CK_Cadmus_ID, Type)
                          ,Count = sum(TotalQty))
  
  
  
  
# Row bind water heater and appliance counts
item80.merge <- rbind.data.frame(item80.site, item80.sum)
  
item80.merge <- left_join(rbsa.dat, item80.merge) #switch RBSA.dat to rbsa.merge to get more info on washers/dryers

item80.merge <- item80.merge[which(!is.na(item80.merge$Type)),]
item80.merge$Count[which(is.na(item80.merge$Count))] <- 0

item80.cast <- dcast(setDT(item80.merge)
                     ,formula = CK_Cadmus_ID ~ Type
                     ,value.var = c("Count"))



# item80.missing.washer <- item80.cast[which(is.na(item80.cast$Washer)),]
# item80.missing.washer <- left_join(item80.missing.washer, rbsa.dat)
# item80.washer.sf <- item80.missing.washer[which(item80.missing.washer$BuildingType == "Single Family"),]
# 
# item80.washer.sf.merge <- left_join(item80.washer.sf, sites.interview.dat1)


item80.cast[is.na(item80.cast),] <- 0

item80.melt <- melt(item80.cast, id.vars = "CK_Cadmus_ID")
names(item80.melt) <- c("CK_Cadmus_ID", "Type", "Count")

item80.merge <- left_join(rbsa.dat, item80.melt)
item80.merge$Type <- as.character(item80.merge$Type)

unique(item80.merge$Type)
item80.merge <- item80.merge[which(item80.merge$Type %in% c("Dishwasher"
                                                            ,"Dryer"
                                                            ,"Freezer"
                                                            ,"Refrigerator"
                                                            ,"Washer"
                                                            ,"Water Heater")),]


################################################
# Adding pop and sample sizes for weights
################################################
item80.data <- weightedData(item80.merge[-which(colnames(item80.merge) %in% c("Count"
                                                                              ,"Type"
                                                                              ,"Age"))])
item80.data <- left_join(item80.data, item80.merge[which(colnames(item80.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Count"
                                                                                       ,"Type"
                                                                                       ,"Age"))])
item80.data$count <- 1

#######################
# Weighted Analysis
#######################
item80.final <- mean_one_group(CustomerLevelData = item80.data
                               ,valueVariable    = 'Count'
                               ,byVariable       = 'Type'
                               ,aggregateRow = "Total")
item80.final <- item80.final[which(item80.final$Type != "Total"),]


item80.final.SF <- item80.final[which(item80.final$BuildingType == "Single Family")
                                ,-which(colnames(item80.final) %in% c("BuildingType"))]
item80.final.MH <- item80.final[which(item80.final$BuildingType == "Manufactured")
                                ,-which(colnames(item80.final) %in% c("BuildingType"))]

# exportTable(item80.final.SF, "SF", "Table 87", weighted = TRUE)
# exportTable(item80.final.MH, "MH", "Table 68", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item80.final <- mean_one_group_unweighted(CustomerLevelData = item80.data
                                          ,valueVariable    = 'Count'
                                          ,byVariable       = 'Type'
                                          ,aggregateRow = "Total")
item80.final <- item80.final[which(item80.final$Type != "Total"),]

item80.final <- item80.final[which(item80.final$Type %in% c("Dishwasher"
                                                            ,"Dryer"
                                                            ,"Freezer"
                                                            ,"Refrigerator"
                                                            ,"Washer"
                                                            ,"Water Heater")),]


item80.final.SF <- item80.final[which(item80.final$BuildingType == "Single Family")
                                ,-which(colnames(item80.final) %in% c("BuildingType"))]
item80.final.MH <- item80.final[which(item80.final$BuildingType == "Manufactured")
                                ,-which(colnames(item80.final) %in% c("BuildingType"))]

# exportTable(item80.final.SF, "SF", "Table 87", weighted = FALSE)
# exportTable(item80.final.MH, "MH", "Table 68", weighted = FALSE)



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


################################################
# Adding pop and sample sizes for weights
################################################
tableAB.data <- weightedData(tableAB.merge[-which(colnames(tableAB.merge) %in% c("count"
                                                                              ,"Type"
                                                                              ,"Age"))])
tableAB.data <- left_join(tableAB.data, tableAB.merge[which(colnames(tableAB.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Type"
                                                                                       ,"Age"))])
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

tableAB.final.SF <- tableAB.final[which(tableAB.final$BuildingType == "Single Family")
                                ,-which(colnames(tableAB.final) %in% c("BuildingType"))]
tableAB.final.MH <- tableAB.final[which(tableAB.final$BuildingType == "Manufactured")
                                ,-which(colnames(tableAB.final) %in% c("BuildingType"))]
tableAB.final.MF <- tableAB.final[which(tableAB.final$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAB.final) %in% c("BuildingType"))]

# exportTable(tableAB.final.SF, "SF", "Table AB", weighted = TRUE)
# exportTable(tableAB.final.MH, "MH", "Table AB", weighted = TRUE)
exportTable(tableAB.final.MF, "MF", "Table AB", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableAB.final <- mean_one_group_unweighted(CustomerLevelData = tableAB.data
                                          ,valueVariable    = 'Age'
                                          ,byVariable       = 'Type'
                                          ,aggregateRow = "Total")
tableAB.final <- tableAB.final[which(tableAB.final$Type != "Total"),]

tableAB.final.SF <- tableAB.final[which(tableAB.final$BuildingType == "Single Family")
                                ,-which(colnames(tableAB.final) %in% c("BuildingType"))]
tableAB.final.MH <- tableAB.final[which(tableAB.final$BuildingType == "Manufactured")
                                ,-which(colnames(tableAB.final) %in% c("BuildingType"))]
tableAB.final.MF <- tableAB.final[which(tableAB.final$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAB.final) %in% c("BuildingType"))]

# exportTable(tableAB.final.SF, "SF", "Table AB", weighted = FALSE)
# exportTable(tableAB.final.MH, "MH", "Table AB", weighted = FALSE)
exportTable(tableAB.final.MF, "MF", "Table AB", weighted = FALSE)




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

################################################
# Adding pop and sample sizes for weights
################################################
tableAC.data <- weightedData(tableAC.merge[-which(colnames(tableAC.merge) %in% c("Type"
                                                                                 ,"Age"
                                                                                 ,"count"
                                                                                 ,"MeasureMap"
                                                                                 ,"Above.Measure.Life"
                                                                                 ,"Age.Diff"
                                                                                 ,"Ind"))])
tableAC.data <- left_join(tableAC.data, tableAC.merge[which(colnames(tableAC.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Type"
                                                                                           ,"Age"
                                                                                           ,"count"
                                                                                           ,"MeasureMap"
                                                                                           ,"Above.Measure.Life"
                                                                                           ,"Age.Diff"
                                                                                           ,"Ind"))])
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

tableAC.final.SF <- tableAC.final[which(tableAC.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableAC.final) %in% c("BuildingType"))]
tableAC.final.MH <- tableAC.final[which(tableAC.final$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAC.final) %in% c("BuildingType"))]
tableAC.final.MF <- tableAC.final[which(tableAC.final$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAC.final) %in% c("BuildingType"))]

exportTable(tableAC.final.SF, "SF", "Table AC", weighted = TRUE)
exportTable(tableAC.final.MH, "MH", "Table AC", weighted = TRUE)
exportTable(tableAC.final.MF, "MF", "Table AC", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableAC.final <- proportions_one_group(CustomerLevelData = tableAC.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "Type"
                                       ,total.name = "Total"
                                       ,weighted = FALSE)
tableAC.final <- tableAC.final[which(tableAC.final$Type != "Total"),]

tableAC.final.SF <- tableAC.final[which(tableAC.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableAC.final) %in% c("BuildingType"))]
tableAC.final.MH <- tableAC.final[which(tableAC.final$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAC.final) %in% c("BuildingType"))]
tableAC.final.MF <- tableAC.final[which(tableAC.final$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAC.final) %in% c("BuildingType"))]

exportTable(tableAC.final.SF, "SF", "Table AC", weighted = FALSE)
exportTable(tableAC.final.MH, "MH", "Table AC", weighted = FALSE)
exportTable(tableAC.final.MF, "MF", "Table AC", weighted = FALSE)






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
                                                                          ,"EquipVintage_bins"))])
item81.data <- left_join(item81.data, item81.merge[which(colnames(item81.merge) %in% c("CK_Cadmus_ID"
                                                                                   ,"count"
                                                                                   ,"Type"
                                                                                   ,"Age"
                                                                                   ,"EquipVintage_bins"))])
item81.data$count <- 1

#######################
# Weighted Analysis
#######################
item81.final <- proportions_one_group(CustomerLevelData = item81.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'EquipVintage_bins'
                                      ,total.name       = 'All Vintages')

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

item81.final.SF <- item81.final[which(item81.final$BuildingType == "Single Family")
                                ,-which(colnames(item81.final) %in% c("BuildingType"))]
item81.final.MH <- item81.final[which(item81.final$BuildingType == "Manufactured")
                                ,-which(colnames(item81.final) %in% c("BuildingType"))]
item81.final.MF <- item81.final[which(item81.final$BuildingType == "Multifamily")
                                ,-which(colnames(item81.final) %in% c("BuildingType"))]

# exportTable(item81.final.SF, "SF", "Table 88", weighted = TRUE)
# exportTable(item81.final.MH, "MH", "Table 69", weighted = TRUE)
exportTable(item81.final.MF, "MF", "Table 87", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item81.final <- proportions_one_group(CustomerLevelData = item81.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'EquipVintage_bins'
                                      ,total.name       = 'All Vintages'
                                      ,weighted         = FALSE)

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

item81.final.SF <- item81.final[which(item81.final$BuildingType == "Single Family")
                                ,-which(colnames(item81.final) %in% c("BuildingType"))]
item81.final.MH <- item81.final[which(item81.final$BuildingType == "Manufactured")
                                ,-which(colnames(item81.final) %in% c("BuildingType"))]
item81.final.MF <- item81.final[which(item81.final$BuildingType == "Multifamily")
                                ,-which(colnames(item81.final) %in% c("BuildingType"))]

# exportTable(item81.final.SF, "SF", "Table 88", weighted = FALSE)
# exportTable(item81.final.MH, "MH", "Table 69", weighted = FALSE)
exportTable(item81.final.MF, "MF", "Table 87", weighted = FALSE)































############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
scl.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.scl.data", rundate, ".xlsx", sep = "")))
length(unique(scl.dat$CK_Cadmus_ID))
scl.dat$CK_Building_ID <- scl.dat$Category
scl.dat <- scl.dat[which(names(scl.dat) != "Category")]

#############################################################################################
#Item 80: AVERAGE NUMBER OF APPLIANCES PER HOME BY TYPE (SF table 87, MH table 68)
#############################################################################################
# For water Heaters
item80.os.mech <- mechanical.dat[grep("Water Heat", mechanical.dat$Generic),]
item80.os.mech$Generic[grep("Water Heat", item80.os.mech$Generic)] <- "Water Heater"
item80.os.mech$WaterHeaterCount <- 1
item80.os.mech1 <- left_join(scl.dat, item80.os.mech, by = "CK_Cadmus_ID")
item80.os.mech2 <- item80.os.mech1
which(duplicated(item80.os.mech2$CK_Cadmus_ID))

item80.os.mech2$WaterHeaterCount[which(is.na(item80.os.mech2$WaterHeaterCount))] <- 0
item80.os.mech2$count <- 1

#summarise by home
item80.os.site <- summarise(group_by(item80.os.mech2, CK_Cadmus_ID, Generic)
                         ,Count = sum(WaterHeaterCount))
unique(item80.os.site$Count)
colnames(item80.os.site)[which(colnames(item80.os.site) == "Generic")] <- "Type"

#For everything else
item80.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Large.Unusual.Load.Quantity"
                                                                   ,"Age"
                                                                   ,""
                                                                   ,""))]
item80.os.dat$count <- 1

item80.os.dat0 <- item80.os.dat[which(item80.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item80.os.dat1 <- left_join(item80.os.dat0, scl.dat, by = "CK_Cadmus_ID")
item80.os.dat1$Large.Unusual.Load.Quantity[which(item80.os.dat1$Large.Unusual.Load.Quantity %in% c("N/A",NA))] <- 1
unique(item80.os.dat1$Large.Unusual.Load.Quantity)
item80.os.dat1$Large.Unusual.Load.Quantity <- as.numeric(as.character(item80.os.dat1$Large.Unusual.Load.Quantity))


item80.os.dat1$TotalQty <- item80.os.dat1$Large.Unusual.Load.Quantity * item80.os.dat1$count

item80.os.sum <- summarise(group_by(item80.os.dat1, CK_Cadmus_ID, Type)
                        ,Count = sum(TotalQty))

# Row bind water heater and appliance counts
item80.os.merge <- rbind.data.frame(item80.os.site, item80.os.sum)

item80.os.merge <- left_join(scl.dat, item80.os.merge) #switch scl.dat to scl.merge to get more info on washers/dryers

item80.os.merge <- item80.os.merge[which(!is.na(item80.os.merge$Type)),]
item80.os.merge$Count[which(is.na(item80.os.merge$Count))] <- 0

item80.os.cast <- dcast(setDT(item80.os.merge)
                     ,formula = CK_Cadmus_ID ~ Type
                     ,value.var = c("Count"))
item80.os.cast[is.na(item80.os.cast),] <- 0

item80.os.melt <- melt(item80.os.cast, id.vars = "CK_Cadmus_ID")
names(item80.os.melt) <- c("CK_Cadmus_ID", "Type", "Count")

item80.os.merge <- left_join(scl.dat, item80.os.melt)
item80.os.merge$Type <- as.character(item80.os.merge$Type)

unique(item80.os.merge$Type)
item80.os.merge <- item80.os.merge[which(item80.os.merge$Type %in% c("Dishwasher"
                                                            ,"Dryer"
                                                            ,"Freezer"
                                                            ,"Refrigerator"
                                                            ,"Washer"
                                                            ,"Water Heater")),]


################################################
# Adding pop and sample sizes for weights
################################################
item80.os.data <- weightedData(item80.os.merge[-which(colnames(item80.os.merge) %in% c("Count"
                                                                              ,"Type"
                                                                              ,"Age"))])
item80.os.data <- left_join(item80.os.data, unique(item80.os.merge[which(colnames(item80.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Count"
                                                                                       ,"Type"
                                                                                       ,"Age"))]))
item80.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item80.os.final <- mean_two_groups(CustomerLevelData = item80.os.data
                               ,valueVariable    = 'Count'
                               ,byVariableColumn = "CK_Building_ID"
                               ,byVariableRow    = 'Type'
                               ,columnAggregate = "Remove"
                               ,rowAggregate = "Total")
item80.os.cast <- item80.os.final[which(item80.os.final$Type != "Total"),]

item80.os.final <- data.frame("BuildingType"          = item80.os.cast$BuildingType
                              ,"Type"                 = item80.os.cast$Type
                              ,"Mean_SCL.GenPop"      = item80.os.cast$`Mean_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item80.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item80.os.cast$`n_SCL GenPop`
                              ,"Mean_SCL.LI"          = item80.os.cast$`Mean_SCL LI`
                              ,"SE_SCL.LI"            = item80.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = item80.os.cast$`n_SCL LI`
                              ,"Mean_SCL.EH"          = item80.os.cast$`Mean_SCL EH`
                              ,"SE_SCL.EH"            = item80.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = item80.os.cast$`n_SCL EH`
                              ,"Mean_2017.RBSA.PS"    = item80.os.cast$`Mean_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item80.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item80.os.cast$`n_2017 RBSA PS`
                              ,"EB_SCL.GenPop"        = item80.os.cast$`EB_SCL GenPop`
                              ,"EB_SCL.LI"            = item80.os.cast$`EB_SCL LI`
                              ,"EB_SCL.EH"            = item80.os.cast$`EB_SCL EH`
                              ,"EB_2017.RBSA.PS"      = item80.os.cast$`EB_2017 RBSA PS`)

item80.os.final.SF <- item80.os.final[which(item80.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item80.os.final) %in% c("BuildingType"))]

exportTable(item80.os.final.SF, "SF", "Table 87", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item80.os.final <- mean_two_groups_unweighted(CustomerLevelData = item80.os.data
                                   ,valueVariable    = 'Count'
                                   ,byVariableColumn = "CK_Building_ID"
                                   ,byVariableRow    = 'Type'
                                   ,columnAggregate = "Remove"
                                   ,rowAggregate = "Total")
item80.os.cast <- item80.os.final[which(item80.os.final$Type != "Total"),]

item80.os.final <- data.frame("BuildingType"          = item80.os.cast$BuildingType
                              ,"Type"                 = item80.os.cast$Type
                              ,"Mean_SCL.GenPop"      = item80.os.cast$`Mean_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item80.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item80.os.cast$`n_SCL GenPop`
                              ,"Mean_SCL.LI"          = item80.os.cast$`Mean_SCL LI`
                              ,"SE_SCL.LI"            = item80.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = item80.os.cast$`n_SCL LI`
                              ,"Mean_SCL.EH"          = item80.os.cast$`Mean_SCL EH`
                              ,"SE_SCL.EH"            = item80.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = item80.os.cast$`n_SCL EH`
                              ,"Mean_2017.RBSA.PS"    = item80.os.cast$`Mean_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item80.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item80.os.cast$`n_2017 RBSA PS`)

item80.os.final.SF <- item80.os.final[which(item80.os.final$BuildingType == "Single Family")
                                      ,-which(colnames(item80.os.final) %in% c("BuildingType"))]

exportTable(item80.os.final.SF, "SF", "Table 87", weighted = FALSE, osIndicator = "SCL", OS = T)



#############################################################################################
#Table AB: Average Age of Appliance Equipment by Type
#############################################################################################
tableAB.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,"Age"))]
tableAB.os.dat$count <- 1

tableAB.os.dat$Age <- as.numeric(as.character(tableAB.os.dat$Age))
tableAB.os.dat0 <- tableAB.os.dat[which(tableAB.os.dat$Age > 0),]

tableAB.os.merge <- left_join(scl.dat, tableAB.os.dat0, by = "CK_Cadmus_ID")
tableAB.os.merge <- tableAB.os.merge[which(tableAB.os.merge$Age > 0),]

unique(tableAB.os.merge$Type)
tableAB.os.merge <- tableAB.os.merge[which(tableAB.os.merge$Type %in% c("Dishwasher"
                                                               ,"Dryer"
                                                               ,"Freezer"
                                                               ,"Refrigerator"
                                                               ,"Washer")),]


################################################
# Adding pop and sample sizes for weights
################################################
tableAB.os.data <- weightedData(tableAB.os.merge[-which(colnames(tableAB.os.merge) %in% c("count"
                                                                                 ,"Type"
                                                                                 ,"Age"))])
tableAB.os.data <- left_join(tableAB.os.data, unique(tableAB.os.merge[which(colnames(tableAB.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"count"
                                                                                           ,"Type"
                                                                                           ,"Age"))]))
tableAB.os.data$count <- 1

#######################
# Weighted Analysis
#######################
tableAB.os.final <- mean_two_groups(CustomerLevelData = tableAB.os.data
                                   ,valueVariable    = 'Age'
                                   ,byVariableColumn = "CK_Building_ID"
                                   ,byVariableRow    = 'Type'
                                   ,columnAggregate = "Remove"
                                   ,rowAggregate = "Total")
tableAB.os.cast <- tableAB.os.final[which(tableAB.os.final$Type != "Total"),]

tableAB.os.final <- data.frame("BuildingType"          = tableAB.os.cast$BuildingType
                              ,"Type"                 = tableAB.os.cast$Type
                              ,"Mean_SCL.GenPop"      = tableAB.os.cast$`Mean_SCL GenPop`
                              ,"SE_SCL.GenPop"        = tableAB.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = tableAB.os.cast$`n_SCL GenPop`
                              ,"Mean_SCL.LI"          = tableAB.os.cast$`Mean_SCL LI`
                              ,"SE_SCL.LI"            = tableAB.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = tableAB.os.cast$`n_SCL LI`
                              ,"Mean_SCL.EH"          = tableAB.os.cast$`Mean_SCL EH`
                              ,"SE_SCL.EH"            = tableAB.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = tableAB.os.cast$`n_SCL EH`
                              ,"Mean_2017.RBSA.PS"    = tableAB.os.cast$`Mean_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = tableAB.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = tableAB.os.cast$`n_2017 RBSA PS`
                              ,"EB_SCL.GenPop"        = tableAB.os.cast$`EB_SCL GenPop`
                              ,"EB_SCL.LI"            = tableAB.os.cast$`EB_SCL LI`
                              ,"EB_SCL.EH"            = tableAB.os.cast$`EB_SCL EH`
                              ,"EB_2017.RBSA.PS"      = tableAB.os.cast$`EB_2017 RBSA PS`)

tableAB.os.final.SF <- tableAB.os.final[which(tableAB.os.final$BuildingType == "Single Family")
                                      ,-which(colnames(tableAB.os.final) %in% c("BuildingType"))]

exportTable(tableAB.os.final.SF, "SF", "Table AB", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
tableAB.os.final <- mean_two_groups_unweighted(CustomerLevelData = tableAB.os.data
                                    ,valueVariable    = 'Age'
                                    ,byVariableColumn = "CK_Building_ID"
                                    ,byVariableRow    = 'Type'
                                    ,columnAggregate = "Remove"
                                    ,rowAggregate = "Total")
tableAB.os.cast <- tableAB.os.final[which(tableAB.os.final$Type != "Total"),]

tableAB.os.final <- data.frame("BuildingType"          = tableAB.os.cast$BuildingType
                               ,"Type"                 = tableAB.os.cast$Type
                               ,"Mean_SCL.GenPop"      = tableAB.os.cast$`Mean_SCL GenPop`
                               ,"SE_SCL.GenPop"        = tableAB.os.cast$`SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = tableAB.os.cast$`n_SCL GenPop`
                               ,"Mean_SCL.LI"          = tableAB.os.cast$`Mean_SCL LI`
                               ,"SE_SCL.LI"            = tableAB.os.cast$`SE_SCL LI`
                               ,"n_SCL.LI"             = tableAB.os.cast$`n_SCL LI`
                               ,"Mean_SCL.EH"          = tableAB.os.cast$`Mean_SCL EH`
                               ,"SE_SCL.EH"            = tableAB.os.cast$`SE_SCL EH`
                               ,"n_SCL.EH"             = tableAB.os.cast$`n_SCL EH`
                               ,"Mean_2017.RBSA.PS"    = tableAB.os.cast$`Mean_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = tableAB.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = tableAB.os.cast$`n_2017 RBSA PS`)

tableAB.os.final.SF <- tableAB.os.final[which(tableAB.os.final$BuildingType == "Single Family")
                                        ,-which(colnames(tableAB.os.final) %in% c("BuildingType"))]

exportTable(tableAB.os.final.SF, "SF", "Table AB", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Table AC: Percent of Appliance Equipment above measure life by Type
#############################################################################################
# For water Heaters
tableAC.os.mech <- mechanical.dat[grep("Water Heat", mechanical.dat$Generic),]
tableAC.os.mech$Generic[grep("Water Heat", tableAC.os.mech$Generic)] <- "Water Heater"
tableAC.os.mech$WaterHeaterCount <- 1

tableAC.os.mech1 <- left_join(scl.dat, tableAC.os.mech, by = "CK_Cadmus_ID")
tableAC.os.mech2 <- tableAC.os.mech1
which(duplicated(tableAC.os.mech2$CK_Cadmus_ID))

tableAC.os.mech2$WaterHeaterCount[which(is.na(tableAC.os.mech2$WaterHeaterCount))] <- 0
tableAC.os.mech2$count <- 1

#summarise by home
tableAC.os.site <- summarise(group_by(tableAC.os.mech2, CK_Cadmus_ID, Generic, DHW.Year.Manufactured)
                          ,count = sum(WaterHeaterCount))
unique(tableAC.os.site$count)
colnames(tableAC.os.site)[which(colnames(tableAC.os.site) %in% c("Generic", "DHW.Year.Manufactured"))] <- c("Type","Age")
tableAC.os.site$Age <- as.numeric(as.character(tableAC.os.site$Age))
tableAC.os.site1 <- tableAC.os.site[which(!is.na(tableAC.os.site$Age)),]
tableAC.os.site2 <- tableAC.os.site1[which(tableAC.os.site1$Age > 0),]


tableAC.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,"Age"
                                                                    ,""
                                                                    ,""))]

tableAC.os.dat$count <- 1

tableAC.os.dat$Age <- as.numeric(as.character(tableAC.os.dat$Age))
tableAC.os.dat0 <- tableAC.os.dat[which(tableAC.os.dat$Age > 0),]

tableAC.os.merge0 <- rbind.data.frame(tableAC.os.site2, tableAC.os.dat0)
tableAC.os.merge <- left_join(scl.dat, tableAC.os.merge0, by = "CK_Cadmus_ID")
tableAC.os.merge <- tableAC.os.merge[which(tableAC.os.merge$Age > 0),]

unique(tableAC.os.merge$Type)
tableAC.os.merge <- tableAC.os.merge[which(tableAC.os.merge$Type %in% c("Dishwasher"
                                                               ,"Dryer"
                                                               ,"Freezer"
                                                               ,"Refrigerator"
                                                               ,"Washer"
                                                               ,"Water Heater")),]

tableAC.os.merge$MeasureMap <- 0
tableAC.os.merge$MeasureMap[which(tableAC.os.merge$Type == "Refrigerator")] <- 15
tableAC.os.merge$MeasureMap[which(tableAC.os.merge$Type == "Freezer")] <- 22
tableAC.os.merge$MeasureMap[which(tableAC.os.merge$Type == "Washer")] <- 14
tableAC.os.merge$MeasureMap[which(tableAC.os.merge$Type == "Dryer")] <- 12
tableAC.os.merge$MeasureMap[which(tableAC.os.merge$Type == "Dishwasher")] <- 12
tableAC.os.merge$MeasureMap[which(tableAC.os.merge$Type == "Water Heater")] <- 15

tableAC.os.merge$Age.Diff <- 2017 - tableAC.os.merge$Age

tableAC.os.merge$Above.Measure.Life <- "No"
tableAC.os.merge$Above.Measure.Life[which(tableAC.os.merge$Age.Diff > tableAC.os.merge$MeasureMap)] <- "Yes"

tableAC.os.merge$Ind <- 0
tableAC.os.merge$Ind[which(tableAC.os.merge$Age.Diff > tableAC.os.merge$MeasureMap)] <- 1

################################################
# Adding pop and sample sizes for weights
################################################
tableAC.os.data <- weightedData(tableAC.os.merge[-which(colnames(tableAC.os.merge) %in% c("Type"
                                                                                 ,"Age"
                                                                                 ,"count"
                                                                                 ,"MeasureMap"
                                                                                 ,"Above.Measure.Life"
                                                                                 ,"Age.Diff"
                                                                                 ,"Ind"))])
tableAC.os.data <- left_join(tableAC.os.data, unique(tableAC.os.merge[which(colnames(tableAC.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Type"
                                                                                           ,"Age"
                                                                                           ,"count"
                                                                                           ,"MeasureMap"
                                                                                           ,"Above.Measure.Life"
                                                                                           ,"Age.Diff"
                                                                                           ,"Ind"))]))
tableAC.os.data$count <- 1
tableAC.os.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableAC.os.final <- proportionRowsAndColumns1(CustomerLevelData = tableAC.os.data
                                       ,valueVariable = "Ind"
                                       ,columnVariable = "CK_Building_ID"
                                       ,rowVariable = "Type"
                                       ,aggregateColumnName = "Remove")
tableAC.os.final <- tableAC.os.final[which(tableAC.os.final$CK_Building_ID != "Remove"),]
tableAC.os.final <- tableAC.os.final[which(tableAC.os.final$Type != "Total"),]

tableAC.os.cast <- dcast(setDT(tableAC.os.final)
                          ,formula = BuildingType + Type ~ CK_Building_ID
                          ,value.var = c("w.percent", "w.SE","n", "EB"))

tableAC.os.final <- data.frame("BuildingType"          = tableAC.os.cast$BuildingType
                               ,"Type"                 = tableAC.os.cast$Type
                               ,"Percent_SCL.GenPop"   = tableAC.os.cast$`w.percent_SCL GenPop`
                               ,"SE_SCL.GenPop"        = tableAC.os.cast$`w.SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = tableAC.os.cast$`n_SCL GenPop`
                               ,"Percent_SCL.LI"       = tableAC.os.cast$`w.percent_SCL LI`
                               ,"SE_SCL.LI"            = tableAC.os.cast$`w.SE_SCL LI`
                               ,"n_SCL.LI"             = tableAC.os.cast$`n_SCL LI`
                               ,"Percent_SCL.EH"       = tableAC.os.cast$`w.percent_SCL EH`
                               ,"SE_SCL.EH"            = tableAC.os.cast$`w.SE_SCL EH`
                               ,"n_SCL.EH"             = tableAC.os.cast$`n_SCL EH`
                               ,"Percent_2017.RBSA.PS" = tableAC.os.cast$`w.percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = tableAC.os.cast$`w.SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = tableAC.os.cast$`n_2017 RBSA PS`
                               ,"EB_SCL.GenPop"        = tableAC.os.cast$`EB_SCL GenPop`
                               ,"EB_SCL.LI"            = tableAC.os.cast$`EB_SCL LI`
                               ,"EB_SCL.EH"            = tableAC.os.cast$`EB_SCL EH`
                               ,"EB_2017.RBSA.PS"      = tableAC.os.cast$`EB_2017 RBSA PS`)

tableAC.os.final.SF <- tableAC.os.final[which(tableAC.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableAC.os.final) %in% c("BuildingType"))]

exportTable(tableAC.os.final.SF, "SF", "Table AC", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
tableAC.os.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAC.os.data
                                              ,valueVariable = "Ind"
                                              ,columnVariable = "CK_Building_ID"
                                              ,rowVariable = "Type"
                                              ,aggregateColumnName = "Remove")
tableAC.os.final <- tableAC.os.final[which(tableAC.os.final$CK_Building_ID != "Remove"),]
tableAC.os.final <- tableAC.os.final[which(tableAC.os.final$Type != "Total"),]

tableAC.os.cast <- dcast(setDT(tableAC.os.final)
                         ,formula = BuildingType + Type ~ CK_Building_ID
                         ,value.var = c("Percent", "SE","n"))

tableAC.os.final <- data.frame("BuildingType"          = tableAC.os.cast$BuildingType
                               ,"Type"                 = tableAC.os.cast$Type
                               ,"Percent_SCL.GenPop"   = tableAC.os.cast$`Percent_SCL GenPop`
                               ,"SE_SCL.GenPop"        = tableAC.os.cast$`SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = tableAC.os.cast$`n_SCL GenPop`
                               ,"Percent_SCL.LI"       = tableAC.os.cast$`Percent_SCL LI`
                               ,"SE_SCL.LI"            = tableAC.os.cast$`SE_SCL LI`
                               ,"n_SCL.LI"             = tableAC.os.cast$`n_SCL LI`
                               ,"Percent_SCL.EH"       = tableAC.os.cast$`Percent_SCL EH`
                               ,"SE_SCL.EH"            = tableAC.os.cast$`SE_SCL EH`
                               ,"n_SCL.EH"             = tableAC.os.cast$`n_SCL EH`
                               ,"Percent_2017.RBSA.PS" = tableAC.os.cast$`Percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = tableAC.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = tableAC.os.cast$`n_2017 RBSA PS`)

tableAC.os.final.SF <- tableAC.os.final[which(tableAC.os.final$BuildingType == "Single Family")
                                        ,-which(colnames(tableAC.os.final) %in% c("BuildingType"))]

exportTable(tableAC.os.final.SF, "SF", "Table AC", weighted = FALSE, osIndicator = "SCL", OS = T)



#############################################################################################
#Item 81: DISTRIBUTION OF REFRIGERATOR/FREEZERS BY VINTAGE (SF table 88, MH table 69)
#############################################################################################
#subset to columns needed for analysis
item81.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,""))]
item81.os.dat$count <- 1

item81.os.dat0 <- item81.os.dat[which(item81.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item81.os.dat1 <- left_join(item81.os.dat0, scl.dat, by = "CK_Cadmus_ID")
item81.os.dat2 <- item81.os.dat1[which(item81.os.dat1$Type %in% c("Refrigerator", "Freezer")),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item81.os.dat2$EquipVintage_bins <- as.numeric(as.character(item81.os.dat2$Age))
item81.os.dat3 <- item81.os.dat2[which(!(is.na(item81.os.dat2$EquipVintage_bins))),]

item81.os.dat3$EquipVintage_bins[which(item81.os.dat3$Age < 1980)] <- "Pre 1980"
item81.os.dat3$EquipVintage_bins[which(item81.os.dat3$Age >= 1980 & item81.os.dat3$Age < 1990)] <- "1980-1989"
item81.os.dat3$EquipVintage_bins[which(item81.os.dat3$Age >= 1990 & item81.os.dat3$Age < 1995)] <- "1990-1994"
item81.os.dat3$EquipVintage_bins[which(item81.os.dat3$Age >= 1995 & item81.os.dat3$Age < 2000)] <- "1995-1999"
item81.os.dat3$EquipVintage_bins[which(item81.os.dat3$Age >= 2000 & item81.os.dat3$Age < 2005)] <- "2000-2004"
item81.os.dat3$EquipVintage_bins[which(item81.os.dat3$Age >= 2005 & item81.os.dat3$Age < 2010)] <- "2005-2009"
item81.os.dat3$EquipVintage_bins[which(item81.os.dat3$Age >= 2010 & item81.os.dat3$Age < 2015)] <- "2010-2014"
item81.os.dat3$EquipVintage_bins[which(item81.os.dat3$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item81.os.dat3$EquipVintage_bins)


item81.os.merge <- left_join(scl.dat, item81.os.dat3)
item81.os.merge <- item81.os.merge[which(!is.na(item81.os.merge$EquipVintage_bins)),]

################################################
# Adding pop and sample sizes for weights
################################################
item81.os.data <- weightedData(item81.os.merge[-which(colnames(item81.os.merge) %in% c("count"
                                                                              ,"Type"
                                                                              ,"Age"
                                                                              ,"EquipVintage_bins"))])
item81.os.data <- left_join(item81.os.data, unique(item81.os.merge[which(colnames(item81.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Type"
                                                                                       ,"Age"
                                                                                       ,"EquipVintage_bins"))]))
item81.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item81.os.final <- proportionRowsAndColumns1(CustomerLevelData = item81.os.data
                                              ,valueVariable = "count"
                                              ,columnVariable = "CK_Building_ID"
                                              ,rowVariable = "EquipVintage_bins"
                                              ,aggregateColumnName = "Remove")
item81.os.final <- item81.os.final[which(item81.os.final$CK_Building_ID != "Remove"),]

item81.os.cast <- dcast(setDT(item81.os.final)
                         ,formula = BuildingType + EquipVintage_bins ~ CK_Building_ID
                         ,value.var = c("w.percent", "w.SE","n", "EB"))

item81.os.final <- data.frame("BuildingType"          = item81.os.cast$BuildingType
                               ,"Equipment.Vintage"    = item81.os.cast$EquipVintage_bins
                               ,"Percent_SCL.GenPop"   = item81.os.cast$`w.percent_SCL GenPop`
                               ,"SE_SCL.GenPop"        = item81.os.cast$`w.SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = item81.os.cast$`n_SCL GenPop`
                               ,"Percent_SCL.LI"       = item81.os.cast$`w.percent_SCL LI`
                               ,"SE_SCL.LI"            = item81.os.cast$`w.SE_SCL LI`
                               ,"n_SCL.LI"             = item81.os.cast$`n_SCL LI`
                               ,"Percent_SCL.EH"       = item81.os.cast$`w.percent_SCL EH`
                               ,"SE_SCL.EH"            = item81.os.cast$`w.SE_SCL EH`
                               ,"n_SCL.EH"             = item81.os.cast$`n_SCL EH`
                               ,"Percent_2017.RBSA.PS" = item81.os.cast$`w.percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item81.os.cast$`w.SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item81.os.cast$`n_2017 RBSA PS`
                               ,"EB_SCL.GenPop"        = item81.os.cast$`EB_SCL GenPop`
                               ,"EB_SCL.LI"            = item81.os.cast$`EB_SCL LI`
                               ,"EB_SCL.EH"            = item81.os.cast$`EB_SCL EH`
                               ,"EB_2017.RBSA.PS"      = item81.os.cast$`EB_2017 RBSA PS`)

unique(item81.os.final$Equipment.Vintage)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item81.os.final <- item81.os.final %>% mutate(Equipment.Vintage = factor(Equipment.Vintage, levels = rowOrder)) %>% arrange(Equipment.Vintage)  
item81.os.final <- data.frame(item81.os.final)

item81.os.final.SF <- item81.os.final[which(item81.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item81.os.final) %in% c("BuildingType"))]

exportTable(item81.os.final.SF, "SF", "Table 88", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item81.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item81.os.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = "EquipVintage_bins"
                                             ,aggregateColumnName = "Remove")
item81.os.final <- item81.os.final[which(item81.os.final$CK_Building_ID != "Remove"),]

item81.os.cast <- dcast(setDT(item81.os.final)
                        ,formula = BuildingType + EquipVintage_bins ~ CK_Building_ID
                        ,value.var = c("Percent", "SE","n"))

item81.os.final <- data.frame("BuildingType"          = item81.os.cast$BuildingType
                              ,"Equipment.Vintage"    = item81.os.cast$EquipVintage_bins
                              ,"Percent_SCL.GenPop"   = item81.os.cast$`Percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item81.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item81.os.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = item81.os.cast$`Percent_SCL LI`
                              ,"SE_SCL.LI"            = item81.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = item81.os.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = item81.os.cast$`Percent_SCL EH`
                              ,"SE_SCL.EH"            = item81.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = item81.os.cast$`n_SCL EH`
                              ,"Percent_2017.RBSA.PS" = item81.os.cast$`Percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item81.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item81.os.cast$`n_2017 RBSA PS`)

unique(item81.os.final$Equipment.Vintage)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item81.os.final <- item81.os.final %>% mutate(Equipment.Vintage = factor(Equipment.Vintage, levels = rowOrder)) %>% arrange(Equipment.Vintage)  
item81.os.final <- data.frame(item81.os.final)

item81.os.final.SF <- item81.os.final[which(item81.os.final$BuildingType == "Single Family")
                                      ,-which(colnames(item81.os.final) %in% c("BuildingType"))]

exportTable(item81.os.final.SF, "SF", "Table 88", weighted = FALSE, osIndicator = "SCL", OS = T)
