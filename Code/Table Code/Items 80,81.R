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
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
appliances.dat <- data.frame(read.xlsx(xlsxFile = file.path(filepathRawData, "Appliances_CS.xlsx")
                                       , sheet = "Sheet1")
                             ,stringsAsFactors = FALSE)
                                                         
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))



#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))


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
  
  item80.sum <- summarise(group_by(item80.dat1, CK_Cadmus_ID, Type)
                          ,Count = sum(TotalQty))
  
# Row bind water heater and appliance counts
  item80.merge <- rbind.data.frame(item80.site, item80.sum)
  
item80.tmp <- left_join(rbsa.dat, item80.merge)
item80.tmp$Count[which(is.na(item80.tmp$Count))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
item80.data <- weightedData(item80.tmp[-which(colnames(item80.tmp) %in% c("Count"
                                                                              ,"Type"))])
item80.data <- left_join(item80.data, item80.tmp[which(colnames(item80.tmp) %in% c("CK_Cadmus_ID"
                                                                                       ,"Count"
                                                                                       ,"Type"))])
item80.data$count <- 1

#######################
# Weighted Analysis
#######################
item80.final <- mean_one_group(CustomerLevelData = item80.data
                               ,valueVariable    = 'Count'
                               ,byVariable       = 'Type'
                               ,aggregateRow     = FALSE)

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

exportTable(item80.final.SF, "SF", "Table 87", weighted = TRUE)
exportTable(item80.final.MH, "MH", "Table 68", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item80.final <- mean_one_group_unweighted(CustomerLevelData = item80.data
                               ,valueVariable    = 'Count'
                               ,byVariable       = 'Type'
                               ,aggregateRow     = FALSE)

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

exportTable(item80.final.SF, "SF", "Table 87", weighted = FALSE)
exportTable(item80.final.MH, "MH", "Table 68", weighted = FALSE)






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
                                      ,total.name       = 'All Vintages'
                                      ,columnName       = 'Remove')

item81.final.SF <- item81.final[which(item81.final$BuildingType == "Single Family")
                                ,-which(colnames(item81.final) %in% c("BuildingType"
                                                                      ,"Remove"))]
item81.final.MH <- item81.final[which(item81.final$BuildingType == "Manufactured")
                                ,-which(colnames(item81.final) %in% c("BuildingType"
                                                                      ,"Remove"))]
item81.final.MF <- item81.final[which(item81.final$BuildingType == "Multifamily")
                                ,-which(colnames(item81.final) %in% c("BuildingType"
                                                                      ,"Remove"))]

exportTable(item81.final.SF, "SF", "Table 88", weighted = TRUE)
exportTable(item81.final.MH, "MH", "Table 69", weighted = TRUE)
exportTable(item81.final.MF, "MF", "Table 87", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item81.final <- proportions_one_group(CustomerLevelData = item81.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'EquipVintage_bins'
                                      ,total.name       = 'All Vintages'
                                      ,columnName       = 'Remove'
                                      ,weighted         = FALSE)

item81.final.SF <- item81.final[which(item81.final$BuildingType == "Single Family")
                                ,-which(colnames(item81.final) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"Total.Count"))]
item81.final.MH <- item81.final[which(item81.final$BuildingType == "Manufactured")
                                ,-which(colnames(item81.final) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"Total.Count"))]
item81.final.MF <- item81.final[which(item81.final$BuildingType == "Multifamily")
                                ,-which(colnames(item81.final) %in% c("BuildingType"
                                                                      ,"Remove"))]

exportTable(item81.final.SF, "SF", "Table 88", weighted = FALSE)
exportTable(item81.final.MH, "MH", "Table 69", weighted = FALSE)
exportTable(item81.final.MF, "MF", "Table 87", weighted = FALSE)

