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

#Read in data for analysis -- Item 77 and table ZZ
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))


#Read in data for analysis -- Item 79
rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
#clean cadmus IDs
rooms.dat$CK_Cadmus_ID <- trimws(toupper(rooms.dat$CK_Cadmus_ID))


#############################################################################################
#Item 77: PERCENTAGE OF ALL CFLS THAT ARE STORED (SF table 84, MH table 63)
#############################################################################################
#subset to columns needed for analysis
item77.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
item77.dat$count <- 1

item77.dat0 <- item77.dat[which(item77.dat$Lamp.Category == "Compact Fluorescent"),]

item77.dat1 <- left_join(rbsa.dat, item77.dat0, by = "CK_Cadmus_ID")

item77.dat2 <- item77.dat1[grep("SITE", item77.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item77.dat2$Fixture.Qty <- as.numeric(as.character(item77.dat2$Fixture.Qty))
item77.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item77.dat2$LIGHTING_BulbsPerFixture))

item77.dat2$Lamps <- item77.dat2$Fixture.Qty * item77.dat2$LIGHTING_BulbsPerFixture
unique(item77.dat2$Lamps)

item77.dat3 <- item77.dat2[which(!(is.na(item77.dat2$Lamps))),]

item77.cfl.sum <- summarise(group_by(item77.dat3, CK_Cadmus_ID)
                            ,TotalBulbs = sum(Lamps))

item77.merge1 <- left_join(rbsa.dat, item77.cfl.sum)

## subset to only storage bulbs
item77.storage <- item77.dat3[which(item77.dat3$Clean.Room == "Storage"),]
#summarise within site
item77.storage.sum <- summarise(group_by(item77.storage, CK_Cadmus_ID)
                                 ,StorageBulbs = sum(Lamps))

item77.merge2 <- left_join(item77.merge1, item77.storage.sum)
item77.merge <- item77.merge2[which(!is.na(item77.merge2$TotalBulbs)),]
item77.merge$StorageBulbs[which(is.na(item77.merge$StorageBulbs))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
item77.data <- weightedData(item77.merge[-which(colnames(item77.merge) %in% c("StorageBulbs"
                                                                              ,"TotalBulbs"))])
item77.data <- left_join(item77.data, item77.merge[which(colnames(item77.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"StorageBulbs"
                                                                                       ,"TotalBulbs"))])
item77.data$count <- 1

#######################
# Weighted Analysis
#######################
item77.final <- proportions_one_group(CustomerLevelData = item77.data
                               ,valueVariable    = 'StorageBulbs'
                               ,groupingVariable = 'State'
                               ,total.name       = 'Region'
                               ,columnName       = 'Remove')

item77.final.SF <- item77.final[which(item77.final$BuildingType == "Single Family")
                                ,-which(colnames(item77.final) %in% c("BuildingType"))]
item77.final.MH <- item77.final[which(item77.final$BuildingType == "Manufactured")
                                ,-which(colnames(item77.final) %in% c("BuildingType"))]

exportTable(item77.final.SF, "SF", "Table 84", weighted = TRUE)
exportTable(item77.final.MH, "MH", "Table 63", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item77.final <- proportions_one_group(CustomerLevelData = item77.data
                                      ,valueVariable    = 'StorageBulbs'
                                      ,groupingVariable = 'State'
                                      ,total.name       = 'Region'
                                      ,columnName       = 'Remove'
                                      ,weighted         = FALSE)

item77.final.SF <- item77.final[which(item77.final$BuildingType == "Single Family")
                                ,-which(colnames(item77.final) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"Total.Count"))]
item77.final.MH <- item77.final[which(item77.final$BuildingType == "Manufactured")
                                ,-which(colnames(item77.final) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"Total.Count"))]

exportTable(item77.final.SF, "SF", "Table 84", weighted = FALSE)
exportTable(item77.final.MH, "MH", "Table 63", weighted = FALSE)




#############################################################################################
#Table ZZ: Percentage of All LEDs that are Stored
#############################################################################################
#subset to columns needed for analysis
tableZZ.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"))]
tableZZ.dat$count <- 1

tableZZ.dat0 <- tableZZ.dat[which(tableZZ.dat$Lamp.Category == "Light Emitting Diode"),]

tableZZ.dat1 <- left_join(rbsa.dat, tableZZ.dat0, by = "CK_Cadmus_ID")

tableZZ.dat2 <- tableZZ.dat1[grep("SITE", tableZZ.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
tableZZ.dat2$Fixture.Qty <- as.numeric(as.character(tableZZ.dat2$Fixture.Qty))
tableZZ.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(tableZZ.dat2$LIGHTING_BulbsPerFixture))

tableZZ.dat2$Lamps <- tableZZ.dat2$Fixture.Qty * tableZZ.dat2$LIGHTING_BulbsPerFixture
unique(tableZZ.dat2$Lamps)

tableZZ.dat3 <- tableZZ.dat2[which(!(is.na(tableZZ.dat2$Lamps))),]

tableZZ.cfl.sum <- summarise(group_by(tableZZ.dat3, CK_Cadmus_ID)
                            ,TotalBulbs = sum(Lamps))

tableZZ.merge1 <- left_join(rbsa.dat, tableZZ.cfl.sum)

## subset to only storage bulbs
tableZZ.storage <- tableZZ.dat3[which(tableZZ.dat3$Clean.Room == "Storage"),]
#summarise within site
tableZZ.storage.sum <- summarise(group_by(tableZZ.storage, CK_Cadmus_ID)
                                ,StorageBulbs = sum(Lamps))

tableZZ.merge2 <- left_join(tableZZ.merge1, tableZZ.storage.sum)
tableZZ.merge <- tableZZ.merge2[which(!is.na(tableZZ.merge2$TotalBulbs)),]
tableZZ.merge$StorageBulbs[which(is.na(tableZZ.merge$StorageBulbs))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
tableZZ.data <- weightedData(tableZZ.merge[-which(colnames(tableZZ.merge) %in% c("StorageBulbs"
                                                                              ,"TotalBulbs"))])
tableZZ.data <- left_join(tableZZ.data, tableZZ.merge[which(colnames(tableZZ.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"StorageBulbs"
                                                                                       ,"TotalBulbs"))])
tableZZ.data$count <- 1

#######################
# Weighted Analysis
#######################
tableZZ.final <- proportions_one_group(CustomerLevelData = tableZZ.data
                                      ,valueVariable    = 'StorageBulbs'
                                      ,groupingVariable = 'State'
                                      ,total.name       = 'Region'
                                      ,columnName       = 'Remove')

tableZZ.final.SF <- tableZZ.final[which(tableZZ.final$BuildingType == "Single Family")
                                ,-which(colnames(tableZZ.final) %in% c("BuildingType"))]
tableZZ.final.MH <- tableZZ.final[which(tableZZ.final$BuildingType == "Manufactured")
                                ,-which(colnames(tableZZ.final) %in% c("BuildingType"))]

exportTable(tableZZ.final.SF, "SF", "Table ZZ", weighted = TRUE)
exportTable(tableZZ.final.MH, "MH", "Table ZZ", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableZZ.final <- proportions_one_group(CustomerLevelData = tableZZ.data
                                      ,valueVariable    = 'StorageBulbs'
                                      ,groupingVariable = 'State'
                                      ,total.name       = 'Region'
                                      ,columnName       = 'Remove'
                                      ,weighted         = FALSE)

tableZZ.final.SF <- tableZZ.final[which(tableZZ.final$BuildingType == "Single Family")
                                ,-which(colnames(tableZZ.final) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"Total.Count"))]
tableZZ.final.MH <- tableZZ.final[which(tableZZ.final$BuildingType == "Manufactured")
                                ,-which(colnames(tableZZ.final) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"Total.Count"))]

exportTable(tableZZ.final.SF, "SF", "Table ZZ", weighted = FALSE)
exportTable(tableZZ.final.MH, "MH", "Table ZZ", weighted = FALSE)







#############################################################################################
#Item 78: AVERAGE LIGHTING POWER DENSITY (LPD) BY STATE (SF table 85, MH table 66)
#############################################################################################
item78.area1 <- summarise(group_by(rbsa.dat, CK_Cadmus_ID)
                          ,SiteArea = sum(Conditioned.Area, na.rm = T))
item78.area2 <- item78.area1[which(item78.area1$SiteArea %notin% c(0, 1, 4,"Unknown")),]

#subset to columns needed for analysis
item78.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"
                                                               ,"Clean.Wattage"
                                                               ,"Wattage.for.Calc"))]
item78.dat$count <- 1

#merge on area information
item78.merge <- left_join(item78.dat, item78.area2, by = "CK_Cadmus_ID")

item78.dat1 <- left_join(item78.merge, rbsa.dat, by = "CK_Cadmus_ID")

item78.dat2 <- item78.dat1[grep("SITE", item78.dat1$CK_SiteID),]

item78.dat3 <- item78.dat2[which(!(item78.dat2$Clean.Room %in% c("Storage"))),]
unique(item78.dat3$Wattage.for.Calc)
item78.dat4 <- item78.dat3[-grep("-|Unknown|unknown", item78.dat3$Wattage.for.Calc),]

item78.dat4$Total.Wattage <- as.numeric(as.character(item78.dat4$Fixture.Qty)) * 
  as.numeric(as.character(item78.dat4$LIGHTING_BulbsPerFixture)) * 
  as.numeric(as.character(item78.dat4$Wattage.for.Calc))

item78.dat5 <- item78.dat4[which(!(is.na(item78.dat4$Total.Wattage))),]


item78.dat6 <- summarise(group_by(item78.dat5, CK_Cadmus_ID, BuildingType, State)
                         ,Total.Wattage = sum(Total.Wattage, na.rm = T))

#merge on area information
item78.dat7 <- left_join(item78.dat6, item78.area1, by = c("CK_Cadmus_ID"))

item78.dat7$LPD <- item78.dat7$Total.Wattage / item78.dat7$SiteArea

item78.dat8 <- item78.dat7[which(!(is.na(item78.dat7$LPD))),]

item78.prep <- left_join(rbsa.dat, item78.dat8)
item78.prep1 <- item78.prep[which(!is.na(item78.prep$LPD)),]
item78.prep1 <- item78.prep1[which(item78.prep1$LPD != "Inf"),]

################################################
# Adding pop and sample sizes for weights
################################################
item78.data <- weightedData(item78.prep1[-which(colnames(item78.prep1) %in% c("Total.Wattage"
                                                                          ,"SiteArea"
                                                                          ,"LPD"))])
item78.data <- left_join(item78.data, item78.prep1[which(colnames(item78.prep1) %in% c("CK_Cadmus_ID"
                                                                                     ,"Total.Wattage"
                                                                                     ,"SiteArea"
                                                                                     ,"LPD"))])

#######################
# Weighted Analysis
#######################
item78.data$count <-1
item78.final <- mean_one_group(CustomerLevelData = item78.data
                               ,valueVariable    = 'LPD'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

# Export table
item78.final.SF <- item78.final[which(item78.final$BuildingType == "Single Family"),-1]
item78.final.MH <- item78.final[which(item78.final$BuildingType == "Manufactured"),-1]

exportTable(item78.final.SF, "SF", "Table 85", weighted = TRUE)
exportTable(item78.final.MH, "MH", "Table 66", weighted = TRUE)


################################
# Unweighted Analysis
################################
item78.final <- mean_one_group_unweighted(CustomerLevelData = item78.data
                                          ,valueVariable    = 'LPD'
                                          ,byVariable       = 'State'
                                          ,aggregateRow     = 'Region')

# Export table
item78.final.SF <- item78.final[which(item78.final$BuildingType == "Single Family"),-1]
item78.final.MH <- item78.final[which(item78.final$BuildingType == "Manufactured"),-1]

exportTable(item78.final.SF, "SF", "Table 85", weighted = FALSE)
exportTable(item78.final.MH, "MH", "Table 66", weighted = FALSE)






#############################################################################################
#Item 79: AVERAGE LIGHTING POWER DENSITY (LPD) BY ROOM TYPE (SF table 86, MH table 64)
#############################################################################################
item79.rooms <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID","Clean.Type","Area"))]
colnames(item79.rooms) <- c("CK_Cadmus_ID","Clean.Room","Area")

item79.area <- item79.rooms[which(!(item79.rooms$Area %in% c("0", "Unknown", NA, "-- Datapoint not asked for --"))),]
unique(item79.area$Area)
item79.area$Area <- as.numeric(as.character(item79.area$Area))

item79.area1 <- summarise(group_by(item79.area, CK_Cadmus_ID, Clean.Room)
                          ,SiteArea = sum(Area))

#subset to columns needed for analysis
item79.dat <- item78.dat5
item79.dat$Total.Wattage <- as.numeric(as.character(item79.dat$Fixture.Qty)) * 
  as.numeric(as.character(item79.dat$LIGHTING_BulbsPerFixture)) * 
  as.numeric(as.character(item79.dat$Clean.Wattage))

item79.dat0 <- summarise(group_by(item79.dat, CK_Cadmus_ID, BuildingType, State, Clean.Room)
                         ,Total.Wattage = sum(Total.Wattage))

#merge on area information
item79.dat1 <- left_join(item79.dat0, item79.area1, by = c("CK_Cadmus_ID", "Clean.Room"))

item79.dat1$LPD <- item79.dat1$Total.Wattage / item79.dat1$SiteArea

item79.dat2 <- item79.dat1[which(!(is.na(item79.dat1$LPD))),]

item79.prep <- left_join(rbsa.dat, item79.dat2)
item79.prep1 <- item79.prep[which(!is.na(item79.prep$LPD)),]
length(unique(item79.prep1$CK_Cadmus_ID))

################################################
# Adding pop and sample sizes for weights
################################################
item79.data <- weightedData(item79.prep1[-which(colnames(item79.prep1) %in% c("Total.Wattage"
                                                                              ,"SiteArea"
                                                                              ,"LPD"
                                                                              ,"Clean.Room"))])
item79.data <- left_join(item79.data, item79.prep1[which(colnames(item79.prep1) %in% c("CK_Cadmus_ID"
                                                                                       ,"Total.Wattage"
                                                                                       ,"SiteArea"
                                                                                       ,"LPD"
                                                                                       ,"Clean.Room"))])

#######################
# Weighted Analysis
#######################
item79.data$count <-1
item79.final <- mean_one_group(CustomerLevelData = item79.data
                               ,valueVariable    = 'LPD'
                               ,byVariable       = 'Clean.Room'
                               ,aggregateRow     = 'All Room Types')

# Export table
item79.final.SF <- item79.final[which(item79.final$BuildingType == "Single Family"),-1]
item79.final.MH <- item79.final[which(item79.final$BuildingType == "Manufactured"),-1]

exportTable(item79.final.SF, "SF", "Table 86", weighted = TRUE)
exportTable(item79.final.MH, "MH", "Table 64", weighted = TRUE)


################################
# Unweighted Analysis
################################
item79.final <- mean_one_group_unweighted(CustomerLevelData = item79.data
                                          ,valueVariable    = 'LPD'
                                          ,byVariable       = 'Clean.Room'
                                          ,aggregateRow     = 'All Room Types')

# Export table
item79.final.SF <- item79.final[which(item79.final$BuildingType == "Single Family"),-1]
item79.final.MH <- item79.final[which(item79.final$BuildingType == "Manufactured"),-1]

exportTable(item79.final.SF, "SF", "Table 86", weighted = FALSE)
exportTable(item79.final.MH, "MH", "Table 64", weighted = FALSE)


