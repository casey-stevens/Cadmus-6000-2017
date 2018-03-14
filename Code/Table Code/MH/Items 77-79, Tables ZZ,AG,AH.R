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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
rbsa.dat <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis -- Item 77 and table ZZ
# lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 3)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))


#Read in data for analysis -- Item 79
# rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
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
                               ,total.name       = 'Region')

item77.final.SF <- item77.final[which(item77.final$BuildingType == "Single Family")
                                ,-which(colnames(item77.final) %in% c("BuildingType"))]
item77.final.MH <- item77.final[which(item77.final$BuildingType == "Manufactured")
                                ,-which(colnames(item77.final) %in% c("BuildingType"))]

# exportTable(item77.final.SF, "SF", "Table 84", weighted = TRUE)
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

# exportTable(item77.final.SF, "SF", "Table 84", weighted = FALSE)
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

tableZZ.led.sum <- summarise(group_by(tableZZ.dat3, CK_Cadmus_ID)
                            ,TotalBulbs = sum(Lamps))

tableZZ.merge1 <- left_join(rbsa.dat, tableZZ.led.sum)

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

# exportTable(tableZZ.final.SF, "SF", "Table ZZ", weighted = TRUE)
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

# exportTable(tableZZ.final.SF, "SF", "Table ZZ", weighted = FALSE)
exportTable(tableZZ.final.MH, "MH", "Table ZZ", weighted = FALSE)







#############################################################################################
#Item 78: AVERAGE LIGHTING POWER DENSITY (LPD) BY STATE (SF table 85, MH table 66)
#############################################################################################
item78.area1 <- summarise(group_by(rbsa.dat, CK_Cadmus_ID)
                          ,SiteArea = sum(Conditioned.Area, na.rm = T))
item78.area1$SiteArea <- as.numeric(as.character(item78.area1$SiteArea))
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


item78.dat6 <- summarise(group_by(item78.dat5, CK_Cadmus_ID)
                         ,Total.Wattage = sum(Total.Wattage, na.rm = T))

#merge on area information
item78.dat7 <- left_join(item78.dat6, item78.area2, by = c("CK_Cadmus_ID"))

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

# exportTable(item78.final.SF, "SF", "Table 85", weighted = TRUE)
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

# exportTable(item78.final.SF, "SF", "Table 85", weighted = FALSE)
exportTable(item78.final.MH, "MH", "Table 66", weighted = FALSE)






#############################################################################################
#Item 79: AVERAGE LIGHTING POWER DENSITY (LPD) BY ROOM TYPE (SF table 86, MH table 64)
#############################################################################################
item79.rooms <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID","Clean.Type","Area"))]
colnames(item79.rooms) <- c("CK_Cadmus_ID","Clean.Room","Area")

item79.area <- item79.rooms[which(!(item79.rooms$Area %in% c("0", "Unknown", NA, "Datapoint not asked for"))),]
unique(item79.area$Area)
item79.area$Area <- as.numeric(as.character(item79.area$Area))

item79.area$Clean.Room[which(item79.area$Clean.Room %in% c("Attic"
                                                           ,"Basement"
                                                           ,"Crawlspace"
                                                           ,"Crawl Space"
                                                           ,"Mechanical"
                                                           ,"Grow Room"))] <- "Other"


item79.area1 <- summarise(group_by(item79.area, CK_Cadmus_ID, Clean.Room)
                          ,SiteArea = sum(Area))

#subset to columns needed for analysis
item79.dat <- item78.dat5
item79.dat$Total.Wattage <- as.numeric(as.character(item79.dat$Fixture.Qty)) * 
  as.numeric(as.character(item79.dat$LIGHTING_BulbsPerFixture)) * 
  as.numeric(as.character(item79.dat$Clean.Wattage))

item79.dat0 <- item79.dat[which(!(item79.dat$Clean.Room %in% c("Storage"))),]
item79.sum <- summarise(group_by(item79.dat, CK_Cadmus_ID, BuildingType, State, Clean.Room)
                         ,Total.Wattage = sum(Total.Wattage))

#merge on area information
item79.dat1 <- left_join(item79.sum, item79.area1, by = c("CK_Cadmus_ID", "Clean.Room"))

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


unique(item79.final$Clean.Room)
rowOrder <- c("Bathroom"
              ,"Bedroom"
              ,"Closet"
              ,"Dining Room"
              ,"Family Room"
              ,"Garage"
              ,"Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Living Room"
              ,"Office"
              ,"Other"
              ,"All Room Types")
item79.final <- item79.final %>% mutate(Clean.Room = factor(Clean.Room, levels = rowOrder)) %>% arrange(Clean.Room)  
item79.final <- data.frame(item79.final)


# Export table
item79.final.SF <- item79.final[which(item79.final$BuildingType == "Single Family"),-1]
item79.final.MH <- item79.final[which(item79.final$BuildingType == "Manufactured"),-1]

# exportTable(item79.final.SF, "SF", "Table 86", weighted = TRUE)
exportTable(item79.final.MH, "MH", "Table 64", weighted = TRUE)


################################
# Unweighted Analysis
################################
item79.final <- mean_one_group_unweighted(CustomerLevelData = item79.data
                                          ,valueVariable    = 'LPD'
                                          ,byVariable       = 'Clean.Room'
                                          ,aggregateRow     = 'All Room Types')


unique(item79.final$Clean.Room)
rowOrder <- c("Bathroom"
              ,"Bedroom"
              ,"Closet"
              ,"Dining Room"
              ,"Family Room"
              ,"Garage"
              ,"Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Living Room"
              ,"Office"
              ,"Other"
              ,"All Room Types")
item79.final <- item79.final %>% mutate(Clean.Room = factor(Clean.Room, levels = rowOrder)) %>% arrange(Clean.Room)  
item79.final <- data.frame(item79.final)


# Export table
item79.final.SF <- item79.final[which(item79.final$BuildingType == "Single Family"),-1]
item79.final.MH <- item79.final[which(item79.final$BuildingType == "Manufactured"),-1]

# exportTable(item79.final.SF, "SF", "Table 86", weighted = FALSE)
exportTable(item79.final.MH, "MH", "Table 64", weighted = FALSE)






#############################################################################################
#Table AG: Distribution of All stored lamps by lamp type and state
#############################################################################################
#subset to columns needed for analysis
tableAG.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"CK_SiteID"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Room"))]
tableAG.dat$count <- 1

tableAG.dat0 <- left_join(rbsa.dat, tableAG.dat, by = "CK_Cadmus_ID")

tableAG.dat1 <- tableAG.dat0[which(!is.na(tableAG.dat0$Lamp.Category)),]

tableAG.dat2 <- tableAG.dat1[grep("SITE", tableAG.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
tableAG.dat2$Fixture.Qty <- as.numeric(as.character(tableAG.dat2$Fixture.Qty))
tableAG.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(tableAG.dat2$LIGHTING_BulbsPerFixture))

tableAG.dat2$Lamps <- tableAG.dat2$Fixture.Qty * tableAG.dat2$LIGHTING_BulbsPerFixture
unique(tableAG.dat2$Lamps)

tableAG.dat3 <- tableAG.dat2[which(!(is.na(tableAG.dat2$Lamps))),]

tableAG.led.sum <- summarise(group_by(tableAG.dat3, CK_Cadmus_ID, Lamp.Category)
                             ,TotalBulbs = sum(Lamps))

## subset to only storage bulbs
tableAG.storage <- tableAG.dat3[which(tableAG.dat3$Clean.Room == "Storage"),]
#summarise within site
tableAG.storage.sum <- summarise(group_by(tableAG.storage, CK_Cadmus_ID, Lamp.Category)
                                 ,StorageBulbs = sum(Lamps))
length(unique(tableAG.storage.sum$CK_Cadmus_ID))


tableAG.merge1 <- left_join(tableAG.led.sum, tableAG.storage.sum)

tableAG.cast1 <- dcast(setDT(tableAG.merge1)
                      ,formula = CK_Cadmus_ID ~ Lamp.Category
                      ,value.var = c("StorageBulbs"))
tableAG.cast1[is.na(tableAG.cast1),] <- 0

tableAG.melt1 <- melt(tableAG.cast1, id.vars = "CK_Cadmus_ID")
names(tableAG.melt1) <- c("CK_Cadmus_ID", "Lamp.Category", "StorageBulbs")

tableAG.cast2 <- dcast(setDT(tableAG.merge1)
                       ,formula = CK_Cadmus_ID ~ Lamp.Category
                       ,value.var = c("TotalBulbs"))
tableAG.cast2[is.na(tableAG.cast2),] <- 0

tableAG.melt2 <- melt(tableAG.cast2, id.vars = "CK_Cadmus_ID")
names(tableAG.melt2) <- c("CK_Cadmus_ID", "Lamp.Category", "TotalBulbs")

tableAG.merge2 <- left_join(tableAG.melt1, tableAG.melt2)
tableAG.merge  <- left_join(rbsa.dat, tableAG.merge2)
tableAG.merge$StorageBulbs[which(is.na(tableAG.merge$StorageBulbs))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
tableAG.data <- weightedData(tableAG.merge[-which(colnames(tableAG.merge) %in% c("StorageBulbs"
                                                                                 ,"TotalBulbs"
                                                                                 ,"Lamp.Category"))])
tableAG.data <- left_join(tableAG.data, tableAG.merge[which(colnames(tableAG.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"StorageBulbs"
                                                                                           ,"TotalBulbs"
                                                                                           ,"Lamp.Category"))])
tableAG.data$count <- 1
stopifnot(nrow(tableAG.data) == nrow(tableAG.merge))
#######################
# Weighted Analysis
#######################
tableAG.summary <- proportionRowsAndColumns1(CustomerLevelData = tableAG.data
                                             ,valueVariable = "StorageBulbs"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Lamp.Category"
                                             ,aggregateColumnName = "Region")
# tableAG.summary <- tableAG.summary[which(tableAG.summary$Lamp.Category != "Total"),]



tableAG.cast <- dcast(setDT(tableAG.summary)
                      ,formula = BuildingType + Lamp.Category ~ State
                      ,value.var = c("w.percent","w.SE","count","n","N","EB"))

tableAG.final <- data.frame("BuildingType"   = tableAG.cast$BuildingType
                            ,"Lamp.Category" = tableAG.cast$Lamp.Category
                            ,"ID"             = tableAG.cast$w.percent_ID
                            ,"ID.SE"          = tableAG.cast$w.SE_ID
                            ,"ID.n"           = tableAG.cast$n_ID
                            ,"MT"             = tableAG.cast$w.percent_MT
                            ,"MT.SE"          = tableAG.cast$w.SE_MT
                            ,"MT.n"           = tableAG.cast$n_MT
                            ,"OR"             = tableAG.cast$w.percent_OR
                            ,"OR.SE"          = tableAG.cast$w.SE_OR
                            ,"OR.n"           = tableAG.cast$n_OR
                            ,"WA"             = tableAG.cast$w.percent_WA
                            ,"WA.SE"          = tableAG.cast$w.SE_WA
                            ,"WA.n"           = tableAG.cast$n_WA
                            ,"Region"         = tableAG.cast$w.percent_Region
                            ,"Region.SE"      = tableAG.cast$w.SE_Region
                            ,"Region.n"       = tableAG.cast$n_Region
                            ,"ID.EB"          = tableAG.cast$EB_ID
                            ,"MT.EB"          = tableAG.cast$EB_MT
                            ,"OR.EB"          = tableAG.cast$EB_OR
                            ,"WA.EB"          = tableAG.cast$EB_WA
                            ,"Region.EB"      = tableAG.cast$EB_Region
)

unique(tableAG.final$Lamp.Category)
rowOrder <- c("Compact Fluorescent"
              ,"Halogen"
              ,"Incandescent"
              ,"Incandescent / Halogen"
              ,"Light Emitting Diode"
              ,"Linear Fluorescent"
              ,"Other"
              ,"Unknown"
              ,"Total")
tableAG.final <- tableAG.final %>% mutate(Lamp.Category = factor(Lamp.Category, levels = rowOrder)) %>% arrange(Lamp.Category)  
tableAG.final <- data.frame(tableAG.final)


tableAG.final.SF <- tableAG.final[which(tableAG.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableAG.final) %in% c("BuildingType"))]
tableAG.final.MH <- tableAG.final[which(tableAG.final$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAG.final) %in% c("BuildingType"))]

# exportTable(tableAG.final.SF, "SF", "Table AG", weighted = TRUE)
exportTable(tableAG.final.MH, "MH", "Table AG", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableAG.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableAG.data
                                             ,valueVariable = "StorageBulbs"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Lamp.Category"
                                             ,aggregateColumnName = "Region")
# tableAG.summary <- tableAG.summary[which(tableAG.summary$Lamp.Category != "Total"),]

tableAG.cast <- dcast(setDT(tableAG.summary)
                      ,formula = BuildingType + Lamp.Category ~ State
                      ,value.var = c("Percent","SE","Count","n"))

tableAG.table <- data.frame("BuildingType"    = tableAG.cast$BuildingType
                            ,"Lamp.Category"  = tableAG.cast$Lamp.Category
                            ,"ID"             = tableAG.cast$Percent_ID
                            ,"ID.SE"          = tableAG.cast$SE_ID
                            ,"ID.n"           = tableAG.cast$n_ID
                            ,"MT"             = tableAG.cast$Percent_MT
                            ,"MT.SE"          = tableAG.cast$SE_MT
                            ,"MT.n"           = tableAG.cast$n_MT
                            ,"OR"             = tableAG.cast$Percent_OR
                            ,"OR.SE"          = tableAG.cast$SE_OR
                            ,"OR.n"           = tableAG.cast$n_OR
                            ,"WA"             = tableAG.cast$Percent_WA
                            ,"WA.SE"          = tableAG.cast$SE_WA
                            ,"WA.n"           = tableAG.cast$n_WA
                            ,"Region"         = tableAG.cast$Percent_Region
                            ,"Region.SE"      = tableAG.cast$SE_Region
                            ,"Region.n"       = tableAG.cast$n_Region
)

unique(tableAG.final$Lamp.Category)
rowOrder <- c("Compact Fluorescent"
              ,"Halogen"
              ,"Incandescent"
              ,"Incandescent / Halogen"
              ,"Light Emitting Diode"
              ,"Linear Fluorescent"
              ,"Other"
              ,"Unknown"
              ,"Total")
tableAG.final <- tableAG.final %>% mutate(Lamp.Category = factor(Lamp.Category, levels = rowOrder)) %>% arrange(Lamp.Category)  
tableAG.final <- data.frame(tableAG.final)

tableAG.final.SF <- tableAG.table[which(tableAG.table$BuildingType == "Single Family")
                                  ,which(colnames(tableAG.table) %notin% c("BuildingType"))]
tableAG.final.MH <- tableAG.table[which(tableAG.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAG.table) %in% c("BuildingType"))]

# exportTable(tableAG.final.SF, "SF", "Table AG", weighted = FALSE)
exportTable(tableAG.final.MH, "MH", "Table AG", weighted = FALSE)



# #######################################################
# # AG Multifamily
# #######################################################
# #######################
# # Weighted Analysis
# #######################
# tableAG.MF.data <- tableAG.data[which(tableAG.data$BuildingType == "Multifamily"),]
# tableAG.MF.data <- tableAG.MF.data[grep("site",tableAG.MF.data$CK_Building_ID,ignore.case = T),]
# tableAG.MF.data$Count <- 1
# 
# tableAG.MF.summary <- proportionRowsAndColumns1(CustomerLevelData = tableAG.MF.data
#                                              ,valueVariable = "StorageBulbs"
#                                              ,columnVariable = "HomeType"
#                                              ,rowVariable = "Lamp.Category"
#                                              ,aggregateColumnName = "All Sizes")
# tableAG.MF.summary <- tableAG.MF.summary[which(tableAG.MF.summary$HomeType != "All Sizes"),]
# 
# tableAG.all.sizes <- proportions_one_group(CustomerLevelData = tableAG.MF.data
#                                            ,valueVariable = "StorageBulbs"
#                                            ,groupingVariable = "Lamp.Category"
#                                            ,total.name = "All Sizes"
#                                            ,columnName = "HomeType"
#                                            ,weighted = TRUE
#                                            ,two.prop.total = TRUE
# )
# 
# tableAG.MF.merge <- rbind.data.frame(tableAG.MF.summary, tableAG.all.sizes, stringsAsFactors = F)
# 
# tableAG.MF.cast <- data.frame(dcast(setDT(tableAG.MF.merge)
#                                     ,formula = BuildingType + Lamp.Category ~ HomeType
#                                     ,value.var = c("w.percent","w.SE","count","n","N","EB")))
# names(tableAG.MF.cast)
# tableAG.MF.final <- data.frame("BuildingType"       = tableAG.MF.cast$BuildingType
#                                ,"Lamp.Category"     = tableAG.MF.cast$Lamp.Category
#                                ,"Percent.Low.Rise"  = tableAG.MF.cast$w.percent_Apartment.Building..3.or.fewer.floors.
#                                ,"SE.Low.Rise"       = tableAG.MF.cast$w.SE_Apartment.Building..3.or.fewer.floors.
#                                ,"n.Low.Rise"        = tableAG.MF.cast$n_Apartment.Building..3.or.fewer.floors.
#                                ,"Percent.Mid.Rise"  = tableAG.MF.cast$w.percent_Apartment.Building..4.to.6.floors.
#                                ,"SE.Mid.Rise"       = tableAG.MF.cast$w.SE_Apartment.Building..4.to.6.floors.
#                                ,"n.Mid.Rise"        = tableAG.MF.cast$n_Apartment.Building..4.to.6.floors.
#                                ,"Percent.High.Rise" = tableAG.MF.cast$w.percent_Apartment.Building..More.than.6.floors.
#                                ,"SE.High.Rise"      = tableAG.MF.cast$w.SE_Apartment.Building..More.than.6.floors.
#                                ,"n.High.Rise"       = tableAG.MF.cast$n_Apartment.Building..More.than.6.floors.
#                                ,"Percent.All.Sizes" = tableAG.MF.cast$w.percent_All.Sizes
#                                ,"SE.All.Sizes"      = tableAG.MF.cast$w.SE_All.Sizes
#                                ,"n.All.Sizes"       = tableAG.MF.cast$n_All.Sizes
#                                ,"EB.Low.Rise"       = tableAG.MF.cast$EB_Apartment.Building..3.or.fewer.floors.
#                                ,"EB.Mid.Rise"       = tableAG.MF.cast$EB_Apartment.Building..4.to.6.floors.
#                                ,"EB.High.Rise"      = tableAG.MF.cast$EB_Apartment.Building..More.than.6.floors.
#                                ,"EB.All.Sizes"      = tableAG.MF.cast$EB_All.Sizes
# )
# 
# unique(tableAG.MF.final$Lamp.Category)
# rowOrder <- c("Compact Fluorescent"
#               ,"Halogen"
#               ,"Incandescent"
#               ,"Incandescent / Halogen"
#               ,"Light Emitting Diode"
#               ,"Linear Fluorescent"
#               ,"Other"
#               ,"Unknown"
#               ,"Total")
# tableAG.MF.final <- tableAG.MF.final %>% mutate(Lamp.Category = factor(Lamp.Category, levels = rowOrder)) %>% arrange(Lamp.Category)  
# tableAG.MF.final <- data.frame(tableAG.MF.final)
# 
# tableAG.MF.final.MF <- tableAG.MF.final[which(tableAG.MF.final$BuildingType == "Multifamily")
#                                   ,-which(colnames(tableAG.MF.final) %in% c("BuildingType"))]
# tableAG.MF.final.MF$n.Low.Rise <- min(tableAG.MF.final.MF$n.Low.Rise)
# tableAG.MF.final.MF$n.Mid.Rise <- min(tableAG.MF.final.MF$n.Mid.Rise)
# tableAG.MF.final.MF$n.High.Rise <- min(tableAG.MF.final.MF$n.High.Rise)
# tableAG.MF.final.MF$n.All.Sizes <- min(tableAG.MF.final.MF$n.All.Sizes)
# 
# # exportTable(tableAG.MF.final.MF, "MF", "Table AG", weighted = TRUE)
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# tableAG.MF.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableAG.MF.data
#                                                      ,valueVariable = "StorageBulbs"
#                                                      ,columnVariable = "HomeType"
#                                                      ,rowVariable = "Lamp.Category"
#                                                      ,aggregateColumnName = "All Sizes")
# # tableAG.MF.summary <- tableAG.MF.summary[which(tableAG.MF.summary$Lamp.Category != "Total"),]
# 
# tableAG.MF.cast <- data.frame(dcast(setDT(tableAG.MF.summary)
#                                     ,formula = BuildingType + Lamp.Category ~ HomeType
#                                     ,value.var = c("Percent","SE","Count","n")))
# 
# tableAG.MF.final <- data.frame("BuildingType"       = tableAG.MF.cast$BuildingType
#                                ,"Lamp.Category"     = tableAG.MF.cast$Lamp.Category
#                                ,"Percent.Low.Rise"  = tableAG.MF.cast$Percent_Apartment.Building..3.or.fewer.floors.
#                                ,"SE.Low.Rise"       = tableAG.MF.cast$SE_Apartment.Building..3.or.fewer.floors.
#                                ,"n.Low.Rise"        = tableAG.MF.cast$n_Apartment.Building..3.or.fewer.floors.
#                                ,"Percent.Mid.Rise"  = tableAG.MF.cast$Percent_Apartment.Building..4.to.6.floors.
#                                ,"SE.Mid.Rise"       = tableAG.MF.cast$SE_Apartment.Building..4.to.6.floors.
#                                ,"n.Mid.Rise"        = tableAG.MF.cast$n_Apartment.Building..4.to.6.floors.
#                                ,"Percent.High.Rise" = tableAG.MF.cast$Percent_Apartment.Building..More.than.6.floors.
#                                ,"SE.High.Rise"      = tableAG.MF.cast$SE_Apartment.Building..More.than.6.floors.
#                                ,"n.High.Rise"       = tableAG.MF.cast$n_Apartment.Building..More.than.6.floors.
#                                ,"Percent.All.Sizes" = tableAG.MF.cast$Percent_All.Sizes
#                                ,"SE.All.Sizes"      = tableAG.MF.cast$SE_All.Sizes
#                                ,"n.All.Sizes"       = tableAG.MF.cast$n_All.Sizes
# )
# 
# unique(tableAG.MF.final$Lamp.Category)
# rowOrder <- c("Compact Fluorescent"
#               ,"Halogen"
#               ,"Incandescent"
#               ,"Incandescent / Halogen"
#               ,"Light Emitting Diode"
#               ,"Linear Fluorescent"
#               ,"Other"
#               ,"Unknown"
#               ,"Total")
# tableAG.MF.final <- tableAG.MF.final %>% mutate(Lamp.Category = factor(Lamp.Category, levels = rowOrder)) %>% arrange(Lamp.Category)  
# tableAG.MF.final <- data.frame(tableAG.MF.final)
# 
# tableAG.MF.final.MF <- tableAG.MF.final[which(tableAG.MF.final$BuildingType == "Multifamily")
#                                         ,-which(colnames(tableAG.MF.final) %in% c("BuildingType"))]
# 
# # exportTable(tableAG.MF.final.MF, "MF", "Table AG", weighted = FALSE)






#############################################################################################
#Table AH: AVERAGE household wattage per lamp BY STATE
#############################################################################################
#subset to columns needed for analysis
tableAH.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"
                                                               ,"Clean.Wattage"
                                                               ,"Wattage.for.Calc"))]
tableAH.dat$count <- 1

tableAH.dat1 <- left_join(tableAH.dat, rbsa.dat, by = "CK_Cadmus_ID")

tableAH.dat2 <- tableAH.dat1[grep("SITE", tableAH.dat1$CK_SiteID),]

tableAH.dat3 <- tableAH.dat2[which(!(tableAH.dat2$Clean.Room %in% c("Storage"))),]
unique(tableAH.dat3$Wattage.for.Calc)
tableAH.dat4 <- tableAH.dat3[-grep("-|Unknown|unknown", tableAH.dat3$Wattage.for.Calc),]

tableAH.dat4$Total.Wattage <- as.numeric(as.character(tableAH.dat4$Fixture.Qty)) * 
  as.numeric(as.character(tableAH.dat4$LIGHTING_BulbsPerFixture)) * 
  as.numeric(as.character(tableAH.dat4$Wattage.for.Calc))

tableAH.dat5 <- tableAH.dat4[which(!(is.na(tableAH.dat4$Wattage.for.Calc))),]
tableAH.dat5$Wattage.for.Calc <- as.numeric(as.character(tableAH.dat5$Wattage.for.Calc))

tableAH.dat6 <- summarise(group_by(tableAH.dat5, CK_Cadmus_ID)
                         ,Wattage.per.bulb = mean(Wattage.for.Calc, na.rm = T))

tableAH.prep <- left_join(rbsa.dat, tableAH.dat6)
tableAH.prep1 <- tableAH.prep[which(!is.na(tableAH.prep$Wattage.per.bulb)),]
tableAH.prep1 <- tableAH.prep1[which(tableAH.prep1$Wattage.per.bulb != "Inf"),]

################################################
# Adding pop and sample sizes for weights
################################################
tableAH.data <- weightedData(tableAH.prep1[-which(colnames(tableAH.prep1) %in% c("Wattage.per.bulb"))])
tableAH.data <- left_join(tableAH.data, tableAH.prep1[which(colnames(tableAH.prep1) %in% c("CK_Cadmus_ID"
                                                                                       ,"Wattage.per.bulb"))])
stopifnot(nrow(tableAH.data) == nrow(tableAH.data))
#######################
# Weighted Analysis
#######################
tableAH.data$count <-1
tableAH.final <- mean_one_group(CustomerLevelData = tableAH.data
                               ,valueVariable    = 'Wattage.per.bulb'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

# Export table
tableAH.final.SF <- tableAH.final[which(tableAH.final$BuildingType == "Single Family"),-1]
tableAH.final.MH <- tableAH.final[which(tableAH.final$BuildingType == "Manufactured"),-1]

# exportTable(tableAH.final.SF, "SF", "Table AH", weighted = TRUE)
exportTable(tableAH.final.MH, "MH", "Table AH", weighted = TRUE)

# #######################
# # MULTIFAMILY
# #######################
# tableAH.final.MF <- mean_one_group(CustomerLevelData = tableAH.data
#                                 ,valueVariable    = 'Wattage.per.bulb'
#                                 ,byVariable       = 'HomeType'
#                                 ,aggregateRow     = 'All Types')
# 
# # Export table
# tableAH.final.MF <- tableAH.final.MF[which(tableAH.final.MF$BuildingType == "Multifamily"),-1]
# # exportTable(tableAH.final.MF, "MF", "Table AH", weighted = TRUE)





################################
# Unweighted Analysis
################################
tableAH.final <- mean_one_group_unweighted(CustomerLevelData = tableAH.data
                                          ,valueVariable    = 'Wattage.per.bulb'
                                          ,byVariable       = 'State'
                                          ,aggregateRow     = 'Region')

# Export table
tableAH.final.SF <- tableAH.final[which(tableAH.final$BuildingType == "Single Family"),-1]
tableAH.final.MH <- tableAH.final[which(tableAH.final$BuildingType == "Manufactured"),-1]

# exportTable(tableAH.final.SF, "SF", "Table AH", weighted = FALSE)
exportTable(tableAH.final.MH, "MH", "Table AH", weighted = FALSE)

# #######################
# # MULTIFAMILY
# #######################
# tableAH.final.MF <- mean_one_group_unweighted(CustomerLevelData = tableAH.data
#                                    ,valueVariable    = 'Wattage.per.bulb'
#                                    ,byVariable       = 'HomeType'
#                                    ,aggregateRow     = 'All Types')
# 
# # Export table
# tableAH.final.MF <- tableAH.final.MF[which(tableAH.final.MF$BuildingType == "Multifamily"),-1]
# # exportTable(tableAH.final.MF, "MF", "Table AH", weighted = FALSE)


