#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
<<<<<<< HEAD
rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)


##  Create "Not In" operator
"%notin%" <- Negate("%in%")

=======
rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)
>>>>>>> 9e4a9af84d58d121769174f0e3f7be66577950a9

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")
<<<<<<< HEAD

=======
>>>>>>> 9e4a9af84d58d121769174f0e3f7be66577950a9

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis -- Item 77
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export))
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))

#Read in data for analysis -- Item 78
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))


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

<<<<<<< HEAD
item77.dat1 <- left_join(rbsa.dat, item77.dat0, by = "CK_Cadmus_ID")

item77.dat2 <- item77.dat1[grep("SITE", item77.dat1$CK_SiteID),]
=======
item77.dat1 <- left_join( rbsa.dat, item77.dat0, by = "CK_Cadmus_ID")

# item77.dat2 <- item77.dat1[-grep("BLDG", item77.dat1$CK_SiteID),]  # old version, caused issue because grep was integer(0)
item77.dat2 <- item77.dat1
>>>>>>> 9e4a9af84d58d121769174f0e3f7be66577950a9

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
<<<<<<< HEAD
item77.storage <- item77.dat3[which(item77.dat3$Clean.Room == "Storage"),]
#summarise within site
item77.storage.sum <- summarise(group_by(item77.storage, CK_Cadmus_ID)
                                 ,StorageBulbs = sum(Lamps))
=======
item77.dat4 <- item77.dat3[which(item77.dat3$Clean.Room == "Storage"),]


# Weighting function
item77.data <- weightedData(item77.dat4[-which(colnames(item77.dat4) %in% c("CK_SiteID"
                                                                            ,"Clean.Room"
                                                                            ,"Fixture.Qty"
                                                                            ,"LIGHTING_BulbsPerFixture"
                                                                            ,"Lamp.Category"
                                                                            ,"count"
                                                                            ,"Lamps"))])
item77.data <- left_join(item77.data, item77.dat4[which(colnames(item77.dat4) %in% c("CK_Cadmus_ID"
                                                                                     ,"CK_SiteID"
                                                                                     ,"Clean.Room"
                                                                                     ,"Fixture.Qty"
                                                                                     ,"LIGHTING_BulbsPerFixture"
                                                                                     ,"Lamp.Category"
                                                                                     ,"count"
                                                                                     ,"Lamps"))])

# Apply analysis
item77.final <- proportions_one_group(CustomerLevelData  = item77.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , columnName       = "CFLs")

# Export table
item77.final.SF <- item77.final[which(item77.final$BuildingType == "Single Family"),-1]
item77.final.MH <- item77.final[which(item77.final$BuildingType == "Manufactured"),-1]

exportTable(item77.final.SF, "SF", "Table 84")
exportTable(item77.final.MH, "SF", "Table 63")


# OLD CODE #
# 
# #summarise within site
# item77.storage.sum <- summarise(group_by(item77.storage, BuildingType, State)
#                                  ,StorageBulbs = sum(Lamps))
# #summarise by state
# item77.storage.state1 <- summarise(group_by(item77.storage.sum, BuildingType, State)
#                                    ,Storage = mean(StorageBulbs))
# #summarise by state
# item77.storage.region1 <- summarise(group_by(item77.storage.sum, BuildingType)
#                                    ,State = "Region"
#                                    ,Storage = mean(StorageBulbs))
# 
# #row bind state and region level information for storage lamps
# item77.storage.total <- rbind.data.frame(item77.storage.state1, item77.storage.region1, stringsAsFactors = F)
# 
# 
# 
# ## use full data (with and without storage bulbs)
# item77.allBulbs <- item77.dat3
# #summarise within site
# item77.all.sum <- summarise(group_by(item77.allBulbs, BuildingType, State)
#                                 ,SampleSize = length(unique(CK_Cadmus_ID))
#                                 ,allBulbs = sum(Lamps))
# item77.regionSS <- summarise(group_by(item77.allBulbs, BuildingType)
#                                 ,State = "Region"
#                                 ,SampleSize = length(unique(CK_Cadmus_ID))
#                                 ,allBulbs = sum(Lamps))
# 
# #Get Sample Size information
# item77.SampleSize <- rbind.data.frame(item77.all.sum, item77.regionSS, stringsAsFactors = F)
# item77.SampleSize1 <- item77.SampleSize[which(colnames(item77.SampleSize) %in% c("BuildingType","State","SampleSize"))]
# 
# #summarise by state
# item77.all.state1 <- summarise(group_by(item77.all.sum, BuildingType, State)
#                                    ,Total = mean(allBulbs))
# #summarise across states
# item77.all.region1 <- summarise(group_by(item77.all.sum, BuildingType)
#                                     ,State = "Region"
#                                     ,Total = mean(allBulbs))
# #row bind state and region level information for all lamp categories
# item77.all.total <- rbind.data.frame(item77.all.state1, item77.all.region1, stringsAsFactors = F)
# 
# #left join together storage and combined results
# item77.final <- left_join(item77.all.total, item77.storage.total, by = c("BuildingType","State"))
# 
# # left join on sample size information
# item77.final1 <- left_join(item77.final, item77.SampleSize1, by = c("BuildingType","State"))
# 
# #Make NA storage to zero
# item77.final1$Storage[which(is.na(item77.final1$Storage))] <- 0
# #calculate percent and SE(percent)
# item77.final1$Percent <- item77.final1$Storage / item77.final1$Total
# item77.final1$SE      <- sqrt(item77.final1$Percent * (1 - item77.final1$Percent) / item77.final1$SampleSize)
# #subset to only wanted columns
# item77.table <- data.frame("BuildingType" = item77.final1$BuildingType
#                            ,"State" = item77.final1$State
#                            ,"Percent" = item77.final1$Percent
#                            ,"SE" = item77.final1$SE
#                            ,"SampleSize" = item77.final1$SampleSize)
# #subset to only relevant building types
# item77.table1 <- item77.table[which(item77.table$BuildingType %in% c("Single Family", "Manufactured")),]
# 
# 

>>>>>>> 9e4a9af84d58d121769174f0e3f7be66577950a9

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
                                ,-which(colnames(item77.final) %in% c("BuildingType"))]
item77.final.MH <- item77.final[which(item77.final$BuildingType == "Manufactured")
                                ,-which(colnames(item77.final) %in% c("BuildingType"))]

exportTable(item77.final.SF, "SF", "Table 84", weighted = FALSE)
exportTable(item77.final.MH, "MH", "Table 63", weighted = FALSE)



#############################################################################################
#Item 78: AVERAGE LIGHTING POWER DENSITY (LPD) BY STATE (SF table 85, MH table 66)
#############################################################################################

item78.envelope <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID","ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]
colnames(item78.envelope) <- c("CK_Cadmus_ID","Area")

item78.area <- item78.envelope[which(!(item78.envelope$Area %in% c("0", "Unknown", NA))),]
unique(item78.area$Area)
item78.area$Area <- as.numeric(as.character(item78.area$Area))

item78.area1 <- summarise(group_by(item78.area, CK_Cadmus_ID)
                          ,SiteArea = sum(Area))

#subset to columns needed for analysis
item78.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"
                                                               ,"Clean.Wattage"))]
item78.dat$count <- 1

#merge on area information
item78.merge <- left_join(item78.dat, item78.area1, by = "CK_Cadmus_ID")

item78.dat1 <- left_join(rbsa.dat, item78.merge, by = "CK_Cadmus_ID")

#item78.dat2 <- item78.dat1[-grep("BLDG", item78.dat1$CK_SiteID),]  # old version, caused issue because grep was integer(0)
item78.dat2 <- item78.dat1

item78.dat3 <- item78.dat2[which(!(item78.dat2$Clean.Room %in% c("Storage"))),]

item78.dat4 <- item78.dat3[-grep("-|Unknown|unknown", item78.dat3$Clean.Wattage),]

item78.dat4$Total.Wattage <- as.numeric(as.character(item78.dat4$Fixture.Qty)) * 
  as.numeric(as.character(item78.dat4$LIGHTING_BulbsPerFixture)) * 
  as.numeric(as.character(item78.dat4$Clean.Wattage))

item78.dat5 <- item78.dat4[which(!(is.na(item78.dat4$Total.Wattage))),]
item78.dat5$LPD <- item78.dat5$Total.Wattage / item78.dat5$SiteArea

item78.dat6 <- item78.dat5[which(!(is.na(item78.dat5$LPD))), ] 



# Weighting
item78.data <- weightedData(item78.dat6[-which(colnames(item78.dat6) %in% c("CK_SiteID"
                                                                            ,"Clean.Room"
                                                                            ,"Fixture.Qty"
                                                                            ,"Clean.Wattage"
                                                                            ,"LIGHTING_BulbsPerFixture"
                                                                            ,"Lamp.Category"
                                                                            ,"count"
                                                                            ,"SiteArea"
                                                                            ,"Total.Wattage"
                                                                            ,"LPD"))])

item78.data <- left_join(item78.data, item78.dat6[which(colnames(item78.dat6) %in% c("CK_Cadmus_ID"
                                                                                     ,"CK_SiteID"
                                                                                     ,"Clean.Room"
                                                                                     ,"Fixture.Qty"
                                                                                     ,"Clean.Wattage"
                                                                                     ,"LIGHTING_BulbsPerFixture"
                                                                                     ,"Lamp.Category"
                                                                                     ,"count"
                                                                                     ,"SiteArea"
                                                                                     ,"Total.Wattage"
                                                                                     ,"LPD"))])

<<<<<<< HEAD
item78.prep <- left_join(rbsa.dat, item78.dat8)
item78.prep1 <- item78.prep[which(!is.na(item78.prep$LPD)),]

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
=======
###############################
# Weighted Analysis
###############################



item78.final <- mean_one_group(CustomerLevelData = item78.data
                               ,valueVariable = 'LPD' 
                               ,byVariable    = 'State'
                               ,aggregateRow  = "Region")

# Export table
item78.final.SF <- item78.final[which(item78.final$BuildingType == "Single Family"),-1]
item78.final.MH <- item78.final[which(item78.final$BuildingType == "Manufactured"),-1]

exportTable(item78.final.SF, "SF", "Table 85", weighted = TRUE)
exportTable(item78.final.MH, "MH", "Table 66", weighted = TRUE)


# OLD CODE #
# 
# item78.dat6 <- summarise(group_by(item78.dat5, CK_Cadmus_ID, BuildingType, State)
#                          ,Total.Wattage = sum(Total.Wattage))
# 
# #merge on area information
# item78.dat7 <- left_join(item78.dat6, item78.area1, by = c("CK_Cadmus_ID"))
# 
# item78.dat7$LPD <- item78.dat7$Total.Wattage / item78.dat7$SiteArea
# 
# item78.dat8 <- item78.dat7[which(!(is.na(item78.dat7$LPD))),]
#
# 
# #summarise by state
# item78.state <- summarise(group_by(item78.dat8, BuildingType, State)
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Mean = mean(LPD)
#                          ,SE   = sd(LPD) / sqrt(SampleSize))
# #summarise across states
# item78.region <- summarise(group_by(item78.dat8, BuildingType)
#                              ,State = "Region"
#                              ,SampleSize = length(unique(CK_Cadmus_ID))
#                              ,Mean = mean(LPD)
#                              ,SE   = sd(LPD) / sqrt(SampleSize))
# 
# item78.final <- rbind.data.frame(item78.state, item78.region, stringsAsFactors = F)
# 
# 
# 
# #table format
# item78.table <- data.frame("BuildingType" = item78.final$BuildingType
#                            ,"State" = item78.final$State
#                            ,"Mean" = item78.final$Mean
#                            ,"SE" = item78.final$SE
#                            ,"SampleSize" = item78.final$SampleSize)
# 
# #subset to only relevant building types
# item78.table1 <- item78.table[which(item78.table$BuildingType %in% c("Single Family", "Manufactured")),]
# 
# 
>>>>>>> 9e4a9af84d58d121769174f0e3f7be66577950a9

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

<<<<<<< HEAD
item79.dat0 <- summarise(group_by(item79.dat, CK_Cadmus_ID, BuildingType, State, Clean.Room)
                         ,Total.Wattage = sum(Total.Wattage))

#merge on area information
item79.dat1 <- left_join(item79.dat0, item79.area1, by = c("CK_Cadmus_ID", "Clean.Room"))

item79.dat1$LPD <- item79.dat1$Total.Wattage / item79.dat1$SiteArea

item79.dat2 <- item79.dat1[which(!(is.na(item79.dat1$LPD))),]

item79.prep <- left_join(rbsa.dat, item79.dat2)
item79.prep1 <- item79.prep[which(!is.na(item79.prep$LPD)),]


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
=======
item79.dat$LPD <- item79.dat$Total.Wattage / item79.dat$SiteArea

item79.dat1 <- item79.dat[which(!(is.na(item79.dat$LPD))), ] 


# Weighting
item79.data <- weightedData(item79.dat1[-which(colnames(item79.dat1) %in% c("CK_SiteID"
                                                                            ,"Clean.Room"
                                                                            ,"Fixture.Qty"
                                                                            ,"Clean.Wattage"
                                                                            ,"LIGHTING_BulbsPerFixture"
                                                                            ,"Lamp.Category"
                                                                            ,"count"
                                                                            ,"SiteArea"
                                                                            ,"Total.Wattage"
                                                                            ,"LPD"))])

item79.data <- left_join(item79.data, item79.dat1[which(colnames(item79.dat1) %in% c("CK_Cadmus_ID"
                                                                                     ,"CK_SiteID"
                                                                                     ,"Clean.Room"
                                                                                     ,"Fixture.Qty"
                                                                                     ,"Clean.Wattage"
                                                                                     ,"LIGHTING_BulbsPerFixture"
                                                                                     ,"Lamp.Category"
                                                                                     ,"count"
                                                                                     ,"SiteArea"
                                                                                     ,"Total.Wattage"
                                                                                     ,"LPD"))])

###############################
# Weighted Analysis
###############################



item79.final <- mean_one_group(CustomerLevelData = item79.data
                               ,valueVariable = 'LPD' 
                               ,byVariable    = 'Clean.Room'
                               ,aggregateRow  = "All Room Types")
>>>>>>> 9e4a9af84d58d121769174f0e3f7be66577950a9

# Export table
item79.final.SF <- item79.final[which(item79.final$BuildingType == "Single Family"),-1]
item79.final.MH <- item79.final[which(item79.final$BuildingType == "Manufactured"),-1]

exportTable(item79.final.SF, "SF", "Table 86", weighted = TRUE)
exportTable(item79.final.MH, "MH", "Table 64", weighted = TRUE)


<<<<<<< HEAD
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


=======
# OLD CODE #
# item79.dat0 <- summarise(group_by(item79.dat, CK_Cadmus_ID, BuildingType, State, Clean.Room)
#                          ,Total.Wattage = sum(Total.Wattage))
# 
# #merge on area information
# item79.dat1 <- left_join(item79.dat0, item79.area1, by = c("CK_Cadmus_ID", "Clean.Room"))
# 
# item79.dat1$LPD <- item79.dat1$Total.Wattage / item79.dat1$SiteArea
# 
# item79.dat2 <- item79.dat1[which(!(is.na(item79.dat1$LPD))),]
#
# 
# #summarise by room type
# item79.room <- summarise(group_by(item79.dat2, BuildingType, Clean.Room)
#                           ,SampleSize = length(unique(CK_Cadmus_ID))
#                           ,Mean = mean(LPD)
#                           ,SE   = sd(LPD) / sqrt(SampleSize))
# #summarise across room types
# item79.allRooms <- summarise(group_by(item79.dat2, BuildingType)
#                            ,Clean.Room = "All Room Types"
#                            ,SampleSize = length(unique(CK_Cadmus_ID))
#                            ,Mean = mean(LPD)
#                            ,SE   = sd(LPD) / sqrt(SampleSize))
# 
# #merge together both summaries
# item79.final <- rbind.data.frame(item79.room, item79.allRooms, stringsAsFactors = F)
# 
# 
# 
# #table format
# item79.table <- data.frame("BuildingType" = item79.final$BuildingType
#                            ,"Clean.Room" = item79.final$Clean.Room
#                            ,"Mean" = item79.final$Mean
#                            ,"SE" = item79.final$SE
#                            ,"SampleSize" = item79.final$SampleSize)
# 
# #subset to only relevant building types
# item79.table1 <- item79.table[which(item79.table$BuildingType %in% c("Single Family", "Manufactured")),]
>>>>>>> 9e4a9af84d58d121769174f0e3f7be66577950a9
