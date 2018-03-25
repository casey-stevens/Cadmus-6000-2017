#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

##  Create "Not In" operator
"%notin%" <- Negate("%in%")

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation - MF-BLDG.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) 
rbsa.dat.MF <- rbsa.dat[grep("Multifamily", rbsa.dat$BuildingType),]
rbsa.dat.MF <- rbsa.dat.MF[grep("BLDG", rbsa.dat.MF$CK_Building_ID),]


# buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))

# sites.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.export))
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))


#read in Envelope data for MF table
# envelope.dat <- read.xlsx(envelope.export)
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))
envelope.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_SiteID"
                                                                  ,"Wall.Area"
                                                                  ,"Conditioned.Living.Area"
                                                                  ,"Category"
                                                                  ,"ENV_Fenestration_WINDOWS_NumOfWindowsFacingEast"
                                                                  ,"ENV_Fenestration_WINDOWS_NumOfWindowsFacingNorth"
                                                                  ,"ENV_Fenestration_WINDOWS_NumOfWindowsFacingNortheast"
                                                                  ,"ENV_Fenestration_WINDOWS_NumOfWindowsFacingNorthwest"
                                                                  ,"ENV_Fenestration_WINDOWS_NumOfWindowsFacingSouth"
                                                                  ,"ENV_Fenestration_WINDOWS_NumOfWindowsFacingSoutheast"
                                                                  ,"ENV_Fenestration_WINDOWS_NumOfWindowsFacingSouthwest"
                                                                  ,"ENV_Fenestration_WINDOWS_NumOfWindowsFacingWest"
                                                                  ))]
envelope.dat2 <- envelope.dat1[which(!is.na(envelope.dat1$ENV_Fenestration_WINDOWS_NumOfWindowsFacingEast)),]
envelope.dat2$Window.Count <- (envelope.dat2$ENV_Fenestration_WINDOWS_NumOfWindowsFacingEast +
                                 envelope.dat2$ENV_Fenestration_WINDOWS_NumOfWindowsFacingNorth +
                                 envelope.dat2$ENV_Fenestration_WINDOWS_NumOfWindowsFacingNortheast +
                                 envelope.dat2$ENV_Fenestration_WINDOWS_NumOfWindowsFacingNorthwest +
                                 envelope.dat2$ENV_Fenestration_WINDOWS_NumOfWindowsFacingSouth +
                                 envelope.dat2$ENV_Fenestration_WINDOWS_NumOfWindowsFacingSoutheast +
                                 envelope.dat2$ENV_Fenestration_WINDOWS_NumOfWindowsFacingSouthwest +
                                 envelope.dat2$ENV_Fenestration_WINDOWS_NumOfWindowsFacingWest)
# envelope.dat2 <- envelope.dat2[-grep("bldg",envelope.dat2$CK_SiteID, ignore.case = T),]
dup.ind <- envelope.dat2$CK_SiteID[which(duplicated(envelope.dat2$CK_SiteID))]
envelope.dat2 <- envelope.dat2[-which(envelope.dat2$CK_SiteID %in% dup.ind & envelope.dat2$Window.Count == 4),]


envelope.merge <- left_join(rbsa.dat.MF, envelope.dat2, by = c("CK_Building_ID" = "CK_SiteID"))
length(unique(envelope.merge$CK_Cadmus_ID)) 
envelope.dat.MF <- envelope.merge


#############################################################################################
# This is data cleaning and merging for item 232
#############################################################################################

one.line.bldg.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), startRow = 2)
one.line.bldg.dat$CK_Building_ID <- trimws(toupper(one.line.bldg.dat$PK_BuildingID))
one.line.bldg.sub <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("Associated.Site.1"
                                                                                ,"CK_Building_ID"
                                                                                ,"Avg.Window.Size"))]
colnames(one.line.bldg.sub)[which(colnames(one.line.bldg.sub) == "Associated.Site.1")] <- "CK_Cadmus_ID"



envelope.window.sub <- unique(envelope.dat2[which(colnames(envelope.dat2) %in% c("CK_SiteID", "Window.Count"))])
rbsa.sub <- rbsa.dat.MF[which(colnames(rbsa.dat.MF) %in% c("CK_Cadmus_ID", "CK_Building_ID"))]
envelope.window.merge <- left_join(envelope.window.sub, rbsa.sub, by = c("CK_SiteID" = "CK_Building_ID"))
envelope.window.merge <- envelope.window.merge[which(!is.na(envelope.window.merge$CK_Cadmus_ID)),]
envelope.window.merge$CK_Cadmus_ID[which(duplicated(envelope.window.merge$CK_Cadmus_ID))]

one.line.bldg.merge1 <- left_join(one.line.bldg.sub, envelope.window.merge) 
one.line.bldg.merge1$Avg.Window.Size <- as.numeric(as.character(one.line.bldg.merge1$Avg.Window.Size))
one.line.bldg.merge1$Window.Count    <- as.numeric(as.character(one.line.bldg.merge1$Window.Count))
one.line.bldg.merge1$Window.Area     <- one.line.bldg.merge1$Avg.Window.Size * one.line.bldg.merge1$Window.Count
length(unique(one.line.bldg.merge1$CK_Cadmus_ID)) 

one.line.bldg.sub1 <- one.line.bldg.merge1[which(colnames(one.line.bldg.merge1) %in% c("CK_Cadmus_ID", "Window.Area"))]
one.line.merge <- left_join(rbsa.dat.MF, one.line.bldg.sub1)

one.line.bldg.dat.MF <- one.line.merge
which(duplicated(one.line.bldg.dat.MF$CK_Cadmus_ID))

oneline.sub <- one.line.bldg.dat.MF[which(names(one.line.bldg.dat.MF) %in% c("CK_Cadmus_ID", "Window.Area"))]
envelope.dat.MF.merge <- left_join(envelope.dat.MF, oneline.sub)






env.buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export), sheet = 1)
env.buildings.dat$CK_Building_ID <- trimws(toupper(env.buildings.dat$PK_BuildingID))
env.buildings.dat <- env.buildings.dat[which(colnames(env.buildings.dat) %in% c("CK_Building_ID"
                                                                                ,"SITES_MFB_cfg_MFB_CONFIG_TotEnclosedBldgArea_IncludResidentialAndCommercialButExcludPkgGarages"))]
colnames(env.buildings.dat) <- c("Floor_Area"
                                 ,"CK_Building_ID")

envelope.dat.MF.merge <- left_join(envelope.dat.MF.merge, env.buildings.dat)





#############################################################################################
#Item 232: Table 24
#############################################################################################
# make numeric
item232.windows <- one.line.bldg.dat.MF[which(!is.na(one.line.bldg.dat.MF$Window.Area)),]
item232.windows$Window.Area <- as.numeric(as.character(item232.windows$Window.Area))

item232.envelope <- envelope.dat1[which(envelope.dat1$Category == "Wall"),]
item232.envelope$Wall.Area <- as.numeric(as.character(item232.envelope$Wall.Area))
item232.envelope <- item232.envelope[which(!is.na(item232.envelope$Wall.Area)),]
##########################################
# Calculate average window size within unit, weighted average by number of windows and area
##########################################
item232.windows.sum <- summarise(group_by(item232.windows, CK_Building_ID)
                                 , WindowArea = sum(Window.Area, na.rm = T))

##########################################
# Calculate total number of windows and wall area per building
##########################################
item232.wall.sum <- summarise(group_by(item232.envelope, CK_SiteID)
                          ,WallArea   = sum(Wall.Area,  na.rm = T))
names(item232.wall.sum) <- c("CK_Building_ID", "WallArea")


##########################################
# Merge Window and Wall Area
##########################################
item232.dat <- left_join(item232.windows.sum, item232.wall.sum)
#Remove any items where walla area and window are are not greater than zero.

item232.dat1 <- item232.dat[which(item232.dat$WallArea > 0 & 
                                    item232.dat$WindowArea > 0),]

#calculate the window to wall ratio
item232.dat1$WindowToWallArea <- item232.dat1$WindowArea / item232.dat1$WallArea
summary(item232.dat1$WindowToWallArea)

item232.dat2 <- item232.dat1#[which(item232.dat1$WindowToWallArea <= 1),]

item232.merge <- left_join(rbsa.dat.MF, item232.dat2)
item232.merge <- item232.merge[which(!is.na(item232.merge$WindowToWallArea)),]
# item232.merge <- item232.merge[which(item232.merge$BuildingTypeXX == "Apartment Building (3 or fewer floors)"),]
################################################
# Adding pop and sample sizes for weights
################################################
item232.data <- weightedData(item232.merge[-which(colnames(item232.merge) %in% c("WindowArea"
                                                                                 ,"WallArea"
                                                                                 ,"WindowToWallArea"))])
item232.data <- left_join(item232.data, item232.merge[which(colnames(item232.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"WindowArea"
                                                                                           ,"WallArea"
                                                                                           ,"WindowToWallArea"))])

item232.data$WindowToWallArea <- as.numeric(as.character(item232.data$WindowToWallArea))
item232.data$count <- 1

export.item232 <- item232.data[which(item232.data$WindowToWallArea > .3), which(names(item232.data) %in% c("CK_Cadmus_ID","WindowArea","WallArea","WindowToWallArea"))]
#######################
# Weighted Analysis
#######################
item232.final <- mean_one_group(CustomerLevelData = item232.data
                                ,valueVariable    = 'WindowToWallArea'
                                ,byVariable       = 'HomeType'
                                ,aggregateRow     = "All Sizes")
item232.final.MF <- item232.final[which(item232.final$BuildingType == "Multifamily")
                                  ,which(colnames(item232.final) %notin% c("BuildingType"))]
exportTable(item232.final.MF, "MF", "Table 24", weighted = TRUE)


#######################
# unweighted Analysis
#######################
item232.final <- mean_one_group_unweighted(CustomerLevelData = item232.data
                                           ,valueVariable    = 'WindowToWallArea'
                                           ,byVariable       = 'HomeType'
                                           ,aggregateRow     = "All Sizes")
item232.final.MF <- item232.final[which(item232.final$BuildingType == "Multifamily")
                                  ,which(colnames(item232.final) %notin% c("BuildingType"))]
exportTable(item232.final.MF, "MF", "Table 24", weighted = FALSE)






#############################################################################################
#Item 233: Table 25
#############################################################################################
##########################################
# Merge Window and Floor Area
##########################################
# Window area is same for item 233 as it was for 232
# item233.windows.sum <- item232.windows.sum

item233.envelope <- envelope.dat.MF.merge[which(!is.na(envelope.dat.MF.merge$Floor_Area)),]
item233.envelope$Floor.Area <- as.numeric(as.character(item233.envelope$Floor_Area))
item233.envelope <- item233.envelope[which(!is.na(item233.envelope$Window.Area)),]
item233.envelope <- item233.envelope[which(!is.na(item233.envelope$Window.Count)),]

item233.sum <- summarise(group_by(item233.envelope, CK_Building_ID)
                         ,FloorArea  = sum(Floor.Area, na.rm = T)
                         ,WindowArea = sum(Window.Area, na.rm = T)
                         ,WindowCount = sum(Window.Count, na.rm = T))

#Remove any items where walla area and window are are not greater than zero.
item233.dat1 <- item233.sum[which(item233.sum$FloorArea > 0 & item233.sum$WindowArea > 0),]

#calculate the window to wall ratio
item233.dat1$WindowToFloorArea <- item233.dat1$WindowArea / item233.dat1$FloorArea
summary(item233.dat1$WindowToFloorArea)

item233.dat2 <- item233.dat1[which(item233.dat1$WindowToFloorArea <= 1),]

item233.merge <- left_join(rbsa.dat.MF, item233.dat2)
item233.merge <- item233.merge[which(!is.na(item233.merge$WindowToFloorArea)),]

################################################
# Adding pop and sample sizes for weights
################################################
item233.data <- weightedData(item233.merge[-which(colnames(item233.merge) %in% c("WindowArea"
                                                                                 ,"FloorArea"
                                                                                 ,"WindowCount"
                                                                                 ,"WindowToFloorArea"))])
item233.data <- left_join(item233.data, item233.merge[which(colnames(item233.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"WindowArea"
                                                                                           ,"FloorArea"
                                                                                           ,"WindowCount"
                                                                                           ,"WindowToFloorArea"))])

item233.data$WindowToFloorArea <- as.numeric(as.character(item233.data$WindowToFloorArea))
item233.data$count <- 1
#######################
# Weighted Analysis
#######################
item233.final <- mean_one_group(CustomerLevelData = item233.data
                                ,valueVariable    = 'WindowToFloorArea'
                                ,byVariable       = 'HomeType'
                                ,aggregateRow     = "All Sizes")
item233.final.MF <- item233.final[which(item233.final$BuildingType == "Multifamily")
                                  ,which(colnames(item233.final) %notin% c("BuildingType"))]
exportTable(item233.final.MF, "MF", "Table 25", weighted = TRUE)


#######################
# unweighted Analysis
#######################
item233.final <- mean_one_group_unweighted(CustomerLevelData = item233.data
                                           ,valueVariable    = 'WindowToFloorArea'
                                           ,byVariable       = 'HomeType'
                                           ,aggregateRow     = "All Sizes")
item233.final.MF <- item233.final[which(item233.final$BuildingType == "Multifamily")
                                  ,which(colnames(item233.final) %notin% c("BuildingType"))]
exportTable(item233.final.MF, "MF", "Table 25", weighted = FALSE)
