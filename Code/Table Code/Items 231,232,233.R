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
rbsa.dat.MF <- rbsa.dat[grep("Multifamily", rbsa.dat$BuildingType),]
rbsa.dat.MF <- rbsa.dat.MF[grep("BLDG", rbsa.dat.MF$CK_Building_ID),]

# #Read in data for analysis
# # Windows
# windows.doors.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, windows.export))
# windows.doors.dat$CK_Cadmus_ID <- trimws(toupper(windows.doors.dat$CK_Cadmus_ID))
# windows.dat <- windows.doors.dat[which(windows.doors.dat$Type == "Window"),]
# #select columns for windows
# windows.dat1 <- windows.dat[which(colnames(windows.dat) %in% c("CK_Cadmus_ID"
#                                                                ,"CK_SiteID"
#                                                                ,"Type"
#                                                                ,"Quantity"
#                                                                ,"Frame./.Body.Type"
#                                                                ,"Glazing.Type"
#                                                                ,"Area"))]
# windows.dat2  <- left_join(rbsa.dat, windows.dat1, by = c("CK_Building_ID" = "CK_SiteID"))
# length(unique(windows.dat2$CK_Cadmus_ID)) 
# #Subset to MF
# windows.dat.MF <- windows.dat2[grep("Multifamily", windows.dat2$BuildingType),]


######################################################################################################################
# Note: Windows data for MF moved to the envelope export

buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))

sites.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.export))
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))


#read in Envelope data for MF table
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
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


envelope.merge <- left_join(rbsa.dat, envelope.dat2, by = c("CK_Building_ID" = "CK_SiteID"))
length(unique(envelope.merge$CK_Cadmus_ID)) 
envelope.dat.MF <- envelope.merge[grep("Multifamily", envelope.merge$BuildingType),]
envelope.dat.MF <- envelope.dat.MF[-grep("SITE", envelope.dat.MF$CK_Building_ID),]


#############################################################################################
# This is data cleaning and merging for item 232
#############################################################################################

one.line.bldg.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), startRow = 2)
one.line.bldg.dat$CK_Building_ID <- trimws(toupper(one.line.bldg.dat$CK_BuildingID))
one.line.bldg.sub <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("Associated.Site.1"
                                                                                ,"CK_Building_ID"
                                                                                ,"Avg.Window.Size"))]
colnames(one.line.bldg.sub)[which(colnames(one.line.bldg.sub) == "Associated.Site.1")] <- "CK_Cadmus_ID"



envelope.window.sub <- unique(envelope.dat2[which(colnames(envelope.dat2) %in% c("CK_SiteID", "Window.Count"))])
rbsa.sub <- rbsa.dat[which(colnames(rbsa.dat) %in% c("CK_Cadmus_ID", "CK_Building_ID"))]
envelope.window.merge <- left_join(envelope.window.sub, rbsa.sub, by = c("CK_SiteID" = "CK_Building_ID"))
envelope.window.merge <- envelope.window.merge[which(!is.na(envelope.window.merge$CK_Cadmus_ID)),]
envelope.window.merge$CK_Cadmus_ID[which(duplicated(envelope.window.merge$CK_Cadmus_ID))]

one.line.bldg.merge1 <- left_join(one.line.bldg.sub, envelope.window.merge) 
one.line.bldg.merge1$Avg.Window.Size <- as.numeric(as.character(one.line.bldg.merge1$Avg.Window.Size))
one.line.bldg.merge1$Window.Count    <- as.numeric(as.character(one.line.bldg.merge1$Window.Count))
one.line.bldg.merge1$Window.Area     <- one.line.bldg.merge1$Avg.Window.Size * one.line.bldg.merge1$Window.Count
length(unique(one.line.bldg.merge1$CK_Cadmus_ID)) 

one.line.bldg.sub1 <- one.line.bldg.merge1[which(colnames(one.line.bldg.merge1) %in% c("CK_Cadmus_ID", "Window.Area"))]
one.line.merge <- left_join(rbsa.dat, one.line.bldg.sub1)

one.line.bldg.dat.MF <- one.line.merge[grep("Multifamily", one.line.merge$BuildingType),]
one.line.bldg.dat.MF <- one.line.merge[-grep("SITE"      , one.line.merge$CK_Building_ID),]
which(duplicated(one.line.bldg.dat.MF$CK_Cadmus_ID))

oneline.sub <- one.line.bldg.dat.MF[which(names(one.line.bldg.dat.MF) %in% c("CK_Cadmus_ID", "Window.Area"))]
envelope.dat.MF.merge <- left_join(envelope.dat.MF, oneline.sub)
#############################################################################################
#Item 231: Table 23
#############################################################################################
##########################################
# Clean up window data
##########################################
item231.dat <- envelope.dat.MF

#clean up frame/body type
unique(item231.dat$`Frame./.Body.Type`)
item231.dat$Frame.Type <- trimws(item231.dat$`Frame./.Body.Type`)
item231.dat$Frame.Type[grep("Wood|Vinyl|Fiberglass|wood|vinyl|fiberglass|tile|Garage", item231.dat$Frame.Type)] <- "Wood/Vinyl/Fiberglass/Tile"
item231.dat$Frame.Type[grep("Metal|Aluminum|metal|aluminum", item231.dat$Frame.Type)] <- "Metal"
item231.dat$Frame.Type[grep("N/A", item231.dat$Frame.Type)] <- "Unknown"
item231.dat$Frame.Type[which(is.na(item231.dat$Frame.Type))] <- "Unknown"
unique(item231.dat$Frame.Type)

item231.dat1 <- item231.dat[which(item231.dat$Frame.Type != "Unknown"),]
unique(item231.dat1$Frame.Type)

#clean up glazing types
item231.dat1$Glazing <- trimws(item231.dat1$Glazing.Type)
item231.dat1$Glazing[grep("Single", item231.dat1$Glazing)] <- "Single"
item231.dat1$Glazing[grep("Double", item231.dat1$Glazing)] <- "Double"
item231.dat1$Glazing[which(!(item231.dat1$Glazing %in% c("Single", "Double")))] <- "Unknown"
unique(item231.dat$Glazing)

item231.dat2 <- item231.dat1[which(item231.dat1$Glazing != "Unknown"),]
unique(item231.dat2$Glazing)

item231.dat2$Framing.Categories <- paste(item231.dat2$Frame.Type, item231.dat2$Glazing, sep = " ")

item231.dat2$count <- 1
item231.dat3 <- item231.dat2[which(!is.na(as.numeric(as.character(item231.dat2$Quantity)))),]

window.area.test <- rep(item231.dat3$Area, item231.dat3$Quantity)
window.type.test <- rep(item231.dat3$Framing.Categories, item231.dat3$Quantity)
cadmus.id.test   <- rep(item231.dat3$CK_Cadmus_ID, item231.dat3$Quantity)
item231.windows  <- cbind.data.frame(cadmus.id.test, window.type.test, window.area.test)
names(item231.windows) <- c("CK_Cadmus_ID", "Window_Type", "Window_Area")

item231.merge <- left_join(rbsa.dat, item231.windows)
item231.merge <- item231.merge[which(item231.merge$Window_Area %notin% c("Unknown", NA)),]
item231.merge <- item231.merge[which(!is.na(item231.merge$HomeYearBuilt)),]




################################################
# Adding pop and sample sizes for weights
################################################
item231.data <- weightedData(item231.merge[-which(colnames(item231.merge) %in% c("Window_Type"
                                                                                 ,"Window_Area"))])
item231.data <- left_join(item231.data, item231.merge[which(colnames(item231.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Window_Type"
                                                                                           ,"Window_Area"))])

item231.data$count <- 1
item231.data$Window_Area <- as.numeric(as.character(item231.data$Window_Area))
item231.data$Window_Type <- as.character(item231.data$Window_Type)

#######################
# Weighted Analysis
#######################
item231.final <- proportionRowsAndColumns1(CustomerLevelData = item231.data
                                           ,valueVariable    = 'Window_Area'
                                           ,columnVariable   = 'HomeYearBuilt_bins_MF'
                                           ,rowVariable      = 'Window_Type'
                                           ,aggregateColumnName = "Remove")
item231.final <- item231.final[which(item231.final$HomeYearBuilt_bins_MF != "Remove"),]
item231.final <- item231.final[which(item231.final$Window_Type != "Total"),]


item231.all.vintages <- proportions_one_group_MF(CustomerLevelData = item231.data
                                              ,valueVariable = 'Window_Area'
                                              ,groupingVariable = 'Window_Type'
                                              ,total.name = "All Vintages"
                                              ,columnName = "HomeYearBuilt_bins_MF"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
# item231.all.vintages <- item231.all.vintages[which(item231.all.vintages$Window_Type != "Total"),]

item231.final <- rbind.data.frame(item231.final, item231.all.vintages, stringsAsFactors = F)

item231.cast <- dcast(setDT(item231.final)
                      ,formula = HomeYearBuilt_bins_MF ~ Window_Type
                      ,value.var = c("w.percent","w.SE", "count","n","N"))

item231.final <- data.frame( "Vintage"                          = item231.cast$HomeYearBuilt_bins_MF
                             ,"Metal Double"                    = item231.cast$`w.percent_Metal Double`
                             ,"Metal Double SE"                 = item231.cast$`w.SE_Metal Double`
                             ,"Metal Single"                    = item231.cast$`w.percent_Metal Single`
                             ,"Metal Single SE"                 = item231.cast$`w.SE_Metal Single`
                             ,"Wood/Vinyl/Fiberglass Double"    = item231.cast$`w.percent_Wood/Vinyl/Fiberglass/Tile Double`
                             ,"Wood/Vinyl/Fiberglass Double SE" = item231.cast$`w.SE_Wood/Vinyl/Fiberglass/Tile Double`
                             ,"Wood/Vinyl/Fiberglass Single"    = item231.cast$`w.percent_Wood/Vinyl/Fiberglass/Tile Single`
                             ,"Wood/Vinyl/Fiberglass Single SE" = item231.cast$`w.SE_Wood/Vinyl/Fiberglass/Tile Single`
                             ,"SampleSize"                      = item231.cast$`n_Wood/Vinyl/Fiberglass/Tile Double`)

exportTable(item231.final, "MF", "Table 23", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item231.final <- proportions_two_groups_unweighted(CustomerLevelData = item231.data
                                           ,valueVariable    = 'Window_Area'
                                           ,columnVariable   = 'HomeYearBuilt_bins_MF'
                                           ,rowVariable      = 'Window_Type'
                                           ,aggregateColumnName = "All Vintages")
item231.final <- item231.final[which(item231.final$Window_Type != "Total"),]

item231.cast <- dcast(setDT(item231.final)
                      ,formula = HomeYearBuilt_bins_MF ~ Window_Type
                      ,value.var = c("Percent","SE", "Count","SampleSize"))

item231.final <- data.frame( "Vintage"                          = item231.cast$HomeYearBuilt_bins_MF
                             ,"Metal Double"                    = item231.cast$`Percent_Metal Double`
                             ,"Metal Double SE"                 = item231.cast$`SE_Metal Double`
                             ,"Metal Double n"                  = item231.cast$`SampleSize_Metal Double`
                             ,"Metal Single"                    = item231.cast$`Percent_Metal Single`
                             ,"Metal Single SE"                 = item231.cast$`SE_Metal Single`
                             ,"Metal Single n"                  = item231.cast$`SampleSize_Metal Single`
                             ,"Wood/Vinyl/Fiberglass Double"    = item231.cast$`Percent_Wood/Vinyl/Fiberglass/Tile Double`
                             ,"Wood/Vinyl/Fiberglass Double SE" = item231.cast$`SE_Wood/Vinyl/Fiberglass/Tile Double`
                             ,"Wood/Vinyl/Fiberglass Double n"  = item231.cast$`SampleSize_Wood/Vinyl/Fiberglass/Tile Double`
                             ,"Wood/Vinyl/Fiberglass Single"    = item231.cast$`Percent_Wood/Vinyl/Fiberglass/Tile Single`
                             ,"Wood/Vinyl/Fiberglass Single SE" = item231.cast$`SE_Wood/Vinyl/Fiberglass/Tile Single`
                             ,"Wood/Vinyl/Fiberglass Single n"  = item231.cast$`SampleSize_Wood/Vinyl/Fiberglass/Tile Single`)

exportTable(item231.final, "MF", "Table 23", weighted = FALSE)





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

item232.merge <- left_join(rbsa.dat, item232.dat2)
item232.merge <- item232.merge[which(!is.na(item232.merge$WindowToWallArea)),]
item232.merge <- item232.merge[which(item232.merge$BuildingTypeXX == "Apartment Building (3 or fewer floors)"),]
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
item232.final.MF <- item232.final[which(item232.final$BuildingType == "Multifamily"),which(colnames(item232.final) %notin% c("BuildingType"))]
exportTable(item232.final.MF, "MF", "Table 24", weighted = TRUE)


#######################
# unweighted Analysis
#######################
item232.final <- mean_one_group_unweighted(CustomerLevelData = item232.data
                                           ,valueVariable    = 'WindowToWallArea'
                                           ,byVariable       = 'HomeType'
                                           ,aggregateRow     = "All Sizes")
item232.final.MF <- item232.final[which(item232.final$BuildingType == "Multifamily"),which(colnames(item232.final) %notin% c("BuildingType"))]
exportTable(item232.final.MF, "MF", "Table 24", weighted = FALSE)






#############################################################################################
#Item 233: Table 25
#############################################################################################
##########################################
# Merge Window and Floor Area
##########################################
# Window area is same for item 233 as it was for 232
# item233.windows.sum <- item232.windows.sum

item233.envelope <- envelope.dat.MF.merge[which(!is.na(envelope.dat.MF.merge$Conditioned.Area)),]
item233.envelope$Floor.Area <- as.numeric(as.character(item233.envelope$Conditioned.Area))
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

item233.merge <- left_join(rbsa.dat, item233.dat2)
item233.merge <- item233.merge[which(!is.na(item233.merge$WindowToFloorArea)),]

################################################
# Adding pop and sample sizes for weights
################################################
item233.data <- weightedData(item233.merge[-which(colnames(item233.merge) %in% c("WindowArea"
                                                                                 ,"FloorArea"
                                                                                 ,"WindowToFloorArea"))])
item233.data <- left_join(item233.data, item233.merge[which(colnames(item233.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"WindowArea"
                                                                                           ,"FloorArea"
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
