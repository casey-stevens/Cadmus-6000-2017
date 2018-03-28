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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
rbsa.dat.MF <- rbsa.dat[which(rbsa.dat$BuildingType == "Multifamily"),]
rbsa.dat.site <- rbsa.dat.MF[grep("site", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat.MF[grep("bldg", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
# lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))

#Read in data for analysis
# buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))


one.line.bldg.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), sheet = "Building One Line Summary", startRow = 3)
one.line.bldg.dat <- one.line.bldg.dat[which(one.line.bldg.dat$Area.of.Conditioned.Common.Space > 0),]
one.line.bldg.dat$CK_Building_ID <- one.line.bldg.dat$PK_BuildingID

one.line.bldg.dat <- one.line.bldg.dat[names(one.line.bldg.dat) %in% c("CK_Building_ID", "Area.of.Conditioned.Common.Space")]

rbsa.merge <- left_join(rbsa.dat.bldg, one.line.bldg.dat)
rbsa.merge <- rbsa.merge[which(!is.na(rbsa.merge$Area.of.Conditioned.Common.Space)),]

# rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
rooms.dat$CK_Cadmus_ID <- trimws(toupper(rooms.dat$CK_Cadmus_ID))
rooms.dat$CK_Building_ID <- trimws(toupper(rooms.dat$CK_SiteID))




#############################################################################################
#Item 260: AVERAGE COMMON AREA LPD (W/SQ.FT.) BY BUILDING VINTAGE (MF Table 52)
#############################################################################################
# item260.buildings <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
#                                                             ,"SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"))]
# 
# item260.buildings1 <- item260.buildings[which(!(is.na(item260.buildings$SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea))),]
# 
# item260.buildings2 <- summarise(group_by(item260.buildings1, CK_Building_ID)
#                             ,CommonFloorArea = sum(SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea))
# 
# item260.buildings3 <- item260.buildings2[which(item260.buildings2$CommonFloorArea > 0),]
rooms.dat1 <- rooms.dat[grep("bldg", rooms.dat$CK_Building_ID, ignore.case = T),]
rooms.dat1$Area <- as.numeric(as.character(rooms.dat1$Area))

rooms.dat2 <- rooms.dat1[which(!is.na(rooms.dat1$Area)),]
names(rooms.dat2)[which(names(rooms.dat2) == "Clean.Type")] <- "Clean.Room"
unique(rooms.dat2$Clean.Room)

rooms.dat3 <- rooms.dat2[which(rooms.dat2$Clean.Room %notin% c("Parking", "Store")),]

rooms.sum <- summarise(group_by(rooms.dat3, CK_Building_ID)
                       ,CommonFloorArea = sum(Area))


#subset to columns needed for analysis
item260.lighting <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"CK_SiteID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Wattage"
                                                                ,"Clean.Room"
                                                                ,"Switch.Type"))]
item260.lighting <- item260.lighting[which(item260.lighting$Clean.Room %notin% c("Parking")),]
item260.lighting$count <- 1

item260.lighting1 <- item260.lighting[which(!(item260.lighting$Clean.Room %in% c("Outside", "Storage"))),]

#join clean rbsa data onto lighting analysis data
item260.dat1 <- left_join(rbsa.dat, item260.lighting1, by  = c("CK_Building_ID" = "CK_SiteID"))

#remove building info
item260.dat2 <- item260.dat1[grep("BLDG", item260.dat1$CK_Building_ID),]
names(item260.dat2)[which(names(item260.dat2) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"
item260.dat2 <- item260.dat2[which(names(item260.dat2) != "CK_Cadmus_ID.y")]

#clean fixture and bulbs per fixture
item260.dat2$Fixture.Qty <- as.numeric(as.character(item260.dat2$Fixture.Qty))
item260.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item260.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item260.dat2$Lamps <- item260.dat2$Fixture.Qty * item260.dat2$LIGHTING_BulbsPerFixture
unique(item260.dat2$Lamps)
#remove missing lamp quantities
item260.dat3 <- item260.dat2[which(!(is.na(item260.dat2$Lamps))),]

#calculate total wattage as wattage per bulb multiplied by total bulbs
item260.dat3$Total.Wattage <- item260.dat3$Lamps * as.numeric(as.character(item260.dat3$Clean.Wattage))
#remove missing wattage quantities
item260.dat4 <- item260.dat3[which(!(is.na(item260.dat3$Total.Wattage))),]
#make wattage numeric
item260.dat4$Total.Wattage <- as.numeric(as.character(item260.dat4$Total.Wattage))

#Subset to Multifamily
item260.dat5 <- item260.dat4[grep("Multifamily", item260.dat4$BuildingType),]


#summarise up to the site level
item260.dat6 <- summarise(group_by(item260.dat5, CK_Building_ID, Category)
                          ,SiteWattage = sum(Total.Wattage))

#merge on building data
item260.dat7 <- left_join(rbsa.dat, item260.dat6)
item260.merge <- left_join(item260.dat7, rooms.sum)
item260.merge <- item260.merge[grep("3 or fewer floors", item260.merge$BuildingTypeXX, ignore.case = T),]

#remove NA
item260.merge1 <- item260.merge[which(!(is.na(item260.merge$CommonFloorArea))),]
item260.merge1 <- item260.merge1[which(!(is.na(item260.merge1$SiteWattage))),]
item260.merge1 <- item260.merge1[which(!(is.na(item260.merge1$HomeYearBuilt_MF))),]


item260.merge1$LPD <- item260.merge1$SiteWattage / item260.merge1$CommonFloorArea
item260.merge1 <- item260.merge1[which(item260.merge1$Category == "PSE"),]
names(item260.merge1)
######################################
#Pop and Sample Sizes for weights
######################################
item260.data <- weightedData(item260.merge1[which(colnames(item260.merge1) %notin% c("SiteWattage"
                                                                                   ,"CommonFloorArea"
                                                                                   ,"LPD"
                                                                                   ,"Category"))])

item260.data <- left_join(item260.data, item260.merge1[which(colnames(item260.merge1) %in% c("CK_Cadmus_ID"
                                                                                           ,"SiteWattage"
                                                                                           ,"CommonFloorArea"
                                                                                           ,"LPD"
                                                                                           ,"Category"))])
item260.data$count <- 1


######################
# weighted analysis
######################
item260.final <- mean_one_group(CustomerLevelData = item260.data
                                ,valueVariable = 'LPD'
                                ,byVariable = 'HomeYearBuilt_bins_MF'
                                ,aggregateRow = "All Vintages")

item260.final <- item260.final[which(colnames(item260.final) %notin% c("BuildingType"))]

exportTable(item260.final, "MF", "Table 52", weighted = TRUE,OS = T, osIndicator = "PSE")



######################
# unweighted analysis
######################
item260.final <- mean_one_group_unweighted(CustomerLevelData = item260.data
                                ,valueVariable = 'LPD'
                                ,byVariable = 'HomeYearBuilt_bins_MF'
                                ,aggregateRow = "All Vintages")

item260.final <- item260.final[which(colnames(item260.final) %notin% c("BuildingType"))]

exportTable(item260.final, "MF", "Table 52", weighted = FALSE,OS = T, osIndicator = "PSE")






#############################################################################################
#Item 261: AVERAGE COMMON AREA LPD (W/SQ.FT.) BY BUILDING SIZE (MF Table 53)
#############################################################################################
item261.dat <- item260.merge

#remove NA
item261.merge1 <- item261.dat[which(!(is.na(item261.dat$CommonFloorArea))),]
item261.merge1 <- item261.merge1[which(!(is.na(item261.merge1$SiteWattage))),]

item261.merge1$LPD <- item261.merge1$SiteWattage / item261.merge1$CommonFloorArea

######################################
#Pop and Sample Sizes for weights
######################################
item261.data <- weightedData(item261.merge1[which(colnames(item261.merge1) %notin% c("SiteWattage"
                                                                                     ,"CommonFloorArea"
                                                                                     ,"LPD"
                                                                                     ,"Category"))])

item261.data <- left_join(item261.data, item261.merge1[which(colnames(item261.merge1) %in% c("CK_Cadmus_ID"
                                                                                             ,"SiteWattage"
                                                                                             ,"CommonFloorArea"
                                                                                             ,"LPD"
                                                                                             ,"Category"))])
item261.data$count <- 1


######################
# weighted analysis
######################
item261.final <- mean_one_group(CustomerLevelData = item261.data
                                ,valueVariable = 'LPD'
                                ,byVariable = 'Category'
                                ,aggregateRow = "Remove")
item261.final <-item261.final[which(item261.final$Category != "Remove"),]

item261.final <- item261.final[which(colnames(item261.final) %notin% c("BuildingType", "n_h", "N_h"))]

exportTable(item261.final, "MF", "Table 53", weighted = TRUE,OS = T, osIndicator = "PSE")



######################
# unweighted analysis
######################
item261.final <- mean_one_group_unweighted(CustomerLevelData = item261.data
                                           ,valueVariable = 'LPD'
                                           ,byVariable = 'Category'
                                           ,aggregateRow = "Remove")
item261.final <-item261.final[which(item261.final$Category != "Remove"),]

item261.final <- item261.final[which(colnames(item261.final) %notin% c("BuildingType", "n_h", "N_h"))]

exportTable(item261.final, "MF", "Table 53", weighted = FALSE,OS = T, osIndicator = "PSE")






#############################################################################################
#Item 262: AVERAGE COMMON AREA ROOM LPD (W/SQ.FT.) BY COMMON AREA ROOM TYPE (MF Table 54)
#############################################################################################
rooms.dat1 <- rooms.dat[grep("bldg", rooms.dat$CK_Building_ID, ignore.case = T),]
rooms.dat1$Area <- as.numeric(as.character(rooms.dat1$Area))

rooms.dat2 <- rooms.dat1[which(!is.na(rooms.dat1$Area)),]
names(rooms.dat2)[which(names(rooms.dat2) == "Clean.Type")] <- "Clean.Room"
unique(rooms.dat2$Clean.Room)

rooms.dat3 <- rooms.dat2[which(rooms.dat2$Clean.Room %notin% c("Parking", "Store")),]

rooms.sum <- summarise(group_by(rooms.dat3, CK_Building_ID, Clean.Room)
                       ,CommonFloorArea = sum(Area))

item262.dat <- item260.dat5

#summarise up to the site level
item262.dat1 <- summarise(group_by(item262.dat, CK_Building_ID, Category, Clean.Room)
                          ,SiteWattage = sum(Total.Wattage))

#merge on building data
item262.dat2 <- left_join(rbsa.dat, item262.dat1)
item262.merge <- left_join(item262.dat2, rooms.sum)

#remove NA
item262.merge1 <- item262.merge[which(!is.na(item262.merge$CommonFloorArea)),]
item262.merge1 <- item262.merge1[which(!(is.na(item262.merge1$SiteWattage))),]

item262.merge1$LPD <- item262.merge1$SiteWattage / item262.merge1$CommonFloorArea
item262.merge1 <- item262.merge1[which(item262.merge1$Category == "PSE"),]
######################################
#Pop and Sample Sizes for weights
######################################
item262.data <- weightedData(item262.merge1[which(colnames(item262.merge1) %notin% c("SiteWattage"
                                                                                     ,"CommonFloorArea"
                                                                                     ,"LPD"
                                                                                     ,"Clean.Room"
                                                                                     ,"Category"))])

item262.data <- left_join(item262.data, item262.merge1[which(colnames(item262.merge1) %in% c("CK_Cadmus_ID"
                                                                                             ,"SiteWattage"
                                                                                             ,"CommonFloorArea"
                                                                                             ,"LPD"
                                                                                             ,"Clean.Room"
                                                                                             ,"Category"))])
item262.data$count <- 1


######################
# weighted analysis
######################
item262.cast <- mean_two_groups(CustomerLevelData = item262.data
                                   ,valueVariable = 'LPD'
                                   ,byVariableRow = 'Clean.Room'
                                   ,byVariableColumn = 'HomeType'
                                   ,columnAggregate = "All Sizes"
                                   ,rowAggregate = "All Types")
names(item262.cast)
#make into table format
item262.table <- data.frame("Common.Area.Room.Type" = item262.cast$Clean.Room
                            ,"Low.Rise.1.3.Mean"    = item262.cast$`Mean_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.SE"      = item262.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.n"       = item262.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"All.Sizes.Mean"       = item262.cast$`Mean_All Sizes`
                            ,"All.Sizes.SE"         = item262.cast$`SE_All Sizes`
                            ,"All.Sizes.n"          = item262.cast$`n_All Sizes`
                            ,"Low.Rise.1.3.EB"      = item262.cast$`EB_Apartment Building (3 or fewer floors)`
                            ,"All.Sizes.EB"         = item262.cast$`EB_All Sizes`)

exportTable(item262.table, "MF", "Table 54", weighted = TRUE,OS = T, osIndicator = "PSE")

######################
# unweighted analysis
######################
item262.cast <- mean_two_groups_unweighted(CustomerLevelData = item262.data
                                ,valueVariable = 'LPD'
                                ,byVariableRow = 'Clean.Room'
                                ,byVariableColumn = 'HomeType'
                                ,columnAggregate = "All Sizes"
                                ,rowAggregate = "All Types")

#make into table format
item262.table <- data.frame("Common.Area.Room.Type" = item262.cast$Clean.Room
                            ,"Low.Rise.1.3.Mean"    = item262.cast$`Mean_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.SE"      = item262.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.n"       = item262.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"All.Sizes.Mean"       = item262.cast$`Mean_All Sizes`
                            ,"All.Sizes.SE"         = item262.cast$`SE_All Sizes`
                            ,"All.Sizes.n"          = item262.cast$`n_All Sizes`)

exportTable(item262.table, "MF", "Table 54", weighted = FALSE,OS = T, osIndicator = "PSE")

            





#############################################################################################
#Item 263: DISTRIBUTION OF COMMON AREA LIGHTING POWER (WATTS) BY CONTROL TYPE (MF Table 55)
#############################################################################################
item263.dat <- item260.dat5[which(item260.dat5$Switch.Type != "Unknown"),]
unique(item263.dat$Switch.Type)
item263.dat$Switch.Type[grep("On/off", item263.dat$Switch.Type, ignore.case = T)] <- "Manual Switch"
item263.dat$Switch.Type[grep("Other", item263.dat$Switch.Type, ignore.case = T)] <- "Other"
item263.dat$Switch.Type[grep("Timer", item263.dat$Switch.Type, ignore.case = T)] <- "Timer Control"
item263.dat$Switch.Type[grep("Dimmer", item263.dat$Switch.Type, ignore.case = T)] <- "Dimmer Switch"

#summarise up to the site level
item263.dat1 <- summarise(group_by(item263.dat, CK_Cadmus_ID, Category, Switch.Type)
                          ,Site.Wattage = sum(Total.Wattage))

item263.merge <- left_join(rbsa.dat, item263.dat1)
item263.merge <- item263.merge[grep("3 or fewer floors", item263.merge$BuildingTypeXX, ignore.case = T),]
item263.merge <- item263.merge[grep("BLDG", item263.merge$CK_Building_ID),]
item263.merge <- item263.merge[which(!is.na(item263.merge$Site.Wattage)),]
item263.merge <- item263.merge[which(item263.merge$Category == "PSE"),]
######################################
#Pop and Sample Sizes for weights
######################################
item263.data <- weightedData(item263.merge[which(colnames(item263.merge) %notin% c("Site.Wattage"
                                                                                     ,"Switch.Type"
                                                                                   ,"Category"))])

item263.data <- left_join(item263.data, item263.merge[which(colnames(item263.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Site.Wattage"
                                                                                           ,"Switch.Type"
                                                                                           ,"Category"))])
item263.data$count <- 1
length(unique(item263.data$CK_Cadmus_ID))

#############################
# weighted analysis
#############################
item263.final <- proportions_one_group(CustomerLevelData = item263.data
                                          ,valueVariable = 'Site.Wattage'
                                          ,groupingVariable = 'Switch.Type'
                                          ,total.name = NA
                                          ,weighted = TRUE)
# item263.final <- item263.final[which(item263.final$Switch.Type != "Total"),]
item263.final <- item263.final[which(colnames(item263.final) %notin% c("BuildingType"))]

exportTable(item263.final, "MF", "Table 55", weighted = TRUE,OS = T, osIndicator = "PSE")

#############################
# uneighted analysis
#############################
item263.final <- proportions_one_group(CustomerLevelData = item263.data
                                          ,valueVariable = 'Site.Wattage'
                                          ,groupingVariable = 'Switch.Type'
                                          ,total.name = NA
                                          ,weighted = FALSE)
# item263.final <- item263.final[which(item263.final$Switch.Type != "Total"),]
item263.final <- item263.final[which(colnames(item263.final) %notin% c("BuildingType"))]

exportTable(item263.final, "MF", "Table 55", weighted = FALSE,OS = T, osIndicator = "PSE")

