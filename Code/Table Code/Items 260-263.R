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

#Read in data for analysis
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))

#Read in data for analysis
buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))






#############################################################################################
#Item 260: AVERAGE COMMON AREA LPD (W/SQ.FT.) BY BUILDING VINTAGE (MF Table 52)
#############################################################################################
item260.buildings <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
                                                            ,"SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"))]

item260.buildings1 <- item260.buildings[which(!(is.na(item260.buildings$SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea))),]

item260.buildings2 <- summarise(group_by(item260.buildings1, CK_Building_ID)
                            ,CommonFloorArea = sum(SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea))

item260.buildings3 <- item260.buildings2[which(item260.buildings2$CommonFloorArea > 0),]


#subset to columns needed for analysis
item260.lighting <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"CK_SiteID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Wattage"
                                                                ,"Clean.Room"
                                                                ,"Switch.Type"))]
item260.lighting$count <- 1

item260.lighting1 <- item260.lighting[which(!(item260.lighting$Clean.Room %in% c("Outside", "Storage"))),]






#join clean rbsa data onto lighting analysis data
item260.dat1 <- left_join(rbsa.dat, item260.lighting1)

#remove building info
item260.dat2 <- item260.dat1[grep("BLDG", item260.dat1$CK_Building_ID),]

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
item260.dat6 <- summarise(group_by(item260.dat5, CK_Cadmus_ID)
                          ,SiteWattage = sum(Total.Wattage))

#merge on building data
item260.dat7 <- left_join(rbsa.dat, item260.dat6)
item260.merge <- left_join(item260.dat7, item260.buildings3)

#remove NA
item260.merge1 <- item260.merge[which(!(is.na(item260.merge$CommonFloorArea))),]
item260.merge1 <- item260.merge1[which(!(is.na(item260.merge1$SiteWattage))),]
item260.merge1 <- item260.merge1[which(!(is.na(item260.merge1$HomeYearBuilt_MF))),]


item260.merge1$LPD <- item260.merge1$SiteWattage / item260.merge1$CommonFloorArea

######################################
#Pop and Sample Sizes for weights
######################################
item260.data <- weightedData(item260.merge1[which(colnames(item260.merge1) %notin% c("SiteWattage"
                                                                                   ,"CommonFloorArea"
                                                                                   ,"LPD"))])

item260.data <- left_join(item260.data, item260.merge1[which(colnames(item260.merge1) %in% c("CK_Cadmus_ID"
                                                                                           ,"SiteWattage"
                                                                                           ,"CommonFloorArea"
                                                                                           ,"LPD"))])
item260.data$count <- 1


######################
# weighted analysis
######################
item260.final <- mean_one_group(CustomerLevelData = item260.data
                                ,valueVariable = 'LPD'
                                ,byVariable = 'HomeYearBuilt_bins_MF'
                                ,aggregateRow = "All Vintages")

item260.final <- item260.final[which(colnames(item260.final) %notin% c("BuildingType"))]

exportTable(item260.final, "MF", "Table 52", weighted = TRUE)



######################
# unweighted analysis
######################
item260.final <- mean_one_group_unweighted(CustomerLevelData = item260.data
                                ,valueVariable = 'LPD'
                                ,byVariable = 'HomeYearBuilt_bins_MF'
                                ,aggregateRow = "All Vintages")

item260.final <- item260.final[which(colnames(item260.final) %notin% c("BuildingType"))]

exportTable(item260.final, "MF", "Table 52", weighted = FALSE)






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
                                                                                     ,"LPD"))])

item261.data <- left_join(item261.data, item261.merge1[which(colnames(item261.merge1) %in% c("CK_Cadmus_ID"
                                                                                             ,"SiteWattage"
                                                                                             ,"CommonFloorArea"
                                                                                             ,"LPD"))])
item261.data$count <- 1


######################
# weighted analysis
######################
item261.final <- mean_one_group(CustomerLevelData = item261.data
                                ,valueVariable = 'LPD'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")

item261.final <- item261.final[which(colnames(item261.final) %notin% c("BuildingType", "n_h", "N_h"))]

exportTable(item261.final, "MF", "Table 53", weighted = TRUE)



######################
# unweighted analysis
######################
item261.final <- mean_one_group_unweighted(CustomerLevelData = item261.data
                                           ,valueVariable = 'LPD'
                                           ,byVariable = 'HomeType'
                                           ,aggregateRow = "All Vintages")

item261.final <- item261.final[which(colnames(item261.final) %notin% c("BuildingType", "n_h", "N_h"))]

exportTable(item261.final, "MF", "Table 53", weighted = FALSE)






#############################################################################################
#Item 262: AVERAGE COMMON AREA ROOM LPD (W/SQ.FT.) BY COMMON AREA ROOM TYPE (MF Table 54)
#############################################################################################
item262.dat <- item260.dat5

#summarise up to the site level
item262.dat1 <- summarise(group_by(item262.dat, CK_Cadmus_ID, Clean.Room)
                          ,SiteWattage = sum(Total.Wattage))

#merge on building data
item262.dat2 <- left_join(rbsa.dat, item262.dat1)
item262.merge <- left_join(item262.dat2, item260.buildings3)

#remove NA
item262.merge1 <- item262.merge[which(!(is.na(item262.merge$CommonFloorArea))),]
item262.merge1 <- item262.merge1[which(!(is.na(item262.merge1$SiteWattage))),]

item262.merge1$LPD <- item262.merge1$SiteWattage / item262.merge1$CommonFloorArea

######################################
#Pop and Sample Sizes for weights
######################################
item262.data <- weightedData(item262.merge1[which(colnames(item262.merge1) %notin% c("SiteWattage"
                                                                                     ,"CommonFloorArea"
                                                                                     ,"LPD"
                                                                                     ,"Clean.Room"))])

item262.data <- left_join(item262.data, item262.merge1[which(colnames(item262.merge1) %in% c("CK_Cadmus_ID"
                                                                                             ,"SiteWattage"
                                                                                             ,"CommonFloorArea"
                                                                                             ,"LPD"
                                                                                             ,"Clean.Room"))])
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

#make into table format
item262.table <- data.frame("Common.Area.Room.Type" = item262.cast$Clean.Room
                            ,"Low.Rise.1.3.Mean"    = item262.cast$`Mean_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.SE"      = item262.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.n"       = item262.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6.Mean"    = item262.cast$`Mean_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.SE"      = item262.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.n"       = item262.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.7.Plus.Mean"= item262.cast$`Mean_Apartment Building (More than 6 floors)`
                            ,"High.Rise.7.Plus.SE"  = item262.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"High.Rise.7.Plus.n"   = item262.cast$`n_Apartment Building (More than 6 floors)`
                            ,"All.Sizes.Mean"       = item262.cast$`Mean_All Sizes`
                            ,"All.Sizes.SE"         = item262.cast$`SE_All Sizes`
                            ,"All.Sizes.n"          = item262.cast$`n_All Sizes`)

exportTable(item262.table, "MF", "Table 54", weighted = TRUE)

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
                            ,"Mid.Rise.4.6.Mean"    = item262.cast$`Mean_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.SE"      = item262.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.n"       = item262.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.7.Plus.Mean"= item262.cast$`Mean_Apartment Building (More than 6 floors)`
                            ,"High.Rise.7.Plus.SE"  = item262.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"High.Rise.7.Plus.n"   = item262.cast$`n_Apartment Building (More than 6 floors)`
                            ,"All.Sizes.Mean"       = item262.cast$`Mean_All Sizes`
                            ,"All.Sizes.SE"         = item262.cast$`SE_All Sizes`
                            ,"All.Sizes.n"          = item262.cast$`n_All Sizes`)

exportTable(item262.table, "MF", "Table 54", weighted = FALSE)

            





#############################################################################################
#Item 263: DISTRIBUTION OF COMMON AREA LIGHTING POWER (WATTS) BY CONTROL TYPE (MF Table 55)
#############################################################################################
item263.dat <- item260.dat5
unique(item263.dat$Switch.Type)
item263.dat$Switch.Type[grep("On/off|3", item263.dat$Switch.Type, ignore.case = T)] <- "Manual Switch"
item263.dat$Switch.Type[grep("Other", item263.dat$Switch.Type, ignore.case = T)] <- "Other"
item263.dat$Switch.Type[grep("Timer", item263.dat$Switch.Type, ignore.case = T)] <- "Timer Control"
item263.dat$Switch.Type[grep("Dimmer", item263.dat$Switch.Type, ignore.case = T)] <- "Dimmer Switch"
item263.dat$Switch.Type[grep("Unknown", item263.dat$Switch.Type, ignore.case = T)] <- "Unknown Switch"


#summarise up to the site level
item263.dat1 <- summarise(group_by(item263.dat, CK_Cadmus_ID, Switch.Type)
                          ,Site.Wattage = sum(Total.Wattage))

item263.merge <- left_join(rbsa.dat, item263.dat1)
item263.merge <- item263.merge[grep("BLDG", item263.merge$CK_Building_ID),]
item263.merge <- item263.merge[which(!is.na(item263.merge$Site.Wattage)),]

######################################
#Pop and Sample Sizes for weights
######################################
item263.data <- weightedData(item263.merge[which(colnames(item263.merge) %notin% c("Site.Wattage"
                                                                                     ,"Switch.Type"))])

item263.data <- left_join(item263.data, item263.merge[which(colnames(item263.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Site.Wattage"
                                                                                           ,"Switch.Type"))])
item263.data$count <- 1


#############################
# weighted analysis
#############################
item263.final <- proportions_one_group(CustomerLevelData = item263.data
                                          ,valueVariable = 'Site.Wattage'
                                          ,groupingVariable = 'Switch.Type'
                                          ,total.name = NA
                                          ,weighted = TRUE)
item263.final <- item263.final[which(item263.final$Switch.Type != "Total"),]
item263.final <- item263.final[which(colnames(item263.final) %notin% c("BuildingType"))]

exportTable(item263.final, "MF", "Table 55", weighted = TRUE)

#############################
# uneighted analysis
#############################
item263.final <- proportions_one_group(CustomerLevelData = item263.data
                                          ,valueVariable = 'Site.Wattage'
                                          ,groupingVariable = 'Switch.Type'
                                          ,total.name = NA
                                          ,weighted = FALSE)
item263.final <- item263.final[which(item263.final$Switch.Type != "Total"),]
item263.final <- item263.final[which(colnames(item263.final) %notin% c("BuildingType"))]

exportTable(item263.final, "MF", "Table 55", weighted = FALSE)

