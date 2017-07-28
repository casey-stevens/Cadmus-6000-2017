#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export))
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))

#Read in data for analysis
buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Cadmus_ID <- trimws(toupper(buildings.dat$CK_Cadmus_ID))




#############################################################################################
#Item 260: AVERAGE COMMON AREA LPD (W/SQ.FT.) BY BUILDING VINTAGE (MF Table 52)
#############################################################################################
item260.buildings <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Cadmus_ID"
                                                            ,"SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"))]

item260.buildings1 <- item260.buildings[which(!(is.na(item260.buildings$SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea))),]

item260.buildings2 <- summarise(group_by(item260.buildings1, CK_Cadmus_ID)
                            ,CommonFloorArea = sum(SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea))

item260.buildings3 <- item260.buildings2[which(item260.buildings2$CommonFloorArea > 0),]


#subset to columns needed for analysis
item260.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"CK_SiteID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Wattage"
                                                                ,"Clean.Room"
                                                                ,"Switch.Type"))]
item260.dat$count <- 1

item260.dat00 <- item260.dat[which(!(item260.dat$Clean.Room %in% c("Outside", "Storage"))),]

#join clean rbsa data onto lighting analysis data
item260.dat0 <- left_join(item260.dat00, rbsa.dat, by = "CK_Cadmus_ID")

#remove missing vintage info
item260.dat1 <- item260.dat0[which(!(is.na(item260.dat0$HomeYearBuilt_MF))),]

#remove building info
item260.dat2 <- item260.dat1[grep("BLDG", item260.dat1$CK_SiteID),]

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
item260.dat6 <- summarise(group_by(item260.dat5, CK_Cadmus_ID, BuildingTypeXX, HomeYearBuilt_MF)
                          ,SiteWattage = sum(Total.Wattage))

#merge on building data
item260.dat7 <- left_join(item260.dat6, item260.buildings3, by = "CK_Cadmus_ID")

#remove NA
item260.dat8 <- item260.dat7[which(!(is.na(item260.dat7$CommonFloorArea))),]

item260.dat8$LPD <- item260.dat8$SiteWattage / item260.dat8$CommonFloorArea

#summarise by vintage
item260.sum1 <- summarise(group_by(item260.dat8, HomeYearBuilt_MF)
                          ,Mean = mean(LPD)
                          ,SE = sd(LPD) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#summarise across vintage
item260.sum2 <- summarise(group_by(item260.dat8)
                          ,HomeYearBuilt_MF = "All Vintages"
                          ,Mean = mean(LPD)
                          ,SE = sd(LPD) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#merge
item260.final <- rbind.data.frame(item260.sum1, item260.sum2, stringsAsFactors = F)
















#############################################################################################
#Item 261: AVERAGE COMMON AREA LPD (W/SQ.FT.) BY BUILDING SIZE (MF Table 53)
#############################################################################################
item261.dat <- item260.dat8

#summarise by building size
item261.sum1 <- summarise(group_by(item261.dat, BuildingTypeXX)
                          ,Mean = mean(LPD)
                          ,SE = sd(LPD) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#summarise across building sizes
item261.sum2 <- summarise(group_by(item261.dat)
                          ,BuildingTypeXX = "All Sizes"
                          ,Mean = mean(LPD)
                          ,SE = sd(LPD) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#merge
item261.final <- rbind.data.frame(item261.sum1, item261.sum2, stringsAsFactors = F)





#############################################################################################
#Item 262: AVERAGE COMMON AREA ROOM LPD (W/SQ.FT.) BY BUILDING SIZE (MF Table 54)
#############################################################################################
item262.dat <- item260.dat5

#summarise up to the site level
item262.dat1 <- summarise(group_by(item262.dat, CK_Cadmus_ID, BuildingTypeXX, HomeYearBuilt_MF, Clean.Room)
                          ,SiteWattage = sum(Total.Wattage))

#merge on building data
item262.dat2 <- left_join(item262.dat1, item260.buildings3, by = "CK_Cadmus_ID")

#remove NA
item262.dat3 <- item262.dat2[which(!(is.na(item262.dat2$CommonFloorArea))),]

item262.dat3$LPD <- item262.dat3$SiteWattage / item262.dat3$CommonFloorArea

#summarise by Building size
# by room type
item262.sum1 <- summarise(group_by(item262.dat3, BuildingTypeXX, Clean.Room)
                          ,Mean = mean(LPD)
                          ,SE = sd(LPD) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

# across room type
item262.sum2 <- summarise(group_by(item262.dat3, BuildingTypeXX)
                          ,Clean.Room = "All Rooms"
                          ,Mean = mean(LPD)
                          ,SE = sd(LPD) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#summarise across Building size
# by room type
item262.sum3 <- summarise(group_by(item262.dat3, Clean.Room)
                          ,BuildingTypeXX = "All Sizes"
                          ,Mean = mean(LPD)
                          ,SE = sd(LPD) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

# across room type
item262.sum4 <- summarise(group_by(item262.dat3)
                          ,BuildingTypeXX = "All Sizes"
                          ,Clean.Room = "All Rooms"
                          ,Mean = mean(LPD)
                          ,SE = sd(LPD) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#merge
item262.final <- rbind.data.frame(item262.sum1, item262.sum2, item262.sum3, item262.sum4, stringsAsFactors = F)

#cast
item262.cast <- dcast(setDT(item262.final)
                      ,formula = Clean.Room ~ BuildingTypeXX
                      ,value.var = c("Mean", "SE", "SampleSize"))

#make into table format
item262.table <- data.frame("Common.Area.Room.Type" = item262.cast$Clean.Room
                            ,"Low.Rise.1.3.Mean" = item262.cast$`Mean_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.SE" = item262.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6.Mean" = NA#item262.cast$`Mean_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.SE" = NA#item262.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.7.Plus.Mean" = NA#item262.cast$`Mean_Apartment Building (]6 or more floors)`
                            ,"High.Rise.7.Plus.SE" = NA#item262.cast$`SE_Apartment Building (]6 or more floors)`
                            ,"All.Sizes.Mean" = item262.cast$`Mean_All Sizes`
                            ,"All.Sizes.SE" = item262.cast$`SE_All Sizes`
                            ,"SampleSize" = item262.cast$`SampleSize_All Sizes`)
                            
            





#############################################################################################
#Item 263: DISTRIBUTION OF COMMON AREA LIGHTING POWER (WATTS) BY CONTROL TYPE (MF Table 55)
#############################################################################################
item263.dat <- item260.dat5
item263.dat$Switch.Type[grep("On/off", item263.dat$Switch.Type)] <- "Manual Switch"
item263.dat$Switch.Type[grep("Other", item263.dat$Switch.Type)] <- "Other"



#summarise up to the site level
item263.dat1 <- summarise(group_by(item263.dat, CK_Cadmus_ID, Switch.Type)
                          ,Site.Wattage = sum(Total.Wattage))

#
item263.site.sum1 <- summarise(group_by(item263.dat1, Switch.Type)
                               ,SampleSize = length(unique(CK_Cadmus_ID))
                               ,Wattage = sum(Site.Wattage))

#
item263.site.sum2 <- summarise(group_by(item263.dat1)
                               ,Total.Wattage = sum(Site.Wattage))

item263.dat2 <- data.frame(item263.site.sum1, item263.site.sum2, stringsAsFactors = F)
item263.dat2$Percent <- item263.dat2$Wattage / item263.dat2$Total.Wattage
item263.dat2$SE <- sqrt(item263.dat2$Percent * (1 - item263.dat2$Percent) / item263.dat2$SampleSize)

item263.sub <- item263.dat2[which(colnames(item263.dat2) %in% c("Switch.Type", "Percent", "SE"))]

item263.table <- data.frame(item263.sub, "SampleSize" = item263.dat2$SampleSize, stringsAsFactors = F)

