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

#Read in data for analysis -- Item 292
rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
#clean cadmus IDs
rooms.dat$CK_Cadmus_ID <- trimws(toupper(rooms.dat$CK_Cadmus_ID))


#############################################################################################
#Item 290: LIGHTING CHARACTERISTICS (MF Table 82)
#############################################################################################
item290.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"CK_SiteID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"Clean.Wattage"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Room"))]

item290.dat$Total.Lamps <- as.numeric(as.character(item290.dat$Fixture.Qty)) * as.numeric(as.character(item290.dat$LIGHTING_BulbsPerFixture))
unique(item290.dat$Total.Lamps)

item290.dat1 <- item290.dat[which(!(item290.dat$Total.Lamps %in% c(NA))),]

item290.dat2 <- left_join(item290.dat1, rbsa.dat, by = "CK_Cadmus_ID")

item290.dat3 <- item290.dat2[which(item290.dat2$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item290.dat4 <- item290.dat3[-grep("BLDG", item290.dat3$CK_SiteID),]

item290.dat5 <- item290.dat4[grep("Multifamily", item290.dat4$BuildingType),]

item290.dat5$Lamp.Category[which(item290.dat5$Lamp.Category %in% c("Unknown", "Incandescent / Halogen"))] <- "Other"



##################################
# For Fixtures and lamps per unit
##################################
item290.site <- summarise(group_by(item290.dat5, CK_Cadmus_ID)
                          ,Total.Unit.Fixtures = sum(Fixture.Qty)
                          ,Total.Unit.Lamps = sum(Total.Lamps))

item290.fixture <- summarise(item290.site
                             ,Category = "Fixtures per Unit"
                             ,Mean = mean(Total.Unit.Fixtures)
                             ,SE = sd(Total.Unit.Fixtures) / sqrt(length(unique(CK_Cadmus_ID)))
                             ,SampleSize = length(unique(CK_Cadmus_ID)))
item290.lamp <- summarise(item290.site
                          ,Category = "Lamps per Unit"
                          ,Mean = mean(Total.Unit.Lamps)
                          ,SE = sd(Total.Unit.Lamps) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
item290.site.sum <- rbind.data.frame(item290.fixture, item290.lamp, stringsAsFactors = F)


##################################
# For specific bulb types per unit
##################################

item290.cast <- dcast(setDT(item290.dat5)
                      ,formula = CK_Cadmus_ID ~ Lamp.Category, sum
                      ,value.var = "Total.Lamps")

colnames(item290.cast)

item290.cfl <- summarise(item290.cast
                         ,Category = "CFL Lamps per Unit"
                         ,Mean = mean(`Compact Fluorescent`)
                         ,SE = sd(`Compact Fluorescent`) / sqrt(length(unique(CK_Cadmus_ID)))
                         ,SampleSize = length(unique(CK_Cadmus_ID)))

item290.halogen <- summarise(item290.cast
                             ,Category = "Halogen Lamps per Unit"
                             ,Mean = mean(Halogen)
                             ,SE = sd(Halogen) / sqrt(length(unique(CK_Cadmus_ID)))
                             ,SampleSize = length(unique(CK_Cadmus_ID)))

item290.incandescent <- summarise(item290.cast
                                  ,Category = "Incandescent Lamps per Unit"
                                  ,Mean = mean(Incandescent)
                                  ,SE = sd(Incandescent) / sqrt(length(unique(CK_Cadmus_ID)))
                                  ,SampleSize = length(unique(CK_Cadmus_ID)))

item290.LED <- summarise(item290.cast
                         ,Category = "LED Lamps per Unit"
                         ,Mean = mean(`Light Emitting Diode`)
                         ,SE = sd(`Light Emitting Diode`) / sqrt(length(unique(CK_Cadmus_ID)))
                         ,SampleSize = length(unique(CK_Cadmus_ID)))

item290.Linear.Fluorescent <- summarise(item290.cast
                                        ,Category = "Linear Fluorescent Lamps per Unit"
                                        ,Mean = mean(`Linear Fluorescent`)
                                        ,SE = sd(`Linear Fluorescent`) / sqrt(length(unique(CK_Cadmus_ID)))
                                        ,SampleSize = length(unique(CK_Cadmus_ID)))

item290.Other <- summarise(item290.cast
                           ,Category = "Other Lamps per Unit"
                           ,Mean = mean(Other)
                           ,SE = sd(Other) / sqrt(length(unique(CK_Cadmus_ID)))
                           ,SampleSize = length(unique(CK_Cadmus_ID)))

item290.lamp.types <- rbind.data.frame(item290.cfl
                                       ,item290.halogen
                                       ,item290.incandescent
                                       ,item290.LED
                                       ,item290.Linear.Fluorescent
                                       ,item290.Other)

##############################
# Combine results
##############################

item290.final <- rbind.data.frame(item290.site.sum, item290.lamp.types, stringsAsFactors = F)














#############################################################################################
#Item 291: DISTRIBUTION OF LAMPS BY TYPE (MF Table 83)
#############################################################################################
item291.dat <- item290.dat4
item291.dat1 <- item291.dat[grep("Multifamily", item291.dat$BuildingType),]

item291.dat1$Lamp.Category[which(item291.dat1$Lamp.Category %in% c("Incandescent / Halogen"))] <- "Other"


item291.dat$count <- 1

item291.sum1 <- summarise(group_by(item291.dat, Lamp.Category)
                         ,Count = sum(count)
                         ,SampleSize = length(unique(CK_Cadmus_ID)))

item291.sum2 <- summarise(group_by(item291.dat)
                         ,Total.Count = sum(count)
                         ,Denom.SampleSize = length(unique(CK_Cadmus_ID)))

item291.final <- cbind.data.frame(item291.sum1, item291.sum2)
item291.final$Percent <- item291.final$Count / item291.final$Total.Count
item291.final$SE <- sqrt(item291.final$Percent * (1 - item291.final$Percent) / item291.final$Denom.SampleSize)

item291.table <- data.frame("Lamp.Category" = item291.final$Lamp.Category
                            ,"Percent" = item291.final$Percent
                            ,"SE" = item291.final$SE
                            ,"SampleSize" = item291.final$SampleSize)











#############################################################################################
#Item 292: AVERAGE LIGHTING POWER DENSITY (LPD) BY ROOM TYPE AND OVERALL (MF Table 84)
#############################################################################################
############
# For Rooms
############
item292.rooms <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID","Clean.Type","Area"))]
colnames(item292.rooms) <- c("CK_Cadmus_ID","Clean.Room","Area")

item292.area <- item292.rooms[which(!(item292.rooms$Area %in% c("0", "Unknown", NA, "-- Datapoint not asked for --"))),]
unique(item292.area$Area)
item292.area$Area <- as.numeric(as.character(item292.area$Area))

item292.area1 <- summarise(group_by(item292.area, CK_Cadmus_ID, Clean.Room)
                          ,SiteArea = sum(Area))


############
# For Lighting
############
item292.dat <- item290.dat4
item292.dat1 <- item292.dat[grep("Multifamily", item292.dat$BuildingType),]

item292.dat1$Lamp.Category[which(item292.dat1$Lamp.Category %in% c("Incandescent / Halogen"))] <- "Other"
item292.dat1$Total.Wattage <- item292.dat1$Total.Lamps * as.numeric(as.character(item292.dat1$Clean.Wattage))

item292.dat2 <- item292.dat1[which(!(is.na(item292.dat1$Total.Wattage))),]

item292.dat3 <- summarise(group_by(item292.dat2, CK_Cadmus_ID, BuildingType, State, Clean.Room)
                         ,Total.Wattage = sum(Total.Wattage))


item292.dat4 <- left_join(item292.dat3, item292.area1, by = c("CK_Cadmus_ID", "Clean.Room"))

item292.dat4$LPD <- item292.dat4$Total.Wattage / item292.dat4$SiteArea

item292.dat5 <- item292.dat4[which(!(is.na(item292.dat4$LPD))),]


#summarise by clean room
item292.sum1 <- summarise(group_by(item292.dat5, Clean.Room)
                          ,Mean = mean(LPD)
                          ,SE = sd(LPD) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#summarise across clean room
item292.sum2 <- summarise(group_by(item292.dat5)
                          ,Clean.Room = "Unit Lighting Power Density"
                          ,Mean = mean(LPD)
                          ,SE = sd(LPD) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))


item292.final <- rbind.data.frame(item292.sum1, item292.sum2, stringsAsFactors = F)

item292.table <- data.frame("Room.Type" = item292.final$Clean.Room
                            ,"Mean" = item292.final$Mean
                            ,"SE" = item292.final$SE
                            ,"SampleSize" = item292.final$SampleSize)
