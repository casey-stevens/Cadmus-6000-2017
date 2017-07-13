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


#############################################################################################
#Item 68: DISTRIBUTION OF LAMPS BY EISA CATEGORY AND STATE (SF table 75, MH table 54)
#############################################################################################
#subset to columns needed for analysis
item68.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,""
                                                               ,""
                                                               ,""
                                                               ,""
                                                               ,""))]
item68.dat$count <- 1

item68.dat1 <- left_join(item68.dat, rbsa.dat, by = "CK_Cadmus_ID")

item68.dat2 <- item68.dat1[-grep("BLDG", item68.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item68.dat2$Fixture.Qty <- as.numeric(as.character(item68.dat2$Fixture.Qty))
item68.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item68.dat2$LIGHTING_BulbsPerFixture))

item68.dat2$Lamps <- item68.dat2$Fixture.Qty * item68.dat2$LIGHTING_BulbsPerFixture
unique(item68.dat2$Lamps)

item68.dat3 <- item68.dat2[which(!(is.na(item68.dat2$Lamps))),]






















#############################################################################################
#Item 69: DISTRIBUTION OF LAMPS BY TYPE AND STATE (SF table 76, MH table 55)
#############################################################################################
#subset to columns needed for analysis
item69.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"))]
item69.dat$count <- 1

#join clean rbsa data onto lighting analysis data
item69.dat1 <- left_join(item69.dat, rbsa.dat, by = "CK_Cadmus_ID")

#remove building info
item69.dat2 <- item69.dat1[-grep("BLDG", item69.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item69.dat2$Fixture.Qty <- as.numeric(as.character(item69.dat2$Fixture.Qty))
item69.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item69.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item69.dat2$Lamps <- item69.dat2$Fixture.Qty * item69.dat2$LIGHTING_BulbsPerFixture
unique(item69.dat2$Lamps)

#remove missing lamp quantities
item69.dat3 <- item69.dat2[which(!(is.na(item69.dat2$Lamps))),]

#check lamp types
unique(item69.dat3$Lamp.Category)

## For State
## by lamp type
item69.state1 <- summarise(group_by(item69.dat3, BuildingType, State, Lamp.Category)
                           ,sampleSize = length(unique(CK_Cadmus_ID))
                           ,LampCount = sum(Lamps))
## across lamp types
item69.state2 <- summarise(group_by(item69.dat3, BuildingType, State)
                           ,Lamp.Category = "Total"
                           ,sampleSize = length(unique(CK_Cadmus_ID))
                           ,LampCount = sum(Lamps))
## join state info by and across lamp types  
item69.state <- rbind.data.frame(item69.state1, item69.state2, stringsAsFactors = F)

## For region
## by lamp type
item69.region1 <- summarise(group_by(item69.dat3, BuildingType, Lamp.Category)
                            ,sampleSize = length(unique(CK_Cadmus_ID))
                            ,State = "Region"
                           ,LampCount = sum(Lamps))
## across lamp types
item69.region2 <- summarise(group_by(item69.dat3, BuildingType)
                            ,State = "Region"
                           ,Lamp.Category = "Total"
                           ,sampleSize = length(unique(CK_Cadmus_ID))
                           ,LampCount = sum(Lamps))
## join region info by and across lamp types  
item69.region <- rbind.data.frame(item69.region1, item69.region2, stringsAsFactors = F)

## join state and region info
item69.dat4 <- rbind.data.frame(item69.state, item69.region, stringsAsFactors = F)

##get total counts
item69.totalcount <- rbind.data.frame(item69.state2, item69.region2, stringsAsFactors = F)
#join on total counts
item69.dat5 <- left_join(item69.dat4, item69.totalcount, by = c("BuildingType", "State"))

#rename columns
colnames(item69.dat5) <- c("BuildingType"
                           ,"State"
                           ,"Lamp.Category"
                           ,"SampleSize"
                           ,"Count"
                           ,"Remove"
                           ,"Remove"
                           ,"Total")
item69.dat6 <- item69.dat5[which(colnames(item69.dat5) != "Remove")]

item69.dat6$Percent <- item69.dat6$Count / item69.dat6$Total
item69.dat6$SE      <- sqrt(item69.dat6$Percent * (1 - item69.dat6$Percent) / item69.dat6$SampleSize)

library(data.table)
item69.dat.cast <- dcast(setDT(item69.dat6)
                         , formula = BuildingType + Lamp.Category ~ State
                         , value.var = c("Percent", "SE", "SampleSize"))

item69.final <- item69.dat.cast[which(item69.dat.cast$BuildingType %in% c("Single Family"
                                                                           ,"Manufactured"))]
item69.final <- data.frame(item69.final, stringsAsFactors = F)
item69.final1 <- item69.final[which(colnames(item69.final) %in% c("BuildingType"
                                                                  ,"Lamp.Category"
                                                                  ,"Percent_MT"
                                                                  ,"Percent_OR"
                                                                  ,"Percent_WA"
                                                                  ,"Percent_Region"
                                                                  ,"SE_MT"
                                                                  ,"SE_OR"
                                                                  ,"SE_WA"
                                                                  ,"SE_Region"
                                                                  ,"SampleSize_Region"))]
item69.table <- data.frame("BuildingType" = item69.final1$BuildingType
                           ,"Lamp.Category" = item69.final1$Lamp.Category
                           ,"Percent_MT" = item69.final1$Percent_MT
                           ,"SE_MT" = item69.final1$SE_MT
                           ,"Percent_OR" = item69.final1$Percent_OR
                           ,"SE_OR" = item69.final1$SE_OR
                           ,"Percent_WA" = item69.final1$Percent_WA
                           ,"SE_WA" = item69.final1$SE_WA
                           ,"Percent_Region" = item69.final1$Percent_Region
                           ,"SE_Region" = item69.final1$SE_Region
                           ,"SampleSize" = item69.final1$SampleSize_Region)

item69.table1 <- item69.table[which(item69.table$BuildingType %in% c("Single Family", "Manufactured")),]







#############################################################################################
#Item 70: DISTRIBUTION OF LAMPS BY TYPE AND ROOM (SF table 77, MH table 56, MF table 84)
#############################################################################################
#subset to columns needed for analysis
item70.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"))]
item70.dat$count <- 1

#join clean rbsa data onto lighting analysis data
item70.dat1 <- left_join(item70.dat, rbsa.dat, by = "CK_Cadmus_ID")

item70.dat1.1 <- item70.dat1[which(!(item70.dat1$Clean.Room %in% c("Basement","Storage"))),]
item70.dat1.1$Clean.Room[which(item70.dat1.1$Clean.Room == "Mechanical")] <- "Other"
#remove building info
item70.dat2 <- item70.dat1.1[-grep("BLDG", item70.dat1.1$CK_SiteID),]

#clean fixture and bulbs per fixture
item70.dat2$Fixture.Qty <- as.numeric(as.character(item70.dat2$Fixture.Qty))
item70.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item70.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item70.dat2$Lamps <- item70.dat2$Fixture.Qty * item70.dat2$LIGHTING_BulbsPerFixture
unique(item70.dat2$Lamps)

#remove missing lamp quantities
item70.dat3 <- item70.dat2[which(!(is.na(item70.dat2$Lamps))),]

#check lamp types
unique(item70.dat3$Lamp.Category)

## For Room Type
## by lamp type
item70.room1 <- summarise(group_by(item70.dat3, BuildingType, Clean.Room, Lamp.Category)
                           ,sampleSize = length(unique(CK_Cadmus_ID))
                           ,LampCount = sum(Lamps))
## across lamp types
item70.room2 <- summarise(group_by(item70.dat3, BuildingType, Clean.Room)
                           ,Lamp.Category = "Total"
                           ,sampleSize = length(unique(CK_Cadmus_ID))
                           ,LampCount = sum(Lamps))
## join state info by and across lamp types  
item70.room <- rbind.data.frame(item70.room1, item70.room2
                                , stringsAsFactors = F)

## Across Room Types
## by lamp type
item70.allRooms1 <- summarise(group_by(item70.dat3, BuildingType, Lamp.Category)
                            ,sampleSize = length(unique(CK_Cadmus_ID))
                            ,Clean.Room = "All Rooms"
                            ,LampCount = sum(Lamps))
## across lamp types
item70.allRooms2 <- summarise(group_by(item70.dat3, BuildingType)
                            ,Lamp.Category = "Total"
                            ,sampleSize = length(unique(CK_Cadmus_ID))
                            ,Clean.Room = "All Rooms"
                            ,LampCount = sum(Lamps))
## join region info by and across lamp types  
item70.allRooms <- rbind.data.frame(item70.allRooms1, item70.allRooms2, stringsAsFactors = F)

## join state and region info
item70.dat4 <- rbind.data.frame(item70.room, item70.allRooms, stringsAsFactors = F)

##get total counts
item70.totalcount <- rbind.data.frame(item70.room2, item70.allRooms2, stringsAsFactors = F)
#join on total counts
item70.dat5 <- left_join(item70.dat4, item70.totalcount, by = c("BuildingType", "Clean.Room"))

#rename columns
colnames(item70.dat5) <- c("BuildingType"
                           ,"Clean.Room"
                           ,"Lamp.Category"
                           ,"SampleSize"
                           ,"Count"
                           ,"Remove"
                           ,"Remove"
                           ,"Total")
item70.dat6 <- item70.dat5[which(colnames(item70.dat5) != "Remove")]

item70.dat6$Percent <- item70.dat6$Count / item70.dat6$Total
item70.dat6$SE      <- sqrt(item70.dat6$Percent * (1 - item70.dat6$Percent) / item70.dat6$SampleSize)

library(data.table)
item70.dat.cast <- dcast(setDT(item70.dat6)
                         , formula = BuildingType + Clean.Room ~ Lamp.Category
                         , value.var = c("Percent", "SE", "SampleSize"))

item70.final <- data.frame(item70.dat.cast, stringsAsFactors = F)
item70.final1 <- item70.final[which(colnames(item70.final) %in% c("BuildingType"
                                                                  ,"Clean.Room"
                                                                  ,"Percent_Compact.Fluorescent"
                                                                  ,"Percent_Halogen"
                                                                  ,"Percent_Incandescent"
                                                                  ,"Percent_Incandescent...Halogen"
                                                                  ,"Percent_Light.Emitting.Diode"
                                                                  ,"Percent_Linear.Fluorescent"
                                                                  ,"Percent_Other"
                                                                  ,"Percent_Unknown"
                                                                  ,"Percent_Total"
                                                                  ,"SampleSize_Total"))]
item70.table <- data.frame("BuildingType"                 = item70.final1$BuildingType
                           ,"Clean.Room"                   = item70.final1$Clean.Room
                           ,"CFL Percent"                  = item70.final1$Percent_Compact.Fluorescent
                           ,"Halogen Percent"              = item70.final1$Percent_Halogen
                           ,"Incandescent Percent"         = item70.final1$Percent_Incandescent
                           ,"Incandescent/Halogen Percent" = item70.final1$Percent_Incandescent...Halogen
                           ,"LED Percent"                  = item70.final1$Percent_Light.Emitting.Diode
                           ,"Linear.Fluorescent Percent"   = item70.final1$Percent_Linear.Fluorescent
                           ,"Other Percent"                = item70.final1$Percent_Other
                           ,"Unknown Percent"              = item70.final1$Percent_Unknown
                           # ,"Total Percent"                = item70.final1$Percent_Total
                           ,"Sample.Size"                  = item70.final1$SampleSize_Total)
item70.table1 <- item70.table[which(item70.table$BuildingType %in% c("Single Family", "Manufactured")),]

