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

item77.dat1 <- left_join(item77.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item77.dat2 <- item77.dat1[-grep("BLDG", item77.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item77.dat2$Fixture.Qty <- as.numeric(as.character(item77.dat2$Fixture.Qty))
item77.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item77.dat2$LIGHTING_BulbsPerFixture))

item77.dat2$Lamps <- item77.dat2$Fixture.Qty * item77.dat2$LIGHTING_BulbsPerFixture
unique(item77.dat2$Lamps)

item77.dat3 <- item77.dat2[which(!(is.na(item77.dat2$Lamps))),]

## subset to only storage bulbs
item77.storage <- item77.dat3[which(item77.dat3$Clean.Room == "Storage"),]
#summarise within site
item77.storage.sum <- summarise(group_by(item77.storage, BuildingType, State)
                                 ,StorageBulbs = sum(Lamps))
#summarise by state
item77.storage.state1 <- summarise(group_by(item77.storage.sum, BuildingType, State)
                                   ,Storage = mean(StorageBulbs))
#summarise by state
item77.storage.region1 <- summarise(group_by(item77.storage.sum, BuildingType)
                                   ,State = "Region"
                                   ,Storage = mean(StorageBulbs))

#row bind state and region level information for storage lamps
item77.storage.total <- rbind.data.frame(item77.storage.state1, item77.storage.region1, stringsAsFactors = F)



## use full data (with and without storage bulbs)
item77.allBulbs <- item77.dat3
#summarise within site
item77.all.sum <- summarise(group_by(item77.allBulbs, BuildingType, State)
                                ,SampleSize = length(unique(CK_Cadmus_ID))
                                ,allBulbs = sum(Lamps))
item77.regionSS <- summarise(group_by(item77.allBulbs, BuildingType)
                                ,State = "Region"
                                ,SampleSize = length(unique(CK_Cadmus_ID))
                                ,allBulbs = sum(Lamps))

#Get Sample Size information
item77.SampleSize <- rbind.data.frame(item77.all.sum, item77.regionSS, stringsAsFactors = F)
item77.SampleSize1 <- item77.SampleSize[which(colnames(item77.SampleSize) %in% c("BuildingType","State","SampleSize"))]

#summarise by state
item77.all.state1 <- summarise(group_by(item77.all.sum, BuildingType, State)
                                   ,Total = mean(allBulbs))
#summarise across states
item77.all.region1 <- summarise(group_by(item77.all.sum, BuildingType)
                                    ,State = "Region"
                                    ,Total = mean(allBulbs))
#row bind state and region level information for all lamp categories
item77.all.total <- rbind.data.frame(item77.all.state1, item77.all.region1, stringsAsFactors = F)

#left join together storage and combined results
item77.final <- left_join(item77.all.total, item77.storage.total, by = c("BuildingType","State"))

# left join on sample size information
item77.final1 <- left_join(item77.final, item77.SampleSize1, by = c("BuildingType","State"))

#Make NA storage to zero
item77.final1$Storage[which(is.na(item77.final1$Storage))] <- 0
#calculate percent and SE(percent)
item77.final1$Percent <- item77.final1$Storage / item77.final1$Total
item77.final1$SE      <- sqrt(item77.final1$Percent * (1 - item77.final1$Percent) / item77.final1$SampleSize)
#subset to only wanted columns
item77.table <- data.frame("BuildingType" = item77.final1$BuildingType
                           ,"State" = item77.final1$State
                           ,"Percent" = item77.final1$Percent
                           ,"SE" = item77.final1$SE
                           ,"SampleSize" = item77.final1$SampleSize)
#subset to only relevant building types
item77.table1 <- item77.table[which(item77.table$BuildingType %in% c("Single Family", "Manufactured")),]







#############################################################################################
#Item 78: PERCENTAGE OF ALL CFLS THAT ARE STORED (SF table 84, MH table 63)
#############################################################################################

item78.envelope <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID","ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]
colnames(item78.envelope) <- c("CK_Cadmus_ID","Area")

item78.area <- item78.envelope[which(!(item78.envelope$Area %in% c("0", "Unknown", NA))),]
unique(item78.area$Area)
item78.area$Area <- as.numeric(as.character(item78.area$Area))

item78.area1 <- summarise(group_by(item78.area, CK_Cadmus_ID)
                          ,SiteArea = mean(Area))

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

item78.dat1 <- left_join(item78.merge, rbsa.dat, by = "CK_Cadmus_ID")

item78.dat2 <- item78.dat1[-grep("BLDG", item78.dat1$CK_SiteID),]

item78.dat3 <- item78.dat2[which(!(item78.dat2$Clean.Room %in% c("Basement","Storage"))),]

item78.dat4 <- item78.dat3[-grep("-|Unknown|unknown", item78.dat3$Clean.Wattage),]
item78.dat5 <- item78.dat4[which(!(is.na(item78.dat4$Clean.Wattage))),]
item78.dat5$Clean.Wattage <- as.numeric(as.character(item78.dat5$Clean.Wattage))
item78.dat5$SiteArea      <- as.numeric(as.character(item78.dat5$SiteArea))
unique(item78.dat5$Clean.Wattage)
str(item78.dat5)

item78.sum <- summarise(group_by(item78.dat5, CK_Cadmus_ID, BuildingType, State)
                        ,Wattage = sum(Clean.Wattage)
                        ,SQFT    = unique(SiteArea))

item78.sum$LPD <- item78.sum$Wattage / item78.sum$SQFT

item78.dat6 <- item78.sum[which(!(is.na(item78.sum$LPD))),]

item78.state <- summarise(group_by(item78.dat6, BuildingType, State)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Mean = mean(LPD)
                         ,SE   = sd(LPD) / sqrt(SampleSize))
item78.region <- summarise(group_by(item78.dat6, BuildingType)
                          ,State = "Region"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean = mean(LPD)
                          ,SE   = sd(LPD) / sqrt(SampleSize))

item78.final <- rbind.data.frame(item78.state, item78.region, stringsAsFactors = F)





#table format
item78.table <- data.frame("BuildingType" = item78.final$BuildingType
                           ,"State" = item78.final$State
                           ,"Mean" = item78.final$Mean
                           ,"SE" = item78.final$SE
                           ,"SampleSize" = item78.final$SampleSize)

#subset to only relevant building types
item78.table1 <- item78.table[which(item78.table$BuildingType %in% c("Single Family", "Manufactured")),]















#############################################################################################
#Item 79: PERCENTAGE OF ALL CFLS THAT ARE STORED (SF table 84, MH table 63)
#############################################################################################
item79.rooms <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID","Clean.Type","Area"))]
colnames(item79.rooms) <- c("CK_Cadmus_ID","Clean.Room","Area")

item79.area <- item79.rooms[which(!(item79.rooms$Area %in% c("0", "Unknown", NA, "-- Datapoint not asked for --"))),]
unique(item79.area$Area)
item79.area$Area <- as.numeric(as.character(item79.area$Area))

item79.area1 <- summarise(group_by(item79.area, CK_Cadmus_ID, Clean.Room)
                          ,SiteArea = sum(Area))

#subset to columns needed for analysis
item79.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Clean.Room"
                                                               ,"Clean.Wattage"))]
item79.dat$count <- 1

#merge on area information
item79.merge <- left_join(item79.dat, item79.area1, by = c("CK_Cadmus_ID", "Clean.Room"))

item79.dat1 <- left_join(item79.merge, rbsa.dat, by = "CK_Cadmus_ID")

item79.dat2 <- item79.dat1[-grep("-|Unknown|unknown", item79.dat1$Clean.Wattage),]
item79.dat3 <- item79.dat2[which(!(is.na(item79.dat2$Clean.Wattage))),]
item79.dat3$Clean.Wattage <- as.numeric(as.character(item79.dat3$Clean.Wattage))
item79.dat3$SiteArea      <- as.numeric(as.character(item79.dat3$SiteArea))
unique(item79.dat3$Clean.Wattage)
str(item79.dat3)

item79.sum <- summarise(group_by(item79.dat3, CK_Cadmus_ID, BuildingType, Clean.Room)
                        ,Wattage = sum(Clean.Wattage)
                        ,SQFT    = sum(unique(SiteArea)))

item79.sum$LPD <- item79.sum$Wattage / item79.sum$SQFT

item79.dat6 <- item79.sum[which(!(is.na(item79.sum$SQFT))),]

item79.room <- summarise(group_by(item79.dat6, BuildingType, Clean.Room)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean = mean(LPD)
                          ,SE   = sd(LPD) / sqrt(SampleSize))
item79.allRooms <- summarise(group_by(item79.dat6, BuildingType)
                           ,Clean.Room = "All Room Types"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(LPD)
                           ,SE   = sd(LPD) / sqrt(SampleSize))

item79.final <- rbind.data.frame(item79.room, item79.allRooms, stringsAsFactors = F)



#table format
item79.table <- data.frame("BuildingType" = item79.final$BuildingType
                           ,"Clean.Room" = item79.final$Clean.Room
                           ,"Mean" = item79.final$Mean
                           ,"SE" = item79.final$SE
                           ,"SampleSize" = item79.final$SampleSize)

#subset to only relevant building types
item79.table1 <- item79.table[which(item79.table$BuildingType %in% c("Single Family", "Manufactured")),]
