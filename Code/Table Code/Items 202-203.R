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
rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
#clean cadmus IDs
rooms.dat$CK_Cadmus_ID <- trimws(toupper(rooms.dat$CK_Cadmus_ID))


#############################################################################################
#For Rooms
#############################################################################################

#subset to columns needed for analysis
rooms.dat1 <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID"
                                                          ,"Clean.Type"
                                                          ,"Area"))]
colnames(rooms.dat1) <- c("CK_Cadmus_ID","Clean.Room","Area")

#remove any repeat header rows from exporting
rooms.dat2 <- rooms.dat1[which(rooms.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

rooms.dat3 <- rooms.dat2[which(!(rooms.dat2$Area %in% c("0", "Unknown", NA, "-- Datapoint not asked for --"))),]
rooms.dat3$Area <- as.numeric(as.character(rooms.dat3$Area))

rooms.dat4 <- summarise(group_by(rooms.dat3, CK_Cadmus_ID)
                          ,SiteArea = sum(Area))




#merge together analysis data with cleaned RBSA data
rooms.dat5 <- left_join(rooms.dat4, rbsa.dat, by = "CK_Cadmus_ID")



#############################################################################################
#For Lighting
#############################################################################################

#subset to columns needed for analysis
lighting.dat1 <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"Fixture.Qty"
                                                                  ,"LIGHTING_BulbsPerFixture"
                                                                  ,"CK_SiteID"
                                                                  ,"Lamp.Category"
                                                                  ,"Clean.Room"
                                                                  ,"Clean.Wattage"))]
lighting.dat1$Total.Wattage <- as.numeric(as.character(lighting.dat1$Fixture.Qty)) * 
  as.numeric(as.character(lighting.dat1$LIGHTING_BulbsPerFixture)) * 
  as.numeric(as.character(lighting.dat1$Clean.Wattage))


#remove any repeat header rows from exporting
lighting.dat2 <- lighting.dat1[which(lighting.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#remove any BLDG info
lighting.dat3 <- lighting.dat2[-grep("BLDG", lighting.dat2$CK_SiteID),]

#subset to only rooms that are outside
unique(lighting.dat3$Clean.Room)
lighting.dat4 <- lighting.dat3[which(lighting.dat3$Clean.Room == "Outside"),]

lighting.dat5 <- lighting.dat4[which(!(is.na(lighting.dat4$Total.Wattage))),]


lighting.dat6 <- summarise(group_by(lighting.dat5, CK_Cadmus_ID)
                           ,Site_Wattage = sum(Total.Wattage))








#############################################################################################
#Item 202: AVERAGE EXTERIOR LIGHTING POWER (WATTS) BY STATE (MH TABLE 65)
#############################################################################################

#merge together analysis data with cleaned RBSA data
item202.dat <- left_join(lighting.dat6, rooms.dat5, by = c("CK_Cadmus_ID"))

#summarise by state
item202.state <- summarise(group_by(item202.dat, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Site_Wattage)
                           ,SE = sd(Site_Wattage) / sqrt(SampleSize))
#summarise across states
item202.region <- summarise(group_by(item202.dat, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean = mean(Site_Wattage)
                            ,SE = sd(Site_Wattage) / sqrt(SampleSize))

item202.final <- rbind.data.frame(item202.state, item202.region, stringsAsFactors = F)

item202.table <- item202.final[which(item202.final$BuildingType == "Manufactured"),]

item202.table2 <- data.frame("BuildingType" = item202.table$BuildingType
                             ,"State" = item202.table$State
                             ,"Mean" = item202.table$Mean
                             ,"SE" = item202.table$SE
                             ,"SampleSize" = item202.table$SampleSize)



#############################################################################################
#Item 203: AVERAGE EXTERIOR LIGHTING LPD BY STATE (MH TABLE 67)
#############################################################################################

#merge together analysis data with cleaned RBSA data
item203.dat <- left_join(lighting.dat6, rooms.dat5, by = c("CK_Cadmus_ID"))

item203.dat$LPD <- item203.dat$Site_Wattage / item203.dat$SiteArea

#summarise by state
item203.state <- summarise(group_by(item203.dat, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = sum(LPD * SiteArea) / sum(SiteArea)
                           ,SE = sd(LPD) / sqrt(SampleSize))
#summarise across states
item203.region <- summarise(group_by(item203.dat, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean = sum(LPD * SiteArea) / sum(SiteArea)
                            ,SE = sd(LPD) / sqrt(SampleSize))

item203.final <- rbind.data.frame(item203.state, item203.region, stringsAsFactors = F)

item203.table <- item203.final[which(item203.final$BuildingType == "Manufactured"),]

item203.table2 <- data.frame("BuildingType" = item203.table$BuildingType
                             ,"State" = item203.table$State
                             ,"Mean" = item203.table$Mean
                             ,"SE" = item203.table$SE
                             ,"SampleSize" = item203.table$SampleSize)
