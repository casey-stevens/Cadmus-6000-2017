#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)


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
lighting.dat3 <- lighting.dat2[grep("SITE", lighting.dat2$CK_SiteID),]

#subset to only rooms that are outside
unique(lighting.dat3$Clean.Room)
lighting.dat4 <- lighting.dat3[which(lighting.dat3$Clean.Room == "Outside"),]

lighting.dat5 <- lighting.dat4[which(!(is.na(lighting.dat4$Total.Wattage))),]


lighting.dat6 <- summarise(group_by(lighting.dat5, CK_Cadmus_ID)
                           ,Site_Wattage = sum(Total.Wattage))



lighting.room.dat <- left_join(rooms.dat4, lighting.dat6)




#############################################################################################
#Item 202: AVERAGE EXTERIOR LIGHTING POWER (WATTS) BY STATE (MH TABLE 65)
#############################################################################################

#merge together analysis data with cleaned RBSA data
item202.dat <- left_join(rbsa.dat, lighting.room.dat)
item202.dat <- item202.dat[which(!is.na(item202.dat$Site_Wattage)),]
colnames(item202.dat) 

################################################
# Adding pop and sample sizes for weights
################################################
item202.data <- weightedData(item202.dat[-which(colnames(item202.dat) %in% c("SiteArea"
                                                                                 ,"Site_Wattage"
                                                                                 ,"aveRval"))])
item202.data <- left_join(item202.data, item202.dat[which(colnames(item202.dat) %in% c("CK_Cadmus_ID"
                                                                                           ,"SiteArea"
                                                                                       ,"Site_Wattage"))])

#######################
# Weighted Analysis
#######################
item202.final <- mean_one_group(item202.data
                                ,valueVariable = 'Site_Wattage'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item202.final.MH <- item202.final[which(item202.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item202.final) %in% c("BuildingType"))]

exportTable(item202.final.MH, "MH", "Table 65", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item202.final <- mean_one_group_unweighted(item202.data
                                           ,valueVariable = 'Site_Wattage'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item202.final.MH <- item202.final[which(item202.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item202.final) %in% c("BuildingType"))]

exportTable(item202.final.MH, "MH", "Table 65", weighted = FALSE)




#############################################################################################
#Item 203: AVERAGE EXTERIOR LIGHTING LPD BY STATE (MH TABLE 67)
#############################################################################################
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
lighting.dat4 <- lighting.dat2[grep("SITE", lighting.dat2$CK_SiteID),]

lighting.dat5 <- lighting.dat4[which(!(is.na(lighting.dat4$Total.Wattage))),]
lighting.dat5$Total.Wattage <- as.numeric(as.character(lighting.dat5$Total.Wattage))

lighting.dat6 <- summarise(group_by(lighting.dat5, CK_Cadmus_ID)
                           ,Site_Wattage = sum(Total.Wattage))



lighting.room.dat <- left_join(rooms.dat4, lighting.dat6)


#merge together analysis data with cleaned RBSA data
item203.dat <- left_join(rbsa.dat, lighting.room.dat)
item203.dat <- item203.dat[which(!is.na(item203.dat$Site_Wattage)),]
colnames(item203.dat) 

################################################
# Adding pop and sample sizes for weights
################################################
item203.data <- weightedData(item203.dat[-which(colnames(item203.dat) %in% c("SiteArea"
                                                                             ,"Site_Wattage"
                                                                             ,"aveRval"))])
item203.data <- left_join(item203.data, item203.dat[which(colnames(item203.dat) %in% c("CK_Cadmus_ID"
                                                                                       ,"SiteArea"
                                                                                       ,"Site_Wattage"))])


#merge together analysis data with cleaned RBSA data
item203.data$LPD <- item203.data$Site_Wattage / item203.data$SiteArea


#######################
# Weighted Analysis
#######################
item203.final <- mean_one_group(item203.data
                                ,valueVariable = 'LPD'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item203.final.MH <- item203.final[which(item203.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item203.final) %in% c("BuildingType"))]

exportTable(item203.final.MH, "MH", "Table 66", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item203.final <- mean_one_group_unweighted(item203.data
                                           ,valueVariable = 'Site_Wattage'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item203.final.MH <- item203.final[which(item203.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item203.final) %in% c("BuildingType"))]

exportTable(item203.final.MH, "MH", "Table 66", weighted = FALSE)
