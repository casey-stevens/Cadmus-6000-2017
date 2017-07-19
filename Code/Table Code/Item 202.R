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
#Item 202: AVERAGE EXTERIOR LIGHTING POWER (WATTS) BY STATE (MH TABLE 65)
#############################################################################################

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

rooms.dat4 <- summarise(group_by(rooms.dat3, CK_Cadmus_ID, Clean.Room)
                          ,SiteArea = sum(Area))


#merge together analysis data with cleaned RBSA data
rooms.datXX <- left_join(rooms.dat4, rbsa.dat, by = "CK_Cadmus_ID")



#############################################################################################
#For Lighting
#############################################################################################

#subset to columns needed for analysis
lighting.dat1 <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"Fixture.Qty"
                                                                  ,"LIGHTING_BulbsPerFixture"
                                                                  ,"CK_SiteID"
                                                                  ,"Lamp.Category"
                                                                  ,"Clean.Room"))]

#remove any repeat header rows from exporting
lighting.dat2 <- lighting.dat1[which(lighting.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#remove any BLDG info
lighting.dat3 <- lighting.dat2[-grep("BLDG", lighting.dat2$CK_SiteID),]

#subset to only rooms that are outside
unique(lighting.dat3$Clean.Room)
lighting.dat4 <- lighting.dat3[which(lighting.dat3$Clean.Room == "Outside"),]



#merge together analysis data with cleaned RBSA data
item202.dat <- left_join(lighting.dat3, rooms.dat3, by = c("CK_Cadmus_ID","Clean.Room"))






