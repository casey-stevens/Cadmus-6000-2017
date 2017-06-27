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
# Windows
windows.doors.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, windows.export))
windows.dat1 <- windows.doors.dat[which(colnames(windows.doors.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Type"
                                                               ,"Frame./.Body.Type"
                                                               ,"Glazing.Type"
                                                               ,"Area"))]
windows.dat2 <- windows.dat1[which(windows.dat1$Type == "Window"),]
colnames(windows.dat2) <- c("CK_Cadmus_ID", "Window_Type", "Window_Area", "Frame_Body_Type", "Glazing_Type")

#Envelope for SF
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
# add indicator of whether a site has a basement or not
envelope.dat$BasementInd <- "No"
basement.tmp <- envelope.dat$CK_Cadmus_ID[which(envelope.dat$Floor.Type == "Basement")]
envelope.dat$BasementInd[which(envelope.dat$CK_Cadmus_ID %in% basement.tmp)] <- "Yes"
envelope.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID","Floor.Type", "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt", "BasementInd"))]
colnames(envelope.dat1) <- c("CK_Cadmus_ID", "Floor_Area" ,"Floor_Type", "BasementInd")

#Rooms for MH and MF
rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
rooms.dat1 <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID","CK_SiteID","Clean.Type","Area"))]
rooms.dat2 <- rooms.dat1[-grep("BLDG",rooms.dat1$CK_SiteID),]
colnames(rooms.dat2) <- c("CK_Cadmus_ID", "Site_ID", "Room_Type", "Room_Area")


#clean cadmus IDs
windows.dat2$CK_Cadmus_ID  <- trimws(toupper(windows.dat2$CK_Cadmus_ID))
envelope.dat1$CK_Cadmus_ID <- trimws(toupper(envelope.dat1$CK_Cadmus_ID))
rooms.dat2$CK_Cadmus_ID    <- trimws(toupper(rooms.dat2$CK_Cadmus_ID))




#############################################################################################
#Item 35: 
#############################################################################################
# Need to discuss
# Average window to floor area ratio
# Does this mean average window area / average floor area
# Fist average within homes, then average across homes within building type and floor area? I think so...


#windows
item35.windows  <- left_join(rbsa.dat, windows.dat2, by = "CK_Cadmus_ID")
length(unique(item35.windows$CK_Cadmus_ID)) #601

#floor area (SF ONLY)
item35.ENV <- left_join(rbsa.dat, envelope.dat1, by = "CK_Cadmus_ID")
length(unique(item35.ENV$CK_Cadmus_ID)) #601

#room area (MH AND MF)
item35.rooms    <- left_join(rbsa.dat, rooms.dat2, by = "CK_Cadmus_ID")
length(unique(item35.rooms$CK_Cadmus_ID)) #601


##########################################
# For Window Area Average
##########################################
# make numeric
item35.windows$Window_Area <- as.numeric(as.character(item35.windows$Window_Area))

# remove zeros (don't make sense)
item35.windows1 <- item35.windows[which(item35.windows$Window_Area > 0),]


item35.windows2 <- summarise(group_by(item35.windows1, CK_Cadmus_ID, BuildingType)
                             ,WindowAreaAve = mean(Window_Area))



#############################################################################################
#FOR SINGLE FAMILY
#############################################################################################

floor.dat <- item35.ENV[which(!(is.na(item35.ENV$Floor_Area))),]
length(unique(floor.dat$CK_Cadmus_ID)) #only 525

#convert to numeric
floor.dat$Floor_Area <- as.numeric(as.character(floor.dat$Floor_Area))

#keep only Floor area greater than zero
floor.dat1 <- floor.dat[which(floor.dat$Floor_Area > 0),]

## Subset to only single family
floor.dat2 <- floor.dat1[which(floor.dat1$BuildingType == "Single Family"),]

#summarize data by cadmus ID and building type
floor.sum <- summarise(group_by(floor.dat2, CK_Cadmus_ID, BuildingType, BasementInd)
                       ,FloorArea_Site = sum(Floor_Area))


##########################################
# Merge Window and Floor Area
##########################################
# merge window and room averages
item35.dat.sf <- left_join(item35.windows2, floor.sum, by = c("CK_Cadmus_ID", "BuildingType"))

# Subset to only single family
item35.dat.sf1 <- item35.dat.sf[which(item35.dat.sf$BuildingType == "Single Family"),]

#calculate the window to floor ratio
item35.dat.sf1$WindowToFloorArea <- item35.dat.sf1$WindowAreaAve / item35.dat.sf1$FloorArea_Site

#remove missing ratio information (missing floor area info)
item35.dat.sf2 <- item35.dat.sf1[which(!(is.na(item35.dat.sf1$WindowToFloorArea))),]

#summarise across sites
item35.sf.final <- summarise(group_by(item35.dat.sf2, BuildingType, BasementInd)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,AveWindowToFloorArea = mean(WindowToFloorArea)
                          ,SE                   = sd(WindowToFloorArea) / sqrt(SampleSize))





#############################################################################################
#FOR MANUFACTURED HOMES AND MULTIFAMILY
#############################################################################################
# ##########################################
# # For Room Area Average
# ##########################################
# # make numeric
# item35.rooms$Room_Area <- as.numeric(as.character(item35.rooms$Room_Area))
# 
# # remove zeros (don't make sense) and datapoint not asked for
# item35.rooms1 <- item35.rooms[which(item35.rooms$Room_Area > 0),]
# item35.rooms2 <- item35.rooms1[which(item35.rooms1$Room_Area != "-- Datapoint not asked for --"),]
# 
# item35.rooms3 <- summarise(group_by(item35.rooms2, CK_Cadmus_ID, BuildingType)
#                              ,RoomAreaAve = mean(Room_Area))
# 
# ##########################################
# # Merge Window and Room Area
# ##########################################
# # merge window and room averages
# item35.dat <- left_join(item35.windows2, item35.rooms3, by = c("CK_Cadmus_ID", "BuildingType"))
# 
# #subset to single family sites only
# item35.dat.sf <- item35.dat[which(item35.dat$BuildingType != "Single Family"),]
# item35.dat.sf$WindowToFloorArea <- item35.dat.sf$WindowAreaAve / item35.dat.sf$RoomAreaAve
# 
# #summarise across sites
# item35.final <- summarise(group_by(item35.dat.sf, BuildingType) #Add indicator of which sites have basements
#                           ,SampleSize = length(unique(CK_Cadmus_ID))
#                           ,AveWindowToFloorArea = mean(WindowToFloorArea)
#                           ,SE                   = sd(WindowToFloorArea) / sqrt(SampleSize))