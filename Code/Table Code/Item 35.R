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
length(unique(rbsa.dat$CK_Cadmus_ID)) 

#Read in data for analysis
# Windows
windows.doors.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, windows.export))
windows.doors.dat$CK_Cadmus_ID <- trimws(toupper(windows.doors.dat$CK_Cadmus_ID))

#select columns for windows
windows.dat1 <- windows.doors.dat[which(colnames(windows.doors.dat) %in% c("CK_Cadmus_ID"
                                                                           ,"Type"
                                                                           ,"Frame./.Body.Type"
                                                                           ,"Glazing.Type"
                                                                           ,"Area"
                                                                           ,"Quantity"))]
#subset to only windows
windows.dat2 <- windows.dat1[which(windows.dat1$Type == "Window"),]
#rename columns
colnames(windows.dat2) <- c("CK_Cadmus_ID", "Window_Type", "Window_Area", "Window_QTY", "Frame_Body_Type", "Glazing_Type")

#read in Envelope data for SF table
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

# add indicator of whether a site has a basement or not
envelope.dat$BasementInd <- 0
basement.tmp <- envelope.dat$CK_Cadmus_ID[which(envelope.dat$Floor.Type == "Basement")]
envelope.dat$BasementInd[which(envelope.dat$CK_Cadmus_ID %in% basement.tmp)] <- 1
envelope.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"Floor.Type"
                                                                  # ,"ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"
                                                                  ,"BasementInd"))]
colnames(envelope.dat1) <- c("CK_Cadmus_ID"
                             # ,"Floor_Area"
                             ,"Floor_Type" 
                             ,"BasementInd")
env.buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
env.buildings.dat$CK_Cadmus_ID <- trimws(toupper(env.buildings.dat$CK_Cadmus_ID))
env.buildings.dat <- env.buildings.dat[which(colnames(env.buildings.dat) %in% c("CK_Cadmus_ID"
                                                                                ,"SITES_MFB_cfg_MFB_CONFIG_TotEnclosedBldgArea_IncludResidentialAndCommercialButExcludPkgGarages "))]
colnames(env.buildings.dat) <- c("CK_Cadmus_ID"
                                 ,"Floor_Area")

envelope.dat1 <- left_join(envelope.dat1, env.buildings.dat)

#Rooms for MH and MF
rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
rooms.dat$CK_Cadmus_ID <- trimws(toupper(rooms.dat$CK_Cadmus_ID))

rooms.dat1 <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID","CK_SiteID","Clean.Type","Area"))]
rooms.dat2 <- rooms.dat1[grep("SITE",rooms.dat1$CK_SiteID),]
colnames(rooms.dat2) <- c("CK_Cadmus_ID", "Site_ID", "Room_Type", "Room_Area")



#############################################################################################
#Item 35: 
#############################################################################################
# Average window to floor area ratio
# Fist average within homes, then average across homes within building type and floor area? I think so...

#windows
item35.windows  <- left_join(rbsa.dat, windows.dat2, by = "CK_Cadmus_ID")
length(unique(item35.windows$CK_Cadmus_ID))

#floor area (SF ONLY)
item35.ENV <- left_join(rbsa.dat, envelope.dat1, by = "CK_Cadmus_ID")
length(unique(item35.ENV$CK_Cadmus_ID))

#room area (MH only)
item35.rooms    <- left_join(rbsa.dat, rooms.dat2, by = "CK_Cadmus_ID")
length(unique(item35.rooms$CK_Cadmus_ID))


##########################################
# For Window Area Average
##########################################
# make numeric
item35.windows$Window_Area <- as.numeric(as.character(item35.windows$Window_Area)) * as.numeric(as.character(item35.windows$Window_QTY))

# remove zeros (don't make sense)
item35.windows0 <- item35.windows[which(!(is.na(item35.windows$Window_Area))),]
item35.windows1 <- item35.windows0[which(item35.windows0$Window_Area > 0),]


item35.windows2 <- summarise(group_by(item35.windows1, CK_Cadmus_ID)
                             ,WindowArea = sum(Window_Area))



#############################################################################################
#FOR SINGLE FAMILY
#############################################################################################

floor.dat <- item35.ENV#[which(!(is.na(item35.ENV$Floor_Area))),]
length(unique(floor.dat$CK_Cadmus_ID)) 

#convert to numeric
floor.dat$Floor_Area <- as.numeric(as.character(floor.dat$Conditioned.Area))

#keep only Floor area greater than zero
floor.dat1 <- floor.dat[which(floor.dat$Floor_Area > 0),]

## Subset to only single family
floor.dat2 <- floor.dat1[which(floor.dat1$BuildingType == "Single Family"),]

#summarize data by cadmus ID and building type
floor.sum <- summarise(group_by(floor.dat2, CK_Cadmus_ID)
                       ,BasementInd = sum(unique(BasementInd))
                       ,FloorArea_Site = unique(Floor_Area))


##########################################
# Merge Window and Floor Area
##########################################
# merge window and room averages
item35.dat <- left_join(item35.windows2, floor.sum)

# Subset to only single family
item35.dat1 <- item35.dat[which(!is.na(item35.dat$BasementInd)),]

#calculate the window to floor ratio
item35.dat1$WindowToFloorArea <- item35.dat1$WindowArea / item35.dat1$FloorArea_Site

#remove missing ratio information (missing floor area info)
item35.dat2 <- item35.dat1[which(!(is.na(item35.dat1$WindowToFloorArea))),]

item35.dat2$Basement <- item35.dat2$BasementInd
item35.dat2$Basement[which(item35.dat2$BasementInd == 0)] <- "Home without Basements"
item35.dat2$Basement[which(item35.dat2$BasementInd == 1)] <- "Home with Basements"

item35.merge <- left_join(rbsa.dat, item35.dat2)
item35.merge <- item35.merge[which(!is.na(item35.merge$Basement)),]


################################################
# Adding pop and sample sizes for weights
################################################
item35.data <- weightedData(item35.merge[-which(colnames(item35.merge) %in% c("WindowArea"
                                                                              ,"BasementInd"
                                                                              ,"FloorArea_Site"
                                                                              ,"WindowToFloorArea"
                                                                              ,"Basement"))])
item35.data <- left_join(item35.data, item35.merge[which(colnames(item35.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"WindowArea"
                                                                                       ,"BasementInd"
                                                                                       ,"FloorArea_Site"
                                                                                       ,"WindowToFloorArea"
                                                                                       ,"Basement"))])
item35.data$count <- 1
#######################
# Weighted Analysis
#######################
item35.final <- mean_one_group(item35.data
                                ,valueVariable = 'WindowToFloorArea'
                                ,byVariable = 'Basement'
                                ,aggregateRow = 'All Homes')

item35.final.SF <- item35.final[which(item35.final$BuildingType == "Single Family")
                                  ,-which(colnames(item35.final) %in% c("BuildingType"))]

exportTable(item35.final.SF, "SF", "Table 42", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item35.final <- mean_one_group_unweighted(item35.data
                                          ,valueVariable = 'WindowToFloorArea'
                                          ,byVariable = 'Basement'
                                          ,aggregateRow = 'All Homes')

item35.final.SF <- item35.final[which(item35.final$BuildingType == "Single Family")
                                  ,-which(colnames(item35.final) %in% c("BuildingType"))]

exportTable(item35.final.SF, "SF", "Table 42", weighted = FALSE)

