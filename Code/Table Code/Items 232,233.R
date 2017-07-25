#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
# Windows
windows.doors.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, windows.export))
windows.doors.dat$CK_Cadmus_ID <- trimws(toupper(windows.doors.dat$CK_Cadmus_ID))

#select columns for windows
windows.dat1 <- windows.doors.dat[which(colnames(windows.doors.dat) %in% c("CK_Cadmus_ID"
                                                                           ,"Wall.Area"
                                                                           ,"Type"
                                                                           ,"Area"
                                                                           ,"ENV_Fenestration_WINDOWS_NumOfWindowsFacingEast",
                                                                           "ENV_Fenestration_WINDOWS_NumOfWindowsFacingNorth",
                                                                           "ENV_Fenestration_WINDOWS_NumOfWindowsFacingNortheast",
                                                                           "ENV_Fenestration_WINDOWS_NumOfWindowsFacingNorthwest",
                                                                           "ENV_Fenestration_WINDOWS_NumOfWindowsFacingSouth",
                                                                           "ENV_Fenestration_WINDOWS_NumOfWindowsFacingSoutheast",
                                                                           "ENV_Fenestration_WINDOWS_NumOfWindowsFacingSouthwest",
                                                                           "ENV_Fenestration_WINDOWS_NumOfWindowsFacingWest"))]
windows.dat2 <- windows.dat1[which(windows.dat1$Type == "Window"),]

colnames(windows.dat2) <- c("CK_Cadmus_ID", "Window_Type", "Window_Area")

#read in Envelope data for MF table
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))
envelope.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"Wall.Area"
                                                                  ,"Type"
                                                                  ,"Area"
                                                                  ,"ENV_Fenestration_WINDOWS_NumOfWindowsFacingEast",
                                                                  "ENV_Fenestration_WINDOWS_NumOfWindowsFacingNorth",
                                                                  "ENV_Fenestration_WINDOWS_NumOfWindowsFacingNortheast",
                                                                  "ENV_Fenestration_WINDOWS_NumOfWindowsFacingNorthwest",
                                                                  "ENV_Fenestration_WINDOWS_NumOfWindowsFacingSouth",
                                                                  "ENV_Fenestration_WINDOWS_NumOfWindowsFacingSoutheast",
                                                                  "ENV_Fenestration_WINDOWS_NumOfWindowsFacingSouthwest",
 
                                                                                                                                   "ENV_Fenestration_WINDOWS_NumOfWindowsFacingWest"))]
#############################################################################################
#Item 232: Table 24
#############################################################################################
item232.windows  <- left_join(rbsa.dat, windows.dat2, by = "CK_Cadmus_ID")
length(unique(item232.windows$CK_Cadmus_ID)) 
#Subset to MF
item232.windows1 <- item232.windows[grep("Multifamily", item232.windows$BuildingType),]

item232.env <- left_join(rbsa.dat, envelope.dat1, by = "CK_Cadmus_ID")
length(unique(item232.env$CK_Cadmus_ID)) #601
item232.env1 <- item232.env[grep("Multifamily", item232.env$BuildingType),]

##########################################
# For Window Area Average
##########################################
# make numeric
item232.windows1$Window_Area <- as.numeric(as.character(item232.windows1$Window_Area))

# remove zeros (don't make sense)
item232.windows2 <- item232.windows1[which(item232.windows1$Window_Area > 0),]


item232.windows3 <- summarise(group_by(item232.windows2, CK_Cadmus_ID, BuildingTypeXX)
                             ,WindowArea = sum(Window_Area))

##########################################
# For WALL Area Average
##########################################
item232.env1$Wall.Area <- as.numeric(as.character(item232.env1$Wall.Area))
# remove zeros (don't make sense)
item232.env2 <- item232.env1[which(item232.env1$Wall.Area > 0),]


item232.env3 <- summarise(group_by(item232.env2, CK_Cadmus_ID, BuildingTypeXX)
                              ,WallArea = sum(Wall.Area))

##########################################
# Merge Window and Floor Area
##########################################

item232.dat <- left_join(item232.windows3, item232.env3, by = c("CK_Cadmus_ID", "BuildingTypeXX"))

#calculate the window to wall ratio
item232.dat$WindowToWallArea <- item232.dat$WindowArea / item232.dat$WallArea

#remove missing ratio information (missing floor area info)
item232.dat1 <- item232.dat[which(!(is.na(item232.dat$WindowToWallArea))),]

