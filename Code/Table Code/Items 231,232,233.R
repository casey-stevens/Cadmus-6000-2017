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
windows.dat <- windows.doors.dat[which(windows.doors.dat$Type == "Window"),]


#read in Envelope data for MF table
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#############################################################################################
#Item 232: Table 24
#############################################################################################
##########################################
# Clean up window data
##########################################
windows.dat1 <- windows.dat[which(colnames(windows.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"Area"))]
windows.dat2  <- left_join(rbsa.dat, windows.dat1, by = "CK_Cadmus_ID")
length(unique(item232.windows1$CK_Cadmus_ID)) 
#Subset to MF
windows.dat3 <- windows.dat2[grep("Multifamily", item232.windows1$BuildingType),]

##########################################
# Calculate average window size per building
##########################################
# make numeric
windows.dat3$Window_Area <- as.numeric(as.character(item232.windows2$Area))
# remove zeros (don't make sense)
windows.dat4 <- windows.dat3[which(windows.dat3$Window_Area > 0),]


windows.dat5 <- summarise(group_by(windows.dat4, CK_Cadmus_ID, BuildingTypeXX)
                              ,AvgWindowArea = mean(Window_Area))

##########################################
# Clean up envelope data
##########################################
envelope.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Wall.Area"
                                                               ,"Floor.Area"
                                                               ,"Type"
                                                               ,"Area",
                                                               "ENV_Fenestration_WINDOWS_NumOfWindowsFacingEast",
                                                               "ENV_Fenestration_WINDOWS_NumOfWindowsFacingNorth",
                                                               "ENV_Fenestration_WINDOWS_NumOfWindowsFacingNortheast",
                                                               "ENV_Fenestration_WINDOWS_NumOfWindowsFacingNorthwest",
                                                               "ENV_Fenestration_WINDOWS_NumOfWindowsFacingSouth",
                                                               "ENV_Fenestration_WINDOWS_NumOfWindowsFacingSoutheast",
                                                               "ENV_Fenestration_WINDOWS_NumOfWindowsFacingSouthwest",
                                                               "ENV_Fenestration_WINDOWS_NumOfWindowsFacingWest"))]
envelope.dat2 <- left_join(rbsa.dat, envelope.dat1, by = "CK_Cadmus_ID")
length(unique(item232.env1$CK_Cadmus_ID)) #601
envelope.dat3 <- envelope.dat2[grep("Multifamily", item232.env1$BuildingType),]
envelope.dat3$NumWindows <- 
  as.numeric(as.character(envelope.dat3$ENV_Fenestration_WINDOWS_NumOfWindowsFacingNorth)) +
  as.numeric(as.character(envelope.dat3$ENV_Fenestration_WINDOWS_NumOfWindowsFacingNortheast)) +
  as.numeric(as.character(envelope.dat3$ENV_Fenestration_WINDOWS_NumOfWindowsFacingNorthwest)) +
  as.numeric(as.character(envelope.dat3$ENV_Fenestration_WINDOWS_NumOfWindowsFacingSouth)) + 
  as.numeric(as.character(envelope.dat3$ENV_Fenestration_WINDOWS_NumOfWindowsFacingSoutheast)) +
  as.numeric(as.character(envelope.dat3$ENV_Fenestration_WINDOWS_NumOfWindowsFacingSouthwest)) +
  as.numeric(as.character(envelope.dat3$ENV_Fenestration_WINDOWS_NumOfWindowsFacingWest)) +
  as.numeric(as.character(envelope.dat3$ENV_Fenestration_WINDOWS_NumOfWindowsFacingEast))

##########################################
# Calculate total number of windows and wall area per building
##########################################
envelope.dat4 <- summarise(group_by(envelope.dat3, CK_Cadmus_ID, BuildingTypeXX)
                          ,NumWindows = sum(NumWindows,na.rm = T)
                          ,WallArea   = sum(Wall.Area, na.rm = T)
                          ,FloorArea  = sum(Floor.Area,na.rm = T))


##########################################
# Merge Window and Wall Area
##########################################
item232.dat <- left_join(windows.dat5, envelope.dat4, by = c("CK_Cadmus_ID", "BuildingTypeXX"))
item232.dat$WindowArea <- item232.dat$AvgWindowArea * item232.dat$NumWindows
#Remove any items where walla area and window are are not greater than zero.

item232.dat1 <- item232.dat[which(item232.dat$WallArea > 0 & 
                                    item232.dat$WindowArea > 0),]


#calculate the window to wall ratio
item232.dat1$WindowToWallArea <- item232.dat1$WindowArea / item232.dat1$WallArea

##########################################
# Summary
##########################################

item232.sum1 <- summarise(group_by(item232.dat1, BuildingTypeXX)
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean       = mean(WindowToWallArea)
                            ,SE         = sd(WindowToWallArea) / sqrt(SampleSize))

item232.sum2 <- summarise(group_by(item232.dat1)
                          ,BuildingTypeXX = "All Sizes"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean       = mean(WindowToWallArea)
                          ,SE         = sd(WindowToWallArea) / sqrt(SampleSize))

item232.sum <- rbind.data.frame(item232.sum1,
                                item232.sum2,
                                stringsAsFactors = F)
item232.final <- data.frame("BuildingSize" = item232.sum$BuildingTypeXX,
                            "Mean" = item232.sum$Mean,
                            "SE" = item232.sum$SE,
                            "SampleSize" = item232.sum$SampleSize,stringsAsFactors = F)
#############################################################################################
#Item 233: Table 25
#############################################################################################
##########################################
# Merge Window and Floor Area
##########################################
item233.dat <- left_join(windows.dat5, envelope.dat4, by = c("CK_Cadmus_ID", "BuildingTypeXX"))
item233.dat$WindowArea <- item233.dat$AvgWindowArea * item233.dat$NumWindows
#Remove any items where walla area and window are are not greater than zero.

item233.dat1 <- item233.dat[which(item233.dat$FloorArea > 0 & 
                                    item233.dat$WindowArea > 0),]


#calculate the window to wall ratio
item233.dat1$WindowToFloorArea <- item233.dat1$WindowArea / item233.dat1$FloorArea

##########################################
# Summary
##########################################

item233.sum1 <- summarise(group_by(item233.dat1, BuildingTypeXX)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean       = mean(WindowToFloorArea)
                          ,SE         = sd(WindowToFloorArea) / sqrt(SampleSize))

item233.sum2 <- summarise(group_by(item233.dat1)
                          ,BuildingTypeXX = "All Sizes"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Mean       = mean(WindowToFloorArea)
                          ,SE         = sd(WindowToFloorArea) / sqrt(SampleSize))

item233.sum <- rbind.data.frame(item233.sum1,
                                item233.sum2,
                                stringsAsFactors = F)
item233.final <- data.frame("BuildingSize" = item233.sum$BuildingTypeXX,
                            "Mean" = item233.sum$Mean,
                            "SE" = item233.sum$SE,
                            "SampleSize" = item233.sum$SampleSize,stringsAsFactors = F)
