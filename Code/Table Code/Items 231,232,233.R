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
#select columns for windows
windows.dat1 <- windows.dat[which(colnames(windows.dat) %in% c("CK_Cadmus_ID"
                                                                           ,"Type"
                                                                           ,"Frame./.Body.Type"
                                                                           ,"Glazing.Type"
                                                                           ,"Area"
                                                                           ,"Quantity"))]
windows.dat2  <- left_join(rbsa.dat, windows.dat1, by = "CK_Cadmus_ID")
length(unique(windows.dat2$CK_Cadmus_ID)) 
#Subset to MF
windows.dat3 <- windows.dat2[grep("Multifamily", windows.dat2$BuildingType),]

#read in Envelope data for MF table
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))
envelope.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"Wall.Area"
                                                                  ,"Floor.Area"
                                                                  ,"Type"
                                                                  ,"Area"))]

envelope.dat2 <- left_join(rbsa.dat, envelope.dat1, by = "CK_Cadmus_ID")
length(unique(envelope.dat2$CK_Cadmus_ID)) #601
envelope.dat3 <- envelope.dat2[grep("Multifamily", envelope.dat2$BuildingType),]

#############################################################################################
#Item 232: Table 24
#############################################################################################

##########################################
# Calculate average window size per building
##########################################
# make numeric
item232.windows <- windows.dat3
item232.windows$Window_Area <- as.numeric(as.character(item232.windows$Area))*as.numeric(as.character(item232.windows$Quantity))
# remove zeros (don't make sense)
item232.windows1 <- item232.windows[which(item232.windows$Window_Area > 0),]
item232.windows2 <- summarise(group_by(item232.windows1, CK_Cadmus_ID, BuildingTypeXX)
                              ,Window_Area = sum(Window_Area))


##########################################
# Calculate total number of windows and wall area per building
##########################################
item232.wall <- summarise(group_by(envelope.dat3, CK_Cadmus_ID, BuildingTypeXX)
                          ,WallArea   = sum(Wall.Area,na.rm = T))


##########################################
# Merge Window and Wall Area
##########################################
item232.dat <- left_join(item232.windows2, item232.wall, by = c("CK_Cadmus_ID", "BuildingTypeXX"))
#Remove any items where walla area and window are are not greater than zero.

item232.dat1 <- item232.dat[which(item232.dat$WallArea > 0 & 
                                    item232.dat$Window_Area > 0),]


#calculate the window to wall ratio
item232.dat1$WindowToWallArea <- item232.dat1$Window_Area / item232.dat1$WallArea

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
item233.windows <- windows.dat3
item233.windows$Window_Area <- as.numeric(as.character(item233.windows$Area))*
  as.numeric(as.character(item233.windows$Quantity))

# remove zeros (don't make sense)
item233.windows1 <- item233.windows[which(item233.windows$Window_Area > 0),]
item233.windows2 <- summarise(group_by(item233.windows1, CK_Cadmus_ID, BuildingTypeXX)
                              ,Window_Area = sum(Window_Area))

item233.floor <- summarise(group_by(envelope.dat3, CK_Cadmus_ID, BuildingTypeXX)
                          ,FloorArea   = sum(Floor.Area,na.rm = T))

item233.dat <- left_join(item233.windows2, item233.floor, by = c("CK_Cadmus_ID", "BuildingTypeXX"))

#Remove any items where walla area and window are are not greater than zero.

item233.dat1 <- item233.dat[which(item233.dat$FloorArea > 0 & 
                                    item233.dat$Window_Area > 0),]


#calculate the window to wall ratio
item233.dat1$WindowToFloorArea <- item233.dat1$Window_Area / item233.dat1$FloorArea

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

#############################################################################################
#Item 231: Table 23
#############################################################################################
##########################################
# Clean up window data
##########################################
windows231.dat <- windows.dat[which(colnames(windows.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Area","Quantity",
                                                               "Frame./.Body.Type",
                                                               "Glazing.Type"))]
windows231.dat2  <- left_join(rbsa.dat, windows231.dat, by = "CK_Cadmus_ID")
length(unique(windows231.dat2$CK_Cadmus_ID)) 
#Subset to MF
windows231.dat3 <- windows.dat2[grep("Multifamily", windows231.dat2$BuildingType),]

windows231.dat3$FrameType <- windows231.dat3$`Frame./.Body.Type`
windows231.dat3$FrameType[grep("Wood|Vinyl|Fiberglass", windows231.dat3$FrameType)] <- "Wood/Vinyl/Fiberglass"
windows231.dat3$FrameType[grep("Metal|Aluminum", windows231.dat3$FrameType)] <- "Metal"
windows231.dat3$FrameType[grep("N/A", windows231.dat3$FrameType)] <- "Unknown"
windows231.dat3$FrameType[is.na(windows231.dat3$FrameType)] <- "Unknown"

windows231.dat3$Glazing <- trimws(windows231.dat3$Glazing.Type)
windows231.dat3$Glazing[grep("Single", windows231.dat3$Glazing)] <- "Single Glazed"
windows231.dat3$Glazing[grep("Double", windows231.dat3$Glazing)] <- "Double Glazed"
windows231.dat3$Glazing[grep("Triple", windows231.dat3$Glazing)] <- "Triple Glazed"
windows231.dat3$Glazing[which(!(windows231.dat3$Glazing %in% c("Single Glazed", "Double Glazed", "Triple Glazed")))] <- "Unknown"

windows231.dat3$FramingCategory <- paste(windows231.dat3$FrameType,windows231.dat3$Glazing)
if 
##########################################
# Calculate average window size per building
##########################################
# make numeric
windows231.dat3$Window_Area <- as.numeric(as.character(windows231.dat3$Area))*
  as.numeric(as.character(windows231.dat3$Quantity))
# remove zeros (don't make sense)
windows231.dat4 <- windows231.dat3[which(windows231.dat3$Window_Area > 0 
                                         & !is.na(windows231.dat3$HomeYearBuilt_MF) 
                                         & !grepl("Unknown",windows231.dat3$FramingCategory)),]


windows231.AreaByFrameVintage <- summarise(group_by(windows231.dat4, HomeYearBuilt_MF,FramingCategory)
                                      ,WindowAreaFrameType = sum(Window_Area))

windows231.TotalWindowAreaByVintage <- summarise(group_by(windows231.dat4, HomeYearBuilt_MF)
                                        ,TotalWindowArea = sum(Window_Area)
                                        ,SampleSize = length(unique(CK_Cadmus_ID)))

windows231.AreaByFrame <- summarise(group_by(windows231.dat4,FramingCategory)
                                    ,HomeYearBuilt_MF = "All Vintages"
                                    ,WindowAreaFrameType = sum(Window_Area))

windows231.TotalWindowArea <- summarise(windows231.dat4
                                        ,HomeYearBuilt_MF = "All Vintages"
                                        ,TotalWindowArea = sum(Window_Area)
                                        ,SampleSize = length(unique(CK_Cadmus_ID)))

windows231.FrameLevelArea <- rbind.data.frame(windows231.AreaByFrameVintage,
                                              windows231.AreaByFrame,
                                              stringsAsFactors = F)
windows231.TotalWindowArea2 <- rbind.data.frame(windows231.TotalWindowAreaByVintage,
                                               windows231.TotalWindowArea,
                                              stringsAsFactors = F)

windows231.combined <- left_join(windows231.FrameLevelArea,
                                       windows231.TotalWindowArea2,
                                       by = c("HomeYearBuilt_MF"))

windows231.combined$Percent <- windows231.combined$WindowAreaFrameType/windows231.combined$TotalWindowArea
windows231.combined$SE      <- sqrt(windows231.combined$Percent * (1 - windows231.combined$Percent) / windows231.combined$SampleSize)

detach(package:reshape2)
library(data.table)

windows231.table <- dcast(setDT(windows231.combined)
                      , formula = HomeYearBuilt_MF + SampleSize ~ FramingCategory
                      , value.var = c("Percent", "SE"))

windows231.final <- data.frame( "Vintage" = windows231.table$HomeYearBuilt_MF
                            ,"Metal Double" = windows231.table$`Percent_Metal Double Glazed`
                            ,"Metal Double SE" = windows231.table$`SE_Metal Double Glazed`
                            ,"Metal Single" = windows231.table$`Percent_Metal Single Glazed`
                            ,"Metal Single SE" = windows231.table$`SE_Metal Single Glazed`
                            ,"Wood/Vinyl/Fiberglass Double" = windows231.table$`Percent_Wood/Vinyl/Fiberglass Double Glazed`
                            ,"Wood/Vinyl/Fiberglass Double SE" = windows231.table$`SE_Wood/Vinyl/Fiberglass Double Glazed`
                            ,"Wood/Vinyl/Fiberglass Single" = windows231.table$`Percent_Wood/Vinyl/Fiberglass Single Glazed`
                            ,"Wood/Vinyl/Fiberglass Single SE" = windows231.table$`SE_Wood/Vinyl/Fiberglass Single Glazed`
                            ,"Wood/Vinyl/Fiberglass Triple" = windows231.table$`Percent_Wood/Vinyl/Fiberglass Triple Glazed`
                            ,"Wood/Vinyl/Fiberglass Triple SE" = windows231.table$`SE_Wood/Vinyl/Fiberglass Triple Glazed`
                            ,"SampleSize" = windows231.table$SampleSize)
