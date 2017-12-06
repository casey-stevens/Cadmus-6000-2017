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
rbsa.dat.MF <- rbsa.dat[grep("Multifamily", rbsa.dat$BuildingType),]

# #Read in data for analysis
# # Windows
# windows.doors.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, windows.export))
# windows.doors.dat$CK_Cadmus_ID <- trimws(toupper(windows.doors.dat$CK_Cadmus_ID))
# windows.dat <- windows.doors.dat[which(windows.doors.dat$Type == "Window"),]
# #select columns for windows
# windows.dat1 <- windows.dat[which(colnames(windows.dat) %in% c("CK_Cadmus_ID"
#                                                                ,"CK_SiteID"
#                                                                ,"Type"
#                                                                ,"Quantity"
#                                                                ,"Frame./.Body.Type"
#                                                                ,"Glazing.Type"
#                                                                ,"Area"))]
# windows.dat2  <- left_join(rbsa.dat, windows.dat1, by = c("CK_Building_ID" = "CK_SiteID"))
# length(unique(windows.dat2$CK_Cadmus_ID)) 
# #Subset to MF
# windows.dat.MF <- windows.dat2[grep("Multifamily", windows.dat2$BuildingType),]


######################################################################################################################
# Note: Windows data for MF moved to the envelope export

#read in Envelope data for MF table
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))
envelope.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"CK_SiteID"
                                                                  ,"Wall.Area"
                                                                  ,"Floor.Area"
                                                                  ,"Category"
                                                                  ,"Area"
                                                                  ,"Fenestration"))]
envelope.dat1$Window.Count <- as.numeric(as.character(gsub("N = ", "", envelope.dat1$Fenestration)))

envelope.dat2 <- left_join(rbsa.dat, envelope.dat1)
length(unique(envelope.dat2$CK_Cadmus_ID)) 
envelope.dat.MF <- envelope.dat2[grep("Multifamily", envelope.dat2$BuildingType),]






#############################################################################################
#Item 231: Table 23
#############################################################################################
##########################################
# Clean up window data
##########################################
item231.dat <- envelope.dat.MF

#clean up frame/body type
unique(item231.dat$`Frame./.Body.Type`)
item231.dat$Frame.Type <- trimws(item231.dat$`Frame./.Body.Type`)
item231.dat$Frame.Type[grep("Wood|Vinyl|Fiberglass|wood|vinyl|fiberglass|tile|Garage", item231.dat$Frame.Type)] <- "Wood/Vinyl/Fiberglass/Tile"
item231.dat$Frame.Type[grep("Metal|Aluminum|metal|aluminum", item231.dat$Frame.Type)] <- "Metal"
item231.dat$Frame.Type[grep("N/A", item231.dat$Frame.Type)] <- "Unknown"
item231.dat$Frame.Type[which(is.na(item231.dat$Frame.Type))] <- "Unknown"
unique(item231.dat$Frame.Type)

item231.dat1 <- item231.dat[which(item231.dat$Frame.Type != "Unknown"),]
unique(item231.dat1$Frame.Type)

#clean up glazing types
item231.dat1$Glazing <- trimws(item231.dat1$Glazing.Type)
item231.dat1$Glazing[grep("Single", item231.dat1$Glazing)] <- "Single"
item231.dat1$Glazing[grep("Double", item231.dat1$Glazing)] <- "Double"
item231.dat1$Glazing[which(!(item231.dat1$Glazing %in% c("Single", "Double")))] <- "Unknown"
unique(item231.dat$Glazing)

item231.dat2 <- item231.dat1[which(item231.dat1$Glazing != "Unknown"),]
unique(item231.dat2$Glazing)

item231.dat2$Framing.Categories <- paste(item231.dat2$Frame.Type, item231.dat2$Glazing, sep = " ")

item231.dat2$count <- 1
item231.dat3 <- item231.dat2[which(!is.na(as.numeric(as.character(item231.dat2$Quantity)))),]

window.area.test <- rep(item231.dat3$Area, item231.dat3$Quantity)
window.type.test <- rep(item231.dat3$Framing.Categories, item231.dat3$Quantity)
cadmus.id.test   <- rep(item231.dat3$CK_Cadmus_ID, item231.dat3$Quantity)
item231.windows  <- cbind.data.frame(cadmus.id.test, window.type.test, window.area.test)
names(item231.windows) <- c("CK_Cadmus_ID", "Window_Type", "Window_Area")

item231.merge <- left_join(rbsa.dat, item231.windows)
item231.merge <- item231.merge[which(item231.merge$Window_Area %notin% c("Unknown", NA)),]
item231.merge <- item231.merge[which(!is.na(item231.merge$HomeYearBuilt)),]




################################################
# Adding pop and sample sizes for weights
################################################
item231.data <- weightedData(item231.merge[-which(colnames(item231.merge) %in% c("Window_Type"
                                                                                 ,"Window_Area"))])
item231.data <- left_join(item231.data, item231.merge[which(colnames(item231.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Window_Type"
                                                                                           ,"Window_Area"))])

item231.data$count <- 1
item231.data$Window_Area <- as.numeric(as.character(item231.data$Window_Area))
item231.data$Window_Type <- as.character(item231.data$Window_Type)

#######################
# Weighted Analysis
#######################
item231.final <- proportionRowsAndColumns1(CustomerLevelData = item231.data
                                           ,valueVariable    = 'Window_Area'
                                           ,columnVariable   = 'HomeYearBuilt_bins_MF'
                                           ,rowVariable      = 'Window_Type'
                                           ,aggregateColumnName = "Remove")
item231.final <- item231.final[which(item231.final$HomeYearBuilt_bins_MF != "Remove"),]
item231.final <- item231.final[which(item231.final$Window_Type != "Total"),]


item231.all.vintages <- proportions_one_group_MF(CustomerLevelData = item231.data
                                              ,valueVariable = 'Window_Area'
                                              ,groupingVariable = 'Window_Type'
                                              ,total.name = "All Vintages"
                                              ,columnName = "HomeYearBuilt_bins_MF"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
# item231.all.vintages <- item231.all.vintages[which(item231.all.vintages$Window_Type != "Total"),]

item231.final <- rbind.data.frame(item231.final, item231.all.vintages, stringsAsFactors = F)

item231.cast <- dcast(setDT(item231.final)
                      ,formula = HomeYearBuilt_bins_MF ~ Window_Type
                      ,value.var = c("w.percent","w.SE", "count","n","N"))

item231.final <- data.frame( "Vintage"                          = item231.cast$HomeYearBuilt_bins_MF
                             ,"Metal Double"                    = item231.cast$`w.percent_Metal Double`
                             ,"Metal Double SE"                 = item231.cast$`w.SE_Metal Double`
                             ,"Metal Single"                    = item231.cast$`w.percent_Metal Single`
                             ,"Metal Single SE"                 = item231.cast$`w.SE_Metal Single`
                             ,"Wood/Vinyl/Fiberglass Double"    = item231.cast$`w.percent_Wood/Vinyl/Fiberglass/Tile Double`
                             ,"Wood/Vinyl/Fiberglass Double SE" = item231.cast$`w.SE_Wood/Vinyl/Fiberglass/Tile Double`
                             ,"Wood/Vinyl/Fiberglass Single"    = item231.cast$`w.percent_Wood/Vinyl/Fiberglass/Tile Single`
                             ,"Wood/Vinyl/Fiberglass Single SE" = item231.cast$`w.SE_Wood/Vinyl/Fiberglass/Tile Single`
                             ,"SampleSize"                      = item231.cast$`n_Wood/Vinyl/Fiberglass/Tile Double`)

exportTable(item231.final, "MF", "Table 23", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item231.final <- proportions_two_groups_unweighted(CustomerLevelData = item231.data
                                           ,valueVariable    = 'Window_Area'
                                           ,columnVariable   = 'HomeYearBuilt_bins_MF'
                                           ,rowVariable      = 'Window_Type'
                                           ,aggregateColumnName = "All Vintages")
item231.final <- item231.final[which(item231.final$Window_Type != "Total"),]

item231.cast <- dcast(setDT(item231.final)
                      ,formula = HomeYearBuilt_bins_MF ~ Window_Type
                      ,value.var = c("Percent","SE", "Count","SampleSize"))

item231.final <- data.frame( "Vintage"                          = item231.cast$HomeYearBuilt_bins_MF
                             ,"Metal Double"                    = item231.cast$`Percent_Metal Double`
                             ,"Metal Double SE"                 = item231.cast$`SE_Metal Double`
                             ,"Metal Double n"                  = item231.cast$`SampleSize_Metal Double`
                             ,"Metal Single"                    = item231.cast$`Percent_Metal Single`
                             ,"Metal Single SE"                 = item231.cast$`SE_Metal Single`
                             ,"Metal Single n"                  = item231.cast$`SampleSize_Metal Single`
                             ,"Wood/Vinyl/Fiberglass Double"    = item231.cast$`Percent_Wood/Vinyl/Fiberglass/Tile Double`
                             ,"Wood/Vinyl/Fiberglass Double SE" = item231.cast$`SE_Wood/Vinyl/Fiberglass/Tile Double`
                             ,"Wood/Vinyl/Fiberglass Double n"  = item231.cast$`SampleSize_Wood/Vinyl/Fiberglass/Tile Double`
                             ,"Wood/Vinyl/Fiberglass Single"    = item231.cast$`Percent_Wood/Vinyl/Fiberglass/Tile Single`
                             ,"Wood/Vinyl/Fiberglass Single SE" = item231.cast$`SE_Wood/Vinyl/Fiberglass/Tile Single`
                             ,"Wood/Vinyl/Fiberglass Single n"  = item231.cast$`SampleSize_Wood/Vinyl/Fiberglass/Tile Single`)

exportTable(item231.final, "MF", "Table 23", weighted = FALSE)





#############################################################################################
#Item 232: Table 24
#############################################################################################
# make numeric
item232.windows1 <- item231.merge
item232.windows1$Window_Area <- as.numeric(as.character(item232.windows1$Window_Area))

item232.envelope <- envelope.dat.MF[which(envelope.dat.MF$Category == "Wall"),]
item232.envelope$Wall.Area <- as.numeric(as.character(item232.envelope$Wall.Area))
##########################################
# Calculate average window size within unit, weighted average by number of windows and area
##########################################
item232.windows <- summarise(group_by(item232.windows1, CK_Cadmus_ID, BuildingTypeXX)
                                 , WindowArea = sum(Window_Area, na.rm = T))

##########################################
# Calculate total number of windows and wall area per building
##########################################
item232.wall <- summarise(group_by(item232.envelope, CK_Cadmus_ID.x, BuildingTypeXX)
                          ,WallArea   = mean(Wall.Area,  na.rm = T))
colnames(item232.wall) <- c("CK_Cadmus_ID", "BuildingTypeXX", "WallArea")


##########################################
# Merge Window and Wall Area
##########################################
item232.dat <- left_join(item232.windows, item232.wall)
#Remove any items where walla area and window are are not greater than zero.

item232.dat1 <- item232.dat[which(item232.dat$WallArea > 0 & 
                                    item232.dat$WindowArea > 0),]

#calculate the window to wall ratio
item232.dat1$WindowToWallArea <- item232.dat1$WindowArea / item232.dat1$WallArea


item232.merge <- left_join(rbsa.dat, item232.dat1)
item232.merge <- item232.merge[which(!is.na(item232.merge$WindowToWallArea)),]

################################################
# Adding pop and sample sizes for weights
################################################
item232.data <- weightedData(item232.merge[-which(colnames(item232.merge) %in% c("WindowArea"
                                                                                 ,"WallArea"
                                                                                 ,"NumWindows"
                                                                                 ,"WindowToWallArea"))])
item232.data <- left_join(item232.data, item232.merge[which(colnames(item232.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"WindowArea"
                                                                                           ,"WallArea"
                                                                                           ,"NumWindows"
                                                                                           ,"WindowToWallArea"))])

item232.data$WindowToWallArea <- as.numeric(as.character(item232.data$WindowToWallArea))
item232.data$count <- 1
#######################
# Weighted Analysis
#######################
item232.final <- mean_one_group(CustomerLevelData = item232.data
                                ,valueVariable = 'WindowToWallArea'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")
exportTable(item232.final[which(colnames(item232.final) %notin% c("BulidingType","n_h"))], "MF", "Table 20", weighted = TRUE)


#######################
# unweighted Analysis
#######################
item232.final <- mean_one_group_unweighted(CustomerLevelData = item232.data
                                ,valueVariable = 'WindowToWallArea'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = "All Sizes")
exportTable(item232.final[which(colnames(item232.final) %notin% c("BulidingType","n_h"))], "MF", "Table 20", weighted = FALSE)






#############################################################################################
#Item 233: Table 25
#############################################################################################
##########################################
# Merge Window and Floor Area
##########################################
# make numeric
item233.windows1 <- item231.merge
item233.windows1$Window_Area <- as.numeric(as.character(item233.windows1$Window_Area))

item233.envelope <- envelope.dat.MF[which(envelope.dat.MF$)]
item233.envelope$Wall.Area <- as.numeric(as.character(item233.envelope$Wall.Area))
item233.envelope$NumWindows <- as.numeric(as.character(item233.envelope$NumWindows))

# item233.floor00 <- item233.floor0[which(!(is.na(item233.floor0$Floor.Area))),]

item233.floor <- summarise(group_by(item233.floor0, CK_Cadmus_ID, BuildingTypeXX)
                          ,FloorArea  = sum(Floor.Area, na.rm = T)
                          ,NumWindows = sum(NumWindows, na.rm = T))

item233.dat <- left_join(item233.windows, item233.floor, by = c("CK_Cadmus_ID", "BuildingTypeXX"))

#Remove any items where walla area and window are are not greater than zero.

item233.dat1 <- item233.dat[which(item233.dat$FloorArea > 0 & 
                                    item233.dat$Avg_Window_Area > 0),]

item233.dat1$Window_Area <- item233.dat1$Avg_Window_Area * item233.dat1$NumWindows

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
