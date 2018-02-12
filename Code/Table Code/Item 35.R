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
download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Envelope.xlsx', envelope.export, mode = 'wb')
envelope.dat <- read.xlsx(envelope.export)
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

# add indicator of whether a site has a basement or not
envelope.dat$BasementInd <- 0
basement.tmp <- envelope.dat$CK_Cadmus_ID[which(envelope.dat$Floor.Type == "Basement")]
envelope.dat$BasementInd[which(envelope.dat$CK_Cadmus_ID %in% basement.tmp)] <- 1
envelope.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"CK_SiteID"
                                                                  ,"BasementInd"))]
colnames(envelope.dat1) <- c("CK_Cadmus_ID"
                             ,"CK_Building_ID"
                             ,"BasementInd")
# envelope.dat1 <- unique(envelope.dat1[-which(colnames(envelope.dat1) == "CK_Cadmus_ID")])

#Rooms for MH and MF
rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
rooms.dat$CK_Cadmus_ID <- trimws(toupper(rooms.dat$CK_Cadmus_ID))

rooms.dat1 <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID","CK_SiteID","Clean.Type","Area"))]
rooms.dat2 <- rooms.dat1[grep("SITE",rooms.dat1$CK_SiteID),]
colnames(rooms.dat2) <- c("CK_Cadmus_ID", "CK_Building_ID", "Room_Type", "Room_Area")



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


##########################################
# Merge Window and Floor Area
##########################################
item35.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Floor.Type"
                                                               ,"Floor.Sub-Type"))]

item35.dat1 <- left_join(rbsa.dat, item35.dat, by = "CK_Cadmus_ID")

#subset to only single family homes
item35.dat2 <- item35.dat1[which(item35.dat1$BuildingType == "Single Family"),]
item35.dat2$count <- 1

item35.dat2$Ind <- 0
item35.dat2$Ind[which(item35.dat2$Floor.Type == "Basement")] <- 1

item35.dat3 <- unique(item35.dat2[which(item35.dat2$Ind == 1),])
item35.dat4 <- left_join(rbsa.dat, item35.dat3)
item35.dat4$Ind[which(is.na(item35.dat4$Ind))] <- 0


# merge window and room averages
item35.dat <- left_join(item35.dat4, item35.windows2)
item35.dat <- item35.dat[grep("site", item35.dat$CK_Building_ID, ignore.case = T),]
item35.dat1 <- item35.dat[-which(duplicated(item35.dat$CK_Cadmus_ID)),]
item35.dat1 <- item35.dat1[which(item35.dat1$Conditioned.Area > 0),]

#calculate the window to floor ratio
item35.dat1$WindowToFloorArea <- item35.dat1$WindowArea / item35.dat1$Conditioned.Area
item35.dat1 <- item35.dat1[which(item35.dat1$WindowToFloorArea < 1),]

#remove missing ratio information (missing floor area info)
item35.dat2 <- item35.dat1[which(!is.na(item35.dat1$WindowToFloorArea)),]

item35.dat2$Basement <- item35.dat2$Ind
item35.dat2$Basement[which(item35.dat2$Ind == 0)] <- "Home without Basements"
item35.dat2$Basement[which(item35.dat2$Ind == 1)] <- "Home with Basements"

item35.merge <- left_join(rbsa.dat, item35.dat2)
item35.merge <- item35.merge[which(!is.na(item35.merge$Basement)),]


################################################
# Adding pop and sample sizes for weights
################################################
item35.data <- weightedData(item35.merge[-which(colnames(item35.merge) %in% c("Floor.Type"
                                                                              ,"Floor.Sub-Type"
                                                                              ,"count"
                                                                              ,"WindowArea"
                                                                              ,"Ind"
                                                                              ,"WindowToFloorArea"
                                                                              ,"Basement"))])
item35.data <- left_join(item35.data, item35.merge[which(colnames(item35.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Floor.Type"
                                                                                       ,"Floor.Sub-Type"
                                                                                       ,"count"
                                                                                       ,"WindowArea"
                                                                                       ,"Ind"
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





















############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
scl.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.scl.data", rundate, ".xlsx", sep = "")))
length(unique(scl.dat$CK_Cadmus_ID))
scl.dat$CK_Building_ID <- scl.dat$Category
scl.dat <- scl.dat[which(names(scl.dat) != "Category")]

#############################################################################################
#Item 35: 
#############################################################################################
# Average window to floor area ratio
# Fist average within homes, then average across homes within building type and floor area? I think so...

#windows
item35.os.windows  <- left_join(scl.dat, windows.dat2, by = "CK_Cadmus_ID")
length(unique(item35.os.windows$CK_Cadmus_ID))

#floor area (SF ONLY)
item35.os.ENV <- left_join(scl.dat, envelope.dat1, by = "CK_Cadmus_ID")
length(unique(item35.os.ENV$CK_Cadmus_ID))

#room area (MH only)
item35.os.rooms    <- left_join(scl.dat, rooms.dat2, by = "CK_Cadmus_ID")
length(unique(item35.os.rooms$CK_Cadmus_ID))


##########################################
# For Window Area Average
##########################################
# make numeric
item35.os.windows$Window_Area <- as.numeric(as.character(item35.os.windows$Window_Area)) * as.numeric(as.character(item35.os.windows$Window_QTY))

# remove zeros (don't make sense)
item35.os.windows0 <- item35.os.windows[which(!(is.na(item35.os.windows$Window_Area))),]
item35.os.windows1 <- item35.os.windows0[which(item35.os.windows0$Window_Area > 0),]


item35.os.windows2 <- summarise(group_by(item35.os.windows1, CK_Cadmus_ID)
                             ,WindowArea = sum(Window_Area))


##########################################
# Merge Window and Floor Area
##########################################
item35.os.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Floor.Type"
                                                               ,"Floor.Sub-Type"))]

item35.os.dat1 <- left_join(scl.dat, item35.os.dat, by = "CK_Cadmus_ID")

#subset to only single family homes
item35.os.dat2 <- item35.os.dat1[which(item35.os.dat1$BuildingType == "Single Family"),]
item35.os.dat2$count <- 1

item35.os.dat2$Ind <- 0
item35.os.dat2$Ind[which(item35.os.dat2$Floor.Type == "Basement")] <- 1

item35.os.dat3 <- unique(item35.os.dat2[which(item35.os.dat2$Ind == 1),])
item35.os.dat4 <- left_join(scl.dat, item35.os.dat3)
item35.os.dat4$Ind[which(is.na(item35.os.dat4$Ind))] <- 0


# merge window and room averages
item35.os.dat <- left_join(item35.os.dat4, item35.os.windows2)
item35.os.dat1 <- item35.os.dat[which(item35.os.dat$Conditioned.Area > 0),]

#calculate the window to floor ratio
item35.os.dat1$WindowToFloorArea <- item35.os.dat1$WindowArea / item35.os.dat1$Conditioned.Area
item35.os.dat1 <- item35.os.dat1[which(item35.os.dat1$WindowToFloorArea < 1),]

#remove missing ratio information (missing floor area info)
item35.os.dat2 <- item35.os.dat1[which(!is.na(item35.os.dat1$WindowToFloorArea)),]

item35.os.dat2$Basement <- item35.os.dat2$Ind
item35.os.dat2$Basement[which(item35.os.dat2$Ind == 0)] <- "Home without Basements"
item35.os.dat2$Basement[which(item35.os.dat2$Ind == 1)] <- "Home with Basements"

item35.os.merge <- left_join(scl.dat, item35.os.dat2)
item35.os.merge <- item35.os.merge[which(!is.na(item35.os.merge$Basement)),]


################################################
# Adding pop and sample sizes for weights
################################################
item35.os.data <- weightedData(item35.os.merge[-which(colnames(item35.os.merge) %in% c("Floor.Type"
                                                                              ,"Floor.Sub-Type"
                                                                              ,"count"
                                                                              ,"WindowArea"
                                                                              ,"Ind"
                                                                              ,"WindowToFloorArea"
                                                                              ,"Basement"))])
item35.os.data <- left_join(item35.os.data, unique(item35.os.merge[which(colnames(item35.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Floor.Type"
                                                                                       ,"Floor.Sub-Type"
                                                                                       ,"count"
                                                                                       ,"WindowArea"
                                                                                       ,"Ind"
                                                                                       ,"WindowToFloorArea"
                                                                                       ,"Basement"))]))
item35.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item35.os.final <- mean_two_groups(CustomerLevelData = item35.os.data
                                   ,valueVariable = 'WindowToFloorArea'
                                   ,byVariableRow = "Basement"
                                   ,byVariableColumn = "CK_Building_ID"
                                   ,columnAggregate = "Remove"
                                   ,rowAggregate = "Total")
remove.ind <- names(item35.os.final)[grep("Remove",names(item35.os.final))]
item35.os.final <- data.frame(item35.os.final)
item35.os.cast <- item35.os.final[which(names(item35.os.final) %notin% remove.ind)]
names(item35.os.cast)

item35.os.table <- data.frame("Basement.Type"        = item35.os.cast$Basement
                              ,"Mean_SCL.GenPop"      = item35.os.cast$Mean_SCL.GenPop
                              ,"SE_SCL.GenPop"        = item35.os.cast$SE_SCL.GenPop
                              ,"n_SCL.GenPop"         = item35.os.cast$n_SCL.GenPop
                              ,"Mean_SCL.LI"          = item35.os.cast$Mean_SCL.LI
                              ,"SE_SCL.LI"            = item35.os.cast$SE_SCL.LI
                              ,"n_SCL.LI"             = item35.os.cast$n_SCL.LI
                              ,"Mean_SCL.EH"          = item35.os.cast$Mean_SCL.EH
                              ,"SE_SCL.EH"            = item35.os.cast$SE_SCL.EH
                              ,"n_SCL.EH"             = item35.os.cast$n_SCL.EH
                              ,"Mean_2017.RBSA.PS"    = item35.os.cast$Mean_2017.RBSA.PS
                              ,"SE_2017.RBSA.PS"      = item35.os.cast$SE_2017.RBSA.PS
                              ,"n_2017.RBSA.PS"       = item35.os.cast$n_2017.RBSA.PS
                              ,"EB_SCL.GenPop"        = item35.os.cast$EB_SCL.GenPop
                              ,"EB_SCL.LI"            = item35.os.cast$EB_SCL.LI
                              ,"EB_SCL.EH"            = item35.os.cast$EB_SCL.EH
                              ,"EB_2017.RBSA.PS"      = item35.os.cast$EB_2017.RBSA.PS)


exportTable(item35.os.table, "SF", "Table 42", weighted = TRUE, osIndicator = "SCL",OS = T)



#######################
# Unweighted Analysis
#######################
item35.os.final <- mean_two_groups_unweighted(CustomerLevelData = item35.os.data
                                   ,valueVariable = 'WindowToFloorArea'
                                   ,byVariableRow = "Basement"
                                   ,byVariableColumn = "CK_Building_ID"
                                   ,columnAggregate = "Remove"
                                   ,rowAggregate = "Total")
remove.ind <- names(item35.os.final)[grep("Remove",names(item35.os.final))]
item35.os.final <- data.frame(item35.os.final)
item35.os.cast <- item35.os.final[which(names(item35.os.final) %notin% remove.ind)]
names(item35.os.cast)

item35.os.table <- data.frame("Basement.Type"        = item35.os.cast$Basement
                              ,"Mean_SCL.GenPop"      = item35.os.cast$Mean_SCL.GenPop
                              ,"SE_SCL.GenPop"        = item35.os.cast$SE_SCL.GenPop
                              ,"n_SCL.GenPop"         = item35.os.cast$n_SCL.GenPop
                              ,"Mean_SCL.LI"          = item35.os.cast$Mean_SCL.LI
                              ,"SE_SCL.LI"            = item35.os.cast$SE_SCL.LI
                              ,"n_SCL.LI"             = item35.os.cast$n_SCL.LI
                              ,"Mean_SCL.EH"          = item35.os.cast$Mean_SCL.EH
                              ,"SE_SCL.EH"            = item35.os.cast$SE_SCL.EH
                              ,"n_SCL.EH"             = item35.os.cast$n_SCL.EH
                              ,"Mean_2017.RBSA.PS"    = item35.os.cast$Mean_2017.RBSA.PS
                              ,"SE_2017.RBSA.PS"      = item35.os.cast$SE_2017.RBSA.PS
                              ,"n_2017.RBSA.PS"       = item35.os.cast$n_2017.RBSA.PS)

exportTable(item35.os.table, "SF", "Table 42", weighted = FALSE, osIndicator = "SCL",OS = T)


