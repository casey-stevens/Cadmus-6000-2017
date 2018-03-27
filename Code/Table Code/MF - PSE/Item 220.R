#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

##  Create "Not In" operator
"%notin%" <- Negate("%in%")

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation - MF-BLDG.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) 
rbsa.dat.site <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat[grep("bldg",rbsa.dat$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
# buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))
length(unique(buildings.dat$CK_Building_ID))
buildings.dat.clean <- buildings.dat[which(!duplicated(buildings.dat$CK_Building_ID)),]


#Read in data for analysis
# rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
#clean cadmus IDs
rooms.dat$CK_Cadmus_ID <- trimws(toupper(rooms.dat$CK_Cadmus_ID))




#############################################################################################
#For Rooms
#############################################################################################

#subset to columns needed for analysis
rooms.dat1 <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID"
                                                         ,"CK_SiteID"
                                                         ,"Clean.Type"
                                                         ,"Area"))]
colnames(rooms.dat1) <- c("CK_Cadmus_ID","CK_SiteID","Clean.Room","Area")

rooms.dat1 <- rooms.dat1[which(rooms.dat1$Area %notin% c("Unknown","N/A")),]
rooms.dat1 <- rooms.dat1[which(rooms.dat1$Clean.Room %notin% c("Parking")),]

rooms.dat1$Area <- as.numeric(as.character(rooms.dat1$Area))
# rooms.dat1$Area[which(rooms.dat1$Area == "N/A")] <- 0
rooms.dat2 <- rooms.dat1#[which(rooms.dat1$Area > 0),]
rooms.dat2 <- rooms.dat2[which(!is.na(rooms.dat2$Area)),]

rooms.dat3 <- rooms.dat2[grep("BLDG", rooms.dat2$CK_SiteID),]
rooms.dat4 <- summarise(group_by(rooms.dat3, CK_SiteID, Clean.Room)
                        ,SiteArea = mean(Area, na.rm = T))

# rooms.cast <- dcast(setDT(rooms.dat4)
#                     ,formula = CK_SiteID ~ Clean.Room
#                     ,value.var = c("SiteArea"))
# rooms.cast[is.na(rooms.cast),] <- 0
# 
# rooms.melt <- melt(rooms.cast, id.vars = c("CK_SiteID"))
# names(rooms.melt) <- c("CK_SiteID", "Clean.Room", "SiteArea") 

#merge together analysis data with cleaned RBSA data
rooms.final <- left_join(rbsa.dat.bldg, rooms.dat4, by = c("CK_Building_ID"="CK_SiteID"))
rooms.final <- rooms.final[which(rooms.final$BuildingType == "Multifamily"),]
rooms.final <- rooms.final[which(rooms.final$SiteArea %notin% c("N/A",NA)),]
names(rooms.final)[which(names(rooms.final) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID" 

#############################################################################################
# Item 220B: AVERAGE COMMON AREA ROOM TYPE FLOOR AREA (SQ.FT.) BY BUILDING SIZE (MF table 12)
#############################################################################################
item220B.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"
                                                                  ,"CK_Building_ID"))]
colnames(item220B.dat) <- c( "CommonFloorArea","CK_Building_ID")
item220B.dat$CK_Building_ID <- as.character(item220B.dat$CK_Building_ID)

# join on rooms and rbsa cleaned data:
item220B.dat1 <- left_join(rooms.final,item220B.dat)

#make numeric
item220B.dat1$CommonFloorArea <- as.numeric(as.character(item220B.dat1$CommonFloorArea))
item220B.dat1$CommonFloorArea[which(is.na(item220B.dat1$CommonFloorArea))] <- 0

item220B.dat1$SiteArea <- as.numeric(as.character(item220B.dat1$SiteArea))

#remove any NAs
item220B.dat2 <- item220B.dat1[which(!is.na(item220B.dat1$SiteArea)),]

unique(item220B.dat2$Clean.Room)

#subset to only relevant columns
item220B.merge <- left_join(rbsa.dat, item220B.dat2)
item220B.merge <- item220B.merge[which(!is.na(item220B.merge$Clean.Room)),]
item220B.merge <- item220B.merge[which(item220B.merge$Clean.Room != "Parking"),]
item220B.merge <- item220B.merge[which(item220B.merge$BuildingType == "Multifamily"),]
# item220B.merge$Total.Area[which(is.na(item220B.merge$Total.Area))] <- 0


item220B.data <- weightedData(item220B.merge[which(colnames(item220B.merge) %notin% c("CK_Cadmus_ID.y"
                                                                                   ,"Clean.Room"
                                                                                   ,"SiteArea"
                                                                                   # ,"Total.Area"
                                                                                   ,"CommonFloorArea"
                                                                                   ,"Category"))])
item220B.data <- left_join(item220B.data, item220B.merge[which(colnames(item220B.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Clean.Room"
                                                                                           ,"SiteArea"
                                                                                           # ,"Total.Area"
                                                                                           ,"CommonFloorArea"
                                                                                           ,"Category"))])
stopifnot(nrow(item220B.merge) == nrow(item220B.data))
item220B.data$count <- 1
##################################
# weighted analysis
##################################
item220B.cast <- mean_two_groups(CustomerLevelData = item220B.data
                                ,valueVariable = 'SiteArea'
                                ,byVariableRow = 'Clean.Room'
                                ,byVariableColumn = 'Category'
                                ,rowAggregate = "All Rooms"
                                ,columnAggregate = "Remove")
names(item220B.cast)
item220B.table <- data.frame("Room_Type"                 = item220B.cast$Clean.Room
                            ,"PSE.Mean"                 = item220B.cast$Mean_PSE
                            ,"PSE.SE"                   = item220B.cast$SE_PSE
                            ,"PSE.n"                    = item220B.cast$n_PSE
                            ,"PSE.King.County_Mean"     = item220B.cast$`Mean_PSE KING COUNTY`
                            ,"PSE.King.County_SE"       = item220B.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"        = item220B.cast$n_PSE
                            ,"PSE.Non.King.County_Mean" = item220B.cast$`Mean_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County_SE"   = item220B.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"    = item220B.cast$n_PSE
                            ,"2017.RBSA.PS_Mean"        = item220B.cast$`Mean_2017 RBSA PS`
                            ,"2017.RBSA.PS_SE"          = item220B.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS_n"           = item220B.cast$`n_2017 RBSA PS`
                            ,"PSE.EB"                   = item220B.cast$EB_PSE
                            ,"PSE.King.County_EB"       = item220B.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"   = item220B.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"          = item220B.cast$`EB_2017 RBSA PS`
)

levels(item220B.table$Room_Type)
rowOrder <- c("Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Lobby"
              ,"Mechanical"
              ,"Office"
              ,"Other"
              ,"Outside"
              ,"Recreation"
              ,"Store"
              ,"All Rooms")
item220B.table <- item220B.table %>% mutate(Room_Type = factor(Room_Type, levels = rowOrder)) %>% arrange(Room_Type)  
item220B.table <- data.frame(item220B.table)

exportTable(item220B.table, "MF", "Table 12", weighted = TRUE, OS = T, osIndicator = "PSE")

##################################
# unweighted analysis
##################################
item220B.cast <- mean_two_groups_unweighted(CustomerLevelData = item220B.data
                                            ,valueVariable = 'SiteArea'
                                            ,byVariableRow = 'Clean.Room'
                                            ,byVariableColumn = 'Category'
                                            ,rowAggregate = "All Rooms"
                                            ,columnAggregate = "Remove")

item220B.table <- data.frame("Room_Type"             = item220B.cast$Clean.Room
                             ,"PSE.Mean"                 = item220B.cast$Mean_PSE
                             ,"PSE.SE"                   = item220B.cast$SE_PSE
                             ,"PSE.n"                    = item220B.cast$n_PSE
                             ,"PSE.King.County_Mean"     = item220B.cast$`Mean_PSE KING COUNTY`
                             ,"PSE.King.County_SE"       = item220B.cast$`SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"        = item220B.cast$n_PSE
                             ,"PSE.Non.King.County_Mean" = item220B.cast$`Mean_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County_SE"   = item220B.cast$`SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"    = item220B.cast$n_PSE
                             ,"2017.RBSA.PS_Mean"        = item220B.cast$`Mean_2017 RBSA PS`
                             ,"2017.RBSA.PS_SE"          = item220B.cast$`SE_2017 RBSA PS`
                             ,"2017.RBSA.PS_n"           = item220B.cast$`n_2017 RBSA PS`)

levels(item220B.table$Room_Type)
rowOrder <- c("Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Lobby"
              ,"Mechanical"
              ,"Office"
              ,"Other"
              ,"Parking"
              ,"Recreation"
              ,"Store"
              ,"All Rooms")
item220B.table <- item220B.table %>% mutate(Room_Type = factor(Room_Type, levels = rowOrder)) %>% arrange(Room_Type)  
item220B.table <- data.frame(item220B.table)

exportTable(item220B.table, "MF", "Table 12", weighted = FALSE, OS = T, osIndicator = "PSE")
