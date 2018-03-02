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
rbsa.dat.site <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat[grep("bldg",rbsa.dat$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))
length(unique(buildings.dat$CK_Building_ID))
buildings.dat.clean <- buildings.dat[which(!duplicated(buildings.dat$CK_Building_ID)),]


#Read in data for analysis
rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
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

rooms.dat1$Area <- as.numeric(as.character(rooms.dat1$Area))
# rooms.dat1$Area[which(rooms.dat1$Area == "N/A")] <- 0
rooms.dat2 <- rooms.dat1#[which(rooms.dat1$Area > 0),]
rooms.dat2 <- rooms.dat2[which(!is.na(rooms.dat2$Area)),]

rooms.dat3 <- rooms.dat2[grep("BLDG", rooms.dat2$CK_SiteID),]
rooms.dat4 <- summarise(group_by(rooms.dat3, CK_SiteID, Clean.Room)
                        ,SiteArea = mean(Area, na.rm = T))

rooms.cast <- dcast(setDT(rooms.dat4)
                    ,formula = CK_SiteID ~ Clean.Room
                    ,value.var = c("SiteArea"))
rooms.cast[is.na(rooms.cast),] <- 0

rooms.melt <- melt(rooms.cast, id.vars = c("CK_SiteID"))
names(rooms.melt) <- c("CK_SiteID", "Clean.Room", "SiteArea") 

#merge together analysis data with cleaned RBSA data
rooms.final <- left_join(rbsa.dat.bldg, rooms.melt, by = c("CK_Building_ID"="CK_SiteID"))
rooms.final <- rooms.final[which(rooms.final$BuildingType == "Multifamily"),]
rooms.final <- rooms.final[which(rooms.final$SiteArea %notin% c("N/A",NA)),]
names(rooms.final)[which(names(rooms.final) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID" 


#############################################################################################
# Item 220: AVERAGE COMMON AREA ROOM TYPE FLOOR AREA (SQ.FT.) BY BUILDING SIZE (MF table 12)
#############################################################################################
item220.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"
                                                                  ,"CK_Building_ID"))]
colnames(item220.dat) <- c( "CommonFloorArea","CK_Building_ID")
item220.dat$CK_Building_ID <- as.character(item220.dat$CK_Building_ID)

# join on rooms and rbsa cleaned data:
item220.dat1 <- left_join(rooms.final,item220.dat)

#make numeric
item220.dat1$CommonFloorArea <- as.numeric(as.character(item220.dat1$CommonFloorArea))
item220.dat1$CommonFloorArea[which(is.na(item220.dat1$CommonFloorArea))] <- 0

item220.dat1$SiteArea <- as.numeric(as.character(item220.dat1$SiteArea))

#remove any NAs
item220.dat2 <- item220.dat1[which(!is.na(item220.dat1$SiteArea)),]

unique(item220.dat2$Clean.Room)

#subset to only relevant columns
item220.merge <- left_join(rbsa.dat, item220.dat2)
item220.merge <- item220.merge[which(!is.na(item220.merge$Clean.Room)),]
item220.merge <- item220.merge[which(item220.merge$BuildingType == "Multifamily"),]
# item220.merge$Total.Area[which(is.na(item220.merge$Total.Area))] <- 0


item220.data <- weightedData(item220.merge[which(colnames(item220.merge) %notin% c("CK_Cadmus_ID.y"
                                                                                   ,"Clean.Room"
                                                                                   ,"SiteArea"
                                                                                   # ,"Total.Area"
                                                                                   ,"CommonFloorArea"))])
item220.data <- left_join(item220.data, item220.merge[which(colnames(item220.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Clean.Room"
                                                                                           ,"SiteArea"
                                                                                           # ,"Total.Area"
                                                                                           ,"CommonFloorArea"))])
item220.data$count <- 1
##################################
# weighted analysis
##################################
item220.cast <- mean_two_groups(CustomerLevelData = item220.data
                                  ,valueVariable = 'SiteArea'
                                  ,byVariableRow = 'Clean.Room'
                                  ,byVariableColumn = 'HomeType'
                                  ,rowAggregate = "All Rooms"
                                  ,columnAggregate = "All Sizes")
names(item220.cast)
item220.table <- data.frame("Room_Type"             = item220.cast$Clean.Room
                            ,"Low_Rise_1.3_Mean"    = item220.cast$`Mean_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_SE"          = item220.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_4.6_Mean"    = item220.cast$`Mean_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_SE"          = item220.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_7Plus_Mean" = NA #item220.cast$`Mean_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE"         = NA #item220.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_Mean"       = item220.cast$`Mean_All Sizes`
                            ,"All_Sizes_SE"         = item220.cast$`SE_All Sizes`
                            ,"n"                    = item220.cast$`n_All Sizes`
                            ,"Low_Rise_EB"          = item220.cast$`EB_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_EB"          = item220.cast$`EB_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_EB"         = NA #item220.cast$`EB_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_EB"         = item220.cast$`EB_All Sizes`
                            )

levels(item220.table$Room_Type)
rowOrder <- c("Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Lobby"
              ,"Mechanical"
              ,"Office"
              ,"Other"
              ,"Outside"
              ,"Parking"
              ,"Recreation"
              ,"Store"
              ,"All Rooms")
item220.table <- item220.table %>% mutate(Room_Type = factor(Room_Type, levels = rowOrder)) %>% arrange(Room_Type)  
item220.table <- data.frame(item220.table)

exportTable(item220.table, "MF", "Table 12", weighted = TRUE)

##################################
# unweighted analysis
##################################
item220.cast <- mean_two_groups_unweighted(CustomerLevelData = item220.data
                                ,valueVariable = 'SiteArea'
                                ,byVariableRow = 'Clean.Room'
                                ,byVariableColumn = 'HomeType'
                                ,rowAggregate = "All Rooms"
                                ,columnAggregate = "All Sizes")

item220.table <- data.frame("Room_Type"             = item220.cast$Clean.Room
                            ,"Low_Rise_1.3_Mean"    = item220.cast$`Mean_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_SE"          = item220.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_4.6_Mean"    = item220.cast$`Mean_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_SE"          = item220.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_7Plus_Mean" = NA #item220.cast$`Mean_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE"         = NA #item220.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_Mean"       = item220.cast$`Mean_All Sizes`
                            ,"All_Sizes_SE"         = item220.cast$`SE_All Sizes`
                            ,"SampleSize"           = item220.cast$`n_All Sizes`)

levels(item220.table$Room_Type)
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
item220.table <- item220.table %>% mutate(Room_Type = factor(Room_Type, levels = rowOrder)) %>% arrange(Room_Type)  
item220.table <- data.frame(item220.table)

exportTable(item220.table, "MF", "Table 12", weighted = FALSE)
