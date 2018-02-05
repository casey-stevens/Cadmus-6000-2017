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

rooms.dat2 <- rooms.dat1[which(rooms.dat1$Area > 0),]
rooms.dat2$Area <- as.numeric(as.character(rooms.dat2$Area))

rooms.dat3 <- rooms.dat2[grep("BLDG", rooms.dat2$CK_SiteID),]

rooms.dat4 <- summarise(group_by(rooms.dat3, CK_Cadmus_ID, CK_SiteID, Clean.Room)
                        ,SiteArea = mean(Area, na.rm = T))

#merge together analysis data with cleaned RBSA data
rooms.final <- left_join(rbsa.dat.bldg, rooms.dat4, by = c("CK_Building_ID"="CK_SiteID"))
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

#calculate total area
item220.dat1$Total.Area <- item220.dat1$SiteArea + item220.dat1$CommonFloorArea

#remove any NAs
item220.dat2 <- item220.dat1[which(!(is.na(item220.dat1$Total.Area))),]

# subset to only clean rooms types wanted:
# item220.dat3 <- item220.dat2[which(item220.dat2$Clean.Room %in% c("Hall"
#                                                                   ,"Kitchen"
#                                                                   ,"Laundry" 
#                                                                   ,"Lobby" 
#                                                                   ,"Mechanical" 
#                                                                   ,"Office" 
#                                                                   ,"Recreation" 
#                                                                   ,"Bathroom" 
#                                                                   ,"Store" #equivalent to closets? Store? 
#                                                                   ,"Closet"
#                                                                   ,"Other")),]
unique(item220.dat2$Clean.Room)

#subset to only relevant columns
item220.merge <- left_join(rbsa.dat, item220.dat2)
item220.merge <- item220.merge[which(!is.na(item220.merge$Clean.Room)),]
item220.merge <- item220.merge[which(item220.merge$BuildingType == "Multifamily"),]
item220.merge$Total.Area[which(is.na(item220.merge$Total.Area))] <- 0


item220.data <- weightedData(item220.merge[which(colnames(item220.merge) %notin% c("CK_Cadmus_ID.y"
                                                                                   ,"Clean.Room"
                                                                                   ,"SiteArea"
                                                                                   ,"Total.Area"
                                                                                   ,"CommonFloorArea"))])
item220.data <- left_join(item220.data, item220.merge[which(colnames(item220.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Clean.Room"
                                                                                           ,"SiteArea"
                                                                                           ,"Total.Area"
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






#############################################################################################
# Item 221: DISTRIBUTION OF BUILDING FLOOR AREA BY FLOOR CATEGORY AND BUILDING SIZE (MF table 13)
#############################################################################################
# item221.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Cadmus_ID"
#                                                                   ,"SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"
#                                                                   ,"SITES_MFB_cfg_MFB_CONFIG_TotalResidentialFloorArea"
#                                                                   ,"SITES_MFB_cfg_MFB_CONFIG_TotEnclosedBldgArea_IncludResidentialAndCommercialButExcludPkgGarages"
#                                                                   # ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt"
#                                                                   ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt"
#                                                                   ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt"
#                                                                   ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt"
#                                                                   ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt"
#                                                                   ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"
#                                                                   ,""))]
# 
# colnames(item221.dat) <- c("CK_Cadmus_ID"
#                            ,"Common.Area"
#                            ,"Total.Residential.Floor.Area"
#                            ,"Total.Residential.Commercial.Floor.Area"
#                            # ,"Total.Commercial.Area"
#                            ,"Nonres.Grocery.SQFT"
#                            ,"Nonres.Office.SQFT"
#                            ,"Nonres.Other.SQFT"
#                            ,"Nonres.Retail.SQFT"
#                            ,"Nonres.Vacant.SQFT")
# 
# item221.dat[is.na(item221.dat)] <- 0
# 
# for (i in 2:ncol(item221.dat)){
#   item221.dat[,i] <- as.numeric(as.character(item221.dat[,i]))
# }
# 
# #calculate total nonres floor area
# item221.dat$Total.Commercial.Area <- item221.dat$Nonres.Office.SQFT +
#   item221.dat$Nonres.Grocery.SQFT +
#   item221.dat$Nonres.Other.SQFT +
#   item221.dat$Nonres.Retail.SQFT +
#   item221.dat$Nonres.Vacant.SQFT
# 
# #calcualte combined total floor area across common area, res, and nonres
# item221.dat$Total.Floor.Area <- item221.dat$Total.Commercial.Area +
#   item221.dat$Total.Residential.Floor.Area +
#   item221.dat$Common.Area
# 
# 
# item221.merge <- left_join(item221.dat, rbsa.dat, by = "CK_Cadmus_ID")
# 
# item221.dat1 <- item221.merge[grep("Multifamily", item221.merge$BuildingType),]
# 
# # summarise by building types
# item221.sum1 <- summarise(group_by(item221.dat1, BuildingTypeXX)
#                           ,Percent_CommonArea = sum(Common.Area) / sum(Total.Floor.Area)
#                           ,SE_CommonArea = sqrt(Percent_CommonArea * (1 - Percent_CommonArea) / length(unique(CK_Cadmus_ID)))
#                           ,Percent_Residential = sum(Total.Residential.Floor.Area) / sum(Total.Floor.Area)
#                           ,SE_Residential = sqrt(Percent_Residential * (1 - Percent_Residential) / length(unique(CK_Cadmus_ID)))
#                           ,Percent_Nonres = sum(Total.Commercial.Area) / sum(Total.Floor.Area)
#                           ,SE_Nonres = sqrt(Percent_Nonres * (1 - Percent_Nonres) / length(unique(CK_Cadmus_ID)))
#                           ,SampleSize = length(unique(CK_Cadmus_ID)))
# # summarise across building types
# item221.sum2 <- summarise(group_by(item221.dat1)
#                           ,BuildingTypeXX = "All Sizes"
#                           ,Percent_CommonArea = sum(Common.Area) / sum(Total.Floor.Area)
#                           ,SE_CommonArea = sqrt(Percent_CommonArea * (1 - Percent_CommonArea) / length(unique(CK_Cadmus_ID)))
#                           ,Percent_Residential = sum(Total.Residential.Floor.Area) / sum(Total.Floor.Area)
#                           ,SE_Residential = sqrt(Percent_Residential * (1 - Percent_Residential) / length(unique(CK_Cadmus_ID)))
#                           ,Percent_Nonres = sum(Total.Commercial.Area) / sum(Total.Floor.Area)
#                           ,SE_Nonres = sqrt(Percent_Nonres * (1 - Percent_Nonres) / length(unique(CK_Cadmus_ID)))
#                           ,SampleSize = length(unique(CK_Cadmus_ID)))
# 
# 
# item221.final <- rbind.data.frame(item221.sum1, item221.sum2, stringsAsFactors = F)
