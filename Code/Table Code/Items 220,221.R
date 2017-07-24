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
buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Cadmus_ID <- trimws(toupper(buildings.dat$CK_Cadmus_ID))
length(unique(buildings.dat$CK_Cadmus_ID)) #196/201 unique

buildings.dat.clean <- buildings.dat[-which(duplicated(buildings.dat$CK_Cadmus_ID)),]


#Read in data for analysis
rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
#clean cadmus IDs
rooms.dat$CK_Cadmus_ID <- trimws(toupper(rooms.dat$CK_Cadmus_ID))




#############################################################################################
#For Rooms
#############################################################################################

#subset to columns needed for analysis
rooms.dat1 <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID"
                                                         ,"Clean.Type"
                                                         ,"Area"))]
colnames(rooms.dat1) <- c("CK_Cadmus_ID","Clean.Room","Area")

#remove any repeat header rows from exporting
rooms.dat2 <- rooms.dat1[which(rooms.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

rooms.dat3 <- rooms.dat2[which(!(rooms.dat2$Area %in% c("0", "Unknown", NA, "-- Datapoint not asked for --"))),]
rooms.dat3$Area <- as.numeric(as.character(rooms.dat3$Area))

rooms.dat4 <- summarise(group_by(rooms.dat3, CK_Cadmus_ID, Clean.Room)
                        ,SiteArea = sum(Area))




#merge together analysis data with cleaned RBSA data
rooms.dat5 <- left_join(rooms.dat4, rbsa.dat, by = "CK_Cadmus_ID")




#############################################################################################
# Item 220: AVERAGE COMMON AREA ROOM TYPE FLOOR AREA (SQ.FT.) BY BUILDING SIZE (MF table 12)
#############################################################################################
item220.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Cadmus_ID"
                                                                     ,"SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"
                                                                     ,""))]
colnames(item220.dat) <- c("CK_Cadmus_ID", "CommonFloorArea")

# join on rooms and rbsa cleaned data:
item220.dat1 <- left_join(rooms.dat5, item220.dat, by = c("CK_Cadmus_ID"))
item220.dat1$CommonFloorArea[which(is.na(item220.dat1$CommonFloorArea))] <- 0

#make numeric
item220.dat1$CommonFloorArea <- as.numeric(as.character(item220.dat1$CommonFloorArea))
item220.dat1$SiteArea <- as.numeric(as.character(item220.dat1$SiteArea))


#calculate total area
item220.dat1$Total.Area <- item220.dat1$SiteArea + item220.dat1$CommonFloorArea

#remove any NAs
item220.dat2 <- item220.dat1[which(!(is.na(item220.dat1$Total.Area))),] #didn't lose any

# subset to only clean rooms types wanted:
item220.dat3 <- item220.dat2[which(item220.dat2$Clean.Room %in% c("Hall"
                                                                  ,"Kitchen"
                                                                  ,"Laundry" 
                                                                  ,"Lobby" 
                                                                  ,"Mechanical" 
                                                                  ,"Office" 
                                                                  ,"Recreation" 
                                                                  ,"Bathroom" 
                                                                  ,"Store" #equivalent to closets? Store? 
                                                                  ,"Other")),]
unique(item220.dat3$Clean.Room)

#subset to only relevant columns
item220.dat4 <- item220.dat3[which(item220.dat3$BuildingTypeXX %in% c("Apartment Building (3 or fewer floors)"
                                                                       ,"Apartment Building (4 to 6 floors)"
                                                                       ,"Apartment Building (More than 6 floors)")),]


#summarise by buildingtypeXX and room.type 
item220.sum1 <- summarise(group_by(item220.dat4, BuildingTypeXX, Clean.Room)
                          ,Mean = mean(SiteArea)
                          ,SE = sd(SiteArea) / sqrt(length(unique(CK_Cadmus_ID))))
#summarise across buildingtypeXX but by room.type 
item220.sum2 <- summarise(group_by(item220.dat4, Clean.Room)
                          ,BuildingTypeXX = "All Sizes"
                          ,Mean = mean(SiteArea)
                          ,SE = sd(SiteArea) / sqrt(length(unique(CK_Cadmus_ID))))

item220.merge1 <- rbind.data.frame(item220.sum1, item220.sum2, stringsAsFactors = F)

#calculate total sample sizes
item220.SampleSize <-  summarise(group_by(item220.dat4, Clean.Room)
                                 ,SampleSize = length(unique(CK_Cadmus_ID)))

item220.merge2 <- left_join(item220.merge1, item220.SampleSize, by = "Clean.Room")


library(data.table)
item220.cast <- dcast(setDT(item220.merge2)
                      ,formula = Clean.Room ~ BuildingTypeXX
                      ,value.var = c("Mean", "SE", "SampleSize"))


item220.table <- data.frame("Room_Type" = item220.cast$Clean.Room
                            ,"Low_Rise_1.3_Mean" = item220.cast$`Mean_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_SE" = item220.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_4.6_Mean" = item220.cast$`Mean_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_SE" = item220.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_7Plus_Mean" = item220.cast$`Mean_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE" = item220.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_Mean" = item220.cast$`Mean_All Sizes`
                            ,"All_Sizes_SE" = item220.cast$`SE_All Sizes`
                            ,"SampleSize" = item220.cast$`SampleSize_All Sizes`)









#############################################################################################
# Item 221: DISTRIBUTION OF BUILDING FLOOR AREA BY FLOOR CATEGORY AND BUILDING SIZE (MF table 13)
#############################################################################################
item221.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_TotalResidentialFloorArea"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_TotEnclosedBldgArea_IncludResidentialAndCommercialButExcludPkgGarages"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"
                                                                  ,""))]

colnames(item221.dat) <- c("CK_Cadmus_ID"
                           ,"Common.Area"
                           ,"Total.Residential.Floor.Area"
                           ,"Total.Residential.Commercial.Floor.Area"
                           ,"Total.Commercial.Area"
                           ,"Nonres.Grocery.SQFT"
                           ,"Nonres.Office.SQFT"
                           ,"Nonres.Other.SQFT"
                           ,"Nonres.Retail.SQFT"
                           ,"Nonres.Vacant.SQFT")

item221.dat[is.na(item221.dat)] <- 0

for (i in 2:ncol(item221.dat)){
  item221.dat[,i] <- as.numeric(as.character(item221.dat[,i]))
}

#calcualte combined total floor area across common area, res, and nonres
item221.dat$Total.Floor.Area <- item221.dat$Total.Commercial.Area +
  item221.dat$Total.Residential.Floor.Area +
  item221.dat$Common.Area


item221.merge <- left_join(item221.dat, rbsa.dat, by = "CK_Cadmus_ID")

item221.dat1 <- item221.merge[grep("Multifamily", item221.merge$BuildingType),]

# summarise across building types
item221.sum1 <- summarise(group_by(item221.dat1)
                          ,BuildingTypeXX = "All Sizes"
                          ,Percent_CommonArea = sum(Common.Area) / sum(Total.Floor.Area)
                          ,SE_CommonArea = sqrt(Percent_CommonArea * (1 - Percent_CommonArea) / length(unique(CK_Cadmus_ID)))
                          ,Percent_Residential = sum(Total.Residential.Floor.Area) / sum(Total.Floor.Area)
                          ,SE_Residential = sqrt(Percent_Residential * (1 - Percent_Residential) / length(unique(CK_Cadmus_ID)))
                          ,Percent_Nonres = sum(Total.Commercial.Area) / sum(Total.Floor.Area)
                          ,SE_Nonres = sqrt(Percent_Nonres * (1 - Percent_Nonres) / length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

item221.dat2 <- data.frame(item221.dat1, "SampleSize" = item221.sum1$SampleSize, stringsAsFactors = F)

# summarise by building types
item221.sum2 <- summarise(group_by(item221.dat2, BuildingTypeXX)
                          ,Percent_CommonArea = sum(Common.Area) / sum(Total.Floor.Area)
                          ,SE_CommonArea = sqrt(Percent_CommonArea * (1 - Percent_CommonArea) / unique(SampleSize))
                          ,Percent_Residential = sum(Total.Residential.Floor.Area) / sum(Total.Floor.Area)
                          ,SE_Residential = sqrt(Percent_Residential * (1 - Percent_Residential) / unique(SampleSize))
                          ,Percent_Nonres = sum(Total.Commercial.Area) / sum(Total.Floor.Area)
                          ,SE_Nonres = sqrt(Percent_Nonres * (1 - Percent_Nonres) / unique(SampleSize))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))


item221.final <- rbind.data.frame(item221.sum2, item221.sum1, stringsAsFactors = F)
