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
# Item 218: AVERAGE CONDITIONED UNIT FLOOR AREA (SQ.FT.) BY VINTAGE AND UNIT TYPE (MF table 10)
#############################################################################################
item218.dat <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID"
                                                          ,"Area"
                                                          ,"Clean.Type"
                                                          ,"Description"))]

item218.dat1 <- item218.dat[which(item218.dat$Clean.Type == "Bedroom"),]
item218.dat1$count <- 1
item218.dat1$Clean.Type[grep("Studio|studio",item218.dat1$Description)] <- "Studio"

item218.sum <- summarise(group_by(item218.dat1, CK_Cadmus_ID, Clean.Type)
                          ,Count = sum(count))


item218.sum$Unit.Type <- item218.sum$Clean.Type
item218.sum$Unit.Type[which(item218.sum$Count == 1 & item218.sum$Clean.Type == "Bedroom")]  <- "One Bedroom"
item218.sum$Unit.Type[which(item218.sum$Count == 2 & item218.sum$Clean.Type == "Bedroom")]  <- "Two Bedroom"
item218.sum$Unit.Type[which(item218.sum$Count >= 3 & item218.sum$Clean.Type == "Bedroom")]  <- "Three or More Bedrooms"
unique(item218.sum$Unit.Type)

item218.sum1 <- item218.sum[which(colnames(item218.sum) %in% c("CK_Cadmus_ID", "Unit.Type"))]

item218.dat2 <- left_join(item218.sum1, item218.dat, by = "CK_Cadmus_ID")

item218.dat3 <- left_join(item218.dat2, rbsa.dat, by = "CK_Cadmus_ID")
item218.dat3 <- item218.dat3[which(!(is.na(item218.dat3$HomeYearBuilt_MF))),]
#subset to only MF sites
item218.dat3 <- item218.dat3[which(item218.dat3$BuildingTypeXX %in% c("Apartment Building (3 or fewer floors)"
                                                                      ,"Apartment Building (4 to 6 floors)"
                                                                      ,"Apartment Building (More than 6 floors)")),]


item218.dat4 <- item218.dat3[which(!(item218.dat3$Area %in% c("-- Datapoint not asked for --", "NA", NA))),]
item218.dat4$Area <- as.numeric(as.character(item218.dat4$Area))

#summarise up to the site level
item218.SITE <- summarise(group_by(item218.dat4, CK_Cadmus_ID, HomeYearBuilt_MF, Unit.Type)
                               ,SiteArea = sum(Area))

#summarise by vintage
#by unit type
item218.summarise1 <- summarise(group_by(item218.SITE, HomeYearBuilt_MF, Unit.Type)
                                ,Mean = mean(SiteArea)
                                ,SE = sd(SiteArea) / sqrt(length(unique(CK_Cadmus_ID)))
                                ,SampleSize = length(unique(CK_Cadmus_ID)))
#across unit type
item218.summarise2 <- summarise(group_by(item218.SITE, HomeYearBuilt_MF)
                                ,Unit.Type = "All Types"
                                ,Mean = mean(SiteArea)
                                ,SE = sd(SiteArea) / sqrt(length(unique(CK_Cadmus_ID)))
                                ,SampleSize = length(unique(CK_Cadmus_ID)))


#summarise across vintage
#by unit type
item218.summarise3 <- summarise(group_by(item218.SITE, Unit.Type)
                                ,HomeYearBuilt_MF = "All Vintages"
                                ,Mean = mean(SiteArea)
                                ,SE = sd(SiteArea) / sqrt(length(unique(CK_Cadmus_ID)))
                                ,SampleSize = length(unique(CK_Cadmus_ID)))
#across unit type
item218.summarise4 <- summarise(group_by(item218.SITE)
                                ,Unit.Type = "All Types"
                                ,HomeYearBuilt_MF = "All Vintages"
                                ,Mean = mean(SiteArea)
                                ,SE = sd(SiteArea) / sqrt(length(unique(CK_Cadmus_ID)))
                                ,SampleSize = length(unique(CK_Cadmus_ID)))


item218.final <- rbind.data.frame(item218.summarise1, item218.summarise2, item218.summarise3, item218.summarise4, stringsAsFactors = F)

library(data.table)
item218.cast <- dcast(setDT(item218.final)
                      ,formula = HomeYearBuilt_MF ~ Unit.Type
                      ,value.var = c("Mean", "SE", "SampleSize"))



item218.table <- data.frame("Housing.Vintage" = item218.cast$HomeYearBuilt_MF
                            ,"Studio.Mean" = item218.cast$Mean_Studio
                            ,"Studio.SE" = item218.cast$SE_Studio
                            ,"One.Bedroom.Mean" = item218.cast$`Mean_One Bedroom`
                            ,"One.Bedroom.SE" = item218.cast$`SE_One Bedroom`
                            ,"Two.Bedroom.Mean" = item218.cast$`Mean_Two Bedroom`
                            ,"Two.Bedroom.SE" = item218.cast$`SE_Two Bedroom`
                            ,"Three.or.More.Bedroom.Mean" = item218.cast$`Mean_Three or More Bedrooms`
                            ,"Three.or.More.Bedroom.SE" = item218.cast$`SE_Three or More Bedrooms`
                            ,"All.Types.Mean" = item218.cast$`Mean_All Types`
                            ,"All.Types.SE" = item218.cast$`SE_All Types`
                            ,"Sample.Size" = item218.cast$`SampleSize_All Types`)







#############################################################################################
# Item 219: PERCENTAGE BUILDINGS WITH CONDITIONED COMMON AREA BY BUILDING SIZE (MF table 11)
#############################################################################################

item219.dat <- unique(buildings.dat.clean[which(colnames(buildings.dat.clean) %in% c("CK_Cadmus_ID",
                                                                          'PK_BuildingID', 
                                                                          'SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround', 
                                                                          'SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea'))])
#remove any repeat header rows from exporting
item219.dat0 <- item219.dat[which(item219.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item219.dat1 <- left_join(item219.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#Clean Building Size
item219.dat1$BuildingSize <- ifelse(item219.dat1$SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround > 0 &
                                      item219.dat1$SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround <= 3,
                                    'Low-Rise (1-3)',
                                    ifelse(item219.dat1$SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround > 3 &
                                             item219.dat1$SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround <= 6,
                                    'Mid-Rise (4-6)',
                                    ifelse(item219.dat1$SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround > 6,
                                    'High-Rise (7+)','Error')))

#Clean Common Floor Area - is this correct though
item219.dat1$CommonFloorFlag <- ifelse(item219.dat1$SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea == 0 | 
                                         is.na(item219.dat1$SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea),
                                       0,1)

### SUmmarize by building type and also total

item219.summary1 <-summarise(group_by(item219.dat1, BuildingSize)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,TotalWithCommonArea = sum(CommonFloorFlag))

item219.summary2  <- summarise(group_by(item219.dat1)
                                   ,BuildingSize = "All Sizes"
                                   ,SampleSize = length(unique(CK_Cadmus_ID))
                                   ,TotalWithCommonArea = sum(CommonFloorFlag)) 

item219.summaryCombined <- rbind.data.frame(item219.summary1,item219.summary2,stringsAsFactors = F)
item219.summaryCombined$Percent <- item219.summaryCombined$TotalWithCommonArea / item219.summaryCombined$SampleSize
item219.summaryCombined$SE <- sqrt(item219.summaryCombined$Percent * (1 - item219.summaryCombined$Percent) / item219.summaryCombined$SampleSize)

item219.summaryCombinedFinal <- subset(item219.summaryCombined,select = -TotalWithCommonArea)


#############################################################################################
# Item 223: Floor Area by Category (MF table 15)
#############################################################################################
item223.dat <- unique(buildings.dat.clean[which(colnames(buildings.dat.clean) %in% c("CK_Cadmus_ID",
                                                                                     "SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"))])
item223.dat0 <- item223.dat[which(item223.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item223.dat1 <- left_join(item223.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item223.dat1$BuildingSize <- ifelse(item223.dat1$SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround > 0 &
                                      item223.dat1$SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround <= 3,
                                    'Low-Rise (1-3)',
                                    ifelse(item223.dat1$SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround > 3 &
                                             item223.dat1$SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround <= 6,
                                           'Mid-Rise (4-6)',
                                           ifelse(item223.dat1$SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround > 6,
                                                  'High-Rise (7+)','Error')))
item223.dat1$NonResFlag <- ifelse(item223.dat1$)
  
  
  
  
  
  