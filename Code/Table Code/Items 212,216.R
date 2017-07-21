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


#############################################################################################
# Item 212: DISTRIBUTION OF BUILDINGS BY BUILDING SIZE AND VINTAGE (MF table 4)
#############################################################################################
item212.dat <- rbsa.dat
item212.dat1$count <- 1

#subset to only MF homes
item212.dat2 <- item212.dat1[grep("Multifamily", item212.dat1$BuildingType),]

#remove missing vintage information
item212.dat3 <- item212.dat2[which(!(is.na(item212.dat2$HomeYearBuilt))),]

#summarise by Vintage
#by buildingtypeXX
item212.size <- summarise(group_by(item212.dat3, HomeYearBuilt_MF, BuildingTypeXX)
                             ,SampleSize = length(unique(CK_Cadmus_ID))
                             ,Count = sum(count))
#across buildingtypeXX
item212.all <- summarise(group_by(item212.dat3, HomeYearBuilt_MF)
                         ,BuildingTypeXX = "All Sizes"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

#summarise across Vintage
#by buildingtypeXX
item212.size1 <- summarise(group_by(item212.dat3, BuildingTypeXX)
                          ,HomeYearBuilt_MF = "All Vintages"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Count = sum(count))
#across buildingtypeXX
item212.all1 <- summarise(group_by(item212.dat3)
                         ,BuildingTypeXX = "All Sizes"
                         ,HomeYearBuilt_MF = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))
#join vintage info
item212.vintage <- rbind.data.frame(item212.size, item212.all, item212.size1, item212.all1, stringsAsFactors = F)

item212.vintage$Total.Count <- 0
item212.vintage$Total.Count[which(item212.vintage$HomeYearBuilt_MF == "Pre 1955")] <- item212.vintage$Count[which(item212.vintage$BuildingTypeXX == "All Sizes" & item212.vintage$HomeYearBuilt_MF == "Pre 1955")]
item212.vintage$Total.Count[which(item212.vintage$HomeYearBuilt_MF == "1955-1970")] <- item212.vintage$Count[which(item212.vintage$BuildingTypeXX == "All Sizes" & item212.vintage$HomeYearBuilt_MF == "1955-1970")]
item212.vintage$Total.Count[which(item212.vintage$HomeYearBuilt_MF == "1971-1980")] <- item212.vintage$Count[which(item212.vintage$BuildingTypeXX == "All Sizes" & item212.vintage$HomeYearBuilt_MF == "1971-1980")]
item212.vintage$Total.Count[which(item212.vintage$HomeYearBuilt_MF == "1981-1990")] <- item212.vintage$Count[which(item212.vintage$BuildingTypeXX == "All Sizes" & item212.vintage$HomeYearBuilt_MF == "1981-1990")]
item212.vintage$Total.Count[which(item212.vintage$HomeYearBuilt_MF == "1991-2000")] <- item212.vintage$Count[which(item212.vintage$BuildingTypeXX == "All Sizes" & item212.vintage$HomeYearBuilt_MF == "1991-2000")]
item212.vintage$Total.Count[which(item212.vintage$HomeYearBuilt_MF == "Post 2000")] <- item212.vintage$Count[which(item212.vintage$BuildingTypeXX == "All Sizes" & item212.vintage$HomeYearBuilt_MF == "Post 2000")]
item212.vintage$Total.Count[which(item212.vintage$HomeYearBuilt_MF == "All Vintages")] <- item212.vintage$Count[which(item212.vintage$BuildingTypeXX == "All Sizes" & item212.vintage$HomeYearBuilt_MF == "All Vintages")]
item212.vintage$Total.Count[which(item212.vintage$BuildingTypeXX == "All Sizes")] <- item212.vintage$Count[which(item212.vintage$BuildingTypeXX == "All Sizes" & item212.vintage$HomeYearBuilt_MF == "All Vintages")]

item212.vintage$Percent <- item212.vintage$Count / item212.vintage$Total.Count
item212.vintage$SE <- sqrt(item212.vintage$Percent * (1 - item212.vintage$Percent) / item212.vintage$SampleSize)

library(data.table)
item212.cast <- dcast(setDT(item212.vintage)
                      ,formula = HomeYearBuilt_MF ~ BuildingTypeXX
                      ,value.var = c("Percent", "SE", "SampleSize"))

item212.final <- data.frame("Housing.Vintage" = item212.cast$HomeYearBuilt_MF
                            ,"Low-Rise.(1-3)" = item212.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low-Rise.SE" = item212.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Mid-Rise.(4-6)" = item212.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid-Rise.SE" = item212.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"High-Rise.(7+)" = item212.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High-Rise.SE" = item212.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"All.Sizes" = item212.cast$`Percent_All Sizes`
                            ,"All.Sizes.SE" = item212.cast$`SE_All Sizes`
                            ,"SampleSize" = item212.cast$`SampleSize_All Sizes`)




#############################################################################################
# Item 216: DISTRIBUTION OF BUILDING FLOOR AREA BY FLOOR AREA CATEGORY AND BUILDING SIZE (MF table 4)
#############################################################################################
item216.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_TotalResidentialFloorArea"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_TotEnclosedBldgArea_IncludResidentialAndCommercialButExcludPkgGarages"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"))]
colnames(item216.dat) <- c("CK_Cadmus_ID"
                           ,"Common.Area"
                           ,"Number.of.Floors"
                           ,"Total.Residential.Floor.Area"
                           ,"Total.Residential.Commercial.Floor.Area"
                           ,"Commercial.Area"
                           ,"Nonres.Grocery.SQFT"
                           ,"Nonres.Office.SQFT"
                           ,"Nonres.Other.SQFT"
                           ,"Nonres.Retail.SQFT"
                           ,"Nonres.Vacant.SQFT")

item216.dat1 <- item216.dat[which(!(is.na(item216.dat$Number.of.Floors))),]
item216.dat1[is.na(item216.dat1)] <- 0

for (i in 2:ncol(item216.dat1)){
  item216.dat1[,i] <- as.numeric(as.character(item216.dat1[,i]))
}

#calculate total nonresidential floor area
item216.dat1$Total.Nonres.Floor.Area <- item216.dat1$Commercial.Area +
  item216.dat1$Nonres.Grocery.SQFT + 
  item216.dat1$Nonres.Office.SQFT +
  item216.dat1$Nonres.Other.SQFT +
  item216.dat1$Nonres.Retail.SQFT +
  item216.dat1$Nonres.Vacant.SQFT

#calcualte combined total floor area across common area, res, and nonres
item216.dat1$Total.Floor.Area <- item216.dat1$Total.Nonres.Floor.Area +
  item216.dat1$Total.Residential.Floor.Area +
  item216.dat1$Common.Area


item216.merge <- left_join(item216.dat1, rbsa.dat, by = "CK_Cadmus_ID")

item216.dat2 <- item216.merge[grep("Multifamily", item216.merge$BuildingType),]

# summarise by building types
item216.sum1 <- summarise(group_by(item216.dat2, BuildingTypeXX)
                         ,Percent_CommonArea = sum(Common.Area) / sum(Total.Floor.Area)
                         ,SE_CommonArea = sqrt(Percent_CommonArea * (1 - Percent_CommonArea) / length(unique(CK_Cadmus_ID)))
                         ,Percent_Residential = sum(Total.Residential.Floor.Area) / sum(Total.Floor.Area)
                         ,SE_Residential = sqrt(Percent_Residential * (1 - Percent_Residential) / length(unique(CK_Cadmus_ID)))
                         ,Percent_Nonres = sum(Total.Nonres.Floor.Area) / sum(Total.Floor.Area)
                         ,SE_Nonres = sqrt(Percent_Nonres * (1 - Percent_Nonres) / length(unique(CK_Cadmus_ID)))
                         ,SampleSize = length(unique(CK_Cadmus_ID)))

# summarise across building types
item216.sum2 <- summarise(group_by(item216.dat2)
                          ,BuildingTypeXX = "All Sizes"
                          ,Percent_CommonArea = sum(Common.Area) / sum(Total.Floor.Area)
                          ,SE_CommonArea = sqrt(Percent_CommonArea * (1 - Percent_CommonArea) / length(unique(CK_Cadmus_ID)))
                          ,Percent_Residential = sum(Total.Residential.Floor.Area) / sum(Total.Floor.Area)
                          ,SE_Residential = sqrt(Percent_Residential * (1 - Percent_Residential) / length(unique(CK_Cadmus_ID)))
                          ,Percent_Nonres = sum(Total.Nonres.Floor.Area) / sum(Total.Floor.Area)
                          ,SE_Nonres = sqrt(Percent_Nonres * (1 - Percent_Nonres) / length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

item216.final <- rbind.data.frame(item216.sum1, item216.sum2, stringsAsFactors = F)


