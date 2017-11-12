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
buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))
length(unique(buildings.dat$CK_Building_ID))
buildings.dat.clean <- buildings.dat[which(!duplicated(buildings.dat$CK_Building_ID)),]


#############################################################################################
# Item 212: DISTRIBUTION OF BUILDINGS BY BUILDING SIZE AND VINTAGE (MF table 4)
#############################################################################################
item212.dat <- rbsa.dat

#subset to only MF homes
item212.dat1 <- item212.dat[grep("Multifamily", item212.dat$BuildingType),]

#remove missing vintage information
item212.dat2 <- item212.dat1[which(!(is.na(item212.dat1$HomeYearBuilt))),]


################################################
# Adding pop and sample sizes for weights
################################################
item212.data <- weightedData(item212.dat2)
item212.data$count <- 1

#######################
# Weighted Analysis
#######################
item212.summary <- proportionRowsAndColumns1(CustomerLevelData = item212.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'HomeYearBuilt_bins_MF'
                                             ,rowVariable = 'HomeType'
                                             ,aggregateColumnName = "All Vintages")
item212.summary <- item212.summary[which(item212.summary$HomeYearBuilt_bins_MF != "All Vintages"),]
item212.summary <- item212.summary[which(item212.summary$HomeType != "Total"),]

item212.all.vintages <- proportions_one_group(CustomerLevelData = item212.data
                                              ,valueVariable = 'count'
                                              ,groupingVariable = 'HomeType'
                                              ,total.name = "All Vintages"
                                              ,columnName = 'HomeYearBuilt_bins_MF'
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
item212.all.vintages <- item212.all.vintages[which(item212.all.vintages$HomeType != "Total"),]


item212.all.sizes <- proportions_one_group(CustomerLevelData = item212.data
                                           ,valueVariable = 'count'
                                           ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                           ,total.name = 'All Sizes'
                                           ,columnName = "HomeType"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)
item212.all.sizes$HomeYearBuilt_bins_MF[which(item212.all.sizes$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"


item212.final <- rbind.data.frame(item212.summary, item212.all.vintages, item212.all.sizes, stringsAsFactors = F)

item212.cast <- dcast(setDT(item212.final)
                      ,formula = HomeYearBuilt_bins_MF ~ HomeType
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N"))

item212.table <- data.frame("Housing.Vintage" = item212.cast$HomeYearBuilt_bins_MF
                            ,"Low-Rise.(1-3)" = item212.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"Low-Rise.SE"    = item212.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"Low-Rise.n"     = item212.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Mid-Rise.(4-6)" = item212.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"Mid-Rise.SE"    = item212.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"Mid-Rise.n"     = item212.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"High-Rise.(7+)" = item212.cast$`w.percent_Apartment Building (More than 6 floors)`
                            ,"High-Rise.SE"   = item212.cast$`w.SE_Apartment Building (More than 6 floors)`
                            ,"High-Rise.n"    = item212.cast$`n_Apartment Building (More than 6 floors)`
                            ,"All.Sizes"      = item212.cast$`w.percent_All Sizes`
                            ,"All.Sizes.SE"   = item212.cast$`w.SE_All Sizes`
                            ,"All.Sizes.n"    = item212.cast$`n_All Sizes`)

exportTable(item212.table, "MF", "Table 4", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item212.summary <- proportions_two_groups_unweighted(CustomerLevelData = item212.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'HomeYearBuilt_bins_MF'
                                             ,rowVariable = 'HomeType'
                                             ,aggregateColumnName = "All Vintages")
item212.summary <- item212.summary[which(item212.summary$HomeYearBuilt_bins_MF != "All Vintages"),]
item212.summary <- item212.summary[which(item212.summary$HomeType != "Total"),]

item212.all.vintages <- proportions_one_group(CustomerLevelData = item212.data
                                              ,valueVariable = 'count'
                                              ,groupingVariable = 'HomeType'
                                              ,total.name = "All Vintages"
                                              ,columnName = 'HomeYearBuilt_bins_MF'
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
item212.all.vintages <- item212.all.vintages[which(item212.all.vintages$HomeType != "Total"),]


item212.all.sizes <- proportions_one_group(CustomerLevelData = item212.data
                                           ,valueVariable = 'count'
                                           ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                           ,total.name = 'All Sizes'
                                           ,columnName = "HomeType"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)
item212.all.sizes$HomeYearBuilt_bins_MF[which(item212.all.sizes$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"


item212.final <- rbind.data.frame(item212.summary, item212.all.vintages, item212.all.sizes, stringsAsFactors = F)

item212.cast <- dcast(setDT(item212.final)
                      ,formula = HomeYearBuilt_bins_MF ~ HomeType
                      ,value.var = c("Percent", "SE", "Count", "SampleSize"))

item212.table <- data.frame("Housing.Vintage" = item212.cast$HomeYearBuilt_bins_MF
                            ,"Low-Rise.(1-3)" = item212.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low-Rise.SE"    = item212.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Low-Rise.n"     = item212.cast$`SampleSize_Apartment Building (3 or fewer floors)`
                            ,"Mid-Rise.(4-6)" = item212.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid-Rise.SE"    = item212.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"Mid-Rise.n"     = item212.cast$`SampleSize_Apartment Building (4 to 6 floors)`
                            ,"High-Rise.(7+)" = item212.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High-Rise.SE"   = item212.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"High-Rise.n"    = item212.cast$`SampleSize_Apartment Building (More than 6 floors)`
                            ,"All.Sizes"      = item212.cast$`Percent_All Sizes`
                            ,"All.Sizes.SE"   = item212.cast$`SE_All Sizes`
                            ,"All.Sizes.n"    = item212.cast$`SampleSize_All Sizes`)

exportTable(item212.table, "MF", "Table 4", weighted = FALSE)




#############################################################################################
# Item 216: DISTRIBUTION OF BUILDING FLOOR AREA BY FLOOR AREA CATEGORY AND BUILDING SIZE (MF table 8)
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
item216.dat1$Total.Nonres.Floor.Area <-
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
item216.dat3 <- item216.dat2[which(item216.dat2$Total.Floor.Area != 0),]

# summarise across building types
item216.sum1 <- summarise(group_by(item216.dat3)
                          ,BuildingTypeXX = "All Sizes"
                          ,Percent_CommonArea = sum(Common.Area) / sum(Total.Floor.Area)
                          ,SE_CommonArea = sqrt(Percent_CommonArea * (1 - Percent_CommonArea) / length(unique(CK_Cadmus_ID)))
                          ,Percent_Residential = sum(Total.Residential.Floor.Area) / sum(Total.Floor.Area)
                          ,SE_Residential = sqrt(Percent_Residential * (1 - Percent_Residential) / length(unique(CK_Cadmus_ID)))
                          ,Percent_Nonres = sum(Total.Nonres.Floor.Area) / sum(Total.Floor.Area)
                          ,SE_Nonres = sqrt(Percent_Nonres * (1 - Percent_Nonres) / length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#item216.dat2 <- data.frame(item216.dat1, "SampleSize" = item216.sum1$SampleSize, stringsAsFactors = F)

# summarise by building types
item216.sum2 <- summarise(group_by(item216.dat3, BuildingTypeXX)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Percent_CommonArea = sum(Common.Area) / sum(Total.Floor.Area)
                          ,SE_CommonArea = sqrt(Percent_CommonArea * (1 - Percent_CommonArea) / unique(SampleSize))
                          ,Percent_Residential = sum(Total.Residential.Floor.Area) / sum(Total.Floor.Area)
                          ,SE_Residential = sqrt(Percent_Residential * (1 - Percent_Residential) / unique(SampleSize))
                          ,Percent_Nonres = sum(Total.Nonres.Floor.Area) / sum(Total.Floor.Area)
                          ,SE_Nonres = sqrt(Percent_Nonres * (1 - Percent_Nonres) / unique(SampleSize)))



item216.final <- rbind.data.frame(item216.sum2, item216.sum1, stringsAsFactors = F)

item216.sub <- item216.final[which(colnames(item216.final) != "SampleSize")]

item216.table <- data.frame(item216.sub,"SampleSize" = item216.final$SampleSize,stringsAsFactors = F)
