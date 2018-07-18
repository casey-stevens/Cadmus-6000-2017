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
source("Code/Table Code/Step 1-Clean Data - Lock Down.R")
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation - MF-BLDG.R")
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
# Item 218: AVERAGE CONDITIONED UNIT FLOOR AREA (SQ.FT.) BY VINTAGE AND UNIT TYPE (MF table 10)
#############################################################################################
item218.dat <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID"
                                                          ,"Area"
                                                          ,"Clean.Type"
                                                          ,"Description"
                                                          ,"Survey.Bedrooms"))]
unique(item218.dat$Survey.Bedrooms)
item218.dat1 <- item218.dat[which(item218.dat$Clean.Type == "Bedroom"),]
item218.dat1$count <- 1
item218.dat1$Clean.Type[grep("Studio|studio",item218.dat1$Description)] <- "Studio"
item218.dat1$Survey.Bedrooms <- as.numeric(as.character(item218.dat1$Survey.Bedrooms))
item218.sum <- summarise(group_by(item218.dat1, CK_Cadmus_ID, Clean.Type)
                          ,Count = sum(count)
                         ,Survey.Bedrooms = sum(unique(Survey.Bedrooms),na.rm = T))

item218.sum$Final.Bedrooms <- item218.sum$Count
for(ii in 1:nrow(item218.sum)){
  if(item218.sum$Survey.Bedrooms[ii] > item218.sum$Final.Bedrooms[ii]){
    item218.sum$Survey.Bedrooms[ii] <- item218.sum$Final.Bedrooms[ii]
  }else{
    item218.sum$Final.Bedrooms[ii]
  }
}

item218.sum$Final.Bedrooms[which(item218.sum$Survey.Bedrooms == 0)] <- 0

item218.sum$Unit.Type <- "Studio"
item218.sum$Unit.Type[which(item218.sum$Final.Bedrooms == 1 & item218.sum$Clean.Type == "Bedroom")]  <- "One Bedroom"
item218.sum$Unit.Type[which(item218.sum$Final.Bedrooms == 2 & item218.sum$Clean.Type == "Bedroom")]  <- "Two Bedroom"
item218.sum$Unit.Type[which(item218.sum$Final.Bedrooms >= 3 & item218.sum$Clean.Type == "Bedroom")]  <- "Three or More Bedrooms"
unique(item218.sum$Unit.Type)

item218.sum1 <- item218.sum[which(colnames(item218.sum) %in% c("CK_Cadmus_ID", "Unit.Type"))]

item218.dat2 <- left_join(item218.dat, item218.sum1)

item218.dat3 <- left_join(rbsa.dat.bldg, item218.dat2)
item218.dat3 <- item218.dat3[which(!(is.na(item218.dat3$HomeYearBuilt_MF))),]
item218.dat3 <- item218.dat3[which(!(is.na(item218.dat3$Unit.Type))),]
#subset to only MF sites
item218.dat4 <- item218.dat3[grep("Multifamily", item218.dat3$BuildingType),]

item218.dat4 <- item218.dat4[which(item218.dat4$Area > 0),]

item218.dat4$Area <- as.numeric(as.character(item218.dat4$Area))

#summarise up to the site level
item218.SITE <- summarise(group_by(item218.dat4, CK_Cadmus_ID, HomeYearBuilt_MF, Unit.Type)
                               ,SiteArea = sum(Area, na.rm = T))
which(duplicated(item218.SITE$CK_Cadmus_ID))

item218.merge <- left_join(rbsa.dat.bldg, item218.SITE)
item218.merge <- item218.merge[which(!is.na(item218.merge$SiteArea)),]


################################################
# Adding pop and sample sizes for weights
################################################
item218.data <- weightedData(item218.merge[which(colnames(item218.merge) %notin% c("Unit.Type"
                                                                                   ,"SiteArea"))])
item218.data <- left_join(item218.data, item218.merge[which(colnames(item218.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Unit.Type"
                                                                                           ,"SiteArea"))])
item218.data$count <- 1

#######################
# Weighted Analysis
#######################
item218.cast <- mean_two_groups(CustomerLevelData = item218.data
                                   ,valueVariable = 'SiteArea'
                                   ,byVariableRow = 'HomeYearBuilt_bins_MF'
                                   ,byVariableColumn = "Unit.Type"
                                   ,columnAggregate = "All.Types"
                                   ,rowAggregate = "All Vintages")


item218.table <- data.frame("Housing.Vintage"             = item218.cast$HomeYearBuilt_bins_MF
                            ,"Studio.Mean"                = item218.cast$Mean_Studio
                            ,"Studio.SE"                  = item218.cast$SE_Studio
                            ,"Studio.n"                   = item218.cast$n_Studio
                            ,"One.Bedroom.Mean"           = item218.cast$`Mean_One Bedroom`
                            ,"One.Bedroom.SE"             = item218.cast$`SE_One Bedroom`
                            ,"One.Bedroom.n"              = item218.cast$`n_One Bedroom`
                            ,"Two.Bedroom.Mean"           = item218.cast$`Mean_Two Bedroom`
                            ,"Two.Bedroom.SE"             = item218.cast$`SE_Two Bedroom`
                            ,"Two.Bedroom.n"              = item218.cast$`n_Two Bedroom`
                            ,"Three.or.More.Bedroom.Mean" = item218.cast$`Mean_Three or More Bedrooms`
                            ,"Three.or.More.Bedroom.SE"   = item218.cast$`SE_Three or More Bedrooms`
                            ,"Three.or.More.Bedroom.n"    = item218.cast$`n_Three or More Bedrooms`
                            ,"All.Types.Mean"             = item218.cast$Mean_All.Types
                            ,"All.Types.SE"               = item218.cast$SE_All.Types
                            ,"All.Types.n"                = item218.cast$n_All.Types
                            ,"Studio.EB"                  = item218.cast$EB_Studio
                            ,"One.Bedroom.EB"             = item218.cast$`EB_One Bedroom`
                            ,"Two.Bedroom.EB"             = item218.cast$`EB_Two Bedroom`
                            ,"Three.or.More.Bedroom.EB"   = item218.cast$`EB_Three or More Bedrooms`
                            ,"All.Types.EB"               = item218.cast$EB_All.Types
                            )
levels(item218.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item218.table <- item218.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item218.table <- data.frame(item218.table)

exportTable(item218.table, "MF", "Table 10", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item218.cast <- mean_two_groups_unweighted(CustomerLevelData = item218.data
                                ,valueVariable = 'SiteArea'
                                ,byVariableRow = 'HomeYearBuilt_bins_MF'
                                ,byVariableColumn = "Unit.Type"
                                ,columnAggregate = "All.Types"
                                ,rowAggregate = "All Vintages")


item218.table <- data.frame("Housing.Vintage"             = item218.cast$HomeYearBuilt_bins_MF
                            ,"Studio.Mean"                = item218.cast$Mean_Studio
                            ,"Studio.SE"                  = item218.cast$SE_Studio
                            ,"Studio.n"                   = item218.cast$n_Studio
                            ,"One.Bedroom.Mean"           = item218.cast$`Mean_One Bedroom`
                            ,"One.Bedroom.SE"             = item218.cast$`SE_One Bedroom`
                            ,"One.Bedroom.n"              = item218.cast$`n_One Bedroom`
                            ,"Two.Bedroom.Mean"           = item218.cast$`Mean_Two Bedroom`
                            ,"Two.Bedroom.SE"             = item218.cast$`SE_Two Bedroom`
                            ,"Two.Bedroom.n"              = item218.cast$`n_Two Bedroom`
                            ,"Three.or.More.Bedroom.Mean" = item218.cast$`Mean_Three or More Bedrooms`
                            ,"Three.or.More.Bedroom.SE"   = item218.cast$`SE_Three or More Bedrooms`
                            ,"Three.or.More.Bedroom.n"    = item218.cast$`n_Three or More Bedrooms`
                            ,"All.Types.Mean"             = item218.cast$Mean_All.Types
                            ,"All.Types.SE"               = item218.cast$SE_All.Types
                            ,"All.Types.n"                = item218.cast$n_All.Types
                            )

levels(item218.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item218.table <- item218.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item218.table <- data.frame(item218.table)

exportTable(item218.table, "MF", "Table 10", weighted = FALSE)







#############################################################################################
# Item 219: PERCENTAGE BUILDINGS WITH CONDITIONED COMMON AREA BY BUILDING SIZE (MF table 11)
#############################################################################################

item219.dat <- unique(buildings.dat.clean[which(colnames(buildings.dat.clean) %in% c("CK_Building_ID", 
                                                                                     'SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea'))])
#merge together analysis data with cleaned RBSA data
item219.dat1 <- left_join(rbsa.dat.bldg, item219.dat)

#Clean Common Floor Area - is this correct though
item219.dat1$CommonFloorFlag <- ifelse(item219.dat1$SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea == 0 | 
                                         is.na(item219.dat1$SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea),
                                       0,1)

item219.dat2 <- item219.dat1[which(item219.dat1$BuildingType == "Multifamily"),]


################################################
# Adding pop and sample sizes for weights
################################################
item219.data <- weightedData(item219.dat2[which(colnames(item219.dat2) %notin% c("SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"                                          
                                                                                 ,"CommonFloorFlag"))])
item219.data <- left_join(item219.data, item219.dat2[which(colnames(item219.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"                                     
                                                                                         ,"CommonFloorFlag"))])
item219.data$Count <- 1
item219.data$Ind <- item219.data$CommonFloorFlag
#######################
# Weighted Analysis
#######################
item219.final <- proportions_one_group(CustomerLevelData = item219.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'HomeType'
                                       ,total.name = "All Sizes"
                                       ,weighted = TRUE)
item219.final <- item219.final[which(names(item219.final) != "BuildingType")]
exportTable(item219.final, "MF", "Table 11", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item219.final <- proportions_one_group(CustomerLevelData = item219.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'HomeType'
                                       ,total.name = "All Sizes"
                                       ,weighted = FALSE)
item219.final <- item219.final[which(names(item219.final) != "BuildingType")]
exportTable(item219.final, "MF", "Table 11", weighted = FALSE)




#############################################################################################
# Item 223: Floor Area by Category (MF table 15)
#############################################################################################
item223.dat <- unique(buildings.dat.clean[which(colnames(buildings.dat.clean) %in% c("CK_Building_ID",
                                                                                     "SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"))])
item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt <- as.numeric(as.character(item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt))
item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt <- as.numeric(as.character(item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt))
item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt <- as.numeric(as.character(item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt))
item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt <- as.numeric(as.character(item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt))
item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt <- as.numeric(as.character(item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt))
item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt <- as.numeric(as.character(item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt))


item223.dat$Nonres.Area <- na.pass(item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt + 
  item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt +
  item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt +
  item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt +
  item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt +
  item223.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt)

item223.nonres.data <- data.frame("CK_Building_ID" = item223.dat$CK_Building_ID
                                  ,"Nonres.Area" = item223.dat$Nonres.Area
                                  ,stringsAsFactors = F)

item223.dat1 <- left_join(rbsa.dat.bldg, item223.nonres.data)


item223.dat1$Ind <- 0
item223.dat1$Ind[which(!is.na(item223.dat1$Nonres.Area))] <- 1
item223.dat1$Ind[which(item223.dat1$Nonres.Area == 0)] <- 0
sort(unique(item223.dat1$Nonres.Area))
  
item223.dat2 <- item223.dat1[which(item223.dat1$BuildingType == "Multifamily"),]


################################################
# Adding pop and sample sizes for weights
################################################
item223.data <- weightedData(item223.dat2[which(colnames(item223.dat2) %notin% c("Nonres.Area"
                                                                                 ,"Ind"))])
item223.data <- left_join(item223.data, unique(item223.dat2[which(colnames(item223.dat2) %in% c("CK_Building_ID"
                                                                                         ,"Nonres.Area"
                                                                                         ,"Ind"))]))
item223.data$Count <- 1
which(duplicated(item223.data$CK_Cadmus_ID))
#######################
# Weighted Analysis
#######################
item223.final <- proportions_one_group(CustomerLevelData = item223.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'HomeType'
                                       ,total.name       = "All Sizes"
                                       ,weighted         = TRUE)

item223.final.MF <- item223.final[which(item223.final$BuildingType == "Multifamily"),
                                  -which(names(item223.final) == "BuildingType")]

exportTable(item223.final.MF, "MF", "Table 15", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item223.final <- proportions_one_group(CustomerLevelData = item223.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'HomeType'
                                       ,total.name       = "All Sizes"
                                       ,weighted         = FALSE)

item223.final.MF <- item223.final[which(item223.final$BuildingType == "Multifamily"),
                                  -which(names(item223.final) == "BuildingType")]

exportTable(item223.final.MF, "MF", "Table 15", weighted = FALSE)

  