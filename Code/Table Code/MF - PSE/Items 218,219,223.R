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
# Item 218A: AVERAGE CONDITIONED UNIT FLOOR AREA (SQ.FT.) BY VINTAGE AND UNIT TYPE (MF table 10)
#############################################################################################
item218A.dat <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID"
                                                          ,"Area"
                                                          ,"Clean.Type"
                                                          ,"Description"
                                                          ,"Survey.Bedrooms"))]
unique(item218A.dat$Survey.Bedrooms)
item218A.dat1 <- item218A.dat[which(item218A.dat$Clean.Type == "Bedroom"),]
item218A.dat1$count <- 1
item218A.dat1$Clean.Type[grep("Studio apartment",item218A.dat1$Description,ignore.case = T)] <- "Studio"
item218A.dat1$Survey.Bedrooms <- as.numeric(as.character(item218A.dat1$Survey.Bedrooms))
item218A.sum <- summarise(group_by(item218A.dat1, CK_Cadmus_ID, Clean.Type)
                          ,Count = sum(count)
                         ,Survey.Bedrooms = sum(unique(Survey.Bedrooms),na.rm = T))

item218A.sum$Final.Bedrooms <- item218A.sum$Count
for(ii in 1:nrow(item218A.sum)){
  if(item218A.sum$Survey.Bedrooms[ii] > item218A.sum$Final.Bedrooms[ii]){
    item218A.sum$Survey.Bedrooms[ii] <- item218A.sum$Final.Bedrooms[ii]
  }else{
    item218A.sum$Final.Bedrooms[ii]
  }
}

item218A.sum$Final.Bedrooms[which(item218A.sum$Survey.Bedrooms == 0)] <- 0

item218A.sum$Unit.Type <- NA
item218A.sum$Unit.Type[which(item218A.sum$Final.Bedrooms == 0 & item218A.sum$Clean.Type == "Bedroom")]  <- "Studio"
item218A.sum$Unit.Type[which(item218A.sum$Final.Bedrooms == 1 & item218A.sum$Clean.Type == "Bedroom")]  <- "One Bedroom"
item218A.sum$Unit.Type[which(item218A.sum$Final.Bedrooms == 2 & item218A.sum$Clean.Type == "Bedroom")]  <- "Two Bedroom"
item218A.sum$Unit.Type[which(item218A.sum$Final.Bedrooms >= 3 & item218A.sum$Clean.Type == "Bedroom")]  <- "Three or More Bedrooms"
unique(item218A.sum$Unit.Type)

item218A.sum1 <- item218A.sum[which(colnames(item218A.sum) %in% c("CK_Cadmus_ID", "Unit.Type"))]

item218A.dat2 <- left_join(item218A.dat, item218A.sum1)
item218A.dat2 <- item218A.dat2[which(item218A.dat2$Area > 0),]

item218A.dat2$Area <- as.numeric(as.character(item218A.dat2$Area))

#summarise up to the site level
item218A.SITE <- summarise(group_by(item218A.dat2, CK_Cadmus_ID, Unit.Type)
                               ,SiteArea = sum(Area, na.rm = T))

item218A.dat3 <- left_join(rbsa.dat.bldg, item218A.SITE)
item218A.dat3 <- item218A.dat3[which(!(is.na(item218A.dat3$HomeYearBuilt_MF))),]
item218A.dat3 <- item218A.dat3[which(!(is.na(item218A.dat3$Unit.Type))),]
#subset to only MF sites
item218A.dat4 <- item218A.dat3[grep("Multifamily", item218A.dat3$BuildingType),]

item218A.merge <- item218A.dat4[which(!is.na(item218A.dat4$SiteArea)),]
item218A.merge <- item218A.merge[which(item218A.merge$Category == "PSE"),]
unique(item218A.merge$Unit.Type)
################################################
# Adding pop and sample sizes for weights
################################################
item218A.data <- weightedData(item218A.merge[which(colnames(item218A.merge) %notin% c("Unit.Type"
                                                                                   ,"SiteArea"
                                                                                   ,"Category"))])
item218A.data <- left_join(item218A.data, item218A.merge[which(colnames(item218A.merge) %in% c("CK_Cadmus_ID"
                                                                                               ,"CK_Building_ID"
                                                                                               ,"Unit.Type"
                                                                                               ,"SiteArea"
                                                                                               ,"Category"))])
item218A.data$count <- 1

#######################
# Weighted Analysis
#######################
item218A.cast <- mean_two_groups(CustomerLevelData = item218A.data
                                   ,valueVariable = 'SiteArea'
                                   ,byVariableRow = 'HomeYearBuilt_bins_MF'
                                   ,byVariableColumn = "Unit.Type"
                                   ,columnAggregate = "All.Types"
                                   ,rowAggregate = "All Vintages")
names(item218A.cast)

item218A.table <- data.frame("Housing.Vintage"             = item218A.cast$HomeYearBuilt_bins_MF
                            ,"Studio.Mean"                = NA#item218A.cast$Mean_Studio
                            ,"Studio.SE"                  = NA#item218A.cast$SE_Studio
                            ,"Studio.n"                   = NA#item218A.cast$n_Studio
                            ,"One.Bedroom.Mean"           = item218A.cast$`Mean_One Bedroom`
                            ,"One.Bedroom.SE"             = item218A.cast$`SE_One Bedroom`
                            ,"One.Bedroom.n"              = item218A.cast$`n_One Bedroom`
                            ,"Two.Bedroom.Mean"           = item218A.cast$`Mean_Two Bedroom`
                            ,"Two.Bedroom.SE"             = item218A.cast$`SE_Two Bedroom`
                            ,"Two.Bedroom.n"              = item218A.cast$`n_Two Bedroom`
                            ,"Three.or.More.Bedroom.Mean" = item218A.cast$`Mean_Three or More Bedrooms`
                            ,"Three.or.More.Bedroom.SE"   = item218A.cast$`SE_Three or More Bedrooms`
                            ,"Three.or.More.Bedroom.n"    = item218A.cast$`n_Three or More Bedrooms`
                            ,"All.Types.Mean"             = item218A.cast$Mean_All.Types
                            ,"All.Types.SE"               = item218A.cast$SE_All.Types
                            ,"All.Types.n"                = item218A.cast$n_All.Types
                            ,"Studio.EB"                  = NA#item218A.cast$EB_Studio
                            ,"One.Bedroom.EB"             = item218A.cast$`EB_One Bedroom`
                            ,"Two.Bedroom.EB"             = item218A.cast$`EB_Two Bedroom`
                            ,"Three.or.More.Bedroom.EB"   = item218A.cast$`EB_Three or More Bedrooms`
                            ,"All.Types.EB"               = item218A.cast$EB_All.Types
                            )
levels(item218A.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item218A.table <- item218A.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item218A.table <- data.frame(item218A.table)

exportTable(item218A.table, "MF", "Table 10A", weighted = TRUE,OS = T,osIndicator = "PSE")


#######################
# Unweighted Analysis
#######################
item218A.cast <- mean_two_groups_unweighted(CustomerLevelData = item218A.data
                                ,valueVariable = 'SiteArea'
                                ,byVariableRow = 'HomeYearBuilt_bins_MF'
                                ,byVariableColumn = "Unit.Type"
                                ,columnAggregate = "All.Types"
                                ,rowAggregate = "All Vintages")


item218A.table <- data.frame("Housing.Vintage"             = item218A.cast$HomeYearBuilt_bins_MF
                            ,"Studio.Mean"                = NA#item218A.cast$Mean_Studio
                            ,"Studio.SE"                  = NA#item218A.cast$SE_Studio
                            ,"Studio.n"                   = NA#item218A.cast$n_Studio
                            ,"One.Bedroom.Mean"           = item218A.cast$`Mean_One Bedroom`
                            ,"One.Bedroom.SE"             = item218A.cast$`SE_One Bedroom`
                            ,"One.Bedroom.n"              = item218A.cast$`n_One Bedroom`
                            ,"Two.Bedroom.Mean"           = item218A.cast$`Mean_Two Bedroom`
                            ,"Two.Bedroom.SE"             = item218A.cast$`SE_Two Bedroom`
                            ,"Two.Bedroom.n"              = item218A.cast$`n_Two Bedroom`
                            ,"Three.or.More.Bedroom.Mean" = item218A.cast$`Mean_Three or More Bedrooms`
                            ,"Three.or.More.Bedroom.SE"   = item218A.cast$`SE_Three or More Bedrooms`
                            ,"Three.or.More.Bedroom.n"    = item218A.cast$`n_Three or More Bedrooms`
                            ,"All.Types.Mean"             = item218A.cast$Mean_All.Types
                            ,"All.Types.SE"               = item218A.cast$SE_All.Types
                            ,"All.Types.n"                = item218A.cast$n_All.Types
                            )

levels(item218A.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item218A.table <- item218A.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item218A.table <- data.frame(item218A.table)

exportTable(item218A.table, "MF", "Table 10A", weighted = FALSE,OS = T,osIndicator = "PSE")


#############################################################################################
# Item 218B: AVERAGE CONDITIONED UNIT FLOOR AREA (SQ.FT.) BY VINTAGE AND UNIT TYPE (MF table 10)
#############################################################################################
item218B.dat <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID"
                                                          ,"Area"
                                                          ,"Clean.Type"
                                                          ,"Description"
                                                          ,"Survey.Bedrooms"))]
unique(item218B.dat$Survey.Bedrooms)
item218B.dat1 <- item218B.dat[which(item218B.dat$Clean.Type == "Bedroom"),]
item218B.dat1$count <- 1
item218B.dat1$Clean.Type[grep("Studio|studio",item218B.dat1$Description)] <- "Studio"
item218B.dat1$Survey.Bedrooms <- as.numeric(as.character(item218B.dat1$Survey.Bedrooms))
item218B.sum <- summarise(group_by(item218B.dat1, CK_Cadmus_ID, Clean.Type)
                         ,Count = sum(count)
                         ,Survey.Bedrooms = sum(unique(Survey.Bedrooms),na.rm = T))

item218B.sum$Final.Bedrooms <- item218B.sum$Count
for(ii in 1:nrow(item218B.sum)){
  if(item218B.sum$Survey.Bedrooms[ii] > item218B.sum$Final.Bedrooms[ii]){
    item218B.sum$Survey.Bedrooms[ii] <- item218B.sum$Final.Bedrooms[ii]
  }else{
    item218B.sum$Final.Bedrooms[ii]
  }
}

item218B.sum$Final.Bedrooms[which(item218B.sum$Survey.Bedrooms == 0)] <- 0

item218B.sum$Unit.Type <- "Studio"
item218B.sum$Unit.Type[which(item218B.sum$Final.Bedrooms == 1 & item218B.sum$Clean.Type == "Bedroom")]  <- "One Bedroom"
item218B.sum$Unit.Type[which(item218B.sum$Final.Bedrooms == 2 & item218B.sum$Clean.Type == "Bedroom")]  <- "Two Bedroom"
item218B.sum$Unit.Type[which(item218B.sum$Final.Bedrooms >= 3 & item218B.sum$Clean.Type == "Bedroom")]  <- "Three or More Bedrooms"
unique(item218B.sum$Unit.Type)

item218B.sum1 <- item218B.sum[which(colnames(item218B.sum) %in% c("CK_Cadmus_ID", "Unit.Type"))]

item218B.dat2 <- left_join(item218B.dat, item218B.sum1)

item218B.dat3 <- left_join(rbsa.dat.bldg, item218B.dat2)
item218B.dat3 <- item218B.dat3[which(!(is.na(item218B.dat3$HomeYearBuilt_MF))),]
item218B.dat3 <- item218B.dat3[which(!(is.na(item218B.dat3$Unit.Type))),]
#subset to only MF sites
item218B.dat4 <- item218B.dat3[grep("Multifamily", item218B.dat3$BuildingType),]

item218B.dat4 <- item218B.dat4[which(item218B.dat4$Area > 0),]

item218B.dat4$Area <- as.numeric(as.character(item218B.dat4$Area))

#summarise up to the site level
item218B.SITE <- summarise(group_by(item218B.dat4, CK_Cadmus_ID, Category, HomeYearBuilt_MF, Unit.Type)
                          ,SiteArea = sum(Area, na.rm = T))
which(duplicated(item218B.SITE$CK_Cadmus_ID))

item218B.merge <- left_join(rbsa.dat.bldg, item218B.SITE)
item218B.merge <- item218B.merge[which(!is.na(item218B.merge$SiteArea)),]


################################################
# Adding pop and sample sizes for weights
################################################
item218B.data <- weightedData(unique(item218B.merge[which(colnames(item218B.merge) %notin% c("Unit.Type"
                                                                                             ,"SiteArea"
                                                                                             ,"Category"))]))
item218B.data <- left_join(item218B.data, unique(item218B.merge[which(colnames(item218B.merge) %in% c("CK_Cadmus_ID"
                                                                                                      ,"CK_Building_ID"
                                                                                                      ,"Unit.Type"
                                                                                                      ,"SiteArea"
                                                                                                      ,"Category"))]))
item218B.data$count <- 1

#######################
# Weighted Analysis
#######################
item218B.cast <- mean_two_groups(CustomerLevelData = item218B.data
                                ,valueVariable = 'SiteArea'
                                ,byVariableRow = 'HomeYearBuilt_bins_MF'
                                ,byVariableColumn = "Category"
                                ,columnAggregate = "Remove"
                                ,rowAggregate = "All Vintages")


item218B.table <- data.frame("Housing.Vintage"          = item218B.cast$HomeYearBuilt_bins_MF
                            ,"PSE.Mean"                 = item218B.cast$Mean_PSE
                            ,"PSE.SE"                   = item218B.cast$SE_PSE
                            ,"PSE.n"                    = item218B.cast$n_PSE
                            ,"PSE.King.County.Mean"     = item218B.cast$`Mean_PSE KING COUNTY`
                            ,"PSE.King.County.SE"       = item218B.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"        = item218B.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Mean" = item218B.cast$`Mean_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"   = item218B.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"    = item218B.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Mean"        = item218B.cast$`Mean_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"          = item218B.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"           = item218B.cast$`n_2017 RBSA PS`
                            ,"PSE.EB"                   = item218B.cast$EB_PSE
                            ,"PSE.King.County.EB"       = item218B.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County.EB"   = item218B.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.EB"          = item218B.cast$`EB_2017 RBSA PS`
)
levels(item218B.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item218B.table <- item218B.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item218B.table <- data.frame(item218B.table)

exportTable(item218B.table, "MF", "Table 10B", weighted = TRUE,OS = T,osIndicator = "PSE")


#######################
# Unweighted Analysis
#######################
item218B.cast <- mean_two_groups_unweighted(CustomerLevelData = item218B.data
                                           ,valueVariable = 'SiteArea'
                                           ,byVariableRow = 'HomeYearBuilt_bins_MF'
                                           ,byVariableColumn = "Category"
                                           ,columnAggregate = "Remove"
                                           ,rowAggregate = "All Vintages")


item218B.table <- data.frame("Housing.Vintage"             = item218B.cast$HomeYearBuilt_bins_MF
                             ,"PSE.Mean"                 = item218B.cast$Mean_PSE
                             ,"PSE.SE"                   = item218B.cast$SE_PSE
                             ,"PSE.n"                    = item218B.cast$n_PSE
                             ,"PSE.King.County.Mean"     = item218B.cast$`Mean_PSE KING COUNTY`
                             ,"PSE.King.County.SE"       = item218B.cast$`SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"        = item218B.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Mean" = item218B.cast$`Mean_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"   = item218B.cast$`SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"    = item218B.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Mean"        = item218B.cast$`Mean_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"          = item218B.cast$`SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"           = item218B.cast$`n_2017 RBSA PS`
)

levels(item218B.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item218B.table <- item218B.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item218B.table <- data.frame(item218B.table)

exportTable(item218B.table, "MF", "Table 10B", weighted = FALSE,OS = T,osIndicator = "PSE")







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
                                                                                 ,"CommonFloorFlag"
                                                                                 ,"Category"))])
item219.data <- left_join(item219.data, item219.dat2[which(colnames(item219.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"                                     
                                                                                         ,"CommonFloorFlag"
                                                                                         ,"Category"))])
item219.data$Count <- 1
item219.data$Ind <- item219.data$CommonFloorFlag
#######################
# Weighted Analysis
#######################
item219.data$State <- item219.data$Category
item219.final <- proportions_one_group(CustomerLevelData = item219.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Remove"
                                       ,weighted = TRUE)
item219.final <- item219.final[which(item219.final$State != "Total"),which(names(item219.final) != "BuildingType")]
exportTable(item219.final, "MF", "Table 11", weighted = TRUE,OS = T,osIndicator = "PSE")


#######################
# Unweighted Analysis
#######################
item219.final <- proportions_one_group(CustomerLevelData = item219.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Remove"
                                       ,weighted = FALSE)
item219.final <- item219.final[which(item219.final$State != "Total"),which(names(item219.final) != "BuildingType")]
exportTable(item219.final, "MF", "Table 11", weighted = FALSE,OS = T,osIndicator = "PSE")




#############################################################################################
# Item 223A: Floor Area by Category (MF table 15)
#############################################################################################
item223A.dat <- unique(buildings.dat.clean[which(colnames(buildings.dat.clean) %in% c("CK_Building_ID",
                                                                                     "SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"))])
item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt <- as.numeric(as.character(item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt))
item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt <- as.numeric(as.character(item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt))
item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt <- as.numeric(as.character(item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt))
item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt <- as.numeric(as.character(item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt))
item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt <- as.numeric(as.character(item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt))
item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt <- as.numeric(as.character(item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt))


item223A.dat$Nonres.Area <- na.pass(item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt + 
  item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt +
  item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt +
  item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt +
  item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt +
  item223A.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt)

item223A.nonres.data <- data.frame("CK_Building_ID" = item223A.dat$CK_Building_ID
                                  ,"Nonres.Area" = item223A.dat$Nonres.Area
                                  ,stringsAsFactors = F)

item223A.dat1 <- left_join(rbsa.dat.bldg, item223A.nonres.data)


item223A.dat1$Ind <- 0
item223A.dat1$Ind[which(!is.na(item223A.dat1$Nonres.Area))] <- 1
item223A.dat1$Ind[which(item223A.dat1$Nonres.Area == 0)] <- 0
sort(unique(item223A.dat1$Nonres.Area))
  
item223A.dat2 <- item223A.dat1[which(item223A.dat1$BuildingType == "Multifamily"),]
item223A.dat2 <- item223A.dat2[which(item223A.dat2$Category == "PSE"),]

################################################
# Adding pop and sample sizes for weights
################################################
item223A.data <- weightedData(item223A.dat2[which(colnames(item223A.dat2) %notin% c("Nonres.Area"
                                                                                 ,"Ind"
                                                                                 ,"Category"))])
item223A.data <- left_join(item223A.data, unique(item223A.dat2[which(colnames(item223A.dat2) %in% c("CK_Building_ID"
                                                                                         ,"Nonres.Area"
                                                                                         ,"Ind"
                                                                                         ,"Category"))]))
item223A.data$Count <- 1
which(duplicated(item223A.data$CK_Cadmus_ID))
#######################
# Weighted Analysis
#######################
item223A.final <- proportions_one_group(CustomerLevelData = item223A.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'HomeType'
                                       ,total.name       = "All Sizes"
                                       ,weighted         = TRUE)

item223A.final.MF <- item223A.final[which(item223A.final$BuildingType == "Multifamily"),
                                  -which(names(item223A.final) == "BuildingType")]

exportTable(item223A.final.MF, "MF", "Table 15A", weighted = TRUE,OS = T,osIndicator = "PSE")


#######################
# Unweighted Analysis
#######################
item223A.final <- proportions_one_group(CustomerLevelData = item223A.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'HomeType'
                                       ,total.name       = "All Sizes"
                                       ,weighted         = FALSE)

item223A.final.MF <- item223A.final[which(item223A.final$BuildingType == "Multifamily"),
                                  -which(names(item223A.final) == "BuildingType")]

exportTable(item223A.final.MF, "MF", "Table 15A", weighted = FALSE,OS = T,osIndicator = "PSE")



#############################################################################################
# Item 223B: Floor Area by Category (MF table 15)
#############################################################################################
item223B.dat <- unique(buildings.dat.clean[which(colnames(buildings.dat.clean) %in% c("CK_Building_ID",
                                                                                     "SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt",
                                                                                     "SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"))])
item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt <- as.numeric(as.character(item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt))
item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt <- as.numeric(as.character(item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt))
item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt <- as.numeric(as.character(item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt))
item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt <- as.numeric(as.character(item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt))
item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt <- as.numeric(as.character(item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt))
item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt <- as.numeric(as.character(item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt))


item223B.dat$Nonres.Area <- na.pass(item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt + 
                                     item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt +
                                     item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt +
                                     item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt +
                                     item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt +
                                     item223B.dat$SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt)

item223B.nonres.data <- data.frame("CK_Building_ID" = item223B.dat$CK_Building_ID
                                  ,"Nonres.Area" = item223B.dat$Nonres.Area
                                  ,stringsAsFactors = F)

item223B.dat1 <- left_join(rbsa.dat.bldg, item223B.nonres.data)


item223B.dat1$Ind <- 0
item223B.dat1$Ind[which(!is.na(item223B.dat1$Nonres.Area))] <- 1
item223B.dat1$Ind[which(item223B.dat1$Nonres.Area == 0)] <- 0
sort(unique(item223B.dat1$Nonres.Area))

item223B.dat2 <- item223B.dat1[which(item223B.dat1$BuildingType == "Multifamily"),]


################################################
# Adding pop and sample sizes for weights
################################################
item223B.data <- weightedData(item223B.dat2[which(colnames(item223B.dat2) %notin% c("Nonres.Area"
                                                                                 ,"Ind"
                                                                                 ,"Category"))])
item223B.data <- left_join(item223B.data, unique(item223B.dat2[which(colnames(item223B.dat2) %in% c("CK_Building_ID"
                                                                                                ,"Nonres.Area"
                                                                                                ,"Ind"
                                                                                                ,"Category"))]))
item223B.data$Count <- 1
which(duplicated(item223B.data$CK_Cadmus_ID))
#######################
# Weighted Analysis
#######################
item223B.data$State <- item223B.data$Category
item223B.final <- proportions_one_group(CustomerLevelData = item223B.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name       = "Remove"
                                       ,weighted         = TRUE)
item223B.final <-item223B.final[which(item223B.final$State != "Total"),]
item223B.final.MF <- item223B.final[which(item223B.final$BuildingType == "Multifamily"),
                                  -which(names(item223B.final) == "BuildingType")]

exportTable(item223B.final.MF, "MF", "Table 15B", weighted = TRUE,OS = T,osIndicator = "PSE")


#######################
# Unweighted Analysis
#######################
item223B.final <- proportions_one_group(CustomerLevelData = item223B.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name       = "Remove"
                                       ,weighted         = FALSE)
item223B.final <-item223B.final[which(item223B.final$State != "Total"),]

item223B.final.MF <- item223B.final[which(item223B.final$BuildingType == "Multifamily"),
                                  -which(names(item223B.final) == "BuildingType")]

exportTable(item223B.final.MF, "MF", "Table 15B", weighted = FALSE,OS = T,osIndicator = "PSE")

  