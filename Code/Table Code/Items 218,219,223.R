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
# Item 218: AVERAGE CONDITIONED UNIT FLOOR AREA (SQ.FT.) BY VINTAGE AND UNIT TYPE (MF table 10)
#############################################################################################

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
  
  
  
  
  
  