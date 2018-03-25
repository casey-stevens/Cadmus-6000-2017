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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
rbsa.dat.MF <- rbsa.dat[which(rbsa.dat$BuildingType == "Multifamily"),]
rbsa.dat.site <- rbsa.dat.MF[grep("site", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat.MF[grep("bldg", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
# appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))
appliances.dat$CK_Building_ID <- appliances.dat$CK_SiteID


#Read in data for analysis
# buildings.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.interview.export))
#clean cadmus IDs
buildings.interview.dat$CK_Building_ID <- trimws(toupper(buildings.interview.dat$CK_BuildingID))



one.line.bldg.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), sheet = "Building One Line Summary", startRow = 3)
one.line.bldg.dat <- one.line.bldg.dat[which(one.line.bldg.dat$Area.of.Conditioned.Common.Space > 0),]
one.line.bldg.dat$CK_Building_ID <- one.line.bldg.dat$PK_BuildingID

one.line.bldg.dat <- one.line.bldg.dat[names(one.line.bldg.dat) %in% c("CK_Building_ID", "Area.of.Conditioned.Common.Space")]

rbsa.merge <- left_join(rbsa.dat.bldg, one.line.bldg.dat)
rbsa.merge <- rbsa.merge[which(!is.na(rbsa.merge$Area.of.Conditioned.Common.Space)),]



#############################################################################################
#Item 277: AVERAGE NUMBER OF COMMON AREA REFRIGERATORS BY BUILDING SIZE (MF Table 69)
#############################################################################################
item277.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_Building_ID"
                                                                    ,"Iteration"
                                                                    ,"Type"))]

#subset to only buidling level information
item277.dat0 <- item277.dat[grep("BLDG",item277.dat$CK_Building_ID),]

item277.dat00 <- item277.dat0[which(item277.dat0$Type == "Refrigerator"),]

#merge on appliances data with rbsa cleaned data
item277.dat1 <- left_join(rbsa.merge, item277.dat00, by = "CK_Building_ID")
colnames(item277.dat1)[which(colnames(item277.dat1) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"


#subset to only multifamily units
item277.dat2 <- item277.dat1[grep("Multifamily",item277.dat1$BuildingType),]

item277.dat2$Ind <- 0
item277.dat2$Ind[which(item277.dat2$Type == "Refrigerator")] <- 1

######################################
#Pop and Sample Sizes for weights
######################################
item277.data <- weightedData(item277.dat2[which(colnames(item277.dat2) %notin% c("CK_Cadmus_ID.y"
                                                                                 ,"Iteration"
                                                                                 ,"Type"
                                                                                 ,"Ind"
                                                                                 ,"Area.of.Conditioned.Common.Space"))])

item277.data <- left_join(item277.data, item277.dat2[which(colnames(item277.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Iteration"
                                                                                         ,"Type"
                                                                                         ,"Ind"
                                                                                         ,"Area.of.Conditioned.Common.Space"))])
item277.data$count <- 1


######################
# weighted analysis
######################
item277.final <- mean_one_group(CustomerLevelData = item277.data
                                ,valueVariable = 'Ind'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = 'All Sizes')
item277.final <- item277.final[which(names(item277.final) != "BuildingType")]
exportTable(item277.final, "MF", "Table 69", weighted = TRUE)


######################
# weighted analysis
######################
item277.final <- mean_one_group_unweighted(CustomerLevelData = item277.data
                                           ,valueVariable = 'Ind'
                                           ,byVariable = 'HomeType'
                                           ,aggregateRow = 'All Sizes')

item277.final <- item277.final[which(names(item277.final) != "BuildingType")]
exportTable(item277.final, "MF", "Table 69", weighted = FALSE)









#############################################################################################
#Item 278: AVERAGE NUMBER OF COMPUTERS IN COMMON AREAS BY BUILDING OWNERSHIP TYPE (MF Table 70)
#############################################################################################
item278.buildings <- unique(buildings.interview.dat[which(colnames(buildings.interview.dat) %in% c("CK_Building_ID"
                                                                          ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership"
                                                                          ,""))])
which(duplicated(item278.buildings$CK_Cadmus_ID))


item278.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_Building_ID"
                                                                    ,"Type"))]

#subset to only buidling level information
item278.dat0 <- item278.dat[grep("BLDG",item278.dat$CK_Building_ID),]

#merge on appliances data with rbsa cleaned data
item278.dat1 <- left_join(rbsa.dat, item278.dat0, by = "CK_Building_ID")
colnames(item278.dat1)[which(colnames(item278.dat1) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"

#subset to only multifamily units
item278.dat2 <- item278.dat1[grep("Multifamily",item278.dat1$BuildingType),]

#subset to computers in common areas
item278.dat3 <- unique(item278.dat2[which(item278.dat2$Type %in% c("Desktop", "Laptop")),])

#merge with buildings interview data
item278.dat4 <- left_join(rbsa.dat, item278.buildings, by = "CK_Building_ID")
colnames(item278.dat4)[which(colnames(item278.dat4) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"

#remove NA building interview info
item278.dat5 <- item278.dat4[which(!(item278.dat4$INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership %in% c(NA, "Unknown","N/A"))),]
item278.dat5$INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership[which(item278.dat5$INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership %in% c("COOP", "HOA"))] <- "Cooperative"
item278.dat5$INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership[which(item278.dat5$INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership %in% c("LLC"))] <- "Corporation/REIT"
unique(item278.dat5$INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership)

item278.dat6 <- left_join(item278.dat5, item278.dat3)
item278.dat6 <- item278.dat6[grep("3 or fewer floors", item278.dat6$BuildingTypeXX, ignore.case = T),]
item278.dat7 <- item278.dat6[which(item278.dat6$BuildingType == "Multifamily"),]

item278.merge <- item278.dat7
item278.merge$Ind <- 0
item278.merge$Ind[which(item278.merge$Type %notin% c("N/A",NA))] <- 1
unique(item278.merge$Ind)

######################################
#Pop and Sample Sizes for weights
######################################
item278.data <- weightedData(item278.merge[which(colnames(item278.merge) %notin% c("CK_Cadmus_ID.y"
                                                                                 ,"Type"
                                                                                 ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership"
                                                                                 ,"Ind"))])

item278.data <- left_join(item278.data, item278.merge[which(colnames(item278.merge) %in% c("CK_Cadmus_ID"
                                                                                         ,"Type"
                                                                                         ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership"
                                                                                         ,"Ind"))])
item278.data$count <- 1


######################
# weighted analysis
######################
item278.final <- mean_one_group(CustomerLevelData = item278.data
                                ,valueVariable = 'Ind'
                                ,byVariable = "INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership"
                                ,aggregateRow = 'All Types')
item278.final <- item278.final[which(names(item278.final) != "BuildingType")]

exportTable(item278.final, "MF", "Table 70", weighted = TRUE)


######################
# weighted analysis
######################
item278.final <- mean_one_group_unweighted(CustomerLevelData = item278.data
                                           ,valueVariable = 'Ind'
                                           ,byVariable = 'INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership'
                                           ,aggregateRow = 'All Types')
item278.final <- item278.final[which(names(item278.final) != "BuildingType")]

exportTable(item278.final, "MF", "Table 70", weighted = FALSE)


