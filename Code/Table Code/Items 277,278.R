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
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))

#Read in data for analysis
buildings.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.interview.export))
#clean cadmus IDs
buildings.interview.dat$CK_Cadmus_ID <- trimws(toupper(buildings.interview.dat$CK_Cadmus_ID))





#############################################################################################
#Item 277: AVERAGE NUMBER OF COMMON AREA REFRIGERATORS BY BUILDING SIZE (MF Table 69)
#############################################################################################
item277.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Iteration"
                                                                    ,"Type"))]

#subset to only buidling level information
item277.dat0 <- item277.dat[grep("BLDG",item277.dat$Iteration),]

item277.dat00 <- item277.dat0[which(item277.dat0$Type == "Refrigerator"),]

#merge on appliances data with rbsa cleaned data
item277.dat1 <- left_join(rbsa.dat, item277.dat00, by = "CK_Cadmus_ID")

#subset to only multifamily units
item277.dat2 <- item277.dat1[grep("Multifamily",item277.dat1$BuildingType),]

item277.dat2$Refrigerator.Ind <- 0
item277.dat2$Refrigerator.Ind[which(item277.dat2$Type == "Refrigerator")] <- 1

#summarise by buildings size
item277.sum1 <- summarise(group_by(item277.dat2, BuildingTypeXX)
                          ,Mean = mean(Refrigerator.Ind)
                          ,SE = sd(Refrigerator.Ind) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#summarise across building size
item277.sum2 <- summarise(group_by(item277.dat2)
                          ,BuildingTypeXX = "All Sizes"
                          ,Mean = mean(Refrigerator.Ind)
                          ,SE = sd(Refrigerator.Ind) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#merge
item277.final <- rbind.data.frame(item277.sum1, item277.sum2, stringsAsFactors = F)

item277.table <- data.frame("Building.Size" = item277.final$BuildingTypeXX
                            ,"Mean" = item277.final$Mean
                            ,"SE" = item277.final$SE
                            ,"SampleSize" = item277.final$SampleSize)




















#############################################################################################
#Item 278: AVERAGE NUMBER OF COMPUTERS IN COMMON AREAS BY BUILDING OWNERSHIP TYPE (MF Table 70)
#############################################################################################
item278.buildings <- unique(buildings.interview.dat[which(colnames(buildings.interview.dat) %in% c("CK_Cadmus_ID"
                                                                          ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership"
                                                                          ,""))])
which(duplicated(item278.buildings$CK_Cadmus_ID))


item278.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Iteration"
                                                                    ,"Type"))]

#subset to only buidling level information
item278.dat0 <- item278.dat[grep("BLDG",item278.dat$Iteration),]

#merge on appliances data with rbsa cleaned data
item278.dat1 <- left_join(rbsa.dat, item278.dat0, by = "CK_Cadmus_ID")

#subset to only multifamily units
item278.dat2 <- item278.dat1[grep("Multifamily",item278.dat1$BuildingType),]

#subset to computers in common areas
item278.dat3 <- item278.dat2#[which(item278.dat2$Type %in% c("Desktop", "Laptop")),]

#merge with buildings interview data
item278.dat4 <- left_join(item278.dat3, item278.buildings, by = "CK_Cadmus_ID")

#remove NA building interview info
item278.dat5 <- item278.dat4[which(!(item278.dat4$INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership %in% c(NA, "Unknown"))),]


####################
# there are no common area computers in the dataset, so cannot complete after here
####################