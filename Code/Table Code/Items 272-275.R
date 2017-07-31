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






#############################################################################################
#Item 272: PERCENTAGE OF BUILDINGS WITH ELEVATORS BY BUILDING SIZE (MF Table 64)
#############################################################################################
item272.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"SITES_MFB_MFB_MISC_NumberOfElevators"))]

#merge on buildings data with rbsa cleaned data
item272.dat1 <- left_join(item272.dat, rbsa.dat, by = "CK_Cadmus_ID")

#subset to only multifamily units
item272.dat2 <- item272.dat1[grep("Multifamily",item272.dat1$BuildingType),]

item272.dat2$Elevator.Ind <- item272.dat2$SITES_MFB_MFB_MISC_NumberOfElevators
item272.dat2$Elevator.Ind[which(item272.dat2$SITES_MFB_MFB_MISC_NumberOfElevators > 0)] <- 1
unique(item272.dat2$Elevator.Ind)

item272.dat3 <- item272.dat2[which(!(is.na(item272.dat2$Elevator.Ind))),]
item272.dat3$count <- 1

item272.dat4 <- item272.dat3[which(!(duplicated(item272.dat3$CK_Cadmus_ID))),]

#summarise by building size
item272.sum1 <- summarise(group_by(item272.dat3, BuildingTypeXX)
                          ,Count = sum(Elevator.Ind)
                          ,Total.Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#summarise across building size
item272.sum2 <- summarise(group_by(item272.dat3)
                          ,BuildingTypeXX = "All Sizes"
                          ,Count = sum(Elevator.Ind)
                          ,Total.Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#merge
item272.final <- rbind.data.frame(item272.sum1, item272.sum2, stringsAsFactors = F)

item272.final$Percent <- item272.final$Count / item272.final$Total.Count
item272.final$SE <- sqrt(item272.final$Percent * (1 - item272.final$Percent) / item272.final$SampleSize)

item272.table <- data.frame("Building.Size" = item272.final$BuildingTypeXX
                            ,"Percent" = item272.final$Percent
                            ,"SE" = item272.final$SE
                            ,"SampleSize" = item272.final$SampleSize)
  







#############################################################################################
#Item 273: AVERAGE NUMBER OF ELEVATORS (IN BUILDINGS WITH ELEVATORS) BY BUILDING SIZE (MF Table 65)
#############################################################################################
item273.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"SITES_MFB_MFB_MISC_NumberOfElevators"))]

#remove sites with missing or no elevators
item273.dat0 <- unique(item273.dat[which(!(item273.dat$SITES_MFB_MFB_MISC_NumberOfElevators %in% c(0, NA))),])

#merge on buildings data with rbsa cleaned data
item273.dat1 <- left_join(item273.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#subset to only multifamily units
item273.dat2 <- item273.dat1[grep("Multifamily",item273.dat1$BuildingType),]

#summarise by building size
item273.sum1 <- summarise(group_by(item273.dat2, BuildingTypeXX)
                          ,Mean = mean(SITES_MFB_MFB_MISC_NumberOfElevators)
                          ,SE = sd(SITES_MFB_MFB_MISC_NumberOfElevators) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#summarise across building size
item273.sum2 <- summarise(group_by(item273.dat2)
                          ,BuildingTypeXX = "All Sizes"
                          ,Mean = mean(SITES_MFB_MFB_MISC_NumberOfElevators)
                          ,SE = sd(SITES_MFB_MFB_MISC_NumberOfElevators) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#merge
item273.final <- rbind.data.frame(item273.sum1, item273.sum2, stringsAsFactors = F)

item273.table <- data.frame("Building.Size" = item273.final$BuildingTypeXX
                            ,"Mean" = item273.final$Mean
                            ,"SE" = item273.final$SE
                            ,"SampleSize" = item273.final$SampleSize)













#############################################################################################
#Item 274: PERCENTAGE OF BUILDINGS WITH POOLS BY POOL TYPE AND BUILDING SIZE (MF Table 66)
#############################################################################################

item274.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"SITES_Pool_POOL_HOT_TUB_PoolType"
                                                                  ,"SITES_Pool_POOL_HOT_TUB_PoolLocation"))]

#remove sites with missing or no elevators
item274.dat0 <- unique(item274.dat[which(!(item274.dat$SITES_Pool_POOL_HOT_TUB_PoolType %in% c(NA))),])

#merge on buildings data with rbsa cleaned data
item274.dat1 <- left_join(item274.dat0, rbsa.dat, by = "CK_Cadmus_ID")
which(duplicated(item272.dat1$CK_Cadmus_ID))

#subset to only multifamily units
item274.dat2 <- item274.dat1[grep("Multifamily",item274.dat1$BuildingType),]

unique(item274.dat2$SITES_Pool_POOL_HOT_TUB_PoolType)

item274.dat2$PoolCount <- 0
item274.dat2$PoolCount[grep("pool|Pool",item274.dat2$SITES_Pool_POOL_HOT_TUB_PoolType)] <- 1
item274.dat2$count <- 1

item274.dat3 <- item274.dat2[which(!(is.na(item274.dat2$SITES_Pool_POOL_HOT_TUB_PoolLocation))),]

#summarise by building size
#by pool location
item274.sum1 <- summarise(group_by(item274.dat3, BuildingTypeXX, SITES_Pool_POOL_HOT_TUB_PoolLocation)
                          ,Percent = sum(PoolCount) / length(unique(item274.dat2$CK_Cadmus_ID))
                          ,SE = sqrt(Percent * (1 - Percent) / (length(unique(item274.dat2$CK_Cadmus_ID)))))
#across pool location
item274.sum2 <- summarise(group_by(item274.dat3, BuildingTypeXX)
                          ,SITES_Pool_POOL_HOT_TUB_PoolLocation = "All Pools"
                          ,Percent = sum(PoolCount) / length(unique(item274.dat2$CK_Cadmus_ID))
                          ,SE = sqrt(Percent * (1 - Percent) / (length(unique(item274.dat2$CK_Cadmus_ID)))))

#summarise across building size
#by pool location
item274.sum3 <- summarise(group_by(item274.dat3, SITES_Pool_POOL_HOT_TUB_PoolLocation)
                          ,BuildingTypeXX = "All Sizes"
                          ,Percent = sum(PoolCount) / length(unique(item274.dat2$CK_Cadmus_ID))
                          ,SE = sqrt(Percent * (1 - Percent) / (length(unique(item274.dat2$CK_Cadmus_ID)))))
#across pool location
item274.sum4 <- summarise(group_by(item274.dat3)
                          ,BuildingTypeXX = "All Sizes"
                          ,SITES_Pool_POOL_HOT_TUB_PoolLocation = "All Pools"
                          ,Percent = sum(PoolCount) / length(unique(item274.dat2$CK_Cadmus_ID))
                          ,SE = sqrt(Percent * (1 - Percent) / (length(unique(item274.dat2$CK_Cadmus_ID)))))


item274.merge <- rbind.data.frame(item274.sum1, item274.sum2, item274.sum3, item274.sum4, stringsAsFactors = F)


item274.sampleSize <- summarise(group_by(item274.dat2, BuildingTypeXX)
                                ,SampleSize = length(unique(CK_Cadmus_ID)))

item274.final <- left_join(item274.merge, item274.sampleSize, by = "BuildingTypeXX")
item274.final$SampleSize[which(is.na(item274.final$SampleSize))] <- sum(unique(item274.final$SampleSize[which(item274.final$BuildingTypeXX != "All Sizes")]))

item274.cast <- dcast(setDT(item274.final)
                      ,formula = BuildingTypeXX + SampleSize ~ SITES_Pool_POOL_HOT_TUB_PoolLocation
                      ,value.var = c("Percent", "SE"))

item274.table <- data.frame("Building.Size" = item274.cast$BuildingTypeXX
                            ,"Exterior.Pools" = item274.cast$Percent_Outdoor
                            ,"Exterior.Pools.SE" = item274.cast$SE_Outdoor
                            ,"Interior.Pools" = item274.cast$Percent_Indoor
                            ,"Interior.Pools.SE" = item274.cast$SE_Indoor
                            ,"All.Pools" = item274.cast$`Percent_All Pools`
                            ,"All.Pools.SE" = item274.cast$`SE_All Pools`
                            ,"SampleSize" = item274.cast$SampleSize) 








#############################################################################################
#Item 275: PERCENTAGE OF BUILDINGS WITH POOLS BY POOL TYPE AND BUILDING SIZE (MF Table 66)
#############################################################################################

item275.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"SITES_Pool_POOL_HOT_TUB_PoolType"
                                                                  ,"SITES_Pool_POOL_HOT_TUB_PoolLocation"))]

#remove sites with missing or no elevators
item275.dat0 <- unique(item275.dat[which(!(item275.dat$SITES_Pool_POOL_HOT_TUB_PoolType %in% c(NA))),])

#merge on buildings data with rbsa cleaned data
item275.dat1 <- left_join(item275.dat0, rbsa.dat, by = "CK_Cadmus_ID")
which(duplicated(item272.dat1$CK_Cadmus_ID))

#subset to only multifamily units
item275.dat2 <- item275.dat1[grep("Multifamily",item275.dat1$BuildingType),]

unique(item275.dat2$SITES_Pool_POOL_HOT_TUB_PoolType)

item275.dat2$PoolCount <- 0
item275.dat2$PoolCount[grep("hot tub|Hot tub|Hot Tub|SPA|spa",item275.dat2$SITES_Pool_POOL_HOT_TUB_PoolType)] <- 1
item275.dat2$count <- 1

item275.dat3 <- item275.dat2[which(!(is.na(item275.dat2$SITES_Pool_POOL_HOT_TUB_PoolLocation))),]

#summarise by building size
#by pool location
item275.sum1 <- summarise(group_by(item275.dat3, BuildingTypeXX, SITES_Pool_POOL_HOT_TUB_PoolLocation)
                          ,Percent = sum(PoolCount) / length(unique(item275.dat2$CK_Cadmus_ID))
                          ,SE = sqrt(Percent * (1 - Percent) / (length(unique(item275.dat2$CK_Cadmus_ID)))))
#across pool location
item275.sum2 <- summarise(group_by(item275.dat3, BuildingTypeXX)
                          ,SITES_Pool_POOL_HOT_TUB_PoolLocation = "All Pools"
                          ,Percent = sum(PoolCount) / length(unique(item275.dat2$CK_Cadmus_ID))
                          ,SE = sqrt(Percent * (1 - Percent) / (length(unique(item275.dat2$CK_Cadmus_ID)))))

#summarise across building size
#by pool location
item275.sum3 <- summarise(group_by(item275.dat3, SITES_Pool_POOL_HOT_TUB_PoolLocation)
                          ,BuildingTypeXX = "All Sizes"
                          ,Percent = sum(PoolCount) / length(unique(item275.dat2$CK_Cadmus_ID))
                          ,SE = sqrt(Percent * (1 - Percent) / (length(unique(item275.dat2$CK_Cadmus_ID)))))
#across pool location
item275.sum4 <- summarise(group_by(item275.dat3)
                          ,BuildingTypeXX = "All Sizes"
                          ,SITES_Pool_POOL_HOT_TUB_PoolLocation = "All Pools"
                          ,Percent = sum(PoolCount) / length(unique(item275.dat2$CK_Cadmus_ID))
                          ,SE = sqrt(Percent * (1 - Percent) / (length(unique(item275.dat2$CK_Cadmus_ID)))))


item275.merge <- rbind.data.frame(item275.sum1, item275.sum2, item275.sum3, item275.sum4, stringsAsFactors = F)


item275.sampleSize <- summarise(group_by(item275.dat2, BuildingTypeXX)
                                ,SampleSize = length(unique(CK_Cadmus_ID)))

item275.final <- left_join(item275.merge, item275.sampleSize, by = "BuildingTypeXX")
item275.final$SampleSize[which(is.na(item275.final$SampleSize))] <- sum(unique(item275.final$SampleSize[which(item275.final$BuildingTypeXX != "All Sizes")]))

item275.cast <- dcast(setDT(item275.final)
                      ,formula = BuildingTypeXX + SampleSize ~ SITES_Pool_POOL_HOT_TUB_PoolLocation
                      ,value.var = c("Percent", "SE"))

item275.table <- data.frame("Building.Size" = item275.cast$BuildingTypeXX
                            ,"Exterior.Pools" = item275.cast$Percent_Outdoor
                            ,"Exterior.Pools.SE" = item275.cast$SE_Outdoor
                            ,"Interior.Pools" = item275.cast$Percent_Indoor
                            ,"Interior.Pools.SE" = item275.cast$SE_Indoor
                            ,"All.Pools" = item275.cast$`Percent_All Pools`
                            ,"All.Pools.SE" = item275.cast$`SE_All Pools`
                            ,"SampleSize" = item275.cast$SampleSize) 

