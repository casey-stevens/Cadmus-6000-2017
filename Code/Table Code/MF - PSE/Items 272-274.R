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
rbsa.dat.bldg <- rbsa.dat[grep("bldg", rbsa.dat$CK_Building_ID, ignore.case = T),]
length(unique(rbsa.dat.bldg$CK_Cadmus_ID))
#Read in data for analysis
buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))






#############################################################################################
#Item 272: PERCENTAGE OF BUILDINGS WITH ELEVATORS BY BUILDING SIZE (MF Table 64)
#############################################################################################
item272.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
                                                                  ,"SITES_MFB_MFB_MISC_NumberOfElevators"))]

#merge on buildings data with rbsa cleaned data
item272.dat1 <- left_join(rbsa.dat.bldg,item272.dat)

#subset to only multifamily units
item272.dat2 <- item272.dat1[grep("Multifamily",item272.dat1$BuildingType),]

item272.dat2$Ind <- item272.dat2$SITES_MFB_MFB_MISC_NumberOfElevators
item272.dat2$Ind[which(item272.dat2$SITES_MFB_MFB_MISC_NumberOfElevators > 0)] <- 1
unique(item272.dat2$Ind)

item272.dat3 <- item272.dat2[which(!(is.na(item272.dat2$Ind))),]
item272.dat3$count <- 1

item272.dat4 <- item272.dat3#[which(!(duplicated(item272.dat3$CK_Cadmus_ID))),]
names(item272.dat4)

######################################
#Pop and Sample Sizes for weights
######################################
item272.data <- weightedData(item272.dat4[which(colnames(item272.dat4) %notin% c("SITES_MFB_MFB_MISC_NumberOfElevators"
                                                                                 ,"Ind"
                                                                                 ,"count"
                                                                                 ,"Category"))])

item272.data <- left_join(item272.data, item272.dat4[which(colnames(item272.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"SITES_MFB_MFB_MISC_NumberOfElevators"
                                                                                         ,"Ind"
                                                                                         ,"count"
                                                                                         ,"Category"))])
item272.data$Count <- 1


######################
# weighted analysis
######################
item272.data$State <- item272.data$Category
item272.final <- proportions_one_group(CustomerLevelData = item272.data
                                          ,valueVariable = 'Ind'
                                          ,groupingVariable = 'State'
                                          ,total.name = "Remove"
                                          ,weighted = TRUE)
item272.final <- item272.final[which(item272.final$State != "Total"),]
item272.final <- item272.final[which(names(item272.final) != "BuildingType")]

exportTable(item272.final, "MF", "Table 64", weighted = TRUE,OS = T, osIndicator = "PSE")


######################
# unweighted analysis
######################
item272.final <- proportions_one_group(CustomerLevelData = item272.data
                                          ,valueVariable = 'Ind'
                                          ,groupingVariable = 'State'
                                          ,total.name = "Remove"
                                          ,weighted = FALSE)
item272.final <- item272.final[which(item272.final$State != "Total"),]
item272.final <- item272.final[which(names(item272.final) != "BuildingType")]

exportTable(item272.final, "MF", "Table 64", weighted = FALSE,OS = T, osIndicator = "PSE")




#############################################################################################
#Item 273: AVERAGE NUMBER OF ELEVATORS (IN BUILDINGS WITH ELEVATORS) BY BUILDING SIZE (MF Table 65)
#############################################################################################
item273.data <- item272.data[which(item272.data$SITES_MFB_MFB_MISC_NumberOfElevators > 0),]
names(item272.data)


######################
# weighted analysis
######################
item273.final <- mean_one_group(CustomerLevelData = item273.data
                                ,valueVariable = 'SITES_MFB_MFB_MISC_NumberOfElevators'
                                ,byVariable = 'Category'
                                ,aggregateRow = "Remove")
item273.final <- item273.final[which(item273.final$Category != "Remove"),]
item273.final <- item273.final[which(colnames(item273.final) %notin% c("BuildingType"))]

exportTable(item273.final, "MF","Table 65", weighted = TRUE,OS = T, osIndicator = "PSE")



######################
# unweighted analysis
######################
item273.final <- mean_one_group_unweighted(CustomerLevelData = item273.data
                                ,valueVariable = 'SITES_MFB_MFB_MISC_NumberOfElevators'
                                ,byVariable = 'Category'
                                ,aggregateRow = "Remove")
item273.final <- item273.final[which(item273.final$Category != "Remove"),]
item273.final <- item273.final[which(colnames(item273.final) %notin% c("BuildingType"))]

exportTable(item273.final, "MF","Table 65", weighted = FALSE,OS = T, osIndicator = "PSE")







#############################################################################################
#Item 274: PERCENTAGE OF BUILDINGS WITH POOLS BY POOL TYPE AND BUILDING SIZE (MF Table 66)
#############################################################################################
item274.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
                                                                  ,"SITES_Pool_POOL_HOT_TUB_PoolType"
                                                                  ,"SITES_Pool_POOL_HOT_TUB_PoolLocation"))]

unique(item274.dat$SITES_Pool_POOL_HOT_TUB_PoolType)

item274.dat$Ind <- 0
item274.dat$Ind[grep("pool|unkn",item274.dat$SITES_Pool_POOL_HOT_TUB_PoolType, ignore.case = T)] <- 1
item274.dat$count <- 1

item274.dat0 <- item274.dat

unique(item274.dat0$SITES_Pool_POOL_HOT_TUB_PoolType)
item274.dat1 <- item274.dat0[which(item274.dat0$SITES_Pool_POOL_HOT_TUB_PoolLocation %notin% c("N/A", NA)),]

#merge on buildings data with rbsa cleaned data
item274.dat2 <- left_join(rbsa.dat.bldg, item274.dat1)
# item274.dat2 <- item274.dat2[which(!is.na(item274.dat2$SITES_Pool_POOL_HOT_TUB_PoolLocation)),]

#subset to only multifamily units
item274.dat3 <- item274.dat2[grep("bldg", item274.dat2$CK_Building_ID, ignore.case = T),]

item274.dat4 <- item274.dat3[grep("Multifamily",item274.dat3$BuildingType),]

item274.dat4$Ind[which(is.na(item274.dat4$Ind))] <- 0
# item274.dat4$SITES_Pool_POOL_HOT_TUB_PoolLocation[which(is.na(item274.dat4$SITES_Pool_POOL_HOT_TUB_PoolLocation))] <- "No Pool"
######################################
#Pop and Sample Sizes for weights
######################################
item274.data <- weightedData(item274.dat4[which(colnames(item274.dat4) %notin% c("SITES_Pool_POOL_HOT_TUB_PoolLocation"
                                                                                 ,"SITES_Pool_POOL_HOT_TUB_PoolType"
                                                                                 ,"Ind"
                                                                                 ,"count"
                                                                                 ,"Category"))])

item274.data <- left_join(item274.data, item274.dat4[which(colnames(item274.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"SITES_Pool_POOL_HOT_TUB_PoolLocation"
                                                                                         ,"SITES_Pool_POOL_HOT_TUB_PoolType"
                                                                                         ,"Ind"
                                                                                         ,"count"
                                                                                         ,"Category"))])
item274.data$Count <- 1
names(item274.data)
length(unique(item274.data$CK_Cadmus_ID))
######################
# weighted analysis
######################
item274.summary <- proportionRowsAndColumns1(CustomerLevelData = item274.data
                                             ,valueVariable = 'Count'
                                             ,columnVariable = 'Category'
                                             ,rowVariable = 'SITES_Pool_POOL_HOT_TUB_PoolLocation'
                                             ,aggregateColumnName = "Remove")
item274.summary <- item274.summary[which(item274.summary$Category != "Remove"),]
item274.summary <- item274.summary[which(item274.summary$SITES_Pool_POOL_HOT_TUB_PoolLocation %notin% c("Total",NA)),]
item274.data$State  <- item274.data$Category
item274.all.pools <- proportions_one_group(CustomerLevelData = item274.data
                                              ,valueVariable = 'Ind'
                                              ,groupingVariable = "State"
                                              ,total.name = "All Pools"
                                              ,columnName = "SITES_Pool_POOL_HOT_TUB_PoolLocation"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
item274.all.pools <- item274.all.pools[which(item274.all.pools$State != "Total"),]
names(item274.all.pools)[which(names(item274.all.pools) == "State")] <- "Category"
item274.final <- rbind.data.frame(item274.summary, item274.all.pools, stringsAsFactors = F)

item274.cast <- dcast(setDT(item274.final)
                      ,formula = Category ~ SITES_Pool_POOL_HOT_TUB_PoolLocation
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item274.table <- data.frame("Building.Size"      = item274.cast$Category
                            ,"Exterior.Pools"    = item274.cast$w.percent_Outdoor
                            ,"Exterior.Pools.SE" = item274.cast$w.SE_Outdoor
                            ,"Interior.Pools"    = item274.cast$w.percent_Indoor
                            ,"Interior.Pools.SE" = item274.cast$w.SE_Indoor
                            ,"All.Pools"         = item274.cast$`w.percent_All Pools`
                            ,"All.Pools.SE"      = item274.cast$`w.SE_All Pools`
                            ,"n"                 = item274.cast$`n_All Pools`
                            ,"Exterior.Pools.EB" = item274.cast$EB_Outdoor
                            ,"Interior.Pools.EB" = item274.cast$EB_Indoor
                            ,"All.Pools.EB"      = item274.cast$`EB_All Pools`) 

exportTable(item274.table, "MF", "Table 66", weighted = TRUE,OS = T, osIndicator = "PSE")

######################
# unweighted analysis
######################
item274.summary <- proportions_two_groups_unweighted(CustomerLevelData = item274.data
                                             ,valueVariable = 'Count'
                                             ,columnVariable = 'Category'
                                             ,rowVariable = 'SITES_Pool_POOL_HOT_TUB_PoolLocation'
                                             ,aggregateColumnName = "Remove")
item274.summary <- item274.summary[which(item274.summary$Category != "Remove"),]
item274.summary <- item274.summary[which(item274.summary$SITES_Pool_POOL_HOT_TUB_PoolLocation != "Total"),]

item274.all.pools <- proportions_one_group(CustomerLevelData = item274.data
                                              ,valueVariable = 'Ind'
                                              ,groupingVariable = "State"
                                              ,total.name = "All Pools"
                                              ,columnName = "SITES_Pool_POOL_HOT_TUB_PoolLocation"
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
item274.all.pools <- item274.all.pools[which(item274.all.pools$State != "Total"),]
names(item274.all.pools)[which(names(item274.all.pools) == "State")] <- "Category"

item274.final <- rbind.data.frame(item274.summary, item274.all.pools, stringsAsFactors = F)

item274.cast <- dcast(setDT(item274.final)
                      ,formula = Category ~ SITES_Pool_POOL_HOT_TUB_PoolLocation
                      ,value.var = c("Percent", "SE", "Count", "n"))

item274.table <- data.frame("Building.Size"      = item274.cast$Category
                            ,"Exterior.Pools"    = item274.cast$Percent_Outdoor
                            ,"Exterior.Pools.SE" = item274.cast$SE_Outdoor
                            ,"Interior.Pools"    = item274.cast$Percent_Indoor
                            ,"Interior.Pools.SE" = item274.cast$SE_Indoor
                            ,"All.Pools"         = item274.cast$`Percent_All Pools`
                            ,"All.Pools.SE"      = item274.cast$`SE_All Pools`
                            ,"n"                 = item274.cast$`n_All Pools`) 

exportTable(item274.table, "MF", "Table 66", weighted = FALSE,OS = T, osIndicator = "PSE")







# #############################################################################################
# #Item 275: PERCENTAGE OF BUILDINGS WITH POOLS BY POOL TYPE AND BUILDING SIZE (MF Table 66)
# #############################################################################################
# 
# item275.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Cadmus_ID"
#                                                                   ,"SITES_Pool_POOL_HOT_TUB_PoolType"
#                                                                   ,"SITES_Pool_POOL_HOT_TUB_PoolLocation"))]
# 
# #remove sites with missing or no elevators
# item275.dat0 <- unique(item275.dat[which(!(item275.dat$SITES_Pool_POOL_HOT_TUB_PoolType %in% c(NA))),])
# 
# #merge on buildings data with rbsa cleaned data
# item275.dat1 <- left_join(item275.dat0, rbsa.dat.bldg, by = "CK_Cadmus_ID")
# which(duplicated(item272.dat1$CK_Cadmus_ID))
# 
# #subset to only multifamily units
# item275.dat2 <- item275.dat1[grep("Multifamily",item275.dat1$BuildingType),]
# 
# unique(item275.dat2$SITES_Pool_POOL_HOT_TUB_PoolType)
# 
# item275.dat2$PoolCount <- 0
# item275.dat2$PoolCount[grep("hot tub|Hot tub|Hot Tub|SPA|spa",item275.dat2$SITES_Pool_POOL_HOT_TUB_PoolType)] <- 1
# item275.dat2$count <- 1
# 
# item275.dat3 <- item275.dat2[which(!(is.na(item275.dat2$SITES_Pool_POOL_HOT_TUB_PoolLocation))),]
# 
# #summarise by building size
# #by pool location
# item275.sum1 <- summarise(group_by(item275.dat3, BuildingTypeXX, SITES_Pool_POOL_HOT_TUB_PoolLocation)
#                           ,Percent = sum(PoolCount) / length(unique(item275.dat2$CK_Cadmus_ID))
#                           ,SE = sqrt(Percent * (1 - Percent) / (length(unique(item275.dat2$CK_Cadmus_ID)))))
# #across pool location
# item275.sum2 <- summarise(group_by(item275.dat3, BuildingTypeXX)
#                           ,SITES_Pool_POOL_HOT_TUB_PoolLocation = "All Pools"
#                           ,Percent = sum(PoolCount) / length(unique(item275.dat2$CK_Cadmus_ID))
#                           ,SE = sqrt(Percent * (1 - Percent) / (length(unique(item275.dat2$CK_Cadmus_ID)))))
# 
# #summarise across building size
# #by pool location
# item275.sum3 <- summarise(group_by(item275.dat3, SITES_Pool_POOL_HOT_TUB_PoolLocation)
#                           ,BuildingTypeXX = "Remove"
#                           ,Percent = sum(PoolCount) / length(unique(item275.dat2$CK_Cadmus_ID))
#                           ,SE = sqrt(Percent * (1 - Percent) / (length(unique(item275.dat2$CK_Cadmus_ID)))))
# #across pool location
# item275.sum4 <- summarise(group_by(item275.dat3)
#                           ,BuildingTypeXX = "Remove"
#                           ,SITES_Pool_POOL_HOT_TUB_PoolLocation = "All Pools"
#                           ,Percent = sum(PoolCount) / length(unique(item275.dat2$CK_Cadmus_ID))
#                           ,SE = sqrt(Percent * (1 - Percent) / (length(unique(item275.dat2$CK_Cadmus_ID)))))
# 
# 
# item275.merge <- rbind.data.frame(item275.sum1, item275.sum2, item275.sum3, item275.sum4, stringsAsFactors = F)
# 
# 
# item275.n <- summarise(group_by(item275.dat2, BuildingTypeXX)
#                                 ,n = length(unique(CK_Cadmus_ID)))
# 
# item275.final <- left_join(item275.merge, item275.n, by = "BuildingTypeXX")
# item275.final$n[which(is.na(item275.final$n))] <- sum(unique(item275.final$n[which(item275.final$BuildingTypeXX != "Remove")]))
# 
# item275.cast <- dcast(setDT(item275.final)
#                       ,formula = BuildingTypeXX + n ~ SITES_Pool_POOL_HOT_TUB_PoolLocation
#                       ,value.var = c("Percent", "SE"))
# 
# item275.table <- data.frame("Building.Size"      = item275.cast$BuildingTypeXX
#                             ,"Exterior.Pools"    = item275.cast$Percent_Outdoor
#                             ,"Exterior.Pools.SE" = item275.cast$SE_Outdoor
#                             ,"Interior.Pools"    = item275.cast$Percent_Indoor
#                             ,"Interior.Pools.SE" = item275.cast$SE_Indoor
#                             ,"All.Pools"         = item275.cast$`Percent_All Pools`
#                             ,"All.Pools.SE"      = item275.cast$`SE_All Pools`
#                             ,"n"                 = item275.cast$n) 
# 
