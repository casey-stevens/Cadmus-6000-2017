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

#Read in data for analysis
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))

#subset to columns provided by Rietz -- this includes columns for both items 300 and 301
appliances.dat1 <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"CK_SiteID"
                                                                        ,"Type"
                                                                        ,"Stove.Fuel"
                                                                        ,"Oven.Fuel"
                                                                        ,"TV.Set.Top.Box"
                                                                        ,"STB.Records?"
                                                                        ,"Contains.Suboofer"
                                                                        ,"Does.Subwoofer.have.indicator.light.or.warm.to.touch"
                                                                        ,""))]
#powered means Does.Subwoofer.have.indicator.light.or.warm.to.touch == Yes
#Read in data for analysis
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))
#subset to columns provided by Rietz -- this includes columns for both items 300 and 301
sites.interview.dat1 <- sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                       ,"INTRVW_CUST_RES_HomeandEnergyUseHome_DishwasherLoadsPerWeek"
                                                                                       ,"INTRVW_CUST_RES_HomeandEnergyUseHomeUse_HowManyHoursPerDayIsThePrimaryTVOn"
                                                                                       ,""))]
#remove any duplicated rows and cases where both interview quantities are NA
sites.interview.dat2 <- unique(sites.interview.dat1[which(!(is.na(sites.interview.dat1$INTRVW_CUST_RES_HomeandEnergyUseHome_DishwasherLoadsPerWeek) & 
                                                              is.na(sites.interview.dat1$INTRVW_CUST_RES_HomeandEnergyUseHomeUse_HowManyHoursPerDayIsThePrimaryTVOn))),])
#verify no duplicated rows
which(duplicated(sites.interview.dat2$CK_Cadmus_ID))

#merge rbsa cleaned data with sites interview data
rbsa.sites.int <- left_join(rbsa.dat, sites.interview.dat2, by = "CK_Cadmus_ID")
#subset to only MF units
rbsa.sites.int1 <- rbsa.sites.int[grep("Multifamily",rbsa.sites.int$BuildingType),]


#merge rbsa cleaned data with appliances data
rbsa.appliances <- left_join(rbsa.dat, appliances.dat1, by = "CK_Cadmus_ID")
#subset to only MF units
rbsa.appliances1 <- rbsa.appliances[grep("Multifamily",rbsa.appliances$BuildingType),]
#remove any BLDG info
rbsa.appliances2 <- rbsa.appliances1[-grep("BLDG", rbsa.appliances1$CK_Building_ID),]




###################################################################################################################
# ITEM 300: IN-UNIT KITCHEN APPLIANCE CHARACTERISTICS (MF Table 94)
###################################################################################################################
################################################
# For Dishwasher loads
################################################
item300.sites <- rbsa.sites.int1
#remove any NAs from dishwasher loads per week for this table
item300.sites1 <- item300.sites[which(!is.na(item300.sites$INTRVW_CUST_RES_HomeandEnergyUseHome_DishwasherLoadsPerWeek)),]
################################################
# Adding pop and sample sizes for weights
################################################
item300.sites.data <- weightedData(item300.sites2[-which(colnames(item300.sites2) %in% c("INTRVW_CUST_RES_HomeandEnergyUseHome_DishwasherLoadsPerWeek"               
                                                                                         ,"INTRVW_CUST_RES_HomeandEnergyUseHomeUse_HowManyHoursPerDayIsThePrimaryTVOn"))])
item300.sites.data <- left_join(item300.sites.data, item300.sites2[which(colnames(item300.sites2) %in% c("CK_Cadmus_ID"
                                                                                                       ,"INTRVW_CUST_RES_HomeandEnergyUseHome_DishwasherLoadsPerWeek"               
                                                                                                       ,"INTRVW_CUST_RES_HomeandEnergyUseHomeUse_HowManyHoursPerDayIsThePrimaryTVOn"))])
names(item300.sites.data)[which(names(item300.sites.data) == "INTRVW_CUST_RES_HomeandEnergyUseHome_DishwasherLoadsPerWeek")] <- "Dishwasher.Loads.per.Week"


################################################
# For Fuel Distribution
################################################
item300.appliances <- rbsa.appliances1

#subset to only electric and gas fuel types for stove(cooktop) and add a Count and Total Count
item300.appliances.stove <- item300.appliances[which(item300.appliances$Stove.Fuel %in% c("Electric", "Gas")),]
item300.appliances.stove <- item300.appliances.stove[grep("site", item300.appliances.stove$CK_Building_ID, ignore.case = T),]

#subset to only electric and gas fuel types for oven and add a Count and Total Count
item300.appliances.oven <- item300.appliances[which(item300.appliances$Oven.Fuel  %in% c("Electric", "Gas")),]
item300.appliances.oven <- item300.appliances.oven[grep("site", item300.appliances.oven$CK_Building_ID, ignore.case = T),]

################################################
# Adding pop and sample sizes for weights
################################################
item300.stove.data <- weightedData(item300.appliances.stove[-which(colnames(item300.appliances.stove) %in% c("CK_SiteID"
                                                                                                             ,"Type"
                                                                                                             ,"Stove.Fuel"
                                                                                                             ,"Oven.Fuel"
                                                                                                             ,"TV.Set.Top.Box"
                                                                                                             ,"STB.Records?"
                                                                                                             ,"Contains.Suboofer"
                                                                                                             ,"Does.Subwoofer.have.indicator.light.or.warm.to.touch"))])
item300.stove.data <- left_join(item300.stove.data, item300.appliances.stove[which(colnames(item300.appliances.stove) %in% c("CK_Cadmus_ID"
                                                                                                                             ,"CK_SiteID"
                                                                                                                             ,"Type"
                                                                                                                             ,"Stove.Fuel"
                                                                                                                             ,"Oven.Fuel"))])
item300.stove.data$count <- 1

item300.oven.data <- weightedData(item300.appliances.oven[-which(colnames(item300.appliances.oven) %in% c("CK_SiteID"
                                                                                                             ,"Type"
                                                                                                             ,"Stove.Fuel"
                                                                                                             ,"Oven.Fuel"
                                                                                                             ,"TV.Set.Top.Box"
                                                                                                             ,"STB.Records?"
                                                                                                             ,"Contains.Suboofer"
                                                                                                             ,"Does.Subwoofer.have.indicator.light.or.warm.to.touch"))])
item300.oven.data <- left_join(item300.oven.data, item300.appliances.oven[which(colnames(item300.appliances.oven) %in% c("CK_Cadmus_ID"
                                                                                                                             ,"CK_SiteID"
                                                                                                                             ,"Type"
                                                                                                                             ,"Stove.Fuel"
                                                                                                                             ,"Oven.Fuel"))])
item300.oven.data$count <- 1

################################################################################################################
# weighted
################################################################################################################
#########################################
# For Dishwasher loads per week
# Calculate the average 
#  across all MF units
#########################################
item300.sum1.w <- mean_one_group(CustomerLevelData = item300.sites.data
                               ,valueVariable      = 'Dishwasher.Loads.per.Week'
                               ,byVariable         = "BuildingType"
                               ,aggregateRow       = "Remove")
names(item300.sum1.w) <- c("Category", "BuildingType", "Mean", "SE", "Remove", "n", "N")
item300.sum1.w <- item300.sum1.w[which(colnames(item300.sum1.w) != "Remove")]



#########################################
# For cook top fuel
# Calculate the distribution between
#  electric and gas fuel types
#  across all MF units
#   Note: in this table, Mean is Percent
#########################################
item300.sum2.w <- proportions_one_group(CustomerLevelData = item300.stove.data
                                        ,valueVariable = 'count'
                                        ,groupingVariable = "Stove.Fuel"
                                        ,total.name = "Remove")
item300.sum2.w <- item300.sum2.w[which(item300.sum2.w$Stove.Fuel != "Total"),]
item300.sum2.w$Stove.Fuel <- paste("Cook Top Fuel: ", item300.sum2.w$Stove.Fuel, sep = "")
names(item300.sum2.w) <- c("BuildingType", "Category", "Mean", "SE", "Remove", "N", "n")
item300.sum2.w <- item300.sum2.w[which(colnames(item300.sum2.w) != "Remove")]


#########################################
# For oven fuel
# Calculate the distribution between
#  electric and gas fuel types
#  across all MF units
#   Note: in this table, Mean is Percent
#########################################
item300.sum3.w <- proportions_one_group(CustomerLevelData = item300.oven.data
                                        ,valueVariable = 'count'
                                        ,groupingVariable = "Oven.Fuel"
                                        ,total.name = "Remove")
item300.sum3.w <- item300.sum3.w[which(item300.sum3.w$Oven.Fuel != "Total"),]
item300.sum3.w$Oven.Fuel <- paste("Oven Fuel: ", item300.sum3.w$Oven.Fuel, sep = "")
names(item300.sum3.w) <- c("BuildingType", "Category", "Mean", "SE", "Remove", "N", "n")
item300.sum3.w <- item300.sum3.w[which(colnames(item300.sum3.w) != "Remove")]


#########################################
# Combine all three summaries
#  to get final table
#########################################
item300.final <- rbind.data.frame(item300.sum1.w, item300.sum2.w, item300.sum3.w, stringsAsFactors = F)
exportTable(item300.final, "MF", "Table 94", weighted = TRUE)



################################################################################################################
# Unweighted
################################################################################################################
#########################################
# For Dishwasher loads per week
# Calculate the average 
#  across all MF units
#########################################
item300.sum1 <- mean_one_group_unweighted(CustomerLevelData = item300.sites.data
                                 ,valueVariable      = 'Dishwasher.Loads.per.Week'
                                 ,byVariable         = "BuildingType"
                                 ,aggregateRow       = "Remove")

#########################################
# For cook top fuel
# Calculate the distribution between
#  electric and gas fuel types
#  across all MF units
#   Note: in this table, Mean is Percent
#########################################
item300.sum2 <- proportions_one_group(CustomerLevelData = item300.stove.data
                                        ,valueVariable = 'count'
                                        ,groupingVariable = "Stove.Fuel"
                                        ,total.name = "Remove"
                                      ,weighted = FALSE)
item300.sum2 <- item300.sum2[which(item300.sum2$Stove.Fuel != "Total"),]
item300.sum2$Stove.Fuel <- paste("Cook Top Fuel: ", item300.sum2$Stove.Fuel, sep = "")
names(item300.sum2) <- c("BuildingType", "Category",  "n", "Remove", "Mean", "SE")
item300.sum2 <- item300.sum2[which(colnames(item300.sum2) != "Remove")]


#########################################
# For oven fuel
# Calculate the distribution between
#  electric and gas fuel types
#  across all MF units
#   Note: in this table, Mean is Percent
#########################################
item300.sum3 <- proportions_one_group(CustomerLevelData = item300.oven.data
                                        ,valueVariable = 'count'
                                        ,groupingVariable = "Oven.Fuel"
                                        ,total.name = "Remove")
item300.sum3 <- item300.sum3[which(item300.sum3$Oven.Fuel != "Total"),]
item300.sum3$Oven.Fuel <- paste("Oven Fuel: ", item300.sum3$Oven.Fuel, sep = "")
names(item300.sum3) <- c("BuildingType", "Category",  "n", "Remove", "Mean", "SE")
item300.sum3 <- item300.sum3[which(colnames(item300.sum3) != "Remove")]


#########################################
# Combine all three summaries
#  to get final table
#########################################
item300.final <- rbind.data.frame(item300.sum1, item300.sum2, item300.sum3, stringsAsFactors = F)
exportTable(item300.final, "MF", "Table 94", weighted = FALSE)
















###################################################################################################################
# ITEM 301: IN-UNIT ELECTRONICS CHARACTERISTICS (MF Table 95)
###################################################################################################################
item301.sites <- rbsa.sites.int1
#remove any NAs from dishwasher loads per week for this table
item301.sites1 <- item301.sites[which(!(is.na(item301.sites$INTRVW_CUST_RES_HomeandEnergyUseHomeUse_HowManyHoursPerDayIsThePrimaryTVOn))),]
names(item301.sites1)[which(names(item301.sites1) == "INTRVW_CUST_RES_HomeandEnergyUseHomeUse_HowManyHoursPerDayIsThePrimaryTVOn")] <- "TV.On.Hours"


################################################
# Adding pop and sample sizes for weights
################################################
item301.sites.data <- weightedData(item301.sites1[-which(colnames(item301.sites1) %in% c("INTRVW_CUST_RES_HomeandEnergyUseHome_DishwasherLoadsPerWeek"               
                                                                                         ,"TV.On.Hours"))])
item301.sites.data <- left_join(item301.sites.data, item301.sites1[which(colnames(item301.sites1) %in% c("CK_Cadmus_ID"
                                                                                                         ,"INTRVW_CUST_RES_HomeandEnergyUseHome_DishwasherLoadsPerWeek"               
                                                                                                         ,"TV.On.Hours"))])


################################################
# For Fuel Distribution
################################################
item301.appliances  <- rbsa.appliances1[grep("site", rbsa.appliances1$CK_Building_ID, ignore.case = T),]
item301.appliances1 <- item301.appliances[grep("multifamily", item301.appliances$BuildingType, ignore.case = T),]

################################################
# Adding pop and sample sizes for weights
################################################
item301.appliances.data <- weightedData(item301.appliances1[-which(colnames(item301.appliances1) %in% c("CK_SiteID"
                                                                                                        ,"Type"
                                                                                                        ,"Stove.Fuel"
                                                                                                        ,"Oven.Fuel"
                                                                                                        ,"TV.Set.Top.Box"
                                                                                                        ,"STB.Records?"
                                                                                                        ,"Contains.Suboofer"
                                                                                                        ,"Does.Subwoofer.have.indicator.light.or.warm.to.touch"))])
item301.appliances.data <- left_join(item301.appliances.data, item301.appliances1[which(colnames(item301.appliances1) %in% c("CK_Cadmus_ID"
                                                                                                                             ,"CK_SiteID"
                                                                                                                             ,"Type"
                                                                                                                             ,"Stove.Fuel"
                                                                                                                             ,"Oven.Fuel"
                                                                                                                             ,"TV.Set.Top.Box"
                                                                                                                             ,"STB.Records?"
                                                                                                                             ,"Contains.Suboofer"
                                                                                                                             ,"Does.Subwoofer.have.indicator.light.or.warm.to.touch"))])
item301.appliances.data$count <- 1


#############################################################
# For Televisions per unit
#  across all MF units
#############################################################
#subset to only TVs
item301.tv <- item301.appliances.data[which(item301.appliances.data$Type == "Television"),]
#merge onto RBSA cleaned data
# item301.tv1 <- left_join(rbsa.dat, item301.tv)
#subset to only MF
item301.tv2 <- item301.tv[grep("Multifamily",item301.tv$BuildingType),]
#remove BLDG info
item301.tv3 <- item301.tv2[grep("site", item301.tv2$CK_Building_ID, ignore.case = T),]

#add count of TV, where if NA, count is 0
item301.tv3$Ind <- 0
item301.tv3$Ind[which(item301.tv3$Type == "Television")] <- 1


#summarise to count total televisions per unit
item301.tv.sum <- summarise(group_by(item301.tv3, CK_Cadmus_ID)
                            ,Televisions.per.Unit = sum(Ind))

item301.merge <- left_join(rbsa.dat, item301.tv.sum)
item301.merge <- item301.merge[which(item301.merge$BuildingType == "Multifamily"),]
item301.merge <- item301.merge[grep("site", item301.merge$CK_Building_ID, ignore.case = T),]
################################################
# Adding pop and sample sizes for weights
################################################
item301.appliances.data <- weightedData(item301.merge[-which(colnames(item301.merge) %in% c("Televisions.per.Unit"))])
item301.appliances.data <- left_join(item301.appliances.data, item301.merge[which(colnames(item301.merge) %in% c("CK_Cadmus_ID"
                                                                                                                 ,"Televisions.per.Unit"))])
item301.appliances.data$count <- 1

################################################
# weighted analysis - TVs per unit
################################################
item301.tv.weighted <- mean_one_group(CustomerLevelData = item301.appliances.data
                                      ,valueVariable    = 'Televisions.per.Unit'
                                      ,byVariable       = 'BuildingType'
                                      ,aggregateRow     = "Remove")
item301.tv.weighted <- item301.tv.weighted[which(names(item301.tv.weighted) != "n")]



###############################################################################
# Weighted analysis - Primary Television On-Time Hours Per Day Per Unit 
###############################################################################
item301.tvHours.weighted <- mean_one_group(CustomerLevelData = item301.sites.data
                                           ,valueVariable = 'TV.On.Hours'
                                           ,byVariable = 'BuildingType'
                                           ,aggregateRow = "Remove")
item301.tvHours.weighted <- item301.tvHours.weighted[which(names(item301.tvHours.weighted) != "n")]



#############################################################
# Weighted analysis - Set-Top Boxes Per Unit
#############################################################

item301.stb <- item301.tv3
item301.stb$Ind <- 0
item301.stb$Ind[which(item301.stb$TV.Set.Top.Box == "Yes")] <- 1


#summarise to count total televisions per unit
item301.stb.sum <- summarise(group_by(item301.stb, CK_Cadmus_ID)
                            ,stb.Count = sum(Ind))
summary(item301.stb.sum$stb.Count)

item301.merge <- left_join(rbsa.dat, item301.stb.sum)
item301.merge <- item301.merge[which(item301.merge$BuildingType == "Multifamily"),]
item301.merge <- item301.merge[grep("site", item301.merge$CK_Building_ID, ignore.case = T),]
################################################
# Adding pop and sample sizes for weights
################################################
item301.appliances.data <- weightedData(item301.merge[-which(colnames(item301.merge) %in% c("stb.Count"))])
item301.appliances.data <- left_join(item301.appliances.data, item301.merge[which(colnames(item301.merge) %in% c("CK_Cadmus_ID"
                                                                                                                 ,"stb.Count"))])
item301.appliances.data$count <- 1

################################################
# weighted analysis - STBs per unit
################################################
item301.STB.weighted <- mean_one_group(CustomerLevelData = item301.appliances.data
                                      ,valueVariable    = 'stb.Count'
                                      ,byVariable       = 'BuildingType'
                                      ,aggregateRow     = "Remove")
item301.STB.weighted <- item301.STB.weighted[which(names(item301.STB.weighted) != "n")]


#############################################################
# weighted analysis - For Units With Set-Top Boxes 
#############################################################
item301.appliances.data$count <- 1
item301.appliances.data$Ind <- 0
item301.appliances.data$Ind[which(item301.appliances.data$stb.Count > 0)] <- 1
item301.stb.percent.weighted <- proportions_one_group(CustomerLevelData = item301.appliances.data
                                                      ,valueVariable    = 'Ind'
                                                      ,groupingVariable = 'BuildingType'
                                                      ,total.name       = "Remove")


#############################################################
# weighted analysis - For Set-Top Boxes With DVR Capability
#############################################################
item301.stb.dvr <- item301.stb[which(item301.stb$TV.Set.Top.Box == "Yes"),]

################################################
# Adding pop and sample sizes for weights
################################################
item301.appliances.data <- weightedData(item301.stb.dvr[-which(colnames(item301.stb.dvr) %in% c("CK_SiteID"
                                                                                                ,"Type"
                                                                                                ,"Stove.Fuel"
                                                                                                ,"Oven.Fuel"
                                                                                                ,"TV.Set.Top.Box"
                                                                                                ,"STB.Records?"
                                                                                                ,"Contains.Suboofer"
                                                                                                ,"Does.Subwoofer.have.indicator.light.or.warm.to.touch"
                                                                                                ,"count"
                                                                                                ,"Ind"))])
item301.appliances.data <- left_join(item301.appliances.data, item301.appliances1[which(colnames(item301.appliances1) %in% c("CK_Cadmus_ID"
                                                                                                                             ,"CK_SiteID"
                                                                                                                             ,"Type"
                                                                                                                             ,"Stove.Fuel"
                                                                                                                             ,"Oven.Fuel"
                                                                                                                             ,"TV.Set.Top.Box"
                                                                                                                             ,"STB.Records?"
                                                                                                                             ,"Contains.Suboofer"
                                                                                                                             ,"Does.Subwoofer.have.indicator.light.or.warm.to.touch"
                                                                                                                             ,"count"
                                                                                                                             ,"Ind"))])
# add count
item301.appliances.data$count <- 1
item301.appliances.data$Ind <- 0
item301.appliances.data$Ind[which(item301.appliances.data$`STB.Records?` == "Yes")] <- 1

item301.stb.dvr.weighted <- mean_one_group(CustomerLevelData = item301.appliances.data
                                      ,valueVariable = 'Ind'
                                      ,byVariable = "BuildingType"
                                      ,aggregateRow = "Remove")


#############################################################
# For Units With Gaming Systems
#  across all MF units
#   Note: Mean in this summary is actually a percent
#############################################################
#subset to only game consoles
item301.game <- item301.appliances[which(item301.appliances$Type == "Game Console"),]
#merge onto RBSA cleaned data
item301.game1 <- unique(left_join(rbsa.dat, item301.game, by = "CK_Cadmus_ID"))
#subset to only MF
item301.game2 <- item301.game1[grep("Multifamily",item301.game1$BuildingType),]

#add count of TV, where if NA, count is 0
item301.game2$game.count <- 0
item301.game2$game.count[which(item301.game2$Type == "Game Console")] <- 1


#summarise to count total televisions per unit
item301.game.sum <- summarise(group_by(item301.game2, CK_Cadmus_ID)
                            ,game.count = sum(game.count))
item301.game.sum$game.count[which(item301.game.sum$game.count > 0)] <- 1
item301.game.sum$count <- 1


#summarise to calculate average number of televisions per home
item301.game.sum1 <- summarise(item301.game.sum
                             ,Category = "Units With Gaming Systems"
                             ,Mean = sum(game.count) / sum(count)
                             ,SE = sqrt(Mean * (1 - Mean) / length(unique(CK_Cadmus_ID)))
                             ,SampleSize = length(unique(CK_Cadmus_ID)))


#############################################################
# For Gaming Systems Per Unit With Gaming Systems
#  across all MF units
#############################################################
#subset to only game consoles
item301.unit.game <- item301.game2

#summarise to count total televisions per unit
item301.unit.game.sum <- summarise(group_by(item301.unit.game, CK_Cadmus_ID)
                              ,game.count = sum(game.count))
unique(item301.unit.game.sum$game.count)

item301.unit.game1 <- item301.unit.game.sum[which(item301.unit.game.sum$game.count != 0),]

item301.unit.game.sum1 <- summarise(item301.unit.game1
                                     ,Category = "Gaming Systems Per Unit With Gaming Systems"
                                     ,Mean = mean(game.count)
                                     ,SE = sd(game.count) / sqrt(length(unique(CK_Cadmus_ID)))
                                     ,SampleSize = length(unique(CK_Cadmus_ID)))







#############################################################
# For Computers per unit and Units With Computers
#  across all MF units
#   Note: Mean in Units With Computers is actually a percent
#############################################################
#subset to only comp consoles
item301.comp <- item301.appliances[which(item301.appliances$Type %in% c("Desktop", "Laptop")),]
#merge onto RBSA cleaned data
item301.comp1 <- unique(left_join(rbsa.dat, item301.comp, by = "CK_Cadmus_ID"))
#subset to only MF
item301.comp2 <- item301.comp1[grep("Multifamily",item301.comp1$BuildingType),]

#add count of TV, where if NA, count is 0
item301.comp2$comp.count <- 0
item301.comp2$comp.count[which(item301.comp2$Type %in% c("Desktop", "Laptop"))] <- 1

##################For Computers per unit
#summarise to count total computers per unit
item301.comp.sum <- summarise(group_by(item301.comp2, CK_Cadmus_ID)
                              ,comp.count = sum(comp.count))

item301.comp.per.unit <- summarise(item301.comp.sum
                                   ,Category = "Computers Per Unit"
                                   ,Mean = mean(comp.count)
                                   ,SE = sd(comp.count) / length(unique(CK_Cadmus_ID))
                                   ,SampleSize = length(unique(CK_Cadmus_ID)))


##################For Units With Computers
item301.comp.sum$comp.count[which(item301.comp.sum$comp.count > 0)] <- 1
item301.comp.sum$count <- 1


#summarise to calculate average number of computers per home
item301.units.comp <- summarise(item301.comp.sum
                               ,Category = "Units With Computers"
                               ,Mean = sum(comp.count) / sum(count)
                               ,SE = sqrt(Mean * (1 - Mean) / length(unique(CK_Cadmus_ID)))
                               ,SampleSize = length(unique(CK_Cadmus_ID)))




#############################################################
# For Audio Systems Per Unit
#  across all MF units
#############################################################
#subset to only audio systems
item301.audio <- item301.appliances[which(item301.appliances$Type %in% c("Audio Equipment")),]
#merge onto RBSA cleaned data
item301.audio1 <- unique(left_join(rbsa.dat, item301.audio, by = "CK_Cadmus_ID"))
#subset to only MF
item301.audio2 <- item301.audio1[grep("Multifamily",item301.audio1$BuildingType),]

#add count of audio systems, where if NA, count is 0
item301.audio2$audio.count <- 0
item301.audio2$audio.count[which(item301.audio2$Type %in% c("Audio Equipment"))] <- 1

##################For audio systems per unit
#summarise to count total audio systems per unit
item301.audio.sum <- summarise(group_by(item301.audio2, CK_Cadmus_ID)
                              ,audio.count = sum(audio.count))

item301.audio.per.unit <- summarise(item301.audio.sum
                                   ,Category = "Audio Systems Per Unit"
                                   ,Mean = mean(audio.count)
                                   ,SE = sd(audio.count) / length(unique(CK_Cadmus_ID))
                                   ,SampleSize = length(unique(CK_Cadmus_ID)))



#############################################################
# For subwoofers (passive, powered, total)
#  across all MF units
#############################################################
# ,"Contains.Suboofer"
# ,"Does.Subwoofer.have.indicator.light.or.warm.to.touch"
item301.subwoofer <- item301.audio2

item301.subwoofer$passive.sub.count <- 0
item301.subwoofer$passive.sub.count[which(item301.subwoofer$Contains.Suboofer == "Yes" & item301.subwoofer$Does.Subwoofer.have.indicator.light.or.warm.to.touch == "No")] <- 1

item301.subwoofer$powered.sub.count <- 0
item301.subwoofer$powered.sub.count[which(item301.subwoofer$Contains.Suboofer == "Yes" & item301.subwoofer$Does.Subwoofer.have.indicator.light.or.warm.to.touch == "Yes")] <- 1

item301.subwoofer$total.sub.count <- 0
item301.subwoofer$total.sub.count[which(item301.subwoofer$Contains.Suboofer == "Yes" & item301.subwoofer$Does.Subwoofer.have.indicator.light.or.warm.to.touch %in% c("Yes","No"))] <- 1


#summarise to count total subwoofers per unit
item301.sub.sum <- summarise(group_by(item301.subwoofer, CK_Cadmus_ID)
                             ,total.sub.count = sum(total.sub.count)
                             ,passive.sub.count = sum(passive.sub.count)
                             ,powered.sub.count = sum(powered.sub.count))


item301.passive.subwoofer <- summarise(item301.sub.sum
                                     ,Category = "Passive Subwoofers Per Unit"
                                     ,Mean = mean(passive.sub.count)
                                     ,SE = sd(passive.sub.count) / length(unique(CK_Cadmus_ID))
                                     ,SampleSize = length(unique(CK_Cadmus_ID)))

item301.powered.subwoofer <- summarise(item301.sub.sum
                                     ,Category = "Powered Subwoofers Per Unit"
                                     ,Mean = mean(powered.sub.count)
                                     ,SE = sd(powered.sub.count) / length(unique(CK_Cadmus_ID))
                                     ,SampleSize = length(unique(CK_Cadmus_ID)))


item301.total.subwoofer <- summarise(item301.sub.sum
                                    ,Category = "Total Subwoofers Per Unit"
                                    ,Mean = mean(total.sub.count)
                                    ,SE = sd(total.sub.count) / length(unique(CK_Cadmus_ID))
                                    ,SampleSize = length(unique(CK_Cadmus_ID)))







#############################################################
# Combined all summaries
#############################################################
item301.final <- rbind.data.frame(item301.tv.sum1
                                  ,item301.tvHours.sum
                                  ,item301.stb.sum1
                                  ,item301.stb.percent.sum
                                  ,item301.stb.dvr.sum
                                  ,item301.game.sum1
                                  ,item301.unit.game.sum1
                                  ,item301.comp.per.unit
                                  ,item301.units.comp
                                  ,item301.passive.subwoofer
                                  ,item301.powered.subwoofer
                                  ,item301.total.subwoofer
                                  )
