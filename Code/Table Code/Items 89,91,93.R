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

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))


#############################################################################################
#Item 89: AVERAGE NUMBER OF CLOTHES WASHER LOADS PER WEEK BY STATE (SF table 96, MH table 77)
#############################################################################################
#subset to columns needed for analysis
item89.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                             ,"INTRVW_CUST_RES_HomeandEnergyUseHome_ClothesWasherLoadsPerWeek"
                                                                             ,""))])
colnames(item89.dat) <- c("CK_Cadmus_ID", "Clothes.Washes.Per.Week")
item89.dat$count <- 1

item89.dat0 <- item89.dat[which(item89.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item89.dat1 <- left_join(rbsa.dat, item89.dat0, by = "CK_Cadmus_ID")

item89.dat2 <- item89.dat1[which(item89.dat1$Clothes.Washes.Per.Week != "No Washing Machine"),]
item89.dat2$Clothes.Washes.Per.Week <- as.numeric(as.character(item89.dat2$Clothes.Washes.Per.Week))


# Weighting
item89.data <- weightedData(item89.dat2[-which(colnames(item89.dat2) %in% c("Clothes.Washes.Per.Week"
                                                                            ,"count"))])

item89.data <- left_join(item89.data, item89.dat2[which(colnames(item89.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"Clothes.Washes.Per.Week"
                                                                                     ,"count"))])

###############################
# Weighted Analysis
###############################

item89.final <- mean_one_group(CustomerLevelData = item89.data
                               ,valueVariable = 'Clothes.Washes.Per.Week' 
                               ,byVariable    = 'State'
                               ,aggregateRow  = "Region")

# Export table
item89.final.SF <- item89.final[which(item89.final$BuildingType == "Single Family"),-1]
item89.final.MH <- item89.final[which(item89.final$BuildingType == "Manufactured"),-1]

exportTable(item89.final.SF, "SF", "Table 96", weighted = TRUE)
exportTable(item89.final.MH, "MH", "Table 77", weighted = TRUE)


## OLD CODE ##
# 
# #summarise by states
# item89.sum <- summarise(group_by(item89.dat2, BuildingType, State)
#                         ,SampleSize = length(unique(CK_Cadmus_ID))
#                         ,Mean = mean(Clothes.Washes.Per.Week)
#                         ,SE   = sd(Clothes.Washes.Per.Week) / SampleSize)
# #summarise across states (by region)
# item89.sum1 <- summarise(group_by(item89.dat2, BuildingType)
#                          ,State = "Region"
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Mean = mean(Clothes.Washes.Per.Week)
#                          ,SE   = sd(Clothes.Washes.Per.Week) / SampleSize)
# 
# item89.final <- rbind.data.frame(item89.sum, item89.sum1, stringsAsFactors = F)
# 
# #put in correct column order
# item89.table <- data.frame("BuildingType" = item89.final$BuildingType
#                            ,"State" = item89.final$State
#                            ,"Mean" = item89.final$Mean
#                            ,"SE" = item89.final$SE
#                            ,"SampleSize" = item89.final$SampleSize)
# item89.table1 <- item89.table[which(item89.table$BuildingType %in% c("Single Family", "Manufactured")),]
# 
# 



#############################################################################################
#Item 91: AVERAGE NUMBER OF CLOTHES WASHER LOADS PER WEEK BY STATE (SF table 98, MH table 79)
#############################################################################################
#subset to columns needed for analysis
item91.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                    ,"INTRVW_CUST_RES_HomeandEnergyUseHome_ClothesWasherLoadsPerWeek"
                                                                                    ,"INTRVW_CUST_RES_HomeandEnergyUseHome_PercentOfLoadsThatGoInDryer"))])
colnames(item91.dat) <- c("CK_Cadmus_ID", "Clothes.Washes.Per.Week", "Percent.Loads.Go.In.Dryer")
item91.dat$count <- 1

item91.dat0 <- item91.dat[which(item91.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item91.dat1 <- left_join(rbsa.dat, item91.dat0, by = "CK_Cadmus_ID")

item91.dat2 <- item91.dat1[which(item91.dat1$Clothes.Washes.Per.Week != "No Washing Machine"),]
item91.dat2$Clothes.Washes.Per.Week   <- as.numeric(as.character(item91.dat2$Clothes.Washes.Per.Week))
item91.dat2$Percent.Loads.Go.In.Dryer <- as.numeric(as.character(item91.dat2$Percent.Loads.Go.In.Dryer))
item91.dat2$Dryer.Loads.Per.Week <- item91.dat2$Clothes.Washes.Per.Week * (item91.dat2$Percent.Loads.Go.In.Dryer / 100)

item91.dat2$Dryer.Loads.Per.Wash <- item91.dat2$Dryer.Loads.Per.Week / item91.dat2$Clothes.Washes.Per.Week


################################################
# Adding pop and sample sizes for weights
################################################
item91.data <- weightedData(item91.dat2[-which(colnames(item91.dat2) %in% c("Clothes.Washes.Per.Week"
                                                                            ,"Percent.Loads.Go.In.Dryer"
                                                                            ,"count"
                                                                            ,"Dryer.Loads.Per.Week"
                                                                            ,"Dryer.Loads.Per.Wash"))])
item91.data <- left_join(item91.data, item91.dat2[which(colnames(item91.dat2) %in% c("CK_Cadmus_ID"
                                                                                      ,"Clothes.Washes.Per.Week"
                                                                                     ,"Percent.Loads.Go.In.Dryer"
                                                                                     ,"count"
                                                                                     ,"Dryer.Loads.Per.Week"
                                                                                     ,"Dryer.Loads.Per.Wash"))])
item91.data$count <- 1

#######################
# Weighted Analysis
#######################
item91.final <- proportions_one_group(CustomerLevelData = item91.data
                                      ,valueVariable    = 'Dryer.Loads.Per.Wash'
                                      ,groupingVariable = 'State'
                                      ,total.name       = 'Region'
                                      ,columnName       = 'Dryer Loads per Washer Load')

item91.final.SF <- item91.final[which(item91.final$BuildingType == "Single Family")
                                ,-which(colnames(item91.final) %in% c("BuildingType"))]
item91.final.MH <- item91.final[which(item91.final$BuildingType == "Manufactured")
                                ,-which(colnames(item91.final) %in% c("BuildingType"))]

exportTable(item91.final.SF, "SF", "Table 98", weighted = TRUE)
exportTable(item91.final.MH, "MH", "Table 79", weighted = TRUE)



## OLD CODE ##
# 
# item91.state <- summarise(group_by(item91.dat2, BuildingType, State)
#                           ,SampleSize = length(unique(CK_Cadmus_ID))
#                           ,Mean = mean(Dryer.Loads.Per.Wash)
#                           ,SE = sd(Dryer.Loads.Per.Wash) / SampleSize)
# 
# item91.region <- summarise(group_by(item91.dat2, BuildingType)
#                           ,State = "Region"
#                           ,SampleSize = length(unique(CK_Cadmus_ID))
#                           ,Mean = mean(Dryer.Loads.Per.Wash)
#                           ,SE = sd(Dryer.Loads.Per.Wash) / sqrt(SampleSize))
# 
# item91.final <- rbind.data.frame(item91.state, item91.region, stringsAsFactors = F)
# 
# item91.table <- item91.final[which(item91.final$BuildingType %in% c("Single Family", "Manufactured")),]
# 
# item91.table1 <- data.frame("BuildingType" = item91.table$BuildingType
#                             ,"State" = item91.table$State
#                             ,"Mean" = item91.table$Mean
#                             ,"SE" = item91.table$SE
#                             ,"SampleSize" = item91.table$SampleSize)
# 
# 






#############################################################################################
#Item 93: AVERAGE NUMBER OF DISHWASHER LOADS PER WEEK (SF table 100, MH table 81)
#############################################################################################
#subset to columns needed for analysis
item93.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                    ,"INTRVW_CUST_RES_HomeandEnergyUseHome_DishwasherLoadsPerWeek"
                                                                                    ,""))])
colnames(item93.dat) <- c("CK_Cadmus_ID", "Dishwashes.Per.Week")
item93.dat$count <- 1

item93.dat0 <- item93.dat[which(item93.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item93.dat1 <- left_join(rbsa.dat, item93.dat0, by = "CK_Cadmus_ID")

item93.dat2 <- item93.dat1[which(!(is.na(item93.dat1$Dishwashes.Per.Week))),]
item93.dat2$Dishwashes.Per.Week <- as.numeric(as.character(item93.dat2$Dishwashes.Per.Week))

# Weighting
item93.data <- weightedData(item93.dat2[-which(colnames(item93.dat2) %in% c("Dishwashes.Per.Week"
                                                                            ,"count"))])

item93.data <- left_join(item93.data, item93.dat2[which(colnames(item93.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"Dishwashes.Per.Week"
                                                                                     ,"count"))])

###############################
# Weighted Analysis
###############################

item93.final <- mean_one_group(CustomerLevelData = item93.data
                               ,valueVariable = 'Dishwashes.Per.Week' 
                               ,byVariable    = 'State'
                               ,aggregateRow  = "Region")

# Export table
item93.final.SF <- item93.final[which(item93.final$BuildingType == "Single Family"),-1]
item93.final.MH <- item93.final[which(item93.final$BuildingType == "Manufactured"),-1]

exportTable(item93.final.SF, "SF", "Table 100", weighted = TRUE)
exportTable(item93.final.MH, "MH", "Table 81", weighted = TRUE)


## OLD CODE ##
# 
# 
# item93.state <- summarise(group_by(item93.dat2, BuildingType, State)
#                           ,SampleSize = length(unique(CK_Cadmus_ID))
#                           ,Mean = mean(Dishwashes.Per.Week)
#                           ,SE = sd(Dishwashes.Per.Week) / sqrt(SampleSize))
# 
# item93.region <- summarise(group_by(item93.dat2, BuildingType)
#                           ,State = "Region"
#                           ,SampleSize = length(unique(CK_Cadmus_ID))
#                           ,Mean = mean(Dishwashes.Per.Week)
#                           ,SE = sd(Dishwashes.Per.Week) / sqrt(SampleSize))
# item93.final <- rbind.data.frame(item93.state, item93.region, stringsAsFactors = F)
# 
# item93.final1 <- item93.final[which(item93.final$BuildingType %in% c("Single Family", "Manufactured")),]
# 
# item93.table <- data.frame("BuildingType" = item93.final1$BuildingType
#                            ,"State" = item93.final1$State
#                            ,"Mean" = item93.final1$Mean
#                            ,"SE"= item93.final1$SE
#                            ,"SampleSize" = item93.final1$SampleSize)
