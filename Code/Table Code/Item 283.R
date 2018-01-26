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
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))





#############################################################################################
#Item 283: IN-UNIT THERMOSTAT SETTINGS AND BEHAVIOR (MF Table 75)
#############################################################################################
item283.dat <- sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                              ,"CK_SiteID"
                                                                              ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouHeatYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                              ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouGoToBedWhatDoYouSetTheThermostatToForHeating"
                                                                              ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouCoolYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                              ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouGoToBedWhatDoYouSetTheThermostatToForCooling"))]
colnames(item283.dat)
#rename in correct order
colnames(item283.dat) <- c("CK_Cadmus_ID"
                           ,"CK_SiteID"
                           ,"Cooling.Setpoint"
                           ,"Cooling.at.Night"
                           ,"Heating.at.Night"
                           ,"Heating.Setpoint")

#subset to only buidling level information
item283.dat0 <- item283.dat[grep("SITE",item283.dat$CK_SiteID),]

#merge on sites.interview data with rbsa cleaned data
item283.dat1 <- left_join(rbsa.dat, item283.dat0, by = "CK_Cadmus_ID")

#subset to only multifamily units
item283.dat2 <- item283.dat1[grep("Multifamily",item283.dat1$BuildingType),]


############################
# For Heating Setpoint
############################
item283.dat2$Heating.Setpoint <- as.numeric(as.character(item283.dat2$Heating.Setpoint))
item283.dat2$Heating.at.Night <- as.numeric(as.character(item283.dat2$Heating.at.Night))
item283.dat2$Cooling.Setpoint <- as.numeric(as.character(item283.dat2$Cooling.Setpoint))
item283.dat2$Cooling.at.Night <- as.numeric(as.character(item283.dat2$Cooling.at.Night))
item283.dat3 <- item283.dat2[which(!(item283.dat2$Heating.Setpoint %in% c(NA, 0,"N/A"))),]

######################################
#Pop and Sample Sizes for weights
######################################
item283.data <- weightedData(item283.dat3[which(colnames(item283.dat3) %notin% c("CK_SiteID"
                                                                                 ,"Cooling.Setpoint"
                                                                                 ,"Cooling.at.Night"
                                                                                 ,"Heating.at.Night"
                                                                                 ,"Heating.Setpoint"))])

item283.data <- left_join(item283.data, item283.dat3[which(colnames(item283.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Cooling.Setpoint"
                                                                                         ,"Cooling.at.Night"
                                                                                         ,"Heating.at.Night"
                                                                                         ,"Heating.Setpoint"))])
item283.data$count <- 1


######################
# weighted analysis
######################

item283.weighted.heat.setpoint <- mean_one_group(CustomerLevelData = item283.data
                                                 ,valueVariable = 'Heating.Setpoint'
                                                 ,byVariable = 'BuildingType'
                                                 ,aggregateRow = "All Types")
item283.weighted.heat.setpoint$Category <- "Heating.Setpoint"
item283.weighted.heat.setpoint <- item283.weighted.heat.setpoint[which(colnames(item283.weighted.heat.setpoint) %notin% c("Precision","n_h","N_h","N"))]



#### unweighted
item283.unweighted.heat.setpoint <- summarise(item283.data
                                              ,Category = "Heating.Setpoint"
                                              ,Mean = mean(Heating.Setpoint,na.rm = T)
                                              ,SE = sd(Heating.Setpoint,na.rm = T) / sqrt(length(unique(CK_Cadmus_ID)))
                                              ,n = length(unique(CK_Cadmus_ID)))



############################
# For Percent Heating Setback
############################
item283.dat4 <- item283.dat2[which(!(item283.dat2$Heating.Setpoint %in% c(NA, 0,"N/A"))),]
item283.dat5 <- item283.dat4[which(!(item283.dat4$Heating.at.Night %in% c(NA, 0,"N/A"))),]
item283.dat5$Ind <- 0
item283.dat5$Ind[which(item283.dat5$Heating.at.Night < item283.dat5$Heating.Setpoint)] <- 1
item283.dat5$count <- 1

######################################
#Pop and Sample Sizes for weights
######################################
item283.data <- weightedData(item283.dat5[which(colnames(item283.dat5) %notin% c("CK_SiteID"
                                                                                 ,"Cooling.Setpoint"
                                                                                 ,"Cooling.at.Night"
                                                                                 ,"Heating.at.Night"
                                                                                 ,"Heating.Setpoint"
                                                                                 ,"Ind"
                                                                                 ,"count"))])

item283.data <- left_join(item283.data, item283.dat5[which(colnames(item283.dat5) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Cooling.Setpoint"
                                                                                         ,"Cooling.at.Night"
                                                                                         ,"Heating.at.Night"
                                                                                         ,"Heating.Setpoint"
                                                                                         ,"Ind"
                                                                                         ,"count"))])
item283.data$count <- 1

item283.data$Count <- 1
######################
# weighted analysis
######################
item283.weighted.reported.setback <- proportions_one_group(CustomerLevelData = item283.data
                                                              ,valueVariable = 'Ind'
                                                              ,groupingVariable = 'BuildingType'
                                                              ,total.name = "All Types")
item283.weighted.reported.setback$Category <- "Percent Heating Setback"
item283.weighted.reported.setback <- item283.weighted.reported.setback[which(colnames(item283.weighted.reported.setback) %notin% c("Precision","n_h","N_h","count","N"))]
names(item283.weighted.reported.setback)[which(names(item283.weighted.reported.setback) %in% c("w.percent","w.SE"))] <- c("Mean","SE")

#please note: Mean here is actually a percent, named Mean for combining purposes
item283.unweighted.reported.setback <- summarise(item283.data
                                               ,Category = "Percent.Heating.Setback"
                                               ,Mean = sum(Ind) / sum(count)
                                               ,SE = sqrt(Mean * (1 - Mean) / length(unique(CK_Cadmus_ID)))
                                               ,n = length(unique(CK_Cadmus_ID)))




############################
# For Average Heating Setback
############################
item283.data$Heating.Setback <- item283.data$Heating.Setpoint - item283.data$Heating.at.Night

item283.weighted.heat.setback <- mean_one_group(CustomerLevelData = item283.data
                                      ,valueVariable = 'Heating.Setback'
                                      ,byVariable = 'BuildingType'
                                      ,aggregateRow = "All Types")
item283.weighted.heat.setback$Category <- "Average Heating Setback"
item283.weighted.heat.setback<-item283.weighted.heat.setback[which(names(item283.weighted.heat.setback) %notin% c("Precision","n_h","N_h","count","N"))]

item283.unweighted.heat.setback <- summarise(item283.data
                          ,Category = "Average.Heating.Setback"
                          ,Mean = mean(Heating.Setback)
                          ,SE = sd(Heating.Setback) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,n = length(unique(CK_Cadmus_ID)))


############################
# For Cooling Setpoint
############################
item283.dat3 <- item283.dat2[which(!(item283.dat2$Cooling.Setpoint %in% c(NA, 0,"N/A"))),]

######################################
#Pop and Sample Sizes for weights
######################################
item283.data <- weightedData(item283.dat3[which(colnames(item283.dat3) %notin% c("CK_SiteID"
                                                                                 ,"Cooling.Setpoint"
                                                                                 ,"Cooling.at.Night"
                                                                                 ,"Heating.at.Night"
                                                                                 ,"Heating.Setpoint"))])

item283.data <- left_join(item283.data, item283.dat3[which(colnames(item283.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Cooling.Setpoint"
                                                                                         ,"Cooling.at.Night"
                                                                                         ,"Heating.at.Night"
                                                                                         ,"Heating.Setpoint"))])
item283.data$count <- 1


######################
# weighted analysis
######################

item283.weighted.cool.setpoint <- mean_one_group(CustomerLevelData = item283.data
                                      ,valueVariable = 'Cooling.Setpoint'
                                      ,byVariable = 'BuildingType'
                                      ,aggregateRow = "All Types")
item283.weighted.cool.setpoint$Category <- "Cooling Setpoint"
item283.weighted.cool.setpoint <- item283.weighted.cool.setpoint[which(names(item283.weighted.cool.setpoint) %notin% c("Precision","n_h","N_h","count","N"))]
#### unweighted
item283.unweighted.cool.setpoint <- summarise(item283.data
                          ,Category = "Cooling.Setpoint"
                          ,Mean = mean(Cooling.Setpoint)
                          ,SE = sd(Cooling.Setpoint) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,n = length(unique(CK_Cadmus_ID)))


############################
# For Percent Cooling Setup
############################
item283.dat4 <- item283.dat2[which(!(item283.dat2$Cooling.Setpoint %in% c(NA, 0,"N/A"))),]
item283.dat4 <- item283.dat4[which(!(item283.dat4$Cooling.at.Night %in% c(NA, 0,"N/A"))),]
item283.dat4$Ind <- 0
item283.dat4$Ind[which(item283.dat4$Cooling.at.Night > item283.dat4$Cooling.Setpoint)] <- 1
item283.dat4$count <- 1

######################################
#Pop and Sample Sizes for weights
######################################
item283.data <- weightedData(item283.dat4[which(colnames(item283.dat4) %notin% c("CK_SiteID"
                                                                                 ,"Cooling.Setpoint"
                                                                                 ,"Cooling.at.Night"
                                                                                 ,"Heating.at.Night"
                                                                                 ,"Heating.Setpoint"
                                                                                 ,"Ind"
                                                                                 ,"count"))])

item283.data <- left_join(item283.data, item283.dat4[which(colnames(item283.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Cooling.Setpoint"
                                                                                         ,"Cooling.at.Night"
                                                                                         ,"Heating.at.Night"
                                                                                         ,"Heating.Setpoint"
                                                                                         ,"Ind"
                                                                                         ,"count"))])
item283.data$count <- 1
item283.data$Count <- 1

######################
# weighted analysis
######################
item283.weighted.reported.setup <- proportions_one_group(CustomerLevelData = item283.data
                                                              ,valueVariable = 'Ind'
                                                              ,groupingVariable = 'BuildingType'
                                                              ,total.name = "All Types")
item283.weighted.reported.setup$Category <- "Percent Cooling Setup"
item283.weighted.reported.setup<-item283.weighted.reported.setup[which(names(item283.weighted.reported.setup) %notin% c("Precision","n_h","N_h","count","N"))]
names(item283.weighted.reported.setup)[which(names(item283.weighted.reported.setup) %in% c("w.percent","w.SE"))] <- c("Mean","SE")
# #please note: Mean here is actually a percent, named Mean for combining purposes
item283.unweighted.reported.setup <- summarise(item283.data
                          ,Category = "Percent.Cooling.Setup"
                          ,Mean = sum(Ind) / sum(count)
                          ,SE = sqrt(Mean * (1 - Mean) / length(unique(CK_Cadmus_ID)))
                          ,n = length(unique(CK_Cadmus_ID)))


############################
# Combine weighted results
############################
item283.final.weighted <- rbind.data.frame(item283.weighted.heat.setpoint
                                  ,item283.weighted.reported.setback
                                  ,item283.weighted.heat.setback
                                  ,item283.weighted.cool.setpoint
                                  ,item283.weighted.reported.setup
                                  ,stringsAsFactors = F)
item283.final.weighted <- item283.final.weighted[which(names(item283.final.weighted) %notin% c("BuildingType"))]
exportTable(item283.final.weighted, "MF", "Table 75", weighted = TRUE)


############################
# Combine unweighted results
############################
item283.final.unweighted <- rbind.data.frame(item283.unweighted.heat.setpoint
                                  ,item283.unweighted.reported.setback
                                  ,item283.unweighted.heat.setback
                                  ,item283.unweighted.cool.setpoint
                                  ,item283.unweighted.reported.setup
                                  ,stringsAsFactors = F)
exportTable(item283.final.unweighted, "MF", "Table 75", weighted = FALSE)
