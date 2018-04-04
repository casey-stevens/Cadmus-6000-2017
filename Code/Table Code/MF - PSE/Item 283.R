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
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))

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
                                                                                 ,"Heating.Setpoint"
                                                                                 ,"Category"))])

item283.data <- left_join(item283.data, item283.dat3[which(colnames(item283.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Cooling.Setpoint"
                                                                                         ,"Cooling.at.Night"
                                                                                         ,"Heating.at.Night"
                                                                                         ,"Heating.Setpoint"
                                                                                         ,"Category"))])
item283.data$count <- 1


######################
# weighted analysis
######################

item283.weighted.heat.setpoint <- mean_one_group(CustomerLevelData = item283.data
                                                 ,valueVariable = 'Heating.Setpoint'
                                                 ,byVariable = 'Category'
                                                 ,aggregateRow = "Remove")
item283.weighted.heat.setpoint <- item283.weighted.heat.setpoint[which(item283.weighted.heat.setpoint$Category != "Remove"),]
item283.weighted.heat.setpoint$Thermostat.Category <- "Heating.Setpoint"
item283.weighted.heat.setpoint <- item283.weighted.heat.setpoint[which(colnames(item283.weighted.heat.setpoint) %notin% c("Precision","n_h","N_h","N","Count","count"))]



#### unweighted
item283.unw.heat.setpoint <- mean_one_group_unweighted(CustomerLevelData = item283.data
                                                            ,valueVariable = 'Heating.Setpoint'
                                                            ,byVariable = 'Category'
                                                            ,aggregateRow = "Remove")
item283.unw.heat.setpoint <- item283.unw.heat.setpoint[which(item283.unw.heat.setpoint$Category != "Remove"),]
item283.unw.heat.setpoint$Thermostat.Category <- "Heating.Setpoint"
item283.unw.heat.setpoint <- item283.unw.heat.setpoint[which(colnames(item283.unw.heat.setpoint) %notin% c("Precision","n_h","N_h","N","Count","count"))]



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
                                                                                 ,"count"
                                                                                 ,"Category"))])

item283.data <- left_join(item283.data, item283.dat5[which(colnames(item283.dat5) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Cooling.Setpoint"
                                                                                         ,"Cooling.at.Night"
                                                                                         ,"Heating.at.Night"
                                                                                         ,"Heating.Setpoint"
                                                                                         ,"Ind"
                                                                                         ,"count"
                                                                                         ,"Category"))])
item283.data$count <- 1

item283.data$Count <- 1
######################
# weighted analysis
######################
item283.data$State <- item283.data$Category
item283.weighted.reported.setback <- proportions_one_group(CustomerLevelData = item283.data
                                                              ,valueVariable = 'Ind'
                                                              ,groupingVariable = 'State'
                                                              ,total.name = "Remove")
item283.weighted.reported.setback <- item283.weighted.reported.setback[which(item283.weighted.reported.setback$State != "Total"),]
item283.weighted.reported.setback$Thermostat.Category <- "Percent Heating Setback"
item283.weighted.reported.setback <- item283.weighted.reported.setback[which(colnames(item283.weighted.reported.setback) %notin% c("Precision","n_h","N_h","count","N","Count"))]
names(item283.weighted.reported.setback)[which(names(item283.weighted.reported.setback) %in% c("State","w.percent","w.SE"))] <- c("Category","Mean","SE")

#please note: Mean here is actually a percent, named Mean for combining purposes
item283.unw.reported.setback <- proportions_one_group(CustomerLevelData = item283.data
                                                      ,valueVariable = 'Ind'
                                                      ,groupingVariable = 'State'
                                                      ,total.name = "Remove"
                                                      ,weighted = FALSE)
item283.unw.reported.setback <- item283.unw.reported.setback[which(item283.unw.reported.setback$State != "Total"),]
item283.unw.reported.setback$Thermostat.Category <- "Percent Heating Setback"
item283.unw.reported.setback <- item283.unw.reported.setback[which(colnames(item283.unw.reported.setback) %notin% c("Precision","n_h","N_h","count","N","Count"))]
names(item283.unw.reported.setback)[which(names(item283.unw.reported.setback) %in% c("State","Percent","SE"))] <- c("Category","Mean","SE")




############################
# For Average Heating Setback
############################
item283.data$Heating.Setback <- item283.data$Heating.Setpoint - item283.data$Heating.at.Night

item283.weighted.heat.setback <- mean_one_group(CustomerLevelData = item283.data
                                      ,valueVariable = 'Heating.Setback'
                                      ,byVariable = 'Category'
                                      ,aggregateRow = "Remove")
item283.weighted.heat.setback <- item283.weighted.heat.setback[which(item283.weighted.heat.setback$Category != "Remove"),]
item283.weighted.heat.setback$Thermostat.Category <- "Average Heating Setback"
item283.weighted.heat.setback<-item283.weighted.heat.setback[which(names(item283.weighted.heat.setback) %notin% c("Precision","n_h","N_h","count","N","Count"))]



item283.unw.heat.setback <- mean_one_group_unweighted(CustomerLevelData = item283.data
                                                ,valueVariable = 'Heating.Setback'
                                                ,byVariable = 'Category'
                                                ,aggregateRow = "Remove")
item283.unw.heat.setback <- item283.unw.heat.setback[which(item283.unw.heat.setback$Category != "Remove"),]
item283.unw.heat.setback$Thermostat.Category <- "Average Heating Setback"
item283.unw.heat.setback<-item283.unw.heat.setback[which(names(item283.unw.heat.setback) %notin% c("Precision","n_h","N_h","count","N","Count"))]


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
                                                                                 ,"Heating.Setpoint"
                                                                                 ,"Category"))])

item283.data <- left_join(item283.data, item283.dat3[which(colnames(item283.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Cooling.Setpoint"
                                                                                         ,"Cooling.at.Night"
                                                                                         ,"Heating.at.Night"
                                                                                         ,"Heating.Setpoint"
                                                                                         ,"Category"))])
item283.data$count <- 1


######################
# weighted analysis
######################

item283.weighted.cool.setpoint <- mean_one_group(CustomerLevelData = item283.data
                                      ,valueVariable = 'Cooling.Setpoint'
                                      ,byVariable = 'Category'
                                      ,aggregateRow = "Remove")
item283.weighted.cool.setpoint <- item283.weighted.cool.setpoint[which(item283.weighted.cool.setpoint$Category != "Remove"),]
item283.weighted.cool.setpoint$Thermostat.Category <- "Cooling Setpoint"
item283.weighted.cool.setpoint <- item283.weighted.cool.setpoint[which(names(item283.weighted.cool.setpoint) %notin% c("Precision","n_h","N_h","count","N","Count"))]
#### unweighted
item283.unw.cool.setpoint <- mean_one_group_unweighted(CustomerLevelData = item283.data
                                                 ,valueVariable = 'Cooling.Setpoint'
                                                 ,byVariable = 'Category'
                                                 ,aggregateRow = "Remove")
item283.unw.cool.setpoint <- item283.unw.cool.setpoint[which(item283.unw.cool.setpoint$Category != "Remove"),]
item283.unw.cool.setpoint$Thermostat.Category <- "Cooling Setpoint"
item283.unw.cool.setpoint <- item283.unw.cool.setpoint[which(names(item283.unw.cool.setpoint) %notin% c("Precision","n_h","N_h","count","N","Count"))]


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
                                                                                 ,"count"
                                                                                 ,"Category"))])

item283.data <- left_join(item283.data, item283.dat4[which(colnames(item283.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Cooling.Setpoint"
                                                                                         ,"Cooling.at.Night"
                                                                                         ,"Heating.at.Night"
                                                                                         ,"Heating.Setpoint"
                                                                                         ,"Ind"
                                                                                         ,"count"
                                                                                         ,"Category"))])
item283.data$count <- 1
item283.data$Count <- 1

######################
# weighted analysis
######################
item283.data$State <- item283.data$Category
item283.weighted.reported.setup <- proportions_one_group(CustomerLevelData = item283.data
                                                              ,valueVariable = 'Ind'
                                                              ,groupingVariable = 'State'
                                                              ,total.name = "Remove")
item283.weighted.reported.setup <- item283.weighted.reported.setup[which(item283.weighted.reported.setup$State != "Total"),]
item283.weighted.reported.setup$Thermostat.Category <- "Percent Cooling Setup"
item283.weighted.reported.setup<-item283.weighted.reported.setup[which(names(item283.weighted.reported.setup) %notin% c("Precision","n_h","N_h","count","N","Count"))]
names(item283.weighted.reported.setup)[which(names(item283.weighted.reported.setup) %in% c("State","w.percent","w.SE"))] <- c("Category","Mean","SE")
# #please note: Mean here is actually a percent, named Mean for combining purposes
item283.unw.reported.setup <- proportions_one_group(CustomerLevelData = item283.data
                                                         ,valueVariable = 'Ind'
                                                         ,groupingVariable = 'State'
                                                         ,total.name = "Remove"
                                                    ,weighted = FALSE)
item283.unw.reported.setup <- item283.unw.reported.setup[which(item283.unw.reported.setup$State != "Total"),]
item283.unw.reported.setup$Thermostat.Category <- "Percent Cooling Setup"
item283.unw.reported.setup<-item283.unw.reported.setup[which(names(item283.unw.reported.setup) %notin% c("Precision","n_h","N_h","count","N","Count"))]
names(item283.unw.reported.setup)[which(names(item283.unw.reported.setup) %in% c("State","Percent","SE"))] <- c("Category","Mean","SE")



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

item283.cast <- dcast(setDT(item283.final.weighted)
                      ,formula = Thermostat.Category ~ Category
                      ,value.var = c("Mean","SE","n","EB"))
names(item283.cast)
item283.table <- data.frame("Thermostat.Category"      = item283.cast$Thermostat.Category
                            ,"PSE.Mean"                 = item283.cast$Mean_PSE
                            ,"PSE.SE"                   = item283.cast$SE_PSE
                            ,"PSE.n"                    = item283.cast$n_PSE
                            ,"PSE.King.County.Mean"     = item283.cast$`Mean_PSE KING COUNTY`
                            ,"PSE.King.County.SE"       = item283.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"        = item283.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Mean" = item283.cast$`Mean_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"   = item283.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"    = item283.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Mean"        = item283.cast$`Mean_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"          = item283.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"           = item283.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                   = item283.cast$EB_PSE
                            ,"PSE.King.County_EB"       = item283.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"   = item283.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"          = item283.cast$`EB_2017 RBSA PS`)

exportTable(item283.table, "MF", "Table 75", weighted = TRUE,OS = T, osIndicator = "PSE")


############################
# Combine unweighted results
############################
item283.final.unw <- rbind.data.frame(item283.unw.heat.setpoint
                                  ,item283.unw.reported.setback
                                  ,item283.unw.heat.setback
                                  ,item283.unw.cool.setpoint
                                  ,item283.unw.reported.setup
                                  ,stringsAsFactors = F)
item283.cast <- dcast(setDT(item283.final.unw)
                      ,formula = Thermostat.Category ~ Category
                      ,value.var = c("Mean","SE","n"))
exportTable(item283.cast, "MF", "Table 75", weighted = FALSE,OS = T, osIndicator = "PSE")
