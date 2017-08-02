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
# item283.dat0 <- item283.dat[-grep("BLDG",item283.dat$CK_SiteID),]
# None of the site IDs in interview data have BLDG

#merge on sites.interview data with rbsa cleaned data
item283.dat1 <- left_join(rbsa.dat, item283.dat, by = "CK_Cadmus_ID")

#subset to only multifamily units
item283.dat2 <- item283.dat1[grep("Multifamily",item283.dat1$BuildingType),]


############################
# For Heating Setpoint
############################
item283.dat3 <- item283.dat2[which(!(item283.dat2$Heating.Setpoint %in% c(NA, 0))),]

item283.sum1 <- summarise(item283.dat3
                          ,Category = "Heating.Setpoint"
                          ,Mean = mean(Heating.Setpoint)
                          ,SE = sd(Heating.Setpoint) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))


############################
# For Percent Heating Setback
############################
item283.dat4 <- item283.dat2[which(!(item283.dat2$Heating.Setpoint %in% c(NA, 0))),]
item283.dat5 <- item283.dat4[which(!(item283.dat4$Heating.at.Night %in% c(NA, 0))),]
item283.dat5$Heating.Setback.Ind <- 0
item283.dat5$Heating.Setback.Ind[which(item283.dat5$Heating.at.Night < item283.dat5$Heating.Setpoint)] <- 1
item283.dat5$count <- 1

#please note: Mean here is actually a percent, named Mean for combining purposes
item283.sum2 <- summarise(item283.dat5
                          ,Category = "Percent.Heating.Setback"
                          ,Mean = sum(Heating.Setback.Ind) / sum(count)
                          ,SE = sqrt(Mean * (1 - Mean) / length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

############################
# For Average Heating Setback
############################
item283.dat6 <- item283.dat5
item283.dat6$Heating.Setback <- item283.dat6$Heating.Setpoint - item283.dat6$Heating.at.Night

item283.sum3 <- summarise(item283.dat6
                          ,Category = "Average.Heating.Setback"
                          ,Mean = mean(Heating.Setback)
                          ,SE = sd(Heating.Setback) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))


############################
# For Cooling Setpoint
############################
item283.dat7 <- item283.dat2[which(!(item283.dat2$Cooling.Setpoint %in% c(NA, 0))),]

item283.sum4 <- summarise(item283.dat7
                          ,Category = "Cooling.Setpoint"
                          ,Mean = mean(Cooling.Setpoint)
                          ,SE = sd(Cooling.Setpoint) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))


############################
# For Percent Cooling Setup
############################
item283.dat8 <- item283.dat7
item283.dat9 <- item283.dat8[which(!(item283.dat8$Cooling.at.Night %in% c(NA, 0))),]
item283.dat9$Cooling.Setup.Ind <- 0
item283.dat9$Cooling.Setup.Ind[which(item283.dat9$Cooling.at.Night > item283.dat9$Cooling.Setpoint)] <- 1
item283.dat9$count <- 1

#please note: Mean here is actually a percent, named Mean for combining purposes
item283.sum5 <- summarise(item283.dat9
                          ,Category = "Percent.Cooling.Setup"
                          ,Mean = sum(Cooling.Setup.Ind) / sum(count)
                          ,SE = sqrt(Mean * (1 - Mean) / length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))


############################
# Combine
############################
item283.final <- rbind.data.frame(item283.sum1
                                  ,item283.sum2
                                  ,item283.sum3
                                  ,item283.sum4
                                  ,item283.sum5
                                  ,stringsAsFactors = F)
