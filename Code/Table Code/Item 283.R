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
                           ,"Cooling.Setup"
                           ,"Heating.Setback"
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
