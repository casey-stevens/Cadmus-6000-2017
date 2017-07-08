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
#Item 124: DISTRIBUTION OF HOMES BY OWNERSHIP TYPE AND STATE (SF table 131, MH table 106)
#############################################################################################
#subset to columns needed for analysis
item124.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_DoYouOwnOrRent"
                                                                                     ,""))])
colnames(item124.dat) <- c("CK_Cadmus_ID", "Ownership")
item124.dat$count <- 1

#remove any repeat header rows from exporting
item124.dat0 <- item124.dat[which(item124.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item124.dat1 <- left_join(item124.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item124.dat1$Ownership)



















#############################################################################################
#Item 125: PERCENTAGE OF HOMES AS PRIMARY RESIDENCE BY STATE (SF table 132, MH table 107)
#############################################################################################
#subset to columns needed for analysis
item125.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_IsThisYourPrimaryHome_Y_N"
                                                                                     ,""))])
colnames(item125.dat) <- c("CK_Cadmus_ID", "Primary_Home")
item125.dat$count <- 1

#remove any repeat header rows from exporting
item125.dat0 <- item125.dat[which(item125.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item125.dat1 <- left_join(item125.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item125.dat1$Primary_Home)

item125.dat2 <- item125.dat1[which(!(is.na(item125.dat1$Primary_Home))),]

#summarise by state
item125.sum1 <- summarise(group_by(item125.dat2, BuildingType, State)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Count = length(which(Primary_Home == "Primary"))
                          ,Total.Count = sum(count)
                          ,Percent = Count / Total.Count
                          ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

#summarise across states
item125.sum2 <- summarise(group_by(item125.dat2, BuildingType)
                          ,State = "Region"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Count = length(which(Primary_Home == "Primary"))
                          ,Total.Count = sum(count)
                          ,Percent = Count / Total.Count
                          ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

item125.sum <- rbind.data.frame(item125.sum1, item125.sum2, stringsAsFactors = F)


item125.final <- data.frame("BuildingType" = item125.sum$BuildingType
                            ,"State" = item125.sum$State
                            ,"Percent" = item125.sum$Percent
                            ,"SE" = item125.sum$SE
                            ,"SampleSize" = item125.sum$SampleSize)

item125.table <- item125.final[which(item125.final$BuildingType %in% c("Single Family", "Manufactured")),]














#############################################################################################
# Needs to be updated once we get the correct fields for this table
#############################################################################################
#Item 126: PERCENTAGE OF HOMES WITH HOME OFFICES BY STATE (SF table 133, MH table 108)
#############################################################################################
#subset to columns needed for analysis
item126.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,""
                                                                                     ,""))])
colnames(item126.dat) <- c("CK_Cadmus_ID", "")
item126.dat$count <- 1

#remove any repeat header rows from exporting
item126.dat0 <- item126.dat[which(item126.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item126.dat1 <- left_join(item126.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item126.dat1$Primary_Home)

item126.dat2 <- item126.dat1[which(!(is.na(item126.dat1$Primary_Home))),]

#summarise by state
item126.sum1 <- summarise(group_by(item126.dat2, BuildingType, State)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Count = length(which(Primary_Home == "Primary"))
                          ,Total.Count = sum(count)
                          ,Percent = Count / Total.Count
                          ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

#summarise across states
item126.sum2 <- summarise(group_by(item126.dat2, BuildingType)
                          ,State = "Region"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Count = length(which(Primary_Home == "Primary"))
                          ,Total.Count = sum(count)
                          ,Percent = Count / Total.Count
                          ,SE = sqrt(Percent * (1 - Percent) / SampleSize))

item126.sum <- rbind.data.frame(item126.sum1, item126.sum2, stringsAsFactors = F)


item126.final <- data.frame("BuildingType" = item126.sum$BuildingType
                            ,"State" = item126.sum$State
                            ,"Percent" = item126.sum$Percent
                            ,"SE" = item126.sum$SE
                            ,"SampleSize" = item126.sum$SampleSize)

item126.table <- item126.final[which(item126.final$BuildingType %in% c("Single Family", "Manufactured")),]














#############################################################################################
#Item 127: DISTRIBUTION OF HOMES WITH ELECTRIC FUEL ASSISTANCE BY PERCENTAGE OF ASSISTANCE AND STATE (SF table 134, MH table 109)
#############################################################################################
#subset to columns needed for analysis
item127.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_DoesAnotherEntityPayPartOfYourElectricBill_WhatShare"
                                                                                     ,""))])
colnames(item127.dat) <- c("CK_Cadmus_ID", "Electric_Fuel_Assistance")
item127.dat$count <- 1

#remove any repeat header rows from exporting
item127.dat0 <- item127.dat[which(item127.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item127.dat1 <- left_join(item127.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item127.dat1$Electric_Fuel_Assistance)
