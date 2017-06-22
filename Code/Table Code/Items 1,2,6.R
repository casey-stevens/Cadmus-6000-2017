#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list=ls())

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))



#############################################################################################
# Item 1
#############################################################################################
item1.dat <- rbsa.dat

item1.dat$count <- 1

#Get state information
item1.state.tab0 <- summarise(group_by(item1.dat, State)
                           ,BT_State_Count = sum(count)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
)

item1.state.tab1 <- summarise(group_by(item1.dat, BuildingType, BuildingTypeXX, State)
                           ,BuildingType_Count = sum(count)
)
item1.state.full <- left_join(item1.state.tab1, item1.state.tab0, by = "State")

#get region information
item1.region.tab0 <- summarise(group_by(item1.dat)
                            ,State = "Region"
                            ,BT_State_Count = sum(count)
                            ,SampleSize = length(unique(CK_Cadmus_ID))
)
item1.region.tab1 <- summarise(group_by(item1.dat, BuildingType, BuildingTypeXX)
                            ,State = "Region"
                            ,BuildingType_Count = sum(count)
)
item1.region.full <- left_join(item1.region.tab1, item1.region.tab0, by = "State")

#rbind state and region information
item1.tab.full <- rbind.data.frame(item1.state.full, item1.region.full, stringsAsFactors = F)

item1.tab.full$Percent <- item1.tab.full$BuildingType_Count / item1.tab.full$BT_State_Count
item1.tab.full$EB      <- 1.645 * sqrt((item1.tab.full$Percent * (1 - item1.tab.full$Percent)) / length(unique(item1.dat$CK_Cadmus_ID)))


#############################################################################################
#Put in correct table format



#############################################################################################
# Item 2
#############################################################################################
item2.dat <- rbsa.dat

item2.dat$count <- 1

#Get state information
item2.state.tab0 <- summarise(group_by(item2.dat, BuildingType, State)
                        ,SampleSize = length(unique(CK_Cadmus_ID))
)

item2.state.tab1 <- summarise(group_by(item2.dat, BuildingType, HomeYearBuilt_bins, State)
                        ,HomeType_Count = sum(count)
)
item2.state.full <- left_join(item2.state.tab1, item2.state.tab0, by = c("BuildingType","State"))

#get region information
item2.region.tab0 <- summarise(group_by(item2.dat, BuildingType)
                                 ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
)
item2.region.tab1 <- summarise(group_by(item2.dat, BuildingType, HomeYearBuilt_bins)
                                 ,State = "Region"
                                 ,HomeType_Count = sum(count)
)
item2.region.full <- left_join(item2.region.tab1, item2.region.tab0, by = c("BuildingType","State"))

#rbind state and region information
item2.tab.full <- rbind.data.frame(item2.state.full, item2.region.full, stringsAsFactors = F)

item2.tab.full$Percent <- item2.tab.full$HomeType_Count / item2.tab.full$SampleSize
item2.tab.full$EB      <- 1.645 * sqrt((item2.tab.full$Percent * (1 - item2.tab.full$Percent)) / item2.tab.full$SampleSize)






#############################################################################################
# Item 6: AVERAGE CONDITIONED FLOOR AREA BY STATE AND BUILDING HEIGHT (table 13)
#############################################################################################
item6.dat  <- rbsa.dat[which(!(is.na(rbsa.dat$BuildingHeight))),] ##only 369
#subset to only single family homes
item6.dat1 <- item6.dat[which(item6.dat$BuildingType == "Single Family"),] ##only 323
item6.dat1$count <- 1

#Get state information
item6.state.tab0 <- summarise(group_by(item6.dat1, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
)

item6.state.tab1 <- summarise(group_by(item6.dat1, BuildingType, State, BuildingHeight)
                           ,Count = sum(count)
)
item6.state.full <- left_join(item6.state.tab1, item6.state.tab0, by = c("BuildingType","State"))

#get region information
item6.region.tab0 <- summarise(group_by(item6.dat1, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
)
item6.region.tab1 <- summarise(group_by(item6.dat1, BuildingType, BuildingHeight)
                            ,State = "Region"
                            ,Count = sum(count)
)
item6.region.full <- left_join(item6.region.tab1, item6.region.tab0, by = c("BuildingType","State"))

#rbind state and region information
item6.tab.full <- rbind.data.frame(item6.state.full, item6.region.full, stringsAsFactors = F)

item6.tab.full$Percent <- item6.tab.full$Count / item6.tab.full$SampleSize
item6.tab.full$EB      <- 1.645 * sqrt((item6.tab.full$Percent * (1 - item6.tab.full$Percent)) / item6.tab.full$SampleSize)

  