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
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"Primary.Heating.System"
                                                                        ,"Heating.Fuel"))]





#############################################################################################
#Item 43: DISTRIBUTION OF PRIMARY HEATING SYSTEMS (SF table 50, MF table 35)
#############################################################################################
item43.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item43.dat1 <- item43.dat[which(item43.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item43.dat2 <- item43.dat1[which(item43.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(item43.dat2$Primary.Heating.System)

item43.dat2$Heating.System.Ind <- item43.dat2$Primary.Heating.System
item43.dat2$Heating.System.Ind[which(item43.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item43.dat2$Heating.System.Ind[which(item43.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"

item43.dat3 <- unique(data.frame("CK_Cadmus_ID" = item43.dat2$CK_Cadmus_ID
                          ,"Heating_Type" = item43.dat2$Generic
                          ,"Primary_Secondary" = item43.dat2$Heating.System.Ind))

item43.dat4 <- left_join(rbsa.dat, item43.dat3, by = "CK_Cadmus_ID")

item43.dat5 <- item43.dat4[which(item43.dat4$Primary_Secondary == "Primary Heating System"),]
length(unique(item43.dat5$CK_Cadmus_ID)) #544
item43.dat5$count <- 1

#summarize across heating types to get totals
item43.tmp1 <- summarise(group_by(item43.dat5, BuildingType)
                         ,PrimaryCount_total = sum(count)) 

#summarize within heating types
item43.tmp2 <- summarise(group_by(item43.dat5, BuildingType, Heating_Type)
                         ,SampleSize   = length(unique(CK_Cadmus_ID)) 
                         ,PrimaryCount_type = sum(count)) 

#join across and within heating type summaries
item43.final <- left_join(item43.tmp2, item43.tmp1, by = c("BuildingType"))
item43.final$Percent <- item43.final$PrimaryCount_type / item43.final$PrimaryCount_total
item43.final$SE <- sqrt(item43.final$Percent * (1 - item43.final$Percent) / item43.final$SampleSize)


###########Make into table
item43.table <- item43.final[which(colnames(item43.final) %in% c("BuildingType"
                                                                 ,"Heating_Type"
                                                                 ,"Percent"
                                                                 ,"SE"
                                                                 ,"SampleSize"))]







#############################################################################################
#Item 44: DISTRIBUTION OF FUEL CHOICE FOR PRIMARY HEATING SYSTEMS BY STATE  (SF table 51)
#############################################################################################
item44.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item44.dat1 <- item44.dat[which(item44.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item44.dat2 <- item44.dat1[which(item44.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(item44.dat2$Primary.Heating.System)

item44.dat2$Heating.System.Ind <- item44.dat2$Primary.Heating.System
item44.dat2$Heating.System.Ind[which(item44.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item44.dat2$Heating.System.Ind[which(item44.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"
item44.dat2$Heating.Fuel[which(item44.dat2$Heating.Fuel == "Natural gas")] <- "Natural Gas"


item44.dat3 <- unique(data.frame("CK_Cadmus_ID" = item44.dat2$CK_Cadmus_ID
                                 ,"Heating_Type" = item44.dat2$Generic
                                 ,"Heating_Fuel" = item44.dat2$Heating.Fuel
                                 ,"Primary_Secondary" = item44.dat2$Heating.System.Ind))

item44.dat4 <- left_join(rbsa.dat, item44.dat3, by = "CK_Cadmus_ID")

item44.dat5 <- item44.dat4[which(item44.dat4$Primary_Secondary == "Primary Heating System"),]
length(unique(item44.dat5$CK_Cadmus_ID)) #544
item44.dat5$count <- 1


#summarize across fuel types to get totals by building type and state
item44.tmp1 <- summarise(group_by(item44.dat5, BuildingType, State)
                         ,FuelCount_total = sum(count)) 

#summarize within fuel types by building type and state
item44.tmp2 <- summarise(group_by(item44.dat5, BuildingType, State, Heating_Fuel)
                         ,SampleSize   = length(unique(CK_Cadmus_ID)) 
                         ,FuelCount_type = sum(count))
#left join tmp1 and tmp 2
item44.merge1 <- left_join(item44.tmp2, item44.tmp1, by = c("BuildingType", "State"))


#summarize across fuel types to get totals by building type across states
item44.tmp3 <- summarise(group_by(item44.dat5, BuildingType)
                         ,State = "Region"
                         ,FuelCount_total = sum(count)) 

#summarize within fuel types by building type across states
item44.tmp4 <- summarise(group_by(item44.dat5, BuildingType, Heating_Fuel)
                         ,State = "Region"
                         ,SampleSize   = length(unique(CK_Cadmus_ID))
                         ,FuelCount_type = sum(count)) 
#left join tmp3 and tmp4
item44.merge2 <- left_join(item44.tmp4, item44.tmp3, by = c("BuildingType", "State"))


# rbind merg1 and merge2
item44.final <- rbind.data.frame(item44.merge1, item44.merge2, stringsAsFactors = F)
item44.final <- item44.final[which(item44.final$BuildingType == "Single Family"),]

item44.final$Percent <- item44.final$FuelCount_type / item44.final$FuelCount_total
item44.final$SE <- sqrt(item44.final$Percent * (1 - item44.final$Percent) / item44.final$FuelCount_type)


###########Make into table
item44.table <- item44.final[which(colnames(item44.final) %in% c("BuildingType"
                                                                 ,"State"
                                                                 ,"Heating_Fuel"
                                                                 ,"Percent"
                                                                 ,"SE"
                                                                 ,"SampleSize"))]
detach(package:reshape2)
library(data.table)
item44.table1 <- dcast(setDT(item44.table)
                      , formula = BuildingType + Heating_Fuel ~ State
                      , value.var = c("SampleSize", "Percent", "SE"))






#############################################################################################
#Item 45: DISTRIBUTION OF SECONDARY HEATING SYSTEMS (SF table 52)
#############################################################################################
item45.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item45.dat1 <- item45.dat[which(item45.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item45.dat2 <- item45.dat1[which(item45.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(item45.dat2$Primary.Heating.System)

item45.dat2$Heating.System.Ind <- item45.dat2$Primary.Heating.System
item45.dat2$Heating.System.Ind[which(item45.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item45.dat2$Heating.System.Ind[which(item45.dat2$Primary.Heating.System ==  "No")] <- "Secondary Heating System"

item45.dat3 <- unique(data.frame("CK_Cadmus_ID" = item45.dat2$CK_Cadmus_ID
                                 ,"Heating_Type" = item45.dat2$Generic
                                 ,"Primary_Secondary" = item45.dat2$Heating.System.Ind))

item45.dat4 <- left_join(rbsa.dat, item45.dat3, by = "CK_Cadmus_ID")

item45.dat5 <- item45.dat4[which(item45.dat4$Primary_Secondary == "Secondary Heating System"),]
length(unique(item45.dat5$CK_Cadmus_ID)) #311
item45.dat5$count <- 1

#summarize across heating types to get totals
item45.tmp1 <- summarise(group_by(item45.dat5, BuildingType)
                         ,PrimaryCount_total = sum(count)) 

#summarize within heating types
item45.tmp2 <- summarise(group_by(item45.dat5, BuildingType, Heating_Type)
                         ,SampleSize   = length(unique(CK_Cadmus_ID)) 
                         ,PrimaryCount_type = sum(count)) 

#join across and within heating type summaries
item45.final <- left_join(item45.tmp2, item45.tmp1, by = c("BuildingType"))
item45.final$Percent <- item45.final$PrimaryCount_type / item45.final$PrimaryCount_total
item45.final$SE <- sqrt(item45.final$Percent * (1 - item45.final$Percent) / item45.final$SampleSize)


###########Make into table
item45.table <- item45.final[which(colnames(item45.final) %in% c("BuildingType"
                                                                 ,"Heating_Type"
                                                                 ,"Percent"
                                                                 ,"SE"
                                                                 ,"SampleSize"))]







#############################################################################################
#Item 44: DISTRIBUTION OF FUEL CHOICE FOR PRIMARY HEATING SYSTEMS BY STATE  (SF table 51)
#############################################################################################
item46.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item46.dat1 <- item46.dat[which(item46.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item46.dat2 <- item46.dat1[which(item46.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(item46.dat2$Primary.Heating.System)

item46.dat2$Heating.System.Ind <- item46.dat2$Primary.Heating.System
item46.dat2$Heating.System.Ind[which(item46.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item46.dat2$Heating.System.Ind[which(item46.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"
item46.dat2$Heating.Fuel[which(item46.dat2$Heating.Fuel == "Natural gas")] <- "Natural Gas"


item46.dat3 <- unique(data.frame("CK_Cadmus_ID" = item46.dat2$CK_Cadmus_ID
                                 ,"Heating_Type" = item46.dat2$Generic
                                 ,"Heating_Fuel" = item46.dat2$Heating.Fuel
                                 ,"Primary_Secondary" = item46.dat2$Heating.System.Ind))

item46.dat4 <- left_join(rbsa.dat, item46.dat3, by = "CK_Cadmus_ID")

item46.dat5 <- item46.dat4[which(item46.dat4$Primary_Secondary == "Secondary Heating System"),]
length(unique(item46.dat5$CK_Cadmus_ID)) #311
item46.dat5$count <- 1


#summarize across fuel types to get totals by building type and state
item46.tmp1 <- summarise(group_by(item46.dat5, BuildingType, State)
                         ,FuelCount_total = sum(count)) 

#summarize within fuel types by building type and state
item46.tmp2 <- summarise(group_by(item46.dat5, BuildingType, State, Heating_Fuel)
                         ,SampleSize   = length(unique(CK_Cadmus_ID)) 
                         ,FuelCount_type = sum(count))
#left join tmp1 and tmp 2
item46.merge1 <- left_join(item46.tmp2, item46.tmp1, by = c("BuildingType", "State"))


#summarize across fuel types to get totals by building type across states
item46.tmp3 <- summarise(group_by(item46.dat5, BuildingType)
                         ,State = "Region"
                         ,FuelCount_total = sum(count)) 

#summarize within fuel types by building type across states
item46.tmp4 <- summarise(group_by(item46.dat5, BuildingType, Heating_Fuel)
                         ,State = "Region"
                         ,SampleSize   = length(unique(CK_Cadmus_ID))
                         ,FuelCount_type = sum(count)) 
#left join tmp3 and tmp4
item46.merge2 <- left_join(item46.tmp4, item46.tmp3, by = c("BuildingType", "State"))


# rbind merg1 and merge2
item46.final <- rbind.data.frame(item46.merge1, item46.merge2, stringsAsFactors = F)
item46.final <- item46.final[which(item46.final$BuildingType %in% c("Single Family", "Manufactured")),]

item46.final$Percent <- item46.final$FuelCount_type / item46.final$FuelCount_total
item46.final$SE <- sqrt(item46.final$Percent * (1 - item46.final$Percent) / item46.final$FuelCount_type)


###########Make into table
item46.table <- item46.final[which(colnames(item46.final) %in% c("BuildingType"
                                                                 ,"State"
                                                                 ,"Heating_Fuel"
                                                                 ,"Percent"
                                                                 ,"SE"
                                                                 ,"SampleSize"))]
detach(package:reshape2)
library(data.table)
item46.table1 <- dcast(setDT(item46.table)
                       , formula = BuildingType + Heating_Fuel ~ State
                       , value.var = c("SampleSize", "Percent", "SE"))

