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






#############################################################################################
#Item 43: DISTRIBUTION OF PRIMARY HEATING SYSTEMS (SF table 50, MF table 35)
#############################################################################################
item43.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"Primary.Heating.System"
                                                                   ,"Secondary.Heating.System"))]
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
