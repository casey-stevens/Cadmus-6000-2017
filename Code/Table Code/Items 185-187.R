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
#Item 185: DISTRIBUTION OF PRIMARY HEATING SYSTEMS BY HEATING SYSTEM TYPE (MH table 32)
#############################################################################################
item185.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item185.dat1 <- item185.dat[which(item185.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item185.dat2 <- unique(item185.dat1[which(item185.dat1$Primary.Heating.System %in% c("Yes")),])
#check uniques
unique(item185.dat2$Primary.Heating.System)

item185.dat3 <- left_join(rbsa.dat, item185.dat2, by = "CK_Cadmus_ID")

# #check with Rietz to see how he would like to handle the fact that some sites have multiple primary heating
# dup.ind <- unique(item185.dat2$CK_Cadmus_ID[which(duplicated(item185.dat2$CK_Cadmus_ID))])
# 
# item185.tmp <- item185.dat2[which(item185.dat2$CK_Cadmus_ID %in% dup.ind),]

item185.dat3$count <- 1
#summarize within heating types
item185.tmp1 <- summarise(group_by(item185.dat3, BuildingType, Generic)
                         ,SampleSize   = length(unique(CK_Cadmus_ID)) 
                         ,Count = sum(count)) 

#summarize across heating types to get totals
item185.tmp2 <- summarise(group_by(item185.dat3, BuildingType)
                         ,Generic = "Total"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count)) 

#join across and within heating type summaries
item185.merge <- rbind.data.frame(item185.tmp1, item185.tmp2, stringsAsFactors = F)

item185.final <- left_join(item185.merge, item185.tmp2, by = "BuildingType")
colnames(item185.final) <- c("BuildingType"
                            ,"Heating.Type"
                            ,"SampleSize"
                            ,"Count"
                            ,"Remove"
                            ,"Remove"
                            ,"Total.Count")

item185.final$Percent <- item185.final$Count / item185.final$Total.Count
item185.final$SE <- sqrt(item185.final$Percent * (1 - item185.final$Percent) / item185.final$SampleSize)


item185.table <- data.frame("BuildingType" = item185.final$BuildingType
                           ,"Heating.Type" = item185.final$Heating.Type
                           ,"Percent" = item185.final$Percent
                           ,"SE" = item185.final$SE
                           ,"SampleSize" = item185.final$SampleSize)
item185.table1 <- item185.table[which(item185.table$BuildingType %in% c("Manufactured")),]

item185.table2 <- item185.table1[which(!(is.na(item185.table1$Heating.Type))),]
