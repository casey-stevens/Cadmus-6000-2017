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
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))





#############################################################################################
#Item : DISTRIBUTION OF ELECTRICALLY HEATED HOMES BY VINTAGE AND STATE (SF table 156)
#############################################################################################
#subset to columns needed for analysis
item156.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"Primary.Heating.System"
                                                                    ,"Heating.Fuel"
                                                                    ,""
                                                                    ,""))]

#remove any repeat header rows from exporting
item156.dat1 <- item156.dat[which(item156.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item156.dat2 <- item156.dat1[which(item156.dat1$Primary.Heating.System == "Yes"),]
length(unique(item156.dat2$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item156.dat2$Primary.Heating.System)
item156.dat2$count <- 1

item156.dat3 <- unique(item156.dat2[which(item156.dat2$Heating.Fuel == "Electric"),])

item156.sum1 <- summarise(group_by(item156.dat3, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item156.sum1$Count <- 1
which(duplicated(item156.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(item156.sum1$Heating.Fuel)

item156.dat4 <- left_join(item156.sum1, rbsa.dat, by = "CK_Cadmus_ID")
item156.dat5 <- item156.dat4[which(!(is.na(item156.dat4$HomeYearBuilt_bins))),]
item156.dat6 <- item156.dat5[which(item156.dat5$BuildingType == "Single Family"),]


#summarise by state
#by vintage bins
item156.state1 <- summarise(group_by(item156.dat6, BuildingType, HomeYearBuilt_bins, State)
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(Count))
#across vintage bins
item156.state2 <- summarise(group_by(item156.dat6, BuildingType, State)
                            ,HomeYearBuilt_bins = "All Vintages"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(Count))
#summarise across states
#by vintage bins
item156.region1 <- summarise(group_by(item156.dat6, BuildingType, HomeYearBuilt_bins)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(Count))
#across vintage bins
item156.region2 <- summarise(group_by(item156.dat6, BuildingType)
                            ,HomeYearBuilt_bins = "All Vintages"
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(Count))
#row bind state information together
item156.merge <- rbind.data.frame(item156.state1, item156.state2, item156.region1, item156.region2, stringsAsFactors = F)

item156.total.counts <-  rbind.data.frame(item156.state2, item156.region2, stringsAsFactors = F)
item156.total.counts <- item156.total.counts[which(colnames(item156.total.counts) %in% c("BuildingType", "State", "Count"))]

#merge on total counts
item156.final <- left_join(item156.merge, item156.total.counts, by = c("BuildingType", "State"))
colnames(item156.final) <- c("BuildingType", "Housing.Vintage", "State", "SampleSize", "Count", "Total.Count")

#calculate percent and SE
item156.final$Percent <- item156.final$Count / item156.final$Total.Count
item156.final$SE <- sqrt(item156.final$Percent * (1 - item156.final$Percent) / item156.final$SampleSize)

#cast out by state
library(data.table)
item156.cast <- dcast(setDT(item156.final)
                      ,formula = BuildingType + Housing.Vintage ~ State
                      ,value.var = c("Percent", "SE", "SampleSize"))

#put in table format
item156.table <- data.frame("BuildingType" = item156.cast$BuildingType
                            ,"Housing.Vintage" = item156.cast$Housing.Vintage
                            ,"Percent_MT" = item156.cast$Percent_MT
                            ,"SE_MT" = item156.cast$SE_MT
                            # ,"Percent_OR" = item156.cast$Percent_OR
                            # ,"SE_OR" = item156.cast$SE_OR
                            ,"Percent_WA" = item156.cast$Percent_WA
                            ,"SE_WA" = item156.cast$SE_WA
                            ,"Percent_Region" = item156.cast$Percent_Region
                            ,"SE_Region" = item156.cast$SE_Region
                            ,"SampleSize" = item156.cast$SampleSize_Region)

item156.table.final <- item156.table[which(item156.table$BuildingType %in% c("Single Family")),]
