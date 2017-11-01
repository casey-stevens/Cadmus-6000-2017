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

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


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


item43.data <- weightedData(item43.dat5[-which(colnames(item43.dat5) %in% c("Heating_Type"
                                                                            ,"Primary_Secondary"
                                                                            ,"count"))])
item43.data <- left_join(item43.data, item43.dat5[which(colnames(item43.dat5) %in% c("CK_Cadmus_ID"
                                                                                     ,"Heating_Type"
                                                                                     ,"Primary_Secondary"
                                                                                     ,"count"))])

item43.final <- proportions_one_group(CustomerLevelData  = item43.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating_Type'
                                      , total.name       = "Total"
                                      , columnName       = "Primary Heating Systems")

# export table
# SF = Table 50, MH = Table 32
item43.final.SF <- item43.final[which(item43.final$BuildingType == "Single Family"),-1]
item43.final.MH <- item43.final[which(item43.final$BuildingType == "Manufactured"),-1]

exportTable(item43.final.SF, "SF", "Table 50")
exportTable(item43.final.MH, "MH", "Table 32")


# OLD CODE #
# 
# #summarize across heating types to get totals
# item43.tmp1 <- summarise(group_by(item43.dat5, BuildingType)
#                          ,Heating_Type = "Total"
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Count = sum(count)) 
# 
# #summarize within heating types
# item43.tmp2 <- summarise(group_by(item43.dat5, BuildingType, Heating_Type)
#                          ,SampleSize   = length(unique(CK_Cadmus_ID)) 
#                          ,Count = sum(count)) 
# 
# #join across and within heating type summaries
# item43.merge <- rbind.data.frame(item43.tmp2, item43.tmp1, stringsAsFactors = F)
# 
# item43.final <- left_join(item43.merge, item43.tmp1, by = "BuildingType")
# colnames(item43.final) <- c("BuildingType"
#                             ,"Heating.Type"
#                             ,"SampleSize"
#                             ,"Count"
#                             ,"Remove"
#                             ,"Remove"
#                             ,"Total.Count")
# 
# item43.final$Percent <- item43.final$Count / item43.final$Total.Count
# item43.final$SE <- sqrt(item43.final$Percent * (1 - item43.final$Percent) / item43.final$SampleSize)
# 
# 
# item43.table <- data.frame("BuildingType" = item43.final$BuildingType
#                            ,"Heating.Type" = item43.final$Heating.Type
#                            ,"Percent" = item43.final$Percent
#                            ,"SE" = item43.final$SE
#                            ,"SampleSize" = item43.final$SampleSize)
# item43.table1 <- item43.table[which(item43.table$BuildingType %in% c("Single Family", "Manufactured")),]



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
length(unique(item44.dat5$CK_Cadmus_ID))
item44.dat5$count <- 1

item44.data <- weightedData(item44.dat5[-which(colnames(item44.dat5) %in% c("Heating_Type"
                                                                            ,"Heating_Fuel"
                                                                            ,"Primary_Secondary"
                                                                            ,"count"))])

item44.data <- left_join(item44.data, item44.dat5[which(colnames(item44.dat5) %in% c("CK_Cadmus_ID"
                                                                                     ,"Heating_Type"
                                                                                     ,"Heating_Fuel"
                                                                                     ,"Primary_Secondary"
                                                                                     ,"count"))])

item44.final <- proportionRowsAndColumns1(CustomerLevelData = item44.data
                                      , valueVariable       = 'count'
                                      , columnVariable      = 'State'
                                      , rowVariable         = 'Heating_Fuel'
                                      , aggregateColumnName = 'Fuel Choice (Primary System)'
                                      , totalRow            = TRUE
                                      , weighted            = TRUE)

# I think this still has to be casted, skip for now
# export table
# SF = Table 51, MH = Table 33
item44.final.SF <- item44.final[which(item44.final$BuildingType == "Single Family"),-1]
item44.final.MH <- item44.final[which(item44.final$BuildingType == "Manufactured"),-1]

exportTable(item44.final.SF, "SF", "Table 51")
exportTable(item44.final.MH, "MH", "Table 33")




# OLD CODE #
# 
# #summarize across fuel types to get totals by building type and state
# item44.tmp1 <- summarise(group_by(item44.dat5, BuildingType, State)
#                          ,Heating_Fuel = "All Fuel Types"
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Count = sum(count)) 
# 
# #summarize within fuel types by building type and state
# item44.tmp2 <- summarise(group_by(item44.dat5, BuildingType, State, Heating_Fuel)
#                          ,SampleSize   = length(unique(CK_Cadmus_ID)) 
#                          ,Count = sum(count))
# #row bind
# item44.merge1 <- rbind.data.frame(item44.tmp2, item44.tmp1, stringsAsFactors = F)
# 
# 
# #summarize across fuel types to get totals by building type across states
# item44.tmp3 <- summarise(group_by(item44.dat5, BuildingType)
#                          ,Heating_Fuel = "All Fuel Types"
#                          ,State = "Region"
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Count = sum(count)) 
# 
# #summarize within fuel types by building type across states
# item44.tmp4 <- summarise(group_by(item44.dat5, BuildingType, Heating_Fuel)
#                          ,State = "Region"
#                          ,SampleSize   = length(unique(CK_Cadmus_ID))
#                          ,Count = sum(count)) 
# # row bind
# item44.merge2 <- rbind.data.frame(item44.tmp4, item44.tmp3, stringsAsFactors = F)
# 
# #combine state and region information
# item44.merge3 <- rbind.data.frame(item44.merge1, item44.merge2, stringsAsFactors = F)
# 
# 
# itemm44.total.counts <- rbind.data.frame(item44.tmp1, item44.tmp3, stringsAsFactors = F)
# itemm44.total.counts <- itemm44.total.counts[which(colnames(itemm44.total.counts) %in% c("BuildingType", "State","Count"))]
# 
# item44.final <- left_join(item44.merge3, itemm44.total.counts, by = c("BuildingType", "State"))
# colnames(item44.final) <- c("BuildingType", "State", "Heating_Fuel", "SampleSize", "Count", "Total.Count")
# 
# item44.final$Percent <- item44.final$Count / item44.final$Total.Count
# item44.final$SE <- sqrt(item44.final$Percent * (1 - item44.final$Percent) / item44.final$SampleSize)
# 
# 
# ###########Make into table
# detach(package:reshape2)
# library(data.table)
# item44.cast <- dcast(setDT(item44.final)
#                       , formula = BuildingType + Heating_Fuel ~ State
#                       , value.var = c("Percent", "SE", "SampleSize"))
# 
# item44.table <- data.frame("BuildingType" = item44.cast$BuildingType
#                             ,"Heating.Fuel" = item44.cast$Heating_Fuel
#                             ,"Percent_MT" = item44.cast$Percent_MT
#                             ,"SE_MT" = item44.cast$SE_MT
#                             ,"Percent_WA" = item44.cast$Percent_WA
#                             ,"SE_WA" = item44.cast$SE_WA
#                             ,"Percent_Region" = item44.cast$Percent_Region
#                             ,"SE_Region" = item44.cast$SE_Region
#                             ,"SampleSize" = item44.cast$SampleSize_Region)
# 
# 
# item44.table1 <- item44.table[which(item44.table$BuildingType %in% c("Single Family", "Manufactured")),]
# 
# item44.table2 <- item44.table1[which(!(is.na(item44.table1$Heating.Fuel))),]
# 



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
                         ,Heating_Type = "Total"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count)) 

#summarize within heating types
item45.tmp2 <- summarise(group_by(item45.dat5, BuildingType, Heating_Type)
                         ,SampleSize   = length(unique(CK_Cadmus_ID)) 
                         ,Count = sum(count)) 

#join across and within heating type summaries
item45.merge <- rbind.data.frame(item45.tmp2, item45.tmp1, stringsAsFactors = F)

item45.final <- left_join(item45.merge, item45.tmp1, by = "BuildingType")
colnames(item45.final) <- c("BuildingType"
                            ,"Heating.Type"
                            ,"SampleSize"
                            ,"Count"
                            ,"Remove"
                            ,"Remove"
                            ,"Total.Count")

item45.final$Percent <- item45.final$Count / item45.final$Total.Count
item45.final$SE <- sqrt(item45.final$Percent * (1 - item45.final$Percent) / item45.final$SampleSize)


item45.table <- data.frame("BuildingType" = item45.final$BuildingType
                           ,"Heating.Type" = item45.final$Heating.Type
                           ,"Percent" = item45.final$Percent
                           ,"SE" = item45.final$SE
                           ,"SampleSize" = item45.final$SampleSize)
item45.table1 <- item45.table[which(item45.table$BuildingType %in% c("Single Family", "Manufactured")),]







#############################################################################################
#Item 46: DISTRIBUTION OF FUEL CHOICE FOR SECONDARY HEATING SYSTEMS BY STATE  (SF table 51)
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
                         ,Heating_Fuel = "All Fuel Types"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count)) 

#summarize within fuel types by building type and state
item46.tmp2 <- summarise(group_by(item46.dat5, BuildingType, State, Heating_Fuel)
                         ,SampleSize   = length(unique(CK_Cadmus_ID)) 
                         ,Count = sum(count))
#row bind
item46.merge1 <- rbind.data.frame(item46.tmp2, item46.tmp1, stringsAsFactors = F)


#summarize across fuel types to get totals by building type across states
item46.tmp3 <- summarise(group_by(item46.dat5, BuildingType)
                         ,Heating_Fuel = "All Fuel Types"
                         ,State = "Region"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count)) 

#summarize within fuel types by building type across states
item46.tmp4 <- summarise(group_by(item46.dat5, BuildingType, Heating_Fuel)
                         ,State = "Region"
                         ,SampleSize   = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count)) 
# row bind
item46.merge2 <- rbind.data.frame(item46.tmp4, item46.tmp3, stringsAsFactors = F)

#combine state and region information
item46.merge3 <- rbind.data.frame(item46.merge1, item46.merge2, stringsAsFactors = F)


itemm46.total.counts <- rbind.data.frame(item46.tmp1, item46.tmp3, stringsAsFactors = F)
itemm46.total.counts <- itemm46.total.counts[which(colnames(itemm46.total.counts) %in% c("BuildingType", "State","Count"))]

item46.final <- left_join(item46.merge3, itemm46.total.counts, by = c("BuildingType", "State"))
colnames(item46.final) <- c("BuildingType", "State", "Heating_Fuel", "SampleSize", "Count", "Total.Count")

item46.final$Percent <- item46.final$Count / item46.final$Total.Count
item46.final$SE <- sqrt(item46.final$Percent * (1 - item46.final$Percent) / item46.final$SampleSize)


###########Make into table
detach(package:reshape2)
library(data.table)
item46.cast <- dcast(setDT(item46.final)
                     , formula = BuildingType + Heating_Fuel ~ State
                     , value.var = c("Percent", "SE", "SampleSize"))

item46.table <- data.frame("BuildingType" = item46.cast$BuildingType
                           ,"Heating.Fuel" = item46.cast$Heating_Fuel
                           ,"Percent_MT" = item46.cast$Percent_MT
                           ,"SE_MT" = item46.cast$SE_MT
                           ,"Percent_WA" = item46.cast$Percent_WA
                           ,"SE_WA" = item46.cast$SE_WA
                           ,"Percent_Region" = item46.cast$Percent_Region
                           ,"SE_Region" = item46.cast$SE_Region
                           ,"SampleSize" = item46.cast$SampleSize_Region)


item46.table1 <- item46.table[which(item46.table$BuildingType %in% c("Single Family")),]

item46.table2 <- item46.table1[which(!(is.na(item46.table1$Heating.Fuel))),]
