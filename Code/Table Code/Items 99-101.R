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

##  Create "Not In" operator
"%notin%" <- Negate("%in%")

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
#Item 99: DISTRIBUTION OF ALL WATER HEATER LOCATIONS BY SPACE HEATING FUEL TYPE (SF table 106, MH table 86)
#############################################################################################
#subset to columns needed for analysis
item99.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,"DHW.Location"
                                                                   ,"Primary.Heating.System"
                                                                   ,"Heating.Fuel"))]
item99.dat$count <- 1

item99.dat0 <- item99.dat[which(item99.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item99.dat1 <- left_join(item99.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item99.ind <- item99.dat1$CK_Cadmus_ID[grep("Water Heater",item99.dat1$Generic)]
item99.dat2 <- item99.dat1[which(item99.dat1$CK_Cadmus_ID %in% item99.ind),]

item99.dat3 <- item99.dat2
unique(item99.dat3$DHW.Location)
unique(item99.dat2$Primary.Heating.System)

item99.ind2 <- item99.dat1$CK_Cadmus_ID[grep("Yes|yes",item99.dat1$Primary.Heating.System)]
item99.dat3 <- item99.dat2[which(item99.dat2$CK_Cadmus_ID %in% item99.ind2),]

#clean heating fuel type
unique(item99.dat3$Heating.Fuel)
item99.dat3$Heating.Fuel[grep("Gas|gas",item99.dat3$Heating.Fuel)] <- "Gas"
item99.dat3$Heating.Fuel[grep("unk|Unk|N/A|Other|from",item99.dat3$Heating.Fuel)] <- NA
item99.dat3$Heating.Fuel[grep("oil|Oil|kero|Kero",item99.dat3$Heating.Fuel)] <- "Oil"
item99.dat3$Heating.Fuel[grep("Elect|elect",item99.dat3$Heating.Fuel)] <- "Electric"
item99.dat3$Heating.Fuel[which(item99.dat3$Heating.Fuel == "Wood (pellets)")] <- "Pellets"
item99.dat3$Heating.Fuel[which(item99.dat3$Heating.Fuel == "Wood (cord)")] <- "Wood"

item99.dat4 <- item99.dat3[which(!is.na(item99.dat3$Heating.Fuel)),]

unique(item99.dat4$Primary.Heating.System)

#summarise for primary heating system
item99.heat <- item99.dat4[which(item99.dat4$Primary.Heating.System == "Yes"),]
item99.sum1 <- summarise(group_by(item99.heat, CK_Cadmus_ID, BuildingType, Heating.Fuel)
                         ,Count = sum(count))

item99.sum1$Count <- 1
unique(item99.sum1$Heating.Fuel)

dup.ind <- item99.sum1$CK_Cadmus_ID[which(duplicated(item99.sum1$CK_Cadmus_ID))]
IDs.remove <- item99.sum1[which(item99.sum1$CK_Cadmus_ID %in% dup.ind & item99.sum1$Heating.Fuel == "Electric"),]

item99.sum2 <- item99.sum1[which(!(item99.sum1$CK_Cadmus_ID %in% IDs.remove$CK_Cadmus_ID & item99.sum1$Heating.Fuel %in% IDs.remove$Heating.Fuel)),]
item99.sum2$CK_Cadmus_ID[which(duplicated(item99.sum2$CK_Cadmus_ID))]

## correct duplicated fuel types
item99.sum2$Heating.Fuel[which(item99.sum2$CK_Cadmus_ID == "BPS22084 OS BPA")] <- "Gas"
item99.sum2$Heating.Fuel[which(item99.sum2$CK_Cadmus_ID == "MS0435")] <- "Propane"
item99.sum2$Heating.Fuel[which(item99.sum2$CK_Cadmus_ID == "MS1998")] <- "Propane"
item99.sum2$Heating.Fuel[which(item99.sum2$CK_Cadmus_ID == "MS3167")] <- "Propane"
item99.sum2$Heating.Fuel[which(item99.sum2$CK_Cadmus_ID == "RBS60190 OS BPA")] <- "Pellets"
item99.sum2$Heating.Fuel[which(item99.sum2$CK_Cadmus_ID == "RBS73757 OS BPA")] <- "Pellets"

item99.heat.final <- unique(item99.sum2)
#qc
stopifnot(nrow(item99.heat.final) == length(unique(item99.heat.final$CK_Cadmus_ID)))

# summarise for water heaters
item99.WH <- item99.dat3[which(item99.dat3$Generic %in% c("Storage Water Heater"
                                                          ,"Instantaneous Water Heater")),]

item99.WH.sum1 <- summarise(group_by(item99.WH, CK_Cadmus_ID, BuildingType, DHW.Location)
                            ,Count = sum(count))


item99.merge1 <- left_join(item99.heat.final, item99.WH.sum1, by = c("CK_Cadmus_ID", "BuildingType"))
item99.merge2 <- item99.merge1[which(!(is.na(item99.merge1$Heating.Fuel))),]
colnames(item99.merge2) <- c("CK_Cadmus_ID", "BuildingType", "Heating.Fuel","Heat.Count", "DHW.Location", "DHW.Count")


item99.join <- left_join(rbsa.dat, item99.merge2)
item99.join <- item99.join[which(!is.na(item99.join$Heating.Fuel)),]
item99.join1 <- item99.join[which(item99.join$DHW.Location != "Unknown"),]

item99.join1$DHW.Location[grep("Crawl",item99.join1$DHW.Location)] <- "Crawlspace"
item99.join1$DHW.Location[grep("In building",item99.join1$DHW.Location)] <- "Main House"

item99.join1$DHW.Location[which(item99.join1$DHW.Location %notin% c("Crawlspace"
                                                                  ,"Basement"
                                                                  ,"Garage"
                                                                  ,"Main House"))] <- "Other"



################################################
# Adding pop and sample sizes for weights
################################################
item99.data <- weightedData(item99.join1[-which(colnames(item99.join1) %in% c("Heating.Fuel"               
                                                                            ,"DHW.Count"
                                                                            ,"DHW.Location"
                                                                            ,"Heat.Count"))])
item99.data <- left_join(item99.data, item99.join1[which(colnames(item99.join1) %in% c("CK_Cadmus_ID"
                                                                                     ,"Heating.Fuel"               
                                                                                     ,"DHW.Count"
                                                                                     ,"DHW.Location"
                                                                                     ,"Heat.Count"))])
#######################
# Weighted Analysis
#######################
item99.final <- proportionRowsAndColumns1(CustomerLevelData = item99.data
                                          ,valueVariable    = 'DHW.Count'
                                          ,columnVariable   = 'Heating.Fuel'
                                          ,rowVariable      = 'DHW.Location'
                                          ,aggregateColumnName = "Remove")
item99.final <- item99.final[which(item99.final$Heating.Fuel != "Remove"),]

item99.all.fuels <- proportions_one_group(CustomerLevelData = item99.data
                                          ,valueVariable = "DHW.Count"
                                          ,groupingVariable = "DHW.Location"
                                          ,total.name = "All Fuels"
                                          ,columnName = "Heating.Fuel"
                                          ,weighted = TRUE)

item99.final <- rbind.data.frame(item99.final, item99.all.fuels)

item99.cast <- dcast(setDT(item99.final)
                      ,formula = BuildingType + DHW.Location ~ Heating.Fuel
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N"))

item99.table <- data.frame("BuildingType"         = item99.cast$BuildingType
                           ,"DHW.Location"        = item99.cast$DHW.Location
                           ,"Percent.Electric"    = item99.cast$w.percent_Electric
                           ,"SE.Electric"         = item99.cast$w.SE_Electric
                           ,"Count.Electric"      = item99.cast$count_Electric
                           ,"Percent.Natural.Gas" = item99.cast$w.percent_Gas
                           ,"SE.Natural.Gas"      = item99.cast$w.SE_Gas
                           ,"Count.Gas"           = item99.cast$count_Gas
                           ,"Percent.Oil"         = item99.cast$w.percent_Oil
                           ,"SE.Oil"              = item99.cast$w.SE_Oil
                           ,"Count.Oil"           = item99.cast$count_Oil
                           ,"Percent.Pellets"     = item99.cast$w.percent_Pellets
                           ,"SE.Pellets"          = item99.cast$w.SE_Pellets
                           ,"Count.Pellets"       = item99.cast$count_Pellets
                           ,"Percent.Propane"     = item99.cast$w.percent_Propane
                           ,"SE.Propane"          = item99.cast$w.SE_Propane
                           ,"Count.Propane"       = item99.cast$count_Propane
                           ,"Percent.Wood"        = item99.cast$w.percent_Wood
                           ,"SE.Wood"             = item99.cast$w.SE_Wood
                           ,"Count.Wood"          = item99.cast$count_Wood
                           ,"Percent.All.Heating.Fuel.Types" = item99.cast$`w.percent_All Fuels`
                           ,"SE.All.Heating.Fuel.Types"      = item99.cast$`w.SE_All Fuels`
                           ,"Count.All.Heating.Fuel.Types"   = item99.cast$`count_All Fuels`
                           ,"n"                              = item99.cast$`n_All Fuels`)

item99.final.SF <- item99.table[which(item99.table$BuildingType == "Single Family")
                                ,-which(colnames(item99.table) %in% c("BuildingType"))]
item99.final.MH <- item99.table[which(item99.table$BuildingType == "Manufactured")
                                ,-which(colnames(item99.table) %in% c("BuildingType"))]

exportTable(item99.final.SF, "SF", "Table 106", weighted = TRUE)
exportTable(item99.final.MH, "MH", "Table 86", weighted = TRUE)



#######################
# Weighted Analysis
#######################
item99.final <- proportions_two_groups_unweighted(CustomerLevelData = item99.data
                                          ,valueVariable    = 'DHW.Count'
                                          ,columnVariable   = 'Heating.Fuel'
                                          ,rowVariable      = 'DHW.Location'
                                          ,aggregateColumnName = "Remove")
item99.final <- item99.final[which(item99.final$Heating.Fuel != "Remove"),]

item99.all.fuels <- proportions_one_group(CustomerLevelData = item99.data
                                          ,valueVariable = "DHW.Count"
                                          ,groupingVariable = "DHW.Location"
                                          ,total.name = "All Fuels"
                                          ,columnName = "Heating.Fuel"
                                          ,weighted = FALSE)

item99.final <- rbind.data.frame(item99.final, item99.all.fuels)

item99.cast <- dcast(setDT(item99.final)
                     ,formula = BuildingType + DHW.Location ~ Heating.Fuel
                     ,value.var = c("Percent", "SE", "Count", "SampleSize"))

item99.table <- data.frame("BuildingType"         = item99.cast$BuildingType
                           ,"DHW.Location"        = item99.cast$DHW.Location
                           ,"Percent.Electric"    = item99.cast$Percent_Electric
                           ,"SE.Electric"         = item99.cast$SE_Electric
                           ,"Count.Electric"      = item99.cast$Count_Electric
                           ,"Percent.Natural.Gas" = item99.cast$Percent_Gas
                           ,"SE.Natural.Gas"      = item99.cast$SE_Gas
                           ,"Count.Gas"           = item99.cast$Count_Gas
                           ,"Percent.Oil"         = item99.cast$Percent_Oil
                           ,"SE.Oil"              = item99.cast$SE_Oil
                           ,"Count.Oil"           = item99.cast$Count_Oil
                           ,"Percent.Pellets"     = item99.cast$Percent_Pellets
                           ,"SE.Pellets"          = item99.cast$SE_Pellets
                           ,"Count.Pellets"       = item99.cast$Count_Pellets
                           ,"Percent.Propane"     = item99.cast$Percent_Propane
                           ,"SE.Propane"          = item99.cast$SE_Propane
                           ,"Count.Propane"       = item99.cast$Count_Propane
                           ,"Percent.Wood"        = item99.cast$Percent_Wood
                           ,"SE.Wood"             = item99.cast$SE_Wood
                           ,"Count.Wood"          = item99.cast$Count_Wood
                           ,"Percent.All.Heating.Fuel.Types" = item99.cast$`Percent_All Fuels`
                           ,"SE.All.Heating.Fuel.Types"      = item99.cast$`SE_All Fuels`
                           ,"Count_All.Heating.Fuel.Types"   = item99.cast$`Count_All Fuels`
                           ,"n"                              = item99.cast$`SampleSize_All Fuels`)

item99.final.SF <- item99.table[which(item99.table$BuildingType == "Single Family")
                                ,-which(colnames(item99.table) %in% c("BuildingType"))]
item99.final.MH <- item99.table[which(item99.table$BuildingType == "Manufactured")
                                ,-which(colnames(item99.table) %in% c("BuildingType"))]

exportTable(item99.final.SF, "SF", "Table 106", weighted = FALSE)
exportTable(item99.final.MH, "MH", "Table 86", weighted = FALSE)







#############################################################################################
#Item 100: DISTRIBUTION OF ELECTRIC WATER HEATER LOCATIONS BY SPACE HEATING FUEL TYPE (SF table 107)
#############################################################################################
#subset to columns needed for analysis
item100.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,"DHW.Location"
                                                                   ,"Primary.Heating.System"
                                                                   ,"Heating.Fuel"))]
item100.dat$count <- 1

item100.dat0 <- item100.dat[which(item100.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item100.dat1 <- left_join(item100.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item100.ind <- item100.dat1$CK_Cadmus_ID[grep("Water Heater",item100.dat1$Generic)]
item100.dat2 <- item100.dat1[which(item100.dat1$CK_Cadmus_ID %in% item100.ind),]

item100.dat2$DHW.Location[grep("storage|Storage",item100.dat2$DHW.Location)] <- "Storage"
item100.dat2$DHW.Location[grep("outside|Outside|exterior|Exterior",item100.dat2$DHW.Location)] <- "Outside"
item100.dat2$DHW.Location[grep("Other|2&3|Mechanical",item100.dat2$DHW.Location)] <- "Other"

unique(item100.dat2$DHW.Location)
unique(item100.dat2$Primary.Heating.System)

item100.ind2 <- item100.dat1$CK_Cadmus_ID[grep("Yes|yes",item100.dat1$Primary.Heating.System)]
item100.dat3 <- item100.dat2[which(item100.dat2$CK_Cadmus_ID %in% item100.ind2),]

#clean heating fuel type
unique(item100.dat2$Heating.Fuel)
item100.dat3$Heating.Fuel[which(item100.dat3$Heating.Fuel == "Natural gas")] <- "Natural Gas"
item100.dat3$Heating.Fuel[which(item100.dat3$Heating.Fuel == "Wood (pellets)")] <- "Pellets"
item100.dat3$Heating.Fuel[which(item100.dat3$Heating.Fuel == "Wood (cord)")] <- "Wood"


#summarise for primary heating system
item100.heat <- item100.dat3[which(item100.dat3$Primary.Heating.System == "Yes"),]
item100.sum1 <- summarise(group_by(item100.heat, CK_Cadmus_ID, BuildingType, Heating.Fuel)
                         ,Count = sum(count))

item100.sum1$Count <- 1

item100.sum2 <- item100.sum1[which(!(is.na(item100.sum1$Heating.Fuel))),]
dup.ind <- item100.sum2$CK_Cadmus_ID[which(duplicated(item100.sum2$CK_Cadmus_ID))]
IDs.remove <- item100.sum2[which(item100.sum2$CK_Cadmus_ID %in% dup.ind & item100.sum2$Heating.Fuel == "Electric"),]

item100.sum3 <- item100.sum2[which(!(item100.sum2$CK_Cadmus_ID %in% IDs.remove$CK_Cadmus_ID & item100.sum2$Heating.Fuel %in% IDs.remove$Heating.Fuel)),]
item100.sum3$CK_Cadmus_ID[which(duplicated(item100.sum3$CK_Cadmus_ID))]
item100.sum3$Heating.Fuel[which(item100.sum3$CK_Cadmus_ID == "SL0582 OS SCL")] <- "Oil"
item100.sum3$Heating.Fuel[which(item100.sum3$CK_Cadmus_ID == "SL1257 OS SCL")] <- "Oil"
item100.sum3$Heating.Fuel[which(item100.sum3$CK_Cadmus_ID == "WH3187")] <- "Propane"

item100.heat.final <- unique(item100.sum3)
#qc
stopifnot(nrow(item100.heat.final) == length(unique(item100.heat.final$CK_Cadmus_ID)))

# summarise for water heaters
item100.WH <- item100.dat3[which(item100.dat3$Generic %in% c("Storage Water Heater"
                                                          ,"Instantaneous Water Heater")),]
item100.WH1 <- item100.WH[which(item100.WH$DHW.Fuel == "Electric"),]

item100.WH.sum1 <- summarise(group_by(item100.WH1, CK_Cadmus_ID, BuildingType, DHW.Location)
                            ,Count = sum(count))


item100.merge1 <- left_join(item100.heat.final, item100.WH.sum1, by = c("CK_Cadmus_ID", "BuildingType"))
item100.merge2 <- item100.merge1[which(!(is.na(item100.merge1$DHW.Location))),]
colnames(item100.merge2) <- c("CK_Cadmus_ID", "BuildingType", "Heating.Fuel","Heat.Count", "DHW.Location", "DHW.Count")

#summarize by heating fuel
#by DHW Locations
item100.tmp1 <- summarise(group_by(item100.merge2, BuildingType, Heating.Fuel, DHW.Location)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Location.Dist = sum(DHW.Count)) 
#across DHW Locations
item100.tmp2 <- summarise(group_by(item100.merge2, BuildingType, Heating.Fuel)
                         ,DHW.Location = "All Locations"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Location.Dist = sum(DHW.Count)) 

#summarize across heating fuels
#by DHW Locations
item100.tmp3 <- summarise(group_by(item100.merge2, BuildingType, DHW.Location)
                         ,Heating.Fuel = "All Heating Fuel Types"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Location.Dist = sum(DHW.Count)) 
#across DHW Locations
item100.tmp4 <- summarise(group_by(item100.merge2, BuildingType)
                         ,DHW.Location = "All Locations"
                         ,Heating.Fuel = "All Heating Fuel Types"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Location.Dist = sum(DHW.Count)) 

item100.merge.final <- rbind.data.frame(item100.tmp1, item100.tmp2, item100.tmp3, item100.tmp4, stringsAsFactors = F)

item100.tot.counts <- rbind.data.frame(item100.tmp2, item100.tmp4, stringsAsFactors = F)


item100.final <- left_join(item100.merge.final, item100.tot.counts, by = c("BuildingType", "Heating.Fuel"))
colnames(item100.final) <- c("BuildingType"
                            , "Heating.Fuel"
                            , "DHW.Location"
                            , "SampleSize"
                            , "Count"
                            , "Remove"
                            , "Remove"
                            , "Total.Count")

item100.final$Percent <- item100.final$Count / item100.final$Total.Count
item100.final$SE <- sqrt(item100.final$Percent * (1 - item100.final$Percent) / item100.final$SampleSize)

library(data.table)
item100.table <- dcast(setDT(item100.final)
                      ,formula = BuildingType + DHW.Location ~ Heating.Fuel
                      ,value.var = c("SampleSize", "Percent","SE"))
item100.table <- data.frame(item100.table)
item100.table1 <- item100.table[-which(colnames(item100.table) %in% c("SampleSize_Electric"
                                                                   ,"SampleSize_Natural.Gas"
                                                                   ,"SampleSize_Oil"
                                                                   ,"SampleSize_Other"
                                                                   ,"SampleSize_Pellets"
                                                                   ,"SampleSize_Propane"
                                                                   ,"SampleSize_Unknown"
                                                                   ,"SampleSize_Wood"))]

item100.table2 <- data.frame("BuildingType" = item100.table1$BuildingType
                            ,"DHW.Location" = item100.table1$DHW.Location
                            ,"Percent.Electric" = item100.table1$Percent_Electric
                            ,"SE.Electric" = item100.table1$SE_Electric
                            ,"Percent.Natural.Gas" = item100.table1$Percent_Natural.Gas
                            ,"SE.Natural.Gas" = item100.table1$SE_Natural.Gas
                            ,"Percent.Oil" = item100.table1$Percent_Oil
                            ,"SE.Oil" = item100.table1$SE_Oil
                            ,"Percent.Pellets" = item100.table1$Percent_Pellets
                            ,"SE.Pellets" = item100.table1$SE_Pellets
                            ,"Percent.Propane" = item100.table1$Percent_Propane
                            ,"SE.Propane" = item100.table1$SE_Propane
                            ,"Percent.Wood" = item100.table1$Percent_Wood
                            ,"SE.Wood" = item100.table1$SE_Wood
                            ,"Percent.Other" = item100.table1$Percent_Other
                            ,"SE.Other" = item100.table1$SE_Other
                            ,"Percent.Unknown" = item100.table1$Percent_Unknown
                            ,"SE.Unknown" = item100.table1$SE_Unknown
                            ,"Percent.All.Heating.Fuel.Types" = item100.table1$Percent_All.Heating.Fuel.Types
                            ,"SE.All.Heating.Fuel.Types" = item100.table1$SE_All.Heating.Fuel.Types
                            ,"SampleSize" = item100.table1$SampleSize_All.Heating.Fuel.Types)

item100.table.final <- item100.table2[which(item100.table2$BuildingType %in% c("Single Family")),]













#############################################################################################
#Item 101: DISTRIBUTION OF GAS WATER HEATER LOCATIONS BY SPACE HEATING FUEL TYPE (SF table 108)
#############################################################################################
#subset to columns needed for analysis
item101.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Location"
                                                                    ,"Primary.Heating.System"
                                                                    ,"Heating.Fuel"))]
item101.dat$count <- 1

item101.dat0 <- item101.dat[which(item101.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item101.dat1 <- left_join(item101.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item101.ind <- item101.dat1$CK_Cadmus_ID[grep("Water Heater",item101.dat1$Generic)]
item101.dat2 <- item101.dat1[which(item101.dat1$CK_Cadmus_ID %in% item101.ind),]

item101.dat2$DHW.Location[grep("storage|Storage",item101.dat2$DHW.Location)] <- "Storage"
item101.dat2$DHW.Location[grep("outside|Outside|exterior|Exterior",item101.dat2$DHW.Location)] <- "Outside"
item101.dat2$DHW.Location[grep("Other|2&3|Mechanical",item101.dat2$DHW.Location)] <- "Other"

unique(item101.dat2$DHW.Location)
unique(item101.dat2$Primary.Heating.System)

item101.ind2 <- item101.dat1$CK_Cadmus_ID[grep("Yes|yes",item101.dat1$Primary.Heating.System)]
item101.dat3 <- item101.dat2[which(item101.dat2$CK_Cadmus_ID %in% item101.ind2),]

#clean heating fuel type
unique(item101.dat2$Heating.Fuel)
item101.dat3$Heating.Fuel[which(item101.dat3$Heating.Fuel == "Natural gas")] <- "Natural Gas"
item101.dat3$Heating.Fuel[which(item101.dat3$Heating.Fuel == "Wood (pellets)")] <- "Pellets"
item101.dat3$Heating.Fuel[which(item101.dat3$Heating.Fuel == "Wood (cord)")] <- "Wood"


#summarise for primary heating system
item101.heat <- item101.dat3[which(item101.dat3$Primary.Heating.System == "Yes"),]
item101.sum1 <- summarise(group_by(item101.heat, CK_Cadmus_ID, BuildingType, Heating.Fuel)
                          ,Count = sum(count))

item101.sum1$Count <- 1

item101.sum2 <- item101.sum1[which(!(is.na(item101.sum1$Heating.Fuel))),]
dup.ind <- item101.sum2$CK_Cadmus_ID[which(duplicated(item101.sum2$CK_Cadmus_ID))]
IDs.remove <- item101.sum2[which(item101.sum2$CK_Cadmus_ID %in% dup.ind & item101.sum2$Heating.Fuel == "Electric"),]

item101.sum3 <- item101.sum2[which(!(item101.sum2$CK_Cadmus_ID %in% IDs.remove$CK_Cadmus_ID & item101.sum2$Heating.Fuel %in% IDs.remove$Heating.Fuel)),]
item101.sum3$CK_Cadmus_ID[which(duplicated(item101.sum3$CK_Cadmus_ID))]
item101.sum3$Heating.Fuel[which(item101.sum3$CK_Cadmus_ID == "SL0582 OS SCL")] <- "Oil"
item101.sum3$Heating.Fuel[which(item101.sum3$CK_Cadmus_ID == "SL1257 OS SCL")] <- "Oil"
item101.sum3$Heating.Fuel[which(item101.sum3$CK_Cadmus_ID == "WH3187")] <- "Propane"

item101.heat.final <- unique(item101.sum3)
#qc
stopifnot(nrow(item101.heat.final) == length(unique(item101.heat.final$CK_Cadmus_ID)))

# summarise for water heaters
item101.WH <- item101.dat3[which(item101.dat3$Generic %in% c("Storage Water Heater"
                                                             ,"Instantaneous Water Heater")),]
item101.WH1 <- item101.WH[which(item101.WH$DHW.Fuel == "Natural Gas"),]

item101.WH.sum1 <- summarise(group_by(item101.WH1, CK_Cadmus_ID, BuildingType, DHW.Location)
                             ,Count = sum(count))


item101.merge1 <- left_join(item101.heat.final, item101.WH.sum1, by = c("CK_Cadmus_ID", "BuildingType"))
item101.merge2 <- item101.merge1[which(!(is.na(item101.merge1$DHW.Location))),]
colnames(item101.merge2) <- c("CK_Cadmus_ID", "BuildingType", "Heating.Fuel","Heat.Count", "DHW.Location", "DHW.Count")

#summarize by heating fuel
#by DHW Locations
item101.tmp1 <- summarise(group_by(item101.merge2, BuildingType, Heating.Fuel, DHW.Location)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Location.Dist = sum(DHW.Count)) 
#across DHW Locations
item101.tmp2 <- summarise(group_by(item101.merge2, BuildingType, Heating.Fuel)
                          ,DHW.Location = "All Locations"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Location.Dist = sum(DHW.Count)) 

#summarize across heating fuels
#by DHW Locations
item101.tmp3 <- summarise(group_by(item101.merge2, BuildingType, DHW.Location)
                          ,Heating.Fuel = "All Heating Fuel Types"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Location.Dist = sum(DHW.Count)) 
#across DHW Locations
item101.tmp4 <- summarise(group_by(item101.merge2, BuildingType)
                          ,DHW.Location = "All Locations"
                          ,Heating.Fuel = "All Heating Fuel Types"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Location.Dist = sum(DHW.Count)) 

item101.merge.final <- rbind.data.frame(item101.tmp1, item101.tmp2, item101.tmp3, item101.tmp4, stringsAsFactors = F)

item101.tot.counts <- rbind.data.frame(item101.tmp2, item101.tmp4, stringsAsFactors = F)


item101.final <- left_join(item101.merge.final, item101.tot.counts, by = c("BuildingType", "Heating.Fuel"))
colnames(item101.final) <- c("BuildingType"
                             , "Heating.Fuel"
                             , "DHW.Location"
                             , "SampleSize"
                             , "Count"
                             , "Remove"
                             , "Remove"
                             , "Total.Count")

item101.final$Percent <- item101.final$Count / item101.final$Total.Count
item101.final$SE <- sqrt(item101.final$Percent * (1 - item101.final$Percent) / item101.final$SampleSize)

library(data.table)
item101.table <- dcast(setDT(item101.final)
                       ,formula = BuildingType + DHW.Location ~ Heating.Fuel
                       ,value.var = c("SampleSize", "Percent","SE"))
item101.table <- data.frame(item101.table)
item101.table1 <- item101.table[-which(colnames(item101.table) %in% c("SampleSize_Electric"
                                                                      ,"SampleSize_Natural.Gas"
                                                                      ,"SampleSize_Oil"
                                                                      ,"SampleSize_Other"
                                                                      ,"SampleSize_Pellets"
                                                                      ,"SampleSize_Propane"
                                                                      ,"SampleSize_Unknown"
                                                                      ,"SampleSize_Wood"))]

item101.table2 <- data.frame("BuildingType" = item101.table1$BuildingType
                             ,"DHW.Location" = item101.table1$DHW.Location
                             ,"Percent.Electric" = item101.table1$Percent_Electric
                             ,"SE.Electric" = item101.table1$SE_Electric
                             ,"Percent.Natural.Gas" = item101.table1$Percent_Natural.Gas
                             ,"SE.Natural.Gas" = item101.table1$SE_Natural.Gas
                             # ,"Percent.Oil" = item101.table1$Percent_Oil
                             # ,"SE.Oil" = item101.table1$SE_Oil
                             # ,"Percent.Pellets" = item101.table1$Percent_Pellets
                             # ,"SE.Pellets" = item101.table1$SE_Pellets
                             # ,"Percent.Propane" = item101.table1$Percent_Propane
                             # ,"SE.Propane" = item101.table1$SE_Propane
                             ,"Percent.Wood" = item101.table1$Percent_Wood
                             ,"SE.Wood" = item101.table1$SE_Wood
                             ,"Percent.Other" = item101.table1$Percent_Other
                             ,"SE.Other" = item101.table1$SE_Other
                             # ,"Percent.Unknown" = item101.table1$Percent_Unknown
                             # ,"SE.Unknown" = item101.table1$SE_Unknown
                             ,"Percent.All.Heating.Fuel.Types" = item101.table1$Percent_All.Heating.Fuel.Types
                             ,"SE.All.Heating.Fuel.Types" = item101.table1$SE_All.Heating.Fuel.Types
                             ,"SampleSize" = item101.table1$SampleSize_All.Heating.Fuel.Types)

item101.table.final <- item101.table2[which(item101.table2$BuildingType %in% c("Single Family")),]
