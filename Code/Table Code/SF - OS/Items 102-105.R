#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list = ls())
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
# download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx', mechanical.export, mode = 'wb')
# mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



# #############################################################################################
# #Item 102: DISTRIBUTION OF TANK SIZE BY FUEL TYPE (SF table 109)
# #############################################################################################
# #subset to columns needed for analysis
# item102.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
#                                                                     ,"Generic"
#                                                                     ,"DHW.Fuel"
#                                                                     ,"DHW.Location"
#                                                                     ,"DHW.Size.(Gallons)"
#                                                                     ,""))]
# item102.dat$count <- 1
# 
# item102.dat0 <- item102.dat[which(item102.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item102.dat1 <- left_join(item102.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# 
# item102.dat2 <- item102.dat1[grep("Water Heater",item102.dat1$Generic),]
# item102.dat3 <- item102.dat2[which(!(is.na(item102.dat2$`DHW.Size.(Gallons)`))),]
# item102.dat4 <- item102.dat3[which(!(item102.dat3$`DHW.Size.(Gallons)` %in% c(0, "Unknown"))),]
# 
# item102.dat4$`DHW.Size.(Gallons)` <- as.numeric(as.character(item102.dat4$`DHW.Size.(Gallons)`))
# 
# item102.dat4$Gallon_bins <- item102.dat4$`DHW.Size.(Gallons)`
# item102.dat4$Gallon_bins[which(item102.dat4$`DHW.Size.(Gallons)` <= 55)] <- "0-55 Gallons"
# item102.dat4$Gallon_bins[which(item102.dat4$`DHW.Size.(Gallons)` >  55)] <- ">55 Gallons"
# unique(item102.dat4$Gallon_bins)
# 
# unique(item102.dat4$DHW.Fuel)
# 
# item102.merge <- left_join(rbsa.dat, item102.dat4)
# item102.merge <- item102.merge[which(!is.na(item102.merge$Gallon_bins)),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item102.data <- weightedData(item102.merge[-which(colnames(item102.merge) %in% c("Generic"               
#                                                                                  ,"count"
#                                                                                  ,"DHW.Size.(Gallons)"
#                                                                                  ,"DHW.Fuel"
#                                                                                  ,"DHW.Location"
#                                                                                  ,"Gallon_bins"))])
# item102.data <- left_join(item102.data, item102.merge[which(colnames(item102.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Generic"               
#                                                                                            ,"count"
#                                                                                            ,"DHW.Size.(Gallons)"
#                                                                                            ,"DHW.Fuel"
#                                                                                            ,"DHW.Location"
#                                                                                            ,"Gallon_bins"))])
# #######################
# # Weighted Analysis
# #######################
# item102.summary <- proportionRowsAndColumns1(CustomerLevelData = item102.data
#                                              ,valueVariable    = 'count'
#                                              ,columnVariable   = 'DHW.Fuel'
#                                              ,rowVariable      = 'Gallon_bins'
#                                              ,aggregateColumnName = "Remove")
# item102.summary <- item102.summary[which(item102.summary$DHW.Fuel != "Remove"),]
# 
# item102.all.fuels <- proportions_one_group(CustomerLevelData = item102.data
#                                            ,valueVariable = "count"
#                                            ,groupingVariable = "Gallon_bins"
#                                            ,total.name = "All Fuel Types"
#                                            ,columnName = "DHW.Fuel"
#                                            ,weighted = TRUE
#                                            ,two.prop.total = TRUE)
# 
# item102.final <- rbind.data.frame(item102.summary, item102.all.fuels)
# 
# item102.cast <- dcast(setDT(item102.final)
#                       ,formula = BuildingType + DHW.Fuel ~ Gallon_bins
#                       ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
# 
# item102.table <- data.frame("BuildingType"          = item102.cast$BuildingType
#                             ,"DHW.Fuel"             = item102.cast$DHW.Fuel
#                             ,"Percent_0.50.Gallons" = item102.cast$`w.percent_0-55 Gallons`
#                             ,"SE_0.50.Gallons"      = item102.cast$`w.SE_0-55 Gallons`
#                             ,"Percent_GT50.Gallons" = item102.cast$`w.percent_>55 Gallons`
#                             ,"SE_GT50.Gallons"      = item102.cast$`w.SE_>55 Gallons`
#                             ,n = item102.cast$n_Total
#                             ,"EB_0.50.Gallons"      = item102.cast$`EB_0-55 Gallons`
#                             ,"EB_GT50.Gallons"      = item102.cast$`EB_>55 Gallons`
# )
# 
# # row ordering example code
# unique(item102.table$DHW.Fuel)
# rowOrder <- c("Electric"
#               ,"Natural Gas"
#               ,"Oil"
#               ,"Propane"
#               ,"Unknown"
#               ,"All Fuel Types")
# item102.table <- item102.table %>% mutate(DHW.Fuel = factor(DHW.Fuel, levels = rowOrder)) %>% arrange(DHW.Fuel)  
# item102.table <- data.frame(item102.table)
# 
# 
# item102.table.SF <- item102.table[which(item102.table$BuildingType == "Single Family"),
#                                   -which(colnames(item102.table) %in% c("BuildingType"))]
# 
# exportTable(item102.table.SF, "SF", "Table 109", weighted = TRUE)
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# item102.final <- proportions_two_groups_unweighted(CustomerLevelData = item102.data
#                                                    ,valueVariable    = 'count'
#                                                    ,columnVariable   = 'DHW.Fuel'
#                                                    ,rowVariable      = 'Gallon_bins'
#                                                    ,aggregateColumnName = "Remove")
# item102.final <- item102.final[which(item102.final$DHW.Fuel != "Remove"),]
# 
# item102.all.fuels <- proportions_one_group(CustomerLevelData = item102.data
#                                            ,valueVariable = "count"
#                                            ,groupingVariable = "Gallon_bins"
#                                            ,total.name = "All Fuel Types"
#                                            ,columnName = "DHW.Fuel"
#                                            ,weighted = FALSE
#                                            ,two.prop.total = TRUE)
# 
# item102.final <- rbind.data.frame(item102.final, item102.all.fuels)
# 
# item102.cast <- dcast(setDT(item102.final)
#                       ,formula = BuildingType + DHW.Fuel ~ Gallon_bins
#                       ,value.var = c("Percent", "SE", "n"))
# 
# item102.table <- data.frame("BuildingType"          = item102.cast$BuildingType
#                             ,"DHW.Fuel"             = item102.cast$DHW.Fuel
#                             ,"Percent_0.50.Gallons" = item102.cast$`Percent_0-55 Gallons`
#                             ,"SE_0.50.Gallons"      = item102.cast$`SE_0-55 Gallons`
#                             ,"Percent_GT50.Gallons" = item102.cast$`Percent_>55 Gallons`
#                             ,"SE_GT50.Gallons"      = item102.cast$`SE_>55 Gallons`
#                             ,"n" = item102.cast$n_Total
# )
# 
# # row ordering example code
# unique(item102.table$DHW.Fuel)
# rowOrder <- c("Electric"
#               ,"Natural Gas"
#               ,"Oil"
#               ,"Propane"
#               ,"Unknown"
#               ,"All Fuel Types")
# item102.table <- item102.table %>% mutate(DHW.Fuel = factor(DHW.Fuel, levels = rowOrder)) %>% arrange(DHW.Fuel)  
# item102.table <- data.frame(item102.table)
# 
# 
# item102.table.SF <- item102.table[which(item102.table$BuildingType == "Single Family"),
#                                   -which(colnames(item102.table) %in% c("BuildingType"))]
# 
# exportTable(item102.table.SF, "SF", "Table 109", weighted = FALSE)
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 103: DISTRIBUTION OF ELECTRIC WATER HEATER TANK SIZE BY LOCATION (SF table 110)
# #############################################################################################
# #subset to columns needed for analysis
# item103.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
#                                                                     ,"Generic"
#                                                                     ,"DHW.Fuel"
#                                                                     ,"DHW.Location"
#                                                                     ,"DHW.Size.(Gallons)"
#                                                                     ,""))]
# item103.dat$count <- 1
# 
# item103.dat0 <- item103.dat[which(item103.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item103.dat1 <- left_join(item103.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# 
# item103.dat2 <- item103.dat1[grep("Water Heater",item103.dat1$Generic),]
# item103.dat3 <- item103.dat2[which(!(is.na(item103.dat2$`DHW.Size.(Gallons)`))),]
# item103.dat4 <- item103.dat3[which(!(item103.dat3$`DHW.Size.(Gallons)` %in% c(0, "Unknown"))),]
# 
# item103.dat4$`DHW.Size.(Gallons)` <- as.numeric(as.character(item103.dat4$`DHW.Size.(Gallons)`))
# 
# #clean gallon bins
# item103.dat4$Gallon_bins <- item103.dat4$`DHW.Size.(Gallons)`
# item103.dat4$Gallon_bins[which(item103.dat4$`DHW.Size.(Gallons)` <= 55)] <- "0-55 Gallons"
# item103.dat4$Gallon_bins[which(item103.dat4$`DHW.Size.(Gallons)` >  55)] <- ">55 Gallons"
# unique(item103.dat4$Gallon_bins)
# 
# #clean location types
# item103.dat5 <- item103.dat4[which(item103.dat4$DHW.Location != "Unknown"),]
# 
# item103.dat5$DHW.Location[grep("Crawl",item103.dat5$DHW.Location)] <- "Crawlspace"
# item103.dat5$DHW.Location[grep("In building",item103.dat5$DHW.Location)] <- "Main House"
# 
# item103.dat5$DHW.Location[which(item103.dat5$DHW.Location %notin% c("Crawlspace"
#                                                                     ,"Basement"
#                                                                     ,"Garage"
#                                                                     ,"Main House"))] <- "Other"
# 
# item103.dat5 <- item103.dat5[which(item103.dat5$DHW.Fuel == "Electric"),]
# 
# item103.merge <- left_join(rbsa.dat, item103.dat5)
# item103.merge <- item103.merge[which(!is.na(item103.merge$count)),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item103.data <- weightedData(item103.merge[-which(colnames(item103.merge) %in% c("Generic"               
#                                                                                  ,"count"
#                                                                                  ,"DHW.Size.(Gallons)"
#                                                                                  ,"DHW.Fuel"
#                                                                                  ,"DHW.Location"
#                                                                                  ,"Gallon_bins"))])
# item103.data <- left_join(item103.data, item103.merge[which(colnames(item103.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Generic"               
#                                                                                            ,"count"
#                                                                                            ,"DHW.Size.(Gallons)"
#                                                                                            ,"DHW.Fuel"
#                                                                                            ,"DHW.Location"
#                                                                                            ,"Gallon_bins"))])
# #######################
# # Weighted Analysis
# #######################
# item103.final <- proportionRowsAndColumns1(CustomerLevelData = item103.data
#                                            ,valueVariable    = 'count'
#                                            ,columnVariable   = 'DHW.Location'
#                                            ,rowVariable      = 'Gallon_bins'
#                                            ,aggregateColumnName = "Remove")
# item103.final <- item103.final[which(item103.final$DHW.Location != "Remove"),]
# 
# item103.all.fuels <- proportions_one_group(CustomerLevelData = item103.data
#                                            ,valueVariable = "count"
#                                            ,groupingVariable = "Gallon_bins"
#                                            ,total.name = "All Locations"
#                                            ,columnName = "DHW.Location"
#                                            ,weighted = TRUE
#                                            ,two.prop.total = TRUE)
# 
# item103.final <- rbind.data.frame(item103.final, item103.all.fuels)
# 
# item103.cast <- dcast(setDT(item103.final)
#                       ,formula = BuildingType + DHW.Location ~ Gallon_bins
#                       ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
# 
# item103.table <- data.frame("BuildingType"          = item103.cast$BuildingType
#                             ,"DHW.Location"         = item103.cast$DHW.Location
#                             ,"Percent_0.50.Gallons" = item103.cast$`w.percent_0-55 Gallons`
#                             ,"SE_0.50.Gallons"      = item103.cast$`w.SE_0-55 Gallons`
#                             ,"Percent_GT50.Gallons" = item103.cast$`w.percent_>55 Gallons`
#                             ,"SE_GT50.Gallons"      = item103.cast$`w.SE_>55 Gallons`
#                             ,"n"                    = item103.cast$n_Total
#                             ,"EB_0.50.Gallons"      = item103.cast$`EB_0-55 Gallons`
#                             ,"EB_GT50.Gallons"      = item103.cast$`EB_>55 Gallons`
# )
# 
# # row ordering example code
# unique(item103.table$DHW.Location)
# rowOrder <- c("Basement"
#               ,"Crawlspace"
#               ,"Garage"
#               ,"Main House"
#               ,"Other"
#               ,"All Locations")
# item103.table <- item103.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
# item103.table <- data.frame(item103.table)
# 
# 
# 
# item103.table.SF <- item103.table[which(item103.table$BuildingType == "Single Family"),
#                                   -which(colnames(item103.table) %in% c("BuildingType"))]
# 
# exportTable(item103.table.SF, "SF", "Table 110", weighted = TRUE)
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# item103.final <- proportions_two_groups_unweighted(CustomerLevelData = item103.data
#                                                    ,valueVariable    = 'count'
#                                                    ,columnVariable   = 'DHW.Location'
#                                                    ,rowVariable      = 'Gallon_bins'
#                                                    ,aggregateColumnName = "Remove")
# item103.final <- item103.final[which(item103.final$DHW.Location != "Remove"),]
# 
# item103.all.fuels <- proportions_one_group(CustomerLevelData = item103.data
#                                            ,valueVariable = "count"
#                                            ,groupingVariable = "Gallon_bins"
#                                            ,total.name = "All Locations"
#                                            ,columnName = "DHW.Location"
#                                            ,weighted = FALSE
#                                            ,two.prop.total = TRUE)
# 
# item103.final <- rbind.data.frame(item103.final, item103.all.fuels)
# 
# item103.cast <- dcast(setDT(item103.final)
#                       ,formula = BuildingType + DHW.Location ~ Gallon_bins
#                       ,value.var = c("Percent", "SE", "Count", "n"))
# 
# item103.table <- data.frame("BuildingType"          = item103.cast$BuildingType
#                             ,"DHW.Location"         = item103.cast$DHW.Location
#                             ,"Percent_0.50.Gallons" = item103.cast$`Percent_0-55 Gallons`
#                             ,"SE_0.50.Gallons"      = item103.cast$`SE_0-55 Gallons`
#                             ,"Percent_GT50.Gallons" = item103.cast$`Percent_>55 Gallons`
#                             ,"SE_GT50.Gallons"      = item103.cast$`SE_>55 Gallons`
#                             ,"n" = item103.cast$n_Total
# )
# 
# # row ordering example code
# unique(item103.table$DHW.Location)
# rowOrder <- c("Basement"
#               ,"Crawlspace"
#               ,"Garage"
#               ,"Main House"
#               ,"Other"
#               ,"All Locations")
# item103.table <- item103.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
# item103.table <- data.frame(item103.table)
# 
# 
# item103.table.SF <- item103.table[which(item103.table$BuildingType == "Single Family"),
#                                   -which(colnames(item103.table) %in% c("BuildingType"))]
# 
# exportTable(item103.table.SF, "SF", "Table 110", weighted = FALSE)
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 104: DISTRIBUTION OF GAS WATER HEATER TANK SIZE BY LOCATION (SF table 111)
# #############################################################################################
# #subset to columns needed for analysis
# item104.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
#                                                                     ,"Generic"
#                                                                     ,"DHW.Fuel"
#                                                                     ,"DHW.Location"
#                                                                     ,"DHW.Size.(Gallons)"
#                                                                     ,""))]
# item104.dat$count <- 1
# 
# item104.dat0 <- item104.dat[which(item104.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item104.dat1 <- left_join(item104.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# 
# item104.dat2 <- item104.dat1[grep("Water Heater",item104.dat1$Generic),]
# item104.dat3 <- item104.dat2[which(!(is.na(item104.dat2$`DHW.Size.(Gallons)`))),]
# item104.dat4 <- item104.dat3[which(!(item104.dat3$`DHW.Size.(Gallons)` %in% c(0, "Unknown"))),]
# 
# item104.dat4$`DHW.Size.(Gallons)` <- as.numeric(as.character(item104.dat4$`DHW.Size.(Gallons)`))
# 
# #clean gallon bins
# item104.dat4$Gallon_bins <- item104.dat4$`DHW.Size.(Gallons)`
# item104.dat4$Gallon_bins[which(item104.dat4$`DHW.Size.(Gallons)` <= 55)] <- "0-55 Gallons"
# item104.dat4$Gallon_bins[which(item104.dat4$`DHW.Size.(Gallons)` >  55)] <- ">55 Gallons"
# unique(item104.dat4$Gallon_bins)
# 
# #clean location types
# item104.dat5 <- item104.dat4[which(item104.dat4$DHW.Location != "Unknown"),]
# 
# item104.dat5$DHW.Location[grep("Crawl",item104.dat5$DHW.Location)] <- "Crawlspace"
# item104.dat5$DHW.Location[grep("In building",item104.dat5$DHW.Location)] <- "Main House"
# 
# item104.dat5$DHW.Location[which(item104.dat5$DHW.Location %notin% c("Crawlspace"
#                                                                     ,"Basement"
#                                                                     ,"Garage"
#                                                                     ,"Main House"))] <- "Other"
# 
# item104.dat6 <- item104.dat5[which(item104.dat5$DHW.Fuel == "Natural Gas"),]
# 
# item104.merge <- left_join(rbsa.dat, item104.dat6)
# item104.merge <- item104.merge[which(!is.na(item104.merge$count)),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item104.data <- weightedData(item104.merge[-which(colnames(item104.merge) %in% c("Generic"               
#                                                                                  ,"count"
#                                                                                  ,"DHW.Size.(Gallons)"
#                                                                                  ,"DHW.Fuel"
#                                                                                  ,"DHW.Location"
#                                                                                  ,"Gallon_bins"))])
# item104.data <- left_join(item104.data, item104.merge[which(colnames(item104.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Generic"               
#                                                                                            ,"count"
#                                                                                            ,"DHW.Size.(Gallons)"
#                                                                                            ,"DHW.Fuel"
#                                                                                            ,"DHW.Location"
#                                                                                            ,"Gallon_bins"))])
# #######################
# # Weighted Analysis
# #######################
# item104.final <- proportionRowsAndColumns1(CustomerLevelData = item104.data
#                                            ,valueVariable    = 'count'
#                                            ,columnVariable   = 'DHW.Location'
#                                            ,rowVariable      = 'Gallon_bins'
#                                            ,aggregateColumnName = "Remove")
# item104.final <- item104.final[which(item104.final$DHW.Location != "Remove"),]
# 
# item104.all.fuels <- proportions_one_group(CustomerLevelData = item104.data
#                                            ,valueVariable = "count"
#                                            ,groupingVariable = "Gallon_bins"
#                                            ,total.name = "All Locations"
#                                            ,columnName = "DHW.Location"
#                                            ,weighted = TRUE
#                                            ,two.prop.total = TRUE)
# 
# 
# item104.final <- rbind.data.frame(item104.final, item104.all.fuels)
# 
# item104.cast <- dcast(setDT(item104.final)
#                       ,formula = BuildingType + DHW.Location ~ Gallon_bins
#                       ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
# 
# item104.table <- data.frame("BuildingType"          = item104.cast$BuildingType
#                             ,"DHW.Location"         = item104.cast$DHW.Location
#                             ,"Percent_0.50.Gallons" = item104.cast$`w.percent_0-55 Gallons`
#                             ,"SE_0.50.Gallons"      = item104.cast$`w.SE_0-55 Gallons`
#                             ,"Percent_GT50.Gallons" = item104.cast$`w.percent_>55 Gallons`
#                             ,"SE_GT50.Gallons"      = item104.cast$`w.SE_>55 Gallons`
#                             ,"n" = item104.cast$n_Total
#                             ,"EB_0.50.Gallons"      = item104.cast$`EB_0-55 Gallons`
#                             ,"EB_GT50.Gallons"      = item104.cast$`EB_>55 Gallons`
# )
# 
# # row ordering example code
# unique(item104.table$DHW.Location)
# rowOrder <- c("Basement"
#               ,"Crawlspace"
#               ,"Garage"
#               ,"Main House"
#               ,"Other"
#               ,"All Locations")
# item104.table <- item104.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
# item104.table <- data.frame(item104.table)
# 
# 
# item104.table.SF <- item104.table[which(item104.table$BuildingType == "Single Family"),
#                                   -which(colnames(item104.table) %in% c("BuildingType"))]
# 
# exportTable(item104.table.SF, "SF", "Table 111", weighted = TRUE)
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# item104.final <- proportions_two_groups_unweighted(CustomerLevelData = item104.data
#                                                    ,valueVariable    = 'count'
#                                                    ,columnVariable   = 'DHW.Location'
#                                                    ,rowVariable      = 'Gallon_bins'
#                                                    ,aggregateColumnName = "Remove")
# item104.final <- item104.final[which(item104.final$DHW.Location != "Remove"),]
# 
# item104.all.fuels <- proportions_one_group(CustomerLevelData = item104.data
#                                            ,valueVariable = "count"
#                                            ,groupingVariable = "Gallon_bins"
#                                            ,total.name = "All Locations"
#                                            ,columnName = "DHW.Location"
#                                            ,weighted = FALSE
#                                            ,two.prop.total = TRUE)
# 
# item104.final <- rbind.data.frame(item104.final, item104.all.fuels)
# 
# item104.cast <- dcast(setDT(item104.final)
#                       ,formula = BuildingType + DHW.Location ~ Gallon_bins
#                       ,value.var = c("Percent", "SE", "Count", "n"))
# 
# item104.table <- data.frame("BuildingType"          = item104.cast$BuildingType
#                             ,"DHW.Location"         = item104.cast$DHW.Location
#                             ,"Percent_0.50.Gallons" = item104.cast$`Percent_0-55 Gallons`
#                             ,"SE_0.50.Gallons"      = item104.cast$`SE_0-55 Gallons`
#                             ,"Percent_GT50.Gallons" = item104.cast$`Percent_>55 Gallons`
#                             ,"SE_GT50.Gallons"      = item104.cast$`SE_>55 Gallons`
#                             ,"n" = item104.cast$n_Total
# )
# 
# # row ordering example code
# unique(item104.table$DHW.Location)
# rowOrder <- c("Basement"
#               ,"Crawlspace"
#               ,"Garage"
#               ,"Main House"
#               ,"Other"
#               ,"All Locations")
# item104.table <- item104.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
# item104.table <- data.frame(item104.table)
# 
# 
# item104.table.SF <- item104.table[which(item104.table$BuildingType == "Single Family"),
#                                   -which(colnames(item104.table) %in% c("BuildingType"))]
# 
# exportTable(item104.table.SF, "SF", "Table 111", weighted = FALSE)
# 
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 105: DISTRIBUTION OF WATER HEATERS BY VINTAGE (SF table 112, MH table 87)
# #############################################################################################
# #subset to columns needed for analysis
# item105.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
#                                                                     ,"Generic"
#                                                                     ,"DHW.Fuel"
#                                                                     ,"DHW.Location"
#                                                                     ,"DHW.Size.(Gallons)"
#                                                                     ,"DHW.Year.Manufactured"))]
# item105.dat$count <- 1
# 
# item105.dat0 <- item105.dat[which(item105.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item105.dat1 <- left_join( rbsa.dat,item105.dat0, by = "CK_Cadmus_ID")
# 
# item105.dat2 <- item105.dat1[which(!(is.na(item105.dat1$DHW.Year.Manufactured))),]
# item105.dat3 <- item105.dat2[which(!(item105.dat2$DHW.Year.Manufactured %in% c("-- Datapoint not asked for --", "Unknown"))),]
# unique(item105.dat3$DHW.Year.Manufactured)
# 
# # Bin equipment vintages for items 50 and 52 (4 categories)
# item105.dat3$EquipVintage_bins <- as.numeric(as.character(item105.dat3$DHW.Year.Manufactured))
# item105.dat4 <- item105.dat3[which(!is.na(item105.dat3$EquipVintage_bins)),]
# item105.dat4$EquipVintage_bins[which(item105.dat4$DHW.Year.Manufactured < 1990)] <- "Pre 1990"
# item105.dat4$EquipVintage_bins[which(item105.dat4$DHW.Year.Manufactured >= 1990 & item105.dat4$DHW.Year.Manufactured < 2000)] <- "1990-1999"
# item105.dat4$EquipVintage_bins[which(item105.dat4$DHW.Year.Manufactured >= 2000 & item105.dat4$DHW.Year.Manufactured < 2005)] <- "2000-2004"
# item105.dat4$EquipVintage_bins[which(item105.dat4$DHW.Year.Manufactured >= 2005 & item105.dat4$DHW.Year.Manufactured < 2010)] <- "2005-2009"
# item105.dat4$EquipVintage_bins[which(item105.dat4$DHW.Year.Manufactured >= 2010 & item105.dat4$DHW.Year.Manufactured < 2015)] <- "2010-2014"
# item105.dat4$EquipVintage_bins[which(item105.dat4$DHW.Year.Manufactured >= 2015)] <- "Post 2014"
# #check uniques
# unique(item105.dat4$EquipVintage_bins)
# 
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item105.data <- weightedData(item105.dat4[-which(colnames(item105.dat4) %in% c("Generic"
#                                                                                ,"DHW.Size.(Gallons)"
#                                                                                ,"DHW.Year.Manufactured"
#                                                                                ,"DHW.Fuel"
#                                                                                ,"DHW.Location"
#                                                                                ,"count"
#                                                                                ,"EquipVintage_bins"))])
# item105.data <- left_join(item105.data, item105.dat4[which(colnames(item105.dat4) %in% c("CK_Cadmus_ID"
#                                                                                          ,"Generic"
#                                                                                          ,"DHW.Size.(Gallons)"
#                                                                                          ,"DHW.Year.Manufactured"
#                                                                                          ,"DHW.Fuel"
#                                                                                          ,"DHW.Location"
#                                                                                          ,"count"
#                                                                                          ,"EquipVintage_bins"))])
# item105.data$m_ilk <- 1
# 
# #######################
# # Weighted Analysis
# #######################
# item105.final <- proportions_one_group(CustomerLevelData  = item105.data
#                                        , valueVariable    = 'm_ilk'
#                                        , groupingVariable = 'EquipVintage_bins'
#                                        , total.name =      "Total")
# 
# unique(item105.final$EquipVintage_bins)
# rowOrder <- c("Pre 1990"
#               ,"1990-1999"
#               ,"2000-2004"
#               ,"2005-2009"
#               ,"2010-2014"
#               ,"Post 2014"
#               ,"Total")
# item105.table <- item105.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
# item105.table <- data.frame(item105.table)
# 
# 
# # SF = Table 112, MH = Table 87
# # Export table
# item105.final.SF <- item105.table[which(item105.table$BuildingType == "Single Family")
#                                   ,-which(colnames(item105.table) %in% c("BuildingType"))]
# item105.final.MH <- item105.table[which(item105.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(item105.table) %in% c("BuildingType"))]
# 
# exportTable(item105.final.SF, "SF", "Table 112", weighted = TRUE)
# # exportTable(item105.final.MH, "MH", "Table 87", weighted = TRUE)
# 
# #######################
# # Unweighted Analysis
# #######################
# item105.final <- proportions_one_group(CustomerLevelData  = item105.data
#                                        , valueVariable    = 'count'
#                                        , groupingVariable = 'EquipVintage_bins'
#                                        , total.name       = "Total"
#                                        , weighted         = FALSE)
# unique(item105.final$EquipVintage_bins)
# rowOrder <- c("Pre 1990"
#               ,"1990-1999"
#               ,"2000-2004"
#               ,"2005-2009"
#               ,"2010-2014"
#               ,"Post 2014"
#               ,"Total")
# item105.table <- item105.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
# item105.table <- data.frame(item105.table)
# 
# 
# # SF = Table 112, MH = Table 87
# # Export table
# item105.final.SF <- item105.table[which(item105.table$BuildingType == "Single Family")
#                                   ,-which(colnames(item105.table) %in% c("BuildingType"))]
# item105.final.MH <- item105.table[which(item105.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(item105.table) %in% c("BuildingType"))]
# 
# exportTable(item105.final.SF, "SF", "Table 112", weighted = FALSE)
# # exportTable(item105.final.MH, "MH", "Table 87", weighted = FALSE)




























############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean os data
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]

#############################################################################################
#Item 102: DISTRIBUTION OF TANK SIZE BY FUEL TYPE (SF table 109)
#############################################################################################
#subset to columns needed for analysis
item102.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Location"
                                                                    ,"DHW.Size.(Gallons)"
                                                                    ,""))]
item102.os.dat$count <- 1

item102.os.dat0 <- item102.os.dat[which(item102.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item102.os.dat1 <- left_join(item102.os.dat0, os.dat, by = "CK_Cadmus_ID")

item102.os.dat2 <- item102.os.dat1[grep("Water Heater",item102.os.dat1$Generic),]
item102.os.dat3 <- item102.os.dat2[which(!(is.na(item102.os.dat2$`DHW.Size.(Gallons)`))),]
item102.os.dat4 <- item102.os.dat3[which(!(item102.os.dat3$`DHW.Size.(Gallons)` %in% c(0, "Unknown"))),]

item102.os.dat4$`DHW.Size.(Gallons)` <- as.numeric(as.character(item102.os.dat4$`DHW.Size.(Gallons)`))

item102.os.dat4$Gallon_bins <- item102.os.dat4$`DHW.Size.(Gallons)`
item102.os.dat4$Gallon_bins[which(item102.os.dat4$`DHW.Size.(Gallons)` <= 55)] <- "0-55 Gallons"
item102.os.dat4$Gallon_bins[which(item102.os.dat4$`DHW.Size.(Gallons)` >  55)] <- ">55 Gallons"
unique(item102.os.dat4$Gallon_bins)

unique(item102.os.dat4$DHW.Fuel)

item102.os.merge <- left_join(os.dat, item102.os.dat4)
item102.os.merge <- item102.os.merge[which(!is.na(item102.os.merge$Gallon_bins)),]

################################################
# Adding pop and sample sizes for weights
################################################
item102.os.data <- weightedData(item102.os.merge[-which(colnames(item102.os.merge) %in% c("Generic"
                                                                                 ,"count"
                                                                                 ,"DHW.Size.(Gallons)"
                                                                                 ,"DHW.Fuel"
                                                                                 ,"DHW.Location"
                                                                                 ,"Gallon_bins"))])
item102.os.data <- left_join(item102.os.data, unique(item102.os.merge[which(colnames(item102.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Generic"
                                                                                           ,"count"
                                                                                           ,"DHW.Size.(Gallons)"
                                                                                           ,"DHW.Fuel"
                                                                                           ,"DHW.Location"
                                                                                           ,"Gallon_bins"))]))
item102.os.data <- item102.os.data[which(item102.os.data$CK_Building_ID == subset.ind),]
#######################
# Weighted Analysis
#######################
item102.os.summary <- proportionRowsAndColumns1(CustomerLevelData = item102.os.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable   = 'DHW.Fuel'
                                             ,rowVariable      = 'Gallon_bins'
                                             ,aggregateColumnName = "Remove")
item102.os.summary <- item102.os.summary[which(item102.os.summary$DHW.Fuel != "Remove"),]

item102.os.all.fuels <- proportions_one_group(CustomerLevelData = item102.os.data
                                           ,valueVariable = "count"
                                           ,groupingVariable = "Gallon_bins"
                                           ,total.name = "All Fuel Types"
                                           ,columnName = "DHW.Fuel"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)

item102.os.final <- rbind.data.frame(item102.os.summary, item102.os.all.fuels)

item102.os.cast <- dcast(setDT(item102.os.final)
                      ,formula = BuildingType + DHW.Fuel ~ Gallon_bins
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
names(item102.os.cast)

item102.os.table <- data.frame("BuildingType"          = item102.os.cast$BuildingType
                            ,"DHW.Fuel"             = item102.os.cast$DHW.Fuel
                            ,"Percent_0.50.Gallons" = item102.os.cast$`w.percent_0-55 Gallons`
                            ,"SE_0.50.Gallons"      = item102.os.cast$`w.SE_0-55 Gallons`
                            ,"Percent_GT50.Gallons" = item102.os.cast$`w.percent_>55 Gallons`
                            ,"SE_GT50.Gallons"      = item102.os.cast$`w.SE_>55 Gallons`
                            ,n = item102.os.cast$n_Total
                            ,"EB_0.50.Gallons"      = item102.os.cast$`EB_0-55 Gallons`
                            ,"EB_GT50.Gallons"      = item102.os.cast$`EB_>55 Gallons`
)

# row ordering example code
unique(item102.os.table$DHW.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"Oil"
              ,"Propane"
              ,"Unknown"
              ,"All Fuel Types")
item102.os.table <- item102.os.table %>% mutate(DHW.Fuel = factor(DHW.Fuel, levels = rowOrder)) %>% arrange(DHW.Fuel)
item102.os.table <- data.frame(item102.os.table)


item102.os.table.SF <- item102.os.table[which(item102.os.table$BuildingType == "Single Family"),
                                  -which(colnames(item102.os.table) %in% c("BuildingType"))]

exportTable(item102.os.table.SF, "SF", "Table 109", weighted = TRUE, osIndicator = export.ind, OS = T)


#######################
# Unweighted Analysis
#######################
item102.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item102.os.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'DHW.Fuel'
                                                   ,rowVariable      = 'Gallon_bins'
                                                   ,aggregateColumnName = "Remove")
item102.os.final <- item102.os.final[which(item102.os.final$DHW.Fuel != "Remove"),]

item102.os.all.fuels <- proportions_one_group(CustomerLevelData = item102.os.data
                                           ,valueVariable = "count"
                                           ,groupingVariable = "Gallon_bins"
                                           ,total.name = "All Fuel Types"
                                           ,columnName = "DHW.Fuel"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)

item102.os.final <- rbind.data.frame(item102.os.final, item102.os.all.fuels)

item102.os.cast <- dcast(setDT(item102.os.final)
                      ,formula = BuildingType + DHW.Fuel ~ Gallon_bins
                      ,value.var = c("Percent", "SE", "Count", "n"))

item102.os.table <- data.frame("BuildingType"       = item102.os.cast$BuildingType
                            ,"DHW.Fuel"             = item102.os.cast$DHW.Fuel
                            ,"Percent_0.50.Gallons" = item102.os.cast$`Percent_0-55 Gallons`
                            ,"SE_0.50.Gallons"      = item102.os.cast$`SE_0-55 Gallons`
                            ,"Percent_GT50.Gallons" = item102.os.cast$`Percent_>55 Gallons`
                            ,"SE_GT50.Gallons"      = item102.os.cast$`SE_>55 Gallons`
                            ,"n"                    = item102.os.cast$n_Total
)

# row ordering example code
unique(item102.os.table$DHW.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"Oil"
              ,"Propane"
              ,"Unknown"
              ,"All Fuel Types")
item102.os.table <- item102.os.table %>% mutate(DHW.Fuel = factor(DHW.Fuel, levels = rowOrder)) %>% arrange(DHW.Fuel)
item102.os.table <- data.frame(item102.os.table)


item102.os.table.SF <- item102.os.table[which(item102.os.table$BuildingType == "Single Family"),
                                  -which(colnames(item102.os.table) %in% c("BuildingType"))]

exportTable(item102.os.table.SF, "SF", "Table 109", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
#Item 103: DISTRIBUTION OF ELECTRIC WATER HEATER TANK SIZE BY LOCATION (SF table 110)
#############################################################################################
#subset to columns needed for analysis
item103.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Location"
                                                                    ,"DHW.Size.(Gallons)"
                                                                    ,""))]
item103.os.dat$count <- 1

item103.os.dat0 <- item103.os.dat[which(item103.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item103.os.dat1 <- left_join(item103.os.dat0, os.dat, by = "CK_Cadmus_ID")

item103.os.dat2 <- item103.os.dat1[grep("Water Heater",item103.os.dat1$Generic),]
item103.os.dat3 <- item103.os.dat2[which(!(is.na(item103.os.dat2$`DHW.Size.(Gallons)`))),]
item103.os.dat4 <- item103.os.dat3[which(!(item103.os.dat3$`DHW.Size.(Gallons)` %in% c(0, "Unknown"))),]

item103.os.dat4$`DHW.Size.(Gallons)` <- as.numeric(as.character(item103.os.dat4$`DHW.Size.(Gallons)`))

#clean gallon bins
item103.os.dat4$Gallon_bins <- item103.os.dat4$`DHW.Size.(Gallons)`
item103.os.dat4$Gallon_bins[which(item103.os.dat4$`DHW.Size.(Gallons)` <= 55)] <- "0-55 Gallons"
item103.os.dat4$Gallon_bins[which(item103.os.dat4$`DHW.Size.(Gallons)` >  55)] <- ">55 Gallons"
unique(item103.os.dat4$Gallon_bins)

#clean location types
item103.os.dat5 <- item103.os.dat4[which(item103.os.dat4$DHW.Location != "Unknown"),]

item103.os.dat5$DHW.Location[grep("Crawl",item103.os.dat5$DHW.Location)] <- "Crawlspace"
item103.os.dat5$DHW.Location[grep("In building",item103.os.dat5$DHW.Location)] <- "Main House"

item103.os.dat5$DHW.Location[which(item103.os.dat5$DHW.Location %notin% c("Crawlspace"
                                                                    ,"Basement"
                                                                    ,"Garage"
                                                                    ,"Main House"))] <- "Other"

item103.os.dat5 <- item103.os.dat5[which(item103.os.dat5$DHW.Fuel == "Electric"),]

item103.os.merge <- left_join(os.dat, item103.os.dat5)
item103.os.merge <- item103.os.merge[which(!is.na(item103.os.merge$count)),]

################################################
# Adding pop and sample sizes for weights
################################################
item103.os.data <- weightedData(item103.os.merge[-which(colnames(item103.os.merge) %in% c("Generic"
                                                                                 ,"count"
                                                                                 ,"DHW.Size.(Gallons)"
                                                                                 ,"DHW.Fuel"
                                                                                 ,"DHW.Location"
                                                                                 ,"Gallon_bins"))])
item103.os.data <- left_join(item103.os.data, unique(item103.os.merge[which(colnames(item103.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Generic"
                                                                                           ,"count"
                                                                                           ,"DHW.Size.(Gallons)"
                                                                                           ,"DHW.Fuel"
                                                                                           ,"DHW.Location"
                                                                                           ,"Gallon_bins"))]))
item103.os.data <- item103.os.data[which(item103.os.data$CK_Building_ID == subset.ind),]
#######################
# Weighted Analysis
#######################
item103.os.final <- proportionRowsAndColumns1(CustomerLevelData = item103.os.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'DHW.Location'
                                           ,rowVariable      = 'Gallon_bins'
                                           ,aggregateColumnName = "Remove")
item103.os.final <- item103.os.final[which(item103.os.final$DHW.Location != "Remove"),]

item103.os.all.fuels <- proportions_one_group(CustomerLevelData = item103.os.data
                                           ,valueVariable = "count"
                                           ,groupingVariable = "Gallon_bins"
                                           ,total.name = "All Locations"
                                           ,columnName = "DHW.Location"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)

item103.os.final <- rbind.data.frame(item103.os.final, item103.os.all.fuels)

item103.os.cast <- dcast(setDT(item103.os.final)
                      ,formula = BuildingType + DHW.Location ~ Gallon_bins
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item103.os.table <- data.frame("BuildingType"          = item103.os.cast$BuildingType
                               ,"DHW.Location"         = item103.os.cast$DHW.Location
                               ,"Percent_0.50.Gallons" = item103.os.cast$`w.percent_0-55 Gallons`
                               ,"SE_0.50.Gallons"      = item103.os.cast$`w.SE_0-55 Gallons`
                               ,"Percent_GT50.Gallons" = item103.os.cast$`w.percent_>55 Gallons`
                               ,"SE_GT50.Gallons"      = item103.os.cast$`w.SE_>55 Gallons`
                               ,"n"                    = item103.os.cast$n_Total
                               ,"EB_0.50.Gallons"      = item103.os.cast$`EB_0-55 Gallons`
                               ,"EB_GT50.Gallons"      = item103.os.cast$`EB_>55 Gallons`
)

# row ordering example code
unique(item103.os.table$DHW.Location)
rowOrder <- c("Basement"
              ,"Crawlspace"
              ,"Garage"
              ,"Main House"
              ,"Other"
              ,"All Locations")
item103.os.table <- item103.os.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)
item103.os.table <- data.frame(item103.os.table)



item103.os.table.SF <- item103.os.table[which(item103.os.table$BuildingType == "Single Family"),
                                  -which(colnames(item103.os.table) %in% c("BuildingType"))]

exportTable(item103.os.table.SF, "SF", "Table 110", weighted = TRUE, osIndicator = export.ind, OS = T)


#######################
# Unweighted Analysis
#######################
item103.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item103.os.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'DHW.Location'
                                                   ,rowVariable      = 'Gallon_bins'
                                                   ,aggregateColumnName = "Remove")
item103.os.final <- item103.os.final[which(item103.os.final$DHW.Location != "Remove"),]

item103.os.all.fuels <- proportions_one_group(CustomerLevelData = item103.os.data
                                           ,valueVariable = "count"
                                           ,groupingVariable = "Gallon_bins"
                                           ,total.name = "All Locations"
                                           ,columnName = "DHW.Location"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)

item103.os.final <- rbind.data.frame(item103.os.final, item103.os.all.fuels)

item103.os.cast <- dcast(setDT(item103.os.final)
                      ,formula = BuildingType + DHW.Location ~ Gallon_bins
                      ,value.var = c("Percent", "SE", "n"))

item103.os.table <- data.frame("BuildingType"          = item103.os.cast$BuildingType
                               ,"DHW.Location"         = item103.os.cast$DHW.Location
                               ,"Percent_0.50.Gallons" = item103.os.cast$`Percent_0-55 Gallons`
                               ,"SE_0.50.Gallons"      = item103.os.cast$`SE_0-55 Gallons`
                               ,"Percent_GT50.Gallons" = item103.os.cast$`Percent_>55 Gallons`
                               ,"SE_GT50.Gallons"      = item103.os.cast$`SE_>55 Gallons`
                               ,"n"                    = item103.os.cast$n_Total
)

# row ordering example code
unique(item103.os.table$DHW.Location)
rowOrder <- c("Basement"
              ,"Crawlspace"
              ,"Garage"
              ,"Main House"
              ,"Other"
              ,"All Locations")
item103.os.table <- item103.os.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)
item103.os.table <- data.frame(item103.os.table)


item103.os.table.SF <- item103.os.table[which(item103.os.table$BuildingType == "Single Family"),
                                  -which(colnames(item103.os.table) %in% c("BuildingType"))]

exportTable(item103.os.table.SF, "SF", "Table 110", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
#Item 104: DISTRIBUTION OF GAS WATER HEATER TANK SIZE BY LOCATION (SF table 111)
#############################################################################################
#subset to columns needed for analysis
item104.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Location"
                                                                    ,"DHW.Size.(Gallons)"
                                                                    ,""))]
item104.os.dat$count <- 1

item104.os.dat0 <- item104.os.dat[which(item104.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item104.os.dat1 <- left_join(item104.os.dat0, os.dat, by = "CK_Cadmus_ID")

item104.os.dat2 <- item104.os.dat1[grep("Water Heater",item104.os.dat1$Generic),]
item104.os.dat3 <- item104.os.dat2[which(!(is.na(item104.os.dat2$`DHW.Size.(Gallons)`))),]
item104.os.dat4 <- item104.os.dat3[which(!(item104.os.dat3$`DHW.Size.(Gallons)` %in% c(0, "Unknown"))),]

item104.os.dat4$`DHW.Size.(Gallons)` <- as.numeric(as.character(item104.os.dat4$`DHW.Size.(Gallons)`))

#clean gallon bins
item104.os.dat4$Gallon_bins <- item104.os.dat4$`DHW.Size.(Gallons)`
item104.os.dat4$Gallon_bins[which(item104.os.dat4$`DHW.Size.(Gallons)` <= 55)] <- "0-55 Gallons"
item104.os.dat4$Gallon_bins[which(item104.os.dat4$`DHW.Size.(Gallons)` >  55)] <- ">55 Gallons"
unique(item104.os.dat4$Gallon_bins)

#clean location types
item104.os.dat5 <- item104.os.dat4[which(item104.os.dat4$DHW.Location != "Unknown"),]

item104.os.dat5$DHW.Location[grep("Crawl",item104.os.dat5$DHW.Location)] <- "Crawlspace"
item104.os.dat5$DHW.Location[grep("In building",item104.os.dat5$DHW.Location)] <- "Main House"

item104.os.dat5$DHW.Location[which(item104.os.dat5$DHW.Location %notin% c("Crawlspace"
                                                                    ,"Basement"
                                                                    ,"Garage"
                                                                    ,"Main House"))] <- "Other"

item104.os.dat6 <- item104.os.dat5[which(item104.os.dat5$DHW.Fuel == "Natural Gas"),]

item104.os.merge <- left_join(os.dat, item104.os.dat6)
item104.os.merge <- item104.os.merge[which(!is.na(item104.os.merge$count)),]

################################################
# Adding pop and sample sizes for weights
################################################
item104.os.data <- weightedData(item104.os.merge[-which(colnames(item104.os.merge) %in% c("Generic"
                                                                                 ,"count"
                                                                                 ,"DHW.Size.(Gallons)"
                                                                                 ,"DHW.Fuel"
                                                                                 ,"DHW.Location"
                                                                                 ,"Gallon_bins"))])
item104.os.data <- left_join(item104.os.data, unique(item104.os.merge[which(colnames(item104.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Generic"
                                                                                           ,"count"
                                                                                           ,"DHW.Size.(Gallons)"
                                                                                           ,"DHW.Fuel"
                                                                                           ,"DHW.Location"
                                                                                           ,"Gallon_bins"))]))
item104.os.data <- item104.os.data[which(item104.os.data$CK_Building_ID == subset.ind),]
#######################
# Weighted Analysis
#######################
item104.os.final <- proportionRowsAndColumns1(CustomerLevelData = item104.os.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'DHW.Location'
                                           ,rowVariable      = 'Gallon_bins'
                                           ,aggregateColumnName = "Remove")
item104.os.final <- item104.os.final[which(item104.os.final$DHW.Location != "Remove"),]

item104.os.all.fuels <- proportions_one_group(CustomerLevelData = item104.os.data
                                           ,valueVariable = "count"
                                           ,groupingVariable = "Gallon_bins"
                                           ,total.name = "All Locations"
                                           ,columnName = "DHW.Location"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)


item104.os.final <- rbind.data.frame(item104.os.final, item104.os.all.fuels)

item104.os.cast <- dcast(setDT(item104.os.final)
                      ,formula = BuildingType + DHW.Location ~ Gallon_bins
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item104.os.table <- data.frame("BuildingType"       = item104.os.cast$BuildingType
                            ,"DHW.Location"         = item104.os.cast$DHW.Location
                            ,"Percent_0.50.Gallons" = item104.os.cast$`w.percent_0-55 Gallons`
                            ,"SE_0.50.Gallons"      = item104.os.cast$`w.SE_0-55 Gallons`
                            ,"Percent_GT50.Gallons" = item104.os.cast$`w.percent_>55 Gallons`
                            ,"SE_GT50.Gallons"      = item104.os.cast$`w.SE_>55 Gallons`
                            ,"n"                    = item104.os.cast$n_Total
                            ,"EB_0.50.Gallons"      = item104.os.cast$`EB_0-55 Gallons`
                            ,"EB_GT50.Gallons"      = item104.os.cast$`EB_>55 Gallons`
)

# row ordering example code
unique(item104.os.table$DHW.Location)
rowOrder <- c("Basement"
              ,"Crawlspace"
              ,"Garage"
              ,"Main House"
              ,"Other"
              ,"All Locations")
item104.os.table <- item104.os.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)
item104.os.table <- data.frame(item104.os.table)


item104.os.table.SF <- item104.os.table[which(item104.os.table$BuildingType == "Single Family"),
                                  -which(colnames(item104.os.table) %in% c("BuildingType"))]

exportTable(item104.os.table.SF, "SF", "Table 111", weighted = TRUE, osIndicator = export.ind, OS = T)


#######################
# Unweighted Analysis
#######################
item104.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item104.os.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'DHW.Location'
                                                   ,rowVariable      = 'Gallon_bins'
                                                   ,aggregateColumnName = "Remove")
item104.os.final <- item104.os.final[which(item104.os.final$DHW.Location != "Remove"),]

item104.os.all.fuels <- proportions_one_group(CustomerLevelData = item104.os.data
                                           ,valueVariable = "count"
                                           ,groupingVariable = "Gallon_bins"
                                           ,total.name = "All Locations"
                                           ,columnName = "DHW.Location"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)

item104.os.final <- rbind.data.frame(item104.os.final, item104.os.all.fuels)

item104.os.cast <- dcast(setDT(item104.os.final)
                      ,formula = BuildingType + DHW.Location ~ Gallon_bins
                      ,value.var = c("Percent", "SE", "Count", "n"))
names(item104.os.cast)
item104.os.table <- data.frame("BuildingType"          = item104.os.cast$BuildingType
                               ,"DHW.Location"           = item104.os.cast$DHW.Location
                               ,"Percent_0.50.Gallons" = item104.os.cast$`Percent_0-55 Gallons`
                               ,"SE_0.50.Gallons"      = item104.os.cast$`SE_0-55 Gallons`
                               ,"Percent_GT50.Gallons" = item104.os.cast$`Percent_>55 Gallons`
                               ,"SE_GT50.Gallons"      = item104.os.cast$`SE_>55 Gallons`
                               ,"n"                    = item104.os.cast$n_Total
)

# row ordering example code
unique(item104.os.table$DHW.Location)
rowOrder <- c("Basement"
              ,"Crawlspace"
              ,"Garage"
              ,"Main House"
              ,"Other"
              ,"All Locations")
item104.os.table <- item104.os.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)
item104.os.table <- data.frame(item104.os.table)


item104.os.table.SF <- item104.os.table[which(item104.os.table$BuildingType == "Single Family"),
                                  -which(colnames(item104.os.table) %in% c("BuildingType"))]

exportTable(item104.os.table.SF, "SF", "Table 111", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
#Item 105: DISTRIBUTION OF WATER HEATERS BY SAMPLE (SF table 112, MH table 87)
#############################################################################################
#subset to columns needed for analysis
item105.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Location"
                                                                    ,"DHW.Size.(Gallons)"
                                                                    ,"DHW.Year.Manufactured"))]
item105.os.dat$count <- 1

item105.os.dat0 <- item105.os.dat[which(item105.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item105.os.dat1 <- left_join( os.dat,item105.os.dat0, by = "CK_Cadmus_ID")

item105.os.dat2 <- item105.os.dat1[which(!(is.na(item105.os.dat1$DHW.Year.Manufactured))),]
item105.os.dat3 <- item105.os.dat2[which(!(item105.os.dat2$DHW.Year.Manufactured %in% c("-- Datapoint not asked for --", "Unknown"))),]
unique(item105.os.dat3$DHW.Year.Manufactured)

# Bin equipment vintages for items 50 and 52 (4 categories)
item105.os.dat3$EquipVintage_bins <- as.numeric(as.character(item105.os.dat3$DHW.Year.Manufactured))
item105.os.dat4 <- item105.os.dat3[which(!is.na(item105.os.dat3$EquipVintage_bins)),]
item105.os.dat4$EquipVintage_bins[which(item105.os.dat4$DHW.Year.Manufactured < 1990)] <- "Pre 1990"
item105.os.dat4$EquipVintage_bins[which(item105.os.dat4$DHW.Year.Manufactured >= 1990 & item105.os.dat4$DHW.Year.Manufactured < 2000)] <- "1990-1999"
item105.os.dat4$EquipVintage_bins[which(item105.os.dat4$DHW.Year.Manufactured >= 2000 & item105.os.dat4$DHW.Year.Manufactured < 2005)] <- "2000-2004"
item105.os.dat4$EquipVintage_bins[which(item105.os.dat4$DHW.Year.Manufactured >= 2005 & item105.os.dat4$DHW.Year.Manufactured < 2010)] <- "2005-2009"
item105.os.dat4$EquipVintage_bins[which(item105.os.dat4$DHW.Year.Manufactured >= 2010 & item105.os.dat4$DHW.Year.Manufactured < 2015)] <- "2010-2014"
item105.os.dat4$EquipVintage_bins[which(item105.os.dat4$DHW.Year.Manufactured >= 2015)] <- "Post 2014"
#check uniques
unique(item105.os.dat4$EquipVintage_bins)


################################################
# Adding pop and sample sizes for weights
################################################
item105.os.data <- weightedData(item105.os.dat4[-which(colnames(item105.os.dat4) %in% c("Generic"
                                                                               ,"DHW.Size.(Gallons)"
                                                                               ,"DHW.Year.Manufactured"
                                                                               ,"DHW.Fuel"
                                                                               ,"DHW.Location"
                                                                               ,"count"
                                                                               ,"EquipVintage_bins"))])
item105.os.data <- left_join(item105.os.data, unique(item105.os.dat4[which(colnames(item105.os.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"Generic"
                                                                                         ,"DHW.Size.(Gallons)"
                                                                                         ,"DHW.Year.Manufactured"
                                                                                         ,"DHW.Fuel"
                                                                                         ,"DHW.Location"
                                                                                         ,"count"
                                                                                         ,"EquipVintage_bins"))]))
item105.os.data$m_ilk <- 1

#######################
# Weighted Analysis
#######################
item105.os.final <- proportionRowsAndColumns1(CustomerLevelData = item105.os.data
                                             ,valueVariable = "m_ilk"
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = "EquipVintage_bins"
                                             ,aggregateColumnName = "Remove")
item105.os.final <- item105.os.final[which(item105.os.final$CK_Building_ID != "Remove"),]

item105.os.cast <- dcast(setDT(item105.os.final)
                        ,formula = BuildingType + EquipVintage_bins ~ CK_Building_ID
                        ,value.var = c("w.percent", "w.SE","n", "EB"))
names(item105.os.cast)

if(os.ind == "scl"){
  item105.os.final <- data.frame("BuildingType"          = item105.os.cast$BuildingType
                                 ,"EquipVintage_bins"    = item105.os.cast$EquipVintage_bins
                                 ,"Percent_SCL.GenPop"   = item105.os.cast$`w.percent_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item105.os.cast$`w.SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item105.os.cast$`n_SCL GenPop`
                                 ,"Percent_SCL.LI"       = item105.os.cast$`w.percent_SCL LI`
                                 ,"SE_SCL.LI"            = item105.os.cast$`w.SE_SCL LI`
                                 ,"n_SCL.LI"             = item105.os.cast$`n_SCL LI`
                                 ,"Percent_SCL.EH"       = item105.os.cast$`w.percent_SCL EH`
                                 ,"SE_SCL.EH"            = item105.os.cast$`w.SE_SCL EH`
                                 ,"n_SCL.EH"             = item105.os.cast$`n_SCL EH`
                                 ,"Percent_2017.RBSA.PS" = item105.os.cast$`w.percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item105.os.cast$`w.SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item105.os.cast$`n_2017 RBSA PS`
                                 ,"EB_SCL.GenPop"        = item105.os.cast$`EB_SCL GenPop`
                                 ,"EB_SCL.LI"            = item105.os.cast$`EB_SCL LI`
                                 ,"EB_SCL.EH"            = item105.os.cast$`EB_SCL EH`
                                 ,"EB_2017.RBSA.PS"      = item105.os.cast$`EB_2017 RBSA PS`)

}else if(os.ind == "snopud"){
  item105.os.final <- data.frame("BuildingType"          = item105.os.cast$BuildingType
                                 ,"EquipVintage_bins"    = item105.os.cast$EquipVintage_bins
                                 ,"Percent_SnoPUD"          = item105.os.cast$`w.percent_SnoPUD`
                                 ,"SE_SnoPUD"               = item105.os.cast$`w.SE_SnoPUD`
                                 ,"n_SnoPUD"                = item105.os.cast$`n_SnoPUD`
                                 ,"Percent_2017.RBSA.PS"    = item105.os.cast$`w.percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"         = item105.os.cast$`w.SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"          = item105.os.cast$`n_2017 RBSA PS`
                                 ,"Percent_RBSA.NW"         = item105.os.cast$`w.percent_2017 RBSA NW`
                                 ,"SE_RBSA.NW"              = item105.os.cast$`w.SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"               = item105.os.cast$`n_2017 RBSA NW`
                                 ,"EB_SnoPUD"               = item105.os.cast$`EB_SnoPUD`
                                 ,"EB_2017.RBSA.PS"         = item105.os.cast$`EB_2017 RBSA PS`
                                 ,"EB_RBSA.NW"              = item105.os.cast$`EB_2017 RBSA NW`)
}


unique(item105.os.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item105.os.table <- item105.os.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)
item105.os.table <- data.frame(item105.os.table)

item105.os.final.SF <- item105.os.table[which(item105.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item105.os.table) %in% c("BuildingType"))]

exportTable(item105.os.final.SF, "SF", "Table 112", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item105.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item105.os.data
                                              ,valueVariable = "m_ilk"
                                              ,columnVariable = "CK_Building_ID"
                                              ,rowVariable = "EquipVintage_bins"
                                              ,aggregateColumnName = "Remove")
item105.os.final <- item105.os.final[which(item105.os.final$CK_Building_ID != "Remove"),]

item105.os.cast <- dcast(setDT(item105.os.final)
                         ,formula = BuildingType + EquipVintage_bins ~ CK_Building_ID
                         ,value.var = c("Percent", "SE","n"))

if(os.ind == "scl"){
  item105.os.final <- data.frame("BuildingType"          = item105.os.cast$BuildingType
                                 ,"EquipVintage_bins"    = item105.os.cast$EquipVintage_bins
                                 ,"Percent_SCL.GenPop"   = item105.os.cast$`Percent_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item105.os.cast$`SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item105.os.cast$`n_SCL GenPop`
                                 ,"Percent_SCL.LI"       = item105.os.cast$`Percent_SCL LI`
                                 ,"SE_SCL.LI"            = item105.os.cast$`SE_SCL LI`
                                 ,"n_SCL.LI"             = item105.os.cast$`n_SCL LI`
                                 ,"Percent_SCL.EH"       = item105.os.cast$`Percent_SCL EH`
                                 ,"SE_SCL.EH"            = item105.os.cast$`SE_SCL EH`
                                 ,"n_SCL.EH"             = item105.os.cast$`n_SCL EH`
                                 ,"Percent_2017.RBSA.PS" = item105.os.cast$`Percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item105.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item105.os.cast$`n_2017 RBSA PS`)

}else if(os.ind == "snopud"){
  item105.os.final <- data.frame("BuildingType"          = item105.os.cast$BuildingType
                                 ,"EquipVintage_bins"    = item105.os.cast$EquipVintage_bins
                                 ,"Percent_SnoPUD"          = item105.os.cast$`Percent_SnoPUD`
                                 ,"SE_SnoPUD"               = item105.os.cast$`SE_SnoPUD`
                                 ,"n_SnoPUD"                = item105.os.cast$`n_SnoPUD`
                                 ,"Percent_2017.RBSA.PS"    = item105.os.cast$`Percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"         = item105.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"          = item105.os.cast$`n_2017 RBSA PS`
                                 ,"Percent_RBSA.NW"         = item105.os.cast$`Percent_2017 RBSA NW`
                                 ,"SE_RBSA.NW"              = item105.os.cast$`SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"               = item105.os.cast$`n_2017 RBSA NW`)

}


unique(item105.os.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item105.os.table <- item105.os.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)
item105.os.table <- data.frame(item105.os.table)

item105.os.final.SF <- item105.os.table[which(item105.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item105.os.table) %in% c("BuildingType"))]

exportTable(item105.os.final.SF, "SF", "Table 112", weighted = FALSE, osIndicator = export.ind, OS = T)
