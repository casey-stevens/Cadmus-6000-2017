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
#Item 102: DISTRIBUTION OF TANK SIZE BY FUEL TYPE (SF table 109)
#############################################################################################
#subset to columns needed for analysis
item102.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,"DHW.Location"
                                                                   ,"DHW.Size.(Gallons)"
                                                                   ,""))]
item102.dat$count <- 1

item102.dat0 <- item102.dat[which(item102.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item102.dat1 <- left_join(item102.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item102.dat2 <- item102.dat1[grep("Water Heater",item102.dat1$Generic),]
item102.dat3 <- item102.dat2[which(!(is.na(item102.dat2$`DHW.Size.(Gallons)`))),]
item102.dat4 <- item102.dat3[which(!(item102.dat3$`DHW.Size.(Gallons)` %in% c(0, "Unknown"))),]

item102.dat4$`DHW.Size.(Gallons)` <- as.numeric(as.character(item102.dat4$`DHW.Size.(Gallons)`))

item102.dat4$Gallon_bins <- item102.dat4$`DHW.Size.(Gallons)`
item102.dat4$Gallon_bins[which(item102.dat4$`DHW.Size.(Gallons)` <= 55)] <- "0-55 Gallons"
item102.dat4$Gallon_bins[which(item102.dat4$`DHW.Size.(Gallons)` >  55)] <- ">55 Gallons"
unique(item102.dat4$Gallon_bins)

unique(item102.dat4$DHW.Fuel)

item102.merge <- left_join(rbsa.dat, item102.dat4)
item102.merge <- item102.merge[which(!is.na(item102.merge$Gallon_bins)),]

################################################
# Adding pop and sample sizes for weights
################################################
item102.data <- weightedData(item102.merge[-which(colnames(item102.merge) %in% c("Generic"               
                                                                              ,"count"
                                                                              ,"DHW.Size.(Gallons)"
                                                                              ,"DHW.Fuel"
                                                                              ,"DHW.Location"
                                                                              ,"Gallon_bins"))])
item102.data <- left_join(item102.data, item102.merge[which(colnames(item102.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Generic"               
                                                                                       ,"count"
                                                                                       ,"DHW.Size.(Gallons)"
                                                                                       ,"DHW.Fuel"
                                                                                       ,"DHW.Location"
                                                                                       ,"Gallon_bins"))])
#######################
# Weighted Analysis
#######################
item102.summary <- proportionRowsAndColumns1(CustomerLevelData = item102.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'DHW.Fuel'
                                          ,rowVariable      = 'Gallon_bins'
                                          ,aggregateColumnName = "Remove")
item102.summary <- item102.summary[which(item102.summary$DHW.Fuel != "Remove"),]

item102.all.fuels <- proportions_one_group(CustomerLevelData = item102.data
                                          ,valueVariable = "count"
                                          ,groupingVariable = "Gallon_bins"
                                          ,total.name = "All Fuel Types"
                                          ,columnName = "DHW.Fuel"
                                          ,weighted = TRUE
                                          ,two.prop.total = TRUE)

item102.final <- rbind.data.frame(item102.summary, item102.all.fuels)

item102.cast <- dcast(setDT(item102.final)
                     ,formula = BuildingType + DHW.Fuel ~ Gallon_bins
                     ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item102.table <- data.frame("BuildingType"          = item102.cast$BuildingType
                            ,"DHW.Fuel"             = item102.cast$DHW.Fuel
                            ,"Percent_0.50.Gallons" = item102.cast$`w.percent_0-55 Gallons`
                            ,"SE_0.50.Gallons"      = item102.cast$`w.SE_0-55 Gallons`
                            # ,"Count_0.50.Gallons"   = item102.cast$`count_0-55 Gallons`
                            # ,"n_0.50.Gallons"       = item102.cast$`n_0-55 Gallons`
                            ,"Percent_GT50.Gallons" = item102.cast$`w.percent_>55 Gallons`
                            ,"SE_GT50.Gallons"      = item102.cast$`w.SE_>55 Gallons`
                            # ,"Count_GT50.Gallons"   = item102.cast$`count_>55 Gallons`
                            # ,"n_GT50.Gallons"       = item102.cast$`n_>55 Gallons`
                            ,n = item102.cast$n_Total
                            ,"EB_0.50.Gallons"      = item102.cast$`EB_0-55 Gallons`
                            ,"EB_GT50.Gallons"      = item102.cast$`EB_>55 Gallons`
                            )

# row ordering example code
unique(item102.table$DHW.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"Oil"
              ,"Propane"
              ,"Unknown"
              ,"All Fuel Types")
item102.table <- item102.table %>% mutate(DHW.Fuel = factor(DHW.Fuel, levels = rowOrder)) %>% arrange(DHW.Fuel)  
item102.table <- data.frame(item102.table)


item102.table.SF <- item102.table[which(item102.table$BuildingType == "Single Family"),
                                  -which(colnames(item102.table) %in% c("BuildingType"))]

exportTable(item102.table.SF, "SF", "Table 109", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item102.final <- proportions_two_groups_unweighted(CustomerLevelData = item102.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'DHW.Fuel'
                                           ,rowVariable      = 'Gallon_bins'
                                           ,aggregateColumnName = "Remove")
item102.final <- item102.final[which(item102.final$DHW.Fuel != "Remove"),]

item102.all.fuels <- proportions_one_group(CustomerLevelData = item102.data
                                           ,valueVariable = "count"
                                           ,groupingVariable = "Gallon_bins"
                                           ,total.name = "All Fuel Types"
                                           ,columnName = "DHW.Fuel"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)

item102.final <- rbind.data.frame(item102.final, item102.all.fuels)

item102.cast <- dcast(setDT(item102.final)
                      ,formula = BuildingType + DHW.Fuel ~ Gallon_bins
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N"))

item102.table <- data.frame("BuildingType"          = item102.cast$BuildingType
                            ,"DHW.Fuel"             = item102.cast$DHW.Fuel
                            ,"Percent_0.50.Gallons" = item102.cast$`w.percent_0-55 Gallons`
                            ,"SE_0.50.Gallons"      = item102.cast$`w.SE_0-55 Gallons`
                            # ,"Count_0.50.Gallons"   = item102.cast$`count_0-55 Gallons`
                            # ,"n_0.50.Gallons"       = item102.cast$`n_0-55 Gallons`
                            ,"Percent_GT50.Gallons" = item102.cast$`w.percent_>55 Gallons`
                            ,"SE_GT50.Gallons"      = item102.cast$`w.SE_>55 Gallons`
                            # ,"Count_GT50.Gallons"   = item102.cast$`count_>55 Gallons`
                            # ,"n_GT50.Gallons"       = item102.cast$`n_>55 Gallons`
                            ,"n" = item102.cast$n_Total
)

# row ordering example code
unique(item102.table$DHW.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"Oil"
              ,"Propane"
              ,"Unknown"
              ,"All Fuel Types")
item102.table <- item102.table %>% mutate(DHW.Fuel = factor(DHW.Fuel, levels = rowOrder)) %>% arrange(DHW.Fuel)  
item102.table <- data.frame(item102.table)


item102.table.SF <- item102.table[which(item102.table$BuildingType == "Single Family"),
                                  -which(colnames(item102.table) %in% c("BuildingType"))]

exportTable(item102.table.SF, "SF", "Table 109", weighted = FALSE)





#############################################################################################
#Item 103: DISTRIBUTION OF ELECTRIC WATER HEATER TANK SIZE BY LOCATION (SF table 110)
#############################################################################################
#subset to columns needed for analysis
item103.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Location"
                                                                    ,"DHW.Size.(Gallons)"
                                                                    ,""))]
item103.dat$count <- 1

item103.dat0 <- item103.dat[which(item103.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item103.dat1 <- left_join(item103.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item103.dat2 <- item103.dat1[grep("Water Heater",item103.dat1$Generic),]
item103.dat3 <- item103.dat2[which(!(is.na(item103.dat2$`DHW.Size.(Gallons)`))),]
item103.dat4 <- item103.dat3[which(!(item103.dat3$`DHW.Size.(Gallons)` %in% c(0, "Unknown"))),]

item103.dat4$`DHW.Size.(Gallons)` <- as.numeric(as.character(item103.dat4$`DHW.Size.(Gallons)`))

#clean gallon bins
item103.dat4$Gallon_bins <- item103.dat4$`DHW.Size.(Gallons)`
item103.dat4$Gallon_bins[which(item103.dat4$`DHW.Size.(Gallons)` <= 55)] <- "0-55 Gallons"
item103.dat4$Gallon_bins[which(item103.dat4$`DHW.Size.(Gallons)` >  55)] <- ">55 Gallons"
unique(item103.dat4$Gallon_bins)

#clean location types
item103.dat5 <- item103.dat4[which(item103.dat4$DHW.Location != "Unknown"),]

item103.dat5$DHW.Location[grep("Crawl",item103.dat5$DHW.Location)] <- "Crawlspace"
item103.dat5$DHW.Location[grep("In building",item103.dat5$DHW.Location)] <- "Main House"

item103.dat5$DHW.Location[which(item103.dat5$DHW.Location %notin% c("Crawlspace"
                                                                    ,"Basement"
                                                                    ,"Garage"
                                                                    ,"Main House"))] <- "Other"

item103.dat5 <- item103.dat5[which(item103.dat5$DHW.Fuel == "Electric"),]

item103.merge <- left_join(rbsa.dat, item103.dat5)
item103.merge <- item103.merge[which(!is.na(item103.merge$count)),]

################################################
# Adding pop and sample sizes for weights
################################################
item103.data <- weightedData(item103.merge[-which(colnames(item103.merge) %in% c("Generic"               
                                                                                 ,"count"
                                                                                 ,"DHW.Size.(Gallons)"
                                                                                 ,"DHW.Fuel"
                                                                                 ,"DHW.Location"
                                                                                 ,"Gallon_bins"))])
item103.data <- left_join(item103.data, item103.merge[which(colnames(item103.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Generic"               
                                                                                       ,"count"
                                                                                       ,"DHW.Size.(Gallons)"
                                                                                       ,"DHW.Fuel"
                                                                                       ,"DHW.Location"
                                                                                       ,"Gallon_bins"))])
#######################
# Weighted Analysis
#######################
item103.final <- proportionRowsAndColumns1(CustomerLevelData = item103.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'DHW.Location'
                                           ,rowVariable      = 'Gallon_bins'
                                           ,aggregateColumnName = "Remove")
item103.final <- item103.final[which(item103.final$DHW.Location != "Remove"),]

item103.all.fuels <- proportions_one_group(CustomerLevelData = item103.data
                                           ,valueVariable = "count"
                                           ,groupingVariable = "Gallon_bins"
                                           ,total.name = "All Locations"
                                           ,columnName = "DHW.Location"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)

item103.final <- rbind.data.frame(item103.final, item103.all.fuels)

item103.cast <- dcast(setDT(item103.final)
                      ,formula = BuildingType + DHW.Location ~ Gallon_bins
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item103.table <- data.frame("BuildingType"          = item103.cast$BuildingType
                            ,"DHW.Location"         = item103.cast$DHW.Location
                            ,"Percent_0.50.Gallons" = item103.cast$`w.percent_0-55 Gallons`
                            ,"SE_0.50.Gallons"      = item103.cast$`w.SE_0-55 Gallons`
                            # ,"Count_0.50.Gallons"   = item103.cast$`count_0-55 Gallons`
                            # ,"n_0.50.Gallons"       = item103.cast$`n_0-55 Gallons`
                            ,"Percent_GT50.Gallons" = item103.cast$`w.percent_>55 Gallons`
                            ,"SE_GT50.Gallons"      = item103.cast$`w.SE_>55 Gallons`
                            # ,"Count_GT50.Gallons"   = item103.cast$`count_>55 Gallons`
                            # ,"n_GT50.Gallons"       = item103.cast$`n_>55 Gallons`
                            ,"n" = item103.cast$n_Total
                            ,"EB_0.50.Gallons"      = item103.cast$`EB_0-55 Gallons`
                            ,"EB_GT50.Gallons"      = item103.cast$`EB_>55 Gallons`
)

# row ordering example code
unique(item103.table$DHW.Location)
rowOrder <- c("Basement"
              ,"Crawlspace"
              ,"Garage"
              ,"Main House"
              ,"Other"
              ,"All Locations")
item103.table <- item103.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
item103.table <- data.frame(item103.table)



item103.table.SF <- item103.table[which(item103.table$BuildingType == "Single Family"),
                                  -which(colnames(item103.table) %in% c("BuildingType"))]

exportTable(item103.table.SF, "SF", "Table 110", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item103.final <- proportions_two_groups_unweighted(CustomerLevelData = item103.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'DHW.Location'
                                                   ,rowVariable      = 'Gallon_bins'
                                                   ,aggregateColumnName = "Remove")
item103.final <- item103.final[which(item103.final$DHW.Location != "Remove"),]

item103.all.fuels <- proportions_one_group(CustomerLevelData = item103.data
                                           ,valueVariable = "count"
                                           ,groupingVariable = "Gallon_bins"
                                           ,total.name = "All Locations"
                                           ,columnName = "DHW.Location"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)

item103.final <- rbind.data.frame(item103.final, item103.all.fuels)

item103.cast <- dcast(setDT(item103.final)
                      ,formula = BuildingType + DHW.Location ~ Gallon_bins
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N"))

item103.table <- data.frame("BuildingType"          = item103.cast$BuildingType
                            ,"DHW.Location"         = item103.cast$DHW.Location
                            ,"Percent_0.50.Gallons" = item103.cast$`w.percent_0-55 Gallons`
                            ,"SE_0.50.Gallons"      = item103.cast$`w.SE_0-55 Gallons`
                            # ,"Count_0.50.Gallons"   = item103.cast$`count_0-55 Gallons`
                            # ,"n_0.50.Gallons"       = item103.cast$`n_0-55 Gallons`
                            ,"Percent_GT50.Gallons" = item103.cast$`w.percent_>55 Gallons`
                            ,"SE_GT50.Gallons"      = item103.cast$`w.SE_>55 Gallons`
                            # ,"Count_GT50.Gallons"   = item103.cast$`count_>55 Gallons`
                            # ,"n_GT50.Gallons"       = item103.cast$`n_>55 Gallons`
                            ,"n" = item103.cast$n_Total
)

# row ordering example code
unique(item103.table$DHW.Location)
rowOrder <- c("Basement"
              ,"Crawlspace"
              ,"Garage"
              ,"Main House"
              ,"Other"
              ,"All Locations")
item103.table <- item103.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
item103.table <- data.frame(item103.table)


item103.table.SF <- item103.table[which(item103.table$BuildingType == "Single Family"),
                                  -which(colnames(item103.table) %in% c("BuildingType"))]

exportTable(item103.table.SF, "SF", "Table 110", weighted = FALSE)





#############################################################################################
#Item 104: DISTRIBUTION OF GAS WATER HEATER TANK SIZE BY LOCATION (SF table 111)
#############################################################################################
#subset to columns needed for analysis
item104.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Location"
                                                                    ,"DHW.Size.(Gallons)"
                                                                    ,""))]
item104.dat$count <- 1

item104.dat0 <- item104.dat[which(item104.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item104.dat1 <- left_join(item104.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item104.dat2 <- item104.dat1[grep("Water Heater",item104.dat1$Generic),]
item104.dat3 <- item104.dat2[which(!(is.na(item104.dat2$`DHW.Size.(Gallons)`))),]
item104.dat4 <- item104.dat3[which(!(item104.dat3$`DHW.Size.(Gallons)` %in% c(0, "Unknown"))),]

item104.dat4$`DHW.Size.(Gallons)` <- as.numeric(as.character(item104.dat4$`DHW.Size.(Gallons)`))

#clean gallon bins
item104.dat4$Gallon_bins <- item104.dat4$`DHW.Size.(Gallons)`
item104.dat4$Gallon_bins[which(item104.dat4$`DHW.Size.(Gallons)` <= 55)] <- "0-55 Gallons"
item104.dat4$Gallon_bins[which(item104.dat4$`DHW.Size.(Gallons)` >  55)] <- ">55 Gallons"
unique(item104.dat4$Gallon_bins)

#clean location types
item104.dat5 <- item104.dat4[which(item104.dat4$DHW.Location != "Unknown"),]

item104.dat5$DHW.Location[grep("Crawl",item104.dat5$DHW.Location)] <- "Crawlspace"
item104.dat5$DHW.Location[grep("In building",item104.dat5$DHW.Location)] <- "Main House"

item104.dat5$DHW.Location[which(item104.dat5$DHW.Location %notin% c("Crawlspace"
                                                                    ,"Basement"
                                                                    ,"Garage"
                                                                    ,"Main House"))] <- "Other"

item104.dat6 <- item104.dat5[which(item104.dat5$DHW.Fuel == "Natural Gas"),]

item104.merge <- left_join(rbsa.dat, item104.dat6)
item104.merge <- item104.merge[which(!is.na(item104.merge$count)),]

################################################
# Adding pop and sample sizes for weights
################################################
item104.data <- weightedData(item104.merge[-which(colnames(item104.merge) %in% c("Generic"               
                                                                                 ,"count"
                                                                                 ,"DHW.Size.(Gallons)"
                                                                                 ,"DHW.Fuel"
                                                                                 ,"DHW.Location"
                                                                                 ,"Gallon_bins"))])
item104.data <- left_join(item104.data, item104.merge[which(colnames(item104.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Generic"               
                                                                                           ,"count"
                                                                                           ,"DHW.Size.(Gallons)"
                                                                                           ,"DHW.Fuel"
                                                                                           ,"DHW.Location"
                                                                                           ,"Gallon_bins"))])
#######################
# Weighted Analysis
#######################
item104.final <- proportionRowsAndColumns1(CustomerLevelData = item104.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'DHW.Location'
                                           ,rowVariable      = 'Gallon_bins'
                                           ,aggregateColumnName = "Remove")
item104.final <- item104.final[which(item104.final$DHW.Location != "Remove"),]

item104.all.fuels <- proportions_one_group(CustomerLevelData = item104.data
                                           ,valueVariable = "count"
                                           ,groupingVariable = "Gallon_bins"
                                           ,total.name = "All Locations"
                                           ,columnName = "DHW.Location"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)


item104.final <- rbind.data.frame(item104.final, item104.all.fuels)

item104.cast <- dcast(setDT(item104.final)
                      ,formula = BuildingType + DHW.Location ~ Gallon_bins
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item104.table <- data.frame("BuildingType"          = item104.cast$BuildingType
                            ,"DHW.Location"         = item104.cast$DHW.Location
                            ,"Percent_0.50.Gallons" = item104.cast$`w.percent_0-55 Gallons`
                            ,"SE_0.50.Gallons"      = item104.cast$`w.SE_0-55 Gallons`
                            # ,"Count_0.50.Gallons"   = item104.cast$`count_0-55 Gallons`
                            # ,"n_0.50.Gallons"       = item104.cast$`n_0-55 Gallons`
                            ,"Percent_GT50.Gallons" = item104.cast$`w.percent_>55 Gallons`
                            ,"SE_GT50.Gallons"      = item104.cast$`w.SE_>55 Gallons`
                            # ,"Count_GT50.Gallons"   = item104.cast$`count_>55 Gallons`
                            # ,"n_GT50.Gallons"       = item104.cast$`n_>55 Gallons`
                            ,"n" = item104.cast$n_Total
                            ,"EB_0.50.Gallons"      = item104.cast$`EB_0-55 Gallons`
                            ,"EB_GT50.Gallons"      = item104.cast$`EB_>55 Gallons`
)

# row ordering example code
unique(item104.table$DHW.Location)
rowOrder <- c("Basement"
              ,"Crawlspace"
              ,"Garage"
              ,"Main House"
              ,"Other"
              ,"All Locations")
item104.table <- item104.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
item104.table <- data.frame(item104.table)


item104.table.SF <- item104.table[which(item104.table$BuildingType == "Single Family"),
                                  -which(colnames(item104.table) %in% c("BuildingType"))]

exportTable(item104.table.SF, "SF", "Table 111", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item104.final <- proportions_two_groups_unweighted(CustomerLevelData = item104.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'DHW.Location'
                                                   ,rowVariable      = 'Gallon_bins'
                                                   ,aggregateColumnName = "Remove")
item104.final <- item104.final[which(item104.final$DHW.Location != "Remove"),]

item104.all.fuels <- proportions_one_group(CustomerLevelData = item104.data
                                           ,valueVariable = "count"
                                           ,groupingVariable = "Gallon_bins"
                                           ,total.name = "All Locations"
                                           ,columnName = "DHW.Location"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)

item104.final <- rbind.data.frame(item104.final, item104.all.fuels)

item104.cast <- dcast(setDT(item104.final)
                      ,formula = BuildingType + DHW.Location ~ Gallon_bins
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N"))

item104.table <- data.frame("BuildingType"          = item104.cast$BuildingType
                            ,"DHW.Location"         = item104.cast$DHW.Location
                            ,"Percent_0.50.Gallons" = item104.cast$`w.percent_0-55 Gallons`
                            ,"SE_0.50.Gallons"      = item104.cast$`w.SE_0-55 Gallons`
                            # ,"Count_0.50.Gallons"   = item104.cast$`count_0-55 Gallons`
                            # ,"n_0.50.Gallons"       = item104.cast$`n_0-55 Gallons`
                            ,"Percent_GT50.Gallons" = item104.cast$`w.percent_>55 Gallons`
                            ,"SE_GT50.Gallons"      = item104.cast$`w.SE_>55 Gallons`
                            # ,"Count_GT50.Gallons"   = item104.cast$`count_>55 Gallons`
                            # ,"n_GT50.Gallons"       = item104.cast$`n_>55 Gallons`
                            ,"n" = item104.cast$n_Total
)

# row ordering example code
unique(item104.table$DHW.Location)
rowOrder <- c("Basement"
              ,"Crawlspace"
              ,"Garage"
              ,"Main House"
              ,"Other"
              ,"All Locations")
item104.table <- item104.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
item104.table <- data.frame(item104.table)


item104.table.SF <- item104.table[which(item104.table$BuildingType == "Single Family"),
                                  -which(colnames(item104.table) %in% c("BuildingType"))]

exportTable(item104.table.SF, "SF", "Table 111", weighted = FALSE)







#############################################################################################
#Item 105: DISTRIBUTION OF WATER HEATERS BY VINTAGE (SF table 112, MH table 87)
#############################################################################################
#subset to columns needed for analysis
item105.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Location"
                                                                    ,"DHW.Size.(Gallons)"
                                                                    ,"DHW.Year.Manufactured"))]
item105.dat$count <- 1

item105.dat0 <- item105.dat[which(item105.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item105.dat1 <- left_join( rbsa.dat,item105.dat0, by = "CK_Cadmus_ID")

item105.dat2 <- item105.dat1[which(!(is.na(item105.dat1$DHW.Year.Manufactured))),]
item105.dat3 <- item105.dat2[which(!(item105.dat2$DHW.Year.Manufactured %in% c("-- Datapoint not asked for --", "Unknown"))),]
unique(item105.dat3$DHW.Year.Manufactured)

# Bin equipment vintages for items 50 and 52 (4 categories)
item105.dat3$EquipVintage_bins <- as.numeric(as.character(item105.dat3$DHW.Year.Manufactured))

item105.dat3$EquipVintage_bins[which(item105.dat3$DHW.Year.Manufactured < 1990)] <- "Pre 1990"
item105.dat3$EquipVintage_bins[which(item105.dat3$DHW.Year.Manufactured >= 1990 & item105.dat3$DHW.Year.Manufactured < 2000)] <- "1990-1999"
item105.dat3$EquipVintage_bins[which(item105.dat3$DHW.Year.Manufactured >= 2000 & item105.dat3$DHW.Year.Manufactured < 2005)] <- "2000-2004"
item105.dat3$EquipVintage_bins[which(item105.dat3$DHW.Year.Manufactured >= 2005 & item105.dat3$DHW.Year.Manufactured < 2010)] <- "2005-2009"
item105.dat3$EquipVintage_bins[which(item105.dat3$DHW.Year.Manufactured >= 2010 & item105.dat3$DHW.Year.Manufactured < 2015)] <- "2010-2014"
item105.dat3$EquipVintage_bins[which(item105.dat3$DHW.Year.Manufactured >= 2015)] <- "Post 2014"
#check uniques
unique(item105.dat3$EquipVintage_bins)


################################################
# Adding pop and sample sizes for weights
################################################
item105.data <- weightedData(item105.dat3[-which(colnames(item105.dat3) %in% c("Generic"
                                                                               ,"DHW.Size.(Gallons)"
                                                                               ,"DHW.Year.Manufactured"
                                                                               ,"DHW.Fuel"
                                                                               ,"DHW.Location"
                                                                               ,"count"
                                                                               ,"EquipVintage_bins"))])
item105.data <- left_join(item105.data, item105.dat3[which(colnames(item105.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"Generic"
                                                                                         ,"DHW.Size.(Gallons)"
                                                                                         ,"DHW.Year.Manufactured"
                                                                                         ,"DHW.Fuel"
                                                                                         ,"DHW.Location"
                                                                                         ,"count"
                                                                                         ,"EquipVintage_bins"))])

#######################
# Weighted Analysis
#######################
item105.final <- proportions_one_group(CustomerLevelData  = item105.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'EquipVintage_bins'
                                      , total.name       = "Total")

unique(item105.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item105.table <- item105.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item105.table <- data.frame(item105.table)


# SF = Table 112, MH = Table 87
# Export table
item105.final.SF <- item105.table[which(item105.table$BuildingType == "Single Family")
                                  ,-which(colnames(item105.table) %in% c("BuildingType"
                                                                         ,"Water.Heaters"))]
item105.final.MH <- item105.table[which(item105.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item105.table) %in% c("BuildingType"
                                                                         ,"Water.Heaters"))]

exportTable(item105.final.SF, "SF", "Table 112", weighted = TRUE)
exportTable(item105.final.MH, "MH", "Table 87", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item105.final <- proportions_one_group(CustomerLevelData  = item105.data
                                       , valueVariable    = 'count'
                                       , groupingVariable = 'EquipVintage_bins'
                                       , total.name       = "Total"
                                       , weighted         = FALSE)
unique(item105.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item105.table <- item105.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item105.table <- data.frame(item105.table)


# SF = Table 112, MH = Table 87
# Export table
item105.final.SF <- item105.table[which(item105.table$BuildingType == "Single Family")
                                  ,-which(colnames(item105.table) %in% c("BuildingType"
                                                                         ,"Water.Heaters"
                                                                         ,"Total.Count"))]
item105.final.MH <- item105.table[which(item105.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item105.table) %in% c("BuildingType"
                                                                         ,"Water.Heaters"
                                                                         ,"Total.Count"))]

exportTable(item105.final.SF, "SF", "Table 112", weighted = FALSE)
exportTable(item105.final.MH, "MH", "Table 87", weighted = FALSE)

