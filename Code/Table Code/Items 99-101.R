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
# mechanical.dat <- read.xlsx(mechanical.export)
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

item99.ind2 <- item99.dat1$CK_Cadmus_ID[grep("Yes",item99.dat1$Primary.Heating.System, ignore.case = T)]
item99.dat3 <- item99.dat2[which(item99.dat2$CK_Cadmus_ID %in% item99.ind2),]

#clean heating fuel type
unique(item99.dat3$Heating.Fuel)
item99.dat3$Heating.Fuel[grep("Gas",item99.dat3$Heating.Fuel, ignore.case = T)]                      <- "Gas"
item99.dat3$Heating.Fuel[grep("unk|N/A|Other|from|can't",item99.dat3$Heating.Fuel, ignore.case = T)] <- NA
item99.dat3$Heating.Fuel[grep("oil|kero",item99.dat3$Heating.Fuel, ignore.case = T)]                 <- "Oil"
item99.dat3$Heating.Fuel[grep("Elect",item99.dat3$Heating.Fuel, ignore.case = T)]                    <- "Electric"
item99.dat3$Heating.Fuel[which(item99.dat3$Heating.Fuel == "Wood (Pellets)")]                        <- "Pellets"
item99.dat3$Heating.Fuel[which(item99.dat3$Heating.Fuel == "Wood (Cord)")]                           <- "Wood"

item99.dat4 <- item99.dat3[which(!is.na(item99.dat3$Heating.Fuel)),]

unique(item99.dat4$Primary.Heating.System)

#summarise for primary heating system
item99.heat <- item99.dat4[which(item99.dat4$Primary.Heating.System == "Yes"),]

item99.sum1 <- summarise(group_by(item99.heat, CK_Cadmus_ID, BuildingType, Heating.Fuel)
                         ,Count = sum(count))

item99.sum1$Count <- 1
unique(item99.sum1$Heating.Fuel)
item99.unique <- unique(item99.sum1)

#find duplicates of 
unique(item99.unique$CK_Cadmus_ID[which(duplicated(item99.unique$CK_Cadmus_ID))])


item99.heat.final <- item99.unique
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

unique(item99.join$DHW.Location)
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
#################################
# Single Family Analysis
#################################
#######################
# Weighted Analysis
#######################
item99.summary <- proportionRowsAndColumns1(CustomerLevelData = item99.data
                                            ,valueVariable    = 'DHW.Count'
                                            ,columnVariable   = 'Heating.Fuel'
                                            ,rowVariable      = 'DHW.Location'
                                            ,aggregateColumnName = "Remove")
item99.summary <- item99.summary[which(item99.summary$Heating.Fuel %notin% c("Remove")),]

item99.all.fuels <- proportions_one_group(CustomerLevelData = item99.data
                                          ,valueVariable = "DHW.Count"
                                          ,groupingVariable = "DHW.Location"
                                          ,total.name = "All Fuels"
                                          ,columnName = "Heating.Fuel"
                                          ,weighted = TRUE
                                          ,two.prop.total = TRUE)

item99.final <- rbind.data.frame(item99.summary, item99.all.fuels, stringsAsFactors = F)

item99.cast <- dcast(setDT(item99.final)
                     ,formula = BuildingType + DHW.Location ~ Heating.Fuel
                     ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
names(item99.cast)
item99.table <- data.frame("BuildingType"         = item99.cast$BuildingType
                           ,"DHW.Location"        = item99.cast$DHW.Location
                           ,"Percent.Electric"    = item99.cast$w.percent_Electric
                           ,"SE.Electric"         = item99.cast$w.SE_Electric
                           ,"n.Electric"          = item99.cast$n_Electric
                           ,"Percent.Natural.Gas" = item99.cast$w.percent_Gas
                           ,"SE.Natural.Gas"      = item99.cast$w.SE_Gas
                           ,"n.Gas"               = item99.cast$n_Gas
                           ,"Percent.Oil"         = item99.cast$w.percent_Oil
                           ,"SE.Oil"              = item99.cast$w.SE_Oil
                           ,"n.Oil"               = item99.cast$n_Oil
                           ,"Percent.Pellets"     = item99.cast$w.percent_Pellets
                           ,"SE.Pellets"          = item99.cast$w.SE_Pellets
                           ,"n.Pellets"           = item99.cast$n_Pellets
                           ,"Percent.Propane"     = item99.cast$w.percent_Propane
                           ,"SE.Propane"          = item99.cast$w.SE_Propane
                           ,"n.Propane"           = item99.cast$n_Propane
                           ,"Percent.Wood"        = item99.cast$w.percent_Wood
                           ,"SE.Wood"             = item99.cast$w.SE_Wood
                           ,"n.Wood"              = item99.cast$n_Wood
                           ,"Percent.All.Heating.Fuel.Types" = item99.cast$`w.percent_All Fuels`
                           ,"SE.All.Heating.Fuel.Types"      = item99.cast$`w.SE_All Fuels`
                           ,"n.All.Heating.Fuel.Types"       = item99.cast$`n_All Fuels`
                           ,"EB_Electric"          = item99.cast$EB_Electric
                           ,"EB_Gas"               = item99.cast$EB_Gas
                           ,"EB_Oil"               = item99.cast$EB_Oil
                           ,"EB_Pellets"           = item99.cast$EB_Pellets
                           ,"EB_Propane"           = item99.cast$EB_Propane
                           ,"EB_Wood"              = item99.cast$EB_Wood
                           ,"EB_All.Heating.Fuels" = item99.cast$`EB_All Fuels`)

levels(item99.table$DHW.Location)
rowOrder <- c("Basement"
              ,"Crawlspace"
              ,"Garage"
              ,"Other"
              ,"Main House"
              ,"Total")
item99.table <- item99.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
item99.table <- data.frame(item99.table)

item99.final.SF <- item99.table[which(item99.table$BuildingType == "Single Family")
                                ,-which(colnames(item99.table) %in% c("BuildingType"))]
exportTable(item99.final.SF, "SF", "Table 106", weighted = TRUE)


#######################
# unweighted Analysis
#######################
item99.summary <- proportions_two_groups_unweighted(CustomerLevelData = item99.data
                                                    ,valueVariable    = 'DHW.Count'
                                                    ,columnVariable   = 'Heating.Fuel'
                                                    ,rowVariable      = 'DHW.Location'
                                                    ,aggregateColumnName = "Remove")
item99.summary <- item99.summary[which(item99.summary$Heating.Fuel %notin% c("Remove")),]

item99.all.fuels <- proportions_one_group(CustomerLevelData = item99.data
                                          ,valueVariable = "DHW.Count"
                                          ,groupingVariable = "DHW.Location"
                                          ,total.name = "All Fuels"
                                          ,columnName = "Heating.Fuel"
                                          ,weighted = FALSE
                                          ,two.prop.total = TRUE)

item99.final <- rbind.data.frame(item99.summary, item99.all.fuels, stringsAsFactors = F)

item99.cast <- dcast(setDT(item99.final)
                     ,formula = BuildingType + DHW.Location ~ Heating.Fuel
                     ,value.var = c("Percent", "SE", "Count", "n"))

item99.table <- data.frame("BuildingType"         = item99.cast$BuildingType
                           ,"DHW.Location"        = item99.cast$DHW.Location
                           ,"Percent.Electric"    = item99.cast$Percent_Electric
                           ,"SE.Electric"         = item99.cast$SE_Electric
                           ,"n.Electric"          = item99.cast$n_Electric
                           ,"Percent.Natural.Gas" = item99.cast$Percent_Gas
                           ,"SE.Natural.Gas"      = item99.cast$SE_Gas
                           ,"n.Gas"               = item99.cast$n_Gas
                           ,"Percent.Oil"         = item99.cast$Percent_Oil
                           ,"SE.Oil"              = item99.cast$SE_Oil
                           ,"n.Oil"               = item99.cast$n_Oil
                           ,"Percent.Pellets"     = item99.cast$Percent_Pellets
                           ,"SE.Pellets"          = item99.cast$SE_Pellets
                           ,"n.Pellets"           = item99.cast$n_Pellets
                           ,"Percent.Propane"     = item99.cast$Percent_Propane
                           ,"SE.Propane"          = item99.cast$SE_Propane
                           ,"n.Propane"           = item99.cast$n_Propane
                           ,"Percent.Wood"        = item99.cast$Percent_Wood
                           ,"SE.Wood"             = item99.cast$SE_Wood
                           ,"n.Wood"              = item99.cast$n_Wood
                           ,"Percent.All.Heating.Fuel.Types" = item99.cast$`Percent_All Fuels`
                           ,"SE.All.Heating.Fuel.Types"      = item99.cast$`SE_All Fuels`
                           ,"n_All.Heating.Fuel.Types"       = item99.cast$`n_All Fuels`)

levels(item99.table$DHW.Location)
rowOrder <- c("Basement"
              ,"Crawlspace"
              ,"Garage"
              ,"Other"
              ,"Main House"
              ,"All Locations")
item99.table <- item99.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
item99.table <- data.frame(item99.table)

item99.final.SF <- item99.table[which(item99.table$BuildingType == "Single Family")
                                ,-which(colnames(item99.table) %in% c("BuildingType"))]

exportTable(item99.final.SF, "SF", "Table 106", weighted = FALSE)


#################################
# Manufactured Analysis
#################################
#######################
# Weighted Analysis
#######################
item99.summary <- proportionRowsAndColumns1(CustomerLevelData = item99.data
                                            ,valueVariable    = 'DHW.Count'
                                            ,columnVariable   = 'DHW.Location'
                                            ,rowVariable      = 'Heating.Fuel'
                                            ,aggregateColumnName = "Remove")
item99.summary <- item99.summary[which(item99.summary$DHW.Location %notin% c("Remove")),]
item99.summary <- item99.summary[which(item99.summary$Heating.Fuel %notin% c("Total")),]

item99.all.fuels <- proportions_one_group(CustomerLevelData = item99.data
                                          ,valueVariable = "DHW.Count"
                                          ,groupingVariable = "DHW.Location"
                                          ,total.name = "All Fuels"
                                          ,columnName = "Heating.Fuel"
                                          ,weighted = TRUE
                                          ,two.prop.total = TRUE)
item99.all.fuels <- item99.all.fuels[which(item99.all.fuels$DHW.Location != "Total"),]# <- "All Locations"

item99.locations <- proportions_one_group(CustomerLevelData = item99.data
                                          ,valueVariable = "DHW.Count"
                                          ,groupingVariable = "Heating.Fuel"
                                          ,total.name = "All Locations"
                                          ,columnName = "DHW.Location"
                                          ,weighted = TRUE
                                          ,two.prop.total = TRUE)
item99.locations$Heating.Fuel[which(item99.locations$Heating.Fuel == "Total")] <- "All Fuels"

item99.final <- rbind.data.frame(item99.summary, item99.all.fuels, item99.locations, stringsAsFactors = F)

item99.cast <- dcast(setDT(item99.final)
                     ,formula = BuildingType + DHW.Location ~ Heating.Fuel
                     ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
names(item99.cast)
item99.table <- data.frame("BuildingType"         = item99.cast$BuildingType
                           ,"DHW.Location"        = item99.cast$DHW.Location
                           ,"Percent.Electric"    = item99.cast$w.percent_Electric
                           ,"SE.Electric"         = item99.cast$w.SE_Electric
                           ,"n.Electric"          = item99.cast$n_Electric
                           ,"Percent.Natural.Gas" = item99.cast$w.percent_Gas
                           ,"SE.Natural.Gas"      = item99.cast$w.SE_Gas
                           ,"n.Gas"               = item99.cast$n_Gas
                           ,"Percent.Oil"         = item99.cast$w.percent_Oil
                           ,"SE.Oil"              = item99.cast$w.SE_Oil
                           ,"n.Oil"               = item99.cast$n_Oil
                           ,"Percent.Pellets"     = item99.cast$w.percent_Pellets
                           ,"SE.Pellets"          = item99.cast$w.SE_Pellets
                           ,"n.Pellets"           = item99.cast$n_Pellets
                           ,"Percent.Propane"     = item99.cast$w.percent_Propane
                           ,"SE.Propane"          = item99.cast$w.SE_Propane
                           ,"n.Propane"           = item99.cast$n_Propane
                           ,"Percent.Wood"        = item99.cast$w.percent_Wood
                           ,"SE.Wood"             = item99.cast$w.SE_Wood
                           ,"n.Wood"              = item99.cast$n_Wood
                           ,"Percent.All.Heating.Fuel.Types" = item99.cast$`w.percent_All Fuels`
                           ,"SE.All.Heating.Fuel.Types"      = item99.cast$`w.SE_All Fuels`
                           ,"n.All.Heating.Fuel.Types"       = item99.cast$`n_All Fuels`
                           ,"EB_Electric"          = item99.cast$EB_Electric
                           ,"EB_Gas"               = item99.cast$EB_Gas
                           ,"EB_Oil"               = item99.cast$EB_Oil
                           ,"EB_Pellets"           = item99.cast$EB_Pellets
                           ,"EB_Propane"           = item99.cast$EB_Propane
                           ,"EB_Wood"              = item99.cast$EB_Wood
                           ,"EB_All.Heating.Fuels" = item99.cast$`EB_All Fuels`)

levels(item99.table$DHW.Location)
rowOrder <- c("Basement"
              ,"Crawlspace"
              ,"Garage"
              ,"Other"
              ,"Main House"
              ,"All Locations")
item99.table <- item99.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
item99.table <- data.frame(item99.table)

item99.final.MH <- item99.table[which(item99.table$BuildingType == "Manufactured")
                                ,-which(colnames(item99.table) %in% c("BuildingType"))]

# exportTable(item99.final.MH, "MH", "Table 86", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item99.summary <- proportions_two_groups_unweighted(CustomerLevelData = item99.data
                                                    ,valueVariable    = 'DHW.Count'
                                                    ,columnVariable   = 'DHW.Location'
                                                    ,rowVariable      = 'Heating.Fuel'
                                                    ,aggregateColumnName = "Remove")
item99.summary <- item99.summary[which(item99.summary$DHW.Location %notin% c("Remove")),]
item99.summary <- item99.summary[which(item99.summary$Heating.Fuel %notin% c("Total")),]

item99.all.fuels <- proportions_one_group(CustomerLevelData = item99.data
                                          ,valueVariable = "DHW.Count"
                                          ,groupingVariable = "DHW.Location"
                                          ,total.name = "All Fuels"
                                          ,columnName = "Heating.Fuel"
                                          ,weighted = FALSE
                                          ,two.prop.total = TRUE)
item99.all.fuels <- item99.all.fuels[which(item99.all.fuels$DHW.Location != "Total"),]# <- "All Locations"

item99.locations <- proportions_one_group(CustomerLevelData = item99.data
                                          ,valueVariable = "DHW.Count"
                                          ,groupingVariable = "Heating.Fuel"
                                          ,total.name = "All Locations"
                                          ,columnName = "DHW.Location"
                                          ,weighted = FALSE
                                          ,two.prop.total = TRUE)
item99.locations$Heating.Fuel[which(item99.locations$Heating.Fuel == "Total")] <- "All Fuels"

item99.final <- rbind.data.frame(item99.summary, item99.all.fuels, item99.locations, stringsAsFactors = F)

item99.cast <- dcast(setDT(item99.final)
                     ,formula = BuildingType + DHW.Location ~ Heating.Fuel
                     ,value.var = c("Percent", "SE", "Count", "n"))

item99.table <- data.frame("BuildingType"         = item99.cast$BuildingType
                           ,"DHW.Location"        = item99.cast$DHW.Location
                           ,"Percent.Electric"    = item99.cast$Percent_Electric
                           ,"SE.Electric"         = item99.cast$SE_Electric
                           ,"n.Electric"          = item99.cast$n_Electric
                           ,"Percent.Natural.Gas" = item99.cast$Percent_Gas
                           ,"SE.Natural.Gas"      = item99.cast$SE_Gas
                           ,"n.Gas"               = item99.cast$n_Gas
                           ,"Percent.Oil"         = item99.cast$Percent_Oil
                           ,"SE.Oil"              = item99.cast$SE_Oil
                           ,"n.Oil"               = item99.cast$n_Oil
                           ,"Percent.Pellets"     = item99.cast$Percent_Pellets
                           ,"SE.Pellets"          = item99.cast$SE_Pellets
                           ,"n.Pellets"           = item99.cast$n_Pellets
                           ,"Percent.Propane"     = item99.cast$Percent_Propane
                           ,"SE.Propane"          = item99.cast$SE_Propane
                           ,"n.Propane"           = item99.cast$n_Propane
                           ,"Percent.Wood"        = item99.cast$Percent_Wood
                           ,"SE.Wood"             = item99.cast$SE_Wood
                           ,"n.Wood"              = item99.cast$n_Wood
                           ,"Percent.All.Heating.Fuel.Types" = item99.cast$`Percent_All Fuels`
                           ,"SE.All.Heating.Fuel.Types"      = item99.cast$`SE_All Fuels`
                           ,"n_All.Heating.Fuel.Types"       = item99.cast$`n_All Fuels`)

levels(item99.table$DHW.Location)
rowOrder <- c("Basement"
              ,"Crawlspace"
              ,"Garage"
              ,"Other"
              ,"Main House"
              ,"All Locations")
item99.table <- item99.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
item99.table <- data.frame(item99.table)

item99.final.MH <- item99.table[which(item99.table$BuildingType == "Manufactured")
                                ,-which(colnames(item99.table) %in% c("BuildingType"))]

# exportTable(item99.final.MH, "MH", "Table 86", weighted = FALSE)







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

item100.dat3 <- item100.dat2
unique(item100.dat3$DHW.Location)
unique(item100.dat2$Primary.Heating.System)

item100.ind2 <- item100.dat1$CK_Cadmus_ID[grep("Yes|yes",item100.dat1$Primary.Heating.System)]
item100.dat3 <- item100.dat2[which(item100.dat2$CK_Cadmus_ID %in% item100.ind2),]

#clean heating fuel type
unique(item100.dat3$Heating.Fuel)
item100.dat3$Heating.Fuel[grep("Gas|gas",item100.dat3$Heating.Fuel)] <- "Gas"
item100.dat3$Heating.Fuel[grep("unk|Unk|N/A|Other|from",item100.dat3$Heating.Fuel)] <- NA
item100.dat3$Heating.Fuel[grep("oil|Oil|kero|Kero",item100.dat3$Heating.Fuel)] <- "Oil"
item100.dat3$Heating.Fuel[grep("Elect|elect",item100.dat3$Heating.Fuel)] <- "Electric"
item100.dat3$Heating.Fuel[which(item100.dat3$Heating.Fuel == "Wood (Pellets)")] <- "Pellets"
item100.dat3$Heating.Fuel[which(item100.dat3$Heating.Fuel == "Wood (Cord)")] <- "Wood"

item100.dat4 <- item100.dat3[which(!is.na(item100.dat3$Heating.Fuel)),]

unique(item100.dat4$Primary.Heating.System)

#summarise for primary heating system
item100.heat <- item100.dat4[which(item100.dat4$Primary.Heating.System == "Yes"),]
item100.sum1 <- summarise(group_by(item100.heat, CK_Cadmus_ID, BuildingType, Heating.Fuel)
                          ,Count = sum(count))

item100.sum1$Count <- 1
unique(item100.sum1$Heating.Fuel)

dup.ind <- item100.sum1$CK_Cadmus_ID[which(duplicated(item100.sum1$CK_Cadmus_ID))]
IDs.remove <- item100.sum1[which(item100.sum1$CK_Cadmus_ID %in% dup.ind & item100.sum1$Heating.Fuel == "Electric"),]

item100.sum2 <- item100.sum1[which(!(item100.sum1$CK_Cadmus_ID %in% IDs.remove$CK_Cadmus_ID & item100.sum1$Heating.Fuel %in% IDs.remove$Heating.Fuel)),]
item100.sum2$CK_Cadmus_ID[which(duplicated(item100.sum2$CK_Cadmus_ID))]

# ## correct duplicated fuel types
# item100.sum2$Heating.Fuel[which(item100.sum2$CK_Cadmus_ID == "BPS22084 OS BPA")] <- "Gas"
# item100.sum2$Heating.Fuel[which(item100.sum2$CK_Cadmus_ID == "MS0435")] <- "Propane"
# item100.sum2$Heating.Fuel[which(item100.sum2$CK_Cadmus_ID == "MS11008")] <- "Propane"
# item100.sum2$Heating.Fuel[which(item100.sum2$CK_Cadmus_ID == "MS3167")] <- "Propane"
# item100.sum2$Heating.Fuel[which(item100.sum2$CK_Cadmus_ID == "RBS60190 OS BPA")] <- "Pellets"
# item100.sum2$Heating.Fuel[which(item100.sum2$CK_Cadmus_ID == "RBS73757 OS BPA")] <- "Pellets"
# item100.sum2$Heating.Fuel[which(item100.sum2$CK_Cadmus_ID == "MS1998")] <- "Propane"

item100.heat.final <- unique(item100.sum2)
which(duplicated(item100.heat.final$CK_Cadmus_ID))
#qc
stopifnot(nrow(item100.heat.final) == length(unique(item100.heat.final$CK_Cadmus_ID)))

# summarise for water heaters
item100.WH <- item100.dat3[which(item100.dat3$Generic %in% c("Storage Water Heater"
                                                             ,"Instantaneous Water Heater")),]

item100.WH.sum1 <- summarise(group_by(item100.WH, CK_Cadmus_ID, BuildingType, DHW.Location, DHW.Fuel)
                             ,Count = sum(count))


item100.merge1 <- left_join(item100.heat.final, item100.WH.sum1, by = c("CK_Cadmus_ID", "BuildingType"))
item100.merge2 <- item100.merge1[which(!(is.na(item100.merge1$DHW.Location))),]
colnames(item100.merge2) <- c("CK_Cadmus_ID", "BuildingType", "Heating.Fuel","Heat.Count", "DHW.Location", "DHW.Fuel", "DHW.Count")

item100.join <- left_join(rbsa.dat, item100.merge2)
item100.join <- item100.join[which(!is.na(item100.join$Heating.Fuel)),]
item100.join1 <- item100.join[which(item100.join$DHW.Location != "Unknown"),]

item100.join1$DHW.Location[grep("Crawl",item100.join1$DHW.Location)] <- "Crawlspace"
item100.join1$DHW.Location[grep("In building",item100.join1$DHW.Location)] <- "Main House"
item100.join1$DHW.Location[which(item100.join1$DHW.Location %notin% c("Crawlspace"
                                                                      ,"Basement"
                                                                      ,"Garage"
                                                                      ,"Main House"))] <- "Other"
unique(item100.join1$DHW.Location)
item100.merge <- item100.join1[which(item100.join1$DHW.Fuel == "Electric"),]

################################################
# Adding pop and sample sizes for weights
################################################
item100.data <- weightedData(item100.merge[-which(colnames(item100.merge) %in% c("Heating.Fuel"               
                                                                                 ,"DHW.Count"
                                                                                 ,"DHW.Location"
                                                                                 ,"Heat.Count"
                                                                                 ,"DHW.Fuel"))])
item100.data <- left_join(item100.data, item100.merge[which(colnames(item100.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Heating.Fuel"               
                                                                                           ,"DHW.Count"
                                                                                           ,"DHW.Location"
                                                                                           ,"Heat.Count"
                                                                                           ,"DHW.Fuel"))])
#######################
# Weighted Analysis
#######################
item100.final <- proportionRowsAndColumns1(CustomerLevelData = item100.data
                                           ,valueVariable    = 'DHW.Count'
                                           ,columnVariable   = 'Heating.Fuel'
                                           ,rowVariable      = 'DHW.Location'
                                           ,aggregateColumnName = "Remove")
item100.final <- item100.final[which(item100.final$Heating.Fuel != "Remove"),]

item100.all.fuels <- proportions_one_group(CustomerLevelData = item100.data
                                           ,valueVariable = "DHW.Count"
                                           ,groupingVariable = "DHW.Location"
                                           ,total.name = "All Fuels"
                                           ,columnName = "Heating.Fuel"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)

item100.final <- rbind.data.frame(item100.final, item100.all.fuels)

item100.cast <- dcast(setDT(item100.final)
                      ,formula = BuildingType + DHW.Location ~ Heating.Fuel
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item100.table <- data.frame("BuildingType"        = item100.cast$BuildingType
                            ,"DHW.Location"        = item100.cast$DHW.Location
                            ,"Percent.Electric"    = item100.cast$w.percent_Electric
                            ,"SE.Electric"         = item100.cast$w.SE_Electric
                            ,"n.Electric"          = item100.cast$n_Electric
                            ,"Percent.Natural.Gas" = item100.cast$w.percent_Gas
                            ,"SE.Natural.Gas"      = item100.cast$w.SE_Gas
                            ,"n.Gas"               = item100.cast$n_Gas
                            ,"Percent.Oil"         = item100.cast$w.percent_Oil
                            ,"SE.Oil"              = item100.cast$w.SE_Oil
                            ,"n.Oil"               = item100.cast$n_Oil
                            ,"Percent.Pellets"     = item100.cast$w.percent_Pellets
                            ,"SE.Pellets"          = item100.cast$w.SE_Pellets
                            ,"n.Pellets"           = item100.cast$n_Pellets
                            ,"Percent.Propane"     = item100.cast$w.percent_Propane
                            ,"SE.Propane"          = item100.cast$w.SE_Propane
                            ,"n.Propane"           = item100.cast$n_Propane
                            ,"Percent.Wood"        = item100.cast$w.percent_Wood
                            ,"SE.Wood"             = item100.cast$w.SE_Wood
                            ,"n.Wood"              = item100.cast$n_Wood
                            ,"Percent.All.Heating.Fuel.Types" = item100.cast$`w.percent_All Fuels`
                            ,"SE.All.Heating.Fuel.Types"      = item100.cast$`w.SE_All Fuels`
                            ,"n.All.Heating.Fuel.Types"       = item100.cast$`n_All Fuels`
                            ,"EB_Electric"          = item100.cast$EB_Electric
                            ,"EB_Gas"               = item100.cast$EB_Gas
                            ,"EB_Oil"               = item100.cast$EB_Oil
                            ,"EB_Pellets"           = item100.cast$EB_Pellets
                            ,"EB_Propane"           = item100.cast$EB_Propane
                            ,"EB_Wood"              = item100.cast$EB_Wood
                            ,"EB_All.Heating.Fuels" = item100.cast$`EB_All Fuels`)

item100.final.SF <- item100.table[which(item100.table$BuildingType == "Single Family")
                                  ,-which(colnames(item100.table) %in% c("BuildingType"))]

exportTable(item100.final.SF, "SF", "Table 107", weighted = TRUE)



#######################
# Weighted Analysis
#######################
item100.final <- proportions_two_groups_unweighted(CustomerLevelData = item100.data
                                                   ,valueVariable    = 'DHW.Count'
                                                   ,columnVariable   = 'Heating.Fuel'
                                                   ,rowVariable      = 'DHW.Location'
                                                   ,aggregateColumnName = "Remove")
item100.final <- item100.final[which(item100.final$Heating.Fuel != "Remove"),]

item100.all.fuels <- proportions_one_group(CustomerLevelData = item100.data
                                           ,valueVariable = "DHW.Count"
                                           ,groupingVariable = "DHW.Location"
                                           ,total.name = "All Fuels"
                                           ,columnName = "Heating.Fuel"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)

item100.final <- rbind.data.frame(item100.final, item100.all.fuels)

item100.cast <- dcast(setDT(item100.final)
                      ,formula = BuildingType + DHW.Location ~ Heating.Fuel
                      ,value.var = c("Percent", "SE", "Count", "n"))

item100.table <- data.frame("BuildingType"         = item100.cast$BuildingType
                            ,"DHW.Location"        = item100.cast$DHW.Location
                            ,"Percent.Electric"    = item100.cast$Percent_Electric
                            ,"SE.Electric"         = item100.cast$SE_Electric
                            ,"n.Electric"          = item100.cast$n_Electric
                            ,"Percent.Natural.Gas" = item100.cast$Percent_Gas
                            ,"SE.Natural.Gas"      = item100.cast$SE_Gas
                            ,"n.Gas"               = item100.cast$n_Gas
                            ,"Percent.Oil"         = item100.cast$Percent_Oil
                            ,"SE.Oil"              = item100.cast$SE_Oil
                            ,"n.Oil"               = item100.cast$n_Oil
                            ,"Percent.Pellets"     = item100.cast$Percent_Pellets
                            ,"SE.Pellets"          = item100.cast$SE_Pellets
                            ,"n.Pellets"           = item100.cast$n_Pellets
                            ,"Percent.Propane"     = item100.cast$Percent_Propane
                            ,"SE.Propane"          = item100.cast$SE_Propane
                            ,"n.Propane"           = item100.cast$n_Propane
                            ,"Percent.Wood"        = item100.cast$Percent_Wood
                            ,"SE.Wood"             = item100.cast$SE_Wood
                            ,"n.Wood"              = item100.cast$n_Wood
                            ,"Percent.All.Heating.Fuel.Types" = item100.cast$`Percent_All Fuels`
                            ,"SE.All.Heating.Fuel.Types"      = item100.cast$`SE_All Fuels`
                            ,"n_All.Heating.Fuel.Types"       = item100.cast$`n_All Fuels`)

item100.final.SF <- item100.table[which(item100.table$BuildingType == "Single Family")
                                  ,-which(colnames(item100.table) %in% c("BuildingType"))]

exportTable(item100.final.SF, "SF", "Table 107", weighted = FALSE)




#############################################################################################
#Item 101: DISTRIBUTION OF GAS WATER HEATER LOCATIONS BY SPACE HEATING FUEL TYPE (SF table 108)
#############################################################################################
item101.merge <- item100.join1[which(item100.join1$DHW.Fuel == "Natural Gas"),]
item101.merge <- item101.merge[which(item101.merge$Heating.Fuel != "Can't Determine"),]
################################################
# Adding pop and sample sizes for weights
################################################
item101.data <- weightedData(item101.merge[-which(colnames(item101.merge) %in% c("Heating.Fuel"               
                                                                                 ,"DHW.Count"
                                                                                 ,"DHW.Location"
                                                                                 ,"Heat.Count"
                                                                                 ,"DHW.Fuel"))])
item101.data <- left_join(item101.data, item101.merge[which(colnames(item101.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Heating.Fuel"               
                                                                                           ,"DHW.Count"
                                                                                           ,"DHW.Location"
                                                                                           ,"Heat.Count"
                                                                                           ,"DHW.Fuel"))])
#######################
# Weighted Analysis
#######################
item101.final <- proportionRowsAndColumns1(CustomerLevelData = item101.data
                                           ,valueVariable    = 'DHW.Count'
                                           ,columnVariable   = 'Heating.Fuel'
                                           ,rowVariable      = 'DHW.Location'
                                           ,aggregateColumnName = "Remove")
item101.final <- item101.final[which(item101.final$Heating.Fuel != "Remove"),]

item101.all.fuels <- proportions_one_group(CustomerLevelData = item101.data
                                           ,valueVariable = "DHW.Count"
                                           ,groupingVariable = "DHW.Location"
                                           ,total.name = "All Fuels"
                                           ,columnName = "Heating.Fuel"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)

item101.final <- rbind.data.frame(item101.final, item101.all.fuels)

item101.cast <- dcast(setDT(item101.final)
                      ,formula = BuildingType + DHW.Location ~ Heating.Fuel
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
names(item101.cast)
item101.table <- data.frame("BuildingType"         = item101.cast$BuildingType
                            ,"DHW.Location"        = item101.cast$DHW.Location
                            ,"Percent.Electric"    = item101.cast$w.percent_Electric
                            ,"SE.Electric"         = item101.cast$w.SE_Electric
                            ,"n.Electric"          = item101.cast$n_Electric
                            ,"Percent.Natural.Gas" = item101.cast$w.percent_Gas
                            ,"SE.Natural.Gas"      = item101.cast$w.SE_Gas
                            ,"n.Gas"               = item101.cast$n_Gas
                            # ,"Percent.Oil"         = item101.cast$w.percent_Oil
                            # ,"SE.Oil"              = item101.cast$w.SE_Oil
                            # ,"n.Oil"               = item101.cast$n_Oil
                            ,"Percent.Pellets"     = item101.cast$w.percent_Pellets
                            ,"SE.Pellets"          = item101.cast$w.SE_Pellets
                            ,"n.Pellets"           = item101.cast$n_Pellets
                            # ,"Percent.Propane"     = item101.cast$w.percent_Propane
                            # ,"SE.Propane"          = item101.cast$w.SE_Propane
                            # ,"n.Propane"           = item101.cast$n_Propane
                            ,"Percent.Wood"        = item101.cast$w.percent_Wood
                            ,"SE.Wood"             = item101.cast$w.SE_Wood
                            ,"n.Wood"              = item101.cast$n_Wood
                            ,"Percent.All.Heating.Fuel.Types" = item101.cast$`w.percent_All Fuels`
                            ,"SE.All.Heating.Fuel.Types"      = item101.cast$`w.SE_All Fuels`
                            ,"n.All.Heating.Fuel.Types"       = item101.cast$`n_All Fuels`
                            ,"EB_Electric"          = item101.cast$EB_Electric
                            ,"EB_Gas"               = item101.cast$EB_Gas
                            # ,"EB_Oil"               = item101.cast$EB_Oil
                            ,"EB_Pellets"           = item101.cast$EB_Pellets
                            # ,"EB_Propane"           = item101.cast$EB_Propane
                            ,"EB_Wood"              = item101.cast$EB_Wood
                            ,"EB_All.Heating.Fuels" = item101.cast$`EB_All Fuels`)

item101.final.SF <- item101.table[which(item101.table$BuildingType == "Single Family")
                                  ,-which(colnames(item101.table) %in% c("BuildingType"))]

exportTable(item101.final.SF, "SF", "Table 108", weighted = TRUE)



#######################
# Weighted Analysis
#######################
item101.final <- proportions_two_groups_unweighted(CustomerLevelData = item101.data
                                                   ,valueVariable    = 'DHW.Count'
                                                   ,columnVariable   = 'Heating.Fuel'
                                                   ,rowVariable      = 'DHW.Location'
                                                   ,aggregateColumnName = "Remove")
item101.final <- item101.final[which(item101.final$Heating.Fuel != "Remove"),]

item101.all.fuels <- proportions_one_group(CustomerLevelData = item101.data
                                           ,valueVariable = "DHW.Count"
                                           ,groupingVariable = "DHW.Location"
                                           ,total.name = "All Fuels"
                                           ,columnName = "Heating.Fuel"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)

item101.final <- rbind.data.frame(item101.final, item101.all.fuels)

item101.cast <- dcast(setDT(item101.final)
                      ,formula = BuildingType + DHW.Location ~ Heating.Fuel
                      ,value.var = c("Percent", "SE", "Count", "n"))

item101.table <- data.frame("BuildingType"         = item101.cast$BuildingType
                            ,"DHW.Location"        = item101.cast$DHW.Location
                            ,"Percent.Electric"    = item101.cast$Percent_Electric
                            ,"SE.Electric"         = item101.cast$SE_Electric
                            ,"n.Electric"          = item101.cast$n_Electric
                            ,"Percent.Natural.Gas" = item101.cast$Percent_Gas
                            ,"SE.Natural.Gas"      = item101.cast$SE_Gas
                            ,"n.Gas"               = item101.cast$n_Gas
                            # ,"Percent.Oil"         = item101.cast$Percent_Oil
                            # ,"SE.Oil"              = item101.cast$SE_Oil
                            # ,"n.Oil"               = item101.cast$n_Oil
                            ,"Percent.Pellets"     = item101.cast$Percent_Pellets
                            ,"SE.Pellets"          = item101.cast$SE_Pellets
                            ,"n.Pellets"           = item101.cast$n_Pellets
                            # ,"Percent.Propane"     = item101.cast$Percent_Propane
                            # ,"SE.Propane"          = item101.cast$SE_Propane
                            # ,"n.Propane"           = item101.cast$n_Propane
                            ,"Percent.Wood"        = item101.cast$Percent_Wood
                            ,"SE.Wood"             = item101.cast$SE_Wood
                            ,"n.Wood"              = item101.cast$n_Wood
                            ,"Percent.All.Heating.Fuel.Types" = item101.cast$`Percent_All Fuels`
                            ,"SE.All.Heating.Fuel.Types"      = item101.cast$`SE_All Fuels`
                            ,"n_All.Heating.Fuel.Types"       = item101.cast$`n_All Fuels`)

item101.final.SF <- item101.table[which(item101.table$BuildingType == "Single Family")
                                  ,-which(colnames(item101.table) %in% c("BuildingType"))]

exportTable(item101.final.SF, "SF", "Table 108", weighted = FALSE)




























# ############################################################################################################
# #
# #
# # OVERSAMPLE ANALYSIS
# #
# #
# ############################################################################################################
# 
# # Read in clean os data
# os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
# length(unique(os.dat$CK_Cadmus_ID))
# os.dat$CK_Building_ID <- os.dat$Category
# os.dat <- os.dat[which(names(os.dat) != "Category")]
# 
# #############################################################################################
# #Item 99: DISTRIBUTION OF ALL WATER HEATER LOCATIONS BY SPACE HEATING FUEL TYPE (SF table 106, MH table 86)
# #############################################################################################
# #subset to columns needed for analysis
# item99.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
#                                                                    ,"Generic"
#                                                                    ,"DHW.Fuel"
#                                                                    ,"DHW.Location"
#                                                                    ,"Primary.Heating.System"
#                                                                    ,"Heating.Fuel"))]
# item99.os.dat$count <- 1
# 
# item99.os.dat0 <- item99.os.dat[which(item99.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item99.os.dat1 <- left_join(item99.os.dat0, os.dat, by = "CK_Cadmus_ID")
# 
# item99.os.ind <- item99.os.dat1$CK_Cadmus_ID[grep("Water Heater",item99.os.dat1$Generic)]
# item99.os.dat2 <- item99.os.dat1[which(item99.os.dat1$CK_Cadmus_ID %in% item99.os.ind),]
# 
# item99.os.dat3 <- item99.os.dat2
# unique(item99.os.dat3$DHW.Location)
# unique(item99.os.dat2$Primary.Heating.System)
# 
# item99.os.ind2 <- item99.os.dat1$CK_Cadmus_ID[grep("Yes",item99.os.dat1$Primary.Heating.System, ignore.case = T)]
# item99.os.dat3 <- item99.os.dat2[which(item99.os.dat2$CK_Cadmus_ID %in% item99.os.ind2),]
# 
# #clean heating fuel type
# unique(item99.os.dat3$Heating.Fuel)
# item99.os.dat3$Heating.Fuel[grep("Gas",item99.os.dat3$Heating.Fuel, ignore.case = T)]                      <- "Gas"
# item99.os.dat3$Heating.Fuel[grep("unk|N/A|Other|from|can't",item99.os.dat3$Heating.Fuel, ignore.case = T)] <- NA
# item99.os.dat3$Heating.Fuel[grep("oil|kero",item99.os.dat3$Heating.Fuel, ignore.case = T)]                 <- "Oil"
# item99.os.dat3$Heating.Fuel[grep("Elect",item99.os.dat3$Heating.Fuel, ignore.case = T)]                    <- "Electric"
# item99.os.dat3$Heating.Fuel[which(item99.os.dat3$Heating.Fuel %in% c("Wood (pellets)","Wood (Pellets)"))]  <- "Pellets"
# item99.os.dat3$Heating.Fuel[which(item99.os.dat3$Heating.Fuel %in% c("Wood (cord)","Wood (Cord)"))]        <- "Wood"
# 
# item99.os.dat4 <- item99.os.dat3[which(!is.na(item99.os.dat3$Heating.Fuel)),]
# 
# unique(item99.os.dat4$Primary.Heating.System)
# 
# #summarise for primary heating system
# item99.os.heat <- item99.os.dat4[which(item99.os.dat4$Primary.Heating.System == "Yes"),]
# 
# item99.os.sum1 <- summarise(group_by(item99.os.heat, CK_Cadmus_ID, CK_Building_ID, Heating.Fuel)
#                          ,Count = sum(count))
# 
# item99.os.sum1$Count <- 1
# unique(item99.os.sum1$Heating.Fuel)
# item99.os.unique <- unique(item99.os.sum1)
# 
# item99.os.heat.final <- item99.os.unique
# 
# # summarise for water heaters
# item99.os.WH <- item99.os.dat3[which(item99.os.dat3$Generic %in% c("Storage Water Heater"
#                                                           ,"Instantaneous Water Heater")),]
# 
# item99.os.WH.sum1 <- summarise(group_by(item99.os.WH, CK_Cadmus_ID, CK_Building_ID, DHW.Location)
#                             ,Count = sum(count))
# 
# 
# item99.os.merge1 <- left_join(item99.os.heat.final, item99.os.WH.sum1, by = c("CK_Cadmus_ID", "CK_Building_ID"))
# item99.os.merge2 <- item99.os.merge1[which(!(is.na(item99.os.merge1$Heating.Fuel))),]
# colnames(item99.os.merge2) <- c("CK_Cadmus_ID", "CK_Building_ID", "Heating.Fuel","Heat.Count", "DHW.Location", "DHW.Count")
# 
# 
# item99.os.join <- left_join(os.dat, item99.os.merge2)
# item99.os.join <- item99.os.join[which(!is.na(item99.os.join$Heating.Fuel)),]
# item99.os.join1 <- item99.os.join[which(item99.os.join$DHW.Location != "Unknown"),]
# 
# item99.os.join1$DHW.Location[grep("crawl",item99.os.join1$DHW.Location, ignore.case = T)] <- "Crawlspace"
# 
# item99.os.join1$DHW.Location[which(item99.os.join1$DHW.Location %notin% c("Crawlspace"
#                                                                     ,"Basement"
#                                                                     ,"Garage"
#                                                                     ,"Main House"))] <- "Other"
# 
# 
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item99.os.data <- weightedData(item99.os.join1[-which(colnames(item99.os.join1) %in% c("Heating.Fuel"               
#                                                                               ,"DHW.Count"
#                                                                               ,"DHW.Location"
#                                                                               ,"Heat.Count"))])
# item99.os.data <- left_join(item99.os.data, unique(item99.os.join1[which(colnames(item99.os.join1) %in% c("CK_Cadmus_ID"
#                                                                                        ,"Heating.Fuel"               
#                                                                                        ,"DHW.Count"
#                                                                                        ,"DHW.Location"
#                                                                                        ,"Heat.Count"))]))
# item99.os.data <- item99.os.data[which(item99.os.data$CK_Building_ID == subset.ind),]
# #######################
# # Weighted Analysis
# #######################
# item99.os.summary <- proportionRowsAndColumns1(CustomerLevelData = item99.os.data
#                                             ,valueVariable    = 'DHW.Count'
#                                             ,columnVariable   = 'Heating.Fuel'
#                                             ,rowVariable      = 'DHW.Location'
#                                             ,aggregateColumnName = "Remove")
# item99.os.summary <- item99.os.summary[which(item99.os.summary$Heating.Fuel %notin% c("Remove")),]
# 
# item99.os.all.fuels <- proportions_one_group(CustomerLevelData = item99.os.data
#                                           ,valueVariable = "DHW.Count"
#                                           ,groupingVariable = "DHW.Location"
#                                           ,total.name = "All Fuels"
#                                           ,columnName = "Heating.Fuel"
#                                           ,weighted = TRUE
#                                           ,two.prop.total = TRUE)
# 
# item99.os.final <- rbind.data.frame(item99.os.summary, item99.os.all.fuels, stringsAsFactors = F)
# 
# item99.os.cast <- dcast(setDT(item99.os.final)
#                      ,formula = BuildingType + DHW.Location ~ Heating.Fuel
#                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
# names(item99.os.cast)
# 
# if(os.ind == "scl"){
#   item99.os.table <- data.frame("BuildingType"         = item99.os.cast$BuildingType
#                                 ,"DHW.Location"        = item99.os.cast$DHW.Location
#                                 ,"Percent.Electric"    = item99.os.cast$w.percent_Electric
#                                 ,"SE.Electric"         = item99.os.cast$w.SE_Electric
#                                 ,"n.Electric"          = item99.os.cast$n_Electric
#                                 ,"Percent.Natural.Gas" = item99.os.cast$w.percent_Gas
#                                 ,"SE.Natural.Gas"      = item99.os.cast$w.SE_Gas
#                                 ,"n.Gas"               = item99.os.cast$n_Gas
#                                 ,"Percent.Oil"         = item99.os.cast$w.percent_Oil
#                                 ,"SE.Oil"              = item99.os.cast$w.SE_Oil
#                                 ,"n.Oil"               = item99.os.cast$n_Oil
#                                 # ,"Percent.Pellets"     = item99.os.cast$w.percent_Pellets
#                                 # ,"SE.Pellets"          = item99.os.cast$w.SE_Pellets
#                                 # ,"n.Pellets"           = item99.os.cast$n_Pellets
#                                 # ,"Percent.Propane"     = item99.os.cast$w.percent_Propane
#                                 # ,"SE.Propane"          = item99.os.cast$w.SE_Propane
#                                 # ,"n.Propane"           = item99.os.cast$n_Propane
#                                 ,"Percent.Wood"        = item99.os.cast$w.percent_Wood
#                                 ,"SE.Wood"             = item99.os.cast$w.SE_Wood
#                                 ,"n.Wood"              = item99.os.cast$n_Wood
#                                 ,"Percent.All.Heating.Fuel.Types" = item99.os.cast$`w.percent_All Fuels`
#                                 ,"SE.All.Heating.Fuel.Types"      = item99.os.cast$`w.SE_All Fuels`
#                                 ,"n.All.Heating.Fuel.Types"       = item99.os.cast$`n_All Fuels`
#                                 ,"EB_Electric"          = item99.os.cast$EB_Electric
#                                 ,"EB_Gas"               = item99.os.cast$EB_Gas
#                                 ,"EB_Oil"               = item99.os.cast$EB_Oil
#                                 # ,"EB_Pellets"           = item99.os.cast$EB_Pellets
#                                 # ,"EB_Propane"           = item99.os.cast$EB_Propane
#                                 ,"EB_Wood"              = item99.os.cast$EB_Wood
#                                 ,"EB_All.Heating.Fuels" = item99.os.cast$`EB_All Fuels`)
#   
# }else if(os.ind == "snopud"){
#   item99.os.table <- data.frame("BuildingType"         = item99.os.cast$BuildingType
#                                 ,"DHW.Location"        = item99.os.cast$DHW.Location
#                                 ,"Percent.Electric"    = item99.os.cast$w.percent_Electric
#                                 ,"SE.Electric"         = item99.os.cast$w.SE_Electric
#                                 ,"n.Electric"          = item99.os.cast$n_Electric
#                                 ,"Percent.Natural.Gas" = item99.os.cast$w.percent_Gas
#                                 ,"SE.Natural.Gas"      = item99.os.cast$w.SE_Gas
#                                 ,"n.Gas"               = item99.os.cast$n_Gas
#                                 ,"Percent.Oil"         = item99.os.cast$w.percent_Oil
#                                 ,"SE.Oil"              = item99.os.cast$w.SE_Oil
#                                 ,"n.Oil"               = item99.os.cast$n_Oil
#                                 # ,"Percent.Pellets"     = item99.os.cast$w.percent_Pellets
#                                 # ,"SE.Pellets"          = item99.os.cast$w.SE_Pellets
#                                 # ,"n.Pellets"           = item99.os.cast$n_Pellets
#                                 ,"Percent.Propane"     = item99.os.cast$w.percent_Propane
#                                 ,"SE.Propane"          = item99.os.cast$w.SE_Propane
#                                 ,"n.Propane"           = item99.os.cast$n_Propane
#                                 ,"Percent.Wood"        = item99.os.cast$w.percent_Wood
#                                 ,"SE.Wood"             = item99.os.cast$w.SE_Wood
#                                 ,"n.Wood"              = item99.os.cast$n_Wood
#                                 ,"Percent.All.Heating.Fuel.Types" = item99.os.cast$`w.percent_All Fuels`
#                                 ,"SE.All.Heating.Fuel.Types"      = item99.os.cast$`w.SE_All Fuels`
#                                 ,"n.All.Heating.Fuel.Types"       = item99.os.cast$`n_All Fuels`
#                                 ,"EB_Electric"          = item99.os.cast$EB_Electric
#                                 ,"EB_Gas"               = item99.os.cast$EB_Gas
#                                 ,"EB_Oil"               = item99.os.cast$EB_Oil
#                                 # ,"EB_Pellets"           = item99.os.cast$EB_Pellets
#                                 ,"EB_Propane"           = item99.os.cast$EB_Propane
#                                 ,"EB_Wood"              = item99.os.cast$EB_Wood
#                                 ,"EB_All.Heating.Fuels" = item99.os.cast$`EB_All Fuels`)
#   
# }
# 
# levels(item99.os.table$DHW.Location)
# rowOrder <- c("Basement"
#               ,"Crawlspace"
#               ,"Garage"
#               ,"Other"
#               ,"Main House"
#               ,"Total")
# item99.os.table <- item99.os.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
# item99.os.table <- data.frame(item99.os.table)
# 
# item99.os.final.SF <- item99.os.table[which(item99.os.table$BuildingType == "Single Family")
#                                 ,-which(colnames(item99.os.table) %in% c("BuildingType"))]
# 
# exportTable(item99.os.final.SF, "SF", "Table 106", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# #######################
# # unweighted Analysis
# #######################
# item99.os.summary <- proportions_two_groups_unweighted(CustomerLevelData = item99.os.data
#                                                ,valueVariable    = 'DHW.Count'
#                                                ,columnVariable   = 'Heating.Fuel'
#                                                ,rowVariable      = 'DHW.Location'
#                                                ,aggregateColumnName = "Remove")
# item99.os.summary <- item99.os.summary[which(item99.os.summary$Heating.Fuel %notin% c("Remove")),]
# 
# item99.os.all.fuels <- proportions_one_group(CustomerLevelData = item99.os.data
#                                              ,valueVariable = "DHW.Count"
#                                              ,groupingVariable = "DHW.Location"
#                                              ,total.name = "All Fuels"
#                                              ,columnName = "Heating.Fuel"
#                                              ,weighted = FALSE
#                                              ,two.prop.total = TRUE)
# 
# item99.os.final <- rbind.data.frame(item99.os.summary, item99.os.all.fuels, stringsAsFactors = F)
# 
# item99.os.cast <- dcast(setDT(item99.os.final)
#                         ,formula = BuildingType + DHW.Location ~ Heating.Fuel
#                         ,value.var = c("Percent", "SE", "n"))
# names(item99.os.cast)
# 
# if(os.ind == "scl"){
#   item99.os.table <- data.frame("BuildingType"         = item99.os.cast$BuildingType
#                                 ,"DHW.Location"        = item99.os.cast$DHW.Location
#                                 ,"Percent.Electric"    = item99.os.cast$Percent_Electric
#                                 ,"SE.Electric"         = item99.os.cast$SE_Electric
#                                 ,"n.Electric"          = item99.os.cast$n_Electric
#                                 ,"Percent.Natural.Gas" = item99.os.cast$Percent_Gas
#                                 ,"SE.Natural.Gas"      = item99.os.cast$SE_Gas
#                                 ,"n.Gas"               = item99.os.cast$n_Gas
#                                 ,"Percent.Oil"         = item99.os.cast$Percent_Oil
#                                 ,"SE.Oil"              = item99.os.cast$SE_Oil
#                                 ,"n.Oil"               = item99.os.cast$n_Oil
#                                 # ,"Percent.Pellets"     = item99.os.cast$Percent_Pellets
#                                 # ,"SE.Pellets"          = item99.os.cast$SE_Pellets
#                                 # ,"n.Pellets"           = item99.os.cast$n_Pellets
#                                 # ,"Percent.Propane"     = item99.os.cast$Percent_Propane
#                                 # ,"SE.Propane"          = item99.os.cast$SE_Propane
#                                 # ,"n.Propane"           = item99.os.cast$n_Propane
#                                 ,"Percent.Wood"        = item99.os.cast$Percent_Wood
#                                 ,"SE.Wood"             = item99.os.cast$SE_Wood
#                                 ,"n.Wood"              = item99.os.cast$n_Wood
#                                 ,"Percent.All.Heating.Fuel.Types" = item99.os.cast$`Percent_All Fuels`
#                                 ,"SE.All.Heating.Fuel.Types"      = item99.os.cast$`SE_All Fuels`
#                                 ,"n_All.Heating.Fuel.Types"       = item99.os.cast$`n_All Fuels`)
#   
# }else if(os.ind == "snopud"){
#   item99.os.table <- data.frame("BuildingType"         = item99.os.cast$BuildingType
#                                 ,"DHW.Location"        = item99.os.cast$DHW.Location
#                                 ,"Percent.Electric"    = item99.os.cast$Percent_Electric
#                                 ,"SE.Electric"         = item99.os.cast$SE_Electric
#                                 ,"n.Electric"          = item99.os.cast$n_Electric
#                                 ,"Percent.Natural.Gas" = item99.os.cast$Percent_Gas
#                                 ,"SE.Natural.Gas"      = item99.os.cast$SE_Gas
#                                 ,"n.Gas"               = item99.os.cast$n_Gas
#                                 ,"Percent.Oil"         = item99.os.cast$Percent_Oil
#                                 ,"SE.Oil"              = item99.os.cast$SE_Oil
#                                 ,"n.Oil"               = item99.os.cast$n_Oil
#                                 # ,"Percent.Pellets"     = item99.os.cast$Percent_Pellets
#                                 # ,"SE.Pellets"          = item99.os.cast$SE_Pellets
#                                 # ,"n.Pellets"           = item99.os.cast$n_Pellets
#                                 ,"Percent.Propane"     = item99.os.cast$Percent_Propane
#                                 ,"SE.Propane"          = item99.os.cast$SE_Propane
#                                 ,"n.Propane"           = item99.os.cast$n_Propane
#                                 ,"Percent.Wood"        = item99.os.cast$Percent_Wood
#                                 ,"SE.Wood"             = item99.os.cast$SE_Wood
#                                 ,"n.Wood"              = item99.os.cast$n_Wood
#                                 ,"Percent.All.Heating.Fuel.Types" = item99.os.cast$`Percent_All Fuels`
#                                 ,"SE.All.Heating.Fuel.Types"      = item99.os.cast$`SE_All Fuels`
#                                 ,"n_All.Heating.Fuel.Types"       = item99.os.cast$`n_All Fuels`)
#   
# }
# 
# levels(item99.os.table$DHW.Location)
# rowOrder <- c("Basement"
#               ,"Crawlspace"
#               ,"Garage"
#               ,"Other"
#               ,"Main House"
#               ,"All Locations")
# item99.os.table <- item99.os.table %>% mutate(DHW.Location = factor(DHW.Location, levels = rowOrder)) %>% arrange(DHW.Location)  
# item99.os.table <- data.frame(item99.os.table)
# 
# 
# item99.os.final.SF <- item99.os.table[which(item99.os.table$BuildingType == "Single Family")
#                                 ,-which(colnames(item99.os.table) %in% c("BuildingType"))]
# 
# exportTable(item99.os.final.SF, "SF", "Table 106", weighted = FALSE, osIndicator = export.ind, OS = T)
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 100: DISTRIBUTION OF ELECTRIC WATER HEATER LOCATIONS BY SPACE HEATING FUEL TYPE (SF table 107)
# #############################################################################################
# #subset to columns needed for analysis
# item100.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
#                                                                     ,"Generic"
#                                                                     ,"DHW.Fuel"
#                                                                     ,"DHW.Location"
#                                                                     ,"Primary.Heating.System"
#                                                                     ,"Heating.Fuel"))]
# item100.os.dat$count <- 1
# 
# item100.os.dat0 <- item100.os.dat[which(item100.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item100.os.dat1 <- left_join(item100.os.dat0, os.dat, by = "CK_Cadmus_ID")
# 
# item100.os.ind <- item100.os.dat1$CK_Cadmus_ID[grep("Water Heater",item100.os.dat1$Generic)]
# item100.os.dat2 <- item100.os.dat1[which(item100.os.dat1$CK_Cadmus_ID %in% item100.os.ind),]
# 
# item100.os.dat3 <- item100.os.dat2
# unique(item100.os.dat3$DHW.Location)
# unique(item100.os.dat2$Primary.Heating.System)
# 
# item100.os.ind2 <- item100.os.dat1$CK_Cadmus_ID[grep("Yes|yes",item100.os.dat1$Primary.Heating.System)]
# item100.os.dat3 <- item100.os.dat2[which(item100.os.dat2$CK_Cadmus_ID %in% item100.os.ind2),]
# 
# #clean heating fuel type
# unique(item100.os.dat3$Heating.Fuel)
# item100.os.dat3$Heating.Fuel[grep("Gas|gas",item100.os.dat3$Heating.Fuel)] <- "Gas"
# item100.os.dat3$Heating.Fuel[grep("unk|Unk|N/A|Other|from",item100.os.dat3$Heating.Fuel)] <- NA
# item100.os.dat3$Heating.Fuel[grep("oil|Oil|kero|Kero",item100.os.dat3$Heating.Fuel)] <- "Oil"
# item100.os.dat3$Heating.Fuel[grep("Elect|elect",item100.os.dat3$Heating.Fuel)] <- "Electric"
# item100.os.dat3$Heating.Fuel[which(item100.os.dat3$Heating.Fuel %in% c("Wood (pellets)","Wood (Pellets)"))]  <- "Pellets"
# item100.os.dat3$Heating.Fuel[which(item100.os.dat3$Heating.Fuel %in% c("Wood (cord)","Wood (Cord)"))]        <- "Wood"
# 
# item100.os.dat4 <- item100.os.dat3[which(!is.na(item100.os.dat3$Heating.Fuel)),]
# 
# unique(item100.os.dat4$Primary.Heating.System)
# 
# #summarise for primary heating system
# item100.os.heat <- item100.os.dat4[which(item100.os.dat4$Primary.Heating.System == "Yes"),]
# item100.os.sum1 <- summarise(group_by(item100.os.heat, CK_Cadmus_ID, CK_Building_ID, Heating.Fuel)
#                           ,Count = sum(count))
# 
# item100.os.sum1$Count <- 1
# unique(item100.os.sum1$Heating.Fuel)
# 
# item100.os.heat.final <- unique(item100.os.sum1)
# 
# # summarise for water heaters
# item100.os.WH <- item100.os.dat3[which(item100.os.dat3$Generic %in% c("Storage Water Heater"
#                                                              ,"Instantaneous Water Heater")),]
# 
# item100.os.WH.sum1 <- summarise(group_by(item100.os.WH, CK_Cadmus_ID, CK_Building_ID, DHW.Location, DHW.Fuel)
#                              ,Count = sum(count))
# 
# 
# item100.os.merge1 <- left_join(item100.os.heat.final, item100.os.WH.sum1, by = c("CK_Cadmus_ID", "CK_Building_ID"))
# item100.os.merge2 <- item100.os.merge1[which(!(is.na(item100.os.merge1$DHW.Location))),]
# colnames(item100.os.merge2) <- c("CK_Cadmus_ID", "CK_Building_ID", "Heating.Fuel","Heat.Count", "DHW.Location", "DHW.Fuel", "DHW.Count")
# 
# item100.os.join <- left_join(os.dat, item100.os.merge2)
# item100.os.join <- item100.os.join[which(!is.na(item100.os.join$Heating.Fuel)),]
# item100.os.join1 <- item100.os.join[which(item100.os.join$DHW.Location != "Unknown"),]
# 
# item100.os.join1$DHW.Location[grep("crawl",item100.os.join1$DHW.Location, ignore.case = T)] <- "Crawlspace"
# 
# item100.os.join1$DHW.Location[which(item100.os.join1$DHW.Location %notin% c("Crawlspace"
#                                                                       ,"Basement"
#                                                                       ,"Garage"
#                                                                       ,"Main House"))] <- "Other"
# unique(item100.os.join1$DHW.Location)
# item100.os.merge <- item100.os.join1[which(item100.os.join1$DHW.Fuel == "Electric"),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item100.os.data <- weightedData(item100.os.merge[-which(colnames(item100.os.merge) %in% c("Heating.Fuel"               
#                                                                                  ,"DHW.Count"
#                                                                                  ,"DHW.Location"
#                                                                                  ,"Heat.Count"
#                                                                                  ,"DHW.Fuel"))])
# item100.os.data <- left_join(item100.os.data, unique(item100.os.merge[which(colnames(item100.os.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Heating.Fuel"               
#                                                                                            ,"DHW.Count"
#                                                                                            ,"DHW.Location"
#                                                                                            ,"Heat.Count"
#                                                                                            ,"DHW.Fuel"))]))
# item100.os.data <- item100.os.data[which(item100.os.data$CK_Building_ID == subset.ind),]
# #######################
# # Weighted Analysis
# #######################
# item100.os.final <- proportionRowsAndColumns1(CustomerLevelData = item100.os.data
#                                            ,valueVariable    = 'DHW.Count'
#                                            ,columnVariable   = 'Heating.Fuel'
#                                            ,rowVariable      = 'DHW.Location'
#                                            ,aggregateColumnName = "Remove")
# item100.os.final <- item100.os.final[which(item100.os.final$Heating.Fuel != "Remove"),]
# 
# item100.os.all.fuels <- proportions_one_group(CustomerLevelData = item100.os.data
#                                            ,valueVariable = "DHW.Count"
#                                            ,groupingVariable = "DHW.Location"
#                                            ,total.name = "All Fuels"
#                                            ,columnName = "Heating.Fuel"
#                                            ,weighted = TRUE
#                                            ,two.prop.total = TRUE)
# 
# item100.os.final <- rbind.data.frame(item100.os.final, item100.os.all.fuels)
# 
# item100.os.cast <- dcast(setDT(item100.os.final)
#                       ,formula = BuildingType + DHW.Location ~ Heating.Fuel
#                       ,value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
# names(item100.os.cast)
# if(os.ind == "scl"){
#   item100.os.table <- data.frame("BuildingType"         = item100.os.cast$BuildingType
#                                 ,"DHW.Location"        = item100.os.cast$DHW.Location
#                                 ,"Percent.Electric"    = item100.os.cast$w.percent_Electric
#                                 ,"SE.Electric"         = item100.os.cast$w.SE_Electric
#                                 ,"n.Electric"          = item100.os.cast$n_Electric
#                                 ,"Percent.Natural.Gas" = item100.os.cast$w.percent_Gas
#                                 ,"SE.Natural.Gas"      = item100.os.cast$w.SE_Gas
#                                 ,"n.Gas"               = item100.os.cast$n_Gas
#                                 ,"Percent.Oil"         = item100.os.cast$w.percent_Oil
#                                 ,"SE.Oil"              = item100.os.cast$w.SE_Oil
#                                 ,"n.Oil"               = item100.os.cast$n_Oil
#                                 # ,"Percent.Pellets"     = item100.os.cast$w.percent_Pellets
#                                 # ,"SE.Pellets"          = item100.os.cast$w.SE_Pellets
#                                 # ,"n.Pellets"           = item100.os.cast$n_Pellets
#                                 # ,"Percent.Propane"     = item100.os.cast$w.percent_Propane
#                                 # ,"SE.Propane"          = item100.os.cast$w.SE_Propane
#                                 # ,"n.Propane"           = item100.os.cast$n_Propane
#                                 ,"Percent.Wood"        = item100.os.cast$w.percent_Wood
#                                 ,"SE.Wood"             = item100.os.cast$w.SE_Wood
#                                 ,"n.Wood"              = item100.os.cast$n_Wood
#                                 ,"Percent.All.Heating.Fuel.Types" = item100.os.cast$`w.percent_All Fuels`
#                                 ,"SE.All.Heating.Fuel.Types"      = item100.os.cast$`w.SE_All Fuels`
#                                 ,"n.All.Heating.Fuel.Types"       = item100.os.cast$`n_All Fuels`
#                                 ,"EB_Electric"          = item100.os.cast$EB_Electric
#                                 ,"EB_Gas"               = item100.os.cast$EB_Gas
#                                 ,"EB_Oil"               = item100.os.cast$EB_Oil
#                                 # ,"EB_Pellets"           = item100.os.cast$EB_Pellets
#                                 # ,"EB_Propane"           = item100.os.cast$EB_Propane
#                                 ,"EB_Wood"              = item100.os.cast$EB_Wood
#                                 ,"EB_All.Heating.Fuels" = item100.os.cast$`EB_All Fuels`)
#   
# }else if(os.ind == "snopud"){
#   item100.os.table <- data.frame("BuildingType"         = item100.os.cast$BuildingType
#                                 ,"DHW.Location"        = item100.os.cast$DHW.Location
#                                 ,"Percent.Electric"    = item100.os.cast$w.percent_Electric
#                                 ,"SE.Electric"         = item100.os.cast$w.SE_Electric
#                                 ,"n.Electric"          = item100.os.cast$n_Electric
#                                 ,"Percent.Natural.Gas" = item100.os.cast$w.percent_Gas
#                                 ,"SE.Natural.Gas"      = item100.os.cast$w.SE_Gas
#                                 ,"n.Gas"               = item100.os.cast$n_Gas
#                                 ,"Percent.Oil"         = item100.os.cast$w.percent_Oil
#                                 ,"SE.Oil"              = item100.os.cast$w.SE_Oil
#                                 ,"n.Oil"               = item100.os.cast$n_Oil
#                                 # ,"Percent.Pellets"     = item100.os.cast$w.percent_Pellets
#                                 # ,"SE.Pellets"          = item100.os.cast$w.SE_Pellets
#                                 # ,"n.Pellets"           = item100.os.cast$n_Pellets
#                                 # ,"Percent.Propane"     = item100.os.cast$w.percent_Propane
#                                 # ,"SE.Propane"          = item100.os.cast$w.SE_Propane
#                                 # ,"n.Propane"           = item100.os.cast$n_Propane
#                                 ,"Percent.Wood"        = item100.os.cast$w.percent_Wood
#                                 ,"SE.Wood"             = item100.os.cast$w.SE_Wood
#                                 ,"n.Wood"              = item100.os.cast$n_Wood
#                                 ,"Percent.All.Heating.Fuel.Types" = item100.os.cast$`w.percent_All Fuels`
#                                 ,"SE.All.Heating.Fuel.Types"      = item100.os.cast$`w.SE_All Fuels`
#                                 ,"n.All.Heating.Fuel.Types"       = item100.os.cast$`n_All Fuels`
#                                 ,"EB_Electric"          = item100.os.cast$EB_Electric
#                                 ,"EB_Gas"               = item100.os.cast$EB_Gas
#                                 ,"EB_Oil"               = item100.os.cast$EB_Oil
#                                 # ,"EB_Pellets"           = item100.os.cast$EB_Pellets
#                                 # ,"EB_Propane"           = item100.os.cast$EB_Propane
#                                 ,"EB_Wood"              = item100.os.cast$EB_Wood
#                                 ,"EB_All.Heating.Fuels" = item100.os.cast$`EB_All Fuels`)
#   
# }
# 
# item100.os.final.SF <- item100.os.table[which(item100.os.table$BuildingType == "Single Family")
#                                   ,-which(colnames(item100.os.table) %in% c("BuildingType"))]
# 
# exportTable(item100.os.final.SF, "SF", "Table 107", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# 
# 
# #######################
# # Weighted Analysis
# #######################
# item100.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item100.os.data
#                                                    ,valueVariable    = 'DHW.Count'
#                                                    ,columnVariable   = 'Heating.Fuel'
#                                                    ,rowVariable      = 'DHW.Location'
#                                                    ,aggregateColumnName = "Remove")
# item100.os.final <- item100.os.final[which(item100.os.final$Heating.Fuel != "Remove"),]
# 
# item100.os.all.fuels <- proportions_one_group(CustomerLevelData = item100.os.data
#                                            ,valueVariable = "DHW.Count"
#                                            ,groupingVariable = "DHW.Location"
#                                            ,total.name = "All Fuels"
#                                            ,columnName = "Heating.Fuel"
#                                            ,weighted = FALSE
#                                            ,two.prop.total = TRUE)
# 
# item100.os.final <- rbind.data.frame(item100.os.final, item100.os.all.fuels)
# 
# item100.os.cast <- dcast(setDT(item100.os.final)
#                       ,formula = BuildingType + DHW.Location ~ Heating.Fuel
#                       ,value.var = c("Percent", "SE", "Count", "n"))
# 
# item100.os.table <- data.frame("BuildingType"         = item100.os.cast$BuildingType
#                             ,"DHW.Location"        = item100.os.cast$DHW.Location
#                             ,"Percent.Electric"    = item100.os.cast$Percent_Electric
#                             ,"SE.Electric"         = item100.os.cast$SE_Electric
#                             ,"n.Electric"          = item100.os.cast$n_Electric
#                             ,"Percent.Natural.Gas" = item100.os.cast$Percent_Gas
#                             ,"SE.Natural.Gas"      = item100.os.cast$SE_Gas
#                             ,"n.Gas"               = item100.os.cast$n_Gas
#                             ,"Percent.Oil"         = item100.os.cast$Percent_Oil
#                             ,"SE.Oil"              = item100.os.cast$SE_Oil
#                             ,"n.Oil"               = item100.os.cast$n_Oil
#                             # ,"Percent.Pellets"     = item100.os.cast$Percent_Pellets
#                             # ,"SE.Pellets"          = item100.os.cast$SE_Pellets
#                             # ,"n.Pellets"           = item100.os.cast$n_Pellets
#                             # ,"Percent.Propane"     = item100.os.cast$Percent_Propane
#                             # ,"SE.Propane"          = item100.os.cast$SE_Propane
#                             # ,"n.Propane"           = item100.os.cast$n_Propane
#                             ,"Percent.Wood"        = item100.os.cast$Percent_Wood
#                             ,"SE.Wood"             = item100.os.cast$SE_Wood
#                             ,"n.Wood"              = item100.os.cast$n_Wood
#                             ,"Percent.All.Heating.Fuel.Types" = item100.os.cast$`Percent_All Fuels`
#                             ,"SE.All.Heating.Fuel.Types"      = item100.os.cast$`SE_All Fuels`
#                             ,"n_All.Heating.Fuel.Types"       = item100.os.cast$`n_All Fuels`)
# 
# item100.os.final.SF <- item100.os.table[which(item100.os.table$BuildingType == "Single Family")
#                                   ,-which(colnames(item100.os.table) %in% c("BuildingType"))]
# 
# exportTable(item100.os.final.SF, "SF", "Table 107", weighted = FALSE, osIndicator = export.ind, OS = T)
# 
# 
# 
# 
# #############################################################################################
# #Item 101: DISTRIBUTION OF GAS WATER HEATER LOCATIONS BY SPACE HEATING FUEL TYPE (SF table 108)
# #############################################################################################
# item101.os.merge <- item100.os.join1[which(item100.os.join1$DHW.Fuel == "Natural Gas"),]
# item101.os.merge <- item101.os.merge[which(item101.os.merge$Heating.Fuel != "Can't Determine"),]
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item101.os.data <- weightedData(item101.os.merge[-which(colnames(item101.os.merge) %in% c("Heating.Fuel"               
#                                                                                  ,"DHW.Count"
#                                                                                  ,"DHW.Location"
#                                                                                  ,"Heat.Count"
#                                                                                  ,"DHW.Fuel"))])
# item101.os.data <- left_join(item101.os.data, unique(item101.os.merge[which(colnames(item101.os.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Heating.Fuel"               
#                                                                                            ,"DHW.Count"
#                                                                                            ,"DHW.Location"
#                                                                                            ,"Heat.Count"
#                                                                                            ,"DHW.Fuel"))]))
# item101.os.data <- item101.os.data[which(item101.os.data$CK_Building_ID == subset.ind),]
# #######################
# # Weighted Analysis
# #######################
# item101.os.final <- proportionRowsAndColumns1(CustomerLevelData = item101.os.data
#                                            ,valueVariable    = 'DHW.Count'
#                                            ,columnVariable   = 'Heating.Fuel'
#                                            ,rowVariable      = 'DHW.Location'
#                                            ,aggregateColumnName = "Remove")
# item101.os.final <- item101.os.final[which(item101.os.final$Heating.Fuel != "Remove"),]
# 
# item101.os.all.fuels <- proportions_one_group(CustomerLevelData = item101.os.data
#                                            ,valueVariable = "DHW.Count"
#                                            ,groupingVariable = "DHW.Location"
#                                            ,total.name = "All Fuels"
#                                            ,columnName = "Heating.Fuel"
#                                            ,weighted = TRUE
#                                            ,two.prop.total = TRUE)
# 
# item101.os.final <- rbind.data.frame(item101.os.final, item101.os.all.fuels)
# 
# item101.os.cast <- dcast(setDT(item101.os.final)
#                       ,formula = BuildingType + DHW.Location ~ Heating.Fuel
#                       ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
# names(item101.os.cast)
# 
# if(os.ind == "scl"){
#   item101.os.table <- data.frame("BuildingType"         = item101.os.cast$BuildingType
#                                  ,"DHW.Location"        = item101.os.cast$DHW.Location
#                                  ,"Percent.Electric"    = item101.os.cast$w.percent_Electric
#                                  ,"SE.Electric"         = item101.os.cast$w.SE_Electric
#                                  ,"n.Electric"          = item101.os.cast$n_Electric
#                                  ,"Percent.Natural.Gas" = item101.os.cast$w.percent_Gas
#                                  ,"SE.Natural.Gas"      = item101.os.cast$w.SE_Gas
#                                  ,"n.Gas"               = item101.os.cast$n_Gas
#                                  # ,"Percent.Oil"         = item101.os.cast$w.percent_Oil
#                                  # ,"SE.Oil"              = item101.os.cast$w.SE_Oil
#                                  # ,"n.Oil"               = item101.os.cast$n_Oil
#                                  # ,"Percent.Pellets"     = item101.os.cast$w.percent_Pellets
#                                  # ,"SE.Pellets"          = item101.os.cast$w.SE_Pellets
#                                  # ,"n.Pellets"           = item101.os.cast$n_Pellets
#                                  # ,"Percent.Propane"     = item101.os.cast$w.percent_Propane
#                                  # ,"SE.Propane"          = item101.os.cast$w.SE_Propane
#                                  # ,"n.Propane"           = item101.os.cast$n_Propane
#                                  ,"Percent.Wood"        = item101.os.cast$w.percent_Wood
#                                  ,"SE.Wood"             = item101.os.cast$w.SE_Wood
#                                  ,"n.Wood"              = item101.os.cast$n_Wood
#                                  ,"Percent.All.Heating.Fuel.Types" = item101.os.cast$`w.percent_All Fuels`
#                                  ,"SE.All.Heating.Fuel.Types"      = item101.os.cast$`w.SE_All Fuels`
#                                  ,"n.All.Heating.Fuel.Types"       = item101.os.cast$`n_All Fuels`
#                                  ,"EB_Electric"          = item101.os.cast$EB_Electric
#                                  ,"EB_Gas"               = item101.os.cast$EB_Gas
#                                  # ,"EB_Oil"               = item101.os.cast$EB_Oil
#                                  # ,"EB_Pellets"           = item101.os.cast$EB_Pellets
#                                  # ,"EB_Propane"           = item101.os.cast$EB_Propane
#                                  ,"EB_Wood"              = item101.os.cast$EB_Wood
#                                  ,"EB_All.Heating.Fuels" = item101.os.cast$`EB_All Fuels`)
#   
# }else if(os.ind == "snopud"){
#   item101.os.table <- data.frame("BuildingType"         = item101.os.cast$BuildingType
#                                  ,"DHW.Location"        = item101.os.cast$DHW.Location
#                                  ,"Percent.Electric"    = item101.os.cast$w.percent_Electric
#                                  ,"SE.Electric"         = item101.os.cast$w.SE_Electric
#                                  ,"n.Electric"          = item101.os.cast$n_Electric
#                                  ,"Percent.Natural.Gas" = item101.os.cast$w.percent_Gas
#                                  ,"SE.Natural.Gas"      = item101.os.cast$w.SE_Gas
#                                  ,"n.Gas"               = item101.os.cast$n_Gas
#                                  # ,"Percent.Oil"         = item101.os.cast$w.percent_Oil
#                                  # ,"SE.Oil"              = item101.os.cast$w.SE_Oil
#                                  # ,"n.Oil"               = item101.os.cast$n_Oil
#                                  # ,"Percent.Pellets"     = item101.os.cast$w.percent_Pellets
#                                  # ,"SE.Pellets"          = item101.os.cast$w.SE_Pellets
#                                  # ,"n.Pellets"           = item101.os.cast$n_Pellets
#                                  # ,"Percent.Propane"     = item101.os.cast$w.percent_Propane
#                                  # ,"SE.Propane"          = item101.os.cast$w.SE_Propane
#                                  # ,"n.Propane"           = item101.os.cast$n_Propane
#                                  # ,"Percent.Wood"        = item101.os.cast$w.percent_Wood
#                                  # ,"SE.Wood"             = item101.os.cast$w.SE_Wood
#                                  # ,"n.Wood"              = item101.os.cast$n_Wood
#                                  ,"Percent.All.Heating.Fuel.Types" = item101.os.cast$`w.percent_All Fuels`
#                                  ,"SE.All.Heating.Fuel.Types"      = item101.os.cast$`w.SE_All Fuels`
#                                  ,"n.All.Heating.Fuel.Types"       = item101.os.cast$`n_All Fuels`
#                                  ,"EB_Electric"          = item101.os.cast$EB_Electric
#                                  ,"EB_Gas"               = item101.os.cast$EB_Gas
#                                  # ,"EB_Oil"               = item101.os.cast$EB_Oil
#                                  # ,"EB_Pellets"           = item101.os.cast$EB_Pellets
#                                  # ,"EB_Propane"           = item101.os.cast$EB_Propane
#                                  # ,"EB_Wood"              = item101.os.cast$EB_Wood
#                                  ,"EB_All.Heating.Fuels" = item101.os.cast$`EB_All Fuels`)
#   
# }
# 
# item101.os.final.SF <- item101.os.table[which(item101.os.table$BuildingType == "Single Family")
#                                   ,-which(colnames(item101.os.table) %in% c("BuildingType"))]
# 
# exportTable(item101.os.final.SF, "SF", "Table 108", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# 
# 
# #######################
# # Weighted Analysis
# #######################
# item101.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item101.os.data
#                                                    ,valueVariable    = 'DHW.Count'
#                                                    ,columnVariable   = 'Heating.Fuel'
#                                                    ,rowVariable      = 'DHW.Location'
#                                                    ,aggregateColumnName = "Remove")
# item101.os.final <- item101.os.final[which(item101.os.final$Heating.Fuel != "Remove"),]
# 
# item101.os.all.fuels <- proportions_one_group(CustomerLevelData = item101.os.data
#                                            ,valueVariable = "DHW.Count"
#                                            ,groupingVariable = "DHW.Location"
#                                            ,total.name = "All Fuels"
#                                            ,columnName = "Heating.Fuel"
#                                            ,weighted = FALSE
#                                            ,two.prop.total = TRUE)
# 
# item101.os.final <- rbind.data.frame(item101.os.final, item101.os.all.fuels)
# 
# item101.os.cast <- dcast(setDT(item101.os.final)
#                       ,formula = BuildingType + DHW.Location ~ Heating.Fuel
#                       ,value.var = c("Percent", "SE", "Count", "n"))
# 
# if(os.ind == "scl"){
#   item101.os.table <- data.frame("BuildingType"         = item101.os.cast$BuildingType
#                                  ,"DHW.Location"        = item101.os.cast$DHW.Location
#                                  ,"Percent.Electric"    = item101.os.cast$Percent_Electric
#                                  ,"SE.Electric"         = item101.os.cast$SE_Electric
#                                  ,"n.Electric"          = item101.os.cast$n_Electric
#                                  ,"Percent.Natural.Gas" = item101.os.cast$Percent_Gas
#                                  ,"SE.Natural.Gas"      = item101.os.cast$SE_Gas
#                                  ,"n.Gas"               = item101.os.cast$n_Gas
#                                  # ,"Percent.Oil"         = item101.os.cast$Percent_Oil
#                                  # ,"SE.Oil"              = item101.os.cast$SE_Oil
#                                  # ,"n.Oil"               = item101.os.cast$n_Oil
#                                  # ,"Percent.Pellets"     = item101.os.cast$Percent_Pellets
#                                  # ,"SE.Pellets"          = item101.os.cast$SE_Pellets
#                                  # ,"n.Pellets"           = item101.os.cast$n_Pellets
#                                  # ,"Percent.Propane"     = item101.os.cast$Percent_Propane
#                                  # ,"SE.Propane"          = item101.os.cast$SE_Propane
#                                  # ,"n.Propane"           = item101.os.cast$n_Propane
#                                  ,"Percent.Wood"        = item101.os.cast$Percent_Wood
#                                  ,"SE.Wood"             = item101.os.cast$SE_Wood
#                                  ,"n.Wood"              = item101.os.cast$n_Wood
#                                  ,"Percent.All.Heating.Fuel.Types" = item101.os.cast$`Percent_All Fuels`
#                                  ,"SE.All.Heating.Fuel.Types"      = item101.os.cast$`SE_All Fuels`
#                                  ,"n_All.Heating.Fuel.Types"       = item101.os.cast$`n_All Fuels`)
#   
# }else if(os.ind == "snopud"){
#   item101.os.table <- data.frame("BuildingType"         = item101.os.cast$BuildingType
#                                  ,"DHW.Location"        = item101.os.cast$DHW.Location
#                                  ,"Percent.Electric"    = item101.os.cast$Percent_Electric
#                                  ,"SE.Electric"         = item101.os.cast$SE_Electric
#                                  ,"n.Electric"          = item101.os.cast$n_Electric
#                                  ,"Percent.Natural.Gas" = item101.os.cast$Percent_Gas
#                                  ,"SE.Natural.Gas"      = item101.os.cast$SE_Gas
#                                  ,"n.Gas"               = item101.os.cast$n_Gas
#                                  # ,"Percent.Oil"         = item101.os.cast$Percent_Oil
#                                  # ,"SE.Oil"              = item101.os.cast$SE_Oil
#                                  # ,"n.Oil"               = item101.os.cast$n_Oil
#                                  # ,"Percent.Pellets"     = item101.os.cast$Percent_Pellets
#                                  # ,"SE.Pellets"          = item101.os.cast$SE_Pellets
#                                  # ,"n.Pellets"           = item101.os.cast$n_Pellets
#                                  # ,"Percent.Propane"     = item101.os.cast$Percent_Propane
#                                  # ,"SE.Propane"          = item101.os.cast$SE_Propane
#                                  # ,"n.Propane"           = item101.os.cast$n_Propane
#                                  # ,"Percent.Wood"        = item101.os.cast$Percent_Wood
#                                  # ,"SE.Wood"             = item101.os.cast$SE_Wood
#                                  # ,"n.Wood"              = item101.os.cast$n_Wood
#                                  ,"Percent.All.Heating.Fuel.Types" = item101.os.cast$`Percent_All Fuels`
#                                  ,"SE.All.Heating.Fuel.Types"      = item101.os.cast$`SE_All Fuels`
#                                  ,"n_All.Heating.Fuel.Types"       = item101.os.cast$`n_All Fuels`)
#   
# }
# 
# item101.os.final.SF <- item101.os.table[which(item101.os.table$BuildingType == "Single Family")
#                                   ,-which(colnames(item101.os.table) %in% c("BuildingType"))]
# 
# exportTable(item101.os.final.SF, "SF", "Table 108", weighted = FALSE, osIndicator = export.ind, OS = T)
