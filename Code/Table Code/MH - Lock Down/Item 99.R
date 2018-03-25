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
# stopifnot(nrow(item99.heat.final) == length(unique(item99.heat.final$CK_Cadmus_ID)))

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

exportTable(item99.final.MH, "MH", "Table 86", weighted = TRUE)

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

exportTable(item99.final.MH, "MH", "Table 86", weighted = FALSE)


