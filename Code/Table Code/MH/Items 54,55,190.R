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
# Mechanical
# download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx', mechanical.export, mode = 'wb')
# mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))





################################################################################################
# ITEM 54 (and item 190 for MH): PERCENTAGE OF HOMES WITH COOLING EQUIPMENT BY COOLING ZONE AND STATE (SF table 61)
################################################################################################
#subset to columns needed for analysis
item54.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Provides"))]
item54.dat1 <- unique(item54.dat[grep("cooling",item54.dat$Provides, ignore.case = T),])
which(duplicated(item54.dat1$CK_Cadmus_ID))
item54.dat1.1 <- item54.dat1[-which(duplicated(item54.dat1$CK_Cadmus_ID)),]

item54.dat2 <- left_join(rbsa.dat, item54.dat1.1)

item54.dat2$Ind <- 0
item54.dat2$Ind[which(!is.na(item54.dat2$Provides))] <- 1
unique(item54.dat2$Ind)

##########################################
# add pop and sample sizes by strata
##########################################
item54.data <- weightedData(item54.dat2[-which(colnames(item54.dat2) %in% c("Provides"
                                                                            ,"Ind"))])
item54.data <- left_join(item54.data, item54.dat2[which(colnames(item54.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"Provides"
                                                                                     ,"Ind"))])
item54.data$Count <- 1


##############################
# Weighted Analysis
##############################
item54.summary <- proportionRowsAndColumns1(CustomerLevelData = item54.data
                                            ,valueVariable = 'Ind'
                                            ,columnVariable = 'Cooling.Zone'
                                            ,rowVariable = 'State'
                                            ,aggregateColumnName = "Remove")
item54.summary$State[which(item54.summary$State == "Total")] <- "Region"
item54.summary <- item54.summary[which(item54.summary$Cooling.Zone != "Remove"),]

item54.all.cooling.zones <- proportions_one_group(CustomerLevelData = item54.data
                                                  ,valueVariable = 'Ind'
                                                  ,groupingVariable = 'State'
                                                  ,total.name = 'All Cooling Zones'
                                                  ,columnName = 'Cooling.Zone'
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE)
item54.all.cooling.zones$State[which(item54.all.cooling.zones$State == "Total")] <- "Region"

item54.final <- rbind.data.frame(item54.summary, item54.all.cooling.zones, stringsAsFactors = F)

item54.cast <- dcast(setDT(item54.final)
                     ,formula = BuildingType + Cooling.Zone ~ State
                     ,value.var = c("w.percent","w.SE","count","N","n", "EB"))

item54.table <- data.frame("BuildingType"    = item54.cast$BuildingType
                           ,"Cooling.Zone"   = item54.cast$Cooling.Zone
                           ,"Percent_ID"     = item54.cast$w.percent_ID
                           ,"SE_ID"          = item54.cast$w.SE_ID
                           ,"n_ID"           = item54.cast$n_ID
                           ,"Percent_MT"     = item54.cast$w.percent_MT
                           ,"SE_MT"          = item54.cast$w.SE_MT
                           ,"n_MT"           = item54.cast$n_MT
                           ,"Percent_OR"     = item54.cast$w.percent_OR
                           ,"SE_OR"          = item54.cast$w.SE_OR
                           ,"n_OR"           = item54.cast$n_OR
                           ,"Percent_WA"     = item54.cast$w.percent_WA
                           ,"SE_WA"          = item54.cast$w.SE_WA
                           ,"n_WA"           = item54.cast$n_WA
                           ,"Percent_Region" = item54.cast$w.percent_Region
                           ,"SE_Region"      = item54.cast$w.SE_Region
                           ,"n_Region"       = item54.cast$n_Region
                           ,"EB_ID"          = item54.cast$EB_ID
                           ,"EB_MT"          = item54.cast$EB_MT
                           ,"EB_OR"          = item54.cast$EB_OR
                           ,"EB_WA"          = item54.cast$EB_WA
                           ,"EB_Region"      = item54.cast$EB_Region)

item54.table.SF <- item54.table[which(item54.table$BuildingType == "Single Family")
                                ,which(colnames(item54.table) %notin% c("BuildingType"))]
item54.table.MH <- item54.table[which(item54.table$BuildingType == "Manufactured")
                                ,which(colnames(item54.table) %notin% c("BuildingType"))]

# exportTable(item54.table.SF, "SF", "Table 61", weighted = TRUE)
exportTable(item54.table.MH, "MH", "Table 41", weighted = TRUE)

##############################
# unweighted Analysis
##############################
item54.summary <- proportions_two_groups_unweighted(CustomerLevelData = item54.data
                                            ,valueVariable = 'Ind'
                                            ,columnVariable = 'Cooling.Zone'
                                            ,rowVariable = 'State'
                                            ,aggregateColumnName = "Remove")
item54.summary <- item54.summary[which(item54.summary$Cooling.Zone != "Remove"),]

item54.all.cooling.zones <- proportions_one_group(CustomerLevelData = item54.data
                                                  ,valueVariable = 'Ind'
                                                  ,groupingVariable = 'State'
                                                  ,total.name = 'All Cooling Zones'
                                                  ,columnName = 'Cooling.Zone'
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE)

item54.final <- rbind.data.frame(item54.summary, item54.all.cooling.zones, stringsAsFactors = F)

item54.final$State[which(item54.final$State == "Total")] <- "Region"

item54.cast <- dcast(setDT(item54.final)
                     ,formula = BuildingType + Cooling.Zone ~ State
                     ,value.var = c("Percent","SE","Count","n"))

item54.table <- data.frame("BuildingType"    = item54.cast$BuildingType
                           ,"Cooling.Zone"   = item54.cast$Cooling.Zone
                           ,"Percent_ID"     = item54.cast$Percent_ID
                           ,"SE_ID"          = item54.cast$SE_ID
                           ,"n_ID"           = item54.cast$n_ID
                           ,"Percent_MT"     = item54.cast$Percent_MT
                           ,"SE_MT"          = item54.cast$SE_MT
                           ,"n_MT"           = item54.cast$n_MT
                           ,"Percent_OR"     = item54.cast$Percent_OR
                           ,"SE_OR"          = item54.cast$SE_OR
                           ,"n_OR"           = item54.cast$n_OR
                           ,"Percent_WA"     = item54.cast$Percent_WA
                           ,"SE_WA"          = item54.cast$SE_WA
                           ,"n_WA"           = item54.cast$n_WA
                           ,"Percent_Region" = item54.cast$Percent_Region
                           ,"SE_Region"      = item54.cast$SE_Region
                           ,"n_Region"       = item54.cast$n_Region)

item54.table.SF <- item54.table[which(item54.table$BuildingType == "Single Family")
                                ,which(colnames(item54.table) %notin% c("BuildingType"))]
item54.table.MH <- item54.table[which(item54.table$BuildingType == "Manufactured")
                                ,which(colnames(item54.table) %notin% c("BuildingType"))]

# exportTable(item54.table.SF, "SF", "Table 61", weighted = FALSE)
exportTable(item54.table.MH, "MH", "Table 41", weighted = FALSE)





################################################################################################
# ITEM 55: PERCENTAGE OF HOMES WITH COOLING EQUIPMENT BY COOLING ZONE AND STATE (SF table 61)
################################################################################################
#subset to columns needed for analysis
item55.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Primary.Cooling.System"
                                                                   ,"System.Type"))]
item55.dat1 <- unique(item55.dat[grep("yes",item55.dat$Primary.Cooling.System, ignore.case = T),])
unique(item55.dat1$CK_Cadmus_ID[which(duplicated(item55.dat1$CK_Cadmus_ID))])
item55.dat1 <- item55.dat1[which(item55.dat1$System.Type %notin% c("N/A",NA)),]
unique(item55.dat1$System.Type)

item55.dat1$System.Type[grep("central", item55.dat1$System.Type, ignore.case = T)] <- "Central Ac"
item55.dat1$System.Type[grep("packaged unit", item55.dat1$System.Type, ignore.case = T)] <- "Packaged Hp"


item55.dat2 <- left_join(rbsa.dat, item55.dat1)

item55.dat2$Dist.Ind <- 0
item55.dat2$Dist.Ind[which(!is.na(item55.dat2$Primary.Cooling.System))] <- 1
unique(item55.dat2$Dist.Ind)
unique(item55.dat2$System.Type)

item55.dat3 <- item55.dat2[which(!is.na(item55.dat2$System.Type)),]

##########################################
# add pop and sample sizes by strata
##########################################
item55.data <- weightedData(item55.dat3[-which(colnames(item55.dat3) %in% c("Primary.Cooling.System"
                                                                            ,"System.Type"
                                                                            ,"Dist.Ind"))])
item55.data <- left_join(item55.data, item55.dat3[which(colnames(item55.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"Primary.Cooling.System"
                                                                                     ,"System.Type"
                                                                                     ,"Dist.Ind"))])
item55.data$Count <- 1
item55.data$count <- 1


##############################
# Weighted Analysis - item 190 (MH)
##############################
item190.summary <- proportionRowsAndColumns1(CustomerLevelData = item55.data
                                            ,valueVariable = 'Dist.Ind'
                                            ,columnVariable = 'System.Type'
                                            ,rowVariable = 'Cooling.Zone'
                                            ,aggregateColumnName = "Total")
# item190.summary <- item190.summary[which(item190.summary$Cooling.Zone != "Total"),]
item190.summary <- item190.summary[which(item190.summary$System.Type != "Total"),]

item190.all.cooling.zones <- proportions_one_group(CustomerLevelData = item55.data
                                                  ,valueVariable = 'Dist.Ind'
                                                  ,groupingVariable = 'System.Type'
                                                  ,total.name = 'All Cooling Zones'
                                                  ,columnName = 'Cooling.Zone'
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE)
item190.all.cooling.zones$System.Type[which(item190.all.cooling.zones$System.Type=="Total")] <- "All Types"
item190.all.system.types <- proportions_one_group(CustomerLevelData = item55.data
                                                 ,valueVariable = 'Dist.Ind'
                                                 ,groupingVariable = 'Cooling.Zone'
                                                 ,total.name = 'All Types'
                                                 ,columnName = 'System.Type'
                                                 ,weighted = TRUE
                                                 ,two.prop.total = TRUE)
item190.all.system.types$System.Type[which(item190.all.system.types$System.Type=="Total")] <- "All Cooling Zones"

item190.final <- rbind.data.frame(item190.summary, item190.all.cooling.zones,item190.all.system.types, stringsAsFactors = F)

item190.cast <- dcast(setDT(item190.final)
                     ,formula = BuildingType + System.Type ~ Cooling.Zone
                     ,value.var = c("w.percent","w.SE","count","N","n","EB"))

item190.table <- data.frame("BuildingType"               = item190.cast$BuildingType
                           ,"Cooling.System.Type"       = item190.cast$System.Type
                           ,"Percent.Cooling.Zone.1"    = item190.cast$w.percent_1
                           ,"SE.Cooling.Zone.1"         = item190.cast$w.SE_1
                           ,"Percent.Cooling.Zone.2"    = item190.cast$w.percent_2
                           ,"SE.Cooling.Zone.2"         = item190.cast$w.SE_2
                           ,"Percent.Cooling.Zone.3"    = item190.cast$w.percent_3
                           ,"SE.Cooling.Zone.3"         = item190.cast$w.SE_3
                           ,"Percent.All.Cooling.Zones" = item190.cast$`w.percent_All Cooling Zones`
                           ,"SE.All.Cooling.Zones"      = item190.cast$`w.SE_All Cooling Zones`
                           ,"n.All.Cooling.Zones"       = item190.cast$`n_All Cooling Zones`
                           ,"EB_Cooling.Zone.1"         = item190.cast$EB_1
                           ,"EB_Cooling.Zone.2"         = item190.cast$EB_2
                           ,"EB_Cooling.Zone.3"         = item190.cast$EB_3
                           ,"EB_All.Cooling.Zones"      = item190.cast$`EB_All Cooling Zones`)
# row ordering example code
levels(item190.table$Cooling.System.Type)
rowOrder <- c("Packaged Ac"
              ,"Packaged Hp"
              ,"Central Ac"
              ,"Evaporative Cooling"
              ,"Water Source Heat Pump"
              ,"Air Source Heat Pump"
              ,"Mini-Split Hp"
              ,"Mini-Split Ac"
              ,"Furnace"
              ,"Geothermal Heat Pump"
              ,"All Types")
item190.table <- item190.table %>% mutate(Cooling.System.Type = factor(Cooling.System.Type, levels = rowOrder)) %>% arrange(Cooling.System.Type)
item190.table <- data.frame(item190.table)

item190.table.MH <- item190.table[which(item190.table$BuildingType == "Manufactured")
                                ,which(colnames(item190.table) %notin% c("BuildingType"))]

exportTable(item190.table.MH, "MH", "Table 42", weighted = TRUE)

##############################
# unweighted Analysis
##############################
item190.summary <- proportions_two_groups_unweighted(CustomerLevelData = item55.data
                                                    ,valueVariable = 'Dist.Ind'
                                                    ,columnVariable = 'System.Type'
                                                    ,rowVariable = 'Cooling.Zone'
                                                    ,aggregateColumnName = "Total")
# item190.summary <- item190.summary[which(item190.summary$Cooling.Zone != "Total"),]
item190.summary <- item190.summary[which(item190.summary$System.Type != "Total"),]

item190.all.cooling.zones <- proportions_one_group(CustomerLevelData = item55.data
                                                  ,valueVariable = 'Dist.Ind'
                                                  ,groupingVariable = 'System.Type'
                                                  ,total.name = 'All Cooling Zones'
                                                  ,columnName = 'Cooling.Zone'
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE)
item190.all.cooling.zones$System.Type[which(item190.all.cooling.zones$System.Type=="Total")] <- "All Types"
item190.all.system.types <- proportions_one_group(CustomerLevelData = item55.data
                                                 ,valueVariable = 'Dist.Ind'
                                                 ,groupingVariable = 'Cooling.Zone'
                                                 ,total.name = 'All Types'
                                                 ,columnName = 'System.Type'
                                                 ,weighted = FALSE
                                                 ,two.prop.total = TRUE)
item190.all.system.types$System.Type[which(item190.all.system.types$System.Type=="Total")] <- "All Cooling Zones"

item190.final <- rbind.data.frame(item190.summary, item190.all.cooling.zones,item190.all.system.types, stringsAsFactors = F)

item190.cast <- dcast(setDT(item190.final)
                     ,formula = BuildingType + System.Type ~ Cooling.Zone
                     ,value.var = c("Percent","SE","Count","n"))

item190.table <- data.frame("BuildingType"               = item190.cast$BuildingType
                           ,"Cooling.System.Type"       = item190.cast$System.Type
                           ,"Percent.Cooling.Zone.1"    = item190.cast$Percent_1
                           ,"SE.Cooling.Zone.1"         = item190.cast$SE_1
                           ,"Percent.Cooling.Zone.2"    = item190.cast$Percent_2
                           ,"SE.Cooling.Zone.2"         = item190.cast$SE_2
                           ,"Percent.Cooling.Zone.3"    = item190.cast$Percent_3
                           ,"SE.Cooling.Zone.3"         = item190.cast$SE_3
                           ,"Percent.All.Cooling.Zones" = item190.cast$`Percent_All Cooling Zones`
                           ,"SE.All.Cooling.Zones"      = item190.cast$`SE_All Cooling Zones`
                           ,"n.All.Cooling.Zones"       = item190.cast$`n_All Cooling Zones`)
# row ordering example code
levels(item190.table$Cooling.System.Type)
rowOrder <- c("Packaged Ac"
              ,"Packaged Hp"
              ,"Central Ac"
              ,"Evaporative Cooling"
              ,"Water Source Heat Pump"
              ,"Air Source Heat Pump"
              ,"Mini-Split Hp"
              ,"Mini-Split Ac"
              ,"Furnace"
              ,"Geothermal Heat Pump"
              ,"All Types")
item190.table <- item190.table %>% mutate(Cooling.System.Type = factor(Cooling.System.Type, levels = rowOrder)) %>% arrange(Cooling.System.Type)
item190.table <- data.frame(item190.table)

item190.table.MH <- item190.table[which(item190.table$BuildingType == "Manufactured")
                                ,which(colnames(item190.table) %notin% c("BuildingType"))]

exportTable(item190.table.MH, "MH", "Table 42", weighted = FALSE)
