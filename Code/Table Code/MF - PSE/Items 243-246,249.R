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
source("Code/Table Code/Weighting Implementation - MF-BLDG.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
rbsa.dat.MF <- rbsa.dat[which(rbsa.dat$BuildingType == "Multifamily"),]
rbsa.dat.site <- rbsa.dat.MF[grep("site",rbsa.dat.MF$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat.MF[grep("bldg",rbsa.dat.MF$CK_Building_ID, ignore.case = T),]

# one.line.bldg.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), startRow = 2)

one.line.bldg.dat1 <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("PK_BuildingID"
                                                                                 ,"Primary.Heating.System"
                                                                                 ,"Primary.Heating.Fuel"
                                                                                 ,"Primary.Cooling.System"
                                                                                 ,"Central.Building.Heat"))]

one.line.bldg.dat2  <- left_join(rbsa.dat.bldg, one.line.bldg.dat1, by = c("CK_Building_ID" = "PK_BuildingID"))
one.line.bldg.dat2 <- one.line.bldg.dat2[which(one.line.bldg.dat2$Primary.Heating.System %notin% c("N/A","Unknown",NA)),]
length(unique(one.line.bldg.dat2$CK_Cadmus_ID))


#############################################################################################
#Item 243: DISTRIBUTION OF PRIMARY HEATING SYSTEMS BY FUEL / Table 35
#############################################################################################
####################################
# For Central Building Systems
####################################
item243.dat <- one.line.bldg.dat2#[which(!is.na(one.line.bldg.dat2$Central.Building.Heat)),]

unique(item243.dat$Primary.Heating.Fuel)
unique(item243.dat$Primary.Heating.System)

#clean heating systems
item243.dat$Primary.Heating.System[grep("Electric Baseboard|baseboard electric",item243.dat$Primary.Heating.System,ignore.case = T)] <- "Electric Baseboard"
item243.dat$Primary.Heating.System[grep("zonal heat",item243.dat$Primary.Heating.System,ignore.case = T)] <- "Other Zonal Heat"
item243.dat$Primary.Heating.System[grep("ductless",item243.dat$Primary.Heating.System,ignore.case = T)] <- "Mini-split HP"
item243.dat$Primary.Heating.System[grep("furnace",item243.dat$Primary.Heating.System,ignore.case = T)] <- "Furnace"
item243.dat$Primary.Heating.System[grep("boiler",item243.dat$Primary.Heating.System,ignore.case = T)] <- "Boiler"


ii=3
for (ii in 1:nrow(item243.dat)){
  if(item243.dat$Central.Building.Heat[ii] != "N/A"){
    item243.dat$Primary.Heating.System[ii] <- paste("Central", item243.dat$Primary.Heating.System[ii])
  }else {
    item243.dat$Primary.Heating.System[ii] <- item243.dat$Primary.Heating.System[ii]
  }
}
item243.dat$Primary.Heating.System[grep("Boiler", item243.dat$Primary.Heating.System)] <- "Central Boiler"
#clean heating fuels
item243.dat$Primary.Heating.Fuel[grep("Wood (cord)",item243.dat$Primary.Heating.Fuel,ignore.case = T)] <- "Wood"
unique(item243.dat$Primary.Heating.Fuel)

unique(item243.dat$Primary.Heating.System)

item243.merge <- left_join(rbsa.dat.bldg, item243.dat)
item243.merge0 <- item243.merge[which(!is.na(item243.merge$Primary.Heating.System)),]
item243.merge <- item243.merge0[which(item243.merge0$Primary.Heating.Fuel %notin% c("N/A","Unknown",NA)),]
item243.merge <- item243.merge[which(item243.merge$Category == "PSE"),]

################################################
# Adding pop and sample sizes for weights
################################################
item243.data <- weightedData(item243.merge[-which(colnames(item243.merge) %in% c("Primary.Heating.System"
                                                                                 ,"Primary.Heating.Fuel"
                                                                                 ,"Primary.Cooling.System"
                                                                                 ,"Central.Building.Heat"
                                                                                 ,"Category"))])
item243.data <- left_join(item243.data, item243.merge[which(colnames(item243.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Primary.Heating.System"
                                                                                           ,"Primary.Heating.Fuel"
                                                                                           ,"Primary.Cooling.System"
                                                                                           ,"Central.Building.Heat"
                                                                                           ,"Category"))])

item243.data$count <- 1
length(unique(item243.data$CK_Cadmus_ID[grep("central",item243.data$Primary.Heating.System, ignore.case = T)]))
#######################
# Weighted Analysis
#######################
item243.final <- proportionRowsAndColumns1(CustomerLevelData = item243.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'Primary.Heating.System'
                                           ,rowVariable      = 'Primary.Heating.Fuel'
                                           ,aggregateColumnName = "All Systems")
item243.final$Primary.Heating.Fuel[which(item243.final$Primary.Heating.Fuel == "Total")] <- "All Fuels"

item243.cast <- dcast(setDT(item243.final)
                      ,formula = Primary.Heating.System ~ Primary.Heating.Fuel
                      ,value.var = c("w.percent","w.SE", "count","n","N","EB"))

names(item243.cast)
item243.table <- data.frame("Primary.Heating.System" = item243.cast$Primary.Heating.System
                            ,"Electric"              = item243.cast$w.percent_Electric
                            ,"Electric.SE"           = item243.cast$w.SE_Electric
                            ,"Natural.Gas"           = item243.cast$`w.percent_Natural Gas`
                            ,"Natural.Gas.SE"        = item243.cast$`w.SE_Natural Gas`
                            ,"Oil"                   = NA
                            ,"Oil.SE"                = NA
                            ,"Wood"                  = NA#item243.cast$`w.percent_Wood (cord)`
                            ,"Wood.SE"               = NA#item243.cast$`w.SE_Wood (cord)`
                            ,"All.Types"             = item243.cast$`w.percent_All Fuels`
                            ,"All.Types.SE"          = item243.cast$`w.SE_All Fuels`
                            ,"n"                     = item243.cast$`n_All Fuels`
                            ,"Electric.EB"           = item243.cast$EB_Electric
                            ,"Natural.Gas.EB"        = item243.cast$`EB_Natural Gas`
                            ,"Oil.EB"                = NA
                            ,"Wood.EB"               = NA#item243.cast$`EB_Wood (cord)`
                            ,"All.Types.EB"          = item243.cast$`EB_All Fuels`
                            )

levels(item243.table$Primary.Heating.System)
rowOrder <- c("Central Boiler"
              ,"Central Furnace"
              ,"Air Handler"
              ,"Air Source Heat Pump"
              ,"Boiler"
              ,"Electric Baseboard"
              ,"Furnace"
              ,"Mini-split HP"
              ,"Other Zonal Heat"
              ,"Package Terminal Heat Pump"
              ,"Stove/Fireplace"
              ,"All Systems")
item243.table <- item243.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item243.table <- data.frame(item243.table)


exportTable(item243.table, "MF", "Table 35", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# unweighted Analysis
#######################
item243.final <- proportions_two_groups_unweighted(CustomerLevelData = item243.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'Primary.Heating.System'
                                                   ,rowVariable      = 'Primary.Heating.Fuel'
                                                   ,aggregateColumnName = "All Systems")
item243.final$Primary.Heating.Fuel[which(item243.final$Primary.Heating.Fuel == "Total")] <- "All Fuels"

item243.cast <- dcast(setDT(item243.final)
                      ,formula = Primary.Heating.System ~ Primary.Heating.Fuel
                      ,value.var = c("Percent","SE", "Count","n"))
names(item243.cast)

item243.table <- data.frame("Primary.Heating.System" = item243.cast$Primary.Heating.System
                            ,"Electric"              = item243.cast$Percent_Electric
                            ,"Electric.SE"           = item243.cast$SE_Electric
                            ,"Natural.Gas"           = item243.cast$`Percent_Natural Gas`
                            ,"Natural.Gas.SE"        = item243.cast$`SE_Natural Gas`
                            ,"Oil"                   = NA
                            ,"Oil.SE"                = NA
                            ,"Wood"                  = NA#item243.cast$`Percent_Wood (cord)`
                            ,"Wood.SE"               = NA#item243.cast$`SE_Wood (cord)`
                            ,"All.Types"             = item243.cast$`Percent_All Fuels`
                            ,"All.Types.SE"          = item243.cast$`SE_All Fuels`
                            ,"n"                     = item243.cast$`n_All Fuels`
)

levels(item243.table$Primary.Heating.System)
rowOrder <- c("Central Boiler"
              ,"Central Furnace"
              ,"Air Handler"
              ,"Air Source Heat Pump"
              ,"Boiler"
              ,"Electric Baseboard"
              ,"Furnace"
              ,"Mini-split HP"
              ,"Other Zonal Heat"
              ,"Package Terminal Heat Pump"
              ,"Stove/Fireplace"
              ,"All Systems")
item243.table <- item243.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item243.table <- data.frame(item243.table)

exportTable(item243.table, "MF", "Table 35", weighted = FALSE,OS = T, osIndicator = "PSE")








#############################################################################################
#Item 244: DISTRIBUTION OF PRIMARY HEATING SYSTEMS BY BUILDING SIZE Table 36
#############################################################################################

item244.merge <- item243.merge0

################################################
# Adding pop and sample sizes for weights
################################################
item244.data <- weightedData(item244.merge[-which(colnames(item244.merge) %in% c("Primary.Heating.System"
                                                                                 ,"Primary.Heating.Fuel"
                                                                                 ,"Primary.Cooling.System"
                                                                                 ,"Central.Building.Heat"
                                                                                 ,"Category"))])
item244.data <- left_join(item244.data, item244.merge[which(colnames(item244.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Primary.Heating.System"
                                                                                           ,"Primary.Heating.Fuel"
                                                                                           ,"Primary.Cooling.System"
                                                                                           ,"Central.Building.Heat"
                                                                                           ,"Category"))])

item244.data$count <- 1
item244.data$Heating_System <- item244.data$Primary.Heating.System
item244.data$Fuel <- item244.data$Primary.Heating.Fuel
#######################
# Weighted Analysis
#######################
item244.summary <- proportionRowsAndColumns1(CustomerLevelData = item244.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'Category'
                                           ,rowVariable      = 'Heating_System'
                                           ,aggregateColumnName = "Remove")
item244.summary <- item244.summary[which(item244.summary$Category != "Remove"),]

item244.cast <- dcast(setDT(item244.summary)
                      ,formula = Heating_System ~ Category
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

names(item244.cast)
item244.table <- data.frame("Primary.Heating.System"= item244.cast$Heating_System
                            ,"PSE.Percent"                 = item244.cast$w.percent_PSE
                            ,"PSE.SE"                      = item244.cast$w.SE_PSE
                            ,"PSE.n"                       = item244.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item244.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item244.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item244.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item244.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item244.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item244.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item244.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item244.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = item244.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = item244.cast$EB_PSE
                            ,"PSE.King.County_EB"          = item244.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = item244.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = item244.cast$`EB_2017 RBSA PS`) 
exportTable(item244.table, "MF", "Table 36A", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# unweighted Analysis
#######################
item244.summary <- proportions_two_groups_unweighted(CustomerLevelData = item244.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable   = 'Category'
                                             ,rowVariable      = 'Heating_System'
                                             ,aggregateColumnName = "Remove")
item244.summary <- item244.summary[which(item244.summary$Category != "Remove"),]

item244.cast <- dcast(setDT(item244.summary)
                      ,formula = Heating_System ~ Category
                      ,value.var = c("Percent", "SE", "Count", "n"))

item244.table <- data.frame("Primary.Heating.System"= item244.cast$Heating_System
                            ,"PSE.Percent"                 = item244.cast$Percent_PSE
                            ,"PSE.SE"                      = item244.cast$SE_PSE
                            ,"PSE.n"                       = item244.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item244.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item244.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item244.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item244.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item244.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item244.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item244.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item244.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS_n"              = item244.cast$`n_2017 RBSA PS`) 

exportTable(item244.table, "MF", "Table 36A", weighted = FALSE,OS = T, osIndicator = "PSE")



#############################################################################################
#Item 244B: DISTRIBUTION OF PRIMARY HEATING SYSTEMS BY BUILDING SIZE Table 36
#############################################################################################

item244B.merge <- item243.merge0

################################################
# Adding pop and sample sizes for weights
################################################
item244B.data <- weightedData(item244B.merge[-which(colnames(item244B.merge) %in% c("Primary.Heating.System"
                                                                                 ,"Primary.Heating.Fuel"
                                                                                 ,"Primary.Cooling.System"
                                                                                 ,"Central.Building.Heat"
                                                                                 ,"Category"))])
item244B.data <- left_join(item244B.data, item244B.merge[which(colnames(item244B.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Primary.Heating.System"
                                                                                           ,"Primary.Heating.Fuel"
                                                                                           ,"Primary.Cooling.System"
                                                                                           ,"Central.Building.Heat"
                                                                                           ,"Category"))])

item244B.data$count <- 1
item244B.data$Heating_System <- item244B.data$Primary.Heating.System
item244B.data$Fuel <- item244B.data$Primary.Heating.Fuel
#######################
# Weighted Analysis
#######################
item244B.summary <- proportionRowsAndColumns1(CustomerLevelData = item244B.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable   = 'Category'
                                             ,rowVariable      = 'Fuel'
                                             ,aggregateColumnName = "Remove")
item244B.summary <- item244B.summary[which(item244B.summary$Category != "Remove"),]

item244B.cast <- dcast(setDT(item244B.summary)
                      ,formula = Fuel ~ Category
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

names(item244B.cast)
item244B.table <- data.frame("Primary.Heating.Fuel"= item244B.cast$Fuel
                            ,"PSE.Percent"                 = item244B.cast$w.percent_PSE
                            ,"PSE.SE"                      = item244B.cast$w.SE_PSE
                            ,"PSE.n"                       = item244B.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item244B.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item244B.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item244B.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item244B.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item244B.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item244B.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item244B.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item244B.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = item244B.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = item244B.cast$EB_PSE
                            ,"PSE.King.County_EB"          = item244B.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = item244B.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = item244B.cast$`EB_2017 RBSA PS`) 
exportTable(item244B.table, "MF", "Table 36B", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# unweighted Analysis
#######################
item244B.summary <- proportions_two_groups_unweighted(CustomerLevelData = item244B.data
                                                     ,valueVariable    = 'count'
                                                     ,columnVariable   = 'Category'
                                                     ,rowVariable      = 'Fuel'
                                                     ,aggregateColumnName = "Remove")
item244B.summary <- item244B.summary[which(item244B.summary$Category != "Remove"),]

item244B.cast <- dcast(setDT(item244B.summary)
                      ,formula = Fuel ~ Category
                      ,value.var = c("Percent", "SE", "Count", "n"))

item244B.table <- data.frame("Primary.Heating.Fuel"= item244B.cast$Fuel
                            ,"PSE.Percent"                 = item244B.cast$Percent_PSE
                            ,"PSE.SE"                      = item244B.cast$SE_PSE
                            ,"PSE.n"                       = item244B.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item244B.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item244B.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item244B.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item244B.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item244B.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item244B.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item244B.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item244B.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = item244B.cast$`n_2017 RBSA PS`) 

exportTable(item244B.table, "MF", "Table 36B", weighted = FALSE,OS = T, osIndicator = "PSE")




#############################################################################################
#Item 244C: DISTRIBUTION OF PRIMARY HEATING SYSTEMS BY BUILDING SIZE Table 36
#############################################################################################

item244C.merge <- item243.merge0[grep("gas", item243.merge0$Primary.Heating.Fuel, ignore.case = T),]

################################################
# Adding pop and sample sizes for weights
################################################
item244C.data <- weightedData(item244C.merge[-which(colnames(item244C.merge) %in% c("Primary.Heating.System"
                                                                                    ,"Primary.Heating.Fuel"
                                                                                    ,"Primary.Cooling.System"
                                                                                    ,"Central.Building.Heat"
                                                                                    ,"Category"))])
item244C.data <- left_join(item244C.data, item244C.merge[which(colnames(item244C.merge) %in% c("CK_Cadmus_ID"
                                                                                               ,"Primary.Heating.System"
                                                                                               ,"Primary.Heating.Fuel"
                                                                                               ,"Primary.Cooling.System"
                                                                                               ,"Central.Building.Heat"
                                                                                               ,"Category"))])

item244C.data$count <- 1
item244C.data$Heating_System <- item244C.data$Primary.Heating.System
item244C.data$Fuel <- item244C.data$Primary.Heating.Fuel
#######################
# Weighted Analysis
#######################
item244C.summary <- proportionRowsAndColumns1(CustomerLevelData = item244C.data
                                              ,valueVariable    = 'count'
                                              ,columnVariable   = 'Category'
                                              ,rowVariable      = 'Heating_System'
                                              ,aggregateColumnName = "Remove")
item244C.summary <- item244C.summary[which(item244C.summary$Category != "Remove"),]

item244C.cast <- dcast(setDT(item244C.summary)
                       ,formula = Heating_System ~ Category
                       ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

names(item244C.cast)
item244C.table <- data.frame("Primary.Heating.System"= item244C.cast$Heating_System
                             ,"PSE.Percent"                 = item244C.cast$w.percent_PSE
                             ,"PSE.SE"                      = item244C.cast$w.SE_PSE
                             ,"PSE.n"                       = item244C.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item244C.cast$`w.percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item244C.cast$`w.SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item244C.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item244C.cast$`w.percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item244C.cast$`w.SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item244C.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item244C.cast$`w.percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item244C.cast$`w.SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"              = item244C.cast$`n_2017 RBSA PS`
                             ,"PSE_EB"                      = item244C.cast$EB_PSE
                             ,"PSE.King.County_EB"          = item244C.cast$`EB_PSE KING COUNTY`
                             ,"PSE.Non.King.County_EB"      = item244C.cast$`EB_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS_EB"             = item244C.cast$`EB_2017 RBSA PS`) 
exportTable(item244C.table, "MF", "Table 36C", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# unweighted Analysis
#######################
item244C.summary <- proportions_two_groups_unweighted(CustomerLevelData = item244C.data
                                                      ,valueVariable    = 'count'
                                                      ,columnVariable   = 'Category'
                                                      ,rowVariable      = 'Heating_System'
                                                      ,aggregateColumnName = "Remove")
item244C.summary <- item244C.summary[which(item244C.summary$Category != "Remove"),]

item244C.cast <- dcast(setDT(item244C.summary)
                       ,formula = Heating_System ~ Category
                       ,value.var = c("Percent", "SE", "Count", "n"))

item244C.table <- data.frame("Primary.Heating.System"= item244C.cast$Heating_System
                             ,"PSE.Percent"                 = item244C.cast$Percent_PSE
                             ,"PSE.SE"                      = item244C.cast$SE_PSE
                             ,"PSE.n"                       = item244C.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item244C.cast$`Percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item244C.cast$`SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item244C.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item244C.cast$`Percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item244C.cast$`SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item244C.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item244C.cast$`Percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item244C.cast$`SE_2017 RBSA PS`
                             ,"2017.RBSA.PS_n"              = item244C.cast$`n_2017 RBSA PS`) 

exportTable(item244C.table, "MF", "Table 36C", weighted = FALSE,OS = T, osIndicator = "PSE")











# #############################################################################################
# #Item 245: DISTRIBUTION OF SECONDARY HEATING SYSTEMS BY FUEL/ Table 37
# #############################################################################################
# #Read in data for analysis
# # Mechanical
# mechanical.dat <- read.xlsx(mechanical.export)
# mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))
# 
# mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
#                                                                         ,"CK_SiteID"
#                                                                         ,"System.Type"
#                                                                         ,"Heating.Fuel",
#                                                                         "Heat.Iteration",
#                                                                         "Cool.Iteration",
#                                                                         "Primary.Heating.System",
#                                                                         "Primary.Cooling.System"
#                                                                         ,"Serves.Common.Areas?"
#                                                                         ,"Central.for.Building"))]
# 
# mechanical.dat2  <- left_join(rbsa.dat.bldg, mechanical.dat1, by = c("CK_Building_ID" = "CK_SiteID"))
# length(unique(mechanical.dat2$CK_Cadmus_ID))
# names(mechanical.dat2)[which(names(mechanical.dat2) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"
# 
# #Subset to MF
# mechanical.dat.MF <- mechanical.dat2[grep("Multifamily", mechanical.dat2$BuildingType),]
# 
# item245.dat <- mechanical.dat.MF[which(mechanical.dat.MF$Heat.Iteration %notin% c("N/A","Unknown",NA)),]
# 
# #remove datapoint not asked for and repeated header lines
# item245.dat1 <- item245.dat[grep("BLDG",item245.dat$CK_Building_ID),]
# unique(item245.dat1$Primary.Heating.System)
# item245.dat1$Primary.Heating.System[which(item245.dat1$Primary.Heating.System == "Unknown")] <- "No"
# item245.dat2 <- item245.dat1[which(item245.dat1$Primary.Heating.System %in% c("Yes", "No")),]
# unique(item245.dat2$Primary.Heating.System)
# 
# item245.dat2$Heating.System.Ind <- item245.dat2$Primary.Heating.System
# item245.dat2$Heating.System.Ind[which(item245.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
# item245.dat2$Heating.System.Ind[which(item245.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"
# 
# unique(item245.dat2$Heating.Fuel)
# item245.dat2$Fuel <- item245.dat2$Heating.Fuel
# 
# unique(item245.dat2$System.Type[which(item245.dat2$Fuel %in% c("Other","Unknown","N/A",NA))])
# 
# item245.dat2$Fuel[grep("hp", item245.dat2$System.Type, ignore.case = T)] <- "Electric"
# item245.dat2$Fuel[grep("natural gas|gas", item245.dat2$Fuel, ignore.case = T)] <- "Natural Gas"
# item245.dat2$Fuel[grep("resistance", item245.dat2$Fuel, ignore.case = T)] <- "Electric"
# item245.dat2$Fuel[grep("N/A", item245.dat2$Fuel, ignore.case = T)] <- "Unknown"
# # item245.dat2$Fuel[is.na(item245.dat2$Fuel)] <- "Unknown"
# # item245.dat2$Fuel[grep("unknown", item245.dat2$Fuel, ignore.case = T)] <- "Electric"
# unique(item245.dat2$Fuel)
# 
# item245.dat2 <- item245.dat2[which(item245.dat2$Fuel != "Unknown"),]
# item245.dat2 <- item245.dat2[which(item245.dat2$System.Type != "Air Handler"),]
# unique(item245.dat2$Fuel)
# unique(item245.dat2$System.Type)
# 
# item245.dat2$System.Type[grep("Electric Baseboard|baseboard electric",item245.dat2$System.Type,ignore.case = T)] <- "Electric Baseboard"
# item245.dat2$System.Type[grep("zonal heat",item245.dat2$System.Type,ignore.case = T)] <- "Other Zonal Heat"
# item245.dat2$System.Type[grep("ductless|mini|packaged",item245.dat2$System.Type,ignore.case = T)] <- "PTHP/DPH"
# item245.dat2$System.Type[grep("furnace",item245.dat2$System.Type,ignore.case = T)] <- "Furnace"
# item245.dat2$System.Type[grep("boiler",item245.dat2$System.Type,ignore.case = T)] <- "Boiler"
# unique(item245.dat2$System.Type)
# 
# item245.dat3 <- unique(data.frame("CK_Cadmus_ID"      = item245.dat2$CK_Cadmus_ID
#                                   ,"Heating_System"    = item245.dat2$System.Type
#                                   ,"Fuel"              = item245.dat2$Fuel
#                                   ,"Primary_Secondary" = item245.dat2$Heating.System.Ind, stringsAsFactors = F))
# 
# item245.dat3 <- item245.dat3
# item245.dat4 <- item245.dat3[which(item245.dat3$Primary_Secondary == "Secondary Heating System"),]
# 
# item245.merge <- left_join(rbsa.dat.bldg, item245.dat4)
# item245.merge <- item245.merge[which(item245.merge$BuildingType == "Multifamily"),]
# item245.merge <- item245.merge[grep("BLDG",item245.merge$CK_Building_ID),]
# item245.merge$Heating_System[which(item245.merge$Primary_Secondary %in% c("N/A",NA))] <- "None"
# item245.merge$Fuel[which(item245.merge$Primary_Secondary %in% c("N/A",NA))] <- "None"
# unique(item245.merge$Heating_System)
# unique(item245.merge$Fuel)
# 
# item245.merge <- item245.merge[which(item245.merge$Category == "PSE"),]
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item245.data <- weightedData(item245.merge[-which(colnames(item245.merge) %in% c("Heating_System"
#                                                                                  ,"Fuel"
#                                                                                  ,"Primary_Secondary"
#                                                                                  ,"Category"))])
# item245.data <- left_join(item245.data, item245.merge[which(colnames(item245.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Heating_System"
#                                                                                            ,"Fuel"
#                                                                                            ,"Primary_Secondary"
#                                                                                            ,"Category"))])
# 
# item245.data$count <- 1
# 
# #######################
# # Weighted Analysis
# #######################
# item245.final <- proportionRowsAndColumns1(CustomerLevelData = item245.data
#                                            ,valueVariable    = 'count'
#                                            ,columnVariable   = 'Heating_System'
#                                            ,rowVariable      = 'Fuel'
#                                            ,aggregateColumnName = "All Systems")
# item245.final$Fuel[which(item245.final$Fuel == "Total")] <- "All Types"
# 
# item245.cast <- dcast(setDT(item245.final)
#                       ,formula = BuildingType + Heating_System ~ Fuel
#                       ,value.var = c("w.percent","w.SE", "count","n","N","EB"))
# names(item245.cast)
# 
# item245.table <- data.frame("Secondary.Heating.System" = item245.cast$Heating_System
#                             ,"Electric"              = item245.cast$w.percent_Electric
#                             ,"Electric.SE"           = item245.cast$w.SE_Electric
#                             ,"Natural.Gas"           = item245.cast$`w.percent_Natural Gas`
#                             ,"Natural.Gas.SE"        = item245.cast$`w.SE_Natural Gas`
#                             ,"Oil"                   = NA
#                             ,"Oil.SE"                = NA
#                             ,"Purchased.Steam"       = NA
#                             ,"Purchased.Steam.SE"    = NA
#                             ,"None"                  = item245.cast$w.percent_None
#                             ,"None.SE"               = item245.cast$w.SE_None
#                             ,"All.Types"             = item245.cast$`w.percent_All Types`
#                             ,"All.Types.SE"          = item245.cast$`w.SE_All Types`
#                             ,"n"                     = item245.cast$`n_All Types`
#                             ,"Electric.EB"           = item245.cast$EB_Electric
#                             ,"Natural.Gas.EB"        = item245.cast$`EB_Natural Gas`
#                             ,"Oil.EB"                = NA
#                             ,"Purchased.Steam.EB"    = NA
#                             ,"None.EB"               = item245.cast$EB_None
#                             ,"All.Types.EB"          = item245.cast$`EB_All Types`
# )
# 
# exportTable(item245.table, "MF", "Table 37", weighted = TRUE,OS = T, osIndicator = "PSE")
# 
# #######################
# # unweighted Analysis
# #######################
# item245.final <- proportions_two_groups_unweighted(CustomerLevelData = item245.data
#                                                    ,valueVariable    = 'count'
#                                                    ,columnVariable   = 'Heating_System'
#                                                    ,rowVariable      = 'Fuel'
#                                                    ,aggregateColumnName = "All Systems")
# item245.final$Fuel[which(item245.final$Fuel == "Total")] <- "All Types"
# 
# item245.cast <- dcast(setDT(item245.final)
#                       ,formula = Heating_System ~ Fuel
#                       ,value.var = c("Percent","SE", "Count","n"))
# 
# 
# item245.table <- data.frame("Secondary.Heating.System" = item245.cast$Heating_System
#                             ,"Electric"              = item245.cast$Percent_Electric
#                             ,"Electric.SE"           = item245.cast$SE_Electric
#                             ,"Natural.Gas"           = item245.cast$`Percent_Natural Gas`
#                             ,"Natural.Gas.SE"        = item245.cast$`SE_Natural Gas`
#                             ,"Oil"                   = NA
#                             ,"Oil.SE"                = NA
#                             ,"Purchased.Steam"       = NA
#                             ,"Purchased.Steam.SE"    = NA
#                             ,"None"                  = item245.cast$Percent_None
#                             ,"None.SE"               = item245.cast$SE_None
#                             ,"All.Types"             = item245.cast$`Percent_All Types`
#                             ,"All.Types.SE"          = item245.cast$`SE_All Types`
#                             ,"n"                     = item245.cast$`n_All Types`
# )
# 
# exportTable(item245.table, "MF", "Table 37", weighted = FALSE,OS = T, osIndicator = "PSE")
# 
# 
# 
# 
# 
# #############################################################################################
# #item246: DISTRIBUTION OF PRIMARY HEATING SYSTEMS BY BUILDING/ Table 38
# #############################################################################################
# item246.data <- item245.data
# 
# #######################
# # Weighted Analysis
# #######################
# item246.summary <- proportionRowsAndColumns1(CustomerLevelData = item246.data
#                                            ,valueVariable    = 'count'
#                                            ,columnVariable   = 'HomeType'
#                                            ,rowVariable      = 'Heating_System'
#                                            ,aggregateColumnName = "All Sizes")
# item246.summary <- item246.summary[which(item246.summary$HomeType != "All Sizes"),]
# 
# item246.all.sizes <- proportions_one_group(CustomerLevelData = item246.data
#                                            ,valueVariable = "count"
#                                            ,groupingVariable = "Heating_System"
#                                            ,total.name = "All Sizes"
#                                            ,columnName = "HomeType"
#                                            ,weighted = TRUE
#                                            ,two.prop.total = TRUE)
# 
# item246.final <- rbind.data.frame(item246.summary, item246.all.sizes, stringsAsFactors = F)
# 
# item246.cast <- dcast(setDT(item246.final)
#                       ,formula = Heating_System ~ HomeType
#                       ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
# 
# 
# item246.table <- data.frame("Secondary.Heating.System"= item246.cast$Heating_System
#                             ,"Low.Rise.1.3"      = item246.cast$`w.percent_Apartment Building (3 or fewer floors)`
#                             ,"Low.Rise.1.3.SE"   = item246.cast$`w.SE_Apartment Building (3 or fewer floors)`
#                             ,"Low.Rise.1.3.n"    = item246.cast$`n_Apartment Building (3 or fewer floors)`
#                             ,"Mid.Rise.4.6"      = item246.cast$`w.percent_Apartment Building (4 to 6 floors)`
#                             ,"Mid.Rise.4.6.SE"   = item246.cast$`w.SE_Apartment Building (4 to 6 floors)`
#                             ,"Mid.Rise.4.6.n"    = item246.cast$`n_Apartment Building (4 to 6 floors)`
#                             ,"High.Rise.gt7"     = item246.cast$`w.percent_Apartment Building (More than 6 floors)`
#                             ,"High.Rise.gt7.SE"  = item246.cast$`w.SE_Apartment Building (More than 6 floors)`
#                             ,"High.Rise.gt7.n"   = item246.cast$`n_Apartment Building (More than 6 floors)`
#                             ,"All.Sizes"         = item246.cast$`w.percent_All Sizes`
#                             ,"All.Sizes.SE"      = item246.cast$`w.SE_All Sizes`
#                             ,"All.Sizes.n"       = item246.cast$`n_All Sizes`
#                             ,"Low.Rise.1.3.EB"   = item246.cast$`EB_Apartment Building (3 or fewer floors)`
#                             ,"Mid.Rise.4.6.EB"   = item246.cast$`EB_Apartment Building (4 to 6 floors)`
#                             ,"High.Rise.gt7.EB"  = item246.cast$`EB_Apartment Building (More than 6 floors)`
#                             ,"All.Sizes.EB"      = item246.cast$`EB_All Sizes`
#                             ) 
# exportTable(item246.table, "MF", "Table 38", weighted = TRUE,OS = T, osIndicator = "PSE")
# 
# #######################
# # unweighted Analysis
# #######################
# item246.summary <- proportions_two_groups_unweighted(CustomerLevelData = item246.data
#                                              ,valueVariable    = 'count'
#                                              ,columnVariable   = 'HomeType'
#                                              ,rowVariable      = 'Heating_System'
#                                              ,aggregateColumnName = "All Sizes")
# item246.summary <- item246.summary[which(item246.summary$HomeType != "All Sizes"),]
# 
# item246.all.sizes <- proportions_one_group(CustomerLevelData = item246.data
#                                            ,valueVariable = "count"
#                                            ,groupingVariable = "Heating_System"
#                                            ,total.name = "All Sizes"
#                                            ,columnName = "HomeType"
#                                            ,weighted = FALSE
#                                            ,two.prop.total = TRUE)
# 
# item246.final <- rbind.data.frame(item246.summary, item246.all.sizes, stringsAsFactors = F)
# 
# item246.cast <- dcast(setDT(item246.final)
#                       ,formula = Heating_System ~ HomeType
#                       ,value.var = c("Percent", "SE", "Count", "n"))
# 
# item246.table <- data.frame("Secondary.Heating.System"= item246.cast$Heating_System
#                             ,"Low.Rise.1.3"      = item246.cast$`Percent_Apartment Building (3 or fewer floors)`
#                             ,"Low.Rise.1.3.SE"   = item246.cast$`SE_Apartment Building (3 or fewer floors)`
#                             ,"Low.Rise.1.3.n"    = item246.cast$`n_Apartment Building (3 or fewer floors)`
#                             ,"Mid.Rise.4.6"      = item246.cast$`Percent_Apartment Building (4 to 6 floors)`
#                             ,"Mid.Rise.4.6.SE"   = item246.cast$`SE_Apartment Building (4 to 6 floors)`
#                             ,"Mid.Rise.4.6.n"    = item246.cast$`n_Apartment Building (4 to 6 floors)`
#                             ,"High.Rise.gt7"     = item246.cast$`Percent_Apartment Building (More than 6 floors)`
#                             ,"High.Rise.gt7.SE"  = item246.cast$`SE_Apartment Building (More than 6 floors)`
#                             ,"High.Rise.gt7.n"   = item246.cast$`n_Apartment Building (More than 6 floors)`
#                             ,"All.Sizes"         = item246.cast$`Percent_All Sizes`
#                             ,"All.Sizes.SE"      = item246.cast$`SE_All Sizes`
#                             ,"All.Sizes.n"       = item246.cast$`n_All Sizes`) 
# 
# exportTable(item246.table, "MF", "Table 38", weighted = FALSE,OS = T, osIndicator = "PSE")
# 






#############################################################################################
#Item 249: DISTRIBUTION OF COMMON AREA COOLING SYSTEMS / Table 41
#############################################################################################
one.line.bldg.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), sheet = "Building One Line Summary", startRow = 3)
one.line.bldg.dat <- one.line.bldg.dat[which(one.line.bldg.dat$Area.of.Conditioned.Common.Space > 0),]
one.line.bldg.dat$CK_Building_ID <- one.line.bldg.dat$PK_BuildingID

one.line.bldg.dat <- one.line.bldg.dat[names(one.line.bldg.dat) %in% c("CK_Building_ID", "Area.of.Conditioned.Common.Space")]

rbsa.merge <- left_join(rbsa.dat.bldg, one.line.bldg.dat)
rbsa.merge <- rbsa.merge[which(!is.na(rbsa.merge$Area.of.Conditioned.Common.Space)),]

item249.dat <- unique(mechanical.dat.MF[which(colnames(mechanical.dat.MF) %in% c("CK_Cadmus_ID",
                                                                                 "Cool.Iteration",
                                                                                 "CK_Building_ID",
                                                                                 "System.Type"
                                                                                 ,"Primary.Cooling.System"))])
item249.dat <- item249.dat[grep("bldg", item249.dat$Cool.Iteration, ignore.case = T),]

item249.dat$CoolingInd <- 1

unique(item249.dat$System.Type)
item249.dat$System.Type[grep("central", item249.dat$System.Type, ignore.case = T)] <- "Central Ac"

item249.dat1 <- data.frame(summarise(group_by(item249.dat,CK_Cadmus_ID,System.Type),
                                     CoolingInd = sum(unique(CoolingInd), na.rm = T)), stringsAsFactors = F)

item249.dat1 <- left_join(rbsa.merge, item249.dat1)
item249.dat1 <- item249.dat1[grep("3 or fewer floors", item249.dat1$BuildingTypeXX, ignore.case = T),]

item249.dat1$CoolingInd[which(is.na(item249.dat1$CoolingInd))] <- 0
item249.dat1$System.Type[which(item249.dat1$CoolingInd == 0)] <- "No Cooling"
item249.dat1 <- item249.dat1[which(item249.dat1$Category == "PSE"),]
################################################
# Adding pop and sample sizes for weights
################################################
item249.data <- weightedData(item249.dat1[-which(colnames(item249.dat1) %in% c("System.Type"
                                                                               ,"Area.of.Conditioned.Common.Space"
                                                                               ,"CoolingInd"
                                                                               ,"Category"))])
item249.data <- left_join(item249.data, item249.dat1[which(colnames(item249.dat1) %in% c("CK_Cadmus_ID"
                                                                                         ,"System.Type"
                                                                                         ,"Area.of.Conditioned.Common.Space"
                                                                                         ,"CoolingInd"
                                                                                         ,"Category"))])

item249.data$count <- 1

#######################
# Weighted Analysis
#######################
item249.final <- proportions_one_group(CustomerLevelData = item249.data
                                       ,valueVariable = 'count'
                                       ,groupingVariable = 'System.Type'
                                       ,total.name = "All Systems"
)
item249.final.MF <- item249.final[which(colnames(item249.final) %notin% c("BuildingType"))]

exportTable(item249.final.MF, "MF", "Table 41", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# unweighted Analysis
#######################
item249.final <- proportions_one_group(CustomerLevelData = item249.data
                                       ,valueVariable = 'count'
                                       ,groupingVariable = 'System.Type'
                                       ,total.name = "All Systems"
                                       ,weighted = FALSE
)
item249.final.MF <- item249.final[which(colnames(item249.final) %notin% c("BuildingType"))]

exportTable(item249.final.MF, "MF", "Table 41", weighted = FALSE,OS = T, osIndicator = "PSE")
