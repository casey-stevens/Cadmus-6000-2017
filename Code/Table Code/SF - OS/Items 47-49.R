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

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")

"%notin%" <- Negate("%in%")

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
# Mechanical
# download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx', mechanical.export, mode = 'wb')
# mechanical.dat <- read.xlsx(mechanical.export)
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

#subset to columns needed for the analysis of items 47,48,49
mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"System.Sub-Type"
                                                                        ,"Heating.Fuel"))]
mechanical.dat1$Heating.Fuel[which(mechanical.dat1$Heating.Fuel == "Natural gas")] <- "Natural Gas"




# #############################################################################################
# #Item 47: DISTRIBUTION OF FUEL CHOICE, FORCED AIR FURNACES (SF table 54, MH table 36)
# #############################################################################################
# 
# item47.dat <- mechanical.dat1
# 
# item47.dat1 <- item47.dat[which(item47.dat$Generic == "Furnace"),]
# item47.dat1 <- unique(item47.dat1)
# item47.dat2 <- left_join(rbsa.dat, item47.dat1, by = "CK_Cadmus_ID")
# item47.dat3 <- item47.dat2[which(item47.dat2$Generic %notin% c("N/A",NA)),]
# 
# unique(item47.dat3$Heating.Fuel)
# item47.dat3$Heating.Fuel[grep("gas", item47.dat3$Heating.Fuel, ignore.case = T)] <- "Gas"
# unique(item47.dat3$Heating.Fuel)
# 
# item47.dat4 <- item47.dat3[which(item47.dat3$Heating.Fuel %notin% c("Other"
#                                                                     ,"Unknown"
#                                                                     ,"Can't Determine"
#                                                                     ,NA
#                                                                     ,"Wood (cord)"
#                                                                     ,"N/A")),]
# unique(item47.dat4$Heating.Fuel)
# 
# item47.dat4$count <- 1
# 
# # Weighting function
# item47.data <- weightedData(item47.dat4[-which(colnames(item47.dat4) %in% c("Generic"
#                                                                             ,"System.Sub-Type"
#                                                                             ,"Heating.Fuel"
#                                                                             ,"count"))])
# item47.data <- left_join(item47.data, item47.dat4[which(colnames(item47.dat4) %in% c("CK_Cadmus_ID"
#                                                                                      ,"Generic"
#                                                                                      ,"System.Sub-Type"
#                                                                                      ,"Heating.Fuel"
#                                                                                      ,"count"))])
# 
# ################################
# # Weighted Analysis
# ################################
# item47.final <- proportions_one_group(CustomerLevelData  = item47.data
#                                       , valueVariable    = 'count'
#                                       , groupingVariable = 'Heating.Fuel'
#                                       , total.name       = "Total"
#                                       , weighted = TRUE)
# 
# item47.final.SF <- item47.final[which(item47.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item47.final) %in% c("BuildingType"))]
# item47.final.MH <- item47.final[which(item47.final$BuildingType == "Manufactured")
#                                 ,-which(colnames(item47.final) %in% c("BuildingType"))]
# 
# exportTable(item47.final.SF, "SF", "Table 54", weighted = TRUE)
# # exportTable(item47.final.MH, "MH", "Table 36", weighted = TRUE)
# 
# 
# ################################
# # Unweighted Analysis
# ################################
# item47.final <- proportions_one_group(CustomerLevelData  = item47.data
#                                       , valueVariable    = 'count'
#                                       , groupingVariable = 'Heating.Fuel'
#                                       , total.name       = "Total"
#                                       , weighted = FALSE)
# 
# item47.final.SF <- item47.final[which(item47.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item47.final) %in% c("BuildingType"))]
# item47.final.MH <- item47.final[which(item47.final$BuildingType == "Manufactured")
#                                 ,-which(colnames(item47.final) %in% c("BuildingType"))]
# 
# exportTable(item47.final.SF, "SF", "Table 54", weighted = FALSE)
# # exportTable(item47.final.MH, "MH", "Table 36", weighted = FALSE)
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 48: DISTRIBUTION OF FUEL CHOICE, BOILERS (SF table 55)
# #############################################################################################
# 
# item48.dat <- mechanical.dat1
# 
# unique(item48.dat$Generic)
# item48.dat1 <- item48.dat[which(item48.dat$Generic == "Boiler"),]
# 
# item48.dat2 <- left_join(item48.dat1, rbsa.dat, by = "CK_Cadmus_ID")
# item48.dat2$count <- 1
# 
# item48.dat3 <- item48.dat2[which(item48.dat2$BuildingType == "Single Family"),]
# item48.dat4 <- item48.dat3[which(item48.dat3$Heating.Fuel %notin% c("N/A",NA)),]
# 
# # Weighting function
# item48.data <- weightedData(item48.dat4[-which(colnames(item48.dat4) %in% c("Generic"
#                                                                             ,"System.Sub-Type"
#                                                                             ,"Heating.Fuel"
#                                                                             ,"count"))])
# item48.data <- left_join(item48.data, item48.dat4[which(colnames(item48.dat4) %in% c("CK_Cadmus_ID"
#                                                                                      ,"Generic"
#                                                                                      ,"System.Sub-Type"
#                                                                                      ,"Heating.Fuel"
#                                                                                      ,"count"))])
# 
# ################################
# # Weighted Analysis
# ################################
# item48.final <- proportions_one_group(CustomerLevelData  = item48.data
#                                       , valueVariable    = 'count'
#                                       , groupingVariable = 'Heating.Fuel'
#                                       , total.name       = "Total"
#                                       , weighted = TRUE)
# 
# 
# item48.final.SF <- item48.final[which(item48.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item48.final) %in% c("BuildingType"))]
# 
# exportTable(item48.final.SF, "SF", "Table 55", weighted = TRUE)
# 
# ################################
# # Unweighted Analysis
# ################################
# item48.final <- proportions_one_group(CustomerLevelData  = item48.data
#                                       , valueVariable    = 'count'
#                                       , groupingVariable = 'Heating.Fuel'
#                                       , total.name       = "Total"
#                                       , weighted = FALSE)
# 
# 
# item48.final.SF <- item48.final[which(item48.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item48.final) %in% c("BuildingType"))]
# 
# exportTable(item48.final.SF, "SF", "Table 55", weighted = FALSE)
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 49: DISTRIBUTION OF FUEL CHOICE, COMBUSTION HEATING STOVES (SF table 56, MH table 37)
# #############################################################################################
# 
# item49.dat <- mechanical.dat1
# 
# unique(item49.dat$`System.Sub-Type`)
# item49.dat1 <- item49.dat[which(item49.dat$`System.Sub-Type` == "Space Heating Stove"),]
# # item49.dat1 <- unique(item49.dat1)
# item49.dat2 <- left_join(item49.dat1, rbsa.dat, by = "CK_Cadmus_ID")
# item49.dat2$count <- 1
# 
# item49.dat3 <- item49.dat2[which(item49.dat2$BuildingType %in% c("Single Family", "Manufactured")),]
# 
# item49.dat4 <- item49.dat3[which(item49.dat3$Heating.Fuel != "Electric"),]
# item49.dat4$Heating.Fuel[grep("cord",item49.dat4$Heating.Fuel, ignore.case = T)] <- "Wood"
# item49.dat4$Heating.Fuel[grep("pellets",item49.dat4$Heating.Fuel, ignore.case = T)] <- "Pellets"
# item49.dat4$Heating.Fuel[grep("gas",item49.dat4$Heating.Fuel, ignore.case = T)] <- "Gas"
# unique(item49.dat4$Heating.Fuel)
# item49.dat4 <- item49.dat4[which(item49.dat4$Heating.Fuel %notin% c("N/A",NA)),]
# # Weighting function
# item49.data <- weightedData(item49.dat4[-which(colnames(item49.dat4) %in% c("Generic"
#                                                                             ,"System.Sub-Type"
#                                                                             ,"Heating.Fuel"
#                                                                             ,"count"))])
# item49.data <- left_join(item49.data, item49.dat4[which(colnames(item49.dat4) %in% c("CK_Cadmus_ID"
#                                                                                      ,"Generic"
#                                                                                      ,"System.Sub-Type"
#                                                                                      ,"Heating.Fuel"
#                                                                                      ,"count"))])
# 
# ################################
# # Weighted Analysis
# ################################
# item49.final <- proportions_one_group(CustomerLevelData  = item49.data
#                                       , valueVariable    = 'count'
#                                       , groupingVariable = 'Heating.Fuel'
#                                       , total.name       = "Total"
#                                       , weighted = TRUE)
# 
# item49.final.SF <- item49.final[which(item49.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item49.final) %in% c("BuildingType"))]
# item49.final.MH <- item49.final[which(item49.final$BuildingType == "Manufactured")
#                                 ,-which(colnames(item49.final) %in% c("BuildingType"))]
# 
# exportTable(item49.final.SF, "SF", "Table 56", weighted = TRUE)
# # exportTable(item49.final.MH, "MH", "Table 37", weighted = TRUE)
# 
# ################################
# # Unweighted Analysis
# ################################
# item49.final <- proportions_one_group(CustomerLevelData  = item49.data
#                                       , valueVariable    = 'count'
#                                       , groupingVariable = 'Heating.Fuel'
#                                       , total.name       = "Total"
#                                       , weighted = FALSE)
# 
# item49.final.SF <- item49.final[which(item49.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item49.final) %in% c("BuildingType"))]
# item49.final.MH <- item49.final[which(item49.final$BuildingType == "Manufactured")
#                                 ,-which(colnames(item49.final) %in% c("BuildingType"))]
# 
# exportTable(item49.final.SF, "SF", "Table 56", weighted = FALSE)
# # exportTable(item49.final.MH, "MH", "Table 37", weighted = FALSE)






















############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]

#############################################################################################
#Item 47: DISTRIBUTION OF FUEL CHOICE, FORCED AIR FURNACES (SF table 54, MH table 36)
#############################################################################################

item47.os.dat <- mechanical.dat1

item47.os.dat1 <- item47.os.dat[which(item47.os.dat$Generic == "Furnace"),]
item47.os.dat1 <- unique(item47.os.dat1)
item47.os.dat2 <- left_join(os.dat, item47.os.dat1, by = "CK_Cadmus_ID")
item47.os.dat3 <- item47.os.dat2[which(item47.os.dat2$Generic %notin% c("N/A",NA)),]

unique(item47.os.dat3$Heating.Fuel)
item47.os.dat3$Heating.Fuel[grep("gas", item47.os.dat3$Heating.Fuel, ignore.case = T)] <- "Gas"
unique(item47.os.dat3$Heating.Fuel)

item47.os.dat4 <- item47.os.dat3[which(item47.os.dat3$Heating.Fuel %notin% c("Other"
                                                                    ,"Unknown"
                                                                    ,"Can't Determine"
                                                                    ,NA
                                                                    ,"Wood (cord)"
                                                                    ,"N/A")),]
unique(item47.os.dat4$Heating.Fuel)

item47.os.dat4$count <- 1

# Weighting function
item47.os.data <- weightedData(item47.os.dat4[-which(colnames(item47.os.dat4) %in% c("Generic"
                                                                            ,"System.Sub-Type"
                                                                            ,"Heating.Fuel"
                                                                            ,"count"))])
item47.os.data <- left_join(item47.os.data, unique(item47.os.dat4[which(colnames(item47.os.dat4) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"System.Sub-Type"
                                                                                     ,"Heating.Fuel"
                                                                                     ,"count"))]))

################################
# Weighted Analysis
################################
item47.os.final <- proportionRowsAndColumns1(CustomerLevelData  = item47.os.data
                                             , valueVariable    = 'count'
                                             ,columnVariable = "CK_Building_ID"
                                             , rowVariable   = 'Heating.Fuel'
                                             ,aggregateColumnName = "Remove")

item47.os.cast <- dcast(setDT(item47.os.final)
                        , formula = Heating.Fuel ~ CK_Building_ID
                        , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item47.os.cast)

if(os.ind == "scl"){
  item47.os.table <- data.frame("Heating.Fuel"          = item47.os.cast$Heating.Fuel
                                ,"Percent_SCL.GenPop"   = item47.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item47.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item47.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item47.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item47.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item47.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item47.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item47.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item47.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item47.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item47.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item47.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item47.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item47.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item47.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item47.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item47.os.table <- data.frame("Heating.Fuel"          = item47.os.cast$Heating.Fuel
                                ,"Percent_SnoPUD"          = item47.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item47.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item47.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item47.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item47.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item47.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item47.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item47.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item47.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item47.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item47.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item47.os.cast$`EB_2017 RBSA NW`)
}




exportTable(item47.os.table, "SF", "Table 54", weighted = TRUE, osIndicator = export.ind, OS = T)


################################
# Unweighted Analysis
################################
item47.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item47.os.data
                                                     , valueVariable       = 'count'
                                                     , columnVariable      = 'CK_Building_ID'
                                                     , rowVariable         = 'Heating.Fuel'
                                                     , aggregateColumnName = 'Region')

item47.os.cast <- dcast(setDT(item47.os.final)
                        , formula = Heating.Fuel ~ CK_Building_ID
                        , value.var = c("Percent", "SE", "Count", "n"))
names(item47.os.cast)

if(os.ind == "scl"){
  item47.os.table <- data.frame("Heating.Fuel"          = item47.os.cast$Heating.Fuel
                                ,"Percent_SCL.GenPop"   = item47.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item47.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item47.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item47.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item47.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item47.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item47.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item47.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item47.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item47.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item47.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item47.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item47.os.table <- data.frame("Heating.Fuel"          = item47.os.cast$Heating.Fuel
                                ,"Percent_SnoPUD"          = item47.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item47.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item47.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item47.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item47.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item47.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item47.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item47.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item47.os.cast$`n_2017 RBSA NW`)
}



exportTable(item47.os.table, "SF", "Table 54", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
#Item 48: DISTRIBUTION OF FUEL CHOICE, BOILERS (SF table 55)
#############################################################################################
item48.os.dat <- mechanical.dat1

unique(item48.os.dat$Generic)
item48.os.dat1 <- item48.os.dat[which(item48.os.dat$Generic == "Boiler"),]

item48.os.dat2 <- left_join(item48.os.dat1, os.dat, by = "CK_Cadmus_ID")
item48.os.dat2$count <- 1

item48.os.dat3 <- item48.os.dat2[which(item48.os.dat2$BuildingType == "Single Family"),]
item48.os.dat4 <- item48.os.dat3[which(item48.os.dat3$Heating.Fuel %notin% c("N/A",NA)),]

# Weighting function
item48.os.data <- weightedData(item48.os.dat4[-which(colnames(item48.os.dat4) %in% c("Generic"
                                                                            ,"System.Sub-Type"
                                                                            ,"Heating.Fuel"
                                                                            ,"count"))])
item48.os.data <- left_join(item48.os.data, unique(item48.os.dat4[which(colnames(item48.os.dat4) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"System.Sub-Type"
                                                                                     ,"Heating.Fuel"
                                                                                     ,"count"))]))

################################
# Weighted Analysis
################################
item48.os.final <- proportionRowsAndColumns1(CustomerLevelData  = item48.os.data
                                             , valueVariable    = 'count'
                                             ,columnVariable = "CK_Building_ID"
                                             , rowVariable   = 'Heating.Fuel'
                                             ,aggregateColumnName = "Remove")

item48.os.cast <- dcast(setDT(item48.os.final)
                        , formula = Heating.Fuel ~ CK_Building_ID
                        , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item48.os.cast)
if(os.ind == "scl"){
  item48.os.table <- data.frame("Heating.Fuel"          = item48.os.cast$Heating.Fuel
                                ,"Percent_SCL.GenPop"   = item48.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item48.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item48.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item48.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item48.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item48.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item48.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item48.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item48.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item48.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item48.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item48.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item48.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item48.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item48.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item48.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item48.os.table <- data.frame("Heating.Fuel"          = item48.os.cast$Heating.Fuel
                                ,"Percent_SnoPUD"          = item48.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item48.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item48.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item48.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item48.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item48.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item48.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item48.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item48.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item48.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item48.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item48.os.cast$`EB_2017 RBSA NW`)
}



exportTable(item48.os.table, "SF", "Table 55", weighted = TRUE, osIndicator = export.ind, OS = T)

################################
# Unweighted Analysis
################################
item48.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item48.os.data
                                                     , valueVariable       = 'count'
                                                     , columnVariable      = 'CK_Building_ID'
                                                     , rowVariable         = 'Heating.Fuel'
                                                     , aggregateColumnName = 'Region')

item48.os.cast <- dcast(setDT(item48.os.final)
                        , formula = Heating.Fuel ~ CK_Building_ID
                        , value.var = c("Percent", "SE", "Count", "n"))
names(item48.os.cast)
if(os.ind == "scl"){
  item48.os.table <- data.frame("Heating.Fuel"          = item48.os.cast$Heating.Fuel
                                ,"Percent_SCL.GenPop"   = item48.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item48.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item48.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item48.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item48.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item48.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item48.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item48.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item48.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item48.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item48.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item48.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item48.os.table <- data.frame("Heating.Fuel"          = item48.os.cast$Heating.Fuel
                                ,"Percent_SnoPUD"          = item48.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item48.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item48.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item48.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item48.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item48.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item48.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item48.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item48.os.cast$`n_2017 RBSA NW`)
}



exportTable(item48.os.table, "SF", "Table 55", weighted = FALSE, osIndicator = export.ind, OS = T)






#############################################################################################
#Item 49: DISTRIBUTION OF FUEL CHOICE, COMBUSTION HEATING STOVES (SF table 56, MH table 37)
#############################################################################################
item49.os.dat <- mechanical.dat1

unique(item49.os.dat$`System.Sub-Type`)
item49.os.dat1 <- item49.os.dat[which(item49.os.dat$`System.Sub-Type` == "Space Heating Stove"),]
# item49.os.dat1 <- unique(item49.os.dat1)
item49.os.dat2 <- left_join(item49.os.dat1, os.dat, by = "CK_Cadmus_ID")
item49.os.dat2$count <- 1

item49.os.dat3 <- item49.os.dat2[which(item49.os.dat2$BuildingType %in% c("Single Family", "Manufactured")),]

item49.os.dat4 <- item49.os.dat3[which(item49.os.dat3$Heating.Fuel != "Electric"),]
item49.os.dat4$Heating.Fuel[grep("cord",item49.os.dat4$Heating.Fuel, ignore.case = T)] <- "Wood"
item49.os.dat4$Heating.Fuel[grep("pellets",item49.os.dat4$Heating.Fuel, ignore.case = T)] <- "Pellets"
item49.os.dat4$Heating.Fuel[grep("gas",item49.os.dat4$Heating.Fuel, ignore.case = T)] <- "Gas"
unique(item49.os.dat4$Heating.Fuel)
item49.os.dat4 <- item49.os.dat4[which(item49.os.dat4$Heating.Fuel %notin% c("N/A",NA)),]
# Weighting function
item49.os.data <- weightedData(item49.os.dat4[-which(colnames(item49.os.dat4) %in% c("Generic"
                                                                            ,"System.Sub-Type"
                                                                            ,"Heating.Fuel"
                                                                            ,"count"))])
item49.os.data <- left_join(item49.os.data, item49.os.dat4[which(colnames(item49.os.dat4) %in% c("CK_Cadmus_ID"
                                                                                                 ,'CK_Building_ID'
                                                                                                 ,"Generic"
                                                                                                 ,"System.Sub-Type"
                                                                                                 ,"Heating.Fuel"
                                                                                                 ,"count"))])

################################
# Weighted Analysis
################################
item49.os.final <- proportionRowsAndColumns1(CustomerLevelData  = item49.os.data
                                             , valueVariable    = 'count'
                                             ,columnVariable = "CK_Building_ID"
                                             , rowVariable   = 'Heating.Fuel'
                                             ,aggregateColumnName = "Remove")

item49.os.cast <- dcast(setDT(item49.os.final)
                        , formula = Heating.Fuel ~ CK_Building_ID
                        , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item49.os.cast)
if(os.ind == "scl"){
  item49.os.table <- data.frame("Heating.Fuel"          = item49.os.cast$Heating.Fuel
                                ,"Percent_SCL.GenPop"   = item49.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item49.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item49.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item49.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item49.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item49.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item49.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item49.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item49.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item49.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item49.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item49.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item49.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item49.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item49.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item49.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item49.os.table <- data.frame("Heating.Fuel"          = item49.os.cast$Heating.Fuel
                                ,"Percent_SnoPUD"          = item49.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item49.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item49.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item49.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item49.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item49.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item49.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item49.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item49.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item49.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item49.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item49.os.cast$`EB_2017 RBSA NW`)
}




exportTable(item49.os.table, "SF", "Table 56", weighted = TRUE, osIndicator = export.ind, OS = T)

################################
# Unweighted Analysis
################################
item49.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item49.os.data
                                                     , valueVariable       = 'count'
                                                     , columnVariable      = 'CK_Building_ID'
                                                     , rowVariable         = 'Heating.Fuel'
                                                     , aggregateColumnName = 'Region')

item49.os.cast <- dcast(setDT(item49.os.final)
                        , formula = Heating.Fuel ~ CK_Building_ID
                        , value.var = c("Percent", "SE", "Count", "n"))
names(item49.os.cast)
if(os.ind == "scl"){
  item49.os.table <- data.frame("Heating.Fuel"          = item49.os.cast$Heating.Fuel
                                ,"Percent_SCL.GenPop"   = item49.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item49.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item49.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item49.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item49.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item49.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item49.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item49.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item49.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item49.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item49.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item49.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item49.os.table <- data.frame("Heating.Fuel"          = item49.os.cast$Heating.Fuel
                                ,"Percent_SnoPUD"          = item49.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item49.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item49.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item49.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item49.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item49.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item49.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item49.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item49.os.cast$`n_2017 RBSA NW`)
}



exportTable(item49.os.table, "SF", "Table 56", weighted = FALSE, osIndicator = export.ind, OS = T)
