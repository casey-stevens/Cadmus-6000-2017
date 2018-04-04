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
# appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))


# #############################################################################################
# #Item 94: DISTRIBUTION OF COOK TOP FUEL BY TYPE (SF table 101, MH table 82)
# #############################################################################################
# #subset to columns needed for analysis
# item94.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
#                                                                    ,"Type"
#                                                                    ,"Stove.Fuel"
#                                                                    ,""))]
# item94.dat$count <- 1
# 
# item94.dat0 <- item94.dat[which(item94.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item94.dat1 <- left_join(rbsa.dat, item94.dat0, by = "CK_Cadmus_ID")
# item94.dat2 <- item94.dat1[which(item94.dat1$Type == "Stove/Oven"),]
# unique(item94.dat2$Stove.Fuel)
# item94.dat2$Stove.Fuel[which(item94.dat2$Stove.Fuel %in% c("Gas", "gas", "natural gas", "Natural Gas"))] <- "Gas"
# item94.dat3 <- item94.dat2[which(item94.dat2$Stove.Fuel %notin% c("No oven ", "No oven", "No Oven", "No Cooktop", "No Stove", NA)),]
# item94.dat3$Stove.Fuel[which(item94.dat3$Stove.Fuel %notin% c("Electric", "Gas", "Propane"))] <- "Other"
# unique(item94.dat3$Stove.Fuel)
# 
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item94.data <- weightedData(item94.dat3[-which(colnames(item94.dat3) %in% c("count"
#                                                                               ,"Type"
#                                                                               ,"Stove.Fuel"))])
# item94.data <- left_join(item94.data, item94.dat3[which(colnames(item94.dat3) %in% c("CK_Cadmus_ID"
#                                                                                        ,"count"
#                                                                                        ,"Type"
#                                                                                        ,"Stove.Fuel"))])
# item94.data$count <- 1
# 
# #######################
# # Weighted Analysis
# #######################
# item94.final <- proportions_one_group(CustomerLevelData = item94.data
#                                       ,valueVariable    = 'count'
#                                       ,groupingVariable = 'Stove.Fuel'
#                                       ,total.name       = 'Total')
# 
# 
# unique(item94.final$Stove.Fuel)
# rowOrder <- c("Electric"
#               ,"Gas"
#               ,"Propane"
#               ,"Other"
#               ,"Total")
# item94.final <- item94.final %>% mutate(Stove.Fuel = factor(Stove.Fuel, levels = rowOrder)) %>% arrange(Stove.Fuel)  
# item94.final <- data.frame(item94.final)
# 
# 
# item94.final.SF <- item94.final[which(item94.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item94.final) %in% c("BuildingType"))]
# item94.final.MH <- item94.final[which(item94.final$BuildingType == "Manufactured")
#                                 ,-which(colnames(item94.final) %in% c("BuildingType"))]
# 
# exportTable(item94.final.SF, "SF", "Table 101", weighted = TRUE)
# # exportTable(item94.final.MH, "MH", "Table 82", weighted = TRUE)
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# item94.final <- proportions_one_group(CustomerLevelData = item94.data
#                                       ,valueVariable    = 'count'
#                                       ,groupingVariable = 'Stove.Fuel'
#                                       ,total.name       = 'Total'
#                                       ,weighted         = FALSE)
# 
# 
# unique(item94.final$Stove.Fuel)
# rowOrder <- c("Electric"
#               ,"Gas"
#               ,"Propane"
#               ,"Other"
#               ,"Total")
# item94.final <- item94.final %>% mutate(Stove.Fuel = factor(Stove.Fuel, levels = rowOrder)) %>% arrange(Stove.Fuel)  
# item94.final <- data.frame(item94.final)
# 
# 
# item94.final.SF <- item94.final[which(item94.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item94.final) %in% c("BuildingType"))]
# item94.final.MH <- item94.final[which(item94.final$BuildingType == "Manufactured")
#                                 ,-which(colnames(item94.final) %in% c("BuildingType"))]
# 
# exportTable(item94.final.SF, "SF", "Table 101", weighted = FALSE)
# # exportTable(item94.final.MH, "MH", "Table 82", weighted = FALSE)
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 95: DISTRIBUTION OF STOVE FUEL BY TYPE (SF table 102, MH table 83)
# #############################################################################################
# #subset to columns needed for analysis
# item95.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
#                                                                    ,"Type"
#                                                                    ,"Oven.Fuel"
#                                                                    ,""))]
# item95.dat$count <- 1
# 
# item95.dat0 <- item95.dat[which(item95.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item95.dat1 <- left_join(rbsa.dat, item95.dat0, by = "CK_Cadmus_ID")
# item95.dat2 <- item95.dat1[which(item95.dat1$Type == "Stove/Oven"),]
# unique(item95.dat2$Oven.Fuel)
# item95.dat2$Oven.Fuel[which(item95.dat2$Oven.Fuel %in% c("Gas", "gas", "natural gas", "Natural Gas"))] <- "Gas"
# item95.dat3 <- item95.dat2[which(item95.dat2$Oven.Fuel %notin% c("No oven ", "No oven", "No Oven", "No Cooktop", "No Stove", NA)),]
# unique(item95.dat3$Oven.Fuel)
# 
# item95.dat3$Oven.Fuel[which(item95.dat3$Oven.Fuel %notin% c("Electric", "Gas", "Propane"))] <- "Other"
# unique(item95.dat3$Oven.Fuel)
# 
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item95.data <- weightedData(item95.dat3[-which(colnames(item95.dat3) %in% c("count"
#                                                                             ,"Type"
#                                                                             ,"Oven.Fuel"))])
# item95.data <- left_join(item95.data, item95.dat3[which(colnames(item95.dat3) %in% c("CK_Cadmus_ID"
#                                                                                      ,"count"
#                                                                                      ,"Type"
#                                                                                      ,"Oven.Fuel"))])
# item95.data$count <- 1
# 
# #######################
# # Weighted Analysis
# #######################
# item95.final <- proportions_one_group(CustomerLevelData = item95.data
#                                       ,valueVariable    = 'count'
#                                       ,groupingVariable = 'Oven.Fuel'
#                                       ,total.name       = 'Total')
# 
# item95.final.SF <- item95.final[which(item95.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item95.final) %in% c("BuildingType"))]
# item95.final.MH <- item95.final[which(item95.final$BuildingType == "Manufactured")
#                                 ,-which(colnames(item95.final) %in% c("BuildingType"))]
# 
# exportTable(item95.final.SF, "SF", "Table 102", weighted = TRUE)
# # exportTable(item95.final.MH, "MH", "Table 83", weighted = TRUE)
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# item95.final <- proportions_one_group(CustomerLevelData = item95.data
#                                       ,valueVariable    = 'count'
#                                       ,groupingVariable = 'Oven.Fuel'
#                                       ,total.name       = 'Total'
#                                       ,weighted         = FALSE)
# 
# item95.final.SF <- item95.final[which(item95.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item95.final) %in% c("BuildingType"))]
# item95.final.MH <- item95.final[which(item95.final$BuildingType == "Manufactured")
#                                 ,-which(colnames(item95.final) %in% c("BuildingType"))]
# 
# exportTable(item95.final.SF, "SF", "Table 102", weighted = FALSE)
# # exportTable(item95.final.MH, "MH", "Table 83", weighted = FALSE)





























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
names(os.dat)

#############################################################################################
#Item 94: DISTRIBUTION OF COOK TOP FUEL BY TYPE (SF table 101, MH table 82)
#############################################################################################
#subset to columns needed for analysis
item94.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Stove.Fuel"))]
item94.os.dat$count <- 1

item94.os.dat0 <- item94.os.dat[which(item94.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item94.os.dat1 <- left_join(os.dat, item94.os.dat0, by = "CK_Cadmus_ID")
item94.os.dat2 <- item94.os.dat1[which(item94.os.dat1$Type == "Stove/Oven"),]

item94.os.dat3 <- item94.os.dat2[which(item94.os.dat2$Stove.Fuel %notin% c("No Stove", "No Cooktop", NA)),]
item94.os.dat3$Stove.Fuel[which(item94.os.dat3$Stove.Fuel %notin% c("Electric", "Gas", "Propane"))] <- "Other"


################################################
# Adding pop and sample sizes for weights
################################################
item94.os.data <- weightedData(item94.os.dat3[-which(colnames(item94.os.dat3) %in% c("count"
                                                                            ,"Type"
                                                                            ,"Stove.Fuel"))])
item94.os.data <- left_join(item94.os.data, unique(item94.os.dat3[which(colnames(item94.os.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"count"
                                                                                     ,"Type"
                                                                                     ,"Stove.Fuel"))]))
item94.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item94.os.final <- proportionRowsAndColumns1(CustomerLevelData = item94.os.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = 'Stove.Fuel'
                                             ,aggregateColumnName = "Remove")
item94.os.final <- item94.os.final[which(item94.os.final$CK_Building_ID != "Remove"),]

item94.os.cast <- dcast(setDT(item94.os.final)
                        ,formula = BuildingType + Stove.Fuel ~ CK_Building_ID
                        ,value.var = c("w.percent", "w.SE", "n", "EB"))

names(item94.os.cast)
if(os.ind == "scl"){
  item94.os.final <- data.frame("BuildingType"          = item94.os.cast$BuildingType
                                ,"Stove.Fuel"           = item94.os.cast$Stove.Fuel
                                ,"Percent_SCL.GenPop"   = item94.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item94.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item94.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item94.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item94.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item94.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item94.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item94.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item94.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item94.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item94.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item94.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item94.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item94.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item94.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item94.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item94.os.final <- data.frame("BuildingType"          = item94.os.cast$BuildingType
                                ,"Stove.Fuel"           = item94.os.cast$Stove.Fuel
                                ,"Percent_SnoPUD"          = item94.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item94.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item94.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item94.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item94.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item94.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item94.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item94.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item94.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item94.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item94.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item94.os.cast$`EB_2017 RBSA NW`)
}



unique(item94.os.final$Stove.Fuel)
rowOrder <- c("Electric"
              ,"Gas"
              ,"Propane"
              ,"Other"
              ,"Total")
item94.os.final <- item94.os.final %>% mutate(Stove.Fuel = factor(Stove.Fuel, levels = rowOrder)) %>% arrange(Stove.Fuel)
item94.os.final <- data.frame(item94.os.final)

item94.os.final.SF <- item94.os.final[which(item94.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item94.os.final) %in% c("BuildingType"))]

exportTable(item94.os.final.SF, "SF", "Table 101", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item94.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item94.os.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = 'Stove.Fuel'
                                             ,aggregateColumnName = "Remove")
item94.os.final <- item94.os.final[which(item94.os.final$CK_Building_ID != "Remove"),]

item94.os.cast <- dcast(setDT(item94.os.final)
                        ,formula = BuildingType + Stove.Fuel ~ CK_Building_ID
                        ,value.var = c("Percent", "SE", "n"))

names(item94.os.cast)
if(os.ind == "scl"){
  item94.os.final <- data.frame("BuildingType"          = item94.os.cast$BuildingType
                                ,"Stove.Fuel"           = item94.os.cast$Stove.Fuel
                                ,"Percent_SCL.GenPop"   = item94.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item94.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item94.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item94.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item94.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item94.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item94.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item94.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item94.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item94.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item94.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item94.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item94.os.final <- data.frame("BuildingType"          = item94.os.cast$BuildingType
                                ,"Stove.Fuel"           = item94.os.cast$Stove.Fuel
                                ,"Percent_SnoPUD"          = item94.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item94.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item94.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item94.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item94.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item94.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item94.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item94.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item94.os.cast$`n_2017 RBSA NW`)
}



unique(item94.os.final$Stove.Fuel)
rowOrder <- c("Electric"
              ,"Gas"
              ,"Propane"
              ,"Other"
              ,"Total")
item94.os.final <- item94.os.final %>% mutate(Stove.Fuel = factor(Stove.Fuel, levels = rowOrder)) %>% arrange(Stove.Fuel)
item94.os.final <- data.frame(item94.os.final)

item94.os.final.SF <- item94.os.final[which(item94.os.final$BuildingType == "Single Family")
                                      ,-which(colnames(item94.os.final) %in% c("BuildingType"))]

exportTable(item94.os.final.SF, "SF", "Table 101", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
#Item 95: DISTRIBUTION OF STOVE FUEL BY TYPE (SF table 102, MH table 83)
#############################################################################################
#subset to columns needed for analysis
item95.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Oven.Fuel"
                                                                   ,""))]
item95.os.dat$count <- 1

item95.os.dat0 <- item95.os.dat[which(item95.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item95.os.dat1 <- left_join(os.dat, item95.os.dat0, by = "CK_Cadmus_ID")
item95.os.dat2 <- item95.os.dat1[which(item95.os.dat1$Type == "Stove/Oven"),]
unique(item95.os.dat2$Oven.Fuel)
item95.os.dat3 <- item95.os.dat2[which(item95.os.dat2$Oven.Fuel %notin% c("No Oven", "No Cooktop", "No Stove", NA)),]

item95.os.dat3$Oven.Fuel[which(item95.os.dat3$Oven.Fuel %notin% c("Electric", "Gas", "Propane"))] <- "Other"


################################################
# Adding pop and sample sizes for weights
################################################
item95.os.data <- weightedData(item95.os.dat3[-which(colnames(item95.os.dat3) %in% c("count"
                                                                            ,"Type"
                                                                            ,"Oven.Fuel"))])
item95.os.data <- left_join(item95.os.data, unique(item95.os.dat3[which(colnames(item95.os.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"count"
                                                                                     ,"Type"
                                                                                     ,"Oven.Fuel"))]))
item95.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item95.os.final <- proportionRowsAndColumns1(CustomerLevelData = item95.os.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = 'Oven.Fuel'
                                             ,aggregateColumnName = "Remove")
item95.os.final <- item95.os.final[which(item95.os.final$CK_Building_ID != "Remove"),]

item95.os.cast <- dcast(setDT(item95.os.final)
                        ,formula = BuildingType + Oven.Fuel ~ CK_Building_ID
                        ,value.var = c("w.percent", "w.SE", "n", "EB"))

names(item95.os.cast)
if(os.ind == "scl"){
  item95.os.final <- data.frame("BuildingType"          = item95.os.cast$BuildingType
                                ,"Oven.Fuel"           = item95.os.cast$Oven.Fuel
                                ,"Percent_SCL.GenPop"   = item95.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item95.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item95.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item95.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item95.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item95.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item95.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item95.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item95.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item95.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item95.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item95.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item95.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item95.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item95.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item95.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item95.os.final <- data.frame("BuildingType"          = item95.os.cast$BuildingType
                                ,"Oven.Fuel"           = item95.os.cast$Oven.Fuel
                                ,"Percent_SnoPUD"          = item95.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item95.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item95.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item95.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item95.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item95.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item95.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item95.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item95.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item95.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item95.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item95.os.cast$`EB_2017 RBSA NW`)
}



unique(item95.os.final$Oven.Fuel)
rowOrder <- c("Electric"
              ,"Gas"
              ,"Propane"
              ,"Other"
              ,"Total")
item95.os.final <- item95.os.final %>% mutate(Oven.Fuel = factor(Oven.Fuel, levels = rowOrder)) %>% arrange(Oven.Fuel)
item95.os.final <- data.frame(item95.os.final)

item95.os.final.SF <- item95.os.final[which(item95.os.final$BuildingType == "Single Family")
                                      ,-which(colnames(item95.os.final) %in% c("BuildingType"))]

exportTable(item95.os.final.SF, "SF", "Table 102", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item95.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item95.os.data
                                                     ,valueVariable    = 'count'
                                                     ,columnVariable = "CK_Building_ID"
                                                     ,rowVariable = 'Oven.Fuel'
                                                     ,aggregateColumnName = "Remove")
item95.os.final <- item95.os.final[which(item95.os.final$CK_Building_ID != "Remove"),]

item95.os.cast <- dcast(setDT(item95.os.final)
                        ,formula = BuildingType + Oven.Fuel ~ CK_Building_ID
                        ,value.var = c("Percent", "SE", "n"))

names(item95.os.cast)
if(os.ind == "scl"){
  item95.os.final <- data.frame("BuildingType"          = item95.os.cast$BuildingType
                                ,"Oven.Fuel"           = item95.os.cast$Oven.Fuel
                                ,"Percent_SCL.GenPop"   = item95.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item95.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item95.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item95.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item95.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item95.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item95.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item95.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item95.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item95.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item95.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item95.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item95.os.final <- data.frame("BuildingType"          = item95.os.cast$BuildingType
                                ,"Oven.Fuel"           = item95.os.cast$Oven.Fuel
                                ,"Percent_SnoPUD"          = item95.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item95.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item95.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item95.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item95.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item95.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item95.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item95.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item95.os.cast$`n_2017 RBSA NW`)
}



unique(item95.os.final$Oven.Fuel)
rowOrder <- c("Electric"
              ,"Gas"
              ,"Propane"
              ,"Other"
              ,"Total")
item95.os.final <- item95.os.final %>% mutate(Oven.Fuel = factor(Oven.Fuel, levels = rowOrder)) %>% arrange(Oven.Fuel)
item95.os.final <- data.frame(item95.os.final)

item95.os.final.SF <- item95.os.final[which(item95.os.final$BuildingType == "Single Family")
                                      ,-which(colnames(item95.os.final) %in% c("BuildingType"))]

exportTable(item95.os.final.SF, "SF", "Table 102", weighted = FALSE, osIndicator = export.ind, OS = T)
