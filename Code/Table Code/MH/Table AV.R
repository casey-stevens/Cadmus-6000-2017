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
rbsa.dat <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]
length(unique(rbsa.dat$CK_Cadmus_ID))

survey.dat <- data.frame(read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Labeled and Translated"), stringsAsFactors = F)
#clean cadmus IDs
# survey.dat <- data.frame(survey.dat, stringsAsFactors = F)
survey.dat$CK_Cadmus_ID <- trimws(toupper(survey.dat$NEXID))

#subset to columns needed for analysis
tableAV.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Was.it....0.to.under..25.000"
                                                            ,"Was.it....25.000.to.under..50.000"
                                                            ,"Was.it....50.000.or.more"))]
colnames(tableAV.dat) <- c("2015.Houshold.Income.0.to.under.25000"
                           , "2015.Houshold.Income.25000.to.under.50000"
                           , "2015.Houshold.Income.50000.or.more"
                           , "CK_Cadmus_ID")

tableAV.melt <- melt(tableAV.dat, id.vars = "CK_Cadmus_ID")
tableAV.melt <- tableAV.melt[which(tableAV.melt$value %notin% c("Unknown","N/A", NA, "Don't know", "Prefer not to say")),]
names(tableAV.melt) <- c("CK_Cadmus_ID", "Income.Level", "Detailed.Income.Level")


#merge together analysis data with cleaned RBSA data
tableAV.dat1 <- left_join(rbsa.dat, tableAV.melt, by = "CK_Cadmus_ID")
tableAV.dat1 <- tableAV.dat1[which(!is.na(tableAV.dat1$Income.Level)),]
################################################
# Adding pop and sample sizes for weights
################################################
tableAV.data <- weightedData(tableAV.dat1[-which(colnames(tableAV.dat1) %in% c("Income.Level"
                                                                               , "Detailed.Income.Level"))])
tableAV.data <- left_join(tableAV.data, unique(tableAV.dat1[which(colnames(tableAV.dat1) %in% c("CK_Cadmus_ID"
                                                                                         ,"Income.Level"
                                                                                         , "Detailed.Income.Level"))]))
tableAV.data$count <- 1
#######################
# Weighted Analysis
#######################
tableAV.final <- proportionRowsAndColumns1(CustomerLevelData = tableAV.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'Income.Level'
                                           ,aggregateColumnName = "Region")

tableAV.cast <- dcast(setDT(tableAV.final)
                      , formula = BuildingType + Income.Level ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

tableAV.table <- data.frame("BuildingType"    = tableAV.cast$BuildingType
                            ,"Income.Level" = tableAV.cast$Income.Level
                            ,"Percent_ID"     = tableAV.cast$w.percent_ID
                            ,"SE_ID"          = tableAV.cast$w.SE_ID
                            ,"n_ID"           = tableAV.cast$n_ID
                            ,"Percent_MT"     = tableAV.cast$w.percent_MT
                            ,"SE_MT"          = tableAV.cast$w.SE_MT
                            ,"n_MT"           = tableAV.cast$n_MT
                            ,"Percent_OR"     = tableAV.cast$w.percent_OR
                            ,"SE_OR"          = tableAV.cast$w.SE_OR
                            ,"n_OR"           = tableAV.cast$n_OR
                            ,"Percent_WA"     = tableAV.cast$w.percent_WA
                            ,"SE_WA"          = tableAV.cast$w.SE_WA
                            ,"n_WA"           = tableAV.cast$n_WA
                            ,"Percent_Region" = tableAV.cast$w.percent_Region
                            ,"SE_Region"      = tableAV.cast$w.SE_Region
                            ,"n_Region"       = tableAV.cast$n_Region
                            ,"EB_ID"          = tableAV.cast$EB_ID
                            ,"EB_MT"          = tableAV.cast$EB_MT
                            ,"EB_OR"          = tableAV.cast$EB_OR
                            ,"EB_WA"          = tableAV.cast$EB_WA
                            ,"EB_Region"      = tableAV.cast$EB_Region
)

tableAV.final.SF <- tableAV.table[which(tableAV.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAV.table) %in% c("BuildingType"))]
tableAV.final.MH <- tableAV.table[which(tableAV.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAV.table) %in% c("BuildingType"))]

exportTable(tableAV.final.SF, "SF", "Table AV", weighted = TRUE)
# exportTable(tableAV.final.MH, "MH", "Table AV", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableAV.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAV.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'Income.Level'
                                                   ,aggregateColumnName = "Region")

tableAV.cast <- dcast(setDT(tableAV.final)
                      , formula = BuildingType + Income.Level ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


tableAV.table <- data.frame("BuildingType"    = tableAV.cast$BuildingType
                            ,"Income.Level"      = tableAV.cast$Income.Level
                            ,"Percent_ID"     = tableAV.cast$Percent_ID
                            ,"SE_ID"          = tableAV.cast$SE_ID
                            ,"n_ID"           = tableAV.cast$n_ID
                            ,"Percent_MT"     = tableAV.cast$Percent_MT
                            ,"SE_MT"          = tableAV.cast$SE_MT
                            ,"n_MT"           = tableAV.cast$n_MT
                            ,"Percent_OR"     = tableAV.cast$Percent_OR
                            ,"SE_OR"          = tableAV.cast$SE_OR
                            ,"n_OR"           = tableAV.cast$n_OR
                            ,"Percent_WA"     = tableAV.cast$Percent_WA
                            ,"SE_WA"          = tableAV.cast$SE_WA
                            ,"n_WA"           = tableAV.cast$n_WA
                            ,"Percent_Region" = tableAV.cast$Percent_Region
                            ,"SE_Region"      = tableAV.cast$SE_Region
                            ,"n_Region"       = tableAV.cast$n_Region
)

tableAV.final.SF <- tableAV.table[which(tableAV.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAV.table) %in% c("BuildingType"))]
tableAV.final.MH <- tableAV.table[which(tableAV.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAV.table) %in% c("BuildingType"))]

exportTable(tableAV.final.SF, "SF", "Table AV", weighted = FALSE)
# exportTable(tableAV.final.MH, "MH", "Table AV", weighted = FALSE)
















#######################
# Weighted Analysis
#######################
tableAV.data <- tableAV.data[which(tableAV.data$BuildingType == "Multifamily"),]
tableAV.final <- proportionRowsAndColumns1(CustomerLevelData = tableAV.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'Income.Level'
                                           ,aggregateColumnName = "All Sizes")

tableAV.cast <- dcast(setDT(tableAV.final)
                      , formula = BuildingType + Income.Level ~ HomeType
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

tableAV.table <- data.frame("BuildingType"          = tableAV.cast$BuildingType
                            ,"Income.Level"         = tableAV.cast$Income.Level
                            ,"Percent_Low.Rise"     = tableAV.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"SE_Low.Rise"          = tableAV.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"n_Low.Rise"           = tableAV.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Percent_Mid.Rise"     = tableAV.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"SE_Mid.Rise"          = tableAV.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"n_Mid.Rise"           = tableAV.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"Percent_High.Rise"    = tableAV.cast$`w.percent_Apartment Building (More than 6 floors)`
                            ,"SE_High.Rise"         = tableAV.cast$`w.SE_Apartment Building (More than 6 floors)`
                            ,"n_High.Rise"          = tableAV.cast$`n_Apartment Building (More than 6 floors)`
                            ,"Percent_All.Sizes"    = tableAV.cast$`w.percent_All Sizes`
                            ,"SE_All.Sizes"         = tableAV.cast$`w.SE_All Sizes`
                            ,"n_All.Sizes"          = tableAV.cast$`n_All Sizes`
                            ,"EB_Low.Rise"          = tableAV.cast$`EB_Apartment Building (3 or fewer floors)`
                            ,"EB_Mid.Rise"          = tableAV.cast$`EB_Apartment Building (4 to 6 floors)`
                            ,"EB_High.Rise"         = tableAV.cast$`EB_Apartment Building (More than 6 floors)`
                            ,"EB_All.Sizes"         = tableAV.cast$`EB_All Sizes`
)

tableAV.final.MF <- tableAV.table[which(tableAV.table$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAV.table) %in% c("BuildingType"))]

# exportTable(tableAV.final.MF, "MF", "Table AV", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableAV.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAV.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'HomeType'
                                                   ,rowVariable      = 'Income.Level'
                                                   ,aggregateColumnName = "All Sizes")

tableAV.cast <- dcast(setDT(tableAV.final)
                      , formula = BuildingType + Income.Level ~ HomeType
                      , value.var = c("Percent", "SE", "Count", "n"))


tableAV.table <- data.frame("BuildingType"          = tableAV.cast$BuildingType
                            ,"Income.Level"         = tableAV.cast$Income.Level
                            ,"Percent_Low.Rise"     = tableAV.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"SE_Low.Rise"          = tableAV.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"n_Low.Rise"           = tableAV.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Percent_Mid.Rise"     = tableAV.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"SE_Mid.Rise"          = tableAV.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"n_Mid.Rise"           = tableAV.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"Percent_High.Rise"    = tableAV.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"SE_High.Rise"         = tableAV.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"n_High.Rise"          = tableAV.cast$`n_Apartment Building (More than 6 floors)`
                            ,"Percent_All.Sizes"    = tableAV.cast$`Percent_All Sizes`
                            ,"SE_All.Sizes"         = tableAV.cast$`SE_All Sizes`
                            ,"n_All.Sizes"          = tableAV.cast$`n_All Sizes`
)

tableAV.final.MF <- tableAV.table[which(tableAV.table$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAV.table) %in% c("BuildingType"))]

# exportTable(tableAV.final.MF, "MF", "Table AV", weighted = TRUE)



























# ############################################################################################################
# #
# #
# # OVERSAMPLE ANALYSIS
# #
# #
# ############################################################################################################
# # Read in clean os data
# os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
# length(unique(os.dat$CK_Cadmus_ID))
# os.dat$CK_Building_ID <- os.dat$Category
# os.dat <- os.dat[which(names(os.dat) != "Category")]
# 
# #############################################################################################
# #Table AK: Average CFM by Tons of System Capacity by System Type and/or CK_Building_ID
# #############################################################################################
# #subset to columns needed for analysis
# tableAV.os.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
#                                                             ,"Was.it....0.to.under..25.000"
#                                                             ,"Was.it....25.000.to.under..50.000"
#                                                             ,"Was.it....50.000.or.more"))]
# colnames(tableAV.os.dat) <- c("2015.Houshold.Income.0.to.under.25000"
#                            , "2015.Houshold.Income.25000.to.under.50000"
#                            , "2015.Houshold.Income.50000.or.more"
#                            , "CK_Cadmus_ID")
# 
# tableAV.os.melt <- melt(tableAV.os.dat, id.vars = "CK_Cadmus_ID")
# tableAV.os.melt <- tableAV.os.melt[which(tableAV.os.melt$value %notin% c("Unknown","N/A", NA, "Don't know", "Prefer not to say")),]
# names(tableAV.os.melt) <- c("CK_Cadmus_ID", "Income.Level", "Detailed.Income.Level")
# 
# 
# #merge together analysis data with cleaned RBSA data
# tableAV.os.dat1 <- left_join(os.dat, tableAV.os.melt, by = "CK_Cadmus_ID")
# tableAV.os.dat1 <- tableAV.os.dat1[which(!is.na(tableAV.os.dat1$Income.Level)),]
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# tableAV.os.data <- weightedData(tableAV.os.dat1[-which(colnames(tableAV.os.dat1) %in% c("Income.Level"
#                                                                                , "Detailed.Income.Level"))])
# tableAV.os.data <- left_join(tableAV.os.data, unique(tableAV.os.dat1[which(colnames(tableAV.os.dat1) %in% c("CK_Cadmus_ID"
#                                                                                                 ,"Income.Level"
#                                                                                                 , "Detailed.Income.Level"))]))
# tableAV.os.data$count <- 1
# #######################
# # Weighted Analysis
# #######################
# tableAV.os.final <- proportionRowsAndColumns1(CustomerLevelData = tableAV.os.data
#                                            ,valueVariable    = 'count'
#                                            ,columnVariable   = 'CK_Building_ID'
#                                            ,rowVariable      = 'Income.Level'
#                                            ,aggregateColumnName = "Remove")
# 
# tableAV.os.cast <- dcast(setDT(tableAV.os.final)
#                       , formula = BuildingType + Income.Level ~ CK_Building_ID
#                       , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
# 
# if(os.ind == "scl"){
#   tableAV.os.table <- data.frame("BuildingType"          = tableAV.os.cast$BuildingType
#                                  ,"Income.Level"         = tableAV.os.cast$Income.Level
#                                  ,"Percent_SCL.GenPop"   = tableAV.os.cast$`w.percent_SCL GenPop`
#                                  ,"SE_SCL.GenPop"        = tableAV.os.cast$`w.SE_SCL GenPop`
#                                  ,"n_SCL.GenPop"         = tableAV.os.cast$`n_SCL GenPop`
#                                  ,"Percent_SCL.LI"       = tableAV.os.cast$`w.percent_SCL LI`
#                                  ,"SE_SCL.LI"            = tableAV.os.cast$`w.SE_SCL LI`
#                                  ,"n_SCL.LI"             = tableAV.os.cast$`n_SCL LI`
#                                  ,"Percent_SCL.EH"       = tableAV.os.cast$`w.percent_SCL EH`
#                                  ,"SE_SCL.EH"            = tableAV.os.cast$`w.SE_SCL EH`
#                                  ,"n_SCL.EH"             = tableAV.os.cast$`n_SCL EH`
#                                  ,"Percent_2017.RBSA.PS" = tableAV.os.cast$`w.percent_2017 RBSA PS`
#                                  ,"SE_2017.RBSA.PS"      = tableAV.os.cast$`w.SE_2017 RBSA PS`
#                                  ,"n_2017.RBSA.PS"       = tableAV.os.cast$`n_2017 RBSA PS`
#                                  ,"EB_SCL.GenPop"        = tableAV.os.cast$`EB_SCL GenPop`
#                                  ,"EB_SCL.LI"            = tableAV.os.cast$`EB_SCL LI`
#                                  ,"EB_SCL.EH"            = tableAV.os.cast$`EB_SCL EH`
#                                  ,"EB_2017.RBSA.PS"      = tableAV.os.cast$`EB_2017 RBSA PS`
#   )
#   
# }else if(os.ind == "snopud"){
#   tableAV.os.table <- data.frame("BuildingType"             = tableAV.os.cast$BuildingType
#                                  ,"Income.Level"            = tableAV.os.cast$Income.Level
#                                  ,"Percent_SnoPUD"          = tableAV.os.cast$`w.percent_SnoPUD`
#                                  ,"SE_SnoPUD"               = tableAV.os.cast$`w.SE_SnoPUD`
#                                  ,"n_SnoPUD"                = tableAV.os.cast$`n_SnoPUD`
#                                  ,"Percent_2017.RBSA.PS"    = tableAV.os.cast$`w.percent_2017 RBSA PS`
#                                  ,"SE_2017.RBSA.PS"         = tableAV.os.cast$`w.SE_2017 RBSA PS`
#                                  ,"n_2017.RBSA.PS"          = tableAV.os.cast$`n_2017 RBSA PS`
#                                  ,"Percent_RBSA.NW"         = tableAV.os.cast$`w.percent_2017 RBSA NW`
#                                  ,"SE_RBSA.NW"              = tableAV.os.cast$`w.SE_2017 RBSA NW`
#                                  ,"n_RBSA.NW"               = tableAV.os.cast$`n_2017 RBSA NW`
#                                  ,"EB_SnoPUD"               = tableAV.os.cast$`EB_SnoPUD`
#                                  ,"EB_2017.RBSA.PS"         = tableAV.os.cast$`EB_2017 RBSA PS`
#                                  ,"EB_RBSA.NW"              = tableAV.os.cast$`EB_2017 RBSA NW`
#   )
#   
# }
# 
# tableAV.os.final.SF <- tableAV.os.table[which(tableAV.os.table$BuildingType == "Single Family")
#                                   ,-which(colnames(tableAV.os.table) %in% c("BuildingType"))]
# 
# exportTable(tableAV.os.final.SF, "SF", "Table AV", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# tableAV.os.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAV.os.data
#                                                    ,valueVariable    = 'count'
#                                                    ,columnVariable   = 'CK_Building_ID'
#                                                    ,rowVariable      = 'Income.Level'
#                                                    ,aggregateColumnName = "Remove")
# 
# tableAV.os.cast <- dcast(setDT(tableAV.os.final)
#                       , formula = BuildingType + Income.Level ~ CK_Building_ID
#                       , value.var = c("Percent", "SE", "Count", "n"))
# 
# if(os.ind == "scl"){
#   tableAV.os.table <- data.frame("BuildingType"          = tableAV.os.cast$BuildingType
#                                  ,"Income.Level"         = tableAV.os.cast$Income.Level
#                                  ,"Percent_SCL.GenPop"   = tableAV.os.cast$`Percent_SCL GenPop`
#                                  ,"SE_SCL.GenPop"        = tableAV.os.cast$`SE_SCL GenPop`
#                                  ,"n_SCL.GenPop"         = tableAV.os.cast$`n_SCL GenPop`
#                                  ,"Percent_SCL.LI"       = tableAV.os.cast$`Percent_SCL LI`
#                                  ,"SE_SCL.LI"            = tableAV.os.cast$`SE_SCL LI`
#                                  ,"n_SCL.LI"             = tableAV.os.cast$`n_SCL LI`
#                                  ,"Percent_SCL.EH"       = tableAV.os.cast$`Percent_SCL EH`
#                                  ,"SE_SCL.EH"            = tableAV.os.cast$`SE_SCL EH`
#                                  ,"n_SCL.EH"             = tableAV.os.cast$`n_SCL EH`
#                                  ,"Percent_2017.RBSA.PS" = tableAV.os.cast$`Percent_2017 RBSA PS`
#                                  ,"SE_2017.RBSA.PS"      = tableAV.os.cast$`SE_2017 RBSA PS`
#                                  ,"n_2017.RBSA.PS"       = tableAV.os.cast$`n_2017 RBSA PS`
#   )
# }else if(os.ind == "snopud"){
#   tableAV.os.table <- data.frame("BuildingType"             = tableAV.os.cast$BuildingType
#                                  ,"Income.Level"            = tableAV.os.cast$Income.Level
#                                  ,"Percent_SnoPUD"          = tableAV.os.cast$`Percent_SnoPUD`
#                                  ,"SE_SnoPUD"               = tableAV.os.cast$`SE_SnoPUD`
#                                  ,"n_SnoPUD"                = tableAV.os.cast$`n_SnoPUD`
#                                  ,"Percent_2017.RBSA.PS"    = tableAV.os.cast$`Percent_2017 RBSA PS`
#                                  ,"SE_2017.RBSA.PS"         = tableAV.os.cast$`SE_2017 RBSA PS`
#                                  ,"n_2017.RBSA.PS"          = tableAV.os.cast$`n_2017 RBSA PS`
#                                  ,"Percent_RBSA.NW"         = tableAV.os.cast$`Percent_2017 RBSA NW`
#                                  ,"SE_RBSA.NW"              = tableAV.os.cast$`SE_2017 RBSA NW`
#                                  ,"n_RBSA.NW"               = tableAV.os.cast$`n_2017 RBSA NW`
#   )
# }
# 
# 
# tableAV.os.final.SF <- tableAV.os.table[which(tableAV.os.table$BuildingType == "Single Family")
#                                   ,-which(colnames(tableAV.os.table) %in% c("BuildingType"))]
# 
# exportTable(tableAV.os.final.SF, "SF", "Table AV", weighted = FALSE, osIndicator = export.ind, OS = T)
