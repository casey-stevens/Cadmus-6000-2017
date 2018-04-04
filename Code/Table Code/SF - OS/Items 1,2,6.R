#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:          09/11/2017                                  
##  Billing Code(s):  6596.0000.0002.1002.0000
#############################################################################################
#############################################################################################
# For items 1, 2, 6 - this code will summarize information to match the previous RBSA table
#   Step 1: State level analysis
#   Step 2: Region level analysis
#   Step 3: Put data into correct format for creating tables, Subset tables by building type 
#           and export to respective workbooks
#############################################################################################
##  Clear variables
# rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)

##  Create "Not In" operator
"%notin%" <- Negate("%in%")

# Source
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")

# rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
# rbsa.dat <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),] 
# 
# #############################################################################################
# # Item 1 : DISTRIBUTION OF HOMES BY TYPE AND STATE (SF Table 8, MH Table 7)
# #############################################################################################
# item1.dat0 <- rbsa.dat[which(!is.na(rbsa.dat$BuildingTypeXX)),]
# item1.dat <- weightedData(item1.dat0)
# 
# item1.dat$count <- 1
# 
# 
# 
# ######################################
# # Weighted Analysis
# ######################################
# item1.final <- proportionRowsAndColumns1(CustomerLevelData = item1.dat
#                                          ,valueVariable = 'count'
#                                          ,columnVariable = 'State'
#                                          ,rowVariable = 'HomeType'
#                                          ,aggregateColumnName = "Region")
# 
# #cast data into correct format
# item1.cast <- dcast(setDT(item1.final)
#                      ,formula = BuildingType + HomeType ~ State
#                      ,value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
# 
# #can add pop and sample sizes if needed in exported table
# item1.table <- data.frame("BuildingType"    = item1.cast$BuildingType
#                           ,"Home.Type"      = item1.cast$HomeType
#                           ,"Percent_ID"     = item1.cast$w.percent_ID
#                           ,"SE_ID"          = item1.cast$w.SE_ID
#                           ,"n_ID"           = item1.cast$n_ID
#                           ,"Percent_MT"     = item1.cast$w.percent_MT
#                           ,"SE_MT"          = item1.cast$w.SE_MT
#                           ,"n_MT"           = item1.cast$n_MT
#                           ,"Percent_OR"     = item1.cast$w.percent_OR
#                           ,"SE_OR"          = item1.cast$w.SE_OR
#                           ,"n_OR"           = item1.cast$n_OR
#                           ,"Percent_WA"     = item1.cast$w.percent_WA
#                           ,"SE_WA"          = item1.cast$w.SE_WA
#                           ,"n_WA"           = item1.cast$n_WA
#                           ,"Percent_Region" = item1.cast$w.percent_Region
#                           ,"SE_Region"      = item1.cast$w.SE_Region
#                           ,"n"              = item1.cast$n_Region
#                           ,"EB_ID"          = item1.cast$EB_ID
#                           ,"EB_MT"          = item1.cast$EB_MT
#                           ,"EB_OR"          = item1.cast$EB_OR
#                           ,"EB_WA"          = item1.cast$EB_WA
#                           ,"EB_Region"      = item1.cast$EB_Region
#                           ,"")
# 
# # row ordering example code
# levels(item1.table$Home.Type)
# rowOrder <- c("Single Family Detached"
#               ,"Duplex, Triplex, or Fourplex"
#               ,"Townhome or Rowhome"
#               ,"Apartment Building (3 or fewer floors)"
#               ,"Apartment Building (4 to 6 floors)"
#               ,"Apartment Building (More than 6 floors)"
#               ,"Single Wide"
#               ,"Double Wide"
#               ,"Triple Wide"
#               ,"Modular / Prefab"
#               ,"Total")
# item1.table <- item1.table %>% mutate(Home.Type = factor(Home.Type, levels = rowOrder)) %>% arrange(Home.Type)  
# item1.table <- data.frame(item1.table)
# 
# 
# ### Split into respective tables
# item1.table.SF <- data.frame(item1.table[which(item1.table$BuildingType %in% c("Single Family")),-1], stringsAsFactors = F)
# item1.table.MH <- item1.table[which(item1.table$BuildingType %in% c("Manufactured")),-1]
# 
# #exporting function
# exportTable(item1.table.SF, "SF", "Table 8", weighted = TRUE)
# # exportTable(item1.table.MH, "MH", "Table 7", weighted = TRUE)
# 
# 
# ######################################
# # Unweighted Analysis
# ######################################
# item1.final.unweighted <- proportions_two_groups_unweighted(CustomerLevelData = item1.dat
#                                                             ,valueVariable = 'count'
#                                                             ,columnVariable = 'State'
#                                                             ,rowVariable = 'HomeType'
#                                                             ,aggregateColumnName = "Region")
# 
# 
# #cast data into correct format
# item1.cast.unw <- dcast(setDT(item1.final.unweighted)
#                     ,formula = BuildingType + HomeType ~ State
#                     ,value.var = c("Percent", "SE", "Count", "n"))
# 
# #can add pop and sample sizes if needed in exported table
# item1.table.unw <- data.frame("BuildingType"= item1.cast.unw$BuildingType
#                           ,"Home.Type"      = item1.cast.unw$HomeType
#                           ,"Percent_ID"     = item1.cast.unw$Percent_ID
#                           ,"SE_ID"          = item1.cast.unw$SE_ID
#                           ,"n_ID"           = item1.cast.unw$n_ID
#                           ,"Percent_MT"     = item1.cast.unw$Percent_MT
#                           ,"SE_MT"          = item1.cast.unw$SE_MT
#                           ,"n_MT"           = item1.cast.unw$n_MT
#                           ,"Percent_OR"     = item1.cast.unw$Percent_OR
#                           ,"SE_OR"          = item1.cast.unw$SE_OR
#                           ,"n_OR"           = item1.cast.unw$n_OR
#                           ,"Percent_WA"     = item1.cast.unw$Percent_WA
#                           ,"SE_WA"          = item1.cast.unw$SE_WA
#                           ,"n_WA"           = item1.cast.unw$n_WA
#                           ,"Percent_Region" = item1.cast.unw$Percent_Region
#                           ,"SE_Region"      = item1.cast.unw$SE_Region
#                           ,"n"              = item1.cast.unw$n_Region)
# 
# # row ordering example code
# levels(item1.table.unw$Home.Type)
# rowOrder <- c("Single Family Detached"
#               ,"Duplex, Triplex, or Fourplex"
#               ,"Townhome or Rowhome"
#               ,"Apartment Building (3 or fewer floors)"
#               ,"Apartment Building (4 to 6 floors)"
#               ,"Apartment Building (More than 6 floors)"
#               ,"Single Wide"
#               ,"Double Wide"
#               ,"Triple Wide"
#               ,"Modular / Prefab"
#               ,"Total")
# item1.table.unw <- item1.table.unw %>% mutate(Home.Type = factor(Home.Type, levels = rowOrder)) %>% arrange(Home.Type)  
# item1.table.unw <- data.frame(item1.table.unw)
# 
# 
# ### Split into respective tables
# item1.table.SF.unw <- item1.table.unw[which(item1.table.unw$BuildingType %in% c("Single Family")),-1]
# item1.table.MH.unw <- item1.table.unw[which(item1.table.unw$BuildingType %in% c("Manufactured")),-1]
# 
# #exporting function
# exportTable(item1.table.SF.unw, "SF", "Table 8", weighted = FALSE)
# # exportTable(item1.table.MH.unw, "MH", "Table 7", weighted = FALSE)
# 
# 
# #############################################################################################
# # Item 2 : DISTRIBUTION OF HOMES BY VINTAGE AND STATE (SF Table 9, MH Table 8)
# #############################################################################################
# item2.dat <- weightedData(rbsa.dat[which(!is.na(rbsa.dat$HomeYearBuilt)),])
# 
# item2.dat$count <- 1
# 
# 
# ############################
# # weighted Analysis
# ############################
# item2.final <- proportionRowsAndColumns1(item2.dat
#                           , valueVariable = 'count'
#                           , columnVariable = 'State'
#                           , rowVariable = 'HomeYearBuilt_bins2'
#                           , aggregateColumnName = "Region"
#                           )
# 
# item2.cast <- dcast(setDT(item2.final)
#                      ,formula = BuildingType + HomeYearBuilt_bins2 ~ State
#                      ,value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
# 
# item2.table <- data.frame("BuildingType"     = item2.cast$BuildingType
#                            ,"Housing.Vintage" = item2.cast$HomeYearBuilt_bins2
#                            ,"Percent_ID"      = item2.cast$w.percent_ID
#                            ,"SE_ID"           = item2.cast$w.SE_ID
#                            ,"n_ID"            = item2.cast$n_ID
#                            ,"Percent_MT"      = item2.cast$w.percent_MT
#                            ,"SE_MT"           = item2.cast$w.SE_MT
#                            ,"n_MT"            = item2.cast$n_MT
#                            ,"Percent_OR"      = item2.cast$w.percent_OR
#                            ,"SE_OR"           = item2.cast$w.SE_OR
#                            ,"n_OR"            = item2.cast$n_OR
#                            ,"Percent_WA"      = item2.cast$w.percent_WA
#                            ,"SE_WA"           = item2.cast$w.SE_WA
#                            ,"n_WA"            = item2.cast$n_WA
#                            ,"Percent_Region"  = item2.cast$w.percent_Region
#                            ,"SE_Region"       = item2.cast$w.SE_Region
#                            ,"n"               = item2.cast$n_Region
#                           ,"EB_ID"          = item2.cast$EB_ID
#                           ,"EB_MT"          = item2.cast$EB_MT
#                           ,"EB_OR"          = item2.cast$EB_OR
#                           ,"EB_WA"          = item2.cast$EB_WA
#                           ,"EB_Region"      = item2.cast$EB_Region)
# 
# # row ordering example code
# levels(item2.table$Housing.Vintage)
# rowOrder <- c("Pre 1951"
#               ,"1951-1960"
#               ,"1961-1970"
#               ,"1971-1980"
#               ,"1981-1990"
#               ,"1991-2000"
#               ,"2001-2010"
#               ,"Post 2010"
#               ,"Total")
# item2.table <- item2.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
# item2.table <- data.frame(item2.table)
# 
# 
# item2.table.SF <- item2.table[which(item2.table$BuildingType == "Single Family"),-1]
# item2.table.MH <- item2.table[which(item2.table$BuildingType == "Manufactured"),-1]
# 
# #exporting function
# exportTable(item2.table.SF, "SF", "Table 9", weighted = TRUE)
# # exportTable(item2.table.MH, "MH", "Table 8", weighted = TRUE)
# 
# 
# ############################
# # Unweighted Analysis
# ############################
# item2.final <- proportions_two_groups_unweighted(item2.dat
#                                                  , valueVariable = 'count'
#                                                  , columnVariable = 'State'
#                                                  , rowVariable = 'HomeYearBuilt_bins2'
#                                                  , aggregateColumnName = "Region")
# 
# item2.cast <- dcast(setDT(item2.final)
#                     ,formula = BuildingType + HomeYearBuilt_bins2 ~ State
#                     ,value.var = c("Percent", "SE", "n", "Count"))
# 
# item2.table <- data.frame("BuildingType"     = item2.cast$BuildingType
#                           ,"Housing.Vintage" = item2.cast$HomeYearBuilt_bins2
#                           ,"Percent_ID"      = item2.cast$Percent_ID
#                           ,"SE_ID"           = item2.cast$SE_ID
#                           ,"n_ID"            = item2.cast$n_ID
#                           ,"Percent_MT"      = item2.cast$Percent_MT
#                           ,"SE_MT"           = item2.cast$SE_MT
#                           ,"n_MT"            = item2.cast$n_MT
#                           ,"Percent_OR"      = item2.cast$Percent_OR
#                           ,"SE_OR"           = item2.cast$SE_OR
#                           ,"n_OR"            = item2.cast$n_OR
#                           ,"Percent_WA"      = item2.cast$Percent_WA
#                           ,"SE_WA"           = item2.cast$SE_WA
#                           ,"n_WA"            = item2.cast$n_WA
#                           ,"Percent_Region"  = item2.cast$Percent_Region
#                           ,"SE_Region"       = item2.cast$SE_Region
#                           ,"n_Region"        = item2.cast$n_Region)
# 
# # row ordering example code
# levels(item2.table$Housing.Vintage)
# rowOrder <- c("Pre 1951"
#               ,"1951-1960"
#               ,"1961-1970"
#               ,"1971-1980"
#               ,"1981-1990"
#               ,"1991-2000"
#               ,"2001-2010"
#               ,"Post 2010"
#               ,"Total")
# item2.table <- item2.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
# item2.table <- data.frame(item2.table)
# 
# item2.table.SF <- item2.table[which(item2.table$BuildingType == "Single Family"),-1]
# item2.table.MH <- item2.table[which(item2.table$BuildingType == "Manufactured"),-1]
# 
# #exporting function
# exportTable(item2.table.SF, "SF", "Table 9", weighted = FALSE)
# # exportTable(item2.table.MH, "MH", "Table 8", weighted = FALSE)
# 
# 
# 
# #############################################################################################
# # Item 6: DISTRIBUTION OF HOMES BY BUILDING HEIGHT AND STATE (SF table 13)
# #############################################################################################
# item6.dat <- weightedData(rbsa.dat[which(rbsa.dat$BuildingHeight %notin% c(NA, "N/A",0)),])
# unique(item6.dat$BuildingHeight)
# item6.dat$count <- 1
# 
# unique(item6.dat$CK_Building_ID)
# 
# ############################
# # weighted Analysis
# ############################
# item6.final <- proportionRowsAndColumns1(item6.dat
#                                          , valueVariable = 'count'
#                                          , columnVariable = 'State'
#                                          , rowVariable = 'BuildingHeight'
#                                          , aggregateColumnName = "Region"
#                                          )
# 
# colnames(item6.final) <- c("BuildingType"
#                            , "State"
#                            , "BuildingHeight"
#                            , "Percent"
#                            , "SE"
#                            , "Count"
#                            , "N"
#                            , "n"
#                            , "EB")
# 
# item6.cast <- dcast(setDT(item6.final)
#                      ,formula = BuildingType + BuildingHeight ~ State
#                      ,value.var = c("Percent", "SE", "Count", "N", "n", "EB"))
# 
# item6.table <- data.frame("BuildingType"    = item6.cast$BuildingType
#                           ,"BuildingHeight" = item6.cast$BuildingHeight
#                           ,"Percent_ID"     = item6.cast$Percent_ID
#                           ,"SE_ID"          = item6.cast$SE_ID
#                           ,"n_ID"           = item6.cast$n_ID
#                           ,"Percent_MT"     = item6.cast$Percent_MT
#                           ,"SE_MT"          = item6.cast$SE_MT
#                           ,"n_MT"           = item6.cast$n_MT
#                           ,"Percent_OR"     = item6.cast$Percent_OR
#                           ,"SE_OR"          = item6.cast$SE_OR
#                           ,"n_OR"           = item6.cast$n_OR
#                           ,"Percent_WA"     = item6.cast$Percent_WA
#                           ,"SE_WA"          = item6.cast$SE_WA
#                           ,"n_WA"           = item6.cast$n_WA
#                           ,"Percent_Region" = item6.cast$Percent_Region
#                           ,"SE_Region"      = item6.cast$SE_Region
#                           ,"n_Region"       = item6.cast$n_Region
#                           ,"EB_ID"          = item6.cast$EB_ID
#                           ,"EB_MT"          = item6.cast$EB_MT
#                           ,"EB_OR"          = item6.cast$EB_OR
#                           ,"EB_WA"          = item6.cast$EB_WA
#                           ,"EB_Region"      = item6.cast$EB_Region)
# 
# item6.table.SF <- item6.table[which(item6.table$BuildingType == "Single Family"),-1]
# 
# exportTable(item6.table.SF, "SF", "Table 13", weighted = TRUE)
# 
# 
# 
# 
# 
# ############################
# # Unweighted Analysis
# ############################
# item6.final <- proportions_two_groups_unweighted(item6.dat
#                                          , valueVariable = 'count'
#                                          , columnVariable = 'State'
#                                          , rowVariable = 'BuildingHeight'
#                                          , aggregateColumnName = "Region"
# )
# 
# colnames(item6.final) <- c("BuildingType"
#                            , "State"
#                            , "BuildingHeight"
#                            , "Count"
#                            , "n"
#                            , "Percent"
#                            , "SE")
# 
# item6.cast <- dcast(setDT(item6.final)
#                     ,formula = BuildingType + BuildingHeight ~ State
#                     ,value.var = c("Percent", "SE", "Count", "n"))
# 
# item6.table <- data.frame("BuildingType"    = item6.cast$BuildingType
#                           ,"BuildingHeight" = item6.cast$BuildingHeight
#                           ,"Percent_ID"     = item6.cast$Percent_ID
#                           ,"SE_ID"          = item6.cast$SE_ID
#                           ,"n_ID"           = item6.cast$n_ID
#                           ,"Percent_MT"     = item6.cast$Percent_MT
#                           ,"SE_MT"          = item6.cast$SE_MT
#                           ,"n_MT"           = item6.cast$n_MT
#                           ,"Percent_OR"     = item6.cast$Percent_OR
#                           ,"SE_OR"          = item6.cast$SE_OR
#                           ,"n_OR"           = item6.cast$n_OR
#                           ,"Percent_WA"     = item6.cast$Percent_WA
#                           ,"SE_WA"          = item6.cast$SE_WA
#                           ,"n_WA"           = item6.cast$n_WA
#                           ,"Percent_Region" = item6.cast$Percent_Region
#                           ,"SE_Region"      = item6.cast$SE_Region
#                           ,"n_Region"       = item6.cast$n_Region)
# 
# item6.table.SF <- item6.table[which(item6.table$BuildingType == "Single Family"),-1]
# 
# exportTable(item6.table.SF, "SF", "Table 13", weighted = FALSE)




























##################################################################################################
#
#
# OVERSAMPLE ANALYSES
#
#
##################################################################################################
# Read in clean os data
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]


#############################################################################################
# Item 1 : DISTRIBUTION OF HOMES BY TYPE AND STATE (SF Table 8, MH Table 7)
#############################################################################################
item1.os.dat0 <- os.dat
item1.os.dat <- weightedData(item1.os.dat0)

item1.os.dat$count <- 1
######################################
# Weighted Analysis - OVERSAMPLES
######################################
item1.os.final <- proportionRowsAndColumns1(CustomerLevelData = item1.os.dat
                                            ,valueVariable = 'count'
                                            ,columnVariable = 'CK_Building_ID'
                                            ,rowVariable = 'HomeType'
                                            ,aggregateColumnName = "Region")

#cast data into correct format
item1.os.cast <- dcast(setDT(item1.os.final)
                       ,formula = BuildingType + HomeType ~ CK_Building_ID
                       ,value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

if(os.ind == "scl"){
  #can add pop and sample sizes if needed in exported table
  item1.os.table <- data.frame("BuildingType"          = item1.os.cast$BuildingType
                                ,"Home.Type"            = item1.os.cast$HomeType
                                ,"Percent_SCL.GenPop"   = item1.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item1.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item1.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item1.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item1.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item1.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item1.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item1.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item1.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item1.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item1.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item1.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item1.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item1.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item1.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item1.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  #can add pop and sample sizes if needed in exported table
  item1.os.table <- data.frame("BuildingType"             = item1.os.cast$BuildingType
                               ,"Home.Type"               = item1.os.cast$HomeType
                               ,"Percent_SnoPUD"          = item1.os.cast$`w.percent_SnoPUD`
                               ,"SE_SnoPUD"               = item1.os.cast$`w.SE_SnoPUD`
                               ,"n_SnoPUD"                = item1.os.cast$`n_SnoPUD`
                               ,"Percent_2017.RBSA.PS"    = item1.os.cast$`w.percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"         = item1.os.cast$`w.SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"          = item1.os.cast$`n_2017 RBSA PS`
                               ,"Percent_RBSA.NW"         = item1.os.cast$`w.percent_2017 RBSA NW`
                               ,"SE_RBSA.NW"              = item1.os.cast$`w.SE_2017 RBSA NW`
                               ,"n_RBSA.NW"               = item1.os.cast$`n_2017 RBSA NW`
                               ,"EB_SnoPUD"               = item1.os.cast$`EB_SnoPUD`
                               ,"EB_2017.RBSA.PS"         = item1.os.cast$`EB_2017 RBSA PS`
                               ,"EB_RBSA.NW"              = item1.os.cast$`EB_2017 RBSA NW`)
}

# row ordering example code
levels(item1.os.table$Home.Type)
rowOrder <- c("Single Family Detached"
              ,"Duplex, Triplex, or Fourplex"
              ,"Townhome or Rowhome"
              ,"Apartment Building (3 or fewer floors)"
              ,"Apartment Building (4 to 6 floors)"
              ,"Apartment Building (More than 6 floors)"
              ,"Single Wide"
              ,"Double Wide"
              ,"Triple Wide"
              ,"Modular / Prefab"
              ,"Total")
item1.os.table <- item1.os.table %>% mutate(Home.Type = factor(Home.Type, levels = rowOrder)) %>% arrange(Home.Type)
item1.os.table <- data.frame(item1.os.table)


### Split into respective tables
item1.os.table.SF <- data.frame(item1.os.table[which(item1.os.table$BuildingType %in% c("Single Family")),-1], stringsAsFactors = F)

exportTable(item1.os.table.SF,"SF","Table 8",weighted = T, osIndicator = export.ind, OS = T)
######################################
# unweighted Analysis - OVERSAMPLES
######################################
item1.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item1.os.dat
                                                    ,valueVariable = 'count'
                                                    ,columnVariable = 'CK_Building_ID'
                                                    ,rowVariable = 'HomeType'
                                                    ,aggregateColumnName = "Region")

#cast data into correct format
item1.os.cast <- dcast(setDT(item1.os.final)
                       ,formula = BuildingType + HomeType ~ CK_Building_ID
                       ,value.var = c("Percent", "SE", "Count", "n"))

if(os.ind == "scl"){
  #can add pop and sample sizes if needed in exported table
  item1.os.table <- data.frame("BuildingType"          = item1.os.cast$BuildingType
                               ,"Home.Type"            = item1.os.cast$HomeType
                               ,"Percent_SCL.GenPop"   = item1.os.cast$`Percent_SCL GenPop`
                               ,"SE_SCL.GenPop"        = item1.os.cast$`SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = item1.os.cast$`n_SCL GenPop`
                               ,"Percent_SCL.LI"       = item1.os.cast$`Percent_SCL LI`
                               ,"SE_SCL.LI"            = item1.os.cast$`SE_SCL LI`
                               ,"n_SCL.LI"             = item1.os.cast$`n_SCL LI`
                               ,"Percent_SCL.EH"       = item1.os.cast$`Percent_SCL EH`
                               ,"SE_SCL.EH"            = item1.os.cast$`SE_SCL EH`
                               ,"n_SCL.EH"             = item1.os.cast$`n_SCL EH`
                               ,"Percent_2017.RBSA.PS" = item1.os.cast$`Percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item1.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item1.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  #can add pop and sample sizes if needed in exported table
  item1.os.table <- data.frame("BuildingType"             = item1.os.cast$BuildingType
                               ,"Home.Type"               = item1.os.cast$HomeType
                               ,"Percent_SnoPUD"          = item1.os.cast$`Percent_SnoPUD`
                               ,"SE_SnoPUD"               = item1.os.cast$`SE_SnoPUD`
                               ,"n_SnoPUD"                = item1.os.cast$`n_SnoPUD`
                               ,"Percent_2017.RBSA.PS"    = item1.os.cast$`Percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"         = item1.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"          = item1.os.cast$`n_2017 RBSA PS`
                               ,"Percent_RBSA.NW"         = item1.os.cast$`Percent_2017 RBSA NW`
                               ,"SE_RBSA.NW"              = item1.os.cast$`SE_2017 RBSA NW`
                               ,"n_RBSA.NW"               = item1.os.cast$`n_2017 RBSA NW`)
}


# row ordering example code
levels(item1.os.table$Home.Type)
rowOrder <- c("Single Family Detached"
              ,"Duplex, Triplex, or Fourplex"
              ,"Townhome or Rowhome"
              ,"Apartment Building (3 or fewer floors)"
              ,"Apartment Building (4 to 6 floors)"
              ,"Apartment Building (More than 6 floors)"
              ,"Single Wide"
              ,"Double Wide"
              ,"Triple Wide"
              ,"Modular / Prefab"
              ,"Total")
item1.os.table <- item1.os.table %>% mutate(Home.Type = factor(Home.Type, levels = rowOrder)) %>% arrange(Home.Type)
item1.os.table <- data.frame(item1.os.table)


### Split into respective tables
item1.os.table.SF <- data.frame(item1.os.table[which(item1.os.table$BuildingType %in% c("Single Family")),-1], stringsAsFactors = F)

exportTable(item1.os.table.SF,"SF","Table 8",weighted = F, osIndicator = export.ind, OS = T)




#############################################################################################
# Item 2 : DISTRIBUTION OF HOMES BY VINTAGE AND STATE (SF Table 9, MH Table 8)
#############################################################################################
item2.dat <- weightedData(os.dat[which(!is.na(os.dat$HomeYearBuilt)),])

item2.dat$count <- 1

######################################
# Weighted Analysis - OVERSAMPLES
######################################
item2.os.final <- proportionRowsAndColumns1(CustomerLevelData = item2.dat
                                            ,valueVariable = 'count'
                                            ,columnVariable = 'CK_Building_ID'
                                            ,rowVariable = 'HomeYearBuilt_bins2'
                                            ,aggregateColumnName = "Region")

#cast data into correct format
item2.os.cast <- dcast(setDT(item2.os.final)
                       ,formula = BuildingType + HomeYearBuilt_bins2 ~ CK_Building_ID
                       ,value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

if(os.ind == "scl"){
  #can add pop and sample sizes if needed in exported table
  item2.os.table <- data.frame("BuildingType"          = item2.os.cast$BuildingType
                               ,"Housing.Vintage"      = item2.os.cast$HomeYearBuilt_bins2
                               ,"Percent_SCL.GenPop"   = item2.os.cast$`w.percent_SCL GenPop`
                               ,"SE_SCL.GenPop"        = item2.os.cast$`w.SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = item2.os.cast$`n_SCL GenPop`
                               ,"Percent_SCL.LI"       = item2.os.cast$`w.percent_SCL LI`
                               ,"SE_SCL.LI"            = item2.os.cast$`w.SE_SCL LI`
                               ,"n_SCL.LI"             = item2.os.cast$`n_SCL LI`
                               ,"Percent_SCL.EH"       = item2.os.cast$`w.percent_SCL EH`
                               ,"SE_SCL.EH"            = item2.os.cast$`w.SE_SCL EH`
                               ,"n_SCL.EH"             = item2.os.cast$`n_SCL EH`
                               ,"Percent_2017.RBSA.PS" = item2.os.cast$`w.percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item2.os.cast$`w.SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item2.os.cast$`n_2017 RBSA PS`
                               ,"EB_SCL.GenPop"        = item2.os.cast$`EB_SCL GenPop`
                               ,"EB_SCL.LI"            = item2.os.cast$`EB_SCL LI`
                               ,"EB_SCL.EH"            = item2.os.cast$`EB_SCL EH`
                               ,"EB_2017.RBSA.PS"      = item2.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  #can add pop and sample sizes if needed in exported table
  item2.os.table <- data.frame("BuildingType"          = item2.os.cast$BuildingType
                               ,"Housing.Vintage"      = item2.os.cast$HomeYearBuilt_bins2
                               ,"Percent_SnoPUD"          = item2.os.cast$`w.percent_SnoPUD`
                               ,"SE_SnoPUD"               = item2.os.cast$`w.SE_SnoPUD`
                               ,"n_SnoPUD"                = item2.os.cast$`n_SnoPUD`
                               ,"Percent_2017.RBSA.PS"    = item2.os.cast$`w.percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"         = item2.os.cast$`w.SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"          = item2.os.cast$`n_2017 RBSA PS`
                               ,"Percent_RBSA.NW"         = item2.os.cast$`w.percent_2017 RBSA NW`
                               ,"SE_RBSA.NW"              = item2.os.cast$`w.SE_2017 RBSA NW`
                               ,"n_RBSA.NW"               = item2.os.cast$`n_2017 RBSA NW`
                               ,"EB_SnoPUD"               = item2.os.cast$`EB_SnoPUD`
                               ,"EB_2017.RBSA.PS"         = item2.os.cast$`EB_2017 RBSA PS`
                               ,"EB_RBSA.NW"              = item2.os.cast$`EB_2017 RBSA NW`)
}


# row ordering example code
# row ordering example code
levels(item2.os.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"Total")
item2.os.table <- item2.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)
item2.os.table <- data.frame(item2.os.table)


### Split into respective tables
item2.os.table.SF <- data.frame(item2.os.table[which(item2.os.table$BuildingType %in% c("Single Family")),-1], stringsAsFactors = F)

exportTable(item2.os.table.SF,"SF","Table 9",weighted = T, osIndicator = export.ind, OS = T)

######################################
# unweighted Analysis - OVERSAMPLES
######################################
item2.os.final <- proportions_two_groups_unweighted(item2.dat
                                                    , valueVariable = 'count'
                                                    , columnVariable = 'CK_Building_ID'
                                                    , rowVariable = 'HomeYearBuilt_bins2'
                                                    , aggregateColumnName = "Region")

item2.os.cast <- dcast(setDT(item2.os.final)
                       ,formula = BuildingType + HomeYearBuilt_bins2 ~ CK_Building_ID
                       ,value.var = c("Percent", "SE", "n", "Count"))

if(os.ind == "scl"){
  #can add pop and sample sizes if needed in exported table
  item2.os.table <- data.frame("BuildingType"          = item2.os.cast$BuildingType
                               ,"Housing.Vintage"      = item2.os.cast$HomeYearBuilt_bins2
                               ,"Percent_SCL.GenPop"   = item2.os.cast$`Percent_SCL GenPop`
                               ,"SE_SCL.GenPop"        = item2.os.cast$`SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = item2.os.cast$`n_SCL GenPop`
                               ,"Percent_SCL.LI"       = item2.os.cast$`Percent_SCL LI`
                               ,"SE_SCL.LI"            = item2.os.cast$`SE_SCL LI`
                               ,"n_SCL.LI"             = item2.os.cast$`n_SCL LI`
                               ,"Percent_SCL.EH"       = item2.os.cast$`Percent_SCL EH`
                               ,"SE_SCL.EH"            = item2.os.cast$`SE_SCL EH`
                               ,"n_SCL.EH"             = item2.os.cast$`n_SCL EH`
                               ,"Percent_2017.RBSA.PS" = item2.os.cast$`Percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item2.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item2.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  #can add pop and sample sizes if needed in exported table
  item2.os.table <- data.frame("BuildingType"          = item2.os.cast$BuildingType
                               ,"Housing.Vintage"      = item2.os.cast$HomeYearBuilt_bins2
                               ,"Percent_SnoPUD"          = item2.os.cast$`Percent_SnoPUD`
                               ,"SE_SnoPUD"               = item2.os.cast$`SE_SnoPUD`
                               ,"n_SnoPUD"                = item2.os.cast$`n_SnoPUD`
                               ,"Percent_2017.RBSA.PS"    = item2.os.cast$`Percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"         = item2.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"          = item2.os.cast$`n_2017 RBSA PS`
                               ,"Percent_RBSA.NW"         = item2.os.cast$`Percent_2017 RBSA NW`
                               ,"SE_RBSA.NW"              = item2.os.cast$`SE_2017 RBSA NW`
                               ,"n_RBSA.NW"               = item2.os.cast$`n_2017 RBSA NW`)
}


# row ordering example code
levels(item2.os.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"Total")
item2.os.table <- item2.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)
item2.os.table <- data.frame(item2.os.table)

item2.os.table.SF <- item2.os.table[which(item2.os.table$BuildingType == "Single Family"),-1]

exportTable(item2.os.table.SF,"SF","Table 9",weighted = F, osIndicator = export.ind, OS = T)




#############################################################################################
# Item 6: DISTRIBUTION OF HOMES BY BUILDING HEIGHT AND STATE (SF table 13)
#############################################################################################
item6.os.dat <- weightedData(os.dat[which(os.dat$BuildingHeight %notin% c(NA, "N/A",0)),])
unique(item6.os.dat$BuildingHeight)
item6.os.dat$count <- 1

unique(item6.os.dat$CK_Building_ID)

############################
# weighted Analysis
############################
item6.os.final <- proportionRowsAndColumns1(item6.os.dat
                                            , valueVariable = 'count'
                                            , columnVariable = 'CK_Building_ID'
                                            , rowVariable = 'BuildingHeight'
                                            , aggregateColumnName = "Region"
)

item6.os.cast <- dcast(setDT(item6.os.final)
                       ,formula = BuildingType + BuildingHeight ~ CK_Building_ID
                       ,value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

if(os.ind == "scl"){
  item6.os.table <- data.frame("BuildingType"       = item6.os.cast$BuildingType
                               ,"Building.Height"      = item6.os.cast$BuildingHeight
                               ,"Percent_SCL.GenPop"   = item6.os.cast$`w.percent_SCL GenPop`
                               ,"SE_SCL.GenPop"        = item6.os.cast$`w.SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = item6.os.cast$`n_SCL GenPop`
                               ,"Percent_SCL.LI"       = item6.os.cast$`w.percent_SCL LI`
                               ,"SE_SCL.LI"            = item6.os.cast$`w.SE_SCL LI`
                               ,"n_SCL.LI"             = item6.os.cast$`n_SCL LI`
                               ,"Percent_SCL.EH"       = item6.os.cast$`w.percent_SCL EH`
                               ,"SE_SCL.EH"            = item6.os.cast$`w.SE_SCL EH`
                               ,"n_SCL.EH"             = item6.os.cast$`n_SCL EH`
                               ,"Percent_2017.RBSA.PS" = item6.os.cast$`w.percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item6.os.cast$`w.SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item6.os.cast$`n_2017 RBSA PS`
                               ,"EB_SCL.GenPop"        = item6.os.cast$`EB_SCL GenPop`
                               ,"EB_SCL.LI"            = item6.os.cast$`EB_SCL LI`
                               ,"EB_SCL.EH"            = item6.os.cast$`EB_SCL EH`
                               ,"EB_2017.RBSA.PS"      = item6.os.cast$`EB_2017 RBSA PS`)

}else if(os.ind == "snopud"){
  item6.os.table <- data.frame("BuildingType"       = item6.os.cast$BuildingType
                               ,"Building.Height"      = item6.os.cast$BuildingHeight
                               ,"Percent_SnoPUD"          = item6.os.cast$`w.percent_SnoPUD`
                               ,"SE_SnoPUD"               = item6.os.cast$`w.SE_SnoPUD`
                               ,"n_SnoPUD"                = item6.os.cast$`n_SnoPUD`
                               ,"Percent_2017.RBSA.PS"    = item6.os.cast$`w.percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"         = item6.os.cast$`w.SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"          = item6.os.cast$`n_2017 RBSA PS`
                               ,"Percent_RBSA.NW"         = item6.os.cast$`w.percent_2017 RBSA NW`
                               ,"SE_RBSA.NW"              = item6.os.cast$`w.SE_2017 RBSA NW`
                               ,"n_RBSA.NW"               = item6.os.cast$`n_2017 RBSA NW`
                               ,"EB_SnoPUD"               = item6.os.cast$`EB_SnoPUD`
                               ,"EB_2017.RBSA.PS"         = item6.os.cast$`EB_2017 RBSA PS`
                               ,"EB_RBSA.NW"              = item6.os.cast$`EB_2017 RBSA NW`)

}

item6.os.table.SF <- item6.os.table[which(item6.os.table$BuildingType == "Single Family"),-1]

exportTable(item6.os.table.SF, "SF", "Table 13", weighted = TRUE, osIndicator = export.ind, OS = T)

############################
# Unweighted Analysis
############################
item6.os.final <- proportions_two_groups_unweighted(item6.os.dat
                                                    , valueVariable = 'count'
                                                    , columnVariable = 'CK_Building_ID'
                                                    , rowVariable = 'BuildingHeight'
                                                    , aggregateColumnName = "Region"
)

item6.os.cast <- dcast(setDT(item6.os.final)
                       ,formula = BuildingType + BuildingHeight ~ CK_Building_ID
                       ,value.var = c("Percent", "SE", "Count", "n"))

if(os.ind == "scl"){
  item6.os.table <- data.frame("BuildingType"          = item6.os.cast$BuildingType
                               ,"Building.Height"      = item6.os.cast$BuildingHeight
                               ,"Percent_SCL.GenPop"   = item6.os.cast$`Percent_SCL GenPop`
                               ,"SE_SCL.GenPop"        = item6.os.cast$`SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = item6.os.cast$`n_SCL GenPop`
                               ,"Percent_SCL.LI"       = item6.os.cast$`Percent_SCL LI`
                               ,"SE_SCL.LI"            = item6.os.cast$`SE_SCL LI`
                               ,"n_SCL.LI"             = item6.os.cast$`n_SCL LI`
                               ,"Percent_SCL.EH"       = item6.os.cast$`Percent_SCL EH`
                               ,"SE_SCL.EH"            = item6.os.cast$`SE_SCL EH`
                               ,"n_SCL.EH"             = item6.os.cast$`n_SCL EH`
                               ,"Percent_2017.RBSA.PS" = item6.os.cast$`Percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item6.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item6.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item6.os.table <- data.frame("BuildingType"          = item6.os.cast$BuildingType
                               ,"Building.Height"      = item6.os.cast$BuildingHeight
                               ,"Percent_SnoPUD"          = item6.os.cast$`Percent_SnoPUD`
                               ,"SE_SnoPUD"               = item6.os.cast$`SE_SnoPUD`
                               ,"n_SnoPUD"                = item6.os.cast$`n_SnoPUD`
                               ,"Percent_2017.RBSA.PS"    = item6.os.cast$`Percent_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"         = item6.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"          = item6.os.cast$`n_2017 RBSA PS`
                               ,"Percent_RBSA.NW"         = item6.os.cast$`Percent_2017 RBSA NW`
                               ,"SE_RBSA.NW"              = item6.os.cast$`SE_2017 RBSA NW`
                               ,"n_RBSA.NW"               = item6.os.cast$`n_2017 RBSA NW`)
}


item6.os.table.SF <- item6.os.table[which(item6.os.table$BuildingType == "Single Family"),-1]

exportTable(item6.os.table.SF, "SF", "Table 13", weighted = FALSE, osIndicator = export.ind, OS = T)
