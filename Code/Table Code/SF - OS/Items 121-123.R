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
# sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))



#############################################################################################
#Item 121: AVERAGE OCCUPANT AGE PER HOME BY STATE (SF table 128, MH table 103)
#############################################################################################
# #subset to columns needed for analysis
# item121.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
#                                                                                      ,""
#                                                                                      ,""))])
# item121.dat$count <- 1
# 
# #remove any repeat header rows from exporting
# item121.dat0 <- item121.dat[which(item121.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# #merge together analysis data with cleaned RBSA data
# item121.dat1 <- left_join(item121.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 122: AVERAGE NUMBER OF OCCUPANTS PER HOME BY STATE (SF table 129, MH table 104)
# #############################################################################################
# #subset to columns needed for analysis
# item122.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
#                                                                                      ,"Qty.Occupants"
#                                                                                      ,""))])
# item122.dat$count <- 1
# 
# #remove any repeat header rows from exporting
# item122.dat0 <- item122.dat[which(item122.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# #merge together analysis data with cleaned RBSA data
# item122.dat1 <- left_join(rbsa.dat, item122.dat0, by = "CK_Cadmus_ID")
# 
# item122.dat2 <- item122.dat1[which(!is.na(item122.dat1$Qty.Occupants)), ]
# 
# item122.merge <- item122.dat2
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item122.data <- weightedData(item122.merge[-which(colnames(item122.merge) %in% c("Qty.Occupants"
#                                                                                  ,"count"))])
# item122.data <- left_join(item122.data, item122.merge[which(colnames(item122.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Qty.Occupants"
#                                                                                            ,"count"))])
# 
# item122.data$count <- 1
# #######################
# # Weighted Analysis
# #######################
# item122.final <- mean_one_group(item122.data
#                                 ,valueVariable = 'Qty.Occupants'
#                                 ,byVariable = 'State'
#                                 ,aggregateRow = 'Region')
# 
# item122.final.SF <- item122.final[which(item122.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item122.final) %in% c("BuildingType"))]
# item122.final.MH <- item122.final[which(item122.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item122.final) %in% c("BuildingType"))]
# 
# exportTable(item122.final.SF, "SF", "Table 129", weighted = TRUE)
# # exportTable(item122.final.MH, "MH", "Table 104", weighted = TRUE)
# 
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# item122.final <- mean_one_group_unweighted(item122.data
#                                            ,valueVariable = 'Qty.Occupants'
#                                            ,byVariable = 'State'
#                                            ,aggregateRow = 'Region')
# 
# item122.final.SF <- item122.final[which(item122.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item122.final) %in% c("BuildingType"))]
# item122.final.MH <- item122.final[which(item122.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item122.final) %in% c("BuildingType"))]
# 
# exportTable(item122.final.SF, "SF", "Table 129", weighted = FALSE)
# # exportTable(item122.final.MH, "MH", "Table 104", weighted = FALSE)
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 123: AVERAGE NUMBER OF OCCUPANTS BY AGE CATEGORY BY STATE (SF table 130, MH table 105)
# #############################################################################################
# #subset to columns needed for analysis
# item123.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
#                                                                                      ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_LessThan1"
#                                                                                      ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_1_5"
#                                                                                      ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_6_10"
#                                                                                      ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_11_18"
#                                                                                      ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_19_45"
#                                                                                      ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_46_64"
#                                                                                      ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_65Older"
#                                                                                      ,"Qty.Occupants"))])
# colnames(item123.dat) <- c("CK_Cadmus_ID"
#                            ,"Age_1_5"
#                            ,"Age_11_18"
#                            ,"Age_19_45"
#                            ,"Age_46_64"
#                            ,"Age_6_10"
#                            ,"Age_65_Older"
#                            ,"Age_Less_Than_1"
#                            ,"Qty.Occupants")
# item123.dat$count <- 1
# 
# #remove any repeat header rows from exporting
# item123.dat0 <- item123.dat[which(item123.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# #merge together analysis data with cleaned RBSA data
# item123.dat1 <- left_join(item123.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# str(item123.dat1)
# item123.dat1$Age_1_5    <- as.numeric(as.character(item123.dat1$Age_1_5))
# item123.dat1$Age_11_18    <- as.numeric(as.character(item123.dat1$Age_11_18))
# item123.dat1$Age_19_45    <- as.numeric(as.character(item123.dat1$Age_19_45))
# item123.dat1$Age_46_64    <- as.numeric(as.character(item123.dat1$Age_46_64))
# item123.dat1$Age_6_10    <- as.numeric(as.character(item123.dat1$Age_6_10))
# item123.dat1$Age_65_Older    <- as.numeric(as.character(item123.dat1$Age_65_Older))
# item123.dat1$Age_Less_Than_1    <- as.numeric(as.character(item123.dat1$Age_Less_Than_1))
# 
# item123.dat1$Age_0_18 <- item123.dat1$Age_Less_Than_1 + item123.dat1$Age_1_5 + item123.dat1$Age_6_10 + item123.dat1$Age_11_18
# item123.dat1$Age_19_64 <- item123.dat1$Age_19_45 + item123.dat1$Age_46_64
# 
# item123.dat2 <- item123.dat1[which(colnames(item123.dat1) %notin% c("Age_1_5"
#                                                                     ,"Age_11_18"
#                                                                     ,"Age_19_45"
#                                                                     ,"Age_46_64"
#                                                                     ,"Age_6_10"
#                                                                     ,"Age_Less_Than_1"))]
# 
# item123.melt <- melt(item123.dat2, measure.vars = c("Age_0_18"
#                                                     , "Age_19_64"
#                                                     , "Age_65_Older"
#                                                     , "Qty.Occupants"))
# colnames(item123.melt)[which(colnames(item123.melt) == "variable")] <- "Age.Category"
# 
# item123.sum <- summarise(group_by(item123.melt, CK_Cadmus_ID, Age.Category)
#                          ,Occupants = sum(value, na.rm = T))
# 
# item123.merge <- left_join(rbsa.dat, item123.sum)
# item123.merge <- item123.merge[which(!is.na(item123.merge$Occupants)),]
# 
# 
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item123.data <- weightedData(item123.merge[-which(colnames(item123.merge) %in% c("Age.Category"               
#                                                                                  ,"Occupants"))])
# item123.data <- left_join(item123.data, item123.merge[which(colnames(item123.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Age.Category"               
#                                                                                            ,"Occupants"))])
# item123.data$count <- 1
# #######################
# # Weighted Analysis
# #######################
# item123.summary <- mean_two_groups(CustomerLevelData = item123.data
#                                 ,valueVariable    = 'Occupants'
#                                 ,byVariableRow    = 'Age.Category'
#                                 ,byVariableColumn = 'State'
#                                 ,columnAggregate  = "Region"
#                                 ,rowAggregate     = "Remove")
# item123.summary <- item123.summary[which(item123.summary$Age.Category != "Remove"),]
# item123.summary$Age.Category <- as.character(item123.summary$Age.Category)
# item123.summary$Age.Category[which(item123.summary$Age.Category == "Qty.Occupants")] <- "All Ages"
# 
# item123.cast <- item123.summary
# 
# item123.table <- data.frame("BuildingType"    = item123.cast$BuildingType
#                             ,"Age.Category"   = item123.cast$Age.Category
#                             ,"Mean_ID"        = item123.cast$Mean_ID
#                             ,"SE_ID"          = item123.cast$SE_ID
#                             ,"n_ID"           = item123.cast$n_ID
#                             ,"Mean_MT"        = item123.cast$Mean_MT
#                             ,"SE_MT"          = item123.cast$SE_MT
#                             ,"n_MT"           = item123.cast$n_MT
#                             ,"Mean_OR"        = item123.cast$Mean_OR
#                             ,"SE_OR"          = item123.cast$SE_OR
#                             ,"n_OR"           = item123.cast$n_OR
#                             ,"Mean_WA"        = item123.cast$Mean_WA
#                             ,"SE_WA"          = item123.cast$SE_WA
#                             ,"n_WA"           = item123.cast$n_WA
#                             ,"Mean_Region"    = item123.cast$Mean_Region
#                             ,"SE_Region"      = item123.cast$SE_Region
#                             ,"n_Region"       = item123.cast$n_Region
#                             ,"EB_ID"          = item123.cast$EB_ID
#                             ,"EB_MT"          = item123.cast$EB_MT
#                             ,"EB_OR"          = item123.cast$EB_OR
#                             ,"EB_WA"          = item123.cast$EB_WA
#                             ,"EB_Region"      = item123.cast$EB_Region
# )
# 
# item123.final.SF <- item123.table[which(item123.table$BuildingType == "Single Family")
#                                   ,-which(colnames(item123.table) %in% c("BuildingType"))]
# item123.final.MH <- item123.table[which(item123.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(item123.table) %in% c("BuildingType"))]
# 
# exportTable(item123.final.SF, "SF", "Table 130", weighted = TRUE)
# # exportTable(item123.final.MH, "MH", "Table 105", weighted = TRUE)
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# item123.summary <- mean_two_groups_unweighted(CustomerLevelData = item123.data
#                                    ,valueVariable    = 'Occupants'
#                                    ,byVariableRow    = 'Age.Category'
#                                    ,byVariableColumn = 'State'
#                                    ,columnAggregate  = "Region"
#                                    ,rowAggregate     = "Remove")
# item123.summary <- item123.summary[which(item123.summary$Age.Category != "Remove"),]
# item123.summary$Age.Category <- as.character(item123.summary$Age.Category)
# item123.summary$Age.Category[which(item123.summary$Age.Category == "Qty.Occupants")] <- "All Ages"
# 
# item123.cast <- item123.summary
# 
# item123.table <- data.frame("BuildingType"    = item123.cast$BuildingType
#                             ,"Age.Category"   = item123.cast$Age.Category
#                             ,"Mean_ID"        = item123.cast$Mean_ID
#                             ,"SE_ID"          = item123.cast$SE_ID
#                             ,"n_ID"           = item123.cast$n_ID
#                             ,"Mean_MT"        = item123.cast$Mean_MT
#                             ,"SE_MT"          = item123.cast$SE_MT
#                             ,"n_MT"           = item123.cast$n_MT
#                             ,"Mean_OR"        = item123.cast$Mean_OR
#                             ,"SE_OR"          = item123.cast$SE_OR
#                             ,"n_OR"           = item123.cast$n_OR
#                             ,"Mean_WA"        = item123.cast$Mean_WA
#                             ,"SE_WA"          = item123.cast$SE_WA
#                             ,"n_WA"           = item123.cast$n_WA
#                             ,"Mean_Region"    = item123.cast$Mean_Region
#                             ,"SE_Region"      = item123.cast$SE_Region
#                             ,"n_Region"       = item123.cast$n_Region
# )
# 
# item123.final.SF <- item123.table[which(item123.table$BuildingType == "Single Family")
#                                   ,-which(colnames(item123.table) %in% c("BuildingType"))]
# item123.final.MH <- item123.table[which(item123.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(item123.table) %in% c("BuildingType"))]
# 
# exportTable(item123.final.SF, "SF", "Table 130", weighted = FALSE)
# # exportTable(item123.final.MH, "MH", "Table 105", weighted = FALSE)


































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
#Item 122: AVERAGE NUMBER OF OCCUPANTS PER HOME BY CK_Building_ID (SF table 129, MH table 104)
#############################################################################################
#subset to columns needed for analysis
item122.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"Qty.Occupants"
                                                                                     ,""))])
item122.os.dat$count <- 1

#remove any repeat header rows from exporting
item122.os.dat0 <- item122.os.dat[which(item122.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned scl data
item122.os.dat1 <- left_join(os.dat, item122.os.dat0, by = "CK_Cadmus_ID")

item122.os.dat2 <- item122.os.dat1[which(!is.na(item122.os.dat1$Qty.Occupants)), ]

item122.os.merge <- item122.os.dat2

################################################
# Adding pop and sample sizes for weights
################################################
item122.os.data <- weightedData(item122.os.merge[-which(colnames(item122.os.merge) %in% c("Qty.Occupants"
                                                                                 ,"count"))])
item122.os.data <- left_join(item122.os.data, unique(item122.os.merge[which(colnames(item122.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Qty.Occupants"
                                                                                           ,"count"))]))

item122.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item122.os.final <- mean_one_group(item122.os.data
                                ,valueVariable = 'Qty.Occupants'
                                ,byVariable = 'CK_Building_ID'
                                ,aggregateRow = 'Remove')
item122.os.final <- item122.os.final[which(item122.os.final$CK_Building_ID != "Remove"),]

item122.os.final.SF <- item122.os.final[which(item122.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item122.os.final) %in% c("BuildingType"))]

exportTable(item122.os.final.SF, "SF", "Table 129", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item122.os.final <- mean_one_group_unweighted(item122.os.data
                                           ,valueVariable = 'Qty.Occupants'
                                           ,byVariable = 'CK_Building_ID'
                                           ,aggregateRow = 'Remove')
item122.os.final <- item122.os.final[which(item122.os.final$CK_Building_ID != "Remove"),]

item122.os.final.SF <- item122.os.final[which(item122.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item122.os.final) %in% c("BuildingType"))]

exportTable(item122.os.final.SF, "SF", "Table 129", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
#Item 123: AVERAGE NUMBER OF OCCUPANTS BY AGE CATEGORY BY CK_Building_ID (SF table 130, MH table 105)
#############################################################################################
#subset to columns needed for analysis
item123.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_LessThan1"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_1_5"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_6_10"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_11_18"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_19_45"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_46_64"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_65Older"
                                                                                     ,"Qty.Occupants"))])
colnames(item123.os.dat) <- c("CK_Cadmus_ID"
                           ,"Age_1_5"
                           ,"Age_11_18"
                           ,"Age_19_45"
                           ,"Age_46_64"
                           ,"Age_6_10"
                           ,"Age_65_Older"
                           ,"Age_Less_Than_1"
                           ,"Qty.Occupants")
item123.os.dat$count <- 1

#remove any repeat header rows from exporting
item123.os.dat0 <- item123.os.dat[which(item123.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned scl data
item123.os.dat1 <- left_join(item123.os.dat0, os.dat, by = "CK_Cadmus_ID")
str(item123.os.dat1)
item123.os.dat1$Age_1_5    <- as.numeric(as.character(item123.os.dat1$Age_1_5))
item123.os.dat1$Age_11_18    <- as.numeric(as.character(item123.os.dat1$Age_11_18))
item123.os.dat1$Age_19_45    <- as.numeric(as.character(item123.os.dat1$Age_19_45))
item123.os.dat1$Age_46_64    <- as.numeric(as.character(item123.os.dat1$Age_46_64))
item123.os.dat1$Age_6_10    <- as.numeric(as.character(item123.os.dat1$Age_6_10))
item123.os.dat1$Age_65_Older    <- as.numeric(as.character(item123.os.dat1$Age_65_Older))
item123.os.dat1$Age_Less_Than_1    <- as.numeric(as.character(item123.os.dat1$Age_Less_Than_1))

item123.os.dat1$Age_0_18 <- item123.os.dat1$Age_Less_Than_1 + item123.os.dat1$Age_1_5 + item123.os.dat1$Age_6_10 + item123.os.dat1$Age_11_18
item123.os.dat1$Age_19_64 <- item123.os.dat1$Age_19_45 + item123.os.dat1$Age_46_64

item123.os.dat2 <- item123.os.dat1[which(colnames(item123.os.dat1) %notin% c("Age_1_5"
                                                                    ,"Age_11_18"
                                                                    ,"Age_19_45"
                                                                    ,"Age_46_64"
                                                                    ,"Age_6_10"
                                                                    ,"Age_Less_Than_1"))]

item123.os.melt <- melt(item123.os.dat2, measure.vars = c("Age_0_18"
                                                    , "Age_19_64"
                                                    , "Age_65_Older"
                                                    , "Qty.Occupants"))
colnames(item123.os.melt)[which(colnames(item123.os.melt) == "variable")] <- "Age.Category"

item123.os.sum <- summarise(group_by(item123.os.melt, CK_Cadmus_ID, CK_Building_ID, Age.Category)
                         ,Occupants = sum(value, na.rm = T))

item123.os.merge <- left_join(os.dat, item123.os.sum)
item123.os.merge <- item123.os.merge[which(!is.na(item123.os.merge$Occupants)),]

################################################
# Adding pop and sample sizes for weights
################################################
item123.os.data <- weightedData(item123.os.merge[-which(colnames(item123.os.merge) %in% c("Age.Category"
                                                                                 ,"Occupants"))])
item123.os.data <- left_join(item123.os.data, unique(item123.os.merge[which(colnames(item123.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Age.Category"
                                                                                           ,"Occupants"))]))
item123.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item123.os.summary <- mean_two_groups(CustomerLevelData = item123.os.data
                                   ,valueVariable    = 'Occupants'
                                   ,byVariableRow    = 'Age.Category'
                                   ,byVariableColumn = 'CK_Building_ID'
                                   ,columnAggregate  = "Remove"
                                   ,rowAggregate     = "Remove")
item123.os.summary <- item123.os.summary[which(item123.os.summary$Age.Category != "Remove"),]
item123.os.summary$Age.Category <- as.character(item123.os.summary$Age.Category)
item123.os.summary$Age.Category[which(item123.os.summary$Age.Category == "Qty.Occupants")] <- "All Ages"

item123.os.cast <- item123.os.summary

if(os.ind == "scl"){
  item123.os.table <- data.frame("BuildingType"    = item123.os.cast$BuildingType
                                 ,"Age.Category"   = item123.os.cast$Age.Category
                                 ,"Mean_SCL.GenPop"      = item123.os.cast$`Mean_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item123.os.cast$`SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item123.os.cast$`n_SCL GenPop`
                                 ,"Mean_SCL.LI"          = item123.os.cast$`Mean_SCL LI`
                                 ,"SE_SCL.LI"            = item123.os.cast$`SE_SCL LI`
                                 ,"n_SCL.LI"             = item123.os.cast$`n_SCL LI`
                                 ,"Mean_SCL.EH"          = item123.os.cast$`Mean_SCL EH`
                                 ,"SE_SCL.EH"            = item123.os.cast$`SE_SCL EH`
                                 ,"n_SCL.EH"             = item123.os.cast$`n_SCL EH`
                                 ,"Mean_2017.RBSA.PS"    = item123.os.cast$`Mean_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item123.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item123.os.cast$`n_2017 RBSA PS`
                                 ,"EB_SCL.GenPop"        = item123.os.cast$`EB_SCL GenPop`
                                 ,"EB_SCL.LI"            = item123.os.cast$`EB_SCL LI`
                                 ,"EB_SCL.EH"            = item123.os.cast$`EB_SCL EH`
                                 ,"EB_2017.RBSA.PS"      = item123.os.cast$`EB_2017 RBSA PS`
  )

}else if(os.ind == "snopud"){
  item123.os.table <- data.frame("BuildingType"    = item123.os.cast$BuildingType
                                 ,"Age.Category"   = item123.os.cast$Age.Category
                                 ,"Mean_SnoPUD"          = item123.os.cast$`Mean_SnoPUD`
                                 ,"SE_SnoPUD"            = item123.os.cast$`SE_SnoPUD`
                                 ,"n_SnoPUD"             = item123.os.cast$`n_SnoPUD`
                                 ,"Mean_2017.RBSA.PS"    = item123.os.cast$`Mean_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item123.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item123.os.cast$`n_2017 RBSA PS`
                                 ,"Mean_RBSA.NW"         = item123.os.cast$`Mean_2017 RBSA NW`
                                 ,"SE_RBSA.NW"           = item123.os.cast$`SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"            = item123.os.cast$`n_2017 RBSA NW`
                                 ,"EB_SnoPUD"            = item123.os.cast$`EB_SnoPUD`
                                 ,"EB_2017.RBSA.PS"      = item123.os.cast$`EB_2017 RBSA PS`
                                 ,"EB_RBSA.NW"           = item123.os.cast$`EB_2017 RBSA NW`
  )

}


item123.os.final.SF <- item123.os.table[which(item123.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item123.os.table) %in% c("BuildingType"))]

exportTable(item123.os.final.SF, "SF", "Table 130", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item123.os.summary <- mean_two_groups_unweighted(CustomerLevelData = item123.os.data
                                              ,valueVariable    = 'Occupants'
                                              ,byVariableRow    = 'Age.Category'
                                              ,byVariableColumn = 'CK_Building_ID'
                                              ,columnAggregate  = "Remove"
                                              ,rowAggregate     = "Remove")
item123.os.summary <- item123.os.summary[which(item123.os.summary$Age.Category != "Remove"),]
item123.os.summary$Age.Category <- as.character(item123.os.summary$Age.Category)
item123.os.summary$Age.Category[which(item123.os.summary$Age.Category == "Qty.Occupants")] <- "All Ages"

item123.os.cast <- item123.os.summary

if(os.ind == "scl"){
  item123.os.table <- data.frame("BuildingType"    = item123.os.cast$BuildingType
                                 ,"Age.Category"   = item123.os.cast$Age.Category
                                 ,"Mean_SCL.GenPop"      = item123.os.cast$`Mean_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item123.os.cast$`SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item123.os.cast$`n_SCL GenPop`
                                 ,"Mean_SCL.LI"          = item123.os.cast$`Mean_SCL LI`
                                 ,"SE_SCL.LI"            = item123.os.cast$`SE_SCL LI`
                                 ,"n_SCL.LI"             = item123.os.cast$`n_SCL LI`
                                 ,"Mean_SCL.EH"          = item123.os.cast$`Mean_SCL EH`
                                 ,"SE_SCL.EH"            = item123.os.cast$`SE_SCL EH`
                                 ,"n_SCL.EH"             = item123.os.cast$`n_SCL EH`
                                 ,"Mean_2017.RBSA.PS"    = item123.os.cast$`Mean_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item123.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item123.os.cast$`n_2017 RBSA PS`
  )

}else if(os.ind == "snopud"){
  item123.os.table <- data.frame("BuildingType"    = item123.os.cast$BuildingType
                                 ,"Age.Category"   = item123.os.cast$Age.Category
                                 ,"Mean_SnoPUD"          = item123.os.cast$`Mean_SnoPUD`
                                 ,"SE_SnoPUD"            = item123.os.cast$`SE_SnoPUD`
                                 ,"n_SnoPUD"             = item123.os.cast$`n_SnoPUD`
                                 ,"Mean_2017.RBSA.PS"    = item123.os.cast$`Mean_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item123.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item123.os.cast$`n_2017 RBSA PS`
                                 ,"Mean_RBSA.NW"         = item123.os.cast$`Mean_2017 RBSA NW`
                                 ,"SE_RBSA.NW"           = item123.os.cast$`SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"            = item123.os.cast$`n_2017 RBSA NW`
  )

}
item123.os.final.SF <- item123.os.table[which(item123.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item123.os.table) %in% c("BuildingType"))]

exportTable(item123.os.final.SF, "SF", "Table 130", weighted = FALSE, osIndicator = export.ind, OS = T)
