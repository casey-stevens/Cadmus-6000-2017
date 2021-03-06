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
# mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))
#subset to columns needed for analysis
mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"Heating.Fuel"
                                                                        ,"Heating.Efficiency.-.High"
                                                                        ,"Component.1.Year.of.Manufacture"
                                                                        ,"HSPF"
                                                                        ,"Primary.Heating.System"
                                                                        ,"SEER"
                                                                        ,"Seasonal./.Portable.Equipment?"))]
#fix capitalization error
mechanical.dat1$Heating.Fuel[which(mechanical.dat1$Heating.Fuel == "Natural gas")] <- "Natural Gas"
mechanical.dat1 <- mechanical.dat1[which(mechanical.dat1$Primary.Heating.System == "Yes"),]
#remove any irrelevant equipment vintages (datapoint not asked for)
mechanical.dat2 <- mechanical.dat1#[which(mechanical.dat1$Component.1.Year.of.Manufacture != "-- Datapoint not asked for --"),]
#remove any NA equipment vintages
mechanical.dat3 <- mechanical.dat2


# #############################################################################################
# #TABLE AQ: 
# #############################################################################################
# #data for item 50
# tableAQ.dat <- mechanical.dat3[which(mechanical.dat3$CK_Cadmus_ID != "BUILDING"),]
# 
# #remove any irrelevant heating efficiency datapoints (datapoint not asked for)
# tableAQ.dat1 <- tableAQ.dat[which(tableAQ.dat$`Heating.Efficiency.-.High` %notin% c("-- Datapoint not asked for --","Datapoint not asked for")),]
# 
# #remove any repeated header lines
# tableAQ.dat2 <- tableAQ.dat1[which(tableAQ.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# #make heating efficiency information numeric
# tableAQ.dat2$`Heating.Efficiency.-.High` <- as.numeric(as.character(tableAQ.dat2$`Heating.Efficiency.-.High`))
# tableAQ.dat2$SEER <- as.numeric(as.character(tableAQ.dat2$SEER))
# tableAQ.dat2$HSPF <- as.numeric(as.character(tableAQ.dat2$HSPF))
# 
# #Join cleaned item 50 mechanical information with cleaned RBSA site information
# tableAQ.dat3 <- left_join(rbsa.dat, tableAQ.dat2, by = "CK_Cadmus_ID")
# tableAQ.dat4 <- tableAQ.dat3[which(!is.na(tableAQ.dat3$`Heating.Efficiency.-.High`)),]
# tableAQ.dat5 <- tableAQ.dat4[which(!is.na(tableAQ.dat4$SEER)),]
# tableAQ.dat6 <- tableAQ.dat5[which(!is.na(tableAQ.dat5$HSPF)),]
# 
# tableAQ.data <- weightedData(tableAQ.dat5[-which(colnames(tableAQ.dat5) %in% c("Generic"
#                                                                             ,"Heating.Fuel"
#                                                                             ,"Component.1.Year.of.Manufacture"
#                                                                             ,"Heating.Efficiency.-.High"
#                                                                             ,"HSPF"
#                                                                             ,"EquipVintage_bins"
#                                                                             ,"Primary.Heating.System"))])
# 
# tableAQ.data <- left_join(tableAQ.data, tableAQ.dat5[which(colnames(tableAQ.dat5) %in% c("CK_Cadmus_ID"
#                                                                                      ,"Generic"
#                                                                                      ,"Heating.Fuel"
#                                                                                      ,"Component.1.Year.of.Manufacture"
#                                                                                      ,"Heating.Efficiency.-.High"
#                                                                                      ,"EquipVintage_bins"
#                                                                                      ,"Primary.Heating.System"))])
# 
# tableAQ.data$`Heating.Efficiency.-.High` <- tableAQ.data$`Heating.Efficiency.-.High` / 100
# unique(tableAQ.data$`Heating.Efficiency.-.High`)
# 
# ###########################
# # Weighted Analysis
# ###########################
# tableAQ.final <- mean_two_groups(CustomerLevelData = tableAQ.data
#                                 ,valueVariable    = 'Heating.Efficiency.-.High'
#                                 ,byVariableRow    = 'EquipVintage_bins'
#                                 ,byVariableColumn = 'State'
#                                 ,columnAggregate  = "Region"
#                                 ,rowAggregate     = "All Vintages"
# )
# 
# 
# #subset to only the columns needed for the final RBSA table
# tableAQ.table <- data.frame("BuildingType"       = tableAQ.final$BuildingType
#                            ,"Equipment.Vintage" = tableAQ.final$EquipVintage_bins
#                            ,"Mean_ID"           = tableAQ.final$Mean_ID
#                            ,"SE_ID"             = tableAQ.final$SE_ID
#                            ,"n_ID"              = tableAQ.final$n_ID
#                            ,"Mean_MT"           = tableAQ.final$Mean_MT
#                            ,"SE_MT"             = tableAQ.final$SE_MT
#                            ,"n_MT"              = tableAQ.final$n_MT
#                            ,"Mean_OR"           = tableAQ.final$Mean_OR
#                            ,"SE_OR"             = tableAQ.final$SE_OR
#                            ,"n_OR"              = tableAQ.final$n_OR
#                            ,"Mean_WA"           = tableAQ.final$Mean_WA
#                            ,"SE_WA"             = tableAQ.final$SE_WA
#                            ,"n_WA"              = tableAQ.final$n_WA
#                            ,"Mean_Region"       = tableAQ.final$Mean_Region
#                            ,"SE_Region"         = tableAQ.final$SE_Region
#                            ,"n_Region"          = tableAQ.final$n_Region
#                            ,"EB_ID"             = tableAQ.final$EB_ID
#                            ,"EB_MT"             = tableAQ.final$EB_MT
#                            ,"EB_OR"             = tableAQ.final$EB_OR
#                            ,"EB_WA"             = tableAQ.final$EB_WA
#                            ,"EB_Region"         = tableAQ.final$EB_Region)
# 
# # row ordering example code
# levels(tableAQ.table$Equipment.Vintage)
# rowOrder <- c("Pre 1990"
#               ,"1990-1999"
#               ,"2000-2006"
#               ,"2007-2014"
#               ,"Post 2014"
#               ,"Vintage Unknown"
#               ,"All Vintages")
# tableAQ.table <- tableAQ.table %>% mutate(Equipment.Vintage = factor(Equipment.Vintage, levels = rowOrder)) %>% arrange(Equipment.Vintage)  
# tableAQ.table <- data.frame(tableAQ.table)
# 
# #subset to only the relevant building types for this item
# tableAQ.table.SF <- tableAQ.table[which(tableAQ.table$BuildingType == "Single Family"),-which(colnames(tableAQ.table) %in% c("BuildingType"))]
# tableAQ.table.MH <- tableAQ.table[which(tableAQ.table$BuildingType == "Manufactured") , -which(colnames(tableAQ.table) %in% c("BuildingType"))]
# 
# 
# exportTable(tableAQ.table.SF, "SF", "Table 57", weighted = TRUE)
# # exportTable(tableAQ.table.MH, "MH", "Table 38", weighted = TRUE)
# 
# 
# ###########################
# # Unweighted Analysis
# ###########################
# tableAQ.final <- mean_two_groups_unweighted(CustomerLevelData = tableAQ.data
#                                            ,valueVariable    = 'Heating.Efficiency.-.High'
#                                            ,byVariableRow    = 'EquipVintage_bins'
#                                            ,byVariableColumn = 'State'
#                                            ,columnAggregate  = "Region"
#                                            ,rowAggregate     = "All Vintages"
# )
# 
# 
# #subset to only the columns needed for the final RBSA table
# tableAQ.table <- data.frame("BuildingType"       = tableAQ.final$BuildingType
#                            ,"Equipment.Vintage" = tableAQ.final$EquipVintage_bins
#                            ,"Mean_ID"           = tableAQ.final$Mean_ID
#                            ,"SE_ID"             = tableAQ.final$SE_ID
#                            ,"n_ID"              = tableAQ.final$n_ID
#                            ,"Mean_MT"           = tableAQ.final$Mean_MT
#                            ,"SE_MT"             = tableAQ.final$SE_MT
#                            ,"n_MT"              = tableAQ.final$n_MT
#                            ,"Mean_OR"           = tableAQ.final$Mean_OR
#                            ,"SE_OR"             = tableAQ.final$SE_OR
#                            ,"n_OR"              = tableAQ.final$n_OR
#                            ,"Mean_WA"           = tableAQ.final$Mean_WA
#                            ,"SE_WA"             = tableAQ.final$SE_WA
#                            ,"n_WA"              = tableAQ.final$n_WA
#                            ,"Mean_Region"       = tableAQ.final$Mean_Region
#                            ,"SE_Region"         = tableAQ.final$SE_Region
#                            ,"n_Region"          = tableAQ.final$n_Region)
# 
# # row ordering example code
# levels(tableAQ.table$Equipment.Vintage)
# rowOrder <- c("Pre 1990"
#               ,"1990-1999"
#               ,"2000-2006"
#               ,"2007-2014"
#               ,"Post 2014"
#               ,"Vintage Unknown"
#               ,"All Vintages")
# tableAQ.table <- tableAQ.table %>% mutate(Equipment.Vintage = factor(Equipment.Vintage, levels = rowOrder)) %>% arrange(Equipment.Vintage)  
# tableAQ.table <- data.frame(tableAQ.table)
# 
# 
# #subset to only the relevant building types for this item
# tableAQ.table.SF <- tableAQ.table[which(tableAQ.table$BuildingType == "Single Family"),-which(colnames(tableAQ.table) %in% c("BuildingType"))]
# tableAQ.table.MH <- tableAQ.table[which(tableAQ.table$BuildingType == "Manufactured"), -which(colnames(tableAQ.table) %in% c("BuildingType"))]
# 
# 
# exportTable(tableAQ.table.SF, "SF", "Table 57", weighted = FALSE)
# # exportTable(tableAQ.table.MH, "MH", "Table 38", weighted = FALSE)
































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
#TABLE AQ:
#############################################################################################
#data for item 50
tableAQ.os.dat <- mechanical.dat3[which(mechanical.dat3$CK_Cadmus_ID != "BUILDING"),]

#remove any irrelevant heating efficiency datapoints (datapoint not asked for)
tableAQ.os.dat1 <- tableAQ.os.dat[which(tableAQ.os.dat$`Heating.Efficiency.-.High` %notin% c("-- Datapoint not asked for --","Datapoint not asked for")),]

#remove any repeated header lines
tableAQ.os.dat2 <- tableAQ.os.dat1[which(tableAQ.os.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#make heating efficiency information numeric
tableAQ.os.dat2$`Heating.Efficiency.-.High` <- as.numeric(as.character(tableAQ.os.dat2$`Heating.Efficiency.-.High`))
tableAQ.os.dat2$SEER <- as.numeric(as.character(tableAQ.os.dat2$SEER))
tableAQ.os.dat2$HSPF <- as.numeric(as.character(tableAQ.os.dat2$HSPF))

#Join cleaned item 50 mechanical information with cleaned RBSA site information
tableAQ.os.dat3 <- left_join(os.dat, tableAQ.os.dat2, by = "CK_Cadmus_ID")
tableAQ.os.dat4 <- tableAQ.os.dat3[which(!is.na(tableAQ.os.dat3$`Heating.Efficiency.-.High`)),]
tableAQ.os.dat5 <- tableAQ.os.dat4[which(!is.na(tableAQ.os.dat4$SEER)),]
tableAQ.os.dat6 <- tableAQ.os.dat5[which(!is.na(tableAQ.os.dat5$HSPF)),]

tableAQ.os.data <- weightedData(tableAQ.os.dat5[-which(colnames(tableAQ.os.dat5) %in% c("Generic"
                                                                                        ,"Seasonal./.Portable.Equipment?"
                                                                                        ,"Heating.Fuel"
                                                                                        ,"Component.1.Year.of.Manufacture"
                                                                                        ,"Heating.Efficiency.-.High"
                                                                                        ,"SEER"
                                                                                        ,"HSPF"
                                                                                        ,"EquipVintage_bins"
                                                                                        ,"Primary.Heating.System"))])

tableAQ.os.data <- left_join(tableAQ.os.data, tableAQ.os.dat5[which(colnames(tableAQ.os.dat5) %in% c("CK_Cadmus_ID"
                                                                                         ,"Generic"
                                                                                         ,"Seasonal./.Portable.Equipment?"
                                                                                         ,"Heating.Fuel"
                                                                                         ,"Component.1.Year.of.Manufacture"
                                                                                         ,"Heating.Efficiency.-.High"
                                                                                         ,"SEER"
                                                                                         ,"HSPF"
                                                                                         ,"EquipVintage_bins"
                                                                                         ,"Primary.Heating.System"))])

tableAQ.os.data$`Heating.Efficiency.-.High` <- tableAQ.os.data$`Heating.Efficiency.-.High` / 100
unique(tableAQ.os.data$`Heating.Efficiency.-.High`)

###########################
# Weighted Analysis
###########################
tableAQ.os.final <- mean_two_groups(CustomerLevelData = tableAQ.os.data
                                 ,valueVariable    = 'Heating.Efficiency.-.High'
                                 ,byVariableRow    = 'EquipVintage_bins'
                                 ,byVariableColumn = 'CK_Building_ID'
                                 ,columnAggregate  = "Remove"
                                 ,rowAggregate     = "All Vintages"
)


#subset to only the columns needed for the final RBSA table
tableAQ.os.table <- data.frame("BuildingType"       = tableAQ.os.final$BuildingType
                            ,"Equipment.Vintage" = tableAQ.os.final$EquipVintage_bins
                            ,"Mean_SCL.GenPop"      = tableAQ.os.cast$Mean_SCL.GenPop
                            ,"SE_SCL.GenPop"        = tableAQ.os.cast$SE_SCL.GenPop
                            ,"n_SCL.GenPop"         = tableAQ.os.cast$n_SCL.GenPop
                            ,"Mean_SCL.LI"          = tableAQ.os.cast$Mean_SCL.LI
                            ,"SE_SCL.LI"            = tableAQ.os.cast$SE_SCL.LI
                            ,"n_SCL.LI"             = tableAQ.os.cast$n_SCL.LI
                            ,"Mean_SCL.EH"          = tableAQ.os.cast$Mean_SCL.EH
                            ,"SE_SCL.EH"            = tableAQ.os.cast$SE_SCL.EH
                            ,"n_SCL.EH"             = tableAQ.os.cast$n_SCL.EH
                            ,"Mean_2017.RBSA.PS"    = tableAQ.os.cast$Mean_2017.RBSA.PS
                            ,"SE_2017.RBSA.PS"      = tableAQ.os.cast$SE_2017.RBSA.PS
                            ,"n_2017.RBSA.PS"       = tableAQ.os.cast$n_2017.RBSA.PS
                            ,"EB_SCL.GenPop"        = tableAQ.os.cast$EB_SCL.GenPop
                            ,"EB_SCL.LI"            = tableAQ.os.cast$EB_SCL.LI
                            ,"EB_SCL.EH"            = tableAQ.os.cast$EB_SCL.EH
                            ,"EB_2017.RBSA.PS"      = tableAQ.os.cast$EB_2017.RBSA.PS)

# row ordering example code
levels(tableAQ.os.table$Equipment.Vintage)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2006"
              ,"2007-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
tableAQ.os.table <- tableAQ.os.table %>% mutate(Equipment.Vintage = factor(Equipment.Vintage, levels = rowOrder)) %>% arrange(Equipment.Vintage)
tableAQ.os.table <- data.frame(tableAQ.os.table)

#subset to only the relevant building types for this item
tableAQ.os.table.SF <- tableAQ.os.table[which(tableAQ.os.table$BuildingType == "Single Family"),-which(colnames(tableAQ.os.table) %in% c("BuildingType"))]

###########################
# Unweighted Analysis
###########################
tableAQ.os.final <- mean_two_groups_unweighted(CustomerLevelData = tableAQ.os.data
                                            ,valueVariable    = 'Heating.Efficiency.-.High'
                                            ,byVariableRow    = 'EquipVintage_bins'
                                            ,byVariableColumn = 'CK_Building_ID'
                                            ,columnAggregate  = "Remove"
                                            ,rowAggregate     = "All Vintages"
)


#subset to only the columns needed for the final RBSA table
tableAQ.os.table <- data.frame("BuildingType"       = tableAQ.os.final$BuildingType
                            ,"Equipment.Vintage" = tableAQ.os.final$EquipVintage_bins
                            ,"Mean_SCL.GenPop"      = tableAQ.os.cast$Mean_SCL.GenPop
                            ,"SE_SCL.GenPop"        = tableAQ.os.cast$SE_SCL.GenPop
                            ,"n_SCL.GenPop"         = tableAQ.os.cast$n_SCL.GenPop
                            ,"Mean_SCL.LI"          = tableAQ.os.cast$Mean_SCL.LI
                            ,"SE_SCL.LI"            = tableAQ.os.cast$SE_SCL.LI
                            ,"n_SCL.LI"             = tableAQ.os.cast$n_SCL.LI
                            ,"Mean_SCL.EH"          = tableAQ.os.cast$Mean_SCL.EH
                            ,"SE_SCL.EH"            = tableAQ.os.cast$SE_SCL.EH
                            ,"n_SCL.EH"             = tableAQ.os.cast$n_SCL.EH
                            ,"Mean_2017.RBSA.PS"    = tableAQ.os.cast$Mean_2017.RBSA.PS
                            ,"SE_2017.RBSA.PS"      = tableAQ.os.cast$SE_2017.RBSA.PS
                            ,"n_2017.RBSA.PS"       = tableAQ.os.cast$n_2017.RBSA.PS)

# row ordering example code
levels(tableAQ.os.table$Equipment.Vintage)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2006"
              ,"2007-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
tableAQ.os.table <- tableAQ.os.table %>% mutate(Equipment.Vintage = factor(Equipment.Vintage, levels = rowOrder)) %>% arrange(Equipment.Vintage)
tableAQ.os.table <- data.frame(tableAQ.os.table)


#subset to only the relevant building types for this item
tableAQ.os.table.SF <- tableAQ.os.table[which(tableAQ.os.table$BuildingType == "Single Family"),-which(colnames(tableAQ.os.table) %in% c("BuildingType"))]
