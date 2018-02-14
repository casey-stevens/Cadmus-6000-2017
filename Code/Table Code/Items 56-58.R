#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:          11/3/2017                                   
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

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
download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx', mechanical.export, mode = 'wb')
mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))
#subset to columns needed for analysis
mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"SEER"
                                                                        ,"Component.1.Year.of.Manufacture"
                                                                        ,"Seasonal./.Portable.Equipment?"))]
unique(mechanical.dat1$Component.1.Year.of.Manufacture)
#remove any irrelevant equipment vintages (datapoint not asked for)
mechanical.dat2 <- mechanical.dat1#[which(mechanical.dat1$Component.1.Year.of.Manufacture != "Datapoint not asked for"),]
#remove any NA equipment vintages
mechanical.dat2$Component.1.Year.of.Manufacture <- as.numeric(as.character(mechanical.dat2$Component.1.Year.of.Manufacture))
# mechanical.dat2[which(is.na(mechanical.dat2$Component.1.Year.of.Manufacture))] <- "Vintage Unknown"
mechanical.dat3 <- mechanical.dat2

# Bin equipment vintages for items 50 and 52 (4 categories)
mechanical.dat3$EquipVintage_bins <- "Vintage Unknown"
mechanical.dat3$EquipVintage_bins_MH <- "Vintage Unknown"
# # Bin equipment vintages for items 50 and 52 (4 categories)
# mechanical.dat3$EquipVintage_bins <- as.numeric(as.character(mechanical.dat3$`Component.1.Year.of.Manufacture`))
# mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` < 1990)] <- "Pre 1990"
# mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 1990 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2000)] <- "1990-1999"
# mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2000 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2006)] <- "2000-2006"
# mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2006)] <- "Post 2006"

# mechanical.dat3$EquipVintage_bins <- as.numeric(as.character(mechanical.dat3$`Component.1.Year.of.Manufacture`))
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` < 1990)] <- "Pre 1990"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 1990 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2000)] <- "1990-1999"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2000 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2007)] <- "2000-2006"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2007 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2015)] <- "2007-2014"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2015)] <- "Post 2014"

#check uniques
unique(mechanical.dat3$EquipVintage_bins)

# mechanical.dat3$EquipVintage_bins_MH <- as.numeric(as.character(mechanical.dat3$`Component.1.Year.of.Manufacture`))
mechanical.dat3$EquipVintage_bins_MH[which(mechanical.dat3$`Component.1.Year.of.Manufacture` < 1990)] <- "Pre 1990"
mechanical.dat3$EquipVintage_bins_MH[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 1990 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2000)] <- "1990-1999"
mechanical.dat3$EquipVintage_bins_MH[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2000 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2006)] <- "2000-2005"
mechanical.dat3$EquipVintage_bins_MH[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2006 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2015)] <- "2006-2014"
mechanical.dat3$EquipVintage_bins_MH[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2015)] <- "Post 2014"

#check uniques
unique(mechanical.dat3$EquipVintage_bins_MH)








#############################################################################################
#Item 56: AVERAGE COOLING EFFICIENCY (SEER) FOR CENTRAL AC SYSTEMS BY VINTAGE (SF table 63)
#############################################################################################
#check unique SEER values
unique(mechanical.dat3$SEER)
#check unique Generic values (need to subset to only Central AC Systems)
unique(mechanical.dat3$Generic)

#data for item 56
item56.dat <- mechanical.dat3[which(mechanical.dat3$Generic == "Central AC"),]

#remove any irrelevant SEER datapoints (could not collect)
item56.dat1 <- item56.dat[which(item56.dat$SEER != "Could Not Collect"),]

#remove any repeated header lines
item56.dat2 <- item56.dat1[which(item56.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#make SEER information numeric
item56.dat2$SEER <- as.numeric(as.character(item56.dat2$SEER))

#remove any NAs in SEER
item56.dat3 <- item56.dat2[which(!(is.na(item56.dat2$SEER))),]
item56.dat3$count <- 1
item56.customer <- summarise(group_by(item56.dat3
                                     , CK_Cadmus_ID
                                     , EquipVintage_bins
                                     , EquipVintage_bins_MH)
                            ,y_bar_ilk  = mean(SEER)
                            ,y_ilk      = sum(SEER)
                            ,m_ilk      = sum(count)
)

#Join cleaned item 56 mechanical information with cleaned RBSA site information
item56.dat4 <- left_join(rbsa.dat, item56.customer, by = "CK_Cadmus_ID")
item56.dat5 <- item56.dat4[-which(is.na(item56.dat4$y_bar_ilk)), ]


# Weighting
item56.data <- weightedData(item56.dat5[-which(colnames(item56.dat5) %in% c("Generic"
                                                                            ,"Seasonal./.Portable.Equipment?"
                                                                            ,"Component.1.Year.of.Manufacture"
                                                                            ,"SEER"
                                                                            ,"EquipVintage_bins"
                                                                            ,"EquipVintage_bins_MH"
                                                                            ,"y_bar_ilk"
                                                                            ,"y_ilk"
                                                                            ,"m_ilk"))])

item56.data <- left_join(item56.data, item56.dat5[which(colnames(item56.dat5) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"Seasonal./.Portable.Equipment?"
                                                                                     ,"Component.1.Year.of.Manufacture"
                                                                                     ,"SEER"
                                                                                     ,"EquipVintage_bins"
                                                                                     ,"EquipVintage_bins_MH"
                                                                                     ,"y_bar_ilk"
                                                                                     ,"y_ilk"
                                                                                     ,"m_ilk"))])

item56.data$count <-1
################################
# Weighted Analysis - single family
################################
item56.final <- mean_one_group_domain(CustomerLevelData = item56.data
                               ,valueVariable    = 'y_bar_ilk'
                               ,byVariable       = 'EquipVintage_bins'
                               ,aggregateRow     = 'All Vintages')

# Export table
item56.final.SF <- item56.final[which(item56.final$BuildingType == "Single Family"),-1]

exportTable(item56.final.SF, "SF", "Table 63", weighted = TRUE)

################################
# Weighted Analysis - manufactured
################################
item56.data$count <-1
item56.final <- mean_one_group_domain(CustomerLevelData = item56.data
                                      ,valueVariable    = 'y_bar_ilk'
                                      ,byVariable       = 'EquipVintage_bins_MH'
                                      ,aggregateRow     = 'All Vintages')

# Export table
item56.final.MH <- item56.final[which(item56.final$BuildingType == "Manufactured"),-1]

# exportTable(item56.final.MH, "MH", "Table 43", weighted = TRUE)



################################
# Unweighted Analysis
################################
item56.final <- mean_one_group_unweighted(CustomerLevelData = item56.data
                               ,valueVariable    = 'y_bar_ilk'
                               ,byVariable       = 'EquipVintage_bins'
                               ,aggregateRow     = 'All Vintages')

# Export table
item56.final.SF <- item56.final[which(item56.final$BuildingType == "Single Family"),-1]

exportTable(item56.final.SF, "SF", "Table 63", weighted = FALSE)

################################
# Unweighted Analysis
################################
item56.final <- mean_one_group_unweighted(CustomerLevelData = item56.data
                                          ,valueVariable    = 'y_bar_ilk'
                                          ,byVariable       = 'EquipVintage_bins_MH'
                                          ,aggregateRow     = 'All Vintages')

# Export table
item56.final.MH <- item56.final[which(item56.final$BuildingType == "Manufactured"),-1]

# exportTable(item56.final.MH, "MH", "Table 43", weighted = FALSE)






#############################################################################################
#Item 57: AVERAGE COOLING EFFICIENCY (SEER) FOR CENTRAL AIR SOURCE HEAT PUMP SYSTEMS BY VINTAGE (SF table 64, MH table 44)
#############################################################################################
#check unique SEER values
unique(mechanical.dat3$SEER)
#check unique Generic values (need to subset to only Central AC Systems)
unique(mechanical.dat3$Generic)

#data for item 57
item57.dat <- mechanical.dat3[which(mechanical.dat3$Generic == "Air Source Heat Pump"),]

#remove any irrelevant SEER datapoints (could not collect)
item57.dat1 <- item57.dat[which(item57.dat$SEER != "Could Not Collect"),]

#remove any repeated header lines
item57.dat2 <- item57.dat1[which(item57.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#make SEER information numeric
item57.dat2$SEER <- as.numeric(as.character(item57.dat2$SEER))

#remove any NAs in SEER
item57.dat3 <- item57.dat2[which(!(is.na(item57.dat2$SEER))),]

item57.dat3$count <- 1
item57.customer <- summarise(group_by(item57.dat3
                                      , CK_Cadmus_ID
                                      , EquipVintage_bins
                                      , EquipVintage_bins_MH)
                             ,y_bar_ilk  = mean(SEER)
                             ,y_ilk      = sum(SEER)
                             ,m_ilk      = sum(count)
)


#Join cleaned item 57 mechanical information with cleaned RBSA site information
item57.dat4 <- left_join(rbsa.dat, item57.customer, by = "CK_Cadmus_ID")
# Remove duplicates
item57.dat5 <- item57.dat4[-which(is.na(item57.dat4$y_bar_ilk)), ]


# Weighting
item57.data <- weightedData(item57.dat5[-which(colnames(item57.dat5) %in% c("Generic"
                                                                            ,"Seasonal./.Portable.Equipment?"
                                                                            ,"Component.1.Year.of.Manufacture"
                                                                            ,"SEER"
                                                                            ,"EquipVintage_bins"
                                                                            ,"EquipVintage_bins_MH"
                                                                            ,"y_bar_ilk"
                                                                            ,"y_ilk"
                                                                            ,"m_ilk"))])

item57.data <- left_join(item57.data, item57.dat5[which(colnames(item57.dat5) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"Seasonal./.Portable.Equipment?"
                                                                                     ,"Component.1.Year.of.Manufacture"
                                                                                     ,"SEER"
                                                                                     ,"EquipVintage_bins"
                                                                                     ,"EquipVintage_bins_MH"
                                                                                     ,"y_bar_ilk"
                                                                                     ,"y_ilk"
                                                                                     ,"m_ilk"))])
item57.data$count <- 1

########################
# Weighted Analysis - single family
########################
item57.final <- mean_one_group_domain(CustomerLevelData = item57.data
                               ,valueVariable    = 'y_bar_ilk'
                               ,byVariable       = 'EquipVintage_bins'
                               ,aggregateRow     = 'All Vintages')

# row ordering example code
unique(item57.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2006"
              ,"2007-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item57.final <- item57.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item57.final <- data.frame(item57.final)


# Export table
item57.final.SF <- item57.final[which(item57.final$BuildingType == "Single Family"),-1]

exportTable(item57.final.SF, "SF", "Table 64",weighted = TRUE)

########################
# Weighted Analysis - manufactured
########################
item57.final <- mean_one_group_domain(CustomerLevelData = item57.data
                                      ,valueVariable    = 'y_bar_ilk'
                                      ,byVariable       = 'EquipVintage_bins_MH'
                                      ,aggregateRow     = 'All Vintages')

# row ordering example code
unique(item57.final$EquipVintage_bins_MH)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2005"
              ,"2006-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item57.final <- item57.final %>% mutate(EquipVintage_bins_MH = factor(EquipVintage_bins_MH, levels = rowOrder)) %>% arrange(EquipVintage_bins_MH)  
item57.final <- data.frame(item57.final)


# Export table
item57.final.MH <- item57.final[which(item57.final$BuildingType == "Manufactured"),-1]

# exportTable(item57.final.MH, "MH", "Table 44",weighted = TRUE)



########################
# Unweighted Analysis - single family
########################
item57.final <- mean_one_group_unweighted(CustomerLevelData = item57.data
                               ,valueVariable    = 'y_bar_ilk'
                               ,byVariable       = 'EquipVintage_bins'
                               ,aggregateRow     = 'All Vintages')

# row ordering example code
unique(item57.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2006"
              ,"2007-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item57.final <- item57.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item57.final <- data.frame(item57.final)

# Export table
item57.final.SF <- item57.final[which(item57.final$BuildingType == "Single Family"),-1]

exportTable(item57.final.SF, "SF", "Table 64",weighted = FALSE)

########################
# Unweighted Analysis - manufactured
########################
item57.final <- mean_one_group_unweighted(CustomerLevelData = item57.data
                                          ,valueVariable    = 'y_bar_ilk'
                                          ,byVariable       = 'EquipVintage_bins_MH'
                                          ,aggregateRow     = 'All Vintages')

# row ordering example code
unique(item57.final$EquipVintage_bins_MH)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2005"
              ,"2006-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item57.final <- item57.final %>% mutate(EquipVintage_bins_MH = factor(EquipVintage_bins_MH, levels = rowOrder)) %>% arrange(EquipVintage_bins_MH)  
item57.final <- data.frame(item57.final)

# Export table
item57.final.MH <- item57.final[which(item57.final$BuildingType == "Manufactured"),-1]

# exportTable(item57.final.MH, "MH", "Table 44",weighted = FALSE)

 





#############################################################################################
#Item 58: AVERAGE NUMBER OF PORTABLE COOLING DEVICES PER HOME BY STATE (SF table 65, MH table 45)
#############################################################################################
#check unique SEER values
unique(mechanical.dat1$`Seasonal./.Portable.Equipment?`)

#subset to only seasonal / portable = YES
item58.dat <- mechanical.dat1[which(mechanical.dat1$`Seasonal./.Portable.Equipment?` == "Yes"),]

#check unique Generic values
unique(item58.dat$Generic)

item58.dat2 <- left_join(rbsa.dat, item58.dat, by = "CK_Cadmus_ID")

item58.dat2$`Seasonal./.Portable.Equipment?`[which(is.na(item58.dat2$`Seasonal./.Portable.Equipment?`))] <- "No"

item58.dat2$Ind <- 0
item58.dat2$Ind[which(item58.dat2$`Seasonal./.Portable.Equipment?` == "Yes")] <- 1


# Weighting
item58.data <- weightedData(item58.dat2[-which(colnames(item58.dat2) %in% c("Generic"
                                                                            ,"Seasonal./.Portable.Equipment?"
                                                                            ,"Component.1.Year.of.Manufacture"
                                                                            ,"SEER"
                                                                            ,"Ind"))])

item58.data <- left_join(item58.data, item58.dat2[which(colnames(item58.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"Seasonal./.Portable.Equipment?"
                                                                                     ,"Component.1.Year.of.Manufacture"
                                                                                     ,"SEER"
                                                                                     ,"Ind"))])
item58.data$count <- 1
item58.data$Ind <- as.numeric(as.character(item58.data$Ind))

########################
# Weighted Analysis
########################
item58.final <- mean_one_group(CustomerLevelData = item58.data
                               ,valueVariable    = 'Ind'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')

# Export table
item58.final.SF <- item58.final[which(item58.final$BuildingType == "Single Family"),-1]
item58.final.MH <- item58.final[which(item58.final$BuildingType == "Manufactured"),-1]

exportTable(item58.final.SF, "SF", "Table 65", weighted = TRUE)
# exportTable(item58.final.MH, "MH", "Table 45", weighted = TRUE)



########################
# Weighted Analysis
########################
item58.final <- mean_one_group_unweighted(CustomerLevelData = item58.data
                               ,valueVariable    = 'Ind'
                               ,byVariable       = 'State'
                               ,aggregateRow     = 'Region')


# Export table
# SF = Table 64, MH = Table 44
item58.final.SF <- item58.final[which(item58.final$BuildingType == "Single Family"),-1]
item58.final.MH <- item58.final[which(item58.final$BuildingType == "Manufactured"),-1]

exportTable(item58.final.SF, "SF", "Table 65", weighted = FALSE)
# exportTable(item58.final.MH, "MH", "Table 45", weighted = FALSE)























############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
scl.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.scl.data", rundate, ".xlsx", sep = "")))
length(unique(scl.dat$CK_Cadmus_ID))
scl.dat$CK_Building_ID <- scl.dat$Category
scl.dat <- scl.dat[which(names(scl.dat) != "Category")]

#############################################################################################
#Item 56: AVERAGE COOLING EFFICIENCY (SEER) FOR CENTRAL AC SYSTEMS BY VINTAGE (SF table 63)
#############################################################################################
#check unique SEER values
unique(mechanical.dat3$SEER)
#check unique Generic values (need to subset to only Central AC Systems)
unique(mechanical.dat3$Generic)

#data for item 56
item56.os.dat <- mechanical.dat3[which(mechanical.dat3$Generic == "Central AC"),]

#remove any irrelevant SEER datapoints (could not collect)
item56.os.dat1 <- item56.os.dat[which(item56.os.dat$SEER != "Could Not Collect"),]

#remove any repeated header lines
item56.os.dat2 <- item56.os.dat1[which(item56.os.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#make SEER information numeric
item56.os.dat2$SEER <- as.numeric(as.character(item56.os.dat2$SEER))

#remove any NAs in SEER
item56.os.dat3 <- item56.os.dat2[which(!(is.na(item56.os.dat2$SEER))),]
item56.os.dat3$count <- 1
item56.os.customer <- summarise(group_by(item56.os.dat3
                                      , CK_Cadmus_ID
                                      , EquipVintage_bins
                                      , EquipVintage_bins_MH)
                             ,y_bar_ilk  = mean(SEER)
                             ,y_ilk      = sum(SEER)
                             ,m_ilk      = sum(count)
)

#Join cleaned item 56 mechanical information with cleaned scl site information
item56.os.dat4 <- left_join(scl.dat, item56.os.customer, by = "CK_Cadmus_ID")
item56.os.dat5 <- item56.os.dat4[-which(is.na(item56.os.dat4$y_bar_ilk)), ]


# Weighting
item56.os.data <- weightedData(item56.os.dat5[-which(colnames(item56.os.dat5) %in% c("Generic"
                                                                            ,"Seasonal./.Portable.Equipment?"
                                                                            ,"Component.1.Year.of.Manufacture"
                                                                            ,"SEER"
                                                                            ,"EquipVintage_bins"
                                                                            ,"EquipVintage_bins_MH"
                                                                            ,"y_bar_ilk"
                                                                            ,"y_ilk"
                                                                            ,"m_ilk"))])

item56.os.data <- left_join(item56.os.data, item56.os.dat5[which(colnames(item56.os.dat5) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"Seasonal./.Portable.Equipment?"
                                                                                     ,"Component.1.Year.of.Manufacture"
                                                                                     ,"SEER"
                                                                                     ,"EquipVintage_bins"
                                                                                     ,"EquipVintage_bins_MH"
                                                                                     ,"y_bar_ilk"
                                                                                     ,"y_ilk"
                                                                                     ,"m_ilk"))])

item56.os.data$count <-1
################################
# Weighted Analysis - single family
################################
item56.os.cast <- mean_two_groups(CustomerLevelData = item56.os.data
                                   ,valueVariable    = 'y_bar_ilk'
                                   ,byVariableColumn = "CK_Building_ID"
                                   ,byVariableRow    = 'EquipVintage_bins'
                                   ,rowAggregate     = 'All Vintages'
                                   ,columnAggregate  = "Remove")

item56.os.table <- data.frame("BuildingType"          = item56.os.cast$BuildingType
                              ,"Equipment.Vintage"    = item56.os.cast$EquipVintage_bins
                              ,"Mean_SCL.GenPop"      = item56.os.cast$`Mean_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item56.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item56.os.cast$`n_SCL GenPop`
                              ,"Mean_SCL.LI"          = item56.os.cast$`Mean_SCL LI`
                              ,"SE_SCL.LI"            = item56.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = item56.os.cast$`n_SCL LI`
                              ,"Mean_SCL.EH"          = NA#item56.os.cast$`Mean_SCL EH`
                              ,"SE_SCL.EH"            = NA#item56.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = NA#item56.os.cast$`n_SCL EH`
                              ,"Mean_2017.RBSA.PS"    = item56.os.cast$`Mean_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item56.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item56.os.cast$`n_2017 RBSA PS`
                              ,"EB_SCL.GenPop"        = item56.os.cast$`EB_SCL GenPop`
                              ,"EB_SCL.LI"            = item56.os.cast$`EB_SCL LI`
                              ,"EB_SCL.EH"            = NA#item56.os.cast$`EB_SCL EH`
                              ,"EB_2017.RBSA.PS"      = item56.os.cast$`EB_2017 RBSA PS`)


# Export table
item56.os.final.SF <- item56.os.table[which(item56.os.table$BuildingType == "Single Family"),-1]

exportTable(item56.os.final.SF, "SF", "Table 63", weighted = TRUE, osIndicator = "SCL", OS = T)

################################
# Unweighted Analysis
################################
item56.os.cast <- mean_two_groups_unweighted(CustomerLevelData = item56.os.data
                                  ,valueVariable    = 'y_bar_ilk'
                                  ,byVariableColumn = "CK_Building_ID"
                                  ,byVariableRow    = 'EquipVintage_bins'
                                  ,rowAggregate     = 'All Vintages'
                                  ,columnAggregate  = "Remove")

item56.os.table <- data.frame("BuildingType"          = item56.os.cast$BuildingType
                              ,"Equipment.Vintage"    = item56.os.cast$EquipVintage_bins
                              ,"Mean_SCL.GenPop"      = item56.os.cast$`Mean_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item56.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item56.os.cast$`n_SCL GenPop`
                              ,"Mean_SCL.LI"          = item56.os.cast$`Mean_SCL LI`
                              ,"SE_SCL.LI"            = item56.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = item56.os.cast$`n_SCL LI`
                              ,"Mean_SCL.EH"          = NA#item56.os.cast$`Mean_SCL EH`
                              ,"SE_SCL.EH"            = NA#item56.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = NA#item56.os.cast$`n_SCL EH`
                              ,"Mean_2017.RBSA.PS"    = item56.os.cast$`Mean_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item56.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item56.os.cast$`n_2017 RBSA PS`)


# Export table
item56.os.final.SF <- item56.os.table[which(item56.os.table$BuildingType == "Single Family"),-1]

exportTable(item56.os.final.SF, "SF", "Table 63", weighted = FALSE, osIndicator = "SCL", OS = T)





#############################################################################################
#Item 57: AVERAGE COOLING EFFICIENCY (SEER) FOR CENTRAL AIR SOURCE HEAT PUMP SYSTEMS BY VINTAGE (SF table 64, MH table 44)
#############################################################################################
#check unique SEER values
unique(mechanical.dat3$SEER)
#check unique Generic values (need to subset to only Central AC Systems)
unique(mechanical.dat3$Generic)

#data for item 57
item57.os.dat <- mechanical.dat3[which(mechanical.dat3$Generic == "Air Source Heat Pump"),]

#remove any irrelevant SEER datapoints (could not collect)
item57.os.dat1 <- item57.os.dat[which(item57.os.dat$SEER != "Could Not Collect"),]

#remove any repeated header lines
item57.os.dat2 <- item57.os.dat1[which(item57.os.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#make SEER information numeric
item57.os.dat2$SEER <- as.numeric(as.character(item57.os.dat2$SEER))

#remove any NAs in SEER
item57.os.dat3 <- item57.os.dat2[which(!(is.na(item57.os.dat2$SEER))),]

item57.os.dat3$count <- 1
item57.os.customer <- summarise(group_by(item57.os.dat3
                                      , CK_Cadmus_ID
                                      , EquipVintage_bins
                                      , EquipVintage_bins_MH)
                             ,y_bar_ilk  = mean(SEER)
                             ,y_ilk      = sum(SEER)
                             ,m_ilk      = sum(count)
)


#Join cleaned item 57 mechanical information with cleaned scl site information
item57.os.dat4 <- left_join(scl.dat, item57.os.customer, by = "CK_Cadmus_ID")
# Remove duplicates
item57.os.dat5 <- item57.os.dat4[-which(is.na(item57.os.dat4$y_bar_ilk)), ]


# Weighting
item57.os.data <- weightedData(item57.os.dat5[-which(colnames(item57.os.dat5) %in% c("Generic"
                                                                            ,"Seasonal./.Portable.Equipment?"
                                                                            ,"Component.1.Year.of.Manufacture"
                                                                            ,"SEER"
                                                                            ,"EquipVintage_bins"
                                                                            ,"EquipVintage_bins_MH"
                                                                            ,"y_bar_ilk"
                                                                            ,"y_ilk"
                                                                            ,"m_ilk"))])

item57.os.data <- left_join(item57.os.data, item57.os.dat5[which(colnames(item57.os.dat5) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"Seasonal./.Portable.Equipment?"
                                                                                     ,"Component.1.Year.of.Manufacture"
                                                                                     ,"SEER"
                                                                                     ,"EquipVintage_bins"
                                                                                     ,"EquipVintage_bins_MH"
                                                                                     ,"y_bar_ilk"
                                                                                     ,"y_ilk"
                                                                                     ,"m_ilk"))])
item57.os.data$count <- 1

########################
# Weighted Analysis - single family
########################
item57.os.cast <- mean_two_groups(CustomerLevelData = item57.os.data
                                  ,valueVariable    = 'y_bar_ilk'
                                  ,byVariableColumn = "CK_Building_ID"
                                  ,byVariableRow    = 'EquipVintage_bins'
                                  ,rowAggregate     = 'All Vintages'
                                  ,columnAggregate  = "Remove")

item57.os.table <- data.frame("BuildingType"          = item57.os.cast$BuildingType
                              ,"Equipment.Vintage"    = item57.os.cast$EquipVintage_bins
                              ,"Mean_SCL.GenPop"      = item57.os.cast$`Mean_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item57.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item57.os.cast$`n_SCL GenPop`
                              ,"Mean_SCL.LI"          = NA#item57.os.cast$`Mean_SCL LI`
                              ,"SE_SCL.LI"            = NA#item57.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = NA#item57.os.cast$`n_SCL LI`
                              ,"Mean_SCL.EH"          = item57.os.cast$`Mean_SCL EH`
                              ,"SE_SCL.EH"            = item57.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = item57.os.cast$`n_SCL EH`
                              ,"Mean_2017.RBSA.PS"    = item57.os.cast$`Mean_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item57.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item57.os.cast$`n_2017 RBSA PS`
                              ,"EB_SCL.GenPop"        = item57.os.cast$`EB_SCL GenPop`
                              ,"EB_SCL.LI"            = NA#item57.os.cast$`EB_SCL LI`
                              ,"EB_SCL.EH"            = item57.os.cast$`EB_SCL EH`
                              ,"EB_2017.RBSA.PS"      = item57.os.cast$`EB_2017 RBSA PS`)


# Export table
item57.os.final.SF <- item57.os.table[which(item57.os.table$BuildingType == "Single Family"),-1]

exportTable(item57.os.final.SF, "SF", "Table 64", weighted = TRUE, osIndicator = "SCL", OS = T)

################################
# Unweighted Analysis
################################
item57.os.cast <- mean_two_groups_unweighted(CustomerLevelData = item57.os.data
                                             ,valueVariable    = 'y_bar_ilk'
                                             ,byVariableColumn = "CK_Building_ID"
                                             ,byVariableRow    = 'EquipVintage_bins'
                                             ,rowAggregate     = 'All Vintages'
                                             ,columnAggregate  = "Remove")

item57.os.table <- data.frame("BuildingType"          = item57.os.cast$BuildingType
                              ,"Equipment.Vintage"    = item57.os.cast$EquipVintage_bins
                              ,"Mean_SCL.GenPop"      = item57.os.cast$`Mean_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item57.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item57.os.cast$`n_SCL GenPop`
                              ,"Mean_SCL.LI"          = NA#item57.os.cast$`Mean_SCL LI`
                              ,"SE_SCL.LI"            = NA#item57.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = NA#item57.os.cast$`n_SCL LI`
                              ,"Mean_SCL.EH"          = item57.os.cast$`Mean_SCL EH`
                              ,"SE_SCL.EH"            = item57.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = item57.os.cast$`n_SCL EH`
                              ,"Mean_2017.RBSA.PS"    = item57.os.cast$`Mean_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item57.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item57.os.cast$`n_2017 RBSA PS`)


# Export table
item57.os.final.SF <- item57.os.table[which(item57.os.table$BuildingType == "Single Family"),-1]

exportTable(item57.os.final.SF, "SF", "Table 64", weighted = FALSE, osIndicator = "SCL", OS = T)





#############################################################################################
#Item 58: AVERAGE NUMBER OF PORTABLE COOLING DEVICES PER HOME BY CK_Building_ID (SF table 65, MH table 45)
#############################################################################################
#check unique SEER values
unique(mechanical.dat1$`Seasonal./.Portable.Equipment?`)

#subset to only seasonal / portable = YES
item58.os.dat <- mechanical.dat1[which(mechanical.dat1$`Seasonal./.Portable.Equipment?` == "Yes"),]

#check unique Generic values
unique(item58.os.dat$Generic)

item58.os.dat2 <- left_join(scl.dat, item58.os.dat, by = "CK_Cadmus_ID")

item58.os.dat2$`Seasonal./.Portable.Equipment?`[which(is.na(item58.os.dat2$`Seasonal./.Portable.Equipment?`))] <- "No"

item58.os.dat2$Ind <- 0
item58.os.dat2$Ind[which(item58.os.dat2$`Seasonal./.Portable.Equipment?` == "Yes")] <- 1


# Weighting
item58.os.data <- weightedData(item58.os.dat2[-which(colnames(item58.os.dat2) %in% c("Generic"
                                                                            ,"Seasonal./.Portable.Equipment?"
                                                                            ,"Component.1.Year.of.Manufacture"
                                                                            ,"SEER"
                                                                            ,"Ind"))])

item58.os.data <- left_join(item58.os.data, item58.os.dat2[which(colnames(item58.os.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"Seasonal./.Portable.Equipment?"
                                                                                     ,"Component.1.Year.of.Manufacture"
                                                                                     ,"SEER"
                                                                                     ,"Ind"))])
item58.os.data$count <- 1
item58.os.data$Ind <- as.numeric(as.character(item58.os.data$Ind))

########################
# Weighted Analysis
########################
item58.os.final <- mean_one_group(CustomerLevelData = item58.os.data
                               ,valueVariable    = 'Ind'
                               ,byVariable       = 'CK_Building_ID'
                               ,aggregateRow     = 'Remove')

# Export table
item58.os.final.SF <- item58.os.final[which(item58.os.final$CK_Building_ID != "Remove"),-1]

exportTable(item58.os.final.SF, "SF", "Table 65", weighted = TRUE, osIndicator = "SCL", OS = T)



########################
# Weighted Analysis
########################
item58.os.final <- mean_one_group_unweighted(CustomerLevelData = item58.os.data
                                          ,valueVariable    = 'Ind'
                                          ,byVariable       = 'CK_Building_ID'
                                          ,aggregateRow     = 'Remove')


# Export table
# SF = Table 64, MH = Table 44
item58.os.final.SF <- item58.os.final[which(item58.os.final$CK_Building_ID != "Remove"),-1]

exportTable(item58.os.final.SF, "SF", "Table 65", weighted = FALSE, osIndicator = "SCL", OS = T)

