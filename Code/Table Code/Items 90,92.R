#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################


##  Clear variables
rm(list = ls())
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
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))


names(appliances.dat)
colnames(appliances.dat)[grep("wifi", colnames(appliances.dat), ignore.case = T)]

#############################################################################################
#Item 90: DISTRIBUTION OF CLOTHES DRYERS BY VINTAGE (SF table 97, MH table 78)
#############################################################################################
#subset to columns needed for analysis
item90.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,""))]
item90.dat$count <- 1

item90.dat0 <- item90.dat[which(item90.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item90.dat1 <- left_join(rbsa.dat, item90.dat0, by = "CK_Cadmus_ID")

item90.dat2 <- item90.dat1[which(item90.dat1$Type == "Dryer"),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item90.dat2$EquipVintage_bins <- as.numeric(as.character(item90.dat2$Age))
unique(item90.dat2$Age)
item90.dat3 <- item90.dat2[which(!(is.na(item90.dat2$EquipVintage_bins))),]

item90.dat3$EquipVintage_bins[which(item90.dat3$Age < 1980)] <- "Pre 1980"
item90.dat3$EquipVintage_bins[which(item90.dat3$Age >= 1980 & item90.dat3$Age < 1990)] <- "1980-1989"
item90.dat3$EquipVintage_bins[which(item90.dat3$Age >= 1990 & item90.dat3$Age < 1995)] <- "1990-1994"
item90.dat3$EquipVintage_bins[which(item90.dat3$Age >= 1995 & item90.dat3$Age < 2000)] <- "1995-1999"
item90.dat3$EquipVintage_bins[which(item90.dat3$Age >= 2000 & item90.dat3$Age < 2005)] <- "2000-2004"
item90.dat3$EquipVintage_bins[which(item90.dat3$Age >= 2005 & item90.dat3$Age < 2010)] <- "2005-2009"
item90.dat3$EquipVintage_bins[which(item90.dat3$Age >= 2010 & item90.dat3$Age < 2015)] <- "2010-2014"
item90.dat3$EquipVintage_bins[which(item90.dat3$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item90.dat3$EquipVintage_bins)

item90.merge <- left_join(rbsa.dat, item90.dat3)
item90.merge <- item90.merge[which(!is.na(item90.merge$EquipVintage_bins)),]

################################################
# Adding pop and sample sizes for weights
################################################
item90.data <- weightedData(item90.merge[-which(colnames(item90.merge) %in% c("count"
                                                                              ,"Type"
                                                                              ,"Age"
                                                                              ,"EquipVintage_bins"))])
item90.data <- left_join(item90.data, item90.merge[which(colnames(item90.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Type"
                                                                                       ,"Age"
                                                                                       ,"EquipVintage_bins"))])
item90.data$count <- 1

#######################
# Weighted Analysis
#######################
item90.final <- proportions_one_group(CustomerLevelData = item90.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'EquipVintage_bins'
                                      ,total.name       = 'All Vintages')

unique(item90.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item90.table <- item90.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item90.table <- data.frame(item90.table)


item90.final.SF <- item90.table[which(item90.table$BuildingType == "Single Family")
                                ,-which(colnames(item90.table) %in% c("BuildingType"))]
item90.final.MH <- item90.table[which(item90.table$BuildingType == "Manufactured")
                                ,-which(colnames(item90.table) %in% c("BuildingType"))]

exportTable(item90.final.SF, "SF", "Table 97", weighted = TRUE)
# exportTable(item90.final.MH, "MH", "Table 78", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item90.final <- proportions_one_group(CustomerLevelData = item90.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'EquipVintage_bins'
                                      ,total.name       = 'All Vintages'
                                      ,weighted         = FALSE)

unique(item90.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item90.table <- item90.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item90.table <- data.frame(item90.table)


item90.final.SF <- item90.table[which(item90.table$BuildingType == "Single Family")
                                ,-which(colnames(item90.table) %in% c("BuildingType","Total.Count"))]
item90.final.MH <- item90.table[which(item90.table$BuildingType == "Manufactured")
                                ,-which(colnames(item90.table) %in% c("BuildingType","Total.Count"))]

exportTable(item90.final.SF, "SF", "Table 97", weighted = FALSE)
# exportTable(item90.final.MH, "MH", "Table 78", weighted = FALSE)








#############################################################################################
#Item 92: DISTRIBUTION OF DISHWASHERS BY VINTAGE (SF table 99, MH table 80)
#############################################################################################
#subset to columns needed for analysis
item92.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,""))]
item92.dat$count <- 1

item92.dat0 <- item92.dat[which(item92.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item92.dat1 <- left_join(rbsa.dat, item92.dat0, by = "CK_Cadmus_ID")

item92.dat2 <- item92.dat1[which(item92.dat1$Type == "Dishwasher"),]

item92.dat2$EquipVintage_bins <- as.numeric(as.character(item92.dat2$Age))
item92.dat3 <- item92.dat2[which(!(is.na(item92.dat2$EquipVintage_bins))),]

item92.dat3$EquipVintage_bins[which(item92.dat3$Age < 1980)] <- "Pre 1980"
item92.dat3$EquipVintage_bins[which(item92.dat3$Age >= 1980 & item92.dat3$Age < 1990)] <- "1980-1989"
item92.dat3$EquipVintage_bins[which(item92.dat3$Age >= 1990 & item92.dat3$Age < 1995)] <- "1990-1994"
item92.dat3$EquipVintage_bins[which(item92.dat3$Age >= 1995 & item92.dat3$Age < 2000)] <- "1995-1999"
item92.dat3$EquipVintage_bins[which(item92.dat3$Age >= 2000 & item92.dat3$Age < 2005)] <- "2000-2004"
item92.dat3$EquipVintage_bins[which(item92.dat3$Age >= 2005 & item92.dat3$Age < 2010)] <- "2005-2009"
item92.dat3$EquipVintage_bins[which(item92.dat3$Age >= 2010 & item92.dat3$Age < 2015)] <- "2010-2014"
item92.dat3$EquipVintage_bins[which(item92.dat3$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item92.dat3$EquipVintage_bins)

item92.merge <- left_join(rbsa.dat, item92.dat3)
item92.merge <- item92.merge[which(!is.na(item92.merge$EquipVintage_bins)),]

################################################
# Adding pop and sample sizes for weights
################################################
item92.data <- weightedData(item92.merge[-which(colnames(item92.merge) %in% c("count"
                                                                              ,"Type"
                                                                              ,"Age"
                                                                              ,"EquipVintage_bins"))])
item92.data <- left_join(item92.data, item92.merge[which(colnames(item92.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Type"
                                                                                       ,"Age"
                                                                                       ,"EquipVintage_bins"))])
item92.data$count <- 1

#######################
# Weighted Analysis
#######################
item92.final <- proportions_one_group(CustomerLevelData = item92.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'EquipVintage_bins'
                                      ,total.name       = 'All Vintages')

unique(item92.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")

item92.table <- item92.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item92.table <- data.frame(item92.table)


item92.final.SF <- item92.table[which(item92.table$BuildingType == "Single Family")
                                ,-which(colnames(item92.table) %in% c("BuildingType"))]
item92.final.MH <- item92.table[which(item92.table$BuildingType == "Manufactured")
                                ,-which(colnames(item92.table) %in% c("BuildingType"))]

exportTable(item92.final.SF, "SF", "Table 99", weighted = TRUE)
# exportTable(item92.final.MH, "MH", "Table 80", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item92.final <- proportions_one_group(CustomerLevelData = item92.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'EquipVintage_bins'
                                      ,total.name       = 'All Vintages'
                                      ,weighted         = FALSE)

unique(item92.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")

item92.table <- item92.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item92.table <- data.frame(item92.table)


item92.final.SF <- item92.table[which(item92.table$BuildingType == "Single Family")
                                ,-which(colnames(item92.table) %in% c("BuildingType"))]
item92.final.MH <- item92.table[which(item92.table$BuildingType == "Manufactured")
                                ,-which(colnames(item92.table) %in% c("BuildingType"))]

exportTable(item92.final.SF, "SF", "Table 99", weighted = FALSE)
# exportTable(item92.final.MH, "MH", "Table 80", weighted = FALSE)






















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
#Item 90: DISTRIBUTION OF CLOTHES DRYERS BY VINTAGE (SF table 97, MH table 78)
#############################################################################################
#subset to columns needed for analysis
item90.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"))]
item90.os.dat$count <- 1

item90.os.dat0 <- item90.os.dat[which(item90.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item90.os.dat1 <- left_join(scl.dat, item90.os.dat0, by = "CK_Cadmus_ID")

item90.os.dat2 <- item90.os.dat1[which(item90.os.dat1$Type == "Dryer"),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item90.os.dat2$EquipVintage_bins <- as.numeric(as.character(item90.os.dat2$Age))
unique(item90.os.dat2$Age)
item90.os.dat3 <- item90.os.dat2[which(!(is.na(item90.os.dat2$EquipVintage_bins))),]

item90.os.dat3$EquipVintage_bins[which(item90.os.dat3$Age < 1980)] <- "Pre 1980"
item90.os.dat3$EquipVintage_bins[which(item90.os.dat3$Age >= 1980 & item90.os.dat3$Age < 1990)] <- "1980-1989"
item90.os.dat3$EquipVintage_bins[which(item90.os.dat3$Age >= 1990 & item90.os.dat3$Age < 1995)] <- "1990-1994"
item90.os.dat3$EquipVintage_bins[which(item90.os.dat3$Age >= 1995 & item90.os.dat3$Age < 2000)] <- "1995-1999"
item90.os.dat3$EquipVintage_bins[which(item90.os.dat3$Age >= 2000 & item90.os.dat3$Age < 2005)] <- "2000-2004"
item90.os.dat3$EquipVintage_bins[which(item90.os.dat3$Age >= 2005 & item90.os.dat3$Age < 2010)] <- "2005-2009"
item90.os.dat3$EquipVintage_bins[which(item90.os.dat3$Age >= 2010 & item90.os.dat3$Age < 2015)] <- "2010-2014"
item90.os.dat3$EquipVintage_bins[which(item90.os.dat3$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item90.os.dat3$EquipVintage_bins)

item90.os.merge <- left_join(scl.dat, item90.os.dat3)
item90.os.merge <- item90.os.merge[which(!is.na(item90.os.merge$EquipVintage_bins)),]

################################################
# Adding pop and sample sizes for weights
################################################
item90.os.data <- weightedData(item90.os.merge[-which(colnames(item90.os.merge) %in% c("count"
                                                                              ,"Type"
                                                                              ,"Age"
                                                                              ,"EquipVintage_bins"))])
item90.os.data <- left_join(item90.os.data, unique(item90.os.merge[which(colnames(item90.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Type"
                                                                                       ,"Age"
                                                                                       ,"EquipVintage_bins"))]))
item90.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item90.os.final <- proportionRowsAndColumns1(CustomerLevelData = item90.os.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = "EquipVintage_bins"
                                             ,aggregateColumnName = "Remove")
item90.os.final <- item90.os.final[which(item90.os.final$CK_Building_ID != "Remove"),]

item90.os.cast <- dcast(setDT(item90.os.final)
                        ,formula = BuildingType + EquipVintage_bins ~ CK_Building_ID
                        ,value.var = c("w.percent", "w.SE","n", "EB"))

item90.os.final <- data.frame("BuildingType"          = item90.os.cast$BuildingType
                              ,"EquipVintage_bins"    = item90.os.cast$EquipVintage_bins
                              ,"Percent_SCL.GenPop"   = item90.os.cast$`w.percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item90.os.cast$`w.SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item90.os.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = item90.os.cast$`w.percent_SCL LI`
                              ,"SE_SCL.LI"            = item90.os.cast$`w.SE_SCL LI`
                              ,"n_SCL.LI"             = item90.os.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = item90.os.cast$`w.percent_SCL EH`
                              ,"SE_SCL.EH"            = item90.os.cast$`w.SE_SCL EH`
                              ,"n_SCL.EH"             = item90.os.cast$`n_SCL EH`
                              ,"Percent_2017.RBSA.PS" = item90.os.cast$`w.percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item90.os.cast$`w.SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item90.os.cast$`n_2017 RBSA PS`
                              ,"EB_SCL.GenPop"        = item90.os.cast$`EB_SCL GenPop`
                              ,"EB_SCL.LI"            = item90.os.cast$`EB_SCL LI`
                              ,"EB_SCL.EH"            = item90.os.cast$`EB_SCL EH`
                              ,"EB_2017.RBSA.PS"      = item90.os.cast$`EB_2017 RBSA PS`)

unique(item90.os.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item90.os.table <- item90.os.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item90.os.table <- data.frame(item90.os.table)

item90.os.final.SF <- item90.os.table[which(item90.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item90.os.table) %in% c("BuildingType"))]

exportTable(item90.os.final.SF, "SF", "Table 97", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item90.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item90.os.data
                                                     ,valueVariable = "count"
                                                     ,columnVariable = "CK_Building_ID"
                                                     ,rowVariable = "EquipVintage_bins"
                                                     ,aggregateColumnName = "Remove")
item90.os.final <- item90.os.final[which(item90.os.final$CK_Building_ID != "Remove"),]

item90.os.cast <- dcast(setDT(item90.os.final)
                        ,formula = BuildingType + EquipVintage_bins ~ CK_Building_ID
                        ,value.var = c("Percent", "SE","n"))

item90.os.final <- data.frame("BuildingType"          = item90.os.cast$BuildingType
                              ,"EquipVintage_bins"    = item90.os.cast$EquipVintage_bins
                              ,"Percent_SCL.GenPop"   = item90.os.cast$`Percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item90.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item90.os.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = item90.os.cast$`Percent_SCL LI`
                              ,"SE_SCL.LI"            = item90.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = item90.os.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = item90.os.cast$`Percent_SCL EH`
                              ,"SE_SCL.EH"            = item90.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = item90.os.cast$`n_SCL EH`
                              ,"Percent_2017.RBSA.PS" = item90.os.cast$`Percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item90.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item90.os.cast$`n_2017 RBSA PS`)

unique(item90.os.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item90.os.table <- item90.os.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item90.os.table <- data.frame(item90.os.table)

item90.os.final.SF <- item90.os.table[which(item90.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item90.os.table) %in% c("BuildingType","Total.Count"))]

exportTable(item90.os.final.SF, "SF", "Table 97", weighted = FALSE, osIndicator = "SCL", OS = T)





#############################################################################################
#Item 92: DISTRIBUTION OF DISHWASHERS BY VINTAGE (SF table 99, MH table 80)
#############################################################################################
#subset to columns needed for analysis
item92.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,""))]
item92.os.dat$count <- 1

item92.os.dat0 <- item92.os.dat[which(item92.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item92.os.dat1 <- left_join(scl.dat, item92.os.dat0, by = "CK_Cadmus_ID")

item92.os.dat2 <- item92.os.dat1[which(item92.os.dat1$Type == "Dishwasher"),]

item92.os.dat2$EquipVintage_bins <- as.numeric(as.character(item92.os.dat2$Age))
item92.os.dat3 <- item92.os.dat2[which(!(is.na(item92.os.dat2$EquipVintage_bins))),]

item92.os.dat3$EquipVintage_bins[which(item92.os.dat3$Age < 1980)] <- "Pre 1980"
item92.os.dat3$EquipVintage_bins[which(item92.os.dat3$Age >= 1980 & item92.os.dat3$Age < 1990)] <- "1980-1989"
item92.os.dat3$EquipVintage_bins[which(item92.os.dat3$Age >= 1990 & item92.os.dat3$Age < 1995)] <- "1990-1994"
item92.os.dat3$EquipVintage_bins[which(item92.os.dat3$Age >= 1995 & item92.os.dat3$Age < 2000)] <- "1995-1999"
item92.os.dat3$EquipVintage_bins[which(item92.os.dat3$Age >= 2000 & item92.os.dat3$Age < 2005)] <- "2000-2004"
item92.os.dat3$EquipVintage_bins[which(item92.os.dat3$Age >= 2005 & item92.os.dat3$Age < 2010)] <- "2005-2009"
item92.os.dat3$EquipVintage_bins[which(item92.os.dat3$Age >= 2010 & item92.os.dat3$Age < 2015)] <- "2010-2014"
item92.os.dat3$EquipVintage_bins[which(item92.os.dat3$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item92.os.dat3$EquipVintage_bins)

item92.os.merge <- left_join(scl.dat, item92.os.dat3)
item92.os.merge <- item92.os.merge[which(!is.na(item92.os.merge$EquipVintage_bins)),]

################################################
# Adding pop and sample sizes for weights
################################################
item92.os.data <- weightedData(item92.os.merge[-which(colnames(item92.os.merge) %in% c("count"
                                                                              ,"Type"
                                                                              ,"Age"
                                                                              ,"EquipVintage_bins"))])
item92.os.data <- left_join(item92.os.data, unique(item92.os.merge[which(colnames(item92.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Type"
                                                                                       ,"Age"
                                                                                       ,"EquipVintage_bins"))]))
item92.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item92.os.final <- proportionRowsAndColumns1(CustomerLevelData = item92.os.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = "EquipVintage_bins"
                                             ,aggregateColumnName = "Remove")
item92.os.final <- item92.os.final[which(item92.os.final$CK_Building_ID != "Remove"),]

item92.os.cast <- dcast(setDT(item92.os.final)
                        ,formula = BuildingType + EquipVintage_bins ~ CK_Building_ID
                        ,value.var = c("w.percent", "w.SE","n", "EB"))

item92.os.final <- data.frame("BuildingType"          = item92.os.cast$BuildingType
                              ,"EquipVintage_bins"    = item92.os.cast$EquipVintage_bins
                              ,"Percent_SCL.GenPop"   = item92.os.cast$`w.percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item92.os.cast$`w.SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item92.os.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = item92.os.cast$`w.percent_SCL LI`
                              ,"SE_SCL.LI"            = item92.os.cast$`w.SE_SCL LI`
                              ,"n_SCL.LI"             = item92.os.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = item92.os.cast$`w.percent_SCL EH`
                              ,"SE_SCL.EH"            = item92.os.cast$`w.SE_SCL EH`
                              ,"n_SCL.EH"             = item92.os.cast$`n_SCL EH`
                              ,"Percent_2017.RBSA.PS" = item92.os.cast$`w.percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item92.os.cast$`w.SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item92.os.cast$`n_2017 RBSA PS`
                              ,"EB_SCL.GenPop"        = item92.os.cast$`EB_SCL GenPop`
                              ,"EB_SCL.LI"            = item92.os.cast$`EB_SCL LI`
                              ,"EB_SCL.EH"            = item92.os.cast$`EB_SCL EH`
                              ,"EB_2017.RBSA.PS"      = item92.os.cast$`EB_2017 RBSA PS`)

unique(item92.os.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")

item92.os.table <- item92.os.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item92.os.table <- data.frame(item92.os.table)


item92.os.final.SF <- item92.os.table[which(item92.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item92.os.table) %in% c("BuildingType"))]

exportTable(item92.os.final.SF, "SF", "Table 99", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item92.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item92.os.data
                                                     ,valueVariable = "count"
                                                     ,columnVariable = "CK_Building_ID"
                                                     ,rowVariable = "EquipVintage_bins"
                                                     ,aggregateColumnName = "Remove")
item92.os.final <- item92.os.final[which(item92.os.final$CK_Building_ID != "Remove"),]

item92.os.cast <- dcast(setDT(item92.os.final)
                        ,formula = BuildingType + EquipVintage_bins ~ CK_Building_ID
                        ,value.var = c("Percent", "SE","n"))

item92.os.final <- data.frame("BuildingType"          = item92.os.cast$BuildingType
                              ,"EquipVintage_bins"    = item92.os.cast$EquipVintage_bins
                              ,"Percent_SCL.GenPop"   = item92.os.cast$`Percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item92.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item92.os.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = item92.os.cast$`Percent_SCL LI`
                              ,"SE_SCL.LI"            = item92.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = item92.os.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = item92.os.cast$`Percent_SCL EH`
                              ,"SE_SCL.EH"            = item92.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = item92.os.cast$`n_SCL EH`
                              ,"Percent_2017.RBSA.PS" = item92.os.cast$`Percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item92.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item92.os.cast$`n_2017 RBSA PS`)

unique(item92.os.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")

item92.os.table <- item92.os.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item92.os.table <- data.frame(item92.os.table)


item92.os.final.SF <- item92.os.table[which(item92.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item92.os.table) %in% c("BuildingType"))]

exportTable(item92.os.final.SF, "SF", "Table 99", weighted = FALSE, osIndicator = "SCL", OS = T)
