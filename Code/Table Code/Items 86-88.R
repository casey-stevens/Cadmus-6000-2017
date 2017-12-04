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


#############################################################################################
#Item 86: DISTRIBUTION OF CLOTHES WASHERS BY VINTAGE (SF table 93, MH table 74)
#############################################################################################
#subset to columns needed for analysis
item86.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,""))]
item86.dat$count <- 1

item86.dat0 <- item86.dat[which(item86.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item86.dat1 <- left_join(item86.dat0, rbsa.dat, by = "CK_Cadmus_ID")


item86.dat2 <- item86.dat1[which(item86.dat1$Type == "Washer"),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item86.dat2$EquipVintage_bins <- as.numeric(as.character(item86.dat2$Age))
item86.dat3 <- item86.dat2[which(!(is.na(item86.dat2$EquipVintage_bins))),]

item86.dat3$EquipVintage_bins[which(item86.dat3$Age < 1980)] <- "Pre 1980"
item86.dat3$EquipVintage_bins[which(item86.dat3$Age >= 1980 & item86.dat3$Age < 1990)] <- "1980-1989"
item86.dat3$EquipVintage_bins[which(item86.dat3$Age >= 1990 & item86.dat3$Age < 1995)] <- "1990-1994"
item86.dat3$EquipVintage_bins[which(item86.dat3$Age >= 1995 & item86.dat3$Age < 2000)] <- "1995-1999"
item86.dat3$EquipVintage_bins[which(item86.dat3$Age >= 2000 & item86.dat3$Age < 2005)] <- "2000-2004"
item86.dat3$EquipVintage_bins[which(item86.dat3$Age >= 2005 & item86.dat3$Age < 2010)] <- "2005-2009"
item86.dat3$EquipVintage_bins[which(item86.dat3$Age >= 2010 & item86.dat3$Age < 2015)] <- "2010-2014"
item86.dat3$EquipVintage_bins[which(item86.dat3$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item86.dat3$EquipVintage_bins)

item86.merge <- left_join(rbsa.dat, item86.dat3)
item86.merge <- item86.merge[which(!is.na(item86.merge$EquipVintage_bins)),]

################################################
# Adding pop and sample sizes for weights
################################################
item86.data <- weightedData(item86.merge[-which(colnames(item86.merge) %in% c("count"
                                                                              ,"Type"
                                                                              ,"Age"
                                                                              ,"EquipVintage_bins"))])
item86.data <- left_join(item86.data, item86.merge[which(colnames(item86.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Type"
                                                                                       ,"Age"
                                                                                       ,"EquipVintage_bins"))])
item86.data$count <- 1

#######################
# Weighted Analysis
#######################
item86.final <- proportions_one_group(CustomerLevelData = item86.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'EquipVintage_bins'
                                      ,total.name       = 'All Vintages')

unique(item86.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item86.table <- item86.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item86.table <- data.frame(item86.table)


item86.final.SF <- item86.table[which(item86.table$BuildingType == "Single Family")
                                ,-which(colnames(item86.table) %in% c("BuildingType"
                                                                      ,"Remove"))]
item86.final.MH <- item86.table[which(item86.table$BuildingType == "Manufactured")
                                ,-which(colnames(item86.table) %in% c("BuildingType"
                                                                      ,"Remove"))]

exportTable(item86.final.SF, "SF", "Table 93", weighted = TRUE)
exportTable(item86.final.MH, "MH", "Table 74", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item86.final <- proportions_one_group(CustomerLevelData = item86.data
                                      ,valueVariable    = 'count'
                                      ,groupingVariable = 'EquipVintage_bins'
                                      ,total.name       = 'All Vintages'
                                      ,weighted         = FALSE)

unique(item86.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item86.table <- item86.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item86.table <- data.frame(item86.table)


item86.final.SF <- item86.table[which(item86.table$BuildingType == "Single Family")
                                ,-which(colnames(item86.table) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"Total.Count"))]
item86.final.MH <- item86.table[which(item86.table$BuildingType == "Manufactured")
                                ,-which(colnames(item86.table) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"Total.Count"))]

exportTable(item86.final.SF, "SF", "Table 93", weighted = FALSE)
exportTable(item86.final.MH, "MH", "Table 74", weighted = FALSE)





#############################################################################################
#Item 87: DISTRIBUTION OF CLOTHES WASHERS BY TYPE AND STATE (SF table 94)
#############################################################################################
#subset to columns needed for analysis
item87.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,"Washer.Type"))]
item87.dat$count <- 1

item87.dat0 <- item87.dat[which(item87.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item87.dat1 <- left_join(item87.dat0, rbsa.dat, by = "CK_Cadmus_ID")


item87.dat2 <- item87.dat1[which(item87.dat1$Type == "Washer"),]

item87.merge <- left_join(rbsa.dat, item87.dat2)
item87.merge <- item87.merge[which(!is.na(item87.merge$Type)),]
item87.merge <- item87.merge[which(item87.merge$Type != "Unknown"),]



################################################
# Adding pop and sample sizes for weights
################################################
item87.data <- weightedData(item87.merge[-which(colnames(item87.merge) %in% c("Type"               
                                                                              ,"Age"
                                                                              ,"Washer.Type"
                                                                              ,"count"))])
item87.data <- left_join(item87.data, item87.merge[which(colnames(item87.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Type"               
                                                                                       ,"Age"
                                                                                       ,"Washer.Type"
                                                                                       ,"count"))])
#######################
# Weighted Analysis
#######################
item87.final <- proportionRowsAndColumns1(CustomerLevelData = item87.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'State'
                                          ,rowVariable      = 'Washer.Type'
                                          ,aggregateColumnName = "Region")

item87.cast <- dcast(setDT(item87.final)
                     , formula = BuildingType + Washer.Type ~ State
                     , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item87.table <- data.frame("BuildingType"    = item87.cast$BuildingType
                           ,"Washer.Type"      = item87.cast$Washer.Type
                           ,"Percent_ID"     = item87.cast$w.percent_ID
                           ,"SE_ID"          = item87.cast$w.SE_ID
                           ,"Count_ID"       = item87.cast$count_ID
                           ,"Percent_MT"     = item87.cast$w.percent_MT
                           ,"SE_MT"          = item87.cast$w.SE_MT
                           ,"Count_MT"       = item87.cast$count_MT
                           ,"Percent_OR"     = item87.cast$w.percent_OR
                           ,"SE_OR"          = item87.cast$w.SE_OR
                           ,"Count_OR"       = item87.cast$count_OR
                           ,"Percent_WA"     = item87.cast$w.percent_WA
                           ,"SE_WA"          = item87.cast$w.SE_WA
                           ,"Count_WA"       = item87.cast$count_WA
                           ,"Percent_Region" = item87.cast$w.percent_Region
                           ,"SE_Region"      = item87.cast$w.SE_Region
                           ,"Count_Region"   = item87.cast$count_Region
                           # ,"n"     = item87.cast$n_Region
)
stopifnot(sum(item87.table[which(item87.table$BuildingType == "Single Family")
                           ,grep("Percent",colnames(item87.table))], na.rm = T) == 10)

unique(item87.table$Washer.Type)
rowOrder <- c("Combined Washer/Dryer in one drum"
              ,"Horizontal Axis"
              ,"Stacked Washer/Dryer"
              ,"Vertical Axis (with agitator)"
              ,"Vertical Axis (without agitator)"
              ,"Unknown"
              ,"Total")
item87.table <- item87.table %>% mutate(Washer.Type = factor(Washer.Type, levels = rowOrder)) %>% arrange(Washer.Type)  
item87.table <- data.frame(item87.table)

item87.final.SF <- item87.table[which(item87.table$BuildingType == "Single Family")
                                ,which(colnames(item87.table) %notin% c("BuildingType"))]
item87.final.MH <- item87.table[which(item87.table$BuildingType == "Manufactured")
                                ,which(colnames(item87.table) %notin% c("BuildingType"))]

exportTable(item87.final.SF, "SF", "Table 94", weighted = TRUE)
exportTable(item87.final.MH, "MH", "Table 75", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item87.final <- proportions_two_groups_unweighted(CustomerLevelData = item87.data
                                                  ,valueVariable    = 'count'
                                                  ,columnVariable   = 'State'
                                                  ,rowVariable      = 'Washer.Type'
                                                  ,aggregateColumnName = "Region")

item87.cast <- dcast(setDT(item87.final)
                     , formula = BuildingType + Washer.Type ~ State
                     , value.var = c("Percent", "SE", "Count", "n"))


item87.table <- data.frame("BuildingType"    = item87.cast$BuildingType
                           ,"Washer.Type"      = item87.cast$Washer.Type
                           ,"Percent_ID"     = item87.cast$Percent_ID
                           ,"SE_ID"          = item87.cast$SE_ID
                           ,"Count_ID"       = item87.cast$Count_ID
                           ,"Percent_MT"     = item87.cast$Percent_MT
                           ,"SE_MT"          = item87.cast$SE_MT
                           ,"Count_MT"       = item87.cast$Count_MT
                           ,"Percent_OR"     = item87.cast$Percent_OR
                           ,"SE_OR"          = item87.cast$SE_OR
                           ,"Count_OR"       = item87.cast$Count_OR
                           ,"Percent_WA"     = item87.cast$Percent_WA
                           ,"SE_WA"          = item87.cast$SE_WA
                           ,"Count_WA"       = item87.cast$Count_WA
                           ,"Percent_Region" = item87.cast$Percent_Region
                           ,"SE_Region"      = item87.cast$SE_Region
                           ,"Count_Region"   = item87.cast$Count_Region
                           # ,"n"     = item87.cast$n_Region
)
stopifnot(sum(item87.table[which(item87.table$BuildingType == "Single Family")
                           ,grep("Percent",colnames(item87.table))], na.rm = T) == 10)

unique(item87.table$Washer.Type)
rowOrder <- c("Combined Washer/Dryer in one drum"
              ,"Horizontal Axis"
              ,"Stacked Washer/Dryer"
              ,"Vertical Axis (with agitator)"
              ,"Vertical Axis (without agitator)"
              ,"Unknown"
              ,"Total")
item87.table <- item87.table %>% mutate(Washer.Type = factor(Washer.Type, levels = rowOrder)) %>% arrange(Washer.Type)  
item87.table <- data.frame(item87.table)

item87.final.SF <- item87.table[which(item87.table$BuildingType == "Single Family")
                                ,-which(colnames(item87.table) %in% c("BuildingType"))]
item87.final.MH <- item87.table[which(item87.table$BuildingType == "Manufactured")
                                ,-which(colnames(item87.table) %in% c("BuildingType"))]

exportTable(item87.final.SF, "SF", "Table 94", weighted = FALSE)
exportTable(item87.final.MH, "MH", "Table 75", weighted = FALSE)







#############################################################################################
#Item 88: DISTRIBUTION OF CLOTHES WASHERS BY TYPE AND VINTAGE (SF table 95, MH table 76)
#############################################################################################
#subset to columns needed for analysis
item88.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,"Washer.Type"))]
item88.dat$count <- 1

item88.dat0 <- item88.dat[which(item88.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item88.dat1 <- left_join(item88.dat0, rbsa.dat, by = "CK_Cadmus_ID")


item88.dat2 <- item88.dat1[which(item88.dat1$Type == "Washer"),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item88.dat2$EquipVintage_bins <- as.numeric(as.character(item88.dat2$Age))
item88.dat3 <- item88.dat2[which(!(is.na(item88.dat2$EquipVintage_bins))),]

item88.dat3$EquipVintage_bins[which(item88.dat3$Age < 1990)] <- "Pre 1990"
item88.dat3$EquipVintage_bins[which(item88.dat3$Age >= 1990 & item88.dat3$Age < 1995)] <- "1990-1994"
item88.dat3$EquipVintage_bins[which(item88.dat3$Age >= 1995 & item88.dat3$Age < 2000)] <- "1995-1999"
item88.dat3$EquipVintage_bins[which(item88.dat3$Age >= 2000 & item88.dat3$Age < 2005)] <- "2000-2004"
item88.dat3$EquipVintage_bins[which(item88.dat3$Age >= 2005 & item88.dat3$Age < 2010)] <- "2005-2009"
item88.dat3$EquipVintage_bins[which(item88.dat3$Age >= 2010 & item88.dat3$Age < 2015)] <- "2010-2014"
item88.dat3$EquipVintage_bins[which(item88.dat3$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item88.dat3$EquipVintage_bins)


item88.merge <- left_join(rbsa.dat, item88.dat3)
item88.merge <- item88.merge[which(!is.na(item88.merge$EquipVintage_bins)),]



################################################
# Adding pop and sample sizes for weights
################################################
item88.data <- weightedData(item88.merge[-which(colnames(item88.merge) %in% c("Type"               
                                                                              ,"Age"
                                                                              ,"Washer.Type"
                                                                              ,"count"
                                                                              ,"EquipVintage_bins"))])
item88.data <- left_join(item88.data, item88.merge[which(colnames(item88.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Type"               
                                                                                       ,"Age"
                                                                                       ,"Washer.Type"
                                                                                       ,"count"
                                                                                       ,"EquipVintage_bins"))])
#######################
# Weighted Analysis
#######################
item88.summary <- proportionRowsAndColumns1(CustomerLevelData = item88.data
                                            ,valueVariable    = 'count'
                                            ,columnVariable   = 'Washer.Type'
                                            ,rowVariable      = 'EquipVintage_bins'
                                            ,aggregateColumnName = "Remove")
item88.summary <- item88.summary[which(item88.summary$Washer.Type %notin% c("Remove", "Total")),]
item88.summary <- item88.summary[which(item88.summary$EquipVintage_bins %notin% c("Remove", "Total")),]

item88.all.washer.types <- proportions_one_group(CustomerLevelData = item88.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = "EquipVintage_bins"
                                                 ,total.name = "All Clothes Washer Types"
                                                 ,columnName = "Washer.Type"
                                                 ,weighted = TRUE
                                                 ,two.prop.total = TRUE)
item88.all.washer.types <- item88.all.washer.types[which(item88.all.washer.types$EquipVintage_bins != "Total"),]


item88.final <- rbind.data.frame(item88.summary, item88.all.washer.types, stringsAsFactors = F)

item88.cast <- dcast(setDT(item88.final)
                     , formula = BuildingType + Washer.Type ~ EquipVintage_bins
                     , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item88.table <- data.frame("BuildingType"           = item88.cast$BuildingType
                           ,"Washer.Type"           = item88.cast$Washer.Type
                           ,"Percent_Pre.1990"      = item88.cast$`w.percent_Pre 1990`
                           ,"SE_Pre.1990"           = item88.cast$`w.SE_Pre 1990`
                           ,"Count_Pre.1990"        = item88.cast$`count_Pre 1990`
                           ,"Percent_1990.1994"     = item88.cast$`w.percent_1990-1994`
                           ,"SE_1990.1994"          = item88.cast$`w.SE_1990-1994`
                           ,"Count_1990.1994"       = item88.cast$`count_1990-1994`
                           ,"Percent_1995.1999"     = item88.cast$`w.percent_1995-1999`
                           ,"SE_1995.1999"          = item88.cast$`w.SE_1995-1999`
                           ,"Count_1995.1999"       = item88.cast$`count_1995-1999`
                           ,"Percent_2000.2004"     = item88.cast$`w.percent_2000-2004`
                           ,"SE_2000.2004"          = item88.cast$`w.SE_2000-2004`
                           ,"Count_2000.2004"       = item88.cast$`count_2000-2004`
                           ,"Percent_2005.2009"     = item88.cast$`w.percent_2005-2009`
                           ,"SE_2005.2009"          = item88.cast$`w.SE_2005-2009`
                           ,"Count_2005.2009"       = item88.cast$`count_2005-2009`
                           ,"Percent_2010.2014"     = item88.cast$`w.percent_2010-2014`
                           ,"SE_2010.2014"          = item88.cast$`w.SE_2010-2014`
                           ,"Count_2010.2014"       = item88.cast$`count_2010-2014`
                           ,"Percent_Post.2014"     = item88.cast$`w.percent_Post 2014`
                           ,"SE_Post.2014"          = item88.cast$`w.SE_Post 2014`
                           ,"Count_Post.2014"       = item88.cast$`count_Post 2014`
                           # ,"n"     = item88.cast$n_Region
)

unique(item88.table$Washer.Type)
rowOrder <- c("Combined Washer/Dryer in one drum"
              ,"Horizontal Axis"
              ,"Stacked Washer/Dryer"
              ,"Vertical Axis (with agitator)"
              ,"Vertical Axis (without agitator)"
              ,"Unknown"
              ,"All Clothes Washer Types")
item88.table <- item88.table %>% mutate(Washer.Type = factor(Washer.Type, levels = rowOrder)) %>% arrange(Washer.Type)  
item88.table <- data.frame(item88.table)


item88.final.SF <- item88.table[which(item88.table$BuildingType == "Single Family")
                                ,-which(colnames(item88.table) %in% c("BuildingType"))]
item88.final.MH <- item88.table[which(item88.table$BuildingType == "Manufactured")
                                ,-which(colnames(item88.table) %in% c("BuildingType"))]

exportTable(item88.final.SF, "SF", "Table 95", weighted = TRUE)
exportTable(item88.final.MH, "MH", "Table 76", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item88.summary <- proportions_two_groups_unweighted(CustomerLevelData = item88.data
                                                    ,valueVariable    = 'count'
                                                    ,columnVariable   = 'Washer.Type'
                                                    ,rowVariable      = 'EquipVintage_bins'
                                                    ,aggregateColumnName = "Remove")
item88.summary <- item88.summary[which(item88.summary$Washer.Type %notin% c("Remove", "Total")),]
item88.summary <- item88.summary[which(item88.summary$EquipVintage_bins %notin% c("Remove", "Total")),]

item88.all.washer.types <- proportions_one_group(CustomerLevelData = item88.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = "EquipVintage_bins"
                                                 ,total.name = "All Clothes Washer Types"
                                                 ,columnName = "Washer.Type"
                                                 ,weighted = FALSE
                                                 ,two.prop.total = TRUE)
item88.all.washer.types <- item88.all.washer.types[which(item88.all.washer.types$EquipVintage_bins != "Total"),]


item88.final <- rbind.data.frame(item88.summary, item88.all.washer.types, stringsAsFactors = F)

item88.cast <- dcast(setDT(item88.final)
                     , formula = BuildingType + Washer.Type ~ EquipVintage_bins
                     , value.var = c("Percent", "SE", "Count", "n"))

item88.table <- data.frame("BuildingType"           = item88.cast$BuildingType
                           ,"Washer.Type"           = item88.cast$Washer.Type
                           ,"Percent_Pre.1990"      = item88.cast$`Percent_Pre 1990`
                           ,"SE_Pre.1990"           = item88.cast$`SE_Pre 1990`
                           ,"Count_Pre.1990"        = item88.cast$`Count_Pre 1990`
                           ,"Percent_1990.1994"     = item88.cast$`Percent_1990-1994`
                           ,"SE_1990.1994"          = item88.cast$`SE_1990-1994`
                           ,"Count_1990.1994"       = item88.cast$`Count_1990-1994`
                           ,"Percent_1995.1999"     = item88.cast$`Percent_1995-1999`
                           ,"SE_1995.1999"          = item88.cast$`SE_1995-1999`
                           ,"Count_1995.1999"       = item88.cast$`Count_1995-1999`
                           ,"Percent_2000.2004"     = item88.cast$`Percent_2000-2004`
                           ,"SE_2000.2004"          = item88.cast$`SE_2000-2004`
                           ,"Count_2000.2004"       = item88.cast$`Count_2000-2004`
                           ,"Percent_2005.2009"     = item88.cast$`Percent_2005-2009`
                           ,"SE_2005.2009"          = item88.cast$`SE_2005-2009`
                           ,"Count_2005.2009"       = item88.cast$`Count_2005-2009`
                           ,"Percent_2010.2014"     = item88.cast$`Percent_2010-2014`
                           ,"SE_2010.2014"          = item88.cast$`SE_2010-2014`
                           ,"Count_2010.2014"       = item88.cast$`Count_2010-2014`
                           ,"Percent_Post.2014"     = item88.cast$`Percent_Post 2014`
                           ,"SE_Post.2014"          = item88.cast$`SE_Post 2014`
                           ,"Count_Post.2014"       = item88.cast$`Count_Post 2014`
                           # ,"n"     = item88.cast$n_Region
)

unique(item88.table$Washer.Type)
rowOrder <- c("Combined Washer/Dryer in one drum"
              ,"Horizontal Axis"
              ,"Stacked Washer/Dryer"
              ,"Vertical Axis (with agitator)"
              ,"Vertical Axis (without agitator)"
              ,"Unknown"
              ,"All Clothes Washer Types")
item88.table <- item88.table %>% mutate(Washer.Type = factor(Washer.Type, levels = rowOrder)) %>% arrange(Washer.Type)  
item88.table <- data.frame(item88.table)


item88.final.SF <- item88.table[which(item88.table$BuildingType == "Single Family")
                                ,-which(colnames(item88.table) %in% c("BuildingType"))]
item88.final.MH <- item88.table[which(item88.table$BuildingType == "Manufactured")
                                ,-which(colnames(item88.table) %in% c("BuildingType"))]

exportTable(item88.final.SF, "SF", "Table 95", weighted = FALSE)
exportTable(item88.final.MH, "MH", "Table 76", weighted = FALSE)

