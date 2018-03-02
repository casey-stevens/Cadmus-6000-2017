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

# exportTable(item86.final.SF, "SF", "Table 93", weighted = TRUE)
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

# exportTable(item86.final.SF, "SF", "Table 93", weighted = FALSE)
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
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item87.table <- data.frame("BuildingType"    = item87.cast$BuildingType
                           ,"Washer.Type"    = item87.cast$Washer.Type
                           ,"Percent_ID"     = item87.cast$w.percent_ID
                           ,"SE_ID"          = item87.cast$w.SE_ID
                           ,"n_ID"           = item87.cast$n_ID
                           ,"Percent_MT"     = item87.cast$w.percent_MT
                           ,"SE_MT"          = item87.cast$w.SE_MT
                           ,"n_MT"           = item87.cast$n_MT
                           ,"Percent_OR"     = item87.cast$w.percent_OR
                           ,"SE_OR"          = item87.cast$w.SE_OR
                           ,"n_OR"           = item87.cast$n_OR
                           ,"Percent_WA"     = item87.cast$w.percent_WA
                           ,"SE_WA"          = item87.cast$w.SE_WA
                           ,"n_WA"           = item87.cast$n_WA
                           ,"Percent_Region" = item87.cast$w.percent_Region
                           ,"SE_Region"      = item87.cast$w.SE_Region
                           ,"n_Region"       = item87.cast$n_Region
                           ,"EB_ID"          = item87.cast$EB_ID
                           ,"EB_MT"          = item87.cast$EB_MT
                           ,"EB_OR"          = item87.cast$EB_OR
                           ,"EB_WA"          = item87.cast$EB_WA
                           ,"EB_Region"      = item87.cast$EB_Region
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

# exportTable(item87.final.SF, "SF", "Table 94", weighted = TRUE)
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
                           ,"Washer.Type"    = item87.cast$Washer.Type
                           ,"Percent_ID"     = item87.cast$Percent_ID
                           ,"SE_ID"          = item87.cast$SE_ID
                           ,"n_ID"           = item87.cast$n_ID
                           ,"Percent_MT"     = item87.cast$Percent_MT
                           ,"SE_MT"          = item87.cast$SE_MT
                           ,"n_MT"           = item87.cast$n_MT
                           ,"Percent_OR"     = item87.cast$Percent_OR
                           ,"SE_OR"          = item87.cast$SE_OR
                           ,"n_OR"           = item87.cast$n_OR
                           ,"Percent_WA"     = item87.cast$Percent_WA
                           ,"SE_WA"          = item87.cast$SE_WA
                           ,"n_WA"           = item87.cast$n_WA
                           ,"Percent_Region" = item87.cast$Percent_Region
                           ,"SE_Region"      = item87.cast$SE_Region
                           ,"n_Region"       = item87.cast$n_Region
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

# exportTable(item87.final.SF, "SF", "Table 94", weighted = FALSE)
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
item88.dat2$EquipVintage_bins_SF <- as.numeric(as.character(item88.dat2$Age))
item88.dat2$EquipVintage_bins_MH <- as.numeric(as.character(item88.dat2$Age))
item88.dat3 <- item88.dat2[which(!(is.na(item88.dat2$EquipVintage_bins_SF))),]

item88.dat3$EquipVintage_bins_SF[which(item88.dat3$Age < 1990)] <- "Pre 1990"
item88.dat3$EquipVintage_bins_SF[which(item88.dat3$Age >= 1990 & item88.dat3$Age < 1995)] <- "1990-1994"
item88.dat3$EquipVintage_bins_SF[which(item88.dat3$Age >= 1995 & item88.dat3$Age < 2000)] <- "1995-1999"
item88.dat3$EquipVintage_bins_SF[which(item88.dat3$Age >= 2000 & item88.dat3$Age < 2005)] <- "2000-2004"
item88.dat3$EquipVintage_bins_SF[which(item88.dat3$Age >= 2005 & item88.dat3$Age < 2010)] <- "2005-2009"
item88.dat3$EquipVintage_bins_SF[which(item88.dat3$Age >= 2010 & item88.dat3$Age < 2015)] <- "2010-2014"
item88.dat3$EquipVintage_bins_SF[which(item88.dat3$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item88.dat3$EquipVintage_bins_SF)

item88.dat3$EquipVintage_bins_MH[which(item88.dat3$Age < 1980)] <- "Pre 1980"
item88.dat3$EquipVintage_bins_MH[which(item88.dat3$Age >= 1980 & item88.dat3$Age < 1990)] <- "1980-1989"
item88.dat3$EquipVintage_bins_MH[which(item88.dat3$Age >= 1990 & item88.dat3$Age < 1995)] <- "1990-1994"
item88.dat3$EquipVintage_bins_MH[which(item88.dat3$Age >= 1995 & item88.dat3$Age < 2000)] <- "1995-1999"
item88.dat3$EquipVintage_bins_MH[which(item88.dat3$Age >= 2000 & item88.dat3$Age < 2005)] <- "2000-2004"
item88.dat3$EquipVintage_bins_MH[which(item88.dat3$Age >= 2005 & item88.dat3$Age < 2010)] <- "2005-2009"
item88.dat3$EquipVintage_bins_MH[which(item88.dat3$Age >= 2010 & item88.dat3$Age < 2015)] <- "2010-2014"
item88.dat3$EquipVintage_bins_MH[which(item88.dat3$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item88.dat3$EquipVintage_bins_MH)



item88.merge <- left_join(rbsa.dat, item88.dat3)
item88.merge <- item88.merge[which(!is.na(item88.merge$EquipVintage_bins_SF)),]



################################################
# Adding pop and sample sizes for weights
################################################
item88.data <- weightedData(item88.merge[-which(colnames(item88.merge) %in% c("Type"               
                                                                              ,"Age"
                                                                              ,"Washer.Type"
                                                                              ,"count"
                                                                              ,"EquipVintage_bins_SF"
                                                                              ,"EquipVintage_bins_MH"))])
item88.data <- left_join(item88.data, item88.merge[which(colnames(item88.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Type"               
                                                                                       ,"Age"
                                                                                       ,"Washer.Type"
                                                                                       ,"count"
                                                                                       ,"EquipVintage_bins_SF"
                                                                                       ,"EquipVintage_bins_MH"))])
#######################
# SINGLE FAMILY
#######################
#######################
# Weighted Analysis
#######################
item88.summary <- proportionRowsAndColumns1(CustomerLevelData = item88.data
                                            ,valueVariable    = 'count'
                                            ,columnVariable   = 'Washer.Type'
                                            ,rowVariable      = 'EquipVintage_bins_SF'
                                            ,aggregateColumnName = "Remove")
item88.summary <- item88.summary[which(item88.summary$Washer.Type %notin% c("Remove", "Total")),]
item88.summary$EquipVintage_bins_SF[which(item88.summary$EquipVintage_bins_SF == "Total")] <- "All Clothes Washer Types"

item88.all.washer.types <- proportions_one_group(CustomerLevelData = item88.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = "EquipVintage_bins_SF"
                                                 ,total.name = "All Clothes Washer Types"
                                                 ,columnName = "Washer.Type"
                                                 ,weighted = TRUE
                                                 ,two.prop.total = TRUE)
item88.all.washer.types$EquipVintage_bins_SF[which(item88.all.washer.types$EquipVintage_bins_SF == "Total")] <- "All Clothes Washer Types"


item88.final <- rbind.data.frame(item88.summary, item88.all.washer.types, stringsAsFactors = F)

item88.cast <- dcast(setDT(item88.final)
                     , formula = BuildingType + Washer.Type ~ EquipVintage_bins_SF
                     , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item88.table <- data.frame("BuildingType"           = item88.cast$BuildingType
                           ,"Washer.Type"           = item88.cast$Washer.Type
                           ,"Percent_Pre.1990"      = item88.cast$`w.percent_Pre 1990`
                           ,"SE_Pre.1990"           = item88.cast$`w.SE_Pre 1990`
                           ,"Percent_1990.1994"     = item88.cast$`w.percent_1990-1994`
                           ,"SE_1990.1994"          = item88.cast$`w.SE_1990-1994`
                           ,"Percent_1995.1999"     = item88.cast$`w.percent_1995-1999`
                           ,"SE_1995.1999"          = item88.cast$`w.SE_1995-1999`
                           ,"Percent_2000.2004"     = item88.cast$`w.percent_2000-2004`
                           ,"SE_2000.2004"          = item88.cast$`w.SE_2000-2004`
                           ,"Percent_2005.2009"     = item88.cast$`w.percent_2005-2009`
                           ,"SE_2005.2009"          = item88.cast$`w.SE_2005-2009`
                           ,"Percent_2010.2014"     = item88.cast$`w.percent_2010-2014`
                           ,"SE_2010.2014"          = item88.cast$`w.SE_2010-2014`
                           ,"Percent_Post.2014"     = item88.cast$`w.percent_Post 2014`
                           ,"SE_Post.2014"          = item88.cast$`w.SE_Post 2014`
                           ,"n"                     = item88.cast$`n_All Clothes Washer Types`
                           ,"EB_Pre.1990"           = item88.cast$`EB_Pre 1990`
                           ,"EB_1990.1994"          = item88.cast$`EB_1990-1994`
                           ,"EB_1995.1999"          = item88.cast$`EB_1995-1999`
                           ,"EB_2000.2004"          = item88.cast$`EB_2000-2004`
                           ,"EB_2005.2009"          = item88.cast$`EB_2005-2009`
                           ,"EB_2010.2014"          = item88.cast$`EB_2010-2014`
                           ,"EB_Post.2014"          = item88.cast$`EB_Post 2014`
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

# exportTable(item88.final.SF, "SF", "Table 95", weighted = TRUE)


#######################
# SINGLE FAMILY
#######################
#######################
# Unweighted Analysis
#######################
item88.summary <- proportions_two_groups_unweighted(CustomerLevelData = item88.data
                                                    ,valueVariable    = 'count'
                                                    ,columnVariable   = 'Washer.Type'
                                                    ,rowVariable      = 'EquipVintage_bins_SF'
                                                    ,aggregateColumnName = "Remove")
item88.summary <- item88.summary[which(item88.summary$Washer.Type %notin% c("Remove", "Total")),]
item88.summary$EquipVintage_bins_SF[which(item88.summary$EquipVintage_bins_SF == "Total")] <- "All Clothes Washer Types"

item88.all.washer.types <- proportions_one_group(CustomerLevelData = item88.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = "EquipVintage_bins_SF"
                                                 ,total.name = "All Clothes Washer Types"
                                                 ,columnName = "Washer.Type"
                                                 ,weighted = FALSE
                                                 ,two.prop.total = TRUE)
item88.all.washer.types <- item88.all.washer.types[which(item88.all.washer.types$EquipVintage_bins_SF != "Total"),]


item88.final <- rbind.data.frame(item88.summary, item88.all.washer.types, stringsAsFactors = F)

item88.cast <- dcast(setDT(item88.final)
                     , formula = BuildingType + Washer.Type ~ EquipVintage_bins_SF
                     , value.var = c("Percent", "SE", "Count", "n"))

item88.table <- data.frame("BuildingType"           = item88.cast$BuildingType
                           ,"Washer.Type"           = item88.cast$Washer.Type
                           ,"Percent_Pre.1990"      = item88.cast$`Percent_Pre 1990`
                           ,"SE_Pre.1990"           = item88.cast$`SE_Pre 1990`
                           ,"n_Pre.1990"            = item88.cast$`n_Pre 1990`
                           ,"Percent_1990.1994"     = item88.cast$`Percent_1990-1994`
                           ,"SE_1990.1994"          = item88.cast$`SE_1990-1994`
                           ,"n_1990.1994"           = item88.cast$`n_1990-1994`
                           ,"Percent_1995.1999"     = item88.cast$`Percent_1995-1999`
                           ,"SE_1995.1999"          = item88.cast$`SE_1995-1999`
                           ,"n_1995.1999"           = item88.cast$`n_1995-1999`
                           ,"Percent_2000.2004"     = item88.cast$`Percent_2000-2004`
                           ,"SE_2000.2004"          = item88.cast$`SE_2000-2004`
                           ,"n_2000.2004"           = item88.cast$`n_2000-2004`
                           ,"Percent_2005.2009"     = item88.cast$`Percent_2005-2009`
                           ,"SE_2005.2009"          = item88.cast$`SE_2005-2009`
                           ,"n_2005.2009"           = item88.cast$`n_2005-2009`
                           ,"Percent_2010.2014"     = item88.cast$`Percent_2010-2014`
                           ,"SE_2010.2014"          = item88.cast$`SE_2010-2014`
                           ,"n_2010.2014"           = item88.cast$`n_2010-2014`
                           ,"Percent_Post.2014"     = item88.cast$`Percent_Post 2014`
                           ,"SE_Post.2014"          = item88.cast$`SE_Post 2014`
                           ,"n_Post.2014"           = item88.cast$`n_Post 2014`
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

# exportTable(item88.final.SF, "SF", "Table 95", weighted = FALSE)





#######################
# Manufactured
#######################
#######################
# Weighted Analysis
#######################
item88.summary <- proportionRowsAndColumns1(CustomerLevelData = item88.data
                                            ,valueVariable    = 'count'
                                            ,columnVariable   = 'Washer.Type'
                                            ,rowVariable      = 'EquipVintage_bins_MH'
                                            ,aggregateColumnName = "Remove")
item88.summary <- item88.summary[which(item88.summary$Washer.Type %notin% c("Remove", "Total")),]
item88.summary$EquipVintage_bins_MH[which(item88.summary$EquipVintage_bins_MH == "Total")] <- "All Clothes Washer Types"

item88.all.washer.types <- proportions_one_group(CustomerLevelData = item88.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = "EquipVintage_bins_MH"
                                                 ,total.name = "All Clothes Washer Types"
                                                 ,columnName = "Washer.Type"
                                                 ,weighted = TRUE
                                                 ,two.prop.total = TRUE)
item88.all.washer.types$EquipVintage_bins_MH[which(item88.all.washer.types$EquipVintage_bins_MH == "Total")] <- "All Clothes Washer Types"


item88.final <- rbind.data.frame(item88.summary, item88.all.washer.types, stringsAsFactors = F)

item88.cast <- dcast(setDT(item88.final)
                     , formula = BuildingType + Washer.Type ~ EquipVintage_bins_MH
                     , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item88.cast)
item88.table <- data.frame("BuildingType"           = item88.cast$BuildingType
                           ,"Washer.Type"           = item88.cast$Washer.Type
                           ,"Percent_Pre.1980"      = 0#item88.cast$`w.percent_Pre 1980`
                           ,"SE_Pre.1980"           = NA#item88.cast$`w.SE_Pre 1980`
                           ,"Percent_1980.1989"     = item88.cast$`w.percent_1980-1989`
                           ,"SE_1980.1989"          = item88.cast$`w.SE_1980-1989`
                           ,"Percent_1990.1994"     = item88.cast$`w.percent_1990-1994`
                           ,"SE_1990.1994"          = item88.cast$`w.SE_1990-1994`
                           ,"Percent_1995.1999"     = item88.cast$`w.percent_1995-1999`
                           ,"SE_1995.1999"          = item88.cast$`w.SE_1995-1999`
                           ,"Percent_2000.2004"     = item88.cast$`w.percent_2000-2004`
                           ,"SE_2000.2004"          = item88.cast$`w.SE_2000-2004`
                           ,"Percent_2005.2009"     = item88.cast$`w.percent_2005-2009`
                           ,"SE_2005.2009"          = item88.cast$`w.SE_2005-2009`
                           ,"Percent_2010.2014"     = item88.cast$`w.percent_2010-2014`
                           ,"SE_2010.2014"          = item88.cast$`w.SE_2010-2014`
                           ,"Percent_Post.2014"     = item88.cast$`w.percent_Post 2014`
                           ,"SE_Post.2014"          = item88.cast$`w.SE_Post 2014`
                           ,"n"                     = item88.cast$`n_All Clothes Washer Types`
                           ,"EB_Pre.1980"           = NA#item88.cast$`EB_Pre 1980`
                           ,"EB_1980.1989"          = item88.cast$`EB_1980-1989`
                           ,"EB_1990.1994"          = item88.cast$`EB_1990-1994`
                           ,"EB_1995.1999"          = item88.cast$`EB_1995-1999`
                           ,"EB_2000.2004"          = item88.cast$`EB_2000-2004`
                           ,"EB_2005.2009"          = item88.cast$`EB_2005-2009`
                           ,"EB_2010.2014"          = item88.cast$`EB_2010-2014`
                           ,"EB_Post.2014"          = item88.cast$`EB_Post 2014`
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

item88.final.MH <- item88.table[which(item88.table$BuildingType == "Manufactured")
                                ,-which(colnames(item88.table) %in% c("BuildingType"))]

exportTable(item88.final.MH, "MH", "Table 76", weighted = TRUE)


#######################
# Manufactured
#######################
#######################
# Unweighted Analysis
#######################
item88.summary <- proportions_two_groups_unweighted(CustomerLevelData = item88.data
                                                    ,valueVariable    = 'count'
                                                    ,columnVariable   = 'Washer.Type'
                                                    ,rowVariable      = 'EquipVintage_bins_MH'
                                                    ,aggregateColumnName = "Remove")
item88.summary <- item88.summary[which(item88.summary$Washer.Type %notin% c("Remove", "Total")),]
item88.summary$EquipVintage_bins_MH[which(item88.summary$EquipVintage_bins_MH == "Total")] <- "All Clothes Washer Types"

item88.all.washer.types <- proportions_one_group(CustomerLevelData = item88.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = "EquipVintage_bins_MH"
                                                 ,total.name = "All Clothes Washer Types"
                                                 ,columnName = "Washer.Type"
                                                 ,weighted = FALSE
                                                 ,two.prop.total = TRUE)
item88.all.washer.types <- item88.all.washer.types[which(item88.all.washer.types$EquipVintage_bins_MH != "Total"),]


item88.final <- rbind.data.frame(item88.summary, item88.all.washer.types, stringsAsFactors = F)

item88.cast <- dcast(setDT(item88.final)
                     , formula = BuildingType + Washer.Type ~ EquipVintage_bins_MH
                     , value.var = c("Percent", "SE", "Count", "n"))

item88.table <- data.frame("BuildingType"           = item88.cast$BuildingType
                           ,"Washer.Type"           = item88.cast$Washer.Type
                           ,"Percent_Pre.1980"      = 0#item88.cast$`Percent_Pre 1980`
                           ,"SE_Pre.1980"           = NA#item88.cast$`SE_Pre 1980`
                           ,"Percent_1980.1989"     = item88.cast$`Percent_1980-1989`
                           ,"SE_1980.1989"          = item88.cast$`SE_1980-1989`
                           ,"Percent_1990.1994"     = item88.cast$`Percent_1990-1994`
                           ,"SE_1990.1994"          = item88.cast$`SE_1990-1994`
                           ,"Percent_1995.1999"     = item88.cast$`Percent_1995-1999`
                           ,"SE_1995.1999"          = item88.cast$`SE_1995-1999`
                           ,"Percent_2000.2004"     = item88.cast$`Percent_2000-2004`
                           ,"SE_2000.2004"          = item88.cast$`SE_2000-2004`
                           ,"Percent_2005.2009"     = item88.cast$`Percent_2005-2009`
                           ,"SE_2005.2009"          = item88.cast$`SE_2005-2009`
                           ,"Percent_2010.2014"     = item88.cast$`Percent_2010-2014`
                           ,"SE_2010.2014"          = item88.cast$`SE_2010-2014`
                           ,"Percent_Post.2014"     = item88.cast$`Percent_Post 2014`
                           ,"SE_Post.2014"          = item88.cast$`SE_Post 2014`
                           ,"n"                     = item88.cast$`n_All Clothes Washer Types`
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

item88.final.MH <- item88.table[which(item88.table$BuildingType == "Manufactured")
                                ,-which(colnames(item88.table) %in% c("BuildingType"))]

exportTable(item88.final.MH, "MH", "Table 76", weighted = FALSE)



























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
#Item 86: DISTRIBUTION OF CLOTHES WASHERS BY VINTAGE (SF table 93, MH table 74)
#############################################################################################
#subset to columns needed for analysis
item86.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"))]
item86.os.dat$count <- 1

item86.os.dat0 <- item86.os.dat[which(item86.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item86.os.dat1 <- left_join(item86.os.dat0, os.dat, by = "CK_Cadmus_ID")

item86.os.dat2 <- item86.os.dat1[which(item86.os.dat1$Type == "Washer"),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item86.os.dat2$EquipVintage_bins <- as.numeric(as.character(item86.os.dat2$Age))
item86.os.dat3 <- item86.os.dat2[which(!(is.na(item86.os.dat2$EquipVintage_bins))),]

item86.os.dat3$EquipVintage_bins[which(item86.os.dat3$Age < 1980)] <- "Pre 1980"
item86.os.dat3$EquipVintage_bins[which(item86.os.dat3$Age >= 1980 & item86.os.dat3$Age < 1990)] <- "1980-1989"
item86.os.dat3$EquipVintage_bins[which(item86.os.dat3$Age >= 1990 & item86.os.dat3$Age < 1995)] <- "1990-1994"
item86.os.dat3$EquipVintage_bins[which(item86.os.dat3$Age >= 1995 & item86.os.dat3$Age < 2000)] <- "1995-1999"
item86.os.dat3$EquipVintage_bins[which(item86.os.dat3$Age >= 2000 & item86.os.dat3$Age < 2005)] <- "2000-2004"
item86.os.dat3$EquipVintage_bins[which(item86.os.dat3$Age >= 2005 & item86.os.dat3$Age < 2010)] <- "2005-2009"
item86.os.dat3$EquipVintage_bins[which(item86.os.dat3$Age >= 2010 & item86.os.dat3$Age < 2015)] <- "2010-2014"
item86.os.dat3$EquipVintage_bins[which(item86.os.dat3$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item86.os.dat3$EquipVintage_bins)

item86.os.merge <- left_join(os.dat, item86.os.dat3)
item86.os.merge <- item86.os.merge[which(!is.na(item86.os.merge$EquipVintage_bins)),]

################################################
# Adding pop and sample sizes for weights
################################################
item86.os.data <- weightedData(item86.os.merge[-which(colnames(item86.os.merge) %in% c("count"
                                                                              ,"Type"
                                                                              ,"Age"
                                                                              ,"EquipVintage_bins"))])
item86.os.data <- left_join(item86.os.data, unique(item86.os.merge[which(colnames(item86.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"count"
                                                                                       ,"Type"
                                                                                       ,"Age"
                                                                                       ,"EquipVintage_bins"))]))
item86.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item86.os.final <- proportionRowsAndColumns1(CustomerLevelData = item86.os.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = "EquipVintage_bins"
                                             ,aggregateColumnName = "Remove")
item86.os.final <- item86.os.final[which(item86.os.final$CK_Building_ID != "Remove"),]

item86.os.cast <- dcast(setDT(item86.os.final)
                        ,formula = BuildingType + EquipVintage_bins ~ CK_Building_ID
                        ,value.var = c("w.percent", "w.SE","n", "EB"))

names(item86.os.cast)
if(os.ind == "scl"){
  item86.os.final <- data.frame("BuildingType"          = item86.os.cast$BuildingType
                                ,"EquipVintage_bins"    = item86.os.cast$EquipVintage_bins
                                ,"Percent_SCL.GenPop"   = item86.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item86.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item86.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item86.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item86.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item86.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item86.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item86.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item86.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item86.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item86.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item86.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item86.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item86.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item86.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item86.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item86.os.final <- data.frame("BuildingType"          = item86.os.cast$BuildingType
                                ,"EquipVintage_bins"    = item86.os.cast$EquipVintage_bins
                                ,"Percent_SnoPUD"          = item86.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item86.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item86.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item86.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item86.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item86.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item86.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item86.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item86.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item86.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item86.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item86.os.cast$`EB_2017 RBSA NW`)


}

unique(item86.os.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item86.os.table <- item86.os.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item86.os.table <- data.frame(item86.os.table)


item86.os.final.SF <- item86.os.table[which(item86.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item86.os.table) %in% c("BuildingType"))]

exportTable(item86.os.final.SF, "SF", "Table 93", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item86.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item86.os.data
                                                     ,valueVariable = "count"
                                                     ,columnVariable = "CK_Building_ID"
                                                     ,rowVariable = "EquipVintage_bins"
                                                     ,aggregateColumnName = "Remove")
item86.os.final <- item86.os.final[which(item86.os.final$CK_Building_ID != "Remove"),]

item86.os.cast <- dcast(setDT(item86.os.final)
                        ,formula = BuildingType + EquipVintage_bins ~ CK_Building_ID
                        ,value.var = c("Percent", "SE","n"))

names(item86.os.cast)
if(os.ind == "scl"){
  item86.os.final <- data.frame("BuildingType"          = item86.os.cast$BuildingType
                                ,"EquipVintage_bins"    = item86.os.cast$EquipVintage_bins
                                ,"Percent_SCL.GenPop"   = item86.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item86.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item86.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item86.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item86.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item86.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item86.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item86.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item86.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item86.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item86.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item86.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item86.os.final <- data.frame("BuildingType"          = item86.os.cast$BuildingType
                                ,"EquipVintage_bins"    = item86.os.cast$EquipVintage_bins
                                ,"Percent_SnoPUD"          = item86.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item86.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item86.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item86.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item86.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item86.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item86.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item86.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item86.os.cast$`n_2017 RBSA NW`)
}




unique(item86.os.final$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item86.os.table <- item86.os.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item86.os.table <- data.frame(item86.os.table)

item86.os.final.SF <- item86.os.table[which(item86.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item86.os.table) %in% c("BuildingType"))]

exportTable(item86.os.final.SF, "SF", "Table 93", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
#Item 87: DISTRIBUTION OF CLOTHES WASHERS BY TYPE AND CK_Building_ID (SF table 94)
#############################################################################################
#subset to columns needed for analysis
item87.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,"Washer.Type"))]
item87.os.dat$count <- 1

item87.os.dat0 <- item87.os.dat[which(item87.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item87.os.dat1 <- left_join(item87.os.dat0, os.dat, by = "CK_Cadmus_ID")


item87.os.dat2 <- item87.os.dat1[which(item87.os.dat1$Type == "Washer"),]

item87.os.merge <- left_join(os.dat, item87.os.dat2)
item87.os.merge <- item87.os.merge[which(!is.na(item87.os.merge$Type)),]
item87.os.merge <- item87.os.merge[which(item87.os.merge$Type != "Unknown"),]



################################################
# Adding pop and sample sizes for weights
################################################
item87.os.data <- weightedData(item87.os.merge[-which(colnames(item87.os.merge) %in% c("Type"               
                                                                              ,"Age"
                                                                              ,"Washer.Type"
                                                                              ,"count"))])
item87.os.data <- left_join(item87.os.data, unique(item87.os.merge[which(colnames(item87.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Type"               
                                                                                       ,"Age"
                                                                                       ,"Washer.Type"
                                                                                       ,"count"))]))
#######################
# Weighted Analysis
#######################
item87.os.final <- proportionRowsAndColumns1(CustomerLevelData = item87.os.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'CK_Building_ID'
                                          ,rowVariable      = 'Washer.Type'
                                          ,aggregateColumnName = "Remove")

item87.os.cast <- dcast(setDT(item87.os.final)
                     , formula = BuildingType + Washer.Type ~ CK_Building_ID
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))


names(item87.os.cast)
if(os.ind == "scl"){
  item87.os.table <- data.frame("BuildingType"       = item87.os.cast$BuildingType
                                ,"Washer.Type"          = item87.os.cast$Washer.Type
                                ,"Percent_SCL.GenPop"   = item87.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item87.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item87.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item87.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item87.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item87.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item87.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item87.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item87.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item87.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item87.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item87.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item87.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item87.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item87.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item87.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item87.os.table <- data.frame("BuildingType"       = item87.os.cast$BuildingType
                                ,"Washer.Type"          = item87.os.cast$Washer.Type
                                ,"Percent_SnoPUD"          = item87.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item87.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item87.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item87.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item87.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item87.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item87.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item87.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item87.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item87.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item87.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item87.os.cast$`EB_2017 RBSA NW`)
}



unique(item87.os.table$Washer.Type)
rowOrder <- c("Combined Washer/Dryer in one drum"
              ,"Horizontal Axis"
              ,"Stacked Washer/Dryer"
              ,"Vertical Axis (with agitator)"
              ,"Vertical Axis (without agitator)"
              ,"Unknown"
              ,"Total")
item87.os.table <- item87.os.table %>% mutate(Washer.Type = factor(Washer.Type, levels = rowOrder)) %>% arrange(Washer.Type)  
item87.os.table <- data.frame(item87.os.table)

item87.os.final.SF <- item87.os.table[which(item87.os.table$BuildingType == "Single Family")
                                ,which(colnames(item87.os.table) %notin% c("BuildingType"))]

exportTable(item87.os.final.SF, "SF", "Table 94", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item87.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item87.os.data
                                                  ,valueVariable    = 'count'
                                                  ,columnVariable   = 'CK_Building_ID'
                                                  ,rowVariable      = 'Washer.Type'
                                                  ,aggregateColumnName = "Remove")

item87.os.cast <- dcast(setDT(item87.os.final)
                     , formula = BuildingType + Washer.Type ~ CK_Building_ID
                     , value.var = c("Percent", "SE", "Count", "n"))


names(item87.os.cast)
if(os.ind == "scl"){
  item87.os.table <- data.frame("BuildingType"    = item87.os.cast$BuildingType
                                ,"Washer.Type"    = item87.os.cast$Washer.Type
                                ,"Percent_SCL.GenPop"   = item87.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item87.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item87.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item87.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item87.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item87.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item87.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item87.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item87.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item87.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item87.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item87.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item87.os.table <- data.frame("BuildingType"    = item87.os.cast$BuildingType
                                ,"Washer.Type"    = item87.os.cast$Washer.Type
                                ,"Percent_SnoPUD"          = item87.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item87.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item87.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item87.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item87.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item87.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item87.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item87.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item87.os.cast$`n_2017 RBSA NW`)
}



unique(item87.os.table$Washer.Type)
rowOrder <- c("Combined Washer/Dryer in one drum"
              ,"Horizontal Axis"
              ,"Stacked Washer/Dryer"
              ,"Vertical Axis (with agitator)"
              ,"Vertical Axis (without agitator)"
              ,"Unknown"
              ,"Total")
item87.os.table <- item87.os.table %>% mutate(Washer.Type = factor(Washer.Type, levels = rowOrder)) %>% arrange(Washer.Type)  
item87.os.table <- data.frame(item87.os.table)

item87.os.final.SF <- item87.os.table[which(item87.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item87.os.table) %in% c("BuildingType"))]

exportTable(item87.os.final.SF, "SF", "Table 94", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
#Item 88: DISTRIBUTION OF CLOTHES WASHERS BY TYPE AND VINTAGE (SF table 95, MH table 76)
#############################################################################################
#subset to columns needed for analysis
item88.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"Age"
                                                                   ,"Washer.Type"))]
item88.os.dat$count <- 1

item88.os.dat0 <- item88.os.dat[which(item88.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item88.os.dat1 <- left_join(item88.os.dat0, os.dat, by = "CK_Cadmus_ID")

item88.os.dat2 <- item88.os.dat1[which(item88.os.dat1$Type == "Washer"),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item88.os.dat2$EquipVintage_bins_SF <- as.numeric(as.character(item88.os.dat2$Age))
item88.os.dat3 <- item88.os.dat2[which(!(is.na(item88.os.dat2$EquipVintage_bins_SF))),]

item88.os.dat3$EquipVintage_bins_SF[which(item88.os.dat3$Age < 1990)] <- "Pre 1990"
item88.os.dat3$EquipVintage_bins_SF[which(item88.os.dat3$Age >= 1990 & item88.os.dat3$Age < 1995)] <- "1990-1994"
item88.os.dat3$EquipVintage_bins_SF[which(item88.os.dat3$Age >= 1995 & item88.os.dat3$Age < 2000)] <- "1995-1999"
item88.os.dat3$EquipVintage_bins_SF[which(item88.os.dat3$Age >= 2000 & item88.os.dat3$Age < 2005)] <- "2000-2004"
item88.os.dat3$EquipVintage_bins_SF[which(item88.os.dat3$Age >= 2005 & item88.os.dat3$Age < 2010)] <- "2005-2009"
item88.os.dat3$EquipVintage_bins_SF[which(item88.os.dat3$Age >= 2010 & item88.os.dat3$Age < 2015)] <- "2010-2014"
item88.os.dat3$EquipVintage_bins_SF[which(item88.os.dat3$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item88.os.dat3$EquipVintage_bins_SF)

item88.os.merge <- left_join(os.dat, item88.os.dat3)
item88.os.merge <- item88.os.merge[which(!is.na(item88.os.merge$EquipVintage_bins_SF)),]
item88.os.merge <- item88.os.merge[which(item88.os.merge$CK_Building_ID == subset.ind),]


################################################
# Adding pop and sample sizes for weights
################################################
item88.os.data <- weightedData(item88.os.merge[-which(colnames(item88.os.merge) %in% c("Type"               
                                                                                       ,"Age"
                                                                                       ,"Washer.Type"
                                                                                       ,"count"
                                                                                       ,"EquipVintage_bins_SF"))])
item88.os.data <- left_join(item88.os.data, unique(item88.os.merge[which(colnames(item88.os.merge) %in% c("CK_Cadmus_ID"
                                                                                                          ,"Type"               
                                                                                                          ,"Age"
                                                                                                          ,"Washer.Type"
                                                                                                          ,"count"
                                                                                                          ,"EquipVintage_bins_SF"))]))
#######################
# Weighted Analysis
#######################
item88.os.summary <- proportionRowsAndColumns1(CustomerLevelData = item88.os.data
                                            ,valueVariable    = 'count'
                                            ,columnVariable   = 'Washer.Type'
                                            ,rowVariable      = 'EquipVintage_bins_SF'
                                            ,aggregateColumnName = "Remove")
item88.os.summary <- item88.os.summary[which(item88.os.summary$Washer.Type %notin% c("Remove", "Total")),]
item88.os.summary$EquipVintage_bins_SF[which(item88.os.summary$EquipVintage_bins_SF == "Total")] <- "All Clothes Washer Types"

item88.os.all.washer.types <- proportions_one_group(CustomerLevelData = item88.os.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = "EquipVintage_bins_SF"
                                                 ,total.name = "All Clothes Washer Types"
                                                 ,columnName = "Washer.Type"
                                                 ,weighted = TRUE
                                                 ,two.prop.total = TRUE)
item88.os.all.washer.types$EquipVintage_bins_SF[which(item88.os.all.washer.types$EquipVintage_bins_SF == "Total")] <- "All Clothes Washer Types"


item88.os.final <- rbind.data.frame(item88.os.summary, item88.os.all.washer.types, stringsAsFactors = F)

item88.os.cast <- dcast(setDT(item88.os.final)
                     , formula = BuildingType + Washer.Type ~ EquipVintage_bins_SF
                     , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

names(item88.os.cast)
if(os.ind == "scl"){
  item88.os.table <- data.frame("BuildingType"           = item88.os.cast$BuildingType
                                ,"Washer.Type"           = item88.os.cast$Washer.Type
                                ,"Percent_Pre.1990"      = item88.os.cast$`w.percent_Pre 1990`
                                ,"SE_Pre.1990"           = item88.os.cast$`w.SE_Pre 1990`
                                ,"Percent_1990.1994"     = item88.os.cast$`w.percent_1990-1994`
                                ,"SE_1990.1994"          = item88.os.cast$`w.SE_1990-1994`
                                ,"Percent_1995.1999"     = item88.os.cast$`w.percent_1995-1999`
                                ,"SE_1995.1999"          = item88.os.cast$`w.SE_1995-1999`
                                ,"Percent_2000.2004"     = item88.os.cast$`w.percent_2000-2004`
                                ,"SE_2000.2004"          = item88.os.cast$`w.SE_2000-2004`
                                ,"Percent_2005.2009"     = item88.os.cast$`w.percent_2005-2009`
                                ,"SE_2005.2009"          = item88.os.cast$`w.SE_2005-2009`
                                ,"Percent_2010.2014"     = item88.os.cast$`w.percent_2010-2014`
                                ,"SE_2010.2014"          = item88.os.cast$`w.SE_2010-2014`
                                ,"Percent_Post.2014"     = item88.os.cast$`w.percent_Post 2014`
                                ,"SE_Post.2014"          = item88.os.cast$`w.SE_Post 2014`
                                ,"n"                     = item88.os.cast$`n_All Clothes Washer Types`
                                ,"EB_Pre.1990"           = item88.os.cast$`EB_Pre 1990`
                                ,"EB_1990.1994"          = item88.os.cast$`EB_1990-1994`
                                ,"EB_1995.1999"          = item88.os.cast$`EB_1995-1999`
                                ,"EB_2000.2004"          = item88.os.cast$`EB_2000-2004`
                                ,"EB_2005.2009"          = item88.os.cast$`EB_2005-2009`
                                ,"EB_2010.2014"          = item88.os.cast$`EB_2010-2014`
                                ,"EB_Post.2014"          = item88.os.cast$`EB_Post 2014`)
}else if(os.ind == "snopud"){
  item88.os.table <- data.frame("BuildingType"           = item88.os.cast$BuildingType
                                ,"Washer.Type"           = item88.os.cast$Washer.Type
                                ,"Percent_Pre.1990"      = NA#item88.os.cast$`w.percent_Pre 1990`
                                ,"SE_Pre.1990"           = NA#item88.os.cast$`w.SE_Pre 1990`
                                ,"Percent_1990.1994"     = NA#item88.os.cast$`w.percent_1990-1994`
                                ,"SE_1990.1994"          = NA#item88.os.cast$`w.SE_1990-1994`
                                ,"Percent_1995.1999"     = item88.os.cast$`w.percent_1995-1999`
                                ,"SE_1995.1999"          = item88.os.cast$`w.SE_1995-1999`
                                ,"Percent_2000.2004"     = item88.os.cast$`w.percent_2000-2004`
                                ,"SE_2000.2004"          = item88.os.cast$`w.SE_2000-2004`
                                ,"Percent_2005.2009"     = item88.os.cast$`w.percent_2005-2009`
                                ,"SE_2005.2009"          = item88.os.cast$`w.SE_2005-2009`
                                ,"Percent_2010.2014"     = item88.os.cast$`w.percent_2010-2014`
                                ,"SE_2010.2014"          = item88.os.cast$`w.SE_2010-2014`
                                ,"Percent_Post.2014"     = item88.os.cast$`w.percent_Post 2014`
                                ,"SE_Post.2014"          = item88.os.cast$`w.SE_Post 2014`
                                ,"n"                     = item88.os.cast$`n_All Clothes Washer Types`
                                ,"EB_Pre.1990"           = NA#item88.os.cast$`EB_Pre 1990`
                                ,"EB_1990.1994"          = NA#item88.os.cast$`EB_1990-1994`
                                ,"EB_1995.1999"          = item88.os.cast$`EB_1995-1999`
                                ,"EB_2000.2004"          = item88.os.cast$`EB_2000-2004`
                                ,"EB_2005.2009"          = item88.os.cast$`EB_2005-2009`
                                ,"EB_2010.2014"          = item88.os.cast$`EB_2010-2014`
                                ,"EB_Post.2014"          = item88.os.cast$`EB_Post 2014`)
}



unique(item88.os.table$Washer.Type)
rowOrder <- c("Combined Washer/Dryer in one drum"
              ,"Horizontal Axis"
              ,"Stacked Washer/Dryer"
              ,"Vertical Axis (with agitator)"
              ,"Vertical Axis (without agitator)"
              ,"Unknown"
              ,"All Clothes Washer Types")
item88.os.table <- item88.os.table %>% mutate(Washer.Type = factor(Washer.Type, levels = rowOrder)) %>% arrange(Washer.Type)  
item88.os.table <- data.frame(item88.os.table)


item88.os.final.SF <- item88.os.table[which(item88.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item88.os.table) %in% c("BuildingType"))]

exportTable(item88.os.final.SF, "SF", "Table 95", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item88.os.summary <- proportions_two_groups_unweighted(CustomerLevelData = item88.os.data
                                                    ,valueVariable    = 'count'
                                                    ,columnVariable   = 'Washer.Type'
                                                    ,rowVariable      = 'EquipVintage_bins_SF'
                                                    ,aggregateColumnName = "Remove")
item88.os.summary <- item88.os.summary[which(item88.os.summary$Washer.Type %notin% c("Remove", "Total")),]
item88.os.summary$EquipVintage_bins_SF[which(item88.os.summary$EquipVintage_bins_SF == "Total")] <- "All Clothes Washer Types"

item88.os.all.washer.types <- proportions_one_group(CustomerLevelData = item88.os.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = "EquipVintage_bins_SF"
                                                 ,total.name = "All Clothes Washer Types"
                                                 ,columnName = "Washer.Type"
                                                 ,weighted = FALSE
                                                 ,two.prop.total = TRUE)
item88.os.all.washer.types <- item88.os.all.washer.types[which(item88.os.all.washer.types$EquipVintage_bins_SF != "Total"),]


item88.os.final <- rbind.data.frame(item88.os.summary, item88.os.all.washer.types, stringsAsFactors = F)

item88.os.cast <- dcast(setDT(item88.os.final)
                     , formula = BuildingType + Washer.Type ~ EquipVintage_bins_SF
                     , value.var = c("Percent", "SE", "Count", "n"))


if(os.ind == "scl"){
  item88.os.table <- data.frame("BuildingType"           = item88.os.cast$BuildingType
                                ,"Washer.Type"           = item88.os.cast$Washer.Type
                                ,"Percent_Pre.1990"      = item88.os.cast$`Percent_Pre 1990`
                                ,"SE_Pre.1990"           = item88.os.cast$`SE_Pre 1990`
                                ,"n_Pre.1990"            = item88.os.cast$`n_Pre 1990`
                                ,"Percent_1990.1994"     = item88.os.cast$`Percent_1990-1994`
                                ,"SE_1990.1994"          = item88.os.cast$`SE_1990-1994`
                                ,"n_1990.1994"           = item88.os.cast$`n_1990-1994`
                                ,"Percent_1995.1999"     = item88.os.cast$`Percent_1995-1999`
                                ,"SE_1995.1999"          = item88.os.cast$`SE_1995-1999`
                                ,"n_1995.1999"           = item88.os.cast$`n_1995-1999`
                                ,"Percent_2000.2004"     = item88.os.cast$`Percent_2000-2004`
                                ,"SE_2000.2004"          = item88.os.cast$`SE_2000-2004`
                                ,"n_2000.2004"           = item88.os.cast$`n_2000-2004`
                                ,"Percent_2005.2009"     = item88.os.cast$`Percent_2005-2009`
                                ,"SE_2005.2009"          = item88.os.cast$`SE_2005-2009`
                                ,"n_2005.2009"           = item88.os.cast$`n_2005-2009`
                                ,"Percent_2010.2014"     = item88.os.cast$`Percent_2010-2014`
                                ,"SE_2010.2014"          = item88.os.cast$`SE_2010-2014`
                                ,"n_2010.2014"           = item88.os.cast$`n_2010-2014`
                                ,"Percent_Post.2014"     = item88.os.cast$`Percent_Post 2014`
                                ,"SE_Post.2014"          = item88.os.cast$`SE_Post 2014`
                                ,"n_Post.2014"           = item88.os.cast$`n_Post 2014`)
}else if(os.ind == "snopud"){
  item88.os.table <- data.frame("BuildingType"           = item88.os.cast$BuildingType
                                ,"Washer.Type"           = item88.os.cast$Washer.Type
                                ,"Percent_Pre.1990"      = NA#item88.os.cast$`Percent_Pre 1990`
                                ,"SE_Pre.1990"           = NA#item88.os.cast$`SE_Pre 1990`
                                ,"n_Pre.1990"            = NA#item88.os.cast$`n_Pre 1990`
                                ,"Percent_1990.1994"     = NA#item88.os.cast$`Percent_1990-1994`
                                ,"SE_1990.1994"          = NA#item88.os.cast$`SE_1990-1994`
                                ,"n_1990.1994"           = NA#item88.os.cast$`n_1990-1994`
                                ,"Percent_1995.1999"     = item88.os.cast$`Percent_1995-1999`
                                ,"SE_1995.1999"          = item88.os.cast$`SE_1995-1999`
                                ,"n_1995.1999"           = item88.os.cast$`n_1995-1999`
                                ,"Percent_2000.2004"     = item88.os.cast$`Percent_2000-2004`
                                ,"SE_2000.2004"          = item88.os.cast$`SE_2000-2004`
                                ,"n_2000.2004"           = item88.os.cast$`n_2000-2004`
                                ,"Percent_2005.2009"     = item88.os.cast$`Percent_2005-2009`
                                ,"SE_2005.2009"          = item88.os.cast$`SE_2005-2009`
                                ,"n_2005.2009"           = item88.os.cast$`n_2005-2009`
                                ,"Percent_2010.2014"     = item88.os.cast$`Percent_2010-2014`
                                ,"SE_2010.2014"          = item88.os.cast$`SE_2010-2014`
                                ,"n_2010.2014"           = item88.os.cast$`n_2010-2014`
                                ,"Percent_Post.2014"     = item88.os.cast$`Percent_Post 2014`
                                ,"SE_Post.2014"          = item88.os.cast$`SE_Post 2014`
                                ,"n_Post.2014"           = item88.os.cast$`n_Post 2014`)
}



unique(item88.os.table$Washer.Type)
rowOrder <- c("Combined Washer/Dryer in one drum"
              ,"Horizontal Axis"
              ,"Stacked Washer/Dryer"
              ,"Vertical Axis (with agitator)"
              ,"Vertical Axis (without agitator)"
              ,"Unknown"
              ,"All Clothes Washer Types")
item88.os.table <- item88.os.table %>% mutate(Washer.Type = factor(Washer.Type, levels = rowOrder)) %>% arrange(Washer.Type)  
item88.os.table <- data.frame(item88.os.table)


item88.os.final.SF <- item88.os.table[which(item88.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item88.os.table) %in% c("BuildingType"))]

exportTable(item88.os.final.SF, "SF", "Table 95", weighted = FALSE, osIndicator = export.ind, OS = T)
