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
#Dryer.Vented
#
colnames(appliances.dat)[grep("wifi", colnames(appliances.dat), ignore.case = T)]
#  "Thermostat.Wifi"
# ,"STB.Wifi"
# ,"TV.Wifi"
# ,"Computer.Wifi"
# ,"Large.Unusual.Load.Wifi.Enabled"
# ,"Wifi.Enabled"


#for distribution of dryer fuel types by state: keep unknowns


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

# exportTable(item90.final.SF, "SF", "Table 97", weighted = TRUE)
exportTable(item90.final.MH, "MH", "Table 78", weighted = TRUE)


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

# exportTable(item90.final.SF, "SF", "Table 97", weighted = FALSE)
exportTable(item90.final.MH, "MH", "Table 78", weighted = FALSE)








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

# exportTable(item92.final.SF, "SF", "Table 99", weighted = TRUE)
exportTable(item92.final.MH, "MH", "Table 80", weighted = TRUE)


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

# exportTable(item92.final.SF, "SF", "Table 99", weighted = FALSE)
exportTable(item92.final.MH, "MH", "Table 80", weighted = FALSE)
