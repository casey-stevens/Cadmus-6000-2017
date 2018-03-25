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
# download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx', mechanical.export, mode = 'wb')
# mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))


#############################################################################################
#Item 105: DISTRIBUTION OF WATER HEATERS BY VINTAGE (SF table 112, MH table 87)
#############################################################################################
#subset to columns needed for analysis
item105.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Location"
                                                                    ,"DHW.Size.(Gallons)"
                                                                    ,"DHW.Year.Manufactured"))]
item105.dat$count <- 1

item105.dat0 <- item105.dat[which(item105.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item105.dat1 <- left_join( rbsa.dat,item105.dat0, by = "CK_Cadmus_ID")

item105.dat2 <- item105.dat1[which(!(is.na(item105.dat1$DHW.Year.Manufactured))),]
item105.dat3 <- item105.dat2[which(!(item105.dat2$DHW.Year.Manufactured %in% c("-- Datapoint not asked for --", "Unknown"))),]
unique(item105.dat3$DHW.Year.Manufactured)

# Bin equipment vintages for items 50 and 52 (4 categories)
item105.dat3$EquipVintage_bins <- as.numeric(as.character(item105.dat3$DHW.Year.Manufactured))
item105.dat4 <- item105.dat3[which(!is.na(item105.dat3$EquipVintage_bins)),]
item105.dat4$EquipVintage_bins[which(item105.dat4$DHW.Year.Manufactured < 1990)] <- "Pre 1990"
item105.dat4$EquipVintage_bins[which(item105.dat4$DHW.Year.Manufactured >= 1990 & item105.dat4$DHW.Year.Manufactured < 2000)] <- "1990-1999"
item105.dat4$EquipVintage_bins[which(item105.dat4$DHW.Year.Manufactured >= 2000 & item105.dat4$DHW.Year.Manufactured < 2005)] <- "2000-2004"
item105.dat4$EquipVintage_bins[which(item105.dat4$DHW.Year.Manufactured >= 2005 & item105.dat4$DHW.Year.Manufactured < 2010)] <- "2005-2009"
item105.dat4$EquipVintage_bins[which(item105.dat4$DHW.Year.Manufactured >= 2010 & item105.dat4$DHW.Year.Manufactured < 2015)] <- "2010-2014"
item105.dat4$EquipVintage_bins[which(item105.dat4$DHW.Year.Manufactured >= 2015)] <- "Post 2014"
#check uniques
unique(item105.dat4$EquipVintage_bins)


################################################
# Adding pop and sample sizes for weights
################################################
item105.data <- weightedData(item105.dat4[-which(colnames(item105.dat4) %in% c("Generic"
                                                                               ,"DHW.Size.(Gallons)"
                                                                               ,"DHW.Year.Manufactured"
                                                                               ,"DHW.Fuel"
                                                                               ,"DHW.Location"
                                                                               ,"count"
                                                                               ,"EquipVintage_bins"))])
item105.data <- left_join(item105.data, item105.dat4[which(colnames(item105.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"Generic"
                                                                                         ,"DHW.Size.(Gallons)"
                                                                                         ,"DHW.Year.Manufactured"
                                                                                         ,"DHW.Fuel"
                                                                                         ,"DHW.Location"
                                                                                         ,"count"
                                                                                         ,"EquipVintage_bins"))])
item105.data$m_ilk <- 1

#######################
# Weighted Analysis
#######################
item105.final <- proportions_one_group(CustomerLevelData  = item105.data
                                       , valueVariable    = 'm_ilk'
                                       , groupingVariable = 'EquipVintage_bins'
                                       , total.name =      "Total")

unique(item105.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item105.table <- item105.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item105.table <- data.frame(item105.table)


# SF = Table 112, MH = Table 87
# Export table
item105.final.SF <- item105.table[which(item105.table$BuildingType == "Single Family")
                                  ,-which(colnames(item105.table) %in% c("BuildingType"))]
item105.final.MH <- item105.table[which(item105.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item105.table) %in% c("BuildingType"))]

# exportTable(item105.final.SF, "SF", "Table 112", weighted = TRUE)
exportTable(item105.final.MH, "MH", "Table 87", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item105.final <- proportions_one_group(CustomerLevelData  = item105.data
                                       , valueVariable    = 'count'
                                       , groupingVariable = 'EquipVintage_bins'
                                       , total.name       = "Total"
                                       , weighted         = FALSE)
unique(item105.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item105.table <- item105.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item105.table <- data.frame(item105.table)


# SF = Table 112, MH = Table 87
# Export table
item105.final.SF <- item105.table[which(item105.table$BuildingType == "Single Family")
                                  ,-which(colnames(item105.table) %in% c("BuildingType"))]
item105.final.MH <- item105.table[which(item105.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item105.table) %in% c("BuildingType"))]

# exportTable(item105.final.SF, "SF", "Table 112", weighted = FALSE)
exportTable(item105.final.MH, "MH", "Table 87", weighted = FALSE)

