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
download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx', mechanical.export, mode = 'wb')
mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))
#subset to columns needed for analysis
mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"Heating.Fuel"
                                                                        ,"Heating.Efficiency.-.High"
                                                                        ,"Component.1.Year.of.Manufacture"
                                                                        ,"HSPF"
                                                                        ,"Primary.Heating.System"))]
#fix capitalization error
mechanical.dat1$Heating.Fuel[which(mechanical.dat1$Heating.Fuel == "Natural gas")] <- "Natural Gas"
mechanical.dat1 <- mechanical.dat1[which(mechanical.dat1$Primary.Heating.System == "Yes"),]
#remove any irrelevant equipment vintages (datapoint not asked for)
mechanical.dat2 <- mechanical.dat1#[which(mechanical.dat1$Component.1.Year.of.Manufacture != "Datapoint not asked for"),]
#remove any NA equipment vintages
mechanical.dat2$Component.1.Year.of.Manufacture <- as.numeric(as.character(mechanical.dat2$Component.1.Year.of.Manufacture))
# mechanical.dat2[which(is.na(mechanical.dat2$Component.1.Year.of.Manufacture))] <- "Vintage Unknown"
mechanical.dat3 <- mechanical.dat2

# Bin equipment vintages for items 50 and 52 (4 categories)
mechanical.dat3$EquipVintage_bins <- "Vintage Unknown"
mechanical.dat3$EquipVintage_bins_MH <- "Vintage Unknown"

mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` < 1990)] <- "Pre 1990"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 1990 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2000)] <- "1990-1999"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2000 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2007)] <- "2000-2006"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2007 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2015)] <- "2007-2014"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2015)] <- "Post 2014"

mechanical.dat3$EquipVintage_bins_MH[which(mechanical.dat3$`Component.1.Year.of.Manufacture` < 1990)] <- "Pre 1990"
mechanical.dat3$EquipVintage_bins_MH[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 1990 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2000)] <- "1990-1999"
mechanical.dat3$EquipVintage_bins_MH[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2000 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2006)] <- "2000-2005"
mechanical.dat3$EquipVintage_bins_MH[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2006 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2015)] <- "2006-2014"
mechanical.dat3$EquipVintage_bins_MH[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2015)] <- "Post 2014"

#check uniques
unique(mechanical.dat3$EquipVintage_bins)
unique(mechanical.dat3$EquipVintage_bins_MH)




#############################################################################################
#Item 50: AVERAGE GAS FURNACE EFFICIENCY (AFUE) BY EQUIPMENT VINTAGE AND STATE (SF table 57, MH table 38)
#############################################################################################
#data for item 50
item50.dat <- mechanical.dat3[which(mechanical.dat3$CK_Cadmus_ID != "BUILDING"),]

#remove any irrelevant heating efficiency datapoints (datapoint not asked for)
item50.dat1 <- item50.dat#[which(item50.dat$`Heating.Efficiency.-.High` != "Datapoint not asked for"),]

#remove any repeated header lines
item50.dat2 <- item50.dat1[which(item50.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Fix any issues with heating efficiency bins (? = unknown)
item50.dat2$`Heating.Efficiency.-.High`[which(item50.dat2$`Heating.Efficiency.-.High` == "?")] <- "Unknown"
#check that we are left with only quantities we want for heating efficiency
unique(item50.dat2$`Heating.Efficiency.-.High`)

#remove unknown heating efficiencies for now -- Rietz might want to note how many are unknown in future
item50.dat3 <- item50.dat2[which(item50.dat2$`Heating.Efficiency.-.High` != "Unknown"),]

#make heating efficiency information numeric
item50.dat3$`Heating.Efficiency.-.High` <- as.numeric(as.character(item50.dat3$`Heating.Efficiency.-.High`))
unique(item50.dat3$`Heating.Efficiency.-.High`)
item50.dat3$count <- 1

item50.dat4 <- item50.dat3[which(item50.dat3$`Heating.Efficiency.-.High` %notin% c("N/A",NA)),]
#average within houses
item50.customer <- summarise(group_by(item50.dat4
                                     , CK_Cadmus_ID
                                     , EquipVintage_bins
                                     , EquipVintage_bins_MH)
                             ,`Heating.Efficiency.-.High` = sum(`Heating.Efficiency.-.High`)
                            ,y_bar_ilk  = mean(`Heating.Efficiency.-.High`)
                            ,y_ilk      = sum(`Heating.Efficiency.-.High`)
                            ,m_ilk      = sum(count)
)

item50.merge <- left_join(rbsa.dat, item50.customer)
item50.merge <- item50.merge[which(!is.na(item50.merge$y_ilk)),]


item50.data <- weightedData(item50.merge[-which(colnames(item50.merge) %in% c("EquipVintage_bins"
                                                                              ,"EquipVintage_bins_MH"
                                                                              ,"y_bar_ilk"
                                                                              ,"y_ilk"
                                                                              ,"m_ilk"
                                                                              ,"Heating.Efficiency.-.High"))])

item50.data <- left_join(item50.data, item50.merge[which(colnames(item50.merge) %in% c("CK_Cadmus_ID"
                                                                                     ,"EquipVintage_bins"
                                                                                     ,"EquipVintage_bins_MH"
                                                                                     ,"y_bar_ilk"
                                                                                     ,"y_ilk"
                                                                                     ,"m_ilk"
                                                                                     ,"Heating.Efficiency.-.High"))])

###########################
# Weighted Analysis - Single Family
###########################
item50.final <- mean_two_groups_domain(CustomerLevelData = item50.data
                                       ,valueVariable    = 'y_ilk'
                                       ,byVariableRow    = 'EquipVintage_bins'
                                       ,byVariableColumn = 'State'
                                       ,aggregateColumn  = "Region"
                                       ,aggregateRow     = "All Vintages"
)


#subset to only the columns needed for the final RBSA table
item50.table <- data.frame("BuildingType"       = item50.final$BuildingType
                           ,"Equipment.Vintage" = item50.final$EquipVintage_bins
                           ,"Mean_ID"           = item50.final$Mean_ID
                           ,"SE_ID"             = item50.final$SE_ID
                           ,"n_ID"              = item50.final$n_ID
                           ,"Mean_MT"           = item50.final$Mean_MT
                           ,"SE_MT"             = item50.final$SE_MT
                           ,"n_MT"              = item50.final$n_MT
                           ,"Mean_OR"           = item50.final$Mean_OR
                           ,"SE_OR"             = item50.final$SE_OR
                           ,"n_OR"              = item50.final$n_OR
                           ,"Mean_WA"           = item50.final$Mean_WA
                           ,"SE_WA"             = item50.final$SE_WA
                           ,"n_WA"              = item50.final$n_WA
                           ,"Mean_Region"       = item50.final$Mean_Region
                           ,"SE_Region"         = item50.final$SE_Region
                           ,"n_Region"          = item50.final$n_Region
                           ,"EB_ID"             = item50.final$EB_ID
                           ,"EB_MT"             = item50.final$EB_MT
                           ,"EB_OR"             = item50.final$EB_OR
                           ,"EB_WA"             = item50.final$EB_WA
                           ,"EB_Region"         = item50.final$EB_Region)

# row ordering example code
levels(item50.table$Equipment.Vintage)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2006"
              ,"2007-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item50.table <- item50.table %>% mutate(Equipment.Vintage = factor(Equipment.Vintage, levels = rowOrder)) %>% arrange(Equipment.Vintage)  
item50.table <- data.frame(item50.table)

#subset to only the relevant building types for this item
item50.table.SF <- item50.table[which(item50.table$BuildingType == "Single Family"),-which(colnames(item50.table) %in% c("BuildingType"))]

# exportTable(item50.table.SF, "SF", "Table 57", weighted = TRUE)

###########################
# Weighted Analysis - Manufactured
###########################
item50.final <- mean_two_groups_domain(CustomerLevelData = item50.data
                                       ,valueVariable    = 'y_ilk'
                                       ,byVariableRow    = 'EquipVintage_bins_MH'
                                       ,byVariableColumn = 'State'
                                       ,aggregateColumn  = "Region"
                                       ,aggregateRow     = "All Vintages"
)


#subset to only the columns needed for the final RBSA table
item50.table <- data.frame("BuildingType"       = item50.final$BuildingType
                           ,"Equipment.Vintage" = item50.final$EquipVintage_bins_MH
                           ,"Mean_ID"           = item50.final$Mean_ID
                           ,"SE_ID"             = item50.final$SE_ID
                           ,"n_ID"              = item50.final$n_ID
                           ,"Mean_MT"           = item50.final$Mean_MT
                           ,"SE_MT"             = item50.final$SE_MT
                           ,"n_MT"              = item50.final$n_MT
                           ,"Mean_OR"           = item50.final$Mean_OR
                           ,"SE_OR"             = item50.final$SE_OR
                           ,"n_OR"              = item50.final$n_OR
                           ,"Mean_WA"           = item50.final$Mean_WA
                           ,"SE_WA"             = item50.final$SE_WA
                           ,"n_WA"              = item50.final$n_WA
                           ,"Mean_Region"       = item50.final$Mean_Region
                           ,"SE_Region"         = item50.final$SE_Region
                           ,"n_Region"          = item50.final$n_Region
                           ,"EB_ID"             = item50.final$EB_ID
                           ,"EB_MT"             = item50.final$EB_MT
                           ,"EB_OR"             = item50.final$EB_OR
                           ,"EB_WA"             = item50.final$EB_WA
                           ,"EB_Region"         = item50.final$EB_Region)

# row ordering example code
levels(item50.table$Equipment.Vintage)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2005"
              ,"2006-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item50.table <- item50.table %>% mutate(Equipment.Vintage = factor(Equipment.Vintage, levels = rowOrder)) %>% arrange(Equipment.Vintage)  
item50.table <- data.frame(item50.table)

item50.table.MH <- item50.table[which(item50.table$BuildingType == "Manufactured") , -which(colnames(item50.table) %in% c("BuildingType"))]

exportTable(item50.table.MH, "MH", "Table 38", weighted = TRUE)



###########################
# Unweighted Analysis - single family
###########################
item50.final <- mean_two_groups_unweighted(CustomerLevelData = item50.data
                                ,valueVariable    = 'y_ilk'
                                ,byVariableRow    = 'EquipVintage_bins'
                                ,byVariableColumn = 'State'
                                ,columnAggregate  = "Region"
                                ,rowAggregate     = "All Vintages"
)


#subset to only the columns needed for the final RBSA table
item50.table <- data.frame("BuildingType"       = item50.final$BuildingType
                           ,"Equipment.Vintage" = item50.final$EquipVintage_bins
                           ,"Mean_ID"           = item50.final$Mean_ID
                           ,"SE_ID"             = item50.final$SE_ID
                           ,"n_ID"              = item50.final$n_ID
                           ,"Mean_MT"           = item50.final$Mean_MT
                           ,"SE_MT"             = item50.final$SE_MT
                           ,"n_MT"              = item50.final$n_MT
                           ,"Mean_OR"           = item50.final$Mean_OR
                           ,"SE_OR"             = item50.final$SE_OR
                           ,"n_OR"              = item50.final$n_OR
                           ,"Mean_WA"           = item50.final$Mean_WA
                           ,"SE_WA"             = item50.final$SE_WA
                           ,"n_WA"              = item50.final$n_WA
                           ,"Mean_Region"       = item50.final$Mean_Region
                           ,"SE_Region"         = item50.final$SE_Region
                           ,"n_Region"          = item50.final$n_Region)

# row ordering example code
levels(item50.table$Equipment.Vintage)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2006"
              ,"2007-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item50.table <- item50.table %>% mutate(Equipment.Vintage = factor(Equipment.Vintage, levels = rowOrder)) %>% arrange(Equipment.Vintage)  
item50.table <- data.frame(item50.table)


#subset to only the relevant building types for this item
item50.table.SF <- item50.table[which(item50.table$BuildingType == "Single Family"),-which(colnames(item50.table) %in% c("BuildingType"))]

# exportTable(item50.table.SF, "SF", "Table 57", weighted = FALSE)

###########################
# unWeighted Analysis - Manufactured
###########################
item50.final <- mean_two_groups_unweighted(CustomerLevelData = item50.data
                                       ,valueVariable    = 'y_ilk'
                                       ,byVariableRow    = 'EquipVintage_bins_MH'
                                       ,byVariableColumn = 'State'
                                       ,columnAggregate  = "Region"
                                       ,rowAggregate     = "All Vintages"
)


#subset to only the columns needed for the final RBSA table
item50.table <- data.frame("BuildingType"       = item50.final$BuildingType
                           ,"Equipment.Vintage" = item50.final$EquipVintage_bins_MH
                           ,"Mean_ID"           = item50.final$Mean_ID
                           ,"SE_ID"             = item50.final$SE_ID
                           ,"n_ID"              = item50.final$n_ID
                           ,"Mean_MT"           = item50.final$Mean_MT
                           ,"SE_MT"             = item50.final$SE_MT
                           ,"n_MT"              = item50.final$n_MT
                           ,"Mean_OR"           = item50.final$Mean_OR
                           ,"SE_OR"             = item50.final$SE_OR
                           ,"n_OR"              = item50.final$n_OR
                           ,"Mean_WA"           = item50.final$Mean_WA
                           ,"SE_WA"             = item50.final$SE_WA
                           ,"n_WA"              = item50.final$n_WA
                           ,"Mean_Region"       = item50.final$Mean_Region
                           ,"SE_Region"         = item50.final$SE_Region
                           ,"n_Region"          = item50.final$n_Region)

# row ordering example code
levels(item50.table$Equipment.Vintage)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2005"
              ,"2006-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item50.table <- item50.table %>% mutate(Equipment.Vintage = factor(Equipment.Vintage, levels = rowOrder)) %>% arrange(Equipment.Vintage)  
item50.table <- data.frame(item50.table)

item50.table.MH <- item50.table[which(item50.table$BuildingType == "Manufactured") , -which(colnames(item50.table) %in% c("BuildingType"))]

exportTable(item50.table.MH, "MH", "Table 38", weighted = FALSE)







#############################################################################################
#Item 51: DISTRIBUTION OF GAS FURNACE EFFICIENCY (AFUE) BY STATE (SF table 58)
#############################################################################################
#data for item 51
item51.data <- item50.data

# Create heating efficiency bins
item51.data$Efficiency_bins <- as.numeric(as.character(item51.data$`Heating.Efficiency.-.High`))
item51.data$Efficiency_bins[which(item51.data$`Heating.Efficiency.-.High` < .80)] <- "< 80%"
item51.data$Efficiency_bins[which(item51.data$`Heating.Efficiency.-.High` >= .80 & item51.data$`Heating.Efficiency.-.High` < .90)] <- "80-89%"
item51.data$Efficiency_bins[which(item51.data$`Heating.Efficiency.-.High` >= .90 & item51.data$`Heating.Efficiency.-.High` < .95)] <- "90-94%"
item51.data$Efficiency_bins[which(item51.data$`Heating.Efficiency.-.High` >= .94)] <- "> 94%"
#check that efficiency bins are what we expect/want
unique(item51.data$Efficiency_bins)
#add count
item51.data$count <- 1



##########################
# Weighted Analysis
##########################
item51.final <- proportionRowsAndColumns1(CustomerLevelData = item51.data
                                          ,valueVariable = 'count'
                                          ,columnVariable = 'State'
                                          ,rowVariable    = 'Efficiency_bins'
                                          ,aggregateColumnName = "Region"
                                          )




#cast data into correct table format
item51.cast <- dcast(setDT(item51.final)
                      , formula = BuildingType + Efficiency_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

#subset to only the columns needed for the final RBSA table
item51.table <- data.frame("BuildingType"    = item51.cast$BuildingType
                           ,"Efficiency"     = item51.cast$Efficiency_bins
                           ,"Percent_ID"     = item51.cast$w.percent_ID
                           ,"SE_ID"          = item51.cast$w.SE_ID
                           ,"n_ID"           = item51.cast$n_ID
                           ,"Percent_MT"     = item51.cast$w.percent_MT
                           ,"SE_MT"          = item51.cast$w.SE_MT
                           ,"n_MT"           = item51.cast$n_MT
                           ,"Percent_OR"     = item51.cast$w.percent_OR
                           ,"SE_OR"          = item51.cast$w.SE_OR
                           ,"n_OR"           = item51.cast$n_OR
                           ,"Percent_WA"     = item51.cast$w.percent_WA
                           ,"SE_WA"          = item51.cast$w.SE_WA
                           ,"n_WA"           = item51.cast$n_WA
                           ,"Percent_Region" = item51.cast$w.percent_Region
                           ,"SE_Region"      = item51.cast$w.SE_Region
                           ,"n_Region"       = item51.cast$n_Region
                           ,"EB_ID"          = item51.cast$EB_ID
                           ,"EB_MT"          = item51.cast$EB_MT
                           ,"EB_OR"          = item51.cast$EB_OR
                           ,"EB_WA"          = item51.cast$EB_WA
                           ,"EB_Region"      = item51.cast$EB_Region)



# row ordering example code
levels(item51.table$Efficiency)
rowOrder <- c("< 80%"
              ,"80-89%"
              ,"90-94%"
              ,"> 94%"
              ,"Total")
item51.table <- item51.table %>% mutate(Efficiency = factor(Efficiency, levels = rowOrder)) %>% arrange(Efficiency)  
item51.table <- data.frame(item51.table)

#subset to only the relevant building types for this item
item51.table.SF <- item51.table[which(item51.table$BuildingType %in% c("Single Family")),-1]

# exportTable(item51.table.SF, "SF", "Table 58", weighted = TRUE)

##########################
# Unweighted Analysis
##########################
item51.final <- proportions_two_groups_unweighted(CustomerLevelData = item51.data
                                          ,valueVariable = 'count'
                                          ,columnVariable = 'State'
                                          ,rowVariable    = 'Efficiency_bins'
                                          ,aggregateColumnName = "Region"
)

#cast data into correct table format
item51.cast <- dcast(setDT(item51.final)
                     , formula = BuildingType + Efficiency_bins ~ State
                     , value.var = c("Percent", "SE", "Count", "n"))

#subset to only the columns needed for the final RBSA table
item51.table <- data.frame("BuildingType"    = item51.cast$BuildingType
                           ,"Efficiency"     = item51.cast$Efficiency_bins
                           ,"Percent_ID"     = item51.cast$Percent_ID
                           ,"SE_ID"          = item51.cast$SE_ID
                           ,"n_ID"           = item51.cast$n_ID
                           ,"Percent_MT"     = item51.cast$Percent_MT
                           ,"SE_MT"          = item51.cast$SE_MT
                           ,"n_MT"           = item51.cast$n_MT
                           ,"Percent_OR"     = item51.cast$Percent_OR
                           ,"SE_OR"          = item51.cast$SE_OR
                           ,"n_OR"           = item51.cast$n_OR
                           ,"Percent_WA"     = item51.cast$Percent_WA
                           ,"SE_WA"          = item51.cast$SE_WA
                           ,"n_WA"           = item51.cast$n_WA
                           ,"Percent_Region" = item51.cast$Percent_Region
                           ,"SE_Region"      = item51.cast$SE_Region
                           ,"n_Region"       = item51.cast$n_Region
                           )

# row ordering example code
levels(item51.table$Efficiency)
rowOrder <- c("< 80%"
              ,"80-89%"
              ,"90-94%"
              ,"> 94%"
              ,"Total")
item51.table <- item51.table %>% mutate(Efficiency = factor(Efficiency, levels = rowOrder)) %>% arrange(Efficiency)  
item51.table <- data.frame(item51.table)

#subset to only the relevant building types for this item
item51.table.SF <- item51.table[which(item51.table$BuildingType %in% c("Single Family")),-1]

# exportTable(item51.table.SF, "SF", "Table 58", weighted = FALSE)






#############################################################################################
#Item 52: AVERAGE AIR SOURCE HEAT PUMP EFFICIENCY (HSPF) BY EQUIPMENT VINTAGE (SF table 59, MH table 39)
#############################################################################################
#data for item 51
item52.dat <- mechanical.dat3[which(mechanical.dat3$CK_Cadmus_ID != "BUILDING"),]

#remove any irrelevant HSPF datapoints (could not collect)
item52.dat1 <- item52.dat#[which(item52.dat$HSPF != "Could Not Collect"),]

#remove any repeated header lines
item52.dat2 <- item52.dat1[which(item52.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#make HSPF information numeric
item52.dat2$HSPF <- as.numeric(as.character(item52.dat2$HSPF))

#remove any NAs in HSPF
item52.dat3 <- item52.dat2[which(item52.dat2$HSPF %notin% c("N/A",NA)),]
item52.dat3$count <- 1
#average within houses
item52.customer <- summarise(group_by(item52.dat3
                                      , CK_Cadmus_ID
                                      , EquipVintage_bins
                                      , EquipVintage_bins_MH)
                             ,y_bar_ilk  = mean(HSPF)
                             ,y_ilk      = sum(HSPF)
                             ,m_ilk      = sum(count)
)



#Join cleaned item 52 mechanical information with cleaned RBSA site information
item52.dat4 <- unique(left_join(rbsa.dat, item52.customer, by = "CK_Cadmus_ID"))
item52.dat4 <- item52.dat4[-grep("bldg", item52.dat4$CK_Building_ID, ignore.case = T),]
item52.dat5 <- item52.dat4[which(!is.na(item52.dat4$y_bar_ilk)),]
item52.dat5 <- item52.dat5[which(!is.na(item52.dat5$EquipVintage_bins)),]
unique(item52.dat5$EquipVintage_bins)

#any duplicates?
item52.dat5$CK_Cadmus_ID[which(duplicated(item52.dat5$CK_Cadmus_ID))]

# Weighting
item52.data <- weightedData(item52.dat5[-which(colnames(item52.dat5) %in% c("Generic"
                                                                            ,"Heating.Fuel"
                                                                            ,"Component.1.Year.of.Manufacture"
                                                                            ,"Heating.Efficiency.-.High"
                                                                            ,"HSPF"
                                                                            ,"EquipVintage_bins"
                                                                            ,"EquipVintage_bins_MH"
                                                                            ,"Primary.Heating.System"
                                                                            ,"y_bar_ilk"
                                                                            ,"y_ilk"
                                                                            ,"m_ilk"))])

item52.data <- left_join(item52.data, item52.dat5[which(colnames(item52.dat5) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"Heating.Fuel"
                                                                                     ,"Component.1.Year.of.Manufacture"
                                                                                     ,"HSPF"
                                                                                     ,"EquipVintage_bins"
                                                                                     ,"EquipVintage_bins_MH"
                                                                                     ,"Primary.Heating.System"
                                                                                     ,"y_bar_ilk"
                                                                                     ,"y_ilk"
                                                                                     ,"m_ilk"))])
item52.data$count <- 1
length(unique(item52.data$CK_Cadmus_ID[which(item52.data$BuildingType == "Manufactured")]))
###############################
# Weighted Analysis - single family
###############################
item52.final <- mean_one_group_domain(CustomerLevelData = item52.data
                               ,valueVariable = 'y_bar_ilk' 
                               ,byVariable    = 'EquipVintage_bins'
                               ,aggregateRow  = "All Vintages")
# row ordering example code
unique(item52.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2006"
              ,"2007-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item52.final <- item52.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item52.final <- data.frame(item52.final)

# Export table
item52.final.SF <- item52.final[which(item52.final$BuildingType == "Single Family"),-1]

# exportTable(item52.final.SF, "SF", "Table 59", weighted = TRUE)

###############################
# weighted Analysis - manufactured
###############################
item52.final <- mean_one_group_domain(CustomerLevelData = item52.data
                                      ,valueVariable = 'y_bar_ilk' 
                                      ,byVariable    = 'EquipVintage_bins_MH'
                                      ,aggregateRow  = "All Vintages")
# row ordering example code
unique(item52.final$EquipVintage_bins_MH)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2005"
              ,"2006-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item52.final <- item52.final %>% mutate(EquipVintage_bins_MH = factor(EquipVintage_bins_MH, levels = rowOrder)) %>% arrange(EquipVintage_bins_MH)  
item52.final <- data.frame(item52.final)

# Export table
item52.final.MH <- item52.final[which(item52.final$BuildingType == "Manufactured"),-1]

exportTable(item52.final.MH, "MH", "Table 39", weighted = TRUE)



###############################
# Unweighted Analysis
###############################
item52.data$count <- 1
item52.final <- mean_one_group_unweighted(CustomerLevelData = item52.data
                               ,valueVariable = 'y_bar_ilk' 
                               ,byVariable    = 'EquipVintage_bins'
                               ,aggregateRow  = "All Vintages")
# row ordering example code
unique(item52.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2006"
              ,"2007-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item52.final <- item52.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item52.final <- data.frame(item52.final)

# Export table
# SF = Table 59, MH = Table 39
item52.final.SF <- item52.final[which(item52.final$BuildingType == "Single Family"),-1]

# exportTable(item52.final.SF, "SF", "Table 59", weighted = FALSE)

###############################
# unweighted Analysis - manufactured
###############################
item52.final <- mean_one_group_unweighted(CustomerLevelData = item52.data
                                      ,valueVariable = 'y_bar_ilk' 
                                      ,byVariable    = 'EquipVintage_bins_MH'
                                      ,aggregateRow  = "All Vintages")
# row ordering example code
unique(item52.final$EquipVintage_bins_MH)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2005"
              ,"2006-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item52.final <- item52.final %>% mutate(EquipVintage_bins_MH = factor(EquipVintage_bins_MH, levels = rowOrder)) %>% arrange(EquipVintage_bins_MH)  
item52.final <- data.frame(item52.final)

# Export table
item52.final.MH <- item52.final[which(item52.final$BuildingType == "Manufactured"),-1]

exportTable(item52.final.MH, "MH", "Table 39", weighted = FALSE)





#############################################################################################
#Item 53: DISTRIBUTION OF AIR SOURCE HEAT PUMP EFFICIENCY (HSPF) BY STATE (SF table 60)
#############################################################################################
#data for item 53
item53.dat <- mechanical.dat3[which(mechanical.dat3$CK_Cadmus_ID != "BUILDING"),]

#remove any irrelevant HSPF datapoints (could not collect)
item53.dat1 <- item53.dat#[which(item53.dat$HSPF != "Could Not Collect"),]

#remove any repeated header lines
item53.dat2 <- item53.dat1[which(item53.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#make HSPF information numeric
item53.dat2$HSPF <- as.numeric(as.character(item53.dat2$HSPF))

#Join cleaned item 53 mechanical information with cleaned RBSA site information
item53.dat3 <- left_join(rbsa.dat, item53.dat2, by = "CK_Cadmus_ID")
item53.dat4 <- item53.dat3[which(!is.na(item53.dat3$HSPF)),]

# Create HSPF_bins according to previous RBSA bins
item53.dat4$HSPF_bins <- as.numeric(as.character(item53.dat4$HSPF))
item53.dat4$HSPF_bins[which(item53.dat4$HSPF >= 6.8 & item53.dat4$HSPF < 7.7)] <- "6.8-7.6"
item53.dat4$HSPF_bins[which(item53.dat4$HSPF >= 7.7 & item53.dat4$HSPF < 8.3)] <- "7.7-8.2"
item53.dat4$HSPF_bins[which(item53.dat4$HSPF >= 8.3 & item53.dat4$HSPF < 9.0)] <- "8.3-8.9"
item53.dat4$HSPF_bins[which(item53.dat4$HSPF >= 9.0)] <- "9.0+"
#make sure that the HSPF bins are what we want/expect
unique(item53.dat4$HSPF_bins)
#add count
item53.dat4$count <- 1


item53.data <- weightedData(item53.dat4[-which(colnames(item53.dat4) %in% c("Generic"                        
                                                                             ,"Heating.Fuel"
                                                                             ,"Component.1.Year.of.Manufacture"
                                                                             ,"Heating.Efficiency.-.High"      
                                                                             ,"HSPF"
                                                                             ,"EquipVintage_bins"
                                                                             ,"EquipVintage_bins_MH"
                                                                             ,"HSPF_bins"                      
                                                                             ,"count"
                                                                            ,"Primary.Heating.System" ))])
item53.data <- left_join(item53.data, item53.dat4[which(colnames(item53.dat4) %in% c("CK_Cadmus_ID"
                                                                                      ,"Generic"                        
                                                                                      ,"Heating.Fuel"
                                                                                      ,"Component.1.Year.of.Manufacture"
                                                                                      ,"Heating.Efficiency.-.High"      
                                                                                      ,"HSPF"
                                                                                      ,"EquipVintage_bins"
                                                                                      ,"EquipVintage_bins_MH"
                                                                                      ,"HSPF_bins"                      
                                                                                      ,"count"
                                                                                     ,"Primary.Heating.System"))])
item53.data$count <- 1
########################
# Weighted Analysis
########################
item53.final <- proportionRowsAndColumns1(CustomerLevelData    = item53.data
                                          ,valueVariable       = 'count'
                                          ,columnVariable      = 'State'
                                          ,rowVariable         = 'HSPF_bins'
                                          ,aggregateColumnName = "Region")


#cast data into correct table format
item53.cast <- dcast(setDT(item53.final)
                     , formula   = BuildingType + HSPF_bins ~ State
                     , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

#subset to only the columns needed for the final RBSA table
item53.table <- data.frame("BuildingType"    = item53.cast$BuildingType
                           ,"HSPF"           = item53.cast$HSPF_bins
                           ,"Percent_ID"     = item53.cast$w.percent_ID
                           ,"SE_ID"          = item53.cast$w.SE_ID
                           ,"n_ID"           = item53.cast$n_ID
                           ,"Percent_MT"     = item53.cast$w.percent_MT
                           ,"SE_MT"          = item53.cast$w.SE_MT
                           ,"n_MT"           = item53.cast$n_MT
                           ,"Percent_OR"     = item53.cast$w.percent_OR
                           ,"SE_OR"          = item53.cast$w.SE_OR
                           ,"n_OR"           = item53.cast$n_OR
                           ,"Percent_WA"     = item53.cast$w.percent_WA
                           ,"SE_WA"          = item53.cast$w.SE_WA
                           ,"n_WA"           = item53.cast$n_WA
                           ,"Percent_Region" = item53.cast$w.percent_Region
                           ,"SE_Region"      = item53.cast$w.SE_Region
                           ,"n_Region"       = item53.cast$n_Region
                           ,"EB_ID"          = item53.cast$EB_ID
                           ,"EB_MT"          = item53.cast$EB_MT
                           ,"EB_OR"          = item53.cast$EB_OR
                           ,"EB_WA"          = item53.cast$EB_WA
                           ,"EB_Region"      = item53.cast$EB_Region)

#subset to only the relevant building types for this item
item53.table.SF <- item53.table[which(item53.table$BuildingType %in% c("Single Family")),-1]
item53.table.MH <- item53.table[which(item53.table$BuildingType %in% c("Manufactured")),-1]

# exportTable(item53.table.SF, "SF", "Table 60", weighted = TRUE)
exportTable(item53.table.MH, "MH", "Table 40", weighted = TRUE)


########################
# Unweighted Analysis
########################
item53.final <- proportions_two_groups_unweighted(CustomerLevelData    = item53.data
                                          ,valueVariable       = 'count'
                                          ,columnVariable      = 'State'
                                          ,rowVariable         = 'HSPF_bins'
                                          ,aggregateColumnName = "Region")


#cast data into correct table format
item53.cast <- dcast(setDT(item53.final)
                     , formula   = BuildingType + HSPF_bins ~ State
                     , value.var = c("Percent", "SE", "Count", "n"))

#subset to only the columns needed for the final RBSA table
item53.table <- data.frame("BuildingType"    = item53.cast$BuildingType
                           ,"HSPF"           = item53.cast$HSPF_bins
                           ,"Percent_ID"     = item53.cast$Percent_ID
                           ,"SE_ID"          = item53.cast$SE_ID
                           ,"n_ID"           = item53.cast$n_ID
                           ,"Percent_MT"     = item53.cast$Percent_MT
                           ,"SE_MT"          = item53.cast$SE_MT
                           ,"n_MT"           = item53.cast$n_MT
                           ,"Percent_OR"     = item53.cast$Percent_OR
                           ,"SE_OR"          = item53.cast$SE_OR
                           ,"n_OR"           = item53.cast$n_OR
                           ,"Percent_WA"     = item53.cast$Percent_WA
                           ,"SE_WA"          = item53.cast$SE_WA
                           ,"n_WA"           = item53.cast$n_WA
                           ,"Percent_Region" = item53.cast$Percent_Region
                           ,"SE_Region"      = item53.cast$SE_Region
                           ,"n_Region"       = item53.cast$n_Region)

#subset to only the relevant building types for this item
item53.table.SF <- item53.table[which(item53.table$BuildingType %in% c("Single Family")),-1]
item53.table.MH <- item53.table[which(item53.table$BuildingType %in% c("Manufactured")),-1]

# exportTable(item53.table.SF, "SF", "Table 60", weighted = FALSE)
exportTable(item53.table.MH, "MH", "Table 40", weighted = FALSE)





















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

#############################################################################################
#Item 50: AVERAGE GAS FURNACE EFFICIENCY (AFUE) BY EQUIPMENT VINTAGE AND CK_Building_ID (SF table 57, MH table 38)
#############################################################################################
#data for item 50
item50.os.dat <- mechanical.dat3[which(mechanical.dat3$CK_Cadmus_ID != "BUILDING"),]

#remove any irrelevant heating efficiency datapoints (datapoint not asked for)
item50.os.dat1 <- item50.os.dat[which(item50.os.dat$`Heating.Efficiency.-.High` != "Datapoint not asked for"),]

#remove any repeated header lines
item50.os.dat2 <- item50.os.dat1[which(item50.os.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Fix any issues with heating efficiency bins (? = unknown)
item50.os.dat2$`Heating.Efficiency.-.High`[which(item50.os.dat2$`Heating.Efficiency.-.High` == "?")] <- "Unknown"
#check that we are left with only quantities we want for heating efficiency
unique(item50.os.dat2$`Heating.Efficiency.-.High`)

#remove unknown heating efficiencies for now -- Rietz might want to note how many are unknown in future
item50.os.dat3 <- item50.os.dat2[which(item50.os.dat2$`Heating.Efficiency.-.High` != "Unknown"),]

#make heating efficiency information numeric
item50.os.dat3$`Heating.Efficiency.-.High` <- as.numeric(as.character(item50.os.dat3$`Heating.Efficiency.-.High`))
item50.os.dat3$`Heating.Efficiency.-.High` <- item50.os.dat3$`Heating.Efficiency.-.High`
unique(item50.os.dat3$`Heating.Efficiency.-.High`)
item50.os.dat3$count <- 1

item50.os.dat4 <- item50.os.dat3[which(item50.os.dat3$`Heating.Efficiency.-.High` %notin% c("N/A",NA)),]
#average within houses
item50.os.customer <- summarise(group_by(item50.os.dat4
                                      , CK_Cadmus_ID
                                      , EquipVintage_bins
                                      , EquipVintage_bins_MH)
                             ,`Heating.Efficiency.-.High` = sum(`Heating.Efficiency.-.High`)
                             ,y_bar_ilk  = mean(`Heating.Efficiency.-.High`)
                             ,y_ilk      = sum(`Heating.Efficiency.-.High`)
                             ,m_ilk      = sum(count)
)

item50.os.merge <- left_join(os.dat, item50.os.customer)
item50.os.merge <- item50.os.merge[which(!is.na(item50.os.merge$y_ilk)),]


item50.os.data <- weightedData(item50.os.merge[-which(colnames(item50.os.merge) %in% c("EquipVintage_bins"
                                                                              ,"EquipVintage_bins_MH"
                                                                              ,"y_bar_ilk"
                                                                              ,"y_ilk"
                                                                              ,"m_ilk"
                                                                              ,"Heating.Efficiency.-.High"))])

item50.os.data <- left_join(item50.os.data, item50.os.merge[which(colnames(item50.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"EquipVintage_bins"
                                                                                       ,"EquipVintage_bins_MH"
                                                                                       ,"y_bar_ilk"
                                                                                       ,"y_ilk"
                                                                                       ,"m_ilk"
                                                                                       ,"Heating.Efficiency.-.High"))])

###########################
# Weighted Analysis - Single Family
###########################
item50.os.cast <- mean_two_groups_domain(CustomerLevelData = item50.os.data
                                       ,valueVariable    = 'y_ilk'
                                       ,byVariableRow    = 'EquipVintage_bins'
                                       ,byVariableColumn = 'CK_Building_ID'
                                       ,aggregateColumn  = "Remove"
                                       ,aggregateRow     = "All Vintages")

names(item50.os.cast)
if(os.ind == "scl"){
  item50.os.table <- data.frame("BuildingType"       = item50.os.cast$BuildingType
                                ,"Equipment.Vintage"    = item50.os.cast$EquipVintage_bins
                                ,"Mean_SCL.GenPop"      = item50.os.cast$`Mean_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item50.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item50.os.cast$`n_SCL GenPop`
                                ,"Mean_SCL.LI"          = item50.os.cast$`Mean_SCL LI`
                                ,"SE_SCL.LI"            = item50.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item50.os.cast$`n_SCL LI`
                                ,"Mean_SCL.EH"          = NA#item50.os.cast$`Mean_SCL EH`
                                ,"SE_SCL.EH"            = NA#item50.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = NA#item50.os.cast$`n_SCL EH`
                                ,"Mean_2017.RBSA.PS"    = item50.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item50.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item50.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item50.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item50.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = NA#item50.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item50.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item50.os.table <- data.frame("BuildingType"       = item50.os.cast$BuildingType
                                ,"Equipment.Vintage"    = item50.os.cast$EquipVintage_bins
                                ,"Mean_SnoPUD"          = item50.os.cast$`Mean_SnoPUD`
                                ,"SE_SnoPUD"            = item50.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"             = item50.os.cast$`n_SnoPUD`
                                ,"Mean_2017.RBSA.PS"    = item50.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item50.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item50.os.cast$`n_2017 RBSA PS`
                                ,"Mean_RBSA.NW"         = item50.os.cast$`Mean_2017 RBSA NW`
                                ,"SE_RBSA.NW"           = item50.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"            = item50.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"            = item50.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"      = item50.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"           = item50.os.cast$`EB_2017 RBSA NW`)
}

                           

# row ordering example code
levels(item50.os.table$Equipment.Vintage)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2006"
              ,"2007-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item50.os.table <- item50.os.table %>% mutate(Equipment.Vintage = factor(Equipment.Vintage, levels = rowOrder)) %>% arrange(Equipment.Vintage)  
item50.os.table <- data.frame(item50.os.table)

#subset to only the relevant building types for this item
item50.os.table.SF <- item50.os.table[which(item50.os.table$BuildingType == "Single Family"),-which(colnames(item50.os.table) %in% c("BuildingType"))]

exportTable(item50.os.table.SF, "SF", "Table 57", weighted = TRUE, osIndicator = export.ind, OS = T)


###########################
# Unweighted Analysis - single family
###########################
item50.os.final <- mean_two_groups_unweighted(CustomerLevelData = item50.os.data
                                           ,valueVariable    = 'y_ilk'
                                           ,byVariableRow    = 'EquipVintage_bins'
                                           ,byVariableColumn = 'CK_Building_ID'
                                           ,columnAggregate  = "Region"
                                           ,rowAggregate     = "All Vintages")

names(item50.os.final)
if(os.ind == "scl"){
  item50.os.table <- data.frame("BuildingType"       = item50.os.final$BuildingType
                                ,"Equipment.Vintage"    = item50.os.final$EquipVintage_bins
                                ,"Mean_SCL.GenPop"      = item50.os.cast$`Mean_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item50.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item50.os.cast$`n_SCL GenPop`
                                ,"Mean_SCL.LI"          = item50.os.cast$`Mean_SCL LI`
                                ,"SE_SCL.LI"            = item50.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item50.os.cast$`n_SCL LI`
                                ,"Mean_SCL.EH"          = NA#item50.os.cast$`Mean_SCL EH`
                                ,"SE_SCL.EH"            = NA#item50.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = NA#item50.os.cast$`n_SCL EH`
                                ,"Mean_2017.RBSA.PS"    = item50.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item50.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item50.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item50.os.table <- data.frame("BuildingType"       = item50.os.final$BuildingType
                                ,"Equipment.Vintage"    = item50.os.final$EquipVintage_bins
                                ,"Mean_SnoPUD"          = item50.os.cast$`Mean_SnoPUD`
                                ,"SE_SnoPUD"            = item50.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"             = item50.os.cast$`n_SnoPUD`
                                ,"Mean_2017.RBSA.PS"    = item50.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item50.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item50.os.cast$`n_2017 RBSA PS`
                                ,"Mean_RBSA.NW"         = item50.os.cast$`Mean_2017 RBSA NW`
                                ,"SE_RBSA.NW"           = item50.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"            = item50.os.cast$`n_2017 RBSA NW`)
}



# row ordering example code
levels(item50.os.table$Equipment.Vintage)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2006"
              ,"2007-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item50.os.table <- item50.os.table %>% mutate(Equipment.Vintage = factor(Equipment.Vintage, levels = rowOrder)) %>% arrange(Equipment.Vintage)  
item50.os.table <- data.frame(item50.os.table)


#subset to only the relevant building types for this item
item50.os.table.SF <- item50.os.table[which(item50.os.table$BuildingType == "Single Family"),-which(colnames(item50.os.table) %in% c("BuildingType"))]

exportTable(item50.os.table.SF, "SF", "Table 57", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
#Item 51: DISTRIBUTION OF GAS FURNACE EFFICIENCY (AFUE) BY CK_Building_ID (SF table 58)
#############################################################################################
#data for item 51
item51.os.data <- item50.os.data

# Create heating efficiency bins
item51.os.data$Efficiency_bins <- as.numeric(as.character(item51.os.data$`Heating.Efficiency.-.High`))
item51.os.data$Efficiency_bins[which(item51.os.data$`Heating.Efficiency.-.High` < .80)] <- "< 80%"
item51.os.data$Efficiency_bins[which(item51.os.data$`Heating.Efficiency.-.High` >= .80 & item51.os.data$`Heating.Efficiency.-.High` < .90)] <- "80-89%"
item51.os.data$Efficiency_bins[which(item51.os.data$`Heating.Efficiency.-.High` >= .90 & item51.os.data$`Heating.Efficiency.-.High` < .95)] <- "90-94%"
item51.os.data$Efficiency_bins[which(item51.os.data$`Heating.Efficiency.-.High` >= .94)] <- "> 94%"
#check that efficiency bins are what we expect/want
unique(item51.os.data$Efficiency_bins)
#add count
item51.os.data$count <- 1



##########################
# Weighted Analysis
##########################
item51.os.final <- proportionRowsAndColumns1(CustomerLevelData = item51.os.data
                                          ,valueVariable = 'count'
                                          ,columnVariable = 'CK_Building_ID'
                                          ,rowVariable    = 'Efficiency_bins'
                                          ,aggregateColumnName = "Region"
)

#cast data into correct table format
item51.os.cast <- dcast(setDT(item51.os.final)
                     , formula = BuildingType + Efficiency_bins ~ CK_Building_ID
                     , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

names(item51.os.cast)

if(os.ind == "scl"){
  item51.os.table <- data.frame("BuildingType"          = item51.os.cast$BuildingType
                                ,"Efficiency"           = item51.os.cast$Efficiency_bins
                                ,"Percent_SCL.GenPop"   = item51.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item51.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item51.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item51.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item51.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item51.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = NA#item51.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = NA#item51.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = NA#item51.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item51.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item51.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item51.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item51.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item51.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = NA#item51.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item51.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item51.os.table <- data.frame("BuildingType"          = item51.os.cast$BuildingType
                                ,"Efficiency"           = item51.os.cast$Efficiency_bins
                                ,"Percent_SnoPUD"          = item51.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item51.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item51.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item51.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item51.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item51.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item51.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item51.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item51.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item51.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item51.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item51.os.cast$`EB_2017 RBSA NW`)
}






# row ordering example code
levels(item51.os.table$Efficiency)
rowOrder <- c("< 80%"
              ,"80-89%"
              ,"90-94%"
              ,"> 94%"
              ,"Total")
item51.os.table <- item51.os.table %>% mutate(Efficiency = factor(Efficiency, levels = rowOrder)) %>% arrange(Efficiency)  
item51.os.table <- data.frame(item51.os.table)

#subset to only the relevant building types for this item
item51.os.table.SF <- item51.os.table[which(item51.os.table$BuildingType %in% c("Single Family")),-1]
exportTable(item51.os.table.SF, "SF", "Table 58", weighted = TRUE, osIndicator = export.ind, OS = T)

##########################
# Unweighted Analysis
##########################
item51.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item51.os.data
                                                  ,valueVariable = 'count'
                                                  ,columnVariable = 'CK_Building_ID'
                                                  ,rowVariable    = 'Efficiency_bins'
                                                  ,aggregateColumnName = "Region"
)

#cast data into correct table format
item51.os.cast <- dcast(setDT(item51.os.final)
                     , formula = BuildingType + Efficiency_bins ~ CK_Building_ID
                     , value.var = c("Percent", "SE", "Count", "n"))

names(item51.os.cast)
if(os.ind == "scl"){
  item51.os.table <- data.frame("BuildingType"          = item51.os.cast$BuildingType
                                ,"Efficiency"           = item51.os.cast$Efficiency_bins
                                ,"Percent_SCL.GenPop"   = item51.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item51.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item51.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item51.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item51.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item51.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = NA#item51.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = NA#item51.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = NA#item51.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item51.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item51.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item51.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item51.os.table <- data.frame("BuildingType"          = item51.os.cast$BuildingType
                                ,"Efficiency"           = item51.os.cast$Efficiency_bins
                                ,"Percent_SnoPUD"          = item51.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item51.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item51.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item51.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item51.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item51.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item51.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item51.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item51.os.cast$`n_2017 RBSA NW`)
}





# row ordering example code
levels(item51.os.table$Efficiency)
rowOrder <- c("< 80%"
              ,"80-89%"
              ,"90-94%"
              ,"> 94%"
              ,"Total")
item51.os.table <- item51.os.table %>% mutate(Efficiency = factor(Efficiency, levels = rowOrder)) %>% arrange(Efficiency)  
item51.os.table <- data.frame(item51.os.table)

#subset to only the relevant building types for this item
item51.os.table.SF <- item51.os.table[which(item51.os.table$BuildingType %in% c("Single Family")),-1]
exportTable(item51.os.table.SF, "SF", "Table 58", weighted = FALSE, osIndicator = export.ind, OS = T)






#############################################################################################
#Item 52: AVERAGE AIR SOURCE HEAT PUMP EFFICIENCY (HSPF) BY EQUIPMENT VINTAGE (SF table 59, MH table 39)
#############################################################################################
#data for item 51
item52.os.dat <- mechanical.dat3[which(mechanical.dat3$CK_Cadmus_ID != "BUILDING"),]

#remove any irrelevant HSPF datapoints (could not collect)
item52.os.dat1 <- item52.os.dat[which(item52.os.dat$HSPF != "Could Not Collect"),]

#remove any repeated header lines
item52.os.dat2 <- item52.os.dat1[which(item52.os.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#make HSPF information numeric
item52.os.dat2$HSPF <- as.numeric(as.character(item52.os.dat2$HSPF))

#remove any NAs in HSPF
item52.os.dat3 <- item52.os.dat2[which(item52.os.dat2$HSPF %notin% c("N/A",NA)),]
item52.os.dat3$count <- 1
#average within houses
item52.os.customer <- summarise(group_by(item52.os.dat3
                                      , CK_Cadmus_ID
                                      , EquipVintage_bins
                                      , EquipVintage_bins_MH)
                             ,y_bar_ilk  = mean(HSPF)
                             ,y_ilk      = sum(HSPF)
                             ,m_ilk      = sum(count)
)



#Join cleaned item 52 mechanical information with cleaned scl site information
item52.os.dat4 <- unique(left_join(os.dat, item52.os.customer, by = "CK_Cadmus_ID"))
item52.os.dat5 <- item52.os.dat4[which(!is.na(item52.os.dat4$y_bar_ilk)),]
item52.os.dat5 <- item52.os.dat4[which(!is.na(item52.os.dat4$EquipVintage_bins)),]
unique(item52.os.dat5$EquipVintage_bins)

# Weighting
item52.os.data <- weightedData(item52.os.dat5[-which(colnames(item52.os.dat5) %in% c("Generic"
                                                                            ,"Heating.Fuel"
                                                                            ,"Component.1.Year.of.Manufacture"
                                                                            ,"Heating.Efficiency.-.High"
                                                                            ,"HSPF"
                                                                            ,"EquipVintage_bins"
                                                                            ,"EquipVintage_bins_MH"
                                                                            ,"Primary.Heating.System"
                                                                            ,"y_bar_ilk"
                                                                            ,"y_ilk"
                                                                            ,"m_ilk"))])

item52.os.data <- left_join(item52.os.data, item52.os.dat5[which(colnames(item52.os.dat5) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"Heating.Fuel"
                                                                                     ,"Component.1.Year.of.Manufacture"
                                                                                     ,"HSPF"
                                                                                     ,"EquipVintage_bins"
                                                                                     ,"EquipVintage_bins_MH"
                                                                                     ,"Primary.Heating.System"
                                                                                     ,"y_bar_ilk"
                                                                                     ,"y_ilk"
                                                                                     ,"m_ilk"))])
item52.os.data$count <- 1
###############################
# Weighted Analysis - single family
###############################
item52.os.cast <- mean_two_groups(CustomerLevelData = item52.os.data
                                   ,valueVariable = 'y_bar_ilk'
                                   ,byVariableRow = "EquipVintage_bins"
                                   ,byVariableColumn = "CK_Building_ID"
                                   ,columnAggregate = "Remove"
                                   ,rowAggregate = "All Vintages")
names(item52.os.cast)
if(os.ind == "scl"){
  item52.os.final <- data.frame("BuildingType"          = item52.os.cast$BuildingType
                                ,"EquipVintage_bins"    = item52.os.cast$EquipVintage_bins
                                ,"Mean_SCL.GenPop"      = item52.os.cast$`Mean_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item52.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item52.os.cast$`n_SCL GenPop`
                                ,"Mean_SCL.LI"          = NA#item52.os.cast$`Mean_SCL LI`
                                ,"SE_SCL.LI"            = NA#item52.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = NA#item52.os.cast$`n_SCL LI`
                                ,"Mean_SCL.EH"          = item52.os.cast$`Mean_SCL EH`
                                ,"SE_SCL.EH"            = item52.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item52.os.cast$`n_SCL EH`
                                ,"Mean_2017.RBSA.PS"    = item52.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item52.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item52.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item52.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = NA#item52.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item52.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item52.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item52.os.final <- data.frame("BuildingType"          = item52.os.cast$BuildingType
                                ,"EquipVintage_bins"    = item52.os.cast$EquipVintage_bins
                                ,"Mean_SnoPUD"          = item52.os.cast$`Mean_SnoPUD`
                                ,"SE_SnoPUD"            = item52.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"             = item52.os.cast$`n_SnoPUD`
                                ,"Mean_2017.RBSA.PS"    = item52.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item52.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item52.os.cast$`n_2017 RBSA PS`
                                ,"Mean_RBSA.NW"         = item52.os.cast$`Mean_2017 RBSA NW`
                                ,"SE_RBSA.NW"           = item52.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"            = item52.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"            = item52.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"      = item52.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"           = item52.os.cast$`EB_2017 RBSA NW`)
}




# row ordering example code
unique(item52.os.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2006"
              ,"2007-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item52.os.final <- item52.os.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item52.os.final <- data.frame(item52.os.final)

# Export table
item52.os.final.SF <- item52.os.final[which(item52.os.final$BuildingType == "Single Family"),-1]

exportTable(item52.os.final.SF, "SF", "Table 59", weighted = TRUE, osIndicator = export.ind, OS = T)

###############################
# Unweighted Analysis
###############################
item52.os.cast <- mean_two_groups_unweighted(CustomerLevelData = item52.os.data
                                  ,valueVariable = 'y_bar_ilk'
                                  ,byVariableRow = "EquipVintage_bins"
                                  ,byVariableColumn = "CK_Building_ID"
                                  ,columnAggregate = "Remove"
                                  ,rowAggregate = "All Vintages")
names(item52.os.cast)
if(os.ind == "scl"){
  item52.os.final <- data.frame("BuildingType"          = item52.os.cast$BuildingType
                                ,"EquipVintage_bins"    = item52.os.cast$EquipVintage_bins
                                ,"Mean_SCL.GenPop"      = item52.os.cast$`Mean_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item52.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item52.os.cast$`n_SCL GenPop`
                                ,"Mean_SCL.LI"          = NA#item52.os.cast$`Mean_SCL LI`
                                ,"SE_SCL.LI"            = NA#item52.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = NA#item52.os.cast$`n_SCL LI`
                                ,"Mean_SCL.EH"          = item52.os.cast$`Mean_SCL EH`
                                ,"SE_SCL.EH"            = item52.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item52.os.cast$`n_SCL EH`
                                ,"Mean_2017.RBSA.PS"    = item52.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item52.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item52.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item52.os.final <- data.frame("BuildingType"          = item52.os.cast$BuildingType
                                ,"EquipVintage_bins"    = item52.os.cast$EquipVintage_bins
                                ,"Mean_SnoPUD"          = item52.os.cast$`Mean_SnoPUD`
                                ,"SE_SnoPUD"            = item52.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"             = item52.os.cast$`n_SnoPUD`
                                ,"Mean_2017.RBSA.PS"    = item52.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item52.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item52.os.cast$`n_2017 RBSA PS`
                                ,"Mean_RBSA.NW"         = item52.os.cast$`Mean_2017 RBSA NW`
                                ,"SE_RBSA.NW"           = item52.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"            = item52.os.cast$`n_2017 RBSA NW`)
}




# row ordering example code
unique(item52.os.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1999"
              ,"2000-2006"
              ,"2007-2014"
              ,"Post 2014"
              ,"Vintage Unknown"
              ,"All Vintages")
item52.os.final <- item52.os.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item52.os.final <- data.frame(item52.os.final)

# Export table
item52.os.final.SF <- item52.os.final[which(item52.os.final$BuildingType == "Single Family"),-1]

exportTable(item52.os.final.SF, "SF", "Table 59", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
#Item 53: DISTRIBUTION OF AIR SOURCE HEAT PUMP EFFICIENCY (HSPF) BY CK_Building_ID (SF table 60)
#############################################################################################
#data for item 53
item53.os.dat <- mechanical.dat3[which(mechanical.dat3$CK_Cadmus_ID != "BUILDING"),]

#remove any irrelevant HSPF datapoints (could not collect)
item53.os.dat1 <- item53.os.dat[which(item53.os.dat$HSPF != "Could Not Collect"),]

#remove any repeated header lines
item53.os.dat2 <- item53.os.dat1[which(item53.os.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#make HSPF information numeric
item53.os.dat2$HSPF <- as.numeric(as.character(item53.os.dat2$HSPF))

#Join cleaned item 53 mechanical information with cleaned scl site information
item53.os.dat3 <- left_join(os.dat, item53.os.dat2, by = "CK_Cadmus_ID")
item53.os.dat4 <- item53.os.dat3[which(!is.na(item53.os.dat3$HSPF)),]

# Create HSPF_bins according to previous scl bins
item53.os.dat4$HSPF_bins <- as.numeric(as.character(item53.os.dat4$HSPF))
item53.os.dat4$HSPF_bins[which(item53.os.dat4$HSPF >= 6.8 & item53.os.dat4$HSPF < 7.7)] <- "6.8-7.6"
item53.os.dat4$HSPF_bins[which(item53.os.dat4$HSPF >= 7.7 & item53.os.dat4$HSPF < 8.3)] <- "7.7-8.2"
item53.os.dat4$HSPF_bins[which(item53.os.dat4$HSPF >= 8.3 & item53.os.dat4$HSPF < 9.0)] <- "8.3-8.9"
item53.os.dat4$HSPF_bins[which(item53.os.dat4$HSPF >= 9.0)] <- "9.0+"
#make sure that the HSPF bins are what we want/expect
unique(item53.os.dat4$HSPF_bins)
#add count
item53.os.dat4$count <- 1


item53.os.data <- weightedData(item53.os.dat4[-which(colnames(item53.os.dat4) %in% c("Generic"                        
                                                                            ,"Heating.Fuel"
                                                                            ,"Component.1.Year.of.Manufacture"
                                                                            ,"Heating.Efficiency.-.High"      
                                                                            ,"HSPF"
                                                                            ,"EquipVintage_bins"
                                                                            ,"EquipVintage_bins_MH"
                                                                            ,"HSPF_bins"                      
                                                                            ,"count"
                                                                            ,"Primary.Heating.System" ))])
item53.os.data <- left_join(item53.os.data, item53.os.dat4[which(colnames(item53.os.dat4) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"                        
                                                                                     ,"Heating.Fuel"
                                                                                     ,"Component.1.Year.of.Manufacture"
                                                                                     ,"Heating.Efficiency.-.High"      
                                                                                     ,"HSPF"
                                                                                     ,"EquipVintage_bins"
                                                                                     ,"EquipVintage_bins_MH"
                                                                                     ,"HSPF_bins"                      
                                                                                     ,"count"
                                                                                     ,"Primary.Heating.System"))])
item53.os.data$count <- 1
########################
# Weighted Analysis
########################
item53.os.final <- proportionRowsAndColumns1(CustomerLevelData    = item53.os.data
                                          ,valueVariable       = 'count'
                                          ,columnVariable      = 'CK_Building_ID'
                                          ,rowVariable         = 'HSPF_bins'
                                          ,aggregateColumnName = "Region")


#cast data into correct table format
item53.os.cast <- dcast(setDT(item53.os.final)
                     , formula   = BuildingType + HSPF_bins ~ CK_Building_ID
                     , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

names(item53.os.cast)
if(os.ind == "scl"){
  item53.os.table <- data.frame("BuildingType"          = item53.os.cast$BuildingType
                                ,"HSPF"                 = item53.os.cast$HSPF_bins
                                ,"Percent_SCL.GenPop"   = item53.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item53.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item53.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = NA#item53.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = NA#item53.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = NA#item53.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item53.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item53.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item53.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item53.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item53.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item53.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item53.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = NA#item53.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item53.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item53.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item53.os.table <- data.frame("BuildingType"          = item53.os.cast$BuildingType
                                ,"HSPF"                 = item53.os.cast$HSPF_bins
                                ,"Percent_SnoPUD"          = item53.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item53.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item53.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item53.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item53.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item53.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item53.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item53.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item53.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item53.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item53.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item53.os.cast$`EB_2017 RBSA NW`)
}



#subset to only the relevant building types for this item
item53.os.table.SF <- item53.os.table[which(item53.os.table$BuildingType %in% c("Single Family")),-1]

exportTable(item53.os.table.SF, "SF", "Table 60", weighted = TRUE, osIndicator = export.ind, OS = T)


########################
# Unweighted Analysis
########################
item53.os.final <- proportions_two_groups_unweighted(CustomerLevelData    = item53.os.data
                                                  ,valueVariable       = 'count'
                                                  ,columnVariable      = 'CK_Building_ID'
                                                  ,rowVariable         = 'HSPF_bins'
                                                  ,aggregateColumnName = "Region")

#cast data into correct table format
item53.os.cast <- dcast(setDT(item53.os.final)
                     , formula   = BuildingType + HSPF_bins ~ CK_Building_ID
                     , value.var = c("Percent", "SE", "Count", "n"))

names(item53.os.cast)
if(os.ind == "scl"){
  item53.os.table <- data.frame("BuildingType"    = item53.os.cast$BuildingType
                                ,"HSPF"           = item53.os.cast$HSPF_bins
                                ,"Percent_SCL.GenPop"   = item53.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item53.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item53.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = NA#item53.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = NA#item53.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = NA#item53.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item53.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item53.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item53.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item53.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item53.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item53.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item53.os.table <- data.frame("BuildingType"    = item53.os.cast$BuildingType
                                ,"HSPF"           = item53.os.cast$HSPF_bins
                                ,"Percent_SnoPUD"          = item53.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item53.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item53.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item53.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item53.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item53.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item53.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item53.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item53.os.cast$`n_2017 RBSA NW`)
}




#subset to only the relevant building types for this item
item53.os.table.SF <- item53.os.table[which(item53.os.table$BuildingType %in% c("Single Family")),-1]

exportTable(item53.os.table.SF, "SF", "Table 60", weighted = FALSE, osIndicator = export.ind, OS = T)
