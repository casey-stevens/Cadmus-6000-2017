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


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))
#subset to columns needed for analysis
mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"SEER"
                                                                        ,"Component.1.Year.of.Manufacture"
                                                                        ,"Seasonal./.Portable.Equipment?"))]
#remove any irrelevant equipment vintages (datapoint not asked for)
mechanical.dat2 <- mechanical.dat1[which(mechanical.dat1$Component.1.Year.of.Manufacture != "-- Datapoint not asked for --"),]
#remove any NA equipment vintages
mechanical.dat3 <- mechanical.dat2[which(!(is.na(mechanical.dat2$Component.1.Year.of.Manufacture))),]


# Bin equipment vintages for items 50 and 52 (4 categories)
mechanical.dat3$EquipVintage_bins <- as.numeric(as.character(mechanical.dat3$`Component.1.Year.of.Manufacture`))
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` < 1990)] <- "Pre 1990"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 1990 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2000)] <- "1990-1999"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2000 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2006)] <- "2000-2006"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2006)] <- "Post 2006"
#check uniques
unique(mechanical.dat3$EquipVintage_bins)









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

#Join cleaned item 56 mechanical information with cleaned RBSA site information
item56.dat4 <- left_join(item56.dat3, rbsa.dat, by = "CK_Cadmus_ID")

# Remove duplicates
item56.dat5 <- item56.dat4[ !duplicated(item56.dat4), ]


# Weighting
item56.data <- weightedData(item56.dat5[-which(colnames(item56.dat5) %in% c("SEER"
                                                                            ,"EquipVintage_bins"))])

item56.data <- left_join(item56.data4, item43.dat5[which(colnames(item56.dat4) %in% c("CK_Cadmus_ID"
                                                                                      ,"HSPF"
                                                                                      ,"EquipVintage_bins"))])

# Analysis application
item56.final <- mean_one_group(CustomerLevelData,
                               valueVariable, 
                               byVariable,
                               aggregateRow,
                               weighted = TRUE)

# Export table
# SF = Table 59, MH = Table 39
item56.final.SF <- item56.final[which(item56.final$BuildingType == "Single Family"),-1]
item56.final.MH <- item56.final[which(item56.final$BuildingType == "Manufactured"),-1]

exportTable(item43.final.SF, "SF", "Table 59")
exportTable(item43.final.MH, "MH", "Table 39")


# OLD CODE #
#
# #Calculate  and region level SEER mean and SD by equip vintage bins:
# #average SEER and standard deviation by building type, equipment vintage
# item56.sum <- summarise(group_by(item56.dat4, BuildingType, EquipVintage_bins)
#                         ,SampleSize = length(unique(CK_Cadmus_ID))
#                         ,Mean = mean(SEER)
#                         ,SE = sd(SEER) / sqrt(SampleSize))
# 
# #Calculate region level counts across equipment bins
# #SEER means and SDs by building type for region and all vintages
# item56.sum1 <- summarise(group_by(item56.dat4, BuildingType)
#                          ,EquipVintage_bins = "All Vintages"
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Mean = mean(SEER)
#                          ,SE = sd(SEER) / sqrt(SampleSize))
# 
# #row bins SEER means and SDs by building types by and across equipment bins for  and region
# item56.final <- rbind.data.frame(item56.sum, item56.sum1, stringsAsFactors = F) 
# 
# item56.table <- data.frame("BuildingType" = item56.final$BuildingType
#                            ,"Equipment.Vintage" = item56.final$EquipVintage_bins
#                            ,"Mean" = item56.final$Mean
#                            ,"SE" = item56.final$SE
#                            ,"SampleSize" = item56.final$SampleSize)
# 
# 
# item56.table1 <- item56.table[which(item56.table$BuildingType %in% c("Single Family")),]
# 




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

#remove any NAs in SEER
item57.dat3 <- item57.dat2[which(!(is.na(item57.dat2$SEER))),]

#make SEER information numeric
item57.dat3$SEER <- as.numeric(as.character(item57.dat3$SEER))

#Join cleaned item 57 mechanical information with cleaned RBSA site information
item57.dat4 <- left_join(item57.dat3, rbsa.dat, by = "CK_Cadmus_ID")



# Weighting
item56.data <- weightedData(item56.dat5[-which(colnames(item56.dat5) %in% c("SEER"
                                                                            ,"EquipVintage_bins"))])

item56.data <- left_join(item56.data4, item43.dat5[which(colnames(item56.dat4) %in% c("CK_Cadmus_ID"
                                                                                      ,"HSPF"
                                                                                      ,"EquipVintage_bins"))])

# Analysis application
item56.final <- mean_one_group(CustomerLevelData,
                               valueVariable, 
                               byVariable,
                               aggregateRow,
                               weighted = TRUE)

# Export table
# SF = Table 59, MH = Table 39
item56.final.SF <- item56.final[which(item56.final$BuildingType == "Single Family"),-1]
item56.final.MH <- item56.final[which(item56.final$BuildingType == "Manufactured"),-1]

exportTable(item43.final.SF, "SF", "Table 59")
exportTable(item43.final.MH, "MH", "Table 39")

# 
# OLD CODE #
# 
# #Calculate  and region level SEER mean and SD by equip vintage bins:
# #average SEER and standard deviation by building type, equipment vintage
# item57.sum <- summarise(group_by(item57.dat4, BuildingType, EquipVintage_bins)
#                         ,SampleSize = length(unique(CK_Cadmus_ID))
#                         ,Mean = mean(SEER)
#                         ,SE = sd(SEER) / sqrt(SampleSize))
# 
# #Calculate region level counts across equipment bins
# #SEER means and SDs by building type for region and all vintages
# item57.sum1 <- summarise(group_by(item57.dat4, BuildingType)
#                          ,EquipVintage_bins = "All Vintages"
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Mean = mean(SEER)
#                          ,SE = sd(SEER) / sqrt(SampleSize))
# 
# #row bins SEER means and SDs by building types by and across equipment bins for  and region
# item57.final <- rbind.data.frame(item57.sum, item57.sum1, stringsAsFactors = F) 
# 
# item57.table <- data.frame("BuildingType" = item57.final$BuildingType
#                            ,"Equipment.Vintage" = item57.final$EquipVintage_bins
#                            ,"Mean" = item57.final$Mean
#                            ,"SE" = item57.final$SE
#                            ,"SampleSize" = item57.final$SampleSize)
# 
# 
# item57.table1 <- item57.table[which(item57.table$BuildingType %in% c("Single Family")),]
# 
# 
# 





#############################################################################################
#Item 58: AVERAGE NUMBER OF PORTABLE COOLING DEVICES PER HOME BY STATE (SF table 65, MH table 45)
#############################################################################################
#check unique SEER values
unique(mechanical.dat1$`Seasonal./.Portable.Equipment?`)

#subset to only seasonal / portable = YES
item58.dat <- mechanical.dat1[which(mechanical.dat1$`Seasonal./.Portable.Equipment?` == "Yes"),]

#check unique Generic values
unique(item58.dat$Generic)



# Weighting
item58.data <- weightedData(item58.dat5[-which(colnames(item58.dat5) %in% c("SEER"
                                                                            ,"EquipVintage_bins"))])

item58.data <- left_join(item58.data4, item43.dat5[which(colnames(item58.dat4) %in% c("CK_Cadmus_ID"
                                                                                      ,"HSPF"
                                                                                      ,"EquipVintage_bins"))])

# Analysis application
item58.final <- mean_one_group(CustomerLevelData,
                               valueVariable, 
                               byVariable,
                               aggregateRow,
                               weighted = TRUE)

# Export table
# SF = Table 59, MH = Table 39
item58.final.SF <- item58.final[which(item58.final$BuildingType == "Single Family"),-1]
item58.final.MH <- item58.final[which(item58.final$BuildingType == "Manufactured"),-1]

exportTable(item43.final.SF, "SF", "Table 59")
exportTable(item43.final.MH, "MH", "Table 39")

