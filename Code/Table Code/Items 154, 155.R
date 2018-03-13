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
rbsa.dat2 <- rbsa.dat[-grep("bldg",rbsa.dat$CK_Building_ID, ignore.case = T),]

# one.line.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.export), 
#                            sheet = "Site One Line Summary", startRow = 2)

colnames(one.line.dat)
keep.cols <- c("Cadmus.ID", 
               "Annual.Wood.Cord.Usage.(Cords)",
               "Annual.Wood.Pellet.Usage.(Tons)",
               "Annual.Propane.Usage.(Gallons)",
               "Annual.Oil.Usage.(Gallons)")
one.line.dat2 <- one.line.dat[,which(colnames(one.line.dat) %in% keep.cols)]
one.line.dat2$CK_Cadmus_ID <- trimws(toupper(one.line.dat2$Cadmus.ID))
final.dat <- merge(rbsa.dat2, one.line.dat2,
                   by = "CK_Cadmus_ID",
                   all.x = T)

#############################################################################################
# Item 154: TABLE 161: AVERAGE ANNUAL OTHER FUEL USE PER HOME BY STATE 
#############################################################################################
item154.data <- final.dat
item154.data$kBtu <- 
  item154.data$`Annual.Wood.Cord.Usage.(Cords)` * 18000 +
  item154.data$`Annual.Wood.Pellet.Usage.(Tons)` * 16000 +
  item154.data$`Annual.Oil.Usage.(Gallons)` * 140 +
  item154.data$`Annual.Propane.Usage.(Gallons)` * 92
  
drop.for.weighting <- c("Cadmus.ID",
                        "Annual.Wood.Cord.Usage.(Cords)",
                        "Annual.Wood.Pellet.Usage.(Tons)",
                        "Annual.Propane.Usage.(Gallons)",
                        "Annual.Oil.Usage.(Gallons)",
                        "kBtu")
# item154.data <- item154.data[which(item154.data$kBtu > 0),]

item154.weighted <- weightedData(item154.data[-c(which(colnames(item154.data) %in% 
                                                         drop.for.weighting))])
item154.weighted <- left_join(item154.weighted, item154.data[c(1,c(which(colnames(item154.data) %in% 
                                                                           drop.for.weighting)))])

################################
# Weighted Analysis1
################################
item154.final <- mean_one_group(CustomerLevelData = item154.weighted
                                , valueVariable = 'kBtu'
                                , byVariable    = 'State'
                                , aggregateRow  = 'Region')

#subset by home type
item154.final.SF <- item154.final[which(item154.final$BuildingType == "Single Family"),-1]
item154.final.MH <- item154.final[which(item154.final$BuildingType == "Manufactured"),-1]
##export data - haven't changed any of the datsets waiting for Casey to QC
exportTable(item154.final.SF, "SF", "Table 161", weighted = TRUE)
# exportTable(item154.final.MH, "MH", "Table 136", weighted = TRUE)

################################
# Unweighted Analysis
################################
item154.final <- mean_one_group_unweighted(CustomerLevelData = item154.weighted
                                           , valueVariable = 'kBtu'
                                           , byVariable    = 'State'
                                           , aggregateRow  = 'Region')
#subset by home type
item154.final.SF <- item154.final[which(item154.final$BuildingType == "Single Family"),-1]
item154.final.MH <- item154.final[which(item154.final$BuildingType == "Manufactured"),-1]
#export data
exportTable(item154.final.SF, "SF", "Table 161", weighted = FALSE)
# exportTable(item154.final.MH, "MH", "Table 136", weighted = FALSE)










#############################################################################################
# Item 155: TABLE 162: AVERAGE EUI OTHER FUEL USE PER HOME BY STATE 
#############################################################################################
item155.data <- final.dat
item155.data$kBtu <- 
  item155.data$`Annual.Wood.Cord.Usage.(Cords)` * 18000 +
  item155.data$`Annual.Wood.Pellet.Usage.(Tons)` * 16000 +
  item155.data$`Annual.Oil.Usage.(Gallons)` * 140 +
  item155.data$`Annual.Propane.Usage.(Gallons)` * 92
item155.data1 <- item155.data[which(item155.data$Conditioned.Area > 0),]
item155.data1$EUI <- item155.data1$kBtu/item155.data1$Conditioned.Area

drop.for.weighting <- c("Cadmus.ID",
                        "Annual.Wood.Cord.Usage.(Cords)",
                        "Annual.Wood.Pellet.Usage.(Tons)",
                        "Annual.Propane.Usage.(Gallons)",
                        "Annual.Oil.Usage.(Gallons)",
                        "kBtu",
                        "EUI")

item155.weighted <- weightedData(item155.data1[-c(which(colnames(item155.data1) %in% 
                                                         drop.for.weighting))])
item155.weighted <- 
  left_join(item155.weighted, item155.data1[c(1,
                                             c(which(colnames(item155.data1) %in% 
                                                       drop.for.weighting)))])

################################
# Weighted Analysis1
################################
item155.final <- mean_one_group(CustomerLevelData = item155.weighted
                                , valueVariable = 'EUI'
                                , byVariable    = 'State'
                                , aggregateRow  = 'Region')

#subset by home type
item155.final.SF <- item155.final[which(item155.final$BuildingType == "Single Family"),-1]
item155.final.MH <- item155.final[which(item155.final$BuildingType == "Manufactured"),-1]
#export data - haven't changed any of the datsets waiting for Casey to QC
exportTable(item155.final.SF, "SF", "Table 162", weighted = TRUE)
# exportTable(item155.final.MH, "MH", "Table 137", weighted = TRUE)

################################
# Unweighted Analysis
################################
item155.final <- mean_one_group_unweighted(CustomerLevelData = item155.weighted
                                           , valueVariable = 'EUI'
                                           , byVariable    = 'State'
                                           , aggregateRow  = 'Region')
#subset by home type
item155.final.SF <- item155.final[which(item155.final$BuildingType == "Single Family"),-1]
item155.final.MH <- item155.final[which(item155.final$BuildingType == "Manufactured"),-1]
#export data
exportTable(item155.final.SF, "SF", "Table 162", weighted = FALSE)
# exportTable(item155.final.MH, "MH", "Table 137", weighted = FALSE)



























# ############################################################################################################
# #
# #
# # OVERSAMPLE ANALYSIS
# #
# #
# ############################################################################################################
# 
# # Read in clean os data
# os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
# length(unique(os.dat$CK_Cadmus_ID))
# os.dat$CK_Building_ID <- os.dat$Category
# os.dat <- os.dat[which(names(os.dat) != "Category")]
# 
# final.dat <- merge(os.dat, one.line.dat2,
#                    by = "CK_Cadmus_ID")
# 
# #############################################################################################
# # Item 154: TABLE 161: AVERAGE ANNUAL OTHER FUEL USE PER HOME BY CK_Building_ID 
# #############################################################################################
# item154.os.data <- final.dat
# item154.os.data$kBtu <- 
#   item154.os.data$`Annual.Wood.Cord.Usage.(Cords)` * 18000 +
#   item154.os.data$`Annual.Wood.Pellet.Usage.(Tons)` * 16000 +
#   item154.os.data$`Annual.Oil.Usage.(Gallons)` * 140 +
#   item154.os.data$`Annual.Propane.Usage.(Gallons)` * 92
# 
# drop.for.weighting <- c("Cadmus.ID",
#                         "Annual.Wood.Cord.Usage.(Cords)",
#                         "Annual.Wood.Pellet.Usage.(Tons)",
#                         "Annual.Propane.Usage.(Gallons)",
#                         "Annual.Oil.Usage.(Gallons)",
#                         "kBtu")
# 
# item154.os.weighted <- weightedData(item154.os.data[-c(which(colnames(item154.os.data) %in% 
#                                                                drop.for.weighting))])
# item154.os.weighted <- left_join(item154.os.weighted, unique(item154.os.data[c(1,c(which(colnames(item154.os.data) %in% 
#                                                                                            drop.for.weighting)))]))
# item154.os.weighted$count <- 1
# ################################
# # Weighted Analysis1
# ################################
# item154.os.final <- mean_one_group(CustomerLevelData = item154.os.weighted
#                                 , valueVariable = 'kBtu'
#                                 , byVariable    = 'CK_Building_ID'
#                                 , aggregateRow  = 'Remove')
# item154.os.final <- item154.os.final[which(item154.os.final$CK_Building_ID %notin% c("Remove", "Total")),]
# #subset by home type
# item154.os.final.SF <- item154.os.final[which(item154.os.final$BuildingType == "Single Family"),-1]
# 
# exportTable(item154.os.final.SF, "SF", "Table 161", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# ################################
# # Unweighted Analysis
# ################################
# item154.os.final <- mean_one_group_unweighted(CustomerLevelData = item154.os.weighted
#                                            , valueVariable = 'kBtu'
#                                            , byVariable    = 'CK_Building_ID'
#                                            , aggregateRow  = 'Remove')
# item154.os.final <- item154.os.final[which(item154.os.final$CK_Building_ID %notin% c("Remove", "Total")),]
# #subset by home type
# item154.os.final.SF <- item154.os.final[which(item154.os.final$BuildingType == "Single Family"),-1]
# 
# exportTable(item154.os.final.SF, "SF", "Table 161", weighted = FALSE, osIndicator = export.ind, OS = T)
# 
# 
# 
# 
# 
# #############################################################################################
# # Item 155: TABLE 162: AVERAGE EUI OTHER FUEL USE PER HOME BY CK_Building_ID 
# #############################################################################################
# item155.os.data <- final.dat
# item155.os.data$kBtu <- 
#   item155.os.data$`Annual.Wood.Cord.Usage.(Cords)` * 18000 +
#   item155.os.data$`Annual.Wood.Pellet.Usage.(Tons)` * 16000 +
#   item155.os.data$`Annual.Oil.Usage.(Gallons)` * 140 +
#   item155.os.data$`Annual.Propane.Usage.(Gallons)` * 92
# item155.os.data1 <- item155.os.data[which(item155.os.data$Conditioned.Area > 0),]
# item155.os.data1$EUI <- item155.os.data1$kBtu/item155.os.data1$Conditioned.Area
# 
# drop.for.weighting <- c("Cadmus.ID",
#                         "Annual.Wood.Cord.Usage.(Cords)",
#                         "Annual.Wood.Pellet.Usage.(Tons)",
#                         "Annual.Propane.Usage.(Gallons)",
#                         "Annual.Oil.Usage.(Gallons)",
#                         "kBtu",
#                         "EUI")
# 
# item155.os.weighted <- weightedData(item155.os.data1[-c(which(colnames(item155.os.data1) %in% 
#                                                           drop.for.weighting))])
# item155.os.weighted <- left_join(item155.os.weighted, unique(item155.os.data1[c(1,c(which(colnames(item155.os.data1) %in% 
#                                                                                    drop.for.weighting)))]))
# item155.os.weighted$count <- 1
# ################################
# # Weighted Analysis1
# ################################
# item155.os.final <- mean_one_group(CustomerLevelData = item155.os.weighted
#                                 , valueVariable = 'EUI'
#                                 , byVariable    = 'CK_Building_ID'
#                                 , aggregateRow  = 'Remove')
# item155.os.final <- item155.os.final[which(item155.os.final$CK_Building_ID %notin% c("Remove", "Total")),]
# 
# #subset by home type
# item155.os.final.SF <- item155.os.final[which(item155.os.final$BuildingType == "Single Family"),-1]
# 
# exportTable(item155.os.final.SF, "SF", "Table 162", weighted = TRUE, osIndicator = export.ind, OS = T)
# ################################
# # Unweighted Analysis
# ################################
# item155.os.final <- mean_one_group_unweighted(CustomerLevelData = item155.os.weighted
#                                            , valueVariable = 'EUI'
#                                            , byVariable    = 'CK_Building_ID'
#                                            , aggregateRow  = 'Remove')
# item155.os.final <- item155.os.final[which(item155.os.final$CK_Building_ID %notin% c("Remove", "Total")),]
# #subset by home type
# item155.os.final.SF <- item155.os.final[which(item155.os.final$BuildingType == "Single Family"),-1]
# 
# exportTable(item155.os.final.SF, "SF", "Table 162", weighted = FALSE, osIndicator = export.ind, OS = T)