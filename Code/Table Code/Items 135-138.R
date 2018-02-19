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
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))



#############################################################################################
#Item 135: DISTRIBUTION OF WOOD USE AS HEATING FUEL BY STATE (SF table 142, MH table 117)
#############################################################################################
#subset to columns needed for analysis
item135.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseOtherFuel_QuantityOfWood_Cords"
                                                                                     ,""))])

colnames(item135.dat) <- c("CK_Cadmus_ID", "QTY")
item135.dat$QTY <- as.numeric(as.character(item135.dat$QTY))
#remove any repeat header rows from exporting
item135.dat0 <- item135.dat[which(item135.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item135.dat1 <- summarise(group_by(item135.dat0, CK_Cadmus_ID)
                          ,QTY = sum(QTY, na.rm = T))
  
item135.dat2 <- left_join(rbsa.dat, item135.dat1, by = "CK_Cadmus_ID")
item135.dat2 <- item135.dat2[which(!is.na(item135.dat2$QTY)),]

unique(item135.dat2$QTY)
item135.dat2$QTY_bins <- as.numeric(as.character(item135.dat2$QTY))
item135.dat2$QTY_bins[which(item135.dat2$QTY == 0)] <- "None"
item135.dat2$QTY_bins[which(item135.dat2$QTY > 0  & item135.dat2$QTY< 1)] <- "< 1 Cord"
item135.dat2$QTY_bins[which(item135.dat2$QTY >= 1 & item135.dat2$QTY < 4)] <- "1-3 Cords"
item135.dat2$QTY_bins[which(item135.dat2$QTY >= 4 & item135.dat2$QTY < 7)] <- "4-6 Cords"
item135.dat2$QTY_bins[which(item135.dat2$QTY > 6)] <- "> 6 Cords"


################################################
# Adding pop and sample sizes for weights
################################################
item135.data <- weightedData(item135.dat2[-which(colnames(item135.dat2) %in% c("QTY"               
                                                                               ,"QTY_bins"))])
item135.data <- left_join(item135.data, item135.dat2[which(colnames(item135.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"QTY"               
                                                                                         ,"QTY_bins"))])
item135.data$count <- 1
#######################
# Weighted Analysis
#######################
item135.final <- proportionRowsAndColumns1(CustomerLevelData = item135.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'QTY_bins'
                                           ,aggregateColumnName = "Region")

item135.cast <- dcast(setDT(item135.final)
                      , formula = BuildingType + QTY_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item135.table <- data.frame("BuildingType"   = item135.cast$BuildingType
                            ,"Annual.Wood.Use"= item135.cast$QTY_bins
                            ,"Percent_ID"     = item135.cast$w.percent_ID
                            ,"SE_ID"          = item135.cast$w.SE_ID
                            ,"n_ID"           = item135.cast$n_ID
                            ,"Percent_MT"     = item135.cast$w.percent_MT
                            ,"SE_MT"          = item135.cast$w.SE_MT
                            ,"n_MT"           = item135.cast$n_MT
                            ,"Percent_OR"     = item135.cast$w.percent_OR
                            ,"SE_OR"          = item135.cast$w.SE_OR
                            ,"n_OR"           = item135.cast$n_OR
                            ,"Percent_WA"     = item135.cast$w.percent_WA
                            ,"SE_WA"          = item135.cast$w.SE_WA
                            ,"n_WA"           = item135.cast$n_WA
                            ,"Percent_Region" = item135.cast$w.percent_Region
                            ,"SE_Region"      = item135.cast$w.SE_Region
                            ,"n_Region"       = item135.cast$n_Region
                            ,"EB_ID"          = item135.cast$EB_ID
                            ,"EB_MT"          = item135.cast$EB_MT
                            ,"EB_OR"          = item135.cast$EB_OR
                            ,"EB_WA"          = item135.cast$EB_WA
                            ,"EB_Region"      = item135.cast$EB_Region
)
#QAQC
stopifnot(sum(item135.table[which(item135.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(item135.table))], na.rm = T) == 10)


item135.final.SF <- item135.table[which(item135.table$BuildingType == "Single Family")
                                  ,-which(colnames(item135.table) %in% c("BuildingType"))]
item135.final.MH <- item135.table[which(item135.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item135.table) %in% c("BuildingType"))]

# exportTable(item135.final.SF, "SF", "Table 142", weighted = TRUE)
exportTable(item135.final.MH, "MH", "Table 117", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item135.final <- proportions_two_groups_unweighted(CustomerLevelData = item135.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'QTY_bins'
                                                   ,aggregateColumnName = "Region")

item135.cast <- dcast(setDT(item135.final)
                      , formula = BuildingType + QTY_bins ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


item135.table <- data.frame("BuildingType"   = item135.cast$BuildingType
                            ,"Annual.Wood.Use"= item135.cast$QTY_bins
                            ,"Percent_ID"     = item135.cast$Percent_ID
                           ,"SE_ID"          = item135.cast$SE_ID
                           ,"n_ID"           = item135.cast$n_ID
                           ,"Percent_MT"     = item135.cast$Percent_MT
                           ,"SE_MT"          = item135.cast$SE_MT
                           ,"n_MT"           = item135.cast$n_MT
                           ,"Percent_OR"     = item135.cast$Percent_OR
                           ,"SE_OR"          = item135.cast$SE_OR
                           ,"n_OR"           = item135.cast$n_OR
                           ,"Percent_WA"     = item135.cast$Percent_WA
                           ,"SE_WA"          = item135.cast$SE_WA
                           ,"n_WA"           = item135.cast$n_WA
                           ,"Percent_Region" = item135.cast$Percent_Region
                           ,"SE_Region"      = item135.cast$SE_Region
                           ,"n_Region"       = item135.cast$n_Region
                           
)
stopifnot(sum(item135.table[which(item135.table$BuildingType == "Single Family")
                           ,grep("Percent",colnames(item135.table))], na.rm = T) == 10)


item135.final.SF <- item135.table[which(item135.table$BuildingType == "Single Family")
                                ,-which(colnames(item135.table) %in% c("BuildingType"))]
item135.final.MH <- item135.table[which(item135.table$BuildingType == "Manufactured")
                                ,-which(colnames(item135.table) %in% c("BuildingType"))]

# exportTable(item135.final.SF, "SF", "Table 142", weighted = FALSE)
exportTable(item135.final.MH, "MH", "Table 117", weighted = FALSE)





#############################################################################################
#Item 136: DISTRIBUTION OF PELLET USE AS HEATING FUEL BY STATE (SF table 143, MH table 118)
#############################################################################################
#subset to columns needed for analysis
item136.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseOtherFuel_QuantityOfPellets_Tons"
                                                                                     ,""))])

colnames(item136.dat) <- c("CK_Cadmus_ID", "QTY")
item136.dat$QTY <- as.numeric(as.character(item136.dat$QTY))
#remove any repeat header rows from exporting
item136.dat0 <- item136.dat[which(item136.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item136.dat1 <- summarise(group_by(item136.dat0, CK_Cadmus_ID)
                          ,QTY = sum(QTY, na.rm = T))

item136.dat2 <- left_join(rbsa.dat, item136.dat1, by = "CK_Cadmus_ID")
item136.dat2 <- item136.dat2[which(!is.na(item136.dat2$QTY)),]

item136.dat2$QTY_bins <- item136.dat2$QTY
item136.dat2$QTY_bins[which(item136.dat2$QTY == 0)] <- "None"
item136.dat2$QTY_bins[which(item136.dat2$QTY >  0 & item136.dat2$QTY < 1)] <- "< 1 Ton"
item136.dat2$QTY_bins[which(item136.dat2$QTY >= 1 & item136.dat2$QTY < 3)] <- "1-2 Tons"
item136.dat2$QTY_bins[which(item136.dat2$QTY >= 2 & item136.dat2$QTY < 5)] <- "2-4 Tons"
item136.dat2$QTY_bins[which(item136.dat2$QTY > 4)] <- "> 4 Tons"


################################################
# Adding pop and sample sizes for weights
################################################
item136.data <- weightedData(item136.dat2[-which(colnames(item136.dat2) %in% c("QTY"               
                                                                               ,"QTY_bins"))])
item136.data <- left_join(item136.data, item136.dat2[which(colnames(item136.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"QTY"               
                                                                                         ,"QTY_bins"))])
item136.data$count <- 1
#######################
# Weighted Analysis
#######################
item136.final <- proportionRowsAndColumns1(CustomerLevelData = item136.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'QTY_bins'
                                           ,aggregateColumnName = "Region")

item136.cast <- dcast(setDT(item136.final)
                      , formula = BuildingType + QTY_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item136.table <- data.frame("BuildingType"   = item136.cast$BuildingType
                            ,"Annual.Wood.Use"= item136.cast$QTY_bins
                            ,"Percent_ID"     = item136.cast$w.percent_ID
                            ,"SE_ID"          = item136.cast$w.SE_ID
                            ,"n_ID"           = item136.cast$n_ID
                            ,"Percent_MT"     = item136.cast$w.percent_MT
                            ,"SE_MT"          = item136.cast$w.SE_MT
                            ,"n_MT"           = item136.cast$n_MT
                            ,"Percent_OR"     = item136.cast$w.percent_OR
                            ,"SE_OR"          = item136.cast$w.SE_OR
                            ,"n_OR"           = item136.cast$n_OR
                            ,"Percent_WA"     = item136.cast$w.percent_WA
                            ,"SE_WA"          = item136.cast$w.SE_WA
                            ,"n_WA"           = item136.cast$n_WA
                            ,"Percent_Region" = item136.cast$w.percent_Region
                            ,"SE_Region"      = item136.cast$w.SE_Region
                            ,"n_Region"       = item136.cast$n_Region
                            ,"EB_ID"          = item136.cast$EB_ID
                            ,"EB_MT"          = item136.cast$EB_MT
                            ,"EB_OR"          = item136.cast$EB_OR
                            ,"EB_WA"          = item136.cast$EB_WA
                            ,"EB_Region"      = item136.cast$EB_Region
)
#QAQC
stopifnot(sum(item136.table[which(item136.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(item136.table))], na.rm = T) == 10)


item136.final.SF <- item136.table[which(item136.table$BuildingType == "Single Family")
                                  ,-which(colnames(item136.table) %in% c("BuildingType"))]
item136.final.MH <- item136.table[which(item136.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item136.table) %in% c("BuildingType"))]

# exportTable(item136.final.SF, "SF", "Table 143", weighted = TRUE)
exportTable(item136.final.MH, "MH", "Table 118", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item136.final <- proportions_two_groups_unweighted(CustomerLevelData = item136.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'QTY_bins'
                                                   ,aggregateColumnName = "Region")

item136.cast <- dcast(setDT(item136.final)
                      , formula = BuildingType + QTY_bins ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


item136.table <- data.frame("BuildingType"   = item136.cast$BuildingType
                            ,"Annual.Wood.Use"= item136.cast$QTY_bins
                            ,"Percent_ID"     = item136.cast$Percent_ID
                            ,"SE_ID"          = item136.cast$SE_ID
                            ,"n_ID"           = item136.cast$n_ID
                            ,"Percent_MT"     = item136.cast$Percent_MT
                            ,"SE_MT"          = item136.cast$SE_MT
                            ,"n_MT"           = item136.cast$n_MT
                            ,"Percent_OR"     = item136.cast$Percent_OR
                            ,"SE_OR"          = item136.cast$SE_OR
                            ,"n_OR"           = item136.cast$n_OR
                            ,"Percent_WA"     = item136.cast$Percent_WA
                            ,"SE_WA"          = item136.cast$SE_WA
                            ,"n_WA"           = item136.cast$n_WA
                            ,"Percent_Region" = item136.cast$Percent_Region
                            ,"SE_Region"      = item136.cast$SE_Region
                            ,"n_Region"       = item136.cast$n_Region
)
stopifnot(sum(item136.table[which(item136.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(item136.table))], na.rm = T) == 10)


item136.final.SF <- item136.table[which(item136.table$BuildingType == "Single Family")
                                  ,-which(colnames(item136.table) %in% c("BuildingType"))]
item136.final.MH <- item136.table[which(item136.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item136.table) %in% c("BuildingType"))]

# exportTable(item136.final.SF, "SF", "Table 143", weighted = FALSE)
exportTable(item136.final.MH, "MH", "Table 118", weighted = FALSE)







#############################################################################################
#Item 137: DISTRIBUTION OF OIL USE AS HEATING FUEL BY STATE (SF table 144)
#############################################################################################
#subset to columns needed for analysis
item137.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseOtherFuel_QuantityOfFuelOil_Kerosene_Gallons"
                                                                                     ,""))])

colnames(item137.dat) <- c("CK_Cadmus_ID", "QTY")
item137.dat$QTY <- as.numeric(as.character(item137.dat$QTY))
#remove any repeat header rows from exporting
item137.dat0 <- item137.dat[which(item137.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item137.dat1 <- summarise(group_by(item137.dat0, CK_Cadmus_ID)
                          ,QTY = sum(QTY, na.rm = T))

item137.dat2 <- left_join(rbsa.dat, item137.dat1, by = "CK_Cadmus_ID")
item137.dat2$QTY[which(is.na(item137.dat2$QTY))] <- 0

item137.dat2$QTY <- as.numeric(as.character(item137.dat2$QTY))

item137.dat2$QTY_bins <- item137.dat2$QTY
item137.dat2$QTY_bins[which(item137.dat2$QTY == 0)] <- "None"
item137.dat2$QTY_bins[which(item137.dat2$QTY >  0   & item137.dat2$QTY < 100)] <- "< 100 Gallons"
item137.dat2$QTY_bins[which(item137.dat2$QTY >= 100 & item137.dat2$QTY < 251)] <- "100-250 Gallons"
item137.dat2$QTY_bins[which(item137.dat2$QTY >= 251 & item137.dat2$QTY < 501)] <- "251-500 Gallons"
item137.dat2$QTY_bins[which(item137.dat2$QTY >= 501 & item137.dat2$QTY < 1001)] <- "501-1000 Gallons"
item137.dat2$QTY_bins[which(item137.dat2$QTY > 1000)] <- "> 1000 Gallons"
unique(item137.dat2$QTY_bins)

################################################
# Adding pop and sample sizes for weights
################################################
item137.data <- weightedData(item137.dat2[-which(colnames(item137.dat2) %in% c("QTY"               
                                                                               ,"QTY_bins"))])
item137.data <- left_join(item137.data, item137.dat2[which(colnames(item137.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"QTY"               
                                                                                         ,"QTY_bins"))])
item137.data$count <- 1
#######################
# Weighted Analysis
#######################
item137.final <- proportionRowsAndColumns1(CustomerLevelData = item137.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'QTY_bins'
                                           ,aggregateColumnName = "Region")

item137.cast <- dcast(setDT(item137.final)
                      , formula = BuildingType + QTY_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item137.table <- data.frame("BuildingType"   = item137.cast$BuildingType
                            ,"Annual.Oil.Fuel.Use"= item137.cast$QTY_bins
                            ,"Percent_ID"     = item137.cast$w.percent_ID
                            ,"SE_ID"          = item137.cast$w.SE_ID
                            ,"n_ID"           = item137.cast$n_ID
                            ,"Percent_MT"     = item137.cast$w.percent_MT
                            ,"SE_MT"          = item137.cast$w.SE_MT
                            ,"n_MT"           = item137.cast$n_MT
                            ,"Percent_OR"     = item137.cast$w.percent_OR
                            ,"SE_OR"          = item137.cast$w.SE_OR
                            ,"n_OR"           = item137.cast$n_OR
                            ,"Percent_WA"     = item137.cast$w.percent_WA
                            ,"SE_WA"          = item137.cast$w.SE_WA
                            ,"n_WA"           = item137.cast$n_WA
                            ,"Percent_Region" = item137.cast$w.percent_Region
                            ,"SE_Region"      = item137.cast$w.SE_Region
                            ,"n_Region"       = item137.cast$n_Region
                            ,"EB_ID"          = item137.cast$EB_ID
                            ,"EB_MT"          = item137.cast$EB_MT
                            ,"EB_OR"          = item137.cast$EB_OR
                            ,"EB_WA"          = item137.cast$EB_WA
                            ,"EB_Region"      = item137.cast$EB_Region
)
#QAQC
stopifnot(sum(item137.table[which(item137.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(item137.table))], na.rm = T) == 10)

# row ordering example code
unique(item137.table$Annual.Oil.Fuel.Use)
rowOrder <- c("< 100 Gallons"
              ,"100-250 Gallons"
              ,"251-500 Gallons"
              ,"501-1000 Gallons"
              ,"> 1000 Gallons"
              ,"None"
              ,"Total")
item137.table <- item137.table %>% mutate(Annual.Oil.Fuel.Use = factor(Annual.Oil.Fuel.Use, levels = rowOrder)) %>% arrange(Annual.Oil.Fuel.Use)  
item137.table <- data.frame(item137.table)


item137.final.SF <- item137.table[which(item137.table$BuildingType == "Single Family")
                                  ,-which(colnames(item137.table) %in% c("BuildingType"))]
item137.final.MH <- item137.table[which(item137.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item137.table) %in% c("BuildingType"))]

# exportTable(item137.final.SF, "SF", "Table 144", weighted = TRUE)
exportTable(item137.final.MH, "MH", "Table 119", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item137.final <- proportions_two_groups_unweighted(CustomerLevelData = item137.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'QTY_bins'
                                                   ,aggregateColumnName = "Region")

item137.cast <- dcast(setDT(item137.final)
                      , formula = BuildingType + QTY_bins ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


item137.table <- data.frame("BuildingType"   = item137.cast$BuildingType
                            ,"Annual.Oil.Fuel.Use"= item137.cast$QTY_bins
                            ,"Percent_ID"     = item137.cast$Percent_ID
                            ,"SE_ID"          = item137.cast$SE_ID
                            ,"n_ID"           = item137.cast$n_ID
                            ,"Percent_MT"     = item137.cast$Percent_MT
                            ,"SE_MT"          = item137.cast$SE_MT
                            ,"n_MT"           = item137.cast$n_MT
                            ,"Percent_OR"     = item137.cast$Percent_OR
                            ,"SE_OR"          = item137.cast$SE_OR
                            ,"n_OR"           = item137.cast$n_OR
                            ,"Percent_WA"     = item137.cast$Percent_WA
                            ,"SE_WA"          = item137.cast$SE_WA
                            ,"n_WA"           = item137.cast$n_WA
                            ,"Percent_Region" = item137.cast$Percent_Region
                            ,"SE_Region"      = item137.cast$SE_Region
                            ,"n_Region"       = item137.cast$n_Region
)
stopifnot(sum(item137.table[which(item137.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(item137.table))], na.rm = T) == 10)

# row ordering example code
unique(item137.table$Annual.Oil.Fuel.Use)
rowOrder <- c("< 100 Gallons"
              ,"100-250 Gallons"
              ,"251-500 Gallons"
              ,"501-1000 Gallons"
              ,"> 1000 Gallons"
              ,"None"
              ,"Total")
item137.table <- item137.table %>% mutate(Annual.Oil.Fuel.Use = factor(Annual.Oil.Fuel.Use, levels = rowOrder)) %>% arrange(Annual.Oil.Fuel.Use)  
item137.table <- data.frame(item137.table)


item137.final.SF <- item137.table[which(item137.table$BuildingType == "Single Family")
                                  ,-which(colnames(item137.table) %in% c("BuildingType"))]
item137.final.MH <- item137.table[which(item137.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item137.table) %in% c("BuildingType"))]

# exportTable(item137.final.SF, "SF", "Table 144", weighted = FALSE)
exportTable(item137.final.MH, "MH", "Table 119", weighted = FALSE)





#############################################################################################
#Item 138: DISTRIBUTION OF PROPANE USE AS HEATING FUEL BY STATE (SF table 145)
#############################################################################################
#subset to columns needed for analysis
item138.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseOtherFuel_QuantityOfPropane_Gallons"
                                                                                     ,""))])

colnames(item138.dat) <- c("CK_Cadmus_ID", "QTY")
item138.dat$QTY <- as.numeric(as.character(item138.dat$QTY))
#remove any repeat header rows from exporting
item138.dat0 <- item138.dat[which(item138.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item138.dat1 <- summarise(group_by(item138.dat0, CK_Cadmus_ID)
                          ,QTY = sum(QTY, na.rm = T))

item138.dat2 <- left_join(rbsa.dat, item138.dat1, by = "CK_Cadmus_ID")
item138.dat2 <- item138.dat2[which(!is.na(item138.dat2$QTY)),]

item138.dat2$QTY_bins <- "None"
item138.dat2$QTY_bins[which(item138.dat2$QTY >  0   & item138.dat2$QTY < 50)] <- "< 50 Gallons"
item138.dat2$QTY_bins[which(item138.dat2$QTY >= 50  & item138.dat2$QTY < 251)] <- "50-250 Gallons"
item138.dat2$QTY_bins[which(item138.dat2$QTY >= 251 & item138.dat2$QTY < 501)] <- "251-500 Gallons"
item138.dat2$QTY_bins[which(item138.dat2$QTY >= 501 & item138.dat2$QTY < 1001)] <- "501-1000 Gallons"
item138.dat2$QTY_bins[which(item138.dat2$QTY > 1000)] <- "> 1000 Gallons"
unique(item138.dat2$QTY_bins)

################################################
# Adding pop and sample sizes for weights
################################################
item138.data <- weightedData(item138.dat2[-which(colnames(item138.dat2) %in% c("QTY"               
                                                                               ,"QTY_bins"))])
item138.data <- left_join(item138.data, item138.dat2[which(colnames(item138.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"QTY"               
                                                                                         ,"QTY_bins"))])
item138.data$count <- 1
#######################
# Weighted Analysis
#######################
item138.final <- proportionRowsAndColumns1(CustomerLevelData = item138.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'QTY_bins'
                                           ,aggregateColumnName = "Region")

item138.cast <- dcast(setDT(item138.final)
                      , formula = BuildingType + QTY_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item138.table <- data.frame("BuildingType"   = item138.cast$BuildingType
                            ,"Annual.Propane.Fuel.Use"= item138.cast$QTY_bins
                            ,"Percent_ID"     = item138.cast$w.percent_ID
                            ,"SE_ID"          = item138.cast$w.SE_ID
                            ,"n_ID"           = item138.cast$n_ID
                            ,"Percent_MT"     = item138.cast$w.percent_MT
                            ,"SE_MT"          = item138.cast$w.SE_MT
                            ,"n_MT"           = item138.cast$n_MT
                            ,"Percent_OR"     = item138.cast$w.percent_OR
                            ,"SE_OR"          = item138.cast$w.SE_OR
                            ,"n_OR"           = item138.cast$n_OR
                            ,"Percent_WA"     = item138.cast$w.percent_WA
                            ,"SE_WA"          = item138.cast$w.SE_WA
                            ,"n_WA"           = item138.cast$n_WA
                            ,"Percent_Region" = item138.cast$w.percent_Region
                            ,"SE_Region"      = item138.cast$w.SE_Region
                            ,"n_Region"       = item138.cast$n_Region
                            ,"EB_ID"          = item138.cast$EB_ID
                            ,"EB_MT"          = item138.cast$EB_MT
                            ,"EB_OR"          = item138.cast$EB_OR
                            ,"EB_WA"          = item138.cast$EB_WA
                            ,"EB_Region"      = item138.cast$EB_Region
)
#QAQC
stopifnot(sum(item138.table[which(item138.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(item138.table))], na.rm = T) == 10)

# row ordering example code
unique(item138.table$Annual.Propane.Fuel.Use)
rowOrder <- c("< 50 Gallons"
              ,"50-250 Gallons"
              ,"251-500 Gallons"
              ,"501-1000 Gallons"
              ,"> 1000 Gallons"
              ,"None"
              ,"Total")
item138.table <- item138.table %>% mutate(Annual.Propane.Fuel.Use = factor(Annual.Propane.Fuel.Use, levels = rowOrder)) %>% arrange(Annual.Propane.Fuel.Use)  
item138.table <- data.frame(item138.table)


item138.final.SF <- item138.table[which(item138.table$BuildingType == "Single Family")
                                  ,-which(colnames(item138.table) %in% c("BuildingType"))]
item138.final.MH <- item138.table[which(item138.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item138.table) %in% c("BuildingType"))]

# exportTable(item138.final.SF, "SF", "Table 145", weighted = TRUE)
exportTable(item138.final.MH, "MH", "Table 120", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item138.final <- proportions_two_groups_unweighted(CustomerLevelData = item138.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'QTY_bins'
                                                   ,aggregateColumnName = "Region")

item138.cast <- dcast(setDT(item138.final)
                      , formula = BuildingType + QTY_bins ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


item138.table <- data.frame("BuildingType"   = item138.cast$BuildingType
                            ,"Annual.Propane.Fuel.Use"= item138.cast$QTY_bins
                            ,"Percent_ID"     = item138.cast$Percent_ID
                            ,"SE_ID"          = item138.cast$SE_ID
                            ,"n_ID"           = item138.cast$n_ID
                            ,"Percent_MT"     = item138.cast$Percent_MT
                            ,"SE_MT"          = item138.cast$SE_MT
                            ,"n_MT"           = item138.cast$n_MT
                            ,"Percent_OR"     = item138.cast$Percent_OR
                            ,"SE_OR"          = item138.cast$SE_OR
                            ,"n_OR"           = item138.cast$n_OR
                            ,"Percent_WA"     = item138.cast$Percent_WA
                            ,"SE_WA"          = item138.cast$SE_WA
                            ,"n_WA"           = item138.cast$n_WA
                            ,"Percent_Region" = item138.cast$Percent_Region
                            ,"SE_Region"      = item138.cast$SE_Region
                            ,"n_Region"       = item138.cast$n_Region
)
stopifnot(sum(item138.table[which(item138.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(item138.table))], na.rm = T) == 10)


item138.final.SF <- item138.table[which(item138.table$BuildingType == "Single Family")
                                  ,-which(colnames(item138.table) %in% c("BuildingType"))]
item138.final.MH <- item138.table[which(item138.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item138.table) %in% c("BuildingType"))]

# exportTable(item138.final.SF, "SF", "Table 145", weighted = FALSE)
exportTable(item138.final.MH, "MH", "Table 120", weighted = FALSE)

































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
#Item 135: DISTRIBUTION OF WOOD USE AS HEATING FUEL BY CK_Building_ID (SF table 142, MH table 117)
#############################################################################################
#subset to columns needed for analysis
item135.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseOtherFuel_QuantityOfWood_Cords"
                                                                                     ,""))])

colnames(item135.os.dat) <- c("CK_Cadmus_ID", "QTY")
item135.os.dat$QTY <- as.numeric(as.character(item135.os.dat$QTY))
#remove any repeat header rows from exporting
item135.os.dat0 <- item135.os.dat[which(item135.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item135.os.dat1 <- summarise(group_by(item135.os.dat0, CK_Cadmus_ID)
                          ,QTY = sum(QTY, na.rm = T))

item135.os.dat2 <- left_join(scl.dat, item135.os.dat1, by = "CK_Cadmus_ID")
item135.os.dat2 <- item135.os.dat2[which(!is.na(item135.os.dat2$QTY)),]

unique(item135.os.dat2$QTY)
item135.os.dat2$QTY_bins <- as.numeric(as.character(item135.os.dat2$QTY))
item135.os.dat2$QTY_bins[which(item135.os.dat2$QTY == 0)] <- "None"
item135.os.dat2$QTY_bins[which(item135.os.dat2$QTY > 0  & item135.os.dat2$QTY< 1)] <- "< 1 Cord"
item135.os.dat2$QTY_bins[which(item135.os.dat2$QTY >= 1 & item135.os.dat2$QTY < 4)] <- "1-3 Cords"
item135.os.dat2$QTY_bins[which(item135.os.dat2$QTY >= 4 & item135.os.dat2$QTY < 7)] <- "4-6 Cords"
item135.os.dat2$QTY_bins[which(item135.os.dat2$QTY > 6)] <- "> 6 Cords"


################################################
# Adding pop and sample sizes for weights
################################################
item135.os.data <- weightedData(item135.os.dat2[-which(colnames(item135.os.dat2) %in% c("QTY"               
                                                                               ,"QTY_bins"))])
item135.os.data <- left_join(item135.os.data, item135.os.dat2[which(colnames(item135.os.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"QTY"               
                                                                                         ,"QTY_bins"))])
item135.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item135.os.final <- proportionRowsAndColumns1(CustomerLevelData = item135.os.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'CK_Building_ID'
                                           ,rowVariable      = 'QTY_bins'
                                           ,aggregateColumnName = "Remove")

item135.os.cast <- dcast(setDT(item135.os.final)
                      , formula = BuildingType + QTY_bins ~ CK_Building_ID
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item135.os.table <- data.frame("BuildingType"   = item135.os.cast$BuildingType
                            ,"Annual.Wood.Use"= item135.os.cast$QTY_bins
                            ,"Percent_SCL.GenPop"   = item135.os.cast$`w.percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = item135.os.cast$`w.SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = item135.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = item135.os.cast$`w.percent_SCL LI`
                            ,"SE_SCL.LI"            = item135.os.cast$`w.SE_SCL LI`
                            ,"n_SCL.LI"             = item135.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = item135.os.cast$`w.percent_SCL EH`
                            ,"SE_SCL.EH"            = item135.os.cast$`w.SE_SCL EH`
                            ,"n_SCL.EH"             = item135.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = item135.os.cast$`w.percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = item135.os.cast$`w.SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = item135.os.cast$`n_2017 RBSA PS`
                            ,"EB_SCL.GenPop"        = item135.os.cast$`EB_SCL GenPop`
                            ,"EB_SCL.LI"            = item135.os.cast$`EB_SCL LI`
                            ,"EB_SCL.EH"            = item135.os.cast$`EB_SCL EH`
                            ,"EB_2017.RBSA.PS"      = item135.os.cast$`EB_2017 RBSA PS`
)

item135.os.final.SF <- item135.os.table[which(item135.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item135.os.table) %in% c("BuildingType"))]

exportTable(item135.os.final.SF, "SF", "Table 142", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item135.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item135.os.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'CK_Building_ID'
                                                   ,rowVariable      = 'QTY_bins'
                                                   ,aggregateColumnName = "Remove")

item135.os.cast <- dcast(setDT(item135.os.final)
                      , formula = BuildingType + QTY_bins ~ CK_Building_ID
                      , value.var = c("Percent", "SE", "Count", "n"))


item135.os.table <- data.frame("BuildingType"   = item135.os.cast$BuildingType
                            ,"Annual.Wood.Use"= item135.os.cast$QTY_bins
                            ,"Percent_SCL.GenPop"   = item135.os.cast$`Percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = item135.os.cast$`SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = item135.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = item135.os.cast$`Percent_SCL LI`
                            ,"SE_SCL.LI"            = item135.os.cast$`SE_SCL LI`
                            ,"n_SCL.LI"             = item135.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = item135.os.cast$`Percent_SCL EH`
                            ,"SE_SCL.EH"            = item135.os.cast$`SE_SCL EH`
                            ,"n_SCL.EH"             = item135.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = item135.os.cast$`Percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = item135.os.cast$`SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = item135.os.cast$`n_2017 RBSA PS`
                            
)

item135.os.final.SF <- item135.os.table[which(item135.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item135.os.table) %in% c("BuildingType"))]

exportTable(item135.os.final.SF, "SF", "Table 142", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Item 136: DISTRIBUTION OF PELLET USE AS HEATING FUEL BY CK_Building_ID (SF table 143, MH table 118)
#############################################################################################
#subset to columns needed for analysis
item136.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseOtherFuel_QuantityOfPellets_Tons"
                                                                                     ,""))])

colnames(item136.os.dat) <- c("CK_Cadmus_ID", "QTY")
item136.os.dat$QTY <- as.numeric(as.character(item136.os.dat$QTY))
#remove any repeat header rows from exporting
item136.os.dat0 <- item136.os.dat[which(item136.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item136.os.dat1 <- summarise(group_by(item136.os.dat0, CK_Cadmus_ID)
                          ,QTY = sum(QTY, na.rm = T))

item136.os.dat2 <- left_join(scl.dat, item136.os.dat1, by = "CK_Cadmus_ID")
item136.os.dat2 <- item136.os.dat2[which(!is.na(item136.os.dat2$QTY)),]

item136.os.dat2$QTY_bins <- item136.os.dat2$QTY
item136.os.dat2$QTY_bins[which(item136.os.dat2$QTY == 0)] <- "None"
item136.os.dat2$QTY_bins[which(item136.os.dat2$QTY >  0 & item136.os.dat2$QTY < 1)] <- "< 1 Ton"
item136.os.dat2$QTY_bins[which(item136.os.dat2$QTY >= 1 & item136.os.dat2$QTY < 3)] <- "1-2 Tons"
item136.os.dat2$QTY_bins[which(item136.os.dat2$QTY >= 2 & item136.os.dat2$QTY < 5)] <- "2-4 Tons"
item136.os.dat2$QTY_bins[which(item136.os.dat2$QTY > 4)] <- "> 4 Tons"


################################################
# Adding pop and sample sizes for weights
################################################
item136.os.data <- weightedData(item136.os.dat2[-which(colnames(item136.os.dat2) %in% c("QTY"               
                                                                               ,"QTY_bins"))])
item136.os.data <- left_join(item136.os.data, unique(item136.os.dat2[which(colnames(item136.os.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"QTY"               
                                                                                         ,"QTY_bins"))]))
item136.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item136.os.final <- proportionRowsAndColumns1(CustomerLevelData = item136.os.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'CK_Building_ID'
                                           ,rowVariable      = 'QTY_bins'
                                           ,aggregateColumnName = "Remove")

item136.os.cast <- dcast(setDT(item136.os.final)
                      , formula = BuildingType + QTY_bins ~ CK_Building_ID
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item136.os.table <- data.frame("BuildingType"   = item136.os.cast$BuildingType
                            ,"Annual.Wood.Use"= item136.os.cast$QTY_bins
                            ,"Percent_SCL.GenPop"   = item136.os.cast$`w.percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = item136.os.cast$`w.SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = item136.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = item136.os.cast$`w.percent_SCL LI`
                            ,"SE_SCL.LI"            = item136.os.cast$`w.SE_SCL LI`
                            ,"n_SCL.LI"             = item136.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = item136.os.cast$`w.percent_SCL EH`
                            ,"SE_SCL.EH"            = item136.os.cast$`w.SE_SCL EH`
                            ,"n_SCL.EH"             = item136.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = item136.os.cast$`w.percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = item136.os.cast$`w.SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = item136.os.cast$`n_2017 RBSA PS`
                            ,"EB_SCL.GenPop"        = item136.os.cast$`EB_SCL GenPop`
                            ,"EB_SCL.LI"            = item136.os.cast$`EB_SCL LI`
                            ,"EB_SCL.EH"            = item136.os.cast$`EB_SCL EH`
                            ,"EB_2017.RBSA.PS"      = item136.os.cast$`EB_2017 RBSA PS`
)

item136.os.final.SF <- item136.os.table[which(item136.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item136.os.table) %in% c("BuildingType"))]

exportTable(item136.os.final.SF, "SF", "Table 143", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item136.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item136.os.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'CK_Building_ID'
                                                   ,rowVariable      = 'QTY_bins'
                                                   ,aggregateColumnName = "Remove")

item136.os.cast <- dcast(setDT(item136.os.final)
                      , formula = BuildingType + QTY_bins ~ CK_Building_ID
                      , value.var = c("Percent", "SE", "Count", "n"))


item136.os.table <- data.frame("BuildingType"   = item136.os.cast$BuildingType
                            ,"Annual.Wood.Use"= item136.os.cast$QTY_bins
                            ,"Percent_SCL.GenPop"   = item136.os.cast$`Percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = item136.os.cast$`SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = item136.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = item136.os.cast$`Percent_SCL LI`
                            ,"SE_SCL.LI"            = item136.os.cast$`SE_SCL LI`
                            ,"n_SCL.LI"             = item136.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = item136.os.cast$`Percent_SCL EH`
                            ,"SE_SCL.EH"            = item136.os.cast$`SE_SCL EH`
                            ,"n_SCL.EH"             = item136.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = item136.os.cast$`Percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = item136.os.cast$`SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = item136.os.cast$`n_2017 RBSA PS`
)
item136.os.final.SF <- item136.os.table[which(item136.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item136.os.table) %in% c("BuildingType"))]

exportTable(item136.os.final.SF, "SF", "Table 143", weighted = FALSE, osIndicator = "SCL", OS = T)





#############################################################################################
#Item 137: DISTRIBUTION OF OIL USE AS HEATING FUEL BY CK_Building_ID (SF table 144)
#############################################################################################
#subset to columns needed for analysis
item137.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseOtherFuel_QuantityOfFuelOil_Kerosene_Gallons"
                                                                                     ,""))])

colnames(item137.os.dat) <- c("CK_Cadmus_ID", "QTY")
item137.os.dat$QTY <- as.numeric(as.character(item137.os.dat$QTY))
#remove any repeat header rows from exporting
item137.os.dat0 <- item137.os.dat[which(item137.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item137.os.dat1 <- summarise(group_by(item137.os.dat0, CK_Cadmus_ID)
                          ,QTY = sum(QTY, na.rm = T))

item137.os.dat2 <- left_join(scl.dat, item137.os.dat1, by = "CK_Cadmus_ID")
item137.os.dat2$QTY[which(is.na(item137.os.dat2$QTY))] <- 0

item137.os.dat2$QTY <- as.numeric(as.character(item137.os.dat2$QTY))

item137.os.dat2$QTY_bins <- item137.os.dat2$QTY
item137.os.dat2$QTY_bins[which(item137.os.dat2$QTY == 0)] <- "None"
item137.os.dat2$QTY_bins[which(item137.os.dat2$QTY >  0   & item137.os.dat2$QTY < 100)] <- "< 100 Gallons"
item137.os.dat2$QTY_bins[which(item137.os.dat2$QTY >= 100 & item137.os.dat2$QTY < 251)] <- "100-250 Gallons"
item137.os.dat2$QTY_bins[which(item137.os.dat2$QTY >= 251 & item137.os.dat2$QTY < 501)] <- "251-500 Gallons"
item137.os.dat2$QTY_bins[which(item137.os.dat2$QTY >= 501 & item137.os.dat2$QTY < 1001)] <- "501-1000 Gallons"
item137.os.dat2$QTY_bins[which(item137.os.dat2$QTY > 1000)] <- "> 1000 Gallons"
unique(item137.os.dat2$QTY_bins)

################################################
# Adding pop and sample sizes for weights
################################################
item137.os.data <- weightedData(item137.os.dat2[-which(colnames(item137.os.dat2) %in% c("QTY"               
                                                                               ,"QTY_bins"))])
item137.os.data <- left_join(item137.os.data, unique(item137.os.dat2[which(colnames(item137.os.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"QTY"               
                                                                                         ,"QTY_bins"))]))
item137.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item137.os.final <- proportionRowsAndColumns1(CustomerLevelData = item137.os.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'CK_Building_ID'
                                           ,rowVariable      = 'QTY_bins'
                                           ,aggregateColumnName = "Remove")

item137.os.cast <- dcast(setDT(item137.os.final)
                      , formula = BuildingType + QTY_bins ~ CK_Building_ID
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item137.os.table <- data.frame("BuildingType"   = item137.os.cast$BuildingType
                            ,"Annual.Oil.Fuel.Use"= item137.os.cast$QTY_bins
                            ,"Percent_SCL.GenPop"   = item137.os.cast$`w.percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = item137.os.cast$`w.SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = item137.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = item137.os.cast$`w.percent_SCL LI`
                            ,"SE_SCL.LI"            = item137.os.cast$`w.SE_SCL LI`
                            ,"n_SCL.LI"             = item137.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = item137.os.cast$`w.percent_SCL EH`
                            ,"SE_SCL.EH"            = item137.os.cast$`w.SE_SCL EH`
                            ,"n_SCL.EH"             = item137.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = item137.os.cast$`w.percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = item137.os.cast$`w.SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = item137.os.cast$`n_2017 RBSA PS`
                            ,"EB_SCL.GenPop"        = item137.os.cast$`EB_SCL GenPop`
                            ,"EB_SCL.LI"            = item137.os.cast$`EB_SCL LI`
                            ,"EB_SCL.EH"            = item137.os.cast$`EB_SCL EH`
                            ,"EB_2017.RBSA.PS"      = item137.os.cast$`EB_2017 RBSA PS`
)

# row ordering example code
unique(item137.os.table$Annual.Oil.Fuel.Use)
rowOrder <- c("< 100 Gallons"
              ,"100-250 Gallons"
              ,"251-500 Gallons"
              ,"501-1000 Gallons"
              ,"> 1000 Gallons"
              ,"None"
              ,"Total")
item137.os.table <- item137.os.table %>% mutate(Annual.Oil.Fuel.Use = factor(Annual.Oil.Fuel.Use, levels = rowOrder)) %>% arrange(Annual.Oil.Fuel.Use)  
item137.os.table <- data.frame(item137.os.table)

item137.os.final.SF <- item137.os.table[which(item137.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item137.os.table) %in% c("BuildingType"))]

exportTable(item137.os.final.SF, "SF", "Table 144", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item137.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item137.os.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'CK_Building_ID'
                                                   ,rowVariable      = 'QTY_bins'
                                                   ,aggregateColumnName = "Remove")

item137.os.cast <- dcast(setDT(item137.os.final)
                      , formula = BuildingType + QTY_bins ~ CK_Building_ID
                      , value.var = c("Percent", "SE", "Count", "n"))


item137.os.table <- data.frame("BuildingType"   = item137.os.cast$BuildingType
                            ,"Annual.Oil.Fuel.Use"= item137.os.cast$QTY_bins
                            ,"Percent_SCL.GenPop"   = item137.os.cast$`Percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = item137.os.cast$`SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = item137.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = item137.os.cast$`Percent_SCL LI`
                            ,"SE_SCL.LI"            = item137.os.cast$`SE_SCL LI`
                            ,"n_SCL.LI"             = item137.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = item137.os.cast$`Percent_SCL EH`
                            ,"SE_SCL.EH"            = item137.os.cast$`SE_SCL EH`
                            ,"n_SCL.EH"             = item137.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = item137.os.cast$`Percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = item137.os.cast$`SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = item137.os.cast$`n_2017 RBSA PS`
)

# row ordering example code
unique(item137.os.table$Annual.Oil.Fuel.Use)
rowOrder <- c("< 100 Gallons"
              ,"100-250 Gallons"
              ,"251-500 Gallons"
              ,"501-1000 Gallons"
              ,"> 1000 Gallons"
              ,"None"
              ,"Total")
item137.os.table <- item137.os.table %>% mutate(Annual.Oil.Fuel.Use = factor(Annual.Oil.Fuel.Use, levels = rowOrder)) %>% arrange(Annual.Oil.Fuel.Use)  
item137.os.table <- data.frame(item137.os.table)

item137.os.final.SF <- item137.os.table[which(item137.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item137.os.table) %in% c("BuildingType"))]

exportTable(item137.os.final.SF, "SF", "Table 144", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Item 138: DISTRIBUTION OF PROPANE USE AS HEATING FUEL BY CK_Building_ID (SF table 145)
#############################################################################################
#subset to columns needed for analysis
item138.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseOtherFuel_QuantityOfPropane_Gallons"
                                                                                     ,""))])

colnames(item138.os.dat) <- c("CK_Cadmus_ID", "QTY")
item138.os.dat$QTY <- as.numeric(as.character(item138.os.dat$QTY))
#remove any repeat header rows from exporting
item138.os.dat0 <- item138.os.dat[which(item138.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item138.os.dat1 <- summarise(group_by(item138.os.dat0, CK_Cadmus_ID)
                          ,QTY = sum(QTY, na.rm = T))

item138.os.dat2 <- left_join(scl.dat, item138.os.dat1, by = "CK_Cadmus_ID")
item138.os.dat2 <- item138.os.dat2[which(!is.na(item138.os.dat2$QTY)),]

item138.os.dat2$QTY_bins <- "None"
item138.os.dat2$QTY_bins[which(item138.os.dat2$QTY >  0   & item138.os.dat2$QTY < 50)] <- "< 50 Gallons"
item138.os.dat2$QTY_bins[which(item138.os.dat2$QTY >= 50  & item138.os.dat2$QTY < 251)] <- "50-250 Gallons"
item138.os.dat2$QTY_bins[which(item138.os.dat2$QTY >= 251 & item138.os.dat2$QTY < 501)] <- "251-500 Gallons"
item138.os.dat2$QTY_bins[which(item138.os.dat2$QTY >= 501 & item138.os.dat2$QTY < 1001)] <- "501-1000 Gallons"
item138.os.dat2$QTY_bins[which(item138.os.dat2$QTY > 1000)] <- "> 1000 Gallons"
unique(item138.os.dat2$QTY_bins)

################################################
# Adding pop and sample sizes for weights
################################################
item138.os.data <- weightedData(item138.os.dat2[-which(colnames(item138.os.dat2) %in% c("QTY"               
                                                                               ,"QTY_bins"))])
item138.os.data <- left_join(item138.os.data, unique(item138.os.dat2[which(colnames(item138.os.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"QTY"               
                                                                                         ,"QTY_bins"))]))
item138.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item138.os.final <- proportionRowsAndColumns1(CustomerLevelData = item138.os.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'CK_Building_ID'
                                           ,rowVariable      = 'QTY_bins'
                                           ,aggregateColumnName = "Remove")

item138.os.cast <- dcast(setDT(item138.os.final)
                      , formula = BuildingType + QTY_bins ~ CK_Building_ID
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item138.os.table <- data.frame("BuildingType"   = item138.os.cast$BuildingType
                            ,"Annual.Propane.Fuel.Use"= item138.os.cast$QTY_bins
                            ,"Percent_SCL.GenPop"   = item138.os.cast$`w.percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = item138.os.cast$`w.SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = item138.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = item138.os.cast$`w.percent_SCL LI`
                            ,"SE_SCL.LI"            = item138.os.cast$`w.SE_SCL LI`
                            ,"n_SCL.LI"             = item138.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = item138.os.cast$`w.percent_SCL EH`
                            ,"SE_SCL.EH"            = item138.os.cast$`w.SE_SCL EH`
                            ,"n_SCL.EH"             = item138.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = item138.os.cast$`w.percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = item138.os.cast$`w.SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = item138.os.cast$`n_2017 RBSA PS`
                            ,"EB_SCL.GenPop"        = item138.os.cast$`EB_SCL GenPop`
                            ,"EB_SCL.LI"            = item138.os.cast$`EB_SCL LI`
                            ,"EB_SCL.EH"            = item138.os.cast$`EB_SCL EH`
                            ,"EB_2017.RBSA.PS"      = item138.os.cast$`EB_2017 RBSA PS`
)

# row ordering example code
unique(item138.os.table$Annual.Propane.Fuel.Use)
rowOrder <- c("< 50 Gallons"
              ,"50-250 Gallons"
              ,"251-500 Gallons"
              ,"501-1000 Gallons"
              ,"> 1000 Gallons"
              ,"None"
              ,"Total")
item138.os.table <- item138.os.table %>% mutate(Annual.Propane.Fuel.Use = factor(Annual.Propane.Fuel.Use, levels = rowOrder)) %>% arrange(Annual.Propane.Fuel.Use)  
item138.os.table <- data.frame(item138.os.table)

item138.os.final.SF <- item138.os.table[which(item138.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item138.os.table) %in% c("BuildingType"))]

exportTable(item138.os.final.SF, "SF", "Table 145", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item138.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item138.os.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'CK_Building_ID'
                                                   ,rowVariable      = 'QTY_bins'
                                                   ,aggregateColumnName = "Remove")

item138.os.cast <- dcast(setDT(item138.os.final)
                      , formula = BuildingType + QTY_bins ~ CK_Building_ID
                      , value.var = c("Percent", "SE", "Count", "n"))


item138.os.table <- data.frame("BuildingType"   = item138.os.cast$BuildingType
                            ,"Annual.Propane.Fuel.Use"= item138.os.cast$QTY_bins
                            ,"Percent_SCL.GenPop"   = item138.os.cast$`Percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = item138.os.cast$`SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = item138.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = item138.os.cast$`Percent_SCL LI`
                            ,"SE_SCL.LI"            = item138.os.cast$`SE_SCL LI`
                            ,"n_SCL.LI"             = item138.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = item138.os.cast$`Percent_SCL EH`
                            ,"SE_SCL.EH"            = item138.os.cast$`SE_SCL EH`
                            ,"n_SCL.EH"             = item138.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = item138.os.cast$`Percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = item138.os.cast$`SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = item138.os.cast$`n_2017 RBSA PS`
)
item138.os.final.SF <- item138.os.table[which(item138.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item138.os.table) %in% c("BuildingType"))]

exportTable(item138.os.final.SF, "SF", "Table 145", weighted = FALSE, osIndicator = "SCL", OS = T)
