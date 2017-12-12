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
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
#Item 96: DISTRIBUTION OF WATER HEATER FUEL BY STATE (SF table 103, MH table 84)
#############################################################################################
#subset to columns needed for analysis
item96.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,""))]
item96.dat$count <- 1

item96.dat0 <- item96.dat[which(item96.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item96.dat1 <- left_join(rbsa.dat, item96.dat0, by = "CK_Cadmus_ID")

unique(item96.dat1$DHW.Fuel)
item96.dat2 <- item96.dat1[which(item96.dat1$DHW.Fuel %in% c("Electric"
                                                             , "Natural Gas"
                                                             , "Propane")),]

################################################
# Adding pop and sample sizes for weights
################################################
item96.data <- weightedData(item96.dat2[-which(colnames(item96.dat2) %in% c("Generic"               
                                                                              ,"DHW.Fuel"
                                                                              ,"count"))])
item96.data <- left_join(item96.data, item96.dat2[which(colnames(item96.dat2) %in% c("CK_Cadmus_ID"
                                                                                       ,"Generic"               
                                                                                       ,"DHW.Fuel"
                                                                                       ,"count"))])
#######################
# Weighted Analysis
#######################
item96.final <- proportionRowsAndColumns1(CustomerLevelData = item96.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'State'
                                          ,rowVariable      = 'DHW.Fuel'
                                          ,aggregateColumnName = "Region")

item96.cast <- dcast(setDT(item96.final)
                     , formula = BuildingType + DHW.Fuel ~ State
                     , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item96.table <- data.frame("BuildingType"       = item96.cast$BuildingType
                           ,"Water.Heater.Fuel" = item96.cast$DHW.Fuel
                           ,"Percent_ID"        = item96.cast$w.percent_ID
                           ,"SE_ID"             = item96.cast$w.SE_ID
                           ,"n_ID"              = item96.cast$n_ID
                           ,"Percent_MT"        = item96.cast$w.percent_MT
                           ,"SE_MT"             = item96.cast$w.SE_MT
                           ,"n_MT"              = item96.cast$n_MT
                           ,"Percent_OR"        = item96.cast$w.percent_OR
                           ,"SE_OR"             = item96.cast$w.SE_OR
                           ,"n_OR"              = item96.cast$n_OR
                           ,"Percent_WA"        = item96.cast$w.percent_WA
                           ,"SE_WA"             = item96.cast$w.SE_WA
                           ,"n_WA"              = item96.cast$n_WA
                           ,"Percent_Region"    = item96.cast$w.percent_Region
                           ,"SE_Region"         = item96.cast$w.SE_Region
                           ,"n_Region"          = item96.cast$n_Region
)
    #QAQC
    stopifnot(sum(item96.table[which(item96.table$BuildingType == "Single Family")
                               ,grep("Percent",colnames(item96.table))], na.rm = T) == 10)


item96.final.SF <- item96.table[which(item96.table$BuildingType == "Single Family")
                                ,-which(colnames(item96.table) %in% c("BuildingType"))]
item96.final.MH <- item96.table[which(item96.table$BuildingType == "Manufactured")
                                ,-which(colnames(item96.table) %in% c("BuildingType"))]

exportTable(item96.final.SF, "SF", "Table 103", weighted = TRUE)
exportTable(item96.final.MH, "MH", "Table 84", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item96.final <- proportions_two_groups_unweighted(CustomerLevelData = item96.data
                                                  ,valueVariable    = 'count'
                                                  ,columnVariable   = 'State'
                                                  ,rowVariable      = 'DHW.Fuel'
                                                  ,aggregateColumnName = "Region")

item96.cast <- dcast(setDT(item96.final)
                     , formula = BuildingType + DHW.Fuel ~ State
                     , value.var = c("Percent", "SE", "Count", "n"))


item96.table <- data.frame("BuildingType"       = item96.cast$BuildingType
                           ,"Water.Heater.Fuel" = item96.cast$DHW.Fuel
                           ,"Percent_ID"        = item96.cast$Percent_ID
                           ,"SE_ID"             = item96.cast$SE_ID
                           ,"n_ID"              = item96.cast$n_ID
                           ,"Percent_MT"        = item96.cast$Percent_MT
                           ,"SE_MT"             = item96.cast$SE_MT
                           ,"n_MT"              = item96.cast$n_MT
                           ,"Percent_OR"        = item96.cast$Percent_OR
                           ,"SE_OR"             = item96.cast$SE_OR
                           ,"n_OR"              = item96.cast$n_OR
                           ,"Percent_WA"        = item96.cast$Percent_WA
                           ,"SE_WA"             = item96.cast$SE_WA
                           ,"n_WA"              = item96.cast$n_WA
                           ,"Percent_Region"    = item96.cast$Percent_Region
                           ,"SE_Region"         = item96.cast$SE_Region
                           ,"n_Region"          = item96.cast$n_Region
)
stopifnot(sum(item96.table[which(item96.table$BuildingType == "Single Family")
                           ,grep("Percent",colnames(item96.table))], na.rm = T) == 10)


item96.final.SF <- item96.table[which(item96.table$BuildingType == "Single Family")
                                ,-which(colnames(item96.table) %in% c("BuildingType"))]
item96.final.MH <- item96.table[which(item96.table$BuildingType == "Manufactured")
                                ,-which(colnames(item96.table) %in% c("BuildingType"))]

exportTable(item96.final.SF, "SF", "Table 103", weighted = FALSE)
exportTable(item96.final.MH, "MH", "Table 84", weighted = FALSE)






#############################################################################################
#Item 97: DISTRIBUTION OF WATER HEATERS BY TYPE (SF table 104)
#############################################################################################
#subset to columns needed for analysis
item97.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,""))]
item97.dat$count <- 1

item97.dat0 <- item97.dat[which(item97.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item97.dat1 <- left_join(rbsa.dat, item97.dat0, by = "CK_Cadmus_ID")

item97.dat2 <- item97.dat1[grep("Water Heater",item97.dat1$Generic),]


################################################
# Adding pop and sample sizes for weights
################################################
item97.data <- weightedData(item97.dat2[-which(colnames(item97.dat2) %in% c("Generic"
                                                                            ,"DHW.Fuel"
                                                                            ,"count"))])
item97.data <- left_join(item97.data, item97.dat2[which(colnames(item97.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"DHW.Fuel"
                                                                                     ,"count"))])

#######################
# Weighted Analysis
#######################
item97.final <- proportions_one_group(CustomerLevelData  = item97.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Generic'
                                      , total.name       = "Total"
                                      , columnName       = "Homes with Ducts")

# Export table
item97.final.SF <- item97.final[which(item97.final$BuildingType == "Single Family")
                                ,-which(colnames(item97.final) %in% c("BuildingType"
                                                                      ,"Homes.with.Ducts"))]

exportTable(item97.final.SF, "SF", "Table 104", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item97.final <- proportions_one_group(CustomerLevelData  = item97.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Generic'
                                      , total.name       = "Total"
                                      , columnName       = "Homes with Ducts"
                                      , weighted = FALSE)

# Export table
item97.final.SF <- item97.final[which(item97.final$BuildingType == "Single Family")
                                ,-which(colnames(item97.final) %in% c("BuildingType"
                                                                      ,"Homes.with.Ducts"
                                                                      ,"Total.Count"))]

exportTable(item97.final.SF, "SF", "Table 104", weighted = FALSE)






#############################################################################################
#Item 98: DISTRIBUTION OF WATER HEATER LOCATION BY STATE (SF table 105, MH table 85)
#############################################################################################
#subset to columns needed for analysis
item98.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,"DHW.Location"))]
item98.dat$count <- 1

item98.dat0 <- item98.dat[which(item98.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item98.dat1 <- left_join(rbsa.dat, item98.dat0, by = "CK_Cadmus_ID")

item98.dat2 <- item98.dat1[grep("Water Heater",item98.dat1$Generic),]

item98.dat3 <- item98.dat2[which(item98.dat2$DHW.Location != "Unknown"),]

item98.dat3$DHW.Location[grep("Crawl",item98.dat3$DHW.Location)] <- "Crawlspace"
item98.dat3$DHW.Location[grep("In building",item98.dat3$DHW.Location)] <- "Main House"

item98.dat3$DHW.Location[which(item98.dat3$DHW.Location %notin% c("Crawlspace"
                                                                  ,"Basement"
                                                                  ,"Garage"
                                                                  ,"Main House"))] <- "Other"

unique(item98.dat3$DHW.Location)


################################################
# Adding pop and sample sizes for weights
################################################
item98.data <- weightedData(item98.dat3[-which(colnames(item98.dat3) %in% c("Generic"               
                                                                            ,"DHW.Fuel"
                                                                            ,"DHW.Location"
                                                                            ,"count"))])
item98.data <- left_join(item98.data, item98.dat3[which(colnames(item98.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"               
                                                                                     ,"DHW.Fuel"
                                                                                     ,"DHW.Location"
                                                                                     ,"count"))])
#######################
# Weighted Analysis
#######################
item98.final <- proportionRowsAndColumns1(CustomerLevelData = item98.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'State'
                                          ,rowVariable      = 'DHW.Location'
                                          ,aggregateColumnName = "Region")

item98.cast <- dcast(setDT(item98.final)
                     , formula = BuildingType + DHW.Location ~ State
                     , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item98.table <- data.frame("BuildingType"           = item98.cast$BuildingType
                           ,"Water.Heater.Location" = item98.cast$DHW.Location
                           ,"Percent_ID"            = item98.cast$w.percent_ID
                           ,"SE_ID"                 = item98.cast$w.SE_ID
                           ,"n_ID"                  = item98.cast$n_ID
                           ,"Percent_MT"            = item98.cast$w.percent_MT
                           ,"SE_MT"                 = item98.cast$w.SE_MT
                           ,"n_MT"                  = item98.cast$n_MT
                           ,"Percent_OR"            = item98.cast$w.percent_OR
                           ,"SE_OR"                 = item98.cast$w.SE_OR
                           ,"n_OR"                  = item98.cast$n_OR
                           ,"Percent_WA"            = item98.cast$w.percent_WA
                           ,"SE_WA"                 = item98.cast$w.SE_WA
                           ,"n_WA"                  = item98.cast$n_WA
                           ,"Percent_Region"        = item98.cast$w.percent_Region
                           ,"SE_Region"             = item98.cast$w.SE_Region
                           ,"n_Region"              = item98.cast$n_Region
)
#QAQC
stopifnot(sum(item98.table[which(item98.table$BuildingType == "Single Family")
                           ,grep("Percent",colnames(item98.table))], na.rm = T) == 10)


item98.final.SF <- item98.table[which(item98.table$BuildingType == "Single Family")
                                ,-which(colnames(item98.table) %in% c("BuildingType"))]
item98.final.MH <- item98.table[which(item98.table$BuildingType == "Manufactured")
                                ,-which(colnames(item98.table) %in% c("BuildingType"))]

exportTable(item98.final.SF, "SF", "Table 105", weighted = TRUE)
exportTable(item98.final.MH, "MH", "Table 85", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item98.final <- proportions_two_groups_unweighted(CustomerLevelData = item98.data
                                                  ,valueVariable    = 'count'
                                                  ,columnVariable   = 'State'
                                                  ,rowVariable      = 'DHW.Location'
                                                  ,aggregateColumnName = "Region")

item98.cast <- dcast(setDT(item98.final)
                     , formula = BuildingType + DHW.Location ~ State
                     , value.var = c("Percent", "SE", "Count", "n"))


item98.table <- data.frame("BuildingType"           = item98.cast$BuildingType
                           ,"Water.Heater.Location" = item98.cast$DHW.Location
                           ,"Percent_ID"            = item98.cast$Percent_ID
                           ,"SE_ID"                 = item98.cast$SE_ID
                           ,"n_ID"                  = item98.cast$n_ID
                           ,"Percent_MT"            = item98.cast$Percent_MT
                           ,"SE_MT"                 = item98.cast$SE_MT
                           ,"n_MT"                  = item98.cast$n_MT
                           ,"Percent_OR"            = item98.cast$Percent_OR
                           ,"SE_OR"                 = item98.cast$SE_OR
                           ,"n_OR"                  = item98.cast$n_OR
                           ,"Percent_WA"            = item98.cast$Percent_WA
                           ,"SE_WA"                 = item98.cast$SE_WA
                           ,"n_WA"                  = item98.cast$n_WA
                           ,"Percent_Region"        = item98.cast$Percent_Region
                           ,"SE_Region"             = item98.cast$SE_Region
                           ,"n_Region"              = item98.cast$n_Region
)
stopifnot(sum(item98.table[which(item98.table$BuildingType == "Single Family")
                           ,grep("Percent",colnames(item98.table))], na.rm = T) == 10)


item98.final.SF <- item98.table[which(item98.table$BuildingType == "Single Family")
                                ,-which(colnames(item98.table) %in% c("BuildingType"))]
item98.final.MH <- item98.table[which(item98.table$BuildingType == "Manufactured")
                                ,-which(colnames(item98.table) %in% c("BuildingType"))]

exportTable(item98.final.SF, "SF", "Table 105", weighted = FALSE)
exportTable(item98.final.MH, "MH", "Table 85", weighted = FALSE)

