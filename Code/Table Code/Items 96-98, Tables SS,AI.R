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
download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx', mechanical.export, mode = 'wb')
mechanical.dat <- read.xlsx(mechanical.export)
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
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

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
                           ,"EB_ID"             = item96.cast$EB_ID
                           ,"EB_MT"             = item96.cast$EB_MT
                           ,"EB_OR"             = item96.cast$EB_OR
                           ,"EB_WA"             = item96.cast$EB_WA
                           ,"EB_Region"         = item96.cast$EB_Region
)
    #QAQC
    stopifnot(sum(item96.table[which(item96.table$BuildingType == "Single Family")
                               ,grep("Percent",colnames(item96.table))], na.rm = T) == 10)


item96.final.SF <- item96.table[which(item96.table$BuildingType == "Single Family")
                                ,-which(colnames(item96.table) %in% c("BuildingType"))]
item96.final.MH <- item96.table[which(item96.table$BuildingType == "Manufactured")
                                ,-which(colnames(item96.table) %in% c("BuildingType"))]

# exportTable(item96.final.SF, "SF", "Table 103", weighted = TRUE)
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

# exportTable(item96.final.SF, "SF", "Table 103", weighted = FALSE)
exportTable(item96.final.MH, "MH", "Table 84", weighted = FALSE)






#############################################################################################
#Item 97: DISTRIBUTION OF WATER HEATERS BY TYPE (SF table 104)
#############################################################################################
#subset to columns needed for analysis
item97.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,"DHW.Type"
                                                                   ,"DHW.Technology"))]
item97.dat$count <- 1

item97.dat0 <- item97.dat[which(item97.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item97.dat1 <- left_join(rbsa.dat, item97.dat0, by = "CK_Cadmus_ID")

item97.dat2 <- item97.dat1[grep("Water Heater",item97.dat1$Generic),]
item97.dat2$Detailed.Type <- paste(item97.dat2$DHW.Type, item97.dat2$DHW.Technology, sep = "-")
unique(item97.dat2$Detailed.Type)

item97.dat3 <- item97.dat2[-grep("unknown",item97.dat2$Detailed.Type, ignore.case = T),]

################################################
# Adding pop and sample sizes for weights
################################################
item97.data <- weightedData(item97.dat3[-which(colnames(item97.dat3) %in% c("Generic"
                                                                            ,"DHW.Fuel"
                                                                            ,"count"
                                                                            ,"DHW.Type"
                                                                            ,"DHW.Technology"
                                                                            ,"Detailed.Type"))])
item97.data <- left_join(item97.data, item97.dat3[which(colnames(item97.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"DHW.Fuel"
                                                                                     ,"count"
                                                                                     ,"DHW.Type"
                                                                                     ,"DHW.Technology"
                                                                                     ,"Detailed.Type"))])

#######################
# Weighted Analysis
#######################
item97.final <- proportions_one_group(CustomerLevelData  = item97.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Generic'
                                      , total.name       = "Total")

# Export table
item97.final.SF <- item97.final[which(item97.final$BuildingType == "Single Family")
                                ,-which(colnames(item97.final) %in% c("BuildingType"))]

# exportTable(item97.final.SF, "SF", "Table 104", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item97.final <- proportions_one_group(CustomerLevelData  = item97.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Generic'
                                      , total.name       = "Total"
                                      , weighted = FALSE)

# Export table
item97.final.SF <- item97.final[which(item97.final$BuildingType == "Single Family")
                                ,-which(colnames(item97.final) %in% c("BuildingType"))]

# exportTable(item97.final.SF, "SF", "Table 104", weighted = FALSE)





#############################################################################################
#Table AI: DISTRIBUTION OF WATER HEATERS BY TYPE (SF table AI)
#############################################################################################
#subset to columns needed for analysis
tableAI.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,"DHW.Type"
                                                                   ,"DHW.Technology"))]
tableAI.dat$count <- 1

tableAI.dat0 <- tableAI.dat[which(tableAI.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableAI.dat1 <- left_join(rbsa.dat, tableAI.dat0, by = "CK_Cadmus_ID")
tableAI.dat1 <- tableAI.dat1[grep("site",tableAI.dat1$CK_Building_ID,ignore.case = T),]
tableAI.dat2 <- tableAI.dat1[grep("Water Heater",tableAI.dat1$Generic),]
tableAI.dat2$Detailed.Type <- paste(tableAI.dat2$DHW.Type, tableAI.dat2$DHW.Technology, sep = "-")
unique(tableAI.dat2$Detailed.Type)

tableAI.dat3 <- tableAI.dat2[-grep("unknown",tableAI.dat2$Detailed.Type, ignore.case = T),]

################################################
# Adding pop and sample sizes for weights
################################################
tableAI.data <- weightedData(tableAI.dat3[-which(colnames(tableAI.dat3) %in% c("Generic"
                                                                            ,"DHW.Fuel"
                                                                            ,"count"
                                                                            ,"DHW.Type"
                                                                            ,"DHW.Technology"
                                                                            ,"Detailed.Type"))])
tableAI.data <- left_join(tableAI.data, tableAI.dat3[which(colnames(tableAI.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"DHW.Fuel"
                                                                                     ,"count"
                                                                                     ,"DHW.Type"
                                                                                     ,"DHW.Technology"
                                                                                     ,"Detailed.Type"))])

#######################
# Weighted Analysis
#######################
tableAI.final <- proportions_one_group(CustomerLevelData  = tableAI.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Detailed.Type'
                                      , total.name       = "Total")

# Export table
tableAI.final.SF <- tableAI.final[which(tableAI.final$BuildingType == "Single Family")
                                ,-which(colnames(tableAI.final) %in% c("BuildingType"))]
tableAI.final.MH <- tableAI.final[which(tableAI.final$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAI.final) %in% c("BuildingType"))]
tableAI.final.MF <- tableAI.final[which(tableAI.final$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAI.final) %in% c("BuildingType"))]

# exportTable(tableAI.final.SF, "SF", "Table AI", weighted = TRUE)
exportTable(tableAI.final.MH, "MH", "Table AI", weighted = TRUE)
# exportTable(tableAI.final.MF, "MF", "Table AI", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
tableAI.final <- proportions_one_group(CustomerLevelData  = tableAI.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Detailed.Type'
                                      , total.name       = "Total"
                                      , weighted = FALSE)

# Export table
tableAI.final.SF <- tableAI.final[which(tableAI.final$BuildingType == "Single Family")
                                ,-which(colnames(tableAI.final) %in% c("BuildingType"))]
tableAI.final.MH <- tableAI.final[which(tableAI.final$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAI.final) %in% c("BuildingType"))]
tableAI.final.MF <- tableAI.final[which(tableAI.final$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAI.final) %in% c("BuildingType"))]

# exportTable(tableAI.final.SF, "SF", "Table AI", weighted = FALSE)
exportTable(tableAI.final.MH, "MH", "Table AI", weighted = FALSE)
# exportTable(tableAI.final.MF, "MF", "Table AI", weighted = FALSE)







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
unique(item98.dat2$DHW.Location)
item98.dat3 <- item98.dat2#[which(item98.dat2$DHW.Location != "Unknown"),]

item98.dat3$DHW.Location[grep("Crawl",item98.dat3$DHW.Location, ignore.case = T)] <- "Crawlspace"
item98.dat3$DHW.Location[grep("In building|in unit|kitchen|bedroom|bathroom|closet|laundry",item98.dat3$DHW.Location, ignore.case = T)] <- "Main House"

item98.dat3$DHW.Location[which(item98.dat3$DHW.Location %notin% c("Crawlspace"
                                                                  ,"Basement"
                                                                  ,"Garage"
                                                                  ,"Main House"))] <- "Other"

unique(item98.dat3$DHW.Location)
length(unique(item98.dat3$CK_Cadmus_ID[which(item98.dat3$BuildingType == "Single Family")]))

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
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

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
                           ,"EB_ID"             = item98.cast$EB_ID
                           ,"EB_MT"             = item98.cast$EB_MT
                           ,"EB_OR"             = item98.cast$EB_OR
                           ,"EB_WA"             = item98.cast$EB_WA
                           ,"EB_Region"         = item98.cast$EB_Region
)
#QAQC
stopifnot(sum(item98.table[which(item98.table$BuildingType == "Single Family")
                           ,grep("Percent",colnames(item98.table))], na.rm = T) == 10)


item98.final.SF <- item98.table[which(item98.table$BuildingType == "Single Family")
                                ,-which(colnames(item98.table) %in% c("BuildingType"))]
item98.final.MH <- item98.table[which(item98.table$BuildingType == "Manufactured")
                                ,-which(colnames(item98.table) %in% c("BuildingType"))]

# exportTable(item98.final.SF, "SF", "Table 105", weighted = TRUE)
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

# exportTable(item98.final.SF, "SF", "Table 105", weighted = FALSE)
exportTable(item98.final.MH, "MH", "Table 85", weighted = FALSE)





#############################################################################################
# Table SS: Average DWH Energy Factor by Fuel Type and State
#############################################################################################
#subset to columns needed for analysis
tableSS.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,"DHW.Energy.Factor"))]

tableSS.dat0 <- tableSS.dat[which(tableSS.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableSS.dat1 <- left_join(rbsa.dat, tableSS.dat0, by = "CK_Cadmus_ID")

unique(tableSS.dat1$DHW.Fuel)
tableSS.dat2 <- tableSS.dat1[which(tableSS.dat1$DHW.Fuel %in% c("Electric"
                                                             , "Natural Gas"
                                                             , "Propane")),]
unique(tableSS.dat2$DHW.Energy.Factor)
tableSS.dat2$DHW.Energy.Factor <- as.numeric(as.character(tableSS.dat2$DHW.Energy.Factor))
tableSS.dat3 <- tableSS.dat2[which(!is.na(tableSS.dat2$DHW.Energy.Factor)),]


################################################
# Adding pop and sample sizes for weights
################################################
tableSS.data <- weightedData(tableSS.dat3[-which(colnames(tableSS.dat3) %in% c("Generic"               
                                                                            ,"DHW.Fuel"
                                                                            ,"DHW.Energy.Factor"))])
tableSS.data <- left_join(tableSS.data, tableSS.dat3[which(colnames(tableSS.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"               
                                                                                     ,"DHW.Fuel"
                                                                                     ,"DHW.Energy.Factor"))])
tableSS.data$count <- 1
#######################
# Weighted Analysis
#######################
tableSS.cast <- mean_two_groups(CustomerLevelData = tableSS.data
                                ,valueVariable = "DHW.Energy.Factor"
                                ,byVariableRow = "DHW.Fuel"
                                ,byVariableColumn = "State"
                                ,columnAggregate = "Region"
                                ,rowAggregate = "All Fuel Types")

tableSS.table <- data.frame("BuildingType"       = tableSS.cast$BuildingType
                           ,"Water.Heater.Fuel" = tableSS.cast$DHW.Fuel
                           ,"Mean_ID"           = tableSS.cast$Mean_ID
                           ,"SE_ID"             = tableSS.cast$SE_ID
                           ,"n_ID"              = tableSS.cast$n_ID
                           ,"Mean_MT"           = tableSS.cast$Mean_MT
                           ,"SE_MT"             = tableSS.cast$SE_MT
                           ,"n_MT"              = tableSS.cast$n_MT
                           ,"Mean_OR"           = tableSS.cast$Mean_OR
                           ,"SE_OR"             = tableSS.cast$SE_OR
                           ,"n_OR"              = tableSS.cast$n_OR
                           ,"Mean_WA"           = tableSS.cast$Mean_WA
                           ,"SE_WA"             = tableSS.cast$SE_WA
                           ,"n_WA"              = tableSS.cast$n_WA
                           ,"Mean_Region"       = tableSS.cast$Mean_Region
                           ,"SE_Region"         = tableSS.cast$SE_Region
                           ,"n_Region"          = tableSS.cast$n_Region
                           ,"EB_ID"             = tableSS.cast$EB_ID
                           ,"EB_MT"             = tableSS.cast$EB_MT
                           ,"EB_OR"             = tableSS.cast$EB_OR
                           ,"EB_WA"             = tableSS.cast$EB_WA
                           ,"EB_Region"         = tableSS.cast$EB_Region
)

# row ordering example code
levels(tableSS.table$Water.Heater.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"Propane"
              ,"All Fuel Types")
tableSS.table <- tableSS.table %>% mutate(Water.Heater.Fuel = factor(Water.Heater.Fuel, levels = rowOrder)) %>% arrange(Water.Heater.Fuel)  
tableSS.table <- data.frame(tableSS.table)

tableSS.final.SF <- tableSS.table[which(tableSS.table$BuildingType == "Single Family")
                                ,-which(colnames(tableSS.table) %in% c("BuildingType"))]
tableSS.final.MH <- tableSS.table[which(tableSS.table$BuildingType == "Manufactured")
                                ,-which(colnames(tableSS.table) %in% c("BuildingType"))]

# exportTable(tableSS.final.SF, "SF", "Table SS", weighted = TRUE)
exportTable(tableSS.final.MH, "MH", "Table SS", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableSS.final <- mean_two_groups_unweighted(CustomerLevelData = tableSS.data
                                            ,valueVariable = "DHW.Energy.Factor"
                                            ,byVariableRow = "DHW.Fuel"
                                            ,byVariableColumn = "State"
                                            ,columnAggregate = "Region"
                                            ,rowAggregate = "All Fuel Types")

tableSS.table <- data.frame("BuildingType"       = tableSS.cast$BuildingType
                            ,"Water.Heater.Fuel" = tableSS.cast$DHW.Fuel
                            ,"Mean_ID"           = tableSS.cast$Mean_ID
                            ,"SE_ID"             = tableSS.cast$SE_ID
                            ,"n_ID"              = tableSS.cast$n_ID
                            ,"Mean_MT"           = tableSS.cast$Mean_MT
                            ,"SE_MT"             = tableSS.cast$SE_MT
                            ,"n_MT"              = tableSS.cast$n_MT
                            ,"Mean_OR"           = tableSS.cast$Mean_OR
                            ,"SE_OR"             = tableSS.cast$SE_OR
                            ,"n_OR"              = tableSS.cast$n_OR
                            ,"Mean_WA"           = tableSS.cast$Mean_WA
                            ,"SE_WA"             = tableSS.cast$SE_WA
                            ,"n_WA"              = tableSS.cast$n_WA
                            ,"Mean_Region"       = tableSS.cast$Mean_Region
                            ,"SE_Region"         = tableSS.cast$SE_Region
                            ,"n_Region"          = tableSS.cast$n_Region
)

# row ordering example code
levels(tableSS.table$Water.Heater.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"Propane"
              ,"All Fuel Types")
tableSS.table <- tableSS.table %>% mutate(Water.Heater.Fuel = factor(Water.Heater.Fuel, levels = rowOrder)) %>% arrange(Water.Heater.Fuel)  
tableSS.table <- data.frame(tableSS.table)

tableSS.final.SF <- tableSS.table[which(tableSS.table$BuildingType == "Single Family")
                                ,-which(colnames(tableSS.table) %in% c("BuildingType"))]
tableSS.final.MH <- tableSS.table[which(tableSS.table$BuildingType == "Manufactured")
                                ,-which(colnames(tableSS.table) %in% c("BuildingType"))]

# exportTable(tableSS.final.SF, "SF", "Table SS", weighted = FALSE)
exportTable(tableSS.final.MH, "MH", "Table SS", weighted = FALSE)




























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
#Item 96: DISTRIBUTION OF WATER HEATER FUEL BY CK_Building_ID (SF table 103, MH table 84)
#############################################################################################
#subset to columns needed for analysis
item96.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,""))]
item96.os.dat$count <- 1

item96.os.dat0 <- item96.os.dat[which(item96.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item96.os.dat1 <- left_join(scl.dat, item96.os.dat0, by = "CK_Cadmus_ID")

unique(item96.os.dat1$DHW.Fuel)
item96.os.dat2 <- item96.os.dat1[which(item96.os.dat1$DHW.Fuel %in% c("Electric"
                                                             , "Natural Gas"
                                                             , "Propane")),]

################################################
# Adding pop and sample sizes for weights
################################################
item96.os.data <- weightedData(item96.os.dat2[-which(colnames(item96.os.dat2) %in% c("Generic"               
                                                                            ,"DHW.Fuel"
                                                                            ,"count"))])
item96.os.data <- left_join(item96.os.data, unique(item96.os.dat2[which(colnames(item96.os.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"               
                                                                                     ,"DHW.Fuel"
                                                                                     ,"count"))]))
#######################
# Weighted Analysis
#######################
item96.os.final <- proportionRowsAndColumns1(CustomerLevelData = item96.os.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'CK_Building_ID'
                                          ,rowVariable      = 'DHW.Fuel'
                                          ,aggregateColumnName = "Remove")

item96.os.cast <- dcast(setDT(item96.os.final)
                     , formula = BuildingType + DHW.Fuel ~ CK_Building_ID
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item96.os.table <- data.frame("BuildingType"       = item96.os.cast$BuildingType
                           ,"Water.Heater.Fuel"    = item96.os.cast$DHW.Fuel
                           ,"Percent_SCL.GenPop"   = item96.os.cast$`w.percent_SCL GenPop`
                           ,"SE_SCL.GenPop"        = item96.os.cast$`w.SE_SCL GenPop`
                           ,"n_SCL.GenPop"         = item96.os.cast$`n_SCL GenPop`
                           ,"Percent_SCL.LI"       = item96.os.cast$`w.percent_SCL LI`
                           ,"SE_SCL.LI"            = item96.os.cast$`w.SE_SCL LI`
                           ,"n_SCL.LI"             = item96.os.cast$`n_SCL LI`
                           ,"Percent_SCL.EH"       = item96.os.cast$`w.percent_SCL EH`
                           ,"SE_SCL.EH"            = item96.os.cast$`w.SE_SCL EH`
                           ,"n_SCL.EH"             = item96.os.cast$`n_SCL EH`
                           ,"Percent_2017.RBSA.PS" = item96.os.cast$`w.percent_2017 RBSA PS`
                           ,"SE_2017.RBSA.PS"      = item96.os.cast$`w.SE_2017 RBSA PS`
                           ,"n_2017.RBSA.PS"       = item96.os.cast$`n_2017 RBSA PS`
                           ,"EB_SCL.GenPop"        = item96.os.cast$`EB_SCL GenPop`
                           ,"EB_SCL.LI"            = item96.os.cast$`EB_SCL LI`
                           ,"EB_SCL.EH"            = item96.os.cast$`EB_SCL EH`
                           ,"EB_2017.RBSA.PS"      = item96.os.cast$`EB_2017 RBSA PS`
)

item96.os.final.SF <- item96.os.table[which(item96.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item96.os.table) %in% c("BuildingType"))]

exportTable(item96.os.final.SF, "SF", "Table 103", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item96.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item96.os.data
                                                  ,valueVariable    = 'count'
                                                  ,columnVariable   = 'CK_Building_ID'
                                                  ,rowVariable      = 'DHW.Fuel'
                                                  ,aggregateColumnName = "Remove")

item96.os.cast <- dcast(setDT(item96.os.final)
                     , formula = BuildingType + DHW.Fuel ~ CK_Building_ID
                     , value.var = c("Percent", "SE", "Count", "n"))


item96.os.table <- data.frame("BuildingType"       = item96.os.cast$BuildingType
                           ,"Water.Heater.Fuel" = item96.os.cast$DHW.Fuel
                           ,"Percent_SCL.GenPop"   = item96.os.cast$`Percent_SCL GenPop`
                           ,"SE_SCL.GenPop"        = item96.os.cast$`SE_SCL GenPop`
                           ,"n_SCL.GenPop"         = item96.os.cast$`n_SCL GenPop`
                           ,"Percent_SCL.LI"       = item96.os.cast$`Percent_SCL LI`
                           ,"SE_SCL.LI"            = item96.os.cast$`SE_SCL LI`
                           ,"n_SCL.LI"             = item96.os.cast$`n_SCL LI`
                           ,"Percent_SCL.EH"       = item96.os.cast$`Percent_SCL EH`
                           ,"SE_SCL.EH"            = item96.os.cast$`SE_SCL EH`
                           ,"n_SCL.EH"             = item96.os.cast$`n_SCL EH`
                           ,"Percent_2017.RBSA.PS" = item96.os.cast$`Percent_2017 RBSA PS`
                           ,"SE_2017.RBSA.PS"      = item96.os.cast$`SE_2017 RBSA PS`
                           ,"n_2017.RBSA.PS"       = item96.os.cast$`n_2017 RBSA PS`
)


item96.os.final.SF <- item96.os.table[which(item96.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item96.os.table) %in% c("BuildingType"))]

exportTable(item96.os.final.SF, "SF", "Table 103", weighted = FALSE, osIndicator = "SCL", OS = T)





#############################################################################################
#Item 97: DISTRIBUTION OF WATER HEATERS BY TYPE (SF table 104)
#############################################################################################
#subset to columns needed for analysis
item97.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,"DHW.Type"
                                                                   ,"DHW.Technology"))]
item97.os.dat$count <- 1

item97.os.dat0 <- item97.os.dat[which(item97.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item97.os.dat1 <- left_join(scl.dat, item97.os.dat0, by = "CK_Cadmus_ID")

item97.os.dat2 <- item97.os.dat1[grep("Water Heater",item97.os.dat1$Generic),]
item97.os.dat2$Detailed.Type <- paste(item97.os.dat2$DHW.Type, item97.os.dat2$DHW.Technology, sep = "-")
unique(item97.os.dat2$Detailed.Type)

item97.os.dat3 <- item97.os.dat2[-grep("unknown",item97.os.dat2$Detailed.Type, ignore.case = T),]

################################################
# Adding pop and sample sizes for weights
################################################
item97.os.data <- weightedData(item97.os.dat3[-which(colnames(item97.os.dat3) %in% c("Generic"
                                                                            ,"DHW.Fuel"
                                                                            ,"count"
                                                                            ,"DHW.Type"
                                                                            ,"DHW.Technology"
                                                                            ,"Detailed.Type"))])
item97.os.data <- left_join(item97.os.data, unique(item97.os.dat3[which(colnames(item97.os.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"DHW.Fuel"
                                                                                     ,"count"
                                                                                     ,"DHW.Type"
                                                                                     ,"DHW.Technology"
                                                                                     ,"Detailed.Type"))]))

#######################
# Weighted Analysis
#######################
item97.os.final <- proportionRowsAndColumns1(CustomerLevelData = item97.os.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable   = 'CK_Building_ID'
                                             ,rowVariable      = 'Generic'
                                             ,aggregateColumnName = "Remove")

item97.os.cast <- dcast(setDT(item97.os.final)
                        , formula = BuildingType + Generic ~ CK_Building_ID
                        , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item97.os.table <- data.frame("BuildingType"          = item97.os.cast$BuildingType
                              ,"Type"                 = item97.os.cast$Generic
                              ,"Percent_SCL.GenPop"   = item97.os.cast$`w.percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item97.os.cast$`w.SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item97.os.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = item97.os.cast$`w.percent_SCL LI`
                              ,"SE_SCL.LI"            = item97.os.cast$`w.SE_SCL LI`
                              ,"n_SCL.LI"             = item97.os.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = item97.os.cast$`w.percent_SCL EH`
                              ,"SE_SCL.EH"            = item97.os.cast$`w.SE_SCL EH`
                              ,"n_SCL.EH"             = item97.os.cast$`n_SCL EH`
                              ,"Percent_2017.RBSA.PS" = item97.os.cast$`w.percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item97.os.cast$`w.SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item97.os.cast$`n_2017 RBSA PS`
                              ,"EB_SCL.GenPop"        = item97.os.cast$`EB_SCL GenPop`
                              ,"EB_SCL.LI"            = item97.os.cast$`EB_SCL LI`
                              ,"EB_SCL.EH"            = item97.os.cast$`EB_SCL EH`
                              ,"EB_2017.RBSA.PS"      = item97.os.cast$`EB_2017 RBSA PS`
)

item97.os.final.SF <- item97.os.table[which(item97.os.table$BuildingType == "Single Family")
                                      ,-which(colnames(item97.os.table) %in% c("BuildingType"))]

exportTable(item97.os.final.SF, "SF", "Table 104", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item97.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item97.os.data
                                                     ,valueVariable    = 'count'
                                                     ,columnVariable   = 'CK_Building_ID'
                                                     ,rowVariable      = 'Generic'
                                                     ,aggregateColumnName = "Remove")

item97.os.cast <- dcast(setDT(item97.os.final)
                        , formula = BuildingType + Generic ~ CK_Building_ID
                        , value.var = c("Percent", "SE", "Count", "n"))


item97.os.table <- data.frame("BuildingType"       = item97.os.cast$BuildingType
                              ,"Type"                 = item97.os.cast$Generic
                              ,"Percent_SCL.GenPop"   = item97.os.cast$`Percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item97.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item97.os.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = item97.os.cast$`Percent_SCL LI`
                              ,"SE_SCL.LI"            = item97.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = item97.os.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = item97.os.cast$`Percent_SCL EH`
                              ,"SE_SCL.EH"            = item97.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = item97.os.cast$`n_SCL EH`
                              ,"Percent_2017.RBSA.PS" = item97.os.cast$`Percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item97.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item97.os.cast$`n_2017 RBSA PS`
)


item97.os.final.SF <- item97.os.table[which(item97.os.table$BuildingType == "Single Family")
                                      ,-which(colnames(item97.os.table) %in% c("BuildingType"))]

exportTable(item97.os.final.SF, "SF", "Table 104", weighted = FALSE, osIndicator = "SCL", OS = T)





#############################################################################################
#Table AI: DISTRIBUTION OF WATER HEATERS BY TYPE (SF table AI)
#############################################################################################
#subset to columns needed for analysis
tableAI.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Type"
                                                                    ,"DHW.Technology"))]
tableAI.os.dat$count <- 1

tableAI.os.dat0 <- tableAI.os.dat[which(tableAI.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableAI.os.dat1 <- left_join(scl.dat, tableAI.os.dat0, by = "CK_Cadmus_ID")
tableAI.os.dat2 <- tableAI.os.dat1[grep("Water Heater",tableAI.os.dat1$Generic),]
tableAI.os.dat2$Detailed.Type <- paste(tableAI.os.dat2$DHW.Type, tableAI.os.dat2$DHW.Technology, sep = "-")
unique(tableAI.os.dat2$Detailed.Type)

tableAI.os.dat3 <- tableAI.os.dat2[-grep("unknown",tableAI.os.dat2$Detailed.Type, ignore.case = T),]

################################################
# Adding pop and sample sizes for weights
################################################
tableAI.os.data <- weightedData(tableAI.os.dat3[-which(colnames(tableAI.os.dat3) %in% c("Generic"
                                                                               ,"DHW.Fuel"
                                                                               ,"count"
                                                                               ,"DHW.Type"
                                                                               ,"DHW.Technology"
                                                                               ,"Detailed.Type"))])
tableAI.os.data <- left_join(tableAI.os.data, unique(tableAI.os.dat3[which(colnames(tableAI.os.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"Generic"
                                                                                         ,"DHW.Fuel"
                                                                                         ,"count"
                                                                                         ,"DHW.Type"
                                                                                         ,"DHW.Technology"
                                                                                         ,"Detailed.Type"))]))

#######################
# Weighted Analysis
#######################
tableAI.os.final <- proportionRowsAndColumns1(CustomerLevelData = tableAI.os.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable   = 'CK_Building_ID'
                                             ,rowVariable      = 'Detailed.Type'
                                             ,aggregateColumnName = "Remove")

tableAI.os.cast <- dcast(setDT(tableAI.os.final)
                        , formula = BuildingType + Detailed.Type ~ CK_Building_ID
                        , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

tableAI.os.table <- data.frame("BuildingType"          = tableAI.os.cast$BuildingType
                              ,"Type"                 = tableAI.os.cast$Detailed.Type
                              ,"Percent_SCL.GenPop"   = tableAI.os.cast$`w.percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = tableAI.os.cast$`w.SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = tableAI.os.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = tableAI.os.cast$`w.percent_SCL LI`
                              ,"SE_SCL.LI"            = tableAI.os.cast$`w.SE_SCL LI`
                              ,"n_SCL.LI"             = tableAI.os.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = tableAI.os.cast$`w.percent_SCL EH`
                              ,"SE_SCL.EH"            = tableAI.os.cast$`w.SE_SCL EH`
                              ,"n_SCL.EH"             = tableAI.os.cast$`n_SCL EH`
                              ,"Percent_2017.RBSA.PS" = tableAI.os.cast$`w.percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = tableAI.os.cast$`w.SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = tableAI.os.cast$`n_2017 RBSA PS`
                              ,"EB_SCL.GenPop"        = tableAI.os.cast$`EB_SCL GenPop`
                              ,"EB_SCL.LI"            = tableAI.os.cast$`EB_SCL LI`
                              ,"EB_SCL.EH"            = tableAI.os.cast$`EB_SCL EH`
                              ,"EB_2017.RBSA.PS"      = tableAI.os.cast$`EB_2017 RBSA PS`
)

tableAI.os.final.SF <- tableAI.os.table[which(tableAI.os.table$BuildingType == "Single Family")
                                      ,-which(colnames(tableAI.os.table) %in% c("BuildingType"))]

exportTable(tableAI.os.final.SF, "SF", "Table AI", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
tableAI.os.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAI.os.data
                                                     ,valueVariable    = 'count'
                                                     ,columnVariable   = 'CK_Building_ID'
                                                     ,rowVariable      = 'Detailed.Type'
                                                     ,aggregateColumnName = "Remove")

tableAI.os.cast <- dcast(setDT(tableAI.os.final)
                        , formula = BuildingType + Detailed.Type ~ CK_Building_ID
                        , value.var = c("Percent", "SE", "Count", "n"))


tableAI.os.table <- data.frame("BuildingType"       = tableAI.os.cast$BuildingType
                              ,"Type"                 = tableAI.os.cast$Detailed.Type
                              ,"Percent_SCL.GenPop"   = tableAI.os.cast$`Percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = tableAI.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = tableAI.os.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = tableAI.os.cast$`Percent_SCL LI`
                              ,"SE_SCL.LI"            = tableAI.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = tableAI.os.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = tableAI.os.cast$`Percent_SCL EH`
                              ,"SE_SCL.EH"            = tableAI.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = tableAI.os.cast$`n_SCL EH`
                              ,"Percent_2017.RBSA.PS" = tableAI.os.cast$`Percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = tableAI.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = tableAI.os.cast$`n_2017 RBSA PS`
)

# Export table
tableAI.os.final.SF <- tableAI.os.final[which(tableAI.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableAI.os.final) %in% c("BuildingType"))]

exportTable(tableAI.os.final.SF, "SF", "Table AI", weighted = FALSE, osIndicator = "SCL", OS = T)





#############################################################################################
#Item 98: DISTRIBUTION OF WATER HEATER LOCATION BY CK_Building_ID (SF table 105, MH table 85)
#############################################################################################
#subset to columns needed for analysis
item98.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,"DHW.Location"))]
item98.os.dat$count <- 1

item98.os.dat0 <- item98.os.dat[which(item98.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item98.os.dat1 <- left_join(scl.dat, item98.os.dat0, by = "CK_Cadmus_ID")

item98.os.dat2 <- item98.os.dat1[grep("Water Heater",item98.os.dat1$Generic),]

item98.os.dat3 <- item98.os.dat2[which(item98.os.dat2$DHW.Location != "Unknown"),]

item98.os.dat3$DHW.Location[grep("Crawl",item98.os.dat3$DHW.Location)] <- "Crawlspace"
item98.os.dat3$DHW.Location[grep("In building",item98.os.dat3$DHW.Location)] <- "Main House"

item98.os.dat3$DHW.Location[which(item98.os.dat3$DHW.Location %notin% c("Crawlspace"
                                                                  ,"Basement"
                                                                  ,"Garage"
                                                                  ,"Main House"))] <- "Other"

unique(item98.os.dat3$DHW.Location)


################################################
# Adding pop and sample sizes for weights
################################################
item98.os.data <- weightedData(item98.os.dat3[-which(colnames(item98.os.dat3) %in% c("Generic"               
                                                                            ,"DHW.Fuel"
                                                                            ,"DHW.Location"
                                                                            ,"count"))])
item98.os.data <- left_join(item98.os.data, unique(item98.os.dat3[which(colnames(item98.os.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"               
                                                                                     ,"DHW.Fuel"
                                                                                     ,"DHW.Location"
                                                                                     ,"count"))]))
#######################
# Weighted Analysis
#######################
item98.os.final <- proportionRowsAndColumns1(CustomerLevelData = item98.os.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'CK_Building_ID'
                                          ,rowVariable      = 'DHW.Location'
                                          ,aggregateColumnName = "Remove")

item98.os.cast <- dcast(setDT(item98.os.final)
                     , formula = BuildingType + DHW.Location ~ CK_Building_ID
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item98.os.table <- data.frame("BuildingType"           = item98.os.cast$BuildingType
                           ,"Water.Heater.Location" = item98.os.cast$DHW.Location
                           ,"Percent_SCL.GenPop"   = item98.os.cast$`w.percent_SCL GenPop`
                           ,"SE_SCL.GenPop"        = item98.os.cast$`w.SE_SCL GenPop`
                           ,"n_SCL.GenPop"         = item98.os.cast$`n_SCL GenPop`
                           ,"Percent_SCL.LI"       = item98.os.cast$`w.percent_SCL LI`
                           ,"SE_SCL.LI"            = item98.os.cast$`w.SE_SCL LI`
                           ,"n_SCL.LI"             = item98.os.cast$`n_SCL LI`
                           ,"Percent_SCL.EH"       = item98.os.cast$`w.percent_SCL EH`
                           ,"SE_SCL.EH"            = item98.os.cast$`w.SE_SCL EH`
                           ,"n_SCL.EH"             = item98.os.cast$`n_SCL EH`
                           ,"Percent_2017.RBSA.PS" = item98.os.cast$`w.percent_2017 RBSA PS`
                           ,"SE_2017.RBSA.PS"      = item98.os.cast$`w.SE_2017 RBSA PS`
                           ,"n_2017.RBSA.PS"       = item98.os.cast$`n_2017 RBSA PS`
                           ,"EB_SCL.GenPop"        = item98.os.cast$`EB_SCL GenPop`
                           ,"EB_SCL.LI"            = item98.os.cast$`EB_SCL LI`
                           ,"EB_SCL.EH"            = item98.os.cast$`EB_SCL EH`
                           ,"EB_2017.RBSA.PS"      = item98.os.cast$`EB_2017 RBSA PS`
)

item98.os.final.SF <- item98.os.table[which(item98.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item98.os.table) %in% c("BuildingType"))]

exportTable(item98.os.final.SF, "SF", "Table 105", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item98.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item98.os.data
                                                  ,valueVariable    = 'count'
                                                  ,columnVariable   = 'CK_Building_ID'
                                                  ,rowVariable      = 'DHW.Location'
                                                  ,aggregateColumnName = "Remove")

item98.os.cast <- dcast(setDT(item98.os.final)
                     , formula = BuildingType + DHW.Location ~ CK_Building_ID
                     , value.var = c("Percent", "SE", "Count", "n"))


item98.os.table <- data.frame("BuildingType"           = item98.os.cast$BuildingType
                           ,"Water.Heater.Location" = item98.os.cast$DHW.Location
                           ,"Percent_SCL.GenPop"   = item98.os.cast$`Percent_SCL GenPop`
                           ,"SE_SCL.GenPop"        = item98.os.cast$`SE_SCL GenPop`
                           ,"n_SCL.GenPop"         = item98.os.cast$`n_SCL GenPop`
                           ,"Percent_SCL.LI"       = item98.os.cast$`Percent_SCL LI`
                           ,"SE_SCL.LI"            = item98.os.cast$`SE_SCL LI`
                           ,"n_SCL.LI"             = item98.os.cast$`n_SCL LI`
                           ,"Percent_SCL.EH"       = item98.os.cast$`Percent_SCL EH`
                           ,"SE_SCL.EH"            = item98.os.cast$`SE_SCL EH`
                           ,"n_SCL.EH"             = item98.os.cast$`n_SCL EH`
                           ,"Percent_2017.RBSA.PS" = item98.os.cast$`Percent_2017 RBSA PS`
                           ,"SE_2017.RBSA.PS"      = item98.os.cast$`SE_2017 RBSA PS`
                           ,"n_2017.RBSA.PS"       = item98.os.cast$`n_2017 RBSA PS`
)

item98.os.final.SF <- item98.os.table[which(item98.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item98.os.table) %in% c("BuildingType"))]

exportTable(item98.os.final.SF, "SF", "Table 105", weighted = FALSE, osIndicator = "SCL", OS = T)



#############################################################################################
# Table SS: Average DWH Energy Factor by Fuel Type and CK_Building_ID
#############################################################################################
#subset to columns needed for analysis
tableSS.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Energy.Factor"))]

tableSS.os.dat0 <- tableSS.os.dat[which(tableSS.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableSS.os.dat1 <- left_join(scl.dat, tableSS.os.dat0, by = "CK_Cadmus_ID")

unique(tableSS.os.dat1$DHW.Fuel)
tableSS.os.dat2 <- tableSS.os.dat1[which(tableSS.os.dat1$DHW.Fuel %in% c("Electric"
                                                                , "Natural Gas"
                                                                , "Propane")),]
unique(tableSS.os.dat2$DHW.Energy.Factor)
tableSS.os.dat2$DHW.Energy.Factor <- as.numeric(as.character(tableSS.os.dat2$DHW.Energy.Factor))
tableSS.os.dat3 <- tableSS.os.dat2[which(!is.na(tableSS.os.dat2$DHW.Energy.Factor)),]


################################################
# Adding pop and sample sizes for weights
################################################
tableSS.os.data <- weightedData(tableSS.os.dat3[-which(colnames(tableSS.os.dat3) %in% c("Generic"               
                                                                               ,"DHW.Fuel"
                                                                               ,"DHW.Energy.Factor"))])
tableSS.os.data <- left_join(tableSS.os.data, unique(tableSS.os.dat3[which(colnames(tableSS.os.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"Generic"               
                                                                                         ,"DHW.Fuel"
                                                                                         ,"DHW.Energy.Factor"))]))
tableSS.os.data$count <- 1
#######################
# Weighted Analysis
#######################
tableSS.os.cast <- mean_two_groups(CustomerLevelData = tableSS.os.data
                                ,valueVariable = "DHW.Energy.Factor"
                                ,byVariableRow = "DHW.Fuel"
                                ,byVariableColumn = "CK_Building_ID"
                                ,columnAggregate = "Remove"
                                ,rowAggregate = "All Fuel Types")

tableSS.os.table <- data.frame("BuildingType"       = tableSS.os.cast$BuildingType
                            ,"Water.Heater.Fuel" = tableSS.os.cast$DHW.Fuel
                            ,"Mean_SCL.GenPop"      = tableSS.os.cast$`Mean_SCL GenPop`
                            ,"SE_SCL.GenPop"        = tableSS.os.cast$`SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = tableSS.os.cast$`n_SCL GenPop`
                            ,"Mean_SCL.LI"          = tableSS.os.cast$`Mean_SCL LI`
                            ,"SE_SCL.LI"            = tableSS.os.cast$`SE_SCL LI`
                            ,"n_SCL.LI"             = tableSS.os.cast$`n_SCL LI`
                            ,"Mean_SCL.EH"          = tableSS.os.cast$`Mean_SCL EH`
                            ,"SE_SCL.EH"            = tableSS.os.cast$`SE_SCL EH`
                            ,"n_SCL.EH"             = tableSS.os.cast$`n_SCL EH`
                            ,"Mean_2017.RBSA.PS"    = tableSS.os.cast$`Mean_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = tableSS.os.cast$`SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = tableSS.os.cast$`n_2017 RBSA PS`
                            ,"EB_SCL.GenPop"        = tableSS.os.cast$`EB_SCL GenPop`
                            ,"EB_SCL.LI"            = tableSS.os.cast$`EB_SCL LI`
                            ,"EB_SCL.EH"            = tableSS.os.cast$`EB_SCL EH`
                            ,"EB_2017.RBSA.PS"      = tableSS.os.cast$`EB_2017 RBSA PS`
)

# row ordering example code
levels(tableSS.os.table$Water.Heater.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"Propane"
              ,"All Fuel Types")
tableSS.os.table <- tableSS.os.table %>% mutate(Water.Heater.Fuel = factor(Water.Heater.Fuel, levels = rowOrder)) %>% arrange(Water.Heater.Fuel)  
tableSS.os.table <- data.frame(tableSS.os.table)

tableSS.os.final.SF <- tableSS.os.table[which(tableSS.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableSS.os.table) %in% c("BuildingType"))]

exportTable(tableSS.os.final.SF, "SF", "Table SS", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
tableSS.os.final <- mean_two_groups_unweighted(CustomerLevelData = tableSS.os.data
                                            ,valueVariable = "DHW.Energy.Factor"
                                            ,byVariableRow = "DHW.Fuel"
                                            ,byVariableColumn = "CK_Building_ID"
                                            ,columnAggregate = "Remove"
                                            ,rowAggregate = "All Fuel Types")

tableSS.os.table <- data.frame("BuildingType"       = tableSS.os.cast$BuildingType
                            ,"Water.Heater.Fuel" = tableSS.os.cast$DHW.Fuel
                            ,"Mean_SCL.GenPop"      = tableSS.os.cast$`Mean_SCL GenPop`
                            ,"SE_SCL.GenPop"        = tableSS.os.cast$`SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = tableSS.os.cast$`n_SCL GenPop`
                            ,"Mean_SCL.LI"          = tableSS.os.cast$`Mean_SCL LI`
                            ,"SE_SCL.LI"            = tableSS.os.cast$`SE_SCL LI`
                            ,"n_SCL.LI"             = tableSS.os.cast$`n_SCL LI`
                            ,"Mean_SCL.EH"          = tableSS.os.cast$`Mean_SCL EH`
                            ,"SE_SCL.EH"            = tableSS.os.cast$`SE_SCL EH`
                            ,"n_SCL.EH"             = tableSS.os.cast$`n_SCL EH`
                            ,"Mean_2017.RBSA.PS"    = tableSS.os.cast$`Mean_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = tableSS.os.cast$`SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = tableSS.os.cast$`n_2017 RBSA PS`
)

# row ordering example code
levels(tableSS.os.table$Water.Heater.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"Propane"
              ,"All Fuel Types")
tableSS.os.table <- tableSS.os.table %>% mutate(Water.Heater.Fuel = factor(Water.Heater.Fuel, levels = rowOrder)) %>% arrange(Water.Heater.Fuel)  
tableSS.os.table <- data.frame(tableSS.os.table)

tableSS.os.final.SF <- tableSS.os.table[which(tableSS.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableSS.os.table) %in% c("BuildingType"))]

exportTable(tableSS.os.final.SF, "SF", "Table SS", weighted = FALSE, osIndicator = "SCL", OS = T)