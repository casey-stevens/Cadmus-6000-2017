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

exportTable(item96.final.SF, "SF", "Table 103", weighted = TRUE)
# exportTable(item96.final.MH, "MH", "Table 84", weighted = TRUE)


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






# #############################################################################################
# #Item 97: DISTRIBUTION OF WATER HEATERS BY TYPE (SF table 104)
# #############################################################################################
# #subset to columns needed for analysis
# item97.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
#                                                                    ,"Generic"
#                                                                    ,"DHW.Fuel"
#                                                                    ,"DHW.Type"
#                                                                    ,"DHW.Technology"))]
# item97.dat$count <- 1
# 
# item97.dat0 <- item97.dat[which(item97.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item97.dat1 <- left_join(rbsa.dat, item97.dat0, by = "CK_Cadmus_ID")
# 
# item97.dat2 <- item97.dat1[grep("Water Heater",item97.dat1$Generic),]
# item97.dat2$Detailed.Type <- paste(item97.dat2$DHW.Type, item97.dat2$DHW.Technology, sep = "-")
# unique(item97.dat2$Detailed.Type)
# 
# item97.dat3 <- item97.dat2[-grep("unknown",item97.dat2$Detailed.Type, ignore.case = T),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item97.data <- weightedData(item97.dat3[-which(colnames(item97.dat3) %in% c("Generic"
#                                                                             ,"DHW.Fuel"
#                                                                             ,"count"
#                                                                             ,"DHW.Type"
#                                                                             ,"DHW.Technology"
#                                                                             ,"Detailed.Type"))])
# item97.data <- left_join(item97.data, item97.dat3[which(colnames(item97.dat3) %in% c("CK_Cadmus_ID"
#                                                                                      ,"Generic"
#                                                                                      ,"DHW.Fuel"
#                                                                                      ,"count"
#                                                                                      ,"DHW.Type"
#                                                                                      ,"DHW.Technology"
#                                                                                      ,"Detailed.Type"))])
# 
# #######################
# # Weighted Analysis
# #######################
# item97.final <- proportions_one_group(CustomerLevelData  = item97.data
#                                       , valueVariable    = 'count'
#                                       , groupingVariable = 'Generic'
#                                       , total.name       = "Total")
# 
# # Export table
# item97.final.SF <- item97.final[which(item97.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item97.final) %in% c("BuildingType"))]
# 
# exportTable(item97.final.SF, "SF", "Table 104", weighted = TRUE)
# 
# #######################
# # Unweighted Analysis
# #######################
# item97.final <- proportions_one_group(CustomerLevelData  = item97.data
#                                       , valueVariable    = 'count'
#                                       , groupingVariable = 'Generic'
#                                       , total.name       = "Total"
#                                       , weighted = FALSE)
# 
# # Export table
# item97.final.SF <- item97.final[which(item97.final$BuildingType == "Single Family")
#                                 ,-which(colnames(item97.final) %in% c("BuildingType"))]
# 
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
