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
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"Primary.Heating.System"
                                                                        ,"Heating.Fuel"))]





#############################################################################################
#Item 43: DISTRIBUTION OF PRIMARY HEATING SYSTEMS (SF table 50, MF table 35)
#############################################################################################
item43.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item43.dat1 <- item43.dat[which(item43.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item43.dat2 <- item43.dat1[which(item43.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(item43.dat2$Primary.Heating.System)



item43.dat2$Heating.System.Ind <- item43.dat2$Primary.Heating.System
item43.dat2$Heating.System.Ind[which(item43.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item43.dat2$Heating.System.Ind[which(item43.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"

item43.dat3 <- unique(data.frame("CK_Cadmus_ID" = item43.dat2$CK_Cadmus_ID
                          ,"Heating_Type"       = item43.dat2$Generic
                          ,"Primary_Secondary"  = item43.dat2$Heating.System.Ind))
### Clean heating type
unique(item43.dat3$Heating_Type)
item43.dat3$Heating_Type[grep("geo|Geo|GEO", item43.dat3$Heating_Type)] <- "Geothermal Heat Pump"

item43.dat4 <- left_join(rbsa.dat, item43.dat3, by = "CK_Cadmus_ID")

item43.dat5 <- item43.dat4[which(item43.dat4$Primary_Secondary == "Primary Heating System"),]
length(unique(item43.dat5$CK_Cadmus_ID))
item43.dat5$count <- 1

item43.dat6 <- item43.dat5[which(!is.na(item43.dat5$Heating_Type)),]

item43.data <- weightedData(item43.dat6[-which(colnames(item43.dat6) %in% c("Heating_Type"
                                                                            ,"Primary_Secondary"
                                                                            ,"count"))])
item43.data <- left_join(item43.data, item43.dat6[which(colnames(item43.dat6) %in% c("CK_Cadmus_ID"
                                                                                     ,"Heating_Type"
                                                                                     ,"Primary_Secondary"
                                                                                     ,"count"))])

#########################
# Weighted Analysis
#########################
item43.final <- proportions_one_group(CustomerLevelData  = item43.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating_Type'
                                      , total.name       = "Total"
                                      , weighted = TRUE)

# export table
# SF = Table 50, MH = Table 32
item43.final.SF <- item43.final[which(item43.final$BuildingType == "Single Family")
                                ,-which(colnames(item43.final) %in% c("BuildingType"
                                                                      ,"Remove"))]
item43.final.MH <- item43.final[which(item43.final$BuildingType == "Manufactured")
                                ,-which(colnames(item43.final) %in% c("BuildingType"
                                                                      ,"Remove"))]

exportTable(item43.final.SF, "SF", "Table 50", weighted = TRUE)
exportTable(item43.final.MH, "MH", "Table 32", weighted = TRUE)

#########################
# Weighted Analysis
#########################
item43.final <- proportions_one_group(CustomerLevelData  = item43.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating_Type'
                                      , total.name       = "Total"
                                      , weighted = FALSE)

# export table
# SF = Table 50, MH = Table 32
item43.final.SF <- item43.final[which(item43.final$BuildingType == "Single Family")
                                ,-which(colnames(item43.final) %in% c("BuildingType"
                                                                      ,"Total.Count"
                                                                      ,"Remove"))]
item43.final.MH <- item43.final[which(item43.final$BuildingType == "Manufactured")
                                ,-which(colnames(item43.final) %in% c("BuildingType"
                                                                      ,"Total.Count"
                                                                      ,"Remove"))]

exportTable(item43.final.SF, "SF", "Table 50", weighted = FALSE)
exportTable(item43.final.MH, "MH", "Table 32", weighted = FALSE)



#############################################################################################
#Item 44: DISTRIBUTION OF FUEL CHOICE FOR PRIMARY HEATING SYSTEMS BY STATE  (SF table 51)
#############################################################################################
item44.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item44.dat1 <- item44.dat[which(item44.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item44.dat2 <- item44.dat1[which(item44.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(item44.dat2$Primary.Heating.System)

item44.dat2$Heating.System.Ind <- item44.dat2$Primary.Heating.System
item44.dat2$Heating.System.Ind[which(item44.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item44.dat2$Heating.System.Ind[which(item44.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"
item44.dat2$Heating.Fuel[which(item44.dat2$Heating.Fuel == "Natural gas")]    <- "Natural Gas"
item44.dat2$Heating.Fuel[which(item44.dat2$Heating.Fuel == "Wood (pellets)")] <- "Pellets"
item44.dat2$Heating.Fuel[which(item44.dat2$Heating.Fuel == "Wood (cord)")]    <- "Wood"
unique(item44.dat2$Heating.Fuel)

item44.dat3 <- unique(data.frame("CK_Cadmus_ID" = item44.dat2$CK_Cadmus_ID
                                 ,"Heating_Type" = item44.dat2$Generic
                                 ,"Heating_Fuel" = item44.dat2$Heating.Fuel
                                 ,"Primary_Secondary" = item44.dat2$Heating.System.Ind))

item44.dat4 <- left_join(rbsa.dat, item44.dat3, by = "CK_Cadmus_ID")

item44.dat5 <- item44.dat4[which(item44.dat4$Primary_Secondary == "Primary Heating System"),]
length(unique(item44.dat5$CK_Cadmus_ID))
item44.dat5$count <- 1

unique(item44.dat5$Heating_Fuel)
item44.dat5$Heating_Fuel[which(item44.dat5$Heating_Fuel == "Kerosene")] <- "Oil"
item44.dat5$Heating_Fuel[which(item44.dat5$Heating_Fuel == "Natural Gas")] <- "Gas"

# Remove entries with missing fuel types
item44.dat6 <- item44.dat5 %>%
                filter(!is.na(Heating_Fuel))

unique(item44.dat6$Heating_Fuel)

item44.dat7 <- item44.dat6 %>%
  filter(Heating_Fuel %notin% c("Unknown"
                                , "Can't Determine"
                                , "Hydronic Gas-Water Fan Heater"
                                , "Hot Water from Water Heater"
                                , "Other"))


item44.data <- weightedData(item44.dat7[-which(colnames(item44.dat7) %in% c("Heating_Type"
                                                                            ,"Heating_Fuel"
                                                                            ,"Primary_Secondary"
                                                                            ,"count"))])

item44.data <- left_join(item44.data, item44.dat7[which(colnames(item44.dat7) %in% c("CK_Cadmus_ID"
                                                                                     ,"Heating_Type"
                                                                                     ,"Heating_Fuel"
                                                                                     ,"Primary_Secondary"
                                                                                     ,"count"))])
################################
# Weighted Analysis
################################
item44.final <- proportionRowsAndColumns1(CustomerLevelData = item44.data
                                      , valueVariable       = 'count'
                                      , columnVariable      = 'State'
                                      , rowVariable         = 'Heating_Fuel'
                                      , aggregateColumnName = 'Region')

item44.cast <- dcast(setDT(item44.final)
                      , formula = BuildingType + Heating_Fuel ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item44.table <- data.frame("BuildingType"    = item44.cast$BuildingType
                           ,"Heating.Fuel"   = item44.cast$Heating_Fuel
                           ,"Percent_ID"     = item44.cast$w.percent_ID
                           ,"SE_ID"          = item44.cast$w.SE_ID
                           ,"n_ID"           = item44.cast$n_ID
                           ,"Percent_MT"     = item44.cast$w.percent_MT
                           ,"SE_MT"          = item44.cast$w.SE_MT
                           ,"n_MT"           = item44.cast$n_MT
                           ,"Percent_OR"     = item44.cast$w.percent_OR
                           ,"SE_OR"          = item44.cast$w.SE_OR
                           ,"n_OR"           = item44.cast$n_OR
                           ,"Percent_WA"     = item44.cast$w.percent_WA
                           ,"SE_WA"          = item44.cast$w.SE_WA
                           ,"n_WA"           = item44.cast$n_WA
                           ,"Percent_Region" = item44.cast$w.percent_Region
                           ,"SE_Region"      = item44.cast$w.SE_Region
                           ,"n_Region"       = item44.cast$n_Region
)


item44.final.SF <- item44.table[which(item44.table$BuildingType == "Single Family")
                                ,-which(colnames(item44.table) %in% c("BuildingType"))]
item44.final.MH <- item44.table[which(item44.table$BuildingType == "Manufactured")
                                ,-which(colnames(item44.table) %in% c("BuildingType"))]

exportTable(item44.final.SF, "SF", "Table 51", weighted = TRUE)
exportTable(item44.final.MH, "MH", "Table 33", weighted = TRUE)




#############################
# Unweighted Analysis
#############################
item44.final <- proportions_two_groups_unweighted(CustomerLevelData = item44.data
                                          , valueVariable       = 'count'
                                          , columnVariable      = 'State'
                                          , rowVariable         = 'Heating_Fuel'
                                          , aggregateColumnName = 'Region')

item44.cast <- dcast(setDT(item44.final)
                     , formula = BuildingType + Heating_Fuel ~ State
                     , value.var = c("Percent", "SE", "Count", "n"))


item44.table <- data.frame("BuildingType"    = item44.cast$BuildingType
                           ,"Heating.Fuel"   = item44.cast$Heating_Fuel
                           ,"Percent_ID"     = item44.cast$Percent_ID
                           ,"SE_ID"          = item44.cast$SE_ID
                           ,"n_ID"           = item44.cast$n_ID
                           ,"Percent_MT"     = item44.cast$Percent_MT
                           ,"SE_MT"          = item44.cast$SE_MT
                           ,"n_MT"           = item44.cast$n_MT
                           ,"Percent_OR"     = item44.cast$Percent_OR
                           ,"SE_OR"          = item44.cast$SE_OR
                           ,"n_OR"           = item44.cast$n_OR
                           ,"Percent_WA"     = item44.cast$Percent_WA
                           ,"SE_WA"          = item44.cast$SE_WA
                           ,"n_WA"           = item44.cast$n_WA
                           ,"Percent_Region" = item44.cast$Percent_Region
                           ,"SE_Region"      = item44.cast$SE_Region
                           ,"n_Region"       = item44.cast$n_Region
)


item44.final.SF <- item44.table[which(item44.table$BuildingType == "Single Family")
                                ,-which(colnames(item44.table) %in% c("BuildingType"))]
item44.final.MH <- item44.table[which(item44.table$BuildingType == "Manufactured")
                                ,-which(colnames(item44.table) %in% c("BuildingType"))]

exportTable(item44.final.SF, "SF", "Table 51", weighted = FALSE)
exportTable(item44.final.MH, "MH", "Table 33", weighted = FALSE)





#############################################################################################
#Item 45: DISTRIBUTION OF SECONDARY HEATING SYSTEMS (SF table 52)
#############################################################################################
item45.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item45.dat1 <- item45.dat[which(item45.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item45.dat2 <- item45.dat1[which(item45.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(item45.dat2$Primary.Heating.System)

item45.dat2$Heating.System.Ind <- item45.dat2$Primary.Heating.System
item45.dat2$Heating.System.Ind[which(item45.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item45.dat2$Heating.System.Ind[which(item45.dat2$Primary.Heating.System ==  "No")] <- "Secondary Heating System"

item45.dat3 <- unique(data.frame("CK_Cadmus_ID" = item45.dat2$CK_Cadmus_ID
                                 ,"Heating_Type" = item45.dat2$Generic
                                 ,"Primary_Secondary" = item45.dat2$Heating.System.Ind))

item45.dat4 <- left_join(rbsa.dat, item45.dat3, by = "CK_Cadmus_ID")

item45.dat5 <- item45.dat4[which(item45.dat4$Primary_Secondary == "Secondary Heating System"),]
length(unique(item45.dat5$CK_Cadmus_ID))
item45.dat5$count <- 1


item45.data <- weightedData(item45.dat5[-which(colnames(item45.dat5) %in% c("Heating_Type"
                                                                            ,"Primary_Secondary"
                                                                            ,"count"))])

item45.data <- left_join(item45.data, item45.dat5[which(colnames(item45.dat5) %in% c("CK_Cadmus_ID"
                                                                                     ,"Heating_Type"
                                                                                     ,"Primary_Secondary"
                                                                                     ,"count"))])

################################
# Weighted Analysis
################################
item45.final <- proportions_one_group(CustomerLevelData  = item45.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating_Type'
                                      , total.name       = "Total"
                                      , columnName       = "Secondary Heating Systems"
                                      , weighted = TRUE)

item45.final.SF <- item45.final[which(item45.final$BuildingType == "Single Family")
                                ,-which(colnames(item45.final) %in% c("BuildingType"))]
item45.final.MH <- item45.final[which(item45.final$BuildingType == "Manufactured")
                                ,-which(colnames(item45.final) %in% c("BuildingType"))]

exportTable(item45.final.SF, "SF", "Table 52", weighted = TRUE)
exportTable(item45.final.MH, "MH", "Table 34", weighted = TRUE)



################################
# Unweighted Analysis
################################
item45.final <- proportions_one_group(CustomerLevelData  = item45.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating_Type'
                                      , total.name       = "Total"
                                      , columnName       = "Remove"
                                      , weighted = FALSE)

item45.final.SF <- item45.final[which(item45.final$BuildingType == "Single Family")
                                ,-which(colnames(item45.final) %in% c("BuildingType"))]
item45.final.MH <- item45.final[which(item45.final$BuildingType == "Manufactured")
                                ,-which(colnames(item45.final) %in% c("BuildingType"))]

exportTable(item45.final.SF, "SF", "Table 52", weighted = FALSE)
exportTable(item45.final.MH, "MH", "Table 34", weighted = FALSE)





#############################################################################################
#Item 46: DISTRIBUTION OF FUEL CHOICE FOR SECONDARY HEATING SYSTEMS BY STATE  (SF table 51)
#############################################################################################
item46.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item46.dat1 <- item46.dat[which(item46.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item46.dat2 <- item46.dat1[which(item46.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(item46.dat2$Primary.Heating.System)

item46.dat2$Heating.System.Ind <- item46.dat2$Primary.Heating.System
item46.dat2$Heating.System.Ind[which(item46.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item46.dat2$Heating.System.Ind[which(item46.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"
item46.dat2$Heating.Fuel[which(item46.dat2$Heating.Fuel == "Natural gas")]       <- "Natural Gas"
item46.dat2$Heating.Fuel[which(item46.dat2$Heating.Fuel == "Natural Gas")]       <- "Gas"
item46.dat2$Heating.Fuel[which(item46.dat2$Heating.Fuel == "Fuel oil/kerosene")] <- "Oil"


item46.dat3 <- unique(data.frame("CK_Cadmus_ID" = item46.dat2$CK_Cadmus_ID
                                 ,"Heating_Type" = item46.dat2$Generic
                                 ,"Heating_Fuel" = item46.dat2$Heating.Fuel
                                 ,"Primary_Secondary" = item46.dat2$Heating.System.Ind))

# Check heating type
unique(item46.dat3$Heating_Type)

item46.dat4 <- left_join(rbsa.dat, item46.dat3, by = "CK_Cadmus_ID")

item46.dat5 <- item46.dat4[which(item46.dat4$Primary_Secondary == "Secondary Heating System"),]
length(unique(item46.dat5$CK_Cadmus_ID))
item46.dat5$count <- 1

# Remove entries with missing fuel types
unique(item46.dat5$Heating_Fuel)
item46.dat6 <- item46.dat5 %>%
  filter(!is.na(Heating_Fuel))
item46.dat7 <- item46.dat6 %>%
  filter(Heating_Fuel %notin% c("Unknown"
                                , "Can't Determine"
                                , "Hydronic Gas-Water Fan Heater"
                                , "Hot Water from Water Heater"
                                , "Other"))


item46.data <- weightedData(item46.dat7[-which(colnames(item46.dat7) %in% c("Heating_Type"
                                                                            ,"Heating_Fuel"
                                                                            ,"Primary_Secondary"
                                                                            ,"count"))])

item46.data <- left_join(item46.data, item46.dat7[which(colnames(item46.dat7) %in% c("CK_Cadmus_ID"
                                                                                     ,"Heating_Type"
                                                                                     ,"Heating_Fuel"
                                                                                     ,"Primary_Secondary"
                                                                                     ,"count"))])

################################
# weighted Analysis
################################
item46.final <- proportionRowsAndColumns1(CustomerLevelData = item46.data
                                          , valueVariable       = 'count'
                                          , columnVariable      = 'State'
                                          , rowVariable         = 'Heating_Fuel'
                                          , aggregateColumnName = 'Region')

item46.cast <- dcast(setDT(item46.final)
                     , formula = BuildingType + Heating_Fuel ~ State
                     , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item46.table <- data.frame("BuildingType"     = item46.cast$BuildingType
                           ,"Heating.Fuel"   = item46.cast$Heating_Fuel
                           ,"Percent_ID"     = item46.cast$w.percent_ID
                           ,"SE_ID"          = item46.cast$w.SE_ID
                           ,"n_ID"           = item46.cast$n_ID
                           ,"Percent_MT"     = item46.cast$w.percent_MT
                           ,"SE_MT"          = item46.cast$w.SE_MT
                           ,"n_MT"           = item46.cast$n_MT
                           ,"Percent_OR"     = item46.cast$w.percent_OR
                           ,"SE_OR"          = item46.cast$w.SE_OR
                           ,"n_OR"           = item46.cast$n_OR
                           ,"Percent_WA"     = item46.cast$w.percent_WA
                           ,"SE_WA"          = item46.cast$w.SE_WA
                           ,"n_WA"           = item46.cast$n_WA
                           ,"Percent_Region" = item46.cast$w.percent_Region
                           ,"SE_Region"      = item46.cast$w.SE_Region
                           ,"n_Region"       = item46.cast$n_Region
)


item46.final.SF <- item46.table[which(item46.table$BuildingType == "Single Family")
                                ,-which(colnames(item46.table) %in% c("BuildingType"))]
item46.final.MH <- item46.table[which(item46.table$BuildingType == "Manufactured")
                                ,-which(colnames(item46.table) %in% c("BuildingType"))]

exportTable(item46.final.SF, "SF", "Table 53", weighted = TRUE)
exportTable(item46.final.MH, "MH", "Table 35", weighted = TRUE)


################################
# Unweighted Analysis
################################
item46.final <- proportions_two_groups_unweighted(CustomerLevelData = item46.data
                                          , valueVariable       = 'count'
                                          , columnVariable      = 'State'
                                          , rowVariable         = 'Heating_Fuel'
                                          , aggregateColumnName = 'Region')

item46.cast <- dcast(setDT(item46.final)
                     , formula = BuildingType + Heating_Fuel ~ State
                     , value.var = c("Percent", "SE", "Count", "n"))

item46.table <- data.frame("BuildingType"     = item46.cast$BuildingType
                           ,"Heating.Fuel"   = item46.cast$Heating_Fuel
                           ,"Percent_ID"     = item46.cast$Percent_ID
                           ,"SE_ID"          = item46.cast$SE_ID
                           ,"n_ID"           = item46.cast$n_ID
                           ,"Percent_MT"     = item46.cast$Percent_MT
                           ,"SE_MT"          = item46.cast$SE_MT
                           ,"n_MT"           = item46.cast$n_MT
                           ,"Percent_OR"     = item46.cast$Percent_OR
                           ,"SE_OR"          = item46.cast$SE_OR
                           ,"n_OR"           = item46.cast$n_OR
                           ,"Percent_WA"     = item46.cast$Percent_WA
                           ,"SE_WA"          = item46.cast$SE_WA
                           ,"n_WA"           = item46.cast$n_WA
                           ,"Percent_Region" = item46.cast$Percent_Region
                           ,"SE_Region"      = item46.cast$SE_Region
                           ,"n_Region"       = item46.cast$n_Region
)


item46.final.SF <- item46.table[which(item46.table$BuildingType == "Single Family")
                                ,-which(colnames(item46.table) %in% c("BuildingType"))]
item46.final.MH <- item46.table[which(item46.table$BuildingType == "Manufactured")
                                ,-which(colnames(item46.table) %in% c("BuildingType"))]

exportTable(item46.final.SF, "SF", "Table 53", weighted = FALSE)
exportTable(item46.final.MH, "MH", "Table 35", weighted = FALSE)
