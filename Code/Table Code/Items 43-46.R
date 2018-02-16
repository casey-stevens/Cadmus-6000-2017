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
download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx'
              , mechanical.export
              , mode = 'wb')
mechanical.dat <- read.xlsx(mechanical.export)
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"Primary.Heating.System"
                                                                        ,"Heating.Fuel"
                                                                        ,"System.Sub-Type"))]





#############################################################################################
#Item 43: DISTRIBUTION OF PRIMARY HEATING SYSTEMS (SF table 50, MF table 35)
#############################################################################################
item43.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item43.dat1 <- item43.dat[which(item43.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item43.dat2 <- item43.dat1[which(item43.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(item43.dat2$Primary.Heating.System)
unique(item43.dat2$Generic)


item43.dat2$Heating.System.Ind <- item43.dat2$Primary.Heating.System
item43.dat2$Heating.System.Ind[which(item43.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item43.dat2$Heating.System.Ind[which(item43.dat2$Primary.Heating.System == "No")]  <- "Secondary Heating System"

item43.dat2$`System.Sub-Type`[grep("plug-in|plug in", item43.dat2$`System.Sub-Type`, ignore.case = T)]

for (ii in 1:nrow(item43.dat2)){
  # if (item43.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
  #   item43.dat2$Generic[ii] <- item43.dat2$`System.Sub-Type`[ii]
  # }
  if (item43.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater", "Vertical Wall Heater")){
    item43.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (item43.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater", "Electric Plug In Heater", "Electric Plug-In Heater", "Plug In Heater")){
    item43.dat2$Generic[ii] <- "Plug-In Heaters"
  }
}

item43.dat2$Generic[grep("Baseboard",item43.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item43.dat2$Generic[grep("zonal heat",item43.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
item43.dat2$Generic[grep("ductless",item43.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
item43.dat2$Generic[grep("furnace",item43.dat2$Generic,ignore.case = T)] <- "Furnace"
item43.dat2$Generic[grep("boiler",item43.dat2$Generic,ignore.case = T)] <- "Boiler"
item43.dat2$Generic[grep("Stove/Fireplace",item43.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"

unique(item43.dat2$Generic)

item43.dat3 <- unique(data.frame("CK_Cadmus_ID" = item43.dat2$CK_Cadmus_ID
                          ,"Heating_Type"       = item43.dat2$Generic
                          ,"Primary_Secondary"  = item43.dat2$Heating.System.Ind))
### Clean heating type
unique(item43.dat3$Heating_Type)
# item43.dat3$Heating_Type[grep("geo", item43.dat3$Heating_Type, ignore.case = T)] <- "Geothermal Heat Pump"

item43.dat4 <- left_join(rbsa.dat, item43.dat3, by = "CK_Cadmus_ID")
unique(item43.dat4$Primary_Secondary)
item43.dat5 <- unique(item43.dat4[which(item43.dat4$Primary_Secondary == "Primary Heating System"),])
unique(item43.dat5$Primary_Secondary)


length(unique(item43.dat5$CK_Cadmus_ID[which(item43.dat5$BuildingType == "Single Family")]))
nrow(item43.dat5[which(item43.dat5$BuildingType == "Single Family"),])
dup.ids <- item43.dat5$CK_Cadmus_ID[which(duplicated(item43.dat5$CK_Cadmus_ID) & item43.dat5$BuildingType == "Single Family")]
item43.dat5$count <- 1

item43.duplicates <- item43.dat5[which(item43.dat5$CK_Cadmus_ID %in% dup.ids),]

item43.dat6 <- item43.dat5[which(item43.dat5$Heating_Type %notin% c("N/A",NA)),]
unique(item43.dat6$Heating_Type)

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
                                ,-which(colnames(item43.final) %in% c("BuildingType"))]
item43.final.MH <- item43.final[which(item43.final$BuildingType == "Manufactured")
                                ,-which(colnames(item43.final) %in% c("BuildingType"))]

# exportTable(item43.final.SF, "SF", "Table 50", weighted = TRUE)
exportTable(item43.final.MH, "MH", "Table 32", weighted = TRUE)

#########################
# unWeighted Analysis
#########################
item43.final <- proportions_one_group(CustomerLevelData  = item43.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating_Type'
                                      , total.name       = "Total"
                                      , weighted = FALSE)

# export table
# SF = Table 50, MH = Table 32
item43.final.SF <- item43.final[which(item43.final$BuildingType == "Single Family")
                                ,-which(colnames(item43.final) %in% c("BuildingType"))]
item43.final.MH <- item43.final[which(item43.final$BuildingType == "Manufactured")
                                ,-which(colnames(item43.final) %in% c("BuildingType"))]

# exportTable(item43.final.SF, "SF", "Table 50", weighted = FALSE)
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

for (ii in 1:nrow(item43.dat2)){
  # if (item43.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
  #   item43.dat2$Generic[ii] <- item43.dat2$`System.Sub-Type`[ii]
  # }
  if (item43.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater", "Vertical Wall Heater")){
    item43.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (item43.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater", "Electric Plug In Heater", "Electric Plug-In Heater", "Plug In Heater")){
    item43.dat2$Generic[ii] <- "Plug-In Heaters"
  }
}
item44.dat2$Generic[grep("Electric Baseboard",item44.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item44.dat2$Generic[grep("zonal heat",item44.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
item44.dat2$Generic[grep("ductless",item44.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
item44.dat2$Generic[grep("furnace",item44.dat2$Generic,ignore.case = T)] <- "Furnace"
item44.dat2$Generic[grep("boiler",item44.dat2$Generic,ignore.case = T)] <- "Boiler"
item44.dat2$Generic[grep("Stove/Fireplace",item44.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"


item44.dat3 <- unique(data.frame("CK_Cadmus_ID" = item44.dat2$CK_Cadmus_ID
                                 ,"Heating_Type" = item44.dat2$Generic
                                 ,"Heating_Fuel" = item44.dat2$Heating.Fuel
                                 ,"Primary_Secondary" = item44.dat2$Heating.System.Ind))

item44.dat4 <- left_join(rbsa.dat, item44.dat3, by = "CK_Cadmus_ID")

item44.dat5 <- item44.dat4[which(item44.dat4$Primary_Secondary == "Primary Heating System"),]
length(unique(item44.dat5$CK_Cadmus_ID))
item44.dat5$count <- 1

item44.dat5$Heating_Fuel <- as.character(item44.dat5$Heating_Fuel)
unique(item44.dat5$Heating_Fuel)
item44.dat5$Heating_Fuel[which(item44.dat5$Heating_Fuel == "Kerosene")] <- "Oil"
item44.dat5$Heating_Fuel[grep("Natural Gas",item44.dat5$Heating_Fuel, ignore.case = T)] <- "Gas"
unique(item44.dat5$Heating_Fuel)

# Remove entries with missing fuel types
item44.dat6 <- item44.dat5 %>%
                filter(Heating_Fuel %notin% c("N/A",NA))

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
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

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
                           ,"EB_ID"          = item44.cast$EB_ID
                           ,"EB_MT"          = item44.cast$EB_MT
                           ,"EB_OR"          = item44.cast$EB_OR
                           ,"EB_WA"          = item44.cast$EB_WA
                           ,"EB_Region"      = item44.cast$EB_Region
)


item44.final.SF <- item44.table[which(item44.table$BuildingType == "Single Family")
                                ,-which(colnames(item44.table) %in% c("BuildingType"))]
item44.final.MH <- item44.table[which(item44.table$BuildingType == "Manufactured")
                                ,-which(colnames(item44.table) %in% c("BuildingType"))]

# exportTable(item44.final.SF, "SF", "Table 51", weighted = TRUE)
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

# exportTable(item44.final.SF, "SF", "Table 51", weighted = FALSE)
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


for (ii in 1:nrow(item43.dat2)){
  # if (item43.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
  #   item43.dat2$Generic[ii] <- item43.dat2$`System.Sub-Type`[ii]
  # }
  if (item43.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater", "Vertical Wall Heater")){
    item43.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (item43.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater", "Electric Plug In Heater", "Electric Plug-In Heater", "Plug In Heater")){
    item43.dat2$Generic[ii] <- "Plug-In Heaters"
  }
}
unique(item45.dat2$Generic)
item45.dat2$Generic[grep("baseboard",item45.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item45.dat2$Generic[grep("zonal heat",item45.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
item45.dat2$Generic[grep("plug in|plug-in",item45.dat2$Generic,ignore.case = T)] <- "Plug-In Heaters"
item45.dat2$Generic[which(item45.dat2$Generic == "Heat Pump")] <- "Air Source Heat Pump"
item45.dat2$Generic[grep("ductless",item45.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
item45.dat2$Generic[grep("furnace",item45.dat2$Generic,ignore.case = T)] <- "Furnace"
item45.dat2$Generic[grep("boiler|storage water heater",item45.dat2$Generic,ignore.case = T)] <- "Boiler"
item45.dat2$Generic[grep("Packaged AC",item45.dat2$Generic,ignore.case = T)] <- "Packaged AC"
item45.dat2$Generic[grep("Stove/Fireplace",item45.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"
item45.dat2$Generic[which(item45.dat2$Generic == "Package Terminal Heat Pump")] <- "Packaged HP"

unique(item45.dat2$Generic)

item45.dat3 <- unique(data.frame("CK_Cadmus_ID" = item45.dat2$CK_Cadmus_ID
                                 ,"Heating_Type" = item45.dat2$Generic
                                 ,"Primary_Secondary" = item45.dat2$Heating.System.Ind))
item45.dat3 <- item45.dat3[which(item45.dat3$Heating_Type %notin% c("N/A",NA)),]

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
                                      , weighted = TRUE)

item45.final.SF <- item45.final[which(item45.final$BuildingType == "Single Family")
                                ,-which(colnames(item45.final) %in% c("BuildingType"))]
item45.final.MH <- item45.final[which(item45.final$BuildingType == "Manufactured")
                                ,-which(colnames(item45.final) %in% c("BuildingType"))]

# exportTable(item45.final.SF, "SF", "Table 52", weighted = TRUE)
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

# exportTable(item45.final.SF, "SF", "Table 52", weighted = FALSE)
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

for (ii in 1:nrow(item43.dat2)){
  # if (item43.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
  #   item43.dat2$Generic[ii] <- item43.dat2$`System.Sub-Type`[ii]
  # }
  if (item43.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater", "Vertical Wall Heater")){
    item43.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (item43.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater", "Electric Plug In Heater", "Electric Plug-In Heater", "Plug In Heater")){
    item43.dat2$Generic[ii] <- "Plug-In Heaters"
  }
}
item46.dat2$Generic[grep("Electric Baseboard",item46.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item46.dat2$Generic[grep("zonal heat",item46.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
item46.dat2$Generic[grep("ductless",item46.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
item46.dat2$Generic[grep("furnace",item46.dat2$Generic,ignore.case = T)] <- "Furnace"
item46.dat2$Generic[grep("boiler",item46.dat2$Generic,ignore.case = T)] <- "Boiler"
item46.dat2$Generic[grep("Stove/Fireplace",item46.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"

item46.dat3 <- unique(data.frame("CK_Cadmus_ID" = item46.dat2$CK_Cadmus_ID
                                 ,"Heating_Type" = item46.dat2$Generic
                                 ,"Heating_Fuel" = item46.dat2$Heating.Fuel
                                 ,"Primary_Secondary" = item46.dat2$Heating.System.Ind))

item46.dat3 <- item46.dat3[which(item46.dat3$Heating_Type %notin% c("N/A",NA)),]
# Check heating type
unique(item46.dat3$Heating_Type)

item46.dat4 <- left_join(rbsa.dat, item46.dat3, by = "CK_Cadmus_ID")

item46.dat5 <- item46.dat4[which(item46.dat4$Primary_Secondary == "Secondary Heating System"),]
length(unique(item46.dat5$CK_Cadmus_ID))
item46.dat5$count <- 1

item46.dat5$Heating_Fuel <- as.character(item46.dat5$Heating_Fuel)

# Remove entries with missing fuel types
unique(item46.dat5$Heating_Fuel)
item46.dat6 <- item46.dat5 %>%
  filter(Heating_Fuel %notin% c("N/A",NA))
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
                     , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item46.table <- data.frame("BuildingType"    = item46.cast$BuildingType
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
                           ,"EB_ID"          = item46.cast$EB_ID
                           ,"EB_MT"          = item46.cast$EB_MT
                           ,"EB_OR"          = item46.cast$EB_OR
                           ,"EB_WA"          = item46.cast$EB_WA
                           ,"EB_Region"      = item46.cast$EB_Region
)


item46.final.SF <- item46.table[which(item46.table$BuildingType == "Single Family")
                                ,-which(colnames(item46.table) %in% c("BuildingType"))]
item46.final.MH <- item46.table[which(item46.table$BuildingType == "Manufactured")
                                ,-which(colnames(item46.table) %in% c("BuildingType"))]

# exportTable(item46.final.SF, "SF", "Table 53", weighted = TRUE)
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

# exportTable(item46.final.SF, "SF", "Table 53", weighted = FALSE)
exportTable(item46.final.MH, "MH", "Table 35", weighted = FALSE)























############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
os.ind <- "snopud"
export.ind <- "SnoPUD"
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]

#############################################################################################
#Item 43: DISTRIBUTION OF PRIMARY HEATING SYSTEMS (SF table 50, MF table 35)
#############################################################################################
item43.os.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item43.os.dat1 <- item43.os.dat[which(item43.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item43.os.dat2 <- item43.os.dat1[which(item43.os.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(item43.os.dat2$Primary.Heating.System)



item43.os.dat2$Heating.System.Ind <- item43.os.dat2$Primary.Heating.System
item43.os.dat2$Heating.System.Ind[which(item43.os.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item43.os.dat2$Heating.System.Ind[which(item43.os.dat2$Primary.Heating.System == "No")]  <- "Secondary Heating System"

unique(item43.os.dat2$`System.Sub-Type`)

for (ii in 1:nrow(item43.os.dat2)){
  # if (item43.os.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
  #   item43.os.dat2$Generic[ii] <- item43.os.dat2$`System.Sub-Type`[ii]
  # }
  if (item43.os.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater")){
    item43.os.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (item43.os.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater")){
    item43.os.dat2$Generic[ii] <- "Plug-In Heaters"
  }
}

item43.os.dat2$Generic[grep("Electric Baseboard",item43.os.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item43.os.dat2$Generic[grep("zonal heat",item43.os.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
item43.os.dat2$Generic[grep("ductless",item43.os.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
item43.os.dat2$Generic[grep("furnace",item43.os.dat2$Generic,ignore.case = T)] <- "Furnace"
item43.os.dat2$Generic[grep("boiler",item43.os.dat2$Generic,ignore.case = T)] <- "Boiler"
item43.os.dat2$Generic[grep("Stove/Fireplace",item43.os.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"

unique(item43.os.dat2$Generic)

item43.os.dat3 <- unique(data.frame("CK_Cadmus_ID" = item43.os.dat2$CK_Cadmus_ID
                                 ,"Heating_Type"       = item43.os.dat2$Generic
                                 ,"Primary_Secondary"  = item43.os.dat2$Heating.System.Ind))
### Clean heating type
unique(item43.os.dat3$Heating_Type)
# item43.os.dat3$Heating_Type[grep("geo", item43.os.dat3$Heating_Type, ignore.case = T)] <- "Geothermal Heat Pump"

item43.os.dat4 <- left_join(os.dat, item43.os.dat3, by = "CK_Cadmus_ID")
unique(item43.os.dat4$Primary_Secondary)
item43.os.dat5 <- unique(item43.os.dat4[which(item43.os.dat4$Primary_Secondary == "Primary Heating System"),])
unique(item43.os.dat5$Primary_Secondary)


length(unique(item43.os.dat5$CK_Cadmus_ID[which(item43.os.dat5$BuildingType == "Single Family")]))
nrow(item43.os.dat5[which(item43.os.dat5$BuildingType == "Single Family"),])
dup.ids <- data.frame(item43.os.dat5$CK_Cadmus_ID[which(duplicated(item43.os.dat5$CK_Cadmus_ID) & item43.os.dat5$BuildingType == "Single Family")])
item43.os.dat5$count <- 1

item43.os.dat6 <- item43.os.dat5[which(item43.os.dat5$Heating_Type %notin% c("N/A",NA)),]
unique(item43.os.dat6$Heating_Type)

item43.os.data <- weightedData(item43.os.dat6[-which(colnames(item43.os.dat6) %in% c("Heating_Type"
                                                                            ,"Primary_Secondary"
                                                                            ,"count"))])
item43.os.data <- left_join(item43.os.data, item43.os.dat6[which(colnames(item43.os.dat6) %in% c("CK_Cadmus_ID"
                                                                                     ,"Heating_Type"
                                                                                     ,"Primary_Secondary"
                                                                                     ,"count"))])

#########################
# Weighted Analysis
#########################
item43.os.final <- proportionRowsAndColumns1(CustomerLevelData  = item43.os.data
                                      , valueVariable    = 'count'
                                      ,columnVariable = "CK_Building_ID"
                                      , rowVariable = 'Heating_Type'
                                      ,aggregateColumnName = "Remove")
item43.os.final <- item43.os.final[which(item43.os.final$CK_Building_ID != "Remove"),]

item43.os.cast <- dcast(setDT(item43.os.final)
                        ,formula = Heating_Type ~ CK_Building_ID
                        ,value.var = c("w.percent","w.SE","count","n","N","EB"))

names(item43.os.cast)
if(os.ind == "scl"){
  item43.os.table <- data.frame("Heating_Type"         = item43.os.cast$Heating_Type
                                ,"Percent_SCL.GenPop"   = item43.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item43.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item43.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item43.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item43.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item43.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item43.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item43.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item43.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item43.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item43.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item43.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item43.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item43.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item43.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item43.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item43.os.table <- data.frame("Heating_Type"         = item43.os.cast$Heating_Type
                                ,"Percent_SnoPUD"          = item43.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item43.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item43.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item43.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item43.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item43.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item43.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item43.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item43.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item43.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item43.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item43.os.cast$`EB_2017 RBSA NW`)
}



exportTable(item43.os.table, "SF", "Table 50", weighted = TRUE, osIndicator = export.ind, OS = T)

#########################
# unWeighted Analysis
#########################
item43.os.final <- proportions_two_groups_unweighted(CustomerLevelData  = item43.os.data
                                             , valueVariable    = 'count'
                                             ,columnVariable = "CK_Building_ID"
                                             , rowVariable = 'Heating_Type'
                                             ,aggregateColumnName = "Remove")
item43.os.final <- item43.os.final[which(item43.os.final$CK_Building_ID != "Remove"),]

item43.os.cast <- dcast(setDT(item43.os.final)
                        ,formula = Heating_Type ~ CK_Building_ID
                        ,value.var = c("Percent","SE","Count","n"))
names(item43.os.cast)

if(os.ind == "scl"){
  item43.os.table <- data.frame("Heating_Type"         = item43.os.cast$Heating_Type
                                ,"Percent_SCL.GenPop"   = item43.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item43.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item43.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item43.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item43.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item43.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item43.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item43.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item43.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item43.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item43.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item43.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item43.os.table <- data.frame("Heating_Type"         = item43.os.cast$Heating_Type
                                ,"Percent_SnoPUD"          = item43.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item43.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item43.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item43.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item43.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item43.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item43.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item43.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item43.os.cast$`n_2017 RBSA NW`)
}



exportTable(item43.os.table, "SF", "Table 50", weighted = FALSE, osIndicator = export.ind, OS = T)



#############################################################################################
#Item 44: DISTRIBUTION OF FUEL CHOICE FOR PRIMARY HEATING SYSTEMS BY CK_Building_ID  (SF table 51)
#############################################################################################
item44.os.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item44.os.dat1 <- item44.os.dat[which(item44.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item44.os.dat2 <- item44.os.dat1[which(item44.os.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(item44.os.dat2$Primary.Heating.System)

item44.os.dat2$Heating.System.Ind <- item44.os.dat2$Primary.Heating.System
item44.os.dat2$Heating.System.Ind[which(item44.os.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item44.os.dat2$Heating.System.Ind[which(item44.os.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"
item44.os.dat2$Heating.Fuel[which(item44.os.dat2$Heating.Fuel == "Natural gas")]    <- "Natural Gas"
item44.os.dat2$Heating.Fuel[which(item44.os.dat2$Heating.Fuel == "Wood (pellets)")] <- "Pellets"
item44.os.dat2$Heating.Fuel[which(item44.os.dat2$Heating.Fuel == "Wood (cord)")]    <- "Wood"
unique(item44.os.dat2$Heating.Fuel)

for (ii in 1:nrow(item44.os.dat2)){
  if (item44.os.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
    item44.os.dat2$Generic[ii] <- item44.os.dat2$`System.Sub-Type`[ii]
  }
  if (item44.os.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater")){
    item44.os.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
}

item44.os.dat2$Generic[grep("Electric Baseboard",item44.os.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item44.os.dat2$Generic[grep("zonal heat",item44.os.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
item44.os.dat2$Generic[grep("ductless",item44.os.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
item44.os.dat2$Generic[grep("furnace",item44.os.dat2$Generic,ignore.case = T)] <- "Furnace"
item44.os.dat2$Generic[grep("boiler",item44.os.dat2$Generic,ignore.case = T)] <- "Boiler"
item44.os.dat2$Generic[grep("Stove/Fireplace",item44.os.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"


item44.os.dat3 <- unique(data.frame("CK_Cadmus_ID" = item44.os.dat2$CK_Cadmus_ID
                                 ,"Heating_Type" = item44.os.dat2$Generic
                                 ,"Heating_Fuel" = item44.os.dat2$Heating.Fuel
                                 ,"Primary_Secondary" = item44.os.dat2$Heating.System.Ind))

item44.os.dat4 <- left_join(os.dat, item44.os.dat3, by = "CK_Cadmus_ID")

item44.os.dat5 <- item44.os.dat4[which(item44.os.dat4$Primary_Secondary == "Primary Heating System"),]
length(unique(item44.os.dat5$CK_Cadmus_ID))
item44.os.dat5$count <- 1

unique(item44.os.dat5$Heating_Fuel)
item44.os.dat5$Heating_Fuel[which(item44.os.dat5$Heating_Fuel == "Kerosene")] <- "Oil"
item44.os.dat5$Heating_Fuel[which(item44.os.dat5$Heating_Fuel == "Natural Gas")] <- "Gas"

# Remove entries with missing fuel types
item44.os.dat6 <- item44.os.dat5 %>%
  filter(Heating_Fuel %notin% c("N/A",NA))

unique(item44.os.dat6$Heating_Fuel)

item44.os.dat7 <- item44.os.dat6 %>%
  filter(Heating_Fuel %notin% c("Unknown"
                                , "Can't Determine"
                                , "Hydronic Gas-Water Fan Heater"
                                , "Hot Water from Water Heater"
                                , "Other"))


item44.os.data <- weightedData(item44.os.dat7[-which(colnames(item44.os.dat7) %in% c("Heating_Type"
                                                                            ,"Heating_Fuel"
                                                                            ,"Primary_Secondary"
                                                                            ,"count"))])

item44.os.data <- left_join(item44.os.data, item44.os.dat7[which(colnames(item44.os.dat7) %in% c("CK_Cadmus_ID"
                                                                                     ,"Heating_Type"
                                                                                     ,"Heating_Fuel"
                                                                                     ,"Primary_Secondary"
                                                                                     ,"count"))])
################################
# Weighted Analysis
################################
item44.os.final <- proportionRowsAndColumns1(CustomerLevelData = item44.os.data
                                          , valueVariable       = 'count'
                                          , columnVariable      = 'CK_Building_ID'
                                          , rowVariable         = 'Heating_Fuel'
                                          , aggregateColumnName = 'Region')

item44.os.cast <- dcast(setDT(item44.os.final)
                     , formula = BuildingType + Heating_Fuel ~ CK_Building_ID
                     , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
names(item44.os.cast)
if(os.ind == "scl"){
  item44.os.table <- data.frame("Heating.Fuel"          = item44.os.cast$Heating_Fuel
                                ,"Percent_SCL.GenPop"   = item44.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item44.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item44.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item44.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item44.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item44.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item44.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item44.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item44.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item44.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item44.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item44.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item44.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item44.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item44.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item44.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item44.os.table <- data.frame("Heating.Fuel"          = item44.os.cast$Heating_Fuel
                                ,"Percent_SnoPUD"          = item44.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item44.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item44.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item44.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item44.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item44.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item44.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item44.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item44.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item44.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item44.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item44.os.cast$`EB_2017 RBSA NW`)
}



exportTable(item44.os.table, "SF", "Table 51", weighted = TRUE, osIndicator = export.ind, OS = T)

#############################
# Unweighted Analysis
#############################
item44.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item44.os.data
                                                  , valueVariable       = 'count'
                                                  , columnVariable      = 'CK_Building_ID'
                                                  , rowVariable         = 'Heating_Fuel'
                                                  , aggregateColumnName = 'Region')

item44.os.cast <- dcast(setDT(item44.os.final)
                     , formula = BuildingType + Heating_Fuel ~ CK_Building_ID
                     , value.var = c("Percent", "SE", "Count", "n"))

names(item44.os.cast)
if(os.ind == "scl"){
  item44.os.table <- data.frame("Heating.Fuel"          = item44.os.cast$Heating_Fuel
                                ,"Percent_SCL.GenPop"   = item44.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item44.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item44.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item44.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item44.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item44.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item44.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item44.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item44.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item44.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item44.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item44.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item44.os.table <- data.frame("Heating.Fuel"          = item44.os.cast$Heating_Fuel
                                ,"Percent_SnoPUD"          = item44.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item44.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item44.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item44.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item44.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item44.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item44.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item44.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item44.os.cast$`n_2017 RBSA NW`)
}



exportTable(item44.os.table, "SF", "Table 51", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
#Item 45: DISTRIBUTION OF SECONDARY HEATING SYSTEMS (SF table 52)
#############################################################################################
item45.os.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item45.os.dat1 <- item45.os.dat[which(item45.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item45.os.dat2 <- item45.os.dat1[which(item45.os.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(item45.os.dat2$Primary.Heating.System)

item45.os.dat2$Heating.System.Ind <- item45.os.dat2$Primary.Heating.System
item45.os.dat2$Heating.System.Ind[which(item45.os.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item45.os.dat2$Heating.System.Ind[which(item45.os.dat2$Primary.Heating.System ==  "No")] <- "Secondary Heating System"


for (ii in 1:nrow(item43.os.dat2)){
  # if (item43.os.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
  #   item43.os.dat2$Generic[ii] <- item43.os.dat2$`System.Sub-Type`[ii]
  # }
  if (item43.os.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater")){
    item43.os.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (item43.os.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater")){
    item43.os.dat2$Generic[ii] <- "Plug-In Heaters"
  }
}

item45.os.dat2$Generic[grep("Electric Baseboard",item45.os.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item45.os.dat2$Generic[grep("zonal heat",item45.os.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
item45.os.dat2$Generic[grep("ductless",item45.os.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
item45.os.dat2$Generic[grep("furnace",item45.os.dat2$Generic,ignore.case = T)] <- "Furnace"
item45.os.dat2$Generic[grep("boiler",item45.os.dat2$Generic,ignore.case = T)] <- "Boiler"
item45.os.dat2$Generic[grep("Stove/Fireplace",item45.os.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"

unique(item43.os.dat2$Generic)

item45.os.dat3 <- unique(data.frame("CK_Cadmus_ID" = item45.os.dat2$CK_Cadmus_ID
                                 ,"Heating_Type" = item45.os.dat2$Generic
                                 ,"Primary_Secondary" = item45.os.dat2$Heating.System.Ind))
item45.os.dat3 <- item45.os.dat3[which(item45.os.dat3$Heating_Type %notin% c("N/A",NA)),]

item45.os.dat4 <- left_join(os.dat, item45.os.dat3, by = "CK_Cadmus_ID")

item45.os.dat5 <- item45.os.dat4[which(item45.os.dat4$Primary_Secondary == "Secondary Heating System"),]
length(unique(item45.os.dat5$CK_Cadmus_ID))
item45.os.dat5$count <- 1


item45.os.data <- weightedData(item45.os.dat5[-which(colnames(item45.os.dat5) %in% c("Heating_Type"
                                                                            ,"Primary_Secondary"
                                                                            ,"count"))])

item45.os.data <- left_join(item45.os.data, item45.os.dat5[which(colnames(item45.os.dat5) %in% c("CK_Cadmus_ID"
                                                                                     ,"Heating_Type"
                                                                                     ,"Primary_Secondary"
                                                                                     ,"count"))])

################################
# Weighted Analysis
################################
item45.os.final <- proportionRowsAndColumns1(CustomerLevelData  = item45.os.data
                                             , valueVariable    = 'count'
                                             ,columnVariable = "CK_Building_ID"
                                             , rowVariable = 'Heating_Type'
                                             ,aggregateColumnName = "Remove")
item45.os.final <- item45.os.final[which(item45.os.final$CK_Building_ID != "Remove"),]

item45.os.cast <- dcast(setDT(item45.os.final)
                        ,formula = Heating_Type ~ CK_Building_ID
                        ,value.var = c("w.percent","w.SE","count","n","N","EB"))

names(item45.os.cast)
if(os.ind == "scl"){
  item45.os.table <- data.frame("Heating_Type"         = item45.os.cast$Heating_Type
                                ,"Percent_SCL.GenPop"   = item45.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item45.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item45.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item45.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item45.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item45.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item45.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item45.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item45.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item45.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item45.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item45.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item45.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item45.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item45.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item45.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item45.os.table <- data.frame("Heating_Type"         = item45.os.cast$Heating_Type
                                ,"Percent_SnoPUD"          = item45.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item45.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item45.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item45.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item45.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item45.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item45.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item45.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item45.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item45.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item45.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item45.os.cast$`EB_2017 RBSA NW`)
}



exportTable(item45.os.table, "SF", "Table 52", weighted = TRUE, osIndicator = export.ind, OS = T)

#########################
# unWeighted Analysis
#########################
item45.os.final <- proportions_two_groups_unweighted(CustomerLevelData  = item45.os.data
                                                     , valueVariable    = 'count'
                                                     ,columnVariable = "CK_Building_ID"
                                                     , rowVariable = 'Heating_Type'
                                                     ,aggregateColumnName = "Remove")
item45.os.final <- item45.os.final[which(item45.os.final$CK_Building_ID != "Remove"),]

item45.os.cast <- dcast(setDT(item45.os.final)
                        ,formula = Heating_Type ~ CK_Building_ID
                        ,value.var = c("Percent","SE","Count","n"))

names(item45.os.cast)
if(os.ind == "scl"){
  item45.os.table <- data.frame("Heating_Type"         = item45.os.cast$Heating_Type
                                ,"Percent_SCL.GenPop"   = item45.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item45.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item45.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item45.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item45.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item45.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item45.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item45.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item45.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item45.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item45.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item45.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item45.os.table <- data.frame("Heating_Type"         = item45.os.cast$Heating_Type
                                ,"Percent_SnoPUD"          = item45.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item45.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item45.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item45.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item45.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item45.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item45.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item45.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item45.os.cast$`n_2017 RBSA NW`)
}




exportTable(item45.os.table, "SF", "Table 52", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
#Item 46: DISTRIBUTION OF FUEL CHOICE FOR SECONDARY HEATING SYSTEMS BY CK_Building_ID  (SF table 51)
#############################################################################################
item46.os.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
item46.os.dat1 <- item46.os.dat[which(item46.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item46.os.dat2 <- item46.os.dat1[which(item46.os.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(item46.os.dat2$Primary.Heating.System)

item46.os.dat2$Heating.System.Ind <- item46.os.dat2$Primary.Heating.System
item46.os.dat2$Heating.System.Ind[which(item46.os.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item46.os.dat2$Heating.System.Ind[which(item46.os.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"
item46.os.dat2$Heating.Fuel[which(item46.os.dat2$Heating.Fuel == "Natural gas")]       <- "Natural Gas"
item46.os.dat2$Heating.Fuel[which(item46.os.dat2$Heating.Fuel == "Natural Gas")]       <- "Gas"
item46.os.dat2$Heating.Fuel[which(item46.os.dat2$Heating.Fuel == "Fuel oil/kerosene")] <- "Oil"

for (ii in 1:nrow(item46.os.dat2)){
  # if (item46.os.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
  #   item46.os.dat2$Generic[ii] <- item46.os.dat2$`System.Sub-Type`[ii]
  # }
  if (item46.os.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater")){
    item46.os.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
}

item46.os.dat2$Generic[grep("Electric Baseboard",item46.os.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item46.os.dat2$Generic[grep("zonal heat",item46.os.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
item46.os.dat2$Generic[grep("ductless",item46.os.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
item46.os.dat2$Generic[grep("furnace",item46.os.dat2$Generic,ignore.case = T)] <- "Furnace"
item46.os.dat2$Generic[grep("boiler",item46.os.dat2$Generic,ignore.case = T)] <- "Boiler"
item46.os.dat2$Generic[grep("Stove/Fireplace",item46.os.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"

item46.os.dat3 <- unique(data.frame("CK_Cadmus_ID" = item46.os.dat2$CK_Cadmus_ID
                                 ,"Heating_Type" = item46.os.dat2$Generic
                                 ,"Heating_Fuel" = item46.os.dat2$Heating.Fuel
                                 ,"Primary_Secondary" = item46.os.dat2$Heating.System.Ind))

item46.os.dat3 <- item46.os.dat3[which(item46.os.dat3$Heating_Type %notin% c("N/A",NA)),]
# Check heating type
unique(item46.os.dat3$Heating_Type)

item46.os.dat4 <- left_join(os.dat, item46.os.dat3, by = "CK_Cadmus_ID")

item46.os.dat5 <- item46.os.dat4[which(item46.os.dat4$Primary_Secondary == "Secondary Heating System"),]
length(unique(item46.os.dat5$CK_Cadmus_ID))
item46.os.dat5$count <- 1

# Remove entries with missing fuel types
unique(item46.os.dat5$Heating_Fuel)
item46.os.dat6 <- item46.os.dat5 %>%
  filter(Heating_Fuel %notin% c("N/A",NA))
item46.os.dat7 <- item46.os.dat6 %>%
  filter(Heating_Fuel %notin% c("Unknown"
                                , "Can't Determine"
                                , "Hydronic Gas-Water Fan Heater"
                                , "Hot Water from Water Heater"
                                , "Other"))


item46.os.data <- weightedData(item46.os.dat7[-which(colnames(item46.os.dat7) %in% c("Heating_Type"
                                                                            ,"Heating_Fuel"
                                                                            ,"Primary_Secondary"
                                                                            ,"count"))])

item46.os.data <- left_join(item46.os.data, item46.os.dat7[which(colnames(item46.os.dat7) %in% c("CK_Cadmus_ID"
                                                                                     ,"Heating_Type"
                                                                                     ,"Heating_Fuel"
                                                                                     ,"Primary_Secondary"
                                                                                     ,"count"))])

################################
# weighted Analysis
################################
item46.os.final <- proportionRowsAndColumns1(CustomerLevelData = item46.os.data
                                          , valueVariable       = 'count'
                                          , columnVariable      = 'CK_Building_ID'
                                          , rowVariable         = 'Heating_Fuel'
                                          , aggregateColumnName = 'Region')

item46.os.cast <- dcast(setDT(item46.os.final)
                     , formula = BuildingType + Heating_Fuel ~ CK_Building_ID
                     , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

names(item46.os.cast)
if(os.ind == "scl"){
  item46.os.table <- data.frame("Heating.Fuel"          = item46.os.cast$Heating_Fuel
                                ,"Percent_SCL.GenPop"   = item46.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item46.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item46.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item46.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item46.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item46.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item46.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item46.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item46.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item46.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item46.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item46.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item46.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item46.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item46.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item46.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item46.os.table <- data.frame("Heating.Fuel"          = item46.os.cast$Heating_Fuel
                                ,"Percent_SnoPUD"          = item46.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item46.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item46.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item46.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item46.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item46.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item46.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item46.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item46.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item46.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item46.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item46.os.cast$`EB_2017 RBSA NW`)
}



exportTable(item46.os.table, "SF", "Table 53", weighted = TRUE, osIndicator = export.ind, OS = T)


################################
# Unweighted Analysis
################################
item46.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item46.os.data
                                                  , valueVariable       = 'count'
                                                  , columnVariable      = 'CK_Building_ID'
                                                  , rowVariable         = 'Heating_Fuel'
                                                  , aggregateColumnName = 'Region')

item46.os.cast <- dcast(setDT(item46.os.final)
                     , formula = BuildingType + Heating_Fuel ~ CK_Building_ID
                     , value.var = c("Percent", "SE", "Count", "n"))

names(item46.os.cast)
if(os.ind == "scl"){
  item46.os.table <- data.frame("Heating.Fuel"          = item46.os.cast$Heating_Fuel
                                ,"Percent_SCL.GenPop"   = item46.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item46.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item46.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item46.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item46.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item46.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item46.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item46.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item46.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item46.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item46.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item46.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item46.os.table <- data.frame("Heating.Fuel"          = item46.os.cast$Heating_Fuel
                                ,"Percent_SnoPUD"          = item46.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item46.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item46.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item46.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item46.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item46.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item46.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item46.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item46.os.cast$`n_2017 RBSA NW`)
}



exportTable(item46.os.table, "SF", "Table 53", weighted = FALSE, osIndicator = export.ind, OS = T)
