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
# download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx'
#               , mechanical.export
#               , mode = 'wb')
mechanical.dat <- read.xlsx(mechanical.export)
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"Primary.Heating.System"
                                                                        ,"Heating.Fuel"
                                                                        ,"System.Sub-Type"))]


#Read in data for analysis
survey.dat <- data.frame(read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Labeled and Translated"),stringsAsFactors = F)
#clean cadmus IDs
survey.dat$CK_Cadmus_ID <- trimws(toupper(survey.dat$NEXID))
survey.dat <- survey.dat[which(names(survey.dat) %in% c("CK_Cadmus_ID"
                                                        ,"What.is.the.highest.level.of.school.that.someone.in.your.home.has.completed."
                                                        ,"In.2015..was.your.annual.household.income.before.taxes.above.or.below..50.000."
                                                        ,"Was.your.annual.household.income.before.taxes.above.or.below..25.000."
                                                        ,"Was.it....0.to.under..25.000"
                                                        ,"Was.it....25.000.to.under..50.000"
                                                        ,"Was.it....50.000.or.more"))]
names(survey.dat) <- c("Education.Level","Income.Above.or.Below.50000","Income.Above.or.Below.25000","Income.0.to.25000","Income.25000.to.50000","Income.More.Than.50000","CK_Cadmus_ID")
survey.dat$Income.Level <- survey.dat$Income.More.Than.50000

ii = 2
for (ii in 1:nrow(survey.dat)){
  if(survey.dat$Income.More.Than.50000[ii] != "N/A"){
    survey.dat$Income.Level[ii] <- survey.dat$Income.More.Than.50000[ii]
  }
  if(survey.dat$Income.25000.to.50000[ii] != "N/A"){
    survey.dat$Income.Level[ii] <- survey.dat$Income.25000.to.50000[ii]
  }
  if(survey.dat$Income.0.to.25000[ii] != "N/A"){
    survey.dat$Income.Level[ii] <- survey.dat$Income.0.to.25000[ii]
  }
  if(survey.dat$Income.Level[ii] %in% c("N/A","Unknown", "Prefer not to say")){
    survey.dat$Income.Level[ii] <- survey.dat$Income.Above.or.Below.50000[ii]
  }
  if(survey.dat$Income.Level[ii] %in% c("N/A","Unknown", "Prefer not to say")){
    survey.dat$Income.Level[ii] <- survey.dat$Income.Above.or.Below.25000[ii]
  }
}
unique(survey.dat$Income.Level)
survey.dat$Income.Level[which(survey.dat$Income.Level == "Exactly $50,000")] <- "$50,000 to under $55,000"
survey.dat <- survey.dat[which(names(survey.dat) %in% c("CK_Cadmus_ID", "Education.Level", "Income.Level"))]


#############################################################################################
#query6: DISTRIBUTION OF GAS PRIMARY HEATING SYSTEMS (SF table 50, MF table 35)
#############################################################################################
query6.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
query6.dat1 <- query6.dat[which(query6.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
query6.dat2 <- query6.dat1[which(query6.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(query6.dat2$Primary.Heating.System)
unique(query6.dat2$Generic)


query6.dat2$Heating.System.Ind <- query6.dat2$Primary.Heating.System
query6.dat2$Heating.System.Ind[which(query6.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
query6.dat2$Heating.System.Ind[which(query6.dat2$Primary.Heating.System == "No")]  <- "Secondary Heating System"

query6.dat2$`System.Sub-Type`[grep("plug-in|plug in", query6.dat2$`System.Sub-Type`, ignore.case = T)]

for (ii in 1:nrow(query6.dat2)){
  # if (query6.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
  #   query6.dat2$Generic[ii] <- query6.dat2$`System.Sub-Type`[ii]
  # }
  if (query6.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater", "Vertical Wall Heater")){
    query6.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (query6.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater", "Electric Plug In Heater", "Electric Plug-In Heater", "Plug In Heater")){
    query6.dat2$Generic[ii] <- "Plug-In Heaters"
  }
}

query6.dat2$Generic[grep("Baseboard",query6.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
query6.dat2$Generic[grep("zonal heat",query6.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
query6.dat2$Generic[grep("ductless",query6.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
query6.dat2$Generic[grep("furnace",query6.dat2$Generic,ignore.case = T)] <- "Furnace"
query6.dat2$Generic[grep("boiler",query6.dat2$Generic,ignore.case = T)] <- "Boiler"
query6.dat2$Generic[grep("Stove/Fireplace",query6.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"

unique(query6.dat2$Generic)

query6.dat3 <- unique(data.frame("CK_Cadmus_ID" = query6.dat2$CK_Cadmus_ID
                                 ,"Heating_Type"       = query6.dat2$Generic
                                 ,"Primary_Secondary"  = query6.dat2$Heating.System.Ind
                                 ,"Heating.Fuel"       = query6.dat2$Heating.Fuel))
### Clean heating type
unique(query6.dat3$Heating_Type)
# query6.dat3$Heating_Type[grep("geo", query6.dat3$Heating_Type, ignore.case = T)] <- "Geothermal Heat Pump"

query6.dat4 <- left_join(rbsa.dat, query6.dat3, by = "CK_Cadmus_ID")
unique(query6.dat4$Primary_Secondary)
query6.dat5 <- unique(query6.dat4[which(query6.dat4$Primary_Secondary == "Primary Heating System"),])
unique(query6.dat5$Primary_Secondary)


length(unique(query6.dat5$CK_Cadmus_ID[which(query6.dat5$BuildingType == "Single Family")]))
nrow(query6.dat5[which(query6.dat5$BuildingType == "Single Family"),])
dup.ids <- query6.dat5$CK_Cadmus_ID[which(duplicated(query6.dat5$CK_Cadmus_ID) & query6.dat5$BuildingType == "Single Family")]
query6.dat5$count <- 1

query6.duplicates <- query6.dat5[which(query6.dat5$CK_Cadmus_ID %in% dup.ids),]

query6.dat6 <- query6.dat5[which(query6.dat5$Heating_Type %notin% c("N/A",NA)),]
unique(query6.dat6$Heating_Type)

unique(query6.dat6$Heating.Fuel)
query6.dat7 <- query6.dat6[which(query6.dat6$Heating.Fuel == "Natural Gas"),]

query6.data <- weightedData(query6.dat7[-which(colnames(query6.dat7) %in% c("Heating_Type"
                                                                            ,"Primary_Secondary"
                                                                            ,"Heating.Fuel"
                                                                            ,"count"))])
query6.data <- left_join(query6.data, unique(query6.dat7[which(colnames(query6.dat7) %in% c("CK_Cadmus_ID"
                                                                                     ,"Heating_Type"
                                                                                     ,"Primary_Secondary"
                                                                                     ,"Heating.Fuel"
                                                                                     ,"count"))]))

#########################
# Weighted Analysis
#########################
query6.final <- proportions_one_group(CustomerLevelData  = query6.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'HomeYearBuilt_bins2'
                                      , total.name       = "All Vintages"
                                      , weighted = TRUE)

# export table
# SF = Table 50, MH = Table 32
query6.final.SF <- query6.final[which(query6.final$BuildingType == "Single Family")
                                ,-which(colnames(query6.final) %in% c("BuildingType"))]
query6.final.MH <- query6.final[which(query6.final$BuildingType == "Manufactured")
                                ,-which(colnames(query6.final) %in% c("BuildingType"))]

# exportTable(query6.final.SF, "SF", "Table ", weighted = TRUE)
# exportTable(query6.final.MH, "MH", "Table ", weighted = TRUE)

#########################
# unWeighted Analysis
#########################
query6.final <- proportions_one_group(CustomerLevelData  = query6.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'HomeYearBuilt_bins2'
                                      , total.name       = "All Vintages"
                                      , weighted = FALSE)

# export table
# SF = Table 50, MH = Table 32
query6.final.SF <- query6.final[which(query6.final$BuildingType == "Single Family")
                                ,-which(colnames(query6.final) %in% c("BuildingType"))]
query6.final.MH <- query6.final[which(query6.final$BuildingType == "Manufactured")
                                ,-which(colnames(query6.final) %in% c("BuildingType"))]

# exportTable(query6.final.SF, "SF", "Table ", weighted = FALSE)
# exportTable(query6.final.MH, "MH", "Table ", weighted = FALSE)








#############################################################################################
#query7: DISTRIBUTION OF GAS PRIMARY HEATING SYSTEMS (SF table 50, MF table 35)
#############################################################################################
query7.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
query7.dat1 <- query7.dat[which(query7.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
query7.dat2 <- query7.dat1[which(query7.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(query7.dat2$Primary.Heating.System)
unique(query7.dat2$Generic)


query7.dat2$Heating.System.Ind <- query7.dat2$Primary.Heating.System
query7.dat2$Heating.System.Ind[which(query7.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
query7.dat2$Heating.System.Ind[which(query7.dat2$Primary.Heating.System == "No")]  <- "Secondary Heating System"

query7.dat2$`System.Sub-Type`[grep("plug-in|plug in", query7.dat2$`System.Sub-Type`, ignore.case = T)]

for (ii in 1:nrow(query7.dat2)){
  # if (query7.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
  #   query7.dat2$Generic[ii] <- query7.dat2$`System.Sub-Type`[ii]
  # }
  if (query7.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater", "Vertical Wall Heater")){
    query7.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (query7.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater", "Electric Plug In Heater", "Electric Plug-In Heater", "Plug In Heater")){
    query7.dat2$Generic[ii] <- "Plug-In Heaters"
  }
}

query7.dat2$Generic[grep("Baseboard",query7.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
query7.dat2$Generic[grep("zonal heat",query7.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
query7.dat2$Generic[grep("ductless",query7.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
query7.dat2$Generic[grep("furnace",query7.dat2$Generic,ignore.case = T)] <- "Furnace"
query7.dat2$Generic[grep("boiler",query7.dat2$Generic,ignore.case = T)] <- "Boiler"
query7.dat2$Generic[grep("Stove/Fireplace",query7.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"

unique(query7.dat2$Generic)

query7.dat3 <- unique(data.frame("CK_Cadmus_ID" = query7.dat2$CK_Cadmus_ID
                                 ,"Heating_Type"       = query7.dat2$Generic
                                 ,"Primary_Secondary"  = query7.dat2$Heating.System.Ind
                                 ,"Heating.Fuel"       = query7.dat2$Heating.Fuel))
### Clean heating type
unique(query7.dat3$Heating_Type)
# query7.dat3$Heating_Type[grep("geo", query7.dat3$Heating_Type, ignore.case = T)] <- "Geothermal Heat Pump"

query7.dat4 <- left_join(rbsa.dat, query7.dat3, by = "CK_Cadmus_ID")
unique(query7.dat4$Primary_Secondary)
query7.dat5 <- unique(query7.dat4[which(query7.dat4$Primary_Secondary == "Primary Heating System"),])
unique(query7.dat5$Primary_Secondary)


length(unique(query7.dat5$CK_Cadmus_ID[which(query7.dat5$BuildingType == "Single Family")]))
nrow(query7.dat5[which(query7.dat5$BuildingType == "Single Family"),])
dup.ids <- query7.dat5$CK_Cadmus_ID[which(duplicated(query7.dat5$CK_Cadmus_ID) & query7.dat5$BuildingType == "Single Family")]
query7.dat5$count <- 1

query7.duplicates <- query7.dat5[which(query7.dat5$CK_Cadmus_ID %in% dup.ids),]

query7.dat6 <- query7.dat5[which(query7.dat5$Heating_Type %notin% c("N/A",NA)),]
unique(query7.dat6$Heating_Type)

unique(query7.dat6$Heating.Fuel)
query7.dat7 <- query7.dat6[which(query7.dat6$Heating.Fuel == "Wood (Cord)"),]

query7.data <- weightedData(query7.dat7[-which(colnames(query7.dat7) %in% c("Heating_Type"
                                                                            ,"Primary_Secondary"
                                                                            ,"Heating.Fuel"
                                                                            ,"count"))])
query7.data <- left_join(query7.data, unique(query7.dat7[which(colnames(query7.dat7) %in% c("CK_Cadmus_ID"
                                                                                            ,"Heating_Type"
                                                                                            ,"Primary_Secondary"
                                                                                            ,"Heating.Fuel"
                                                                                            ,"count"))]))

#########################
# Weighted Analysis
#########################
query7.final <- proportions_one_group(CustomerLevelData  = query7.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'HomeYearBuilt_bins2'
                                      , total.name       = "All Vintages"
                                      , weighted = TRUE)

# export table
# SF = Table 50, MH = Table 32
query7.final.SF <- query7.final[which(query7.final$BuildingType == "Single Family")
                                ,-which(colnames(query7.final) %in% c("BuildingType"))]
query7.final.MH <- query7.final[which(query7.final$BuildingType == "Manufactured")
                                ,-which(colnames(query7.final) %in% c("BuildingType"))]

# exportTable(query7.final.SF, "SF", "Table ", weighted = TRUE)
# exportTable(query7.final.MH, "MH", "Table ", weighted = TRUE)

#########################
# unWeighted Analysis
#########################
query7.final <- proportions_one_group(CustomerLevelData  = query7.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'HomeYearBuilt_bins2'
                                      , total.name       = "All Vintages"
                                      , weighted = FALSE)

# export table
# SF = Table 50, MH = Table 32
query7.final.SF <- query7.final[which(query7.final$BuildingType == "Single Family")
                                ,-which(colnames(query7.final) %in% c("BuildingType"))]
query7.final.MH <- query7.final[which(query7.final$BuildingType == "Manufactured")
                                ,-which(colnames(query7.final) %in% c("BuildingType"))]

# exportTable(query7.final.SF, "SF", "Table ", weighted = FALSE)
# exportTable(query7.final.MH, "MH", "Table ", weighted = FALSE)





################################################################################################
# query8 (and item 190 for MH): PERCENTAGE OF HOMES WITH COOLING EQUIPMENT BY COOLING ZONE AND STATE (SF table 61)
################################################################################################
#subset to columns needed for analysis
query8.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Provides"
                                                                   ,"Generic"))]
query8.dat1 <- unique(query8.dat[grep("cooling",query8.dat$Provides, ignore.case = T),])
which(duplicated(query8.dat1$CK_Cadmus_ID))
query8.dat1.1 <- query8.dat1[-which(duplicated(query8.dat1$CK_Cadmus_ID)),]

query8.dat2 <- left_join(rbsa.dat, query8.dat1.1)
query8.dat2 <- query8.dat2[which(!is.na(query8.dat2$HomeYearBuilt)),]

query8.dat2$Ind <- 0
query8.dat2$Ind[which(!is.na(query8.dat2$Provides))] <- 1
unique(query8.dat2$Ind)

unique(query8.dat2$Generic)
query8.dat2 <- query8.dat2[-grep("mini|package|evaporative",query8.dat2$Generic, ignore.case = T),]
##########################################
# add pop and sample sizes by strata
##########################################
query8.data <- weightedData(query8.dat2[-which(colnames(query8.dat2) %in% c("Provides"
                                                                            ,"Ind"
                                                                            ,"Generic"))])
query8.data <- left_join(query8.data, query8.dat2[which(colnames(query8.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"Provides"
                                                                                     ,"Ind"
                                                                                     ,"Generic"))])
query8.data$Count <- 1


##############################
# Weighted Analysis
##############################
query8.data$HomeYearBuilt_bins_percentage <- query8.data$HomeYearBuilt_bins2
query8.table <- proportions_one_group(CustomerLevelData = query8.data
                                      ,valueVariable = 'Ind'
                                      ,groupingVariable = 'HomeYearBuilt_bins_percentage'
                                      ,weighted = TRUE)

query8.table.SF <- query8.table[which(query8.table$BuildingType == "Single Family")
                                ,which(colnames(query8.table) %notin% c("BuildingType"))]
query8.table.MH <- query8.table[which(query8.table$BuildingType == "Manufactured")
                                ,which(colnames(query8.table) %notin% c("BuildingType"))]

# exportTable(query8.table.SF, "SF", "Table ", weighted = TRUE)
# exportTable(query8.table.MH, "MH", "Table ", weighted = TRUE)

##############################
# unweighted Analysis
##############################
query8.data$HomeYearBuilt_bins_percentage <- query8.data$HomeYearBuilt_bins2
query8.table <- proportions_one_group(CustomerLevelData = query8.data
                                      ,valueVariable = 'Ind'
                                      ,groupingVariable = 'HomeYearBuilt_bins_percentage'
                                      ,weighted = FALSE)

query8.table.SF <- query8.table[which(query8.table$BuildingType == "Single Family")
                                ,which(colnames(query8.table) %notin% c("BuildingType"))]
query8.table.MH <- query8.table[which(query8.table$BuildingType == "Manufactured")
                                ,which(colnames(query8.table) %notin% c("BuildingType"))]

# exportTable(query8.table.SF, "SF", "Table ", weighted = FALSE)
# exportTable(query8.table.MH, "MH", "Table ", weighted = FALSE)





################################################################################################
# query9
################################################################################################
query9.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
query9.dat1 <- query9.dat[which(query9.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
query9.dat2 <- query9.dat1[which(query9.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(query9.dat2$Primary.Heating.System)
unique(query9.dat2$Generic)


query9.dat2$Heating.System.Ind <- query9.dat2$Primary.Heating.System
query9.dat2$Heating.System.Ind[which(query9.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
query9.dat2$Heating.System.Ind[which(query9.dat2$Primary.Heating.System == "No")]  <- "Secondary Heating System"

query9.dat2$`System.Sub-Type`[grep("plug-in|plug in", query9.dat2$`System.Sub-Type`, ignore.case = T)]

for (ii in 1:nrow(query9.dat2)){
  # if (query9.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
  #   query9.dat2$Generic[ii] <- query9.dat2$`System.Sub-Type`[ii]
  # }
  if (query9.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater", "Vertical Wall Heater")){
    query9.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (query9.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater", "Electric Plug In Heater", "Electric Plug-In Heater", "Plug In Heater")){
    query9.dat2$Generic[ii] <- "Plug-In Heaters"
  }
}

query9.dat2$Generic[grep("Baseboard",query9.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
query9.dat2$Generic[grep("zonal heat",query9.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
query9.dat2$Generic[grep("ductless",query9.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
query9.dat2$Generic[grep("furnace",query9.dat2$Generic,ignore.case = T)] <- "Furnace"
query9.dat2$Generic[grep("boiler",query9.dat2$Generic,ignore.case = T)] <- "Boiler"
query9.dat2$Generic[grep("Stove/Fireplace",query9.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"

unique(query9.dat2$Generic)

query9.dat3 <- unique(data.frame("CK_Cadmus_ID" = query9.dat2$CK_Cadmus_ID
                                 ,"Heating_Type"       = query9.dat2$Generic
                                 ,"Primary_Secondary"  = query9.dat2$Heating.System.Ind
                                 ,"Heating.Fuel"       = query9.dat2$Heating.Fuel))
### Clean heating type
unique(query9.dat3$Heating_Type)
# query9.dat3$Heating_Type[grep("geo", query9.dat3$Heating_Type, ignore.case = T)] <- "Geothermal Heat Pump"

query9.dat4 <- left_join(rbsa.dat, query9.dat3, by = "CK_Cadmus_ID")
unique(query9.dat4$Primary_Secondary)
query9.dat5 <- unique(query9.dat4[which(!is.na(query9.dat4$Primary_Secondary)),])
unique(query9.dat5$Primary_Secondary)


length(unique(query9.dat5$CK_Cadmus_ID[which(query9.dat5$BuildingType == "Single Family")]))
nrow(query9.dat5[which(query9.dat5$BuildingType == "Single Family"),])
dup.ids <- query9.dat5$CK_Cadmus_ID[which(duplicated(query9.dat5$CK_Cadmus_ID) & query9.dat5$BuildingType == "Single Family")]
query9.dat5$count <- 1

query9.duplicates <- query9.dat5[which(query9.dat5$CK_Cadmus_ID %in% dup.ids),]

query9.dat6 <- query9.dat5[which(query9.dat5$Heating_Type %notin% c("N/A",NA)),]
unique(query9.dat6$Heating_Type)

query9.dat <- query9.dat6[grep("mini split|mini-split",query9.dat6$Heating_Type, ignore.case = T),]
query9.dat <- query9.dat[which(!is.na(query9.dat$HomeYearBuilt)),]
##########################################
# add pop and sample sizes by strata
##########################################
query9.data <- weightedData(query9.dat[-which(colnames(query9.dat) %in% c("count"
                                                                          ,"Heating_Type"
                                                                          ,"Primary_Secondary"
                                                                          ,"Heating.Fuel"))])
query9.data <- left_join(query9.data, query9.dat[which(colnames(query9.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"count"
                                                                                   ,"Heating_Type"
                                                                                   ,"Primary_Secondary"
                                                                                   ,"Heating.Fuel"))])
query9.data$Count <- 1


##############################
# Weighted Analysis
##############################
query9.table <- proportions_one_group(CustomerLevelData = query9.data
                                      ,valueVariable = 'count'
                                      ,groupingVariable = 'HomeYearBuilt_bins2'
                                      ,weighted = TRUE)

query9.table.SF <- query9.table[which(query9.table$BuildingType == "Single Family")
                                ,which(colnames(query9.table) %notin% c("BuildingType"))]
query9.table.MH <- query9.table[which(query9.table$BuildingType == "Manufactured")
                                ,which(colnames(query9.table) %notin% c("BuildingType"))]

# exportTable(query9.table.SF, "SF", "Table ", weighted = TRUE)
# exportTable(query9.table.MH, "MH", "Table ", weighted = TRUE)

##############################
# unweighted Analysis
##############################
query9.table <- proportions_one_group(CustomerLevelData = query9.data
                                      ,valueVariable = 'count'
                                      ,groupingVariable = 'HomeYearBuilt_bins2'
                                      ,weighted = FALSE)

query9.table.SF <- query9.table[which(query9.table$BuildingType == "Single Family")
                                ,which(colnames(query9.table) %notin% c("BuildingType"))]
query9.table.MH <- query9.table[which(query9.table$BuildingType == "Manufactured")
                                ,which(colnames(query9.table) %notin% c("BuildingType"))]

# exportTable(query9.table.SF, "SF", "Table ", weighted = FALSE)
# exportTable(query9.table.MH, "MH", "Table ", weighted = FALSE)





################################################################################################
# query10
################################################################################################
##############################
# Weighted Analysis
##############################
query9.data$HomeType_dist <- query9.data$HomeType
query10.table <- proportions_one_group(CustomerLevelData = query9.data
                                      ,valueVariable = 'count'
                                      ,groupingVariable = 'HomeType_dist'
                                      ,weighted = TRUE)

query10.table.SF <- query10.table[which(query10.table$BuildingType == "Single Family")
                                ,which(colnames(query10.table) %notin% c("BuildingType"))]
query10.table.MH <- query10.table[which(query10.table$BuildingType == "Manufactured")
                                ,which(colnames(query10.table) %notin% c("BuildingType"))]

# exportTable(query10.table.SF, "SF", "Table ", weighted = TRUE)
# exportTable(query10.table.MH, "MH", "Table ", weighted = TRUE)

##############################
# unweighted Analysis
##############################
query10.table <- proportions_one_group(CustomerLevelData = query9.data
                                      ,valueVariable = 'count'
                                      ,groupingVariable = 'HomeType_dist'
                                      ,weighted = FALSE)

query10.table.SF <- query10.table[which(query10.table$BuildingType == "Single Family")
                                ,which(colnames(query10.table) %notin% c("BuildingType"))]
query10.table.MH <- query10.table[which(query10.table$BuildingType == "Manufactured")
                                ,which(colnames(query10.table) %notin% c("BuildingType"))]

# exportTable(query10.table.SF, "SF", "Table ", weighted = FALSE)
# exportTable(query10.table.MH, "MH", "Table ", weighted = FALSE)






################################################################################################
# query11
################################################################################################
query11.dat <- mechanical.dat1

#remove datapoint not asked for and repeated header lines
query11.dat1 <- query11.dat[which(query11.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
query11.dat2 <- query11.dat1[which(query11.dat1$Primary.Heating.System %in% c("Yes", "No")),]
#check uniques
unique(query11.dat2$Primary.Heating.System)
unique(query11.dat2$Generic)


query11.dat2$Heating.System.Ind <- query11.dat2$Primary.Heating.System
query11.dat2$Heating.System.Ind[which(query11.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
query11.dat2$Heating.System.Ind[which(query11.dat2$Primary.Heating.System == "No")]  <- "Secondary Heating System"

query11.dat2$`System.Sub-Type`[grep("plug-in|plug in", query11.dat2$`System.Sub-Type`, ignore.case = T)]

for (ii in 1:nrow(query11.dat2)){
  # if (query11.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
  #   query11.dat2$Generic[ii] <- query11.dat2$`System.Sub-Type`[ii]
  # }
  if (query11.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater", "Vertical Wall Heater")){
    query11.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (query11.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater", "Electric Plug In Heater", "Electric Plug-In Heater", "Plug In Heater")){
    query11.dat2$Generic[ii] <- "Plug-In Heaters"
  }
}

query11.dat2$Generic[grep("Baseboard",query11.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
query11.dat2$Generic[grep("zonal heat",query11.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
query11.dat2$Generic[grep("ductless",query11.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
query11.dat2$Generic[grep("furnace",query11.dat2$Generic,ignore.case = T)] <- "Furnace"
query11.dat2$Generic[grep("boiler",query11.dat2$Generic,ignore.case = T)] <- "Boiler"
query11.dat2$Generic[grep("Stove/Fireplace",query11.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"

unique(query11.dat2$Generic)

query11.dat3 <- unique(data.frame("CK_Cadmus_ID" = query11.dat2$CK_Cadmus_ID
                                 ,"Heating_Type"       = query11.dat2$Generic
                                 ,"Primary_Secondary"  = query11.dat2$Heating.System.Ind
                                 ,"Heating.Fuel"       = query11.dat2$Heating.Fuel))
### Clean heating type
unique(query11.dat3$Heating_Type)
# query11.dat3$Heating_Type[grep("geo", query11.dat3$Heating_Type, ignore.case = T)] <- "Geothermal Heat Pump"

query11.dat4 <- left_join(rbsa.dat, query11.dat3, by = "CK_Cadmus_ID")
unique(query11.dat4$Primary_Secondary)
query11.dat5 <- unique(query11.dat4[which(!is.na(query11.dat4$Primary_Secondary)),])
unique(query11.dat5$Primary_Secondary)


length(unique(query11.dat5$CK_Cadmus_ID[which(query11.dat5$BuildingType == "Single Family")]))
nrow(query11.dat5[which(query11.dat5$BuildingType == "Single Family"),])
dup.ids <- query11.dat5$CK_Cadmus_ID[which(duplicated(query11.dat5$CK_Cadmus_ID) & query11.dat5$BuildingType == "Single Family")]
query11.dat5$count <- 1

query11.duplicates <- query11.dat5[which(query11.dat5$CK_Cadmus_ID %in% dup.ids),]

query11.dat6 <- query11.dat5[which(query11.dat5$Heating_Type %notin% c("N/A",NA)),]
unique(query11.dat6$Heating_Type)

query11.dat <- query11.dat6[grep("mini split|mini-split",query11.dat6$Heating_Type, ignore.case = T),]
query11.dat <- query11.dat[which(query11.dat$Primary_Secondary == "Primary Heating System"),]
query11.dat <- query11.dat[which(!is.na(query11.dat$HomeYearBuilt)),]
minisplit.ind <- unique(query11.dat$CK_Cadmus_ID)


query11.dat7 <- query11.dat6[which(query11.dat6$CK_Cadmus_ID %in% minisplit.ind),]
query11.dat8 <- query11.dat7[which(query11.dat7$Primary_Secondary == "Secondary Heating System"),]

##########################################
# add pop and sample sizes by strata
##########################################
query11.data <- weightedData(query11.dat8[-which(colnames(query11.dat8) %in% c("count"
                                                                          ,"Heating_Type"
                                                                          ,"Primary_Secondary"
                                                                          ,"Heating.Fuel"))])
query11.data <- left_join(query11.data, query11.dat8[which(colnames(query11.dat8) %in% c("CK_Cadmus_ID"
                                                                                   ,"count"
                                                                                   ,"Heating_Type"
                                                                                   ,"Primary_Secondary"
                                                                                   ,"Heating.Fuel"))])
query11.data$Count <- 1


##############################
# Weighted Analysis
##############################
query11.table <- proportionRowsAndColumns1(CustomerLevelData = query11.data
                                           ,valueVariable = 'count'
                                           ,columnVariable = "Heating.Fuel"
                                           ,rowVariable = "Heating_Type"
                                           ,aggregateColumnName = "Total")
query11.table <- query11.table[which(query11.table$Heating.Fuel != "Total"),]

query11.cast <- data.frame(dcast(setDT(query11.table)
                      ,formula = BuildingType + Heating_Type ~ Heating.Fuel
                      ,value.var = c("w.percent","w.SE","n","N","EB")),stringsAsFactors = F)

query11.table.SF <- query11.cast[which(query11.cast$BuildingType == "Single Family")
                                ,which(colnames(query11.cast) %notin% c("BuildingType"))]
query11.table.MH <- query11.cast[which(query11.cast$BuildingType == "Manufactured")
                                ,which(colnames(query11.cast) %notin% c("BuildingType"))]

# exportTable(query11.table.SF, "SF", "Table ", weighted = TRUE)
# exportTable(query11.table.MH, "MH", "Table ", weighted = TRUE)

##############################
# unweighted Analysis
##############################
query11.table <- proportions_two_groups_unweighted(CustomerLevelData = query11.data
                                           ,valueVariable = 'count'
                                           ,columnVariable = "Heating.Fuel"
                                           ,rowVariable = "Heating_Type"
                                           ,aggregateColumnName = "Total")
query11.table <- query11.table[which(query11.table$Heating.Fuel != "Total"),]

query11.cast <- data.frame(dcast(setDT(query11.table)
                                 ,formula = BuildingType + Heating_Type ~ Heating.Fuel
                                 ,value.var = c("Percent","SE","n")),stringsAsFactors = F)

query11.table.SF <- query11.cast[which(query11.cast$BuildingType == "Single Family")
                                ,which(colnames(query11.cast) %notin% c("BuildingType"))]
query11.table.MH <- query11.cast[which(query11.cast$BuildingType == "Manufactured")
                                ,which(colnames(query11.cast) %notin% c("BuildingType"))]

# exportTable(query11.table.SF, "SF", "Table ", weighted = FALSE)
# exportTable(query11.table.MH, "MH", "Table ", weighted = FALSE)










################################################################################################
# query12
################################################################################################
#subset to columns needed for analysis
query12.dat <- left_join(rbsa.dat, survey.dat)
query12.dat1 <- query12.dat[which(!is.na(query12.dat$Income.Level)),]
query12.dat2 <- query12.dat1[which(query12.dat1$Income.Level %notin% c("Unknown", "Prefer not to say","N/A","Above $50,000","Below $50,000")),]

query12.mech <- query11.dat6[grep("wood|oil|propane|pellet", query11.dat6$Heating.Fuel, ignore.case = T),]
query12.mech <- query12.mech[which(query12.mech$Primary_Secondary == "Primary Heating System"),]

query12.merge <- left_join(query12.mech, query12.dat2)
query12.merge <- query12.merge[which(!is.na(query12.merge$Income.Level)),]

#merge together analysis data with cleaned RBSA data
query12.dat1 <- left_join(rbsa.dat, query12.merge)
query12.dat1 <- query12.dat1[which(!is.na(query12.dat1$Income.Level)),]


##########################################
# add pop and sample sizes by strata
##########################################
query12.data <- weightedData(query12.dat1[-which(colnames(query12.dat1) %in% c("count"
                                                                               ,"Heating_Type"
                                                                               ,"Primary_Secondary"
                                                                               ,"Heating.Fuel"
                                                                               ,"Income.Level"
                                                                               ,"Detailed.Income.Level"
                                                                               ,"System.Sub-Type"
                                                                               ,"Generic"
                                                                               ,"Heating_Type"
                                                                               ,"Education.Level"))])
query12.data <- left_join(query12.data, query12.dat1[which(colnames(query12.dat1) %in% c("CK_Cadmus_ID"
                                                                                         ,"count"
                                                                                         ,"Heating_Type"
                                                                                         ,"Primary_Secondary"
                                                                                         ,"Heating.Fuel"
                                                                                         ,"Income.Level"
                                                                                         ,"Detailed.Income.Level"
                                                                                         ,"System.Sub-Type"
                                                                                         ,"Generic"
                                                                                         ,"Heating_Type"
                                                                                         ,"Education.Level"))])
query12.data$Count <- 1


##############################
# Weighted Analysis
##############################
query12.table <- proportionRowsAndColumns1(CustomerLevelData = query12.data
                                           ,valueVariable = 'count'
                                           ,columnVariable = "Heating.Fuel"
                                           ,rowVariable = "Income.Level"
                                           ,aggregateColumnName = "Total")
query12.table <- query12.table[which(query12.table$Heating.Fuel != "Total"),]

query12.cast <- data.frame(dcast(setDT(query12.table)
                                 ,formula = BuildingType + Income.Level ~ Heating.Fuel
                                 ,value.var = c("w.percent","w.SE","n","N","EB")),stringsAsFactors = F)
query12.table <- data.frame(query12.cast,stringsAsFactors = F)

unique(query12.table$Income.Level)
rowOrder <- c("Under $5,000"
              ,"$5,000 to under $10,000"
              ,"$10,000 to under $15,000"
              ,"$15,000 to under $20,000"
              ,"$20,000 to under $25,000"
              ,"$25,000 to under $30,000"
              ,"$30,000 to under $35,000"
              ,"$35,000 to under $40,000"
              ,"$40,000 to under $45,000"
              ,"$45,000 to under $50,000"
              ,"$50,000 to under $55,000"
              ,"$55,000 to under $60,000"
              ,"$60,000 to under $65,000"
              ,"$65,000 to under $70,000"
              ,"$70,000 to under $75,000"
              ,"$75,000 to under $80,000"
              ,"$80,000 to under $85,000"
              ,"$85,000 to under $90,000"
              ,"$90,000 to under $95,000"
              ,"$95,000 to under $100,000"
              ,"$100,000 to under $150,000"
              ,"$150,000 to under $200,000"
              ,"$200,000 or more"
              ,"Total")
query12.table <- query12.table %>% mutate(Income.Level = factor(Income.Level, levels = rowOrder)) %>% arrange(Income.Level)  
query12.table <- data.frame(query12.table)

query12.table.SF <- query12.table[which(query12.table$BuildingType == "Single Family")
                                 ,which(colnames(query12.table) %notin% c("BuildingType"))]
query12.table.MH <- query12.table[which(query12.table$BuildingType == "Manufactured")
                                 ,which(colnames(query12.table) %notin% c("BuildingType"))]

# exportTable(query12.table.SF, "SF", "Table ", weighted = TRUE)
# exportTable(query12.table.MH, "MH", "Table ", weighted = TRUE)

##############################
# unweighted Analysis
##############################
query12.table <- proportions_two_groups_unweighted(CustomerLevelData = query12.data
                                           ,valueVariable = 'count'
                                           ,columnVariable = "Heating.Fuel"
                                           ,rowVariable = "Income.Level"
                                           ,aggregateColumnName = "Total")
query12.table <- query12.table[which(query12.table$Heating.Fuel != "Total"),]

query12.cast <- data.frame(dcast(setDT(query12.table)
                                 ,formula = BuildingType + Income.Level ~ Heating.Fuel
                                 ,value.var = c("Percent","SE","n")),stringsAsFactors = F)

query12.table <- data.frame(query12.cast,stringsAsFactors = F)

unique(query12.table$Income.Level)
rowOrder <- c("Under $5,000"
              ,"$5,000 to under $10,000"
              ,"$10,000 to under $15,000"
              ,"$15,000 to under $20,000"
              ,"$20,000 to under $25,000"
              ,"$25,000 to under $30,000"
              ,"$30,000 to under $35,000"
              ,"$35,000 to under $40,000"
              ,"$40,000 to under $45,000"
              ,"$45,000 to under $50,000"
              ,"$50,000 to under $55,000"
              ,"$55,000 to under $60,000"
              ,"$60,000 to under $65,000"
              ,"$65,000 to under $70,000"
              ,"$70,000 to under $75,000"
              ,"$75,000 to under $80,000"
              ,"$80,000 to under $85,000"
              ,"$85,000 to under $90,000"
              ,"$90,000 to under $95,000"
              ,"$95,000 to under $100,000"
              ,"$100,000 to under $150,000"
              ,"$150,000 to under $200,000"
              ,"$200,000 or more"
              ,"Total")
query12.table <- query12.table %>% mutate(Income.Level = factor(Income.Level, levels = rowOrder)) %>% arrange(Income.Level)  
query12.table <- data.frame(query12.table)

query12.table.SF <- query12.table[which(query12.table$BuildingType == "Single Family")
                                  ,which(colnames(query12.table) %notin% c("BuildingType"))]
query12.table.MH <- query12.table[which(query12.table$BuildingType == "Manufactured")
                                  ,which(colnames(query12.table) %notin% c("BuildingType"))]

# exportTable(query12.table.SF, "SF", "Table ", weighted = FALSE)
# exportTable(query12.table.MH, "MH", "Table ", weighted = FALSE)








#############################################################################################
#query13
#############################################################################################
#subset to columns needed for analysis
query13.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Type"
                                                                    ,"DHW.Technology"))]
query13.dat$count <- 1

query13.dat0 <- query13.dat[which(query13.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

query13.dat1 <- left_join(rbsa.dat, query13.dat0, by = "CK_Cadmus_ID")
query13.dat1 <- query13.dat1[grep("site",query13.dat1$CK_Building_ID,ignore.case = T),]
query13.dat2 <- query13.dat1[grep("heat pump",query13.dat1$DHW.Technology, ignore.case = T),]
query13.dat2$Detailed.Type <- paste(query13.dat2$DHW.Type, query13.dat2$DHW.Technology, sep = "-")
unique(query13.dat2$Detailed.Type)

query13.dat3 <- query13.dat2#[-grep("unknown",query13.dat2$Detailed.Type, ignore.case = T),]

################################################
# Adding pop and sample sizes for weights
################################################
query13.data <- weightedData(query13.dat3[-which(colnames(query13.dat3) %in% c("Generic"
                                                                               ,"DHW.Fuel"
                                                                               ,"count"
                                                                               ,"DHW.Type"
                                                                               ,"DHW.Technology"
                                                                               ,"Detailed.Type"))])
query13.data <- left_join(query13.data, query13.dat3[which(colnames(query13.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"Generic"
                                                                                         ,"DHW.Fuel"
                                                                                         ,"count"
                                                                                         ,"DHW.Type"
                                                                                         ,"DHW.Technology"
                                                                                         ,"Detailed.Type"))])

#######################
# Weighted Analysis
#######################
query13.final <- proportions_one_group(CustomerLevelData  = query13.data
                                       , valueVariable    = 'count'
                                       , groupingVariable = 'HomeYearBuilt_bins2'
                                       , total.name       = "Total")

# Export table
query13.final.SF <- query13.final[which(query13.final$BuildingType == "Single Family")
                                  ,-which(colnames(query13.final) %in% c("BuildingType"))]
query13.final.MH <- query13.final[which(query13.final$BuildingType == "Manufactured")
                                  ,-which(colnames(query13.final) %in% c("BuildingType"))]

# exportTable(query13.final.SF, "SF", "query13", weighted = TRUE)
# exportTable(query13.final.MH, "MH", "query13", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
query13.final <- proportions_one_group(CustomerLevelData  = query13.data
                                       , valueVariable    = 'count'
                                       , groupingVariable = 'HomeYearBuilt_bins2'
                                       , total.name       = "Total"
                                       , weighted = FALSE)

# Export table
query13.final.SF <- query13.final[which(query13.final$BuildingType == "Single Family")
                                  ,-which(colnames(query13.final) %in% c("BuildingType"))]
query13.final.MH <- query13.final[which(query13.final$BuildingType == "Manufactured")
                                  ,-which(colnames(query13.final) %in% c("BuildingType"))]

# exportTable(query13.final.SF, "SF", "query13", weighted = FALSE)
# exportTable(query13.final.MH, "MH", "query13", weighted = FALSE)






#############################################################################################
#query14
#############################################################################################
#######################
# Weighted Analysis
#######################
query14.final <- proportions_one_group(CustomerLevelData  = query13.data
                                       , valueVariable    = 'count'
                                       , groupingVariable = 'HomeType'
                                       , total.name       = "Total")

# Export table
query14.final.SF <- query14.final[which(query14.final$BuildingType == "Single Family")
                                  ,-which(colnames(query14.final) %in% c("BuildingType"))]
query14.final.MH <- query14.final[which(query14.final$BuildingType == "Manufactured")
                                  ,-which(colnames(query14.final) %in% c("BuildingType"))]

# exportTable(query14.final.SF, "SF", "query14", weighted = TRUE)
# exportTable(query14.final.MH, "MH", "query14", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
query13.data$Count <- 1
query14.final <- proportions_one_group(CustomerLevelData  = query13.data
                                       , valueVariable    = 'count'
                                       , groupingVariable = 'HomeType'
                                       , total.name       = "Total"
                                       , weighted = FALSE)

# Export table
query14.final.SF <- query14.final[which(query14.final$BuildingType == "Single Family")
                                  ,-which(colnames(query14.final) %in% c("BuildingType"))]
query14.final.MH <- query14.final[which(query14.final$BuildingType == "Manufactured")
                                  ,-which(colnames(query14.final) %in% c("BuildingType"))]

# exportTable(query14.final.SF, "SF", "query14", weighted = FALSE)
# exportTable(query14.final.MH, "MH", "query14", weighted = FALSE)










#############################################################################################
#query15
#############################################################################################
query15.dat <- left_join(query13.dat3,query12.dat2)
query15.dat1 <- query15.dat[which(!is.na(query15.dat$Income.Level)),]
################################################
# Adding pop and sample sizes for weights
################################################
query15.data <- weightedData(query15.dat1[-which(colnames(query15.dat1) %in% c("Generic"
                                                                               ,"DHW.Fuel"
                                                                               ,"count"
                                                                               ,"DHW.Type"
                                                                               ,"DHW.Technology"
                                                                               ,"Detailed.Type"
                                                                               ,"Income.Level"
                                                                               ,"Education.Level"
                                                                               ,"Detailed.Income.Level"))])
query15.data <- left_join(query15.data, query15.dat1[which(colnames(query15.dat1) %in% c("CK_Cadmus_ID"
                                                                                         ,"Generic"
                                                                                         ,"DHW.Fuel"
                                                                                         ,"count"
                                                                                         ,"DHW.Type"
                                                                                         ,"DHW.Technology"
                                                                                         ,"Detailed.Type"
                                                                                         ,"Income.Level"
                                                                                         ,"Detailed.Income.Level"
                                                                                         ,"Education.Level"))])

#######################
# Weighted Analysis
#######################
query15.final <- proportions_one_group(CustomerLevelData  = query15.data
                                       , valueVariable    = 'count'
                                       , groupingVariable = 'Income.Level'
                                       , total.name       = "Total")
unique(query15.final$Income.Level)
rowOrder <- c("Under $5,000"
              ,"$5,000 to under $10,000"
              ,"$10,000 to under $15,000"
              ,"$15,000 to under $20,000"
              ,"$20,000 to under $25,000"
              ,"$25,000 to under $30,000"
              ,"$30,000 to under $35,000"
              ,"$35,000 to under $40,000"
              ,"$40,000 to under $45,000"
              ,"$45,000 to under $50,000"
              ,"$50,000 to under $55,000"
              ,"$55,000 to under $60,000"
              ,"$60,000 to under $65,000"
              ,"$65,000 to under $70,000"
              ,"$70,000 to under $75,000"
              ,"$75,000 to under $80,000"
              ,"$80,000 to under $85,000"
              ,"$85,000 to under $90,000"
              ,"$90,000 to under $95,000"
              ,"$95,000 to under $100,000"
              ,"$100,000 to under $150,000"
              ,"$150,000 to under $200,000"
              ,"$200,000 or more"
              ,"Total")
query15.final <- query15.final %>% mutate(Income.Level = factor(Income.Level, levels = rowOrder)) %>% arrange(Income.Level)  
query15.final <- data.frame(query15.final)

# Export table
query15.final.SF <- query15.final[which(query15.final$BuildingType == "Single Family")
                                  ,-which(colnames(query15.final) %in% c("BuildingType"))]
query15.final.MH <- query15.final[which(query15.final$BuildingType == "Manufactured")
                                  ,-which(colnames(query15.final) %in% c("BuildingType"))]

# exportTable(query15.final.SF, "SF", "query15", weighted = TRUE)
# exportTable(query15.final.MH, "MH", "query15", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
query15.final <- proportions_one_group(CustomerLevelData  = query15.data
                                       , valueVariable    = 'count'
                                       , groupingVariable = 'Income.Level'
                                       , total.name       = "Total"
                                       , weighted = FALSE)
unique(query15.final$Income.Level)
rowOrder <- c("Under $5,000"
              ,"$5,000 to under $10,000"
              ,"$10,000 to under $15,000"
              ,"$15,000 to under $20,000"
              ,"$20,000 to under $25,000"
              ,"$25,000 to under $30,000"
              ,"$30,000 to under $35,000"
              ,"$35,000 to under $40,000"
              ,"$40,000 to under $45,000"
              ,"$45,000 to under $50,000"
              ,"$50,000 to under $55,000"
              ,"$55,000 to under $60,000"
              ,"$60,000 to under $65,000"
              ,"$65,000 to under $70,000"
              ,"$70,000 to under $75,000"
              ,"$75,000 to under $80,000"
              ,"$80,000 to under $85,000"
              ,"$85,000 to under $90,000"
              ,"$90,000 to under $95,000"
              ,"$95,000 to under $100,000"
              ,"$100,000 to under $150,000"
              ,"$150,000 to under $200,000"
              ,"$200,000 or more"
              ,"Total")
query15.final <- query15.final %>% mutate(Income.Level = factor(Income.Level, levels = rowOrder)) %>% arrange(Income.Level)  
query15.final <- data.frame(query15.final)

# Export table
query15.final.SF <- query15.final[which(query15.final$BuildingType == "Single Family")
                                  ,-which(colnames(query15.final) %in% c("BuildingType"))]
query15.final.MH <- query15.final[which(query15.final$BuildingType == "Manufactured")
                                  ,-which(colnames(query15.final) %in% c("BuildingType"))]

# exportTable(query15.final.SF, "SF", "query15", weighted = FALSE)
# exportTable(query15.final.MH, "MH", "query15", weighted = FALSE)
















#############################################################################################
#query16
#############################################################################################
#subset to columns needed for analysis
query16.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Type"
                                                                    ,"DHW.Technology"
                                                                    ,"DHW.Location"))]
query16.dat$count <- 1

query16.dat0 <- query16.dat[which(query16.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

query16.dat1 <- left_join(rbsa.dat, query16.dat0, by = "CK_Cadmus_ID")
query16.dat1 <- query16.dat1[grep("site",query16.dat1$CK_Building_ID,ignore.case = T),]
query16.dat2 <- query16.dat1[grep("heat pump",query16.dat1$DHW.Technology, ignore.case = T),]
query16.dat2$Detailed.Type <- paste(query16.dat2$DHW.Type, query16.dat2$DHW.Technology, sep = "-")
unique(query16.dat2$Detailed.Type)
unique(query16.dat2$DHW.Location)

query16.dat3 <- query16.dat2#[-grep("unknown",query16.dat2$Detailed.Type, ignore.case = T),]

################################################
# Adding pop and sample sizes for weights
################################################
query16.data <- weightedData(query16.dat3[-which(colnames(query16.dat3) %in% c("Generic"
                                                                               ,"DHW.Fuel"
                                                                               ,"count"
                                                                               ,"DHW.Type"
                                                                               ,"DHW.Technology"
                                                                               ,"Detailed.Type"
                                                                               ,"DHW.Location"))])
query16.data <- left_join(query16.data, query16.dat3[which(colnames(query16.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"Generic"
                                                                                         ,"DHW.Fuel"
                                                                                         ,"count"
                                                                                         ,"DHW.Type"
                                                                                         ,"DHW.Technology"
                                                                                         ,"Detailed.Type"
                                                                                         ,"DHW.Location"))])

#######################
# Weighted Analysis
#######################
query16.final <- proportions_one_group(CustomerLevelData  = query16.data
                                       , valueVariable    = 'count'
                                       , groupingVariable = 'DHW.Location'
                                       , total.name       = "Total")

# Export table
query16.final.SF <- query16.final[which(query16.final$BuildingType == "Single Family")
                                  ,-which(colnames(query16.final) %in% c("BuildingType"))]
query16.final.MH <- query16.final[which(query16.final$BuildingType == "Manufactured")
                                  ,-which(colnames(query16.final) %in% c("BuildingType"))]

# exportTable(query16.final.SF, "SF", "query16", weighted = TRUE)
# exportTable(query16.final.MH, "MH", "query16", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
query16.final <- proportions_one_group(CustomerLevelData  = query16.data
                                       , valueVariable    = 'count'
                                       , groupingVariable = 'DHW.Location'
                                       , total.name       = "Total"
                                       , weighted = FALSE)

# Export table
query16.final.SF <- query16.final[which(query16.final$BuildingType == "Single Family")
                                  ,-which(colnames(query16.final) %in% c("BuildingType"))]
query16.final.MH <- query16.final[which(query16.final$BuildingType == "Manufactured")
                                  ,-which(colnames(query16.final) %in% c("BuildingType"))]

# exportTable(query16.final.SF, "SF", "query16", weighted = FALSE)
# exportTable(query16.final.MH, "MH", "query16", weighted = FALSE)
