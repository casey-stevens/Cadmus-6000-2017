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
# mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))


#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
tableB18.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"System.Type"
                                                                      ,"System.Sub-Type"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
tableB18.dat.11 <- tableB18.dat.1[which(tableB18.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
tableB18.dat.12 <- tableB18.dat.11[which(tableB18.dat.11$Primary.Heating.System == "Yes"),]
length(unique(tableB18.dat.12$CK_Cadmus_ID))

#check uniques
unique(tableB18.dat.12$Primary.Heating.System)
tableB18.dat.12$count <- 1

#subset to only electric primary heating systems
tableB18.dat.13 <- unique(tableB18.dat.12[which(tableB18.dat.12$Heating.Fuel == "Electric"),])

#summaryise up to the customer level - keeping heating fuel information
tableB18.sum <- summarise(group_by(tableB18.dat.13, CK_Cadmus_ID, Heating.Fuel)
                         ,Count = sum(count))
tableB18.sum$Count <- 1

#check uniques
which(duplicated(tableB18.sum$CK_Cadmus_ID)) #none are duplicated!
unique(tableB18.sum$Heating.Fuel)

#merge with cleaned rbsa data and subset to complete data
tableB18.merge <- left_join(rbsa.dat, tableB18.sum)
tableB18.merge <- tableB18.merge[which(!is.na(tableB18.merge$Heating.Fuel)),]

#re-assign mechanical data for later use
tableB18.mechanical <- tableB18.merge











tableB18.dat2 <- left_join(rbsa.dat, tableB18.dat.13)

tableB18.dat2$`System.Sub-Type`[grep("plug-in|plug in", tableB18.dat2$`System.Sub-Type`, ignore.case = T)]

for (ii in 1:nrow(tableB18.dat2)){
  # if (tableB18.dat2$`System.Sub-Type`[ii] %in% c("Dual Fuel Primary", "Dual Fuel Secondary")){
  #   tableB18.dat2$Generic[ii] <- tableB18.dat2$`System.Sub-Type`[ii]
  # }
  if (tableB18.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater", "Vertical Wall Heater")){
    tableB18.dat2$Generic[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (tableB18.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater", "Electric Plug In Heater", "Electric Plug-In Heater", "Plug In Heater")){
    tableB18.dat2$Generic[ii] <- "Plug-In Heaters"
  }
}

tableB18.dat2$Generic[grep("Baseboard",tableB18.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
tableB18.dat2$Generic[grep("zonal heat",tableB18.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
tableB18.dat2$Generic[grep("ductless",tableB18.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
tableB18.dat2$Generic[grep("furnace",tableB18.dat2$Generic,ignore.case = T)] <- "Furnace"
tableB18.dat2$Generic[grep("boiler",tableB18.dat2$Generic,ignore.case = T)] <- "Boiler"
tableB18.dat2$Generic[grep("Stove/Fireplace",tableB18.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"

unique(tableB18.dat2$Generic)

tableB18.dat3 <- unique(data.frame("CK_Cadmus_ID" = tableB18.dat2$CK_Cadmus_ID
                                 ,"Heating_Type"       = tableB18.dat2$Generic
                                 ,"Primary_Secondary"  = tableB18.dat2$Primary.Heating.System))
### Clean heating type
unique(tableB18.dat3$Heating_Type)
tableB18.dat4 <- left_join(rbsa.dat, tableB18.dat3, by = "CK_Cadmus_ID")
unique(tableB18.dat4$Primary_Secondary)
tableB18.dat5 <- unique(tableB18.dat4[which(tableB18.dat4$Primary_Secondary == "Yes"),])
unique(tableB18.dat5$Primary_Secondary)


length(unique(tableB18.dat5$CK_Cadmus_ID[which(tableB18.dat5$BuildingType == "Single Family")]))
nrow(tableB18.dat5[which(tableB18.dat5$BuildingType == "Single Family"),])
dup.ids <- tableB18.dat5$CK_Cadmus_ID[which(duplicated(tableB18.dat5$CK_Cadmus_ID) & tableB18.dat5$BuildingType == "Single Family")]
tableB18.dat5$count <- 1

tableB18.duplicates <- tableB18.dat5[which(tableB18.dat5$CK_Cadmus_ID %in% dup.ids),]

tableB18.dat6 <- tableB18.dat5[which(tableB18.dat5$Heating_Type %notin% c("N/A",NA)),]
unique(tableB18.dat6$Heating_Type)

tableB18.data <- weightedData(tableB18.dat6[-which(colnames(tableB18.dat6) %in% c("Heating_Type"
                                                                            ,"Primary_Secondary"
                                                                            ,"count"))])
tableB18.data <- left_join(tableB18.data, tableB18.dat6[which(colnames(tableB18.dat6) %in% c("CK_Cadmus_ID"
                                                                                     ,"Heating_Type"
                                                                                     ,"Primary_Secondary"
                                                                                     ,"count"))])

#########################
# Weighted Analysis
#########################
tableB18.final <- proportions_one_group(CustomerLevelData  = tableB18.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating_Type'
                                      , total.name       = "Total"
                                      , weighted = TRUE)

# export table
# SF = Table 50, MH = Table 32
tableB18.final.SF <- tableB18.final[which(tableB18.final$BuildingType == "Single Family")
                                ,-which(colnames(tableB18.final) %in% c("BuildingType"))]

exportTable(tableB18.final.SF, "SF", "Table B-18", weighted = TRUE)

#########################
# unWeighted Analysis
#########################
tableB18.final <- proportions_one_group(CustomerLevelData  = tableB18.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating_Type'
                                      , total.name       = "Total"
                                      , weighted = FALSE)

# export table
# SF = Table 50, MH = Table 32
tableB18.final.SF <- tableB18.final[which(tableB18.final$BuildingType == "Single Family")
                                ,-which(colnames(tableB18.final) %in% c("BuildingType"))]

exportTable(tableB18.final.SF, "SF", "Table B-18", weighted = FALSE)
