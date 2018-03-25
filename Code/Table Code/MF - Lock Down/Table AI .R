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

tableAI.dat3 <- tableAI.dat2[-grep("unknown|N/A",tableAI.dat2$Detailed.Type, ignore.case = T),]

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
# exportTable(tableAI.final.MH, "MH", "Table AI", weighted = TRUE)
exportTable(tableAI.final.MF, "MF", "Table AI", weighted = TRUE)

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
# exportTable(tableAI.final.MH, "MH", "Table AI", weighted = FALSE)
exportTable(tableAI.final.MF, "MF", "Table AI", weighted = FALSE)

