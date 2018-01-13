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

#subset to columns needed for the analysis of items 47,48,49
mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"System.Sub-Type"
                                                                        ,"Heating.Fuel"))]
mechanical.dat1$Heating.Fuel[which(mechanical.dat1$Heating.Fuel == "Natural gas")] <- "Natural Gas"




#############################################################################################
#Item 47: DISTRIBUTION OF FUEL CHOICE, FORCED AIR FURNACES (SF table 54, MH table 36)
#############################################################################################

item47.dat <- mechanical.dat1

item47.dat1 <- item47.dat[which(item47.dat$Generic == "Furnace"),]
item47.dat1 <- unique(item47.dat1)
item47.dat2 <- left_join(rbsa.dat, item47.dat1, by = "CK_Cadmus_ID")
item47.dat3 <- item47.dat2[which(!is.na(item47.dat2$Generic)),]

unique(item47.dat3$Heating.Fuel)
item47.dat3$Heating.Fuel[grep("gas|kerosene", item47.dat3$Heating.Fuel, ignore.case = T)] <- "Gas"
unique(item47.dat3$Heating.Fuel)

item47.dat4 <- item47.dat3[which(item47.dat3$Heating.Fuel %notin% c("Other"
                                                                    ,"Unknown"
                                                                    ,"Can't Determine"
                                                                    ,NA
                                                                    ,"Wood (cord)")),]
unique(item47.dat4$Heating.Fuel)

item47.dat4$count <- 1

# Weighting function
item47.data <- weightedData(item47.dat4[-which(colnames(item47.dat4) %in% c("Generic"
                                                                            ,"System.Sub-Type"
                                                                            ,"Heating.Fuel"
                                                                            ,"count"))])
item47.data <- left_join(item47.data, item47.dat4[which(colnames(item47.dat4) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"System.Sub-Type"
                                                                                     ,"Heating.Fuel"
                                                                                     ,"count"))])

################################
# Weighted Analysis
################################
item47.final <- proportions_one_group(CustomerLevelData  = item47.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating.Fuel'
                                      , total.name       = "Total"
                                      , weighted = TRUE)

item47.final.SF <- item47.final[which(item47.final$BuildingType == "Single Family")
                                ,-which(colnames(item47.final) %in% c("BuildingType"))]
item47.final.MH <- item47.final[which(item47.final$BuildingType == "Manufactured")
                                ,-which(colnames(item47.final) %in% c("BuildingType"))]

exportTable(item47.final.SF, "SF", "Table 54", weighted = TRUE)
exportTable(item47.final.MH, "MH", "Table 36", weighted = TRUE)


################################
# Unweighted Analysis
################################
item47.final <- proportions_one_group(CustomerLevelData  = item47.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating.Fuel'
                                      , total.name       = "Total"
                                      , weighted = FALSE)

item47.final.SF <- item47.final[which(item47.final$BuildingType == "Single Family")
                                ,-which(colnames(item47.final) %in% c("BuildingType"))]
item47.final.MH <- item47.final[which(item47.final$BuildingType == "Manufactured")
                                ,-which(colnames(item47.final) %in% c("BuildingType"))]

exportTable(item47.final.SF, "SF", "Table 54", weighted = FALSE)
exportTable(item47.final.MH, "MH", "Table 36", weighted = FALSE)





#############################################################################################
#Item 48: DISTRIBUTION OF FUEL CHOICE, BOILERS (SF table 55)
#############################################################################################

item48.dat <- mechanical.dat1

unique(item48.dat$Generic)
item48.dat1 <- item48.dat[which(item48.dat$Generic == "Boiler"),]

item48.dat2 <- left_join(item48.dat1, rbsa.dat, by = "CK_Cadmus_ID")
item48.dat2$count <- 1

item48.dat3 <- item48.dat2[which(item48.dat2$BuildingType == "Single Family"),]
item48.dat4 <- item48.dat3[which(!is.na(item48.dat3$Heating.Fuel)),]

# Weighting function
item48.data <- weightedData(item48.dat4[-which(colnames(item48.dat4) %in% c("Generic"
                                                                            ,"System.Sub-Type"
                                                                            ,"Heating.Fuel"
                                                                            ,"count"))])
item48.data <- left_join(item48.data, item48.dat4[which(colnames(item48.dat4) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"System.Sub-Type"
                                                                                     ,"Heating.Fuel"
                                                                                     ,"count"))])

################################
# Weighted Analysis
################################
item48.final <- proportions_one_group(CustomerLevelData  = item48.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating.Fuel'
                                      , total.name       = "Total"
                                      , weighted = TRUE)


item48.final.SF <- item48.final[which(item48.final$BuildingType == "Single Family")
                                ,-which(colnames(item48.final) %in% c("BuildingType"))]

exportTable(item48.final.SF, "SF", "Table 55", weighted = TRUE)

################################
# Unweighted Analysis
################################
item48.final <- proportions_one_group(CustomerLevelData  = item48.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating.Fuel'
                                      , total.name       = "Total"
                                      , weighted = FALSE)


item48.final.SF <- item48.final[which(item48.final$BuildingType == "Single Family")
                                ,-which(colnames(item48.final) %in% c("BuildingType"))]

exportTable(item48.final.SF, "SF", "Table 55", weighted = FALSE)






#############################################################################################
#Item 49: DISTRIBUTION OF FUEL CHOICE, COMBUSTION HEATING STOVES (SF table 56, MH table 37)
#############################################################################################

item49.dat <- mechanical.dat1

unique(item49.dat$`System.Sub-Type`)
item49.dat1 <- item49.dat[which(item49.dat$`System.Sub-Type` == "Space Heating Stove"),]
# item49.dat1 <- unique(item49.dat1)
item49.dat2 <- left_join(item49.dat1, rbsa.dat, by = "CK_Cadmus_ID")
item49.dat2$count <- 1

item49.dat3 <- item49.dat2[which(item49.dat2$BuildingType %in% c("Single Family", "Manufactured")),]

item49.dat4 <- item49.dat3[which(item49.dat3$Heating.Fuel != "Electric"),]
item49.dat4$Heating.Fuel[grep("cord",item49.dat4$Heating.Fuel, ignore.case = T)] <- "Wood"
item49.dat4$Heating.Fuel[grep("pellets",item49.dat4$Heating.Fuel, ignore.case = T)] <- "Pellets"
item49.dat4$Heating.Fuel[grep("gas",item49.dat4$Heating.Fuel, ignore.case = T)] <- "Gas"
unique(item49.dat4$Heating.Fuel)

# Weighting function
item49.data <- weightedData(item49.dat4[-which(colnames(item49.dat4) %in% c("Generic"
                                                                            ,"System.Sub-Type"
                                                                            ,"Heating.Fuel"
                                                                            ,"count"))])
item49.data <- left_join(item49.data, item49.dat4[which(colnames(item49.dat4) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"System.Sub-Type"
                                                                                     ,"Heating.Fuel"
                                                                                     ,"count"))])

################################
# Weighted Analysis
################################
item49.final <- proportions_one_group(CustomerLevelData  = item49.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating.Fuel'
                                      , total.name       = "Total"
                                      , weighted = TRUE)

item49.final.SF <- item49.final[which(item49.final$BuildingType == "Single Family")
                                ,-which(colnames(item49.final) %in% c("BuildingType"))]
item49.final.MH <- item49.final[which(item49.final$BuildingType == "Manufactured")
                                ,-which(colnames(item49.final) %in% c("BuildingType"))]

exportTable(item49.final.SF, "SF", "Table 56", weighted = TRUE)
exportTable(item49.final.MH, "MH", "Table 37", weighted = TRUE)

################################
# Unweighted Analysis
################################
item49.final <- proportions_one_group(CustomerLevelData  = item49.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'Heating.Fuel'
                                      , total.name       = "Total"
                                      , weighted = FALSE)

item49.final.SF <- item49.final[which(item49.final$BuildingType == "Single Family")
                                ,-which(colnames(item49.final) %in% c("BuildingType"))]
item49.final.MH <- item49.final[which(item49.final$BuildingType == "Manufactured")
                                ,-which(colnames(item49.final) %in% c("BuildingType"))]

exportTable(item49.final.SF, "SF", "Table 56", weighted = FALSE)
exportTable(item49.final.MH, "MH", "Table 37", weighted = FALSE)

