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
# sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))

#Read in data for analysis
survey.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Labeled and Translated")
#clean cadmus IDs
survey.dat$CK_Cadmus_ID <- trimws(toupper(survey.dat$NEXID))


#############################################################################################
#TABLE AD: Percent of homes reporting having completed an energy audit in the last two years
#############################################################################################
#subset to columns needed for analysis
tableAD.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Has.your.household.completed.a.home.energy.audit.in.the.past.2.years?"))]
colnames(tableAD.dat) <- c("Energy.Audit", "CK_Cadmus_ID")

#merge together analysis data with cleaned RBSA data
tableAD.dat1 <- left_join(rbsa.dat, tableAD.dat, by = "CK_Cadmus_ID")
tableAD.dat2 <- tableAD.dat1[which(tableAD.dat1$Energy.Audit %in% c("Yes","No")),]
unique(tableAD.dat2$Energy.Audit)

tableAD.dat2$Ind <- 0
tableAD.dat2$Ind[which(tableAD.dat2$Energy.Audit == "Yes")] <- 1

################################################
# Adding pop and sample sizes for weights
################################################
tableAD.data <- weightedData(tableAD.dat2[-which(colnames(tableAD.dat2) %in% c("Energy.Audit"
                                                                               ,"Ind"))])
tableAD.data <- left_join(tableAD.data, tableAD.dat2[which(colnames(tableAD.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Energy.Audit"
                                                                                         ,"Ind"))])
tableAD.data$count <- 1
tableAD.data$Count <- 1
#######################
# Weighted Analysis
#######################
# tableAD.final <- proportions_one_group(CustomerLevelData = tableAD.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "State"
#                                        ,total.name = "Region")
# tableAD.final$State[which(tableAD.final$State == "Total")] <- "Region"
# 
# 
# tableAD.final.SF <- tableAD.final[which(tableAD.final$BuildingType == "Single Family")
#                                   ,-which(colnames(tableAD.final) %in% c("BuildingType"))]
# tableAD.final.MH <- tableAD.final[which(tableAD.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(tableAD.final) %in% c("BuildingType"))]
# 
# exportTable(tableAD.final.SF, "SF", "Table AD", weighted = TRUE)
# # exportTable(tableAD.final.MH, "MH", "Table AD", weighted = TRUE)

#######################
# MULTIFAMILY
#######################
tableAD.final.MF <- proportions_one_group(CustomerLevelData = tableAD.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Types")
tableAD.final.MF <- tableAD.final.MF[which(tableAD.final.MF$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAD.final.MF) %in% c("BuildingType"))]
exportTable(tableAD.final.MF, "MF", "Table AD", weighted = TRUE)





#######################
# Unweighted Analysis
#######################
# tableAD.final <- proportions_one_group(CustomerLevelData = tableAD.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "State"
#                                        ,total.name = "Region"
#                                        ,weighted = FALSE)
# tableAD.final$State[which(tableAD.final$State == "Total")] <- "Region"
# 
# 
# tableAD.final.SF <- tableAD.final[which(tableAD.final$BuildingType == "Single Family")
#                                   ,-which(colnames(tableAD.final) %in% c("BuildingType"))]
# tableAD.final.MH <- tableAD.final[which(tableAD.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(tableAD.final) %in% c("BuildingType"))]
# 
# exportTable(tableAD.final.SF, "SF", "Table AD", weighted = FALSE)
# # exportTable(tableAD.final.MH, "MH", "Table AD", weighted = FALSE)

#######################
# MULTIFAMILY
#######################
tableAD.final.MF <- proportions_one_group(CustomerLevelData = tableAD.data
                                          ,valueVariable = "Ind"
                                          ,groupingVariable = "HomeType"
                                          ,total.name = "All Types"
                                          ,weighted = FALSE)
tableAD.final.MF <- tableAD.final.MF[which(tableAD.final.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAD.final.MF) %in% c("BuildingType"))]
exportTable(tableAD.final.MF, "MF", "Table AD", weighted = FALSE)

