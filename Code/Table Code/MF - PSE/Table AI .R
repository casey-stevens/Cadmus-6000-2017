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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
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
                                                                            ,"Detailed.Type"
                                                                            ,"Category"))])
tableAI.data <- left_join(tableAI.data, tableAI.dat3[which(colnames(tableAI.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"DHW.Fuel"
                                                                                     ,"count"
                                                                                     ,"DHW.Type"
                                                                                     ,"DHW.Technology"
                                                                                     ,"Detailed.Type"
                                                                                     ,"Category"))])

#######################
# Weighted Analysis
#######################
tableAI.final <- proportionRowsAndColumns1(CustomerLevelData = tableAI.data
                                           ,valueVariable = "count"
                                           ,columnVariable = "Category"
                                           ,rowVariable = "Detailed.Type"
                                           ,aggregateColumnName = "Remove")
tableAI.final <- tableAI.final[which(tableAI.final$Category != "Remove"),]

tableAI.cast <- dcast(setDT(tableAI.final)
                      ,formula = BuildingType + Detailed.Type ~ Category
                      ,value.var = c("w.percent","w.SE","n","EB"))

tableAI.table <- data.frame("BuildingType" = tableAI.cast$BuildingType
                            ,"Water.Heater.Type" = tableAI.cast$Detailed.Type
                            ,"PSE.Percent"                 = tableAI.cast$w.percent_PSE
                            ,"PSE.SE"                      = tableAI.cast$w.SE_PSE
                            ,"PSE.n"                       = tableAI.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableAI.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableAI.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableAI.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableAI.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableAI.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableAI.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableAI.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableAI.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableAI.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = tableAI.cast$EB_PSE
                            ,"PSE.King.County_EB"          = tableAI.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = tableAI.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = tableAI.cast$`EB_2017 RBSA PS`
                            )

tableAI.final.MF <- tableAI.table[which(tableAI.table$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAI.table) %in% c("BuildingType"))]

exportTable(tableAI.final.MF, "MF", "Table AI", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# Unweighted Analysis
#######################
tableAI.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAI.data
                                           ,valueVariable = "count"
                                           ,columnVariable = "Category"
                                           ,rowVariable = "Detailed.Type"
                                           ,aggregateColumnName = "Remove")
tableAI.final <- tableAI.final[which(tableAI.final$Category != "Remove"),]

tableAI.cast <- dcast(setDT(tableAI.final)
                      ,formula = BuildingType + Detailed.Type ~ Category
                      ,value.var = c("Percent","SE","n"))

tableAI.table <- data.frame("BuildingType" = tableAI.cast$BuildingType
                            ,"Water.Heater.Type" = tableAI.cast$Detailed.Type
                            ,"PSE.Percent"                 = tableAI.cast$Percent_PSE
                            ,"PSE.SE"                      = tableAI.cast$SE_PSE
                            ,"PSE.n"                       = tableAI.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableAI.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableAI.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableAI.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableAI.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableAI.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableAI.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableAI.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableAI.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableAI.cast$`n_2017 RBSA PS`
)

tableAI.final.MF <- tableAI.table[which(tableAI.table$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAI.table) %in% c("BuildingType"))]
exportTable(tableAI.final.MF, "MF", "Table AI", weighted = FALSE,OS = T, osIndicator = "PSE")

