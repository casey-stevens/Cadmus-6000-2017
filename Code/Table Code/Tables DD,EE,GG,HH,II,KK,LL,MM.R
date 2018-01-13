#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)


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
rbsa.dat <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
appliances.dat <- data.frame(read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
                             ,stringsAsFactors = FALSE)

#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))




#############################################################################################
# Table DD: Distribution of thermostats by Type
#############################################################################################
#For everything else
tableDD.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","Type","Thermostat.Type"))]

tableDD.dat0 <- tableDD.dat[which(tableDD.dat$Type == "Thermostat"),]

tableDD.merge <- left_join(rbsa.dat, tableDD.dat0, by = "CK_Cadmus_ID")
tableDD.merge <- tableDD.merge[which(!is.na(tableDD.merge$Thermostat.Type)),]

unique(tableDD.merge$Thermostat.Type)
tableDD.merge$Thermostat.Type[which(tableDD.merge$Thermostat.Type %in% c("Manual Remote", "Hand remote"))]<- "Manual thermostat - Digital"
tableDD.merge$Thermostat.Type[which(tableDD.merge$Thermostat.Type == "Programmable Remote")]        <- "Programmable thermostat"
tableDD.merge$Thermostat.Type[which(tableDD.merge$Thermostat.Type == "Manual Thermostat - Analog")] <- "Manual thermostat - Analog"



################################################
# Adding pop and sample sizes for weights
################################################
tableDD.data <- weightedData(tableDD.merge[-which(colnames(tableDD.merge) %in% c("Count"
                                                                              ,"Type"
                                                                              ,"Thermostat.Type"))])
tableDD.data <- left_join(tableDD.data, tableDD.merge[which(colnames(tableDD.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Count"
                                                                                       ,"Type"
                                                                                       ,"Thermostat.Type"))])
tableDD.data$count <- 1
tableDD.data$Thermostat.Count <- 1
#######################
# Weighted Analysis
#######################
tableDD.summary <- proportionRowsAndColumns1(CustomerLevelData = tableDD.data
                                             ,valueVariable = "Thermostat.Count"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Thermostat.Type"
                                             ,aggregateColumnName = "Region")

tableDD.cast <- dcast(setDT(tableDD.summary)
                      ,formula = BuildingType + Thermostat.Type ~ State
                      ,value.var = c("w.percent","w.SE","count","n", "N","EB"))

tableDD.table <- data.frame("BuildingType"    = tableDD.cast$BuildingType
                            ,"Thermostat.Type"= tableDD.cast$Thermostat.Type
                            ,"ID"             = tableDD.cast$w.percent_ID
                            ,"ID.SE"          = tableDD.cast$w.SE_ID
                            ,"ID.n"           = tableDD.cast$n_ID
                            ,"MT"             = tableDD.cast$w.percent_MT
                            ,"MT.SE"          = tableDD.cast$w.SE_MT
                            ,"MT.n"           = tableDD.cast$n_MT
                            ,"OR"             = tableDD.cast$w.percent_OR
                            ,"OR.SE"          = tableDD.cast$w.SE_OR
                            ,"OR.n"           = tableDD.cast$n_OR
                            ,"WA"             = tableDD.cast$w.percent_WA
                            ,"WA.SE"          = tableDD.cast$w.SE_WA
                            ,"WA.n"           = tableDD.cast$n_WA
                            ,"Region"         = tableDD.cast$w.percent_Region
                            ,"Region.SE"      = tableDD.cast$w.SE_Region
                            ,"Region.n"       = tableDD.cast$n_Region
                            ,"EB_ID"          = tableDD.cast$EB_ID
                            ,"EB_MT"          = tableDD.cast$EB_MT
                            ,"EB_OR"          = tableDD.cast$EB_OR
                            ,"EB_WA"          = tableDD.cast$EB_WA
                            ,"EB_Region"      = tableDD.cast$EB_Region
)

levels(tableDD.table$Thermostat.Type)
rowOrder <- c("Manual thermostat - Analog"
              ,"Manual thermostat - Digital"
              ,"Programmable thermostat"
              ,"Smart thermostat"
              ,"Smart/Wi-Fi thermostat"
              ,"Wi-Fi enabled thermostat"
              ,"None"
              ,"Total")
tableDD.table <- tableDD.table %>% mutate(Thermostat.Type = factor(Thermostat.Type, levels = rowOrder)) %>% arrange(Thermostat.Type)  
tableDD.table <- data.frame(tableDD.table)

tableDD.table.SF <- tableDD.table[which(tableDD.table$BuildingType == "Single Family")
                                  ,which(colnames(tableDD.table) %notin% c("BuildingType"))]
tableDD.table.MH <- tableDD.table[which(tableDD.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableDD.table) %notin% c("BuildingType"))]

exportTable(tableDD.table.SF, "SF", "Table DD", weighted = TRUE)
exportTable(tableDD.table.MH, "MH", "Table DD", weighted = TRUE)

#######################
# unweighted Analysis
#######################
tableDD.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableDD.data
                                                     ,valueVariable = "Thermostat.Count"
                                                     ,columnVariable = "State"
                                                     ,rowVariable = "Thermostat.Type"
                                                     ,aggregateColumnName = "Region")

tableDD.cast <- dcast(setDT(tableDD.summary)
                      ,formula = BuildingType + Thermostat.Type ~ State
                      ,value.var = c("Percent","SE","Count","n"))

tableDD.table <- data.frame("BuildingType"    = tableDD.cast$BuildingType
                            ,"Thermostat.Type" = tableDD.cast$Thermostat.Type
                            ,"ID"             = tableDD.cast$Percent_ID
                            ,"ID.SE"          = tableDD.cast$SE_ID
                            ,"ID.n"           = tableDD.cast$n_ID
                            ,"MT"             = tableDD.cast$Percent_MT
                            ,"MT.SE"          = tableDD.cast$SE_MT
                            ,"MT.n"           = tableDD.cast$n_MT
                            ,"OR"             = tableDD.cast$Percent_OR
                            ,"OR.SE"          = tableDD.cast$SE_OR
                            ,"OR.n"           = tableDD.cast$n_OR
                            ,"WA"             = tableDD.cast$Percent_WA
                            ,"WA.SE"          = tableDD.cast$SE_WA
                            ,"WA.n"           = tableDD.cast$n_WA
                            ,"Region"         = tableDD.cast$Percent_Region
                            ,"Region.SE"      = tableDD.cast$SE_Region
                            ,"Region.n"       = tableDD.cast$n_Region
)

levels(tableDD.table$Thermostat.Type)
rowOrder <- c("Hand remote"
              ,"Manual thermostat - Analog"
              ,"Manual thermostat - Digital"
              ,"Programmable thermostat"
              ,"Smart thermostat"
              ,"Smart/Wi-Fi thermostat"
              ,"Wi-Fi enabled thermostat"
              ,"None"
              ,"Total")
tableDD.table <- tableDD.table %>% mutate(Thermostat.Type = factor(Thermostat.Type, levels = rowOrder)) %>% arrange(Thermostat.Type)  
tableDD.table <- data.frame(tableDD.table)

tableDD.table.SF <- tableDD.table[which(tableDD.table$BuildingType == "Single Family")
                                  ,which(colnames(tableDD.table) %notin% c("BuildingType"))]
tableDD.table.MH <- tableDD.table[which(tableDD.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableDD.table) %notin% c("BuildingType"))]

exportTable(tableDD.table.SF, "SF", "Table DD", weighted = FALSE)
exportTable(tableDD.table.MH, "MH", "Table DD", weighted = FALSE)




#############################################################################################
# Table EE: Percent of homes with smart thermostats by State
#############################################################################################
#For everything else
tableEE.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","Type","Thermostat.Type"))]

tableEE.dat0 <- tableEE.dat[grep("smart",tableEE.dat$Thermostat.Type, ignore.case = T),]

tableEE.merge <- left_join(rbsa.dat, tableEE.dat0, by = "CK_Cadmus_ID")
tableEE.merge$Ind <- 0
tableEE.merge$Ind[which(!is.na(tableEE.merge$Thermostat.Type))] <- 1


################################################
# AEEing pop and sample sizes for weights
################################################
tableEE.data <- weightedData(tableEE.merge[-which(colnames(tableEE.merge) %in% c("Count"
                                                                                 ,"Type"
                                                                                 ,"Thermostat.Type","Ind"))])
tableEE.data <- left_join(tableEE.data, tableEE.merge[which(colnames(tableEE.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Count"
                                                                                           ,"Type"
                                                                                           ,"Thermostat.Type","Ind"))])
tableEE.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableEE.table <- proportions_one_group(CustomerLevelData = tableEE.data
                                         ,valueVariable = "Ind"
                                         ,groupingVariable = "State"
                                         ,total.name = "Region"
                                         ,weighted = TRUE)

tableEE.table.SF <- tableEE.table[which(tableEE.table$BuildingType == "Single Family")
                                  ,which(colnames(tableEE.table) %notin% c("BuildingType"))]
tableEE.table.MH <- tableEE.table[which(tableEE.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableEE.table) %notin% c("BuildingType"))]

exportTable(tableEE.table.SF, "SF", "Table EE", weighted = TRUE)
exportTable(tableEE.table.MH, "MH", "Table EE", weighted = TRUE)

#######################
# Weighted Analysis
#######################
tableEE.table <- proportions_one_group(CustomerLevelData = tableEE.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = FALSE)

tableEE.table.SF <- tableEE.table[which(tableEE.table$BuildingType == "Single Family")
                                  ,which(colnames(tableEE.table) %notin% c("BuildingType"))]
tableEE.table.MH <- tableEE.table[which(tableEE.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableEE.table) %notin% c("BuildingType"))]

exportTable(tableEE.table.SF, "SF", "Table EE", weighted = FALSE)
exportTable(tableEE.table.MH, "MH", "Table EE", weighted = FALSE)




#############################################################################################
# Table HH: Percent of homes with smart powerstips by State
#############################################################################################
#For everything else
tableHH.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","Type","Smart.Power.Strip"))]

tableHH.dat0 <- tableHH.dat[grep("power",tableHH.dat$Type, ignore.case = T),]
tableHH.dat1 <- tableHH.dat0[which(tableHH.dat0$Smart.Power.Strip %in% c("Yes","No")),]

tableHH.merge <- left_join(rbsa.dat, tableHH.dat1, by = "CK_Cadmus_ID")

tableHH.merge <- tableHH.merge[which(!is.na(tableHH.merge$Smart.Power.Strip)),]
tableHH.merge$Ind <- 0
tableHH.merge$Ind[which(tableHH.merge$Smart.Power.Strip == "Yes")] <- 1

tableHH.sum <- summarise(group_by(tableHH.merge, CK_Cadmus_ID)
                         ,Ind = sum(Ind))

tableHH.sum$Ind[which(tableHH.sum$Ind > 0)] <- 1

tableHH.merge <- left_join(rbsa.dat, tableHH.sum, by = "CK_Cadmus_ID")
tableHH.merge$Ind[which(is.na(tableHH.merge$Ind))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
tableHH.data <- weightedData(tableHH.merge[-which(colnames(tableHH.merge) %in% c("Ind"))])
tableHH.data <- left_join(tableHH.data, tableHH.merge[which(colnames(tableHH.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])
tableHH.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableHH.table <- proportions_one_group(CustomerLevelData = tableHH.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = TRUE)

tableHH.table.SF <- tableHH.table[which(tableHH.table$BuildingType == "Single Family")
                                  ,which(colnames(tableHH.table) %notin% c("BuildingType"))]
tableHH.table.MH <- tableHH.table[which(tableHH.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableHH.table) %notin% c("BuildingType"))]

exportTable(tableHH.table.SF, "SF", "Table HH", weighted = TRUE)
exportTable(tableHH.table.MH, "MH", "Table HH", weighted = TRUE)

#######################
# Weighted Analysis
#######################
tableHH.table <- proportions_one_group(CustomerLevelData = tableHH.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = FALSE)

tableHH.table.SF <- tableHH.table[which(tableHH.table$BuildingType == "Single Family")
                                  ,which(colnames(tableHH.table) %notin% c("BuildingType"))]
tableHH.table.MH <- tableHH.table[which(tableHH.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableHH.table) %notin% c("BuildingType"))]

exportTable(tableHH.table.SF, "SF", "Table HH", weighted = FALSE)
exportTable(tableHH.table.MH, "MH", "Table HH", weighted = FALSE)





#############################################################################################
# Table II: Distribution of power strips by use type and state
#############################################################################################
#For everything else
colnames(appliances.dat)[grep("power",colnames(appliances.dat), ignore.case = T)]
tableII.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","Type","Power.Strip.Use"))]

tableII.dat0 <- tableII.dat[grep("power",tableII.dat$Type, ignore.case = T),]

tableII.merge <- left_join(rbsa.dat, tableII.dat0, by = "CK_Cadmus_ID")

tableII.merge <- tableII.merge[which((tableII.merge$Power.Strip.Use %notin% c("Unknown",NA))),]
tableII.merge$Power.Strip.Use <- trimws(tableII.merge$Power.Strip.Use)

tableII.merge <- left_join(rbsa.dat, tableII.merge)
tableII.merge <- tableII.merge[which(!is.na(tableII.merge$Power.Strip.Use)),]

################################################
# Adding pop and sample sizes for weights
################################################
tableII.data <- weightedData(tableII.merge[-which(colnames(tableII.merge) %in% c("Type"
                                                                                 ,"Power.Strip.Use"))])
tableII.data <- left_join(tableII.data, tableII.merge[which(colnames(tableII.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Type"
                                                                                           ,"Power.Strip.Use"))])
tableII.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableII.summary <- proportionRowsAndColumns1(CustomerLevelData = tableII.data
                                             ,valueVariable = "Count"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Power.Strip.Use"
                                             ,aggregateColumnName = "Region")
tableII.cast <- dcast(setDT(tableII.summary)
                      ,formula = BuildingType + Power.Strip.Use ~ State
                      ,value.var = c("w.percent","w.SE","count","n","N","EB"))

tableII.table <- data.frame("BuildingType" = tableII.cast$BuildingType
                            ,"Power.Strip.Use" = tableII.cast$Power.Strip.Use
                            ,"ID"             = tableII.cast$w.percent_ID
                            ,"ID.SE"          = tableII.cast$w.SE_ID
                            ,"ID.n"           = tableII.cast$n_ID
                            ,"MT"             = tableII.cast$w.percent_MT
                            ,"MT.SE"          = tableII.cast$w.SE_MT
                            ,"MT.n"           = tableII.cast$n_MT
                            ,"OR"             = tableII.cast$w.percent_OR
                            ,"OR.SE"          = tableII.cast$w.SE_OR
                            ,"OR.n"           = tableII.cast$n_OR
                            ,"WA"             = tableII.cast$w.percent_WA
                            ,"WA.SE"          = tableII.cast$w.SE_WA
                            ,"WA.n"           = tableII.cast$n_WA
                            ,"Region"         = tableII.cast$w.percent_Region
                            ,"Region.SE"      = tableII.cast$w.SE_Region
                            ,"Region.n"       = tableII.cast$n_Region
                            ,"EB_ID"          = tableII.cast$EB_ID
                            ,"EB_MT"          = tableII.cast$EB_MT
                            ,"EB_OR"          = tableII.cast$EB_OR
                            ,"EB_WA"          = tableII.cast$EB_WA
                            ,"EB_Region"      = tableII.cast$EB_Region)

tableII.table.SF <- tableII.table[which(tableII.table$BuildingType == "Single Family")
                                  ,which(colnames(tableII.table) %notin% c("BuildingType"))]
tableII.table.MH <- tableII.table[which(tableII.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableII.table) %notin% c("BuildingType"))]

exportTable(tableII.table.SF, "SF", "Table II", weighted = TRUE)
exportTable(tableII.table.MH, "MH", "Table II", weighted = TRUE)


#######################
# Weighted Analysis
#######################
tableII.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableII.data
                                             ,valueVariable = "Count"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Power.Strip.Use"
                                             ,aggregateColumnName = "Region")
tableII.cast <- dcast(setDT(tableII.summary)
                      ,formula = BuildingType + Power.Strip.Use ~ State
                      ,value.var = c("Percent","SE","Count","n"))

tableII.table <- data.frame("BuildingType" = tableII.cast$BuildingType
                            ,"Power.Strip.Use" = tableII.cast$Power.Strip.Use
                            ,"ID"             = tableII.cast$Percent_ID
                            ,"ID.SE"          = tableII.cast$SE_ID
                            ,"ID.n"           = tableII.cast$n_ID
                            ,"MT"             = tableII.cast$Percent_MT
                            ,"MT.SE"          = tableII.cast$SE_MT
                            ,"MT.n"           = tableII.cast$n_MT
                            ,"OR"             = tableII.cast$Percent_OR
                            ,"OR.SE"          = tableII.cast$SE_OR
                            ,"OR.n"           = tableII.cast$n_OR
                            ,"WA"             = tableII.cast$Percent_WA
                            ,"WA.SE"          = tableII.cast$SE_WA
                            ,"WA.n"           = tableII.cast$n_WA
                            ,"Region"         = tableII.cast$Percent_Region
                            ,"Region.SE"      = tableII.cast$SE_Region
                            ,"Region.n"       = tableII.cast$n_Region)

tableII.table.SF <- tableII.table[which(tableII.table$BuildingType == "Single Family")
                                  ,which(colnames(tableII.table) %notin% c("BuildingType"))]
tableII.table.MH <- tableII.table[which(tableII.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableII.table) %notin% c("BuildingType"))]

exportTable(tableII.table.SF, "SF", "Table II", weighted = FALSE)
exportTable(tableII.table.MH, "MH", "Table II", weighted = FALSE)






#############################################################################################
# Table KK: Percent of Homes with Vented Dryers by State
#############################################################################################
#For everything else
tableKK.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","Type","Dryer.Vented"))]

tableKK.dat0 <- tableKK.dat[grep("dryer",tableKK.dat$Type, ignore.case = T),]
tableKK.dat1 <- tableKK.dat0[which(tableKK.dat0$Dryer.Vented %in% c("Yes","No")),]

tableKK.merge <- left_join(rbsa.dat, tableKK.dat1, by = "CK_Cadmus_ID")

tableKK.merge <- tableKK.merge[which(!is.na(tableKK.merge$Dryer.Vented)),]
tableKK.merge$Ind <- 0
tableKK.merge$Ind[which(tableKK.merge$Dryer.Vented == "Yes")] <- 1

tableKK.sum <- summarise(group_by(tableKK.merge, CK_Cadmus_ID)
                         ,Ind = sum(Ind))

tableKK.sum$Ind[which(tableKK.sum$Ind > 0)] <- 1

tableKK.merge <- left_join(rbsa.dat, tableKK.sum, by = "CK_Cadmus_ID")
tableKK.merge <- tableKK.merge[which(!is.na(tableKK.merge$Ind)),]

################################################
# Adding pop and sample sizes for weights
################################################
tableKK.data <- weightedData(tableKK.merge[-which(colnames(tableKK.merge) %in% c("Ind"))])
tableKK.data <- left_join(tableKK.data, tableKK.merge[which(colnames(tableKK.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])
tableKK.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableKK.table <- proportions_one_group(CustomerLevelData = tableKK.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = TRUE)

tableKK.table.SF <- tableKK.table[which(tableKK.table$BuildingType == "Single Family")
                                  ,which(colnames(tableKK.table) %notin% c("BuildingType"))]
tableKK.table.MH <- tableKK.table[which(tableKK.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableKK.table) %notin% c("BuildingType"))]

exportTable(tableKK.table.SF, "SF", "Table KK", weighted = TRUE)
exportTable(tableKK.table.MH, "MH", "Table KK", weighted = TRUE)

#######################
# Weighted Analysis
#######################
tableKK.table <- proportions_one_group(CustomerLevelData = tableKK.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = FALSE)

tableKK.table.SF <- tableKK.table[which(tableKK.table$BuildingType == "Single Family")
                                  ,which(colnames(tableKK.table) %notin% c("BuildingType"))]
tableKK.table.MH <- tableKK.table[which(tableKK.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableKK.table) %notin% c("BuildingType"))]

exportTable(tableKK.table.SF, "SF", "Table KK", weighted = FALSE)
exportTable(tableKK.table.MH, "MH", "Table KK", weighted = FALSE)







#############################################################################################
# Table MM: Percent of homes with smart powerstips by State
#############################################################################################
#For everything else
tableMM.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","Type","Dryer.Fuel"))]

tableMM.dat0 <- tableMM.dat[grep("dryer",tableMM.dat$Type, ignore.case = T),]

tableMM.merge <- left_join(rbsa.dat, tableMM.dat0, by = "CK_Cadmus_ID")

tableMM.merge <- tableMM.merge[which((tableMM.merge$Dryer.Fuel %notin% c("Unknown",NA))),]
tableMM.merge$Dryer.Fuel <- trimws(tableMM.merge$Dryer.Fuel)

################################################
# Adding pop and sample sizes for weights
################################################
tableMM.data <- weightedData(tableMM.merge[-which(colnames(tableMM.merge) %in% c("Type"
                                                                                 ,"Dryer.Fuel"))])
tableMM.data <- left_join(tableMM.data, tableMM.merge[which(colnames(tableMM.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Type"
                                                                                           ,"Dryer.Fuel"))])
tableMM.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableMM.summary <- proportionRowsAndColumns1(CustomerLevelData = tableMM.data
                                             ,valueVariable = "Count"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Dryer.Fuel"
                                             ,aggregateColumnName = "Region")
tableMM.cast <- dcast(setDT(tableMM.summary)
                      ,formula = BuildingType + Dryer.Fuel ~ State
                      ,value.var = c("w.percent","w.SE","count","n","N","EB"))

tableMM.table <- data.frame("BuildingType" = tableMM.cast$BuildingType
                            ,"Dryer.Fuel" = tableMM.cast$Dryer.Fuel
                            ,"ID"             = tableMM.cast$w.percent_ID
                            ,"ID.SE"          = tableMM.cast$w.SE_ID
                            ,"ID.n"           = tableMM.cast$n_ID
                            ,"MT"             = tableMM.cast$w.percent_MT
                            ,"MT.SE"          = tableMM.cast$w.SE_MT
                            ,"MT.n"           = tableMM.cast$n_MT
                            ,"OR"             = tableMM.cast$w.percent_OR
                            ,"OR.SE"          = tableMM.cast$w.SE_OR
                            ,"OR.n"           = tableMM.cast$n_OR
                            ,"WA"             = tableMM.cast$w.percent_WA
                            ,"WA.SE"          = tableMM.cast$w.SE_WA
                            ,"WA.n"           = tableMM.cast$n_WA
                            ,"Region"         = tableMM.cast$w.percent_Region
                            ,"Region.SE"      = tableMM.cast$w.SE_Region
                            ,"Region.n"       = tableMM.cast$n_Region
                            ,"EB_ID"          = tableMM.cast$EB_ID
                            ,"EB_MT"          = tableMM.cast$EB_MT
                            ,"EB_OR"          = tableMM.cast$EB_OR
                            ,"EB_WA"          = tableMM.cast$EB_WA
                            ,"EB_Region"      = tableMM.cast$EB_Region)

tableMM.table.SF <- tableMM.table[which(tableMM.table$BuildingType == "Single Family")
                                  ,which(colnames(tableMM.table) %notin% c("BuildingType"))]
tableMM.table.MH <- tableMM.table[which(tableMM.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableMM.table) %notin% c("BuildingType"))]

exportTable(tableMM.table.SF, "SF", "Table MM", weighted = TRUE)
exportTable(tableMM.table.MH, "MH", "Table MM", weighted = TRUE)


#######################
# Weighted Analysis
#######################
tableMM.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableMM.data
                                                     ,valueVariable = "Count"
                                                     ,columnVariable = "State"
                                                     ,rowVariable = "Dryer.Fuel"
                                                     ,aggregateColumnName = "Region")
tableMM.cast <- dcast(setDT(tableMM.summary)
                      ,formula = BuildingType + Dryer.Fuel ~ State
                      ,value.var = c("Percent","SE","Count","n"))

tableMM.table <- data.frame("BuildingType" = tableMM.cast$BuildingType
                            ,"Dryer.Fuel" = tableMM.cast$Dryer.Fuel
                            ,"ID"             = tableMM.cast$Percent_ID
                            ,"ID.SE"          = tableMM.cast$SE_ID
                            ,"ID.n"           = tableMM.cast$n_ID
                            ,"MT"             = tableMM.cast$Percent_MT
                            ,"MT.SE"          = tableMM.cast$SE_MT
                            ,"MT.n"           = tableMM.cast$n_MT
                            ,"OR"             = tableMM.cast$Percent_OR
                            ,"OR.SE"          = tableMM.cast$SE_OR
                            ,"OR.n"           = tableMM.cast$n_OR
                            ,"WA"             = tableMM.cast$Percent_WA
                            ,"WA.SE"          = tableMM.cast$SE_WA
                            ,"WA.n"           = tableMM.cast$n_WA
                            ,"Region"         = tableMM.cast$Percent_Region
                            ,"Region.SE"      = tableMM.cast$SE_Region
                            ,"Region.n"       = tableMM.cast$n_Region)

tableMM.table.SF <- tableMM.table[which(tableMM.table$BuildingType == "Single Family")
                                  ,which(colnames(tableMM.table) %notin% c("BuildingType"))]
tableMM.table.MH <- tableMM.table[which(tableMM.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableMM.table) %notin% c("BuildingType"))]

exportTable(tableMM.table.SF, "SF", "Table MM", weighted = FALSE)
exportTable(tableMM.table.MH, "MH", "Table MM", weighted = FALSE)







#############################################################################################
# Table LL: Percent of homes with smart powerstips by State
#############################################################################################
#For everything else
tableLL.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    # ,"Thermostat.Wifi"
                                                                    # ,"STB.Wifi"
                                                                    # ,"TV.Wifi"
                                                                    # ,"Audio.Wifi.Enabled"
                                                                    # ,"Game.System.Wifi"
                                                                    # ,"Computer.Wifi"
                                                                    # ,"Large.Unusual.Load.Wifi.Enabled"
                                                                    ,"Wifi.Enabled"))]
names(tableLL.dat)

tableLL.melt <- melt(tableLL.dat, id.vars = c("CK_Cadmus_ID","Type"))
tableLL.melt <- tableLL.melt[which(!is.na(tableLL.melt$value)),]
tableLL.melt <- tableLL.melt[which(tableLL.melt$value %in% c("Yes","No")),]
unique(tableLL.melt$value)
names(tableLL.melt) <- c("CK_Cadmus_ID","Type","Type.Wifi","Wifi.Connected")

tableLL.melt$Ind <- 0
tableLL.melt$Ind[which(tableLL.melt$Wifi.Connected == "Yes")] <- 1
unique(tableLL.melt$Type)

tableLL.sum <- summarise(group_by(tableLL.melt, CK_Cadmus_ID, Type)
                         ,Site.Count = sum(Ind))


tableLL.sub <- tableLL.sum[which(tableLL.sum$Type %in% c("Dryer","Washer","Refrigerator","Freezer","Stove/Oven")),]

tableLL.merge <- left_join(rbsa.dat, tableLL.sub)
tableLL.merge <- tableLL.merge[which(!is.na(tableLL.merge$Type)),]


################################################
# Adding pop and sample sizes for weights
################################################
tableLL.data <- weightedData(tableLL.merge[-which(colnames(tableLL.merge) %in% c("Type"
                                                                                 ,"Site.Count"))])
tableLL.data <- left_join(tableLL.data, tableLL.merge[which(colnames(tableLL.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Type"
                                                                                           ,"Site.Count"))])
tableLL.data$Count <- 1
# tableLL.data$Type.Wifi <- gsub(".Wifi","",tableLL.data$Type.Wifi)
# tableLL.data$Type.Wifi <- gsub("Wifi.","",tableLL.data$Type.Wifi)
# tableLL.data$Type.Wifi <- gsub(".Enabled","",tableLL.data$Type.Wifi)
# tableLL.data$Type.Wifi[which(tableLL.data$Type.Wifi == "Enabled")] <- tableLL.data$Type[which(tableLL.data$Type.Wifi == "Enabled")]
# tableLL.data$Type.Wifi <- gsub("Desktop","Computer",tableLL.data$Type.Wifi)
# unique(tableLL.data$Type.Wifi)

tableLL.data$Wifi.Ind <- tableLL.data$Site.Count


#######################
# Weighted Analysis
#######################
tableLL.summary <- proportionRowsAndColumns1(CustomerLevelData = tableLL.data
                                             ,valueVariable = "Wifi.Ind"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Type"
                                             ,aggregateColumnName = "Region")
tableLL.summary <- tableLL.summary[which(tableLL.summary$Type != "Total"),]

tableLL.cast <- dcast(setDT(tableLL.summary)
                      ,formula = BuildingType + Type ~ State
                      ,value.var = c("w.percent","w.SE","count","n","N","EB"))

tableLL.table <- data.frame("BuildingType"    = tableLL.cast$BuildingType
                            ,"Type"           = tableLL.cast$Type
                            ,"ID"             = tableLL.cast$w.percent_ID
                            ,"ID.SE"          = tableLL.cast$w.SE_ID
                            ,"ID.n"           = tableLL.cast$n_ID
                            ,"MT"             = tableLL.cast$w.percent_MT
                            ,"MT.SE"          = tableLL.cast$w.SE_MT
                            ,"MT.n"           = tableLL.cast$n_MT
                            ,"OR"             = tableLL.cast$w.percent_OR
                            ,"OR.SE"          = tableLL.cast$w.SE_OR
                            ,"OR.n"           = tableLL.cast$n_OR
                            ,"WA"             = tableLL.cast$w.percent_WA
                            ,"WA.SE"          = tableLL.cast$w.SE_WA
                            ,"WA.n"           = tableLL.cast$n_WA
                            ,"Region"         = tableLL.cast$w.percent_Region
                            ,"Region.SE"      = tableLL.cast$w.SE_Region
                            ,"Region.n"       = tableLL.cast$n_Region
                            ,"EB_ID"          = tableLL.cast$EB_ID
                            ,"EB_MT"          = tableLL.cast$EB_MT
                            ,"EB_OR"          = tableLL.cast$EB_OR
                            ,"EB_WA"          = tableLL.cast$EB_WA
                            ,"EB_Region"      = tableLL.cast$EB_Region)

tableLL.table.SF <- tableLL.table[which(tableLL.table$BuildingType == "Single Family")
                                  ,which(colnames(tableLL.table) %notin% c("BuildingType"))]
tableLL.table.MH <- tableLL.table[which(tableLL.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableLL.table) %notin% c("BuildingType"))]

exportTable(tableLL.table.SF, "SF", "Table LL", weighted = TRUE)
exportTable(tableLL.table.MH, "MH", "Table LL", weighted = TRUE)


#######################
# Weighted Analysis
#######################
tableLL.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableLL.data
                                             ,valueVariable = "Wifi.Ind"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Type.Wifi"
                                             ,aggregateColumnName = "Region")
tableLL.summary <- tableLL.summary[which(tableLL.summary$Type.Wifi != "Total"),]

tableLL.cast <- dcast(setDT(tableLL.summary)
                      ,formula = BuildingType + Type.Wifi ~ State
                      ,value.var = c("Percent","SE","Count","n"))

tableLL.table <- data.frame("BuildingType"    = tableLL.cast$BuildingType
                            ,"Type.Wifi"      = tableLL.cast$Type.Wifi
                            ,"ID"             = tableLL.cast$Percent_ID
                            ,"ID.SE"          = tableLL.cast$SE_ID
                            ,"ID.n"           = tableLL.cast$n_ID
                            ,"MT"             = tableLL.cast$Percent_MT
                            ,"MT.SE"          = tableLL.cast$SE_MT
                            ,"MT.n"           = tableLL.cast$n_MT
                            ,"OR"             = tableLL.cast$Percent_OR
                            ,"OR.SE"          = tableLL.cast$SE_OR
                            ,"OR.n"           = tableLL.cast$n_OR
                            ,"WA"             = tableLL.cast$Percent_WA
                            ,"WA.SE"          = tableLL.cast$SE_WA
                            ,"WA.n"           = tableLL.cast$n_WA
                            ,"Region"         = tableLL.cast$Percent_Region
                            ,"Region.SE"      = tableLL.cast$SE_Region
                            ,"Region.n"       = tableLL.cast$n_Region)

tableLL.table.SF <- tableLL.table[which(tableLL.table$BuildingType == "Single Family")
                                  ,which(colnames(tableLL.table) %notin% c("BuildingType"))]
tableLL.table.MH <- tableLL.table[which(tableLL.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableLL.table) %notin% c("BuildingType"))]

exportTable(tableLL.table.SF, "SF", "Table LL", weighted = FALSE)
exportTable(tableLL.table.MH, "MH", "Table LL", weighted = FALSE)








#############################################################################################
# Table GG: Distribution of Vented Dryers by State
#############################################################################################
#For everything else
tableGG.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","Type","TV.Size"))]

tableGG.dat0 <- tableGG.dat[grep("television",tableGG.dat$Type, ignore.case = T),]
unique(tableGG.dat0$TV.Size)
tableGG.dat0$TV.Size <- as.numeric(as.character(tableGG.dat0$TV.Size))

tableGG.dat1 <- tableGG.dat0[which(!is.na(tableGG.dat0$TV.Size)),]

tableGG.merge <- left_join(rbsa.dat, tableGG.dat1, by = "CK_Cadmus_ID")

tableGG.merge <- tableGG.merge[which(!is.na(tableGG.merge$TV.Size)),]

tableGG.mean <- summarise(group_by(tableGG.merge, CK_Cadmus_ID)
                         ,TV.Size = mean(TV.Size))

tableGG.merge <- left_join(rbsa.dat, tableGG.mean, by = "CK_Cadmus_ID")
tableGG.merge <- tableGG.merge[which(!is.na(tableGG.merge$TV.Size)),]

################################################
# Adding pop and sample sizes for weights
################################################
tableGG.data <- weightedData(tableGG.merge[-which(colnames(tableGG.merge) %in% c("TV.Size"))])
tableGG.data <- left_join(tableGG.data, tableGG.merge[which(colnames(tableGG.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"TV.Size"))])
tableGG.data$Count <- 1
tableGG.data$count <- 1
#######################
# Weighted Analysis
#######################
tableGG.table <- mean_one_group(CustomerLevelData = tableGG.data
                                ,valueVariable = "TV.Size"
                                ,byVariable = "State"
                                ,aggregateRow = "Region")

tableGG.table.SF <- tableGG.table[which(tableGG.table$BuildingType == "Single Family")
                                  ,which(colnames(tableGG.table) %notin% c("BuildingType"))]
tableGG.table.MH <- tableGG.table[which(tableGG.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableGG.table) %notin% c("BuildingType"))]

exportTable(tableGG.table.SF, "SF", "Table GG", weighted = TRUE)
exportTable(tableGG.table.MH, "MH", "Table GG", weighted = TRUE)

#######################
# Weighted Analysis
#######################
tableGG.table <- mean_one_group_unweighted(CustomerLevelData = tableGG.data
                                           ,valueVariable = "TV.Size"
                                           ,byVariable = "State"
                                           ,aggregateRow = "Region")

tableGG.table.SF <- tableGG.table[which(tableGG.table$BuildingType == "Single Family")
                                  ,which(colnames(tableGG.table) %notin% c("BuildingType"))]
tableGG.table.MH <- tableGG.table[which(tableGG.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableGG.table) %notin% c("BuildingType"))]

exportTable(tableGG.table.SF, "SF", "Table GG", weighted = FALSE)
exportTable(tableGG.table.MH, "MH", "Table GG", weighted = FALSE)
