#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())
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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))
rbsa.dat <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
appliances.dat <- data.frame(read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
                             ,stringsAsFactors = FALSE)
# applliances.dat <- data.frame(appliances.dat, stringsAsFactors = F)
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
tableDD.merge$Thermostat.Type[which(tableDD.merge$Thermostat.Type %in% c("Manual Remote", "Hand remote"))]<- "Manual Thermostat - Digital"
tableDD.merge$Thermostat.Type[which(tableDD.merge$Thermostat.Type == "Programmable Remote")]        <- "Programmable Thermostat"
tableDD.merge$Thermostat.Type[which(tableDD.merge$Thermostat.Type == "Manual Thermostat - Analog")] <- "Manual Thermostat - Analog"



################################################
# Adding pop and sample sizes for weights
################################################
tableDD.data <- weightedData(tableDD.merge[-which(colnames(tableDD.merge) %in% c("Count"
                                                                              ,"Type"
                                                                              ,"Thermostat.Type"
                                                                              ,"Category"))])
tableDD.data <- left_join(tableDD.data, tableDD.merge[which(colnames(tableDD.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Count"
                                                                                       ,"Type"
                                                                                       ,"Thermostat.Type"
                                                                                       ,"Category"))])
tableDD.data$count <- 1
tableDD.data$Thermostat.Count <- 1
stopifnot(nrow(tableDD.data) == nrow(tableDD.merge))
#######################
# Weighted Analysis
#######################
tableDD.summary <- proportionRowsAndColumns1(CustomerLevelData = tableDD.data
                                             ,valueVariable = "Thermostat.Count"
                                             ,columnVariable = "Category"
                                             ,rowVariable = "Thermostat.Type"
                                             ,aggregateColumnName = "Remove")

tableDD.cast <- dcast(setDT(tableDD.summary)
                      ,formula = BuildingType + Thermostat.Type ~ Category
                      ,value.var = c("w.percent","w.SE","count","n", "N","EB"))

tableDD.table <- data.frame("BuildingType"    = tableDD.cast$BuildingType
                            ,"Thermostat.Type" = tableDD.cast$Thermostat.Type
                            ,"PSE.Percent"                 = tableDD.cast$w.percent_PSE
                            ,"PSE.SE"                      = tableDD.cast$w.SE_PSE
                            ,"PSE.n"                       = tableDD.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableDD.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableDD.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableDD.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableDD.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableDD.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableDD.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableDD.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableDD.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableDD.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = tableDD.cast$EB_PSE
                            ,"PSE.King.County_EB"          = tableDD.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = tableDD.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = tableDD.cast$`EB_2017 RBSA PS`
)

levels(tableDD.table$Thermostat.Type)
rowOrder <- c("Manual Thermostat - Analog"
              ,"Manual Thermostat - Digital"
              ,"Programmable Thermostat"
              ,"Smart Thermostat"
              ,"Smart/Wi-Fi Thermostat"
              ,"Wi-Fi Enabled Thermostat"
              ,"None"
              ,"Unknown"
              ,"Total")
tableDD.table <- tableDD.table %>% mutate(Thermostat.Type = factor(Thermostat.Type, levels = rowOrder)) %>% arrange(Thermostat.Type)
tableDD.table <- data.frame(tableDD.table)

tableDD.table.MF <- tableDD.table[which(tableDD.table$BuildingType == "Multifamily")
                                     ,which(names(tableDD.table) != "BuildingType")]
exportTable(tableDD.table.MF, "MF", "Table DD", weighted = TRUE,OS = T, osIndicator = "PSE")


#######################
# unweighted Analysis
#######################
tableDD.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableDD.data
                                                     ,valueVariable = "Thermostat.Count"
                                                     ,columnVariable = "Category"
                                                     ,rowVariable = "Thermostat.Type"
                                                     ,aggregateColumnName = "Remove")

tableDD.cast <- dcast(setDT(tableDD.summary)
                      ,formula = BuildingType + Thermostat.Type ~ Category
                      ,value.var = c("Percent","SE","Count","n"))

tableDD.table <- data.frame("BuildingType"    = tableDD.cast$BuildingType
                            ,"Thermostat.Type" = tableDD.cast$Thermostat.Type
                            ,"PSE.Percent"                 = tableDD.cast$Percent_PSE
                            ,"PSE.SE"                      = tableDD.cast$SE_PSE
                            ,"PSE.n"                       = tableDD.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableDD.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableDD.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableDD.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableDD.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableDD.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableDD.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableDD.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableDD.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableDD.cast$`n_2017 RBSA PS`
)

levels(tableDD.table$Thermostat.Type)
rowOrder <- c("Manual Thermostat - Analog"
              ,"Manual Thermostat - Digital"
              ,"Programmable Thermostat"
              ,"Smart Thermostat"
              ,"Smart/Wi-Fi Thermostat"
              ,"Wi-Fi Enabled Thermostat"
              ,"None"
              ,"Unknown"
              ,"Total")
tableDD.table <- tableDD.table %>% mutate(Thermostat.Type = factor(Thermostat.Type, levels = rowOrder)) %>% arrange(Thermostat.Type)
tableDD.table <- data.frame(tableDD.table)

tableDD.table.MF <- tableDD.table[which(tableDD.table$BuildingType == "Multifamily")
                                     ,which(names(tableDD.table) != "BuildingType")]
exportTable(tableDD.table.MF, "MF", "Table DD", weighted = FALSE,OS = T, osIndicator = "PSE")





#############################################################################################
# Table KK: Percent of Homes with Vented Dryers by State
#############################################################################################
#For everything else
tableKK.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","CK_SiteID","Type","Dryer.Vented"))]

tableKK.dat0 <- tableKK.dat[grep("dryer",tableKK.dat$Type, ignore.case = T),]
tableKK.dat0 <- tableKK.dat0[grep("site", tableKK.dat$CK_SiteID,ignore.case = T),]
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
tableKK.data <- weightedData(tableKK.merge[-which(colnames(tableKK.merge) %in% c("Ind"
                                                                                 ,"Category"))])
tableKK.data <- left_join(tableKK.data, tableKK.merge[which(colnames(tableKK.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"
                                                                                           ,"Category"))])
tableKK.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableKK.data$State <- tableKK.data$Category
tableKK.table <- proportions_one_group(CustomerLevelData = tableKK.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Remove"
                                       ,weighted = TRUE)

tableKK.table.MF <- tableKK.table[which(tableKK.table$BuildingType == "Multifamily")
                                  ,which(colnames(tableKK.table) %notin% c("BuildingType"))]
exportTable(tableKK.table.MF, "MF","Table KK",weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# Weighted Analysis
#######################
tableKK.table <- proportions_one_group(CustomerLevelData = tableKK.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Remove"
                                       ,weighted = FALSE)

tableKK.table.MF <- tableKK.table[which(tableKK.table$BuildingType == "Multifamily")
                                  ,which(colnames(tableKK.table) %notin% c("BuildingType"))]
exportTable(tableKK.table.MF, "MF","Table KK",weighted = FALSE,OS = T, osIndicator = "PSE")






#############################################################################################
# Table MM: Percent of homes with smart powerstips by State
#############################################################################################
#For everything else
tableMM.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","CK_SiteID","Type","Dryer.Fuel"))]

tableMM.dat0 <- tableMM.dat[grep("dryer",tableMM.dat$Type, ignore.case = T),]
tableMM.dat0 <- tableMM.dat0[grep("site", tableMM.dat0$CK_SiteID, ignore.case = T),]

tableMM.merge <- left_join(rbsa.dat, tableMM.dat0, by = "CK_Cadmus_ID")

tableMM.merge <- tableMM.merge[which((tableMM.merge$Dryer.Fuel %notin% c("N/A",NA))),]
tableMM.merge$Dryer.Fuel <- trimws(tableMM.merge$Dryer.Fuel)

################################################
# Adding pop and sample sizes for weights
################################################
tableMM.data <- weightedData(tableMM.merge[-which(colnames(tableMM.merge) %in% c("Type"
                                                                                 ,"Dryer.Fuel"
                                                                                 ,"CK_SiteID"
                                                                                 ,"Category"))])
tableMM.data <- left_join(tableMM.data, tableMM.merge[which(colnames(tableMM.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Type"
                                                                                           ,"Dryer.Fuel"
                                                                                           ,"Category"))])
tableMM.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableMM.summary <- proportionRowsAndColumns1(CustomerLevelData = tableMM.data
                                             ,valueVariable = "Count"
                                             ,columnVariable = "Category"
                                             ,rowVariable = "Dryer.Fuel"
                                             ,aggregateColumnName = "Region")
tableMM.cast <- dcast(setDT(tableMM.summary)
                      ,formula = BuildingType + Dryer.Fuel ~ Category
                      ,value.var = c("w.percent","w.SE","count","n","N","EB"))

tableMM.table <- data.frame("BuildingType" = tableMM.cast$BuildingType
                            ,"Dryer.Fuel" = tableMM.cast$Dryer.Fuel
                            ,"PSE.Percent"                 = tableMM.cast$w.percent_PSE
                            ,"PSE.SE"                      = tableMM.cast$w.SE_PSE
                            ,"PSE.n"                       = tableMM.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableMM.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableMM.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableMM.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableMM.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableMM.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableMM.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableMM.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableMM.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableMM.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = tableMM.cast$EB_PSE
                            ,"PSE.King.County_EB"          = tableMM.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = tableMM.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = tableMM.cast$`EB_2017 RBSA PS`)


tableMM.table.MF <- tableMM.table[which(tableMM.table$BuildingType == "Multifamily")
                                     ,which(names(tableMM.table) != "BuildingType")]
exportTable(tableMM.table.MF, "MF","Table MM",weighted = TRUE,OS = T, osIndicator = "PSE")



#######################
# unweighted Analysis
#######################
tableMM.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableMM.data
                                                     ,valueVariable = "Count"
                                                     ,columnVariable = "Category"
                                                     ,rowVariable = "Dryer.Fuel"
                                                     ,aggregateColumnName = "Region")
tableMM.cast <- dcast(setDT(tableMM.summary)
                      ,formula = BuildingType + Dryer.Fuel ~ Category
                      ,value.var = c("Percent","SE","Count","n"))

tableMM.table <- data.frame("BuildingType" = tableMM.cast$BuildingType
                            ,"Dryer.Fuel" = tableMM.cast$Dryer.Fuel
                            ,"PSE.Percent"                 = tableMM.cast$Percent_PSE
                            ,"PSE.SE"                      = tableMM.cast$SE_PSE
                            ,"PSE.n"                       = tableMM.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableMM.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableMM.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableMM.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableMM.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableMM.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableMM.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableMM.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableMM.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableMM.cast$`n_2017 RBSA PS`)

tableMM.table.MF <- tableMM.table[which(tableMM.table$BuildingType == "Multifamily")
                                    ,which(names(tableMM.table) != "BuildingType")]
exportTable(tableMM.table.MF, "MF","Table MM",weighted = FALSE,OS = T, osIndicator = "PSE")







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
                                                                                 ,"Site.Count"
                                                                                 ,"Category"))])
tableLL.data <- left_join(tableLL.data, tableLL.merge[which(colnames(tableLL.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Type"
                                                                                           ,"Site.Count"
                                                                                           ,"Category"))])
tableLL.data$Count <- 1
# tableLL.data$Type.Wifi <- gsub(".Wifi","",tableLL.data$Type.Wifi)
# tableLL.data$Type.Wifi <- gsub("Wifi.","",tableLL.data$Type.Wifi)
# tableLL.data$Type.Wifi <- gsub(".Enabled","",tableLL.data$Type.Wifi)
# tableLL.data$Type.Wifi[which(tableLL.data$Type.Wifi == "Enabled")] <- tableLL.data$Type[which(tableLL.data$Type.Wifi == "Enabled")]
# tableLL.data$Type.Wifi <- gsub("Desktop","Computer",tableLL.data$Type.Wifi)
# unique(tableLL.data$Type.Wifi)

tableLL.data$Ind <- tableLL.data$Site.Count
tableLL.data$count <- 1

#######################
# Weighted Analysis
#######################
tableLL.summary <- proportionRowsAndColumns1(CustomerLevelData = tableLL.data
                                             ,valueVariable = "Ind"
                                             ,columnVariable = "Category"
                                             ,rowVariable = "Type"
                                             ,aggregateColumnName = "Region")
tableLL.summary <- tableLL.summary[which(tableLL.summary$Type != "Total"),]

tableLL.cast <- dcast(setDT(tableLL.summary)
                      ,formula = BuildingType + Type ~ Category
                      ,value.var = c("w.percent","w.SE","count","n","N","EB"))

tableLL.table <- data.frame("BuildingType"    = tableLL.cast$BuildingType
                            ,"Type"           = tableLL.cast$Type
                            ,"PSE.Percent"                 = tableLL.cast$w.percent_PSE
                            ,"PSE.SE"                      = tableLL.cast$w.SE_PSE
                            ,"PSE.n"                       = tableLL.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableLL.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableLL.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableLL.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableLL.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableLL.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableLL.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableLL.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableLL.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableLL.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = tableLL.cast$EB_PSE
                            ,"PSE.King.County_EB"          = tableLL.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = tableLL.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = tableLL.cast$`EB_2017 RBSA PS`)

tableLL.table.MF <- tableLL.table[which(tableLL.table$BuildingType == "Multifamily")
                                     ,which(names(tableLL.table) != "BuildingType")]
exportTable(tableLL.table.MF, "MF","Table LL",weighted = TRUE,OS = T, osIndicator = "PSE")





#######################
# unweighted Analysis
#######################
tableLL.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableLL.data
                                             ,valueVariable = "Ind"
                                             ,columnVariable = "Category"
                                             ,rowVariable = "Type"
                                             ,aggregateColumnName = "Region")
tableLL.summary <- tableLL.summary[which(tableLL.summary$Type != "Total"),]

tableLL.cast <- dcast(setDT(tableLL.summary)
                      ,formula = BuildingType + Type ~ Category
                      ,value.var = c("Percent","SE","Count","n"))

tableLL.table <- data.frame("BuildingType"    = tableLL.cast$BuildingType
                            ,"Type"           = tableLL.cast$Type
                            ,"PSE.Percent"                 = tableLL.cast$Percent_PSE
                            ,"PSE.SE"                      = tableLL.cast$SE_PSE
                            ,"PSE.n"                       = tableLL.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableLL.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableLL.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableLL.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableLL.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableLL.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableLL.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableLL.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableLL.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableLL.cast$`n_2017 RBSA PS`)

tableLL.table.MF <- tableLL.table[which(tableLL.table$BuildingType == "Multifamily")
                                     ,which(names(tableLL.table) != "BuildingType")]
exportTable(tableLL.table.MF, "MF","Table LL",weighted = FALSE,OS = T, osIndicator = "PSE")















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
tableHH.data <- weightedData(tableHH.merge[-which(colnames(tableHH.merge) %in% c("Ind"
                                                                                 ,"Category"))])
tableHH.data <- left_join(tableHH.data, tableHH.merge[which(colnames(tableHH.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"
                                                                                           ,"Category"))])
tableHH.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableHH.data$State <- tableHH.data$Category
tableHH.table <- proportions_one_group(CustomerLevelData = tableHH.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Remove"
                                       ,weighted = TRUE)
tableHH.table <- tableHH.table[which(tableHH.table$State != "Remove"),]

tableHH.table.MF <- tableHH.table[which(tableHH.table$BuildingType == "Multifamily")
                                  ,which(colnames(tableHH.table) %notin% c("BuildingType"))]

exportTable(tableHH.table.MF, "MF", "Table HH", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# Weighted Analysis
#######################
tableHH.table <- proportions_one_group(CustomerLevelData = tableHH.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Remove"
                                       ,weighted = FALSE)
tableHH.table <- tableHH.table[which(tableHH.table$State != "Remove"),]

tableHH.table.MF <- tableHH.table[which(tableHH.table$BuildingType == "Multifamily")
                                  ,which(colnames(tableHH.table) %notin% c("BuildingType"))]

exportTable(tableHH.table.MF, "MF", "Table HH", weighted = FALSE,OS = T, osIndicator = "PSE")





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
unique(tableII.merge$Power.Strip.Use)

tableII.merge$Power.Strip.Use[grep("aquarium|charger", tableII.merge$Power.Strip.Use, ignore.case = T)] <- "Other"

tableII.merge <- left_join(rbsa.dat, tableII.merge)
tableII.merge <- tableII.merge[which(!is.na(tableII.merge$Power.Strip.Use)),]
length(unique(tableII.merge$CK_Cadmus_ID[which(tableII.merge$BuildingType == "Single Family")]))
################################################
# Adding pop and sample sizes for weights
################################################
tableII.data <- weightedData(tableII.merge[-which(colnames(tableII.merge) %in% c("Type"
                                                                                 ,"Power.Strip.Use"
                                                                                 ,"Category"))])
tableII.data <- left_join(tableII.data, tableII.merge[which(colnames(tableII.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Type"
                                                                                           ,"Power.Strip.Use"
                                                                                           ,"Category"))])
tableII.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableII.summary <- proportionRowsAndColumns1(CustomerLevelData = tableII.data
                                             ,valueVariable = "Count"
                                             ,columnVariable = "Category"
                                             ,rowVariable = "Power.Strip.Use"
                                             ,aggregateColumnName = "Remove")

tableII.cast <- dcast(setDT(tableII.summary)
                      ,formula = BuildingType + Power.Strip.Use ~ Category
                      ,value.var = c("w.percent","w.SE","count","n","N","EB"))

tableII.table <- data.frame("BuildingType" = tableII.cast$BuildingType
                            ,"Power.Strip.Use" = tableII.cast$Power.Strip.Use
                            ,"PSE.Percent"                 = tableII.cast$w.percent_PSE
                            ,"PSE.SE"                      = tableII.cast$w.SE_PSE
                            ,"PSE.n"                       = tableII.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableII.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableII.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableII.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableII.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableII.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableII.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableII.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableII.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableII.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = tableII.cast$EB_PSE
                            ,"PSE.King.County_EB"          = tableII.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = tableII.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = tableII.cast$`EB_2017 RBSA PS`)

tableII.table.MF <- tableII.table[which(tableII.table$BuildingType == "Multifamily")
                                  ,which(colnames(tableII.table) %notin% c("BuildingType"))]

exportTable(tableII.table.MF, "MF", "Table II", weighted = TRUE,OS = T, osIndicator = "PSE")


#######################
# Weighted Analysis
#######################
tableII.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableII.data
                                             ,valueVariable = "Count"
                                             ,columnVariable = "Category"
                                             ,rowVariable = "Power.Strip.Use"
                                             ,aggregateColumnName = "Remove")
tableII.cast <- dcast(setDT(tableII.summary)
                      ,formula = BuildingType + Power.Strip.Use ~ Category
                      ,value.var = c("Percent","SE","Count","n"))

tableII.table <- data.frame("BuildingType" = tableII.cast$BuildingType
                            ,"Power.Strip.Use" = tableII.cast$Power.Strip.Use
                            ,"PSE.Percent"                 = tableII.cast$Percent_PSE
                            ,"PSE.SE"                      = tableII.cast$SE_PSE
                            ,"PSE.n"                       = tableII.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableII.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableII.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableII.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableII.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableII.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableII.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableII.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableII.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableII.cast$`n_2017 RBSA PS`)

tableII.table.MF <- tableII.table[which(tableII.table$BuildingType == "Multifamily")
                                  ,which(colnames(tableII.table) %notin% c("BuildingType"))]

exportTable(tableII.table.MF, "MF", "Table II", weighted = FALSE,OS = T, osIndicator = "PSE")
