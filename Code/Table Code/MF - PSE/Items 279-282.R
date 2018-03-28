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
rbsa.dat.MF <- rbsa.dat[which(rbsa.dat$BuildingType == "Multifamily"),]
rbsa.dat.site <- rbsa.dat.MF[grep("site", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat.MF[grep("bldg", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
# mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))





#############################################################################################
#Item 279: DISTRIBUTION OF PRIMARY IN-UNIT HEATING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
#############################################################################################
item279.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    # ,"Generic"
                                                                    ,"System.Type"
                                                                    ,"System.Sub-Type"
                                                                    ,"Heating.Fuel"
                                                                    ,"Primary.Heating.System"))]

#subset to only buidling level information
item279.dat0 <- item279.dat[grep("SITE",item279.dat$CK_SiteID),]

#merge on mechanical data with rbsa cleaned data
item279.dat1 <- left_join(rbsa.dat.site, item279.dat0, by = "CK_Cadmus_ID")

#subset to only multifamily units
item279.dat2 <- item279.dat1[grep("Multifamily",item279.dat1$BuildingType),]
item279.dat2 <- item279.dat2[grep("site",item279.dat2$CK_Building_ID,ignore.case = T),]

for (ii in 1:nrow(item279.dat2)){
  if (item279.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater", "Vertical Wall Heater")){
    item279.dat2$System.Type[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (item279.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater", "Electric Plug In Heater", "Electric Plug-In Heater", "Plug In Heater")){
    item279.dat2$System.Type[ii] <- "Plug-In Heaters"
  }
}

#subset to only primary heating rows
item279.dat3 <- unique(item279.dat2[which(item279.dat2$Primary.Heating.System == "Yes"),])
which(duplicated(item279.dat3$CK_Cadmus_ID))
item279.dat3$count <- 1

item279.dat3$Heating_System <- item279.dat3$System.Type
item279.dat3$Heating_System[grep("baseboard",item279.dat3$Heating_System,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item279.dat3$Heating_System[grep("fireplace",item279.dat3$Heating_System,ignore.case = T)] <- "Stove/Fireplace"
item279.dat3$Heating_System[grep("package",item279.dat3$Heating_System,ignore.case = T)] <- "Packaged HP"
item279.dat3$Heating_System[grep("ductless",item279.dat3$Heating_System,ignore.case = T)] <- "Mini-split HP"
item279.dat3$Heating_System[grep("boiler",item279.dat3$Heating_System,ignore.case = T)] <- "Boiler"
item279.dat3$Heating_System[grep("ceiling|radiant|zonal",item279.dat3$Heating_System,ignore.case = T)] <- "Other Zonal Heat"
item279.dat3$Heating_System[grep("plug-in",item279.dat3$Heating_System,ignore.case = T)] <- "Plug In Heaters"
unique(item279.dat3$Heating_System)

#remove NA in heating fuel types
unique(item279.dat3$Heating.Fuel)
item279.dat4 <- item279.dat3[grep("electric|gas|wood",item279.dat3$Heating.Fuel, ignore.case = T),]
# item279.dat4 <- item279.dat4[-grep("water heat",item279.dat4$Heating_System, ignore.case = T),]
item279.dat4$Heating.Fuel[which(item279.dat4$Heating.Fuel == "Natural gas")] <- "Natural Gas"
item279.dat4$Heating.Fuel[grep("wood",item279.dat4$Heating.Fuel, ignore.case = T)] <- "Wood"
item279.dat4 <- item279.dat4[which(item279.dat4$Heating.Fuel %notin% c("N/A", NA, "Unknown")),]
unique(item279.dat4$Heating.Fuel)
unique(item279.dat4$Heating_System)
names(item279.dat4)

item279.dat4 <- item279.dat4[which(item279.dat4$Category == "PSE"),]
######################################
#Pop and Sample Sizes for weights
######################################
item279.data <- weightedData(item279.dat4[which(colnames(item279.dat4) %notin% c("CK_SiteID"
                                                                                 ,"System.Type"
                                                                                 ,"System.Sub-Type"
                                                                                 ,"Primary.Heating.System"
                                                                                 ,"Heating.Fuel"
                                                                                 ,"count"
                                                                                 ,"Heating_System"
                                                                                 ,"Category"))])

item279.data <- left_join(item279.data, item279.dat4[which(colnames(item279.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"System.Type"
                                                                                         ,"System.Sub-Type"
                                                                                         ,"Primary.Heating.System"
                                                                                         ,"Heating.Fuel"
                                                                                         ,"count"
                                                                                         ,"Heating_System"
                                                                                         ,"Category"))])
item279.data$count <- 1


######################
# weighted analysis
######################
item279.summary <- proportionRowsAndColumns1(CustomerLevelData = item279.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'Heating_System'
                                             ,rowVariable = 'Heating.Fuel'
                                             ,aggregateColumnName = "All Systems")
item279.summary <- item279.summary[which(item279.summary$Heating.Fuel != "Total"),]
item279.summary <- item279.summary[which(item279.summary$Heating_System != "All Systems"),]

item279.all.types <- proportions_one_group(CustomerLevelData = item279.data
                                              ,valueVariable = 'count'
                                              ,groupingVariable = 'Heating_System'
                                              ,total.name = 'All Types'
                                              ,columnName = 'Heating.Fuel'
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
item279.all.types$Heating_System[which(item279.all.types$Heating_System == "Total")] <- "All Systems"

item279.all.systems <- proportions_one_group(CustomerLevelData = item279.data
                                                ,valueVariable = 'count'
                                                ,groupingVariable = 'Heating.Fuel'
                                                ,total.name = 'All Systems'
                                                ,columnName = 'Heating_System'
                                                ,weighted = TRUE
                                                ,two.prop.total = TRUE)
# item279.all.systems <- item279.all.systems[which(item279.all.systems$Heating.Fuel ! "Total"),]

item279.final <- rbind.data.frame(item279.summary, item279.all.types, item279.all.systems, stringsAsFactors = F)

item279.cast <- dcast(setDT(item279.final)
                      ,formula = Heating_System ~ Heating.Fuel
                      ,value.var = c("w.percent", "w.SE","count","n","N","EB"))
names(item279.cast)

item279.table <- data.frame("Primary.Heating.System" = item279.cast$Heating_System
                            ,"Electric"              = item279.cast$w.percent_Electric
                            ,"Electric.SE"           = item279.cast$w.SE_Electric
                            ,"Gas"                   = item279.cast$`w.percent_Natural Gas`
                            ,"Gas.SE"                = item279.cast$`w.SE_Natural Gas`
                            # ,"Wood"                  = item279.cast$`w.percent_Wood`
                            # ,"Wood.SE"               = item279.cast$`w.SE_Wood`
                            ,"All.Types"             = item279.cast$`w.percent_All Types`
                            ,"All.Types.SE"          = item279.cast$`w.SE_All Types`
                            ,"n"                     = item279.cast$`n_All Types`
                            ,"Electric.EB"           = item279.cast$EB_Electric
                            ,"Gas.EB"                = item279.cast$`EB_Natural Gas`
                            # ,"Wood.EB"               = item279.cast$`EB_Wood`
                            ,"All.Types.EB"          = item279.cast$`EB_All Types`
                            )
levels(item279.table$Primary.Heating.System)
rowOrder <- c("Air Handler"
              ,"Air Source Heat Pump"
              ,"Boiler"
              ,"Electric Baseboard and Wall Heaters"
              ,"Furnace"
              ,"Mini-Split HP"
              ,"Packaged HP"
              ,"Stove/Fireplace"
              ,"Plug In Heaters"
              ,"Other Zonal Heat"
              ,"All Systems")
item279.table <- item279.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item279.table <- data.frame(item279.table)

exportTable(item279.table, "MF", "Table 71A", weighted = TRUE,OS = T, osIndicator = "PSE")



######################
# unweighted analysis
######################
item279.data$Fuel <- item279.data$Heating.Fuel
item279.summary <- proportions_two_groups_unweighted(CustomerLevelData = item279.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'Heating_System'
                                             ,rowVariable = 'Fuel'
                                             ,aggregateColumnName = "All Systems")
item279.summary <- item279.summary[which(item279.summary$Fuel != "Total"),]
item279.summary <- item279.summary[which(item279.summary$Heating_System != "All Systems"),]

item279.all.types <- proportions_one_group(CustomerLevelData = item279.data
                                              ,valueVariable = 'count'
                                              ,groupingVariable = 'Heating_System'
                                              ,total.name = 'All Types'
                                              ,columnName = 'Fuel'
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
item279.all.types$Heating_System[which(item279.all.types$Heating_System == "Total")] <- "All Systems"

item279.all.systems <- proportions_one_group(CustomerLevelData = item279.data
                                                ,valueVariable = 'count'
                                                ,groupingVariable = 'Fuel'
                                                ,total.name = 'All Systems'
                                                ,columnName = 'Heating_System'
                                                ,weighted = FALSE
                                                ,two.prop.total = TRUE)
# item279.all.systems <- item279.all.systems[which(item279.all.systems$Fuel != "Total"),]

item279.final <- rbind.data.frame(item279.summary, item279.all.types, item279.all.systems, stringsAsFactors = F)

item279.cast <- dcast(setDT(item279.final)
                      ,formula = Heating_System ~ Fuel
                      ,value.var = c("Percent", "SE","Count","n"))

item279.table <- data.frame("Primary.Heating.System" = item279.cast$Heating_System
                            ,"Electric"              = item279.cast$Percent_Electric
                            ,"Electric.SE"           = item279.cast$SE_Electric
                            ,"Gas"                   = item279.cast$`Percent_Natural Gas`
                            ,"Gas.SE"                = item279.cast$`SE_Natural Gas`
                            ,"Wood"                  = item279.cast$`Percent_Wood`
                            ,"Wood.SE"               = item279.cast$`SE_Wood`
                            ,"All.Types"             = item279.cast$`Percent_All Types`
                            ,"All.Types.SE"          = item279.cast$`SE_All Types`
                            ,"n"                     = item279.cast$`n_All Types`)
levels(item279.table$Primary.Heating.System)
rowOrder <- c("Air Handler"
              ,"Air Source Heat Pump"
              ,"Boiler"
              ,"Electric Baseboard and Wall Heaters"
              ,"Furnace"
              ,"Mini-Split HP"
              ,"Packaged HP"
              ,"Stove/Fireplace"
              ,"Plug In Heaters"
              ,"Other Zonal Heat"
              ,"All Systems")
item279.table <- item279.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item279.table <- data.frame(item279.table)

exportTable(item279.table, "MF", "Table 71A", weighted = FALSE,OS = T, osIndicator = "PSE")


#############################################################################################
#Item 279B: DISTRIBUTION OF PRIMARY IN-UNIT HEATING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
#############################################################################################
item279B.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    # ,"Generic"
                                                                    ,"System.Type"
                                                                    ,"System.Sub-Type"
                                                                    ,"Heating.Fuel"
                                                                    ,"Primary.Heating.System"))]

#subset to only buidling level information
item279B.dat0 <- item279B.dat[grep("SITE",item279B.dat$CK_SiteID),]

#merge on mechanical data with rbsa cleaned data
item279B.dat1 <- left_join(rbsa.dat.site, item279B.dat0, by = "CK_Cadmus_ID")

#subset to only multifamily units
item279B.dat2 <- item279B.dat1[grep("Multifamily",item279B.dat1$BuildingType),]
item279B.dat2 <- item279B.dat2[grep("site",item279B.dat2$CK_Building_ID,ignore.case = T),]

for (ii in 1:nrow(item279B.dat2)){
  if (item279B.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater", "Vertical Wall Heater")){
    item279B.dat2$System.Type[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (item279B.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater", "Electric Plug In Heater", "Electric Plug-In Heater", "Plug In Heater")){
    item279B.dat2$System.Type[ii] <- "Plug-In Heaters"
  }
}

#subset to only primary heating rows
item279B.dat3 <- unique(item279B.dat2[which(item279B.dat2$Primary.Heating.System == "Yes"),])
which(duplicated(item279B.dat3$CK_Cadmus_ID))
item279B.dat3$count <- 1

item279B.dat3$Heating_System <- item279B.dat3$System.Type
item279B.dat3$Heating_System[grep("baseboard",item279B.dat3$Heating_System,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item279B.dat3$Heating_System[grep("fireplace",item279B.dat3$Heating_System,ignore.case = T)] <- "Stove/Fireplace"
item279B.dat3$Heating_System[grep("package",item279B.dat3$Heating_System,ignore.case = T)] <- "Packaged HP"
item279B.dat3$Heating_System[grep("ductless",item279B.dat3$Heating_System,ignore.case = T)] <- "Mini-split HP"
item279B.dat3$Heating_System[grep("boiler",item279B.dat3$Heating_System,ignore.case = T)] <- "Boiler"
item279B.dat3$Heating_System[grep("ceiling|radiant|zonal",item279B.dat3$Heating_System,ignore.case = T)] <- "Other Zonal Heat"
item279B.dat3$Heating_System[grep("plug-in",item279B.dat3$Heating_System,ignore.case = T)] <- "Plug In Heaters"
unique(item279B.dat3$Heating_System)

#remove NA in heating fuel types
unique(item279B.dat3$Heating.Fuel)
item279B.dat4 <- item279B.dat3[grep("electric|gas|wood",item279B.dat3$Heating.Fuel, ignore.case = T),]
# item279B.dat4 <- item279B.dat4[-grep("water heat",item279B.dat4$Heating_System, ignore.case = T),]
item279B.dat4$Heating.Fuel[which(item279B.dat4$Heating.Fuel == "Natural gas")] <- "Natural Gas"
item279B.dat4$Heating.Fuel[grep("wood",item279B.dat4$Heating.Fuel, ignore.case = T)] <- "Wood"
item279B.dat4 <- item279B.dat4[which(item279B.dat4$Heating.Fuel %notin% c("N/A", NA, "Unknown")),]
unique(item279B.dat4$Heating.Fuel)
unique(item279B.dat4$Heating_System)
names(item279B.dat4)

######################################
#Pop and Sample Sizes for weights
######################################
item279B.data <- weightedData(item279B.dat4[which(colnames(item279B.dat4) %notin% c("CK_SiteID"
                                                                                 ,"System.Type"
                                                                                 ,"System.Sub-Type"
                                                                                 ,"Primary.Heating.System"
                                                                                 ,"Heating.Fuel"
                                                                                 ,"count"
                                                                                 ,"Heating_System"
                                                                                 ,"Category"))])

item279B.data <- left_join(item279B.data, item279B.dat4[which(colnames(item279B.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"System.Type"
                                                                                         ,"System.Sub-Type"
                                                                                         ,"Primary.Heating.System"
                                                                                         ,"Heating.Fuel"
                                                                                         ,"count"
                                                                                         ,"Heating_System"
                                                                                         ,"Category"))])
item279B.data$count <- 1


######################
# weighted analysis
######################
item279B.summary <- proportionRowsAndColumns1(CustomerLevelData = item279B.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'Category'
                                             ,rowVariable = 'Heating_System'
                                             ,aggregateColumnName = "Remove")
item279B.summary <- item279B.summary[which(item279B.summary$Category != "Remove"),]

item279B.cast <- dcast(setDT(item279B.summary)
                      ,formula = Heating_System ~ Category
                      ,value.var = c("w.percent", "w.SE","count","n","N","EB"))
names(item279B.cast)

item279B.table <- data.frame("Primary.Heating.System" = item279B.cast$Heating_System
                             ,"PSE.Percent"                 = item279B.cast$w.percent_PSE
                             ,"PSE.SE"                      = item279B.cast$w.SE_PSE
                             ,"PSE.n"                       = item279B.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item279B.cast$`w.percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item279B.cast$`w.SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item279B.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item279B.cast$`w.percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item279B.cast$`w.SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item279B.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item279B.cast$`w.percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item279B.cast$`w.SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"              = item279B.cast$`n_2017 RBSA PS`
                             ,"PSE_EB"                      = item279B.cast$EB_PSE
                             ,"PSE.King.County_EB"          = item279B.cast$`EB_PSE KING COUNTY`
                             ,"PSE.Non.King.County_EB"      = item279B.cast$`EB_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS_EB"             = item279B.cast$`EB_2017 RBSA PS`
)
levels(item279B.table$Primary.Heating.System)
rowOrder <- c("Air Handler"
              ,"Air Source Heat Pump"
              ,"Boiler"
              ,"Electric Baseboard and Wall Heaters"
              ,"Furnace"
              ,"Mini-Split HP"
              ,"Packaged HP"
              ,"Stove/Fireplace"
              ,"Plug In Heaters"
              ,"Other Zonal Heat"
              ,"Total")
item279B.table <- item279B.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item279B.table <- data.frame(item279B.table)

exportTable(item279B.table, "MF", "Table 71B", weighted = TRUE,OS = T, osIndicator = "PSE")



######################
# unweighted analysis
######################
item279B.summary <- proportions_two_groups_unweighted(CustomerLevelData = item279B.data
                                                      ,valueVariable = 'count'
                                                      ,columnVariable = 'Category'
                                                      ,rowVariable = 'Heating_System'
                                                      ,aggregateColumnName = "Remove")
item279B.summary <- item279B.summary[which(item279B.summary$Category != "Remove"),]

item279B.cast <- dcast(setDT(item279B.summary)
                       ,formula = Heating_System ~ Category
                       ,value.var = c("Percent", "SE","n"))
names(item279B.cast)

item279B.table <- data.frame("Primary.Heating.System" = item279B.cast$Heating_System
                             ,"PSE.Percent"                 = item279B.cast$Percent_PSE
                             ,"PSE.SE"                      = item279B.cast$SE_PSE
                             ,"PSE.n"                       = item279B.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item279B.cast$`Percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item279B.cast$`SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item279B.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item279B.cast$`Percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item279B.cast$`SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item279B.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item279B.cast$`Percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item279B.cast$`SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"              = item279B.cast$`n_2017 RBSA PS`)
levels(item279B.table$Primary.Heating.System)
rowOrder <- c("Air Handler"
              ,"Air Source Heat Pump"
              ,"Boiler"
              ,"Electric Baseboard and Wall Heaters"
              ,"Furnace"
              ,"Mini-Split HP"
              ,"Packaged HP"
              ,"Stove/Fireplace"
              ,"Plug In Heaters"
              ,"Other Zonal Heat"
              ,"Total")
item279B.table <- item279B.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item279B.table <- data.frame(item279B.table)

exportTable(item279B.table, "MF", "Table 71B", weighted = FALSE,OS = T, osIndicator = "PSE")


#############################################################################################
#Item 279C: DISTRIBUTION OF PRIMARY IN-UNIT HEATING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
#############################################################################################
######################
# weighted analysis
######################
item279C.summary <- proportionRowsAndColumns1(CustomerLevelData = item279B.data
                                              ,valueVariable = 'count'
                                              ,columnVariable = 'Category'
                                              ,rowVariable = 'Heating.Fuel'
                                              ,aggregateColumnName = "Remove")
item279C.summary <- item279C.summary[which(item279C.summary$Category != "Remove"),]

item279C.cast <- dcast(setDT(item279C.summary)
                       ,formula = Heating.Fuel ~ Category
                       ,value.var = c("w.percent", "w.SE","count","n","N","EB"))
names(item279C.cast)

item279C.table <- data.frame("Primary.Heating.Fuel" = item279C.cast$Heating.Fuel
                             ,"PSE.Percent"                 = item279C.cast$w.percent_PSE
                             ,"PSE.SE"                      = item279C.cast$w.SE_PSE
                             ,"PSE.n"                       = item279C.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item279C.cast$`w.percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item279C.cast$`w.SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item279C.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item279C.cast$`w.percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item279C.cast$`w.SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item279C.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item279C.cast$`w.percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item279C.cast$`w.SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"              = item279C.cast$`n_2017 RBSA PS`
                             ,"PSE_EB"                      = item279C.cast$EB_PSE
                             ,"PSE.King.County_EB"          = item279C.cast$`EB_PSE KING COUNTY`
                             ,"PSE.Non.King.County_EB"      = item279C.cast$`EB_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS_EB"             = item279C.cast$`EB_2017 RBSA PS`
)
levels(item279C.table$Primary.Heating.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"Wood"
              ,"Total")
item279C.table <- item279C.table %>% mutate(Primary.Heating.Fuel = factor(Primary.Heating.Fuel, levels = rowOrder)) %>% arrange(Primary.Heating.Fuel)  
item279C.table <- data.frame(item279C.table)

exportTable(item279C.table, "MF", "Table 71C", weighted = TRUE,OS = T, osIndicator = "PSE")



######################
# unweighted analysis
######################
item279C.summary <- proportions_two_groups_unweighted(CustomerLevelData = item279B.data
                                                      ,valueVariable = 'count'
                                                      ,columnVariable = 'Category'
                                                      ,rowVariable = 'Heating.Fuel'
                                                      ,aggregateColumnName = "Remove")
item279C.summary <- item279C.summary[which(item279C.summary$Category != "Remove"),]

item279C.cast <- dcast(setDT(item279C.summary)
                       ,formula = Heating.Fuel ~ Category
                       ,value.var = c("Percent", "SE","n"))
names(item279C.cast)

item279C.table <- data.frame("Primary.Heating.Fuel" = item279C.cast$Heating.Fuel
                             ,"PSE.Percent"                 = item279C.cast$Percent_PSE
                             ,"PSE.SE"                      = item279C.cast$SE_PSE
                             ,"PSE.n"                       = item279C.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item279C.cast$`Percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item279C.cast$`SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item279C.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item279C.cast$`Percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item279C.cast$`SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item279C.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item279C.cast$`Percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item279C.cast$`SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"              = item279C.cast$`n_2017 RBSA PS`)
levels(item279C.table$Primary.Heating.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"Wood"
              ,"Total")
item279C.table <- item279C.table %>% mutate(Primary.Heating.Fuel = factor(Primary.Heating.Fuel, levels = rowOrder)) %>% arrange(Primary.Heating.Fuel)  
item279C.table <- data.frame(item279C.table)

exportTable(item279C.table, "MF", "Table 71C", weighted = FALSE,OS = T, osIndicator = "PSE")


#############################################################################################
#Item 279D: DISTRIBUTION OF PRIMARY IN-UNIT HEATING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
#############################################################################################
item279D.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                     ,"CK_SiteID"
                                                                     # ,"Generic"
                                                                     ,"System.Type"
                                                                     ,"System.Sub-Type"
                                                                     ,"Heating.Fuel"
                                                                     ,"Primary.Heating.System"))]

#subset to only buidling level information
item279D.dat0 <- item279D.dat[grep("SITE",item279D.dat$CK_SiteID),]

#merge on mechanical data with rbsa cleaned data
item279D.dat1 <- left_join(rbsa.dat.site, item279D.dat0, by = "CK_Cadmus_ID")

#subset to only multifamily units
item279D.dat2 <- item279D.dat1[grep("Multifamily",item279D.dat1$BuildingType),]
item279D.dat2 <- item279D.dat2[grep("site",item279D.dat2$CK_Building_ID,ignore.case = T),]

for (ii in 1:nrow(item279D.dat2)){
  if (item279D.dat2$`System.Sub-Type`[ii] %in% c("Vertical wall heater", "Vertical Wall Heater")){
    item279D.dat2$System.Type[ii] <- "Electric Baseboard and Wall Heaters"
  }
  if (item279D.dat2$`System.Sub-Type`[ii] %in% c("Electric plug-in heater", "Electric Plug In Heater", "Electric Plug-In Heater", "Plug In Heater")){
    item279D.dat2$System.Type[ii] <- "Plug-In Heaters"
  }
}

#subset to only primary heating rows
item279D.dat3 <- unique(item279D.dat2[which(item279D.dat2$Primary.Heating.System == "Yes"),])
which(duplicated(item279D.dat3$CK_Cadmus_ID))
item279D.dat3$count <- 1

item279D.dat3$Heating_System <- item279D.dat3$System.Type
item279D.dat3$Heating_System[grep("baseboard",item279D.dat3$Heating_System,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item279D.dat3$Heating_System[grep("fireplace",item279D.dat3$Heating_System,ignore.case = T)] <- "Stove/Fireplace"
item279D.dat3$Heating_System[grep("package",item279D.dat3$Heating_System,ignore.case = T)] <- "Packaged HP"
item279D.dat3$Heating_System[grep("ductless",item279D.dat3$Heating_System,ignore.case = T)] <- "Mini-split HP"
item279D.dat3$Heating_System[grep("boiler",item279D.dat3$Heating_System,ignore.case = T)] <- "Boiler"
item279D.dat3$Heating_System[grep("ceiling|radiant|zonal",item279D.dat3$Heating_System,ignore.case = T)] <- "Other Zonal Heat"
item279D.dat3$Heating_System[grep("plug-in",item279D.dat3$Heating_System,ignore.case = T)] <- "Plug In Heaters"
unique(item279D.dat3$Heating_System)

#remove NA in heating fuel types
unique(item279D.dat3$Heating.Fuel)
item279D.dat4 <- item279D.dat3[grep("electric|gas|wood",item279D.dat3$Heating.Fuel, ignore.case = T),]
# item279D.dat4 <- item279D.dat4[-grep("water heat",item279D.dat4$Heating_System, ignore.case = T),]
item279D.dat4$Heating.Fuel[which(item279D.dat4$Heating.Fuel == "Natural gas")] <- "Natural Gas"
item279D.dat4$Heating.Fuel[grep("wood",item279D.dat4$Heating.Fuel, ignore.case = T)] <- "Wood"
item279D.dat4 <- item279D.dat4[which(item279D.dat4$Heating.Fuel %notin% c("N/A", NA, "Unknown")),]
unique(item279D.dat4$Heating.Fuel)
unique(item279D.dat4$Heating_System)
names(item279D.dat4)
item279D.dat4 <- item279D.dat4[which(item279D.dat4$Heating.Fuel == "Natural Gas"),]
######################################
#Pop and Sample Sizes for weights
######################################
item279D.data <- weightedData(item279D.dat4[which(colnames(item279D.dat4) %notin% c("CK_SiteID"
                                                                                    ,"System.Type"
                                                                                    ,"System.Sub-Type"
                                                                                    ,"Primary.Heating.System"
                                                                                    ,"Heating.Fuel"
                                                                                    ,"count"
                                                                                    ,"Heating_System"
                                                                                    ,"Category"))])

item279D.data <- left_join(item279D.data, item279D.dat4[which(colnames(item279D.dat4) %in% c("CK_Cadmus_ID"
                                                                                             ,"CK_SiteID"
                                                                                             ,"System.Type"
                                                                                             ,"System.Sub-Type"
                                                                                             ,"Primary.Heating.System"
                                                                                             ,"Heating.Fuel"
                                                                                             ,"count"
                                                                                             ,"Heating_System"
                                                                                             ,"Category"))])
item279D.data$count <- 1


######################
# weighted analysis
######################
item279D.summary <- proportionRowsAndColumns1(CustomerLevelData = item279D.data
                                              ,valueVariable = 'count'
                                              ,columnVariable = 'Category'
                                              ,rowVariable = 'Heating_System'
                                              ,aggregateColumnName = "Remove")
item279D.summary <- item279D.summary[which(item279D.summary$Category != "Remove"),]

item279D.cast <- dcast(setDT(item279D.summary)
                       ,formula = Heating_System ~ Category
                       ,value.var = c("w.percent", "w.SE","count","n","N","EB"))
names(item279D.cast)

item279D.table <- data.frame("Primary.Heating.System" = item279D.cast$Heating_System
                             ,"PSE.Percent"                 = item279D.cast$w.percent_PSE
                             ,"PSE.SE"                      = item279D.cast$w.SE_PSE
                             ,"PSE.n"                       = item279D.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item279D.cast$`w.percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item279D.cast$`w.SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item279D.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item279D.cast$`w.percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item279D.cast$`w.SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item279D.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item279D.cast$`w.percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item279D.cast$`w.SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"              = item279D.cast$`n_2017 RBSA PS`
                             ,"PSE_EB"                      = item279D.cast$EB_PSE
                             ,"PSE.King.County_EB"          = item279D.cast$`EB_PSE KING COUNTY`
                             ,"PSE.Non.King.County_EB"      = item279D.cast$`EB_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS_EB"             = item279D.cast$`EB_2017 RBSA PS`
)
levels(item279D.table$Primary.Heating.System)
rowOrder <- c("Air Handler"
              ,"Air Source Heat Pump"
              ,"Boiler"
              ,"Electric Baseboard and Wall Heaters"
              ,"Furnace"
              ,"Mini-Split HP"
              ,"Packaged HP"
              ,"Stove/Fireplace"
              ,"Plug In Heaters"
              ,"Other Zonal Heat"
              ,"Total")
item279D.table <- item279D.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item279D.table <- data.frame(item279D.table)

exportTable(item279D.table, "MF", "Table 71D", weighted = TRUE,OS = T, osIndicator = "PSE")



######################
# unweighted analysis
######################
item279D.summary <- proportions_two_groups_unweighted(CustomerLevelData = item279D.data
                                                      ,valueVariable = 'count'
                                                      ,columnVariable = 'Category'
                                                      ,rowVariable = 'Heating_System'
                                                      ,aggregateColumnName = "Remove")
item279D.summary <- item279D.summary[which(item279D.summary$Category != "Remove"),]

item279D.cast <- dcast(setDT(item279D.summary)
                       ,formula = Heating_System ~ Category
                       ,value.var = c("Percent", "SE","n"))
names(item279D.cast)

item279D.table <- data.frame("Primary.Heating.System" = item279D.cast$Heating_System
                             ,"PSE.Percent"                 = item279D.cast$Percent_PSE
                             ,"PSE.SE"                      = item279D.cast$SE_PSE
                             ,"PSE.n"                       = item279D.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item279D.cast$`Percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item279D.cast$`SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item279D.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item279D.cast$`Percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item279D.cast$`SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item279D.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item279D.cast$`Percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item279D.cast$`SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"              = item279D.cast$`n_2017 RBSA PS`)
levels(item279D.table$Primary.Heating.System)
rowOrder <- c("Air Handler"
              ,"Air Source Heat Pump"
              ,"Boiler"
              ,"Electric Baseboard and Wall Heaters"
              ,"Furnace"
              ,"Mini-Split HP"
              ,"Packaged HP"
              ,"Stove/Fireplace"
              ,"Plug In Heaters"
              ,"Other Zonal Heat"
              ,"Total")
item279D.table <- item279D.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item279D.table <- data.frame(item279D.table)

exportTable(item279D.table, "MF", "Table 71D", weighted = FALSE,OS = T, osIndicator = "PSE")









#############################################################################################
#Item 280A: DISTRIBUTION OF SECONDARY IN-UNIT HEATING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
#############################################################################################
#subset to only primary heating rows
item280A.dat3 <- unique(item279.dat2[which(item279.dat2$Primary.Heating.System %in% c("No","Unknown")),])
which(duplicated(item280A.dat3$CK_Cadmus_ID))



item280A.dat3$Heating_System <- item280A.dat3$System.Type
item280A.dat3$Heating_System[grep("baseboard",item280A.dat3$Heating_System,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item280A.dat3$Heating_System[grep("fireplace",item280A.dat3$Heating_System,ignore.case = T)] <- "Stove/Fireplace"
item280A.dat3$Heating_System[grep("package",item280A.dat3$Heating_System,ignore.case = T)] <- "Packaged HP"
item280A.dat3$Heating_System[grep("ductless",item280A.dat3$Heating_System,ignore.case = T)] <- "Mini-split HP"
item280A.dat3$Heating_System[grep("boiler",item280A.dat3$Heating_System,ignore.case = T)] <- "Boiler"
item280A.dat3$Heating_System[grep("ceiling|radiant|zonal",item280A.dat3$Heating_System,ignore.case = T)] <- "Other Zonal Heat"
item280A.dat3$Heating_System[grep("plug-in",item280A.dat3$Heating_System,ignore.case = T)] <- "Plug In Heaters"
unique(item280A.dat3$Heating_System)


#remove NA in heating fuel types
unique(item280A.dat3$Heating.Fuel)
item280A.dat4 <- item280A.dat3[-grep("other|unknown|hot water",item280A.dat3$Heating.Fuel, ignore.case = T),]
item280A.dat4$Heating.Fuel[which(item280A.dat4$Heating.Fuel == "Natural gas")] <- "Natural Gas"
item280A.dat4$Heating.Fuel[grep("wood",item280A.dat4$Heating.Fuel, ignore.case = T)] <- "Wood"
item280A.dat4 <- item280A.dat4[which(item280A.dat4$Heating.Fuel  %notin% c("N/A",NA,"None","Building Central System")),]
unique(item280A.dat4$Heating.Fuel)
names(item280A.dat4)


item280A.merge <- left_join(rbsa.dat.site, item280A.dat4)
# item280A.merge <- item280A.merge[which(item280A.merge$Heating.Fuel %notin% c("N/A",NA,"None")),]
item280A.merge$Heating.Fuel[which(item280A.merge$Heating.Fuel %in% c("N/A",NA))] <- "None"
item280A.merge$Heating_System[which(item280A.merge$Heating_System %in% c("N/A",NA))] <- "None"
item280A.merge$count <- 1

unique(item280A.merge$Heating.Fuel)
item280A.merge <- item280A.merge[which(item280A.merge$Category == "PSE"),]
######################################
#Pop and Sample Sizes for weights
######################################
item280A.data <- weightedData(item280A.merge[which(colnames(item280A.merge) %notin% c("CK_SiteID"
                                                                                 ,"System.Type"
                                                                                 ,"System.Sub-Type"
                                                                                 ,"Primary.Heating.System"
                                                                                 ,"Heating.Fuel"
                                                                                 ,"count"
                                                                                 ,"Heating_System"
                                                                                 ,"Category"))])

item280A.data <- left_join(item280A.data, item280A.merge[which(colnames(item280A.merge) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"System.Type"
                                                                                         ,"System.Sub-Type"
                                                                                         ,"Primary.Heating.System"
                                                                                         ,"Heating.Fuel"
                                                                                         ,"count"
                                                                                         ,"Heating_System"
                                                                                         ,"Category"))])
item280A.data$count <- 1
item280A.data$Ind <- 1
length(unique(item280A.data$CK_Cadmus_ID))

######################
# weighted analysis
######################
item280A.summary <- proportionRowsAndColumns1(CustomerLevelData = item280A.data
                                             ,valueVariable = 'Ind'
                                             ,columnVariable = 'Heating_System'
                                             ,rowVariable = 'Heating.Fuel'
                                             ,aggregateColumnName = "All Systems")
item280A.summary$Heating.Fuel[which(item280A.summary$Heating.Fuel == "Total")] <- "All Types"


item280A.cast <- dcast(setDT(item280A.summary)
                      ,formula = Heating_System ~ Heating.Fuel
                      ,value.var = c("w.percent", "w.SE","count","n","N","EB"))
names(item280A.cast)
item280A.table <- data.frame("Primary.Heating.System" = item280A.cast$Heating_System
                            ,"Electric"              = item280A.cast$w.percent_Electric
                            ,"Electric.SE"           = item280A.cast$w.SE_Electric
                            ,"Gas"                   = item280A.cast$`w.percent_Natural Gas`
                            ,"Gas.SE"                = item280A.cast$`w.SE_Natural Gas`
                            ,"Wood"                  = item280A.cast$`w.percent_Wood`
                            ,"Wood.SE"               = item280A.cast$`w.SE_Wood`
                            ,"Propane"               = item280A.cast$`w.percent_Propane`
                            ,"Propane.SE"            = item280A.cast$`w.SE_Propane`
                            ,"None"                  = item280A.cast$w.percent_None
                            ,"None.SE"               = item280A.cast$w.SE_None
                            ,"All.Types"             = item280A.cast$`w.percent_All Types`
                            ,"All.Types.SE"          = item280A.cast$`w.SE_All Types`
                            ,"n"                     = item280A.cast$`n_All Types`
                            ,"Electric.EB"           = item280A.cast$EB_Electric
                            ,"Gas.EB"                = item280A.cast$`EB_Natural Gas`
                            ,"Wood.EB"               = item280A.cast$`EB_Wood`
                            ,"Propane.EB"            = item280A.cast$`EB_Propane`
                            ,"None.EB"               = item280A.cast$EB_None
                            ,"All.Types.EB"          = item280A.cast$`EB_All Types`)

levels(item280A.table$Primary.Heating.System)
rowOrder <- c("Electric Baseboard and Wall Heaters"
              ,"Furnace"
              ,"Mini-Split HP"
              ,"Stove/Fireplace"
              ,"Other Zonal Heat"
              ,"Plug In Heaters"
              ,"None"
              ,"All Systems")
item280A.table <- item280A.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item280A.table <- data.frame(item280A.table)

exportTable(item280A.table, "MF", "Table 72A", weighted = TRUE,OS = T, osIndicator = "PSE")



######################
# unweighted analysis
######################
item280A.data$Fuel <- item280A.data$Heating.Fuel
item280A.summary <- proportions_two_groups_unweighted(CustomerLevelData = item280A.data
                                                     ,valueVariable = 'count'
                                                     ,columnVariable = 'Heating_System'
                                                     ,rowVariable = 'Fuel'
                                                     ,aggregateColumnName = "All Systems")
item280A.summary$Fuel[which(item280A.summary$Fuel == "Total")] <- "All Types"

item280A.cast <- dcast(setDT(item280A.summary)
                      ,formula = Heating_System ~ Fuel
                      ,value.var = c("Percent", "SE","Count","n"))
names(item280A.cast)
item280A.table <- data.frame("Primary.Heating.System" = item280A.cast$Heating_System
                            ,"Electric"              = item280A.cast$Percent_Electric
                            ,"Electric.SE"           = item280A.cast$SE_Electric
                            ,"Gas"                   = item280A.cast$`Percent_Natural Gas`
                            ,"Gas.SE"                = item280A.cast$`SE_Natural Gas`
                            ,"Propane"               = item280A.cast$`Percent_Propane`
                            ,"Propane.SE"            = item280A.cast$`SE_Propane`
                            ,"Wood"                  = item280A.cast$`Percent_Wood`
                            ,"Wood.SE"               = item280A.cast$`SE_Wood`
                            ,"None"                  = item280A.cast$Percent_None
                            ,"None.SE"               = item280A.cast$SE_None
                            ,"All.Types"             = item280A.cast$`Percent_All Types`
                            ,"All.Types.SE"          = item280A.cast$`SE_All Types`
                            ,"n"                     = item280A.cast$`n_All Types`)

levels(item280A.table$Primary.Heating.System)
rowOrder <- c("Electric Baseboard and Wall Heaters"
              ,"Furnace"
              ,"Mini-Split HP"
              ,"Stove/Fireplace"
              ,"Other Zonal Heat"
              ,"Plug In Heaters"
              ,"None"
              ,"All Systems")
item280A.table <- item280A.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item280A.table <- data.frame(item280A.table)

exportTable(item280A.table, "MF", "Table 72A", weighted = FALSE,OS = T, osIndicator = "PSE")



#############################################################################################
#Item 280B: DISTRIBUTION OF SECONDARY IN-UNIT HEATING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
#############################################################################################
#subset to only primary heating rows
item280B.dat3 <- unique(item279.dat2[which(item279.dat2$Primary.Heating.System %in% c("No","Unknown")),])
which(duplicated(item280B.dat3$CK_Cadmus_ID))



item280B.dat3$Heating_System <- item280B.dat3$System.Type
item280B.dat3$Heating_System[grep("baseboard",item280B.dat3$Heating_System,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item280B.dat3$Heating_System[grep("fireplace",item280B.dat3$Heating_System,ignore.case = T)] <- "Stove/Fireplace"
item280B.dat3$Heating_System[grep("package",item280B.dat3$Heating_System,ignore.case = T)] <- "Packaged HP"
item280B.dat3$Heating_System[grep("ductless",item280B.dat3$Heating_System,ignore.case = T)] <- "Mini-split HP"
item280B.dat3$Heating_System[grep("boiler",item280B.dat3$Heating_System,ignore.case = T)] <- "Boiler"
item280B.dat3$Heating_System[grep("ceiling|radiant|zonal",item280B.dat3$Heating_System,ignore.case = T)] <- "Other Zonal Heat"
item280B.dat3$Heating_System[grep("plug-in",item280B.dat3$Heating_System,ignore.case = T)] <- "Plug In Heaters"
unique(item280B.dat3$Heating_System)


#remove NA in heating fuel types
unique(item280B.dat3$Heating.Fuel)
item280B.dat4 <- item280B.dat3[-grep("other|unknown|hot water",item280B.dat3$Heating.Fuel, ignore.case = T),]
item280B.dat4$Heating.Fuel[which(item280B.dat4$Heating.Fuel == "Natural gas")] <- "Natural Gas"
item280B.dat4$Heating.Fuel[grep("wood",item280B.dat4$Heating.Fuel, ignore.case = T)] <- "Wood"
item280B.dat4 <- item280B.dat4[which(item280B.dat4$Heating.Fuel  %notin% c("N/A",NA,"None","Building Central System")),]
unique(item280B.dat4$Heating.Fuel)
names(item280B.dat4)


item280B.merge <- left_join(rbsa.dat.site, item280B.dat4)
# item280B.merge <- item280B.merge[which(item280B.merge$Heating.Fuel %notin% c("N/A",NA,"None")),]
item280B.merge$Heating.Fuel[which(item280B.merge$Heating.Fuel %in% c("N/A",NA))] <- "None"
item280B.merge$Heating_System[which(item280B.merge$Heating_System %in% c("N/A",NA))] <- "None"
item280B.merge$count <- 1

unique(item280B.merge$Heating.Fuel)
######################################
#Pop and Sample Sizes for weights
######################################
item280B.data <- weightedData(item280B.merge[which(colnames(item280B.merge) %notin% c("CK_SiteID"
                                                                                      ,"System.Type"
                                                                                      ,"System.Sub-Type"
                                                                                      ,"Primary.Heating.System"
                                                                                      ,"Heating.Fuel"
                                                                                      ,"count"
                                                                                      ,"Heating_System"
                                                                                      ,"Category"))])

item280B.data <- left_join(item280B.data, item280B.merge[which(colnames(item280B.merge) %in% c("CK_Cadmus_ID"
                                                                                               ,"CK_SiteID"
                                                                                               ,"System.Type"
                                                                                               ,"System.Sub-Type"
                                                                                               ,"Primary.Heating.System"
                                                                                               ,"Heating.Fuel"
                                                                                               ,"count"
                                                                                               ,"Heating_System"
                                                                                               ,"Category"))])
item280B.data$count <- 1
item280B.data$Ind <- 1
length(unique(item280B.data$CK_Cadmus_ID))

######################
# weighted analysis
######################
item280B.summary <- proportionRowsAndColumns1(CustomerLevelData = item280B.data
                                              ,valueVariable = 'Ind'
                                              ,columnVariable = 'Category'
                                              ,rowVariable = 'Heating_System'
                                              ,aggregateColumnName = "Remove")
item280B.summary <- item280B.summary[which(item280B.summary$Category != "Remove"),]

item280B.cast <- dcast(setDT(item280B.summary)
                       ,formula = Heating_System ~ Category
                       ,value.var = c("w.percent", "w.SE","count","n","N","EB"))
names(item280B.cast)
item280B.table <- data.frame("Primary.Heating.System" = item280B.cast$Heating_System
                             ,"PSE.Percent"                 = item280B.cast$w.percent_PSE
                             ,"PSE.SE"                      = item280B.cast$w.SE_PSE
                             ,"PSE.n"                       = item280B.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item280B.cast$`w.percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item280B.cast$`w.SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item280B.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item280B.cast$`w.percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item280B.cast$`w.SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item280B.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item280B.cast$`w.percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item280B.cast$`w.SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"              = item280B.cast$`n_2017 RBSA PS`
                             ,"PSE_EB"                      = item280B.cast$EB_PSE
                             ,"PSE.King.County_EB"          = item280B.cast$`EB_PSE KING COUNTY`
                             ,"PSE.Non.King.County_EB"      = item280B.cast$`EB_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS_EB"             = item280B.cast$`EB_2017 RBSA PS`)

levels(item280B.table$Primary.Heating.System)
rowOrder <- c("Electric Baseboard and Wall Heaters"
              ,"Furnace"
              ,"Mini-Split HP"
              ,"Stove/Fireplace"
              ,"Other Zonal Heat"
              ,"Plug In Heaters"
              ,"None"
              ,"Total")
item280B.table <- item280B.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item280B.table <- data.frame(item280B.table)

exportTable(item280B.table, "MF", "Table 72B", weighted = TRUE,OS = T, osIndicator = "PSE")



######################
# unweighted analysis
######################
item280B.summary <- proportions_two_groups_unweighted(CustomerLevelData = item280B.data
                                                      ,valueVariable = 'Ind'
                                                      ,columnVariable = 'Category'
                                                      ,rowVariable = 'Heating_System'
                                                      ,aggregateColumnName = "Remove")
item280B.summary <- item280B.summary[which(item280B.summary$Category != "Remove"),]

item280B.cast <- dcast(setDT(item280B.summary)
                       ,formula = Heating_System ~ Category
                       ,value.var = c("Percent", "SE","Count","n"))
names(item280B.cast)
item280B.table <- data.frame("Primary.Heating.System" = item280B.cast$Heating_System
                             ,"PSE.Percent"                 = item280B.cast$Percent_PSE
                             ,"PSE.SE"                      = item280B.cast$SE_PSE
                             ,"PSE.n"                       = item280B.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item280B.cast$`Percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item280B.cast$`SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item280B.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item280B.cast$`Percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item280B.cast$`SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item280B.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item280B.cast$`Percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item280B.cast$`SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"              = item280B.cast$`n_2017 RBSA PS`)

levels(item280B.table$Primary.Heating.System)
rowOrder <- c("Electric Baseboard and Wall Heaters"
              ,"Furnace"
              ,"Mini-Split HP"
              ,"Stove/Fireplace"
              ,"Other Zonal Heat"
              ,"Plug In Heaters"
              ,"None"
              ,"Total")
item280B.table <- item280B.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item280B.table <- data.frame(item280B.table)

exportTable(item280B.table, "MF", "Table 72B", weighted = FALSE,OS = T, osIndicator = "PSE")



#############################################################################################
#Item 280C: DISTRIBUTION OF SECONDARY IN-UNIT HEATING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
#############################################################################################
#subset to only primary heating rows
item280C.dat3 <- unique(item279.dat2[which(item279.dat2$Primary.Heating.System %in% c("No","Unknown")),])
which(duplicated(item280C.dat3$CK_Cadmus_ID))



item280C.dat3$Heating_System <- item280C.dat3$System.Type
item280C.dat3$Heating_System[grep("baseboard",item280C.dat3$Heating_System,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item280C.dat3$Heating_System[grep("fireplace",item280C.dat3$Heating_System,ignore.case = T)] <- "Stove/Fireplace"
item280C.dat3$Heating_System[grep("package",item280C.dat3$Heating_System,ignore.case = T)] <- "Packaged HP"
item280C.dat3$Heating_System[grep("ductless",item280C.dat3$Heating_System,ignore.case = T)] <- "Mini-split HP"
item280C.dat3$Heating_System[grep("boiler",item280C.dat3$Heating_System,ignore.case = T)] <- "Boiler"
item280C.dat3$Heating_System[grep("ceiling|radiant|zonal",item280C.dat3$Heating_System,ignore.case = T)] <- "Other Zonal Heat"
item280C.dat3$Heating_System[grep("plug-in",item280C.dat3$Heating_System,ignore.case = T)] <- "Plug In Heaters"
unique(item280C.dat3$Heating_System)


#remove NA in heating fuel types
unique(item280C.dat3$Heating.Fuel)
item280C.dat4 <- item280C.dat3[-grep("other|unknown|hot water",item280C.dat3$Heating.Fuel, ignore.case = T),]
item280C.dat4$Heating.Fuel[which(item280C.dat4$Heating.Fuel == "Natural gas")] <- "Natural Gas"
item280C.dat4$Heating.Fuel[grep("wood",item280C.dat4$Heating.Fuel, ignore.case = T)] <- "Wood"
item280C.dat4 <- item280C.dat4[which(item280C.dat4$Heating.Fuel  %notin% c("N/A",NA,"None","Building Central System")),]
unique(item280C.dat4$Heating.Fuel)
names(item280C.dat4)


item280C.merge <- left_join(rbsa.dat.site, item280C.dat4)
# item280C.merge <- item280C.merge[which(item280C.merge$Heating.Fuel %notin% c("N/A",NA,"None")),]
item280C.merge$Heating.Fuel[which(item280C.merge$Heating.Fuel %in% c("N/A",NA))] <- "None"
item280C.merge$Heating_System[which(item280C.merge$Heating_System %in% c("N/A",NA))] <- "None"
item280C.merge$count <- 1

unique(item280C.merge$Heating.Fuel)
######################################
#Pop and Sample Sizes for weights
######################################
item280C.data <- weightedData(item280C.merge[which(colnames(item280C.merge) %notin% c("CK_SiteID"
                                                                                      ,"System.Type"
                                                                                      ,"System.Sub-Type"
                                                                                      ,"Primary.Heating.System"
                                                                                      ,"Heating.Fuel"
                                                                                      ,"count"
                                                                                      ,"Heating_System"
                                                                                      ,"Category"))])

item280C.data <- left_join(item280C.data, item280C.merge[which(colnames(item280C.merge) %in% c("CK_Cadmus_ID"
                                                                                               ,"CK_SiteID"
                                                                                               ,"System.Type"
                                                                                               ,"System.Sub-Type"
                                                                                               ,"Primary.Heating.System"
                                                                                               ,"Heating.Fuel"
                                                                                               ,"count"
                                                                                               ,"Heating_System"
                                                                                               ,"Category"))])
item280C.data$count <- 1
item280C.data$Ind <- 1
length(unique(item280C.data$CK_Cadmus_ID))

######################
# weighted analysis
######################
item280C.summary <- proportionRowsAndColumns1(CustomerLevelData = item280C.data
                                              ,valueVariable = 'Ind'
                                              ,columnVariable = 'Category'
                                              ,rowVariable = 'Heating.Fuel'
                                              ,aggregateColumnName = "Remove")


item280C.cast <- dcast(setDT(item280C.summary)
                       ,formula = Heating.Fuel ~ Category
                       ,value.var = c("w.percent", "w.SE","count","n","N","EB"))
names(item280C.cast)
item280C.table <- data.frame("Primary.Heating.Fuel" = item280C.cast$Heating.Fuel
                             ,"PSE.Percent"                 = item280C.cast$w.percent_PSE
                             ,"PSE.SE"                      = item280C.cast$w.SE_PSE
                             ,"PSE.n"                       = item280C.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item280C.cast$`w.percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item280C.cast$`w.SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item280C.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item280C.cast$`w.percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item280C.cast$`w.SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item280C.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item280C.cast$`w.percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item280C.cast$`w.SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"              = item280C.cast$`n_2017 RBSA PS`
                             ,"PSE_EB"                      = item280C.cast$EB_PSE
                             ,"PSE.King.County_EB"          = item280C.cast$`EB_PSE KING COUNTY`
                             ,"PSE.Non.King.County_EB"      = item280C.cast$`EB_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS_EB"             = item280C.cast$`EB_2017 RBSA PS`)

levels(item280C.table$Primary.Heating.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"Propane"
              ,"Wood"
              ,"None"
              ,"Total")
item280C.table <- item280C.table %>% mutate(Primary.Heating.Fuel = factor(Primary.Heating.Fuel, levels = rowOrder)) %>% arrange(Primary.Heating.Fuel)  
item280C.table <- data.frame(item280C.table)

exportTable(item280C.table, "MF", "Table 72C", weighted = TRUE,OS = T, osIndicator = "PSE")



######################
# unweighted analysis
######################
item280C.summary <- proportions_two_groups_unweighted(CustomerLevelData = item280C.data
                                                      ,valueVariable = 'Ind'
                                                      ,columnVariable = 'Category'
                                                      ,rowVariable = 'Heating.Fuel'
                                                      ,aggregateColumnName = "Remove")
item280C.summary <- item280C.summary[which(item280C.summary$Category != "Remove"),]

item280C.cast <- dcast(setDT(item280C.summary)
                       ,formula = Heating.Fuel ~ Category
                       ,value.var = c("Percent", "SE","Count","n"))
names(item280C.cast)
item280C.table <- data.frame("Primary.Heating.Fuel" = item280C.cast$Heating.Fuel
                             ,"PSE.Percent"                 = item280C.cast$Percent_PSE
                             ,"PSE.SE"                      = item280C.cast$SE_PSE
                             ,"PSE.n"                       = item280C.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item280C.cast$`Percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item280C.cast$`SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item280C.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item280C.cast$`Percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item280C.cast$`SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item280C.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item280C.cast$`Percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item280C.cast$`SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"              = item280C.cast$`n_2017 RBSA PS`)

levels(item280C.table$Primary.Heating.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"Propane"
              ,"Wood"
              ,"None"
              ,"Total")
item280C.table <- item280C.table %>% mutate(Primary.Heating.Fuel = factor(Primary.Heating.Fuel, levels = rowOrder)) %>% arrange(Primary.Heating.Fuel)  
item280C.table <- data.frame(item280C.table)

exportTable(item280C.table, "MF", "Table 72C", weighted = FALSE,OS = T, osIndicator = "PSE")













#############################################################################################
#Item 281: DISTRIBUTION OF IN-UNIT COOLING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 73)
#############################################################################################
item281.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"System.Type"
                                                                    ,"Primary.Cooling.System"))]

#subset to only buidling level information
item281.dat0 <- item281.dat[grep("SITE",item281.dat$CK_SiteID),]


#subset to only primary Cooling rows
item281.dat1 <- unique(item281.dat0[which(item281.dat0$Primary.Cooling.System == "Yes"),])
which(duplicated(item281.dat1$CK_Cadmus_ID))

#merge on mechanical data with rbsa cleaned data
item281.dat2 <- left_join(rbsa.dat.site, item281.dat1, by = "CK_Cadmus_ID")

#subset to only multifamily units
item281.dat3 <- item281.dat2[grep("Multifamily",item281.dat2$BuildingType),]
item281.dat3$Ind <- 0
item281.dat3$Ind[which(item281.dat3$Primary.Cooling.System == "Yes")] <- 1
item281.dat3$count <- 1


######################################
#Pop and Sample Sizes for weights
######################################
item281.data <- weightedData(item281.dat3[which(colnames(item281.dat3) %notin% c("CK_SiteID"
                                                                                 ,"System.Type"
                                                                                 ,"Primary.Cooling.System"
                                                                                 ,"Ind"
                                                                                 ,"count"
                                                                                 ,"Category"))])

item281.data <- left_join(item281.data, unique(item281.dat3[which(colnames(item281.dat3) %in% c("CK_Cadmus_ID"
                                                                                                ,"System.Type"
                                                                                                ,"Primary.Cooling.System"
                                                                                                ,"Ind"
                                                                                                ,"count"
                                                                                                ,"Category"))]))
item281.data$Count <- 1


######################
# weighted analysis
######################
item281.data$State <- item281.data$Category
item281.final <- proportions_one_group(CustomerLevelData = item281.data
                                          ,valueVariable = 'Ind'
                                          ,groupingVariable = 'State'
                                          ,total.name = 'Remove'
                                          ,weighted = TRUE)
item281.final <- item281.final[which(item281.final$State != "Remove"),]
item281.final <- item281.final[which(names(item281.final) != "BuildingType")]
exportTable(item281.final, "MF","Table 73",weighted = TRUE,OS = T, osIndicator = "PSE")

######################
# unweighted analysis
######################
item281.final <- proportions_one_group(CustomerLevelData = item281.data
                                          ,valueVariable = 'Ind'
                                          ,groupingVariable = 'State'
                                          ,total.name = 'Remove'
                                          ,weighted = FALSE)
item281.final <- item281.final[which(item281.final$State != "Remove"),]
item281.final <- item281.final[which(names(item281.final) != "BuildingType")]
exportTable(item281.final, "MF","Table 73",weighted = FALSE,OS = T, osIndicator = "PSE")





#############################################################################################
#Item 282: DISTRIBUTION OF IN-UNIT COOLING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
#############################################################################################
item282.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"System.Type"
                                                                    ,"Primary.Cooling.System"))]

unique(item282.dat$System.Type)
item282.dat$System.Type[grep("mini", item282.dat$System.Type, ignore.case = T)] <- "Mini-split HP"
item282.dat$System.Type[grep("plug", item282.dat$System.Type, ignore.case = T)] <- "Plug In Heater"
item282.dat$System.Type[grep("baseboard", item282.dat$System.Type, ignore.case = T)] <- "Electric Baseboard"
item282.dat$System.Type[grep("fireplace", item282.dat$System.Type, ignore.case = T)] <- "Stove/Fireplace"
item282.dat$System.Type[grep("boiler", item282.dat$System.Type, ignore.case = T)] <- "Boiler"
item282.dat$System.Type[grep("furnace", item282.dat$System.Type, ignore.case = T)] <- "Furnace"
item282.dat$System.Type[grep("central", item282.dat$System.Type, ignore.case = T)] <- "Central AC"

#subset to only buidling level information
item282.dat0 <- item282.dat[grep("SITE",item282.dat$CK_SiteID),]


#subset to only primary Cooling rows
item282.dat1 <- unique(item282.dat0[which(item282.dat0$Primary.Cooling.System == "Yes"),])
which(duplicated(item282.dat1$CK_Cadmus_ID))

#merge on mechanical data with rbsa cleaned data
item282.dat2 <- left_join(rbsa.dat.site, item282.dat1, by = "CK_Cadmus_ID")

#subset to only multifamily units
item282.dat3 <- item282.dat2[grep("Multifamily",item282.dat2$BuildingType),]
item282.dat3$Ind <- 0
item282.dat3$Ind[which(item282.dat3$Primary.Cooling.System == "Yes")] <- 1
item282.dat3$count <- 1

item282.dat4 <- item282.dat3[which(item282.dat3$System.Type != 0),]
item282.dat4$Heating_System <- item282.dat4$System.Type
######################################
#Pop and Sample Sizes for weights
######################################
item282.data <- weightedData(item282.dat4[which(colnames(item282.dat4) %notin% c("CK_SiteID"
                                                                                 ,"System.Type"
                                                                                 ,"Primary.Cooling.System"
                                                                                 ,"Ind"
                                                                                 ,"count"
                                                                                 ,"Heating_System"
                                                                                 ,"Category"))])

item282.data <- left_join(item282.data, item282.dat4[which(colnames(item282.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"System.Type"
                                                                                         ,"Primary.Cooling.System"
                                                                                         ,"Ind"
                                                                                         ,"count"
                                                                                         ,"Heating_System"
                                                                                         ,"Category"))])
item282.data$Count <- 1


######################
# weighted analysis
######################
item282.summary <- proportionRowsAndColumns1(CustomerLevelData = item282.data
                                             ,valueVariable = 'Ind'
                                             ,columnVariable = 'Heating_System'
                                             ,rowVariable = 'Category'
                                             ,aggregateColumnName = "All Systems")
item282.summary <- item282.summary[which(item282.summary$Category != "Total"),]


item282.cast <- dcast(setDT(item282.summary)
                      ,formula = Heating_System ~ Category
                      ,value.var = c("w.percent", "w.SE","count","n","N", "EB"))

item282.table <- data.frame("Cooling.Systems"          = item282.cast$Heating_System
                            ,"PSE.Percent"                 = item282.cast$w.percent_PSE
                            ,"PSE.SE"                      = item282.cast$w.SE_PSE
                            ,"PSE.n"                       = item282.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item282.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item282.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item282.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item282.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item282.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item282.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item282.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item282.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = item282.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = item282.cast$EB_PSE
                            ,"PSE.King.County_EB"          = item282.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = item282.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = item282.cast$`EB_2017 RBSA PS`
                            )

levels(item282.table$Cooling.Systems)
rowOrder <- c("Air Source Heat Pump"
              ,"Central AC"
              ,"Evaporative Cooling"
              ,"Mini-split HP"
              ,"Packaged AC"
              ,"Packaged HP"
              ,"Packaged Unit"
              ,"All Systems")
item282.table <- item282.table %>% mutate(Cooling.Systems = factor(Cooling.Systems, levels = rowOrder)) %>% arrange(Cooling.Systems)  
item282.table <- data.frame(item282.table)


exportTable(item282.table, "MF","Table 74",weighted = TRUE,OS = T, osIndicator = "PSE")


######################
# unweighted analysis
######################
item282.summary <- proportions_two_groups_unweighted(CustomerLevelData = item282.data
                                                     ,valueVariable = 'Ind'
                                                     ,columnVariable = 'Heating_System'
                                                     ,rowVariable = 'Category'
                                                     ,aggregateColumnName = "All Systems")
item282.summary <- item282.summary[which(item282.summary$Category != "Total"),]


item282.cast <- dcast(setDT(item282.summary)
                      ,formula = Heating_System ~ Category
                      ,value.var = c("Percent", "SE","Count","n"))

item282.table <- data.frame("Cooling.Systems"          = item282.cast$Heating_System
                            ,"PSE.Percent"                 = item282.cast$Percent_PSE
                            ,"PSE.SE"                      = item282.cast$SE_PSE
                            ,"PSE.n"                       = item282.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item282.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item282.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item282.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item282.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item282.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item282.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item282.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item282.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = item282.cast$`n_2017 RBSA PS`)

levels(item282.table$Cooling.Systems)
rowOrder <- c("Air Source Heat Pump"
              ,"Central AC"
              ,"Evaporative Cooling"
              ,"Mini-split HP"
              ,"Packaged AC"
              ,"Packaged HP"
              ,"Packaged Unit"
              ,"All Systems")
item282.table <- item282.table %>% mutate(Cooling.Systems = factor(Cooling.Systems, levels = rowOrder)) %>% arrange(Cooling.Systems)  
item282.table <- data.frame(item282.table)

exportTable(item282.table, "MF","Table 74",weighted = FALSE,OS = T, osIndicator = "PSE")
