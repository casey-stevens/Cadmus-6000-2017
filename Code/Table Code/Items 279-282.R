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

##  Create "Not In" operator
"%notin%" <- Negate("%in%")

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
rbsa.dat.MF <- rbsa.dat[which(rbsa.dat$BuildingType == "Multifamily"),]
rbsa.dat.site <- rbsa.dat.MF[grep("site", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat.MF[grep("bldg", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
mechanical.dat <- read.xlsx(mechanical.export)
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
item279.dat4 <- item279.dat4[-grep("water heat",item279.dat4$Heating_System, ignore.case = T),]
item279.dat4$Heating.Fuel[which(item279.dat4$Heating.Fuel == "Natural gas")] <- "Natural Gas"
item279.dat4$Heating.Fuel[grep("wood",item279.dat4$Heating.Fuel, ignore.case = T)] <- "Wood"
item279.dat4 <- item279.dat4[which(item279.dat4$Heating.Fuel %notin% c("N/A", NA, "Unknown")),]
unique(item279.dat4$Heating.Fuel)
unique(item279.dat4$Heating_System)
names(item279.dat4)

######################################
#Pop and Sample Sizes for weights
######################################
item279.data <- weightedData(item279.dat4[which(colnames(item279.dat4) %notin% c("CK_SiteID"
                                                                                 ,"System.Type"
                                                                                 ,"System.Sub-Type"
                                                                                 ,"Primary.Heating.System"
                                                                                 ,"Heating.Fuel"
                                                                                 ,"count"
                                                                                 ,"Heating_System"))])

item279.data <- left_join(item279.data, item279.dat4[which(colnames(item279.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"System.Type"
                                                                                         ,"System.Sub-Type"
                                                                                         ,"Primary.Heating.System"
                                                                                         ,"Heating.Fuel"
                                                                                         ,"count"
                                                                                         ,"Heating_System"))])
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
                            ,"Wood"                  = item279.cast$`w.percent_Wood`
                            ,"Wood.SE"               = item279.cast$`w.SE_Wood`
                            ,"All.Types"             = item279.cast$`w.percent_All Types`
                            ,"All.Types.SE"          = item279.cast$`w.SE_All Types`
                            ,"n"                     = item279.cast$`n_All Types`
                            ,"Electric.EB"           = item279.cast$EB_Electric
                            ,"Gas.EB"                = item279.cast$`EB_Natural Gas`
                            ,"Wood.EB"               = item279.cast$`EB_Wood`
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

exportTable(item279.table, "MF", "Table 71", weighted = TRUE)



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

exportTable(item279.table, "MF", "Table 71", weighted = FALSE)







#############################################################################################
#Item 280: DISTRIBUTION OF SECONDARY IN-UNIT HEATING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
#############################################################################################
#subset to only primary heating rows
item280.dat3 <- unique(item279.dat2[which(item279.dat2$Primary.Heating.System == "No"),])
which(duplicated(item280.dat3$CK_Cadmus_ID))



item280.dat3$Heating_System <- item280.dat3$System.Type
item280.dat3$Heating_System[grep("baseboard",item280.dat3$Heating_System,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
item280.dat3$Heating_System[grep("fireplace",item280.dat3$Heating_System,ignore.case = T)] <- "Stove/Fireplace"
item280.dat3$Heating_System[grep("package",item280.dat3$Heating_System,ignore.case = T)] <- "Packaged HP"
item280.dat3$Heating_System[grep("ductless",item280.dat3$Heating_System,ignore.case = T)] <- "Mini-split HP"
item280.dat3$Heating_System[grep("boiler",item280.dat3$Heating_System,ignore.case = T)] <- "Boiler"
item280.dat3$Heating_System[grep("ceiling|radiant|zonal",item280.dat3$Heating_System,ignore.case = T)] <- "Other Zonal Heat"
item280.dat3$Heating_System[grep("plug-in",item280.dat3$Heating_System,ignore.case = T)] <- "Plug In Heaters"
unique(item280.dat3$Heating_System)


#remove NA in heating fuel types
unique(item280.dat3$Heating.Fuel)
item280.dat4 <- item280.dat3[-grep("other|unknown|hot water",item280.dat3$Heating.Fuel, ignore.case = T),]
item280.dat4$Heating.Fuel[which(item280.dat4$Heating.Fuel == "Natural gas")] <- "Natural Gas"
item280.dat4$Heating.Fuel[grep("wood",item280.dat4$Heating.Fuel, ignore.case = T)] <- "Wood"
item280.dat4 <- item280.dat4[which(item280.dat4$Heating.Fuel  %notin% c("N/A",NA,"None","Building Central System")),]
unique(item280.dat4$Heating.Fuel)
names(item280.dat4)


item280.merge <- left_join(rbsa.dat.site, item280.dat4)
# item280.merge <- item280.merge[which(item280.merge$Heating.Fuel %notin% c("N/A",NA,"None")),]
item280.merge$Heating.Fuel[which(item280.merge$Heating.Fuel %in% c("N/A",NA))] <- "None"
item280.merge$Heating_System[which(item280.merge$Heating_System %in% c("N/A",NA))] <- "None"
item280.merge$count <- 1

unique(item280.merge$Heating.Fuel)
######################################
#Pop and Sample Sizes for weights
######################################
item280.data <- weightedData(item280.merge[which(colnames(item280.merge) %notin% c("CK_SiteID"
                                                                                 ,"System.Type"
                                                                                 ,"System.Sub-Type"
                                                                                 ,"Primary.Heating.System"
                                                                                 ,"Heating.Fuel"
                                                                                 ,"count"
                                                                                 ,"Heating_System"))])

item280.data <- left_join(item280.data, item280.merge[which(colnames(item280.merge) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"System.Type"
                                                                                         ,"System.Sub-Type"
                                                                                         ,"Primary.Heating.System"
                                                                                         ,"Heating.Fuel"
                                                                                         ,"count"
                                                                                         ,"Heating_System"))])
item280.data$count <- 1
item280.data$Ind <- 1
length(unique(item280.data$CK_Cadmus_ID))

######################
# weighted analysis
######################
item280.summary <- proportionRowsAndColumns1(CustomerLevelData = item280.data
                                             ,valueVariable = 'Ind'
                                             ,columnVariable = 'Heating_System'
                                             ,rowVariable = 'Heating.Fuel'
                                             ,aggregateColumnName = "All Systems")
item280.summary$Heating.Fuel[which(item280.summary$Heating.Fuel == "Total")] <- "All Types"


item280.cast <- dcast(setDT(item280.summary)
                      ,formula = Heating_System ~ Heating.Fuel
                      ,value.var = c("w.percent", "w.SE","count","n","N","EB"))
names(item280.cast)
item280.table <- data.frame("Primary.Heating.System" = item280.cast$Heating_System
                            ,"Electric"              = item280.cast$w.percent_Electric
                            ,"Electric.SE"           = item280.cast$w.SE_Electric
                            ,"Gas"                   = item280.cast$`w.percent_Natural Gas`
                            ,"Gas.SE"                = item280.cast$`w.SE_Natural Gas`
                            ,"Wood"                  = item280.cast$`w.percent_Wood`
                            ,"Wood.SE"               = item280.cast$`w.SE_Wood`
                            ,"Propane"               = item280.cast$`w.percent_Propane`
                            ,"Propane.SE"            = item280.cast$`w.SE_Propane`
                            ,"None"                  = item280.cast$w.percent_None
                            ,"None.SE"               = item280.cast$w.SE_None
                            ,"All.Types"             = item280.cast$`w.percent_All Types`
                            ,"All.Types.SE"          = item280.cast$`w.SE_All Types`
                            ,"n"                     = item280.cast$`n_All Types`
                            ,"Electric.EB"           = item280.cast$EB_Electric
                            ,"Gas.EB"                = item280.cast$`EB_Natural Gas`
                            ,"Wood.EB"               = item280.cast$`EB_Wood`
                            ,"Propane.EB"            = item280.cast$`EB_Propane`
                            ,"None.EB"               = item280.cast$EB_None
                            ,"All.Types.EB"          = item280.cast$`EB_All Types`)

levels(item280.table$Primary.Heating.System)
rowOrder <- c("Electric Baseboard and Wall Heaters"
              ,"Furnace"
              ,"Mini-Split HP"
              ,"Stove/Fireplace"
              ,"Other Zonal Heat"
              ,"Plug In Heaters"
              ,"None"
              ,"All Systems")
item280.table <- item280.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item280.table <- data.frame(item280.table)

exportTable(item280.table, "MF", "Table 72", weighted = TRUE)



######################
# unweighted analysis
######################
item280.data$Fuel <- item280.data$Heating.Fuel
item280.summary <- proportions_two_groups_unweighted(CustomerLevelData = item280.data
                                                     ,valueVariable = 'count'
                                                     ,columnVariable = 'Heating_System'
                                                     ,rowVariable = 'Fuel'
                                                     ,aggregateColumnName = "All Systems")
item280.summary$Fuel[which(item280.summary$Fuel == "Total")] <- "All Types"

item280.cast <- dcast(setDT(item280.summary)
                      ,formula = Heating_System ~ Fuel
                      ,value.var = c("Percent", "SE","Count","n"))
names(item280.cast)
item280.table <- data.frame("Primary.Heating.System" = item280.cast$Heating_System
                            ,"Electric"              = item280.cast$Percent_Electric
                            ,"Electric.SE"           = item280.cast$SE_Electric
                            ,"Gas"                   = item280.cast$`Percent_Natural Gas`
                            ,"Gas.SE"                = item280.cast$`SE_Natural Gas`
                            ,"Propane"               = item280.cast$`Percent_Propane`
                            ,"Propane.SE"            = item280.cast$`SE_Propane`
                            ,"Wood"                  = item280.cast$`Percent_Wood`
                            ,"Wood.SE"               = item280.cast$`SE_Wood`
                            ,"None"                  = item280.cast$Percent_None
                            ,"None.SE"               = item280.cast$SE_None
                            ,"All.Types"             = item280.cast$`Percent_All Types`
                            ,"All.Types.SE"          = item280.cast$`SE_All Types`
                            ,"n"                     = item280.cast$`n_All Types`)

levels(item280.table$Primary.Heating.System)
rowOrder <- c("Electric Baseboard and Wall Heaters"
              ,"Furnace"
              ,"Mini-Split HP"
              ,"Stove/Fireplace"
              ,"Other Zonal Heat"
              ,"Plug In Heaters"
              ,"None"
              ,"All Systems")
item280.table <- item280.table %>% mutate(Primary.Heating.System = factor(Primary.Heating.System, levels = rowOrder)) %>% arrange(Primary.Heating.System)  
item280.table <- data.frame(item280.table)

exportTable(item280.table, "MF", "Table 72", weighted = FALSE)















#############################################################################################
#Item 281: DISTRIBUTION OF SECONDARY IN-UNIT COOLING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
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
                                                                                 ,"count"))])

item281.data <- left_join(item281.data, unique(item281.dat3[which(colnames(item281.dat3) %in% c("CK_Cadmus_ID"
                                                                                                ,"System.Type"
                                                                                                ,"Primary.Cooling.System"
                                                                                                ,"Ind"
                                                                                                ,"count"))]))
item281.data$Count <- 1


######################
# weighted analysis
######################
item281.final <- proportions_one_group(CustomerLevelData = item281.data
                                          ,valueVariable = 'Ind'
                                          ,groupingVariable = 'HomeType'
                                          ,total.name = 'All Sizes'
                                          ,weighted = TRUE)
item281.final <- item281.final[which(names(item281.final) != "BuildingType")]
exportTable(item281.final, "MF","Table 73",weighted = TRUE)

######################
# unweighted analysis
######################
item281.final <- proportions_one_group(CustomerLevelData = item281.data
                                          ,valueVariable = 'Ind'
                                          ,groupingVariable = 'HomeType'
                                          ,total.name = 'All Sizes'
                                          ,weighted = FALSE)
item281.final <- item281.final[which(names(item281.final) != "BuildingType")]
exportTable(item281.final, "MF","Table 73",weighted = FALSE)





#############################################################################################
#Item 282: DISTRIBUTION OF SECONDARY IN-UNIT COOLING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
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
                                                                                 ,"Heating_System"))])

item282.data <- left_join(item282.data, item282.dat4[which(colnames(item282.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"System.Type"
                                                                                         ,"Primary.Cooling.System"
                                                                                         ,"Ind"
                                                                                         ,"count"
                                                                                         ,"Heating_System"))])
item282.data$Count <- 1


######################
# weighted analysis
######################
item282.summary <- proportionRowsAndColumns1(CustomerLevelData = item282.data
                                             ,valueVariable = 'Ind'
                                             ,columnVariable = 'Heating_System'
                                             ,rowVariable = 'HomeType'
                                             ,aggregateColumnName = "All Systems")
item282.summary$HomeType[which(item282.summary$HomeType == "Total")] <- "All Sizes"


item282.cast <- dcast(setDT(item282.summary)
                      ,formula = Heating_System ~ HomeType
                      ,value.var = c("w.percent", "w.SE","count","n","N", "EB"))

item282.table <- data.frame("Cooling.Systems"          = item282.cast$Heating_System
                            ,"Low_Rise_1.3_percent"    = item282.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_SE"             = item282.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_n"              = item282.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_4.6_percent"    = item282.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_SE"             = item282.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_n"              = item282.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_7Plus_percent" = item282.cast$`w.percent_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE"            = item282.cast$`w.SE_Apartment Building (More than 6 floors)`
                            ,"Hight_Rise_n"            = item282.cast$`n_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_percent"       = item282.cast$`w.percent_All Sizes`
                            ,"All_Sizes_SE"            = item282.cast$`w.SE_All Sizes`
                            ,"All_Sizes_n"             = item282.cast$`n_All Sizes`
                            ,"Low_Rise_EB"             = item282.cast$`EB_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_EB"             = item282.cast$`EB_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_EB"            = item282.cast$`EB_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_EB"            = item282.cast$`EB_All Sizes`
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


exportTable(item282.table, "MF","Table 74",weighted = TRUE)


######################
# unweighted analysis
######################
item282.summary <- proportions_two_groups_unweighted(CustomerLevelData = item282.data
                                             ,valueVariable = 'Ind'
                                             ,columnVariable = 'System.Type'
                                             ,rowVariable = 'HomeType'
                                             ,aggregateColumnName = "All Systems")
item282.summary$HomeType[which(item282.summary$HomeType == "Total")] <- "All Sizes"


item282.cast <- dcast(setDT(item282.summary)
                      ,formula = System.Type ~ HomeType
                      ,value.var = c("Percent", "SE","Count","n"))

item282.table <- data.frame("Cooling.Systems"          = item282.cast$System.Type
                            ,"Low_Rise_1.3_Mean"       = item282.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_SE"             = item282.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_n"              = item282.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_4.6_Mean"       = item282.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_SE"             = item282.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_n"              = item282.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_7Plus_Mean"    = item282.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE"            = item282.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"Hight_Rise_n"            = item282.cast$`n_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_Mean"          = item282.cast$`Percent_All Sizes`
                            ,"All_Sizes_SE"            = item282.cast$`SE_All Sizes`
                            ,"All_Sizes_n"             = item282.cast$`n_All Sizes`)

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

exportTable(item282.table, "MF","Table 74",weighted = FALSE)
