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
length(unique(rbsa.dat$CK_Cadmus_ID)) 

#Read in data for analysis
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))

#Read in data for analysis
survey.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Labeled and Translated")
#clean cadmus IDs
survey.dat$CK_Cadmus_ID <- trimws(toupper(survey.dat$NEXID))



#############################################################################################
#Item 128: DISTRIBUTION OF HOMES WITH GAS FUEL ASSISTANCE BY PERCENTAGE OF ASSISTANCE AND STATE (SF table 135, MH table 110)
#############################################################################################
#subset to columns needed for analysis
item128.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Does.your.household.receive.financial.assistance.to.pay.a.portion.or.all.of.your.gas.bill?"
                                                            ,"For.what.share.does.your.household.receive.assistance?.Gas"))]
colnames(item128.dat) <- c("Financial.Assistance", "Percent.Assistance", "CK_Cadmus_ID")

#merge together analysis data with cleaned RBSA data
item128.dat1 <- left_join(rbsa.dat, item128.dat, by = "CK_Cadmus_ID")
item128.dat1$Percent.Assistance[which(item128.dat1$Financial.Assistance == "No")] <- "No Utility Bill Assistance"
unique(item128.dat1$Percent.Assistance)

item128.dat2 <- item128.dat1[which(!(is.na(item128.dat1$Percent.Assistance))),]
item128.dat3 <- item128.dat2[which(item128.dat2$Percent.Assistance %notin% c("Don't know"
                                                                             ,"Prefer not to say")),]
################################################
# Adding pop and sample sizes for weights
################################################
item128.data <- weightedData(item128.dat3[-which(colnames(item128.dat3) %in% c("Financial.Assistance"
                                                                               ,"Percent.Assistance"))])
item128.data <- left_join(item128.data, item128.dat3[which(colnames(item128.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"Financial.Assistance"
                                                                                         ,"Percent.Assistance"))])
item128.data$count <- 1
#######################
# Weighted Analysis
#######################
item128.final <- proportionRowsAndColumns1(CustomerLevelData = item128.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'Percent.Assistance'
                                           ,aggregateColumnName = "Region")

item128.cast <- dcast(setDT(item128.final)
                      , formula = BuildingType + Percent.Assistance ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item128.table <- data.frame("BuildingType"    = item128.cast$BuildingType
                            ,"Percent.Assistance" = item128.cast$Percent.Assistance
                            ,"Percent_ID"     = item128.cast$w.percent_ID
                            ,"SE_ID"          = item128.cast$w.SE_ID
                            ,"n_ID"           = item128.cast$n_ID
                            ,"Percent_MT"     = item128.cast$w.percent_MT
                            ,"SE_MT"          = item128.cast$w.SE_MT
                            ,"n_MT"           = item128.cast$n_MT
                            ,"Percent_OR"     = item128.cast$w.percent_OR
                            ,"SE_OR"          = item128.cast$w.SE_OR
                            ,"n_OR"           = item128.cast$n_OR
                            ,"Percent_WA"     = item128.cast$w.percent_WA
                            ,"SE_WA"          = item128.cast$w.SE_WA
                            ,"n_WA"           = item128.cast$n_WA
                            ,"Percent_Region" = item128.cast$w.percent_Region
                            ,"SE_Region"      = item128.cast$w.SE_Region
                            ,"n_Region"       = item128.cast$n_Region
)

# row ordering example code
unique(item128.table$Percent.Assistance)
rowOrder <- c("Less than 25%"
              ,"Between 26% and 50%"
              ,"Between 51% and 75%"
              ,"Between 76% and 100%"
              ,"No Utility Bill Assistance"
              ,"Total")
item128.table <- item128.table %>% mutate(Percent.Assistance = factor(Percent.Assistance, levels = rowOrder)) %>% arrange(Percent.Assistance)  
item128.table <- data.frame(item128.table)


item128.final.SF <- item128.table[which(item128.table$BuildingType == "Single Family")
                                  ,-which(colnames(item128.table) %in% c("BuildingType"))]
item128.final.MH <- item128.table[which(item128.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item128.table) %in% c("BuildingType"))]

exportTable(item128.final.SF, "SF", "Table 135", weighted = TRUE)
exportTable(item128.final.MH, "MH", "Table 110", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item128.final <- proportions_two_groups_unweighted(CustomerLevelData = item128.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'Percent.Assistance'
                                                   ,aggregateColumnName = "Region")

item128.cast <- dcast(setDT(item128.final)
                      , formula = BuildingType + Percent.Assistance ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


item128.table <- data.frame("BuildingType"    = item128.cast$BuildingType
                            ,"Percent.Assistance"      = item128.cast$Percent.Assistance
                            ,"Percent_ID"     = item128.cast$Percent_ID
                            ,"SE_ID"          = item128.cast$SE_ID
                            ,"n_ID"           = item128.cast$n_ID
                            ,"Percent_MT"     = item128.cast$Percent_MT
                            ,"SE_MT"          = item128.cast$SE_MT
                            ,"n_MT"           = item128.cast$n_MT
                            ,"Percent_OR"     = item128.cast$Percent_OR
                            ,"SE_OR"          = item128.cast$SE_OR
                            ,"n_OR"           = item128.cast$n_OR
                            ,"Percent_WA"     = item128.cast$Percent_WA
                            ,"SE_WA"          = item128.cast$SE_WA
                            ,"n_WA"           = item128.cast$n_WA
                            ,"Percent_Region" = item128.cast$Percent_Region
                            ,"SE_Region"      = item128.cast$SE_Region
                            ,"n_Region"       = item128.cast$n_Region
)


item128.final.SF <- item128.table[which(item128.table$BuildingType == "Single Family")
                                  ,-which(colnames(item128.table) %in% c("BuildingType"))]
item128.final.MH <- item128.table[which(item128.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item128.table) %in% c("BuildingType"))]

exportTable(item128.final.SF, "SF", "Table 135", weighted = FALSE)
exportTable(item128.final.MH, "MH", "Table 110", weighted = FALSE)







#############################################################################################
#Item 129: AVERAGE HEATING THERMOSTAT SETPOINT BY STATE (SF table 136, MH table 111)
#############################################################################################
#subset to columns needed for analysis
item129.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouHeatYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,""))])
colnames(item129.dat) <- c("CK_Cadmus_ID", "Thermostat_Setpoint")
item129.dat$count <- 1

#remove any repeat header rows from exporting
item129.dat0 <- item129.dat[which(item129.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item129.dat1 <- left_join(rbsa.dat, item129.dat0, by = "CK_Cadmus_ID")

unique(item129.dat1$Thermostat_Setpoint)

item129.dat2.0 <- item129.dat1[which(!(is.na(item129.dat1$Thermostat_Setpoint))),]
item129.dat2 <- item129.dat2.0[which(item129.dat2.0$Thermostat_Setpoint != 0),]
colnames(item129.dat2)

################################################
# Adding pop and sample sizes for weights
################################################
item129.data <- weightedData(item129.dat2[-which(colnames(item129.dat2) %in% c("Thermostat_Setpoint"
                                                                                 ,"count"))])
item129.data <- left_join(item129.data, item129.dat2[which(colnames(item129.dat2) %in% c("CK_Cadmus_ID"
                                                                                           ,"Thermostat_Setpoint"
                                                                                           ,"count"))])

item129.data$count <- 1
#######################
# Weighted Analysis
#######################
item129.final <- mean_one_group(item129.data
                                ,valueVariable = 'Thermostat_Setpoint'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item129.final.SF <- item129.final[which(item129.final$BuildingType == "Single Family")
                                  ,-which(colnames(item129.final) %in% c("BuildingType"
                                                                         ,"Count"))]
item129.final.MH <- item129.final[which(item129.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item129.final) %in% c("BuildingType"
                                                                         ,"Count"))]

exportTable(item129.final.SF, "SF", "Table 136", weighted = TRUE)
exportTable(item129.final.MH, "MH", "Table 111", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item129.final <- mean_one_group_unweighted(item129.data
                                           ,valueVariable = 'Thermostat_Setpoint'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item129.final.SF <- item129.final[which(item129.final$BuildingType == "Single Family")
                                  ,-which(colnames(item129.final) %in% c("BuildingType"))]
item129.final.MH <- item129.final[which(item129.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item129.final) %in% c("BuildingType"))]

exportTable(item129.final.SF, "SF", "Table 136", weighted = FALSE)
exportTable(item129.final.MH, "MH", "Table 111", weighted = FALSE)







#############################################################################################
#Item 130: PERCENTAGE OF HOMES REPORTING A HEATING SETBACK BY STATE (SF table 137, MH table 112)
#############################################################################################
#subset to columns needed for analysis
item130.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouHeatYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouGoToBedWhatDoYouSetTheThermostatToForHeating"))])
colnames(item130.dat) <- c("CK_Cadmus_ID", "Nighttime_Heating", "Thermostat_Setpoint")

#remove any repeat header rows from exporting
item130.dat0 <- item130.dat[which(item130.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item130.dat1 <- left_join(item130.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item130.dat2.0 <- item130.dat1[which(!(is.na(item130.dat1$Thermostat_Setpoint))),]
item130.dat2 <- item130.dat2.0[which(item130.dat2.0$Thermostat_Setpoint != 0),]
unique(item130.dat2$Thermostat_Setpoint)
unique(item130.dat2$Nighttime_Heating)

item130.dat3.0 <- item130.dat2[which(!(is.na(item130.dat2$Nighttime_Heating))),]
item130.dat3 <- item130.dat3.0[which(item130.dat3.0$Nighttime_Heating != 0),]

item130.dat3$Heating.Setback <- 0
item130.dat3$Heating.Setback[which(item130.dat3$Nighttime_Heating < item130.dat3$Thermostat_Setpoint)] <- 1

item130.sum <- summarise(group_by(item130.dat3, CK_Cadmus_ID)
                         ,Ind = sum(Heating.Setback))

item130.merge <- left_join(rbsa.dat, item130.sum)
item130.merge <- item130.merge[which(!is.na(item130.merge$Ind)),]


################################################
# Adding pop and sample sizes for weights
################################################
item130.data <- weightedData(item130.merge[-which(colnames(item130.merge) %in% c("Ind"))])
item130.data <- left_join(item130.data, item130.merge[which(colnames(item130.merge) %in% c("CK_Cadmus_ID"
                                                                                         ,"Ind"))])
item130.data$count <- 1
item130.data$Count <- 1
#######################
# Weighted Analysis
#######################
item130.final <- proportions_one_group(CustomerLevelData = item130.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = TRUE
                                       ,two.prop.total = NA)

item130.final.SF <- item130.final[which(item130.final$BuildingType == "Single Family")
                                  ,-which(colnames(item130.final) %in% c("BuildingType"))]
item130.final.MH <- item130.final[which(item130.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item130.final) %in% c("BuildingType"))]

exportTable(item130.final.SF, "SF", "Table 137", weighted = TRUE)
exportTable(item130.final.MH, "MH", "Table 112", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item130.final <- proportions_one_group(CustomerLevelData = item130.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = FALSE
                                       ,two.prop.total = NA)

item130.final.SF <- item130.final[which(item130.final$BuildingType == "Single Family")
                                  ,-which(colnames(item130.final) %in% c("BuildingType"
                                                                         ,"Remove"))]
item130.final.MH <- item130.final[which(item130.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item130.final) %in% c("BuildingType"
                                                                         ,"Remove"))]

exportTable(item130.final.SF, "SF", "Table 137", weighted = FALSE)
exportTable(item130.final.MH, "MH", "Table 112", weighted = FALSE)






#############################################################################################
#Item 131: AVERAGE SIZE OF HEATING SETBACK BY STATE (SF table 138, MH table 113)
#############################################################################################
#subset to columns needed for analysis
item131.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouHeatYourHomeWhatTemperatureDoYouTryToMaintain"
                                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseTemp_WhenYouGoToBedWhatDoYouSetTheThermostatToForHeating"))])
colnames(item131.dat) <- c("CK_Cadmus_ID", "Nighttime_Heating", "Thermostat_Setpoint")
item131.dat$count <- 1

#remove any repeat header rows from exporting
item131.dat0 <- item131.dat[which(item131.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item131.dat1 <- left_join(item131.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item131.dat2.0 <- item131.dat1[which(!(is.na(item131.dat1$Thermostat_Setpoint))),]
item131.dat2 <- item131.dat2.0[which(item131.dat2.0$Thermostat_Setpoint != 0),]
unique(item131.dat2$Thermostat_Setpoint)
unique(item131.dat2$Nighttime_Heating)

item131.dat3.0 <- item131.dat2[which(!(is.na(item131.dat2$Nighttime_Heating))),]
item131.dat3 <- item131.dat3.0[which(item131.dat3.0$Nighttime_Heating != 0),]

item131.dat3$Heating.Setback <- item131.dat3$Thermostat_Setpoint - item131.dat3$Nighttime_Heating
item131.dat4 <- item131.dat3[which(colnames(item131.dat3) %in% c("CK_Cadmus_ID", "Heating.Setback"))]

item131.merge <- left_join(rbsa.dat, item131.dat4)
item131.merge <- item131.merge[which(!is.na(item131.merge$Heating.Setback)),]

################################################
# Adding pop and sample sizes for weights
################################################
item131.data <- weightedData(item131.merge[-which(colnames(item131.merge) %in% c("Heating.Setback"))])
item131.data <- left_join(item131.data, item131.merge[which(colnames(item131.merge) %in% c("CK_Cadmus_ID"
                                                                                         ,"Heating.Setback"))])

item131.data$count <- 1
#######################
# Weighted Analysis
#######################
item131.final <- mean_one_group(item131.data
                                ,valueVariable = 'Heating.Setback'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item131.final.SF <- item131.final[which(item131.final$BuildingType == "Single Family")
                                  ,-which(colnames(item131.final) %in% c("BuildingType"
                                                                         ,"Count"))]
item131.final.MH <- item131.final[which(item131.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item131.final) %in% c("BuildingType"
                                                                         ,"Count"))]

exportTable(item131.final.SF, "SF", "Table 138", weighted = TRUE)
exportTable(item131.final.MH, "MH", "Table 113", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item131.final <- mean_one_group_unweighted(item131.data
                                           ,valueVariable = 'Heating.Setback'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item131.final.SF <- item131.final[which(item131.final$BuildingType == "Single Family")
                                  ,-which(colnames(item131.final) %in% c("BuildingType"))]
item131.final.MH <- item131.final[which(item131.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item131.final) %in% c("BuildingType"))]

exportTable(item131.final.SF, "SF", "Table 138", weighted = FALSE)
exportTable(item131.final.MH, "MH", "Table 113", weighted = FALSE)
