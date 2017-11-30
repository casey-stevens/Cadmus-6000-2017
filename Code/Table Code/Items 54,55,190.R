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


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))





################################################################################################
# ITEM 54: PERCENTAGE OF HOMES WITH COOLING EQUIPMENT BY COOLING ZONE AND STATE (SF table 61)
################################################################################################
#subset to columns needed for analysis
item54.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        # ,"Generic"
                                                                        ,"Primary.Cooling.System"
                                                                        # ,"Heating.Fuel"
                                                                        # ,"Heating.Efficiency.-.High"
                                                                        # ,"Component.1.Year.of.Manufacture"
                                                                        # ,"HSPF"
                                                                        ))]
item54.dat1 <- unique(item54.dat[which(item54.dat$Primary.Cooling.System == "Yes"),])
which(duplicated(item54.dat1$CK_Cadmus_ID))
item54.dat2 <- left_join(rbsa.dat, item54.dat1)

item54.dat2$Primary.Cooling.System[which(is.na(item54.dat2$Primary.Cooling.System))] <- "No"
item54.dat2$Ind <- 0
item54.dat2$Ind[which(item54.dat2$Primary.Cooling.System == "Yes")] <- 1


##########################################
# add pop and sample sizes by strata
##########################################
item54.data <- weightedData(item54.dat2[-which(colnames(item54.dat2) %in% c("Primary.Cooling.System"
                                                                            ,"Ind"))])
item54.data <- left_join(item54.data, item54.dat2[which(colnames(item54.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"Primary.Cooling.System"
                                                                                     ,"Ind"))])
item54.data$Count <- 1
##############################
# Weighted Analysis
##############################
item54.summary <- proportionRowsAndColumns1(CustomerLevelData = item54.data
                                            ,valueVariable = 'Ind'
                                            ,columnVariable = 'Cooling.Zone'
                                            ,rowVariable = 'State'
                                            ,aggregateColumnName = "Remove")
item54.summary$State[which(item54.summary$State == "Total")] <- "Region"
item54.summary <- item54.summary[which(item54.summary$Cooling.Zone != "Remove"),]

item54.all.cooling.zones <- proportions_one_group(CustomerLevelData = item54.data
                                                  ,valueVariable = 'Ind'
                                                  ,groupingVariable = 'State'
                                                  ,total.name = 'All Cooling Zones'
                                                  ,columnName = 'Cooling.Zone'
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE)

item54.final <- rbind.data.frame(item54.summary, item54.all.cooling.zones, stringsAsFactors = F)

item54.cast <- dcast(setDT(item54.final)
                     ,formula = BuildingType + Cooling.Zone ~ State
                     ,value.var = c("w.percent","w.SE","count","N","n"))

item54.table <- data.frame("BuildingType"    = item54.cast$BuildingType
                           ,"Cooling.Zone"   = item54.cast$Cooling.Zone
                           ,"Percent_ID"     = item54.cast$w.percent_ID
                           ,"SE_ID"          = item54.cast$w.SE_ID
                           ,"n_ID"           = item54.cast$n_ID
                           ,"Percent_MT"     = item54.cast$w.percent_MT
                           ,"SE_MT"          = item54.cast$w.SE_MT
                           ,"n_MT"           = item54.cast$n_MT
                           ,"Percent_OR"     = item54.cast$w.percent_OR
                           ,"SE_OR"          = item54.cast$w.SE_OR
                           ,"n_OR"           = item54.cast$n_OR
                           ,"Percent_WA"     = item54.cast$w.percent_WA
                           ,"SE_WA"          = item54.cast$w.SE_WA
                           ,"n_WA"           = item54.cast$n_WA
                           ,"Percent_Region" = item54.cast$w.percent_Region
                           ,"SE_Region"      = item54.cast$w.SE_Region
                           ,"n_Region"       = item54.cast$n_Region)
