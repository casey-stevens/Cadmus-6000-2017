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
source("Code/Table Code/Weighting Implementation - MF-BLDG.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
rbsa.dat.MF <- rbsa.dat[grep("bldg",rbsa.dat$CK_Building_ID,ignore.case = T),]

#Read in data for analysis
mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))


#############################################################################################
#Item 253: MF table 42
#############################################################################################
item253.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_SiteID"
                                                                    ,"Generic"
                                                                    ,"System.Type"
                                                                    ,"DHW.Fuel"))]
item253.dat1 <- item253.dat[grep("water heat", item253.dat$Generic,ignore.case = T),]

item253.dat2 <- left_join(rbsa.dat.MF, item253.dat1, by = c("CK_Building_ID" = "CK_SiteID"))

item253.dat3 <- item253.dat2[which(!is.na(item253.dat2$Generic)),]

######################################
#Pop and Sample Sizes for weights
######################################
item253.data <- weightedData(item253.dat3[which(colnames(item253.dat3) %notin% c("Generic"
                                                                                 ,"System.Type"
                                                                                 ,"DHW.Fuel"))])

item253.data <- left_join(item253.data, item253.dat3[which(colnames(item253.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"Generic"
                                                                                         ,"System.Type"
                                                                                         ,"DHW.Fuel"))])
item253.data$count <- 1


#########################
# weighted analysis
#########################
item253.summary <- proportionRowsAndColumns1(CustomerLevelData = item253.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = "System.Type"
                                             ,rowVariable = "DHW.Fuel"
                                             ,aggregateColumnName = "All Systems")
# item253.summary <- item253.summary[which(item253.summary$System.Type != "Total"),]
# 
# item253.all.types <- proportions_one_group(CustomerLevelData = item253.data
#                                            ,valueVariable = 'count'
#                                            ,groupingVariable = 'Heating.Fuel'
#                                            ,total.name = "All System Types"
#                                            ,columnName = 'System.Type'
#                                            ,weighted = TRUE
#                                            ,two.prop.total = TRUE)
# 
# item253.final <- rbind.data.frame(item253.summary, item253.all.types)

item253.cast <- dcast(setDT(item253.summary)
                      ,formula = BuildingType + System.Type ~ DHW.Fuel
                      ,value.var = c("w.percent","w.SE","count","n","N","EB"))
names(item253.cast)
item253.table <- data.frame("System.Type"          = item253.cast$System.Type
                            ,"Percent.Electric"    = item253.cast$w.percent_Electric
                            ,"SE.Electric"         = item253.cast$w.SE_Electric
                            ,"Percent.Natrual.Gas" = item253.cast$`w.percent_Natural Gas`
                            ,"SE.Natrual.Gas"      = item253.cast$`w.SE_Natural Gas`
                            ,"Percent.Unknown"     = item253.cast$w.percent_Unknown
                            ,"SE.Unknown"          = item253.cast$w.SE_Unknown
                            ,"n"                   = item253.cast$n_Total
                            ,"EB.Electric"         = item253.cast$EB_Electric
                            ,"EB.Natrual.Gas"      = item253.cast$`EB_Natural Gas`
                            ,"EB.Unknown"          = item253.cast$EB_Unknown)
levels(item253.table$System.Type)
rowOrder <- c("Storage Water Heater"
              ,"All Systems")
item253.table <- item253.table %>% mutate(System.Type = factor(System.Type, levels = rowOrder)) %>% arrange(System.Type)  
item253.table <- data.frame(item253.table)

exportTable(item253.table, "MF", "Table 45", weighted = TRUE)

#########################
# weighted analysis
#########################
item253.summary <- proportions_two_groups_unweighted(CustomerLevelData = item253.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = "System.Type"
                                             ,rowVariable = "DHW.Fuel"
                                             ,aggregateColumnName = "All Systems")
# item253.summary <- item253.summary[which(item253.summary$System.Type != "Total"),]
# 
# item253.all.types <- proportions_one_group(CustomerLevelData = item253.data
#                                            ,valueVariable = 'count'
#                                            ,groupingVariable = 'Heating.Fuel'
#                                            ,total.name = "All System Types"
#                                            ,columnName = 'System.Type'
#                                            ,weighted = FALSE
#                                            ,two.prop.total = TRUE)
# 
# item253.final <- rbind.data.frame(item253.summary, item253.all.types)

item253.cast <- dcast(setDT(item253.summary)
                      ,formula = BuildingType + System.Type ~ DHW.Fuel
                      ,value.var = c("Percent","SE","Count","n"))
names(item253.cast)
item253.table <- data.frame("System.Type" = item253.cast$System.Type
                            ,"Percent.Electric" = item253.cast$Percent_Electric
                            ,"SE.Electric"      = item253.cast$SE_Electric
                            ,"Percent.Natrual.Gas" = item253.cast$`Percent_Natural Gas`
                            ,"SE.Natrual.Gas"      = item253.cast$`SE_Natural Gas`
                            ,"Percent.Unknown" = item253.cast$Percent_Unknown
                            ,"SE.Unknown"      = item253.cast$SE_Unknown
                            ,"n"                 = item253.cast$n_Total)
levels(item253.table$System.Type)
rowOrder <- c("Storage Water Heater"
              ,"All Systems")
item253.table <- item253.table %>% mutate(System.Type = factor(System.Type, levels = rowOrder)) %>% arrange(System.Type)  
item253.table <- data.frame(item253.table)

exportTable(item253.table, "MF", "Table 45", weighted = FALSE)

