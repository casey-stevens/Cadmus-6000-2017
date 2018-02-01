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
rbsa.dat.MF <- rbsa.dat.MF[grep("bldg",rbsa.dat.MF$CK_Building_ID,ignore.case = T),]

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Building_ID <- trimws(toupper(mechanical.dat$CK_SiteID))


#############################################################################################
#Item 247
#############################################################################################
item247.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Building_ID"
                                                                    ,"Primary.Heating.System"
                                                                    ,"Heating.Fuel"
                                                                    ,"System.Type"))]
item247.dat1 <- item247.dat[which(item247.dat$Heating.Fuel %notin% c("N/A",NA,"Unknown","Can't Determine")),]

item247.merge <- left_join(rbsa.dat.MF, item247.dat1)
item247.merge <- item247.merge[which(item247.merge$Heating.Fuel %notin% c("N/A",NA)),]

item247.dat2 <- item247.merge[which(item247.merge$Primary.Heating.System == "Yes"),]
item247.dat2$System.Type[grep("baseboard",item247.dat2$System.Type,ignore.case = T)] <- "Electric Baseboard"
item247.dat2$System.Type[grep("mini-split|mini split",item247.dat2$System.Type,ignore.case = T)] <- "Mini-split HP"

######################################
#Pop and Sample Sizes for weights
######################################
item247.data <- weightedData(item247.dat2[which(colnames(item247.dat2) %notin% c("System.Type"
                                                                                 ,"Primary.Heating.System"
                                                                                 ,"Heating.Fuel"))])

item247.data <- left_join(item247.data, item247.dat2[which(colnames(item247.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"System.Type"
                                                                                         ,"Primary.Heating.System"
                                                                                         ,"Heating.Fuel"))])
item247.data$count <- 1


#########################
# weighted analysis
#########################
item247.summary <- proportionRowsAndColumns1(CustomerLevelData = item247.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = "Heating.Fuel"
                                             ,rowVariable = "System.Type"
                                             ,aggregateColumnName = "All Fuel Types")
# item247.summary <- item247.summary[which(item247.summary$System.Type != "Total"),]
# 
# item247.all.types <- proportions_one_group(CustomerLevelData = item247.data
#                                            ,valueVariable = 'count'
#                                            ,groupingVariable = 'Heating.Fuel'
#                                            ,total.name = "All System Types"
#                                            ,columnName = 'System.Type'
#                                            ,weighted = TRUE
#                                            ,two.prop.total = TRUE)
# 
# item247.final <- rbind.data.frame(item247.summary, item247.all.types)

item247.cast <- dcast(setDT(item247.summary)
                      ,formula = BuildingType + System.Type ~ Heating.Fuel
                      ,value.var = c("w.percent","w.SE","count","n","N","EB"))

item247.table <- data.frame("System.Type" = item247.cast$System.Type
                            ,"Percent.Electric" = item247.cast$w.percent_Electric
                            ,"SE.Electric"      = item247.cast$w.SE_Electric
                            ,"Percent.All.Fuels" = item247.cast$`w.percent_All Fuel Types`
                            ,"SE.All.Fuels"      = item247.cast$`w.SE_All Fuel Types`
                            ,"n"                 = item247.cast$`n_All Fuel Types`)
levels(item247.table$System.Type)
rowOrder <- c("Boiler"
              ,"Electric Baseboard"
              ,"Furnace"
              ,"Mini-split HP"
              ,"Radiant Ceiling Heat"
              ,"Zonal Heat"
              ,"Total")
item247.table <- item247.table %>% mutate(System.Type = factor(System.Type, levels = rowOrder)) %>% arrange(System.Type)  
item247.table <- data.frame(item247.table)

exportTable(item247.table, "MF", "Table 39", weighted = TRUE)

#########################
# weighted analysis
#########################
item247.summary <- proportions_two_groups_unweighted(CustomerLevelData = item247.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = "Heating.Fuel"
                                             ,rowVariable = "System.Type"
                                             ,aggregateColumnName = "All Fuel Types")
# item247.summary <- item247.summary[which(item247.summary$System.Type != "Total"),]
# 
# item247.all.types <- proportions_one_group(CustomerLevelData = item247.data
#                                            ,valueVariable = 'count'
#                                            ,groupingVariable = 'Heating.Fuel'
#                                            ,total.name = "All System Types"
#                                            ,columnName = 'System.Type'
#                                            ,weighted = FALSE
#                                            ,two.prop.total = TRUE)
# 
# item247.final <- rbind.data.frame(item247.summary, item247.all.types)

item247.cast <- dcast(setDT(item247.summary)
                      ,formula = BuildingType + System.Type ~ Heating.Fuel
                      ,value.var = c("Percent","SE","Count","n"))

item247.table <- data.frame("System.Type" = item247.cast$System.Type
                            ,"Percent.Electric" = item247.cast$Percent_Electric
                            ,"SE.Electric"      = item247.cast$SE_Electric
                            ,"Percent.All.Fuels" = item247.cast$`Percent_All Fuel Types`
                            ,"SE.All.Fuels"      = item247.cast$`SE_All Fuel Types`
                            ,"n"                 = item247.cast$`n_All Fuel Types`)
levels(item247.table$System.Type)
rowOrder <- c("Boiler"
              ,"Electric Baseboard"
              ,"Furnace"
              ,"Mini-split HP"
              ,"Radiant Ceiling Heat"
              ,"Zonal Heat"
              ,"Total")
item247.table <- item247.table %>% mutate(System.Type = factor(System.Type, levels = rowOrder)) %>% arrange(System.Type)  
item247.table <- data.frame(item247.table)

exportTable(item247.table, "MF", "Table 39", weighted = FALSE)

