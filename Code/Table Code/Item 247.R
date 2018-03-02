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
one.line.bldg.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), sheet = 1, startRow = 3)
#clean cadmus IDs
one.line.bldg.dat$CK_Building_ID <- trimws(toupper(one.line.bldg.dat$PK_BuildingID))


#############################################################################################
#Item 247
#############################################################################################
item247.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Common.Area.Primary.Heating.System"
                                                                          ,"Common.Area.Primary.Heating.Fuel"))]
unique(item247.dat$Common.Area.Primary.Heating.Fuel)
item247.dat1 <- item247.dat[which(item247.dat$Common.Area.Primary.Heating.Fuel %notin% c("N/A","Unknown")),]

item247.merge <- left_join(rbsa.dat.MF, item247.dat1)
item247.merge <- item247.merge[which(item247.merge$Common.Area.Primary.Heating.Fuel %notin% c("N/A",NA)),]

unique(item247.merge$Common.Area.Primary.Heating.System)

######################################
#Pop and Sample Sizes for weights
######################################
item247.data <- weightedData(item247.merge[which(colnames(item247.merge) %notin% c("Common.Area.Primary.Heating.System"
                                                                                   ,"Common.Area.Primary.Heating.Fuel"))])

item247.data <- left_join(item247.data, item247.merge[which(colnames(item247.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Common.Area.Primary.Heating.System"
                                                                                           ,"Common.Area.Primary.Heating.Fuel"))])
item247.data$count <- 1

item247.data$Heating_System <- item247.data$Common.Area.Primary.Heating.System
item247.data$Fuel           <- item247.data$Common.Area.Primary.Heating.Fuel
#########################
# weighted analysis
#########################
item247.summary <- proportionRowsAndColumns1(CustomerLevelData = item247.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = "Heating_System"
                                             ,rowVariable = "Fuel"
                                             ,aggregateColumnName = "Total")

item247.cast <- dcast(setDT(item247.summary)
                      ,formula = BuildingType + Heating_System ~ Fuel
                      ,value.var = c("w.percent","w.SE","count","n","N","EB"))
names(item247.cast)

item247.table <- data.frame("Common.Area.Primary.Heating.System" = item247.cast$Heating_System
                            ,"Percent.Electric"  = item247.cast$w.percent_Electric
                            ,"SE.Electric"       = item247.cast$w.SE_Electric
                            ,"Percent.Gas"       = item247.cast$`w.percent_Natural Gas`
                            ,"SE.Gas"            = item247.cast$`w.SE_Natural Gas`
                            ,"Percent.All.Fuels" = item247.cast$w.percent_Total
                            ,"SE.All.Fuels"      = item247.cast$w.SE_Total
                            ,"n"                 = item247.cast$n_Total
                            ,"EB.Electric"       = item247.cast$EB_Electric
                            ,"EB.Gas"            = item247.cast$`EB_Natural Gas`
                            ,"EB.All.Fuels"      = item247.cast$EB_Total)
levels(item247.table$Common.Area.Primary.Heating.System)
rowOrder <- c("Boiler"
              ,"Electric Baseboard"
              ,"Furnace"
              ,"Mini-split HP"
              ,"Radiant Ceiling Heat"
              ,"Zonal Heat"
              ,"Total")
item247.table <- item247.table %>% mutate(Common.Area.Primary.Heating.System = factor(Common.Area.Primary.Heating.System, levels = rowOrder)) %>% arrange(Common.Area.Primary.Heating.System)  
item247.table <- data.frame(item247.table)

exportTable(item247.table, "MF", "Table 39", weighted = TRUE)

#########################
# weighted analysis
#########################
item247.summary <- proportions_two_groups_unweighted(CustomerLevelData = item247.data
                                                     ,valueVariable = 'count'
                                                     ,columnVariable = "Heating_System"
                                                     ,rowVariable = "Fuel"
                                                     ,aggregateColumnName = "Total")

item247.cast <- dcast(setDT(item247.summary)
                      ,formula = BuildingType + Heating_System ~ Fuel
                      ,value.var = c("Percent","SE","Count","n"))

item247.table <- data.frame("Common.Area.Primary.Heating.System" = item247.cast$Heating_System
                            ,"Percent.Electric"  = item247.cast$Percent_Electric
                            ,"SE.Electric"       = item247.cast$SE_Electric
                            ,"Percent.Gas"       = item247.cast$`Percent_Natural Gas`
                            ,"SE.Gas"            = item247.cast$`SE_Natural Gas`
                            ,"Percent.All.Fuels" = item247.cast$`Percent_Total`
                            ,"SE.All.Fuels"      = item247.cast$`SE_Total`
                            ,"n"                 = item247.cast$`n_Total`)

levels(item247.table$Common.Area.Primary.Heating.System)
rowOrder <- c("Boiler"
              ,"Electric Baseboard"
              ,"Furnace"
              ,"Mini-split HP"
              ,"Radiant Ceiling Heat"
              ,"Zonal Heat"
              ,"Total")
item247.table <- item247.table %>% mutate(Common.Area.Primary.Heating.System = factor(Common.Area.Primary.Heating.System, levels = rowOrder)) %>% arrange(Common.Area.Primary.Heating.System)  
item247.table <- data.frame(item247.table)

exportTable(item247.table, "MF", "Table 39", weighted = FALSE)

