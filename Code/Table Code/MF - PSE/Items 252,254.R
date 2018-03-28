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
source("Code/Table Code/Weighting Implementation - MF-BLDG.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
rbsa.dat.bldg <- rbsa.dat[grep("bldg", rbsa.dat$CK_Building_ID, ignore.case = T),]
length(unique(rbsa.dat$CK_Cadmus_ID)) 

#Read in data for analysis
# mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))
mechanical.dat$CK_Building_ID <- trimws(toupper(mechanical.dat$CK_SiteID))

one.line.bldg.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), startRow = 2)
one.line.bldg.dat$CK_Building_ID <- one.line.bldg.dat$PK_BuildingID
#############################################################################################
#Item 252: DISTRIBUTION OF DHW SERVICE TYPE BY BUILDING SIZE (MF Table 44)
#############################################################################################
#subset to columns needed for analysis
item252.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID","Water.Heater.In-Unit?", "Associated.Site.1"))]
names(item252.dat) <- c("CK_Cadmus_ID","Water.Heater.In.Unit", "CK_Building_ID")
which(duplicated(item252.dat$CK_Cadmus_ID))

item252.dat1 <- left_join(rbsa.dat.bldg, item252.dat)

#Subset to Multifamily
item252.dat2 <- item252.dat1[grep("Multifamily", item252.dat1$BuildingType),]

item252.dat3 <- item252.dat2[which(!is.na(item252.dat2$Water.Heater.In.Unit)),]

item252.dat3$DHW.Location <- item252.dat3$Water.Heater.In.Unit
item252.dat3$DHW.Location[which(item252.dat3$Water.Heater.In.Unit == "No")] <- "Common Area Water Heater"
item252.dat3$DHW.Location[which(item252.dat3$Water.Heater.In.Unit == "Yes")] <- "In-Unit Water Heater"
unique(item252.dat3$DHW.Location)

################################################
# Adding pop and sample sizes for weights
################################################
item252.data <- weightedData(item252.dat3[-which(colnames(item252.dat3) %in% c("Water.Heater.In.Unit"
                                                                               ,"DHW.Location"
                                                                               ,"Category"))])
item252.data <- left_join(item252.data, item252.dat3[which(colnames(item252.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"Water.Heater.In.Unit"
                                                                                         ,"DHW.Location"
                                                                                         ,"Category"))])
item252.data$count <- 1
colnames(item252.data)

#######################
# Weighted Analysis
#######################
item252.summary <- proportionRowsAndColumns1(CustomerLevelData = item252.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'Category'
                                             ,rowVariable = 'DHW.Location'
                                             ,aggregateColumnName = "Remove")
item252.summary <- item252.summary[which(item252.summary$Category != "Remove"),]

item252.cast <- dcast(setDT(item252.summary)
                      ,formula = DHW.Location ~ Category
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item252.table <- data.frame("DHW.Location" = item252.cast$DHW.Location
                            ,"PSE.Percent"                 = item252.cast$w.percent_PSE
                            ,"PSE.SE"                      = item252.cast$w.SE_PSE
                            ,"PSE.n"                       = item252.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item252.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item252.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item252.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item252.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item252.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item252.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item252.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item252.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = item252.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = item252.cast$EB_PSE
                            ,"PSE.King.County_EB"          = item252.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = item252.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = item252.cast$`EB_2017 RBSA PS`)

exportTable(item252.table, "MF", "Table 44", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# unweighted Analysis
#######################
item252.summary <- proportions_two_groups_unweighted(CustomerLevelData = item252.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'Category'
                                             ,rowVariable = 'DHW.Location'
                                             ,aggregateColumnName = "Remove")
item252.summary <- item252.summary[which(item252.summary$Category != "Remove"),]

item252.cast <- dcast(setDT(item252.summary)
                      ,formula = DHW.Location ~ Category
                      ,value.var = c("Percent", "SE", "Count", "n"))

item252.table <- data.frame("DHW.Location" = item252.cast$DHW.Location
                            ,"PSE.Percent"                 = item252.cast$Percent_PSE
                            ,"PSE.SE"                      = item252.cast$SE_PSE
                            ,"PSE.n"                       = item252.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item252.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item252.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item252.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item252.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item252.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item252.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item252.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item252.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = item252.cast$`n_2017 RBSA PS`)

exportTable(item252.table, "MF", "Table 44", weighted = FALSE,OS = T, osIndicator = "PSE")










#############################################################################################
#Item 254: DISTRIBUTION OF COMMON AREA DHW SYSTEMS BY FUEL TYPE (MF Table 46)
#############################################################################################
#subset to columns needed for analysis
item254.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"System.Type"
                                                                    ,"DHW.Fuel"                                  
                                                                    ,"DHW.Location"                                                                    
                                                                    ,"DHW.Serves.Whole.House?"))]
item254.dat0 <- item254.dat[grep("water heat",item254.dat$System.Type, ignore.case = T),]
item254.dat1 <- item254.dat0[grep("bldg",item254.dat0$CK_SiteID, ignore.case = T),]
item254.dat2 <- item254.dat1[which(item254.dat1$DHW.Fuel != "Unknown"),]

item254.dat3 <- left_join(rbsa.dat.bldg, item254.dat2, by = c("CK_Building_ID" = "CK_SiteID"))
colnames(item254.dat3)[which(colnames(item254.dat3) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"

#Subset to Multifamily
item254.dat4 <- item254.dat3[grep("Multifamily", item254.dat3$BuildingType),]
item254.dat5 <- item254.dat4[which(item254.dat4$DHW.Fuel %notin% c("N/A",NA)),]
item254.dat5 <- item254.dat5[which(item254.dat5$Category == "PSE"),]
################################################
# Adding pop and sample sizes for weights
################################################
item254.data <- weightedData(item254.dat5[-which(colnames(item254.dat5) %in% c("CK_Cadmus_ID.y"
                                                                               ,"System.Type"
                                                                               ,"DHW.Fuel"
                                                                               ,"DHW.Location"
                                                                               ,"DHW.Serves.Whole.House?"
                                                                               ,"Category"))])
item254.data <- left_join(item254.data, item254.dat5[which(colnames(item254.dat5) %in% c("CK_Cadmus_ID"
                                                                                         ,"System.Type"
                                                                                         ,"DHW.Fuel"
                                                                                         ,"DHW.Location"
                                                                                         ,"DHW.Serves.Whole.House?"
                                                                                         ,"Category"))])
item254.data$count <- 1
colnames(item254.data)

#######################
# weighted Analysis
#######################
item254.summary <- proportionRowsAndColumns1_within_row(CustomerLevelData = item254.data
                                                        ,valueVariable = 'count'
                                                        ,columnVariable = 'System.Type'
                                                        ,rowVariable = 'DHW.Fuel'
                                                        ,aggregateColumnName = 'All Systems')
item254.summary <- item254.summary[which(item254.summary$DHW.Fuel != "Total"),]

item254.cast <- dcast(setDT(item254.summary)
                      ,formula = System.Type ~ DHW.Fuel
                      ,value.var = c("w.percent", "w.SE", "count","n","N","EB"))
names(item254.cast)
item254.table <- data.frame("System.Type" = item254.cast$System.Type
                            ,"Electric"   = item254.cast$w.percent_Electric
                            ,"Electric.SE" = item254.cast$w.SE_Electric
                            ,"Gas"         = NA#item254.cast$`w.percent_Natural Gas`
                            ,"Gas.SE"      = NA#item254.cast$`w.SE_Natural Gas`
                            ,"Gas.Electric" = NA#
                            ,"Gas.Electric.SE" = NA#
                            ,"Purchased.Steam" = NA#
                            ,"Purchased.Steam.SE" = NA#
                            ,"n" = item254.cast$n
                            ,"Electric.EB" = item254.cast$EB_Electric
                            ,"Gas.EB"      = NA#item254.cast$`EB_Natural Gas`
                            ,"Gas.Electric.EB" = NA#
                            ,"Purchased.Steam.EB" = NA#
                            )

# row ordering example code
levels(item254.table$System.Type)
rowOrder <- c("Storage Water Heater"
              ,"All Systems")
item254.table <- item254.table %>% mutate(System.Type = factor(System.Type, levels = rowOrder)) %>% arrange(System.Type)  
item254.table <- data.frame(item254.table)

exportTable(item254.table, "MF", "Table 46", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# unweighted Analysis
#######################
item254.summary <- proportions_two_groups_unweighted(CustomerLevelData = item254.data
                                                        ,valueVariable = 'count'
                                                        ,columnVariable = 'System.Type'
                                                        ,rowVariable = 'DHW.Fuel'
                                                        ,aggregateColumnName = 'All Systems')
item254.summary <- item254.summary[which(item254.summary$DHW.Fuel != "Total"),]

item254.cast <- dcast(setDT(item254.summary)
                      ,formula = System.Type ~ DHW.Fuel
                      ,value.var = c("Percent", "SE", "Count", "n"))

item254.table <- data.frame("System.Type" = item254.cast$System.Type
                            ,"Electric"   = item254.cast$Percent_Electric
                            ,"Electric.SE" = item254.cast$SE_Electric
                            ,"Gas"         = NA#item254.cast$`Percent_Natural Gas`
                            ,"Gas.SE"      = NA#item254.cast$`SE_Natural Gas`
                            ,"Gas.Electric" = NA#
                            ,"Gas.Electric.SE" = NA#
                            ,"Purchased.Steam" = NA#
                            ,"Purchased.Steam.SE" = NA#
                            ,"n" = item254.cast$n_Electric)

# row ordering example code
levels(item254.table$System.Type)
rowOrder <- c("Storage Water Heater"
              ,"All Systems")
item254.table <- item254.table %>% mutate(System.Type = factor(System.Type, levels = rowOrder)) %>% arrange(System.Type)  
item254.table <- data.frame(item254.table)

exportTable(item254.table, "MF", "Table 46", weighted = FALSE,OS = T, osIndicator = "PSE")