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

#Read in data for analysis
# mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

mechanical.dat2 <- mechanical.dat[,which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                       ,"CK_SiteID"
                                                                       ,"DHW.Type"
                                                                       ,"DHW.Provided.by.Heating.System"
                                                                       ,"DHW.Size.(Gallons)"
                                                                       ,"DHW.Fuel"
                                                                       ,"DHW.Year.Manufactured"
                                                                       ,"System.Type"
                                                                       ,"Category"))]
names(mechanical.dat2)[which(names(mechanical.dat2) == "Category")] <- "Mechanical.Category"

mechanical.dat3 <- left_join(mechanical.dat2, rbsa.dat, by = "CK_Cadmus_ID")

mechanical.dat4 <- mechanical.dat3[which(mechanical.dat3$CK_Cadmus_ID != "CK_CADMUS_ID"),]
mechanical.dat5 <- mechanical.dat4[grep("Multifamily", mechanical.dat4$BuildingType),]

#############################################################################################
#Item 285: DISTRIBUTION OF UNIT WATER HEATERS BY TYPE  (MF Table 77)
#############################################################################################

item285.dat <- mechanical.dat5[,which(colnames(mechanical.dat5) %in% c("CK_Cadmus_ID"
                                                                     ,"CK_SiteID"
                                                                     ,"DHW.Type"
                                                                     ,"DHW.Provided.by.Heating.System"
                                                                     ,"Mechanical.Category"
                                                                     ,"System.Type"))]

item285.dat1 <- item285.dat[which((!is.na(item285.dat$System.Type) | 
                                    !is.na(item285.dat$DHW.Type)) &
                                    item285.dat$Mechanical.Category == "DHW"),]

item285.dat2 <- item285.dat1[grep("site",item285.dat1$CK_SiteID,ignore.case = T),]

item285.dat3 <- unique(item285.dat2)

item285.merge <- left_join(rbsa.dat, item285.dat3)
item285.merge <- item285.merge[grep("site",item285.merge$CK_Building_ID,ignore.case = T),]
item285.merge <- item285.merge[which(!is.na(item285.merge$Mechanical.Category)),]

######################################
#Pop and Sample Sizes for weights
######################################
item285.data <- weightedData(item285.merge[which(colnames(item285.merge) %notin% c("CK_SiteID"
                                                                                 ,"System.Type"
                                                                                 ,"Mechanical.Category"
                                                                                 ,"DHW.Type"
                                                                                 ,"DHW.Provided.by.Heating.System"
                                                                                 ,"Category"))])

item285.data <- left_join(item285.data, item285.merge[which(colnames(item285.merge) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"System.Type"
                                                                                         ,"Mechanical.Category"
                                                                                         ,"DHW.Type"
                                                                                         ,"DHW.Provided.by.Heating.System"
                                                                                         ,"Category"))])
item285.data$count <- 1


######################
# weighted analysis
######################
item285.final <- proportionRowsAndColumns1(CustomerLevelData = item285.data
                                          ,valueVariable = 'count'
                                          ,columnVariable = "Category"
                                          ,rowVariable = 'System.Type'
                                          ,aggregateColumnName = 'Remove')
item285.final <- item285.final[which(item285.final$Category != "Remove"),which(names(item285.final) != "BuildingType")]

item285.cast <- dcast(setDT(item285.final)
                      ,formula = System.Type ~ Category
                      ,value.var = c("w.percent", "w.SE", "n","EB"))

item285.table <- data.frame("System.Type"                    = item285.cast$System.Type
                            ,"PSE.Percent"                 = item285.cast$w.percent_PSE
                            ,"PSE.SE"                      = item285.cast$w.SE_PSE
                            ,"PSE.n"                       = item285.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item285.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item285.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item285.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item285.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item285.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item285.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item285.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item285.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = item285.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = item285.cast$EB_PSE
                            ,"PSE.King.County_EB"          = item285.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = item285.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = item285.cast$`EB_2017 RBSA PS`
)

exportTable(item285.table, "MF", "Table 77", weighted = TRUE,OS = T, osIndicator = "PSE")

######################
# unweighted analysis
######################
item285.final <- proportions_two_groups_unweighted(CustomerLevelData = item285.data
                                           ,valueVariable = 'count'
                                           ,columnVariable = "Category"
                                           ,rowVariable = 'System.Type'
                                           ,aggregateColumnName = 'Remove')
item285.final <- item285.final[which(item285.final$Category != "Remove"),which(names(item285.final) != "BuildingType")]

item285.cast <- dcast(setDT(item285.final)
                      ,formula = System.Type ~ Category
                      ,value.var = c("Percent", "SE", "n"))

item285.table <- data.frame("System.Type"                    = item285.cast$System.Type
                            ,"PSE.Percent"                 = item285.cast$Percent_PSE
                            ,"PSE.SE"                      = item285.cast$SE_PSE
                            ,"PSE.n"                       = item285.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item285.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item285.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item285.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item285.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item285.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item285.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item285.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item285.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = item285.cast$`n_2017 RBSA PS`
)
exportTable(item285.table, "MF", "Table 77", weighted = FALSE,OS = T, osIndicator = "PSE")






#############################################################################################
#Item 286: DISTRIBUTION OF IN-UNIT WATER HEATER TANKS BY SIZE AND FUEL TYPE (MF Table 78)
#############################################################################################

item286.dat <- mechanical.dat5[,which(colnames(mechanical.dat5) %in% c("CK_Cadmus_ID"
                                                                       ,"CK_SiteID"
                                                                       ,"DHW.Size.(Gallons)"
                                                                       ,"DHW.Fuel"
                                                                       ,"Mechanical.Category"))]

item286.dat1 <- item286.dat[grep("site",item286.dat$CK_SiteID,ignore.case = T),]
item286.dat2 <- item286.dat1[which(item286.dat1$Mechanical.Category == "DHW"),]

item286.dat3 <- item286.dat2[which(!is.na(item286.dat2$`DHW.Size.(Gallons)`) &
                                     !is.na(item286.dat2$DHW.Fuel) &
                                     item286.dat2$DHW.Fuel != "Unknown" &
                                     item286.dat2$`DHW.Size.(Gallons)` != "Unknown"),]

item286.dat3$Count <- 1
item286.dat3$TankSize <- NA
item286.dat3$TankSize[which(item286.dat3$`DHW.Size.(Gallons)` >= 0 &
                              item286.dat3$`DHW.Size.(Gallons)` <= 55)] <-  "0-55"
item286.dat3$TankSize[which(item286.dat3$`DHW.Size.(Gallons)` > 55)] <- ">55"

unique(item286.dat3$TankSize)

item286.merge <- left_join(rbsa.dat, item286.dat3)
item286.merge <- item286.merge[which(!is.na(item286.merge$TankSize)),]
item286.merge <- item286.merge[which(item286.merge$DHW.Fuel %notin% c("N/A", NA)),]
item286.merge <- item286.merge[which(item286.merge$Category == "PSE"),]
######################################
#Pop and Sample Sizes for weights
######################################
item286.data <- weightedData(item286.merge[which(colnames(item286.merge) %notin% c("CK_SiteID"
                                                                                 ,"Mechanical.Category"
                                                                                 ,"DHW.Size.(Gallons)"
                                                                                 ,"DHW.Fuel"
                                                                                 ,"Count"
                                                                                 ,"TankSize"
                                                                                 ,"Category"))])

item286.data <- left_join(item286.data, item286.merge[which(colnames(item286.merge) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Mechanical.Category"
                                                                                         ,"DHW.Size.(Gallons)"
                                                                                         ,"DHW.Fuel"
                                                                                         ,"Count"
                                                                                         ,"TankSize"
                                                                                         ,"Category"))])
item286.data$count <- 1



#########################
# weighted analysis
#########################
item286.summary <- proportionRowsAndColumns1(CustomerLevelData = item286.data
                                                     ,valueVariable = 'count'
                                                     ,columnVariable = 'TankSize'
                                                     ,rowVariable = 'DHW.Fuel'
                                                     ,aggregateColumnName = "All Sizes")
item286.summary$DHW.Fuel[which(item286.summary$DHW.Fuel == "Total")] <- "All Types"


item286.cast <- dcast(setDT(item286.summary)
                      ,formula = DHW.Fuel ~ TankSize
                      ,value.var = c("w.percent", "w.SE","count","n", "N","EB"))
names(item286.cast)
item286.table <- data.frame("Water.Heater.Fuel" = item286.cast$DHW.Fuel
                            ,"0.55.Gal"         = item286.cast$`w.percent_0-55`
                            ,"0.55.Gal.SE"      = item286.cast$`w.SE_0-55`
                            ,"GT55.Gal"         = NA#item286.cast$`w.percent_>55`
                            ,"GT55.Gal.SE"      = NA#item286.cast$`w.SE_>55`
                            ,"All.Sizes"        = item286.cast$`w.percent_All Sizes`
                            ,"All.Sizes.SE"     = item286.cast$`w.SE_All Sizes`
                            ,"n"                = item286.cast$`n_All Sizes`
                            ,"0.55.Gal.EB"      = item286.cast$`EB_0-55`
                            ,"GT55.Gal.EB"      = NA#item286.cast$`EB_>55`
                            ,"All.Sizes.EB"     = item286.cast$`EB_All Sizes`)

levels(item286.table$Water.Heater.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"All Types")
item286.table <- item286.table %>% mutate(Water.Heater.Fuel = factor(Water.Heater.Fuel, levels = rowOrder)) %>% arrange(Water.Heater.Fuel)  
item286.table <- data.frame(item286.table)


exportTable(item286.table, "MF", "Table 78A", weighted = TRUE,OS = T, osIndicator = "PSE")

#########################
# unweighted analysis
#########################
item286.summary <- proportions_two_groups_unweighted(CustomerLevelData = item286.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'TankSize'
                                             ,rowVariable = 'DHW.Fuel'
                                             ,aggregateColumnName = "All Sizes")
item286.summary$DHW.Fuel[which(item286.summary$DHW.Fuel == "Total")] <- "All Types"

item286.cast <- dcast(setDT(item286.summary)
                      ,formula = DHW.Fuel ~ TankSize
                      ,value.var = c("Percent", "SE","Count","n"))

item286.table <- data.frame("Water.Heater.Fuel" = item286.cast$DHW.Fuel
                            ,"0.55.Gal"         = item286.cast$`Percent_0-55`
                            ,"0.55.Gal.SE"      = item286.cast$`SE_0-55`
                            ,"GT55.Gal"         = NA#item286.cast$`Percent_>55`
                            ,"GT55.Gal.SE"      = NA#item286.cast$`SE_>55`
                            ,"All.Sizes"        = item286.cast$`Percent_All Sizes`
                            ,"All.Sizes.SE"     = item286.cast$`SE_All Sizes`
                            ,"n"                = item286.cast$`n_All Sizes`)

levels(item286.table$Water.Heater.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"All Types")
item286.table <- item286.table %>% mutate(Water.Heater.Fuel = factor(Water.Heater.Fuel, levels = rowOrder)) %>% arrange(Water.Heater.Fuel)  
item286.table <- data.frame(item286.table)

exportTable(item286.table, "MF", "Table 78A", weighted = FALSE,OS = T, osIndicator = "PSE")



#############################################################################################
#Item 286B: DISTRIBUTION OF IN-UNIT WATER HEATER TANKS BY SIZE AND FUEL TYPE (MF Table 78)
#############################################################################################

item286B.dat <- mechanical.dat5[,which(colnames(mechanical.dat5) %in% c("CK_Cadmus_ID"
                                                                       ,"CK_SiteID"
                                                                       ,"DHW.Size.(Gallons)"
                                                                       ,"DHW.Fuel"
                                                                       ,"Mechanical.Category"))]

item286B.dat1 <- item286B.dat[grep("site",item286B.dat$CK_SiteID,ignore.case = T),]
item286B.dat2 <- item286B.dat1[which(item286B.dat1$Mechanical.Category == "DHW"),]

item286B.dat3 <- item286B.dat2[which(!is.na(item286B.dat2$`DHW.Size.(Gallons)`) &
                                     !is.na(item286B.dat2$DHW.Fuel) &
                                     item286B.dat2$DHW.Fuel != "Unknown" &
                                     item286B.dat2$`DHW.Size.(Gallons)` != "Unknown"),]

item286B.dat3$Count <- 1
item286B.dat3$TankSize <- NA
item286B.dat3$TankSize[which(item286B.dat3$`DHW.Size.(Gallons)` >= 0 &
                              item286B.dat3$`DHW.Size.(Gallons)` <= 55)] <-  "0-55"
item286B.dat3$TankSize[which(item286B.dat3$`DHW.Size.(Gallons)` > 55)] <- ">55"

unique(item286B.dat3$TankSize)

item286B.merge <- left_join(rbsa.dat, item286B.dat3)
item286B.merge <- item286B.merge[which(!is.na(item286B.merge$TankSize)),]
item286B.merge <- item286B.merge[which(item286B.merge$DHW.Fuel %notin% c("N/A", NA)),]
######################################
#Pop and Sample Sizes for weights
######################################
item286B.data <- weightedData(item286B.merge[which(colnames(item286B.merge) %notin% c("CK_SiteID"
                                                                                   ,"Mechanical.Category"
                                                                                   ,"DHW.Size.(Gallons)"
                                                                                   ,"DHW.Fuel"
                                                                                   ,"Count"
                                                                                   ,"TankSize"
                                                                                   ,"Category"))])

item286B.data <- left_join(item286B.data, item286B.merge[which(colnames(item286B.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"CK_SiteID"
                                                                                           ,"Mechanical.Category"
                                                                                           ,"DHW.Size.(Gallons)"
                                                                                           ,"DHW.Fuel"
                                                                                           ,"Count"
                                                                                           ,"TankSize"
                                                                                           ,"Category"))])
item286B.data$count <- 1



#########################
# weighted analysis
#########################
item286B.summary <- proportionRowsAndColumns1(CustomerLevelData = item286B.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'Category'
                                             ,rowVariable = 'DHW.Fuel'
                                             ,aggregateColumnName = "Remove")
item286B.summary$DHW.Fuel[which(item286B.summary$DHW.Fuel == "Total")] <- "All Types"


item286B.cast <- dcast(setDT(item286B.summary)
                      ,formula = DHW.Fuel ~ Category
                      ,value.var = c("w.percent", "w.SE","count","n", "N","EB"))
names(item286B.cast)
item286B.table <- data.frame("Water.Heater.Fuel" = item286B.cast$DHW.Fuel
                             ,"PSE.Percent"                 = item286B.cast$w.percent_PSE
                             ,"PSE.SE"                      = item286B.cast$w.SE_PSE
                             ,"PSE.n"                       = item286B.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item286B.cast$`w.percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item286B.cast$`w.SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item286B.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item286B.cast$`w.percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item286B.cast$`w.SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item286B.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item286B.cast$`w.percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item286B.cast$`w.SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"              = item286B.cast$`n_2017 RBSA PS`
                             ,"PSE_EB"                      = item286B.cast$EB_PSE
                             ,"PSE.King.County_EB"          = item286B.cast$`EB_PSE KING COUNTY`
                             ,"PSE.Non.King.County_EB"      = item286B.cast$`EB_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS_EB"             = item286B.cast$`EB_2017 RBSA PS`)

levels(item286B.table$Water.Heater.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"All Types")
item286B.table <- item286B.table %>% mutate(Water.Heater.Fuel = factor(Water.Heater.Fuel, levels = rowOrder)) %>% arrange(Water.Heater.Fuel)  
item286B.table <- data.frame(item286B.table)


exportTable(item286B.table, "MF", "Table 78B", weighted = TRUE,OS = T, osIndicator = "PSE")

#########################
# unweighted analysis
#########################
item286B.summary <- proportions_two_groups_unweighted(CustomerLevelData = item286B.data
                                                      ,valueVariable = 'count'
                                                      ,columnVariable = 'Category'
                                                      ,rowVariable = 'DHW.Fuel'
                                                      ,aggregateColumnName = "Remove")
item286B.summary$DHW.Fuel[which(item286B.summary$DHW.Fuel == "Total")] <- "All Types"


item286B.cast <- dcast(setDT(item286B.summary)
                       ,formula = DHW.Fuel ~ Category
                      ,value.var = c("Percent", "SE","Count","n"))

item286B.table <- data.frame("Water.Heater.Fuel" = item286B.cast$DHW.Fuel
                             ,"PSE.Percent"                 = item286B.cast$Percent_PSE
                             ,"PSE.SE"                      = item286B.cast$SE_PSE
                             ,"PSE.n"                       = item286B.cast$n_PSE
                             ,"PSE.King.County.Percent"     = item286B.cast$`Percent_PSE KING COUNTY`
                             ,"PSE.King.County.SE"          = item286B.cast$`SE_PSE KING COUNTY`
                             ,"PSE.King.County.n"           = item286B.cast$`n_PSE KING COUNTY`
                             ,"PSE.Non.King.County.Percent" = item286B.cast$`Percent_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.SE"      = item286B.cast$`SE_PSE NON-KING COUNTY`
                             ,"PSE.Non.King.County.n"       = item286B.cast$`n_PSE NON-KING COUNTY`
                             ,"2017.RBSA.PS.Percent"        = item286B.cast$`Percent_2017 RBSA PS`
                             ,"2017.RBSA.PS.SE"             = item286B.cast$`SE_2017 RBSA PS`
                             ,"2017.RBSA.PS.n"              = item286B.cast$`n_2017 RBSA PS`)

levels(item286B.table$Water.Heater.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"All Types")
item286B.table <- item286B.table %>% mutate(Water.Heater.Fuel = factor(Water.Heater.Fuel, levels = rowOrder)) %>% arrange(Water.Heater.Fuel)  
item286B.table <- data.frame(item286B.table)

exportTable(item286B.table, "MF", "Table 78B", weighted = FALSE,OS = T, osIndicator = "PSE")









#############################################################################################
#Item 287: DISTRIBUTION OF IN-UNIT WATER HEATERS BY VINTAGE  (MF Table 78)
#############################################################################################

item287.dat <- mechanical.dat5[,which(colnames(mechanical.dat5) %in% c("CK_Cadmus_ID"
                                                                       ,"CK_SiteID"
                                                                       ,"DHW.Type"
                                                                       ,"DHW.Fuel"
                                                                       ,"Mechanical.Category"
                                                                       ,"DHW.Year.Manufactured"))]
item287.dat1 <- item287.dat[grep("site",item287.dat$CK_SiteID,ignore.case = T),]

item287.dat3 <- item287.dat[which(!is.na(item287.dat$DHW.Type)),]
item287.dat3$Year <- as.numeric(as.character(item287.dat3$DHW.Year.Manufactured))

item287.dat4 <- item287.dat3[which(item287.dat3$Year %notin% c("N/A",NA)),]

item287.dat4$Vintage <- NA
item287.dat4$Vintage[which(item287.dat4$Year < 1990)] <- "Pre_1990"
item287.dat4$Vintage[which(item287.dat4$Year > 1990 & item287.dat4$Year < 1999)] <- "1990_1999"
item287.dat4$Vintage[which(item287.dat4$Year >= 2000 & item287.dat4$Year <= 2004)] <- "2000_2004"
item287.dat4$Vintage[which(item287.dat4$Year >= 2005 & item287.dat4$Year <= 2009)] <- "2005_2009"
item287.dat4$Vintage[which(item287.dat4$Year > 2009)] <- "Post_2009"
unique(item287.dat4$Vintage)
item287.dat4$Count <- 1

item287.merge <- left_join(rbsa.dat, item287.dat4)
item287.merge <- item287.merge[grep("site",item287.merge$CK_Building_ID,ignore.case = T),]
item287.merge <- item287.merge[grep("multifamily",item287.merge$BuildingType,ignore.case = T),]
item287.merge <- item287.merge[which(!duplicated(item287.merge$CK_Cadmus_ID)),]
item287.merge <- item287.merge[which(!is.na(item287.merge$Vintage)),]
item287.merge <- item287.merge[which(item287.merge$Category == "PSE"),]
######################################
#Pop and Sample Sizes for weights
######################################
item287.data <- weightedData(item287.merge[which(colnames(item287.merge) %notin% c("CK_SiteID"
                                                                                   ,"Mechanical.Category"
                                                                                   ,"DHW.Size.(Gallons)"
                                                                                   ,"DHW.Fuel"
                                                                                   ,"DHW.Type"
                                                                                   ,"DHW.Year.Manufactured"
                                                                                   ,"Count"
                                                                                   ,"Year"
                                                                                   ,'Vintage'
                                                                                   ,"TankSize"
                                                                                   ,"Category"))])

item287.data <- left_join(item287.data, item287.merge[which(colnames(item287.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"CK_SiteID"
                                                                                           ,"Mechanical.Category"
                                                                                           ,"DHW.Size.(Gallons)"
                                                                                           ,"DHW.Fuel"
                                                                                           ,"DHW.Type"
                                                                                           ,"DHW.Year.Manufactured"
                                                                                           ,"Count"
                                                                                           ,"Year"
                                                                                           ,'Vintage'
                                                                                           ,"TankSize"
                                                                                           ,"Category"))])
item287.data$count <- 1



#########################
# weighted analysis
#########################
item287.final <- proportions_one_group(CustomerLevelData = item287.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'Vintage'
                                          ,total.name = "All Vintages")
item287.final$Vintage[which(item287.final$Vintage == "Total")] <- "All Vintages"
item287.final$Vintage <- as.factor(item287.final$Vintage)

levels(item287.final$Vintage)
rowOrder <- c("Pre_1990"
              ,"1990_1999"
              ,"2000_2004"
              ,"2005_2009"
              ,"Post_2009"
              ,"All Vintages")
item287.final <- item287.final %>% mutate(Vintage = factor(Vintage, levels = rowOrder)) %>% arrange(Vintage)  
item287.final <- data.frame(item287.final)
item287.final <- item287.final[which(names(item287.final) != "BuildingType")]

exportTable(item287.final, "MF", "Table 79", weighted = TRUE,OS = T, osIndicator = "PSE")

#########################
# weighted analysis
#########################
item287.final <- proportions_one_group(CustomerLevelData = item287.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'Vintage'
                                          ,total.name = "Total"
                                          ,weighted = FALSE)
item287.final$Vintage[which(item287.final$Vintage == "Total")] <- "All Vintages"
item287.final$Vintage <- as.factor(item287.final$Vintage)

levels(item287.final$Vintage)
rowOrder <- c("Pre_1990"
              ,"1990_1999"
              ,"2000_2004"
              ,"2005_2009"
              ,"Post_2009"
              ,"All Vintages")
item287.final <- item287.final %>% mutate(Vintage = factor(Vintage, levels = rowOrder)) %>% arrange(Vintage)  
item287.final <- data.frame(item287.final)
item287.final <- item287.final[which(names(item287.final) != "BuildingType")]


exportTable(item287.final, "MF", "Table 79", weighted = FALSE,OS = T, osIndicator = "PSE")
