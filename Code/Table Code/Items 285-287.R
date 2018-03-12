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

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
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
                                                                     ,"Category"
                                                                     ,"System.Type"))]

item285.dat1 <- item285.dat[which((!is.na(item285.dat$System.Type) | 
                                    !is.na(item285.dat$DHW.Type)) &
                                    item285.dat$Category == "DHW"),]

item285.dat2 <- item285.dat1[grep("site",item285.dat1$CK_SiteID,ignore.case = T),]

item285.dat3 <- unique(item285.dat2)

item285.merge <- left_join(rbsa.dat, item285.dat3)
item285.merge <- item285.merge[grep("site",item285.merge$CK_Building_ID,ignore.case = T),]
item285.merge <- item285.merge[which(!is.na(item285.merge$Category)),]

######################################
#Pop and Sample Sizes for weights
######################################
item285.data <- weightedData(item285.merge[which(colnames(item285.merge) %notin% c("CK_SiteID"
                                                                                 ,"System.Type"
                                                                                 ,"Category"
                                                                                 ,"DHW.Type"
                                                                                 ,"DHW.Provided.by.Heating.System"))])

item285.data <- left_join(item285.data, item285.merge[which(colnames(item285.merge) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"System.Type"
                                                                                         ,"Category"
                                                                                         ,"DHW.Type"
                                                                                         ,"DHW.Provided.by.Heating.System"))])
item285.data$count <- 1


######################
# weighted analysis
######################
item285.final <- proportions_one_group(CustomerLevelData = item285.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'System.Type'
                                          ,total.name = 'Remove')
item285.final <- item285.final[which(item285.final$System.Type != "Total"),which(names(item285.final) != "BuildingType")]

exportTable(item285.final, "MF", "Table 77", weighted = TRUE)

######################
# unweighted analysis
######################
item285.final <- proportions_one_group(CustomerLevelData = item285.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'System.Type'
                                          ,total.name = 'Remove'
                                          ,weighted = FALSE)
item285.final <- item285.final[which(item285.final$System.Type != "Total"),which(names(item285.final) != "BuildingType")]

exportTable(item285.final, "MF", "Table 77", weighted = FALSE)






#############################################################################################
#Item 286: DISTRIBUTION OF IN-UNIT WATER HEATER TANKS BY SIZE AND FUEL TYPE (MF Table 78)
#############################################################################################

item286.dat <- mechanical.dat5[,which(colnames(mechanical.dat5) %in% c("CK_Cadmus_ID"
                                                                       ,"CK_SiteID"
                                                                       ,"DHW.Size.(Gallons)"
                                                                       ,"DHW.Fuel"
                                                                       ,"Category"))]

item286.dat1 <- item286.dat[grep("site",item286.dat$CK_SiteID,ignore.case = T),]
item286.dat2 <- item286.dat1[which(item286.dat1$Category == "DHW"),]

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
######################################
#Pop and Sample Sizes for weights
######################################
item286.data <- weightedData(item286.merge[which(colnames(item286.merge) %notin% c("CK_SiteID"
                                                                                 ,"Category"
                                                                                 ,"DHW.Size.(Gallons)"
                                                                                 ,"DHW.Fuel"
                                                                                 ,"Count"
                                                                                 ,"TankSize"))])

item286.data <- left_join(item286.data, item286.merge[which(colnames(item286.merge) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Category"
                                                                                         ,"DHW.Size.(Gallons)"
                                                                                         ,"DHW.Fuel"
                                                                                         ,"Count"
                                                                                         ,"TankSize"))])
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

item286.table <- data.frame("Water.Heater.Fuel" = item286.cast$DHW.Fuel
                            ,"0.55.Gal"         = item286.cast$`w.percent_0-55`
                            ,"0.55.Gal.SE"      = item286.cast$`w.SE_0-55`
                            ,"GT55.Gal"         = item286.cast$`w.percent_>55`
                            ,"GT55.Gal.SE"      = item286.cast$`w.SE_>55`
                            ,"All.Sizes"        = item286.cast$`w.percent_All Sizes`
                            ,"All.Sizes.SE"     = item286.cast$`w.SE_All Sizes`
                            ,"n"                = item286.cast$`n_All Sizes`
                            ,"0.55.Gal.EB"      = item286.cast$`EB_0-55`
                            ,"GT55.Gal.EB"      = item286.cast$`EB_>55`
                            ,"All.Sizes.EB"     = item286.cast$`EB_All Sizes`)

levels(item286.table$Water.Heater.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"All Types")
item286.table <- item286.table %>% mutate(Water.Heater.Fuel = factor(Water.Heater.Fuel, levels = rowOrder)) %>% arrange(Water.Heater.Fuel)  
item286.table <- data.frame(item286.table)


exportTable(item286.table, "MF", "Table 78", weighted = TRUE)

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
                            ,"GT55.Gal"         = item286.cast$`Percent_>55`
                            ,"GT55.Gal.SE"      = item286.cast$`SE_>55`
                            ,"All.Sizes"        = item286.cast$`Percent_All Sizes`
                            ,"All.Sizes.SE"     = item286.cast$`SE_All Sizes`
                            ,"n"                = item286.cast$`n_All Sizes`)

levels(item286.table$Water.Heater.Fuel)
rowOrder <- c("Electric"
              ,"Natural Gas"
              ,"All Types")
item286.table <- item286.table %>% mutate(Water.Heater.Fuel = factor(Water.Heater.Fuel, levels = rowOrder)) %>% arrange(Water.Heater.Fuel)  
item286.table <- data.frame(item286.table)

exportTable(item286.table, "MF", "Table 78", weighted = FALSE)







#############################################################################################
#Item 287: DISTRIBUTION OF IN-UNIT WATER HEATERS BY VINTAGE  (MF Table 78)
#############################################################################################

item287.dat <- mechanical.dat5[,which(colnames(mechanical.dat5) %in% c("CK_Cadmus_ID"
                                                                       ,"CK_SiteID"
                                                                       ,"DHW.Type"
                                                                       ,"DHW.Fuel"
                                                                       ,"Category"
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

######################################
#Pop and Sample Sizes for weights
######################################
item287.data <- weightedData(item287.merge[which(colnames(item287.merge) %notin% c("CK_SiteID"
                                                                                   ,"Category"
                                                                                   ,"DHW.Size.(Gallons)"
                                                                                   ,"DHW.Fuel"
                                                                                   ,"DHW.Type"
                                                                                   ,"DHW.Year.Manufactured"
                                                                                   ,"Count"
                                                                                   ,"Year"
                                                                                   ,'Vintage'
                                                                                   ,"TankSize"))])

item287.data <- left_join(item287.data, item287.merge[which(colnames(item287.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"CK_SiteID"
                                                                                           ,"Category"
                                                                                           ,"DHW.Size.(Gallons)"
                                                                                           ,"DHW.Fuel"
                                                                                           ,"DHW.Type"
                                                                                           ,"DHW.Year.Manufactured"
                                                                                           ,"Count"
                                                                                           ,"Year"
                                                                                           ,'Vintage'
                                                                                           ,"TankSize"))])
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

exportTable(item287.final, "MF", "Table 79", weighted = TRUE)

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


exportTable(item287.final, "MF", "Table 79", weighted = FALSE)
