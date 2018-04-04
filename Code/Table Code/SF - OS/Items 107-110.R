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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) 

#Read in data for analysis
# appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))



# #############################################################################################
# #Item 107: AVERAGE NUMBER OF TELEVISIONS PER HOME BY STATE (SF table 114)
# #############################################################################################
# #subset to columns needed for analysis
# item107.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
#                                                                     ,"Type"
#                                                                     ,""))]
# item107.dat$count <- 1
# 
# item107.dat0 <- item107.dat[which(item107.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# 
# item107.dat1 <- item107.dat0[which(item107.dat0$Type == "Television"),]
# 
# item107.customer <- summarise(group_by(item107.dat1, CK_Cadmus_ID)
#                               ,Site.Count = sum(count))
# 
# item107.customer <- left_join(rbsa.dat, item107.customer)
# item107.customer$Site.Count[which(is.na(item107.customer$Site.Count))] <- 0
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item107.data <- weightedData(item107.customer[-which(colnames(item107.customer) %in% c("Site.Count"))])
# item107.data <- left_join(item107.data, item107.customer[which(colnames(item107.customer) %in% c("CK_Cadmus_ID"
#                                                                                                  ,"Site.Count"))])
# item107.data$count <- 1
# #######################
# # Weighted Analysis
# #######################
# item107.final <- mean_one_group(item107.data
#                                 ,valueVariable = 'Site.Count'
#                                 ,byVariable = 'State'
#                                 ,aggregateRow = 'Region')
# 
# item107.final.SF <- item107.final[which(item107.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item107.final) %in% c("BuildingType"
#                                                                          ,"Count"))]
# item107.final.MH <- item107.final[which(item107.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item107.final) %in% c("BuildingType"
#                                                                          ,"Count"))]
# 
# exportTable(item107.final.SF, "SF", "Table 114", weighted = TRUE)
# # exportTable(item107.final.MH, "MH", "Table 89", weighted = TRUE)
# 
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# item107.final <- mean_one_group_unweighted(item107.data
#                                 ,valueVariable = 'Site.Count'
#                                 ,byVariable = 'State'
#                                 ,aggregateRow = 'Region')
# 
# item107.final.SF <- item107.final[which(item107.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item107.final) %in% c("BuildingType"
#                                                                          ,"Count"))]
# item107.final.MH <- item107.final[which(item107.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item107.final) %in% c("BuildingType"
#                                                                          ,"Count"))]
# 
# exportTable(item107.final.SF, "SF", "Table 114", weighted = FALSE)
# # exportTable(item107.final.MH, "MH", "Table 89", weighted = FALSE)
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 108: AVERAGE TELEVISION POWER BY VINTAGE (SF table 115, MH table 90)
# #############################################################################################
# #subset to columns needed for analysis
# item108.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
#                                                                     ,"Type"
#                                                                     ,"TV.Wattage"
#                                                                     ,"Age"))]
# item108.dat$count <- 1
# 
# item108.dat0 <- item108.dat[which(item108.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item108.dat1 <- left_join(item108.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# 
# item108.dat2 <- item108.dat1[which(item108.dat1$Type == "Television"),]
# 
# #clean wattage
# unique(item108.dat2$TV.Wattage)
# item108.dat2$TV.Wattage <- as.numeric(as.character(item108.dat2$TV.Wattage))
# item108.dat3            <- item108.dat2[which(!(is.na(item108.dat2$TV.Wattage))),]
# 
# #clean age
# unique(item108.dat3$Age)
# item108.dat3$Age <- as.numeric(as.character(item108.dat3$Age))
# item108.dat4     <- item108.dat3#[which(!(is.na(item108.dat3$Age))),]
# 
# # Bin equipment vintages for items 50 and 52 (4 categories)
# item108.dat4$Age <- as.numeric(as.character(item108.dat4$Age))
# item108.dat4$EquipVintage_bins <- "Unknown Vintage"
# item108.dat4$EquipVintage_bins[which(item108.dat4$Age <  1990)] <- "Pre 1990"
# item108.dat4$EquipVintage_bins[which(item108.dat4$Age >= 1990 & item108.dat4$Age < 1995)] <- "1990-1994"
# item108.dat4$EquipVintage_bins[which(item108.dat4$Age >= 1995 & item108.dat4$Age < 2000)] <- "1995-1999"
# item108.dat4$EquipVintage_bins[which(item108.dat4$Age >= 2000 & item108.dat4$Age < 2005)] <- "2000-2004"
# item108.dat4$EquipVintage_bins[which(item108.dat4$Age >= 2005 & item108.dat4$Age < 2010)] <- "2005-2009"
# item108.dat4$EquipVintage_bins[which(item108.dat4$Age >= 2010 & item108.dat4$Age < 2015)] <- "2010-2014"
# item108.dat4$EquipVintage_bins[which(item108.dat4$Age >= 2015)] <- "Post 2014"
# #check uniques
# unique(item108.dat4$EquipVintage_bins)
# 
# 
# item108.customer <- left_join(item108.dat4, rbsa.dat)
# 
# 
# ###########################################
# # add pop and sample sizes for weighting
# ###########################################
# item108.data <- weightedData(item108.customer[-which(colnames(item108.customer) %in% c("Type"
#                                                                                        ,"Age"
#                                                                                        ,"TV.Wattage"
#                                                                                        ,"count"
#                                                                                        ,"EquipVintage_bins"))])
# item108.data <- left_join(item108.data, item108.customer[which(colnames(item108.customer) %in% c("CK_Cadmus_ID"
#                                                                                                  ,"Type"
#                                                                                                  ,"Age"
#                                                                                                  ,"TV.Wattage"
#                                                                                                  ,"count"
#                                                                                                  ,"EquipVintage_bins"))])
# 
# 
# #####################
# # Weighted analysis
# #####################
# item108.final    <- mean_one_group(item108.data
#                                    ,valueVariable = 'TV.Wattage'
#                                    ,byVariable    = 'EquipVintage_bins'
#                                    ,aggregateRow  = "All Vintages")
# 
# unique(item108.final$EquipVintage_bins)
# rowOrder <- c("Pre 1990"
#               ,"1990-1994"
#               ,"1995-1999"
#               ,"2000-2004"
#               ,"2005-2009"
#               ,"2010-2014"
#               ,"Post 2014"
#               ,"Unknown Vintage"
#               ,"All Vintages")
# item108.table <- item108.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
# item108.table <- data.frame(item108.table)
# 
# 
# item108.final.SF <- item108.table[which(item108.table$BuildingType == "Single Family")
#                                   ,-which(colnames(item108.table) %in% c("BuildingType"
#                                                                          ,"Count"))]
# item108.final.MH <- item108.table[which(item108.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(item108.table) %in% c("BuildingType"
#                                                                          ,"Count"))]
# 
# 
# exportTable(item108.final.SF, "SF", "Table 115", weighted = TRUE)
# # exportTable(item108.final.MH, "MH", "Table 90", weighted = TRUE)
# 
# 
# ##############
# # Unweighted
# ##############
# item108.final    <- mean_one_group_unweighted(item108.data
#                                    ,valueVariable = 'TV.Wattage'
#                                    ,byVariable    = 'EquipVintage_bins'
#                                    ,aggregateRow  = "All Vintages")
# 
# unique(item108.final$EquipVintage_bins)
# rowOrder <- c("Pre 1990"
#               ,"1990-1994"
#               ,"1995-1999"
#               ,"2000-2004"
#               ,"2005-2009"
#               ,"2010-2014"
#               ,"Post 2014"
#               ,"Unknown Vintage"
#               ,"All Vintages")
# item108.table <- item108.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
# item108.table <- data.frame(item108.table)
# 
# 
# item108.final.SF <- item108.table[which(item108.table$BuildingType == "Single Family")
#                                   ,-which(colnames(item108.table) %in% c("BuildingType"
#                                                                          ,"Count"))]
# item108.final.MH <- item108.table[which(item108.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(item108.table) %in% c("BuildingType"
#                                                                          ,"Count"))]
# 
# 
# exportTable(item108.final.SF, "SF", "Table 115", weighted = FALSE)
# # exportTable(item108.final.MH, "MH", "Table 90", weighted = FALSE)
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 109: DISTRIBUTION OF TELEVISION SCREENS BY TYPE AND VINTAGE (SF table 116, MH table 91)
# #############################################################################################
# #subset to columns needed for analysis
# item109.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
#                                                                     ,"Type"
#                                                                     ,"TV.Screen.Type"
#                                                                     ,"Age"))]
# item109.dat$count <- 1
# 
# item109.dat0 <- item109.dat[which(item109.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item109.dat1 <- left_join(item109.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# 
# item109.dat2 <- item109.dat1[which(item109.dat1$Type == "Television"),]
# 
# #clean screen type
# unique(item109.dat2$TV.Screen.Type)
# item109.dat3 <- item109.dat2[which(!(is.na(item109.dat2$TV.Screen.Type))),]
# item109.dat4 <- item109.dat3[which(!(item109.dat3$TV.Screen.Type %in% c("Unknown"))),]
# 
# item109.dat4$TV.Screen.Type[grep("Tube",item109.dat4$TV.Screen.Type)] <- "CRT"
# item109.dat4$TV.Screen.Type[which(item109.dat4$TV.Screen.Type %in% c("Other", "Projector (Non-CRT)", "DLP"))] <- "Other"
# unique(item109.dat4$TV.Screen.Type)
# 
# 
# #clean age
# unique(item109.dat4$Age)
# item109.dat4$Age <- as.numeric(as.character(item109.dat4$Age))
# item109.dat5     <- item109.dat4[which(!(is.na(item109.dat4$Age))),]
# 
# # Bin equipment vintages for items 50 and 52 (4 categories)
# item109.dat5$EquipVintage_bins <- as.numeric(as.character(item109.dat5$Age))
# 
# item109.dat5$EquipVintage_bins[which(item109.dat5$Age < 1990)] <- "Pre 1990"
# item109.dat5$EquipVintage_bins[which(item109.dat5$Age >= 1990 & item109.dat5$Age < 1995)] <- "1990-1994"
# item109.dat5$EquipVintage_bins[which(item109.dat5$Age >= 1995 & item109.dat5$Age < 2000)] <- "1995-1999"
# item109.dat5$EquipVintage_bins[which(item109.dat5$Age >= 2000 & item109.dat5$Age < 2005)] <- "2000-2004"
# item109.dat5$EquipVintage_bins[which(item109.dat5$Age >= 2005 & item109.dat5$Age < 2010)] <- "2005-2009"
# item109.dat5$EquipVintage_bins[which(item109.dat5$Age >= 2010 & item109.dat5$Age < 2015)] <- "2010-2014"
# item109.dat5$EquipVintage_bins[which(item109.dat5$Age >= 2015)] <- "Post 2014"
# #check uniques
# unique(item109.dat5$EquipVintage_bins)
# 
# item109.customer <- left_join(item109.dat5, rbsa.dat)
# 
# 
# ###########################################
# # add pop and sample sizes for weighting
# ###########################################
# item109.data <- weightedData(item109.customer[-which(colnames(item109.customer) %in% c("Type"
#                                                                                        ,"Age"
#                                                                                        ,"TV.Screen.Type"
#                                                                                        ,"count"
#                                                                                        ,"EquipVintage_bins"))])
# item109.data <- left_join(item109.data, item109.customer[which(colnames(item109.customer) %in% c("CK_Cadmus_ID"
#                                                                                                  ,"Type"
#                                                                                                  ,"Age"
#                                                                                                  ,"TV.Screen.Type"
#                                                                                                  ,"count"
#                                                                                                  ,"EquipVintage_bins"))])
# 
# length(unique(item109.data$CK_Cadmus_ID[which(item109.data$BuildingType == "Manufactured")]))
# #####################
# # Weighted analysis
# #####################
# item109.summary    <- proportionRowsAndColumns1(item109.data
#                                               ,valueVariable       = 'count'
#                                               ,columnVariable      = "EquipVintage_bins"
#                                               ,rowVariable         = "TV.Screen.Type"
#                                               ,aggregateColumnName = "Remove")
# item109.summary <- item109.summary[which(item109.summary$EquipVintage_bins != "Remove"),]
# 
# item109.all.vintages <- proportions_one_group(CustomerLevelData = item109.data
#                                               ,valueVariable = 'count'
#                                               ,groupingVariable = 'TV.Screen.Type'
#                                               ,total.name = "All Vintages"
#                                               ,columnName = "EquipVintage_bins"
#                                               ,weighted = TRUE
#                                               ,two.prop.total = TRUE)
# # item109.all.vintages <- item109.all.vintages[which(item109.all.vintages$TV.Screen.Type != "Total"),]
# 
# 
# item109.final <- rbind.data.frame(item109.summary, item109.all.vintages, stringsAsFactors = F)
# 
# item109.cast <- dcast(setDT(item109.final)
#                       ,BuildingType + EquipVintage_bins ~ TV.Screen.Type
#                       ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
# 
# item109.table <- data.frame("BuildingType"    = item109.cast$BuildingType
#                             ,"Vintage"        = item109.cast$EquipVintage_bins
#                             ,"Percent_CRT"    = item109.cast$w.percent_CRT
#                             ,"SE_CRT"         = item109.cast$w.SE_CRT
#                             ,"Percent_LED"    = item109.cast$w.percent_LED
#                             ,"SE_LED"         = item109.cast$w.SE_LED
#                             ,"Percent_LCD"    = item109.cast$w.percent_LCD
#                             ,"SE_LCD"         = item109.cast$w.SE_LCD
#                             ,"Percent_LED.LCD"= item109.cast$`w.percent_LED LCD`
#                             ,"SE_LED.LCD"     = item109.cast$`w.SE_LED LCD`
#                             ,"Percent_Plasma" = item109.cast$w.percent_Plasma
#                             ,"SE_Plasma"      = item109.cast$w.SE_Plasma
#                             ,"Percent_Other"  = item109.cast$w.percent_Other
#                             ,"SE_Other"       = item109.cast$w.SE_Other
#                             ,"n"              = item109.cast$n_Total
#                             ,"EB_CRT"         = item109.cast$EB_CRT
#                             ,"EB_LED"         = item109.cast$EB_LED
#                             ,"EB_LCD"         = item109.cast$EB_LCD
#                             ,"EB_LED.LCD"     = item109.cast$`EB_LED LCD`
#                             ,"EB_Plasma"      = item109.cast$EB_Plasma
#                             ,"EB_Other"       = item109.cast$EB_Other
#                             )
# 
# unique(item109.table$Vintage)
# rowOrder <- c("Pre 1990"
#               ,"1990-1994"
#               ,"1995-1999"
#               ,"2000-2004"
#               ,"2005-2009"
#               ,"2010-2014"
#               ,"Post 2014"
#               ,"All Vintages")
# item109.table <- item109.table %>% mutate(Vintage = factor(Vintage, levels = rowOrder)) %>% arrange(Vintage)  
# item109.table <- data.frame(item109.table)
# 
# 
# item109.final.SF <- item109.table[which(item109.table$BuildingType == "Single Family")
#                                  ,-which(colnames(item109.table) %in% c("BuildingType"))]
# item109.final.MH <- item109.table[which(item109.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(item109.table) %in% c("BuildingType"))]
# 
# 
# exportTable(item109.final.SF, "SF", "Table 116", weighted = TRUE)
# # exportTable(item109.final.MH, "MH", "Table 91", weighted = TRUE)
# 
# 
# #####################
# # Uneighted analysis
# #####################
# item109.final    <- proportions_two_groups_unweighted(item109.data
#                                               ,valueVariable       = 'count'
#                                               ,columnVariable      = "EquipVintage_bins"
#                                               ,rowVariable         = "TV.Screen.Type"
#                                               ,aggregateColumnName = "Remove")
# item109.final <- item109.final[which(item109.final$EquipVintage_bins != "Remove"),]
# 
# item109.all.vintages <- proportions_one_group(CustomerLevelData = item109.data
#                                               ,valueVariable = 'count'
#                                               ,groupingVariable = 'TV.Screen.Type'
#                                               ,total.name = "All Vintages"
#                                               ,columnName = "EquipVintage_bins"
#                                               ,weighted = FALSE
#                                               ,two.prop.total = TRUE)
# # item109.all.vintages <- item109.all.vintages[which(item109.all.vintages$TV.Screen.Type != "Total"),]
# 
# 
# item109.final <- rbind.data.frame(item109.final, item109.all.vintages, stringsAsFactors = F)
# 
# item109.cast <- dcast(setDT(item109.final)
#                       ,BuildingType + EquipVintage_bins ~ TV.Screen.Type
#                       ,value.var = c("Percent", "SE", "Count", "n"))
# 
# item109.table <- data.frame("BuildingType"    = item109.cast$BuildingType
#                             ,"Vintage"        = item109.cast$EquipVintage_bins
#                             ,"Percent_CRT"    = item109.cast$Percent_CRT
#                             ,"SE_CRT"         = item109.cast$SE_CRT
#                             ,"Percent_LED"    = item109.cast$Percent_LED
#                             ,"SE_LED"         = item109.cast$SE_LED
#                             ,"Percent_LCD"    = item109.cast$Percent_LCD
#                             ,"SE_LCD"         = item109.cast$SE_LCD
#                             ,"Percent_LED.LCD"= item109.cast$`Percent_LED LCD`
#                             ,"SE_LED.LCD"     = item109.cast$`SE_LED LCD`
#                             ,"Percent_Plasma" = item109.cast$Percent_Plasma
#                             ,"SE_Plasma"      = item109.cast$SE_Plasma
#                             ,"Percent_Other"  = item109.cast$Percent_Other
#                             ,"SE_Other"       = item109.cast$SE_Other
#                             ,"n"              = item109.cast$n_Total
#                             )
# 
# 
# unique(item109.table$Vintage)
# rowOrder <- c("Pre 1990"
#               ,"1990-1994"
#               ,"1995-1999"
#               ,"2000-2004"
#               ,"2005-2009"
#               ,"2010-2014"
#               ,"Post 2014"
#               ,"All Vintages")
# item109.table <- item109.table %>% mutate(Vintage = factor(Vintage, levels = rowOrder)) %>% arrange(Vintage)  
# item109.table <- data.frame(item109.table)
# 
# 
# item109.final.SF <- item109.table[which(item109.table$BuildingType == "Single Family")
#                                   ,-which(colnames(item109.table) %in% c("BuildingType"))]
# item109.final.MH <- item109.table[which(item109.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(item109.table) %in% c("BuildingType"))]
# 
# 
# exportTable(item109.final.SF, "SF", "Table 116", weighted = FALSE)
# # exportTable(item109.final.MH, "MH", "Table 91", weighted = FALSE)
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 110: DISTRIBUTION OF TELEVISIONS BY ROOM TYPE (SF table 117, MH table 92)
# #############################################################################################
# #subset to columns needed for analysis
# item110.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
#                                                                     ,"Type"
#                                                                     ,"Clean.Room"))]
# item110.dat$count <- 1
# 
# item110.dat0 <- item110.dat[which(item110.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item110.dat1 <- left_join(item110.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# 
# #subset to only television
# item110.dat2 <- item110.dat1[which(item110.dat1$Type == "Television"),]
# #remove any missing room types
# item110.dat3 <- item110.dat2[which(!(is.na(item110.dat2$Clean.Room))),]
# 
# item110.dat3$Clean.Room[which(item110.dat3$Clean.Room %in% c("Attic"
#                                                              ,"Basement"
#                                                              ,"Crawlspace"
#                                                              ,"Crawl Space"
#                                                              ,"Mechanical"
#                                                              ,"Grow Room"))] <- "Other"
# unique(item110.dat3$Clean.Room[which(item110.dat3$BuildingType == "Single Family")])
# 
# item110.dat3$count <- 1
# item110.customer <- summarise(group_by(item110.dat3, CK_Cadmus_ID, Type,Clean.Room)
#                               ,m_ilk = sum(count))
# item110.merge <- left_join(rbsa.dat, item110.customer)
# item110.merge <- item110.merge[which(!is.na(item110.merge$m_ilk)),]
# 
# 
# 
# ###########################################
# # add pop and sample sizes for weighting
# ###########################################
# item110.data <- weightedData(item110.merge[-which(colnames(item110.merge) %in% c("Type"
#                                                                                  ,"Clean.Room"
#                                                                                  ,"m_ilk"))])
# item110.data <- left_join(item110.data, item110.merge[which(colnames(item110.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Type"
#                                                                                            ,"Clean.Room"
#                                                                                            ,"m_ilk"))])
# 
# 
# #####################
# # Weighted analysis
# #####################
# item110.final    <- proportions_one_group_domain(CustomerLevelData = item110.data
#                                                  ,valueVariable    = 'm_ilk'
#                                                  ,byVariable       = 'Clean.Room'
#                                                  ,aggregateRow     = "Total")
# item110.final.SF <- item110.final[which(item110.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item110.final) %in% c("BuildingType"))]
# exportTable(item110.final.SF, "SF", "Table 117", weighted = TRUE)
# 
# item110.final.MH <- item110.final[which(item110.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item110.final) %in% c("BuildingType"))]
# # exportTable(item110.final.MH, "MH", "Table 92", weighted = TRUE)
# 
# 
# ##############
# # Unweighted
# ##############
# item110.data$count <- 1
# item110.final    <- proportions_one_group(CustomerLevelData = item110.data
#                                           ,valueVariable    = 'm_ilk'
#                                           ,groupingVariable = 'Clean.Room'
#                                           ,total.name       = "Total"
#                                           ,weighted         = FALSE)
# item110.final.SF <- item110.final[which(item110.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item110.final) %in% c("BuildingType"))]
# exportTable(item110.final.SF, "SF", "Table 117", weighted = FALSE)
# 
# item110.final.MH <- item110.final[which(item110.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item110.final) %in% c("BuildingType"))]
# # exportTable(item110.final.MH, "MH", "Table 92", weighted = FALSE)








































############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean os data
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]

#############################################################################################
#Item 107: AVERAGE NUMBER OF TELEVISIONS PER HOME BY CK_Building_ID (SF table 114)
#############################################################################################
#subset to columns needed for analysis
item107.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]
item107.os.dat$count <- 1

item107.os.dat0 <- item107.os.dat[which(item107.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]


item107.os.dat1 <- item107.os.dat0[which(item107.os.dat0$Type == "Television"),]

item107.os.customer <- summarise(group_by(item107.os.dat1, CK_Cadmus_ID)
                              ,Site.Count = sum(count))

item107.os.customer <- left_join(os.dat, item107.os.customer)
item107.os.customer$Site.Count[which(is.na(item107.os.customer$Site.Count))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
item107.os.data <- weightedData(item107.os.customer[-which(colnames(item107.os.customer) %in% c("Site.Count"))])
item107.os.data <- left_join(item107.os.data, unique(item107.os.customer[which(colnames(item107.os.customer) %in% c("CK_Cadmus_ID"
                                                                                                 ,"Site.Count"))]))
item107.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item107.os.final <- mean_one_group(item107.os.data
                                ,valueVariable = 'Site.Count'
                                ,byVariable = 'CK_Building_ID'
                                ,aggregateRow = 'Remove')
item107.os.final <- item107.os.final[which(item107.os.final$CK_Building_ID != "Remove"),]

item107.os.final.SF <- item107.os.final[which(item107.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item107.os.final) %in% c("BuildingType"))]

exportTable(item107.os.final.SF, "SF", "Table 114", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item107.os.final <- mean_one_group_unweighted(item107.os.data
                                           ,valueVariable = 'Site.Count'
                                           ,byVariable = 'CK_Building_ID'
                                           ,aggregateRow = 'Remove')
item107.os.final <- item107.os.final[which(item107.os.final$CK_Building_ID != "Remove"),]

item107.os.final.SF <- item107.os.final[which(item107.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item107.os.final) %in% c("BuildingType"))]

exportTable(item107.os.final.SF, "SF", "Table 114", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
#Item 108: AVERAGE TELEVISION POWER BY VINTAGE (SF table 115, MH table 90)
#############################################################################################
#subset to columns needed for analysis
item108.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,"TV.Wattage"
                                                                    ,"Age"))]
item108.os.dat$count <- 1

item108.os.dat0 <- item108.os.dat[which(item108.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item108.os.dat1 <- left_join(item108.os.dat0, os.dat, by = "CK_Cadmus_ID")

item108.os.dat2 <- item108.os.dat1[which(item108.os.dat1$Type == "Television"),]

#clean wattage
unique(item108.os.dat2$TV.Wattage)
item108.os.dat2$TV.Wattage <- as.numeric(as.character(item108.os.dat2$TV.Wattage))
item108.os.dat3            <- item108.os.dat2[which(!(is.na(item108.os.dat2$TV.Wattage))),]

#clean age
unique(item108.os.dat3$Age)
item108.os.dat3$Age <- as.numeric(as.character(item108.os.dat3$Age))
item108.os.dat4     <- item108.os.dat3#[which(!(is.na(item108.os.dat3$Age))),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item108.os.dat4$Age <- as.numeric(as.character(item108.os.dat4$Age))
item108.os.dat4$EquipVintage_bins <- "Unknown Vintage"
item108.os.dat4$EquipVintage_bins[which(item108.os.dat4$Age <  1990)] <- "Pre 1990"
item108.os.dat4$EquipVintage_bins[which(item108.os.dat4$Age >= 1990 & item108.os.dat4$Age < 1995)] <- "1990-1994"
item108.os.dat4$EquipVintage_bins[which(item108.os.dat4$Age >= 1995 & item108.os.dat4$Age < 2000)] <- "1995-1999"
item108.os.dat4$EquipVintage_bins[which(item108.os.dat4$Age >= 2000 & item108.os.dat4$Age < 2005)] <- "2000-2004"
item108.os.dat4$EquipVintage_bins[which(item108.os.dat4$Age >= 2005 & item108.os.dat4$Age < 2010)] <- "2005-2009"
item108.os.dat4$EquipVintage_bins[which(item108.os.dat4$Age >= 2010 & item108.os.dat4$Age < 2015)] <- "2010-2014"
item108.os.dat4$EquipVintage_bins[which(item108.os.dat4$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item108.os.dat4$EquipVintage_bins)


item108.os.customer <- left_join(os.dat, item108.os.dat4)
item108.os.customer <- item108.os.customer[which(!is.na(item108.os.customer$TV.Wattage)),]

###########################################
# add pop and sample sizes for weighting
###########################################
item108.os.data <- weightedData(item108.os.customer[-which(colnames(item108.os.customer) %in% c("Type"
                                                                                       ,"Age"
                                                                                       ,"TV.Wattage"
                                                                                       ,"count"
                                                                                       ,"EquipVintage_bins"))])
item108.os.data <- left_join(item108.os.data, unique(item108.os.customer[which(colnames(item108.os.customer) %in% c("CK_Cadmus_ID"
                                                                                                 ,"Type"
                                                                                                 ,"Age"
                                                                                                 ,"TV.Wattage"
                                                                                                 ,"count"
                                                                                                 ,"EquipVintage_bins"))]))
#####################
# Weighted analysis    TV.Wattage
#####################
item108.os.cast    <- mean_two_groups(CustomerLevelData = item108.os.data
                                   ,valueVariable = 'TV.Wattage'
                                   ,byVariableRow = 'EquipVintage_bins'
                                   ,byVariableColumn = 'CK_Building_ID'
                                   ,rowAggregate = "All Vintages"
                                   ,columnAggregate = "Remove")

if(os.ind == "scl"){
  item108.os.final <- data.frame("BuildingType"          = item108.os.cast$BuildingType
                                 ,"EquipVintage_bins"    = item108.os.cast$EquipVintage_bins
                                 ,"Mean_SCL.GenPop"      = item108.os.cast$`Mean_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item108.os.cast$`SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item108.os.cast$`n_SCL GenPop`
                                 ,"Mean_SCL.LI"          = item108.os.cast$`Mean_SCL LI`
                                 ,"SE_SCL.LI"            = item108.os.cast$`SE_SCL LI`
                                 ,"n_SCL.LI"             = item108.os.cast$`n_SCL LI`
                                 ,"Mean_SCL.EH"          = item108.os.cast$`Mean_SCL EH`
                                 ,"SE_SCL.EH"            = item108.os.cast$`SE_SCL EH`
                                 ,"n_SCL.EH"             = item108.os.cast$`n_SCL EH`
                                 ,"Mean_2017.RBSA.PS"    = item108.os.cast$`Mean_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item108.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item108.os.cast$`n_2017 RBSA PS`
                                 ,"EB_SCL.GenPop"        = item108.os.cast$`EB_SCL GenPop`
                                 ,"EB_SCL.LI"            = item108.os.cast$`EB_SCL LI`
                                 ,"EB_SCL.EH"            = item108.os.cast$`EB_SCL EH`
                                 ,"EB_2017.RBSA.PS"      = item108.os.cast$`EB_2017 RBSA PS`)

}else if(os.ind == "snopud"){
  item108.os.final <- data.frame("BuildingType"          = item108.os.cast$BuildingType
                                 ,"EquipVintage_bins"    = item108.os.cast$EquipVintage_bins
                                 ,"Mean_SnoPUD"          = item108.os.cast$`Mean_SnoPUD`
                                 ,"SE_SnoPUD"            = item108.os.cast$`SE_SnoPUD`
                                 ,"n_SnoPUD"             = item108.os.cast$`n_SnoPUD`
                                 ,"Mean_2017.RBSA.PS"    = item108.os.cast$`Mean_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item108.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item108.os.cast$`n_2017 RBSA PS`
                                 ,"Mean_RBSA.NW"         = item108.os.cast$`Mean_2017 RBSA NW`
                                 ,"SE_RBSA.NW"           = item108.os.cast$`SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"            = item108.os.cast$`n_2017 RBSA NW`
                                 ,"EB_SnoPUD"            = item108.os.cast$`EB_SnoPUD`
                                 ,"EB_2017.RBSA.PS"      = item108.os.cast$`EB_2017 RBSA PS`
                                 ,"EB_RBSA.NW"           = item108.os.cast$`EB_2017 RBSA NW`)

}

unique(item108.os.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Unknown Vintage"
              ,"All Vintages")
item108.os.table <- item108.os.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)
item108.os.table <- data.frame(item108.os.table)

item108.os.final.SF <- item108.os.table[which(item108.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item108.os.table) %in% c("BuildingType"))]

exportTable(item108.os.final.SF, "SF", "Table 115", weighted = TRUE, osIndicator = export.ind, OS = T)

##############
# Unweighted
##############
item108.os.cast    <- mean_two_groups_unweighted(CustomerLevelData = item108.os.data
                                      ,valueVariable = 'TV.Wattage'
                                      ,byVariableRow = 'EquipVintage_bins'
                                      ,byVariableColumn = 'CK_Building_ID'
                                      ,rowAggregate = "All Vintages"
                                      ,columnAggregate = "Remove")

if(os.ind == "scl"){
  item108.os.final <- data.frame("BuildingType"          = item108.os.cast$BuildingType
                                 ,"EquipVintage_bins"    = item108.os.cast$EquipVintage_bins
                                 ,"Mean_SCL.GenPop"      = item108.os.cast$`Mean_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item108.os.cast$`SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item108.os.cast$`n_SCL GenPop`
                                 ,"Mean_SCL.LI"          = item108.os.cast$`Mean_SCL LI`
                                 ,"SE_SCL.LI"            = item108.os.cast$`SE_SCL LI`
                                 ,"n_SCL.LI"             = item108.os.cast$`n_SCL LI`
                                 ,"Mean_SCL.EH"          = item108.os.cast$`Mean_SCL EH`
                                 ,"SE_SCL.EH"            = item108.os.cast$`SE_SCL EH`
                                 ,"n_SCL.EH"             = item108.os.cast$`n_SCL EH`
                                 ,"Mean_2017.RBSA.PS"    = item108.os.cast$`Mean_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item108.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item108.os.cast$`n_2017 RBSA PS`)

}else if(os.ind == "snopud"){
  item108.os.final <- data.frame("BuildingType"          = item108.os.cast$BuildingType
                                 ,"EquipVintage_bins"    = item108.os.cast$EquipVintage_bins
                                 ,"Mean_SnoPUD"          = item108.os.cast$`Mean_SnoPUD`
                                 ,"SE_SnoPUD"            = item108.os.cast$`SE_SnoPUD`
                                 ,"n_SnoPUD"             = item108.os.cast$`n_SnoPUD`
                                 ,"Mean_2017.RBSA.PS"    = item108.os.cast$`Mean_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item108.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item108.os.cast$`n_2017 RBSA PS`
                                 ,"Mean_RBSA.NW"         = item108.os.cast$`Mean_2017 RBSA NW`
                                 ,"SE_RBSA.NW"           = item108.os.cast$`SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"            = item108.os.cast$`n_2017 RBSA NW`)

}
unique(item108.os.final$EquipVintage_bins)
rowOrder <- c("Pre 1990"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Unknown Vintage"
              ,"All Vintages")
item108.os.table <- item108.os.final %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)
item108.os.table <- data.frame(item108.os.table)

item108.os.final.SF <- item108.os.table[which(item108.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item108.os.table) %in% c("BuildingType"))]

exportTable(item108.os.final.SF, "SF", "Table 115", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
#Item 109: DISTRIBUTION OF TELEVISION SCREENS BY TYPE AND VINTAGE (SF table 116, MH table 91)
#############################################################################################
#subset to columns needed for analysis
item109.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,"TV.Screen.Type"
                                                                    ,"Age"))]
item109.os.dat$count <- 1

item109.os.dat0 <- item109.os.dat[which(item109.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item109.os.dat1 <- left_join(item109.os.dat0, os.dat, by = "CK_Cadmus_ID")

item109.os.dat2 <- item109.os.dat1[which(item109.os.dat1$Type == "Television"),]

#clean screen type
unique(item109.os.dat2$TV.Screen.Type)
item109.os.dat3 <- item109.os.dat2[which(!(is.na(item109.os.dat2$TV.Screen.Type))),]
item109.os.dat4 <- item109.os.dat3[which(!(item109.os.dat3$TV.Screen.Type %in% c("Unknown"))),]

item109.os.dat4$TV.Screen.Type[grep("Tube",item109.os.dat4$TV.Screen.Type)] <- "CRT"
item109.os.dat4$TV.Screen.Type[which(item109.os.dat4$TV.Screen.Type %in% c("Other", "Projector (Non-CRT)", "DLP"))] <- "Other"
unique(item109.os.dat4$TV.Screen.Type)


#clean age
unique(item109.os.dat4$Age)
item109.os.dat4$Age <- as.numeric(as.character(item109.os.dat4$Age))
item109.os.dat5     <- item109.os.dat4[which(!(is.na(item109.os.dat4$Age))),]

# Bin equipment vintages for items 50 and 52 (4 categories)
item109.os.dat5$EquipVintage_bins <- as.numeric(as.character(item109.os.dat5$Age))

item109.os.dat5$EquipVintage_bins[which(item109.os.dat5$Age < 1990)] <- "Pre 1990"
item109.os.dat5$EquipVintage_bins[which(item109.os.dat5$Age >= 1990 & item109.os.dat5$Age < 1995)] <- "1990-1994"
item109.os.dat5$EquipVintage_bins[which(item109.os.dat5$Age >= 1995 & item109.os.dat5$Age < 2000)] <- "1995-1999"
item109.os.dat5$EquipVintage_bins[which(item109.os.dat5$Age >= 2000 & item109.os.dat5$Age < 2005)] <- "2000-2004"
item109.os.dat5$EquipVintage_bins[which(item109.os.dat5$Age >= 2005 & item109.os.dat5$Age < 2010)] <- "2005-2009"
item109.os.dat5$EquipVintage_bins[which(item109.os.dat5$Age >= 2010 & item109.os.dat5$Age < 2015)] <- "2010-2014"
item109.os.dat5$EquipVintage_bins[which(item109.os.dat5$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item109.os.dat5$EquipVintage_bins)

item109.os.customer <- left_join(os.dat, item109.os.dat5)
item109.os.customer <- item109.os.customer[which(!is.na(item109.os.customer$EquipVintage_bins)),]

###########################################
# add pop and sample sizes for weighting
###########################################
item109.os.data <- weightedData(item109.os.customer[-which(colnames(item109.os.customer) %in% c("Type"
                                                                                       ,"Age"
                                                                                       ,"TV.Screen.Type"
                                                                                       ,"count"
                                                                                       ,"EquipVintage_bins"))])
item109.os.data <- left_join(item109.os.data, unique(item109.os.customer[which(colnames(item109.os.customer) %in% c("CK_Cadmus_ID"
                                                                                                 ,"Type"
                                                                                                 ,"Age"
                                                                                                 ,"TV.Screen.Type"
                                                                                                 ,"count"
                                                                                                 ,"EquipVintage_bins"))]))
item109.os.data <- item109.os.data[which(item109.os.data$CK_Building_ID == subset.ind),]
#####################
# Weighted analysis
#####################
item109.os.summary    <- proportionRowsAndColumns1(item109.os.data
                                                ,valueVariable       = 'count'
                                                ,columnVariable      = "EquipVintage_bins"
                                                ,rowVariable         = "TV.Screen.Type"
                                                ,aggregateColumnName = "Remove")
item109.os.summary <- item109.os.summary[which(item109.os.summary$EquipVintage_bins != "Remove"),]

item109.os.all.vintages <- proportions_one_group(CustomerLevelData = item109.os.data
                                              ,valueVariable = 'count'
                                              ,groupingVariable = 'TV.Screen.Type'
                                              ,total.name = "All Vintages"
                                              ,columnName = "EquipVintage_bins"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
# item109.os.all.vintages <- item109.os.all.vintages[which(item109.os.all.vintages$TV.Screen.Type != "Total"),]


item109.os.final <- rbind.data.frame(item109.os.summary, item109.os.all.vintages, stringsAsFactors = F)

item109.os.cast <- dcast(setDT(item109.os.final)
                      ,BuildingType + EquipVintage_bins ~ TV.Screen.Type
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
names(item109.os.cast)
if(os.ind == "scl"){
  item109.os.table <- data.frame("BuildingType"    = item109.os.cast$BuildingType
                                 ,"Vintage"        = item109.os.cast$EquipVintage_bins
                                 ,"Percent_CRT"    = item109.os.cast$w.percent_CRT
                                 ,"SE_CRT"         = item109.os.cast$w.SE_CRT
                                 ,"Percent_LED"    = item109.os.cast$w.percent_LED
                                 ,"SE_LED"         = item109.os.cast$w.SE_LED
                                 ,"Percent_LCD"    = item109.os.cast$w.percent_LCD
                                 ,"SE_LCD"         = item109.os.cast$w.SE_LCD
                                 ,"Percent_LED.LCD"= item109.os.cast$`w.percent_LED LCD`
                                 ,"SE_LED.LCD"     = item109.os.cast$`w.SE_LED LCD`
                                 ,"Percent_Plasma" = item109.os.cast$w.percent_Plasma
                                 ,"SE_Plasma"      = item109.os.cast$w.SE_Plasma
                                 ,"Percent_Other"  = item109.os.cast$w.percent_Other
                                 ,"SE_Other"       = item109.os.cast$w.SE_Other
                                 ,"n"              = item109.os.cast$n_Total
                                 ,"EB_CRT"         = item109.os.cast$EB_CRT
                                 ,"EB_LED"         = item109.os.cast$EB_LED
                                 ,"EB_LCD"         = item109.os.cast$EB_LCD
                                 ,"EB_LED.LCD"     = item109.os.cast$`EB_LED LCD`
                                 ,"EB_Plasma"      = item109.os.cast$EB_Plasma
                                 ,"EB_Other"       = item109.os.cast$EB_Other
  )

}else if(os.ind == "snopud"){
  item109.os.table <- data.frame("BuildingType"    = item109.os.cast$BuildingType
                                 ,"Vintage"        = item109.os.cast$EquipVintage_bins
                                 ,"Percent_CRT"    = item109.os.cast$w.percent_CRT
                                 ,"SE_CRT"         = item109.os.cast$w.SE_CRT
                                 ,"Percent_LED"    = item109.os.cast$w.percent_LED
                                 ,"SE_LED"         = item109.os.cast$w.SE_LED
                                 ,"Percent_LCD"    = item109.os.cast$w.percent_LCD
                                 ,"SE_LCD"         = item109.os.cast$w.SE_LCD
                                 # ,"Percent_LED.LCD"= item109.os.cast$`w.percent_LED LCD`
                                 # ,"SE_LED.LCD"     = item109.os.cast$`w.SE_LED LCD`
                                 ,"Percent_Plasma" = item109.os.cast$w.percent_Plasma
                                 ,"SE_Plasma"      = item109.os.cast$w.SE_Plasma
                                 ,"Percent_Other"  = item109.os.cast$w.percent_Other
                                 ,"SE_Other"       = item109.os.cast$w.SE_Other
                                 ,"n"              = item109.os.cast$n_Total
                                 ,"EB_CRT"         = item109.os.cast$EB_CRT
                                 ,"EB_LED"         = item109.os.cast$EB_LED
                                 ,"EB_LCD"         = item109.os.cast$EB_LCD
                                 # ,"EB_LED.LCD"     = item109.os.cast$`EB_LED LCD`
                                 ,"EB_Plasma"      = item109.os.cast$EB_Plasma
                                 ,"EB_Other"       = item109.os.cast$EB_Other
  )

}


unique(item109.os.table$Vintage)
rowOrder <- c("Pre 1990"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"All Vintages")
item109.os.table <- item109.os.table %>% mutate(Vintage = factor(Vintage, levels = rowOrder)) %>% arrange(Vintage)
item109.os.table <- data.frame(item109.os.table)

item109.os.final.SF <- item109.os.table[which(item109.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item109.os.table) %in% c("BuildingType"))]

exportTable(item109.os.final.SF, "SF", "Table 116", weighted = TRUE, osIndicator = export.ind, OS = T)

#####################
# Uneighted analysis
#####################
item109.os.final    <- proportions_two_groups_unweighted(item109.os.data
                                                      ,valueVariable       = 'count'
                                                      ,columnVariable      = "EquipVintage_bins"
                                                      ,rowVariable         = "TV.Screen.Type"
                                                      ,aggregateColumnName = "Remove")
item109.os.final <- item109.os.final[which(item109.os.final$EquipVintage_bins != "Remove"),]

item109.os.all.vintages <- proportions_one_group(CustomerLevelData = item109.os.data
                                              ,valueVariable = 'count'
                                              ,groupingVariable = 'TV.Screen.Type'
                                              ,total.name = "All Vintages"
                                              ,columnName = "EquipVintage_bins"
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
# item109.os.all.vintages <- item109.os.all.vintages[which(item109.os.all.vintages$TV.Screen.Type != "Total"),]

item109.os.final <- rbind.data.frame(item109.os.final, item109.os.all.vintages, stringsAsFactors = F)

item109.os.cast <- dcast(setDT(item109.os.final)
                      ,BuildingType + EquipVintage_bins ~ TV.Screen.Type
                      ,value.var = c("Percent", "SE", "Count", "n"))

if(os.ind == "scl"){
  item109.os.table <- data.frame("BuildingType"    = item109.os.cast$BuildingType
                                 ,"Vintage"        = item109.os.cast$EquipVintage_bins
                                 ,"Percent_CRT"    = item109.os.cast$Percent_CRT
                                 ,"SE_CRT"         = item109.os.cast$SE_CRT
                                 ,"Percent_LED"    = item109.os.cast$Percent_LED
                                 ,"SE_LED"         = item109.os.cast$SE_LED
                                 ,"Percent_LCD"    = item109.os.cast$Percent_LCD
                                 ,"SE_LCD"         = item109.os.cast$SE_LCD
                                 ,"Percent_LED.LCD"= item109.os.cast$`Percent_LED LCD`
                                 ,"SE_LED.LCD"     = item109.os.cast$`SE_LED LCD`
                                 ,"Percent_Plasma" = item109.os.cast$Percent_Plasma
                                 ,"SE_Plasma"      = item109.os.cast$SE_Plasma
                                 ,"Percent_Other"  = item109.os.cast$Percent_Other
                                 ,"SE_Other"       = item109.os.cast$SE_Other
                                 ,"n"              = item109.os.cast$n_Total
  )

}else if(os.ind == "snopud"){
  item109.os.table <- data.frame("BuildingType"    = item109.os.cast$BuildingType
                                 ,"Vintage"        = item109.os.cast$EquipVintage_bins
                                 ,"Percent_CRT"    = item109.os.cast$Percent_CRT
                                 ,"SE_CRT"         = item109.os.cast$SE_CRT
                                 ,"Percent_LED"    = item109.os.cast$Percent_LED
                                 ,"SE_LED"         = item109.os.cast$SE_LED
                                 ,"Percent_LCD"    = item109.os.cast$Percent_LCD
                                 ,"SE_LCD"         = item109.os.cast$SE_LCD
                                 # ,"Percent_LED.LCD"= item109.os.cast$`Percent_LED LCD`
                                 # ,"SE_LED.LCD"     = item109.os.cast$`SE_LED LCD`
                                 ,"Percent_Plasma" = item109.os.cast$Percent_Plasma
                                 ,"SE_Plasma"      = item109.os.cast$SE_Plasma
                                 ,"Percent_Other"  = item109.os.cast$Percent_Other
                                 ,"SE_Other"       = item109.os.cast$SE_Other
                                 ,"n"              = item109.os.cast$n_Total
  )

}

unique(item109.os.table$Vintage)
rowOrder <- c("Pre 1990"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"All Vintages")
item109.os.table <- item109.os.table %>% mutate(Vintage = factor(Vintage, levels = rowOrder)) %>% arrange(Vintage)
item109.os.table <- data.frame(item109.os.table)


item109.os.final.SF <- item109.os.table[which(item109.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item109.os.table) %in% c("BuildingType"))]

exportTable(item109.os.final.SF, "SF", "Table 116", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
#Item 110: DISTRIBUTION OF TELEVISIONS BY ROOM TYPE (SF table 117, MH table 92)
#############################################################################################
#subset to columns needed for analysis
item110.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,"Clean.Room"))]
item110.os.dat$count <- 1

item110.os.dat0 <- item110.os.dat[which(item110.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item110.os.dat1 <- left_join(item110.os.dat0, os.dat, by = "CK_Cadmus_ID")

#subset to only television
item110.os.dat2 <- item110.os.dat1[which(item110.os.dat1$Type == "Television"),]
#remove any missing room types
item110.os.dat3 <- item110.os.dat2[which(!(is.na(item110.os.dat2$Clean.Room))),]

item110.os.dat3$Clean.Room[which(item110.os.dat3$Clean.Room %in% c("Attic"
                                                             ,"Basement"
                                                             ,"Crawlspace"
                                                             ,"Crawl Space"
                                                             ,"Mechanical"
                                                             ,"Grow Room"))] <- "Other"
unique(item110.os.dat3$Clean.Room[which(item110.os.dat3$BuildingType == "Single Family")])

item110.os.dat3$count <- 1
item110.os.customer <- summarise(group_by(item110.os.dat3, CK_Cadmus_ID, CK_Building_ID, Type,Clean.Room)
                              ,m_ilk = sum(count))
item110.os.merge <- left_join(os.dat, item110.os.customer)
item110.os.merge <- item110.os.merge[which(!is.na(item110.os.merge$m_ilk)),]

###########################################
# add pop and sample sizes for weighting
###########################################
item110.os.data <- weightedData(item110.os.merge[-which(colnames(item110.os.merge) %in% c("Type"
                                                                                 ,"Clean.Room"
                                                                                 ,"m_ilk"))])
item110.os.data <- left_join(item110.os.data, unique(item110.os.merge[which(colnames(item110.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Type"
                                                                                           ,"Clean.Room"
                                                                                           ,"m_ilk"))]))
#####################
# Weighted analysis
#####################
item110.os.final <- proportionRowsAndColumns1(CustomerLevelData = item110.os.data
                                             ,valueVariable = "m_ilk"
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = "Clean.Room"
                                             ,aggregateColumnName = "Remove")
item110.os.final <- item110.os.final[which(item110.os.final$CK_Building_ID != "Remove"),]

item110.os.cast <- dcast(setDT(item110.os.final)
                        ,formula = BuildingType + Clean.Room ~ CK_Building_ID
                        ,value.var = c("w.percent", "w.SE","n", "EB"))

if(os.ind == "scl"){
  item110.os.table <- data.frame("BuildingType"          = item110.os.cast$BuildingType
                                 ,"Clean.Room"           = item110.os.cast$Clean.Room
                                 ,"Percent_SCL.GenPop"   = item110.os.cast$`w.percent_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item110.os.cast$`w.SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item110.os.cast$`n_SCL GenPop`
                                 ,"Percent_SCL.LI"       = item110.os.cast$`w.percent_SCL LI`
                                 ,"SE_SCL.LI"            = item110.os.cast$`w.SE_SCL LI`
                                 ,"n_SCL.LI"             = item110.os.cast$`n_SCL LI`
                                 ,"Percent_SCL.EH"       = item110.os.cast$`w.percent_SCL EH`
                                 ,"SE_SCL.EH"            = item110.os.cast$`w.SE_SCL EH`
                                 ,"n_SCL.EH"             = item110.os.cast$`n_SCL EH`
                                 ,"Percent_2017.RBSA.PS" = item110.os.cast$`w.percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item110.os.cast$`w.SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item110.os.cast$`n_2017 RBSA PS`
                                 ,"EB_SCL.GenPop"        = item110.os.cast$`EB_SCL GenPop`
                                 ,"EB_SCL.LI"            = item110.os.cast$`EB_SCL LI`
                                 ,"EB_SCL.EH"            = item110.os.cast$`EB_SCL EH`
                                 ,"EB_2017.RBSA.PS"      = item110.os.cast$`EB_2017 RBSA PS`)

}else if(os.ind == "snopud"){
  item110.os.table <- data.frame("BuildingType"          = item110.os.cast$BuildingType
                                 ,"Clean.Room"           = item110.os.cast$Clean.Room
                                 ,"Percent_SnoPUD"          = item110.os.cast$`w.percent_SnoPUD`
                                 ,"SE_SnoPUD"               = item110.os.cast$`w.SE_SnoPUD`
                                 ,"n_SnoPUD"                = item110.os.cast$`n_SnoPUD`
                                 ,"Percent_2017.RBSA.PS"    = item110.os.cast$`w.percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"         = item110.os.cast$`w.SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"          = item110.os.cast$`n_2017 RBSA PS`
                                 ,"Percent_RBSA.NW"         = item110.os.cast$`w.percent_2017 RBSA NW`
                                 ,"SE_RBSA.NW"              = item110.os.cast$`w.SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"               = item110.os.cast$`n_2017 RBSA NW`
                                 ,"EB_SnoPUD"               = item110.os.cast$`EB_SnoPUD`
                                 ,"EB_2017.RBSA.PS"         = item110.os.cast$`EB_2017 RBSA PS`
                                 ,"EB_RBSA.NW"              = item110.os.cast$`EB_2017 RBSA NW`)

}

item110.os.final.SF <- item110.os.table[which(item110.os.table$BuildingType == "Single Family")
                                      ,-which(colnames(item110.os.table) %in% c("BuildingType"))]

exportTable(item110.os.final.SF, "SF", "Table 117", weighted = TRUE, osIndicator = export.ind, OS = T)

##############
# Unweighted
##############
item110.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item110.os.data
                                              ,valueVariable = "m_ilk"
                                              ,columnVariable = "CK_Building_ID"
                                              ,rowVariable = "Clean.Room"
                                              ,aggregateColumnName = "Remove")
item110.os.final <- item110.os.final[which(item110.os.final$CK_Building_ID != "Remove"),]

item110.os.cast <- dcast(setDT(item110.os.final)
                         ,formula = BuildingType + Clean.Room ~ CK_Building_ID
                         ,value.var = c("Percent", "SE","n"))

if(os.ind == "scl"){
  item110.os.table <- data.frame("BuildingType"          = item110.os.cast$BuildingType
                                 ,"Clean.Room"           = item110.os.cast$Clean.Room
                                 ,"Percent_SCL.GenPop"   = item110.os.cast$`Percent_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item110.os.cast$`SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item110.os.cast$`n_SCL GenPop`
                                 ,"Percent_SCL.LI"       = item110.os.cast$`Percent_SCL LI`
                                 ,"SE_SCL.LI"            = item110.os.cast$`SE_SCL LI`
                                 ,"n_SCL.LI"             = item110.os.cast$`n_SCL LI`
                                 ,"Percent_SCL.EH"       = item110.os.cast$`Percent_SCL EH`
                                 ,"SE_SCL.EH"            = item110.os.cast$`SE_SCL EH`
                                 ,"n_SCL.EH"             = item110.os.cast$`n_SCL EH`
                                 ,"Percent_2017.RBSA.PS" = item110.os.cast$`Percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item110.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item110.os.cast$`n_2017 RBSA PS`)

}else if(os.ind == "snopud"){
  item110.os.table <- data.frame("BuildingType"          = item110.os.cast$BuildingType
                                 ,"Clean.Room"           = item110.os.cast$Clean.Room
                                 ,"Percent_SnoPUD"          = item110.os.cast$`Percent_SnoPUD`
                                 ,"SE_SnoPUD"               = item110.os.cast$`SE_SnoPUD`
                                 ,"n_SnoPUD"                = item110.os.cast$`n_SnoPUD`
                                 ,"Percent_2017.RBSA.PS"    = item110.os.cast$`Percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"         = item110.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"          = item110.os.cast$`n_2017 RBSA PS`
                                 ,"Percent_RBSA.NW"         = item110.os.cast$`Percent_2017 RBSA NW`
                                 ,"SE_RBSA.NW"              = item110.os.cast$`SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"               = item110.os.cast$`n_2017 RBSA NW`)

}

item110.os.final.SF <- item110.os.table[which(item110.os.table$BuildingType == "Single Family")
                                  ,which(colnames(item110.os.table) %notin% c("BuildingType"))]

exportTable(item110.os.final.SF, "SF", "Table 117", weighted = FALSE, osIndicator = export.ind, OS = T)

