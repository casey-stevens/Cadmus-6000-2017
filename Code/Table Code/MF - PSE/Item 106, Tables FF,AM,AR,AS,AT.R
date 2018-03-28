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
length(unique(rbsa.dat$CK_Cadmus_ID)) 
rbsa.dat <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID,ignore.case = T),]

#Read in data for analysis
# water.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, water.export))
#clean cadmus IDs
water.dat$CK_Cadmus_ID <- trimws(toupper(water.dat$CK_Cadmus_ID))



#############################################################################################
#Item 106: DISTRIBUTION OF SHOWERHEAD FLOW RATE BY STATE (SF table 113, MH table 88)
#############################################################################################
#subset to columns needed for analysis
item106.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
item106.dat$count <- 1

item106.dat0 <- item106.dat[which(item106.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item106.dat1 <- left_join(item106.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item106.dat1$GPM_Measured <- as.numeric(as.character(item106.dat1$GPM_Measured))
item106.dat2 <- item106.dat1[which(!(is.na(item106.dat1$GPM_Measured))),]
unique(item106.dat2$GPM_Measured)

item106.dat3 <- item106.dat2[grep("shower|Shower",item106.dat2$Fixture.Type),]

item106.dat4 <- summarise(group_by(item106.dat3, CK_Cadmus_ID, BuildingType, State, count)
                          ,GPM.Measured.Site = mean(GPM_Measured))

item106.dat4$GPM_bins <- item106.dat4$GPM.Measured.Site
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site <= 1.5)] <- "< 1.5"
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >  1.5 & item106.dat4$GPM.Measured.Site < 2.1)] <- "1.6-2.0"
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >= 2.1 & item106.dat4$GPM.Measured.Site < 2.6)] <- "2.1-2.5"
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >= 2.6 & item106.dat4$GPM.Measured.Site < 3.6)] <- "2.6-3.5"
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >= 3.6)] <- "> 3.6"
# item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >= 2.6)] <- "> 2.5"
unique(item106.dat4$GPM_bins)

item106.merge <- left_join(rbsa.dat, item106.dat4)
item106.merge <- item106.merge[which(!is.na(item106.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
item106.data <- weightedData(item106.merge[-which(colnames(item106.merge) %in% c("GPM.Measured.Site"               
                                                                            ,"GPM_bins"
                                                                            ,"count"
                                                                            ,"Category"))])
item106.data <- left_join(item106.data, item106.merge[which(colnames(item106.merge) %in% c("CK_Cadmus_ID"
                                                                                     ,"GPM.Measured.Site"               
                                                                                     ,"GPM_bins"
                                                                                     ,"count"
                                                                                     ,"Category"))])
#######################
# Weighted Analysis
#######################
item106.final <- proportionRowsAndColumns1(CustomerLevelData = item106.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'Category'
                                          ,rowVariable      = 'GPM_bins'
                                          ,aggregateColumnName = "Remove")

item106.cast <- dcast(setDT(item106.final)
                     , formula = BuildingType + GPM_bins ~ Category
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item106.table <- data.frame("Flow.Rate.GPM"   = item106.cast$GPM_bins
                            ,"PSE.Percent"                 = item106.cast$w.percent_PSE
                            ,"PSE.SE"                      = item106.cast$w.SE_PSE
                            ,"PSE.n"                       = item106.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item106.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item106.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item106.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item106.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item106.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item106.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item106.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item106.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = item106.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = item106.cast$EB_PSE
                            ,"PSE.King.County_EB"          = item106.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = item106.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = item106.cast$`EB_2017 RBSA PS`
)

levels(item106.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106.table <- item106.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)
item106.table <- data.frame(item106.table)

exportTable(item106.table, "MF", "Table 80", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# Unweighted Analysis
#######################
item106.final <- proportions_two_groups_unweighted(CustomerLevelData = item106.data
                                                  ,valueVariable    = 'count'
                                                  ,columnVariable   = 'State'
                                                  ,rowVariable      = 'GPM_bins'
                                                  ,aggregateColumnName = "Region")

item106.cast <- dcast(setDT(item106.final)
                     , formula = BuildingType + GPM_bins ~ State
                     , value.var = c("Percent", "SE", "Count", "n"))


item106.table <- data.frame("Flow.Rate.GPM"   = item106.cast$GPM_bins
                            ,"PSE.Percent"                 = item106.cast$Percent_PSE
                            ,"PSE.SE"                      = item106.cast$SE_PSE
                            ,"PSE.n"                       = item106.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item106.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item106.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item106.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item106.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item106.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item106.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item106.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item106.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = item106.cast$`n_2017 RBSA PS`
)
stopifnot(sum(item106.table[which(item106.table$BuildingType == "Single Family")
                           ,grep("Percent",colnames(item106.table))], na.rm = T) == 10)

levels(item106.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106.table <- item106.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)
item106.table <- data.frame(item106.table)

exportTable(item106.table, "MF", "Table 73", weighted = FALSE,OS = T, osIndicator = "PSE")




#############################################################################################
# Table FF: PERCENTAGE OF HOMES WITH SHOWERHEADS ABOVE 2.0 GPM BY STATE
#############################################################################################
#subset to columns needed for analysis
tableFF.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
tableFF.dat$count <- 1

tableFF.dat0 <- tableFF.dat[which(tableFF.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableFF.dat1 <- left_join(rbsa.dat, tableFF.dat0, by = "CK_Cadmus_ID")

tableFF.dat1$GPM_Measured <- as.numeric(as.character(tableFF.dat1$GPM_Measured))
tableFF.dat2 <- tableFF.dat1[which(!(is.na(tableFF.dat1$GPM_Measured))),]
unique(tableFF.dat2$GPM_Measured)

tableFF.dat3 <- tableFF.dat2[grep("shower|Shower",tableFF.dat2$Fixture.Type),]

tableFF.dat4 <- summarise(group_by(tableFF.dat3, CK_Cadmus_ID)
                          ,GPM.Measured.Site = mean(GPM_Measured))

tableFF.dat4$Ind <- 0
tableFF.dat4$Ind[which(tableFF.dat4$GPM.Measured.Site > 2)] <- 1


tableFF.merge <- left_join(rbsa.dat, tableFF.dat4)
tableFF.merge <- tableFF.merge[which(!is.na(tableFF.merge$Ind)),]


################################################
# Adding pop and sample sizes for weights
################################################
tableFF.data <- weightedData(tableFF.merge[-which(colnames(tableFF.merge) %in% c("GPM.Measured.Site"
                                                                                 ,"Ind"
                                                                                 ,"Category"))])
tableFF.data <- left_join(tableFF.data, tableFF.merge[which(colnames(tableFF.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"
                                                                                           ,"Ind"
                                                                                           ,"Category"))])
tableFF.data$count <- 1
tableFF.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableFF.data$State <- tableFF.data$Category
tableFF.table.MF <- proportions_one_group(CustomerLevelData = tableFF.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Remove"
                                       ,weighted = TRUE)
tableFF.table.MF <- tableFF.table.MF[which(tableFF.table.MF$State != "Total"),]

tableFF.final.MF <- tableFF.table.MF[which(tableFF.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableFF.table.MF) %in% c("BuildingType"))]
exportTable(tableFF.final.MF, "MF", "Table FF", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# Unweighted Analysis
#######################
tableFF.table.MF <- proportions_one_group(CustomerLevelData = tableFF.data
                                          ,valueVariable = "Ind"
                                          ,groupingVariable = "State"
                                          ,total.name = "Region"
                                          ,weighted = FALSE)
tableFF.table.MF <- tableFF.table.MF[which(tableFF.table.MF$State != "Total"),]

tableFF.final.MF <- tableFF.table.MF[which(tableFF.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableFF.table.MF) %in% c("BuildingType"))]
exportTable(tableFF.final.MF, "MF", "Table FF", weighted = FALSE,OS = T, osIndicator = "PSE")









#############################################################################################
#Table AM: Average number of showerheads and faucets per home by Type and State
#############################################################################################
#subset to columns needed for analysis
tableAM.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
tableAM.dat$count <- 1

tableAM.dat0 <- tableAM.dat[which(tableAM.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableAM.dat1 <- left_join(tableAM.dat0, rbsa.dat, by = "CK_Cadmus_ID")

tableAM.dat1$GPM_Measured <- as.numeric(as.character(tableAM.dat1$GPM_Measured))
tableAM.dat2 <- tableAM.dat1[which(!(is.na(tableAM.dat1$GPM_Measured))),]
unique(tableAM.dat2$GPM_Measured)
unique(tableAM.dat2$Fixture.Type)
tableAM.dat3 <- tableAM.dat2[grep("bathroom|faucet|shower",tableAM.dat2$Fixture.Type,ignore.case = T),]
tableAM.dat3$count <- 1
tableAM.dat3$Count <- 1

#cast the melt example code
tableAM.cast <- dcast(setDT(tableAM.dat3)
                      ,formula = CK_Cadmus_ID ~ Fixture.Type,sum
                      ,value.var = c("Count"))
tableAM.cast[is.na(tableAM.cast),] <- 0

tableAM.melt <- melt(tableAM.cast, id.vars = "CK_Cadmus_ID")
names(tableAM.melt) <- c("CK_Cadmus_ID", "Fixture.Type", "Count")

tableAM.dat4 <- summarise(group_by(tableAM.melt, CK_Cadmus_ID, Fixture.Type)
                          ,Site.Count = sum(Count))

tableAM.merge <- left_join(rbsa.dat, tableAM.dat4)
tableAM.merge$Site.Count[which(is.na(tableAM.merge$Site.Count))] <- 0
tableAM.merge <- tableAM.merge[which(tableAM.merge$Fixture.Type %notin% c("N/A",NA)),]

unique(tableAM.merge$Fixture.Type)
################################################
# Adding pop and sample sizes for weights
################################################
tableAM.data <- weightedData(tableAM.merge[-which(colnames(tableAM.merge) %in% c("Site.Count"
                                                                                 ,"Fixture.Type"
                                                                                 ,"Category"))])
tableAM.data <- left_join(tableAM.data, tableAM.merge[which(colnames(tableAM.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Site.Count"
                                                                                           ,"Fixture.Type"
                                                                                           ,"Category"))])
tableAM.data$count <- 1
#######################
# Weighted Analysis
#######################
tableAM.cast <- mean_two_groups(CustomerLevelData = tableAM.data
                                ,valueVariable = "Site.Count"
                                ,byVariableRow = "Fixture.Type"
                                ,byVariableColumn = "Category"
                                ,columnAggregate = "Remove"
                                ,rowAggregate = "All Fixtures")

tableAM.table <- data.frame("BuildingType"    = tableAM.cast$BuildingType
                            ,"Fixture.Type"   = tableAM.cast$Fixture.Type
                            ,"PSE.Mean"                 = tableAM.cast$Mean_PSE
                            ,"PSE.SE"                   = tableAM.cast$SE_PSE
                            ,"PSE.n"                    = tableAM.cast$n_PSE
                            ,"PSE.King.County.Mean"     = tableAM.cast$`Mean_PSE KING COUNTY`
                            ,"PSE.King.County.SE"       = tableAM.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"        = tableAM.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Mean" = tableAM.cast$`Mean_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"   = tableAM.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"    = tableAM.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Mean"        = tableAM.cast$`Mean_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"          = tableAM.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"           = tableAM.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                   = tableAM.cast$EB_PSE
                            ,"PSE.King.County_EB"       = tableAM.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"   = tableAM.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"          = tableAM.cast$`EB_2017 RBSA PS`
)

tableAM.table <- tableAM.table[which(tableAM.table$Fixture.Type != "All Fixtures"),]

tableAM.final.MF <- tableAM.table[which(tableAM.table$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAM.table) %in% c("BuildingType"))]

exportTable(tableAM.final.MF, "MF", "Table AM", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# Unweighted Analysis
#######################
tableAM.cast <- mean_two_groups_unweighted(CustomerLevelData = tableAM.data
                                           ,valueVariable = "Site.Count"
                                           ,byVariableRow = "Fixture.Type"
                                           ,byVariableColumn = "Category"
                                           ,columnAggregate = "Remove"
                                           ,rowAggregate = "All Fixtures")

tableAM.table <- data.frame("BuildingType"    = tableAM.cast$BuildingType
                            ,"Fixture.Type"   = tableAM.cast$Fixture.Type
                            ,"PSE.Mean"                 = tableAM.cast$Mean_PSE
                            ,"PSE.SE"                   = tableAM.cast$SE_PSE
                            ,"PSE.n"                    = tableAM.cast$n_PSE
                            ,"PSE.King.County.Mean"     = tableAM.cast$`Mean_PSE KING COUNTY`
                            ,"PSE.King.County.SE"       = tableAM.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"        = tableAM.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Mean" = tableAM.cast$`Mean_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"   = tableAM.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"    = tableAM.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Mean"        = tableAM.cast$`Mean_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"          = tableAM.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"           = tableAM.cast$`n_2017 RBSA PS`
)

tableAM.table <- tableAM.table[which(tableAM.table$Fixture.Type != "All Fixtures"),]

tableAM.final.MF <- tableAM.table[which(tableAM.table$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAM.table) %in% c("BuildingType"))]

exportTable(tableAM.final.MF, "MF", "Table AM", weighted = FALSE,OS = T, osIndicator = "PSE")







#############################################################################################
# Table AS: DISTRIBUTION OF Bathroom Faucet FLOW RATE BY STATE
#############################################################################################
#subset to columns needed for analysis
tableAS.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
tableAS.dat$count <- 1

tableAS.dat0 <- tableAS.dat[which(tableAS.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableAS.dat1 <- left_join(tableAS.dat0, rbsa.dat, by = "CK_Cadmus_ID")

tableAS.dat1$GPM_Measured <- as.numeric(as.character(tableAS.dat1$GPM_Measured))
tableAS.dat2 <- tableAS.dat1[which(!(is.na(tableAS.dat1$GPM_Measured))),]
unique(tableAS.dat2$GPM_Measured)

tableAS.dat3 <- tableAS.dat2[grep("bathroom",tableAS.dat2$Fixture.Type, ignore.case = T),]

tableAS.dat4 <- summarise(group_by(tableAS.dat3, CK_Cadmus_ID)
                          ,GPM.Measured.Site = mean(GPM_Measured))

tableAS.dat4$GPM_bins <- tableAS.dat4$GPM.Measured.Site
tableAS.dat4$GPM_bins[which(tableAS.dat4$GPM.Measured.Site <  2.2)] <- "<= 2.2"
tableAS.dat4$GPM_bins[which(tableAS.dat4$GPM.Measured.Site >= 2.2)] <- "> 2.2"
unique(tableAS.dat4$GPM_bins)

tableAS.merge <- left_join(rbsa.dat, tableAS.dat4)
tableAS.merge <- tableAS.merge[which(!is.na(tableAS.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
tableAS.data <- weightedData(tableAS.merge[-which(colnames(tableAS.merge) %in% c("GPM.Measured.Site"               
                                                                                 ,"GPM_bins"
                                                                                 ,"count"
                                                                                 ,"Category"))])
tableAS.data <- left_join(tableAS.data, tableAS.merge[which(colnames(tableAS.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"
                                                                                           ,"Category"))])

tableAS.data$count <- 1
#######################
# Weighted Analysis
#######################
tableAS.final <- proportionRowsAndColumns1(CustomerLevelData = tableAS.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'Category'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Remove")

tableAS.cast <- dcast(setDT(tableAS.final)
                      , formula = BuildingType + GPM_bins ~ Category
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

tableAS.table <- data.frame("BuildingType"   = tableAS.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAS.cast$GPM_bins
                            ,"PSE.Percent"                 = tableAS.cast$w.percent_PSE
                            ,"PSE.SE"                      = tableAS.cast$w.SE_PSE
                            ,"PSE.n"                       = tableAS.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableAS.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableAS.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableAS.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableAS.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableAS.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableAS.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableAS.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableAS.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableAS.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = tableAS.cast$EB_PSE
                            ,"PSE.King.County_EB"          = tableAS.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = tableAS.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = tableAS.cast$`EB_2017 RBSA PS`
)

levels(tableAS.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
tableAS.table <- tableAS.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)
tableAS.table <- data.frame(tableAS.table)

tableAS.final.MF <- tableAS.table[which(tableAS.table$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAS.table) %in% c("BuildingType"))]
exportTable(tableAS.final.MF, "MF", "Table AS", weighted = TRUE,OS = T, osIndicator = "PSE")



#######################
# Unweighted Analysis
#######################
tableAS.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAS.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'GPM_bins'
                                                   ,aggregateColumnName = "Region")

tableAS.cast <- dcast(setDT(tableAS.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


tableAS.table <- data.frame("BuildingType"   = tableAS.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAS.cast$GPM_bins
                            ,"PSE.Percent"                 = tableAS.cast$Percent_PSE
                            ,"PSE.SE"                      = tableAS.cast$SE_PSE
                            ,"PSE.n"                       = tableAS.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableAS.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableAS.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableAS.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableAS.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableAS.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableAS.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableAS.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableAS.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableAS.cast$`n_2017 RBSA PS`
)

levels(tableAS.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
tableAS.table <- tableAS.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)
tableAS.table <- data.frame(tableAS.table)

tableAS.final.MF <- tableAS.table[which(tableAS.table$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAS.table) %in% c("BuildingType"))]
exportTable(tableAS.final.MF, "MF", "Table AS", weighted = FALSE,OS = T, osIndicator = "PSE")





#############################################################################################
# Table AT: DISTRIBUTION OF Kitchen Faucet FLOW RATE BY STATE
#############################################################################################
#subset to columns needed for analysis
tableAT.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
tableAT.dat$count <- 1

tableAT.dat0 <- tableAT.dat[which(tableAT.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableAT.dat1 <- left_join(tableAT.dat0, rbsa.dat, by = "CK_Cadmus_ID")

tableAT.dat1$GPM_Measured <- as.numeric(as.character(tableAT.dat1$GPM_Measured))
tableAT.dat2 <- tableAT.dat1[which(!(is.na(tableAT.dat1$GPM_Measured))),]
unique(tableAT.dat2$GPM_Measured)

tableAT.dat3 <- tableAT.dat2[grep("kitchen",tableAT.dat2$Fixture.Type, ignore.case = T),]

tableAT.dat4 <- summarise(group_by(tableAT.dat3, CK_Cadmus_ID)
                          ,GPM.Measured.Site = mean(GPM_Measured))

tableAT.dat4$GPM_bins <- tableAT.dat4$GPM.Measured.Site
tableAT.dat4$GPM_bins[which(tableAT.dat4$GPM.Measured.Site <  2.2)] <- "<= 2.2"
tableAT.dat4$GPM_bins[which(tableAT.dat4$GPM.Measured.Site >= 2.2)] <- "> 2.2"
unique(tableAT.dat4$GPM_bins)

tableAT.merge <- left_join(rbsa.dat, tableAT.dat4)
tableAT.merge <- tableAT.merge[which(!is.na(tableAT.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
tableAT.data <- weightedData(tableAT.merge[-which(colnames(tableAT.merge) %in% c("GPM.Measured.Site"               
                                                                                 ,"GPM_bins"
                                                                                 ,"count"
                                                                                 ,"Category"))])
tableAT.data <- left_join(tableAT.data, tableAT.merge[which(colnames(tableAT.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"
                                                                                           ,"Category"))])
tableAT.data$count <- 1
tableAT.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableAT.final <- proportionRowsAndColumns1(CustomerLevelData = tableAT.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'Category'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Remove")

tableAT.cast <- dcast(setDT(tableAT.final)
                      , formula = BuildingType + GPM_bins ~ Category
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

tableAT.table <- data.frame("BuildingType"   = tableAT.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAT.cast$GPM_bins
                            ,"PSE.Percent"                 = tableAT.cast$w.percent_PSE
                            ,"PSE.SE"                      = tableAT.cast$w.SE_PSE
                            ,"PSE.n"                       = tableAT.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableAT.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableAT.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableAT.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableAT.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableAT.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableAT.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableAT.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableAT.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableAT.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = tableAT.cast$EB_PSE
                            ,"PSE.King.County_EB"          = tableAT.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = tableAT.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = tableAT.cast$`EB_2017 RBSA PS`
)
levels(tableAT.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
tableAT.table <- tableAT.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)
tableAT.table <- data.frame(tableAT.table)

tableAT.final.MF <- tableAT.table[which(tableAT.table$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAT.table) %in% c("BuildingType"))]
exportTable(tableAT.final.MF, "MF", "Table AT", weighted = TRUE,OS = T, osIndicator = "PSE")



#######################
# Unweighted Analysis
#######################
tableAT.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAT.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'Category'
                                                   ,rowVariable      = 'GPM_bins'
                                                   ,aggregateColumnName = "Remove")

tableAT.cast <- dcast(setDT(tableAT.final)
                      , formula = BuildingType + GPM_bins ~ Category
                      , value.var = c("Percent", "SE", "Count", "n"))


tableAT.table <- data.frame("BuildingType"   = tableAT.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAT.cast$GPM_bins
                            ,"PSE.Percent"                 = tableAT.cast$Percent_PSE
                            ,"PSE.SE"                      = tableAT.cast$SE_PSE
                            ,"PSE.n"                       = tableAT.cast$n_PSE
                            ,"PSE.King.County.Percent"     = tableAT.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = tableAT.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = tableAT.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = tableAT.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = tableAT.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = tableAT.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = tableAT.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = tableAT.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = tableAT.cast$`n_2017 RBSA PS`
)

levels(tableAT.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
tableAT.table <- tableAT.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)
tableAT.table <- data.frame(tableAT.table)

tableAT.final.MF <- tableAT.table[which(tableAT.table$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAT.table) %in% c("BuildingType"))]
exportTable(tableAT.final.MF, "MF", "Table AT", weighted = FALSE,OS = T, osIndicator = "PSE")

