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
water.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, water.export))
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
# item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >= 2.6 & item106.dat4$GPM.Measured.Site < 3.6)] <- "2.6-3.5"
# item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >= 3.6)] <- "> 3.6"
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >= 2.6)] <- "> 2.5"
unique(item106.dat4$GPM_bins)

item106.merge <- left_join(rbsa.dat, item106.dat4)
item106.merge <- item106.merge[which(!is.na(item106.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
item106.data <- weightedData(item106.merge[-which(colnames(item106.merge) %in% c("GPM.Measured.Site"               
                                                                            ,"GPM_bins"
                                                                            ,"count"))])
item106.data <- left_join(item106.data, item106.merge[which(colnames(item106.merge) %in% c("CK_Cadmus_ID"
                                                                                     ,"GPM.Measured.Site"               
                                                                                     ,"GPM_bins"
                                                                                     ,"count"))])
#######################
# Weighted Analysis
#######################
item106.final <- proportionRowsAndColumns1(CustomerLevelData = item106.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'State'
                                          ,rowVariable      = 'GPM_bins'
                                          ,aggregateColumnName = "Region")

item106.cast <- dcast(setDT(item106.final)
                     , formula = BuildingType + GPM_bins ~ State
                     , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item106.table <- data.frame("BuildingType"   = item106.cast$BuildingType
                           ,"Flow.Rate.GPM"  = item106.cast$GPM_bins
                           ,"Percent_ID"     = item106.cast$w.percent_ID
                           ,"SE_ID"          = item106.cast$w.SE_ID
                           ,"n_ID"           = item106.cast$n_ID
                           ,"Percent_MT"     = item106.cast$w.percent_MT
                           ,"SE_MT"          = item106.cast$w.SE_MT
                           ,"n_MT"           = item106.cast$n_MT
                           ,"Percent_OR"     = item106.cast$w.percent_OR
                           ,"SE_OR"          = item106.cast$w.SE_OR
                           ,"n_OR"           = item106.cast$n_OR
                           ,"Percent_WA"     = item106.cast$w.percent_WA
                           ,"SE_WA"          = item106.cast$w.SE_WA
                           ,"n_WA"           = item106.cast$n_WA
                           ,"Percent_Region" = item106.cast$w.percent_Region
                           ,"SE_Region"      = item106.cast$w.SE_Region
                           ,"n_Region"       = item106.cast$n_Region
                           ,"EB_ID"          = item106.cast$EB_ID
                           ,"EB_MT"          = item106.cast$EB_MT
                           ,"EB_OR"          = item106.cast$EB_OR
                           ,"EB_WA"          = item106.cast$EB_WA
                           ,"EB_Region"      = item106.cast$EB_Region
) 
#QAQC
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

item106.final.SF <- item106.table[which(item106.table$BuildingType == "Single Family")
                                ,-which(colnames(item106.table) %in% c("BuildingType"))]
item106.final.MH <- item106.table[which(item106.table$BuildingType == "Manufactured")
                                ,-which(colnames(item106.table) %in% c("BuildingType"))]
item106.final.MF <- item106.table[which(item106.table$BuildingType == "Multifamily")
                                  ,-which(colnames(item106.table) %in% c("BuildingType"))]

# exportTable(item106.final.SF, "SF", "Table 113", weighted = TRUE)
exportTable(item106.final.MH, "MH", "Table 88", weighted = TRUE)
exportTable(item106.final.MF, "MF", "Table 80", weighted = TRUE)


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


item106.table <- data.frame("BuildingType"   = item106.cast$BuildingType
                           ,"Flow.Rate.GPM"  = item106.cast$GPM_bins
                           ,"Percent_ID"     = item106.cast$Percent_ID
                           ,"SE_ID"          = item106.cast$SE_ID
                           ,"n_ID"           = item106.cast$n_ID
                           ,"Percent_MT"     = item106.cast$Percent_MT
                           ,"SE_MT"          = item106.cast$SE_MT
                           ,"n_MT"           = item106.cast$n_MT
                           ,"Percent_OR"     = item106.cast$Percent_OR
                           ,"SE_OR"          = item106.cast$SE_OR
                           ,"n_OR"           = item106.cast$n_OR
                           ,"Percent_WA"     = item106.cast$Percent_WA
                           ,"SE_WA"          = item106.cast$SE_WA
                           ,"n_WA"           = item106.cast$n_WA
                           ,"Percent_Region" = item106.cast$Percent_Region
                           ,"SE_Region"      = item106.cast$SE_Region
                           ,"n_Region"       = item106.cast$n_Region
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

item106.final.SF <- item106.table[which(item106.table$BuildingType == "Single Family")
                                ,-which(colnames(item106.table) %in% c("BuildingType"))]
item106.final.MH <- item106.table[which(item106.table$BuildingType == "Manufactured")
                                ,-which(colnames(item106.table) %in% c("BuildingType"))]
item106.final.MF <- item106.table[which(item106.table$BuildingType == "Multifamily")
                                  ,-which(colnames(item106.table) %in% c("BuildingType"))]

# exportTable(item106.final.SF, "SF", "Table 113", weighted = FALSE)
exportTable(item106.final.MH, "MH", "Table 88", weighted = FALSE)
exportTable(item106.final.MF, "MF", "Table 80", weighted = FALSE)










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
                                                                                 ,"Ind"))])
tableFF.data <- left_join(tableFF.data, tableFF.merge[which(colnames(tableFF.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"
                                                                                           ,"Ind"))])
tableFF.data$count <- 1
tableFF.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableFF.table <- proportions_one_group(CustomerLevelData = tableFF.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = TRUE)

tableFF.final.SF <- tableFF.table[which(tableFF.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableFF.table) %in% c("BuildingType"))]
tableFF.final.MH <- tableFF.table[which(tableFF.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableFF.table) %in% c("BuildingType"))]

# exportTable(tableFF.final.SF, "SF", "Table FF", weighted = TRUE)
exportTable(tableFF.final.MH, "MH", "Table FF", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableFF.table <- proportions_one_group(CustomerLevelData = tableFF.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = FALSE)

tableFF.final.SF <- tableFF.table[which(tableFF.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableFF.table) %in% c("BuildingType"))]
tableFF.final.MH <- tableFF.table[which(tableFF.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableFF.table) %in% c("BuildingType"))]

# exportTable(tableFF.final.SF, "SF", "Table FF", weighted = FALSE)
exportTable(tableFF.final.MH, "MH", "Table FF", weighted = FALSE)










#############################################################################################
#Table AN: DISTRIBUTION OF KITCHEN FAUCET FLOW RATE BY STATE (SF table 113, MH table 88)
#############################################################################################
#subset to columns needed for analysis
tableAN.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
tableAN.dat$count <- 1

tableAN.dat0 <- tableAN.dat[which(tableAN.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableAN.dat1 <- left_join(tableAN.dat0, rbsa.dat, by = "CK_Cadmus_ID")

tableAN.dat1$GPM_Measured <- as.numeric(as.character(tableAN.dat1$GPM_Measured))
tableAN.dat2 <- tableAN.dat1[which(!(is.na(tableAN.dat1$GPM_Measured))),]
unique(tableAN.dat2$GPM_Measured)

tableAN.dat3 <- tableAN.dat2[grep("kitchen",tableAN.dat2$Fixture.Type,ignore.case = T),]

tableAN.dat4 <- summarise(group_by(tableAN.dat3, CK_Cadmus_ID)
                          ,GPM.Measured.Site = mean(GPM_Measured))

tableAN.dat4$GPM_bins <- tableAN.dat4$GPM.Measured.Site
tableAN.dat4$GPM_bins[which(tableAN.dat4$GPM.Measured.Site <= 1.5)] <- "< 1.5"
tableAN.dat4$GPM_bins[which(tableAN.dat4$GPM.Measured.Site >  1.5 & tableAN.dat4$GPM.Measured.Site < 2.1)] <- "1.6-2.0"
tableAN.dat4$GPM_bins[which(tableAN.dat4$GPM.Measured.Site >= 2.1 & tableAN.dat4$GPM.Measured.Site < 2.6)] <- "2.1-2.5"
tableAN.dat4$GPM_bins[which(tableAN.dat4$GPM.Measured.Site >= 2.6 & tableAN.dat4$GPM.Measured.Site < 3.6)] <- "2.6-3.5"
tableAN.dat4$GPM_bins[which(tableAN.dat4$GPM.Measured.Site >= 3.6)] <- "> 3.6"
unique(tableAN.dat4$GPM_bins)

tableAN.merge <- left_join(rbsa.dat, tableAN.dat4)
tableAN.merge <- tableAN.merge[which(!is.na(tableAN.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
tableAN.data <- weightedData(tableAN.merge[-which(colnames(tableAN.merge) %in% c("GPM.Measured.Site"               
                                                                                 ,"GPM_bins"
                                                                                 ,"count"))])
tableAN.data <- left_join(tableAN.data, tableAN.merge[which(colnames(tableAN.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"))])
tableAN.data$count <- 1
#######################
# Weighted Analysis
#######################
tableAN.final <- proportionRowsAndColumns1(CustomerLevelData = tableAN.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Region")

tableAN.cast <- dcast(setDT(tableAN.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

tableAN.table <- data.frame("BuildingType"   = tableAN.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAN.cast$GPM_bins
                            ,"Percent_ID"     = tableAN.cast$w.percent_ID
                            ,"SE_ID"          = tableAN.cast$w.SE_ID
                            ,"n_ID"           = tableAN.cast$n_ID
                            ,"Percent_MT"     = tableAN.cast$w.percent_MT
                            ,"SE_MT"          = tableAN.cast$w.SE_MT
                            ,"n_MT"           = tableAN.cast$n_MT
                            ,"Percent_OR"     = tableAN.cast$w.percent_OR
                            ,"SE_OR"          = tableAN.cast$w.SE_OR
                            ,"n_OR"           = tableAN.cast$n_OR
                            ,"Percent_WA"     = tableAN.cast$w.percent_WA
                            ,"SE_WA"          = tableAN.cast$w.SE_WA
                            ,"n_WA"           = tableAN.cast$n_WA
                            ,"Percent_Region" = tableAN.cast$w.percent_Region
                            ,"SE_Region"      = tableAN.cast$w.SE_Region
                            ,"n_Region"       = tableAN.cast$n_Region
                            ,"EB_ID"          = tableAN.cast$EB_ID
                            ,"EB_MT"          = tableAN.cast$EB_MT
                            ,"EB_OR"          = tableAN.cast$EB_OR
                            ,"EB_WA"          = tableAN.cast$EB_WA
                            ,"EB_Region"      = tableAN.cast$EB_Region
) 
#QAQC
stopifnot(sum(tableAN.table[which(tableAN.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(tableAN.table))], na.rm = T) == 10)

levels(tableAN.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
tableAN.table <- tableAN.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAN.table <- data.frame(tableAN.table)

tableAN.final.SF <- tableAN.table[which(tableAN.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAN.table) %in% c("BuildingType"))]
tableAN.final.MH <- tableAN.table[which(tableAN.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAN.table) %in% c("BuildingType"))]
tableAN.final.MF <- tableAN.table[which(tableAN.table$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAN.table) %in% c("BuildingType"))]

# exportTable(tableAN.final.SF, "SF", "Table AN", weighted = TRUE)
exportTable(tableAN.final.MH, "MH", "Table AN", weighted = TRUE)
# exportTable(tableAN.final.MF, "MF", "Table AN", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableAN.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAN.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'GPM_bins'
                                                   ,aggregateColumnName = "Region")

tableAN.cast <- dcast(setDT(tableAN.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


tableAN.table <- data.frame("BuildingType"   = tableAN.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAN.cast$GPM_bins
                            ,"Percent_ID"     = tableAN.cast$Percent_ID
                            ,"SE_ID"          = tableAN.cast$SE_ID
                            ,"n_ID"           = tableAN.cast$n_ID
                            ,"Percent_MT"     = tableAN.cast$Percent_MT
                            ,"SE_MT"          = tableAN.cast$SE_MT
                            ,"n_MT"           = tableAN.cast$n_MT
                            ,"Percent_OR"     = tableAN.cast$Percent_OR
                            ,"SE_OR"          = tableAN.cast$SE_OR
                            ,"n_OR"           = tableAN.cast$n_OR
                            ,"Percent_WA"     = tableAN.cast$Percent_WA
                            ,"SE_WA"          = tableAN.cast$SE_WA
                            ,"n_WA"           = tableAN.cast$n_WA
                            ,"Percent_Region" = tableAN.cast$Percent_Region
                            ,"SE_Region"      = tableAN.cast$SE_Region
                            ,"n_Region"       = tableAN.cast$n_Region
)
stopifnot(sum(tableAN.table[which(tableAN.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(tableAN.table))], na.rm = T) == 10)

levels(tableAN.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
tableAN.table <- tableAN.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAN.table <- data.frame(tableAN.table)

tableAN.final.SF <- tableAN.table[which(tableAN.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAN.table) %in% c("BuildingType"))]
tableAN.final.MH <- tableAN.table[which(tableAN.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAN.table) %in% c("BuildingType"))]
tableAN.final.MF <- tableAN.table[which(tableAN.table$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAN.table) %in% c("BuildingType"))]

# exportTable(tableAN.final.SF, "SF", "Table AN", weighted = FALSE)
exportTable(tableAN.final.MH, "MH", "Table AN", weighted = FALSE)
# exportTable(tableAN.final.MF, "MF", "Table AN", weighted = FALSE)





#############################################################################################
#Table AO: DISTRIBUTION OF BATHROOM FAUCET FLOW RATE BY STATE (SF table 113, MH table 88)
#############################################################################################
#subset to columns needed for analysis
tableAO.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
tableAO.dat$count <- 1

tableAO.dat0 <- tableAO.dat[which(tableAO.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableAO.dat1 <- left_join(tableAO.dat0, rbsa.dat, by = "CK_Cadmus_ID")

tableAO.dat1$GPM_Measured <- as.numeric(as.character(tableAO.dat1$GPM_Measured))
tableAO.dat2 <- tableAO.dat1[which(!(is.na(tableAO.dat1$GPM_Measured))),]
unique(tableAO.dat2$GPM_Measured)

tableAO.dat3 <- tableAO.dat2[grep("bathroom",tableAO.dat2$Fixture.Type,ignore.case = T),]

tableAO.dat4 <- summarise(group_by(tableAO.dat3, CK_Cadmus_ID)
                          ,GPM.Measured.Site = mean(GPM_Measured))

tableAO.dat4$GPM_bins <- tableAO.dat4$GPM.Measured.Site
tableAO.dat4$GPM_bins[which(tableAO.dat4$GPM.Measured.Site <= 1.5)] <- "< 1.5"
tableAO.dat4$GPM_bins[which(tableAO.dat4$GPM.Measured.Site >  1.5 & tableAO.dat4$GPM.Measured.Site < 2.1)] <- "1.6-2.0"
tableAO.dat4$GPM_bins[which(tableAO.dat4$GPM.Measured.Site >= 2.1 & tableAO.dat4$GPM.Measured.Site < 2.6)] <- "2.1-2.5"
tableAO.dat4$GPM_bins[which(tableAO.dat4$GPM.Measured.Site >= 2.6 & tableAO.dat4$GPM.Measured.Site < 3.6)] <- "2.6-3.5"
tableAO.dat4$GPM_bins[which(tableAO.dat4$GPM.Measured.Site >= 3.6)] <- "> 3.6"
unique(tableAO.dat4$GPM_bins)

tableAO.merge <- left_join(rbsa.dat, tableAO.dat4)
tableAO.merge <- tableAO.merge[which(!is.na(tableAO.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
tableAO.data <- weightedData(tableAO.merge[-which(colnames(tableAO.merge) %in% c("GPM.Measured.Site"               
                                                                                 ,"GPM_bins"
                                                                                 ,"count"))])
tableAO.data <- left_join(tableAO.data, tableAO.merge[which(colnames(tableAO.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"))])
tableAO.data$count <- 1
#######################
# Weighted Analysis
#######################
tableAO.final <- proportionRowsAndColumns1(CustomerLevelData = tableAO.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Region")

tableAO.cast <- dcast(setDT(tableAO.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

tableAO.table <- data.frame("BuildingType"   = tableAO.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAO.cast$GPM_bins
                            ,"Percent_ID"     = tableAO.cast$w.percent_ID
                            ,"SE_ID"          = tableAO.cast$w.SE_ID
                            ,"n_ID"           = tableAO.cast$n_ID
                            ,"Percent_MT"     = tableAO.cast$w.percent_MT
                            ,"SE_MT"          = tableAO.cast$w.SE_MT
                            ,"n_MT"           = tableAO.cast$n_MT
                            ,"Percent_OR"     = tableAO.cast$w.percent_OR
                            ,"SE_OR"          = tableAO.cast$w.SE_OR
                            ,"n_OR"           = tableAO.cast$n_OR
                            ,"Percent_WA"     = tableAO.cast$w.percent_WA
                            ,"SE_WA"          = tableAO.cast$w.SE_WA
                            ,"n_WA"           = tableAO.cast$n_WA
                            ,"Percent_Region" = tableAO.cast$w.percent_Region
                            ,"SE_Region"      = tableAO.cast$w.SE_Region
                            ,"n_Region"       = tableAO.cast$n_Region
                            ,"EB_ID"          = tableAO.cast$EB_ID
                            ,"EB_MT"          = tableAO.cast$EB_MT
                            ,"EB_OR"          = tableAO.cast$EB_OR
                            ,"EB_WA"          = tableAO.cast$EB_WA
                            ,"EB_Region"      = tableAO.cast$EB_Region
) 
#QAQC
stopifnot(sum(tableAO.table[which(tableAO.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(tableAO.table))], na.rm = T) == 10)

levels(tableAO.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
tableAO.table <- tableAO.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAO.table <- data.frame(tableAO.table)

tableAO.final.SF <- tableAO.table[which(tableAO.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAO.table) %in% c("BuildingType"))]
tableAO.final.MH <- tableAO.table[which(tableAO.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAO.table) %in% c("BuildingType"))]
tableAO.final.MF <- tableAO.table[which(tableAO.table$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAO.table) %in% c("BuildingType"))]

# exportTable(tableAO.final.SF, "SF", "Table AO", weighted = TRUE)
exportTable(tableAO.final.MH, "MH", "Table AO", weighted = TRUE)
# exportTable(tableAO.final.MF, "MF", "Table AO", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableAO.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAO.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'GPM_bins'
                                                   ,aggregateColumnName = "Region")

tableAO.cast <- dcast(setDT(tableAO.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


tableAO.table <- data.frame("BuildingType"   = tableAO.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAO.cast$GPM_bins
                            ,"Percent_ID"     = tableAO.cast$Percent_ID
                            ,"SE_ID"          = tableAO.cast$SE_ID
                            ,"n_ID"           = tableAO.cast$n_ID
                            ,"Percent_MT"     = tableAO.cast$Percent_MT
                            ,"SE_MT"          = tableAO.cast$SE_MT
                            ,"n_MT"           = tableAO.cast$n_MT
                            ,"Percent_OR"     = tableAO.cast$Percent_OR
                            ,"SE_OR"          = tableAO.cast$SE_OR
                            ,"n_OR"           = tableAO.cast$n_OR
                            ,"Percent_WA"     = tableAO.cast$Percent_WA
                            ,"SE_WA"          = tableAO.cast$SE_WA
                            ,"n_WA"           = tableAO.cast$n_WA
                            ,"Percent_Region" = tableAO.cast$Percent_Region
                            ,"SE_Region"      = tableAO.cast$SE_Region
                            ,"n_Region"       = tableAO.cast$n_Region
)
stopifnot(sum(tableAO.table[which(tableAO.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(tableAO.table))], na.rm = T) == 10)

levels(tableAO.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
tableAO.table <- tableAO.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAO.table <- data.frame(tableAO.table)

tableAO.final.SF <- tableAO.table[which(tableAO.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAO.table) %in% c("BuildingType"))]
tableAO.final.MH <- tableAO.table[which(tableAO.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAO.table) %in% c("BuildingType"))]
tableAO.final.MF <- tableAO.table[which(tableAO.table$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAO.table) %in% c("BuildingType"))]

# exportTable(tableAO.final.SF, "SF", "Table AO", weighted = FALSE)
exportTable(tableAO.final.MH, "MH", "Table AO", weighted = FALSE)
# exportTable(tableAO.final.MF, "MF", "Table AO", weighted = FALSE)






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

################################################
# Adding pop and sample sizes for weights
################################################
tableAM.data <- weightedData(tableAM.merge[-which(colnames(tableAM.merge) %in% c("Site.Count"
                                                                                 ,"Fixture.Type"))])
tableAM.data <- left_join(tableAM.data, tableAM.merge[which(colnames(tableAM.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Site.Count"
                                                                                           ,"Fixture.Type"))])
tableAM.data$count <- 1
#######################
# Weighted Analysis
#######################
tableAM.cast <- mean_two_groups(CustomerLevelData = tableAM.data
                                 ,valueVariable = "Site.Count"
                                 ,byVariableRow = "Fixture.Type"
                                 ,byVariableColumn = "State"
                                 ,columnAggregate = "Region"
                                 ,rowAggregate = "All Fixtures")

tableAM.table <- data.frame("BuildingType"    = tableAM.cast$BuildingType
                            ,"Fixture.Type"   = tableAM.cast$Fixture.Type
                            ,"Mean_ID"        = tableAM.cast$Mean_ID
                            ,"SE_ID"          = tableAM.cast$SE_ID
                            ,"n_ID"           = tableAM.cast$n_ID
                            ,"Mean_MT"        = tableAM.cast$Mean_MT
                            ,"SE_MT"          = tableAM.cast$SE_MT
                            ,"n_MT"           = tableAM.cast$n_MT
                            ,"Mean_OR"        = tableAM.cast$Mean_OR
                            ,"SE_OR"          = tableAM.cast$SE_OR
                            ,"n_OR"           = tableAM.cast$n_OR
                            ,"Mean_WA"        = tableAM.cast$Mean_WA
                            ,"SE_WA"          = tableAM.cast$SE_WA
                            ,"n_WA"           = tableAM.cast$n_WA
                            ,"Mean_Region"    = tableAM.cast$Mean_Region
                            ,"SE_Region"      = tableAM.cast$SE_Region
                            ,"n_Region"       = tableAM.cast$n_Region
                            ,"EB_ID"          = tableAM.cast$EB_ID
                            ,"EB_MT"          = tableAM.cast$EB_MT
                            ,"EB_OR"          = tableAM.cast$EB_OR
                            ,"EB_WA"          = tableAM.cast$EB_WA
                            ,"EB_Region"      = tableAM.cast$EB_Region
) 

tableAM.table <- tableAM.table[which(tableAM.table$Fixture.Type != "All Fixtures"),]

tableAM.final.SF <- tableAM.table[which(tableAM.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAM.table) %in% c("BuildingType"))]
tableAM.final.MH <- tableAM.table[which(tableAM.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAM.table) %in% c("BuildingType"))]
tableAM.final.MF <- tableAM.table[which(tableAM.table$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAM.table) %in% c("BuildingType"))]

# exportTable(tableAM.final.SF, "SF", "Table AM", weighted = TRUE)
exportTable(tableAM.final.MH, "MH", "Table AM", weighted = TRUE)
# exportTable(tableAM.final.MF, "MF", "Table AM", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableAM.cast <- mean_two_groups_unweighted(CustomerLevelData = tableAM.data
                                           ,valueVariable = "Site.Count"
                                           ,byVariableRow = "Fixture.Type"
                                           ,byVariableColumn = "State"
                                           ,columnAggregate = "Region"
                                           ,rowAggregate = "All Fixtures")

tableAM.table <- data.frame("BuildingType"    = tableAM.cast$BuildingType
                            ,"Fixture.Type"   = tableAM.cast$Fixture.Type
                            ,"Mean_ID"        = tableAM.cast$Mean_ID
                            ,"SE_ID"          = tableAM.cast$SE_ID
                            ,"n_ID"           = tableAM.cast$n_ID
                            ,"Mean_MT"        = tableAM.cast$Mean_MT
                            ,"SE_MT"          = tableAM.cast$SE_MT
                            ,"n_MT"           = tableAM.cast$n_MT
                            ,"Mean_OR"        = tableAM.cast$Mean_OR
                            ,"SE_OR"          = tableAM.cast$SE_OR
                            ,"n_OR"           = tableAM.cast$n_OR
                            ,"Mean_WA"        = tableAM.cast$Mean_WA
                            ,"SE_WA"          = tableAM.cast$SE_WA
                            ,"n_WA"           = tableAM.cast$n_WA
                            ,"Mean_Region"    = tableAM.cast$Mean_Region
                            ,"SE_Region"      = tableAM.cast$SE_Region
                            ,"n_Region"       = tableAM.cast$n_Region
)

tableAM.table <- tableAM.table[which(tableAM.table$Fixture.Type != "All Fixtures"),]

tableAM.final.SF <- tableAM.table[which(tableAM.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAM.table) %in% c("BuildingType"))]
tableAM.final.MH <- tableAM.table[which(tableAM.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAM.table) %in% c("BuildingType"))]
tableAM.final.MF <- tableAM.table[which(tableAM.table$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAM.table) %in% c("BuildingType"))]

# exportTable(tableAM.final.SF, "SF", "Table AM", weighted = FALSE)
exportTable(tableAM.final.MH, "MH", "Table AM", weighted = FALSE)
# exportTable(tableAM.final.MF, "MF", "Table AM", weighted = FALSE)







#############################################################################################
# Table AR: DISTRIBUTION OF SHOWERHEAD FLOW RATE BY STATE (new bins)
#############################################################################################
#subset to columns needed for analysis
TableAR.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
TableAR.dat$count <- 1

TableAR.dat0 <- TableAR.dat[which(TableAR.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

TableAR.dat1 <- left_join(TableAR.dat0, rbsa.dat, by = "CK_Cadmus_ID")

TableAR.dat1$GPM_Measured <- as.numeric(as.character(TableAR.dat1$GPM_Measured))
TableAR.dat2 <- TableAR.dat1[which(!(is.na(TableAR.dat1$GPM_Measured))),]
unique(TableAR.dat2$GPM_Measured)

TableAR.dat3 <- TableAR.dat2[grep("shower|Shower",TableAR.dat2$Fixture.Type),]

TableAR.dat4 <- summarise(group_by(TableAR.dat3, CK_Cadmus_ID)
                          ,GPM.Measured.Site = mean(GPM_Measured))

TableAR.dat4$GPM_bins <- TableAR.dat4$GPM.Measured.Site
TableAR.dat4$GPM_bins[which(TableAR.dat4$GPM.Measured.Site <  2.5)] <- "< 2.5"
TableAR.dat4$GPM_bins[which(TableAR.dat4$GPM.Measured.Site >= 2.5)] <- ">= 2.5"
unique(TableAR.dat4$GPM_bins)

TableAR.merge <- left_join(rbsa.dat, TableAR.dat4)
TableAR.merge <- TableAR.merge[which(!is.na(TableAR.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
TableAR.data <- weightedData(TableAR.merge[-which(colnames(TableAR.merge) %in% c("GPM.Measured.Site"               
                                                                                 ,"GPM_bins"
                                                                                 ,"count"))])
TableAR.data <- left_join(TableAR.data, TableAR.merge[which(colnames(TableAR.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"))])
#######################
# Weighted Analysis
#######################
TableAR.data$count <- 1
TableAR.final <- proportionRowsAndColumns1(CustomerLevelData = TableAR.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Region")

TableAR.cast <- dcast(setDT(TableAR.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

TableAR.table <- data.frame("BuildingType"   = TableAR.cast$BuildingType
                            ,"Flow.Rate.GPM"  = TableAR.cast$GPM_bins
                            ,"Percent_ID"     = TableAR.cast$w.percent_ID
                            ,"SE_ID"          = TableAR.cast$w.SE_ID
                            ,"n_ID"           = TableAR.cast$n_ID
                            ,"Percent_MT"     = TableAR.cast$w.percent_MT
                            ,"SE_MT"          = TableAR.cast$w.SE_MT
                            ,"n_MT"           = TableAR.cast$n_MT
                            ,"Percent_OR"     = TableAR.cast$w.percent_OR
                            ,"SE_OR"          = TableAR.cast$w.SE_OR
                            ,"n_OR"           = TableAR.cast$n_OR
                            ,"Percent_WA"     = TableAR.cast$w.percent_WA
                            ,"SE_WA"          = TableAR.cast$w.SE_WA
                            ,"n_WA"           = TableAR.cast$n_WA
                            ,"Percent_Region" = TableAR.cast$w.percent_Region
                            ,"SE_Region"      = TableAR.cast$w.SE_Region
                            ,"n_Region"       = TableAR.cast$n_Region
                            ,"EB_ID"          = TableAR.cast$EB_ID
                            ,"EB_MT"          = TableAR.cast$EB_MT
                            ,"EB_OR"          = TableAR.cast$EB_OR
                            ,"EB_WA"          = TableAR.cast$EB_WA
                            ,"EB_Region"      = TableAR.cast$EB_Region
) 
#QAQC
stopifnot(sum(TableAR.table[which(TableAR.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(TableAR.table))], na.rm = T) == 10)

levels(TableAR.table$Flow.Rate.GPM)
rowOrder <- c("< 2.5"
              ,">= 2.5"
              ,"Total")
TableAR.table <- TableAR.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
TableAR.table <- data.frame(TableAR.table)

TableAR.final.SF <- TableAR.table[which(TableAR.table$BuildingType == "Single Family")
                                  ,-which(colnames(TableAR.table) %in% c("BuildingType"))]
TableAR.final.MH <- TableAR.table[which(TableAR.table$BuildingType == "Manufactured")
                                  ,-which(colnames(TableAR.table) %in% c("BuildingType"))]

# exportTable(TableAR.final.SF, "SF", "Table AR", weighted = TRUE)
exportTable(TableAR.final.MH, "MH", "Table AR", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
TableAR.final <- proportions_two_groups_unweighted(CustomerLevelData = TableAR.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'GPM_bins'
                                                   ,aggregateColumnName = "Region")

TableAR.cast <- dcast(setDT(TableAR.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


TableAR.table <- data.frame("BuildingType"   = TableAR.cast$BuildingType
                            ,"Flow.Rate.GPM"  = TableAR.cast$GPM_bins
                            ,"Percent_ID"     = TableAR.cast$Percent_ID
                            ,"SE_ID"          = TableAR.cast$SE_ID
                            ,"n_ID"           = TableAR.cast$n_ID
                            ,"Percent_MT"     = TableAR.cast$Percent_MT
                            ,"SE_MT"          = TableAR.cast$SE_MT
                            ,"n_MT"           = TableAR.cast$n_MT
                            ,"Percent_OR"     = TableAR.cast$Percent_OR
                            ,"SE_OR"          = TableAR.cast$SE_OR
                            ,"n_OR"           = TableAR.cast$n_OR
                            ,"Percent_WA"     = TableAR.cast$Percent_WA
                            ,"SE_WA"          = TableAR.cast$SE_WA
                            ,"n_WA"           = TableAR.cast$n_WA
                            ,"Percent_Region" = TableAR.cast$Percent_Region
                            ,"SE_Region"      = TableAR.cast$SE_Region
                            ,"n_Region"       = TableAR.cast$n_Region
)
stopifnot(sum(TableAR.table[which(TableAR.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(TableAR.table))], na.rm = T) == 10)

levels(TableAR.table$Flow.Rate.GPM)
rowOrder <- c("< 2.5"
              ,">= 2.5"
              ,"Total")
TableAR.table <- TableAR.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
TableAR.table <- data.frame(TableAR.table)

TableAR.final.SF <- TableAR.table[which(TableAR.table$BuildingType == "Single Family")
                                  ,-which(colnames(TableAR.table) %in% c("BuildingType"))]
TableAR.final.MH <- TableAR.table[which(TableAR.table$BuildingType == "Manufactured")
                                  ,-which(colnames(TableAR.table) %in% c("BuildingType"))]

# exportTable(TableAR.final.SF, "SF", "Table AR", weighted = FALSE)
exportTable(TableAR.final.MH, "MH", "Table AR", weighted = FALSE)






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
                                                                                 ,"count"))])
tableAS.data <- left_join(tableAS.data, tableAS.merge[which(colnames(tableAS.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"))])
#######################
# Weighted Analysis
#######################
tableAS.data$count <- 1
tableAS.final <- proportionRowsAndColumns1(CustomerLevelData = tableAS.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Region")

tableAS.cast <- dcast(setDT(tableAS.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

tableAS.table <- data.frame("BuildingType"   = tableAS.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAS.cast$GPM_bins
                            ,"Percent_ID"     = tableAS.cast$w.percent_ID
                            ,"SE_ID"          = tableAS.cast$w.SE_ID
                            ,"n_ID"           = tableAS.cast$n_ID
                            ,"Percent_MT"     = tableAS.cast$w.percent_MT
                            ,"SE_MT"          = tableAS.cast$w.SE_MT
                            ,"n_MT"           = tableAS.cast$n_MT
                            ,"Percent_OR"     = tableAS.cast$w.percent_OR
                            ,"SE_OR"          = tableAS.cast$w.SE_OR
                            ,"n_OR"           = tableAS.cast$n_OR
                            ,"Percent_WA"     = tableAS.cast$w.percent_WA
                            ,"SE_WA"          = tableAS.cast$w.SE_WA
                            ,"n_WA"           = tableAS.cast$n_WA
                            ,"Percent_Region" = tableAS.cast$w.percent_Region
                            ,"SE_Region"      = tableAS.cast$w.SE_Region
                            ,"n_Region"       = tableAS.cast$n_Region
                            ,"EB_ID"          = tableAS.cast$EB_ID
                            ,"EB_MT"          = tableAS.cast$EB_MT
                            ,"EB_OR"          = tableAS.cast$EB_OR
                            ,"EB_WA"          = tableAS.cast$EB_WA
                            ,"EB_Region"      = tableAS.cast$EB_Region
) 
#QAQC
stopifnot(sum(tableAS.table[which(tableAS.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(tableAS.table))], na.rm = T) == 10)

levels(tableAS.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
tableAS.table <- tableAS.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAS.table <- data.frame(tableAS.table)

tableAS.final.SF <- tableAS.table[which(tableAS.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAS.table) %in% c("BuildingType"))]
tableAS.final.MH <- tableAS.table[which(tableAS.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAS.table) %in% c("BuildingType"))]

# exportTable(tableAS.final.SF, "SF", "Table AS", weighted = TRUE)
exportTable(tableAS.final.MH, "MH", "Table AS", weighted = TRUE)


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
                            ,"Percent_ID"     = tableAS.cast$Percent_ID
                            ,"SE_ID"          = tableAS.cast$SE_ID
                            ,"n_ID"           = tableAS.cast$n_ID
                            ,"Percent_MT"     = tableAS.cast$Percent_MT
                            ,"SE_MT"          = tableAS.cast$SE_MT
                            ,"n_MT"           = tableAS.cast$n_MT
                            ,"Percent_OR"     = tableAS.cast$Percent_OR
                            ,"SE_OR"          = tableAS.cast$SE_OR
                            ,"n_OR"           = tableAS.cast$n_OR
                            ,"Percent_WA"     = tableAS.cast$Percent_WA
                            ,"SE_WA"          = tableAS.cast$SE_WA
                            ,"n_WA"           = tableAS.cast$n_WA
                            ,"Percent_Region" = tableAS.cast$Percent_Region
                            ,"SE_Region"      = tableAS.cast$SE_Region
                            ,"n_Region"       = tableAS.cast$n_Region
)
stopifnot(sum(tableAS.table[which(tableAS.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(tableAS.table))], na.rm = T) == 10)

levels(tableAS.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
tableAS.table <- tableAS.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAS.table <- data.frame(tableAS.table)

tableAS.final.SF <- tableAS.table[which(tableAS.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAS.table) %in% c("BuildingType"))]
tableAS.final.MH <- tableAS.table[which(tableAS.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAS.table) %in% c("BuildingType"))]

# exportTable(tableAS.final.SF, "SF", "Table AS", weighted = FALSE)
exportTable(tableAS.final.MH, "MH", "Table AS", weighted = FALSE)







#############################################################################################
# Table AT: DISTRIBUTION OF Kitchen Faucet FLOW RATE BY STATE
#############################################################################################
#subset to columns needed for analysis
TableAT.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
TableAT.dat$count <- 1

TableAT.dat0 <- TableAT.dat[which(TableAT.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

TableAT.dat1 <- left_join(TableAT.dat0, rbsa.dat, by = "CK_Cadmus_ID")

TableAT.dat1$GPM_Measured <- as.numeric(as.character(TableAT.dat1$GPM_Measured))
TableAT.dat2 <- TableAT.dat1[which(!(is.na(TableAT.dat1$GPM_Measured))),]
unique(TableAT.dat2$GPM_Measured)

TableAT.dat3 <- TableAT.dat2[grep("kitchen",TableAT.dat2$Fixture.Type, ignore.case = T),]

TableAT.dat4 <- summarise(group_by(TableAT.dat3, CK_Cadmus_ID)
                          ,GPM.Measured.Site = mean(GPM_Measured))

TableAT.dat4$GPM_bins <- TableAT.dat4$GPM.Measured.Site
TableAT.dat4$GPM_bins[which(TableAT.dat4$GPM.Measured.Site <  2.2)] <- "<= 2.2"
TableAT.dat4$GPM_bins[which(TableAT.dat4$GPM.Measured.Site >= 2.2)] <- "> 2.2"
unique(TableAT.dat4$GPM_bins)

TableAT.merge <- left_join(rbsa.dat, TableAT.dat4)
TableAT.merge <- TableAT.merge[which(!is.na(TableAT.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
TableAT.data <- weightedData(TableAT.merge[-which(colnames(TableAT.merge) %in% c("GPM.Measured.Site"               
                                                                                 ,"GPM_bins"
                                                                                 ,"count"))])
TableAT.data <- left_join(TableAT.data, TableAT.merge[which(colnames(TableAT.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"))])
#######################
# Weighted Analysis
#######################
TableAT.data$count <- 1
TableAT.final <- proportionRowsAndColumns1(CustomerLevelData = TableAT.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Region")

TableAT.cast <- dcast(setDT(TableAT.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

TableAT.table <- data.frame("BuildingType"   = TableAT.cast$BuildingType
                            ,"Flow.Rate.GPM"  = TableAT.cast$GPM_bins
                            ,"Percent_ID"     = TableAT.cast$w.percent_ID
                            ,"SE_ID"          = TableAT.cast$w.SE_ID
                            ,"n_ID"           = TableAT.cast$n_ID
                            ,"Percent_MT"     = TableAT.cast$w.percent_MT
                            ,"SE_MT"          = TableAT.cast$w.SE_MT
                            ,"n_MT"           = TableAT.cast$n_MT
                            ,"Percent_OR"     = TableAT.cast$w.percent_OR
                            ,"SE_OR"          = TableAT.cast$w.SE_OR
                            ,"n_OR"           = TableAT.cast$n_OR
                            ,"Percent_WA"     = TableAT.cast$w.percent_WA
                            ,"SE_WA"          = TableAT.cast$w.SE_WA
                            ,"n_WA"           = TableAT.cast$n_WA
                            ,"Percent_Region" = TableAT.cast$w.percent_Region
                            ,"SE_Region"      = TableAT.cast$w.SE_Region
                            ,"n_Region"       = TableAT.cast$n_Region
                            ,"EB_ID"          = TableAT.cast$EB_ID
                            ,"EB_MT"          = TableAT.cast$EB_MT
                            ,"EB_OR"          = TableAT.cast$EB_OR
                            ,"EB_WA"          = TableAT.cast$EB_WA
                            ,"EB_Region"      = TableAT.cast$EB_Region
) 
#QAQC
stopifnot(sum(TableAT.table[which(TableAT.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(TableAT.table))], na.rm = T) == 10)

levels(TableAT.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
TableAT.table <- TableAT.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
TableAT.table <- data.frame(TableAT.table)

TableAT.final.SF <- TableAT.table[which(TableAT.table$BuildingType == "Single Family")
                                  ,-which(colnames(TableAT.table) %in% c("BuildingType"))]
TableAT.final.MH <- TableAT.table[which(TableAT.table$BuildingType == "Manufactured")
                                  ,-which(colnames(TableAT.table) %in% c("BuildingType"))]

# exportTable(TableAT.final.SF, "SF", "Table AT", weighted = TRUE)
exportTable(TableAT.final.MH, "MH", "Table AT", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
TableAT.final <- proportions_two_groups_unweighted(CustomerLevelData = TableAT.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'GPM_bins'
                                                   ,aggregateColumnName = "Region")

TableAT.cast <- dcast(setDT(TableAT.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


TableAT.table <- data.frame("BuildingType"   = TableAT.cast$BuildingType
                            ,"Flow.Rate.GPM"  = TableAT.cast$GPM_bins
                            ,"Percent_ID"     = TableAT.cast$Percent_ID
                            ,"SE_ID"          = TableAT.cast$SE_ID
                            ,"n_ID"           = TableAT.cast$n_ID
                            ,"Percent_MT"     = TableAT.cast$Percent_MT
                            ,"SE_MT"          = TableAT.cast$SE_MT
                            ,"n_MT"           = TableAT.cast$n_MT
                            ,"Percent_OR"     = TableAT.cast$Percent_OR
                            ,"SE_OR"          = TableAT.cast$SE_OR
                            ,"n_OR"           = TableAT.cast$n_OR
                            ,"Percent_WA"     = TableAT.cast$Percent_WA
                            ,"SE_WA"          = TableAT.cast$SE_WA
                            ,"n_WA"           = TableAT.cast$n_WA
                            ,"Percent_Region" = TableAT.cast$Percent_Region
                            ,"SE_Region"      = TableAT.cast$SE_Region
                            ,"n_Region"       = TableAT.cast$n_Region
)
stopifnot(sum(TableAT.table[which(TableAT.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(TableAT.table))], na.rm = T) == 10)

levels(TableAT.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
TableAT.table <- TableAT.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
TableAT.table <- data.frame(TableAT.table)

TableAT.final.SF <- TableAT.table[which(TableAT.table$BuildingType == "Single Family")
                                  ,-which(colnames(TableAT.table) %in% c("BuildingType"))]
TableAT.final.MH <- TableAT.table[which(TableAT.table$BuildingType == "Manufactured")
                                  ,-which(colnames(TableAT.table) %in% c("BuildingType"))]

# exportTable(TableAT.final.SF, "SF", "Table AT", weighted = FALSE)
exportTable(TableAT.final.MH, "MH", "Table AT", weighted = FALSE)
