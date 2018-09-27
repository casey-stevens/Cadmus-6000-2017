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
rbsa.dat <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID,ignore.case = T),]

#Read in data for analysis
water.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, water.export))
#clean cadmus IDs
water.dat$CK_Cadmus_ID <- trimws(toupper(water.dat$CK_Cadmus_ID))



#############################################################################################
#Item 106: DISTRIBUTION OF SHOWERHEAD FLOW RATE BY STATE (SF table 113, MH table 88)
#############################################################################################
#subset to columns needed for analysis
item106.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"New.Flow.(Measured.GPM)"
                                                          ,"Fixture.Type"))]
names(item106.dat)[which(names(item106.dat) == "New.Flow.(Measured.GPM)")] <- "GPM_Measured"
item106.dat$count <- 1
item106.dat0 <- item106.dat[which(item106.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item106.dat1 <- left_join(item106.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item106.dat1$GPM_Measured <- as.numeric(as.character(item106.dat1$GPM_Measured))
item106.dat2 <- item106.dat1[which(!(is.na(item106.dat1$GPM_Measured))),]
unique(item106.dat2$GPM_Measured)

item106.dat3 <- item106.dat2[grep("shower",item106.dat2$Fixture.Type, ignore.case = T),]

item106.dat4 <- summarise(group_by(item106.dat3, CK_Cadmus_ID, BuildingType, State, count)
                          ,GPM.Measured.Site = mean(GPM_Measured))
summary(item106.dat4$GPM.Measured.Site)
summary(item106.dat3$GPM_Measured)
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

# exportTable(item106.final.SF, "SF", "Table 113", weighted = TRUE)
# exportTable(item106.final.MH, "MH", "Table 88", weighted = TRUE)

################################################################################
# For Multifamily
################################################################################
item106.table.MF <- proportions_one_group(CustomerLevelData    = item106.data
                                         ,valueVariable    = 'count'
                                         ,groupingVariable = 'GPM_bins'
                                         ,total.name       = "Remove")
# item106.table.MF <- item106.table.MF[which(item106.table.MF$GPM_bins != "Total"),]

unique(item106.table.MF$GPM_bins)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106.table.MF <- item106.table.MF %>% mutate(GPM_bins = factor(GPM_bins, levels = rowOrder)) %>% arrange(GPM_bins)  
item106.table.MF <- data.frame(item106.table.MF)


item106.final.MF <- item106.table.MF[which(item106.table.MF$BuildingType == "Multifamily")
                                   ,-which(colnames(item106.table.MF) %in% c("BuildingType"))]
# exportTable(item106.final.MF, "MF", "Table 80", weighted = TRUE)



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

# exportTable(item106.final.SF, "SF", "Table 113", weighted = FALSE)
# exportTable(item106.final.MH, "MH", "Table 88", weighted = FALSE)


################################################################################
# For Multifamily
################################################################################
item106.table.MF <- proportions_one_group(CustomerLevelData    = item106.data
                                          ,valueVariable    = 'count'
                                          ,groupingVariable = 'GPM_bins'
                                          ,total.name       = "Remove"
                                          ,weighted = FALSE)
# item106.table.MF <- item106.table.MF[which(item106.table.MF$GPM_bins != "Total"),]

unique(item106.table.MF$GPM_bins)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106.table.MF <- item106.table.MF %>% mutate(GPM_bins = factor(GPM_bins, levels = rowOrder)) %>% arrange(GPM_bins)  
item106.table.MF <- data.frame(item106.table.MF)


item106.final.MF <- item106.table.MF[which(item106.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(item106.table.MF) %in% c("BuildingType"))]
# exportTable(item106.final.MF, "MF", "Table 80", weighted = FALSE)





#############################################################################################
# Table AR: DISTRIBUTION OF SHOWERHEAD FLOW RATE BY STATE (new bins)
#############################################################################################
#subset to columns needed for analysis
colnames(water.dat)
tableAR.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"New.Flow.(Measured.GPM)"
                                                          ,"Fixture.Type"))]
names(tableAR.dat)[which(names(tableAR.dat) == "New.Flow.(Measured.GPM)")] <- "GPM_Measured"
tableAR.dat$count <- 1

tableAR.dat0 <- tableAR.dat[which(tableAR.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableAR.dat1 <- left_join(tableAR.dat0, rbsa.dat, by = "CK_Cadmus_ID")

tableAR.dat1$GPM_Measured <- as.numeric(as.character(tableAR.dat1$GPM_Measured))
tableAR.dat2 <- tableAR.dat1[which(!(is.na(tableAR.dat1$GPM_Measured))),]
unique(tableAR.dat2$GPM_Measured)

tableAR.dat3 <- tableAR.dat2[grep("shower|Shower",tableAR.dat2$Fixture.Type),]

tableAR.dat4 <- summarise(group_by(tableAR.dat3, CK_Cadmus_ID)
                          ,GPM.Measured.Site = mean(GPM_Measured))

tableAR.dat4$GPM_bins <- tableAR.dat4$GPM.Measured.Site
tableAR.dat4$GPM_bins[which(tableAR.dat4$GPM.Measured.Site <  2.5)] <- "< 2.5"
tableAR.dat4$GPM_bins[which(tableAR.dat4$GPM.Measured.Site >= 2.5)] <- ">= 2.5"
unique(tableAR.dat4$GPM_bins)

tableAR.merge <- left_join(rbsa.dat, tableAR.dat4)
tableAR.merge <- tableAR.merge[which(!is.na(tableAR.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
tableAR.data <- weightedData(tableAR.merge[-which(colnames(tableAR.merge) %in% c("GPM.Measured.Site"               
                                                                                 ,"GPM_bins"
                                                                                 ,"count"))])
tableAR.data <- left_join(tableAR.data, tableAR.merge[which(colnames(tableAR.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"))])

tableAR.data$count <- 1
#######################
# Weighted Analysis
#######################
tableAR.data$Count <- 1
tableAR.final <- proportionRowsAndColumns1(CustomerLevelData = tableAR.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Region")

tableAR.cast <- dcast(setDT(tableAR.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

tableAR.table <- data.frame("BuildingType"   = tableAR.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAR.cast$GPM_bins
                            ,"Percent_ID"     = tableAR.cast$w.percent_ID
                            ,"SE_ID"          = tableAR.cast$w.SE_ID
                            ,"n_ID"           = tableAR.cast$n_ID
                            ,"Percent_MT"     = tableAR.cast$w.percent_MT
                            ,"SE_MT"          = tableAR.cast$w.SE_MT
                            ,"n_MT"           = tableAR.cast$n_MT
                            ,"Percent_OR"     = tableAR.cast$w.percent_OR
                            ,"SE_OR"          = tableAR.cast$w.SE_OR
                            ,"n_OR"           = tableAR.cast$n_OR
                            ,"Percent_WA"     = tableAR.cast$w.percent_WA
                            ,"SE_WA"          = tableAR.cast$w.SE_WA
                            ,"n_WA"           = tableAR.cast$n_WA
                            ,"Percent_Region" = tableAR.cast$w.percent_Region
                            ,"SE_Region"      = tableAR.cast$w.SE_Region
                            ,"n_Region"       = tableAR.cast$n_Region
                            ,"EB_ID"          = tableAR.cast$EB_ID
                            ,"EB_MT"          = tableAR.cast$EB_MT
                            ,"EB_OR"          = tableAR.cast$EB_OR
                            ,"EB_WA"          = tableAR.cast$EB_WA
                            ,"EB_Region"      = tableAR.cast$EB_Region
) 
#QAQC
stopifnot(sum(tableAR.table[which(tableAR.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(tableAR.table))], na.rm = T) == 10)

levels(tableAR.table$Flow.Rate.GPM)
rowOrder <- c("< 2.5"
              ,">= 2.5"
              ,"Total")
tableAR.table <- tableAR.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAR.table <- data.frame(tableAR.table)

tableAR.final.SF <- tableAR.table[which(tableAR.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAR.table) %in% c("BuildingType"))]
tableAR.final.MH <- tableAR.table[which(tableAR.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAR.table) %in% c("BuildingType"))]

# exportTable(tableAR.final.SF, "SF", "Table AR", weighted = TRUE)
# exportTable(tableAR.final.MH, "MH", "Table AR", weighted = TRUE)

################################################################################
# For Multifamily
################################################################################
tableAR.table.MF <- proportions_one_group(CustomerLevelData    = tableAR.data
                                          ,valueVariable    = 'count'
                                          ,groupingVariable = 'GPM_bins'
                                          ,total.name       = "Remove"
                                          ,weighted = TRUE)

tableAR.final.MF <- tableAR.table.MF[which(tableAR.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAR.table.MF) %in% c("BuildingType"))]
# exportTable(tableAR.final.MF, "MF", "Table AR", weighted = TRUE)




#######################
# Unweighted Analysis
#######################
tableAR.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAR.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'GPM_bins'
                                                   ,aggregateColumnName = "Region")

tableAR.cast <- dcast(setDT(tableAR.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


tableAR.table <- data.frame("BuildingType"   = tableAR.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAR.cast$GPM_bins
                            ,"Percent_ID"     = tableAR.cast$Percent_ID
                            ,"SE_ID"          = tableAR.cast$SE_ID
                            ,"n_ID"           = tableAR.cast$n_ID
                            ,"Percent_MT"     = tableAR.cast$Percent_MT
                            ,"SE_MT"          = tableAR.cast$SE_MT
                            ,"n_MT"           = tableAR.cast$n_MT
                            ,"Percent_OR"     = tableAR.cast$Percent_OR
                            ,"SE_OR"          = tableAR.cast$SE_OR
                            ,"n_OR"           = tableAR.cast$n_OR
                            ,"Percent_WA"     = tableAR.cast$Percent_WA
                            ,"SE_WA"          = tableAR.cast$SE_WA
                            ,"n_WA"           = tableAR.cast$n_WA
                            ,"Percent_Region" = tableAR.cast$Percent_Region
                            ,"SE_Region"      = tableAR.cast$SE_Region
                            ,"n_Region"       = tableAR.cast$n_Region
)
stopifnot(sum(tableAR.table[which(tableAR.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(tableAR.table))], na.rm = T) == 10)

levels(tableAR.table$Flow.Rate.GPM)
rowOrder <- c("< 2.5"
              ,">= 2.5"
              ,"Total")
tableAR.table <- tableAR.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAR.table <- data.frame(tableAR.table)

tableAR.final.SF <- tableAR.table[which(tableAR.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAR.table) %in% c("BuildingType"))]
tableAR.final.MH <- tableAR.table[which(tableAR.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAR.table) %in% c("BuildingType"))]

# exportTable(tableAR.final.SF, "SF", "Table AR", weighted = FALSE)
# exportTable(tableAR.final.MH, "MH", "Table AR", weighted = FALSE)

################################################################################
# For Multifamily
################################################################################
tableAR.table.MF <- proportions_one_group(CustomerLevelData    = tableAR.data
                                          ,valueVariable    = 'count'
                                          ,groupingVariable = 'GPM_bins'
                                          ,total.name       = "Remove"
                                          ,weighted = FALSE)

tableAR.final.MF <- tableAR.table.MF[which(tableAR.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAR.table.MF) %in% c("BuildingType"))]
# exportTable(tableAR.final.MF, "MF", "Table AR", weighted = FALSE)





#############################################################################################
# Table AS: DISTRIBUTION OF Bathroom Faucet FLOW RATE BY STATE
#############################################################################################
#subset to columns needed for analysis
tableAS.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"New.Flow.(Measured.GPM)"
                                                          ,"Fixture.Type"))]
names(tableAS.dat)[which(names(tableAS.dat) == "New.Flow.(Measured.GPM)")] <- "GPM_Measured"
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
tableAS.dat4$GPM_bins[which(tableAS.dat4$GPM.Measured.Site <=  2.2)] <- "<= 2.2"
tableAS.dat4$GPM_bins[which(tableAS.dat4$GPM.Measured.Site > 2.2)] <- "> 2.2"
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

tableAS.data$count <- 1
#######################
# Weighted Analysis
#######################
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
# exportTable(tableAS.final.MH, "MH", "Table AS", weighted = TRUE)


################################################################################
# For Multifamily
################################################################################
tableAS.table.MF <- proportions_one_group(CustomerLevelData    = tableAS.data
                                          ,valueVariable    = 'count'
                                          ,groupingVariable = 'GPM_bins'
                                          ,total.name       = "Remove"
                                          ,weighted = TRUE)

tableAS.final.MF <- tableAS.table.MF[which(tableAS.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAS.table.MF) %in% c("BuildingType"))]
# exportTable(tableAS.final.MF, "MF", "Table AS", weighted = TRUE)



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
# exportTable(tableAS.final.MH, "MH", "Table AS", weighted = FALSE)


################################################################################
# For Multifamily
################################################################################
tableAS.table.MF <- proportions_one_group(CustomerLevelData    = tableAS.data
                                          ,valueVariable    = 'count'
                                          ,groupingVariable = 'GPM_bins'
                                          ,total.name       = "Remove"
                                          ,weighted = FALSE)

tableAS.final.MF <- tableAS.table.MF[which(tableAS.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAS.table.MF) %in% c("BuildingType"))]
# exportTable(tableAS.final.MF, "MF", "Table AS", weighted = FALSE)





#############################################################################################
# Table AT: DISTRIBUTION OF Kitchen Faucet FLOW RATE BY STATE
#############################################################################################
#subset to columns needed for analysis
tableAT.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"New.Flow.(Measured.GPM)"
                                                          ,"Fixture.Type"))]
names(tableAT.dat)[which(names(tableAT.dat) == "New.Flow.(Measured.GPM)")] <- "GPM_Measured"
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
tableAT.dat4$GPM_bins[which(tableAT.dat4$GPM.Measured.Site <=  2.2)] <- "<= 2.2"
tableAT.dat4$GPM_bins[which(tableAT.dat4$GPM.Measured.Site > 2.2)] <- "> 2.2"
unique(tableAT.dat4$GPM_bins)

tableAT.merge <- left_join(rbsa.dat, tableAT.dat4)
tableAT.merge <- tableAT.merge[which(!is.na(tableAT.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
tableAT.data <- weightedData(tableAT.merge[-which(colnames(tableAT.merge) %in% c("GPM.Measured.Site"               
                                                                                 ,"GPM_bins"
                                                                                 ,"count"))])
tableAT.data <- left_join(tableAT.data, tableAT.merge[which(colnames(tableAT.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"))])
tableAT.data$count <- 1
tableAT.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableAT.final <- proportionRowsAndColumns1(CustomerLevelData = tableAT.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Region")

tableAT.cast <- dcast(setDT(tableAT.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

tableAT.table <- data.frame("BuildingType"   = tableAT.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAT.cast$GPM_bins
                            ,"Percent_ID"     = tableAT.cast$w.percent_ID
                            ,"SE_ID"          = tableAT.cast$w.SE_ID
                            ,"n_ID"           = tableAT.cast$n_ID
                            ,"Percent_MT"     = tableAT.cast$w.percent_MT
                            ,"SE_MT"          = tableAT.cast$w.SE_MT
                            ,"n_MT"           = tableAT.cast$n_MT
                            ,"Percent_OR"     = tableAT.cast$w.percent_OR
                            ,"SE_OR"          = tableAT.cast$w.SE_OR
                            ,"n_OR"           = tableAT.cast$n_OR
                            ,"Percent_WA"     = tableAT.cast$w.percent_WA
                            ,"SE_WA"          = tableAT.cast$w.SE_WA
                            ,"n_WA"           = tableAT.cast$n_WA
                            ,"Percent_Region" = tableAT.cast$w.percent_Region
                            ,"SE_Region"      = tableAT.cast$w.SE_Region
                            ,"n_Region"       = tableAT.cast$n_Region
                            ,"EB_ID"          = tableAT.cast$EB_ID
                            ,"EB_MT"          = tableAT.cast$EB_MT
                            ,"EB_OR"          = tableAT.cast$EB_OR
                            ,"EB_WA"          = tableAT.cast$EB_WA
                            ,"EB_Region"      = tableAT.cast$EB_Region
) 
#QAQC
stopifnot(sum(tableAT.table[which(tableAT.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(tableAT.table))], na.rm = T) == 10)

levels(tableAT.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
tableAT.table <- tableAT.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAT.table <- data.frame(tableAT.table)

tableAT.final.SF <- tableAT.table[which(tableAT.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAT.table) %in% c("BuildingType"))]
tableAT.final.MH <- tableAT.table[which(tableAT.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAT.table) %in% c("BuildingType"))]

# exportTable(tableAT.final.SF, "SF", "Table AT", weighted = TRUE)
# exportTable(tableAT.final.MH, "MH", "Table AT", weighted = TRUE)


################################################################################
# For Multifamily
################################################################################
tableAT.table.MF <- proportions_one_group(CustomerLevelData    = tableAT.data
                                          ,valueVariable    = 'count'
                                          ,groupingVariable = 'GPM_bins'
                                          ,total.name       = "Remove"
                                          ,weighted = TRUE)

tableAT.final.MF <- tableAT.table.MF[which(tableAT.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAT.table.MF) %in% c("BuildingType"))]
# exportTable(tableAT.final.MF, "MF", "Table AT", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
tableAT.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAT.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'GPM_bins'
                                                   ,aggregateColumnName = "Region")

tableAT.cast <- dcast(setDT(tableAT.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


tableAT.table <- data.frame("BuildingType"   = tableAT.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAT.cast$GPM_bins
                            ,"Percent_ID"     = tableAT.cast$Percent_ID
                            ,"SE_ID"          = tableAT.cast$SE_ID
                            ,"n_ID"           = tableAT.cast$n_ID
                            ,"Percent_MT"     = tableAT.cast$Percent_MT
                            ,"SE_MT"          = tableAT.cast$SE_MT
                            ,"n_MT"           = tableAT.cast$n_MT
                            ,"Percent_OR"     = tableAT.cast$Percent_OR
                            ,"SE_OR"          = tableAT.cast$SE_OR
                            ,"n_OR"           = tableAT.cast$n_OR
                            ,"Percent_WA"     = tableAT.cast$Percent_WA
                            ,"SE_WA"          = tableAT.cast$SE_WA
                            ,"n_WA"           = tableAT.cast$n_WA
                            ,"Percent_Region" = tableAT.cast$Percent_Region
                            ,"SE_Region"      = tableAT.cast$SE_Region
                            ,"n_Region"       = tableAT.cast$n_Region
)
stopifnot(sum(tableAT.table[which(tableAT.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(tableAT.table))], na.rm = T) == 10)

levels(tableAT.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
tableAT.table <- tableAT.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAT.table <- data.frame(tableAT.table)

tableAT.final.SF <- tableAT.table[which(tableAT.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAT.table) %in% c("BuildingType"))]
tableAT.final.MH <- tableAT.table[which(tableAT.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAT.table) %in% c("BuildingType"))]

# exportTable(tableAT.final.SF, "SF", "Table AT", weighted = FALSE)
# exportTable(tableAT.final.MH, "MH", "Table AT", weighted = FALSE)


################################################################################
# For Multifamily
################################################################################
tableAT.table.MF <- proportions_one_group(CustomerLevelData    = tableAT.data
                                          ,valueVariable    = 'count'
                                          ,groupingVariable = 'GPM_bins'
                                          ,total.name       = "Remove"
                                          ,weighted = FALSE)

tableAT.final.MF <- tableAT.table.MF[which(tableAT.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAT.table.MF) %in% c("BuildingType"))]
# exportTable(tableAT.final.MF, "MF", "Table AT", weighted = FALSE)
















# #############################################################################################
# # Table FF: PERCENTAGE OF HOMES WITH SHOWERHEADS ABOVE 2.0 GPM BY STATE
# #############################################################################################
# #subset to columns needed for analysis
# tableFF.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
#                                                           ,"New.Flow.(Measured.GPM)"
#                                                           ,"Fixture.Type"))]
# names(item106.dat)[which(names(item106.dat) == "New.Flow.(Measured.GPM)")] <- "GPM_Measured"
# tableFF.dat$count <- 1
# 
# tableFF.dat0 <- tableFF.dat[which(tableFF.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# tableFF.dat1 <- left_join(rbsa.dat, tableFF.dat0, by = "CK_Cadmus_ID")
# 
# tableFF.dat1$GPM_Measured <- as.numeric(as.character(tableFF.dat1$GPM_Measured))
# tableFF.dat2 <- tableFF.dat1[which(!(is.na(tableFF.dat1$GPM_Measured))),]
# unique(tableFF.dat2$GPM_Measured)
# 
# tableFF.dat3 <- tableFF.dat2[grep("shower|Shower",tableFF.dat2$Fixture.Type),]
# 
# tableFF.dat4 <- summarise(group_by(tableFF.dat3, CK_Cadmus_ID)
#                           ,GPM.Measured.Site = mean(GPM_Measured))
# 
# tableFF.dat4$Ind <- 0
# tableFF.dat4$Ind[which(tableFF.dat4$GPM.Measured.Site > 2)] <- 1
# 
# 
# tableFF.merge <- left_join(rbsa.dat, tableFF.dat4)
# tableFF.merge <- tableFF.merge[which(!is.na(tableFF.merge$Ind)),]
# 
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# tableFF.data <- weightedData(tableFF.merge[-which(colnames(tableFF.merge) %in% c("GPM.Measured.Site"
#                                                                                  ,"Ind"))])
# tableFF.data <- left_join(tableFF.data, tableFF.merge[which(colnames(tableFF.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"GPM.Measured.Site"
#                                                                                            ,"Ind"))])
# tableFF.data$count <- 1
# tableFF.data$Count <- 1
# 
# #######################
# # Weighted Analysis
# #######################
# tableFF.table <- proportions_one_group(CustomerLevelData = tableFF.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "State"
#                                        ,total.name = "Region"
#                                        ,weighted = TRUE)
# 
# tableFF.final.SF <- tableFF.table[which(tableFF.table$BuildingType == "Single Family")
#                                   ,-which(colnames(tableFF.table) %in% c("BuildingType"))]
# tableFF.final.MH <- tableFF.table[which(tableFF.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(tableFF.table) %in% c("BuildingType"))]
# 
# # exportTable(tableFF.final.SF, "SF", "Table FF", weighted = TRUE)
# # exportTable(tableFF.final.MH, "MH", "Table FF", weighted = TRUE)
# 
# ################################################################################
# # For Multifamily
# ################################################################################
# tableFF.table.MF <- proportions_one_group(CustomerLevelData = tableFF.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "State"
#                                        ,total.name = "Region"
#                                        ,weighted = TRUE)
# tableFF.table.MF <- tableFF.table.MF[which(tableFF.table.MF$State == "Total"),]
# tableFF.table.MF$State[which(tableFF.table.MF$State == "Total")] <- "Region"
# 
# tableFF.final.MF <- tableFF.table.MF[which(tableFF.table.MF$BuildingType == "Multifamily")
#                                      ,-which(colnames(tableFF.table.MF) %in% c("BuildingType"))]
# # exportTable(tableFF.final.MF, "MF", "Table FF", weighted = TRUE)
# 
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# tableFF.table <- proportions_one_group(CustomerLevelData = tableFF.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "State"
#                                        ,total.name = "Region"
#                                        ,weighted = FALSE)
# 
# tableFF.final.SF <- tableFF.table[which(tableFF.table$BuildingType == "Single Family")
#                                   ,-which(colnames(tableFF.table) %in% c("BuildingType"))]
# tableFF.final.MH <- tableFF.table[which(tableFF.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(tableFF.table) %in% c("BuildingType"))]
# 
# # exportTable(tableFF.final.SF, "SF", "Table FF", weighted = FALSE)
# # exportTable(tableFF.final.MH, "MH", "Table FF", weighted = FALSE)
# 
# 
# ################################################################################
# # For Multifamily
# ################################################################################
# tableFF.table.MF <- proportions_one_group(CustomerLevelData = tableFF.data
#                                           ,valueVariable = "Ind"
#                                           ,groupingVariable = "State"
#                                           ,total.name = "Region"
#                                           ,weighted = FALSE)
# tableFF.table.MF <- tableFF.table.MF[which(tableFF.table.MF$State == "Total"),]
# tableFF.table.MF$State[which(tableFF.table.MF$State == "Total")] <- "Region"
# 
# tableFF.final.MF <- tableFF.table.MF[which(tableFF.table.MF$BuildingType == "Multifamily")
#                                      ,-which(colnames(tableFF.table.MF) %in% c("BuildingType"))]
# # exportTable(tableFF.final.MF, "MF", "Table FF", weighted = FALSE)









# #############################################################################################
# #Table AM: Average number of showerheads and faucets per home by Type and State
# #############################################################################################
# #subset to columns needed for analysis
# tableAM.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
#                                                           ,"New.Flow.(Measured.GPM)"
#                                                           ,"Fixture.Type"))]
# names(item106.dat)[which(names(item106.dat) == "New.Flow.(Measured.GPM)")] <- "GPM_Measured"
# tableAM.dat$count <- 1
# 
# tableAM.dat0 <- tableAM.dat[which(tableAM.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# tableAM.dat1 <- left_join(tableAM.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# 
# tableAM.dat1$GPM_Measured <- as.numeric(as.character(tableAM.dat1$GPM_Measured))
# tableAM.dat2 <- tableAM.dat1[which(!(is.na(tableAM.dat1$GPM_Measured))),]
# unique(tableAM.dat2$GPM_Measured)
# unique(tableAM.dat2$Fixture.Type)
# tableAM.dat3 <- tableAM.dat2[grep("bathroom|faucet|shower",tableAM.dat2$Fixture.Type,ignore.case = T),]
# tableAM.dat3$count <- 1
# tableAM.dat3$Count <- 1
# 
# #cast the melt example code
# tableAM.cast <- dcast(setDT(tableAM.dat3)
#                       ,formula = CK_Cadmus_ID ~ Fixture.Type,sum
#                       ,value.var = c("Count"))
# tableAM.cast[is.na(tableAM.cast),] <- 0
# 
# tableAM.melt <- melt(tableAM.cast, id.vars = "CK_Cadmus_ID")
# names(tableAM.melt) <- c("CK_Cadmus_ID", "Fixture.Type", "Count")
# 
# tableAM.dat4 <- summarise(group_by(tableAM.melt, CK_Cadmus_ID, Fixture.Type)
#                           ,Site.Count = sum(Count))
# 
# tableAM.merge <- left_join(rbsa.dat, tableAM.dat4)
# tableAM.merge$Site.Count[which(is.na(tableAM.merge$Site.Count))] <- 0
# tableAM.merge <- tableAM.merge[which(tableAM.merge$Fixture.Type %notin% c("N/A",NA)),]
# 
# unique(tableAM.merge$Fixture.Type)
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# tableAM.data <- weightedData(tableAM.merge[-which(colnames(tableAM.merge) %in% c("Site.Count"
#                                                                                  ,"Fixture.Type"))])
# tableAM.data <- left_join(tableAM.data, tableAM.merge[which(colnames(tableAM.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Site.Count"
#                                                                                            ,"Fixture.Type"))])
# tableAM.data$count <- 1
# #######################
# # Weighted Analysis
# #######################
# tableAM.cast <- mean_two_groups(CustomerLevelData = tableAM.data
#                                 ,valueVariable = "Site.Count"
#                                 ,byVariableRow = "Fixture.Type"
#                                 ,byVariableColumn = "State"
#                                 ,columnAggregate = "Region"
#                                 ,rowAggregate = "All Fixtures")
# 
# tableAM.table <- data.frame("BuildingType"    = tableAM.cast$BuildingType
#                             ,"Fixture.Type"   = tableAM.cast$Fixture.Type
#                             ,"Mean_ID"        = tableAM.cast$Mean_ID
#                             ,"SE_ID"          = tableAM.cast$SE_ID
#                             ,"n_ID"           = tableAM.cast$n_ID
#                             ,"Mean_MT"        = tableAM.cast$Mean_MT
#                             ,"SE_MT"          = tableAM.cast$SE_MT
#                             ,"n_MT"           = tableAM.cast$n_MT
#                             ,"Mean_OR"        = tableAM.cast$Mean_OR
#                             ,"SE_OR"          = tableAM.cast$SE_OR
#                             ,"n_OR"           = tableAM.cast$n_OR
#                             ,"Mean_WA"        = tableAM.cast$Mean_WA
#                             ,"SE_WA"          = tableAM.cast$SE_WA
#                             ,"n_WA"           = tableAM.cast$n_WA
#                             ,"Mean_Region"    = tableAM.cast$Mean_Region
#                             ,"SE_Region"      = tableAM.cast$SE_Region
#                             ,"n_Region"       = tableAM.cast$n_Region
#                             ,"EB_ID"          = tableAM.cast$EB_ID
#                             ,"EB_MT"          = tableAM.cast$EB_MT
#                             ,"EB_OR"          = tableAM.cast$EB_OR
#                             ,"EB_WA"          = tableAM.cast$EB_WA
#                             ,"EB_Region"      = tableAM.cast$EB_Region
# ) 
# 
# tableAM.table <- tableAM.table[which(tableAM.table$Fixture.Type != "All Fixtures"),]
# 
# tableAM.final.SF <- tableAM.table[which(tableAM.table$BuildingType == "Single Family")
#                                   ,-which(colnames(tableAM.table) %in% c("BuildingType"))]
# tableAM.final.MH <- tableAM.table[which(tableAM.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(tableAM.table) %in% c("BuildingType"))]
# 
# # exportTable(tableAM.final.SF, "SF", "Table AM", weighted = TRUE)
# # exportTable(tableAM.final.MH, "MH", "Table AM", weighted = TRUE)
# 
# #######################
# # Multifamily
# #######################
# tableAM.table.MF <- mean_one_group(CustomerLevelData = tableAM.data
#                                    ,valueVariable = "Site.Count"
#                                    ,byVariable = "Fixture.Type"
#                                    ,aggregateRow = "All Types")
# 
# tableAM.final.MF <- tableAM.table.MF[which(tableAM.table.MF$BuildingType == "Multifamily")
#                                      ,-which(colnames(tableAM.table.MF) %in% c("BuildingType"))]
# 
# # exportTable(tableAM.final.MF, "MF", "Table AM", weighted = TRUE)
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# tableAM.cast <- mean_two_groups_unweighted(CustomerLevelData = tableAM.data
#                                            ,valueVariable = "Site.Count"
#                                            ,byVariableRow = "Fixture.Type"
#                                            ,byVariableColumn = "State"
#                                            ,columnAggregate = "Region"
#                                            ,rowAggregate = "All Fixtures")
# 
# tableAM.table <- data.frame("BuildingType"    = tableAM.cast$BuildingType
#                             ,"Fixture.Type"   = tableAM.cast$Fixture.Type
#                             ,"Mean_ID"        = tableAM.cast$Mean_ID
#                             ,"SE_ID"          = tableAM.cast$SE_ID
#                             ,"n_ID"           = tableAM.cast$n_ID
#                             ,"Mean_MT"        = tableAM.cast$Mean_MT
#                             ,"SE_MT"          = tableAM.cast$SE_MT
#                             ,"n_MT"           = tableAM.cast$n_MT
#                             ,"Mean_OR"        = tableAM.cast$Mean_OR
#                             ,"SE_OR"          = tableAM.cast$SE_OR
#                             ,"n_OR"           = tableAM.cast$n_OR
#                             ,"Mean_WA"        = tableAM.cast$Mean_WA
#                             ,"SE_WA"          = tableAM.cast$SE_WA
#                             ,"n_WA"           = tableAM.cast$n_WA
#                             ,"Mean_Region"    = tableAM.cast$Mean_Region
#                             ,"SE_Region"      = tableAM.cast$SE_Region
#                             ,"n_Region"       = tableAM.cast$n_Region
# )
# 
# tableAM.table <- tableAM.table[which(tableAM.table$Fixture.Type != "All Fixtures"),]
# 
# tableAM.final.SF <- tableAM.table[which(tableAM.table$BuildingType == "Single Family")
#                                   ,-which(colnames(tableAM.table) %in% c("BuildingType"))]
# tableAM.final.MH <- tableAM.table[which(tableAM.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(tableAM.table) %in% c("BuildingType"))]
# 
# # exportTable(tableAM.final.SF, "SF", "Table AM", weighted = FALSE)
# # exportTable(tableAM.final.MH, "MH", "Table AM", weighted = FALSE)
# 
# 
# #######################
# # Multifamily
# #######################
# tableAM.table.MF <- mean_one_group_unweighted(CustomerLevelData = tableAM.data
#                                               ,valueVariable = "Site.Count"
#                                               ,byVariable = "Fixture.Type"
#                                               ,aggregateRow = "All Types")
# 
# tableAM.final.MF <- tableAM.table.MF[which(tableAM.table.MF$BuildingType == "Multifamily")
#                                      ,-which(colnames(tableAM.table.MF) %in% c("BuildingType"))]
# 
# # exportTable(tableAM.final.MF, "MF", "Table AM", weighted = FALSE)






