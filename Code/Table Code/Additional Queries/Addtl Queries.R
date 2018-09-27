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
item106Q1.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"
                                                          ,"Primary.Shower"))]
item106Q1.dat$count <- 1

item106Q1.dat0 <- item106Q1.dat[which(item106Q1.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item106Q1.dat1 <- left_join(item106Q1.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item106Q1.dat1$GPM_Measured <- as.numeric(as.character(item106Q1.dat1$GPM_Measured))
item106Q1.dat2 <- item106Q1.dat1[which(!(is.na(item106Q1.dat1$GPM_Measured))),]
unique(item106Q1.dat2$GPM_Measured)

item106Q1.dat3 <- item106Q1.dat2[grep("shower",item106Q1.dat2$Fixture.Type, ignore.case = T),]
item106Q1.dat3 <- item106Q1.dat3[grep("primary|all used",item106Q1.dat3$Primary.Shower, ignore.case = T),]

item106Q1.dat4 <- summarise(group_by(item106Q1.dat3, CK_Cadmus_ID, BuildingType, State, count)
                          ,GPM.Measured.Site = mean(GPM_Measured))

item106Q1.dat4$GPM_bins <- item106Q1.dat4$GPM.Measured.Site
item106Q1.dat4$GPM_bins[which(item106Q1.dat4$GPM.Measured.Site <= 1.5)] <- "< 1.5"
item106Q1.dat4$GPM_bins[which(item106Q1.dat4$GPM.Measured.Site >  1.5 & item106Q1.dat4$GPM.Measured.Site < 2.1)] <- "1.6-2.0"
item106Q1.dat4$GPM_bins[which(item106Q1.dat4$GPM.Measured.Site >= 2.1 & item106Q1.dat4$GPM.Measured.Site < 2.6)] <- "2.1-2.5"
item106Q1.dat4$GPM_bins[which(item106Q1.dat4$GPM.Measured.Site >= 2.6 & item106Q1.dat4$GPM.Measured.Site < 3.6)] <- "2.6-3.5"
item106Q1.dat4$GPM_bins[which(item106Q1.dat4$GPM.Measured.Site >= 3.6)] <- "> 3.6"
# item106Q1.dat4$GPM_bins[which(item106Q1.dat4$GPM.Measured.Site >= 2.6)] <- "> 2.5"
unique(item106Q1.dat4$GPM_bins)

item106Q1.merge <- left_join(rbsa.dat, item106Q1.dat4)
item106Q1.merge <- item106Q1.merge[which(!is.na(item106Q1.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
item106Q1.data <- weightedData(item106Q1.merge[-which(colnames(item106Q1.merge) %in% c("GPM.Measured.Site"               
                                                                                 ,"GPM_bins"
                                                                                 ,"count"
                                                                                 ,"Primary.Shower"))])
item106Q1.data <- left_join(item106Q1.data, item106Q1.merge[which(colnames(item106Q1.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"
                                                                                           ,"Primary.Shower"))])
#######################
# Weighted Analysis
#######################
item106Q1.final <- proportionRowsAndColumns1(CustomerLevelData = item106Q1.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Region")

item106Q1.cast <- dcast(setDT(item106Q1.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item106Q1.table <- data.frame("BuildingType"   = item106Q1.cast$BuildingType
                            ,"Flow.Rate.GPM"  = item106Q1.cast$GPM_bins
                            ,"Percent_ID"     = item106Q1.cast$w.percent_ID
                            ,"SE_ID"          = item106Q1.cast$w.SE_ID
                            ,"n_ID"           = item106Q1.cast$n_ID
                            ,"Percent_MT"     = item106Q1.cast$w.percent_MT
                            ,"SE_MT"          = item106Q1.cast$w.SE_MT
                            ,"n_MT"           = item106Q1.cast$n_MT
                            ,"Percent_OR"     = item106Q1.cast$w.percent_OR
                            ,"SE_OR"          = item106Q1.cast$w.SE_OR
                            ,"n_OR"           = item106Q1.cast$n_OR
                            ,"Percent_WA"     = item106Q1.cast$w.percent_WA
                            ,"SE_WA"          = item106Q1.cast$w.SE_WA
                            ,"n_WA"           = item106Q1.cast$n_WA
                            ,"Percent_Region" = item106Q1.cast$w.percent_Region
                            ,"SE_Region"      = item106Q1.cast$w.SE_Region
                            ,"n_Region"       = item106Q1.cast$n_Region
                            ,"EB_ID"          = item106Q1.cast$EB_ID
                            ,"EB_MT"          = item106Q1.cast$EB_MT
                            ,"EB_OR"          = item106Q1.cast$EB_OR
                            ,"EB_WA"          = item106Q1.cast$EB_WA
                            ,"EB_Region"      = item106Q1.cast$EB_Region
) 
#QAQC
stopifnot(sum(item106Q1.table[which(item106Q1.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(item106Q1.table))], na.rm = T) == 10)

levels(item106Q1.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q1.table <- item106Q1.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
item106Q1.table <- data.frame(item106Q1.table)

item106Q1.final.SF <- item106Q1.table[which(item106Q1.table$BuildingType == "Single Family")
                                  ,-which(colnames(item106Q1.table) %in% c("BuildingType"))]
item106Q1.final.MH <- item106Q1.table[which(item106Q1.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item106Q1.table) %in% c("BuildingType"))]


################################################################################
# For Multifamily
################################################################################
item106Q1.table.MF <- proportions_one_group(CustomerLevelData    = item106Q1.data
                                          ,valueVariable    = 'count'
                                          ,groupingVariable = 'GPM_bins'
                                          ,total.name       = "Remove")
# item106Q1.table.MF <- item106Q1.table.MF[which(item106Q1.table.MF$GPM_bins != "Total"),]

unique(item106Q1.table.MF$GPM_bins)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q1.table.MF <- item106Q1.table.MF %>% mutate(GPM_bins = factor(GPM_bins, levels = rowOrder)) %>% arrange(GPM_bins)  
item106Q1.table.MF <- data.frame(item106Q1.table.MF)


item106Q1.final.MF <- item106Q1.table.MF[which(item106Q1.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(item106Q1.table.MF) %in% c("BuildingType"))]



#######################
# Unweighted Analysis
#######################
item106Q1.final <- proportions_two_groups_unweighted(CustomerLevelData = item106Q1.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'GPM_bins'
                                                   ,aggregateColumnName = "Region")

item106Q1.cast <- dcast(setDT(item106Q1.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


item106Q1.table <- data.frame("BuildingType"   = item106Q1.cast$BuildingType
                            ,"Flow.Rate.GPM"  = item106Q1.cast$GPM_bins
                            ,"Percent_ID"     = item106Q1.cast$Percent_ID
                            ,"SE_ID"          = item106Q1.cast$SE_ID
                            ,"n_ID"           = item106Q1.cast$n_ID
                            ,"Percent_MT"     = item106Q1.cast$Percent_MT
                            ,"SE_MT"          = item106Q1.cast$SE_MT
                            ,"n_MT"           = item106Q1.cast$n_MT
                            ,"Percent_OR"     = item106Q1.cast$Percent_OR
                            ,"SE_OR"          = item106Q1.cast$SE_OR
                            ,"n_OR"           = item106Q1.cast$n_OR
                            ,"Percent_WA"     = item106Q1.cast$Percent_WA
                            ,"SE_WA"          = item106Q1.cast$SE_WA
                            ,"n_WA"           = item106Q1.cast$n_WA
                            ,"Percent_Region" = item106Q1.cast$Percent_Region
                            ,"SE_Region"      = item106Q1.cast$SE_Region
                            ,"n_Region"       = item106Q1.cast$n_Region
)
stopifnot(sum(item106Q1.table[which(item106Q1.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(item106Q1.table))], na.rm = T) == 10)

levels(item106Q1.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q1.table <- item106Q1.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
item106Q1.table <- data.frame(item106Q1.table)

item106Q1.final.SF <- item106Q1.table[which(item106Q1.table$BuildingType == "Single Family")
                                  ,-which(colnames(item106Q1.table) %in% c("BuildingType"))]
item106Q1.final.MH <- item106Q1.table[which(item106Q1.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item106Q1.table) %in% c("BuildingType"))]


################################################################################
# For Multifamily
################################################################################
item106Q1.table.MF <- proportions_one_group(CustomerLevelData    = item106Q1.data
                                          ,valueVariable    = 'count'
                                          ,groupingVariable = 'GPM_bins'
                                          ,total.name       = "Remove"
                                          ,weighted = FALSE)
# item106Q1.table.MF <- item106Q1.table.MF[which(item106Q1.table.MF$GPM_bins != "Total"),]

unique(item106Q1.table.MF$GPM_bins)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q1.table.MF <- item106Q1.table.MF %>% mutate(GPM_bins = factor(GPM_bins, levels = rowOrder)) %>% arrange(GPM_bins)  
item106Q1.table.MF <- data.frame(item106Q1.table.MF)


item106Q1.final.MF <- item106Q1.table.MF[which(item106Q1.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(item106Q1.table.MF) %in% c("BuildingType"))]











#############################################################################################
#Item 106: DISTRIBUTION OF SHOWERHEAD FLOW RATE BY STATE (SF table 113, MH table 88)
#############################################################################################
#subset to columns needed for analysis
item106Q2.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                            ,"GPM_Measured"
                                                            ,"Fixture.Type"
                                                            ,"Primary.Shower"))]
item106Q2.dat$count <- 1

item106Q2.dat0 <- item106Q2.dat[which(item106Q2.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item106Q2.dat1 <- left_join(item106Q2.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item106Q2.dat1$GPM_Measured <- as.numeric(as.character(item106Q2.dat1$GPM_Measured))
item106Q2.dat2 <- item106Q2.dat1[which(!(is.na(item106Q2.dat1$GPM_Measured))),]
unique(item106Q2.dat2$GPM_Measured)

item106Q2.dat3 <- item106Q2.dat2[grep("shower",item106Q2.dat2$Fixture.Type, ignore.case = T),]
item106Q2.dat3 <- item106Q2.dat3[grep("secondary",item106Q2.dat3$Primary.Shower, ignore.case = T),]

item106Q2.dat4 <- summarise(group_by(item106Q2.dat3, CK_Cadmus_ID, BuildingType, State, count)
                            ,GPM.Measured.Site = mean(GPM_Measured))

item106Q2.dat4$GPM_bins <- item106Q2.dat4$GPM.Measured.Site
item106Q2.dat4$GPM_bins[which(item106Q2.dat4$GPM.Measured.Site <= 1.5)] <- "< 1.5"
item106Q2.dat4$GPM_bins[which(item106Q2.dat4$GPM.Measured.Site >  1.5 & item106Q2.dat4$GPM.Measured.Site < 2.1)] <- "1.6-2.0"
item106Q2.dat4$GPM_bins[which(item106Q2.dat4$GPM.Measured.Site >= 2.1 & item106Q2.dat4$GPM.Measured.Site < 2.6)] <- "2.1-2.5"
item106Q2.dat4$GPM_bins[which(item106Q2.dat4$GPM.Measured.Site >= 2.6 & item106Q2.dat4$GPM.Measured.Site < 3.6)] <- "2.6-3.5"
item106Q2.dat4$GPM_bins[which(item106Q2.dat4$GPM.Measured.Site >= 3.6)] <- "> 3.6"
# item106Q2.dat4$GPM_bins[which(item106Q2.dat4$GPM.Measured.Site >= 2.6)] <- "> 2.5"
unique(item106Q2.dat4$GPM_bins)

item106Q2.merge <- left_join(rbsa.dat, item106Q2.dat4)
item106Q2.merge <- item106Q2.merge[which(!is.na(item106Q2.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
item106Q2.data <- weightedData(item106Q2.merge[-which(colnames(item106Q2.merge) %in% c("GPM.Measured.Site"               
                                                                                       ,"GPM_bins"
                                                                                       ,"count"
                                                                                       ,"Primary.Shower"))])
item106Q2.data <- left_join(item106Q2.data, item106Q2.merge[which(colnames(item106Q2.merge) %in% c("CK_Cadmus_ID"
                                                                                                   ,"GPM.Measured.Site"               
                                                                                                   ,"GPM_bins"
                                                                                                   ,"count"
                                                                                                   ,"Primary.Shower"))])
#######################
# Weighted Analysis
#######################
item106Q2.final <- proportionRowsAndColumns1(CustomerLevelData = item106Q2.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable   = 'State'
                                             ,rowVariable      = 'GPM_bins'
                                             ,aggregateColumnName = "Region")

item106Q2.cast <- dcast(setDT(item106Q2.final)
                        , formula = BuildingType + GPM_bins ~ State
                        , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item106Q2.table <- data.frame("BuildingType"   = item106Q2.cast$BuildingType
                              ,"Flow.Rate.GPM"  = item106Q2.cast$GPM_bins
                              ,"Percent_ID"     = item106Q2.cast$w.percent_ID
                              ,"SE_ID"          = item106Q2.cast$w.SE_ID
                              ,"n_ID"           = item106Q2.cast$n_ID
                              ,"Percent_MT"     = item106Q2.cast$w.percent_MT
                              ,"SE_MT"          = item106Q2.cast$w.SE_MT
                              ,"n_MT"           = item106Q2.cast$n_MT
                              ,"Percent_OR"     = item106Q2.cast$w.percent_OR
                              ,"SE_OR"          = item106Q2.cast$w.SE_OR
                              ,"n_OR"           = item106Q2.cast$n_OR
                              ,"Percent_WA"     = item106Q2.cast$w.percent_WA
                              ,"SE_WA"          = item106Q2.cast$w.SE_WA
                              ,"n_WA"           = item106Q2.cast$n_WA
                              ,"Percent_Region" = item106Q2.cast$w.percent_Region
                              ,"SE_Region"      = item106Q2.cast$w.SE_Region
                              ,"n_Region"       = item106Q2.cast$n_Region
                              ,"EB_ID"          = item106Q2.cast$EB_ID
                              ,"EB_MT"          = item106Q2.cast$EB_MT
                              ,"EB_OR"          = item106Q2.cast$EB_OR
                              ,"EB_WA"          = item106Q2.cast$EB_WA
                              ,"EB_Region"      = item106Q2.cast$EB_Region
) 
#QAQC
stopifnot(sum(item106Q2.table[which(item106Q2.table$BuildingType == "Single Family")
                              ,grep("Percent",colnames(item106Q2.table))], na.rm = T) == 10)

levels(item106Q2.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q2.table <- item106Q2.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
item106Q2.table <- data.frame(item106Q2.table)

item106Q2.final.SF <- item106Q2.table[which(item106Q2.table$BuildingType == "Single Family")
                                      ,-which(colnames(item106Q2.table) %in% c("BuildingType"))]
item106Q2.final.MH <- item106Q2.table[which(item106Q2.table$BuildingType == "Manufactured")
                                      ,-which(colnames(item106Q2.table) %in% c("BuildingType"))]


################################################################################
# For Multifamily
################################################################################
item106Q2.table.MF <- proportions_one_group(CustomerLevelData    = item106Q2.data
                                            ,valueVariable    = 'count'
                                            ,groupingVariable = 'GPM_bins'
                                            ,total.name       = "Remove")
# item106Q2.table.MF <- item106Q2.table.MF[which(item106Q2.table.MF$GPM_bins != "Total"),]

unique(item106Q2.table.MF$GPM_bins)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q2.table.MF <- item106Q2.table.MF %>% mutate(GPM_bins = factor(GPM_bins, levels = rowOrder)) %>% arrange(GPM_bins)  
item106Q2.table.MF <- data.frame(item106Q2.table.MF)


item106Q2.final.MF <- item106Q2.table.MF[which(item106Q2.table.MF$BuildingType == "Multifamily")
                                         ,-which(colnames(item106Q2.table.MF) %in% c("BuildingType"))]



#######################
# Unweighted Analysis
#######################
item106Q2.final <- proportions_two_groups_unweighted(CustomerLevelData = item106Q2.data
                                                     ,valueVariable    = 'count'
                                                     ,columnVariable   = 'State'
                                                     ,rowVariable      = 'GPM_bins'
                                                     ,aggregateColumnName = "Region")

item106Q2.cast <- dcast(setDT(item106Q2.final)
                        , formula = BuildingType + GPM_bins ~ State
                        , value.var = c("Percent", "SE", "Count", "n"))


item106Q2.table <- data.frame("BuildingType"   = item106Q2.cast$BuildingType
                              ,"Flow.Rate.GPM"  = item106Q2.cast$GPM_bins
                              ,"Percent_ID"     = item106Q2.cast$Percent_ID
                              ,"SE_ID"          = item106Q2.cast$SE_ID
                              ,"n_ID"           = item106Q2.cast$n_ID
                              ,"Percent_MT"     = item106Q2.cast$Percent_MT
                              ,"SE_MT"          = item106Q2.cast$SE_MT
                              ,"n_MT"           = item106Q2.cast$n_MT
                              ,"Percent_OR"     = item106Q2.cast$Percent_OR
                              ,"SE_OR"          = item106Q2.cast$SE_OR
                              ,"n_OR"           = item106Q2.cast$n_OR
                              ,"Percent_WA"     = item106Q2.cast$Percent_WA
                              ,"SE_WA"          = item106Q2.cast$SE_WA
                              ,"n_WA"           = item106Q2.cast$n_WA
                              ,"Percent_Region" = item106Q2.cast$Percent_Region
                              ,"SE_Region"      = item106Q2.cast$SE_Region
                              ,"n_Region"       = item106Q2.cast$n_Region
)
stopifnot(sum(item106Q2.table[which(item106Q2.table$BuildingType == "Single Family")
                              ,grep("Percent",colnames(item106Q2.table))], na.rm = T) == 10)

levels(item106Q2.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q2.table <- item106Q2.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
item106Q2.table <- data.frame(item106Q2.table)

item106Q2.final.SF <- item106Q2.table[which(item106Q2.table$BuildingType == "Single Family")
                                      ,-which(colnames(item106Q2.table) %in% c("BuildingType"))]
item106Q2.final.MH <- item106Q2.table[which(item106Q2.table$BuildingType == "Manufactured")
                                      ,-which(colnames(item106Q2.table) %in% c("BuildingType"))]


################################################################################
# For Multifamily
################################################################################
item106Q2.table.MF <- proportions_one_group(CustomerLevelData    = item106Q2.data
                                            ,valueVariable    = 'count'
                                            ,groupingVariable = 'GPM_bins'
                                            ,total.name       = "Remove"
                                            ,weighted = FALSE)
# item106Q2.table.MF <- item106Q2.table.MF[which(item106Q2.table.MF$GPM_bins != "Total"),]

unique(item106Q2.table.MF$GPM_bins)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q2.table.MF <- item106Q2.table.MF %>% mutate(GPM_bins = factor(GPM_bins, levels = rowOrder)) %>% arrange(GPM_bins)  
item106Q2.table.MF <- data.frame(item106Q2.table.MF)


item106Q2.final.MF <- item106Q2.table.MF[which(item106Q2.table.MF$BuildingType == "Multifamily")
                                         ,-which(colnames(item106Q2.table.MF) %in% c("BuildingType"))]

















#############################################################################################
#Item 106: DISTRIBUTION OF SHOWERHEAD FLOW RATE BY STATE (SF table 113, MH table 88)
#############################################################################################
#subset to columns needed for analysis
item106Q3.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                            ,"GPM_Measured"
                                                            ,"Fixture.Type"
                                                            ,"Primary.Shower"))]
item106Q3.dat$count <- 1

item106Q3.dat0 <- item106Q3.dat[which(item106Q3.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item106Q3.dat1 <- left_join(item106Q3.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item106Q3.dat1$GPM_Measured <- as.numeric(as.character(item106Q3.dat1$GPM_Measured))
item106Q3.dat2 <- item106Q3.dat1[which(!(is.na(item106Q3.dat1$GPM_Measured))),]
unique(item106Q3.dat2$GPM_Measured)

item106Q3.dat3 <- item106Q3.dat2[grep("shower",item106Q3.dat2$Fixture.Type, ignore.case = T),]
item106Q3.dat3 <- item106Q3.dat3[grep("primary|all used",item106Q3.dat3$Primary.Shower, ignore.case = T),]
item106Q3.dat4 <- item106Q3.dat3
# item106Q3.dat4 <- summarise(group_by(item106Q3.dat3, CK_Cadmus_ID, BuildingType, State, count)
#                             ,GPM_Measured = mean(GPM_Measured))

item106Q3.dat4$GPM_bins <- item106Q3.dat4$GPM_Measured
item106Q3.dat4$GPM_bins[which(item106Q3.dat4$GPM_Measured <= 1.5)] <- "< 1.5"
item106Q3.dat4$GPM_bins[which(item106Q3.dat4$GPM_Measured >  1.5 & item106Q3.dat4$GPM_Measured < 2.1)] <- "1.6-2.0"
item106Q3.dat4$GPM_bins[which(item106Q3.dat4$GPM_Measured >= 2.1 & item106Q3.dat4$GPM_Measured < 2.6)] <- "2.1-2.5"
item106Q3.dat4$GPM_bins[which(item106Q3.dat4$GPM_Measured >= 2.6 & item106Q3.dat4$GPM_Measured < 3.6)] <- "2.6-3.5"
item106Q3.dat4$GPM_bins[which(item106Q3.dat4$GPM_Measured >= 3.6)] <- "> 3.6"
# item106Q3.dat4$GPM_bins[which(item106Q3.dat4$GPM_Measured >= 2.6)] <- "> 2.5"
unique(item106Q3.dat4$GPM_bins)

item106Q3.merge <- left_join(rbsa.dat, item106Q3.dat4)
item106Q3.merge <- item106Q3.merge[which(!is.na(item106Q3.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
item106Q3.data <- weightedData(item106Q3.merge[-which(colnames(item106Q3.merge) %in% c("GPM_Measured"               
                                                                                       ,"GPM_bins"
                                                                                       ,"count"
                                                                                       ,"Primary.Shower"
                                                                                       ,"Fixture.Type"))])
item106Q3.data <- left_join(item106Q3.data, item106Q3.merge[which(colnames(item106Q3.merge) %in% c("CK_Cadmus_ID"
                                                                                                   ,"GPM_Measured"               
                                                                                                   ,"GPM_bins"
                                                                                                   ,"count"
                                                                                                   ,"Primary.Shower"
                                                                                                   ,"Fixture.Type"))])
#######################
# Weighted Analysis
#######################
item106Q3.final <- proportionRowsAndColumns1(CustomerLevelData = item106Q3.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable   = 'State'
                                             ,rowVariable      = 'GPM_bins'
                                             ,aggregateColumnName = "Region")

item106Q3.cast <- dcast(setDT(item106Q3.final)
                        , formula = BuildingType + GPM_bins ~ State
                        , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item106Q3.table <- data.frame("BuildingType"   = item106Q3.cast$BuildingType
                              ,"Flow.Rate.GPM"  = item106Q3.cast$GPM_bins
                              ,"Percent_ID"     = item106Q3.cast$w.percent_ID
                              ,"SE_ID"          = item106Q3.cast$w.SE_ID
                              ,"n_ID"           = item106Q3.cast$n_ID
                              ,"Percent_MT"     = item106Q3.cast$w.percent_MT
                              ,"SE_MT"          = item106Q3.cast$w.SE_MT
                              ,"n_MT"           = item106Q3.cast$n_MT
                              ,"Percent_OR"     = item106Q3.cast$w.percent_OR
                              ,"SE_OR"          = item106Q3.cast$w.SE_OR
                              ,"n_OR"           = item106Q3.cast$n_OR
                              ,"Percent_WA"     = item106Q3.cast$w.percent_WA
                              ,"SE_WA"          = item106Q3.cast$w.SE_WA
                              ,"n_WA"           = item106Q3.cast$n_WA
                              ,"Percent_Region" = item106Q3.cast$w.percent_Region
                              ,"SE_Region"      = item106Q3.cast$w.SE_Region
                              ,"n_Region"       = item106Q3.cast$n_Region
                              ,"EB_ID"          = item106Q3.cast$EB_ID
                              ,"EB_MT"          = item106Q3.cast$EB_MT
                              ,"EB_OR"          = item106Q3.cast$EB_OR
                              ,"EB_WA"          = item106Q3.cast$EB_WA
                              ,"EB_Region"      = item106Q3.cast$EB_Region
) 
#QAQC
stopifnot(sum(item106Q3.table[which(item106Q3.table$BuildingType == "Single Family")
                              ,grep("Percent",colnames(item106Q3.table))], na.rm = T) == 10)

levels(item106Q3.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q3.table <- item106Q3.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
item106Q3.table <- data.frame(item106Q3.table)

item106Q3.final.SF <- item106Q3.table[which(item106Q3.table$BuildingType == "Single Family")
                                      ,-which(colnames(item106Q3.table) %in% c("BuildingType"))]
item106Q3.final.MH <- item106Q3.table[which(item106Q3.table$BuildingType == "Manufactured")
                                      ,-which(colnames(item106Q3.table) %in% c("BuildingType"))]


################################################################################
# For Multifamily
################################################################################
item106Q3.table.MF <- proportions_one_group(CustomerLevelData    = item106Q3.data
                                            ,valueVariable    = 'count'
                                            ,groupingVariable = 'GPM_bins'
                                            ,total.name       = "Remove")
# item106Q3.table.MF <- item106Q3.table.MF[which(item106Q3.table.MF$GPM_bins != "Total"),]

unique(item106Q3.table.MF$GPM_bins)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q3.table.MF <- item106Q3.table.MF %>% mutate(GPM_bins = factor(GPM_bins, levels = rowOrder)) %>% arrange(GPM_bins)  
item106Q3.table.MF <- data.frame(item106Q3.table.MF)


item106Q3.final.MF <- item106Q3.table.MF[which(item106Q3.table.MF$BuildingType == "Multifamily")
                                         ,-which(colnames(item106Q3.table.MF) %in% c("BuildingType"))]



#######################
# Unweighted Analysis
#######################
item106Q3.final <- proportions_two_groups_unweighted(CustomerLevelData = item106Q3.data
                                                     ,valueVariable    = 'count'
                                                     ,columnVariable   = 'State'
                                                     ,rowVariable      = 'GPM_bins'
                                                     ,aggregateColumnName = "Region")

item106Q3.cast <- dcast(setDT(item106Q3.final)
                        , formula = BuildingType + GPM_bins ~ State
                        , value.var = c("Percent", "SE", "Count", "n"))


item106Q3.table <- data.frame("BuildingType"   = item106Q3.cast$BuildingType
                              ,"Flow.Rate.GPM"  = item106Q3.cast$GPM_bins
                              ,"Percent_ID"     = item106Q3.cast$Percent_ID
                              ,"SE_ID"          = item106Q3.cast$SE_ID
                              ,"n_ID"           = item106Q3.cast$n_ID
                              ,"Percent_MT"     = item106Q3.cast$Percent_MT
                              ,"SE_MT"          = item106Q3.cast$SE_MT
                              ,"n_MT"           = item106Q3.cast$n_MT
                              ,"Percent_OR"     = item106Q3.cast$Percent_OR
                              ,"SE_OR"          = item106Q3.cast$SE_OR
                              ,"n_OR"           = item106Q3.cast$n_OR
                              ,"Percent_WA"     = item106Q3.cast$Percent_WA
                              ,"SE_WA"          = item106Q3.cast$SE_WA
                              ,"n_WA"           = item106Q3.cast$n_WA
                              ,"Percent_Region" = item106Q3.cast$Percent_Region
                              ,"SE_Region"      = item106Q3.cast$SE_Region
                              ,"n_Region"       = item106Q3.cast$n_Region
)
stopifnot(sum(item106Q3.table[which(item106Q3.table$BuildingType == "Single Family")
                              ,grep("Percent",colnames(item106Q3.table))], na.rm = T) == 10)

levels(item106Q3.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q3.table <- item106Q3.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
item106Q3.table <- data.frame(item106Q3.table)

item106Q3.final.SF <- item106Q3.table[which(item106Q3.table$BuildingType == "Single Family")
                                      ,-which(colnames(item106Q3.table) %in% c("BuildingType"))]
item106Q3.final.MH <- item106Q3.table[which(item106Q3.table$BuildingType == "Manufactured")
                                      ,-which(colnames(item106Q3.table) %in% c("BuildingType"))]


################################################################################
# For Multifamily
################################################################################
item106Q3.table.MF <- proportions_one_group(CustomerLevelData    = item106Q3.data
                                            ,valueVariable    = 'count'
                                            ,groupingVariable = 'GPM_bins'
                                            ,total.name       = "Remove"
                                            ,weighted = FALSE)
# item106Q3.table.MF <- item106Q3.table.MF[which(item106Q3.table.MF$GPM_bins != "Total"),]

unique(item106Q3.table.MF$GPM_bins)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q3.table.MF <- item106Q3.table.MF %>% mutate(GPM_bins = factor(GPM_bins, levels = rowOrder)) %>% arrange(GPM_bins)  
item106Q3.table.MF <- data.frame(item106Q3.table.MF)


item106Q3.final.MF <- item106Q3.table.MF[which(item106Q3.table.MF$BuildingType == "Multifamily")
                                         ,-which(colnames(item106Q3.table.MF) %in% c("BuildingType"))]











#############################################################################################
#Item 106: DISTRIBUTION OF SHOWERHEAD FLOW RATE BY STATE (SF table 113, MH table 88)
#############################################################################################
#subset to columns needed for analysis
item106Q4.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                            ,"GPM_Measured"
                                                            ,"Fixture.Type"
                                                            ,"Primary.Shower"))]
item106Q4.dat$count <- 1

item106Q4.dat0 <- item106Q4.dat[which(item106Q4.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item106Q4.dat1 <- left_join(item106Q4.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item106Q4.dat1$GPM_Measured <- as.numeric(as.character(item106Q4.dat1$GPM_Measured))
item106Q4.dat2 <- item106Q4.dat1[which(!(is.na(item106Q4.dat1$GPM_Measured))),]
unique(item106Q4.dat2$GPM_Measured)

item106Q4.dat3 <- item106Q4.dat2[grep("shower",item106Q4.dat2$Fixture.Type, ignore.case = T),]
item106Q4.dat3 <- item106Q4.dat3[grep("secondary",item106Q4.dat3$Primary.Shower, ignore.case = T),]
item106Q4.dat4 <- item106Q4.dat3
# item106Q4.dat4 <- summarise(group_by(item106Q4.dat3, CK_Cadmus_ID, BuildingType, State, count)
#                             ,GPM_Measured = mean(GPM_Measured))

item106Q4.dat4$GPM_bins <- item106Q4.dat4$GPM_Measured
item106Q4.dat4$GPM_bins[which(item106Q4.dat4$GPM_Measured <= 1.5)] <- "< 1.5"
item106Q4.dat4$GPM_bins[which(item106Q4.dat4$GPM_Measured >  1.5 & item106Q4.dat4$GPM_Measured < 2.1)] <- "1.6-2.0"
item106Q4.dat4$GPM_bins[which(item106Q4.dat4$GPM_Measured >= 2.1 & item106Q4.dat4$GPM_Measured < 2.6)] <- "2.1-2.5"
item106Q4.dat4$GPM_bins[which(item106Q4.dat4$GPM_Measured >= 2.6 & item106Q4.dat4$GPM_Measured < 3.6)] <- "2.6-3.5"
item106Q4.dat4$GPM_bins[which(item106Q4.dat4$GPM_Measured >= 3.6)] <- "> 3.6"
# item106Q4.dat4$GPM_bins[which(item106Q4.dat4$GPM_Measured >= 2.6)] <- "> 2.5"
unique(item106Q4.dat4$GPM_bins)

item106Q4.merge <- left_join(rbsa.dat, item106Q4.dat4)
item106Q4.merge <- item106Q4.merge[which(!is.na(item106Q4.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
item106Q4.data <- weightedData(item106Q4.merge[-which(colnames(item106Q4.merge) %in% c("GPM_Measured"               
                                                                                       ,"GPM_bins"
                                                                                       ,"count"
                                                                                       ,"Primary.Shower"
                                                                                       ,"Fixture.Type"))])
item106Q4.data <- left_join(item106Q4.data, item106Q4.merge[which(colnames(item106Q4.merge) %in% c("CK_Cadmus_ID"
                                                                                                   ,"GPM_Measured"               
                                                                                                   ,"GPM_bins"
                                                                                                   ,"count"
                                                                                                   ,"Primary.Shower"
                                                                                                   ,"Fixture.Type"))])
#######################
# Weighted Analysis
#######################
item106Q4.final <- proportionRowsAndColumns1(CustomerLevelData = item106Q4.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable   = 'State'
                                             ,rowVariable      = 'GPM_bins'
                                             ,aggregateColumnName = "Region")

item106Q4.cast <- dcast(setDT(item106Q4.final)
                        , formula = BuildingType + GPM_bins ~ State
                        , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item106Q4.table <- data.frame("BuildingType"   = item106Q4.cast$BuildingType
                              ,"Flow.Rate.GPM"  = item106Q4.cast$GPM_bins
                              ,"Percent_ID"     = item106Q4.cast$w.percent_ID
                              ,"SE_ID"          = item106Q4.cast$w.SE_ID
                              ,"n_ID"           = item106Q4.cast$n_ID
                              ,"Percent_MT"     = item106Q4.cast$w.percent_MT
                              ,"SE_MT"          = item106Q4.cast$w.SE_MT
                              ,"n_MT"           = item106Q4.cast$n_MT
                              ,"Percent_OR"     = item106Q4.cast$w.percent_OR
                              ,"SE_OR"          = item106Q4.cast$w.SE_OR
                              ,"n_OR"           = item106Q4.cast$n_OR
                              ,"Percent_WA"     = item106Q4.cast$w.percent_WA
                              ,"SE_WA"          = item106Q4.cast$w.SE_WA
                              ,"n_WA"           = item106Q4.cast$n_WA
                              ,"Percent_Region" = item106Q4.cast$w.percent_Region
                              ,"SE_Region"      = item106Q4.cast$w.SE_Region
                              ,"n_Region"       = item106Q4.cast$n_Region
                              ,"EB_ID"          = item106Q4.cast$EB_ID
                              ,"EB_MT"          = item106Q4.cast$EB_MT
                              ,"EB_OR"          = item106Q4.cast$EB_OR
                              ,"EB_WA"          = item106Q4.cast$EB_WA
                              ,"EB_Region"      = item106Q4.cast$EB_Region
) 
#QAQC
stopifnot(sum(item106Q4.table[which(item106Q4.table$BuildingType == "Single Family")
                              ,grep("Percent",colnames(item106Q4.table))], na.rm = T) == 10)

levels(item106Q4.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q4.table <- item106Q4.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
item106Q4.table <- data.frame(item106Q4.table)

item106Q4.final.SF <- item106Q4.table[which(item106Q4.table$BuildingType == "Single Family")
                                      ,-which(colnames(item106Q4.table) %in% c("BuildingType"))]
item106Q4.final.MH <- item106Q4.table[which(item106Q4.table$BuildingType == "Manufactured")
                                      ,-which(colnames(item106Q4.table) %in% c("BuildingType"))]


################################################################################
# For Multifamily
################################################################################
item106Q4.table.MF <- proportions_one_group(CustomerLevelData    = item106Q4.data
                                            ,valueVariable    = 'count'
                                            ,groupingVariable = 'GPM_bins'
                                            ,total.name       = "Remove")
# item106Q4.table.MF <- item106Q4.table.MF[which(item106Q4.table.MF$GPM_bins != "Total"),]

unique(item106Q4.table.MF$GPM_bins)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q4.table.MF <- item106Q4.table.MF %>% mutate(GPM_bins = factor(GPM_bins, levels = rowOrder)) %>% arrange(GPM_bins)  
item106Q4.table.MF <- data.frame(item106Q4.table.MF)


item106Q4.final.MF <- item106Q4.table.MF[which(item106Q4.table.MF$BuildingType == "Multifamily")
                                         ,-which(colnames(item106Q4.table.MF) %in% c("BuildingType"))]



#######################
# Unweighted Analysis
#######################
item106Q4.final <- proportions_two_groups_unweighted(CustomerLevelData = item106Q4.data
                                                     ,valueVariable    = 'count'
                                                     ,columnVariable   = 'State'
                                                     ,rowVariable      = 'GPM_bins'
                                                     ,aggregateColumnName = "Region")

item106Q4.cast <- dcast(setDT(item106Q4.final)
                        , formula = BuildingType + GPM_bins ~ State
                        , value.var = c("Percent", "SE", "Count", "n"))


item106Q4.table <- data.frame("BuildingType"   = item106Q4.cast$BuildingType
                              ,"Flow.Rate.GPM"  = item106Q4.cast$GPM_bins
                              ,"Percent_ID"     = item106Q4.cast$Percent_ID
                              ,"SE_ID"          = item106Q4.cast$SE_ID
                              ,"n_ID"           = item106Q4.cast$n_ID
                              ,"Percent_MT"     = item106Q4.cast$Percent_MT
                              ,"SE_MT"          = item106Q4.cast$SE_MT
                              ,"n_MT"           = item106Q4.cast$n_MT
                              ,"Percent_OR"     = item106Q4.cast$Percent_OR
                              ,"SE_OR"          = item106Q4.cast$SE_OR
                              ,"n_OR"           = item106Q4.cast$n_OR
                              ,"Percent_WA"     = item106Q4.cast$Percent_WA
                              ,"SE_WA"          = item106Q4.cast$SE_WA
                              ,"n_WA"           = item106Q4.cast$n_WA
                              ,"Percent_Region" = item106Q4.cast$Percent_Region
                              ,"SE_Region"      = item106Q4.cast$SE_Region
                              ,"n_Region"       = item106Q4.cast$n_Region
)
stopifnot(sum(item106Q4.table[which(item106Q4.table$BuildingType == "Single Family")
                              ,grep("Percent",colnames(item106Q4.table))], na.rm = T) == 10)

levels(item106Q4.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q4.table <- item106Q4.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
item106Q4.table <- data.frame(item106Q4.table)

item106Q4.final.SF <- item106Q4.table[which(item106Q4.table$BuildingType == "Single Family")
                                      ,-which(colnames(item106Q4.table) %in% c("BuildingType"))]
item106Q4.final.MH <- item106Q4.table[which(item106Q4.table$BuildingType == "Manufactured")
                                      ,-which(colnames(item106Q4.table) %in% c("BuildingType"))]


################################################################################
# For Multifamily
################################################################################
item106Q4.table.MF <- proportions_one_group(CustomerLevelData    = item106Q4.data
                                            ,valueVariable    = 'count'
                                            ,groupingVariable = 'GPM_bins'
                                            ,total.name       = "Remove"
                                            ,weighted = FALSE)
# item106Q4.table.MF <- item106Q4.table.MF[which(item106Q4.table.MF$GPM_bins != "Total"),]

unique(item106Q4.table.MF$GPM_bins)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q4.table.MF <- item106Q4.table.MF %>% mutate(GPM_bins = factor(GPM_bins, levels = rowOrder)) %>% arrange(GPM_bins)  
item106Q4.table.MF <- data.frame(item106Q4.table.MF)


item106Q4.final.MF <- item106Q4.table.MF[which(item106Q4.table.MF$BuildingType == "Multifamily")
                                         ,-which(colnames(item106Q4.table.MF) %in% c("BuildingType"))]




#############################################################################################
#Item 106: DISTRIBUTION OF SHOWERHEAD FLOW RATE BY STATE (SF table 113, MH table 88)
#############################################################################################
#subset to columns needed for analysis
item106Q5.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                            ,"GPM_Measured"
                                                            ,"Fixture.Type"
                                                            ,"Primary.Shower"))]
item106Q5.dat$count <- 1

item106Q5.dat0 <- item106Q5.dat[which(item106Q5.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item106Q5.dat1 <- left_join(item106Q5.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item106Q5.dat1$GPM_Measured <- as.numeric(as.character(item106Q5.dat1$GPM_Measured))
item106Q5.dat2 <- item106Q5.dat1[which(!(is.na(item106Q5.dat1$GPM_Measured))),]
unique(item106Q5.dat2$GPM_Measured)

item106Q5.dat3 <- item106Q5.dat2[grep("shower",item106Q5.dat2$Fixture.Type, ignore.case = T),]
item106Q5.dat3 <- item106Q5.dat3[grep("primary",item106Q5.dat3$Primary.Shower, ignore.case = T),]
# item106Q5.dat4 <- item106Q5.dat3
item106Q5.dat4 <- summarise(group_by(item106Q5.dat3, CK_Cadmus_ID, BuildingType, State, count)
                            ,GPM_Measured = mean(GPM_Measured))

item106Q5.dat4$GPM_bins <- item106Q5.dat4$GPM_Measured
item106Q5.dat4$GPM_bins[which(item106Q5.dat4$GPM_Measured <= 1.5)] <- "< 1.5"
item106Q5.dat4$GPM_bins[which(item106Q5.dat4$GPM_Measured >  1.5 & item106Q5.dat4$GPM_Measured < 2.1)] <- "1.6-2.0"
item106Q5.dat4$GPM_bins[which(item106Q5.dat4$GPM_Measured >= 2.1 & item106Q5.dat4$GPM_Measured < 2.6)] <- "2.1-2.5"
item106Q5.dat4$GPM_bins[which(item106Q5.dat4$GPM_Measured >= 2.6 & item106Q5.dat4$GPM_Measured < 3.6)] <- "2.6-3.5"
item106Q5.dat4$GPM_bins[which(item106Q5.dat4$GPM_Measured >= 3.6)] <- "> 3.6"
# item106Q5.dat4$GPM_bins[which(item106Q5.dat4$GPM_Measured >= 2.6)] <- "> 2.5"
unique(item106Q5.dat4$GPM_bins)

item106Q5.merge <- left_join(rbsa.dat, item106Q5.dat4)
item106Q5.merge <- item106Q5.merge[which(!is.na(item106Q5.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
item106Q5.data <- weightedData(item106Q5.merge[-which(colnames(item106Q5.merge) %in% c("GPM_Measured"               
                                                                                       ,"GPM_bins"
                                                                                       ,"count"
                                                                                       ,"Primary.Shower"
                                                                                       ,"Fixture.Type"))])
item106Q5.data <- left_join(item106Q5.data, item106Q5.merge[which(colnames(item106Q5.merge) %in% c("CK_Cadmus_ID"
                                                                                                   ,"GPM_Measured"               
                                                                                                   ,"GPM_bins"
                                                                                                   ,"count"
                                                                                                   ,"Primary.Shower"
                                                                                                   ,"Fixture.Type"))])
#######################
# Weighted Analysis
#######################
item106Q5.final <- proportionRowsAndColumns1(CustomerLevelData = item106Q5.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable   = 'State'
                                             ,rowVariable      = 'GPM_bins'
                                             ,aggregateColumnName = "Region")

item106Q5.cast <- dcast(setDT(item106Q5.final)
                        , formula = BuildingType + GPM_bins ~ State
                        , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item106Q5.table <- data.frame("BuildingType"   = item106Q5.cast$BuildingType
                              ,"Flow.Rate.GPM"  = item106Q5.cast$GPM_bins
                              ,"Percent_ID"     = item106Q5.cast$w.percent_ID
                              ,"SE_ID"          = item106Q5.cast$w.SE_ID
                              ,"n_ID"           = item106Q5.cast$n_ID
                              ,"Percent_MT"     = item106Q5.cast$w.percent_MT
                              ,"SE_MT"          = item106Q5.cast$w.SE_MT
                              ,"n_MT"           = item106Q5.cast$n_MT
                              ,"Percent_OR"     = item106Q5.cast$w.percent_OR
                              ,"SE_OR"          = item106Q5.cast$w.SE_OR
                              ,"n_OR"           = item106Q5.cast$n_OR
                              ,"Percent_WA"     = item106Q5.cast$w.percent_WA
                              ,"SE_WA"          = item106Q5.cast$w.SE_WA
                              ,"n_WA"           = item106Q5.cast$n_WA
                              ,"Percent_Region" = item106Q5.cast$w.percent_Region
                              ,"SE_Region"      = item106Q5.cast$w.SE_Region
                              ,"n_Region"       = item106Q5.cast$n_Region
                              ,"EB_ID"          = item106Q5.cast$EB_ID
                              ,"EB_MT"          = item106Q5.cast$EB_MT
                              ,"EB_OR"          = item106Q5.cast$EB_OR
                              ,"EB_WA"          = item106Q5.cast$EB_WA
                              ,"EB_Region"      = item106Q5.cast$EB_Region
) 
#QAQC
stopifnot(sum(item106Q5.table[which(item106Q5.table$BuildingType == "Single Family")
                              ,grep("Percent",colnames(item106Q5.table))], na.rm = T) == 10)

levels(item106Q5.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q5.table <- item106Q5.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
item106Q5.table <- data.frame(item106Q5.table)

item106Q5.final.SF <- item106Q5.table[which(item106Q5.table$BuildingType == "Single Family")
                                      ,-which(colnames(item106Q5.table) %in% c("BuildingType"))]
item106Q5.final.MH <- item106Q5.table[which(item106Q5.table$BuildingType == "Manufactured")
                                      ,-which(colnames(item106Q5.table) %in% c("BuildingType"))]


################################################################################
# For Multifamily
################################################################################
item106Q5.table.MF <- proportions_one_group(CustomerLevelData    = item106Q5.data
                                            ,valueVariable    = 'count'
                                            ,groupingVariable = 'GPM_bins'
                                            ,total.name       = "Remove")
# item106Q5.table.MF <- item106Q5.table.MF[which(item106Q5.table.MF$GPM_bins != "Total"),]

unique(item106Q5.table.MF$GPM_bins)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q5.table.MF <- item106Q5.table.MF %>% mutate(GPM_bins = factor(GPM_bins, levels = rowOrder)) %>% arrange(GPM_bins)  
item106Q5.table.MF <- data.frame(item106Q5.table.MF)


item106Q5.final.MF <- item106Q5.table.MF[which(item106Q5.table.MF$BuildingType == "Multifamily")
                                         ,-which(colnames(item106Q5.table.MF) %in% c("BuildingType"))]



#######################
# Unweighted Analysis
#######################
item106Q5.final <- proportions_two_groups_unweighted(CustomerLevelData = item106Q5.data
                                                     ,valueVariable    = 'count'
                                                     ,columnVariable   = 'State'
                                                     ,rowVariable      = 'GPM_bins'
                                                     ,aggregateColumnName = "Region")

item106Q5.cast <- dcast(setDT(item106Q5.final)
                        , formula = BuildingType + GPM_bins ~ State
                        , value.var = c("Percent", "SE", "Count", "n"))


item106Q5.table <- data.frame("BuildingType"   = item106Q5.cast$BuildingType
                              ,"Flow.Rate.GPM"  = item106Q5.cast$GPM_bins
                              ,"Percent_ID"     = item106Q5.cast$Percent_ID
                              ,"SE_ID"          = item106Q5.cast$SE_ID
                              ,"n_ID"           = item106Q5.cast$n_ID
                              ,"Percent_MT"     = item106Q5.cast$Percent_MT
                              ,"SE_MT"          = item106Q5.cast$SE_MT
                              ,"n_MT"           = item106Q5.cast$n_MT
                              ,"Percent_OR"     = item106Q5.cast$Percent_OR
                              ,"SE_OR"          = item106Q5.cast$SE_OR
                              ,"n_OR"           = item106Q5.cast$n_OR
                              ,"Percent_WA"     = item106Q5.cast$Percent_WA
                              ,"SE_WA"          = item106Q5.cast$SE_WA
                              ,"n_WA"           = item106Q5.cast$n_WA
                              ,"Percent_Region" = item106Q5.cast$Percent_Region
                              ,"SE_Region"      = item106Q5.cast$SE_Region
                              ,"n_Region"       = item106Q5.cast$n_Region
)
stopifnot(sum(item106Q5.table[which(item106Q5.table$BuildingType == "Single Family")
                              ,grep("Percent",colnames(item106Q5.table))], na.rm = T) == 10)

levels(item106Q5.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q5.table <- item106Q5.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
item106Q5.table <- data.frame(item106Q5.table)

item106Q5.final.SF <- item106Q5.table[which(item106Q5.table$BuildingType == "Single Family")
                                      ,-which(colnames(item106Q5.table) %in% c("BuildingType"))]
item106Q5.final.MH <- item106Q5.table[which(item106Q5.table$BuildingType == "Manufactured")
                                      ,-which(colnames(item106Q5.table) %in% c("BuildingType"))]


################################################################################
# For Multifamily
################################################################################
item106Q5.table.MF <- proportions_one_group(CustomerLevelData    = item106Q5.data
                                            ,valueVariable    = 'count'
                                            ,groupingVariable = 'GPM_bins'
                                            ,total.name       = "Remove"
                                            ,weighted = FALSE)
# item106Q5.table.MF <- item106Q5.table.MF[which(item106Q5.table.MF$GPM_bins != "Total"),]

unique(item106Q5.table.MF$GPM_bins)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106Q5.table.MF <- item106Q5.table.MF %>% mutate(GPM_bins = factor(GPM_bins, levels = rowOrder)) %>% arrange(GPM_bins)  
item106Q5.table.MF <- data.frame(item106Q5.table.MF)


item106Q5.final.MF <- item106Q5.table.MF[which(item106Q5.table.MF$BuildingType == "Multifamily")
                                         ,-which(colnames(item106Q5.table.MF) %in% c("BuildingType"))]

