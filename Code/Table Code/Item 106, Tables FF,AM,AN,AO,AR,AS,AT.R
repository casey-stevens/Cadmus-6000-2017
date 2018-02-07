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
exportTable(item106.final.MH, "MH", "Table 88", weighted = TRUE)

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

# exportTable(item106.final.SF, "SF", "Table 113", weighted = FALSE)
exportTable(item106.final.MH, "MH", "Table 88", weighted = FALSE)


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

################################################################################
# For Multifamily
################################################################################
tableFF.table.MF <- proportions_one_group(CustomerLevelData = tableFF.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = TRUE)
tableFF.table.MF <- tableFF.table.MF[which(tableFF.table.MF$State == "Total"),]
tableFF.table.MF$State[which(tableFF.table.MF$State == "Total")] <- "Region"

tableFF.final.MF <- tableFF.table.MF[which(tableFF.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableFF.table.MF) %in% c("BuildingType"))]
exportTable(tableFF.final.MF, "MF", "Table FF", weighted = TRUE)



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


################################################################################
# For Multifamily
################################################################################
tableFF.table.MF <- proportions_one_group(CustomerLevelData = tableFF.data
                                          ,valueVariable = "Ind"
                                          ,groupingVariable = "State"
                                          ,total.name = "Region"
                                          ,weighted = FALSE)
tableFF.table.MF <- tableFF.table.MF[which(tableFF.table.MF$State == "Total"),]
tableFF.table.MF$State[which(tableFF.table.MF$State == "Total")] <- "Region"

tableFF.final.MF <- tableFF.table.MF[which(tableFF.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableFF.table.MF) %in% c("BuildingType"))]
exportTable(tableFF.final.MF, "MF", "Table FF", weighted = FALSE)








# #############################################################################################
# #Table AN: DISTRIBUTION OF KITCHEN FAUCET FLOW RATE BY STATE (SF table 113, MH table 88)
# #############################################################################################
# #subset to columns needed for analysis
# tableAN.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
#                                                           ,"GPM_Measured"
#                                                           ,"Fixture.Type"))]
# tableAN.dat$count <- 1
# 
# tableAN.dat0 <- tableAN.dat[which(tableAN.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# tableAN.dat1 <- left_join(tableAN.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# 
# tableAN.dat1$GPM_Measured <- as.numeric(as.character(tableAN.dat1$GPM_Measured))
# tableAN.dat2 <- tableAN.dat1[which(!(is.na(tableAN.dat1$GPM_Measured))),]
# unique(tableAN.dat2$GPM_Measured)
# 
# tableAN.dat3 <- tableAN.dat2[grep("kitchen",tableAN.dat2$Fixture.Type,ignore.case = T),]
# 
# tableAN.dat4 <- summarise(group_by(tableAN.dat3, CK_Cadmus_ID)
#                           ,GPM.Measured.Site = mean(GPM_Measured))
# 
# tableAN.dat4$GPM_bins <- tableAN.dat4$GPM.Measured.Site
# tableAN.dat4$GPM_bins[which(tableAN.dat4$GPM.Measured.Site <= 1.5)] <- "< 1.5"
# tableAN.dat4$GPM_bins[which(tableAN.dat4$GPM.Measured.Site >  1.5 & tableAN.dat4$GPM.Measured.Site < 2.1)] <- "1.6-2.0"
# tableAN.dat4$GPM_bins[which(tableAN.dat4$GPM.Measured.Site >= 2.1 & tableAN.dat4$GPM.Measured.Site < 2.6)] <- "2.1-2.5"
# tableAN.dat4$GPM_bins[which(tableAN.dat4$GPM.Measured.Site >= 2.6 & tableAN.dat4$GPM.Measured.Site < 3.6)] <- "2.6-3.5"
# tableAN.dat4$GPM_bins[which(tableAN.dat4$GPM.Measured.Site >= 3.6)] <- "> 3.6"
# unique(tableAN.dat4$GPM_bins)
# 
# tableAN.merge <- left_join(rbsa.dat, tableAN.dat4)
# tableAN.merge <- tableAN.merge[which(!is.na(tableAN.merge$GPM_bins)),]
# 
# 
# 
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# tableAN.data <- weightedData(tableAN.merge[-which(colnames(tableAN.merge) %in% c("GPM.Measured.Site"               
#                                                                                  ,"GPM_bins"
#                                                                                  ,"count"))])
# tableAN.data <- left_join(tableAN.data, tableAN.merge[which(colnames(tableAN.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"GPM.Measured.Site"               
#                                                                                            ,"GPM_bins"
#                                                                                            ,"count"))])
# tableAN.data$count <- 1
# #######################
# # Weighted Analysis
# #######################
# tableAN.final <- proportionRowsAndColumns1(CustomerLevelData = tableAN.data
#                                            ,valueVariable    = 'count'
#                                            ,columnVariable   = 'State'
#                                            ,rowVariable      = 'GPM_bins'
#                                            ,aggregateColumnName = "Region")
# 
# tableAN.cast <- dcast(setDT(tableAN.final)
#                       , formula = BuildingType + GPM_bins ~ State
#                       , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
# 
# tableAN.table <- data.frame("BuildingType"   = tableAN.cast$BuildingType
#                             ,"Flow.Rate.GPM"  = tableAN.cast$GPM_bins
#                             ,"Percent_ID"     = tableAN.cast$w.percent_ID
#                             ,"SE_ID"          = tableAN.cast$w.SE_ID
#                             ,"n_ID"           = tableAN.cast$n_ID
#                             ,"Percent_MT"     = tableAN.cast$w.percent_MT
#                             ,"SE_MT"          = tableAN.cast$w.SE_MT
#                             ,"n_MT"           = tableAN.cast$n_MT
#                             ,"Percent_OR"     = tableAN.cast$w.percent_OR
#                             ,"SE_OR"          = tableAN.cast$w.SE_OR
#                             ,"n_OR"           = tableAN.cast$n_OR
#                             ,"Percent_WA"     = tableAN.cast$w.percent_WA
#                             ,"SE_WA"          = tableAN.cast$w.SE_WA
#                             ,"n_WA"           = tableAN.cast$n_WA
#                             ,"Percent_Region" = tableAN.cast$w.percent_Region
#                             ,"SE_Region"      = tableAN.cast$w.SE_Region
#                             ,"n_Region"       = tableAN.cast$n_Region
#                             ,"EB_ID"          = tableAN.cast$EB_ID
#                             ,"EB_MT"          = tableAN.cast$EB_MT
#                             ,"EB_OR"          = tableAN.cast$EB_OR
#                             ,"EB_WA"          = tableAN.cast$EB_WA
#                             ,"EB_Region"      = tableAN.cast$EB_Region
# ) 
# #QAQC
# stopifnot(sum(tableAN.table[which(tableAN.table$BuildingType == "Single Family")
#                             ,grep("Percent",colnames(tableAN.table))], na.rm = T) == 10)
# 
# levels(tableAN.table$Flow.Rate.GPM)
# rowOrder <- c("< 1.5"
#               ,"1.6-2.0"
#               ,"2.1-2.5"
#               ,"2.6-3.5"
#               ,"> 3.6"
#               ,"Total")
# tableAN.table <- tableAN.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
# tableAN.table <- data.frame(tableAN.table)
# 
# tableAN.final.SF <- tableAN.table[which(tableAN.table$BuildingType == "Single Family")
#                                   ,-which(colnames(tableAN.table) %in% c("BuildingType"))]
# tableAN.final.MH <- tableAN.table[which(tableAN.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(tableAN.table) %in% c("BuildingType"))]
# tableAN.final.MF <- tableAN.table[which(tableAN.table$BuildingType == "Multifamily")
#                                   ,-which(colnames(tableAN.table) %in% c("BuildingType"))]
# 
# # exportTable(tableAN.final.SF, "SF", "Table AN", weighted = TRUE)
# exportTable(tableAN.final.MH, "MH", "Table AN", weighted = TRUE)
# # exportTable(tableAN.final.MF, "MF", "Table AN", weighted = TRUE)
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# tableAN.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAN.data
#                                                    ,valueVariable    = 'count'
#                                                    ,columnVariable   = 'State'
#                                                    ,rowVariable      = 'GPM_bins'
#                                                    ,aggregateColumnName = "Region")
# 
# tableAN.cast <- dcast(setDT(tableAN.final)
#                       , formula = BuildingType + GPM_bins ~ State
#                       , value.var = c("Percent", "SE", "Count", "n"))
# 
# 
# tableAN.table <- data.frame("BuildingType"   = tableAN.cast$BuildingType
#                             ,"Flow.Rate.GPM"  = tableAN.cast$GPM_bins
#                             ,"Percent_ID"     = tableAN.cast$Percent_ID
#                             ,"SE_ID"          = tableAN.cast$SE_ID
#                             ,"n_ID"           = tableAN.cast$n_ID
#                             ,"Percent_MT"     = tableAN.cast$Percent_MT
#                             ,"SE_MT"          = tableAN.cast$SE_MT
#                             ,"n_MT"           = tableAN.cast$n_MT
#                             ,"Percent_OR"     = tableAN.cast$Percent_OR
#                             ,"SE_OR"          = tableAN.cast$SE_OR
#                             ,"n_OR"           = tableAN.cast$n_OR
#                             ,"Percent_WA"     = tableAN.cast$Percent_WA
#                             ,"SE_WA"          = tableAN.cast$SE_WA
#                             ,"n_WA"           = tableAN.cast$n_WA
#                             ,"Percent_Region" = tableAN.cast$Percent_Region
#                             ,"SE_Region"      = tableAN.cast$SE_Region
#                             ,"n_Region"       = tableAN.cast$n_Region
# )
# stopifnot(sum(tableAN.table[which(tableAN.table$BuildingType == "Single Family")
#                             ,grep("Percent",colnames(tableAN.table))], na.rm = T) == 10)
# 
# levels(tableAN.table$Flow.Rate.GPM)
# rowOrder <- c("< 1.5"
#               ,"1.6-2.0"
#               ,"2.1-2.5"
#               ,"2.6-3.5"
#               ,"> 3.6"
#               ,"Total")
# tableAN.table <- tableAN.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
# tableAN.table <- data.frame(tableAN.table)
# 
# tableAN.final.SF <- tableAN.table[which(tableAN.table$BuildingType == "Single Family")
#                                   ,-which(colnames(tableAN.table) %in% c("BuildingType"))]
# tableAN.final.MH <- tableAN.table[which(tableAN.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(tableAN.table) %in% c("BuildingType"))]
# tableAN.final.MF <- tableAN.table[which(tableAN.table$BuildingType == "Multifamily")
#                                   ,-which(colnames(tableAN.table) %in% c("BuildingType"))]
# 
# # exportTable(tableAN.final.SF, "SF", "Table AN", weighted = FALSE)
# exportTable(tableAN.final.MH, "MH", "Table AN", weighted = FALSE)
# # exportTable(tableAN.final.MF, "MF", "Table AN", weighted = FALSE)
# 
# 
# 
# 
# 
# #############################################################################################
# #Table AO: DISTRIBUTION OF BATHROOM FAUCET FLOW RATE BY STATE (SF table 113, MH table 88)
# #############################################################################################
# #subset to columns needed for analysis
# tableAO.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
#                                                           ,"GPM_Measured"
#                                                           ,"Fixture.Type"))]
# tableAO.dat$count <- 1
# 
# tableAO.dat0 <- tableAO.dat[which(tableAO.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# tableAO.dat1 <- left_join(tableAO.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# 
# tableAO.dat1$GPM_Measured <- as.numeric(as.character(tableAO.dat1$GPM_Measured))
# tableAO.dat2 <- tableAO.dat1[which(!(is.na(tableAO.dat1$GPM_Measured))),]
# unique(tableAO.dat2$GPM_Measured)
# 
# tableAO.dat3 <- tableAO.dat2[grep("bathroom",tableAO.dat2$Fixture.Type,ignore.case = T),]
# 
# tableAO.dat4 <- summarise(group_by(tableAO.dat3, CK_Cadmus_ID)
#                           ,GPM.Measured.Site = mean(GPM_Measured))
# 
# tableAO.dat4$GPM_bins <- tableAO.dat4$GPM.Measured.Site
# tableAO.dat4$GPM_bins[which(tableAO.dat4$GPM.Measured.Site <= 1.5)] <- "< 1.5"
# tableAO.dat4$GPM_bins[which(tableAO.dat4$GPM.Measured.Site >  1.5 & tableAO.dat4$GPM.Measured.Site < 2.1)] <- "1.6-2.0"
# tableAO.dat4$GPM_bins[which(tableAO.dat4$GPM.Measured.Site >= 2.1 & tableAO.dat4$GPM.Measured.Site < 2.6)] <- "2.1-2.5"
# tableAO.dat4$GPM_bins[which(tableAO.dat4$GPM.Measured.Site >= 2.6 & tableAO.dat4$GPM.Measured.Site < 3.6)] <- "2.6-3.5"
# tableAO.dat4$GPM_bins[which(tableAO.dat4$GPM.Measured.Site >= 3.6)] <- "> 3.6"
# unique(tableAO.dat4$GPM_bins)
# 
# tableAO.merge <- left_join(rbsa.dat, tableAO.dat4)
# tableAO.merge <- tableAO.merge[which(!is.na(tableAO.merge$GPM_bins)),]
# 
# 
# 
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# tableAO.data <- weightedData(tableAO.merge[-which(colnames(tableAO.merge) %in% c("GPM.Measured.Site"               
#                                                                                  ,"GPM_bins"
#                                                                                  ,"count"))])
# tableAO.data <- left_join(tableAO.data, tableAO.merge[which(colnames(tableAO.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"GPM.Measured.Site"               
#                                                                                            ,"GPM_bins"
#                                                                                            ,"count"))])
# tableAO.data$count <- 1
# #######################
# # Weighted Analysis
# #######################
# tableAO.final <- proportionRowsAndColumns1(CustomerLevelData = tableAO.data
#                                            ,valueVariable    = 'count'
#                                            ,columnVariable   = 'State'
#                                            ,rowVariable      = 'GPM_bins'
#                                            ,aggregateColumnName = "Region")
# 
# tableAO.cast <- dcast(setDT(tableAO.final)
#                       , formula = BuildingType + GPM_bins ~ State
#                       , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
# 
# tableAO.table <- data.frame("BuildingType"   = tableAO.cast$BuildingType
#                             ,"Flow.Rate.GPM"  = tableAO.cast$GPM_bins
#                             ,"Percent_ID"     = tableAO.cast$w.percent_ID
#                             ,"SE_ID"          = tableAO.cast$w.SE_ID
#                             ,"n_ID"           = tableAO.cast$n_ID
#                             ,"Percent_MT"     = tableAO.cast$w.percent_MT
#                             ,"SE_MT"          = tableAO.cast$w.SE_MT
#                             ,"n_MT"           = tableAO.cast$n_MT
#                             ,"Percent_OR"     = tableAO.cast$w.percent_OR
#                             ,"SE_OR"          = tableAO.cast$w.SE_OR
#                             ,"n_OR"           = tableAO.cast$n_OR
#                             ,"Percent_WA"     = tableAO.cast$w.percent_WA
#                             ,"SE_WA"          = tableAO.cast$w.SE_WA
#                             ,"n_WA"           = tableAO.cast$n_WA
#                             ,"Percent_Region" = tableAO.cast$w.percent_Region
#                             ,"SE_Region"      = tableAO.cast$w.SE_Region
#                             ,"n_Region"       = tableAO.cast$n_Region
#                             ,"EB_ID"          = tableAO.cast$EB_ID
#                             ,"EB_MT"          = tableAO.cast$EB_MT
#                             ,"EB_OR"          = tableAO.cast$EB_OR
#                             ,"EB_WA"          = tableAO.cast$EB_WA
#                             ,"EB_Region"      = tableAO.cast$EB_Region
# ) 
# #QAQC
# stopifnot(sum(tableAO.table[which(tableAO.table$BuildingType == "Single Family")
#                             ,grep("Percent",colnames(tableAO.table))], na.rm = T) == 10)
# 
# levels(tableAO.table$Flow.Rate.GPM)
# rowOrder <- c("< 1.5"
#               ,"1.6-2.0"
#               ,"2.1-2.5"
#               ,"2.6-3.5"
#               ,"> 3.6"
#               ,"Total")
# tableAO.table <- tableAO.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
# tableAO.table <- data.frame(tableAO.table)
# 
# tableAO.final.SF <- tableAO.table[which(tableAO.table$BuildingType == "Single Family")
#                                   ,-which(colnames(tableAO.table) %in% c("BuildingType"))]
# tableAO.final.MH <- tableAO.table[which(tableAO.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(tableAO.table) %in% c("BuildingType"))]
# tableAO.final.MF <- tableAO.table[which(tableAO.table$BuildingType == "Multifamily")
#                                   ,-which(colnames(tableAO.table) %in% c("BuildingType"))]
# 
# # exportTable(tableAO.final.SF, "SF", "Table AO", weighted = TRUE)
# exportTable(tableAO.final.MH, "MH", "Table AO", weighted = TRUE)
# # exportTable(tableAO.final.MF, "MF", "Table AO", weighted = TRUE)
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# tableAO.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAO.data
#                                                    ,valueVariable    = 'count'
#                                                    ,columnVariable   = 'State'
#                                                    ,rowVariable      = 'GPM_bins'
#                                                    ,aggregateColumnName = "Region")
# 
# tableAO.cast <- dcast(setDT(tableAO.final)
#                       , formula = BuildingType + GPM_bins ~ State
#                       , value.var = c("Percent", "SE", "Count", "n"))
# 
# 
# tableAO.table <- data.frame("BuildingType"   = tableAO.cast$BuildingType
#                             ,"Flow.Rate.GPM"  = tableAO.cast$GPM_bins
#                             ,"Percent_ID"     = tableAO.cast$Percent_ID
#                             ,"SE_ID"          = tableAO.cast$SE_ID
#                             ,"n_ID"           = tableAO.cast$n_ID
#                             ,"Percent_MT"     = tableAO.cast$Percent_MT
#                             ,"SE_MT"          = tableAO.cast$SE_MT
#                             ,"n_MT"           = tableAO.cast$n_MT
#                             ,"Percent_OR"     = tableAO.cast$Percent_OR
#                             ,"SE_OR"          = tableAO.cast$SE_OR
#                             ,"n_OR"           = tableAO.cast$n_OR
#                             ,"Percent_WA"     = tableAO.cast$Percent_WA
#                             ,"SE_WA"          = tableAO.cast$SE_WA
#                             ,"n_WA"           = tableAO.cast$n_WA
#                             ,"Percent_Region" = tableAO.cast$Percent_Region
#                             ,"SE_Region"      = tableAO.cast$SE_Region
#                             ,"n_Region"       = tableAO.cast$n_Region
# )
# stopifnot(sum(tableAO.table[which(tableAO.table$BuildingType == "Single Family")
#                             ,grep("Percent",colnames(tableAO.table))], na.rm = T) == 10)
# 
# levels(tableAO.table$Flow.Rate.GPM)
# rowOrder <- c("< 1.5"
#               ,"1.6-2.0"
#               ,"2.1-2.5"
#               ,"2.6-3.5"
#               ,"> 3.6"
#               ,"Total")
# tableAO.table <- tableAO.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
# tableAO.table <- data.frame(tableAO.table)
# 
# tableAO.final.SF <- tableAO.table[which(tableAO.table$BuildingType == "Single Family")
#                                   ,-which(colnames(tableAO.table) %in% c("BuildingType"))]
# tableAO.final.MH <- tableAO.table[which(tableAO.table$BuildingType == "Manufactured")
#                                   ,-which(colnames(tableAO.table) %in% c("BuildingType"))]
# tableAO.final.MF <- tableAO.table[which(tableAO.table$BuildingType == "Multifamily")
#                                   ,-which(colnames(tableAO.table) %in% c("BuildingType"))]
# 
# # exportTable(tableAO.final.SF, "SF", "Table AO", weighted = FALSE)
# exportTable(tableAO.final.MH, "MH", "Table AO", weighted = FALSE)
# # exportTable(tableAO.final.MF, "MF", "Table AO", weighted = FALSE)






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
tableAM.merge <- tableAM.merge[which(tableAM.merge$Fixture.Type %notin% c("N/A",NA)),]

unique(tableAM.merge$Fixture.Type)
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

# exportTable(tableAM.final.SF, "SF", "Table AM", weighted = TRUE)
exportTable(tableAM.final.MH, "MH", "Table AM", weighted = TRUE)

#######################
# Multifamily
#######################
tableAM.table.MF <- mean_one_group(CustomerLevelData = tableAM.data
                                   ,valueVariable = "Site.Count"
                                   ,byVariable = "Fixture.Type"
                                   ,aggregateRow = "All Types")

tableAM.final.MF <- tableAM.table.MF[which(tableAM.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAM.table.MF) %in% c("BuildingType"))]

exportTable(tableAM.final.MF, "MF", "Table AM", weighted = TRUE)


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

# exportTable(tableAM.final.SF, "SF", "Table AM", weighted = FALSE)
exportTable(tableAM.final.MH, "MH", "Table AM", weighted = FALSE)


#######################
# Multifamily
#######################
tableAM.table.MF <- mean_one_group_unweighted(CustomerLevelData = tableAM.data
                                              ,valueVariable = "Site.Count"
                                              ,byVariable = "Fixture.Type"
                                              ,aggregateRow = "All Types")

tableAM.final.MF <- tableAM.table.MF[which(tableAM.table.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAM.table.MF) %in% c("BuildingType"))]

exportTable(tableAM.final.MF, "MF", "Table AM", weighted = FALSE)








#############################################################################################
# Table AR: DISTRIBUTION OF SHOWERHEAD FLOW RATE BY STATE (new bins)
#############################################################################################
#subset to columns needed for analysis
tableAR.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
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
exportTable(tableAR.final.MH, "MH", "Table AR", weighted = TRUE)

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
exportTable(tableAR.final.MF, "MF", "Table AR", weighted = TRUE)




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
exportTable(tableAR.final.MH, "MH", "Table AR", weighted = FALSE)

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
exportTable(tableAR.final.MF, "MF", "Table AR", weighted = FALSE)





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
exportTable(tableAS.final.MH, "MH", "Table AS", weighted = TRUE)


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
exportTable(tableAS.final.MF, "MF", "Table AS", weighted = TRUE)



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
exportTable(tableAS.final.MF, "MF", "Table AS", weighted = FALSE)





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
exportTable(tableAT.final.MH, "MH", "Table AT", weighted = TRUE)


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
exportTable(tableAT.final.MF, "MF", "Table AT", weighted = TRUE)



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
exportTable(tableAT.final.MH, "MH", "Table AT", weighted = FALSE)


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
exportTable(tableAT.final.MF, "MF", "Table AT", weighted = FALSE)






















############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
scl.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.scl.data", rundate, ".xlsx", sep = "")))
length(unique(scl.dat$CK_Cadmus_ID))
scl.dat$CK_Building_ID <- scl.dat$Category
scl.dat <- scl.dat[which(names(scl.dat) != "Category")]

#############################################################################################
#Item 106: DISTRIBUTION OF SHOWERHEAD FLOW RATE BY CK_Building_ID (SF table 113, MH table 88)
#############################################################################################
#subset to columns needed for analysis
item106.os.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
item106.os.dat$count <- 1

item106.os.dat0 <- item106.os.dat[which(item106.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item106.os.dat1 <- left_join(item106.os.dat0, scl.dat, by = "CK_Cadmus_ID")

item106.os.dat1$GPM_Measured <- as.numeric(as.character(item106.os.dat1$GPM_Measured))
item106.os.dat2 <- item106.os.dat1[which(!(is.na(item106.os.dat1$GPM_Measured))),]
unique(item106.os.dat2$GPM_Measured)

item106.os.dat3 <- item106.os.dat2[grep("shower|Shower",item106.os.dat2$Fixture.Type),]

item106.os.dat4 <- summarise(group_by(item106.os.dat3, CK_Cadmus_ID, BuildingType, CK_Building_ID, count)
                          ,GPM.Measured.Site = mean(GPM_Measured))

item106.os.dat4$GPM_bins <- item106.os.dat4$GPM.Measured.Site
item106.os.dat4$GPM_bins[which(item106.os.dat4$GPM.Measured.Site <= 1.5)] <- "< 1.5"
item106.os.dat4$GPM_bins[which(item106.os.dat4$GPM.Measured.Site >  1.5 & item106.os.dat4$GPM.Measured.Site < 2.1)] <- "1.6-2.0"
item106.os.dat4$GPM_bins[which(item106.os.dat4$GPM.Measured.Site >= 2.1 & item106.os.dat4$GPM.Measured.Site < 2.6)] <- "2.1-2.5"
item106.os.dat4$GPM_bins[which(item106.os.dat4$GPM.Measured.Site >= 2.6 & item106.os.dat4$GPM.Measured.Site < 3.6)] <- "2.6-3.5"
item106.os.dat4$GPM_bins[which(item106.os.dat4$GPM.Measured.Site >= 3.6)] <- "> 3.6"
# item106.os.dat4$GPM_bins[which(item106.os.dat4$GPM.Measured.Site >= 2.6)] <- "> 2.5"
unique(item106.os.dat4$GPM_bins)

item106.os.merge <- left_join(scl.dat, item106.os.dat4)
item106.os.merge <- item106.os.merge[which(!is.na(item106.os.merge$GPM_bins)),]

################################################
# Adding pop and sample sizes for weights
################################################
item106.os.data <- weightedData(item106.os.merge[-which(colnames(item106.os.merge) %in% c("GPM.Measured.Site"               
                                                                                 ,"GPM_bins"
                                                                                 ,"count"))])
item106.os.data <- left_join(item106.os.data, unique(item106.os.merge[which(colnames(item106.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"))]))
#######################
# Weighted Analysis
#######################
item106.os.final <- proportionRowsAndColumns1(CustomerLevelData = item106.os.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'CK_Building_ID'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Remove")

item106.os.cast <- dcast(setDT(item106.os.final)
                      , formula = BuildingType + GPM_bins ~ CK_Building_ID
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item106.os.table <- data.frame("BuildingType"   = item106.os.cast$BuildingType
                            ,"Flow.Rate.GPM"  = item106.os.cast$GPM_bins
                            ,"Percent_SCL.GenPop"   = item106.os.cast$`w.percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = item106.os.cast$`w.SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = item106.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = item106.os.cast$`w.percent_SCL LI`
                            ,"SE_SCL.LI"            = item106.os.cast$`w.SE_SCL LI`
                            ,"n_SCL.LI"             = item106.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = item106.os.cast$`w.percent_SCL EH`
                            ,"SE_SCL.EH"            = item106.os.cast$`w.SE_SCL EH`
                            ,"n_SCL.EH"             = item106.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = item106.os.cast$`w.percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = item106.os.cast$`w.SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = item106.os.cast$`n_2017 RBSA PS`
                            ,"EB_SCL.GenPop"        = item106.os.cast$`EB_SCL GenPop`
                            ,"EB_SCL.LI"            = item106.os.cast$`EB_SCL LI`
                            ,"EB_SCL.EH"            = item106.os.cast$`EB_SCL EH`
                            ,"EB_2017.RBSA.PS"      = item106.os.cast$`EB_2017 RBSA PS`
)

levels(item106.os.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106.os.table <- item106.os.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
item106.os.table <- data.frame(item106.os.table)

item106.os.final.SF <- item106.os.table[which(item106.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item106.os.table) %in% c("BuildingType"))]

exportTable(item106.os.final.SF, "SF", "Table 113", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item106.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item106.os.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'CK_Building_ID'
                                                   ,rowVariable      = 'GPM_bins'
                                                   ,aggregateColumnName = "Remove")

item106.os.cast <- dcast(setDT(item106.os.final)
                      , formula = BuildingType + GPM_bins ~ CK_Building_ID
                      , value.var = c("Percent", "SE", "Count", "n"))


item106.os.table <- data.frame("BuildingType"   = item106.os.cast$BuildingType
                            ,"Flow.Rate.GPM"  = item106.os.cast$GPM_bins
                            ,"Percent_SCL.GenPop"   = item106.os.cast$`Percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = item106.os.cast$`SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = item106.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = item106.os.cast$`Percent_SCL LI`
                            ,"SE_SCL.LI"            = item106.os.cast$`SE_SCL LI`
                            ,"n_SCL.LI"             = item106.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = item106.os.cast$`Percent_SCL EH`
                            ,"SE_SCL.EH"            = item106.os.cast$`SE_SCL EH`
                            ,"n_SCL.EH"             = item106.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = item106.os.cast$`Percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = item106.os.cast$`SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = item106.os.cast$`n_2017 RBSA PS`
)

levels(item106.os.table$Flow.Rate.GPM)
rowOrder <- c("< 1.5"
              ,"1.6-2.0"
              ,"2.1-2.5"
              ,"2.6-3.5"
              ,"> 3.6"
              ,"Total")
item106.os.table <- item106.os.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
item106.os.table <- data.frame(item106.os.table)

item106.os.final.SF <- item106.os.table[which(item106.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item106.os.table) %in% c("BuildingType"))]

exportTable(item106.os.final.SF, "SF", "Table 113", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
# Table FF: PERCENTAGE OF HOMES WITH SHOWERHEADS ABOVE 2.0 GPM BY CK_Building_ID
#############################################################################################
#subset to columns needed for analysis
tableFF.os.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
tableFF.os.dat$count <- 1

tableFF.os.dat0 <- tableFF.os.dat[which(tableFF.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableFF.os.dat1 <- left_join(scl.dat, tableFF.os.dat0, by = "CK_Cadmus_ID")

tableFF.os.dat1$GPM_Measured <- as.numeric(as.character(tableFF.os.dat1$GPM_Measured))
tableFF.os.dat2 <- tableFF.os.dat1[which(!(is.na(tableFF.os.dat1$GPM_Measured))),]
unique(tableFF.os.dat2$GPM_Measured)

tableFF.os.dat3 <- tableFF.os.dat2[grep("shower|Shower",tableFF.os.dat2$Fixture.Type),]

tableFF.os.dat4 <- summarise(group_by(tableFF.os.dat3, CK_Cadmus_ID)
                          ,GPM.Measured.Site = mean(GPM_Measured))

tableFF.os.dat4$Ind <- 0
tableFF.os.dat4$Ind[which(tableFF.os.dat4$GPM.Measured.Site > 2)] <- 1


tableFF.os.merge <- left_join(scl.dat, tableFF.os.dat4)
tableFF.os.merge <- tableFF.os.merge[which(!is.na(tableFF.os.merge$Ind)),]


################################################
# Adding pop and sample sizes for weights
################################################
tableFF.os.data <- weightedData(tableFF.os.merge[-which(colnames(tableFF.os.merge) %in% c("GPM.Measured.Site"
                                                                                 ,"Ind"))])
tableFF.os.data <- left_join(tableFF.os.data, unique(tableFF.os.merge[which(colnames(tableFF.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"
                                                                                           ,"Ind"))]))
tableFF.os.data$count <- 1
tableFF.os.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableFF.os.table <- proportions_one_group(CustomerLevelData = tableFF.os.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "CK_Building_ID"
                                       ,total.name = "Remove"
                                       ,weighted = TRUE)
tableFF.os.table <- tableFF.os.table[which(tableFF.os.table$CK_Building_ID != "Total"),]

tableFF.os.final.SF <- tableFF.os.table[which(tableFF.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableFF.os.table) %in% c("BuildingType"))]

exportTable(tableFF.os.final.SF, "SF", "Table FF", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
tableFF.os.table <- proportions_one_group(CustomerLevelData = tableFF.os.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "CK_Building_ID"
                                       ,total.name = "Remove"
                                       ,weighted = FALSE)
tableFF.os.table <- tableFF.os.table[which(tableFF.os.table$CK_Building_ID != "Total"),]

tableFF.os.final.SF <- tableFF.os.table[which(tableFF.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableFF.os.table) %in% c("BuildingType"))]

exportTable(tableFF.os.final.SF, "SF", "Table FF", weighted = FALSE, osIndicator = "SCL", OS = T)



#############################################################################################
#Table AM: Average number of showerheads and faucets per home by Type and CK_Building_ID
#############################################################################################
#subset to columns needed for analysis
tableAM.os.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
tableAM.os.dat$count <- 1

tableAM.os.dat0 <- tableAM.os.dat[which(tableAM.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableAM.os.dat1 <- left_join(tableAM.os.dat0, scl.dat, by = "CK_Cadmus_ID")

tableAM.os.dat1$GPM_Measured <- as.numeric(as.character(tableAM.os.dat1$GPM_Measured))
tableAM.os.dat2 <- tableAM.os.dat1[which(!(is.na(tableAM.os.dat1$GPM_Measured))),]
unique(tableAM.os.dat2$GPM_Measured)

tableAM.os.dat3 <- tableAM.os.dat2[grep("bathroom|faucet|shower",tableAM.os.dat2$Fixture.Type,ignore.case = T),]
tableAM.os.dat3$count <- 1
tableAM.os.dat3$Count <- 1

#cast the melt example code
tableAM.os.cast <- dcast(setDT(tableAM.os.dat3)
                      ,formula = CK_Cadmus_ID ~ Fixture.Type,sum
                      ,value.var = c("Count"))
tableAM.os.cast[is.na(tableAM.os.cast),] <- 0

tableAM.os.melt <- melt(tableAM.os.cast, id.vars = "CK_Cadmus_ID")
names(tableAM.os.melt) <- c("CK_Cadmus_ID", "Fixture.Type", "Count")

tableAM.os.dat4 <- summarise(group_by(tableAM.os.melt, CK_Cadmus_ID, Fixture.Type)
                          ,Site.Count = sum(Count))

tableAM.os.merge <- left_join(scl.dat, tableAM.os.dat4)
tableAM.os.merge$Site.Count[which(is.na(tableAM.os.merge$Site.Count))] <- 0
tableAM.os.merge <- tableAM.os.merge[which(tableAM.os.merge$Fixture.Type %notin% c("N/A",NA)),]

unique(tableAM.os.merge$Fixture.Type)
################################################
# Adding pop and sample sizes for weights
################################################
tableAM.os.data <- weightedData(tableAM.os.merge[-which(colnames(tableAM.os.merge) %in% c("Site.Count"
                                                                                 ,"Fixture.Type"))])
tableAM.os.data <- left_join(tableAM.os.data, unique(tableAM.os.merge[which(colnames(tableAM.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Site.Count"
                                                                                           ,"Fixture.Type"))]))
tableAM.os.data$count <- 1
#######################
# Weighted Analysis
#######################
tableAM.os.cast <- mean_two_groups(CustomerLevelData = tableAM.os.data
                                ,valueVariable = "Site.Count"
                                ,byVariableRow = "Fixture.Type"
                                ,byVariableColumn = "CK_Building_ID"
                                ,columnAggregate = "Remove"
                                ,rowAggregate = "All Fixtures")

tableAM.os.table <- data.frame("BuildingType"    = tableAM.os.cast$BuildingType
                            ,"Fixture.Type"   = tableAM.os.cast$Fixture.Type
                            ,"Mean_SCL.GenPop"      = tableAM.os.cast$`Mean_SCL GenPop`
                            ,"SE_SCL.GenPop"        = tableAM.os.cast$`SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = tableAM.os.cast$`n_SCL GenPop`
                            ,"Mean_SCL.LI"          = tableAM.os.cast$`Mean_SCL LI`
                            ,"SE_SCL.LI"            = tableAM.os.cast$`SE_SCL LI`
                            ,"n_SCL.LI"             = tableAM.os.cast$`n_SCL LI`
                            ,"Mean_SCL.EH"          = tableAM.os.cast$`Mean_SCL EH`
                            ,"SE_SCL.EH"            = tableAM.os.cast$`SE_SCL EH`
                            ,"n_SCL.EH"             = tableAM.os.cast$`n_SCL EH`
                            ,"Mean_2017.RBSA.PS"    = tableAM.os.cast$`Mean_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = tableAM.os.cast$`SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = tableAM.os.cast$`n_2017 RBSA PS`
                            ,"EB_SCL.GenPop"        = tableAM.os.cast$`EB_SCL GenPop`
                            ,"EB_SCL.LI"            = tableAM.os.cast$`EB_SCL LI`
                            ,"EB_SCL.EH"            = tableAM.os.cast$`EB_SCL EH`
                            ,"EB_2017.RBSA.PS"      = tableAM.os.cast$`EB_2017 RBSA PS`
) 

tableAM.os.table <- tableAM.os.table[which(tableAM.os.table$Fixture.Type != "All Fixtures"),]

tableAM.os.final.SF <- tableAM.os.table[which(tableAM.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAM.os.table) %in% c("BuildingType"))]

exportTable(tableAM.os.final.SF, "SF", "Table AM", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
tableAM.os.cast <- mean_two_groups_unweighted(CustomerLevelData = tableAM.os.data
                                           ,valueVariable = "Site.Count"
                                           ,byVariableRow = "Fixture.Type"
                                           ,byVariableColumn = "CK_Building_ID"
                                           ,columnAggregate = "Remove"
                                           ,rowAggregate = "All Fixtures")

tableAM.os.table <- data.frame("BuildingType"    = tableAM.os.cast$BuildingType
                            ,"Fixture.Type"   = tableAM.os.cast$Fixture.Type
                            ,"Mean_SCL.GenPop"      = tableAM.os.cast$`Mean_SCL GenPop`
                            ,"SE_SCL.GenPop"        = tableAM.os.cast$`SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = tableAM.os.cast$`n_SCL GenPop`
                            ,"Mean_SCL.LI"          = tableAM.os.cast$`Mean_SCL LI`
                            ,"SE_SCL.LI"            = tableAM.os.cast$`SE_SCL LI`
                            ,"n_SCL.LI"             = tableAM.os.cast$`n_SCL LI`
                            ,"Mean_SCL.EH"          = tableAM.os.cast$`Mean_SCL EH`
                            ,"SE_SCL.EH"            = tableAM.os.cast$`SE_SCL EH`
                            ,"n_SCL.EH"             = tableAM.os.cast$`n_SCL EH`
                            ,"Mean_2017.RBSA.PS"    = tableAM.os.cast$`Mean_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = tableAM.os.cast$`SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = tableAM.os.cast$`n_2017 RBSA PS`
)

tableAM.os.table <- tableAM.os.table[which(tableAM.os.table$Fixture.Type != "All Fixtures"),]

tableAM.os.final.SF <- tableAM.os.table[which(tableAM.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAM.os.table) %in% c("BuildingType"))]

exportTable(tableAM.os.final.SF, "SF", "Table AM", weighted = FALSE, osIndicator = "SCL", OS = T)



#############################################################################################
# Table AR: DISTRIBUTION OF SHOWERHEAD FLOW RATE BY CK_Building_ID (new bins)
#############################################################################################
#subset to columns needed for analysis
tableAR.os.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
tableAR.os.dat$count <- 1

tableAR.os.dat0 <- tableAR.os.dat[which(tableAR.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableAR.os.dat1 <- left_join(tableAR.os.dat0, scl.dat, by = "CK_Cadmus_ID")

tableAR.os.dat1$GPM_Measured <- as.numeric(as.character(tableAR.os.dat1$GPM_Measured))
tableAR.os.dat2 <- tableAR.os.dat1[which(!(is.na(tableAR.os.dat1$GPM_Measured))),]
unique(tableAR.os.dat2$GPM_Measured)

tableAR.os.dat3 <- tableAR.os.dat2[grep("shower|Shower",tableAR.os.dat2$Fixture.Type),]

tableAR.os.dat4 <- summarise(group_by(tableAR.os.dat3, CK_Cadmus_ID)
                          ,GPM.Measured.Site = mean(GPM_Measured))

tableAR.os.dat4$GPM_bins <- tableAR.os.dat4$GPM.Measured.Site
tableAR.os.dat4$GPM_bins[which(tableAR.os.dat4$GPM.Measured.Site <  2.5)] <- "< 2.5"
tableAR.os.dat4$GPM_bins[which(tableAR.os.dat4$GPM.Measured.Site >= 2.5)] <- ">= 2.5"
unique(tableAR.os.dat4$GPM_bins)

tableAR.os.merge <- left_join(scl.dat, tableAR.os.dat4)
tableAR.os.merge <- tableAR.os.merge[which(!is.na(tableAR.os.merge$GPM_bins)),]




################################################
# Adding pop and sample sizes for weights
################################################
tableAR.os.data <- weightedData(tableAR.os.merge[-which(colnames(tableAR.os.merge) %in% c("GPM.Measured.Site"               
                                                                                 ,"GPM_bins"
                                                                                 ,"count"))])
tableAR.os.data <- left_join(tableAR.os.data, unique(tableAR.os.merge[which(colnames(tableAR.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"))]))

tableAR.os.data$count <- 1
#######################
# Weighted Analysis
#######################
tableAR.os.final <- proportionRowsAndColumns1(CustomerLevelData = tableAR.os.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'CK_Building_ID'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Remove")

tableAR.os.cast <- dcast(setDT(tableAR.os.final)
                      , formula = BuildingType + GPM_bins ~ CK_Building_ID
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

tableAR.os.table <- data.frame("BuildingType"   = tableAR.os.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAR.os.cast$GPM_bins
                            ,"Percent_SCL.GenPop"   = tableAR.os.cast$`w.percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = tableAR.os.cast$`w.SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = tableAR.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = tableAR.os.cast$`w.percent_SCL LI`
                            ,"SE_SCL.LI"            = tableAR.os.cast$`w.SE_SCL LI`
                            ,"n_SCL.LI"             = tableAR.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = tableAR.os.cast$`w.percent_SCL EH`
                            ,"SE_SCL.EH"            = tableAR.os.cast$`w.SE_SCL EH`
                            ,"n_SCL.EH"             = tableAR.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = tableAR.os.cast$`w.percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = tableAR.os.cast$`w.SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = tableAR.os.cast$`n_2017 RBSA PS`
                            ,"EB_SCL.GenPop"        = tableAR.os.cast$`EB_SCL GenPop`
                            ,"EB_SCL.LI"            = tableAR.os.cast$`EB_SCL LI`
                            ,"EB_SCL.EH"            = tableAR.os.cast$`EB_SCL EH`
                            ,"EB_2017.RBSA.PS"      = tableAR.os.cast$`EB_2017 RBSA PS`
)

levels(tableAR.os.table$Flow.Rate.GPM)
rowOrder <- c("< 2.5"
              ,">= 2.5"
              ,"Total")
tableAR.os.table <- tableAR.os.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAR.os.table <- data.frame(tableAR.os.table)

tableAR.os.final.SF <- tableAR.os.table[which(tableAR.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAR.os.table) %in% c("BuildingType"))]

exportTable(tableAR.os.final.SF, "SF", "Table AR", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
tableAR.os.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAR.os.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'CK_Building_ID'
                                                   ,rowVariable      = 'GPM_bins'
                                                   ,aggregateColumnName = "Remove")

tableAR.os.cast <- dcast(setDT(tableAR.os.final)
                      , formula = BuildingType + GPM_bins ~ CK_Building_ID
                      , value.var = c("Percent", "SE", "Count", "n"))


tableAR.os.table <- data.frame("BuildingType"   = tableAR.os.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAR.os.cast$GPM_bins
                            ,"Percent_SCL.GenPop"   = tableAR.os.cast$`Percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = tableAR.os.cast$`SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = tableAR.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = tableAR.os.cast$`Percent_SCL LI`
                            ,"SE_SCL.LI"            = tableAR.os.cast$`SE_SCL LI`
                            ,"n_SCL.LI"             = tableAR.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = tableAR.os.cast$`Percent_SCL EH`
                            ,"SE_SCL.EH"            = tableAR.os.cast$`SE_SCL EH`
                            ,"n_SCL.EH"             = tableAR.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = tableAR.os.cast$`Percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = tableAR.os.cast$`SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = tableAR.os.cast$`n_2017 RBSA PS`
)

levels(tableAR.os.table$Flow.Rate.GPM)
rowOrder <- c("< 2.5"
              ,">= 2.5"
              ,"Total")
tableAR.os.table <- tableAR.os.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAR.os.table <- data.frame(tableAR.os.table)

tableAR.os.final.SF <- tableAR.os.table[which(tableAR.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAR.os.table) %in% c("BuildingType"))]

exportTable(tableAR.os.final.SF, "SF", "Table AR", weighted = FALSE, osIndicator = "SCL", OS = T)



#############################################################################################
# Table AS: DISTRIBUTION OF Bathroom Faucet FLOW RATE BY CK_Building_ID
#############################################################################################
#subset to columns needed for analysis
tableAS.os.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
tableAS.os.dat$count <- 1

tableAS.os.dat0 <- tableAS.os.dat[which(tableAS.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableAS.os.dat1 <- left_join(tableAS.os.dat0, scl.dat, by = "CK_Cadmus_ID")

tableAS.os.dat1$GPM_Measured <- as.numeric(as.character(tableAS.os.dat1$GPM_Measured))
tableAS.os.dat2 <- tableAS.os.dat1[which(!(is.na(tableAS.os.dat1$GPM_Measured))),]
unique(tableAS.os.dat2$GPM_Measured)

tableAS.os.dat3 <- tableAS.os.dat2[grep("bathroom",tableAS.os.dat2$Fixture.Type, ignore.case = T),]

tableAS.os.dat4 <- summarise(group_by(tableAS.os.dat3, CK_Cadmus_ID)
                          ,GPM.Measured.Site = mean(GPM_Measured))

tableAS.os.dat4$GPM_bins <- tableAS.os.dat4$GPM.Measured.Site
tableAS.os.dat4$GPM_bins[which(tableAS.os.dat4$GPM.Measured.Site <  2.2)] <- "<= 2.2"
tableAS.os.dat4$GPM_bins[which(tableAS.os.dat4$GPM.Measured.Site >= 2.2)] <- "> 2.2"
unique(tableAS.os.dat4$GPM_bins)

tableAS.os.merge <- left_join(scl.dat, tableAS.os.dat4)
tableAS.os.merge <- tableAS.os.merge[which(!is.na(tableAS.os.merge$GPM_bins)),]

################################################
# Adding pop and sample sizes for weights
################################################
tableAS.os.data <- weightedData(tableAS.os.merge[-which(colnames(tableAS.os.merge) %in% c("GPM.Measured.Site"               
                                                                                 ,"GPM_bins"
                                                                                 ,"count"))])
tableAS.os.data <- left_join(tableAS.os.data, unique(tableAS.os.merge[which(colnames(tableAS.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"))]))

tableAS.os.data$count <- 1
#######################
# Weighted Analysis
#######################
tableAS.os.final <- proportionRowsAndColumns1(CustomerLevelData = tableAS.os.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'CK_Building_ID'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Remove")

tableAS.os.cast <- dcast(setDT(tableAS.os.final)
                      , formula = BuildingType + GPM_bins ~ CK_Building_ID
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

tableAS.os.table <- data.frame("BuildingType"   = tableAS.os.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAS.os.cast$GPM_bins
                            ,"Percent_SCL.GenPop"   = tableAS.os.cast$`w.percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = tableAS.os.cast$`w.SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = tableAS.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = tableAS.os.cast$`w.percent_SCL LI`
                            ,"SE_SCL.LI"            = tableAS.os.cast$`w.SE_SCL LI`
                            ,"n_SCL.LI"             = tableAS.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = tableAS.os.cast$`w.percent_SCL EH`
                            ,"SE_SCL.EH"            = tableAS.os.cast$`w.SE_SCL EH`
                            ,"n_SCL.EH"             = tableAS.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = tableAS.os.cast$`w.percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = tableAS.os.cast$`w.SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = tableAS.os.cast$`n_2017 RBSA PS`
                            ,"EB_SCL.GenPop"        = tableAS.os.cast$`EB_SCL GenPop`
                            ,"EB_SCL.LI"            = tableAS.os.cast$`EB_SCL LI`
                            ,"EB_SCL.EH"            = tableAS.os.cast$`EB_SCL EH`
                            ,"EB_2017.RBSA.PS"      = tableAS.os.cast$`EB_2017 RBSA PS`
)

levels(tableAS.os.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
tableAS.os.table <- tableAS.os.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAS.os.table <- data.frame(tableAS.os.table)

tableAS.os.final.SF <- tableAS.os.table[which(tableAS.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAS.os.table) %in% c("BuildingType"))]

exportTable(tableAS.os.final.SF, "SF", "Table AS", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
tableAS.os.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAS.os.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'CK_Building_ID'
                                                   ,rowVariable      = 'GPM_bins'
                                                   ,aggregateColumnName = "Remove")

tableAS.os.cast <- dcast(setDT(tableAS.os.final)
                      , formula = BuildingType + GPM_bins ~ CK_Building_ID
                      , value.var = c("Percent", "SE", "Count", "n"))


tableAS.os.table <- data.frame("BuildingType"   = tableAS.os.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAS.os.cast$GPM_bins
                            ,"Percent_SCL.GenPop"   = tableAS.os.cast$`Percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = tableAS.os.cast$`SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = tableAS.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = tableAS.os.cast$`Percent_SCL LI`
                            ,"SE_SCL.LI"            = tableAS.os.cast$`SE_SCL LI`
                            ,"n_SCL.LI"             = tableAS.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = tableAS.os.cast$`Percent_SCL EH`
                            ,"SE_SCL.EH"            = tableAS.os.cast$`SE_SCL EH`
                            ,"n_SCL.EH"             = tableAS.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = tableAS.os.cast$`Percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = tableAS.os.cast$`SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = tableAS.os.cast$`n_2017 RBSA PS`
)

levels(tableAS.os.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
tableAS.os.table <- tableAS.os.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAS.os.table <- data.frame(tableAS.os.table)

tableAS.os.final.SF <- tableAS.os.table[which(tableAS.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAS.os.table) %in% c("BuildingType"))]

exportTable(tableAS.os.final.SF, "SF", "Table AS", weighted = FALSE, osIndicator = "SCL", OS = T)



#############################################################################################
# Table AT: DISTRIBUTION OF Kitchen Faucet FLOW RATE BY CK_Building_ID
#############################################################################################
#subset to columns needed for analysis
tableAT.os.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
tableAT.os.dat$count <- 1

tableAT.os.dat0 <- tableAT.os.dat[which(tableAT.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

tableAT.os.dat1 <- left_join(tableAT.os.dat0, scl.dat, by = "CK_Cadmus_ID")

tableAT.os.dat1$GPM_Measured <- as.numeric(as.character(tableAT.os.dat1$GPM_Measured))
tableAT.os.dat2 <- tableAT.os.dat1[which(!(is.na(tableAT.os.dat1$GPM_Measured))),]
unique(tableAT.os.dat2$GPM_Measured)

tableAT.os.dat3 <- tableAT.os.dat2[grep("kitchen",tableAT.os.dat2$Fixture.Type, ignore.case = T),]

tableAT.os.dat4 <- summarise(group_by(tableAT.os.dat3, CK_Cadmus_ID)
                          ,GPM.Measured.Site = mean(GPM_Measured))

tableAT.os.dat4$GPM_bins <- tableAT.os.dat4$GPM.Measured.Site
tableAT.os.dat4$GPM_bins[which(tableAT.os.dat4$GPM.Measured.Site <  2.2)] <- "<= 2.2"
tableAT.os.dat4$GPM_bins[which(tableAT.os.dat4$GPM.Measured.Site >= 2.2)] <- "> 2.2"
unique(tableAT.os.dat4$GPM_bins)

tableAT.os.merge <- left_join(scl.dat, tableAT.os.dat4)
tableAT.os.merge <- tableAT.os.merge[which(!is.na(tableAT.os.merge$GPM_bins)),]

################################################
# Adding pop and sample sizes for weights
################################################
tableAT.os.data <- weightedData(tableAT.os.merge[-which(colnames(tableAT.os.merge) %in% c("GPM.Measured.Site"               
                                                                                 ,"GPM_bins"
                                                                                 ,"count"))])
tableAT.os.data <- left_join(tableAT.os.data, unique(tableAT.os.merge[which(colnames(tableAT.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"GPM.Measured.Site"               
                                                                                           ,"GPM_bins"
                                                                                           ,"count"))]))
tableAT.os.data$count <- 1
tableAT.os.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableAT.os.final <- proportionRowsAndColumns1(CustomerLevelData = tableAT.os.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'CK_Building_ID'
                                           ,rowVariable      = 'GPM_bins'
                                           ,aggregateColumnName = "Remove")

tableAT.os.cast <- dcast(setDT(tableAT.os.final)
                      , formula = BuildingType + GPM_bins ~ CK_Building_ID
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

tableAT.os.table <- data.frame("BuildingType"   = tableAT.os.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAT.os.cast$GPM_bins
                            ,"Percent_SCL.GenPop"   = tableAT.os.cast$`w.percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = tableAT.os.cast$`w.SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = tableAT.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = tableAT.os.cast$`w.percent_SCL LI`
                            ,"SE_SCL.LI"            = tableAT.os.cast$`w.SE_SCL LI`
                            ,"n_SCL.LI"             = tableAT.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = tableAT.os.cast$`w.percent_SCL EH`
                            ,"SE_SCL.EH"            = tableAT.os.cast$`w.SE_SCL EH`
                            ,"n_SCL.EH"             = tableAT.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = tableAT.os.cast$`w.percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = tableAT.os.cast$`w.SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = tableAT.os.cast$`n_2017 RBSA PS`
                            ,"EB_SCL.GenPop"        = tableAT.os.cast$`EB_SCL GenPop`
                            ,"EB_SCL.LI"            = tableAT.os.cast$`EB_SCL LI`
                            ,"EB_SCL.EH"            = tableAT.os.cast$`EB_SCL EH`
                            ,"EB_2017.RBSA.PS"      = tableAT.os.cast$`EB_2017 RBSA PS`
)

levels(tableAT.os.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
tableAT.os.table <- tableAT.os.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAT.os.table <- data.frame(tableAT.os.table)

tableAT.os.final.SF <- tableAT.os.table[which(tableAT.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAT.os.table) %in% c("BuildingType"))]

exportTable(tableAT.os.final.SF, "SF", "Table AT", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
tableAT.os.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAT.os.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'CK_Building_ID'
                                                   ,rowVariable      = 'GPM_bins'
                                                   ,aggregateColumnName = "Remove")

tableAT.os.cast <- dcast(setDT(tableAT.os.final)
                      , formula = BuildingType + GPM_bins ~ CK_Building_ID
                      , value.var = c("Percent", "SE", "Count", "n"))


tableAT.os.table <- data.frame("BuildingType"   = tableAT.os.cast$BuildingType
                            ,"Flow.Rate.GPM"  = tableAT.os.cast$GPM_bins
                            ,"Percent_SCL.GenPop"   = tableAT.os.cast$`Percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = tableAT.os.cast$`SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = tableAT.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = tableAT.os.cast$`Percent_SCL LI`
                            ,"SE_SCL.LI"            = tableAT.os.cast$`SE_SCL LI`
                            ,"n_SCL.LI"             = tableAT.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = tableAT.os.cast$`Percent_SCL EH`
                            ,"SE_SCL.EH"            = tableAT.os.cast$`SE_SCL EH`
                            ,"n_SCL.EH"             = tableAT.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = tableAT.os.cast$`Percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = tableAT.os.cast$`SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = tableAT.os.cast$`n_2017 RBSA PS`
)

levels(tableAT.os.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
tableAT.os.table <- tableAT.os.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
tableAT.os.table <- data.frame(tableAT.os.table)

tableAT.os.final.SF <- tableAT.os.table[which(tableAT.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAT.os.table) %in% c("BuildingType"))]

exportTable(tableAT.os.final.SF, "SF", "Table AT", weighted = FALSE, osIndicator = "SCL", OS = T)
