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
rbsa.dat <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]
length(unique(rbsa.dat$CK_Cadmus_ID))

survey.dat <- data.frame(read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Labeled and Translated"), stringsAsFactors = F)
#clean cadmus IDs
survey.dat$CK_Cadmus_ID <- trimws(toupper(survey.dat$NEXID))

#subset to columns needed for analysis
tableAV.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Was.it....0.to.under..25.000"
                                                            ,"Was.it....25.000.to.under..50.000"
                                                            ,"Was.it....50.000.or.more"))]
colnames(tableAV.dat) <- c("2015.Houshold.Income.0.to.under.25000"
                           , "2015.Houshold.Income.25000.to.under.50000"
                           , "2015.Houshold.Income.50000.or.more"
                           , "CK_Cadmus_ID")

tableAV.melt <- melt(tableAV.dat, id.vars = "CK_Cadmus_ID")
tableAV.melt <- tableAV.melt[which(tableAV.melt$value %notin% c("Unknown","N/A", NA, "Don't know", "Prefer not to say")),]
names(tableAV.melt) <- c("CK_Cadmus_ID", "Income.Level", "Detailed.Income.Level")


#merge together analysis data with cleaned RBSA data
tableAV.dat1 <- left_join(rbsa.dat, tableAV.melt, by = "CK_Cadmus_ID")
tableAV.dat1 <- tableAV.dat1[which(!is.na(tableAV.dat1$Income.Level)),]
################################################
# Adding pop and sample sizes for weights
################################################
tableAV.data <- weightedData(tableAV.dat1[-which(colnames(tableAV.dat1) %in% c("Income.Level"
                                                                               , "Detailed.Income.Level"))])
tableAV.data <- left_join(tableAV.data, unique(tableAV.dat1[which(colnames(tableAV.dat1) %in% c("CK_Cadmus_ID"
                                                                                         ,"Income.Level"
                                                                                         , "Detailed.Income.Level"))]))
tableAV.data$count <- 1
#######################
# Weighted Analysis
#######################
tableAV.final <- proportionRowsAndColumns1(CustomerLevelData = tableAV.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'Income.Level'
                                           ,aggregateColumnName = "Region")

tableAV.cast <- dcast(setDT(tableAV.final)
                      , formula = BuildingType + Income.Level ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

tableAV.table <- data.frame("BuildingType"    = tableAV.cast$BuildingType
                            ,"Income.Level" = tableAV.cast$Income.Level
                            ,"Percent_ID"     = tableAV.cast$w.percent_ID
                            ,"SE_ID"          = tableAV.cast$w.SE_ID
                            ,"n_ID"           = tableAV.cast$n_ID
                            ,"Percent_MT"     = tableAV.cast$w.percent_MT
                            ,"SE_MT"          = tableAV.cast$w.SE_MT
                            ,"n_MT"           = tableAV.cast$n_MT
                            ,"Percent_OR"     = tableAV.cast$w.percent_OR
                            ,"SE_OR"          = tableAV.cast$w.SE_OR
                            ,"n_OR"           = tableAV.cast$n_OR
                            ,"Percent_WA"     = tableAV.cast$w.percent_WA
                            ,"SE_WA"          = tableAV.cast$w.SE_WA
                            ,"n_WA"           = tableAV.cast$n_WA
                            ,"Percent_Region" = tableAV.cast$w.percent_Region
                            ,"SE_Region"      = tableAV.cast$w.SE_Region
                            ,"n_Region"       = tableAV.cast$n_Region
                            ,"EB_ID"          = tableAV.cast$EB_ID
                            ,"EB_MT"          = tableAV.cast$EB_MT
                            ,"EB_OR"          = tableAV.cast$EB_OR
                            ,"EB_WA"          = tableAV.cast$EB_WA
                            ,"EB_Region"      = tableAV.cast$EB_Region
)

tableAV.final.SF <- tableAV.table[which(tableAV.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAV.table) %in% c("BuildingType"))]
tableAV.final.MH <- tableAV.table[which(tableAV.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAV.table) %in% c("BuildingType"))]

# exportTable(tableAV.final.SF, "SF", "Table AV", weighted = TRUE)
exportTable(tableAV.final.MH, "MH", "Table AV", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableAV.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAV.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'Income.Level'
                                                   ,aggregateColumnName = "Region")

tableAV.cast <- dcast(setDT(tableAV.final)
                      , formula = BuildingType + Income.Level ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


tableAV.table <- data.frame("BuildingType"    = tableAV.cast$BuildingType
                            ,"Income.Level"      = tableAV.cast$Income.Level
                            ,"Percent_ID"     = tableAV.cast$Percent_ID
                            ,"SE_ID"          = tableAV.cast$SE_ID
                            ,"n_ID"           = tableAV.cast$n_ID
                            ,"Percent_MT"     = tableAV.cast$Percent_MT
                            ,"SE_MT"          = tableAV.cast$SE_MT
                            ,"n_MT"           = tableAV.cast$n_MT
                            ,"Percent_OR"     = tableAV.cast$Percent_OR
                            ,"SE_OR"          = tableAV.cast$SE_OR
                            ,"n_OR"           = tableAV.cast$n_OR
                            ,"Percent_WA"     = tableAV.cast$Percent_WA
                            ,"SE_WA"          = tableAV.cast$SE_WA
                            ,"n_WA"           = tableAV.cast$n_WA
                            ,"Percent_Region" = tableAV.cast$Percent_Region
                            ,"SE_Region"      = tableAV.cast$SE_Region
                            ,"n_Region"       = tableAV.cast$n_Region
)

tableAV.final.SF <- tableAV.table[which(tableAV.table$BuildingType == "Single Family")
                                  ,-which(colnames(tableAV.table) %in% c("BuildingType"))]
tableAV.final.MH <- tableAV.table[which(tableAV.table$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAV.table) %in% c("BuildingType"))]

# exportTable(tableAV.final.SF, "SF", "Table AV", weighted = FALSE)
exportTable(tableAV.final.MH, "MH", "Table AV", weighted = FALSE)
















#######################
# Weighted Analysis
#######################
tableAV.data <- tableAV.data[which(tableAV.data$BuildingType == "Multifamily"),]
tableAV.final <- proportionRowsAndColumns1(CustomerLevelData = tableAV.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'Income.Level'
                                           ,aggregateColumnName = "All Sizes")

tableAV.cast <- dcast(setDT(tableAV.final)
                      , formula = BuildingType + Income.Level ~ HomeType
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

tableAV.table <- data.frame("BuildingType"          = tableAV.cast$BuildingType
                            ,"Income.Level"         = tableAV.cast$Income.Level
                            ,"Percent_Low.Rise"     = tableAV.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"SE_Low.Rise"          = tableAV.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"n_Low.Rise"           = tableAV.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Percent_Mid.Rise"     = tableAV.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"SE_Mid.Rise"          = tableAV.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"n_Mid.Rise"           = tableAV.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"Percent_High.Rise"    = tableAV.cast$`w.percent_Apartment Building (More than 6 floors)`
                            ,"SE_High.Rise"         = tableAV.cast$`w.SE_Apartment Building (More than 6 floors)`
                            ,"n_High.Rise"          = tableAV.cast$`n_Apartment Building (More than 6 floors)`
                            ,"Percent_All.Sizes"    = tableAV.cast$`w.percent_All Sizes`
                            ,"SE_All.Sizes"         = tableAV.cast$`w.SE_All Sizes`
                            ,"n_All.Sizes"          = tableAV.cast$`n_All Sizes`
                            ,"EB_Low.Rise"          = tableAV.cast$`EB_Apartment Building (3 or fewer floors)`
                            ,"EB_Mid.Rise"          = tableAV.cast$`EB_Apartment Building (4 to 6 floors)`
                            ,"EB_High.Rise"         = tableAV.cast$`EB_Apartment Building (More than 6 floors)`
                            ,"EB_All.Sizes"         = tableAV.cast$`EB_All Sizes`
)

tableAV.final.MF <- tableAV.table[which(tableAV.table$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAV.table) %in% c("BuildingType"))]

exportTable(tableAV.final.MF, "MF", "Table AV", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
tableAV.final <- proportions_two_groups_unweighted(CustomerLevelData = tableAV.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'HomeType'
                                                   ,rowVariable      = 'Income.Level'
                                                   ,aggregateColumnName = "All Sizes")

tableAV.cast <- dcast(setDT(tableAV.final)
                      , formula = BuildingType + Income.Level ~ HomeType
                      , value.var = c("Percent", "SE", "Count", "n"))


tableAV.table <- data.frame("BuildingType"          = tableAV.cast$BuildingType
                            ,"Income.Level"         = tableAV.cast$Income.Level
                            ,"Percent_Low.Rise"     = tableAV.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"SE_Low.Rise"          = tableAV.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"n_Low.Rise"           = tableAV.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Percent_Mid.Rise"     = tableAV.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"SE_Mid.Rise"          = tableAV.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"n_Mid.Rise"           = tableAV.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"Percent_High.Rise"    = tableAV.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"SE_High.Rise"         = tableAV.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"n_High.Rise"          = tableAV.cast$`n_Apartment Building (More than 6 floors)`
                            ,"Percent_All.Sizes"    = tableAV.cast$`Percent_All Sizes`
                            ,"SE_All.Sizes"         = tableAV.cast$`SE_All Sizes`
                            ,"n_All.Sizes"          = tableAV.cast$`n_All Sizes`
)

tableAV.final.MF <- tableAV.table[which(tableAV.table$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAV.table) %in% c("BuildingType"))]

exportTable(tableAV.final.MF, "MF", "Table AV", weighted = TRUE)
