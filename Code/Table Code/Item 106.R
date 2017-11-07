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
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >= 2.6 & item106.dat4$GPM.Measured.Site < 3.6)] <- "2.6-3.5"
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >= 3.6)] <- "> 3.6"
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
                     , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item106.table <- data.frame("BuildingType"    = item106.cast$BuildingType
                           ,"Flow.Rate.GPM"      = item106.cast$GPM_bins
                           ,"Percent_ID"     = item106.cast$w.percent_ID
                           ,"SE_ID"          = item106.cast$w.SE_ID
                           ,"Count_ID"       = item106.cast$count_ID
                           ,"Percent_MT"     = item106.cast$w.percent_MT
                           ,"SE_MT"          = item106.cast$w.SE_MT
                           ,"Count_MT"       = item106.cast$count_MT
                           ,"Percent_OR"     = item106.cast$w.percent_OR
                           ,"SE_OR"          = item106.cast$w.SE_OR
                           ,"Count_OR"       = item106.cast$count_OR
                           ,"Percent_WA"     = item106.cast$w.percent_WA
                           ,"SE_WA"          = item106.cast$w.SE_WA
                           ,"Count_WA"       = item106.cast$count_WA
                           ,"Percent_Region" = item106.cast$w.percent_Region
                           ,"SE_Region"      = item106.cast$w.SE_Region
                           ,"Count_Region"   = item106.cast$count_Region
                           # ,"SampleSize"     = item106.cast$SampleSize_Region
)
#QAQC
stopifnot(sum(item106.table[which(item106.table$BuildingType == "Single Family")
                           ,grep("Percent",colnames(item106.table))], na.rm = T) == 10)


item106.final.SF <- item106.table[which(item106.table$BuildingType == "Single Family")
                                ,-which(colnames(item106.table) %in% c("BuildingType"))]
item106.final.MH <- item106.table[which(item106.table$BuildingType == "Manufactured")
                                ,-which(colnames(item106.table) %in% c("BuildingType"))]

exportTable(item106.final.SF, "SF", "Table 113", weighted = TRUE)
exportTable(item106.final.MH, "MH", "Table 88", weighted = TRUE)


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
                     , value.var = c("Percent", "SE", "Count", "SampleSize"))


item106.table <- data.frame("BuildingType"   = item106.cast$BuildingType
                           ,"Flow.Rate.GPM"  = item106.cast$GPM_bins
                           ,"Percent_ID"     = item106.cast$Percent_ID
                           ,"SE_ID"          = item106.cast$SE_ID
                           ,"Count_ID"       = item106.cast$Count_ID
                           ,"Percent_MT"     = item106.cast$Percent_MT
                           ,"SE_MT"          = item106.cast$SE_MT
                           ,"Count_MT"       = item106.cast$Count_MT
                           ,"Percent_OR"     = item106.cast$Percent_OR
                           ,"SE_OR"          = item106.cast$SE_OR
                           ,"Count_OR"       = item106.cast$Count_OR
                           ,"Percent_WA"     = item106.cast$Percent_WA
                           ,"SE_WA"          = item106.cast$SE_WA
                           ,"Count_WA"       = item106.cast$Count_WA
                           ,"Percent_Region" = item106.cast$Percent_Region
                           ,"SE_Region"      = item106.cast$SE_Region
                           ,"Count_Region"   = item106.cast$Count_Region
                           # ,"SampleSize"     = item106.cast$SampleSize_Region
)
stopifnot(sum(item106.table[which(item106.table$BuildingType == "Single Family")
                           ,grep("Percent",colnames(item106.table))], na.rm = T) == 10)


item106.final.SF <- item106.table[which(item106.table$BuildingType == "Single Family")
                                ,-which(colnames(item106.table) %in% c("BuildingType"))]
item106.final.MH <- item106.table[which(item106.table$BuildingType == "Manufactured")
                                ,-which(colnames(item106.table) %in% c("BuildingType"))]

exportTable(item106.final.SF, "SF", "Table 113", weighted = FALSE)
exportTable(item106.final.MH, "MH", "Table 88", weighted = FALSE)
