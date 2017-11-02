#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:          09/11/2017                                  
##  Billing Code(s):  6596.0000.0002.1002.0000
#############################################################################################
#############################################################################################
# For items 1, 2, 6 - this code will summarize information to match the previous RBSA table
#   Step 1: State level analysis
#   Step 2: Region level analysis
#   Step 3: Put data into correct format for creating tables, Subset tables by building type 
#           and export to respective workbooks
#############################################################################################
##  Clear variables
rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)

# Source
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData
                                           ,paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))

#############################################################################################
# Item 1 : DISTRIBUTION OF HOMES BY TYPE AND STATE (SF Table 8, MH Table 7)
#############################################################################################
item1.dat0 <- rbsa.dat[which(!is.na(rbsa.dat$BuildingTypeXX)),]
item1.dat <- weightedData(item1.dat0)

item1.dat$count <- 1

item1.final <- proportionRowsAndColumns1(item1.dat
                                         ,'count'
                                         ,'State'
                                         ,'HomeType'
                                         , aggregateColumnName = "Region"
                                         # , weighted = TRUE
                                         )


#cast data into correct format
library(data.table)
library(gdata)
item1.cast <- dcast(setDT(item1.final)
                     ,formula = BuildingType + HomeType ~ State
                     ,value.var = c("w.percent", "w.SE", "count", "n", "N"))

#can add pop and sample sizes if needed in exported table
item1.table <- data.frame("BuildingType"    = item1.cast$BuildingType
                           ,"Home.Type"      = item1.cast$HomeType
                           ,"Percent_ID"     = item1.cast$w.percent_ID
                           ,"SE_ID"          = item1.cast$w.SE_ID
                           ,"n_ID"           = item1.cast$count_ID
                           ,"Percent_MT"     = item1.cast$w.percent_MT
                           ,"SE_MT"          = item1.cast$w.SE_MT
                           ,"n_MT"           = item1.cast$count_MT
                           ,"Percent_OR"     = item1.cast$w.percent_OR
                           ,"SE_OR"          = item1.cast$w.SE_OR
                           ,"n_OR"           = item1.cast$count_OR
                           ,"Percent_WA"     = item1.cast$w.percent_WA
                           ,"SE_WA"          = item1.cast$w.SE_WA
                           ,"n_WA"           = item1.cast$count_WA
                           ,"Percent_Region" = item1.cast$w.percent_Region
                           ,"SE_Region"      = item1.cast$w.SE_Region
                           ,"SampleSize"     = item1.cast$count_Region)

### Split into respective tables

## Re-order rows according to previous tables
item1.table.SF <- data.frame(item1.table[which(item1.table$BuildingType %in% c("Single Family")),-1],stringsAsFactors = F)
sf.target <- c("Single Family Detached"
            ,"Duplex, Triplex, or Fourplex"
            ,"Townhome or Rowhome"
            ,"Total")
item1.table.SF$Home.Type <- reorder.factor(item1.table.SF$Home.Type, new.order = sf.target)
item1.table.SF <- item1.table.SF %>% arrange(Home.Type)

## Re-order rows according to previous tables
item1.table.MH <- item1.table[which(item1.table$BuildingType %in% c("Manufactured")),-1]
mh.target <- c("Single Wide"
               ,"Double Wide"
               ,"Triple Wide"
               ,"Modular / Prefab"
               ,"Total")
item1.table.MH$Home.Type <- reorder.factor(item1.table.MH$Home.Type, new.order = mh.target)
item1.table.MH <- item1.table.MH %>% arrange(Home.Type)



#exporting function
exportTable(item1.table.SF, "SF", "Table 8"
            , weighted = TRUE)
exportTable(item1.table.MH, "MH", "Table 7"
            , weighted = TRUE)









#############################################################################################
# 
# Item 2 : DISTRIBUTION OF HOMES BY VINTAGE AND STATE (SF Table 9, MH Table 8)
# 
#############################################################################################
item2.dat <- weightedData(rbsa.dat[which(!is.na(rbsa.dat$HomeYearBuilt)),])

item2.dat$count <- 1

#function for weighted analysis
item2.final <- proportionRowsAndColumns1(item2.dat
                          , valueVariable = 'count'
                          , columnVariable = 'State'
                          , rowVariable = 'HomeYearBuilt_bins2'
                          , aggregateColumnName = "Region"
                          # , weighted = TRUE
                          )

colnames(item2.final) <- c("BuildingType"
                           , "State"
                           , "Housing.Vintage"
                           , "Percent"
                           , "SE"
                           , "Count"
                           , "Population.Size"
                           , "Sample.Size")


##################################################
# Cast data and create table
##################################################
library(data.table)
item2.cast <- dcast(setDT(item2.final)
                     ,formula = BuildingType + Housing.Vintage ~ State
                     ,value.var = c("Percent", "SE", "Sample.Size", "Population.Size", "Count"))

item2.table <- data.frame("BuildingType"     = item2.cast$BuildingType
                           ,"Housing.Vintage" = item2.cast$Housing.Vintage
                           ,"Percent_ID"      = item2.cast$Percent_ID
                           ,"SE_ID"           = item2.cast$SE_ID
                           ,"n_ID"            = item2.cast$Count_ID
                           ,"Percent_MT"      = item2.cast$Percent_MT
                           ,"SE_MT"           = item2.cast$SE_MT
                           ,"n_MT"            = item2.cast$Count_MT
                           ,"Percent_OR"      = item2.cast$Percent_OR
                           ,"SE_OR"           = item2.cast$SE_OR
                           ,"n_OR"            = item2.cast$Count_OR
                           ,"Percent_WA"      = item2.cast$Percent_WA
                           ,"SE_WA"           = item2.cast$SE_WA
                           ,"n_WA"            = item2.cast$Count_WA
                           ,"Percent_Region"  = item2.cast$Percent_Region
                           ,"SE_Region"       = item2.cast$SE_Region
                           ,"SampleSize"      = item2.cast$Count_Region)



##################################################
# Split table by building type
# and export to correct workbook
##################################################
item2.table.SF <- item2.table[which(item2.table$BuildingType == "Single Family"),-1]
item2.table.MH <- item2.table[which(item2.table$BuildingType == "Manufactured"),-1]

#exporting function
exportTable(item2.table.SF, "SF", "Table 9"
            , weighted = TRUE)
exportTable(item2.table.MH, "MH", "Table 8"
            , weighted = TRUE)










#############################################################################################
#
# Item 6: DISTRIBUTION OF HOMES BY BUILDING HEIGHT AND STATE (SF table 13)
#
#############################################################################################
item6.dat <- weightedData(rbsa.dat[which(!is.na(rbsa.dat$BuildingHeight)),])

item6.dat$count <- 1

#function for weighted analysis
item6.final <- proportionRowsAndColumns1(item6.dat
                                         , valueVariable = 'count'
                                         , columnVariable = 'State'
                                         , rowVariable = 'BuildingHeight'
                                         , aggregateColumnName = "Region"
                                         # , weighted = TRUE
                                         )

colnames(item6.final) <- c("BuildingType"
                           , "State"
                           , "BuildingHeight"
                           , "Percent"
                           , "SE"
                           , "Count"
                           , "N"
                           , "n")


##################################################
# Step 5: Cast data and create table
##################################################
library(data.table)
item6.cast <- dcast(setDT(item6.final)
                     ,formula = BuildingType + BuildingHeight ~ State
                     ,value.var = c("Percent", "SE", "Count", "N", "n"))

item6.table <- data.frame("BuildingType"    = item6.cast$BuildingType
                           ,"BuildingHeight" = item6.cast$BuildingHeight
                          ,"Percent_ID"      = item6.cast$Percent_ID
                          ,"SE_ID"           = item6.cast$SE_ID
                          ,"n_ID"            = item6.cast$Count_ID
                           ,"Percent_MT"     = item6.cast$Percent_MT
                           ,"SE_MT"          = item6.cast$SE_MT
                           ,"n_MT"           = item6.cast$Count_MT
                           ,"Percent_OR"     = item6.cast$Percent_OR
                           ,"SE_OR"          = item6.cast$SE_OR
                           ,"n_OR"           = item6.cast$Count_OR
                           ,"Percent_WA"     = item6.cast$Percent_WA
                           ,"SE_WA"          = item6.cast$SE_WA
                           ,"n_WA"           = item6.cast$Count_WA
                           ,"Percent_Region" = item6.cast$Percent_Region
                           ,"SE_Region"      = item6.cast$SE_Region
                           ,"SampleSize"     = item6.cast$Count_Region)




##################################################
# Step 6: Split table by building type
# and export to correct workbook
##################################################
item6.table.SF <- item6.table[which(item6.table$BuildingType == "Single Family"),-1]


exportTable(item6.table.SF, "SF", "Table 13"
            , weighted = TRUE)

