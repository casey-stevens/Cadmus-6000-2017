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


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData
                                           ,paste("clean.rbsa.data.unweighted", rundate, ".xlsx", sep = "")))

#############################################################################################
# Item 1 : DISTRIBUTION OF HOMES BY TYPE AND STATE (SF Table 8, MH Table 7)
#############################################################################################
item1.dat <- weightedData(rbsa.dat)

item1.dat$count <- 1

item1.final <- proportionRowsAndColumns1(item1.dat
                                         ,'count'
                                         ,'State'
                                         ,'HomeType'
                                         , aggregateColumnName = "Region")
colnames(item1.final) <- c("BuildingType"
                          , "State"
                          , "Home.Type"
                          , "Percent"
                          , "SE"
                          , "Count" 
                          , "N"
                          , "n")


#cast data into correct format
library(data.table)
library(gdata)
item1.cast <- dcast(setDT(item1.final)
                     ,formula = BuildingType + Home.Type ~ State
                     ,value.var = c("Percent", "SE", "Count", "n", "N")) #determine if this needs to be sample size or count depending on each table

item1.table <- data.frame("BuildingType"    = item1.cast$BuildingType
                           ,"Home.Type"      = item1.cast$Home.Type
                           ,"Percent_ID"     = "" #item1.cast$Percent_ID  -- missing for now, only partial data, placeholder until we get full data
                           ,"SE_ID"          = "" #item1.cast$SE_ID  -- missing for now, only partial data, placeholder until we get full data
                           ,"n_ID"           = "" #item1.cast$Count_ID
                           ,"Percent_MT"     = item1.cast$Percent_MT
                           ,"SE_MT"          = item1.cast$SE_MT
                           ,"n_MT"           = item1.cast$Count_MT
                           ,"Percent_OR"     = "" #item1.cast$Percent_OR  -- missing for now, only partial data, placeholder until we get full data
                           ,"SE_OR"          = "" #item1.cast$SE_OR  -- missing for now, only partial data, placeholder until we get full data 
                           ,"n_OR"           = "" #item1.cast$Count_OR
                           ,"Percent_WA"     = item1.cast$Percent_WA
                           ,"SE_WA"          = item1.cast$SE_WA
                           ,"n_WA"           = item1.cast$Count_WA
                           ,"Percent_Region" = item1.cast$Percent_Region
                           ,"SE_Region"      = item1.cast$SE_Region
                           ,"SampleSize"     = item1.cast$Count_Region)

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
               ,"Total")
item1.table.MH$Home.Type <- reorder.factor(item1.table.MH$Home.Type, new.order = mh.target)
item1.table.MH <- item1.table.MH %>% arrange(Home.Type)



#exporting function
exportTable(item1.table.SF, "SF", "Table 8")
exportTable(item1.table.MH, "MH", "Table 7")









#############################################################################################
# 
# Item 2 : DISTRIBUTION OF HOMES BY VINTAGE AND STATE (SF Table 9, MH Table 8)
# 
#############################################################################################

item2.dat <- rbsa.dat#[which(!is.na(rbsa.dat$HomeYearBuilt)),]

item2.dat$count <- 1

#function for weighted analysis
item2.final <- proportionRowsAndColumns1(item2.dat
                          , valueVariable = 'count'
                          , columnVariable = 'State'
                          , rowVariable = 'HomeYearBuilt_bins2'
                          , aggregateColumnName = "Region")

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
                           ,"Percent_ID"      = NA #missing for now, only partial data, placeholder until we get full data
                           ,"SE_ID"           = NA #missing for now, only partial data, placeholder until we get full data
                           ,"n_ID"            = NA #
                           ,"Percent_MT"      = item2.cast$Percent_MT
                           ,"SE_MT"           = item2.cast$SE_MT
                           ,"n_MT"            = item2.cast$Count_MT
                           ,"Percent_OR"      = NA #item2.cast$Percent_OR
                           ,"SE_OR"           = NA #item2.cast$SE_OR
                           ,"n_OR"            = NA #item2.cast$Count_OR
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
exportTable(item2.table.SF, "SF", "Table 9")
exportTable(item2.table.MH, "MH", "Table 8")










#############################################################################################
#
# Item 6: DISTRIBUTION OF HOMES BY BUILDING HEIGHT AND STATE (SF table 13)
#
#############################################################################################
item6.dat <- rbsa.dat[which(!(is.na(rbsa.dat$BuildingHeight))),] 
item6.dat  <- weightedData(item6.dat)

item6.dat$count <- 1

#function for weighted analysis
item6.final <- proportionRowsAndColumns1(item6.dat
                                         , valueVariable = 'count'
                                         , columnVariable = 'State'
                                         , rowVariable = 'BuildingHeight'
                                         , aggregateColumnName = "Region")

# ########################
# # Step 1: State
# ########################
# #by home type
# item6.state <- summarise(group_by(item6.dat, BuildingType, BuildingHeight, State)
#                               ,Count = sum(count)
#                               ,SampleSize = length(unique(CK_Cadmus_ID)))
# #across home type
# item6.state.tot <- summarise(group_by(item6.dat, BuildingType, State)
#                               ,BuildingHeight = "Total"
#                               ,Count = sum(count)
#                               ,SampleSize = length(unique(CK_Cadmus_ID)))
# #combine
# item6.state.full <- rbind.data.frame(item6.state, item6.state.tot, stringsAsFactors = F)
# 
# 
# ########################
# # Step 2: Region (across states)
# ########################
# #by home type
# item6.region <- summarise(group_by(item6.dat, BuildingType, BuildingHeight)
#                                ,State = "Region"
#                                ,Count = sum(count)
#                                ,SampleSize = length(unique(CK_Cadmus_ID))
# )
# #across home type
# item6.region.tot <- summarise(group_by(item6.dat, BuildingType)
#                                ,BuildingHeight = "Total"
#                                ,State = "Region"
#                                ,Count = sum(count)
#                                ,SampleSize = length(unique(CK_Cadmus_ID))
# )
# #combine
# item6.region.full <- rbind.data.frame(item6.region, item6.region.tot, stringsAsFactors = F)
# 
# 
# 
# 
# ##################################################
# # Step 3 (part a): Sample Sizes for SEs
# ##################################################
# item6.total <- rbind.data.frame(item6.state.tot, item6.region.tot, stringsAsFactors = F)
# item6.total1 <- item6.total[which(colnames(item6.total) %in% c("BuildingType"
#                                                                , "State"
#                                                                , "Count"
#                                                                , "SampleSize"))]
# 
# #rbind state and region information
# item6.full <- rbind.data.frame(item6.state.full, item6.region.full, stringsAsFactors = F)
# 
# item6.full1 <- left_join(item6.full, item6.total1, by = c("BuildingType", "State"))
colnames(item6.full1) <- c("BuildingType"
                           , "BuildingHeight"
                           , "State"
                           , "Count"
                           , "Remove.SampleSize" # Step 3 (part b) -- This sample size is only used for the report table
                           , "Total.Count"
                           , "SampleSize")


##################################################
# Step 4: Calculate Percent distribution and SEs
##################################################
# calculate Percent distribution
item6.full1$Percent <- item6.full1$Count / item6.full1$Total.Count
# the denominator of this SE is the sample size of the denominator in the percent
item6.full1$SE      <- sqrt((item6.full1$Percent * (1 - item6.full1$Percent)) / item6.full1$SampleSize)



##################################################
# Step 5: Cast data and create table
##################################################
library(data.table)
item6.table <- dcast(setDT(item6.full1)
                     ,formula = BuildingType + BuildingHeight ~ State
                     ,value.var = c("Percent", "SE", "SampleSize"))

item6.table1 <- data.frame("BuildingType"    = item6.table$BuildingType
                           ,"BuildingHeight" = item6.table$BuildingHeight
                           ,"Percent_ID"     = NA #missing for now, only partial data, placeholder until we get full data
                           ,"SE_ID"          = NA #missing for now, only partial data, placeholder until we get full data
                           ,"Percent_MT"     = item6.table$Percent_MT
                           ,"SE_MT"          = item6.table$SE_MT
                           ,"Percent_OR"     = item6.table$Percent_OR
                           ,"SE_OR"          = item6.table$SE_OR
                           ,"Percent_WA"     = item6.table$Percent_WA
                           ,"SE_WA"          = item6.table$SE_WA
                           ,"Percent_Region" = item6.table$Percent_Region
                           ,"SE_Region"      = item6.table$SE_Region
                           ,"SampleSize"     = item6.table$Remove.SampleSize_Region)

item6.table.final <- item6.table1[which(item6.table1$BuildingType %in% c("Single Family")),]





##################################################
# Step 6: Split table by building type
# and export to correct workbook
##################################################
item6.table.SF <- item6.table.final[which(item6.table.final$BuildingType == "Single Family"),-1]


library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
workbook <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook, sheet = "Table 13", x = item6.table.SF, startRow = 20)

saveWorkbook(workbook, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)

