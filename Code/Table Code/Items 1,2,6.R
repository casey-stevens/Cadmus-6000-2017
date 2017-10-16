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


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData
                                           ,paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))

weights.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData
                                              ,paste("weights.data", rundate, ".xlsx", sep = "")))

#############################################################################################
# Item 1 : DISTRIBUTION OF HOMES BY TYPE AND STATE (SF Table 8, MH Table 7)
#############################################################################################
item1.dat <- rbsa.dat

item1.dat$count <- 1


########################
# Step 1: State
########################

#sample and pop sizes within defined strata - this is to account for the fact that not all categories from each table will be observed in each strata
item1.state1 <- summarise(group_by(item1.dat, BuildingType, State, Region, Territory)
                         ,N.h   = unique(N.h)
                         ,n.h   = unique(n.h)
)
# obtain count and proportion by strata and home type
item1.state2 <- summarise(group_by(item1.dat, BuildingType, State, Region, Territory, HomeType)
                         ,count = sum(count)
                         ,p.h   = count / unique(n.h)
)

item1.state <- left_join(item1.state2 , item1.state1, by = c("BuildingType", "State", "Region", "Territory"))

#obtain the total population size for the building type by state combination observed in the sample
weights.state <- summarise(group_by(item1.state, BuildingType, State)
                         ,State.N.h = sum(unique(N.h), na.rm = T)
                         ,State.n.h = sum(unique(n.h)), na.rm = T)

item1.state.join <- left_join(item1.state, weights.state, by = c("BuildingType","State"))


#summarise by home type
item1.state.weighted <- summarise(group_by(item1.state.join, BuildingType, State, HomeType)
                                  ,w.percent = sum(N.h * p.h) / unique(State.N.h)
                                  ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(State.N.h)
                                  ,count     = sum(count)
                                  ,N         = unique(State.N.h)
                                  ,n         = unique(State.n.h))


#summarise across home types (total level)
item1.state.tot <- summarise(group_by(item1.state.weighted, BuildingType, State)
                             ,HomeType = "Total"
                             ,w.percent      = sum(w.percent)
                             ,w.SE           = NA
                             ,count          = sum(count, na.rm = T)
                             ,n              = sum(unique(n), na.rm = T)
                             ,N              = sum(unique(N), na.rm = T)
                             ) 


item1.state.full  <- rbind.data.frame(item1.state.weighted, item1.state.tot, stringsAsFactors = F)
# item1.state.final <- item1.state.full[which(!is.na(item1.state.full$HomeType)),]
item1.state.final <- item1.state.full[which(item1.state.full$n != 0),]

#################################
# Step 2: Region (across states)
#################################
#obtain the total population size for the building type by state combination observed in the sample
weights.region <- summarise(group_by(item1.state, BuildingType)
                           ,Region.N.h = sum(unique(N.h), na.rm = T)
                           ,Region.n.h = sum(unique(n.h), na.rm = T))

item1.region.join <- left_join(item1.state, weights.region, by = c("BuildingType"))

#summarise by home type
item1.region.weighted <- summarise(group_by(item1.region.join, BuildingType, HomeType)
                                  ,State     = "Region"
                                  ,w.percent = sum(N.h * p.h) / unique(Region.N.h)
                                  ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(Region.N.h)
                                  ,count     = sum(count)
                                  ,N         = unique(Region.N.h)
                                  ,n         = unique(Region.n.h)
                                  )



#summarise across home types (total level)
item1.region.tot <- summarise(group_by(item1.region.weighted, BuildingType)
                              ,State = "Region"
                              ,HomeType = "Total"
                              ,w.percent = sum(w.percent)
                              ,w.SE      = NA
                              ,count     = sum(count, na.rm = T)
                              ,n         = sum(unique(n), na.rm = T)
                              ,N         = sum(unique(N), na.rm = T))


item1.region.full <- rbind.data.frame(item1.region.weighted, item1.region.tot, stringsAsFactors = F)
# item1.region.final <- item1.region.full[which(!is.na(item1.region.full$HomeType)),]
item1.region.final <- item1.region.full[which(item1.region.full$n != 0),]


############################################################
# Step 3: Combine data, Cast data, create, and export tables
############################################################
#combine state and region information
item1.full <- rbind.data.frame(item1.state.final, item1.region.final, stringsAsFactors = F)
colnames(item1.full) <- c("BuildingType"
                          , "State"
                          , "Home.Type"
                          , "Percent"
                          , "SE"
                          , "Count" 
                          , "PopSize"
                          , "SampleSize")


#cast data into correct format
library(data.table)
library(gdata)
item1.table <- dcast(setDT(item1.full)
                     ,formula = BuildingType + Home.Type ~ State
                     ,value.var = c("Percent", "SE", "Count")) #determine if this needs to be sample size or count depending on each table

item1.table1 <- data.frame("BuildingType"    = item1.table$BuildingType
                           ,"Home.Type"      = item1.table$Home.Type
                           ,"Percent_ID"     = "" #item1.table$Percent_ID  -- missing for now, only partial data, placeholder until we get full data
                           ,"SE_ID"          = "" #item1.table$SE_ID  -- missing for now, only partial data, placeholder until we get full data
                           ,"Percent_MT"     = item1.table$Percent_MT
                           ,"SE_MT"          = item1.table$SE_MT
                           ,"Percent_OR"     = "" #item1.table$Percent_OR  -- missing for now, only partial data, placeholder until we get full data
                           ,"SE_OR"          = "" #item1.table$SE_OR  -- missing for now, only partial data, placeholder until we get full data 
                           ,"Percent_WA"     = item1.table$Percent_WA
                           ,"SE_WA"          = item1.table$SE_WA
                           ,"Percent_Region" = item1.table$Percent_Region
                           ,"SE_Region"      = item1.table$SE_Region
                           ,"SampleSize"     = item1.table$Count_Region)

### Split into respective tables

## Re-order rows according to previous tables
item1.table.SF <- data.frame(item1.table1[which(item1.table1$BuildingType %in% c("Single Family")),-1],stringsAsFactors = F)
sf.target <- c("Single Family Detached"
            ,"Duplex, Triplex, or Fourplex"
            ,"Townhome or Rowhome"
            ,"Total")
item1.table.SF$Home.Type <- reorder.factor(item1.table.SF$Home.Type, new.order = sf.target)
item1.table.SF <- item1.table.SF %>% arrange(Home.Type)

## Re-order rows according to previous tables
item1.table.MH <- item1.table1[which(item1.table1$BuildingType %in% c("Manufactured")),-1]
mh.target <- c("Single Wide"
               ,"Double Wide"
               ,"Triple Wide"
               ,"Total")
item1.table.MH$Home.Type <- reorder.factor(item1.table.MH$Home.Type, new.order = mh.target)
item1.table.MH <- item1.table.MH %>% arrange(Home.Type)



library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))
workbook.MH <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.SF, sheet = "Table 8", x = item1.table.SF, startRow = 20)
writeData(workbook.MH, sheet = "Table 7", x = item1.table.MH, startRow = 20)

saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)
saveWorkbook(workbook.MH, file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"), overwrite = T)








#############################################################################################
# 
# Item 2 : DISTRIBUTION OF HOMES BY VINTAGE AND STATE (SF Table 9, MH Table 8)
# 
#############################################################################################

item2.dat <- rbsa.dat[which(!is.na(rbsa.dat$HomeYearBuilt)),]

item2.dat$count <- 1

item2.final <- proportionRowsAndColumns1(item2.dat
                          , valueVariable = 'count'
                          , columnVariable = 'State'
                          , rowVariable = 'HomeYearBuilt_bins2'
                          , aggregateColumnName = "Region")

# ########################
# # Step 1: State
# ########################
# #by vintage
# item2.state <- summarise(group_by(item2.dat, BuildingType, HomeYearBuilt_bins2, State)
#                               ,Count = sum(count)
#                               ,SampleSize = length(unique(CK_Cadmus_ID)))
# #across vintages
# item2.state.tot <- summarise(group_by(item2.dat, BuildingType, State)
#                               ,HomeYearBuilt_bins2 = "Total"
#                               ,Count = sum(count)
#                               ,SampleSize = length(unique(CK_Cadmus_ID)))
# #combine
# item2.state.full <- rbind.data.frame(item2.state, item2.state.tot, stringsAsFactors = F)
# 
# 
# ########################
# # Step 2: Region (across states)
# ########################
# #by vintage
# item2.region <- summarise(group_by(item2.dat, BuildingType, HomeYearBuilt_bins2)
#                                ,State = "Region"
#                                ,Count = sum(count)
#                                ,SampleSize = length(unique(CK_Cadmus_ID))
# )
# #across vintages
# item2.region.tot <- summarise(group_by(item2.dat, BuildingType)
#                                ,HomeYearBuilt_bins2 = "Total"
#                                ,State = "Region"
#                                ,Count = sum(count)
#                                ,SampleSize = length(unique(CK_Cadmus_ID))
# )
# #combine
# item2.region.full <- rbind.data.frame(item2.region, item2.region.tot, stringsAsFactors = F)
# 
# 
# ##################################################
# # Step 3 (part a): Sample Sizes for SEs
# ##################################################
# # Extract sample sizes for standard error calculations from the Total Row information for states and region
# #   and subset to only columns needed
# item2.total <- rbind.data.frame(item2.state.tot, item2.region.tot, stringsAsFactors = F)
# item2.total1 <- item2.total[which(colnames(item2.total) %in% c("BuildingType"
#                                                                , "State"
#                                                                , "Count"
#                                                                , "SampleSize"))]
# 
# #rbind state and region information
# item2.full <- rbind.data.frame(item2.state.full, item2.region.full, stringsAsFactors = F)
# 
# item2.full1 <- left_join(item2.full, item2.total1, by = c("BuildingType", "State"))
colnames(item2.final) <- c("BuildingType"
                           , "State"
                           , "Housing.Vintage"
                           , "Percent"
                           , "SE"
                           , "Count"
                           , "Population.Size"
                           , "Sample.Size")





# ##################################################
# # Step 4: Calculate Percent distribution and SEs
# ##################################################
# # calculate Percent distribution
# item2.full1$Percent <- item2.full1$Count / item2.full1$Total.Count
# # the denominator of this SE is the sample size of the denominator in the percent
# item2.full1$SE      <- sqrt((item2.full1$Percent * (1 - item2.full1$Percent)) / item2.full1$SampleSize)



##################################################
# Step 5: Cast data and create table
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
# Step 6: Split table by building type
# and export to correct workbook
##################################################
item2.table.SF <- item2.table[which(item2.table$BuildingType == "Single Family"),-1]
item2.table.MH <- item2.table[which(item2.table$BuildingType == "Manufactured"),-1]


library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))
workbook.MH <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.SF, sheet = "Table 9", x = item2.table.SF, startRow = 20)
writeData(workbook.MH, sheet = "Table 8", x = item2.table.MH, startRow = 20)

saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)
saveWorkbook(workbook.MH, file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"), overwrite = T)











#############################################################################################
#
# Item 6: DISTRIBUTION OF HOMES BY BUILDING HEIGHT AND STATE (SF table 13)
#
#############################################################################################
item6.dat  <- rbsa.dat[which(!(is.na(rbsa.dat$BuildingHeight))),] ##only 369

item6.dat$count <- 1

########################
# Step 1: State
########################
#by home type
item6.state <- summarise(group_by(item6.dat, BuildingType, BuildingHeight, State)
                              ,Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID)))
#across home type
item6.state.tot <- summarise(group_by(item6.dat, BuildingType, State)
                              ,BuildingHeight = "Total"
                              ,Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID)))
#combine
item6.state.full <- rbind.data.frame(item6.state, item6.state.tot, stringsAsFactors = F)


########################
# Step 2: Region (across states)
########################
#by home type
item6.region <- summarise(group_by(item6.dat, BuildingType, BuildingHeight)
                               ,State = "Region"
                               ,Count = sum(count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)
#across home type
item6.region.tot <- summarise(group_by(item6.dat, BuildingType)
                               ,BuildingHeight = "Total"
                               ,State = "Region"
                               ,Count = sum(count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)
#combine
item6.region.full <- rbind.data.frame(item6.region, item6.region.tot, stringsAsFactors = F)




##################################################
# Step 3 (part a): Sample Sizes for SEs
##################################################
item6.total <- rbind.data.frame(item6.state.tot, item6.region.tot, stringsAsFactors = F)
item6.total1 <- item6.total[which(colnames(item6.total) %in% c("BuildingType"
                                                               , "State"
                                                               , "Count"
                                                               , "SampleSize"))]

#rbind state and region information
item6.full <- rbind.data.frame(item6.state.full, item6.region.full, stringsAsFactors = F)

item6.full1 <- left_join(item6.full, item6.total1, by = c("BuildingType", "State"))
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

