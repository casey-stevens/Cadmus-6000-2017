#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:          09/11/2017                                  
##  Billing Code(s):  6596.0000.0002.1002.0000
#############################################################################################
#############################################################################################
# For items 1, 2, 6 - this code will summarize information to match the previous RBSA table
#   Step 1: Obtain the count distribution of home type within each state
#     as well as total information before a cast
#   Step 2: Obtain the count distribution of home types across states (i.e. within region)
#     as well as total information before a cast
#   Step 3: Part a: Extract Sample Sizes for standard error calculations and join them onto 
#             State and region information
#           Part b: Note which sample sizes are to be used in the final tables
#   Step 4: Calculate estimates and standard errors 
#   Step 5: Put data into correct format for creating tables
#   Step 6: Subset tables by building type and export to respective workbooks
#############################################################################################

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData
                                           ,paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))

#############################################################################################
# Item 1 : DISTRIBUTION OF HOMES BY TYPE AND STATE (SF Table 8, MH Table 7)
#############################################################################################
item1.dat <- rbsa.dat

item1.dat$count <- 1

########################
# Step 1: State
########################
#summarise by home type
item1.state <- summarise(group_by(item1.dat, BuildingType, BuildingTypeXX, State)
                              ,Count = sum((N.h / n.h) * count)
                              ,SampleSize = "")
#summarise across home types (total level)
item1.state.tot <- summarise(group_by(item1.dat, BuildingType, State)
                              ,BuildingTypeXX = "Total"
                              ,Count = sum(N.h / n.h * count)
                              ,SampleSize = length(unique(CK_Cadmus_ID))) #this n will be used for SE Calcs
item1.state.full <- rbind.data.frame(item1.state, item1.state.tot, stringsAsFactors = F)

########################
# Step 2: Region (across states)
########################
#by home types
item1.region <- summarise(group_by(item1.dat, BuildingType, BuildingTypeXX)
                               ,State = "Region"
                               ,Count = sum(N.h / n.h * count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))) #this n will be reported in table
#summarise across home types
item1.region.tot <- summarise(group_by(item1.dat, BuildingType)
                               ,BuildingTypeXX = "Total"
                               ,State = "Region"
                               ,Count = sum(N.h / n.h * count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))) #this n will be reported in table AND used for SE Calcs
item1.region.full <- rbind.data.frame(item1.region, item1.region.tot, stringsAsFactors = F)



#rbind state and region information
item1.full <- rbind.data.frame(item1.state.full, item1.region.full, stringsAsFactors = F)


##################################################
# Step 3 (part a): Sample Sizes for SEs
##################################################
# Extract sample sizes for standard error calculations from the Total Row information for states and region
#   and subset to only columns needed
item1.total <- rbind.data.frame(item1.state.tot, item1.region.tot, stringsAsFactors = F)
item1.total1 <- item1.total[which(colnames(item1.total) %in% c("BuildingType"
                                                               , "State"
                                                               , "Count"
                                                               , "SampleSize"))]

# merge SE sample sizes onto rest of data
item1.full1 <- left_join(item1.full, item1.total1, by = c("BuildingType", "State"))
colnames(item1.full1) <- c("BuildingType"
                           , "Home.Type"
                           , "State"
                           , "Count"
                           , "Remove.SampleSize" # Step 3 (part b) -- This sample size is only used for the report table
                           , "Total.Count"
                           , "SampleSize")



##################################################
# Step 4: Calculate Percent distribution and SEs
##################################################
# calculate Percent distribution
item1.full1$Percent <- item1.full1$Count / item1.full1$Total.Count
# the denominator of this SE is the sample size of the denominator in the percent
item1.full1$SE      <- sqrt((item1.full1$Percent * (1 - item1.full1$Percent)) / item1.full1$SampleSize)



##################################################
# Step 5: Cast data and create table
##################################################
library(data.table)
item1.table <- dcast(setDT(item1.full1)
                     ,formula = BuildingType + Home.Type ~ State
                     ,value.var = c("Percent", "SE", "Remove.SampleSize"))

item1.table1 <- data.frame("BuildingType"      = item1.table$BuildingType
                             ,"Home.Type"      = item1.table$Home.Type
                             ,"Percent_ID"     = "" #missing for now, only partial data, placeholder until we get full data
                             ,"SE_ID"          = "" #missing for now, only partial data, placeholder until we get full data
                             ,"Percent_MT"     = item1.table$Percent_MT
                             ,"SE_MT"          = item1.table$SE_MT
                             ,"Percent_OR"     = item1.table$Percent_OR
                             ,"SE_OR"          = item1.table$SE_OR
                             ,"Percent_WA"     = item1.table$Percent_WA
                             ,"SE_WA"          = item1.table$SE_WA
                             ,"Percent_Region" = item1.table$Percent_Region
                             ,"SE_Region"      = item1.table$SE_Region
                             ,"SampleSize"     = item1.table$Remove.SampleSize_Region)


##################################################
# Step 6: Split table by building type
# and export to correct workbook
##################################################
item1.table.SF <- item1.table1[which(item1.table1$BuildingType %in% c("Single Family")),-1]
item1.table.MH <- item1.table1[which(item1.table1$BuildingType %in% c("Manufactured")),-1]

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

item2.dat <- rbsa.dat

item2.dat$count <- 1

########################
# Step 1: State
########################
#by vintage
item2.state <- summarise(group_by(item2.dat, BuildingType, HomeYearBuilt_bins, State)
                              ,Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID)))
#across vintages
item2.state.tot <- summarise(group_by(item2.dat, BuildingType, State)
                              ,HomeYearBuilt_bins = "Total"
                              ,Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID)))
#combine
item2.state.full <- rbind.data.frame(item2.state, item2.state.tot, stringsAsFactors = F)


########################
# Step 2: Region (across states)
########################
#by vintage
item2.region <- summarise(group_by(item2.dat, BuildingType, HomeYearBuilt_bins)
                               ,State = "Region"
                               ,Count = sum(count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)
#across vintages
item2.region.tot <- summarise(group_by(item2.dat, BuildingType)
                               ,HomeYearBuilt_bins = "Total"
                               ,State = "Region"
                               ,Count = sum(count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)
#combine
item2.region.full <- rbind.data.frame(item2.region, item2.region.tot, stringsAsFactors = F)


##################################################
# Step 3 (part a): Sample Sizes for SEs
##################################################
# Extract sample sizes for standard error calculations from the Total Row information for states and region
#   and subset to only columns needed
item2.total <- rbind.data.frame(item2.state.tot, item2.region.tot, stringsAsFactors = F)
item2.total1 <- item2.total[which(colnames(item2.total) %in% c("BuildingType"
                                                               , "State"
                                                               , "Count"
                                                               , "SampleSize"))]

#rbind state and region information
item2.full <- rbind.data.frame(item2.state.full, item2.region.full, stringsAsFactors = F)

item2.full1 <- left_join(item2.full, item2.total1, by = c("BuildingType", "State"))
colnames(item2.full1) <- c("BuildingType"
                           , "Housing.Vintage"
                           , "State"
                           , "Count"
                           , "Remove.SampleSize" # Step 3 (part b) -- This sample size is only used for the report table
                           , "Total.Count"
                           , "SampleSize")





##################################################
# Step 4: Calculate Percent distribution and SEs
##################################################
# calculate Percent distribution
item2.full1$Percent <- item2.full1$Count / item2.full1$Total.Count
# the denominator of this SE is the sample size of the denominator in the percent
item2.full1$SE      <- sqrt((item2.full1$Percent * (1 - item2.full1$Percent)) / item2.full1$SampleSize)



##################################################
# Step 5: Cast data and create table
##################################################
library(data.table)
item2.table <- dcast(setDT(item2.full1)
                     ,formula = BuildingType + Housing.Vintage ~ State
                     ,value.var = c("Percent", "SE", "SampleSize"))

item2.table1 <- data.frame("BuildingType"     = item2.table$BuildingType
                           ,"Housing.Vintage" = item2.table$Housing.Vintage
                           ,"Percent_ID"      = NA #missing for now, only partial data, placeholder until we get full data
                           ,"SE_ID"           = NA #missing for now, only partial data, placeholder until we get full data
                           ,"Percent_MT"      = item2.table$Percent_MT
                           ,"SE_MT"           = item2.table$SE_MT
                           ,"Percent_OR"      = item2.table$Percent_OR
                           ,"SE_OR"           = item2.table$SE_OR
                           ,"Percent_WA"      = item2.table$Percent_WA
                           ,"SE_WA"           = item2.table$SE_WA
                           ,"Percent_Region"  = item2.table$Percent_Region
                           ,"SE_Region"       = item2.table$SE_Region
                           ,"SampleSize"      = item2.table$Remove.SampleSize_Region)

item2.table2 <- item2.table1[which(item2.table1$BuildingType %in% c("Single Family", "Manufactured")),]
item2.table.final <- item2.table2[which(!(is.na(item2.table2$Housing.Vintage))),]




##################################################
# Step 6: Split table by building type
# and export to correct workbook
##################################################
item2.table.SF <- item2.table.final[which(item2.table.final$BuildingType == "Single Family"),-1]
item2.table.MH <- item2.table.final[which(item2.table.final$BuildingType == "Manufactured"),-1]


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

