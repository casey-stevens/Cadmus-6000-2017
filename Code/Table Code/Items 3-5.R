#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:          09/11/2017                                  
##  Billing Code(s):  6596.0000.0002.1002.0000
#############################################################################################
#############################################################################################
# For items 3-5 - this code will summarize information to match the previous RBSA table
#   Step 1: Subset to columns for analysis, normalize/clean, and 
#     subset to correct building types needed for analysis
#   Step 1.1 (FOR MEANS ONLY): Summarise data up to unique customer level
#   Step 1.2 (FOR MEANS ONLY): Summarise customer data up to strata level
#   Step 2: State level analysis
#   Step 3: Region level analysis 
#   Step 4: combine full data into one, put data into correct format for creating tables,
#           subset tables by building type and export to respective workbooks
#############################################################################################
##  Clear variables
rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)

# Source
source("Code/Table Code/SourceCode.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565


#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

# Bring in clean ground contact types
GroundContactTypes <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "Ground Contact Types.xlsx"), sheet = 1)
GroundContactTypes <- GroundContactTypes[which(colnames(GroundContactTypes) != "Notes")]




#############################################################################################
# Item 3: DISTRIBUTION OF HOMES BY GROUND CONTACT TYPE AND STATE 
#############################################################################################

######################################################
# Step 1: Subset to columns needed for analysis
#         Perform data normalization / data cleaning
#         subset to only single family homes
######################################################
env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                            , "ENV_Construction_BLDG_STRUCTURE_FoundationType"))]
colnames(env.dat) <- c("CK_Cadmus_ID"
                       , "FoundationType")
env.dat1 <- env.dat[which(!(is.na(env.dat$FoundationType))),]

#merge table columns to generic columns
item3.dat <- unique(left_join(rbsa.dat, env.dat1, by = "CK_Cadmus_ID"))
item3.dat$FoundationType <- trimws(item3.dat$FoundationType)


# Clean Ground Contact types
item3.dat$GroundContact <- item3.dat$FoundationType
for (i in 1:length(GroundContactTypes$Raw.data.categories)){
  item3.dat$GroundContact[which(item3.dat$GroundContact == GroundContactTypes$Raw.data.categories[i])] <- GroundContactTypes$New.categories[i]
}
# End cleaning Step
unique(item3.dat$GroundContact)

# Remove unwanted ground contact types
item3.dat1 <- item3.dat[which(item3.dat$GroundContact != "Remove"),]

#subset to only single family for item 3
item3.dat2 <- item3.dat1[which(item3.dat1$BuildingType == "Single Family"),]


######################################################
# Step 2: State Analysis 
######################################################
item3.dat2$count <- 1
#Get state information
#across home types
item3.state.tot <- summarise(group_by(item3.dat2, BuildingType, State)
                              ,GroundContact = "Total"
                              ,Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID)))

item3.state <- summarise(group_by(item3.dat2, BuildingType, GroundContact, State)
                              ,Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID)))
item3.state <- rbind.data.frame(item3.state, item3.state.tot, stringsAsFactors = F)


######################################################
# Step 3: Region Analysis 
######################################################
#
item3.region.tot <- summarise(group_by(item3.dat2, BuildingType)
                               ,GroundContact = "Total"
                               ,State = "Region"
                               ,Count = sum(count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)
item3.region <- summarise(group_by(item3.dat2, BuildingType, GroundContact)
                               ,State = "Region"
                               ,Count = sum(count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)
item3.region <- rbind.data.frame(item3.region, item3.region.tot, stringsAsFactors = F)

#rbind state and region information
item3.full <- rbind.data.frame(item3.state, item3.region, stringsAsFactors = F)



##################################################
# Step 4 (part a): Sample Sizes for SEs
##################################################
# Extract sample sizes for standard error calculations from the Total Row information for states and region
#   and subset to only columns needed
item3.total <- rbind.data.frame(item3.state.tot, item3.region.tot, stringsAsFactors = F)
item3.total1 <- item3.total[which(colnames(item3.total) %in% c("BuildingType"
                                                               , "State"
                                                               , "Count"
                                                               , "SampleSize"))]

# merge SE sample sizes onto rest of data
item3.final <- left_join(item3.full, item3.total1, by = c("BuildingType", "State"))
colnames(item3.final) <- c("BuildingType"
                           , "GroundContact"
                           , "State"
                           , "Count"
                           , "Remove.SampleSize"
                           , "Total.Count"
                           , "SampleSize")



##################################################
# Step 5: Calculate Percent distribution and SEs
##################################################
# calculate Percent distribution
item3.final$Percent <- item3.final$Count / item3.final$Total.Count
# the denominator of this SE is the sample size of the denominator in the percent
item3.final$SE      <- sqrt((item3.final$Percent * (1 - item3.final$Percent)) / item3.final$SampleSize)


## Step 4 (part b): Fix sample size for Region as they should appear in the report tables 
#     Note: this needs to be done after the SEs have been calcualted
item3.final$SampleSize[which(item3.final$State == "Region")] <- item3.final$Remove.SampleSize[which(item3.final$State == "Region")]




##################################################
# Step 5: Cast data and create table
##################################################
library(data.table)
item3.table <- dcast(setDT(item3.final)
                     ,formula = BuildingType + GroundContact ~ State
                     ,value.var = c("Percent", "SE", "SampleSize"))

item3.table1 <- data.frame("BuildingType" = item3.table$BuildingType
                           ,"GroundContact" = item3.table$GroundContact
                           ,"Percent_MT" = item3.table$Percent_MT
                           ,"SE_MT" = item3.table$SE_MT
                           ,"Percent_OR" = item3.table$Percent_OR
                           ,"SE_OR" = item3.table$SE_OR
                           ,"Percent_WA" = item3.table$Percent_WA
                           ,"SE_WA" = item3.table$SE_WA
                           ,"Percent_Region" = item3.table$Percent_Region
                           ,"SE_Region" = item3.table$SE_Region
                           ,"SampleSize" = item3.table$SampleSize_Region)

item3.table.final <- item3.table1[which(item3.table1$BuildingType %in% c("Single Family")),]

##################################################
# Step 6: Split table by building type
# and export to correct workbook
##################################################
item3.table.SF <- item3.table.final[which(item3.table.final$BuildingType == "Single Family"),-1]


library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.SF, sheet = "Table 10", x = item3.table.SF, startRow = 20)

saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)










#############################################################################################
# Item 4: AVERAGE CONDITIONED FLOOR AREA BY STATE
#############################################################################################
######################################################
# Step 1: Subset to columns needed for analysis
#         Perform data normalization / data cleaning
#         subset to only single family homes
######################################################
item4.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                              , "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]
colnames(item4.dat) <- c("CK_Cadmus_ID"
                         , "BldgLevel_Area_SqFt")

#merge
item4.dat0 <- left_join(rbsa.dat, item4.dat, by = "CK_Cadmus_ID")
length(unique(item4.dat0$CK_Cadmus_ID)) #601

item4.dat1 <- item4.dat0[which(item4.dat0$BuildingType != "Multifamily"),]

#make conditioned area as.numeric
item4.dat1$ConditionedArea <- as.numeric(as.character(item4.dat1$BldgLevel_Area_SqFt))

#remove NAs
item4.dat2 <- item4.dat1[which(!(is.na(item4.dat1$ConditionedArea))),]
length(unique(item4.dat2$CK_Cadmus_ID)) #374


######################################################
# Step 1.1: Summarise data up to unique customer level
######################################################
item4.customer <- summarise(group_by(item4.dat2,BuildingType , CK_Cadmus_ID, State, Region, Territory, n.h, N.h)
                      ,siteAreaConditioned = sum(ConditionedArea)
)

######################################################
# Step 1.2: Using customer level data,
#   Summarise data up to strata level
######################################################
item4.strata <- summarise(group_by(item4.customer, BuildingType, State, Region, Territory)
                              ,n_h        = unique(n.h)
                              ,N_h        = unique(N.h)
                              ,fpc        = (1 - n_h / N_h)
                              ,w_h        = n_h / N_h
                              ,strataArea = sum(siteAreaConditioned) / n_h
                              ,strataSD   = sd(siteAreaConditioned)
                              ,n          = length(unique(CK_Cadmus_ID))
)

item4.strata$strataSD[which(item4.strata$strataSD == "NaN")] <- 0

######################################################
# Step 2: Using strata level data,
#   Perform state level analysis
######################################################
item4.state <- summarise(group_by(item4.strata, BuildingType, State, )
                        ,Mean       = sum(N_h * strataArea) / sum(N_h)
                        ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                        ,SampleSize = sum(unique(n))
                        )


######################################################
# Step 3: Using strata level data,
#   Perform region level analysis
######################################################
item4.region <- summarise(group_by(item4.strata, BuildingType)
                              ,State      = "Region"
                              ,Mean       = sum(N_h * strataArea) / sum(N_h)
                              ,SE         = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2)) / sum(unique(N_h))
                              ,SampleSize = sum(unique(n)))


######################################################
# Step 4: Combine results into correct table format,
#   Split table by building type
#   and export tables to respective workbooks
######################################################
item4.final <- rbind.data.frame(item4.state, item4.region, stringsAsFactors = F) 

### Andres added: issues with exporting - check to see this got resolved
# item4.final.SF <- item4.final[which(item4.final$BuildingType == 'Single Family'),
#                               -which(colnames(item4.final) == 'BuildingType')]
# 
# 
# workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep = "/"))
# writeData(workbook.SF, sheet = "Table 11", x = item4.final.SF, startRow = 20)
# saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)

item4.table.SF <- item4.final[which(item4.final$BuildingType %in% c("Single Family")),-1]
item4.table.MH <- item4.final[which(item4.final$BuildingType %in% c("Manufactured")),-1]


library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"))
workbook.MH <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.SF, sheet = "Table 11", x = item4.table.SF, startRow = 20)
writeData(workbook.MH, sheet = "Table 10", x = item4.table.MH, startRow = 20)

saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)
saveWorkbook(workbook.MH, file = paste(outputFolder, "Tables in Excel - MH - COPY.xlsx", sep="/"), overwrite = T)









##########################################################################
# Item 5: AVERAGE CONDITIONED FLOOR AREA BY STATE AND VINTAGE
##########################################################################
item5.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                              , "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]
colnames(item5.dat) <- c("CK_Cadmus_ID"
                         , "BldgLevel_Area_SqFt")
#merge
item5.dat1 <- left_join(rbsa.dat, item5.dat, by = "CK_Cadmus_ID")
length(unique(item5.dat1$CK_Cadmus_ID)) #565, yay!

#remove NAs
item5.dat2 <- item5.dat1[which(!(is.na(item5.dat1$BldgLevel_Area_SqFt))),]
length(unique(item5.dat2$CK_Cadmus_ID)) #410, boo!

#make conditioned area as.numeric
item5.dat2$ConditionedArea <- as.numeric(as.character(item5.dat2$BldgLevel_Area_SqFt))


#### By state/region across homeyearbuilt
#summarise by state
item5.state.dat00 <- summarise(group_by(item5.dat2, BuildingType, CK_Cadmus_ID, State)
                             ,siteAreaConditioned = sum(ConditionedArea)
)

item5.state.dat01 <- summarise(group_by(item5.state.dat00, BuildingType, State)
                               ,HomeYearBuilt_bins = "All Vintages"
                              ,Mean = mean(siteAreaConditioned)
                              ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,SampleSize = length(unique(CK_Cadmus_ID))
)

#summarise by region
item5.region.dat00 <- summarise(group_by(item5.dat2, BuildingType, CK_Cadmus_ID)
                              ,State = "Region"
                              ,siteAreaConditioned = sum(ConditionedArea)
)

item5.region.dat01 <- summarise(group_by(item5.region.dat00, BuildingType)
                               ,State = "Region"
                               ,HomeYearBuilt_bins = "All Vintages"
                               ,Mean = mean(siteAreaConditioned)
                               ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)

item5.tmp1 <- rbind.data.frame(item5.state.dat01, item5.region.dat01, stringsAsFactors = F) 



#### By state/region and homeyearbuilt bins
#summarise by state
item5.state.dat <- summarise(group_by(item5.dat2, BuildingType, CK_Cadmus_ID, State, HomeYearBuilt_bins)
                             ,siteAreaConditioned = sum(ConditionedArea)
)

item5.state.dat1 <- summarise(group_by(item5.state.dat, BuildingType, State, HomeYearBuilt_bins)
                              ,Mean = mean(siteAreaConditioned)
                              ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,SampleSize = length(unique(CK_Cadmus_ID))
                              )

#summarise by region
item5.region.dat <- summarise(group_by(item5.dat2, BuildingType, CK_Cadmus_ID, HomeYearBuilt_bins)
                              ,State = "Region"
                              ,siteAreaConditioned = sum(ConditionedArea)
)

item5.region.dat1 <- summarise(group_by(item5.region.dat, BuildingType, HomeYearBuilt_bins)
                               ,State = "Region"
                               ,Mean = mean(siteAreaConditioned)
                               ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)

item5.tmp2 <- rbind.data.frame(item5.state.dat1, item5.region.dat1, stringsAsFactors = F) 


item5.final <- rbind.data.frame(item5.tmp1, item5.tmp2, stringsAsFactors = F)


#############################################################################################
library(data.table)
library(gdata)

item5.table <- dcast(setDT(item5.final)
                     ,formula = BuildingType + HomeYearBuilt_bins ~ State
                     ,value.var = c("Mean", "SE", "SampleSize"))

item5.table.tmp <- data.frame(item5.table, stringsAsFactors = F)
target <- c("Pre 1951", "1951-1960", "1961-1970",
             "1971-1980", "1981-1990", "1991-2000",
             "Post 2000", "All Vintages", NA)

item5.table.tmp$HomeYearBuilt_bins <- reorder.factor(item5.table.tmp$HomeYearBuilt_bins, 
                                                     new.order = target)

item5.table.tmp2 <- item5.table.tmp %>% arrange(HomeYearBuilt_bins)

item5.table1 <- data.frame("BuildingType" = item5.table$BuildingType
                           ,"Housing.Vintage" = item5.table$HomeYearBuilt_bins
                           ,"Mean_MT" = item5.table$Mean_MT
                           ,"SE_MT" = item5.table$SE_MT
                           # ,"Mean_OR" = item5.table$Mean_OR
                           # ,"SE_OR" = item5.table$SE_OR
                           ,"Mean_WA" = item5.table$Mean_WA
                           ,"SE_WA" = item5.table$SE_WA
                           ,"Mean_Region" = item5.table$Mean_Region
                           ,"SE_Region" = item5.table$SE_Region
                           ,"SampleSize" = item5.table$SampleSize_Region)

item5.table2 <- item5.table1[which(item5.table1$BuildingType %in% c("Single Family", "Manufactured")),]
item5.table.final <- item5.table2[which(!(is.na(item5.table2$Housing.Vintage))),]


