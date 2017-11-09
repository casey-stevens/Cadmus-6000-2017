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
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))



#############################################################################################
#Item 173: DISTRIBUTION OF HOMES BY AGE/STANDARD AND STATE (MH TABLE 9)
#############################################################################################
#subset to columns needed for analysis
item173.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"Age.and.Construction.Standard"
                                                                                     ,""))])
item173.dat$count <- 1

#remove any repeat header rows from exporting
item173.dat0 <- item173.dat[which(item173.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item173.dat1 <- left_join(item173.dat0, rbsa.dat, by = "CK_Cadmus_ID")
unique(item173.dat1$Age.and.Construction.Standard)

item173.dat2 <- item173.dat1[which(!(is.na(item173.dat1$Age.and.Construction.Standard))),]


#summarise by state
#by age/standard
item173.state1 <- summarise(group_by(item173.dat2, BuildingType, State, Age.and.Construction.Standard)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(count))
#across age/standard
item173.state2 <- summarise(group_by(item173.dat2, BuildingType, State)
                            ,Age.and.Construction.Standard = "Total"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(count))

#summarise across states
#by age/standard
item173.region1 <- summarise(group_by(item173.dat2, BuildingType, Age.and.Construction.Standard)
                            , State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(count))
#across age/standard
item173.region2 <- summarise(group_by(item173.dat2, BuildingType)
                            ,Age.and.Construction.Standard = "Total"
                            , State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(count))

item173.merge <- rbind.data.frame(item173.state1,item173.state2,item173.region1, item173.region2, stringsAsFactors = F)

item173.tot.counts <- rbind.data.frame(item173.state2, item173.region2, stringsAsFactors = F)

item173.final <- left_join(item173.merge, item173.tot.counts, by = c("BuildingType", "State"))
colnames(item173.final) <- c("BuildingType"
                             , "State"
                             , "Age_Construction_Std"
                             , "SampleSize"
                             , "Count"
                             , "Remove"
                             , "Remove"
                             , "TotalCount")

item173.final$Percent <- item173.final$Count / item173.final$TotalCount
item173.final$SE <- sqrt(item173.final$Percent * (1 - item173.final$Percent) / item173.final$SampleSize)

library(data.table)
item173.cast <- dcast(setDT(item173.final)
                      ,formula = BuildingType + Age_Construction_Std ~ State
                      ,value.var = c("Percent", "SE", "SampleSize"))

item173.table <- data.frame("BuildingType" = item173.cast$BuildingType
                             ,"Age_Construction_Standard" = item173.cast$Age_Construction_Std
                             ,"Percent_MT" = item173.cast$Percent_MT
                             ,"SE_MT" = item173.cast$SE_MT
                             ,"Percent_WA" = item173.cast$Percent_WA
                             ,"SE_WA" = item173.cast$SE_WA
                             ,"Percent_Region" = item173.cast$Percent_Region
                             ,"SE_Region" = item173.cast$SE_Region
                             ,"SampleSize" = item173.cast$SampleSize_Region)

item173.table1 <- item173.table[which(item173.table$BuildingType %in% c("Manufactured")),]
