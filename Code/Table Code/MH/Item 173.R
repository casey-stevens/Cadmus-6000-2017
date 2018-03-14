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

#Read in data for analysis
# sites.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.export))
#clean cadmus IDs
sites.dat$CK_Cadmus_ID <- trimws(toupper(sites.dat$CK_Cadmus_ID))



#############################################################################################
#Item 173: DISTRIBUTION OF HOMES BY AGE/STANDARD AND STATE (MH TABLE 9)
#############################################################################################
#subset to columns needed for analysis
item173.dat <- unique(sites.dat[which(colnames(sites.dat) %in% c("CK_Cadmus_ID"
                                                                 ,"SITE_Construction_CONSTRUCTION_STANDARD_AgeAndConstructionStandard"
                                                                 ,""))])
names(item173.dat) <- c("CK_Cadmus_ID", "Age.and.Construction.Standard")
item173.dat$count <- 1

#remove any repeat header rows from exporting
item173.dat0 <- item173.dat[which(item173.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item173.dat1 <- left_join(rbsa.dat, item173.dat0, by = "CK_Cadmus_ID")
unique(item173.dat1$Age.and.Construction.Standard)

item173.dat2 <- item173.dat1[which(!(is.na(item173.dat1$Age.and.Construction.Standard))),]
item173.dat3 <- item173.dat2[which(item173.dat2$Age.and.Construction.Standard %notin% c("N/A", "Unknown", "1977")),]
unique(item173.dat3$Age.and.Construction.Standard)

################################################
# Adding pop and sample sizes for weights
################################################
item173.data <- weightedData(item173.dat3[-which(colnames(item173.dat3) %in% c("Age.and.Construction.Standard"
                                                                                 ,"count"))])
item173.data <- left_join(item173.data, item173.dat3[which(colnames(item173.dat3) %in% c("CK_Cadmus_ID"
                                                                                           ,"Age.and.Construction.Standard"
                                                                                         ,"count"))])


#######################
# Weighted Analysis
#######################
item173.final <- proportionRowsAndColumns1(CustomerLevelData = item173.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'Age.and.Construction.Standard'
                                           ,aggregateColumnName = "Region")

item173.cast <- dcast(setDT(item173.final)
                      , formula = BuildingType + Age.and.Construction.Standard ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item173.table <- data.frame("BuildingType"    = item173.cast$BuildingType
                            ,"Age_Standard"   = item173.cast$Age.and.Construction.Standard
                            ,"Percent_ID"     = item173.cast$w.percent_ID
                            ,"SE_ID"          = item173.cast$w.SE_ID
                            ,"n_ID"           = item173.cast$n_ID
                            ,"Percent_MT"     = item173.cast$w.percent_MT
                            ,"SE_MT"          = item173.cast$w.SE_MT
                            ,"n_MT"           = item173.cast$n_MT
                            ,"Percent_OR"     = item173.cast$w.percent_OR
                            ,"SE_OR"          = item173.cast$w.SE_OR
                            ,"n_OR"           = item173.cast$n_OR
                            ,"Percent_WA"     = item173.cast$w.percent_WA
                            ,"SE_WA"          = item173.cast$w.SE_WA
                            ,"n_WA"           = item173.cast$n_WA
                            ,"Percent_Region" = item173.cast$w.percent_Region
                            ,"SE_Region"      = item173.cast$w.SE_Region
                            ,"n_Region"       = item173.cast$n_Region
                            ,"EB_ID"          = item173.cast$EB_ID
                            ,"EB_MT"          = item173.cast$EB_MT
                            ,"EB_OR"          = item173.cast$EB_OR
                            ,"EB_WA"          = item173.cast$EB_WA
                            ,"EB_Region"      = item173.cast$EB_Region
)


item173.final.MH <- item173.table[which(item173.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item173.table) %in% c("BuildingType"))]

exportTable(item173.final.MH, "MH", "Table 9", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item173.final <- proportions_two_groups_unweighted(CustomerLevelData = item173.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'Age.and.Construction.Standard'
                                                   ,aggregateColumnName = "Region")


item173.cast <- dcast(setDT(item173.final)
                      , formula = BuildingType + Age.and.Construction.Standard ~ State
                      , value.var = c("Percent", "SE", "n"))


item173.table <- data.frame("BuildingType"    = item173.cast$BuildingType
                            ,"Age_Standard"   = item173.cast$Age.and.Construction.Standard
                            ,"Percent_ID"     = item173.cast$Percent_ID
                            ,"SE_ID"          = item173.cast$SE_ID
                            ,"n_ID"           = item173.cast$n_ID
                            ,"Percent_MT"     = item173.cast$Percent_MT
                            ,"SE_MT"          = item173.cast$SE_MT
                            ,"n_MT"           = item173.cast$n_MT
                            ,"Percent_OR"     = item173.cast$Percent_OR
                            ,"SE_OR"          = item173.cast$SE_OR
                            ,"n_OR"           = item173.cast$n_OR
                            ,"Percent_WA"     = item173.cast$Percent_WA
                            ,"SE_WA"          = item173.cast$SE_WA
                            ,"n_WA"           = item173.cast$n_WA
                            ,"Percent_Region" = item173.cast$Percent_Region
                            ,"SE_Region"      = item173.cast$SE_Region
                            ,"n_Region"       = item173.cast$n_Region
)


item173.final.MH <- item173.table[which(item173.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item173.table) %in% c("BuildingType"))]

exportTable(item173.final.MH, "MH", "Table 9", weighted = FALSE)

