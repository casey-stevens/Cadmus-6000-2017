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
mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))





#############################################################################################
#Item : DISTRIBUTION OF ELECTRICALLY HEATED HOMES BY VINTAGE AND STATE (SF table 156)
#############################################################################################
#subset to columns needed for analysis
item156.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"Primary.Heating.System"
                                                                    ,"Heating.Fuel"
                                                                    ,""
                                                                    ,""))]

#remove any repeat header rows from exporting
item156.dat1 <- item156.dat[which(item156.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item156.dat2 <- item156.dat1[which(item156.dat1$Primary.Heating.System == "Yes"),]
length(unique(item156.dat2$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item156.dat2$Primary.Heating.System)
item156.dat2$count <- 1

item156.dat3 <- unique(item156.dat2[which(item156.dat2$Heating.Fuel == "Electric"),])

item156.sum <- summarise(group_by(item156.dat3, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item156.sum$Count <- 1
which(duplicated(item156.sum$CK_Cadmus_ID)) #none are duplicated!
unique(item156.sum$Heating.Fuel)

item156.merge <- left_join(rbsa.dat, item156.sum)
item156.merge <- item156.merge[which(!is.na(item156.merge$Heating.Fuel)),]
item156.merge <- item156.merge[which(!is.na(item156.merge$HomeYearBuilt_bins2)),]


################################################
# Adding pop and sample sizes for weights
################################################
item156.data <- weightedData(item156.merge[-which(colnames(item156.merge) %in% c("Heating.Fuel"
                                                                               ,"Count"))])
item156.data <- left_join(item156.data, item156.merge[which(colnames(item156.merge) %in% c("CK_Cadmus_ID"
                                                                                         ,"Heating.Fuel"
                                                                                         ,"Count"))])
item156.data$count <- 1
#######################
# Weighted Analysis
#######################
item156.final <- proportionRowsAndColumns1(CustomerLevelData = item156.data
                                           ,valueVariable    = 'Count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'HomeYearBuilt_bins2'
                                           ,aggregateColumnName = "Region")

item156.cast <- dcast(setDT(item156.final)
                      , formula = BuildingType + HomeYearBuilt_bins2 ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item156.table <- data.frame("BuildingType"    = item156.cast$BuildingType
                            ,"Housing.Vintage"= item156.cast$HomeYearBuilt_bins2
                            ,"Percent_ID"     = item156.cast$w.percent_ID
                            ,"SE_ID"          = item156.cast$w.SE_ID
                            ,"Count_ID"       = item156.cast$count_ID
                            ,"n_ID"           = item156.cast$n_ID
                            ,"Percent_MT"     = item156.cast$w.percent_MT
                            ,"SE_MT"          = item156.cast$w.SE_MT
                            ,"Count_MT"       = item156.cast$count_MT
                            ,"n_MT"           = item156.cast$n_MT
                            ,"Percent_OR"     = item156.cast$w.percent_OR
                            ,"SE_OR"          = item156.cast$w.SE_OR
                            ,"Count_OR"       = item156.cast$count_OR
                            ,"n_OR"           = item156.cast$n_OR
                            ,"Percent_WA"     = item156.cast$w.percent_WA
                            ,"SE_WA"          = item156.cast$w.SE_WA
                            ,"Count_WA"       = item156.cast$count_WA
                            ,"n_WA"           = item156.cast$n_WA
                            ,"Percent_Region" = item156.cast$w.percent_Region
                            ,"SE_Region"      = item156.cast$w.SE_Region
                            ,"Count_Region"   = item156.cast$count_Region
                            ,"n_Region"       = item156.cast$n_Region
                            ,"EB_ID"          = item156.cast$EB_ID
                            ,"EB_MT"          = item156.cast$EB_MT
                            ,"EB_OR"          = item156.cast$EB_OR
                            ,"EB_WA"          = item156.cast$EB_WA
                            ,"EB_Region"      = item156.cast$EB_Region
)

# If final table have <NA> something was named incorrectly
levels(item156.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"Total")
item156.table <- item156.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item156.table <- data.frame(item156.table)

item156.final.SF <- item156.table[which(item156.table$BuildingType == "Single Family")
                                  ,-which(colnames(item156.table) %in% c("BuildingType"))]

exportTable(item156.final.SF, "SF", "Table B-1", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item156.final <- proportions_two_groups_unweighted(CustomerLevelData = item156.data
                                                   ,valueVariable    = 'Count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'HomeYearBuilt_bins2'
                                                   ,aggregateColumnName = "Region")

item156.cast <- dcast(setDT(item156.final)
                      , formula = BuildingType + HomeYearBuilt_bins2 ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


item156.table <- data.frame("BuildingType"    = item156.cast$BuildingType
                            ,"Housing.Vintage"= item156.cast$HomeYearBuilt_bins2
                            ,"Percent_ID"     = item156.cast$Percent_ID
                            ,"SE_ID"          = item156.cast$SE_ID
                            ,"Count_ID"       = item156.cast$Count_ID
                            ,"n_ID"           = item156.cast$n_ID
                            ,"Percent_MT"     = item156.cast$Percent_MT
                            ,"SE_MT"          = item156.cast$SE_MT
                            ,"Count_MT"       = item156.cast$Count_MT
                            ,"n_MT"           = item156.cast$n_MT
                            ,"Percent_OR"     = item156.cast$Percent_OR
                            ,"SE_OR"          = item156.cast$SE_OR
                            ,"Count_OR"       = item156.cast$Count_OR
                            ,"n_OR"           = item156.cast$n_OR
                            ,"Percent_WA"     = item156.cast$Percent_WA
                            ,"SE_WA"          = item156.cast$SE_WA
                            ,"Count_WA"       = item156.cast$Count_WA
                            ,"n_WA"           = item156.cast$n_WA
                            ,"Percent_Region" = item156.cast$Percent_Region
                            ,"SE_Region"      = item156.cast$SE_Region
                            ,"Count_Region"   = item156.cast$Count_Region
                            ,"n_Region"       = item156.cast$n_Region
)

# If final table have <NA> something was named incorrectly
levels(item156.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"Total")
item156.table <- item156.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item156.table <- data.frame(item156.table)

item156.final.SF <- item156.table[which(item156.table$BuildingType == "Single Family")
                                  ,-which(colnames(item156.table) %in% c("BuildingType"))]

exportTable(item156.final.SF, "SF", "Table B-1", weighted = FALSE)

