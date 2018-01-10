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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData ,paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
rbsa.dat <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),]
#############################################################################################
# Item 1 : DISTRIBUTION OF HOMES BY TYPE AND STATE (SF Table 8, MH Table 7)
#############################################################################################
item1.dat0 <- rbsa.dat[which(!is.na(rbsa.dat$BuildingTypeXX)),]
item1.dat <- weightedData(item1.dat0)

item1.dat$count <- 1



######################################
# Weighted Analysis
######################################
item1.final <- proportionRowsAndColumns1(CustomerLevelData = item1.dat
                                         ,valueVariable = 'count'
                                         ,columnVariable = 'State'
                                         ,rowVariable = 'HomeType'
                                         ,aggregateColumnName = "Region")

#cast data into correct format
item1.cast <- dcast(setDT(item1.final)
                     ,formula = BuildingType + HomeType ~ State
                     ,value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

#can add pop and sample sizes if needed in exported table
item1.table <- data.frame("BuildingType"    = item1.cast$BuildingType
                          ,"Home.Type"      = item1.cast$HomeType
                          ,"Percent_ID"     = item1.cast$w.percent_ID
                          ,"SE_ID"          = item1.cast$w.SE_ID
                          ,"n_ID"           = item1.cast$n_ID
                          ,"Percent_MT"     = item1.cast$w.percent_MT
                          ,"SE_MT"          = item1.cast$w.SE_MT
                          ,"n_MT"           = item1.cast$n_MT
                          ,"Percent_OR"     = item1.cast$w.percent_OR
                          ,"SE_OR"          = item1.cast$w.SE_OR
                          ,"n_OR"           = item1.cast$n_OR
                          ,"Percent_WA"     = item1.cast$w.percent_WA
                          ,"SE_WA"          = item1.cast$w.SE_WA
                          ,"n_WA"           = item1.cast$n_WA
                          ,"Percent_Region" = item1.cast$w.percent_Region
                          ,"SE_Region"      = item1.cast$w.SE_Region
                          ,"n"              = item1.cast$n_Region
                          ,"EB_ID"          = item1.cast$EB_ID
                          ,"EB_MT"          = item1.cast$EB_MT
                          ,"EB_OR"          = item1.cast$EB_OR
                          ,"EB_WA"          = item1.cast$EB_WA
                          ,"EB_Region"      = item1.cast$EB_Region
                          ,"")

# row ordering example code
levels(item1.table$Home.Type)
rowOrder <- c("Single Family Detached"
              ,"Duplex, Triplex, or Fourplex"
              ,"Townhome or Rowhome"
              ,"Apartment Building (3 or fewer floors)"
              ,"Apartment Building (4 to 6 floors)"
              ,"Apartment Building (More than 6 floors)"
              ,"Single Wide"
              ,"Double Wide"
              ,"Triple Wide"
              ,"Modular / Prefab"
              ,"Total")
item1.table <- item1.table %>% mutate(Home.Type = factor(Home.Type, levels = rowOrder)) %>% arrange(Home.Type)  
item1.table <- data.frame(item1.table)


### Split into respective tables
item1.table.SF <- data.frame(item1.table[which(item1.table$BuildingType %in% c("Single Family")),-1], stringsAsFactors = F)
item1.table.MH <- item1.table[which(item1.table$BuildingType %in% c("Manufactured")),-1]

#exporting function
exportTable(item1.table.SF, "SF", "Table 8"
            , weighted = TRUE)
exportTable(item1.table.MH, "MH", "Table 7"
            , weighted = TRUE)



######################################
# Unweighted Analysis
######################################
item1.final.unweighted <- proportions_two_groups_unweighted(CustomerLevelData = item1.dat
                                                            ,valueVariable = 'count'
                                                            ,columnVariable = 'State'
                                                            ,rowVariable = 'HomeType'
                                                            ,aggregateColumnName = "Region")


#cast data into correct format
item1.cast.unw <- dcast(setDT(item1.final.unweighted)
                    ,formula = BuildingType + HomeType ~ State
                    ,value.var = c("Percent", "SE", "Count", "n"))

#can add pop and sample sizes if needed in exported table
item1.table.unw <- data.frame("BuildingType"= item1.cast.unw$BuildingType
                          ,"Home.Type"      = item1.cast.unw$HomeType
                          ,"Percent_ID"     = item1.cast.unw$Percent_ID
                          ,"SE_ID"          = item1.cast.unw$SE_ID
                          ,"n_ID"           = item1.cast.unw$n_ID
                          ,"Percent_MT"     = item1.cast.unw$Percent_MT
                          ,"SE_MT"          = item1.cast.unw$SE_MT
                          ,"n_MT"           = item1.cast.unw$n_MT
                          ,"Percent_OR"     = item1.cast.unw$Percent_OR
                          ,"SE_OR"          = item1.cast.unw$SE_OR
                          ,"n_OR"           = item1.cast.unw$n_OR
                          ,"Percent_WA"     = item1.cast.unw$Percent_WA
                          ,"SE_WA"          = item1.cast.unw$SE_WA
                          ,"n_WA"           = item1.cast.unw$n_WA
                          ,"Percent_Region" = item1.cast.unw$Percent_Region
                          ,"SE_Region"      = item1.cast.unw$SE_Region
                          ,"n"              = item1.cast.unw$n_Region)

# row ordering example code
levels(item1.table.unw$Home.Type)
rowOrder <- c("Single Family Detached"
              ,"Duplex, Triplex, or Fourplex"
              ,"Townhome or Rowhome"
              ,"Apartment Building (3 or fewer floors)"
              ,"Apartment Building (4 to 6 floors)"
              ,"Apartment Building (More than 6 floors)"
              ,"Single Wide"
              ,"Double Wide"
              ,"Triple Wide"
              ,"Modular / Prefab"
              ,"Total")
item1.table.unw <- item1.table.unw %>% mutate(Home.Type = factor(Home.Type, levels = rowOrder)) %>% arrange(Home.Type)  
item1.table.unw <- data.frame(item1.table.unw)


### Split into respective tables
item1.table.SF.unw <- item1.table.unw[which(item1.table.unw$BuildingType %in% c("Single Family")),-1]
item1.table.MH.unw <- item1.table.unw[which(item1.table.unw$BuildingType %in% c("Manufactured")),-1]

#exporting function
exportTable(item1.table.SF.unw, "SF", "Table 8"
            , weighted = FALSE)
exportTable(item1.table.MH.unw, "MH", "Table 7"
            , weighted = FALSE)







#############################################################################################
# 
# Item 2 : DISTRIBUTION OF HOMES BY VINTAGE AND STATE (SF Table 9, MH Table 8)
# 
#############################################################################################
item2.dat <- weightedData(rbsa.dat[which(!is.na(rbsa.dat$HomeYearBuilt)),])

item2.dat$count <- 1


############################
# weighted Analysis
############################
item2.final <- proportionRowsAndColumns1(item2.dat
                          , valueVariable = 'count'
                          , columnVariable = 'State'
                          , rowVariable = 'HomeYearBuilt_bins2'
                          , aggregateColumnName = "Region"
                          )

item2.cast <- dcast(setDT(item2.final)
                     ,formula = BuildingType + HomeYearBuilt_bins2 ~ State
                     ,value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item2.table <- data.frame("BuildingType"     = item2.cast$BuildingType
                           ,"Housing.Vintage" = item2.cast$HomeYearBuilt_bins2
                           ,"Percent_ID"      = item2.cast$w.percent_ID
                           ,"SE_ID"           = item2.cast$w.SE_ID
                           ,"n_ID"            = item2.cast$n_ID
                           ,"Percent_MT"      = item2.cast$w.percent_MT
                           ,"SE_MT"           = item2.cast$w.SE_MT
                           ,"n_MT"            = item2.cast$n_MT
                           ,"Percent_OR"      = item2.cast$w.percent_OR
                           ,"SE_OR"           = item2.cast$w.SE_OR
                           ,"n_OR"            = item2.cast$n_OR
                           ,"Percent_WA"      = item2.cast$w.percent_WA
                           ,"SE_WA"           = item2.cast$w.SE_WA
                           ,"n_WA"            = item2.cast$n_WA
                           ,"Percent_Region"  = item2.cast$w.percent_Region
                           ,"SE_Region"       = item2.cast$w.SE_Region
                           ,"n"               = item2.cast$n_Region
                          ,"EB_ID"          = item2.cast$EB_ID
                          ,"EB_MT"          = item2.cast$EB_MT
                          ,"EB_OR"          = item2.cast$EB_OR
                          ,"EB_WA"          = item2.cast$EB_WA
                          ,"EB_Region"      = item2.cast$EB_Region)

# row ordering example code
levels(item2.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"Total")
item2.table <- item2.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item2.table <- data.frame(item2.table)


item2.table.SF <- item2.table[which(item2.table$BuildingType == "Single Family"),-1]
item2.table.MH <- item2.table[which(item2.table$BuildingType == "Manufactured"),-1]

#exporting function
exportTable(item2.table.SF, "SF", "Table 9"
            , weighted = TRUE)
exportTable(item2.table.MH, "MH", "Table 8"
            , weighted = TRUE)




############################
# Unweighted Analysis
############################
item2.final <- proportions_two_groups_unweighted(item2.dat
                                                 , valueVariable = 'count'
                                                 , columnVariable = 'State'
                                                 , rowVariable = 'HomeYearBuilt_bins2'
                                                 , aggregateColumnName = "Region")

item2.cast <- dcast(setDT(item2.final)
                    ,formula = BuildingType + HomeYearBuilt_bins2 ~ State
                    ,value.var = c("Percent", "SE", "n", "Count"))

item2.table <- data.frame("BuildingType"     = item2.cast$BuildingType
                          ,"Housing.Vintage" = item2.cast$HomeYearBuilt_bins2
                          ,"Percent_ID"      = item2.cast$Percent_ID
                          ,"SE_ID"           = item2.cast$SE_ID
                          ,"n_ID"            = item2.cast$n_ID
                          ,"Percent_MT"      = item2.cast$Percent_MT
                          ,"SE_MT"           = item2.cast$SE_MT
                          ,"n_MT"            = item2.cast$n_MT
                          ,"Percent_OR"      = item2.cast$Percent_OR
                          ,"SE_OR"           = item2.cast$SE_OR
                          ,"n_OR"            = item2.cast$n_OR
                          ,"Percent_WA"      = item2.cast$Percent_WA
                          ,"SE_WA"           = item2.cast$SE_WA
                          ,"n_WA"            = item2.cast$n_WA
                          ,"Percent_Region"  = item2.cast$Percent_Region
                          ,"SE_Region"       = item2.cast$SE_Region
                          ,"n_Region"        = item2.cast$n_Region)

# row ordering example code
levels(item2.table$Housing.Vintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"Total")
item2.table <- item2.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item2.table <- data.frame(item2.table)

item2.table.SF <- item2.table[which(item2.table$BuildingType == "Single Family"),-1]
item2.table.MH <- item2.table[which(item2.table$BuildingType == "Manufactured"),-1]

#exporting function
exportTable(item2.table.SF, "SF", "Table 9"
            , weighted = FALSE)
exportTable(item2.table.MH, "MH", "Table 8"
            , weighted = FALSE)








#############################################################################################
#
# Item 6: DISTRIBUTION OF HOMES BY BUILDING HEIGHT AND STATE (SF table 13)
#
#############################################################################################
item6.dat <- weightedData(rbsa.dat[which(!is.na(rbsa.dat$BuildingHeight)),])
item6.dat$BuildingHeight[which(item6.dat$BuildingHeight %in% c(1.0))] <- "1 Story"
item6.dat$BuildingHeight[which(item6.dat$BuildingHeight %in% c(1.5))] <- "1.5 Stories"
item6.dat$BuildingHeight[which(item6.dat$BuildingHeight %in% c(2.0))] <- "2 Stories"
item6.dat$BuildingHeight[which(item6.dat$BuildingHeight %in% c(2.5))] <- "2.5 Stories"
item6.dat$BuildingHeight[which(item6.dat$BuildingHeight %in% c(3.0,4.0))] <- "3+ Stories"
unique(item6.dat$BuildingHeight)
item6.dat$count <- 1



############################
# weighted Analysis
############################
item6.final <- proportionRowsAndColumns1(item6.dat
                                         , valueVariable = 'count'
                                         , columnVariable = 'State'
                                         , rowVariable = 'BuildingHeight'
                                         , aggregateColumnName = "Region"
                                         )

colnames(item6.final) <- c("BuildingType"
                           , "State"
                           , "BuildingHeight"
                           , "Percent"
                           , "SE"
                           , "Count"
                           , "N"
                           , "n"
                           , "EB")

item6.cast <- dcast(setDT(item6.final)
                     ,formula = BuildingType + BuildingHeight ~ State
                     ,value.var = c("Percent", "SE", "Count", "N", "n", "EB"))

item6.table <- data.frame("BuildingType"    = item6.cast$BuildingType
                          ,"BuildingHeight" = item6.cast$BuildingHeight
                          ,"Percent_ID"     = item6.cast$Percent_ID
                          ,"SE_ID"          = item6.cast$SE_ID
                          ,"n_ID"           = item6.cast$n_ID
                          ,"Percent_MT"     = item6.cast$Percent_MT
                          ,"SE_MT"          = item6.cast$SE_MT
                          ,"n_MT"           = item6.cast$n_MT
                          ,"Percent_OR"     = item6.cast$Percent_OR
                          ,"SE_OR"          = item6.cast$SE_OR
                          ,"n_OR"           = item6.cast$n_OR
                          ,"Percent_WA"     = item6.cast$Percent_WA
                          ,"SE_WA"          = item6.cast$SE_WA
                          ,"n_WA"           = item6.cast$n_WA
                          ,"Percent_Region" = item6.cast$Percent_Region
                          ,"SE_Region"      = item6.cast$SE_Region
                          ,"n_Region"       = item6.cast$n_Region
                          ,"EB_ID"          = item6.cast$EB_ID
                          ,"EB_MT"          = item6.cast$EB_MT
                          ,"EB_OR"          = item6.cast$EB_OR
                          ,"EB_WA"          = item6.cast$EB_WA
                          ,"EB_Region"      = item6.cast$EB_Region)

item6.table.SF <- item6.table[which(item6.table$BuildingType == "Single Family"),-1]
exportTable(item6.table.SF, "SF", "Table 13"
            , weighted = TRUE)





############################
# Unweighted Analysis
############################
item6.final <- proportions_two_groups_unweighted(item6.dat
                                         , valueVariable = 'count'
                                         , columnVariable = 'State'
                                         , rowVariable = 'BuildingHeight'
                                         , aggregateColumnName = "Region"
)

colnames(item6.final) <- c("BuildingType"
                           , "State"
                           , "BuildingHeight"
                           , "Count"
                           , "n"
                           , "Percent"
                           , "SE")

item6.cast <- dcast(setDT(item6.final)
                    ,formula = BuildingType + BuildingHeight ~ State
                    ,value.var = c("Percent", "SE", "Count", "n"))

item6.table <- data.frame("BuildingType"    = item6.cast$BuildingType
                          ,"BuildingHeight" = item6.cast$BuildingHeight
                          ,"Percent_ID"     = item6.cast$Percent_ID
                          ,"SE_ID"          = item6.cast$SE_ID
                          ,"n_ID"           = item6.cast$n_ID
                          ,"Percent_MT"     = item6.cast$Percent_MT
                          ,"SE_MT"          = item6.cast$SE_MT
                          ,"n_MT"           = item6.cast$n_MT
                          ,"Percent_OR"     = item6.cast$Percent_OR
                          ,"SE_OR"          = item6.cast$SE_OR
                          ,"n_OR"           = item6.cast$n_OR
                          ,"Percent_WA"     = item6.cast$Percent_WA
                          ,"SE_WA"          = item6.cast$SE_WA
                          ,"n_WA"           = item6.cast$n_WA
                          ,"Percent_Region" = item6.cast$Percent_Region
                          ,"SE_Region"      = item6.cast$SE_Region
                          ,"n_Region"       = item6.cast$n_Region)

item6.table.SF <- item6.table[which(item6.table$BuildingType == "Single Family"),-1]
exportTable(item6.table.SF, "SF", "Table 13"
            , weighted = FALSE)
