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
source("Code/Table Code/Weighting Implementation - MF-BLDG.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) 
rbsa.dat.site <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat[grep("bldg",rbsa.dat$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))
length(unique(buildings.dat$CK_Building_ID))
buildings.dat.clean <- buildings.dat[which(!duplicated(buildings.dat$CK_Building_ID)),]


#############################################################################################
# Item 212: DISTRIBUTION OF BUILDINGS BY BUILDING SIZE AND VINTAGE (MF table 4)
#############################################################################################
item212A.dat <- rbsa.dat.bldg[which(rbsa.dat.bldg$Category == "PSE"),]
length(unique(item212A.dat$CK_Building_ID[which(!is.na(item212A.dat$HomeYearBuilt))]))

#subset to only MF homes
item212A.dat1 <- item212A.dat[grep("Multifamily", item212A.dat$BuildingType),]

#remove missing vintage information
item212A.dat2 <- unique(item212A.dat1[which(item212A.dat1$HomeYearBuilt %notin% c("N/A",NA)),])
names(item212A.dat2)

################################################
# Adding pop and sample sizes for weights
################################################
item212A.data <- weightedData(item212A.dat2[which(names(item212A.dat2) %notin% c("Category"))])
item212A.data <- left_join(item212A.data, item212A.dat2[which(names(item212A.dat2) %in% c("CK_Cadmus_ID","CK_Building_ID","Category"))])
item212A.data$count <- 1
item212A.data$Count <- 1

#######################
# Weighted Analysis
#######################
item212A.summary <- proportionRowsAndColumns1(CustomerLevelData = item212A.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'HomeYearBuilt_bins_MF'
                                             ,rowVariable = 'HomeType'
                                             ,aggregateColumnName = "All Vintages")
item212A.summary <- item212A.summary[which(item212A.summary$HomeYearBuilt_bins_MF != "All Vintages"),]
item212A.summary <- item212A.summary[which(item212A.summary$HomeType != "Total"),]

item212A.data$HousingType <- item212A.data$HomeType
item212A.all.vintages <- proportions_one_group(CustomerLevelData = item212A.data
                                              ,valueVariable = 'count'
                                              ,groupingVariable = 'HousingType'
                                              ,total.name = "All Vintages"
                                              ,columnName = 'HomeYearBuilt_bins_MF'
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
names(item212A.all.vintages)[which(names(item212A.all.vintages) == "HousingType")] <- "HomeType"
item212A.all.vintages <- item212A.all.vintages[which(item212A.all.vintages$HomeType != "Total"),]


item212A.all.sizes <- proportions_one_group(CustomerLevelData = item212A.data
                                           ,valueVariable = 'count'
                                           ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                           ,total.name = 'Remove'
                                           ,columnName = "HomeType"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)
item212A.all.sizes$HomeYearBuilt_bins_MF[which(item212A.all.sizes$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"


item212A.final <- rbind.data.frame(item212A.summary, item212A.all.vintages, item212A.all.sizes, stringsAsFactors = F)

item212A.cast <- dcast(setDT(item212A.final)
                      ,formula = HomeYearBuilt_bins_MF ~ HomeType
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N", 'EB'))

item212A.table <- data.frame("Housing.Vintage" = item212A.cast$HomeYearBuilt_bins_MF
                            ,"Low-Rise.(1-3)Percent" = item212A.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"Low-Rise.SE"    = item212A.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"Mid-Rise.(4-6)Percent" = item212A.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"Mid-Rise.SE"    = item212A.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"High-Rise.(7+)Percent" = NA#item212A.cast$`w.percent_Apartment Building (More than 6 floors)`
                            ,"High-Rise.SE"   = NA#item212A.cast$`w.SE_Apartment Building (More than 6 floors)`
                            ,"All.Sizes.Percent"      = item212A.cast$`w.percent_Remove`
                            ,"All.Sizes.SE"   = item212A.cast$`w.SE_Remove`
                            ,"n"              = item212A.cast$`n_Remove`
                            ,"Low-Rise.EB"    = item212A.cast$`EB_Apartment Building (3 or fewer floors)`
                            ,"Mid-Rise.EB"    = item212A.cast$`EB_Apartment Building (4 to 6 floors)`
                            ,"High-Rise.EB"   = NA#item212A.cast$`EB_Apartment Building (More than 6 floors)`
                            ,"All.Sizes.EB"   = item212A.cast$`EB_Remove`
                            )

# row ordering example code
levels(item212A.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item212A.table <- item212A.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item212A.table <- data.frame(item212A.table)

exportTable(item212A.table, "MF", "Table 4A", weighted = TRUE, OS = T, osIndicator = "PSE")

#######################
# Unweighted Analysis
#######################
item212A.summary <- proportions_two_groups_unweighted(CustomerLevelData = item212A.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = 'HomeYearBuilt_bins_MF'
                                             ,rowVariable = 'HomeType'
                                             ,aggregateColumnName = "All Vintages")
item212A.summary <- item212A.summary[which(item212A.summary$HomeYearBuilt_bins_MF != "All Vintages"),]
item212A.summary <- item212A.summary[which(item212A.summary$HomeType != "Total"),]

item212A.data$HousingType <- item212A.data$HomeType
item212A.all.vintages <- proportions_one_group(CustomerLevelData = item212A.data
                                              ,valueVariable = 'count'
                                              ,groupingVariable = 'HousingType'
                                              ,total.name = "All Vintages"
                                              ,columnName = 'HomeYearBuilt_bins_MF'
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
names(item212A.all.vintages)[which(names(item212A.all.vintages) == "HousingType")] <- "HomeType"
item212A.all.vintages <- item212A.all.vintages[which(item212A.all.vintages$HomeType != "Total"),]

item212A.all.sizes <- proportions_one_group(CustomerLevelData = item212A.data
                                           ,valueVariable = 'count'
                                           ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                           ,total.name = 'Remove'
                                           ,columnName = "HomeType"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)
item212A.all.sizes$HomeYearBuilt_bins_MF[which(item212A.all.sizes$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"


item212A.final <- rbind.data.frame(item212A.summary, item212A.all.vintages, item212A.all.sizes, stringsAsFactors = F)

item212A.cast <- dcast(setDT(item212A.final)
                      ,formula = HomeYearBuilt_bins_MF ~ HomeType
                      ,value.var = c("Percent", "SE", "Count", "n"))

item212A.table <- data.frame("Housing.Vintage" = item212A.cast$HomeYearBuilt_bins_MF
                            ,"Low-Rise.(1-3)Percent" = item212A.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low-Rise.SE"    = item212A.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Mid-Rise.(4-6)Percent" = item212A.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid-Rise.SE"    = item212A.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"High-Rise.(7+)Percent" = NA#item212A.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High-Rise.SE"   = NA#item212A.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"All.Sizes.Percent"      = item212A.cast$`Percent_Remove`
                            ,"All.Sizes.SE"   = item212A.cast$`SE_Remove`
                            ,"n"              = item212A.cast$`n_Remove`)
# row ordering example code
levels(item212A.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item212A.table <- item212A.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item212A.table <- data.frame(item212A.table)

exportTable(item212A.table, "MF", "Table 4A", weighted = FALSE, OS = T, osIndicator = "PSE")
#############################################################################################
# Item 212: DISTRIBUTION OF BUILDINGS BY BUILDING SIZE AND VINTAGE (MF table 4)
#############################################################################################
item212B.dat <- rbsa.dat.bldg[which(rbsa.dat.bldg$Category == "PSE KING COUNTY"),]
length(unique(item212B.dat$CK_Building_ID[which(!is.na(item212B.dat$HomeYearBuilt))]))

#subset to only MF homes
item212B.dat1 <- item212B.dat[grep("Multifamily", item212B.dat$BuildingType),]

#remove missing vintage information
item212B.dat2 <- unique(item212B.dat1[which(item212B.dat1$HomeYearBuilt %notin% c("N/A",NA)),])
names(item212B.dat2)

################################################
# Adding pop and sample sizes for weights
################################################
item212B.data <- weightedData(item212B.dat2[which(names(item212B.dat2) %notin% c("Category"))])
item212B.data <- left_join(item212B.data, item212B.dat2[which(names(item212B.dat2) %in% c("CK_Cadmus_ID","CK_Building_ID","Category"))])
item212B.data$count <- 1
item212B.data$Count <- 1

#######################
# Weighted Analysis
#######################
item212B.summary <- proportionRowsAndColumns1(CustomerLevelData = item212B.data
                                              ,valueVariable = 'count'
                                              ,columnVariable = 'HomeYearBuilt_bins_MF'
                                              ,rowVariable = 'HomeType'
                                              ,aggregateColumnName = "All Vintages")
item212B.summary <- item212B.summary[which(item212B.summary$HomeYearBuilt_bins_MF != "All Vintages"),]
item212B.summary <- item212B.summary[which(item212B.summary$HomeType != "Total"),]

item212B.data$HousingType <- item212B.data$HomeType
item212B.all.vintages <- proportions_one_group(CustomerLevelData = item212B.data
                                               ,valueVariable = 'count'
                                               ,groupingVariable = 'HousingType'
                                               ,total.name = "All Vintages"
                                               ,columnName = 'HomeYearBuilt_bins_MF'
                                               ,weighted = TRUE
                                               ,two.prop.total = TRUE)
names(item212B.all.vintages)[which(names(item212B.all.vintages) == "HousingType")] <- "HomeType"
item212B.all.vintages <- item212B.all.vintages[which(item212B.all.vintages$HomeType != "Total"),]


item212B.all.sizes <- proportions_one_group(CustomerLevelData = item212B.data
                                            ,valueVariable = 'count'
                                            ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                            ,total.name = 'Remove'
                                            ,columnName = "HomeType"
                                            ,weighted = TRUE
                                            ,two.prop.total = TRUE)
item212B.all.sizes$HomeYearBuilt_bins_MF[which(item212B.all.sizes$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"


item212B.final <- rbind.data.frame(item212B.summary, item212B.all.vintages, item212B.all.sizes, stringsAsFactors = F)

item212B.cast <- dcast(setDT(item212B.final)
                       ,formula = HomeYearBuilt_bins_MF ~ HomeType
                       ,value.var = c("w.percent", "w.SE", "count", "n", "N", 'EB'))

item212B.table <- data.frame("Housing.Vintage" = item212B.cast$HomeYearBuilt_bins_MF
                             ,"Low-Rise.(1-3)Percent" = item212B.cast$`w.percent_Apartment Building (3 or fewer floors)`
                             ,"Low-Rise.SE"    = item212B.cast$`w.SE_Apartment Building (3 or fewer floors)`
                             ,"Mid-Rise.(4-6)Percent" = item212B.cast$`w.percent_Apartment Building (4 to 6 floors)`
                             ,"Mid-Rise.SE"    = item212B.cast$`w.SE_Apartment Building (4 to 6 floors)`
                             ,"High-Rise.(7+)Percent" = NA#item212B.cast$`w.percent_Apartment Building (More than 6 floors)`
                             ,"High-Rise.SE"   = NA#item212B.cast$`w.SE_Apartment Building (More than 6 floors)`
                             ,"All.Sizes.Percent"      = item212B.cast$`w.percent_Remove`
                             ,"All.Sizes.SE"   = item212B.cast$`w.SE_Remove`
                             ,"n"              = item212B.cast$`n_Remove`
                             ,"Low-Rise.EB"    = item212B.cast$`EB_Apartment Building (3 or fewer floors)`
                             ,"Mid-Rise.EB"    = item212B.cast$`EB_Apartment Building (4 to 6 floors)`
                             ,"High-Rise.EB"   = NA#item212B.cast$`EB_Apartment Building (More than 6 floors)`
                             ,"All.Sizes.EB"   = item212B.cast$`EB_Remove`
)

# row ordering example code
levels(item212B.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item212B.table <- item212B.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item212B.table <- data.frame(item212B.table)

exportTable(item212B.table, "MF", "Table 4B", weighted = TRUE, OS = T, osIndicator = "PSE")

#######################
# Unweighted Analysis
#######################
item212B.summary <- proportions_two_groups_unweighted(CustomerLevelData = item212B.data
                                                      ,valueVariable = 'count'
                                                      ,columnVariable = 'HomeYearBuilt_bins_MF'
                                                      ,rowVariable = 'HomeType'
                                                      ,aggregateColumnName = "All Vintages")
item212B.summary <- item212B.summary[which(item212B.summary$HomeYearBuilt_bins_MF != "All Vintages"),]
item212B.summary <- item212B.summary[which(item212B.summary$HomeType != "Total"),]

item212B.data$HousingType <- item212B.data$HomeType
item212B.all.vintages <- proportions_one_group(CustomerLevelData = item212B.data
                                               ,valueVariable = 'count'
                                               ,groupingVariable = 'HousingType'
                                               ,total.name = "All Vintages"
                                               ,columnName = 'HomeYearBuilt_bins_MF'
                                               ,weighted = FALSE
                                               ,two.prop.total = TRUE)
names(item212B.all.vintages)[which(names(item212B.all.vintages) == "HousingType")] <- "HomeType"
item212B.all.vintages <- item212B.all.vintages[which(item212B.all.vintages$HomeType != "Total"),]

item212B.all.sizes <- proportions_one_group(CustomerLevelData = item212B.data
                                            ,valueVariable = 'count'
                                            ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                            ,total.name = 'Remove'
                                            ,columnName = "HomeType"
                                            ,weighted = FALSE
                                            ,two.prop.total = TRUE)
item212B.all.sizes$HomeYearBuilt_bins_MF[which(item212B.all.sizes$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"


item212B.final <- rbind.data.frame(item212B.summary, item212B.all.vintages, item212B.all.sizes, stringsAsFactors = F)

item212B.cast <- dcast(setDT(item212B.final)
                       ,formula = HomeYearBuilt_bins_MF ~ HomeType
                       ,value.var = c("Percent", "SE", "Count", "n"))

item212B.table <- data.frame("Housing.Vintage" = item212B.cast$HomeYearBuilt_bins_MF
                             ,"Low-Rise.(1-3)Percent" = item212B.cast$`Percent_Apartment Building (3 or fewer floors)`
                             ,"Low-Rise.SE"    = item212B.cast$`SE_Apartment Building (3 or fewer floors)`
                             ,"Mid-Rise.(4-6)Percent" = item212B.cast$`Percent_Apartment Building (4 to 6 floors)`
                             ,"Mid-Rise.SE"    = item212B.cast$`SE_Apartment Building (4 to 6 floors)`
                             ,"High-Rise.(7+)Percent" = NA#item212B.cast$`Percent_Apartment Building (More than 6 floors)`
                             ,"High-Rise.SE"   = NA#item212B.cast$`SE_Apartment Building (More than 6 floors)`
                             ,"All.Sizes.Percent"      = item212B.cast$`Percent_Remove`
                             ,"All.Sizes.SE"   = item212B.cast$`SE_Remove`
                             ,"n"              = item212B.cast$`n_Remove`)
# row ordering example code
levels(item212B.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item212B.table <- item212B.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item212B.table <- data.frame(item212B.table)

exportTable(item212B.table, "MF", "Table 4B", weighted = FALSE, OS = T, osIndicator = "PSE")
#############################################################################################
# Item 212: DISTRIBUTION OF BUILDINGS BY BUILDING SIZE AND VINTAGE (MF table 4)
#############################################################################################
item212C.dat <- rbsa.dat.bldg[which(rbsa.dat.bldg$Category == "PSE NON-KING COUNTY"),]
length(unique(item212C.dat$CK_Building_ID[which(!is.na(item212C.dat$HomeYearBuilt))]))

#subset to only MF homes
item212C.dat1 <- item212C.dat[grep("Multifamily", item212C.dat$BuildingType),]

#remove missing vintage information
item212C.dat2 <- unique(item212C.dat1[which(item212C.dat1$HomeYearBuilt %notin% c("N/A",NA)),])
names(item212C.dat2)

################################################
# Adding pop and sample sizes for weights
################################################
item212C.data <- weightedData(item212C.dat2[which(names(item212C.dat2) %notin% c("Category"))])
item212C.data <- left_join(item212C.data, item212C.dat2[which(names(item212C.dat2) %in% c("CK_Cadmus_ID","CK_Building_ID","Category"))])
item212C.data$count <- 1
item212C.data$Count <- 1

#######################
# Weighted Analysis
#######################
item212C.summary <- proportionRowsAndColumns1(CustomerLevelData = item212C.data
                                              ,valueVariable = 'count'
                                              ,columnVariable = 'HomeYearBuilt_bins_MF'
                                              ,rowVariable = 'HomeType'
                                              ,aggregateColumnName = "All Vintages")
item212C.summary <- item212C.summary[which(item212C.summary$HomeYearBuilt_bins_MF != "All Vintages"),]
item212C.summary <- item212C.summary[which(item212C.summary$HomeType != "Total"),]

item212C.data$HousingType <- item212C.data$HomeType
item212C.all.vintages <- proportions_one_group(CustomerLevelData = item212C.data
                                               ,valueVariable = 'count'
                                               ,groupingVariable = 'HousingType'
                                               ,total.name = "All Vintages"
                                               ,columnName = 'HomeYearBuilt_bins_MF'
                                               ,weighted = TRUE
                                               ,two.prop.total = TRUE)
names(item212C.all.vintages)[which(names(item212C.all.vintages) == "HousingType")] <- "HomeType"
item212C.all.vintages <- item212C.all.vintages[which(item212C.all.vintages$HomeType != "Total"),]


item212C.all.sizes <- proportions_one_group(CustomerLevelData = item212C.data
                                            ,valueVariable = 'count'
                                            ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                            ,total.name = 'Remove'
                                            ,columnName = "HomeType"
                                            ,weighted = TRUE
                                            ,two.prop.total = TRUE)
item212C.all.sizes$HomeYearBuilt_bins_MF[which(item212C.all.sizes$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"


item212C.final <- rbind.data.frame(item212C.summary, item212C.all.vintages, item212C.all.sizes, stringsAsFactors = F)

item212C.cast <- dcast(setDT(item212C.final)
                       ,formula = HomeYearBuilt_bins_MF ~ HomeType
                       ,value.var = c("w.percent", "w.SE", "count", "n", "N", 'EB'))

item212C.table <- data.frame("Housing.Vintage" = item212C.cast$HomeYearBuilt_bins_MF
                             ,"Low-Rise.(1-3)Percent" = item212C.cast$`w.percent_Apartment Building (3 or fewer floors)`
                             ,"Low-Rise.SE"    = item212C.cast$`w.SE_Apartment Building (3 or fewer floors)`
                             ,"Mid-Rise.(4-6)Percent" = item212C.cast$`w.percent_Apartment Building (4 to 6 floors)`
                             ,"Mid-Rise.SE"    = item212C.cast$`w.SE_Apartment Building (4 to 6 floors)`
                             ,"High-Rise.(7+)Percent" = NA#item212C.cast$`w.percent_Apartment Building (More than 6 floors)`
                             ,"High-Rise.SE"   = NA#item212C.cast$`w.SE_Apartment Building (More than 6 floors)`
                             ,"All.Sizes.Percent"      = item212C.cast$`w.percent_Remove`
                             ,"All.Sizes.SE"   = item212C.cast$`w.SE_Remove`
                             ,"n"              = item212C.cast$`n_Remove`
                             ,"Low-Rise.EB"    = item212C.cast$`EB_Apartment Building (3 or fewer floors)`
                             ,"Mid-Rise.EB"    = item212C.cast$`EB_Apartment Building (4 to 6 floors)`
                             ,"High-Rise.EB"   = NA#item212C.cast$`EB_Apartment Building (More than 6 floors)`
                             ,"All.Sizes.EB"   = item212C.cast$`EB_Remove`
)

# row ordering example code
levels(item212C.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item212C.table <- item212C.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item212C.table <- data.frame(item212C.table)

exportTable(item212C.table, "MF", "Table 4C", weighted = TRUE, OS = T, osIndicator = "PSE")

#######################
# Unweighted Analysis
#######################
item212C.summary <- proportions_two_groups_unweighted(CustomerLevelData = item212C.data
                                                      ,valueVariable = 'count'
                                                      ,columnVariable = 'HomeYearBuilt_bins_MF'
                                                      ,rowVariable = 'HomeType'
                                                      ,aggregateColumnName = "All Vintages")
item212C.summary <- item212C.summary[which(item212C.summary$HomeYearBuilt_bins_MF != "All Vintages"),]
item212C.summary <- item212C.summary[which(item212C.summary$HomeType != "Total"),]

item212C.data$HousingType <- item212C.data$HomeType
item212C.all.vintages <- proportions_one_group(CustomerLevelData = item212C.data
                                               ,valueVariable = 'count'
                                               ,groupingVariable = 'HousingType'
                                               ,total.name = "All Vintages"
                                               ,columnName = 'HomeYearBuilt_bins_MF'
                                               ,weighted = FALSE
                                               ,two.prop.total = TRUE)
names(item212C.all.vintages)[which(names(item212C.all.vintages) == "HousingType")] <- "HomeType"
item212C.all.vintages <- item212C.all.vintages[which(item212C.all.vintages$HomeType != "Total"),]

item212C.all.sizes <- proportions_one_group(CustomerLevelData = item212C.data
                                            ,valueVariable = 'count'
                                            ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                            ,total.name = 'Remove'
                                            ,columnName = "HomeType"
                                            ,weighted = FALSE
                                            ,two.prop.total = TRUE)
item212C.all.sizes$HomeYearBuilt_bins_MF[which(item212C.all.sizes$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"


item212C.final <- rbind.data.frame(item212C.summary, item212C.all.vintages, item212C.all.sizes, stringsAsFactors = F)

item212C.cast <- dcast(setDT(item212C.final)
                       ,formula = HomeYearBuilt_bins_MF ~ HomeType
                       ,value.var = c("Percent", "SE", "Count", "n"))

item212C.table <- data.frame("Housing.Vintage" = item212C.cast$HomeYearBuilt_bins_MF
                             ,"Low-Rise.(1-3)Percent" = item212C.cast$`Percent_Apartment Building (3 or fewer floors)`
                             ,"Low-Rise.SE"    = item212C.cast$`SE_Apartment Building (3 or fewer floors)`
                             ,"Mid-Rise.(4-6)Percent" = item212C.cast$`Percent_Apartment Building (4 to 6 floors)`
                             ,"Mid-Rise.SE"    = item212C.cast$`SE_Apartment Building (4 to 6 floors)`
                             ,"High-Rise.(7+)Percent" = NA#item212C.cast$`Percent_Apartment Building (More than 6 floors)`
                             ,"High-Rise.SE"   = NA#item212C.cast$`SE_Apartment Building (More than 6 floors)`
                             ,"All.Sizes.Percent"      = item212C.cast$`Percent_Remove`
                             ,"All.Sizes.SE"   = item212C.cast$`SE_Remove`
                             ,"n"              = item212C.cast$`n_Remove`)
# row ordering example code
levels(item212C.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item212C.table <- item212C.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item212C.table <- data.frame(item212C.table)

exportTable(item212C.table, "MF", "Table 4C", weighted = FALSE, OS = T, osIndicator = "PSE")


#############################################################################################
# Item 212: DISTRIBUTION OF BUILDINGS BY BUILDING SIZE AND VINTAGE (MF table 4)
#############################################################################################
item212D.dat <- rbsa.dat.bldg[which(rbsa.dat.bldg$Category == "2017 RBSA PS"),]
length(unique(item212D.dat$CK_Building_ID[which(!is.na(item212D.dat$HomeYearBuilt))]))

#subset to only MF homes
item212D.dat1 <- item212D.dat[grep("Multifamily", item212D.dat$BuildingType),]

#remove missing vintage information
item212D.dat2 <- unique(item212D.dat1[which(item212D.dat1$HomeYearBuilt %notin% c("N/A",NA)),])
names(item212D.dat2)

################################################
# Adding pop and sample sizes for weights
################################################
item212D.data <- weightedData(item212D.dat2[which(names(item212D.dat2) %notin% c("Category"))])
item212D.data <- left_join(item212D.data, item212D.dat2[which(names(item212D.dat2) %in% c("CK_Cadmus_ID","CK_Building_ID","Category"))])
item212D.data$count <- 1
item212D.data$Count <- 1

#######################
# Weighted Analysis
#######################
item212D.summary <- proportionRowsAndColumns1(CustomerLevelData = item212D.data
                                              ,valueVariable = 'count'
                                              ,columnVariable = 'HomeYearBuilt_bins_MF'
                                              ,rowVariable = 'HomeType'
                                              ,aggregateColumnName = "All Vintages")
item212D.summary <- item212D.summary[which(item212D.summary$HomeYearBuilt_bins_MF != "All Vintages"),]
item212D.summary <- item212D.summary[which(item212D.summary$HomeType != "Total"),]

item212D.data$HousingType <- item212D.data$HomeType
item212D.all.vintages <- proportions_one_group(CustomerLevelData = item212D.data
                                               ,valueVariable = 'count'
                                               ,groupingVariable = 'HousingType'
                                               ,total.name = "All Vintages"
                                               ,columnName = 'HomeYearBuilt_bins_MF'
                                               ,weighted = TRUE
                                               ,two.prop.total = TRUE)
names(item212D.all.vintages)[which(names(item212D.all.vintages) == "HousingType")] <- "HomeType"
item212D.all.vintages <- item212D.all.vintages[which(item212D.all.vintages$HomeType != "Total"),]


item212D.all.sizes <- proportions_one_group(CustomerLevelData = item212D.data
                                            ,valueVariable = 'count'
                                            ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                            ,total.name = 'Remove'
                                            ,columnName = "HomeType"
                                            ,weighted = TRUE
                                            ,two.prop.total = TRUE)
item212D.all.sizes$HomeYearBuilt_bins_MF[which(item212D.all.sizes$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"


item212D.final <- rbind.data.frame(item212D.summary, item212D.all.vintages, item212D.all.sizes, stringsAsFactors = F)

item212D.cast <- dcast(setDT(item212D.final)
                       ,formula = HomeYearBuilt_bins_MF ~ HomeType
                       ,value.var = c("w.percent", "w.SE", "count", "n", "N", 'EB'))
names(item212D.cast)
item212D.table <- data.frame("Housing.Vintage" = item212D.cast$HomeYearBuilt_bins_MF
                             ,"Low-Rise.(1-3)Percent" = item212D.cast$`w.percent_Apartment Building (3 or fewer floors)`
                             ,"Low-Rise.SE"    = item212D.cast$`w.SE_Apartment Building (3 or fewer floors)`
                             ,"Mid-Rise.(4-6)Percent" = item212D.cast$`w.percent_Apartment Building (4 to 6 floors)`
                             ,"Mid-Rise.SE"    = item212D.cast$`w.SE_Apartment Building (4 to 6 floors)`
                             ,"High-Rise.(7+)Percent" = item212D.cast$`w.percent_Apartment Building (More than 6 floors)`
                             ,"High-Rise.SE"   = item212D.cast$`w.SE_Apartment Building (More than 6 floors)`
                             ,"All.Sizes.Percent"      = item212D.cast$`w.percent_Remove`
                             ,"All.Sizes.SE"   = item212D.cast$`w.SE_Remove`
                             ,"n"              = item212D.cast$`n_Remove`
                             ,"Low-Rise.EB"    = item212D.cast$`EB_Apartment Building (3 or fewer floors)`
                             ,"Mid-Rise.EB"    = item212D.cast$`EB_Apartment Building (4 to 6 floors)`
                             ,"High-Rise.EB"   = item212D.cast$`EB_Apartment Building (More than 6 floors)`
                             ,"All.Sizes.EB"   = item212D.cast$`EB_Remove`
)

# row ordering example code
levels(item212D.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item212D.table <- item212D.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item212D.table <- data.frame(item212D.table)

exportTable(item212D.table, "MF", "Table 4D", weighted = TRUE, OS = T, osIndicator = "PSE")

#######################
# Unweighted Analysis
#######################
item212D.summary <- proportions_two_groups_unweighted(CustomerLevelData = item212D.data
                                                      ,valueVariable = 'count'
                                                      ,columnVariable = 'HomeYearBuilt_bins_MF'
                                                      ,rowVariable = 'HomeType'
                                                      ,aggregateColumnName = "All Vintages")
item212D.summary <- item212D.summary[which(item212D.summary$HomeYearBuilt_bins_MF != "All Vintages"),]
item212D.summary <- item212D.summary[which(item212D.summary$HomeType != "Total"),]

item212D.data$HousingType <- item212D.data$HomeType
item212D.all.vintages <- proportions_one_group(CustomerLevelData = item212D.data
                                               ,valueVariable = 'count'
                                               ,groupingVariable = 'HousingType'
                                               ,total.name = "All Vintages"
                                               ,columnName = 'HomeYearBuilt_bins_MF'
                                               ,weighted = FALSE
                                               ,two.prop.total = TRUE)
names(item212D.all.vintages)[which(names(item212D.all.vintages) == "HousingType")] <- "HomeType"
item212D.all.vintages <- item212D.all.vintages[which(item212D.all.vintages$HomeType != "Total"),]

item212D.all.sizes <- proportions_one_group(CustomerLevelData = item212D.data
                                            ,valueVariable = 'count'
                                            ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                            ,total.name = 'Remove'
                                            ,columnName = "HomeType"
                                            ,weighted = FALSE
                                            ,two.prop.total = TRUE)
item212D.all.sizes$HomeYearBuilt_bins_MF[which(item212D.all.sizes$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"


item212D.final <- rbind.data.frame(item212D.summary, item212D.all.vintages, item212D.all.sizes, stringsAsFactors = F)

item212D.cast <- dcast(setDT(item212D.final)
                       ,formula = HomeYearBuilt_bins_MF ~ HomeType
                       ,value.var = c("Percent", "SE", "Count", "n"))

item212D.table <- data.frame("Housing.Vintage" = item212D.cast$HomeYearBuilt_bins_MF
                             ,"Low-Rise.(1-3)Percent" = item212D.cast$`Percent_Apartment Building (3 or fewer floors)`
                             ,"Low-Rise.SE"    = item212D.cast$`SE_Apartment Building (3 or fewer floors)`
                             ,"Mid-Rise.(4-6)Percent" = item212D.cast$`Percent_Apartment Building (4 to 6 floors)`
                             ,"Mid-Rise.SE"    = item212D.cast$`SE_Apartment Building (4 to 6 floors)`
                             ,"High-Rise.(7+)Percent" = item212D.cast$`Percent_Apartment Building (More than 6 floors)`
                             ,"High-Rise.SE"   = item212D.cast$`SE_Apartment Building (More than 6 floors)`
                             ,"All.Sizes.Percent"      = item212D.cast$`Percent_Remove`
                             ,"All.Sizes.SE"   = item212D.cast$`SE_Remove`
                             ,"n"              = item212D.cast$`n_Remove`)
# row ordering example code
levels(item212D.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item212D.table <- item212D.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item212D.table <- data.frame(item212D.table)

exportTable(item212D.table, "MF", "Table 4D", weighted = FALSE, OS = T, osIndicator = "PSE")















#############################################################################################
# Item 216: DISTRIBUTION OF BUILDING FLOOR AREA BY FLOOR AREA CATEGORY AND BUILDING SIZE (MF table 8)
#############################################################################################
item216.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_TotalResidentialFloorArea"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_TotEnclosedBldgArea_IncludResidentialAndCommercialButExcludPkgGarages"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"))]
colnames(item216.dat) <- c("Common.Area"
                           ,"Number.of.Floors"
                           ,"Total.Residential.Floor.Area"
                           ,"Total.Residential.Commercial.Floor.Area"
                           ,"Commercial.Area"
                           ,"Nonres.Grocery.SQFT"
                           ,"Nonres.Office.SQFT"
                           ,"Nonres.Other.SQFT"
                           ,"Nonres.Retail.SQFT"
                           ,"Nonres.Vacant.SQFT"
                           ,"CK_Building_ID")

item216.dat1 <- item216.dat[which(item216.dat$Number.of.Floors %notin% c("N/A",NA)),]
item216.dat1[is.na(item216.dat1)] <- 0

for (i in 1:10){
  item216.dat1[,i] <- as.numeric(as.character(item216.dat1[,i]))
}

#calculate total nonresidential floor area
item216.dat1$Total.Nonres.Floor.Area <-
  item216.dat1$Nonres.Grocery.SQFT + 
  item216.dat1$Nonres.Office.SQFT +
  item216.dat1$Nonres.Other.SQFT +
  item216.dat1$Nonres.Retail.SQFT +
  item216.dat1$Nonres.Vacant.SQFT

#calcualte combined total floor area across common area, res, and nonres
item216.dat1$Total.Floor.Area <- item216.dat1$Total.Nonres.Floor.Area +
  item216.dat1$Total.Residential.Floor.Area +
  item216.dat1$Common.Area


item216.merge <- left_join(rbsa.dat.bldg, item216.dat1)

item216.dat2 <- item216.merge[grep("Multifamily", item216.merge$BuildingType),]
item216.dat3 <- item216.dat2[which(item216.dat2$Total.Floor.Area > 0),]
item216.dat4 <- item216.dat3[which(!is.na(item216.dat3$Total.Floor.Area)),]
item216.dat4 <- item216.dat4[which(item216.dat4$Category == "PSE"),]
################################################
# Adding pop and sample sizes for weights
################################################
item216.data <- weightedData(item216.dat4[which(colnames(item216.dat4) %notin% c("Common.Area"
                                                                                 ,"Number.of.Floors"
                                                                                 ,"Total.Residential.Floor.Area"
                                                                                 ,"Total.Residential.Commercial.Floor.Area"
                                                                                 ,"Commercial.Area"
                                                                                 ,"Nonres.Grocery.SQFT"
                                                                                 ,"Nonres.Office.SQFT"
                                                                                 ,"Nonres.Other.SQFT"
                                                                                 ,"Nonres.Retail.SQFT"
                                                                                 ,"Nonres.Vacant.SQFT"
                                                                                 ,"Total.Nonres.Floor.Area"
                                                                                 ,"Total.Floor.Area"
                                                                                 ,"Category"))])

item216.data <- left_join(item216.data, unique(item216.dat4[which(colnames(item216.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_Building_ID"
                                                                                         ,"Common.Area"
                                                                                         ,"Number.of.Floors"
                                                                                         ,"Total.Residential.Floor.Area"
                                                                                         ,"Total.Residential.Commercial.Floor.Area"
                                                                                         ,"Commercial.Area"
                                                                                         ,"Nonres.Grocery.SQFT"
                                                                                         ,"Nonres.Office.SQFT"
                                                                                         ,"Nonres.Other.SQFT"
                                                                                         ,"Nonres.Retail.SQFT"
                                                                                         ,"Nonres.Vacant.SQFT"
                                                                                         ,"Total.Nonres.Floor.Area"
                                                                                         ,"Total.Floor.Area"
                                                                                         ,"Category"))]))


item216.sub <- item216.data[which(colnames(item216.data) %in% c("CK_Cadmus_ID"
                                                                ,"CK_Building_ID"
                                                                ,"HomeType"
                                                                ,"Common.Area"
                                                                ,"Total.Nonres.Floor.Area"
                                                                ,"Total.Residential.Floor.Area"
                                                                ,"Total.Floor.Area"))]
item216.melt <- melt(data = item216.sub, id.vars = c("CK_Cadmus_ID","CK_Building_ID", "HomeType", "Total.Floor.Area"))
colnames(item216.melt) <- c("CK_Cadmus_ID"
                            ,"CK_Building_ID"
                            ,"HomeType"
                            ,"Total.Floor.Area"
                            ,"Area.Type"
                            ,"Floor.Area")

item216.merge <- left_join(item216.data, item216.melt)
item216.merge$Count <- 1
#######################
# Weighted Analysis
#######################
item216.summary <- proportionRowsAndColumns1(CustomerLevelData = item216.merge
                                             ,valueVariable = 'Floor.Area'
                                             ,columnVariable = "HomeType"
                                             ,rowVariable = "Area.Type"
                                             ,aggregateColumnName = "All.Sizes")
item216.summary <- item216.summary[which(item216.summary$HomeType != "All.Sizes"),]
item216.summary <- item216.summary[which(item216.summary$Area.Type != "Total"),]

item216.all.sizes <- proportions_one_group(CustomerLevelData = item216.merge
                                           ,valueVariable = 'Floor.Area'
                                           ,groupingVariable = 'Area.Type'
                                           ,total.name = 'All Sizes'
                                           ,columnName = "HomeType"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)
item216.all.sizes <- item216.all.sizes[which(item216.all.sizes$Area.Type != "Total"),]

item216.sample.sizes <- proportions_one_group(CustomerLevelData = item216.merge
                                              ,valueVariable = 'Floor.Area'
                                              ,groupingVariable = 'HomeType'
                                              ,total.name = 'Total'
                                              ,columnName = "Area.Type"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
item216.sample.sizes$HomeType[which(item216.sample.sizes$HomeType == "Total")] <- "All Sizes"


item216.final <- rbind.data.frame(item216.summary, item216.all.sizes, stringsAsFactors = F)


item216.cast <- dcast(setDT(item216.final)
                      ,formula = BuildingType + HomeType ~ Area.Type
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item216.table <- data.frame("BuildingType"                  = item216.cast$BuildingType
                            ,"HomeType"                     = item216.cast$HomeType
                            ,"Percent_Common.Area"          = item216.cast$w.percent_Common.Area
                            ,"SE_Common.Area"               = item216.cast$w.SE_Common.Area
                            ,"Percent_Non-Residential.Area" = item216.cast$w.percent_Total.Nonres.Floor.Area
                            ,"SE_Non-Residential.Area"      = item216.cast$w.SE_Total.Nonres.Floor.Area
                            ,"Percent_Residential.Area."     = item216.cast$w.percent_Total.Residential.Floor.Area
                            ,"SE_Residential.Area."          = item216.cast$w.SE_Total.Residential.Floor.Area
                            ,"n"                            = item216.cast$n_Total.Residential.Floor.Area
                            ,"EB_Common.Area"               = item216.cast$EB_Common.Area
                            ,"EB_Non-Residential.Area"      = item216.cast$EB_Total.Nonres.Floor.Area
                            ,"EB_Residential.Area"          = item216.cast$EB_Total.Residential.Floor.Area
)

# row ordering example code
levels(item216.table$HomeType)
rowOrder <- c("Apartment Building (3 or fewer floors)"
              ,"Apartment Building (4 to 6 floors)"
              ,"Apartment Building (More than 6 floors)"
              ,"All Sizes")
item216.table <- item216.table %>% mutate(HomeType = factor(HomeType, levels = rowOrder)) %>% arrange(HomeType)  
item216.table <- data.frame(item216.table)

item216.table.MF <- item216.table[which(item216.table$BuildingType == "Multifamily"),
                                  which(names(item216.table) != "BuildingType")]

exportTable(item216.table.MF, "MF", "Table 8", weighted = TRUE, OS = T, osIndicator = "PSE")
# exportTable(item216.table.MF, "MF", "Table 13", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item216.summary <- proportions_two_groups_unweighted(CustomerLevelData = item216.merge
                                                     ,valueVariable = 'Floor.Area'
                                                     ,columnVariable = "HomeType"
                                                     ,rowVariable = "Area.Type"
                                                     ,aggregateColumnName = "All.Sizes")
item216.summary <- item216.summary[which(item216.summary$HomeType != "All.Sizes"),]
item216.summary <- item216.summary[which(item216.summary$Area.Type != "Total"),]

item216.all.sizes <- proportions_one_group(CustomerLevelData = item216.merge
                                           ,valueVariable = 'Floor.Area'
                                           ,groupingVariable = 'Area.Type'
                                           ,total.name = 'All Sizes'
                                           ,columnName = "HomeType"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)
item216.all.sizes <- item216.all.sizes[which(item216.all.sizes$Area.Type != "Total"),]


# item216.sample.sizes <- proportions_one_group(CustomerLevelData = item216.merge
#                                               ,valueVariable = 'Floor.Area'
#                                               ,groupingVariable = 'HomeType'
#                                               ,total.name = 'Total'
#                                               ,columnName = "Area.Type"
#                                               ,weighted = FALSE
#                                               ,two.prop.total = TRUE)
# item216.sample.sizes$HomeType[which(item216.sample.sizes$HomeType == "Total")] <- "All Sizes"


item216.final <- rbind.data.frame(item216.summary, item216.all.sizes, stringsAsFactors = F)


item216.cast <- dcast(setDT(item216.final)
                      ,formula = BuildingType + HomeType ~ Area.Type
                      ,value.var = c("Percent", "SE", "Count", "n"))

item216.table <- data.frame("BuildingType" = item216.cast$BuildingType
                            ,"HomeType"    = item216.cast$HomeType
                            ,"Percent_Common.Area" = item216.cast$Percent_Common.Area
                            ,"SE_Common.Area"      = item216.cast$SE_Common.Area
                            # ,"n_Common.Area"       = item216.cast$n_Common.Area
                            ,"Percent_Non-Residential.Area" = item216.cast$Percent_Total.Nonres.Floor.Area
                            ,"SE_Non-Residential.Area"      = item216.cast$SE_Total.Nonres.Floor.Area
                            # ,"n_Non-Residential.Area"       = item216.cast$n_Total.Nonres.Floor.Area
                            ,"Percent_Residential.Area"     = item216.cast$Percent_Total.Residential.Floor.Area
                            ,"SE_Residential.Area"          = item216.cast$SE_Total.Residential.Floor.Area
                            # ,"n_Residential.Area"           = item216.cast$n_Total.Residential.Floor.Area
                            ,"n" = item216.cast$n_Total.Residential.Floor.Area
)

# row ordering example code
levels(item216.table$HomeType)
rowOrder <- c("Apartment Building (3 or fewer floors)"
              ,"Apartment Building (4 to 6 floors)"
              ,"Apartment Building (More than 6 floors)"
              ,"All Sizes")
item216.table <- item216.table %>% mutate(HomeType = factor(HomeType, levels = rowOrder)) %>% arrange(HomeType)  
item216.table <- data.frame(item216.table)


item216.table.MF <- item216.table[which(item216.table$BuildingType == "Multifamily"),
                                  which(names(item216.table) != "BuildingType")]

exportTable(item216.table.MF, "MF", "Table 8", weighted = FALSE, OS = T, osIndicator = "PSE")
# exportTable(item216.table.MF, "MF", "Table 13", weighted = FALSE)

#############################################################################################
# Item 216: DISTRIBUTION OF BUILDING FLOOR AREA BY FLOOR AREA CATEGORY AND BUILDING SIZE (MF table 8)
#############################################################################################
item216.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_ResidentialInteriorCommonFloorArea"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_TotalResidentialFloorArea"
                                                                  ,"SITES_MFB_cfg_MFB_CONFIG_TotEnclosedBldgArea_IncludResidentialAndCommercialButExcludPkgGarages"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"))]
colnames(item216.dat) <- c("Common.Area"
                           ,"Number.of.Floors"
                           ,"Total.Residential.Floor.Area"
                           ,"Total.Residential.Commercial.Floor.Area"
                           ,"Commercial.Area"
                           ,"Nonres.Grocery.SQFT"
                           ,"Nonres.Office.SQFT"
                           ,"Nonres.Other.SQFT"
                           ,"Nonres.Retail.SQFT"
                           ,"Nonres.Vacant.SQFT"
                           ,"CK_Building_ID")

item216.dat1 <- item216.dat[which(item216.dat$Number.of.Floors %notin% c("N/A",NA)),]
item216.dat1[is.na(item216.dat1)] <- 0

for (i in 1:10){
  item216.dat1[,i] <- as.numeric(as.character(item216.dat1[,i]))
}

#calculate total nonresidential floor area
item216.dat1$Total.Nonres.Floor.Area <-
  item216.dat1$Nonres.Grocery.SQFT + 
  item216.dat1$Nonres.Office.SQFT +
  item216.dat1$Nonres.Other.SQFT +
  item216.dat1$Nonres.Retail.SQFT +
  item216.dat1$Nonres.Vacant.SQFT

#calcualte combined total floor area across common area, res, and nonres
item216.dat1$Total.Floor.Area <- item216.dat1$Total.Nonres.Floor.Area +
  item216.dat1$Total.Residential.Floor.Area +
  item216.dat1$Common.Area


item216.merge <- left_join(rbsa.dat.bldg, item216.dat1)

item216.dat2 <- item216.merge[grep("Multifamily", item216.merge$BuildingType),]
item216.dat3 <- item216.dat2[which(item216.dat2$Total.Floor.Area > 0),]
item216.dat4 <- item216.dat3[which(!is.na(item216.dat3$Total.Floor.Area)),]

################################################
# Adding pop and sample sizes for weights
################################################
item216.data <- weightedData(item216.dat4[which(colnames(item216.dat4) %notin% c("Common.Area"
                                                                                 ,"Number.of.Floors"
                                                                                 ,"Total.Residential.Floor.Area"
                                                                                 ,"Total.Residential.Commercial.Floor.Area"
                                                                                 ,"Commercial.Area"
                                                                                 ,"Nonres.Grocery.SQFT"
                                                                                 ,"Nonres.Office.SQFT"
                                                                                 ,"Nonres.Other.SQFT"
                                                                                 ,"Nonres.Retail.SQFT"
                                                                                 ,"Nonres.Vacant.SQFT"
                                                                                 ,"Total.Nonres.Floor.Area"
                                                                                 ,"Total.Floor.Area"
                                                                                 ,"Category"))])

item216.data <- left_join(item216.data, item216.dat4[which(colnames(item216.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_Building_ID"
                                                                                         ,"Common.Area"
                                                                                         ,"Number.of.Floors"
                                                                                         ,"Total.Residential.Floor.Area"
                                                                                         ,"Total.Residential.Commercial.Floor.Area"
                                                                                         ,"Commercial.Area"
                                                                                         ,"Nonres.Grocery.SQFT"
                                                                                         ,"Nonres.Office.SQFT"
                                                                                         ,"Nonres.Other.SQFT"
                                                                                         ,"Nonres.Retail.SQFT"
                                                                                         ,"Nonres.Vacant.SQFT"
                                                                                         ,"Total.Nonres.Floor.Area"
                                                                                         ,"Total.Floor.Area"
                                                                                         ,"Category"))])


item216.sub <- item216.data[which(colnames(item216.data) %in% c("CK_Cadmus_ID"
                                                                ,"CK_Building_ID"
                                                                ,"Category"
                                                                ,"Common.Area"
                                                                ,"Total.Nonres.Floor.Area"
                                                                ,"Total.Residential.Floor.Area"
                                                                ,"Total.Floor.Area"))]
item216.melt <- melt(data = item216.sub, id.vars = c("CK_Cadmus_ID","CK_Building_ID", "Category", "Total.Floor.Area"))
colnames(item216.melt) <- c("CK_Cadmus_ID"
                            ,"CK_Building_ID"
                            ,"Category"
                            ,"Total.Floor.Area"
                            ,"Area.Type"
                            ,"Floor.Area")

item216.merge <- left_join(item216.data, item216.melt)
item216.merge$Count <- 1
length(unique(item216.merge$CK_Cadmus_ID))
#######################
# Weighted Analysis
#######################
item216.summary <- proportionRowsAndColumns1(CustomerLevelData = item216.merge
                                             ,valueVariable = 'Floor.Area'
                                             ,columnVariable = "Category"
                                             ,rowVariable = "Area.Type"
                                             ,aggregateColumnName = "Remove")
item216.summary <- item216.summary[which(item216.summary$Category != "Remove"),]
item216.summary <- item216.summary[which(item216.summary$Area.Type != "Total"),]

item216.all.sizes <- proportions_one_group(CustomerLevelData = item216.merge
                                           ,valueVariable = 'Floor.Area'
                                           ,groupingVariable = 'Area.Type'
                                           ,total.name = 'Remove'
                                           ,columnName = "Category"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)
item216.all.sizes <- item216.all.sizes[which(item216.all.sizes$Area.Type != "Total"),]

item216.sample.sizes <- proportions_one_group(CustomerLevelData = item216.merge
                                           ,valueVariable = 'Floor.Area'
                                           ,groupingVariable = 'Category'
                                           ,total.name = 'Total'
                                           ,columnName = "Area.Type"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)
item216.sample.sizes$Category[which(item216.sample.sizes$Category == "Total")] <- "Remove"


item216.final <- rbind.data.frame(item216.summary, item216.all.sizes, item216.sample.sizes, stringsAsFactors = F)


item216.cast <- dcast(setDT(item216.final)
                      ,formula = BuildingType + Category ~ Area.Type
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item216.table <- data.frame("BuildingType"                  = item216.cast$BuildingType
                            ,"Category"                     = item216.cast$Category
                            ,"Percent_Common.Area"          = item216.cast$w.percent_Common.Area
                            ,"SE_Common.Area"               = item216.cast$w.SE_Common.Area
                            ,"Percent_Non-Residential.Area" = item216.cast$w.percent_Total.Nonres.Floor.Area
                            ,"SE_Non-Residential.Area."      = item216.cast$w.SE_Total.Nonres.Floor.Area
                            ,"Percent_Residential.Area."     = item216.cast$w.percent_Total.Residential.Floor.Area
                            ,"SE_Residential.Area"          = item216.cast$w.SE_Total.Residential.Floor.Area
                            ,"n"                            = item216.cast$n_Total
                            ,"EB_Common.Area"               = item216.cast$EB_Common.Area
                            ,"EB_Non-Residential.Area"      = item216.cast$EB_Total.Nonres.Floor.Area
                            ,"EB_Residential.Area"          = item216.cast$EB_Total.Residential.Floor.Area
                            )

# row ordering example code
levels(item216.table$Category)
rowOrder <- c("PSE"
              ,"PSE KING COUNTY"
              ,"PSE NON-KING COUNTY"
              ,"2017 RBSA PS"
              ,"Remove")
item216.table <- item216.table %>% mutate(Category = factor(Category, levels = rowOrder)) %>% arrange(Category)  
item216.table <- data.frame(item216.table[which(item216.table$Category != "Remove"),])

item216.table.MF <- item216.table[which(item216.table$BuildingType == "Multifamily"),
                                  which(names(item216.table) != "BuildingType")]

# exportTable(item216.table.MF, "MF", "Table 8", weighted = TRUE)
exportTable(item216.table.MF, "MF", "Table 13", weighted = TRUE, OS = T, osIndicator = "PSE")

#######################
# Unweighted Analysis
#######################
item216.summary <- proportions_two_groups_unweighted(CustomerLevelData = item216.merge
                                             ,valueVariable = 'Floor.Area'
                                             ,columnVariable = "Category"
                                             ,rowVariable = "Area.Type"
                                             ,aggregateColumnName = "All.Sizes")
item216.summary <- item216.summary[which(item216.summary$Category != "All.Sizes"),]
item216.summary <- item216.summary[which(item216.summary$Area.Type != "Total"),]

item216.all.sizes <- proportions_one_group(CustomerLevelData = item216.merge
                                           ,valueVariable = 'Floor.Area'
                                           ,groupingVariable = 'Area.Type'
                                           ,total.name = 'Remove'
                                           ,columnName = "Category"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)
item216.all.sizes <- item216.all.sizes[which(item216.all.sizes$Area.Type != "Total"),]


item216.sample.sizes <- proportions_one_group(CustomerLevelData = item216.merge
                                              ,valueVariable = 'Floor.Area'
                                              ,groupingVariable = 'Category'
                                              ,total.name = 'Total'
                                              ,columnName = "Area.Type"
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
item216.sample.sizes$Category[which(item216.sample.sizes$Category == "Total")] <- "Remove"


item216.final <- rbind.data.frame(item216.summary, item216.all.sizes, item216.sample.sizes, stringsAsFactors = F)


item216.cast <- dcast(setDT(item216.final)
                      ,formula = BuildingType + Category ~ Area.Type
                      ,value.var = c("Percent", "SE", "Count", "n"))

item216.table <- data.frame("BuildingType" = item216.cast$BuildingType
                            ,"Category"    = item216.cast$Category
                            ,"Percent_Common.Area" = item216.cast$Percent_Common.Area
                            ,"SE_Common.Area"      = item216.cast$SE_Common.Area
                            ,"Percent_Non-Residential.Area" = item216.cast$Percent_Total.Nonres.Floor.Area
                            ,"SE_Non-Residential.Area"      = item216.cast$SE_Total.Nonres.Floor.Area
                            ,"Percent_Residential.Area"     = item216.cast$Percent_Total.Residential.Floor.Area
                            ,"SE_Residential.Area"          = item216.cast$SE_Total.Residential.Floor.Area
                            ,"n" = item216.cast$n_Total
                            )

# row ordering example code
levels(item216.table$Category)
rowOrder <- c("PSE"
              ,"PSE KING COUNTY"
              ,"PSE NON-KING COUNTY"
              ,"2017 RBSA PS"
              ,"Remove")
item216.table <- item216.table %>% mutate(Category = factor(Category, levels = rowOrder)) %>% arrange(Category)  
item216.table <- data.frame(item216.table[which(item216.table$Category != "Remove"),])

item216.table.MF <- item216.table[which(item216.table$BuildingType == "Multifamily"),
                                  which(names(item216.table) != "BuildingType")]

# exportTable(item216.table.MF, "MF", "Table 8", weighted = FALSE)
exportTable(item216.table.MF, "MF", "Table 13", weighted = FALSE, OS = T, osIndicator = "PSE")
