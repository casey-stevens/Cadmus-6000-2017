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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) 
rbsa.dat.site <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat[grep("bldg",rbsa.dat$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
# buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))
length(unique(buildings.dat$CK_Building_ID))
buildings.dat.clean <- buildings.dat[which(!duplicated(buildings.dat$CK_Building_ID)),]


# buildings.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.interview.export))
buildings.interview.dat$CK_Building_ID <- trimws(toupper(buildings.interview.dat$CK_BuildingID))


# one.line.bldg.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), startRow = 2)
one.line.bldg.dat$CK_Building_ID <- trimws(toupper(one.line.bldg.dat$PK_BuildingID))


#############################################################################################
#Item 213: DISTRIBUTION OF UNITS BY BUILDING SIZE AND VINTAGE (MF Table 5)
#############################################################################################
#subset to columns needed for analysis
#from buildings interview data:
item213.interview.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                                    ,"Total.Units.in.Building"))]
names(item213.interview.dat) <- c("Number.of.Units", "CK_Building_ID")

# item213.building.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
#                                                                            ,"SITES_MFB_cfg_MFB_CONFIG_TotalNumberFloorsAboveGround"))]
# names(item213.building.dat) <- c("Total.Floors", "CK_Building_ID")


item213.dat <- item213.interview.dat#left_join(item213.building.dat, item213.interview.dat)
item213.dat1 <- item213.dat[which(item213.dat$Number.of.Units %notin% c("N/A",NA)),]

item213.merge <- left_join(rbsa.dat.bldg, item213.dat1)

item213.merge$Number.of.Units <- as.numeric(as.character(item213.merge$Number.of.Units))
item213.merge$HomeYearBuilt <- as.numeric(as.character(item213.merge$HomeYearBuilt))

item213.merge <- item213.merge[which(item213.merge$Number.of.Units %notin% c("N/A",NA)),]
item213.merge <- item213.merge[which(item213.merge$HomeYearBuilt %notin% c("N/A",NA)),]


################################################
# Adding pop and sample sizes for weights
################################################
item213.data <- weightedData(item213.merge[-which(colnames(item213.merge) %in% c("Total.Floors"
                                                                                 ,"Number.of.Units"))])
item213.data <- left_join(item213.data, item213.merge[which(colnames(item213.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Total.Floors"
                                                                                           ,"Number.of.Units"))])
item213.data$count <- 1
item213.data$Count <- 1
colnames(item213.data)
#######################
# Weighted Analysis
#######################
item213.summary <- proportionRowsAndColumns1(CustomerLevelData = item213.data
                                             ,valueVariable = 'Number.of.Units'
                                             ,columnVariable = 'HomeYearBuilt_bins_MF'
                                             ,rowVariable = 'HomeType'
                                             ,aggregateColumnName = "Remove")
item213.summary <- item213.summary[which(item213.summary$HomeYearBuilt_bins_MF != "Remove"),]
item213.summary <- item213.summary[which(item213.summary$HomeType != "Total"),]

item213.data$HousingType <- item213.data$HomeType
item213.all.vintages <- proportions_one_group(CustomerLevelData = item213.data
                                              ,valueVariable = 'Number.of.Units'
                                              ,groupingVariable = 'HousingType'
                                              ,total.name = "All Vintages"
                                              ,columnName = 'HomeYearBuilt_bins_MF'
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
names(item213.all.vintages)[which(names(item213.all.vintages) == "HousingType")] <- "HomeType"
item213.all.vintages$HomeType[which(item213.all.vintages$HomeType == "Total")] <- "All Sizes"

item213.all.sizes <- proportions_one_group(CustomerLevelData = item213.data
                                           ,valueVariable = 'Number.of.Units'
                                           ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                           ,total.name = "All Sizes"
                                           ,columnName = 'HomeType'
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)
item213.all.sizes <- item213.all.sizes[which(item213.all.sizes$HomeYearBuilt_bins_MF != "Total"),]

item213.final <- rbind.data.frame(item213.summary, item213.all.vintages, item213.all.sizes, stringsAsFactors = F)
item213.final$HomeYearBuilt_bins_MF[which(item213.final$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"

item213.cast <- dcast(setDT(item213.final)
                      ,formula = BuildingType + HomeYearBuilt_bins_MF~ HomeType
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
colnames(item213.cast)
item213.table <- data.frame("BuildingType"     = item213.cast$BuildingType
                            ,"Housing.Vintage" = item213.cast$HomeYearBuilt_bins_MF
                            ,"Low.Rise.1.3"    = item213.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.SE"     = item213.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6"    = item213.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.SE"     = item213.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.7.Plus"= item213.cast$`w.percent_Apartment Building (More than 6 floors)`
                            ,"High.Rise.SE"    = item213.cast$`w.SE_Apartment Building (More than 6 floors)`
                            ,"All.Sizes"       = item213.cast$`w.percent_All Sizes`
                            ,"All.Sizes.SE"    = item213.cast$`w.SE_All Sizes`
                            ,"n"               = item213.cast$`n_All Sizes`
                            ,"Low.Rise.EB"     = item213.cast$`EB_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.EB"     = item213.cast$`EB_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.EB"    = item213.cast$`EB_Apartment Building (More than 6 floors)`
)
# row ordering example code
levels(item213.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item213.table <- item213.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item213.table <- data.frame(item213.table)

item213.table.MF <- item213.table[which(item213.table$BuildingType == "Multifamily")
                                  ,which(colnames(item213.table) %notin% c("BuildingType"))]
exportTable(item213.table.MF, "MF", "Table 5", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item213.summary <- proportions_two_groups_unweighted(CustomerLevelData = item213.data
                                                     ,valueVariable = 'Number.of.Units'
                                                     ,columnVariable = 'HomeYearBuilt_bins_MF'
                                                     ,rowVariable = 'HomeType'
                                                     ,aggregateColumnName = "Remove")
item213.summary <- item213.summary[which(item213.summary$HomeYearBuilt_bins_MF != "Remove"),]
item213.summary <- item213.summary[which(item213.summary$HomeType != "Total"),]

item213.data$HousingType <- item213.data$HomeType
item213.all.vintages <- proportions_one_group(CustomerLevelData = item213.data
                                              ,valueVariable = 'Number.of.Units'
                                              ,groupingVariable = 'HousingType'
                                              ,total.name = "All Vintages"
                                              ,columnName = 'HomeYearBuilt_bins_MF'
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
names(item213.all.vintages)[which(names(item213.all.vintages) == "HousingType")] <- "HomeType"
item213.all.vintages$HomeType[which(item213.all.vintages$HomeType == "Total")] <- "All Sizes"

item213.all.sizes <- proportions_one_group(CustomerLevelData = item213.data
                                           ,valueVariable = 'Number.of.Units'
                                           ,groupingVariable = 'HomeYearBuilt_bins_MF'
                                           ,total.name = "All Sizes"
                                           ,columnName = 'HomeType'
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)
item213.all.sizes <- item213.all.sizes[which(item213.all.sizes$HomeYearBuilt_bins_MF != "Total"),]

item213.final <- rbind.data.frame(item213.summary, item213.all.vintages, item213.all.sizes, stringsAsFactors = F)
item213.final$HomeYearBuilt_bins_MF[which(item213.final$HomeYearBuilt_bins_MF == "Total")] <- "All Vintages"

item213.cast <- dcast(setDT(item213.final)
                      ,formula = BuildingType + HomeYearBuilt_bins_MF~ HomeType
                      ,value.var = c("Percent", "SE", "n"))
colnames(item213.cast)
item213.table <- data.frame("BuildingType" = item213.cast$BuildingType
                            ,"Housing.Vintage" = item213.cast$HomeYearBuilt_bins_MF
                            ,"Low.Rise.1.3"    = item213.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.SE"     = item213.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6"    = item213.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.SE"     = item213.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.7.Plus"= item213.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High.Rise.SE"    = item213.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"All.Sizes"       = item213.cast$`Percent_All Sizes`
                            ,"All.Sizes.SE"    = item213.cast$`SE_All Sizes`
                            ,"n"               = item213.cast$`n_All Sizes`
)
# row ordering example code
levels(item213.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item213.table <- item213.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item213.table <- data.frame(item213.table)

item213.table.MF <- item213.table[which(item213.table$BuildingType == "Multifamily")
                                  ,which(colnames(item213.table) %notin% c("BuildingType"))]
exportTable(item213.table.MF, "MF", "Table 5", weighted = FALSE)





#############################################################################################
#Item 217: DISTRIBUTION OF UNIT TYPES BY VINTAGE (MF Table 9)
#############################################################################################
#subset to columns needed for analysis
#from buildings interview data:
item217.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Qty.Studio.Units"
                                                                          ,"Qty.One-Bedroom.Units"
                                                                          ,"Qty.Two-Bedroom.Units"
                                                                          ,"Qty.Three-Bedroom.Units"
                                                                          ,"Qty.Four-Bedroom.Units"))]
names(item217.dat) <- c("Number.of.Studio.Units"
                        ,"Number.of.1.Bedroom.Units"
                        ,"Number.of.2.Bedroom.Units"
                        ,"Number.of.3.Bedroom.Units"
                        ,"Number.of.4.Plus.Bedroom.Units"
                        ,"CK_Building_ID")

item217.dat$Number.of.Studio.Units <- as.numeric(as.character(item217.dat$Number.of.Studio.Units))
item217.dat$Number.of.1.Bedroom.Units <- as.numeric(as.character(item217.dat$Number.of.1.Bedroom.Units))
item217.dat$Number.of.2.Bedroom.Units <- as.numeric(as.character(item217.dat$Number.of.2.Bedroom.Units))
item217.dat$Number.of.3.Bedroom.Units <- as.numeric(as.character(item217.dat$Number.of.3.Bedroom.Units))
item217.dat$Number.of.4.Plus.Bedroom.Units <- as.numeric(as.character(item217.dat$Number.of.4.Plus.Bedroom.Units))
item217.dat1 <- item217.dat[which(!is.na(item217.dat$Number.of.Studio.Units)),]
item217.melt <- melt(item217.dat1, id.vars = "CK_Building_ID")
names(item217.melt) <- c("CK_Building_ID", "Number.of.Units", "Count")

item217.merge <- left_join(rbsa.dat.bldg, item217.melt)
item217.merge <- item217.merge[which(!is.na(item217.merge$Number.of.Units)),]
item217.merge <- item217.merge[which(!is.na(item217.merge$Count)),]
item217.merge <- item217.merge[which(!is.na(item217.merge$HomeYearBuilt)),]


################################################
# Adding pop and sample sizes for weights
################################################
item217.data <- weightedData(item217.merge[-which(colnames(item217.merge) %in% c("Number.of.Units"
                                                                                 ,"Count"))])
item217.data <- left_join(item217.data, item217.merge[which(colnames(item217.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Number.of.Units"
                                                                                           ,"Count"))])
item217.data$count <- 1
colnames(item217.data)

#######################
# Weighted Analysis
#######################
item217.summary <- proportionRowsAndColumns1_within_row(CustomerLevelData = item217.data
                                                        ,valueVariable = 'Count'
                                                        ,columnVariable = "HomeYearBuilt_bins_MF"
                                                        ,rowVariable = "Number.of.Units"
                                                        ,aggregateColumnName = "Remove")
item217.summary <- item217.summary[which(item217.summary$HomeYearBuilt_bins_MF != "Remove"),]

item217.all.vintages <- proportions_one_group_within_row(CustomerLevelData = item217.data
                                                         ,valueVariable = 'Count'
                                                         ,groupingVariable = 'Number.of.Units'
                                                         ,total.name = "All Vintages"
                                                         ,columnName = "HomeYearBuilt_bins_MF"
                                                         ,weighted = TRUE
                                                         ,two.prop.total = TRUE)
item217.all.vintages <- item217.all.vintages[which(item217.all.vintages$Number.of.Units != "Total"),]

item217.final <- rbind.data.frame(item217.summary, item217.all.vintages, stringsAsFactors = F)

item217.cast <- dcast(setDT(item217.final)
                      ,formula = BuildingType + HomeYearBuilt_bins_MF + n + N ~ Number.of.Units
                      ,value.var = c("w.percent","w.SE","count","EB"))
item217.table <- data.frame("BuildingType"                 = item217.cast$BuildingType
                            ,"Housing.Vintage"             = item217.cast$HomeYearBuilt_bins_MF
                            ,"Percent.Studio"              = item217.cast$w.percent_Number.of.Studio.Units
                            ,"SE.Studio"                   = item217.cast$w.SE_Number.of.Studio.Units
                            ,"Percent.One.Bedroom"         = item217.cast$w.percent_Number.of.1.Bedroom.Units
                            ,"SE.One.Bedroom"              = item217.cast$w.SE_Number.of.1.Bedroom.Units
                            ,"Percent.Two.Bedroom"         = item217.cast$w.percent_Number.of.2.Bedroom.Units
                            ,"SE.Two.Bedroom"              = item217.cast$w.SE_Number.of.2.Bedroom.Units
                            ,"Percent.Three.Bedroom"       = item217.cast$w.percent_Number.of.3.Bedroom.Units
                            ,"SE.Three.Bedroom"            = item217.cast$w.SE_Number.of.3.Bedroom.Units
                            ,"Percent.Four.Plus.Bedrooms"  = item217.cast$w.percent_Number.of.4.Plus.Bedroom.Units
                            ,"SE.Four.Plus.Bedrooms"       = item217.cast$w.SE_Number.of.4.Plus.Bedroom.Units
                            ,"n"                           = item217.cast$n
                            ,"EB.Studio"                   = item217.cast$EB_Number.of.Studio.Units
                            ,"EB.One.Bedroom"              = item217.cast$EB_Number.of.1.Bedroom.Units
                            ,"EB.Two.Bedroom"              = item217.cast$EB_Number.of.2.Bedroom.Units
                            ,"EB.Three.Bedroom"            = item217.cast$EB_Number.of.3.Bedroom.Units
                            ,"EB.Four.Plus.Bedrooms"       = item217.cast$EB_Number.of.4.Plus.Bedroom.Units)
# row ordering example code
levels(item217.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item217.table <- item217.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item217.table <- data.frame(item217.table)

item217.table.MF <- item217.table[which(item217.table$BuildingType == "Multifamily")
                                  ,which(colnames(item217.table) %notin% c("BuildingType"))]
exportTable(item217.table.MF, "MF", "Table 9", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item217.summary <- proportions_two_groups_unweighted(CustomerLevelData = item217.data
                                                     ,valueVariable = 'Count'
                                                     ,columnVariable = "HomeYearBuilt_bins_MF"
                                                     ,rowVariable = "Number.of.Units"
                                                     ,aggregateColumnName = "Remove")
item217.summary <- item217.summary[which(item217.summary$HomeYearBuilt_bins_MF != "Remove"),]

item217.all.vintages <- proportions_one_group(CustomerLevelData = item217.data
                                              ,valueVariable = 'Count'
                                              ,groupingVariable = 'Number.of.Units'
                                              ,total.name = "All Vintages"
                                              ,columnName = "HomeYearBuilt_bins_MF"
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
item217.all.vintages <- item217.all.vintages[which(item217.all.vintages$Number.of.Units != "Total"),]

item217.final <- rbind.data.frame(item217.summary, item217.all.vintages, stringsAsFactors = F)

item217.cast <- dcast(setDT(item217.final)
                      ,formula = BuildingType + HomeYearBuilt_bins_MF ~ Number.of.Units
                      ,value.var = c("Percent","SE","Count", "n"))

item217.table <- data.frame("BuildingType" = item217.cast$BuildingType
                            ,"Housing.Vintage" = item217.cast$HomeYearBuilt_bins_MF
                            ,"Percent.Studio"  = item217.cast$Percent_Number.of.Studio.Units
                            ,"SE.Studio"       = item217.cast$SE_Number.of.Studio.Units
                            ,"Percent.One.Bedroom" = item217.cast$Percent_Number.of.1.Bedroom.Units
                            ,"SE.One.Bedroom"      = item217.cast$SE_Number.of.1.Bedroom.Units
                            ,"Percent.Two.Bedroom" = item217.cast$Percent_Number.of.2.Bedroom.Units
                            ,"SE.Two.Bedroom"      = item217.cast$SE_Number.of.2.Bedroom.Units
                            ,"Percent.Three.Bedroom" = item217.cast$Percent_Number.of.3.Bedroom.Units
                            ,"SE.Three.Bedroom"      = item217.cast$SE_Number.of.3.Bedroom.Units
                            ,"Percent.Four.Plus.Bedrooms"  = item217.cast$Percent_Number.of.4.Plus.Bedroom.Units
                            ,"SE.Four.Plus.Bedrooms"       = item217.cast$SE_Number.of.4.Plus.Bedroom.Units
                            ,"n"                           = item217.cast$n_Total
)
# row ordering example code
levels(item217.table$Housing.Vintage)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item217.table <- item217.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item217.table <- data.frame(item217.table)

item217.table.MF <- item217.table[which(item217.table$BuildingType == "Multifamily")
                                  ,which(colnames(item217.table) %notin% c("BuildingType"))]
exportTable(item217.table.MF, "MF", "Table 9", weighted = FALSE)







#############################################################################################
#Item 226: DISTRIBUTION OF UNITS BY TENANT TYPE AND INCOME RESTRICTION (MF Table 18)
#############################################################################################
#subset to columns needed for analysis
#from buildings interview data:
item226.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Total.Units.in.Building"
                                                                          ,"Limited.to.55.or.Older?"
                                                                          ,"Limited.to.Low.Income?"))]
names(item226.dat) <- c("Total.Units.in.Building"
                        ,"Senior.Tenants"
                        ,"Low.Income.Tenants"
                        ,"CK_Building_ID")
item226.dat$Total.Units.in.Building <- as.numeric(as.character(item226.dat$Total.Units.in.Building))
item226.dat <- item226.dat[which(item226.dat$Total.Units.in.Building %notin% c("N/A",NA)),]

item226.dat1 <- item226.dat[which(item226.dat$Low.Income.Tenants %in% c("Yes", "No")),]
item226.dat2 <- item226.dat1[which(item226.dat1$Senior.Tenants %in% c("Yes", "No")),]


item226.dat2$Income.Restriction <- "No Income Restrictions"
item226.dat2$Income.Restriction[which(item226.dat2$Low.Income.Tenants == "Yes")] <- "Low Income Only"

item226.dat2$Tenant.Type <- "No Demographic Restrictions"
item226.dat2$Tenant.Type[which(item226.dat2$Senior.Tenants == "Yes")] <- "Senior Housing"

item226.merge <- left_join(rbsa.dat.bldg, item226.dat2)
item226.merge <- item226.merge[which(!is.na(item226.merge$Tenant.Type)),]
which(duplicated(item226.merge$CK_Cadmus_ID))

################################################
# Adding pop and sample sizes for weights
################################################
item226.data <- weightedData(item226.merge[-which(colnames(item226.merge) %in% c("Senior.Tenants"
                                                                                 ,"Low.Income.Tenants"
                                                                                 ,"Income.Restriction"
                                                                                 ,"Tenant.Type"
                                                                                 ,"Total.Units.in.Building"))])
item226.data <- left_join(item226.data, item226.merge[which(colnames(item226.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Senior.Tenants"
                                                                                           ,"Low.Income.Tenants"
                                                                                           ,"Income.Restriction"
                                                                                           ,"Tenant.Type"
                                                                                           ,"Total.Units.in.Building"))])
item226.data$count <- 1
colnames(item226.data)

#######################
# Weighted Analysis
#######################

item226.summary <- proportionRowsAndColumns1(CustomerLevelData = item226.data
                                             ,valueVariable  = 'Total.Units.in.Building'
                                             ,columnVariable = "Income.Restriction"
                                             ,rowVariable = "Tenant.Type"
                                             ,aggregateColumnName = "Remove")
item226.summary <- item226.summary[which(item226.summary$Income.Restriction != "Remove"),]
item226.summary <- item226.summary[which(item226.summary$Tenant.Type != "Total"),]

item226.all.income.types <- proportions_one_group(CustomerLevelData = item226.data
                                                  ,valueVariable = "Total.Units.in.Building"
                                                  ,groupingVariable = "Tenant.Type"
                                                  ,total.name = "All Types"
                                                  ,columnName = "Income.Restriction"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE)
item226.all.income.types$Tenant.Type[which(item226.all.income.types$Tenant.Type == "Total")] <- "All Types"

item226.all.tenant.types <- proportions_one_group(CustomerLevelData = item226.data
                                                  ,valueVariable = "Total.Units.in.Building"
                                                  ,groupingVariable = "Income.Restriction"
                                                  ,total.name = "All Types"
                                                  ,columnName = "Tenant.Type"
                                                  ,weighted = TRUE
                                                  ,two.prop.total = TRUE)
item226.all.tenant.types <- item226.all.tenant.types[which(item226.all.tenant.types$Income.Restriction != "Total"),]


item226.final <- rbind.data.frame(item226.summary, item226.all.income.types, item226.all.tenant.types, stringsAsFactors = F)

item226.cast <- dcast(setDT(item226.final)
                      ,formula = BuildingType + Tenant.Type ~ Income.Restriction
                      ,value.var = c("w.percent","w.SE","count", "n","N","EB"))
names(item226.cast)
item226.table <- data.frame("BuildingType"                 = item226.cast$BuildingType
                            ,"Tenant.Type"                 = item226.cast$Tenant.Type
                            ,"Percent.Low.Income"      = item226.cast$`w.percent_Low Income Only`
                            ,"SE.Low.Income"           = item226.cast$`w.SE_Low Income Only`
                            ,"n.Low.Income"            = item226.cast$`n_Low Income Only`
                            ,"Percent.No.Income.Restrictions" = item226.cast$`w.percent_No Income Restrictions`
                            ,"SE.No.Income.Restrictions"      = item226.cast$`w.SE_No Income Restrictions`
                            ,"n.No.Income.Restrictions"       = item226.cast$`n_No Income Restrictions`
                            ,"Percent.All.Types"           = item226.cast$`w.percent_All Types`
                            ,"SE.All.Types"                = item226.cast$`w.SE_All Types`
                            ,"n.All.Types"                 = item226.cast$`n_All Types`
                            ,"EB.Low.Income"               = item226.cast$`EB_Low Income Only`
                            ,"EB.No.Income.Restrictions"   = item226.cast$`EB_No Income Restrictions`
                            ,"EB.All.Types"                = item226.cast$`EB_All Types`
                            
)
# row ordering example code
levels(item226.table$Tenant.Type)
rowOrder <- c("Senior Housing"
              ,"No Demographic Restrictions"
              ,"All Types")
item226.table <- item226.table %>% mutate(Tenant.Type = factor(Tenant.Type, levels = rowOrder)) %>% arrange(Tenant.Type)  
item226.table <- data.frame(item226.table)

item226.table.MF <- item226.table[which(item226.table$BuildingType == "Multifamily")
                                  ,which(colnames(item226.table) %notin% c("BuildingType"))]
exportTable(item226.table.MF, "MF", "Table 18", weighted = TRUE)


#######################
# Weighted Analysis
#######################
item226.summary <- proportions_two_groups_unweighted(CustomerLevelData = item226.data
                                                     ,valueVariable  = 'Total.Units.in.Building'
                                                     ,columnVariable = "Income.Restriction"
                                                     ,rowVariable = "Tenant.Type"
                                                     ,aggregateColumnName = "Remove")
item226.summary <- item226.summary[which(item226.summary$Income.Restriction != "Remove"),]
item226.summary <- item226.summary[which(item226.summary$Tenant.Type != "Total"),]

item226.all.income.types <- proportions_one_group(CustomerLevelData = item226.data
                                                  ,valueVariable = "Total.Units.in.Building"
                                                  ,groupingVariable = "Tenant.Type"
                                                  ,total.name = "All Types"
                                                  ,columnName = "Income.Restriction"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE)
item226.all.income.types$Tenant.Type[which(item226.all.income.types$Tenant.Type == "Total")] <- "All Types"

item226.all.tenant.types <- proportions_one_group(CustomerLevelData = item226.data
                                                  ,valueVariable = "Total.Units.in.Building"
                                                  ,groupingVariable = "Income.Restriction"
                                                  ,total.name = "All Types"
                                                  ,columnName = "Tenant.Type"
                                                  ,weighted = FALSE
                                                  ,two.prop.total = TRUE)
item226.all.tenant.types <- item226.all.tenant.types[which(item226.all.tenant.types$Income.Restriction != "Total"),]


item226.final <- rbind.data.frame(item226.summary, item226.all.income.types, item226.all.tenant.types, stringsAsFactors = F)

item226.cast <- dcast(setDT(item226.final)
                      ,formula = BuildingType + Tenant.Type ~ Income.Restriction
                      ,value.var = c("Percent","SE","Count", "n"))
names(item226.cast)
item226.table <- data.frame("BuildingType"                 = item226.cast$BuildingType
                            ,"Tenant.Type"                 = item226.cast$Tenant.Type
                            ,"Percent.Low.Income"      = item226.cast$`Percent_Low Income Only`
                            ,"SE.Low.Income"           = item226.cast$`SE_Low Income Only`
                            ,"n.Low.Income"            = item226.cast$`n_Low Income Only`
                            ,"Percent.No.Income.Restrictions" = item226.cast$`Percent_No Income Restrictions`
                            ,"SE.No.Income.Restrictions"      = item226.cast$`SE_No Income Restrictions`
                            ,"n.No.Income.Restrictions"       = item226.cast$`n_No Income Restrictions`
                            ,"Percent.All.Types"           = item226.cast$`Percent_All Types`
                            ,"SE.All.Types"                = item226.cast$`SE_All Types`
                            ,"n.All.Types"                 = item226.cast$`n_All Types`
                            
)
# row ordering example code
levels(item226.table$Tenant.Type)
rowOrder <- c("Senior Housing"
              ,"No Demographic Restrictions"
              ,"All Types")
item226.table <- item226.table %>% mutate(Tenant.Type = factor(Tenant.Type, levels = rowOrder)) %>% arrange(Tenant.Type)  
item226.table <- data.frame(item226.table)

item226.table.MF <- item226.table[which(item226.table$BuildingType == "Multifamily")
                                  ,which(colnames(item226.table) %notin% c("BuildingType"))]
exportTable(item226.table.MF, "MF", "Table 18", weighted = FALSE)
