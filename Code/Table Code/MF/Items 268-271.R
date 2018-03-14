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
source("Code/Table Code/Weighting Implementation - MF-BLDG.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
rbsa.dat.MF <- rbsa.dat[which(rbsa.dat$BuildingType == "Multifamily"),]
rbsa.dat.site <- rbsa.dat.MF[grep("site", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat.MF[grep("bldg", rbsa.dat.MF$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))

#Read in data for analysis
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))
grep("loads",names(sites.interview.dat), ignore.case = T)

one.line.bldg.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), sheet = "Building One Line Summary", startRow = 3)
one.line.bldg.dat <- one.line.bldg.dat[which(one.line.bldg.dat$Area.of.Conditioned.Common.Space > 0),]
one.line.bldg.dat$CK_Building_ID <- one.line.bldg.dat$PK_BuildingID

one.line.bldg.dat <- one.line.bldg.dat[names(one.line.bldg.dat) %in% c("CK_Building_ID", "Area.of.Conditioned.Common.Space")]

rbsa.merge <- left_join(rbsa.dat.bldg, one.line.bldg.dat)
rbsa.merge <- rbsa.merge[which(!is.na(rbsa.merge$Area.of.Conditioned.Common.Space)),]



#############################################################################################
#Item 268: DISTRIBUTION OF BUILDING LAUNDRY TYPE BY BUILDING VINTAGE (MF Table 60)
#############################################################################################

#############################################################################################
#subset to necessary columns, clean, and normalize data for table
#############################################################################################

#subset to columns needed for analysis
item268.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"Age"
                                                                    ,"Type"
                                                                    ,"Washer.Type"))]

item268.dat1 <- item268.dat[which(item268.dat$Type %in% c("Washer", "Dryer")),]

item268.dat1$Laundry.Location <- item268.dat1$CK_SiteID
item268.dat1$Laundry.Location[grep("BLDG", item268.dat1$CK_SiteID)] <- "Common"
item268.dat1$Laundry.Location[grep("SITE", item268.dat1$CK_SiteID)] <- "In.Unit"
unique(item268.dat1$Laundry.Location)

item268.dat2 <- item268.dat1[which(item268.dat1$Laundry.Location %notin% c("Delete", NA)),]

#join clean rbsa data onto appliances analysis data
item268.dat3 <- left_join(rbsa.dat, item268.dat2, by = c("CK_Building_ID" = "CK_SiteID"))
item268.dat3 <- item268.dat3[which(item268.dat3$BuildingType == "Multifamily"),]

item268.dat4 <- (item268.dat3[which(!is.na(item268.dat3$CK_Cadmus_ID.y)),])

item268.dat5 <- unique(data.frame("CK_Cadmus_ID" = item268.dat4$CK_Cadmus_ID.x
                                  ,"CK_Building_ID" = item268.dat4$CK_Building_ID
                                  ,"Laundry.Location" = item268.dat4$Laundry.Location
                                  , stringsAsFactors = F))

#############################################################################################
#Identify which sites have in-unit and common area washers/dryers
#############################################################################################
dup.ind <- unique(item268.dat5$CK_Cadmus_ID[which(duplicated(item268.dat5$CK_Cadmus_ID))])
item268.dat5$Laundry.Location[which(item268.dat5$CK_Cadmus_ID %in% dup.ind)] <- "In.Unit.and.Common"

item268.merge <- left_join(rbsa.dat, item268.dat5)

#subset to only MF
item268.merge <- item268.merge[grep("Multifamily", item268.merge$BuildingType),]
item268.merge <- item268.merge[grep("3 or fewer floors", item268.merge$BuildingTypeXX, ignore.case = T),]
item268.merge$Laundry.Location[which(is.na(item268.merge$Laundry.Location))] <- "None"
item268.merge$count <- 1

######################################
#Pop and Sample Sizes for weights
######################################
item268.data <- weightedData(item268.merge[which(colnames(item268.merge) %notin% c("Laundry.Location"
                                                                                     ,"count"))])

item268.data <- left_join(item268.data, item268.merge[which(colnames(item268.merge) %in% c("CK_Cadmus_ID"
                                                                                                  ,"CK_Building_ID"
                                                                                                  ,"Laundry.Location"
                                                                                                  ,"count"))])

names(item268.data)
length(unique(item268.data$CK_Cadmus_ID))
length(unique(item268.data$CK_Cadmus_ID[which(item268.data$HomeYearBuilt_bins_MF == "Pre 1955")]))
length(unique(item268.data$CK_Cadmus_ID[which(item268.data$HomeYearBuilt_bins_MF == "1955-1970")]))
length(unique(item268.data$CK_Cadmus_ID[which(item268.data$HomeYearBuilt_bins_MF == "1971-1980")]))
length(unique(item268.data$CK_Cadmus_ID[which(item268.data$HomeYearBuilt_bins_MF == "1981-1990")]))
length(unique(item268.data$CK_Cadmus_ID[which(item268.data$HomeYearBuilt_bins_MF == "1991-2000")]))
length(unique(item268.data$CK_Cadmus_ID[which(item268.data$HomeYearBuilt_bins_MF == "2001-2010")]))
length(unique(item268.data$CK_Cadmus_ID[which(item268.data$HomeYearBuilt_bins_MF == "Post 2010")]))

######################
# weighted analysis
######################
item268.summary <- proportionRowsAndColumns1(CustomerLevelData = item268.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = "HomeYearBuilt_bins_MF"
                                             ,rowVariable = 'Laundry.Location'
                                             ,aggregateColumnName = "Remove")
item268.summary <- item268.summary[which(item268.summary$HomeYearBuilt_bins_MF != "Remove"),]
# item268.summary <- item268.summary[which(item268.summary$Laundry.Location != "Total"),]

item268.all.vintages <- proportions_one_group(CustomerLevelData = item268.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = 'Laundry.Location'
                                                 ,total.name = 'All Vintages'
                                                 ,columnName = "HomeYearBuilt_bins_MF"
                                                 ,weighted = TRUE
                                                 ,two.prop.total = TRUE)
# item268.all.vintages <- item268.all.vintages[which(item268.all.vintages$Laundry.Location %notin% c("Remove","Total")),]

item268.final <- rbind.data.frame(item268.summary, item268.all.vintages, stringsAsFactors = F)

item268.cast <- dcast(setDT(item268.final)
                      ,formula = HomeYearBuilt_bins_MF ~ Laundry.Location
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
item268.cast[is.na(item268.cast)] <- 0

item268.table <- data.frame("Housing.Vintage"        = item268.cast$HomeYearBuilt_bins_MF
                            ,"Common.Only"           = item268.cast$w.percent_Common
                            ,"Common.Only.SE"        = item268.cast$w.SE_Common
                            ,"In.Unit.Only"          = item268.cast$w.percent_In.Unit
                            ,"In.Unit.Only.SE"       = item268.cast$w.SE_In.Unit
                            ,"In.Unit.and.Common"    = item268.cast$w.percent_In.Unit.and.Common
                            ,"In.Unit.and.Common.SE" = item268.cast$w.SE_In.Unit.and.Common
                            ,"None"                  = item268.cast$w.percent_None
                            ,"None.SE"               = item268.cast$w.SE_None
                            , "n"                    = item268.cast$n_Total
                            ,"Common.Only.EB"        = item268.cast$EB_Common
                            ,"In.Unit.Only.EB"       = item268.cast$EB_In.Unit
                            ,"In.Unit.and.Common.EB" = item268.cast$EB_In.Unit.and.Common
                            ,"None.EB"               = item268.cast$EB_None)

exportTable(item268.table, "MF", "Table 60", weighted = TRUE)

######################
# unweighted analysis
######################
item268.summary <- proportions_two_groups_unweighted(CustomerLevelData = item268.data
                                             ,valueVariable = 'count'
                                             ,columnVariable = "HomeYearBuilt_bins_MF"
                                             ,rowVariable = 'Laundry.Location'
                                             ,aggregateColumnName = "Remove")
item268.summary <- item268.summary[which(item268.summary$HomeYearBuilt_bins_MF != "Remove"),]

item268.all.vintages <- proportions_one_group(CustomerLevelData = item268.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = 'Laundry.Location'
                                                 ,total.name = 'All Vintages'
                                                 ,columnName = "HomeYearBuilt_bins_MF"
                                                 ,weighted = FALSE
                                                 ,two.prop.total = TRUE)
# item268.all.vintages <- item268.all.vintages[which(item268.all.vintages$Laundry.Location != "Total"),]

item268.final <- rbind.data.frame(item268.summary, item268.all.vintages, stringsAsFactors = F)

item268.cast <- dcast(setDT(item268.final)
                      ,formula = HomeYearBuilt_bins_MF ~ Laundry.Location
                      ,value.var = c("Percent", "SE", "Count", "n"))
item268.cast[is.na(item268.cast)] <- 0

item268.table <- data.frame("Housing.Vintage"        = item268.cast$HomeYearBuilt_bins_MF
                            ,"Common.Only"           = item268.cast$Percent_Common
                            ,"Common.Only.SE"        = item268.cast$SE_Common
                            ,"In.Unit.Only"          = item268.cast$Percent_In.Unit
                            ,"In.Unit.Only.SE"       = item268.cast$SE_In.Unit
                            ,"In.Unit.and.Common"    = item268.cast$Percent_In.Unit.and.Common
                            ,"In.Unit.and.Common.SE" = item268.cast$SE_In.Unit.and.Common
                            ,"None"                  = item268.cast$Percent_None
                            ,"None.SE"               = item268.cast$SE_None
                            ,"n"                     = item268.cast$n_Total)

exportTable(item268.table, "MF", "Table 60", weighted = FALSE)









#############################################################################################
#Item 269: DISTRIBUTION OF COMMON AREA CLOTHES WASHER TYPE BY WASHER VINTAGE (MF Table 61)
#############################################################################################
#subset to columns needed for analysis
item269.dat <- item268.dat4[grep("common",item268.dat4$Laundry.Location, ignore.case = T),]
names(item269.dat)[which(names(item269.dat) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"

#subset to only common area washers that have observed age info
unique(item269.dat$Age)
item269.dat$Age <- as.numeric(as.character(item269.dat$Age))
item269.dat2 <- item269.dat[which(item269.dat$Age %notin% c("N/A",NA)),]
item269.dat2 <- item269.dat2[which(item269.dat2$Washer.Type %notin% c("N/A",NA)),]

unique(item269.dat2$Washer.Type)
item269.dat3 <- item269.dat2[which(item269.dat2$Washer.Type %notin% c("Unknown", NA)),]

item269.dat3 <- item269.dat3[grep("3 or fewer floors", item269.dat3$BuildingTypeXX, ignore.case = T),]
#####################
# At this point, code will be needed to bin ages into categories according to previous table
#####################
item269.dat3$EquipVintage_bins <- as.numeric(as.character(item269.dat3$Age))

item269.dat3$EquipVintage_bins[which(item269.dat3$Age < 1980)] <- "Pre 1980"
item269.dat3$EquipVintage_bins[which(item269.dat3$Age >= 1980 & item269.dat3$Age < 1990)] <- "1980-1989"
item269.dat3$EquipVintage_bins[which(item269.dat3$Age >= 1990 & item269.dat3$Age < 1995)] <- "1990-1994"
item269.dat3$EquipVintage_bins[which(item269.dat3$Age >= 1995 & item269.dat3$Age < 2000)] <- "1995-1999"
item269.dat3$EquipVintage_bins[which(item269.dat3$Age >= 2000 & item269.dat3$Age < 2005)] <- "2000-2004"
item269.dat3$EquipVintage_bins[which(item269.dat3$Age >= 2005 & item269.dat3$Age < 2010)] <- "2005-2009"
item269.dat3$EquipVintage_bins[which(item269.dat3$Age >= 2010 & item269.dat3$Age < 2015)] <- "2010-2014"
item269.dat3$EquipVintage_bins[which(item269.dat3$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item269.dat3$EquipVintage_bins)

################################################
# Adding pop and sample sizes for weights
################################################
item269.data <- weightedData(item269.dat3[-which(colnames(item269.dat3) %in% c("CK_Cadmus_ID.y"
                                                                               ,"Type"
                                                                               ,"Age"
                                                                               ,"Washer.Type"
                                                                               ,"Laundry.Location"
                                                                               ,"EquipVintage_bins"))])
item269.data <- left_join(item269.data, item269.dat3[which(colnames(item269.dat3) %in% c("CK_Building_ID"
                                                                                       ,"Type"
                                                                                       ,"Age"
                                                                                       ,"Washer.Type"
                                                                                       ,"Laundry.Location"
                                                                                       ,"EquipVintage_bins"))])
item269.data$count <- 1
length(unique(item269.data$CK_Cadmus_ID))
#######################
# Weighted Analysis
#######################
item269.summary <- proportionRowsAndColumns1(CustomerLevelData = item269.data
                                                        ,valueVariable = 'count'
                                                        ,columnVariable = "EquipVintage_bins"
                                                        ,rowVariable = "Washer.Type"
                                                        ,aggregateColumnName = "Remove")
item269.summary <- item269.summary[which(item269.summary$EquipVintage_bins %notin% c("Total","Remove")),]
item269.summary <- item269.summary[which(item269.summary$Washer.Type %notin% c("Total","Remove")),]

item269.all.vintages <- proportions_one_group(CustomerLevelData = item269.data
                                                         ,valueVariable    = 'count'
                                                         ,groupingVariable = 'Washer.Type'
                                                         ,total.name = 'All Vintages'
                                                         ,columnName = "EquipVintage_bins"
                                                         ,weighted = TRUE
                                                         ,two.prop.total = TRUE)
item269.all.vintages <- item269.all.vintages[which(item269.all.vintages$Washer.Type %notin% c("Total","Remove")),]

item269.all.types <- proportions_one_group(CustomerLevelData = item269.data
                                                          ,valueVariable    = 'count'
                                                          ,groupingVariable = 'EquipVintage_bins'
                                                          ,total.name = 'All Types'
                                                          ,columnName = "Washer.Type"
                                                          ,weighted = TRUE
                                                          ,two.prop.total = TRUE)
item269.all.types$EquipVintage_bins[which(item269.all.types$EquipVintage_bins == "Total")] <- "All Vintages"

item269.final <- rbind.data.frame(item269.summary, item269.all.vintages, item269.all.types, stringsAsFactors = F)

item269.cast <- dcast(setDT(item269.final)
                      ,formula = BuildingType + Washer.Type ~ EquipVintage_bins
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))
item269.cast[is.na(item269.cast)] <- 0
names(item269.cast)
item269.table <- data.frame("Washer.Type" = item269.cast$Washer.Type
                            ,"Pre.1980"     = NA#item269.cast$`w.percent_Pre 1980`
                            ,"Pre.1980.SE"  = NA#item269.cast$`w.SE_Pre 1980`
                            ,"1980.1989"    = NA#item269.cast$`w.percent_1980-1989`
                            ,"1980.1989.SE" = NA#item269.cast$`w.SE_1980-1989`
                            ,"1990.1994"    = item269.cast$`w.percent_1990-1994`
                            ,"1990.1994.SE" = item269.cast$`w.SE_1990-1994`
                            ,"1995.1999"    = item269.cast$`w.percent_1995-1999`
                            ,"1995.1999.SE" = item269.cast$`w.SE_1995-1999`
                            ,"2000.2004"    = item269.cast$`w.percent_2000-2004`
                            ,"2000.2004.SE" = item269.cast$`w.SE_2000-2004`
                            ,"2005.2009"    = item269.cast$`w.percent_2005-2009`
                            ,"2005.2009.SE" = item269.cast$`w.SE_2005-2009`
                            ,"2010.2014"    = item269.cast$`w.percent_2010-2014`
                            ,"2010.2014.SE" = item269.cast$`w.SE_2010-2014`
                            ,"Post.2014"    = item269.cast$`w.percent_Post 2014`
                            ,"Post.2014.SE" = item269.cast$`w.SE_Post 2014`
                            ,"All.Vintages" = item269.cast$`w.percent_All Vintages`
                            ,"All.Vintages.SE" = item269.cast$`w.SE_All Vintages`
                            ,"n" = item269.cast$`n_All Vintages`
                            ,"Pre.1980.EB"  = NA#item269.cast$`EB_Pre 1980`
                            ,"1980.1989.EB" = NA#item269.cast$`EB_1980-1989`
                            ,"1990.1994.EB" = item269.cast$`EB_1990-1994`
                            ,"1995.1999.EB" = item269.cast$`EB_1995-1999`
                            ,"2000.2004.EB" = item269.cast$`EB_2000-2004`
                            ,"2005.2009.EB" = item269.cast$`EB_2005-2009`
                            ,"2010.2014.EB" = item269.cast$`EB_2010-2014`
                            ,"Post.2014.EB" = item269.cast$`EB_Post 2014`
                            ,"All.Vintages.EB" = item269.cast$`EB_All Vintages`
                            )

levels(item269.table$Washer.Type)
rowOrder <- c("Horizontal Axis"
              ,"Stacked Washer/Dryer"
              ,"Vertical Axis (with agitator)"
              ,"Vertical Axis (without agitator)"
              ,"Unknown"
              ,"All Types")
item269.table <- item269.table %>% mutate(Washer.Type = factor(Washer.Type, levels = rowOrder)) %>% arrange(Washer.Type)  
item269.table <- data.frame(item269.table)

exportTable(item269.table, "MF", "Table 61", weighted = TRUE)


#######################
# unweighted Analysis
#######################
item269.summary <- proportions_two_groups_unweighted(CustomerLevelData = item269.data
                                                        ,valueVariable = 'count'
                                                        ,columnVariable = "EquipVintage_bins"
                                                        ,rowVariable = "Washer.Type"
                                                        ,aggregateColumnName = "Remove")
item269.summary <- item269.summary[which(item269.summary$EquipVintage_bins %notin% c("Total","Remove")),]
item269.summary <- item269.summary[which(item269.summary$Washer.Type %notin% c("Total","Remove")),]

item269.all.vintages <- proportions_one_group(CustomerLevelData = item269.data
                                                         ,valueVariable    = 'count'
                                                         ,groupingVariable = 'Washer.Type'
                                                         ,total.name = 'All Vintages'
                                                         ,columnName = "EquipVintage_bins"
                                                         ,weighted = FALSE
                                                         ,two.prop.total = TRUE)
item269.all.vintages <- item269.all.vintages[which(item269.all.vintages$Washer.Type %notin% c("Total","Remove")),]

item269.all.types <- proportions_one_group(CustomerLevelData = item269.data
                                                      ,valueVariable    = 'count'
                                                      ,groupingVariable = 'EquipVintage_bins'
                                                      ,total.name = 'All Types'
                                                      ,columnName = "Washer.Type"
                                                      ,weighted = FALSE
                                                      ,two.prop.total = TRUE)
item269.all.types$EquipVintage_bins[which(item269.all.types$EquipVintage_bins == "Total")] <- "All Vintages"

item269.final <- rbind.data.frame(item269.summary, item269.all.vintages, item269.all.types, stringsAsFactors = F)

item269.cast <- dcast(setDT(item269.final)
                      ,formula = BuildingType + Washer.Type ~ EquipVintage_bins
                      ,value.var = c("Percent", "SE", "Count", "n"))
item269.cast[is.na(item269.cast)] <- 0

item269.table <- data.frame("Washer.Type" = item269.cast$Washer.Type
                            ,"Pre.1980"     = NA#item269.cast$
                            ,"Pre.1980.SE"  = NA#item269.cast$
                            ,"1980.1989"    = NA#item269.cast$`Percent_1980-1989`
                            ,"1980.1989.SE" = NA#item269.cast$`SE_1980-1989`
                            ,"1990.1994"    = item269.cast$`Percent_1990-1994`
                            ,"1990.1994.SE" = item269.cast$`SE_1990-1994`
                            ,"1995.1999"    = item269.cast$`Percent_1995-1999`
                            ,"1995.1999.SE" = item269.cast$`SE_1995-1999`
                            ,"2000.2004"    = item269.cast$`Percent_2000-2004`
                            ,"2000.2004.SE" = item269.cast$`SE_2000-2004`
                            ,"2005.2009"    = item269.cast$`Percent_2005-2009`
                            ,"2005.2009.SE" = item269.cast$`SE_2005-2009`
                            ,"2010.2014"    = item269.cast$`Percent_2010-2014`
                            ,"2010.2014.SE" = item269.cast$`SE_2010-2014`
                            ,"Post.2014"    = item269.cast$`Percent_Post 2014`
                            ,"Post.2014.SE" = item269.cast$`SE_Post 2014`
                            ,"All.Vintages" = item269.cast$`Percent_All Vintages`
                            ,"All.Vintages.SE" = item269.cast$`SE_All Vintages`
                            ,"n" = item269.cast$`n_All Vintages`)
item269.table$n[which(item269.table$Washer.Type == "All Types")] <- sum(item269.table$n[which(item269.table$Washer.Type != "All Types")])

levels(item269.table$Washer.Type)
rowOrder <- c("Combined Washer/Dryer in one drum"
              ,"Horizontal Axis"
              ,"Stacked Washer/Dryer"
              ,"Vertical Axis (with agitator)"
              ,"Vertical Axis (without agitator)"
              ,"Unknown"
              ,"All Types")
item269.table <- item269.table %>% mutate(Washer.Type = factor(Washer.Type, levels = rowOrder)) %>% arrange(Washer.Type)  
item269.table <- data.frame(item269.table)

exportTable(item269.table, "MF", "Table 61", weighted = FALSE)







#############################################################################################
#Item 270: AVERAGE NUMBER OF CLOTHES WASHER LOADS PER WEEK BY LAUNDRY TYPE (MF Table 62)
#############################################################################################
#subset to columns needed for analysis
item270.dat <- item268.merge

interview.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"CK_SiteID"
                                                                                    ,"INTRVW_CUST_RES_HomeandEnergyUseHome_ClothesWasherLoadsPerWeek"
                                                                                    ,""))])
colnames(interview.dat) <- c("CK_Cadmus_ID", "CK_Building_ID", "Clothes.Washes.Per.Week")
interview.dat1 <- 

item270.dat1 <- left_join(item270.dat, interview.dat, by = c("CK_Cadmus_ID", "CK_Building_ID"))

item270.dat2 <- unique(item270.dat1[which(colnames(item270.dat1) != "CK_Building_ID")])
item270.dat2$CK_Cadmus_ID[which(duplicated(item270.dat2$CK_Cadmus_ID))]

unique(item270.dat2$Clothes.Washes.Per.Week)
item270.dat3 <- item270.dat2[which(item270.dat2$Clothes.Washes.Per.Week %notin% c("No Washing Machine")),]
item270.dat3$Clothes.Washes.Per.Week <- as.numeric(as.character(item270.dat3$Clothes.Washes.Per.Week))

item270.sub <- unique(data.frame("CK_Cadmus_ID" = item270.dat1$CK_Cadmus_ID
                          ,"CK_Building_ID" = item270.dat1$CK_Building_ID))

item270.merge <- left_join(item270.sub, item270.dat3)
item270.merge <- item270.merge[which(!is.na(item270.merge$Clothes.Washes.Per.Week)),]
# item270.merge <- item270.merge[-grep("bldg",item270.merge$CK_Building_ID, ignore.case = T),]

# Weighting
item270.data <- weightedData(item270.merge[-which(colnames(item270.merge) %in% c("Laundry.Location"
                                                                                 ,"Clothes.Washes.Per.Week"
                                                                                 ,"count"
                                                                                 ,"CK_SiteID"))])

item270.data <- left_join(item270.data, unique(item270.merge[which(colnames(item270.merge) %in% c("CK_Cadmus_ID"
                                                                                                  ,"CK_Building_ID"
                                                                                           ,"Laundry.Location"
                                                                                           ,"Clothes.Washes.Per.Week"
                                                                                           ,"count"))]))
item270.data
#######################
# weighted Analysis
#######################
item270.final <- mean_one_group(CustomerLevelData = item270.data
                                ,valueVariable = 'Clothes.Washes.Per.Week'
                                ,byVariable = 'Laundry.Location'
                                ,aggregateRow = "All Types")

item270.final.MF <- item270.final[which(colnames(item270.final) %notin% c("BuildingType"))]
exportTable(item270.final.MF, "MF", "Table 62", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item270.final <- mean_one_group_unweighted(CustomerLevelData = item270.data
                                           ,valueVariable = 'Clothes.Washes.Per.Week'
                                           ,byVariable = 'Laundry.Location'
                                           ,aggregateRow = "All Types")

item270.final.MF <- item270.final[which(colnames(item270.final) %notin% c("BuildingType"))]
exportTable(item270.final.MF, "MF", "Table 62", weighted = FALSE)





#############################################################################################
#Item 271: DISTRIBUTION OF COMMON AREA CLOTHES DRYER BY DRYER VINTAGE (MF Table 63)
#############################################################################################
#subset to columns needed for analysis
item271.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_SiteID"
                                                                    ,"Age"
                                                                    ,"Type"))]
item271.dat1 <- item271.dat[grep("bldg", item271.dat$CK_SiteID, ignore.case = T),]

#join clean rbsa data onto appliances analysis data
item271.dat2 <- left_join(rbsa.dat, item271.dat1, by = c("CK_Building_ID" = "CK_SiteID"))

#subset to only MF
item271.dat3 <- item271.dat2[grep("Multifamily", item271.dat2$BuildingType),]
item271.dat3 <- item271.dat3[grep("3 or fewer floors", item271.dat3$BuildingTypeXX, ignore.case = T),]


#subset to only Dryers
item271.dat4 <- item271.dat3[which(item271.dat3$Type %in% c("Dryer")),]
item271.dat4$Age <- as.numeric(as.character(item271.dat4$Age))

item271.dat5 <- item271.dat4[which(!is.na(item271.dat4$Age)),]

#####################
# equipment vintage bins
#####################
item271.dat5$EquipVintage_bins <- item271.dat5$Age
item271.dat5$EquipVintage_bins[which(item271.dat5$Age < 1980)] <- "Pre 1980"
item271.dat5$EquipVintage_bins[which(item271.dat5$Age >= 1980 & item271.dat5$Age < 1990)] <- "1980-1989"
item271.dat5$EquipVintage_bins[which(item271.dat5$Age >= 1990 & item271.dat5$Age < 1995)] <- "1990-1994"
item271.dat5$EquipVintage_bins[which(item271.dat5$Age >= 1995 & item271.dat5$Age < 2000)] <- "1995-1999"
item271.dat5$EquipVintage_bins[which(item271.dat5$Age >= 2000 & item271.dat5$Age < 2005)] <- "2000-2004"
item271.dat5$EquipVintage_bins[which(item271.dat5$Age >= 2005 & item271.dat5$Age < 2010)] <- "2005-2009"
item271.dat5$EquipVintage_bins[which(item271.dat5$Age >= 2010 & item271.dat5$Age < 2015)] <- "2010-2014"
item271.dat5$EquipVintage_bins[which(item271.dat5$Age >= 2015)] <- "Post 2014"
#check uniques
unique(item271.dat5$EquipVintage_bins)

################################################
# Adding pop and sample sizes for weights
################################################
item271.data <- weightedData(item271.dat5[-which(colnames(item271.dat5) %in% c("Type"
                                                                               ,"Age"
                                                                               ,"EquipVintage_bins"))])
item271.data <- left_join(item271.data, item271.dat5[which(colnames(item271.dat5) %in% c("CK_Building_ID"
                                                                                         ,"Type"
                                                                                         ,"Age"
                                                                                         ,"EquipVintage_bins"))])
item271.data$count <- 1

#######################
# Weighted Analysis
#######################
item271.table <- proportions_one_group(CustomerLevelData = item271.data
                                       ,valueVariable = 'count'
                                       ,groupingVariable = 'EquipVintage_bins'
                                       ,weighted = TRUE)
levels(item271.table$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item271.table <- item271.table %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item271.table <- data.frame(item271.table)

item271.table.MF <- item271.table[which(names(item271.table) %notin% c("BuildingType"))]
exportTable(item271.table.MF, "MF", "Table 63", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item271.table <- proportions_one_group(CustomerLevelData = item271.data
                                       ,valueVariable = 'count'
                                       ,groupingVariable = 'EquipVintage_bins'
                                       ,weighted = FALSE)
unique(item271.table$EquipVintage_bins)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"Total")
item271.table <- item271.table %>% mutate(EquipVintage_bins = factor(EquipVintage_bins, levels = rowOrder)) %>% arrange(EquipVintage_bins)  
item271.table <- data.frame(item271.table)

item271.table.MF <- item271.table[which(names(item271.table) %notin% c("BuildingType"))]
exportTable(item271.table.MF, "MF", "Table 63", weighted = FALSE)
