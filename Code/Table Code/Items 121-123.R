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
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))



#############################################################################################
#Item 121: AVERAGE OCCUPANT AGE PER HOME BY STATE (SF table 128, MH table 103)
#############################################################################################
#subset to columns needed for analysis
item121.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,""
                                                                                     ,""))])
item121.dat$count <- 1

#remove any repeat header rows from exporting
item121.dat0 <- item121.dat[which(item121.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item121.dat1 <- left_join(item121.dat0, rbsa.dat, by = "CK_Cadmus_ID")













#############################################################################################
#Item 122: AVERAGE NUMBER OF OCCUPANTS PER HOME BY STATE (SF table 129, MH table 104)
#############################################################################################
#subset to columns needed for analysis
item122.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                              ,"Qty.Occupants"
                                                                              ,""))])
item122.dat$count <- 1

#remove any repeat header rows from exporting
item122.dat0 <- item122.dat[which(item122.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item122.dat1 <- left_join(rbsa.dat, item122.dat0, by = "CK_Cadmus_ID")

item122.dat2 <- item122.dat1[which(!is.na(item122.dat1$Qty.Occupants)), ]

item122.merge <- item122.dat2

################################################
# Adding pop and sample sizes for weights
################################################
item122.data <- weightedData(item122.merge[-which(colnames(item122.merge) %in% c("Qty.Occupants"
                                                                                 ,"count"))])
item122.data <- left_join(item122.data, item122.merge[which(colnames(item122.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Qty.Occupants"
                                                                                           ,"count"))])

item122.data$count <- 1
#######################
# Weighted Analysis
#######################
item122.final <- mean_one_group(item122.data
                                ,valueVariable = 'Qty.Occupants'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item122.final.SF <- item122.final[which(item122.final$BuildingType == "Single Family")
                                  ,-which(colnames(item122.final) %in% c("BuildingType"
                                                                         ,"Count"))]
item122.final.MH <- item122.final[which(item122.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item122.final) %in% c("BuildingType"
                                                                         ,"Count"))]

exportTable(item122.final.SF, "SF", "Table 129", weighted = TRUE)
exportTable(item122.final.MH, "MH", "Table 104", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item122.final <- mean_one_group_unweighted(item122.data
                                           ,valueVariable = 'Qty.Occupants'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item122.final.SF <- item122.final[which(item122.final$BuildingType == "Single Family")
                                  ,-which(colnames(item122.final) %in% c("BuildingType"
                                                                         ,"Count"
                                                                         ,"count"))]
item122.final.MH <- item122.final[which(item122.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item122.final) %in% c("BuildingType"
                                                                         ,"Count"
                                                                         ,"count"))]

exportTable(item122.final.SF, "SF", "Table 129", weighted = FALSE)
exportTable(item122.final.MH, "MH", "Table 104", weighted = FALSE)






#############################################################################################
#Item 123: AVERAGE NUMBER OF OCCUPANTS BY AGE CATEGORY BY STATE (SF table 130, MH table 105)
#############################################################################################
#subset to columns needed for analysis
item123.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_LessThan1"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_1_5"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_6_10"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_11_18"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_19_45"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_46_64"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_65Older"
                                                                                     ,""))])
colnames(item123.dat) <- c("CK_Cadmus_ID"
                           ,"Age_1_5"
                           ,"Age_11_18"
                           ,"Age_19_45"
                           ,"Age_46_64"
                           ,"Age_6_10"
                           ,"Age_65_Older"
                           ,"Age_Less_Than_1")
item123.dat$count <- 1

#remove any repeat header rows from exporting
item123.dat0 <- item123.dat[which(item123.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item123.dat1 <- left_join(item123.dat0, rbsa.dat, by = "CK_Cadmus_ID")
str(item123.dat1)
item123.dat1$Age_0_18 <- item123.dat1$Age_Less_Than_1 + item123.dat1$Age_1_5 + item123.dat1$Age_6_10 + item123.dat1$Age_11_18
item123.dat1$Age_19_64 <- item123.dat1$Age_19_45 + item123.dat1$Age_46_64

item123.dat2 <- item123.dat1[which(colnames(item123.dat1) %notin% c("Age_1_5"
                                                                    ,"Age_11_18"
                                                                    ,"Age_19_45"
                                                                    ,"Age_46_64"
                                                                    ,"Age_6_10"
                                                                    ,"Age_Less_Than_1"))]

item123.melt <- melt(item123.dat2, measure.vars = c("Age_0_18"
                                                    , "Age_19_64"
                                                    , "Age_65_Older"))
colnames(item123.melt)[which(colnames(item123.melt) == "variable")] <- "Age.Category"

item123.sum <- summarise(group_by(item123.melt, CK_Cadmus_ID, Age.Category)
                         ,Occupants = sum(value))

item123.merge <- left_join(rbsa.dat, item123.sum)
item123.merge <- item123.merge[which(!is.na(item123.merge$Occupants)),]



################################################
# Adding pop and sample sizes for weights
################################################
item123.data <- weightedData(item123.merge[-which(colnames(item123.merge) %in% c("Age.Category"               
                                                                               ,"Occupants"))])
item123.data <- left_join(item123.data, item123.merge[which(colnames(item123.merge) %in% c("CK_Cadmus_ID"
                                                                                         ,"Age.Category"               
                                                                                         ,"Occupants"))])
item123.data$count <- 1
#######################
# Weighted Analysis
#######################
item123.cast <- mean_two_groups(CustomerLevelData = item123.data
                                 ,valueVariable    = 'Occupants'
                                 ,byVariableRow    = 'Age.Category'
                                 ,byVariableColumn = 'State'
                                 ,columnAggregate  = "Region"
                                 ,rowAggregate     = "All_Ages")

item123.table <- data.frame("BuildingType"    = item123.cast$BuildingType
                            ,"Age.Category"   = item123.cast$Age.Category
                            ,"Mean_ID"        = item123.cast$Mean_ID
                            ,"SE_ID"          = item123.cast$SE_ID
                            ,"n_ID"           = item123.cast$SampleSize_ID
                            ,"Mean_MT"        = item123.cast$Mean_MT
                            ,"SE_MT"          = item123.cast$SE_MT
                            ,"n_MT"           = item123.cast$SampleSize_MT
                            ,"Mean_OR"        = item123.cast$Mean_OR
                            ,"SE_OR"          = item123.cast$SE_OR
                            ,"n_OR"           = item123.cast$SampleSize_OR
                            ,"Mean_WA"        = item123.cast$Mean_WA
                            ,"SE_WA"          = item123.cast$SE_WA
                            ,"n_WA"           = item123.cast$SampleSize_WA
                            ,"Mean_Region"    = item123.cast$Mean_Region
                            ,"SE_Region"      = item123.cast$SE_Region
                            ,"n_Region"       = item123.cast$SampleSize_Region
                            # ,"SampleSize"     = item123.cast$SampleSize_Region
)

item123.final.SF <- item123.table[which(item123.table$BuildingType == "Single Family")
                                  ,-which(colnames(item123.table) %in% c("BuildingType"))]
item123.final.MH <- item123.table[which(item123.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item123.table) %in% c("BuildingType"))]

exportTable(item123.final.SF, "SF", "Table ", weighted = TRUE)
exportTable(item123.final.MH, "MH", "Table ", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item123.final <- proportions_two_groups_unweighted(CustomerLevelData = item123.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'QTY_bins'
                                                   ,aggregateColumnName = "Region")

item123.cast <- dcast(setDT(item123.final)
                      , formula = BuildingType + QTY_bins ~ State
                      , value.var = c("Percent", "SE", "Count", "SampleSize"))


item123.table <- data.frame("BuildingType"   = item123.cast$BuildingType
                            ,"Annual.Wood.Use"= item123.cast$QTY_bins
                            ,"Percent_ID"     = item123.cast$Percent_ID
                            ,"SE_ID"          = item123.cast$SE_ID
                            ,"Count_ID"       = item123.cast$Count_ID
                            ,"Percent_MT"     = item123.cast$Percent_MT
                            ,"SE_MT"          = item123.cast$SE_MT
                            ,"Count_MT"       = item123.cast$Count_MT
                            ,"Percent_OR"     = item123.cast$Percent_OR
                            ,"SE_OR"          = item123.cast$SE_OR
                            ,"Count_OR"       = item123.cast$Count_OR
                            ,"Percent_WA"     = item123.cast$Percent_WA
                            ,"SE_WA"          = item123.cast$SE_WA
                            ,"Count_WA"       = item123.cast$Count_WA
                            ,"Percent_Region" = item123.cast$Percent_Region
                            ,"SE_Region"      = item123.cast$SE_Region
                            ,"Count_Region"   = item123.cast$Count_Region
                            # ,"SampleSize"     = item123.cast$SampleSize_Region
)
stopifnot(sum(item123.table[which(item123.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(item123.table))], na.rm = T) == 10)


item123.final.SF <- item123.table[which(item123.table$BuildingType == "Single Family")
                                  ,-which(colnames(item123.table) %in% c("BuildingType"))]
item123.final.MH <- item123.table[which(item123.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item123.table) %in% c("BuildingType"))]

exportTable(item123.final.SF, "SF", "Table ", weighted = FALSE)
exportTable(item123.final.MH, "MH", "Table ", weighted = FALSE)

