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

#Read in data for analysis
survey.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Labeled and Translated")
#clean cadmus IDs
survey.dat$CK_Cadmus_ID <- trimws(toupper(survey.dat$NEXID))




#############################################################################################
#Item 124: DISTRIBUTION OF HOMES BY OWNERSHIP TYPE AND STATE (SF table 131, MH table 106)
#############################################################################################
#subset to columns needed for analysis
item124.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Do.you.own.or.rent.your.home?"))]
colnames(item124.dat) <- c("Ownership.Type", "CK_Cadmus_ID")

#merge together analysis data with cleaned RBSA data
item124.dat1 <- left_join(rbsa.dat, item124.dat, by = "CK_Cadmus_ID")
unique(item124.dat1$Ownership.Type)

#remove NA from ownership type
item124.dat2 <- item124.dat1[which(!(is.na(item124.dat1$Ownership.Type))),]
length(unique(item124.dat2$CK_Cadmus_ID))

################################################
# Adding pop and sample sizes for weights
################################################
item124.data <- weightedData(item124.dat2[-which(colnames(item124.dat2) %in% c("Ownership.Type"))])
item124.data <- left_join(item124.data, item124.dat2[which(colnames(item124.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"Ownership.Type"))])
item124.data$count <- 1
#######################
# Weighted Analysis
#######################
item124.final <- proportionRowsAndColumns1(CustomerLevelData = item124.data
                                          ,valueVariable    = 'count'
                                          ,columnVariable   = 'State'
                                          ,rowVariable      = 'Ownership.Type'
                                          ,aggregateColumnName = "Region")

item124.cast <- dcast(setDT(item124.final)
                     , formula = BuildingType + Ownership.Type ~ State
                     , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item124.table <- data.frame("BuildingType"   = item124.cast$BuildingType
                           ,"Ownership.Type" = item124.cast$Ownership.Type
                           ,"Percent_ID"     = item124.cast$w.percent_ID
                           ,"SE_ID"          = item124.cast$w.SE_ID
                           ,"n_ID"           = item124.cast$n_ID
                           ,"Percent_MT"     = item124.cast$w.percent_MT
                           ,"SE_MT"          = item124.cast$w.SE_MT
                           ,"n_MT"           = item124.cast$n_MT
                           ,"Percent_OR"     = item124.cast$w.percent_OR
                           ,"SE_OR"          = item124.cast$w.SE_OR
                           ,"n_OR"           = item124.cast$n_OR
                           ,"Percent_WA"     = item124.cast$w.percent_WA
                           ,"SE_WA"          = item124.cast$w.SE_WA
                           ,"n_WA"           = item124.cast$n_WA
                           ,"Percent_Region" = item124.cast$w.percent_Region
                           ,"SE_Region"      = item124.cast$w.SE_Region
                           ,"n_Region"       = item124.cast$n_Region
                           ,"EB_ID"          = item124.cast$EB_ID
                           ,"EB_MT"          = item124.cast$EB_MT
                           ,"EB_OR"          = item124.cast$EB_OR
                           ,"EB_WA"          = item124.cast$EB_WA
                           ,"EB_Region"      = item124.cast$EB_Region
)
#QAQC
stopifnot(sum(item124.table[which(item124.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(item124.table))], na.rm = T) == 10)


item124.final.SF <- item124.table[which(item124.table$BuildingType == "Single Family")
                                  ,-which(colnames(item124.table) %in% c("BuildingType"))]
item124.final.MH <- item124.table[which(item124.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item124.table) %in% c("BuildingType"))]

# exportTable(item124.final.SF, "SF", "Table 131", weighted = TRUE)
exportTable(item124.final.MH, "MH", "Table 106", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item124.final <- proportions_two_groups_unweighted(CustomerLevelData = item124.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'Ownership.Type'
                                                   ,aggregateColumnName = "Region")

item124.cast <- dcast(setDT(item124.final)
                      , formula = BuildingType + Ownership.Type ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


item124.table <- data.frame("BuildingType"    = item124.cast$BuildingType
                            ,"Ownership.Type" = item124.cast$Ownership.Type
                            ,"Percent_ID"     = item124.cast$Percent_ID
                            ,"SE_ID"          = item124.cast$SE_ID
                            ,"n_ID"           = item124.cast$n_ID
                            ,"Percent_MT"     = item124.cast$Percent_MT
                            ,"SE_MT"          = item124.cast$SE_MT
                            ,"n_MT"           = item124.cast$n_MT
                            ,"Percent_OR"     = item124.cast$Percent_OR
                            ,"SE_OR"          = item124.cast$SE_OR
                            ,"n_OR"           = item124.cast$n_OR
                            ,"Percent_WA"     = item124.cast$Percent_WA
                            ,"SE_WA"          = item124.cast$SE_WA
                            ,"n_WA"           = item124.cast$n_WA
                            ,"Percent_Region" = item124.cast$Percent_Region
                            ,"SE_Region"      = item124.cast$SE_Region
                            ,"n_Region"       = item124.cast$n_Region
)
stopifnot(sum(item124.table[which(item124.table$BuildingType == "Single Family")
                            ,grep("Percent",colnames(item124.table))], na.rm = T) == 10)


item124.final.SF <- item124.table[which(item124.table$BuildingType == "Single Family")
                                  ,-which(colnames(item124.table) %in% c("BuildingType"))]
item124.final.MH <- item124.table[which(item124.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item124.table) %in% c("BuildingType"))]

# exportTable(item124.final.SF, "SF", "Table 131", weighted = FALSE)
exportTable(item124.final.MH, "MH", "Table 106", weighted = FALSE)







#############################################################################################
#Item 125: PERCENTAGE OF HOMES AS PRIMARY RESIDENCE BY STATE (SF table 132, MH table 107)
#############################################################################################
#subset to columns needed for analysis
item125.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_IsThisYourPrimaryHome_Y_N"
                                                                                     ,""))])
colnames(item125.dat) <- c("CK_Cadmus_ID", "Primary_Home")

#remove any repeat header rows from exporting
item125.dat0 <- item125.dat[which(item125.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item125.dat1 <- left_join(rbsa.dat, item125.dat0, by = "CK_Cadmus_ID")

unique(item125.dat1$Primary_Home)

item125.dat2 <- item125.dat1[which(!(is.na(item125.dat1$Primary_Home))),]
item125.dat2 <- item125.dat2[which(item125.dat2$Primary_Home != "N/A"),]

item125.dat2$Ind <- 0
item125.dat2$Ind[which(item125.dat2$Primary_Home == "Primary")] <- 1

unique(item125.dat2$Ind)
unique(item125.dat2$Primary_Home)

################################################
# Adding pop and sample sizes for weights
################################################
item125.data <- weightedData(item125.dat2[-which(colnames(item125.dat2) %in% c("Primary_Home"
                                                                               ,"Ind"))])
item125.data <- left_join(item125.data, item125.dat2[which(colnames(item125.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Primary_Home"
                                                                                         ,"Ind"))])
item125.data$count <- 1
item125.data$Count <- 1
#######################
# Weighted Analysis
#######################
item125.final <- proportions_one_group(CustomerLevelData = item125.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = TRUE)

item125.final.SF <- item125.final[which(item125.final$BuildingType == "Single Family")
                                  ,-which(colnames(item125.final) %in% c("BuildingType"))]
item125.final.MH <- item125.final[which(item125.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item125.final) %in% c("BuildingType"))]

# exportTable(item125.final.SF, "SF", "Table 132", weighted = TRUE)
exportTable(item125.final.MH, "MH", "Table 107", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item125.final <- proportions_one_group(CustomerLevelData = item125.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = FALSE)

item125.final.SF <- item125.final[which(item125.final$BuildingType == "Single Family")
                                  ,-which(colnames(item125.final) %in% c("BuildingType"))]
item125.final.MH <- item125.final[which(item125.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item125.final) %in% c("BuildingType"))]

# exportTable(item125.final.SF, "SF", "Table 132", weighted = FALSE)
exportTable(item125.final.MH, "MH", "Table 107", weighted = FALSE)





#############################################################################################
# Needs to be updated once we get the correct fields for this table
#############################################################################################
#Item 126: PERCENTAGE OF HOMES WITH HOME OFFICES BY STATE (SF table 133, MH table 108)
#############################################################################################
# #subset to columns needed for analysis
# item126.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
#                                                                                      ,""
#                                                                                      ,""))])
# colnames(item126.dat) <- c("CK_Cadmus_ID", "")
# item126.dat$count <- 1
# 
# #remove any repeat header rows from exporting
# item126.dat0 <- item126.dat[which(item126.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# #merge together analysis data with cleaned RBSA data
# item126.dat1 <- left_join(item126.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# 
# unique(item126.dat1$)
# 
# item126.dat2 <- item126.dat1[which(!(is.na(item126.dat1$))),]





#############################################################################################
#Item 127: DISTRIBUTION OF HOMES WITH ELECTRIC FUEL ASSISTANCE BY PERCENTAGE OF ASSISTANCE AND STATE 
# (SF table 134, MH table 109)
#############################################################################################
#subset to columns needed for analysis
item127.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Does.your.household.receive.financial.assistance.to.pay.a.portion.or.all.of.your.electric.utility.bill?"
                                                            ,"For.what.share.does.your.household.receive.assistance?.Electric"))]
colnames(item127.dat) <- c("Financial.Assistance", "Percent.Assistance", "CK_Cadmus_ID")

#merge together analysis data with cleaned RBSA data
item127.dat1 <- left_join(rbsa.dat, item127.dat, by = "CK_Cadmus_ID")
item127.dat1$Percent.Assistance[which(item127.dat1$Financial.Assistance == "No")] <- "No Utility Bill Assistance"
unique(item127.dat1$Percent.Assistance)

item127.dat2 <- item127.dat1[which(!(is.na(item127.dat1$Percent.Assistance))),]
item127.dat3 <- item127.dat2[which(item127.dat2$Percent.Assistance %notin% c("Don't know"
                                                                             ,"Prefer not to say"
                                                                             ,"N/A")),]

item127.dat3$CK_Cadmus_ID[which(duplicated(item127.dat3$CK_Cadmus_ID[which(item127.dat3$BuildingType == "Single Family")]))]
length(unique(item127.dat3$CK_Cadmus_ID[which(item127.dat3$BuildingType == "Single Family")]))
################################################
# Adding pop and sample sizes for weights
################################################
item127.data <- weightedData(item127.dat3[-which(colnames(item127.dat3) %in% c("Financial.Assistance"
                                                                               ,"Percent.Assistance"))])
item127.data <- left_join(item127.data, item127.dat3[which(colnames(item127.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"Financial.Assistance"
                                                                                         ,"Percent.Assistance"))])
item127.data$count <- 1
#######################
# Weighted Analysis
#######################
item127.final <- proportionRowsAndColumns1(CustomerLevelData = item127.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'Percent.Assistance'
                                           ,aggregateColumnName = "Region")

item127.cast <- dcast(setDT(item127.final)
                      , formula = BuildingType + Percent.Assistance ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item127.table <- data.frame("BuildingType"        = item127.cast$BuildingType
                            ,"Percent.Assistance" = item127.cast$Percent.Assistance
                            ,"Percent_ID"         = item127.cast$w.percent_ID
                            ,"SE_ID"              = item127.cast$w.SE_ID
                            ,"n_ID"               = item127.cast$n_ID
                            ,"Percent_MT"         = item127.cast$w.percent_MT
                            ,"SE_MT"              = item127.cast$w.SE_MT
                            ,"n_MT"               = item127.cast$n_MT
                            ,"Percent_OR"         = item127.cast$w.percent_OR
                            ,"SE_OR"              = item127.cast$w.SE_OR
                            ,"n_OR"               = item127.cast$n_OR
                            ,"Percent_WA"         = item127.cast$w.percent_WA
                            ,"SE_WA"              = item127.cast$w.SE_WA
                            ,"n_WA"               = item127.cast$n_WA
                            ,"Percent_Region"     = item127.cast$w.percent_Region
                            ,"SE_Region"          = item127.cast$w.SE_Region
                            ,"n_Region"           = item127.cast$n_Region
                            ,"EB_ID"          = item127.cast$EB_ID
                            ,"EB_MT"          = item127.cast$EB_MT
                            ,"EB_OR"          = item127.cast$EB_OR
                            ,"EB_WA"          = item127.cast$EB_WA
                            ,"EB_Region"      = item127.cast$EB_Region
)

# row ordering example code
unique(item127.table$Percent.Assistance)
rowOrder <- c("Less than 25%"
              ,"Between 26% and 50%"
              ,"Between 51% and 75%"
              ,"Between 76% and 100%"
              ,"No Utility Bill Assistance"
              ,"Total")
item127.table <- item127.table %>% mutate(Percent.Assistance = factor(Percent.Assistance, levels = rowOrder)) %>% arrange(Percent.Assistance)  
item127.table <- data.frame(item127.table)

item127.final.SF <- item127.table[which(item127.table$BuildingType == "Single Family")
                                  ,-which(colnames(item127.table) %in% c("BuildingType"))]
item127.final.MH <- item127.table[which(item127.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item127.table) %in% c("BuildingType"))]

# exportTable(item127.final.SF, "SF", "Table 134", weighted = TRUE)
exportTable(item127.final.MH, "MH", "Table 109", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item127.final <- proportions_two_groups_unweighted(CustomerLevelData = item127.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'Percent.Assistance'
                                                   ,aggregateColumnName = "Region")

item127.cast <- dcast(setDT(item127.final)
                      , formula = BuildingType + Percent.Assistance ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


item127.table <- data.frame("BuildingType"        = item127.cast$BuildingType
                            ,"Percent.Assistance" = item127.cast$Percent.Assistance
                            ,"Percent_ID"         = item127.cast$Percent_ID
                            ,"SE_ID"              = item127.cast$SE_ID
                            ,"n_ID"               = item127.cast$n_ID
                            ,"Percent_MT"         = item127.cast$Percent_MT
                            ,"SE_MT"              = item127.cast$SE_MT
                            ,"n_MT"               = item127.cast$n_MT
                            ,"Percent_OR"         = item127.cast$Percent_OR
                            ,"SE_OR"              = item127.cast$SE_OR
                            ,"n_OR"               = item127.cast$n_OR
                            ,"Percent_WA"         = item127.cast$Percent_WA
                            ,"SE_WA"              = item127.cast$SE_WA
                            ,"n_WA"               = item127.cast$n_WA
                            ,"Percent_Region"     = item127.cast$Percent_Region
                            ,"SE_Region"          = item127.cast$SE_Region
                            ,"n_Region"           = item127.cast$n_Region
)

# row ordering example code
unique(item127.table$Percent.Assistance)
rowOrder <- c("Less than 25%"
              ,"Between 26% and 50%"
              ,"Between 51% and 75%"
              ,"Between 76% and 100%"
              ,"No Utility Bill Assistance"
              ,"Total")
item127.table <- item127.table %>% mutate(Percent.Assistance = factor(Percent.Assistance, levels = rowOrder)) %>% arrange(Percent.Assistance)  
item127.table <- data.frame(item127.table)

item127.final.SF <- item127.table[which(item127.table$BuildingType == "Single Family")
                                  ,-which(colnames(item127.table) %in% c("BuildingType"))]
item127.final.MH <- item127.table[which(item127.table$BuildingType == "Manufactured")
                                  ,-which(colnames(item127.table) %in% c("BuildingType"))]

# exportTable(item127.final.SF, "SF", "Table 134", weighted = FALSE)
exportTable(item127.final.MH, "MH", "Table 109", weighted = FALSE)











#############################################################################################
#TABLE AD: Percent of homes reporting having completed an energy audit in the last two years
#############################################################################################
#subset to columns needed for analysis
tableAD.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Has.your.household.completed.a.home.energy.audit.in.the.past.2.years?"))]
colnames(tableAD.dat) <- c("Energy.Audit", "CK_Cadmus_ID")

#merge together analysis data with cleaned RBSA data
tableAD.dat1 <- left_join(rbsa.dat, tableAD.dat, by = "CK_Cadmus_ID")
tableAD.dat2 <- tableAD.dat1[which(tableAD.dat1$Energy.Audit %in% c("Yes","No")),]
unique(tableAD.dat2$Energy.Audit)

tableAD.dat2$Ind <- 0
tableAD.dat2$Ind[which(tableAD.dat2$Energy.Audit == "Yes")] <- 1

################################################
# Adding pop and sample sizes for weights
################################################
tableAD.data <- weightedData(tableAD.dat2[-which(colnames(tableAD.dat2) %in% c("Energy.Audit"
                                                                               ,"Ind"))])
tableAD.data <- left_join(tableAD.data, tableAD.dat2[which(colnames(tableAD.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Energy.Audit"
                                                                                         ,"Ind"))])
tableAD.data$count <- 1
tableAD.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableAD.final <- proportions_one_group(CustomerLevelData = tableAD.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region")
tableAD.final$State[which(tableAD.final$State == "Total")] <- "Region"


tableAD.final.SF <- tableAD.final[which(tableAD.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableAD.final) %in% c("BuildingType"))]
tableAD.final.MH <- tableAD.final[which(tableAD.final$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAD.final) %in% c("BuildingType"))]

# exportTable(tableAD.final.SF, "SF", "Table AD", weighted = TRUE)
exportTable(tableAD.final.MH, "MH", "Table AD", weighted = TRUE)

#######################
# MULTIFAMILY
#######################
tableAD.final.MF <- proportions_one_group(CustomerLevelData = tableAD.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Types")
tableAD.final.MF <- tableAD.final.MF[which(tableAD.final.MF$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAD.final.MF) %in% c("BuildingType"))]
# exportTable(tableAD.final.MF, "MF", "Table AD", weighted = TRUE)





#######################
# Unweighted Analysis
#######################
tableAD.final <- proportions_one_group(CustomerLevelData = tableAD.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = FALSE)
tableAD.final$State[which(tableAD.final$State == "Total")] <- "Region"


tableAD.final.SF <- tableAD.final[which(tableAD.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableAD.final) %in% c("BuildingType"))]
tableAD.final.MH <- tableAD.final[which(tableAD.final$BuildingType == "Manufactured")
                                  ,-which(colnames(tableAD.final) %in% c("BuildingType"))]

# exportTable(tableAD.final.SF, "SF", "Table AD", weighted = FALSE)
exportTable(tableAD.final.MH, "MH", "Table AD", weighted = FALSE)

#######################
# MULTIFAMILY
#######################
tableAD.final.MF <- proportions_one_group(CustomerLevelData = tableAD.data
                                          ,valueVariable = "Ind"
                                          ,groupingVariable = "HomeType"
                                          ,total.name = "All Types"
                                          ,weighted = FALSE)
tableAD.final.MF <- tableAD.final.MF[which(tableAD.final.MF$BuildingType == "Multifamily")
                                     ,-which(colnames(tableAD.final.MF) %in% c("BuildingType"))]
# exportTable(tableAD.final.MF, "MF", "Table AD", weighted = FALSE)
































############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean os data
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]

#############################################################################################
#Item 124: DISTRIBUTION OF HOMES BY OWNERSHIP TYPE AND CK_Building_ID (SF table 131, MH table 106)
#############################################################################################
#subset to columns needed for analysis
item124.os.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Do.you.own.or.rent.your.home?"))]
colnames(item124.os.dat) <- c("Ownership.Type", "CK_Cadmus_ID")

#merge together analysis data with cleaned scl data
item124.os.dat1 <- left_join(os.dat, item124.os.dat, by = "CK_Cadmus_ID")
unique(item124.os.dat1$Ownership.Type)

#remove NA from ownership type
item124.os.dat2 <- item124.os.dat1[which(!(is.na(item124.os.dat1$Ownership.Type))),]
length(unique(item124.os.dat2$CK_Cadmus_ID))

################################################
# Adding pop and sample sizes for weights
################################################
item124.os.data <- weightedData(item124.os.dat2[-which(colnames(item124.os.dat2) %in% c("Ownership.Type"))])
item124.os.data <- left_join(item124.os.data, unique(item124.os.dat2[which(colnames(item124.os.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Ownership.Type"))]))
item124.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item124.os.final <- proportionRowsAndColumns1(CustomerLevelData = item124.os.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'CK_Building_ID'
                                           ,rowVariable      = 'Ownership.Type'
                                           ,aggregateColumnName = "Remove")

item124.os.cast <- dcast(setDT(item124.os.final)
                      , formula = BuildingType + Ownership.Type ~ CK_Building_ID
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

if(os.ind == "scl"){
  item124.os.table <- data.frame("BuildingType"   = item124.os.cast$BuildingType
                                 ,"Ownership.Type" = item124.os.cast$Ownership.Type
                                 ,"Percent_SCL.GenPop"   = item124.os.cast$`w.percent_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item124.os.cast$`w.SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item124.os.cast$`n_SCL GenPop`
                                 ,"Percent_SCL.LI"       = item124.os.cast$`w.percent_SCL LI`
                                 ,"SE_SCL.LI"            = item124.os.cast$`w.SE_SCL LI`
                                 ,"n_SCL.LI"             = item124.os.cast$`n_SCL LI`
                                 ,"Percent_SCL.EH"       = item124.os.cast$`w.percent_SCL EH`
                                 ,"SE_SCL.EH"            = item124.os.cast$`w.SE_SCL EH`
                                 ,"n_SCL.EH"             = item124.os.cast$`n_SCL EH`
                                 ,"Percent_2017.RBSA.PS" = item124.os.cast$`w.percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item124.os.cast$`w.SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item124.os.cast$`n_2017 RBSA PS`
                                 ,"EB_SCL.GenPop"        = item124.os.cast$`EB_SCL GenPop`
                                 ,"EB_SCL.LI"            = item124.os.cast$`EB_SCL LI`
                                 ,"EB_SCL.EH"            = item124.os.cast$`EB_SCL EH`
                                 ,"EB_2017.RBSA.PS"      = item124.os.cast$`EB_2017 RBSA PS`
  )
  
}else if(os.ind == "snopud"){
  item124.os.table <- data.frame("BuildingType"   = item124.os.cast$BuildingType
                                 ,"Ownership.Type" = item124.os.cast$Ownership.Type
                                 ,"Percent_SnoPUD"          = item124.os.cast$`w.percent_SnoPUD`
                                 ,"SE_SnoPUD"               = item124.os.cast$`w.SE_SnoPUD`
                                 ,"n_SnoPUD"                = item124.os.cast$`n_SnoPUD`
                                 ,"Percent_2017.RBSA.PS"    = item124.os.cast$`w.percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"         = item124.os.cast$`w.SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"          = item124.os.cast$`n_2017 RBSA PS`
                                 ,"Percent_RBSA.NW"         = item124.os.cast$`w.percent_2017 RBSA NW`
                                 ,"SE_RBSA.NW"              = item124.os.cast$`w.SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"               = item124.os.cast$`n_2017 RBSA NW`
                                 ,"EB_SnoPUD"               = item124.os.cast$`EB_SnoPUD`
                                 ,"EB_2017.RBSA.PS"         = item124.os.cast$`EB_2017 RBSA PS`
                                 ,"EB_RBSA.NW"              = item124.os.cast$`EB_2017 RBSA NW`
  )
  
}

item124.os.final.SF <- item124.os.table[which(item124.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item124.os.table) %in% c("BuildingType"))]

exportTable(item124.os.final.SF, "SF", "Table 131", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item124.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item124.os.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'CK_Building_ID'
                                                   ,rowVariable      = 'Ownership.Type'
                                                   ,aggregateColumnName = "Remove")

item124.os.cast <- dcast(setDT(item124.os.final)
                      , formula = BuildingType + Ownership.Type ~ CK_Building_ID
                      , value.var = c("Percent", "SE", "Count", "n"))

if(os.ind == "scl"){
  item124.os.table <- data.frame("BuildingType"    = item124.os.cast$BuildingType
                                 ,"Ownership.Type" = item124.os.cast$Ownership.Type
                                 ,"Percent_SCL.GenPop"   = item124.os.cast$`Percent_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item124.os.cast$`SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item124.os.cast$`n_SCL GenPop`
                                 ,"Percent_SCL.LI"       = item124.os.cast$`Percent_SCL LI`
                                 ,"SE_SCL.LI"            = item124.os.cast$`SE_SCL LI`
                                 ,"n_SCL.LI"             = item124.os.cast$`n_SCL LI`
                                 ,"Percent_SCL.EH"       = item124.os.cast$`Percent_SCL EH`
                                 ,"SE_SCL.EH"            = item124.os.cast$`SE_SCL EH`
                                 ,"n_SCL.EH"             = item124.os.cast$`n_SCL EH`
                                 ,"Percent_2017.RBSA.PS" = item124.os.cast$`Percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item124.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item124.os.cast$`n_2017 RBSA PS`
  )
  
}else if(os.ind == "snopud"){
  item124.os.table <- data.frame("BuildingType"    = item124.os.cast$BuildingType
                                 ,"Ownership.Type" = item124.os.cast$Ownership.Type
                                 ,"Percent_SnoPUD"          = item124.os.cast$`Percent_SnoPUD`
                                 ,"SE_SnoPUD"               = item124.os.cast$`SE_SnoPUD`
                                 ,"n_SnoPUD"                = item124.os.cast$`n_SnoPUD`
                                 ,"Percent_2017.RBSA.PS"    = item124.os.cast$`Percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"         = item124.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"          = item124.os.cast$`n_2017 RBSA PS`
                                 ,"Percent_RBSA.NW"         = item124.os.cast$`Percent_2017 RBSA NW`
                                 ,"SE_RBSA.NW"              = item124.os.cast$`SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"               = item124.os.cast$`n_2017 RBSA NW`
  )
  
}

item124.os.final.SF <- item124.os.table[which(item124.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item124.os.table) %in% c("BuildingType"))]

exportTable(item124.os.final.SF, "SF", "Table 131", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
#Item 125: PERCENTAGE OF HOMES AS PRIMARY RESIDENCE BY CK_Building_ID (SF table 132, MH table 107)
#############################################################################################
#subset to columns needed for analysis
item125.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_IsThisYourPrimaryHome_Y_N"
                                                                                     ,""))])
colnames(item125.os.dat) <- c("CK_Cadmus_ID", "Primary_Home")

#remove any repeat header rows from exporting
item125.os.dat0 <- item125.os.dat[which(item125.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned scl data
item125.os.dat1 <- left_join(os.dat, item125.os.dat0, by = "CK_Cadmus_ID")

unique(item125.os.dat1$Primary_Home)

item125.os.dat2 <- item125.os.dat1[which(!(is.na(item125.os.dat1$Primary_Home))),]
item125.os.dat2 <- item125.os.dat2[which(item125.os.dat2$Primary_Home != "N/A"),]

item125.os.dat2$Ind <- 0
item125.os.dat2$Ind[which(item125.os.dat2$Primary_Home == "Primary")] <- 1

unique(item125.os.dat2$Ind)
unique(item125.os.dat2$Primary_Home)

################################################
# Adding pop and sample sizes for weights
################################################
item125.os.data <- weightedData(item125.os.dat2[-which(colnames(item125.os.dat2) %in% c("Primary_Home"
                                                                               ,"Ind"))])
item125.os.data <- left_join(item125.os.data, unique(item125.os.dat2[which(colnames(item125.os.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Primary_Home"
                                                                                         ,"Ind"))]))
item125.os.data$count <- 1
item125.os.data$Count <- 1
#######################
# Weighted Analysis
#######################
item125.os.final <- proportions_one_group(CustomerLevelData = item125.os.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'CK_Building_ID'
                                       ,total.name = "Remove"
                                       ,weighted = TRUE)
item125.os.final <- item125.os.final[which(item125.os.final$CK_Building_ID != "Total"),]

item125.os.final.SF <- item125.os.final[which(item125.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item125.os.final) %in% c("BuildingType"))]

exportTable(item125.os.final.SF, "SF", "Table 132", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item125.os.final <- proportions_one_group(CustomerLevelData = item125.os.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'CK_Building_ID'
                                       ,total.name = "Remove"
                                       ,weighted = FALSE)
item125.os.final <- item125.os.final[which(item125.os.final$CK_Building_ID != "Total"),]

item125.os.final.SF <- item125.os.final[which(item125.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item125.os.final) %in% c("BuildingType"))]

exportTable(item125.os.final.SF, "SF", "Table 132", weighted = FALSE, osIndicator = export.ind, OS = T)






#############################################################################################
#Item 127: DISTRIBUTION OF HOMES WITH ELECTRIC FUEL ASSISTANCE BY PERCENTAGE OF ASSISTANCE AND CK_Building_ID 
# (SF table 134, MH table 109)
#############################################################################################
#subset to columns needed for analysis
item127.os.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Does.your.household.receive.financial.assistance.to.pay.a.portion.or.all.of.your.electric.utility.bill?"
                                                            ,"For.what.share.does.your.household.receive.assistance?.Electric"))]
colnames(item127.os.dat) <- c("Financial.Assistance", "Percent.Assistance", "CK_Cadmus_ID")

#merge together analysis data with cleaned scl data
item127.os.dat1 <- left_join(os.dat, item127.os.dat, by = "CK_Cadmus_ID")
item127.os.dat1$Percent.Assistance[which(item127.os.dat1$Financial.Assistance == "No")] <- "No Utility Bill Assistance"
unique(item127.os.dat1$Percent.Assistance)

item127.os.dat2 <- item127.os.dat1[which(!(is.na(item127.os.dat1$Percent.Assistance))),]
item127.os.dat3 <- item127.os.dat2[which(item127.os.dat2$Percent.Assistance %notin% c("Don't know"
                                                                             ,"Prefer not to say"
                                                                             ,"N/A")),]

item127.os.dat3$CK_Cadmus_ID[which(duplicated(item127.os.dat3$CK_Cadmus_ID[which(item127.os.dat3$BuildingType == "Single Family")]))]
length(unique(item127.os.dat3$CK_Cadmus_ID[which(item127.os.dat3$BuildingType == "Single Family")]))
################################################
# Adding pop and sample sizes for weights
################################################
item127.os.data <- weightedData(item127.os.dat3[-which(colnames(item127.os.dat3) %in% c("Financial.Assistance"
                                                                               ,"Percent.Assistance"))])
item127.os.data <- left_join(item127.os.data, unique(item127.os.dat3[which(colnames(item127.os.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"Financial.Assistance"
                                                                                         ,"Percent.Assistance"))]))
item127.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item127.os.final <- proportionRowsAndColumns1(CustomerLevelData = item127.os.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'CK_Building_ID'
                                           ,rowVariable      = 'Percent.Assistance'
                                           ,aggregateColumnName = "Remove")

item127.os.cast <- dcast(setDT(item127.os.final)
                      , formula = BuildingType + Percent.Assistance ~ CK_Building_ID
                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

if(os.ind == "scl"){
  item127.os.table <- data.frame("BuildingType"        = item127.os.cast$BuildingType
                                 ,"Percent.Assistance" = item127.os.cast$Percent.Assistance
                                 ,"Percent_SCL.GenPop"   = item127.os.cast$`w.percent_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item127.os.cast$`w.SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item127.os.cast$`n_SCL GenPop`
                                 ,"Percent_SCL.LI"       = item127.os.cast$`w.percent_SCL LI`
                                 ,"SE_SCL.LI"            = item127.os.cast$`w.SE_SCL LI`
                                 ,"n_SCL.LI"             = item127.os.cast$`n_SCL LI`
                                 ,"Percent_SCL.EH"       = item127.os.cast$`w.percent_SCL EH`
                                 ,"SE_SCL.EH"            = item127.os.cast$`w.SE_SCL EH`
                                 ,"n_SCL.EH"             = item127.os.cast$`n_SCL EH`
                                 ,"Percent_2017.RBSA.PS" = item127.os.cast$`w.percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item127.os.cast$`w.SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item127.os.cast$`n_2017 RBSA PS`
                                 ,"EB_SCL.GenPop"        = item127.os.cast$`EB_SCL GenPop`
                                 ,"EB_SCL.LI"            = item127.os.cast$`EB_SCL LI`
                                 ,"EB_SCL.EH"            = item127.os.cast$`EB_SCL EH`
                                 ,"EB_2017.RBSA.PS"      = item127.os.cast$`EB_2017 RBSA PS`
  )
  
}else if(os.ind == "snopud"){
  item127.os.table <- data.frame("BuildingType"        = item127.os.cast$BuildingType
                                 ,"Percent.Assistance" = item127.os.cast$Percent.Assistance
                                 ,"Percent_SnoPUD"          = item127.os.cast$`w.percent_SnoPUD`
                                 ,"SE_SnoPUD"               = item127.os.cast$`w.SE_SnoPUD`
                                 ,"n_SnoPUD"                = item127.os.cast$`n_SnoPUD`
                                 ,"Percent_2017.RBSA.PS"    = item127.os.cast$`w.percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"         = item127.os.cast$`w.SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"          = item127.os.cast$`n_2017 RBSA PS`
                                 ,"Percent_RBSA.NW"         = item127.os.cast$`w.percent_2017 RBSA NW`
                                 ,"SE_RBSA.NW"              = item127.os.cast$`w.SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"               = item127.os.cast$`n_2017 RBSA NW`
                                 ,"EB_SnoPUD"               = item127.os.cast$`EB_SnoPUD`
                                 ,"EB_2017.RBSA.PS"         = item127.os.cast$`EB_2017 RBSA PS`
                                 ,"EB_RBSA.NW"              = item127.os.cast$`EB_2017 RBSA NW`
  )
  
}


# row ordering example code
unique(item127.os.table$Percent.Assistance)
rowOrder <- c("Less than 25%"
              ,"Between 26% and 50%"
              ,"Between 51% and 75%"
              ,"Between 76% and 100%"
              ,"No Utility Bill Assistance"
              ,"Total")
item127.os.table <- item127.os.table %>% mutate(Percent.Assistance = factor(Percent.Assistance, levels = rowOrder)) %>% arrange(Percent.Assistance)  
item127.os.table <- data.frame(item127.os.table)

item127.os.final.SF <- item127.os.table[which(item127.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item127.os.table) %in% c("BuildingType"))]

exportTable(item127.os.final.SF, "SF", "Table 134", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
item127.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item127.os.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'CK_Building_ID'
                                                   ,rowVariable      = 'Percent.Assistance'
                                                   ,aggregateColumnName = "Remove")

item127.os.cast <- dcast(setDT(item127.os.final)
                      , formula = BuildingType + Percent.Assistance ~ CK_Building_ID
                      , value.var = c("Percent", "SE", "Count", "n"))

if(os.ind == "scl"){
  item127.os.table <- data.frame("BuildingType"        = item127.os.cast$BuildingType
                                 ,"Percent.Assistance" = item127.os.cast$Percent.Assistance
                                 ,"Percent_SCL.GenPop"   = item127.os.cast$`Percent_SCL GenPop`
                                 ,"SE_SCL.GenPop"        = item127.os.cast$`SE_SCL GenPop`
                                 ,"n_SCL.GenPop"         = item127.os.cast$`n_SCL GenPop`
                                 ,"Percent_SCL.LI"       = item127.os.cast$`Percent_SCL LI`
                                 ,"SE_SCL.LI"            = item127.os.cast$`SE_SCL LI`
                                 ,"n_SCL.LI"             = item127.os.cast$`n_SCL LI`
                                 ,"Percent_SCL.EH"       = item127.os.cast$`Percent_SCL EH`
                                 ,"SE_SCL.EH"            = item127.os.cast$`SE_SCL EH`
                                 ,"n_SCL.EH"             = item127.os.cast$`n_SCL EH`
                                 ,"Percent_2017.RBSA.PS" = item127.os.cast$`Percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"      = item127.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"       = item127.os.cast$`n_2017 RBSA PS`
  )
  
}else if(os.ind == "snopud"){
  item127.os.table <- data.frame("BuildingType"        = item127.os.cast$BuildingType
                                 ,"Percent.Assistance" = item127.os.cast$Percent.Assistance
                                 ,"Percent_SnoPUD"          = item127.os.cast$`Percent_SnoPUD`
                                 ,"SE_SnoPUD"               = item127.os.cast$`SE_SnoPUD`
                                 ,"n_SnoPUD"                = item127.os.cast$`n_SnoPUD`
                                 ,"Percent_2017.RBSA.PS"    = item127.os.cast$`Percent_2017 RBSA PS`
                                 ,"SE_2017.RBSA.PS"         = item127.os.cast$`SE_2017 RBSA PS`
                                 ,"n_2017.RBSA.PS"          = item127.os.cast$`n_2017 RBSA PS`
                                 ,"Percent_RBSA.NW"         = item127.os.cast$`Percent_2017 RBSA NW`
                                 ,"SE_RBSA.NW"              = item127.os.cast$`SE_2017 RBSA NW`
                                 ,"n_RBSA.NW"               = item127.os.cast$`n_2017 RBSA NW`
  )
  
}


# row ordering example code
unique(item127.os.table$Percent.Assistance)
rowOrder <- c("Less than 25%"
              ,"Between 26% and 50%"
              ,"Between 51% and 75%"
              ,"Between 76% and 100%"
              ,"No Utility Bill Assistance"
              ,"Total")
item127.os.table <- item127.os.table %>% mutate(Percent.Assistance = factor(Percent.Assistance, levels = rowOrder)) %>% arrange(Percent.Assistance)  
item127.os.table <- data.frame(item127.os.table)

item127.os.final.SF <- item127.os.table[which(item127.os.table$BuildingType == "Single Family")
                                  ,-which(colnames(item127.os.table) %in% c("BuildingType"))]

exportTable(item127.os.final.SF, "SF", "Table 134", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
#TABLE AD: Percent of homes reporting having completed an energy audit in the last two years
#############################################################################################
#subset to columns needed for analysis
tableAD.os.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Has.your.household.completed.a.home.energy.audit.in.the.past.2.years?"))]
colnames(tableAD.os.dat) <- c("Energy.Audit", "CK_Cadmus_ID")

#merge together analysis data with cleaned scl data
tableAD.os.dat1 <- left_join(os.dat, tableAD.os.dat, by = "CK_Cadmus_ID")
tableAD.os.dat2 <- tableAD.os.dat1[which(tableAD.os.dat1$Energy.Audit %in% c("Yes","No")),]
unique(tableAD.os.dat2$Energy.Audit)

tableAD.os.dat2$Ind <- 0
tableAD.os.dat2$Ind[which(tableAD.os.dat2$Energy.Audit == "Yes")] <- 1

################################################
# Adding pop and sample sizes for weights
################################################
tableAD.os.data <- weightedData(tableAD.os.dat2[-which(colnames(tableAD.os.dat2) %in% c("Energy.Audit"
                                                                               ,"Ind"))])
tableAD.os.data <- left_join(tableAD.os.data, unique(tableAD.os.dat2[which(colnames(tableAD.os.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Energy.Audit"
                                                                                         ,"Ind"))]))
tableAD.os.data$count <- 1
tableAD.os.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableAD.os.final <- proportions_one_group(CustomerLevelData = tableAD.os.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "CK_Building_ID"
                                       ,total.name = "Remove")
tableAD.os.final <- tableAD.os.final[which(tableAD.os.final$CK_Building_ID %notin% c("Total", "Remove")),]

tableAD.os.final.SF <- tableAD.os.final[which(tableAD.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableAD.os.final) %in% c("BuildingType"))]

exportTable(tableAD.os.final.SF, "SF", "Table AD", weighted = TRUE, osIndicator = export.ind, OS = T)

#######################
# Unweighted Analysis
#######################
tableAD.os.final <- proportions_one_group(CustomerLevelData = tableAD.os.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "CK_Building_ID"
                                       ,total.name = "Remove"
                                       ,weighted = FALSE)
tableAD.os.final <- tableAD.os.final[which(tableAD.os.final$CK_Building_ID %notin% c("Total", "Remove")),]

tableAD.os.final.SF <- tableAD.os.final[which(tableAD.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(tableAD.os.final) %in% c("BuildingType"))]

exportTable(tableAD.os.final.SF, "SF", "Table AD", weighted = FALSE, osIndicator = export.ind, OS = T)