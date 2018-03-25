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
# sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))

#Read in data for analysis
# survey.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Labeled and Translated")
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

# #######################
# # MULTIFAMILY
# #######################
# tableAD.final.MF <- proportions_one_group(CustomerLevelData = tableAD.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "HomeType"
#                                        ,total.name = "All Types")
# tableAD.final.MF <- tableAD.final.MF[which(tableAD.final.MF$BuildingType == "Multifamily")
#                                   ,-which(colnames(tableAD.final.MF) %in% c("BuildingType"))]
# # exportTable(tableAD.final.MF, "MF", "Table AD", weighted = TRUE)





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

# #######################
# # MULTIFAMILY
# #######################
# tableAD.final.MF <- proportions_one_group(CustomerLevelData = tableAD.data
#                                           ,valueVariable = "Ind"
#                                           ,groupingVariable = "HomeType"
#                                           ,total.name = "All Types"
#                                           ,weighted = FALSE)
# tableAD.final.MF <- tableAD.final.MF[which(tableAD.final.MF$BuildingType == "Multifamily")
#                                      ,-which(colnames(tableAD.final.MF) %in% c("BuildingType"))]
# # exportTable(tableAD.final.MF, "MF", "Table AD", weighted = FALSE)


