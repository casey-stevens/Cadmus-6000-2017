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
#Item 139: PERCENTAGE OF HOUSEHOLDS REPORTING RECENT SELF-FUNDED CONSERVATION BY STATE (SF table 146, MH table 121)
#############################################################################################
#subset to columns needed for analysis
item139.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_ConservationEffortsConversation_OnYourOwn_LastFewYears_Y_N"
                                                                                     ,""))])

colnames(item139.dat) <- c("CK_Cadmus_ID", "Reporting")
#remove any repeat header rows from exporting
item139.dat0 <- item139.dat[which(item139.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

unique(item139.dat0$Reporting)
item139.dat1   <- item139.dat0[which(item139.dat0$Reporting %in% c("Yes", "No")),]
item139.dat1$Ind <- 0
item139.dat1$Ind[which(item139.dat1$Reporting == "Yes")] <- 1

item139.sum <- summarise(group_by(item139.dat1, CK_Cadmus_ID)
                         ,Ind = sum(unique(Ind)))

item139.merge <- left_join(rbsa.dat, item139.sum)
item139.merge <- item139.merge[which(!is.na(item139.merge$Ind)),]



################################################
# Adding pop and sample sizes for weights
################################################
item139.data <- weightedData(item139.merge[-which(colnames(item139.merge) %in% c("Ind"))])
item139.data <- left_join(item139.data, item139.merge[which(colnames(item139.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])
item139.data$count <- 1
#######################
# Weighted Analysis
#######################
item139.final <- proportions_one_group(CustomerLevelData = item139.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = TRUE
                                       ,two.prop.total = NA)

item139.final.SF <- item139.final[which(item139.final$BuildingType == "Single Family")
                                  ,-which(colnames(item139.final) %in% c("BuildingType"))]
item139.final.MH <- item139.final[which(item139.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item139.final) %in% c("BuildingType"))]

exportTable(item139.final.SF, "SF", "Table 146", weighted = TRUE)
exportTable(item139.final.MH, "MH", "Table 121", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item139.final <- proportions_one_group(CustomerLevelData = item139.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = FALSE
                                       ,two.prop.total = NA)

item139.final.SF <- item139.final[which(item139.final$BuildingType == "Single Family")
                                  ,-which(colnames(item139.final) %in% c("BuildingType"
                                                                         ,"Remove"))]
item139.final.MH <- item139.final[which(item139.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item139.final) %in% c("BuildingType"
                                                                         ,"Remove"))]

exportTable(item139.final.SF, "SF", "Table 146", weighted = FALSE)
exportTable(item139.final.MH, "MH", "Table 121", weighted = FALSE)




#############################################################################################
#Item 140: PERCENTAGE OF HOUSEHOLDS REPORTING RECENT USE OF UTILITY CONSERVATION PROGRAMS BY STATE (SF table 147, MH table 122)
#############################################################################################
#subset to columns needed for analysis
item140.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
                                                            ,"Did.you.receive.a.rebate.or.incentive.from.your.utility.for.making.any.energy-efficiency.improvements.or.installing.energy-efficiency.equipment.in.the.last.two.years?"
                                                            ,""))]

colnames(item140.dat) <- c("Reporting", "CK_Cadmus_ID")

item140.dat1   <- item140.dat[which(item140.dat$Reporting %in% c("Yes", "No")),]
unique(item140.dat1$Reporting)

item140.dat1$Reporting.Count <- 0
item140.dat1$Reporting.Count[which(item140.dat1$Reporting == "Yes")] <- 1

item140.sum <- summarise(group_by(item140.dat1, CK_Cadmus_ID)
                         ,Ind = sum(unique(Reporting.Count)))

item140.merge <- left_join(rbsa.dat, item140.sum)
item140.merge <- item140.merge[which(!is.na(item140.merge$Ind)),]

################################################
# Adding pop and sample sizes for weights
################################################
item140.data <- weightedData(item140.merge[-which(colnames(item140.merge) %in% c("Ind"))])
item140.data <- left_join(item140.data, item140.merge[which(colnames(item140.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])
item140.data$count <- 1
#######################
# Weighted Analysis
#######################
item140.final <- proportions_one_group(CustomerLevelData = item140.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = TRUE
                                       ,two.prop.total = NA)

item140.final.SF <- item140.final[which(item140.final$BuildingType == "Single Family")
                                  ,-which(colnames(item140.final) %in% c("BuildingType"))]
item140.final.MH <- item140.final[which(item140.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item140.final) %in% c("BuildingType"))]

exportTable(item140.final.SF, "SF", "Table 147", weighted = TRUE)
exportTable(item140.final.MH, "MH", "Table 122", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item140.final <- proportions_one_group(CustomerLevelData = item140.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = FALSE
                                       ,two.prop.total = NA)

item140.final.SF <- item140.final[which(item140.final$BuildingType == "Single Family")
                                  ,-which(colnames(item140.final) %in% c("BuildingType"
                                                                         ,"Remove"))]
item140.final.MH <- item140.final[which(item140.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item140.final) %in% c("BuildingType"
                                                                         ,"Remove"))]

exportTable(item140.final.SF, "SF", "Table 147", weighted = FALSE)
exportTable(item140.final.MH, "MH", "Table 122", weighted = FALSE)







#############################################################################################
#Item 141: PERCENTAGE OF HOUSEHOLDS REPORTING USE OF CONSERVATION TAX CREDIT (SF table 148, MH table 123)
#############################################################################################
#subset to columns needed for analysis
item141.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_ConservationEffortsConversation_DidYouReceiveATaxCreditForThisWork_Y_N"
                                                                                     ,""))])

colnames(item141.dat) <- c("CK_Cadmus_ID", "Reporting.tax")
#remove any repeat header rows from exporting
item141.dat0 <- item141.dat[which(item141.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item141.dat1   <- item141.dat0[which(item141.dat0$Reporting.tax %in% c("Yes", "No")),]
item141.dat1$Reporting.tax.count <- 0
item141.dat1$Reporting.tax.count[which(item141.dat1$Reporting.tax == "Yes")] <- 1

item141.sum <- summarise(group_by(item141.dat1, CK_Cadmus_ID)
                         ,Ind = sum(unique(Reporting.tax.count)))

item141.merge <- left_join(rbsa.dat, item141.sum)
item141.merge <- item141.merge[which(!is.na(item141.merge$Ind)),]

################################################
# Adding pop and sample sizes for weights
################################################
item141.data <- weightedData(item141.merge[-which(colnames(item141.merge) %in% c("Ind"))])
item141.data <- left_join(item141.data, item141.merge[which(colnames(item141.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])
item141.data$count <- 1
#######################
# Weighted Analysis
#######################
item141.final <- proportions_one_group(CustomerLevelData = item141.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = TRUE
                                       ,two.prop.total = NA)

item141.final.SF <- item141.final[which(item141.final$BuildingType == "Single Family")
                                  ,-which(colnames(item141.final) %in% c("BuildingType"))]
item141.final.MH <- item141.final[which(item141.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item141.final) %in% c("BuildingType"))]

exportTable(item141.final.SF, "SF", "Table 148", weighted = TRUE)
exportTable(item141.final.MH, "MH", "Table 123", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item141.final <- proportions_one_group(CustomerLevelData = item141.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = FALSE
                                       ,two.prop.total = NA)

item141.final.SF <- item141.final[which(item141.final$BuildingType == "Single Family")
                                  ,-which(colnames(item141.final) %in% c("BuildingType"
                                                                         ,"Remove"))]
item141.final.MH <- item141.final[which(item141.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item141.final) %in% c("BuildingType"
                                                                         ,"Remove"))]

exportTable(item141.final.SF, "SF", "Table 148", weighted = FALSE)
exportTable(item141.final.MH, "MH", "Table 123", weighted = FALSE)






#############################################################################################
#Item 142: PERCENTAGE OF HOUSEHOLDS REPORTING USE OF BOTH UTILITY AND TAX CREDIT CONSERVATION PROGRAMS (SF table 149, MH table 124)
#############################################################################################
item141.tmp <- item141.dat1[which(colnames(item141.dat1) %in% c("CK_Cadmus_ID"
                                                                ,"Reporting.tax"
                                                                ,"Reporting.tax.count"))]
item142.dat <- left_join(item140.merge, item141.tmp, by = "CK_Cadmus_ID")

item142.dat$Total.Reporting.Count <- 0
item142.dat$Total.Reporting.Count[which(item142.dat$Ind == 1 & item142.dat$Reporting.tax.count == 1)] <- 1

unique(item142.dat$Total.Reporting.Count)

item142.sum <- summarise(group_by(item142.dat, CK_Cadmus_ID)
                         ,Ind = sum(unique(Total.Reporting.Count)))

item142.merge <- left_join(rbsa.dat, item142.sum)
item142.merge <- item142.merge[which(!is.na(item142.merge$Ind)),]


################################################
# Adding pop and sample sizes for weights
################################################
item142.data <- weightedData(item142.merge[-which(colnames(item142.merge) %in% c("Ind"))])
item142.data <- left_join(item142.data, item142.merge[which(colnames(item142.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])
item142.data$count <- 1
#######################
# Weighted Analysis
#######################
item142.final <- proportions_one_group(CustomerLevelData = item142.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = TRUE
                                       ,two.prop.total = NA)

item142.final.SF <- item142.final[which(item142.final$BuildingType == "Single Family")
                                  ,-which(colnames(item142.final) %in% c("BuildingType"))]
item142.final.MH <- item142.final[which(item142.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item142.final) %in% c("BuildingType"))]

exportTable(item142.final.SF, "SF", "Table 149", weighted = TRUE)
exportTable(item142.final.MH, "MH", "Table 124", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item142.final <- proportions_one_group(CustomerLevelData = item142.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = FALSE
                                       ,two.prop.total = NA)

item142.final.SF <- item142.final[which(item142.final$BuildingType == "Single Family")
                                  ,-which(colnames(item142.final) %in% c("BuildingType"
                                                                         ,"Remove"))]
item142.final.MH <- item142.final[which(item142.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item142.final) %in% c("BuildingType"
                                                                         ,"Remove"))]

exportTable(item142.final.SF, "SF", "Table 149", weighted = FALSE)
exportTable(item142.final.MH, "MH", "Table 124", weighted = FALSE)
