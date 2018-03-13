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
item139.data$Count <- 1
#######################
# Weighted Analysis
#######################
item139.final <- proportions_one_group(CustomerLevelData = item139.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = TRUE)

item139.final.SF <- item139.final[which(item139.final$BuildingType == "Single Family")
                                  ,-which(colnames(item139.final) %in% c("BuildingType"))]
item139.final.MH <- item139.final[which(item139.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item139.final) %in% c("BuildingType"))]

exportTable(item139.final.SF, "SF", "Table 146", weighted = TRUE)
# exportTable(item139.final.MH, "MH", "Table 121", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item139.final <- proportions_one_group(CustomerLevelData = item139.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = FALSE)

item139.final.SF <- item139.final[which(item139.final$BuildingType == "Single Family")
                                  ,-which(colnames(item139.final) %in% c("BuildingType"))]
item139.final.MH <- item139.final[which(item139.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item139.final) %in% c("BuildingType"))]

exportTable(item139.final.SF, "SF", "Table 146", weighted = FALSE)
# exportTable(item139.final.MH, "MH", "Table 121", weighted = FALSE)




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
item140.data$Count <- 1
#######################
# Weighted Analysis
#######################
item140.final <- proportions_one_group(CustomerLevelData = item140.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = TRUE)

item140.final.SF <- item140.final[which(item140.final$BuildingType == "Single Family")
                                  ,-which(colnames(item140.final) %in% c("BuildingType"))]
item140.final.MH <- item140.final[which(item140.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item140.final) %in% c("BuildingType"))]

exportTable(item140.final.SF, "SF", "Table 147", weighted = TRUE)
# exportTable(item140.final.MH, "MH", "Table 122", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item140.final <- proportions_one_group(CustomerLevelData = item140.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = FALSE)

item140.final.SF <- item140.final[which(item140.final$BuildingType == "Single Family")
                                  ,-which(colnames(item140.final) %in% c("BuildingType"))]
item140.final.MH <- item140.final[which(item140.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item140.final) %in% c("BuildingType"))]

exportTable(item140.final.SF, "SF", "Table 147", weighted = FALSE)
# exportTable(item140.final.MH, "MH", "Table 122", weighted = FALSE)







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
item141.data$Count <- 1
#######################
# Weighted Analysis
#######################
item141.final <- proportions_one_group(CustomerLevelData = item141.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = TRUE)

item141.final.SF <- item141.final[which(item141.final$BuildingType == "Single Family")
                                  ,-which(colnames(item141.final) %in% c("BuildingType"))]
item141.final.MH <- item141.final[which(item141.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item141.final) %in% c("BuildingType"))]

exportTable(item141.final.SF, "SF", "Table 148", weighted = TRUE)
# exportTable(item141.final.MH, "MH", "Table 123", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item141.final <- proportions_one_group(CustomerLevelData = item141.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = FALSE)

item141.final.SF <- item141.final[which(item141.final$BuildingType == "Single Family")
                                  ,-which(colnames(item141.final) %in% c("BuildingType"))]
item141.final.MH <- item141.final[which(item141.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item141.final) %in% c("BuildingType"))]

exportTable(item141.final.SF, "SF", "TableC 148", weighted = FALSE)
# exportTable(item141.final.MH, "MH", "Table 123", weighted = FALSE)






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
item142.data$Count <- 1
#######################
# Weighted Analysis
#######################
item142.final <- proportions_one_group(CustomerLevelData = item142.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = TRUE)

item142.final.SF <- item142.final[which(item142.final$BuildingType == "Single Family")
                                  ,-which(colnames(item142.final) %in% c("BuildingType"))]
item142.final.MH <- item142.final[which(item142.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item142.final) %in% c("BuildingType"))]

exportTable(item142.final.SF, "SF", "Table 149", weighted = TRUE)
# exportTable(item142.final.MH, "MH", "Table 124", weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item142.final <- proportions_one_group(CustomerLevelData = item142.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name = "Region"
                                       ,weighted = FALSE)

item142.final.SF <- item142.final[which(item142.final$BuildingType == "Single Family")
                                  ,-which(colnames(item142.final) %in% c("BuildingType"))]
item142.final.MH <- item142.final[which(item142.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item142.final) %in% c("BuildingType"))]

exportTable(item142.final.SF, "SF", "Table 149", weighted = FALSE)
# exportTable(item142.final.MH, "MH", "Table 124", weighted = FALSE)






























# ############################################################################################################
# #
# #
# # OVERSAMPLE ANALYSIS
# #
# #
# ############################################################################################################
# 
# # Read in clean os data
# os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
# length(unique(os.dat$CK_Cadmus_ID))
# os.dat$CK_Building_ID <- os.dat$Category
# os.dat <- os.dat[which(names(os.dat) != "Category")]
# 
# #############################################################################################
# #Item 139: PERCENTAGE OF HOUSEHOLDS REPORTING RECENT SELF-FUNDED CONSERVATION BY CK_Building_ID (SF table 146, MH table 121)
# #############################################################################################
# #subset to columns needed for analysis
# item139.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
#                                                                                      ,"INTRVW_CUST_RES_ConservationEffortsConversation_OnYourOwn_LastFewYears_Y_N"
#                                                                                      ,""))])
# 
# colnames(item139.os.dat) <- c("CK_Cadmus_ID", "Reporting")
# #remove any repeat header rows from exporting
# item139.os.dat0 <- item139.os.dat[which(item139.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# unique(item139.os.dat0$Reporting)
# item139.os.dat1   <- item139.os.dat0[which(item139.os.dat0$Reporting %in% c("Yes", "No")),]
# item139.os.dat1$Ind <- 0
# item139.os.dat1$Ind[which(item139.os.dat1$Reporting == "Yes")] <- 1
# 
# item139.os.sum <- summarise(group_by(item139.os.dat1, CK_Cadmus_ID)
#                          ,Ind = sum(unique(Ind)))
# 
# item139.os.merge <- left_join(os.dat, item139.os.sum)
# item139.os.merge <- item139.os.merge[which(!is.na(item139.os.merge$Ind)),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item139.os.data <- weightedData(item139.os.merge[-which(colnames(item139.os.merge) %in% c("Ind"))])
# item139.os.data <- left_join(item139.os.data, unique(item139.os.merge[which(colnames(item139.os.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Ind"))]))
# item139.os.data$Count <- 1
# #######################
# # Weighted Analysis
# #######################
# item139.os.final <- proportions_one_group(CustomerLevelData = item139.os.data
#                                        ,valueVariable = 'Ind'
#                                        ,groupingVariable = 'CK_Building_ID'
#                                        ,total.name = "Remove"
#                                        ,weighted = TRUE)
# item139.os.final <- item139.os.final[which(item139.os.final$CK_Building_ID %notin% c("Remove", "Total")),]
# 
# item139.os.final.SF <- item139.os.final[which(item139.os.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item139.os.final) %in% c("BuildingType"))]
# 
# exportTable(item139.os.final.SF, "SF", "Table 146", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# #######################
# # Unweighted Analysis
# #######################
# item139.os.final <- proportions_one_group(CustomerLevelData = item139.os.data
#                                        ,valueVariable = 'Ind'
#                                        ,groupingVariable = 'CK_Building_ID'
#                                        ,total.name = "Remove"
#                                        ,weighted = FALSE)
# item139.os.final <- item139.os.final[which(item139.os.final$CK_Building_ID %notin% c("Remove", "Total")),]
# 
# item139.os.final.SF <- item139.os.final[which(item139.os.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item139.os.final) %in% c("BuildingType"))]
# 
# exportTable(item139.os.final.SF, "SF", "Table 146", weighted = FALSE, osIndicator = export.ind, OS = T)
# 
# 
# 
# 
# #############################################################################################
# #Item 140: PERCENTAGE OF HOUSEHOLDS REPORTING RECENT USE OF UTILITY CONSERVATION PROGRAMS BY CK_Building_ID (SF table 147, MH table 122)
# #############################################################################################
# #subset to columns needed for analysis
# item140.os.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID"
#                                                             ,"Did.you.receive.a.rebate.or.incentive.from.your.utility.for.making.any.energy-efficiency.improvements.or.installing.energy-efficiency.equipment.in.the.last.two.years?"
#                                                             ,""))]
# 
# colnames(item140.os.dat) <- c("Reporting", "CK_Cadmus_ID")
# 
# item140.os.dat1   <- item140.os.dat[which(item140.os.dat$Reporting %in% c("Yes", "No")),]
# unique(item140.os.dat1$Reporting)
# 
# item140.os.dat1$Reporting.Count <- 0
# item140.os.dat1$Reporting.Count[which(item140.os.dat1$Reporting == "Yes")] <- 1
# 
# item140.os.sum <- summarise(group_by(item140.os.dat1, CK_Cadmus_ID)
#                          ,Ind = sum(unique(Reporting.Count)))
# 
# item140.os.merge <- left_join(os.dat, item140.os.sum)
# item140.os.merge <- item140.os.merge[which(!is.na(item140.os.merge$Ind)),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item140.os.data <- weightedData(item140.os.merge[-which(colnames(item140.os.merge) %in% c("Ind"))])
# item140.os.data <- left_join(item140.os.data, unique(item140.os.merge[which(colnames(item140.os.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Ind"))]))
# item140.os.data$Count <- 1
# #######################
# # Weighted Analysis
# #######################
# item140.os.final <- proportions_one_group(CustomerLevelData = item140.os.data
#                                        ,valueVariable = 'Ind'
#                                        ,groupingVariable = 'CK_Building_ID'
#                                        ,total.name = "Remove"
#                                        ,weighted = TRUE)
# item140.os.final <- item140.os.final[which(item140.os.final$CK_Building_ID %notin% c("Remove", "Total")),]
# 
# item140.os.final.SF <- item140.os.final[which(item140.os.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item140.os.final) %in% c("BuildingType"))]
# 
# exportTable(item140.os.final.SF, "SF", "Table 147", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# #######################
# # Unweighted Analysis
# #######################
# item140.os.final <- proportions_one_group(CustomerLevelData = item140.os.data
#                                        ,valueVariable = 'Ind'
#                                        ,groupingVariable = 'CK_Building_ID'
#                                        ,total.name = "Remove"
#                                        ,weighted = FALSE)
# item140.os.final <- item140.os.final[which(item140.os.final$CK_Building_ID %notin% c("Remove", "Total")),]
# 
# item140.os.final.SF <- item140.os.final[which(item140.os.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item140.os.final) %in% c("BuildingType"))]
# 
# exportTable(item140.os.final.SF, "SF", "Table 147", weighted = FALSE, osIndicator = export.ind, OS = T)
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 141: PERCENTAGE OF HOUSEHOLDS REPORTING USE OF CONSERVATION TAX CREDIT (SF table 148, MH table 123)
# #############################################################################################
# #subset to columns needed for analysis
# item141.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
#                                                                                      ,"INTRVW_CUST_RES_ConservationEffortsConversation_DidYouReceiveATaxCreditForThisWork_Y_N"
#                                                                                      ,""))])
# 
# colnames(item141.os.dat) <- c("CK_Cadmus_ID", "Reporting.tax")
# #remove any repeat header rows from exporting
# item141.os.dat0 <- item141.os.dat[which(item141.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item141.os.dat1   <- item141.os.dat0[which(item141.os.dat0$Reporting.tax %in% c("Yes", "No")),]
# item141.os.dat1$Reporting.tax.count <- 0
# item141.os.dat1$Reporting.tax.count[which(item141.os.dat1$Reporting.tax == "Yes")] <- 1
# 
# item141.os.sum <- summarise(group_by(item141.os.dat1, CK_Cadmus_ID)
#                          ,Ind = sum(unique(Reporting.tax.count)))
# 
# item141.os.merge <- left_join(os.dat, item141.os.sum)
# item141.os.merge <- item141.os.merge[which(!is.na(item141.os.merge$Ind)),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item141.os.data <- weightedData(item141.os.merge[-which(colnames(item141.os.merge) %in% c("Ind"))])
# item141.os.data <- left_join(item141.os.data, unique(item141.os.merge[which(colnames(item141.os.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Ind"))]))
# item141.os.data$Count <- 1
# #######################
# # Weighted Analysis
# #######################
# item141.os.final <- proportions_one_group(CustomerLevelData = item141.os.data
#                                        ,valueVariable = 'Ind'
#                                        ,groupingVariable = 'CK_Building_ID'
#                                        ,total.name = "Remove"
#                                        ,weighted = TRUE)
# item141.os.final <- item141.os.final[which(item141.os.final$CK_Building_ID %notin% c("Remove", "Total")),]
# 
# item141.os.final.SF <- item141.os.final[which(item141.os.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item141.os.final) %in% c("BuildingType"))]
# 
# exportTable(item141.os.final.SF, "SF", "Table 148", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# #######################
# # Unweighted Analysis
# #######################
# item141.os.final <- proportions_one_group(CustomerLevelData = item141.os.data
#                                        ,valueVariable = 'Ind'
#                                        ,groupingVariable = 'CK_Building_ID'
#                                        ,total.name = "Remove"
#                                        ,weighted = FALSE)
# item141.os.final <- item141.os.final[which(item141.os.final$CK_Building_ID %notin% c("Remove", "Total")),]
# 
# item141.os.final.SF <- item141.os.final[which(item141.os.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item141.os.final) %in% c("BuildingType"))]
# 
# exportTable(item141.os.final.SF, "SF", "Table 148", weighted = FALSE, osIndicator = export.ind, OS = T)
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 142: PERCENTAGE OF HOUSEHOLDS REPORTING USE OF BOTH UTILITY AND TAX CREDIT CONSERVATION PROGRAMS (SF table 149, MH table 124)
# #############################################################################################
# item141.os.tmp <- item141.os.dat1[which(colnames(item141.os.dat1) %in% c("CK_Cadmus_ID"
#                                                                 ,"Reporting.tax"
#                                                                 ,"Reporting.tax.count"))]
# item142.os.dat <- left_join(item140.os.merge, item141.os.tmp, by = "CK_Cadmus_ID")
# 
# item142.os.dat$Total.Reporting.Count <- 0
# item142.os.dat$Total.Reporting.Count[which(item142.os.dat$Ind == 1 & item142.os.dat$Reporting.tax.count == 1)] <- 1
# 
# unique(item142.os.dat$Total.Reporting.Count)
# 
# item142.os.sum <- summarise(group_by(item142.os.dat, CK_Cadmus_ID)
#                          ,Ind = sum(unique(Total.Reporting.Count)))
# 
# item142.os.merge <- left_join(os.dat, item142.os.sum)
# item142.os.merge <- item142.os.merge[which(!is.na(item142.os.merge$Ind)),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item142.os.data <- weightedData(item142.os.merge[-which(colnames(item142.os.merge) %in% c("Ind"))])
# item142.os.data <- left_join(item142.os.data, unique(item142.os.merge[which(colnames(item142.os.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Ind"))]))
# item142.os.data$Count <- 1
# #######################
# # Weighted Analysis
# #######################
# item142.os.final <- proportions_one_group(CustomerLevelData = item142.os.data
#                                        ,valueVariable = 'Ind'
#                                        ,groupingVariable = 'CK_Building_ID'
#                                        ,total.name = "Remove"
#                                        ,weighted = TRUE)
# item142.os.final <- item142.os.final[which(item142.os.final$CK_Building_ID %notin% c("Remove", "Total")),]
# 
# item142.os.final.SF <- item142.os.final[which(item142.os.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item142.os.final) %in% c("BuildingType"))]
# 
# exportTable(item142.os.final.SF, "SF", "Table 149", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# #######################
# # Unweighted Analysis
# #######################
# item142.os.final <- proportions_one_group(CustomerLevelData = item142.os.data
#                                        ,valueVariable = 'Ind'
#                                        ,groupingVariable = 'CK_Building_ID'
#                                        ,total.name = "Remove"
#                                        ,weighted = FALSE)
# item142.os.final <- item142.os.final[which(item142.os.final$CK_Building_ID %notin% c("Remove", "Total")),]
# 
# item142.os.final.SF <- item142.os.final[which(item142.os.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item142.os.final) %in% c("BuildingType"))]
# 
# exportTable(item142.os.final.SF, "SF", "Table 149", weighted = FALSE, osIndicator = export.ind, OS = T)
