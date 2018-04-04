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

#Read in data for analysis
# appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))


# #############################################################################################
# #Item 82: DISTRIBUTION OF REFRIGERATORS BY TYPE (SF table 89, MH table 70)
# #############################################################################################
# #subset to columns needed for analysis
# item82.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
#                                                                    ,"Type"
#                                                                    ,"APPLIANCE_FRIDGE_FREEZER_Type"
#                                                                    ,"Refrigerator/Freezer.Size"))]
# names(item82.dat)
# 
# item82.dat0 <- item82.dat[which(item82.dat$APPLIANCE_FRIDGE_FREEZER_Type %notin% c("N/A",NA,"Unknown")),]
# item82.dat1 <- left_join(rbsa.dat, item82.dat0)
# 
# #clean type to match detailed type
# item82.dat1$Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Freezer, chest")] <- "Freezer"
# item82.dat1$Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Mini-Freezer")] <- "Freezer"
# item82.dat1$Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Freezer, upright")] <- "Freezer"
# 
# #clean detailed type to match previous RBSA table
# unique(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type)
# item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "R/F Top Freezer")] <- "Refrigerator with Top Freezer"
# item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "R/F Bottom Freezer")] <- "Refrigerator with Bottom Freezer"
# item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Side by Side w/ Bottom Freezer")] <- "Side-by-Side Refrigerator with Bottom Freezer"
# item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Side by Side Refrigerator/Freezer")] <- "Refrigerator with Side-by-Side Freezer"
# item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Full Size Single Refrigerator Only")] <- "Full Size Refrigerator Only"
# item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Mini-Fridge")] <- "Mini Refrigerator"
# 
# item82.dat2 <- item82.dat1
# 
# item82.dat3 <- item82.dat2[which(item82.dat2$Type == "Refrigerator"),]
# 
# ######################################
# #Pop and Sample Sizes for weights
# ######################################
# item82.data <- weightedData(item82.dat3[which(colnames(item82.dat3) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
#                                                                               ,"Type"
#                                                                               ,"Refrigerator/Freezer.Size"))])
# 
# item82.data <- left_join(item82.data, item82.dat3[which(colnames(item82.dat3) %in% c("CK_Cadmus_ID"
#                                                                                      ,"APPLIANCE_FRIDGE_FREEZER_Type"
#                                                                                      ,"Type"
#                                                                                      ,"Refrigerator/Freezer.Size"))])
# item82.data$count <- 1
# 
# 
# ######################
# # weighted analysis
# ######################
# item82.final <- proportions_one_group(CustomerLevelData = item82.data
#                                       ,valueVariable = 'count'
#                                       ,groupingVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
#                                       ,total.name = 'Total')
# 
# unique(item82.final$APPLIANCE_FRIDGE_FREEZER_Type)
# rowOrder <- c("Full Size Refrigerator Only"
#               ,"Mini Refrigerator"
#               ,"Refrigerated Beer Cooler"
#               ,"Refrigerator with Bottom Freezer"
#               ,"Refrigerator with Side-by-Side Freezer"
#               ,"Refrigerator with Top Freezer"
#               ,"Refrigerated Wine Cooler"
#               ,"Side-by-Side Refrigerator with Bottom Freezer"
#               ,"Total")
# item82.final <- item82.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)  
# item82.final <- data.frame(item82.final)
# 
# 
# item82.final.SF <- item82.final[which(item82.final$BuildingType == "Single Family")
#                                 ,which(colnames(item82.final) %notin% c("BuildingType"))]
# item82.final.MH <- item82.final[which(item82.final$BuildingType == "Manufactured")
#                                 ,which(colnames(item82.final) %notin% c("BuildingType"))]
# 
# exportTable(item82.final.SF, "SF", "Table 89", weighted = TRUE)
# # exportTable(item82.final.MH, "MH", "Table 70", weighted = TRUE)
# 
# ######################
# # unweighted analysis
# ######################
# item82.final <- proportions_one_group(CustomerLevelData = item82.data
#                                       ,valueVariable = 'count'
#                                       ,groupingVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
#                                       ,total.name = 'Total'
#                                       ,weighted = FALSE)
# 
# 
# unique(item82.final$APPLIANCE_FRIDGE_FREEZER_Type)
# rowOrder <- c("Full Size Refrigerator Only"
#               ,"Mini Refrigerator"
#               ,"Refrigerated Beer Cooler"
#               ,"Refrigerator with Bottom Freezer"
#               ,"Refrigerator with Side-by-Side Freezer"
#               ,"Refrigerator with Top Freezer"
#               ,"Refrigerated Wine Cooler"
#               ,"Side-by-Side Refrigerator with Bottom Freezer"
#               ,"Total")
# item82.final <- item82.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)  
# item82.final <- data.frame(item82.final)
# 
# 
# item82.final.SF <- item82.final[which(item82.final$BuildingType == "Single Family")
#                                 ,which(colnames(item82.final) %notin% c("BuildingType"))]
# item82.final.MH <- item82.final[which(item82.final$BuildingType == "Manufactured")
#                                 ,which(colnames(item82.final) %notin% c("BuildingType"))]
# 
# exportTable(item82.final.SF, "SF", "Table 89", weighted = FALSE)
# # exportTable(item82.final.MH, "MH", "Table 70", weighted = FALSE)
# 
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 83: AVERAGE REFRIGERATOR VOLUME BY TYPE (SF table 90, MH table 71)
# #############################################################################################
# #subset to columns needed for analysis
# item83.dat <- item82.dat3[-grep("unknown",item82.dat3$`Refrigerator/Freezer.Size`, ignore.case = T),]
# 
# names(item83.dat)
# ######################################
# #Pop and Sample Sizes for weights
# ######################################
# item83.data <- weightedData(item83.dat[which(colnames(item83.dat) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
#                                                                             ,"Type"
#                                                                             ,"Refrigerator/Freezer.Size"))])
# 
# item83.data <- left_join(item83.data, item83.dat[which(colnames(item83.dat) %in% c("CK_Cadmus_ID"
#                                                                                    ,"APPLIANCE_FRIDGE_FREEZER_Type"
#                                                                                    ,"Type"
#                                                                                    ,"Refrigerator/Freezer.Size"))])
# item83.data$count <- 1
# 
# item83.data$`Refrigerator/Freezer.Size` <- gsub(" ", "", item83.data$`Refrigerator/Freezer.Size`)
# item83.data$`Refrigerator/Freezer.Size` <- gsub("18..61", "18.61", item83.data$`Refrigerator/Freezer.Size`)
# item83.data$`Refrigerator/Freezer.Size` <- gsub("19..1", "19.1", item83.data$`Refrigerator/Freezer.Size`)
# unique(item83.data$`Refrigerator/Freezer.Size`)
# item83.data$`Refrigerator/Freezer.Size` <- as.numeric(as.character(item83.data$`Refrigerator/Freezer.Size`))
# 
# ######################
# # weighted analysis
# ######################
# item83.final <- mean_one_group(CustomerLevelData = item83.data
#                                ,valueVariable = 'Refrigerator/Freezer.Size'
#                                ,byVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
#                                ,aggregateRow = "All Refrigerator Types")
# 
# 
# unique(item83.final$APPLIANCE_FRIDGE_FREEZER_Type)
# rowOrder <- c("Full Size Refrigerator Only"
#               ,"Mini Refrigerator"
#               ,"Refrigerated Beer Cooler"
#               ,"Refrigerator with Bottom Freezer"
#               ,"Refrigerator with Side-by-Side Freezer"
#               ,"Refrigerator with Top Freezer"
#               ,"Refrigerated Wine Cooler"
#               ,"Side-by-Side Refrigerator with Bottom Freezer"
#               ,"All Refrigerator Types")
# item83.final <- item83.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)  
# item83.final <- data.frame(item83.final)
# 
# 
# item83.final.SF <- item83.final[which(item83.final$BuildingType == "Single Family")
#                                 ,which(colnames(item83.final) %notin% c("BuildingType"))]
# item83.final.MH <- item83.final[which(item83.final$BuildingType == "Manufactured")
#                                 ,which(colnames(item83.final) %notin% c("BuildingType"))]
# 
# exportTable(item83.final.SF, "SF", "Table 90", weighted = TRUE)
# # exportTable(item83.final.MH, "MH", "Table 71", weighted = TRUE)
# 
# ######################
# # unweighted analysis
# ######################
# item83.final <- mean_one_group_unweighted(CustomerLevelData = item83.data
#                                           ,valueVariable = 'Refrigerator/Freezer.Size'
#                                           ,byVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
#                                           ,aggregateRow = "All Refrigerator Types")
# 
# 
# unique(item83.final$APPLIANCE_FRIDGE_FREEZER_Type)
# rowOrder <- c("Full Size Refrigerator Only"
#               ,"Mini Refrigerator"
#               ,"Refrigerated Beer Cooler"
#               ,"Refrigerator with Bottom Freezer"
#               ,"Refrigerator with Side-by-Side Freezer"
#               ,"Refrigerator with Top Freezer"
#               ,"Refrigerated Wine Cooler"
#               ,"Side-by-Side Refrigerator with Bottom Freezer"
#               ,"All Refrigerator Types")
# item83.final <- item83.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)  
# item83.final <- data.frame(item83.final)
# 
# item83.final.SF <- item83.final[which(item83.final$BuildingType == "Single Family")
#                                 ,which(colnames(item83.final) %notin% c("BuildingType"))]
# item83.final.MH <- item83.final[which(item83.final$BuildingType == "Manufactured")
#                                 ,which(colnames(item83.final) %notin% c("BuildingType"))]
# 
# exportTable(item83.final.SF, "SF", "Table 90", weighted = FALSE)
# # exportTable(item83.final.MH, "MH", "Table 71", weighted = FALSE)
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 84: DISTRIBUTION OF FREEZERS BY TYPE IN HOMES (SF table 91, MH table 72)
# #############################################################################################
# item84.dat <- item82.dat2[which(item82.dat2$Type == "Freezer"),]
# 
# ######################################
# #Pop and Sample Sizes for weights
# ######################################
# item84.data <- weightedData(item84.dat[which(colnames(item84.dat) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
#                                                                               ,"Type"
#                                                                               ,"Refrigerator/Freezer.Size"))])
# 
# item84.data <- left_join(item84.data, item84.dat[which(colnames(item84.dat) %in% c("CK_Cadmus_ID"
#                                                                                      ,"APPLIANCE_FRIDGE_FREEZER_Type"
#                                                                                      ,"Type"
#                                                                                      ,"Refrigerator/Freezer.Size"))])
# item84.data$count <- 1
# 
# 
# ######################
# # weighted analysis
# ######################
# item84.final <- proportions_one_group(CustomerLevelData = item84.data
#                                       ,valueVariable = 'count'
#                                       ,groupingVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
#                                       ,total.name = 'Total')
# 
# item84.final.SF <- item84.final[which(item84.final$BuildingType == "Single Family")
#                                 ,which(colnames(item84.final) %notin% c("BuildingType"))]
# item84.final.MH <- item84.final[which(item84.final$BuildingType == "Manufactured")
#                                 ,which(colnames(item84.final) %notin% c("BuildingType"))]
# 
# exportTable(item84.final.SF, "SF", "Table 91", weighted = TRUE)
# # exportTable(item84.final.MH, "MH", "Table 72", weighted = TRUE)
# 
# 
# ######################
# # unweighted analysis
# ######################
# item84.final <- proportions_one_group(CustomerLevelData = item84.data
#                                       ,valueVariable = 'count'
#                                       ,groupingVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
#                                       ,total.name = 'Total'
#                                       ,weighted = FALSE)
# 
# item84.final.SF <- item84.final[which(item84.final$BuildingType == "Single Family")
#                                 ,which(colnames(item84.final) %notin% c("BuildingType"))]
# item84.final.MH <- item84.final[which(item84.final$BuildingType == "Manufactured")
#                                 ,which(colnames(item84.final) %notin% c("BuildingType"))]
# 
# exportTable(item84.final.SF, "SF", "Table 91", weighted = FALSE)
# # exportTable(item84.final.MH, "MH", "Table 72", weighted = FALSE)
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 85: AVERAGE FREEZER VOLUME BY TYPE (SF table 92, MH table 73)
# #############################################################################################
# item85.dat <- item82.dat2[which(item82.dat2$Type == "Freezer"),]
# item85.dat1 <- item85.dat[-grep("unknown",item85.dat$`Refrigerator/Freezer.Size`, ignore.case = T),]
# 
# ######################################
# #Pop and Sample Sizes for weights
# ######################################
# item85.data <- weightedData(item85.dat1[which(colnames(item85.dat1) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
#                                                                               ,"Type"
#                                                                               ,"Refrigerator/Freezer.Size"))])
# 
# item85.data <- left_join(item85.data, item85.dat1[which(colnames(item85.dat1) %in% c("CK_Cadmus_ID"
#                                                                                      ,"APPLIANCE_FRIDGE_FREEZER_Type"
#                                                                                      ,"Type"
#                                                                                      ,"Refrigerator/Freezer.Size"))])
# item85.data$count <- 1
# 
# item85.data$`Refrigerator/Freezer.Size` <- gsub(" cu ft", "", item85.data$`Refrigerator/Freezer.Size`)
# item85.data$`Refrigerator/Freezer.Size` <- gsub("cu ft", "", item85.data$`Refrigerator/Freezer.Size`)
# item85.data$`Refrigerator/Freezer.Size` <- gsub(" ", "", item85.data$`Refrigerator/Freezer.Size`)
# unique(item85.data$`Refrigerator/Freezer.Size`)
# item85.data$`Refrigerator/Freezer.Size` <- as.numeric(as.character(item85.data$`Refrigerator/Freezer.Size`))
# 
# ######################
# # weighted analysis
# ######################
# item85.final <- mean_one_group(CustomerLevelData = item85.data
#                                ,valueVariable = 'Refrigerator/Freezer.Size'
#                                ,byVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
#                                ,aggregateRow = "All Refrigerator Types")
# 
# item85.final.SF <- item85.final[which(item85.final$BuildingType == "Single Family")
#                                 ,which(colnames(item85.final) %notin% c("BuildingType"))]
# item85.final.MH <- item85.final[which(item85.final$BuildingType == "Manufactured")
#                                 ,which(colnames(item85.final) %notin% c("BuildingType"))]
# 
# exportTable(item85.final.SF, "SF", "Table 92", weighted = TRUE)
# # exportTable(item85.final.MH, "MH", "Table 73", weighted = TRUE)
# 
# ######################
# # unweighted analysis
# ######################
# item85.final <- mean_one_group_unweighted(CustomerLevelData = item85.data
#                                           ,valueVariable = 'Refrigerator/Freezer.Size'
#                                           ,byVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
#                                           ,aggregateRow = "All Refrigerator Types")
# 
# item85.final.SF <- item85.final[which(item85.final$BuildingType == "Single Family")
#                                 ,which(colnames(item85.final) %notin% c("BuildingType"))]
# item85.final.MH <- item85.final[which(item85.final$BuildingType == "Manufactured")
#                                 ,which(colnames(item85.final) %notin% c("BuildingType"))]
# 
# exportTable(item85.final.SF, "SF", "Table 92", weighted = FALSE)
# # exportTable(item85.final.MH, "MH", "Table 73", weighted = FALSE)

































############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]
names(os.dat)

#############################################################################################
#Item 82: DISTRIBUTION OF REFRIGERATORS BY TYPE (SF table 89, MH table 70)
#############################################################################################
#subset to columns needed for analysis
item82.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                   ,"Refrigerator/Freezer.Size"))]
names(item82.os.dat)

item82.os.dat0 <- item82.os.dat[which(item82.os.dat$APPLIANCE_FRIDGE_FREEZER_Type %notin% c("N/A",NA,"Unknown")),]
item82.os.dat1 <- left_join(os.dat, item82.os.dat0)

#clean type to match detailed type
item82.os.dat1$Type[which(item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Freezer, chest")] <- "Freezer"
item82.os.dat1$Type[which(item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Mini-Freezer")] <- "Freezer"
item82.os.dat1$Type[which(item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Freezer, upright")] <- "Freezer"

#clean detailed type to match previous scl table
unique(item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type)
item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "R/F Top Freezer")] <- "Refrigerator with Top Freezer"
item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "R/F Bottom Freezer")] <- "Refrigerator with Bottom Freezer"
item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Side by Side w/ Bottom Freezer")] <- "Side-by-Side Refrigerator with Bottom Freezer"
item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Side by Side Refrigerator/Freezer")] <- "Refrigerator with Side-by-Side Freezer"
item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Full Size Single Refrigerator Only")] <- "Full Size Refrigerator Only"
item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.os.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Mini-Fridge")] <- "Mini Refrigerator"

item82.os.dat2 <- item82.os.dat1

item82.os.dat3 <- item82.os.dat2[which(item82.os.dat2$Type == "Refrigerator"),]

######################################
#Pop and Sample Sizes for weights
######################################
item82.os.data <- weightedData(item82.os.dat3[which(colnames(item82.os.dat3) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
                                                                              ,"Type"
                                                                              ,"Refrigerator/Freezer.Size"))])

item82.os.data <- left_join(item82.os.data, unique(item82.os.dat3[which(colnames(item82.os.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                                     ,"Type"
                                                                                     ,"Refrigerator/Freezer.Size"))]))
item82.os.data$count <- 1


######################
# weighted analysis
######################
item82.os.final <- proportionRowsAndColumns1(CustomerLevelData = item82.os.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = "APPLIANCE_FRIDGE_FREEZER_Type"
                                             ,aggregateColumnName = "Remove")
item82.os.final <- item82.os.final[which(item82.os.final$CK_Building_ID != "Remove"),]

item82.os.cast <- dcast(setDT(item82.os.final)
                        ,formula = BuildingType + APPLIANCE_FRIDGE_FREEZER_Type ~ CK_Building_ID
                        ,value.var = c("w.percent", "w.SE","n", "EB"))

names(item82.os.cast)
if(os.ind == "scl"){
  item82.os.final <- data.frame("BuildingType"          = item82.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"    = item82.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Percent_SCL.GenPop"   = item82.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item82.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item82.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item82.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item82.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item82.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item82.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item82.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item82.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item82.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item82.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item82.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item82.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item82.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item82.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item82.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item82.os.final <- data.frame("BuildingType"          = item82.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"    = item82.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Percent_SnoPUD"          = item82.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item82.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item82.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item82.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item82.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item82.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item82.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item82.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item82.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item82.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item82.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item82.os.cast$`EB_2017 RBSA NW`)
}





unique(item82.os.final$APPLIANCE_FRIDGE_FREEZER_Type)
rowOrder <- c("Full Size Refrigerator Only"
              ,"Mini Refrigerator"
              ,"Refrigerated Beer Cooler"
              ,"Refrigerator with Bottom Freezer"
              ,"Refrigerator with Side-by-Side Freezer"
              ,"Refrigerator with Top Freezer"
              ,"Refrigerated Wine Cooler"
              ,"Side-by-Side Refrigerator with Bottom Freezer"
              ,"Total")
item82.os.final <- item82.os.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)
item82.os.final <- data.frame(item82.os.final)


item82.os.final.SF <- item82.os.final[which(item82.os.final$BuildingType == "Single Family")
                                ,which(colnames(item82.os.final) %notin% c("BuildingType"))]

exportTable(item82.os.final.SF, "SF", "Table 89", weighted = TRUE, osIndicator = export.ind, OS = T)

######################
# unweighted analysis
######################
item82.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item82.os.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = "APPLIANCE_FRIDGE_FREEZER_Type"
                                             ,aggregateColumnName = "Remove")
item82.os.final <- item82.os.final[which(item82.os.final$CK_Building_ID != "Remove"),]

item82.os.cast <- dcast(setDT(item82.os.final)
                        ,formula = BuildingType + APPLIANCE_FRIDGE_FREEZER_Type ~ CK_Building_ID
                        ,value.var = c("Percent", "SE","n"))

names(item82.os.cast)
if(os.ind == "scl"){
  item82.os.final <- data.frame("BuildingType"          = item82.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"    = item82.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Percent_SCL.GenPop"   = item82.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item82.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item82.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item82.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item82.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item82.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item82.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item82.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item82.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item82.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item82.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item82.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item82.os.final <- data.frame("BuildingType"          = item82.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"    = item82.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Percent_SnoPUD"          = item82.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item82.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item82.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item82.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item82.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item82.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item82.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item82.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item82.os.cast$`n_2017 RBSA NW`)
}



unique(item82.os.final$APPLIANCE_FRIDGE_FREEZER_Type)
rowOrder <- c("Full Size Refrigerator Only"
              ,"Mini Refrigerator"
              ,"Refrigerated Beer Cooler"
              ,"Refrigerator with Bottom Freezer"
              ,"Refrigerator with Side-by-Side Freezer"
              ,"Refrigerator with Top Freezer"
              ,"Refrigerated Wine Cooler"
              ,"Side-by-Side Refrigerator with Bottom Freezer"
              ,"Total")
item82.os.final <- item82.os.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)
item82.os.final <- data.frame(item82.os.final)


item82.os.final.SF <- item82.os.final[which(item82.os.final$BuildingType == "Single Family")
                                ,which(colnames(item82.os.final) %notin% c("BuildingType"))]

exportTable(item82.os.final.SF, "SF", "Table 89", weighted = FALSE, osIndicator = export.ind, OS = T)




#############################################################################################
#Item 83: AVERAGE REFRIGERATOR VOLUME BY TYPE (SF table 90, MH table 71)
#############################################################################################
#subset to columns needed for analysis
item83.os.dat <- item82.os.dat3[-grep("unknown",item82.os.dat3$`Refrigerator/Freezer.Size`, ignore.case = T),]
names(item83.os.dat)
######################################
#Pop and Sample Sizes for weights
######################################
item83.os.data <- weightedData(item83.os.dat[which(colnames(item83.os.dat) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
                                                                            ,"Type"
                                                                            ,"Refrigerator/Freezer.Size"))])

item83.os.data <- left_join(item83.os.data, unique(item83.os.dat[which(colnames(item83.os.dat) %in% c("CK_Cadmus_ID"
                                                                                   ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                                   ,"Type"
                                                                                   ,"Refrigerator/Freezer.Size"))]))
item83.os.data$count <- 1

item83.os.data$`Refrigerator/Freezer.Size` <- gsub(" cu ft", "", item83.os.data$`Refrigerator/Freezer.Size`)
item83.os.data$`Refrigerator/Freezer.Size` <- gsub("cu ft", "", item83.os.data$`Refrigerator/Freezer.Size`)
item83.os.data$`Refrigerator/Freezer.Size` <- gsub(" ", "", item83.os.data$`Refrigerator/Freezer.Size`)
item83.os.data$`Refrigerator/Freezer.Size` <- gsub("18..61", "18.61", item83.os.data$`Refrigerator/Freezer.Size`)
item83.os.data$`Refrigerator/Freezer.Size` <- gsub("19..1", "19.1", item83.os.data$`Refrigerator/Freezer.Size`)
unique(item83.os.data$`Refrigerator/Freezer.Size`)
item83.os.data$`Refrigerator/Freezer.Size` <- as.numeric(as.character(item83.os.data$`Refrigerator/Freezer.Size`))

######################
# weighted analysis
######################
item83.os.cast <- mean_two_groups(CustomerLevelData = item83.os.data
                               ,valueVariable = 'Refrigerator/Freezer.Size'
                               ,byVariableRow = 'APPLIANCE_FRIDGE_FREEZER_Type'
                               ,byVariableColumn = "CK_Building_ID"
                               ,rowAggregate = "All Refrigerator Types"
                               ,columnAggregate = "Remove")

names(item.os.cast)
if(os.ind == "scl"){
  item83.os.final <- data.frame("BuildingType"          = item83.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"= item83.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Mean_SCL.GenPop"      = item83.os.cast$`Mean_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item83.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item83.os.cast$`n_SCL GenPop`
                                ,"Mean_SCL.LI"          = item83.os.cast$`Mean_SCL LI`
                                ,"SE_SCL.LI"            = item83.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item83.os.cast$`n_SCL LI`
                                ,"Mean_SCL.EH"          = item83.os.cast$`Mean_SCL EH`
                                ,"SE_SCL.EH"            = item83.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item83.os.cast$`n_SCL EH`
                                ,"Mean_2017.RBSA.PS"    = item83.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item83.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item83.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item83.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item83.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item83.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item83.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item83.os.final <- data.frame("BuildingType"          = item83.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"= item83.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Mean_SnoPUD"          = item83.os.cast$`Mean_SnoPUD`
                                ,"SE_SnoPUD"            = item83.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"             = item83.os.cast$`n_SnoPUD`
                                ,"Mean_2017.RBSA.PS"    = item83.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item83.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item83.os.cast$`n_2017 RBSA PS`
                                ,"Mean_RBSA.NW"         = item83.os.cast$`Mean_2017 RBSA NW`
                                ,"SE_RBSA.NW"           = item83.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"            = item83.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"            = item83.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"      = item83.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"           = item83.os.cast$`EB_2017 RBSA NW`)
}


unique(item83.os.final$APPLIANCE_FRIDGE_FREEZER_Type)
rowOrder <- c("Full Size Refrigerator Only"
              ,"Mini Refrigerator"
              ,"Refrigerated Beer Cooler"
              ,"Refrigerator with Bottom Freezer"
              ,"Refrigerator with Side-by-Side Freezer"
              ,"Refrigerator with Top Freezer"
              ,"Refrigerated Wine Cooler"
              ,"Side-by-Side Refrigerator with Bottom Freezer"
              ,"All Refrigerator Types")
item83.os.final <- item83.os.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)
item83.os.final <- data.frame(item83.os.final)


item83.os.final.SF <- item83.os.final[which(item83.os.final$BuildingType == "Single Family")
                                ,which(colnames(item83.os.final) %notin% c("BuildingType"))]

exportTable(item83.os.final.SF, "SF", "Table 90", weighted = TRUE, osIndicator = export.ind, OS = T)

######################
# unweighted analysis
######################
item83.os.cast <- mean_two_groups_unweighted(CustomerLevelData = item83.os.data
                                  ,valueVariable = 'Refrigerator/Freezer.Size'
                                  ,byVariableRow = 'APPLIANCE_FRIDGE_FREEZER_Type'
                                  ,byVariableColumn = "CK_Building_ID"
                                  ,rowAggregate = "All Refrigerator Types"
                                  ,columnAggregate = "Remove")

if(os.ind == "scl"){
  item83.os.final <- data.frame("BuildingType"          = item83.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"= item83.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Mean_SCL.GenPop"      = item83.os.cast$`Mean_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item83.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item83.os.cast$`n_SCL GenPop`
                                ,"Mean_SCL.LI"          = item83.os.cast$`Mean_SCL LI`
                                ,"SE_SCL.LI"            = item83.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item83.os.cast$`n_SCL LI`
                                ,"Mean_SCL.EH"          = item83.os.cast$`Mean_SCL EH`
                                ,"SE_SCL.EH"            = item83.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item83.os.cast$`n_SCL EH`
                                ,"Mean_2017.RBSA.PS"    = item83.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item83.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item83.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item83.os.final <- data.frame("BuildingType"          = item83.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"= item83.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Mean_SnoPUD"          = item83.os.cast$`Mean_SnoPUD`
                                ,"SE_SnoPUD"            = item83.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"             = item83.os.cast$`n_SnoPUD`
                                ,"Mean_2017.RBSA.PS"    = item83.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item83.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item83.os.cast$`n_2017 RBSA PS`
                                ,"Mean_RBSA.NW"         = item83.os.cast$`Mean_2017 RBSA NW`
                                ,"SE_RBSA.NW"           = item83.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"            = item83.os.cast$`n_2017 RBSA NW`)
}



unique(item83.os.final$APPLIANCE_FRIDGE_FREEZER_Type)
rowOrder <- c("Full Size Refrigerator Only"
              ,"Mini Refrigerator"
              ,"Refrigerated Beer Cooler"
              ,"Refrigerator with Bottom Freezer"
              ,"Refrigerator with Side-by-Side Freezer"
              ,"Refrigerator with Top Freezer"
              ,"Refrigerated Wine Cooler"
              ,"Side-by-Side Refrigerator with Bottom Freezer"
              ,"All Refrigerator Types")
item83.os.final <- item83.os.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)
item83.os.final <- data.frame(item83.os.final)

item83.os.final.SF <- item83.os.final[which(item83.os.final$BuildingType == "Single Family")
                                ,which(colnames(item83.os.final) %notin% c("BuildingType"))]

exportTable(item83.os.final.SF, "SF", "Table 90", weighted = FALSE, osIndicator = export.ind, OS = T)





#############################################################################################
#Item 84: DISTRIBUTION OF FREEZERS BY TYPE IN HOMES (SF table 91, MH table 72)
#############################################################################################
item84.os.dat <- item82.os.dat2[which(item82.os.dat2$Type == "Freezer"),]

######################################
#Pop and Sample Sizes for weights
######################################
item84.os.data <- weightedData(item84.os.dat[which(colnames(item84.os.dat) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
                                                                            ,"Type"
                                                                            ,"Refrigerator/Freezer.Size"))])

item84.os.data <- left_join(item84.os.data, unique(item84.os.dat[which(colnames(item84.os.dat) %in% c("CK_Cadmus_ID"
                                                                                   ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                                   ,"Type"
                                                                                   ,"Refrigerator/Freezer.Size"))]))
item84.os.data$count <- 1


######################
# weighted analysis
######################
item84.os.final <- proportionRowsAndColumns1(CustomerLevelData = item84.os.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = "APPLIANCE_FRIDGE_FREEZER_Type"
                                             ,aggregateColumnName = "Remove")
item84.os.final <- item84.os.final[which(item84.os.final$CK_Building_ID != "Remove"),]

item84.os.cast <- dcast(setDT(item84.os.final)
                        ,formula = BuildingType + APPLIANCE_FRIDGE_FREEZER_Type ~ CK_Building_ID
                        ,value.var = c("w.percent", "w.SE","n", "EB"))


names(item84.os.cast)
if(os.ind == "scl"){
  item84.os.final <- data.frame("BuildingType"          = item84.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"    = item84.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Percent_SCL.GenPop"   = item84.os.cast$`w.percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item84.os.cast$`w.SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item84.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item84.os.cast$`w.percent_SCL LI`
                                ,"SE_SCL.LI"            = item84.os.cast$`w.SE_SCL LI`
                                ,"n_SCL.LI"             = item84.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item84.os.cast$`w.percent_SCL EH`
                                ,"SE_SCL.EH"            = item84.os.cast$`w.SE_SCL EH`
                                ,"n_SCL.EH"             = item84.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item84.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item84.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item84.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item84.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item84.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item84.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item84.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item84.os.final <- data.frame("BuildingType"          = item84.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"    = item84.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Percent_SnoPUD"          = item84.os.cast$`w.percent_SnoPUD`
                                ,"SE_SnoPUD"               = item84.os.cast$`w.SE_SnoPUD`
                                ,"n_SnoPUD"                = item84.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item84.os.cast$`w.percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item84.os.cast$`w.SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item84.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item84.os.cast$`w.percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item84.os.cast$`w.SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item84.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"               = item84.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"         = item84.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"              = item84.os.cast$`EB_2017 RBSA NW`)
}



unique(item84.os.final$APPLIANCE_FRIDGE_FREEZER_Type)
rowOrder <- c("Freezer, chest"
              ,"Freezer, upright"
              ,"Mini-Freezer"
              ,"Total")
item84.os.final <- item84.os.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)
item84.os.final <- data.frame(item84.os.final)

item84.os.final.SF <- item84.os.final[which(item84.os.final$BuildingType == "Single Family")
                                      ,which(colnames(item84.os.final) %notin% c("BuildingType"))]

exportTable(item84.os.final.SF, "SF", "Table 91", weighted = TRUE, osIndicator = export.ind, OS = T)


######################
# unweighted analysis
######################
item84.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item84.os.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = "APPLIANCE_FRIDGE_FREEZER_Type"
                                             ,aggregateColumnName = "Remove")
item84.os.final <- item84.os.final[which(item84.os.final$CK_Building_ID != "Remove"),]

item84.os.cast <- dcast(setDT(item84.os.final)
                        ,formula = BuildingType + APPLIANCE_FRIDGE_FREEZER_Type ~ CK_Building_ID
                        ,value.var = c("Percent", "SE","n"))


names(item84.os.cast)
if(os.ind == "scl"){
  item84.os.final <- data.frame("BuildingType"          = item84.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"    = item84.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Percent_SCL.GenPop"   = item84.os.cast$`Percent_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item84.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item84.os.cast$`n_SCL GenPop`
                                ,"Percent_SCL.LI"       = item84.os.cast$`Percent_SCL LI`
                                ,"SE_SCL.LI"            = item84.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item84.os.cast$`n_SCL LI`
                                ,"Percent_SCL.EH"       = item84.os.cast$`Percent_SCL EH`
                                ,"SE_SCL.EH"            = item84.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item84.os.cast$`n_SCL EH`
                                ,"Percent_2017.RBSA.PS" = item84.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item84.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item84.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item84.os.final <- data.frame("BuildingType"          = item84.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"    = item84.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Percent_SnoPUD"          = item84.os.cast$`Percent_SnoPUD`
                                ,"SE_SnoPUD"               = item84.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"                = item84.os.cast$`n_SnoPUD`
                                ,"Percent_2017.RBSA.PS"    = item84.os.cast$`Percent_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"         = item84.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"          = item84.os.cast$`n_2017 RBSA PS`
                                ,"Percent_RBSA.NW"         = item84.os.cast$`Percent_2017 RBSA NW`
                                ,"SE_RBSA.NW"              = item84.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"               = item84.os.cast$`n_2017 RBSA NW`)
}



unique(item84.os.final$APPLIANCE_FRIDGE_FREEZER_Type)
rowOrder <- c("Freezer, chest"
              ,"Freezer, upright"
              ,"Mini-Freezer"
              ,"Total")
item84.os.final <- item84.os.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)
item84.os.final <- data.frame(item84.os.final)

item84.os.final.SF <- item84.os.final[which(item84.os.final$BuildingType == "Single Family")
                                      ,which(colnames(item84.os.final) %notin% c("BuildingType"))]

exportTable(item84.os.final.SF, "SF", "Table 91", weighted = FALSE, osIndicator = export.ind, OS = T)






#############################################################################################
#Item 85: AVERAGE FREEZER VOLUME BY TYPE (SF table 92, MH table 73)
#############################################################################################
item85.os.dat <- item82.os.dat2[which(item82.os.dat2$Type == "Freezer"),]
item85.os.dat1 <- item85.os.dat[-grep("unknown",item85.os.dat$`Refrigerator/Freezer.Size`, ignore.case = T),]

######################################
#Pop and Sample Sizes for weights
######################################
item85.os.data <- weightedData(item85.os.dat1[which(colnames(item85.os.dat1) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
                                                                              ,"Type"
                                                                              ,"Refrigerator/Freezer.Size"))])

item85.os.data <- left_join(item85.os.data, unique(item85.os.dat1[which(colnames(item85.os.dat1) %in% c("CK_Cadmus_ID"
                                                                                     ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                                     ,"Type"
                                                                                     ,"Refrigerator/Freezer.Size"))]))
item85.os.data$count <- 1

item85.os.data$`Refrigerator/Freezer.Size` <- gsub(" cu ft", "", item85.os.data$`Refrigerator/Freezer.Size`)
item85.os.data$`Refrigerator/Freezer.Size` <- gsub("cu ft", "", item85.os.data$`Refrigerator/Freezer.Size`)
item85.os.data$`Refrigerator/Freezer.Size` <- gsub(" ", "", item85.os.data$`Refrigerator/Freezer.Size`)
unique(item85.os.data$`Refrigerator/Freezer.Size`)
item85.os.data$`Refrigerator/Freezer.Size` <- as.numeric(as.character(item85.os.data$`Refrigerator/Freezer.Size`))

######################
# weighted analysis
######################
item85.os.cast <- mean_two_groups(CustomerLevelData = item85.os.data
                                  ,valueVariable = 'Refrigerator/Freezer.Size'
                                  ,byVariableRow = 'APPLIANCE_FRIDGE_FREEZER_Type'
                                  ,byVariableColumn = "CK_Building_ID"
                                  ,rowAggregate = "Total"
                                  ,columnAggregate = "Remove")

names()
if(os.ind == "scl"){
  item85.os.final <- data.frame("BuildingType"          = item85.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"= item85.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Mean_SCL.GenPop"      = item85.os.cast$`Mean_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item85.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item85.os.cast$`n_SCL GenPop`
                                ,"Mean_SCL.LI"          = item85.os.cast$`Mean_SCL LI`
                                ,"SE_SCL.LI"            = item85.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item85.os.cast$`n_SCL LI`
                                ,"Mean_SCL.EH"          = item85.os.cast$`Mean_SCL EH`
                                ,"SE_SCL.EH"            = item85.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item85.os.cast$`n_SCL EH`
                                ,"Mean_2017.RBSA.PS"    = item85.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item85.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item85.os.cast$`n_2017 RBSA PS`
                                ,"EB_SCL.GenPop"        = item85.os.cast$`EB_SCL GenPop`
                                ,"EB_SCL.LI"            = item85.os.cast$`EB_SCL LI`
                                ,"EB_SCL.EH"            = item85.os.cast$`EB_SCL EH`
                                ,"EB_2017.RBSA.PS"      = item85.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item85.os.final <- data.frame("BuildingType"          = item85.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"= item85.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Mean_SnoPUD"          = item85.os.cast$`Mean_SnoPUD`
                                ,"SE_SnoPUD"            = item85.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"             = item85.os.cast$`n_SnoPUD`
                                ,"Mean_2017.RBSA.PS"    = item85.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item85.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item85.os.cast$`n_2017 RBSA PS`
                                ,"Mean_RBSA.NW"         = item85.os.cast$`Mean_2017 RBSA NW`
                                ,"SE_RBSA.NW"           = item85.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"            = item85.os.cast$`n_2017 RBSA NW`
                                ,"EB_SnoPUD"            = item85.os.cast$`EB_SnoPUD`
                                ,"EB_2017.RBSA.PS"      = item85.os.cast$`EB_2017 RBSA PS`
                                ,"EB_RBSA.NW"           = item85.os.cast$`EB_2017 RBSA NW`)
}



unique(item85.os.final$APPLIANCE_FRIDGE_FREEZER_Type)
rowOrder <- c("Freezer, chest"
              ,"Freezer, upright"
              ,"Total")
item85.os.final <- item85.os.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)
item85.os.final <- data.frame(item85.os.final)

item85.os.final.SF <- item85.os.final[which(item85.os.final$BuildingType == "Single Family")
                                ,which(colnames(item85.os.final) %notin% c("BuildingType"))]

exportTable(item85.os.final.SF, "SF", "Table 92", weighted = TRUE, osIndicator = export.ind, OS = T)

######################
# unweighted analysis
######################
item85.os.cast <- mean_two_groups_unweighted(CustomerLevelData = item85.os.data
                                  ,valueVariable = 'Refrigerator/Freezer.Size'
                                  ,byVariableRow = 'APPLIANCE_FRIDGE_FREEZER_Type'
                                  ,byVariableColumn = "CK_Building_ID"
                                  ,rowAggregate = "Total"
                                  ,columnAggregate = "Remove")
if(os.ind == "scl"){
  item85.os.final <- data.frame("BuildingType"          = item85.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"= item85.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Mean_SCL.GenPop"      = item85.os.cast$`Mean_SCL GenPop`
                                ,"SE_SCL.GenPop"        = item85.os.cast$`SE_SCL GenPop`
                                ,"n_SCL.GenPop"         = item85.os.cast$`n_SCL GenPop`
                                ,"Mean_SCL.LI"          = item85.os.cast$`Mean_SCL LI`
                                ,"SE_SCL.LI"            = item85.os.cast$`SE_SCL LI`
                                ,"n_SCL.LI"             = item85.os.cast$`n_SCL LI`
                                ,"Mean_SCL.EH"          = item85.os.cast$`Mean_SCL EH`
                                ,"SE_SCL.EH"            = item85.os.cast$`SE_SCL EH`
                                ,"n_SCL.EH"             = item85.os.cast$`n_SCL EH`
                                ,"Mean_2017.RBSA.PS"    = item85.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item85.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item85.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item85.os.final <- data.frame("BuildingType"          = item85.os.cast$BuildingType
                                ,"APPLIANCE_FRIDGE_FREEZER_Type"= item85.os.cast$APPLIANCE_FRIDGE_FREEZER_Type
                                ,"Mean_SnoPUD"          = item85.os.cast$`Mean_SnoPUD`
                                ,"SE_SnoPUD"            = item85.os.cast$`SE_SnoPUD`
                                ,"n_SnoPUD"             = item85.os.cast$`n_SnoPUD`
                                ,"Mean_2017.RBSA.PS"    = item85.os.cast$`Mean_2017 RBSA PS`
                                ,"SE_2017.RBSA.PS"      = item85.os.cast$`SE_2017 RBSA PS`
                                ,"n_2017.RBSA.PS"       = item85.os.cast$`n_2017 RBSA PS`
                                ,"Mean_RBSA.NW"         = item85.os.cast$`Mean_2017 RBSA NW`
                                ,"SE_RBSA.NW"           = item85.os.cast$`SE_2017 RBSA NW`
                                ,"n_RBSA.NW"            = item85.os.cast$`n_2017 RBSA NW`)
}


unique(item85.os.final$APPLIANCE_FRIDGE_FREEZER_Type)
rowOrder <- c("Freezer, chest"
              ,"Freezer, upright"
              ,"Total")
item85.os.final <- item85.os.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)
item85.os.final <- data.frame(item85.os.final)

item85.os.final.SF <- item85.os.final[which(item85.os.final$BuildingType == "Single Family")
                                      ,which(colnames(item85.os.final) %notin% c("BuildingType"))]

exportTable(item85.os.final.SF, "SF", "Table 92", weighted = FALSE, osIndicator = export.ind, OS = T)
