#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)


##  Create "Not In" operator
"%notin%" <- Negate("%in%")


# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
rbsa.dat.site <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
# lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))
lighting.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"
                                                               ,"Switch.Type"))]
lighting.dat.LED <- lighting.dat[which(lighting.dat$Lamp.Category == "Light Emitting Diode"),]
lighting.dat.CFL <- lighting.dat[which(lighting.dat$Lamp.Category == "Compact Fluorescent"),]

#Read in data for analysis
survey.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Labeled and Translated")
#clean cadmus IDs
survey.dat$CK_Cadmus_ID <- trimws(toupper(survey.dat$NEXID))
survey.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID", "Do.you.own.or.rent.your.home?"))]
colnames(survey.dat) <- c("Ownership.Type", "CK_Cadmus_ID")

#merge together analysis data with cleaned RBSA data
merge.dat1 <- left_join(rbsa.dat, survey.dat, by = "CK_Cadmus_ID")
unique(merge.dat1$Ownership.Type)

#remove NA from ownership type
merge.dat2 <- merge.dat1[which(merge.dat1$Ownership.Type %notin% c("N/A",NA)),]
length(unique(merge.dat2$CK_Cadmus_ID))



# #############################################################################################
# # Table NN: Percentage of homes with CFLs by state
# #############################################################################################
# tableNN.dat <- left_join(rbsa.dat, lighting.dat.CFL)
# tableNN.dat$Ind <- 0
# tableNN.dat$Ind[which(tableNN.dat$Lamp.Category == "Compact Fluorescent")] <- 1
# 
# tableNN.sum <- summarise(group_by(tableNN.dat, CK_Cadmus_ID)
#                          ,Ind = sum(unique((Ind))))
# 
# tableNN.merge <- left_join(rbsa.dat, tableNN.sum)
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# tableNN.data <- weightedData(tableNN.merge[-which(colnames(tableNN.merge) %in% c("Ind"))])
# tableNN.data <- left_join(tableNN.data, tableNN.merge[which(colnames(tableNN.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Ind"))])
# tableNN.data$count <- 1
# tableNN.data$Count <- 1
# 
#######################
# Weighted Analysis
#######################
# tableNN.table <- proportions_one_group(CustomerLevelData = tableNN.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "State"
#                                        ,total.name = "Region"
#                                        ,weighted = TRUE)
# tableNN.table.SF <- tableNN.table[which(tableNN.table$BuildingType == "Single Family")
#                                   ,which(colnames(tableNN.table) %notin% c("BuildingType"))]
# tableNN.table.MH <- tableNN.table[which(tableNN.table$BuildingType == "Manufactured")
#                                   ,which(colnames(tableNN.table) %notin% c("BuildingType"))]
# 
# exportTable(tableNN.table.SF, "SF", "Table NN", weighted = TRUE)
# # exportTable(tableNN.table.MH, "MH", "Table NN", weighted = TRUE)

#######################
# unweighted Analysis
#######################
# tableNN.table <- proportions_one_group(CustomerLevelData = tableNN.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "State"
#                                        ,total.name = "Region"
#                                        ,weighted = FALSE)
# tableNN.table.SF <- tableNN.table[which(tableNN.table$BuildingType == "Single Family")
#                                   ,which(colnames(tableNN.table) %notin% c("BuildingType"))]
# tableNN.table.MH <- tableNN.table[which(tableNN.table$BuildingType == "Manufactured")
#                                   ,which(colnames(tableNN.table) %notin% c("BuildingType"))]
# 
# exportTable(tableNN.table.SF, "SF", "Table NN", weighted = FALSE)
# # exportTable(tableNN.table.MH, "MH", "Table NN", weighted = FALSE)


###########################################################
# Multifamily
###########################################################
tableNN.MF.dat <- left_join(rbsa.dat, lighting.dat.CFL)
tableNN.MF.dat$Ind <- 0
tableNN.MF.dat$Ind[which(tableNN.MF.dat$Lamp.Category == "Compact Fluorescent")] <- 1

tableNN.MF.sum <- summarise(group_by(tableNN.MF.dat, CK_Cadmus_ID)
                            ,Ind = sum(unique((Ind))))

tableNN.MF.merge <- left_join(rbsa.dat, tableNN.MF.sum)

################################################
# Adding pop and sample sizes for weights
################################################
tableNN.MF.data <- weightedData(tableNN.MF.merge[-which(colnames(tableNN.MF.merge) %in% c("Ind"
                                                                                          ,"Category"))])
tableNN.MF.data <- left_join(tableNN.MF.data, tableNN.MF.merge[which(colnames(tableNN.MF.merge) %in% c("CK_Cadmus_ID"
                                                                                                       ,"Ind"
                                                                                                       ,"Category"))])
tableNN.MF.data$count <- 1
tableNN.MF.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableNN.MF.data$State <- tableNN.MF.data$Category
tableNN.MF.table <- proportions_one_group(CustomerLevelData = tableNN.MF.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Remove"
                                       ,weighted = TRUE)
tableNN.MF.table <- tableNN.MF.table[which(tableNN.MF.table$State != "Total"),]
tableNN.MF.table.MF <- tableNN.MF.table[which(tableNN.MF.table$BuildingType == "Multifamily")
                                        ,which(colnames(tableNN.MF.table) %notin% c("BuildingType"))]

exportTable(tableNN.MF.table, "MF", "Table NN", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# unweighted Analysis
#######################
tableNN.MF.table <- proportions_one_group(CustomerLevelData = tableNN.MF.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Remove"
                                       ,weighted = FALSE)
tableNN.MF.table <- tableNN.MF.table[which(tableNN.MF.table$State != "Total"),]
tableNN.MF.table.MF <- tableNN.MF.table[which(tableNN.MF.table$BuildingType == "Multifamily")
                                  ,which(colnames(tableNN.MF.table) %notin% c("BuildingType"))]

exportTable(tableNN.MF.table.MF, "MF", "Table NN", weighted = FALSE,OS = T, osIndicator = "PSE")






#############################################################################################
# Table OO: Percentage of homes with LEDs by state
#############################################################################################
# tableOO.dat <- left_join(rbsa.dat, lighting.dat.LED)
# tableOO.dat$Ind <- 0
# tableOO.dat$Ind[which(tableOO.dat$Lamp.Category == "Light Emitting Diode")] <- 1
# 
# tableOO.sum <- summarise(group_by(tableOO.dat, CK_Cadmus_ID)
#                          ,Ind = sum(unique((Ind))))
# 
# tableOO.merge <- left_join(rbsa.dat, tableOO.sum)
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# tableOO.data <- weightedData(tableOO.merge[-which(colnames(tableOO.merge) %in% c("Ind"))])
# tableOO.data <- left_join(tableOO.data, tableOO.merge[which(colnames(tableOO.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Ind"))])
# tableOO.data$count <- 1
# tableOO.data$Count <- 1
# 
# #######################
# # Weighted Analysis
# #######################
# tableOO.table <- proportions_one_group(CustomerLevelData = tableOO.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "State"
#                                        ,total.name = "Region"
#                                        ,weighted = TRUE)
# tableOO.table.SF <- tableOO.table[which(tableOO.table$BuildingType == "Single Family")
#                                   ,which(colnames(tableOO.table) %notin% c("BuildingType"))]
# tableOO.table.MH <- tableOO.table[which(tableOO.table$BuildingType == "Manufactured")
#                                   ,which(colnames(tableOO.table) %notin% c("BuildingType"))]
# 
# exportTable(tableOO.table.SF, "SF", "Table OO", weighted = TRUE)
# # exportTable(tableOO.table.MH, "MH", "Table OO", weighted = TRUE)
# 
# 
# #######################
# # unweighted Analysis
# #######################
# tableOO.table <- proportions_one_group(CustomerLevelData = tableOO.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "State"
#                                        ,total.name = "Region"
#                                        ,weighted = FALSE)
# tableOO.table.SF <- tableOO.table[which(tableOO.table$BuildingType == "Single Family")
#                                   ,which(colnames(tableOO.table) %notin% c("BuildingType"))]
# tableOO.table.MH <- tableOO.table[which(tableOO.table$BuildingType == "Manufactured")
#                                   ,which(colnames(tableOO.table) %notin% c("BuildingType"))]
# 
# exportTable(tableOO.table.SF, "SF", "Table OO", weighted = FALSE)
# # exportTable(tableOO.table.MH, "MH", "Table OO", weighted = FALSE)


###########################################################
# Multifamily
###########################################################
tableOO.MF.dat <- left_join(rbsa.dat, lighting.dat.LED)
tableOO.MF.dat$Ind <- 0
tableOO.MF.dat$Ind[which(tableOO.MF.dat$Lamp.Category == "Light Emitting Diode")] <- 1

tableOO.MF.sum <- summarise(group_by(tableOO.MF.dat, CK_Cadmus_ID)
                            ,Ind = sum(unique((Ind))))

tableOO.MF.merge <- left_join(rbsa.dat, tableOO.MF.sum)

################################################
# Adding pop and sample sizes for weights
################################################
tableOO.MF.data <- weightedData(tableOO.MF.merge[-which(colnames(tableOO.MF.merge) %in% c("Ind"
                                                                                          ,"Category"))])
tableOO.MF.data <- left_join(tableOO.MF.data, tableOO.MF.merge[which(colnames(tableOO.MF.merge) %in% c("CK_Cadmus_ID"
                                                                                                       ,"Ind"
                                                                                                       ,"Category"))])
tableOO.MF.data$count <- 1
tableOO.MF.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableOO.MF.data$State <- tableOO.MF.data$Category
tableOO.MF.table <- proportions_one_group(CustomerLevelData = tableOO.MF.data
                                          ,valueVariable = "Ind"
                                          ,groupingVariable = "State"
                                          ,total.name = "Remove"
                                          ,weighted = TRUE)
tableNN.MF.table <- tableNN.MF.table[which(tableNN.MF.table$State != "Total"),]
tableOO.MF.table.MF <- tableOO.MF.table[which(tableOO.MF.table$BuildingType == "Multifamily")
                                        ,which(colnames(tableOO.MF.table) %notin% c("BuildingType"))]

exportTable(tableOO.MF.table.MF, "MF", "Table OO", weighted = TRUE,OS = T, osIndicator = "PSE")

#######################
# unweighted Analysis
#######################
tableOO.MF.table <- proportions_one_group(CustomerLevelData = tableOO.MF.data
                                          ,valueVariable = "Ind"
                                          ,groupingVariable = "State"
                                          ,total.name = "Remove"
                                          ,weighted = FALSE)
tableNN.MF.table <- tableNN.MF.table[which(tableNN.MF.table$State != "Total"),]
tableOO.MF.table.MF <- tableOO.MF.table[which(tableOO.MF.table$BuildingType == "Multifamily")
                                        ,which(colnames(tableOO.MF.table) %notin% c("BuildingType"))]

exportTable(tableOO.MF.table.MF, "MF", "Table OO", weighted = FALSE,OS = T, osIndicator = "PSE")

