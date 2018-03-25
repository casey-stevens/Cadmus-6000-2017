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

# appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))

#Read in data for analysis
# sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))



#############################################################################################
#Table AJ: AVERAGE CLOTHES WASHER SIZE (CU. FT.) BY STATE
#############################################################################################
#subset to columns needed for analysis
tableAJ.dat <- unique(appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                           ,"Washer.Size"
                                                                           ,""))])

tableAJ.dat0 <- tableAJ.dat[which(!is.na(tableAJ.dat$Washer.Size)),]
tableAJ.dat1 <- tableAJ.dat0[-grep("unknown|-- Datapoint not asked for --|Datapoint not asked for|N/A",tableAJ.dat0$Washer.Size, ignore.case = T),]
tableAJ.dat1$Washer.Size <- gsub(" cu ft| u ft","",tableAJ.dat1$Washer.Size)
tableAJ.dat1$Washer.Size <- as.numeric(as.character(tableAJ.dat1$Washer.Size))
unique(tableAJ.dat1$Washer.Size)

tableAJ.dat2 <- left_join(rbsa.dat, tableAJ.dat1, by = "CK_Cadmus_ID")
tableAJ.dat2 <- tableAJ.dat2[which(!is.na(tableAJ.dat2$Washer.Size)),]
tableAJ.dat2$CK_Cadmus_ID[which(duplicated(tableAJ.dat2$CK_Cadmus_ID))]

# Weighting
tableAJ.data <- weightedData(tableAJ.dat2[-which(colnames(tableAJ.dat2) %in% c("Washer.Size"))])

tableAJ.data <- left_join(tableAJ.data, tableAJ.dat2[which(colnames(tableAJ.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Washer.Size"))])

tableAJ.data$count <- 1
# ###############################
# # Weighted Analysis
# ###############################
# tableAJ.final <- mean_one_group(CustomerLevelData = tableAJ.data
#                                ,valueVariable = 'Washer.Size' 
#                                ,byVariable    = 'State'
#                                ,aggregateRow  = "Region")
# 
# unique(tableAJ.final$State)
# rowOrder <- c("ID"
#               ,"MT"
#               ,"OR"
#               ,"WA"
#               ,"Region")
# tableAJ.final <- tableAJ.final %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
# tableAJ.final <- data.frame(tableAJ.final)
# 
# # Export table
# tableAJ.final.SF <- tableAJ.final[which(tableAJ.final$BuildingType == "Single Family"),-1]
# tableAJ.final.MH <- tableAJ.final[which(tableAJ.final$BuildingType == "Manufactured"),-1]
# 
# # exportTable(tableAJ.final.SF, "SF", "Table AJ", weighted = TRUE)
# exportTable(tableAJ.final.MH, "MH", "Table AJ", weighted = TRUE)

###############################
# MULTIFAMILY
###############################
tableAJ.final.MF <- mean_one_group(CustomerLevelData = tableAJ.data
                                ,valueVariable = 'Washer.Size'
                                ,byVariable    = 'HomeType'
                                ,aggregateRow  = "All Types")

tableAJ.final.MF <- tableAJ.final.MF[which(tableAJ.final.MF$BuildingType == "Multifamily"),-1]
exportTable(tableAJ.final.MF, "MF", "Table AJ", weighted = TRUE)


# ###############################
# # Unweighted Analysis
# ###############################
# tableAJ.final <- mean_one_group_unweighted(CustomerLevelData = tableAJ.data
#                                           ,valueVariable = 'Washer.Size' 
#                                           ,byVariable    = 'State'
#                                           ,aggregateRow  = "Region")
# 
# unique(tableAJ.final$State)
# rowOrder <- c("ID"
#               ,"MT"
#               ,"OR"
#               ,"WA"
#               ,"Region")
# tableAJ.final <- tableAJ.final %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
# tableAJ.final <- data.frame(tableAJ.final)
# 
# # Export table
# tableAJ.final.SF <- tableAJ.final[which(tableAJ.final$BuildingType == "Single Family"),-1]
# tableAJ.final.MH <- tableAJ.final[which(tableAJ.final$BuildingType == "Manufactured"),-1]
# 
# # exportTable(tableAJ.final.SF, "SF", "Table AJ", weighted = FALSE)
# exportTable(tableAJ.final.MH, "MH", "Table AJ", weighted = FALSE)

###############################
# MULTIFAMILY
###############################
tableAJ.final.MF <- mean_one_group_unweighted(CustomerLevelData = tableAJ.data
                                   ,valueVariable = 'Washer.Size'
                                   ,byVariable    = 'HomeType'
                                   ,aggregateRow  = "All Types")

tableAJ.final.MF <- tableAJ.final.MF[which(tableAJ.final.MF$BuildingType == "Multifamily"),-1]
exportTable(tableAJ.final.MF, "MF", "Table AJ", weighted = FALSE)

