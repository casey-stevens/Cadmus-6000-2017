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

#Read in data for analysis
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))


#############################################################################################
#Item 82: DISTRIBUTION OF REFRIGERATORS BY TYPE (SF table 89, MH table 70)
#############################################################################################
#subset to columns needed for analysis
item82.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                   ,"Refrigerator/Freezer.Size"))]


item82.dat0 <- item82.dat[which(!is.na(item82.dat$APPLIANCE_FRIDGE_FREEZER_Type)),]
item82.dat1 <- left_join(rbsa.dat, item82.dat0)

#clean type to match detailed type
item82.dat1$Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Freezer, chest")] <- "Freezer"
item82.dat1$Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Mini-Freezer")] <- "Freezer"
item82.dat1$Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Freezer, upright")] <- "Freezer"

#clean detailed type to match previous RBSA table
unique(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type)
item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "R/F Top Freezer")] <- "Refrigerator with Top Freezer"
item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "R/F Bottom Freezer")] <- "Refrigerator with Bottom Freezer"
item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Side by Side w/ Bottom Freezer")] <- "Side-by-Side Refrigerator with Bottom Freezer"
item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Side by Side Refrigerator/Freezer")] <- "Refrigerator with Side-by-Side Freezer"
item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Full Size Single Refrigerator Only")] <- "Full Size Refrigerator Only"
item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Mini-Fridge")] <- "Mini Refrigerator"

item82.dat2 <- item82.dat1[which(item82.dat1$APPLIANCE_FRIDGE_FREEZER_Type != "Unknown"),]

item82.dat3 <- item82.dat2[which(item82.dat2$Type == "Refrigerator"),]

######################################
#Pop and Sample Sizes for weights
######################################
item82.data <- weightedData(item82.dat3[which(colnames(item82.dat3) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
                                                                              ,"Type"
                                                                              ,"Refrigerator/Freezer.Size"))])

item82.data <- left_join(item82.data, item82.dat3[which(colnames(item82.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                                     ,"Type"
                                                                                     ,"Refrigerator/Freezer.Size"))])
item82.data$count <- 1


######################
# weighted analysis
######################
item82.final <- proportions_one_group(CustomerLevelData = item82.data
                                      ,valueVariable = 'count'
                                      ,groupingVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
                                      ,total.name = 'Total')

unique(item82.final$APPLIANCE_FRIDGE_FREEZER_Type)
rowOrder <- c("Full Size Refrigerator Only"
              ,"Mini Refrigerator"
              ,"Refrigerated Beer Cooler"
              ,"Refrigerator with Bottom Freezer"
              ,"Refrigerator with Side-by-Side Freezer"
              ,"Refrigerator with Top Freezer"
              ,"Refrigerated Wine Cooler"
              ,"Side-by-Side Refrigerator with Bottom Freezer"
              ,"Total")
item82.final <- item82.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)  
item82.final <- data.frame(item82.final)


item82.final.SF <- item82.final[which(item82.final$BuildingType == "Single Family")
                                ,which(colnames(item82.final) %notin% c("BuildingType"))]
exportTable(item82.final.SF, "SF", "Table 89", weighted = TRUE)

######################
# unweighted analysis
######################
item82.final <- proportions_one_group(CustomerLevelData = item82.data
                                      ,valueVariable = 'count'
                                      ,groupingVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
                                      ,total.name = 'Total'
                                      ,weighted = FALSE)


unique(item82.final$APPLIANCE_FRIDGE_FREEZER_Type)
rowOrder <- c("Full Size Refrigerator Only"
              ,"Mini Refrigerator"
              ,"Refrigerated Beer Cooler"
              ,"Refrigerator with Bottom Freezer"
              ,"Refrigerator with Side-by-Side Freezer"
              ,"Refrigerator with Top Freezer"
              ,"Refrigerated Wine Cooler"
              ,"Side-by-Side Refrigerator with Bottom Freezer"
              ,"Total")
item82.final <- item82.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)  
item82.final <- data.frame(item82.final)


item82.final.SF <- item82.final[which(item82.final$BuildingType == "Single Family")
                                ,which(colnames(item82.final) %notin% c("BuildingType"))]
exportTable(item82.final.SF, "SF", "Table 89", weighted = FALSE)







#############################################################################################
#Item 83: AVERAGE REFRIGERATOR VOLUME BY TYPE (SF table 90, MH table 71)
#############################################################################################
#subset to columns needed for analysis
item83.dat <- item82.dat3[-grep("unknown",item82.dat3$`Refrigerator/Freezer.Size`, ignore.case = T),]

######################################
#Pop and Sample Sizes for weights
######################################
item83.data <- weightedData(item83.dat[which(colnames(item83.dat) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
                                                                            ,"Type"
                                                                            ,"Refrigerator/Freezer.Size"))])

item83.data <- left_join(item83.data, item83.dat[which(colnames(item83.dat) %in% c("CK_Cadmus_ID"
                                                                                   ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                                   ,"Type"
                                                                                   ,"Refrigerator/Freezer.Size"))])
item83.data$count <- 1

item83.data$`Refrigerator/Freezer.Size` <- gsub(" cu ft", "", item83.data$`Refrigerator/Freezer.Size`)
item83.data$`Refrigerator/Freezer.Size` <- gsub("cu ft", "", item83.data$`Refrigerator/Freezer.Size`)
item83.data$`Refrigerator/Freezer.Size` <- gsub(" ", "", item83.data$`Refrigerator/Freezer.Size`)
item83.data$`Refrigerator/Freezer.Size` <- gsub("18..61", "18.61", item83.data$`Refrigerator/Freezer.Size`)
item83.data$`Refrigerator/Freezer.Size` <- gsub("19..1", "19.1", item83.data$`Refrigerator/Freezer.Size`)
unique(item83.data$`Refrigerator/Freezer.Size`)
item83.data$`Refrigerator/Freezer.Size` <- as.numeric(as.character(item83.data$`Refrigerator/Freezer.Size`))

######################
# weighted analysis
######################
item83.final <- mean_one_group(CustomerLevelData = item83.data
                               ,valueVariable = 'Refrigerator/Freezer.Size'
                               ,byVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
                               ,aggregateRow = "All Refrigerator Types")


unique(item83.final$APPLIANCE_FRIDGE_FREEZER_Type)
rowOrder <- c("Full Size Refrigerator Only"
              ,"Mini Refrigerator"
              ,"Refrigerated Beer Cooler"
              ,"Refrigerator with Bottom Freezer"
              ,"Refrigerator with Side-by-Side Freezer"
              ,"Refrigerator with Top Freezer"
              ,"Refrigerated Wine Cooler"
              ,"Side-by-Side Refrigerator with Bottom Freezer"
              ,"All Refrigerator Types")
item83.final <- item83.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)  
item83.final <- data.frame(item83.final)


item83.final.SF <- item83.final[which(item83.final$BuildingType == "Single Family")
                                ,which(colnames(item83.final) %notin% c("BuildingType"))]
exportTable(item83.final.SF, "SF", "Table 90", weighted = TRUE)

######################
# unweighted analysis
######################
item83.final <- mean_one_group_unweighted(CustomerLevelData = item83.data
                                          ,valueVariable = 'Refrigerator/Freezer.Size'
                                          ,byVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
                                          ,aggregateRow = "All Refrigerator Types")


unique(item83.final$APPLIANCE_FRIDGE_FREEZER_Type)
rowOrder <- c("Full Size Refrigerator Only"
              ,"Mini Refrigerator"
              ,"Refrigerated Beer Cooler"
              ,"Refrigerator with Bottom Freezer"
              ,"Refrigerator with Side-by-Side Freezer"
              ,"Refrigerator with Top Freezer"
              ,"Refrigerated Wine Cooler"
              ,"Side-by-Side Refrigerator with Bottom Freezer"
              ,"All Refrigerator Types")
item83.final <- item83.final %>% mutate(APPLIANCE_FRIDGE_FREEZER_Type = factor(APPLIANCE_FRIDGE_FREEZER_Type, levels = rowOrder)) %>% arrange(APPLIANCE_FRIDGE_FREEZER_Type)  
item83.final <- data.frame(item83.final)

item83.final.SF <- item83.final[which(item83.final$BuildingType == "Single Family")
                                ,which(colnames(item83.final) %notin% c("BuildingType"))]
exportTable(item83.final.SF, "SF", "Table 90", weighted = FALSE)





#############################################################################################
#Item 84: DISTRIBUTION OF FREEZERS BY TYPE IN HOMES (SF table 91, MH table 72)
#############################################################################################
item84.dat <- item82.dat2[which(item82.dat2$Type == "Freezer"),]

######################################
#Pop and Sample Sizes for weights
######################################
item84.data <- weightedData(item84.dat[which(colnames(item84.dat) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
                                                                              ,"Type"
                                                                              ,"Refrigerator/Freezer.Size"))])

item84.data <- left_join(item84.data, item84.dat[which(colnames(item84.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                                     ,"Type"
                                                                                     ,"Refrigerator/Freezer.Size"))])
item84.data$count <- 1


######################
# weighted analysis
######################
item84.final <- proportions_one_group(CustomerLevelData = item84.data
                                      ,valueVariable = 'count'
                                      ,groupingVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
                                      ,total.name = 'Total')
item84.final.SF <- item84.final[which(item84.final$BuildingType == "Single Family")
                                ,which(colnames(item84.final) %notin% c("BuildingType"))]
exportTable(item84.final.SF, "SF", "Table 91", weighted = TRUE)

######################
# unweighted analysis
######################
item84.final <- proportions_one_group(CustomerLevelData = item84.data
                                      ,valueVariable = 'count'
                                      ,groupingVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
                                      ,total.name = 'Total'
                                      ,weighted = FALSE)
item84.final.SF <- item84.final[which(item84.final$BuildingType == "Single Family")
                                ,which(colnames(item84.final) %notin% c("BuildingType"))]
exportTable(item84.final.SF, "SF", "Table 91", weighted = FALSE)






#############################################################################################
#Item 85: AVERAGE FREEZER VOLUME BY TYPE (SF table 92, MH table 73)
#############################################################################################
item85.dat <- item82.dat2[which(item82.dat2$Type == "Freezer"),]
item85.dat1 <- item85.dat[-grep("unknown",item85.dat$`Refrigerator/Freezer.Size`, ignore.case = T),]

######################################
#Pop and Sample Sizes for weights
######################################
item85.data <- weightedData(item85.dat1[which(colnames(item85.dat1) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
                                                                              ,"Type"
                                                                              ,"Refrigerator/Freezer.Size"))])

item85.data <- left_join(item85.data, item85.dat1[which(colnames(item85.dat1) %in% c("CK_Cadmus_ID"
                                                                                     ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                                     ,"Type"
                                                                                     ,"Refrigerator/Freezer.Size"))])
item85.data$count <- 1

item85.data$`Refrigerator/Freezer.Size` <- gsub(" cu ft", "", item85.data$`Refrigerator/Freezer.Size`)
item85.data$`Refrigerator/Freezer.Size` <- gsub("cu ft", "", item85.data$`Refrigerator/Freezer.Size`)
item85.data$`Refrigerator/Freezer.Size` <- gsub(" ", "", item85.data$`Refrigerator/Freezer.Size`)
unique(item85.data$`Refrigerator/Freezer.Size`)
item85.data$`Refrigerator/Freezer.Size` <- as.numeric(as.character(item85.data$`Refrigerator/Freezer.Size`))

######################
# weighted analysis
######################
item85.final <- mean_one_group(CustomerLevelData = item85.data
                               ,valueVariable = 'Refrigerator/Freezer.Size'
                               ,byVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
                               ,aggregateRow = "All Refrigerator Types")

item85.final.SF <- item85.final[which(item85.final$BuildingType == "Single Family")
                                ,which(colnames(item85.final) %notin% c("BuildingType"))]
exportTable(item85.final.SF, "SF", "Table 92", weighted = TRUE)

######################
# unweighted analysis
######################
item85.final <- mean_one_group_unweighted(CustomerLevelData = item85.data
                                          ,valueVariable = 'Refrigerator/Freezer.Size'
                                          ,byVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
                                          ,aggregateRow = "All Refrigerator Types")

item85.final.SF <- item85.final[which(item85.final$BuildingType == "Single Family")
                                ,which(colnames(item85.final) %notin% c("BuildingType"))]
exportTable(item85.final.SF, "SF", "Table 92", weighted = FALSE)
