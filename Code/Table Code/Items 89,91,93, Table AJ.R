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

appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))

#Read in data for analysis
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))

#############################################################################################
#Item 89: AVERAGE NUMBER OF CLOTHES WASHER LOADS PER WEEK BY STATE (SF table 96, MH table 77)
#############################################################################################
#subset to columns needed for analysis
item89.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                    ,"INTRVW_CUST_RES_HomeandEnergyUseHome_ClothesWasherLoadsPerWeek"
                                                                                    ,""))])
colnames(item89.dat) <- c("CK_Cadmus_ID", "Clothes.Washes.Per.Week")
item89.dat$count <- 1

item89.dat0 <- unique(item89.dat)

item89.dat1 <- left_join(rbsa.dat, item89.dat0, by = "CK_Cadmus_ID")
item89.dat1$CK_Cadmus_ID[which(duplicated(item89.dat1$CK_Cadmus_ID))]

unique(item89.dat1$Clothes.Washes.Per.Week)
item89.dat1$Clothes.Washes.Per.Week[which(item89.dat1$Clothes.Washes.Per.Week %in% c("No Washing Machine", NA))] <- 0
item89.dat1$Clothes.Washes.Per.Week <- as.numeric(as.character(item89.dat1$Clothes.Washes.Per.Week))


# Weighting
item89.data <- weightedData(item89.dat1[-which(colnames(item89.dat1) %in% c("Clothes.Washes.Per.Week"
                                                                            ,"count"))])

item89.data <- left_join(item89.data, item89.dat1[which(colnames(item89.dat1) %in% c("CK_Cadmus_ID"
                                                                                     ,"Clothes.Washes.Per.Week"
                                                                                     ,"count"))])

###############################
# Weighted Analysis
###############################

item89.final <- mean_one_group(CustomerLevelData = item89.data
                               ,valueVariable = 'Clothes.Washes.Per.Week' 
                               ,byVariable    = 'State'
                               ,aggregateRow  = "Region")

unique(item89.final$State)
rowOrder <- c("ID"
              ,"MT"
              ,"OR"
              ,"WA"
              ,"Region")
item89.final <- item89.final %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
item89.final <- data.frame(item89.final)

# Export table
item89.final.SF <- item89.final[which(item89.final$BuildingType == "Single Family"),-1]
item89.final.MH <- item89.final[which(item89.final$BuildingType == "Manufactured"),-1]

# exportTable(item89.final.SF, "SF", "Table 96", weighted = TRUE)
exportTable(item89.final.MH, "MH", "Table 77", weighted = TRUE)


###############################
# Unweighted Analysis
###############################

item89.final <- mean_one_group_unweighted(CustomerLevelData = item89.data
                                          ,valueVariable = 'Clothes.Washes.Per.Week' 
                                          ,byVariable    = 'State'
                                          ,aggregateRow  = "Region")

unique(item89.final$State)
rowOrder <- c("ID"
              ,"MT"
              ,"OR"
              ,"WA"
              ,"Region")
item89.final <- item89.final %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
item89.final <- data.frame(item89.final)

# Export table
item89.final.SF <- item89.final[which(item89.final$BuildingType == "Single Family"),-1]
item89.final.MH <- item89.final[which(item89.final$BuildingType == "Manufactured"),-1]

# exportTable(item89.final.SF, "SF", "Table 96", weighted = FALSE)
exportTable(item89.final.MH, "MH", "Table 77", weighted = FALSE)




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
###############################
# Weighted Analysis
###############################
tableAJ.final <- mean_one_group(CustomerLevelData = tableAJ.data
                               ,valueVariable = 'Washer.Size' 
                               ,byVariable    = 'State'
                               ,aggregateRow  = "Region")

unique(tableAJ.final$State)
rowOrder <- c("ID"
              ,"MT"
              ,"OR"
              ,"WA"
              ,"Region")
tableAJ.final <- tableAJ.final %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
tableAJ.final <- data.frame(tableAJ.final)

# Export table
tableAJ.final.SF <- tableAJ.final[which(tableAJ.final$BuildingType == "Single Family"),-1]
tableAJ.final.MH <- tableAJ.final[which(tableAJ.final$BuildingType == "Manufactured"),-1]

# exportTable(tableAJ.final.SF, "SF", "Table AJ", weighted = TRUE)
exportTable(tableAJ.final.MH, "MH", "Table AJ", weighted = TRUE)

###############################
# MULTIFAMILY
###############################
tableAJ.final.MF <- mean_one_group(CustomerLevelData = tableAJ.data
                                ,valueVariable = 'Washer.Size' 
                                ,byVariable    = 'HomeType'
                                ,aggregateRow  = "All Types")

tableAJ.final.MF <- tableAJ.final.MF[which(tableAJ.final.MF$BuildingType == "Multifamily"),-1]
# exportTable(tableAJ.final.MF, "MF", "Table AJ", weighted = TRUE)


###############################
# Unweighted Analysis
###############################
tableAJ.final <- mean_one_group_unweighted(CustomerLevelData = tableAJ.data
                                          ,valueVariable = 'Washer.Size' 
                                          ,byVariable    = 'State'
                                          ,aggregateRow  = "Region")

unique(tableAJ.final$State)
rowOrder <- c("ID"
              ,"MT"
              ,"OR"
              ,"WA"
              ,"Region")
tableAJ.final <- tableAJ.final %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
tableAJ.final <- data.frame(tableAJ.final)

# Export table
tableAJ.final.SF <- tableAJ.final[which(tableAJ.final$BuildingType == "Single Family"),-1]
tableAJ.final.MH <- tableAJ.final[which(tableAJ.final$BuildingType == "Manufactured"),-1]

# exportTable(tableAJ.final.SF, "SF", "Table AJ", weighted = FALSE)
exportTable(tableAJ.final.MH, "MH", "Table AJ", weighted = FALSE)

###############################
# MULTIFAMILY
###############################
tableAJ.final.MF <- mean_one_group_unweighted(CustomerLevelData = tableAJ.data
                                   ,valueVariable = 'Washer.Size' 
                                   ,byVariable    = 'HomeType'
                                   ,aggregateRow  = "All Types")

tableAJ.final.MF <- tableAJ.final.MF[which(tableAJ.final.MF$BuildingType == "Multifamily"),-1]
# exportTable(tableAJ.final.MF, "MF", "Table AJ", weighted = FALSE)





#############################################################################################
#Item 91: AVERAGE NUMBER OF CLOTHES WASHER LOADS PER WEEK BY STATE (SF table 98, MH table 79)
#############################################################################################
#subset to columns needed for analysis
item91.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                    ,"INTRVW_CUST_RES_HomeandEnergyUseHome_ClothesWasherLoadsPerWeek"
                                                                                    ,"INTRVW_CUST_RES_HomeandEnergyUseHome_PercentOfLoadsThatGoInDryer"))])
colnames(item91.dat) <- c("CK_Cadmus_ID", "Clothes.Washes.Per.Week", "Percent.Loads.Go.In.Dryer")
item91.dat$count <- 1

item91.dat0 <- item91.dat[which(item91.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item91.dat1 <- left_join(rbsa.dat, item91.dat0, by = "CK_Cadmus_ID")

item91.dat2 <- item91.dat1[which(item91.dat1$Clothes.Washes.Per.Week != "No Washing Machine"),]
item91.dat2$Clothes.Washes.Per.Week   <- as.numeric(as.character(item91.dat2$Clothes.Washes.Per.Week))
item91.dat2$Percent.Loads.Go.In.Dryer <- as.numeric(as.character(item91.dat2$Percent.Loads.Go.In.Dryer))
item91.dat2$Dryer.Loads.Per.Week <- item91.dat2$Clothes.Washes.Per.Week * (item91.dat2$Percent.Loads.Go.In.Dryer)

item91.dat2$Dryer.Loads.Per.Wash <- item91.dat2$Dryer.Loads.Per.Week / item91.dat2$Clothes.Washes.Per.Week
unique(item91.dat2$Dryer.Loads.Per.Wash)

item91.dat3 <- item91.dat2[which(item91.dat2$Dryer.Loads.Per.Wash %notin% c("NaN", NA,"N/A")),]

################################################
# Adding pop and sample sizes for weights
################################################
item91.data <- weightedData(item91.dat3[-which(colnames(item91.dat3) %in% c("Clothes.Washes.Per.Week"
                                                                            ,"Percent.Loads.Go.In.Dryer"
                                                                            ,"count"
                                                                            ,"Dryer.Loads.Per.Week"
                                                                            ,"Dryer.Loads.Per.Wash"))])
item91.data <- left_join(item91.data, item91.dat3[which(colnames(item91.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"Clothes.Washes.Per.Week"
                                                                                     ,"Percent.Loads.Go.In.Dryer"
                                                                                     ,"count"
                                                                                     ,"Dryer.Loads.Per.Week"
                                                                                     ,"Dryer.Loads.Per.Wash"))])
item91.data$count <- 1

#######################
# Weighted Analysis
#######################
item91.final <- mean_one_group(CustomerLevelData = item91.data
                               ,valueVariable = 'Dryer.Loads.Per.Wash'
                               ,byVariable = 'State'
                               ,aggregateRow = "Region")

item91.final.SF <- item91.final[which(item91.final$BuildingType == "Single Family")
                                ,-which(colnames(item91.final) %in% c("BuildingType"))]
item91.final.MH <- item91.final[which(item91.final$BuildingType == "Manufactured")
                                ,-which(colnames(item91.final) %in% c("BuildingType"))]

# exportTable(item91.final.SF, "SF", "Table 98", weighted = TRUE)
exportTable(item91.final.MH, "MH", "Table 79", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item91.final <- mean_one_group_unweighted(CustomerLevelData = item91.data
                                          ,valueVariable = 'Dryer.Loads.Per.Wash'
                                          ,byVariable = 'State'
                                          ,aggregateRow = "Region")

item91.final.SF <- item91.final[which(item91.final$BuildingType == "Single Family")
                                ,-which(colnames(item91.final) %in% c("BuildingType"))]
item91.final.MH <- item91.final[which(item91.final$BuildingType == "Manufactured")
                                ,-which(colnames(item91.final) %in% c("BuildingType"))]

# exportTable(item91.final.SF, "SF", "Table 98", weighted = FALSE)
exportTable(item91.final.MH, "MH", "Table 79", weighted = FALSE)






#############################################################################################
#Item 93: AVERAGE NUMBER OF DISHWASHER LOADS PER WEEK (SF table 100, MH table 81)
#############################################################################################
#subset to columns needed for analysis
item93.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                    ,"INTRVW_CUST_RES_HomeandEnergyUseHome_DishwasherLoadsPerWeek"
                                                                                    ,""))])
colnames(item93.dat) <- c("CK_Cadmus_ID", "Dishwashes.Per.Week")
item93.dat$count <- 1

item93.dat0 <- item93.dat[which(item93.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item93.dat1 <- left_join(rbsa.dat, item93.dat0, by = "CK_Cadmus_ID")

item93.dat2 <- item93.dat1[which(!(is.na(item93.dat1$Dishwashes.Per.Week))),]
item93.dat2$Dishwashes.Per.Week <- as.numeric(as.character(item93.dat2$Dishwashes.Per.Week))

# Weighting
item93.data <- weightedData(item93.dat2[-which(colnames(item93.dat2) %in% c("Dishwashes.Per.Week"
                                                                            ,"count"))])

item93.data <- left_join(item93.data, item93.dat2[which(colnames(item93.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"Dishwashes.Per.Week"
                                                                                     ,"count"))])

###############################
# Weighted Analysis
###############################
item93.final <- mean_one_group(CustomerLevelData = item93.data
                               ,valueVariable = 'Dishwashes.Per.Week' 
                               ,byVariable    = 'State'
                               ,aggregateRow  = "Region")


unique(item93.final$State)
rowOrder <- c("ID"
              ,"MT"
              ,"OR"
              ,"WA"
              ,"Region")
item93.final <- item93.final %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
item93.final <- data.frame(item93.final)


# Export table
item93.final.SF <- item93.final[which(item93.final$BuildingType == "Single Family")
                                ,-which(colnames(item93.final) %in% c("BuildingType"))]
item93.final.MH <- item93.final[which(item93.final$BuildingType == "Manufactured")
                                ,-which(colnames(item93.final) %in% c("BuildingType"))]

# exportTable(item93.final.SF, "SF", "Table 100", weighted = TRUE)
exportTable(item93.final.MH, "MH", "Table 81", weighted = TRUE)

###############################
# Unweighted Analysis
###############################
item93.final <- mean_one_group_unweighted(CustomerLevelData = item93.data
                               ,valueVariable = 'Dishwashes.Per.Week' 
                               ,byVariable    = 'State'
                               ,aggregateRow  = "Region")

unique(item93.final$State)
rowOrder <- c("ID"
              ,"MT"
              ,"OR"
              ,"WA"
              ,"Region")
item93.final <- item93.final %>% mutate(State = factor(State, levels = rowOrder)) %>% arrange(State)  
item93.final <- data.frame(item93.final)


# Export table
item93.final.SF <- item93.final[which(item93.final$BuildingType == "Single Family")
                                ,-which(colnames(item93.final) %in% c("BuildingType"))]
item93.final.MH <- item93.final[which(item93.final$BuildingType == "Manufactured")
                                ,-which(colnames(item93.final) %in% c("BuildingType"))]

# exportTable(item93.final.SF, "SF", "Table 100", weighted = FALSE)
exportTable(item93.final.MH, "MH", "Table 81", weighted = FALSE)






















############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
scl.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.scl.data", rundate, ".xlsx", sep = "")))
length(unique(scl.dat$CK_Cadmus_ID))
scl.dat$CK_Building_ID <- scl.dat$Category
scl.dat <- scl.dat[which(names(scl.dat) != "Category")]

#############################################################################################
#Item 89: AVERAGE NUMBER OF CLOTHES WASHER LOADS PER WEEK BY CK_Building_ID (SF table 96, MH table 77)
#############################################################################################
#subset to columns needed for analysis
item89.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                    ,"INTRVW_CUST_RES_HomeandEnergyUseHome_ClothesWasherLoadsPerWeek"
                                                                                    ,""))])
colnames(item89.os.dat) <- c("CK_Cadmus_ID", "Clothes.Washes.Per.Week")
item89.os.dat$count <- 1

item89.os.dat0 <- unique(item89.os.dat)

item89.os.dat1 <- left_join(scl.dat, item89.os.dat0, by = "CK_Cadmus_ID")

unique(item89.os.dat1$Clothes.Washes.Per.Week)
item89.os.dat1$Clothes.Washes.Per.Week[which(item89.os.dat1$Clothes.Washes.Per.Week %in% c("No Washing Machine", NA))] <- 0
item89.os.dat1$Clothes.Washes.Per.Week <- as.numeric(as.character(item89.os.dat1$Clothes.Washes.Per.Week))


# Weighting
item89.os.data <- weightedData(item89.os.dat1[-which(colnames(item89.os.dat1) %in% c("Clothes.Washes.Per.Week"
                                                                            ,"count"))])

item89.os.data <- left_join(item89.os.data, unique(item89.os.dat1[which(colnames(item89.os.dat1) %in% c("CK_Cadmus_ID"
                                                                                     ,"Clothes.Washes.Per.Week"
                                                                                     ,"count"))]))

###############################
# Weighted Analysis
###############################
item89.os.final <- mean_one_group(CustomerLevelData = item89.os.data
                               ,valueVariable = 'Clothes.Washes.Per.Week' 
                               ,byVariable    = 'CK_Building_ID'
                               ,aggregateRow  = "Remove")
item89.os.final <- item89.os.final[which(item89.os.final$CK_Building_ID != "Remove"),]
item89.os.final.SF <- item89.os.final[which(item89.os.final$BuildingType == "Single Family"),-1]

exportTable(item89.os.final.SF, "SF", "Table 96", weighted = TRUE, osIndicator = "SCL", OS = T)

###############################
# Unweighted Analysis
###############################

item89.os.final <- mean_one_group_unweighted(CustomerLevelData = item89.os.data
                                          ,valueVariable = 'Clothes.Washes.Per.Week' 
                                          ,byVariable    = 'CK_Building_ID'
                                          ,aggregateRow  = "Remove")
item89.os.final <- item89.os.final[which(item89.os.final$CK_Building_ID != "Remove"),]

item89.os.final.SF <- item89.os.final[which(item89.os.final$BuildingType == "Single Family"),-1]

exportTable(item89.os.final.SF, "SF", "Table 96", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Table AJ: AVERAGE CLOTHES WASHER SIZE (CU. FT.) BY CK_Building_ID
#############################################################################################
#subset to columns needed for analysis
tableAJ.os.dat <- unique(appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                           ,"Washer.Size"
                                                                           ,""))])

tableAJ.os.dat0 <- tableAJ.os.dat[which(!is.na(tableAJ.os.dat$Washer.Size)),]
tableAJ.os.dat1 <- tableAJ.os.dat0[-grep("unknown|-- Datapoint not asked for --|Datapoint not asked for|N/A",tableAJ.os.dat0$Washer.Size, ignore.case = T),]
tableAJ.os.dat1$Washer.Size <- gsub(" cu ft| u ft","",tableAJ.os.dat1$Washer.Size)
tableAJ.os.dat1$Washer.Size <- as.numeric(as.character(tableAJ.os.dat1$Washer.Size))
unique(tableAJ.os.dat1$Washer.Size)

tableAJ.os.dat2 <- left_join(scl.dat, tableAJ.os.dat1, by = "CK_Cadmus_ID")
tableAJ.os.dat2$CK_Cadmus_ID[which(duplicated(tableAJ.os.dat2$CK_Cadmus_ID))]

# Weighting
tableAJ.os.data <- weightedData(tableAJ.os.dat2[-which(colnames(tableAJ.os.dat2) %in% c("Washer.Size"))])

tableAJ.os.data <- left_join(tableAJ.os.data, unique(tableAJ.os.dat2[which(colnames(tableAJ.os.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Washer.Size"))]))

tableAJ.os.data$count <- 1
###############################
# Weighted Analysis
###############################
tableAJ.os.final <- mean_one_group(CustomerLevelData = tableAJ.os.data
                                ,valueVariable = 'Washer.Size' 
                                ,byVariable    = 'CK_Building_ID'
                                ,aggregateRow  = "Remove")
tableAJ.os.final <- tableAJ.os.final[which(tableAJ.os.final$CK_Building_ID != "Remove"),]

tableAJ.os.final.SF <- tableAJ.os.final[which(tableAJ.os.final$BuildingType == "Single Family"),-1]

exportTable(tableAJ.os.final.SF, "SF", "Table AJ", weighted = TRUE, osIndicator = "SCL", OS = T)

###############################
# Unweighted Analysis
###############################
tableAJ.os.final <- mean_one_group_unweighted(CustomerLevelData = tableAJ.os.data
                                           ,valueVariable = 'Washer.Size' 
                                           ,byVariable    = 'CK_Building_ID'
                                           ,aggregateRow  = "Remove")
tableAJ.os.final <- tableAJ.os.final[which(tableAJ.os.final$CK_Building_ID != "Remove"),]

tableAJ.os.final.SF <- tableAJ.os.final[which(tableAJ.os.final$BuildingType == "Single Family"),-1]

exportTable(tableAJ.os.final.SF, "SF", "Table AJ", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Item 91: AVERAGE NUMBER OF CLOTHES WASHER LOADS PER WEEK BY CK_Building_ID (SF table 98, MH table 79)
#############################################################################################
#subset to columns needed for analysis
item91.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                    ,"INTRVW_CUST_RES_HomeandEnergyUseHome_ClothesWasherLoadsPerWeek"
                                                                                    ,"INTRVW_CUST_RES_HomeandEnergyUseHome_PercentOfLoadsThatGoInDryer"))])
colnames(item91.os.dat) <- c("CK_Cadmus_ID", "Clothes.Washes.Per.Week", "Percent.Loads.Go.In.Dryer")
item91.os.dat$count <- 1

item91.os.dat0 <- item91.os.dat[which(item91.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item91.os.dat1 <- left_join(scl.dat, item91.os.dat0, by = "CK_Cadmus_ID")

item91.os.dat2 <- item91.os.dat1[which(item91.os.dat1$Clothes.Washes.Per.Week != "No Washing Machine"),]
item91.os.dat2$Clothes.Washes.Per.Week   <- as.numeric(as.character(item91.os.dat2$Clothes.Washes.Per.Week))
item91.os.dat2$Percent.Loads.Go.In.Dryer <- as.numeric(as.character(item91.os.dat2$Percent.Loads.Go.In.Dryer))
item91.os.dat2$Dryer.Loads.Per.Week <- item91.os.dat2$Clothes.Washes.Per.Week * (item91.os.dat2$Percent.Loads.Go.In.Dryer / 100)

item91.os.dat2$Dryer.Loads.Per.Wash <- item91.os.dat2$Dryer.Loads.Per.Week / item91.os.dat2$Clothes.Washes.Per.Week
unique(item91.os.dat2$Dryer.Loads.Per.Wash)

item91.os.dat3 <- item91.os.dat2[which(item91.os.dat2$Dryer.Loads.Per.Wash %notin% c("NaN", NA,"N/A")),]

################################################
# Adding pop and sample sizes for weights
################################################
item91.os.data <- weightedData(item91.os.dat3[-which(colnames(item91.os.dat3) %in% c("Clothes.Washes.Per.Week"
                                                                            ,"Percent.Loads.Go.In.Dryer"
                                                                            ,"count"
                                                                            ,"Dryer.Loads.Per.Week"
                                                                            ,"Dryer.Loads.Per.Wash"))])
item91.os.data <- left_join(item91.os.data, item91.os.dat3[which(colnames(item91.os.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"Clothes.Washes.Per.Week"
                                                                                     ,"Percent.Loads.Go.In.Dryer"
                                                                                     ,"count"
                                                                                     ,"Dryer.Loads.Per.Week"
                                                                                     ,"Dryer.Loads.Per.Wash"))])
item91.os.data$count <- 1

#######################
# Weighted Analysis
#######################
item91.os.final <- mean_one_group(CustomerLevelData = item91.os.data
                               ,valueVariable = 'Dryer.Loads.Per.Wash'
                               ,byVariable = 'CK_Building_ID'
                               ,aggregateRow = "Remove")
item91.os.final <- item91.os.final[which(item91.os.final$CK_Building_ID != "Remove"),]

item91.os.final.SF <- item91.os.final[which(item91.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item91.os.final) %in% c("BuildingType"))]

exportTable(item91.os.final.SF, "SF", "Table 98", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item91.os.final <- mean_one_group_unweighted(CustomerLevelData = item91.os.data
                                          ,valueVariable = 'Dryer.Loads.Per.Wash'
                                          ,byVariable = 'CK_Building_ID'
                                          ,aggregateRow = "Remove")
item91.os.final <- item91.os.final[which(item91.os.final$CK_Building_ID != "Remove"),]

item91.os.final.SF <- item91.os.final[which(item91.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item91.os.final) %in% c("BuildingType"))]

exportTable(item91.os.final.SF, "SF", "Table 98", weighted = FALSE, osIndicator = "SCL", OS = T)



#############################################################################################
#Item 93: AVERAGE NUMBER OF DISHWASHER LOADS PER WEEK (SF table 100, MH table 81)
#############################################################################################
#subset to columns needed for analysis
item93.os.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                    ,"INTRVW_CUST_RES_HomeandEnergyUseHome_DishwasherLoadsPerWeek"
                                                                                    ,""))])
colnames(item93.os.dat) <- c("CK_Cadmus_ID", "Dishwashes.Per.Week")
item93.os.dat$count <- 1

item93.os.dat0 <- item93.os.dat[which(item93.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item93.os.dat1 <- left_join(scl.dat, item93.os.dat0, by = "CK_Cadmus_ID")

item93.os.dat2 <- item93.os.dat1[which(!(is.na(item93.os.dat1$Dishwashes.Per.Week))),]
item93.os.dat2$Dishwashes.Per.Week <- as.numeric(as.character(item93.os.dat2$Dishwashes.Per.Week))

# Weighting
item93.os.data <- weightedData(item93.os.dat2[-which(colnames(item93.os.dat2) %in% c("Dishwashes.Per.Week"
                                                                            ,"count"))])

item93.os.data <- left_join(item93.os.data, unique(item93.os.dat2[which(colnames(item93.os.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"Dishwashes.Per.Week"
                                                                                     ,"count"))]))

###############################
# Weighted Analysis
###############################
item93.os.final <- mean_one_group(CustomerLevelData = item93.os.data
                               ,valueVariable = 'Dishwashes.Per.Week' 
                               ,byVariable    = 'CK_Building_ID'
                               ,aggregateRow  = "Remove")
item93.os.final <- item93.os.final[which(item93.os.final$CK_Building_ID != "Remove"),]

item93.os.final.SF <- item93.os.final[which(item93.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item93.os.final) %in% c("BuildingType"))]

exportTable(item93.os.final.SF, "SF", "Table 100", weighted = TRUE, osIndicator = "SCL", OS = T)

###############################
# Unweighted Analysis
###############################
item93.os.final <- mean_one_group_unweighted(CustomerLevelData = item93.os.data
                                          ,valueVariable = 'Dishwashes.Per.Week' 
                                          ,byVariable    = 'CK_Building_ID'
                                          ,aggregateRow  = "Remove")
item93.os.final <- item93.os.final[which(item93.os.final$CK_Building_ID != "Remove"),]

item93.os.final.SF <- item93.os.final[which(item93.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item93.os.final) %in% c("BuildingType"))]

exportTable(item93.os.final.SF, "SF", "Table 100", weighted = FALSE, osIndicator = "SCL", OS = T)
