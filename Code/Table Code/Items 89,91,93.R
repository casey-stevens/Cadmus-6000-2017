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

exportTable(item89.final.SF, "SF", "Table 96", weighted = TRUE)
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

exportTable(item89.final.SF, "SF", "Table 96", weighted = FALSE)
exportTable(item89.final.MH, "MH", "Table 77", weighted = FALSE)





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
item91.dat2$Dryer.Loads.Per.Week <- item91.dat2$Clothes.Washes.Per.Week * (item91.dat2$Percent.Loads.Go.In.Dryer / 100)

item91.dat2$Dryer.Loads.Per.Wash <- item91.dat2$Dryer.Loads.Per.Week / item91.dat2$Clothes.Washes.Per.Week
unique(item91.dat2$Dryer.Loads.Per.Wash)

item91.dat3 <- item91.dat2[which(item91.dat2$Dryer.Loads.Per.Wash %notin% c("NaN", NA)),]

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
                                ,-which(colnames(item91.final) %in% c("BuildingType"
                                                                      ,"Count"))]
item91.final.MH <- item91.final[which(item91.final$BuildingType == "Manufactured")
                                ,-which(colnames(item91.final) %in% c("BuildingType"
                                                                      ,"Count"))]

exportTable(item91.final.SF, "SF", "Table 98", weighted = TRUE)
exportTable(item91.final.MH, "MH", "Table 79", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item91.final <- mean_one_group_unweighted(CustomerLevelData = item91.data
                                          ,valueVariable = 'Dryer.Loads.Per.Wash'
                                          ,byVariable = 'State'
                                          ,aggregateRow = "Region")

item91.final.SF <- item91.final[which(item91.final$BuildingType == "Single Family")
                                ,-which(colnames(item91.final) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"count"))]
item91.final.MH <- item91.final[which(item91.final$BuildingType == "Manufactured")
                                ,-which(colnames(item91.final) %in% c("BuildingType"
                                                                      ,"Remove"
                                                                      ,"count"))]

exportTable(item91.final.SF, "SF", "Table 98", weighted = FALSE)
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

# Export table
item93.final.SF <- item93.final[which(item93.final$BuildingType == "Single Family")
                                ,-which(colnames(item93.final) %in% c("BuildingType"
                                                                      ,"Count"))]
item93.final.MH <- item93.final[which(item93.final$BuildingType == "Manufactured")
                                ,-which(colnames(item93.final) %in% c("BuildingType"
                                                                      ,"Count"))]

exportTable(item93.final.SF, "SF", "Table 100", weighted = TRUE)
exportTable(item93.final.MH, "MH", "Table 81", weighted = TRUE)

###############################
# Unweighted Analysis
###############################
item93.final <- mean_one_group_unweighted(CustomerLevelData = item93.data
                               ,valueVariable = 'Dishwashes.Per.Week' 
                               ,byVariable    = 'State'
                               ,aggregateRow  = "Region")

# Export table
item93.final.SF <- item93.final[which(item93.final$BuildingType == "Single Family")
                                ,-which(colnames(item93.final) %in% c("BuildingType"
                                                                      ,"count"))]
item93.final.MH <- item93.final[which(item93.final$BuildingType == "Manufactured")
                                ,-which(colnames(item93.final) %in% c("BuildingType"
                                                                      ,"count"))]

exportTable(item93.final.SF, "SF", "Table 100", weighted = FALSE)
exportTable(item93.final.MH, "MH", "Table 81", weighted = FALSE)

