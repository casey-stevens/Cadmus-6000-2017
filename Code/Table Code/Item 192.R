#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list=ls())
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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) 

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
#Item 192: CROSSOVER DUCT CONDITION IN MULTI-SECTION HOMES (MH TABLE 46)
#############################################################################################
#subset to columns needed for analysis
item192.dat <- unique(mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                           ,"MECH_Ducting_DUCTS_CrossoverCondition"
                                                                           ,"MECH_Ducting_DUCTS_DuctCrossoverPresent_Y_N"
                                                                           , ""))])

#remove any repeat header rows from exporting
item192.dat0 <- item192.dat[which(item192.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item192.dat1 <- left_join(rbsa.dat, item192.dat0)

#subset to only crossover present == Yes or No
unique(item192.dat2$MECH_Ducting_DUCTS_DuctCrossoverPresent_Y_N)
item192.dat2 <- item192.dat1[which(item192.dat1$MECH_Ducting_DUCTS_DuctCrossoverPresent_Y_N %in% c("Yes", "No")),]

item192.dat2$DuctCount <- 0
item192.dat2$DuctCount[which(item192.dat2$MECH_Ducting_DUCTS_DuctCrossoverPresent_Y_N == "Yes")] <- 1
unique(item192.dat2$MECH_Ducting_DUCTS_CrossoverCondition)

item192.merge <- item192.dat2[which(item192.dat2$MECH_Ducting_DUCTS_CrossoverCondition %notin% c("Unknown", NA, "N/A")),]
item192.merge <- item192.merge[which(item192.merge$BuildingType == "Manufactured"),]

##########################################
# add pop and sample sizes by strata
##########################################
item192.data <- weightedData(item192.merge[-which(colnames(item192.merge) %in% c("MECH_Ducting_DUCTS_CrossoverCondition"
                                                                                 ,"MECH_Ducting_DUCTS_DuctCrossoverPresent_Y_N"
                                                                                 ,"DuctCount"))])
item192.data <- left_join(item192.data, item192.merge[which(colnames(item192.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"MECH_Ducting_DUCTS_CrossoverCondition"
                                                                                           ,"MECH_Ducting_DUCTS_DuctCrossoverPresent_Y_N"
                                                                                           ,"DuctCount"))])
item192.data$count <- 1
##############################
# Weighted Analysis
##############################
item192.summary <- proportionRowsAndColumns1(CustomerLevelData     = item192.data
                                             , valueVariable       = 'DuctCount'
                                             , columnVariable      = 'HomeType'
                                             , rowVariable         = 'MECH_Ducting_DUCTS_CrossoverCondition'
                                             , aggregateColumnName = "Total"
)
item192.summary <- item192.summary[which(item192.summary$HomeType != "Total"),]
item192.summary <- item192.summary[which(item192.summary$MECH_Ducting_DUCTS_CrossoverCondition != "Total"),]

## Summary only for "All Vintages"
item192.all.types <- proportions_one_group(item192.data
                                              ,valueVariable    = "DuctCount"
                                              ,groupingVariable = "MECH_Ducting_DUCTS_CrossoverCondition"
                                              ,total.name       = "All Types"
                                              ,columnName       = "HomeType"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)



#merge together!
item192.final <- rbind.data.frame(item192.summary
                                  , item192.all.types
                                  , stringsAsFactors = F)

item192.cast <- dcast(setDT(item192.final),
                      formula   = BuildingType + HomeType ~ MECH_Ducting_DUCTS_CrossoverCondition,
                      value.var = c("w.percent", "w.SE", "count", "n", "N"))

item192.table <- data.frame("BuildingType"       = item192.cast$BuildingType
                            ,"Home.Type"          = item192.cast$HomeType
                            ,"Percent.Connected" = item192.cast$w.percent_Connected
                            ,"SE.Connected"      = item192.cast$w.SE_Connected
                            ,"Count.Connected"   = item192.cast$count_Connected
                            ,"Percent.Partially.Connected" = NA #item192.cast$w.percent_Partially.Connected
                            ,"SE.Partially.Connected"      = NA #item192.cast$w.SE_Partially.Connected
                            ,"Count.Partially.Connected"   = NA #item192.cast$count_Partially.Connected
                            ,"Percent.Disconnected" = NA #item192.cast$w.percent_Disconnected
                            ,"SE.Disconnected"      = NA #item192.cast$w.SE_Disconnected
                            ,"Count.Disconnected"   = NA #item192.cast$count_Disconnected
                            )

# row ordering example code
levels(item192.table$Home.Type)
rowOrder <- c("Single Family Detached"
              ,"Duplex, Triplex, or Fourplex"
              ,"Townhome or Rowhome"
              ,"Apartment Building (3 or fewer floors)"
              ,"Apartment Building (4 to 6 floors)"
              ,"Apartment Building (More than 6 floors)"
              ,"Single Wide"
              ,"Double Wide"
              ,"Triple Wide"
              ,"Modular / Prefab"
              ,"Total"
              ,"All Types")
item192.table <- item192.table %>% mutate(Home.Type = factor(Home.Type, levels = rowOrder)) %>% arrange(Home.Type)  
item192.table <- data.frame(item192.table)


item192.table.MH <- item192.table[which(item192.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item192.table.MH, "MH", "Table 46", weighted = TRUE)

############################################################################################################
# Unweighted - Manufactured
############################################################################################################
item192.summary <- proportions_two_groups_unweighted(CustomerLevelData     = item192.data
                                             , valueVariable       = 'DuctCount'
                                             , columnVariable      = 'HomeType'
                                             , rowVariable         = 'MECH_Ducting_DUCTS_CrossoverCondition'
                                             , aggregateColumnName = "Total"
)
item192.summary <- item192.summary[which(item192.summary$HomeType != "Total"),]
item192.summary <- item192.summary[which(item192.summary$MECH_Ducting_DUCTS_CrossoverCondition != "Total"),]

## Summary only for "All Vintages"
item192.all.types <- proportions_one_group(item192.data
                                           ,valueVariable    = "DuctCount"
                                           ,groupingVariable = "MECH_Ducting_DUCTS_CrossoverCondition"
                                           ,total.name       = "All Types"
                                           ,columnName       = "HomeType"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)
item192.all.types$HomeType[which(item192.all.types$HomeType == "Total")] <- "All Types"



#merge together!
item192.final <- rbind.data.frame(item192.summary
                                  , item192.all.types
                                  , stringsAsFactors = F)

item192.cast <- dcast(setDT(item192.final),
                      formula   = BuildingType + HomeType ~ MECH_Ducting_DUCTS_CrossoverCondition,
                      value.var = c("Percent", "SE", "Count", "SampleSize"))

item192.table <- data.frame("BuildingType"       = item192.cast$BuildingType
                            ,"Home.Type"          = item192.cast$HomeType
                            ,"Percent.Connected" = item192.cast$Percent_Connected
                            ,"SE.Connected"      = item192.cast$SE_Connected
                            ,"Count.Connected"   = item192.cast$Count_Connected
                            ,"Percent.Partially.Connected" = NA #item192.cast$w.percent_Partially.Connected
                            ,"SE.Partially.Connected"      = NA #item192.cast$w.SE_Partially.Connected
                            ,"Count.Partially.Connected"   = NA #item192.cast$count_Partially.Connected
                            ,"Percent.Disconnected" = NA #item192.cast$w.percent_Disconnected
                            ,"SE.Disconnected"      = NA #item192.cast$w.SE_Disconnected
                            ,"Count.Disconnected"   = NA #item192.cast$count_Disconnected
)

# row ordering example code
levels(item192.table$Home.Type)
rowOrder <- c("Single Family Detached"
              ,"Duplex, Triplex, or Fourplex"
              ,"Townhome or Rowhome"
              ,"Apartment Building (3 or fewer floors)"
              ,"Apartment Building (4 to 6 floors)"
              ,"Apartment Building (More than 6 floors)"
              ,"Single Wide"
              ,"Double Wide"
              ,"Triple Wide"
              ,"Modular / Prefab"
              ,"Total"
              ,"All Types")
item192.table <- item192.table %>% mutate(Home.Type = factor(Home.Type, levels = rowOrder)) %>% arrange(Home.Type)  
item192.table <- data.frame(item192.table)


item192.table.MH <- item192.table[which(item192.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item192.table.MH, "MH", "Table 46", weighted = FALSE)
