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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))


#Read in data for analysis
# sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))



#############################################################################################
#Item 179: PERCENT OF HOMES WITH REPLACEMENT WINDOWS BY STATE (MH TABLE 22)
#############################################################################################
#subset to columns needed for analysis
item179.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_ConservationEffortsConversation_ReplacementWindows_Y_N"
                                                                                     ,""))])
colnames(item179.dat) <- c("CK_Cadmus_ID", "Replacement_Windows")

#remove any repeat header rows from exporting
item179.dat0 <- item179.dat[which(item179.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item179.dat1 <- left_join(rbsa.dat, item179.dat0, by = "CK_Cadmus_ID")
unique(item179.dat1$Replacement_Windows)
item179.dat2 <- item179.dat1

item179.dat2$Ind <- item179.dat2$Replacement_Windows
item179.dat2$Ind[which(item179.dat2$Replacement_Windows == "Yes")] <- 1
item179.dat2$Ind[which(item179.dat2$Replacement_Windows != "Yes")] <- 0
item179.dat2$Ind[which(is.na(item179.dat2$Ind))] <- 0

unique(item179.dat2$Ind)

item179.dat2$Ind <- as.numeric(as.character(item179.dat2$Ind))

item179.dat2$count <- 1


##########################################
# add pop and sample sizes by strata
##########################################
item179.data <- weightedData(item179.dat2[-which(colnames(item179.dat2) %in% c("Replacement_Windows"
                                                                               ,"Ind"
                                                                               ,"count"))])
item179.data <- left_join(item179.data, item179.dat2[which(colnames(item179.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Replacement_Windows"
                                                                                         ,"Ind"
                                                                                         ,"count"))])
item179.data$Count <- 1
##############################
# Weighted Analysis
##############################
item179.final <- proportions_one_group(CustomerLevelData = item179.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name       = "Region"
                                       ,weighted         = TRUE)
item179.final.MH <- item179.final[which(item179.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item179.final) %in% c("BuildingType"))]
exportTable(item179.final.MH, "MH", "Table 22", weighted = TRUE)

##############################
# Unweighted Analysis
##############################
item179.final <- proportions_one_group(CustomerLevelData = item179.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name       = "Region"
                                       ,weighted         = FALSE)
item179.final.MH <- item179.final[which(item179.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item179.final) %in% c("BuildingType"))]
exportTable(item179.final.MH, "MH", "Table 22", weighted = FALSE)
