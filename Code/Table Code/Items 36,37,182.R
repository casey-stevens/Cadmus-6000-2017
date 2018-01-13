#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:          09/11/2017                                  
##  Billing Code(s):  6596.0000.0002.1002.0000
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


#############################################################################################
#Item 36: AVERAGE NORMALIZED HEAT-LOSS RATE BY VINTAGE AND STATE (SF table 43, MH table 24)
#############################################################################################




################################################
# Adding pop and sample sizes for weights
################################################
item36.data <- weightedData(item36.merge[-which(colnames(item36.merge) %in% c(""))])
item36.data <- left_join(item36.data, item36.merge[which(colnames(item36.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,""))])
item36.data$count <- 1
item36.data$Count <- 1
#######################
# Weighted Analysis
#######################


#######################
# Unweighted Analysis
#######################

