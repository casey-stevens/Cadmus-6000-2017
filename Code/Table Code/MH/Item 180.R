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
# windows.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, windows.export))
#clean cadmus IDs
windows.dat$CK_Cadmus_ID <- trimws(toupper(windows.dat$CK_Cadmus_ID))



#############################################################################################
# Item 180: DISTRIBUTION OF WINDOW U-VALUE BY STATE (MH table 23)
#############################################################################################
item180.dat <- windows.dat[which(names(windows.dat) %in% c("CK_Cadmus_ID"
                                                           ,"U-Value"))]

item180.merge <- left_join(rbsa.dat, item180.dat)

item180.MH <- item180.merge[which(item180.merge$BuildingType == "Manufactured"),]
item180.MH$`U-Value` <- as.numeric(as.character(item180.MH$`U-Value`))

item180.dat2 <- item180.MH[which(!is.na(item180.MH$`U-Value`)),]

item180.customer <- summarise(group_by(item180.dat2, CK_Cadmus_ID)
                              ,U.Value = mean(`U-Value`))

item180.merge <- left_join(rbsa.dat, item180.customer)
item180.merge <- item180.merge[which(!is.na(item180.merge$U.Value)),]

######################################
#Pop and Sample Sizes for weights
######################################
item180.data <- weightedData(item180.merge[which(colnames(item180.merge) %notin% c("U.Value"))])

item180.data <- left_join(item180.data, item180.merge[which(colnames(item180.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"U.Value"))])
item180.data$count <- 1


#########################
# weighted analysis
#########################
item180.final <- mean_one_group(CustomerLevelData = item180.data
                                ,valueVariable = 'U.Value'
                                ,byVariable = 'State'
                                ,aggregateRow = "Region")

item180.final.MH <- item180.final[which(names(item180.final) != "BuildingType")]

exportTable(item180.final.MH, "MH", "Table 23", weighted = TRUE)

#########################
# weighted analysis
#########################
item180.final <- mean_one_group_unweighted(CustomerLevelData = item180.data
                                ,valueVariable = 'U.Value'
                                ,byVariable = 'State'
                                ,aggregateRow = "Region")

item180.final.MH <- item180.final[which(names(item180.final) != "BuildingType")]

exportTable(item180.final.MH, "MH", "Table 23", weighted = FALSE)