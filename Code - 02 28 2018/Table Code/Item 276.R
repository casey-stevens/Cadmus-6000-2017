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
rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
#clean cadmus IDs
rooms.dat$CK_Cadmus_ID <- trimws(toupper(rooms.dat$CK_Cadmus_ID))






#############################################################################################
#Item 276: AVERAGE NUMBER OF KITCHEN FACILITIES BY BUILDING SIZE (MF Table 68)
#############################################################################################
item276.dat <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID"
                                                          ,"CK_SiteID"
                                                          ,"Clean.Type"))]

item276.dat0 <- item276.dat[grep("BLDG",item276.dat$CK_SiteID),]

item276.dat00 <- item276.dat0[which(item276.dat0$Clean.Type == "Kitchen"),]

#merge on rooms data with rbsa cleaned data
item276.dat1 <- left_join(rbsa.dat, item276.dat00, by = c("CK_Building_ID" = "CK_SiteID"))
colnames(item276.dat1)[which(colnames(item276.dat1) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"

#subset to only multifamily units
item276.dat2 <- item276.dat1[grep("Multifamily",item276.dat1$BuildingType),]
item276.dat2$Ind <- 0
item276.dat2$Ind[which(item276.dat2$Clean.Type == "Kitchen")] <- 1

######################################
#Pop and Sample Sizes for weights
######################################
item276.data <- weightedData(item276.dat2[which(colnames(item276.dat2) %notin% c("CK_Cadmus_ID.y"
                                                                                 ,"Clean.Type"
                                                                                 ,"Ind"))])

item276.data <- left_join(item276.data, item276.dat2[which(colnames(item276.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Clean.Type"
                                                                                         ,"Ind"))])
item276.data$count <- 1


######################
# weighted analysis
######################
item276.final <- mean_one_group(CustomerLevelData = item276.data
                                ,valueVariable = 'Ind'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = 'All Sizes')
item276.final <- item276.final[which(names(item276.final) != "BuildingType")]
exportTable(item276.final, "MF", "Table 68", weighted = TRUE)


######################
# weighted analysis
######################
item276.final <- mean_one_group_unweighted(CustomerLevelData = item276.data
                                ,valueVariable = 'Ind'
                                ,byVariable = 'HomeType'
                                ,aggregateRow = 'All Sizes')
item276.final <- item276.final[which(names(item276.final) != "BuildingType")]

exportTable(item276.final, "MF", "Table 68", weighted = FALSE)
