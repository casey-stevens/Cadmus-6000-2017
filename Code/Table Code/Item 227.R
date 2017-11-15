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
#Item 227: AVERAGE NUMBER OF OCCUPANTS BY AGE CATEGORY (MF Table 19)
#############################################################################################

item227.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_LessThan1"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_1_5"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_6_10"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_11_18"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_19_45"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_46_64"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_65Older"
                                                                                     ,""))])
colnames(item227.dat) <- c("CK_Cadmus_ID"
                           ,"Age_1_5"
                           ,"Age_11_18"
                           ,"Age_19_45"
                           ,"Age_46_64"
                           ,"Age_6_10"
                           ,"Age_65_Older"
                           ,"Age_Less_Than_1"
                           )

item227.dat$count <- 1

#remove any repeat header rows from exporting
item227.dat0 <- item227.dat[which(item227.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item227.dat0$Age_0_18 <- item227.dat0$Age_Less_Than_1 + item227.dat0$Age_1_5 + item227.dat0$Age_6_10 + item227.dat0$Age_11_18
item227.dat0$Age_19_64 <- item227.dat0$Age_19_45 + item227.dat0$Age_46_64
item227.dat0$AllCategories <- item227.dat0$Age_0_18 + item227.dat0$Age_19_64 + item227.dat0$Age_65_Older

item227.dat0 <- item227.dat0[which(colnames(item227.dat0) %in% c("CK_Cadmus_ID"
                                                                 ,"Age_0_18"
                                                                 ,"Age_19_64"
                                                                 ,"Age_65_Older"
                                                                 ,"AllCategories"))]

item227.melt <- melt(item227.dat0, id = c("CK_Cadmus_ID"))
colnames(item227.melt) <- c("CK_Cadmus_ID", "Age.Category", "Number.of.Residents")
item227.melt$CK_Cadmus_ID <- as.character(item227.melt$CK_Cadmus_ID)

item227.merge <- left_join(rbsa.dat, item227.melt)

#Subset to Multifamily
item227.merge <- item227.merge[grep("Multifamily", item227.merge$BuildingType),]

#remove missing
item227.merge <- item227.merge[which(!is.na(item227.merge$Number.of.Residents)),]
item227.merge <- item227.merge[which(!is.na(item227.merge$Age.Category)),]

################################################
# Adding pop and sample sizes for weights
################################################
item227.data <- weightedData(item227.merge[-which(colnames(item227.merge) %in% c("Age.Category"
                                                                                 ,"Number.of.Residents"))])
item227.data <- left_join(item227.data, item227.merge[which(colnames(item227.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Age.Category"
                                                                                           ,"Number.of.Residents"))])


item227.data$Number.of.Residents <- as.numeric(as.character(item227.data$Number.of.Residents))
item227.data$count <- 1
item227.data$count[which(item227.data$Number.of.Residents == 0)] <- 0

#######################
# weighted analysis
#######################

item227.final <- mean_one_group(CustomerLevelData = item227.data
                                ,valueVariable = 'Number.of.Residents'
                                ,byVariable = 'Age.Category'
                                ,aggregateRow = NA)
item227.final <- item227.final[which(!is.na(item227.final$Age.Category)),]

exportTable(item227.final, "MF", "Table 19", weighted = TRUE)

#######################
# unweighted analysis
#######################

item227.final <- mean_one_group_unweighted(CustomerLevelData = item227.data
                                ,valueVariable = 'Number.of.Residents'
                                ,byVariable = 'Age.Category'
                                ,aggregateRow = NA)
item227.final <- item227.final[which(!is.na(item227.final$Age.Category)),]

item227.final.MF <- item227.final[which(colnames(item227.final) %notin% c("BuidingType", "n"))]
exportTable(item227.final.MF, "MF", "Table 19", weighted = FALSE)

