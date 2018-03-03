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
#Item 299: DISTRIBUTION OF IN-UNIT DISHWASHERS BY APPLIANCE VINTAGE (MF Table 93)
#############################################################################################
#subset to columns needed for analysis
item299.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"Age"
                                                                    ,"Type"
                                                                    ,""))]

#join clean rbsa data onto appliances analysis data
item299.dat1 <- left_join(rbsa.dat, item299.dat, by = "CK_Cadmus_ID")

#subset to only MF
item299.dat2 <- item299.dat1[grep("Multifamily", item299.dat1$BuildingType),]

#subset to only Dishwashers
item299.dat3 <- item299.dat2[which(item299.dat2$Type %in% c("Dishwasher")),]
item299.dat3$Age <- as.numeric(as.character(item299.dat3$Age))

# If missing AGE, make Unknown
item299.dat3$Age[which(item299.dat3$Age %in% c(NA, 0))] <- "Unknown"

item299.tmp <- item299.dat3[which(colnames(item299.dat3) %in% c("CK_Cadmus_ID", "Age"))]
colnames(item299.tmp) <- c("CK_Cadmus_ID", "Dishwasher.Age")

#merge back on with cleaned RBSA data to observe how many MF do not have dishwashers
item299.dat4 <- left_join(rbsa.dat, item299.tmp)
item299.dat4 <- item299.dat4[grep("bldg",item299.dat4$CK_Building_ID, ignore.case = T),]

#subset to only MF
item299.dat5 <- item299.dat4[grep("Multifamily", item299.dat4$BuildingType),]
#create "None" category by identifying which of the MF sites did not have dishwasher info merged on
item299.dat5$Dishwasher.Age[which(is.na(item299.dat5$Dishwasher.Age))] <- "None"
unique(item299.dat5$Dishwasher.Age)

####################
# Clean AGE
####################
item299.dat5$Age.Num <- as.numeric(as.character(item299.dat5$Dishwasher.Age))
item299.dat5$Dishwasher.Cat <- item299.dat5$Dishwasher.Age
item299.dat5$Dishwasher.Cat[which(item299.dat5$Dishwasher.Age == "Unknown")] <- "Unknown"
item299.dat5$Dishwasher.Cat[which(item299.dat5$Dishwasher.Age == "None")] <- "None"
item299.dat5$Dishwasher.Cat[which(item299.dat5$Age.Num < 1980)] <- "Pre 1980" ###################################Double check this doesn't overwrite Unknown or None
item299.dat5$Dishwasher.Cat[which(item299.dat5$Age.Num >= 1980 & item299.dat5$Age.Num < 1990)] <- "1980-1989"
item299.dat5$Dishwasher.Cat[which(item299.dat5$Age.Num >= 1990 & item299.dat5$Age.Num < 1995)] <- "1990-1994"
item299.dat5$Dishwasher.Cat[which(item299.dat5$Age.Num >= 1995 & item299.dat5$Age.Num < 2000)] <- "1995-1999"
item299.dat5$Dishwasher.Cat[which(item299.dat5$Age.Num >= 2000 & item299.dat5$Age.Num < 2005)] <- "2000-2004"
item299.dat5$Dishwasher.Cat[which(item299.dat5$Age.Num >= 2005 & item299.dat5$Age.Num < 2010)] <- "2005-2009"
item299.dat5$Dishwasher.Cat[which(item299.dat5$Age.Num >= 2010 & item299.dat5$Age.Num < 2015)] <- "2010-2014"
item299.dat5$Dishwasher.Cat[which(item299.dat5$Age.Num >= 2015)] <- "Post 2014"
unique(item299.dat5$Dishwasher.Cat)
####################
# end cleaning
####################

#call new data
item299.dat6 <- item299.dat5

#add counter
item299.dat6$count <- 1

######################################
#Pop and Sample Sizes for weights
######################################
item299.data <- weightedData(item299.dat6[which(colnames(item299.dat6) %notin% c("Dishwasher.Age"
                                                                                 ,"Age.Num"
                                                                                 ,"Dishwasher.Cat"
                                                                                 ,"count"
                                                                                 ,""))])

item299.data <- left_join(item299.data, item299.dat6[which(colnames(item299.dat6) %in% c("CK_Cadmus_ID"
                                                                                         ,"Dishwasher.Age"
                                                                                         ,"Age.Num"
                                                                                         ,"Dishwasher.Cat"
                                                                                         ,"count"))])
item299.data$count <- 1


######################
# weighted analysis
######################
item299.final <- proportions_one_group(CustomerLevelData = item299.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'Dishwasher.Cat'
                                          ,total.name = 'Remove')
# item299.final <- item299.final[which(item299.final$Dishwasher.Cat != "Total"),]
item299.final$Dishwasher.Cat <- as.factor(item299.final$Dishwasher.Cat)

levels(item299.final$Dishwasher.Cat)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"None"
              ,"Unknown"
              ,"Total")
item299.final <- item299.final %>% mutate(Dishwasher.Cat = factor(Dishwasher.Cat, levels = rowOrder)) %>% arrange(Dishwasher.Cat)  
item299.final <- data.frame(item299.final)


exportTable(item299.final, "MF", "Table 93", weighted = TRUE)

######################
# unweighted analysis
######################
item299.final <- proportions_one_group(CustomerLevelData = item299.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'Dishwasher.Cat'
                                          ,total.name = 'Remove'
                                          ,weighted = FALSE)
# item299.final <- item299.final[which(item299.final$Dishwasher.Cat != "Total"),]

levels(item299.final$Dishwasher.Cat)
rowOrder <- c("Pre 1980"
              ,"1980-1989"
              ,"1990-1994"
              ,"1995-1999"
              ,"2000-2004"
              ,"2005-2009"
              ,"2010-2014"
              ,"Post 2014"
              ,"None"
              ,"Unknown"
              ,"Total")
item299.final <- item299.final %>% mutate(Dishwasher.Cat = factor(Dishwasher.Cat, levels = rowOrder)) %>% arrange(Dishwasher.Cat)  
item299.final <- data.frame(item299.final)

exportTable(item299.final, "MF", "Table 93", weighted = FALSE)
