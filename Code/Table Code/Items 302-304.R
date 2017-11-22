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
#subset to columns provided by Rietz -- this includes columns for all three items (302-304)
appliances.dat1 <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"CK_SiteID"
                                                                        ,"Type"
                                                                        ,"TV.Wattage"
                                                                        ,"Age"
                                                                        ,"TV.Screen.Type"
                                                                        ,"Room"
                                                                        ,"Clean.Room"
                                                                        ,""))]

#merge appliances data onto RBSA cleaned data
rbsa.appliances <- left_join(rbsa.dat, appliances.dat1, by = "CK_Cadmus_ID")

rbsa.appliances1 <- rbsa.appliances[grep("SITE", rbsa.appliances$CK_SiteID),]

#subset to only MF
rbsa.app.MF <- rbsa.appliances1[grep("Multifamily",rbsa.appliances1$BuildingType),]

###################################################################################################################
# ITEM 302: AVERAGE IN-UNIT TELEVISION POWER BY VINTAGE (MF Table 96)
###################################################################################################################
#subset to only televisions
item302.dat <- rbsa.app.MF[which(rbsa.app.MF$Type == "Television"),]

#remove missing TV.Wattage values
item302.dat1 <- item302.dat[which(!(item302.dat$TV.Wattage %in% c("unknown",NA))),]

#subset to only known TV vintages
item302.dat2 <- item302.dat1[which(item302.dat1$Age != "-- Datapoint not asked for --"),]
item302.dat2$Age <- as.numeric(as.character(item302.dat2$Age))
item302.dat2$TV.Wattage <- as.numeric(as.character(item302.dat2$TV.Wattage))

####Create vintage bins according to previous RBSA table
item302.dat2$Age.Cat <- item302.dat2$Age
item302.dat2$Age.Cat[which(item302.dat2$Age < 1990)] <- "Pre 1990"
item302.dat2$Age.Cat[which(item302.dat2$Age >= 1990 & item302.dat2$Age < 2000)] <- "1990-1999"
item302.dat2$Age.Cat[which(item302.dat2$Age >= 2000 & item302.dat2$Age < 2005)] <- "2000-2004"
item302.dat2$Age.Cat[which(item302.dat2$Age >= 2005 & item302.dat2$Age < 2010)] <- "2005-2009"
item302.dat2$Age.Cat[which(item302.dat2$Age >= 2010)] <- "Post 2009"
#check age category mapping
unique(item302.dat2$Age.Cat)

item302.dat3 <- item302.dat2[which(!is.na(item302.dat2$Age.Cat)),]
item302.dat4 <- item302.dat3[which(!is.na(item302.dat3$TV.Wattage)),]

######################################
#Pop and Sample Sizes for weights
######################################
item302.data <- weightedData(item302.dat4[which(colnames(item302.dat4) %notin% c("CK_SiteID"
                                                                                 ,"Type"
                                                                                 ,"Age"                
                                                                                 ,"TV.Screen.Type"
                                                                                 ,"TV.Wattage"
                                                                                 ,"Room"
                                                                                 ,"Clean.Room"
                                                                                 ,"Age.Cat"))])

item302.data <- left_join(item302.data, item302.dat4[which(colnames(item302.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"CK_SiteID"
                                                                                         ,"Type"
                                                                                         ,"Age"                
                                                                                         ,"TV.Screen.Type"
                                                                                         ,"TV.Wattage"
                                                                                         ,"Room"
                                                                                         ,"Clean.Room"
                                                                                         ,"Age.Cat"))])
item302.data$count <- 1


######################
# weighted analysis
######################
item302.final <- mean_one_group(CustomerLevelData = item302.data
                                ,valueVariable = 'TV.Wattage'
                                ,byVariable = 'Age.Cat'
                                ,aggregateRow = "All Vintages")








###################################################################################################################
# ITEM 303: DISTRIBUTION OF IN-UNIT TELEVISION SCREENS BY TYPE AND VINTAGE (MF Table 97)
###################################################################################################################
#subset to only televisions
item303.dat1 <- rbsa.app.MF[which(rbsa.app.MF$Type == "Television"),]

#subset to only known TV vintages
item303.dat2 <- item303.dat1[which(item303.dat1$Age != "-- Datapoint not asked for --"),]
item303.dat2$Age <- as.numeric(as.character(item303.dat2$Age))
item303.dat2$TV.Wattage <- as.numeric(as.character(item303.dat2$TV.Wattage))

####Create vintage bins according to previous RBSA table
item303.dat2$Age.Cat <- item303.dat2$Age
item303.dat2$Age.Cat[which(item303.dat2$Age < 1990)] <- "Pre 1990"
item303.dat2$Age.Cat[which(item303.dat2$Age >= 1990 & item303.dat2$Age < 2000)] <- "1990-1999"
item303.dat2$Age.Cat[which(item303.dat2$Age >= 2000 & item303.dat2$Age < 2005)] <- "2000-2004"
item303.dat2$Age.Cat[which(item303.dat2$Age >= 2005 & item303.dat2$Age < 2010)] <- "2005-2009"
item303.dat2$Age.Cat[which(item303.dat2$Age >= 2010)] <- "Post 2009"
#check age category mapping
unique(item303.dat2$Age.Cat)

###### create TV screen type categories accoring to previous RBSA table (CRT and other)
item303.dat2$TV.Screen.Cat <- NA
item303.dat2$TV.Screen.Cat[grep("CRT|crt|Crt", item303.dat2$TV.Screen.Type)] <- "CRT"
item303.dat2$TV.Screen.Cat[which(is.na(item303.dat2$TV.Screen.Cat))] <- "Other"

#create counter
item303.dat2$count <- 1

#summarise by vintages
item303.sum1 <- summarise(group_by(item303.dat2, Age.Cat, TV.Screen.Cat)
                          ,Count = sum(count))
item303.tot.count1 <- summarise(group_by(item303.dat2, Age.Cat)
                                ,Total.Count = sum(count)
                                ,SampleSize = length(unique(CK_Cadmus_ID)))
item303.vintage1 <- left_join(item303.sum1, item303.tot.count1, by = "Age.Cat")

#summarise across vintages
item303.sum2 <- summarise(group_by(item303.dat2, TV.Screen.Cat)
                          ,Age.Cat = "All Vintages"
                          ,Count = sum(count))
item303.tot.count2 <- summarise(group_by(item303.dat2)
                                ,Age.Cat = "All Vintages"
                                ,Total.Count = sum(count)
                                ,SampleSize = length(unique(CK_Cadmus_ID)))
item303.vintage2 <- left_join(item303.sum2, item303.tot.count2, by = "Age.Cat")

#row bind by and across vintage information 
item303.final <- rbind.data.frame(item303.vintage1, item303.vintage2)

#calculate percent and SE of percent
item303.final$Percent <- item303.final$Count / item303.final$Total.Count
item303.final$SE <- sqrt(item303.final$Percent * (1 - item303.final$Percent) / item303.final$SampleSize)

#cast data by screen type
item303.cast <- dcast(setDT(item303.final)
                      ,formula = Age.Cat + SampleSize ~ TV.Screen.Cat
                      ,value.var = c("Percent", "SE"))

#put into correct table format
item303.table <- data.frame("Equipment_Vintage" = item303.cast$Age.Cat
                            ,"CRT_Percent" = item303.cast$Percent_CRT
                            ,"CRT_SE" = item303.cast$SE_CRT
                            ,"Other_Percent"= item303.cast$Percent_Other
                            ,"Other_SE" = item303.cast$SE_Other
                            ,"SampleSize" = item303.cast$SampleSize)










###################################################################################################################
# ITEM 304: DISTRIBUTION OF IN-UNIT TELEVISIONS BY ROOM TYPE (MF Table 98)
###################################################################################################################
#subset to only televisions
item304.dat <- rbsa.app.MF[which(rbsa.app.MF$Type == "Television"),]

#add counter
item304.dat$count <- 1

#subset to remove any missing room types
item304.dat1 <- item304.dat[which(!(is.na(item304.dat$Clean.Room))),]

#summarise by clean room type
item304.sum1 <- summarise(group_by(item304.dat1, Clean.Room)
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#get total count (sum of count across room types)
item304.sum1$Total.Count <- sum(item304.sum1$Count)

#calculate percent and SE of percent
item304.sum1$Percent <- item304.sum1$Count / item304.sum1$Total.Count
item304.sum1$SE <- sqrt(item304.sum1$Percent * (1 - item304.sum1$Percent) / item304.sum1$SampleSize)

#put in table format
item304.table <- data.frame("Room_Type" = item304.sum1$Clean.Room
                            ,"Percent" = item304.sum1$Percent
                            ,"SE" = item304.sum1$SE
                            ,"SampleSize" = item304.sum1$SampleSize)








