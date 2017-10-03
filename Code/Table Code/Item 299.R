#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

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
                                                                    ,"Iteration"
                                                                    ,""
                                                                    ,""))]
item299.dat$count <- 1

#join clean rbsa data onto appliances analysis data
item299.dat0 <- left_join(item299.dat, rbsa.dat, by = "CK_Cadmus_ID")

#remove missing vintage info
item299.dat1 <- item299.dat0[which(!(is.na(item299.dat0$HomeYearBuilt_MF))),]

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
item299.dat4 <- left_join(rbsa.dat, item299.tmp, by = "CK_Cadmus_ID")

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
item299.dat5$Dishwasher.Cat[which(item299.dat5$Age.Num >= 2005 & item299.dat5$Age.Num < 2009)] <- "2005-2009"
item299.dat5$Dishwasher.Cat[which(item299.dat5$Age.Num >= 2009)] <- "Post 2009"
unique(item299.dat5$Dishwasher.Cat)
####################
# end cleaning
####################

#call new data
item299.dat6 <- item299.dat5

#add counter
item299.dat6$count <- 1

#summarise by AGE
item299.sum1 <- summarise(group_by(item299.dat6, Dishwasher.Cat)
                          ,Count = sum(count)
                          ,Num.SampleSize = length(unique(CK_Cadmus_ID)))
#across AGE
item299.sum2 <- summarise(group_by(item299.dat6)
                          ,Dishwasher.Cat = "All Vintages"
                          ,Count = sum(count)
                          ,Num.SampleSize = length(unique(CK_Cadmus_ID)))

item299.merge1 <- rbind.data.frame(item299.sum1,item299.sum2, stringsAsFactors = F)

item299.samplesize <- summarise(group_by(item299.dat6)
                                ,SampleSize = length(unique(CK_Cadmus_ID)))

item299.final <- data.frame(item299.merge1, item299.samplesize, stringsAsFactors = F)

item299.final$Total.Count <- item299.sum2$Count
item299.final$Percent <- item299.final$Count / item299.final$Total.Count
item299.final$SE <- sqrt(item299.final$Percent * (1 - item299.final$Percent) / item299.final$SampleSize)


item299.table <- data.frame("Dishwasher.Vintage" = item299.final$Dishwasher.Cat
                            ,"Percent" = item299.final$Percent
                            ,"SE" = item299.final$SE
                            ,"SampleSize" = item299.final$Num.SampleSize)
