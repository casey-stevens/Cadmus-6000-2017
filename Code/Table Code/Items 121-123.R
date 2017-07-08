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
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))



#############################################################################################
#Item 121: AVERAGE OCCUPANT AGE PER HOME BY STATE (SF table 128, MH table 103)
#############################################################################################
#subset to columns needed for analysis
item121.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,""
                                                                                     ,""))])
item121.dat$count <- 1

#remove any repeat header rows from exporting
item121.dat0 <- item121.dat[which(item121.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item121.dat1 <- left_join(item121.dat0, rbsa.dat, by = "CK_Cadmus_ID")













#############################################################################################
#Item 122: AVERAGE NUMBER OF OCCUPANTS PER HOME BY STATE (SF table 129, MH table 104)
#############################################################################################
#subset to columns needed for analysis
item122.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                              ,"Qty.Occupants"
                                                                              ,""))])
item122.dat$count <- 1

#remove any repeat header rows from exporting
item122.dat0 <- item122.dat[which(item122.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item122.dat1 <- left_join(item122.dat0, rbsa.dat, by = "CK_Cadmus_ID")


#by state
item122.state <- summarise(group_by(item122.dat1, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Qty.Occupants)
                           ,SE = sd(Qty.Occupants) / sqrt(SampleSize))
#by region
item122.region <- summarise(group_by(item122.dat1, BuildingType)
                            , State = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(Qty.Occupants)
                           ,SE = sd(Qty.Occupants) / sqrt(SampleSize))

item122.final <- rbind.data.frame(item122.state, item122.region, stringsAsFactors = F)

item122.table <- data.frame("BuildingType" = item122.final$BuildingType
                            ,"State" = item122.final$State
                            ,"Mean" = item122.final$Mean
                            ,"SE" = item122.final$SE
                            ,"SampleSize" = item122.final$SampleSize)
item122.table1 <- item122.table[which(item122.table$BuildingType %in% c("Single Family", "Manufactured")),]














#############################################################################################
#Item 123: AVERAGE NUMBER OF OCCUPANTS BY AGE CATEGORY BY STATE (SF table 130, MH table 105)
#############################################################################################
#subset to columns needed for analysis
item123.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_LessThan1"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_1_5"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_6_10"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_11_18"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_19_45"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_46_64"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_65Older"
                                                                                     ,""))])
colnames(item123.dat) <- c("CK_Cadmus_ID"
                           ,"Age_Less_Than_1"
                           ,"Age_1_5"
                           ,"Age_6_10"
                           ,"Age_11_18"
                           ,"Age_19_45"
                           ,"Age_46_64"
                           ,"Age_65_Older")
item123.dat$count <- 1

#remove any repeat header rows from exporting
item123.dat0 <- item123.dat[which(item123.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item123.dat1 <- left_join(item123.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item123.dat1$Age_0_18 <- item123.dat1$Age_Less_Than_1 + item123.dat1$Age_1_5 + item123.dat1$Age_6_10 + item123.dat1$Age_11_18
item123.dat1$Age_19_64 <- item123.dat1$Age_19_45 + item123.dat1$Age_46_64


#summarise by state
item123.state <- summarise(group_by(item123.dat1, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean_0 = mean(Age_0_18)
                           ,SE_0 = sd(Age_0_18) / sqrt(SampleSize)
                           ,Mean_19 = mean(Age_19_64)
                           ,SE_19 = sd(Age_19_64) / sqrt(SampleSize)
                           ,Mean_65 = mean(Age_65_Older)
                           ,SE_65 = sd(Age_65_Older) / sqrt(SampleSize))

#summarise across states
item123.region <- summarise(group_by(item123.dat1, BuildingType)
                           ,State = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean_0 = mean(Age_0_18)
                           ,SE_0 = sd(Age_0_18) / sqrt(SampleSize)
                           ,Mean_19 = mean(Age_19_64)
                           ,SE_19 = sd(Age_19_64) / sqrt(SampleSize)
                           ,Mean_65 = mean(Age_65_Older)
                           ,SE_65 = sd(Age_65_Older) / sqrt(SampleSize))

item123.merge <- rbind.data.frame(item123.state, item123.region, stringsAsFactors = F)
item123.merge <- data.table(item123.merge)
library(data.table)
item123.melt <- reshape(item123.merge, varying = 4:9, sep = "_", direction = 'long')
item123.melt$Age.Category <- c(rep("0 to 18", 12)
                               ,rep("19 to 64", 12)
                               ,rep("Older than 65", 12))

item123.cast <- dcast(setDT(item123.melt)
                      ,formula = BuildingType + Age.Category ~ State
                      ,value.var = c("Mean", "SE", "SampleSize"))

item123.cast <- data.frame(item123.cast)
item123.cast1 <- item123.cast[which(!(colnames(item123.cast) %in% c("SampleSize_MT"
                                                                    ,"SampleSize_OR"
                                                                    ,"SampleSize_WA")))]

item123.final <- data.frame("BuildingType" = item123.cast1$BuildingType
                            ,"Age.Category" = item123.cast1$Age.Category
                            ,"Mean_MT" = item123.cast1$Mean_MT
                            ,"SE_MT" = item123.cast1$SE_MT
                            ,"Mean_OR" = item123.cast1$Mean_OR
                            ,"SE_OR" = item123.cast1$SE_OR
                            ,"Mean_WA" = item123.cast1$Mean_WA
                            ,"SE_WA" = item123.cast1$SE_WA
                            ,"Mean_Region" = item123.cast1$Mean_Region
                            ,"SE_Region" = item123.cast1$SE_Region
                            ,"SampleSize" = item123.cast1$SampleSize_Region)

item123.table <- item123.final[which(item123.final$BuildingType %in% c("Single Family", "Manufactured")),]
