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
#Item 268: DISTRIBUTION OF BUILDING LAUNDRY TYPE BY BUILDING VINTAGE (MF Table 60)
#############################################################################################

#############################################################################################
#subset to necessary columns, clean, and normalize data for table
#############################################################################################

#subset to columns needed for analysis
item268.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"Age"
                                                                    ,"Type"
                                                                    ,"Washer.Type"
                                                                    ,"Iteration"
                                                                    ,""
                                                                    ,""))]
item268.dat$count <- 1

rbsa.sub <- data.frame("CK_Cadmus_ID" = rbsa.dat$CK_Cadmus_ID
                       ,"BuildingType" = rbsa.dat$BuildingType
                       ,"HomeYearBuilt_MF" = rbsa.dat$HomeYearBuilt_MF)

#join clean rbsa data onto appliances analysis data
item268.dat0 <- left_join(item268.dat, rbsa.sub, by = "CK_Cadmus_ID")

#remove missing vintage info
item268.dat1 <- item268.dat0[which(!(is.na(item268.dat0$HomeYearBuilt_MF))),]

#subset to only MF
item268.dat2 <- item268.dat1[grep("Multifamily", item268.dat1$BuildingType),]

item268.dat3 <- item268.dat2[which(item268.dat2$Type %in% c("Washer", "Dryer")),]

item268.dat3$Laundry.Location <- item268.dat3$Iteration
item268.dat3$Laundry.Location[grep("BLDG", item268.dat3$Iteration)] <- "Common"
item268.dat3$Laundry.Location[grep("SITE", item268.dat3$Iteration)] <- "In.Unit"

# Note that none of the sites in the current subset of sampled sites have common and in-unit laundry, 
# but we may see it with the full data
item268.dat3$Laundry.Location[which(item268.dat3$Laundry.Location == "Common" & item268.dat3$Laundry.Location == "In.Unit")] <- "In.Unit.and.Common"

unique(item268.dat3$Laundry.Location)



#############################################################################################
#Merging normalized data to RBSA data to create "None" category
#############################################################################################

item268.sub <- unique(data.frame("CK_Cadmus_ID" = item268.dat3$CK_Cadmus_ID
                                 , "Laundry.Location" = item268.dat3$Laundry.Location, stringsAsFactors = F))

item268.dat4 <- left_join(rbsa.dat, item268.sub, by = "CK_Cadmus_ID") #should be 601

#remove missing vintage info
item268.dat5 <- item268.dat4[which(!(is.na(item268.dat4$HomeYearBuilt_MF))),]

#subset to only MF
item268.dat6 <- item268.dat5[grep("Multifamily", item268.dat5$BuildingType),]

item268.dat6$Laundry.Location[which(is.na(item268.dat6$Laundry.Location))] <- "None"
item268.dat6$count <- 1



#############################################################################################
#Analysis
#############################################################################################

#summarise by vintage
#by laundry location
item268.sum1 <- summarise(group_by(item268.dat6, HomeYearBuilt_MF, Laundry.Location)
                          ,Count = sum(count))

#summarise across vintage
#by laundry location
item268.sum2 <- summarise(group_by(item268.dat6, Laundry.Location)
                          ,HomeYearBuilt_MF = "All Vintages"
                          ,Count = sum(count))

item268.merge1 <- rbind.data.frame(item268.sum1, item268.sum2, stringsAsFactors = F)

#summarise to get total counts
item268.total.count1 <- summarise(group_by(item268.dat6, HomeYearBuilt_MF)
                                 ,Total.Count = sum(count)
                                 ,SampleSize = length(unique(CK_Cadmus_ID)))
#summarise to get total counts
item268.total.count2 <- summarise(group_by(item268.dat6)
                                  ,HomeYearBuilt_MF = "All Vintages"
                                  ,Total.Count = sum(count)
                                  ,SampleSize = length(unique(CK_Cadmus_ID)))
item268.merge2 <- rbind.data.frame(item268.total.count1, item268.total.count2, stringsAsFactors = F)

item268.final <- left_join(item268.merge1, item268.merge2, by = "HomeYearBuilt_MF")

item268.final$Percent <- item268.final$Count / item268.final$Total.Count
item268.final$SE <- sqrt(item268.final$Percent * (1 - item268.final$Percent) / item268.final$SampleSize)

item268.cast <- dcast(setDT(item268.final)
                      ,formula = HomeYearBuilt_MF + SampleSize ~ Laundry.Location
                      ,value.var = c("Percent", "SE"))

item268.table <- data.frame("Housing.Vintage" = item268.cast$HomeYearBuilt_MF
                            ,"Common.Only" = item268.cast$Percent_Common
                            ,"Common.Only.SE" = item268.cast$SE_Common
                            ,"In.Unit.Only" = item268.cast$Percent_In.Unit
                            ,"In.Unit.Only.SE" = item268.cast$SE_In.Unit
                            ,"In.Unit.and.Common" = NA#
                            ,"In.Unit.and.Common.SE" = NA#
                            ,"None" = item268.cast$Percent_None
                            ,"None.SE" = item268.cast$SE_None
                            ,"SampleSize" = item268.cast$SampleSize)











#############################################################################################
#Item 269: DISTRIBUTION OF COMMON AREA CLOTHES WASHER TYPE BY WASHER VINTAGE (MF Table 61)
#############################################################################################
#subset to columns needed for analysis
item269.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"Age"
                                                                    ,"Type"
                                                                    ,"Washer.Type"
                                                                    ,"Iteration"
                                                                    ,""
                                                                    ,""))]
item269.dat$count <- 1

#join clean rbsa data onto appliances analysis data
item269.dat0 <- left_join(item269.dat, rbsa.dat, by = "CK_Cadmus_ID")

#remove missing vintage info
item269.dat1 <- item269.dat0[which(!(is.na(item269.dat0$HomeYearBuilt_MF))),]

#subset to only MF
item269.dat2 <- item269.dat1[grep("Multifamily", item269.dat1$BuildingType),]

#subset to only washers
item269.dat3 <- item269.dat2[which(item269.dat2$Type %in% c("Washer")),]

#subset to only common area washers
item269.dat4 <- item269.dat3[grep("BLDG", item269.dat3$Iteration),]

#subset to only common area washers that have observed age info
item269.dat5 <- item269.dat4#[which(item269.dat4$Age > 0),]

#####################
# At this point, code will be needed to bin ages into categories according to previous table
#####################


#add counter
item269.dat5$count <- 1

#summarise by washer type
#by measure age
item269.sum1 <- summarise(group_by(item269.dat5, Washer.Type, Age)
                          ,Count = sum(count))
#across measure age
item269.sum2 <- summarise(group_by(item269.dat5, Washer.Type)
                          ,Age = "All Vintages"
                          ,Count = sum(count))
#summarise across washer type
#by measure age
item269.sum3 <- summarise(group_by(item269.dat5, Age)
                          ,Washer.Type = "All Types"
                          ,Count = sum(count))
#across measure age
item269.sum4 <- summarise(group_by(item269.dat5)
                          ,Washer.Type = "All Types"
                          ,Age = "All Vintages"
                          ,Count = sum(count))

item269.merge1 <- rbind.data.frame(item269.sum1,item269.sum2,item269.sum3,item269.sum4, stringsAsFactors = F)

#Sample Sizes and total counts
item269.tmp1 <- summarise(group_by(item269.dat5, Washer.Type)
                          ,Total.Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
item269.tmp2 <- summarise(group_by(item269.dat5)
                          ,Washer.Type = "All Types"
                          ,Total.Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
item269.merge2 <- rbind.data.frame(item269.tmp1, item269.tmp2, stringsAsFactors = F)

#merge on sample sizes
item269.final <- left_join(item269.merge1, item269.merge2, by = "Washer.Type")
item269.final$Total.Count[which(item269.final$Age == "All Vintages")] <- item269.final$Total.Count[which(item269.final$Age == "All Vintages" & item269.final$Washer.Type == "All Types")]

#calculate percent and SE
item269.final$Percent <- item269.final$Count / item269.final$Total.Count
item269.final$SE <- sqrt(item269.final$Percent * (1 - item269.final$Percent) / item269.final$SampleSize)

item269.cast <- dcast(setDT(item269.final)
                      ,formula = Washer.Type + SampleSize ~ Age
                      ,value.var = c("Percent", "SE"))

item269.table <- data.frame("Clothes.Washer.Type" = item269.cast$Washer.Type
                            ,"Pre.1980" = NA#item269.cast$
                            ,"Pre.1980.SE" = NA#item269.cast$
                            ,"1980.1989" = NA#item269.cast$
                            ,"1980.1989.SE" = NA#item269.cast$
                            ,"1990.1994" = NA#item269.cast$
                            ,"1990.1994.SE" = NA#item269.cast$
                            ,"1995.1999" = NA#item269.cast$
                            ,"1995.1999.SE" = NA#item269.cast$
                            ,"2000.2004" = NA#item269.cast$
                            ,"2000.2004.SE" = NA#item269.cast$
                            ,"2005.2009" = NA#item269.cast$
                            ,"2005.2009.SE" = NA#item269.cast$
                            ,"Post.2010" = NA#item269.cast$
                            ,"Post.2010.SE" = NA#item269.cast$
                            ,"All.Vintages" = item269.cast$`Percent_All Vintages`
                            ,"All.Vintages.SE" = item269.cast$`SE_All Vintages`
                            ,"SampleSize" = item269.cast$SampleSize)













#############################################################################################
#Item 270: (MF Table 62)
#############################################################################################













#############################################################################################
#Item 271: DISTRIBUTION OF COMMON AREA CLOTHES DRYER BY DRYER VINTAGE (MF Table 63)
#############################################################################################
#subset to columns needed for analysis
item271.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"Age"
                                                                    ,"Type"
                                                                    ,"Iteration"
                                                                    ,""
                                                                    ,""))]
item271.dat$count <- 1

#join clean rbsa data onto appliances analysis data
item271.dat0 <- left_join(item271.dat, rbsa.dat, by = "CK_Cadmus_ID")

#remove missing vintage info
item271.dat1 <- item271.dat0[which(!(is.na(item271.dat0$HomeYearBuilt_MF))),]

#subset to only MF
item271.dat2 <- item271.dat1[grep("Multifamily", item271.dat1$BuildingType),]

#subset to only Dryers
item271.dat3 <- item271.dat2[which(item271.dat2$Type %in% c("Dryer")),]

#subset to only common area Dryers
item271.dat4 <- item271.dat3[grep("BLDG", item271.dat3$Iteration),]

#subset to only common area Dryers that have observed age info
item271.dat5 <- item271.dat4#[which(item271.dat4$Age > 0),]

#####################
# At this point, code will be needed to bin ages into categories according to previous table
#####################


#add counter
item271.dat5$count <- 1

#summarise by AGE
item271.sum1 <- summarise(group_by(item271.dat5, Age)
                          ,Count = sum(count)
                          ,Num.SampleSize = length(unique(CK_Cadmus_ID)))
#across AGE
item271.sum2 <- summarise(group_by(item271.dat5)
                          ,Age = "All Vintages"
                          ,Count = sum(count)
                          ,Num.SampleSize = length(unique(CK_Cadmus_ID)))

item271.merge1 <- rbind.data.frame(item271.sum1,item271.sum2, stringsAsFactors = F)

item271.samplesize <- summarise(group_by(item271.dat5)
                                ,SampleSize = length(unique(CK_Cadmus_ID)))

item271.final <- data.frame(item271.merge1, item271.samplesize, stringsAsFactors = F)

item271.final$Total.Count <- item271.sum2$Count
item271.final$Percent <- item271.final$Count / item271.final$Total.Count
item271.final$SE <- sqrt(item271.final$Percent * (1 - item271.final$Percent) / item271.final$SampleSize)


item271.table <- data.frame("Dryer.Vintage" = item271.final$Age
                            ,"Percent" = item271.final$Percent
                            ,"SE" = item271.final$SE
                            ,"SampleSize" = item271.final$Num.SampleSize)
