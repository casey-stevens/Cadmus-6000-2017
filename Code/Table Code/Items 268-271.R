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
