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
item276.dat1 <- left_join(rbsa.dat, item276.dat00, by = "CK_Cadmus_ID")

#subset to only multifamily units
item276.dat2 <- item276.dat1[grep("Multifamily",item276.dat1$BuildingType),]
item276.dat2$Kitchen.Ind <- 0
item276.dat2$Kitchen.Ind[which(item276.dat2$Clean.Type == "Kitchen")] <- 1

#summarise by buildings size
item276.sum1 <- summarise(group_by(item276.dat2, BuildingTypeXX)
                          ,Mean = mean(Kitchen.Ind)
                          ,SE = sd(Kitchen.Ind) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#summarise across building size
item276.sum2 <- summarise(group_by(item276.dat2)
                          ,BuildingTypeXX = "All Sizes"
                          ,Mean = mean(Kitchen.Ind)
                          ,SE = sd(Kitchen.Ind) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#merge
item276.final <- rbind.data.frame(item276.sum1, item276.sum2, stringsAsFactors = F)

item276.table <- data.frame("Building.Size" = item276.final$BuildingTypeXX
                            ,"Mean" = item276.final$Mean
                            ,"SE" = item276.final$SE
                            ,"SampleSize" = item276.final$SampleSize)

                          