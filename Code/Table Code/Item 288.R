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
water.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, water.export))
#clean cadmus IDs
water.dat$CK_Cadmus_ID <- trimws(toupper(water.dat$CK_Cadmus_ID))

water.dat2 <- water.dat[,which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                           ,"CK_SiteID"
                                                           ,"GPM_Measured"))]

water.dat3 <- left_join(water.dat2, rbsa.dat, by = "CK_Cadmus_ID")

water.dat4 <- water.dat3[which(water.dat3$CK_Cadmus_ID != "CK_CADMUS_ID"),]
water.dat5 <- water.dat4[grep("Multifamily", water.dat4$BuildingType),]
water.dat6 <- water.dat5[grep("site",water.dat5$CK_SiteID,ignore.case = T),]

#############################################################################################
#Item 288: DISTRIBUTION OF UNIT WATER HEATERS BY TYPE  (MF Table 80)
#############################################################################################

item288.dat <- water.dat6

item288.dat$Flow <- as.numeric(as.character(item288.dat$GPM_Measured))
item288.dat1 <- item288.dat[which(!is.na(item288.dat$Flow)),]

item288.dat1$FlowRate <- NA
item288.dat1$FlowRate[which(item288.dat1$Flow <= 1.5)] <- "<=1.5"
item288.dat1$FlowRate[which(item288.dat1$Flow > 1.5 & item288.dat1$Flow < 2)] <- "1.6_2.0"
item288.dat1$FlowRate[which(item288.dat1$Flow > 1.5 & item288.dat1$Flow <= 2)] <- "1.6_2.0"
item288.dat1$FlowRate[which(item288.dat1$Flow > 2 & item288.dat1$Flow <= 2.5)] <- "2.1_2.5"
item288.dat1$FlowRate[which(item288.dat1$Flow > 2.5 & item288.dat1$Flow <= 3.5)] <- "2.6_3.5"
item288.dat1$FlowRate[which(item288.dat1$Flow > 3.5)] <- ">3.5"

unique(item288.dat1$FlowRate)
item288.dat1$Count <- 1
######################################
#Summarize distribution
######################################

item288.sum1 <- summarise(group_by(item288.dat1,FlowRate),
                          Count = sum(Count))

item288.sum1$SampleSize <- sum(item288.sum1$Count)

item288.sum1$Percent <- item288.sum1$Count / item288.sum1$SampleSize
item288.sum1$SE <- sqrt(item288.sum1$Percent * (1 - item288.sum1$Percent) / item288.sum1$SampleSize)

item288.final <- data.frame("FlowRate" = item288.sum1$FlowRate,
                            "Percent" = item288.sum1$Percent,
                            "SE" = item288.sum1$SE,
                            "n" = item288.sum1$Count)
