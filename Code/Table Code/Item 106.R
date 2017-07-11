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



#############################################################################################
#Item 106: DISTRIBUTION OF SHOWERHEAD FLOW RATE BY STATE (SF table 113, MH table 88)
#############################################################################################
#subset to columns needed for analysis
item106.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"GPM_Measured"
                                                          ,"Fixture.Type"))]
item106.dat$count <- 1

item106.dat0 <- item106.dat[which(item106.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item106.dat1 <- left_join(item106.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item106.dat1$GPM_Measured <- as.numeric(as.character(item106.dat1$GPM_Measured))
item106.dat2 <- item106.dat1[which(!(is.na(item106.dat1$GPM_Measured))),]
unique(item106.dat2$GPM_Measured)

item106.dat3 <- item106.dat2[grep("shower|Shower",item106.dat2$Fixture.Type),]

item106.dat4 <- summarise(group_by(item106.dat3, CK_Cadmus_ID, BuildingType, State, count)
                          ,GPM.Measured.Site = mean(GPM_Measured))

item106.dat4$GPM_bins <- item106.dat4$GPM.Measured.Site
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site <= 1.5)] <- "< 1.5"
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >  1.5 & item106.dat4$GPM.Measured.Site < 2.1)] <- "1.6-2.0"
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >= 2.1 & item106.dat4$GPM.Measured.Site < 2.6)] <- "2.1-2.5"
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >= 2.6 & item106.dat4$GPM.Measured.Site < 3.6)] <- "2.6-3.5"
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site >= 3.6)] <- "> 3.6"
unique(item106.dat4$GPM_bins)

#summarise by State
#summarise by gpm bins
item106.state1 <- summarise(group_by(item106.dat4, BuildingType, State, GPM_bins)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Count = sum(count))
#summarise across gpm bins
item106.state2 <- summarise(group_by(item106.dat4, BuildingType, State)
                          ,GPM_bins = "Total"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Count = sum(count))



#summarise across States
#summarise by gpm bins
item106.region1 <- summarise(group_by(item106.dat4, BuildingType, GPM_bins)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(count))
#summarise across gpm bins
item106.region2 <- summarise(group_by(item106.dat4, BuildingType)
                            ,GPM_bins = "Total"
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(count))

item106.merge1 <- rbind.data.frame(item106.state1, item106.state2, item106.region1, item106.region2, stringsAsFactors = F)


item106.tot.counts <- rbind.data.frame(item106.state2, item106.region2, stringsAsFactors = F)

item106.final <- left_join(item106.merge1, item106.tot.counts, by = c("BuildingType", "State"))
colnames(item106.final) <- c("BuildingType"
                              ,"State"
                              ,"GPM"
                              ,"SampleSize"
                              ,"Count" 
                              ,"Remove"
                              ,"Remove"
                              ,"Total.Count")
item106.final$Percent <- item106.final$Count / item106.final$Total.Count
item106.final$SE <- sqrt(item106.final$Percent * (1 - item106.final$Percent) / item106.final$SampleSize)

library(data.table)
item106.cast <- dcast(setDT(item106.final)
                      ,formula = BuildingType + GPM ~ State
                      ,value.var = c("SampleSize", "Percent","SE"))

item106.final <- data.frame("BuildingType" = item106.cast$BuildingType
                            ,"GPM" = item106.cast$GPM
                            ,"Percent_MT" = item106.cast$Percent_MT
                            ,"SE_MT" = item106.cast$SE_MT
                            ,"Percent_WA" = item106.cast$Percent_WA
                            ,"SE_WA" = item106.cast$SE_WA
                            ,"Percent_Region" = item106.cast$Percent_Region
                            ,"SE_Region" = item106.cast$SE_Region
                            ,"SampleSize" = item106.cast$SampleSize_Region)

item106.table <- item106.final[which(item106.final$BuildingType %in% c("Single Family", "Manufactured")),]
