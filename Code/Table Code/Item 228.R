#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################


rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
buildings.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.interview.export))
#clean cadmus IDs
buildings.interview.dat$CK_Cadmus_ID <- trimws(toupper(buildings.interview.dat$CK_Cadmus_ID))


#############################################################################################
#Item 228: REPORTED BUILDING VACANCY RATE BY VINTAGE (MF Table 20)
#############################################################################################
item228.dat <- unique(buildings.interview.dat[which(colnames(buildings.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataVacancy_PercentOfUnitsNotOccupiedInAuditedBuilding"))])


colnames(item228.dat) <- c("CK_Cadmus_ID"
                           ,"PercentUnoccupied")

item228.dat$count <- 1

#remove any repeat header rows from exporting
item228.dat0 <- item228.dat[which(item228.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item228.dat1 <- left_join(item228.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#Subset to Multifamily
item228.dat2 <- item228.dat1[grep("Multifamily", item228.dat1$BuildingType),]


#Will remove NA's from this analysis since it appears zeros are legitamte averages

item228.dat3 <- item228.dat2[which(!is.na(item228.dat2$PercentUnoccupied)
                                     & !is.na(item228.dat2$HomeYearBuilt_MF)),]

#summarise by vintage
item228.vintage <- summarise(group_by(item228.dat3, HomeYearBuilt_MF)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(PercentUnoccupied)
                           ,SE = sd(PercentUnoccupied) / sqrt(SampleSize))

item228.vintageAll <- summarise(item228.dat3
                             ,SampleSize = length(unique(CK_Cadmus_ID))
                             ,HomeYearBuilt_MF = "All Vintages"
                             ,Mean = mean(PercentUnoccupied)
                             ,SE = sd(PercentUnoccupied) / sqrt(SampleSize))

item228.table <- rbind.data.frame(item228.vintage,item228.vintageAll,stringsAsFactors = F)

item228.final <- data.frame("Vintage" = item228.table$HomeYearBuilt_MF
                            ,"Percent" = item228.table$Mean
                            ,"SE" = item228.table$SE
                            ,"SampleSize" = item228.table$SampleSize
                            ,stringsAsFactors = F)


