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
buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Cadmus_ID <- trimws(toupper(buildings.dat$CK_Cadmus_ID))
length(unique(buildings.dat$CK_Cadmus_ID)) #196/201 unique

buildings.dat.clean <- buildings.dat[-which(duplicated(buildings.dat$CK_Cadmus_ID)),]


#Read in data for analysis
buildings.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.interview.export))
#clean cadmus IDs
buildings.interview.dat$CK_Cadmus_ID <- trimws(toupper(buildings.interview.dat$CK_Cadmus_ID))



#############################################################################################
# Item 225: DISTRIBUTION OF OWNERSHIP TYPE BY BUILDING SIZE (MF table 17)
#############################################################################################
item225.dat <- buildings.interview.dat[which(colnames(buildings.interview.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership"))]

colnames(item225.dat) <- c("CK_Cadmus_ID", "Ownership")

item225.dat0 <- item225.dat[which(!(is.na(item225.dat$Ownership))),]
item225.dat1 <- item225.dat0[which(item225.dat0$Ownership != "Unknown"),]


item225.dat2 <- left_join(item225.dat1, rbsa.dat, by = "CK_Cadmus_ID")
item225.dat2$count <- 1


#subset to only MF sites
item225.dat3 <- item225.dat2[which(item225.dat2$BuildingTypeXX %in% c("Apartment Building (3 or fewer floors)"
                                                                        ,"Apartment Building (4 to 6 floors)"
                                                                        ,"Apartment Building (More than 6 floors)")),]


#summarise by nonresidential type, across housing sizes
item225.sum1 <- summarise(group_by(item225.dat3, Ownership)
                          ,BuildingTypeXX = "All Sizes"
                          ,Count = sum(count))

#summarise by nonresidential type and housing sizes
item225.sum2 <- summarise(group_by(item225.dat3, BuildingTypeXX, Ownership)
                          ,Count = sum(count))



#row bind
item225.merge1 <- rbind.data.frame(item225.sum1, item225.sum2, stringsAsFactors = F)

item225.sampleSizes <- summarise(group_by(item225.dat3, Ownership)
                                ,SampleSize = length(unique(CK_Cadmus_ID)))


item225.merge2 <- left_join(item225.merge1, item225.sampleSizes, by = "Ownership")


#get total counts
item225.total.counts <- summarise(group_by(item225.merge2, BuildingTypeXX)
                                  ,Total.Count = sum(Count))


item225.final <- left_join(item225.merge2, item225.total.counts, by = "BuildingTypeXX")
item225.final$Percent <- item225.final$Count / item225.final$Total.Count
item225.final$SE <- sqrt(item225.final$Percent * (1 - item225.final$Percent) / item225.final$SampleSize)

item225.cast <- dcast(setDT(item225.final)
                      ,formula = Ownership ~ BuildingTypeXX
                      ,value.var = c("Percent","SE", "SampleSize"))

item225.table <- data.frame("Ownership" = item225.cast$Ownership
                            ,"Low_Rise_1.3_Percent" = item225.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_SE" = item225.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_4.6_Percent" = NA#item225.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_SE" = NA#item225.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_7Plus_Percent" = NA#item225.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE" = NA#item225.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_Percent" = item225.cast$`Percent_All Sizes`
                            ,"All_Sizes_SE" = item225.cast$`SE_All Sizes`
                            ,"SampleSize" = item225.cast$`SampleSize_All Sizes`)

item225.table.MF <- item225.table

library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
workbook.MF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - MF - COPY.xlsx", sep="/"))

# UPDATE SHEET AND X
writeData(workbook.MF, sheet = "Table 17", x = item225.table.MF, startRow = 20)

saveWorkbook(workbook.MF, file = paste(outputFolder, "Tables in Excel - MF - COPY.xlsx", sep="/"), overwrite = T)

