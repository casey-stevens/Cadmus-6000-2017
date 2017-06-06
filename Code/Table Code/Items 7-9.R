#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)

##  Include packages
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(openxlsx)
library(stringr)
library(data.table)

#############################################################################################
# Import Data
#############################################################################################
# Define File Path
SPPath   <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/Data for SCL"
sourcePath <- "C:/Users/Casey.Stevens/Documents/RBSA/RBSA Analysis"
stopifnot(file.exists(SPPath))

rbsa.dat     <- read.xlsx(xlsxFile = file.path(sourcePath, paste("clean.rbsa.data", rundate, ".xlsx")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

room.dat <- read.xlsx(xlsxFile = file.path(SPPath, "ROOMS_2017.03.30.xlsx"))


##############################################################################################################################
# Item 7: AVERAGE NUMBER OF BEDROOMS PER HOME BY STATE (SF Table 14)
##############################################################################################################################

#subset to columns need in table
item7.dat <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID", "Iteration", "Clean.Type", "Area"))]
item7.dat0 <- left_join(rbsa.dat, item7.dat, by = "CK_Cadmus_ID")

#remove building information
item7.dat1 <- item7.dat0[-grep("BLDG", item7.dat0$Iteration),]

#subset to only bedrooms
item7.dat2 <- item7.dat1[which(item7.dat1$Clean.Type == "Bedroom"),]

item7.dat2$count <- 1

#summarize within home by state
item7.state.dat <- summarise(group_by(item7.dat2, CK_Cadmus_ID, BuildingType, State)
                        ,Home_Count = sum(count)
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        )
#summarize across homes by state
item7.state.dat <- summarise(group_by(item7.state.dat, BuildingType, State)
                        ,Avg = mean(Home_Count)
                        ,SE  = sd(Home_Count) / sqrt(length(unique(CK_Cadmus_ID)))
                        ,EB  = SE * 1.645
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        )

#summarize within home by region
item7.region.dat <- summarise(group_by(item7.dat2, CK_Cadmus_ID, BuildingType)
                              ,State = "Region"
                              ,Home_Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID))
)
#summarize across homes by region
item7.region.dat <- summarise(group_by(item7.region.dat, BuildingType)
                              ,State = "Region"
                              ,Avg = mean(Home_Count)
                              ,SE  = sd(Home_Count) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,EB  = SE * 1.645
                              ,SampleSize = length(unique(CK_Cadmus_ID))
)


#rbind state and region information
item7.full <- rbind.data.frame(item7.state.dat, item7.region.dat, stringsAsFactors = F)




##############################################################################################################################
# Item 8: AVERAGE NUMBER OF BATHROOMS PER HOME BY STATE (SF Table 15)
##############################################################################################################################
#subset to columns need in table
item8.dat <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID", "Iteration", "Clean.Type", "Area"))]
item8.dat0 <- left_join(rbsa.dat, item8.dat, by = "CK_Cadmus_ID")

#remove building information
item8.dat1 <- item8.dat0[-grep("BLDG", item8.dat0$Iteration),]

#subset to only bedrooms
item8.dat2 <- item8.dat1[which(item8.dat1$Clean.Type == "Bathroom"),]

item8.dat2$count <- 1

#summarize within home by state
item8.state.dat <- summarise(group_by(item8.dat2, CK_Cadmus_ID, BuildingType, State)
                             ,Home_Count = sum(count)
                             ,SampleSize = length(unique(CK_Cadmus_ID))
)
#summarize across homes by state
item8.state.dat <- summarise(group_by(item8.state.dat, BuildingType, State)
                             ,Avg = mean(Home_Count)
                             ,SE  = sd(Home_Count) / sqrt(length(unique(CK_Cadmus_ID)))
                             ,EB  = SE * 1.645
                             ,SampleSize = length(unique(CK_Cadmus_ID))
)

#summarize within home by region
item8.region.dat <- summarise(group_by(item8.dat2, CK_Cadmus_ID, BuildingType)
                              ,State = "Region"
                              ,Home_Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID))
)
#summarize across homes by region
item8.region.dat <- summarise(group_by(item8.region.dat, BuildingType)
                              ,State = "Region"
                              ,Avg = mean(Home_Count)
                              ,SE  = sd(Home_Count) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,EB  = SE * 1.645
                              ,SampleSize = length(unique(CK_Cadmus_ID))
)


#rbind state and region information
item8.full <- rbind.data.frame(item8.state.dat, item8.region.dat, stringsAsFactors = F)




##############################################################################################################################
# Item 9: AVERAGE ROOM AREAS BY ROOM TYPE (SF Table 16)
##############################################################################################################################
#subset to columns need in table
item9.dat <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID", "Iteration", "Clean.Type", "Area"))]
item9.dat0 <- left_join(rbsa.dat, item9.dat, by = "CK_Cadmus_ID")

item9.dat0$count <- 1
item9.dat0$Area <- as.numeric(as.character(item9.dat0$Area))

#average within houses
item9.sum <- summarise(group_by(item9.dat0, CK_Cadmus_ID, Clean.Type)
          ,Site_Area = mean(Area)
          )
#remove missing area information
item9.dat1 <- item9.sum[which(!(is.na(item9.sum$Site_Area))),]

#average across houses
item9.sum1 <- summarise(group_by(item9.dat1, Clean.Type)
                        ,Avg_Area = mean(Site_Area)
                        ,EB_Area  = sd(Site_Area) / sqrt(length(unique(CK_Cadmus_ID)))
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        )
item9.sum2 <- item9.sum1[which(item9.sum1$Clean.Type %in% c("Bathroom"
                                                             ,"Bedroom"
                                                             ,"Closet"
                                                             ,"Dining Room"
                                                             ,"Family Room"
                                                             ,"Garage"
                                                             ,"Hall"
                                                             ,"Kitchen"
                                                             ,"Laundry Room"
                                                             ,"Living Room"
                                                             ,"Master Bedroom"
                                                             ,"Office"
                                                             ,"Other")),]
item9.sum3 <- summarise(group_by(item9.dat1)
                        ,Clean.Type = "All Room Types"
                        ,Avg_Area = mean(Site_Area)
                        ,EB_Area  = sd(Site_Area) / sqrt(length(unique(CK_Cadmus_ID)))
                        ,SampleSize = length(unique(CK_Cadmus_ID))
)

item9.final <- rbind.data.frame(item9.sum2, item9.sum3, stringsAsFactors = F)
