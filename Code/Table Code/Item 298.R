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
#Item 298: IN-UNIT LAUNDRY CHARACTERISTICS (MF Table 92)
#############################################################################################
#subset to correct columns for analysis
item298.dat <- sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                              ,"CK_SiteID"
                                                                              ,"INTRVW_CUST_RES_HomeandEnergyUseHome_ClothesWasherLoadsPerWeek"
                                                                              ,"INTRVW_CUST_RES_HomeandEnergyUseHome_PercentOfLoadsThatGoInDryer"
                                                                              ,""))]
#rename columns
colnames(item298.dat) <- c("CK_Cadmus_ID"
                           ,"CK_Site_ID"
                           ,"Clothes.Washer.Loads.per.Week"
                           ,"Dryer.Loads.per.Washer.Load")

#remove NA values for clothes washers
item298.dat1 <- unique(item298.dat[which(!(is.na(item298.dat$Clothes.Washer.Loads.per.Week))),])
#view if any IDs are duplicated (none should be)
which(duplicated(item298.dat1$CK_Cadmus_ID))

#join cleaned RBSA data onto subsetted data for item
item298.dat2 <- left_join(item298.dat1, rbsa.dat, by = "CK_Cadmus_ID")

#subset to only multifamily units
item298.dat3 <- item298.dat2[grep("Multifamily", item298.dat2$BuildingType),]
#view unique values of clothes washers
unique(item298.dat3$Clothes.Washer.Loads.per.Week)

#remove any sites that do not have clothes washers
item298.dat4 <- item298.dat3[which(item298.dat3$Clothes.Washer.Loads.per.Week != "No Washing Machine"),]
item298.dat4$Clothes.Washer.Loads.per.Week <- as.numeric(as.character(item298.dat4$Clothes.Washer.Loads.per.Week))
item298.dat4$Dryer.Loads.per.Washer.Load <- as.numeric(as.character(item298.dat4$Dryer.Loads.per.Washer.Load))

#Summarise to obtain the average number of washer loads per week
item298.sum1 <- summarise(item298.dat4
                          ,Category = "Clothes Washer Loads per Week" 
                          ,Mean = mean(Clothes.Washer.Loads.per.Week)
                          ,SE = sd(Clothes.Washer.Loads.per.Week) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

# summarise the percent of those loads that go into the dryer
item298.sum2 <- summarise(item298.dat4
                          ,Category = "Dryer Loads per Washer Load"
                          ,Mean = mean(Dryer.Loads.per.Washer.Load/100) #make dryer loads into percent
                          ,SE = sd(Dryer.Loads.per.Washer.Load/100) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#combine into one table
item298.final <- rbind.data.frame(item298.sum1, item298.sum2, stringsAsFactors = F)

