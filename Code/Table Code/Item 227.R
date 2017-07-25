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
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))


#############################################################################################
#Item 227: AVERAGE NUMBER OF OCCUPANTS BY AGE CATEGORY (MF Table 19)
#############################################################################################

item227.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_LessThan1"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_1_5"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_6_10"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_11_18"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_19_45"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_46_64"
                                                                                     ,"INTRVW_CUST_RES_DemographicsDemo_HowManyOfThePeopleWhoLiveHereAreAged_65Older"
                                                                                     ,""))])
colnames(item227.dat) <- c("CK_Cadmus_ID"
                           ,"Age_Less_Than_1"
                           ,"Age_1_5"
                           ,"Age_6_10"
                           ,"Age_11_18"
                           ,"Age_19_45"
                           ,"Age_46_64"
                           ,"Age_65_Older")

item227.dat$count <- 1

#remove any repeat header rows from exporting
item227.dat0 <- item227.dat[which(item227.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item227.dat1 <- left_join(item227.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item227.dat1$Age_0_18 <- item227.dat1$Age_Less_Than_1 + item227.dat1$Age_1_5 + item227.dat1$Age_6_10 + item227.dat1$Age_11_18
item227.dat1$Age_19_64 <- item227.dat1$Age_19_45 + item227.dat1$Age_46_64
item227.dat1$AllCategories <- item227.dat1$Age_0_18 + item227.dat1$Age_19_64 + item227.dat1$Age_65_Older
#Subset to Multifamily
item227.dat2 <- item227.dat1[grep("Multifamily", item227.dat1$BuildingType),]

item227.age <- summarise(item227.dat2
                           ,SampleSize_0 = length(unique(CK_Cadmus_ID)[which(Age_0_18 >0)])
                           ,SampleSize_19 = length(unique(CK_Cadmus_ID)[which(Age_19_64 >0)])
                           ,SampleSize_65 = length(unique(CK_Cadmus_ID)[which(Age_65_Older >0)])
                           ,SampleSize_All = length(unique(CK_Cadmus_ID)[which(AllCategories >0)])
                           ,Mean_0 = mean(Age_0_18)
                           ,SE_0 = sd(Age_0_18) / sqrt(SampleSize_0)
                           ,Mean_19 = mean(Age_19_64)
                           ,SE_19 = sd(Age_19_64) / sqrt(SampleSize_19)
                           ,Mean_65 = mean(Age_65_Older)
                           ,SE_65 = sd(Age_65_Older) / sqrt(SampleSize_65)
                           ,Mean_All = mean(AllCategories)
                           ,SE_All = sd(AllCategories) / sqrt(SampleSize_All))

item227.table <- data.table(item227.age)

item227.melt <- reshape(item227.table, varying = 1:12, sep = "_", direction = 'long')
item227.melt$Age.Category <- c(rep("0 to 18", 1)
                               ,rep("19 to 64", 1)
                               ,rep("Older than 65", 1)
                               ,rep("All Categories", 1))
item227.final <- data.frame("Age Category" = item227.melt$Age.Category
                            ,"Mean" = item227.melt$Mean
                            ,"SE" = item227.melt$SE
                            ,"SampleSize" = item227.melt$SampleSize
                            ,stringsAsFactors = F)

