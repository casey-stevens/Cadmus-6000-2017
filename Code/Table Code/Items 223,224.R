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



#############################################################################################
# Item 223: PERCENTAGE OF BUILDINGS WITH NON-RESIDENTIAL USES BY BUILDING SIZE (MF table 15)
#############################################################################################
item223.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"
                                                                  ,""))]

colnames(item223.dat) <- c("CK_Cadmus_ID"
                           ,"Commercial.Area"
                           ,"Nonres.Grocery.SQFT"
                           ,"Nonres.Office.SQFT"
                           ,"Nonres.Other.SQFT"
                           ,"Nonres.Retail.SQFT"
                           ,"Nonres.Vacant.SQFT")

item223.dat[is.na(item223.dat)] <- 0

for (i in 2:ncol(item223.dat)){
  item223.dat[,i] <- as.numeric(as.character(item223.dat[,i]))
}

item223.dat$CommercialAreaFlag <- ifelse(item223.dat$Nonres.Grocery.SQFT + 
                                           item223.dat$Nonres.Office.SQFT +
                                           item223.dat$Nonres.Other.SQFT +
                                           item223.dat$Nonres.Retail.SQFT +
                                           item223.dat$Nonres.Vacant.SQFT +
                                           item223.dat$Commercial.Area > 0,1,0)

item223.dat1 <- left_join(item223.dat, rbsa.dat, by = "CK_Cadmus_ID")
item223.dat2 <- item223.dat1[grep("Multifamily", item223.dat1$BuildingType),]


item223.summary1 <-summarise(group_by(item223.dat2, BuildingTypeXX)
                             ,SampleSize = length(unique(CK_Cadmus_ID))
                             ,TotalWithCommercArea = sum(CommercialAreaFlag))

item223.summary2  <- summarise(group_by(item223.dat2)
                               ,BuildingTypeXX = "All Sizes"
                               ,SampleSize = length(unique(CK_Cadmus_ID))
                               ,TotalWithCommercArea = sum(CommercialAreaFlag)) 


item223.final <- rbind.data.frame(item223.summary1,item223.summary2)

item223.final$Percent <- item223.final$TotalWithCommercArea / item223.final$SampleSize
item223.final$SE <- sqrt(item223.final$Percent * (1 - item223.final$Percent) / item223.final$SampleSize)

item223.sub <- item223.final[,which(!(colnames(item223.final) %in% c("TotalWithCommercArea","SampleSize")))]
item223.table <- data.frame(item223.sub,"SampleSize" = item223.final$SampleSize,stringsAsFactors = F)

#############################################################################################
# Item 224: DISTRIBUTION OF NON-RESIDENTIAL FLOOR AREA (IN BUILDINGS WITH NON-RESIDENTIAL) BY USE TYPE AND BUILDING SIZE (MF table 16)
#############################################################################################
item224.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Cadmus_ID"
                                                                  # ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"
                                                                  ,""))]

colnames(item224.dat) <- c("CK_Cadmus_ID"
                           # ,"Commercial.Area"
                           ,"Grocery"
                           ,"Office"
                           ,"Other"
                           ,"Retail"
                           ,"Vacant")

item224.dat[is.na(item224.dat)] <- 0

for (i in 2:ncol(item224.dat)){
  item224.dat[,i] <- as.numeric(as.character(item224.dat[,i]))
}

item224.dat$Total.Area <- item224.dat$Grocery + item224.dat$Office + item224.dat$Other + item224.dat$Retail + item224.dat$Vacant

item224.melt <- melt(item224.dat, id = c("CK_Cadmus_ID","Total.Area"))
colnames(item224.melt) <- c("CK_Cadmus_ID", "Total.Area", "Nonres.Type", "Area")

item224.merge <- left_join(item224.melt, rbsa.dat, by = "CK_Cadmus_ID")

#subset to only MF sites
item224.dat1 <- item224.merge[which(item224.merge$BuildingTypeXX %in% c("Apartment Building (3 or fewer floors)"
                                                                        ,"Apartment Building (4 to 6 floors)"
                                                                        ,"Apartment Building (More than 6 floors)")),]

#remove any zeros 
item224.dat2 <- item224.dat1[which(item224.dat1$Total.Area != 0),]
item224.dat2$count <- 0
item224.dat2$count[which(item224.dat2$Area > 0)] <- 1

#summarise by nonresidential type, across housing sizes
item224.sum1 <- summarise(group_by(item224.dat2, Nonres.Type)
                         ,BuildingTypeXX = "All Sizes"
                         ,Percent = sum(Area) / sum(Total.Area)
                         ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
                         ,SampleSize = length(unique(CK_Cadmus_ID)))

#summarise by nonresidential type and housing sizes
item224.sum2 <- summarise(group_by(item224.dat2, BuildingTypeXX, Nonres.Type)
                         ,Percent = sum(Area) / sum(Total.Area)
                         ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
                         ,SampleSize = length(unique(CK_Cadmus_ID)))

#row bind
item224.final <- rbind.data.frame(item224.sum1, item224.sum2, stringsAsFactors = F)

item224.cast <- dcast(setDT(item224.final)
                      ,formula = Nonres.Type ~ BuildingTypeXX
                      ,value.var = c("Percent","SE"))

item224.table <- data.frame("Nonresidential_Use_Type" = item224.cast$Nonres.Type
                            ,"Low_Rise_1.3_Percent" = item224.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_SE" = item224.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_4.6_Percent" = item224.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_SE" = item224.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_7Plus_Percent" = item224.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE" = item224.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_Percent" = item224.cast$`Percent_All Sizes`
                            ,"All_Sizes_SE" = item224.cast$`SE_All Sizes`
                            ,"SampleSize" = item224.sum1$SampleSize)
                            