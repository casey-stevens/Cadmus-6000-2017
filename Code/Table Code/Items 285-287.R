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
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

mechanical.dat2 <- mechanical.dat[,which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                       ,"CK_SiteID"
                                                                       ,"DHW.Type"
                                                                       ,"DHW.Provided.by.Heating.System"
                                                                       ,"DHW.Size.(Gallons)"
                                                                       ,"DHW.Fuel"
                                                                       ,"DHW.Year.Manufactured"
                                                                       ,"System.Type"
                                                                       ,"Category"))]

mechanical.dat3 <- left_join(mechanical.dat2, rbsa.dat, by = "CK_Cadmus_ID")

mechanical.dat4 <- mechanical.dat3[which(mechanical.dat3$CK_Cadmus_ID != "CK_CADMUS_ID"),]
mechanical.dat5 <- mechanical.dat4[grep("Multifamily", mechanical.dat4$BuildingType),]

#############################################################################################
#Item 285: DISTRIBUTION OF UNIT WATER HEATERS BY TYPE  (MF Table 77)
#############################################################################################

item285.dat <- mechanical.dat5[,which(colnames(mechanical.dat5) %in% c("CK_Cadmus_ID"
                                                                     ,"CK_SiteID"
                                                                     ,"DHW.Type"
                                                                     ,"DHW.Provided.by.Heating.System"
                                                                     ,"Category"
                                                                     ,"System.Type"))]

item285.dat1 <- item285.dat[which((!is.na(item285.dat$System.Type) | 
                                    !is.na(item285.dat$DHW.Type)) &
                                    item285.dat$Category == "DHW"),]

item285.dat2 <- item285.dat1[-grep("bldg",item285.dat1$CK_SiteID,ignore.case = T),]

item285.dat3 <- unique(item285.dat2)
item285.dat3$count <- 1
######################################
#Summarize system types
######################################

item285.sum1 <- summarise(group_by(item285.dat3,DHW.Type),
                          count = sum(count),
                          SampleSize = length(unique(CK_Cadmus_ID)))
                          
item285.sum1$SampleSize <- sum(item285.sum1$SampleSize)

item285.sum1$Percent <- item285.sum1$count / item285.sum1$SampleSize
item285.sum1$SE <- sqrt(item285.sum1$Percent * (1 - item285.sum1$Percent) / item285.sum1$SampleSize)

item285.final <- data.frame("Heater Type" = item285.sum1$DHW.Type,
                            "Percent" = item285.sum1$Percent,
                            "SE" = item285.sum1$SE,
                            "n" = item285.sum1$count)

#############################################################################################
#Item 286: DISTRIBUTION OF IN-UNIT WATER HEATER TANKS BY SIZE AND FUEL TYPE (MF Table 78)
#############################################################################################

item286.dat <- mechanical.dat5[,which(colnames(mechanical.dat5) %in% c("CK_Cadmus_ID"
                                                                       ,"CK_SiteID"
                                                                       ,"DHW.Size.(Gallons)"
                                                                       ,"DHW.Fuel"
                                                                       ,"Category"))]

item286.dat1 <- item286.dat[-grep("bldg",item286.dat$CK_SiteID,ignore.case = T),]
item286.dat2 <- item286.dat1[which(item286.dat1$Category == "DHW"),]

item286.dat3 <- item286.dat2[which(!is.na(item286.dat2$`DHW.Size.(Gallons)`) &
                                     !is.na(item286.dat2$DHW.Fuel) &
                                     item286.dat2$DHW.Fuel != "Unknown" &
                                     item286.dat2$`DHW.Size.(Gallons)` != "Unknown"),]

item286.dat3$Count <- 1
item286.dat3$TankSize <- NA
item286.dat3$TankSize[which(item286.dat3$`DHW.Size.(Gallons)` >= 0 &
                              item286.dat3$`DHW.Size.(Gallons)` <= 55)] <-  "0-55"
item286.dat3$TankSize[which(item286.dat3$`DHW.Size.(Gallons)` > 55)] <- ">55"

unique(item286.dat3$TankSize)
######################################
#Summarize BY FUEL AND SIZE
######################################

item286.sum1 <- summarise(group_by(item286.dat3,TankSize, DHW.Fuel),
                          SizeCount = sum(Count))
                        
item286.sum2 <-  summarise(group_by(item286.dat3, TankSize),
                           DHW.Fuel = "All Types",
                           SizeCount = sum(Count))  

item286.sum3 <- rbind.data.frame(item286.sum1,item286.sum2,stringsAsFactors = F)

item286.sum4 <- summarise(group_by(item286.sum3, DHW.Fuel),
                          n = sum(SizeCount))  

item286.sum5 <-  summarise(group_by(item286.dat3, DHW.Fuel),
                           TankSize = "All Sizes",
                           SizeCount = sum(Count))  

item286.sum6 <- rbind.data.frame(item286.sum3,item286.sum5,stringsAsFactors = F)
item286.sum7 <- left_join(item286.sum6,item286.sum4, by = "DHW.Fuel")


item286.sum7$SampleSize <- unique(item286.sum7$n[which(item286.sum7$DHW.Fuel == "All Types")])

item286.table <- item286.sum7

item286.table$Percent <- item286.table$SizeCount / item286.table$SampleSize
item286.table$SE <- sqrt(item286.table$Percent * (1 - item286.table$Percent) / item286.table$SampleSize)

item286.table2 <- dcast(setDT(item286.table),
                        formula = DHW.Fuel + n ~ TankSize,
                        value.var = c("Percent", "SE"))


item286.final <- data.frame("WaterHeaterFuel" = item286.table2$DHW.Fuel,
                            "0 to 55" = item286.table2$`Percent_0-55`,
                            "0 to 55 SE" = item286.table2$`SE_0-55`,
                            "Greater than 55" = item286.table2$`Percent_>55`,
                            "Greater than 55 SE" = item286.table2$`SE_>55`,
                            "All Sizes" = item286.table2$`Percent_All Sizes`,
                            "All Sizes SE" = item286.table2$`SE_All Sizes`,
                            "n" = item286.table2$n)

#############################################################################################
#Item 287: DISTRIBUTION OF IN-UNIT WATER HEATERS BY VINTAGE  (MF Table 78)
#############################################################################################

item287.dat <- mechanical.dat5[,which(colnames(mechanical.dat5) %in% c("CK_Cadmus_ID"
                                                                       ,"CK_SiteID"
                                                                       ,"DHW.Type"
                                                                       ,"DHW.Fuel"
                                                                       ,"Category"
                                                                       ,"DHW.Year.Manufactured"))]


item287.dat2 <- item287.dat[-grep("bldg",item287.dat$CK_SiteID,ignore.case = T),]
item287.dat3 <- item287.dat2[which(!is.na(item287.dat2$DHW.Type)),]
item287.dat3$Year <- as.numeric(as.character(item287.dat3$DHW.Year.Manufactured))

item287.dat4 <- item287.dat3[which(!is.na(item287.dat3$Year)),]

item287.dat4$Vintage <- NA
item287.dat4$Vintage[which(item287.dat4$Year < 1990)] <- "Pre_1990"
item287.dat4$Vintage[which(item287.dat4$Year > 1990 & item287.dat4$Year < 1999)] <- "1990_1999"
item287.dat4$Vintage[which(item287.dat4$Year >= 2000 & item287.dat4$Year <= 2004)] <- "2000_2004"
item287.dat4$Vintage[which(item287.dat4$Year >= 2005 & item287.dat4$Year <= 2009)] <- "2005_2009"
item287.dat4$Vintage[which(item287.dat4$Year > 2009)] <- "Post_2009"

item287.dat4$Count <- 1
######################################
#Summarize BY FUEL AND SIZE
######################################


item287.sum1 <- summarise(group_by(item287.dat4, Vintage),
                          Count = sum(Count))

item287.sum1$SampleSize <- sum(item287.sum1$Count)

item287.table <- item287.sum1

item287.table$Percent <- item287.table$Count / item287.table$SampleSize
item287.table$SE <- sqrt(item287.table$Percent * (1 - item287.table$Percent) / item287.table$SampleSize)

item287.final <- data.frame("Vintage" = item287.table$Vintage,
                            "Percent" = item287.table$Percent,
                            "SE" = item287.table$SE,
                            "n" = item287.table$Count)

