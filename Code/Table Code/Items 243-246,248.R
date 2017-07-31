#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"CK_SiteID"
                                                                        ,"System.Type"
                                                                        ,"Heating.Fuel",
                                                                        "Heat.Iteration",
                                                                        "Cool.Iteration",
                                                                        "Primary.Heating.System",
                                                                        "Primary.Cooling.System"))]

mechanical.dat2  <- left_join(rbsa.dat, mechanical.dat1, by = "CK_Cadmus_ID")
length(unique(mechanical.dat2$CK_Cadmus_ID)) 
#Subset to MF
mechanical.dat3 <- mechanical.dat2[grep("Multifamily", mechanical.dat2$BuildingType),]

#############################################################################################
#Item 243: DISTRIBUTION OF PRIMARY HEATING SYSTEMS BY FUEL / Table 35
#############################################################################################
item243.dat <- mechanical.dat3[which(!is.na(mechanical.dat3$Heat.Iteration)),]

#remove datapoint not asked for and repeated header lines
item243.dat1 <- item243.dat[which(item243.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item243.dat2 <- item243.dat1[which(item243.dat1$Primary.Heating.System %in% c("Yes", "No")),]

unique(item243.dat2$Primary.Heating.System)

item243.dat2$Heating.System.Ind <- item243.dat2$Primary.Heating.System
item243.dat2$Heating.System.Ind[which(item243.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item243.dat2$Heating.System.Ind[which(item243.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"
unique(item243.dat2$Heating.Fuel)
item243.dat2$Fuel <- item243.dat2$Heating.Fuel
item243.dat2$Fuel[grep("wood",item243.dat2$Fuel, ignore.case = T)] <- "Wood"
unique(item243.dat2$System.Type[which(is.na(item243.dat2$Fuel)|
                          item243.dat2$Fuel == "Other" |
                          item243.dat2$Fuel == "Unknown")])
item243.dat2$Fuel[grep("hp", item243.dat2$System.Type, ignore.case = T)] <- "Electric"
item243.dat2$Fuel[grep("natural gas", item243.dat2$Fuel, ignore.case = T)] <- "Natural Gas"
item243.dat2$Fuel[is.na(item243.dat2$Fuel)] <- "Unknown"
unique(item243.dat2$Fuel)

item243.dat3 <- unique(data.frame("CK_Cadmus_ID" = item243.dat2$CK_Cadmus_ID
                                 ,"Heating_System" = item243.dat2$System.Type
                                 ,"Fuel" = item243.dat2$Fuel
                                 ,"Primary_Secondary" = item243.dat2$Heating.System.Ind,
                                 stringsAsFactors = F))

item243.dat4 <- item243.dat3[which(item243.dat3$Primary_Secondary == "Primary Heating System"),]
length(unique(item243.dat4$CK_Cadmus_ID)) #544
item243.dat4$count <- 1

#################
#Make summary by System Type and Fuel
#################

item243.sum1 <- summarise(group_by(item243.dat4, Heating_System,Fuel)
                          ,Count = sum(count)) 
item243.sum2 <- summarise(group_by(item243.dat4,Fuel)
                          ,Heating_System = "All Systems"
                          ,Count = sum(count))

item243.Combined1 <- rbind.data.frame(item243.sum1,
                                      item243.sum2,
                                      stringsAsFactors = F) 
item243.sum3 <- summarise(group_by(item243.dat4, Heating_System)
                          ,n = sum(count))  
item243.sum4 <- summarise(group_by(item243.dat4, Heating_System)
                         ,Count = sum(count)
                         ,Fuel = "All Types")
item243.Combined2 <- rbind.data.frame(item243.Combined1,item243.sum4, stringsAsFactors = F )

item243.Combined3 <- left_join(item243.Combined2,item243.sum3, by = "Heating_System")
item243.Combined3$n[which(item243.Combined3$Heating_System == "All Systems")] <- 
  sum(item243.Combined3$Count[which(item243.Combined3$Heating_System == "All Systems")])

item243.Combined3$SampleSize <- 
  sum(item243.Combined3$Count[which(item243.Combined3$Heating_System == "All Systems")])

item243.Combined3$Percent <- item243.Combined3$Count / item243.Combined3$SampleSize
item243.Combined3$SE <- sqrt(item243.Combined3$Percent * (1 - item243.Combined3$Percent) / item243.Combined3$SampleSize)

detach(package:reshape2)
library(data.table)

item243.table <- dcast(setDT(item243.Combined3)
                          , formula = Heating_System + n ~ Fuel
                          , value.var = c("Percent", "SE"))

item243.final <- data.frame("Heating_System" = item243.table$Heating_System,
                            "Electric" = item243.table$Percent_Electric,
                            "Electric SE" = item243.table$SE_Electric,
                            "Natural Gas" = item243.table$`Percent_Natural Gas`,
                            "Natural Gas SE" = item243.table$`SE_Natural Gas`,
                            "Other" = item243.table$Percent_Other,
                            "Other SE" = item243.table$SE_Other,
                            "Unknown" = item243.table$Percent_Unknown,
                            "Unknown SE" = item243.table$SE_Unknown,
                            "All Types" = item243.table$`Percent_All Types`,
                            "All Types SE" = item243.table$`SE_All Types`,
                            "n" = item243.table$n) 

#############################################################################################
#Item 244: DISTRIBUTION OF PRIMARY HEATING SYSTEMS BY BUILDING/ Table 36
#############################################################################################
item244.dat <- mechanical.dat3[which(!is.na(mechanical.dat3$Heat.Iteration)),]

#remove datapoint not asked for and repeated header lines
item244.dat1 <- item244.dat[which(item244.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item244.dat2 <- item244.dat1[which(item244.dat1$Primary.Heating.System %in% c("Yes", "No")),]

unique(item244.dat2$Primary.Heating.System)

item244.dat2$Heating.System.Ind <- item244.dat2$Primary.Heating.System
item244.dat2$Heating.System.Ind[which(item244.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item244.dat2$Heating.System.Ind[which(item244.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"
unique(item244.dat2$Heating.Fuel)
item244.dat2$Fuel <- item244.dat2$Heating.Fuel
item244.dat2$Fuel[grep("wood",item244.dat2$Fuel, ignore.case = T)] <- "Wood"
unique(item244.dat2$System.Type[which(is.na(item244.dat2$Fuel)|
                                        item244.dat2$Fuel == "Other" |
                                        item244.dat2$Fuel == "Unknown")])
item244.dat2$Fuel[grep("hp", item244.dat2$System.Type, ignore.case = T)] <- "Electric"
item244.dat2$Fuel[grep("natural gas", item244.dat2$Fuel, ignore.case = T)] <- "Natural Gas"
item244.dat2$Fuel[is.na(item244.dat2$Fuel)] <- "Unknown"
unique(item244.dat2$Fuel)

item244.dat3 <- unique(data.frame("CK_Cadmus_ID" = item244.dat2$CK_Cadmus_ID
                                  ,"Heating_System" = item244.dat2$System.Type
                                  ,"BuildingTypeXX" = item244.dat2$BuildingTypeXX
                                  ,"Primary_Secondary" = item244.dat2$Heating.System.Ind,
                                  stringsAsFactors = F))

item244.dat4 <- item244.dat3[which(item244.dat3$Primary_Secondary == "Primary Heating System"),]
length(unique(item244.dat4$CK_Cadmus_ID)) #544
item244.dat4$count <- 1

#################
#Make summary by System Type and Building
#################

#Get counts of heating systems
item244.sum1 <- summarise(group_by(item244.dat4, Heating_System,BuildingTypeXX)
 
                                                   ,count = sum(count))
item244.sum2 <- summarise(group_by(item244.dat4, Heating_System)
                          ,BuildingTypeXX = "All Sizes"
                          ,count = sum(count))

item244.Combined1 <- rbind.data.frame(item244.sum1,item244.sum2,stringsAsFactors = F)

#Get Sample Sizes
item244.sum3 <- summarise(group_by(item244.dat4,BuildingTypeXX)
                          ,SampleSize = sum(count))
item244.sum4 <- summarise(group_by(item244.dat4)
                          ,BuildingTypeXX = "All Sizes"
                          ,SampleSize = sum(count))
item244.Combined2 <- rbind.data.frame(item244.sum3,item244.sum4,stringsAsFactors = F)

#Get n's
item244.sum5 <- summarise(group_by(item244.dat4, Heating_System)
                          ,n = sum(count))

item244.Combined3 <- left_join(item244.Combined1,
                               item244.Combined2,
                               by = c("BuildingTypeXX"))
item244.Combined4 <- left_join(item244.Combined3,
                               item244.sum5,
                               by = c("Heating_System"))


item244.Combined4$Percent <- item244.Combined4$count / item244.Combined4$SampleSize
item244.Combined4$SE <- sqrt(item244.Combined4$Percent * (1 - item244.Combined4$Percent) / item244.Combined4$SampleSize)

detach(package:reshape2)
library(data.table)

item244.table <- dcast(setDT(item244.Combined4)
                       , formula = Heating_System + n ~ BuildingTypeXX
                       , value.var = c("Percent", "SE"))

item244.final <- data.frame("Heating_System" = item244.table$Heating_System,
                            "Low Rise (1-3)" = item244.table$`Percent_Apartment Building (3 or fewer floors)`,
                            "Low Rise (1-3) SE" = item244.table$`SE_Apartment Building (3 or fewer floors)`,
                            "Mid Rise (4-6)" = item244.table$`Percent_Apartment Building (4 to 6 floors)`,
                            "Mid Rise (4-6) SE" = item244.table$`SE_Apartment Building (4 to 6 floors)`,
                            "High Rise (7+)" = item244.table$`Percent_Apartment Building (More than 6 floors)`,
                            "High Rise (7+) SE" = item244.table$`SE_Apartment Building (More than 6 floors)`,
                            "All Sizes" = item244.table$`Percent_All Sizes`,
                            "All Sizes SE" = item244.table$`SE_All Sizes`,
                            "n" = item244.table$n) 
#############################################################################################
#Item 245: DISTRIBUTION OF SECONDARY HEATING SYSTEMS BY FUEL/ Table 37
#############################################################################################
item245.dat <- mechanical.dat3[which(!is.na(mechanical.dat3$Heat.Iteration)),]

#remove datapoint not asked for and repeated header lines
item245.dat1 <- item245.dat[which(item245.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item245.dat2 <- item245.dat1[which(item245.dat1$Primary.Heating.System %in% c("Yes", "No")),]

unique(item245.dat2$Primary.Heating.System)

item245.dat2$Heating.System.Ind <- item245.dat2$Primary.Heating.System
item245.dat2$Heating.System.Ind[which(item245.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item245.dat2$Heating.System.Ind[which(item245.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"
unique(item245.dat2$Heating.Fuel)
item245.dat2$Fuel <- item245.dat2$Heating.Fuel
item245.dat2$Fuel[grep("wood",item245.dat2$Fuel, ignore.case = T)] <- "Wood"
unique(item245.dat2$System.Type[which(is.na(item245.dat2$Fuel)|
                                        item245.dat2$Fuel == "Other" |
                                        item245.dat2$Fuel == "Unknown")])
item245.dat2$Fuel[grep("hp", item245.dat2$System.Type, ignore.case = T)] <- "Electric"
item245.dat2$Fuel[grep("natural gas", item245.dat2$Fuel, ignore.case = T)] <- "Natural Gas"
item245.dat2$Fuel[is.na(item245.dat2$Fuel)] <- "Unknown"
unique(item245.dat2$Fuel)

item245.dat3 <- unique(data.frame("CK_Cadmus_ID" = item245.dat2$CK_Cadmus_ID
                                  ,"Heating_System" = item245.dat2$System.Type
                                  ,"Fuel" = item245.dat2$Fuel
                                  ,"Primary_Secondary" = item245.dat2$Heating.System.Ind,
                                  stringsAsFactors = F))

item245.dat4 <- item245.dat3[which(item245.dat3$Primary_Secondary == "Secondary Heating System"),]
length(unique(item245.dat4$CK_Cadmus_ID)) #544
item245.dat4$count <- 1


#################
#Make summary by System Type and Fuel
#################

item245.sum1 <- summarise(group_by(item245.dat4, Heating_System,Fuel)
                          ,Count = sum(count)) 
item245.sum2 <- summarise(group_by(item245.dat4,Fuel)
                          ,Heating_System = "All Systems"
                          ,Count = sum(count))

item245.Combined1 <- rbind.data.frame(item245.sum1,
                                      item245.sum2,
                                      stringsAsFactors = F) 
item245.sum3 <- summarise(group_by(item245.dat4, Heating_System)
                          ,n = sum(count))  
item245.sum4 <- summarise(group_by(item245.dat4, Heating_System)
                          ,Count = sum(count)
                          ,Fuel = "All Types")
item245.Combined2 <- rbind.data.frame(item245.Combined1,item245.sum4, stringsAsFactors = F )

item245.Combined3 <- left_join(item245.Combined2,item245.sum3, by = "Heating_System")
item245.Combined3$n[which(item245.Combined3$Heating_System == "All Systems")] <- 
  sum(item245.Combined3$Count[which(item245.Combined3$Heating_System == "All Systems")])

item245.Combined3$SampleSize <- 
  sum(item245.Combined3$Count[which(item245.Combined3$Heating_System == "All Systems")])

item245.Combined3$Percent <- item245.Combined3$Count / item245.Combined3$SampleSize
item245.Combined3$SE <- sqrt(item245.Combined3$Percent * (1 - item245.Combined3$Percent) / item245.Combined3$SampleSize)

detach(package:reshape2)
library(data.table)

item245.table <- dcast(setDT(item245.Combined3)
                       , formula = Heating_System + n ~ Fuel
                       , value.var = c("Percent", "SE"))

item245.final <- data.frame("Heating_System" = item245.table$Heating_System,
                            "Electric" = item245.table$Percent_Electric,
                            "Electric SE" = item245.table$SE_Electric,
                            "Natural Gas" = item245.table$`Percent_Natural Gas`,
                            "Natural Gas SE" = item245.table$`SE_Natural Gas`,
                            "Other" = item245.table$Percent_Other,
                            "Other SE" = item245.table$SE_Other,
                            "Unknown" = item245.table$Percent_Unknown,
                            "Unknown SE" = item245.table$SE_Unknown,
                            "All Types" = item245.table$`Percent_All Types`,
                            "All Types SE" = item245.table$`SE_All Types`,
                            "n" = item245.table$n) 

#############################################################################################
#item246: DISTRIBUTION OF PRIMARY HEATING SYSTEMS BY BUILDING/ Table 38
#############################################################################################
item246.dat <- mechanical.dat3[which(!is.na(mechanical.dat3$Heat.Iteration)),]

#remove datapoint not asked for and repeated header lines
item246.dat1 <- item246.dat[which(item246.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item246.dat2 <- item246.dat1[which(item246.dat1$Primary.Heating.System %in% c("Yes", "No")),]

unique(item246.dat2$Primary.Heating.System)

item246.dat2$Heating.System.Ind <- item246.dat2$Primary.Heating.System
item246.dat2$Heating.System.Ind[which(item246.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item246.dat2$Heating.System.Ind[which(item246.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"
unique(item246.dat2$Heating.Fuel)
item246.dat2$Fuel <- item246.dat2$Heating.Fuel
item246.dat2$Fuel[grep("wood",item246.dat2$Fuel, ignore.case = T)] <- "Wood"
unique(item246.dat2$System.Type[which(is.na(item246.dat2$Fuel)|
                                        item246.dat2$Fuel == "Other" |
                                        item246.dat2$Fuel == "Unknown")])
item246.dat2$Fuel[grep("hp", item246.dat2$System.Type, ignore.case = T)] <- "Electric"
item246.dat2$Fuel[grep("natural gas", item246.dat2$Fuel, ignore.case = T)] <- "Natural Gas"
item246.dat2$Fuel[is.na(item246.dat2$Fuel)] <- "Unknown"
unique(item246.dat2$Fuel)

item246.dat3 <- unique(data.frame("CK_Cadmus_ID" = item246.dat2$CK_Cadmus_ID
                                  ,"Heating_System" = item246.dat2$System.Type
                                  ,"BuildingTypeXX" = item246.dat2$BuildingTypeXX
                                  ,"Primary_Secondary" = item246.dat2$Heating.System.Ind,
                                  stringsAsFactors = F))

item246.dat4 <- item246.dat3[which(item246.dat3$Primary_Secondary == "Secondary Heating System"),]
length(unique(item246.dat4$CK_Cadmus_ID)) #544
item246.dat4$count <- 1

#################
#Make summary by System Type and Building
#################

#Get counts of heating systems
item246.sum1 <- summarise(group_by(item246.dat4, Heating_System,BuildingTypeXX)
                          
                          ,count = sum(count))
item246.sum2 <- summarise(group_by(item246.dat4, Heating_System)
                          ,BuildingTypeXX = "All Sizes"
                          ,count = sum(count))

item246.Combined1 <- rbind.data.frame(item246.sum1,item246.sum2,stringsAsFactors = F)

#Get Sample Sizes
item246.sum3 <- summarise(group_by(item246.dat4,BuildingTypeXX)
                          ,SampleSize = sum(count))
item246.sum4 <- summarise(group_by(item246.dat4)
                          ,BuildingTypeXX = "All Sizes"
                          ,SampleSize = sum(count))
item246.Combined2 <- rbind.data.frame(item246.sum3,item246.sum4,stringsAsFactors = F)

#Get n's
item246.sum5 <- summarise(group_by(item246.dat4, Heating_System)
                          ,n = sum(count))

item246.Combined3 <- left_join(item246.Combined1,
                               item246.Combined2,
                               by = c("BuildingTypeXX"))
item246.Combined4 <- left_join(item246.Combined3,
                               item246.sum5,
                               by = c("Heating_System"))


item246.Combined4$Percent <- item246.Combined4$count / item246.Combined4$SampleSize
item246.Combined4$SE <- sqrt(item246.Combined4$Percent * (1 - item246.Combined4$Percent) / item246.Combined4$SampleSize)

detach(package:reshape2)
library(data.table)

item246.table <- dcast(setDT(item246.Combined4)
                       , formula = Heating_System + n ~ BuildingTypeXX
                       , value.var = c("Percent", "SE"))

item246.final <- data.frame("Heating_System" = item246.table$Heating_System,
                            "Low Rise (1-3)" = item246.table$`Percent_Apartment Building (3 or fewer floors)`,
                            "Low Rise (1-3) SE" = item246.table$`SE_Apartment Building (3 or fewer floors)`,
                            "Mid Rise (4-6)" = item246.table$`Percent_Apartment Building (4 to 6 floors)`,
                            "Mid Rise (4-6) SE" = item246.table$`SE_Apartment Building (4 to 6 floors)`,
                            "High Rise (7+)" = item246.table$`Percent_Apartment Building (More than 6 floors)`,
                            "High Rise (7+) SE" = item246.table$`SE_Apartment Building (More than 6 floors)`,
                            "All Sizes" = item246.table$`Percent_All Sizes`,
                            "All Sizes SE" = item246.table$`SE_All Sizes`,
                            "n" = item246.table$n) 


#############################################################################################
#Item 248: DISTRIBUTION OF UNIT COOLING SYSTEMS / Table 40
#############################################################################################

item248.dat <- mechanical.dat3
item248.dat <- unique(mechanical.dat3[,c("CK_Cadmus_ID",
                                         "Cool.Iteration",
                                         "CK_SiteID",
                                         "System.Type")])

item248.dat$CoolingInd <- 0
item248.dat$CoolingInd[which(!is.na(item248.dat$Cool.Iteration))] <- 1

item248.dat1 <- data.frame(
  summarise(group_by(item248.dat,CK_Cadmus_ID,System.Type),
            CoolingInd = sum(CoolingInd)),stringsAsFactors = F )
item248.dat2 <- data.frame(
  summarise(group_by(item248.dat1,CK_Cadmus_ID),
            CoolingSum = sum(CoolingInd)),stringsAsFactors = F )

item248.dat1$SystemType[which(item248.dat1$CoolingInd == 0)] <- "No Cooling"
item248.dat1$SystemType[which(item248.dat1$CoolingInd > 0)] <- 
  item248.dat1$System.Type[which(item248.dat1$CoolingInd > 0)]

item248.dat3 <- unique(item248.dat1[,c("CK_Cadmus_ID","SystemType")])

item248.dat3 <- left_join(item248.dat3,item248.dat2, by = "CK_Cadmus_ID")

item248.dat4 <- item248.dat3[-which(item248.dat3$CoolingSum > 0 &
                                      item248.dat3$SystemType == "No Cooling"),]

item248.dat4$Count <- 1
#################
#Make summary by System Type and Building
#################
item248.sum1 <- summarise(group_by(item248.dat4, SystemType)
                          ,Count = sum(Count))

item248.sum2 <- summarise(group_by(item248.dat4)
                          ,SampleSize = sum(Count))

item248.combined1 <-item248.sum1 
item248.combined1$SampleSize <- item248.sum2$SampleSize
item248.combined1$Percent <- item248.combined1$Count / item248.combined1$SampleSize
item248.combined1$SE <- sqrt(item248.combined1$Percent * (1 - item248.combined1$Percent) / item248.combined1$SampleSize)

item248.table <- item248.combined1
item248.final <- data.frame("CoolingSystem" = item248.table$SystemType,
                            "Percent" = item248.table$Percent,
                            "SE" = item248.table$SE,
                            "n" = item248.table$Count,
                            stringsAsFactors = F)
  
)
