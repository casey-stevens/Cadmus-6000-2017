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





#############################################################################################
#Item 279: DISTRIBUTION OF PRIMARY IN-UNIT HEATING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
#############################################################################################
item279.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"System.Type"
                                                                    ,"Heating.Fuel"
                                                                    ,"Primary.Heating.System"))]

#subset to only buidling level information
item279.dat0 <- item279.dat[-grep("BLDG",item279.dat$CK_SiteID),]

#merge on mechanical data with rbsa cleaned data
item279.dat1 <- left_join(rbsa.dat, item279.dat0, by = "CK_Cadmus_ID")

#subset to only multifamily units
item279.dat2 <- item279.dat1[grep("Multifamily",item279.dat1$BuildingType),]

#subset to only primary heating rows
item279.dat3 <- unique(item279.dat2[which(item279.dat2$Primary.Heating.System == "Yes"),])
which(duplicated(item279.dat3$CK_Cadmus_ID))
item279.dat3$count <- 1

#remove NA in heating fuel types
item279.dat4 <- item279.dat3[which(!(item279.dat3$Heating.Fuel %in% c(NA, "Unknown", "Other"))),]
item279.dat4$Heating.Fuel[which(item279.dat4$Heating.Fuel == "Natural gas")] <- "Natural Gas"

#Summarise by system type
#by heating fuel
item279.sum1 <- summarise(group_by(item279.dat4, System.Type, Heating.Fuel)
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#across heating fuel
item279.sum2 <- summarise(group_by(item279.dat4, System.Type)
                          ,Heating.Fuel = "All Types"
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#Summarise across system type
#by heating fuel
item279.sum3 <- summarise(group_by(item279.dat4, Heating.Fuel)
                          ,System.Type = "All Systems"
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#across heating fuel
item279.sum4 <- summarise(group_by(item279.dat4)
                          ,Heating.Fuel = "All Types"
                          ,System.Type = "All Systems"
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

item279.final <- rbind.data.frame(item279.sum1,item279.sum2,item279.sum3,item279.sum4, stringsAsFactors = F)
item279.final$Total.Count <- item279.sum4$Count
item279.final$Denom.SampleSize <- item279.sum4$SampleSize

item279.final$Percent <- item279.final$Count / item279.final$Total.Count
item279.final$SE <- sqrt(item279.final$Percent * (1 - item279.final$Percent) / item279.final$Denom.SampleSize)

item279.cast <- dcast(setDT(item279.final)
                      ,formula = System.Type ~ Heating.Fuel
                      ,value.var = c("Percent", "SE", "SampleSize"))
item279.cast[is.na(item279.cast)] <- 0

item279.table <- data.frame("Primary.Heating.System" = item279.cast$System.Type
                            ,"Electric" = item279.cast$Percent_Electric
                            ,"Electric.SE" = item279.cast$SE_Electric
                            ,"Gas" = item279.cast$`Percent_Natural Gas`
                            ,"Gas.SE" = item279.cast$`SE_Natural Gas`
                            ,"All.Types" = item279.cast$`Percent_All Types`
                            ,"All.Types.SE" = item279.cast$`SE_All Types`
                            ,"SampleSize" = item279.cast$`SampleSize_All Types`)
















#############################################################################################
#Item 280: DISTRIBUTION OF SECONDARY IN-UNIT HEATING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
#############################################################################################
item280.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"System.Type"
                                                                    ,"Heating.Fuel"
                                                                    ,"Primary.Heating.System"))]

#subset to only buidling level information
item280.dat0 <- item280.dat[-grep("BLDG",item280.dat$CK_SiteID),]

#merge on mechanical data with rbsa cleaned data
item280.dat1 <- left_join(rbsa.dat, item280.dat0, by = "CK_Cadmus_ID")

#subset to only multifamily units
item280.dat2 <- item280.dat1[grep("Multifamily",item280.dat1$BuildingType),]

#subset to only primary heating rows
item280.dat3 <- unique(item280.dat2[which(item280.dat2$Primary.Heating.System == "No"),])
which(duplicated(item280.dat3$CK_Cadmus_ID))
item280.dat3$count <- 1

#remove NA in heating fuel types
item280.dat4 <- item280.dat3[which(!(item280.dat3$Heating.Fuel %in% c(NA, "Unknown", "Other"))),]
item280.dat4$Heating.Fuel[which(item280.dat4$Heating.Fuel == "Natural gas")] <- "Natural Gas"
item280.dat4$Heating.Fuel[grep("Wood|wood", item280.dat4$Heating.Fuel)] <- "Wood"

#Summarise by system type
#by heating fuel
item280.sum1 <- summarise(group_by(item280.dat4, System.Type, Heating.Fuel)
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#across heating fuel
item280.sum2 <- summarise(group_by(item280.dat4, System.Type)
                          ,Heating.Fuel = "All Types"
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#Summarise across system type
#by heating fuel
item280.sum3 <- summarise(group_by(item280.dat4, Heating.Fuel)
                          ,System.Type = "All Systems"
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#across heating fuel
item280.sum4 <- summarise(group_by(item280.dat4)
                          ,Heating.Fuel = "All Types"
                          ,System.Type = "All Systems"
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

item280.final <- rbind.data.frame(item280.sum1,item280.sum2,item280.sum3,item280.sum4, stringsAsFactors = F)
item280.final$Total.Count <- item280.sum4$Count
item280.final$Denom.SampleSize <- item280.sum4$SampleSize

item280.final$Percent <- item280.final$Count / item280.final$Total.Count
item280.final$SE <- sqrt(item280.final$Percent * (1 - item280.final$Percent) / item280.final$Denom.SampleSize)

item280.cast <- dcast(setDT(item280.final)
                      ,formula = System.Type ~ Heating.Fuel
                      ,value.var = c("Percent", "SE", "SampleSize"))
item280.cast[is.na(item280.cast)] <- 0

item280.table <- data.frame("Primary.Heating.System" = item280.cast$System.Type
                            ,"Electric" = item280.cast$Percent_Electric
                            ,"Electric.SE" = item280.cast$SE_Electric
                            ,"Gas" = item280.cast$`Percent_Natural Gas`
                            ,"Gas.SE" = item280.cast$`SE_Natural Gas`
                            ,"Wood" = item280.cast$Percent_Wood
                            ,"Wood.SE" = item280.cast$SE_Wood
                            ,"Propane" = item280.cast$Percent_Propane
                            ,"Propane.SE" = item280.cast$SE_Propane
                            ,"All.Types" = item280.cast$`Percent_All Types`
                            ,"All.Types.SE" = item280.cast$`SE_All Types`
                            ,"SampleSize" = item280.cast$`SampleSize_All Types`)















#############################################################################################
#Item 281: DISTRIBUTION OF SECONDARY IN-UNIT COOLING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
#############################################################################################
item281.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"System.Type"
                                                                    ,"Primary.Cooling.System"))]

#subset to only buidling level information
item281.dat0 <- item281.dat[-grep("BLDG",item281.dat$CK_SiteID),]


#subset to only primary Cooling rows
item281.dat1 <- unique(item281.dat0[which(item281.dat0$Primary.Cooling.System == "Yes"),])
which(duplicated(item281.dat1$CK_Cadmus_ID))

#merge on mechanical data with rbsa cleaned data
item281.dat2 <- left_join(rbsa.dat, item281.dat1, by = "CK_Cadmus_ID")

#subset to only multifamily units
item281.dat3 <- item281.dat2[grep("Multifamily",item281.dat2$BuildingType),]
item281.dat3$Cooling.Count <- 0
item281.dat3$Cooling.Count[which(item281.dat3$Primary.Cooling.System == "Yes")] <- 1
item281.dat3$count <- 1

#Summarise by building size
item281.sum1 <- summarise(group_by(item281.dat3, BuildingTypeXX)
                          ,Percent = sum(Cooling.Count) / sum(count)
                          ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#across building size
item281.sum2 <- summarise(group_by(item281.dat3)
                          ,BuildingTypeXX = "All Sizes"
                          ,Percent = sum(Cooling.Count) / sum(count)
                          ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))


item281.final <- rbind.data.frame(item281.sum1,item281.sum2,stringsAsFactors = F)














#############################################################################################
#Item 282: DISTRIBUTION OF SECONDARY IN-UNIT COOLING SYSTEMS BY SYSTEM AND FUEL TYPE (MF Table 71)
#############################################################################################
item282.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"System.Type"
                                                                    ,"Primary.Cooling.System"))]

#subset to only buidling level information
item282.dat0 <- item282.dat[-grep("BLDG",item282.dat$CK_SiteID),]


#subset to only primary Cooling rows
item282.dat1 <- unique(item282.dat0[which(item282.dat0$Primary.Cooling.System == "Yes"),])
which(duplicated(item282.dat1$CK_Cadmus_ID))

#merge on mechanical data with rbsa cleaned data
item282.dat2 <- left_join(rbsa.dat, item282.dat1, by = "CK_Cadmus_ID")

#subset to only multifamily units
item282.dat3 <- item282.dat2[grep("Multifamily",item282.dat2$BuildingType),]
item282.dat3$Cooling.Count <- 0
item282.dat3$Cooling.Count[which(item282.dat3$Primary.Cooling.System == "Yes")] <- 1
item282.dat3$count <- 1

item282.dat4 <- item282.dat3[which(item282.dat3$System.Type != 0),]


#Summarise by system type
#by building size
item282.sum1 <- summarise(group_by(item282.dat4, System.Type, BuildingTypeXX)
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#across building size
item282.sum2 <- summarise(group_by(item282.dat4, System.Type)
                          ,BuildingTypeXX = "All Sizes"
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#Summarise across system type
#by building size
item282.sum3 <- summarise(group_by(item282.dat4, BuildingTypeXX)
                          ,System.Type = "All Systems"
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#across building size
item282.sum4 <- summarise(group_by(item282.dat4)
                          ,BuildingTypeXX = "All Sizes"
                          ,System.Type = "All Systems"
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

item282.final <- rbind.data.frame(item282.sum1,item282.sum2,item282.sum3,item282.sum4, stringsAsFactors = F)
item282.final$Total.Count <- item282.sum4$Count
item282.final$Denom.SampleSize <- item282.sum4$SampleSize

item282.final$Percent <- item282.final$Count / item282.final$Total.Count
item282.final$SE <- sqrt(item282.final$Percent * (1 - item282.final$Percent) / item282.final$Denom.SampleSize)

item282.cast <- dcast(setDT(item282.final)
                      ,formula = System.Type ~ BuildingTypeXX
                      ,value.var = c("Percent", "SE", "SampleSize"))
item282.cast[is.na(item282.cast)] <- 0

item282.table <- data.frame("Primary.Cooling.System" = item282.cast$System.Type
                            ,"Low.Rise.1.3" = item282.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.SE" = item282.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6" = item282.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.SE" = item282.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.GT6" = item282.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High.Rise.GT6.SE" = item282.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"All.Sizes" = item282.cast$`Percent_All Sizes`
                            ,"All.Types.SE" = item282.cast$`SE_All Sizes`
                            ,"SampleSize" = item282.cast$`SampleSize_All Sizes`)
