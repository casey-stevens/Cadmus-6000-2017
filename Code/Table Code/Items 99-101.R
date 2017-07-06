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
#Item 99: DISTRIBUTION OF ALL WATER HEATER LOCATIONS BY SPACE HEATING FUEL TYPE (SF table 106, MH table 86)
#############################################################################################
#subset to columns needed for analysis
item99.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,"DHW.Location"
                                                                   ,"Primary.Heating.System"
                                                                   ,"Heating.Fuel"))]
item99.dat$count <- 1

item99.dat0 <- item99.dat[which(item99.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item99.dat1 <- left_join(item99.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item99.ind <- item99.dat1$CK_Cadmus_ID[grep("Water Heater",item99.dat1$Generic)]
item99.dat2 <- item99.dat1[which(item99.dat1$CK_Cadmus_ID %in% item99.ind),]

item99.dat2$DHW.Location[grep("storage|Storage",item99.dat2$DHW.Location)] <- "Storage"
item99.dat2$DHW.Location[grep("outside|Outside|exterior|Exterior",item99.dat2$DHW.Location)] <- "Outside"
item99.dat2$DHW.Location[grep("Other|2&3|Mechanical",item99.dat2$DHW.Location)] <- "Other"

unique(item99.dat2$DHW.Location)
unique(item99.dat2$Primary.Heating.System)

item99.ind2 <- item99.dat1$CK_Cadmus_ID[grep("Yes|yes",item99.dat1$Primary.Heating.System)]
item99.dat3 <- item99.dat2[which(item99.dat2$CK_Cadmus_ID %in% item99.ind2),]

item99.prim.heat.source.ind <- unique(item99.dat3$Generic[which(item99.dat3$Primary.Heating.System %in% c("Yes","yes"))])

item99.dat4 <- item99.dat3[which(item99.dat3$Generic %in% c("Storage Water Heater"
                                                            ,"Instantaneous Water Heater"
                                                            ,item99.prim.heat.source.ind)),]
item99.dat5 <- item99.dat4[which(!(item99.dat4$Primary.Heating.System %in% c("No","Unknown","N/A"))),]

#summarise for primary heating system
item99.heat <- item99.dat5[which(item99.dat5$Primary.Heating.System == "Yes"),]
item99.sum1 <- summarise(group_by(item99.heat, CK_Cadmus_ID, BuildingType, Generic)
                         ,Count = sum(count))

