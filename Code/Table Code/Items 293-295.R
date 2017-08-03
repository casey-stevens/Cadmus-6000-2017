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
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))






#############################################################################################
#Item 293: AVERAGE NUMBER OF APPLIANCES PER HOME BY TYPE (MF table 86)
#############################################################################################
#subset to columns needed for analysis
item293.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Iteration"
                                                                   ,"Type"
                                                                   ,"Large.Unusual.Load.Quantity"
                                                                   ,""))]
item293.dat$count <- 1

item293.dat00 <- item293.dat[-grep("BLDG",item293.dat$Iteration),]

item293.dat0 <- item293.dat00[which(item293.dat00$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item293.dat1 <- left_join(item293.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item293.dat2 <- item293.dat1[grep("Multifamily", item293.dat1$BuildingType),]

item293.dat2$Large.Unusual.Load.Quantity[which(is.na(item293.dat2$Large.Unusual.Load.Quantity))] <- 1
unique(item293.dat2$Large.Unusual.Load.Quantity)
item293.dat2$Large.Unusual.Load.Quantity <- as.numeric(as.character(item293.dat2$Large.Unusual.Load.Quantity))


item293.dat2$TotalQty <- item293.dat2$Large.Unusual.Load.Quantity * item293.dat2$count

item293.sum <- summarise(group_by(item293.dat2, CK_Cadmus_ID, Type)
                        ,SiteCount = sum(TotalQty))

#get sample sizes within building type categories
item293.sampleSize <- summarise(group_by(item293.sum)
                               ,SampleSize = length(unique(CK_Cadmus_ID)))

item293.merge <- cbind.data.frame(item293.sum, item293.sampleSize)

#summarise by type
item293.final <- summarise(group_by(item293.merge, Type)
                          ,SampleSize = unique(SampleSize)
                          ,Mean = sum(SiteCount) / SampleSize
                          ,SE   = sd(SiteCount) / sqrt(SampleSize))


item293.table <- data.frame("Type" = item293.final$Type
                           ,"Mean" = item293.final$Mean
                           ,"SE" = item293.final$SE
                           ,"SampleSize" = item293.final$SampleSize)

item293.table1 <- item293.table[which(item293.table$Type %in% c("Washer"
                                                                ,"Dishwasher"
                                                                ,"Dryer"
                                                                ,"Freezer"
                                                                ,"Refrigerator")),]









#############################################################################################
#Item 294: DISTRIBUTION OF IN-UNIT REFRIGERATORS BY TYPE (MF table 88)
#############################################################################################
item294.dat <- item293.sum

item294.dat1 <- item294.dat[which(item294.dat$Type == "Refrigerator"),]













#############################################################################################
#Item 294: DISTRIBUTION OF IN-UNIT REFRIGERATORS BY TYPE (MF table 90)
#############################################################################################
item295.dat <- item293.sum

item295.dat1 <- item295.dat[which(item295.dat$Type == "Refrigerator"),]
