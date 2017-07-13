#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
# Item 161: DISTRIBUTION OF FRAME WALL INSULATION LEVELS, ELECTRICALLY HEATED HOMES (SF table B-6)
#############################################################################################
#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item161.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item161.dat.11 <- item161.dat.1[which(item161.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item161.dat.12 <- item161.dat.11[which(item161.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item161.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item161.dat.12$Primary.Heating.System)
item161.dat.12$count <- 1

item161.dat.13 <- unique(item161.dat.12[which(item161.dat.12$Heating.Fuel == "Electric"),])

item161.sum1 <- summarise(group_by(item161.dat.13, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item161.sum1$Count <- 1
which(duplicated(item161.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(item161.sum1$Heating.Fuel)

item161.mechanical <- item161.sum1







#############################################################################################
#
#For Envelope information
#
#############################################################################################
item161.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Floor.Type"
                                                               ,"Floor.Sub-Type"))]

item161.dat0 <- left_join(item161.dat, item161.mechanical, by = "CK_Cadmus_ID")
item161.dat0.1 <- item161.dat0[which(item161.dat0$Heating.Fuel == "Electric"),]

item161.dat1 <- left_join(rbsa.dat, item161.dat0.1, by = "CK_Cadmus_ID")
length(unique(item161.dat1$CK_Cadmus_ID))#601

#subset to only single family homes
item161.dat2 <- item161.dat1[which(item161.dat1$BuildingType == "Single Family"),]
item161.dat2$count <- 1

item161.dat3 <- item161.dat2[which(item161.dat2$Floor.Type == "Basement"),]
item161.dat3$cond.ind <- 0
item161.dat3$cond.ind[which(item161.dat3$`Floor.Sub-Type` == "Conditioned")] <- 1

#summarise -- State JUST Basement
item161.sum1 <- summarise(group_by(item161.dat3, BuildingType, State)
                         , BSMTCount = sum(count))
#summarise -- State All Sites Basement
item161.sum2 <- summarise(group_by(item161.dat2, BuildingType, State)
                         , SampleSize = length(unique(CK_Cadmus_ID)))

#summarise -- Region JUST Basement
item161.sum3 <- summarise(group_by(item161.dat3, BuildingType)
                         , State = "Region"
                         , BSMTCount = sum(count))
#summarise -- Region All Sites Basement
item161.sum4 <- summarise(group_by(item161.dat2, BuildingType)
                         , State = "Region"
                         , SampleSize = length(unique(CK_Cadmus_ID)))

## rbind state and region sample sizes
item161.tmp1 <- left_join(item161.sum1, item161.sum2, by = c("BuildingType", "State"))
item161.tmp2 <- left_join(item161.sum3, item161.sum4, by = c("BuildingType", "State"))

item161.final <- rbind.data.frame(item161.tmp1, item161.tmp2, stringsAsFactors = F)

item161.final$Percent <- item161.final$BSMTCount / item161.final$SampleSize
item161.final$SE <- sqrt(item161.final$Percent * (1 - item161.final$Percent) / item161.final$SampleSize)

