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
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))



#############################################################################################
#Item 229: DISTRIBUTION OF STRUCTURAL SYSTEM TYPES BY BUILDING SIZE (MF Table 21)
#############################################################################################
#subset to columns needed for analysis
item229.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                ,"ENV_Construction_CONSTRUCTION_ConstructionType"
                                                                ,"ENV_Construction_BLDG_STRUCTURE_TypeOfFrame"))]
item229.dat$count <- 1

item229.dat0 <- item229.dat[which(item229.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item229.dat1 <- left_join(item229.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#Subset to Multifamily
item229.dat2 <- item229.dat1[grep("Multifamily", item229.dat1$BuildingType),]

item229.dat3 <- item229.dat2#[which(!(item229.dat2$ENV_Construction_CONSTRUCTION_ConstructionType %in% c("Unknown", NA, "N/A", "-- Datapoint not asked for --"))),]
item229.dat4 <- item229.dat3[which(!(item229.dat3$ENV_Construction_BLDG_STRUCTURE_TypeOfFrame %in% c("Unknown", NA, "N/A", "-- Datapoint not asked for --"))),]

item229.dat4$Structural.System <- paste(item229.dat4$ENV_Construction_CONSTRUCTION_ConstructionType, item229.dat4$ENV_Construction_BLDG_STRUCTURE_TypeOfFrame, sep = " ")
item229.dat4$count <- 1

#summarise by building height
#by structural system
item229.sum1 <- summarise(group_by(item229.dat4, BuildingTypeXX, Structural.System)
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#summarise across building height
#by structural system
item229.sum2 <- summarise(group_by(item229.dat4, Structural.System)
                          ,BuildingTypeXX = "All Sizes"
                          ,Count = sum(count)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#row bind
item229.merge <- rbind.data.frame(item229.sum1, item229.sum2, stringsAsFactors = F)





















#############################################################################################
#Item 230: DISTRIBUTION OF WALL AREA BY STRUCTURAL SYSTEM AND WALL TYPE IN RIGID FRAME BUILDINGS (MF Table 22)
#############################################################################################
#subset to columns needed for analysis
item230.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                ,"ENV_Construction_CONSTRUCTION_ConstructionType"
                                                                ,"ENV_Construction_BLDG_STRUCTURE_TypeOfFrame"
                                                                ,"Wall.Type"))]
item230.dat$count <- 1

item230.dat0 <- item230.dat[which(item230.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item230.dat1 <- left_join(item230.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#Subset to Multifamily
item230.dat2 <- item230.dat1[grep("Multifamily", item230.dat1$BuildingType),]

