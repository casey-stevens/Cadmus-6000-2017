#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
room.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
room.dat$CK_Cadmus_ID <- trimws(toupper(room.dat$CK_Cadmus_ID))


##########################################################################
# Item 7: AVERAGE NUMBER OF BEDROOMS PER HOME BY STATE (SF Table 14)
##########################################################################
######################################################
# Weighting Implementation function: Mean, one group
######################################################

#subset to columns need in table
room.tmp <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID", "Iteration", "Clean.Type", "Area"))]
item7.dat <- left_join(rbsa.dat, room.tmp, by = "CK_Cadmus_ID")

#remove building information
item7.dat1 <- item7.dat[-grep("BLDG", item7.dat$Iteration),]

#subset to only bedrooms
item7.dat2 <- item7.dat1[which(item7.dat1$Clean.Type == "Bedroom"),]
item7.dat2$count <- 1

# Summarise up to the customer level
item7.customer <- summarise(group_by(item7.dat2
                                     , CK_Cadmus_ID
                                     , BuildingType
                                     , State)
                            ,CountRooms = sum(count))

item7.merge <- left_join(rbsa.dat, item7.customer)

# apply weights to the subset of the data
item7.data <- weightedData(item7.merge[-which(colnames(item7.merge) == "CountRooms")])
#merge back on measured variable
item7.data <- left_join(item7.data, item7.customer[which(colnames(item7.customer) %in% c("CK_Cadmus_ID", "CountRooms"))])
item7.data$count <- 1

# Perform analysis for mean, one group
item7.final <- mean_one_group(CustomerLevelData = item7.data
                              , valueVariable = 'CountRooms'
                              , byVariable    = 'State'
                              , aggregateRow  = 'Region')


#subset by home type
item7.final.SF <- item7.final[which(item7.final$BuildingType == "Single Family"),]
item7.final.MH <- item7.final[which(item7.final$BuildingType == "Manufactured"),]


#export data
exportTable(item7.final.SF, "SF", "Table 14")
exportTable(item7.final.MH, "MH", "Table 12")



#####################################################################################
# Item 8: AVERAGE NUMBER OF BATHROOMS PER HOME BY STATE (SF Table 15, MH Table 13)
#####################################################################################
######################################################
# Weighting Implementation function: Mean, one group
######################################################

#subset to columns need in table
room.tmp  <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID", "Iteration", "Clean.Type", "Area"))]
item8.dat <- left_join(rbsa.dat, room.tmp, by = "CK_Cadmus_ID")

#remove building information
item8.dat1 <- item8.dat[-grep("BLDG", item8.dat$Iteration),]

#subset to only bedrooms
item8.dat2 <- item8.dat1[which(item8.dat1$Clean.Type == "Bathroom"),]
item8.dat2$count <- 1


# Summarise up to the customer level
item8.customer <- summarise(group_by(item8.dat2
                                     , CK_Cadmus_ID
                                     , BuildingType
                                     , State)
                            ,CountRooms = sum(count))

# apply weights to the subset of the data
item8.data <- weightedData(item8.customer[-which(colnames(item8.customer) == "CountRooms")])
#merge back on measured variable
item8.data <- left_join(item8.data, item8.customer[which(colnames(item8.customer) %in% c("CK_Cadmus_ID", "CountRooms"))])
item8.data$count <- 1

# Perform analysis for mean, one group
item8.final <- mean_one_group(CustomerLevelData = item8.data
                              , valueVariable = 'CountRooms'
                              , byVariable    = 'State'
                              , aggregateRow  = 'Region')


#subset by home type
item8.final.SF <- item8.final[which(item8.final$BuildingType == "Single Family"),]
item8.final.MH <- item8.final[which(item8.final$BuildingType == "Manufactured"),]


#export data
exportTable(item8.final.SF, "SF", "Table 14")
exportTable(item8.final.MH, "MH", "Table 12")




##############################################################################################################################
# Item 9: AVERAGE ROOM AREAS BY ROOM TYPE (SF Table 16)
##############################################################################################################################
#subset to columns need in table
item9.dat <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID", "Iteration", "Clean.Type", "Area"))]
item9.dat0 <- left_join(rbsa.dat, item9.dat, by = "CK_Cadmus_ID")

item9.dat0$count <- 1
item9.dat0$Area <- as.numeric(as.character(item9.dat0$Area))

#average within houses
item9.sum <- summarise(group_by(item9.dat0, CK_Cadmus_ID, BuildingType, Clean.Type)
          ,Site_Area = mean(Area)
          )
#remove missing area information
item9.dat1 <- item9.sum[which(!(is.na(item9.sum$Site_Area))),]

#average across houses
item9.sum1 <- summarise(group_by(item9.dat1, BuildingType, Clean.Type)
                        ,Mean = mean(Site_Area)
                        ,SE  = sd(Site_Area) / sqrt(length(unique(CK_Cadmus_ID)))
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        )
item9.sum2 <- item9.sum1[which(item9.sum1$Clean.Type %in% c("Bathroom"
                                                             ,"Bedroom"
                                                             ,"Closet"
                                                             ,"Dining Room"
                                                             ,"Family Room"
                                                             ,"Garage"
                                                             ,"Hall"
                                                             ,"Kitchen"
                                                             ,"Laundry Room"
                                                             ,"Living Room"
                                                             ,"Master Bedroom"
                                                             ,"Office"
                                                             ,"Other")),]
item9.sum3 <- summarise(group_by(item9.dat1, BuildingType)
                        ,Clean.Type = "All Room Types"
                        ,Mean = mean(Site_Area)
                        ,SE  = sd(Site_Area) / sqrt(length(unique(CK_Cadmus_ID)))
                        ,SampleSize = length(unique(CK_Cadmus_ID))
)

item9.final <- rbind.data.frame(item9.sum2, item9.sum3, stringsAsFactors = F)

item9.final.SF <- item9.final[which(item9.final$BuildingType == 'Single Family'),
                              -which(colnames(item9.final) == 'BuildingType')]

workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep = "/"))
writeData(workbook.SF, sheet = "Table 16", x = item9.final.SF, startRow = 20)
saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)
