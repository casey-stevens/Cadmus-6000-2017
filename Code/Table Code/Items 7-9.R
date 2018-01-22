#############################################################################################
##  Title:            RBSA Analysis                      
##  Authors:          Casey Stevens, Andres Roma, Diego Sosa-Coba, Cadmus Group               
##  Created:          10/2017
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
rbsa.dat <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),]
length(unique(rbsa.dat$CK_Cadmus_ID))

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
room.tmp <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID"
                                                     , "Iteration"
                                                     , "Clean.Type"
                                                     , "Area"))]
item7.dat <- left_join(rbsa.dat, room.tmp, by = "CK_Cadmus_ID")

#remove building information
item7.dat1 <- item7.dat[grep("SITE", item7.dat$Iteration),]

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
item7.merge$CountRooms[which(is.na(item7.merge$CountRooms))] <- 0

# apply weights to the subset of the data
item7.data <- weightedData(item7.merge[-which(colnames(item7.merge) == "CountRooms")])
#merge back on measured variable
item7.data <- left_join(item7.data, item7.customer[which(colnames(item7.customer) %in% c("CK_Cadmus_ID"
                                                                                         , "CountRooms"))])
item7.data$count <- 1


################################
# Weighted Analysis
################################
item7.final <- mean_one_group(CustomerLevelData = item7.data
                              , valueVariable = 'CountRooms'
                              , byVariable    = 'State'
                              , aggregateRow  = 'Region')
#subset by home type
item7.final.SF <- item7.final[which(item7.final$BuildingType == "Single Family"),-1]
item7.final.MH <- item7.final[which(item7.final$BuildingType == "Manufactured"),-1]
#export data
exportTable(item7.final.SF, "SF", "Table 14"
            , weighted = TRUE)
exportTable(item7.final.MH, "MH", "Table 12"
            , weighted = TRUE)




################################
# Unweighted Analysis
################################
item7.final <- mean_one_group_unweighted(CustomerLevelData = item7.data
                              , valueVariable = 'CountRooms'
                              , byVariable    = 'State'
                              , aggregateRow  = 'Region')
#subset by home type
item7.final.SF <- item7.final[which(item7.final$BuildingType == "Single Family"),-1]
item7.final.MH <- item7.final[which(item7.final$BuildingType == "Manufactured"),-1]
#export data
exportTable(item7.final.SF, "SF", "Table 14"
            , weighted = FALSE)
exportTable(item7.final.MH, "MH", "Table 12"
            , weighted = FALSE)





#####################################################################################
# Item 8: AVERAGE NUMBER OF BATHROOMS PER HOME BY STATE (SF Table 15, MH Table 13)
#####################################################################################
######################################################
# Weighting Implementation function: Mean, one group
######################################################

#subset to columns need in table
room.tmp  <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID"
                                                      , "Iteration"
                                                      , "Clean.Type"
                                                      , "Area"))]
item8.dat <- left_join(rbsa.dat, room.tmp, by = "CK_Cadmus_ID")

#remove building information
item8.dat1 <- item8.dat[grep("SITE", item8.dat$Iteration),]

#subset to only bedrooms
item8.dat2 <- item8.dat1[which(item8.dat1$Clean.Type == "Bathroom"),]
item8.dat2$count <- 1


# Summarise up to the customer level
item8.customer <- summarise(group_by(item8.dat2
                                     , CK_Cadmus_ID
                                     , BuildingType
                                     , State)
                            ,CountRooms = sum(count))

item8.merge <- left_join(rbsa.dat, item8.customer)
item8.merge$CountRooms[which(is.na(item8.merge$CountRooms))] <- 0

# apply weights to the subset of the data
item8.data <- weightedData(item8.merge[-which(colnames(item8.merge) == "CountRooms")])
#merge back on measured variable
item8.data <- left_join(item8.data, item8.customer[which(colnames(item8.customer) %in% c("CK_Cadmus_ID"
                                                                                         , "CountRooms"))])
item8.data$count <- 1


################################
# Weighted Analysis
################################
item8.final <- mean_one_group(CustomerLevelData = item8.data
                              , valueVariable = 'CountRooms'
                              , byVariable    = 'State'
                              , aggregateRow  = "Region")
#subset by home type
item8.final.SF <- item8.final[which(item8.final$BuildingType == "Single Family"),-1]
item8.final.MH <- item8.final[which(item8.final$BuildingType == "Manufactured"),-1]
#export data
exportTable(item8.final.SF, "SF", "Table 15"
            , weighted = TRUE)
exportTable(item8.final.MH, "MH", "Table 13"
            , weighted = TRUE)





################################
# Unweighted Analysis
################################
item8.final <- mean_one_group_unweighted(CustomerLevelData = item8.data
                              , valueVariable = 'CountRooms'
                              , byVariable    = 'State'
                              , aggregateRow  = "Region")
#subset by home type
item8.final.SF <- item8.final[which(item8.final$BuildingType == "Single Family"),-1]
item8.final.MH <- item8.final[which(item8.final$BuildingType == "Manufactured"),-1]
#export data
exportTable(item8.final.SF, "SF", "Table 15"
            , weighted = FALSE)
exportTable(item8.final.MH, "MH", "Table 13"
            , weighted = FALSE)



##############################################################################################################################
# Item 9: AVERAGE ROOM AREAS BY ROOM TYPE (SF Table 16, MH Table 14)
##############################################################################################################################
######################################################
# Weighting Implementation function: Mean, two groups
######################################################

#subset to columns need in table
room.tmp  <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID"
                                                      , "Iteration"
                                                      , "Clean.Type"
                                                      , "Area"))]
item9.dat <- left_join(rbsa.dat, room.tmp, by = "CK_Cadmus_ID")

item9.dat$count <- 1
item9.dat$Area <- as.numeric(as.character(item9.dat$Area))
#remove missing area information
item9.dat1 <- item9.dat[which(!is.na(item9.dat$Area)),]
#clean room types
unique(item9.dat1$Clean.Type)
item9.dat1$Clean.Type[which(item9.dat1$Clean.Type %in% c("Attic"
                                                         ,"Basement"
                                                         ,"Crawlspace"
                                                         ,"Crawl Space"
                                                         ,"Mechanical"
                                                         ,"Grow Room"))] <- "Other"

unique(item9.dat1$Clean.Type)


#average within houses
item9.customer <- summarise(group_by(item9.dat1
                                     , CK_Cadmus_ID
                                     , Clean.Type)
                            ,y_bar_ilk  = mean(Area)
                            ,y_ilk      = sum(Area)
                            ,m_ilk      = sum(count)
)

item9.merge <- left_join(rbsa.dat, item9.customer)
item9.merge <- item9.merge[which(!is.na(item9.merge$y_ilk)),]

# apply weights to the subset of the data
item9.data <- weightedData(item9.merge[-which(colnames(item9.merge) %in% c("y_bar_ilk"
                                                                           ,"Clean.Type"
                                                                           ,"Area"
                                                                           ,"count"
                                                                           ,"Iteration"
                                                                           ,"m_ilk"
                                                                           ,"y_ilk"
                                                                           ,"Site_Area"
                                                                           ,"Site_Count"
                                                                           ,"Site_Sum"))])
#merge back on measured variable
item9.data <- left_join(item9.data, item9.merge[which(colnames(item9.merge) %in% c("CK_Cadmus_ID"
                                                                                   ,"y_bar_ilk"
                                                                                   ,"Clean.Type"
                                                                                   ,"Area"
                                                                                   ,"count"
                                                                                   ,"Iteration"
                                                                                   ,"m_ilk"
                                                                                   ,"y_ilk"
                                                                                   ,"Site_Area"
                                                                                   ,"Site_Count"
                                                                                   ,"Site_Sum"))])
item9.data$count <- 1



################################
# Weighted Analysis
################################
item9.final <- mean_one_group_domain(CustomerLevelData = item9.data
                              , valueVariable = 'y_ilk'
                              , byVariable    = 'Clean.Type'
                              , aggregateRow  = "All Room Types")


#subset by home type
item9.final.SF <- item9.final[which(item9.final$BuildingType == "Single Family"),-1]
item9.final.MH <- item9.final[which(item9.final$BuildingType == "Manufactured"),-1]


#export data
exportTable(item9.final.SF, "SF", "Table 16"
            , weighted = TRUE)
exportTable(item9.final.MH, "MH", "Table 14"
            , weighted = TRUE)


################################
# Unweighted Analysis
################################
item9.final <- mean_one_group_unweighted(CustomerLevelData = item9.data
                              , valueVariable = 'y_bar_ilk'
                              , byVariable    = 'Clean.Type'
                              , aggregateRow  = "All Room Types")


#subset by home type
item9.final.SF <- item9.final[which(item9.final$BuildingType == "Single Family"),-1]
item9.final.MH <- item9.final[which(item9.final$BuildingType == "Manufactured"),-1]


#export data
exportTable(item9.final.SF, "SF", "Table 16"
            , weighted = FALSE)
exportTable(item9.final.MH, "MH", "Table 14"
            , weighted = FALSE)
