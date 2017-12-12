#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

##  Create "Not In" operator
"%notin%" <- Negate("%in%")

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) 

#Read in data for analysis
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))



#############################################################################################
#Item 115: PERCENTAGE OF HOMES WITH GAMING SYSTEMS (SF table 122, MH table 97)
#############################################################################################
#subset to columns needed for analysis
item115.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]
item115.dat$count <- 1

#remove any repeat header rows from exporting
item115.dat0 <- item115.dat[which(item115.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item115.dat1 <- left_join(item115.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item115.dat1$Type)
#subset to only Game COnsoles

item115.dat2 <- item115.dat1[which(item115.dat1$Type == "Game Console"),]
item115.dat2$Ind <- 1

item115.sum <- summarise(group_by(item115.dat2, CK_Cadmus_ID)
                         ,Ind = sum(Ind))

item115.sum$Ind <- 1
#QAQC
which(duplicated(item115.sum$CK_Cadmus_ID))


item115.merge <- left_join(rbsa.dat, item115.sum)
item115.merge$Ind[which(is.na(item115.merge$Ind))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
item115.data <- weightedData(item115.merge[-which(colnames(item115.merge) %in% c("Ind"))])
item115.data <- left_join(item115.data, item115.merge[which(colnames(item115.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])
item115.data$Count <- 1
#######################
# Weighted Analysis
#######################
item115.final <- proportions_one_group(CustomerLevelData = item115.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name       = 'Region'
                                       ,columnName       = 'Remove')

item115.final.SF <- item115.final[which(item115.final$BuildingType == "Single Family")
                                  ,-which(colnames(item115.final) %in% c("BuildingType"
                                                                         ,"Remove"))]
item115.final.MH <- item115.final[which(item115.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item115.final) %in% c("BuildingType"
                                                                         ,"Remove"))]

exportTable(item115.final.SF, "SF", "Table 122", weighted = TRUE)
exportTable(item115.final.MH, "MH", "Table 97", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item115.final <- proportions_one_group(CustomerLevelData = item115.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name       = 'Region'
                                       ,columnName       = 'Remove'
                                       ,weighted         = FALSE)

item115.final.SF <- item115.final[which(item115.final$BuildingType == "Single Family")
                                  ,-which(colnames(item115.final) %in% c("BuildingType"
                                                                         ,"Remove"
                                                                         ,"Total.Count"))]
item115.final.MH <- item115.final[which(item115.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item115.final) %in% c("BuildingType"
                                                                         ,"Remove"
                                                                         ,"Total.Count"))]

exportTable(item115.final.SF, "SF", "Table 122", weighted = FALSE)
exportTable(item115.final.MH, "MH", "Table 97", weighted = FALSE)






#############################################################################################
#Item 116: AVERAGE NUMBER OF GAMING SYSTEMS PER HOME WITH GAMING SYSTEMS (SF table 123, MH table 98)
#############################################################################################
# NOTE: for this table, this is FOR THE HOMES WITH GAMING SYSTEMS, what is the average number of systems
#############################################################################################
#subset to columns needed for analysis
item116.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]
item116.dat$count <- 1

#remove any repeat header rows from exporting
item116.dat0 <- item116.dat[which(item116.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item116.dat1 <- left_join(item116.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item116.dat1$Type)
#subset to only Game COnsoles

item116.dat2 <- item116.dat1[which(item116.dat1$Type == "Game Console"),]
item116.dat2$Ind <- 1

item116.sum <- summarise(group_by(item116.dat2, CK_Cadmus_ID)
                         ,Ind = sum(Ind))

item116.merge <- left_join(rbsa.dat, item116.sum)
item116.merge <- item116.merge[which(!is.na(item116.merge$Ind)),]

################################################
# Adding pop and sample sizes for weights
################################################
item116.data <- weightedData(item116.merge[-which(colnames(item116.merge) %in% c("Ind"))])
item116.data <- left_join(item116.data, item116.merge[which(colnames(item116.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])

item116.data$count <- 1
item115.data$Count <- 1
#######################
# Weighted Analysis
#######################
item116.final <- mean_one_group(item116.data
                                ,valueVariable = 'Ind'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item116.final.SF <- item116.final[which(item116.final$BuildingType == "Single Family")
                                  ,-which(colnames(item116.final) %in% c("BuildingType"
                                                                         ,"Count"))]
item116.final.MH <- item116.final[which(item116.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item116.final) %in% c("BuildingType"
                                                                         ,"Count"))]

exportTable(item116.final.SF, "SF", "Table 123", weighted = TRUE)
exportTable(item116.final.MH, "MH", "Table 98", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item116.final <- mean_one_group_unweighted(item116.data
                                           ,valueVariable = 'Ind'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item116.final.SF <- item116.final[which(item116.final$BuildingType == "Single Family")
                                  ,-which(colnames(item116.final) %in% c("BuildingType"
                                                                         ,"Count"
                                                                         ,"count"))]
item116.final.MH <- item116.final[which(item116.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item116.final) %in% c("BuildingType"
                                                                         ,"Count"
                                                                         ,"count"))]

exportTable(item116.final.SF, "SF", "Table 123", weighted = FALSE)
exportTable(item116.final.MH, "MH", "Table 98", weighted = FALSE)






#############################################################################################
#Item 117: AVERAGE NUMBER OF COMPUTERS PER HOME WITH COMPUTERS (SF table 124, MH table 99)
#############################################################################################
# NOTE: for this table, this is FOR THE HOMES WITH COMPUTERS, what is the average number of computers
#############################################################################################
#subset to columns needed for analysis
item117.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]
item117.dat$count <- 1

#remove any repeat header rows from exporting
item117.dat0 <- item117.dat[which(item117.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item117.dat1 <- left_join(item117.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item117.dat1$Type)
#subset to only computers

item117.dat2 <- item117.dat1[which(item117.dat1$Type %in% c("Laptop", "Desktop")),]
item117.dat2$Ind <- 1

item117.sum <- summarise(group_by(item117.dat2, CK_Cadmus_ID)
                         ,Site.Count = sum(Ind, na.rm = T))

item117.merge <- left_join(rbsa.dat, item117.sum)
item117.merge <- item117.merge[which(!is.na(item117.merge$Site.Count)),]


################################################
# Adding pop and sample sizes for weights
################################################
item117.data <- weightedData(item117.merge[-which(colnames(item117.merge) %in% c("Ind"
                                                                                 ,"Site.Count"))])
item117.data <- left_join(item117.data, item117.merge[which(colnames(item117.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"
                                                                                           ,"Site.Count"))])

item117.data$Count <- 1
#######################
# Weighted Analysis
#######################
item117.final <- mean_one_group(item117.data
                                ,valueVariable = 'Site.Count'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item117.final.SF <- item117.final[which(item117.final$BuildingType == "Single Family")
                                  ,-which(colnames(item117.final) %in% c("BuildingType"))]
item117.final.MH <- item117.final[which(item117.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item117.final) %in% c("BuildingType"))]

exportTable(item117.final.SF, "SF", "Table 124", weighted = TRUE)
exportTable(item117.final.MH, "MH", "Table 99", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item117.final <- mean_one_group_unweighted(item117.data
                                           ,valueVariable = 'Site.Count'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item117.final.SF <- item117.final[which(item117.final$BuildingType == "Single Family")
                                  ,-which(colnames(item117.final) %in% c("BuildingType"))]
item117.final.MH <- item117.final[which(item117.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item117.final) %in% c("BuildingType"))]

exportTable(item117.final.SF, "SF", "Table 124", weighted = FALSE)
exportTable(item117.final.MH, "MH", "Table 99", weighted = FALSE)






#############################################################################################
#Item 118: PERCENTAGE OF HOMES WITH COMPUTERS BY STATE (SF table 125, MH table 100)
#############################################################################################
#subset to columns needed for analysis
item118.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]

#remove any repeat header rows from exporting
item118.dat0 <- item118.dat[which(item118.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item118.dat1 <- left_join(item118.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item118.dat1$Type)

item118.dat2 <- item118.dat1[which(item118.dat1$Type %in% c("Desktop", "Laptop")),]

item118.dat2$Ind <- 1

item118.sum <- summarise(group_by(item118.dat2, CK_Cadmus_ID)
                         ,Ind = sum(Ind))

item118.sum$Ind <- 1
#QAQC
which(duplicated(item118.sum$CK_Cadmus_ID))


item118.merge <- unique(left_join(rbsa.dat, item118.sum))
item118.merge$Ind[which(is.na(item118.merge$Ind))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
item118.data <- weightedData(item118.merge[-which(colnames(item118.merge) %in% c("Ind"))])
item118.data <- left_join(item118.data, item118.merge[which(colnames(item118.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])
item118.data$Count <- 1

#######################
# Weighted Analysis
#######################
item118.final <- proportions_one_group(CustomerLevelData = item118.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name       = 'Region')

item118.final.SF <- item118.final[which(item118.final$BuildingType == "Single Family")
                                  ,-which(colnames(item118.final) %in% c("BuildingType"))]
item118.final.MH <- item118.final[which(item118.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item118.final) %in% c("BuildingType"))]

exportTable(item118.final.SF, "SF", "Table 125", weighted = TRUE)
exportTable(item118.final.MH, "MH", "Table 100", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item118.final <- proportions_one_group(CustomerLevelData = item118.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name       = 'Region'
                                       ,weighted         = FALSE)

item118.final.SF <- item118.final[which(item118.final$BuildingType == "Single Family")
                                  ,-which(colnames(item118.final) %in% c("BuildingType"))]
item118.final.MH <- item118.final[which(item118.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item118.final) %in% c("BuildingType"))]

exportTable(item118.final.SF, "SF", "Table 125", weighted = FALSE)
exportTable(item118.final.MH, "MH", "Table 100", weighted = FALSE)






#############################################################################################
#Item 119: AVERAGE NUMBER OF AUDIO SYSTEMS PER HOME BY STATE (SF table 126, MH table 101)
#############################################################################################
#subset to columns needed for analysis
item119.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]
item119.dat$count <- 1

#remove any repeat header rows from exporting
item119.dat0 <- item119.dat[which(item119.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item119.dat1 <- left_join(item119.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item119.dat1$Type)
#subset to only Game COnsoles

item119.dat2 <- item119.dat1[which(item119.dat1$Type %in% c("Audio Equipment")),]
item119.dat2$Ind <- 1

item119.sum <- summarise(group_by(item119.dat2, CK_Cadmus_ID)
                         ,Ind = sum(Ind))

item119.merge <- left_join(rbsa.dat, item119.sum)
item119.merge <- item119.merge[which(!is.na(item119.merge$Ind)),]


################################################
# Adding pop and sample sizes for weights
################################################
item119.data <- weightedData(item119.merge[-which(colnames(item119.merge) %in% c("Ind"))])
item119.data <- left_join(item119.data, item119.merge[which(colnames(item119.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])

item119.data$count <- 1
item115.data$Count <- 1
#######################
# Weighted Analysis
#######################
item119.final <- mean_one_group(item119.data
                                ,valueVariable = 'Ind'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item119.final.SF <- item119.final[which(item119.final$BuildingType == "Single Family")
                                  ,-which(colnames(item119.final) %in% c("BuildingType"
                                                                         ,"Count"))]
item119.final.MH <- item119.final[which(item119.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item119.final) %in% c("BuildingType"
                                                                         ,"Count"))]

exportTable(item119.final.SF, "SF", "Table 126", weighted = TRUE)
exportTable(item119.final.MH, "MH", "Table 101", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item119.final <- mean_one_group_unweighted(item119.data
                                           ,valueVariable = 'Ind'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item119.final.SF <- item119.final[which(item119.final$BuildingType == "Single Family")
                                  ,-which(colnames(item119.final) %in% c("BuildingType"
                                                                         ,"Count"
                                                                         ,"count"))]
item119.final.MH <- item119.final[which(item119.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item119.final) %in% c("BuildingType"
                                                                         ,"Count"
                                                                         ,"count"))]

exportTable(item119.final.SF, "SF", "Table 126", weighted = FALSE)
exportTable(item119.final.MH, "MH", "Table 101", weighted = FALSE)







#############################################################################################
#Item 120: AVERAGE NUMBER OF SUBWOOFERS PER HOME BY TYPE (SF table 127, MH table 102)
#############################################################################################
#subset to columns needed for analysis
item120.dat <- appliances.dat[c(which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Type"
                                                                      ,"Sub-Type"
                                                                      ,"Contains.Suboofer")), grep("subwoofer", colnames(appliances.dat), ignore.case = T))]
item120.dat$count <- 1

#remove any repeat header rows from exporting
item120.dat0 <- item120.dat[which(item120.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item120.dat1 <- left_join(item120.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item120.dat1$Type)

item120.dat2 <- item120.dat1[which(item120.dat1$Contains.Suboofer %in% c("Yes")),]
item120.dat2$Ind <- 1

item120.sum <- summarise(group_by(item120.dat2, CK_Cadmus_ID, Type, `Sub-Type`)
                         ,Ind = sum(Ind))

item120.merge <- left_join(rbsa.dat, item120.sum)
item120.merge <- item120.merge[which(!is.na(item120.merge$Ind)),]



################################################
# Adding pop and sample sizes for weights
################################################
item120.data <- weightedData(item120.merge[-which(colnames(item120.merge) %in% c("Ind"))])
item120.data <- left_join(item120.data, item120.merge[which(colnames(item120.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])

item120.data$count <- 1
item115.data$Count <- 1
#######################
# Weighted Analysis
#######################
item120.final <- mean_one_group(item120.data
                                ,valueVariable = 'Ind'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item120.final.SF <- item120.final[which(item120.final$BuildingType == "Single Family")
                                  ,-which(colnames(item120.final) %in% c("BuildingType"
                                                                         ,"Count"))]
item120.final.MH <- item120.final[which(item120.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item120.final) %in% c("BuildingType"
                                                                         ,"Count"))]

exportTable(item120.final.SF, "SF", "Table 127", weighted = TRUE)
exportTable(item120.final.MH, "MH", "Table 102", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item120.final <- mean_one_group_unweighted(item120.data
                                           ,valueVariable = 'Ind'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item120.final.SF <- item120.final[which(item120.final$BuildingType == "Single Family")
                                  ,-which(colnames(item120.final) %in% c("BuildingType"
                                                                         ,"Count"
                                                                         ,"count"))]
item120.final.MH <- item120.final[which(item120.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item120.final) %in% c("BuildingType"
                                                                         ,"Count"
                                                                         ,"count"))]

exportTable(item120.final.SF, "SF", "Table 127", weighted = FALSE)
exportTable(item120.final.MH, "MH", "Table 102", weighted = FALSE)

