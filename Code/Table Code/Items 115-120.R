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
                                       ,total.name       = 'Region')

item115.final.SF <- item115.final[which(item115.final$BuildingType == "Single Family")
                                  ,-which(colnames(item115.final) %in% c("BuildingType"))]
item115.final.MH <- item115.final[which(item115.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item115.final) %in% c("BuildingType"))]

# exportTable(item115.final.SF, "SF", "Table 122", weighted = TRUE)
exportTable(item115.final.MH, "MH", "Table 97", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item115.final <- proportions_one_group(CustomerLevelData = item115.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name       = 'Region'
                                       ,weighted         = FALSE)

item115.final.SF <- item115.final[which(item115.final$BuildingType == "Single Family")
                                  ,-which(colnames(item115.final) %in% c("BuildingType"))]
item115.final.MH <- item115.final[which(item115.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item115.final) %in% c("BuildingType"))]

# exportTable(item115.final.SF, "SF", "Table 122", weighted = FALSE)
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
item116.merge$Ind[which(is.na(item116.merge$Ind))] <- 0

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
                                  ,-which(colnames(item116.final) %in% c("BuildingType"))]
item116.final.MH <- item116.final[which(item116.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item116.final) %in% c("BuildingType"))]

# exportTable(item116.final.SF, "SF", "Table 123", weighted = TRUE)
exportTable(item116.final.MH, "MH", "Table 98", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item116.final <- mean_one_group_unweighted(item116.data
                                           ,valueVariable = 'Ind'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item116.final.SF <- item116.final[which(item116.final$BuildingType == "Single Family")
                                  ,-which(colnames(item116.final) %in% c("BuildingType"))]
item116.final.MH <- item116.final[which(item116.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item116.final) %in% c("BuildingType"))]

# exportTable(item116.final.SF, "SF", "Table 123", weighted = FALSE)
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
item117.merge$Site.Count[which(is.na(item117.merge$Site.Count))] <- 0


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

# exportTable(item117.final.SF, "SF", "Table 124", weighted = TRUE)
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

# exportTable(item117.final.SF, "SF", "Table 124", weighted = FALSE)
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

# exportTable(item118.final.SF, "SF", "Table 125", weighted = TRUE)
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

# exportTable(item118.final.SF, "SF", "Table 125", weighted = FALSE)
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
item119.dat1 <- left_join(rbsa.dat, item119.dat0, by = "CK_Cadmus_ID")

item119.cast <- dcast(setDT(item119.dat1)
                      ,formula = CK_Cadmus_ID ~ Type,sum
                      ,value.var = "count")
item119.cast <- data.frame(item119.cast, stringsAsFactors = F)
item119.cast <- item119.cast[which(colnames(item119.cast) %in% c("CK_Cadmus_ID","Audio.Equipment"))]

item119.melt <- melt(item119.cast, id.vars = "CK_Cadmus_ID")
names(item119.melt) <- c("CK_Cadmus_ID", "Type","Ind")

unique(item119.melt$Type)

item119.merge <- left_join(rbsa.dat, item119.melt)


################################################
# Adding pop and sample sizes for weights
################################################
item119.data <- weightedData(item119.merge[-which(colnames(item119.merge) %in% c("Type","Ind"))])
item119.data <- left_join(item119.data, item119.merge[which(colnames(item119.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"
                                                                                           ,"Type"))])

item119.data$count <- 1
item119.data$Count <- 1
#######################
# Weighted Analysis
#######################
item119.final <- mean_one_group(item119.data
                                ,valueVariable = 'Ind'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item119.final.SF <- item119.final[which(item119.final$BuildingType == "Single Family")
                                  ,-which(colnames(item119.final) %in% c("BuildingType"))]
item119.final.MH <- item119.final[which(item119.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item119.final) %in% c("BuildingType"))]

# exportTable(item119.final.SF, "SF", "Table 126", weighted = TRUE)
exportTable(item119.final.MH, "MH", "Table 101", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item119.final <- mean_one_group_unweighted(item119.data
                                           ,valueVariable = 'Ind'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item119.final.SF <- item119.final[which(item119.final$BuildingType == "Single Family")
                                  ,-which(colnames(item119.final) %in% c("BuildingType"))]
item119.final.MH <- item119.final[which(item119.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item119.final) %in% c("BuildingType"))]

# exportTable(item119.final.SF, "SF", "Table 126", weighted = FALSE)
exportTable(item119.final.MH, "MH", "Table 101", weighted = FALSE)







#############################################################################################
#Item 120: AVERAGE NUMBER OF SUBWOOFERS PER HOME BY TYPE (SF table 127, MH table 102)
#############################################################################################
#subset to columns needed for analysis
item120.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Type"
                                                                      ,"Sub-Type"
                                                                      ,"Contains.Suboofer"
                                                                      ,"Does.Subwoofer.have.indicator.light.or.warm.to.touch"))]
item120.dat$count <- 1

#remove any repeat header rows from exporting
item120.dat0 <- item120.dat[which(item120.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item120.dat1 <- left_join(rbsa.dat, item120.dat0, by = "CK_Cadmus_ID")

unique(item120.dat1$Type)

item120.dat2 <- item120.dat1[which(item120.dat1$Contains.Suboofer %in% c("Yes")),]
unique(item120.dat2$Does.Subwoofer.have.indicator.light.or.warm.to.touch)
item120.dat3 <- item120.dat2[which(item120.dat2$Does.Subwoofer.have.indicator.light.or.warm.to.touch != "Unknown"),]

item120.dat3$Subwoofer.Type <- "Passive"
item120.dat3$Subwoofer.Type[which(item120.dat3$Does.Subwoofer.have.indicator.light.or.warm.to.touch == "Yes")] <- "Powered"


item120.merge <- left_join(rbsa.dat, item120.dat3)


item120.cast <- dcast(setDT(item120.merge)
                      ,formula = CK_Cadmus_ID~Subwoofer.Type,sum
                      ,value.var = "count")
item120.cast <- data.frame(item120.cast, stringsAsFactors = F)
item120.cast <- item120.cast[which(colnames(item120.cast) %in% c("CK_Cadmus_ID","Passive","Powered"))]


item120.melt <- melt(item120.cast,id.vars = "CK_Cadmus_ID")
names(item120.melt) <- c("CK_Cadmus_ID", "Subwoofer.Type","Subwoofer.Count")

item120.merge <- left_join(rbsa.dat, item120.melt)

################################################
# Adding pop and sample sizes for weights
################################################
item120.data <- weightedData(item120.merge[-which(colnames(item120.merge) %in% c("Subwoofer.Type"
                                                                                 ,"Subwoofer.Count"))])
item120.data <- left_join(item120.data, item120.merge[which(colnames(item120.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Subwoofer.Type"
                                                                                           ,"Subwoofer.Count"))])

item120.data$count <- 1
item120.data$Count <- 1
#######################
# Weighted Analysis
#######################
item120.final <- mean_one_group(item120.data
                                ,valueVariable = 'Subwoofer.Count'
                                ,byVariable = 'Subwoofer.Type'
                                ,aggregateRow = 'All Subwoofers')

item120.final.SF <- item120.final[which(item120.final$BuildingType == "Single Family")
                                  ,-which(colnames(item120.final) %in% c("BuildingType"))]
item120.final.MH <- item120.final[which(item120.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item120.final) %in% c("BuildingType"))]

# exportTable(item120.final.SF, "SF", "Table 127", weighted = TRUE)
exportTable(item120.final.MH, "MH", "Table 102", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item120.final <- mean_one_group_unweighted(item120.data
                                ,valueVariable = 'Subwoofer.Count'
                                ,byVariable = 'Subwoofer.Type'
                                ,aggregateRow = 'All Subwoofers')

item120.final.SF <- item120.final[which(item120.final$BuildingType == "Single Family")
                                  ,-which(colnames(item120.final) %in% c("BuildingType"))]
item120.final.MH <- item120.final[which(item120.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item120.final) %in% c("BuildingType"))]

# exportTable(item120.final.SF, "SF", "Table 127", weighted = FALSE)
exportTable(item120.final.MH, "MH", "Table 102", weighted = FALSE)










































############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
scl.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.scl.data", rundate, ".xlsx", sep = "")))
length(unique(scl.dat$CK_Cadmus_ID))
scl.dat$CK_Building_ID <- scl.dat$Category
scl.dat <- scl.dat[which(names(scl.dat) != "Category")]

#############################################################################################
#Item 115: PERCENTAGE OF HOMES WITH GAMING SYSTEMS (SF table 122, MH table 97)
#############################################################################################
#subset to columns needed for analysis
item115.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"))]
item115.os.dat$count <- 1

#remove any repeat header rows from exporting
item115.os.dat0 <- item115.os.dat[which(item115.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned scl data
item115.os.dat1 <- left_join(item115.os.dat0, scl.dat, by = "CK_Cadmus_ID")

unique(item115.os.dat1$Type)
#subset to only Game COnsoles

item115.os.dat2 <- item115.os.dat1[which(item115.os.dat1$Type == "Game Console"),]
item115.os.dat2$Ind <- 1

item115.os.sum <- summarise(group_by(item115.os.dat2, CK_Cadmus_ID)
                         ,Ind = sum(Ind))

item115.os.sum$Ind <- 1
item115.os.merge <- left_join(scl.dat, item115.os.sum)
item115.os.merge$Ind[which(is.na(item115.os.merge$Ind))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
item115.os.data <- weightedData(item115.os.merge[-which(colnames(item115.os.merge) %in% c("Ind"))])
item115.os.data <- left_join(item115.os.data, unique(item115.os.merge[which(colnames(item115.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))]))
item115.os.data$Count <- 1
#######################
# Weighted Analysis
#######################
item115.os.final <- proportions_one_group(CustomerLevelData = item115.os.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'CK_Building_ID'
                                       ,total.name       = 'Remove')
item115.os.final <- item115.os.final[which(item115.os.final$CK_Building_ID != "Remove"),]

item115.os.final.SF <- item115.os.final[which(item115.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item115.os.final) %in% c("BuildingType"))]

exportTable(item115.os.final.SF, "SF", "Table 122", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item115.os.final <- proportions_one_group(CustomerLevelData = item115.os.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'CK_Building_ID'
                                       ,total.name       = 'Remove'
                                       ,weighted         = FALSE)
item115.os.final <- item115.os.final[which(item115.os.final$CK_Building_ID != "Remove"),]

item115.os.final.SF <- item115.os.final[which(item115.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item115.os.final) %in% c("BuildingType"))]

exportTable(item115.os.final.SF, "SF", "Table 122", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Item 116: AVERAGE NUMBER OF GAMING SYSTEMS PER HOME WITH GAMING SYSTEMS (SF table 123, MH table 98)
#############################################################################################
# NOTE: for this table, this is FOR THE HOMES WITH GAMING SYSTEMS, what is the average number of systems
#############################################################################################
#subset to columns needed for analysis
item116.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]
item116.os.dat$count <- 1

#remove any repeat header rows from exporting
item116.os.dat0 <- item116.os.dat[which(item116.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned scl data
item116.os.dat1 <- left_join(item116.os.dat0, scl.dat, by = "CK_Cadmus_ID")

item116.os.dat2 <- item116.os.dat1[which(item116.os.dat1$Type == "Game Console"),]
item116.os.dat2$Ind <- 1

item116.os.sum <- summarise(group_by(item116.os.dat2, CK_Cadmus_ID)
                         ,Ind = sum(Ind))

item116.os.merge <- left_join(scl.dat, item116.os.sum)
item116.os.merge$Ind[which(is.na(item116.os.merge$Ind))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
item116.os.data <- weightedData(item116.os.merge[-which(colnames(item116.os.merge) %in% c("Ind"))])
item116.os.data <- left_join(item116.os.data, unique(item116.os.merge[which(colnames(item116.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))]))

item116.os.data$count <- 1
item116.os.data$Count <- 1
#######################
# Weighted Analysis
#######################
item116.os.final <- mean_one_group(item116.os.data
                                ,valueVariable = 'Ind'
                                ,byVariable = 'CK_Building_ID'
                                ,aggregateRow = 'Remove')
item116.os.final <- item116.os.final[which(item116.os.final$CK_Building_ID != "Remove"),]

item116.os.final.SF <- item116.os.final[which(item116.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item116.os.final) %in% c("BuildingType"))]

exportTable(item116.os.final.SF, "SF", "Table 123", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item116.os.final <- mean_one_group_unweighted(item116.os.data
                                           ,valueVariable = 'Ind'
                                           ,byVariable = 'CK_Building_ID'
                                           ,aggregateRow = 'Remove')
item116.os.final <- item116.os.final[which(item116.os.final$CK_Building_ID != "Remove"),]

item116.os.final.SF <- item116.os.final[which(item116.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item116.os.final) %in% c("BuildingType"))]

exportTable(item116.os.final.SF, "SF", "Table 123", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Item 117: AVERAGE NUMBER OF COMPUTERS PER HOME WITH COMPUTERS (SF table 124, MH table 99)
#############################################################################################
# NOTE: for this table, this is FOR THE HOMES WITH COMPUTERS, what is the average number of computers
#############################################################################################
#subset to columns needed for analysis
item117.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,""))]
item117.os.dat$count <- 1

#remove any repeat header rows from exporting
item117.os.dat0 <- item117.os.dat[which(item117.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned scl data
item117.os.dat1 <- left_join(item117.os.dat0, scl.dat, by = "CK_Cadmus_ID")

item117.os.dat2 <- item117.os.dat1[which(item117.os.dat1$Type %in% c("Laptop", "Desktop")),]
item117.os.dat2$Ind <- 1

item117.os.sum <- summarise(group_by(item117.os.dat2, CK_Cadmus_ID)
                         ,Site.Count = sum(Ind, na.rm = T))

item117.os.merge <- left_join(scl.dat, item117.os.sum)
item117.os.merge$Site.Count[which(is.na(item117.os.merge$Site.Count))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
item117.os.data <- weightedData(item117.os.merge[-which(colnames(item117.os.merge) %in% c("Ind"
                                                                                 ,"Site.Count"))])
item117.os.data <- left_join(item117.os.data, unique(item117.os.merge[which(colnames(item117.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"
                                                                                           ,"Site.Count"))]))

item117.os.data$Count <- 1
#######################
# Weighted Analysis
#######################
item117.os.final <- mean_one_group(item117.os.data
                                ,valueVariable = 'Site.Count'
                                ,byVariable = 'CK_Building_ID'
                                ,aggregateRow = 'Remove')

item117.os.final <- item117.os.final[which(item117.os.final$CK_Building_ID %notin% c("Remove","Total")),]

item117.os.final.SF <- item117.os.final[which(item117.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item117.os.final) %in% c("BuildingType"))]

exportTable(item117.os.final.SF, "SF", "Table 124", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item117.os.final <- mean_one_group_unweighted(item117.os.data
                                           ,valueVariable = 'Site.Count'
                                           ,byVariable = 'CK_Building_ID'
                                           ,aggregateRow = 'Remove')
item117.os.final <- item117.os.final[which(item117.os.final$CK_Building_ID %notin% c("Remove","Total")),]

item117.os.final.SF <- item117.os.final[which(item117.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item117.os.final) %in% c("BuildingType"))]

exportTable(item117.os.final.SF, "SF", "Table 124", weighted = FALSE, osIndicator = "SCL", OS = T)





#############################################################################################
#Item 118: PERCENTAGE OF HOMES WITH COMPUTERS BY CK_Building_ID (SF table 125, MH table 100)
#############################################################################################
#subset to columns needed for analysis
item118.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"))]

#remove any repeat header rows from exporting
item118.os.dat0 <- item118.os.dat[which(item118.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned scl data
item118.os.dat1 <- left_join(item118.os.dat0, scl.dat, by = "CK_Cadmus_ID")

item118.os.dat2 <- item118.os.dat1[which(item118.os.dat1$Type %in% c("Desktop", "Laptop")),]

item118.os.dat2$Ind <- 1

item118.os.sum <- summarise(group_by(item118.os.dat2, CK_Cadmus_ID)
                         ,Ind = sum(Ind))

item118.os.sum$Ind <- 1

item118.os.merge <- unique(left_join(scl.dat, item118.os.sum))
item118.os.merge$Ind[which(is.na(item118.os.merge$Ind))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
item118.os.data <- weightedData(item118.os.merge[-which(colnames(item118.os.merge) %in% c("Ind"))])
item118.os.data <- left_join(item118.os.data, unique(item118.os.merge[which(colnames(item118.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))]))
item118.os.data$Count <- 1

#######################
# Weighted Analysis
#######################
item118.os.final <- proportions_one_group(CustomerLevelData = item118.os.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'CK_Building_ID'
                                       ,total.name       = 'Remove')
item118.os.final <- item118.os.final[which(item118.os.final$CK_Building_ID %notin% c("Remove","Total")),]

item118.os.final.SF <- item118.os.final[which(item118.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item118.os.final) %in% c("BuildingType"))]

exportTable(item118.os.final.SF, "SF", "Table 125", weighted = TRUE, osIndicator = "SCL", OS = T)


#######################
# Unweighted Analysis
#######################
item118.os.final <- proportions_one_group(CustomerLevelData = item118.os.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'CK_Building_ID'
                                       ,total.name       = 'Remove'
                                       ,weighted         = FALSE)
item118.os.final <- item118.os.final[which(item118.os.final$CK_Building_ID %notin% c("Remove","Total")),]

item118.os.final.SF <- item118.os.final[which(item118.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item118.os.final) %in% c("BuildingType"))]

exportTable(item118.os.final.SF, "SF", "Table 125", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Item 119: AVERAGE NUMBER OF AUDIO SYSTEMS PER HOME BY CK_Building_ID (SF table 126, MH table 101)
#############################################################################################
#subset to columns needed for analysis
item119.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"))]
item119.os.dat$count <- 1
#remove any repeat header rows from exporting
item119.os.dat0 <- item119.os.dat[which(item119.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned scl data
item119.os.dat1 <- left_join(scl.dat, item119.os.dat0, by = "CK_Cadmus_ID")

item119.os.cast <- dcast(setDT(item119.os.dat1)
                      ,formula = CK_Cadmus_ID ~ Type,sum
                      ,value.var = "count")
item119.os.cast <- data.frame(item119.os.cast, stringsAsFactors = F)
item119.os.cast <- item119.os.cast[which(colnames(item119.os.cast) %in% c("CK_Cadmus_ID","Audio.Equipment"))]

item119.os.melt <- melt(item119.os.cast, id.vars = "CK_Cadmus_ID")
names(item119.os.melt) <- c("CK_Cadmus_ID", "Type","Ind")

unique(item119.os.melt$Type)

item119.os.merge <- left_join(scl.dat, item119.os.melt)

################################################
# Adding pop and sample sizes for weights
################################################
item119.os.data <- weightedData(item119.os.merge[-which(colnames(item119.os.merge) %in% c("Type","Ind"))])
item119.os.data <- left_join(item119.os.data, unique(item119.os.merge[which(colnames(item119.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"
                                                                                           ,"Type"))]))

item119.os.data$count <- 1
item119.os.data$Count <- 1
#######################
# Weighted Analysis
#######################
item119.os.final <- mean_one_group(item119.os.data
                                ,valueVariable = 'Ind'
                                ,byVariable = 'CK_Building_ID'
                                ,aggregateRow = 'Remove')
item119.os.final <- item119.os.final[which(item119.os.final$CK_Building_ID %notin% c("Remove","Total")),]

item119.os.final.SF <- item119.os.final[which(item119.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item119.os.final) %in% c("BuildingType"))]

exportTable(item119.os.final.SF, "SF", "Table 126", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item119.os.final <- mean_one_group_unweighted(item119.os.data
                                           ,valueVariable = 'Ind'
                                           ,byVariable = 'CK_Building_ID'
                                           ,aggregateRow = 'Remove')
item119.os.final <- item119.os.final[which(item119.os.final$CK_Building_ID %notin% c("Remove","Total")),]

item119.os.final.SF <- item119.os.final[which(item119.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item119.os.final) %in% c("BuildingType"))]

exportTable(item119.os.final.SF, "SF", "Table 126", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
#Item 120: AVERAGE NUMBER OF SUBWOOFERS PER HOME BY TYPE (SF table 127, MH table 102)
#############################################################################################
#subset to columns needed for analysis
item120.os.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,"Sub-Type"
                                                                    ,"Contains.Suboofer"
                                                                    ,"Does.Subwoofer.have.indicator.light.or.warm.to.touch"))]
item120.os.dat$count <- 1

#remove any repeat header rows from exporting
item120.os.dat0 <- item120.os.dat[which(item120.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned scl data
item120.os.dat1 <- left_join(scl.dat, item120.os.dat0, by = "CK_Cadmus_ID")

unique(item120.os.dat1$Type)

item120.os.dat2 <- item120.os.dat1[which(item120.os.dat1$Contains.Suboofer %in% c("Yes")),]
unique(item120.os.dat2$Does.Subwoofer.have.indicator.light.or.warm.to.touch)
item120.os.dat3 <- item120.os.dat2[which(item120.os.dat2$Does.Subwoofer.have.indicator.light.or.warm.to.touch != "Unknown"),]

item120.os.dat3$Subwoofer.Type <- "Passive"
item120.os.dat3$Subwoofer.Type[which(item120.os.dat3$Does.Subwoofer.have.indicator.light.or.warm.to.touch == "Yes")] <- "Powered"


item120.os.merge <- left_join(scl.dat, item120.os.dat3)


item120.os.cast <- dcast(setDT(item120.os.merge)
                      ,formula = CK_Cadmus_ID~Subwoofer.Type,sum
                      ,value.var = "count")
item120.os.cast <- data.frame(item120.os.cast, stringsAsFactors = F)
item120.os.cast <- item120.os.cast[which(colnames(item120.os.cast) %in% c("CK_Cadmus_ID","Passive","Powered"))]

item120.os.melt <- melt(item120.os.cast,id.vars = "CK_Cadmus_ID")
names(item120.os.melt) <- c("CK_Cadmus_ID", "Subwoofer.Type","Subwoofer.Count")

item120.os.merge <- left_join(scl.dat, item120.os.melt)

################################################
# Adding pop and sample sizes for weights
################################################
item120.os.data <- weightedData(item120.os.merge[-which(colnames(item120.os.merge) %in% c("Subwoofer.Type"
                                                                                 ,"Subwoofer.Count"))])
item120.os.data <- left_join(item120.os.data, unique(item120.os.merge[which(colnames(item120.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Subwoofer.Type"
                                                                                           ,"Subwoofer.Count"))]))

item120.os.data$count <- 1
item120.os.data$Count <- 1
#######################
# Weighted Analysis
#######################
item120.os.cast <- mean_two_groups(CustomerLevelData = item120.os.data
                                    ,valueVariable = 'Subwoofer.Count'
                                    ,byVariableColumn = "CK_Building_ID"
                                    ,byVariableRow = 'Subwoofer.Type'
                                    ,rowAggregate = 'All Subwoofers'
                                    ,columnAggregate = "Remove")

item120.os.final <- data.frame("BuildingType"          = item120.os.cast$BuildingType
                              ,"Subwoofer.Type"       = item120.os.cast$Subwoofer.Type
                              ,"Mean_SCL.GenPop"      = item120.os.cast$`Mean_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item120.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item120.os.cast$`n_SCL GenPop`
                              ,"Mean_SCL.LI"          = item120.os.cast$`Mean_SCL LI`
                              ,"SE_SCL.LI"            = item120.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = item120.os.cast$`n_SCL LI`
                              ,"Mean_SCL.EH"          = item120.os.cast$`Mean_SCL EH`
                              ,"SE_SCL.EH"            = item120.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = item120.os.cast$`n_SCL EH`
                              ,"Mean_2017.RBSA.PS"    = item120.os.cast$`Mean_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item120.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item120.os.cast$`n_2017 RBSA PS`
                              ,"EB_SCL.GenPop"        = item120.os.cast$`EB_SCL GenPop`
                              ,"EB_SCL.LI"            = item120.os.cast$`EB_SCL LI`
                              ,"EB_SCL.EH"            = item120.os.cast$`EB_SCL EH`
                              ,"EB_2017.RBSA.PS"      = item120.os.cast$`EB_2017 RBSA PS`)

item120.os.final.SF <- item120.os.final[which(item120.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item120.os.final) %in% c("BuildingType"))]

exportTable(item120.os.final.SF, "SF", "Table 127", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# Unweighted Analysis
#######################
item120.os.cast <- mean_two_groups_unweighted(CustomerLevelData = item120.os.data
                                   ,valueVariable = 'Subwoofer.Count'
                                   ,byVariableColumn = "CK_Building_ID"
                                   ,byVariableRow = 'Subwoofer.Type'
                                   ,rowAggregate = 'All Subwoofers'
                                   ,columnAggregate = "Remove")

item120.os.final <- data.frame("BuildingType"          = item120.os.cast$BuildingType
                               ,"Subwoofer.Type"       = item120.os.cast$Subwoofer.Type
                               ,"Mean_SCL.GenPop"      = item120.os.cast$`Mean_SCL GenPop`
                               ,"SE_SCL.GenPop"        = item120.os.cast$`SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = item120.os.cast$`n_SCL GenPop`
                               ,"Mean_SCL.LI"          = item120.os.cast$`Mean_SCL LI`
                               ,"SE_SCL.LI"            = item120.os.cast$`SE_SCL LI`
                               ,"n_SCL.LI"             = item120.os.cast$`n_SCL LI`
                               ,"Mean_SCL.EH"          = item120.os.cast$`Mean_SCL EH`
                               ,"SE_SCL.EH"            = item120.os.cast$`SE_SCL EH`
                               ,"n_SCL.EH"             = item120.os.cast$`n_SCL EH`
                               ,"Mean_2017.RBSA.PS"    = item120.os.cast$`Mean_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item120.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item120.os.cast$`n_2017 RBSA PS`)
item120.os.final.SF <- item120.os.final[which(item120.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item120.os.final) %in% c("BuildingType"))]

exportTable(item120.os.final.SF, "SF", "Table 127", weighted = FALSE, osIndicator = "SCL", OS = T)
