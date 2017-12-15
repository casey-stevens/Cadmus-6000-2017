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
#Item 112: AVERAGE NUMBER OF SET-TOP BOXES PER HOME BY STATE (SF table 119, MH table 94)
#############################################################################################
#subset to columns needed for analysis
item112.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"TV.Set.Top.Box"
                                                                    ,"STB.Records?"))]
item112.dat$count <- 1

#remove any repeat header rows from exporting
item112.dat0 <- item112.dat[which(item112.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item112.dat1 <- left_join(item112.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#subset to set.top.box not NA
item112.dat2 <- item112.dat1[which(item112.dat1$TV.Set.Top.Box == "Yes"),]

#summarise by site
item112.customer <- summarise(group_by(item112.dat2, CK_Cadmus_ID)
                          ,Site.Count = sum(count))

item112.merge <- left_join(rbsa.dat, item112.customer)
item112.merge$Site.Count[which(is.na(item112.merge$Site.Count))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
item112.data <- weightedData(item112.merge[-which(colnames(item112.merge) %in% c("Site.Count"))])
item112.data <- left_join(item112.data, item112.merge[which(colnames(item112.merge) %in% c("CK_Cadmus_ID"
                                                                                                 ,"Site.Count"))])
item112.data$count <- 1
#######################
# Weighted Analysis
#######################
item112.final <- mean_one_group(item112.data
                                ,valueVariable = 'Site.Count'
                                ,byVariable = 'State'
                                ,aggregateRow = 'Region')

item112.final.SF <- item112.final[which(item112.final$BuildingType == "Single Family")
                                  ,-which(colnames(item112.final) %in% c("BuildingType"
                                                                         ,"Count"))]
item112.final.MH <- item112.final[which(item112.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item112.final) %in% c("BuildingType"
                                                                         ,"Count"))]

exportTable(item112.final.SF, "SF", "Table 119", weighted = TRUE)
exportTable(item112.final.MH, "MH", "Table 94", weighted = TRUE)



#######################
# Unweighted Analysis
#######################
item112.final <- mean_one_group_unweighted(item112.data
                                           ,valueVariable = 'Site.Count'
                                           ,byVariable = 'State'
                                           ,aggregateRow = 'Region')

item112.final.SF <- item112.final[which(item112.final$BuildingType == "Single Family")
                                  ,-which(colnames(item112.final) %in% c("BuildingType"
                                                                         ,"Count"))]
item112.final.MH <- item112.final[which(item112.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item112.final) %in% c("BuildingType"
                                                                         ,"Count"))]

exportTable(item112.final.SF, "SF", "Table 119", weighted = FALSE)
exportTable(item112.final.MH, "MH", "Table 94", weighted = FALSE)





#############################################################################################
#Item 113: PERCENTAGE OF HOMES WITH SET-TOP BOXES (SF table 120, MH table 95)
#############################################################################################
#subset to columns needed for analysis
item113.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"TV.Set.Top.Box"
                                                                    ,"STB.Records?"))]
item113.dat$count <- 1

#remove any repeat header rows from exporting
item113.dat0 <- item113.dat[which(item113.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item113.dat1 <- left_join(item113.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#subset to set.top.box not NA
item113.dat2 <- item113.dat1[which(item113.dat1$TV.Set.Top.Box == "Yes"),]
item113.dat2$Ind <- 1

item113.merge <- unique(left_join(rbsa.dat, item113.dat2))
item113.merge$Ind[which(is.na(item113.merge$Ind))] <- 0
unique(item113.merge$Ind)

################################################
# Adding pop and sample sizes for weights
################################################
item113.data <- weightedData(item113.merge[-which(colnames(item113.merge) %in% c("TV.Set.Top.Box"
                                                                                 ,"STB.Records?"
                                                                                 ,"count"
                                                                                 ,"Ind"))])
item113.data <- left_join(item113.data, item113.merge[which(colnames(item113.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"TV.Set.Top.Box"
                                                                                           ,"STB.Records?"
                                                                                           ,"count"
                                                                                           ,"Ind"))])
item113.data$count <- 1
#######################
# Weighted Analysis
#######################
item113.final <- proportions_one_group(CustomerLevelData = item113.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,columnName = "Remove"
                                       ,weighted = TRUE)

item113.final.SF <- item113.final[which(item113.final$BuildingType == "Single Family")
                                  ,-which(colnames(item113.final) %in% c("BuildingType"
                                                                         ,"Remove"
                                                                         ,"Total.Count"))]
item113.final.MH <- item113.final[which(item113.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item113.final) %in% c("BuildingType"
                                                                         ,"Remove"
                                                                         ,"Total.Count"))]
exportTable(item113.final.SF, "SF", "Table 120", weighted = TRUE)
exportTable(item113.final.MH, "MH", "Table 95", weighted = TRUE)


#######################
# Weighted Analysis
#######################
item113.final <- proportions_one_group(CustomerLevelData = item113.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,columnName = "Remove"
                                       ,weighted = FALSE)

item113.final.SF <- item113.final[which(item113.final$BuildingType == "Single Family")
                                  ,-which(colnames(item113.final) %in% c("BuildingType"
                                                                         ,"Remove"
                                                                         ,"Total.Count"))]
item113.final.MH <- item113.final[which(item113.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item113.final) %in% c("BuildingType"
                                                                         ,"Remove"
                                                                         ,"Total.Count"))]
exportTable(item113.final.SF, "SF", "Table 120", weighted = FALSE)
exportTable(item113.final.MH, "MH", "Table 95", weighted = FALSE)







#############################################################################################
#Item 114: PERCENTAGE OF SET-TOP BOXES WITH DVR CAPABILITY BY STATE (SF table 121, MH table 96)
#############################################################################################
#subset to columns needed for analysis
item114.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"TV.Set.Top.Box"
                                                                    ,"STB.Records?"))]
item114.dat$count <- 1

#remove any repeat header rows from exporting
item114.dat0 <- item114.dat[which(item114.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item114.dat1 <- left_join(item114.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#subset to set.top.box not NA
unique(item114.dat1$TV.Set.Top.Box)
item114.dat2 <- item114.dat1[which(item114.dat1$TV.Set.Top.Box == "Yes"),]

#remove unknown or DP not asked for
item114.dat3 <- item114.dat2[which(item114.dat2$`STB.Records?` %in% c("Yes", "No")),]


item114.merge <- left_join(rbsa.dat, item114.dat3)
item114.merge <- item114.merge[which(!is.na(item114.merge$`STB.Records?`)),]

item114.merge$Ind <- item114.merge$`STB.Records?`
item114.merge$Ind[which(item114.merge$`STB.Records?` == "Yes")]  <- 1
item114.merge$Ind[which(item114.merge$`STB.Records?` == "No")]   <- 0



################################################
# Adding pop and sample sizes for weights
################################################
item114.data <- weightedData(item114.merge[-which(colnames(item114.merge) %in% c("TV.Set.Top.Box"
                                                                                 ,"STB.Records?"
                                                                                 ,"count"
                                                                                 ,"Ind"))])
item114.data <- left_join(item114.data, item114.merge[which(colnames(item114.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"TV.Set.Top.Box"
                                                                                           ,"STB.Records?"
                                                                                           ,"count"
                                                                                           ,"Ind"))])
item114.data$count <- 1
item114.data$Count <- 1
item114.data$Ind <- as.numeric(as.character(item114.data$Ind))
#######################
# Weighted Analysis
#######################
item114.final <- proportions_one_group(CustomerLevelData = item114.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = TRUE)

item114.final.SF <- item114.final[which(item114.final$BuildingType == "Single Family")
                                  ,-which(colnames(item114.final) %in% c("BuildingType"))]
item114.final.MH <- item114.final[which(item114.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item114.final) %in% c("BuildingType"))]
exportTable(item114.final.SF, "SF", "Table 121", weighted = TRUE)
exportTable(item114.final.MH, "MH", "Table 96", weighted = TRUE)


#######################
# unweighted Analysis
#######################
item114.final <- proportions_one_group(CustomerLevelData = item114.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = FALSE)

item114.final.SF <- item114.final[which(item114.final$BuildingType == "Single Family")
                                  ,-which(colnames(item114.final) %in% c("BuildingType"))]
item114.final.MH <- item114.final[which(item114.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item114.final) %in% c("BuildingType"))]
exportTable(item114.final.SF, "SF", "Table 121", weighted = FALSE)
exportTable(item114.final.MH, "MH", "Table 96", weighted = FALSE)

