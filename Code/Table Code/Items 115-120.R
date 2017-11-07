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
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, "Appliances_CS.xlsx")
                            , sheet = "Sheet1")
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
                                                                         ,"Count"))]
item116.final.MH <- item116.final[which(item116.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item116.final) %in% c("BuildingType"
                                                                         ,"Count"))]

exportTable(item116.final.SF, "SF", "Table 123", weighted = FALSE)
exportTable(item116.final.MH, "MH", "Table 98", weighted = FALSE)


# #total gaming systems by site
# item116.dat3 <- summarise(group_by(item116.dat2, CK_Cadmus_ID, BuildingType, State)
#                           ,Site.Count = sum(count))
# 
# ##average by state
# item116.state <- summarise(group_by(item116.dat3, BuildingType, State)
#                            ,SampleSize = length(unique(CK_Cadmus_ID))
#                            ,Mean = mean(Site.Count)
#                            ,SE = sd(Site.Count) / sqrt(SampleSize))
# 
# #average across states
# item116.region <- summarise(group_by(item116.dat3, BuildingType)
#                             ,State = "Region"
#                             ,SampleSize = length(unique(CK_Cadmus_ID))
#                             ,Mean = mean(Site.Count)
#                             ,SE = sd(Site.Count) / sqrt(SampleSize))
# 
# item116.final <- rbind.data.frame(item116.state, item116.region, stringsAsFactors = F)
# 
# item116.table <- data.frame("BuildingType" = item116.final$BuildingType
#                             ,"State" = item116.final$State
#                             ,"Mean" = item116.final$Mean
#                             ,"SE" = item116.final$SE
#                             ,"SampleSize" = item116.final$SampleSize)
# item116.table1 <- item116.table[which(item116.table$BuildingType %in% c("Single Family", "Manufactured")),]












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
                         ,Ind = sum(Ind))

item117.merge <- left_join(rbsa.dat, item117.sum)
item117.merge <- item117.merge[which(is.na(item117.merge$Ind)),]

################################################
# Adding pop and sample sizes for weights
################################################
item117.data <- weightedData(item117.merge[-which(colnames(item117.merge) %in% c("Ind"))])
item117.data <- left_join(item117.data, item117.merge[which(colnames(item117.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])


# #total computers by site
# item117.dat3 <- summarise(group_by(item117.dat2, CK_Cadmus_ID, BuildingType, State)
#                           ,Computer.Count = sum(count))
# 
# #join on with RBSA data (again)
# item117.merge1 <- left_join(rbsa.dat, item117.dat3, by = c("CK_Cadmus_ID","BuildingType", "State"))
# #change computer count NA to zero
# item117.merge1$Computer.Count[which(is.na(item117.merge1$Computer.Count))] <- 0
# 
# ##average by state
# item117.state <- summarise(group_by(item117.merge1, BuildingType, State)
#                            ,SampleSize = length(unique(CK_Cadmus_ID))
#                            ,Mean = mean(Computer.Count)
#                            ,SE = sd(Computer.Count) / sqrt(SampleSize))
# 
# #average across states
# item117.region <- summarise(group_by(item117.merge1, BuildingType)
#                             ,State = "Region"
#                             ,SampleSize = length(unique(CK_Cadmus_ID))
#                             ,Mean = mean(Computer.Count)
#                             ,SE = sd(Computer.Count) / sqrt(SampleSize))
# 
# item117.final <- rbind.data.frame(item117.state, item117.region, stringsAsFactors = F)
# 
# item117.table <- data.frame("BuildingType" = item117.final$BuildingType
#                             ,"State" = item117.final$State
#                             ,"Mean" = item117.final$Mean
#                             ,"SE" = item117.final$SE
#                             ,"SampleSize" = item117.final$SampleSize)
# item117.table1 <- item117.table[which(item117.table$BuildingType %in% c("Single Family", "Manufactured")),]















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
#subset to only Game COnsoles

item118.dat2 <- item118.dat1[which(item118.dat1$Type %in% c("Desktop", "Laptop")),]

item118.dat2$Ind <- 1

item118.sum <- summarise(group_by(item118.dat2, CK_Cadmus_ID)
                         ,Ind = sum(Ind))

item118.sum$Ind <- 1
#QAQC
which(duplicated(item118.sum$CK_Cadmus_ID))


item118.merge <- left_join(rbsa.dat, item118.sum)
item118.merge$Ind[which(is.na(item118.merge$Ind))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
item118.data <- weightedData(item118.merge[-which(colnames(item118.merge) %in% c("Ind"))])
item118.data <- left_join(item118.data, item118.merge[which(colnames(item118.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])

#######################
# Weighted Analysis
#######################
item118.final <- proportions_one_group(CustomerLevelData = item118.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name       = 'Region'
                                       ,columnName       = 'Remove')

item118.final.SF <- item118.final[which(item118.final$BuildingType == "Single Family")
                                  ,-which(colnames(item118.final) %in% c("BuildingType"
                                                                         ,"Remove"))]
item118.final.MH <- item118.final[which(item118.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item118.final) %in% c("BuildingType"
                                                                         ,"Remove"))]

exportTable(item118.final.SF, "SF", "Table 125", weighted = TRUE)
exportTable(item118.final.MH, "MH", "Table 100", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item118.final <- proportions_one_group(CustomerLevelData = item118.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'State'
                                       ,total.name       = 'Region'
                                       ,columnName       = 'Remove'
                                       ,weighted         = FALSE)

item118.final.SF <- item118.final[which(item118.final$BuildingType == "Single Family")
                                  ,-which(colnames(item118.final) %in% c("BuildingType"
                                                                         ,"Remove"
                                                                         ,"Total.Count"))]
item118.final.MH <- item118.final[which(item118.final$BuildingType == "Manufactured")
                                  ,-which(colnames(item118.final) %in% c("BuildingType"
                                                                         ,"Remove"
                                                                         ,"Total.Count"))]

exportTable(item118.final.SF, "SF", "Table 125", weighted = FALSE)
exportTable(item118.final.MH, "MH", "Table 100", weighted = FALSE)





# #summarise by states
# item118.state <- summarise(group_by(item118.merge1, BuildingType, State)
#                            ,SampleSize = length(unique(CK_Cadmus_ID))
#                            ,Comp.Count = sum(Computer.Sites)
#                            ,Site.Count = sum(count)
#                            ,Percent = Comp.Count / Site.Count
#                            ,SE = sqrt(Percent * (1 - Percent) / SampleSize))
# 
# #summarise across states
# item118.region <- summarise(group_by(item118.merge1, BuildingType)
#                             ,State = "Region"
#                             ,SampleSize = length(unique(CK_Cadmus_ID))
#                             ,Comp.Count = sum(Computer.Sites)
#                             ,Site.Count = sum(count)
#                             ,Percent = Comp.Count / Site.Count
#                             ,SE = sqrt(Percent * (1 - Percent) / SampleSize))
# 
# 
# # row bind state and region info
# item118.final <- rbind.data.frame(item118.state, item118.region, stringsAsFactors = F)
# 
# item118.table <- data.frame("BuildingType" = item118.final$BuildingType
#                             ,"State" = item118.final$State
#                             ,"Percent" = item118.final$Percent
#                             ,"SE" = item118.final$SE
#                             ,"SampleSize" = item118.final$SampleSize)
# item118.table1 <- item118.table[which(item118.table$BuildingType %in% c("Single Family", "Manufactured")),]













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
item119.merge <- item119.merge[which(is.na(item119.merge$Ind)),]


# #total computers by site
# item119.dat3 <- summarise(group_by(item119.dat2, CK_Cadmus_ID, BuildingType, State)
#                           ,Audio.Count = sum(count))
# 
# #join on with RBSA data (again)
# item119.merge1 <- left_join(rbsa.dat, item119.dat3, by = c("CK_Cadmus_ID","BuildingType", "State"))
# #change computer count NA to zero
# item119.merge1$Audio.Count[which(is.na(item119.merge1$Audio.Count))] <- 0
# item119.merge1$count <- 1
# 
# 
# ##average by state
# item119.state <- summarise(group_by(item119.merge1, BuildingType, State)
#                            ,SampleSize = length(unique(CK_Cadmus_ID))
#                            ,Mean = mean(Audio.Count)
#                            ,SE = sd(Audio.Count) / sqrt(SampleSize))
# 
# #average across states
# item119.region <- summarise(group_by(item119.merge1, BuildingType)
#                             ,State = "Region"
#                             ,SampleSize = length(unique(CK_Cadmus_ID))
#                             ,Mean = mean(Audio.Count)
#                             ,SE = sd(Audio.Count) / sqrt(SampleSize))
# 
# item119.final <- rbind.data.frame(item119.state, item119.region, stringsAsFactors = F)
# 
# item119.table <- data.frame("BuildingType" = item119.final$BuildingType
#                             ,"State" = item119.final$State
#                             ,"Mean" = item119.final$Mean
#                             ,"SE" = item119.final$SE
#                             ,"SampleSize" = item119.final$SampleSize)
# item119.table1 <- item119.table[which(item119.table$BuildingType %in% c("Single Family", "Manufactured")),]














#############################################################################################
#Item 120: AVERAGE NUMBER OF SUBWOOFERS PER HOME BY STATE (SF table 127, MH table 102)
#############################################################################################
#subset to columns needed for analysis
item120.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Type"
                                                                    ,"Contains.Suboofer"))]
item120.dat$count <- 1

#remove any repeat header rows from exporting
item120.dat0 <- item120.dat[which(item120.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned RBSA data
item120.dat1 <- left_join(item120.dat0, rbsa.dat, by = "CK_Cadmus_ID")

unique(item120.dat1$Type)
#subset to only subwoofer

item120.dat2 <- item120.dat1[which(item120.dat1$Contains.Suboofer %in% c("Yes")),]
item120.dat2$Ind <- 1

item120.sum <- summarise(group_by(item120.dat2, CK_Cadmus_ID)
                         ,Ind = sum(Ind))

item120.merge <- left_join(rbsa.dat, item120.sum)
item120.merge <- item120.merge[which(is.na(item120.merge$Ind)),]




# unique(item120.dat2$Contains.Suboofer)
# 
# #total computers by site
# item120.dat3 <- summarise(group_by(item120.dat2, CK_Cadmus_ID, BuildingType, State)
#                           ,Subwoof.Count = sum(count))
# 
# #join on with RBSA data (again)
# item120.merge1 <- left_join(rbsa.dat, item120.dat3, by = c("CK_Cadmus_ID","BuildingType", "State"))
# #change computer count NA to zero
# item120.merge1$Subwoof.Count[which(is.na(item120.merge1$Subwoof.Count))] <- 0
# item120.merge1$count <- 1
# 
# ##average by state
# item120.state <- summarise(group_by(item120.merge1, BuildingType, State)
#                            ,SampleSize = length(unique(CK_Cadmus_ID))
#                            ,Mean = mean(Subwoof.Count)
#                            ,SE = sd(Subwoof.Count) / sqrt(SampleSize))
# 
# #average across states
# item120.region <- summarise(group_by(item120.merge1, BuildingType)
#                             ,State = "Region"
#                             ,SampleSize = length(unique(CK_Cadmus_ID))
#                             ,Mean = mean(Subwoof.Count)
#                             ,SE = sd(Subwoof.Count) / sqrt(SampleSize))
# 
# item120.final <- rbind.data.frame(item120.state, item120.region, stringsAsFactors = F)
# 
# item120.table <- data.frame("BuildingType" = item120.final$BuildingType
#                             ,"State" = item120.final$State
#                             ,"Mean" = item120.final$Mean
#                             ,"SE" = item120.final$SE
#                             ,"SampleSize" = item120.final$SampleSize)
# item120.table1 <- item120.table[which(item120.table$BuildingType %in% c("Single Family", "Manufactured")),]

