#############################################################################################
##  Title:            RBSA Analysis                      
##  Authors:          Casey Stevens, Andres Roma, Diego Sosa-Coba, Cadmus Group               
##  Created:          10/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)

"%notin%" <- Negate("%in%")

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")

# rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
# rbsa.dat <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),] 

#Read in data for analysis
# room.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
room.dat$CK_Cadmus_ID <- trimws(toupper(room.dat$CK_Cadmus_ID))


# ##########################################################################
# # Item 7: AVERAGE NUMBER OF BEDROOMS PER HOME BY STATE (SF Table 14)
# ##########################################################################
# #subset to columns need in table
# room.tmp <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID"
#                                                      , "Iteration"
#                                                      , "Clean.Type"
#                                                      , "Area"))]
# item7.dat <- left_join(rbsa.dat, room.tmp, by = "CK_Cadmus_ID")
# 
# #remove building information
# item7.dat1 <- item7.dat[grep("SITE", item7.dat$Iteration),]
# 
# #subset to only bedrooms
# item7.dat2 <- item7.dat1[which(item7.dat1$Clean.Type == "Bedroom"),]
# item7.dat2$count <- 1
# 
# # Summarise up to the customer level
# item7.customer <- summarise(group_by(item7.dat2
#                                      , CK_Cadmus_ID
#                                      , BuildingType
#                                      , State)
#                             ,CountRooms = sum(count))
# 
# item7.merge <- left_join(rbsa.dat, item7.customer)
# item7.merge$CountRooms[which(is.na(item7.merge$CountRooms))] <- 0
# 
# # apply weights to the subset of the data
# item7.data <- weightedData(item7.merge[-which(colnames(item7.merge) == "CountRooms")])
# #merge back on measured variable
# item7.data <- left_join(item7.data, item7.customer[which(colnames(item7.customer) %in% c("CK_Cadmus_ID"
#                                                                                          , "CountRooms"))])
# item7.data$count <- 1
# 
# 
# ################################
# # Weighted Analysis
# ################################
# item7.final <- mean_one_group(CustomerLevelData = item7.data
#                               , valueVariable = 'CountRooms'
#                               , byVariable    = 'State'
#                               , aggregateRow  = 'Region')
# #subset by home type
# item7.final.SF <- item7.final[which(item7.final$BuildingType == "Single Family"),-1]
# item7.final.MH <- item7.final[which(item7.final$BuildingType == "Manufactured"),-1]
# 
# #export data
# exportTable(item7.final.SF, "SF", "Table 14", weighted = TRUE)
# # exportTable(item7.final.MH, "MH", "Table 12", weighted = TRUE)
# 
# 
# 
# 
# ################################
# # Unweighted Analysis
# ################################
# item7.final <- mean_one_group_unweighted(CustomerLevelData = item7.data
#                               , valueVariable = 'CountRooms'
#                               , byVariable    = 'State'
#                               , aggregateRow  = 'Region')
# #subset by home type
# item7.final.SF <- item7.final[which(item7.final$BuildingType == "Single Family"),-1]
# item7.final.MH <- item7.final[which(item7.final$BuildingType == "Manufactured"),-1]
# #export data
# exportTable(item7.final.SF, "SF", "Table 14", weighted = FALSE)
# # exportTable(item7.final.MH, "MH", "Table 12", weighted = FALSE)
# 
# 
# 
# 
# 
# #####################################################################################
# # Item 8: AVERAGE NUMBER OF BATHROOMS PER HOME BY STATE (SF Table 15, MH Table 13)
# #####################################################################################
# #subset to columns need in table
# room.tmp  <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID"
#                                                       , "Iteration"
#                                                       , "Clean.Type"
#                                                       , "Area"))]
# item8.dat <- left_join(rbsa.dat, room.tmp, by = "CK_Cadmus_ID")
# 
# #remove building information
# item8.dat1 <- item8.dat[grep("SITE", item8.dat$Iteration),]
# 
# #subset to only bedrooms
# item8.dat2 <- item8.dat1[which(item8.dat1$Clean.Type == "Bathroom"),]
# item8.dat2$count <- 1
# 
# 
# # Summarise up to the customer level
# item8.customer <- summarise(group_by(item8.dat2
#                                      , CK_Cadmus_ID
#                                      , BuildingType
#                                      , State)
#                             ,CountRooms = sum(count))
# 
# item8.merge <- left_join(rbsa.dat, item8.customer)
# item8.merge$CountRooms[which(is.na(item8.merge$CountRooms))] <- 0
# 
# # apply weights to the subset of the data
# item8.data <- weightedData(item8.merge[-which(colnames(item8.merge) == "CountRooms")])
# #merge back on measured variable
# item8.data <- left_join(item8.data, item8.customer[which(colnames(item8.customer) %in% c("CK_Cadmus_ID"
#                                                                                          , "CountRooms"))])
# item8.data$count <- 1
# 
# 
# ################################
# # Weighted Analysis
# ################################
# item8.final <- mean_one_group(CustomerLevelData = item8.data
#                               , valueVariable = 'CountRooms'
#                               , byVariable    = 'State'
#                               , aggregateRow  = "Region")
# #subset by home type
# item8.final.SF <- item8.final[which(item8.final$BuildingType == "Single Family"),-1]
# item8.final.MH <- item8.final[which(item8.final$BuildingType == "Manufactured"),-1]
# #export data
# exportTable(item8.final.SF, "SF", "Table 15", weighted = TRUE)
# # exportTable(item8.final.MH, "MH", "Table 13", weighted = TRUE)
# 
# 
# 
# 
# 
# ################################
# # Unweighted Analysis
# ################################
# item8.final <- mean_one_group_unweighted(CustomerLevelData = item8.data
#                               , valueVariable = 'CountRooms'
#                               , byVariable    = 'State'
#                               , aggregateRow  = "Region")
# #subset by home type
# item8.final.SF <- item8.final[which(item8.final$BuildingType == "Single Family"),-1]
# item8.final.MH <- item8.final[which(item8.final$BuildingType == "Manufactured"),-1]
# #export data
# exportTable(item8.final.SF, "SF", "Table 15", weighted = FALSE)
# # exportTable(item8.final.MH, "MH", "Table 13", weighted = FALSE)
# 
# 
# 
# ############################################################################################
# # Item 9: AVERAGE ROOM AREAS BY ROOM TYPE (SF Table 16, MH Table 14)
# ############################################################################################
# 
# #subset to columns need in table
# room.tmp  <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID"
#                                                       , "Iteration"
#                                                       , "Clean.Type"
#                                                       , "Area"))]
# item9.dat <- left_join(rbsa.dat, room.tmp, by = "CK_Cadmus_ID")
# 
# item9.dat$count <- 1
# item9.dat$Area <- as.numeric(as.character(item9.dat$Area))
# #remove missing area information
# item9.dat1 <- item9.dat[which(!is.na(item9.dat$Area)),]
# #clean room types
# unique(item9.dat1$Clean.Type)
# item9.dat1$Clean.Type[which(item9.dat1$Clean.Type %in% c("Attic"
#                                                          ,"Basement"
#                                                          ,"Crawlspace"
#                                                          ,"Crawl Space"
#                                                          ,"Mechanical"
#                                                          ,"Grow Room"))] <- "Other"
# 
# unique(item9.dat1$Clean.Type)
# 
# 
# #average within houses
# item9.customer <- summarise(group_by(item9.dat1
#                                      , CK_Cadmus_ID
#                                      , Clean.Type)
#                             ,y_bar_ilk  = mean(Area)
#                             ,y_ilk      = sum(Area)
#                             ,m_ilk      = sum(count)
# )
# 
# item9.merge <- left_join(rbsa.dat, item9.customer)
# item9.merge <- item9.merge[which(!is.na(item9.merge$y_ilk)),]
# 
# # apply weights to the subset of the data
# item9.data <- weightedData(item9.merge[-which(colnames(item9.merge) %in% c("y_bar_ilk"
#                                                                            ,"Clean.Type"
#                                                                            ,"Area"
#                                                                            ,"count"
#                                                                            ,"Iteration"
#                                                                            ,"m_ilk"
#                                                                            ,"y_ilk"
#                                                                            ,"Site_Area"
#                                                                            ,"Site_Count"
#                                                                            ,"Site_Sum"))])
# #merge back on measured variable
# item9.data <- left_join(item9.data, item9.merge[which(colnames(item9.merge) %in% c("CK_Cadmus_ID"
#                                                                                    ,"y_bar_ilk"
#                                                                                    ,"Clean.Type"
#                                                                                    ,"Area"
#                                                                                    ,"count"
#                                                                                    ,"Iteration"
#                                                                                    ,"m_ilk"
#                                                                                    ,"y_ilk"
#                                                                                    ,"Site_Area"
#                                                                                    ,"Site_Count"
#                                                                                    ,"Site_Sum"))])
# item9.data$count <- 1
# 
# 
# 
# ################################
# # Weighted Analysis
# ################################
# item9.final <- mean_one_group_domain(CustomerLevelData = item9.data
#                               , valueVariable = 'y_ilk'
#                               , byVariable    = 'Clean.Type'
#                               , aggregateRow  = "All Room Types")
# 
# 
# #subset by home type
# item9.final.SF <- item9.final[which(item9.final$BuildingType == "Single Family"),-1]
# item9.final.MH <- item9.final[which(item9.final$BuildingType == "Manufactured"),-1]
# 
# 
# #export data
# exportTable(item9.final.SF, "SF", "Table 16", weighted = TRUE)
# # exportTable(item9.final.MH, "MH", "Table 14", weighted = TRUE)
# 
# 
# ################################
# # Unweighted Analysis
# ################################
# item9.final <- mean_one_group_unweighted(CustomerLevelData = item9.data
#                               , valueVariable = 'y_bar_ilk'
#                               , byVariable    = 'Clean.Type'
#                               , aggregateRow  = "All Room Types")
# 
# 
# #subset by home type
# item9.final.SF <- item9.final[which(item9.final$BuildingType == "Single Family"),-1]
# item9.final.MH <- item9.final[which(item9.final$BuildingType == "Manufactured"),-1]
# 
# 
# #export data
# exportTable(item9.final.SF, "SF", "Table 16", weighted = FALSE)
# # exportTable(item9.final.MH, "MH", "Table 14", weighted = FALSE)































##################################################################################################
#
#
# OVERSAMPLE ANALYSES
#
#
##################################################################################################
# Read in clean os data
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]

##########################################################################
# Item 7: AVERAGE NUMBER OF BEDROOMS PER HOME BY STATE (SF Table 14)
##########################################################################
#subset to columns need in table
room.tmp <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID"
                                                     , "Iteration"
                                                     , "Clean.Type"
                                                     , "Area"))]
item7.os.dat <- left_join(os.dat, room.tmp, by = "CK_Cadmus_ID")

#remove building information
item7.os.dat1 <- item7.os.dat[grep("SITE", item7.os.dat$Iteration),]

#subset to only bedrooms
item7.os.dat2 <- item7.os.dat1[which(item7.os.dat1$Clean.Type == "Bedroom"),]
item7.os.dat2$count <- 1

# Summarise up to the customer level
item7.os.customer <- summarise(group_by(item7.os.dat2
                                     , CK_Cadmus_ID, CK_Building_ID)
                            ,CountRooms = sum(count))

item7.os.merge <- left_join(os.dat, item7.os.customer)
item7.os.merge$CountRooms[which(is.na(item7.os.merge$CountRooms))] <- 0

# apply weights to the subset of the data
item7.os.data <- weightedData(item7.os.merge[-which(colnames(item7.os.merge) == "CountRooms")])
#merge back on measured variable
item7.os.data <- left_join(item7.os.data, item7.os.customer[which(colnames(item7.os.customer) %in% c("CK_Cadmus_ID"
                                                                                         , "CountRooms"))])
item7.os.data$count <- 1

################################
# Weighted Analysis
################################
item7.os.final <- mean_one_group(CustomerLevelData = item7.os.data
                              , valueVariable = 'CountRooms'
                              , byVariable    = 'CK_Building_ID'
                              , aggregateRow  = 'Remove')
item7.os.final <- item7.os.final[which(item7.os.final$CK_Building_ID %notin% c("Remove", "Total")),]

# row ordering example code
levels(item7.os.final$CK_Building_ID)
if(os.ind == "scl"){
  rowOrder <- c("SCL GenPop"
                ,"SCL LI"
                ,"SCL EH"
                ,"2017 RBSA PS")
}else if(os.ind == "snopud"){
  rowOrder <- c("SnoPUD"
                ,"2017 RBSA PS"
                ,"2017 RBSA NW")
}
item7.os.final <- item7.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)
item7.os.final <- data.frame(item7.os.final)

#subset by home type
item7.os.final.SF <- item7.os.final[which(item7.os.final$BuildingType == "Single Family"),-1]

#export data
exportTable(item7.os.final.SF, "SF", "Table 14", weighted = TRUE, osIndicator = export.ind, OS = T)

################################
# Unweighted Analysis
################################
item7.os.final <- mean_one_group_unweighted(CustomerLevelData = item7.os.data
                                         , valueVariable = 'CountRooms'
                                         , byVariable    = 'CK_Building_ID'
                                         , aggregateRow  = 'Remove')
item7.os.final <- item7.os.final[which(item7.os.final$CK_Building_ID %notin% c("Remove", "Total")),]

# row ordering example code
levels(item7.os.final$CK_Building_ID)
if(os.ind == "scl"){
  rowOrder <- c("SCL GenPop"
                ,"SCL LI"
                ,"SCL EH"
                ,"2017 RBSA PS")
}else if(os.ind == "snopud"){
  rowOrder <- c("SnoPUD"
                ,"2017 RBSA PS"
                ,"2017 RBSA NW")
}
item7.os.final <- item7.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)
item7.os.final <- data.frame(item7.os.final)

#subset by home type
item7.os.final.SF <- item7.os.final[which(item7.os.final$BuildingType == "Single Family"),-1]

#export data
exportTable(item7.os.final.SF, "SF", "Table 14", weighted = FALSE, osIndicator = export.ind, OS = T)




#####################################################################################
# Item 8: AVERAGE NUMBER OF BATHROOMS PER HOME BY CK_Building_ID (SF Table 15, MH Table 13)
#####################################################################################
#subset to columns need in table
room.tmp  <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID"
                                                      , "Iteration"
                                                      , "Clean.Type"
                                                      , "Area"))]
item8.os.dat <- left_join(os.dat, room.tmp, by = "CK_Cadmus_ID")

#remove building information
item8.os.dat1 <- item8.os.dat[grep("SITE", item8.os.dat$Iteration),]

#subset to only bedrooms
item8.os.dat2 <- item8.os.dat1[which(item8.os.dat1$Clean.Type == "Bathroom"),]
item8.os.dat2$count <- 1


# Summarise up to the customer level
item8.os.customer <- summarise(group_by(item8.os.dat2
                                     , CK_Cadmus_ID, CK_Building_ID
                                     , BuildingType
                                     , CK_Building_ID)
                            ,CountRooms = sum(count))

item8.os.merge <- left_join(os.dat, item8.os.customer)
item8.os.merge$CountRooms[which(is.na(item8.os.merge$CountRooms))] <- 0

# apply weights to the subset of the data
item8.os.data <- weightedData(item8.os.merge[-which(colnames(item8.os.merge) == "CountRooms")])
#merge back on measured variable
item8.os.data <- left_join(item8.os.data, item8.os.customer[which(colnames(item8.os.customer) %in% c("CK_Cadmus_ID"
                                                                                         , "CountRooms"))])
item8.os.data$count <- 1


################################
# Weighted Analysis
################################
item8.os.final <- mean_one_group(CustomerLevelData = item8.os.data
                              , valueVariable = 'CountRooms'
                              , byVariable    = 'CK_Building_ID'
                              , aggregateRow  = "Remove")
item8.os.final <- item8.os.final[which(item8.os.final$CK_Building_ID %notin% c("Remove", "Total")),]

# row ordering example code
levels(item8.os.final$CK_Building_ID)
if(os.ind == "scl"){
  rowOrder <- c("SCL GenPop"
                ,"SCL LI"
                ,"SCL EH"
                ,"2017 RBSA PS")
}else if(os.ind == "snopud"){
  rowOrder <- c("SnoPUD"
                ,"2017 RBSA PS"
                ,"2017 RBSA NW")
}
item8.os.final <- item8.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)
item8.os.final <- data.frame(item8.os.final)

#subset by home type
item8.os.final.SF <- item8.os.final[which(item8.os.final$BuildingType == "Single Family"),-1]

#export data
exportTable(item8.os.final.SF, "SF", "Table 15", weighted = TRUE, osIndicator = export.ind, OS = T)

################################
# Unweighted Analysis
################################
item8.os.final <- mean_one_group_unweighted(CustomerLevelData = item8.os.data
                                         , valueVariable = 'CountRooms'
                                         , byVariable    = 'CK_Building_ID'
                                         , aggregateRow  = "Remove")
item8.os.final <- item8.os.final[which(item8.os.final$CK_Building_ID %notin% c("Remove", "Total")),]

# row ordering example code
levels(item8.os.final$CK_Building_ID)
if(os.ind == "scl"){
  rowOrder <- c("SCL GenPop"
                ,"SCL LI"
                ,"SCL EH"
                ,"2017 RBSA PS")
}else if(os.ind == "snopud"){
  rowOrder <- c("SnoPUD"
                ,"2017 RBSA PS"
                ,"2017 RBSA NW")
}
item8.os.final <- item8.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)
item8.os.final <- data.frame(item8.os.final)

#subset by home type
item8.os.final.SF <- item8.os.final[which(item8.os.final$BuildingType == "Single Family"),-1]

#export data
exportTable(item8.os.final.SF, "SF", "Table 15", weighted = FALSE, osIndicator = export.ind, OS = T)



############################################################################################
# Item 9: AVERAGE ROOM AREAS BY ROOM TYPE (SF Table 16, MH Table 14)
############################################################################################
#subset to columns need in table
room.tmp  <- room.dat[which(colnames(room.dat) %in% c("CK_Cadmus_ID"
                                                      , "Iteration"
                                                      , "Clean.Type"
                                                      , "Area"))]
item9.os.dat <- left_join(os.dat, room.tmp, by = "CK_Cadmus_ID")

item9.os.dat$count <- 1
item9.os.dat$Area <- as.numeric(as.character(item9.os.dat$Area))
#remove missing area information
item9.os.dat1 <- item9.os.dat[which(!is.na(item9.os.dat$Area)),]
#clean room types
unique(item9.os.dat1$Clean.Type)
item9.os.dat1$Clean.Type[which(item9.os.dat1$Clean.Type %in% c("Attic"
                                                         ,"Basement"
                                                         ,"Crawlspace"
                                                         ,"Crawl Space"
                                                         ,"Mechanical"
                                                         ,"Grow Room"))] <- "Other"

unique(item9.os.dat1$Clean.Type)


#average within houses
item9.os.customer <- summarise(group_by(item9.os.dat1
                                     , CK_Cadmus_ID, CK_Building_ID
                                     , Clean.Type)
                            ,y_bar_ilk  = mean(Area)
                            ,y_ilk      = sum(Area)
                            ,m_ilk      = sum(count)
)

item9.os.merge <- left_join(os.dat, item9.os.customer)
item9.os.merge <- item9.os.merge[which(!is.na(item9.os.merge$y_ilk)),]

# apply weights to the subset of the data
item9.os.data <- weightedData(item9.os.merge[-which(colnames(item9.os.merge) %in% c("y_bar_ilk"
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
item9.os.data <- left_join(item9.os.data, item9.os.merge[which(colnames(item9.os.merge) %in% c("CK_Cadmus_ID"
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
item9.os.data$count <- 1



################################
# Weighted Analysis
################################
item9.os.cast <- mean_two_groups(CustomerLevelData = item9.os.data
                                  , valueVariable = 'y_bar_ilk'
                                  , byVariableRow = 'Clean.Type'
                                  , byVariableColumn    = 'CK_Building_ID'
                                  , rowAggregate  = "All Room Types"
                                  , columnAggregate = "Remove")

if(os.ind == "scl"){
  item9.os.table <- data.frame("BuildingType"       = item9.os.cast$BuildingType
                               ,"Room.Type"            = item9.os.cast$Clean.Type
                               ,"Mean_SCL.GenPop"      = item9.os.cast$`Mean_SCL GenPop`
                               ,"SE_SCL.GenPop"        = item9.os.cast$`SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = item9.os.cast$`n_SCL GenPop`
                               ,"Mean_SCL.LI"          = item9.os.cast$`Mean_SCL LI`
                               ,"SE_SCL.LI"            = item9.os.cast$`SE_SCL LI`
                               ,"n_SCL.LI"             = item9.os.cast$`n_SCL LI`
                               ,"Mean_SCL.EH"          = item9.os.cast$`Mean_SCL EH`
                               ,"SE_SCL.EH"            = item9.os.cast$`SE_SCL EH`
                               ,"n_SCL.EH"             = item9.os.cast$`n_SCL EH`
                               ,"Mean_2017.RBSA.PS"    = item9.os.cast$`Mean_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item9.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item9.os.cast$`n_2017 RBSA PS`
                               ,"EB_SCL.GenPop"        = item9.os.cast$`EB_SCL GenPop`
                               ,"EB_SCL.LI"            = item9.os.cast$`EB_SCL LI`
                               ,"EB_SCL.EH"            = item9.os.cast$`EB_SCL EH`
                               ,"EB_2017.RBSA.PS"      = item9.os.cast$`EB_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item9.os.table <- data.frame("BuildingType"       = item9.os.cast$BuildingType
                               ,"Room.Type"            = item9.os.cast$Clean.Type
                               ,"Mean_SnoPUD"          = item9.os.cast$`Mean_SnoPUD`
                               ,"SE_SnoPUD"            = item9.os.cast$`SE_SnoPUD`
                               ,"n_SnoPUD"             = item9.os.cast$`n_SnoPUD`
                               ,"Mean_2017.RBSA.PS"    = item9.os.cast$`Mean_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item9.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item9.os.cast$`n_2017 RBSA PS`
                               ,"Mean_RBSA.NW"         = item9.os.cast$`Mean_2017 RBSA NW`
                               ,"SE_RBSA.NW"           = item9.os.cast$`SE_2017 RBSA NW`
                               ,"n_RBSA.NW"            = item9.os.cast$`n_2017 RBSA NW`
                               ,"EB_SnoPUD"            = item9.os.cast$`EB_SnoPUD`
                               ,"EB_2017.RBSA.PS"      = item9.os.cast$`EB_2017 RBSA PS`
                               ,"EB_RBSA.NW"           = item9.os.cast$`EB_2017 RBSA NW`)
}

# row ordering example code
levels(item9.os.table$Room.Type)
rowOrder <- c("Bathroom"
              ,"Bedroom"
              ,"Closet"
              ,"Dining Room"
              ,"Family Room"
              ,"Garage"
              ,"Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Living Room"
              ,"Office"
              ,"Other"
              ,"Outside"
              ,"All Room Types")
item9.os.table <- item9.os.table %>% mutate(Room.Type = factor(Room.Type, levels = rowOrder)) %>% arrange(Room.Type)
item9.os.table <- data.frame(item9.os.table)


#subset by home type
item9.os.final.SF <- item9.os.table[which(item9.os.table$BuildingType == "Single Family"),-1]

#export data
exportTable(item9.os.final.SF, "SF", "Table 16", weighted = TRUE, osIndicator = export.ind, OS = T)

################################
# Unweighted Analysis
################################
item9.os.cast <- mean_two_groups_unweighted(CustomerLevelData = item9.os.data
                                 , valueVariable = 'y_bar_ilk'
                                 , byVariableRow = 'Clean.Type'
                                 , byVariableColumn    = 'CK_Building_ID'
                                 , rowAggregate  = "All Room Types"
                                 , columnAggregate = "Remove")

if(os.ind == "scl"){
  item9.os.table <- data.frame("BuildingType"       = item9.os.cast$BuildingType
                               ,"Room.Type"            = item9.os.cast$Clean.Type
                               ,"Mean_SCL.GenPop"      = item9.os.cast$`Mean_SCL GenPop`
                               ,"SE_SCL.GenPop"        = item9.os.cast$`SE_SCL GenPop`
                               ,"n_SCL.GenPop"         = item9.os.cast$`n_SCL GenPop`
                               ,"Mean_SCL.LI"          = item9.os.cast$`Mean_SCL LI`
                               ,"SE_SCL.LI"            = item9.os.cast$`SE_SCL LI`
                               ,"n_SCL.LI"             = item9.os.cast$`n_SCL LI`
                               ,"Mean_SCL.EH"          = item9.os.cast$`Mean_SCL EH`
                               ,"SE_SCL.EH"            = item9.os.cast$`SE_SCL EH`
                               ,"n_SCL.EH"             = item9.os.cast$`n_SCL EH`
                               ,"Mean_2017.RBSA.PS"    = item9.os.cast$`Mean_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item9.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item9.os.cast$`n_2017 RBSA PS`)
}else if(os.ind == "snopud"){
  item9.os.table <- data.frame("BuildingType"       = item9.os.cast$BuildingType
                               ,"Room.Type"            = item9.os.cast$Clean.Type
                               ,"Mean_SnoPUD"          = item9.os.cast$`Mean_SnoPUD`
                               ,"SE_SnoPUD"            = item9.os.cast$`SE_SnoPUD`
                               ,"n_SnoPUD"             = item9.os.cast$`n_SnoPUD`
                               ,"Mean_2017.RBSA.PS"    = item9.os.cast$`Mean_2017 RBSA PS`
                               ,"SE_2017.RBSA.PS"      = item9.os.cast$`SE_2017 RBSA PS`
                               ,"n_2017.RBSA.PS"       = item9.os.cast$`n_2017 RBSA PS`
                               ,"Mean_RBSA.NW"         = item9.os.cast$`Mean_2017 RBSA NW`
                               ,"SE_RBSA.NW"           = item9.os.cast$`SE_2017 RBSA NW`
                               ,"n_RBSA.NW"            = item9.os.cast$`n_2017 RBSA NW`)
}

# row ordering example code
levels(item9.os.table$Room.Type)
rowOrder <- c("Bathroom"
              ,"Bedroom"
              ,"Closet"
              ,"Dining Room"
              ,"Family Room"
              ,"Garage"
              ,"Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Living Room"
              ,"Office"
              ,"Other"
              ,"Outside"
              ,"All Room Types")
item9.os.table <- item9.os.table %>% mutate(Room.Type = factor(Room.Type, levels = rowOrder)) %>% arrange(Room.Type)
item9.os.table <- data.frame(item9.os.table)

#subset by home type
item9.os.final.SF <- item9.os.table[which(item9.os.table$BuildingType == "Single Family"),-1]

#export data
exportTable(item9.os.final.SF, "SF", "Table 16", weighted = FALSE, osIndicator = export.ind, OS = T)

