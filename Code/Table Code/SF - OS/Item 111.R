#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list = ls())
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
# sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))



# #############################################################################################
# #Item 111: AVERAGE PRIMARY TELEVISION ON-TIME HOURS PER DAY PER HOME BY STATE (SF table 118, MH table 93)
# #############################################################################################
# #subset to columns needed for analysis
# item111.dat <- sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
#                                                                     ,"INTRVW_CUST_RES_HomeandEnergyUseHomeUse_HowManyHoursPerDayIsThePrimaryTVOn"
#                                                                     ,""))]
# colnames(item111.dat) <- c("CK_Cadmus_ID", "TV_on_hours")
# item111.dat$count <- 1
# 
# #remove any repeat header rows from exporting
# item111.dat0 <- item111.dat[which(item111.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# #merge together analysis data with cleaned RBSA data
# item111.dat1 <- left_join(item111.dat0, rbsa.dat, by = "CK_Cadmus_ID")
# 
# #make tv hours to numeric
# item111.dat1$TV_on_hours <- as.numeric(as.character(item111.dat1$TV_on_hours))
# unique(item111.dat1$TV_on_hours)
# 
# #remove any NA in tv hours
# item111.dat2 <- item111.dat1[which(!(is.na(item111.dat1$TV_on_hours))),]
# 
# #summarise by site
# item111.sum <- summarise(group_by(item111.dat2, CK_Cadmus_ID, BuildingType, State)
#                          ,Site.Mean = mean(TV_on_hours))
# 
# item111.merge <- left_join(rbsa.dat, item111.sum)
# item111.merge <- item111.merge[which(!is.na(item111.merge$Site.Mean)),]
# 
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item111.data <- weightedData(item111.merge[-which(colnames(item111.merge) %in% c("Site.Mean"))])
# item111.data <- left_join(item111.data, item111.merge[which(colnames(item111.merge) %in% c("CK_Cadmus_ID"
#                                                                                                  ,"Site.Mean"))])
# item111.data$count <- 1
# #######################
# # Weighted Analysis
# #######################
# item111.final <- mean_one_group(item111.data
#                                 ,valueVariable = 'Site.Mean'
#                                 ,byVariable = 'State'
#                                 ,aggregateRow = 'Region')
# 
# item111.final.SF <- item111.final[which(item111.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item111.final) %in% c("BuildingType"))]
# item111.final.MH <- item111.final[which(item111.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item111.final) %in% c("BuildingType"))]
# 
# exportTable(item111.final.SF, "SF", "Table 118", weighted = TRUE)
# # exportTable(item111.final.MH, "MH", "Table 93", weighted = TRUE)
# 
# 
# 
# #######################
# # Unweighted Analysis
# #######################
# item111.final <- mean_one_group_unweighted(item111.data
#                                            ,valueVariable = 'Site.Mean'
#                                            ,byVariable = 'State'
#                                            ,aggregateRow = 'Region')
# 
# item111.final.SF <- item111.final[which(item111.final$BuildingType == "Single Family")
#                                   ,-which(colnames(item111.final) %in% c("BuildingType"))]
# item111.final.MH <- item111.final[which(item111.final$BuildingType == "Manufactured")
#                                   ,-which(colnames(item111.final) %in% c("BuildingType"))]
# 
# exportTable(item111.final.SF, "SF", "Table 118", weighted = FALSE)
# # exportTable(item111.final.MH, "MH", "Table 93", weighted = FALSE)








































############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean os data
os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
length(unique(os.dat$CK_Cadmus_ID))
os.dat$CK_Building_ID <- os.dat$Category
os.dat <- os.dat[which(names(os.dat) != "Category")]

#############################################################################################
#Item 111: AVERAGE PRIMARY TELEVISION ON-TIME HOURS PER DAY PER HOME BY CK_Building_ID (SF table 118, MH table 93)
#############################################################################################
#subset to columns needed for analysis
item111.os.dat <- sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                              ,"INTRVW_CUST_RES_HomeandEnergyUseHomeUse_HowManyHoursPerDayIsThePrimaryTVOn"
                                                                              ,""))]
colnames(item111.os.dat) <- c("CK_Cadmus_ID", "TV_on_hours")
item111.os.dat$count <- 1

#remove any repeat header rows from exporting
item111.os.dat0 <- item111.os.dat[which(item111.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#merge together analysis data with cleaned scl data
item111.os.dat1 <- left_join(item111.os.dat0, os.dat, by = "CK_Cadmus_ID")

#make tv hours to numeric
item111.os.dat1$TV_on_hours <- as.numeric(as.character(item111.os.dat1$TV_on_hours))
unique(item111.os.dat1$TV_on_hours)

#remove any NA in tv hours
item111.os.dat2 <- item111.os.dat1[which(!(is.na(item111.os.dat1$TV_on_hours))),]

#summarise by site
item111.os.sum <- summarise(group_by(item111.os.dat2, CK_Cadmus_ID, CK_Building_ID)
                         ,Site.Mean = mean(TV_on_hours))

item111.os.merge <- left_join(os.dat, item111.os.sum)
item111.os.merge <- item111.os.merge[which(!is.na(item111.os.merge$Site.Mean)),]

################################################
# Adding pop and sample sizes for weights
################################################
item111.os.data <- weightedData(item111.os.merge[-which(colnames(item111.os.merge) %in% c("Site.Mean"))])
item111.os.data <- left_join(item111.os.data, unique(item111.os.merge[which(colnames(item111.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Site.Mean"))]))
item111.os.data$count <- 1
#######################
# Weighted Analysis
#######################
item111.os.final <- mean_one_group(item111.os.data
                                ,valueVariable = 'Site.Mean'
                                ,byVariable = 'CK_Building_ID'
                                ,aggregateRow = 'Remove')
item111.os.final <- item111.os.final[which(item111.os.final$CK_Building_ID != "Remove"),]

item111.os.final.SF <- item111.os.final[which(item111.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item111.os.final) %in% c("BuildingType"))]

exportTable(item111.os.final.SF, "SF", "Table 118", weighted = TRUE, osIndicator = export.ind, OS = T)



#######################
# Unweighted Analysis
#######################
item111.os.final <- mean_one_group_unweighted(item111.os.data
                                           ,valueVariable = 'Site.Mean'
                                           ,byVariable = 'CK_Building_ID'
                                           ,aggregateRow = 'Remove')
item111.os.final <- item111.os.final[which(item111.os.final$CK_Building_ID != "Remove"),]

item111.os.final.SF <- item111.os.final[which(item111.os.final$BuildingType == "Single Family")
                                  ,-which(colnames(item111.os.final) %in% c("BuildingType"))]

exportTable(item111.os.final.SF, "SF", "Table 118", weighted = FALSE, osIndicator = export.ind, OS = T)
