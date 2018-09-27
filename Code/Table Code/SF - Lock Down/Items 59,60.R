#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:          11/3/2017                                   
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

"%notin%" <- Negate("%in%")

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
# Mechanical
# mechanical.dat <- read.xlsx(mechanical.export)
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))







#############################################################################################
#Item 59: PERCENTAGE OF HOMES WITH DUCT SYSTEMS BY STATE (SF table 66)
#############################################################################################
#subset to columns needed for analysis
item59.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"System.Type"))]
#remove any repeat header rows 
item59.dat00 <- item59.dat[which(item59.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item59.dat$count <- 1
#check unique system types
unique(item59.dat00$Generic)

item59.dat0 <- left_join(rbsa.dat, item59.dat00, by = "CK_Cadmus_ID")

#subset to only Generic = Ducting
item59.dat1 <- unique(item59.dat0[which(item59.dat0$Generic == "Ducting"),])
length(unique(item59.dat1$CK_Cadmus_ID))
unique(item59.dat1$System.Type)

item59.dat2 <- item59.dat1[-grep("not present|presence unknown|not asked for|N/A", item59.dat1$System.Type,ignore.case = T),]
unique(item59.dat2$System.Type)

# Add count var
item59.dat2$Ind <- 1

item59.dat3 <- left_join(rbsa.dat, item59.dat2)
item59.dat3$Ind[which(is.na(item59.dat3$Ind))] <- 0
which(duplicated(item59.dat3$CK_Cadmus_ID))

item59.customer <- summarise(group_by(item59.dat3, CK_Cadmus_ID)
                             ,Ind = sum(unique(Ind)))
item59.customer$Ind[which(item59.customer$Ind > 0)] <- 1

item59.merge <- left_join(rbsa.dat, item59.customer)
item59.merge <- item59.merge[grep("site",item59.merge$CK_Building_ID, ignore.case = T),]

# Weighting function
item59.data <- weightedData(item59.merge[-which(colnames(item59.merge) %in% c("Generic"
                                                                            ,"System.Type"
                                                                            ,"count"
                                                                            ,"Ind"))])
item59.data <- left_join(item59.data, item59.merge[which(colnames(item59.merge) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"System.Type"
                                                                                     ,"Ind"))])
item59.data$Count <- 1
item59.data$count <- 1

#############################
# Weighted Analysis
#############################
item59.final <- proportions_one_group(CustomerLevelData  = item59.data
                                      , valueVariable    = 'Ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , weighted = TRUE)

# SF = Table 66
# Export table
item59.final.SF <- item59.final[which(item59.final$BuildingType == "Single Family"),-1]

# exportTable(item59.final.SF, "SF", "Table 66", weighted = TRUE)



#############################
# Unweighted Analysis
#############################
item59.final <- proportions_one_group(CustomerLevelData  = item59.data
                                      , valueVariable    = 'Ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , weighted = FALSE)

# SF = Table 66
# Export table
item59.final.SF <- item59.final[which(item59.final$BuildingType == "Single Family")
                                ,-which(colnames(item59.final) %in% c("BuildingType"))]

# exportTable(item59.final.SF, "SF", "Table 66", weighted = FALSE)






#############################################################################################
#Item 60: PERCENTAGE OF HOMES WITH DUCT SYSTEMS BY STATE (SF table 66)
#############################################################################################
#subset to columns needed for analysis
item60.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"System.Type"
                                                                   ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"))]
#check unique values for conditioned space
unique(item60.dat$Percentage.of.Supply.Ducts.in.Conditioned.Space)

item60.dat0 <- left_join(rbsa.dat, item60.dat, by = "CK_Cadmus_ID")

#remove coditioned space = datapoint not asked for
item60.dat1 <- item60.dat0[which(item60.dat0$Percentage.of.Supply.Ducts.in.Conditioned.Space != "-- Datapoint not asked for --"),]
#remove conditioned space = NA
item60.dat2 <- item60.dat1[which(item60.dat1$Percentage.of.Supply.Ducts.in.Conditioned.Space %notin% c("N/A",NA)),]

#Make conditioned space numeric
item60.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space <- as.numeric(as.character(item60.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space))

#Create Unconditioned Space Percent
item60.dat2$PercentDuctsUnconditionedSpace <- 1 - item60.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space
#double check these make sense
item60.dat2$PercentDuctsUnconditionedSpace

#create percent ducts unconditioned space bins
item60.dat2$UnconditionedBins <- "MISSING"
item60.dat2$UnconditionedBins[which(item60.dat2$PercentDuctsUnconditionedSpace == 0)] <- "None"
item60.dat2$UnconditionedBins[which(item60.dat2$PercentDuctsUnconditionedSpace >= 0.01 & item60.dat2$PercentDuctsUnconditionedSpace < 0.51)] <- "1-50%"
item60.dat2$UnconditionedBins[which(item60.dat2$PercentDuctsUnconditionedSpace >= 0.51 & item60.dat2$PercentDuctsUnconditionedSpace < 1)] <- "51-99%"
item60.dat2$UnconditionedBins[which(item60.dat2$PercentDuctsUnconditionedSpace >= 1)] <- "100%"
unique(item60.dat2$UnconditionedBins)
item60.dat2$count <- 1

item60.dat3 <- item60.dat2[which(item60.dat2$CK_Cadmus_ID != "CK_CADMUS_ID"),]
item60.dat4 <- item60.dat3[which(item60.dat3$UnconditionedBins != "MISSING"),]


item60.data <- weightedData(item60.dat4[-which(colnames(item60.dat4) %in% c("Generic"
                                                                            ,"System.Type"
                                                                            ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"
                                                                            ,"PercentDuctsUnconditionedSpace"
                                                                            ,"UnconditionedBins"
                                                                            ,"count"))])

item60.data <- left_join(item60.data, item60.dat4[which(colnames(item60.dat4) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"System.Type"
                                                                                     ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"
                                                                                     ,"PercentDuctsUnconditionedSpace"
                                                                                     ,"UnconditionedBins"
                                                                                     ,"count"))])
################################
# Weighted Analysis
################################
item60.final <- proportionRowsAndColumns1(CustomerLevelData = item60.data
                                          , valueVariable       = 'count'
                                          , columnVariable      = 'State'
                                          , rowVariable         = 'UnconditionedBins'
                                          , aggregateColumnName = 'Region')

item60.cast <- dcast(setDT(item60.final)
                     , formula = BuildingType + UnconditionedBins ~ State
                     , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))

item60.table <- data.frame("BuildingType"     = item60.cast$BuildingType
                           ,"Percentage.of.Ducts.in.Unconditioned.Space" = item60.cast$UnconditionedBins
                           ,"Percent_ID"     = item60.cast$w.percent_ID
                           ,"SE_ID"          = item60.cast$w.SE_ID
                           ,"n_ID"           = item60.cast$n_ID
                           ,"Percent_MT"     = item60.cast$w.percent_MT
                           ,"SE_MT"          = item60.cast$w.SE_MT
                           ,"n_MT"           = item60.cast$n_MT
                           ,"Percent_OR"     = item60.cast$w.percent_OR
                           ,"SE_OR"          = item60.cast$w.SE_OR
                           ,"n_OR"           = item60.cast$n_OR
                           ,"Percent_WA"     = item60.cast$w.percent_WA
                           ,"SE_WA"          = item60.cast$w.SE_WA
                           ,"n_WA"           = item60.cast$n_WA
                           ,"Percent_Region" = item60.cast$w.percent_Region
                           ,"SE_Region"      = item60.cast$w.SE_Region
                           ,"n_Region"       = item60.cast$n_Region
                           ,"EB_ID"          = item60.cast$EB_ID
                           ,"EB_MT"          = item60.cast$EB_MT
                           ,"EB_OR"          = item60.cast$EB_OR
                           ,"EB_WA"          = item60.cast$EB_WA
                           ,"EB_Region"      = item60.cast$EB_Region
)

levels(item60.table$Percentage.of.Ducts.in.Unconditioned.Space)
rowOrder <- c("1-50%"
              ,"51-99%"
              ,"100%"
              ,"None"
              ,"Total")
item60.table <- item60.table %>% mutate(Percentage.of.Ducts.in.Unconditioned.Space = factor(Percentage.of.Ducts.in.Unconditioned.Space, levels = rowOrder)) %>% arrange(Percentage.of.Ducts.in.Unconditioned.Space)  
item60.table <- data.frame(item60.table)


item60.final.SF <- item60.table[which(item60.table$BuildingType == "Single Family")
                                ,-which(colnames(item60.table) %in% c("BuildingType"))]

# exportTable(item60.final.SF, "SF", "Table 67", weighted = TRUE)




#############################
# Unweighted Analysis
#############################
item60.final <- proportions_two_groups_unweighted(CustomerLevelData = item60.data
                                                  , valueVariable       = 'count'
                                                  , columnVariable      = 'State'
                                                  , rowVariable         = 'UnconditionedBins'
                                                  , aggregateColumnName = 'Region')

item60.cast <- dcast(setDT(item60.final)
                     , formula = BuildingType + UnconditionedBins ~ State
                     , value.var = c("Percent", "SE", "Count", "n"))


item60.table <- data.frame("BuildingType"     = item60.cast$BuildingType
                           ,"Percentage.of.Ducts.in.Unconditioned.Space"   = item60.cast$UnconditionedBins
                           ,"Percent_ID"     = item60.cast$Percent_ID
                           ,"SE_ID"          = item60.cast$SE_ID
                           ,"n_ID"           = item60.cast$n_ID
                           ,"Percent_MT"     = item60.cast$Percent_MT
                           ,"SE_MT"          = item60.cast$SE_MT
                           ,"n_MT"           = item60.cast$n_MT
                           ,"Percent_OR"     = item60.cast$Percent_OR
                           ,"SE_OR"          = item60.cast$SE_OR
                           ,"n_OR"           = item60.cast$n_OR
                           ,"Percent_WA"     = item60.cast$Percent_WA
                           ,"SE_WA"          = item60.cast$SE_WA
                           ,"n_WA"           = item60.cast$n_WA
                           ,"Percent_Region" = item60.cast$Percent_Region
                           ,"SE_Region"      = item60.cast$SE_Region
                           ,"n_Region"       = item60.cast$n_Region
)

levels(item60.table$Percentage.of.Ducts.in.Unconditioned.Space)
rowOrder <- c("1-50%"
              ,"51-99%"
              ,"100%"
              ,"None"
              ,"Total")
item60.table <- item60.table %>% mutate(Percentage.of.Ducts.in.Unconditioned.Space = factor(Percentage.of.Ducts.in.Unconditioned.Space, levels = rowOrder)) %>% arrange(Percentage.of.Ducts.in.Unconditioned.Space)  
item60.table <- data.frame(item60.table)


item60.final.SF <- item60.table[which(item60.table$BuildingType == "Single Family")
                                ,-which(colnames(item60.table) %in% c("BuildingType"))]

# exportTable(item60.final.SF, "SF", "Table 67", weighted = FALSE)
























# ############################################################################################################
# #
# #
# # OVERSAMPLE ANALYSIS
# #
# #
# ############################################################################################################
# 
# # Read in clean scl data
# os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
# length(unique(os.dat$CK_Cadmus_ID))
# os.dat$CK_Building_ID <- os.dat$Category
# os.dat <- os.dat[which(names(os.dat) != "Category")]
# 
# #############################################################################################
# #Item 59: PERCENTAGE OF HOMES WITH DUCT SYSTEMS BY CK_Building_ID (SF table 66)
# #############################################################################################
# #subset to columns needed for analysis
# item59.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
#                                                                    ,"Generic"
#                                                                    ,"System.Type"))]
# #remove any repeat header rows 
# item59.os.dat00 <- item59.os.dat[which(item59.os.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# 
# item59.os.dat$count <- 1
# #check unique system types
# unique(item59.os.dat00$Generic)
# 
# item59.os.dat0 <- left_join(os.dat, item59.os.dat00, by = "CK_Cadmus_ID")
# 
# #subset to only Generic = Ducting
# item59.os.dat1 <- unique(item59.os.dat0[which(item59.os.dat0$Generic == "Ducting"),])
# length(unique(item59.os.dat1$CK_Cadmus_ID))
# unique(item59.os.dat1$System.Type)
# 
# item59.os.dat2 <- item59.os.dat1[-grep("not present|presence unknown|not asked for|N/A", item59.os.dat1$System.Type,ignore.case = T),]
# unique(item59.os.dat2$System.Type)
# 
# # Add count var
# item59.os.dat2$Ind <- 1
# 
# item59.os.dat3 <- left_join(os.dat, item59.os.dat2)
# item59.os.dat3$Ind[which(is.na(item59.os.dat3$Ind))] <- 0
# which(duplicated(item59.os.dat3$CK_Cadmus_ID))
# 
# item59.os.customer <- summarise(group_by(item59.os.dat3, CK_Cadmus_ID, CK_Building_ID)
#                              ,Ind = sum(unique(Ind)))
# item59.os.customer$Ind[which(item59.os.customer$Ind > 0)] <- 1
# 
# item59.os.merge <- left_join(os.dat, item59.os.customer)
# 
# # Weighting function
# item59.os.data <- weightedData(item59.os.merge[-which(colnames(item59.os.merge) %in% c("Ind"))])
# item59.os.data <- left_join(item59.os.data, unique(item59.os.merge[which(colnames(item59.os.merge) %in% c("CK_Cadmus_ID"
#                                                                                                           ,"Ind"))]))
# item59.os.data$Count <- 1
# item59.os.data$count <- 1
# 
# #############################
# # Weighted Analysis
# #############################
# item59.os.final <- proportions_one_group(CustomerLevelData  = item59.os.data
#                                       , valueVariable    = 'Ind'
#                                       , groupingVariable = 'CK_Building_ID'
#                                       , total.name       = "Remove"
#                                       , weighted = TRUE)
# 
# levels(item59.os.final$CK_Building_ID)
# if(os.ind == "scl"){
#   rowOrder <- c("SCL GenPop"
#                 ,"SCL LI"
#                 ,"SCL EH"
#                 ,"2017 RBSA PS")
# }else if(os.ind == "snopud"){
#   rowOrder <- c("SnoPUD"
#                 ,"2017 RBSA PS"
#                 ,"2017 RBSA NW")
# }
# item59.os.final <- item59.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)  
# item59.os.final <- data.frame(item59.os.final)
# 
# 
# item59.os.final.SF <- item59.os.final[which(item59.os.final$CK_Building_ID != "Total"),-1]
# 
# # exportTable(item59.os.final.SF, "SF", "Table 66", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# 
# 
# #############################
# # Unweighted Analysis
# #############################
# item59.os.final <- proportions_one_group(CustomerLevelData  = item59.os.data
#                                       , valueVariable    = 'Ind'
#                                       , groupingVariable = 'CK_Building_ID'
#                                       , total.name       = "Remove"
#                                       , weighted = FALSE)
# 
# levels(item59.os.final$CK_Building_ID)
# if(os.ind == "scl"){
#   rowOrder <- c("SCL GenPop"
#                 ,"SCL LI"
#                 ,"SCL EH"
#                 ,"2017 RBSA PS")
# }else if(os.ind == "snopud"){
#   rowOrder <- c("SnoPUD"
#                 ,"2017 RBSA PS"
#                 ,"2017 RBSA NW")
# }
# item59.os.final <- item59.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)  
# item59.os.final <- data.frame(item59.os.final)
# 
# item59.os.final.SF <- item59.os.final[which(item59.os.final$CK_Building_ID != "Total")
#                                 ,-which(colnames(item59.os.final) %in% c("BuildingType"))]
# 
# # exportTable(item59.os.final.SF, "SF", "Table 66", weighted = FALSE, osIndicator = export.ind, OS = T)
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 60: PERCENTAGE OF HOMES WITH DUCT SYSTEMS BY CK_Building_ID (SF table 66)
# #############################################################################################
# #subset to columns needed for analysis
# item60.os.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
#                                                                    ,"Generic"
#                                                                    ,"System.Type"
#                                                                    ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"))]
# #check unique values for conditioned space
# unique(item60.os.dat$Percentage.of.Supply.Ducts.in.Conditioned.Space)
# 
# item60.os.dat0 <- left_join(os.dat, item60.os.dat, by = "CK_Cadmus_ID")
# 
# #remove coditioned space = datapoint not asked for
# item60.os.dat1 <- item60.os.dat0[which(item60.os.dat0$Percentage.of.Supply.Ducts.in.Conditioned.Space != "-- Datapoint not asked for --"),]
# #remove conditioned space = NA
# item60.os.dat2 <- item60.os.dat1[which(item60.os.dat1$Percentage.of.Supply.Ducts.in.Conditioned.Space %notin% c("N/A",NA)),]
# 
# #Make conditioned space numeric
# item60.os.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space <- as.numeric(as.character(item60.os.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space))
# 
# #Create Unconditioned Space Percent
# item60.os.dat2$PercentDuctsUnconditionedSpace <- 1 - item60.os.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space
# #double check these make sense
# item60.os.dat2$PercentDuctsUnconditionedSpace
# 
# 
# #create percent ducts unconditioned space bins
# item60.os.dat2$UnconditionedBins <- "MISSING"
# item60.os.dat2$UnconditionedBins[which(item60.os.dat2$PercentDuctsUnconditionedSpace == 0)] <- "None"
# item60.os.dat2$UnconditionedBins[which(item60.os.dat2$PercentDuctsUnconditionedSpace >= 0.01 & item60.os.dat2$PercentDuctsUnconditionedSpace < 0.51)] <- "1-50%"
# item60.os.dat2$UnconditionedBins[which(item60.os.dat2$PercentDuctsUnconditionedSpace >= 0.51 & item60.os.dat2$PercentDuctsUnconditionedSpace < 1)] <- "51-99%"
# item60.os.dat2$UnconditionedBins[which(item60.os.dat2$PercentDuctsUnconditionedSpace >= 1)] <- "100%"
# unique(item60.os.dat2$UnconditionedBins)
# item60.os.dat2$count <- 1
# 
# item60.os.dat3 <- item60.os.dat2[which(item60.os.dat2$CK_Cadmus_ID != "CK_CADMUS_ID"),]
# item60.os.dat4 <- item60.os.dat3[which(item60.os.dat3$UnconditionedBins != "MISSING"),]
# 
# 
# item60.os.data <- weightedData(item60.os.dat4[-which(colnames(item60.os.dat4) %in% c("Generic"
#                                                                                      ,"System.Type"
#                                                                                      ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"
#                                                                                      ,"PercentDuctsUnconditionedSpace"
#                                                                                      ,"UnconditionedBins"
#                                                                                      ,"count"))])
# 
# item60.os.data <- left_join(item60.os.data, unique(item60.os.dat4[which(colnames(item60.os.dat4) %in% c("CK_Cadmus_ID"
#                                                                                                  ,"Generic"
#                                                                                                  ,"System.Type"
#                                                                                                  ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"
#                                                                                                  ,"PercentDuctsUnconditionedSpace"
#                                                                                                  ,"UnconditionedBins"
#                                                                                                  ,"count"))]))
# ################################
# # Weighted Analysis
# ################################
# item60.os.final <- proportionRowsAndColumns1(CustomerLevelData = item60.os.data
#                                           , valueVariable       = 'count'
#                                           , columnVariable      = 'CK_Building_ID'
#                                           , rowVariable         = 'UnconditionedBins'
#                                           , aggregateColumnName = 'Remove')
# 
# item60.os.cast <- dcast(setDT(item60.os.final)
#                      , formula = BuildingType + UnconditionedBins ~ CK_Building_ID
#                      , value.var = c("w.percent", "w.SE", "count", "n", "N", "EB"))
# 
# names(item60.os.cast)
# if(os.ind == "scl"){
#   item60.os.table <- data.frame("BuildingType"     = item60.os.cast$BuildingType
#                                 ,"Percentage.of.Ducts.in.Unconditioned.Space" = item60.os.cast$UnconditionedBins
#                                 ,"Percent_SCL.GenPop"   = item60.os.cast$`w.percent_SCL GenPop`
#                                 ,"SE_SCL.GenPop"        = item60.os.cast$`w.SE_SCL GenPop`
#                                 ,"n_SCL.GenPop"         = item60.os.cast$`n_SCL GenPop`
#                                 ,"Percent_SCL.LI"       = item60.os.cast$`w.percent_SCL LI`
#                                 ,"SE_SCL.LI"            = item60.os.cast$`w.SE_SCL LI`
#                                 ,"n_SCL.LI"             = item60.os.cast$`n_SCL LI`
#                                 ,"Percent_SCL.EH"       = item60.os.cast$`w.percent_SCL EH`
#                                 ,"SE_SCL.EH"            = item60.os.cast$`w.SE_SCL EH`
#                                 ,"n_SCL.EH"             = item60.os.cast$`n_SCL EH`
#                                 ,"Percent_2017.RBSA.PS" = item60.os.cast$`w.percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item60.os.cast$`w.SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item60.os.cast$`n_2017 RBSA PS`
#                                 ,"EB_SCL.GenPop"        = item60.os.cast$`EB_SCL GenPop`
#                                 ,"EB_SCL.LI"            = item60.os.cast$`EB_SCL LI`
#                                 ,"EB_SCL.EH"            = item60.os.cast$`EB_SCL EH`
#                                 ,"EB_2017.RBSA.PS"      = item60.os.cast$`EB_2017 RBSA PS`)
# }else if(os.ind == "snopud"){
#   item60.os.table <- data.frame("BuildingType"     = item60.os.cast$BuildingType
#                                 ,"Percentage.of.Ducts.in.Unconditioned.Space" = item60.os.cast$UnconditionedBins
#                                 ,"Percent_SnoPUD"          = item60.os.cast$`w.percent_SnoPUD`
#                                 ,"SE_SnoPUD"               = item60.os.cast$`w.SE_SnoPUD`
#                                 ,"n_SnoPUD"                = item60.os.cast$`n_SnoPUD`
#                                 ,"Percent_2017.RBSA.PS"    = item60.os.cast$`w.percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"         = item60.os.cast$`w.SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"          = item60.os.cast$`n_2017 RBSA PS`
#                                 ,"Percent_RBSA.NW"         = item60.os.cast$`w.percent_2017 RBSA NW`
#                                 ,"SE_RBSA.NW"              = item60.os.cast$`w.SE_2017 RBSA NW`
#                                 ,"n_RBSA.NW"               = item60.os.cast$`n_2017 RBSA NW`
#                                 ,"EB_SnoPUD"               = item60.os.cast$`EB_SnoPUD`
#                                 ,"EB_2017.RBSA.PS"         = item60.os.cast$`EB_2017 RBSA PS`
#                                 ,"EB_RBSA.NW"              = item60.os.cast$`EB_2017 RBSA NW`)
# }
# 
# 
# 
# 
# levels(item60.os.table$Percentage.of.Ducts.in.Unconditioned.Space)
# rowOrder <- c("1-50%"
#               ,"51-99%"
#               ,"100%"
#               ,"None"
#               ,"Total")
# item60.os.table <- item60.os.table %>% mutate(Percentage.of.Ducts.in.Unconditioned.Space = factor(Percentage.of.Ducts.in.Unconditioned.Space, levels = rowOrder)) %>% arrange(Percentage.of.Ducts.in.Unconditioned.Space)  
# item60.os.table <- data.frame(item60.os.table)
# 
# item60.os.final.SF <- item60.os.table[which(item60.os.table$BuildingType == "Single Family")
#                                 ,-which(colnames(item60.os.table) %in% c("BuildingType"))]
# 
# # exportTable(item60.os.final.SF, "SF", "Table 67", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# #############################
# # Unweighted Analysis
# #############################
# item60.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item60.os.data
#                                                   , valueVariable       = 'count'
#                                                   , columnVariable      = 'CK_Building_ID'
#                                                   , rowVariable         = 'UnconditionedBins'
#                                                   , aggregateColumnName = 'Remove')
# 
# item60.os.cast <- dcast(setDT(item60.os.final)
#                      , formula = BuildingType + UnconditionedBins ~ CK_Building_ID
#                      , value.var = c("Percent", "SE", "Count", "n"))
# 
# names(item60.os.cast)
# if(os.ind == "scl"){
#   item60.os.table <- data.frame("BuildingType"     = item60.os.cast$BuildingType
#                                 ,"Percentage.of.Ducts.in.Unconditioned.Space"   = item60.os.cast$UnconditionedBins
#                                 ,"Percent_SCL.GenPop"   = item60.os.cast$`Percent_SCL GenPop`
#                                 ,"SE_SCL.GenPop"        = item60.os.cast$`SE_SCL GenPop`
#                                 ,"n_SCL.GenPop"         = item60.os.cast$`n_SCL GenPop`
#                                 ,"Percent_SCL.LI"       = item60.os.cast$`Percent_SCL LI`
#                                 ,"SE_SCL.LI"            = item60.os.cast$`SE_SCL LI`
#                                 ,"n_SCL.LI"             = item60.os.cast$`n_SCL LI`
#                                 ,"Percent_SCL.EH"       = item60.os.cast$`Percent_SCL EH`
#                                 ,"SE_SCL.EH"            = item60.os.cast$`SE_SCL EH`
#                                 ,"n_SCL.EH"             = item60.os.cast$`n_SCL EH`
#                                 ,"Percent_2017.RBSA.PS" = item60.os.cast$`Percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item60.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item60.os.cast$`n_2017 RBSA PS`)
# }else if(os.ind == "snopud"){
#   item60.os.table <- data.frame("BuildingType"     = item60.os.cast$BuildingType
#                                 ,"Percentage.of.Ducts.in.Unconditioned.Space"   = item60.os.cast$UnconditionedBins
#                                 ,"Percent_SnoPUD"          = item60.os.cast$`Percent_SnoPUD`
#                                 ,"SE_SnoPUD"               = item60.os.cast$`SE_SnoPUD`
#                                 ,"n_SnoPUD"                = item60.os.cast$`n_SnoPUD`
#                                 ,"Percent_2017.RBSA.PS"    = item60.os.cast$`Percent_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"         = item60.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"          = item60.os.cast$`n_2017 RBSA PS`
#                                 ,"Percent_RBSA.NW"         = item60.os.cast$`Percent_2017 RBSA NW`
#                                 ,"SE_RBSA.NW"              = item60.os.cast$`SE_2017 RBSA NW`
#                                 ,"n_RBSA.NW"               = item60.os.cast$`n_2017 RBSA NW`)
# }
# 
# 
# 
# 
# levels(item60.os.table$Percentage.of.Ducts.in.Unconditioned.Space)
# rowOrder <- c("1-50%"
#               ,"51-99%"
#               ,"100%"
#               ,"None"
#               ,"Total")
# item60.os.table <- item60.os.table %>% mutate(Percentage.of.Ducts.in.Unconditioned.Space = factor(Percentage.of.Ducts.in.Unconditioned.Space, levels = rowOrder)) %>% arrange(Percentage.of.Ducts.in.Unconditioned.Space)  
# item60.os.table <- data.frame(item60.os.table)
# 
# 
# item60.os.final.SF <- item60.os.table[which(item60.os.table$BuildingType == "Single Family")
#                                 ,-which(colnames(item60.os.table) %in% c("BuildingType"))]
# 
# # exportTable(item60.os.final.SF, "SF", "Table 67", weighted = FALSE, osIndicator = export.ind, OS = T)
