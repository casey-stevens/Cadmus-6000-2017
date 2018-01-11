#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:          11/3/2017                                   
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
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

item59.dat2 <- item59.dat1[-grep("not present|presence unknown|not asked for", item59.dat1$System.Type,ignore.case = T),]
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

exportTable(item59.final.SF, "SF", "Table 66", weighted = TRUE)



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

exportTable(item59.final.SF, "SF", "Table 66", weighted = FALSE)






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
item60.dat2 <- item60.dat1[which(!(is.na(item60.dat1$Percentage.of.Supply.Ducts.in.Conditioned.Space))),]

#Make conditioned space numeric
item60.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space <- as.numeric(as.character(item60.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space))

#Create Unconditioned Space Percent
item60.dat2$PercentDuctsUnconditionedSpace <- 100 - item60.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space
#double check these make sense
item60.dat2$PercentDuctsUnconditionedSpace


#create percent ducts unconditioned space bins
item60.dat2$UnconditionedBins <- "MISSING"
item60.dat2$UnconditionedBins[which(item60.dat2$PercentDuctsUnconditionedSpace == 0)] <- "None"
item60.dat2$UnconditionedBins[which(item60.dat2$PercentDuctsUnconditionedSpace >= 1 & item60.dat2$PercentDuctsUnconditionedSpace < 51)] <- "1-50%"
item60.dat2$UnconditionedBins[which(item60.dat2$PercentDuctsUnconditionedSpace >= 51 & item60.dat2$PercentDuctsUnconditionedSpace < 100)] <- "51-99%"
item60.dat2$UnconditionedBins[which(item60.dat2$PercentDuctsUnconditionedSpace >= 100)] <- "100%"
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

exportTable(item60.final.SF, "SF", "Table 67", weighted = TRUE)




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

exportTable(item60.final.SF, "SF", "Table 67", weighted = FALSE)
