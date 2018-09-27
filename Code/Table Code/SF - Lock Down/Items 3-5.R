#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:          09/11/2017                                  
##  Billing Code(s):  6596.0000.0002.1002.0000
#############################################################################################

##  Clear variables
# rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)

# # Source codes
# source("Code/Table Code/SourceCode.R")
# source("Code/Table Code/Weighting Implementation Functions.R")
# source("Code/Sample Weighting/Weights.R")
# source("Code/Table Code/Export Function.R")

##  Create "Not In" operator
"%notin%" <- Negate("%in%")


rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
rbsa.dat <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),] 


#Read in data for analysis
# envelope.dat <- read.xlsx(envelope.export)
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))
stopifnot(length(unique(envelope.dat$CK_Cadmus_ID)) <= length(unique(rbsa.dat$CK_Cadmus_ID)))

# one.line.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.export), startRow = 2, sheet = "Site One Line Summary")
one.line.dat$CK_Cadmus_ID <- trimws(toupper(one.line.dat$Cadmus.ID))

# Bring in clean ground contact types
GroundContactTypes <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "Ground Contact Types.xlsx"), sheet = 1)
GroundContactTypes <- GroundContactTypes[which(colnames(GroundContactTypes) %in% c("Raw.data.categories", "Final"))]




#############################################################################################
# Item 3: DISTRIBUTION OF HOMES BY GROUND CONTACT TYPE AND STATE 
#############################################################################################
env.dat <- one.line.dat[which(colnames(one.line.dat) %in% c("CK_Cadmus_ID"
                                                            , "Foundation.Type"))]
colnames(env.dat) <- c("FoundationType","CK_Cadmus_ID")
env.dat1 <- env.dat[which(!(is.na(env.dat$FoundationType))),]
env.dat1$FoundationType <- trimws(env.dat1$FoundationType)

#merge table columns to generic columns
item3.dat <- unique(left_join(rbsa.dat, env.dat1, by = "CK_Cadmus_ID"))

unique(item3.dat$FoundationType)

length(unique(item3.dat$CK_Cadmus_ID[which(item3.dat$FoundationType == "Other")]))

# # Clean Ground Contact types
# i=10
item3.dat$GroundContact <- item3.dat$FoundationType
# for (i in 1:length(GroundContactTypes$Raw.data.categories)){
#   item3.dat$GroundContact[which(item3.dat$GroundContact == GroundContactTypes$Raw.data.categories[i])] <- GroundContactTypes$New.categories[i]
# }
# item3.dat$GroundContact <- trimws(item3.dat$GroundContact)
# # End cleaning Step
# unique(item3.dat$GroundContact)
# 
# item3.dat$GroundContact <- gsub("&gt; ",">", item3.dat$GroundContact)
item3.dat$GroundContact[grep("90% crawl", item3.dat$GroundContact, ignore.case = T)] <- ">90% Crawlspace"
item3.dat$GroundContact[grep("90% conditioned", item3.dat$GroundContact, ignore.case = T)] <- ">90% Conditioned Basement"

# Remove unwanted ground contact types
item3.dat1 <- item3.dat[which(item3.dat$GroundContact %notin% c("Remove", NA, 0)),]

#subset to only single family for item 3
item3.dat2 <- item3.dat1[which(item3.dat1$BuildingType == "Single Family"),]
item3.dat2$count <- 1
item3.customer <- summarise(group_by(item3.dat2, CK_Cadmus_ID, State, GroundContact)
                              ,m_ilk = sum(count))
item3.merge <- left_join(rbsa.dat, item3.customer)
item3.merge <- item3.merge[which(!is.na(item3.merge$m_ilk)),]



#add weighting information
item3.data <- weightedData(item3.merge[-which(colnames(item3.merge) %in% c("m_ilk", "GroundContact"))])

item3.data <- left_join(item3.data, item3.merge[which(colnames(item3.merge) %in% c("CK_Cadmus_ID", "m_ilk", "GroundContact"))])

item3.data$count <- 1
colnames(item3.data)




##############################
# Weighted Analysis
##############################
item3.final <- proportions_two_groups_domain(CustomerLevelData     = item3.data
                                             , valueVariable       = 'm_ilk'
                                             , byVariableColumn    = 'State'
                                             , byVariableRow       = 'GroundContact'
                                             , aggregateColumn     = "Region"
                                             , aggregateRow        = "Total")
item3.cast <- dcast(setDT(item3.final)
                   ,formula = BuildingType + GroundContact ~ State
                   ,value.var = c("w.percent", "w.SE", "n", "EB"))

item3.table <- data.frame("BuildingType"    = item3.cast$BuildingType
                          ,"GroundContact"  = item3.cast$GroundContact
                          ,"Percent_ID"     = item3.cast$w.percent_ID
                          ,"SE_ID"          = item3.cast$w.SE_ID
                          ,"n_ID"           = item3.cast$n_ID
                          ,"Percent_MT"     = item3.cast$w.percent_MT
                          ,"SE_MT"          = item3.cast$w.SE_MT
                          ,"n_MT"           = item3.cast$n_MT
                          ,"Percent_OR"     = item3.cast$w.percent_OR
                          ,"SE_OR"          = item3.cast$w.SE_OR
                          ,"n_OR"           = item3.cast$n_OR
                          ,"Percent_WA"     = item3.cast$w.percent_WA
                          ,"SE_WA"          = item3.cast$w.SE_WA
                          ,"n_WA"           = item3.cast$n_WA
                          ,"Percent_Region" = item3.cast$w.percent_Region
                          ,"SE_Region"      = item3.cast$w.SE_Region
                          ,"n_Region"       = item3.cast$n_Region
                          ,"EB_ID"          = item3.cast$EB_ID
                          ,"EB_MT"          = item3.cast$EB_MT
                          ,"EB_OR"          = item3.cast$EB_OR
                          ,"EB_WA"          = item3.cast$EB_WA
                          ,"EB_Region"      = item3.cast$EB_Region)

item3.table.SF <- item3.table[which(item3.table$BuildingType == "Single Family"),-1]

# exportTable(item3.table.SF, "SF", "Table 10", weighted = TRUE)



##############################
# Unweighted Analysis
##############################
item3.final <- proportions_two_groups_unweighted(CustomerLevelData = item3.data
                                         , valueVariable = 'count'
                                         , columnVariable = 'State'
                                         , rowVariable = 'GroundContact'
                                         , aggregateColumnName = "Region")

item3.cast <- dcast(setDT(item3.final)
                    ,formula = BuildingType + GroundContact ~ State
                    ,value.var = c("Percent", "SE", "Count", "n"))

item3.table <- data.frame("BuildingType"    = item3.cast$BuildingType
                          ,"GroundContact"  = item3.cast$GroundContact
                          ,"Percent_ID"     = item3.cast$Percent_ID
                          ,"SE_ID"          = item3.cast$SE_ID
                          ,"n_ID"           = item3.cast$n_ID
                          ,"Percent_MT"     = item3.cast$Percent_MT
                          ,"SE_MT"          = item3.cast$SE_MT
                          ,"n_MT"           = item3.cast$n_MT
                          ,"Percent_OR"     = item3.cast$Percent_OR
                          ,"SE_OR"          = item3.cast$SE_OR
                          ,"n_OR"           = item3.cast$n_OR
                          ,"Percent_WA"     = item3.cast$Percent_WA
                          ,"SE_WA"          = item3.cast$SE_WA
                          ,"n_WA"           = item3.cast$n_WA
                          ,"Percent_Region" = item3.cast$Percent_Region
                          ,"SE_Region"      = item3.cast$SE_Region
                          ,"n_Region"       = item3.cast$n_Region)

item3.table.SF <- item3.table[which(item3.table$BuildingType == "Single Family"),-1]

# exportTable(item3.table.SF, "SF", "Table 10", weighted = FALSE)






#############################################################################################
# Item 4: AVERAGE CONDITIONED FLOOR AREA BY STATE
#############################################################################################
######################################################
# Weighting Implementation function: Mean, one group
######################################################
env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                            , "Conditioned.Living.Area"))]
colnames(env.dat) <- c("CK_Cadmus_ID"
                       , "BldgLevel_Area_SqFt")

#make conditioned area as.numeric
env.dat$ConditionedArea <- as.numeric(as.character(env.dat$BldgLevel_Area_SqFt))


#merge
item4.dat <- left_join(rbsa.dat, env.dat, by = "CK_Cadmus_ID")
length(unique(item4.dat$CK_Cadmus_ID))

item4.dat1 <- item4.dat[which(item4.dat$BuildingType != "Multifamily"),]
item4.dat2 <- item4.dat1[which(!is.na(item4.dat1$ConditionedArea)),]

######################################################
# Summarise data up to unique customer level
######################################################
item4.customer <- summarise(group_by(item4.dat2, CK_Cadmus_ID)
                            ,siteAreaConditioned = sum(ConditionedArea))

item4.merge <- left_join(rbsa.dat, item4.customer)
item4.merge <- item4.merge[which(!is.na(item4.merge$siteAreaConditioned)),]


item4.data <- weightedData(item4.merge[-which(colnames(item4.merge) %in% c("BldgLevel_Area_SqFt"
                                                                         ,"siteAreaConditioned"))])
item4.data <- left_join(item4.data, item4.merge[which(colnames(item4.merge) %in% c("CK_Cadmus_ID"
                                                                                 ,"BldgLevel_Area_SqFt"
                                                                                 , "siteAreaConditioned"))])

item4.data$count <- 1
colnames(item4.data)




##############################
# Weighted Analysis
##############################
item4.final <- mean_one_group(CustomerLevelData = item4.data
                              , valueVariable = 'siteAreaConditioned'
                              , byVariable    = 'State'
                              , aggregateRow  = 'Region')


item4.table.SF <- item4.final[which(item4.final$BuildingType %in% c("Single Family")),-1]
item4.table.MH <- item4.final[which(item4.final$BuildingType %in% c("Manufactured")),-1]

# exportTable(item4.table.SF, "SF", "Table 11", weighted = TRUE)
# exportTable(item4.table.MH, "MH", "Table 10", weighted = TRUE)



##############################
# Unweighted Analysis
##############################
item4.final <- mean_one_group_unweighted(CustomerLevelData = item4.data
                                         , valueVariable = 'siteAreaConditioned'
                                         , byVariable    = 'State'
                                         , aggregateRow  = 'Region')


item4.table.SF <- item4.final[which(item4.final$BuildingType %in% c("Single Family")),-1]
item4.table.MH <- item4.final[which(item4.final$BuildingType %in% c("Manufactured")),-1]

# exportTable(item4.table.SF, "SF", "Table 11", weighted = FALSE)
# exportTable(item4.table.MH, "MH", "Table 10", weighted = FALSE)








##########################################################################
# Item 5: AVERAGE CONDITIONED FLOOR AREA BY STATE AND VINTAGE
##########################################################################
######################################################
# Weighting Implementation function: Mean, two groups
######################################################

# env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
#                                                             , "Conditioned.Living.Area"))]
# colnames(env.dat) <- c("CK_Cadmus_ID"
#                        , "BldgLevel_Area_SqFt")
# 
# #make conditioned area as.numeric
# env.dat$ConditionedArea <- as.numeric(as.character(env.dat$BldgLevel_Area_SqFt))


#merge
item5.dat <- rbsa.dat#left_join(rbsa.dat, env.dat, by = "CK_Cadmus_ID")
length(unique(item5.dat$CK_Cadmus_ID))

item5.dat1 <- item5.dat[which(item5.dat$BuildingType != "Multifamily"),]
# item5.dat1 <- item5.dat1[-grep("bldg", item5.dat1$CK_Building_ID, ignore.case = T),]
item5.dat2 <- item5.dat1[which(!is.na(item5.dat1$Conditioned.Area)),]
item5.dat2 <- item5.dat2[which(item5.dat2$Conditioned.Area > 0),]
item5.dat3 <- item5.dat2[which(!is.na(item5.dat2$HomeYearBuilt)),]

######################################################
# Summarise data up to unique customer level
######################################################
item5.customer <- summarise(group_by(item5.dat3, CK_Cadmus_ID)
                            ,siteAreaConditioned = sum(Conditioned.Area))

item5.merge <- left_join(rbsa.dat, item5.customer)
item5.merge <- item5.merge[which(!is.na(item5.merge$siteAreaConditioned)),]


item5.data <- weightedData(item5.merge[which(colnames(item5.merge) %notin% c("BldgLevel_Area_SqFt"
                                                                         ,"siteAreaConditioned"))])
item5.data <- left_join(item5.data, item5.merge[which(colnames(item5.merge) %in% c("CK_Cadmus_ID"
                                                                                 , "BldgLevel_Area_SqFt"
                                                                                 , "siteAreaConditioned"))])

item5.data$count <- 1
colnames(item5.data)


##############################
# Weighted Analysis
##############################
item5.final <- mean_two_groups(CustomerLevelData  = item5.data
                               , valueVariable    = 'siteAreaConditioned'
                               , byVariableRow    = 'HomeYearBuilt_bins2'
                               , byVariableColumn = 'State'
                               , columnAggregate  = "Region"
                               , rowAggregate     = "All Vintages"
                               )

item5.table <- data.frame("BuildingType"     = item5.final$BuildingType
                          ,"HousingVintage"  = item5.final$HomeYearBuilt_bins2
                          ,"Mean_ID"         = item5.final$Mean_ID
                          ,"SE_ID"           = item5.final$SE_ID
                          ,"n_ID"            = item5.final$n_ID
                          ,"Mean_MT"         = item5.final$Mean_MT
                          ,"SE_MT"           = item5.final$SE_MT
                          ,"n_MT"            = item5.final$n_MT
                          ,"Mean_OR"         = item5.final$Mean_OR
                          ,"SE_OR"           = item5.final$SE_OR
                          ,"n_OR"            = item5.final$n_OR
                          ,"Mean_WA"         = item5.final$Mean_WA
                          ,"SE_WA"           = item5.final$SE_WA
                          ,"n_WA"            = item5.final$n_WA
                          ,"Mean_Region"     = item5.final$Mean_Region
                          ,"SE_Region"       = item5.final$SE_Region
                          ,"n_Region"        = item5.final$n_Region
                          ,"EB_ID"          = item5.final$EB_ID
                          ,"EB_MT"          = item5.final$EB_MT
                          ,"EB_OR"          = item5.final$EB_OR
                          ,"EB_WA"          = item5.final$EB_WA
                          ,"EB_Region"      = item5.final$EB_Region
                          )

# row ordering example code
levels(item5.table$HousingVintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item5.table <- item5.table %>% mutate(HousingVintage = factor(HousingVintage, levels = rowOrder)) %>% arrange(HousingVintage)  
item5.table <- data.frame(item5.table)

item5.table.SF <- item5.table[which(item5.table$BuildingType %in% c("Single Family")),-1]
item5.table.MH <- item5.table[which(item5.table$BuildingType %in% c("Manufactured")),-1]

# exportTable(item5.table.SF, "SF", "Table 12", weighted = TRUE)
# exportTable(item5.table.MH, "MH", "Table 11", weighted = TRUE)



##############################
# Unweighted Analysis
##############################
item5.final <- mean_two_groups_unweighted(CustomerLevelData  = item5.data
                                          , valueVariable    = 'siteAreaConditioned'
                                          , byVariableRow    = 'HomeYearBuilt_bins2'
                                          , byVariableColumn = 'State'
                                          , columnAggregate  = "Region"
                                          , rowAggregate     = "All Vintages")

item5.table <- data.frame("BuildingType"     = item5.final$BuildingType
                          ,"HousingVintage"  = item5.final$HomeYearBuilt_bins2
                          ,"Mean_ID"         = item5.final$Mean_ID
                          ,"SE_ID"           = item5.final$SE_ID
                          ,"n_ID"            = item5.final$n_ID
                          ,"Mean_MT"         = item5.final$Mean_MT
                          ,"SE_MT"           = item5.final$SE_MT
                          ,"n_MT"            = item5.final$n_MT
                          ,"Mean_OR"         = item5.final$Mean_OR
                          ,"SE_OR"           = item5.final$SE_OR
                          ,"n_OR"            = item5.final$n_OR
                          ,"Mean_WA"         = item5.final$Mean_WA
                          ,"SE_WA"           = item5.final$SE_WA
                          ,"n_WA"            = item5.final$n_WA
                          ,"Mean_Region"     = item5.final$Mean_Region
                          ,"SE_Region"       = item5.final$SE_Region
                          ,"n_Region"        = item5.final$n_Region)

# row ordering example code
levels(item5.table$HousingVintage)
rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item5.table <- item5.table %>% mutate(HousingVintage = factor(HousingVintage, levels = rowOrder)) %>% arrange(HousingVintage)  
item5.table <- data.frame(item5.table)

item5.table.SF <- item5.table[which(item5.table$BuildingType %in% c("Single Family")),-1]
item5.table.MH <- item5.table[which(item5.table$BuildingType %in% c("Manufactured")),-1]

# exportTable(item5.table.SF, "SF", "Table 12", weighted = FALSE)
# exportTable(item5.table.MH, "MH", "Table 11", weighted = FALSE)





















# ##################################################################################################
# #
# #
# # OVERSAMPLE ANALYSES
# # 
# #
# ##################################################################################################
# # Read in clean os data
# os.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))
# length(unique(os.dat$CK_Cadmus_ID))
# os.dat$CK_Building_ID <- os.dat$Category
# os.dat <- os.dat[which(names(os.dat) != "Category")]
# 
# #############################################################################################
# # Item 3: DISTRIBUTION OF HOMES BY GROUND CONTACT TYPE AND STATE 
# #############################################################################################
# env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
#                                                             , "ENV_Construction_BLDG_STRUCTURE_FoundationType"))]
# colnames(env.dat) <- c("CK_Cadmus_ID"
#                        , "FoundationType")
# env.dat1 <- env.dat[which(!(is.na(env.dat$FoundationType))),]
# env.dat1$FoundationType <- trimws(env.dat1$FoundationType)
# 
# #merge table columns to generic columns
# item3.os.dat <- unique(left_join(os.dat, env.dat1, by = "CK_Cadmus_ID"))
# 
# # Clean Ground Contact types
# item3.os.dat$GroundContact <- item3.os.dat$FoundationType
# item3.os.dat$GroundContact <- gsub("&gt; ",">", item3.os.dat$GroundContact)
# item3.os.dat$GroundContact[grep("90% crawl", item3.os.dat$GroundContact, ignore.case = T)] <- ">90% Crawlspace"
# 
# # Remove unwanted ground contact types
# item3.os.dat1 <- item3.os.dat[which(item3.os.dat$GroundContact %notin% c("Remove", NA, 0)),]
# 
# #subset to only single family for item 3
# item3.os.dat2 <- item3.os.dat1[which(item3.os.dat1$BuildingType == "Single Family"),]
# item3.os.dat2$count <- 1
# item3.os.customer <- summarise(group_by(item3.os.dat2, CK_Cadmus_ID, CK_Building_ID, State, GroundContact)
#                             ,m_ilk = sum(count))
# item3.os.merge <- left_join(os.dat, item3.os.customer)
# item3.os.merge <- item3.os.merge[which(!is.na(item3.os.merge$m_ilk)),]
# 
# 
# #add weighting information
# item3.os.data <- weightedData(item3.os.merge[-which(colnames(item3.os.merge) %in% c("m_ilk"
#                                                                                     , "GroundContact"))])
# 
# item3.os.data <- left_join(item3.os.data, unique(item3.os.merge[which(colnames(item3.os.merge) %in% c("CK_Cadmus_ID"
#                                                                                                , "m_ilk"
#                                                                                                , "GroundContact"))]))
# 
# item3.os.data$count <- 1
# colnames(item3.os.data)
# ##############################
# # Weighted Analysis
# ##############################
# item3.os.final <- proportions_two_groups_domain(CustomerLevelData  = item3.os.data
#                                              , valueVariable       = 'm_ilk'
#                                              , byVariableColumn    = 'CK_Building_ID'
#                                              , byVariableRow       = 'GroundContact'
#                                              , aggregateColumn     = "Region"
#                                              , aggregateRow        = "Total")
# item3.os.cast <- dcast(setDT(item3.os.final)
#                     ,formula = BuildingType + GroundContact ~ CK_Building_ID
#                     ,value.var = c("w.percent", "w.SE", "n", "EB"))
# 
# if(os.ind == "scl"){
#   item3.os.table <- data.frame("BuildingType"    = item3.os.cast$BuildingType
#                                ,"GroundContact"  = item3.os.cast$GroundContact
#                                ,"Percent_SCL.GenPop"   = item3.os.cast$`w.percent_SCL GenPop`
#                                ,"SE_SCL.GenPop"        = item3.os.cast$`w.SE_SCL GenPop`
#                                ,"n_SCL.GenPop"         = item3.os.cast$`n_SCL GenPop`
#                                ,"Percent_SCL.LI"       = item3.os.cast$`w.percent_SCL LI`
#                                ,"SE_SCL.LI"            = item3.os.cast$`w.SE_SCL LI`
#                                ,"n_SCL.LI"             = item3.os.cast$`n_SCL LI`
#                                ,"Percent_SCL.EH"       = item3.os.cast$`w.percent_SCL EH`
#                                ,"SE_SCL.EH"            = item3.os.cast$`w.SE_SCL EH`
#                                ,"n_SCL.EH"             = item3.os.cast$`n_SCL EH`
#                                ,"Percent_2017.RBSA.PS" = item3.os.cast$`w.percent_2017 RBSA PS`
#                                ,"SE_2017.RBSA.PS"      = item3.os.cast$`w.SE_2017 RBSA PS`
#                                ,"n_2017.RBSA.PS"       = item3.os.cast$`n_2017 RBSA PS`
#                                ,"EB_SCL.GenPop"        = item3.os.cast$`EB_SCL GenPop`
#                                ,"EB_SCL.LI"            = item3.os.cast$`EB_SCL LI`
#                                ,"EB_SCL.EH"            = item3.os.cast$`EB_SCL EH`
#                                ,"EB_2017.RBSA.PS"      = item3.os.cast$`EB_2017 RBSA PS`)
# }else if(os.ind == "snopud"){
#   item3.os.table <- data.frame("BuildingType"    = item3.os.cast$BuildingType
#                                ,"GroundContact"  = item3.os.cast$GroundContact
#                                ,"Percent_SnoPUD"          = item3.os.cast$`w.percent_SnoPUD`
#                                ,"SE_SnoPUD"               = item3.os.cast$`w.SE_SnoPUD`
#                                ,"n_SnoPUD"                = item3.os.cast$`n_SnoPUD`
#                                ,"Percent_2017.RBSA.PS"    = item3.os.cast$`w.percent_2017 RBSA PS`
#                                ,"SE_2017.RBSA.PS"         = item3.os.cast$`w.SE_2017 RBSA PS`
#                                ,"n_2017.RBSA.PS"          = item3.os.cast$`n_2017 RBSA PS`
#                                ,"Percent_RBSA.NW"         = item3.os.cast$`w.percent_2017 RBSA NW`
#                                ,"SE_RBSA.NW"              = item3.os.cast$`w.SE_2017 RBSA NW`
#                                ,"n_RBSA.NW"               = item3.os.cast$`n_2017 RBSA NW`
#                                ,"EB_SnoPUD"               = item3.os.cast$`EB_SnoPUD`
#                                ,"EB_2017.RBSA.PS"         = item3.os.cast$`EB_2017 RBSA PS`
#                                ,"EB_RBSA.NW"              = item3.os.cast$`EB_2017 RBSA NW`)
# }
# 
# 
# item3.os.table.SF <- item3.os.table[which(item3.os.table$BuildingType == "Single Family"),-1]
# 
# exportTable(item3.os.table.SF, "SF", "Table 10", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# 
# 
# ##############################
# # Unweighted Analysis
# ##############################
# item3.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item3.os.data
#                                                  , valueVariable = 'count'
#                                                  , columnVariable = 'CK_Building_ID'
#                                                  , rowVariable = 'GroundContact'
#                                                  , aggregateColumnName = "Region")
# 
# item3.os.cast <- dcast(setDT(item3.os.final)
#                     ,formula = BuildingType + GroundContact ~ CK_Building_ID
#                     ,value.var = c("Percent", "SE", "Count", "n"))
# 
# if(os.ind == "scl"){
#   item3.os.table <- data.frame("BuildingType"    = item3.os.cast$BuildingType
#                                ,"GroundContact"  = item3.os.cast$GroundContact
#                                ,"Percent_SCL.GenPop"   = item3.os.cast$`Percent_SCL GenPop`
#                                ,"SE_SCL.GenPop"        = item3.os.cast$`SE_SCL GenPop`
#                                ,"n_SCL.GenPop"         = item3.os.cast$`n_SCL GenPop`
#                                ,"Percent_SCL.LI"       = item3.os.cast$`Percent_SCL LI`
#                                ,"SE_SCL.LI"            = item3.os.cast$`SE_SCL LI`
#                                ,"n_SCL.LI"             = item3.os.cast$`n_SCL LI`
#                                ,"Percent_SCL.EH"       = item3.os.cast$`Percent_SCL EH`
#                                ,"SE_SCL.EH"            = item3.os.cast$`SE_SCL EH`
#                                ,"n_SCL.EH"             = item3.os.cast$`n_SCL EH`
#                                ,"Percent_2017.RBSA.PS" = item3.os.cast$`Percent_2017 RBSA PS`
#                                ,"SE_2017.RBSA.PS"      = item3.os.cast$`SE_2017 RBSA PS`
#                                ,"n_2017.RBSA.PS"       = item3.os.cast$`n_2017 RBSA PS`)
# }else if(os.ind == "snopud"){
#   item3.os.table <- data.frame("BuildingType"    = item3.os.cast$BuildingType
#                                ,"GroundContact"  = item3.os.cast$GroundContact
#                                ,"Percent_SnoPUD"          = item3.os.cast$`Percent_SnoPUD`
#                                ,"SE_SnoPUD"               = item3.os.cast$`SE_SnoPUD`
#                                ,"n_SnoPUD"                = item3.os.cast$`n_SnoPUD`
#                                ,"Percent_2017.RBSA.PS"    = item3.os.cast$`Percent_2017 RBSA PS`
#                                ,"SE_2017.RBSA.PS"         = item3.os.cast$`SE_2017 RBSA PS`
#                                ,"n_2017.RBSA.PS"          = item3.os.cast$`n_2017 RBSA PS`
#                                ,"Percent_RBSA.NW"         = item3.os.cast$`Percent_2017 RBSA NW`
#                                ,"SE_RBSA.NW"              = item3.os.cast$`SE_2017 RBSA NW`
#                                ,"n_RBSA.NW"               = item3.os.cast$`n_2017 RBSA NW`)
# }
# 
# 
# item3.os.table.SF <- item3.os.table[which(item3.os.table$BuildingType == "Single Family"),-1]
# 
# exportTable(item3.os.table.SF, "SF", "Table 10", weighted = FALSE, osIndicator = export.ind, OS = T)
# 
# 
# 
# 
# #############################################################################################
# # Item 4: AVERAGE CONDITIONED FLOOR AREA BY CK_Building_ID
# #############################################################################################
# env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
#                                                             , "Conditioned.Living.Area"))]
# colnames(env.dat) <- c("CK_Cadmus_ID"
#                        , "BldgLevel_Area_SqFt")
# 
# #make conditioned area as.numeric
# env.dat$ConditionedArea <- as.numeric(as.character(env.dat$BldgLevel_Area_SqFt))
# 
# #merge
# item4.os.dat <- left_join(os.dat, env.dat, by = "CK_Cadmus_ID")
# length(unique(item4.os.dat$CK_Cadmus_ID))
# 
# item4.os.dat1 <- item4.os.dat[which(item4.os.dat$BuildingType != "Multifamily"),]
# item4.os.dat2 <- item4.os.dat1[which(!is.na(item4.os.dat1$ConditionedArea)),]
# 
# ######################################################
# # Summarise data up to unique customer level
# ######################################################
# item4.os.customer <- summarise(group_by(item4.os.dat2, CK_Cadmus_ID, CK_Building_ID)
#                             ,siteAreaConditioned = sum(ConditionedArea))
# 
# item4.os.merge <- left_join(os.dat, item4.os.customer)
# item4.os.merge <- item4.os.merge[which(!is.na(item4.os.merge$siteAreaConditioned)),]
# 
# 
# item4.os.data <- weightedData(item4.os.merge[-which(colnames(item4.os.merge) %in% c("BldgLevel_Area_SqFt"
#                                                                            ,"siteAreaConditioned"))])
# item4.os.data <- left_join(item4.os.data, unique(item4.os.merge[which(colnames(item4.os.merge) %in% c("CK_Cadmus_ID"
#                                                                                    ,"BldgLevel_Area_SqFt"
#                                                                                    , "siteAreaConditioned"))]))
# 
# item4.os.data$count <- 1
# colnames(item4.os.data)
# 
# ##############################
# # Weighted Analysis
# ##############################
# item4.os.final <- mean_one_group(CustomerLevelData = item4.os.data
#                               , valueVariable = 'siteAreaConditioned'
#                               , byVariable    = 'CK_Building_ID'
#                               , aggregateRow  = 'Region')
# item4.os.final <- data.frame(item4.os.final[which(item4.os.final$CK_Building_ID != "Region"),], stringsAsFactors = F)
# # row ordering example code
# unique(item4.os.final$CK_Building_ID)
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
# item4.os.final <- item4.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)  
# item4.os.final <- data.frame(item4.os.final)
# 
# 
# item4.os.table.SF <- item4.os.final[which(item4.os.final$BuildingType %in% c("Single Family")),-1]
# 
# exportTable(item4.os.table.SF, "SF", "Table 11", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# ##############################
# # Unweighted Analysis
# ##############################
# item4.os.final <- mean_one_group_unweighted(CustomerLevelData = item4.os.data
#                                          , valueVariable = 'siteAreaConditioned'
#                                          , byVariable    = 'CK_Building_ID'
#                                          , aggregateRow  = 'Region')
# 
# item4.os.final <- data.frame(item4.os.final[which(item4.os.final$CK_Building_ID != "Region"),], stringsAsFactors = F)
# # row ordering example code
# unique(item4.os.final$CK_Building_ID)
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
# item4.os.final <- item4.os.final %>% mutate(CK_Building_ID = factor(CK_Building_ID, levels = rowOrder)) %>% arrange(CK_Building_ID)  
# item4.os.final <- data.frame(item4.os.final)
# 
# item4.os.table.SF <- item4.os.final[which(item4.os.final$BuildingType %in% c("Single Family")),-1]
# 
# exportTable(item4.os.table.SF, "SF", "Table 11", weighted = FALSE, osIndicator = export.ind, OS = T)
# 
# 
# 
# 
# ##########################################################################
# # Item 5: AVERAGE CONDITIONED FLOOR AREA BY CK_Building_ID AND VINTAGE
# ##########################################################################
# item5.os.dat <- os.dat
# length(unique(item5.os.dat$CK_Cadmus_ID))
# 
# item5.os.dat1 <- item5.os.dat[which(item5.os.dat$BuildingType != "Multifamily"),]
# item5.os.dat2 <- item5.os.dat1[which(!is.na(item5.os.dat1$Conditioned.Area)),]
# item5.os.dat2 <- item5.os.dat2[which(item5.os.dat2$Conditioned.Area > 0),]
# item5.os.dat3 <- item5.os.dat2[which(!is.na(item5.os.dat2$HomeYearBuilt)),]
# 
# ######################################################
# # Summarise data up to unique customer level
# ######################################################
# item5.os.customer <- summarise(group_by(item5.os.dat3, CK_Cadmus_ID, CK_Building_ID)
#                             ,siteAreaConditioned = sum(Conditioned.Area))
# 
# item5.os.merge <- left_join(os.dat, item5.os.customer)
# item5.os.merge <- item5.os.merge[which(!is.na(item5.os.merge$siteAreaConditioned)),]
# 
# 
# item5.os.data <- weightedData(item5.os.merge[which(colnames(item5.os.merge) %notin% c("BldgLevel_Area_SqFt"
#                                                                              ,"siteAreaConditioned"))])
# item5.os.data <- left_join(item5.os.data, item5.os.merge[which(colnames(item5.os.merge) %in% c("CK_Cadmus_ID"
#                                                                                    , "BldgLevel_Area_SqFt"
#                                                                                    , "siteAreaConditioned"))])
# 
# item5.os.data$count <- 1
# colnames(item5.os.data)
# 
# 
# ##############################
# # Weighted Analysis
# ##############################
# item5.os.cast <- mean_two_groups(CustomerLevelData  = item5.os.data
#                                , valueVariable    = 'siteAreaConditioned'
#                                , byVariableRow    = 'HomeYearBuilt_bins2'
#                                , byVariableColumn = 'CK_Building_ID'
#                                , columnAggregate  = "Region"
#                                , rowAggregate     = "All Vintages"
# )
# if(os.ind == "scl"){
#   item5.os.table <- data.frame("BuildingType"     = item5.os.cast$BuildingType
#                                ,"HousingVintage"     = item5.os.cast$HomeYearBuilt_bins2
#                                ,"Mean_SCL.GenPop"    = item5.os.cast$`Mean_SCL GenPop`
#                                ,"SE_SCL.GenPop"      = item5.os.cast$`SE_SCL GenPop`
#                                ,"n_SCL.GenPop"       = item5.os.cast$`n_SCL GenPop`
#                                ,"Mean_SCL.LI"        = item5.os.cast$`Mean_SCL LI`
#                                ,"SE_SCL.LI"          = item5.os.cast$`SE_SCL LI`
#                                ,"n_SCL.LI"           = item5.os.cast$`n_SCL LI`
#                                ,"Mean_SCL.EH"        = item5.os.cast$`Mean_SCL EH`
#                                ,"SE_SCL.EH"          = item5.os.cast$`SE_SCL EH`
#                                ,"n_SCL.EH"           = item5.os.cast$`n_SCL EH`
#                                ,"Mean_2017.RBSA.PS"  = item5.os.cast$`Mean_2017 RBSA PS`
#                                ,"SE_2017.RBSA.PS"    = item5.os.cast$`SE_2017 RBSA PS`
#                                ,"n_2017.RBSA.PS"     = item5.os.cast$`n_2017 RBSA PS`
#                                ,"EB_SCL.GenPop"      = item5.os.cast$`EB_SCL GenPop`
#                                ,"EB_SCL.LI"          = item5.os.cast$`EB_SCL LI`
#                                ,"EB_SCL.EH"          = item5.os.cast$`EB_SCL EH`
#                                ,"EB_2017.RBSA.PS"    = item5.os.cast$`EB_2017 RBSA PS`
#   )
# }else if(os.ind == "snopud"){
#   item5.os.table <- data.frame("BuildingType"     = item5.os.cast$BuildingType
#                                ,"HousingVintage"     = item5.os.cast$HomeYearBuilt_bins2
#                                ,"Mean_SnoPUD"          = item5.os.cast$`Mean_SnoPUD`
#                                ,"SE_SnoPUD"            = item5.os.cast$`SE_SnoPUD`
#                                ,"n_SnoPUD"             = item5.os.cast$`n_SnoPUD`
#                                ,"Mean_2017.RBSA.PS"    = item5.os.cast$`Mean_2017 RBSA PS`
#                                ,"SE_2017.RBSA.PS"      = item5.os.cast$`SE_2017 RBSA PS`
#                                ,"n_2017.RBSA.PS"       = item5.os.cast$`n_2017 RBSA PS`
#                                ,"Mean_RBSA.NW"         = item5.os.cast$`Mean_2017 RBSA NW`
#                                ,"SE_RBSA.NW"           = item5.os.cast$`SE_2017 RBSA NW`
#                                ,"n_RBSA.NW"            = item5.os.cast$`n_2017 RBSA NW`
#                                ,"EB_SnoPUD"            = item5.os.cast$`EB_SnoPUD`
#                                ,"EB_2017.RBSA.PS"      = item5.os.cast$`EB_2017 RBSA PS`
#                                ,"EB_RBSA.NW"           = item5.os.cast$`EB_2017 RBSA NW`
#   )
# }
# 
# 
# # row ordering example code
# levels(item5.os.table$HousingVintage)
# rowOrder <- c("Pre 1951"
#               ,"1951-1960"
#               ,"1961-1970"
#               ,"1971-1980"
#               ,"1981-1990"
#               ,"1991-2000"
#               ,"2001-2010"
#               ,"Post 2010"
#               ,"All Vintages")
# item5.os.table <- item5.os.table %>% mutate(HousingVintage = factor(HousingVintage, levels = rowOrder)) %>% arrange(HousingVintage)  
# item5.os.table <- data.frame(item5.os.table)
# 
# item5.os.table.SF <- item5.os.table[which(item5.os.table$BuildingType %in% c("Single Family")),-1]
# 
# exportTable(item5.os.table.SF, "SF", "Table 12", weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# 
# 
# ##############################
# # Unweighted Analysis
# ##############################
# item5.os.final <- mean_two_groups_unweighted(CustomerLevelData  = item5.os.data
#                                           , valueVariable    = 'siteAreaConditioned'
#                                           , byVariableRow    = 'HomeYearBuilt_bins2'
#                                           , byVariableColumn = 'CK_Building_ID'
#                                           , columnAggregate  = "Region"
#                                           , rowAggregate     = "All Vintages")
# 
# if(os.ind == "scl"){
#   item5.os.table <- data.frame("BuildingType"     = item5.os.cast$BuildingType
#                                ,"HousingVintage"     = item5.os.cast$HomeYearBuilt_bins2
#                                ,"Mean_SCL.GenPop"    = item5.os.cast$`Mean_SCL GenPop`
#                                ,"SE_SCL.GenPop"      = item5.os.cast$`SE_SCL GenPop`
#                                ,"n_SCL.GenPop"       = item5.os.cast$`n_SCL GenPop`
#                                ,"Mean_SCL.LI"        = item5.os.cast$`Mean_SCL LI`
#                                ,"SE_SCL.LI"          = item5.os.cast$`SE_SCL LI`
#                                ,"n_SCL.LI"           = item5.os.cast$`n_SCL LI`
#                                ,"Mean_SCL.EH"        = item5.os.cast$`Mean_SCL EH`
#                                ,"SE_SCL.EH"          = item5.os.cast$`SE_SCL EH`
#                                ,"n_SCL.EH"           = item5.os.cast$`n_SCL EH`
#                                ,"Mean_2017.RBSA.PS"  = item5.os.cast$`Mean_2017 RBSA PS`
#                                ,"SE_2017.RBSA.PS"    = item5.os.cast$`SE_2017 RBSA PS`
#                                ,"n_2017.RBSA.PS"     = item5.os.cast$`n_2017 RBSA PS`
#   )
# }else if(os.ind == "snopud"){
#   item5.os.table <- data.frame("BuildingType"     = item5.os.cast$BuildingType
#                                ,"HousingVintage"     = item5.os.cast$HomeYearBuilt_bins2
#                                ,"Mean_SnoPUD"          = item5.os.cast$`Mean_SnoPUD`
#                                ,"SE_SnoPUD"            = item5.os.cast$`SE_SnoPUD`
#                                ,"n_SnoPUD"             = item5.os.cast$`n_SnoPUD`
#                                ,"Mean_2017.RBSA.PS"    = item5.os.cast$`Mean_2017 RBSA PS`
#                                ,"SE_2017.RBSA.PS"      = item5.os.cast$`SE_2017 RBSA PS`
#                                ,"n_2017.RBSA.PS"       = item5.os.cast$`n_2017 RBSA PS`
#                                ,"Mean_RBSA.NW"         = item5.os.cast$`Mean_2017 RBSA NW`
#                                ,"SE_RBSA.NW"           = item5.os.cast$`SE_2017 RBSA NW`
#                                ,"n_RBSA.NW"            = item5.os.cast$`n_2017 RBSA NW`
#   )
# }
# # row ordering example code
# levels(item5.os.table$HousingVintage)
# rowOrder <- c("Pre 1951"
#               ,"1951-1960"
#               ,"1961-1970"
#               ,"1971-1980"
#               ,"1981-1990"
#               ,"1991-2000"
#               ,"2001-2010"
#               ,"Post 2010"
#               ,"All Vintages")
# item5.os.table <- item5.os.table %>% mutate(HousingVintage = factor(HousingVintage, levels = rowOrder)) %>% arrange(HousingVintage)  
# item5.os.table <- data.frame(item5.os.table)
# 
# item5.os.table.SF <- item5.os.table[which(item5.os.table$BuildingType %in% c("Single Family")),-1]
# 
# exportTable(item5.os.table.SF, "SF", "Table 12", weighted = FALSE, osIndicator = export.ind, OS = T)
