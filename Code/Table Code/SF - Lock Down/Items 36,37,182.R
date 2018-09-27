#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:          09/11/2017                                  
##  Billing Code(s):  6596.0000.0002.1002.0000
#############################################################################################

##  Clear variables
# rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

##  Create "Not In" operator
"%notin%" <- Negate("%in%")

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) 

# one.line.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.export), sheet = "Site One Line Summary", startRow = 2)
one.line.dat$CK_Cadmus_ID <- trimws(toupper(one.line.dat$Cadmus.ID))

#############################################################################################
#Item 36: AVERAGE NORMALIZED HEAT-LOSS RATE BY VINTAGE AND STATE (SF table 43, MH table 24)
#############################################################################################
item36.dat <- one.line.dat[which(colnames(one.line.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Whole.House.UA"))]
item36.dat1 <- left_join(rbsa.dat, item36.dat)
item36.dat1 <- item36.dat1[grep("SITE",item36.dat1$CK_Building_ID),]
item36.dat1$Whole.House.UA <- as.numeric(as.character(item36.dat1$Whole.House.UA))
item36.dat2 <- item36.dat1[which(!is.na(item36.dat1$Whole.House.UA)),]
item36.dat3 <- item36.dat2[grep("site",item36.dat2$CK_Building_ID, ignore.case = T),]
which(duplicated(item36.dat3$CK_Cadmus_ID))
item36.dat4 <- item36.dat3[which(item36.dat3$Conditioned.Area > 0),]


item36.dat4$Normalized.Heat.Loss.Rate <- item36.dat4$Whole.House.UA / item36.dat4$Conditioned.Area

item36.dat5 <- item36.dat4[which(!is.na(item36.dat4$HomeYearBuilt_bins3)),]

################################################
# Adding pop and sample sizes for weights
################################################
item36.data <- weightedData(item36.dat5[-which(colnames(item36.dat5) %in% c("Whole.House.UA"
                                                                            ,"Normalized.Heat.Loss.Rate"))])
item36.data <- left_join(item36.data, item36.dat5[which(colnames(item36.dat5) %in% c("CK_Cadmus_ID"
                                                                                     ,"Whole.House.UA"
                                                                                     ,"Normalized.Heat.Loss.Rate"))])
item36.data$count <- 1
which(duplicated(item36.data$CK_Cadmus_ID))

#######################
# Weighted Analysis
#######################
item36.cast <- mean_two_groups(CustomerLevelData = item36.data
                               ,valueVariable = "Normalized.Heat.Loss.Rate"
                               ,byVariableRow = "HomeYearBuilt_bins3"
                               ,byVariableColumn = "State"
                               ,columnAggregate = "Region"
                               ,rowAggregate = "All Vintages")

item36.table <- data.frame("BuildingType"     = item36.cast$BuildingType
                           ,"Housing.Vintage" = item36.cast$HomeYearBuilt_bins3
                           ,"ID"              = item36.cast$Mean_ID
                           ,"ID.SE"           = item36.cast$SE_ID
                           ,"ID.n"            = item36.cast$n_ID
                           ,"MT"              = item36.cast$Mean_MT
                           ,"MT.SE"           = item36.cast$SE_MT
                           ,"MT.n"            = item36.cast$n_MT
                           ,"OR"              = item36.cast$Mean_OR
                           ,"OR.SE"           = item36.cast$SE_OR
                           ,"OR.n"            = item36.cast$n_OR
                           ,"WA"              = item36.cast$Mean_WA
                           ,"WA.SE"           = item36.cast$SE_WA
                           ,"WA.n"            = item36.cast$n_WA
                           ,"Region"          = item36.cast$Mean_Region
                           ,"Region.SE"       = item36.cast$SE_Region
                           ,"Region.n"        = item36.cast$n_Region
                           ,"ID.EB"           = item36.cast$EB_ID
                           ,"MT.EB"           = item36.cast$EB_MT
                           ,"OR.EB"           = item36.cast$EB_OR
                           ,"WA.EB"           = item36.cast$EB_WA
                           ,"Region.EB"       = item36.cast$EB_Region)

levels(item36.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item36.table <- item36.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item36.table <- data.frame(item36.table)


item36.table.SF <- item36.table[which(item36.table$BuildingType == "Single Family"),
                                -which(colnames(item36.table) == "BuildingType")]
item36.table.MH <- item36.table[which(item36.table$BuildingType == "Manufactured"),
                                -which(colnames(item36.table) == "BuildingType")]

# exportTable(item36.table.SF, "SF","Table 43",weighted = TRUE)
# # exportTable(item36.table.MH, "MH","Table 24",weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item36.cast <- mean_two_groups_unweighted(CustomerLevelData = item36.data
                               ,valueVariable = "Normalized.Heat.Loss.Rate"
                               ,byVariableRow = "HomeYearBuilt_bins3"
                               ,byVariableColumn = "State"
                               ,columnAggregate = "Region"
                               ,rowAggregate = "All Vintages")

item36.table <- data.frame("BuildingType"     = item36.cast$BuildingType
                           ,"Housing.Vintage" = item36.cast$HomeYearBuilt_bins3
                           ,"ID"              = item36.cast$Mean_ID
                           ,"ID.SE"           = item36.cast$SE_ID
                           ,"ID.n"            = item36.cast$n_ID
                           ,"MT"              = item36.cast$Mean_MT
                           ,"MT.SE"           = item36.cast$SE_MT
                           ,"MT.n"            = item36.cast$n_MT
                           ,"OR"              = item36.cast$Mean_OR
                           ,"OR.SE"           = item36.cast$SE_OR
                           ,"OR.n"            = item36.cast$n_OR
                           ,"WA"              = item36.cast$Mean_WA
                           ,"WA.SE"           = item36.cast$SE_WA
                           ,"WA.n"            = item36.cast$n_WA
                           ,"Region"          = item36.cast$Mean_Region
                           ,"Region.SE"       = item36.cast$SE_Region
                           ,"Region.n"        = item36.cast$n_Region)

levels(item36.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item36.table <- item36.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item36.table <- data.frame(item36.table)


item36.table.SF <- item36.table[which(item36.table$BuildingType == "Single Family"),
                                -which(colnames(item36.table) == "BuildingType")]
item36.table.MH <- item36.table[which(item36.table$BuildingType == "Manufactured"),
                                -which(colnames(item36.table) == "BuildingType")]

# exportTable(item36.table.SF, "SF","Table 43",weighted = FALSE)
# # exportTable(item36.table.MH, "MH","Table 24",weighted = FALSE)






#############################################################################################
#Item 37: AVERAGE HEAT-LOSS RATE BY VINTAGE AND STATE (SF table 44, MH table 26)
#############################################################################################
item37.dat <- one.line.dat[which(colnames(one.line.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Whole.House.UA"))]
item37.dat1 <- left_join(rbsa.dat, item37.dat)
item37.dat1$Whole.House.UA <- as.numeric(as.character(item37.dat1$Whole.House.UA))
item37.dat2 <- item37.dat1[which(item37.dat1$Whole.House.UA %notin% c("N/A",NA)),]
item37.dat3 <- item37.dat2[grep("site",item37.dat2$CK_Building_ID, ignore.case = T),]
which(duplicated(item37.dat3$CK_Cadmus_ID))

item37.dat4 <-  item37.dat3[which(item37.dat3$HomeYearBuilt_bins3  %notin% c("N/A",NA)),]

################################################
# Adding pop and sample sizes for weights
################################################
item37.data <- weightedData(item37.dat4[-which(colnames(item37.dat4) %in% c("Whole.House.UA"))])
item37.data <- left_join(item37.data, item37.dat4[which(colnames(item37.dat4) %in% c("CK_Cadmus_ID"
                                                                                     ,"Whole.House.UA"))])
item37.data$count <- 1
#######################
# Weighted Analysis
#######################
item37.cast <- mean_two_groups(CustomerLevelData = item37.data
                               ,valueVariable    = "Whole.House.UA"
                               ,byVariableRow    = "HomeYearBuilt_bins3"
                               ,byVariableColumn = "State"
                               ,columnAggregate  = "Region"
                               ,rowAggregate     = "All Vintages")

item37.table <- data.frame("BuildingType"     = item37.cast$BuildingType
                           ,"Housing.Vintage" = item37.cast$HomeYearBuilt_bins3
                           ,"ID"              = item37.cast$Mean_ID
                           ,"ID.SE"           = item37.cast$SE_ID
                           ,"ID.n"            = item37.cast$n_ID
                           ,"MT"              = item37.cast$Mean_MT
                           ,"MT.SE"           = item37.cast$SE_MT
                           ,"MT.n"            = item37.cast$n_MT
                           ,"OR"              = item37.cast$Mean_OR
                           ,"OR.SE"           = item37.cast$SE_OR
                           ,"OR.n"            = item37.cast$n_OR
                           ,"WA"              = item37.cast$Mean_WA
                           ,"WA.SE"           = item37.cast$SE_WA
                           ,"WA.n"            = item37.cast$n_WA
                           ,"Region"          = item37.cast$Mean_Region
                           ,"Region.SE"       = item37.cast$SE_Region
                           ,"Region.n"        = item37.cast$n_Region
                           ,"ID.EB"           = item37.cast$EB_ID
                           ,"MT.EB"           = item37.cast$EB_MT
                           ,"OR.EB"           = item37.cast$EB_OR
                           ,"WA.EB"           = item37.cast$EB_WA
                           ,"Region.EB"       = item37.cast$EB_Region)

levels(item37.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item37.table <- item37.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item37.table <- data.frame(item37.table)


item37.table.SF <- item37.table[which(item37.table$BuildingType == "Single Family"),
                                -which(colnames(item37.table) == "BuildingType")]
item37.table.MH <- item37.table[which(item37.table$BuildingType == "Manufactured"),
                                -which(colnames(item37.table) == "BuildingType")]

# exportTable(item37.table.SF, "SF","Table 44",weighted = TRUE)
# # exportTable(item37.table.MH, "MH","Table 26",weighted = TRUE)

#######################
# Unweighted Analysis
#######################
item37.cast <- mean_two_groups_unweighted(CustomerLevelData = item37.data
                                          ,valueVariable    = "Whole.House.UA"
                                          ,byVariableRow    = "HomeYearBuilt_bins3"
                                          ,byVariableColumn = "State"
                                          ,columnAggregate  = "Region"
                                          ,rowAggregate     = "All Vintages")

item37.table <- data.frame("BuildingType"     = item37.cast$BuildingType
                           ,"Housing.Vintage" = item37.cast$HomeYearBuilt_bins3
                           ,"ID"              = item37.cast$Mean_ID
                           ,"ID.SE"           = item37.cast$SE_ID
                           ,"ID.n"            = item37.cast$n_ID
                           ,"MT"              = item37.cast$Mean_MT
                           ,"MT.SE"           = item37.cast$SE_MT
                           ,"MT.n"            = item37.cast$n_MT
                           ,"OR"              = item37.cast$Mean_OR
                           ,"OR.SE"           = item37.cast$SE_OR
                           ,"OR.n"            = item37.cast$n_OR
                           ,"WA"              = item37.cast$Mean_WA
                           ,"WA.SE"           = item37.cast$SE_WA
                           ,"WA.n"            = item37.cast$n_WA
                           ,"Region"          = item37.cast$Mean_Region
                           ,"Region.SE"       = item37.cast$SE_Region
                           ,"Region.n"        = item37.cast$n_Region)

levels(item37.table$Housing.Vintage)
rowOrder <- c("Pre 1981"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item37.table <- item37.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
item37.table <- data.frame(item37.table)


item37.table.SF <- item37.table[which(item37.table$BuildingType == "Single Family"),
                                -which(colnames(item37.table) == "BuildingType")]
item37.table.MH <- item37.table[which(item37.table$BuildingType == "Manufactured"),
                                -which(colnames(item37.table) == "BuildingType")]

# exportTable(item37.table.SF, "SF","Table 44",weighted = FALSE)
# # exportTable(item37.table.MH, "MH","Table 26",weighted = FALSE)










# #############################################################################################
# #Item 182: AVERAGE HEAT-LOSS RATE BY AGE/STANDARD AND STATE (MH table 25)
# #############################################################################################
# #Read in data for analysis
# sites.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.export))
# #clean cadmus IDs
# sites.dat$CK_Cadmus_ID <- trimws(toupper(sites.dat$CK_Cadmus_ID))
# 
# item182.sites <- unique(sites.dat[which(colnames(sites.dat) %in% c("CK_Cadmus_ID"
#                                                                    ,"SITE_Construction_CONSTRUCTION_STANDARD_AgeAndConstructionStandard"))])
# names(item182.sites) <- c("CK_Cadmus_ID", "Age.and.Construction.Standard")
# item182.dat <- one.line.dat[which(colnames(one.line.dat) %in% c("CK_Cadmus_ID"
#                                                                ,"Whole.House.UA"))]
# item182.dat0 <- left_join(item182.sites, item182.dat)
# item182.dat1 <- left_join(rbsa.dat, item182.dat0)
# item182.dat1$Whole.House.UA <- as.numeric(as.character(item182.dat1$Whole.House.UA))
# item182.dat2 <- item182.dat1[which(item182.dat1$Whole.House.UA %notin% c("N/A",NA)),]
# item182.dat3 <- item182.dat2[grep("site",item182.dat2$CK_Building_ID, ignore.case = T),]
# which(duplicated(item182.dat3$CK_Cadmus_ID))
# 
# item182.dat4 <-  item182.dat3[which(item182.dat3$Age.and.Construction.Standard %notin% c("Unknown","unknown","N/A","1977")),]
# unique(item182.dat4$Age.and.Construction.Standard)
# item182.dat4$Normalized.Heat.Loss.Rate <- item182.dat4$Whole.House.UA / item182.dat4$Conditioned.Area
# item182.dat5 <- item182.dat4[which(!is.na(item182.dat4$Normalized.Heat.Loss.Rate)),]
# 
# 
# unique(item182.dat5$Age.and.Construction.Standard)
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item182.data <- weightedData(item182.dat5[-which(colnames(item182.dat5) %in% c("Whole.House.UA"
#                                                                                ,"Age.and.Construction.Standard"
#                                                                                ,"Normalized.Heat.Loss.Rate"))])
# item182.data <- left_join(item182.data, item182.dat5[which(colnames(item182.dat5) %in% c("CK_Cadmus_ID"
#                                                                                      ,"Whole.House.UA"
#                                                                                      ,"Age.and.Construction.Standard"
#                                                                                      ,"Normalized.Heat.Loss.Rate"))])
# item182.data$count <- 1
# #######################
# # Weighted Analysis
# #######################
# item182.summary <- mean_two_groups(CustomerLevelData = item182.data
#                                ,valueVariable    = "Normalized.Heat.Loss.Rate"
#                                ,byVariableRow    = "Age.and.Construction.Standard"
#                                ,byVariableColumn = "State"
#                                ,columnAggregate  = "Region"
#                                ,rowAggregate     = "All Age/Standards")
# # item182.all.ages.standards <- mean_one_group(CustomerLevelData = item182.data
# #                                              ,valueVariable = "Normalized.Heat.Loss.Rate"
# #                                              ,byVariable = "State"
# #                                              ,aggregateRow = "Region")
# # item182.all.ages.standards <- dcast(setDT(item182.all.ages.standards)
# #                                     ,formula = BuildingType ~ State
# #                                     ,value.var = c("Mean","SE","n","EB"))
# # item182.all.ages.standards$Age.and.Construction.Standard <- "All Age/Standards"
# 
# item182.cast <- item182.summary#rbind.data.frame(item182.summary, item182.all.ages.standards, stringsAsFactors = F)
# 
# item182.table <- data.frame("BuildingType"     = item182.cast$BuildingType
#                            ,"Age.Construction.Standard" = item182.cast$Age.and.Construction.Standard
#                            ,"ID"              = item182.cast$Mean_ID
#                            ,"ID.SE"           = item182.cast$SE_ID
#                            ,"ID.n"            = item182.cast$n_ID
#                            ,"MT"              = item182.cast$Mean_MT
#                            ,"MT.SE"           = item182.cast$SE_MT
#                            ,"MT.n"            = item182.cast$n_MT
#                            ,"OR"              = item182.cast$Mean_OR
#                            ,"OR.SE"           = item182.cast$SE_OR
#                            ,"OR.n"            = item182.cast$n_OR
#                            ,"WA"              = item182.cast$Mean_WA
#                            ,"WA.SE"           = item182.cast$SE_WA
#                            ,"WA.n"            = item182.cast$n_WA
#                            ,"Region"          = item182.cast$Mean_Region
#                            ,"Region.SE"       = item182.cast$SE_Region
#                            ,"Region.n"        = item182.cast$n_Region
#                            ,"ID.EB"           = item182.cast$EB_ID
#                            ,"MT.EB"           = item182.cast$EB_MT
#                            ,"OR.EB"           = item182.cast$EB_OR
#                            ,"WA.EB"           = item182.cast$EB_WA
#                            ,"Region.EB"       = item182.cast$EB_Region)
# 
# item182.table.MH <- item182.table[which(item182.table$BuildingType == "Manufactured"),
#                                 -which(colnames(item182.table) == "BuildingType")]
# 
# # exportTable(item182.table.MH, "MH","Table 25",weighted = TRUE)
# 
# #######################
# # Unweighted Analysis
# #######################
# item182.summary <- mean_two_groups_unweighted(CustomerLevelData = item182.data
#                                           ,valueVariable    = "Normalized.Heat.Loss.Rate"
#                                           ,byVariableRow    = "Age.and.Construction.Standard"
#                                           ,byVariableColumn = "State"
#                                           ,columnAggregate  = "Region"
#                                           ,rowAggregate     = "All Age/Standards")
# 
# item182.cast <- item182.summary
# 
# item182.table <- data.frame("BuildingType"     = item182.cast$BuildingType
#                             ,"Age.Construction.Standard" = item182.cast$Age.and.Construction.Standard
#                            ,"ID"              = item182.cast$Mean_ID
#                            ,"ID.SE"           = item182.cast$SE_ID
#                            ,"ID.n"            = item182.cast$n_ID
#                            ,"MT"              = item182.cast$Mean_MT
#                            ,"MT.SE"           = item182.cast$SE_MT
#                            ,"MT.n"            = item182.cast$n_MT
#                            ,"OR"              = item182.cast$Mean_OR
#                            ,"OR.SE"           = item182.cast$SE_OR
#                            ,"OR.n"            = item182.cast$n_OR
#                            ,"WA"              = item182.cast$Mean_WA
#                            ,"WA.SE"           = item182.cast$SE_WA
#                            ,"WA.n"            = item182.cast$n_WA
#                            ,"Region"          = item182.cast$Mean_Region
#                            ,"Region.SE"       = item182.cast$SE_Region
#                            ,"Region.n"        = item182.cast$n_Region)
# 
# item182.table.MH <- item182.table[which(item182.table$BuildingType == "Manufactured"),
#                                 -which(colnames(item182.table) == "BuildingType")]
# 
# # exportTable(item182.table.MH, "MH","Table 25",weighted = FALSE)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
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
# #Item 36: AVERAGE NORMALIZED HEAT-LOSS RATE BY VINTAGE AND CK_Building_ID (SF table 43, MH table 24)
# #############################################################################################
# item36.os.dat <- one.line.dat[which(colnames(one.line.dat) %in% c("CK_Cadmus_ID"
#                                                                ,"Whole.House.UA"))]
# item36.os.dat1 <- left_join(os.dat, item36.os.dat)
# item36.os.dat1$Whole.House.UA <- as.numeric(as.character(item36.os.dat1$Whole.House.UA))
# 
# item36.os.dat2 <- item36.os.dat1[which(!is.na(item36.os.dat1$Whole.House.UA)),]
# 
# item36.os.dat4 <- item36.os.dat2[which(item36.os.dat2$Conditioned.Area > 0),]
# item36.os.dat4$Normalized.Heat.Loss.Rate <- item36.os.dat4$Whole.House.UA / item36.os.dat4$Conditioned.Area
# 
# item36.os.dat5 <- item36.os.dat4[which(!is.na(item36.os.dat4$HomeYearBuilt_bins3)),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item36.os.data <- weightedData(item36.os.dat5[-which(colnames(item36.os.dat5) %in% c("Whole.House.UA"
#                                                                             ,"Normalized.Heat.Loss.Rate"))])
# item36.os.data <- left_join(item36.os.data, item36.os.dat5[which(colnames(item36.os.dat5) %in% c("CK_Cadmus_ID"
#                                                                                      ,"Whole.House.UA"
#                                                                                      ,"Normalized.Heat.Loss.Rate"))])
# item36.os.data$count <- 1
# 
# #######################
# # Weighted Analysis
# #######################
# item36.os.cast <- mean_two_groups(CustomerLevelData = item36.os.data
#                                ,valueVariable = "Normalized.Heat.Loss.Rate"
#                                ,byVariableRow = "HomeYearBuilt_bins3"
#                                ,byVariableColumn = "CK_Building_ID"
#                                ,columnAggregate = "Remove"
#                                ,rowAggregate = "All Vintages")
# 
# names(item36.os.cast)
# 
# if(os.ind == "scl"){
#   item36.os.table <- data.frame("Housing.Vintage" = item36.os.cast$HomeYearBuilt_bins3
#                                 ,"Mean_SCL.GenPop"      = item36.os.cast$`Mean_SCL GenPop`
#                                 ,"SE_SCL.GenPop"        = item36.os.cast$`SE_SCL GenPop`
#                                 ,"n_SCL.GenPop"         = item36.os.cast$`n_SCL GenPop`
#                                 ,"Mean_SCL.LI"          = item36.os.cast$`Mean_SCL LI`
#                                 ,"SE_SCL.LI"            = item36.os.cast$`SE_SCL LI`
#                                 ,"n_SCL.LI"             = item36.os.cast$`n_SCL LI`
#                                 ,"Mean_SCL.EH"          = item36.os.cast$`Mean_SCL EH`
#                                 ,"SE_SCL.EH"            = item36.os.cast$`SE_SCL EH`
#                                 ,"n_SCL.EH"             = item36.os.cast$`n_SCL EH`
#                                 ,"Mean_2017.RBSA.PS"    = item36.os.cast$`Mean_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item36.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item36.os.cast$`n_2017 RBSA PS`
#                                 ,"EB_SCL.GenPop"        = item36.os.cast$`EB_SCL GenPop`
#                                 ,"EB_SCL.LI"            = item36.os.cast$`EB_SCL LI`
#                                 ,"EB_SCL.EH"            = item36.os.cast$`EB_SCL EH`
#                                 ,"EB_2017.RBSA.PS"      = item36.os.cast$`EB_2017 RBSA PS`)
# }else if(os.ind == "snopud"){
#   item36.os.table <- data.frame("Housing.Vintage" = item36.os.cast$HomeYearBuilt_bins3
#                                 ,"Mean_SnoPUD"          = item36.os.cast$`Mean_SnoPUD`
#                                 ,"SE_SnoPUD"            = item36.os.cast$`SE_SnoPUD`
#                                 ,"n_SnoPUD"             = item36.os.cast$`n_SnoPUD`
#                                 ,"Mean_2017.RBSA.PS"    = item36.os.cast$`Mean_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item36.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item36.os.cast$`n_2017 RBSA PS`
#                                 ,"Mean_RBSA.NW"         = item36.os.cast$`Mean_2017 RBSA NW`
#                                 ,"SE_RBSA.NW"           = item36.os.cast$`SE_2017 RBSA NW`
#                                 ,"n_RBSA.NW"            = item36.os.cast$`n_2017 RBSA NW`
#                                 ,"EB_SnoPUD"            = item36.os.cast$`EB_SnoPUD`
#                                 ,"EB_2017.RBSA.PS"      = item36.os.cast$`EB_2017 RBSA PS`
#                                 ,"EB_RBSA.NW"           = item36.os.cast$`EB_2017 RBSA NW`)
# }
# 
# 
# 
# levels(item36.os.table$Housing.Vintage)
# rowOrder <- c("Pre 1981"
#               ,"1981-1990"
#               ,"1991-2000"
#               ,"2001-2010"
#               ,"Post 2010"
#               ,"All Vintages")
# item36.os.table <- item36.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
# item36.os.table <- data.frame(item36.os.table)
# 
# # exportTable(item36.os.table, "SF","Table 43",weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# #######################
# # Unweighted Analysis
# #######################
# item36.os.cast <- mean_two_groups_unweighted(CustomerLevelData = item36.os.data
#                                           ,valueVariable = "Normalized.Heat.Loss.Rate"
#                                           ,byVariableRow = "HomeYearBuilt_bins3"
#                                           ,byVariableColumn = "CK_Building_ID"
#                                           ,columnAggregate = "Region"
#                                           ,rowAggregate = "All Vintages")
# names(item36.os.cast)
# 
# if(os.ind == "scl"){
#   item36.os.table <- data.frame("Housing.Vintage"       = item36.os.cast$HomeYearBuilt_bins3
#                                 ,"Mean_SCL.GenPop"      = item36.os.cast$`Mean_SCL GenPop`
#                                 ,"SE_SCL.GenPop"        = item36.os.cast$`SE_SCL GenPop`
#                                 ,"n_SCL.GenPop"         = item36.os.cast$`n_SCL GenPop`
#                                 ,"Mean_SCL.LI"          = item36.os.cast$`Mean_SCL LI`
#                                 ,"SE_SCL.LI"            = item36.os.cast$`SE_SCL LI`
#                                 ,"n_SCL.LI"             = item36.os.cast$`n_SCL LI`
#                                 ,"Mean_SCL.EH"          = item36.os.cast$`Mean_SCL EH`
#                                 ,"SE_SCL.EH"            = item36.os.cast$`SE_SCL EH`
#                                 ,"n_SCL.EH"             = item36.os.cast$`n_SCL EH`
#                                 ,"Mean_2017.RBSA.PS"    = item36.os.cast$`Mean_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item36.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item36.os.cast$`n_2017 RBSA PS`)
# }else if(os.ind == "snopud"){
#   item36.os.table <- data.frame("Housing.Vintage"       = item36.os.cast$HomeYearBuilt_bins3
#                                 ,"Mean_SCL.GenPop"      = item36.os.cast$`Mean_SnoPUD`
#                                 ,"SE_SnoPUD"            = item36.os.cast$`SE_SnoPUD`
#                                 ,"n_SnoPUD"             = item36.os.cast$`n_SnoPUD`
#                                 ,"Mean_2017.RBSA.PS"    = item36.os.cast$`Mean_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item36.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item36.os.cast$`n_2017 RBSA PS`
#                                 ,"Mean_RBSA.NW"         = item36.os.cast$`Mean_2017 RBSA NW`
#                                 ,"SE_RBSA.NW"           = item36.os.cast$`SE_2017 RBSA NW`
#                                 ,"n_RBSA.NW"            = item36.os.cast$`n_2017 RBSA NW`)
# }
# 
# 
# levels(item36.os.table$Housing.Vintage)
# rowOrder <- c("Pre 1981"
#               ,"1981-1990"
#               ,"1991-2000"
#               ,"2001-2010"
#               ,"Post 2010"
#               ,"All Vintages")
# item36.os.table <- item36.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
# item36.os.table <- data.frame(item36.os.table)
# 
# # exportTable(item36.os.table, "SF","Table 43",weighted = FALSE, osIndicator = export.ind, OS = T)
# 
# 
# 
# 
# 
# 
# #############################################################################################
# #Item 37: AVERAGE HEAT-LOSS RATE BY VINTAGE AND CK_Building_ID (SF table 44, MH table 26)
# #############################################################################################
# item37.os.dat <- one.line.dat[which(colnames(one.line.dat) %in% c("CK_Cadmus_ID"
#                                                                ,"Whole.House.UA"))]
# item37.os.dat1 <- left_join(os.dat, item37.os.dat)
# item37.os.dat1$Whole.House.UA <- as.numeric(as.character(item37.os.dat1$Whole.House.UA))
# item37.os.dat2 <- item37.os.dat1[which(item37.os.dat1$Whole.House.UA %notin% c("N/A",NA)),]
# 
# item37.os.dat4 <-  item37.os.dat2[which(item37.os.dat2$HomeYearBuilt_bins3  %notin% c("N/A",NA)),]
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# item37.os.data <- weightedData(item37.os.dat4[-which(colnames(item37.os.dat4) %in% c("Whole.House.UA"))])
# item37.os.data <- left_join(item37.os.data, item37.os.dat4[which(colnames(item37.os.dat4) %in% c("CK_Cadmus_ID"
#                                                                                      ,"Whole.House.UA"))])
# item37.os.data$count <- 1
# #######################
# # Weighted Analysis
# #######################
# item37.os.cast <- mean_two_groups(CustomerLevelData = item37.os.data
#                                ,valueVariable    = "Whole.House.UA"
#                                ,byVariableRow    = "HomeYearBuilt_bins3"
#                                ,byVariableColumn = "CK_Building_ID"
#                                ,columnAggregate  = "Remove"
#                                ,rowAggregate     = "All Vintages")
# 
# names(item37.os.cast)
# 
# if(os.ind == "scl"){
#   item37.os.table <- data.frame("Housing.Vintage" = item37.os.cast$HomeYearBuilt_bins3
#                                 ,"Mean_SCL.GenPop"      = item37.os.cast$`Mean_SCL GenPop`
#                                 ,"SE_SCL.GenPop"        = item37.os.cast$`SE_SCL GenPop`
#                                 ,"n_SCL.GenPop"         = item37.os.cast$`n_SCL GenPop`
#                                 ,"Mean_SCL.LI"          = item37.os.cast$`Mean_SCL LI`
#                                 ,"SE_SCL.LI"            = item37.os.cast$`SE_SCL LI`
#                                 ,"n_SCL.LI"             = item37.os.cast$`n_SCL LI`
#                                 ,"Mean_SCL.EH"          = item37.os.cast$`Mean_SCL EH`
#                                 ,"SE_SCL.EH"            = item37.os.cast$`SE_SCL EH`
#                                 ,"n_SCL.EH"             = item37.os.cast$`n_SCL EH`
#                                 ,"Mean_2017.RBSA.PS"    = item37.os.cast$`Mean_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item37.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item37.os.cast$`n_2017 RBSA PS`
#                                 ,"EB_SCL.GenPop"        = item37.os.cast$`EB_SCL GenPop`
#                                 ,"EB_SCL.LI"            = item37.os.cast$`EB_SCL LI`
#                                 ,"EB_SCL.EH"            = item37.os.cast$`EB_SCL EH`
#                                 ,"EB_2017.RBSA.PS"      = item37.os.cast$`EB_2017 RBSA PS`)
# }else if(os.ind == "snopud"){
#   item37.os.table <- data.frame("Housing.Vintage" = item37.os.cast$HomeYearBuilt_bins3
#                                 ,"Mean_SnoPUD"          = item37.os.cast$`Mean_SnoPUD`
#                                 ,"SE_SnoPUD"            = item37.os.cast$`SE_SnoPUD`
#                                 ,"n_SnoPUD"             = item37.os.cast$`n_SnoPUD`
#                                 ,"Mean_2017.RBSA.PS"    = item37.os.cast$`Mean_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item37.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item37.os.cast$`n_2017 RBSA PS`
#                                 ,"Mean_RBSA.NW"         = item37.os.cast$`Mean_2017 RBSA NW`
#                                 ,"SE_RBSA.NW"           = item37.os.cast$`SE_2017 RBSA NW`
#                                 ,"n_RBSA.NW"            = item37.os.cast$`n_2017 RBSA NW`
#                                 ,"EB_SnoPUD"            = item37.os.cast$`EB_SnoPUD`
#                                 ,"EB_2017.RBSA.PS"      = item37.os.cast$`EB_2017 RBSA PS`
#                                 ,"EB_RBSA.NW"           = item37.os.cast$`EB_2017 RBSA NW`)
# }
# 
# 
# 
# levels(item37.os.table$Housing.Vintage)
# rowOrder <- c("Pre 1981"
#               ,"1981-1990"
#               ,"1991-2000"
#               ,"2001-2010"
#               ,"Post 2010"
#               ,"All Vintages")
# item37.os.table <- item37.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
# item37.os.table <- data.frame(item37.os.table)
# 
# # exportTable(item37.os.table, "SF","Table 44",weighted = TRUE, osIndicator = export.ind, OS = T)
# 
# #######################
# # Unweighted Analysis
# #######################
# item37.os.cast <- mean_two_groups_unweighted(CustomerLevelData = item37.os.data
#                                           ,valueVariable    = "Whole.House.UA"
#                                           ,byVariableRow    = "HomeYearBuilt_bins3"
#                                           ,byVariableColumn = "CK_Building_ID"
#                                           ,columnAggregate  = "Region"
#                                           ,rowAggregate     = "All Vintages")
# 
# names(item37.os.cast)
# 
# if(os.ind == "scl"){
#   item37.os.table <- data.frame("Housing.Vintage" = item37.os.cast$HomeYearBuilt_bins3
#                                 ,"Mean_SCL.GenPop"      = item37.os.cast$`Mean_SCL GenPop`
#                                 ,"SE_SCL.GenPop"        = item37.os.cast$`SE_SCL GenPop`
#                                 ,"n_SCL.GenPop"         = item37.os.cast$`n_SCL GenPop`
#                                 ,"Mean_SCL.LI"          = item37.os.cast$`Mean_SCL LI`
#                                 ,"SE_SCL.LI"            = item37.os.cast$`SE_SCL LI`
#                                 ,"n_SCL.LI"             = item37.os.cast$`n_SCL LI`
#                                 ,"Mean_SCL.EH"          = item37.os.cast$`Mean_SCL EH`
#                                 ,"SE_SCL.EH"            = item37.os.cast$`SE_SCL EH`
#                                 ,"n_SCL.EH"             = item37.os.cast$`n_SCL EH`
#                                 ,"Mean_2017.RBSA.PS"    = item37.os.cast$`Mean_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item37.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item37.os.cast$`n_2017 RBSA PS`)
# }else if(os.ind == "snopud"){
#   item37.os.table <- data.frame("Housing.Vintage" = item37.os.cast$HomeYearBuilt_bins3
#                                 ,"Mean_SnoPUD"          = item37.os.cast$`Mean_SnoPUD`
#                                 ,"SE_SnoPUD"            = item37.os.cast$`SE_SnoPUD`
#                                 ,"n_SnoPUD"             = item37.os.cast$`n_SnoPUD`
#                                 ,"Mean_2017.RBSA.PS"    = item37.os.cast$`Mean_2017 RBSA PS`
#                                 ,"SE_2017.RBSA.PS"      = item37.os.cast$`SE_2017 RBSA PS`
#                                 ,"n_2017.RBSA.PS"       = item37.os.cast$`n_2017 RBSA PS`
#                                 ,"Mean_RBSA.NW"         = item37.os.cast$`Mean_2017 RBSA NW`
#                                 ,"SE_RBSA.NW"           = item37.os.cast$`SE_2017 RBSA NW`
#                                 ,"n_RBSA.NW"            = item37.os.cast$`n_2017 RBSA NW`)
# }
# 
# 
# 
# levels(item37.os.table$Housing.Vintage)
# rowOrder <- c("Pre 1981"
#               ,"1981-1990"
#               ,"1991-2000"
#               ,"2001-2010"
#               ,"Post 2010"
#               ,"All Vintages")
# item37.os.table <- item37.os.table %>% mutate(Housing.Vintage = factor(Housing.Vintage, levels = rowOrder)) %>% arrange(Housing.Vintage)  
# item37.os.table <- data.frame(item37.os.table)
# 
# # exportTable(item37.os.table, "SF","Table 44",weighted = FALSE, osIndicator = export.ind, OS = T)
