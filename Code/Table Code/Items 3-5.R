#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:          09/11/2017                                  
##  Billing Code(s):  6596.0000.0002.1002.0000
#############################################################################################

##  Clear variables
rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")

##  Create "Not In" operator
"%notin%" <- Negate("%in%")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601


#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

# Bring in clean ground contact types
GroundContactTypes <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "Ground Contact Types.xlsx"), sheet = 1)
GroundContactTypes <- GroundContactTypes[which(colnames(GroundContactTypes) != "Notes")]




#############################################################################################
# Item 3: DISTRIBUTION OF HOMES BY GROUND CONTACT TYPE AND STATE 
#############################################################################################
#############################################################
# Weighting Implementation function: Proportion, two groups
#############################################################

env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                            , "ENV_Construction_BLDG_STRUCTURE_FoundationType"))]
colnames(env.dat) <- c("CK_Cadmus_ID"
                       , "FoundationType")
env.dat1 <- env.dat[which(!(is.na(env.dat$FoundationType))),]
env.dat1$FoundationType <- trimws(env.dat1$FoundationType)

#merge table columns to generic columns
item3.dat <- unique(left_join(rbsa.dat, env.dat1, by = "CK_Cadmus_ID"))



# Clean Ground Contact types
i=10
item3.dat$GroundContact <- item3.dat$FoundationType
for (i in 1:length(GroundContactTypes$Raw.data.categories)){
  item3.dat$GroundContact[which(item3.dat$GroundContact == GroundContactTypes$Raw.data.categories[i])] <- GroundContactTypes$New.categories[i]
}
item3.dat$GroundContact <- trimws(item3.dat$GroundContact)
# End cleaning Step
unique(item3.dat$GroundContact)

# Remove unwanted ground contact types
item3.dat1 <- item3.dat[which(item3.dat$GroundContact %notin% c("Remove", NA)),]

#subset to only single family for item 3
item3.dat2 <- item3.dat1[which(item3.dat1$BuildingType == "Single Family"),]



#add weighting information
item3.data <- weightedData(item3.dat2[-which(colnames(item3.dat2) %in% c("FoundationType", "GroundContact"))])

item3.data <- left_join(item3.data, item3.dat2[which(colnames(item3.dat2) %in% c("CK_Cadmus_ID", "FoundationType", "GroundContact"))])

item3.data$count <- 1
colnames(item3.data)



##############################
# Weighted Analysis
##############################
item3.final <- proportionRowsAndColumns1(item3.data
                                         , valueVariable = 'count'
                                         , columnVariable = 'State'
                                         , rowVariable = 'GroundContact'
                                         , aggregateColumnName = "Region")

colnames(item3.final) <- c("BuildingType"
                           , "State"
                           , "GroundContact"
                           , "Percent"
                           , "SE"
                           , "Count"
                           , "PopSize"
                           , "SampleSize")

item3.cast <- dcast(setDT(item3.final)
                    ,formula = BuildingType + GroundContact ~ State
                    ,value.var = c("Percent", "SE", "SampleSize", "Count", "PopSize"))

item3.table <- data.frame("BuildingType"    = item3.cast$BuildingType
                          ,"GroundContact"  = item3.cast$GroundContact
                          ,"Percent_ID"     = item3.cast$Percent_ID
                          ,"SE_ID"          = item3.cast$SE_ID
                          ,"n_ID"           = item3.cast$Count_ID
                          ,"Percent_MT"     = item3.cast$Percent_MT
                          ,"SE_MT"          = item3.cast$SE_MT
                          ,"n_MT"           = item3.cast$Count_MT
                          ,"Percent_OR"     = item3.cast$Percent_OR
                          ,"SE_OR"          = item3.cast$SE_OR
                          ,"n_OR"           = item3.cast$Count_OR
                          ,"Percent_WA"     = item3.cast$Percent_WA
                          ,"SE_WA"          = item3.cast$SE_WA
                          ,"n_WA"           = item3.cast$Count_WA
                          ,"Percent_Region" = item3.cast$Percent_Region
                          ,"SE_Region"      = item3.cast$SE_Region
                          ,"SampleSize"     = item3.cast$Count_Region)

item3.table.SF <- item3.table[which(item3.table$BuildingType == "Single Family"),-1]


exportTable(item3.table.SF, "SF", "Table 10"
            , weighted = TRUE)



##############################
# Unweighted Analysis
##############################
item3.final <- proportions_two_groups_unweighted(item3.data
                                         , valueVariable = 'count'
                                         , columnVariable = 'State'
                                         , rowVariable = 'GroundContact'
                                         , aggregateColumnName = "Region")

colnames(item3.final) <- c("BuildingType"
                           , "State"
                           , "GroundContact"
                           , "Count"
                           , "n"
                           , "Total.Count"
                           , "Denom"
                           , "Percent"
                           , "SE")

item3.cast <- dcast(setDT(item3.final)
                    ,formula = BuildingType + GroundContact ~ State
                    ,value.var = c("Percent", "SE", "Count", "n"))

item3.table <- data.frame("BuildingType"    = item3.cast$BuildingType
                          ,"GroundContact"  = item3.cast$GroundContact
                          ,"Percent_ID"     = item3.cast$Percent_ID
                          ,"SE_ID"          = item3.cast$SE_ID
                          ,"n_ID"           = item3.cast$Count_ID
                          ,"Percent_MT"     = item3.cast$Percent_MT
                          ,"SE_MT"          = item3.cast$SE_MT
                          ,"n_MT"           = item3.cast$Count_MT
                          ,"Percent_OR"     = item3.cast$Percent_OR
                          ,"SE_OR"          = item3.cast$SE_OR
                          ,"n_OR"           = item3.cast$Count_OR
                          ,"Percent_WA"     = item3.cast$Percent_WA
                          ,"SE_WA"          = item3.cast$SE_WA
                          ,"n_WA"           = item3.cast$Count_WA
                          ,"Percent_Region" = item3.cast$Percent_Region
                          ,"SE_Region"      = item3.cast$SE_Region
                          ,"n_Region"       = item3.cast$Count_Region
                          ,"SampleSize"     = item3.cast$n_Region)

item3.table.SF <- item3.table[which(item3.table$BuildingType == "Single Family"),-1]


exportTable(item3.table.SF, "SF", "Table 10"
            , weighted = FALSE)






#############################################################################################
# Item 4: AVERAGE CONDITIONED FLOOR AREA BY STATE
#############################################################################################
######################################################
# Weighting Implementation function: Mean, one group
######################################################
env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                            , "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]
colnames(env.dat) <- c("CK_Cadmus_ID"
                       , "BldgLevel_Area_SqFt")

#make conditioned area as.numeric
env.dat$ConditionedArea <- as.numeric(as.character(env.dat$BldgLevel_Area_SqFt))


#merge
item4.dat <- left_join(rbsa.dat, env.dat, by = "CK_Cadmus_ID")
length(unique(item4.dat$CK_Cadmus_ID)) #601

item4.dat1 <- item4.dat[which(item4.dat$BuildingType != "Multifamily"),]
item4.dat2 <- item4.dat1[which(!is.na(item4.dat1$ConditionedArea)),]


item4.data <- weightedData(item4.dat2[-which(colnames(item4.dat2) %in% c("BldgLevel_Area_SqFt"
                                                                         ,"ConditionedArea"))])
item4.data <- left_join(item4.data, item4.dat2[which(colnames(item4.dat2) %in% c("CK_Cadmus_ID"
                                                                                 , "BldgLevel_Area_SqFt"
                                                                                 , "ConditionedArea"))])

item4.data$count <- 1
colnames(item4.data)

######################################################
# Summarise data up to unique customer level
######################################################
item4.customer <- summarise(group_by(item4.data
                                     , BuildingType
                                     , CK_Cadmus_ID
                                     , State
                                     , Region
                                     , Territory
                                     , n.h
                                     , N.h)
                            ,siteAreaConditioned = sum(ConditionedArea)
)



##############################
# Weighted Analysis
##############################
item4.final <- mean_one_group(CustomerLevelData = item4.customer
                              , valueVariable = 'siteAreaConditioned'
                              , byVariable    = 'State'
                              , aggregateRow  = 'Region'
                              , weighted = TRUE)


item4.table.SF <- item4.final[which(item4.final$BuildingType %in% c("Single Family")),-1]
item4.table.MH <- item4.final[which(item4.final$BuildingType %in% c("Manufactured")),-1]

exportTable(item4.table.SF, "SF", "Table 11"
            , weighted = TRUE)
exportTable(item4.table.MH, "MH", "Table 10"
            , weighted = TRUE)



##############################
# Unweighted Analysis
##############################
item4.final <- mean_one_group(CustomerLevelData = item4.customer
                              , valueVariable = 'siteAreaConditioned'
                              , byVariable    = 'State'
                              , aggregateRow  = 'Region'
                              , weighted = FALSE)


item4.table.SF <- item4.final[which(item4.final$BuildingType %in% c("Single Family")),-1]
item4.table.MH <- item4.final[which(item4.final$BuildingType %in% c("Manufactured")),-1]

exportTable(item4.table.SF, "SF", "Table 11"
            , weighted = FALSE)
exportTable(item4.table.MH, "MH", "Table 10"
            , weighted = FALSE)








##########################################################################
# Item 5: AVERAGE CONDITIONED FLOOR AREA BY STATE AND VINTAGE
##########################################################################
######################################################
# Weighting Implementation function: Mean, two groups
######################################################

env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                            , "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]
colnames(env.dat) <- c("CK_Cadmus_ID"
                       , "BldgLevel_Area_SqFt")

#make conditioned area as.numeric
env.dat$ConditionedArea <- as.numeric(as.character(env.dat$BldgLevel_Area_SqFt))


#merge
item5.dat <- left_join(rbsa.dat, env.dat, by = "CK_Cadmus_ID")
length(unique(item5.dat$CK_Cadmus_ID)) #601

item5.dat1 <- item5.dat[which(item5.dat$BuildingType != "Multifamily"),]
item5.dat2 <- item5.dat1[which(!is.na(item5.dat1$ConditionedArea)),]
item5.dat3 <- item5.dat2[which(!is.na(item5.dat2$HomeYearBuilt)),]


item5.data <- weightedData(item5.dat3[-which(colnames(item5.dat3) %in% c("BldgLevel_Area_SqFt"
                                                                         ,"ConditionedArea"))])
item5.data <- left_join(item5.data, item5.dat3[which(colnames(item5.dat3) %in% c("CK_Cadmus_ID"
                                                                                 , "BldgLevel_Area_SqFt"
                                                                                 , "ConditionedArea"))])

item5.data$count <- 1
colnames(item5.data)

##############################
# Summarise to customer level
##############################
item5.customer <- summarise(group_by(item5.data
                                     , BuildingType
                                     , HomeYearBuilt_bins2
                                     , CK_Cadmus_ID
                                     , State
                                     , Region
                                     , Territory
                                     , n.h
                                     , N.h)
                            ,siteAreaConditioned = sum(ConditionedArea)
)



##############################
# Weighted Analysis
##############################
item5.final <- mean_two_groups(CustomerLevelData  = item5.customer
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
                          ,"n_ID"            = item5.final$SampleSize_ID
                          ,"Mean_MT"         = item5.final$Mean_MT
                          ,"SE_MT"           = item5.final$SE_MT
                          ,"n_MT"            = item5.final$SampleSize_MT
                          ,"Mean_OR"         = item5.final$Mean_OR
                          ,"SE_OR"           = item5.final$SE_OR
                          ,"n_OR"            = item5.final$SampleSize_OR
                          ,"Mean_WA"         = item5.final$Mean_WA
                          ,"SE_WA"           = item5.final$SE_WA
                          ,"n_WA"            = item5.final$SampleSize_WA
                          ,"Mean_Region"     = item5.final$Mean_Region
                          ,"SE_Region"       = item5.final$SE_Region
                          ,"n_Region"        = item5.final$SampleSize_Region)


item5.table.SF <- item5.table[which(item5.table$BuildingType %in% c("Single Family")),-1]
item5.table.MH <- item5.table[which(item5.table$BuildingType %in% c("Manufactured")),-1]

exportTable(item5.table.SF, "SF", "Table 12"
            , weighted = TRUE)
exportTable(item5.table.MH, "MH", "Table 11"
            , weighted = TRUE)



##############################
# Unweighted Analysis
##############################
item5.final <- mean_two_groups_unweighted(CustomerLevelData  = item5.customer
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


item5.table.SF <- item5.table[which(item5.table$BuildingType %in% c("Single Family")),-1]
item5.table.MH <- item5.table[which(item5.table$BuildingType %in% c("Manufactured")),-1]

exportTable(item5.table.SF, "SF", "Table 12"
            , weighted = FALSE)
exportTable(item5.table.MH, "MH", "Table 11"
            , weighted = FALSE)
