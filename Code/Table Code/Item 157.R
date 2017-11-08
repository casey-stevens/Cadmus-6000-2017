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
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
# Item 157: DISTRIBUTION OF ELECTRICALLY HEATED HOMES BY GROUND CONTACT TYPE AND STATE (SF table B-2)
#############################################################################################

#############################################################################################
#
#For Envelope information
#
#############################################################################################

# Bring in clean ground contact types
GroundContactTypes <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "Ground Contact Types.xlsx"), sheet = 1)
GroundContactTypes <- GroundContactTypes[which(!(colnames(GroundContactTypes) == "Notes"))]

#subset to columns need in table
env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_Construction_BLDG_STRUCTURE_FoundationType"))]
env.dat1 <- env.dat[which(!(is.na(env.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType))),]

#merge table columns to generic columns
item157.dat <- unique(left_join(rbsa.dat, env.dat1, by = "CK_Cadmus_ID"))
item157.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType <- trimws(item157.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType)
unique(item157.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType)

###########################
# Clean Ground Contact types
###########################
item157.dat$GroundContact <- item157.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType
for (i in 1:length(GroundContactTypes$Raw.data.categories)){
  item157.dat$GroundContact[which(item157.dat$GroundContact == GroundContactTypes$Raw.data.categories[i])] <- GroundContactTypes$New.categories[i]
}
###########################
# End cleaning Step
###########################
item157.dat$GroundContact <- trimws(item157.dat$GroundContact)
unique(item157.dat$GroundContact)

item157.dat1 <- item157.dat[which(!(is.na(item157.dat$GroundContact))),]
item157.dat2 <- item157.dat1[which(item157.dat1$GroundContact != "Remove"),]

item157.envelope <- item157.dat2


#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item157.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"Primary.Heating.System"
                                                                    ,"Heating.Fuel"
                                                                    ,""
                                                                    ,""))]

#remove any repeat header rows from exporting
item157.dat.11 <- item157.dat.1[which(item157.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item157.dat.12 <- item157.dat.11[which(item157.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item157.dat.12$CK_Cadmus_ID))
#check uniques
unique(item157.dat.12$Primary.Heating.System)
item157.dat.12$count <- 1

item157.dat.13 <- unique(item157.dat.12[which(item157.dat.12$Heating.Fuel == "Electric"),])

item157.sum <- summarise(group_by(item157.dat.13, CK_Cadmus_ID, Heating.Fuel)
                         ,Count = sum(count))
item157.sum$Count <- 1
which(duplicated(item157.sum$CK_Cadmus_ID)) #none are duplicated!
unique(item157.sum$Heating.Fuel)

item157.merge <- left_join(rbsa.dat, item157.sum)
item157.merge <- item157.merge[which(!is.na(item157.merge$Heating.Fuel)),]

item157.mechanical <- item157.merge






#############################################################################################
#
#Merge mechanical and envelope data together
#
#############################################################################################

item157.merge1 <- left_join(item157.mechanical, item157.envelope)

item157.merge2 <- item157.merge1[which(!(is.na(item157.merge1$GroundContact))),]



item157.merge <- left_join(rbsa.dat, item157.merge2)
item157.merge <- item157.merge[which(!is.na(item157.merge$GroundContact)),]


################################################
# Adding pop and sample sizes for weights
################################################
item157.data <- weightedData(item157.merge[-which(colnames(item157.merge) %in% c("Heating.Fuel"
                                                                                 ,"Count"
                                                                                 ,"ENV_Construction_BLDG_STRUCTURE_FoundationType"
                                                                                 ,"GroundContact"))])
item157.data <- left_join(item157.data, item157.merge[which(colnames(item157.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Heating.Fuel"
                                                                                           ,"Count"
                                                                                           ,"GroundContact"))])
#######################
# Weighted Analysis
#######################
item157.final <- proportionRowsAndColumns1(CustomerLevelData = item157.data
                                           ,valueVariable    = 'Count'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'GroundContact'
                                           ,aggregateColumnName = "Region")

item157.cast <- dcast(setDT(item157.final)
                      , formula = BuildingType + GroundContact ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item157.table <- data.frame("BuildingType"    = item157.cast$BuildingType
                            ,"GroundContact"= item157.cast$GroundContact
                            ,"Percent_ID"     = item157.cast$w.percent_ID
                            ,"SE_ID"          = item157.cast$w.SE_ID
                            ,"Count_ID"       = item157.cast$count_ID
                            ,"n_ID"           = item157.cast$n_ID
                            ,"Percent_MT"     = item157.cast$w.percent_MT
                            ,"SE_MT"          = item157.cast$w.SE_MT
                            ,"Count_MT"       = item157.cast$count_MT
                            ,"n_MT"           = item157.cast$n_MT
                            ,"Percent_OR"     = item157.cast$w.percent_OR
                            ,"SE_OR"          = item157.cast$w.SE_OR
                            ,"Count_OR"       = item157.cast$count_OR
                            ,"n_OR"           = item157.cast$n_OR
                            ,"Percent_WA"     = item157.cast$w.percent_WA
                            ,"SE_WA"          = item157.cast$w.SE_WA
                            ,"Count_WA"       = item157.cast$count_WA
                            ,"n_WA"           = item157.cast$n_WA
                            ,"Percent_Region" = item157.cast$w.percent_Region
                            ,"SE_Region"      = item157.cast$w.SE_Region
                            ,"Count_Region"   = item157.cast$count_Region
                            ,"n_Region"       = item157.cast$n_Region
)


item157.final.SF <- item157.table[which(item157.table$BuildingType == "Single Family")
                                  ,-which(colnames(item157.table) %in% c("BuildingType"))]

exportTable(item157.final.SF, "SF", "Table B-2", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item157.final <- proportions_two_groups_unweighted(CustomerLevelData = item157.data
                                                   ,valueVariable    = 'Count'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'GroundContact'
                                                   ,aggregateColumnName = "Region")

item157.cast <- dcast(setDT(item157.final)
                      , formula = BuildingType + GroundContact ~ State
                      , value.var = c("Percent", "SE", "Count", "SampleSize"))


item157.table <- data.frame("BuildingType"    = item157.cast$BuildingType
                            ,"GroundContact"  = item157.cast$GroundContact
                            ,"Percent_ID"     = item157.cast$Percent_ID
                            ,"SE_ID"          = item157.cast$SE_ID
                            ,"Count_ID"       = item157.cast$Count_ID
                            ,"Percent_MT"     = item157.cast$Percent_MT
                            ,"SE_MT"          = item157.cast$SE_MT
                            ,"Count_MT"       = item157.cast$Count_MT
                            ,"Percent_OR"     = item157.cast$Percent_OR
                            ,"SE_OR"          = item157.cast$SE_OR
                            ,"Count_OR"       = item157.cast$Count_OR
                            ,"Percent_WA"     = item157.cast$Percent_WA
                            ,"SE_WA"          = item157.cast$SE_WA
                            ,"Count_WA"       = item157.cast$Count_WA
                            ,"Percent_Region" = item157.cast$Percent_Region
                            ,"SE_Region"      = item157.cast$SE_Region
                            ,"Count_Region"   = item157.cast$Count_Region
)


item157.final.SF <- item157.table[which(item157.table$BuildingType == "Single Family")
                                  ,-which(colnames(item157.table) %in% c("BuildingType"))]

exportTable(item157.final.SF, "SF", "Table B-2", weighted = FALSE)

