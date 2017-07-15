#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

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
unique(item157.dat$GroundContact)


item157.dat1 <- item157.dat[which(!(is.na(item157.dat$GroundContact))),]
item157.dat2 <- item157.dat1[which(item157.dat1$GroundContact != "Remove"),]
item157.dat2$GroundContact <- trimws(item157.dat2$GroundContact)



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
length(unique(item157.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item157.dat.12$Primary.Heating.System)
item157.dat.12$count <- 1

item157.dat.13 <- unique(item157.dat.12[which(item157.dat.12$Heating.Fuel == "Electric"),])

item157.sum1 <- summarise(group_by(item157.dat.13, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item157.sum1$Count <- 1
which(duplicated(item157.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(item157.sum1$Heating.Fuel)

item157.mechanical <- item157.sum1






#############################################################################################
#
#Merge mechanical and envelope data together
#
#############################################################################################

item157.merge <- left_join(item157.mechanical, item157.envelope, by = c("CK_Cadmus_ID"))

item157.merge1 <- item157.merge[which(!(is.na(item157.merge$GroundContact))),]

#summarise by state
#by ground contact types
item157.state1 <- summarise(group_by(item157.merge1, BuildingType, GroundContact, State)
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(Count))
#across ground contact types
item157.state2 <- summarise(group_by(item157.merge1, BuildingType, State)
                            ,GroundContact = "Total"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(Count))
#summarise across states
#by ground contact types
item157.region1 <- summarise(group_by(item157.merge1, BuildingType, GroundContact)
                             ,State = "Region"
                             ,SampleSize = length(unique(CK_Cadmus_ID))
                             ,Count = sum(Count))
#across ground contact types
item157.region2 <- summarise(group_by(item157.merge1, BuildingType)
                             ,GroundContact = "Total"
                             ,State = "Region"
                             ,SampleSize = length(unique(CK_Cadmus_ID))
                             ,Count = sum(Count))
#row bind state information together
item157.merge2 <- rbind.data.frame(item157.state1, item157.state2, item157.region1, item157.region2, stringsAsFactors = F)

item157.total.counts <-  rbind.data.frame(item157.state2, item157.region2, stringsAsFactors = F)
item157.total.counts <- item157.total.counts[which(colnames(item157.total.counts) %in% c("BuildingType", "State", "Count"))]

#merge on total counts
item157.final <- left_join(item157.merge2, item157.total.counts, by = c("BuildingType", "State"))
colnames(item157.final) <- c("BuildingType", "Ground.Contact", "State", "SampleSize", "Count", "Total.Count")

#calculate percent and SE
item157.final$Percent <- item157.final$Count / item157.final$Total.Count
item157.final$SE <- sqrt(item157.final$Percent * (1 - item157.final$Percent) / item157.final$SampleSize)

#cast out by state
library(data.table)
item157.cast <- dcast(setDT(item157.final)
                      ,formula = BuildingType + Ground.Contact ~ State
                      ,value.var = c("Percent", "SE", "SampleSize"))

#put in table format
item157.table <- data.frame("BuildingType" = item157.cast$BuildingType
                            ,"Ground.Contact" = item157.cast$Ground.Contact
                            ,"Percent_MT" = item157.cast$Percent_MT
                            ,"SE_MT" = item157.cast$SE_MT
                            # ,"Percent_OR" = item157.cast$Percent_OR
                            # ,"SE_OR" = item157.cast$SE_OR
                            ,"Percent_WA" = item157.cast$Percent_WA
                            ,"SE_WA" = item157.cast$SE_WA
                            ,"Percent_Region" = item157.cast$Percent_Region
                            ,"SE_Region" = item157.cast$SE_Region
                            ,"SampleSize" = item157.cast$SampleSize_Region)

item157.table.final <- item157.table[which(item157.table$BuildingType %in% c("Single Family")),]




