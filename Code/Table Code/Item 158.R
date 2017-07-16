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
# Item 158: AVERAGE CONDITIONED FLOOR AREA BY STATE, ELECTRICALLY HEATED HOMES (SF table B-4)
#############################################################################################

#############################################################################################
#
#For Envelope information
#
#############################################################################################

#subset to columns need in table
env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]
env.dat1 <- env.dat[which(!(is.na(env.dat$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))),]

env.dat2 <- env.dat1[which(!(env.dat1$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt %in% c("0", "Unknown"))),]
env.dat2$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt <- as.numeric(as.character(env.dat2$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))

env.sum <- summarise(group_by(env.dat2, CK_Cadmus_ID)
                     ,Site.SQFT = sum(ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))

item158.envelope <- env.sum



#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item158.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item158.dat.11 <- item158.dat.1[which(item158.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item158.dat.12 <- item158.dat.11[which(item158.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item158.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item158.dat.12$Primary.Heating.System)
item158.dat.12$count <- 1

item158.dat.13 <- unique(item158.dat.12[which(item158.dat.12$Heating.Fuel == "Electric"),])

item158.sum1 <- summarise(group_by(item158.dat.13, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item158.sum1$Count <- 1
which(duplicated(item158.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(item158.sum1$Heating.Fuel)

item158.mechanical <- item158.sum1






#############################################################################################
#
#Merge mechanical and envelope data together
#
#############################################################################################

item158.merge <- left_join(item158.envelope, item158.mechanical, by = c("CK_Cadmus_ID"))

item158.merge1 <- item158.merge[which(!(is.na(item158.merge$Heating.Fuel))),]

item158.merge2 <- left_join(item158.merge1, rbsa.dat, by = c("CK_Cadmus_ID"))

#summarise by state
item158.state <- summarise(group_by(item158.merge2, BuildingType, State)
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean = mean(Site.SQFT)
                            ,SE = sd(Site.SQFT) / sqrt(SampleSize))
#summarise across states
item158.region <- summarise(group_by(item158.merge2, BuildingType)
                             ,State = "Region"
                             ,SampleSize = length(unique(CK_Cadmus_ID))
                             ,Mean = mean(Site.SQFT)
                             ,SE = sd(Site.SQFT) / sqrt(SampleSize))

#row bind state information together
item158.final <- rbind.data.frame(item158.state, item158.region, stringsAsFactors = F)

item158.table <- data.frame("BuildingType" = item158.final$BuildingType
                            ,"State" = item158.final$State
                            ,"Mean" = item158.final$Mean
                            ,"SE" = item158.final$SE
                            ,"SampleSize" = item158.final$SampleSize)


item158.table1 <- item158.table[which(item158.table$BuildingType %in% c("Single Family")),]




