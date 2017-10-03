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



#############################################################################################
# Item 3: DISTRIBUTION OF HOMES BY GROUND CONTACT TYPE AND STATE 
#############################################################################################
# Bring in clean ground contact types
GroundContactTypes <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "Ground Contact Types.xlsx"), sheet = 1)
GroundContactTypes <- GroundContactTypes[which(colnames(GroundContactTypes) != "Notes")]

#subset to columns need in table
env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_Construction_BLDG_STRUCTURE_FoundationType"))]
env.dat1 <- env.dat[which(!(is.na(env.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType))),]

#merge table columns to generic columns
item3.dat <- unique(left_join(rbsa.dat, env.dat1, by = "CK_Cadmus_ID"))
item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType <- trimws(item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType)

###########################
# Clean Ground Contact types
###########################
item3.dat$GroundContact <- item3.dat$ENV_Construction_BLDG_STRUCTURE_FoundationType
for (i in 1:length(GroundContactTypes$Raw.data.categories)){
  item3.dat$GroundContact[which(item3.dat$GroundContact == GroundContactTypes$Raw.data.categories[i])] <- GroundContactTypes$New.categories[i]
}
###########################
# End cleaning Step
###########################
unique(item3.dat$GroundContact)

# Remove unwanted ground contact types
item3.dat1 <- item3.dat[which(item3.dat$GroundContact != "Remove"),]
item3.dat2 <- item3.dat1[which(item3.dat1$BuildingType == "Single Family"),]

item3.dat2$count <- 1
#Get state information
#across home types
item3.state.tab0 <- summarise(group_by(item3.dat2, BuildingType, State)
                              ,GroundContact = "Total"
                              ,Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID)))

item3.state.tab1 <- summarise(group_by(item3.dat2, BuildingType, GroundContact, State)
                              ,Count = sum(count)
                              ,SampleSize = length(unique(CK_Cadmus_ID)))
item3.state <- rbind.data.frame(item3.state.tab1, item3.state.tab0, stringsAsFactors = F)

#get region information
item3.region.tab0 <- summarise(group_by(item3.dat2, BuildingType)
                               ,GroundContact = "Total"
                               ,State = "Region"
                               ,Count = sum(count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)
item3.region.tab1 <- summarise(group_by(item3.dat2, BuildingType, GroundContact)
                               ,State = "Region"
                               ,Count = sum(count)
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)
item3.region <- rbind.data.frame(item3.region.tab1, item3.region.tab0, stringsAsFactors = F)

item3.total <- rbind.data.frame(item3.state.tab0, item3.region.tab0, stringsAsFactors = F)
item3.total1 <- item3.total[which(colnames(item3.total) %in% c("BuildingType", "State", "Count"))]

#rbind state and region information
item3.tab.full <- rbind.data.frame(item3.state, item3.region, stringsAsFactors = F)

item3.final <- left_join(item3.tab.full, item3.total1, by = c("BuildingType", "State"))
colnames(item3.final) <- c("BuildingType", "GroundContact", "State", "Count", "SampleSize", "Total.Count")

item3.final$Percent <- item3.final$Count / item3.final$Total.Count
item3.final$SE      <- sqrt((item3.final$Percent * (1 - item3.final$Percent)) / item3.final$SampleSize)


#############################################################################################
library(data.table)
item3.table <- dcast(setDT(item3.final)
                     ,formula = BuildingType + GroundContact ~ State
                     ,value.var = c("Percent", "SE", "SampleSize"))

item3.table1 <- data.frame("BuildingType" = item3.table$BuildingType
                           ,"GroundContact" = item3.table$GroundContact
                           ,"Percent_MT" = item3.table$Percent_MT
                           ,"SE_MT" = item3.table$SE_MT
                           ,"Percent_OR" = item3.table$Percent_OR
                           ,"SE_OR" = item3.table$SE_OR
                           ,"Percent_WA" = item3.table$Percent_WA
                           ,"SE_WA" = item3.table$SE_WA
                           ,"Percent_Region" = item3.table$Percent_Region
                           ,"SE_Region" = item3.table$SE_Region
                           ,"SampleSize" = item3.table$SampleSize_Region)

item3.table.final <- item3.table1[which(item3.table1$BuildingType %in% c("Single Family", "Manufactured")),]










#############################################################################################
# Item 4: AVERAGE CONDITIONED FLOOR AREA BY STATE
#############################################################################################

item4.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]

#merge
item4.dat1 <- left_join(rbsa.dat, item4.dat, by = "CK_Cadmus_ID")
length(unique(item4.dat1$CK_Cadmus_ID)) #565, yay!

#remove NAs
item4.dat2 <- item4.dat1[which(!(is.na(item4.dat1$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))),]
length(unique(item4.dat2$CK_Cadmus_ID)) #410, boo!

#make conditioned area as.numeric
item4.dat2$ConditionedArea <- as.numeric(as.character(item4.dat2$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))

#summarise by state
item4.state.dat <- summarise(group_by(item4.dat2,BuildingType , CK_Cadmus_ID, State)
                      ,siteAreaConditioned = sum(ConditionedArea)
)

item4.state.dat1 <- summarise(group_by(item4.state.dat, BuildingType, State)
                        ,AveAreaConditioned = mean(siteAreaConditioned)
                        ,SEAreaConditioned  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                        ,SampleSize         = length(unique(CK_Cadmus_ID))
                        )

#summarise by region
item4.region.dat <- summarise(group_by(item4.dat2, BuildingType, CK_Cadmus_ID)
                              ,State = "Region"
                             ,siteAreaConditioned = sum(ConditionedArea)
)

item4.region.dat1 <- summarise(group_by(item4.region.dat, BuildingType)
                               ,State = "Region"
                              ,AveAreaConditioned = mean(siteAreaConditioned)
                              ,SEAreaConditioned  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,SampleSize         = length(unique(CK_Cadmus_ID))
                              )

item4.final <- rbind.data.frame(item4.state.dat1, item4.region.dat1, stringsAsFactors = F) 


item4.final.SF <- item4.final[which(item4.final$BuildingType == 'Single Family'),
                              -which(colnames(item4.final) == 'BuildingType')]
workbook.SF <- loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep = "/"))
writeData(workbook.SF, sheet = "Table 11", x = item4.final.SF, startRow = 20)
saveWorkbook(workbook.SF, file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep="/"), overwrite = T)










#############################################################################################
# Item 5: AVERAGE CONDITIONED FLOOR AREA BY STATE AND VINTAGE
##########################################################################item4.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]

item5.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]

#merge
item5.dat1 <- left_join(rbsa.dat, item5.dat, by = "CK_Cadmus_ID")
length(unique(item5.dat1$CK_Cadmus_ID)) #565, yay!

#remove NAs
item5.dat2 <- item5.dat1[which(!(is.na(item5.dat1$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))),]
length(unique(item5.dat2$CK_Cadmus_ID)) #410, boo!

#make conditioned area as.numeric
item5.dat2$ConditionedArea <- as.numeric(as.character(item5.dat2$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))


#### By state/region across homeyearbuilt
#summarise by state
item5.state.dat00 <- summarise(group_by(item5.dat2, BuildingType, CK_Cadmus_ID, State)
                             ,siteAreaConditioned = sum(ConditionedArea)
)

item5.state.dat01 <- summarise(group_by(item5.state.dat00, BuildingType, State)
                               ,HomeYearBuilt_bins = "All Vintages"
                              ,Mean = mean(siteAreaConditioned)
                              ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,SampleSize = length(unique(CK_Cadmus_ID))
)

#summarise by region
item5.region.dat00 <- summarise(group_by(item5.dat2, BuildingType, CK_Cadmus_ID)
                              ,State = "Region"
                              ,siteAreaConditioned = sum(ConditionedArea)
)

item5.region.dat01 <- summarise(group_by(item5.region.dat00, BuildingType)
                               ,State = "Region"
                               ,HomeYearBuilt_bins = "All Vintages"
                               ,Mean = mean(siteAreaConditioned)
                               ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)

item5.tmp1 <- rbind.data.frame(item5.state.dat01, item5.region.dat01, stringsAsFactors = F) 



#### By state/region and homeyearbuilt bins
#summarise by state
item5.state.dat <- summarise(group_by(item5.dat2, BuildingType, CK_Cadmus_ID, State, HomeYearBuilt_bins)
                             ,siteAreaConditioned = sum(ConditionedArea)
)

item5.state.dat1 <- summarise(group_by(item5.state.dat, BuildingType, State, HomeYearBuilt_bins)
                              ,Mean = mean(siteAreaConditioned)
                              ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                              ,SampleSize = length(unique(CK_Cadmus_ID))
                              )

#summarise by region
item5.region.dat <- summarise(group_by(item5.dat2, BuildingType, CK_Cadmus_ID, HomeYearBuilt_bins)
                              ,State = "Region"
                              ,siteAreaConditioned = sum(ConditionedArea)
)

item5.region.dat1 <- summarise(group_by(item5.region.dat, BuildingType, HomeYearBuilt_bins)
                               ,State = "Region"
                               ,Mean = mean(siteAreaConditioned)
                               ,SE  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
                               ,SampleSize = length(unique(CK_Cadmus_ID))
)

item5.tmp2 <- rbind.data.frame(item5.state.dat1, item5.region.dat1, stringsAsFactors = F) 


item5.final <- rbind.data.frame(item5.tmp1, item5.tmp2, stringsAsFactors = F)


#############################################################################################
library(data.table)
item5.table <- dcast(setDT(item5.final)
                     ,formula = BuildingType + HomeYearBuilt_bins ~ State
                     ,value.var = c("Mean", "SE", "SampleSize"))

item5.table1 <- data.frame("BuildingType" = item5.table$BuildingType
                           ,"Housing.Vintage" = item5.table$HomeYearBuilt_bins
                           ,"Mean_MT" = item5.table$Mean_MT
                           ,"SE_MT" = item5.table$SE_MT
                           # ,"Mean_OR" = item5.table$Mean_OR
                           # ,"SE_OR" = item5.table$SE_OR
                           ,"Mean_WA" = item5.table$Mean_WA
                           ,"SE_WA" = item5.table$SE_WA
                           ,"Mean_Region" = item5.table$Mean_Region
                           ,"SE_Region" = item5.table$SE_Region
                           ,"SampleSize" = item5.table$SampleSize_Region)

item5.table2 <- item5.table1[which(item5.table1$BuildingType %in% c("Single Family", "Manufactured")),]
item5.table.final <- item5.table2[which(!(is.na(item5.table2$Housing.Vintage))),]


