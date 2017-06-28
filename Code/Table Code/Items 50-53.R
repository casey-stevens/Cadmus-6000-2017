#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))
#subset to columns needed for analysis
mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"Heating.Fuel"
                                                                        ,"Heating.Efficiency.-.High"
                                                                        ,"Component.1.Year.of.Manufacture"
                                                                        ,"HSPF"))]
#fix capitalization error
mechanical.dat1$Heating.Fuel[which(mechanical.dat1$Heating.Fuel == "Natural gas")] <- "Natural Gas"
#remove any irrelevant equipment vintages (datapoint not asked for)
mechanical.dat2 <- mechanical.dat1[which(mechanical.dat1$Component.1.Year.of.Manufacture != "-- Datapoint not asked for --"),]
#remove any NA equipment vintages
mechanical.dat3 <- mechanical.dat2[which(!(is.na(mechanical.dat2$Component.1.Year.of.Manufacture))),]

# Bin equipment vintages for items 50 and 52 (4 categories)
mechanical.dat3$EquipVintage_bins <- as.numeric(as.character(mechanical.dat3$`Component.1.Year.of.Manufacture`))
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` < 1990)] <- "Pre 1990"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 1990 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2000)] <- "1990-1999"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2000 & mechanical.dat3$`Component.1.Year.of.Manufacture` < 2006)] <- "2000-2006"
mechanical.dat3$EquipVintage_bins[which(mechanical.dat3$`Component.1.Year.of.Manufacture` >= 2006)] <- "Post 2006"
#check uniques
unique(mechanical.dat3$EquipVintage_bins)




#############################################################################################
#Item 50: AVERAGE GAS FURNACE EFFICIENCY (AFUE) BY EQUIPMENT VINTAGE AND STATE (SF table 57, MH table 38)
#############################################################################################
#data for item 50
item50.dat <- mechanical.dat3

#remove any irrelevant heating efficiency datapoints (datapoint not asked for)
item50.dat1 <- item50.dat[which(item50.dat$`Heating.Efficiency.-.High` != "-- Datapoint not asked for --"),]

#remove any repeated header lines
item50.dat2 <- item50.dat1[which(item50.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Fix any issues with heating efficiency bins (? = unknown)
item50.dat2$`Heating.Efficiency.-.High`[which(item50.dat2$`Heating.Efficiency.-.High` == "?")] <- "Unknown"
#check that we are left with only quantities we want for heating efficiency
unique(item50.dat2$`Heating.Efficiency.-.High`)

#remove unknown heating efficiencies for now -- Rietz might want to note how many are unknown in future
item50.dat3 <- item50.dat2[which(item50.dat2$`Heating.Efficiency.-.High` != "Unknown"),]

#make heating efficiency information numeric
item50.dat3$`Heating.Efficiency.-.High` <- as.numeric(as.character(item50.dat3$`Heating.Efficiency.-.High`))

#Join cleaned item 50 mechanical information with cleaned RBSA site information
item50.dat4 <- left_join(item50.dat3, rbsa.dat, by = "CK_Cadmus_ID")

#Calculate appropriate sample size information:
#Sample sizes by building type, state, and equipment vintage
SampleSize1 <- summarise(group_by(item50.dat4, BuildingType, EquipVintage_bins, State)
                        ,SampleSize = length(unique(CK_Cadmus_ID)))
#sample sizes by building type and equipment vintage for the region (i.e. across states)
SampleSize2 <- summarise(group_by(item50.dat4, BuildingType, EquipVintage_bins)
                         ,State = "Region"
                         ,SampleSize = length(unique(CK_Cadmus_ID)))
#sample sizes by building type and state for all vintages (i.e. across equipment vintages)
SampleSize3 <- summarise(group_by(item50.dat4, BuildingType, State)
                         ,EquipVintage_bins = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID)))
#sample sizes by building type for the region and all vintages (i.e. across states and equipment vintages)
SampleSize4 <- summarise(group_by(item50.dat4, BuildingType)
                         ,EquipVintage_bins = "All Vintages"
                         ,State = "Region"
                         ,SampleSize = length(unique(CK_Cadmus_ID)))
#row bind all sample sizes together for a merge later
SampleSizes <- rbind.data.frame(SampleSize1, SampleSize2, SampleSize3, SampleSize4, stringsAsFactors = F)


#Calculate state and region level average heating efficiency and standard deviation by equipment vintage bins
#average heating efficiency and SD by building type, equipment vintage, and state
item50.sum <- summarise(group_by(item50.dat4, BuildingType, EquipVintage_bins, State)
                        ,AFUEaverage = mean(`Heating.Efficiency.-.High`)
                        ,SD = sd(`Heating.Efficiency.-.High`))
#average heating efficiency and SD by building type, equipment vintage for the region (i.e. across states)
item50.sum1 <- summarise(group_by(item50.dat4, BuildingType, EquipVintage_bins)
                         ,State = "Region"
                        ,AFUEaverage = mean(`Heating.Efficiency.-.High`)
                        ,SD = sd(`Heating.Efficiency.-.High`))
#row bind average heating efficiency and SD information by building type and equipment vintage for state and region
item50.merge1 <- rbind.data.frame(item50.sum, item50.sum1, stringsAsFactors = F)

#Calculate state and region level average heating efficiency and standard deviation for all vintages (i.e. across all equip vintage bins)
#average heating efficiency and SD by building type and state for all vintages
item50.sum2 <- summarise(group_by(item50.dat4, BuildingType, State)
                         ,EquipVintage_bins = "All Vintages"
                        ,AFUEaverage = mean(`Heating.Efficiency.-.High`)
                        ,SD = sd(`Heating.Efficiency.-.High`))
#average heating efficiency and SD by building type for the region and for all vintages (i.e. across states)
item50.sum3 <- summarise(group_by(item50.dat4, BuildingType)
                         ,State = "Region"
                         ,EquipVintage_bins = "All Vintages"
                         ,AFUEaverage = mean(`Heating.Efficiency.-.High`)
                         ,SD = sd(`Heating.Efficiency.-.High`))
#row bind average heating efficiency and SD information by building type for state and region for all vintages 
item50.merge2 <- rbind.data.frame(item50.sum2, item50.sum3, stringsAsFactors = F)

#row bind heating efficiency and SD info by building by and across vintages, and by and across states
item50.merge.final <- rbind.data.frame(item50.merge1, item50.merge2, stringsAsFactors = F) 

#join sample sizes onto heating efficiency and SD information
item50.final <- left_join(item50.merge.final, SampleSizes, by = c("BuildingType","State","EquipVintage_bins"))
#calculate SE = SD / sqrt(SampleSize)
item50.final$SE <- item50.final$SD / sqrt(item50.final$SampleSize)

#subset to only the columns needed for the final RBSA table
item50.final1 <- item50.final[which(colnames(item50.final) %in% c("BuildingType"
                                                                  ,"EquipVintage_bins"
                                                                  ,"State"
                                                                  ,"AFUEaverage"
                                                                  ,"SampleSize"
                                                                  ,"SE"))]
#need this library for casting data
library(data.table)

#cast data into correct table format
item50.table <- dcast(setDT(item50.final1)
                       , formula = BuildingType + EquipVintage_bins ~ State
                       , value.var = c("AFUEaverage", "SE", "SampleSize"))

#subset to only the relevant building types for this item
item50.table1 <- item50.table[which(item50.table$BuildingType %in% c("Single Family", "Manufactured")),]







#############################################################################################
#Item 51: DISTRIBUTION OF GAS FURNACE EFFICIENCY (AFUE) BY STATE (SF table 58)
#############################################################################################
#data for item 51
item51.dat <- mechanical.dat1

#remove any irrelevant heating efficiency datapoints (datapoint not asked for)
item51.dat1 <- item51.dat[which(item51.dat$`Heating.Efficiency.-.High` != "-- Datapoint not asked for --"),]

#remove any repeated header lines
item51.dat2 <- item51.dat1[which(item51.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Fix any issues with heating efficiency bins (? = unknown)
item51.dat2$`Heating.Efficiency.-.High`[which(item51.dat2$`Heating.Efficiency.-.High` == "?")] <- "Unknown"
#check that we are left with only quantities we want for heating efficiency
unique(item51.dat2$`Heating.Efficiency.-.High`)

#remove unknown heating efficiencies for now -- Rietz might want to note how many are unknown in future
item51.dat3 <- item51.dat2[which(item51.dat2$`Heating.Efficiency.-.High` != "Unknown"),]

#make heating efficiency information numeric
item51.dat3$`Heating.Efficiency.-.High` <- as.numeric(as.character(item51.dat3$`Heating.Efficiency.-.High`))

#Join cleaned item 50 mechanical information with cleaned RBSA site information
item51.dat4 <- left_join(item51.dat3, rbsa.dat, by = "CK_Cadmus_ID")

# Create heating efficiency bins
item51.dat4$Efficiency_bins <- as.numeric(as.character(item51.dat4$`Heating.Efficiency.-.High`))
item51.dat4$Efficiency_bins[which(item51.dat4$`Heating.Efficiency.-.High` < 80)] <- "< 80%"
item51.dat4$Efficiency_bins[which(item51.dat4$`Heating.Efficiency.-.High` >= 80 & item51.dat4$`Heating.Efficiency.-.High` < 90)] <- "80-89%"
item51.dat4$Efficiency_bins[which(item51.dat4$`Heating.Efficiency.-.High` >= 90 & item51.dat4$`Heating.Efficiency.-.High` < 94)] <- "90-94%"
item51.dat4$Efficiency_bins[which(item51.dat4$`Heating.Efficiency.-.High` >= 94)] <- "> 94%"
#check that efficiency bins are what we expect/want
unique(item51.dat4$Efficiency_bins)
#add count
item51.dat4$count <- 1

#Calculate appropriate sample size information:
#Sample sizes by building type, state, and Heating efficiency bins
SampleSize1 <- summarise(group_by(item51.dat4, BuildingType, Efficiency_bins, State)
                         ,SampleSize = length(unique(CK_Cadmus_ID)))
#sample sizes by building type and efficiency for the region (i.e. across states)
SampleSize2 <- summarise(group_by(item51.dat4, BuildingType, Efficiency_bins)
                         ,State = "Region"
                         ,SampleSize = length(unique(CK_Cadmus_ID)))
#sample sizes by building type and state for all efficiencies (i.e. across efficiency bins)
SampleSize3 <- summarise(group_by(item51.dat4, BuildingType, State)
                         ,Efficiency_bins = "All Efficiencies"
                         ,SampleSize = length(unique(CK_Cadmus_ID)))
#sample sizes by building type for the region and all efficiencies (i.e. across states and efficiency bins)
SampleSize4 <- summarise(group_by(item51.dat4, BuildingType)
                         ,Efficiency_bins = "All Efficiencies"
                         ,State = "Region"
                         ,SampleSize = length(unique(CK_Cadmus_ID)))
#row bind all sample sizes together for a merge later
SampleSizes <- rbind.data.frame(SampleSize1, SampleSize2, SampleSize3, SampleSize4, stringsAsFactors = F)

#Calculate total counts:
# by building type and state for all efficiencies (i.e. across efficiency bins)
TotalCount1 <- summarise(group_by(item51.dat4, BuildingType, State)
                         ,TotalCount = sum(count))
# by building types for the region and for all efficiencies (i.e. across states and efficiency bins)
TotalCount2 <- summarise(group_by(item51.dat4, BuildingType)
                         ,State = "Region"
                         ,TotalCount = sum(count))
#row bind total counts together for a merge later -- NOTE: this could be done in the same step with sample sizes
# however, I thought it would make it very clear to have everything explicitly named
TotalCount <- rbind.data.frame(TotalCount1, TotalCount2, stringsAsFactors = F)



#Calculate state and region level counts by efficiency bins
#efficiency count by building type, efficiency bins, and state
item51.sum <- summarise(group_by(item51.dat4, BuildingType, Efficiency_bins, State)
                        ,Count = sum(count))
#efficiency count by building type and efficiency bins for the region (i.e.across states)
item51.sum1 <- summarise(group_by(item51.dat4, BuildingType, Efficiency_bins)
                         ,State = "Region"
                         ,Count = sum(count))
#row bind efficiency counts by building type and efficiency bins for state and region
item51.merge1 <- rbind.data.frame(item51.sum, item51.sum1, stringsAsFactors = F)

#Calculate state and region level counts across efficiency bins
#efficiency count by building type and state for all efficiencies
item51.sum2 <- summarise(group_by(item51.dat4, BuildingType, State)
                         ,Efficiency_bins = "All Efficiencies"
                         ,Count = sum(count))
#efficiency count by building type for the region and for all efficiencies
item51.sum3 <- summarise(group_by(item51.dat4, BuildingType)
                         ,State = "Region"
                         ,Efficiency_bins = "All Efficiencies"
                         ,Count = sum(count))
#row bind efficiency counts by building type across efficiency bins for state and region
item51.merge2 <- rbind.data.frame(item51.sum2, item51.sum3, stringsAsFactors = F)

#row bins efficiency counts by building types by and across efficiency bins for state and region
item51.merge.final <- rbind.data.frame(item51.merge1, item51.merge2, stringsAsFactors = F) 

#merge sample sizes onto final efficiency counts
item51.final <- left_join(item51.merge.final, SampleSizes, by = c("BuildingType","State","Efficiency_bins"))
#merge total counts onto final efficiency counts + sample sizes
item51.final1 <- left_join(item51.final, TotalCount, by = c("BuildingType","State"))

#calculate the percent of heating efficiency counts in each bin
item51.final1$Percent <- item51.final1$Count / item51.final1$TotalCount
#calcualte the standard error around a proportion (percent)
item51.final1$SE <- sqrt(item51.final1$Percent * (1 - item51.final1$Percent) / item51.final1$SampleSize)


#subset to only the columns needed for the final RBSA table
item51.final2 <- item51.final1[which(colnames(item51.final1) %in% c("BuildingType"
                                                                  ,"Efficiency_bins"
                                                                  ,"State"
                                                                  ,"Percent"
                                                                  ,"SE"
                                                                  ,"SampleSize"))]
#Need this library for casting
library(data.table)

#cast data into correct table format
item51.table <- dcast(setDT(item51.final1)
                      , formula = BuildingType + Efficiency_bins ~ State
                      , value.var = c("Percent", "SE", "SampleSize"))

#subset to only the relevant building types for this item
item51.table1 <- item51.table[which(item51.table$BuildingType == "Single Family"),]






#############################################################################################
#Item 52: AVERAGE AIR SOURCE HEAT PUMP EFFICIENCY (HSPF) BY EQUIPMENT VINTAGE (SF table 59, MH table 39)
#############################################################################################
#data for item 51
item52.dat <- mechanical.dat3

#remove any irrelevant HSPF datapoints (could not collect)
item52.dat1 <- item52.dat[which(item52.dat$HSPF != "Could Not Collect"),]

#remove any repeated header lines
item52.dat2 <- item52.dat1[which(item52.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#remove any NAs in HSPF
item52.dat3 <- item52.dat2[which(!(is.na(item52.dat2$HSPF))),]

#make HSPF information numeric
item52.dat3$HSPF <- as.numeric(as.character(item52.dat3$HSPF))

#Join cleaned item 52 mechanical information with cleaned RBSA site information
item52.dat4 <- left_join(item52.dat3, rbsa.dat, by = "CK_Cadmus_ID")

#Calculate state and region level HSPF mean and SD by equip vintage bins:
#average HSPF and standard deviation by building type, equipment vintage, 
item52.sum <- summarise(group_by(item52.dat4, BuildingType, EquipVintage_bins)
                        ,SampleSize = length(unique(CK_Cadmus_ID))
                        ,Mean = mean(HSPF)
                        ,SD = sd(HSPF))

#Calculate state and region level counts across equipment bins
#HSPF means and SDs by building type  for all vintages
item52.sum1 <- summarise(group_by(item52.dat4, BuildingType)
                         ,EquipVintage_bins = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Mean = mean(HSPF)
                         ,SD = sd(HSPF))
#row bind HSPF means and SDs by building type across equipment bins for state and region
item52.final <- rbind.data.frame(item52.sum, item52.sum1, stringsAsFactors = F)

#calcuate SE = SD/sqrt(samplesize)
item52.final$SE <- item52.final$SD / sqrt(item52.final$SampleSize)

#subset to only the columns needed for the final RBSA table
item52.final1 <- item52.final[which(colnames(item52.final) %in% c("BuildingType"
                                                                  ,"EquipVintage_bins"
                                                                  ,"Mean"
                                                                  ,"SampleSize"
                                                                  ,"SE"))]
#subset to only relevant building types for this item
item52.table <- item52.final1[which(item52.final1$BuildingType %in% c("Single Family", "Manufactured")),]





#############################################################################################
#Item 53: DISTRIBUTION OF AIR SOURCE HEAT PUMP EFFICIENCY (HSPF) BY STATE (SF table 60)
#############################################################################################
#data for item 53
item53.dat <- mechanical.dat1

#remove any irrelevant HSPF datapoints (could not collect)
item53.dat1 <- item53.dat[which(item53.dat$HSPF != "Could Not Collect"),]

#remove any repeated header lines
item53.dat2 <- item53.dat1[which(item53.dat1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#make HSPF information numeric
item53.dat2$HSPF <- as.numeric(as.character(item53.dat2$HSPF))

#Join cleaned item 53 mechanical information with cleaned RBSA site information
item53.dat3 <- left_join(item53.dat2, rbsa.dat, by = "CK_Cadmus_ID")

# Create HSPF_bins according to previous RBSA bins
item53.dat3$HSPF_bins <- as.numeric(as.character(item53.dat3$HSPF))
item53.dat3$HSPF_bins[which(item53.dat3$HSPF >= 6.8 & item53.dat3$HSPF < 7.7)] <- "6.8-7.6"
item53.dat3$HSPF_bins[which(item53.dat3$HSPF >= 7.7 & item53.dat3$HSPF < 8.3)] <- "7.7-8.2"
item53.dat3$HSPF_bins[which(item53.dat3$HSPF >= 8.3 & item53.dat3$HSPF < 9.0)] <- "8.3-8.9"
item53.dat3$HSPF_bins[which(item53.dat3$HSPF >= 9.0)] <- "9.0+"
#make sure that the HSPF bins are what we want/expect
unique(item53.dat3$HSPF_bins)
#add count
item53.dat3$count <- 1


#Calculate appropriate sample size information:
#Sample sizes by building type, state, and HSPF bins
SampleSize1 <- summarise(group_by(item53.dat3, BuildingType, HSPF_bins, State)
                         ,SampleSize = length(unique(CK_Cadmus_ID)))
#sample sizes by building type and HSPF for the region (i.e. across states)
SampleSize2 <- summarise(group_by(item53.dat3, BuildingType, HSPF_bins)
                         ,State = "Region"
                         ,SampleSize = length(unique(CK_Cadmus_ID)))
#sample sizes by building type and state for all HSPFs (i.e. across HSPF bins)
SampleSize3 <- summarise(group_by(item53.dat3, BuildingType, State)
                         ,HSPF_bins = "All HSPF Values"
                         ,SampleSize = length(unique(CK_Cadmus_ID)))
#sample sizes by building type for the region and all HSPFs (i.e. across states and HSPF bins)
SampleSize4 <- summarise(group_by(item53.dat3, BuildingType)
                         ,HSPF_bins = "All HSPF Values"
                         ,State = "Region"
                         ,SampleSize = length(unique(CK_Cadmus_ID)))
#row bind all sample sizes together for a merge later
SampleSizes <- rbind.data.frame(SampleSize1, SampleSize2, SampleSize3, SampleSize4, stringsAsFactors = F)


#Calculate total count information:
#Total counts by building type and state across HSPF bins
TotalCount1 <- summarise(group_by(item53.dat3, BuildingType, State)
                         ,TotalCount = sum(count))
#Total counts by building type for the region across HSPF bins
TotalCount2 <- summarise(group_by(item53.dat3, BuildingType)
                         ,State = "Region"
                         ,TotalCount = sum(count))
#row bind all total counts together for a merge later
TotalCount <- rbind.data.frame(TotalCount1, TotalCount2, stringsAsFactors = F)


#Calculate state and region level HSPF counts by HSPF bins:
#HSPF counts by building type, equipment vintage, and state
item53.sum <- summarise(group_by(item53.dat3, BuildingType, HSPF_bins, State)
                        ,Count = sum(count))
#HSPF counts by building type, equipment vintage for the region
item53.sum1 <- summarise(group_by(item53.dat3, BuildingType, HSPF_bins)
                         ,State = "Region"
                         ,Count = sum(count))
#row bind HSPF counts by buildign type and equip vintage for states and the region
item53.merge1 <- rbind.data.frame(item53.sum, item53.sum1, stringsAsFactors = F)

#Calculate state and region level HSPF counts across HSPF bins:
#HSPF counts by building type and state for all HSPF values
item53.sum2 <- summarise(group_by(item53.dat3, BuildingType, State)
                         ,HSPF_bins = "All HSPF Values"
                         ,Count = sum(count))
#HSPF counts by building type for all HSPF values for the region
item53.sum3 <- summarise(group_by(item53.dat3, BuildingType)
                         ,State = "Region"
                         ,HSPF_bins = "All HSPF Values"
                         ,Count = sum(count))
#row bind HSPF counts by building types across equip vintages, for states and the region
item53.merge2 <- rbind.data.frame(item53.sum2, item53.sum3, stringsAsFactors = F)

#row bind HSPF counts by building types, by and across equip vintages for state and region
item53.merge.final <- rbind.data.frame(item53.merge1, item53.merge2, stringsAsFactors = F) 

#join sample size information onto final HSPF count information
item53.final <- left_join(item53.merge.final, SampleSizes, by = c("BuildingType","State","HSPF_bins"))
#join total count information onto final HSPF count and sample size information
item53.final1 <- left_join(item53.final, TotalCount, by = c("BuildingType","State"))

#calculate percent as the bin count divided by total count across bins
item53.final1$Percent <- item53.final1$Count / item53.final1$TotalCount
# calculate SE for proportion (percent)
item53.final1$SE <- sqrt(item53.final1$Percent * (1 - item53.final1$Percent) / item53.final1$SampleSize)

#subset to only the columns needed for the final RBSA table
item53.final2 <- item53.final1[which(colnames(item53.final1) %in% c("BuildingType"
                                                                    ,"HSPF_bins"
                                                                    ,"State"
                                                                    ,"Percent"
                                                                    ,"SE"
                                                                    ,"SampleSize"))]
library(data.table)
#cast data into correct table format
item53.table <- dcast(setDT(item53.final1)
                      , formula = BuildingType + HSPF_bins ~ State
                      , value.var = c("Percent", "SE", "SampleSize"))

#subset to building types relevant to this item
item53.table1 <- item53.table[which(item53.table$BuildingType %in% c("Manufactured","Single Family")),]

