#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          08/16/2018
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
source("Code/Table Code/GPM Tables/Weighting_Implementation.R")

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, "SiteOneLine.xlsx"))
rbsa.dat$CK_Cadmus_ID <- trimws(toupper(rbsa.dat$CK_SiteID))
rbsa.dat$BuildingType <- trimws(toupper(rbsa.dat$Building.Category))
rbsa.dat$Territory <- trimws(toupper(rbsa.dat$Strata.Territory))
rbsa.dat$N.h <- trimws(toupper(rbsa.dat$Strata.Population.Estimate))

#Read in data for analysis
water.dat0 <- read.xlsx(xlsxFile = file.path(filepathRawData, water.export))
water.dat0$CK_Cadmus_ID <- trimws(toupper(water.dat0$DB_SiteID))
water.dat0 <- water.dat0[,1:71]
unique(water.dat0$Fixture.Type)
water.dat0$Fixture <- water.dat0$Fixture.Type
water.dat0$Fixture[grep("shower",water.dat0$Fixture, ignore.case = T)] <- "Shower"
water.dat0$Fixture[grep("bathroom",water.dat0$Fixture, ignore.case = T)] <- "Faucet"
water.dat0$Fixture[grep("kitchen",water.dat0$Fixture, ignore.case = T)] <- "Faucet"

meas.uncertainty <- read.xlsx(xlsxFile = file.path(filepathRawData, "Measurement_Uncertainty.xlsx"))
meas.uncertainty <- meas.uncertainty[which(names(meas.uncertainty) %in% c("Weir.or.Bag"
                                                                          ,"Fixture"
                                                                          ,"Mean.Residual"
                                                                          ,"Residual.CV.old"))]

water.dat <- left_join(water.dat0, meas.uncertainty, by = c("Weir.or.Bag","Fixture"))
# water.dat$Measurement_SE <- (water.dat$Residual.CV * water.dat$Mean.Residual)
# unique(water.dat$Measurement_SE)
# water.dat[which(is.na(water.dat$Measurement_SE)),]


#############################################################################################
#Item 106: DISTRIBUTION OF SHOWERHEAD FLOW RATE BY STATE
#############################################################################################
#subset to columns needed for analysis
item106.dat <- water.dat[which(colnames(water.dat) %in% c("CK_Cadmus_ID"
                                                          ,"New.Flow.(Measured.GPM)"
                                                          ,"Fixture.Type"
                                                          ,"Residual.CV.old"
                                                          ,"Mean.Residual"
                                                          ,"Weir.or.Bag"))]
names(item106.dat)[which(names(item106.dat) == "New.Flow.(Measured.GPM)")] <- "GPM_Measured"

item106.dat1 <- left_join(item106.dat, rbsa.dat, by = "CK_Cadmus_ID")

#remove any unwanted or missing GPM values
item106.dat1$GPM_Measured <- as.numeric(as.character(item106.dat1$GPM_Measured))
item106.dat2 <- item106.dat1[which(!(is.na(item106.dat1$GPM_Measured))),]
unique(item106.dat2$GPM_Measured)

item106.dat3 <- item106.dat2[grep("bathroom",item106.dat2$Fixture.Type, ignore.case = T),]

item106.dat4 <- item106.dat3 %>% group_by(CK_Cadmus_ID, BuildingType, State, Weir.or.Bag) %>%
  summarise(GPM.Measured.Site = mean(GPM_Measured, na.rm = T)
            ,CV_Measurement = (mean(Residual.CV.old, na.rm = T)))
summary(item106.dat4$GPM.Measured.Site)
summary(item106.dat3$GPM_Measured)
summary(item106.dat4$CV_Measurement)

item106.dat4$GPM_bins <- item106.dat4$GPM.Measured.Site
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site <=  2.2)] <- "<= 2.2"
item106.dat4$GPM_bins[which(item106.dat4$GPM.Measured.Site > 2.2)] <- "> 2.2"
unique(item106.dat4$GPM_bins)

item106.merge <- left_join(rbsa.dat, item106.dat4)
item106.merge <- item106.merge[which(!is.na(item106.merge$GPM_bins)),]
item106.merge$count <- 1
item106.samplesize <- item106.merge %>%
  group_by(BuildingType, State, Territory, N.h) %>%
  summarise(n.h = sum(count))
item106.data <- left_join(item106.merge, item106.samplesize)

item106.data$n.h <- as.numeric(item106.data$n.h)
item106.data$N.h <- as.numeric(item106.data$N.h)
item106.data$weight_recalc <- item106.data$N.h / item106.data$n.h
item106.data$count <- 1

# # numerator of weighted proportion
# item106.check.num <- item106.data %>%
#   group_by(BuildingType, State, GPM_bins) %>%
#   summarise(numerator = sum(weight_recalc))
# 
# # denominator of weighted proportion
# item106.check.denom <- item106.data %>%
#   group_by(BuildingType, State) %>%
#   summarise(denominator = sum(weight_recalc))
# 
# # calculate weighted proportion
# item106.check <- item106.check.denom %>%
#   left_join(item106.check.num) %>%
#   group_by(BuildingType, State, GPM_bins) %>%
#   summarise(weighted_percent = sum(numerator / denominator))

# View(item106.check)
#######################
# Weighted Analysis
#######################
item106.final <- proportion_table_two_groups(CustomerLevelData = item106.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable   = 'State'
                                             ,rowVariable      = 'GPM_bins'
                                             ,aggregateColumnName = "Region")
# View(item106.final)


item106.final$CV_sampling <- item106.final$w.SE / item106.final$w.percent
CV_measurement1 <- item106.dat4 %>% group_by(BuildingType, State, GPM_bins) %>% summarise(CV_measurement = sqrt(mean(CV_Measurement^2)))
CV_measurement2 <- item106.dat4 %>% group_by(BuildingType, GPM_bins) %>% summarise(CV_measurement = sqrt(mean(CV_Measurement^2)))
CV_measurement2$State <- "Region"
CV_measurement  <- CV_measurement1 %>% bind_rows(CV_measurement2)

item106.final <- left_join(item106.final, CV_measurement)
item106.final$SD_measurement <- item106.final$w.percent * item106.final$CV_measurement
#UB/LB 1/2 width is EB 
item106.final$lb_measurement <- item106.final$w.percent-item106.final$SD_measurement/sqrt(item106.final$n)*1.645
item106.final$ub_measurement <- item106.final$w.percent+item106.final$SD_measurement/sqrt(item106.final$n)*1.645
item106.final$EB_measurement <- (item106.final$ub_measurement-item106.final$lb_measurement)/2

item106.final$EB_sampling <- item106.final$w.SE * qt(1-(1-.9)/2,item106.final$n)
item106.final$EB_overall <- sqrt(item106.final$EB_sampling^2 + item106.final$EB_measurement^2)
names(item106.final)

item106.cast <- dcast(setDT(item106.final)
                      , formula = BuildingType + GPM_bins ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N","EB_sampling","EB_measurement","EB_overall"))

item106.table <- data.frame("BuildingType"   = item106.cast$BuildingType
                            ,"Flow.Rate.GPM"  = item106.cast$GPM_bins
                            ,"Percent_ID"     = item106.cast$w.percent_ID
                            ,"EB_sampling_ID" = item106.cast$EB_sampling_ID
                            ,"EB_measurement_ID" = item106.cast$EB_measurement_ID
                            ,"EB_ID"          = item106.cast$EB_overall_ID
                            ,"Percent_MT"     = item106.cast$w.percent_MT
                            ,"EB_sampling_MT" = item106.cast$EB_sampling_MT
                            ,"EB_measurement_MT" = item106.cast$EB_measurement_MT
                            ,"EB_MT"          = item106.cast$EB_overall_MT
                            ,"Percent_OR"     = item106.cast$w.percent_OR
                            ,"EB_sampling_OR" = item106.cast$EB_sampling_OR
                            ,"EB_measurement_OR" = item106.cast$EB_measurement_OR
                            ,"EB_OR"          = item106.cast$EB_overall_OR
                            ,"Percent_WA"     = item106.cast$w.percent_WA
                            ,"EB_sampling_WA" = item106.cast$EB_sampling_WA
                            ,"EB_measurement_WA" = item106.cast$EB_measurement_WA
                            ,"EB_WA"          = item106.cast$EB_overall_WA
                            ,"Percent_Region" = item106.cast$w.percent_Region
                            ,"EB_sampling_Region" = item106.cast$EB_sampling_Region
                            ,"EB_measurement_Region" = item106.cast$EB_measurement_Region
                            ,"EB_Region"      = item106.cast$EB_overall_Region
                            ,"n_Region"       = item106.cast$n_Region
) 

levels(item106.table$Flow.Rate.GPM)
levels(item106.table$Flow.Rate.GPM)
rowOrder <- c("<= 2.2"
              ,"> 2.2"
              ,"Total")
item106.table <- item106.table %>% mutate(Flow.Rate.GPM = factor(Flow.Rate.GPM, levels = rowOrder)) %>% arrange(Flow.Rate.GPM)  
item106.table <- data.frame(item106.table)

item106.final.SF <- item106.table[which(item106.table$BuildingType == "SINGLE FAMILY (4 OR FEWER UNITS)")
                                  ,-which(colnames(item106.table) %in% c("BuildingType"))]
item106.final.MH <- item106.table[which(item106.table$BuildingType == "MANUFACTURED")
                                  ,-which(colnames(item106.table) %in% c("BuildingType"))]

View(item106.final.SF)
