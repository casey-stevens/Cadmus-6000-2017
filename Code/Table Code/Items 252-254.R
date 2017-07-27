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
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
#Item 252: DISTRIBUTION OF DHW SERVICE TYPE BY BUILDING SIZE (MF Table 44)
#############################################################################################
#subset to columns needed for analysis
item252.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"System.Type"
                                                                    ,"Serves.Common.Areas?"
                                                                    ,"Serves.Residences?"))]
item252.dat$count <- 1

item252.dat0 <- item252.dat[which(item252.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item252.dat1 <- left_join(item252.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#Subset to Multifamily
item252.dat2 <- item252.dat1[grep("Multifamily", item252.dat1$BuildingType),]

item252.dat3 <- item252.dat2[grep("Water Heat", item252.dat2$System.Type),]
item252.dat3$DHW.Location <- item252.dat3$CK_SiteID
item252.dat3$DHW.Location[grep("BLDG",item252.dat3$CK_SiteID)] <- "Central Water Heater"
item252.dat3$DHW.Location[grep("SITE",item252.dat3$CK_SiteID)] <- "In-Unit Water Heater"
unique(item252.dat3$DHW.Location)













#############################################################################################
#Item 253: DISTRIBUTION OF CENTRAL DHW SYSTEMS BY FUEL TYPE (MF Table 45)
#############################################################################################
#subset to columns needed for analysis
item253.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"System.Type"
                                                                    ,"DHW.Provided.by.Heating.System"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Type"                                                                     
                                                                    ,"DHW.Technology"                                                                  
                                                                    ,"DHW.Size.(Gallons)"                                                  
                                                                    ,"DHW.Provided.by.Heating.System"                                      
                                                                    ,"DHW.Capacity"                                                                    
                                                                    ,"DHW.Capacity.Units"                                                              
                                                                    ,"DHW.Energy.Factor"                                                               
                                                                    ,"DHW.HPWH.in.Conditioned.Space"                                                   
                                                                    ,"DHW.HPWH.Conditioned.Space.Type"                                                 
                                                                    ,"DHW.HPWH.Conditioned.Space.Size"                                                 
                                                                    ,"DHW.HPWH.Relation.to.Space.Heating.Unit"                                         
                                                                    ,"DHW.HPWH.Ducting"                                                                
                                                                    ,"DHW.Location"                                                                    
                                                                    ,"DHW.Serves.Whole.House?"))]
item253.dat$count <- 1

item253.dat0 <- item253.dat[which(item253.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item253.dat1 <- left_join(item253.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#Subset to Multifamily
item253.dat2 <- item253.dat1[grep("Multifamily", item253.dat1$BuildingType),]

item253.dat3 <- item253.dat2[grep("Water Heat|Multiple|Boiler|Tank|Other", item253.dat2$System.Type),]
item253.dat3$DHW.Location <- item253.dat3$CK_SiteID
item253.dat3$DHW.Location[grep("BLDG",item253.dat3$CK_SiteID)] <- "Central Water Heater"
item253.dat3$DHW.Location[grep("SITE",item253.dat3$CK_SiteID)] <- "In-Unit Water Heater"
unique(item253.dat3$DHW.Location)

item253.dat4 <- item253.dat3[which(item253.dat3$DHW.Location == "Central Water Heater"),]





























#############################################################################################
#Item 254: DISTRIBUTION OF CENTRAL DHW SYSTEMS BY FUEL TYPE (MF Table 45)
#############################################################################################
#subset to columns needed for analysis
item254.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"System.Type"
                                                                    ,"DHW.Provided.by.Heating.System"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Type"                                                                     
                                                                    ,"DHW.Technology"                                                                  
                                                                    ,"DHW.Size.(Gallons)"                                                  
                                                                    ,"DHW.Provided.by.Heating.System"                                      
                                                                    ,"DHW.Capacity"                                                                    
                                                                    ,"DHW.Capacity.Units"                                                              
                                                                    ,"DHW.Energy.Factor"                                                               
                                                                    ,"DHW.HPWH.in.Conditioned.Space"                                                   
                                                                    ,"DHW.HPWH.Conditioned.Space.Type"                                                 
                                                                    ,"DHW.HPWH.Conditioned.Space.Size"                                                 
                                                                    ,"DHW.HPWH.Relation.to.Space.Heating.Unit"                                         
                                                                    ,"DHW.HPWH.Ducting"                                                                
                                                                    ,"DHW.Location"                                                                    
                                                                    ,"DHW.Serves.Whole.House?"))]
item254.dat$count <- 1

item254.dat0 <- item254.dat[which(item254.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item254.dat1 <- left_join(item254.dat0, rbsa.dat, by = "CK_Cadmus_ID")

#Subset to Multifamily
item254.dat2 <- item254.dat1[grep("Multifamily", item254.dat1$BuildingType),]

item254.dat3 <- item254.dat2[grep("Water Heat|Multiple|Boiler|Tank|Other", item254.dat2$System.Type),]
item254.dat3$DHW.Location <- item254.dat3$CK_SiteID
item254.dat3$DHW.Location[grep("BLDG",item254.dat3$CK_SiteID)] <- "Central Water Heater"
item254.dat3$DHW.Location[grep("SITE",item254.dat3$CK_SiteID)] <- "In-Unit Water Heater"
unique(item254.dat3$DHW.Location)

item254.dat4 <- item254.dat3[which(item254.dat3$DHW.Location == "In-Unit Water Heater"),]
