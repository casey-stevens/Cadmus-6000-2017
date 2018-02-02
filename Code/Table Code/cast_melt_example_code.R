#cast the melt example code
item80.cast <- dcast(setDT(item80.tmp)
                     ,formula = CK_Cadmus_ID ~ Type
                     ,value.var = c("Count"))
item80.cast[is.na(item80.cast),] <- 0

item80.melt <- melt(item80.cast, id.vars = "CK_Cadmus_ID")
names(item80.melt) <- c("CK_Cadmus_ID", "Type", "Count")


# row ordering example code
####################################
# Update this code Row Variable = Washer.Type in this example (all Washer.Type will need to be updated)
# Update row order to make match with previous or at least have total row at bottom
# If final table have <NA> something was named incorrectly
levels(item87.table$Washer.Type)
rowOrder <- c("Pre 1955"
              ,"1955-1970"
              ,"1971-1980"
              ,"1981-1990"
              ,"1991-2000"
              ,"2001-2010"
              ,"Post 2010"
              ,"All Vintages")
item87.table <- item87.table %>% mutate(Washer.Type = factor(Washer.Type, levels = rowOrder)) %>% arrange(Washer.Type)  
item87.table <- data.frame(item87.table)



rowOrder <- c("Combined Washer/Dryer in one drum"
              ,"Horizontal Axis"
              ,"Stacked Washer/Dryer"
              ,"Vertical Axis (with agitator)"
              ,"Vertical Axis (without agitator)"
              ,"Unknown"
              ,"Total")

rowOrder <- c("Apartment Building (3 or fewer floors)"
              ,"Apartment Building (4 to 6 floors)"
              ,"Apartment Building (More than 6 floors)"
              ,"All Sizes")


# row ordering example code
levels(item1.table$Home.Type)
rowOrder <- c("Single Family Detached"
              ,"Duplex, Triplex, or Fourplex"
              ,"Townhome or Rowhome"
              ,"Apartment Building (3 or fewer floors)"
              ,"Apartment Building (4 to 6 floors)"
              ,"Apartment Building (More than 6 floors)"
              ,"Single Wide"
              ,"Double Wide"
              ,"Triple Wide"
              ,"Modular / Prefab"
              ,"Total")
item1.table <- item1.table %>% mutate(Home.Type = factor(Home.Type, levels = rowOrder)) %>% arrange(Home.Type)  
item1.table <- data.frame(item1.table)



# Some more row orderings
rowOrder <- c("Metal Single Glazed"
              ,"Metal Double Glazed"
              ,"Metal Triple Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Single Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Double Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Triple Glazed"
              ,"Other Double Glazed" 
              ,"All Framing Categories"
)


rowOrder <- c("Pre 1951"
              ,"1951-1960"
              ,"1961-1970"
              ,"1971-1980"
              ,"1981-1985"
              ,"1986-1990"
              ,"1991-1995"
              ,"1996-2000"
              ,"2001-2005"
              ,"2006-2010"
              ,"Post 2010"
              ,"All Vintages")


rowOrder <- c("1-50%"
              ,"51-99%"
              ,"100%"
              ,"None"
              ,"Total")


rowOrder <- c("Bathroom"
              ,"Bedroom"
              ,"Closet"
              ,"Dining Room"
              ,"Family Room"
              ,"Garage"
              ,"Hall"
              ,"Kitchen"
              ,"Laundry"
              ,"Living Room"
              ,"Office"
              ,"Other"
              ,"Outside"
              ,"All Room Types")




for (i in 1:5){
  item224.dat[,i] <- as.numeric(as.character(item224.dat[,i]))
}
item224.dat[is.na(item224.dat)] <- 0







# ,"EB_ID"          = item106.cast$EB_ID
# ,"EB_MT"          = item106.cast$EB_MT
# ,"EB_OR"          = item106.cast$EB_OR
# ,"EB_WA"          = item106.cast$EB_WA
# ,"EB_Region"      = item106.cast$EB_Region








# For any primary or secondary heating systems
itemXX.dat2$Generic[grep("Electric Baseboard",itemXX.dat2$Generic,ignore.case = T)] <- "Electric Baseboard and Wall Heaters"
itemXX.dat2$Generic[grep("zonal heat",itemXX.dat2$Generic,ignore.case = T)] <- "Other Zonal Heat"
itemXX.dat2$Generic[grep("ductless",itemXX.dat2$Generic,ignore.case = T)] <- "Mini-split HP"
itemXX.dat2$Generic[grep("furnace",itemXX.dat2$Generic,ignore.case = T)] <- "Furnace"
itemXX.dat2$Generic[grep("boiler",itemXX.dat2$Generic,ignore.case = T)] <- "Boiler"
itemXX.dat2$Generic[grep("Stove/Fireplace",itemXX.dat2$Generic,ignore.case = T)] <- "Stove/Fireplace"







item1.table <- data.frame("BuildingType"          = item1.cast$BuildingType
                          ,"Home.Type"            = item1.cast$HomeType
                          ,"Percent_2017.RBSA.PS" = item1.cast$`w.percent_2017 RBSA PS`
                          ,"SE_2017.RBSA.PS"      = item1.cast$`w.SE_2017 RBSA PS`
                          ,"n_2017.RBSA.PS"       = item1.cast$`n_2017 RBSA PS`
                          ,"Percent_SCL.GenPop"   = item1.cast$`w.percent_SCL GenPop`
                          ,"SE_SCL.GenPop"        = item1.cast$`w.SE_SCL GenPop`
                          ,"n_SCL.GenPop"         = item1.cast$`n_SCL GenPop`
                          ,"Percent_SCL.LI"       = item1.cast$`w.percent_SCL LI`
                          ,"SE_SCL.LI"            = item1.cast$`w.SE_SCL LI`
                          ,"n_SCL.LI"             = item1.cast$`n_SCL LI`
                          ,"Percent_SCL.EH"       = item1.cast$`w.percent_SCL EH`
                          ,"SE_SCL.EH"            = item1.cast$`w.SE_SCL EH`
                          ,"n_SCL.EH"             = item1.cast$`n_SCL EH`
                          ,"EB_2017.RBSA.PS"      = item1.cast$`EB_2017 RBSA PS`
                          ,"EB_SCL.GenPop"        = item1.cast$`EB_SCL GenPop`
                          ,"EB_SCL.LI"            = item1.cast$`EB_SCL LI`
                          ,"EB_SCL.EH"            = item1.cast$`EB_SCL EH`)


#can add pop and sample sizes if needed in exported table
item2.os.table <- data.frame("BuildingType"          = item2.os.cast$BuildingType
                             ,"Home.Type"            = item2.os.cast$HomeType
                             ,"Percent_2017.RBSA.PS" = item2.os.cast$`Percent_2017 RBSA PS`
                             ,"SE_2017.RBSA.PS"      = item2.os.cast$`SE_2017 RBSA PS`
                             ,"n_2017.RBSA.PS"       = item2.os.cast$`n_2017 RBSA PS`
                             ,"Percent_SCL.GenPop"   = item2.os.cast$`Percent_SCL GenPop`
                             ,"SE_SCL.GenPop"        = item2.os.cast$`SE_SCL GenPop`
                             ,"n_SCL.GenPop"         = item2.os.cast$`n_SCL GenPop`
                             ,"Percent_SCL.LI"       = item2.os.cast$`Percent_SCL LI`
                             ,"SE_SCL.LI"            = item2.os.cast$`SE_SCL LI`
                             ,"n_SCL.LI"             = item2.os.cast$`n_SCL LI`
                             ,"Percent_SCL.EH"       = item2.os.cast$`Percent_SCL EH`
                             ,"SE_SCL.EH"            = item2.os.cast$`SE_SCL EH`
                             ,"n_SCL.EH"             = item2.os.cast$`n_SCL EH`)
