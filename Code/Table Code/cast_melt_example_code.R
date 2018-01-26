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