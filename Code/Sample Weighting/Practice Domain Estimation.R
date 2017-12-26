#################################################################################
#Function: mean_one_group
#Used For: Calculating the mean where means are calculated accross 
#          multiple rows for one grouping variable
#          For example, calculating the average of a measure in each state
#################################################################################
# Test
# CustomerLevelData = item9.data
# valueVariable = 'Site_Area'
# byVariable    = 'Clean.Type'
# aggregateRow  = "All Room Types"


#weighted function for means with one grouping variable
mean_one_group <- function(CustomerLevelData, valueVariable, 
                           byVariable, aggregateRow) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  Popandns0 <- data.frame(ddply(CustomerLevelData
                                , c("BuildingType", "State", "Region", "Territory"), summarise
                                ,n_h        = unique(n.h)
                                ,N_h        = unique(N.h)), stringsAsFactors = F)
  Popandns <- data.frame(ddply(Popandns0
                               , c("BuildingType"), summarise
                               ,n_h        = sum(n_h)
                               ,N_h        = sum(N_h)), stringsAsFactors = F)
  DomainCounts    <- data.frame(ddply(CustomerLevelData #item.strata.group
                                      , c("BuildingType", "State", "Region", "Territory", byVariable), summarise
                                      # ,DomainSum = sum(get(valueVariable), na.rm = T)
                                      # ,DomainMean = mean(get(valueVariable), na.rm = T)
                                      ,strataMean = mean(get(valueVariable), na.rm = T)
                                      ,strataSD   = sd(get(valueVariable), na.rm = T)## needs updated per cluster SE
                                      ,count_hj   = sum(count)
                                      ,n_hj       = length(unique(CK_Cadmus_ID))), stringsAsFactors = F)
  item.strata <- left_join(DomainCounts, Popandns) #replaced item.strata.group with DomainCounts
  
  #QAQC
  stopifnot(item.strata$n <= item.strata$n_h)
  
  
  ######################################################
  # weighted means and SEs by grouping variables
  ######################################################
  # test.data <- item.strata[which(item.strata$BuildingType == "Single Family" & item.strata$Clean.Type == "Grow Room"),]
  
  # customerleveldata.2 <- left_join(CustomerLevelData, item.strata)
  # customerleveldata.2 put into the data spot for ddply below
  
  item.group <- data.frame(ddply(item.strata, c("BuildingType", byVariable), summarise
                                 # ,Numerator   = sum(N_h / n_h * DomainSum)
                                 # ,Denominator = sum((N_h / n_h) * n_hj)
                                 # ,Mean =  Numerator / Denominator 
                                 ,Mean = sum(N_h * strataMean) / sum(N_h)
                                 
                                 # ,Variance = 1 / Denominator^2 * sum((N_h^2 * (1 - n_h/N_h))/(n_h * (n_h)) * ((Area - DomainMean)^2 + n_hj * (1 - n_hj/n_h) * (DomainMean - Mean)^2))
                                 # ,SE = sqrt(Variance)
                                 # ,EB = SE * 1.645
                                 
                                 ,SE   = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2, na.rm = T)) / sum(unique(N_h))
                                 ,n      = sum(n_hj)
                                 ,n_h    = (unique(n_h))
                                 ,N_h    = (unique(N_h)))
                           , stringsAsFactors = F)
  
  item.region <- data.frame(ddply(item.strata, "BuildingType", summarise
                                  ,byRow  = aggregateRow
                                  ,Mean   = sum(N_h * strataMean) / sum(N_h)
                                  ,SE     = sqrt(sum((1 - n_h / N_h) * (N_h^2 / n_h) * strataSD^2, na.rm = T)) / sum(unique(N_h))
                                  ,n      = unique((n_h))
                                  ,n_h    = unique((n_h))
                                  ,N_h    = unique((N_h))
  ), stringsAsFactors = F)
  #rename columns
  colnames(item.region)[which(colnames(item.region) == 'byRow')] <- byVariable
  
  item.final <- rbind.data.frame(item.group, item.region, stringsAsFactors = F)
  
  return(item.final)
}

