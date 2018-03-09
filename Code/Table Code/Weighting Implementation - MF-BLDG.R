#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Andres Roma, Cadmus Group               
##  Created:          10/10/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

#################################################################################################################
#Function: mean_one_group
#Used For: Calculating the mean where means are calculated accross 
#          multiple rows for one grouping variable
#          For example, calculating the average of a measure in each state
#################################################################################################################

#weighted function for means with one grouping variable
mean_one_group <- function(CustomerLevelData, valueVariable, 
                                    byVariable, aggregateRow) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  Popandns <- data.frame(ddply(CustomerLevelData
                               , c("BuildingType", "State", "Region", "Territory"), summarise
                               ,n_h        = unique(n.h)
                               ,N_h        = sum(unique(N.h))), stringsAsFactors = F)

  if(byVariable %in% c("State","BuildingType")){
    
    item.strata <- data.frame(ddply(CustomerLevelData
                                    , c("BuildingType", "State", "Region", "Territory"), summarise
                              ,strataMean = mean(get(valueVariable), na.rm = T)
                              ,strataSD   = sd(get(valueVariable), na.rm = T)
                              ,n_hj       = length(unique(CK_Building_ID))
                              ,n_h        = sum(n.h)), stringsAsFactors = F)
    #QAQC
    stopifnot(item.strata$n_hj == item.strata$n_h)
    item.strata <- left_join(item.strata, Popandns)
  }else if(byVariable == "HomeType"){
    byVariableCounts    <- data.frame(ddply(CustomerLevelData #item.strata.group
                                    , c("BuildingType", "State", "Region", "Territory", byVariable), summarise
                                    ,strataMean = mean(get(valueVariable), na.rm = T)
                                    ,strataSD   = sd(get(valueVariable), na.rm = T)
                                    ,count_hj   = sum(count)
                                    ,n_hj       = length(unique(CK_Building_ID))
                                    # ,n.h        = sum(n.h)
                                    ), stringsAsFactors = F)
    item.strata <- left_join(byVariableCounts, Popandns) #replaced item.strata.group with byVariableCounts
    item.strata$N_h <- item.strata$N_h * item.strata$n_hj / item.strata$n_h
    #QAQC
    stopifnot(item.strata$n <= item.strata$n_h)
  }else {
    byVariableCounts    <- data.frame(ddply(CustomerLevelData #item.strata.group
                                            , c("BuildingType", "State", "Region", "Territory", byVariable), summarise
                                            ,strataMean = mean(get(valueVariable), na.rm = T)
                                            ,strataSD   = sd(get(valueVariable), na.rm = T)
                                            ,count_hj   = sum(count)
                                            ,n_hj       = length(unique(CK_Building_ID))
                                            # ,n.h        = sum(n.h)
    ), stringsAsFactors = F)
    item.strata <- left_join(byVariableCounts, Popandns) #replaced item.strata.group with byVariableCounts
    # item.strata$N_h <- item.strata$N_h * item.strata$n_hj / item.strata$n_h
    #QAQC
    stopifnot(item.strata$n <= item.strata$n_h)
  }
  item.strata$strataSD[which(item.strata$strataSD %in% c("N/A","NaN",NA))] <- 0
  item.strata$strataMean[which(item.strata$strataMean %in% c("N/A","NaN",NA))] <- 0
  
  ######################################################
  # weighted means and SEs by grouping variables
  ######################################################
  if(byVariable == "BuildingType"){
    item.final <- data.frame(ddply(item.strata, c("BuildingType"), summarise
                                   ,Mean = sum(N_h * strataMean) / sum(N_h)
                                   ,SE   = sqrt(sum(N_h^2 * (1 / n_h) * (1 - n_h / N_h) * strataSD^2, na.rm = T)) / sum(N_h)
                                   ,n      = sum(n_hj)
                                   ,n_h    = sum(n_h)
                                   ,N_h    = sum(N_h)
                                   ,EB   = SE * qt(1-(1-0.9)/2, n-length(strataMean))
                                   ,Precision = EB / Mean), stringsAsFactors = F)
    
    Category <- valueVariable
    item.final <- data.frame("Category" = Category
                             ,item.final
                             ,stringsAsFactors = F)

  }else{
    item.group <- data.frame(ddply(item.strata, c("BuildingType", byVariable), summarise
                           ,Mean = sum(N_h * strataMean) / sum(N_h)
                           ,SE   = sqrt(sum(N_h^2 * (1 / n_h) * (1 - n_h / N_h) * strataSD^2)) / sum(N_h)
                           ,n    = sum(n_hj)
                           ,n_h  = sum(n_h)
                           ,N_h  = sum(N_h)
                           ,EB   = SE * qt(1-(1-0.9)/2, n-length(strataMean))
                           ,Precision = EB / Mean), stringsAsFactors = F)
    
    item.region <- data.frame(ddply(item.strata, "BuildingType", summarise
                                    ,byRow  = aggregateRow
                                    ,Mean   = sum(N_h * strataMean) / sum(N_h)
                                    ,SE     = sqrt(sum(N_h^2 * (1 / n_h) * (1 - n_h / N_h) * strataSD^2)) / sum(N_h)
                                    ,n      = sum(n_h)
                                    ,n_h    = sum(n_h)
                                    ,N_h    = sum(N_h)
                                    ,EB     = SE * qt(1-(1-0.9)/2, n-length(strataMean))
                                    ,Precision = EB / Mean), stringsAsFactors = F)
    #rename columns
    colnames(item.region)[which(colnames(item.region) == 'byRow')] <- byVariable
    
    item.final <- rbind.data.frame(item.group, item.region, stringsAsFactors = F)
  }
  
  return(item.final)
}





#################################################################################################################
#Function: mean_one_group_domain
#Used For: 
# 
# 
#################################################################################################################
#weighted function for means with one grouping variable
mean_one_group_domain <- function(CustomerLevelData, valueVariable, byVariable, aggregateRow) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ### Get appropriate sample and population sizes for the strata and the domain
  strata_domain_level <- data.frame(ddply(CustomerLevelData
                                          , c("BuildingType", "State", "Region", "Territory"), summarise
                                          ,n_l        = unique(n.h)
                                          ,N_l        = sum(N.h)), stringsAsFactors = F)
  
  ### Get sum and mean of metrics when applicable as well as the strata-domain sample size and unit size
  strata_domain_summary    <- data.frame(ddply(CustomerLevelData
                                               , c("BuildingType", "State", "Region", "Territory", byVariable), summarise
                                               ,y_lk     = sum(get(valueVariable), na.rm = T)
                                               ,y_bar_lk = y_lk / length(unique(CK_Building_ID))
                                               ,n_lk     = length(unique(CK_Building_ID))
                                               ,m_lk     = sum(m_ilk)
  ), stringsAsFactors = F)
  strata_domain_merge      <- left_join(strata_domain_summary, strata_domain_level)
  site_strata_domain_merge <- left_join(CustomerLevelData, strata_domain_merge)
  
  
  
  
  
  
  ##################################################################################################x
  # Domain estimation
  ##################################################################################################x
  ### Get esimtated number of units in the population and estimated population average of the metric
  domain_summary <- data.frame(ddply(site_strata_domain_merge
                                     , c("BuildingType",byVariable), summarise
                                     ,M_hat_k = sum(sum(N_l / n_lk * m_ilk))
                                     ,y_bar_hat_k  = (1 / M_hat_k) * sum(sum(N_l / n_lk * get(valueVariable)))
  ), stringsAsFactors = F)
  
  
  strata_domain_merge <- left_join(strata_domain_merge, domain_summary)
  ### calculate the inner sum to get the between-strata variance at the domain level
  across_strata_estimation <- data.frame(ddply(strata_domain_merge
                                               , c("BuildingType",byVariable), summarise
                                               ,sum_2 = sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * n_lk * (y_bar_lk - y_bar_hat_k)^2)
  ), stringsAsFactors = F)


site_strata_domain_merge <- left_join(site_strata_domain_merge, domain_summary)
  ### calculate the outer sum to get the within-strata variance, the combined total estimated variance, and the standard error at the domain level 
  domain_estimation <- data.frame(ddply(site_strata_domain_merge
                                        , c("BuildingType",byVariable), summarise
                                        ,sum_1 = sum(sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * (1 / n_lk) * (1 - n_l / N_l) * (y_bar_ilk - y_bar_lk)^2 / (n_lk )))
  ), stringsAsFactors = F)
  
  domain_variance_merge <- left_join(across_strata_estimation, domain_estimation)
  domain_variance_merge <- left_join(domain_variance_merge, domain_summary)
  domain_variance_merge$var_hat_y_bar_hat_k <- (domain_variance_merge$sum_1 + domain_variance_merge$sum_2) / domain_variance_merge$M_hat_k^2
  domain_variance_merge$SE_y_bar_hat_k <- sqrt(domain_variance_merge$var_hat_y_bar_hat_k)
  domain_variance_merge$EB_y_bar_hat_k <- domain_variance_merge$SE_y_bar_hat_k * 1.645
  domain_variance_merge$Precision <- domain_variance_merge$EB_y_bar_hat_k / domain_variance_merge$y_bar_hat_k
  
  
  
  ##################################################################################################x
  # Across Domain estimation
  ##################################################################################################x
  ### calculate the inner sum to get the between-strata variance across domains
  across_domain_summary <- data.frame(ddply(site_strata_domain_merge
                                            , c("BuildingType"), summarise
                                            , byRow              = aggregateRow
                                            , M_hat_k = sum(sum(N_l / n_lk * m_ilk))
                                            , y_bar_hat_k  = (1 / M_hat_k) * sum(sum(N_l / n_lk * get(valueVariable)))
  ), stringsAsFactors = F)
  
  
  ### calculate the inner sum to get the between-strata variance at the domain level
  across_domain_estimation1 <- data.frame(ddply(strata_domain_merge
                                               , c("BuildingType"), summarise
                                               , byRow              = aggregateRow
                                               , sum_2 = sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * n_lk * (y_bar_lk - y_bar_hat_k)^2)
  ), stringsAsFactors = F)
  ### calculate the inner sum to get the between-strata variance at the domain level
  across_domain_estimation2 <- data.frame(ddply(site_strata_domain_merge
                                        , c("BuildingType"), summarise
                                        , byRow              = aggregateRow
                                        ,sum_1 = sum(sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * (1 / n_lk) * (1 - n_l / N_l) * (y_bar_ilk - y_bar_lk)^2 / (n_lk )))
  ), stringsAsFactors = F)
  
  across_domain_variance_merge <- left_join(across_domain_estimation1, across_domain_estimation2)
  across_domain_variance_merge <- left_join(across_domain_variance_merge, across_domain_summary)
  across_domain_variance_merge$var_hat_y_bar_hat_k <- (across_domain_variance_merge$sum_1 + across_domain_variance_merge$sum_2) / across_domain_variance_merge$M_hat_k^2
  across_domain_variance_merge$SE_y_bar_hat_k <- sqrt(across_domain_variance_merge$var_hat_y_bar_hat_k)
  across_domain_variance_merge$EB_y_bar_hat_k <- across_domain_variance_merge$SE_y_bar_hat_k * 1.645
  across_domain_variance_merge$Precision <- across_domain_variance_merge$EB_y_bar_hat_k / across_domain_variance_merge$y_bar_hat_k

  #rename columns
  colnames(across_domain_variance_merge)[which(colnames(across_domain_variance_merge) == 'byRow')] <- byVariable


  
  ##################################################################################################x
  # Combining Domain and Across Domain Estimation
  ##################################################################################################x
  ### Merge estimation infomration together
  item.estimation            <- rbind.data.frame(domain_variance_merge, across_domain_variance_merge, stringsAsFactors = F)

  #obatin correct sample sizes for
  samplesize.sub <- unique(site_strata_domain_merge[which(colnames(site_strata_domain_merge) %in% c("BuildingType",byVariable,"n_lk"))])
  item.samplesize <- data.frame(ddply(samplesize.sub
                                      , c("BuildingType",byVariable), summarise
                                      ,n = sum(n_lk)), stringsAsFactors = F)
  
  if(byVariable == "Clean.Type"){
    domain.samplesize <- data.frame(ddply(item.samplesize
                                          , c("BuildingType"), summarise
                                          ,byRow = aggregateRow
                                          ,n = max(n)), stringsAsFactors = F)
  }else{
    domain.samplesize <- data.frame(ddply(item.samplesize
                                          , c("BuildingType"), summarise
                                          ,byRow = aggregateRow
                                          ,n = sum(n)), stringsAsFactors = F)
  }
  colnames(domain.samplesize)[which(colnames(domain.samplesize) == 'byRow')] <- byVariable
  # domain.samplesize$Clean.Type <- as.character(domain.samplesize$Clean.Type)
  samplesizes <- rbind.data.frame(item.samplesize, domain.samplesize, stringsAsFactors = F)
  
  
  ### Add sample sizes onto final data
  item.final <- item.estimation
  item.final <- left_join(item.final, samplesizes)
  names(item.final)[which(names(item.final) %in% c("y_bar_hat_k","SE_y_bar_hat_k", "EB_y_bar_hat_k"))] <- c("Mean","SE","EB")
  item.final <- item.final[which(!colnames(item.final) %in% c("sum_1","sum_2","M_hat_k","outer_sum","var_hat_y_bar_hat_k"))]
  
  
  
  return(item.final)
}




#################################################################################################################
#Function: mean_one_group_unweighted
#Used For: 
# 
# 
#################################################################################################################
mean_one_group_unweighted <- function(CustomerLevelData, valueVariable, 
                           byVariable, aggregateRow) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
    ######################################################
    # means and SEs by grouping variables
    ######################################################
    item.byGroup <- data.frame(ddply(CustomerLevelData, c("BuildingType", byVariable), summarise
                              ,n     = length(unique(CK_Building_ID))
                              ,Mean  = mean(get(valueVariable), na.rm = T)
                              ,SE    = sd(get(valueVariable), na.rm = T) / sqrt(n)), stringsAsFactors = F)
    
    
    ######################################################
    # means and SEs across grouping variables
    ######################################################
    item.all <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                          ,All   = aggregateRow
                          ,n     = length(unique(CK_Building_ID))
                          ,Mean  = mean(get(valueVariable), na.rm = T)
                          ,SE    = sd(get(valueVariable), na.rm = T) / sqrt(n)), stringsAsFactors = F)
    
    item.all <- data.frame(ConvertColName(item.all,'All',byVariable),stringsAsFactors = F)
    
    
    if (byVariable == "BuildingType"){
      item.final <- item.byGroup
      item.final$Category <- valueVariable
    }else{
      item.final <- rbind.data.frame(item.byGroup, item.all, stringsAsFactors = F)
    }
    
    
    return(item.final)
}






#################################################################################################################
#Function: mean_two_groups
#Used For: 
# 
# 
#################################################################################################################
mean_two_groups <- function(CustomerLevelData
                            , valueVariable
                            , byVariableRow
                            , byVariableColumn
                            , columnAggregate = NA
                            , rowAggregate    = NA) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ######################################################
  # Using customer level data, Summarise data up to strata level
  # Treat summarise differently depending if either the column or row varaible is state
  ######################################################
  StrataPopCounts <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType", "State", "Region", "Territory"), summarise
                                        ,N_h   = sum(unique(N.h))
                                        ,n_h   = unique(n.h)), stringsAsFactors = F)
  if (byVariableRow == 'State') {
    item.strata.group <- data.frame(ddply(CustomerLevelData
                                          , c("BuildingType", "State", "Region", "Territory", byVariableColumn), summarise
                                          ,strataMean = mean(get(valueVariable), na.rm = T)
                                          ,strataSD   = sd(get(valueVariable), na.rm = T)
                                          ,n_hj       = length(unique(CK_Building_ID))), stringsAsFactors = F)
    item.strata.group$strataSD[which(item.strata.group$strataSD %in% c(NA,"NaN"))] <- 0
  } 
  
  if (byVariableColumn == 'State') {
    item.strata.group <- data.frame(ddply(CustomerLevelData
                                          , c("BuildingType", "State", "Region", "Territory", byVariableRow), summarise
                                          ,strataMean = mean(get(valueVariable))
                                          ,strataSD   = sd(get(valueVariable))
                                          ,n_hj       = length(unique(CK_Building_ID))), stringsAsFactors = F)
    item.strata.group$strataSD[which(item.strata.group$strataSD %in% c(NA,"NaN"))] <- 0
  }
  
  if (byVariableRow != 'State' & byVariableColumn != 'State') {
    item.strata.group <- data.frame(ddply(CustomerLevelData
                                          , c("BuildingType", "State", "Region", "Territory", byVariableRow, byVariableColumn), summarise
                                          ,strataMean = mean(get(valueVariable), na.rm = T)
                                          ,strataSD   = sd(get(valueVariable), na.rm = T)
                                          ,n_hj       = length(unique(CK_Building_ID))), stringsAsFactors = F)
    item.strata.group$strataSD[which(item.strata.group$strataSD %in% c(NA,"NaN"))] <- 0
  }
  
  item.strata <- left_join(item.strata.group, StrataPopCounts)
  
  stopifnot(item.strata$n <= item.strata$n_h)
  
  ######################################################
  # Get sample sizes, means, and SEs by both grouping variables
  ######################################################
  # item.group.sample.size <- data.frame(ddply(CustomerLevelData, c("BuildingType", byVariableRow, byVariableColumn),summarise
  #                                 ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)
  
  item.group.all <- data.frame(ddply(item.strata, c("BuildingType", byVariableRow, byVariableColumn), summarise
                                     ,Mean = sum(N_h * strataMean) / sum(N_h)
                                     ,SE   = sqrt(sum(N_h^2 * (1 / n_h) * (1 - n_h / N_h) * strataSD^2)) / sum(N_h)
                                     ,n    = sum(n_hj)
                                     ,n_h  = sum(n_h)
                                     ,N_h  = sum(N_h)
                                     ,EB   = SE * qt(1-(1-0.9)/2, n-length(strataMean))
                          ), stringsAsFactors = F)
  
  #merge samples sizes onto mean and SE info
  # item.group.all <- left_join(item.group.all, item.group.sample.size)
  
  ######################################################
  # Perform grouping variable level analysis aggregating the row by group
  ######################################################
  if (!is.na(rowAggregate)){
    item.group.rowAgg <- data.frame(ddply(item.strata, c("BuildingType", byVariableColumn), summarise
                               ,byRow      = rowAggregate
                               ,Mean = sum(N_h * strataMean) / sum(N_h)
                               ,SE   = sqrt(sum(N_h^2 * (1 / n_h) * (1 - n_h / N_h) * strataSD^2)) / sum(N_h)
                               ,n    = sum(n_hj)
                               ,n_h  = sum(n_h)
                               ,N_h  = sum(N_h)
                               ,EB   = SE * qt(1-(1-0.9)/2, n-length(strataMean))
                               ), stringsAsFactors = F)
    #Rename column byrow
    item.group.rowAgg <- ConvertColName(item.group.rowAgg,
                                        "byRow",
                                        byVariableRow)
    item.group.rowFinal <- rbind.data.frame(item.group.all, item.group.rowAgg, stringsAsFactors = F)
  }

  if (!is.na(columnAggregate)) {
    item.group.colAgg1 <- data.frame(ddply(item.strata, c("BuildingType", byVariableRow), summarise
                                ,byCol = columnAggregate
                                ,Mean = sum(N_h * strataMean) / sum(N_h)
                                ,SE   = sqrt(sum(N_h^2 * (1 / n_h) * (1 - n_h / N_h) * strataSD^2)) / sum(N_h)
                                ,n    = sum(n_hj)
                                ,n_h  = sum(n_h)
                                ,N_h  = sum(N_h)
                                ,EB   = SE * qt(1-(1-0.9)/2, n-length(strataMean))
                                ), stringsAsFactors = F)
    #rename column byCol
    item.group.colAgg1 <- ConvertColName(item.group.colAgg1, "byCol",
                                         byVariableColumn)
    
    if (!is.na(rowAggregate)) {
      item.group.colAgg2 <- data.frame(ddply(item.strata, "BuildingType", summarise
                                  ,byCol = columnAggregate
                                  ,byRow = rowAggregate
                                  ,Mean = sum(N_h * strataMean) / sum(N_h)
                                  ,SE   = sqrt(sum(N_h^2 * (1 / n_h) * (1 - n_h / N_h) * strataSD^2)) / sum(N_h)
                                  ,n    = sum(n_hj)
                                  ,n_h  = sum(n_h)
                                  ,N_h  = sum(N_h)
                                  ,EB   = SE * qt(1-(1-0.9)/2, n-length(strataMean))
                                  ), stringsAsFactors = F)
      #rename columns byCol and by Row
      item.group.colAgg2 <- ConvertColName(item.group.colAgg2, "byRow",byVariableRow)
      item.group.colAgg2 <- ConvertColName(item.group.colAgg2, "byCol",byVariableColumn)
      
      item.group.colAggFinal <- rbind.data.frame(item.group.colAgg1,
                                                 item.group.colAgg2,
                                                 stringsAsFactors = F)
    } else {
      item.group.colAggFinal <- item.group.colAgg1
    }
  }
  
  ## row bind the data before casting
  #  If column variable is NA, row bind row variable final with aggregate final
  if (nrow(item.group.rowFinal) > 0) {
    dataToCast <- rbind.data.frame(item.group.rowFinal, item.group.colAggFinal,
                                   stringsAsFactors = F)
  } else { 
    #  If column variable is not missing, row bind column and row variables (group) final with aggregate final
    dataToCast <- rbind.data.frame(item.group.all, item.group.colAggFinal,
                                   stringsAsFactors = F)
  }
  
  #Cast data
  CastedData <- dcast(setDT(dataToCast)
                      ,formula = BuildingType + get(byVariableRow) ~ get(byVariableColumn)
                      ,value.var = c("Mean", "SE", "n", "EB"))
  CastedData <- ConvertColName(CastedData, 'byVariableRow',
                               byVariableRow)
  return(CastedData)
}



#################################################################################################################
#Function: mean_two_groups_domain
#Used For: 
# 
# 
#################################################################################################################
#weighted function for means with one grouping variable
mean_two_groups_domain <- function(CustomerLevelData
                                  , valueVariable
                                  , byVariableRow
                                  , byVariableColumn
                                  , aggregateColumn = NA
                                  , aggregateRow    = NA) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ### Get appropriate sample and population sizes for the strata and the domain
  strata_domain_level <- data.frame(ddply(CustomerLevelData
                                          , c("BuildingType", "State", "Region", "Territory"), summarise
                                          ,n_l        = unique(n.h)
                                          ,N_l        = sum(N.h)), stringsAsFactors = F)
  
  ### Get sum and mean of metrics when applicable as well as the strata-domain sample size and unit size
  strata_domain_summary    <- data.frame(ddply(CustomerLevelData
                                               , c("BuildingType", "State", "Region", "Territory", byVariableRow, byVariableColumn), summarise
                                               ,y_lk     = sum(get(valueVariable), na.rm = T)
                                               ,y_bar_lk = y_lk / length(unique(CK_Building_ID))
                                               ,n_lk     = length(unique(CK_Building_ID))
                                               ,m_lk     = sum(m_ilk)
  ), stringsAsFactors = F)
  strata_domain_merge      <- left_join(strata_domain_summary, strata_domain_level)
  site_strata_domain_merge <- left_join(CustomerLevelData, strata_domain_merge)
  
  
  
  
  
  
  ##################################################################################################x
  # Domain estimation
  ##################################################################################################x
  ### Get esimtated number of units in the population and estimated population average of the metric
  domain_summary <- data.frame(ddply(site_strata_domain_merge
                                     , c("BuildingType",byVariableRow,byVariableColumn), summarise
                                     ,M_hat_k = sum(sum(N_l / n_lk * m_ilk))
                                     ,y_bar_hat_k  = (1 / M_hat_k) * sum(sum(N_l / n_lk * get(valueVariable)))
  ), stringsAsFactors = F)
  
  
  strata_domain_merge <- left_join(strata_domain_merge, domain_summary)
  ### calculate the inner sum to get the between-strata variance at the domain level
  across_strata_estimation <- data.frame(ddply(strata_domain_merge
                                               , c("BuildingType",byVariableRow,byVariableColumn), summarise
                                               ,sum_2 = sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * n_lk * (y_bar_lk - y_bar_hat_k)^2)
  ), stringsAsFactors = F)
  
  
  site_strata_domain_merge <- left_join(site_strata_domain_merge, domain_summary)
  ### calculate the outer sum to get the within-strata variance, the combined total estimated variance, and the standard error at the domain level 
  domain_estimation <- data.frame(ddply(site_strata_domain_merge
                                        , c("BuildingType",byVariableRow,byVariableColumn), summarise
                                        ,sum_1 = sum(sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * (1 / n_lk) * (1 - n_l / N_l) * (y_bar_ilk - y_bar_lk)^2 / (n_lk )))
  ), stringsAsFactors = F)
  
  domain_variance_merge <- left_join(across_strata_estimation, domain_estimation)
  domain_variance_merge <- left_join(domain_variance_merge, domain_summary)
  domain_variance_merge$var_hat_y_bar_hat_k <- (domain_variance_merge$sum_1 + domain_variance_merge$sum_2) / domain_variance_merge$M_hat_k^2
  domain_variance_merge$SE_y_bar_hat_k <- sqrt(domain_variance_merge$var_hat_y_bar_hat_k)
  
  
  
  ##################################################################################################x
  # Across Domain estimation (ACROSS ROWS)
  ##################################################################################################x
  ### calculate the inner sum to get the between-strata variance across domains
  across_domain_rows_summary <- data.frame(ddply(site_strata_domain_merge
                                            , c("BuildingType", byVariableColumn), summarise
                                            , byRow              = aggregateRow
                                            , M_hat_k = sum(sum(N_l / n_lk * m_ilk))
                                            , y_bar_hat_k  = (1 / M_hat_k) * sum(sum(N_l / n_lk * get(valueVariable)))
  ), stringsAsFactors = F)
  
  
  ### calculate the inner sum to get the between-strata variance at the domain level
  across_domain_rows_estimation1 <- data.frame(ddply(strata_domain_merge
                                                , c("BuildingType", byVariableColumn), summarise
                                                , byRow              = aggregateRow
                                                , sum_2 = sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * n_lk * (y_bar_lk - y_bar_hat_k)^2)
  ), stringsAsFactors = F)
  ### calculate the inner sum to get the between-strata variance at the domain level
  across_domain_rows_estimation2 <- data.frame(ddply(site_strata_domain_merge
                                                , c("BuildingType", byVariableColumn), summarise
                                                , byRow              = aggregateRow
                                                ,sum_1 = sum(sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * (1 / n_lk) * (1 - n_l / N_l) * (y_bar_ilk - y_bar_lk)^2 / (n_lk )))
  ), stringsAsFactors = F)
  
  across_domain_rows_variance_merge <- left_join(across_domain_rows_estimation1, across_domain_rows_estimation2)
  across_domain_rows_variance_merge <- left_join(across_domain_rows_variance_merge, across_domain_rows_summary)
  across_domain_rows_variance_merge$var_hat_y_bar_hat_k <- (across_domain_rows_variance_merge$sum_1 + across_domain_rows_variance_merge$sum_2) / across_domain_rows_variance_merge$M_hat_k^2
  across_domain_rows_variance_merge$SE_y_bar_hat_k <- sqrt(across_domain_rows_variance_merge$var_hat_y_bar_hat_k)
  
  #rename columns
  colnames(across_domain_rows_variance_merge)[which(colnames(across_domain_rows_variance_merge) == 'byRow')] <- byVariableRow
  
  
  
  ##################################################################################################x
  # Across Domain estimation (ACROSS COLUMNS)
  ##################################################################################################x
  ### calculate the inner sum to get the between-strata variance across domains
  across_domain_cols_summary <- data.frame(ddply(site_strata_domain_merge
                                            , c("BuildingType", byVariableRow), summarise
                                            , byCol              = aggregateColumn
                                            , M_hat_k = sum(sum(N_l / n_lk * m_ilk))
                                            , y_bar_hat_k  = (1 / M_hat_k) * sum(sum(N_l / n_lk * get(valueVariable)))
  ), stringsAsFactors = F)
  
  
  ### calculate the inner sum to get the between-strata variance at the domain level
  across_domain_cols_estimation1 <- data.frame(ddply(strata_domain_merge
                                                , c("BuildingType", byVariableRow), summarise
                                                , byCol              = aggregateColumn
                                                , sum_2 = sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * n_lk * (y_bar_lk - y_bar_hat_k)^2)
  ), stringsAsFactors = F)
  ### calculate the inner sum to get the between-strata variance at the domain level
  across_domain_cols_estimation2 <- data.frame(ddply(site_strata_domain_merge
                                                , c("BuildingType", byVariableRow), summarise
                                                , byCol              = aggregateColumn
                                                ,sum_1 = sum(sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * (1 / n_lk) * (1 - n_l / N_l) * (y_bar_ilk - y_bar_lk)^2 / (n_lk )))
  ), stringsAsFactors = F)
  
  across_domain_cols_variance_merge <- left_join(across_domain_cols_estimation1, across_domain_cols_estimation2)
  across_domain_cols_variance_merge <- left_join(across_domain_cols_variance_merge, across_domain_cols_summary)
  across_domain_cols_variance_merge$var_hat_y_bar_hat_k <- (across_domain_cols_variance_merge$sum_1 + across_domain_cols_variance_merge$sum_2) / across_domain_cols_variance_merge$M_hat_k^2
  across_domain_cols_variance_merge$SE_y_bar_hat_k <- sqrt(across_domain_cols_variance_merge$var_hat_y_bar_hat_k)
  
  #rename columns
  colnames(across_domain_cols_variance_merge)[which(colnames(across_domain_cols_variance_merge) == 'byCol')] <- byVariableColumn
  
  
  
  
  ##################################################################################################x
  # Across Domain estimation (ACROSS BOTH ROWS AND COLUMNS)
  ##################################################################################################x
  ### calculate the inner sum to get the between-strata variance across domains
  across_both_summary <- data.frame(ddply(site_strata_domain_merge
                                          , c("BuildingType"), summarise
                                          , byRow = aggregateRow
                                          , byCol = aggregateColumn
                                          , M_hat_k = sum(sum(N_l / n_lk * m_ilk))
                                          , y_bar_hat_k  = (1 / M_hat_k) * sum(sum(N_l / n_lk * get(valueVariable)))
  ), stringsAsFactors = F)
  
  
  ### calculate the inner sum to get the between-strata variance at the domain level
  across_both_estimation1 <- data.frame(ddply(strata_domain_merge
                                              , c("BuildingType"), summarise
                                              , byRow = aggregateRow
                                              , byCol = aggregateColumn
                                              , sum_2 = sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * n_lk * (y_bar_lk - y_bar_hat_k)^2)
  ), stringsAsFactors = F)
  ### calculate the inner sum to get the between-strata variance at the domain level
  across_both_estimation2 <- data.frame(ddply(site_strata_domain_merge
                                              , c("BuildingType"), summarise
                                              , byRow = aggregateRow
                                              , byCol = aggregateColumn
                                              ,sum_1 = sum(sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * (1 / n_lk) * (1 - n_l / N_l) * (y_bar_ilk - y_bar_lk)^2 / (n_lk )))
  ), stringsAsFactors = F)
  
  across_both_variance_merge <- left_join(across_both_estimation1, across_both_estimation2)
  across_both_variance_merge <- left_join(across_both_variance_merge, across_both_summary)
  across_both_variance_merge$var_hat_y_bar_hat_k <- (across_both_variance_merge$sum_1 + across_both_variance_merge$sum_2) / across_both_variance_merge$M_hat_k^2
  across_both_variance_merge$SE_y_bar_hat_k <- sqrt(across_both_variance_merge$var_hat_y_bar_hat_k)
  
  #rename columns
  colnames(across_both_variance_merge)[which(colnames(across_both_variance_merge) == 'byCol')] <- byVariableColumn
  colnames(across_both_variance_merge)[which(colnames(across_both_variance_merge) == 'byRow')] <- byVariableRow
  
  
  
  
  
  
  ##################################################################################################x
  # Combining Domain and Across Domain Estimation
  ##################################################################################################x
  ### Merge estimation infomration together
  item.estimation            <- rbind.data.frame(domain_variance_merge
                                                 , across_domain_rows_variance_merge
                                                 , across_domain_cols_variance_merge
                                                 , across_both_variance_merge
                                                 , stringsAsFactors = F)
  
  #obatin correct sample sizes for
  item.samplesize <- data.frame(ddply(CustomerLevelData
                                      , c("BuildingType",byVariableRow,byVariableColumn), summarise
                                      ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)
  #obatin correct sample sizes for
  rows.samplesize <- data.frame(ddply(CustomerLevelData
                                      , c("BuildingType",byVariableRow), summarise
                                      ,byCol = aggregateColumn
                                      ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)
  #obatin correct sample sizes for
  cols.samplesize <- data.frame(ddply(CustomerLevelData
                                      , c("BuildingType",byVariableColumn), summarise
                                      ,byRow = aggregateRow
                                      ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)
  domain.samplesize <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType"), summarise
                                        ,byRow = aggregateRow
                                        ,byCol = aggregateColumn
                                        ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)
  
  
  colnames(domain.samplesize)[which(colnames(domain.samplesize) == 'byRow')] <- byVariableRow
  colnames(domain.samplesize)[which(colnames(domain.samplesize) == 'byCol')] <- byVariableColumn
  colnames(rows.samplesize)[which(colnames(rows.samplesize) == 'byCol')] <- byVariableColumn
  colnames(cols.samplesize)[which(colnames(cols.samplesize) == 'byRow')] <- byVariableRow
  
  samplesizes <- rbind.data.frame(item.samplesize,rows.samplesize,cols.samplesize,domain.samplesize, stringsAsFactors = F)
  
  
  ### Add sample sizes onto final data
  item.final <- data.frame(item.estimation, stringsAsFactors = F)
  item.final <- left_join(item.final, samplesizes)
  names(item.final)[which(names(item.final) %in% c("y_bar_hat_k","SE_y_bar_hat_k"))] <- c("Mean","SE")
  item.final <- item.final[which(!colnames(item.final) %in% c("sum_1","sum_2","M_hat_k","outer_sum","var_hat_y_bar_hat_k"))]
  item.final$EB <- item.final$SE * qt(1-(1-0.9)/2,item.final$n-1)
  item.final$Precision <- item.final$EB / item.final$Mean
  
  item.cast <- dcast(setDT(item.final)
                     ,formula = BuildingType + get(byVariableRow) ~ get(byVariableColumn)
                     ,value.var = c("Mean", "SE", "n", "EB", "Precision"))
  item.cast <- ConvertColName(item.cast, 'byVariableRow',byVariableRow)
  
  
  return(item.cast)
}









#################################################################################################################
#Function: mean_two_groups_unweighted
#Used For: 
# 
# 
#################################################################################################################
mean_two_groups_unweighted <- function(CustomerLevelData
                                       , valueVariable
                                       , byVariableRow
                                       , byVariableColumn
                                       , columnAggregate = NA
                                       , rowAggregate    = NA) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ######################################################
  # Using customer level data, Summarise data up to strata level
  # Treat summarise differently depending if either the column or row varaible is state
  ######################################################
  if (byVariableRow == 'State') {
    item.strata <- data.frame(ddply(CustomerLevelData
                                    , c("BuildingType", "State", "Region", "Territory", byVariableColumn), summarise
                                    ,Mean = mean(get(valueVariable),na.rm = T)
                                    ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Building_ID)))
                                    ,n    = length(unique(CK_Building_ID))), stringsAsFactors = F)
    item.strata$SE[which(item.strata$SE == "NaN")] <- 0
  } 
  if (byVariableColumn == 'State') {
    item.strata <- data.frame(ddply(CustomerLevelData
                                    , c("BuildingType", "State", "Region", "Territory", byVariableRow), summarise
                                    ,Mean = mean(get(valueVariable),na.rm = T)
                                    ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Building_ID)))
                                    ,n    = length(unique(CK_Building_ID))), stringsAsFactors = F)
    item.strata$SE[which(item.strata$SE == "NaN")] <- 0
  }
  if (byVariableRow != 'State' & byVariableColumn != 'State') {
    item.strata <- data.frame(ddply(CustomerLevelData
                                    , c("BuildingType", "State", "Region", "Territory", byVariableRow, byVariableColumn), summarise
                                    ,Mean = mean(get(valueVariable),na.rm = T)
                                    ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Building_ID)))
                                    ,n    = length(unique(CK_Building_ID))), stringsAsFactors = F)
    item.strata$SE[which(item.strata$SE == "NaN")] <- 0
  }
  
  ######################################################
  # Perform analysis using both by groups
  ######################################################
  item.group.all <- data.frame(ddply(CustomerLevelData, c("BuildingType", byVariableRow, byVariableColumn), summarise
                                     ,Mean = mean(get(valueVariable),na.rm = T)
                                     ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Building_ID)))
                                     ,n    = length(unique(CK_Building_ID))), stringsAsFactors = F)
  
  ######################################################
  # Perform analysis aggregating the row by group
  ######################################################
  if (!is.na(rowAggregate)){
    item.group.rowAgg <- data.frame(ddply(CustomerLevelData, c("BuildingType", byVariableColumn), summarise
                                          ,byRow= rowAggregate
                                          ,Mean = mean(get(valueVariable),na.rm = T)
                                          ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Building_ID)))
                                          ,n    = length(unique(CK_Building_ID))), stringsAsFactors = F)
    #rename column
    item.group.rowAgg <- ConvertColName(item.group.rowAgg,"byRow",byVariableRow)
    
    item.group.rowFinal <- rbind.data.frame(item.group.all, item.group.rowAgg, stringsAsFactors = F)
  }
  
  if (!is.na(columnAggregate)) {
    item.group.colAgg1 <- data.frame(ddply(CustomerLevelData, c("BuildingType", byVariableRow), summarise
                                           ,byCol = columnAggregate
                                           ,Mean = mean(get(valueVariable),na.rm = T)
                                           ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Building_ID)))
                                           ,n    = length(unique(CK_Building_ID))), stringsAsFactors = F)
    #rename column
    item.group.colAgg1 <- ConvertColName(item.group.colAgg1, "byCol",byVariableColumn)
    
    if (!is.na(rowAggregate)) {
      item.group.colAgg2 <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                             ,byCol = columnAggregate
                                             ,byRow = rowAggregate
                                             ,Mean = mean(get(valueVariable),na.rm = T)
                                             ,SE   = sd(get(valueVariable),na.rm = T) / sqrt(length(unique(CK_Building_ID)))
                                             ,n    = length(unique(CK_Building_ID))), stringsAsFactors = F)
      #rename columns
      item.group.colAgg2 <- ConvertColName(item.group.colAgg2, "byRow",byVariableRow)
      item.group.colAgg2 <- ConvertColName(item.group.colAgg2, "byCol",byVariableColumn)
      
      item.group.colAggFinal <- rbind.data.frame(item.group.colAgg1,item.group.colAgg2,stringsAsFactors = F)
    } else {
      item.group.colAggFinal <- item.group.colAgg1
    }
  }
  
  
  ## row bind the data before casting
  #  If column variable is NA, row bind row variable final with aggregate final
  if (nrow(item.group.rowFinal) > 0) {
    dataToCast <- rbind.data.frame(item.group.rowFinal, item.group.colAggFinal,
                                   stringsAsFactors = F)
  } else {
    #  If column variable is not missing, row bind column and row variables (group) final with aggregate final
    dataToCast <- rbind.data.frame(item.group.all,item.group.colAggFinal,
                                   stringsAsFactors = F)
  }
  
  #cast data
  CastedData <- dcast(setDT(dataToCast)
                      ,formula = BuildingType + get(byVariableRow) ~ get(byVariableColumn)
                      ,value.var = c("Mean", "SE", "n"))
  CastedData <- ConvertColName(CastedData, 'byVariableRow',
                               byVariableRow)
  return(CastedData)
}




#################################################################################
#Function: proportions_one_group
#Used For: Calculating the proportion where proportions are calculated accross 
#          multiple rows. 
#          For example, calculating the distribution of a measure for all homes in each state
#################################################################################
proportions_one_group <- function(CustomerLevelData
                                  , valueVariable
                                  , groupingVariable
                                  , total.name
                                  , columnName
                                  , weighted  = TRUE
                                  , two.prop.total = NA) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ###################################################################################################x
  # For Weighted
  ###################################################################################################x
  if (weighted == TRUE){
    #sample and pop sizes within defined strata - this is to account for the fact that not all categories from each table will be observed in each strata
    StrataPopCounts <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType", "State", "Region", "Territory"), summarise
                                        ,N.h   = sum(unique(N.h))
                                        ,n.h   = unique(n.h)), stringsAsFactors = F)
    
    if(groupingVariable %in% c("State","BuildingType", "EUI_Quartile")){
      
      # this if statement is specifically to calculate the denominator for tables
      # where we want to know the percent of stored bulbs by bulb type
      if(valueVariable %in% c("StorageBulbs")){
        # obtain count and proportion by strata and row grouping variable
        StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                     , c("BuildingType", "State", "Region", "Territory"), summarise
                                                     ,n_hj        = length(unique(CK_Building_ID))
                                                     ,count       = sum(get(valueVariable))
                                                     ,total.count = sum(TotalBulbs)
                                                     ,p.h = count / total.count), stringsAsFactors = F)
      }else if(valueVariable %in% c("Ind", "Thermostat.Count")){
        # obtain count and proportion by strata and row grouping variable
        StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                     , c("BuildingType", "State", "Region", "Territory"), summarise
                                                     ,n_hj        = length(unique(CK_Building_ID))
                                                     ,count       = sum(get(valueVariable))
                                                     ,total.count = sum(Count)
                                                     ,p.h = count / total.count), stringsAsFactors = F)
      }else if(valueVariable %in% c("EfficientTotal")){
        # obtain count and proportion by strata and row grouping variable
        StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                     , c("BuildingType", "State", "Region", "Territory", groupingVariable), summarise
                                                     ,n_hj        = length(unique(CK_Building_ID))
                                                     ,count       = sum(get(valueVariable))
                                                     ,total.count = sum(TotalBulbs)
                                                     ,p.h = count / total.count), stringsAsFactors = F)
      }else if(valueVariable %in% c("ElectricInd", "Has_AC", "Electric_DWH")){
        # obtain count and proportion by strata and row grouping variable
        StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                     , c("BuildingType", "State", "Region", "Territory", groupingVariable), summarise
                                                     ,n_hj        = length(unique(CK_Building_ID))
                                                     ,count       = sum(get(valueVariable))
                                                     ,total.count = sum(Count)
                                                     ,p.h = count / total.count), stringsAsFactors = F)
      }else{
        # obtain count and proportion by strata and row grouping variable
        StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                     , c("BuildingType", "State", "Region", "Territory", groupingVariable), summarise
                                                     ,n_hj        = length(unique(CK_Building_ID))
                                                     ,count       = sum(get(valueVariable))
                                                     ,total.count = length(unique(CK_Building_ID))
                                                     ,p.h = count / total.count), stringsAsFactors = F)
      }
      # If grouping variable is NOT state
    } else if(groupingVariable %in% c("Clean.Type","Type","HomeType") & valueVariable == "Ind"){
      # obtain count and proportion by strata and row grouping variable
      StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                   , c("BuildingType", "State", "Region", "Territory", groupingVariable), summarise
                                                   ,n_hj = length(unique(CK_Building_ID))
                                                   ,count = sum(get(valueVariable))
                                                   ,total.count = sum(Count)
                                                   ,p.h = count / total.count), stringsAsFactors = F)
      
    }else if(groupingVariable %in% c("CK_Building_ID") & valueVariable %in% c("Ind", "cond.ind")){
      # obtain count and proportion by strata and row grouping variable
      StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                   , c("BuildingType", "State", "Region", "Territory", groupingVariable), summarise
                                                   ,n_hj = length(unique(CK_Building_ID))
                                                   ,count = sum(get(valueVariable))
                                                   ,total.count = n_hj
                                                   ,p.h = count / total.count), stringsAsFactors = F)
      
    }else if(groupingVariable %in% c("CK_Building_ID") & valueVariable %in% c("StorageBulbs")){
      # obtain count and proportion by strata and row grouping variable
      StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                   , c("BuildingType", "State", "Region", "Territory", groupingVariable), summarise
                                                   ,n_hj = length(unique(CK_Building_ID))
                                                   ,count = sum(get(valueVariable))
                                                   ,total.count = sum(TotalBulbs)
                                                   ,p.h = count / total.count), stringsAsFactors = F)
      
    }else{
      # obtain count and proportion by strata and row grouping variable
      StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                   , c("BuildingType", "State", "Region", "Territory", groupingVariable), summarise
                                                   ,n_hj = length(unique(CK_Building_ID))
                                                   ,count = sum(get(valueVariable))), stringsAsFactors = F)
      
      
      StrataProportion <- data.frame(ddply(StrataGroupedProportions
                                           , c("BuildingType", "State", "Region", "Territory"), summarise
                                           ,total.count = sum(count)), stringsAsFactors = F)
      
      StrataGroupedProportions     <- left_join(StrataGroupedProportions, StrataProportion)
      StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
      StrataGroupedProportions$p.h[which(StrataGroupedProportions$p.h == "NaN")] <- 0
    }
    
    
    #####################################################################################################x
    # For "Percentage" tables
    #####################################################################################################x
    #join strata counts with summary of grouping variable within strata
    if(groupingVariable == "HomeType"){
      StrataData <- left_join(StrataPopCounts , StrataGroupedProportions,
                              by = c("BuildingType", "State", "Region","Territory"))
      StrataData$N.h  <- StrataData$N.h * StrataData$n_hj / StrataData$n.h
      
    }else {
      StrataData <- left_join(StrataPopCounts , StrataGroupedProportions,
                              by = c("BuildingType", "State", "Region","Territory"))
    }
    
    #####################################################################################################x
    # For "Distribution" tables
    #####################################################################################################x
    #obtain the total population size for the building type by state combination observed in the sample
    StrataData_n <- unique(StrataData[which(colnames(StrataData) %in% c("BuildingType"
                                                                        ,"State"
                                                                        ,"Region"
                                                                        ,"Territory"
                                                                        ,"N.h"
                                                                        ,"n.h"))])
    
    columnVarWeights <- data.frame(ddply(StrataData, c("BuildingType"),summarise
                                         ,columnVar.N.h = sum(unique(N.h))
                                         ,columnVar.n.h = sum(n.h)), stringsAsFactors = F)
    
    #join strata data with weights by column grouping variable
    StrataDataWeights <- left_join(StrataData, columnVarWeights, by = "BuildingType")
    
    
    #####################################################################################################x
    # For "Percentage" tables
    #####################################################################################################x
    if(groupingVariable %in% c("State", "Clean.Type", "Wall.Type", "EUI_Quartile","HomeType", "CK_Building_ID")){ # & valueVariable %in% c("Ind", "cond.ind")
      #summarise by column variable
      #summary of both grouping variables
      ColumnProportionsByGroup <- data.frame(ddply(StrataData
                                                   , c("BuildingType", groupingVariable), summarise
                                                   ,w.percent = sum(N.h * p.h) / sum(N.h)
                                                   ,w.SE      = sqrt(sum(N.h^2 * (1 / n.h) * (1 - n.h / N.h) * p.h * (1 - p.h))) / sum(N.h)
                                                   ,count     = sum(count)
                                                   ,N         = sum(N.h)
                                                   ,n         = sum(n_hj)
                                                   ,EB   = w.SE * qt(1-(1-0.9)/2, n)
      ), stringsAsFactors = F)
      #summarise across home types (total level)
      ColumnTotals <- data.frame(ddply(ColumnProportionsByGroup, "BuildingType", summarise
                                       ,rowTotal  = "Total"
                                       ,w.percent = sum(N * w.percent) / sum(N)
                                       ,w.SE      = sqrt(sum(N^2 * (1 / n) * (1 - n / N) * w.percent * (1 - w.percent))) / sum(N)
                                       ,count     = sum(count)
                                       ,N         = sum(N)
                                       ,n         = sum(n)
                                       ,EB   = w.SE * qt(1-(1-0.9)/2, n)), stringsAsFactors = F) 
      # ColumnProportionsByGroup$N.times.p <- ColumnProportionsByGroup$N*ColumnProportionsByGroup$w.percent
      # sum(ColumnProportionsByGroup$N.times.p) / sum(ColumnProportionsByGroup$N)
      # sum(ColumnProportionsByGroup$N)
    }else{
      
      
      
      
      
      #####################################################################################################x
      # For "Distribution" tables
      #####################################################################################################x
      #summarise by column variable
      #summary of both grouping variables
      ColumnProportionsByGroup <- data.frame(ddply(StrataDataWeights
                                                   , c("BuildingType", groupingVariable), summarise
                                                   ,w.percent = sum(N.h * p.h) / unique(columnVar.N.h)
                                                   ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(columnVar.N.h)
                                                   ,count     = sum(count)
                                                   ,N         = unique(columnVar.N.h)
                                                   ,n         = sum(n_hj)
                                                   ,EB   = w.SE * qt(1-(1-0.9)/2, n)), stringsAsFactors = F)
      if(groupingVariable == "Lamp.Category"){
        #summarise across home types (total level)
        ColumnTotals <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                         ,rowTotal  = "Total"
                                         ,w.percent = 1
                                         ,w.SE      = NA
                                         ,count     = sum(count)
                                         ,N         = sum(unique(N.h))
                                         ,n         = length(unique(CK_Building_ID))
                                         ,EB        = w.SE * qt(1-(1-0.9)/2, n)), stringsAsFactors = F)
      }else {
        #summarise across home types (total level)
        ColumnTotals <- data.frame(ddply(ColumnProportionsByGroup, "BuildingType", summarise
                                         ,rowTotal  = "Total"
                                         ,w.percent = sum(w.percent)
                                         ,w.SE      = NA
                                         ,count     = sum(count, na.rm = T)
                                         ,N         = sum(unique(N), na.rm = T)
                                         ,EB        = NA), stringsAsFactors = F)
        SampleSizes <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                        ,n         = length(unique(CK_Building_ID))))
        ColumnTotals <- left_join(ColumnTotals, SampleSizes)
      }
    }## End loops for distribution and percentage tables
    
    
    #rename column
    ColumnTotals <- ConvertColName(ColumnTotals, 'rowTotal', groupingVariable)
    
    
    if(groupingVariable == "BuildingType"){
      AllRowsFinal <- ColumnProportionsByGroup
    } else {
      #join total information onto summary by grouping variables
      AllRowsFinal  <- rbind.data.frame(ColumnProportionsByGroup, 
                                        ColumnTotals, stringsAsFactors = F)
    }
    
    
    if(!is.na(two.prop.total)){
      AllRowsFinal$tmp.total <- total.name
      AllRowsFinal <- ConvertColName(AllRowsFinal, 'tmp.total', columnName)
    }
    
    item.full <- data.frame(AllRowsFinal, stringsAsFactors = F)
    item.full <- item.full[which(colnames(item.full) != "Total.Count")]
    return(item.full)
    
    
    ###################################################################################################x
    # For Unweighted
    ###################################################################################################x
  } else {
    if (groupingVariable == "BuildingType") {
      item.combined <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                        ,n     = length(unique(CK_Building_ID))
                                        ,Count = sum(get(valueVariable))), stringsAsFactors = F)
    }else{
      item.tmp1 <- data.frame(ddply(CustomerLevelData, c("BuildingType", groupingVariable), summarise
                                    ,n     = length(unique(CK_Building_ID))
                                    ,Count = sum(get(valueVariable))), stringsAsFactors = F)
      
      
      item.tmp2 <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                    ,Total = "Total"
                                    ,n     = length(unique(CK_Building_ID))
                                    ,Count = sum(get(valueVariable))), stringsAsFactors = F)
      
      # Convert column name
      item.tmp2 <- ConvertColName(item.tmp2, 'Total', groupingVariable)
      item.combined <- rbind.data.frame(item.tmp1, item.tmp2, stringsAsFactors = F)
    }
    
    if(valueVariable == "StorageBulbs"){
      item.tmpyy <- data.frame(ddply(CustomerLevelData, c("BuildingType", "State"), summarise
                                     ,Total.Count   = sum(TotalBulbs)), stringsAsFactors = F)
      item.tmpxx <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                     ,State = "Total"
                                     ,Total.Count = sum(TotalBulbs)), stringsAsFactors = F)
      
      item.tmp3 <- rbind.data.frame(item.tmpyy, item.tmpxx, stringsAsFactors = F)
    }else if(groupingVariable == "HomeType" & valueVariable == "Number.of.Units"){
      item.tmp3 <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                    ,Total.Count = sum(Number.of.Units, na.rm = T)), stringsAsFactors = F)
    }else if(groupingVariable == "BuildingType" & valueVariable == "Ind"){
      item.tmp3 <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                    ,Total.Count = length(unique(CK_Building_ID))), stringsAsFactors = F)
    }else if(groupingVariable == "HomeType" & valueVariable != "Number.of.Units"){
      item.tmpyy <- data.frame(ddply(CustomerLevelData, c("BuildingType", "HomeType"), summarise
                                     ,Total.Count   = sum(Count)), stringsAsFactors = F)
      item.tmpxx <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                     ,HomeType = "Total"
                                     ,Total.Count = sum(Count)), stringsAsFactors = F)
      item.tmp3 <- rbind.data.frame(item.tmpyy, item.tmpxx, stringsAsFactors = F)
    }else if(groupingVariable %in% c("State","HomeType") & valueVariable == "Ind"){
      item.tmpyy <- data.frame(ddply(CustomerLevelData, c("BuildingType", groupingVariable), summarise
                                     ,Total.Count   = sum(Count)), stringsAsFactors = F)
      item.tmpxx <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                     ,byVariable = "Total"
                                     ,Total.Count = sum(Count)), stringsAsFactors = F)
      item.tmpxx <- ConvertColName(item.tmpxx, 'byVariable', groupingVariable)
      item.tmp3 <- rbind.data.frame(item.tmpyy, item.tmpxx, stringsAsFactors = F)
    }else if(groupingVariable %in% c("CK_Building_ID") & valueVariable %in% c("Ind", "cond.ind")){
      item.tmpyy <- data.frame(ddply(CustomerLevelData, c("BuildingType", groupingVariable), summarise
                                     ,Total.Count   = length(unique(CK_Building_ID))), stringsAsFactors = F)
      item.tmpxx <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                     ,byVariable = "Total"
                                     ,Total.Count = length(unique(CK_Building_ID))), stringsAsFactors = F)
      item.tmpxx <- ConvertColName(item.tmpxx, 'byVariable', groupingVariable)
      item.tmp3 <- rbind.data.frame(item.tmpyy, item.tmpxx, stringsAsFactors = F)
    }else{
      item.tmp3 <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                    ,Total.Count   = sum(get(valueVariable))), stringsAsFactors = F)
    }
    
    
    item.final <- left_join(item.combined, item.tmp3)
    
    if(!is.na(two.prop.total)){
      item.final$tmp.total <- total.name
      item.final <- ConvertColName(item.final, 'tmp.total', columnName)
    }
    
    item.final <- data.frame(item.final, stringsAsFactors = F)
    
    
    if(groupingVariable %in% c("State", "BuildingType")){
      
      # this if statement is specifically to calculate the denominator for tables
      # where we want to know the percent of stored bulbs by bulb type
      if(valueVariable == "StorageBulbs"){
        get.unweighted.proportion <- data.frame(ddply(CustomerLevelData
                                                      , c("BuildingType", "State"), summarise
                                                      ,Percent = sum(get(valueVariable))/ sum(TotalBulbs)), stringsAsFactors = F)
        
        item.final <- left_join(item.final, get.unweighted.proportion)
        
        #calculate percent
        item.final$Percent[which(is.na(item.final$Percent))] <- item.final$Count[which(is.na(item.final$Percent))] / item.final$Total.Count[which(is.na(item.final$Percent))]
        
      }else if(valueVariable == "Ind"){
        item.final$Percent <- item.final$Count / item.final$Total.Count
      }else{
        item.final$Percent <- item.final$Count / item.final$n
      }
    }else if(groupingVariable == "Clean.Type"){
      get.unweighted.proportion <- data.frame(ddply(CustomerLevelData
                                                    , c("BuildingType", "State", groupingVariable), summarise
                                                    ,Percent = sum(get(valueVariable))/ sum(Room.Count)), stringsAsFactors = F)
      
      item.final <- left_join(item.final, get.unweighted.proportion)
      
      #calculate percent
      item.final$Percent[which(is.na(item.final$Percent))] <- item.final$Count[which(is.na(item.final$Percent))] / item.final$Total.Count[which(is.na(item.final$Percent))]
      
      # }else if(valueVariable == "Ind"){
      #   item.final$Percent <- item.final$Count / item.final$n
    }else{
      item.final$Percent <- item.final$Count / item.final$Total.Count
    }
    
    item.final$SE      <- sqrt(item.final$Percent * (1 - item.final$Percent) / item.final$n)
    
    item.final <- item.final[which(colnames(item.final) != "Total.Count")]
    return(item.final)
  }
}





#################################################################################################################
#Function: proportions_one_group_domain
#Used For: 
# 
# 
#################################################################################################################
#weighted function for means with one grouping variable
proportions_one_group_domain <- function(CustomerLevelData, valueVariable, byVariable, aggregateRow) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ### Get appropriate sample and population sizes for the strata and the domain
  strata_domain_level <- data.frame(ddply(CustomerLevelData
                                          , c("BuildingType", "State", "Region", "Territory"), summarise
                                          ,n_l        = unique(n.h)
                                          ,N_l        = sum(N.h)), stringsAsFactors = F)
  
  ### Get sum and mean of metrics when applicable as well as the strata-domain sample size and unit size
  strata_domain_summary1    <- data.frame(ddply(CustomerLevelData
                                               , c("BuildingType", "State", "Region", "Territory", byVariable), summarise
                                               ,n_lk    = length(unique(CK_Building_ID))
                                               ,N_lk    = sum(N.h) * n_lk / unique(n.h)
                                               ,m_lk    = sum(m_ilk)
                                               ,m_bar_lk    = sum(m_ilk) / unique(n.h)
  ), stringsAsFactors = F)
  ### Get sum and mean of metrics when applicable as well as the strata-domain sample size and unit size
  strata_domain_summary2    <- data.frame(ddply(CustomerLevelData
                                               , c("BuildingType", "State", "Region", "Territory"), summarise
                                               # ,n_l          = length(unique(CK_Building_ID))
                                               ,m_l          = sum(m_ilk)
  ), stringsAsFactors = F)
  strata_domain_summary <- left_join(strata_domain_summary1, strata_domain_summary2)
  strata_domain_summary$p_hat_ilk <- strata_domain_summary$m_lk / strata_domain_summary$m_l
  
  ## QAQC
  sum(strata_domain_summary$p_hat_ilk[which(strata_domain_summary$BuildingType == "Single Family" & strata_domain_summary$State == "WA" & strata_domain_summary$Region == "PS" & strata_domain_summary$Territory == "BPA")])
  
  strata_domain_merge      <- left_join(strata_domain_summary, strata_domain_level)
  site_strata_domain_merge <- left_join(CustomerLevelData, strata_domain_merge)

  
  ##################################################################################################x
  # Domain estimation
  ##################################################################################################x
  ### Get esimtated number of units in the population and estimated population average of the metric
  domain_summary <- data.frame(ddply(site_strata_domain_merge
                                     , c("BuildingType",byVariable), summarise
                                     ,M_hat_k = sum(sum(N_l / n_l * m_ilk))
                                     # ,m_bar_k  = (1 / M_hat_lk) * sum(sum(N_lk * m_bar_lk))
  ), stringsAsFactors = F)
  
  domain_summary1 <- data.frame(ddply(domain_summary
                                      , c("BuildingType"), summarise
                                      ,M_hat = sum(M_hat_k)
  ), stringsAsFactors = F)
  ## QAQC - should be equal to 100% across domains within building type
  sum(domain_summary1$M_hat_k[which(domain_summary1$BuildingType == "Single Family")]) / 4254404 #Note that sum(strata_domain_level$N_l[which(strata_domain_level$BuildingType == "Single Family")]) = 4254404
  
  
  domain_merge <- left_join(domain_summary, domain_summary1)
  domain_merge$p_hat_k <- domain_merge$M_hat_k / domain_merge$M_hat
  sum(domain_merge$p_hat_k)
  
  strata_domain_merge <- left_join(strata_domain_merge, domain_merge)
  site_strata_domain_merge <- left_join(site_strata_domain_merge, domain_merge)
  ### calculate the inner sum to get the between-strata variance at the domain level
  across_strata_estimation <- data.frame(ddply(site_strata_domain_merge
                                               , c("BuildingType",byVariable), summarise
                                               ,var = sum(1 / M_hat^2 * N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * (m_ilk - m_bar_lk)^2)
                                               ,SE = sqrt(var)
                                               ,EB = SE * 1.645
  ), stringsAsFactors = F)
  
  
  domain_variance_merge <- left_join(domain_merge, across_strata_estimation)
  domain_variance_merge <- domain_variance_merge[which(colnames(domain_variance_merge) %notin% c("M_hat_k","M_hat","var"))]
  colnames(domain_variance_merge)[which(colnames(domain_variance_merge) %in% c("p_hat_k","SE"))] <- c("w.percent","w.SE")
  
  across_domain_variance_merge <- data.frame(ddply(domain_variance_merge
                                                   , c("BuildingType"), summarise
                                                   ,byRow = aggregateRow
                                                   ,w.percent = sum(w.percent)
                                                   ,w.SE = NA
                                                   ,EB   = NA
                                                   ), stringsAsFactors = F)
  
  colnames(across_domain_variance_merge)[which(colnames(across_domain_variance_merge) == 'byRow')] <- byVariable
  ##################################################################################################x
  # Combining Domain and Across Domain Estimation
  ##################################################################################################x
  ### Merge estimation infomration together
  item.estimation            <- rbind.data.frame(domain_variance_merge, across_domain_variance_merge, stringsAsFactors = F)
  
  #obatin correct sample sizes for
  samplesize.sub <- unique(site_strata_domain_merge[which(colnames(site_strata_domain_merge) %in% c("BuildingType",byVariable,"n_lk"))])
  item.samplesize <- data.frame(ddply(samplesize.sub
                                      , c("BuildingType",byVariable), summarise
                                      ,n = sum(n_lk)), stringsAsFactors = F)
  
  if(byVariable == "Clean.Type"){
    domain.samplesize <- data.frame(ddply(item.samplesize
                                          , c("BuildingType"), summarise
                                          ,byRow = aggregateRow
                                          ,n = max(n)), stringsAsFactors = F)
  }else{
    domain.samplesize <- data.frame(ddply(CustomerLevelData
                                          , c("BuildingType"), summarise
                                          ,byRow = aggregateRow
                                          ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)
  }
  colnames(domain.samplesize)[which(colnames(domain.samplesize) == 'byRow')] <- byVariable
  # domain.samplesize$Clean.Type <- as.character(domain.samplesize$Clean.Type)
  samplesizes <- rbind.data.frame(item.samplesize, domain.samplesize, stringsAsFactors = F)
  
  
  ### Add sample sizes onto final data
  item.final <- item.estimation
  item.final <- left_join(item.final, samplesizes)
  
  return(item.final)
}







#################################################################################
#Function: proportionRowsAndColumns1
#Used For: 
#
# 
#
#################################################################################

proportionRowsAndColumns1 <- function(CustomerLevelData
                                      , valueVariable
                                      , columnVariable
                                      , rowVariable
                                      , aggregateColumnName){
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ########################################################################
  # Obtain correct population and sample size information within strata
  ########################################################################
  # this if statement is because insulation tables
  
  if (rowVariable == "rvalue.bins"){
    StrataPopCounts <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType", "State", "Region", "Territory", columnVariable)
                                        , summarise
                                        ,N.h   = sum(N.h)
                                        ,n.h   = unique(n.h)), stringsAsFactors = F)
  }  else {
    StrataPopCounts <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType", "State", "Region", "Territory")
                                        , summarise
                                        ,N.h   = sum(unique(N.h))
                                        ,n.h   = unique(n.h)), stringsAsFactors = F)
  }
  
  
  #####################################################################
  # Obtain count, total count, and proportions at the correct levels
  #####################################################################
  # If state is the column variable, we need to perform all analyses without it included as a group_by variable
  # Otherwise it will duplicate the State column
  if (columnVariable %in% "State") {
    if(valueVariable == "Ind"){
      StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                   , c("BuildingType", "State", "Region", "Territory", rowVariable)
                                                   , summarise
                                                   , count = sum(get(valueVariable))
                                                   , n_hj = length(unique(CK_Building_ID))
                                                   , total.count = sum(Count)
                                                   , p.h = count / total.count), stringsAsFactors = F)

    }else if(valueVariable == "Wifi.Ind"){
      StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                   , c("BuildingType", "State", "Region", "Territory", rowVariable)
                                                   , summarise
                                                   , count = sum(get(valueVariable))
                                                   , n_hj = length(unique(CK_Building_ID)) 
                                                   , total.count = sum(Count)
                                                   , p.h = count / total.count), stringsAsFactors = F)
      
    }else{
      StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                   , c("BuildingType", "State", "Region", "Territory", rowVariable)
                                                   , summarise
                                                   , count = sum(get(valueVariable))
                                                   , n_hj = length(unique(CK_Building_ID)) ), stringsAsFactors = F)
      
      StrataProportion         <- data.frame(ddply(StrataGroupedProportions
                                                   , c("BuildingType", "State", "Region", "Territory")
                                                   , summarise
                                                   , total.count = sum(count)), stringsAsFactors = F)
      StrataGroupedProportions     <- left_join(StrataGroupedProportions, StrataProportion)
      StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
      StrataGroupedProportions$p.h[which(StrataGroupedProportions$p.h == "NaN")] <- 0
      
    }
    
    
    
    # Analysis for any column variable that is not state should include columnVariable as a grouping variable
  }else if(columnVariable %in% c("Cooling.Zone", "CK_Building_ID") & valueVariable == "Ind"){
    StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                 , summarise
                                                 , count = sum(get(valueVariable))
                                                 , n_hj = length(unique(CK_Building_ID))
                                                 , total.count = sum(Count)
                                                 , p.h = count / total.count), stringsAsFactors = F)
    
  }else if(columnVariable %in% c("HomeType","Lamp.Category") & valueVariable %in% c("Ind")){
    StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                 , summarise
                                                 , count = sum(get(valueVariable))
                                                 , n_hj = length(unique(CK_Building_ID))), stringsAsFactors = F)
    StrataProportion         <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType", "State", "Region", "Territory", columnVariable)
                                                 , summarise
                                                 , total.count = sum(Count)), stringsAsFactors = F)
    
    StrataGroupedProportions     <- left_join(StrataGroupedProportions, StrataProportion)
    StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
    
  }else if(columnVariable %in% c("HomeType","Lamp.Category","Status") & valueVariable %in% c("StorageBulbs", "Lamps")){
    StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                 , summarise
                                                 , count = sum(get(valueVariable))
                                                 , n_hj = length(unique(CK_Building_ID))), stringsAsFactors = F)
    StrataProportion         <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType", "State", "Region", "Territory", columnVariable)
                                                 , summarise
                                                 , total.count = sum(get(valueVariable))), stringsAsFactors = F)
    StrataProportion <- StrataProportion[which(StrataProportion$total.count > 0),]
    StrataGroupedProportions     <- left_join(StrataProportion, StrataGroupedProportions)
    StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
    # StrataGroupedProportions$p.h[which(StrataGroupedProportions$p.h == "NaN")] <- 0
    
  }else if(columnVariable %in% c("TankSize", "Washer.Age","Heating_System","Primary.Heating.System")){
    StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType","Territory", rowVariable, columnVariable)
                                                 , summarise
                                                 , count = sum(get(valueVariable))
                                                 , n_hj = length(unique(CK_Building_ID))), stringsAsFactors = F)
    
    StrataProportion         <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType","Territory")
                                                 , summarise
                                                 , total.count = sum(count)), stringsAsFactors = F)
    
    StrataGroupedProportions     <- left_join(StrataGroupedProportions, StrataProportion)
    StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
    
  }else if(columnVariable %in% c("System.Type")){
    #Summarise
    StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                 ,summarise
                                                 ,count = sum(get(valueVariable))
                                                 ,n_hj  = length(unique(CK_Building_ID))), stringsAsFactors = F)
    
    #Summarise
    StrataProportion         <- data.frame(ddply(StrataGroupedProportions
                                                 , c("BuildingType", "State", "Region", "Territory", columnVariable)
                                                 ,summarise
                                                 ,total.count = sum(count)), stringsAsFactors = F)
    
    #Join Data
    StrataGroupedProportions <- left_join(StrataGroupedProportions, StrataProportion)
    
    #Summarise
    StrataGroupedProportions <- data.frame(ddply(StrataGroupedProportions
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                 ,summarise
                                                 ,count = count
                                                 ,n_hj  = n_hj
                                                 ,p.h   = count / total.count), stringsAsFactors = F)
    StrataGroupedProportions$p.h[which(StrataGroupedProportions$p.h == "NaN")] <- 0
    
  }else{
    #Summarise
    StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                 ,summarise
                                                 ,count = sum(get(valueVariable))
                                                 ,n_hj  = length(unique(CK_Building_ID))), stringsAsFactors = F)
    
    #Summarise
    StrataProportion         <- data.frame(ddply(StrataGroupedProportions
                                                 , c("BuildingType", "State", "Region", "Territory", columnVariable)
                                                 ,summarise
                                                 ,total.count = sum(count)), stringsAsFactors = F)
    
    #Join Data
    StrataGroupedProportions <- left_join(StrataGroupedProportions, StrataProportion)
    
    #Summarise
    StrataGroupedProportions <- data.frame(ddply(StrataGroupedProportions
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                 ,summarise
                                                 ,count = count
                                                 ,n_hj  = n_hj
                                                 ,p.h   = count / total.count), stringsAsFactors = F)
    StrataGroupedProportions$p.h[which(StrataGroupedProportions$p.h == "NaN")] <- 0
  } # END if else statement to get count, total count, and proportion
  
  #join strata counts with summary of grouping variable within strata
  StrataData <- left_join(StrataPopCounts , StrataGroupedProportions)
  
  #################################################################################
  #obtain the total population size for the strata and columnVariable combination
  #################################################################################
  if(columnVariable %in% c("Heating_System", "Primary.Heating.System", "Washer.Age")){
    StrataData_n <- unique(StrataData[which(colnames(StrataData) %in% c("BuildingType"
                                                                        ,"State"
                                                                        ,"Region"
                                                                        ,"Territory"
                                                                        ,"N.h"
                                                                        ,"n.h"))])
    columnVarWeights <- data.frame(ddply(StrataData_n, c("BuildingType")
                                         ,summarise
                                         ,columnVar.N.h = sum(N.h)
                                         ,columnVar.n.h = sum(n.h)), stringsAsFactors = F)

    #join strata data with weights by column grouping variable
    StrataDataWeights <- left_join(StrataData, columnVarWeights, by = c("BuildingType"))

  }else{
    StrataData_n <- unique(StrataData[which(colnames(StrataData) %in% c("BuildingType"
                                                                        ,"State"
                                                                        ,"Region"
                                                                        ,"Territory"
                                                                        ,columnVariable
                                                                        ,"N.h"
                                                                        ,"n.h"))])
    columnVarWeights <- data.frame(ddply(StrataData_n, c("BuildingType",columnVariable)
                                         ,summarise
                                         ,columnVar.N.h = sum(N.h)
                                         ,columnVar.n.h = sum(n.h)), stringsAsFactors = F)
    #join strata data with weights by column grouping variable 
    StrataDataWeights <- left_join(StrataData, columnVarWeights, by = c("BuildingType",columnVariable))
    
  }

  
  
  ####################################################################################################
  #calculate weighted percent and weighted standard errors grouping by both column and row variables
  ####################################################################################################
  
  if (columnVariable %in% c("Cooling.Zone","State","System.Type") & valueVariable == "Ind"){
    ColumnProportionsByGroup <- data.frame(ddply(StrataDataWeights
                                                 , c("BuildingType", columnVariable, rowVariable)
                                                 , summarise
                                                 ,w.percent = sum(N.h * p.h, na.rm = T) / sum(N.h)
                                                 ,w.SE      = sqrt(sum((1 - n.h / N.h) * 
                                                                         (N.h^2 / n.h) * 
                                                                         (p.h * (1 - p.h)), na.rm = T)) / sum(N.h)
                                                 ,count     = sum(count)
                                                 ,N         = sum(N.h)
                                                 ,n         = sum(n_hj)
                                                 ,EB   = w.SE * qt(1-(1-0.9)/2, n)
    ), stringsAsFactors = F)
    
    # calculate column totals
    ColumnTotals <- data.frame(ddply(StrataDataWeights
                                     , c("BuildingType", columnVariable)
                                     ,summarise
                                     ,rowTotal       = "Total"
                                     ,w.percent = sum(N.h * p.h) / unique(columnVar.N.h)
                                     ,w.SE      = sqrt(sum((1 - n.h / N.h) * 
                                                             (N.h^2 / n.h) * 
                                                             (p.h * (1 - p.h)), na.rm = T)) / unique(columnVar.N.h)
                                     ,count     = sum(count)
                                     ,N         = unique(columnVar.N.h)
                                     ,n         = sum(n_hj)
                                     ,EB   = w.SE * qt(1-(1-0.9)/2, n)), stringsAsFactors = F) 
  }else if(columnVariable %in% c("Heating_System","Primary.Heating.System")){
    #say i did this for washer.age
    ColumnProportionsByGroup <- data.frame(ddply(StrataDataWeights
                                                 , c("BuildingType", columnVariable, rowVariable)
                                                 , summarise
                                                 ,w.percent = sum(N.h * p.h, na.rm = T) / unique(columnVar.N.h) #sum(N.h)
                                                 ,w.SE      = sqrt(sum((1 - n.h / N.h) * 
                                                                         (N.h^2 / n.h) * 
                                                                         (p.h * (1 - p.h)), na.rm = T)) / unique(columnVar.N.h)
                                                 ,count     = sum((count))
                                                 # ,col.N     = unique(columnVar.N.h)
                                                 ,N         = sum((N.h))
                                                 ,n         = sum((n_hj))
                                                 ,EB   = w.SE * qt(1-(1-0.9)/2, n)
                                                 # ,n         = unique(columnVar.n.h)
    ), stringsAsFactors = F)
    
    # calculate column totals
    ColumnTotals <- data.frame(ddply(ColumnProportionsByGroup
                                     , c("BuildingType", columnVariable)
                                     ,summarise
                                     ,rowTotal       = "Total"
                                     ,w.percent      = sum(w.percent)
                                     ,w.SE           = sqrt(w.percent * (1 - w.percent) / sum(n, na.rm = T))
                                     ,count          = sum(count, na.rm = T)
                                     # ,n              = sum(n_hj, na.rm = T)
                                     ,n              = sum((n), na.rm = T)
                                     ,N              = sum(unique(N), na.rm = T)
                                     ,EB   = w.SE * qt(1-(1-0.9)/2, n)), stringsAsFactors = F) 
  }else {
    ColumnProportionsByGroup <- data.frame(ddply(StrataDataWeights
                                                 , c("BuildingType", columnVariable, rowVariable)
                                                 , summarise
                                                 ,w.percent = sum(N.h * p.h) / unique(columnVar.N.h) #sum(N.h)
                                                 ,w.SE      = sqrt(sum((1 - n.h / N.h) * 
                                                                         (N.h^2 / n.h) * 
                                                                         (p.h * (1 - p.h)), na.rm = T)) / unique(columnVar.N.h)
                                                 ,count     = sum((count))
                                                 # ,col.N     = unique(columnVar.N.h)
                                                 ,N         = sum((N.h))
                                                 ,n         = sum((n_hj))
                                                 ,EB   = w.SE * qt(1-(1-0.9)/2, n)
                                                 # ,n         = unique(columnVar.n.h)
    ), stringsAsFactors = F)
    
    # calculate column totals
    ColumnTotals <- data.frame(ddply(ColumnProportionsByGroup
                                     , c("BuildingType", columnVariable)
                                     ,summarise
                                     ,rowTotal       = "Total"
                                     ,w.percent      = sum(w.percent)
                                     ,w.SE           = NA
                                     ,count          = sum(count, na.rm = T)
                                     # ,n              = sum(n_hj, na.rm = T)
                                     ,n              = sum((n), na.rm = T)
                                     ,N              = sum(unique(N), na.rm = T)
                                     ,EB   = w.SE * qt(1-(1-0.9)/2, n)), stringsAsFactors = F) 
  }
  
  #rename column
  ColumnTotals <- ConvertColName(ColumnTotals, 'rowTotal', rowVariable)
  
  
  #join total information onto summary by grouping variables
  AllRowsFinal  <- rbind.data.frame(ColumnProportionsByGroup, ColumnTotals, stringsAsFactors = F)
  
  
  ########################################################################
  #obtain the total population size for the strata observed in the sample
  ########################################################################
  StrataData_n_agg <- unique(StrataData[which(colnames(StrataData) %in% c("BuildingType"
                                                                          ,"State"
                                                                          ,"Region"
                                                                          ,"Territory"
                                                                          ,"N.h"
                                                                          ,"n.h"))])
  AggregateWeight <- ddply(StrataData_n_agg, c("BuildingType")
                           ,summarise
                           ,aggregate.N.h = sum(N.h, na.rm = T)
                           ,aggregate.n.h = sum(n.h, na.rm = T))
  
  #join strata data onto region weights
  item.agg.join <- left_join(StrataData, AggregateWeight, by = c("BuildingType"))
  
  
  #summarise by second grouping variable
  # if (columnVariable == "Something"){
  #   item.agg.weighted <- ddply(item.agg.join, c("BuildingType", rowVariable), summarise
  #                              ,aggregateName = aggregateColumnName
  #                              ,w.percent = sum(N.h * p.h) / sum(N.h)#unique(aggregate.N.h)
  #                              ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / sum(N.h)#unique(aggregate.N.h)
  #                              ,count     = sum(count)
  #                              ,N         = unique(aggregate.N.h)
  #                              ,n         = sum(n_hj)
  #                              ,EB   = w.SE * qt(1-(1-0.9)/2, n)
  #                              # ,n         = unique(aggregate.n.h)
  #   )
  # }else{
    item.agg.weighted <- ddply(item.agg.join, c("BuildingType", rowVariable), summarise
                               ,aggregateName = aggregateColumnName
                               ,w.percent = sum(N.h * p.h) / unique(aggregate.N.h)
                               ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(aggregate.N.h)
                               ,count     = sum(count)
                               ,N         = unique(aggregate.N.h)
                               ,n         = sum(n_hj)
                               ,EB   = w.SE * qt(1-(1-0.9)/2, n)
                               # ,n         = unique(aggregate.n.h)
    )
  # }

  #rename column
  colnames(item.agg.weighted)[which(colnames(item.agg.weighted) == 'aggregateName')] <- columnVariable
  
  #summarise at the total level
  item.agg.tot <- ddply(item.agg.weighted, c("BuildingType", columnVariable), summarise
                        ,rowTotal = "Total"
                        ,w.percent = sum(w.percent)
                        ,w.SE      = NA
                        ,count     = sum(count, na.rm = T)
                        # ,n         = sum((n), na.rm = T)
                        ,n         = sum((n), na.rm = T)
                        ,N         = sum(unique(N), na.rm = T)
                        ,EB   = w.SE * qt(1-(1-0.9)/2, n))
  #rename column
  colnames(item.agg.tot)[which(colnames(item.agg.tot) == 'rowTotal')]   <- rowVariable
  
  #join table with total info
  item.agg.full <- rbind.data.frame(item.agg.weighted ,item.agg.tot, stringsAsFactors = F)
  
  #reassign
  item.agg.final <- item.agg.full
  
  
  #################################################################
  # Combine calculations made at the column variable level with 
  # calculations made across the column variables
  #################################################################
  item.full <- rbind.data.frame(AllRowsFinal, item.agg.final, stringsAsFactors = F)
  item.full <- item.full[which(colnames(item.full) != "Total.Count")]
  
  return(item.full)
}







#################################################################################################################
#Function: proportions_two_groups_domain
#Used For: 
# 
# 
#################################################################################################################
#weighted function for means with one grouping variable
proportions_two_groups_domain <- function(CustomerLevelData
                                   , valueVariable
                                   , byVariableRow
                                   , byVariableColumn
                                   , aggregateColumn = NA
                                   , aggregateRow    = NA) {
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ### Get appropriate sample and population sizes for the strata and the domain
  strata_domain_level <- data.frame(ddply(CustomerLevelData
                                          , c("BuildingType", "State", "Region", "Territory"), summarise
                                          ,n_l        = unique(n.h)
                                          ,N_l        = sum(N.h)), stringsAsFactors = F)
  
  ### Get sum and mean of metrics when applicable as well as the strata-domain sample size and unit size
  strata_domain_summary1    <- data.frame(ddply(CustomerLevelData
                                                , c("BuildingType", "State", "Region", "Territory", byVariableRow, byVariableColumn), summarise
                                                ,n_lk    = length(unique(CK_Building_ID))
                                                ,N_lk    = unique(N.h) * n_lk / unique(n.h)
                                                ,m_lk    = sum(m_ilk)
                                                ,m_bar_lk    = sum(m_ilk) / unique(n.h)
  ), stringsAsFactors = F)
  ### Get sum and mean of metrics when applicable as well as the strata-domain sample size and unit size
  strata_domain_summary2    <- data.frame(ddply(CustomerLevelData
                                                , c("BuildingType", "State", "Region", "Territory"), summarise
                                                # ,n_l          = length(unique(CK_Building_ID))
                                                ,m_l          = sum(m_ilk)
  ), stringsAsFactors = F)
  strata_domain_summary <- left_join(strata_domain_summary1, strata_domain_summary2)
  strata_domain_summary$p_hat_ilk <- strata_domain_summary$m_lk / strata_domain_summary$m_l
  
  ## QAQC
  sum(strata_domain_summary$p_hat_ilk[which(strata_domain_summary$BuildingType == "Single Family" & strata_domain_summary$State == "WA" & strata_domain_summary$Region == "PS" & strata_domain_summary$Territory == "BPA")])
  
  strata_domain_merge      <- left_join(strata_domain_summary, strata_domain_level)
  site_strata_domain_merge <- left_join(CustomerLevelData, strata_domain_merge)
  
  
  ##################################################################################################x
  # Domain estimation
  ##################################################################################################x
  ### Get esimtated number of units in the population and estimated population average of the metric
  domain_summary <- data.frame(ddply(site_strata_domain_merge
                                     , c("BuildingType", byVariableRow, byVariableColumn), summarise
                                     ,M_hat_k = sum(sum(N_l / n_l * m_ilk))
  ), stringsAsFactors = F)
  
  domain_summary1 <- data.frame(ddply(domain_summary
                                      , c("BuildingType", byVariableColumn), summarise
                                      ,M_hat = sum(M_hat_k)
  ), stringsAsFactors = F)
  ## QAQC - should be equal to 100% across domains within building type
  sum(domain_summary1$M_hat[which(domain_summary1$BuildingType == "Single Family")]) / 4254404 #Note that sum(strata_domain_level$N_l[which(strata_domain_level$BuildingType == "Single Family")]) = 4254404
  
  
  domain_merge <- left_join(domain_summary, domain_summary1)
  domain_merge$p_hat_k <- domain_merge$M_hat_k / domain_merge$M_hat
  sum(domain_merge$p_hat_k)
  
  strata_domain_merge <- left_join(strata_domain_merge, domain_merge)
  site_strata_domain_merge <- left_join(site_strata_domain_merge, domain_merge)
  ### calculate the inner sum to get the between-strata variance at the domain level
  across_strata_estimation <- data.frame(ddply(site_strata_domain_merge
                                               , c("BuildingType",byVariableRow,byVariableColumn), summarise
                                               ,var = sum(1 / M_hat^2 * N_l^2 / (n_l * (n_l -1)) * (1 - n_l / N_l) * (m_ilk - m_bar_lk)^2)
                                               ,SE = sqrt(var)
                                               ,EB = SE * 1.645
  ), stringsAsFactors = F)
  
  
  domain_variance_merge <- left_join(domain_merge, across_strata_estimation)
  domain_variance_merge <- domain_variance_merge[which(colnames(domain_variance_merge) %notin% c("M_hat_k","M_hat","var"))]
  colnames(domain_variance_merge)[which(colnames(domain_variance_merge) %in% c("p_hat_k","SE"))] <- c("w.percent","w.SE")
  
  across_domain_variance_merge <- data.frame(ddply(domain_variance_merge
                                                   , c("BuildingType",byVariableColumn), summarise
                                                   ,byRow = aggregateRow
                                                   # ,byCol = aggregateColumn
                                                   ,w.percent = sum(w.percent)
                                                   ,w.SE = NA
                                                   ,EB   = NA
  ), stringsAsFactors = F)
  
  colnames(across_domain_variance_merge)[which(colnames(across_domain_variance_merge) == 'byRow')] <- byVariableRow
  # colnames(across_domain_variance_merge)[which(colnames(across_domain_variance_merge) == 'byCol')] <- byVariableColumn

  
  
  domain_estimation_final <- rbind.data.frame(domain_variance_merge, across_domain_variance_merge)



  # ##################################################################################################x
  # # Across Domain estimation (ACROSS ROWS)
  # ##################################################################################################x
  # ### calculate the inner sum to get the between-strata variance across domains
  # across_domain_rows_summary <- data.frame(ddply(site_strata_domain_merge
  #                                                , c("BuildingType", byVariableColumn), summarise
  #                                                , byRow              = aggregateRow
  #                                                , M_hat_k = sum(sum(N_l / n_lk * m_ilk))
  #                                                , y_bar_hat_k  = (1 / M_hat_k) * sum(sum(N_l / n_lk * get(valueVariable)))
  # ), stringsAsFactors = F)
  # 
  # 
  # ### calculate the inner sum to get the between-strata variance at the domain level
  # across_domain_rows_estimation1 <- data.frame(ddply(strata_domain_merge
  #                                                    , c("BuildingType", byVariableColumn), summarise
  #                                                    , byRow              = aggregateRow
  #                                                    , sum_2 = sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * n_lk * (y_bar_lk - y_bar_hat_k)^2)
  # ), stringsAsFactors = F)
  # ### calculate the inner sum to get the between-strata variance at the domain level
  # across_domain_rows_estimation2 <- data.frame(ddply(site_strata_domain_merge
  #                                                    , c("BuildingType", byVariableColumn), summarise
  #                                                    , byRow              = aggregateRow
  #                                                    ,sum_1 = sum(sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * (1 / n_lk) * (1 - n_l / N_l) * (y_bar_ilk - y_bar_lk)^2 / (n_lk )))
  # ), stringsAsFactors = F)
  # 
  # across_domain_rows_variance_merge <- left_join(across_domain_rows_estimation1, across_domain_rows_estimation2)
  # across_domain_rows_variance_merge <- left_join(across_domain_rows_variance_merge, across_domain_rows_summary)
  # across_domain_rows_variance_merge$var_hat_y_bar_hat_k <- (across_domain_rows_variance_merge$sum_1 + across_domain_rows_variance_merge$sum_2) / across_domain_rows_variance_merge$M_hat_k^2
  # across_domain_rows_variance_merge$SE_y_bar_hat_k <- sqrt(across_domain_rows_variance_merge$var_hat_y_bar_hat_k)
  # across_domain_rows_variance_merge$EB_y_bar_hat_k <- across_domain_rows_variance_merge$SE_y_bar_hat_k * 1.645
  # across_domain_rows_variance_merge$Precision <- across_domain_rows_variance_merge$EB_y_bar_hat_k / across_domain_rows_variance_merge$y_bar_hat_k
  # 
  # #rename columns
  # colnames(across_domain_rows_variance_merge)[which(colnames(across_domain_rows_variance_merge) == 'byRow')] <- byVariableRow



  ##################################################################################################x
  # Across Domain estimation (ACROSS COLUMNS)
  ##################################################################################################x
  ### Get esimtated number of units in the population and estimated population average of the metric
  across_domain_cols_summary <- data.frame(ddply(site_strata_domain_merge
                                                 , c("BuildingType", byVariableRow), summarise
                                                 , byCol              = aggregateColumn
                                                 ,M_hat_k = sum(sum(N_l / n_l * m_ilk))
  ), stringsAsFactors = F)
  
  across_domain_cols_summary1 <- data.frame(ddply(across_domain_cols_summary
                                                  , byCol              = aggregateColumn
                                                  , c("BuildingType"), summarise
                                                  ,M_hat = sum(M_hat_k)
  ), stringsAsFactors = F)
  ## QAQC - should be equal to 100% across domains within building type
  sum(across_domain_cols_summary1$M_hat[which(across_domain_cols_summary1$BuildingType == "Single Family")]) / 4254404 #Note that sum(strata_across_domain_cols_level$N_l[which(strata_across_domain_cols_level$BuildingType == "Single Family")]) = 4254404
  
  
  across_domain_cols_merge <- left_join(across_domain_cols_summary, across_domain_cols_summary1)
  across_domain_cols_merge$p_hat_k <- across_domain_cols_merge$M_hat_k / across_domain_cols_merge$M_hat
  sum(across_domain_cols_merge$p_hat_k)
  
  strata_across_domain_cols_merge <- left_join(strata_domain_merge, across_domain_cols_merge)
  site_strata_across_domain_cols_merge <- left_join(site_strata_domain_merge, across_domain_cols_merge)
  ### calculate the inner sum to get the between-strata variance at the domain level
  across_strata_estimation <- data.frame(ddply(site_strata_across_domain_cols_merge
                                               , c("BuildingType",byVariableRow), summarise
                                               , byCol              = aggregateColumn
                                               ,var = sum(1 / M_hat^2 * N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * (m_ilk - m_bar_lk)^2)
                                               ,SE = sqrt(var)
                                               ,EB = SE * 1.645
  ), stringsAsFactors = F)
  
  
  across_domain_cols_variance_merge <- left_join(across_domain_cols_merge, across_strata_estimation)
  across_domain_cols_variance_merge <- across_domain_cols_variance_merge[which(colnames(across_domain_cols_variance_merge) %notin% c("M_hat_k","M_hat","var"))]
  colnames(across_domain_cols_variance_merge)[which(colnames(across_domain_cols_variance_merge) %in% c("p_hat_k","SE"))] <- c("w.percent","w.SE")
  
  across_across_domain_cols_variance_merge <- data.frame(ddply(across_domain_cols_variance_merge
                                                               , c("BuildingType"), summarise
                                                               , byCol              = aggregateColumn
                                                               , byRow = aggregateRow
                                                               # ,byCol = aggregateColumn
                                                               ,w.percent = sum(w.percent)
                                                               ,w.SE = NA
                                                               ,EB   = NA
  ), stringsAsFactors = F)
  colnames(across_across_domain_cols_variance_merge)[which(colnames(across_across_domain_cols_variance_merge) == 'byRow')] <- byVariableRow
  

  across_columns_domain_estimation_final <- rbind.data.frame(across_domain_cols_variance_merge, across_across_domain_cols_variance_merge)
  colnames(across_columns_domain_estimation_final)[which(colnames(across_columns_domain_estimation_final) == 'byCol')] <- byVariableColumn
  


  # ##################################################################################################x
  # # Across Domain estimation (ACROSS COLUMNS)
  # ##################################################################################################x
  # ### calculate the inner sum to get the between-strata variance across domains
  # across_both_summary <- data.frame(ddply(site_strata_domain_merge
  #                                         , c("BuildingType"), summarise
  #                                         , byRow = aggregateRow
  #                                         , byCol = aggregateColumn
  #                                         , M_hat_k = sum(sum(N_l / n_lk * m_ilk))
  #                                         , y_bar_hat_k  = (1 / M_hat_k) * sum(sum(N_l / n_lk * get(valueVariable)))
  # ), stringsAsFactors = F)
  # 
  # 
  # ### calculate the inner sum to get the between-strata variance at the domain level
  # across_both_estimation1 <- data.frame(ddply(strata_domain_merge
  #                                             , c("BuildingType"), summarise
  #                                             , byRow = aggregateRow
  #                                             , byCol = aggregateColumn
  #                                             , sum_2 = sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * n_lk * (y_bar_lk - y_bar_hat_k)^2)
  # ), stringsAsFactors = F)
  # ### calculate the inner sum to get the between-strata variance at the domain level
  # across_both_estimation2 <- data.frame(ddply(site_strata_domain_merge
  #                                             , c("BuildingType"), summarise
  #                                             , byRow = aggregateRow
  #                                             , byCol = aggregateColumn
  #                                             ,sum_1 = sum(sum(N_l^2 / (n_l * (n_l )) * (1 - n_l / N_l) * (1 / n_lk) * (1 - n_l / N_l) * (y_bar_ilk - y_bar_lk)^2 / (n_lk )))
  # ), stringsAsFactors = F)
  # 
  # across_both_variance_merge <- left_join(across_both_estimation1, across_both_estimation2)
  # across_both_variance_merge <- left_join(across_both_variance_merge, across_both_summary)
  # across_both_variance_merge$var_hat_y_bar_hat_k <- (across_both_variance_merge$sum_1 + across_both_variance_merge$sum_2) / across_both_variance_merge$M_hat_k^2
  # across_both_variance_merge$SE_y_bar_hat_k <- sqrt(across_both_variance_merge$var_hat_y_bar_hat_k)
  # across_both_variance_merge$EB_y_bar_hat_k <- across_both_variance_merge$SE_y_bar_hat_k * 1.645
  # across_both_variance_merge$Precision <- across_both_variance_merge$EB_y_bar_hat_k / across_both_variance_merge$y_bar_hat_k
  # 
  # #rename columns
  # colnames(across_both_variance_merge)[which(colnames(across_both_variance_merge) == 'byCol')] <- byVariableColumn
  # colnames(across_both_variance_merge)[which(colnames(across_both_variance_merge) == 'byRow')] <- byVariableRow

  
  
  
  
  
  ##################################################################################################x
  # Combining Domain and Across Domain Estimation
  ##################################################################################################x
  ### Merge estimation infomration together
  item.estimation <- rbind.data.frame(domain_estimation_final
                                      , across_columns_domain_estimation_final
                                      , stringsAsFactors = F)
  
  #obatin correct sample sizes for
  item.samplesize <- data.frame(ddply(CustomerLevelData
                                      , c("BuildingType",byVariableRow,byVariableColumn), summarise
                                      ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)
  #obatin correct sample sizes for
  rows.samplesize <- data.frame(ddply(CustomerLevelData
                                      , c("BuildingType",byVariableRow), summarise
                                      ,byCol = aggregateColumn
                                      ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)
  # obatin correct sample sizes for
  cols.samplesize <- data.frame(ddply(CustomerLevelData
                                      , c("BuildingType",byVariableColumn), summarise
                                      ,byRow = aggregateRow
                                      ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)
  domain.samplesize <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType"), summarise
                                        ,byRow = aggregateRow
                                        ,byCol = aggregateColumn
                                        ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)


  colnames(domain.samplesize)[which(colnames(domain.samplesize) == 'byRow')] <- byVariableRow
  colnames(domain.samplesize)[which(colnames(domain.samplesize) == 'byCol')] <- byVariableColumn
  colnames(rows.samplesize)[which(colnames(rows.samplesize) == 'byCol')] <- byVariableColumn
  colnames(cols.samplesize)[which(colnames(cols.samplesize) == 'byRow')] <- byVariableRow
  
  samplesizes <- rbind.data.frame(item.samplesize,rows.samplesize,cols.samplesize,domain.samplesize, stringsAsFactors = F)
  
  
  ### Add sample sizes onto final data
  item.final <- data.frame(item.estimation, stringsAsFactors = F)
  item.final <- left_join(item.final, samplesizes)
  
  # item.cast <- dcast(setDT(item.final)
  #                    ,formula = BuildingType + get(byVariableRow) ~ get(byVariableColumn)
  #                    ,value.var = c("w.percent", "w.SE", "n", "EB"))
  # item.cast <- ConvertColName(item.cast, 'byVariableRow',byVariableRow)

  
  return(item.final)
}








#################################################################################################################
#Function: proportions_two_groups_unweighted
#Used For: 
# 
# 
#################################################################################################################
proportions_two_groups_unweighted <- function(CustomerLevelData
                                              , valueVariable
                                              , columnVariable
                                              , rowVariable
                                              , aggregateColumnName){
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ########################################################################
  # Obtain correct population and sample size information within strata
  ########################################################################
  
  #count and sample size by building types and both grouping variables
  item.unweighted1 <- data.frame(ddply(CustomerLevelData, c("BuildingType", columnVariable, rowVariable), summarise
                                       ,Count      = sum(get(valueVariable))
                                       ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)
  
  #count and sample size by building type and column variable (total across rows)
  item.unweighted2 <- data.frame(ddply(CustomerLevelData, c("BuildingType", columnVariable), summarise
                                       ,rowTotal = "Total"
                                       ,Count = sum(get(valueVariable))
                                       ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)
  #rename column
  item.unweighted2 <- ConvertColName(item.unweighted2,'rowTotal',rowVariable)
  
  #count and sample size by building type and row variable
  item.unweighted3 <- data.frame(ddply(CustomerLevelData, c("BuildingType", rowVariable), summarise
                                       ,colTotal = aggregateColumnName
                                       ,Count = sum(get(valueVariable))
                                       ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)
  #rename column
  item.unweighted3 <- ConvertColName(item.unweighted3,'colTotal',columnVariable)
  
  #count and sample size by onlyl building types (total across rows and columns)
  item.unweighted4 <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                       ,colTotal = aggregateColumnName
                                       ,rowTotal = "Total"
                                       ,Count = sum(get(valueVariable))
                                       ,n = length(unique(CK_Building_ID))), stringsAsFactors = F)
  #rename columns
  item.unweighted4 <- ConvertColName(item.unweighted4,'rowTotal',rowVariable)
  item.unweighted4 <- ConvertColName(item.unweighted4,'colTotal',columnVariable)
  
  #row bind all calculations
  item.combined <- rbind.data.frame(item.unweighted1, item.unweighted2,
                                    item.unweighted3, item.unweighted4, stringsAsFactors = F)
  
  #get information for total rows only and rename then to get the total count (for the proportion)
  item.totals <- rbind.data.frame(item.unweighted2, item.unweighted4, stringsAsFactors = F)
  #subset to only column names needed (i.e. building type, count, and the column variable)
  item.totals <- item.totals[which(colnames(item.totals) %in% c("BuildingType",columnVariable, "Count"))]
  colnames(item.totals) <- c("BuildingType",columnVariable, "Total.Count")
  
  
  #join the total count onto the counts and sample sizes
  item.final         <- left_join(item.combined, item.totals, by = c("BuildingType",columnVariable))
  
  if(columnVariable %in% c("System.Type") & rowVariable %in% c("Heating.Fuel")){
    item.final$Percent <- item.final$Count / sum(item.final$Count[which(item.final$Heating.Fuel == "Total" & item.final$System.Type == "All Systems")])
  }else if(columnVariable %in% c("Heating_System") & rowVariable %in% c("Fuel")){
    item.final$Percent <- item.final$Count / sum(item.final$Count[which(item.final$Fuel %in% c("Total","All Fuels") & item.final$Heating_System %in% c("All Systems", "Total"))])
  }else if(columnVariable %in% c("Primary.Heating.System") & rowVariable %in% c("Primary.Heating.Fuel")){
    item.final$Percent <- item.final$Count / sum(item.final$Count[which(item.final$Primary.Heating.Fuel == "Total" & item.final$Primary.Heating.System == "All Systems")])
  }else if(columnVariable == "System.Type" & rowVariable == "HomeType"){
    item.final$Percent <- item.final$Count / sum(item.final$Count[which(item.final$HomeType == "Total" & item.final$System.Type == "All Systems")])
  }else if(columnVariable == "TankSize" & rowVariable == "DHW.Fuel"){
    item.final$Percent <- item.final$Count / sum(item.final$Count[which(item.final$DHW.Fuel == "Total" & item.final$TankSize == "All Sizes")])
  }else if(columnVariable == "Washer.Age" & rowVariable == "Washer.Type"){
    item.final$Percent <- item.final$Count / sum(item.final$Count[which(item.final$Washer.Type == "Total" & item.final$Washer.Age == "All Vintages")])
  }else if(columnVariable == "Cooling.Zone" & rowVariable == "State"){
    item.final$Percent <- item.final$Count / item.final$n
  }else if(columnVariable == "State" & rowVariable == "Ownership.Type" & valueVariable == "count"){
    item.final$Percent <- item.final$Count / item.final$Total.Count
  }else if(columnVariable == "State" & rowVariable == "Ownership.Type"){
    item.final$Percent <- item.final$Count / item.final$n
  }else {
    item.final$Percent <- item.final$Count / item.final$Total.Count
  }
  
  item.final$SE      <- sqrt(item.final$Percent * (1 - item.final$Percent) / item.final$n)
  
  item.final <- item.final[which(colnames(item.final) != "Total.Count")]
  
  return(item.final)
}










#################################################################################
#Function: proportions_one_group
#  this function is applicable when percentages sum to 100% within a row
#################################################################################
proportions_one_group_within_row <- function(CustomerLevelData
                                             , valueVariable
                                             , groupingVariable
                                             , total.name
                                             , columnName
                                             , weighted  = TRUE
                                             , two.prop.total = NA) {
  
  # Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  
  if (weighted == TRUE){
    #sample and pop sizes within defined strata - this is to account for the fact that not all categories from each table will be observed in each strata
    StrataPopCounts <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType", "State", "Region", "Territory"), summarise
                                        ,N.h   = unique(N.h)
                                        ,n.h   = unique(n.h)), stringsAsFactors = F)
    
    if(groupingVariable == "Washer.Type"){
      StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                   , c("BuildingType", "State", "Region", "Territory", groupingVariable), summarise
                                                   ,count = sum(get(valueVariable))
                                                   ,n_hj  = length(unique(CK_Building_ID))), stringsAsFactors = F)
      
      
      StrataProportion <- data.frame(ddply(StrataGroupedProportions
                                           , c("BuildingType", "State", "Region", "Territory"), summarise
                                           ,total.count = sum(count)), stringsAsFactors = F)
      
      StrataGroupedProportions     <- left_join(StrataGroupedProportions, StrataProportion)
      StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
      
    }else{
      StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                   , c("BuildingType", "State", "Region", "Territory", groupingVariable), summarise
                                                   ,count = sum(get(valueVariable))), stringsAsFactors = F)
      
      
      StrataProportion <- data.frame(ddply(StrataGroupedProportions
                                           , c("BuildingType", "State", "Region", "Territory"), summarise
                                           ,total.count = sum(count)), stringsAsFactors = F)
      
      StrataGroupedProportions     <- left_join(StrataGroupedProportions, StrataProportion)
      StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
      
      Strata_n <- data.frame(ddply(CustomerLevelData
                                   , c("BuildingType")
                                   , summarise
                                   , n_hj = length(unique(CK_Building_ID))
      ))
      
      StrataGroupedProportions <- left_join(StrataGroupedProportions, Strata_n)
    }
    
    
    #join strata counts with summary of grouping variable within strata
    StrataData <- left_join(StrataPopCounts, StrataGroupedProportions)
    
    #obtain the total population size for the building type by state combination observed in the sample
    StrataData_n <- unique(StrataData[which(colnames(StrataData) %in% c("BuildingType"
                                                                        ,"State"
                                                                        ,"Region"
                                                                        ,"Territory"
                                                                        ,"N.h"
                                                                        ,"n.h"))])
    columnVarWeights <- data.frame(ddply(StrataData_n, c("BuildingType"),summarise
                                         ,columnVar.N.h = sum(N.h)), stringsAsFactors = F)
    
    
    #join strata data with weights by column grouping variable 
    StrataDataWeights <- left_join(StrataData, columnVarWeights, by = c("BuildingType"))
    
    
    #summarise by column variable
    #summary of both grouping variables
    if(groupingVariable == "Washer.Type"){
      ColumnProportionsByGroup <- data.frame(ddply(StrataDataWeights
                                                   , c("BuildingType", groupingVariable), summarise
                                                   ,w.percent = sum(N.h * p.h) / unique(columnVar.N.h)
                                                   ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(columnVar.N.h)
                                                   ,count     = sum(count)
                                                   ,N         = unique(columnVar.N.h)
                                                   ,n         = sum(n_hj)
                                                   ,EB   = w.SE * qt(1-(1-0.9)/2, n)), stringsAsFactors = F)
      
      #summarise across home types (total level)
      ColumnTotals <- data.frame(ddply(ColumnProportionsByGroup, "BuildingType", summarise
                                       ,rowTotal       = "Total"
                                       ,w.percent      = sum(w.percent)
                                       ,w.SE           = NA
                                       ,count          = sum(count, na.rm = T)
                                       ,n              = sum(unique(n))
                                       ,N              = unique(N)
                                       ,EB   = w.SE * qt(1-(1-0.9)/2, n)), stringsAsFactors = F) 
      #rename column
      ColumnTotals <- ConvertColName(ColumnTotals, 'rowTotal', groupingVariable)
    }else{
      ColumnProportionsByGroup <- data.frame(ddply(StrataDataWeights
                                                   , c("BuildingType", groupingVariable), summarise
                                                   ,w.percent = sum(N.h * p.h) / unique(columnVar.N.h)
                                                   ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(columnVar.N.h)
                                                   ,count     = sum(count)
                                                   ,N         = unique(columnVar.N.h)
                                                   ,n         = unique(n_hj)
                                                   ,EB   = w.SE * qt(1-(1-0.9)/2, n)), stringsAsFactors = F)
      
      #summarise across home types (total level)
      ColumnTotals <- data.frame(ddply(ColumnProportionsByGroup, "BuildingType", summarise
                                       ,rowTotal       = "Total"
                                       ,w.percent      = sum(w.percent)
                                       ,w.SE           = NA
                                       ,count          = sum(count, na.rm = T)
                                       ,n              = unique(n)
                                       ,N              = unique(N)
                                       ,EB   = w.SE * qt(1-(1-0.9)/2, n)), stringsAsFactors = F) 
      #rename column
      ColumnTotals <- ConvertColName(ColumnTotals, 'rowTotal', groupingVariable)
    }
    
    #join total information onto summary by grouping variables
    AllRowsFinal  <- rbind.data.frame(ColumnProportionsByGroup, 
                                      ColumnTotals, stringsAsFactors = F)
    
    if(!is.na(two.prop.total)){
      AllRowsFinal$tmp.total <- total.name
      AllRowsFinal <- ConvertColName(AllRowsFinal, 'tmp.total', columnName)
    }
    
    item.full <- data.frame(AllRowsFinal, stringsAsFactors = F)
    item.full <- item.full[which(colnames(item.full) != "Total.Count")]
    return(item.full)
    
    #For Unweighted
  } else {
    item.tmp1 <- data.frame(ddply(CustomerLevelData, c("BuildingType", groupingVariable), summarise
                                  ,n = length(unique(CK_Building_ID))
                                  ,Count       = sum(get(valueVariable))), stringsAsFactors = F)
    
    
    item.tmp2 <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                  ,Total = "Total"
                                  ,n = length(unique(CK_Building_ID))
                                  ,Count   = sum(get(valueVariable))), stringsAsFactors = F)
    
    # Convert column name
    item.tmp2 <- ConvertColName(item.tmp2, 'Total', groupingVariable)
    
    item.combined <- rbind.data.frame(item.tmp1, item.tmp2, stringsAsFactors = F)
    
    
    if(valueVariable == "StorageBulbs"){
      item.tmpyy <- data.frame(ddply(CustomerLevelData, c("BuildingType", "State"), summarise
                                     ,Total.Count   = sum(TotalBulbs)), stringsAsFactors = F)
      item.tmpxx <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                     ,State = "Total"
                                     ,Total.Count = sum(TotalBulbs)), stringsAsFactors = F)
      
      item.tmp3 <- rbind.data.frame(item.tmpyy, item.tmpxx, stringsAsFactors = F)
    }else if(groupingVariable == "HomeType" & valueVariable == "Number.of.Units"){
      item.tmp3 <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                    ,Total.Count = sum(Number.of.Units, na.rm = T)), stringsAsFactors = F)
    }else if(groupingVariable == "HomeType" & valueVariable != "Number.of.Units"){
      item.tmpyy <- data.frame(ddply(CustomerLevelData, c("BuildingType", "HomeType"), summarise
                                     ,Total.Count   = sum(Count)), stringsAsFactors = F)
      item.tmpxx <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                     ,HomeType = "Total"
                                     ,Total.Count = sum(Count)), stringsAsFactors = F)
      item.tmp3 <- rbind.data.frame(item.tmpyy, item.tmpxx, stringsAsFactors = F)
    }else{
      item.tmp3 <- data.frame(ddply(CustomerLevelData, "BuildingType", summarise
                                    ,Total.Count   = sum(get(valueVariable))), stringsAsFactors = F)
    }
    
    
    item.final <- left_join(item.combined, item.tmp3)
    
    if(!is.na(two.prop.total)){
      item.final$tmp.total <- total.name
      item.final <- ConvertColName(item.final, 'tmp.total', columnName)
    }
    
    item.final <- data.frame(item.final, stringsAsFactors = F)
    
    
    if(groupingVariable == "State"){
      
      # this if statement is specifically to calculate the denominator for tables
      # where we want to know the percent of stored bulbs by bulb type
      if(valueVariable == "StorageBulbs"){
        get.unweighted.proportion <- data.frame(ddply(CustomerLevelData
                                                      , c("BuildingType", "State"), summarise
                                                      ,Percent = sum(get(valueVariable))/ sum(TotalBulbs)), stringsAsFactors = F)
        
        item.final <- left_join(item.final, get.unweighted.proportion)
        
        #calculate percent
        item.final$Percent[which(is.na(item.final$Percent))] <- item.final$Count[which(is.na(item.final$Percent))] / item.final$Total.Count[which(is.na(item.final$Percent))]
        
      }else {
        item.final$Percent <- item.final$Count / item.final$n
      }
    }else{
      item.final$Percent <- item.final$Count / item.final$Total.Count
    }
    
    item.final$SE      <- sqrt(item.final$Percent * (1 - item.final$Percent) / item.final$n)
    
    item.final <- item.final[which(colnames(item.final) != "Total.Count")]
    return(item.final)
  }
}





#################################################################################
#Function: proportionRowsAndColumns1_within_row
# For when percentages sum to 100% within rows
#################################################################################
proportionRowsAndColumns1_within_row <- function(CustomerLevelData
                                                 , valueVariable
                                                 , columnVariable
                                                 , rowVariable
                                                 , aggregateColumnName){
  
  ### Function to convert column names
  ConvertColName <- function(dataset, currentColName, newColName) {
    data <- dataset
    colnames(data)[which(colnames(data) == currentColName)] <- newColName
    return(data)
  }
  
  ########################################################################
  # Obtain correct population and sample size information within strata
  ########################################################################
  # this if statement is because insulation tables
  ############# FIGURE OUT WHY
  
  if (rowVariable == "rvalue.bins"){
    StrataPopCounts <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType", "State", "Region", "Territory", columnVariable)
                                        , summarise
                                        ,N.h   = unique(N.h)
                                        ,n.h   = unique(n.h)), stringsAsFactors = F)
  }  else {
    StrataPopCounts <- data.frame(ddply(CustomerLevelData
                                        , c("BuildingType", "State", "Region", "Territory")
                                        , summarise
                                        ,N.h   = unique(N.h)
                                        ,n.h   = unique(n.h)), stringsAsFactors = F)
  }
  
  
  #####################################################################
  # Obtain count, total count, and proportions at the correct levels
  #####################################################################
  # If state is the column variable, we need to perform all analyses without it included as a group_by variable
  # Otherwise it will duplicate the State column
  if (columnVariable == "State") {
    StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable)
                                                 , summarise
                                                 , count = sum(get(valueVariable))), stringsAsFactors = F)
    
    StrataProportion         <- data.frame(ddply(StrataGroupedProportions
                                                 , c("BuildingType", "State", "Region", "Territory", columnVariable)
                                                 , summarise
                                                 , total.count = sum(count)), stringsAsFactors = F)
    
    StrataGroupedProportions     <- left_join(StrataGroupedProportions, StrataProportion)
    StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
    
    # Analysis for any column variable that is not state should include columnVariable as a grouping variable
  }else if(columnVariable == "Cooling.Zone" & valueVariable == "Ind"){
    StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                 , summarise
                                                 , count = sum(get(valueVariable))
                                                 , total.count = sum(Count)
                                                 , p.h = count / total.count), stringsAsFactors = F)
    
  }else if(columnVariable %in% c("System.Type", "TankSize", "Washer.Age")){
    StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType","Territory", rowVariable, columnVariable)
                                                 , summarise
                                                 , count = sum(get(valueVariable))), stringsAsFactors = F)
    
    StrataProportion         <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType","Territory")
                                                 , summarise
                                                 , total.count = sum(count)), stringsAsFactors = F)
    
    StrataGroupedProportions     <- left_join(StrataGroupedProportions, StrataProportion)
    StrataGroupedProportions$p.h <- StrataGroupedProportions$count / StrataGroupedProportions$total.count
    
  }else if(rowVariable == "Number.of.Units" & valueVariable == "Count"){
    #Summarise
    StrataGroupedProportions_tmp <- data.frame(ddply(CustomerLevelData
                                                     , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                     ,summarise
                                                     ,count = sum(get(valueVariable))), stringsAsFactors = F)
    
    #Summarise
    StrataProportion         <- data.frame(ddply(StrataGroupedProportions_tmp
                                                 , c("BuildingType", "State", "Region", "Territory", columnVariable)
                                                 ,summarise
                                                 ,total.count = sum(count)), stringsAsFactors = F)
    
    #Join Data
    StrataGroupedProportions_tmp <- left_join(StrataGroupedProportions_tmp, StrataProportion)
    
    #Summarise
    StrataGroupedProportions <- data.frame(ddply(StrataGroupedProportions_tmp
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                 ,summarise
                                                 ,count = count
                                                 ,p.h   = count / total.count), stringsAsFactors = F)
    StrataGroupedProportions <- StrataGroupedProportions[which(StrataGroupedProportions$p.h != "NaN"),]
    # stopifnot(sum(StrataGroupedProportions$p.h[which(StrataGroupedProportions$BuildingType == "Multifamily" & StrataGroupedProportions$HomeYearBuilt_bins_MF == "1955-1970")]) == nrow(unique(CustomerLevelData[which(colnames(CustomerLevelData) == rowVariable)])))
    
  }else{
    #Summarise
    StrataGroupedProportions <- data.frame(ddply(CustomerLevelData
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                 ,summarise
                                                 ,count = sum(get(valueVariable))), stringsAsFactors = F)
    
    #Summarise
    StrataProportion         <- data.frame(ddply(StrataGroupedProportions
                                                 , c("BuildingType", "State", "Region", "Territory", columnVariable)
                                                 ,summarise
                                                 ,total.count = sum(count)), stringsAsFactors = F)
    
    #Join Data
    StrataGroupedProportions <- left_join(StrataGroupedProportions, StrataProportion)
    
    #Summarise
    StrataGroupedProportions <- data.frame(ddply(StrataGroupedProportions
                                                 , c("BuildingType", "State", "Region", "Territory", rowVariable, columnVariable)
                                                 ,summarise
                                                 ,count = count
                                                 ,p.h   = count / total.count), stringsAsFactors = F)
    StrataGroupedProportions$p.h[which(StrataGroupedProportions$p.h == "NaN")] <- 0
  } # END if else statement to get count, total count, and proportion
  
  
  Strata_n <- data.frame(ddply(CustomerLevelData
                               , c("BuildingType", columnVariable)
                               , summarise
                               , n_hj = length(unique(CK_Building_ID))
  ))
  
  StrataGroupedProportions <- left_join(StrataGroupedProportions, Strata_n)
  
  #join strata counts with summary of grouping variable within strata
  StrataData <- left_join(StrataPopCounts, StrataGroupedProportions)
  
  #################################################################################
  #obtain the total population size for the strata and columnVariable combination
  #################################################################################
  StrataData_n0 <- unique(StrataData[which(colnames(StrataData) %in% c("BuildingType"
                                                                       ,"State"
                                                                       ,"Region"
                                                                       ,"Territory"
                                                                       ,columnVariable
                                                                       ,"N.h"))])
  # column.var.ind <- unique(CustomerLevelData[which(colnames(CustomerLevelData) == columnVariable)])
  # StrataData_n <- StrataData_n0[rep(seq_len(nrow(StrataData_n0)), each = nrow(column.var.ind)),]
  # StrataData_n <- cbind.data.frame(StrataData_n, column.var.ind)
  # 
  columnVarWeights <- data.frame(ddply(StrataData_n0, c("BuildingType", columnVariable)
                                       ,summarise
                                       ,columnVar.N.h = sum(N.h)), stringsAsFactors = F)
  # 
  # #join strata data with weights by column grouping variable 
  StrataDataWeights <- left_join(StrataData, columnVarWeights, by = c("BuildingType", columnVariable))
  
  ####################################################################################################
  #calculate weighted percent and weighted standard errors grouping by both column and row variables
  ####################################################################################################
  ColumnProportionsByGroup <- data.frame(ddply(StrataDataWeights
                                               , c("BuildingType", columnVariable, rowVariable)
                                               , summarise
                                               ,w.percent = sum(N.h * p.h) / unique(columnVar.N.h)
                                               ,w.SE      = sqrt(sum((1 - n.h / N.h) * 
                                                                       (N.h^2 / n.h) * 
                                                                       (p.h * (1 - p.h)), na.rm = T)) / unique(columnVar.N.h)
                                               ,count     = sum(count)
                                               ,N         = unique(columnVar.N.h)
                                               ,n         = unique(n_hj)
                                               ,EB   = w.SE * qt(1-(1-0.9)/2, n)), stringsAsFactors = F)
  if (columnVariable == "Cooling.Zone" & valueVariable == "Ind"){
    # calculate column totals
    ColumnTotals <- data.frame(ddply(StrataData
                                     , c("BuildingType", columnVariable)
                                     ,summarise
                                     ,rowTotal       = "Total"
                                     ,w.percent = sum(N.h * p.h) / unique(columnVar.N.h)
                                     ,w.SE      = sqrt(sum((1 - n.h / N.h) * 
                                                             (N.h^2 / n.h) * 
                                                             (p.h * (1 - p.h)), na.rm = T)) / unique(columnVar.N.h)
                                     ,count     = sum(count)
                                     ,N         = unique(columnVar.N.h)
                                     ,n         = sum(n_hj)
                                     ,EB   = w.SE * qt(1-(1-0.9)/2, n)), stringsAsFactors = F) 
  }else {
    # calculate column totals
    ColumnTotals <- data.frame(ddply(ColumnProportionsByGroup
                                     , c("BuildingType", columnVariable)
                                     ,summarise
                                     ,rowTotal       = "Total"
                                     ,w.percent      = sum(w.percent)
                                     ,w.SE           = NA
                                     ,count          = sum(count, na.rm = T)
                                     ,n              = sum(unique(n), na.rm = T)
                                     ,N              = sum(unique(N), na.rm = T)
                                     ,EB   = w.SE * qt(1-(1-0.9)/2, n)), stringsAsFactors = F) 
  }
  
  #rename column
  ColumnTotals <- ConvertColName(ColumnTotals, 'rowTotal', rowVariable)
  
  
  #join total information onto summary by grouping variables
  AllRowsFinal  <- rbind.data.frame(ColumnProportionsByGroup, ColumnTotals, stringsAsFactors = F)
  
  
  ########################################################################
  #obtain the total population size for the strata observed in the sample
  ########################################################################
  StrataData_n_agg <- unique(StrataData[which(colnames(StrataData) %in% c("BuildingType"
                                                                          ,"State"
                                                                          ,"Region"
                                                                          ,"Territory"
                                                                          ,"N.h"
                                                                          ,"n.h"))])
  AggregateWeight <- ddply(StrataData_n_agg, "BuildingType", summarise
                           ,aggregate.N.h = sum(N.h, na.rm = T)
                           ,aggregate.n.h = sum(n.h, na.rm = T))
  
  #join strata data onto region weights
  item.agg.join <- left_join(StrataData, AggregateWeight, by = c("BuildingType"))
  
  
  #summarise by second grouping variable
  item.agg.weighted <- ddply(item.agg.join, c("BuildingType", rowVariable), summarise
                             ,aggregateName = aggregateColumnName
                             ,w.percent = sum(N.h * p.h) / unique(aggregate.N.h)
                             ,w.SE      = sqrt(sum((1 - n.h / N.h) * (N.h^2 / n.h) * (p.h * (1 - p.h)))) / unique(aggregate.N.h)
                             ,count     = sum(count)
                             ,N         = unique(aggregate.N.h)
                             ,n         = unique(aggregate.n.h)
                             ,EB   = w.SE * qt(1-(1-0.9)/2, n))
  #rename column
  colnames(item.agg.weighted)[which(colnames(item.agg.weighted) == 'aggregateName')] <- columnVariable
  
  #summarise at the total level
  item.agg.tot <- ddply(item.agg.weighted, c("BuildingType", columnVariable), summarise
                        ,rowTotal = "Total"
                        ,w.percent = sum(w.percent)
                        ,w.SE      = NA
                        ,count     = sum(count, na.rm = T)
                        ,n         = sum(unique(n), na.rm = T)
                        ,N         = sum(unique(N), na.rm = T)
                        ,EB   = w.SE * qt(1-(1-0.9)/2, n))
  #rename column
  colnames(item.agg.tot)[which(colnames(item.agg.tot) == 'rowTotal')]   <- rowVariable
  
  #join table with total info
  item.agg.full <- rbind.data.frame(item.agg.weighted ,item.agg.tot, stringsAsFactors = F)
  
  #reassign
  item.agg.final <- item.agg.full
  
  
  #################################################################
  # Combine calculations made at the column variable level with 
  # calculations made across the column variables
  #################################################################
  item.full <- rbind.data.frame(AllRowsFinal, item.agg.final, stringsAsFactors = F)
  item.full <- item.full[which(colnames(item.full) != "Total.Count")]
  
  return(item.full)
}
