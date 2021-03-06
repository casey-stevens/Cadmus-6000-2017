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
  
  
  ### Get appropriate sample and population sizes for the strata and the domain
  strata_domain_level <- data.frame(ddply(CustomerLevelData
                                          , c("BuildingType", "State", "Region", "Territory"), summarise
                                          ,n_l        = unique(n.h)
                                          ,N_l        = unique(N.h)), stringsAsFactors = F)
  domain_level <- data.frame(ddply(strata_domain_level
                                   , c("BuildingType"), summarise
                                   ,n_k        = sum((n_l))
                                   ,N_k        = sum((N_l))), stringsAsFactors = F)
  
  
  
  ### Get sum and mean of metrics when applicable as well as the strata-domain sample size and unit size
  strata_domain_summary    <- data.frame(ddply(CustomerLevelData
                                               , c("BuildingType", "State", "Region", "Territory", byVariable), summarise
                                               ,y_lk     = sum(get(valueVariable), na.rm = T)
                                               ,y_bar_lk = mean(y_bar_ilk, na.rm = T)#sum(y_ilk, na.rm = T) / sum(m_ilk)
                                               ,n_lk     = length(unique(CK_Cadmus_ID))
                                               ,m_lk     = sum(m_ilk)
  ), stringsAsFactors = F)
  strata_domain_merge      <- left_join(strata_domain_summary, strata_domain_level)
  site_strata_domain_merge <- left_join(CustomerLevelData, strata_domain_merge)
  site_strata_domain_merge <- left_join(site_strata_domain_merge, domain_level)
  
  ### Get esimtated number of units in the population and estimated population average of the metric
  domain_summary <- data.frame(ddply(site_strata_domain_merge
                                     , c("BuildingType",byVariable), summarise
                                     ,M_hat_k = sum(unique(N_k) / unique(n_k) * sum(m_ilk))
                                     ,y_bar_hat_k  = (1 / M_hat_k) * sum(unique(N_k) / unique(n_k) * sum(y_ilk))
  ), stringsAsFactors = F)
  

  site_strata_domain_merge <- left_join(site_strata_domain_merge, domain_summary)
  
  
  ### calculate the inner sum to get the between-strata variance at the domain level
  strata_domain_estimation <- data.frame(ddply(site_strata_domain_merge
                                     , c("BuildingType","State","Region","Territory",byVariable), summarise
                                     ,inner_sum = sum(((y_bar_ilk - y_bar_lk)^2 / (n_lk )) + (n_lk * (1 - n_lk / n_l) * (y_bar_lk - y_bar_hat_k)^2))
  ), stringsAsFactors = F)
  
  site_strata_domain_merge <- left_join(site_strata_domain_merge, strata_domain_estimation)
  
  ### calculate the outer sum to get the within-strata variance, the combined total estimated variance, and the standard error at the domain level 
  domain_estimation <- data.frame(ddply(site_strata_domain_merge
                                               , c("BuildingType",byVariable), summarise
                                               ,outer_sum = sum((N_l^2 / (n_l * (n_l ))) * (1 - n_l / N_l) * ((1 / n_lk) * (1 - n_l / N_l) * inner_sum))
                                               ,var_hat_y_bar_hat_k = (1 / unique(M_hat_k)^2) * outer_sum
                                               ,SE_y_bar_hat_k = sqrt(var_hat_y_bar_hat_k)
  ), stringsAsFactors = F)
  
  ### calculate the inner sum to get the between-strata variance across domains
  across_domain_summary <- data.frame(ddply(site_strata_domain_merge
                                     , c("BuildingType"), summarise
                                     , byRow = aggregateRow
                                     ,M_hat_k = sum(unique(N_k) / unique(n_k) * sum(m_ilk))
                                     ,y_bar_hat_k  = (1 / M_hat_k) * sum(unique(N_k) / unique(n_k) * sum(y_ilk))
  ), stringsAsFactors = F)
  
  ### calculate the outer sum to get the within-strata variance, the combined total estimated variance, and the standard error across domains 
  across_domain_estimation <- data.frame(ddply(site_strata_domain_merge
                                     , c("BuildingType"), summarise
                                     , byRow  = aggregateRow
                                     ,outer_sum = sum((N_l^2 / (n_l * (n_l ))) * (1 - n_l / N_l) * ((1 / n_lk) * (1 - n_l / N_l) * inner_sum))
                                     ,var_hat_y_bar_hat_k = (1 / sum(unique(M_hat_k))^2) * outer_sum
                                     ,SE_y_bar_hat_k = sqrt(var_hat_y_bar_hat_k)
  ), stringsAsFactors = F)
  #rename columns
  colnames(across_domain_estimation)[which(colnames(across_domain_estimation) == 'byRow')] <- byVariable
  colnames(across_domain_summary)[which(colnames(across_domain_summary) == 'byRow')] <- byVariable
  
  
  ### Merge summary infomration together
  item.summary               <- rbind.data.frame(domain_summary, across_domain_summary, stringsAsFactors = F)
  item.summary$Clean.Type    <- as.character(item.summary$Clean.Type)
  
  ### Merge estimation infomration together
  item.estimation            <- rbind.data.frame(domain_estimation, across_domain_estimation, stringsAsFactors = F)
  item.estimation$Clean.Type <- as.character(item.estimation$Clean.Type)
  
  #obatin correct sample sizes for 
  samplesize.sub <- unique(site_strata_domain_merge[which(colnames(site_strata_domain_merge) %in% c("BuildingType",byVariable,"n_lk"))])
  item.samplesize <- data.frame(ddply(samplesize.sub
                                      , c("BuildingType",byVariable), summarise
                                      ,n = sum(n_lk)), stringsAsFactors = F)
  item.samplesize$Clean.Type <- as.character(item.samplesize$Clean.Type)
  # samplesize.sub <- strata_domain_level[which(colnames(strata_domain_level) %in% c("BuildingType","n_l"))]
  domain.samplesize <- data.frame(ddply(item.samplesize
                                      , c("BuildingType"), summarise
                                      ,byRow = aggregateRow
                                      ,n = max(n)), stringsAsFactors = F)
  colnames(domain.samplesize)[which(colnames(domain.samplesize) == 'byRow')] <- byVariable
  domain.samplesize$Clean.Type <- as.character(domain.samplesize$Clean.Type)
  samplesizes <- rbind.data.frame(item.samplesize, domain.samplesize, stringsAsFactors = F)
  
  ### Add sample sizes onto final data
  item.final <- left_join(item.summary, item.estimation)
  item.final <- left_join(item.final, samplesizes)
  names(item.final)[which(names(item.final) %in% c("y_bar_hat_k","SE_y_bar_hat_k"))] <- c("Mean","SE")
  item.final <- item.final[which(!colnames(item.final) %in% c("M_hat_k","outer_sum","var_hat_y_bar_hat_k"))]
  item.final$EB <- item.final$SE * qt(0.9, item.final$n - 1)
  return(item.final)
}

