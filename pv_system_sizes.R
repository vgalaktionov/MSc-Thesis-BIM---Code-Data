library(dplyr)
library(psych)

survey_2013 = read_feather('survey_2013.feather')
survey_2014 = read_feather('survey_2014.feather')
data_all = read_feather('data_all.feather')

for_size <- data_all %>% group_by(dataid) %>% summarise(pv_size_max_yield = max(gen))
for_size <- na.omit(for_size)

reported_sizes <- select(survey_2013, dataid, pv_system_size)
reported_sizes <- rbind(reported_sizes, select(survey_2014, dataid, pv_system_size))
reported_sizes <- reported_sizes[!duplicated(reported_sizes$dataid),]

reported_sizes$pv_system_size <- gsub('[A-Za-z]', '', reported_sizes$pv_system_size)
reported_sizes$pv_system_size <- as.numeric(reported_sizes$pv_system_size)

reported_sizes <- mutate(reported_sizes, 
                         pv_system_size = ifelse(pv_system_size %% 1000 != pv_system_size, 
                                                 pv_system_size / 1000,
                                                 pv_system_size))
reported_sizes <- na.omit(reported_sizes)

for_size <- merge(for_size, reported_sizes, by = 'dataid')

for_size <- mutate(for_size, diff = pv_size_max_yield / pv_system_size)

outliers_removed <- for_size$diff[!for_size$diff %in% boxplot.stats(for_size$diff)$out]

describe(outliers_removed)

installed_cost <- data_all %>% group_by(dataid) %>% summarise(pv_sys_cost = (max(gen) / 0.85) * 5.1 * 1000)

installed_cost[is.na(installed_cost)] <- 0
