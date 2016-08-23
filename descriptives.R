library(psych)
library(GGally)
library(grid)

nests_ids = metadata_clean[metadata_clean$number_of_nests != 0,]$dataid
no_nests_ids = metadata_clean[is.na(metadata_clean$number_of_nests),]$dataid

desc = dataset %>% mutate(nests = ifelse(dataid %in% intersect(nests_ids,all_data_ids), 1, 0),
       no_nests = ifelse(dataid %in% intersect(no_nests_ids,all_data_ids), 1, 0),
       gen = replace(gen, gen<0, 0))


desc_indiv = desc %>%
        mutate(grid_usage = ifelse(grid >= 0, grid, 0)) %>%
        mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
        group_by(dataid, year, month) %>%
        mutate(grid_monthly_cumsum = cumsum(grid_usage)) %>%
        select(-grid_usage) %>%
        ungroup() %>%
        calc_period_value(utility_rate_standard) %>%
        group_by(datetime) %>%
        summarise(indiv_benefits = sum(feedin_benefit) + sum(generated_benefit),
                  gen = sum(gen),
                  grid = sum(grid),
                  use = sum(use),
                  utility_rate_indiv = mean(utility_rate))

desc_combined <- desc %>%
        group_by(datetime) %>%
        summarize(use = sum(use), grid = sum(grid), gen = sum(gen)) %>% 
        mutate(grid_usage = ifelse(grid >= 0, grid, 0)) %>%
        mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
        group_by(year, month) %>%
        mutate(grid_monthly_cumsum = cumsum(grid_usage)) %>%
        select(-grid_usage) %>%
        ungroup() %>%
        calc_period_value(utility_rate_standard) %>%
        mutate(cooperative_value = feedin_benefit + generated_benefit)

desc_full = cbind(desc_indiv, coop_benefits = desc_combined$cooperative_value, utility_rate_coop = desc_combined$utility_rate)

ggpairs(desc_full, axisLabels = 'none') 

desc_2 = select(desc, use, gen, grid)

describe(desc_2)

desc_3 = select(desc, use, gen, grid, nests)

describeBy(desc_3, desc_3$nests)