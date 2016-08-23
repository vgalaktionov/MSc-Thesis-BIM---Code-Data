library(dplyr)
library(data.table)
library(dtplyr)
library(lubridate)
library(forecast)
library(feather)
library(FinCal)
library(matrixStats)
library(boot)
library(ggplot2)
library(scales)
library(reshape2)
library(tidyr)

utility_rates_summer <- c(0.03300,0.08000,0.09100,0.11000,0.11400)

utility_rates_winter <- c(0.01800,0.05600,0.07200, 0.08400,0.09600)

tou_rates_summer_offpeak <- c(0.00493,0.01188,0.02182,0.02679,0.06158)

tou_rates_summer_midpeak <- c(0.05040,0.06218,0.07134,0.07934,0.09512)

tou_rates_summer_onpeak <- c(0.09761,0.11003,0.12196,0.13031,0.14979)

tou_rates_winter_offpeak <- c(-0.00924,-0.00427,-0.00014,0.00692,0.04170)

tou_rates_winter_midpeak <- c(0.01201,0.03673,0.04891,0.06282,0.09761)

utility_rates_add <- 0.03139 + 0.00172 + 0.00093 + 0.00289 + 0.01414

feedin_rate <- 0.1090

hourly_interest = EIR(0.06, p = 8766)

# according to the austin rate structure
# For normal pricing
utility_rate_standard <- function(datetime, current_month_grid) {
        
        summer = ifelse(lubridate::month(datetime) %in% 6:9, TRUE, FALSE)
        usage_level = findInterval(current_month_grid, c(0, 501, 1001, 1501, 2501, Inf))
        utility_rate = ifelse(summer, 
                              utility_rates_summer[usage_level], 
                              utility_rates_winter[usage_level])
        
        return(utility_rate + utility_rates_add)
}

# For time-of-use pricing 
utility_rate_tou <- function(datetime, current_month_grid) {
        
        hour = lubridate::hour(datetime)
        summer = ifelse(lubridate::month(datetime) %in% 6:9, TRUE, FALSE)
        weekend = ifelse(lubridate::wday(datetime) %in% 6:7, TRUE, FALSE)
        usage_level = findInterval(current_month_grid, c(0, 501, 1001, 1501, 2501, Inf))
                
        utility_rate = ifelse(summer, 
                              ifelse(hour %in% 14:19 & !weekend, 
                                     tou_rates_summer_onpeak[usage_level], 
                                     ifelse(hour %in% 22:23 | hour %in% 0:6, 
                                            tou_rates_summer_offpeak[usage_level], 
                                            tou_rates_summer_midpeak[usage_level])),
                              ifelse(hour %in% 22:23 | hour %in% 0:6,
                                     tou_rates_winter_offpeak[usage_level],
                                     tou_rates_winter_midpeak[usage_level]))

        return(utility_rate + utility_rates_add)
}


# Load and filter accordingly
prepare_data <- function(testing=FALSE, present_whole_time = TRUE) {
        
        data_all = read_feather('datasets/data_all.feather')
        
        dataset <- data_all %>% 
                arrange(datetime) %>%
                select(-car1) %>%
                mutate(use = replace(use, use < 0, 0),
                       gen = replace(gen, is.na(gen), 0),
                       gen = replace(gen, gen <0, 0)
                       grid = replace(grid, is.na(grid), 0)) %>%
                group_by(dataid) %>% 
                filter(mean(gen) != 0) %>%
                ungroup()
        
        cutoff_date = if (testing) as.Date('2012-12-26') else as.Date('2013-12-26')
        
        #include only households present for the whole time
        if (present_whole_time) {
                households_start = unique(dataset[lubridate::date(dataset$datetime) == cutoff_date,]$dataid)
                households_end = unique(dataset[lubridate::date(dataset$datetime) == date(max(dataset$datetime)),]$dataid)
                dataset = filter(dataset, dataid %in% intersect(households_start, households_end))
        }

        dataset <- dataset %>% filter(lubridate::date(datetime) >= cutoff_date) %>%
                filter(date(datetime) < as.Date('2015-12-27'))
        
        rm(data_all)
        
        return(dataset)
}

# calculate installed cost
calc_installed_cost <- function(dataset) {
        
        dataset <- dataset %>% 
                group_by(dataid) %>% 
                summarise(pv_sys_cost = (max(gen) / 0.85) * 5.1 * 1000 * (1 - 0.84))
        return(sum(dataset$pv_sys_cost))
}

calc_installed_capacity <- function(dataset) {
        
        dataset <- dataset %>% 
                group_by(dataid) %>% 
                summarise(pv_sys_capacity = max(gen))
        return(sum(dataset$pv_sys_capacity))
}

# calculate utility rate, and then benefit (and cost if necessary)
calc_period_value <- function(dataset, utility_rate_function, cost = FALSE) {
        
        dataset <- dataset %>% mutate(utility_rate = utility_rate_function(datetime, grid_monthly_cumsum)) %>%
                mutate(feedin_benefit = ifelse(grid < 0, abs(grid) * feedin_rate, 0)) %>%
                mutate(generated_benefit = ifelse(grid < 0, use * utility_rate, gen * utility_rate))

        if (cost) {
                dataset <- dataset %>% mutate(grid_electricity_cost = ifelse(grid >= 0, grid * utility_rate,0))
        }
        
        return(dataset)
}

# calculate and aggregate hourly individual series
calc_individual <- function(dataset, utility_rate_function) {
        
        dataset <- dataset %>%
                mutate(grid_usage = ifelse(grid >= 0, grid, 0)) %>%
                mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
                group_by(dataid, year, month) %>%
                mutate(grid_monthly_cumsum = cumsum(grid_usage)) %>%
                select(-grid_usage) %>%
                ungroup() %>%
                calc_period_value(utility_rate_function) %>%
                group_by(datetime) %>%
                summarise(value = sum(feedin_benefit) + sum(generated_benefit)) 
        
        return(dataset)
}


# calculate hourly cooperative series
calc_combined <- function(dataset, utility_rate_function) {
        
        dataset <- dataset %>%
                group_by(datetime) %>%
                summarize(use = sum(use), grid = sum(grid), gen = sum(gen)) %>% 
                mutate(grid_usage = ifelse(grid >= 0, grid, 0)) %>%
                mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
                group_by(year, month) %>%
                mutate(grid_monthly_cumsum = cumsum(grid_usage)) %>%
                select(-grid_usage) %>%
                ungroup() %>%
                calc_period_value(utility_rate_function) %>%
                mutate(value = feedin_benefit + generated_benefit) 
        
        return(dataset)
}


# forecast 23 years with stl + arima
lifetime_projection <- function(value, installed_cost, hourly = TRUE, return_ts = TRUE) {
        freq = if (hourly) 8766 else 365
        timeseries = ts(tail(value, 17533), start = 2014, frequency = freq)
        #timeseries = window(timeseries, start=c(2014,11))
        timeseries = replace(timeseries, timeseries < 0, 0)
        forecast <- stlf(timeseries, robust = TRUE, method = 'arima', h = frequency(timeseries)*23)$mean
        output = c(timeseries, forecast)
        output = replace(output, output < 0, 0)
        output <- cumsum(output) - installed_cost
        if (return_ts) {
                output = ts(output, start = 2014, frequency = freq)
        }
        return(output)
}

# the bootstrap procedure
bootstrap_forecast <- function(calc_function, utility_rate_function, nests = NULL, n_iter=1000) {
        
        ids = unique(dataset$dataid)

        if (!is.null(nests)) {
                if(nests) {
                        ids = intersect(metadata_clean[metadata_clean$number_of_nests != 0,]$dataid, ids)
                } else {
                        ids = intersect(metadata_clean[is.na(metadata_clean$number_of_nests),]$dataid, ids)
                }
        }
        results = vector('list', n_iter)
        
        iter = 1
        while (iter <= n_iter) {
                
                sample_ids = sample(ids, length(ids), replace = TRUE)
                
                sample_set = lapply(seq_along(sample_ids), function (i) {
                        sample_set = dataset %>%
                                filter(dataid == sample_ids[i]) %>%
                                mutate(dataid = i) # regenerate dataid so stuff doesnt break
                })
                sample_set = bind_rows(sample_set)
                
                installed_cost = calc_installed_cost(sample_set)
                
                sample_set = calc_function(sample_set, utility_rate_function)
                
                results[[iter]] = lifetime_projection(sample_set$value, installed_cost, return_ts=FALSE)
                
                test = tail(results[[iter]], 8766)
                sensible_max = 0.7 * max(diff(head(results[[iter]], 8766)))
                
                failed_forecast = all(test == test[1])
                failed_trend_positive = min(diff(test)) > 0.1
                failed_trend_negative = max(diff(test)) < sensible_max
                
                if (!failed_forecast & !failed_trend_positive &  !failed_trend_negative) {
                        print(sprintf('%f percent done!', (iter/n_iter)*100))
                        iter = iter + 1
                }
        }
        output = as_data_frame(do.call(cbind, results))
        return(output)
}

# percentile intervals
calc_confidence_interval = function(dataset, observed, percentiles = c(0.025, 0.975)) {
        
        dataset_list = split(dataset, (as.numeric(rownames(dataset))-1) %/% 10000)
        
        rm(dataset)
        gc() # clear up some memory
        
        dataset_quantiles = vector(mode = 'list', length = length(dataset_list))
        
        for (i in seq_along(dataset_list)) {
                dataset_quantiles[[i]] = rowQuantiles(dataset_list[[i]], probs = percentiles)
        }
        dataset_quantiles = do.call(rbind, dataset_quantiles)
        
        confint = as_data_frame(cbind(observed,dataset_quantiles[1:219151,]))
        return(confint)
}

calc_npv = function(cashflows) {
        
        initial_cost = abs(cashflows[1])
        
        cashflows.diff = diff(cashflows + initial_cost)
        
        npv = npv(hourly_interest, c(-initial_cost, cashflows.diff))
        
        return(npv)
}

calc_discounted_cashflows = function(cashflows) {
        
        initial_cost = abs(cashflows[1])
        
        cashflows.diff = diff(cashflows + initial_cost)
        
        discounted = numeric()
        
        discounted = vector(mode = 'numeric', length = length(cashflows.diff))
        
        for (t in seq_along(discounted)) {
                discounted[t] = cashflows.diff[t] / ((1 + hourly_interest) ^ t)
        }
        
        discounted = cumsum(c(-initial_cost, discounted))
        
        return(discounted)
}

calc_payback_period = function(cashflows) {
        
        initial_cost = abs(cashflows[1])
        
        cashflows.diff = diff(cashflows + initial_cost)
        
        discounted = vector(mode = 'numeric', length = length(cashflows.diff))
        
        for (t in seq_along(discounted)) {
                discounted[t] = cashflows.diff[t] / ((1 + hourly_interest) ^ t)
        }
        
        discounted = cumsum(c(-initial_cost, discounted))
        
        payback = min(which(discounted >= 0)) / 8766
        
        return(payback)
}

calc_irr = function(cashflows) {
        
        initial_cost = abs(cashflows[1])
        
        cashflows_yearly = cashflows[seq(1, length(cashflows), 8766)]
        
        irr_yearly = irr(cashflows_yearly)
        
        return(irr_yearly)
}


sens_analysis = function(data, indices, calc_function, utility_rate_function, rr = TRUE) {
        
        ## Warning: this changes global variables!!!

        
        sample_set = lapply(seq_along(indices), function (i) {
                sample_set = dataset %>%
                        filter(dataid == data[indices[i]]) %>%
                        mutate(dataid = i) # regenerate dataid so stuff doesnt break
        })
        sample_set = bind_rows(sample_set)
        installed_cost = calc_installed_cost(sample_set)
        
        #rates_to_try = seq(0.07, 0.13, 0.01)
        
        yearly_benefit = sapply(seq(0.07, 0.13, 0.01), function(x) {
                
                feedin_rate <<- x
                
                calculated = calc_function(sample_set, utility_rate_function)
                
                if (rr) {
                        return((sum(calculated$value)/2)/installed_cost)
                }
                else {
                        return(mean(calculated$value))
                }
        })
        return(coef(lm(yearly_benefit ~ rates_to_try, data = data.frame(rates_to_try = seq(0.07, 0.13, 0.01), yearly_benefit))))
}


## Initial loading


if (!exists('dataset')) {
        dataset = prepare_data()
}
if (!exists('metadata_clean' )) {
        metadata_clean = read_feather('datasets/metadata_clean.feather')
}

unique_data_ids = unique(dataset$dataid)
