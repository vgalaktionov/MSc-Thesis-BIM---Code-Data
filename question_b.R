combined_nests = bootstrap_forecast(calc_combined, utility_rate_standard, nests=TRUE, n_iter = 1000)
combined_no_nests = bootstrap_forecast(calc_combined, utility_rate_standard, nests=FALSE, n_iter = 100)

###############################################################################################
ids = intersect(metadata_clean[metadata_clean$number_of_nests != 0,]$dataid, unique_data_ids)

sample_set = dataset[dataset$dataid %in% ids,]

calculated = calc_combined(sample_set, utility_rate_standard)

installed_cost = calc_installed_cost(sample_set)

forecasted = lifetime_projection(calculated$value, installed_cost, return_ts = FALSE)
plot.ts(forecasted)

combined_nests_observed_mean = forecasted

combined_nests_observed_npv = calc_npv(forecasted)

combined_nests_observed_payback = calc_payback_period(combined_nests_observed_mean)

combined_nests_observed_irr = calc_irr(combined_nests_observed_mean)

combined_nests_confint = calc_confidence_interval(combined_nests, combined_nests_observed_mean)

combined_nests_npv_lb = calc_npv(combined_nests_confint$`2.5%`)
combined_nests_npv_ub = calc_npv(combined_nests_confint$`97.5%`)

combined_nests_payback_lb = calc_payback_period(combined_nests_confint$`2.5%`)
combined_nests_payback_ub = calc_payback_period(combined_nests_confint$`97.5%`)

combined_nests_irr_lb = calc_irr(combined_nests_confint$`2.5%`)
combined_nests_irr_ub = calc_irr(combined_nests_confint$`97.5%`)

######################################################################

ids = intersect(metadata_clean[is.na(metadata_clean$number_of_nests),]$dataid, unique_data_ids)

sample_set = dataset[dataset$dataid %in% ids,]

calculated = calc_combined(sample_set, utility_rate_standard)

installed_cost = calc_installed_cost(sample_set)

forecasted = lifetime_projection(calculated$value, installed_cost, return_ts = FALSE)
plot.ts(forecasted)

combined_no_nests_observed_mean = forecasted

combined_no_nests_observed_npv = calc_npv(forecasted)

combined_no_nests_observed_payback = calc_payback_period(combined_no_nests_observed_mean)

combined_no_nests_observed_irr = calc_irr(combined_no_nests_observed_mean)

combined_no_nests_confint = calc_confidence_interval(combined_no_nests, combined_no_nests_observed_mean)

combined_no_nests_npv_lb = calc_npv(combined_no_nests_confint$`2.5%`)
combined_no_nests_npv_ub = calc_npv(combined_no_nests_confint$`97.5%`)

combined_no_nests_payback_lb = calc_payback_period(combined_no_nests_confint$`2.5%`)
combined_no_nests_payback_ub = calc_payback_period(combined_no_nests_confint$`97.5%`)

combined_no_nests_irr_lb = calc_irr(combined_no_nests_confint$`2.5%`)
combined_no_nests_irr_ub = calc_irr(combined_no_nests_confint$`97.5%`)

######################################################################