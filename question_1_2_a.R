indiv_standard = bootstrap_forecast(calc_individual, utility_rate_tou,n_iter = 1000)
combined_standard = bootstrap_forecast(calc_combined, utility_rate_tou,n_iter = 1000)

indiv_tou = bootstrap_forecast(calc_individual, utility_rate_tou,n_iter = 1000)
combined_tou = bootstrap_forecast(calc_combined, utility_rate_tou,n_iter = 1000)

#########################################################################################
calculated = calc_individual(dataset, utility_rate_standard)

installed_cost = calc_installed_cost(dataset)

forecasted = lifetime_projection(calculated$value, installed_cost, return_ts = FALSE)
plot.ts(forecasted)

indiv_standard_observed_mean = forecasted

indiv_standard_observed_npv = calc_npv(forecasted)

indiv_standard_observed_payback = calc_payback_period(indiv_standard_observed_mean)

indiv_standard_observed_irr = calc_irr(indiv_standard_observed_mean)

indiv_standard_confint = calc_confidence_interval(indiv_standard, indiv_standard_observed_mean)

indiv_standard_npv_lb = calc_npv(indiv_standard_confint$`2.5%`)
indiv_standard_npv_ub = calc_npv(indiv_standard_confint$`97.5%`)

indiv_standard_payback_lb = calc_payback_period(indiv_standard_confint$`2.5%`)
indiv_standard_payback_ub = calc_payback_period(indiv_standard_confint$`97.5%`)

indiv_standard_irr_lb = calc_irr(indiv_standard_confint$`2.5%`)
indiv_standard_irr_ub = calc_irr(indiv_standard_confint$`97.5%`)

######################################################################

calculated = calc_combined(dataset, utility_rate_standard)

forecasted = lifetime_projection(calculated$value, installed_cost, return_ts = FALSE)
plot.ts(forecasted)

combined_standard_observed_mean = forecasted

combined_standard_observed_npv = calc_npv(forecasted)

combined_standard_observed_payback = calc_payback_period(combined_standard_observed_mean)

combined_standard_observed_irr = calc_irr(combined_standard_observed_mean)

combined_standard_confint = calc_confidence_interval(combined_standard, combined_standard_observed_mean)

combined_standard_npv_lb = calc_npv(combined_standard_confint$`2.5%`)
combined_standard_npv_ub = calc_npv(combined_standard_confint$`97.5%`)

combined_standard_payback_lb = calc_payback_period(combined_standard_confint$`2.5%`)
combined_standard_payback_ub = calc_payback_period(combined_standard_confint$`97.5%`)

combined_standard_irr_lb = calc_irr(combined_standard_confint$`2.5%`)
combined_standard_irr_ub = calc_irr(combined_standard_confint$`97.5%`)

######################################################################

calculated = calc_individual(dataset, utility_rate_tou)

forecasted = lifetime_projection(calculated$value, installed_cost, return_ts = FALSE)
plot.ts(forecasted)

indiv_tou_observed_mean = forecasted

indiv_tou_observed_npv = calc_npv(forecasted)

indiv_tou_observed_payback = calc_payback_period(indiv_tou_observed_mean)

indiv_tou_observed_irr = calc_irr(indiv_tou_observed_mean)

indiv_tou_confint = calc_confidence_interval(indiv_tou, indiv_tou_observed_mean)

indiv_tou_npv_lb = calc_npv(indiv_tou_confint$`2.5%`)
indiv_tou_npv_ub = calc_npv(indiv_tou_confint$`97.5%`)

indiv_tou_payback_lb = calc_payback_period(indiv_tou_confint$`2.5%`)
indiv_tou_payback_ub = calc_payback_period(indiv_tou_confint$`97.5%`)

indiv_tou_irr_lb = calc_irr(indiv_tou_confint$`2.5%`)
indiv_tou_irr_ub = calc_irr(indiv_tou_confint$`97.5%`)

######################################################################

calculated = calc_combined(dataset, utility_rate_tou)

forecasted = lifetime_projection(calculated$value, installed_cost, return_ts = FALSE)
plot.ts(forecasted)

combined_tou_observed_mean = forecasted

combined_tou_observed_npv = calc_npv(forecasted)

combined_tou_observed_payback = calc_payback_period(combined_tou_observed_mean)

combined_tou_observed_irr = calc_irr(combined_tou_observed_mean)

combined_tou_confint = calc_confidence_interval(combined_tou, combined_tou_observed_mean)

combined_tou_npv_lb = calc_npv(combined_tou_confint$`2.5%`)
combined_tou_npv_ub = calc_npv(combined_tou_confint$`97.5%`)

combined_tou_payback_lb = calc_payback_period(combined_tou_confint$`2.5%`)
combined_tou_payback_ub = calc_payback_period(combined_tou_confint$`97.5%`)

combined_tou_irr_lb = calc_irr(combined_tou_confint$`2.5%`)
combined_tou_irr_ub = calc_irr(combined_tou_confint$`97.5%`)

