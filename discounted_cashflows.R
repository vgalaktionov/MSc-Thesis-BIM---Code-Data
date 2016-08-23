forecasted_dates = seq(ymd_h("2013-12-26 11"), length=(25 * 8766)+1, by="1 hour")


#############################################################################

Individual = lapply(indiv_standard_confint, calc_discounted_cashflows)
Individual$datetime = forecasted_dates

Cooperative = lapply(combined_standard_confint, calc_discounted_cashflows)
Cooperative$datetime = forecasted_dates


discounted_confint_standard = bind_rows('Individual' = Individual, 'Cooperative' = Cooperative, .id = 'Model')
names(discounted_confint_standard) = c('Model', 'estimate', 'lb', 'ub', 'datetime')
rm(Individual)
rm(Cooperative)


ggplot(discounted_confint_standard, aes(x=datetime)) +
        geom_line(aes(y=estimate, color=Model)) +
        geom_ribbon(aes(ymin=lb, ymax=ub, fill=Model), alpha=0.3) +
        geom_hline(yintercept = 0, alpha=0.3) +
        xlab('Date') +
        ylab('Total discounted cashflows ($)') +
        ggtitle('Discounted cashflows by model, standard utility rates') + 
        scale_y_continuous(labels = comma, breaks = c(-500000, 0, 500000, 1000000, 1500000, 2000000))
        

###############################################################################

Individual = lapply(indiv_tou_confint, calc_discounted_cashflows)
Individual$datetime = forecasted_dates

Cooperative = lapply(combined_tou_confint, calc_discounted_cashflows)
Cooperative$datetime = forecasted_dates


discounted_confint_tou = bind_rows('Individual' = Individual, 'Cooperative' = Cooperative, .id = 'Model')
names(discounted_confint_tou) = c('Model', 'estimate', 'lb', 'ub', 'datetime')
rm(Individual)
rm(Cooperative)

ggplot(discounted_confint_tou, aes(x=datetime)) +
        geom_line(aes(y=estimate, color=Model)) +
        geom_ribbon(aes(ymin=lb, ymax=ub, fill=Model), alpha=0.3) +
        geom_hline(yintercept = 0, alpha=0.3) +
        xlab('Date') +
        ylab('Total discounted cashflows ($)') +
        ggtitle('Discounted cashflows by model, time-of-use utility rates') + 
        scale_y_continuous(labels = comma, breaks = c(-500000, 0, 500000, 1000000, 1500000, 2000000))
