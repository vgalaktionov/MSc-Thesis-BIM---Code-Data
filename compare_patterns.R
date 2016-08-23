horizon = 17533

historical_dates = tail(unique(dataset$datetime), horizon)

########################################################################

historical_estimates_standard = data.frame(datetime = historical_dates,
                                  Individual = 
                                          head(diff(indiv_standard_observed_mean + abs(indiv_standard_observed_mean[1])), horizon),
                                  Cooperative = 
                                          head(diff(combined_standard_observed_mean + abs(combined_standard_observed_mean[1])), horizon))



historical_estimates_standard = historical_estimates_standard %>%
        mutate(Difference = Cooperative - Individual) %>%
        melt('datetime')

ggplot(historical_estimates_standard, aes(x=datetime, y=value, color=variable)) + 
        geom_line() + 
        facet_wrap(~variable, ncol = 1) +
        guides(color=F) +
        ylab('Benefits ($)') +
        xlab('Date') +
        ggtitle('Benefits patterns by model, standard utility rate')




##################################################################


historical_estimates_tou = data.frame(datetime = historical_dates,
                                           Individual = 
                                                   head(diff(indiv_tou_observed_mean + abs(indiv_tou_observed_mean[1])), horizon),
                                           Cooperative = 
                                                   head(diff(combined_tou_observed_mean + abs(combined_tou_observed_mean[1])), horizon))



historical_estimates_tou = historical_estimates_tou %>%
        mutate(Difference = Cooperative - Individual) %>%
        melt('datetime')

ggplot(historical_estimates_tou, aes(x=datetime, y=value, color=variable)) + 
        geom_line() + 
        facet_wrap(~variable, ncol = 1) +
        guides(color=F) +
        ylab('Benefits ($)') +
        xlab('Date') +
        ggtitle('Benefits patterns by model, time-of-use utility rate')

##################################################################

historical_estimates_both = data.frame(datetime = historical_dates,
                                      indiv_tou = 
                                              head(diff(indiv_tou_observed_mean), horizon),
                                      coop_tou = 
                                              head(diff(combined_tou_observed_mean), horizon),
                                      indiv_standard = 
                                              head(diff(indiv_standard_observed_mean), horizon),
                                      coop_standard = 
                                              head(diff(combined_standard_observed_mean), horizon))



historical_estimates_both = historical_estimates_both %>%
        mutate('Individual difference' = indiv_standard - indiv_tou) %>%
        mutate('Cooperative difference' = coop_standard - coop_tou) %>%
        select(-c(indiv_tou,coop_tou,indiv_standard,coop_standard)) %>%
        melt('datetime')

ggplot(historical_estimates_both, aes(x=datetime, y=value, color=variable)) + 
        geom_line() + 
        facet_wrap(~variable, ncol = 1) +
        guides(color=F) +
        ylab('Benefits ($)') +
        xlab('Date') +
        ggtitle('Difference of benefits between rate structures by model')