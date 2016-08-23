results_combined_standard <- boot(data=unique_data_ids, 
                                  statistic=sens_analysis, 
                                  R=1000, 
                                  calc_function = calc_combined,
                                  utility_rate_function = utility_rate_standard)

boot.ci(results_combined_standard)

results_indiv_standard <- boot(data=unique_data_ids,
                               statistic=sens_analysis,
                               R=1000,
                               calc_function = calc_individual,
                               utility_rate_function = utility_rate_standard)

boot.ci(results_indiv_standard)

x=seq(0, 3, by = 0.1)
lb= function(x) 0.4364*x + 0.1408
ub= function(x) 0.5971*x + 0.1592
val= function(x) 0.5182665*x + 0.1500715
Cooperative = data.frame(x, lb = lb(x), ub=ub(x), val=val(x))

x=seq(0, 3, by = 0.1)
lb= function(x) 0.6330*x + 0.0726
ub= function(x) 0.7421*x + 0.0856
val= function(x) 0.6885305*x + 0.0791854
Individual = data.frame(x, lb = lb(x), ub=ub(x), val=val(x))

eqs = bind_rows('Individual' = Individual, 'Cooperative' = Cooperative, .id = 'Model')

ggplot(eqs, aes(x=x)) +
        geom_line(aes(y=val, color=Model)) +
        geom_ribbon(aes(ymin=lb, ymax=ub, fill=Model), alpha=0.3) +
        geom_hline(yintercept = 0, alpha=0.3) +
        geom_vline(xintercept = 0, alpha=0.3) +
        xlab('Feed-in rate ($)') +
        ylab('Yearly return (times initial investment)')


