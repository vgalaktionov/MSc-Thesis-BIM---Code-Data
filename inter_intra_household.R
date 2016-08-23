library(ggplot2)
library(reshape2)

#Figure 1

dataset = prepare_data()

plot_1 = dataset %>% filter(year(datetime) == 2014, week(datetime) == 24, dataid %in% c(171, 114)) %>%
        select(datetime, dataid, use) %>%
        mutate(dataid = as.factor(dataid)) %>%
        melt(measure.vars = 'use')

dates = dataset[year(dataset$datetime) == 2014 & dataset$dataid == 114 & week(dataset$datetime) == 24,]$datetime

ggplot(data = plot_1,
       aes(x=datetime, y=value, colour = dataid)) +
        geom_line(size=1) +
        ggtitle ('Inter-household variability in power use patterns') +
        ylab('Power (kW)') +
        xlab('Date')+scale_colour_hc()+theme_hc()

plot_2 = dataset %>% filter(week(datetime) == 24, dataid == 171) %>%
        select(datetime, use) %>%
        mutate(year = as.factor(year(datetime))) %>%
        group_by(year) %>% mutate(datetime = dates) %>%
        ungroup() %>%
        melt(measure.vars = 'use')

ggplot(data = plot_2,
       aes(x=datetime, y=value, colour = year)) +
        geom_line(size=1) +
        ggtitle('Inter-annual variability in power use patterns') +
        ylab('Power (kW)') +
        xlab('Date')+scale_colour_hc()+theme_hc()
