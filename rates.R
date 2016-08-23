library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(gridExtra)

rates_standard = data.frame(datetime = seq(ymd_h("2014-09-01 00"), length=1465, by="1 hour"),
                       tier1 = rep_len(250, 1465),
                       tier2 = rep_len(750, 1465),
                       tier3 = rep_len(1250, 1465),
                       tier4 = rep_len(1750, 1465),
                       tier5 = rep_len(3000, 1465))

rates_standard$`<501` = utility_rate_standard(rates_standard$datetime, rates_standard$tier1)
rates_standard$`501-1000` = utility_rate_standard(rates_standard$datetime, rates_standard$tier2) - utility_rate_standard(rates_standard$datetime, rates_standard$tier1)
rates_standard$`1001-1500` = utility_rate_standard(rates_standard$datetime, rates_standard$tier3) - utility_rate_standard(rates_standard$datetime, rates_standard$tier2)
rates_standard$`1501-2500` = utility_rate_standard(rates_standard$datetime, rates_standard$tier4) - utility_rate_standard(rates_standard$datetime, rates_standard$tier3)
rates_standard$`>2501` = utility_rate_standard(rates_standard$datetime, rates_standard$tier5) - utility_rate_standard(rates_standard$datetime, rates_standard$tier4)

rates_standard = melt(select(rates_standard, -c(tier1,tier2,tier3,tier4,tier5)), id.vars='datetime')

names(rates_standard) = c('Date','Usage Tier', 'Utility Rate ($/kWh)')

#####################################################################################

rates_tou = data.frame(datetime = seq(ymd_h("2014-09-01 00"), length=1465, by="1 hour"),
                            tier1 = rep_len(250, 1465),
                            tier2 = rep_len(750, 1465),
                            tier3 = rep_len(1250, 1465),
                            tier4 = rep_len(1750, 1465),
                            tier5 = rep_len(3000, 1465))

rates_tou$`<501` = utility_rate_tou(rates_tou$datetime, rates_tou$tier1)
rates_tou$`501-1000` = utility_rate_tou(rates_tou$datetime, rates_tou$tier2) - utility_rate_tou(rates_tou$datetime, rates_tou$tier1)
rates_tou$`1001-1500` = utility_rate_tou(rates_tou$datetime, rates_tou$tier3) - utility_rate_tou(rates_tou$datetime, rates_tou$tier2)
rates_tou$`1501-2500` = utility_rate_tou(rates_tou$datetime, rates_tou$tier4) - utility_rate_tou(rates_tou$datetime, rates_tou$tier3)
rates_tou$`>2501` = utility_rate_tou(rates_tou$datetime, rates_tou$tier5) - utility_rate_tou(rates_tou$datetime, rates_tou$tier4)

rates_tou = melt(select(rates_tou, -c(tier1,tier2,tier3,tier4,tier5)), id.vars='datetime')
names(rates_tou) = c('Date','Usage Tier', 'Utility Rate ($/kWh)')

grid.arrange(
  `Standard` = ggplot(data=rates_standard, aes(x=Date, y=`Utility Rate ($/kWh)`, fill=`Usage Tier`)) + geom_area() + xlab('') + ggtitle('Utility rates visualized') + scale_fill_brewer(palette="OrRd")+ theme(legend.position="none"),
  `Time of use` = ggplot(data=rates_tou, aes(x=Date, y=`Utility Rate ($/kWh)`, fill=`Usage Tier`)) + geom_area() + scale_fill_brewer(palette="OrRd") + theme(legend.position="bottom"),
  ncol=1
)
  