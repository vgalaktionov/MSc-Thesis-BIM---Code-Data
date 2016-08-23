library(profvis)

dataset = prepare_data(testing = TRUE)

calculated = calc_individual(dataset, utility_rate_standard)

timeseries = msts(calculated$value, start = 2013, seasonal.periods = c(24, 8766))
timeseries = window(timeseries, start=c(2013,6))
timeseries = replace(timeseries, timeseries < 0, 0.01)

test = msts(tail(timeseries, 8766), start = c(2015,7), seasonal.periods = c(24, 8766))
train = msts(head(timeseries, length(timeseries) -8766), start = c(2013,6), seasonal.periods = c(24, 8766))

autoplot(train)+scale_color_fivethirtyeight()+theme_fivethirtyeight()+ggtitle('Historical Data')

profvis({train_forecast <- stlf(train, robust = TRUE, method = 'arima')}) ### 2400ms, 1.1972569 MASE, visual pass
accuracy(train_forecast, test)
plot.ts(c(timeseries, train_forecast$mean))
autoplot(train_forecast$mean)+scale_color_fivethirtyeight()+theme_fivethirtyeight()+ggtitle('STL + ARIMA')

profvis({train_forecast <- stlf(train, robust = TRUE, method = 'ets')}) ### 470ms, 1.2419793 MASE, visual pass
accuracy(train_forecast, test)
plot.ts(c(timeseries, train_forecast$mean))
autoplot(train_forecast$mean)+scale_color_fivethirtyeight()+theme_fivethirtyeight()+ggtitle('STL + ETS') 

profvis({train_forecast <- stlf(train, robust = TRUE, method = 'rwdrift')}) ### 100ms, 1.2622369 MASE, visual pass
accuracy(train_forecast, test)
plot.ts(c(timeseries, train_forecast$mean))
autoplot(train_forecast$mean)+scale_color_fivethirtyeight()+theme_fivethirtyeight()+ggtitle('STL + Naive with Drift') 

profvis({train_forecast <- forecast(tbats(train, num.cores = 3))}) ### 161670ms,  0.3655600 MASE, visual fail
accuracy(train_forecast, test)
plot.ts(c(timeseries, train_forecast$mean))
autoplot(train_forecast$mean)+scale_color_fivethirtyeight()+theme_fivethirtyeight()+ggtitle('TBATS') 

profvis({train_forecast <- forecast(nnetar(train))}) ### 204560ms, 1.3941295 MASE, visual fail
accuracy(train_forecast, test)
plot.ts(c(timeseries, train_forecast$mean))
autoplot(train_forecast$mean)+scale_color_fivethirtyeight()+theme_fivethirtyeight()+ggtitle('NNETAR')
