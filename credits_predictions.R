library(fpp3)


#Data Cleaning
credit <- read.csv('credit.csv')

credit[ , 2] <- nrow(credit) : 1

names(credit)[2] <- 'month'
names(credit)[1] <- 'credits'

credit <- credit %>%
  mutate(month = yearmonth(month)) %>%
  as_tsibble(index = month)


#EDA
credit %>%
  autoplot()
#Upward trend

gg_season(credit, credits)
gg_subseries(credit, credits)
#No seasonality bc every month is about the same

#Train/Test Split
train <- credit[1:480,]
holdout <- credit[481:492,]



#Simple Model Building

simple_fit <- train %>%
  model(
    tslm = TSLM(credits ~ trend() + season() ),
    tsf = TSLM(credits ~ trend() + season() + fourier(K = 1)),
    Mean = MEAN(credits),
    Naive = NAIVE(credits),
    Drift = RW(credits ~ drift())
  )
fit_fc <- simple_fit %>%
  forecast(h = '12 months')

accuracy(fit_fc, holdout)


#Predictive Model Building
#ETS
ETSfit <- train %>%
  #stretch_tsibble(.init = 48, .step = 12) %>%
  model(
    ets = ETS(credits)
  )

ETS_pred <- ETSfc$.mean


ETSfit %>%
  forecast(h = 12) %>%
  autoplot(holdout)

ETSfc <- ETSfit %>%
  forecast(h = 12)

ETSfc %>%
  accuracy(holdout)

ETSfit %>%
  gg_tsresiduals()


#Neural Net
NNfit <- train %>%
  #stretch_tsibble(.init = 48, .step = 24) %>%
  model(
    nnet = NNETAR(credits)
  )

NNfit %>%
  forecast(h = 12, times = 5) %>%
  autoplot(holdout)


NNfit %>%
  accuracy(holdout) %>%
  arrange(RMSE)
NNfit %>%
  accuracy()

accuracy(NNfc, holdout)

#ARIMA
ARIMAFIT <- train %>%
  #stretch_tsibble(.init = 60, .step = 6) %>%
  model(
    arima1 = ARIMA(credits)
  )

ARIMAFIT %>%
  forecast(h = 12) %>%
  autoplot(holdout)

ARIMA_fc <- ARIMAfit %>%
  forecast(h = 12)
ARIMAFIT %>%
  gg_tsresiduals()







