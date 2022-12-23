library(tidyverse)
library(fpp3)
library(tsibble)
library(lubridate)

walmart_data=readr::read_csv("Walmart.csv")%>%
                        mutate(Date=dmy(Date))

ts_walmart_data=walmart_data %>%
                  as_tsibble(key=Store,index=Date)


features=readr::read_csv("../data/features/features.csv")
sampleSubmission=readr::read_csv("../data/sampleSubmission/sampleSubmission.csv")
test=readr::read_csv("../data/test/test.csv")  
train=readr::read_csv("../data/train/train.csv")  

holidays=data.frame(super_bowl=c(dmy("12-02-2010"),dmy("11-02-2011"),dmy("10-02-2012"),dmy("08-02-2013")),
                    labor_day=c(dmy("10-09-2010"),dmy("09-09-2011"),dmy("07-10-2012"),dmy("06-09-2013")),
                    thanksgiving=c(dmy("26-11-2010"),dmy("25-11-2011"),dmy("23-11-2012"),dmy("29-11-2013")),
                    christmas=c(dmy("31-12-2010"),dmy("30-12-2011"),dmy("28-12-2012"),dmy("27-12-2013"))) %>%
            as_tibble()

ts_walmart_data%>%
  filter(Store==sample(length(unique(ts_walmart_data$Store)),10))%>%
  autoplot(.vars=Weekly_Sales )
                  


### General analytics##################

## Viewing the data

ts_walmart_meaned_data=walmart_data%>%
                            group_by(Date)%>%
                            summarise_all(mean)%>%
                            select(-Store)%>%
                            as_tsibble(index=Date)

ts_walmart_meaned_data%>%
  autoplot(.vars=Weekly_Sales,lwd=1.5)+
  geom_vline(xintercept =holidays$thanksgiving)

ts_walmart_meaned_data%>%
  gather(Weekly_Sales,Temperature,CPI)%>%
  ggplot(aes(x=Date,y=Weekly_Sales))+
  geom_line()

ts_walmart_meaned_data%>%
  gg_tsdisplay(difference(Weekly_Sales, 12),
               plot_type='partial', lag=36) +
    labs(title="Seasonally differenced", y="")

## Testing models

arima_walmart_fit=ts_walmart_meaned_data %>%
  model(arima = ARIMA(Weekly_Sales ~  pdq(2,1,0) ))

arima_walmart_fit=ts_walmart_meaned_data %>%
  model(arima = ARIMA(Weekly_Sales))



forecast(arima_walmart_fit, h=12) %>%
  autoplot(ts_walmart_meaned_data)

forecast(arima_walmart_fit,h='1 month')

walmart_basics_fits=ts_walmart_meaned_data%>%
                      model(
                            naive=NAIVE(Weekly_Sales),
                            drift=RW(Weekly_Sales~drift()),
                            mean=MEAN(Weekly_Sales))

walmart_basics_fits%>%
              forecast(h='1 month')%>%
  autoplot()
######################################

          