library(dplyr)
library(alphavantager)
library(ggplot2)

my_api='CJA0RCBPJIL23J4D'
av_api_key(my_api)



# Replace "AAPL" with the desired stock symbol
symbol <- "NVDA"

# Get daily stock prices
prices1 <- av_get(symbol=symbol,av_fun = "TIME_SERIES_WEEKLY")

# write.csv(file="NVDA.csv",prices1)

prices<-prices1%>%filter(timestamp>'2020-01-20')
# "TIME_SERIES_DAILY", "TIME_SERIES_WEEKLY"
ggplot(data=prices1%>%filter(timestamp>"2000-01-01"),aes(x=timestamp,y=close))+
  geom_line(color='navy',size=0.8)+
  xlab("Date")+ylab("USD")+
  ggtitle("NVIDIA Closing Stock Prices")+
  geom_vline(xintercept = as.Date("2020-01-20"),linetype='dashed',color='red')+
  geom_vline(xintercept = as.Date("2022-10-21"),linetype='dashed',color='red')+
  annotate("text",label="Start of COVID-19 Pandemic",x=as.Date("2019-05-20"),y=500,angle=90)+
  # annotate("text",label="December 2021",x=as.Date("2021-05-20"),y=100,angle=90)+
  annotate("text",label="2022-10-21",x=as.Date("2022-04-21"),y=450,angle=90)+
  geom_point(aes(x=as.Date(unlist(prices1%>%filter(close==max(close))%>%select(timestamp))),y=unlist(prices1%>%filter(close==max(close))%>%select(close))),color='forestgreen',size=4)+
  annotate("text",label="Maximum",x=as.Date(unlist(prices1%>%filter(close==max(close))%>%select(timestamp))),y=unlist(prices1%>%filter(close==max(close))%>%select(close))+27)+
  # geom_point(aes(x=as.Date(unlist(prices1%>%filter(close==min(close))%>%select(timestamp))),y=unlist(prices1%>%filter(close==min(close))%>%select(close))),color='red',size=4)+
  # annotate("text",label="Minimum",x=as.Date(unlist(prices1%>%filter(close==min(close))%>%select(timestamp))),y=unlist(prices1%>%filter(close==min(close))%>%select(close))+45)+
  labs(caption = "Source: By Raulin L. Cadet, with data from Alpha Vantage.",size=18)+
  theme_light()+
  theme(
    axis.text = element_text(color = "black",size=12),   # Axis text color
    axis.title = element_text(color = "black"),   # Axis title color
    axis.ticks = element_line(color = "black"),  # Axis ticks color
    axis.line = element_line(color = "black",size = 0.8),
    plot.caption = element_text(size = 12),
    plot.title = element_text(size=14),
    text = element_text(size = 12,color = 'black')
  )




