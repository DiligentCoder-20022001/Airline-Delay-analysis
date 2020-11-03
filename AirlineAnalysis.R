#title: "Airline delay analysis"
#author: "Siddharth - 18BCE1003"
#date: "07/09/2020"

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(plotly)
library(sqldf)
flights<-read.csv("C:/Users/Siddharth S Chandran/Desktop/airplanes.csv",header=T,na.strings=c("","NA"))
airports<-read.csv("C:/Users/Siddharth S Chandran/Desktop/cities.csv",header=T,na.strings=c("","NA"))
airlines<-read.csv("C:/Users/Siddharth S Chandran/Desktop/companies.csv",header=T,na.strings=c("","NA"))
sapply(airports, function(x) sum(is.na(x))) / nrow(airports) * 100
sapply(airlines, function(x) sum(is.na(x))) / nrow(airlines) * 100
unique(flights$ORIGIN_AIRPORT)
unique(flights$DESTINATION_AIRPORT)
sql <- "SELECT 
f.*,
al.AIRLINE AS AIRLINE_NAME,
a1.AIRPORT as ORIGIN_AIRPORT_NAME,
a1.CITY as ORIGIN_CITY,
a1.STATE as ORIGIN_STATE,
a1.LATITUDE as ORIGIN_LATITUDE,
a1.LONGITUDE as ORIGIN_LONGITUDE,
a2.AIRPORT as DESTINATION_AIRPORT_NAME,
a2.CITY as DESTINATION_CITY,
a2.STATE as DESTINATION_STATE,
a2.LATITUDE as DESTINATION_LATITUDE,
a2.LONGITUDE as DESTINATION_LONGITUDE
FROM 
flights f JOIN 
airports a1 ON 
f.ORIGIN_AIRPORT = a1.IATA_CODE JOIN 
airports a2 ON
f.DESTINATION_AIRPORT = a2.IATA_CODE join
airlines al on f.AIRLINE=al.IATA_CODE"

flights <- sqldf(sql)
flights$DAY_F <-factor(flights$DAY)
flights$DAY_OF_WEEK_F <-factor(flights$DAY_OF_WEEK)
flights$MONTH_F <-factor(flights$MONTH)
flights$YEAR_F <-factor(flights$YEAR)
flights$CANCELLED_F <-factor(flights$CANCELLED)
flights$AIRLINE_F <-factor(flights$AIRLINE)
na_percentages <- sapply(flights, function(x) sum(is.na(x)))/nrow(flights)*100
sort(format(round(na_percentages, 4), nsmall = 4))
flights<-subset(flights, !(is.na(flights$SCHEDULED_TIME)))
net_flights <- subset(flights, flights$CANCELLED==0)
net_flights <- subset(net_flights, !(is.na(net_flights$AIR_TIME)))
na_percentages <- sapply(net_flights, function(x) sum(is.na(x))) / nrow(net_flights) * 100
sort(format(round(na_percentages, 4), nsmall = 4))
marketShare <- flights %>% 
  group_by(AIRLINE) %>%
  dplyr::summarise(Count = n(),
                   mean_ARRIVAL_DELAY = mean(ARRIVAL_DELAY),
                   median_ARRIVAL_DELAY = median(ARRIVAL_DELAY),
                   min_ARRIVAL_DELAY = min(ARRIVAL_DELAY),
                   max_ARRIVAL_DELAY = max(ARRIVAL_DELAY),
                   mean_DEPARTURE_DELAY = mean(DEPARTURE_DELAY),
                   median_DEPARTURE_DELAY = median(DEPARTURE_DELAY),
                   min_DEPARTURE_DELAY = min(DEPARTURE_DELAY),
                   max_DEPARTURE_DELAY = max(DEPARTURE_DELAY)
  ) %>% arrange(desc(Count))


plot_ly(marketShare, labels = ~AIRLINE, values = ~Count, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        marker = list(
          line = list(color = '#FFFFFF', width = 1)),
        showlegend = FALSE)
cancelled_flights <- subset(flights, flights$CANCELLED==1)
na_percentages <- sapply(cancelled_flights, function(x) sum(is.na(x))) / nrow(cancelled_flights) * 100
sort(format(round(na_percentages, 4), nsmall = 4))
nrow(cancelled_flights)/nrow(flights)*100
cancellation_reason <- summary(factor(cancelled_flights$CANCELLATION_REASON))
cancellation_reason[2]/sum(cancellation_reason)*100 #Weather caused cancellation
cancellation_reason[1]/sum(cancellation_reason)*100 #Airline caused cancellation
cancellation_reason[4]/sum(cancellation_reason)*100 #Security caused cancellation
cancellation_reason[3]/sum(cancellation_reason)*100 #Air system caused cancellation
temp <- subset(cancelled_flights, factor(cancelled_flights$CANCELLATION_REASON)=='A')
min(summary(temp$AIRLINE_F))/nrow(temp)*100
max(summary(temp$AIRLINE_F))/nrow(temp)*100
airline_cancellation_fq <- summary(temp$AIRLINE_F)
all_cancellation_fq <- summary(cancelled_flights$AIRLINE_F)
all_flight_fq <- summary(flights$AIRLINE_F)

sort(airline_cancellation_fq / all_cancellation_fq, decreasing = TRUE)
sort(all_cancellation_fq / all_flight_fq, decreasing = TRUE)
airline_cancellation_score <- airline_cancellation_fq / all_flight_fq #Save cancellation rate as airline cancellation score for later use
airline_cancellation_rates <- sort(airline_cancellation_fq / all_flight_fq, decreasing = TRUE)

remove(cancelled_flights, airline_cancellation_fq, all_cancellation_fq, all_flight_fq, temp)

cols <- colorRampPalette(c('blue','green','red','yellow'))(14)
barplot(airline_cancellation_rates, las = 1, col = cols, xlab = "Airline Company Codes", ylab = "Cancellation Rates", main="Histogram of Cancellation Rates")
ggplot(data=flights, aes(x=flights$AIRLINE, fill=flights$CANCELLED_F)) +
  geom_histogram(stat = "Count",position = "dodge", col="red") +
  labs(title="Histogram for Airline") +
  labs(x="Airline", y="Number of Flights") + scale_x_discrete() +
  scale_y_log10() +
  labs(fill='Cancelled or Not')
flights$status<-'ON TIME'
flights$status[flights$DEPARTURE_DELAY>0] <-'DEPARTURE DELAY'
flights$status[flights$CANCELLED==1] <-'CANCELLED FLIGHTS'
flights$status[flights$ARRIVAL_DELAY>0 & (flights$ARRIVAL_DELAY-flights$DEPARTURE_DELAY)>0 & flights$DEPARTURE_DELAY>0] <-'DEPR & ARVL_DELAY'
flights$status[flights$ARRIVAL_DELAY>0 & flights$DEPARTURE_DELAY<=0] <-'ARRIVAL_DELAY'
flights$status<-factor(flights$status)
ggplot(flights,aes(x=MONTH_F,fill=status),binwidth=12) + 
  geom_bar(position = "fill") + 
  ggtitle("Flights by Months According to Flights Status")
flightsDepDelay<-subset(flights,flights$DEPARTURE_DELAY>0)
flightsDepDelay$DelayTimeInterval<-cut(flightsDepDelay$DEPARTURE_DELAY,
                                       breaks=c(0,10,30,60,90,120,180,1988),
                                       labels=c("(0-10]","(10-30]","(30-60]","(60-90]","(90-120]","(120-180]","(180-1988]")
)
ggplot(flightsDepDelay,aes(x=MONTH_F,fill=DelayTimeInterval)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(breaks=seq(0,1,0.1))
ggtitle("Departure Delay Flights by Months According To DelayTime Interval")

remove(flightsDepDelay)
net_flights$DEP_HOUR <- NA
net_flights$DEP_HOUR <- as.integer(net_flights$DEPARTURE_TIME / 100)

net_flights$LATENCY <- NA
net_flights$LATENCY <- case_when(
  net_flights$ARRIVAL_DELAY <= 5 ~"On Time (0-5 mins)",
  net_flights$ARRIVAL_DELAY > 5 & net_flights$ARRIVAL_DELAY <= 15 ~"Small Delay (5-15 mins)",
  net_flights$ARRIVAL_DELAY > 15 ~ "Large Delay (> 15 mins)" )
ggplot(data=net_flights, aes(x=factor(net_flights$DEP_HOUR),fill=factor(net_flights$LATENCY))) + 
  geom_histogram(stat = "Count",position = "dodge", col="red") +
  labs(title="Histogram for Airline") +
  labs(x="Departure Hours", y="Number of Flights") + scale_x_discrete()


d_late_flights <- subset(net_flights, net_flights$DEPARTURE_DELAY>0)
d_ontime_flights <- subset(net_flights, net_flights$DEPARTURE_DELAY<=0)
d_early_flights <- subset(net_flights, net_flights$DEPARTURE_DELAY <= -5)

nrow(d_late_flights)/nrow(net_flights)*100
nrow(d_early_flights)/nrow(net_flights)*100
```


remove(d_early_flights, d_ontime_flights)

temp <- subset(d_late_flights, !is.na(d_late_flights$AIRLINE_DELAY))

temp <- subset(net_flights, !is.na(net_flights$AIRLINE_DELAY))
summary(temp$AIRLINE_F)
min(summary(temp$AIRLINE_F))/nrow(temp)*100
max(summary(temp$AIRLINE_F))/nrow(temp)*100

airline_delay_fq <- summary(temp$AIRLINE_F) #delay numbers for airline firms
all_flight_fq <- summary(net_flights$AIRLINE_F) #flight numbers for airline firms

airline_delay_score <- airline_delay_fq / all_flight_fq #save airline delay rates for later use
airline_delay_rates <- sort(airline_delay_fq / all_flight_fq, decreasing = TRUE)
cols <- colorRampPalette(c('blue','green','red','yellow'))(14)

barplot(airline_delay_rates, las = 1, space = 0, col = cols, xlab = "Airline Company Codes", ylab = "Delay Rates", main="Histogram of Delay Count Rates")

remove(airline_delay_fq, airline_delay_rates, all_flight_fq)

airline_depdelay_time <- aggregate(temp$DEPARTURE_DELAY, by=list(Airline=temp$AIRLINE), FUN=sum)
airline_flight_fq <- summary(net_flights$AIRLINE_F)

rated_depdelay_time <- data.frame(AIRLINE=c(names(airline_flight_fq)), DEP_DELAY_RATE=c(airline_depdelay_time[2] / data.frame(AIRLINE=c(names(airline_flight_fq)), FLIGHT_FREQ=c(as.numeric(airline_flight_fq)))[2]))

airline_rated_depdelay_time <- as.numeric(unlist(rated_depdelay_time)[15:28])
names(airline_rated_depdelay_time) <-names(airline_flight_fq)
airline_depdelay_score <- airline_rated_depdelay_time
airline_rated_depdelay_time <- sort(airline_rated_depdelay_time, decreasing = TRUE)
cols <- colorRampPalette(c('blue','green','red','yellow'))(14);

barplot(airline_rated_depdelay_time, las = 1, space = 0, col = cols, xlab = "Airline Company Codes", ylab = "Rated Departure Delay Times (mins)", main="Histogram of Rated Departure Delay Times")

a_late_flights <- subset(net_flights, net_flights$ARRIVAL_DELAY > 0)
#a_ontime_flights <- subset(net_flights, net_flights$ARRIVAL_DELAY <= 0)
a_early_flights <- subset(net_flights, net_flights$ARRIVAL_DELAY <= -5)
nrow(a_late_flights)/nrow(net_flights)*100
nrow(a_early_flights)/nrow(net_flights)*100

remove(a_early_flights, temp)
temp <- subset(a_late_flights, !is.na(a_late_flights$AIRLINE_DELAY))
airline_arrdelay_time <- aggregate(temp$ARRIVAL_DELAY, by=list(Airline=temp$AIRLINE), FUN=sum)
airline_flight_fq <- summary(net_flights$AIRLINE_F)
rated_arrdelay_time <- data.frame(AIRLINE=c(names(airline_flight_fq)), ARR_DELAY_RATE=c(airline_arrdelay_time[2] / data.frame(AIRLINE=c(names(airline_flight_fq)), FLIGHT_FREQ=c(as.numeric(airline_flight_fq)))[2]))
remove(temp)
airline_rated_arrdelay_time <- as.numeric(unlist(rated_arrdelay_time)[15:28])
names(airline_rated_arrdelay_time) <-names(airline_flight_fq)
airline_arrdelay_score <- as.numeric(airline_rated_arrdelay_time)
airline_rated_arrdelay_time <- sort(airline_rated_arrdelay_time, decreasing = TRUE)
cols <- colorRampPalette(c('blue','green','red','yellow'))(14);
barplot(airline_rated_arrdelay_time, las=1,col = cols,xlab = "Airline Company Codes", ylab = "Rated Arrival Delay Times (mins)", main="Histogram of Rated Arrival Delay Times")
barplot(sort(airline_arrdelay_score+airline_cancellation_score+airline_delay_score+airline_depdelay_score, decreasing = TRUE), las = 1, col = cols, xlab = "Airline Company Codes", ylab = "Delay Rates", main="Histogram of Delay Rates")




ggplot(subset(net_flights, ARRIVAL_DELAY>=0), aes(x=reorder(AIRLINE, ARRIVAL_DELAY, FUN=median), y=ARRIVAL_DELAY, fill=AIRLINE)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(0,50)) +
  labs(x = 'Airline', y = 'Arrival Delay',
       title = 'Arrival Delay vs. Airlines')
ggplot(aes(x=MONTH, y=ARRIVAL_DELAY),
       data = subset(a_late_flights, !is.na(AIRLINE))) +
  geom_line(aes(color=AIRLINE), stat='summary', fun.y=mean)  +
  facet_wrap(~AIRLINE_F, ncol=3) + geom_smooth(method = "lm")

remove(a_late_flights)
ggplot(aes(x=MONTH, y=DEPARTURE_DELAY),
       data = subset(d_late_flights, !is.na(AIRLINE))) +
  geom_line(aes(color=AIRLINE), stat='summary', fun.y=mean) +
  scale_x_continuous(breaks = seq(1,12,1))

remove(d_late_flights)


departureDelayCity1<-subset(net_flights,net_flights$DEPARTURE_DELAY>0 ) %>%
  group_by(ORIGIN_AIRPORT,ORIGIN_CITY,ORIGIN_STATE) %>%
  dplyr::summarise(avg_departure_delay=mean(DEPARTURE_DELAY),count_of_fly=n()) %>%
  arrange(ORIGIN_AIRPORT)
flightsCity<-net_flights%>%
  group_by(ORIGIN_AIRPORT,ORIGIN_CITY,ORIGIN_STATE) %>%
  dplyr::summarise(count_of_fly=n()) %>%
  arrange(ORIGIN_AIRPORT)
departureDelayCity<- sqldf("
                           select d.ORIGIN_AIRPORT,d.ORIGIN_CITY,f.ORIGIN_STATE,
                           d.count_of_fly as delay_cnt,
                           f.count_of_fly as total_cnt,
                           avg_departure_delay
                           from departureDelayCity1 as d,flightsCity f
                           where d.ORIGIN_AIRPORT=f.ORIGIN_AIRPORT
                           and d.ORIGIN_CITY=f.ORIGIN_CITY
                           and d.ORIGIN_STATE=f.ORIGIN_STATE
                           ")
departureDelayCity$percentage<-round(departureDelayCity$delay_cnt/departureDelayCity$total_cnt*100)

mostDelayedAirport<-subset(departureDelayCity, percentage>25)
mostDelayedAirportCity <- sqldf("select mostDelayedAirport.ORIGIN_CITY,
                                mostDelayedAirport.ORIGIN_AIRPORT as MOST_DELAY_AIRP,
                                departureDelayCity.ORIGIN_AIRPORT as OTHER_AIRP,
                                mostDelayedAirport.percentage as MOST_DELAY_AIRP_PERCENTAGE,
                                departureDelayCity.percentage as OTHER_AIRP_PERCENTAGE,
                                case when departureDelayCity.percentage>25 
                                then 1 else 0 end as OTHER_AIRP_DELAYED_F
                                from mostDelayedAirport
                                join departureDelayCity
                                where departureDelayCity.ORIGIN_CITY=mostDelayedAirport.ORIGIN_CITY
                                and mostDelayedAirport.ORIGIN_AIRPORT<>departureDelayCity.ORIGIN_AIRPORT
                                ") 
net_flights$flight_speed <- net_flights$DISTANCE / net_flights$AIR_TIME

ggplot(net_flights,aes(x=AIRLINE_NAME,y=flight_speed)) +
  geom_boxplot(aes(fill = AIRLINE_NAME)) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
taxiInAirport<-subset(net_flights,!is.na(net_flights$TAXI_IN)) %>%
  group_by(DESTINATION_AIRPORT,DESTINATION_CITY,DESTINATION_STATE) %>%
  dplyr::summarise(mean_taxiIn=mean(TAXI_IN), count_of_fly=n()) %>%
  arrange(DESTINATION_AIRPORT)
cor.test(taxiInAirport$mean_taxiIn,taxiInAirport$count_of_fly)
ggplot(taxiInAirport,aes(x=mean_taxiIn,y=count_of_fly/100)) +
  geom_point(color='#66CC99',alpha=0.4) +
coord_cartesian(xlim=c(0,7)) +
  ylab('Count of flight/100') +
  geom_smooth(linetype=2, color="#0000FF", aes(group=1), se=FALSE)
ggplot(taxiInAirport,aes(x=mean_taxiIn,y=count_of_fly/100)) +
  geom_point(color='#66CC99',alpha=0.4) +
coord_cartesian(xlim=c(0,7), ylim=c(0,200)) +
  ylab('Count of flight/100') +
  geom_smooth(linetype=2, color="#0000FF", aes(group=1), se=FALSE)
taxiInAirport<-na.omit(taxiInAirport)
model <- lm((count_of_fly/100)~mean_taxiIn, data = taxiInAirport)
model
summary(model)
library(car)
library(moments)
library(caret)
library(tidyverse)
set.seed(123)
train_samples <- taxiInAirport$count_of_fly%>%
  createDataPartition(p = 0.8, list = FALSE)
train_data<-taxiInAirport[train_samples, ]
test_data<-taxiInAirport[-train_samples, ]
pred <- model%>%
  predict(train_data)
y_act <- test_data$count_of_fly
rmse_value<-sqrt(mean(y_act-pred))
rmse_value
summary(model)

