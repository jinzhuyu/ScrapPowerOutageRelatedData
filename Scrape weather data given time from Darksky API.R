setwd('C:/GitHub/Power grid resilience/weather data from darksky api')

# install.packages('darksky')
library('darksky')
library('tidyverse')
#data soruce: https://darksky.net/dev/docs#time-machine-request
#Reference: https://github.com/hrbrmstr/darksky

# Apparent (feels-like) temperature
# Atmospheric pressure
# Humidity
# Liquid precipitation rate
# Nearest storm distance
# Nearest storm direction
# Temperature
# Text summaries
# Wind gust
# Wind speed
# Wind direction

# get a key before making calls to retrieve/acquire historical weather data from darksky

Sys.setenv('DARKSKY_API_KEY'='0f9958c3ae3734181d9aca5a3d0db82d')

# test if the key has been added
Sys.getenv("DARKSKY_API_KEY")

# # Make time-machine request of hourly data
# 
# weather_records=get_forecast_for(latitude = 35.1495 ,longitude = -90.0490, timestamp = '2013-05-06T12:00:00-0400')
#   
# humidity=weather_records$hourly$humidity




# getting data for more than one location and one time point
outage_data_raw=read.csv('C:/GitHub/Power grid resilience/weather data from darksky api/date_number_weather.csv')
# note that date format should be set as: yy-mm-dd 

date=rep(NA, length(outage_data_raw[,1]))
for (i in 1:length(outage_data_raw[,1])){
  date[i]=paste(outage_data_raw[i,1],'T12:00:00-0400',sep ='',collapse = ',')
}


more_than_one = data.frame(#loc=c("Memphis"),
                            lat=c(35.1495),
                            lon=c(-90.0490),
                            when=date,
                            stringsAsFactors=FALSE)

bigger_list = pmap(list(more_than_one$lat, more_than_one$lon,
                         more_than_one$when), get_forecast_for)

weather_condition_types=c('time','summary','icon','precipIntensity',
          'precipProbability','temperature','apparentTemperature',
          'dewPoint','humidity','pressure','windSpeed','windGust',
          'windBearing','cloudCover','uvIndex','visibility') # 'precipType' is not alway available

# select the weather conditions associated with the time point
#define function
date_length=length(outage_data_raw[,1])
select_weather_condition=function(weather_condition){
  weather_data=rep(NA,date_length)
  for (i_date in 1:date_length){
    print(i_date)
    weather_data[i_date]=bigger_list[[i_date]]$hourly[weather_condition][outage_data_raw[i_date,2]+1,1] # starts from 12 am
  }
  return(weather_data)
}

weather_conditions=matrix(, nrow = length(outage_data_raw[,1]), ncol = length(weather_condition_types))
for (j in 1:length(weather_condition_types)){
  weather_conditions[,j]=select_weather_condition(weather_condition_types[j])
}

weather_conditions_df=as.data.frame(weather_conditions)
colnames(weather_conditions_df)=weather_condition_types
names(weather_conditions_df)[1]=c('date')

weather_conditions_df$hour=outage_data_raw[,2]
weather_conditions_df=weather_conditions_df[,c(1,17,seq(2,16,1))]

write.csv(weather_conditions_df,'C:/GitHub/Power grid resilience/weather data from darksky api/weather_conditions_data.csv', row.names = FALSE)
# # plot temperate within a speficied period
# seq(as.Date('2019-06-01'), as.Date('2019-06-06'), "1 day") %>% 
#   map(~get_forecast_for(35.1495, -90.0490, .x)) %>% 
#   map_df("hourly") %>% 
#   ggplot(aes(x=time, y=temperature)) +
#   geom_line()


# plot recovery rate as a function of time

change_point=which(outage_data_raw[,4]==0)
n_storms=length(change_point)
storm_dates=outage_data_raw[change_point,1]

colors=rainbow(n_storms-1)
windowsFonts(TNR = windowsFont("Times New Roman"))
size_tick=1.15

for (i in 1:(n_storms-1)){
  time_after_storm=outage_data_raw[change_point[i]:(change_point[i+1]-1),3]
  recovery_rate=outage_data_raw[change_point[i]:(change_point[i+1]-1),4]/outage_data_raw[(change_point[i+1]-1),4]
  if (i==1){
    plot(time_after_storm,recovery_rate,
         type = 'b',col=colors[i],lwd = 2,family="TNR",
         main = 'Recovery rate vs time after storm',
         xlab = 'Time after storm (h)', ylab='Recovery rate',
         cex.lab=1.25, cex.axis=size_tick, cex.main=1.5, font.lab=2) # for ticks, font = 2
  }
  else{
    points(time_after_storm,recovery_rate,
           type = 'b', col=colors[i], lwd = 2, cex = 1.25, family="TNR")
  }
}

legend("bottomright", legend = storm_dates, col = colors, lwd = 2,pch=1,cex = 1.25)

grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)


# plot recovery rate as a function of 
# weather_condition_types=c('temperature','apparentTemperature','humidity','pressure','windSpeed','windGust',
# 'windBearing','cloudCover')
# A gust is a sudden increase of the wind's speed that lasts no more than 20 seconds.

# deal with NA in the data

# wind gust
# there is only one NA in windGust column
wind_gust=weather_conditions_df$windGust
wind_gust=as.vector(as.numeric(wind_gust))
wind_gust[which(is.na(wind_gust))]=0


# recovery rate vs windgust
for (i in 1:(n_storms-1)){
  wind_gust_i=wind_gust[change_point[i]:(change_point[i+1]-1)]
  recovery_rate=outage_data_raw[change_point[i]:(change_point[i+1]-1),4]/outage_data_raw[(change_point[i+1]-1),4]
  if (i==1){
    plot(wind_gust_i,recovery_rate,
         xlim = c(0,max(wind_gust)),
         type = 'p',col=colors[i],lwd = 2,family="TNR",
         main = 'Recovery rate vs wind gust',
         xlab = 'Wind gust', ylab='Recovery rate',
         cex.lab=1.25, cex.axis=size_tick, cex.main=1.5, font.lab=2) # for ticks, font = 2
  }
  else{
    points(wind_gust_i,recovery_rate,
           type = 'p', col=colors[i], lwd = 2, cex = 1.25, family="TNR")
  }
}

# wind speed
wind_speed=weather_conditions_df$windSpeed
wind_speed=as.vector(as.numeric(wind_speed))
wind_speed[which(is.na(wind_speed))]=0

for (i in 1:(n_storms-1)){
  wind_speed_i=wind_speed[change_point[i]:(change_point[i+1]-1)]
  recovery_rate=outage_data_raw[change_point[i]:(change_point[i+1]-1),4]/outage_data_raw[(change_point[i+1]-1),4]
  if (i==1){
    plot(wind_speed_i,recovery_rate,
         xlim = c(0,max(wind_speed)),
         type = 'p',col=colors[i],lwd = 2,family="TNR",
         main = 'Recovery rate vs wind speed',
         xlab = 'Wind speed', ylab='Recovery rate',
         cex.lab=1.25, cex.axis=size_tick, cex.main=1.5, font.lab=2) # for ticks, font = 2
  }
  else{
    points(wind_speed_i,recovery_rate,
           type = 'p', col=colors[i], lwd = 2, cex = 1.25, family="TNR")
  }
}

# apparent temperature
apperent_temp=weather_conditions_df$apparentTemperature
apperent_temp=as.vector(as.numeric(apperent_temp))
for (i in 1:(n_storms-1)){
  apperent_temp_i=apperent_temp[change_point[i]:(change_point[i+1]-1)]
  recovery_rate=outage_data_raw[change_point[i]:(change_point[i+1]-1),4]/outage_data_raw[(change_point[i+1]-1),4]
  if (i==1){
    plot(apperent_temp_i,recovery_rate,
         xlim = c(min(apperent_temp),max(apperent_temp)),
         type = 'p',col=colors[i],lwd = 2,family="TNR",
         main = 'Recovery rate vs apparent temperature',
         xlab = 'Apparent temperature', ylab='Recovery rate',
         cex.lab=1.25, cex.axis=size_tick, cex.main=1.5, font.lab=2) # for ticks, font = 2
  }
  else{
    points(apperent_temp_i,recovery_rate,
           type = 'p', col=colors[i], lwd = 2, cex = 1.25, family="TNR")
  }
}



# humidity
humidity=weather_conditions_df$humidity
humidity=as.vector(as.numeric(humidity))
for (i in 1:(n_storms-1)){
  humidity_i=humidity[change_point[i]:(change_point[i+1]-1)]
  recovery_rate=outage_data_raw[change_point[i]:(change_point[i+1]-1),4]/outage_data_raw[(change_point[i+1]-1),4]
  if (i==1){
    plot(humidity_i,recovery_rate,
         xlim = c(min(humidity),max(humidity)),
         type = 'p',col=colors[i],lwd = 2,family="TNR",
         main = 'Recovery rate vs humidity',
         xlab = 'Humidity', ylab='Recovery rate',
         cex.lab=1.25, cex.axis=size_tick, cex.main=1.5, font.lab=2) # for ticks, font = 2
  }
  else{
    points(humidity_i,recovery_rate,
           type = 'p', col=colors[i], lwd = 2, cex = 1.25, family="TNR")
  }
}

# maximum number of customers without power vs. wind speed when the outage started
plot(wind_speed[change_point[-1]-1],outage_data_raw[change_point[-1]-1,4],
     type = 'p',col=colors[i],lwd = 2,family="TNR",
     main = 'Maximum # of customers without power \n vs. wind speed when the outage started',
     xlab = 'Wind speed when the outage started', ylab='Maximum # of customers without power',
     cex.lab=1.25, cex.axis=size_tick, cex.main=1.5, font.lab=2)
