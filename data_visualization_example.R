#Time-series Data Visualization using ggplot

library(ggplot2)
data = read.csv("vix_timeseries_data.csv",header=T,stringsAsFactor=F)
data$Date <- as.Date(as.character(data$Date),format="%Y%m%d")
data_date=data$Date
data_price=data$Vix_Price

#find local maximas 1D algorithm 
find_local_maximas <- function (x, m){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  local_maximas <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  local_maximas <- unlist(local_maximas)
  local_maximas
} #source: https://github.com/stas-g/findPeaks

#create a data frame for local maximas only
peak_index=find_local_maximas(data_price, m = 15) 
local_maxima_result <-data.frame(Date=data_date[peak_index],Vix_Price=data_price[peak_index])
local_date=local_maxima_result$Date
local_peak=local_maxima_result$Vix_Price
dt = 4

#visualize Vix time-series data
ggplot()+
  geom_line(aes(data_date,data_price),color="blue",size=0.2,alpha=0.4)+theme_bw()+
  labs(title = "VIX Index Daily Closing Price", x="Time", y="Price")+
  geom_rect(data=local_maxima_result,mapping=aes(xmin=local_date-dt, xmax=local_date+dt, 
            ymin=-Inf, ymax=+Inf),fill='pink',alpha=0.4,linetype ="blank")+
  geom_point(data=local_maxima_result, mapping=aes(x=local_date, y=local_peak),
             color="red", size=1.5)




