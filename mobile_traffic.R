library(tidyverse)
library(readxl)

data<-read_excel("tms_raw.xlsx", sheet = "raw_hourly", na = c("불량", "교정", "단절"))
#어째서 transmute가 제대로 작동 안하는지 모르겠음 그래서 mutate후에 data에 덮어씌우는 형태로 진행
data<-mutate(data, date=as.POSIXct(as.character(data$date), tz="Asia/Seoul", origin = "1970-01-01", format = "%Y%m%d%H"))

summary <- data %>% group_by(city) %>% group_by(type) %>% group_by(date) %>% summarise(avg= mean(c(CO,SO2)))