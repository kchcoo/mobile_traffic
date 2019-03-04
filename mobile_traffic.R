library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)

data<-read_excel("tms_raw.xlsx", sheet = "raw_hourly") #NA나 이상값들은 모두 공백처리
#어째서 transmute가 제대로 작동 안하는지 모르겠음 그래서 mutate후에 data에 덮어씌우는 형태로 진행

data<-mutate(data, date=as.POSIXct(as.character(data$date), tz="Asia/Seoul", origin = "1970-01-01", format = "%Y%m%d%H"))

#summarise에 계속 NA가 나타나는 현상이 발생, NA가 하나도 없어야 함수가 작동함, NA를 빼고 처리하는 방법은 아직 알지못해서 rawdata의 NA를 전부 삭제

#WD를 위한 최빈값 구하는 함수
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
  
}

data_date<-data %>% group_by(city, type, date) %>% summarise_each(funs(mean))

#https://wotres.tistory.com/31 aggregation 함수 관련 오류 참조
#openair manual 49page : wind direction 평균내는 방법
