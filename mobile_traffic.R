library(tidyverse)
library(lattice)
library(readxl)
library(dplyr)
library(tidyr)
library(openair)

data<-read_excel("tms_raw.xlsx", sheet = "raw_hourly") #NA나 이상값들은 모두 공백처리
#어째서 transmute가 제대로 작동 안하는지 모르겠음 그래서 mutate후에 data에 덮어씌우는 형태로 진행

data<-mutate(data, date=as.POSIXct(as.character(data$date), tz="Asia/Seoul", origin = "1970-01-01", format = "%Y%m%d%H"))

#summarise에 계속 NA가 나타나는 현상이 발생, NA가 하나도 없어야 함수가 작동함, NA를 빼고 처리하는 방법은 아직 알지못해서 rawdata의 NA를 전부 삭제

#WD를 위한 최빈값 구하는 함수
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
  
}

data_date<-data %>% group_by(city, type, date) %>% summarise_each(funs(mean)) #WD 평균관련 오류를 해결하기 전까지 WD자료 사용 주의
data_date<-select(data_date, -site_name, -site_number) #측정소명과 측정소번호 자료에서 제외
data_date$city<-as.factor(data_date$city) #도시명과 측정소형태를 factor화
data_date$type<-as.factor(data_date$type)

data_city_split <- data_date %>% group_by(city) %>% split(data_date, f = "type")
data_Sw <- data_date %>% filter(city=="수원")
data_Yi <- data_date %>% filter(city=="용인")
data_Nyj <- data_date %>% filter(city=="남양주")
data_Sn <- data_date %>% filter(city=="성남")
data_Ay <- data_date %>% filter(city=="안양")
data_Bc <- data_date %>% filter(city=="부천")
data_Gy <- data_date %>% filter(city=="고양")
data_Pt <- data_date %>% filter(city=="평택")

timePlot_kch(subset(data_date, city=="수원" & type=="이동차"), subset(data_date, city=="수원" & type=="도시대기"), pollutant =c("NO2", "O3", "PM10", "PM25", "WS", "temp"), y.relation="free", lwd = 2, main = "이동차(수원)")
timePlot(subset(data_date, city=="수원" & type=="도시대기"), pollutant =c("NO2", "O3", "PM10", "PM25", "WS", "temp"), y.relation="free", lwd = 2, main = "도시대기(수원)")
timePlot(subset(data_date, city=="수원" & type=="도로변"), pollutant =c("NO2", "O3", "PM10", "PM25", "WS", "temp"), y.relation="free", lwd = 2, main = "도로변(수원)")

timePlot(data_Sw, pollutant = c("NO2", "O3", "PM10", "PM25", "WS"), group = T, y.relation = "free", lwd = 2) #실패 그래프 그룹화 in r 찾아볼것
#y.relation : 각 그래프의 y축 scale을 각각 구성

#timeVariation 함수를 이동차와 도시대기 그리고 그 difference로 표시하면 좋을듯
timeVariation(data_date, pollutant = "SO2", group = "type", lwd = 2)
timeVariation(data_date, pollutant = "NO2", group = "type", lwd = 2)
timeVariation(data_date, pollutant = "CO", group = "type", lwd = 2)
timeVariation(data_date, pollutant = "O3", group = "type", lwd = 2)
timeVariation(data_date, pollutant = "PM10", group = "type", lwd = 2)
timeVariation(data_date, pollutant = "PM25", group = "type", lwd = 2)


timeVariation(data_Sw, pollutant = "NO2", group = "type") #데이터가 list형식이면 plot이 그려지지 않음
timeVariation(data_Yi, pollutant = "NO2", group = "type")
timeVariation(data_Nyj, pollutant = "NO2", group = "type")
timeVariation(data_Sn, pollutant = "NO2", group = "type")
timeVariation(data_Ay, pollutant = "NO2", group = "type")
timeVariation(data_Bc, pollutant = "NO2", group = "type")
timeVariation(data_Gy, pollutant = "NO2", group = "type")
timeVariation(data_Pt, pollutant = "NO2", group = "type")

#https://wotres.tistory.com/31 aggregation 함수 관련 오류 참조
#openair manual 49page : wind direction 평균내는 방법
